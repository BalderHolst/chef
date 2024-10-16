use std::ops::Mul;

use factorio_blueprint::{
    objects::{self as fbo, ArithmeticOperation, BlueprintBook, DeciderComparator, Direction},
    Container,
};

use factorio_circuit_networks::{
    self, ArithmeticCombinator, ConstantCombinator, ConstantSignal, DeciderCombinator, Entity,
    EntityKind, EntitySignal, WireColor,
};

use eframe::{
    egui::{
        self, vec2, Align2, Color32, FontFamily, FontId, Frame, Key, Margin, Painter, Pos2, Rect,
        Rgba, ScrollArea, Sense, Shape, Stroke, Ui, Vec2,
    },
    epaint::{CubicBezierShape, Hsva, PathShape},
};

const ROUNDING: f32 = 0.03;

pub fn run(container: Container) {
    let options = eframe::NativeOptions {
        viewport: egui::ViewportBuilder::default().with_inner_size([320.0, 240.0]),
        ..Default::default()
    };

    if let Err(e) = eframe::run_native(
        "Chef Inspector",
        options,
        Box::new(|_cc| Ok(Box::new(App::new(container)))),
    ) {
        let m = match e {
            eframe::Error::AppCreation(_) => todo!(),
            eframe::Error::Winit(_) => todo!(),
            eframe::Error::WinitEventLoop(e) => todo!("{e:?}"),
            eframe::Error::Glutin(_) => todo!(),
            eframe::Error::NoGlutinConfigs(_, _) => {
                format!("{e}.\n\nIs the system library in sync with the dynamically linked one?")
            }
            other => format!("{}", other),
        };
        eprintln!("Error running GUI: {m}");
    }
}

/// Choose a deterministic hue based on the name
fn choose_hue(name: &str) -> f32 {
    let mut sum = 0.0;
    for c in name.chars() {
        sum += f32::from(c as u8) / f32::from(u8::MAX);
    }
    sum % 1.0
}

fn const_sig_hue(sig: &ConstantSignal) -> f32 {
    choose_hue(&sig.name)
}

fn draw_const_sig(
    sig: &ConstantSignal,
    painter: &Painter,
    cam: &Camera,
    pos: Pos2,
    size: f32,
    luminance: f32,
) {
    let size = cam.scaled(size);
    let rect = Rect::from_center_size(pos, Vec2::new(size, size));
    painter.rect_filled(
        rect,
        cam.scaled(ROUNDING),
        Hsva::new(const_sig_hue(sig), 0.8, luminance, 1.0),
    );

    let (s, font_size) = match sig.name.strip_prefix("signal-") {
        Some(letter) if letter.len() == 1 => (letter.to_string(), 0.14),
        Some(color) => (color.to_string(), 0.10),
        None => (sig.name.replace('-', "\n"), 0.045),
    };

    let offset = 0.01;
    painter.text(
        rect.center() + Vec2::new(0.0, cam.scaled(offset)),
        Align2::CENTER_CENTER,
        &s,
        FontId {
            size: cam.scaled(font_size),
            family: FontFamily::Monospace,
        },
        Color32::BLACK,
    );

    painter.text(
        rect.right_bottom() + cam.scaled(Vec2::new(-0.02, -0.005)),
        Align2::RIGHT_BOTTOM,
        sig.count.to_string(),
        FontId {
            size: cam.scaled(0.05),
            family: FontFamily::Monospace,
        },
        Color32::BLACK,
    );
}

fn entity_hue(e: &EntitySignal) -> f32 {
    match e {
        EntitySignal::Signal(s) => choose_hue(s),
        EntitySignal::Constant(_) => 0.083,
    }
}

fn draw_entity(
    e: &EntitySignal,
    painter: &Painter,
    cam: &Camera,
    pos: Pos2,
    size: f32,
    luminance: f32,
) {
    let size = cam.scaled(size);
    let rect = Rect::from_center_size(pos, Vec2::new(size, size));
    let painter = painter.with_clip_rect(rect);
    painter.rect_filled(
        rect,
        cam.scaled(ROUNDING),
        Hsva::new(entity_hue(e), 0.8, luminance, 1.0),
    );

    let (s, size) = match &e {
        EntitySignal::Signal(s) => match s.strip_prefix("signal-") {
            //Some("each") => todo!(),
            //Some("anything") => todo!(),
            //Some("everything") => todo!(),
            Some(letter) if letter.len() == 1 => (letter.to_string(), 0.20),
            Some(color) => (color.to_string(), 0.10),
            None => (s.replace('-', "\n"), 0.055),
        },
        EntitySignal::Constant(c) => {
            let s = c.to_string();
            let len = s.len() as f32;
            (s, 0.20 / len.sqrt())
        }
    };

    let color = match e {
        EntitySignal::Signal(_) => Color32::BLACK,
        EntitySignal::Constant(_) => Rgba::from_rgb(0.0, 0.1, 0.4).into(),
    };

    let offset = 0.01;
    painter.text(
        rect.center() + Vec2::new(0.0, cam.scaled(offset)),
        Align2::CENTER_CENTER,
        &s,
        FontId {
            size: cam.scaled(size),
            family: FontFamily::Monospace,
        },
        color,
    );
}

#[derive(Debug)]
struct GuiEntity {
    entity: Entity,
}

impl From<fbo::Entity> for GuiEntity {
    fn from(e: fbo::Entity) -> Self {
        Self { entity: e.into() }
    }
}

impl GuiEntity {
    const PORT_DISTANCE: f32 = 0.6;
    const MARGIN: f32 = 0.11;

    fn hue(&self) -> f32 {
        match &self.entity.kind {
            EntityKind::ArithmeticCombinator(_) => 0.3,
            EntityKind::DeciderCombinator(_) => 0.8,
            EntityKind::ConstantCombinator(_) => 0.5,
            EntityKind::Other(_) => choose_hue(self.entity.name()),
        }
    }

    fn pos(&self) -> Pos2 {
        Pos2 {
            x: self.entity.x,
            y: self.entity.y,
        }
    }

    fn display_name(&self) -> &str {
        match &self.entity.kind {
            EntityKind::ArithmeticCombinator(_) => "Arithmetic Combinator",
            EntityKind::DeciderCombinator(_) => "Decider Combinator",
            EntityKind::ConstantCombinator(_) => "Constant Combinator",
            EntityKind::Other(_) => self.entity.name(),
        }
    }

    fn direction_vec(&self) -> Vec2 {
        match &self.entity.direction {
            fbo::Direction::North => Vec2::new(0.0, -1.0),
            fbo::Direction::NorthEast => Vec2::new(1.0, -1.0).normalized(),
            fbo::Direction::East => Vec2::new(1.0, 0.0),
            fbo::Direction::SouthEast => Vec2::new(1.0, 1.0).normalized(),
            fbo::Direction::South => Vec2::new(0.0, 1.0),
            fbo::Direction::SouthWest => Vec2::new(-1.0, 1.0).normalized(),
            fbo::Direction::West => Vec2::new(-1.0, 0.0),
            fbo::Direction::NorthWest => Vec2::new(-1.0, -1.0).normalized(),
        }
    }

    fn contains_point(&self, point: Pos2) -> bool {
        let size = match &self.entity.kind {
            EntityKind::ArithmeticCombinator(_) | EntityKind::DeciderCombinator(_) => {
                match &self.entity.direction {
                    Direction::East | Direction::West => Vec2::new(2.0, 1.0),
                    _ => Vec2::new(1.0, 2.0),
                }
            }
            _ => Vec2::splat(1.0),
        };

        let size = size - Vec2::splat(Self::MARGIN);

        let rect = Rect::from_center_size(self.pos(), size);
        rect.contains(point)
    }

    fn op_string(&self) -> &str {
        match &self.entity.kind {
            EntityKind::ArithmeticCombinator(ArithmeticCombinator { operator, .. }) => {
                match operator {
                    ArithmeticOperation::Add => "+",
                    ArithmeticOperation::Subtract => "-",
                    ArithmeticOperation::Multiply => "*",
                    ArithmeticOperation::Divide => "/",
                    ArithmeticOperation::Modulo => "%",
                    ArithmeticOperation::Exponentiate => "^",
                    ArithmeticOperation::LeftShift => "<<",
                    ArithmeticOperation::RightShift => ">>",
                    ArithmeticOperation::And => "&",
                    ArithmeticOperation::Or => "|",
                    ArithmeticOperation::Xor => "XOR",
                }
            }
            EntityKind::DeciderCombinator(DeciderCombinator { operator, .. }) => match operator {
                DeciderComparator::GreaterThan => ">",
                DeciderComparator::LessThan => "<",
                DeciderComparator::GreaterThanOrEqual => ">=",
                DeciderComparator::LessThanOrEqual => "<=",
                DeciderComparator::Equal => "==",
                DeciderComparator::NotEqual => "!=",
            },
            _ => "",
        }
    }

    fn input_port(&self) -> Pos2 {
        match &self.entity.kind {
            EntityKind::ArithmeticCombinator(_) | EntityKind::DeciderCombinator(_) => {
                self.pos() - self.direction_vec() * Self::PORT_DISTANCE
            }
            EntityKind::ConstantCombinator(_) => self.pos() + Vec2::splat(0.32),
            EntityKind::Other(_) => self.pos(),
        }
    }

    fn output_port(&self) -> Pos2 {
        match &self.entity.kind {
            EntityKind::ArithmeticCombinator(_) | EntityKind::DeciderCombinator { .. } => {
                self.pos() + self.direction_vec() * Self::PORT_DISTANCE
            }
            EntityKind::ConstantCombinator(_) => self.pos() + Vec2::splat(0.32),
            EntityKind::Other(_) => self.pos(),
        }
    }

    // TODO: Use actual direction
    fn port_from_index(&self, index: i32) -> Pos2 {
        match &self.entity.kind {
            EntityKind::ArithmeticCombinator { .. } | EntityKind::DeciderCombinator { .. } => {
                match index {
                    1 => self.input_port(),
                    2 => self.output_port(),
                    other => panic!("Invalid port index {other}."),
                }
            }
            EntityKind::ConstantCombinator { .. } => self.input_port(),
            EntityKind::Other { .. } => self.pos(),
        }
    }

    fn render_info(&self, ui: &mut Ui) {
        ui.heading(self.display_name());

        ui.separator();

        macro_rules! row {
                { $ui:expr, $label:expr, $($val:expr),+ } => {{
                    $ui.label(format!("{}:", $label));
                    $ui.label(format!("{:?}", $($val),+));
                    $ui.end_row();
                }}
            }

        egui::Grid::new("specific-entity-info").show(ui, |ui| {
            row! { ui, "Entity Number", self.entity.entity_number };
            row! { ui, "Posititon",     self.pos()                };
            row! { ui, "Direction",     self.entity.direction     };
        });

        ui.separator();

        egui::Grid::new("general-entity-info").show(ui, |ui| match &self.entity.kind {
            EntityKind::ArithmeticCombinator(ArithmeticCombinator {
                left,
                right,
                output,
                operator,
            }) => {
                row! { ui, "Left",     left     };
                row! { ui, "Right",    right    };
                row! { ui, "Operator", operator };
                row! { ui, "Output",   output   };
            }
            EntityKind::DeciderCombinator(DeciderCombinator {
                left,
                right,
                output,
                operator,
                copy_count_from_input,
            }) => {
                row! { ui, "Left",       left                  };
                row! { ui, "Right",      right                 };
                row! { ui, "Operator",   operator              };
                row! { ui, "Output",     output                };
                row! { ui, "Copy Count", copy_count_from_input };
            }
            EntityKind::ConstantCombinator(ConstantCombinator { signals }) => {
                for s in signals.values() {
                    row! { ui, s.name, s.count }
                }
            }
            EntityKind::Other(_) => {}
        });
    }

    fn draw(&self, app: &App, painter: &Painter, cam: &Camera) {
        let mut focused = false;

        let luminance = match &app.focused_entity {
            Some(en) if *en == self.entity.entity_number => {
                focused = true;
                0.4
            }
            Some(_) => 0.2,
            _ => 0.35,
        };

        let entity_stroke = match focused {
            true => Stroke::new(cam.scaled(0.01), Color32::WHITE),
            false => Stroke::NONE,
        };

        let icon_luminance = (luminance + 0.45_f32).min(1.0);

        match &self.entity.kind {
            EntityKind::ArithmeticCombinator(ArithmeticCombinator {
                left,
                right,
                output,
                ..
            })
            | EntityKind::DeciderCombinator(DeciderCombinator {
                left,
                right,
                output,
                ..
            }) => {
                let canvas_pos = cam.world_to_viewport(self.pos());

                const WIDTH: f32 = 1.0;
                const LENGTH: f32 = 2.0;

                let x_dir = self.direction_vec();
                let y_dir = x_dir.rot90().normalized();
                let front = canvas_pos + x_dir * cam.scaled(LENGTH / 2.0 - Self::MARGIN / 2.0);
                let back = canvas_pos - x_dir * cam.scaled(LENGTH / 2.0 - Self::MARGIN / 2.0);

                let top_front = front - y_dir * cam.scaled(WIDTH / 2.0 - Self::MARGIN / 2.0);
                let bot_front = front + y_dir * cam.scaled(WIDTH / 2.0 - Self::MARGIN / 2.0);
                let top_back = back - y_dir * cam.scaled(WIDTH / 2.0 - Self::MARGIN / 2.0);
                let bot_back = back + y_dir * cam.scaled(WIDTH / 2.0 - Self::MARGIN / 2.0);

                let points = vec![top_front, bot_front, bot_back, top_back];

                if points.iter().all(|p| !cam.viewport.contains(*p)) {
                    return;
                }

                let shape = PathShape::convex_polygon(
                    points,
                    Hsva::new(self.hue(), 0.5, luminance, 1.0),
                    entity_stroke,
                );
                painter.add(shape);

                let center_y_offset = cam.scaled(0.20);
                let operand_offset = cam.scaled(0.25);

                // Draw operands
                const ICON_SIZE: f32 = 0.30;
                draw_entity(
                    left,
                    painter,
                    cam,
                    canvas_pos + Vec2::LEFT * operand_offset + Vec2::DOWN * center_y_offset,
                    ICON_SIZE,
                    icon_luminance,
                );
                draw_entity(
                    right,
                    painter,
                    cam,
                    canvas_pos + Vec2::RIGHT * operand_offset + Vec2::DOWN * center_y_offset,
                    ICON_SIZE,
                    icon_luminance,
                );

                // Draw result
                draw_entity(
                    output,
                    painter,
                    cam,
                    canvas_pos + Vec2::UP * center_y_offset,
                    ICON_SIZE,
                    icon_luminance,
                );

                let font_id = FontId {
                    size: cam.scaled(0.07),
                    family: FontFamily::Monospace,
                };

                let port_size = cam.scaled(0.20);
                let fill = Color32::from_black_alpha(0x80);
                let stroke = Stroke::new(cam.scaled(0.01), Color32::WHITE);

                let draw_port_label = |label: &str, port: Pos2, direction: Vec2| {
                    let (align, pos) = match direction {
                        Vec2::DOWN => (Align2::CENTER_TOP, port + Vec2::DOWN * port_size / 2.0),
                        _ => (Align2::CENTER_BOTTOM, port + Vec2::UP * port_size / 2.0),
                    };
                    painter.text(pos, align, label, font_id.clone(), Color32::BLACK);
                };

                // Draw ports
                let dir = self.direction_vec();
                let input_port = cam.world_to_viewport(self.input_port());
                painter.circle(input_port, port_size / 2.0, fill, stroke);
                draw_port_label("INPUT", input_port, -dir);

                let output_port = cam.world_to_viewport(self.output_port());
                let rect = Rect::from_center_size(output_port, Vec2::splat(port_size));
                painter.rect(rect, 0.0, fill, stroke);
                draw_port_label("OUTPUT", output_port, dir);

                painter.text(
                    canvas_pos + Vec2::DOWN * center_y_offset,
                    Align2::CENTER_CENTER,
                    self.op_string(),
                    FontId {
                        size: cam.scale * 0.30,
                        family: FontFamily::Monospace,
                    },
                    Color32::BLACK,
                );
            }
            EntityKind::ConstantCombinator(ConstantCombinator { signals }) => {
                let canvas_pos = cam.world_to_viewport(self.pos());
                let size = Vec2::new(1.0, 1.0);
                let rect = Rect::from_center_size(
                    canvas_pos,
                    (size - Vec2::splat(Self::MARGIN)) * cam.scale,
                );

                painter.rect(
                    rect,
                    0.0,
                    Hsva::new(self.hue(), 0.5, luminance, 1.0),
                    entity_stroke,
                );

                const CENTER_OFFSET: f32 = 0.15;
                const MAX_WIDTH: f32 = 0.9;
                const SPACING: f32 = 0.15;

                let origin = rect.center();
                match signals.values().collect::<Vec<_>>().as_slice() {
                    [] => {}
                    [a] => {
                        const SIZE: f32 = 0.40;
                        draw_const_sig(a, painter, cam, origin, SIZE, icon_luminance);
                    }
                    [a, b] => {
                        const SIZE: f32 = 0.25;
                        draw_const_sig(
                            a,
                            painter,
                            cam,
                            origin + Vec2::new(-CENTER_OFFSET, 0.0) * cam.scale,
                            SIZE,
                            icon_luminance,
                        );
                        draw_const_sig(
                            b,
                            painter,
                            cam,
                            origin + Vec2::new(CENTER_OFFSET, 0.0) * cam.scale,
                            SIZE,
                            icon_luminance,
                        );
                    }
                    [a, b, c] => {
                        const SIZE: f32 = 0.25;
                        draw_const_sig(
                            a,
                            painter,
                            cam,
                            origin + Vec2::new(-CENTER_OFFSET, -CENTER_OFFSET) * cam.scale,
                            SIZE,
                            icon_luminance,
                        );
                        draw_const_sig(
                            b,
                            painter,
                            cam,
                            origin + Vec2::new(CENTER_OFFSET, -CENTER_OFFSET) * cam.scale,
                            SIZE,
                            icon_luminance,
                        );
                        draw_const_sig(
                            c,
                            painter,
                            cam,
                            origin + Vec2::new(0.0, CENTER_OFFSET) * cam.scale,
                            SIZE,
                            icon_luminance,
                        );
                    }
                    [a, b, c, d] => {
                        const SIZE: f32 = 0.25;
                        draw_const_sig(
                            a,
                            painter,
                            cam,
                            origin + Vec2::new(-CENTER_OFFSET, -CENTER_OFFSET) * cam.scale,
                            SIZE,
                            icon_luminance,
                        );
                        draw_const_sig(
                            b,
                            painter,
                            cam,
                            origin + Vec2::new(CENTER_OFFSET, -CENTER_OFFSET) * cam.scale,
                            SIZE,
                            icon_luminance,
                        );
                        draw_const_sig(
                            c,
                            painter,
                            cam,
                            origin + Vec2::new(-CENTER_OFFSET, CENTER_OFFSET) * cam.scale,
                            SIZE,
                            icon_luminance,
                        );
                        draw_const_sig(
                            d,
                            painter,
                            cam,
                            origin + Vec2::new(CENTER_OFFSET, CENTER_OFFSET) * cam.scale,
                            SIZE,
                            icon_luminance,
                        );
                    }
                    other => {
                        let len = other.len();
                        let item_width = len / 2 + len % 2;
                        let item_size = MAX_WIDTH / item_width as f32;
                        for i in 0..item_width {
                            let top_item = other[i];
                            let bot_item = other.get(i + item_width);
                            let x =
                                rect.left() + cam.scaled(item_size * i as f32 + item_size / 2.0);
                            let pos = Pos2::new(x, origin.y - cam.scaled(item_size / 2.0));
                            draw_const_sig(
                                top_item,
                                painter,
                                cam,
                                pos,
                                item_size * (1.0 - SPACING / 2.0),
                                icon_luminance,
                            );
                            if let Some(bot_item) = bot_item {
                                let pos = Pos2::new(x, origin.y + cam.scaled(item_size / 2.0));
                                draw_const_sig(
                                    bot_item,
                                    painter,
                                    cam,
                                    pos,
                                    item_size * (1.0 - SPACING / 2.0),
                                    icon_luminance,
                                );
                            }
                        }
                    }
                }

                // Draw port
                const PORT_SIZE: f32 = 0.15;
                let fill = Color32::from_black_alpha(0x80);
                let stroke = Stroke::new(cam.scaled(0.02), Color32::WHITE);
                let port = cam.world_to_viewport(self.input_port());
                painter.circle(port, cam.scaled(PORT_SIZE) / 2.0, fill, stroke);
            }
            EntityKind::Other(_) => {
                let pos = cam.world_to_viewport(self.pos());
                let bounding_box = Rect::from_center_size(pos, cam.scaled(Vec2::splat(1.0)));
                if !cam.viewport.intersects(bounding_box) {
                    return;
                }
                painter.circle(
                    pos,
                    cam.scaled(0.4),
                    Hsva::new(self.hue(), 0.5, luminance, 1.0),
                    entity_stroke,
                );
            }
        }
    }
}

#[derive(Debug)]
struct Camera {
    home: Pos2,
    pos: Pos2,
    scale: f32,
    viewport: Rect,
}

impl Default for Camera {
    fn default() -> Self {
        Self {
            home: Pos2::ZERO,
            pos: Pos2::ZERO,
            viewport: Rect::ZERO,
            scale: 100.0,
        }
    }
}

impl Camera {
    fn new(pos: Pos2) -> Self {
        Self {
            home: pos,
            pos,
            ..Default::default()
        }
    }

    fn home(&mut self) {
        self.pos = self.home;
    }

    fn scaled<X>(&self, x: X) -> X
    where
        X: Mul<f32, Output = X>,
    {
        x * self.scale
    }

    fn world_to_viewport(&self, world_pos: Pos2) -> Pos2 {
        let rel_pos = world_pos - self.pos;
        self.viewport.center() + rel_pos * self.scale
    }

    fn canvas_to_world(&self, canvas_pos: Pos2) -> Pos2 {
        let rel_pos = (canvas_pos - self.viewport.center()) / self.scale;

        self.pos + rel_pos
    }

    fn canvas_to_rel(&self, canvas_pos: Vec2) -> Vec2 {
        canvas_pos / self.scale
    }
}

#[derive(Default)]
struct App {
    camera: Camera,
    stack: Vec<Container>,
    entities: Vec<GuiEntity>,
    focused_entity: Option<fbo::EntityNumber>,
}

impl eframe::App for App {
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        egui::TopBottomPanel::top("top-bar")
            .frame(Frame {
                inner_margin: Margin::symmetric(4.0, 4.0),
                fill: Hsva::new(0.0, 0.0, 0.005, 1.0).into(),
                ..Default::default()
            })
            .show(ctx, |ui| {
                ui.horizontal(|ui| {
                    ui.heading("Chef Inspector");
                    ui.button("Back").clicked().then(|| {
                        self.close_container();
                    });
                });
            });

        egui::SidePanel::right("right-panel")
            .resizable(true)
            .show_animated(ctx, self.focused_entity.is_some(), |ui| {
                if let Some(entity) = self.entities.iter().find(|e| {
                    e.entity.entity_number
                        == self
                            .focused_entity
                            .expect("We should not be here without a focused entity.")
                }) {
                    entity.render_info(ui);
                }
            });

        egui::CentralPanel::default()
            .frame(Frame {
                fill: Hsva::new(0.0, 0.0, 0.01, 1.0).into(),
                ..Default::default()
            })
            .show(ctx, |ui| match self.stack.last().unwrap() {
                Container::Blueprint(_) => self.view_blueprint(ui),
                Container::BlueprintBook(book) => self.view_book(ui, book.clone()),
                Container::DeconstructionPlanner(_) => todo!(),
                Container::UpgradePlanner(_) => todo!(),
            });
    }
}

impl App {
    fn new(container: Container) -> Self {
        let mut app = Self::default();
        app.open_container(container);
        app
    }

    fn view_blueprint(&mut self, ui: &mut egui::Ui) {
        let size = ui.available_size_before_wrap();

        let (resp, painter) = ui.allocate_painter(size, Sense::click_and_drag());

        // Zoom
        if resp.hovered() {
            ui.input(|i| {
                let delta_scale = i.smooth_scroll_delta.y;
                let delta_scale = 0.01 * delta_scale * self.camera.scale;
                self.camera.scale += delta_scale;
                self.camera.scale = self.camera.scale.clamp(10.0, 500.0);
            });
        }

        // Drag view
        if resp.dragged() {
            let delta = self.camera.canvas_to_rel(resp.drag_delta());
            self.camera.pos -= delta;
        }

        if resp.clicked() {
            if let Some(viewport_pos) = resp.interact_pointer_pos() {
                let pos = self.camera.canvas_to_world(viewport_pos);
                self.focused_entity = self
                    .entities
                    .iter()
                    .find(|e| e.contains_point(pos))
                    .map(|e| e.entity.entity_number);
            }
        }

        // Keys
        ui.input(|i| {
            macro_rules! bind {
                ($bind:tt, $($key:expr),+ => $e:expr) => {{
                    if $(i.$bind($key))|+ {
                        $e;
                    }
                }}
            }
            macro_rules! bind_down {
                ($($key:expr),+ => $e:expr) => {bind!(key_down, $($key),+ => $e)}
            }
            macro_rules! bind_pressed {
                ($($key:expr),+ => $e:expr) => {bind!(key_pressed, $($key),+ => $e)}
            }

            let mut velocity = Vec2::ZERO;
            bind_down!(Key::ArrowRight, Key::D => velocity.x += 1.0);
            bind_down!(Key::ArrowLeft,  Key::A => velocity.x -= 1.0);
            bind_down!(Key::ArrowUp,    Key::W => velocity.y -= 1.0);
            bind_down!(Key::ArrowDown,  Key::S => velocity.y += 1.0);

            bind_pressed!(Key::Escape => self.focused_entity = None);
            bind_pressed!(Key::H => self.camera.home());

            if velocity != Vec2::ZERO {
                self.camera.pos += velocity * 0.001 * self.camera.scale;
            }
        });

        // Draw canvas
        self.camera.viewport = ui.ctx().input(|i| i.screen_rect());
        painter.rect_filled(painter.clip_rect(), 0.0, Rgba::from_white_alpha(0.01));

        // Draw combinators
        for e in &self.entities {
            e.draw(self, &painter, &self.camera)
        }

        for this_entity in &self.entities {
            for (this_port, other_entity_number, other_port, wire) in
                &this_entity.entity.connections
            {
                let this_port = this_entity.port_from_index(usize::from(*this_port) as i32);
                let other_entity = self
                    .entities
                    .iter()
                    .find(|e| e.entity.entity_number == *other_entity_number)
                    .expect("Entity not found");

                let other_port = other_entity.port_from_index(*other_port);

                let (wire_opacity, width) = match self.focused_entity {
                    Some(focused)
                        if focused == this_entity.entity.entity_number
                            || focused == *other_entity_number =>
                    {
                        (0.7, 0.04)
                    }
                    Some(_) => (0.1, 0.01),
                    _ => (0.6, 0.02),
                };

                // Draw wire
                let from = self.camera.world_to_viewport(this_port);
                let to = self.camera.world_to_viewport(other_port);

                let (color, sag) = match wire {
                    WireColor::Red => (
                        Rgba::from_rgba_unmultiplied(1.0, 0.0, 0.0, wire_opacity),
                        vec2(-0.08, 0.15),
                    ),
                    WireColor::Green => (
                        Rgba::from_rgba_unmultiplied(0.0, 1.0, 0.0, wire_opacity),
                        vec2(0.08, -0.15),
                    ),
                };

                let target = (from + to.to_vec2()) / 2.0 + self.camera.scaled(sag);

                if !self.camera.viewport.contains(from)
                    && !self.camera.viewport.contains(to)
                    && !self.camera.viewport.contains(target)
                {
                    continue;
                }

                painter.add(Shape::CubicBezier(CubicBezierShape::from_points_stroke(
                    [from, target, target, to],
                    false,
                    Color32::TRANSPARENT,
                    Stroke::new(self.camera.scaled(width), color),
                )));
            }
        }
    }

    fn at_top_level(&self) -> bool {
        self.stack.len() == 1
    }

    fn close_container(&mut self) {
        if !self.at_top_level() {
            self.stack.pop();
        }
    }

    fn open_container(&mut self, container: Container) {
        if let Container::Blueprint(blueprint) = &container {
            let entities: Vec<_> = blueprint
                .entities
                .clone()
                .into_iter()
                .map(GuiEntity::from)
                .collect();

            // Position camera in the middle of the entities
            let cam_pos = if let Ok(len) = u16::try_from(entities.len()) {
                let len = len as f32;
                entities
                    .iter()
                    .map(|e| e.pos())
                    .fold(Pos2::ZERO, |acc, p| acc + p.to_vec2())
                    / len
            } else {
                Pos2::ZERO
            };

            // Set app state
            self.entities = entities;
            self.camera = Camera::new(cam_pos);
        }

        self.stack.push(container);
    }

    fn view_book(&mut self, ui: &mut egui::Ui, book: BlueprintBook) {
        const SQUARE_SIZE: f32 = 140.0;
        const TEXT_SIZE: f32 = SQUARE_SIZE * 0.14;
        const MARGIN: f32 = 10.0;
        const OUTER_SIZE: f32 = SQUARE_SIZE + 2.0 * MARGIN;

        ui.add_space(7.0);
        ui.allocate_ui(Vec2::new(ui.available_width(), 30.0), |ui| {
            ui.centered_and_justified(|ui| {
                ui.heading(
                    book.label
                        .map_or("Blueprint Book".to_string(), |l| l.clone()),
                );
            });
        });
        ui.separator();
        ui.add_space(10.0);

        let label_font = FontId {
            size: TEXT_SIZE,
            family: FontFamily::Proportional,
        };

        ScrollArea::vertical().show(ui, |ui| {
            ui.horizontal_wrapped(|ui| {
                for book_item in &book.blueprints {
                    let name = match &book_item.item {
                        Container::BlueprintBook(b) => {
                            b.label.clone().unwrap_or("Book".to_string())
                        }
                        Container::Blueprint(b) => {
                            b.label.clone().unwrap_or("Blueprint".to_string())
                        }
                        Container::DeconstructionPlanner(_) => todo!(),
                        Container::UpgradePlanner(_) => todo!(),
                    };

                    let (resp, p) = ui.allocate_painter(
                        Vec2::new(OUTER_SIZE, OUTER_SIZE + TEXT_SIZE * 2.0),
                        Sense::click(),
                    );

                    let square = Rect::from_center_size(
                        resp.rect.center_top() + Vec2::DOWN * OUTER_SIZE / 2.0,
                        Vec2::splat(SQUARE_SIZE),
                    );

                    p.rect_filled(square, 10.0, Hsva::new(choose_hue(&name), 0.6, 0.3, 1.0));

                    p.text(
                        square.center_bottom() + Vec2::DOWN * 2.0,
                        Align2::CENTER_TOP,
                        name,
                        label_font.clone(),
                        Color32::WHITE,
                    );

                    resp.clicked().then(|| {
                        self.open_container(book_item.item.clone());
                    });
                    resp.hovered().then(|| {
                        ui.ctx().set_cursor_icon(egui::CursorIcon::PointingHand);
                    });
                }
            });
        });
    }
}
