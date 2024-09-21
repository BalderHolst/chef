use std::{
    collections::{BTreeMap, HashMap},
    ops::Mul,
};

use factorio_blueprint::{
    objects::{self as fbo, ArithmeticOperation, BlueprintBook, DeciderComparator, OneBasedIndex},
    Container,
};

use eframe::{
    egui::{
        self, Align2, Color32, FontFamily, FontId, Frame, Key, Margin, Painter, Pos2, Rect, Rgba,
        ScrollArea, Sense, Shape, Stroke, Vec2,
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

#[derive(Debug)]
struct ConstantSignal {
    name: String,
    count: i32,
}

impl ConstantSignal {
    fn hue(&self) -> f32 {
        choose_hue(&self.name)
    }

    fn draw(&self, painter: &Painter, cam: &Camera, pos: Pos2, size: f32) {
        let size = cam.scaled(size);
        let rect = Rect::from_center_size(pos, Vec2::new(size, size));
        painter.rect_filled(
            rect,
            cam.scaled(ROUNDING),
            Hsva::new(self.hue(), 0.8, 1.0, 1.0),
        );

        let (s, font_size) = match self.name.strip_prefix("signal-") {
            Some(letter) if letter.len() == 1 => (letter.to_string(), 0.14),
            Some(color) => (color.to_string(), 0.10),
            None => (self.name.replace('-', "\n"), 0.045),
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
            self.count.to_string(),
            FontId {
                size: cam.scaled(0.05),
                family: FontFamily::Monospace,
            },
            Color32::BLACK,
        );
    }
}

#[derive(Debug)]
enum OpSig {
    Signal(String),
    Constant(i32),
}

impl OpSig {
    fn signal<S>(s: S) -> Self
    where
        S: ToString,
    {
        Self::Signal(s.to_string())
    }

    fn constant(c: i32) -> Self {
        Self::Constant(c)
    }

    fn unknown() -> Self {
        Self::Signal("UNKNOWN".to_string())
    }

    fn hue(&self) -> f32 {
        match self {
            Self::Signal(s) => choose_hue(s),
            Self::Constant(_) => 0.083,
        }
    }

    fn draw(&self, painter: &Painter, cam: &Camera, pos: Pos2, size: f32) {
        let size = cam.scaled(size);
        let rect = Rect::from_center_size(pos, Vec2::new(size, size));
        let painter = painter.with_clip_rect(rect);
        painter.rect_filled(
            rect,
            cam.scaled(ROUNDING),
            Hsva::new(self.hue(), 0.8, 1.0, 1.0),
        );

        let (s, size) = match &self {
            Self::Signal(s) => match s.strip_prefix("signal-") {
                //Some("each") => todo!(),
                //Some("anything") => todo!(),
                //Some("everything") => todo!(),
                Some(letter) if letter.len() == 1 => (letter.to_string(), 0.20),
                Some(color) => (color.to_string(), 0.10),
                None => (s.replace('-', "\n"), 0.055),
            },
            Self::Constant(c) => {
                let s = c.to_string();
                let len = s.len() as f32;
                (s, 0.20 / len.sqrt())
            }
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
            Color32::BLACK,
        );
    }
}

#[derive(Debug, Clone)]
enum WireColor {
    Red,
    Green,
}

impl Copy for WireColor {}

type EntityConnections = Vec<(fbo::OneBasedIndex, fbo::EntityNumber, i32, WireColor)>;

#[derive(Debug)]
struct GuiEntity {
    entity_number: fbo::OneBasedIndex,
    name: String,
    pos: Pos2,
    direction: Vec2,
    connections: EntityConnections,
    kind: GuiEntityKind,
}

#[derive(Debug)]
enum GuiEntityKind {
    ArithmeticCombinator {
        left: OpSig,
        right: OpSig,
        output: OpSig,
        operator: ArithmeticOperation,
    },
    DeciderCombinator {
        left: OpSig,
        right: OpSig,
        output: OpSig,
        operator: DeciderComparator,
    },
    ConstantCombinator {
        signals: BTreeMap<fbo::OneBasedIndex, ConstantSignal>,
    },
    Other,
}

fn pos_from_fbo_position(pos: fbo::Position) -> Pos2 {
    let x: f64 = pos.x.into();
    let y: f64 = pos.y.into();
    Pos2::new(x as f32, y as f32)
}

impl From<fbo::Entity> for GuiEntity {
    fn from(e: fbo::Entity) -> Self {
        let connections = (|| {
            Some({
                match e.connections?.clone() {
                    fbo::EntityConnections::StringIdx(conns) => conns
                        .iter()
                        .map(|(port, conn)| {
                            let port = port.parse::<fbo::OneBasedIndex>().unwrap();
                            (port, conn.clone())
                        })
                        .collect::<HashMap<OneBasedIndex, fbo::ConnectionPoint>>(),
                    fbo::EntityConnections::NumberIdx(conns) => conns,
                }
                .into_iter()
                .flat_map(|(port, conn)| {
                    conn.red
                        .map(move |red_cons| {
                            red_cons.into_iter().map(move |red_conn| {
                                (
                                    port,
                                    red_conn.entity_id,
                                    red_conn.circuit_id.unwrap_or(1),
                                    WireColor::Red,
                                )
                            })
                        })
                        .into_iter()
                        .flatten()
                        .chain(
                            conn.green
                                .map(move |green_cons| {
                                    green_cons.into_iter().map(move |green_conn| {
                                        (
                                            port,
                                            green_conn.entity_id,
                                            green_conn.circuit_id.unwrap_or(1),
                                            WireColor::Green,
                                        )
                                    })
                                })
                                .into_iter()
                                .flatten(),
                        )
                })
                .collect::<Vec<_>>()
            })
        })()
        .unwrap_or(vec![]);

        let direction = match &e.direction {
            fbo::Direction::North => Vec2::new(0.0, -1.0),
            fbo::Direction::NorthEast => Vec2::new(1.0, -1.0).normalized(),
            fbo::Direction::East => Vec2::new(1.0, 0.0),
            fbo::Direction::SouthEast => Vec2::new(1.0, 1.0).normalized(),
            fbo::Direction::South => Vec2::new(0.0, 1.0),
            fbo::Direction::SouthWest => Vec2::new(-1.0, 1.0).normalized(),
            fbo::Direction::West => Vec2::new(-1.0, 0.0),
            fbo::Direction::NorthWest => Vec2::new(-1.0, -1.0).normalized(),
        };

        let kind = match e.name.as_str() {
            "arithmetic-combinator" => {
                let c = e
                    .control_behavior
                    .as_ref()
                    .unwrap()
                    .arithmetic_conditions
                    .as_ref()
                    .unwrap();
                let left = c.first_signal.as_ref().map_or(
                    c.first_constant.map_or(OpSig::unknown(), OpSig::constant),
                    |s| OpSig::signal(&s.name),
                );
                let right = c.second_signal.as_ref().map_or(
                    c.second_constant.map_or(OpSig::unknown(), OpSig::constant),
                    |s| OpSig::signal(&s.name),
                );
                let output = c
                    .output_signal
                    .as_ref()
                    .map_or(OpSig::unknown(), |s| OpSig::signal(&s.name));
                GuiEntityKind::ArithmeticCombinator {
                    left,
                    right,
                    output,
                    operator: c.operation.clone(),
                }
            }
            "decider-combinator" => {
                let c = e
                    .control_behavior
                    .as_ref()
                    .unwrap()
                    .decider_conditions
                    .as_ref()
                    .unwrap();
                let left = c
                    .first_signal
                    .as_ref()
                    .map_or(OpSig::unknown(), |s| OpSig::signal(&s.name));
                let right = c
                    .second_signal
                    .as_ref()
                    .map_or(OpSig::unknown(), |s| OpSig::signal(&s.name));
                let output = c
                    .output_signal
                    .as_ref()
                    .map_or(OpSig::unknown(), |s| OpSig::signal(&s.name));

                GuiEntityKind::DeciderCombinator {
                    left,
                    right,
                    output,
                    operator: c.comparator.clone(),
                }
            }
            "constant-combinator" => {
                let signals = (|| {
                    Some(
                        e.control_behavior?
                            .filters?
                            .into_iter()
                            .map(|filter| {
                                (
                                    filter.index,
                                    ConstantSignal {
                                        name: filter.signal.name,
                                        count: filter.count,
                                    },
                                )
                            })
                            .collect::<BTreeMap<OneBasedIndex, ConstantSignal>>(),
                    )
                })()
                .unwrap_or(BTreeMap::default());

                GuiEntityKind::ConstantCombinator { signals }
            }
            _ => GuiEntityKind::Other,
        };

        GuiEntity {
            entity_number: e.entity_number,
            name: e.name,
            pos: pos_from_fbo_position(e.position),
            direction,
            connections,
            kind,
        }
    }
}

impl GuiEntity {
    const PORT_DISTANCE: f32 = 0.6;

    fn hue(&self) -> f32 {
        match &self.kind {
            GuiEntityKind::ArithmeticCombinator { .. } => 0.1,
            GuiEntityKind::DeciderCombinator { .. } => 0.8,
            GuiEntityKind::ConstantCombinator { .. } => 0.5,
            GuiEntityKind::Other => choose_hue(&self.name),
        }
    }

    fn input_port(&self) -> Pos2 {
        match &self.kind {
            GuiEntityKind::ArithmeticCombinator { .. }
            | GuiEntityKind::DeciderCombinator { .. } => {
                self.pos + self.direction * Self::PORT_DISTANCE
            }
            GuiEntityKind::ConstantCombinator { .. } => self.pos + Vec2::splat(0.32),
            GuiEntityKind::Other => self.pos,
        }
    }

    fn output_port(&self) -> Pos2 {
        match &self.kind {
            GuiEntityKind::ArithmeticCombinator { .. }
            | GuiEntityKind::DeciderCombinator { .. } => {
                self.pos - self.direction * Self::PORT_DISTANCE
            }
            GuiEntityKind::ConstantCombinator { .. } => self.pos + Vec2::splat(0.32),
            GuiEntityKind::Other => self.pos,
        }
    }

    // TODO: Use actual direction
    fn port_from_index(&self, index: i32) -> Pos2 {
        match &self.kind {
            GuiEntityKind::ArithmeticCombinator { .. }
            | GuiEntityKind::DeciderCombinator { .. } => match index {
                1 => self.input_port(),
                2 => self.output_port(),
                other => panic!("Invalid port index {other}."),
            },
            GuiEntityKind::ConstantCombinator { .. } => self.input_port(),
            GuiEntityKind::Other { .. } => self.pos,
        }
    }

    fn draw(&self, painter: &Painter, cam: &Camera) {
        // All sizes are in taller than wide if they are not square

        const MARGIN: f32 = 0.11;

        match &self.kind {
            GuiEntityKind::ArithmeticCombinator {
                left,
                right,
                output,
                ..
            }
            | GuiEntityKind::DeciderCombinator {
                left,
                right,
                output,
                ..
            } => {
                let canvas_pos = cam.world_to_viewport(self.pos);

                const WIDTH: f32 = 1.0;
                const LENGTH: f32 = 2.0;

                let x_dir = self.direction;
                let y_dir = x_dir.rot90().normalized();
                let front = canvas_pos + x_dir * cam.scaled(LENGTH / 2.0 - MARGIN / 2.0);
                let back = canvas_pos - x_dir * cam.scaled(LENGTH / 2.0 - MARGIN / 2.0);

                let top_front = front - y_dir * cam.scaled(WIDTH / 2.0 - MARGIN / 2.0);
                let bot_front = front + y_dir * cam.scaled(WIDTH / 2.0 - MARGIN / 2.0);
                let top_back = back - y_dir * cam.scaled(WIDTH / 2.0 - MARGIN / 2.0);
                let bot_back = back + y_dir * cam.scaled(WIDTH / 2.0 - MARGIN / 2.0);

                let points = vec![top_front, bot_front, bot_back, top_back];

                if points.iter().all(|p| !cam.viewport.contains(*p)) {
                    return;
                }

                let shape = PathShape::convex_polygon(
                    points,
                    Hsva::new(self.hue(), 0.5, 0.5, 1.0),
                    Stroke::NONE,
                );

                painter.add(shape);

                let title_pos = if x_dir == Vec2::UP {
                    canvas_pos + Vec2::UP * cam.scaled(LENGTH / 2.0 - MARGIN / 2.0 - 0.04)
                } else {
                    canvas_pos + Vec2::UP * cam.scaled(WIDTH / 2.0 - MARGIN / 2.0 - 0.04)
                };

                painter.text(
                    title_pos,
                    Align2::CENTER_TOP,
                    &self.name,
                    FontId {
                        size: cam.scale * 0.065,
                        family: FontFamily::Monospace,
                    },
                    Color32::BLACK,
                );

                let center_y_offset = cam.scaled(0.20);
                let operand_offset = cam.scaled(0.25);

                // Draw operands
                const ICON_SIZE: f32 = 0.30;
                left.draw(
                    painter,
                    cam,
                    canvas_pos + Vec2::LEFT * operand_offset + Vec2::DOWN * center_y_offset,
                    ICON_SIZE,
                );
                right.draw(
                    painter,
                    cam,
                    canvas_pos + Vec2::RIGHT * operand_offset + Vec2::DOWN * center_y_offset,
                    ICON_SIZE,
                );

                // Draw result
                output.draw(
                    painter,
                    cam,
                    canvas_pos + Vec2::UP * center_y_offset,
                    ICON_SIZE,
                );

                let port_size = cam.scaled(0.20);
                let fill = Color32::from_black_alpha(0x80);
                let stroke = Stroke::new(cam.scaled(0.02), Color32::WHITE);

                // Draw ports
                let input_port = cam.world_to_viewport(self.input_port());
                painter.circle(input_port, port_size / 2.0, fill, stroke);
                let output_port = cam.world_to_viewport(self.output_port());
                let rect = Rect::from_center_size(output_port, Vec2::splat(port_size));
                painter.rect(rect, 0.0, fill, stroke);

                let op = match &self.kind {
                    GuiEntityKind::ArithmeticCombinator { operator, .. } => operator.to_string(),
                    GuiEntityKind::DeciderCombinator { operator, .. } => operator.to_string(),
                    _ => unreachable!(),
                };

                painter.text(
                    canvas_pos + Vec2::DOWN * center_y_offset,
                    Align2::CENTER_CENTER,
                    op,
                    FontId {
                        size: cam.scale * 0.30,
                        family: FontFamily::Monospace,
                    },
                    Color32::BLACK,
                );
            }
            GuiEntityKind::ConstantCombinator { signals } => {
                let canvas_pos = cam.world_to_viewport(self.pos);
                let size = Vec2::new(1.0, 1.0);
                let rect =
                    Rect::from_center_size(canvas_pos, (size - Vec2::splat(MARGIN)) * cam.scale);

                painter.rect_filled(
                    rect,
                    cam.scaled(ROUNDING),
                    Hsva::new(self.hue(), 0.5, 0.4, 1.0),
                );

                painter.text(
                    rect.center_top() + Vec2::new(0.0, 0.04) * cam.scale,
                    Align2::CENTER_TOP,
                    &self.name,
                    FontId {
                        size: cam.scale * 0.065,
                        family: FontFamily::Monospace,
                    },
                    Color32::BLACK,
                );

                const CENTER_OFFSET: f32 = 0.15;
                const MAX_WIDTH: f32 = 0.9;
                const SPACING: f32 = 0.15;

                let origin = rect.center();
                match signals.values().collect::<Vec<_>>().as_slice() {
                    [] => {}
                    [a] => {
                        const SIZE: f32 = 0.40;
                        a.draw(painter, cam, origin, SIZE);
                    }
                    [a, b] => {
                        const SIZE: f32 = 0.25;
                        a.draw(
                            painter,
                            cam,
                            origin + Vec2::new(-CENTER_OFFSET, 0.0) * cam.scale,
                            SIZE,
                        );
                        b.draw(
                            painter,
                            cam,
                            origin + Vec2::new(CENTER_OFFSET, 0.0) * cam.scale,
                            SIZE,
                        );
                    }
                    [a, b, c] => {
                        const SIZE: f32 = 0.25;
                        a.draw(
                            painter,
                            cam,
                            origin + Vec2::new(-CENTER_OFFSET, -CENTER_OFFSET) * cam.scale,
                            SIZE,
                        );
                        b.draw(
                            painter,
                            cam,
                            origin + Vec2::new(CENTER_OFFSET, -CENTER_OFFSET) * cam.scale,
                            SIZE,
                        );
                        c.draw(
                            painter,
                            cam,
                            origin + Vec2::new(0.0, CENTER_OFFSET) * cam.scale,
                            SIZE,
                        );
                    }
                    [a, b, c, d] => {
                        const SIZE: f32 = 0.25;
                        a.draw(
                            painter,
                            cam,
                            origin + Vec2::new(-CENTER_OFFSET, -CENTER_OFFSET) * cam.scale,
                            SIZE,
                        );
                        b.draw(
                            painter,
                            cam,
                            origin + Vec2::new(CENTER_OFFSET, -CENTER_OFFSET) * cam.scale,
                            SIZE,
                        );
                        c.draw(
                            painter,
                            cam,
                            origin + Vec2::new(-CENTER_OFFSET, CENTER_OFFSET) * cam.scale,
                            SIZE,
                        );
                        d.draw(
                            painter,
                            cam,
                            origin + Vec2::new(CENTER_OFFSET, CENTER_OFFSET) * cam.scale,
                            SIZE,
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
                            top_item.draw(painter, cam, pos, item_size * (1.0 - SPACING / 2.0));
                            if let Some(bot_item) = bot_item {
                                let pos = Pos2::new(x, origin.y + cam.scaled(item_size / 2.0));
                                bot_item.draw(painter, cam, pos, item_size * (1.0 - SPACING / 2.0));
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
            GuiEntityKind::Other => {}
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

    fn _canvas_to_world(&self, canvas_pos: Pos2) -> Pos2 {
        let rel_pos = (canvas_pos - self.viewport.center()) / self.scale;

        self.pos + rel_pos
    }

    fn canvas_to_rel(&self, canvas_pos: Vec2) -> Vec2 {
        canvas_pos / self.scale
    }
}

struct App {
    camera: Camera,
    stack: Vec<Container>,
    entities: Vec<GuiEntity>,
}

impl eframe::App for App {
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        egui::TopBottomPanel::top("right-panel")
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
        let mut app = Self {
            camera: Camera::new(Pos2::ZERO),
            stack: vec![],
            entities: vec![],
        };
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

        if resp.dragged() {
            let delta = self.camera.canvas_to_rel(resp.drag_delta());
            self.camera.pos -= delta;
        }

        // Keys
        ui.input(|i| {
            let mut velocity = Vec2::ZERO;
            if i.key_down(Key::ArrowRight) | i.key_down(Key::D) {
                velocity.x += 1.0;
            }
            if i.key_down(Key::ArrowLeft) | i.key_down(Key::A) {
                velocity.x -= 1.0;
            }
            if i.key_down(Key::ArrowUp) | i.key_down(Key::W) {
                velocity.y -= 1.0;
            }
            if i.key_down(Key::ArrowDown) | i.key_down(Key::S) {
                velocity.y += 1.0;
            }

            if i.key_down(Key::H) {
                self.camera.home()
            }

            if velocity != Vec2::ZERO {
                self.camera.pos += velocity * 0.001 * self.camera.scale;
            }
        });

        // Draw canvas
        self.camera.viewport = painter.clip_rect();
        painter.rect_filled(painter.clip_rect(), 0.0, Rgba::from_white_alpha(0.01));

        // Draw combinators
        for e in &self.entities {
            e.draw(&painter, &self.camera)
        }

        for e in &self.entities {
            for (port, other_en, other_port, wire) in &e.connections {
                self.draw_conn(e, *port, *other_en, *other_port, *wire, &painter)
            }
        }
    }

    // Draw connections
    fn draw_conn(
        &self,
        e: &GuiEntity,
        this_port: fbo::OneBasedIndex,
        other_entity_id: fbo::EntityNumber,
        other_port: i32,
        wire: WireColor,
        painter: &Painter,
    ) {
        const WIRE_OPACITY: f32 = 0.6;

        let this_port = e.port_from_index(usize::from(this_port) as i32);
        let other_entity = self
            .entities
            .iter()
            .find(|e| e.entity_number == other_entity_id)
            .expect("Entity not found");

        let other_port = other_entity.port_from_index(other_port);

        // Draw wire
        let from = self.camera.world_to_viewport(this_port);
        let to = self.camera.world_to_viewport(other_port);

        let (color, sag) = match wire {
            WireColor::Red => (
                Rgba::from_rgba_unmultiplied(1.0, 0.0, 0.0, WIRE_OPACITY),
                0.22,
            ),
            WireColor::Green => (
                Rgba::from_rgba_unmultiplied(0.0, 1.0, 0.0, WIRE_OPACITY),
                0.19,
            ),
        };

        let target = (from + to.to_vec2()) / 2.0 + Vec2::DOWN * self.camera.scaled(sag);

        if !self.camera.viewport.contains(from)
            && !self.camera.viewport.contains(to)
            && !self.camera.viewport.contains(target)
        {
            return;
        }

        painter.add(Shape::CubicBezier(CubicBezierShape::from_points_stroke(
            [from, target, target, to],
            false,
            Color32::TRANSPARENT,
            Stroke::new(self.camera.scaled(0.02), color),
        )));
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
                    .map(|e| e.pos)
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
