use std::ops::Mul;

use factorio_blueprint::{
    objects::{self as fbo, Blueprint, BlueprintBook, ConnectionPoint, OneBasedIndex},
    Container,
};

use eframe::{
    egui::{
        self, Align, Align2, Area, Color32, FontFamily, FontId, Frame, InnerResponse, Key, Label,
        Layout, Margin, Painter, Pos2, Rangef, Rect, Rgba, ScrollArea, Sense, Stroke, Vec2,
    },
    epaint::Hsva,
};

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

    fn draw(&self, painter: &Painter, cam: &Camera, pos: Pos2) {
        let size = cam.scaled(0.30);
        let rect = Rect::from_center_size(pos, Vec2::new(size, size));
        let painter = painter.with_clip_rect(rect);
        painter.rect_filled(rect, cam.scaled(0.05), Hsva::new(self.hue(), 0.8, 1.0, 1.0));

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

struct GuiEntity {
    inner: fbo::Entity,
}

impl GuiEntity {
    const PORT_DISTANCE: f32 = 0.6;

    fn new(entity: fbo::Entity) -> Self {
        Self { inner: entity }
    }

    fn name(&self) -> &str {
        &self.inner.name
    }

    fn pos(&self) -> Pos2 {
        let x: f64 = self.inner.position.x.into();
        let y: f64 = self.inner.position.y.into();
        Pos2::new(x as f32, y as f32)
    }

    fn hue(&self) -> f32 {
        choose_hue(self.name())
    }

    // TODO: Use actual direction
    fn input_port(&self) -> Pos2 {
        match self.name() {
            "arithmetic-combinator" | "decider-combinator" => {
                self.pos() + Vec2::new(0.0, Self::PORT_DISTANCE)
            }
            _ => self.pos(),
        }
    }

    // TODO: Use actual direction
    fn output_port(&self) -> Pos2 {
        match self.name() {
            "arithmetic-combinator" | "decider-combinator" => {
                self.pos() + Vec2::new(0.0, -Self::PORT_DISTANCE)
            }
            _ => self.pos(),
        }
    }

    // TODO: Use actual direction
    fn port_from_index(&self, index: i32) -> Pos2 {
        match self.name() {
            "arithmetic-combinator" | "decider-combinator" => match index {
                1 => self.input_port(),
                2 => self.output_port(),
                other => panic!("Invalid port index {other}."),
            },
            "constant-combinator" => self.pos(),
            other => todo!("{other}"),
        }
    }

    fn draw_combinator(
        &self,
        painter: &Painter,
        cam: &Camera,
        left: OpSig,
        right: OpSig,
        output: OpSig,
        op: String,
    ) {
        const MARGIN: f32 = 0.11;
        let canvas_pos = cam.world_to_viewport(self.pos());

        let size = Vec2::new(1.0, 2.0);
        let rect = Rect::from_center_size(canvas_pos, (size - Vec2::splat(MARGIN)) * cam.scale);

        let painter = painter.with_clip_rect(rect);
        painter.rect_filled(rect, 0.05 * cam.scale, Hsva::new(self.hue(), 0.5, 0.5, 1.0));

        painter.text(
            rect.center_top() + Vec2::new(0.0, 0.10) * cam.scale,
            Align2::CENTER_CENTER,
            self.name(),
            FontId {
                size: cam.scale * 0.065,
                family: FontFamily::Monospace,
            },
            Color32::BLACK,
        );

        let center_y_offset = cam.scaled(0.20);
        let operand_offset = cam.scaled(0.25);

        // Draw operands
        left.draw(
            &painter,
            cam,
            canvas_pos + Vec2::LEFT * operand_offset + Vec2::DOWN * center_y_offset,
        );
        right.draw(
            &painter,
            cam,
            canvas_pos + Vec2::RIGHT * operand_offset + Vec2::DOWN * center_y_offset,
        );

        // Draw result
        output.draw(&painter, cam, canvas_pos + Vec2::UP * center_y_offset);

        let port_size = cam.scaled(0.20);
        let fill = Color32::from_black_alpha(0x80);
        let stroke = Stroke::new(cam.scaled(0.02), Color32::WHITE);

        // Draw ports
        let input_port = cam.world_to_viewport(self.input_port());
        painter.circle(input_port, port_size / 2.0, fill, stroke);
        let output_port = cam.world_to_viewport(self.output_port());
        let rect = Rect::from_center_size(output_port, Vec2::splat(port_size));
        painter.rect(rect, 0.0, fill, stroke);

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

    fn draw(&self, painter: &Painter, camera: &Camera) {
        // All sizes are in taller than wide if they are not square
        match self.name() {
            "decider-combinator" => {
                let c = self
                    .inner
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
                self.draw_combinator(painter, camera, left, right, output, c.comparator.clone())
            }
            "arithmetic-combinator" => {
                let c = self
                    .inner
                    .control_behavior
                    .as_ref()
                    .unwrap()
                    .arithmetic_conditions
                    .as_ref()
                    .unwrap();
                let left = c.first_signal.as_ref().map_or(
                    c.first_constant
                        .map_or(OpSig::unknown(), |n| OpSig::constant(n)),
                    |s| OpSig::signal(&s.name),
                );
                let right = c.second_signal.as_ref().map_or(
                    c.second_constant
                        .map_or(OpSig::unknown(), |n| OpSig::constant(n)),
                    |s| OpSig::signal(&s.name),
                );
                let output = c
                    .output_signal
                    .as_ref()
                    .map_or(OpSig::unknown(), |s| OpSig::signal(&s.name));
                self.draw_combinator(painter, camera, left, right, output, c.operation.clone())
            }
            //"constant-combinator" => {
            //    todo!()
            //}
            _ => {
                let canvas_pos = camera.world_to_viewport(self.pos());
                painter.circle_filled(canvas_pos, 0.4 * camera.scale, Color32::RED);
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

    fn scaled<X>(&self, x: X) -> f32
    where
        X: Mul<f32, Output = f32>,
    {
        x * self.scale
    }

    fn world_to_viewport(&self, world_pos: Pos2) -> Pos2 {
        let rel_pos = world_pos - self.pos;
        let canvas_pos = self.viewport.center() + rel_pos * self.scale;
        canvas_pos
    }

    fn _canvas_to_world(&self, canvas_pos: Pos2) -> Pos2 {
        let rel_pos = (canvas_pos - self.viewport.center()) / self.scale;
        let world_pos = self.pos + rel_pos;
        world_pos
    }

    fn canvas_to_rel(&self, canvas_pos: Vec2) -> Vec2 {
        canvas_pos / self.scale
    }
}

#[derive(Clone)]
enum AppScreen {
    Blueprint(Blueprint),
    Book(BlueprintBook),
}

struct App {
    camera: Camera,
    stack: Vec<Container>,
}

impl eframe::App for App {
    fn update(&mut self, ctx: &egui::Context, frame: &mut eframe::Frame) {
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
                Container::Blueprint(blueprint) => {
                    self.view_blueprint(ui, blueprint.entities.clone())
                }
                Container::BlueprintBook(book) => self.view_book(ui, book.clone()),
            });
    }
}

impl App {
    fn new(container: Container) -> Self {
        Self {
            camera: Camera::new(Pos2::ZERO),
            stack: vec![container],
        }
    }

    fn view_blueprint(&mut self, ui: &mut egui::Ui, entities: Vec<fbo::Entity>) {
        let entities: Vec<_> = entities.iter().cloned().map(GuiEntity::new).collect();

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
        for e in &entities {
            e.draw(&painter, &self.camera)
        }

        for e in &entities {
            e.inner.connections.iter().for_each(|c| match c {
                fbo::EntityConnections::NumberIdx(port_conns) => {
                    for (port_index, c) in port_conns.iter() {
                        self.draw_from_port(e, c, port_index, &entities, &painter);
                    }
                }
                fbo::EntityConnections::StringIdx(port_conns) => {
                    for (port_string, c) in port_conns.iter() {
                        let port_index = port_string.parse::<usize>().unwrap();
                        let port_index = OneBasedIndex::try_from(port_index).unwrap();
                        self.draw_from_port(e, c, &port_index, &entities, &painter);
                    }
                }
            })
        }
    }

    // Draw connections
    fn draw_from_port(
        &self,
        e: &GuiEntity,
        c: &ConnectionPoint,
        port_index: &OneBasedIndex,
        others: &Vec<GuiEntity>,
        painter: &Painter,
    ) {
        let this_port = e.port_from_index(usize::from(*port_index) as i32);
        let red_offset = Vec2::RIGHT * 0.02;
        if let Some(red) = c.red.as_ref() {
            for red_con in red {
                if let Some(other_port_index) = red_con.circuit_id {
                    let other_entity = others
                        .iter()
                        .find(|e| e.inner.entity_number == red_con.entity_id)
                        .expect("Entity not found");
                    let other_port = other_entity.port_from_index(other_port_index);
                    painter.line_segment(
                        [
                            self.camera.world_to_viewport(this_port + red_offset),
                            self.camera.world_to_viewport(other_port + red_offset),
                        ],
                        Stroke::new(
                            0.02 * self.camera.scale,
                            Rgba::from_rgba_unmultiplied(1.0, 0.0, 0.0, 0.5),
                        ),
                    );
                }
            }
        }

        let green_offset = Vec2::LEFT * 0.02;
        if let Some(green) = c.green.as_ref() {
            for green_con in green {
                if let Some(other_port_index) = green_con.circuit_id {
                    let other_entity = others
                        .iter()
                        .find(|e| e.inner.entity_number == green_con.entity_id)
                        .expect("Entity not found");
                    let other_port = other_entity.port_from_index(other_port_index);
                    painter.line_segment(
                        [
                            self.camera.world_to_viewport(this_port + green_offset),
                            self.camera.world_to_viewport(other_port + green_offset),
                        ],
                        Stroke::new(
                            0.02 * self.camera.scale,
                            Rgba::from_rgba_unmultiplied(0.0, 1.0, 0.0, 0.5),
                        ),
                    );
                }
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
            let entities: Vec<_> = blueprint.entities.clone().into_iter().map(GuiEntity::new).collect();

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
            self.camera = Camera::new(cam_pos);
        }

        self.stack.push(container);
    }

    fn view_book(&mut self, ui: &mut egui::Ui, book: BlueprintBook) {
        const ITEM_MIN_HEIGHT: f32 = 200.0;
        const ITEM_MAX_WIDTH: f32 = 800.0;

        ui.vertical(|ui| {
            ScrollArea::vertical().show(ui, |ui| {
                ui.vertical_centered(|ui| {
                    for item in &book.blueprints {
                        let container = &item.item;
                        let name = match container {
                            Container::BlueprintBook(b) => {
                                b.label.clone().unwrap_or("No Name".to_string())
                            }
                            Container::Blueprint(b) => b.label.clone(),
                        };

                        let (rect, response) = ui.allocate_at_least(
                            Vec2::new(ui.available_width().min(ITEM_MAX_WIDTH), ITEM_MIN_HEIGHT),
                            Sense::click(),
                        );

                        ui.add_space(ITEM_MIN_HEIGHT * 0.06);

                        let p = ui.painter();
                        p.rect_filled(rect, 10.0, Hsva::new(choose_hue(&name), 0.5, 0.1, 1.0));

                        p.text(
                            rect.left_top() + Vec2::splat(10.0),
                            Align2::LEFT_TOP,
                            &name,
                            FontId {
                                size: 24.0,
                                family: FontFamily::Monospace,
                            },
                            Color32::WHITE,
                        );

                        if response.clicked() {
                            self.open_container(container.clone());
                        }

                        if response.hovered() {
                            ui.ctx().set_cursor_icon(egui::CursorIcon::PointingHand);
                        }
                    }
                });
            });
        });
    }
}
