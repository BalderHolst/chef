use factorio_blueprint::objects as fbo;

use eframe::{
    egui::{
        self, Align2, Color32, FontFamily, FontId, Key, Painter, Pos2, Rect, Rgba, Sense, Stroke,
        Vec2,
    },
    epaint::Hsva,
};

pub fn run_gui(entities: Vec<fbo::Entity>) {
    let options = eframe::NativeOptions {
        viewport: egui::ViewportBuilder::default().with_inner_size([320.0, 240.0]),
        ..Default::default()
    };

    if let Err(e) = eframe::run_native(
        "Chef Inspector",
        options,
        Box::new(|_cc| {
            let entities: Vec<_> = entities.into_iter().map(GuiEntity::new).collect();

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

            let camera = Camera::new(cam_pos);
            Ok(Box::new(App { entities, camera }))
        }),
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

struct GuiEntity {
    inner: fbo::Entity,
}

impl GuiEntity {
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

    fn draw(&self, painter: &Painter, camera: &Camera) {
        const MARGIN: f32 = 0.11;

        let canvas_pos = camera.world_to_viewport(self.pos());

        macro_rules! text {
            ($text:expr; $pos:expr; $size:expr) => {
                painter.text(
                    $pos,
                    Align2::CENTER_CENTER,
                    $text,
                    FontId {
                        size: camera.scale * $size,
                        family: FontFamily::Monospace,
                    },
                    Color32::BLACK,
                );
            };
        }

        // All sizes are in taller than wide if they are not square
        match self.name() {
            "decider-combinator" | "arithmetic-combinator" => {
                let control = self.inner.control_behavior.as_ref().unwrap();

                let (left, right, op) = match (
                    control.decider_conditions.as_ref(),
                    control.arithmetic_conditions.as_ref(),
                ) {
                    (None, Some(c)) => {
                        let left = c.first_signal.as_ref().map_or(
                            c.first_constant
                                .map_or("UNKNOWN".to_string(), |n| n.to_string()),
                            |s| s.name.clone(),
                        );
                        let right = c.second_signal.as_ref().map_or(
                            c.second_constant
                                .map_or("UNKNOWN".to_string(), |n| n.to_string()),
                            |s| s.name.clone(),
                        );
                        (left, right, c.operation.clone())
                    }
                    (Some(c), None) => {
                        let left = c
                            .first_signal
                            .as_ref()
                            .map_or("UNKNOWN".to_string(), |s| s.name.clone());
                        let right = c
                            .second_signal
                            .as_ref()
                            .map_or("UNKNOWN".to_string(), |s| s.name.clone());
                        (left, right, c.comparator.clone())
                    }
                    (None, None) => todo!(),
                    (Some(_), Some(_)) => todo!(),
                };

                let size = Vec2::new(1.0, 2.0);
                let rect = Rect::from_center_size(
                    canvas_pos,
                    (size - Vec2::new(MARGIN, MARGIN)) * camera.scale,
                );
                painter.rect_filled(
                    rect,
                    0.05 * camera.scale,
                    Hsva::new(self.hue(), 0.7, 0.8, 1.0),
                );

                // TODO: Depend on actual direction
                let arrow_len = 0.60;
                let center = rect.center();
                let vec = Vec2::new(0.0, -arrow_len) * camera.scale;
                painter.arrow(
                    center - vec / 2.0,
                    vec,
                    Stroke::new(0.02 * camera.scale, Rgba::from_rgb(0.2, 0.2, 0.3)),
                );

                text!(
                    self.name();
                    rect.center_top() + Vec2::new(0.0, 0.10)*camera.scale;
                    0.065
                );

                let offset = Vec2::new(0.25, 0.0) * camera.scale;
                text!(op; canvas_pos; 0.20);
                text!(left; canvas_pos-offset; 0.05);
                text!(right; canvas_pos+offset; 0.05);
            }
            "constant-combinator" => {
                todo!()
            }
            _ => {
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

struct App {
    camera: Camera,
    entities: Vec<GuiEntity>,
}

impl eframe::App for App {
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        egui::CentralPanel::default().show(ctx, |ui| {
            ui.heading("Chef Inspector");
            self.grid(ui)
        });
    }
}

impl App {
    fn grid(&mut self, ui: &mut egui::Ui) {
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

        for e in &self.entities {
            e.draw(&painter, &self.camera)
        }
    }
}
