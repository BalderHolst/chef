use factorio_blueprint::objects as fbo;

use eframe::egui::{self, Align2, Color32, FontFamily, FontId, Key, Pos2, Rect, Rgba, Sense, Vec2};

pub fn run_gui(entities: Vec<fbo::Entity>) {
    let options = eframe::NativeOptions {
        viewport: egui::ViewportBuilder::default().with_inner_size([320.0, 240.0]),
        ..Default::default()
    };

    if let Err(e) = eframe::run_native(
        "Chef Inspector",
        options,
        Box::new(|_cc| {
            Ok(Box::new(App {
                entities,
                camera: Default::default(),
                canvas: Rect::ZERO,
            }))
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

#[derive(Debug)]
struct Camera {
    pos: Pos2,
    scale: f32,
}

impl Default for Camera {
    fn default() -> Self {
        Self {
            pos: Pos2::ZERO,
            scale: 100.0,
        }
    }
}

struct App {
    camera: Camera,
    canvas: Rect,
    entities: Vec<fbo::Entity>,
}

impl eframe::App for App {
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        egui::CentralPanel::default().show(ctx, |ui| {
            ui.heading("Chef Inspector");
            ui.label(format!("Camera: {:?}", self.camera));
            self.grid(ui)
        });
    }
}

fn prototype_size(name: &fbo::Prototype) -> Option<Vec2> {
    // All sizes are in taller than wide if they are not square
    match name.as_str() {
        "decider-combinator" => Some(Vec2::new(1.0, 2.0)),
        "arithmetic-combinator" => Some(Vec2::new(1.0, 2.0)),
        "constant-combinator" => Some(Vec2::new(1.0, 1.0)),
        _ => None,
    }
}

fn factorio_pos_to_egui(pos: &fbo::Position) -> Pos2 {
    let x: f64 = pos.x.into();
    let y: f64 = pos.y.into();
    Pos2::new(x as f32, y as f32)
}

impl App {
    fn world_to_canvas(&self, world_pos: Pos2) -> Pos2 {
        let rel_pos = world_pos - self.camera.pos;
        let canvas_pos = self.canvas.center() + rel_pos * self.camera.scale;
        canvas_pos
    }

    fn canvas_to_world(&self, canvas_pos: Pos2) -> Pos2 {
        let rel_pos = (canvas_pos - self.canvas.center()) / self.camera.scale;
        let world_pos = self.camera.pos + rel_pos;
        world_pos
    }

    fn canvas_to_rel(&self, canvas_pos: Vec2) -> Vec2 {
        canvas_pos / self.camera.scale
    }

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
            let delta = self.canvas_to_rel(resp.drag_delta());
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

            if velocity != Vec2::ZERO {
                self.camera.pos += velocity * 0.001 * self.camera.scale;
            }
        });

        // Select font
        let font = FontId {
            size: self.camera.scale * 0.08,
            family: FontFamily::default(),
        };

        // Draw canvas
        self.canvas = painter.clip_rect();
        painter.rect_filled(painter.clip_rect(), 0.0, Rgba::from_white_alpha(0.01));

        for e in &self.entities {
            let world_pos = factorio_pos_to_egui(&e.position);
            let canvas_pos = self.world_to_canvas(world_pos);
            let entity_size = prototype_size(&e.name);

            if let Some(s) = entity_size {
                let rect = Rect::from_center_size(canvas_pos, s * 0.9 * self.camera.scale);
                painter.rect_filled(rect, 0.05 * self.camera.scale, Rgba::GREEN);
            } else {
                painter.circle_filled(canvas_pos, 1.0, Color32::RED);
            }

            painter.text(
                canvas_pos,
                Align2::CENTER_CENTER,
                &e.name,
                font.clone(),
                Color32::BLACK,
            );
        }
    }
}
