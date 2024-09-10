use factorio_blueprint::objects as fbo;

use eframe::{
    egui::{self, Pos2, Rect, Response, Rgba, Sense, Shape, Ui, Vec2},
    epaint::Hsva,
};

pub fn run_gui(entities: &Vec<fbo::Entity>) {
    let options = eframe::NativeOptions {
        viewport: egui::ViewportBuilder::default().with_inner_size([320.0, 240.0]),
        ..Default::default()
    };

    if let Err(e) = eframe::run_native(
        "My egui App",
        options,
        Box::new(|_cc| Ok(Box::<App>::default())),
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

struct App {
    camera: Vec2,
}

impl Default for App {
    fn default() -> Self {
        Self {
            camera: Vec2::new(0.0, 0.0),
        }
    }
}

impl eframe::App for App {
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        egui::CentralPanel::default().show(ctx, |ui| {
            ui.heading("My egui App");
            self.grid(ui)
        });
    }
}

impl App {
    fn grid(&mut self, ui: &mut egui::Ui) {
        let size = ui.available_size_before_wrap();

        const TILE_SIZE: f32 = 32.0;
        const TILE_MARGIN: f32 = TILE_SIZE / 10.0;
        const GRID_SIZE: usize = 30;

        let (resp, painter) = ui.allocate_painter(size, Sense::click_and_drag());

        if resp.dragged() {
            self.camera += resp.drag_delta();
        }

        if resp.clicked() {
            self.camera = Vec2::new(0.0, 0.0);
        }

        let canvas = painter.clip_rect();

        painter.rect_filled(painter.clip_rect(), 0.0, Rgba::from_white_alpha(0.01));

        for x in 0..GRID_SIZE {
            for y in 0..GRID_SIZE {
                let pos = Pos2::new(x as f32, y as f32) * (TILE_SIZE + TILE_MARGIN)
                    + Vec2::new(TILE_MARGIN / 2.0, TILE_MARGIN / 2.0)
                    + canvas.left_top().to_vec2()
                    + self.camera;
                let rect = Rect::from_two_pos(pos, pos + Vec2::new(TILE_SIZE, TILE_SIZE));
                let h = ((x * y) as f32) / ((GRID_SIZE * GRID_SIZE) as f32);
                let v = (y as f32) / (GRID_SIZE as f32);
                painter.rect_filled(rect, 0.0, Hsva::new(h, 1.0, v, 1.0));
            }
        }
    }
}
