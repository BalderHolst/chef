use super::Graph;

use factorio_blueprint::objects as fbo;

mod recursive;
mod turdmaster2000;
mod utils;

type TilePos = (i64, i64);

pub trait Placer {
    fn place(graph: Graph) -> Vec<fbo::Entity>;
}

#[derive(Debug, Clone, clap::ValueEnum, Default)]
pub enum PlacerName {
    Recursive,

    #[default]
    TurdMaster2000,
}

impl PlacerName {
    pub fn place(&self, graph: Graph) -> Vec<fbo::Entity> {
        match self {
            PlacerName::Recursive => recursive::RecursivePlacer::place(graph),
            PlacerName::TurdMaster2000 => turdmaster2000::TurdMaster2000::place(graph),
        }
    }
}
