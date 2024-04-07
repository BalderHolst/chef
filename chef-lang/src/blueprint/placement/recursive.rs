pub struct RecursivePlacer;
impl RecursivePlacer {}

impl super::Placer for RecursivePlacer {
    fn place(_graph: crate::blueprint::Graph) -> Vec<factorio_blueprint::objects::Entity> {
        todo!()
    }
}
