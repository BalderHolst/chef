use fnv::FnvHashMap;
use std::hash::Hash;

struct Graph<VId, E = (), V = ()> {
    vertices: FnvHashMap<VId, V>,
    adjacency: FnvHashMap<VId, Vec<(VId, E)>>,
}

impl<VId, E, V> Graph<VId, E, V> 
where 
    VId: Eq + Hash, 
    V: Hash 
{
    pub fn new() -> Graph<VId, E, V> {
        Graph { vertices: FnvHashMap::default(), adjacency: FnvHashMap::default() }
    }

    pub fn push_vertex(&mut self, vid: VId, vertex: V) {
        self.vertices.insert(vid, vertex);
    }

    pub fn push_edge(&mut self, from: VId, to: VId, edge: E) {
        let adjacent_to_from = self.adjacency.entry(from).or_default();
        adjacent_to_from.push((to, edge))
    }
}

impl<VId, E, V> Graph<VId, E, V>
where
    VId: Eq + Hash + Clone,
    V: Hash,
    E: Clone,
{
    pub fn push_undirected_edge(
        &mut self,
        from: VId,
        to: VId,
        edge: E,
        ) {
        self.push_edge(from.clone(), to.clone(), edge.clone());
        self.push_edge(from, to, edge);
    }
}

impl<VId, E> Graph<VId, E, ()>
where
    VId: Eq + Hash,
{
    pub fn push_vid(&mut self, vid: VId) {
        self.vertices.insert(vid, ());
    }
}
