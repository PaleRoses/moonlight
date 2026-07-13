use crate::util::id::IdFactory;

/// An object that can be represented in GraphViz format.
pub trait GraphViz {
    /// #### Return
    /// A string representation of this object in GraphViz format.
    fn graphviz(&self, label: &str) -> String;
}

/// #### Return
/// A unique identifier for a graph in GraphViz format.
pub fn new_graph_id() -> String {
    format!("graph_{}", crate::util::id::TimeIds::create_id())
}
