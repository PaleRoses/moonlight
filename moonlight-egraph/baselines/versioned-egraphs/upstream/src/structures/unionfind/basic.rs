use crate::structures::unionfind::UnionFindView;
use crate::util::graphviz::GraphViz;
use crate::util::id::{AsIndex, Id, IdRange};

#[derive(Debug, Clone, Default)]
pub struct UnionFind<A> {
    /// A map from [Id] to [UnionFindNode].
    universe: Vec<UnionFindNode<A>>,
}

#[derive(Debug, Clone)]
struct UnionFindNode<A> {
    /// The parent of this node in the [UnionFind].
    parent: Id,
    /// The element represented by this node.
    element: A,
}
impl<A> UnionFind<A> {
    /// #### Description
    /// Adds an edge from `child` to `parent`:
    ///
    /// `parent <--- child`
    ///
    /// #### Note
    /// This is a low-level operation and should be used with caution: the
    /// caller is responsible for ensuring the correctness of the union-find
    /// structure. Use at your own discretion.
    #[inline]
    pub fn add_edge(&mut self, parent: Id, child: Id) {
        self.universe[child.to_i()].parent = parent;
    }
}
impl<A> super::UnionFindView for UnionFind<A> {
    type ElementData = A;
    #[inline]
    fn universe(&self) -> impl std::iter::Iterator<Item = Id> {
        IdRange::new(0, self.universe.len())
    }
    #[inline]
    fn get_parent(&self, x: Id) -> Id {
        self.universe[x.to_i()].parent
    }
    #[inline]
    fn get_element(&self, x: Id) -> &A {
        &self.universe[x.to_i()].element
    }
}
impl<A> super::UnionFind for UnionFind<A> {
    #[cfg_attr(feature = "trace-unionfind-basic", tracing::instrument(skip(self, element), ret))]
    fn new_element(&mut self, element: A) -> Id {
        let id = Id::from(self.universe.len());
        let node = UnionFindNode { parent: id, element };
        self.universe.push(node);
        id
    }
    #[cfg_attr(feature = "trace-unionfind-basic", tracing::instrument(skip(self), ret))]
    fn find_mut(&mut self, x: Id) -> Id {
        let mut current = x;
        let mut parent = self.get_parent(current);
        #[cfg(feature = "pc-full")]
        // For every traversed node, make it point to its canonical.
        // Amortized logarithmic (or Inverse Ackermann with union by rank).
        {
            let mut path = vec![];
            while current != parent {
                let child = current;
                current = parent;
                parent = self.get_parent(current);
                if current != parent {
                    path.push(child);
                }
            }
            for traversed in path {
                self.add_edge(current, traversed); // compression
            }
        }
        #[cfg(feature = "pc-none")]
        // No path compression.
        while current != parent {
            current = parent;
            parent = self.get_parent(current);
        }
        #[cfg(feature = "pc-split")]
        // For every traversed node, make its child point to its parent.
        // Preserve complexity with less memory consumption.
        while current != parent {
            let child = current;
            current = parent;
            parent = self.get_parent(current);
            if current != parent {
                self.add_edge(parent, child); // compression
            }
        }
        #[cfg(feature = "pc-half")] // egg's original; our default
        // For every second traversed node, make its child point to its parent.
        // Preserve complexity with less memory consumption and accesses.
        {
            let mut compress: bool = true;
            while current != parent {
                let child = current;
                current = parent;
                parent = self.get_parent(current);
                if compress && current != parent {
                    self.add_edge(parent, child); // compression
                }
                compress = !compress;
            }
        }
        current
    }
    #[cfg_attr(feature = "trace-unionfind-basic", tracing::instrument(skip(self), ret))]
    fn union_canonical(&mut self, xc: Id, yc: Id) -> Id {
        let (parent, child) = (xc, yc);
        self.add_edge(parent, child);
        parent
    }
}

super::testing::test_unionfind!(unionfind_tests, UnionFind<()>);

impl<A: std::fmt::Debug> GraphViz for UnionFind<A> {
    #[cfg_attr(feature = "trace-unionfind-basic", tracing::instrument(skip(self), ret))]
    fn graphviz(&self, label: &str) -> String {
        super::graphics::graphviz(self, label)
    }
}
