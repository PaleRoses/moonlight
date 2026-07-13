// ### DISCLAIMER ##############################################################
// This file is an adaptation of the private unionfind from the egg's:
// https://github.com/egraphs-good/egg/blob/main/src/unionfind.rs
//
// The implementation has been adapted to conform to our public interface of a
// unionfind, making it easier to swap between implementations.
// #############################################################################

use crate::util::{
    graphviz::GraphViz,
    id::{AsIndex, Id, IdRange},
};

#[derive(Debug, Clone, Default)]
pub struct UnionFind {
    parents: Vec<Id>,
}

impl UnionFind {
    #[cfg_attr(feature = "trace-unionfind-egg", tracing::instrument(skip(self), ret))]
    pub fn make_set(&mut self) -> Id {
        let id = Id::from(self.parents.len());
        self.parents.push(id);
        id
    }
    pub fn size(&self) -> usize {
        self.parents.len()
    }
    fn parent(&self, query: Id) -> Id {
        self.parents[query.to_i()]
    }
    fn parent_mut(&mut self, query: Id) -> &mut Id {
        &mut self.parents[query.to_i()]
    }
    pub fn find(&self, mut current: Id) -> Id {
        while current != self.parent(current) {
            current = self.parent(current)
        }
        current
    }
    #[cfg_attr(feature = "trace-unionfind-egg", tracing::instrument(skip(self), ret))]
    pub fn find_mut(&mut self, mut current: Id) -> Id {
        let mut parent = self.parent(current);
        #[cfg(feature = "pc-full")]
        // For every traversed node, make it point to its canonical.
        // Amortized logarithmic (or Inverse Ackermann with union by rank).
        {
            let mut path = vec![];
            while current != parent {
                path.push(current);
                current = parent;
                parent = self.parent(current);
            }
            for traversed in path {
                *self.parent_mut(traversed) = current;
            }
        }
        #[cfg(feature = "pc-none")]
        // No path compression.
        while current != parent {
            current = parent;
            parent = self.parent(current);
        }
        #[cfg(feature = "pc-split")]
        // For every traversed node, make its child point to its parent.
        // Preserve complexity with less memory consumption.
        {
            while current != parent {
                let child = current;
                current = parent;
                parent = self.parent(current);
                *self.parent_mut(child) = parent;
            }
        }
        #[cfg(feature = "pc-half")] // original
        // For every second traversed node, make its child point to its parent.
        // Preserve complexity with less memory consumption and accesses.
        while current != parent {
            let grandparent = self.parent(parent);
            *self.parent_mut(current) = grandparent;
            current = grandparent;
            parent = self.parent(current);
        }
        current
    }

    #[cfg_attr(feature = "trace-unionfind-egg", tracing::instrument(skip(self), ret))]
    pub fn union(&mut self, root1: Id, root2: Id) -> Id {
        *self.parent_mut(root2) = root1;
        root1
    }
}

impl super::UnionFindView for UnionFind {
    type ElementData = ();
    fn universe(&self) -> impl std::iter::Iterator<Item = Id> {
        IdRange::new(0, self.parents.len())
    }
    fn get_parent(&self, x: Id) -> Id {
        self.parents[x.to_i()]
    }
    fn get_element(&self, _id: Id) -> &() {
        &()
    }
}
impl super::UnionFind for UnionFind {
    fn new_element(&mut self, _element: ()) -> Id {
        self.make_set()
    }
    fn find_mut(&mut self, x: Id) -> Id {
        self.find_mut(x)
    }
    fn union_canonical(&mut self, xc: Id, yc: Id) -> Id {
        self.union(xc, yc)
    }
}

super::testing::test_unionfind!(unionfind_tests, UnionFind);

impl GraphViz for UnionFind {
    #[cfg_attr(feature = "trace-unionfind-egg", tracing::instrument(skip(self), ret))]
    fn graphviz(&self, label: &str) -> String {
        super::graphics::graphviz(self, label)
    }
}
