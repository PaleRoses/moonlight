use crate::util::id::Id;

pub mod basic;
pub mod egg;
pub mod persistent;
pub mod versioned;

/// The immutable operations of a [UnionFind].
pub trait UnionFindView {
    /// The type of the additional information bound to each element.
    type ElementData;

    /// #### Return
    /// The set of all elements in this [UnionFind].
    fn universe(&self) -> impl std::iter::Iterator<Item = Id>;

    /// #### Return
    /// The raw parent of the element `x`.
    /// #### Note
    /// The raw parent may not be the representative of the equivalence class of
    /// `x`. Also, the parent of a representative is the representative itself.
    fn get_parent(&self, x: Id) -> Id;

    /// #### Return
    /// The information bound to the element `x`.
    fn get_element(&self, x: Id) -> &Self::ElementData;

    /// #### Return
    /// An iterator over the ancestors of the element `x`, starting from `x`
    /// up to the representative of its equivalence class.
    fn get_ancestors(&self, x: Id) -> impl std::iter::Iterator<Item = Id> + '_ {
        std::iter::successors(Some(x), |&current| {
            let parent = self.get_parent(current);
            if parent == current {
                None
            } else {
                Some(parent)
            }
        })
    }

    /// #### Return
    /// The representative of the equivalence class of `x`.
    fn find(&self, x: Id) -> Id {
        let mut current = x;
        let mut parent = self.get_parent(current);
        while current != parent {
            current = parent;
            parent = self.get_parent(current);
        }
        current
    }

    /// #### Return
    /// True if the elements `x` and `y` are equivalent (i.e. in the same
    /// equivalence class. False otherwise.
    fn equal(&self, x: Id, y: Id) -> bool {
        self.find(x) == self.find(y)
    }

    /// #### Return
    /// A new [UnionFind] that encodes the same equivalence relation as this
    /// [UnionFind].
    fn copy_into<U2>(&self) -> U2
    where
        Self::ElementData: Clone,
        U2: UnionFind<ElementData = Self::ElementData> + Default,
    {
        let mut uf: U2 = U2::default();
        for x in self.universe() {
            uf.new_element(self.get_element(x).clone());
        }
        for x in self.universe() {
            let canonical_id = self.get_parent(x);
            uf.union(x, canonical_id);
        }
        for x in self.universe() {
            uf.find_mut(x);
        }
        uf
    }
}
/// A union-find is a data structure that represent an equivalence relation
/// over a generic set of elements. It supports two operations:
/// - `find(x)`: returns the representative of the equivalence class of `x`.
/// - `union(x, y)`: merges the equivalence classes of `x` and `y`.
///
/// Elements are uniquely identified by an `Id`. Additionally, each element is
/// bound to some additional information of type `UnionFind::ElementData`.
pub trait UnionFind: UnionFindView {
    /// #### Description
    /// Create a new element bound to the information `element`.
    /// #### Return
    /// The `Id` of the new element.
    fn new_element(&mut self, element: Self::ElementData) -> Id;

    /// #### Return
    /// The representative of the equivalence class of `x`.
    /// #### Note
    /// This method performs `path compression`, reducing the cost of future
    /// calls.
    fn find_mut(&mut self, x: Id) -> Id;

    /// #### Description
    /// Merge the equivalence classes of `xc` and `yc` into a new equivalence
    /// class.
    /// #### Return
    /// The representative of the new equivalence class.
    /// #### Note
    /// The elements `xc` and `yc` must be representatives of their respective
    /// equivalence classes, otherwise the behavior is undefined. You can use
    /// [union] to merge elements without this restriction, at a slight cost in
    /// performances.
    fn union_canonical(&mut self, xc: Id, yc: Id) -> Id;

    /// Same as [union_canonical], but the elements `x` and `y` are not
    /// required to be representatives of their equivalence classes.
    fn union(&mut self, x: Id, y: Id) -> Id {
        let xc = self.find_mut(x);
        let yc = self.find_mut(y);
        if xc == yc {
            return xc;
        }
        self.union_canonical(xc, yc)
    }

    /// Same as [new_element], but creates multiple elements in sequence.
    fn new_elements(&mut self, elements: Vec<Self::ElementData>) -> Vec<Id> {
        let mut vec = Vec::new();
        for e in elements {
            let new_id = self.new_element(e);
            vec.push(new_id);
        }
        vec
    }

    /// Shorthand for [new_element], using the default [Self::ElementData].
    fn new_default(&mut self) -> Id
    where
        Self::ElementData: Default,
    {
        self.new_element(Default::default())
    }

    /// Shorthand for [new_elements], using the default [Self::ElementData].
    fn new_defaults(&mut self, n: usize) -> Vec<Id>
    where
        Self::ElementData: Default,
    {
        let mut vec = Vec::new();
        for _ in 0..n {
            let new_id = self.new_default();
            vec.push(new_id);
        }
        vec
    }
}

#[rustfmt::skip]
pub mod testing {
    use crate::util::graphviz::GraphViz;

    use super::*;

    pub fn empty<U>()
    where U: UnionFind + Default, U::ElementData: Default,
    {
        //! An empty union-find should have an empty universe.
        let uf = U::default();
        assert!(uf.universe().count() == 0)
    }
    pub fn find_canonical<U>()
    where U: UnionFind + Default, U::ElementData: Default,
    {
        //! Finding a canonical element yields the element itself.
        let mut uf = U::default();
        let x = uf.new_default();
        let xc = uf.find(x);
        assert_eq!(x, xc);
    }
    pub fn union_and_find<U>()
    where U: UnionFind + Default, U::ElementData: Default,
    {
        //! After unioning some elements, the elements should belong to the same
        //! equivalence class.
        let mut uf = U::default();
        let x = uf.new_default();
        let y = uf.new_default();
        let z = uf.new_default();

        let xc = uf.find(x);
        let yc = uf.find(y);
        let zc = uf.find(z);
        assert!(xc != yc && xc != zc && yc != zc);

        let _xyc = uf.union(x, y);
        let xyzc = uf.union(y, z);

        let xc = uf.find(x);
        let yc = uf.find(y);
        let zc = uf.find(y);
        assert!(xc == xyzc && yc == xyzc && zc == xyzc);
    }
    pub fn equal<U>()
    where U: UnionFind + Default, U::ElementData: Default,
    {
        //! Querying the equality of two elements should be true if they belong
        //! to the same equivalence class.
        let mut uf = U::default();
        let x = uf.new_default();
        let y = uf.new_default();

        assert!(!uf.equal(x, y));
        uf.union(x, y);
        assert!(uf.equal(x, y));
    }
    pub fn path_compression<U>()
    where U: UnionFind + Default + GraphViz, U::ElementData: Default + std::fmt::Debug,
    {
        //! After finding the canonical element of an element, its parent should
        //! be updated to the canonical element, depending on the path 
        //! compression strategy.
        const N: usize = 10;
        let mut uf = U::default();
        let ids = uf.new_defaults(N);
        assert!(uf.universe().count() == N);

        // Union-Find as lists of children: *[9, 5[8[7], 6, 4[3[2[1[0]]]]]]
        uf.union(ids[4], ids[5]);
        uf.union(ids[3], ids[4]);
        uf.union(ids[2], ids[3]);
        uf.union(ids[1], ids[2]);
        uf.union(ids[0], ids[1]);
        uf.union(ids[0], ids[6]);
        uf.union(ids[7], ids[8]);
        uf.union(ids[0], ids[7]);

        // Before Path Compression
        #[cfg(feature = "pc-none")]
        let previous_parents: Vec<Id> = ids.iter().map(|id| uf.get_parent(*id)).collect();
        #[cfg(feature = "pc-full")]
        let canonicals: Vec<Id> = ids.iter().map(|id| uf.find(*id)).collect();

        // Path Compression (on all leaves)
        let leaves = [5, 6, 8, 9];
        for &leaf in leaves.iter() {
            uf.find_mut(ids[leaf]);
        }

        // After Path Compression
        let current_parents: Vec<Id> = ids.iter().map(|id| uf.get_parent(*id)).collect();
        #[cfg(feature = "pc-none")]
        assert_eq!(current_parents, previous_parents);
        #[cfg(feature = "pc-split")]
        // Node:                             0       1       2       3       4       5       6       7       8       9
        assert_eq!(current_parents, vec![ids[0], ids[0], ids[0], ids[1], ids[2], ids[3], ids[0], ids[0], ids[0], ids[9]]);
        #[cfg(feature = "pc-half")]
        // Node:                             0       1       2       3       4       5       6       7       8       9
        assert_eq!(current_parents, vec![ids[0], ids[0], ids[1], ids[1], ids[3], ids[3], ids[0], ids[0], ids[0], ids[9]]);
        #[cfg(feature = "pc-full")]
        assert_eq!(current_parents, canonicals);
    }

    /// #### Description
    /// Generate a test module for the specified [UnionFind] implementation.
    /// The module will be named after `$module` and will test the
    /// implementation of the concrete type `$implementation`.
    #[macro_export]
    macro_rules! test_unionfind {
        ($module: ident, $implementation: ty) => {
            #[cfg(test)]
            mod $module {
                use super::*;
                use $crate::structures::unionfind::testing;
                type Impl = $implementation;

                #[test]
                fn empty() { testing::empty::<Impl>(); }
                #[test]
                fn find_canonical() { testing::find_canonical::<Impl>(); }
                #[test]
                fn union_and_find() { testing::union_and_find::<Impl>(); }
                #[test]
                fn equal() { testing::equal::<Impl>(); }
                #[test]
                fn path_compression() { testing::path_compression::<Impl>(); }
            }
        };
    }
    /// #### Return
    /// A [Vec] containing the specified elements. Duplicate elements are
    /// removed.
    #[macro_export]
    macro_rules! set {
        ($($x: expr);*) => ({
            let mut set = std::collections::HashSet::new();
            $(set.insert($x);)*
            set
        });
    }
    /// #### Description
    /// Assert that the specified union-find encodes the specified equivalence
    /// classes. The only requirement for the union-find is to implement a
    /// method with signature `equal(x: usize, y: usize) -> bool`.
    #[macro_export]
    macro_rules! assert_classes {
        ($uf: expr, $($xc: expr),+) => ({
            let classes: Vec<std::collections::HashSet<$crate::util::id::Id>> = vec![$($xc,)+];
            for i in 0..classes.len() {
                for j in i..classes.len() {
                    for &ni in classes[i].iter() {
                        for &nj in classes[j].iter() {
                            assert_eq!(
                                $uf.equal(ni, nj), i == j,
                                "expect {} = {} to be {}", ni, nj, i == j
                            );
                        }
                    }
                }
            }
        });
    }
    pub use assert_classes;
    pub use set;
    pub use test_unionfind;
}

pub mod graphics {
    use crate::util::id::AsIndex;

    /// #### Description
    /// Generate a graphviz representation of the unionfind `uf`. The graph will
    /// be labelled with the string `label`.
    /// #### Return
    /// A string containing the graphviz representation of the unionfind.
    /// #### Note
    /// This function will generate a cluster subgraph with a pseudo-unique
    /// identifier. This is so it is possible to display multiple unionfinds in
    /// sequence by simply wrapping their representations in a single digraph.
    ///
    /// The implementation is best-effort using the public api of the unionfind.
    /// In particular, it guarantees to represent the same information encoded
    /// in the unionfind, but not necessarily the actual structure used in the
    /// implementation. If the structure is important, it is recommended to use
    /// the implementation-specific
    /// [crate::graphics::graphviz::IntoGraphViz::into_graphviz] method.
    pub fn graphviz<U>(uf: &U, label: &str) -> String
    where
        U: super::UnionFind,
        U::ElementData: std::fmt::Debug,
    {
        let mut out = String::new();
        let mut names: Vec<String> = Vec::new();

        let graph_id: String = crate::util::graphviz::new_graph_id();
        out += &format!("subgraph cluster_{} {{", graph_id);
        out += &format!("\n\tlabel = \"{}\";", label);
        for node_id in uf.universe() {
            let node_name = format!("{}_{}", graph_id, node_id);
            let node_label = format!("{}: {:?}", node_id, uf.get_element(node_id));
            out += &format!("\n\t{} [label=\"{}\"];", node_name, node_label);
            names.push(node_name);
        }
        for child_id in uf.universe() {
            let child_label = &names[child_id.to_i()];
            let parent_label = &names[uf.get_parent(child_id).to_i()];
            out += &format!("\n\t\"{}\" -> \"{}\";", child_label, parent_label);
        }
        out += "\n}";
        out
    }
}
