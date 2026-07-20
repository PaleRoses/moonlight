pub mod basic;

use super::{UnionFind, UnionFindView};
use crate::structures::versiontree::{Version, VersionTree};
use crate::util::id::Id;

/// Flags that customize the behavior of a [VersionedUnionFind].
#[derive(Debug, Clone)]
pub struct Flags {
    /// Guarantees that mutable operations are only applied to leaf versions if
    /// and only if this flag is set to true.
    pub lock_superversions: bool,
    /// Guarantees that any operation applied to a superversion is also
    /// propagated to all subversions.
    pub propagate: bool,
}
impl crate::util::memory::HasMemory for Flags {
    fn fields(&self) -> Vec<(&str, &dyn crate::util::memory::HasMemory)> {
        vec![("lock_superversions", &self.lock_superversions), ("propagate", &self.propagate)]
    }
}
impl Default for Flags {
    fn default() -> Self {
        Flags { lock_superversions: true, propagate: false }
    }
}

/// A [UnionFind] that encodes multiple equivalence relations at the same time.
/// Each equivalence relation is identified by a unique version. Versions may
/// inherit from other versions, meaning that their equivalence relations share
/// some equivalences.
pub trait VersionedUnionFind {
    /// The type of the additional information bound to each element.
    type ElementData;
    /// The type of immutable projections you can obtain from this [VersionedUnionFind].
    type Projection<'a>: UnionFindView<ElementData = Self::ElementData>
    where
        Self: 'a;
    /// The type of mutable projections you can obtain from this [VersionedUnionFind].
    type ProjectionMut<'a>: UnionFind<ElementData = Self::ElementData>
    where
        Self: 'a;

    /// #### Return
    /// An immutable reference to the [Flags] configured for this
    /// [VersionedUnionFind].
    fn flags(&self) -> &Flags;

    /// #### Return
    /// A mutable reference to the [Flags] configured for this
    /// [VersionedUnionFind].
    fn flags_mut(&mut self) -> &mut Flags;

    /// #### Return
    /// The version tree maintaining the versions of this [VersionedUnionFind].
    fn versioning(&self) -> &VersionTree;

    /// #### Description
    /// Create a new subversion that inherits from version `v`.
    /// #### Return
    /// The new subversion.
    /// #### Panics
    /// - "Cannot branchout from removed version" : if version `v` was removed.
    fn branchout(&mut self, v: Version) -> Version;

    /// #### Description
    /// Create a twin version of version `v`.
    /// #### Return
    /// The new twin version.
    /// #### Panics
    /// - "Cannot create a twin of a removed version" : if version `v` was
    ///   removed.
    /// - "Cannot create a twin of the root version" : if version `v` is
    ///   [VersionTree::ROOT_VERSION].
    /// #### Note
    /// Can provide a performance improvement over [branchout] for leaf-only
    /// workloads, where removing superversions is allowed and beneficial.
    fn twin(&mut self, v: Version) -> Version;

    /// #### Description
    /// Generate a new immutable [Projection] of this [VersionedUnionFind] at
    /// version `v`. Immutable [Projection]s are used to perform read operations
    /// on specific versions of a [VersionedUnionFind].
    /// #### Return
    /// The new immutable [Projection].
    /// #### Panics
    /// - "Cannot project removed version": if version `v` was removed.
    fn project(&self, v: Version) -> Self::Projection<'_>;

    /// #### Description
    /// Generate a new mutable [Projection] of this [VersionedUnionFind] at
    /// version `v`. Mutable [Projection]s are used to perform any operations
    /// on specific versions of a [VersionedUnionFind].
    /// #### Return
    /// The new mutable [Projection].
    /// #### Panics
    /// - "Cannot project removed version": if version `v` was removed.
    fn project_mut(&mut self, v: Version) -> Self::ProjectionMut<'_>;

    /// #### Description
    /// Remove all data bound to version `v` and its subversions.
    /// #### Panics
    /// - "Cannot remove the root version": if `v` is [VersionTree::ROOT_VERSION].
    /// - "Cannot remove already removed version": if `v` was already removed.
    /// #### Warnings
    /// See [VersionTree::remove_version].
    fn remove_version(&mut self, v: Version);

    /// #### Description
    /// Rebase version `v` to the [VersionTree::ROOT_VERSION]. This means that
    /// `v` will become independent from its current superversion.
    ///
    /// #### Panics
    /// - "Cannot rebase the root version": if `v` is [VersionTree::ROOT_VERSION].
    /// - "Cannot rebase removed version": if `v` was removed.
    fn rebase(&mut self, v: Version);

    /// Alias for [project]
    #[inline]
    fn focus(&self, v: Version) -> Self::Projection<'_> {
        self.project(v)
    }
    /// Alias for [project_mut].
    #[inline]
    fn take(&mut self, v: Version) -> Self::ProjectionMut<'_> {
        self.project_mut(v)
    }

    /// As [UnionFindView::get_parent], but also returns the version of the edge
    /// followed to reach the closest valid parent.
    fn get_parent_and_version(&self, x: Id, v: Version) -> (Id, Version);

    /// #### Description
    /// Iterate over all subversions of the specified `superversion` and
    /// apply the `callback` to each of them.
    #[inline(always)]
    fn for_each_subversion<F, B>(&mut self, superversion: Version, callback: F)
    where
        F: Fn(&mut Self, Version) -> B,
    {
        for i in 0..self.versioning().subversions(superversion).len() {
            let subversion = self.versioning().subversions(superversion)[i];
            callback(self, subversion);
        }
    }
}

#[rustfmt::skip]
pub mod testing {
    use super::*;
    use crate::{structures::unionfind::testing::{assert_classes, set}, util::id::Id};

    pub fn version_independence<U>()
    where U: VersionedUnionFind + Default, U::ElementData: Default
    {
        //! Performing operations on independent versions should not affect each
        //! other.
        let mut uf: U = U::default();
        let ids = uf.take(VersionTree::ROOT_VERSION).new_defaults(4);
        let (x, y, v, w) = (ids[0], ids[1], ids[2], ids[3]);

        let v1 = uf.branchout(VersionTree::ROOT_VERSION);
        assert_classes!(uf.focus(v1), set!(x), set!(y), set!(v), set!(w));
        uf.take(v1).union(x, y);
        assert_classes!(uf.focus(v1), set!(x;y), set!(v), set!(w));

        let v2 = uf.branchout(VersionTree::ROOT_VERSION);
        assert_classes!(uf.focus(v2), set!(x), set!(y), set!(v), set!(w));
        uf.take(v2).union(v, w);
        assert_classes!(uf.focus(v2), set!(x), set!(y), set!(v;w));
        assert_classes!(uf.focus(v1), set!(x;y), set!(v), set!(w));
    }
    pub fn version_inheritance<U>()
    where U: VersionedUnionFind + Default, U::ElementData: Default
    {
        //! A subversion should inherit the equivalences of its superversion.
        let mut uf: U = U::default();
        let ids = uf.take(VersionTree::ROOT_VERSION).new_defaults(4);
        let (x, y, v, w) = (ids[0], ids[1], ids[2], ids[3]);

        let v1 = uf.branchout(VersionTree::ROOT_VERSION);
        assert_classes!(uf.focus(v1), set!(x), set!(y), set!(v), set!(w));
        uf.take(v1).union(x, w);
        assert_classes!(uf.focus(v1), set!(x;w), set!(y), set!(v));

        // Inheritance at creation
        let v1_1 = uf.branchout(v1);
        assert_classes!(uf.focus(v1_1), set!(x;w), set!(y), set!(v));
        uf.take(v1_1).union(x, y);
        assert_classes!(uf.focus(v1_1), set!(x;w;y), set!(v));
        assert_classes!(uf.focus(v1), set!(x;w), set!(y), set!(v));

        // Inheritance after creation
        uf.flags_mut().lock_superversions = false;
        uf.flags_mut().propagate = true;
        uf.take(v1).union(v, y);
        assert_classes!(uf.focus(v1_1), set!(x;w;y;v));
        assert_classes!(uf.focus(v1), set!(x;w), set!(y;v));
    }
    pub fn version_inheritance_nested<U>()
    where U: VersionedUnionFind + Default, U::ElementData: Default
    {
        //! A descendant should inherit the equivalences of its ancestor.
        let mut uf: U = U::default();
        let ids = uf.take(VersionTree::ROOT_VERSION).new_defaults(4);
        let (x, y, v, w) = (ids[0], ids[1], ids[2], ids[3]);

        let v1 = uf.branchout(VersionTree::ROOT_VERSION);
        assert_classes!(uf.focus(v1), set!(x), set!(y), set!(v), set!(w));
        uf.take(v1).union(x, w);
        assert_classes!(uf.focus(v1), set!(x;w), set!(y), set!(v));

        // Inheritance at creation
        let v1_1 = uf.branchout(v1);
        let v1_1_1 = uf.branchout(v1_1);
        assert_classes!(uf.focus(v1_1_1), set!(x;w), set!(y), set!(v));
        uf.take(v1_1_1).union(x, y);
        assert_classes!(uf.focus(v1_1_1), set!(x;w;y), set!(v));
        assert_classes!(uf.focus(v1), set!(x;w), set!(y), set!(v));

        // Inheritance after creation
        uf.flags_mut().lock_superversions = false;
        uf.flags_mut().propagate = true;
        uf.take(v1).union(v, y);
        assert_classes!(uf.focus(v1_1_1), set!(x;w;y;v));
        assert_classes!(uf.focus(v1), set!(x;w), set!(y;v));
    }
    pub fn version_inheritance_loops<U>()
    where U: VersionedUnionFind + Default, U::ElementData: Default
    {
        //! Performing operations on superversions should terminate when they
        //! create an inheritance loop.
        let mut uf: U = U::default();
        uf.flags_mut().lock_superversions = false;
        uf.flags_mut().propagate = true;

        let ids = uf.take(VersionTree::ROOT_VERSION).new_defaults(4);
        let (x, y, v, w) = (ids[0], ids[1], ids[2], ids[3]);

        let v1 = uf.branchout(VersionTree::ROOT_VERSION);
        let v1_1 = uf.branchout(v1);
        let v1_1_1 = uf.branchout(v1_1);

        assert_classes!(uf.focus(v1), set!(x), set!(y), set!(v), set!(w));
        assert_classes!(uf.focus(v1_1), set!(x), set!(y), set!(v), set!(w));
        assert_classes!(uf.focus(v1_1_1), set!(x), set!(y), set!(v), set!(w));
        uf.take(v1_1_1).union(y, x); // x ----1.1.1----> y ..... v ----1.1.1----> w ..... x
        uf.take(v1_1_1).union(w, v); // x ----1.1.1----> y ..... v ----1.1.1----> w ..... x
        uf.take(v1_1).union(y, x); // x -{1.1.1;1.1}-> y ..... v ----1.1.1----> w ..... x
        uf.take(v1_1).union(w, v); // x -{1.1.1;1.1}-> y ..... v -{1.1.1;1.1}-> w ..... x
        uf.take(v1).union(v, y); // x -{1.1.1;1.1}-> y --1-> v -{1.1.1;1.1}-> w ..... x
        uf.take(v1).union(x, w); // x -{1.1.1;1.1}-> y --1-> v -{1.1.1;1.1}-> w --1-> x
        assert_classes!(uf.focus(v1), set!(x;w), set!(y;v));
        assert_classes!(uf.focus(v1_1), set!(x;y;v;w));
        assert_classes!(uf.focus(v1_1_1), set!(x;y;v;w));
    }
    pub fn version_path_compression<U>()
    where U: VersionedUnionFind + Default, U::ElementData: Default
    {
        //! After finding the canonical element of an element in one version,
        //! its parent should be updated to the canonical element in that
        //! version, depending on the path compression strategy.
        let mut uf: U = U::default();

        let v0 = VersionTree::ROOT_VERSION;
        let ids = uf.take(v0).new_defaults(10);
        assert!(uf.focus(v0).universe().count() == 10);

        // Union-Find as lists of children: *[9, 5[8[7], 6, 4[3[2[1[0]]]]]]
        uf.take(v0).union(ids[4], ids[5]);
        uf.take(v0).union(ids[3], ids[4]);
        uf.take(v0).union(ids[0], ids[6]);
        uf.take(v0).union(ids[7], ids[8]);

        let v0_1 = uf.branchout(VersionTree::ROOT_VERSION);
        uf.take(v0_1).union(ids[2], ids[3]);
        uf.take(v0_1).union(ids[1], ids[2]);
        uf.take(v0_1).union(ids[0], ids[7]);
        uf.take(v0_1).union(ids[0], ids[1]);

        let v0_1_1 = uf.branchout(v0_1);
        // uf.take(v0_1_1).union(ids[0], ids[1]);
   
        // Before Path Compression
        #[cfg(feature = "pc-none")]
        let previous_parents: Vec<(Id, Version)> = ids.iter().map(|id| uf.get_parent_and_version(*id, v0_1_1)).collect();
        #[cfg(feature = "pc-full")]
        let canonicals: Vec<(Id, Version)> = ids.iter().map(|id| (uf.focus(v0_1_1).find(*id), v0_1_1)).collect();

        // Path Compression (in 0.1.1; on all leaves)
        let leaves = [5, 6, 8, 9];
        for &leaf in leaves.iter() {
            uf.take(v0_1_1).find_mut(ids[leaf]);
        }

        // After Path Compression
        let current_parents: Vec<(Id, Version)> = ids.iter().map(|id| uf.get_parent_and_version(*id, v0_1_1)).collect();
        #[cfg(feature = "pc-none")]
        assert_eq!(current_parents, previous_parents);
        #[cfg(feature = "pc-split")]
        // Node:                         0       1       2       3       4       5       6       7       8       9
        assert_eq!(current_parents, [ids[0], ids[0], ids[0], ids[1], ids[2], ids[3], ids[0], ids[0], ids[0], ids[9]].map(|id| (id, v0_1_1)).to_vec());
        #[cfg(feature = "pc-half")]
        assert_eq!(current_parents, vec![ // Node:
            (ids[0], v0_1_1),             // 0
            (ids[0], v0_1_1),             // 1
            (ids[1], v0_1),               // 2   
            (ids[1], v0_1_1),             // 3
            (ids[3], v0),                 // 4
            (ids[3], v0_1_1),             // 5
            (ids[0], v0_1_1),             // 6
            (ids[0], v0_1),               // 7
            (ids[0], v0_1_1),             // 8
            (ids[9], v0_1_1),             // 9
        ]);
        #[cfg(feature = "pc-full")]
        assert_eq!(current_parents, canonicals);
    }
    pub fn version_projection<U>()
    where U: VersionedUnionFind + Default, U::ElementData: Default + Clone
    {
        //! The projection of a version should be the [UnionFind] that encodes
        //! the same equivalence relation as the original [VersionedUnionFind]
        //! at that version.
        let mut uf: U = U::default();
        let ids = uf.take(VersionTree::ROOT_VERSION).new_defaults(4);
        let (x, y, v, w) = (ids[0], ids[1], ids[2], ids[3]);

        let v1 = uf.branchout(VersionTree::ROOT_VERSION);
        assert_classes!(uf.focus(v1), set!(x), set!(y), set!(v), set!(w));
        uf.take(v1).union(y, v); // x ...... y <--1-- v
        assert_classes!(uf.focus(v1), set!(x), set!(y;v), set!(w));

        let v2 = uf.branchout(VersionTree::ROOT_VERSION);
        assert_classes!(uf.focus(v2), set!(x), set!(y), set!(v), set!(w));
        assert_classes!(uf.focus(v2), set!(x), set!(y), set!(v), set!(w));

        let v1_1 = uf.branchout(v1);
        assert_classes!(uf.focus(v1_1), set!(x), set!(y;v), set!(w));
        uf.take(v1_1).union(x, y); // x <-1.1- y <--1-- v
        assert_classes!(uf.focus(v1_1), set!(x;y;v), set!(w));

        use crate::structures::unionfind::basic::UnionFind as Standard;
        use crate::structures::unionfind::UnionFind as _;
        let uf1 = uf.focus(v1).copy_into::<Standard<U::ElementData>>();
        assert_classes!(uf1, set!(x), set!(y;v), set!(w));
        let uf2 = uf.focus(v2).copy_into::<Standard<U::ElementData>>();
        assert_classes!(uf2, set!(x), set!(y), set!(v), set!(w));
        let uf1_1 = uf.focus(v1_1).copy_into::<Standard<U::ElementData>>();
        assert_classes!(uf1_1, set!(x;y;v), set!(w));
    }
    pub fn rebase_lossless<U>()
    where U: VersionedUnionFind + Default, U::ElementData: Default
    {
        //! Rebasing a version should not lose any information contained by its
        //! previous ancestors.
        let mut uf: U = U::default();
        let ids = uf.take(VersionTree::ROOT_VERSION).new_defaults(4);
        let (x, y, v, w) = (ids[0], ids[1], ids[2], ids[3]);

        let v1 = uf.branchout(VersionTree::ROOT_VERSION);
        uf.take(v1).union(y, v); // x ...... y <--1-- v ...... w
        let v1_1 = uf.branchout(v1);
        uf.take(v1_1).union(x, y); // x <-1.1- y <--1-- v ...... w

        let v1_1_1 = uf.branchout(v1_1);
        uf.rebase(v1_1_1);
        assert_classes!(uf.focus(v1_1_1), set!(x;y;v), set!(w));
    }
    pub fn rebase_independence<U>()
    where U: VersionedUnionFind + Default, U::ElementData: Default
    {
        //! Rebasing a version should make it independent from its previous
        //! ancestors.
        let mut uf: U = U::default();
        let ids = uf.take(VersionTree::ROOT_VERSION).new_defaults(4);
        let (x, y, v, w) = (ids[0], ids[1], ids[2], ids[3]);

        let v1 = uf.branchout(VersionTree::ROOT_VERSION);
        let v1_1 = uf.branchout(v1);
        uf.rebase(v1_1);

        assert_classes!(uf.focus(v1), set!(x), set!(y), set!(v), set!(w));
        assert_classes!(uf.focus(v1_1), set!(x), set!(y), set!(v), set!(w));
        uf.take(v1).union(x, y);
        assert_classes!(uf.focus(v1), set!(x;y), set!(v), set!(w));
        assert_classes!(uf.focus(v1_1), set!(x), set!(y), set!(v), set!(w));
    }
    pub fn rebase_root_version<U>()
    where U: VersionedUnionFind + Default, U::ElementData: Default
    {
        //! Should panic
        let mut uf: U = U::default();
        uf.rebase(VersionTree::ROOT_VERSION);
    }
    pub fn rebase_removed_version<U>()
    where U: VersionedUnionFind + Default, U::ElementData: Default
    {
        //! Should panic
        let mut uf: U = U::default();
        let v0 = VersionTree::ROOT_VERSION;
        let v0_1 = uf.branchout(v0);
        uf.remove_version(v0_1);
        uf.rebase(v0_1);
    }
    pub fn remove_root_version<U>()
    where U: VersionedUnionFind + Default, U::ElementData: Default
    {
        //! Should panic
        let mut uf: U = U::default();
        uf.remove_version(VersionTree::ROOT_VERSION);
    }
    pub fn remove_removed_version<U>()
    where U: VersionedUnionFind + Default, U::ElementData: Default
    {
        //! Should panic
        let mut uf: U = U::default();
        let v0 = VersionTree::ROOT_VERSION;
        let v0_1 = uf.branchout(v0);
        uf.remove_version(v0_1);
        uf.remove_version(v0_1);
    }
    pub fn remove_removed_subversion<U>()
    where U: VersionedUnionFind + Default, U::ElementData: Default
    {
        //! Should panic
        let mut uf: U = U::default();
        let v0 = VersionTree::ROOT_VERSION;
        let v0_1 = uf.branchout(v0);
        let v0_1_1 = uf.branchout(v0_1);
        uf.remove_version(v0_1);
        uf.remove_version(v0_1_1);
    }
    pub fn branchout_from_removed_version<U>()
    where U: VersionedUnionFind + Default, U::ElementData: Default
    {
        //! Should panic
        let mut uf: U = U::default();
        let v0 = VersionTree::ROOT_VERSION;
        let v0_1 = uf.branchout(v0);
        uf.remove_version(v0_1);
        uf.branchout(v0_1);
    }
    pub fn project_from_removed_version<U>()
    where U: VersionedUnionFind + Default, U::ElementData: Default
    {
        //! Should panic
        let mut uf: U = U::default();
        let v0 = VersionTree::ROOT_VERSION;
        let v0_1 = uf.branchout(v0);
        uf.remove_version(v0_1);
        uf.project(v0_1);
    }
    pub fn project_mut_from_removed_version<U>()
    where U: VersionedUnionFind + Default, U::ElementData: Default
    {
        //! Should panic
        let mut uf: U = U::default();
        let v0 = VersionTree::ROOT_VERSION;
        let v0_1 = uf.branchout(v0);
        uf.remove_version(v0_1);
        uf.project_mut(v0_1);
    }

    /// #### Description
    /// Generate a test module for the specified [VersionedUnionFind]
    /// implementation. The module will be named after `$module` and will test
    /// the implementation of concrete type `$implementation`.
    #[macro_export]
    macro_rules! test_versioned_unionfind {
        ($module: ident, $implementation: ty) => {
            #[cfg(test)]
            mod $module {
                use super::*;
                use $crate::structures::unionfind::versioned::testing;
                type Impl = $implementation;

                #[test] 
                fn version_independence() { testing::version_independence::<Impl>(); }
                #[test] 
                fn version_inheritance() { testing::version_inheritance::<Impl>(); }
                #[test] 
                fn version_inheritance_nested() { testing::version_inheritance_nested::<Impl>(); }
                #[test] 
                fn version_inheritance_loops() { testing::version_inheritance_loops::<Impl>(); }
                #[test] 
                fn version_path_compression() { testing::version_path_compression::<Impl>(); }
                #[test] 
                fn version_projection() { testing::version_projection::<Impl>(); }
                #[ignore] #[test]
                fn rebase_lossless() { testing::rebase_lossless::<Impl>(); }
                #[ignore] #[test]
                fn rebase_independence() { testing::rebase_independence::<Impl>(); } 
                #[ignore] #[test] #[should_panic(expected = "Cannot rebase the root version")]
                fn rebase_root_version() { testing::rebase_root_version::<Impl>(); }
                #[ignore] #[test] #[should_panic(expected = "Cannot rebase removed version")]
                fn rebase_removed_version() { testing::rebase_removed_version::<Impl>(); }
                #[test] #[should_panic(expected = "Cannot remove the root version")]
                fn remove_root_version() { testing::remove_root_version::<Impl>(); }
                #[test] #[should_panic(expected = "Cannot remove already removed version")]
                fn remove_removed_version() { testing::remove_removed_version::<Impl>(); }
                #[test] #[should_panic(expected = "Cannot remove already removed version")]
                fn remove_removed_subversion() { testing::remove_removed_subversion::<Impl>(); }
                #[test] #[should_panic(expected = "Cannot branchout from removed version")]
                fn branchout_from_removed_version() { testing::branchout_from_removed_version::<Impl>(); }
                #[test] #[should_panic(expected = "Cannot project removed version")]
                fn project_from_removed_version() { testing::project_from_removed_version::<Impl>(); }
                #[test] #[should_panic(expected = "Cannot project removed version")] 
                fn project_mut_from_removed_version() { testing::project_mut_from_removed_version::<Impl>(); }
            }
        };
    }
    pub use test_versioned_unionfind;
}

pub mod graphics {
    use crate::util::id::AsIndex;

    /// Same as [[crate::unionfind::graphics::graphviz]], but for
    /// [VersionedUnionFind]. In particular, edges are labelled with semantic
    /// versioning.
    pub fn graphviz<U>(uf: &U, label: &str) -> String
    where
        U: super::VersionedUnionFind,
        U::ElementData: std::fmt::Debug,
    {
        use crate::structures::unionfind::UnionFindView;
        use crate::structures::versiontree::info::VersionInfo;
        use crate::structures::versiontree::VersionTree;

        let mut out = String::new();
        let version_infos = VersionInfo::extract(uf.versioning());
        let graph_id: String = crate::util::graphviz::new_graph_id();
        out += &format!("subgraph cluster_{} {{", graph_id);
        out += &format!("\n\tlabel = \"{}\";", label);
        let mut names: Vec<String> = Vec::new();
        let uf0 = uf.focus(VersionTree::ROOT_VERSION);
        for node_id in uf0.universe() {
            let node_name = format!("{}_{}", graph_id, node_id);
            let node_label = format!("{}: {:?}", node_id, uf0.get_element(node_id));
            out += &format!("\n\t{} [label=\"{}\"];", node_name, node_label);
            names.push(node_name);
        }
        for child in uf0.universe() {
            let child_label = &names[child.to_i()];
            for version in uf.versioning().versions() {
                let parent = uf.focus(version).get_parent(child);
                let parent_label = &names[parent.to_i()];
                out += &format!(
                    "\n\t\"{}\" -> \"{}\" [label=\"{}\"];",
                    child_label,
                    parent_label,
                    version_infos.get(&version).unwrap().name,
                );
            }
        }
        out += "\n}";
        out
    }
}
