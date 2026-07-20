pub mod basic;

use crate::structures::egraph::{Analysis, EClass, EGraph, EGraphView};
use crate::structures::unionfind::versioned::Flags;
use crate::structures::versiontree::{Version, VersionTree};

/// An [EGraph] that encodes multiple equivalence relations over congruent
/// functions at the same time. Each equivalence relation is identified by a
/// unique version. Versions may inherit from other versions, meaning that
/// their equivalence relations share some equivalences.
pub trait VersionedEGraph {
    type Analysis: Analysis;
    /// The type of immutable projections you can obtain from this [VersionedEGraph].
    type Projection<'a>: EGraphView<Analysis = Self::Analysis>
    where
        Self: 'a;
    /// The type of mutable projections you can obtain from this [VersionedEGraph].
    type ProjectionMut<'a>: EGraph<Analysis = Self::Analysis>
    where
        Self: 'a;

    /// #### Return
    /// An immutable reference to the [Flags] configured for this [VersionedEGraph].
    fn flags(&self) -> &Flags;

    /// #### Return
    /// A mutable reference to the [Flags] configured for this [VersionedEGraph].
    fn flags_mut(&mut self) -> &mut Flags;

    /// #### Return
    /// The version tree maintaining the versions of this [VersionedEGraph].
    fn versioning(&self) -> &VersionTree;

    // TODO unify all branching operations into a single one: maybe make Version
    // an enum where cases have different semantics (e.g., Subversion, Twin,
    // Union, Intersection...)

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
    /// Generate a new immutable projection of this [VersionedEGraph] at version
    /// `v`. Immutable projections are used to perform read operations on
    /// specific versions of a [VersionedEGraph].
    /// #### Return
    /// The new immutable projection.
    /// #### Panics
    /// - "Cannot project removed version": if version `v` was removed.
    fn project(&self, v: Version) -> Self::Projection<'_>;

    /// #### Description
    /// Generate a new mutable projection of this [VersionedEGraph] at version
    /// `v`. Mutable projections are used to perform any operations on specific
    /// versions of a [VersionedEGraph].
    /// #### Return
    /// The new mutable projection.
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

    /// As [EGraphView::get_parent], but also returns the version of the edge
    /// followed to reach the closest valid parent.
    fn get_parent_and_version(&self, x: EClass, v: Version) -> (EClass, Version);

    /// Alias for [project].
    #[inline(always)]
    fn focus(&self, v: Version) -> Self::Projection<'_> {
        self.project(v)
    }
    /// Alias for [project_mut].
    #[inline(always)]
    fn take(&mut self, v: Version) -> Self::ProjectionMut<'_> {
        self.project_mut(v)
    }
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
pub mod adapters {
    use crate::{structures::unionfind::versioned::{graphics::graphviz, VersionedUnionFind}, util::graphviz::GraphViz};
    use crate::util::id::Id;

    use super::*;
    pub struct UnionFindAdapter<G: VersionedEGraph>(G);
    #[inline(always)]
    pub fn unionfind<G: VersionedEGraph>(egraph: G) -> UnionFindAdapter<G> { UnionFindAdapter(egraph) }
    impl<G: VersionedEGraph> VersionedUnionFind for UnionFindAdapter<G> {
        type ElementData = crate::structures::egraph::ENode;
        type Projection<'a> = crate::structures::egraph::adapters::UnionFindAdapter<G::Projection<'a>> where G: 'a;
        type ProjectionMut<'a> = crate::structures::egraph::adapters::UnionFindAdapter<G::ProjectionMut<'a>> where G: 'a;
        #[inline(always)] fn flags(&self) -> &Flags { self.0.flags() }
        #[inline(always)] fn flags_mut(&mut self) -> &mut Flags { self.0.flags_mut() }
        #[inline(always)] fn versioning(&self) -> &VersionTree { self.0.versioning() }
        #[inline(always)] fn branchout(&mut self, v: Version) -> Version { self.0.branchout(v) }
        #[inline(always)] fn twin(&mut self, v: Version) -> Version { self.0.twin(v) }
        #[inline(always)] fn project(&self, v: Version) -> Self::Projection<'_> { crate::structures::egraph::adapters::unionfind(self.0.project(v)) }
        #[inline(always)] fn project_mut(&mut self, v: Version) -> Self::ProjectionMut<'_> { crate::structures::egraph::adapters::unionfind(self.0.project_mut(v)) }
        #[inline(always)] fn remove_version(&mut self, v: Version) { self.0.remove_version(v) }
        #[inline(always)] fn rebase(&mut self, v: Version) { self.0.rebase(v) }
        #[inline(always)] fn get_parent_and_version(&self, x: Id, v: Version) -> (Id, Version) { self.0.get_parent_and_version(x, v) }
    }
    impl<G: VersionedEGraph + Default> Default for UnionFindAdapter<G> {
        fn default() -> Self { UnionFindAdapter(G::default()) }
    }
    impl<G: VersionedEGraph> GraphViz for UnionFindAdapter<G> {
        fn graphviz(&self, label: &str) -> String { graphviz(self, label) }
    }
}

#[rustfmt::skip]
pub mod testing {
    use super::*;
    use crate::structures::egraph::{EClass, ENode, Operator};
    use crate::structures::unionfind::testing::{assert_classes, set};
    use crate::util::id::SymbolIds;

    /// #### Description
    /// Initializes the universe of the given [VersionedEGraph], adding some
    /// [ENode]s to the [VersionTree::ROOT_VERSION].
    /// #### Return
    /// The [EClass]es of `[x,y,v,w,fx,fy,fv,fw]` added in this order.
    pub fn create_universe<G: VersionedEGraph + Default>(egraph: &mut G) -> [EClass; 8] {
        let f: Operator = SymbolIds::from_string("f");
        let &[x, y, v, w] = 
            ["x", "y", "v", "w"]
            .iter()
            .map(|op| { egraph.take(VersionTree::ROOT_VERSION).add(ENode::constant(SymbolIds::from_string(op))) })
            .collect::<Vec<EClass>>()
            .as_slice()
            else { panic!("test is wrongly implemented") };
        let &[fx, fy, fv, fw] = 
            [x, y, v, w].iter()
            .map(|&c| { egraph.take(VersionTree::ROOT_VERSION).add(ENode::application(f, vec![c]))})
            .collect::<Vec<EClass>>()
            .as_slice()
            else { panic!("test is wrongly implemented") };
        [x, y, v, w, fx, fy, fv, fw]
    }

    pub fn versions_should_track_own_enodes<G: VersionedEGraph + Default>() {
        //! Versioned egraphs should keep track of which e-nodes have been added
        //! to which versions for optimizing version-specific operations.
        let mut egraph: G = G::default();
        let v1 = egraph.branchout(VersionTree::ROOT_VERSION);
        let xn = ENode::constant(SymbolIds::from_string("x"));
        let x = egraph.take(v1).add(xn.clone());
        let v2 = egraph.branchout(VersionTree::ROOT_VERSION);
        let yn = ENode::constant(SymbolIds::from_string("y"));
        let y = egraph.take(v2).add(yn.clone());
        assert_eq!(egraph.focus(v1).get_class(&xn).cloned(), Some(x));
        assert_eq!(egraph.focus(v1).get_class(&yn).cloned(), None);
        assert_eq!(egraph.focus(v2).get_class(&xn).cloned(), None);
        assert_eq!(egraph.focus(v2).get_class(&yn).cloned(), Some(y));
    }
    pub fn versions_should_reuse_eclasses<G: VersionedEGraph + Default>() {
        //! Adding the same e-nodes in different versions should reuse the same
        //! eclasses.
        let mut egraph: G = G::default();
        let v1 = egraph.branchout(VersionTree::ROOT_VERSION);
        let x1 = egraph.take(v1).add(ENode::constant(SymbolIds::from_string("x")));
        let v2 = egraph.branchout(VersionTree::ROOT_VERSION);
        let x2 = egraph.take(v2).add(ENode::constant(SymbolIds::from_string("x")));
        assert_eq!(x1, x2)
    }
    pub fn version_independence<G: VersionedEGraph + Default>() {
        //! Performing operations on independent versions should not affect each
        //! other.
        let mut egraph: G = G::default();
        let [x, y, v, w, fx, fy, fv, fw] = create_universe(&mut egraph);

        let v1 = egraph.branchout(VersionTree::ROOT_VERSION);
        assert_classes!(egraph.focus(v1), set!(x), set!(y), set!(v), set!(w), set!(fx), set!(fy), set!(fv), set!(fw));
        egraph.take(v1).union(x, y);
        egraph.take(v1).rebuild();
        assert_classes!(egraph.focus(v1), set!(x;y), set!(v), set!(w), set!(fx;fy), set!(fv), set!(fw));

        let v2 = egraph.branchout(VersionTree::ROOT_VERSION);
        assert_classes!(egraph.focus(v2), set!(x), set!(y), set!(v), set!(w), set!(fx), set!(fy), set!(fv), set!(fw));
        egraph.take(v2).union(v, w);
        egraph.take(v2).rebuild();
        assert_classes!(egraph.focus(v2), set!(x), set!(y), set!(v;w), set!(fx), set!(fy), set!(fv;fw));
    }
    pub fn version_inheritance<G: VersionedEGraph + Default>() {
        //! A subversion should inherit the equivalences of its superversion
        //! at creation.
        let mut egraph: G = G::default();
        let [x, y, v, w, fx, fy, fv, fw] = create_universe(&mut egraph);

        let v1 = egraph.branchout(VersionTree::ROOT_VERSION);
        assert_classes!(egraph.focus(v1), set!(x), set!(y), set!(v), set!(w), set!(fx), set!(fy), set!(fv), set!(fw));
        egraph.take(v1).union(x, y);
        egraph.take(v1).rebuild();
        assert_classes!(egraph.focus(v1), set!(x;y), set!(v), set!(w), set!(fx;fy), set!(fv), set!(fw));

        // Inheritance at creation
        let v1_1 = egraph.branchout(v1);
        assert_classes!(egraph.focus(v1_1), set!(x;y), set!(v), set!(w), set!(fx;fy), set!(fv), set!(fw));
        egraph.take(v1_1).union(v, w);
        egraph.take(v1_1).rebuild();
        assert_classes!(egraph.focus(v1_1), set!(x;y), set!(v;w), set!(fx;fy), set!(fv;fw));
        assert_classes!(egraph.focus(v1), set!(x;y), set!(v), set!(w), set!(fx;fy), set!(fv), set!(fw));

        // Inheritance after creation
        egraph.flags_mut().lock_superversions = false;
        egraph.flags_mut().propagate = true;
        egraph.take(v1).union(x, w);
        egraph.take(v1_1).rebuild();
        assert_classes!(egraph.focus(v1), set!(x;y;w), set!(v), set!(fx;fy), set!(fv), set!(fw));
        assert_classes!(egraph.focus(v1_1), set!(x;y;v;w), set!(fx;fy;fv;fw));
    }
    pub fn version_inheritance_of_rebuilding<G: VersionedEGraph + Default>() {
        //! Rebuilding a superversion first should not break soundness of
        //! rebuilding a subversion later.
        let mut egraph: G = G::default();

        // This problem only exist if you allow modifying superversions
        egraph.flags_mut().lock_superversions = false;  
        egraph.flags_mut().propagate = true;

        let v0 = VersionTree::ROOT_VERSION;
        let [z, x, y, f] = ["z","x","y","f"].map(SymbolIds::from_string);
        let z = egraph.take(v0).add(ENode::constant(z));
        let x = egraph.take(v0).add(ENode::constant(x));
        let y = egraph.take(v0).add(ENode::constant(y));
        let f_x = egraph.take(v0).add(ENode::application(f, vec![x]));
        let f_y = egraph.take(v0).add(ENode::application(f, vec![y]));
        let v0_1 = egraph.branchout(v0);
        assert_classes!(egraph.focus(v0), set!(x), set!(y), set!(z), set!(f_x), set!(f_y));
        assert_classes!(egraph.focus(v0_1), set!(x), set!(y), set!(z), set!(f_x), set!(f_y));

        egraph.take(v0_1).union(z, f_y); // z  <---0.1--- fy (direction is important)
        egraph.take(v0).union(x, y);     // x  <---0.1--- y  (direction is important)
        egraph.take(v0).rebuild();       // fx <----0---- fy (direction is important)
        egraph.take(v0_1).rebuild();     // here: z should be equal to fx
        assert_classes!(egraph.focus(v0), set!(x;y), set!(z), set!(f_x;f_y));
        assert_classes!(egraph.focus(v0_1), set!(x;y), set!(z;f_x;f_y));
    }
    pub fn version_inheritance_of_uncanonical_enodes<G: VersionedEGraph + Default>() {
        //! You should be able to add an e-node in a superversion, and then use
        //! it to query its canonical representative in a subversion, even if
        //! the e-node was never added directly to the subversion itself.
        let mut egraph: G = G::default();

        // This problem only exist if you allow modifying superversions
        egraph.flags_mut().lock_superversions = false;  
        egraph.flags_mut().propagate = true;

        let v0 = VersionTree::ROOT_VERSION;
        let [x, y, f] = ["x","y","f"].map(SymbolIds::from_string);
        let x = egraph.take(v0).add(ENode::constant(x));
        let y = egraph.take(v0).add(ENode::constant(y));
        let v0_1 = egraph.branchout(v0);
        assert_classes!(egraph.focus(v0), set!(x), set!(y));
        assert_classes!(egraph.focus(v0_1), set!(x), set!(y));

        egraph.take(v0_1).union(x, y);  // x <---0.1--- y (direction is important)
        assert_classes!(egraph.focus(v0), set!(x), set!(y));
        assert_classes!(egraph.focus(v0_1), set!(x;y));
        egraph.take(v0_1).rebuild();    

        let f_x = egraph.take(v0).add(ENode::application(f, vec![x]));
        let f_y = egraph.take(v0).add(ENode::application(f, vec![y]));
        assert_classes!(egraph.focus(v0), set!(x), set!(y), set!(f_x), set!(f_y));
        assert_classes!(egraph.focus(v0_1), set!(x;y), set!(f_x;f_y));
    }
    pub fn version_projection<G: VersionedEGraph + Default>() {
        //! The projection of a version should be the [EGraph] that encodes
        //! the same equivalence relation as the original [VersionedEGraph]
        //! at that version.
        let mut egraph: G = G::default();
        let [x, y, v, w, fx, fy, fv, fw] = create_universe(&mut egraph);

        let v1 = egraph.branchout(VersionTree::ROOT_VERSION);
        assert_classes!(egraph.focus(v1), set!(x), set!(y), set!(v), set!(w), set!(fx), set!(fy), set!(fv), set!(fw));
        egraph.take(v1).union(y, v); // x ...... y <--1-- v
        egraph.take(v1).rebuild();
        assert_classes!(egraph.focus(v1), set!(x), set!(y;v), set!(w), set!(fx), set!(fy;fv), set!(fw));

        let v2 = egraph.branchout(VersionTree::ROOT_VERSION);
        assert_classes!(egraph.focus(v2), set!(x), set!(y), set!(v), set!(w), set!(fx), set!(fy), set!(fv), set!(fw));
        egraph.take(v2).rebuild();
        assert_classes!(egraph.focus(v2), set!(x), set!(y), set!(v), set!(w), set!(fx), set!(fy), set!(fv), set!(fw));

        let v1_1 = egraph.branchout(v1);
        assert_classes!(egraph.focus(v1_1), set!(x), set!(y;v), set!(w), set!(fx), set!(fy;fv), set!(fw));
        egraph.take(v1_1).union(x, y); // x <-1.1- y <--1-- v
        egraph.take(v1_1).rebuild();
        assert_classes!(egraph.focus(v1_1), set!(x;y;v), set!(w), set!(fx;fy;fv), set!(fw));

        use crate::structures::egraph::basic::EGraph as Standard;
        use crate::structures::egraph::EGraph as _;
        let egraph1 = egraph.focus(v1).copy_into::<Standard<()>>();
        assert_classes!(egraph1, set!(x), set!(y;v), set!(w), set!(fx), set!(fy;fv), set!(fw));

        let egraph1_1 = egraph.focus(v1_1).copy_into::<Standard<()>>();
        assert_classes!(egraph1_1, set!(x;y;v), set!(w), set!(fx;fy;fv), set!(fw));

        let egraph2 = egraph.focus(v2).copy_into::<Standard<()>>();
        assert_classes!(egraph2, set!(x), set!(y), set!(v), set!(w), set!(fx), set!(fy), set!(fv), set!(fw));
    }

    pub fn rebase_lossless<G: VersionedEGraph + Default>() {
        //! Rebasing a version should not lose any information contained by its
        //! previous ancestors.
        let mut egraph: G = G::default();
        let [x, y, v, w, fx, fy, fv, fw] = create_universe(&mut egraph);

        // TODO broken test
        let v1 = egraph.branchout(VersionTree::ROOT_VERSION);
        egraph.take(v1).union(y, v); // x ...... y <--1-- v
        egraph.take(v1).rebuild();

        let v1_1 = egraph.branchout(v1);
        egraph.take(v1_1).union(x, y); // x <-1.1- y <--1-- v
        egraph.take(v1_1).rebuild();

        let v1_1_1 = egraph.branchout(v1_1);
        egraph.rebase(v1_1_1);
        assert_classes!(egraph.focus(v1_1_1), set!(x;y;v), set!(w), set!(fx;fy;fv), set!(fw));
    }
    pub fn rebase_independence<G: VersionedEGraph + Default>() {
        //! Rebasing a version should make it independent from its previous
        //! ancestors.
        let mut egraph: G = G::default();
        let [x, y, v, w, fx, fy, fv, fw] = create_universe(&mut egraph);

        let v1 = egraph.branchout(VersionTree::ROOT_VERSION);
        let v1_1 = egraph.branchout(v1);
        egraph.rebase(v1_1);

        assert_classes!(egraph.focus(v1), set!(x), set!(y), set!(v), set!(w), set!(fx), set!(fy), set!(fv), set!(fw));
        assert_classes!(egraph.focus(v1_1), set!(x), set!(y), set!(v), set!(w), set!(fx), set!(fy), set!(fv), set!(fw));
        egraph.take(v1).union(x, y);
        egraph.take(v1).rebuild();
        assert_classes!(egraph.focus(v1), set!(x;y), set!(v), set!(w), set!(fx;fy), set!(fv), set!(fw));
        assert_classes!(egraph.focus(v1_1), set!(x), set!(y), set!(v), set!(w), set!(fx), set!(fy), set!(fv), set!(fw));
    }
    pub fn rebase_root_version<G: VersionedEGraph + Default>() {
        //! Should panic
        let mut egraph: G = G::default();
        egraph.rebase(VersionTree::ROOT_VERSION);
    }
    pub fn rebase_removed_version<G: VersionedEGraph + Default>() {
        //! Should panic
        let mut egraph: G = G::default();
        let v0 = VersionTree::ROOT_VERSION;
        let v0_1 = egraph.branchout(v0);
        egraph.remove_version(v0_1);
        egraph.rebase(v0_1);
    }

    pub fn remove_root_version<G: VersionedEGraph + Default>() {
        //! Should panic
        let mut egraph: G = G::default();
        egraph.remove_version(VersionTree::ROOT_VERSION);
    }

    pub fn remove_removed_version<G: VersionedEGraph + Default>() {
        //! Should panic
        let mut egraph: G = G::default();
        let v0 = VersionTree::ROOT_VERSION;
        let v0_1 = egraph.branchout(v0);
        egraph.remove_version(v0_1);
        egraph.remove_version(v0_1);
    }

    pub fn remove_removed_subversion<G: VersionedEGraph + Default>() {
        //! Should panic
        let mut egraph: G = G::default();
        let v0 = VersionTree::ROOT_VERSION;
        let v0_1 = egraph.branchout(v0);
        let v0_1_1 = egraph.branchout(v0_1);
        egraph.remove_version(v0_1);
        egraph.remove_version(v0_1_1);
    }

    pub fn branchout_from_removed_version<G: VersionedEGraph + Default>() {
        //! Should panic
        let mut egraph: G = G::default();
        let v0 = VersionTree::ROOT_VERSION;
        let v0_1 = egraph.branchout(v0);
        egraph.remove_version(v0_1);
        egraph.branchout(v0_1);
    }

    pub fn project_from_removed_version<G: VersionedEGraph + Default>() {
        //! Should panic
        let mut egraph: G = G::default();
        let v0 = VersionTree::ROOT_VERSION;
        let v0_1 = egraph.branchout(v0);
        egraph.remove_version(v0_1);
        egraph.project(v0_1);
    }

    pub fn project_mut_from_removed_version<G: VersionedEGraph + Default>() {
        //! Should panic
        let mut egraph: G = G::default();
        let v0 = VersionTree::ROOT_VERSION;
        let v0_1 = egraph.branchout(v0);
        egraph.remove_version(v0_1);
        egraph.project_mut(v0_1);
    }

    /// #### Description
    /// Generate a test module for the specified [VersionedEGraph]
    /// implementation. The module will be named after `$module` and will test
    /// the implementation of concrete type `$implementation`.
    #[macro_export]
    macro_rules! test_versioned_egraph {
        ($module: ident, $implementation: ty) => {
            #[cfg(test)]
            mod $module {
                use super::*;
                use $crate::structures::egraph::versioned::testing;
                type Impl = $implementation;
                type UnionFindImpl = $crate::structures::egraph::versioned::adapters::UnionFindAdapter<Impl>;

                $crate::structures::unionfind::versioned::testing::test_versioned_unionfind!(unionfind_tests, UnionFindImpl);
                #[test]
                fn versions_should_reuse_eclasses() { testing::versions_should_reuse_eclasses::<Impl>(); }
                #[test]
                fn versions_should_track_own_enodes() { testing::versions_should_track_own_enodes::<Impl>(); }
                #[test]
                fn version_independence() { testing::version_independence::<Impl>(); }
                #[test]
                fn version_inheritance() { testing::version_inheritance::<Impl>(); }
                #[test]
                fn version_inheritance_of_rebuilding() { testing::version_inheritance_of_rebuilding::<Impl>(); }
                #[test]
                fn version_inheritance_of_uncanonical_enodes() { testing::version_inheritance_of_uncanonical_enodes::<Impl>(); }
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
    pub use test_versioned_egraph;
}
