use crate::structures::{
    egraph::{Analysis, EClass, EGraph, EGraphView, ENode, Operator},
    Map, Set,
};
/// The immutable operations of an [DiEGraph].
pub trait DiEGraphView: EGraphView {
    /// #### Return
    /// True if the [EClass]es `x` and `y` are not equivalent. False otherwise.
    /// #### Note
    /// The [EClass] `xc` and `yc` must be canonical, otherwise the behavior is
    /// undefined. You can use [unequal] to query for disequality without this
    /// restriction, at a slight cost in performances.
    fn unequal_canonical(&self, xc: EClass, yc: EClass) -> bool;
    /// #### Return
    /// True if any two [EClass]es in this [EGraph] are both [equal] and
    /// [unequal]. False otherwise.
    fn is_consistent(&self) -> bool;
    /// #### Return
    /// True if the [EClass]es `x` and `y` are not equivalent. False otherwise.
    fn unequal(&self, x: EClass, y: EClass) -> bool {
        let xc = self.find(x);
        let yc = self.find(y);
        self.unequal_canonical(xc, yc)
    }
}
/// An [EGraph] with support for disequalities. The user can specify disequality
/// constraints between [EClass]es with [DiEGraph::disunion] and discover
/// contradictions with [DiEGraphView::is_consistent].
pub trait DiEGraph: DiEGraphView + EGraph {
    /// #### Description
    /// Add a new disequality constraint between the [EClass]es `x` and `y`.
    /// #### Note
    /// The [EClass] `xc` and `yc` must be canonical, otherwise the behavior is
    /// undefined. You can use [disunion] to merge [EClass]es without this
    /// restriction, at a slight cost in performances.
    fn disunion_canonical(&mut self, xc: EClass, yc: EClass);

    /// Same as [disunion_canonical], but the [EClass]es `x` and `y` are not
    /// required to be canonical.
    fn disunion(&mut self, x: EClass, y: EClass) {
        let xc = self.find_mut(x);
        let yc = self.find_mut(y);
        self.disunion_canonical(xc, yc);
    }
}

#[derive(Clone, Copy, Default)]
/// The analysis used to store disequality edges in an [EGraph].
pub struct DisequalityEdges;
pub type Forbids = Map<EClass, Set<EClass>>;
impl Analysis for DisequalityEdges {
    type EClassDatas = Forbids;
    #[inline(always)]
    fn make_data<G: EGraph<Analysis = Self>>(_egraph: &mut G, _xc: &EClass) {
        // avoid empty forbids and only create them when needed during union
    }
    #[inline(always)]
    fn merge_data<G: EGraph<Analysis = Self>>(egraph: &mut G, parent: &EClass, child: &EClass) {
        if let Some(child_forbids) = egraph.datas_mut().remove(child) {
            egraph.datas_mut().entry(*parent).or_default().extend(child_forbids);
        }
    }
}

/// The default implementation of [DiEGraphView] for an [EGraphView] with [DisequalityEdges].
pub trait DefaultDiEGraphView: EGraphView<Analysis = DisequalityEdges> {}
impl<G: DefaultDiEGraphView> DiEGraphView for G {
    #[cfg_attr(feature = "trace-disequalities", tracing::instrument(skip(self), ret))]
    fn is_consistent(&self) -> bool {
        self.datas().iter().all(|(xc, nxs)| nxs.iter().all(|nx| self.find(*nx) != *xc))
    }
    #[cfg_attr(feature = "trace-disequalities", tracing::instrument(skip(self), ret))]
    fn unequal_canonical(&self, xc: EClass, yc: EClass) -> bool {
        self.datas().get(&xc).filter(|&nxs| nxs.iter().any(|nx| self.find(*nx) == yc)).is_some()
    }
}
/// The default implementation of [DiEGraph] for an [EGraph] with [DisequalityEdges].
pub trait DefaultDiEGraph: DefaultDiEGraphView + EGraph<Analysis = DisequalityEdges> {}
impl<G: DefaultDiEGraph> DiEGraph for G {
    #[cfg_attr(feature = "trace-disequalities", tracing::instrument(skip(self), ret))]
    fn disunion_canonical(&mut self, xc: EClass, yc: EClass) {
        self.datas_mut().entry(xc).or_default().insert(yc);
        if xc != yc {
            self.datas_mut().entry(yc).or_default().insert(xc);
        }
    }
}

#[rustfmt::skip]
pub mod testing {
    use super::*;
    use crate::structures::egraph::versioned::*;
    use crate::structures::unionfind::testing::{assert_classes, set};
    use crate::structures::versiontree::*;
    use crate::util::id::SymbolIds;

    pub fn disequality_recursive_eclasses<G>()
    where G: DiEGraph + Default
    {
        //! A [DiEGraph] should be able to encode disequality constraints for
        //! recursively-defined equivalence classes.
        let mut egraph: G = G::default();
        let f: Operator = SymbolIds::from_string("f");
        let x = egraph.add(ENode::constant(SymbolIds::from_string("x")));
        let fx = egraph.add(ENode::application(f, vec![x]));
        egraph.disunion(fx, x);
        egraph.union(fx, x); // deeper term first

        let y = egraph.add(ENode::constant(SymbolIds::from_string("y")));
        let fy = egraph.add(ENode::application(f, vec![y]));
        egraph.disunion(y, fy);
        egraph.union(y, fy); // deeper term last

        egraph.rebuild();
        assert_classes!(egraph, set!(x;fx), set!(y;fy));
        assert!(egraph.unequal(fx, x));
        assert!(egraph.unequal(fy, y));
        assert!(!egraph.is_consistent());
    }

    pub fn disequality_deeply_nested_recursive_eclasses<G>()
    where G: DiEGraph + Default
    {
        //! As [disequality_recursive_eclasses], but with deeper recursive terms.
        let mut egraph: G = G::default();
        let f: Operator = SymbolIds::from_string("f");
        let x = egraph.add(ENode::constant(SymbolIds::from_string("x")));
        let fx = egraph.add(ENode::application(f, vec![x]));
        let ffx = egraph.add(ENode::application(f, vec![fx]));
        let fffx = egraph.add(ENode::application(f, vec![ffx]));
        let ffffx = egraph.add(ENode::application(f, vec![fffx]));
        let fffffx = egraph.add(ENode::application(f, vec![ffffx]));

        assert_classes!(egraph, set!(x), set!(fx), set!(ffx), set!(fffx), set!(ffffx), set!(fffffx));
        egraph.union(ffx, x);
        egraph.disunion(x, ffffx);
        egraph.disunion(fx, fffffx);
        egraph.rebuild();
        assert_classes!(egraph, set!(x;ffx;ffffx), set!(fx;fffx;fffffx));
        assert!(egraph.unequal(x, ffx));
        assert!(egraph.unequal(fx, fffx));
        assert!(!egraph.is_consistent());
    }

    /// #### Description
    /// Generate a test module for the specified [DiEGraph] implementation.
    /// The module will be named after `$module` and will test the
    /// implementation of the concrete type `$implementation`.
    #[macro_export]
    macro_rules! test_diegraph {
        ($module: ident, $implementation: ty) => {
            #[cfg(test)]
            mod $module {
                use super::*;
                use $crate::structures::egraph::extensions::diegraph::testing;
                type Impl = $implementation;

                #[test]
                fn disequality_recursive_eclasses() { testing::disequality_recursive_eclasses::<Impl>(); }
                #[test]
                fn disequality_deeply_nested_recursive_eclasses() { testing::disequality_deeply_nested_recursive_eclasses::<Impl>(); }
            }
        };
    }
    pub use test_diegraph;

    pub fn disequality_version_independence<G>()
    where G: VersionedEGraph + Default, for<'a> G::ProjectionMut<'a>: DiEGraph,
    {
        //! Performing operations on independent versions should not affect each
        //! other.
        use crate::structures::egraph::versioned::testing::create_universe;
        let mut egraph: G = G::default();
        let [x, y, v, w, _, _, _, _] = create_universe(&mut egraph);

        let v1 = egraph.branchout(VersionTree::ROOT_VERSION);
        assert!(egraph.take(v1).is_consistent());
        egraph.take(v1).union(x, y);
        egraph.take(v1).disunion(x, y);
        egraph.take(v1).rebuild();
        assert!(!egraph.take(v1).is_consistent());

        let v2 = egraph.branchout(VersionTree::ROOT_VERSION);
        assert!(egraph.take(v2).is_consistent());
        egraph.take(v2).union(v, w);
        egraph.take(v2).disunion(v, w);
        egraph.take(v2).rebuild();
        assert!(!egraph.take(v2).is_consistent());

        assert!(egraph.take(v1).unequal(x, y));
        assert!(!egraph.take(v2).unequal(x, y));
        assert!(!egraph.take(v1).unequal(v, w));
        assert!(egraph.take(v2).unequal(v, w));
    }
    pub fn disequality_version_inheritance<G>()
    where G: VersionedEGraph + Default, for<'a> G::ProjectionMut<'a>: DiEGraph
    {
        //! Performing operations on superversions should affect their
        //! subversions.
        use crate::structures::egraph::versioned::testing::create_universe;
        let mut egraph: G = G::default();
        let [x, y, v, w, fx, fy, fv, fw] = create_universe(&mut egraph);

        let v1 = egraph.branchout(VersionTree::ROOT_VERSION);
        egraph.take(v1).union(x, y);
        egraph.take(v1).disunion(fv, fw);
        egraph.take(v1).rebuild();
        assert!(egraph.take(v1).unequal(fv, fw));
        assert!(egraph.take(v1).is_consistent());

        // Inheritance at creation
        let v1_1 = egraph.branchout(v1);
        egraph.take(v1_1).union(v, w);
        egraph.take(v1_1).rebuild();
        assert!(egraph.take(v1_1).unequal(fv, fw));
        assert!(!egraph.take(v1_1).is_consistent());

        // Inheritance after creation
        egraph.flags_mut().lock_superversions = false;
        egraph.flags_mut().propagate = true;
        let v1_2 = egraph.branchout(v1);
        egraph.take(v1).disunion(fx, fy);
        assert!(egraph.take(v1_2).unequal(fx, fy));
        assert!(!egraph.take(v1_2).is_consistent());
    }

    /// #### Description
    /// Generate a test module for the specified [VersionedEGraph]
    /// implementation with support for disequality constraints. The module will
    /// be named after `$module` and will test the implementation of concrete
    /// type `$implementation`.
    #[macro_export]
    macro_rules! test_versioned_diegraph {
        ($module: ident, $implementation: ty) => {
            #[cfg(test)]
            mod $module {
                use super::*;
                use $crate::structures::egraph::extensions::diegraph::testing;
                type Impl = $implementation;

                #[test]
                fn disequality_version_independence() { testing::disequality_version_independence::<Impl>(); }
                #[test]
                fn disequality_version_inheritance() { testing::disequality_version_inheritance::<Impl>(); }
            }
        };
    }
    pub use test_versioned_diegraph;
}
