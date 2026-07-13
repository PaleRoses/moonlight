use crate::util::id::{AsIndex, Id};

pub mod basic;
pub mod ematching;
pub mod extensions;
pub mod persistent;
pub mod versioned;

/// A uniquely identified operator.
pub type Operator = Id;
/// A uniquely identified equivalence class.
pub type EClass = Id;

/// A node in an [EGraph]. An [ENode] is an expression representing the
/// application of an unintepreted function `op` to a list of arguments `args`.
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ENode {
    pub op: Operator,
    pub args: Vec<EClass>,
}
impl ENode {
    /// #### Return
    /// A new [ENode] representing the uninterpreted function `operator` applied
    /// to no arguments.
    pub fn constant(operator: Operator) -> ENode {
        ENode { op: operator, args: Vec::new() }
    }

    /// #### Return
    /// A new [ENode] representing the uninterpreted function `operator` applied
    /// to the arguments `arguments`.
    pub fn application(operator: Operator, arguments: Vec<EClass>) -> ENode {
        ENode { op: operator, args: arguments }
    }
}
impl std::fmt::Debug for ENode {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let op = match crate::util::id::SymbolIds::to_string(self.op) {
            Some(s) => s.to_string(),
            None => format!("{}", self.op),
        };
        if self.args.is_empty() {
            write!(f, "{}", op)
        } else {
            write!(f, "{}{:?}", op, self.args)
        }
    }
}
// TODO This is only printing the operator because egg expect this to
// pretty print `RecExpr`. We should separate this properly in another
// trait, because it does not make sense to use `Display` for that.
impl std::fmt::Display for ENode {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(
            f,
            "{}",
            crate::util::id::SymbolIds::to_string(self.op)
                .map(|s| s.to_string())
                .unwrap_or(self.op.to_string())
        )
    }
}
impl crate::util::memory::HasMemory for ENode {
    fn fields(&self) -> Vec<(&str, &dyn crate::util::memory::HasMemory)> {
        vec![("op", &self.op), ("args", &self.args)]
    }
}

pub trait Analysis: Default + Sized {
    type EClassDatas: Default + Clone;
    fn make_data<G: EGraph<Analysis = Self>>(egraph: &mut G, xc: &EClass);
    fn merge_data<G: EGraph<Analysis = Self>>(egraph: &mut G, parent: &EClass, child: &EClass);
}
impl Analysis for () {
    type EClassDatas = ();
    #[inline(always)]
    fn make_data<G: EGraph>(_egraph: &mut G, _xc: &EClass) {}
    #[inline(always)]
    fn merge_data<G: EGraph>(_egraph: &mut G, _xn: &EClass, _xc: &EClass) {}
}

/// The immutable operations of an [EGraph].
pub trait EGraphView {
    type Analysis: Analysis;

    /// #### Return
    /// An immutable reference to the data bound to all [EClass]es in this [EGraph].
    fn datas(&self) -> &<Self::Analysis as Analysis>::EClassDatas;

    /// #### Return
    /// True if this [EGraphView] needs to be rebuilt to be consistent, false
    /// if it was already rebuilt.
    fn is_clean(&self) -> bool;

    /// #### Return
    /// The set of all [EClass]es in this [EGraph].
    fn universe(&self) -> impl std::iter::Iterator<Item = EClass>;

    /// #### Return
    /// The raw parent of the [EClass] `x`.
    /// #### Note
    /// The raw parent may not be the canonical equivalent of the [EClass] `x`.
    fn get_parent(&self, x: EClass) -> EClass;

    /// #### Return
    /// The [ENode] representative of the [EClass] `x`.
    fn get_enode(&self, x: EClass) -> &ENode;

    /// #### Return
    /// A [Some] containing the original [EClass] of the [ENode] `x`, or [None]
    /// if `x` was never added to this [EGraph].
    /// #### Note
    /// The [ENode] is not [canonicalize]d before the query.
    fn get_class(&self, x: &ENode) -> Option<&EClass>;

    /// #### Return
    /// The canonical equivalent of the [EClass] `x`.
    fn find(&self, x: EClass) -> EClass;

    /// #### Return
    /// True if the [EClass]es `x` and `y` are equivalent. False otherwise.
    fn equal(&self, x: EClass, y: EClass) -> bool {
        self.equal_canonical(self.find(x), self.find(y))
    }

    /// #### Return
    /// True if the [EClass]es `x` and `y` are equivalent. False otherwise.
    /// /// #### Note
    /// The [EClass] `xc` and `yc` must be canonical, otherwise the behavior is
    /// undefined. You can use [equal] to query for equality without this
    /// restriction, at a slight cost in performances.
    #[inline(always)]
    fn equal_canonical(&self, xc: EClass, yc: EClass) -> bool {
        xc == yc
    }

    /// #### Return
    /// A new [ENode] obtained by replacing the arguments of `xn` with canonical
    /// [EClass]es.
    fn canonicalize(&self, xn: ENode) -> ENode {
        let mut xn = xn.clone();
        for i in 0..xn.args.len() {
            xn.args[i] = self.find(xn.args[i])
        }
        xn
    }

    /// #### Return
    /// A new [EGraph] that encodes the same equivalence relation as this
    /// [EGraph].
    fn copy_into<G: EGraph + Default>(&self) -> G {
        let mut egraph: G = G::default();
        let mut eclasses: Vec<EClass> = Vec::new();
        for xc in self.universe() {
            let xn = self.get_enode(xc).clone();
            let x = egraph.add(xn);
            eclasses.push(x);
        }
        for xc in self.universe() {
            for yc in self.universe() {
                if self.equal(xc, yc) {
                    egraph.union(eclasses[xc.to_i()], eclasses[yc.to_i()]);
                }
            }
        }
        egraph.rebuild();
        egraph
    }
}

/// An equivalence graph. An [EGraph] is a data structure that represents an
/// equivalence relation over congruent functions.
///
/// It supports operations similar to the [UnionFind], but it also has to
/// maintain congruence invariance.
///
/// Maintainance of congruence invariance is deferred until the user requests it
/// by calling [EGraph::rebuild].
pub trait EGraph: EGraphView {
    /// #### Return
    /// A mutable reference to the data bound to all [EClass]es in this [EGraph].
    fn datas_mut(&mut self) -> &mut <Self::Analysis as Analysis>::EClassDatas;

    /// #### Description
    /// Add an [ENode] to the [EGraph]. The [ENode] is not [canonicalize]d
    /// before being added (as it is assumed to be already [canonicalize]d).
    /// #### Return
    /// The [EClass] containing the specified [ENode].
    /// #### Note
    /// The implementation ensures that calling [EGraph::add] with the same
    /// [ENode] multiple times will always return the same [EClass].
    fn add_canonical(&mut self, xn: ENode) -> EClass;

    /// #### Return
    /// The canonical equivalent of the [EClass] `x`.
    /// #### Note
    /// This method performs `path compression`, reducing the cost of future
    /// calls.
    fn find_mut(&mut self, x: EClass) -> EClass;

    /// #### Description
    /// Merge the [EClass]es `xc` and `yc` into a new [EClass].
    /// #### Return
    /// The new [EClass].
    /// #### Note
    /// The [EClass] `xc` and `yc` must be canonical, otherwise the behavior is
    /// undefined. You can use [union] to merge [EClass]es without this
    /// restriction, at a slight cost in performances.
    fn union_canonical(&mut self, xc: EClass, yc: EClass) -> EClass;

    /// #### Description
    /// Restore congruence invariance in this [EGraph]. This can be achieved by
    /// canonicalizing every argument used by the [ENode]s in the [EGraph].
    /// #### Note
    /// Congruence invariance may be broken after every [add] or [union].
    ///
    /// In the case of [add], you may add an [ENode] that is congruent to an
    /// existing [ENode], without being equal in the [EGraph].
    ///
    /// In the case of [union], you may merge two [EClass]es, such that two or
    /// more [ENode]s become congruent.
    ///
    /// You should call [rebuild] when you want to query the [EGraph] after a
    /// relatively long series of [add]s and [union]s.
    fn rebuild(&mut self);

    /// Same as [add_canonical], but the [ENode] is [canonicalize]d before being
    /// added.
    fn add(&mut self, xn: ENode) -> EClass {
        let xn = self.canonicalize_mut(xn);
        self.add_canonical(xn)
    }

    /// Same as [union_canonical], but the [EClass]es `x` and `y` are not
    /// required to be canonical.
    fn union(&mut self, x: EClass, y: EClass) -> EClass {
        let xc = self.find_mut(x);
        let yc = self.find_mut(y);
        if xc == yc {
            return xc;
        }
        self.union_canonical(xc, yc)
    }

    /// #### Return
    /// A new [ENode] obtained by replacing the arguments of `xn` with canonical
    /// [EClass]es.
    fn canonicalize_mut(&mut self, mut xn: ENode) -> ENode {
        for i in 0..xn.args.len() {
            xn.args[i] = self.find_mut(xn.args[i])
        }
        xn
    }
}

#[rustfmt::skip]
pub mod adapters {
    use crate::{structures::unionfind::{UnionFind, UnionFindView}, util::graphviz::GraphViz};

    use super::*;
    pub struct UnionFindAdapter<G: EGraphView>(G);
    pub fn unionfind<G: EGraphView>(egraph: G) -> UnionFindAdapter<G> { UnionFindAdapter(egraph) }
    impl<G: EGraphView> UnionFindView for UnionFindAdapter<G> {
        type ElementData = ENode;
        #[inline(always)] fn universe(&self) -> impl std::iter::Iterator<Item = EClass> { self.0.universe() }
        #[inline(always)] fn get_element(&self, x: Id) -> &ENode { self.0.get_enode(x) }
        #[inline(always)] fn get_parent(&self, x: Id) -> Id { self.0.get_parent(x) }
        #[inline(always)] fn find(&self, x: Id) -> Id { self.0.find(x) }
        #[inline(always)] fn equal(&self, x: Id, y: Id) -> bool { self.0.equal(x, y) }
    }
    impl<G: EGraph> UnionFind for UnionFindAdapter<G> {
        #[inline(always)] fn new_element(&mut self, element: ENode) -> Id { self.0.add(element) }
        #[inline(always)] fn find_mut(&mut self, x: Id) -> Id { self.0.find_mut(x) }
        #[inline(always)] fn union_canonical(&mut self, xc: Id, yc: Id) -> Id { self.0.union_canonical(xc, yc) }
        #[inline(always)] fn union(&mut self, x: Id, y: Id) -> Id { self.0.union(x, y) }
    }
    impl<G: EGraph + Default> Default for UnionFindAdapter<G> {
        fn default() -> Self { UnionFindAdapter(G::default()) }
    }
    impl<G: EGraph> GraphViz for UnionFindAdapter<G> {
        fn graphviz(&self, label: &str) -> String {
            crate::structures::unionfind::graphics::graphviz(self, label)
        }
    }
}

#[rustfmt::skip]
pub mod testing {
    use super::*;
    use crate::structures::unionfind::testing::{assert_classes, set};
    use crate::util::id::{IdFactory, SymbolIds};

    impl Default for ENode {
        /// #### Return
        /// A new [ENode::constant] with a unique operator.
        /// #### Note
        /// The unique operator is a [crate::UniqueId]. This is used in the test
        /// suite to test an [EGraph] as a [UnionFind].
        fn default() -> Self { ENode::constant(crate::util::id::UniqueIds::create_id()) }
    }

    pub fn recursive_eclasses<G: EGraph + Default>() {
        //! An [EGraph] should be able to encode recursively-defined equivalence
        //! classes.
        let mut egraph: G = G::default();
        let f: Operator = SymbolIds::from_string("f");
        let x = egraph.add(ENode::constant(SymbolIds::from_string("x")));
        let fx = egraph.add(ENode::application(f, vec![x]));
        assert_classes!(egraph, set!(x), set!(fx));
        egraph.union(fx, x); // deeper term first
        assert_classes!(egraph, set!(x;fx));

        let y = egraph.add(ENode::constant(SymbolIds::from_string("y")));
        let fy = egraph.add(ENode::application(f, vec![y]));
        assert_classes!(egraph, set!(y), set!(fy));
        egraph.union(y, fy); // deeper term last
        assert_classes!(egraph, set!(y;fy));

        egraph.rebuild();
        assert_classes!(egraph, set!(x;fx), set!(y;fy));
    }
    pub fn deeply_nested_recursive_eclasses<G: EGraph + Default>() {
        //! As [recursive_eclasses], but with deeper recursive terms.
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
        egraph.rebuild();
        assert_classes!(egraph, set!(x;ffx;ffffx), set!(fx;fffx;fffffx));
    }
    pub fn past_congruence_invariance<G: EGraph + Default>() {
        //! Rebuilding should restore the congruence invariance for nodes that
        //! were defined before any equality.
        let mut egraph: G = G::default();
        let x = egraph.add(ENode::constant(SymbolIds::from_string("x")));
        let y = egraph.add(ENode::constant(SymbolIds::from_string("y")));

        let f: Operator = SymbolIds::from_string("f");
        let fx = egraph.add(ENode::application(f, vec![x]));
        let fy = egraph.add(ENode::application(f, vec![y]));

        let g: Operator = SymbolIds::from_string("g");
        let gx = egraph.add(ENode::application(g, vec![x]));
        let gy = egraph.add(ENode::application(g, vec![y]));
        let ffx = egraph.add(ENode::application(f, vec![fx]));
        let ffy = egraph.add(ENode::application(f, vec![fy]));

        assert_classes!(egraph, set!(x), set!(y), set!(fx), set!(fy), set!(gx), set!(gy), set!(ffx), set!(ffy));
        egraph.union(x, y);
        egraph.rebuild();
        assert_classes!(egraph, set!(x;y), set!(fx;fy), set!(gx;gy), set!(ffx;ffy));
    }
    pub fn future_congruence_invariance<G: EGraph + Default>() {
        //! Rebuilding should restore the congruence invariance for nodes that
        //! were defined after any equality.
        let mut egraph: G = G::default();
        let x = egraph.add(ENode::constant(SymbolIds::from_string("x")));
        let y = egraph.add(ENode::constant(SymbolIds::from_string("y")));

        let f: Operator = SymbolIds::from_string("f");
        let fx = egraph.add(ENode::application(f, vec![x]));
        assert_classes!(egraph, set!(x), set!(y), set!(fx));
        egraph.union(y, x);
        assert_classes!(egraph, set!(x;y), set!(fx));

        let fy = egraph.add(ENode::application(f, vec![y]));
        egraph.rebuild();
        assert_classes!(egraph, set!(x;y), set!(fx;fy));
    }

    /// #### Description
    /// Generate a test module for the specified [EGraph] implementation.
    /// The module will be named after `$module` and will test the
    /// implementation of the concrete type `$implementation`.
    #[macro_export]
    macro_rules! test_egraph {
        ($module: ident, $implementation: ty) => {
            #[cfg(test)]
            mod $module {
                use super::*;
                use $crate::structures::egraph::testing;
                type Impl = $implementation;
                type UnionFindImpl = $crate::structures::egraph::adapters::UnionFindAdapter<Impl>;

                $crate::structures::unionfind::testing::test_unionfind!(unionfind_tests, UnionFindImpl);
                #[test]
                fn recursive_eclasses() { testing::recursive_eclasses::<Impl>(); }
                #[test]
                fn deeply_nested_recursive_eclasses() { testing::deeply_nested_recursive_eclasses::<Impl>(); }
                #[test]
                fn past_congruence_invariance() { testing::past_congruence_invariance::<Impl>(); }
                #[test]
                fn future_congruence_invariance() { testing::future_congruence_invariance::<Impl>(); }
            }
        };
    }
    pub use test_egraph;
}
