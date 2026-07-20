use super::extensions::diegraph::{DefaultDiEGraph, DefaultDiEGraphView};
use super::EGraphView;
use crate::structures::egraph::ematching::machine::MachineRequirements;
use crate::structures::egraph::ematching::naive::engine::HasOperatorStatistics;
use crate::structures::egraph::ematching::naive::testing::test_ematching;
use crate::structures::egraph::extensions::diegraph::testing::test_diegraph;
use crate::structures::egraph::extensions::diegraph::DisequalityEdges;
use crate::structures::egraph::testing::test_egraph;
use crate::structures::egraph::{Analysis, EClass, ENode, Operator};
use crate::structures::unionfind::basic::UnionFind;
use crate::structures::unionfind::{UnionFind as _, UnionFindView as _};
use crate::structures::Map;
use crate::util::id::AsIndex;
use std::collections::BTreeMap;
use std::fmt::Debug;

#[derive(Clone, Default)]
pub struct EGraph<A: Analysis> {
    /// [UnionFind] of [EClass]es bound to their representative [ENode]s.
    unionfind: UnionFind<ENode>,
    /// Map from [ENode]s to their original [EClass], grouped by [Operator].
    /// Used to bind the same [ENode]s to the same [EClass]es.
    enodes_by_op: Vec<BTreeMap<ENode, EClass>>,
    /// The data of each [EClass] in this [EGraph].
    /// Used by the [Analysis] on top of this [EGraph].
    datas: A::EClassDatas,
    // ---- CACHES ---------------------------------------------------------------------------------
    // The following fields do not increase expressiveness, but speed up certain operations.
    // On the other hand, they increase memory consumption and maintainance cost for operations.
    // ---------------------------------------------------------------------------------------------
    /// Map of [EClass]es which use the index [EClass] as an argument.
    /// Speeds up rebuilding.
    dependants: BTreeMap<EClass, Vec<EClass>>,
    /// List of uncanonical [EClass]es involved in unions performed since last
    /// rebuild. Used to defer restoring congruence invariance.   
    worklist: Vec<EClass>,
    /// Whether the e-graph need to be rebuilt (`true`) or not (`false`).
    dirty: bool,
    /// Map from [EClass]es to equivalent [EClass]es.
    /// Used to optimize top-down e-matching.
    eclasses: Map<EClass, Vec<EClass>>,
    /// Enable or disable cache for top-down e-matching.
    ematching_cache: bool,
}
impl<A: Analysis> EGraph<A> {
    pub fn with_ematching_cache() -> Self {
        let mut egraph = Self::default();
        egraph.init_ematching_cache();
        egraph
    }
}
impl<A: Analysis<EClassDatas: Debug>> Debug for EGraph<A> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("EGraph")
            .field("unionfind", &self.unionfind)
            .field("enodes_by_op", &self.enodes_by_op)
            .field("datas", &self.datas)
            .field("dependants", &self.dependants)
            .field("worklist", &self.worklist)
            .field("dirty", &self.dirty)
            .field("eclasses", &self.eclasses)
            .field("ematching_cache", &self.ematching_cache)
            .finish()
    }
}
impl<A: Analysis> super::EGraphView for EGraph<A> {
    type Analysis = A;
    #[inline]
    fn datas(&self) -> &A::EClassDatas {
        &self.datas
    }
    #[inline]
    fn is_clean(&self) -> bool {
        !self.dirty
    }
    #[inline]
    fn universe(&self) -> impl std::iter::Iterator<Item = EClass> {
        self.unionfind.universe()
    }
    #[inline]
    fn get_enode(&self, x: EClass) -> &ENode {
        self.unionfind.get_element(x)
    }
    #[inline]
    fn get_class(&self, xn: &ENode) -> Option<&EClass> {
        self.enodes_by_op.get(xn.op.to_i()).and_then(|xns| xns.get(xn))
    }
    #[inline]
    fn get_parent(&self, x: EClass) -> EClass {
        self.unionfind.get_parent(x)
    }
    #[inline]
    fn find(&self, x: EClass) -> EClass {
        self.unionfind.find(x)
    }
}
impl<A: Analysis> super::EGraph for EGraph<A> {
    fn datas_mut(&mut self) -> &mut A::EClassDatas {
        &mut self.datas
    }
    #[cfg_attr(feature = "trace-egraph-basic", tracing::instrument(skip(self), ret))]
    fn add_canonical(&mut self, xn: ENode) -> EClass {
        match self.get_class(&xn) {
            Some(&x) => self.find_mut(x),
            None => {
                let xc = self.unionfind.new_element(xn.clone());
                for arg in xn.args.iter() {
                    self.dependants.entry(*arg).or_default().push(xc)
                }
                let op_i = xn.op.to_i();
                if op_i >= self.enodes_by_op.len() {
                    self.enodes_by_op.resize_with(op_i + 1, Default::default);
                }
                self.enodes_by_op[op_i].insert(xn, xc);
                if self.ematching_cache {
                    self.eclasses.entry(xc).or_default().push(xc);
                }
                A::make_data(self, &xc);
                xc
            },
        }
    }
    #[cfg_attr(feature = "trace-egraph-basic", tracing::instrument(skip(self), ret))]
    fn find_mut(&mut self, x: EClass) -> EClass {
        self.unionfind.find_mut(x)
    }
    #[cfg_attr(feature = "trace-egraph-basic", tracing::instrument(skip(self), ret))]
    fn union_canonical(&mut self, xc: EClass, yc: EClass) -> EClass {
        // NOTE: order by increasing id (age), which is a partial ordering on
        // depth (?). This avoids infinite loops during rebuilding.
        // Failing Test: [crate::egraph::testing::recursive_eclasses]
        // TODO: try ordering by self.dependants.len() to reduce repairs.
        let mut parent = xc;
        let mut child = yc;
        if parent > child {
            std::mem::swap(&mut parent, &mut child);
        }
        let xy = self.unionfind.union_canonical(parent, child);
        self.worklist.push(child);
        self.dirty = true;
        if self.ematching_cache {
            let mut child_classes = self.eclasses.remove(&child).unwrap();
            self.eclasses.get_mut(&parent).unwrap().append(&mut child_classes);
        }
        A::merge_data(self, &parent, &child);
        xy
    }
    #[cfg_attr(feature = "trace-egraph-basic", tracing::instrument(skip(self), ret))]
    fn rebuild(&mut self) {
        while let Some(child) = self.worklist.pop() {
            if let Some(child_dependants) = self.dependants.remove(&child) {
                for stale in child_dependants {
                    let stale_n = self.unionfind.get_element(stale);
                    let fresh_n = self.canonicalize_mut(stale_n.clone());
                    let fresh = self.add_canonical(fresh_n);
                    self.union(fresh, stale);
                }
            }
        }
        self.rebuild_ematching_cache();
        self.dirty = false;
    }
}
impl<A: Analysis> EGraph<A> {
    #[cfg_attr(feature = "trace-egraph-basic", tracing::instrument(skip(self), ret))]
    fn init_ematching_cache(&mut self) {
        self.ematching_cache = true;
        let mut cache: Map<EClass, Vec<EClass>> = Default::default();
        // NOTE already sorted by operator (the index of the vector)
        for xns in self.enodes_by_op.iter() {
            for xn in xns.keys() {
                let xn = self.canonicalize(xn.clone());
                let xc = self.get_class(&xn).unwrap();
                let xcc = self.find(*xc);
                cache.entry(xcc).or_default().push(*xc);
            }
        }
        self.eclasses = cache;
    }
    #[cfg_attr(feature = "trace-egraph-basic", tracing::instrument(skip(self), ret))]
    fn rebuild_ematching_cache(&mut self) {
        // Restore the cache of equivalent canonicalized enodes, sorted by operator
        if self.ematching_cache {
            let mut eclasses = std::mem::take(&mut self.eclasses);
            for (&_xc, xns) in eclasses.iter_mut() {
                for xnc in xns.iter_mut() {
                    let xn = self.canonicalize(self.unionfind.get_element(*xnc).clone());
                    *xnc = *self.get_class(&xn).unwrap();
                }
                xns.sort_unstable_by_key(|c| self.unionfind.get_element(*c).op);
                xns.dedup();
            }
            self.eclasses = eclasses
        }
    }
}
impl<A: Analysis> MachineRequirements for EGraph<A> {
    #[cfg_attr(
        all(feature = "trace-egraph-basic", feature = "trace-machine"),
        tracing::instrument(skip(self), ret)
    )]
    fn canonicals(&self) -> impl std::iter::Iterator<Item = &egg::Id> {
        self.eclasses.keys()
    }
    #[cfg_attr(
        all(feature = "trace-egraph-basic", feature = "trace-machine"),
        tracing::instrument(skip(self, f))
    )]
    fn for_each_matching_node<Err>(
        &self,
        eclass: EClass,
        enode: &ENode,
        f: impl FnMut(&ENode) -> Result<(), Err>,
    ) -> Result<(), Err> {
        let classes = &self.eclasses[&eclass];
        if classes.len() < 50 {
            classes
                .iter()
                .map(|c| self.unionfind.get_element(*c))
                .filter(|n| egg::Language::matches(enode, n))
                .try_for_each(f)
        } else {
            let start = classes.partition_point(|c| self.unionfind.get_element(*c).op < enode.op);
            classes[start..]
                .iter()
                .map(|c| self.unionfind.get_element(*c))
                .take_while(|&n| n.op == enode.op)
                .filter(|n| n.args.len() == enode.args.len())
                .try_for_each(f)
        }
    }
}
impl<A: Analysis> HasOperatorStatistics for EGraph<A> {
    fn enodes_of_op(
        &self,
        op: Operator,
    ) -> Option<impl std::iter::Iterator<Item = (&ENode, &EClass)>> {
        self.enodes_by_op.get(op.to_i()).map(|xns| xns.iter())
    }
    fn frequency_of_op(&self, op: Operator) -> usize {
        self.enodes_by_op.get(op.to_i()).map(|xns| xns.len()).unwrap_or(0)
    }
}
impl DefaultDiEGraphView for EGraph<DisequalityEdges> {}
impl DefaultDiEGraph for EGraph<DisequalityEdges> {}

mod tests {
    use super::*;
    test_egraph!(egraph_tests, EGraph<()>);
    test_ematching!(ematching_tests, EGraph<()>);
    test_diegraph!(diegraph_tests, EGraph<DisequalityEdges>);
}
impl<A: Analysis> crate::util::graphviz::GraphViz for EGraph<A> {
    #[cfg_attr(feature = "trace-egraph-basic", tracing::instrument(skip(self), ret))]
    fn graphviz(&self, label: &str) -> String {
        self.unionfind.graphviz(label)
    }
}
