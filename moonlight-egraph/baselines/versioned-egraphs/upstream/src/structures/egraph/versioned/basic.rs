use crate::structures::egraph::ematching::machine::MachineRequirements;
use crate::structures::egraph::extensions::diegraph::testing::test_diegraph;
use crate::structures::egraph::extensions::diegraph::testing::test_versioned_diegraph;
use crate::structures::egraph::extensions::diegraph::DefaultDiEGraph;
use crate::structures::egraph::extensions::diegraph::DefaultDiEGraphView;
use crate::structures::egraph::extensions::diegraph::DiEGraph;
use crate::structures::egraph::extensions::diegraph::DisequalityEdges;
use crate::structures::egraph::testing::test_egraph;
use crate::structures::egraph::versioned::testing::test_versioned_egraph;
use crate::structures::egraph::versioned::VersionedEGraph as _;
use crate::structures::egraph::{Analysis, EClass, EGraph, EGraphView, ENode};
use crate::structures::unionfind::versioned::basic::VersionedUnionFind;
use crate::structures::unionfind::versioned::{Flags, VersionedUnionFind as _};
use crate::structures::unionfind::{UnionFind, UnionFindView};
use crate::structures::versiontree::{Version, VersionTree};
use crate::structures::{Map, Set};
use crate::util::bitset::BitSet;
use crate::util::debug::{proposition, HasInvariant};
use crate::util::graphviz::GraphViz;
use crate::util::id::AsIndex;
use crate::util::memory::HasMemory;
use std::fmt::Debug;
use std::ops::{Deref, DerefMut};

// TODO consider grouping all versioned data in a single struct: Vec<VersionData>
#[derive(Clone)]
pub struct VersionedEGraph<A: Analysis> {
    /// [VersionedUnionFind] of [EClass]es bound to their representative
    /// [ENode]s.
    unionfind: VersionedUnionFind<ENode>,
    /// Map from [ENode]s to their original [EClass], grouped by [Operator].
    /// Used to bind the same [ENode]s to the same [EClass]es.
    /// [ENode]s are grouped by [Operator]s to optimize e-matching.
    enodes_by_op: Vec<Map<ENode, EClass>>,
    /// Map from [Version] to the data of each [EClass] in this [EGraph].
    datas: Vec<A::EClassDatas>,
    // ---- CACHES ---------------------------------------------------------------------------------
    // The following fields do not increase expressiveness, but speed up certain operations.
    // On the other hand, they increase memory consumption and maintainance cost for operations.
    // ---------------------------------------------------------------------------------------------
    /// Map from [Version] to the set of [EClass]es added in that [Version].
    /// Used to avoid reasoning about nodes of other independent versions.
    enodes_by_version: Vec<BitSet>,
    /// Map from [Version] to [EClass]es which use this [EClass] as an argument.
    /// Speeds up rebuilding.
    dependants: Vec<Map<EClass, Set<EClass>>>, // TODO use SparseMap?
    /// Map from [Version] to list of uncanonical [EClass]es involved in unions
    /// performed since last rebuild in that [Version].
    /// Used to defer restoring congruence invariance.   
    worklist: Vec<Vec<EClass>>,
    /// The set of dirty versions, i.e. versions that need to be rebuilt.
    dirty_versions: BitSet,
    /// Map from [Version] to map from [EClass]es to equivalent [EClass]es.
    /// Used to optimize top-down e-matching.
    eclasses_by_version: Map<Version, Map<EClass, Vec<EClass>>>,
    /// The maximum number of co-existing e-matching caches.
    /// Used to balance the amount of cached information for e-matching.
    /// Higher implies better performances, but worse memory.
    /// TODO Make it more flexible. Now, you cannot create a cache on demand:
    /// caches are removed if there is no more space for a new one. In other
    /// words, it requires knowing in advance the number of versions you will
    /// e-match on. Caches are also not created automatically, because read
    /// operations cannot modify the data structure, i.e. immutable borrows.
    max_ematching_caches: usize,
    #[cfg(debug_assertions)] currently_repairing: Map<Version, EClass>,
}
impl<A: Analysis<EClassDatas: HasMemory>> HasMemory for VersionedEGraph<A> {
    fn fields(&self) -> Vec<(&str, &dyn HasMemory)> {
        vec![
            ("unionfind", &self.unionfind),
            ("enodes_by_op", &self.enodes_by_op),
            ("datas", &self.datas),
            ("enodes_by_version", &self.enodes_by_version),
            ("worklist", &self.worklist),
            ("dependants", &self.dependants),
            ("dirty_versions", &self.dirty_versions),
            ("eclasses_by_version", &self.eclasses_by_version),
            ("max_ematching_caches", &self.max_ematching_caches),
        ]
    }
}

impl<A: Analysis> VersionedEGraph<A> {
    pub fn with_ematching_caches(n: usize) -> Self {
        let mut egraph = Self { max_ematching_caches: n, ..Default::default() };
        egraph.project_mut(VersionTree::ROOT_VERSION).init_ematching_cache();
        egraph
    }
}
impl<A: Analysis> Default for VersionedEGraph<A> {
    fn default() -> Self {
        VersionedEGraph {
            unionfind: VersionedUnionFind::default(),
            enodes_by_op: vec![],
            datas: vec![A::EClassDatas::default()],
            enodes_by_version: vec![Default::default()],
            dependants: vec![Default::default()],
            worklist: vec![Vec::new()],
            dirty_versions: Default::default(),
            eclasses_by_version: Default::default(),
            max_ematching_caches: 0,
            #[cfg(debug_assertions)] currently_repairing: Default::default(),
        }
    }
}
impl<A: Analysis> std::fmt::Debug for VersionedEGraph<A>
where
    A::EClassDatas: Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{}", crate::util::graphviz::GraphViz::graphviz(self, "veg"))?;
        f.debug_struct("VersionedEGraph")
            .field("unionfind", &self.unionfind)
            .field("enodes_by_op", &self.enodes_by_op)
            .field("datas", &self.datas)
            .field("enodes_by_version", &self.enodes_by_version)
            .field("dependants", &self.dependants)
            .field("worklist", &self.worklist)
            .field("dirty_versions", &self.dirty_versions)
            .field("eclasses_by_version", &self.eclasses_by_version)
            .field("max_ematching_caches", &self.max_ematching_caches)
            .finish()
    }
}
impl<A: Analysis> super::VersionedEGraph for VersionedEGraph<A> {
    type Analysis = A;
    type Projection<'a>
        = Projection<A, &'a Self>
    where
        Self: 'a;
    type ProjectionMut<'a>
        = Projection<A, &'a mut Self>
    where
        Self: 'a;
    #[inline(always)]
    fn flags(&self) -> &Flags {
        self.unionfind.flags()
    }
    #[inline(always)]
    fn flags_mut(&mut self) -> &mut Flags {
        self.unionfind.flags_mut()
    }
    #[inline(always)]
    fn versioning(&self) -> &VersionTree {
        self.unionfind.versioning()
    }
    #[cfg_attr(feature = "trace-egraph-versioned", tracing::instrument(skip(self), ret))]
    fn branchout(&mut self, v: Version) -> Version {
        assert!(self.versioning().exists(v), "Cannot branchout from removed version: version {v}");
        if !self.flags().lock_superversions || self.versioning().subversions(v).is_empty() {
            self.take(v).rebuild()
        }

        let new_v = self.unionfind.branchout(v);
        let max_v = self.dependants.len();
        let is_new_version = new_v >= max_v;

        // TODO it should be possible to reduce cloning here by traversing
        //      superversions to retrieve versioned information
        if is_new_version {
            self.worklist.push(Default::default());
            self.dependants.push(Default::default());
            self.enodes_by_version.push(self.enodes_by_version[v].clone());
            self.datas.push(self.datas[v].clone());
        } else {
            // here version was removed before: worklist and dependants are already empty
            // self.worklist[new_v] = Default::default();
            // self.dependants[new_v] = Default::default();
            self.enodes_by_version[new_v] = self.enodes_by_version[v].clone();
            self.datas[new_v] = self.datas[v].clone();
        }
        self.take(new_v).inherit_ematching_cache();
        new_v
    }
    #[cfg_attr(feature = "trace-egraph-versioned", tracing::instrument(skip(self), ret))]
    fn twin(&mut self, v: Version) -> Version {
        assert!(self.versioning().exists(v), "Cannot branchout from removed version: version {v}");
        self.take(v).rebuild();

        let new_v = self.unionfind.twin(v);
        let max_v = self.dependants.len();
        let is_new_version = new_v >= max_v;
        if is_new_version {
            self.worklist.push(self.worklist[v].clone());
            self.dependants.push(self.dependants[v].clone());
            self.enodes_by_version.push(self.enodes_by_version[v].clone());
            self.datas.push(self.datas[v].clone());
        } else {
            self.worklist[new_v] = self.worklist[v].clone();
            self.dependants[new_v] = self.dependants[v].clone();
            self.enodes_by_version[new_v] = self.enodes_by_version[v].clone();
            self.datas[new_v] = self.datas[v].clone();
        }
        if self.max_ematching_caches > 0 {
            self.eclasses_by_version.insert(new_v, self.eclasses_by_version[&v].clone());
        }
        new_v
    }
    #[cfg_attr(feature = "trace-egraph-versioned", tracing::instrument(skip(self)))]
    fn project(&self, v: Version) -> Self::Projection<'_> {
        assert!(self.versioning().exists(v), "Cannot project removed version: version {v}");
        Projection { source: self, version: v }
    }
    #[cfg_attr(feature = "trace-egraph-versioned", tracing::instrument(skip(self)))]
    fn project_mut(&mut self, v: Version) -> Self::ProjectionMut<'_> {
        assert!(self.versioning().exists(v), "Cannot project removed version: version {v}");
        assert!(
            !self.flags().lock_superversions || self.versioning().subversions(v).is_empty(),
            "Cannot mutably project locked superversion {v}",
        );
        Projection { source: self, version: v }
    }
    #[cfg_attr(feature = "trace-egraph-versioned", tracing::instrument(skip(self), ret))]
    fn remove_version(&mut self, v: Version) {
        assert!(v != VersionTree::ROOT_VERSION, "Cannot remove the root version");
        assert!(self.versioning().exists(v), "Cannot remove already removed version: version {v}");
        self.clear_version(v);
        self.unionfind.remove_version(v);
    }
    #[cfg_attr(feature = "trace-egraph-versioned", tracing::instrument(skip(self), ret))]
    fn rebase(&mut self, v: Version) {
        todo!("this is unstable due to improper cache invalidation");
        assert!(v != VersionTree::ROOT_VERSION, "Cannot rebase the root version");
        assert!(self.versioning().exists(v), "Cannot rebase removed version: version {v}");
        self.take(v).rebuild();
        let mut dependants: Map<EClass, Set<EClass>> = Default::default();
        let path = self.versioning().path(v);
        for ancestor in path {
            for (xc, xdeps) in self.dependants[ancestor].iter() {
                dependants.entry(*xc).or_default().extend(xdeps.iter());
            }
        }
        self.dependants[v] = dependants;
        self.unionfind.rebase(v);
    }
    fn get_parent_and_version(&self, x: EClass, v: Version) -> (EClass, Version) {
        self.unionfind.get_parent_and_version(x, v)
    }
}
impl<A: Analysis> VersionedEGraph<A> {
    fn clear_version(&mut self, v: Version) {
        self.for_each_subversion(v, |this, sv| this.clear_version(sv));
        self.worklist[v].clear();
        self.dependants[v].clear();
        self.enodes_by_version[v].clear();
        self.eclasses_by_version.remove(&v);
        self.datas[v] = A::EClassDatas::default();
    }
}
impl<A: Analysis> EGraphView for VersionedEGraph<A> {
    type Analysis = A;
    #[inline]
    fn datas(&self) -> &A::EClassDatas {
        &self.datas[VersionTree::ROOT_VERSION]
    }
    #[inline]
    fn is_clean(&self) -> bool {
        !self.dirty_versions.contains(VersionTree::ROOT_VERSION)
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
        self.unionfind.focus(VersionTree::ROOT_VERSION).get_parent(x)
    }
    #[inline]
    fn find(&self, x: EClass) -> EClass {
        self.unionfind.focus(VersionTree::ROOT_VERSION).find(x)
    }
}
impl<A: Analysis> EGraph for VersionedEGraph<A> {
    #[inline]
    fn datas_mut(&mut self) -> &mut A::EClassDatas {
        &mut self.datas[VersionTree::ROOT_VERSION]
    }
    #[inline]
    fn add_canonical(&mut self, xn: ENode) -> EClass {
        self.take(VersionTree::ROOT_VERSION).add_canonical(xn)
    }
    #[inline]
    fn find_mut(&mut self, x: EClass) -> EClass {
        self.take(VersionTree::ROOT_VERSION).find_mut(x)
    }
    #[inline]
    fn union_canonical(&mut self, xc: EClass, yc: EClass) -> EClass {
        self.take(VersionTree::ROOT_VERSION).union_canonical(xc, yc)
    }
    #[inline]
    fn rebuild(&mut self) {
        self.take(VersionTree::ROOT_VERSION).rebuild()
    }
}
impl DefaultDiEGraphView for VersionedEGraph<DisequalityEdges> {}
impl DefaultDiEGraph for VersionedEGraph<DisequalityEdges> {}

/// A [Projection] of the `source` (i.e. versioned data) to a given [Version].
// TODO why does this projection requires the additional type parameter A, which
//      the unionfind does not?
pub struct Projection<A: Analysis, S: Deref<Target = VersionedEGraph<A>>> {
    source: S,
    version: Version,
}
impl<A, S> Debug for Projection<A, S>
where
    A: Analysis,
    S: Deref<Target = VersionedEGraph<A>>,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut builder = f.debug_struct("VersionedEGraphProjection");
        builder
            .field("unionfind", &self.source.unionfind.focus(self.version))
            .field("enodes", &self.source.enodes_by_op.iter().flat_map(|xns| xns.iter().filter(|(_, xc)| self.source.enodes_by_version[self.version].contains(xc.to_i()))).collect::<Map<_,_>>())
            //.field("datas", "...")
            //.field("enodes_set", &self.source.enodes_by_version[self.version])
            .field("dependants", &self.source.dependants[self.version])
            .field("worklist", &self.source.worklist[self.version])
            .field("dirty", &self.source.dirty_versions.contains(self.version))
            .field("ematching_cache", &self.source.eclasses_by_version.get(&self.version))
            .field("max_ematching_caches", &self.source.max_ematching_caches);
        #[cfg(debug_assertions)] builder.field("currently_repairing", &self.source.currently_repairing.get(&self.version));
        builder.finish()
    }
}
impl<A, S> HasInvariant for Projection<A, S>
where
    A: Analysis,
    S: Deref<Target = VersionedEGraph<A>>,
{
    #[inline]
    #[rustfmt::skip]
    fn condition(&self) {
        #[cfg(debug_assertions)] {
        #[cfg(feature = "trace")]
        tracing::info!("checking invariant at version {}", self.version);

        // Sanity checks
        let data_exist = self.source.datas.get(self.version).is_some();
        let worklist_exist = self.source.worklist.get(self.version).is_some();
        let dependants_exist = self.source.dependants.get(self.version).is_some();
        let enodes_by_version_exist = self.source.enodes_by_version.get(self.version).is_some();
        let ematching_cache_doesnt_exist = !self.source.eclasses_by_version.contains_key(&self.version);
        let ematching_disabled = self.source.max_ematching_caches == 0;
        assert!(proposition!(data_exist), 
        "invariant: maintains data analysis | {self:#?}");
        assert!(proposition!(worklist_exist), 
        "invariant: maintains worklist for deferred rebuilding | {self:#?}");
        assert!(proposition!(dependants_exist), 
        "invariant: maintains dependants for efficient rebuilding | {self:#?}");
        assert!(proposition!(enodes_by_version_exist), 
        "invariant: maintains own e-nodes to avoid reasoning about e-nodes in independent versions | {self:#?}");
        assert!(proposition!(ematching_disabled => ematching_cache_doesnt_exist), 
        "invariant: does not maintain an ematching cache, if the cache is disabled | {self:#?}");

        // Canonicalization Invariants
        let canonicalization = false;
        'enodes: for xci in self.source.enodes_by_version[self.version].iter() {
            let xc = EClass::from(xci);
            let xn = self.source.unionfind.get_element(xc);
            for &arg in xn.args.iter() {
                let has_to_be_repaired = self.source.worklist[self.version].iter().any(|&w| self.equal(arg, w));
                if has_to_be_repaired { continue 'enodes; }
                let is_repairing = self.source.currently_repairing.get(&self.version).is_some_and(|&r| self.equal(arg, r));
                if is_repairing { continue 'enodes; }
            }
            let xnc = self.canonicalize(xn.clone());
            match self.get_class(&xnc) {
                None => {
                    let ancestors = format!("{} {:?}", xn.op, xn.args.iter().map(|&arg| self.source.unionfind.focus(self.version).get_ancestors(arg).collect::<Vec<_>>()).collect::<Vec<_>>());
                    let currently_repairing = self.source.currently_repairing.get(&self.version);
                    let worklist = &self.source.worklist[self.version];
                    let ce = format!("e-node {xn:?} in e-class {xc:?}: missing canonicalization {xnc:?} (ancestors {ancestors}; currently repairing {currently_repairing:?}; worklist {worklist:?})");
                    assert!(proposition!(canonicalization),
                    "invariant: for all e-nodes, for all their arguments, either: (a) it has to be repaired; (b) it's being repaired; (c) its canonicalization exists in the e-graph.\ncounterexample: {ce}\nstate: {self:#?}");
                },
                Some(&xcc) if !self.equal(xc, xcc) => {
                    let ancestors = format!("{} {:?}", xn.op, xn.args.iter().map(|&arg| self.source.unionfind.focus(self.version).get_ancestors(arg).collect::<Vec<_>>()).collect::<Vec<_>>());
                    let currently_repairing = self.source.currently_repairing.get(&self.version);
                    let worklist = &self.source.worklist[self.version];
                    let ce = format!("e-node {xn:?} in e-class {xc:?}: unequal to its canonicalization {xnc:?} in e-class {xcc:?} (ancestors {ancestors}; currently repairing {currently_repairing:?}; worklist {worklist:?})");
                    assert!(proposition!(canonicalization),
                    "invariant: for all e-nodes, for all their arguments, either: (a) it has to be repaired; (b) it's being repaired; (c) it's equal to its canonicalization.\ncounterexample: {ce}\nstate: {self:#?}");
                }
                _ => {}
            }
        }
        let dependencies_canonicalization = false;
        for &dependency in self.source.dependants[self.version].keys() {
            let has_to_be_repaired = self.source.worklist[self.version].contains(&dependency);
            if has_to_be_repaired { continue; }
            let is_repairing = self.source.currently_repairing.get(&self.version).is_some_and(|r| dependency == *r);
            if is_repairing { continue; }
            let is_canonical = dependency == self.find(dependency);
            if !is_canonical {
                let ancestors = format!("{:?}", self.source.unionfind.focus(self.version).get_ancestors(dependency).collect::<Vec<_>>());
                let currently_repairing = self.source.currently_repairing.get(&self.version);
                let worklist = &self.source.worklist[self.version];
                let canonical = self.find(dependency);
                let ce = format!("key {dependency:?} in dependants has canonicalization {canonical:?} (ancestors {ancestors}; currently repairing {currently_repairing:?}; worklist {worklist:?})");
                assert!(proposition!(dependencies_canonicalization),
                "invariant: for all dependencies, either (a): it has to be repaired; (b) it's being repaired; (c) it's a root.\ncounterexample: {ce}\nstate: {self:#?}");
            }
        }

        // Ematching Cache Invariants
        let rebuilt = self.is_clean();
        if rebuilt {
            let ematching_cache_correct = false;
            let ematching_cache_canonicalized = false;
            let ematching_cache_sorted = false;
            let ematching_cache_deduped = false;
            let mut seen: Set<EClass> = Default::default();
            if let Some(cache) = self.source.eclasses_by_version.get(&self.version) {
                for (&xc, xns) in cache.iter() {
                    let mut prev: Option<&ENode> = None;
                    for &xnc in xns.iter() {
                        let xn = self.source.unionfind.get_element(xnc);
                        if self.find(xnc) != xc { 
                            let ce = format!("key {xc:?}: e-node {xn:?} from e-class {xnc:?} should not be a member");
                            assert!(proposition!(ematching_cache_correct), 
                            "invariant: after rebuilding, ematching cache maps e-classes to their members.\ncounterexample: {ce:?}\nstate: {self:#?}");
                        }
                        if prev.is_some_and(|pn| pn.op > xn.op) { 
                            let ce = format!("key {xc:?}: e-node {xn:?} from e-class {xnc:?} should precede {prev:?}");
                            assert!(proposition!(ematching_cache_sorted), 
                            "invariant: after rebuilding, ematching cache has e-nodes per e-class sorted by operator.\ncounterexample: {ce:?}\nstate: {self:#?}");
                        } 
                        for &arg in xn.args.iter() {
                            let is_canonical = arg == self.find(arg);
                            if !is_canonical {
                                let argc = self.find(arg);
                                let ancestors = self.source.unionfind.focus(self.version).get_ancestors(arg).collect::<Vec<_>>();
                                let ce = format!("key {xc:?}: e-node {xn:?} from e-class {xnc:?} is not canonicalized: argument {arg:?} has canonicalization {argc:?} (ancestors: {ancestors:?})");
                                assert!(proposition!(ematching_cache_canonicalized), 
                                "invariant: after rebuilding, ematching cache has only canonicalized e-nodes.\ncounterexample: {ce:?}\nstate: {self:#?}");
                            }
                        }
                        if !seen.insert(xnc) {
                            let ce = format!("key {xc:?}: e-node {xn:?} from e-class {xnc:?} appears multiple times");
                            assert!(proposition!(ematching_cache_deduped), 
                            "invariant: after rebuilding, ematching cache has e-nodes members appear only once per e-class.\ncounterexample: {ce:?}\nstate: {self:#?}");
                        }
                        prev = Some(xn);
                    }
                    seen.clear();
                }
            }
        }

        #[cfg(feature = "trace")]
        tracing::info!("holds.");
    }
    }
}
impl<A, S> EGraphView for Projection<A, S>
where
    A: Analysis,
    S: Deref<Target = VersionedEGraph<A>>,
{
    type Analysis = A;
    #[inline]
    fn datas(&self) -> &A::EClassDatas {
        &self.source.datas[self.version]
    }
    #[inline]
    fn is_clean(&self) -> bool {
        !self.source.dirty_versions.contains(self.version)
    }
    fn universe(&self) -> impl std::iter::Iterator<Item = EClass> {
        self.source
            .unionfind
            .universe()
            .filter(|xc| self.source.enodes_by_version[self.version].contains(xc.to_i()))
    }
    #[inline]
    fn get_enode(&self, x: EClass) -> &ENode {
        self.source.unionfind.get_element(x)
    }
    fn get_class(&self, xn: &ENode) -> Option<&EClass> {
        self.source
            .enodes_by_op
            .get(xn.op.to_i())
            .and_then(|xns| xns.get(xn))
            .filter(|&xc| self.source.enodes_by_version[self.version].contains(xc.to_i()))
    }
    #[inline]
    fn get_parent(&self, x: EClass) -> EClass {
        self.source.unionfind.focus(self.version).get_parent(x)
    }
    #[inline]
    fn find(&self, x: EClass) -> EClass {
        self.source.unionfind.focus(self.version).find(x)
    }
}
impl<A, S> Projection<A, S>
where
    A: Analysis,
    S: DerefMut<Target = VersionedEGraph<A>>,
{
    #[inline(always)]
    fn propagate<O, F: Fn(Projection<A, &mut VersionedEGraph<A>>) -> O>(&mut self, callback: F) {
        if self.source.flags().propagate {
            self.source.for_each_subversion(self.version, |this, sv| callback(this.take(sv)));
        }
    }
    #[inline(always)]
    fn restore_congruence<I: std::iter::IntoIterator<Item = EClass>>(&mut self, dependants: I) {
        for stale in dependants {
            let stale_n = self.get_enode(stale);
            let fresh_n = self.canonicalize_mut(stale_n.clone());
            let fresh = self.add_canonical(fresh_n);
            self.source.flags_mut().propagate = false;
            self.union(fresh, stale);
            self.source.flags_mut().propagate = true;
        }
    }
    fn add_to_all_descendants(&mut self, xc: EClass) {
        self.propagate(|mut sv| {
            if sv.source.enodes_by_version[sv.version].add(xc.to_i()) {
                sv.add_to_all_descendants(xc);
            }
        });
    }
}
impl<A: Analysis, S: DerefMut<Target = VersionedEGraph<A>>> EGraph for Projection<A, S> {
    #[inline]
    fn datas_mut(&mut self) -> &mut A::EClassDatas {
        &mut self.source.datas[self.version]
    }
    #[cfg_attr(feature = "trace-egraph-versioned", tracing::instrument(skip(self), ret))]
    fn add_canonical(&mut self, xn: ENode) -> EClass {
        self.debug_invariant();
        let xc: EClass = match self.source.get_class(&xn) {
            Some(&x) => self.find_mut(x),
            None => {
                let x = self.source.unionfind.take(self.version).new_element(xn.clone());
                let op_i = xn.op.to_i();
                if op_i >= self.source.enodes_by_op.len() {
                    self.source.enodes_by_op.resize_with(op_i + 1, Default::default);
                }
                self.source.enodes_by_op[op_i].insert(xn.clone(), x);
                x
            },
        };
        if !self.source.enodes_by_version[self.version].contains(xc.to_i()) {
            self.source.enodes_by_version[self.version].add(xc.to_i());
            for i in 0..self.get_enode(xc).args.len() {
                let argi = self.get_enode(xc).args[i];
                self.source.dependants[self.version].entry(argi).or_default().insert(xc);
            }
            self.source
                .eclasses_by_version
                .get_mut(&self.version)
                .map(|xcs| xcs.insert(xc, vec![xc]));
            A::make_data(self, &xc);
        }
        // NOTE propagation uses `add_canonical` because the user should be able
        // to use the class of `xn` in all subversions. However, `xn` may have
        // different canonicalizations in the subversions, so we restore
        // congruence for it. Failing Test:
        // [crate::structures::egraph::versioned::testing::version_inheritance_of_uncanonical_enodes]
        self.propagate(|mut sv| {
            let xcc = sv.add(xn.clone());
            sv.union(xc, xcc);
        });
        // TODO optimize this: there shouldn't be a need to traverse the version
        //      tree twice (once in propagate, once here)
        self.add_to_all_descendants(xc);
        self.debug_invariant();
        xc
    }
    #[cfg_attr(feature = "trace-egraph-versioned", tracing::instrument(skip(self), ret))]
    fn find_mut(&mut self, x: EClass) -> EClass {
        self.source.unionfind.take(self.version).find_mut(x)
    }
    #[cfg_attr(feature = "trace-egraph-versioned", tracing::instrument(skip(self), ret))]
    fn union_canonical(&mut self, xc: EClass, yc: EClass) -> EClass {
        self.debug_invariant();
        
        // NOTE propagation uses `union` because `xc` and `yc` are assumed to be
        // canonical in this version, but may not be canonical in subversions
        self.propagate(|mut sv| sv.union(xc, yc));

        // NOTE order by increasing id (age), which is a partial ordering on
        // depth (?). This avoids infinite loops during rebuilding.
        // Failing Test: [crate::egraph::testing::recursive_eclasses]
        // TODO consider ordering by self.dependants.len() to reduce repairs.
        let mut parent = xc;
        let mut child = yc;
        if parent > child {
            std::mem::swap(&mut parent, &mut child);
        }
        self.source.unionfind.add_edge(parent, self.version, child);

        self.source.worklist[self.version].push(child);
        self.source.dirty_versions.add(self.version);
        if let Some(eclasses) = self.source.eclasses_by_version.get_mut(&self.version) {
            let mut child_classes = eclasses.remove(&child);
            let parent_classes = eclasses.entry(parent).or_insert_with(|| vec![parent]);
            match child_classes {
                None => parent_classes.push(child),
                Some(ref mut child_classes) => parent_classes.append(child_classes),
            }
        }
        A::merge_data(self, &parent, &child);

        self.debug_invariant();
        parent
    }
    #[cfg_attr(feature = "trace-egraph-versioned", tracing::instrument(skip(self), ret))]
    fn rebuild(&mut self) {
        self.debug_invariant();

        // NOTE rebuilding does not need to be propagated to:
        // - superversions: (i) before a version is created, its superversion is
        //   already rebuilt; (ii) after a version is created, all unions in the
        //   superversion are propagated to the subversion, updating its
        //   worklist the same way
        // - subversions: congruence in this version is independent from
        //   congruence in the subversions
        // TODO for performances, consider not updating the worklist when
        // propagating unions to the subversions, and instead rebuilding all
        // superversions when rebuilding a version (probably not easy, since
        // propagating unions could add different eclasses to the worklist in
        // different subversions)

        // NOTE if the worklist is empty, there is nothing to do: this
        // short-circuit is needed to avoid re-optimizing the cache
        if self.source.worklist[self.version].is_empty() {
            return;
        }

        // NOTE While not necessary, propagating rebuilding to subversions
        // allows us to remove the dependants of a repaired EClass from the
        // current version, speeding up future rebuilds. Notably, congruence may
        // not be restored for subversions at the end of rebuild. Failing test:
        // [crate::structures::egraph::versioned::testing::version_inheritance_of_rebuilding]
        self.propagate(|mut sv| sv.rebuild());

        // NOTE every EClass only ever appears in the worklist once for a
        // version, because it can only ever be the child in a union once

        // Restore congruence in this version...
        while let Some(child) = self.source.worklist[self.version].pop() {
            self.repair(child);
        }
        #[cfg(debug_assertions)] self.source.currently_repairing.remove(&self.version);
        self.rebuild_ematching_cache();
        self.source.dirty_versions.remove(self.version);
        self.debug_invariant();
    }
}
impl<A, S> Projection<A, S>
where
    A: Analysis,
    S: DerefMut<Target = VersionedEGraph<A>>,
{
    #[cfg_attr(feature = "trace-egraph-versioned", tracing::instrument(skip(self), ret))]
    fn repair(&mut self, dependency: EClass) {
        #[cfg(debug_assertions)] self.source.currently_repairing.insert(self.version, dependency);

        // ... for the dependants stored in this version
        if let Some(dependants) = self.source.dependants[self.version].remove(&dependency) {
            self.restore_congruence(dependants);
        }
        // ... for the dependants stored in the superversions
        let mut checkout = self.version;
        while checkout != VersionTree::ROOT_VERSION {
            checkout = self.source.versioning().superversion(checkout);

            // NOTE the dependants in the superversions cannot be removed,
            // as they may be needed for their own rebuilding
            if let Some(dependants) = self.source.dependants[checkout].get(&dependency).cloned() {
                self.restore_congruence(dependants);
            }
        }
    }
}
impl<A, S> Projection<A, S>
where
    A: Analysis,
    S: DerefMut<Target = VersionedEGraph<A>>,
{
    #[cfg_attr(feature = "trace-egraph-versioned", tracing::instrument(skip(self), ret))]
    #[inline(always)]
    fn has_cache(&self) -> bool {
        self.source.eclasses_by_version.contains_key(&self.version)
    }
    #[cfg_attr(feature = "trace-egraph-versioned", tracing::instrument(skip(self), ret))]
    fn new_ematching_cache(&self) -> Map<EClass, Vec<EClass>> {
        let mut cache: Map<EClass, Vec<EClass>> = Default::default();
        // NOTE already sorted by operator (the index of the vector)
        let mut seen: Set<EClass> = Default::default(); // TODO is there a better way to deduplicate?
        for ops in self.source.enodes_by_op.iter() {
            for (xn, xc) in ops {
                if self.source.enodes_by_version[self.version].contains((*xc).into()) {
                    let xn = self.canonicalize(xn.clone());
                    let xc = self.get_class(&xn).unwrap();
                    if !seen.insert(*xc) {
                        continue;
                    }
                    let xcc = self.find(*xc);
                    cache.entry(xcc).or_default().push(*xc);
                }
            }
            seen.clear();
        }
        cache
    }
    #[cfg_attr(feature = "trace-egraph-versioned", tracing::instrument(skip(self), ret))]
    fn reserve_new_cache(&mut self) -> bool {
        self.debug_invariant();
        if self.has_cache() {
            return true;
        }
        let success = if self.source.max_ematching_caches > 0 {
            if self.source.eclasses_by_version.len() >= self.source.max_ematching_caches {
                // Random Version
                // let v = *self.source.eclasses_by_version.iter().next().unwrap().0;
                // self.source.eclasses_by_version.remove(&v);

                // Latest Version
                let v = *self.source.eclasses_by_version.keys().max().unwrap();
                self.source.eclasses_by_version.remove(&v);
            }
            true
        } else {
            false
        };
        self.debug_invariant();
        success
    }
    #[cfg_attr(feature = "trace-egraph-versioned", tracing::instrument(skip(self), ret))]
    fn init_ematching_cache(&mut self) {
        self.debug_invariant();
        if self.reserve_new_cache() {
            let cache = self.new_ematching_cache();
            self.source.eclasses_by_version.insert(self.version, cache);
        }
        self.debug_invariant();
    }
    #[cfg_attr(feature = "trace-egraph-versioned", tracing::instrument(skip(self), ret))]
    fn inherit_ematching_cache(&mut self) {
        self.debug_invariant();
        if self.reserve_new_cache() {
            let parent = self.source.versioning().superversion(self.version);
            let cache = match self.source.eclasses_by_version.get(&parent) {
                Some(inherit) => inherit.clone(),
                None => self.new_ematching_cache(),
            };
            self.source.eclasses_by_version.insert(self.version, cache);
        }
        self.debug_invariant();
    }
    #[cfg_attr(feature = "trace-egraph-versioned", tracing::instrument(skip(self), ret))]
    fn rebuild_ematching_cache(&mut self) {
        self.debug_invariant();
        // Restore the cache of equivalent canonicalized enodes, sorted by operator
        if let Some(mut eclasses) = self.source.eclasses_by_version.remove(&self.version) {
            let mut seen = Set::default(); // TODO is there a better way to deduplicate?
            for (&_xc, xns) in eclasses.iter_mut() {
                for xnc in xns.iter_mut() {
                    self.rebuild_ematching_entry(xnc);
                }
                xns.sort_unstable_by_key(|c| self.source.unionfind.get_element(*c).op);
                xns.retain(|c| seen.insert(*c));
                seen.clear();
            }
            self.source.eclasses_by_version.insert(self.version, eclasses);
        }
        self.debug_invariant();
    }
    #[cfg_attr(feature = "trace-egraph-versioned", tracing::instrument(skip(self), ret))]
    fn rebuild_ematching_entry(&mut self, xnc: &mut EClass) {
        let xn = self.source.unionfind.get_element(*xnc);
        let xn0 = self.canonicalize(xn.clone());
        *xnc = *self.get_class(&xn0).unwrap();
    }
}

impl<A, S> MachineRequirements for Projection<A, S>
where
    A: Analysis,
    S: Deref<Target = VersionedEGraph<A>>,
{
    #[cfg_attr(
        all(feature = "trace-egraph-versioned", feature = "trace-machine"),
        tracing::instrument(skip(self), ret)
    )]
    fn canonicals(&self) -> impl std::iter::Iterator<Item = &egg::Id> {
        self.source
            .eclasses_by_version
            .get(&self.version)
            .expect("max ematching caches to be greater than 0")
            .keys()
    }
    #[cfg_attr(
        all(feature = "trace-egraph-versioned", feature = "trace-machine"),
        tracing::instrument(skip(self, f))
    )]
    fn for_each_matching_node<Err>(
        &self,
        eclass: EClass,
        enode: &ENode,
        mut f: impl FnMut(&ENode) -> Result<(), Err>,
    ) -> Result<(), Err> {
        let classes = &self.source.eclasses_by_version[&self.version][&eclass];
        if classes.len() < 50 {
            classes
                .iter()
                .map(|c| self.source.unionfind.get_element(*c))
                .filter(|n| egg::Language::matches(enode, n))
                .try_for_each(f)
        } else {
            let start = classes.partition_point(|c| self.source.unionfind.get_element(*c).op < enode.op);
            classes[start..]
                .iter()
                .map(|c| self.source.unionfind.get_element(*c))
                .take_while(|&n| n.op == enode.op)
                .filter(|n| n.args.len() == enode.args.len())
                .try_for_each(&mut f)
        }
    }
}
impl<S> DefaultDiEGraphView for Projection<DisequalityEdges, S> where
    S: Deref<Target = VersionedEGraph<DisequalityEdges>>
{
}
impl<S> DiEGraph for Projection<DisequalityEdges, S>
where
    S: DerefMut<Target = VersionedEGraph<DisequalityEdges>>,
{
    #[cfg_attr(
        all(feature = "trace-egraph-versioned", feature = "trace-disequalities"),
        tracing::instrument(skip(self), ret)
    )]
    fn disunion_canonical(&mut self, xc: EClass, yc: EClass) {
        self.propagate(|mut sv| sv.disunion(xc, yc));
        self.datas_mut().entry(xc).or_default().insert(yc);
        if xc != yc {
            self.datas_mut().entry(yc).or_default().insert(xc);
        }
    }
}

mod tests {
    use super::*;
    test_egraph!(egraph_tests, VersionedEGraph<()>);
    test_diegraph!(diegraph_tests, VersionedEGraph<DisequalityEdges>);
    test_versioned_egraph!(versioned_egraph_tests, VersionedEGraph<()>);
    test_versioned_diegraph!(versioned_diegraph_tests, VersionedEGraph<DisequalityEdges>);
}
impl<A: Analysis> GraphViz for VersionedEGraph<A> {
    #[cfg_attr(feature = "trace-egraph-versioned", tracing::instrument(skip(self), ret))]
    fn graphviz(&self, label: &str) -> String {
        self.unionfind.graphviz(label)
    }
}
