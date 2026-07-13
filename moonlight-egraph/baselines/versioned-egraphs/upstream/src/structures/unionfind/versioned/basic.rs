use crate::structures::unionfind::versioned::{Flags, VersionedUnionFind as _};
use crate::structures::unionfind::{UnionFind, UnionFindView};
use crate::structures::versiontree::{Version, VersionTree};
use crate::structures::Map;
use crate::util::graphviz::GraphViz;
use crate::util::id::{AsIndex, Id, IdRange};
use crate::util::memory::HasMemory;
use std::ops::{Deref, DerefMut};

#[derive(Debug, Clone)]
pub struct VersionedUnionFind<A> {
    /// The [VersionTree] managing [Version]s for this [VersionedUnionFind].
    version_tree: VersionTree,
    /// A map from [Id] to elements in the union-find.
    universe: Vec<A>,
    /// A map from [Version] to edges from elements to their parent.
    /// In this [VersionedUnionFind], parents are indexed by [Version]:
    /// + Operations on [Version]s are faster (e.g. removal, rebase...).
    /// - Indexing edges by versions is slower when the number of versions is
    ///   low. This is because `insert` now searches over elements instead of
    ///   over versions, contrary to before.
    parents: Vec<Map<Id, Id>>,
    /// Configuration flags for versioning in this [VersionedUnionFind].
    flags: Flags,
}
impl<A: HasMemory> HasMemory for VersionedUnionFind<A> {
    fn fields(&self) -> Vec<(&str, &dyn HasMemory)> {
        vec![
            ("version_tree", &self.version_tree),
            ("universe", &self.universe),
            ("parents", &self.parents),
            ("flags", &self.flags),
        ]
    }
}
impl<A> Default for VersionedUnionFind<A> {
    fn default() -> Self {
        VersionedUnionFind {
            version_tree: VersionTree::default(),
            universe: Vec::new(),
            parents: vec![Default::default()],
            flags: Default::default(),
        }
    }
}
impl<A> super::VersionedUnionFind for VersionedUnionFind<A> {
    type ElementData = A;
    type Projection<'a>
        = Projection<&'a Self>
    where
        Self: 'a;
    type ProjectionMut<'a>
        = Projection<&'a mut Self>
    where
        Self: 'a;
    #[inline]
    fn flags(&self) -> &Flags {
        &self.flags
    }
    #[inline]
    fn flags_mut(&mut self) -> &mut Flags {
        &mut self.flags
    }
    #[inline]
    fn versioning(&self) -> &VersionTree {
        &self.version_tree
    }
    #[cfg_attr(feature = "trace-unionfind-versioned", tracing::instrument(skip(self), ret))]
    fn branchout(&mut self, v: Version) -> Version {
        assert!(self.version_tree.exists(v), "Cannot branchout from removed version: version {v}",);
        let new_v = self.version_tree.branchout(v);
        let max_v = self.parents.len();
        let is_new_version = new_v >= max_v;
        if is_new_version {
            self.parents.push(Default::default());
        } /*else {
            // here version was removed before: parents are already empty
          } */
        new_v
    }
    #[cfg_attr(feature = "trace-unionfind-versioned", tracing::instrument(skip(self), ret))]
    fn twin(&mut self, v: Version) -> Version {
        assert!(v != VersionTree::ROOT_VERSION, "Cannot create a twin of the root version");
        assert!(
            self.version_tree.exists(v),
            "Cannot create a twin of a removed version: version {v}",
        );
        let parent = self.version_tree.superversion(v);
        let twin = self.version_tree.branchout(parent);
        if twin >= self.parents.len() {
            self.parents.push(self.parents[v].clone());
        } else {
            self.parents[twin] = self.parents[v].clone();
        }
        twin
    }
    #[cfg_attr(feature = "trace-unionfind-versioned", tracing::instrument(skip(self)))]
    fn project(&self, v: Version) -> Self::Projection<'_> {
        assert!(self.version_tree.exists(v), "Cannot project removed version: version {v}",);
        Projection { source: self, version: v }
    }
    #[cfg_attr(feature = "trace-unionfind-versioned", tracing::instrument(skip(self)))]
    fn project_mut(&mut self, v: Version) -> Self::ProjectionMut<'_> {
        assert!(self.version_tree.exists(v), "Cannot project removed version: version {v}",);
        assert!(
            !self.flags.lock_superversions || self.version_tree.subversions(v).is_empty(),
            "Cannot mutably project locked superversion {v} in unionfind",
        );
        Projection { source: self, version: v }
    }
    #[cfg_attr(feature = "trace-unionfind-versioned", tracing::instrument(skip(self), ret))]
    fn remove_version(&mut self, v: Version) {
        assert!(v != VersionTree::ROOT_VERSION, "Cannot remove the root version");
        assert!(self.version_tree.exists(v), "Cannot remove already removed version: version {v}",);
        self.clear_version(v);
        self.version_tree.remove(v);
    }
    #[cfg_attr(feature = "trace-unionfind-versioned", tracing::instrument(skip(self), ret))]
    fn rebase(&mut self, v: Version) {
        assert!(v != VersionTree::ROOT_VERSION, "Cannot rebase the root version");
        assert!(self.version_tree.exists(v), "Cannot rebase removed version: version {v}",);
        let mut parents: Map<Id, Id> = Default::default();
        let mut path = self.version_tree.path(v);
        while let Some(ancestor) = path.pop() {
            parents.extend(self.parents[ancestor].clone());
        }
        self.parents[v] = parents;
        self.version_tree.rebase(v, VersionTree::ROOT_VERSION);
    }
    fn get_parent_and_version(&self, x: Id, v: Version) -> (Id, Version) {
        let mut checkout = v;
        let parent = loop {
            match self.parents[checkout].get(&x) {
                Some(&parent) => break parent,
                None => checkout = self.version_tree.superversion(checkout),
            }
        };
        (parent, checkout)
    }
}
impl<A> VersionedUnionFind<A> {
    #[inline]
    fn clear_version(&mut self, v: Version) {
        self.for_each_subversion(v, |this, sv| this.clear_version(sv));
        self.parents[v].clear();
    }
}
impl<A> VersionedUnionFind<A> {
    /// #### Description
    /// Adds an edge from `child` to `parent` at the given `version`:
    ///
    /// `parent <- version -- child`
    ///
    /// #### Note
    /// This is a low-level operation and should be used with caution: the
    /// caller is responsible for ensuring the correctness of the union-find
    /// structure. Use at your own discretion.
    #[inline]
    pub fn add_edge(&mut self, parent: Id, version: Version, child: Id) {
        self.parents[version].insert(child, parent);
    }
}
impl<A> UnionFindView for VersionedUnionFind<A> {
    type ElementData = A;
    #[inline]
    fn universe(&self) -> impl std::iter::Iterator<Item = Id> {
        IdRange::new(0, self.universe.len())
    }
    #[inline]
    fn get_element(&self, x: Id) -> &Self::ElementData {
        &self.universe[x.to_i()]
    }
    #[inline]
    fn get_parent(&self, x: Id) -> Id {
        self.focus(VersionTree::ROOT_VERSION).get_parent(x)
    }
}
impl<A> UnionFind for VersionedUnionFind<A> {
    #[inline]
    fn new_element(&mut self, element: Self::ElementData) -> Id {
        self.take(VersionTree::ROOT_VERSION).new_element(element)
    }
    #[inline]
    fn find_mut(&mut self, x: Id) -> Id {
        self.take(VersionTree::ROOT_VERSION).find_mut(x)
    }
    #[inline]
    fn union_canonical(&mut self, xc: Id, yc: Id) -> Id {
        self.take(VersionTree::ROOT_VERSION).union_canonical(xc, yc)
    }
}
/// A [Projection] of the `source` (i.e. versioned data) to a given `version`,
pub struct Projection<S: std::ops::Deref> {
    source: S,
    version: Version,
}
impl<A: std::fmt::Debug, S> std::fmt::Debug for Projection<S>
where
    S: Deref<Target = VersionedUnionFind<A>>,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("UnionFindProjection")
            .field("version", &self.version)
            .field("ancestors", &self.source.versioning().path(self.version))
            .field("universe", &self.source.universe)
            .field("parents", &self.source.parents[self.version])
            .field("flags", &self.source.flags)
            .finish()
    }
}
impl<A, S: Deref<Target = VersionedUnionFind<A>>> Projection<S> {
    #[inline]
    fn get_parent_and_version(&self, x: Id) -> (Id, Version) {
        self.source.get_parent_and_version(x, self.version)
    }
}
impl<A, S: Deref<Target = VersionedUnionFind<A>>> UnionFindView for Projection<S> {
    type ElementData = A;
    #[inline]
    fn universe(&self) -> impl std::iter::Iterator<Item = Id> {
        IdRange::new(0, self.source.universe.len())
    }
    #[inline]
    fn get_element(&self, x: Id) -> &Self::ElementData {
        &self.source.universe[x.to_i()]
    }
    #[inline]
    fn get_parent(&self, x: Id) -> Id {
        self.get_parent_and_version(x).0
    }
    // NOTE you can have path compression also in immutable find (through unsafe)
    // fn find(&self, x: Id) -> Id {
    //     fn force_mut<T>(r: &T) -> &mut T {
    //         #![allow(clippy::mut_from_ref)]
    //         #![allow(invalid_reference_casting)]
    //         unsafe { &mut *(r as *const T as *mut T) }
    //     }
    //     let mut current = x;
    //     let (mut next, mut checkout) = self.get_parent_and_version(current);
    //     while current != next {
    //         let previous = current;
    //         let previous_checkout = checkout;
    //         current = next;
    //         (next, checkout) = self.get_parent_and_version(current);
    //         // NOTE path compression
    //         // add `previous -- v -> next` at queried version `v` for patterns
    //         // (1) previous -- ? -> current -- ? -> next
    //         // (2) previous -- !v -> {current, next} -- ? <-
    //         if current != next || previous_checkout != self.version {
    //             force_mut(&self.source.parents[self.version]).insert(previous, next);
    //         }
    //     }
    //     // NOTE keep track of versioned canonicals
    //     // add `next -- v <-` at the queried version `v` for patterns
    //     // (1) {current, next} -- !v <-
    //     // Failing Test: [crate::unionfind::versioned::testing::version_inheritance_loops]
    //     if !self.source.flags.lock_superversions && checkout != self.version {
    //         force_mut(&self.source.parents[self.version]).insert(next, next);
    //     }
    //     next
    // }
}
impl<A, S: DerefMut<Target = VersionedUnionFind<A>>> Projection<S> {
    #[inline(always)]
    fn propagate<F: Fn(Projection<&mut VersionedUnionFind<A>>) -> B, B>(&mut self, callback: F) {
        if self.source.flags.propagate {
            self.source.for_each_subversion(self.version, |this, sv| callback(this.take(sv)));
        }
    }
}
impl<A, S: DerefMut<Target = VersionedUnionFind<A>>> UnionFind for Projection<S> {
    #[cfg_attr(
        feature = "trace-unionfind-versioned",
        tracing::instrument(skip(self, element), ret)
    )]
    fn new_element(&mut self, element: Self::ElementData) -> Id {
        let id = Id::from(self.source.universe.len());
        self.source.universe.push(element);
        self.source.parents[VersionTree::ROOT_VERSION].insert(id, id);
        id
    }
    #[cfg_attr(feature = "trace-unionfind-versioned", tracing::instrument(skip(self), ret))]
    fn find_mut(&mut self, x: Id) -> Id {
        let mut current = x;
        let (mut parent, mut checkout) = self.get_parent_and_version(current);
        #[cfg(feature = "pc-full")]
        // For every traversed node, make it point to its canonical in this version.
        // Amortized logarithmic (or Inverse Ackermann with union by rank).
        {
            let mut path = vec![];
            while current != parent {
                let child = current;
                let previous_checkout = checkout;
                current = parent;
                (parent, checkout) = self.get_parent_and_version(current);
                if current != parent || previous_checkout != self.version {
                    path.push(child);
                }
            }
            for traversed in path {
                self.source.add_edge(current, self.version, traversed); // compression
            }
        }
        #[cfg(feature = "pc-none")]
        // No path compression.
        while current != parent {
            current = parent;
            (parent, checkout) = self.get_parent_and_version(current);
        }
        #[cfg(feature = "pc-split")]
        // For every traversed node, make its child point to its parent in this version.
        // Preserve complexity with less memory consumption.
        while current != parent {
            let child = current;
            let previous_checkout = checkout;
            current = parent;
            (parent, checkout) = self.get_parent_and_version(current);
            if current != parent || previous_checkout != self.version {
                self.source.add_edge(parent, self.version, child); // compression
            }
        }
        #[cfg(feature = "pc-half")] // egg's original; our default
        // For every second traversed node, make its child point to its parent in this version.
        // Preserve complexity with less memory consumption and accesses.
        {
            let mut compress: bool = true;
            while current != parent {
                let child = current;
                let previous_checkout = checkout;
                current = parent;
                (parent, checkout) = self.get_parent_and_version(current);
                if compress && (current != parent || previous_checkout != self.version) {
                    self.source.add_edge(parent, self.version, child); // compression
                }
                compress = !compress;
            }
        }
        // NOTE keep track of versioned canonicals
        // add `current -- v <-` at the queried version `v` for patterns
        // (1) {current, parent} -- !v <-
        // Failing Test: [crate::unionfind::versioned::testing::version_inheritance_loops]
        #[cfg(feature = "pc-none")]
        if !self.source.flags.lock_superversions && checkout != self.version {
            self.source.add_edge(current, self.version, current);
        }
        // NOTE when path compression is enabled, we always track versioned
        // canonicals to compress the path on the version tree for canonicals,
        // even if it is not needed to prevent cycles with lock_superversions.
        #[cfg(not(feature = "pc-none"))]
        if checkout != self.version {
            self.source.add_edge(current, self.version, current);
        }
        current
    }
    #[cfg_attr(feature = "trace-unionfind-versioned", tracing::instrument(skip(self), ret))]
    fn union_canonical(&mut self, xc: Id, yc: Id) -> Id {
        self.propagate(|mut sp| sp.union(xc, yc));
        let (parent, child) = (xc, yc);
        self.source.add_edge(parent, self.version, child);
        parent
    }
}

crate::structures::unionfind::testing::test_unionfind!(unionfind_tests, VersionedUnionFind<()>);
crate::structures::unionfind::versioned::testing::test_versioned_unionfind!(
    versioned_unionfind_tests,
    VersionedUnionFind<()>
);

impl<A: std::fmt::Debug> GraphViz for VersionedUnionFind<A> {
    #[cfg_attr(feature = "trace-unionfind-versioned", tracing::instrument(skip(self), ret))]
    fn graphviz(&self, label: &str) -> String {
        use crate::structures::unionfind::UnionFindView;
        use crate::structures::versiontree::info::VersionInfo;

        let mut out = String::new();
        let version_infos = VersionInfo::extract(self.versioning());
        let graph_id: String = crate::util::graphviz::new_graph_id();
        out += &format!("subgraph cluster_{} {{", graph_id);
        out += &format!("\n\tlabel = \"{}\";", label);
        let mut names: Vec<String> = Vec::new();
        for node_id in self.universe() {
            let node_name = format!("{}_{}", graph_id, node_id);
            let node_label = format!("{}: {:?}", node_id, self.get_element(node_id));
            out += &format!("\n\t{} [label=\"{}\"];", node_name, node_label);
            names.push(node_name);
        }
        for version in self.versioning().versions() {
            if self.versioning().exists(version) {
                for (child, parent) in self.parents[version].iter() {
                    let child_label = &names[child.to_i()];
                    let parent_label = &names[parent.to_i()];
                    out += &format!(
                        "\n\t\"{}\" -> \"{}\" [label=\"{}\"];",
                        child_label,
                        parent_label,
                        version_infos.get(&version).unwrap().name,
                    );
                }
            }
        }
        out += "\n}";
        out
    }
}
