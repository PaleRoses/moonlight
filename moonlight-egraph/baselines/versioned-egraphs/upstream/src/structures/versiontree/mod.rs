use crate::structures::Map;

/// A uniquely identified version.
pub type Version = usize;

#[derive(Debug, Clone)]
/// A node in the [VersionTree].
struct VersionNode {
    /// The superversion of this [Version].
    superversion: Version,
    /// The subversions of this [Version].
    subversions: Vec<Version>,
    /// True if the [Version] exists; false otherwise. A [Version] does not
    /// exist if it was removed and its identifier never recycled.
    exist: bool,
}
impl crate::util::memory::HasMemory for VersionNode {
    fn fields(&self) -> Vec<(&str, &dyn crate::util::memory::HasMemory)> {
        vec![
            ("superversion", &self.superversion),
            ("subversions", &self.subversions),
            ("exist", &self.exist),
        ]
    }
}

/// A tree of [Version]s. Each [Version] inherits some semantics from its
/// superversion. Every [VersionTree] has at least one [Version], namely
/// [VersionTree::ROOT_VERSION].
#[derive(Debug, Clone)]
pub struct VersionTree {
    /// The [VersionNode]s in this [VersionTree].
    versions: Vec<VersionNode>,
    /// The history of removed [Version]s. Used to recycle identifiers.
    removed_versions: Vec<Version>,
}
impl crate::util::memory::HasMemory for VersionTree {
    fn fields(&self) -> Vec<(&str, &dyn crate::util::memory::HasMemory)> {
        vec![("versions", &self.versions), ("removed_versions", &self.removed_versions)]
    }
}

impl VersionTree {
    /// The first [Version] in every [VersionTree].
    pub const ROOT_VERSION: Version = 0;

    /// #### Return
    /// A new [VersionTree] with a single [Version], namely
    /// [VersionTree::ROOT_VERSION].
    pub fn new() -> VersionTree {
        let root = VersionNode {
            superversion: VersionTree::ROOT_VERSION,
            subversions: Vec::new(),
            exist: true,
        };
        VersionTree { versions: vec![root], removed_versions: Vec::new() }
    }

    /// #### Return
    /// The path from `v` to the [VersionTree::ROOT_VERSION], including
    /// both `v` and the [VersionTree::ROOT_VERSION].
    pub fn path(&self, v: Version) -> Vec<Version> {
        let mut path = vec![v];
        self.ancestors(v).for_each(|sv| path.push(sv));
        path
    }

    /// #### Description
    /// Create a new [Version] that inherits from the [Version] `v`.
    /// #### Return
    /// The new [Version].
    /// #### Panics
    /// - "Cannot branchout from removed version": if `superversion` was removed
    pub fn branchout(&mut self, v: Version) -> Version {
        assert!(self.exists(v), "Cannot branchout from removed version: version {v}",);
        let new_version = VersionNode { superversion: v, subversions: Vec::new(), exist: true };
        let new_version_id = match self.removed_versions.pop() {
            Some(recycled_id) => {
                self.versions[recycled_id] = new_version;
                recycled_id
            },
            None => {
                let new_id = self.versions.len();
                self.versions.push(new_version);
                new_id
            },
        };
        self.versions[v].subversions.push(new_version_id);
        new_version_id
    }

    /// #### Description
    /// Remove the [Version] `v` and all its subversions.
    /// #### Panics
    /// - "Cannot remove the root version": if `v` is [VersionTree::ROOT_VERSION]
    /// - "Cannot remove already removed version": if `v` was already removed
    /// #### Warnings
    /// Care must be taken when using this method. In fact, users may hold
    /// references to [Version]s that are removed. Additionally, the
    /// [VersionTree] may recycle identifiers of removed [Version]s. As a
    /// consequence, any operation on a previously removed [Version] is
    /// undefined behavior.
    pub fn remove(&mut self, v: Version) {
        assert!(v != VersionTree::ROOT_VERSION, "Cannot remove the root version.");
        assert!(self.exists(v), "Cannot remove already removed version: version {v}.",);
        for sv in 0..self.versions[v].subversions.len() {
            self.remove_version(self.versions[v].subversions[sv]);
        }
        let parent = self.versions[v].superversion;
        self.versions[parent].subversions.retain(|&sv| sv != v);
        self.remove_version(v);
    }
    fn remove_version(&mut self, version: Version) {
        let node = &mut self.versions[version];
        node.subversions.clear();
        node.exist = false;
        self.removed_versions.push(version);
    }

    /// #### Description
    /// Change the superversion of `v` to `new_parent`.
    /// #### Panics
    /// - "Cannot rebase the root version": if `v` is
    ///   [VersionTree::ROOT_VERSION]
    /// - "Cannot rebase removed version: version {}": if `v` was removed
    /// - "Cannot rebase to removed version: version {}": if `new_parent` was
    ///   removed
    pub fn rebase(&mut self, v: Version, new_parent: Version) {
        assert!(v != VersionTree::ROOT_VERSION, "Cannot rebase the root version.");
        assert!(self.exists(v), "Cannot rebase removed version: version {v}.");
        assert!(self.exists(new_parent), "Cannot rebase to removed version: version {new_parent}.");
        let parent = self.versions[v].superversion;
        if parent != new_parent {
            self.versions[parent].subversions.retain(|&sv| sv != v);
            self.versions[v].superversion = new_parent;
            self.versions[new_parent].subversions.push(v);
        }
    }

    /// #### Return
    /// The set of all [Version]s in this [VersionTree]. The set may contain
    /// removed [Version]s, which can be filtered with [VersionTree::exists].
    pub fn versions(&self) -> std::ops::Range<Version> {
        0..self.versions.len()
    }

    /// #### Return
    /// The superversion of the [Version] `v`, or `v` if it's the [VersionTree::ROOT_VERSION].
    /// #### Panics
    /// - "Cannot query superversion of removed version": if `v` was removed
    pub fn superversion(&self, v: Version) -> Version {
        assert!(self.exists(v), "Cannot query superversion of removed version: version {v}",);
        self.versions[v].superversion
    }

    /// #### Return
    /// The subversions of the [Version] `v`.
    /// #### Panics
    /// - "Cannot query subversions of removed version": if `v` was removed
    pub fn subversions(&self, v: Version) -> &Vec<Version> {
        assert!(self.exists(v), "Cannot query subversions of removed version: version {v}",);
        &self.versions[v].subversions
    }

    /// #### Return
    /// An bottom-up iterator over the ancestors of the [Version] `version`,
    /// excluding `version` itself.
    pub fn ancestors(&self, version: Version) -> impl std::iter::Iterator<Item = Version> + '_ {
        struct AncestorIter<'a> {
            vt: &'a VersionTree,
            current: Version,
        }
        impl Iterator for AncestorIter<'_> {
            type Item = Version;
            fn next(&mut self) -> Option<Self::Item> {
                let superversion = self.vt.superversion(self.current);
                if self.current == superversion {
                    return None;
                }
                self.current = superversion;
                Some(superversion)
            }
        }
        AncestorIter { vt: self, current: version }
    }

    /// #### Return
    /// A pre-order DFS iterator over the descendants of the [Version]
    /// `version`, excluding `version` itself.
    pub fn descendants(&self, version: Version) -> impl std::iter::Iterator<Item = Version> + '_ {
        struct DescendantIter<'a> {
            vt: &'a VersionTree,
            stack: Vec<Version>,
        }
        impl Iterator for DescendantIter<'_> {
            type Item = Version;
            fn next(&mut self) -> Option<Self::Item> {
                let current_option = self.stack.pop();
                if let Some(current) = current_option {
                    for &child in &self.vt.versions[current].subversions {
                        self.stack.push(child);
                    }
                    current_option
                } else {
                    None
                }
            }
        }
        DescendantIter { vt: self, stack: self.versions[version].subversions.clone() }
    }

    /// #### Return
    /// True if the [Version] `v` exists; false otherwise.
    /// #### Note
    /// A [Version] does not exist if it was removed and its identifier never
    /// recycled.
    #[inline(always)]
    pub fn exists(&self, v: Version) -> bool {
        self.versions[v].exist
    }
}

impl Default for VersionTree {
    fn default() -> VersionTree {
        VersionTree::new()
    }
}

impl std::fmt::Display for VersionTree {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut s: String = String::from("");
        let indent: &str = "  ";
        let dfs = info::VersionInfo::extract_dfs(self);
        for info::VersionInfo { name, depth, parent: _, this } in dfs {
            if depth != 0 {
                s.push_str(format!("{}|-", indent.repeat(depth)).as_str());
            }
            s.push_str(format!("{} ({})\n", name, this).as_str());
        }
        f.write_str(s.as_str())
    }
}

pub mod info {
    use super::*;

    #[derive(Debug)]
    pub struct VersionInfo {
        pub name: String,
        pub depth: usize,
        pub parent: Version,
        pub this: Version,
    }
    impl std::fmt::Display for VersionInfo {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            f.write_str(&self.name)
        }
    }
    impl VersionInfo {
        pub fn extract(vt: &VersionTree) -> Map<Version, VersionInfo> {
            VersionInfo::extract_dfs(vt).into_iter().map(|n| (n.this, n)).collect::<Map<_, _>>()
        }
        pub fn extract_dfs(vt: &VersionTree) -> Vec<VersionInfo> {
            let children = VersionInfo::children(vt);
            let root_info: VersionInfo = VersionInfo {
                name: String::from("0"),
                depth: 0,
                parent: VersionTree::ROOT_VERSION,
                this: VersionTree::ROOT_VERSION,
            };
            let mut dfs: Vec<VersionInfo> = vec![];
            let mut stack: Vec<VersionInfo> = vec![root_info];
            while let Some(current) = stack.pop() {
                for i in (0..children[current.this].len()).rev() {
                    stack.push(VersionInfo {
                        name: format!("{}.{}", current.name, i + 1),
                        depth: current.depth + 1,
                        parent: current.this,
                        this: children[current.this][i],
                    });
                }
                dfs.push(current);
            }
            dfs
        }
        fn children(vt: &VersionTree) -> Vec<Vec<Version>> {
            let mut children: Vec<Vec<Version>> = vt.versions.iter().map(|_| Vec::new()).collect();
            for child in 0..vt.versions.len() {
                let parent = vt.versions[child].superversion;
                if child == parent {
                    continue;
                }
                children[parent].push(child);
            }
            children
        }
    }
}

#[cfg(test)]
#[rustfmt::skip]
mod tests {
    use super::*;
    use crate::util::testing::*;

    #[test]
    fn new() {
        let vt = VersionTree::new();
        assert_eq!(vt.versions.len(), 1);

        let v0 = VersionTree::ROOT_VERSION;
        assert_eq!(vt.versions[v0].superversion, VersionTree::ROOT_VERSION);
        assert!(vt.versions[v0].subversions.is_empty());
        assert!(vt.versions[v0].exist);
    }

    #[test]
    fn branchout() {
        let mut vt = VersionTree::new();
        let v0 = VersionTree::ROOT_VERSION;
        let v0_1 = vt.branchout(v0);
        let v0_2 = vt.branchout(v0);
        let v0_1_1 = vt.branchout(v0_1);
        let v0_1_2 = vt.branchout(v0_1);

        assert_eq!(vt.versions.len(), 5);
        assert!([v0, v0_1, v0_2, v0_1_1, v0_1_2].iter().all(|&v| vt.exists(v)));
        assert_eq!(vt.versions[v0].superversion, v0);
        assert_eq!(vt.versions[v0_1].superversion, v0);
        assert_eq!(vt.versions[v0_2].superversion, v0);
        assert_eq!(vt.versions[v0_1_1].superversion, v0_1);
        assert_eq!(vt.versions[v0_1_2].superversion, v0_1);

        assert_eq_by!(eqs::vec::no_order, &vt.subversions(v0), &vec![v0_1, v0_2]);
        assert_eq_by!(eqs::vec::no_order, &vt.subversions(v0_1), &vec![v0_1_1, v0_1_2]);
        assert!(vt.subversions(v0_2).is_empty());
        assert!(vt.subversions(v0_1_1).is_empty());
        assert!(vt.subversions(v0_1_2).is_empty());
    }

    #[test]
    #[should_panic(expected = "Cannot branchout from removed version: version 1")]
    fn branchout_from_removed_version() {
        let mut vt = VersionTree::new();
        let v0 = VersionTree::ROOT_VERSION;
        let v0_1 = vt.branchout(v0);
        vt.remove(v0_1);
        vt.branchout(v0_1);
    }

    #[test]
    fn rebase() {
        let mut vt = VersionTree::new();
        let v0 = VersionTree::ROOT_VERSION;
        let v0_1 = vt.branchout(v0);
        let v0_1_1 = vt.branchout(v0_1);
        let v0_1_2 = vt.branchout(v0_1);
        let v0_2 = vt.branchout(v0);
        let v0_2_1 = vt.branchout(v0_2);
        let v0_2_2 = vt.branchout(v0_2);

        assert_eq!(vt.versions[v0_2_1].superversion, v0_2);
        assert_eq_by!(eqs::vec::no_order, &vt.subversions(v0_1), &vec![v0_1_1, v0_1_2]);
        assert_eq_by!(eqs::vec::no_order, &vt.subversions(v0_2), &vec![v0_2_1, v0_2_2]);
        vt.rebase(v0_2_1, v0_1);
        assert_eq!(vt.versions[v0_2_1].superversion, v0_1);
        assert_eq_by!(eqs::vec::no_order, &vt.subversions(v0_1), &vec![v0_1_1, v0_1_2, v0_2_1]);
        assert_eq_by!(eqs::vec::no_order, &vt.subversions(v0_2), &vec![v0_2_2]);
    }

    #[test]
    #[should_panic(expected = "Cannot rebase the root version")]
    fn rebase_root_version() {
        let mut vt = VersionTree::new();
        let v0_1 = vt.branchout(VersionTree::ROOT_VERSION);
        vt.rebase(VersionTree::ROOT_VERSION, v0_1);
    }

    #[test]
    #[should_panic(expected = "Cannot rebase removed version: version 1")]
    fn rebase_removed_version() {
        let mut vt = VersionTree::new();
        let v0 = VersionTree::ROOT_VERSION;
        let v0_1 = vt.branchout(v0);
        vt.remove(v0_1);
        vt.rebase(v0_1, VersionTree::ROOT_VERSION);
    }

    #[test]
    #[should_panic(expected = "Cannot rebase to removed version: version 1")]
    fn rebase_to_removed_version() {
        let mut vt = VersionTree::new();
        let v0 = VersionTree::ROOT_VERSION;
        let v0_1 = vt.branchout(v0);
        let v0_2 = vt.branchout(v0);
        vt.remove(v0_1);
        vt.rebase(v0_2, v0_1);
    }

    #[test]
    fn remove() {
        let mut vt = VersionTree::new();
        let v0 = VersionTree::ROOT_VERSION;
        let v0_1 = vt.branchout(v0);
        let v0_1_1 = vt.branchout(v0_1);
        let v0_1_1_1 = vt.branchout(v0_1_1);
        assert!(vt.versions[v0_1_1].exist);
        assert!(vt.versions[v0_1_1_1].exist);
        assert_eq!(*vt.subversions(v0_1), vec![v0_1_1]);
        vt.remove(v0_1_1);
        assert!(!vt.versions[v0_1_1].exist);
        assert!(!vt.versions[v0_1_1_1].exist);
        assert_eq!(*vt.subversions(v0_1), vec![] as Vec<Version>);
    }

    #[test]
    #[should_panic(expected = "Cannot remove the root version")]
    fn remove_root_version() {
        let mut vt = VersionTree::new();
        vt.remove(VersionTree::ROOT_VERSION);
    }

    #[test]
    #[should_panic(expected = "Cannot remove already removed version: version 1")]
    fn remove_removed_version() {
        let mut vt = VersionTree::new();
        let v0 = VersionTree::ROOT_VERSION;
        let v0_1 = vt.branchout(v0);
        vt.remove(v0_1);
        vt.remove(v0_1);
    }

    #[test]
    #[should_panic(expected = "Cannot remove already removed version: version 2")]
    fn remove_removed_subversion() {
        let mut vt = VersionTree::new();
        let v0 = VersionTree::ROOT_VERSION;
        let v0_1 = vt.branchout(v0);
        let v0_1_1 = vt.branchout(v0_1);
        vt.remove(v0_1);
        vt.remove(v0_1_1);
    }

    #[test]
    #[should_panic(expected = "Cannot query superversion of removed version: version 1")]
    fn query_superversion_of_removed_version() {
        let mut vt = VersionTree::new();
        let v0 = VersionTree::ROOT_VERSION;
        let v0_1 = vt.branchout(v0);
        vt.remove(v0_1);
        vt.superversion(v0_1);
    }

    #[test]
    #[should_panic(expected = "Cannot query subversions of removed version: version 1")]
    fn query_subversions_of_removed_version() {
        let mut vt = VersionTree::new();
        let v0 = VersionTree::ROOT_VERSION;
        let v0_1 = vt.branchout(v0);
        vt.remove(v0_1);
        vt.subversions(v0_1);
    }
}
