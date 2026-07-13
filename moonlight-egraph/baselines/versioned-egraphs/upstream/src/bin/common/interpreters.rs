use super::generators::{Action, Log};
use veg::structures::egraph::versioned::VersionedEGraph;
use veg::structures::egraph::{EClass, EGraph, EGraphView, ENode, Operator};
use veg::structures::unionfind::versioned::VersionedUnionFind;
use veg::structures::unionfind::UnionFind;
use veg::structures::versiontree::{Version, VersionTree};

pub mod cloning {
    use super::*;
    pub fn interpret<G: EGraph + Clone + Default>(log: Log) -> Vec<G> {
        let mut universe: Vec<EClass> = Vec::new();
        let mut versions: Vec<G> = vec![G::default()];
        let mut version_tree: VersionTree = VersionTree::new();
        let mut checkout: usize = 0;
        for action in log.consume() {
            match action {
                Action::Add { op, args } => {
                    let enode = ENode::application(
                        Operator::from(op),
                        args.into_iter().map(|arg| universe[arg]).collect(),
                    );

                    // All elements are added before forking, so they always go to the root version
                    universe.push(versions[checkout].add(enode.clone()));
                },
                Action::Union { xi, yi } => {
                    let x: EClass = universe[xi];
                    let y: EClass = universe[yi];
                    versions[checkout].union(x, y);
                    versions[checkout].rebuild(); // Standard union (without deferred rebuilding)

                    // NOTE to have the same inheritance semantics as versioned egraphs, we need to
                    // propagate the union to all descendants of the current version. Otherwise,
                    // unions performed in parent versions would not be visible in child versions.
                    for sv in version_tree.descendants(checkout) {
                        versions[sv].union(x, y);
                        versions[sv].rebuild(); // Standard union (without deferred rebuilding)
                    }
                },
                Action::Find { xi } => {
                    versions[checkout].find_mut(universe[xi]);
                },
                Action::Checkout { vi } => {
                    checkout = vi;
                },
                Action::Branchout => {
                    let latest = version_tree.branchout(checkout);
                    versions.push(versions[checkout].clone());
                    checkout = latest;
                },
            }
        }
        versions
    }
}
pub mod versioned {
    use super::*;
    pub fn interpret<G: VersionedEGraph + Default>(log: Log) -> G {
        let mut egraph: G = G::default();

        // Allow operations on any versions (not just leaves)
        egraph.flags_mut().lock_superversions = false;
        egraph.flags_mut().propagate = true;

        let mut universe: Vec<EClass> = Vec::new();
        let mut versions: Vec<Version> = vec![VersionTree::ROOT_VERSION];
        let mut checkout: usize = 0;
        for action in log.consume() {
            match action {
                Action::Add { op, args } => {
                    let enode = ENode::application(
                        Operator::from(op),
                        args.into_iter().map(|arg| universe[arg]).collect(),
                    );
                    // All elements are added before forking, so they always go to the root version
                    universe.push(egraph.take(versions[checkout]).add(enode))
                },
                Action::Union { xi, yi } => {
                    egraph.take(versions[checkout]).union(universe[xi], universe[yi]);
                    egraph.take(versions[checkout]).rebuild(); // Standard union (without deferred rebuilding)
                },
                Action::Find { xi } => {
                    egraph.focus(versions[checkout]).find(universe[xi]);
                },
                Action::Checkout { vi } => {
                    checkout = vi;
                },
                Action::Branchout => {
                    let latest = versions.len();
                    versions.push(egraph.branchout(versions[checkout]));
                    checkout = latest;
                },
            };
        }
        egraph
    }
    pub mod unionfind {
        use super::*;
        pub fn interpret<U: VersionedUnionFind<ElementData: Default> + Default>(log: Log) -> U {
            let mut uf: U = Default::default();

            // Allow operations on any versions (not just leaves)
            uf.flags_mut().lock_superversions = false;
            uf.flags_mut().propagate = true;

            let mut universe: Vec<veg::util::id::Id> = Vec::new();
            let mut versions: Vec<Version> = vec![VersionTree::ROOT_VERSION];
            let mut checkout: usize = 0;

            for action in log.consume() {
                match action {
                    Action::Add { op: _, args: _ } => {
                        universe.push(uf.take(versions[checkout]).new_default());
                    },
                    Action::Union { xi, yi } => {
                        uf.take(checkout).union_canonical(universe[xi], universe[yi]);
                    },
                    Action::Find { xi } => {
                        uf.take(checkout).find_mut(universe[xi]);
                    },
                    Action::Checkout { vi } => {
                        checkout = vi;
                    },
                    Action::Branchout => {
                        let latest = versions.len();
                        versions.push(uf.branchout(versions[checkout]));
                        checkout = latest;
                    },
                }
            }
            uf
        }
    }
}

pub mod colored {
    use super::*;
    use easter_egg::{ColorId, EGraph, Id, SymbolLang};
    pub fn interpret(log: Log) -> EGraph<SymbolLang, ()> {
        let mut egraph: EGraph<SymbolLang, ()> = Default::default();
        let mut universe: Vec<Id> = Vec::new();
        let mut versions: Vec<ColorId> = vec![egraph.create_color(None)];
        let mut checkout: usize = 0;
        for action in log.consume() {
            match action {
                Action::Add { op, args } => {
                    let enode = SymbolLang::new(
                        op.to_string(),
                        args.into_iter().map(|arg| universe[arg]).collect(),
                    );
                    // All elements are added before forking, so they always go to the root version
                    universe.push(egraph.colored_add(versions[checkout], enode))
                },
                Action::Union { xi, yi } => {
                    egraph.colored_union(versions[checkout], universe[xi], universe[yi]);
                    egraph.rebuild(); // Standard union (without deferred rebuilding)
                },
                Action::Find { xi } => {
                    egraph.colored_find(versions[checkout], universe[xi]);
                },
                Action::Checkout { vi } => {
                    checkout = vi;
                },
                Action::Branchout => {
                    let latest = versions.len();
                    versions.push(egraph.create_color(Some(versions[checkout])));
                    checkout = latest;
                },
            }
        }
        egraph
    }
    pub fn interpret_with_disequalities(log: Log) -> EGraph<SymbolLang, ()> {
        // You cannot model forbids using `easter_egg::Analysis`, since its api is not colored.
        // In fact, you cannot use versioned disequality edges without modifying the internals of
        // easteregg.
        // Even what follows, would not work in practice, because it does not track unions that
        // happens during rebuild. Still, it under-estimates the space that easteregg would require
        // to store versioned disequality edges.
        use im::{HashMap, HashSet};
        type Forbids = rustc_hash::FxHashMap<Id, HashSet<Id>>;
        let mut egraph: EGraph<SymbolLang, ()> = Default::default();
        let mut forbids: HashMap<ColorId, Forbids> = Default::default();
        let mut universe: Vec<Id> = Vec::new();
        let mut versions: Vec<ColorId> = vec![egraph.create_color(None)];
        let mut checkout: usize = 0;
        for action in log.consume() {
            match action {
                Action::Add { op, args } => {
                    let enode = SymbolLang::new(
                        op.to_string(),
                        args.into_iter().map(|arg| universe[arg]).collect(),
                    );
                    // All elements are added before forking, so they always go to the root version
                    universe.push(egraph.colored_add(versions[checkout], enode))
                },
                Action::Union { xi, yi } => {
                    let xic = egraph.colored_find(versions[checkout], universe[xi]);
                    let yic = egraph.colored_find(versions[checkout], universe[yi]);
                    let (parent, _) = egraph.colored_union(versions[checkout], xic, yic);
                    let child = if xic == parent { yic } else { xic };
                    if let Some(child_forbids) = forbids[&versions[checkout]].remove(&child) {
                        forbids[&versions[checkout]]
                            .entry(parent)
                            .or_default()
                            .extend(child_forbids);
                    }
                    egraph.rebuild(); // Standard union (without deferred rebuilding)
                },
                Action::Find { xi } => {
                    egraph.colored_find(versions[checkout], universe[xi]);
                },
                Action::Checkout { vi } => {
                    checkout = vi;
                },
                Action::Branchout => {
                    let latest = versions.len();
                    versions.push(egraph.create_color(Some(versions[checkout])));
                    forbids[&versions[latest]] = forbids[&versions[checkout]].clone();
                    checkout = latest;
                },
            }
        }
        egraph
    }
}
