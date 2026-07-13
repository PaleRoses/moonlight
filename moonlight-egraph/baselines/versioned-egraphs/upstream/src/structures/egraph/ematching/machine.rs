// ### DISCLAIMER ##############################################################
// This file is an adaptation of the implementation of ematching from the egg's:
// https://github.com/egraphs-good/egg/blob/main/src/machine.rs
//
// The implementation has been adapted to conform to our public interfaces.
// #############################################################################

use crate::structures::egraph::{EClass, EGraph, EGraphView, ENode, Operator};
use crate::structures::{Map, Set};
use crate::util::id::SymbolIds;
pub use egg::{ENodeOrVar, Pattern, SearchMatches, Subst, Var};
use egg::{FromOp, Id, Language, PatternAst, RecExpr};
use std::result::Result;

fn pretty(ast: &PatternAst<ENode>) -> String {
    ast.pretty(0).split_whitespace().collect::<Vec<_>>().join(" ")
}

impl Language for ENode {
    type Discriminant = Operator;
    fn discriminant(&self) -> Self::Discriminant {
        self.op
    }
    fn matches(&self, other: &Self) -> bool {
        self.op == other.op && self.args.len() == other.args.len()
    }
    fn children(&self) -> &[egg::Id] {
        &self.args
    }
    fn children_mut(&mut self) -> &mut [egg::Id] {
        &mut self.args
    }
}
impl FromOp for ENode {
    type Error = ();
    fn from_op(op: &str, children: Vec<Id>) -> Result<Self, Self::Error> {
        Ok(ENode { op: SymbolIds::from_string(op), args: children })
    }
}
pub trait MachineRequirements {
    fn canonicals(&self) -> impl std::iter::Iterator<Item = &Id>;
    fn for_each_matching_node<Err>(
        &self,
        eclass: EClass,
        enode: &ENode,
        callback: impl FnMut(&ENode) -> Result<(), Err>,
    ) -> Result<(), Err>;
}
fn private_extract<L: Language>(expr: &RecExpr<L>, new_root: Id) -> RecExpr<L> {
    expr[new_root].build_recexpr(|id| expr[id].clone())
}

#[derive(Default)]
struct Machine {
    reg: Vec<Id>,
    // a buffer to re-use for lookups
    lookup: Vec<Id>,
}

#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
struct Reg(u32);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Program {
    instructions: Vec<Instruction>,
    subst: Subst,
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum Instruction {
    Bind { node: ENode, i: Reg, out: Reg },
    Compare { i: Reg, j: Reg },
    Lookup { term: Vec<ENodeOrReg>, i: Reg },
    Scan { out: Reg },
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum ENodeOrReg {
    ENode(ENode),
    Reg(Reg),
}

impl Machine {
    #[inline(always)]
    fn reg(&self, reg: Reg) -> Id {
        self.reg[reg.0 as usize]
    }
    #[cfg_attr(feature = "trace-machine", tracing::instrument(skip(self, egraph, yield_fn), ret))]
    fn run<G: EGraphView + MachineRequirements>(
        &mut self,
        egraph: &G,
        instructions: &[Instruction],
        subst: &Subst,
        yield_fn: &mut impl FnMut(&Self, &Subst) -> Result<(), ()>,
    ) -> Result<(), ()> {
        let mut instructions = instructions.iter();
        while let Some(instruction) = instructions.next() {
            match instruction {
                Instruction::Bind { i, out, node } => {
                    let remaining_instructions = instructions.as_slice();
                    let eclass = egraph.find(self.reg(*i));
                    return egraph.for_each_matching_node(eclass, node, |matched| {
                        self.reg.truncate(out.0 as usize);
                        matched.for_each(|id| self.reg.push(id));
                        self.run(egraph, remaining_instructions, subst, yield_fn)
                    });
                },
                Instruction::Scan { out } => {
                    let remaining_instructions = instructions.as_slice();
                    for &class in egraph.canonicals() {
                        self.reg.truncate(out.0 as usize);
                        self.reg.push(class);
                        self.run(egraph, remaining_instructions, subst, yield_fn)?
                    }
                    return Ok(());
                },
                Instruction::Compare { i, j } => {
                    if egraph.find(self.reg(*i)) != egraph.find(self.reg(*j)) {
                        return Ok(());
                    }
                },
                Instruction::Lookup { term, i } => {
                    self.lookup.clear();
                    for node in term {
                        match node {
                            ENodeOrReg::ENode(node) => {
                                let look = |i| self.lookup[usize::from(i)];
                                match egraph.get_class(&node.clone().map_children(look)) {
                                    Some(id) => self.lookup.push(egraph.find(*id)),
                                    None => return Ok(()),
                                }
                            },
                            ENodeOrReg::Reg(r) => {
                                self.lookup.push(egraph.find(self.reg(*r)));
                            },
                        }
                    }

                    let id = egraph.find(self.reg(*i));
                    if self.lookup.last().copied() != Some(id) {
                        return Ok(());
                    }
                },
            }
        }
        yield_fn(self, subst)
    }
}

struct Compiler {
    v2r: Map<Var, Reg>, // CHANGED was IndexMap
    free_vars: Vec<Set<Var>>,
    subtree_size: Vec<usize>,
    todo_nodes: Map<(Id, Reg), ENode>,
    instructions: Vec<Instruction>,
    next_reg: Reg,
}

impl Compiler {
    fn new() -> Self {
        Self {
            free_vars: Default::default(),
            subtree_size: Default::default(),
            v2r: Default::default(),
            todo_nodes: Default::default(),
            instructions: Default::default(),
            next_reg: Reg(0),
        }
    }

    fn add_todo(&mut self, pattern: &PatternAst<ENode>, id: Id, reg: Reg) {
        match &pattern[id] {
            ENodeOrVar::Var(v) => {
                if let Some(&j) = self.v2r.get(v) {
                    self.instructions.push(Instruction::Compare { i: reg, j })
                } else {
                    self.v2r.insert(*v, reg);
                }
            },
            ENodeOrVar::ENode(pat) => {
                self.todo_nodes.insert((id, reg), pat.clone());
            },
        }
    }

    fn load_pattern(&mut self, pattern: &PatternAst<ENode>) {
        let len = pattern.len();
        self.free_vars = Vec::with_capacity(len);
        self.subtree_size = Vec::with_capacity(len);

        for node in pattern {
            let mut free = Set::default();
            let mut size = 0;
            match node {
                ENodeOrVar::ENode(n) => {
                    size = 1;
                    for &child in n.children() {
                        free.extend(&self.free_vars[usize::from(child)]);
                        size += self.subtree_size[usize::from(child)];
                    }
                },
                ENodeOrVar::Var(v) => {
                    free.insert(*v);
                },
            }
            self.free_vars.push(free);
            self.subtree_size.push(size);
        }
    }

    fn next(&mut self) -> Option<((Id, Reg), ENode)> {
        // we take the max todo according to this key
        // - prefer grounded
        // - prefer more free variables
        // - prefer smaller term
        let key = |(id, _): &&(Id, Reg)| {
            let i = usize::from(*id);
            let n_bound = self.free_vars[i].iter().filter(|v| self.v2r.contains_key(*v)).count();
            let n_free = self.free_vars[i].len() - n_bound;
            let size = self.subtree_size[i] as isize;
            (n_free == 0, n_free, -size)
        };

        self.todo_nodes
            .keys()
            .max_by_key(key)
            .copied()
            .map(|k| (k, self.todo_nodes.remove(&k).unwrap()))
    }

    /// check to see if this e-node corresponds to a term that is grounded by
    /// the variables bound at this point
    fn is_ground_now(&self, id: Id) -> bool {
        self.free_vars[usize::from(id)].iter().all(|v| self.v2r.contains_key(v))
    }

    #[cfg_attr(feature = "trace-machine", tracing::instrument(skip(self), ret))]
    fn compile(&mut self, patternbinder: Option<Var>, pattern: &PatternAst<ENode>) {
        self.load_pattern(pattern);
        let root = pattern.root();

        let mut next_out = self.next_reg;

        // Check if patternbinder already bound in v2r
        // Behavior common to creating a new pattern
        let add_new_pattern = |comp: &mut Compiler| {
            if !comp.instructions.is_empty() {
                // After first pattern needs scan
                comp.instructions.push(Instruction::Scan { out: comp.next_reg });
            }
            comp.add_todo(pattern, root, comp.next_reg);
        };

        if let Some(v) = patternbinder {
            if let Some(&i) = self.v2r.get(&v) {
                // patternbinder already bound
                self.add_todo(pattern, root, i);
            } else {
                // patternbinder is new variable
                next_out.0 += 1;
                add_new_pattern(self);
                self.v2r.insert(v, self.next_reg); //add to known variables.
            }
        } else {
            // No pattern binder
            next_out.0 += 1;
            add_new_pattern(self);
        }

        while let Some(((id, reg), node)) = self.next() {
            if self.is_ground_now(id) && !node.is_leaf() {
                let extracted = private_extract(pattern, id);
                self.instructions.push(Instruction::Lookup {
                    i: reg,
                    term: extracted
                        .iter()
                        .map(|n| match n {
                            ENodeOrVar::ENode(n) => ENodeOrReg::ENode(n.clone()),
                            ENodeOrVar::Var(v) => ENodeOrReg::Reg(self.v2r[v]),
                        })
                        .collect(),
                });
            } else {
                let out = next_out;
                next_out.0 += node.len() as u32;

                // zero out the children so Bind can use it to sort
                let op = node.clone().map_children(|_| Id::from(0));
                self.instructions.push(Instruction::Bind { i: reg, node: op, out });

                for (i, &child) in node.children().iter().enumerate() {
                    self.add_todo(pattern, child, Reg(out.0 + i as u32));
                }
            }
        }
        self.next_reg = next_out;
    }

    fn extract(self) -> Program {
        let mut subst = Subst::default();
        for (v, r) in self.v2r {
            subst.insert(v, Id::from(r.0 as usize));
        }
        Program { instructions: self.instructions, subst }
    }
}

impl Program {
    #[cfg_attr(feature = "trace-machine", tracing::instrument(fields(pattern = %pretty(pattern)), ret))]
    pub(crate) fn compile_from_pat(pattern: &PatternAst<ENode>) -> Self {
        let mut compiler = Compiler::new();
        compiler.compile(None, pattern);
        let program = compiler.extract();
        program
    }
    #[cfg_attr(feature = "trace-machine", tracing::instrument(ret))]
    pub(crate) fn compile_from_multi_pat(patterns: &[(Var, PatternAst<ENode>)]) -> Self {
        let mut compiler = Compiler::new();
        for (var, pattern) in patterns {
            compiler.compile(Some(*var), pattern);
        }
        compiler.extract()
    }

    #[cfg_attr(feature = "trace-machine", tracing::instrument(skip(self, egraph), ret))]
    pub fn run_with_limit<G: EGraphView + MachineRequirements>(
        &self,
        egraph: &G,
        eclass: Id,
        mut limit: usize,
    ) -> Vec<Subst> {
        assert!(egraph.is_clean(), "Tried to search a dirty e-graph!");

        if limit == 0 {
            return vec![];
        }

        let mut machine = Machine::default();
        assert_eq!(machine.reg.len(), 0);
        machine.reg.push(eclass);

        let mut matches: Vec<Subst> = Vec::new();
        machine
            .run(egraph, &self.instructions, &self.subst, &mut |machine, subst| {
                // CHANGED we don't need this
                // if !egraph.analysis.allow_ematching_cycles() {
                //     if let Some((first, rest)) = machine.reg.split_first() {
                //         if rest.contains(first) {
                //             return Ok(());
                //         }
                //     }
                // }

                let subst_vec_private = unsafe {
                    std::mem::transmute::<&Subst, &smallvec::SmallVec<[(Var, Id); 3]>>(subst)
                };
                let subst_vec = subst_vec_private
                    .iter()
                    // HACK we are reusing Ids here, this is bad
                    .map(|(v, reg_id)| (*v, machine.reg(Reg(usize::from(*reg_id) as u32))))
                    .collect();
                let subst_private = unsafe {
                    std::mem::transmute::<smallvec::SmallVec<[(Var, Id); 3]>, Subst>(subst_vec)
                };
                matches.push(subst_private);
                limit -= 1;
                if limit != 0 {
                    Ok(())
                } else {
                    Err(())
                }
            })
            .unwrap_or_default();
        matches
    }
}

struct ProgramInspector {
    ast: PatternAst<ENode>,
    program: Program,
}
impl ProgramInspector {
    #[cfg_attr(feature = "trace-machine", tracing::instrument(fields(pattern = %pretty(&pattern.ast))))]
    pub fn inspect(pattern: &Pattern<ENode>) -> &ProgramInspector {
        unsafe { std::mem::transmute::<&Pattern<ENode>, &ProgramInspector>(pattern) }
    }
}

pub trait CanEMatch<L: Language> {
    fn ematch<G: EGraphView + MachineRequirements>(&self, egraph: &G) -> Vec<SearchMatches<L>>;
}
impl CanEMatch<ENode> for Pattern<ENode> {
    #[cfg_attr(feature = "trace-machine", tracing::instrument(fields(self = %pretty(&self.ast)), skip(egraph), ret))]
    fn ematch<G: EGraphView + MachineRequirements>(&self, egraph: &G) -> Vec<SearchMatches<ENode>> {
        let mut ms = vec![];
        for &eclass in egraph.canonicals() {
            let inspector = ProgramInspector::inspect(self);
            let substs = inspector.program.run_with_limit(egraph, eclass, usize::MAX);
            if !substs.is_empty() {
                let ast = Some(std::borrow::Cow::Borrowed(&inspector.ast));
                let m = SearchMatches { eclass, substs, ast };
                ms.push(m)
            };
        }
        ms
    }
}

pub trait CanBind {
    fn bind<G: EGraph>(&self, egraph: &mut G, eclass: Id, subst: &Subst) -> Vec<Id>;
}
impl CanBind for Pattern<ENode> {
    #[cfg_attr(feature = "trace-machine", tracing::instrument(fields(self = %pretty(&self.ast)), skip(egraph), ret))]
    fn bind<G: EGraph>(&self, egraph: &mut G, eclass: Id, subst: &Subst) -> Vec<Id> {
        let mut ids = vec![0.into(); self.ast.len()];
        for (i, pat_node) in self.ast.iter().enumerate() {
            let id = match pat_node {
                ENodeOrVar::Var(w) => subst[*w],
                ENodeOrVar::ENode(e) => {
                    let n = e.clone().map_children(|child| ids[usize::from(child)]);
                    egraph.add(n)
                },
            };
            ids[i] = id;
        }
        let id = *ids.last().unwrap();

        if eclass == id {
            vec![]
        } else {
            vec![id]
        }
    }
}

#[cfg(test)]
#[allow(non_snake_case)]
#[allow(clippy::just_underscores_and_digits)]
mod tests {
    use std::fmt::Debug;

    use super::*;
    use crate::structures::egraph::versioned::VersionedEGraph;
    use crate::structures::versiontree::{Version, VersionTree};
    use crate::util::testing::{assert_eq_by, eqs};

    macro_rules! assert_search {
        ($search: expr, $expected: expr) => {{
            let expected: Vec<EClass> = $expected;
            let actual: Vec<EClass> = ($search).into_iter().map(|r| r.eclass).collect::<Vec<_>>();
            assert_eq_by!(eqs::vec::no_order, &actual, &expected);
        }};
    }

    pub fn search<G: EGraph + MachineRequirements + Default>(mut egraph: G) {
        let [_0, _1, _2] =
            ["0", "1", "2"].map(|num| egraph.add(ENode::constant(SymbolIds::from_string(num))));
        let add: Operator = SymbolIds::from_string("add");
        let add_0_1 = egraph.add(ENode::application(add, vec![_0, _1]));
        let add_1_2 = egraph.add(ENode::application(add, vec![_1, _2]));
        let add_2_2 = egraph.add(ENode::application(add, vec![_2, _2]));
        let add_0_add_1_2 = egraph.add(ENode::application(add, vec![_0, add_1_2]));
        let add_1_add_1_2 = egraph.add(ENode::application(add, vec![_1, add_1_2]));
        let add_2_add_2_2 = egraph.add(ENode::application(add, vec![_2, add_2_2]));

        let pattern = |s: &str| s.parse::<Pattern<ENode>>().unwrap();
        assert_search!(pattern("(0)").ematch(&egraph), vec![_0]);
        assert_search!(pattern("(add 2 2)").ematch(&egraph), vec![add_2_2]);
        assert_search!(
            pattern("(?x)").ematch(&egraph),
            vec![
                _0,
                _1,
                _2,
                add_0_1,
                add_1_2,
                add_2_2,
                add_0_add_1_2,
                add_1_add_1_2,
                add_2_add_2_2
            ]
        );
        assert_search!(pattern("(add ?x ?x)").ematch(&egraph), vec![add_2_2]);
        assert_search!(pattern("(add ?x 2)").ematch(&egraph), vec![add_1_2, add_2_2]);
        assert_search!(
            pattern("(add ?x ?y)").ematch(&egraph),
            vec![add_0_1, add_1_2, add_2_2, add_0_add_1_2, add_1_add_1_2, add_2_add_2_2]
        );
        assert_search!(pattern("(add ?x (add ?y ?y))").ematch(&egraph), vec![add_2_add_2_2]);
        assert_search!(
            pattern("(add ?x (add ?x ?y))").ematch(&egraph),
            vec![add_1_add_1_2, add_2_add_2_2]
        );
        assert_search!(
            pattern("(add ?x (add ?y ?z))").ematch(&egraph),
            vec![add_0_add_1_2, add_1_add_1_2, add_2_add_2_2]
        );
    }

    pub fn search_version_independence<G>(mut egraph: G)
    where
        G: VersionedEGraph + Default + Debug,
        for<'a> G::ProjectionMut<'a>: EGraph + MachineRequirements,
    {
        //! Searching in independent [Version]s should only e-match against [ENode]s that were added
        //! to the queried [Version].
        let v1: Version = egraph.branchout(VersionTree::ROOT_VERSION);
        let [_0, _1, _2, _3, _4, _5] = ["0", "1", "2", "3", "4", "5"]
            .map(|num| egraph.take(v1).add(ENode::constant(SymbolIds::from_string(num))));
        let add: Operator = SymbolIds::from_string("add");
        let add_0_1 = egraph.take(v1).add(ENode::application(add, vec![_0, _1]));
        let add_1_2 = egraph.take(v1).add(ENode::application(add, vec![_1, _2]));
        let add_2_2 = egraph.take(v1).add(ENode::application(add, vec![_2, _2]));

        let v1_1: Version = egraph.branchout(v1);
        let add_0_add_1_2 = egraph.take(v1_1).add(ENode::application(add, vec![_0, add_1_2]));
        let add_1_add_1_2 = egraph.take(v1_1).add(ENode::application(add, vec![_1, add_1_2]));
        let add_2_add_2_2 = egraph.take(v1_1).add(ENode::application(add, vec![_2, add_2_2]));

        let v1_2: Version = egraph.branchout(v1);
        let add_3_add_1_2 = egraph.take(v1_2).add(ENode::application(add, vec![_3, add_1_2]));
        let add_4_add_1_2 = egraph.take(v1_2).add(ENode::application(add, vec![_4, add_1_2]));
        let add_5_add_2_2 = egraph.take(v1_2).add(ENode::application(add, vec![_5, add_2_2]));

        let pattern = |s: &str| s.parse::<Pattern<ENode>>().unwrap();
        assert_search!(pattern("0").ematch(&egraph.take(v1_1)), vec![_0]);
        assert_search!(pattern("(add 2 2)").ematch(&egraph.take(v1_1)), vec![add_2_2]);
        assert_search!(
            pattern("?x").ematch(&egraph.take(v1_1)),
            vec![
                _0,
                _1,
                _2,
                _3,
                _4,
                _5,
                add_0_1,
                add_1_2,
                add_2_2,
                add_0_add_1_2,
                add_1_add_1_2,
                add_2_add_2_2
            ]
        );
        assert_search!(pattern("(add ?x ?x)").ematch(&egraph.take(v1_1)), vec![add_2_2]);
        assert_search!(
            pattern("(add ?x ?y)").ematch(&egraph.take(v1_1)),
            vec![add_0_1, add_1_2, add_2_2, add_0_add_1_2, add_1_add_1_2, add_2_add_2_2]
        );
        assert_search!(
            pattern("(add ?x (add ?y ?y))").ematch(&egraph.take(v1_1)),
            vec![add_2_add_2_2]
        );
        assert_search!(
            pattern("(add ?x (add ?x ?y))").ematch(&egraph.take(v1_1)),
            vec![add_1_add_1_2, add_2_add_2_2]
        );
        assert_search!(
            pattern("(add ?x (add ?y ?z))").ematch(&egraph.take(v1_1)),
            vec![add_0_add_1_2, add_1_add_1_2, add_2_add_2_2]
        );

        assert_search!(pattern("0").ematch(&egraph.take(v1_2)), vec![_0]);
        assert_search!(pattern("(add 2 2)").ematch(&egraph.take(v1_2)), vec![add_2_2]);
        assert_search!(
            pattern("?x").ematch(&egraph.take(v1_2)),
            vec![
                _0,
                _1,
                _2,
                _3,
                _4,
                _5,
                add_0_1,
                add_1_2,
                add_2_2,
                add_3_add_1_2,
                add_4_add_1_2,
                add_5_add_2_2
            ]
        );
        assert_search!(pattern("(add ?x ?x)").ematch(&egraph.take(v1_2)), vec![add_2_2]);
        assert_search!(
            pattern("(add ?x ?y)").ematch(&egraph.take(v1_2)),
            vec![add_0_1, add_1_2, add_2_2, add_3_add_1_2, add_4_add_1_2, add_5_add_2_2]
        );
        assert_search!(
            pattern("(add ?x (add ?y ?y))").ematch(&egraph.take(v1_2)),
            vec![add_5_add_2_2]
        );
        assert_search!(pattern("(add ?x (add ?x ?y))").ematch(&egraph.take(v1_2)), vec![]);
        assert_search!(
            pattern("(add ?x (add ?y ?z))").ematch(&egraph.take(v1_2)),
            vec![add_3_add_1_2, add_4_add_1_2, add_5_add_2_2]
        );
    }

    // TODO port all tests from naive.rs
    #[test]
    fn test_veg_search_version_independence() {
        search_version_independence(crate::structures::egraph::versioned::basic::VersionedEGraph::<()>::with_ematching_caches(10))
    }

    #[test]
    fn test_basic_search() {
        search(crate::structures::egraph::basic::EGraph::<()>::with_ematching_cache());
    }
}
