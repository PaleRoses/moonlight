use crate::structures::egraph::{EClass, EGraph, EGraphView, ENode, Id, Operator};
use crate::structures::versiontree::{Version, VersionTree};
use crate::structures::{Set, Map};

pub mod query {
    use super::*;

    /// A [Query] that binds to any [EClass]. [Var]s with the same identifier
    /// will be bound to the same [EClass].
    #[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct Var {
        pub id: Id,
    }
    impl std::fmt::Display for Var {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "?{:?}", self.id)
        }
    }

    /// A recursive [Query] that binds to any [EClass] for which this [Pattern]
    /// can unify with any [ENode] of that [EClass].
    #[derive(Clone, Debug)]
    pub struct Pattern {
        pub op: Operator,
        pub subqueries: Vec<Query>,
    }
    impl Pattern {
        pub fn reify(&self) -> Option<ENode> {
            let mut enode = ENode::application(self.op, vec![]);
            for subquery in self.subqueries.iter() {
                if let Query::EClass(argi) = subquery {
                    enode.args.push(*argi);
                } else {
                    return None;
                }
            }
            Some(enode)
        }
        pub fn patterns(&self) -> Vec<&Pattern> {
            fn aux<'a>(pattern: &'a Pattern, patterns: &mut Vec<&'a Pattern>) {
                patterns.push(pattern);
                for subquery in pattern.subqueries.iter() {
                    if let Query::Pattern(subpattern) = subquery {
                        aux(subpattern, patterns)
                    }
                }
            }
            let mut patterns = vec![];
            aux(self, &mut patterns);
            patterns
        }
        pub fn vars(&self) -> Set<&Var> {
            fn aux<'a>(pattern: &'a Pattern, vars: &mut Set<&'a Var>) {
                for subquery in pattern.subqueries.iter() {
                    match subquery {
                        Query::Var(var) => {
                            vars.insert(var);
                        },
                        Query::Pattern(subpattern) => aux(subpattern, vars),
                        _ => {},
                    }
                }
            }
            let mut vars = Default::default();
            aux(self, &mut vars);
            vars
        }
    }
    impl std::fmt::Display for Pattern {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            let substrings =
                self.subqueries.iter().map(|sq| sq.to_string()).collect::<Vec<_>>().join(",");
            write!(f, "{}({})", self.op, substrings)
        }
    }

    /// A query for extracting information from an [EGraph].
    #[derive(Clone, Default, Debug)]
    pub enum Query {
        /// A [Query] that matches against nothing.
        #[default]
        Nothing,
        /// A [Query] that matches against a specific [EClass].
        EClass(EClass),
        /// A [Query] that matches against any [EClass].
        Var(Var),
        /// A [Query] that matches against a [Pattern].
        Pattern(Pattern),
    }
    impl Query {
        /// #### Returns
        /// True if this [Query] is [Query::Nothing], false otherwise.
        #[inline(always)]
        pub fn is_nothing(&self) -> bool {
            matches!(self, Query::Nothing)
        }
        /// Try to cast this [Query] to [Query::EClass].
        #[inline(always)]
        pub fn as_eclass(&self) -> Option<&EClass> {
            match self {
                Query::EClass(eclass) => Some(eclass),
                _ => None,
            }
        }
        /// Try to cast this [Query] to [Query::Var].
        #[inline(always)]
        pub fn as_var(&self) -> Option<&Var> {
            match self {
                Query::Var(var) => Some(var),
                _ => None,
            }
        }
        /// Try to cast this [Query] to [Query::Pattern].
        #[inline(always)]
        pub fn as_pattern(&self) -> Option<&Pattern> {
            match self {
                Query::Pattern(pattern) => Some(pattern),
                _ => None,
            }
        }
    }
    impl std::fmt::Display for Query {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            let string: String = match self {
                Query::Nothing => "⊥".to_string(),
                Query::EClass(x) => x.to_string(),
                Query::Var(var) => var.to_string(),
                Query::Pattern(pattern) => pattern.to_string(),
            };
            write!(f, "{}", string)
        }
    }

    /// A query for applying a rewrite into an [EGraph].
    ///
    /// For all e-matches of `lhs`, the query applies the same substitutions to
    /// `rhs` , adds `rhs` to the [EGraph], and merges the [EClass]es of `lhs`
    /// and `rhs`.
    pub struct RewriteQuery {
        pub lhs: Query,
        pub rhs: Query,
    }
    impl std::fmt::Display for RewriteQuery {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "{} => {}", self.lhs, self.rhs)
        }
    }

    /// Utilities for writing [Query]s.
    pub mod language {
        pub use super::*;

        /// Creates a [Query::EClass].
        #[macro_export]
        macro_rules! eclass {
            ($x: expr) => {{
                let eclass: EClass = $x;
                Query::EClass(eclass)
            }};
        }

        /// Creates a [Query::Pattern].
        #[macro_export]
        macro_rules! pattern {
            ($op: expr, $($subqueries: expr),*) => {{
                let op: Operator = $op;
                let subqueries: Vec<Query> = vec![$($subqueries.clone()),*];
                Query::Pattern(Pattern { op, subqueries })
            }};
        }

        /// Creates a [Query] whose [Var]s are inferred at compile-time. This is
        /// the intended way to inject [Var]s into other [Query]s.
        /// ```
        /// use veg::structures::egraph::{Operator, ematching::naive::query::{*, language::*}};
        /// let add: Operator = Operator::from(0);
        /// let add_X_Y: Query = forall(|[X,Y]| pattern!(add, X, Y));
        /// let add_comm: RewriteQuery = forall(|[X,Y]| rewrite!(pattern!(add, X, Y) => pattern!(add, Y, X)));
        /// ```
        #[inline(always)]
        pub fn forall<const VARS: usize, F: FnOnce([Query; VARS]) -> Q, Q>(query: F) -> Q {
            query(core::array::from_fn(|id| Query::Var(Var { id: Id::from(id) })))
        }

        /// Creates a [RewriteQuery].
        #[macro_export]
        macro_rules! rewrite {
            ($lhs: expr => $rhs: expr) => {{
                let lhs: Query = $lhs.clone();
                let rhs: Query = $rhs.clone();
                RewriteQuery { lhs, rhs }
            }};
        }
        pub use eclass;
        pub use pattern;
        pub use rewrite;

        pub mod shorthands {
            /// Alias for [super::eclass].
            #[macro_export]
            macro_rules! C {
                ($x: expr) => {{
                    $crate::structures::egraph::ematching::naive::query::language::eclass!($x)
                }};
            }
            /// Alias for [super::pattern].
            #[macro_export]
            macro_rules! P {
                ($op: expr, $($subqueries: expr),*) => {{
                    $crate::structures::egraph::ematching::naive::query::language::pattern!($op, $($subqueries),*)
                }};
            }
            /// Alias for [super::rewrite].
            #[macro_export]
            macro_rules! R {
                ($lhs: expr => $rhs: expr) => {{
                    $crate::structures::egraph::ematching::naive::query::language::rewrite!($lhs => $rhs)
                }};
            }
            pub use C;
            pub use P;
            pub use R;
        }
    }
}

pub mod engine {
    use crate::util::id::AsIndex;

    use super::query::*;
    use super::*;

    #[derive(Default, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub(crate) struct OptimizedBinding {
        assignments: Vec<Option<EClass>>,
        size: usize,
    }
    impl OptimizedBinding {
        pub(crate) fn new(capacity: usize) -> OptimizedBinding {
            OptimizedBinding { 
                assignments: vec![None; capacity],
                size: 0,
            }
        }
        pub(crate) fn into_binding(self, vars: &[Var]) -> Binding {
            let mut bindings: Binding = Binding::default();
            for (i, var) in vars.iter().enumerate() {
                if let Some(value) = self.assignments[i] {
                    bindings.bind(var, value);
                }
            }
            bindings
        }
        #[inline(always)]
        pub(crate) fn is_empty(&self) -> bool {
            self.size == 0
        }
        #[inline(always)]
        pub(crate) fn len(&self) -> usize {
            self.size
        }
        pub(crate) fn merge(&self, other: &OptimizedBinding) -> OptimizedBinding {
            let mut merged: OptimizedBinding = self.clone();
            for (index, value) in other.assignments.iter().enumerate() {
                if value.is_some() {
                    merged.assignments[index] = *value;
                }
            }
            merged
        }
        #[inline(always)]
        pub(crate) fn bind(&mut self, index: usize, value: EClass) {
            if self.assignments[index].is_none() { self.size += 1 }
            self.assignments[index] = Some(value);
        }
    }

    /// A mapping from [Var]s to their bound [EClass] value.
    #[derive(Clone, Debug, PartialEq, Eq, Default)]
    pub struct Binding {
        pub assignments: Map<Var, EClass>,
    }
    impl Binding {
        /// #### Returns
        /// True if this [Binding] contains no assignments, false otherwise.
        #[inline(always)]
        pub fn is_empty(&self) -> bool {
            self.assignments.is_empty()
        }
        /// #### Returns
        /// The number of variables assigned in this [Binding].
        #[inline(always)]
        pub fn len(&self) -> usize {
            self.assignments.len()
        }
        /// #### Returns
        /// A new [Binding] obtained by mapping all variables in both `self` and
        /// `other` to their corresponding value in either `self` or `other`, giving
        /// priority to `other` in case of conflicts.
        pub fn merge(&self, other: &Binding) -> Binding {
            let mut merged: Binding = self.clone();
            for (var, value) in other.assignments.iter() {
                merged.bind(var, *value);
            }
            merged
        }
        /// #### Description
        /// Bind the specified [Var] `var` to the specified [EClass] `value`.
        #[inline(always)]
        pub fn bind(&mut self, var: &Var, value: EClass) {
            self.assignments.insert(*var, value);
        }
    }
    impl std::fmt::Display for Binding {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(
                f,
                "[{}]",
                self.assignments
                    .iter()
                    .map(|(v, x)| format!("{}:={}", v, x))
                    .collect::<Vec<_>>()
                    .join(",")
            )
        }
    }

    /// The result of searching for a [Query], including the [Binding] of the
    /// e-match `binding` and the matched [EClass] `eclass`.
    #[derive(Debug, Clone, Default)]
    pub struct SearchResult {
        pub binding: Binding,
        pub eclass: EClass,
    }
    impl std::fmt::Display for SearchResult {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "{}{{{}}}", self.binding, self.eclass)
        }
    }

    /// An engine for processing [Query]s and [RewriteQuery]s.
    pub trait EngineView {
        /// #### Return
        /// A list of all [SearchResult]s obtained by e-matching the specified
        /// [Query] `query`.
        fn search(&self, query: &Query) -> Vec<SearchResult>;
    }
    pub trait Engine: EngineView {
        /// #### Description
        /// Add the specified [Query] `query` in the context of the provided
        /// [Binding] `bindings`.
        /// #### Return
        /// The [EClass] of the added `query`.
        fn add_query(&mut self, query: &Query, bindings: &Binding) -> EClass;
        /// #### Description
        /// Union the specified [Query] `query` to the specified [EClass]
        /// `eclass`, in the context of the provided [Binding] `bindings`.
        fn apply(&mut self, query: &Query, bindings: &Binding, eclass: EClass);
        /// ### Description
        /// Perform the specified [RewriteQuery] `rewrite`.
        fn rewrite(&mut self, rewrite: &RewriteQuery);
        /// ### Description
        /// Perform the specified [RewriteQuery]s `rewrites`, in order.
        fn rewrites(&mut self, rewrites: &[RewriteQuery]);
        /// ### Description
        /// Iteratively perform the specified [RewriteQuery]s `rewrites`, in
        /// order. Saturation is stopped whenever the `termination` condition is
        /// satisfied (i.e. returns true).
        fn saturate<F>(&mut self, rewrites: &[RewriteQuery], termination: F)
        where
            F: FnMut(&mut Self) -> bool;
    }

    /// An [EGraphView] which provides information about the [Operator]s of its
    /// [ENode]s. Implementing this trait provides a default implementation of
    /// [Engine] for free.
    pub trait HasOperatorStatistics: EGraphView {
        /// #### Return
        /// The [ENode]s that use the specified [Operator] `op` in this
        /// [EGraphView].
        fn enodes_of_op(
            &self,
            op: Operator,
        ) -> Option<impl std::iter::Iterator<Item = (&ENode, &EClass)>>;

        /// #### Return
        /// The number of [ENode]s that use the specified [Operator] `op` in
        /// this [EGraphView].
        fn frequency_of_op(&self, op: Operator) -> usize;
    }

    impl<G: EGraphView + HasOperatorStatistics> EngineView for G {
        fn search(&self, query: &Query) -> Vec<SearchResult> {
            let mut query = query.clone();
            bind(self, &mut query, &OptimizedBinding::default());
            match query {
                Query::Nothing => vec![],
                Query::EClass(eclass) => {
                    vec![SearchResult { binding: Binding::default(), eclass }]
                },
                Query::Var(var) =>
                    self
                        .universe()
                        .map(|eclass| {
                            let mut result = SearchResult { binding: Binding::default(), eclass };
                            result.binding.bind(&var, eclass);
                            result
                        })
                        .collect(),
                Query::Pattern(ref mut pattern) => {
                    let mut vars: Vec<Var> = vec![];
                    index_vars(pattern, &mut vars);
                    if let Some(x) = pattern.reify().and_then(|xn| self.get_class(&xn)) {
                        vec![SearchResult { binding: Binding::default(), eclass: self.find(*x) }]
                    } else {
                        search_patterns(self, pattern.patterns(), &vars)
                    }
                },
            }
        }
    }
    impl<G: EGraph + HasOperatorStatistics> Engine for G {
        fn add_query(&mut self, query: &Query, bindings: &Binding) -> EClass {
            let mut query = query.clone();
            bind_add(self, &mut query, bindings);
            if let Query::EClass(xc) = query { 
                xc
            } else {
                panic!("Could not add query {query}: not an eclass after binding")
            }
        }
        fn apply(&mut self, query: &Query, bindings: &Binding, eclass: EClass) {
            let mut query = query.clone();
            bind_add(self, &mut query, bindings);
            if let Query::EClass(rc) = query {
                self.union(eclass, rc);
            }
        }
        fn rewrite(&mut self, rewrite: &RewriteQuery) {
            let RewriteQuery { lhs, rhs } = rewrite;
            for search_result in self.search(lhs) {
                let lc = search_result.eclass;
                self.apply(rhs, &search_result.binding, lc);
            }
            self.rebuild();
        }
        fn rewrites(&mut self, rewrites: &[RewriteQuery]) {
            let mut writes: Vec<(Query, SearchResult)> = vec![];
            for RewriteQuery { lhs, rhs } in rewrites {
                for search_result in self.search(lhs) {
                    writes.push((rhs.clone(), search_result));
                }
            }
            for (mut rhs, search_result) in writes.into_iter() {
                let lc = search_result.eclass;
                bind_add(self, &mut rhs, &search_result.binding);
                if let Query::EClass(rc) = rhs {
                    self.union(lc, rc);
                }
            }
        }
        fn saturate<F>(&mut self, rewrites: &[RewriteQuery], mut termination: F)
        where
            F: FnMut(&mut Self) -> bool,
        {
            while {
                self.rewrites(rewrites);
                !termination(self)
            } {}
        }
    }

    fn index_vars(pattern: &mut Pattern, vars: &mut Vec<Var>) {
        for subquery in pattern.subqueries.iter_mut() {
            match subquery {
                Query::Var(var) => {
                    if let Some(rename) = vars.iter().position(|v| v == var) {
                        var.id = Id::from(rename);
                    } else {
                        let rename = vars.len();
                        vars.push(*var);
                        var.id = Id::from(rename);
                    }
                },
                Query::Pattern(pattern) => {
                    index_vars(pattern, vars);
                }
                _ => {}
            }
        }
    }

    fn bind<G: EGraphView + HasOperatorStatistics>(
        egraph: &G,
        q: &mut Query,
        subst: &OptimizedBinding,
    ) {
        match q {
            Query::Nothing => {},
            Query::EClass(_) => {},
            Query::Var(Var { id }) => {
                if let Some(value) = subst.assignments.get(id.to_i()).and_then(|x| *x) {
                    *q = Query::EClass(value)
                }
            },
            Query::Pattern(ref mut pattern) => {
                let mut ground = true;
                for subquery in pattern.subqueries.iter_mut() {
                    bind(egraph, subquery, subst);
                    match subquery {
                        Query::Nothing => {
                            *q = Query::Nothing;
                            return;
                        },
                        Query::Pattern(_) | Query::Var(_) => ground = false,
                        _ => {},
                    }
                }
                if ground {
                    pattern.reify().into_iter().for_each(|xn| match egraph.get_class(&xn) {
                        Some(x) => *q = Query::EClass(egraph.find(*x)),
                        None => *q = Query::Nothing,
                    });
                }
            },
        };
    }
    // TODO refactor: this is the same as `bind`, except patterns are added to the
    //      egraphs (i.e. reified to `EClass`es) instead of being reified to `ENode`s
    fn bind_add<G: EGraph + HasOperatorStatistics>(
        egraph: &mut G,
        q: &mut Query,
        subst: &Binding,
    ) {
        match q {
            Query::Nothing => {},
            Query::EClass(_) => {},
            Query::Var(var) => {
                if let Some(value) = subst.assignments.get(var) {
                    *q = Query::EClass(*value)
                }
            },
            Query::Pattern(ref mut pattern) => {
                let mut ground = true;
                for subquery in pattern.subqueries.iter_mut() {
                    bind_add(egraph, subquery, subst);
                    match subquery {
                        Query::Nothing => {
                            *q = Query::Nothing;
                            return;
                        },
                        Query::Pattern(_) | Query::Var(_) => ground = false,
                        _ => {},
                    }
                }
                if ground {
                    pattern
                        .reify()
                        .into_iter()
                        .for_each(|xn| *q = Query::EClass(egraph.add(xn)));
                }
            },
        };
    }
    fn search_patterns<G: EGraphView + HasOperatorStatistics>(
        egraph: &G,
        mut patterns: Vec<&Pattern>,
        vars: &[Var],
    ) -> Vec<SearchResult> {
        // Sort the pattern by increasing frequency (we want to look at the
        // least frequent ones first to reduce the size of the upper-bound on
        // possible candidates). If the minimum frequency is 0, then there
        // exist a pattern that matches no e-nodes, so there are no candidates
        let searcher = patterns[0];

        patterns.sort_by_key(|p| egraph.frequency_of_op(p.op));
        if patterns.is_empty() || egraph.frequency_of_op(patterns[0].op) == 0 {
            return vec![];
        }

        let mut candidates: Set<OptimizedBinding> = Default::default();
        let mut search_results: Vec<SearchResult> = Default::default();
        let mut var_coverage: Set<Var> = Default::default();

        // For all patterns...
        for pattern in patterns.into_iter() {
            let Pattern { op, subqueries, .. } = pattern;

            // Look at the variables inside the pattern to update the current coverage
            let mut bindable_vars: Map<Var, Vec<usize>> = Default::default();
            for (position, subquery) in subqueries.iter().enumerate() {
                if let Query::Var(var) = *subquery {
                    bindable_vars.entry(var).or_default().push(position);
                    var_coverage.insert(var);
                }
            }

            // Look at all e-nodes that share the same operator as the pattern
            // (note: `unwrap` is fine because the frequency is always > 0 here)
            let mut candidate_diff: Set<OptimizedBinding> = Default::default();
            for (selectable, _) in egraph.enodes_of_op(*op).unwrap() {
                let mut binding = OptimizedBinding::new(vars.len());
                for (var, positions) in bindable_vars.iter() {
                    let Some(arg_truth) = selectable.args.get(positions[0]) else { continue; };
                    positions
                        .iter()
                        .skip(1)
                        .all(|i| selectable.args.get(positions[*i]).is_some_and(|x| x == arg_truth))
                        .then(|| binding.bind(var.id.into(), egraph.find(*arg_truth)));
                }
                if !binding.is_empty() {
                    candidate_diff.insert(binding);
                }
            }

            // Set the new candidates or evaluate all possible combinations
            // between the current candidates and the previous
            if candidates.is_empty() {
                candidates = candidate_diff;
            } else {
                candidates = std::mem::take(&mut candidates)
                    .into_iter()
                    .flat_map(|b1| candidate_diff.iter().map(move |b2| b1.merge(b2)))
                    .collect();
            }

            // If there is an assignment for all variables, we found an
            // upper-bound on the candidates for e-matching, and return it
            if var_coverage.len() == vars.len() {
                break;
            }
        }

        // Filter the upper-bound on candidates by substituting candidates and
        // verifying the existence of their e-nodes in the e-graph
        for candidate in std::mem::take(&mut candidates).into_iter() {
            let mut search_query = Query::Pattern(searcher.clone());
            bind(egraph, &mut search_query, &candidate);
            if let Query::EClass(x) = search_query {
                search_results.push(SearchResult { binding: candidate.into_binding(vars), eclass: x });
            }
        }
        search_results
    }
}

#[rustfmt::skip]
#[allow(non_snake_case)]
#[allow(clippy::just_underscores_and_digits)] 
pub mod testing {
    use super::*;
    use super::query::language::{*, shorthands::{P, R, C}};
    use crate::structures::egraph::ematching::naive::engine::{EngineView, Engine, HasOperatorStatistics};
    use crate::structures::egraph::versioned::VersionedEGraph;
    use crate::structures::unionfind::testing::{assert_classes, set};
    use crate::util::testing::*;

    macro_rules! assert_search {
        ($search: expr, $expected: expr) => {{
            let expected: Vec<EClass> = $expected;
            let actual: Vec<EClass> = ($search).into_iter().map(|r| r.eclass).collect::<Vec<_>>();
            assert_eq_by!(eqs::vec::no_order, &actual, &expected);
        }};
    }

    pub fn enodes_of_op<G: EGraph + HasOperatorStatistics + Default>(){
        let mut egraph: G = G::default();
        let [x, y, z] = [0,1,2].map(|i| egraph.add(ENode::constant(Operator::from(i))));
        let f: Operator = Operator::from(3);
        let [fx, fy, fz] = [x,y,z].map(|i| egraph.add(ENode::application(f, vec![i])));
        assert!(egraph.enodes_of_op(f).is_some());
        assert_eq_by!(eqs::vec::no_order, &[fx, fy, fz], &egraph.enodes_of_op(f).unwrap().map(|x| x.1).cloned().collect::<Vec<_>>());
        assert!(egraph.frequency_of_op(f) == 3);
    }
    pub fn search<G: EGraph + Engine + Default>() {
        let mut egraph: G = G::default();
        let [_0, _1, _2] = [0,1,2].map(|num| egraph.add(ENode::constant(Operator::from(num))));
        let add: Operator = Operator::from(3);
        let add_0_1 = egraph.add(ENode::application(add, vec![_0, _1]));
        let add_1_2 = egraph.add(ENode::application(add, vec![_1, _2]));
        let add_2_2 = egraph.add(ENode::application(add, vec![_2, _2]));
        let add_0_add_1_2 = egraph.add(ENode::application(add, vec![_0, add_1_2]));
        let add_1_add_1_2 = egraph.add(ENode::application(add, vec![_1, add_1_2]));
        let add_2_add_2_2 = egraph.add(ENode::application(add, vec![_2, add_2_2]));

        assert_search!(egraph.search(&forall(|[]| C!(_0))), vec![_0]);
        assert_search!(egraph.search(&forall(|[]| P!(_0,))), vec![_0]);
        assert_search!(egraph.search(&forall(|[]| P!(add, C!(_2), C!(_2)))), vec![add_2_2]);
        assert_search!(egraph.search(&forall(|[X]| X)), vec![_0, _1, _2, add_0_1, add_1_2, add_2_2, add_0_add_1_2, add_1_add_1_2, add_2_add_2_2]);
        assert_search!(egraph.search(&forall(|[X]| P!(add, X, X))), vec![add_2_2]);
        assert_search!(egraph.search(&forall(|[X]| P!(add, X, C!(_2)))), vec![add_1_2, add_2_2]);
        assert_search!(egraph.search(&forall(|[X]| P!(add, X, P!(_2,)))), vec![add_1_2, add_2_2]);
        assert_search!(egraph.search(&forall(|[X, Y]| P!(add, X, Y))), vec![add_0_1, add_1_2, add_2_2, add_0_add_1_2, add_1_add_1_2, add_2_add_2_2]);
        assert_search!(egraph.search(&forall(|[X, Y]| P!(add, X, P!(add, Y, Y)))), vec![add_2_add_2_2]);
        assert_search!(egraph.search(&forall(|[X, Y]| P!(add, X, P!(add, X, Y)))), vec![add_1_add_1_2, add_2_add_2_2]);
        assert_search!(egraph.search(&forall(|[X, Y, Z]| P!(add, X, P!(add, Y, Z)))), vec![add_0_add_1_2, add_1_add_1_2, add_2_add_2_2]);
    }

    pub fn rewrite<G: EGraph + Engine + Default>() {
        let mut egraph: G = G::default();
        let [_0, _1, _2] = [0,1,2].map(|num| egraph.add(ENode::constant(Operator::from(num))));
        let add: Operator = Operator::from(3);
        let add_0_1 = egraph.add(ENode::application(add, vec![_0, _1]));
        let add_1_2 = egraph.add(ENode::application(add, vec![_1, _2]));
        let add_2_2 = egraph.add(ENode::application(add, vec![_2, _2]));
        let add_0_add_1_2 = egraph.add(ENode::application(add, vec![_0, add_1_2]));
        let add_1_add_1_2 = egraph.add(ENode::application(add, vec![_1, add_1_2]));
        let add_2_add_2_2 = egraph.add(ENode::application(add, vec![_2, add_2_2]));

        let add_comm = forall(|[X, Y]| R!(P!(add, X, Y) => P!(add, Y, X)));
        egraph.rewrite(&add_comm);
        let add_1_0 = *egraph.get_class(&ENode::application(add, vec![_1, _0])).expect("rewrite expected");
        let add_2_1 = *egraph.get_class(&ENode::application(add, vec![_2, _1])).expect("rewrite expected");
        let add_add_1_2_0 = *egraph.get_class(&ENode::application(add, vec![add_1_2, _0])).expect("rewrite expected");
        let add_add_1_2_1 = *egraph.get_class(&ENode::application(add, vec![add_1_2, _1])).expect("rewrite expected");
        assert_classes!(egraph, 
            set!(add_0_1; add_1_0),
            set!(add_1_2; add_2_1),
            set!(add_2_2),
            set!(add_0_add_1_2; add_add_1_2_0),
            set!(add_1_add_1_2; add_add_1_2_1),
            set!(add_2_add_2_2)
        );

        let add_assoc = forall(|[X, Y, Z]| R!(P!(add, X, P!(add, Y, Z)) => P!(add, P!(add, X, Y), Z)));
        egraph.rewrite(&add_assoc);
        let add_1_1 = *egraph.get_class(&ENode::application(add, vec![_1, _1])).expect("rewrite expected");
        let add_add_0_1_2 = *egraph.get_class(&ENode::application(add, vec![add_0_1, _2])).expect("rewrite expected");
        let add_add_1_1_2 = *egraph.get_class(&ENode::application(add, vec![add_1_1, _2])).expect("rewrite expected");
        assert_classes!(egraph, 
            set!(add_0_1; add_1_0),
            set!(add_1_2; add_2_1),
            set!(add_2_2),
            set!(add_0_add_1_2; add_add_1_2_0; add_add_0_1_2),
            set!(add_1_add_1_2; add_add_1_2_1; add_add_1_1_2),
            set!(add_2_add_2_2)
        );

        let add_identity = forall(|[X]| R!(P!(add, C!(_0), X) => X));
        egraph.rewrite(&add_identity);
        assert_classes!(egraph, 
            set!(add_0_1; add_1_0; _1),
            set!(add_1_2; add_2_1; add_0_add_1_2; add_add_1_2_0; add_add_0_1_2),
            set!(add_2_2),
            set!(add_1_add_1_2; add_add_1_2_1; add_add_1_1_2),
            set!(add_2_add_2_2)
        );
    }

    pub fn rewrites<G: EGraph + Engine + Default>() {
        let mut egraph: G = G::default();
        let [_0, _1, _2] = [0,1,2].map(|num| egraph.add(ENode::constant(Operator::from(num))));
        let add: Operator = Operator::from(3);
        let add_0_1 = egraph.add(ENode::application(add, vec![_0, _1]));
        let add_1_2 = egraph.add(ENode::application(add, vec![_1, _2]));
        let add_2_2 = egraph.add(ENode::application(add, vec![_2, _2]));
        let add_0_add_1_2 = egraph.add(ENode::application(add, vec![_0, add_1_2]));
        let add_1_add_1_2 = egraph.add(ENode::application(add, vec![_1, add_1_2]));
        let add_2_add_2_2 = egraph.add(ENode::application(add, vec![_2, add_2_2]));

        let add_comm = forall(|[X, Y]| R!(P!(add, X, Y) => P!(add, Y, X)));
        let add_assoc = forall(|[X, Y, Z]| R!(P!(add, X, P!(add, Y, Z)) => P!(add, P!(add, X, Y), Z)));
        let add_identity = forall(|[X]| R!(P!(add, C!(_0), X) => X));

        egraph.rewrites(&[add_comm, add_assoc, add_identity]);

        let add_1_0 = *egraph.get_class(&ENode::application(add, vec![_1, _0])).expect("rewrite expected");
        let add_2_1 = *egraph.get_class(&ENode::application(add, vec![_2, _1])).expect("rewrite expected");
        let add_add_1_2_0 = *egraph.get_class(&ENode::application(add, vec![add_1_2, _0])).expect("rewrite expected");
        let add_add_1_2_1 = *egraph.get_class(&ENode::application(add, vec![add_1_2, _1])).expect("rewrite expected");
        let add_1_1 = *egraph.get_class(&ENode::application(add, vec![_1, _1])).expect("rewrite expected");
        let add_add_0_1_2 = *egraph.get_class(&ENode::application(add, vec![add_0_1, _2])).expect("rewrite expected");
        let add_add_1_1_2 = *egraph.get_class(&ENode::application(add, vec![add_1_1, _2])).expect("rewrite expected");
        assert_classes!(egraph, 
            set!(add_0_1; add_1_0; _1),
            set!(add_1_2; add_2_1; add_0_add_1_2; add_add_1_2_0; add_add_0_1_2),
            set!(add_2_2),
            set!(add_1_add_1_2; add_add_1_2_1; add_add_1_1_2),
            set!(add_2_add_2_2)
        );
    }

    pub fn saturate<G: EGraph + Engine + Default>() {
        let mut egraph: G = G::default();
        let [_0, _1] = [0,1].map(|num| egraph.add(ENode::constant(Operator::from(num))));
        let add: Operator = Operator::from(2);

        const ITERATIONS: u8 = 100;
        let mut iterations = 0;
        egraph.saturate(&[
            forall(|[X]| R!(X => P!(add, X, C!(_0)))),
            forall(|[X, Y]| R!(P!(add, X, Y) => P!(add, Y, X))),
        ], |_| { iterations += 1; iterations > ITERATIONS });

        let mut add_1_0_100_times = _1;
        let mut add_0_1_100_times = _1;
        for _ in 0..ITERATIONS {
            add_1_0_100_times = egraph.add(ENode::application(add, vec![add_1_0_100_times, _0]));
            add_0_1_100_times = egraph.add(ENode::application(add, vec![_0, add_0_1_100_times]));
        }
        assert!(egraph.equal(add_1_0_100_times, add_0_1_100_times));   
    }

    /// #### Description
    /// Generate a test module for ematching using the specified [EGraph]
    /// implementation. The module will be named after `$module` and will test
    /// the implementation of the concrete type `$implementation`.
    #[macro_export]
    macro_rules! test_ematching {
        ($module: ident, $implementation: ty) => {
            #[cfg(test)]
            mod $module {
                use super::*;
                use $crate::structures::egraph::ematching::naive::testing;
                type Impl = $implementation;
                #[test]
                fn enodes_of_op() { testing::enodes_of_op::<Impl>(); }
                #[test]
                fn search() { testing::search::<Impl>(); }
                #[test]
                fn rewrite() { testing::rewrite::<Impl>(); }
                #[test]
                fn rewrites() { testing::rewrites::<Impl>(); }
                #[test]
                fn saturate() { testing::saturate::<Impl>(); }
            }
        };
    }
    pub use test_ematching;

    pub fn search_version_independence<G>()
    where G: VersionedEGraph + Default, for<'a> G::ProjectionMut<'a>: Engine {
        //! Searching in independent [Version]s should only e-match against [ENode]s that were added
        //! to the queried [Version].
        let mut egraph: G = G::default();
        let v1: Version = egraph.branchout(VersionTree::ROOT_VERSION);
        let [_0, _1, _2, _3, _4, _5] = [0,1,2,3,4,5].map(|num| egraph.take(v1).add(ENode::constant(Operator::from(num))));
        let add: Operator = Operator::from(6);
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

        assert_search!(egraph.take(v1_1).search(&forall(|[]| C!(_0))), vec![_0]);
        assert_search!(egraph.take(v1_1).search(&forall(|[]| P!(add, C!(_2), C!(_2)))), vec![add_2_2]);
        assert_search!(egraph.take(v1_1).search(&forall(|[X]| X)), vec![_0, _1, _2, _3, _4, _5, add_0_1, add_1_2, add_2_2, add_0_add_1_2, add_1_add_1_2, add_2_add_2_2]);
        assert_search!(egraph.take(v1_1).search(&forall(|[X]| P!(add, X, X))), vec![add_2_2]);
        assert_search!(egraph.take(v1_1).search(&forall(|[X, Y]| P!(add, X, Y))), vec![add_0_1, add_1_2, add_2_2, add_0_add_1_2, add_1_add_1_2, add_2_add_2_2]);
        assert_search!(egraph.take(v1_1).search(&forall(|[X, Y]| P!(add, X, P!(add, Y, Y)))), vec![add_2_add_2_2]);
        assert_search!(egraph.take(v1_1).search(&forall(|[X, Y]| P!(add, X, P!(add, X, Y)))), vec![add_1_add_1_2, add_2_add_2_2]);
        assert_search!(egraph.take(v1_1).search(&forall(|[X, Y, Z]| P!(add, X, P!(add, Y, Z)))), vec![add_0_add_1_2, add_1_add_1_2, add_2_add_2_2]);
        
        assert_search!(egraph.take(v1_2).search(&forall(|[]| C!(_0))), vec![_0]);
        assert_search!(egraph.take(v1_2).search(&forall(|[]| P!(add, C!(_2), C!(_2)))), vec![add_2_2]);
        assert_search!(egraph.take(v1_2).search(&forall(|[X]| X)), vec![_0, _1, _2, _3, _4, _5, add_0_1, add_1_2, add_2_2, add_3_add_1_2, add_4_add_1_2, add_5_add_2_2]);
        assert_search!(egraph.take(v1_2).search(&forall(|[X]| P!(add, X, X))), vec![add_2_2]);
        assert_search!(egraph.take(v1_2).search(&forall(|[X, Y]| P!(add, X, Y))), vec![add_0_1, add_1_2, add_2_2, add_3_add_1_2, add_4_add_1_2, add_5_add_2_2]);
        assert_search!(egraph.take(v1_2).search(&forall(|[X, Y]| P!(add, X, P!(add, Y, Y)))), vec![add_5_add_2_2]);
        assert_search!(egraph.take(v1_2).search(&forall(|[X, Y]| P!(add, X, P!(add, X, Y)))), vec![]);
        assert_search!(egraph.take(v1_2).search(&forall(|[X, Y, Z]| P!(add, X, P!(add, Y, Z)))), vec![add_3_add_1_2, add_4_add_1_2, add_5_add_2_2]);
    }

    pub fn search_version_inheritance<G>()
    where G: VersionedEGraph + Default, for<'a> G::ProjectionMut<'a>: Engine {
        //! Searching in a [Version]s should also e-match against [ENode]s that were added to its
        //! ancestor [Version]s.
        let mut egraph: G = G::default();
        let v1: Version = egraph.branchout(VersionTree::ROOT_VERSION);
        let [_0, _1, _2] = [0,1,2].map(|num| egraph.take(v1).add(ENode::constant(Operator::from(num))));
        let add: Operator = Operator::from(3);
        let add_0_1 = egraph.take(v1).add(ENode::application(add, vec![_0, _1]));
        let add_1_2 = egraph.take(v1).add(ENode::application(add, vec![_1, _2]));
        let add_2_2 = egraph.take(v1).add(ENode::application(add, vec![_2, _2]));

        // Inheritance at creation
        let v1_1: Version = egraph.branchout(v1);
        let add_0_add_1_2 = egraph.take(v1_1).add(ENode::application(add, vec![_0, add_1_2]));
        let add_1_add_1_2 = egraph.take(v1_1).add(ENode::application(add, vec![_1, add_1_2]));
        let add_2_add_2_2 = egraph.take(v1_1).add(ENode::application(add, vec![_2, add_2_2]));
        assert_search!(egraph.take(v1_1).search(&forall(|[]| C!(_0))), vec![_0]);
        assert_search!(egraph.take(v1_1).search(&forall(|[]| P!(add, C!(_2), C!(_2)))), vec![add_2_2]);
        assert_search!(egraph.take(v1_1).search(&forall(|[X]| X)), vec![_0, _1, _2, add_0_1, add_1_2, add_2_2, add_0_add_1_2, add_1_add_1_2, add_2_add_2_2]);
        assert_search!(egraph.take(v1_1).search(&forall(|[X]| P!(add, X, X))), vec![add_2_2]);
        assert_search!(egraph.take(v1_1).search(&forall(|[X, Y]| P!(add, X, Y))), vec![add_0_1, add_1_2, add_2_2, add_0_add_1_2, add_1_add_1_2, add_2_add_2_2]);
        assert_search!(egraph.take(v1_1).search(&forall(|[X, Y]| P!(add, X, P!(add, Y, Y)))), vec![add_2_add_2_2]);
        assert_search!(egraph.take(v1_1).search(&forall(|[X, Y]| P!(add, X, P!(add, X, Y)))), vec![add_1_add_1_2, add_2_add_2_2]);
        assert_search!(egraph.take(v1_1).search(&forall(|[X, Y, Z]| P!(add, X, P!(add, Y, Z)))), vec![add_0_add_1_2, add_1_add_1_2, add_2_add_2_2]);

        // Inheritance after creation
        egraph.flags_mut().lock_superversions = false;
        egraph.flags_mut().propagate = true;
        let v1_2: Version = egraph.branchout(v1);
        let add_0_add_1_2 = egraph.take(v1).add(ENode::application(add, vec![_0, add_1_2]));
        let add_1_add_1_2 = egraph.take(v1).add(ENode::application(add, vec![_1, add_1_2]));
        let add_2_add_2_2 = egraph.take(v1).add(ENode::application(add, vec![_2, add_2_2]));
        assert_search!(egraph.take(v1_2).search(&forall(|[]| C!(_0))), vec![_0]);
        assert_search!(egraph.take(v1_2).search(&forall(|[]| P!(add, C!(_2), C!(_2)))), vec![add_2_2]);
        assert_search!(egraph.take(v1_2).search(&forall(|[X]| X)), vec![_0, _1, _2, add_0_1, add_1_2, add_2_2, add_0_add_1_2, add_1_add_1_2, add_2_add_2_2]);
        assert_search!(egraph.take(v1_2).search(&forall(|[X]| P!(add, X, X))), vec![add_2_2]);
        assert_search!(egraph.take(v1_2).search(&forall(|[X, Y]| P!(add, X, Y))), vec![add_0_1, add_1_2, add_2_2, add_0_add_1_2, add_1_add_1_2, add_2_add_2_2]);
        assert_search!(egraph.take(v1_2).search(&forall(|[X, Y]| P!(add, X, P!(add, Y, Y)))), vec![add_2_add_2_2]);
        assert_search!(egraph.take(v1_2).search(&forall(|[X, Y]| P!(add, X, P!(add, X, Y)))), vec![add_1_add_1_2, add_2_add_2_2]);
        assert_search!(egraph.take(v1_2).search(&forall(|[X, Y, Z]| P!(add, X, P!(add, Y, Z)))), vec![add_0_add_1_2, add_1_add_1_2, add_2_add_2_2]);
    }

    /// #### Description
    /// Generate a test module for ematching using the specified [VersionedEGraph]
    /// implementation. The module will be named after `$module` and will test
    /// the implementation of the concrete type `$implementation`.
    #[macro_export]
    macro_rules! test_versioned_ematching {
        ($module: ident, $implementation: ty) => {
            #[cfg(test)]
            mod $module {
                use super::*;
                use $crate::structures::egraph::ematching::naive::testing;
                type Impl = $implementation;
                #[test]
                fn search_version_independence() { testing::search_version_independence::<Impl>(); }
                #[test]
                fn search_version_inheritance() { testing::search_version_inheritance::<Impl>(); }
            }
        };
    }
    pub use test_versioned_ematching;
}
