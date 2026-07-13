use easter_egg::{ColorId, EGraph, Id, SymbolLang};
use std::{
    collections::BTreeSet,
    env,
    error::Error,
    fmt, fs,
    path::{Path, PathBuf},
    process::ExitCode,
    str::FromStr,
    time::{Duration, Instant},
};
use symbolic_expressions::{parser, Sexp};

fn main() -> ExitCode {
    match Config::from_args(env::args().skip(1)).and_then(run) {
        Ok(()) => ExitCode::SUCCESS,
        Err(BenchError::HelpRequested) => {
            println!("{}", Config::usage());
            ExitCode::SUCCESS
        }
        Err(error) => {
            eprintln!("{error}");
            eprintln!("{}", Config::usage());
            ExitCode::from(2)
        }
    }
}

fn run(config: Config) -> Result<(), BenchError> {
    let rows = case_specs(&config).into_iter().try_fold(
        Vec::<MeasurementRow>::new(),
        |rows, case_spec| {
            let corpus = read_thesy_suite_corpus(&config.fixture_dir, case_spec.suite)?;
            (0..config.trials).try_fold(rows, |mut current_rows, trial_index| {
                current_rows.push(measure_case(&corpus, case_spec, trial_index)?);
                Ok::<Vec<MeasurementRow>, BenchError>(current_rows)
            })
        },
    )?;

    println!("{}", MeasurementRow::csv_header());
    rows.iter().for_each(|row| println!("{}", row.to_csv()));
    Ok(())
}

fn case_specs(config: &Config) -> Vec<CaseSpec> {
    config
        .suites
        .iter()
        .flat_map(|suite| {
            config.colors.iter().flat_map(move |color_count| {
                config.terms.iter().map(move |term_count| CaseSpec {
                    suite: *suite,
                    color_count: *color_count,
                    term_count: *term_count,
                    mode: config.mode,
                })
            })
        })
        .collect()
}

#[derive(Clone, Debug)]
struct Config {
    fixture_dir: PathBuf,
    suites: Vec<ThesySuiteName>,
    colors: Vec<usize>,
    terms: Vec<usize>,
    mode: BuildMode,
    trials: usize,
}

impl Default for Config {
    fn default() -> Self {
        Self {
            fixture_dir: PathBuf::from(env!("CARGO_MANIFEST_DIR"))
                .join("../../bench/fixtures/thesy-cvc4"),
            suites: vec![ThesySuiteName::HipspecRevEquiv],
            colors: vec![1, 2, 4, 8, 16, 32],
            terms: vec![100_000],
            mode: BuildMode::ColorsOnly,
            trials: 1,
        }
    }
}

impl Config {
    fn from_args(args: impl IntoIterator<Item = String>) -> Result<Self, BenchError> {
        args.into_iter().try_fold(Config::default(), |config, arg| {
            if arg == "--help" || arg == "-h" {
                return Err(BenchError::HelpRequested);
            }

            let (key, value) = arg
                .strip_prefix("--")
                .and_then(|body| body.split_once('='))
                .ok_or_else(|| BenchError::InvalidArgument(arg.clone()))?;

            match key {
                "fixture-dir" => Ok(Self {
                    fixture_dir: PathBuf::from(value),
                    ..config
                }),
                "suites" => Ok(Self {
                    suites: parse_suite_list(value)?,
                    ..config
                }),
                "colors" => Ok(Self {
                    colors: parse_usize_list("colors", value)?,
                    ..config
                }),
                "terms" => Ok(Self {
                    terms: parse_usize_list("terms", value)?,
                    ..config
                }),
                "mode" => Ok(Self {
                    mode: BuildMode::from_str(value)?,
                    ..config
                }),
                "trials" => Ok(Self {
                    trials: parse_positive_usize("trials", value)?,
                    ..config
                }),
                _ => Err(BenchError::UnknownArgument(key.to_owned())),
            }
        })
    }

    fn usage() -> &'static str {
        "usage: moonlight-easter-egg-colored-bench [--fixture-dir=PATH] [--suites=all|hipspec-rev-equiv,...] [--colors=1,2,4,8,16,32] [--terms=1000,100000] [--mode=colors-only|colored-unions] [--trials=1]"
    }
}

fn parse_suite_list(source: &str) -> Result<Vec<ThesySuiteName>, BenchError> {
    if source == "all" {
        Ok(ThesySuiteName::all())
    } else {
        source
            .split(',')
            .map(ThesySuiteName::from_str)
            .collect::<Result<Vec<_>, _>>()
            .and_then(require_non_empty_suites)
    }
}

fn require_non_empty_suites(
    suites: Vec<ThesySuiteName>,
) -> Result<Vec<ThesySuiteName>, BenchError> {
    if suites.is_empty() {
        Err(BenchError::EmptyArgument("suites"))
    } else {
        Ok(suites)
    }
}

fn parse_usize_list(name: &'static str, source: &str) -> Result<Vec<usize>, BenchError> {
    source
        .split(',')
        .map(|value| parse_positive_usize(name, value))
        .collect::<Result<Vec<_>, _>>()
        .and_then(|values| require_non_empty_usizes(name, values))
}

fn require_non_empty_usizes(
    name: &'static str,
    values: Vec<usize>,
) -> Result<Vec<usize>, BenchError> {
    if values.is_empty() {
        Err(BenchError::EmptyArgument(name))
    } else {
        Ok(values)
    }
}

fn parse_positive_usize(name: &'static str, value: &str) -> Result<usize, BenchError> {
    value
        .parse::<usize>()
        .map_err(|_| BenchError::InvalidUsizeArgument {
            name,
            value: value.to_owned(),
        })
        .and_then(|parsed| {
            if parsed == 0 {
                Err(BenchError::ZeroArgument(name))
            } else {
                Ok(parsed)
            }
        })
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum BuildMode {
    ColorsOnly,
    ColoredUnions,
}

impl BuildMode {
    fn label(self) -> &'static str {
        match self {
            BuildMode::ColorsOnly => "colors-only",
            BuildMode::ColoredUnions => "colored-unions",
        }
    }
}

impl FromStr for BuildMode {
    type Err = BenchError;

    fn from_str(source: &str) -> Result<Self, Self::Err> {
        match source {
            "colors-only" => Ok(BuildMode::ColorsOnly),
            "colored-unions" => Ok(BuildMode::ColoredUnions),
            _ => Err(BenchError::InvalidMode(source.to_owned())),
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
struct CaseSpec {
    suite: ThesySuiteName,
    color_count: usize,
    term_count: usize,
    mode: BuildMode,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum ThesySuiteName {
    Clam,
    HipspecRevEquiv,
    HipspecRotate,
    Isaplanner,
    LeonAmortizeQueue,
    LeonHeap,
}

impl ThesySuiteName {
    fn all() -> Vec<Self> {
        vec![
            Self::Clam,
            Self::HipspecRevEquiv,
            Self::HipspecRotate,
            Self::Isaplanner,
            Self::LeonAmortizeQueue,
            Self::LeonHeap,
        ]
    }

    fn label(self) -> &'static str {
        match self {
            ThesySuiteName::Clam => "clam",
            ThesySuiteName::HipspecRevEquiv => "hipspec-rev-equiv",
            ThesySuiteName::HipspecRotate => "hipspec-rotate",
            ThesySuiteName::Isaplanner => "isaplanner",
            ThesySuiteName::LeonAmortizeQueue => "leon-amortize-queue",
            ThesySuiteName::LeonHeap => "leon-heap",
        }
    }
}

impl FromStr for ThesySuiteName {
    type Err = BenchError;

    fn from_str(source: &str) -> Result<Self, Self::Err> {
        match source {
            "clam" => Ok(Self::Clam),
            "hipspec-rev-equiv" => Ok(Self::HipspecRevEquiv),
            "hipspec-rotate" => Ok(Self::HipspecRotate),
            "isaplanner" => Ok(Self::Isaplanner),
            "leon-amortize-queue" => Ok(Self::LeonAmortizeQueue),
            "leon-heap" => Ok(Self::LeonHeap),
            _ => Err(BenchError::InvalidSuite(source.to_owned())),
        }
    }
}

fn measure_case(
    corpus: &ThesySuiteCorpus,
    case_spec: CaseSpec,
    trial_index: usize,
) -> Result<MeasurementRow, BenchError> {
    let started = Instant::now();
    let outcome = build_easter_egg_case(corpus, case_spec)?;
    Ok(MeasurementRow {
        suite: case_spec.suite,
        mode: case_spec.mode,
        color_count: case_spec.color_count,
        term_count: case_spec.term_count,
        trial_index,
        elapsed: started.elapsed(),
        rule_count: corpus.rules.len(),
        proof_goal_count: corpus.proof_goals.len(),
        seed_count: corpus.seed_pool.len(),
        ground_atom_count: corpus.known_ground_atoms.len(),
        node_count: outcome.node_count,
        class_count: outcome.class_count,
        eclass_node_count: outcome.eclass_node_count,
        color_count_observed: outcome.color_count,
        colored_equivalence_count: outcome.colored_equivalence_count,
        colored_memo_entry_count: outcome.colored_memo_entry_count,
        root_digest: outcome.root_digest,
    })
}

fn build_easter_egg_case(
    corpus: &ThesySuiteCorpus,
    case_spec: CaseSpec,
) -> Result<BuildOutcome, BenchError> {
    let mut egraph = EGraph::<SymbolLang, ()>::default();
    let roots = (0..case_spec.term_count)
        .zip(corpus.seed_pool.iter().cycle())
        .map(|(salt, seed)| {
            add_sexpr_to_egraph(&mut egraph, None, &corpus.known_ground_atoms, salt, seed)
        })
        .collect::<Result<Vec<_>, _>>()?;
    let colors = (0..case_spec.color_count)
        .map(|_| egraph.create_color(None))
        .collect::<Vec<_>>();

    apply_build_mode(case_spec.mode, &mut egraph, &roots, &colors)?;
    egraph.rebuild();

    Ok(BuildOutcome {
        node_count: egraph.total_number_of_nodes(),
        class_count: egraph.number_of_classes(),
        eclass_node_count: egraph.total_size(),
        color_count: egraph.colors().count(),
        colored_equivalence_count: egraph.colored_equivalences_size(),
        colored_memo_entry_count: egraph.color_sizes().map(|(_, size)| size).sum(),
        root_digest: root_digest(&roots),
    })
}

fn apply_build_mode(
    mode: BuildMode,
    egraph: &mut EGraph<SymbolLang, ()>,
    roots: &[Id],
    colors: &[ColorId],
) -> Result<(), BenchError> {
    match mode {
        BuildMode::ColorsOnly => Ok(()),
        BuildMode::ColoredUnions => {
            let root_count = roots.len();
            if root_count < 2 {
                Err(BenchError::InsufficientRoots(root_count))
            } else {
                colors
                    .iter()
                    .enumerate()
                    .try_for_each(|(color_index, color)| {
                        let left = roots
                            .get((color_index * 2) % root_count)
                            .copied()
                            .ok_or(BenchError::MissingRoot(color_index * 2))?;
                        let right = roots
                            .get((color_index * 2 + 1) % root_count)
                            .copied()
                            .ok_or(BenchError::MissingRoot(color_index * 2 + 1))?;
                        let _ = egraph.colored_union(*color, left, right);
                        Ok(())
                    })
            }
        }
    }
}

fn add_sexpr_to_egraph(
    egraph: &mut EGraph<SymbolLang, ()>,
    color: Option<ColorId>,
    known_ground_atoms: &BTreeSet<String>,
    salt: usize,
    sexpr: &SExpr,
) -> Result<Id, BenchError> {
    match sexpr {
        SExpr::Atom(atom) => Ok(add_node_to_egraph(
            egraph,
            color,
            SymbolLang::leaf(thesy_ground_atom(known_ground_atoms, salt, atom)),
        )),
        SExpr::List(items) => match items.as_slice() {
            [SExpr::Atom(operator), arguments @ ..] => arguments
                .iter()
                .map(|argument| {
                    add_sexpr_to_egraph(egraph, color, known_ground_atoms, salt, argument)
                })
                .collect::<Result<Vec<_>, _>>()
                .map(|children| {
                    add_node_to_egraph(egraph, color, SymbolLang::new(operator.as_str(), children))
                }),
            arguments => arguments
                .iter()
                .map(|argument| {
                    add_sexpr_to_egraph(egraph, color, known_ground_atoms, salt, argument)
                })
                .collect::<Result<Vec<_>, _>>()
                .map(|children| {
                    add_node_to_egraph(egraph, color, SymbolLang::new("$list", children))
                }),
        },
    }
}

fn add_node_to_egraph(
    egraph: &mut EGraph<SymbolLang, ()>,
    color: Option<ColorId>,
    node: SymbolLang,
) -> Id {
    match color {
        Some(color_id) => egraph.colored_add(color_id, node),
        None => egraph.add(node),
    }
}

fn thesy_ground_atom(known_ground_atoms: &BTreeSet<String>, salt: usize, atom: &str) -> String {
    if known_ground_atoms.contains(atom) {
        atom.to_owned()
    } else {
        format!("{atom}#{salt}")
    }
}

fn root_digest(roots: &[Id]) -> usize {
    roots.iter().fold(0usize, |digest, root| {
        digest
            .wrapping_mul(16_777_619)
            .wrapping_add(usize::from(*root))
    })
}

#[derive(Clone, Debug)]
struct BuildOutcome {
    node_count: usize,
    class_count: usize,
    eclass_node_count: usize,
    color_count: usize,
    colored_equivalence_count: usize,
    colored_memo_entry_count: usize,
    root_digest: usize,
}

#[derive(Clone, Debug)]
struct MeasurementRow {
    suite: ThesySuiteName,
    mode: BuildMode,
    color_count: usize,
    term_count: usize,
    trial_index: usize,
    elapsed: Duration,
    rule_count: usize,
    proof_goal_count: usize,
    seed_count: usize,
    ground_atom_count: usize,
    node_count: usize,
    class_count: usize,
    eclass_node_count: usize,
    color_count_observed: usize,
    colored_equivalence_count: usize,
    colored_memo_entry_count: usize,
    root_digest: usize,
}

impl MeasurementRow {
    fn csv_header() -> &'static str {
        "implementation,suite,mode,K,N,trial,elapsed_ns,elapsed_ms,rules,proof_goals,seeds,ground_atoms,nodes,classes,eclass_nodes,observed_colors,colored_equivalences,colored_memo_entries,root_digest"
    }

    fn to_csv(&self) -> String {
        [
            "easter-egg-colored-proper".to_owned(),
            self.suite.label().to_owned(),
            self.mode.label().to_owned(),
            self.color_count.to_string(),
            self.term_count.to_string(),
            self.trial_index.to_string(),
            self.elapsed.as_nanos().to_string(),
            format!("{:.6}", self.elapsed.as_secs_f64() * 1000.0),
            self.rule_count.to_string(),
            self.proof_goal_count.to_string(),
            self.seed_count.to_string(),
            self.ground_atom_count.to_string(),
            self.node_count.to_string(),
            self.class_count.to_string(),
            self.eclass_node_count.to_string(),
            self.color_count_observed.to_string(),
            self.colored_equivalence_count.to_string(),
            self.colored_memo_entry_count.to_string(),
            self.root_digest.to_string(),
        ]
        .join(",")
    }
}

#[derive(Clone, Debug)]
struct ThesySuiteCorpus {
    known_ground_atoms: BTreeSet<String>,
    rules: Vec<ThesyRule>,
    proof_goals: Vec<SExpr>,
    seed_pool: SeedPool,
}

#[derive(Clone, Debug)]
struct SeedPool {
    seeds: Vec<SExpr>,
}

impl SeedPool {
    fn from_seeds(suite: ThesySuiteName, seeds: Vec<SExpr>) -> Result<Self, BenchError> {
        if seeds.is_empty() {
            Err(BenchError::EmptySeedPool(suite))
        } else {
            Ok(Self { seeds })
        }
    }

    fn iter(&self) -> std::slice::Iter<'_, SExpr> {
        self.seeds.iter()
    }

    fn len(&self) -> usize {
        self.seeds.len()
    }
}

#[derive(Clone, Debug)]
struct ThesyRule {
    lhs: SExpr,
    rhs: SExpr,
}

#[derive(Clone, Debug, Eq, PartialEq)]
enum SExpr {
    Atom(String),
    List(Vec<SExpr>),
}

fn read_thesy_suite_corpus(
    fixture_dir: &Path,
    suite: ThesySuiteName,
) -> Result<ThesySuiteCorpus, BenchError> {
    let path = fixture_dir.join(format!("{}.thbundle", suite.label()));
    let source = fs::read_to_string(&path)?;
    let wrapped_source = format!("({})", strip_comments(&source));
    let parsed = parser::parse_str(&wrapped_source)
        .map_err(|error| BenchError::SexpParse(path.clone(), error.to_string()))?;
    let forms = sexp_forms(parsed)?;
    thesy_suite_corpus(suite, forms)
}

fn strip_comments(source: &str) -> String {
    source
        .lines()
        .map(|line| line.split_once(';').map_or(line, |(prefix, _)| prefix))
        .collect::<Vec<_>>()
        .join("\n")
}

fn sexp_forms(sexp: Sexp) -> Result<Vec<SExpr>, BenchError> {
    match sexp {
        Sexp::List(forms) => forms
            .into_iter()
            .map(sexp_to_thesy)
            .collect::<Result<Vec<_>, _>>(),
        Sexp::String(atom) => Ok(vec![SExpr::Atom(atom)]),
        Sexp::Empty => Ok(Vec::new()),
    }
}

fn sexp_to_thesy(sexp: Sexp) -> Result<SExpr, BenchError> {
    match sexp {
        Sexp::String(atom) => Ok(SExpr::Atom(atom)),
        Sexp::List(items) => items
            .into_iter()
            .map(sexp_to_thesy)
            .collect::<Result<Vec<_>, _>>()
            .map(SExpr::List),
        Sexp::Empty => Ok(SExpr::List(Vec::new())),
    }
}

fn thesy_suite_corpus(
    suite: ThesySuiteName,
    forms: Vec<SExpr>,
) -> Result<ThesySuiteCorpus, BenchError> {
    let known_ground_atoms = forms
        .iter()
        .fold(thesy_builtin_ground_atoms(), |mut atoms, form| {
            atoms.extend(thesy_known_ground_atoms(form));
            atoms
        });
    let rules = forms.iter().flat_map(thesy_rules).collect::<Vec<_>>();
    let proof_goals = forms
        .iter()
        .filter_map(thesy_proof_goal)
        .collect::<Vec<_>>();
    let seed_pool = SeedPool::from_seeds(
        suite,
        rules
            .iter()
            .flat_map(|rule| [rule.lhs.clone(), rule.rhs.clone()])
            .chain(proof_goals.iter().cloned())
            .collect(),
    )?;

    Ok(ThesySuiteCorpus {
        known_ground_atoms,
        rules,
        proof_goals,
        seed_pool,
    })
}

fn thesy_builtin_ground_atoms() -> BTreeSet<String> {
    ["false", "true"].into_iter().map(str::to_owned).collect()
}

fn thesy_known_ground_atoms(sexpr: &SExpr) -> BTreeSet<String> {
    match sexpr {
        SExpr::List(items) => match items.as_slice() {
            [SExpr::Atom(head), _type_name, _parameters, SExpr::List(constructors)]
                if head == "datatype" =>
            {
                constructors
                    .iter()
                    .filter_map(constructor_name)
                    .map(str::to_owned)
                    .collect()
            }
            [SExpr::Atom(head), SExpr::Atom(function_name), SExpr::List(arguments), _result_type]
                if head == "declare-fun" && arguments.is_empty() =>
            {
                [function_name.clone()].into_iter().collect()
            }
            _ => BTreeSet::new(),
        },
        SExpr::Atom(_) => BTreeSet::new(),
    }
}

fn constructor_name(sexpr: &SExpr) -> Option<&str> {
    match sexpr {
        SExpr::List(items) => items.first().and_then(atom_name),
        SExpr::Atom(_) => None,
    }
}

fn atom_name(sexpr: &SExpr) -> Option<&str> {
    match sexpr {
        SExpr::Atom(atom) => Some(atom.as_str()),
        SExpr::List(_) => None,
    }
}

fn thesy_rules(sexpr: &SExpr) -> Vec<ThesyRule> {
    match sexpr {
        SExpr::List(items) => match items.as_slice() {
            [SExpr::Atom(head), _rule_name, lhs, rhs] if head == "=>" => vec![ThesyRule {
                lhs: lhs.clone(),
                rhs: rhs.clone(),
            }],
            [SExpr::Atom(head), _rule_name, lhs, rhs] if head == "<=>" => vec![
                ThesyRule {
                    lhs: lhs.clone(),
                    rhs: rhs.clone(),
                },
                ThesyRule {
                    lhs: rhs.clone(),
                    rhs: lhs.clone(),
                },
            ],
            _ => Vec::new(),
        },
        SExpr::Atom(_) => Vec::new(),
    }
}

fn thesy_proof_goal(sexpr: &SExpr) -> Option<SExpr> {
    match sexpr {
        SExpr::List(items) => match items.as_slice() {
            [SExpr::Atom(head), goal] if head == "prove" => Some(thesy_strip_forall(goal)),
            _ => None,
        },
        SExpr::Atom(_) => None,
    }
}

fn thesy_strip_forall(sexpr: &SExpr) -> SExpr {
    match sexpr {
        SExpr::List(items) => match items.as_slice() {
            [SExpr::Atom(head), SExpr::List(_), body] if head == "forall" => body.clone(),
            _ => sexpr.clone(),
        },
        SExpr::Atom(_) => sexpr.clone(),
    }
}

#[derive(Debug)]
enum BenchError {
    EmptyArgument(&'static str),
    EmptySeedPool(ThesySuiteName),
    HelpRequested,
    InsufficientRoots(usize),
    InvalidArgument(String),
    InvalidMode(String),
    InvalidSuite(String),
    InvalidUsizeArgument { name: &'static str, value: String },
    Io(std::io::Error),
    MissingRoot(usize),
    SexpParse(PathBuf, String),
    UnknownArgument(String),
    ZeroArgument(&'static str),
}

impl fmt::Display for BenchError {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BenchError::EmptyArgument(name) => write!(formatter, "--{name} must not be empty"),
            BenchError::EmptySeedPool(suite) => {
                write!(formatter, "suite {} produced no seed terms", suite.label())
            }
            BenchError::HelpRequested => write!(formatter, "help requested"),
            BenchError::InsufficientRoots(root_count) => write!(
                formatter,
                "colored-unions mode needs at least two roots; got {root_count}"
            ),
            BenchError::InvalidArgument(argument) => {
                write!(
                    formatter,
                    "invalid argument {argument:?}; expected --name=value"
                )
            }
            BenchError::InvalidMode(mode) => write!(formatter, "invalid mode {mode:?}"),
            BenchError::InvalidSuite(suite) => write!(formatter, "invalid suite {suite:?}"),
            BenchError::InvalidUsizeArgument { name, value } => {
                write!(
                    formatter,
                    "--{name} value {value:?} is not a positive integer"
                )
            }
            BenchError::Io(error) => write!(formatter, "{error}"),
            BenchError::MissingRoot(index) => write!(formatter, "missing root at index {index}"),
            BenchError::SexpParse(path, error) => {
                write!(formatter, "failed to parse {}: {error}", path.display())
            }
            BenchError::UnknownArgument(argument) => {
                write!(formatter, "unknown argument --{argument}")
            }
            BenchError::ZeroArgument(name) => write!(formatter, "--{name} must be positive"),
        }
    }
}

impl Error for BenchError {}

impl From<std::io::Error> for BenchError {
    fn from(error: std::io::Error) -> Self {
        BenchError::Io(error)
    }
}
