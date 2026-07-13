mod common;

#[global_allocator]
static ALLOCATOR: cap::Cap<std::alloc::System> = cap::Cap::new(std::alloc::System, usize::MAX);
const GB: usize = 1024 * 1024 * 1024;

fn main() {
    use common::generators::*;
    use common::interpreters;
    use veg::structures::egraph::extensions::diegraph::DisequalityEdges;

    let raw_args: Vec<String> = std::env::args().collect();
    let _program: &String = &raw_args[0];
    let args: &[String] = &raw_args[1..];

    // Parse
    let mut implementation_name: &str = "versioned";
    let mut analysis_mode: &str = "unit";
    let mut universe_size: usize = DEFAULT_CONFIGURATION.universe_size;
    let mut enodes_max_arity: usize = DEFAULT_CONFIGURATION.enodes_max_arity;
    let mut versions: usize = DEFAULT_CONFIGURATION.versions;
    let mut union_size: usize = DEFAULT_CONFIGURATION.union_size;
    let mut find_size: usize = DEFAULT_CONFIGURATION.find_size;
    let mut seed: u64 = DEFAULT_CONFIGURATION.seed;
    let mut max_memory: usize = DEFAULT_CONFIGURATION.max_memory;
    let mut args = args.iter();
    if let Some(argi) = args.next() {
        implementation_name = argi.as_str();
    }
    if let Some(argi) = args.next() {
        analysis_mode = argi.as_str();
    }
    if let Some(argi) = args.next() {
        universe_size = argi.parse().expect("3rd arg: universe size should be unsigned integer");
    }
    if let Some(argi) = args.next() {
        enodes_max_arity =
            argi.parse().expect("4th arg: enode max arity should be unsigned integer");
    }
    if let Some(argi) = args.next() {
        versions = argi.parse().expect("5th arg: versions should be unsigned integer");
    }
    if let Some(argi) = args.next() {
        union_size = argi.parse().expect("6th arg: union size should be unsigned integer");
    }
    if let Some(argi) = args.next() {
        find_size = argi.parse().expect("7th arg: find size should be unsigned integer");
    }
    if let Some(argi) = args.next() {
        seed = argi.parse().expect("8th arg: seed should be unsigned integer.");
    }
    if let Some(argi) = args.next() {
        max_memory = argi.parse().expect("9th arg: max memory should be unsigned integer.");
    }

    ALLOCATOR
        .set_limit(max_memory * GB)
        .unwrap_or_else(|_| panic!("could not reserve {max_memory}GB of memory"));

    let interpreter = match (implementation_name, analysis_mode) {
        ("versioned", "unit") => |log| {
            use veg::structures::egraph::versioned::basic::VersionedEGraph;
            interpreters::versioned::interpret::<VersionedEGraph<()>>(log);
        },
        ("versioned", "disequalities") => |log| {
            use veg::structures::egraph::versioned::basic::VersionedEGraph;
            interpreters::versioned::interpret::<VersionedEGraph<DisequalityEdges>>(log);
        },
        ("cloning", "unit") => |log| {
            use veg::structures::egraph::basic;
            interpreters::cloning::interpret::<basic::EGraph<()>>(log);
        },
        ("cloning", "disequalities") => |log| {
            use veg::structures::egraph::basic;
            interpreters::cloning::interpret::<basic::EGraph<DisequalityEdges>>(log);
        },
        ("persistent", "unit") => |log| {
            use veg::structures::egraph::persistent;
            interpreters::cloning::interpret::<persistent::EGraph<()>>(log);
        },
        ("persistent", "disequalities") => |log| {
            use veg::structures::egraph::persistent;
            interpreters::cloning::interpret::<persistent::EGraph<DisequalityEdges>>(log);
        },
        ("colored", "unit") => |log| {
            interpreters::colored::interpret(log);
        },
        ("colored", "disequalities") => |log| {
            interpreters::colored::interpret_with_disequalities(log);
        },
        any => panic!("Missing log interpreter for egraph `{}` with analysis `{}`", any.0, any.1),
    };

    // Configuration
    println!("# CONFIGURATION");
    println!("# Implementation: {implementation_name}");
    println!("# RNG Seed: {seed}");
    println!("# Number of Initial ENodes: {universe_size}");
    println!("# Max Arity for ENodes: {enodes_max_arity}");
    println!("# Number of Versions: {versions}");
    println!("# Number of Unions: {union_size}");
    println!("# Number of Finds: {find_size}");
    println!("# Max Memory: {max_memory}GB");

    // Execute
    interpreter(Log::random(
        &Configuration {
            seed,
            universe_size,
            enodes_max_arity,
            versions,
            union_size,
            find_size,
            max_memory,
        },
        false,
    ));
}
