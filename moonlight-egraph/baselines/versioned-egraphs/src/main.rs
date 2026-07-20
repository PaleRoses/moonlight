mod common {
    pub mod generators {
        include!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/upstream/src/bin/common/generators.rs"
        ));
    }

    pub mod interpreters {
        include!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/upstream/src/bin/common/interpreters.rs"
        ));
    }
}

use common::generators::{Configuration, Log};
use std::{
    env, fmt, fs, hint::black_box, num::ParseIntError, path::PathBuf, process::ExitCode,
    time::Instant,
};
use veg::structures::egraph::versioned::basic::VersionedEGraph;

const MAX_MEMORY_GB: usize = 32;

fn main() -> ExitCode {
    match parse_command(env::args().skip(1)).and_then(run_command) {
        Ok(()) => ExitCode::SUCCESS,
        Err(BenchError::HelpRequested) => {
            println!("{}", usage());
            ExitCode::SUCCESS
        }
        Err(error) => {
            eprintln!("{error}");
            eprintln!("{}", usage());
            ExitCode::from(2)
        }
    }
}

enum Command {
    Emit {
        output: PathBuf,
        configuration: Configuration,
    },
    Replay {
        input: PathBuf,
    },
}

fn parse_command(args: impl IntoIterator<Item = String>) -> Result<Command, BenchError> {
    let arguments = args.into_iter().collect::<Vec<_>>();
    match arguments.as_slice() {
        [help] if help == "help" || help == "--help" || help == "-h" => {
            Err(BenchError::HelpRequested)
        }
        [command, input] if command == "replay" => Ok(Command::Replay {
            input: PathBuf::from(input),
        }),
        [command, output, universe, max_arity, versions, unions, finds, seed]
            if command == "emit" =>
        {
            Ok(Command::Emit {
                output: PathBuf::from(output),
                configuration: Configuration {
                    universe_size: parse_positive("universe", universe)?,
                    enodes_max_arity: parse_positive("max-arity", max_arity)?,
                    versions: parse_positive("versions", versions)?,
                    union_size: parse_number("unions", unions)?,
                    find_size: parse_number("finds", finds)?,
                    seed: parse_number("seed", seed)?,
                    max_memory: MAX_MEMORY_GB,
                },
            })
        }
        _ => Err(BenchError::InvalidArguments(arguments)),
    }
}

fn parse_positive(name: &'static str, source: &str) -> Result<usize, BenchError> {
    parse_number(name, source).and_then(|value| {
        if value == 0 {
            Err(BenchError::ZeroArgument(name))
        } else {
            Ok(value)
        }
    })
}

fn parse_number<T>(name: &'static str, source: &str) -> Result<T, BenchError>
where
    T: std::str::FromStr<Err = ParseIntError>,
{
    source
        .parse::<T>()
        .map_err(|source_error| BenchError::InvalidNumber {
            name,
            value: source.to_owned(),
            source: source_error,
        })
}

fn run_command(command: Command) -> Result<(), BenchError> {
    match command {
        Command::Emit {
            output,
            configuration,
        } => emit_trace(output, configuration),
        Command::Replay { input } => replay_trace(input),
    }
}

fn emit_trace(output: PathBuf, configuration: Configuration) -> Result<(), BenchError> {
    let trace = Log::random(&configuration, false).serialize();
    fs::write(&output, trace).map_err(|source| BenchError::WriteTrace { output, source })
}

fn replay_trace(input: PathBuf) -> Result<(), BenchError> {
    let source = fs::read_to_string(&input).map_err(|source| BenchError::ReadTrace {
        input: input.clone(),
        source,
    })?;
    let shape = validate_trace(&source)?;
    let log = Log::deserialize(source);
    let started = Instant::now();
    let egraph = black_box(common::interpreters::versioned::interpret::<
        VersionedEGraph<()>,
    >(log));
    let elapsed_ns = started.elapsed().as_nanos();
    black_box(&egraph);
    println!(
        "implementation,elapsed_ns,enodes,versions,unions,finds\nveg-versioned,{elapsed_ns},{},{},{},{}",
        shape.elements, shape.versions, shape.unions, shape.finds
    );
    Ok(())
}

#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
struct TraceShape {
    elements: usize,
    versions: usize,
    checkout: usize,
    unions: usize,
    finds: usize,
}

fn validate_trace(source: &str) -> Result<TraceShape, BenchError> {
    source.lines().enumerate().try_fold(
        TraceShape {
            versions: 1,
            ..TraceShape::default()
        },
        validate_trace_line,
    )
}

fn validate_trace_line(
    shape: TraceShape,
    (line_index, line): (usize, &str),
) -> Result<TraceShape, BenchError> {
    let line_number = line_index + 1;
    let words = line.split_whitespace().collect::<Vec<_>>();
    match words.as_slice() {
        ["add", operator, arguments @ ..] => {
            let _ = parse_trace_index(line_number, operator)?;
            arguments
                .iter()
                .try_for_each(|argument| {
                    parse_trace_index(line_number, argument).and_then(|index| {
                        require_bound(line_number, "element", index, shape.elements)
                    })
                })
                .map(|()| TraceShape {
                    elements: shape.elements + 1,
                    ..shape
                })
        }
        ["union", left, right] => [left, right]
            .into_iter()
            .try_for_each(|index| {
                parse_trace_index(line_number, index)
                    .and_then(|value| require_bound(line_number, "element", value, shape.elements))
            })
            .map(|()| TraceShape {
                unions: shape.unions + 1,
                ..shape
            }),
        ["find", element] => parse_trace_index(line_number, element)
            .and_then(|value| require_bound(line_number, "element", value, shape.elements))
            .map(|()| TraceShape {
                finds: shape.finds + 1,
                ..shape
            }),
        ["checkout", version] => parse_trace_index(line_number, version)
            .and_then(|value| {
                require_bound(line_number, "version", value, shape.versions).map(|()| value)
            })
            .map(|checkout| TraceShape { checkout, ..shape }),
        ["branchout"] => Ok(TraceShape {
            checkout: shape.versions,
            versions: shape.versions + 1,
            ..shape
        }),
        _ => Err(BenchError::MalformedTraceLine {
            line: line_number,
            source: line.to_owned(),
        }),
    }
}

fn parse_trace_index(line: usize, source: &str) -> Result<usize, BenchError> {
    source
        .parse::<usize>()
        .map_err(|parse_error| BenchError::InvalidTraceIndex {
            line,
            value: source.to_owned(),
            source: parse_error,
        })
}

fn require_bound(
    line: usize,
    kind: &'static str,
    index: usize,
    upper_bound: usize,
) -> Result<(), BenchError> {
    if index < upper_bound {
        Ok(())
    } else {
        Err(BenchError::TraceIndexOutOfBounds {
            line,
            kind,
            index,
            upper_bound,
        })
    }
}

#[derive(Debug)]
enum BenchError {
    HelpRequested,
    InvalidArguments(Vec<String>),
    InvalidNumber {
        name: &'static str,
        value: String,
        source: ParseIntError,
    },
    ZeroArgument(&'static str),
    ReadTrace {
        input: PathBuf,
        source: std::io::Error,
    },
    WriteTrace {
        output: PathBuf,
        source: std::io::Error,
    },
    MalformedTraceLine {
        line: usize,
        source: String,
    },
    InvalidTraceIndex {
        line: usize,
        value: String,
        source: ParseIntError,
    },
    TraceIndexOutOfBounds {
        line: usize,
        kind: &'static str,
        index: usize,
        upper_bound: usize,
    },
}

impl fmt::Display for BenchError {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BenchError::HelpRequested => write!(formatter, "help requested"),
            BenchError::InvalidArguments(arguments) => {
                write!(formatter, "invalid arguments: {arguments:?}")
            }
            BenchError::InvalidNumber {
                name,
                value,
                source,
            } => write!(formatter, "invalid {name} value {value:?}: {source}"),
            BenchError::ZeroArgument(name) => write!(formatter, "{name} must be positive"),
            BenchError::ReadTrace { input, source } => {
                write!(
                    formatter,
                    "could not read trace {}: {source}",
                    input.display()
                )
            }
            BenchError::WriteTrace { output, source } => {
                write!(
                    formatter,
                    "could not write trace {}: {source}",
                    output.display()
                )
            }
            BenchError::MalformedTraceLine { line, source } => {
                write!(formatter, "malformed trace line {line}: {source:?}")
            }
            BenchError::InvalidTraceIndex {
                line,
                value,
                source,
            } => write!(
                formatter,
                "invalid trace index {value:?} at line {line}: {source}"
            ),
            BenchError::TraceIndexOutOfBounds {
                line,
                kind,
                index,
                upper_bound,
            } => write!(
                formatter,
                "{kind} index {index} at line {line} is outside 0..{upper_bound}"
            ),
        }
    }
}

fn usage() -> &'static str {
    "usage:\n  moonlight-versioned-egraphs-bench emit TRACE N ARITY VERSIONS UNIONS FINDS SEED\n  moonlight-versioned-egraphs-bench replay TRACE"
}
