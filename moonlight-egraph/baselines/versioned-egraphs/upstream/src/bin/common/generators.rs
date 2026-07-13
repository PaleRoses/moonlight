/// #### Description
/// A configuration for generating random unionfinds.
pub struct Configuration {
    /// A [u64] used to control the randomness of the generation process.
    pub seed: u64,
    /// The number of elements inside the union-find.
    pub universe_size: usize,
    /// Any [ENode] in the universe will have a random arity between 0 and this value.
    pub enodes_max_arity: usize,
    /// The number of versions created. Unions are distributed uniformly among
    /// every version, so it must be 1 <= `versions` <= `union_size`.
    pub versions: usize,
    /// The number of random unions performed on the union-find.
    pub union_size: usize,
    /// The number of random finds performed on the union-find.
    pub find_size: usize,
    /// The maximum memory (in GB) that the egraph is allowed to use.
    pub max_memory: usize,
}
pub const DEFAULT_CONFIGURATION: Configuration = Configuration {
    seed: 1234,
    universe_size: 8192,
    enodes_max_arity: 5,
    versions: 8192,
    union_size: 8192,
    find_size: 8192,
    max_memory: 32,
};
/// Actions that can be performed on a versioned egraph.
#[derive(Clone, Debug)]
pub enum Action {
    /// Add a new [ENode] with operator `f` and arguments `args`.
    Add { op: usize, args: Vec<usize> },
    /// Union elements with indexes `xi` and `yi`.
    Union { xi: usize, yi: usize },
    /// Find element with index `xi`.
    Find { xi: usize },
    /// Checkout at the version with index `vi`.
    Checkout { vi: usize },
    /// Create a new subversion inheriting from the current version.
    Branchout,
}
/// A transactional definition of egraph.
#[derive(Clone, Default)]
pub struct Log {
    transactions: Vec<Action>,
}
impl Log {
    /// Get the list of transactions.
    pub fn transactions(&self) -> &[Action] {
        &self.transactions
    }
    /// Consume this [Log] and return the list of transactions.
    pub fn consume(self) -> Vec<Action> {
        self.transactions
    }
    /// Serialize this [Log] into a string.
    pub fn serialize(&self) -> String {
        let mut result: String = String::new();
        for action in &self.transactions {
            match action {
                Action::Add { op, args } => {
                    let args = args.iter().map(|x| x.to_string()).collect::<Vec<_>>().join(" ");
                    result.push_str(&format!("add {} {}\n", op, args));
                },
                Action::Union { xi, yi } => {
                    result.push_str(&format!("union {} {}\n", xi, yi));
                },
                Action::Find { xi } => {
                    result.push_str(&format!("find {}\n", xi));
                },
                Action::Checkout { vi } => {
                    result.push_str(&format!("checkout {}\n", vi));
                },
                Action::Branchout => {
                    result.push_str("branchout\n");
                },
            }
        }
        result
    }
    /// Deserialize a [Log] from a string.
    pub fn deserialize(input: String) -> Log {
        let mut transactions: Vec<Action> = Vec::new();
        for line in input.lines() {
            let words: Vec<&str> = line.split_whitespace().collect();
            match words[0] {
                "add" => {
                    let op: usize = words[1].parse().unwrap();
                    let args: Vec<_> = words[2..].iter().map(|x| x.parse().unwrap()).collect();
                    transactions.push(Action::Add { op, args });
                },
                "union" => {
                    let xi: usize = words[1].parse().unwrap();
                    let yi: usize = words[2].parse().unwrap();
                    transactions.push(Action::Union { xi, yi });
                },
                "find" => {
                    let xi: usize = words[1].parse().unwrap();
                    transactions.push(Action::Find { xi });
                },
                "checkout" => {
                    let vi: usize = words[1].parse().unwrap();
                    transactions.push(Action::Checkout { vi });
                },
                "branchout" => {
                    transactions.push(Action::Branchout);
                },
                _ => panic!("Unknown action: {}", words[0]),
            }
        }
        Log { transactions }
    }
    /// Generate a random [Log] based on the [Configuration] `conf`.
    pub fn random(conf: &Configuration, only_leaves: bool) -> Log {
        use rand::{Rng, SeedableRng};
        struct Sampler {
            pool: Vec<usize>,
            size: usize,
        }
        impl Sampler {
            fn new(pool: Vec<usize>) -> Self {
                let size: usize = pool.iter().sum();
                Self { pool, size }
            }
            fn random<R: Rng>(&mut self, rng: &mut R) -> Option<usize> {
                if self.size == 0 {
                    return None;
                }
                let choice: usize = rng.gen_range(0..self.size);
                let mut cumulative: usize = 0;
                for (index, elements) in self.pool.iter_mut().enumerate() {
                    cumulative += *elements;
                    if choice < cumulative {
                        *elements -= 1;
                        self.size -= 1;
                        return Some(index);
                    }
                }
                unreachable!()
            }
        }
        let mut rng = rand::rngs::StdRng::seed_from_u64(conf.seed);
        let mut log = Log { transactions: Vec::new() };
        let mut element_count: usize = 0;
        let mut version_count: usize = 1;

        // Create universe
        for _ in 0..conf.universe_size {
            let op: usize = rng.gen_range(0..=element_count);
            let arity: usize = rng.gen_range(0..=conf.enodes_max_arity);
            let mut args: Vec<usize> = Vec::new();
            if element_count > 0 {
                for _ in 0..arity {
                    args.push(rng.gen_range(0..element_count));
                }
            }
            element_count += 1;
            log.transactions.push(Action::Add { op, args });
        }
        // Apply random operations
        let mut sampler: Sampler = Sampler::new(vec![
            conf.union_size, // # Union
            conf.find_size,  // # Find
            conf.versions,   // # Branchout (Forks)
        ]);
        while let Some(action) = sampler.random(&mut rng) {
            let checkout = rng.gen_range(0..version_count);
            match action {
                0 => {
                    if !only_leaves {
                        log.transactions.push(Action::Checkout { vi: checkout });
                    }
                    let xi = rng.gen_range(0..element_count);
                    let yi = rng.gen_range(0..element_count);
                    log.transactions.push(Action::Union { xi, yi });
                },
                1 => {
                    if !only_leaves {
                        log.transactions.push(Action::Checkout { vi: checkout });
                    }
                    let xi = rng.gen_range(0..element_count);
                    log.transactions.push(Action::Find { xi });
                },
                2 => {
                    log.transactions.push(Action::Checkout { vi: checkout });
                    log.transactions.push(Action::Branchout);
                    version_count += 1;
                },
                _ => unreachable!(),
            }
        }
        log
    }
}
