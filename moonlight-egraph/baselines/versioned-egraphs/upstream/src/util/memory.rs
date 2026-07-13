/// The size of some data.
pub struct Memory {
    bytes: usize,
}
#[rustfmt::skip]
impl Memory {
    /// #### Return
    /// A new [Memory] with the specified number of bytes.
    pub fn of_bytes(bytes: usize) -> Memory { Memory { bytes } }

    pub fn bytes(&self) -> usize { self.bytes }
    pub fn kilobytes(&self) -> f64 { self.bytes() as f64 / 1024.0 }
    pub fn megabytes(&self) -> f64 { self.kilobytes() / 1024.0 }
    pub fn gigabytes(&self) -> f64 { self.megabytes() / 1024.0 }
    pub fn bits(&self) -> usize { self.bytes() * 8 }
    pub fn kilobits(&self) -> f64 { self.kilobytes() * 8.0 }
    pub fn megabits(&self) -> f64 { self.megabytes() * 8.0 }
    pub fn gigabits(&self) -> f64 { self.gigabytes() * 8.0 }
}
impl std::ops::Add for Memory {
    type Output = Memory;
    fn add(self, other: Memory) -> Memory {
        Memory::of_bytes(self.bytes + other.bytes)
    }
}
impl std::iter::Sum for Memory {
    fn sum<I: Iterator<Item = Memory>>(iter: I) -> Memory {
        iter.fold(Memory::of_bytes(0), std::ops::Add::add)
    }
}
impl std::fmt::Display for Memory {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let bytes = self.bytes();
        if bytes < 1024 {
            write!(f, "{} B", bytes)
        } else if bytes < 1024 * 1024 {
            write!(f, "{:.3} KB", self.kilobytes())
        } else if bytes < 1024 * 1024 * 1024 {
            write!(f, "{:.3} MB", self.megabytes())
        } else {
            write!(f, "{:.3} GB", self.gigabytes())
        }
    }
}

/// Data whose [Memory] can be retrieved at runtime.
pub trait HasMemory {
    /// #### Return
    /// The static name of this struct.
    fn struct_name(&self) -> &'static str {
        std::any::type_name::<Self>()
    }
    /// #### Return
    /// A vector binding the name of each field of this struct to its reference.
    fn fields(&self) -> Vec<(&str, &dyn HasMemory)> {
        vec![]
    }
    /// #### Return
    /// The [Memory] occupied by this struct instance.
    fn memory(&self) -> Memory {
        self.fields().iter().map(|(_fieldname, field)| field.memory()).sum::<Memory>()
    }
    /// #### Description
    /// Print the memory occupied by this struct instance and all nested fields.
    fn print_memory(&self, label: &str, depth: usize, total: &Memory) {
        let memory = self.memory();
        let percentage = if total.bytes() == 0 {
            100.0
        } else {
            100.0 * memory.bytes() as f64 / total.bytes() as f64
        };
        println!("{}{}: {} ({:.2}%)", "  ".repeat(depth), label, memory, percentage);
        self.fields().iter().for_each(|(name, field)| {
            field.print_memory(name, depth + 1, &memory);
        });
    }
    /// Alias for [print_memory] with the default configuration.
    fn default_print_memory(&self) {
        self.print_memory(self.struct_name(), 0, &Memory::of_bytes(0));
    }
}

// ### IMPLEMENTATIONS FOR BUILT-IN TYPES ######################################
// Primitives
macro_rules! primitive_has_memory {
    ($t:ty) => {
        impl HasMemory for $t {
            fn memory(&self) -> Memory {
                Memory::of_bytes(std::mem::size_of_val(self))
            }
        }
    };
}
macro_rules! primitives_have_memory {
    ($($t:ty),*) => {
        $(primitive_has_memory!($t);)*
    };
}
primitives_have_memory!(
    (),
    bool,
    char,
    usize,
    isize,
    u8,
    i8,
    u16,
    i16,
    u32,
    i32,
    u64,
    i64,
    u128,
    i128,
    f32,
    f64
);

impl<A: HasMemory> HasMemory for Option<A> {
    fn memory(&self) -> Memory {
        let option = match self {
            Some(value) => value.memory(),
            None => Memory::of_bytes(std::mem::size_of::<A>()),
        };
        option + Memory::of_bytes(1)
    }
}
impl<A: HasMemory, B: HasMemory> HasMemory for (A, B) {
    fn memory(&self) -> Memory {
        self.0.memory() + self.1.memory()
    }
}

// Collections
use std::collections::*;

macro_rules! iterable1_has_memory {
    ($t:ty) => {
        impl<A: HasMemory> HasMemory for $t {
            fn memory(&self) -> Memory {
                self.iter().map(|x| x.memory()).sum::<Memory>()
                    + Memory::of_bytes(std::mem::size_of_val(self))
            }
        }
    };
}
macro_rules! iterables1_have_memory {
    ($($t:ty),*) => {
        $(iterable1_has_memory!($t);)*
    };
}
iterables1_have_memory!([A], Vec<A>, HashSet<A>, BTreeSet<A>, rustc_hash::FxHashSet<A>);

macro_rules! iterable2_has_memory {
    ($t:ty) => {
        impl<A: HasMemory, B: HasMemory> HasMemory for $t {
            fn memory(&self) -> Memory {
                self.iter().map(|x| x.0.memory() + x.1.memory()).sum::<Memory>()
                    + Memory::of_bytes(std::mem::size_of_val(self))
            }
        }
    };
}
macro_rules! iterables2_have_memory {
    ($($t:ty),*) => {
        $(iterable2_has_memory!($t);)*
    };
}
iterables2_have_memory!(HashMap<A, B>, BTreeMap<A, B>, rustc_hash::FxHashMap<A, B>);

// #############################################################################
