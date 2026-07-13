pub use egg::{Id, Symbol};
use indexmap::IndexSet;
use std::sync::{Mutex, OnceLock};

use crate::util::memory::{HasMemory, Memory};

impl HasMemory for Id {
    fn memory(&self) -> Memory {
        Memory::of_bytes(std::mem::size_of_val(self))
    }
}
/// A struct that can be interpreted as an index.
pub trait AsIndex {
    /// #### Return
    /// This struct as an index for a [Vec].
    fn to_i(self) -> usize;
}
impl AsIndex for Id {
    #[inline(always)]
    fn to_i(self) -> usize {
        Into::<usize>::into(self)
    }
}

/// A range over [Id]s.
pub struct IdRange {
    current: u32,
    end: u32,
}
impl IdRange {
    #[inline(always)]
    pub fn new(start: usize, end: usize) -> IdRange {
        IdRange { current: start as u32, end: end as u32 }
    }
}
impl Iterator for IdRange {
    type Item = Id;
    fn next(&mut self) -> Option<Self::Item> {
        if self.current < self.end {
            let id = Id::from(self.current as usize);
            self.current += 1;
            Some(id)
        } else {
            None
        }
    }
}

/// A factory for [Id]s.
pub trait IdFactory {
    type Id;
    /// #### Return
    /// A new [Id].
    fn create_id() -> Self::Id;
}

/// A unique identifier for an element.
pub struct UniqueIds;
impl IdFactory for UniqueIds {
    type Id = Id;
    fn create_id() -> Self::Id {
        use std::sync::atomic::*;
        static ID_COUNT: AtomicU32 = AtomicU32::new(0);
        Id::from(ID_COUNT.fetch_add(1, Ordering::Relaxed) as usize)
    }
}

/// Utilities for generating identifiers bound to machine-time.
pub struct TimeIds;
impl IdFactory for TimeIds {
    type Id = u128;
    //// #### Return
    /// A new identifier bound to the current machine-time.
    fn create_id() -> Self::Id {
        use std::time::SystemTime;
        SystemTime::now().duration_since(SystemTime::UNIX_EPOCH).unwrap().as_nanos()
    }
}

/// Utilities for generating identifiers bound to unique names.
pub struct SymbolIds;
static VOCABULARY: OnceLock<Mutex<IndexSet<Symbol>>> = OnceLock::new();
impl SymbolIds {
    pub fn from_symbol(symbol: &Symbol) -> Id {
        let mut v = VOCABULARY.get_or_init(|| Mutex::new(Default::default())).lock().unwrap();
        let (index, _wasnt_present) = v.insert_full(*symbol);
        Id::from(index)
    }
    pub fn to_symbol(id: Id) -> Option<Symbol> {
        let v = VOCABULARY.get_or_init(|| Mutex::new(Default::default())).lock().unwrap();
        v.get_index(id.into()).cloned()
    }
    pub fn from_string(string: &str) -> Id {
        SymbolIds::from_symbol(&Symbol::new(string))
    }
    pub fn to_string(id: Id) -> Option<&'static str> {
        SymbolIds::to_symbol(id).map(|s| s.as_str())
    }
}
