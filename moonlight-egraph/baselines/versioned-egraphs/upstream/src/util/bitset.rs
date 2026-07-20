use super::memory::HasMemory;

/// A block of bits.
type Block = usize;
/// The number of bits in a block.
const BLOCK_BITS: usize = Block::BITS as usize;

#[derive(Debug, Clone, Default)]
/// A set of integers, where membership is encoded with a bit for each integer
/// (1 for presence, 0 for absence).
pub struct BitSet {
    // TODO consider making this an BTreeMap<BlockIndex, Block> instead of a Vec<Block>:
    //      reduces memory consumption and traversing may be faster, but search becomes log(n)
    underlying: Vec<Block>,
    size: usize,
}

impl BitSet {
    /// #### Return
    /// The number of elements in this [BitSet].
    /// #### Complexity
    /// O(1)
    pub fn size(&self) -> usize {
        self.size
    }

    /// #### Return
    /// The number of blocks in this [BitSet].
    ///
    /// #### Notes
    /// The number of blocks is proportional to the maximum element contained
    /// in this [BitSet].
    ///
    /// #### Complexity
    /// O(1)
    pub fn blocks(&self) -> usize {
        self.underlying.len()
    }

    /// #### Description
    /// Add the specified element `x` to this [BitSet].
    /// #### Return
    /// False if the element was already present, true otherwise.
    /// #### Complexity
    /// O(1)
    pub fn add(&mut self, x: usize) -> bool {
        let (block, index) = (x / BLOCK_BITS, x % BLOCK_BITS);
        if block >= self.underlying.len() {
            self.underlying.resize(block + 1, 0);
        }
        let mask = 1 << index;
        let block_before = self.underlying[block];
        self.underlying[block] |= mask;
        if block_before != self.underlying[block] {
            self.size += 1;
            return true;
        }
        false
    }

    /// #### Return
    /// True if the specified element `x` is contained by this [BitSet],
    /// false otherwise.
    /// #### Complexity
    /// O(1)
    #[inline(always)]
    pub fn contains(&self, x: usize) -> bool {
        let block = x / BLOCK_BITS;
        match self.underlying.get(block) {
            Some(block) => {
                let index = x % BLOCK_BITS;
                (block & (1 << index)) != 0
            },
            None => false,
        }
    }

    /// #### Description
    /// Remove the specified element `x` from this [BitSet].
    /// #### Return
    /// False if the element was not found, true otherwise.
    /// #### Complexity
    /// O(1)
    pub fn remove(&mut self, x: usize) -> bool {
        let (block, index) = (x / BLOCK_BITS, x % BLOCK_BITS);
        if block >= self.underlying.len() {
            return false;
        }
        let mask = Block::MAX ^ (1 << index);
        let block_before = self.underlying[block];
        self.underlying[block] &= mask;
        if block_before != self.underlying[block] {
            self.size -= 1;
            return true;
        }
        false
    }

    /// ### Description
    /// Remove all elements from this [BitSet].
    pub fn clear(&mut self) {
        self.underlying.clear();
        self.size = 0;
    }

    /// ### Return
    /// An iterator over the elements contained in this [BitSet].
    /// #### Complexity
    /// O([BitSet::blocks] + [BitSet::size])
    pub fn iter(&self) -> impl std::iter::Iterator<Item = usize> + '_ {
        let mut iter = self.underlying.iter();
        let head_block = iter.next().cloned().unwrap_or_default();
        BitSetIterator { blocks: iter, block: head_block, base: 0 }
    }
}

struct BitSetIterator<'a> {
    blocks: std::slice::Iter<'a, Block>,
    block: Block,
    base: usize,
}
impl Iterator for BitSetIterator<'_> {
    type Item = usize;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if self.block != 0 {
                let skip = self.block.trailing_zeros() as usize;
                self.block &= self.block - 1;
                return Some(self.base + skip);
            }
            let next_block = self.blocks.next()?;
            self.block = *next_block;
            self.base += BLOCK_BITS;
        }
    }
}

impl HasMemory for BitSet {
    fn fields(&self) -> Vec<(&str, &dyn HasMemory)> {
        vec![("underlying", &self.underlying), ("size", &self.size)]
    }
}

impl std::fmt::Display for BitSet {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "BitSet{{{}}}", self.iter().map(|x| x.to_string()).collect::<Vec<_>>().join(", "))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn empty() {
        assert!(BitSet::default().size() == 0)
    }

    #[test]
    fn add() {
        let mut set = BitSet::default();
        let success = set.add(0);
        assert!(success);
        assert!(set.size() == 1);
        let success = set.add(42);
        assert!(success);
        assert!(set.size() == 2);
        let success = set.add(u32::MAX as usize);
        // NOTE this is too much to instantiate for the Vec implementation
        // let success = set.add(std::u64::MAX as usize);
        assert!(success);
        assert!(set.size() == 3);
        let failure = !set.add(0);
        assert!(failure);
        assert!(set.size() == 3);
    }

    #[test]
    fn contains() {
        let mut set = BitSet::default();
        assert!(!set.contains(0));
        set.add(0);
        assert!(set.contains(0));
        assert!(!set.contains(42));
        set.add(42);
        assert!(set.contains(42));
        assert!(!set.contains(u32::MAX as usize));
        set.add(u32::MAX as usize);
        assert!(set.contains(u32::MAX as usize));
    }

    #[test]
    fn remove() {
        let mut set = BitSet::default();
        set.add(0);
        set.add(42);
        set.add(u32::MAX as usize);
        assert!(set.contains(0) && set.contains(42) && set.contains(u32::MAX as usize));
        assert!(set.size() == 3);

        let success = set.remove(0);
        assert!(success);
        assert!(!set.contains(0));
        assert!(set.size() == 2);

        let success = set.remove(42);
        assert!(success);
        assert!(!set.contains(42));
        assert!(set.size() == 1);

        let success = set.remove(u32::MAX as usize);
        assert!(success);
        assert!(!set.contains(u32::MAX as usize));
        assert!(set.size() == 0);

        let failure = !set.remove(u32::MAX as usize);
        assert!(failure);
    }

    #[test]
    fn empty_iter() {
        let elements: Vec<usize> = BitSet::default().iter().collect();
        assert!(elements == vec![], "found elements: {:?}", elements);
    }

    #[test]
    fn iter() {
        let mut set = BitSet::default();
        set.add(0);
        set.add(2);
        set.add(4);
        set.add(42);
        set.add(1000);
        let elements: Vec<usize> = set.iter().collect();
        assert!(elements == vec![0, 2, 4, 42, 1000], "found elements: {:?}", elements);
    }

    #[test]
    fn memory() {
        let mut set = BitSet::default();
        for x in 0..BLOCK_BITS {
            set.add(x);
        }
        assert!(set.memory().bytes() < BLOCK_BITS / 8 * set.size());
    }
}
