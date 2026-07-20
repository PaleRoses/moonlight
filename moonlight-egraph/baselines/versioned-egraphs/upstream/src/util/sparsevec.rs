use std::fmt::Debug;

use crate::util::memory::HasMemory;

#[derive(Clone, Copy, Debug)]
struct Pointer(usize);
impl HasMemory for Pointer {
    fn memory(&self) -> super::memory::Memory {
        self.0.memory()
    }
}
#[derive(Clone, Copy, Debug)]
struct Shift(u8);
impl HasMemory for Shift {
    fn memory(&self) -> super::memory::Memory {
        self.0.memory()
    }
}

#[derive(Clone, Debug)]
enum SparseVecNode {
    Block(Vec<Pointer>),
    SparseVec(Shift, Vec<SparseVecNode>),
}
impl HasMemory for SparseVecNode {
    fn fields(&self) -> Vec<(&str, &dyn HasMemory)> {
        match self {
            SparseVecNode::Block(vec) => vec![("vec", vec)],
            SparseVecNode::SparseVec(shift, children) => {
                vec![("shift", shift), ("children", children)]
            },
        }
    }
}
#[derive(Clone, Debug)]
pub struct SparseVec<A: Into<usize>, B> {
    root: SparseVecNode,
    elements: Vec<(A, B)>,
    block_capacity_bits: u8,
    block_capacity: usize,
}
impl<A: Into<usize> + HasMemory, B: HasMemory> HasMemory for SparseVec<A, B> {
    fn fields(&self) -> Vec<(&str, &dyn HasMemory)> {
        vec![
            ("root", &self.root),
            ("elements", &self.elements),
            ("block_capacity", &self.block_capacity),
            ("block_capacity_bits", &self.block_capacity_bits),
        ]
    }
}
impl<A: Into<usize> + Copy, B> Default for SparseVec<A, B> {
    fn default() -> Self {
        Self::with_block_capacity_bits(16)
    }
}
impl<A: Into<usize>, B> std::fmt::Display for SparseVec<A, B> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        fn aux(
            f: &mut std::fmt::Formatter<'_>,
            root: &SparseVecNode,
            block_capacity: usize,
            block: &mut usize,
            indent: usize,
        ) -> std::fmt::Result {
            match root {
                SparseVecNode::SparseVec(shift, children) => {
                    writeln!(
                        f,
                        "{}SparseVec [{}; {}]",
                        "--".repeat(indent),
                        block_capacity * *block,
                        block_capacity * *block + (block_capacity << shift.0),
                    )?;
                    children
                        .iter()
                        .try_for_each(|c| aux(f, c, block_capacity, block, indent + 1))?;
                },
                SparseVecNode::Block(vec) => {
                    writeln!(
                        f,
                        "{}Vec [{}; {}]: {:?}",
                        "--".repeat(indent),
                        block_capacity * *block,
                        block_capacity * *block + block_capacity,
                        vec
                    )?;
                    *block += 1;
                },
            }
            Ok(())
        }
        aux(f, &self.root, self.block_capacity, &mut 0, 0)
    }
}
impl<A: Into<usize> + Copy, B> SparseVec<A, B> {
    #[inline(always)]
    pub fn with_block_capacity_bits(block_capacity_bits: u8) -> Self {
        SparseVec {
            root: SparseVecNode::Block(Default::default()),
            elements: Default::default(),
            block_capacity_bits,
            block_capacity: 1 << block_capacity_bits,
        }
    }
    #[inline(always)]
    pub fn insert(&mut self, key: A, value: B) {
        Self::insert_into(
            &mut self.root,
            &mut self.elements,
            self.block_capacity,
            self.block_capacity_bits,
            key.into(),
            (key, value),
        )
    }
    #[inline(always)]
    pub fn get(&self, key: &A) -> Option<&B> {
        self.entry(key).map(|e| e.1)
    }
    #[inline(always)]
    pub fn get_mut(&mut self, key: &A) -> Option<&mut B> {
        self.entry_mut(key).map(|e| e.1)
    }
    #[inline(always)]
    pub fn clear(&mut self) {
        self.root = SparseVecNode::Block(vec![]);
        self.elements.clear();
    }
    #[inline(always)]
    pub fn is_empty(&self) -> bool {
        self.elements.is_empty()
    }
    #[inline(always)]
    pub fn len(&self) -> usize {
        self.elements.len()
    }
    #[inline(always)]
    pub fn iter(&self) -> impl std::iter::Iterator<Item = &(A, B)> + '_ {
        self.elements.iter()
    }
    #[inline(always)]
    pub fn into_iter(self) -> impl std::iter::Iterator<Item = (A, B)> {
        self.elements.into_iter()
    }
    #[inline(always)]
    pub fn extend(&mut self, other: SparseVec<A, B>) {
        for p in other.into_iter() {
            self.insert(p.0, p.1);
        }
    }
    #[inline(always)]
    pub fn entry(&self, key: &A) -> Option<(&A, &B)> {
        Self::index_of(&self.root, (*key).into()).map(|p| {
            // NOTE bound checks are ~2x slower and not needed here
            let (ref fst, ref snd) = unsafe { self.elements.get_unchecked(p.0 - 1) };
            (fst, snd)
        })
    }
    #[inline(always)]
    fn entry_mut(&mut self, key: &A) -> Option<(&A, &mut B)> {
        Self::index_of(&self.root, (*key).into()).map(|p| {
            let (ref fst, ref mut snd) = unsafe { self.elements.get_unchecked_mut(p.0 - 1) };
            (fst, snd)
        })
    }
    fn insert_into(
        root: &mut SparseVecNode,
        elements: &mut Vec<(A, B)>,
        block_capacity: usize,
        block_capacity_bits: u8,
        key: usize,
        entry: (A, B),
    ) {
        match root {
            SparseVecNode::Block(ref mut vec) => {
                if key < block_capacity {
                    match vec.get(key) {
                        Some(pointer) if pointer.0 != 0 => {
                            *unsafe { elements.get_unchecked_mut(pointer.0 - 1) } = entry;
                        },
                        _ => {
                            // 1-based index for elements
                            let pointer = Pointer(elements.len() + 1);
                            elements.push(entry);
                            Self::allocate(vec, key, || Pointer(0));
                            *unsafe { vec.get_unchecked_mut(key) } = pointer;
                        },
                    };
                } else {
                    let parent_shift = Shift(block_capacity_bits);
                    let mut parent = Self::sparse(parent_shift);
                    std::mem::swap(root, &mut parent);
                    if let SparseVecNode::SparseVec(shift, ref mut children) = root {
                        children.push(parent);
                        let (index, subindex) = Self::index_and_subindex(key, shift);
                        if index < block_capacity {
                            let pointer = Pointer(elements.len() + 1);
                            elements.push(entry);
                            Self::allocate(children, index, || Self::block());
                            let child = unsafe { children.get_unchecked_mut(index) };
                            if let SparseVecNode::Block(ref mut block) = child {
                                Self::allocate(block, subindex, || Pointer(0));
                                *unsafe { block.get_unchecked_mut(subindex) } = pointer;
                            }
                        } else {
                            Self::insert_into(
                                root,
                                elements,
                                block_capacity,
                                block_capacity_bits,
                                key,
                                entry,
                            )
                        }
                    }
                }
            },
            SparseVecNode::SparseVec(shift, ref mut children) => {
                let (index, subindex) = Self::index_and_subindex(key, shift);
                if index < block_capacity {
                    if shift.0 == block_capacity_bits {
                        Self::allocate(children, index, || Self::block())
                    } else {
                        let child_shift = Shift(shift.0 - block_capacity_bits);
                        Self::allocate(children, index, || Self::sparse(child_shift))
                    }
                    let child = unsafe { children.get_unchecked_mut(index) };
                    Self::insert_into(
                        child,
                        elements,
                        block_capacity,
                        block_capacity_bits,
                        subindex,
                        entry,
                    )
                } else {
                    let parent_shift = Shift(shift.0 + block_capacity_bits);
                    let mut parent = Self::sparse(parent_shift);
                    std::mem::swap(root, &mut parent);
                    if let SparseVecNode::SparseVec(shift, ref mut children) = root {
                        children.push(parent);
                        let (index, subindex) = Self::index_and_subindex(key, shift);
                        if index < block_capacity {
                            let child_shift = Shift(shift.0 + block_capacity_bits);
                            Self::allocate(children, index, || Self::sparse(child_shift));
                            let child = unsafe { children.get_unchecked_mut(index) };
                            Self::insert_into(
                                child,
                                elements,
                                block_capacity,
                                block_capacity_bits,
                                subindex,
                                entry,
                            )
                        } else {
                            Self::insert_into(
                                root,
                                elements,
                                block_capacity,
                                block_capacity_bits,
                                key,
                                entry,
                            )
                        }
                    }
                }
            },
        }
    }
    fn index_of(root: &SparseVecNode, key: usize) -> Option<&Pointer> {
        match root {
            SparseVecNode::Block(ref vec) => vec.get(key).filter(|p| p.0 != 0),
            SparseVecNode::SparseVec(shift, ref children) => {
                let (index, subindex) = Self::index_and_subindex(key, shift);
                match children.get(index) {
                    Some(child) => Self::index_of(child, subindex),
                    None => None,
                }
            },
        }
    }
    #[inline(always)]
    fn allocate<V: Clone>(vec: &mut Vec<V>, index: usize, mut value: impl FnMut() -> V) {
        if index >= vec.len() {
            vec.resize(index + 1, value());
        }
    }
    #[inline(always)]
    fn index_and_subindex(key: usize, shift: &Shift) -> (usize, usize) {
        let ushift = shift.0;
        let mask = (1 << ushift) - 1;
        let quotient = key >> ushift;
        let remainder = key & mask;
        (quotient, remainder)
    }
    #[inline(always)]
    fn block() -> SparseVecNode {
        SparseVecNode::Block(vec![])
    }
    #[inline(always)]
    fn sparse(shift: Shift) -> SparseVecNode {
        SparseVecNode::SparseVec(shift, vec![])
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn fixture() -> SparseVec<usize, usize> {
        Default::default()
    }

    #[test]
    fn empty() {
        #![allow(clippy::len_zero)]
        let map = fixture();
        assert!(map.is_empty());
        assert!(map.len() == 0);
    }

    #[test]
    fn read_write() {
        let mut map = fixture();
        map.insert(0, 21);
        assert_eq!(map.get(&0), Some(&21));

        // assert!(map.len() == 1);
        // assert!(!map.is_empty());
        map.insert(200, 42);
        assert_eq!(map.get(&0), Some(&21));
        assert_eq!(map.get(&100), None);
        assert_eq!(map.get(&200), Some(&42));

        // assert!(map.len() == 2);
        // assert!(!map.is_empty());
        map.insert(2000, 84);
        assert_eq!(map.get(&0), Some(&21));
        assert_eq!(map.get(&100), None);
        assert_eq!(map.get(&200), Some(&42));
        assert_eq!(map.get(&2000), Some(&84));

        map.insert(1_000_000_000, 128);
        assert_eq!(map.get(&0), Some(&21));
        assert_eq!(map.get(&100), None);
        assert_eq!(map.get(&200), Some(&42));
        assert_eq!(map.get(&2000), Some(&84));
        assert_eq!(map.get(&1_000_000_000), Some(&128));

        map.insert(1_000_000_000, 256);
        assert_eq!(map.get(&0), Some(&21));
        assert_eq!(map.get(&100), None);
        assert_eq!(map.get(&200), Some(&42));
        assert_eq!(map.get(&2000), Some(&84));
        assert_eq!(map.get(&1_000_000_000), Some(&256));

        map.clear();
        map.insert(1_000_000_000, 128);
        assert_eq!(map.get(&0), None);
        assert_eq!(map.get(&100), None);
        assert_eq!(map.get(&200), None);
        assert_eq!(map.get(&2000), None);
        assert_eq!(map.get(&1_000_000_000), Some(&128));
    }
    #[test]
    fn extend() {
        #![allow(clippy::len_zero)]
        let mut map1 = fixture();
        map1.insert(0, 0);
        map1.insert(1, 20);
        let mut map2 = fixture();
        map2.insert(0, 42);
        map2.insert(2, 60);
        map1.extend(map2);
        assert!(map1.get(&0).is_some_and(|v| *v == 42));
        assert!(map1.get(&1).is_some_and(|v| *v == 20));
        assert!(map1.get(&2).is_some_and(|v| *v == 60));
        assert!(map1.len() == 3);
        assert!(!map1.is_empty());
    }

    #[test]
    fn iter() {
        #[derive(Default, Clone, Copy, PartialEq, Eq, Debug)]
        struct CustomData(usize);
        impl From<usize> for CustomData {
            fn from(value: usize) -> Self {
                CustomData(value)
            }
        }
        impl From<CustomData> for usize {
            fn from(value: CustomData) -> Self {
                value.0
            }
        }

        let mut map = SparseVec::<CustomData, CustomData>::default();

        map.insert(CustomData(0), CustomData(0));
        map.insert(CustomData(1_000), CustomData(20));
        map.insert(CustomData(2_000_000), CustomData(42));
        assert_eq!(
            map.iter().collect::<Vec<_>>(),
            vec![
                &(CustomData(0), CustomData(0)),
                &(CustomData(1_000), CustomData(20)),
                &(CustomData(2_000_000), CustomData(42))
            ]
        );
    }

    #[test]
    fn memory() {
        type Byte = u8;
        const ALLOC: usize = 1_000_000;
        let mut map = SparseVec::<usize, Byte>::default();
        map.insert(ALLOC, 0);
        println!("bytes: {}", map.memory().bytes());
        assert!(map.memory().bytes() < ALLOC);
    }
}
