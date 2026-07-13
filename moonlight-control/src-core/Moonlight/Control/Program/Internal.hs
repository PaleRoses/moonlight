{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

-- | Raw representation of control programs.
--
-- The constructors here are exact: no smart construction, no normalization.
-- Equality and ordering on 'Program' are /canonical/ (computed on
-- 'normalize'd forms); 'structuralEq' and 'structuralCompare' expose the
-- representational versions for tests that need them.
module Moonlight.Control.Program.Internal
  ( Program (..),
    normalize,
    structuralEq,
    structuralCompare,
    seqSpine,
    orSpine,
    seqFromList,
    orFromList,
  )
where

import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty (..), (<|))
import Numeric.Natural (Natural)

import Moonlight.Control.Class (Control (..))

-- | The canonical deep embedding of the control algebra. All constructors
-- are O(1); composition never normalizes. Two programs are '==' when their
-- 'normalize'd forms coincide.
type Program :: Type -> Type -> Type
data Program ctx p
  = Skip
  | Phase !p
  | Seq (Program ctx p) (Program ctx p)
  | Or (Program ctx p) (Program ctx p)
  | UpTo !Natural (Program ctx p)
  | Attempt (Program ctx p)
  | Scoped !ctx (Program ctx p)
  deriving stock (Show, Functor, Foldable, Traversable)

instance Monoid ctx => Control (Program ctx p) where
  type PhaseOf (Program ctx p) = p
  type ContextOf (Program ctx p) = ctx
  skip = Skip
  phase = Phase
  andThen left right =
    case (left, right) of
      (Skip, _) -> right
      (_, Skip) -> left
      _ -> Seq left right
  orElse = Or
  upTo repeatCount body =
    case body of
      Skip -> Skip
      _
        | repeatCount == 0 -> Skip
        | otherwise -> UpTo repeatCount body
  attempt body =
    case body of
      Skip -> Skip
      _ -> Attempt body
  scoped context body =
    case body of
      Skip -> Skip
      _ -> Scoped context body

instance Monoid ctx => Applicative (Program ctx) where
  pure = Phase
  programF <*> programA = programF >>= \f -> fmap f programA

instance Monoid ctx => Monad (Program ctx) where
  program >>= substitutePhase =
    case program of
      Skip -> Skip
      Phase phaseValue -> substitutePhase phaseValue
      Seq left right -> Seq (left >>= substitutePhase) (right >>= substitutePhase)
      Or left right -> Or (left >>= substitutePhase) (right >>= substitutePhase)
      UpTo repeatCount body -> UpTo repeatCount (body >>= substitutePhase)
      Attempt body -> Attempt (body >>= substitutePhase)
      Scoped context body -> Scoped context (body >>= substitutePhase)

-- | Canonical equality: programs are equal when their normal forms coincide
-- structurally. O(s + t) in the sizes of the operands.
instance (Monoid ctx, Eq ctx, Eq p) => Eq (Program ctx p) where
  left == right = structuralEq (normalize left) (normalize right)

-- | Canonical ordering on normal forms. O(s + t).
instance (Monoid ctx, Ord ctx, Ord p) => Ord (Program ctx p) where
  compare left right = structuralCompare (normalize left) (normalize right)

-- | One-pass reduction to canonical form. O(n).
--
-- * 'Seq' spines are right-nested with 'Skip' segments eliminated.
-- * 'Or' spines are right-nested with 'Skip' branches /preserved/ — a
--   skipped branch is an observable alternative.
-- * @'UpTo' 0 x@, @'UpTo' n 'Skip'@, @'Attempt' 'Skip'@, and
--   @'Scoped' c 'Skip'@ reduce to 'Skip'.
-- * Adjacent scopes fuse: @'Scoped' a ('Scoped' b x)@ becomes
--   @'Scoped' (a '<>' b) x@.
-- * Nested 'Attempt' is /not/ collapsed (the inner attempt is
--   trace-observable).
normalize :: Monoid ctx => Program ctx p -> Program ctx p
normalize program =
  case program of
    Skip -> Skip
    Phase phaseValue -> Phase phaseValue
    Seq {} -> seqFromList (seqBranches program [])
    Or {} -> orFromList (orBranches program)
    UpTo repeatCount body ->
      case normalize body of
        Skip -> Skip
        normalBody
          | repeatCount == 0 -> Skip
          | otherwise -> UpTo repeatCount normalBody
    Attempt body ->
      case normalize body of
        Skip -> Skip
        normalBody -> Attempt normalBody
    Scoped context body ->
      case normalize body of
        Skip -> Skip
        Scoped innerContext innerBody -> Scoped (context <> innerContext) innerBody
        normalBody -> Scoped context normalBody

seqBranches :: Monoid ctx => Program ctx p -> [Program ctx p] -> [Program ctx p]
seqBranches program rest =
  case program of
    Seq left right -> seqBranches left (seqBranches right rest)
    _ ->
      case normalize program of
        Skip -> rest
        normalProgram -> normalProgram : rest

orBranches :: Monoid ctx => Program ctx p -> NonEmpty (Program ctx p)
orBranches program =
  go program []
  where
    go branch rest =
      case branch of
        Or left right -> go left (nonEmptyToList (go right rest))
        _ -> flattenNormalOr (normalize branch) rest
    flattenNormalOr normalBranch rest =
      case normalBranch of
        Or left right -> left <| flattenNormalOr right rest
        _ -> normalBranch :| rest

-- | Rebuild a right-nested sequence from in-order segments. @[]@ yields
-- 'Skip', a singleton yields its element. O(n).
seqFromList :: [Program ctx p] -> Program ctx p
seqFromList segments =
  case segments of
    [] -> Skip
    [single] -> single
    firstSegment : rest -> Seq firstSegment (seqFromList rest)

-- | Rebuild a right-nested choice from in-order branches; a singleton yields
-- its element. 'Skip' branches are preserved. O(n).
orFromList :: NonEmpty (Program ctx p) -> Program ctx p
orFromList (firstBranch :| rest) =
  case rest of
    [] -> firstBranch
    nextBranch : restBranches -> Or firstBranch (orFromList (nextBranch :| restBranches))

-- | The maximal in-order 'Seq' spine of a program; a non-'Seq' program is
-- its own singleton spine. O(spine length).
seqSpine :: Program ctx p -> NonEmpty (Program ctx p)
seqSpine program =
  go program []
  where
    go segment rest =
      case segment of
        Seq left right -> go left (nonEmptyToList (go right rest))
        _ -> segment :| rest

-- | The maximal in-order 'Or' spine of a program; a non-'Or' program is its
-- own singleton spine. O(spine length).
orSpine :: Program ctx p -> NonEmpty (Program ctx p)
orSpine program =
  go program []
  where
    go branch rest =
      case branch of
        Or left right -> go left (nonEmptyToList (go right rest))
        _ -> branch :| rest

nonEmptyToList :: NonEmpty a -> [a]
nonEmptyToList (firstValue :| rest) = firstValue : rest

-- | Representational equality: compares constructors exactly, with no
-- normalization. O(min(s, t)).
structuralEq :: (Eq ctx, Eq p) => Program ctx p -> Program ctx p -> Bool
structuralEq left right =
  case (left, right) of
    (Skip, Skip) -> True
    (Phase leftPhase, Phase rightPhase) -> leftPhase == rightPhase
    (Seq leftA leftB, Seq rightA rightB) ->
      structuralEq leftA rightA && structuralEq leftB rightB
    (Or leftA leftB, Or rightA rightB) ->
      structuralEq leftA rightA && structuralEq leftB rightB
    (UpTo leftCount leftBody, UpTo rightCount rightBody) ->
      leftCount == rightCount && structuralEq leftBody rightBody
    (Attempt leftBody, Attempt rightBody) -> structuralEq leftBody rightBody
    (Scoped leftContext leftBody, Scoped rightContext rightBody) ->
      leftContext == rightContext && structuralEq leftBody rightBody
    _ -> False

-- | Representational ordering companion to 'structuralEq'. O(min(s, t)).
structuralCompare :: (Ord ctx, Ord p) => Program ctx p -> Program ctx p -> Ordering
structuralCompare left right =
  case (left, right) of
    (Skip, Skip) -> EQ
    (Phase leftPhase, Phase rightPhase) -> compare leftPhase rightPhase
    (Seq leftA leftB, Seq rightA rightB) ->
      structuralCompare leftA rightA <> structuralCompare leftB rightB
    (Or leftA leftB, Or rightA rightB) ->
      structuralCompare leftA rightA <> structuralCompare leftB rightB
    (UpTo leftCount leftBody, UpTo rightCount rightBody) ->
      compare leftCount rightCount <> structuralCompare leftBody rightBody
    (Attempt leftBody, Attempt rightBody) -> structuralCompare leftBody rightBody
    (Scoped leftContext leftBody, Scoped rightContext rightBody) ->
      compare leftContext rightContext <> structuralCompare leftBody rightBody
    _ -> compare (constructorRank left) (constructorRank right)
  where
    constructorRank :: Program ctx p -> Int
    constructorRank program =
      case program of
        Skip -> 0
        Phase {} -> 1
        Seq {} -> 2
        Or {} -> 3
        UpTo {} -> 4
        Attempt {} -> 5
        Scoped {} -> 6
