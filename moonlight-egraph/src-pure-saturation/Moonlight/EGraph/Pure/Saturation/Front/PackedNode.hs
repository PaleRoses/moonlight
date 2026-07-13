{-# LANGUAGE TypeFamilies #-}

-- | Packed-key carrier for front DSL nodes whose 'Ord' is bit-identical to the reflective 'Node' ordering.
module Moonlight.EGraph.Pure.Saturation.Front.PackedNode
  ( PackedNode,
    packNode,
    packedNode,
    packedSortKey,
    packedTag,
    packedChildren,
    packAnalysisSpec,
    packAnalysisCostAlgebra,
    packTheorySpec,
    packFix,
    unpackFix,
    unpackExtractionResult,
    packPattern,
    unpackPattern,
    encodeSortKey,
    compareChildArrays,
  )
where

import Data.Bits (shiftR, (.&.), (.|.))
import Data.ByteString.Short (ShortByteString)
import Data.ByteString.Short qualified as ShortByteString
import Data.Char (ord)
import Data.Fix (Fix (..))
import Data.Kind (Type)
import Data.Primitive.SmallArray
  ( SmallArray,
    indexSmallArray,
    sizeofSmallArray,
    smallArrayFromList,
  )
import Data.Word (Word8)
import GHC.TypeLits (Symbol)
import Moonlight.Core
  ( HasConstructorTag (..),
    Pattern (..),
    StructuralLaw (..),
    TheorySpec (..),
    ZipMatch (..),
  )
import Moonlight.EGraph.Pure.Analysis
  ( AnalysisSpec (..),
  )
import Moonlight.EGraph.Pure.Extraction
  ( AnalysisCostAlgebra (..),
    ExtractionResult (..),
  )
import Moonlight.Rewrite.DSL
  ( Node (..),
    NodeTag,
    RewriteSignature (..),
    SortWitness,
    nodeChildren,
    sortWitnessName,
  )

type PackedNode :: (Symbol -> (Symbol -> Type) -> Type) -> Type -> Type
data PackedNode sig a = PackedNode
  { packedSortKey :: !ShortByteString,
    packedTag :: !(NodeTag sig),
    packedChildren :: !(SmallArray a),
    packedNode :: !(Node sig a)
  }

packNode :: RewriteSignature sig => Node sig a -> PackedNode sig a
packNode nodeValue =
  case nodeValue of
    Node sigNode ->
      PackedNode
        { packedSortKey = sortWitnessKey (nodeResultSort sigNode),
          packedTag = nodeTag sigNode,
          packedChildren = smallArrayFromList (nodeChildren sigNode),
          packedNode = nodeValue
        }

repackKeeping :: RewriteSignature sig => ShortByteString -> NodeTag sig -> Node sig a -> PackedNode sig a
repackKeeping sortKey tagValue nodeValue =
  case nodeValue of
    Node sigNode ->
      PackedNode
        { packedSortKey = sortKey,
          packedTag = tagValue,
          packedChildren = smallArrayFromList (nodeChildren sigNode),
          packedNode = nodeValue
        }

sortWitnessKey :: SortWitness sort -> ShortByteString
sortWitnessKey witness =
  encodeSortKey (sortWitnessName witness)

encodeSortKey :: String -> ShortByteString
encodeSortKey sortString =
  ShortByteString.pack (concatMap encodeCharBytes sortString)

encodeCharBytes :: Char -> [Word8]
encodeCharBytes character
  | codePoint < 0x80 =
      [fromIntegral codePoint]
  | codePoint < 0x800 =
      [ fromIntegral (0xC0 .|. shiftR codePoint 6),
        continuationByteAt 0 codePoint
      ]
  | codePoint < 0x10000 =
      [ fromIntegral (0xE0 .|. shiftR codePoint 12),
        continuationByteAt 6 codePoint,
        continuationByteAt 0 codePoint
      ]
  | otherwise =
      [ fromIntegral (0xF0 .|. shiftR codePoint 18),
        continuationByteAt 12 codePoint,
        continuationByteAt 6 codePoint,
        continuationByteAt 0 codePoint
      ]
  where
    codePoint = ord character

continuationByteAt :: Int -> Int -> Word8
continuationByteAt bitOffset codePoint =
  fromIntegral (0x80 .|. (shiftR codePoint bitOffset .&. 0x3F))

compareChildArrays :: Ord a => SmallArray a -> SmallArray a -> Ordering
compareChildArrays leftChildren rightChildren =
  let leftLength = sizeofSmallArray leftChildren
      rightLength = sizeofSmallArray rightChildren
      go index
        | index >= leftLength || index >= rightLength =
            compare leftLength rightLength
        | otherwise =
            compare (indexSmallArray leftChildren index) (indexSmallArray rightChildren index)
              <> go (index + 1)
   in go 0

instance (Ord (NodeTag sig), Ord a) => Eq (PackedNode sig a) where
  leftNode == rightNode =
    compare leftNode rightNode == EQ

instance (Ord (NodeTag sig), Ord a) => Ord (PackedNode sig a) where
  compare leftNode rightNode =
    compare (packedSortKey leftNode) (packedSortKey rightNode)
      <> compare (packedTag leftNode) (packedTag rightNode)
      <> compareChildArrays (packedChildren leftNode) (packedChildren rightNode)

instance (RewriteSignature sig, Show (NodeTag sig), Show a) => Show (PackedNode sig a) where
  showsPrec precedence =
    showsPrec precedence . packedNode

instance RewriteSignature sig => Functor (PackedNode sig) where
  fmap transform packed =
    PackedNode
      { packedSortKey = packedSortKey packed,
        packedTag = packedTag packed,
        packedChildren = fmap transform (packedChildren packed),
        packedNode = fmap transform (packedNode packed)
      }

instance Foldable (PackedNode sig) where
  foldMap transform =
    foldMap transform . packedChildren

instance RewriteSignature sig => Traversable (PackedNode sig) where
  traverse transform packed =
    fmap
      (repackKeeping (packedSortKey packed) (packedTag packed))
      (traverse transform (packedNode packed))

instance (RewriteSignature sig, ZipMatch (Node sig)) => ZipMatch (PackedNode sig) where
  zipMatch leftNode rightNode =
    fmap packNode (zipMatch (packedNode leftNode) (packedNode rightNode))

instance (RewriteSignature sig, Ord (NodeTag sig)) => HasConstructorTag (PackedNode sig) where
  type ConstructorTag (PackedNode sig) = NodeTag sig

  constructorTag =
    packedTag

packAnalysisSpec :: AnalysisSpec (Node sig) analysis -> AnalysisSpec (PackedNode sig) analysis
packAnalysisSpec source =
  AnalysisSpec
    { asMake = asMake source . packedNode,
      asJoin = asJoin source,
      asJoinChanged = asJoinChanged source
    }

packAnalysisCostAlgebra ::
  AnalysisCostAlgebra (Node sig) analysis cost ->
  AnalysisCostAlgebra (PackedNode sig) analysis cost
packAnalysisCostAlgebra (AnalysisCostAlgebra computeCost) =
  AnalysisCostAlgebra
    ( \analysis nodeValue ->
        computeCost analysis (packedNode nodeValue)
    )

packTheorySpec :: RewriteSignature sig => TheorySpec (Node sig) -> TheorySpec (PackedNode sig)
packTheorySpec source =
  TheorySpec
    { tsClassify =
        \packed ->
          case tsClassify source (packedNode packed) of
            Ordinary ->
              Ordinary
            CommutativeBinary law ->
              CommutativeBinary (\left right -> packNode (law left right))
    }

packFix :: RewriteSignature sig => Fix (Node sig) -> Fix (PackedNode sig)
packFix (Fix nodeValue) =
  Fix (packNode (fmap packFix nodeValue))

unpackFix :: RewriteSignature sig => Fix (PackedNode sig) -> Fix (Node sig)
unpackFix (Fix packed) =
  Fix (fmap unpackFix (packedNode packed))

unpackExtractionResult ::
  RewriteSignature sig =>
  ExtractionResult (PackedNode sig) cost ->
  ExtractionResult (Node sig) cost
unpackExtractionResult result =
  ExtractionResult
    { erTerm = unpackFix (erTerm result),
      erCost = erCost result,
      erClass = erClass result
    }

packPattern :: RewriteSignature sig => Pattern (Node sig) -> Pattern (PackedNode sig)
packPattern patternValue =
  case patternValue of
    PatternVar patternVar ->
      PatternVar patternVar
    PatternNode nodeValue ->
      PatternNode (packNode (fmap packPattern nodeValue))

unpackPattern :: RewriteSignature sig => Pattern (PackedNode sig) -> Pattern (Node sig)
unpackPattern patternValue =
  case patternValue of
    PatternVar patternVar ->
      PatternVar patternVar
    PatternNode packed ->
      PatternNode (fmap unpackPattern (packedNode packed))
