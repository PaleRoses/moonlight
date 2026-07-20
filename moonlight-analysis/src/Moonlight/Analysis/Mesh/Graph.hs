{-# LANGUAGE BangPatterns #-}

module Moonlight.Analysis.Mesh.Graph
  ( Graph(..)
  , DirectedPairOrientation(..)
  , FacePairVectorComponent(..)
  , FacePairIncidenceObstruction(..)
  , FaceDirectedPairIncidence
  , buildFaceDirectedPairIncidence
  , faceDirectedPairFaceCount
  , faceDirectedPairEntryCount
  , faceDirectedPairRange
  , faceDirectedPairIdAt
  , faceDirectedPairOrientationAt
  , edgeRange
  , pairMetricNormalFactor
  ) where

import Control.Monad.ST (ST, runST)
import Data.Foldable (traverse_)
import Data.Kind (Type)
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import Data.Word (Word8)

type Graph :: Type
data Graph = Graph
  { grFaces          :: !Int
  , grOffsets        :: !(VU.Vector Int)
  , grNbrs           :: !(VU.Vector Int)
  , grEdgePair       :: !(VU.Vector Int)
  , grPairA          :: !(VU.Vector Int)
  , grPairB          :: !(VU.Vector Int)
  , grPairHasAB      :: !(VU.Vector Word8)
  , grPairHasBA      :: !(VU.Vector Word8)
  , grPairBaseW      :: !(VU.Vector Double)
  , grFaceArea       :: !(VU.Vector Double)
  , grPairEdgeLen    :: !(VU.Vector Double)
  , grPairCenterDist :: !(VU.Vector Double)
  , grPairNx         :: !(VU.Vector Double)
  , grPairNy         :: !(VU.Vector Double)
  , grPairMetric11   :: !(VU.Vector Double)
  , grPairMetric12   :: !(VU.Vector Double)
  , grPairMetric22   :: !(VU.Vector Double)
  , grFaceOutDeg     :: !(VU.Vector Int)
  , grNewToOld       :: !(VU.Vector Int)
  , grOldToNew       :: !(VU.Vector Int)
  }

type DirectedPairOrientation :: Type
data DirectedPairOrientation
  = PairFromA
  | PairFromB
  deriving stock (Eq, Show)

type FacePairVectorComponent :: Type
data FacePairVectorComponent
  = PairFaceB
  | PairHasAB
  | PairHasBA
  | PairBaseWeight
  | PairEdgeLength
  | PairCenterDistance
  | PairNormalX
  | PairNormalY
  | PairMetric11
  | PairMetric12
  | PairMetric22
  deriving stock (Eq, Show)

type FacePairIncidenceObstruction :: Type
data FacePairIncidenceObstruction
  = NegativeFacePairIncidenceFaceCount !Int
  | FacePairIncidenceFaceCountOverflow !Int
  | FacePairVectorLengthMismatch !FacePairVectorComponent !Int !Int
  | FacePairEndpointOutOfRange !DirectedPairOrientation !Int !Int !Int
  | FacePairIncidenceSizeOverflow !Int
  | FaceOffsetLengthMismatch !Int !Int
  | FaceOffsetOriginMismatch !Int
  | FaceOffsetOutOfBounds !Int !Int !Int
  | FaceOffsetsNotMonotone !Int !Int !Int
  | FaceOffsetTerminalMismatch !Int !Int
  | DirectedEdgePairLengthMismatch !Int !Int
  | DirectedEdgeNeighborOutOfRange !Int !Int !Int
  | DirectedEdgePairOutOfRange !Int !Int !Int
  | DirectedEdgeEndpointMismatch !Int !Int !Int !Int !Int !Int
  | DirectedEdgeOrientationMissing !Int !Int !DirectedPairOrientation
  deriving stock (Eq, Show)

type FaceDirectedPairIncidence :: Type
data FaceDirectedPairIncidence = FaceDirectedPairIncidence
  !(VU.Vector Int)
  !(VU.Vector Int)
  !(VU.Vector Word8)


type DirectedEdgeValidation :: Type
data DirectedEdgeValidation
  = DirectedEdgesValid
  | DirectedEdgeInvalid !Int


type PairEndpointValidation :: Type
data PairEndpointValidation
  = PairEndpointsValid
  | PairEndpointInvalid !Int !Int


buildFaceDirectedPairIncidence
  :: Graph
  -> Either FacePairIncidenceObstruction FaceDirectedPairIncidence
buildFaceDirectedPairIncidence !graphValue
  | faceCount < 0 =
      Left (NegativeFacePairIncidenceFaceCount faceCount)
  | faceCount == maxBound =
      Left (FacePairIncidenceFaceCountOverflow faceCount)
  | pairCount > maxBound `quot` 2 =
      Left (FacePairIncidenceSizeOverflow pairCount)
  | otherwise = do
      validateCsrShape
      traverse_ validatePairVectorLength pairVectorLengths
      validatePairEndpoints PairFromA (grPairA graphValue)
      validatePairEndpoints PairFromB (grPairB graphValue)
      validateDirectedEdges
      pure
        ( if graphIncidenceIsCanonical graphValue
            then buildGraphOrderedFaceDirectedPairIncidence graphValue
            else buildValidatedIncidence
        )
  where
    !faceCount = grFaces graphValue
    !pairCount = VU.length (grPairA graphValue)

    pairVectorLengths =
      [ (PairFaceB, VU.length (grPairB graphValue))
      , (PairHasAB, VU.length (grPairHasAB graphValue))
      , (PairHasBA, VU.length (grPairHasBA graphValue))
      , (PairBaseWeight, VU.length (grPairBaseW graphValue))
      , (PairEdgeLength, VU.length (grPairEdgeLen graphValue))
      , (PairCenterDistance, VU.length (grPairCenterDist graphValue))
      , (PairNormalX, VU.length (grPairNx graphValue))
      , (PairNormalY, VU.length (grPairNy graphValue))
      , (PairMetric11, VU.length (grPairMetric11 graphValue))
      , (PairMetric12, VU.length (grPairMetric12 graphValue))
      , (PairMetric22, VU.length (grPairMetric22 graphValue))
      ]

    validatePairVectorLength (!component, !actualLength)
      | actualLength == pairCount = Right ()
      | otherwise =
          Left
            (FacePairVectorLengthMismatch component pairCount actualLength)

    validatePairEndpoints !orientation !endpoints =
      case firstInvalidPairEndpoint faceCount endpoints of
        PairEndpointsValid -> Right ()
        PairEndpointInvalid !pairIndex !faceIndex ->
          Left
            ( FacePairEndpointOutOfRange
                orientation
                pairIndex
                faceIndex
                faceCount
            )

    validateCsrShape
      | VU.length (grOffsets graphValue) /= faceCount + 1 =
          Left
            ( FaceOffsetLengthMismatch
                (faceCount + 1)
                (VU.length (grOffsets graphValue))
            )
      | VU.unsafeIndex (grOffsets graphValue) 0 /= 0 =
          Left
            (FaceOffsetOriginMismatch (VU.unsafeIndex (grOffsets graphValue) 0))
      | VU.length (grEdgePair graphValue) /= directedEdgeCount =
          Left
            ( DirectedEdgePairLengthMismatch
                directedEdgeCount
                (VU.length (grEdgePair graphValue))
            )
      | otherwise = do
          validateFaceOffsets
          let !terminalOffset =
                VU.unsafeIndex (grOffsets graphValue) faceCount
          if terminalOffset == directedEdgeCount
            then Right ()
            else
              Left
                (FaceOffsetTerminalMismatch directedEdgeCount terminalOffset)

    !directedEdgeCount = VU.length (grNbrs graphValue)

    validateFaceOffsets =
      VU.ifoldM'
        (\() !faceIndex !lowerOffset ->
          let !upperOffset =
                VU.unsafeIndex (grOffsets graphValue) (faceIndex + 1)
          in if lowerOffset < 0 || lowerOffset > directedEdgeCount
               then
                 Left
                   ( FaceOffsetOutOfBounds
                       faceIndex
                       lowerOffset
                       directedEdgeCount
                   )
               else
                 if upperOffset < lowerOffset
                   then
                     Left
                       ( FaceOffsetsNotMonotone
                           faceIndex
                           lowerOffset
                           upperOffset
                       )
                   else Right ()
        )
        ()
        (VU.take faceCount (grOffsets graphValue))

    validateDirectedEdges =
      case firstInvalidDirectedEdge graphValue faceCount pairCount of
        DirectedEdgesValid -> Right ()
        DirectedEdgeInvalid !edgeIndex ->
          validateDirectedEdgeAt
            (directedEdgeSourceFaceAt graphValue edgeIndex)
            edgeIndex

    validateDirectedEdgeAt !sourceFace !edgeIndex =
      let !neighborFace = VU.unsafeIndex (grNbrs graphValue) edgeIndex
          !pairIndex = VU.unsafeIndex (grEdgePair graphValue) edgeIndex
      in if neighborFace < 0 || neighborFace >= faceCount
           then
             Left
               ( DirectedEdgeNeighborOutOfRange
                   edgeIndex
                   neighborFace
                   faceCount
               )
           else
             if pairIndex < 0 || pairIndex >= pairCount
               then
                 Left
                   (DirectedEdgePairOutOfRange edgeIndex pairIndex pairCount)
               else
                 validateDirectedEdgeEndpoints
                   edgeIndex
                   sourceFace
                   neighborFace
                   pairIndex

    validateDirectedEdgeEndpoints
      !edgeIndex
      !sourceFace
      !neighborFace
      !pairIndex =
        let !faceA = VU.unsafeIndex (grPairA graphValue) pairIndex
            !faceB = VU.unsafeIndex (grPairB graphValue) pairIndex
            !matchesAB = sourceFace == faceA && neighborFace == faceB
            !matchesBA = sourceFace == faceB && neighborFace == faceA
            !hasAB = VU.unsafeIndex (grPairHasAB graphValue) pairIndex /= 0
            !hasBA = VU.unsafeIndex (grPairHasBA graphValue) pairIndex /= 0
        in case (matchesAB && hasAB, matchesBA && hasBA) of
             (True, _) -> Right ()
             (_, True) -> Right ()
             _
               | matchesAB ->
                   Left
                     ( DirectedEdgeOrientationMissing
                         edgeIndex
                         pairIndex
                         PairFromA
                     )
               | matchesBA ->
                   Left
                     ( DirectedEdgeOrientationMissing
                         edgeIndex
                         pairIndex
                         PairFromB
                     )
               | otherwise ->
                   Left
                     ( DirectedEdgeEndpointMismatch
                         edgeIndex
                         sourceFace
                         neighborFace
                         pairIndex
                         faceA
                         faceB
                     )

    buildValidatedIncidence =
      buildValidatedFaceDirectedPairIncidence graphValue faceCount


firstInvalidPairEndpoint
  :: Int
  -> VU.Vector Int
  -> PairEndpointValidation
firstInvalidPairEndpoint !faceCount =
  VU.ifoldl' inspectEndpoint PairEndpointsValid
  where
    inspectEndpoint
      !validation@PairEndpointInvalid {}
      !_pairIndex
      !_faceIndex = validation
    inspectEndpoint PairEndpointsValid !pairIndex !faceIndex
      | faceIndex < 0 || faceIndex >= faceCount =
          PairEndpointInvalid pairIndex faceIndex
      | otherwise = PairEndpointsValid


firstInvalidDirectedEdge
  :: Graph
  -> Int
  -> Int
  -> DirectedEdgeValidation
firstInvalidDirectedEdge !graphValue !faceCount !pairCount =
  VU.ifoldl'
    inspectEdge
    DirectedEdgesValid
    (grNbrs graphValue)
  where
    inspectEdge
      !validation@DirectedEdgeInvalid {}
      !_edgeIndex
      !_neighborFace = validation
    inspectEdge
      DirectedEdgesValid
      !edgeIndex
      !neighborFace =
        let !pairIndex = VU.unsafeIndex (grEdgePair graphValue) edgeIndex
        in if directedEdgeIsValid
                graphValue
                faceCount
                pairCount
                edgeIndex
                neighborFace
                pairIndex
             then DirectedEdgesValid
             else DirectedEdgeInvalid edgeIndex


directedEdgeIsValid
  :: Graph
  -> Int
  -> Int
  -> Int
  -> Int
  -> Int
  -> Bool
directedEdgeIsValid
  !graphValue
  !faceCount
  !pairCount
  !edgeIndex
  !neighborFace
  !pairIndex
  | neighborFace < 0 || neighborFace >= faceCount = False
  | pairIndex < 0 || pairIndex >= pairCount = False
  | otherwise =
      let !faceA = VU.unsafeIndex (grPairA graphValue) pairIndex
          !faceB = VU.unsafeIndex (grPairB graphValue) pairIndex
          !hasAB = VU.unsafeIndex (grPairHasAB graphValue) pairIndex /= 0
          !hasBA = VU.unsafeIndex (grPairHasBA graphValue) pairIndex /= 0
      in ( neighborFace == faceB
             && hasAB
             && directedEdgeBelongsToFace graphValue edgeIndex faceA
         )
           || ( neighborFace == faceA
                  && hasBA
                  && directedEdgeBelongsToFace graphValue edgeIndex faceB
              )


directedEdgeBelongsToFace :: Graph -> Int -> Int -> Bool
directedEdgeBelongsToFace !graphValue !edgeIndex !faceIndex =
  let !lowerOffset = VU.unsafeIndex (grOffsets graphValue) faceIndex
      !upperOffset = VU.unsafeIndex (grOffsets graphValue) (faceIndex + 1)
  in edgeIndex >= lowerOffset && edgeIndex < upperOffset
{-# INLINE directedEdgeBelongsToFace #-}


directedEdgeSourceFaceAt :: Graph -> Int -> Int
directedEdgeSourceFaceAt !graphValue !edgeIndex =
  VU.length
    (VU.takeWhile (<= edgeIndex) (grOffsets graphValue))
    - 1


graphIncidenceIsCanonical :: Graph -> Bool
graphIncidenceIsCanonical !graphValue =
  directedEdgeCount == presentDirectionCount
    && VU.ifoldl'
         inspectDirectedPairOrder
         True
         (grNbrs graphValue)
  where
    !directedEdgeCount = VU.length (grNbrs graphValue)
    !presentDirectionCount =
      VU.ifoldl'
        (\ !count !pairIndex !hasAB ->
          count
            + directionFlagCount hasAB
            + directionFlagCount
                (VU.unsafeIndex (grPairHasBA graphValue) pairIndex)
        )
        0
        (grPairHasAB graphValue)

    inspectDirectedPairOrder !False !_edgeIndex !_neighborFace = False
    inspectDirectedPairOrder !True !edgeIndex !_neighborFace =
      let !pairIndex = VU.unsafeIndex (grEdgePair graphValue) edgeIndex
          !orientationCode =
            graphDirectedPairOrientationCode graphValue edgeIndex pairIndex
          !sourceFace =
            if orientationCode == 0
              then VU.unsafeIndex (grPairA graphValue) pairIndex
              else VU.unsafeIndex (grPairB graphValue) pairIndex
          !lowerOffset = VU.unsafeIndex (grOffsets graphValue) sourceFace
      in edgeIndex == lowerOffset
           || graphDirectedPairCode graphValue edgeIndex
                > graphDirectedPairCode graphValue (edgeIndex - 1)


directionFlagCount :: Word8 -> Int
directionFlagCount !directionFlag =
  if directionFlag == 0 then 0 else 1
{-# INLINE directionFlagCount #-}


graphDirectedPairCode :: Graph -> Int -> Int
graphDirectedPairCode !graphValue !edgeIndex =
  let !pairIndex = VU.unsafeIndex (grEdgePair graphValue) edgeIndex
      !orientationCode =
        graphDirectedPairOrientationCode graphValue edgeIndex pairIndex
  in pairIndex * 2 + orientationCode
{-# INLINE graphDirectedPairCode #-}


graphDirectedPairOrientationCode :: Graph -> Int -> Int -> Int
graphDirectedPairOrientationCode !graphValue !edgeIndex !pairIndex =
  let !neighborFace = VU.unsafeIndex (grNbrs graphValue) edgeIndex
      !faceB = VU.unsafeIndex (grPairB graphValue) pairIndex
      !hasAB = VU.unsafeIndex (grPairHasAB graphValue) pairIndex /= 0
  in if neighborFace == faceB && hasAB then 0 else 1
{-# INLINE graphDirectedPairOrientationCode #-}


buildGraphOrderedFaceDirectedPairIncidence
  :: Graph
  -> FaceDirectedPairIncidence
buildGraphOrderedFaceDirectedPairIncidence !graphValue =
  FaceDirectedPairIncidence
    (grOffsets graphValue)
    (grEdgePair graphValue)
    ( VU.generate
        (VU.length (grNbrs graphValue))
        (\edgeIndex ->
          let !pairIndex = VU.unsafeIndex (grEdgePair graphValue) edgeIndex
          in fromIntegral
               (graphDirectedPairOrientationCode graphValue edgeIndex pairIndex)
        )
    )

buildValidatedFaceDirectedPairIncidence
  :: Graph
  -> Int
  -> FaceDirectedPairIncidence
buildValidatedFaceDirectedPairIncidence !graphValue !faceCount = runST $ do
  faceCounts <- VUM.replicate faceCount 0
  _ <-
    VU.ifoldM'
      (accumulatePairCount graphValue faceCounts)
      ()
      (grPairA graphValue)
  frozenFaceCounts <- VU.unsafeFreeze faceCounts
  let !faceOffsets = VU.scanl' (+) 0 frozenFaceCounts
      !entryCount = VU.sum frozenFaceCounts
  writeOffsets <- VU.thaw (VU.take faceCount faceOffsets)
  pairIds <- VUM.unsafeNew entryCount
  orientations <- VUM.unsafeNew entryCount
  _ <-
    VU.ifoldM'
      (writePairEntries graphValue writeOffsets pairIds orientations)
      ()
      (grPairA graphValue)
  frozenPairIds <- VU.unsafeFreeze pairIds
  frozenOrientations <- VU.unsafeFreeze orientations
  pure (FaceDirectedPairIncidence faceOffsets frozenPairIds frozenOrientations)

accumulatePairCount
  :: Graph
  -> VUM.MVector s Int
  -> ()
  -> Int
  -> Int
  -> ST s ()
accumulatePairCount !graphValue !faceCounts !() !pairIndex !faceA = do
  incrementWhenPresent
    faceCounts
    faceA
    (VU.unsafeIndex (grPairHasAB graphValue) pairIndex)
  incrementWhenPresent
    faceCounts
    (VU.unsafeIndex (grPairB graphValue) pairIndex)
    (VU.unsafeIndex (grPairHasBA graphValue) pairIndex)

incrementWhenPresent
  :: VUM.MVector s Int
  -> Int
  -> Word8
  -> ST s ()
incrementWhenPresent !faceCounts !faceIndex !isPresent =
  if isPresent == 0
    then pure ()
    else do
      oldCount <- VUM.unsafeRead faceCounts faceIndex
      VUM.unsafeWrite faceCounts faceIndex (oldCount + 1)

writePairEntries
  :: Graph
  -> VUM.MVector s Int
  -> VUM.MVector s Int
  -> VUM.MVector s Word8
  -> ()
  -> Int
  -> Int
  -> ST s ()
writePairEntries
  !graphValue
  !writeOffsets
  !pairIds
  !orientations
  !()
  !pairIndex
  !faceA = do
    writeWhenPresent
      writeOffsets
      pairIds
      orientations
      faceA
      (VU.unsafeIndex (grPairHasAB graphValue) pairIndex)
      pairIndex
      0
    writeWhenPresent
      writeOffsets
      pairIds
      orientations
      (VU.unsafeIndex (grPairB graphValue) pairIndex)
      (VU.unsafeIndex (grPairHasBA graphValue) pairIndex)
      pairIndex
      1

writeWhenPresent
  :: VUM.MVector s Int
  -> VUM.MVector s Int
  -> VUM.MVector s Word8
  -> Int
  -> Word8
  -> Int
  -> Word8
  -> ST s ()
writeWhenPresent
  !writeOffsets
  !pairIds
  !orientations
  !faceIndex
  !isPresent
  !pairIndex
  !orientation =
    if isPresent == 0
      then pure ()
      else do
        writeOffset <- VUM.unsafeRead writeOffsets faceIndex
        VUM.unsafeWrite pairIds writeOffset pairIndex
        VUM.unsafeWrite orientations writeOffset orientation
        VUM.unsafeWrite writeOffsets faceIndex (writeOffset + 1)

faceDirectedPairFaceCount :: FaceDirectedPairIncidence -> Int
faceDirectedPairFaceCount (FaceDirectedPairIncidence !offsets _ _) =
  VU.length offsets - 1

faceDirectedPairEntryCount :: FaceDirectedPairIncidence -> Int
faceDirectedPairEntryCount (FaceDirectedPairIncidence _ !pairIds _) =
  VU.length pairIds

faceDirectedPairRange :: FaceDirectedPairIncidence -> Int -> (Int, Int)
faceDirectedPairRange (FaceDirectedPairIncidence !offsets _ _) !faceIndex =
  ( VU.unsafeIndex offsets faceIndex
  , VU.unsafeIndex offsets (faceIndex + 1)
  )
{-# INLINE faceDirectedPairRange #-}

faceDirectedPairIdAt :: FaceDirectedPairIncidence -> Int -> Int
faceDirectedPairIdAt (FaceDirectedPairIncidence _ !pairIds _) !entryIndex =
  VU.unsafeIndex pairIds entryIndex
{-# INLINE faceDirectedPairIdAt #-}

faceDirectedPairOrientationAt
  :: FaceDirectedPairIncidence
  -> Int
  -> DirectedPairOrientation
faceDirectedPairOrientationAt
  (FaceDirectedPairIncidence _ _ !orientations)
  !entryIndex =
    if VU.unsafeIndex orientations entryIndex == 0
      then PairFromA
      else PairFromB
{-# INLINE faceDirectedPairOrientationAt #-}

pairMetricNormalFactor :: Graph -> Int -> Double
pairMetricNormalFactor !gr !p =
  let !nx = VU.unsafeIndex (grPairNx gr) p
      !ny = VU.unsafeIndex (grPairNy gr) p
      !g11 = VU.unsafeIndex (grPairMetric11 gr) p
      !g12 = VU.unsafeIndex (grPairMetric12 gr) p
      !g22 = VU.unsafeIndex (grPairMetric22 gr) p
      !q0 = g11 * nx + g12 * ny
      !q1 = g12 * nx + g22 * ny
  in max 0.0 (nx * q0 + ny * q1)
{-# INLINE pairMetricNormalFactor #-}

edgeRange :: Graph -> Int -> (Int, Int)
edgeRange !gr !i =
  ( VU.unsafeIndex (grOffsets gr) i
  , VU.unsafeIndex (grOffsets gr) (i + 1)
  )
{-# INLINE edgeRange #-}
