{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Moonlight.Geometry.Tetrahedralization.Core
  ( SegmentIx
  , FacetTriIx
  , TetIx
  , HasXYZ(..)
  , CDT3DOptions(..)
  , defaultCDT3DOptions
  , Tetrahedralization(..)
  , EdgeKey
  , FaceKey
  , Tet(..)
  , Mesh(..)
  , PointLocation(..)
  , initMesh
  , pointOf
  , originOf
  , tetVerts
  , tetFacesRaw
  , tetFaceKeys
  , tetEdges
  , normalizeTet
  , addTetFresh
  , deleteTetById
  , neighborAcrossFace
  , incidentTetsAtEdge
  , edgeExists
  , canon2
  , canon3
  , mkArray
  , dedupSegments
  , splitSegmentsAtInputPoints
  , recoverSegments
  , insertSteinerPoint
  , extendWithSuperTet
  , insertPointById
  , extractTetrahedralization
  , validateMesh3D
  ) where

import Control.Monad (foldM, when)
import Data.Array (Array, listArray, (!))
import Data.Kind (Constraint, Type)
import Data.List (sortBy)
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Ord (comparing)
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import qualified Data.Map.Strict as M

import Moonlight.Core (adjacentPairs)
import Moonlight.Geometry.Predicate
  ( Point3, BBox3
  , orient3Sign, insphereSign
  , midpoint3
  , pointOnClosedSegment3, projectionParam3
  )

type SegmentIx :: Type
type SegmentIx = (Int, Int)
type FacetTriIx :: Type
type FacetTriIx = (Int, Int, Int)
type TetIx :: Type
type TetIx = (Int, Int, Int, Int)

type HasXYZ :: Type -> Type -> Constraint
class HasXYZ p a | p -> a where
  pointXYZ :: p -> Point3 a

instance HasXYZ (a, a, a) a where
  pointXYZ = id

type CDT3DOptions :: Type
data CDT3DOptions = CDT3DOptions
  { cdt3DUseMorton       :: !Bool
  , cdt3DMaxSegmentSplits :: !Int
  , cdt3DValidate        :: !Bool
  } deriving stock (Eq, Show)

defaultCDT3DOptions :: CDT3DOptions
defaultCDT3DOptions = CDT3DOptions
  { cdt3DUseMorton = True
  , cdt3DMaxSegmentSplits = 24
  , cdt3DValidate = False
  }

type Tetrahedralization :: Type -> Type
data Tetrahedralization a = Tetrahedralization
  { tdPoints     :: !(Array Int (Point3 a))
  , tdOrigin     :: !(Array Int (Maybe Int))
  , tdSegments   :: ![SegmentIx]
  , tdTetrahedra :: ![TetIx]
  } deriving stock (Eq, Show)

type EdgeKey :: Type
type EdgeKey = (Int, Int)
type FaceKey :: Type
type FaceKey = (Int, Int, Int)

type Tet :: Type
data Tet = Tet !Int !Int !Int !Int deriving stock (Eq, Ord, Show)

type Mesh :: Type -> Type
data Mesh a = Mesh
  { meshPoints      :: !(IM.IntMap (Point3 a))
  , meshOrigin      :: !(IM.IntMap (Maybe Int))
  , meshPointIndex  :: !(M.Map (Point3 a) Int)
  , meshTets        :: !(IM.IntMap Tet)
  , meshFaceMap     :: !(M.Map FaceKey [Int])
  , meshEdgeCount   :: !(M.Map EdgeKey Int)
  , meshEdgeTets    :: !(M.Map EdgeKey IS.IntSet)
  , meshSuper       :: !IS.IntSet
  , meshNextPid     :: !Int
  , meshNextTid     :: !Int
  , meshRecentTet   :: !(Maybe Int)
  } deriving stock (Show)

type PointLocation :: Type
data PointLocation
  = LocatedIn !Int
  | LocatedOnFace !Int !FaceKey
  | LocatedOnEdge !Int !EdgeKey
  | LocatedAtVertex !Int
  deriving stock (Eq, Show)

canon2 :: SegmentIx -> SegmentIx
canon2 (a, b)
  | a <= b = (a, b)
  | otherwise = (b, a)

canon3 :: (Int, Int, Int) -> (Int, Int, Int)
canon3 (a, b, c)
  | a <= b && b <= c = (a, b, c)
  | a <= c && c <= b = (a, c, b)
  | b <= a && a <= c = (b, a, c)
  | b <= c && c <= a = (b, c, a)
  | c <= a && a <= b = (c, a, b)
  | otherwise = (c, b, a)

mkArray :: [a] -> Array Int a
mkArray xs = listArray (0, max (-1) (length xs - 1)) xs

initMesh :: forall a. RealFloat a
         => [Point3 a] -> [Maybe Int] -> Int -> Int -> Int -> Int
         -> Either String (Mesh a)
initMesh pts origin s0 s1 s2 s3 = do
  let pointMap = IM.fromList (zip [0 :: Int ..] pts)
      originMap = IM.fromList (zip [0 :: Int ..] origin)
      indexMap = M.fromList (zip pts [0 :: Int ..])
      mesh0 = Mesh
        { meshPoints = pointMap, meshOrigin = originMap, meshPointIndex = indexMap
        , meshTets = IM.empty, meshFaceMap = M.empty
        , meshEdgeCount = M.empty, meshEdgeTets = M.empty
        , meshSuper = IS.fromList [s0, s1, s2, s3]
        , meshNextPid = length pts, meshNextTid = 0, meshRecentTet = Nothing }
  fst <$> addTetFresh (Tet s0 s1 s2 s3) mesh0

pointOf :: Mesh a -> Int -> Point3 a
pointOf mesh i = fromMaybe (error ("pointOf: missing point id " ++ show i)) (IM.lookup i (meshPoints mesh))

originOf :: Mesh a -> Int -> Maybe Int
originOf mesh i = fromMaybe Nothing (IM.lookup i (meshOrigin mesh))

tetVerts :: Tet -> (Int, Int, Int, Int)
tetVerts (Tet a b c d) = (a, b, c, d)

tetFacesRaw :: Tet -> [(Int, Int, Int)]
tetFacesRaw (Tet a b c d) = [(b, c, d), (a, c, d), (a, b, d), (a, b, c)]

tetFaceKeys :: Tet -> [FaceKey]
tetFaceKeys = map canon3 . tetFacesRaw

tetEdges :: Tet -> [EdgeKey]
tetEdges (Tet a b c d) =
  [ canon2 (a, b), canon2 (a, c), canon2 (a, d)
  , canon2 (b, c), canon2 (b, d), canon2 (c, d) ]

normalizeTet :: forall a. RealFloat a => Mesh a -> Tet -> Either String Tet
normalizeTet mesh (Tet a b c d) =
  case orient3Sign (pointOf mesh a) (pointOf mesh b) (pointOf mesh c) (pointOf mesh d) of
    GT -> pure (Tet a b c d)
    LT -> pure (Tet a b d c)
    EQ -> Left ("tetrahedralize3D: degenerate tetrahedron on vertices " ++ show (a, b, c, d))

addTetFresh :: forall a. RealFloat a => Tet -> Mesh a -> Either String (Mesh a, Int)
addTetFresh tet0 mesh0 = do
  tet <- normalizeTet mesh0 tet0
  let !tid = meshNextTid mesh0
      !mesh1 = mesh0
        { meshTets = IM.insert tid tet (meshTets mesh0)
        , meshFaceMap = foldl' (insertFaceTid tid) (meshFaceMap mesh0) (tetFaceKeys tet)
        , meshEdgeCount = foldl' insertEdgeCount (meshEdgeCount mesh0) (tetEdges tet)
        , meshEdgeTets = foldl' (insertEdgeTid tid) (meshEdgeTets mesh0) (tetEdges tet)
        , meshNextTid = tid + 1, meshRecentTet = Just tid }
  pure (mesh1, tid)

deleteTetById :: Int -> Mesh a -> Mesh a
deleteTetById tid mesh0 =
  case IM.lookup tid (meshTets mesh0) of
    Nothing -> mesh0
    Just tet -> mesh0
      { meshTets = IM.delete tid (meshTets mesh0)
      , meshFaceMap = foldl' (deleteFaceTid tid) (meshFaceMap mesh0) (tetFaceKeys tet)
      , meshEdgeCount = foldl' deleteEdgeCount (meshEdgeCount mesh0) (tetEdges tet)
      , meshEdgeTets = foldl' (deleteEdgeTid tid) (meshEdgeTets mesh0) (tetEdges tet) }

insertFaceTid :: Int -> M.Map FaceKey [Int] -> FaceKey -> M.Map FaceKey [Int]
insertFaceTid tid m fk = M.insertWith ins fk [tid] m
  where
    ins :: Eq a => [a] -> [a] -> [a]
    ins [x] ys | x `elem` ys = ys
    ins xs ys = xs ++ ys

deleteFaceTid :: Int -> M.Map FaceKey [Int] -> FaceKey -> M.Map FaceKey [Int]
deleteFaceTid tid m fk = case M.lookup fk m of
  Nothing -> m
  Just tids -> let tids' = filter (/= tid) tids in if null tids' then M.delete fk m else M.insert fk tids' m

insertEdgeCount :: M.Map EdgeKey Int -> EdgeKey -> M.Map EdgeKey Int
insertEdgeCount m ek = M.insertWith (+) ek 1 m

deleteEdgeCount :: M.Map EdgeKey Int -> EdgeKey -> M.Map EdgeKey Int
deleteEdgeCount m ek = case M.lookup ek m of
  Nothing -> m; Just 1 -> M.delete ek m; Just k -> M.insert ek (k - 1) m

insertEdgeTid :: Int -> M.Map EdgeKey IS.IntSet -> EdgeKey -> M.Map EdgeKey IS.IntSet
insertEdgeTid tid m ek = M.insertWith IS.union ek (IS.singleton tid) m

deleteEdgeTid :: Int -> M.Map EdgeKey IS.IntSet -> EdgeKey -> M.Map EdgeKey IS.IntSet
deleteEdgeTid tid m ek = case M.lookup ek m of
  Nothing -> m
  Just s -> let s' = IS.delete tid s in if IS.null s' then M.delete ek m else M.insert ek s' m

neighborAcrossFace :: FaceKey -> Int -> Mesh a -> Maybe Int
neighborAcrossFace fk tid mesh = do
  tids <- M.lookup fk (meshFaceMap mesh)
  listToMaybe [ t | t <- tids, t /= tid ]

incidentTetsAtEdge :: EdgeKey -> Mesh a -> [Int]
incidentTetsAtEdge ek mesh = maybe [] IS.toList (M.lookup ek (meshEdgeTets mesh))

edgeExists :: EdgeKey -> Mesh a -> Bool
edgeExists ek mesh = M.member ek (meshEdgeCount mesh)

dedupSegments :: [SegmentIx] -> [SegmentIx]
dedupSegments segs = M.keys (foldl' (\m e -> M.insert (canon2 e) () m) M.empty segs)

splitSegmentsAtInputPoints :: forall a. RealFloat a => [Point3 a] -> [SegmentIx] -> [SegmentIx]
splitSegmentsAtInputPoints pts segs =
  dedupSegments $ concatMap expand segs
  where
    pointArray = listArray (0, length pts - 1) pts
    indexed = zip [0 :: Int ..] pts
    expand (u0, v0)
      | u0 == v0 = []
      | otherwise =
          let (u, v) = canon2 (u0, v0)
              pu = pointArray ! u
              pv = pointArray ! v
              mids = [ i | (i, p) <- indexed, i /= u, i /= v, pointOnClosedSegment3 pu pv p ]
              chain = u : sortBy (comparing (projectionParam3 pu pv . (pointArray !))) mids ++ [v]
          in [ canon2 (a, b) | (a, b) <- adjacentPairs chain, a /= b ]

extendWithSuperTet :: forall a. RealFloat a => [Point3 a] -> BBox3 a -> ([Point3 a], [Maybe Int], Int, Int, Int, Int)
extendWithSuperTet pts (xmin, ymin, zmin, xmax, ymax, zmax) =
  let dx = xmax - xmin; dy = ymax - ymin; dz = zmax - zmin
      m = max dx (max dy dz) + 1
      l = dx + dy + dz + 3 * m + 1
      p0 = (xmin - m, ymin - m, zmin - m)
      p1 = (xmin - m + l, ymin - m, zmin - m)
      p2 = (xmin - m, ymin - m + l, zmin - m)
      p3 = (xmin - m, ymin - m, zmin - m + l)
      n = length pts
  in (pts ++ [p0, p1, p2, p3], map Just [0 .. n - 1] ++ replicate 4 Nothing, n, n + 1, n + 2, n + 3)

recoverSegments
  :: forall a. (RealFloat a, Show a)
  => Int -> [SegmentIx] -> Mesh a -> Either String (Mesh a, [SegmentIx])
recoverSegments maxDepth segs mesh0 = go mesh0 [] [ (canon2 s, 0 :: Int) | s <- segs ]
  where
    go !mesh !done [] = pure (mesh, dedupSegments (reverse done))
    go !mesh !done (((u, v), depth):todo)
      | u == v = go mesh done todo
      | edgeExists (canon2 (u, v)) mesh = go mesh ((u, v) : done) todo
      | depth >= maxDepth =
          Left ("tetrahedralize3D: failed to recover constrained segment " ++ show (u, v) ++ " within split depth " ++ show maxDepth)
      | otherwise = do
          let !pu = pointOf mesh u; !pv = pointOf mesh v; !pm = midpoint3 pu pv
          (mesh1, mid) <- insertSteinerPoint pm mesh
          if mid == u || mid == v
            then Left ("tetrahedralize3D: midpoint refinement stalled while recovering segment " ++ show (u, v))
            else go mesh1 done ((canon2 (u, mid), depth + 1) : (canon2 (mid, v), depth + 1) : todo)

insertSteinerPoint
  :: forall a. (RealFloat a, Show a) => Point3 a -> Mesh a -> Either String (Mesh a, Int)
insertSteinerPoint p mesh0 =
  case M.lookup p (meshPointIndex mesh0) of
    Just pid -> pure (mesh0, pid)
    Nothing -> do
      let !pid = meshNextPid mesh0
          !mesh1 = mesh0
            { meshPoints = IM.insert pid p (meshPoints mesh0)
            , meshOrigin = IM.insert pid Nothing (meshOrigin mesh0)
            , meshPointIndex = M.insert p pid (meshPointIndex mesh0)
            , meshNextPid = pid + 1 }
      mesh2 <- insertPointById mesh1 pid
      pure (mesh2, pid)

insertPointById :: forall a. (RealFloat a, Show a) => Mesh a -> Int -> Either String (Mesh a)
insertPointById mesh0 pid = do
  let !p = pointOf mesh0 pid
  loc <- locatePoint mesh0 p
  case loc of
    LocatedAtVertex _ -> pure mesh0
    _ -> do
      let (seeds, weakCavity) = cavitySeeds loc mesh0
      cavity <- buildCavity weakCavity seeds p mesh0
      let !boundary = boundaryFaces cavity mesh0
          !mesh1 = IS.foldl' (flip deleteTetById) mesh0 cavity
      when (null boundary) $ Left ("tetrahedralize3D: empty cavity for point " ++ show pid)
      foldM (addConeTet pid) mesh1 boundary

cavitySeeds :: PointLocation -> Mesh a -> (IS.IntSet, Bool)
cavitySeeds (LocatedIn tid) _ = (IS.singleton tid, False)
cavitySeeds (LocatedOnFace _ fk) mesh = (IS.fromList (fromMaybe [] (M.lookup fk (meshFaceMap mesh))), True)
cavitySeeds (LocatedOnEdge _ ek) mesh = (IS.fromList (incidentTetsAtEdge ek mesh), True)
cavitySeeds (LocatedAtVertex _) _ = (IS.empty, True)

addConeTet :: forall a. RealFloat a => Int -> Mesh a -> FaceKey -> Either String (Mesh a)
addConeTet pid mesh (a, b, c) = fst <$> addTetFresh (Tet a b c pid) mesh

boundaryFaces :: IS.IntSet -> Mesh a -> [FaceKey]
boundaryFaces cavity mesh = M.keys $ foldl' step M.empty (IS.toList cavity)
  where
    step acc tid = case IM.lookup tid (meshTets mesh) of
      Nothing -> acc
      Just tet -> foldl' (accFace tid) acc (tetFaceKeys tet)
    accFace tid acc fk = case neighborAcrossFace fk tid mesh of
      Just nbr | nbr `IS.member` cavity -> acc
      _ -> M.insert fk () acc

buildCavity :: forall a. RealFloat a => Bool -> IS.IntSet -> Point3 a -> Mesh a -> Either String IS.IntSet
buildCavity weak seeds p mesh
  | IS.null seeds = Left "tetrahedralize3D: empty cavity seed set"
  | otherwise = go seeds (IS.toList seeds)
  where
    accept tid = case IM.lookup tid (meshTets mesh) of
      Nothing -> False
      Just (Tet a b c d) ->
        let s = insphereSign (pointOf mesh a) (pointOf mesh b) (pointOf mesh c) (pointOf mesh d) p
        in if weak then s /= LT else s == GT
    go !seen [] = pure seen
    go !seen (tid:todo) = case IM.lookup tid (meshTets mesh) of
      Nothing -> go seen todo
      Just tet ->
        let nbrs = mapMaybe (\fk -> neighborAcrossFace fk tid mesh) (tetFaceKeys tet)
            fresh = [ n | n <- nbrs, n `IS.notMember` seen, accept n ]
        in go (foldl' (flip IS.insert) seen fresh) (fresh ++ todo)

locatePoint :: forall a. (RealFloat a, Show a) => Mesh a -> Point3 a -> Either String PointLocation
locatePoint mesh p =
  case meshRecentTet mesh <|> fmap fst (IM.lookupMin (meshTets mesh)) of
    Nothing -> Left "tetrahedralize3D: empty tetrahedralization"
    Just tid0 -> case walk 0 tid0 of
      Just loc -> pure loc
      Nothing -> case locateByScan mesh p of
        Just loc -> pure loc
        Nothing -> Left ("tetrahedralize3D: point location failed for point " ++ show p)
  where
    limit = 8 * IM.size (meshTets mesh) + 64
    walk !k !tid
      | k > limit = Nothing
      | otherwise = case IM.lookup tid (meshTets mesh) of
          Nothing -> Nothing
          Just tet -> case pointInTetRelation mesh p tid tet of
            Just loc -> Just loc
            Nothing -> case firstNegativeFace mesh p tet of
              Nothing -> Nothing
              Just fk -> case neighborAcrossFace fk tid mesh of
                Nothing -> Nothing
                Just tid' -> walk (k + 1) tid'

locateByScan :: RealFloat a => Mesh a -> Point3 a -> Maybe PointLocation
locateByScan mesh p = listToMaybe
  [ loc | (tid, tet) <- IM.toList (meshTets mesh), Just loc <- [pointInTetRelation mesh p tid tet] ]

pointInTetRelation :: RealFloat a => Mesh a -> Point3 a -> Int -> Tet -> Maybe PointLocation
pointInTetRelation mesh p tid (Tet a b c d) =
  let sA = orient3Sign p (pointOf mesh b) (pointOf mesh c) (pointOf mesh d)
      sB = orient3Sign (pointOf mesh a) p (pointOf mesh c) (pointOf mesh d)
      sC = orient3Sign (pointOf mesh a) (pointOf mesh b) p (pointOf mesh d)
      sD = orient3Sign (pointOf mesh a) (pointOf mesh b) (pointOf mesh c) p
      signs = [sA, sB, sC, sD]
      zeros = [ i | (i, s) <- zip [0 :: Int ..] signs, s == EQ ]
  in if any (== LT) signs then Nothing
     else case zeros of
       [] -> Just (LocatedIn tid)
       [i] -> Just (LocatedOnFace tid (faceOppIndex (Tet a b c d) i))
       [i, j] -> Just (LocatedOnEdge tid (edgeOppIndices (Tet a b c d) i j))
       [i, j, k] -> Just (LocatedAtVertex (vertexOppIndices (Tet a b c d) i j k))
       _ -> Just (LocatedAtVertex a)

firstNegativeFace :: RealFloat a => Mesh a -> Point3 a -> Tet -> Maybe FaceKey
firstNegativeFace mesh p (Tet a b c d) =
  fmap (faceOppIndex (Tet a b c d) . fst) $ listToMaybe
    [ t | t@(_, s) <- [ (0, orient3Sign p (pointOf mesh b) (pointOf mesh c) (pointOf mesh d))
                       , (1, orient3Sign (pointOf mesh a) p (pointOf mesh c) (pointOf mesh d))
                       , (2, orient3Sign (pointOf mesh a) (pointOf mesh b) p (pointOf mesh d))
                       , (3, orient3Sign (pointOf mesh a) (pointOf mesh b) (pointOf mesh c) p) ]
        , s == LT ]

faceOppIndex :: Tet -> Int -> FaceKey
faceOppIndex (Tet a b c d) i = canon3 $ case i of
  0 -> (b, c, d); 1 -> (a, c, d); 2 -> (a, b, d); _ -> (a, b, c)

edgeOppIndices :: Tet -> Int -> Int -> EdgeKey
edgeOppIndices tet@(Tet a b _ _) i j =
  case remainingTetVertices tet [i, j] of
    [u, v] -> canon2 (u, v)
    _ -> canon2 (a, b)

vertexOppIndices :: Tet -> Int -> Int -> Int -> Int
vertexOppIndices tet@(Tet a _ _ _) i j k =
  case remainingTetVertices tet [i, j, k] of
    [vertexValue] -> vertexValue
    _ -> a

remainingTetVertices :: Tet -> [Int] -> [Int]
remainingTetVertices (Tet a b c d) skippedIndices =
  [ vertexValue
  | (indexValue, vertexValue) <- zip [0 :: Int ..] [a, b, c, d]
  , indexValue `notElem` skippedIndices
  ]

extractTetrahedralization :: Mesh a -> [SegmentIx] -> Tetrahedralization a
extractTetrahedralization mesh segs =
  let !superSet = meshSuper mesh
      keepIds = [ i | i <- IM.keys (meshPoints mesh), i `IS.notMember` superSet ]
      remap = M.fromList (zip keepIds [0 :: Int ..])
      remapId i = fromMaybe (error ("extractTetrahedralization: missing remap for point " ++ show i)) (M.lookup i remap)
      pts = [ pointOf mesh i | i <- keepIds ]
      org = [ originOf mesh i | i <- keepIds ]
      tets = [ let (tetA, tetB, tetC, tetD) = tetVerts tet in (remapId tetA, remapId tetB, remapId tetC, remapId tetD)
             | tet <- IM.elems (meshTets mesh)
             , let (a, b, c, d) = tetVerts tet
             , all (`IS.notMember` superSet) [a, b, c, d] ]
      segs' = [ (remapId u, remapId v) | (u, v) <- dedupSegments segs, all (`M.member` remap) [u, v] ]
  in Tetrahedralization (mkArray pts) (mkArray org) segs' tets

validateMesh3D :: forall a. RealFloat a => Mesh a -> [SegmentIx] -> Either String ()
validateMesh3D mesh segs = do
  mapM_ checkTet (IM.toList (meshTets mesh))
  mapM_ checkFace (M.toList (meshFaceMap mesh))
  mapM_ checkSeg segs
  where
    checkTet (tid, Tet a b c d) =
      case orient3Sign (pointOf mesh a) (pointOf mesh b) (pointOf mesh c) (pointOf mesh d) of
        GT -> pure ()
        _ -> Left ("validateMesh3D: non-positive tetrahedron at id " ++ show tid)
    checkFace :: (FaceKey, [Int]) -> Either String ()
    checkFace (fk, tids)
      | null tids || length tids > 2 = Left ("validateMesh3D: invalid face incidence " ++ show fk ++ " -> " ++ show tids)
      | otherwise = pure ()
    checkSeg e
      | edgeExists (canon2 e) mesh = pure ()
      | otherwise = Left ("validateMesh3D: missing constrained segment edge " ++ show e)

infixl 3 <|>
(<|>) :: Maybe a -> Maybe a -> Maybe a
(<|>) (Just x) _ = Just x
(<|>) Nothing y = y
