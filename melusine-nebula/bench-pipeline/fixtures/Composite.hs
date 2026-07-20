module BenchPipeline.Composite where

data Review a
  = Reject
  | Approve a
  | Reconsider a
  deriving (Eq, Show)

data Batch a
  = BatchEnd
  | BatchNext a (Batch a)
  deriving (Eq, Show)

data ReviewTree a
  = ReviewLeaf a
  | ReviewFork (ReviewTree a) (ReviewTree a)
  deriving (Eq, Show)

data Route a b
  = Primary a
  | Secondary b
  deriving (Eq, Show)

data PipelineSummary = PipelineSummary Int Int Int
  deriving (Eq, Show)

inc :: Int -> Int
inc value = value + 1

dbl :: Int -> Int
dbl value = value * 2

incDouble :: [Int] -> [Int]
incDouble xs = map inc (map dbl xs)

incDoubleReversed :: [Int] -> [Int]
incDoubleReversed xs = map inc (map dbl (reverse xs))

eitherKnown :: Either a b -> Bool
eitherKnown e = case e of
  Left x -> either (const True) (const True) e
  Right y -> either (const True) (const True) e

routeKnown :: Route a b -> Bool
routeKnown routeValue = case routeValue of
  Primary primaryValue -> routeFold (const True) (const True) routeValue
  Secondary secondaryValue -> routeFold (const True) (const True) routeValue

routeFold :: (a -> result) -> (b -> result) -> Route a b -> result
routeFold onPrimary onSecondary routeValue = case routeValue of
  Primary primaryValue -> onPrimary primaryValue
  Secondary secondaryValue -> onSecondary secondaryValue

mapBatch :: (a -> b) -> Batch a -> Batch b
mapBatch transform batch = case batch of
  BatchEnd -> BatchEnd
  BatchNext value rest -> BatchNext (transform value) (mapBatch transform rest)

appendBatch :: Batch a -> Batch a -> Batch a
appendBatch leftBatch rightBatch = case leftBatch of
  BatchEnd -> rightBatch
  BatchNext value rest -> BatchNext value (appendBatch rest rightBatch)

batchToList :: Batch a -> [a]
batchToList batch = case batch of
  BatchEnd -> []
  BatchNext value rest -> value : batchToList rest

listToBatch :: [a] -> Batch a
listToBatch values = case values of
  [] -> BatchEnd
  value : rest -> BatchNext value (listToBatch rest)

filterApproved :: (a -> Review a) -> [a] -> [a]
filterApproved review values = case values of
  [] -> []
  value : rest -> case review value of
    Reject -> filterApproved review rest
    Approve approved -> approved : filterApproved review rest
    Reconsider revised -> case review revised of
      Reject -> filterApproved review rest
      Approve approved -> approved : filterApproved review rest
      Reconsider approved -> approved : filterApproved review rest

filterApprovedBatch :: (a -> Review a) -> Batch a -> Batch a
filterApprovedBatch review batch = case batch of
  BatchEnd -> BatchEnd
  BatchNext value rest -> case review value of
    Reject -> filterApprovedBatch review rest
    Approve approved -> BatchNext approved (filterApprovedBatch review rest)
    Reconsider revised -> case review revised of
      Reject -> filterApprovedBatch review rest
      Approve approved -> BatchNext approved (filterApprovedBatch review rest)
      Reconsider approved -> BatchNext approved (filterApprovedBatch review rest)

refreshRoute :: Route [Int] [Int] -> [Int]
refreshRoute routeValue = case routeValue of
  Primary values -> case values of
    [] -> map inc (map dbl values)
    firstValue : rest -> map inc (map dbl (firstValue : rest))
  Secondary values -> case values of
    [] -> map inc (map dbl values)
    firstValue : rest -> map inc (map dbl (firstValue : rest))

refreshReviewed :: (Int -> Review Int) -> [Int] -> [Int]
refreshReviewed review values = case values of
  [] -> map inc (map dbl values)
  value : rest -> case review value of
    Reject -> refreshReviewed review rest
    Approve approved -> map inc (map dbl (approved : rest))
    Reconsider revised -> case review revised of
      Reject -> refreshReviewed review rest
      Approve approved -> map inc (map dbl (approved : rest))
      Reconsider approved -> map inc (map dbl (approved : rest))

refreshBatch :: (Int -> Review Int) -> Batch Int -> Batch Int
refreshBatch review batch = case batch of
  BatchEnd -> BatchEnd
  BatchNext value rest -> case review value of
    Reject -> refreshBatch review rest
    Approve approved ->
      appendBatch
        (listToBatch (map inc (map dbl [approved])))
        (refreshBatch review rest)
    Reconsider revised -> case review revised of
      Reject -> refreshBatch review rest
      Approve approved ->
        appendBatch
          (listToBatch (map inc (map dbl [approved])))
          (refreshBatch review rest)
      Reconsider approved ->
        appendBatch
          (listToBatch (map inc (map dbl [approved])))
          (refreshBatch review rest)

filterReviewTree :: (a -> Review a) -> ReviewTree a -> [a]
filterReviewTree review tree = case tree of
  ReviewLeaf value -> case review value of
    Reject -> []
    Approve approved -> [approved]
    Reconsider revised -> case review revised of
      Reject -> []
      Approve approved -> [approved]
      Reconsider approved -> [approved]
  ReviewFork leftTree rightTree ->
    filterReviewTree review leftTree ++ filterReviewTree review rightTree

refreshReviewTree :: (Int -> Review Int) -> ReviewTree Int -> ReviewTree Int
refreshReviewTree review tree = case tree of
  ReviewLeaf value -> case review value of
    Reject -> ReviewLeaf value
    Approve approved -> case map inc (map dbl [approved]) of
      [] -> ReviewLeaf approved
      refreshed : rest -> ReviewLeaf refreshed
    Reconsider revised -> case review revised of
      Reject -> ReviewLeaf value
      Approve approved -> case map inc (map dbl [approved]) of
        [] -> ReviewLeaf approved
        refreshed : rest -> ReviewLeaf refreshed
      Reconsider approved -> case map inc (map dbl [approved]) of
        [] -> ReviewLeaf approved
        refreshed : rest -> ReviewLeaf refreshed
  ReviewFork leftTree rightTree ->
    ReviewFork
      (refreshReviewTree review leftTree)
      (refreshReviewTree review rightTree)

routeReviewed ::
  (left -> Review left) ->
  (right -> Review right) ->
  Route left right ->
  Route left right
routeReviewed reviewLeft reviewRight routeValue = case routeValue of
  Primary value -> case reviewLeft value of
    Reject -> Primary value
    Approve approved -> Primary approved
    Reconsider revised -> case reviewLeft revised of
      Reject -> Primary value
      Approve approved -> Primary approved
      Reconsider approved -> Primary approved
  Secondary value -> case reviewRight value of
    Reject -> Secondary value
    Approve approved -> Secondary approved
    Reconsider revised -> case reviewRight revised of
      Reject -> Secondary value
      Approve approved -> Secondary approved
      Reconsider approved -> Secondary approved

summarizeReviews :: (a -> Review a) -> [a] -> PipelineSummary
summarizeReviews review values = case values of
  [] -> PipelineSummary 0 0 0
  value : rest -> case review value of
    Reject -> addRejected (summarizeReviews review rest)
    Approve approved -> addApproved (summarizeReviews review rest)
    Reconsider revised -> case review revised of
      Reject -> addRejected (summarizeReviews review rest)
      Approve approved -> addApproved (summarizeReviews review rest)
      Reconsider approved -> addReconsidered (summarizeReviews review rest)

addRejected :: PipelineSummary -> PipelineSummary
addRejected summaryValue = case summaryValue of
  PipelineSummary rejected approved reconsidered ->
    PipelineSummary (rejected + 1) approved reconsidered

addApproved :: PipelineSummary -> PipelineSummary
addApproved summaryValue = case summaryValue of
  PipelineSummary rejected approved reconsidered ->
    PipelineSummary rejected (approved + 1) reconsidered

addReconsidered :: PipelineSummary -> PipelineSummary
addReconsidered summaryValue = case summaryValue of
  PipelineSummary rejected approved reconsidered ->
    PipelineSummary rejected approved (reconsidered + 1)
