{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module AutomataLaws
  ( tests,
  )
where

import Control.Comonad.Cofree (Cofree (..))
import Data.Fix (Fix (..))
import Data.Functor.Base (ListF (..), TreeF (..))
import Data.Functor.Identity (Identity (..))
import Data.Kind (Type)
import Moonlight.Algebra
  ( BooleanAlgebra (complement),
    JoinSemilattice (join),
    MeetSemilattice (meet),
  )
import Moonlight.Core (IsLawName (..), constructorLawName)
import Moonlight.Automata.Effect.LawNames (LawName (..), lawName)
import Moonlight.Automata.Effect.Laws
  ( Rose (..),
    complementAcceptanceLaw,
    denotationalComplementHomomorphismLaw,
    denotationalIntersectionHomomorphismLaw,
    denotationalUnionHomomorphismLaw,
    dependentDBTAIsZygoLaw,
    listFix,
    productDBTALaw,
    rootAttributeProjectionLaw,
    topDownAnnotationLaw,
    topDownAnnotatedAttributeLaw,
    topDownAttributeProjectionLaw,
    topDownFoldLaw,
    topDownStateProjectionLaw,
    treeFix,
    unionAcceptanceLaw,
    intersectionAcceptanceLaw,
    unlistFix,
    untreeFix,
  )
import Moonlight.Automata.Pure.Algebra (annotateBottomUp)
import Moonlight.Automata.Pure.Coalgebra
  ( annotateTopDown,
    annotateTopDownWithAttribute,
    inheritedAttribute,
    projectAttributeAnnotation,
    rootAttribute,
    topDownFold,
  )
import Moonlight.Automata.Pure.Core
  ( Acceptance (..),
    AcceptingDBTA (..),
    DBTA (..),
    TopDownTA (..),
  )
import Moonlight.Automata.Pure.Language
  ( Language (..),
    treeLanguageFromAcceptingDBTA,
  )
import Moonlight.Automata.Pure.Transducer
  ( BottomUpTransducer (..),
    LookaheadMacroTreeTransducer (..),
    MacroTreeTransducer (..),
    TreeContext,
    TopDownTransducer (..),
    composeBottomUp,
    composeTopDown,
    contextHole,
    contextLayer,
    foldTreeContext,
    runBottomUpTransducer,
    runLookaheadMacroTreeTransducer,
    runMacroTreeTransducer,
    runTopDownTransducer,
    substituteTreeContext,
  )
import Data.Functor.Foldable (cata)
import Numeric.Natural (Natural)
import Moonlight.Pale.Test.LawSuite
  ( QuickCheckLawBundle,
    lawSuiteGroup,
    quickCheckLawBundle,
    quickCheckLawBundleGroup,
    quickCheckLawDefinition,
  )
import Test.Tasty (TestTree, localOption)
import qualified Test.Tasty.QuickCheck as QC

type AutomataLawName :: Type
data AutomataLawName
  = NamedAutomataLaw LawName
  | TopDownDepthAttribute
  | TopDownFoldDepthAttribute
  | TopDownAnnotatedDepthAttribute
  | TopDownStateProjectionRoot
  | TopDownAttributeProjectionRoot
  | TopDownRootAttribute
  | TreeContextHoleFold
  | TreeContextLayerFold
  | TreeContextSubstituteAssociative
  | BottomUpTransducerIdentityList
  | BottomUpTransducerIdentityTree
  | BottomUpTransducerComposeTree
  | TopDownTransducerIdentityList
  | TopDownTransducerDepthRelabel
  | TopDownTransducerPrune
  | TopDownTransducerCompose
  | MacroTreeTransducerIdentity
  | MacroTreeTransducerDuplicateChildren
  | LookaheadMacroTreeTransducerLabels
  | BottomUpAnnotationRoot
  | BottomUpTransducerIncrement
  | BottomUpTransducerCompose
  | TreeLanguageJoinCommutative
  | TreeLanguageMeetCommutative
  | TreeLanguageAbsorptionJoin
  | TreeLanguageAbsorptionMeet
  | TreeLanguageComplementJoin
  | TreeLanguageComplementMeet
  deriving stock (Eq, Ord, Show)

instance IsLawName AutomataLawName where
  lawNameText automataLawName =
    case automataLawName of
      NamedAutomataLaw namedLaw -> lawName namedLaw
      specificLawName -> constructorLawName (show specificLawName)

genListFixInt :: QC.Gen (Fix (ListF Int))
genListFixInt = listFix <$> (QC.arbitrary :: QC.Gen [Int])

shrinkListFixInt :: Fix (ListF Int) -> [Fix (ListF Int)]
shrinkListFixInt = fmap listFix . QC.shrink . unlistFix

genRoseInt :: QC.Gen (Rose Int)
genRoseInt = QC.sized go
  where
    go :: Int -> QC.Gen (Rose Int)
    go size
      | size <= 0 = Rose <$> QC.arbitrary <*> pure []
      | otherwise =
          let nextSize = size `div` 2
           in Rose
                <$> QC.arbitrary
                <*> QC.resize nextSize (QC.listOf (go nextSize))

shrinkRoseInt :: Rose Int -> [Rose Int]
shrinkRoseInt (Rose value children) =
  fmap (`Rose` children) (QC.shrink value)
    <> fmap (Rose value) (QC.shrinkList shrinkRoseInt children)

genTreeFixInt :: QC.Gen (Fix (TreeF Int))
genTreeFixInt = treeFix <$> genRoseInt

shrinkTreeFixInt :: Fix (TreeF Int) -> [Fix (TreeF Int)]
shrinkTreeFixInt = fmap treeFix . shrinkRoseInt . untreeFix

evenLengthAlgebra :: ListF Int Bool -> Bool
evenLengthAlgebra Nil = True
evenLengthAlgebra (Cons _ rest) = not rest

evenLengthDBTA :: DBTA (ListF Int) Bool
evenLengthDBTA = DBTA evenLengthAlgebra

sumListAlgebra :: ListF Int Int -> Int
sumListAlgebra Nil = 0
sumListAlgebra (Cons value rest) = value + rest

sumListDBTA :: DBTA (ListF Int) Int
sumListDBTA = DBTA sumListAlgebra

dependentParityWeightedSumAlgebra :: ListF Int (Bool, Int) -> Int
dependentParityWeightedSumAlgebra Nil = 0
dependentParityWeightedSumAlgebra (Cons value (isEvenSuffix, suffixResult)) =
  (if isEvenSuffix then value else negate value) + suffixResult

evenLengthAccepting :: AcceptingDBTA (ListF Int) Bool
evenLengthAccepting =
  AcceptingDBTA
    { adbtaAlgebra = evenLengthDBTA,
      adbtaAcceptance = Acceptance id
    }

positiveSumAccepting :: AcceptingDBTA (ListF Int) Int
positiveSumAccepting =
  AcceptingDBTA
    { adbtaAlgebra = sumListDBTA,
      adbtaAcceptance = Acceptance (> 0)
    }

depthTopDown :: TopDownTA (TreeF Int) Natural
depthTopDown = TopDownTA assignDepth
  where
    assignDepth :: Natural -> TreeF Int child -> TreeF Int (Natural, child)
    assignDepth depth (NodeF value children) =
      NodeF value (fmap (depth + 1,) children)

depthAttribute :: Natural -> TreeF Int Natural -> Natural
depthAttribute depth (NodeF _ childDepths) = foldr max depth childDepths

bottomUpListRelabelTransducer :: (Int -> Int) -> BottomUpTransducer (ListF Int) () (ListF Int)
bottomUpListRelabelTransducer relabel = BottomUpTransducer step
  where
    step :: ListF Int ((), hole) -> ((), TreeContext (ListF Int) hole)
    step Nil = ((), contextLayer Nil)
    step (Cons value (_, hole)) =
      ((), contextLayer (Cons (relabel value) (contextHole hole)))

incrementListTransducer :: BottomUpTransducer (ListF Int) () (ListF Int)
incrementListTransducer =
  bottomUpListRelabelTransducer (+ 1)

identityListBottomUp :: BottomUpTransducer (ListF Int) () (ListF Int)
identityListBottomUp =
  bottomUpListRelabelTransducer id

bottomUpTreeRelabelTransducer :: (Int -> Int) -> BottomUpTransducer (TreeF Int) () (TreeF Int)
bottomUpTreeRelabelTransducer relabel = BottomUpTransducer step
  where
    step :: TreeF Int ((), hole) -> ((), TreeContext (TreeF Int) hole)
    step (NodeF value children) =
      ((), contextLayer (NodeF (relabel value) (fmap (contextHole . snd) children)))

identityTreeBottomUp :: BottomUpTransducer (TreeF Int) () (TreeF Int)
identityTreeBottomUp =
  bottomUpTreeRelabelTransducer id

incrementTreeTransducer :: BottomUpTransducer (TreeF Int) () (TreeF Int)
incrementTreeTransducer =
  bottomUpTreeRelabelTransducer (+ 1)

topDownListRelabelTransducer :: (Int -> Int) -> TopDownTransducer (ListF Int) () (ListF Int)
topDownListRelabelTransducer relabel = TopDownTransducer step
  where
    step :: () -> ListF Int child -> TreeContext (ListF Int) ((), child)
    step () Nil = contextLayer Nil
    step () (Cons value child) =
      contextLayer (Cons (relabel value) (contextHole ((), child)))

identityListTopDown :: TopDownTransducer (ListF Int) () (ListF Int)
identityListTopDown =
  topDownListRelabelTransducer id

incrementListTopDown :: TopDownTransducer (ListF Int) () (ListF Int)
incrementListTopDown =
  topDownListRelabelTransducer (+ 1)

takeNonNegativePrefixTopDown :: TopDownTransducer (ListF Int) () (ListF Int)
takeNonNegativePrefixTopDown = TopDownTransducer step
  where
    step :: () -> ListF Int child -> TreeContext (ListF Int) ((), child)
    step () Nil = contextLayer Nil
    step () (Cons value child)
      | value < 0 = contextLayer Nil
      | otherwise = contextLayer (Cons value (contextHole ((), child)))

depthRelabelTopDown :: TopDownTransducer (TreeF Int) Natural (TreeF Natural)
depthRelabelTopDown = TopDownTransducer step
  where
    step :: Natural -> TreeF Int child -> TreeContext (TreeF Natural) (Natural, child)
    step depth (NodeF _ children) =
      contextLayer (NodeF depth (fmap (\child -> contextHole (depth + 1, child)) children))

identityListMacro :: MacroTreeTransducer (ListF Int) Identity (ListF Int)
identityListMacro = MacroTreeTransducer step
  where
    step :: Identity hole -> ListF Int (Identity (TreeContext (ListF Int) hole) -> hole) -> TreeContext (ListF Int) hole
    step _ Nil = contextLayer Nil
    step (Identity hole) (Cons value child) =
      contextLayer (Cons value (contextHole (child (Identity (contextHole hole)))))

duplicateChildrenMacro :: MacroTreeTransducer (TreeF Int) Identity (TreeF Int)
duplicateChildrenMacro = MacroTreeTransducer step
  where
    step :: Identity hole -> TreeF Int (Identity (TreeContext (TreeF Int) hole) -> hole) -> TreeContext (TreeF Int) hole
    step (Identity hole) (NodeF value children) =
      let runChild child = contextHole (child (Identity (contextHole hole)))
       in contextLayer (NodeF value (fmap runChild children <> fmap runChild children))

subtreeSizeDBTA :: DBTA (TreeF Int) Natural
subtreeSizeDBTA = DBTA measure
  where
    measure :: TreeF Int Natural -> Natural
    measure (NodeF _ children) =
      1 + sum children

lookaheadLabelMacro :: LookaheadMacroTreeTransducer (TreeF Int) Natural Identity (TreeF Natural)
lookaheadLabelMacro = LookaheadMacroTreeTransducer step
  where
    step :: Identity hole -> Natural -> TreeF Int (Identity (TreeContext (TreeF Natural) hole) -> hole, Natural) -> TreeContext (TreeF Natural) hole
    step (Identity hole) lookahead (NodeF _ children) =
      let runChild (child, _) = contextHole (child (Identity (contextHole hole)))
       in contextLayer (NodeF lookahead (fmap runChild children))

treeDepth :: Fix (TreeF Int) -> Natural
treeDepth = cata measure
  where
    measure :: TreeF Int Natural -> Natural
    measure (NodeF _ children) = 1 + foldr max 0 children

topDownDepthProperty :: Fix (TreeF Int) -> Bool
topDownDepthProperty value =
  treeDepth value
    == 1 + inheritedAttribute depthTopDown depthAttribute 0 value

topDownFoldDepthProperty :: Fix (TreeF Int) -> Bool
topDownFoldDepthProperty value =
  treeDepth value
    == 1 + topDownFold depthTopDown depthAttribute 0 value

topDownAnnotatedDepthProperty :: Fix (TreeF Int) -> Bool
topDownAnnotatedDepthProperty value =
  case annotateTopDownWithAttribute depthTopDown depthAttribute 0 value of
    (_, depth) :< _ -> treeDepth value == 1 + depth

topDownStateProjectionProperty :: Fix (TreeF Int) -> Bool
topDownStateProjectionProperty value =
  case annotateTopDown depthTopDown 0 value of
    depth :< _ -> depth == 0

topDownAttributeProjectionProperty :: Fix (TreeF Int) -> Bool
topDownAttributeProjectionProperty value =
  case projectAttributeAnnotation (annotateTopDownWithAttribute depthTopDown depthAttribute 0 value) of
    depth :< _ -> treeDepth value == 1 + depth

topDownRootAttributeProperty :: Fix (TreeF Int) -> Bool
topDownRootAttributeProperty value =
  rootAttribute (annotateTopDownWithAttribute depthTopDown depthAttribute 0 value)
    == inheritedAttribute depthTopDown depthAttribute 0 value

collectListContext :: TreeContext (ListF Int) Int -> [Int]
collectListContext =
  foldTreeContext (: []) collectLayer
  where
    collectLayer :: ListF Int [Int] -> [Int]
    collectLayer Nil = []
    collectLayer (Cons value rest) = value : rest

observeListFix :: Fix (ListF Int) -> [Int]
observeListFix =
  unlistFix

observeTreeFix :: Fix (TreeF Int) -> Rose Int
observeTreeFix =
  untreeFix

bottomUpIdentityLaw ::
  (Functor f, Eq observation) =>
  (Fix f -> observation) ->
  BottomUpTransducer f state f ->
  Fix f ->
  Bool
bottomUpIdentityLaw observe transducer value =
  observe (runBottomUpTransducer transducer value)
    == observe value

bottomUpSelfCompositionLaw ::
  forall f observation state.
  (Functor f, Eq observation) =>
  (Fix f -> observation) ->
  BottomUpTransducer f state f ->
  Fix f ->
  Bool
bottomUpSelfCompositionLaw observe transducer value =
  let composed :: Fix f
      composed =
        runBottomUpTransducer (composeBottomUp transducer transducer) value
      once :: Fix f
      once =
        runBottomUpTransducer transducer value
      twice :: Fix f
      twice =
        runBottomUpTransducer transducer once
   in observe composed == observe twice

topDownIdentityLaw ::
  (Functor f, Eq observation) =>
  (Fix f -> observation) ->
  TopDownTransducer f state f ->
  state ->
  Fix f ->
  Bool
topDownIdentityLaw observe transducer initialState value =
  observe (runTopDownTransducer transducer initialState value)
    == observe value

topDownSelfCompositionLaw ::
  forall f observation state.
  (Functor f, Eq observation) =>
  (Fix f -> observation) ->
  TopDownTransducer f state f ->
  state ->
  Fix f ->
  Bool
topDownSelfCompositionLaw observe transducer initialState value =
  let composed :: Fix f
      composed =
        runTopDownTransducer
          (composeTopDown transducer transducer)
          (initialState, initialState)
          value
      once :: Fix f
      once =
        runTopDownTransducer transducer initialState value
      twice :: Fix f
      twice =
        runTopDownTransducer transducer initialState once
   in observe composed == observe twice

treeContextHoleFoldProperty :: Int -> Bool
treeContextHoleFoldProperty value =
  collectListContext (contextHole value) == [value]

treeContextLayerFoldProperty :: Int -> Int -> Bool
treeContextLayerFoldProperty value hole =
  collectListContext (contextLayer (Cons value (contextHole hole)))
    == [value, hole]

treeContextSubstituteAssociativeProperty :: Int -> Bool
treeContextSubstituteAssociativeProperty value =
  collectListContext
    ( substituteTreeContext secondSubstitution
        (substituteTreeContext firstSubstitution context)
    )
    == collectListContext
      ( substituteTreeContext
          (substituteTreeContext secondSubstitution . firstSubstitution)
          context
      )
  where
    context :: TreeContext (ListF Int) Int
    context =
      contextLayer (Cons value (contextHole (value + 1)))

    firstSubstitution :: Int -> TreeContext (ListF Int) Int
    firstSubstitution hole =
      contextLayer (Cons (hole * 2) (contextHole hole))

    secondSubstitution :: Int -> TreeContext (ListF Int) Int
    secondSubstitution hole =
      contextLayer (Cons (hole + 3) (contextHole hole))

bottomUpTransducerIncrementProperty :: Fix (ListF Int) -> Bool
bottomUpTransducerIncrementProperty value =
  unlistFix (runBottomUpTransducer incrementListTransducer value)
    == fmap (+ 1) (unlistFix value)

bottomUpTransducerComposeProperty :: Fix (ListF Int) -> Bool
bottomUpTransducerComposeProperty =
  bottomUpSelfCompositionLaw observeListFix incrementListTransducer

bottomUpTransducerIdentityListProperty :: Fix (ListF Int) -> Bool
bottomUpTransducerIdentityListProperty =
  bottomUpIdentityLaw observeListFix identityListBottomUp

bottomUpTransducerIdentityTreeProperty :: Fix (TreeF Int) -> Bool
bottomUpTransducerIdentityTreeProperty =
  bottomUpIdentityLaw observeTreeFix identityTreeBottomUp

bottomUpTransducerComposeTreeProperty :: Fix (TreeF Int) -> Bool
bottomUpTransducerComposeTreeProperty =
  bottomUpSelfCompositionLaw observeTreeFix incrementTreeTransducer

topDownTransducerIdentityListProperty :: Fix (ListF Int) -> Bool
topDownTransducerIdentityListProperty =
  topDownIdentityLaw observeListFix identityListTopDown ()

topDownTransducerPruneProperty :: Fix (ListF Int) -> Bool
topDownTransducerPruneProperty value =
  unlistFix (runTopDownTransducer takeNonNegativePrefixTopDown () value)
    == takeWhile (>= 0) (unlistFix value)

topDownTransducerComposeProperty :: Fix (ListF Int) -> Bool
topDownTransducerComposeProperty =
  topDownSelfCompositionLaw observeListFix incrementListTopDown ()

topDownTransducerDepthRelabelProperty :: Fix (TreeF Int) -> Bool
topDownTransducerDepthRelabelProperty value =
  depthLabelsAre 0 (runTopDownTransducer depthRelabelTopDown 0 value)

depthLabelsAre :: Natural -> Fix (TreeF Natural) -> Bool
depthLabelsAre expectedDepth (Fix (NodeF actualDepth children)) =
  actualDepth == expectedDepth
    && all (depthLabelsAre (expectedDepth + 1)) children

macroTreeTransducerIdentityProperty :: Fix (ListF Int) -> Bool
macroTreeTransducerIdentityProperty value =
  unlistFix (runMacroTreeTransducer identityListMacro (Identity (Fix Nil)) value)
    == unlistFix value

macroTreeTransducerDuplicateChildrenProperty :: Bool
macroTreeTransducerDuplicateChildrenProperty =
  untreeFix
    ( runMacroTreeTransducer
        duplicateChildrenMacro
        (Identity (Fix (NodeF 0 [])))
        duplicateFixture
    )
    == Rose 1 [Rose 2 [], Rose 3 [], Rose 2 [], Rose 3 []]
  where
    duplicateFixture :: Fix (TreeF Int)
    duplicateFixture =
      treeFix (Rose 1 [Rose 2 [], Rose 3 []])

subtreeSizeLabelTree :: Fix (TreeF Int) -> Fix (TreeF Natural)
subtreeSizeLabelTree =
  snd . cata labelLayer
  where
    labelLayer :: TreeF Int (Natural, Fix (TreeF Natural)) -> (Natural, Fix (TreeF Natural))
    labelLayer (NodeF _ children) =
      let size = 1 + foldr ((+) . fst) 0 children
       in (size, Fix (NodeF size (fmap snd children)))

lookaheadMacroTreeTransducerLabelsProperty :: Fix (TreeF Int) -> Bool
lookaheadMacroTreeTransducerLabelsProperty value =
  untreeFix
    ( runLookaheadMacroTreeTransducer
        subtreeSizeDBTA
        lookaheadLabelMacro
        (Identity (Fix (NodeF 0 [])))
        value
    )
    == untreeFix (subtreeSizeLabelTree value)

bottomUpAnnotationRootProperty :: Fix (TreeF Int) -> Bool
bottomUpAnnotationRootProperty value =
  case annotateBottomUp subtreeSizeDBTA value of
    size :< _ -> size == treeNodeCount value

treeNodeCount :: Fix (TreeF Int) -> Natural
treeNodeCount =
  cata countLayer
  where
    countLayer :: TreeF Int Natural -> Natural
    countLayer (NodeF _ children) =
      1 + sum children

treeLanguageLeft :: Language (Fix (ListF Int))
treeLanguageLeft = treeLanguageFromAcceptingDBTA evenLengthAccepting

treeLanguageRight :: Language (Fix (ListF Int))
treeLanguageRight = treeLanguageFromAcceptingDBTA positiveSumAccepting

languageJoinCommutativeLaw :: Language carrier -> Language carrier -> carrier -> Bool
languageJoinCommutativeLaw left right input =
  runLanguage (join left right) input
    == runLanguage (join right left) input

languageMeetCommutativeLaw :: Language carrier -> Language carrier -> carrier -> Bool
languageMeetCommutativeLaw left right input =
  runLanguage (meet left right) input
    == runLanguage (meet right left) input

languageAbsorptionJoinLaw :: Language carrier -> Language carrier -> carrier -> Bool
languageAbsorptionJoinLaw left right input =
  runLanguage (join left (meet left right)) input
    == runLanguage left input

languageAbsorptionMeetLaw :: Language carrier -> Language carrier -> carrier -> Bool
languageAbsorptionMeetLaw left right input =
  runLanguage (meet left (join left right)) input
    == runLanguage left input

languageComplementJoinLaw :: Language carrier -> carrier -> Bool
languageComplementJoinLaw language input =
  runLanguage (join language (complement language)) input
    == runLanguage (Language (const True)) input

languageComplementMeetLaw :: Language carrier -> carrier -> Bool
languageComplementMeetLaw language input =
  runLanguage (meet language (complement language)) input
    == runLanguage (Language (const False)) input

listProperty :: (Fix (ListF Int) -> Bool) -> QC.Property
listProperty =
  QC.forAllShrink genListFixInt shrinkListFixInt

treeProperty :: (Fix (TreeF Int) -> Bool) -> QC.Property
treeProperty =
  QC.forAllShrink genTreeFixInt shrinkTreeFixInt

automataLawBundle :: QuickCheckLawBundle String AutomataLawName
automataLawBundle =
  quickCheckLawBundle
    "automata"
    [ quickCheckLawDefinition (NamedAutomataLaw DependentDBTAIsZygo) (listProperty (dependentDBTAIsZygoLaw evenLengthDBTA dependentParityWeightedSumAlgebra)),
      quickCheckLawDefinition (NamedAutomataLaw ProductDBTA) (listProperty (productDBTALaw evenLengthDBTA sumListDBTA)),
      quickCheckLawDefinition (NamedAutomataLaw IntersectionAcceptance) (listProperty (intersectionAcceptanceLaw evenLengthAccepting positiveSumAccepting)),
      quickCheckLawDefinition (NamedAutomataLaw UnionAcceptance) (listProperty (unionAcceptanceLaw evenLengthAccepting positiveSumAccepting)),
      quickCheckLawDefinition (NamedAutomataLaw ComplementAcceptance) (complementAcceptanceLaw (Acceptance even) :: Int -> Bool),
      quickCheckLawDefinition (NamedAutomataLaw DenotationalUnionHomomorphism) (listProperty (denotationalUnionHomomorphismLaw evenLengthAccepting positiveSumAccepting)),
      quickCheckLawDefinition (NamedAutomataLaw DenotationalIntersectionHomomorphism) (listProperty (denotationalIntersectionHomomorphismLaw evenLengthAccepting positiveSumAccepting)),
      quickCheckLawDefinition (NamedAutomataLaw DenotationalComplementHomomorphism) (listProperty (denotationalComplementHomomorphismLaw evenLengthAccepting)),
      quickCheckLawDefinition (NamedAutomataLaw TopDownFold) (treeProperty (topDownFoldLaw depthTopDown depthAttribute 0)),
      quickCheckLawDefinition (NamedAutomataLaw TopDownAnnotation) (treeProperty (topDownAnnotationLaw depthTopDown 0)),
      quickCheckLawDefinition (NamedAutomataLaw TopDownAnnotatedAttribute) (treeProperty (topDownAnnotatedAttributeLaw depthTopDown depthAttribute 0)),
      quickCheckLawDefinition (NamedAutomataLaw TopDownStateProjection) (treeProperty (topDownStateProjectionLaw depthTopDown depthAttribute 0)),
      quickCheckLawDefinition (NamedAutomataLaw TopDownAttributeProjection) (treeProperty (topDownAttributeProjectionLaw depthTopDown depthAttribute 0)),
      quickCheckLawDefinition (NamedAutomataLaw RootAttributeProjection) (treeProperty (rootAttributeProjectionLaw depthTopDown depthAttribute 0)),
      quickCheckLawDefinition TopDownDepthAttribute (treeProperty topDownDepthProperty),
      quickCheckLawDefinition TopDownFoldDepthAttribute (treeProperty topDownFoldDepthProperty),
      quickCheckLawDefinition TopDownAnnotatedDepthAttribute (treeProperty topDownAnnotatedDepthProperty),
      quickCheckLawDefinition TopDownStateProjectionRoot (treeProperty topDownStateProjectionProperty),
      quickCheckLawDefinition TopDownAttributeProjectionRoot (treeProperty topDownAttributeProjectionProperty),
      quickCheckLawDefinition TopDownRootAttribute (treeProperty topDownRootAttributeProperty),
      quickCheckLawDefinition TreeContextHoleFold treeContextHoleFoldProperty,
      quickCheckLawDefinition TreeContextLayerFold treeContextLayerFoldProperty,
      quickCheckLawDefinition TreeContextSubstituteAssociative treeContextSubstituteAssociativeProperty,
      quickCheckLawDefinition BottomUpTransducerIdentityList (listProperty bottomUpTransducerIdentityListProperty),
      quickCheckLawDefinition BottomUpTransducerIdentityTree (treeProperty bottomUpTransducerIdentityTreeProperty),
      quickCheckLawDefinition BottomUpTransducerComposeTree (treeProperty bottomUpTransducerComposeTreeProperty),
      quickCheckLawDefinition TopDownTransducerIdentityList (listProperty topDownTransducerIdentityListProperty),
      quickCheckLawDefinition TopDownTransducerDepthRelabel (treeProperty topDownTransducerDepthRelabelProperty),
      quickCheckLawDefinition TopDownTransducerPrune (listProperty topDownTransducerPruneProperty),
      quickCheckLawDefinition TopDownTransducerCompose (listProperty topDownTransducerComposeProperty),
      quickCheckLawDefinition MacroTreeTransducerIdentity (listProperty macroTreeTransducerIdentityProperty),
      quickCheckLawDefinition MacroTreeTransducerDuplicateChildren macroTreeTransducerDuplicateChildrenProperty,
      quickCheckLawDefinition LookaheadMacroTreeTransducerLabels (treeProperty lookaheadMacroTreeTransducerLabelsProperty),
      quickCheckLawDefinition BottomUpAnnotationRoot (treeProperty bottomUpAnnotationRootProperty),
      quickCheckLawDefinition BottomUpTransducerIncrement (listProperty bottomUpTransducerIncrementProperty),
      quickCheckLawDefinition BottomUpTransducerCompose (listProperty bottomUpTransducerComposeProperty)
    ]

languageLawBundle :: QuickCheckLawBundle String AutomataLawName
languageLawBundle =
  quickCheckLawBundle
    "language-laws"
    [ quickCheckLawDefinition TreeLanguageJoinCommutative (listProperty (languageJoinCommutativeLaw treeLanguageLeft treeLanguageRight)),
      quickCheckLawDefinition TreeLanguageMeetCommutative (listProperty (languageMeetCommutativeLaw treeLanguageLeft treeLanguageRight)),
      quickCheckLawDefinition TreeLanguageAbsorptionJoin (listProperty (languageAbsorptionJoinLaw treeLanguageLeft treeLanguageRight)),
      quickCheckLawDefinition TreeLanguageAbsorptionMeet (listProperty (languageAbsorptionMeetLaw treeLanguageLeft treeLanguageRight)),
      quickCheckLawDefinition TreeLanguageComplementJoin (listProperty (languageComplementJoinLaw treeLanguageLeft)),
      quickCheckLawDefinition TreeLanguageComplementMeet (listProperty (languageComplementMeetLaw treeLanguageLeft))
    ]

tests :: TestTree
tests =
  localOption
    (QC.QuickCheckTests 100)
    ( lawSuiteGroup
        "automata"
        [ quickCheckLawBundleGroup "bundles" id [automataLawBundle, languageLawBundle]
        ]
    )
