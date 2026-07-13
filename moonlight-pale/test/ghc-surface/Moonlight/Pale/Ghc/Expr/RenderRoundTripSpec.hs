{-# LANGUAGE LambdaCase #-}

module Moonlight.Pale.Ghc.Expr.RenderRoundTripSpec
  ( tests,
  )
where

import Data.List (isInfixOf)
import GHC.Types.Name.Occurrence (mkVarOcc, occNameString)
import GHC.Types.Name.Reader (mkRdrUnqual, rdrNameOcc)
import Moonlight.Core (BinderId (..), Pattern (..))
import Moonlight.Core qualified as EGraph
import Moonlight.Pale.Ghc.Expr
import Moonlight.Pale.Test.Site.Assertion (expectRightWithLabel)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "pale.expr"
    [ renderRoundTripTests,
      spanLockstepTests
    ]

renderRoundTripTests :: TestTree
renderRoundTripTests =
  testGroup
    "render.roundtrip"
    [ roundTripCase
        "lambda and application round-trip"
        [ "module Fixture where",
          "",
          "handle = \\evt -> process evt evt"
        ],
      roundTripCase
        "multi-argument function becomes a lambda chain"
        [ "module Fixture where",
          "",
          "apply2 f x = f x x"
        ],
      roundTripCase
        "plain where round-trips with layout"
        [ "module Fixture where",
          "",
          "scale x = base * x",
          "  where",
          "    base = 10"
        ],
      roundTripCase
        "multi-bind where round-trips"
        [ "module Fixture where",
          "",
          "f x = combine y z",
          "  where",
          "    y = deriveY x",
          "    z = deriveZ x"
        ],
      roundTripCase
        "where guarded local function round-trips"
        [ "module Fixture where",
          "",
          "clamp q = saturate q",
          "  where",
          "    saturate value",
          "      | value > upper = upper",
          "      | value < lower = lower",
          "      | otherwise = value"
        ],
      roundTripCase
        "where tuple pattern bind round-trips"
        [ "module Fixture where",
          "",
          "f x = combine a b where (a, b) = splitPair x"
        ],
      roundTripCase
        "let constructor pattern bind round-trips"
        [ "module Fixture where",
          "",
          "g m = let Just y = m in use y"
        ],
      roundTripCase
        "mixed var and pattern where binds round-trip"
        [ "module Fixture where",
          "",
          "mix x = combine seed a b",
          "  where",
          "    seed = deriveSeed x",
          "    (a, b) = splitPair x"
        ],
      roundTripCase
        "do-let tuple pattern bind round-trips"
        [ "module Fixture where",
          "",
          "run pair = do { let { (a, b) = pair }; pure (combine a b) }"
        ],
      roundTripCase
        "lazy where pattern bind round-trips"
        [ "module Fixture where",
          "",
          "lazyBind pair = combine a b",
          "  where",
          "    ~(a, b) = pair"
        ],
      roundTripCase
        "case with tuple and wildcard branches"
        [ "module Fixture where",
          "",
          "swap p = case p of { (a, b) -> (b, a); _ -> p }"
        ],
      roundTripCase
        "do block with bind, let, and body statements"
        [ "module Fixture where",
          "",
          "run action = do { x <- action; let { y = combine x x }; pure y }"
        ],
      testCase "generated top-level binding renders do and let as layout while compact rendering stays compact" $ do
        convertedModule <-
          expectRightWithLabel
            "fixture conversion"
            ( convertHaskellSource
                "Fixture.hs"
                ( unlines
                    [ "module Fixture where",
                      "",
                      "run action = do { x <- action; let { y = combine x x }; pure y }"
                    ]
                )
            )
        bindingValue <- singleBinding convertedModule
        compactRendered <- expectRightWithLabel "compact render" (renderTopLevelBinding "run" (tlbTerm bindingValue))
        assertBool
          ("compact top-level renderer changed its round-trip surface:\n" <> compactRendered)
          ("do {" `isInfixOf` compactRendered && "let {" `isInfixOf` compactRendered)
        generatedRendered <- expectRightWithLabel "generated render" (renderGeneratedTopLevelBinding "run" (tlbTerm bindingValue))
        assertBool
          ("generated renderer still emitted compact do syntax:\n" <> generatedRendered)
          (not ("do {" `isInfixOf` generatedRendered))
        assertBool
          ("generated renderer still emitted compact let syntax:\n" <> generatedRendered)
          (not ("let {" `isInfixOf` generatedRendered))
        assertBool
          ("generated renderer did not emit layout do syntax:\n" <> generatedRendered)
          ("\n  do" `isInfixOf` generatedRendered || "= do" `isInfixOf` generatedRendered)
        reparsedModule <-
          expectRightWithLabel
            "re-parse of generated rendering"
            (convertHaskellSource "Generated.hs" ("module Generated where\n\n" <> generatedRendered <> "\n"))
        reparsedBinding <- singleBinding reparsedModule
        assertRoundTripBinding generatedRendered (bindingValue, reparsedBinding),
      testCase "readable rendering keeps lambda-case out of brace layout" $ do
        convertedModule <-
          expectRightWithLabel
            "fixture conversion"
            ( convertHaskellSource
                "Fixture.hs"
                ( unlines
                    [ "module Fixture where",
                      "",
                      "choose input = consume (\\case { Just x -> pure x; Nothing -> empty }) input"
                    ]
                )
            )
        bindingValue <- singleBinding convertedModule
        compactRendered <- expectRightWithLabel "compact render" (renderTopLevelBinding "choose" (tlbTerm bindingValue))
        assertBool
          ("compact lambda-case renderer changed its round-trip surface:\n" <> compactRendered)
          ("\\case {" `isInfixOf` compactRendered)
        readableRendered <- expectRightWithLabel "readable render" (renderReadableTopLevelBinding "choose" (tlbTerm bindingValue))
        assertBool
          ("readable renderer still emitted compact lambda-case syntax:\n" <> readableRendered)
          (not ("\\case {" `isInfixOf` readableRendered))
        assertBool
          ("readable renderer did not emit layout lambda-case syntax:\n" <> readableRendered)
          ("\\case\n" `isInfixOf` readableRendered)
        reparsedModule <-
          expectRightWithLabel
            "re-parse of readable rendering"
            (convertHaskellSource "Readable.hs" ("module Readable where\n\n" <> readableRendered <> "\n"))
        reparsedBinding <- singleBinding reparsedModule
        assertRoundTripBinding readableRendered (bindingValue, reparsedBinding),
      testCase "readable rendering keeps record construction in layout form" $ do
        convertedModule <-
          expectRightWithLabel
            "fixture conversion"
            ( convertHaskellSource
                "Fixture.hs"
                ( unlines
                    [ "module Fixture where",
                      "",
                      "record = Metrics { alpha = one, beta = two }"
                    ]
                )
            )
        bindingValue <- singleBinding convertedModule
        compactRendered <- expectRightWithLabel "compact render" (renderTopLevelBinding "record" (tlbTerm bindingValue))
        assertBool
          ("compact record renderer changed its round-trip surface:\n" <> compactRendered)
          ("{ alpha = one, beta = two }" `isInfixOf` compactRendered)
        readableRendered <- expectRightWithLabel "readable render" (renderReadableTopLevelBinding "record" (tlbTerm bindingValue))
        assertBool
          ("readable renderer still emitted one-line record syntax:\n" <> readableRendered)
          (not ("{ alpha = one, beta = two }" `isInfixOf` readableRendered))
        assertBool
          ("readable renderer did not emit layout record syntax:\n" <> readableRendered)
          ("\n    { alpha = one\n    , beta = two\n    }" `isInfixOf` readableRendered)
        reparsedModule <-
          expectRightWithLabel
            "re-parse of readable record rendering"
            (convertHaskellSource "ReadableRecord.hs" ("module ReadableRecord where\n\n" <> readableRendered <> "\n"))
        reparsedBinding <- singleBinding reparsedModule
        assertRoundTripBinding readableRendered (bindingValue, reparsedBinding),
      roundTripCase
        "operator applications with symbolic and alphanumeric operators"
        [ "module Fixture where",
          "",
          "total a b c = a + b * c",
          "",
          "halve a b = a `div` b",
          "",
          "summed = foldr (+) 0"
        ],
      testCase "mixed-precedence operator chains render without parser-tree parens" $ do
        convertedModule <-
          expectRightWithLabel
            "fixture conversion"
            ( convertHaskellSource
                "Fixture.hs"
                ( unlines
                    [ "module Fixture where",
                      "",
                      "bounded value = value >= 0 && value <= 32"
                    ]
                )
            )
        bindingValue <- singleBinding convertedModule
        bindingPair <- namedBinding bindingValue
        renderedSource <- expectRightWithLabel "render" (renderModuleSource "" (Just "Fixture") [bindingPair])
        assertBool
          ("mixed fixity chain was rendered through the parser tree:\n" <> renderedSource)
          (not ("((value >= 0) && value) <= 32" `isInfixOf` renderedSource))
        assertBool
          ("mixed fixity chain lost its surface order:\n" <> renderedSource)
          ("value >= 0 && value <= 32" `isInfixOf` renderedSource),
      testCase "case operand in operator application renders multiline" $ do
        convertedModule <-
          expectRightWithLabel
            "fixture conversion"
            ( convertHaskellSource
                "Fixture.hs"
                ( unlines
                    [ "module Fixture where",
                      "",
                      "globalReferenceNames nodeValue = (case nodeValue of { Just value -> pure value; Nothing -> mempty }) <> foldMap globalReferenceNames nodeValue"
                    ]
                )
            )
        bindingValue <- singleBinding convertedModule
        bindingPair <- namedBinding bindingValue
        renderedSource <- expectRightWithLabel "render" (renderModuleSource "" (Just "Fixture") [bindingPair])
        assertBool
          ("case expression should not be crushed into brace layout:\n" <> renderedSource)
          (not ("case nodeValue of {" `isInfixOf` renderedSource))
        assertBool
          ("case operand did not render as a readable multiline operator operand:\n" <> renderedSource)
          ( "globalReferenceNames nodeValue =\n  (\n    case nodeValue of\n      Just value -> pure value\n      Nothing -> mempty\n  ) <> foldMap globalReferenceNames nodeValue"
              `isInfixOf` renderedSource
          )
        reparsedModule <- expectRightWithLabel "re-parse of rendered source" (convertHaskellSource "Fixture.hs" renderedSource)
        reparsedBinding <- singleBinding reparsedModule
        assertRoundTripBinding renderedSource (bindingValue, reparsedBinding),
      roundTripCase
        "left and right sections"
        [ "module Fixture where",
          "",
          "increment = (1 +)",
          "",
          "halved = (`div` 2)"
        ],
      roundTripCase
        "if-then-else"
        [ "module Fixture where",
          "",
          "choose c = if c then trueBranch else falseBranch"
        ],
      roundTripCase
        "multi-way if with boolean guards round-trips"
        [ "module Fixture where",
          "",
          "choose x = if | isSmall x -> small",
          "              | isBig x -> big",
          "              | otherwise -> unknown"
        ],
      roundTripCase
        "multi-way if with pattern guard round-trips"
        [ "module Fixture where",
          "",
          "pick source = if | Just y <- lookupThing source -> y",
          "                 | otherwise -> fallback"
        ],
      roundTripCase
        "expression type signature in argument position round-trips"
        [ "module Fixture where",
          "",
          "typedArg x = apply (x :: Int)"
        ],
      roundTripCase
        "type applications round-trip"
        [ "module Fixture where",
          "",
          "typedApps f = pair (f @Int) (f @(Maybe a))"
        ],
      roundTripCase
        "lists and tuples"
        [ "module Fixture where",
          "",
          "trio = [1, 2, 3]",
          "",
          "pair = (1, \"two\")"
        ],
      roundTripCase
        "record construction"
        [ "module Fixture where",
          "",
          "settings = MkSettings { width = 3, label = \"wide\" }"
        ],
      roundTripCase
        "record update"
        [ "module Fixture where",
          "",
          "widen settings = settings { width = 4, label = \"wider\" }"
        ],
      roundTripCase
        "record patterns round-trip"
        [ "module Fixture where",
          "",
          "recordPat value = case value of { MkRec {left = Just a, right = (b, c)} -> combine a b c; EmptyRec {} -> empty; _ -> fallback }"
        ],
      roundTripCase
        "record pun pattern round-trips"
        [ "module Fixture where",
          "",
          "recordPun value = case value of { MkRec {field} -> field; _ -> fallback }"
        ],
      roundTripCase
        "arithmetic sequences"
        [ "module Fixture where",
          "",
          "open = [0 ..]",
          "",
          "steppedOpen = [0, 2 ..]",
          "",
          "closed = [0 .. 10]",
          "",
          "steppedClosed = [0, 2 .. 10]"
        ],
      roundTripCase
        "negation"
        [ "module Fixture where",
          "",
          "invert x = -x"
        ],
      roundTripCase
        "character, string, and numeric literals"
        [ "module Fixture where",
          "",
          "letter = 'c'",
          "",
          "greeting = \"hello\"",
          "",
          "answer = 42",
          "",
          "ratio = 2.5"
        ],
      roundTripCase
        "symbolic top-level definition"
        [ "module Fixture where",
          "",
          "(<+>) = \\x -> x"
        ],
      roundTripCase
        "nested let and shadow-style reuse"
        [ "module Fixture where",
          "",
          "shadow = let g = \\x -> use x x in g alpha"
        ],
      roundTripCase
        "constructor-pattern case alternatives round-trip"
        [ "module Fixture where",
          "",
          "unwrap m = case m of { Just x -> x; Nothing -> fallback }"
        ],
      roundTripCase
        "nested constructor patterns round-trip"
        [ "module Fixture where",
          "",
          "nested x = case x of { Just (Left y) -> y; _ -> other }"
        ],
      roundTripCase
        "as-patterns round-trip"
        [ "module Fixture where",
          "",
          "asPat x = case x of { all@(Just y) -> use all y; Nothing -> base }"
        ],
      roundTripCase
        "list patterns round-trip"
        [ "module Fixture where",
          "",
          "listPat xs = case xs of { [a, b] -> combine a b; _ -> empty }"
        ],
      roundTripCase
        "tuple-inside-constructor patterns round-trip"
        [ "module Fixture where",
          "",
          "tupCon x = case x of { Just (a, b) -> pair a b; Nothing -> base }"
        ],
      roundTripCase
        "integer literal alternatives round-trip"
        [ "module Fixture where",
          "",
          "classify n = case n of { 0 -> zero; _ -> other }"
        ],
      roundTripCase
        "character and string literal alternatives round-trip"
        [ "module Fixture where",
          "",
          "tag c = case c of { 'a' -> alpha; 'b' -> beta; _ -> other }",
          "",
          "named s = case s of { \"yes\" -> true; _ -> false }"
        ],
      roundTripCase
        "infix constructor patterns round-trip"
        [ "module Fixture where",
          "",
          "headTail xs = case xs of { (h : t) -> use h t; [] -> base }"
        ],
      roundTripCase
        "bang patterns in case alternatives round-trip"
        [ "module Fixture where",
          "",
          "strict x = case x of { !y -> use y }"
        ],
      roundTripCase
        "wildcard alternatives round-trip"
        [ "module Fixture where",
          "",
          "ignore x = case x of { _ -> constant }"
        ],
      roundTripCase
        "do-bind with constructor pattern round-trips"
        [ "module Fixture where",
          "",
          "run action = do { Just v <- action; pure v }"
        ],
      roundTripCase
        "adversarial tuple-of-constructor-and-list pattern round-trips"
        [ "module Fixture where",
          "",
          "adversarial x = case x of { (Just a, [b, c]) -> combine a b c; _ -> base }"
        ],
      roundTripCase
        "guarded otherwise chain round-trips"
        [ "module Fixture where",
          "",
          "choose x",
          "  | isPrimary x = primary",
          "  | otherwise = secondary"
        ],
      roundTripCase
        "multi-alternative boolean guard chain round-trips"
        [ "module Fixture where",
          "",
          "traffic signal",
          "  | isRed signal = stop",
          "  | isYellow signal = caution",
          "  | isGreen signal = go",
          "  | otherwise = unknown"
        ],
      roundTripCase
        "pattern guard round-trips"
        [ "module Fixture where",
          "",
          "lookupValue x",
          "  | Just y <- lookupThing x = y",
          "  | otherwise = fallback"
        ],
      roundTripCase
        "let guard round-trips"
        [ "module Fixture where",
          "",
          "letGuard x",
          "  | let { y = normalize x } = y"
        ],
      roundTripCase
        "guarded case alternative round-trips"
        [ "module Fixture where",
          "",
          "select m = case m of { Just x | valid x -> x; _ -> fallback }"
        ],
      roundTripCase
        "case alternative where round-trips"
        [ "module Fixture where",
          "",
          "select m = case m of { Just x -> use x y where { y = derive x }; Nothing -> fallback }"
        ],
      roundTripCase
        "guarded binding with multiple arguments round-trips"
        [ "module Fixture where",
          "",
          "combineGuard a b",
          "  | ok a b = pair a b",
          "  | otherwise = fallback a b"
        ],
      roundTripCase
        "guarded where round-trips"
        [ "module Fixture where",
          "",
          "f x",
          "  | isBig x = large y",
          "  | otherwise = small y",
          "  where",
          "    y = derive x"
        ],
      roundTripCase
        "clauses multi-clause recursion round-trips"
        [ "module Fixture where",
          "",
          "factorial 0 = 1",
          "factorial n = times n (factorial (minus n one))"
        ],
      roundTripCase
        "clauses multi-clause constructor patterns round-trip"
        [ "module Fixture where",
          "",
          "unwrap (Just x) = x",
          "unwrap Nothing = fallback"
        ],
      roundTripCase
        "clauses pattern lambda in expression position round-trips"
        [ "module Fixture where",
          "",
          "mapper = apply (\\(Just x) -> use x)"
        ],
      roundTripCase
        "clauses lambda-case multi-alternative round-trips"
        [ "module Fixture where",
          "",
          "handler = \\case { Just x -> use x; Nothing -> fallback }"
        ],
      roundTripCase
        "clauses lambda-cases two-pattern round-trips"
        [ "module Fixture where",
          "",
          "combiner = \\cases { (Just x) (Just y) -> pair x y; _ _ -> fallback }"
        ],
      roundTripCase
        "clauses guarded multi-clause binding round-trips"
        [ "module Fixture where",
          "",
          "classify x",
          "  | isBig x = large",
          "classify y = small y"
        ],
      roundTripCase
        "clause where under multi-clause definition round-trips"
        [ "module Fixture where",
          "",
          "choose 0 = zero",
          "choose n = combine n y",
          "  where",
          "    y = derive n"
        ],
      testCase "clauses multi-clause definition converts without opaque lambda match group" $ do
        convertedModule <-
          expectRightWithLabel
            "fixture conversion"
            ( convertHaskellSource
                "Fixture.hs"
                ( unlines
                    [ "module Fixture where",
                      "",
                      "factorial 0 = 1",
                      "factorial n = times n (factorial (minus n one))"
                    ]
                )
            )
        bindingValue <- singleBinding convertedModule
        assertBool
          "multi-clause definition must not contain OpaqueF"
          (not (patternContainsOpaque (tlbTerm bindingValue)))
        case tlbTerm bindingValue of
          PatternNode (ClausesF clauseValues) -> do
            length clauseValues @?= 2
            case fmap fst clauseValues of
              [[POverLitP (NormalizedIntegralOverLit 0)], [PVarP _]] ->
                pure ()
              patternShapes ->
                assertFailure ("expected literal and variable clause patterns, got " <> show patternShapes)
          otherTerm ->
            assertFailure ("expected a ClausesF multi-clause binding, got " <> show otherTerm),
      testCase "clauses var-only single-clause top-level rendering refuses lam territory" $ do
        let binderAnn = BinderAnn (BinderId 0) (mkRdrUnqual (mkVarOcc "x"))
            bodyValue :: Pattern HsExprF
            bodyValue = PatternNode (VarF (LocalName binderAnn))
        renderTopLevelBinding "identity" (PatternNode (ClausesF [([PVarP binderAnn], bodyValue)]))
          @?= Left RenderClausesShape,
      testCase "lambda spine renders on lhs and reconverts" $ do
        convertedModule <-
          expectRightWithLabel
            "fixture conversion"
            ( convertHaskellSource
                "Fixture.hs"
                ( unlines
                    [ "module Fixture where",
                      "",
                      "f = \\x -> use x"
                    ]
                )
            )
        bindingValue <- singleBinding convertedModule
        bindingPair <- namedBinding bindingValue
        renderedSource <- expectRightWithLabel "render" (renderModuleSource "" (Just "Fixture") [bindingPair])
        renderedSource @?= unlines ["module Fixture where", "", "f x = use x"]
        reparsedModule <- expectRightWithLabel "re-parse of rendered source" (convertHaskellSource "Fixture.hs" renderedSource)
        reparsedBinding <- singleBinding reparsedModule
        assertRoundTripBinding renderedSource (bindingValue, reparsedBinding),
      testCase "guarded where renders multi-line layout and reconverts" $ do
        convertedModule <-
          expectRightWithLabel
            "fixture conversion"
            ( convertHaskellSource
                "Fixture.hs"
                ( unlines
                    [ "module Fixture where",
                      "",
                      "f x | isBig x = large y | otherwise = small y where y = derive x"
                    ]
                )
            )
        bindingValue <- singleBinding convertedModule
        bindingPair <- namedBinding bindingValue
        renderedSource <- expectRightWithLabel "render" (renderModuleSource "" (Just "Fixture") [bindingPair])
        renderedSource
          @?= unlines
            [ "module Fixture where",
              "",
              "f x",
              "  | isBig x = large y",
              "  | otherwise = small y",
              "  where",
              "    y = derive x"
            ]
        reparsedModule <- expectRightWithLabel "re-parse of rendered source" (convertHaskellSource "Fixture.hs" renderedSource)
        reparsedBinding <- singleBinding reparsedModule
        assertRoundTripBinding renderedSource (bindingValue, reparsedBinding),
      testCase "guarded binding converts without opaque fallback" $ do
        convertedModule <-
          expectRightWithLabel
            "fixture conversion"
            ( convertHaskellSource
                "Fixture.hs"
                ( unlines
                    [ "module Fixture where",
                      "",
                      "lookupValue x",
                      "  | Just y <- lookupThing x = y"
                    ]
                )
            )
        bindingValue <- singleBinding convertedModule
        assertBool
          "guarded binding must not contain an opaque fallback"
          (not (patternContainsOpaque (tlbTerm bindingValue)))
        case stripBindingLambdas (tlbTerm bindingValue) of
          PatternNode (GuardedF [GuardedAltF [GuardPatF (PConP _ [PVarP guardBinder]) _] (PatternNode (VarF (LocalName bodyBinder)))]) ->
            occNameString (rdrNameOcc (baName guardBinder)) @?= occNameString (rdrNameOcc (baName bodyBinder))
          otherBody ->
            assertFailure ("expected a pattern-guarded body, got " <> show otherBody),
      testCase "multi-way if pattern guard converts without opaque fallback" $ do
        convertedModule <-
          expectRightWithLabel
            "fixture conversion"
            ( convertHaskellSource
                "Fixture.hs"
                ( unlines
                    [ "module Fixture where",
                      "",
                      "pick source = if | Just y <- lookupThing source -> y",
                      "                 | otherwise -> fallback"
                    ]
                )
            )
        bindingValue <- singleBinding convertedModule
        assertBool
          "multi-way if must not contain an opaque fallback"
          (not (patternContainsOpaque (tlbTerm bindingValue)))
        case stripBindingLambdas (tlbTerm bindingValue) of
          PatternNode (MultiIfF [GuardedAltF [GuardPatF (PConP _ [PVarP guardBinder]) _] (PatternNode (VarF (LocalName bodyBinder))), GuardedAltF [GuardBoolF _] _]) ->
            occNameString (rdrNameOcc (baName guardBinder)) @?= occNameString (rdrNameOcc (baName bodyBinder))
          otherBody ->
            assertFailure ("expected a pattern-guarded multi-way if, got " <> show otherBody),
      testCase "type syntax constructors convert without opaque fallback" $ do
        convertedModule <-
          expectRightWithLabel
            "fixture conversion"
            ( convertHaskellSource
                "Fixture.hs"
                ( unlines
                    [ "module Fixture where",
                      "",
                      "typed f x = pair (apply (x :: Int)) (pair (f @Int) (f @(Maybe a)))"
                    ]
                )
            )
        bindingValue <- singleBinding convertedModule
        assertBool
          "type syntax fixture must not contain OpaqueF"
          (not (patternContainsOpaque (tlbTerm bindingValue)))
        assertBool
          "fixture must contain an expression type signature node"
          (patternContainsExprWithTySig (tlbTerm bindingValue))
        assertBool
          "fixture must contain visible type application nodes"
          (patternContainsAppType (tlbTerm bindingValue)),
      testCase "record patterns convert to field rows without lossy fallback" $ do
        convertedModule <-
          expectRightWithLabel
            "fixture conversion"
            ( convertHaskellSource
                "Fixture.hs"
                ( unlines
                    [ "module Fixture where",
                      "",
                      "recordPat value = case value of { MkRec {left = Just a, right = (b, c)} -> combine a b c; EmptyRec {} -> empty; _ -> fallback }"
                    ]
                )
            )
        bindingValue <- singleBinding convertedModule
        assertBool
          "record pattern fixture must not contain OpaqueF"
          (not (patternContainsOpaque (tlbTerm bindingValue)))
        case stripBindingLambdas (tlbTerm bindingValue) of
          PatternNode (CaseF _ branchValues) ->
            case fmap (stripTestPatParens . fst) branchValues of
              [ PRecP _ [("left", PConP _ [PVarP _]), ("right", PTupleP [PVarP _, PVarP _])],
                PRecP _ [],
                PWildP
                ] ->
                  pure ()
              alternativePatterns ->
                assertFailure ("expected faithful record pattern rows, got " <> show alternativePatterns)
          otherBody ->
            assertFailure ("expected a case expression with record patterns, got " <> show otherBody),
      testCase "record pun pattern renders to explicit field row" $ do
        convertedModule <-
          expectRightWithLabel
            "fixture conversion"
            ( convertHaskellSource
                "Fixture.hs"
                ( unlines
                    [ "module Fixture where",
                      "",
                      "recordPun value = case value of { MkRec {field} -> field; _ -> fallback }"
                    ]
                )
            )
        bindingValue <- singleBinding convertedModule
        bindingPair <- namedBinding bindingValue
        renderedSource <- expectRightWithLabel "render" (renderModuleSource "" (Just "Fixture") [bindingPair])
        assertBool
          ("record pun must render as an explicit field row:\n" <> renderedSource)
          ("field = field" `isInfixOf` renderedSource)
        reparsedModule <- expectRightWithLabel "re-parse of rendered source" (convertHaskellSource "Fixture.hs" renderedSource)
        reparsedBinding <- singleBinding reparsedModule
        assertRoundTripBinding renderedSource (bindingValue, reparsedBinding),
      testCase "record wildcard patterns stay lossy and refuse rendering" $ do
        convertedModule <-
          expectRightWithLabel
            "fixture conversion"
            ( convertHaskellSource
                "Fixture.hs"
                ( unlines
                    [ "module Fixture where",
                      "",
                      "recordWildcard value = case value of { MkRec {..} -> fallback }"
                    ]
                )
            )
        bindingValue <- singleBinding convertedModule
        case stripBindingLambdas (tlbTerm bindingValue) of
          PatternNode (CaseF _ [(wildcardPattern, _)]) ->
            case stripTestPatParens wildcardPattern of
              PLossyP _ _ ->
                pure ()
              otherPattern ->
                assertFailure ("expected record wildcard to stay lossy, got " <> show otherPattern)
          otherBody ->
            assertFailure ("expected a single record-wildcard case alternative, got " <> show otherBody)
        case renderTopLevelBinding "recordWildcard" (tlbTerm bindingValue) of
          Left (RenderPatOpaque _) ->
            pure ()
          otherRendering ->
            assertFailure ("expected RenderPatOpaque refusal, got " <> show otherRendering),
      testCase "constructor-pattern case alternatives convert to PConP, not lossy shapes" $ do
        convertedModule <-
          expectRightWithLabel
            "fixture conversion"
            ( convertHaskellSource
                "Fixture.hs"
                ( unlines
                    [ "module Fixture where",
                      "",
                      "unwrap m = case m of { Just x -> x; Nothing -> fallback }"
                    ]
                )
            )
        bindingValue <- singleBinding convertedModule
        case stripBindingLambdas (tlbTerm bindingValue) of
          PatternNode (CaseF _ branchValues) ->
            case fmap fst branchValues of
              [PConP _ [PVarP _], PConP _ []] ->
                pure ()
              alternativePatterns ->
                assertFailure
                  ("expected faithful constructor patterns, got " <> show alternativePatterns)
          _ ->
            assertFailure "expected the fixture body to convert to a case expression",
      testCase "view-pattern alternatives go lossy and refuse rendering while preserving scope" $ do
        convertedModule <-
          expectRightWithLabel
            "fixture conversion"
            ( convertHaskellSource
                "Fixture.hs"
                ( unlines
                    [ "module Fixture where",
                      "",
                      "viewed x = case x of { (project -> y) -> use y }"
                    ]
                )
            )
        bindingValue <- singleBinding convertedModule
        lossyPattern <-
          case stripBindingLambdas (tlbTerm bindingValue) of
            PatternNode (CaseF _ [(PParP alternativePattern@(PLossyP _ _), _)]) ->
              pure alternativePattern
            otherBody ->
              assertFailure ("expected a single parenthesized lossy view-pattern alternative, got " <> show otherBody)
        case renderTopLevelBinding "viewed" (tlbTerm bindingValue) of
          Left (RenderPatOpaque _) ->
            pure ()
          otherRendering ->
            assertFailure ("expected RenderPatOpaque refusal, got " <> show otherRendering)
        assertBool
          "the swallowed view-pattern binder stays in scope"
          (any ((== "y") . occNameString . rdrNameOcc . baName) (patBinders lossyPattern)),
      testCase "lossy alternatives with different tags are not round-trip equivalent" $ do
        viewModule <-
          expectRightWithLabel
            "view-pattern conversion"
            ( convertHaskellSource
                "Fixture.hs"
                ( unlines
                    [ "module Fixture where",
                      "",
                      "lossy x = case x of { (project -> y) -> use y }"
                    ]
                )
            )
        plusKModule <-
          expectRightWithLabel
            "n-plus-k conversion"
            ( convertHaskellSource
                "Fixture.hs"
                ( unlines
                    [ "{-# LANGUAGE NPlusKPatterns #-}",
                      "module Fixture where",
                      "",
                      "lossy x = case x of { (y + 1) -> use y }"
                    ]
                )
            )
        viewBinding <- singleBinding viewModule
        plusKBinding <- singleBinding plusKModule
        assertBool
          "two distinct lossy pattern kinds must not collapse as equivalent"
          (not (renderRoundTripEquivalent (tlbTerm viewBinding) (tlbTerm plusKBinding))),
      testCase "where pattern bindings convert without opaque local-binds fallback" $ do
        convertedModule <-
          expectRightWithLabel
            "fixture conversion"
            ( convertHaskellSource
                "Fixture.hs"
                ( unlines
                    [ "module Fixture where",
                      "",
                      "clear x = combine a b where (a, b) = splitPair x"
                    ]
                )
            )
        bindingValue <- singleBinding convertedModule
        assertBool
          "where pattern bind must not contain OpaqueF"
          (not (patternContainsOpaque (tlbTerm bindingValue)))
        case stripBindingLambdas (tlbTerm bindingValue) of
          PatternNode (LetF (LetMode _ WhereSyntax) [(bindingPattern, _)] _) ->
            case stripTestPatParens bindingPattern of
              PTupleP [PVarP _, PVarP _] ->
                pure ()
              otherPattern ->
                assertFailure ("expected a tuple pattern where binding, got " <> show otherPattern)
          otherBody ->
            assertFailure ("expected a where group with a pattern binding, got " <> show otherBody),
      testCase "lossy let binding patterns refuse rendering" $ do
        let binderAnn = BinderAnn (BinderId 0) (mkRdrUnqual (mkVarOcc "value"))
            rhsValue :: Pattern HsExprF
            rhsValue = PatternNode (OverLitF (NormalizedIntegralOverLit 1))
        renderHsExpr
          ( PatternNode
              ( LetF
                  (LetMode NonRecursiveBinds LetSyntax)
                  [(PLossyP PatOpaqueView [binderAnn], rhsValue)]
                  (PatternNode (VarF (LocalName binderAnn)))
              )
          )
          @?= Left (RenderPatOpaque PatOpaqueView),
      testCase "var let binding rows render byte-identically" $ do
        let binderAnn = BinderAnn (BinderId 0) (mkRdrUnqual (mkVarOcc "y"))
        renderHsExpr
          ( PatternNode
              ( LetF
                  (LetMode NonRecursiveBinds LetSyntax)
                  [(PVarP binderAnn, PatternNode (OverLitF (NormalizedIntegralOverLit 1)))]
                  (PatternNode (VarF (LocalName binderAnn)))
              )
          )
          @?= Right "let y = 1 in y",
      testCase "render refuses opaque nodes, pattern variables, and lossy patterns" $ do
        let binderAnn = BinderAnn (BinderId 0) (mkRdrUnqual (mkVarOcc "y"))
        renderHsExpr (PatternNode (OpaqueF OpaqueXExpr)) @?= Left (RenderOpaque OpaqueXExpr)
        renderHsExpr (PatternVar (EGraph.mkPatternVar 0)) @?= Left RenderPatternVariable
        renderTopLevelBinding "" (PatternNode (OverLitF (NormalizedIntegralOverLit 1)))
          @?= Left RenderEmptyBindingName
        renderHsExpr
          ( PatternNode
              ( LetF
                  (LetMode NonRecursiveBinds WhereSyntax)
                  [(PVarP binderAnn, PatternNode (OverLitF (NormalizedIntegralOverLit 1)))]
                  (PatternNode (VarF (LocalName binderAnn)))
              )
          )
          @?= Left RenderWhereExpression
        renderHsExpr
          ( PatternNode
              ( CaseF
                  (PatternNode (OverLitF (NormalizedIntegralOverLit 1)))
                  [(PLossyP PatOpaqueSum [], PatternNode (OverLitF (NormalizedIntegralOverLit 2)))]
              )
          )
          @?= Left (RenderPatOpaque PatOpaqueSum),
      testCase "prim literals render with their hash-suffixed forms" $ do
        renderHsExpr (PatternNode (LitF (NormalizedIntPrim 5))) @?= Right "5#"
        renderHsExpr (PatternNode (LitF (NormalizedWordPrim 5))) @?= Right "5##"
        renderHsExpr (PatternNode (LitF (NormalizedDoublePrim 2.5))) @?= Right "2.5##",
      testCase "top-level bindings carry ordered source regions" $ do
        convertedModule <-
          expectRightWithLabel
            "fixture conversion"
            ( convertHaskellSource
                "Fixture.hs"
                ( unlines
                    [ "module Fixture where",
                      "",
                      "first = 1",
                      "",
                      "second x = x"
                    ]
                )
            )
        case fmap tlbRegion (cmBindings convertedModule) of
          [Just firstRegion, Just secondRegion] -> do
            srStartLine firstRegion @?= 3
            srStartLine secondRegion @?= 5
            fmap (sxRegion . tlbSpannedTerm) (cmBindings convertedModule) @?= [Just firstRegion, Just secondRegion]
            assertBool
              "regions must not overlap"
              (srEndLine firstRegion <= srStartLine secondRegion)
          regions ->
            assertFailure ("expected two located bindings, got " <> show regions)
    ]

spanLockstepTests :: TestTree
spanLockstepTests =
  testGroup
    "pale.spans.lockstep"
    [ testCase "tlbSpannedTerm erases to tlbTerm across renderable expression shapes" $ do
        convertedModule <-
          expectRightWithLabel
            "fixture conversion"
            ( convertHaskellSource
                "Fixture.hs"
                ( unlines
                    [ "module Fixture where",
                      "",
                      "lockstep flag action p = do { x <- action; let { y = case p of { (a, b) -> if flag then [a + b, -x] else [x]; _ -> [x] } }; pure (y, MkSettings { width = 3, label = \"wide\" }) }"
                    ]
                )
            )
        assertBool "fixture must contain at least one binding" (not (null (cmBindings convertedModule)))
        mapM_ assertSpannedBindingLockstep (cmBindings convertedModule)
    ]

assertSpannedBindingLockstep :: TopLevelBinding -> IO ()
assertSpannedBindingLockstep bindingValue =
  eraseSpannedExpr (tlbSpannedTerm bindingValue) @?= tlbTerm bindingValue

roundTripCase :: String -> [String] -> TestTree
roundTripCase caseName fixtureLines =
  testCase caseName $ do
    let sourceText = unlines fixtureLines
    convertedModule <- expectRightWithLabel "fixture conversion" (convertHaskellSource "Fixture.hs" sourceText)
    assertBool "fixture must contain at least one binding" (not (null (cmBindings convertedModule)))
    bindingPairs <- traverse namedBinding (cmBindings convertedModule)
    renderedSource <- expectRightWithLabel "render" (renderModuleSource "" (Just "Fixture") bindingPairs)
    reparsedModule <-
      expectRightWithLabel
        ("re-parse of rendered source:\n" <> renderedSource)
        (convertHaskellSource "Fixture.hs" renderedSource)
    length (cmBindings reparsedModule) @?= length (cmBindings convertedModule)
    mapM_
      (assertRoundTripBinding renderedSource)
      (zip (cmBindings convertedModule) (cmBindings reparsedModule))

assertRoundTripBinding :: String -> (TopLevelBinding, TopLevelBinding) -> IO ()
assertRoundTripBinding renderedSource (originalBinding, reparsedBinding) = do
  bindingNames originalBinding @?= bindingNames reparsedBinding
  eraseSpannedExpr (tlbSpannedTerm originalBinding) @?= tlbTerm originalBinding
  eraseSpannedExpr (tlbSpannedTerm reparsedBinding) @?= tlbTerm reparsedBinding
  assertBool
    ( "binding "
        <> show (bindingNames originalBinding)
        <> " is not round-trip equivalent; rendered source:\n"
        <> renderedSource
    )
    (renderRoundTripEquivalent (tlbTerm originalBinding) (tlbTerm reparsedBinding))

bindingNames :: TopLevelBinding -> [String]
bindingNames =
  fmap (occNameString . rdrNameOcc) . tlbNames

namedBinding :: TopLevelBinding -> IO (String, Pattern HsExprF)
namedBinding bindingValue =
  case tlbNames bindingValue of
    [bindingName] ->
      pure (occNameString (rdrNameOcc bindingName), tlbTerm bindingValue)
    names ->
      assertFailure ("fixture bindings must carry exactly one name, got " <> show (length names))

singleBinding :: ConvertedModule -> IO TopLevelBinding
singleBinding convertedModule =
  case cmBindings convertedModule of
    [bindingValue] -> pure bindingValue
    bindingValues -> assertFailure ("expected exactly one binding, got " <> show (length bindingValues))

stripBindingLambdas :: Pattern HsExprF -> Pattern HsExprF
stripBindingLambdas = \case
  PatternNode (LamF _ bodyValue) -> stripBindingLambdas bodyValue
  patternValue -> patternValue

stripTestPatParens :: HsPatF -> HsPatF
stripTestPatParens = \case
  PParP innerPattern -> stripTestPatParens innerPattern
  patternValue -> patternValue

patternContainsOpaque :: Pattern HsExprF -> Bool
patternContainsOpaque = \case
  PatternVar {} -> False
  PatternNode (OpaqueF _) -> True
  PatternNode layer -> any patternContainsOpaque layer

patternContainsExprWithTySig :: Pattern HsExprF -> Bool
patternContainsExprWithTySig = \case
  PatternVar {} -> False
  PatternNode (ExprWithTySigF _ _) -> True
  PatternNode layer -> any patternContainsExprWithTySig layer

patternContainsAppType :: Pattern HsExprF -> Bool
patternContainsAppType = \case
  PatternVar {} -> False
  PatternNode (AppTypeF _ _) -> True
  PatternNode layer -> any patternContainsAppType layer
