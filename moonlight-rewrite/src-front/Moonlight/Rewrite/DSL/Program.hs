{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Program builder for the front DSL.
-- Owns ordered declarations of rules, contexts, and macros, including captured
-- call sites for later typed diagnostics.
-- Contracts: macro composition records a non-empty path of rule-name
-- references; names remain raw until elaboration validates them.
module Moonlight.Rewrite.DSL.Program
  ( ContextName,
    ContextNameError (..),
    contextName,
    contextNameString,
    RuleNameRef (..),
    MacroPath,
    macroPathRefs,
    MacroDecl (..),
    RuleDecl (..),
    ContextDecl (..),
    Program (..),
    ProgramM,
    program,
    rule,
    ruleBi,
    context,
    macro,
    compose,
    (>>>),
  )
where

import Control.Monad.Trans.State.Strict (State, execState, modify')
import Data.List.NonEmpty (NonEmpty (..))
import Data.Sequence (Seq, (|>))
import Data.Sequence qualified as Seq
import GHC.Stack
  ( CallStack,
    HasCallStack,
    SrcLoc,
    callStack,
    getCallStack,
  )
import Moonlight.Rewrite.DSL.Rule
  ( ContextName,
    ContextNameError (..),
    RuleBody,
    (==>),
    contextName,
    contextNameString,
  )
import Moonlight.Rewrite.DSL.Term (Term)

data RuleNameRef = RuleNameRef
  { rnrRawName :: !String,
    rnrCallSite :: !(Maybe SrcLoc)
  }
  deriving stock (Show)

newtype MacroPath = MacroPath
  { unMacroPath :: NonEmpty RuleNameRef
  }
  deriving stock (Show)

data RuleDecl sig atom = RuleDecl
  { rdName :: !String,
    rdBody :: !(RuleBody sig atom),
    rdCallSite :: !(Maybe SrcLoc)
  }

data ContextDecl = ContextDecl
  { cdName :: !String,
    cdCallSite :: !(Maybe SrcLoc)
  }
  deriving stock (Show)

data MacroDecl = MacroDecl
  { mdName :: !String,
    mdPath :: !MacroPath,
    mdCallSite :: !(Maybe SrcLoc)
  }
  deriving stock (Show)

data Program sig atom = Program
  { pRules :: !(Seq (RuleDecl sig atom)),
    pContexts :: !(Seq ContextDecl),
    pMacros :: !(Seq MacroDecl)
  }

newtype ProgramM sig atom a = ProgramM
  { runProgramM :: State (Program sig atom) a
  }
  deriving newtype (Functor, Applicative, Monad)

program :: ProgramM sig atom () -> Program sig atom
program action =
  execState (runProgramM action) emptyProgram

rule :: HasCallStack => String -> RuleBody sig atom -> ProgramM sig atom ()
rule rawName ruleBody =
  ProgramM
    ( modify'
        ( \builder ->
            builder
              { pRules =
                  pRules builder
                    |> RuleDecl
                      { rdName = rawName,
                        rdBody = ruleBody,
                        rdCallSite = currentSrcLoc callStack
                      }
              }
        )
    )

ruleBi :: HasCallStack => String -> Term sig sort -> Term sig sort -> ProgramM sig atom ()
ruleBi rawName leftTerm rightTerm =
  rule (rawName <> ".fwd") (leftTerm ==> rightTerm)
    *> rule (rawName <> ".bwd") (rightTerm ==> leftTerm)

context :: HasCallStack => String -> ProgramM sig atom ()
context rawName =
  ProgramM
    ( modify'
        ( \builder ->
            builder
              { pContexts =
                  pContexts builder
                    |> ContextDecl
                      { cdName = rawName,
                        cdCallSite = currentSrcLoc callStack
                      }
              }
        )
    )

macro :: HasCallStack => String -> MacroPath -> ProgramM sig atom ()
macro rawName macroPath =
  ProgramM
    ( modify'
        ( \builder ->
            builder
              { pMacros =
                  pMacros builder
                    |> MacroDecl
                      { mdName = rawName,
                        mdPath = macroPath,
                        mdCallSite = currentSrcLoc callStack
                      }
              }
        )
    )

compose :: HasCallStack => String -> MacroPath
compose rawName =
  MacroPath
    ( RuleNameRef
        { rnrRawName = rawName,
          rnrCallSite = currentSrcLoc callStack
        }
        :| []
    )

infixr 1 >>>

(>>>) :: MacroPath -> MacroPath -> MacroPath
MacroPath leftRefs >>> MacroPath rightRefs =
  MacroPath (leftRefs <> rightRefs)

macroPathRefs :: MacroPath -> NonEmpty RuleNameRef
macroPathRefs (MacroPath refs) =
  refs

emptyProgram :: Program sig atom
emptyProgram =
  Program
    { pRules = Seq.empty,
      pContexts = Seq.empty,
      pMacros = Seq.empty
    }

currentSrcLoc :: CallStack -> Maybe SrcLoc
currentSrcLoc stack =
  case getCallStack stack of
    [] ->
      Nothing
    (_, srcLoc) : _ ->
      Just srcLoc
