import Lean

open Lean

namespace Moonlight.EGraph

structure FiniteLattice where
  size : Nat
  top : Nat
  bottom : Nat
  joinTable : Array (Array Nat)
  leqTable : Array (Array Bool)
deriving Repr, FromJson

structure KernelContextMorphism where
  source : Nat
  target : Nat
deriving Repr, FromJson, ToJson, BEq

inductive KernelRequest where
  | mkContextMorphism (lattice : FiniteLattice) (source target : Nat)
  | identityContextMorphism (context : Nat)
  | composeContextMorphism (lattice : FiniteLattice) (left right : KernelContextMorphism)
  | principalSupport (context : Nat)
  | normalizeSupport (lattice : FiniteLattice) (generators : List Nat)
  | supportContains (lattice : FiniteLattice) (generators : List Nat) (context : Nat)
  | supportUnion (lattice : FiniteLattice) (leftGenerators rightGenerators : List Nat)
  | supportMeet (lattice : FiniteLattice) (leftGenerators rightGenerators : List Nat)
deriving Repr

inductive KernelResponse where
  | morphism (value : KernelContextMorphism)
  | none
  | support (generators : List Nat)
  | contains (value : Bool)
deriving Repr, BEq

instance : FromJson KernelRequest where
  fromJson? json := do
    let tag ← json.getObjValAs? String "tag"
    match tag with
    | "mk_context_morphism" => do
        let lattice ← json.getObjValAs? FiniteLattice "lattice"
        let source ← json.getObjValAs? Nat "source"
        let target ← json.getObjValAs? Nat "target"
        pure (.mkContextMorphism lattice source target)
    | "identity_context_morphism" => do
        let context ← json.getObjValAs? Nat "context"
        pure (.identityContextMorphism context)
    | "compose_context_morphism" => do
        let lattice ← json.getObjValAs? FiniteLattice "lattice"
        let left ← json.getObjValAs? KernelContextMorphism "left"
        let right ← json.getObjValAs? KernelContextMorphism "right"
        pure (.composeContextMorphism lattice left right)
    | "principal_support" => do
        let context ← json.getObjValAs? Nat "context"
        pure (.principalSupport context)
    | "normalize_support" => do
        let lattice ← json.getObjValAs? FiniteLattice "lattice"
        let generators ← json.getObjValAs? (List Nat) "generators"
        pure (.normalizeSupport lattice generators)
    | "support_contains" => do
        let lattice ← json.getObjValAs? FiniteLattice "lattice"
        let generators ← json.getObjValAs? (List Nat) "generators"
        let context ← json.getObjValAs? Nat "context"
        pure (.supportContains lattice generators context)
    | "support_union" => do
        let lattice ← json.getObjValAs? FiniteLattice "lattice"
        let leftGenerators ← json.getObjValAs? (List Nat) "leftGenerators"
        let rightGenerators ← json.getObjValAs? (List Nat) "rightGenerators"
        pure (.supportUnion lattice leftGenerators rightGenerators)
    | "support_meet" => do
        let lattice ← json.getObjValAs? FiniteLattice "lattice"
        let leftGenerators ← json.getObjValAs? (List Nat) "leftGenerators"
        let rightGenerators ← json.getObjValAs? (List Nat) "rightGenerators"
        pure (.supportMeet lattice leftGenerators rightGenerators)
    | otherTag =>
        throw s!"unknown Phase A kernel request tag '{otherTag}'"

instance : ToJson KernelResponse where
  toJson
    | .morphism value =>
        Json.mkObj
          [ ("tag", toJson "morphism"),
            ("source", toJson value.source),
            ("target", toJson value.target)
          ]
    | .none =>
        Json.mkObj [("tag", toJson "none")]
    | .support generators =>
        Json.mkObj
          [ ("tag", toJson "support"),
            ("generators", toJson generators)
          ]
    | .contains value =>
        Json.mkObj
          [ ("tag", toJson "contains"),
            ("value", toJson value)
          ]

def validateRows {α : Type} (expectedSize : Nat) (rows : Array (Array α)) (name : String) :
    Except String Unit :=
  let rec go (index : Nat) : Except String Unit :=
    if h : index < rows.size then
      let row := rows[index]
      if Array.size row == expectedSize then
        go (index + 1)
      else
        throw s!"{name} row {index} has size {Array.size row}, expected {expectedSize}"
    else
      pure ()
  go 0

def validateContextIndex (lattice : FiniteLattice) (context : Nat) : Except String Nat :=
  if context < lattice.size then
    pure context
  else
    throw s!"context index {context} is out of bounds for lattice of size {lattice.size}"

def validateFiniteLattice (lattice : FiniteLattice) : Except String FiniteLattice := do
  if lattice.joinTable.size != lattice.size then
    throw s!"joinTable has {lattice.joinTable.size} rows, expected {lattice.size}"
  if lattice.leqTable.size != lattice.size then
    throw s!"leqTable has {lattice.leqTable.size} rows, expected {lattice.size}"
  discard <| validateRows lattice.size lattice.joinTable "joinTable"
  discard <| validateRows lattice.size lattice.leqTable "leqTable"
  discard <| validateContextIndex lattice lattice.top
  discard <| validateContextIndex lattice lattice.bottom
  pure lattice

def latticeLeq (lattice : FiniteLattice) (left right : Nat) : Bool :=
  (lattice.leqTable[left]!)[right]!

def latticeJoin (lattice : FiniteLattice) (left right : Nat) : Nat :=
  (lattice.joinTable[left]!)[right]!

def listContainsContext (generators : List Nat) (context : Nat) : Bool :=
  generators.any fun generator => generator == context

def canonicalGenerators (lattice : FiniteLattice) (generators : List Nat) : List Nat :=
  (List.range lattice.size).filter fun context => listContainsContext generators context

def normalizeSupportCore (lattice : FiniteLattice) (generators : List Nat) : List Nat :=
  let canonical := canonicalGenerators lattice generators
  (List.range lattice.size).filter fun candidate =>
    listContainsContext canonical candidate
      && !(canonical.any fun generator =>
        !(generator == candidate) && latticeLeq lattice generator candidate)

def validateGenerators (lattice : FiniteLattice) (generators : List Nat) : Except String (List Nat) :=
  generators.mapM (validateContextIndex lattice)

def normalizeSupport (lattice : FiniteLattice) (generators : List Nat) : Except String (List Nat) := do
  let validLattice <- validateFiniteLattice lattice
  let validGenerators <- validateGenerators validLattice generators
  pure (normalizeSupportCore validLattice validGenerators)

def supportContains (lattice : FiniteLattice) (generators : List Nat) (context : Nat) :
    Except String Bool := do
  let validLattice <- validateFiniteLattice lattice
  let validContext <- validateContextIndex validLattice context
  let normalized <- normalizeSupport validLattice generators
  pure (normalized.any fun generator => latticeLeq validLattice generator validContext)

def supportUnion (lattice : FiniteLattice) (leftGenerators rightGenerators : List Nat) :
    Except String (List Nat) :=
  normalizeSupport lattice (leftGenerators ++ rightGenerators)

def supportMeet (lattice : FiniteLattice) (leftGenerators rightGenerators : List Nat) :
    Except String (List Nat) := do
  let validLattice <- validateFiniteLattice lattice
  let validLeft <- validateGenerators validLattice leftGenerators
  let validRight <- validateGenerators validLattice rightGenerators
  let joined :=
    validLeft.foldr
      (fun leftGenerator joinedGenerators =>
        (validRight.map fun rightGenerator => latticeJoin validLattice leftGenerator rightGenerator)
          ++ joinedGenerators)
      []
  normalizeSupport validLattice joined

def mkContextMorphism (lattice : FiniteLattice) (source target : Nat) :
    Except String (Option KernelContextMorphism) := do
  let validLattice <- validateFiniteLattice lattice
  let validSource <- validateContextIndex validLattice source
  let validTarget <- validateContextIndex validLattice target
  pure <|
    if latticeLeq validLattice validTarget validSource then
      some { source := validSource, target := validTarget }
    else
      none

def identityContextMorphism (context : Nat) : KernelContextMorphism :=
  { source := context, target := context }

def composeContextMorphism (lattice : FiniteLattice)
    (left right : KernelContextMorphism) :
    Except String (Option KernelContextMorphism) := do
  let validLattice <- validateFiniteLattice lattice
  discard <| validateContextIndex validLattice left.source
  discard <| validateContextIndex validLattice left.target
  discard <| validateContextIndex validLattice right.source
  discard <| validateContextIndex validLattice right.target
  if left.target == right.source then
    mkContextMorphism validLattice left.source right.target
  else
    pure none

def evaluateRequest : KernelRequest → Except String KernelResponse
  | .mkContextMorphism lattice source target => do
      match ← mkContextMorphism lattice source target with
      | some value => pure (.morphism value)
      | none => pure .none
  | .identityContextMorphism context =>
      pure (.morphism (identityContextMorphism context))
  | .composeContextMorphism lattice left right => do
      match ← composeContextMorphism lattice left right with
      | some value => pure (.morphism value)
      | none => pure .none
  | .principalSupport context =>
      pure (.support [context])
  | .normalizeSupport lattice generators => do
      pure (.support (← normalizeSupport lattice generators))
  | .supportContains lattice generators context => do
      pure (.contains (← supportContains lattice generators context))
  | .supportUnion lattice leftGenerators rightGenerators => do
      pure (.support (← supportUnion lattice leftGenerators rightGenerators))
  | .supportMeet lattice leftGenerators rightGenerators => do
      pure (.support (← supportMeet lattice leftGenerators rightGenerators))

def evaluateRequests (requests : List KernelRequest) : Except String (List KernelResponse) :=
  requests.mapM evaluateRequest

def emitResult (result : Except String (List KernelResponse)) : IO UInt32 :=
  match result with
  | .ok responses =>
      do
        IO.println (Json.compress (toJson responses))
        pure 0
  | .error message =>
      do
        IO.eprintln message
        pure 1

def main (_args : List String) : IO UInt32 := do
  let input <- (← IO.getStdin).readToEnd
  emitResult <| do
    let json <- Json.parse input
    let requests <- (fromJson? json : Except String (List KernelRequest))
    evaluateRequests requests

end Moonlight.EGraph

def main (args : List String) : IO UInt32 :=
  Moonlight.EGraph.main args
