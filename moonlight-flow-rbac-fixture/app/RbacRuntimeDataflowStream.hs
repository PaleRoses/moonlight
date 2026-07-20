module Main (main) where

import Control.Concurrent
  ( threadDelay,
  )
import Data.Bifunctor
  ( first,
  )
import System.Environment
  ( getArgs,
  )
import System.Exit
  ( die,
  )
import System.IO
  ( hFlush,
    stdout,
  )
import Moonlight.Flow.Runtime.RbacDataflowFixture
  ( RbacDataflowLiveState,
    RuntimeDataflowSnapshot,
    initialRbacDataflowLiveState,
    initialRbacDataflowLiveStateWith,
    rbacDataflowWorkloadPatchShape,
    runtimeDataflowSnapshotHex,
    stepRbacDataflowLiveState,
  )
import Moonlight.Flow.Runtime.RbacFixture.Types
  ( RbacPatchShape (..),
  )
import Text.Read
  ( readMaybe,
  )

data StreamConfig = StreamConfig
  { scIntervalMicros :: !Int,
    scFrameLimit :: !(Maybe Int),
    scPatchShape :: !(Maybe RbacPatchShape)
  }

defaultStreamConfig :: StreamConfig
defaultStreamConfig =
  StreamConfig
    { scIntervalMicros = 750000,
      scFrameLimit = Nothing,
      scPatchShape = Nothing
    }

main :: IO ()
main =
  getArgs >>= either die runRuntimeDataflowStream . parseStreamConfig defaultStreamConfig

parseStreamConfig :: StreamConfig -> [String] -> Either String StreamConfig
parseStreamConfig config args =
  case args of
    [] ->
      Right config
    "--interval-ms" : value : rest ->
      parsePositiveInt "--interval-ms" value
        >>= \millis ->
          parseStreamConfig
            config {scIntervalMicros = millis * 1000}
            rest
    "--frames" : value : rest ->
      parsePositiveInt "--frames" value
        >>= \frames ->
          parseStreamConfig
            config {scFrameLimit = Just frames}
            rest
    "--member-moves" : value : rest ->
      parsePatchShapeField "--member-moves" value config rest $
        \parsed shape -> shape {rpsMemberMoves = parsed}
    "--user-attr-moves" : value : rest ->
      parsePatchShapeField "--user-attr-moves" value config rest $
        \parsed shape -> shape {rpsUserAttrMoves = parsed}
    "--resource-scope-moves" : value : rest ->
      parsePatchShapeField "--resource-scope-moves" value config rest $
        \parsed shape -> shape {rpsResourceScopeMoves = parsed}
    "--role-action-moves" : value : rest ->
      parsePatchShapeField "--role-action-moves" value config rest $
        \parsed shape -> shape {rpsRoleActionMoves = parsed}
    "--group-role-moves" : value : rest ->
      parsePatchShapeField "--group-role-moves" value config rest $
        \parsed shape -> shape {rpsGroupRoleMoves = parsed}
    "--deny-moves" : value : rest ->
      parsePatchShapeField "--deny-moves" value config rest $
        \parsed shape -> shape {rpsDenyMoves = parsed}
    "--group-scope-moves" : value : rest ->
      parsePatchShapeField "--group-scope-moves" value config rest $
        \parsed shape -> shape {rpsGroupScopeMoves = parsed}
    _ ->
      Left "usage: moonlight-rbac-runtime-dataflow-stream [--interval-ms positive-int] [--frames positive-int] [--member-moves non-negative-int] [--user-attr-moves non-negative-int] [--resource-scope-moves non-negative-int] [--role-action-moves non-negative-int] [--group-role-moves non-negative-int] [--deny-moves non-negative-int] [--group-scope-moves non-negative-int]"

parsePatchShapeField ::
  String ->
  String ->
  StreamConfig ->
  [String] ->
  (Int -> RbacPatchShape -> RbacPatchShape) ->
  Either String StreamConfig
parsePatchShapeField label value config rest updateShape =
  parseNonNegativeInt label value
    >>= \parsed ->
      parseStreamConfig
        config
          { scPatchShape =
              Just $
                updateShape parsed $
                  maybe rbacDataflowWorkloadPatchShape id (scPatchShape config)
          }
        rest

parsePositiveInt :: String -> String -> Either String Int
parsePositiveInt label value =
  case readMaybe value of
    Just parsed | parsed > 0 ->
      Right parsed
    _ ->
      Left (label <> " expects a positive integer, got: " <> value)

parseNonNegativeInt :: String -> String -> Either String Int
parseNonNegativeInt label value =
  case readMaybe value of
    Just parsed | parsed >= 0 ->
      Right parsed
    _ ->
      Left (label <> " expects a non-negative integer, got: " <> value)

runRuntimeDataflowStream :: StreamConfig -> IO ()
runRuntimeDataflowStream config =
  either die (streamRuntimeDataflow config 0) (initialStateForConfig config)

initialStateForConfig :: StreamConfig -> Either String RbacDataflowLiveState
initialStateForConfig config =
  first show $
    maybe initialRbacDataflowLiveState initialRbacDataflowLiveStateWith (scPatchShape config)

streamRuntimeDataflow :: StreamConfig -> Int -> RbacDataflowLiveState -> IO ()
streamRuntimeDataflow config emitted state =
  if streamComplete config emitted
    then pure ()
    else
      case stepRbacDataflowLiveState state of
        Left err ->
          die (show err)
        Right (snapshot, nextState) -> do
          emitRuntimeDataflowSnapshotEvent emitted snapshot
          threadDelay (scIntervalMicros config)
          streamRuntimeDataflow config (emitted + 1) nextState

streamComplete :: StreamConfig -> Int -> Bool
streamComplete config emitted =
  maybe False (emitted >=) (scFrameLimit config)

emitRuntimeDataflowSnapshotEvent :: Int -> RuntimeDataflowSnapshot -> IO ()
emitRuntimeDataflowSnapshotEvent index snapshot = do
  putStr "event: snapshot\n"
  putStr ("id: " <> show index <> "\n")
  putStr ("data: " <> runtimeDataflowSnapshotHex snapshot <> "\n\n")
  hFlush stdout
