module Moonlight.Pale.Test.Site.Fixture
  ( FixtureM (..),
    runFixture,
    withFixture,
    fixtureFromEither,
    fixtureFromMaybe,
    fixtureFromIO,
  )
where

import Data.Kind (Type)
import GHC.Stack (HasCallStack)
import Test.Tasty.HUnit (Assertion, assertFailure)

type FixtureM :: Type -> Type
newtype FixtureM a = FixtureM {unFixtureM :: IO (Either String a)}

instance Functor FixtureM where
  fmap f (FixtureM action) = FixtureM (fmap (fmap f) action)

instance Applicative FixtureM where
  pure a = FixtureM (pure (Right a))
  FixtureM ff <*> FixtureM fa = FixtureM $ do
    ef <- ff
    case ef of
      Left err -> pure (Left err)
      Right f -> fmap (fmap f) fa

instance Monad FixtureM where
  FixtureM ma >>= f = FixtureM $ do
    ea <- ma
    case ea of
      Left err -> pure (Left err)
      Right a -> unFixtureM (f a)

runFixture :: HasCallStack => String -> FixtureM a -> IO a
runFixture label (FixtureM action) = do
  result <- action
  case result of
    Left err -> assertFailure (label <> ": " <> err)
    Right val -> pure val

withFixture :: HasCallStack => String -> FixtureM a -> (a -> Assertion) -> Assertion
withFixture label fixture check = do
  val <- runFixture label fixture
  check val

fixtureFromEither :: Either String a -> FixtureM a
fixtureFromEither = FixtureM . pure

fixtureFromMaybe :: String -> Maybe a -> FixtureM a
fixtureFromMaybe label result =
  case result of
    Nothing -> FixtureM (pure (Left label))
    Just val -> FixtureM (pure (Right val))

fixtureFromIO :: IO (Either String a) -> FixtureM a
fixtureFromIO = FixtureM
