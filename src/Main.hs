{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Monoid
import Lens.Micro
import Brick
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Brick.Widgets.Center
import Data.Aeson
import Data.ByteString.Lazy.Char8 as LBS
import GHC.Generics
import Network.HTTP
import Data.String

newtype Fixtures =
  Fixtures [Fixture]
  deriving (Show)

instance FromJSON Fixtures where
  parseJSON = withObject "Fixtures" $ \v -> Fixtures <$> v .: "fixtures"

data Fixture = Fixture
  { status :: FixtureStatus
  , homeTeamName :: String
  , awayTeamName :: String
  , result :: FixtureResult
  } deriving (Show)

instance FromJSON Fixture where
  parseJSON =
    withObject "Fixture" $ \v ->
      Fixture <$> v .: "status" <*> v .: "homeTeamName" <*> v .: "awayTeamName" <*>
      v .: "result"

data FixtureStatus
  = Finished
  | InPlay
  | Timed
  | Postponed
  deriving (Show)

instance FromJSON FixtureStatus where
  parseJSON (String s) = return $ mkFixtureStatus s
  parseJSON _ = fail "Invalid JSON FixtureStatus"

mkFixtureStatus :: (Eq a, Data.String.IsString a) => a -> FixtureStatus
mkFixtureStatus "IN_PLAY" = InPlay
mkFixtureStatus "TIMED" = Timed
mkFixtureStatus "FINISHED" = Finished
mkFixtureStatus "POSTPONED" = Postponed
mkFixtureStatus _ = error "Invalid JSON mkFixtureStatus"

data FixtureResult = FixtureResult
  { goalsHomeTeam :: Maybe Int
  , goalsAwayTeam :: Maybe Int
  } deriving (Generic, Show)

instance FromJSON FixtureResult

data League = Premier deriving (Show)

main :: IO ()
main = do
  let url = "http://www.football-data.org/v1/fixtures?league=EL1&timeFrame=p4"
  resp <- simpleHTTP $ getRequest url
  responseBody <- getResponseBody resp
  let fixtures :: Maybe Fixtures = decode $ LBS.pack responseBody
  _ <-
    case fixtures of
      Just (Fixtures xs) ->
        -- simpleMain . hBox $ displaySizedGame <$> Prelude.take 10 xs
        simpleMain $ customWidget "Testing..."
        -- simpleMain $
        -- Prelude.foldl
        --   (<=>)
        --   emptyWidget 
        --   (displayGame <$> Prelude.take 4 xs)
      _ -> return ()
  return ()

customWidget :: String -> Widget ()
customWidget s =
    Widget Fixed Fixed $ do
        ctx <- getContext
        render $ str (s <> " " <> show (ctx^.availWidthL))

displaySizedGame :: Fixture -> Widget ()
displaySizedGame f = padAll 5 $ setAvailableSize (35, 3) $ displayGame f

displayGame :: Fixture -> Widget ()
displayGame f =
  withBorderStyle unicode $
  borderWithLabel
    (str $ homeTeamName f ++ " vs. " ++ awayTeamName f)
    (center (str (show . goalsHomeTeam . result $ f)) <+>
     vBorder <+> center (str (show . goalsAwayTeam . result $ f)))
