{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Fixture where

import           Data.Aeson
import           Data.ByteString.Lazy.Char8 as LBS
import           Data.String
import           GHC.Generics
import           Network.HTTP

newtype Fixtures =
  Fixtures [Fixture]
  deriving (Show)

instance FromJSON Fixtures where
  parseJSON = withObject "Fixtures" $ \v -> Fixtures <$> v .: "fixtures"

data Fixture = Fixture
  { status       :: FixtureStatus
  , homeTeamName :: String
  , awayTeamName :: String
  , result       :: FixtureResult
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
  parseJSON _          = fail "Invalid JSON FixtureStatus"

mkFixtureStatus :: (Eq a, Data.String.IsString a) => a -> FixtureStatus
mkFixtureStatus "IN_PLAY"   = InPlay
mkFixtureStatus "TIMED"     = Timed
mkFixtureStatus "FINISHED"  = Finished
mkFixtureStatus "POSTPONED" = Postponed
mkFixtureStatus _           = error "Invalid JSON mkFixtureStatus"

data FixtureResult = FixtureResult
  { goalsHomeTeam :: Maybe Int
  , goalsAwayTeam :: Maybe Int
  } deriving (Generic, Show)

instance FromJSON FixtureResult

getFixtures :: IO (Maybe Fixtures)
getFixtures = do
  let url = "http://www.football-data.org/v1/fixtures?league=EL1&timeFrame=p4"
  resp <- simpleHTTP $ getRequest url
  responseBody <- getResponseBody resp
  return . decode $ LBS.pack responseBody