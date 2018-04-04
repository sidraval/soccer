{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Types where

import           Data.Aeson
import           Data.ByteString.Lazy.Char8 as LBS
import           Data.Char
import           Data.List
import           Data.String
import           GHC.Generics
import           Network.HTTP

type Columns = Int
type Width = Int

data League = Premier | Champions deriving (Show, Eq)

leagueSymbol :: League -> String
leagueSymbol Premier   = "PL"
leagueSymbol Champions = "CL"

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

getFixtures :: League -> IO [Fixture]
getFixtures l = do
  let url = "http://www.football-data.org/v1/fixtures?league=" ++ leagueSymbol l ++ "&timeFrame=p3"
  resp <- simpleHTTP $ getRequest url
  responseBody <- getResponseBody resp
  let fixtures :: Maybe Fixtures = decode . LBS.pack $ responseBody
  return $ case fixtures of
    (Just (Fixtures xs)) -> xs
    _                    -> []

filterFixtures :: [Fixture] -> String -> [Fixture]
filterFixtures xs f = Prelude.filter (\fixture -> lcaseString `isInfixOf` lcaseOther (name fixture)) xs
  where lcaseString = toLower <$> f; lcaseOther = fmap toLower

name :: Fixture -> String
name f = homeTeamName f ++ " vs. " ++ awayTeamName f
