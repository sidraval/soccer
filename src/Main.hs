{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Main where

import           Brick
import           Brick.Forms
import           Brick.Widgets.Border
import           Brick.Widgets.Border.Style
import           Brick.Widgets.Center
import           Control.Monad
import           Fixture
import           Lens.Micro
import           Lens.Micro.TH

type Columns = Int
type Width = Int

data League = Premier | Champions deriving (Show, Eq)
data AppInfo = FormState  { _league :: League } deriving (Show)

makeLenses ''AppInfo

data Name = PremierField
          | ChampionsField
          deriving (Eq, Ord, Show)

mkLeagueSelection :: AppInfo -> Form AppInfo e Name
mkLeagueSelection = newForm [ radioField league [ (Premier, PremierField, "Premier League")
                                                , (Champions, ChampionsField, "Champions League")
                                                ]
                            ]

drawRows :: Columns -> [Fixture] -> Widget ()
drawRows _ [] = emptyWidget
drawRows columns xs = hCenter (hBox (displaySizedGame <$> Prelude.take columns xs))
  <=> drawRows columns (Prelude.drop columns xs)

resizingGrid :: Width -> [Fixture] -> Widget ()
resizingGrid width xs =
    Widget Fixed Fixed $ do
        ctx <- getContext
        let thing = ctx ^. availWidthL `div` width
        render $ drawRows thing xs

displaySizedGame :: Fixture -> Widget ()
displaySizedGame f = padBottom (Pad 2) $ padRight (Pad 5) $ setAvailableSize (55, 5) $ displayGame f

displayGame :: Fixture -> Widget ()
displayGame f =
  withBorderStyle unicode $
  borderWithLabel
    (str $ homeTeamName f ++ " vs. " ++ awayTeamName f)
    (center (str (goalsNum . goalsHomeTeam . result $ f)) <+>
     vBorder <+> center (str (goalsNum . goalsAwayTeam . result $ f)))

goalsNum :: Maybe Int -> String
goalsNum (Just x) = show x
goalsNum Nothing  = "-"

main :: IO ()
main = do
  fixtures <- getFixtures
  void $ case fixtures of
      Just (Fixtures xs) -> simpleMain $ renderForm $ mkLeagueSelection FormState { _league = Premier }
      _                  -> return ()
