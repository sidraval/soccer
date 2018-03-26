{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Brick
import           Brick.Widgets.Border
import           Brick.Widgets.Border.Style
import           Brick.Widgets.Center
import           Control.Monad
import           Fixture
import           Lens.Micro

main :: IO ()
main = do
  fixtures <- getFixtures
  void $ case fixtures of
      Just (Fixtures xs) -> simpleMain $ customWidget xs
      _                  -> return ()

drawRows :: Int -> [Fixture] -> Widget ()
drawRows _ [] = emptyWidget
drawRows columns xs = hCenter $ hBox (displaySizedGame <$> Prelude.take columns xs)
  <=> drawRows columns (Prelude.drop columns xs)

customWidget :: [Fixture] -> Widget ()
customWidget xs =
    Widget Fixed Fixed $ do
        ctx <- getContext
        let thing = ctx ^. availWidthL `div` 60
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
