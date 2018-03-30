{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Main where

import           Brick
import           Brick.Focus
import           Brick.Forms
import           Brick.Widgets.Border
import           Brick.Widgets.Border.Style
import           Brick.Widgets.Center
import           Control.Monad
import           Control.Monad.IO.Class
import           Fixture
import qualified Graphics.Vty as V
import           Lens.Micro
import           Lens.Micro.TH

type Columns = Int
type Width = Int

data League = Premier | Champions deriving (Show, Eq)
data AppInfo = AppInfo  { _league :: League, _fixtures :: Maybe Fixtures } deriving (Show)

makeLenses ''AppInfo

data Name = PremierField
          | ChampionsField
          deriving (Eq, Ord, Show)

mkLeagueSelection :: AppInfo -> Form AppInfo e Name
mkLeagueSelection = newForm [ radioField league [ (Premier, PremierField, "Premier League")
                                                , (Champions, ChampionsField, "Champions League")
                                                ]
                            ]

drawRows :: Columns -> [Fixture] -> Widget Name
drawRows _ [] = emptyWidget
drawRows columns xs = hCenter (hBox (displaySizedGame <$> Prelude.take columns xs))
  <=> drawRows columns (Prelude.drop columns xs)

resizingGrid :: Width -> [Fixture] -> Widget Name
resizingGrid width xs =
    Widget Fixed Fixed $ do
        ctx <- getContext
        let thing = ctx ^. availWidthL `div` width
        render $ drawRows thing xs

displaySizedGame :: Fixture -> Widget Name
displaySizedGame f = padBottom (Pad 2) $ padRight (Pad 5) $ setAvailableSize (55, 5) $ displayGame f

displayGame :: Fixture -> Widget Name
displayGame f =
  withBorderStyle unicode $
  borderWithLabel
    (str $ homeTeamName f ++ " vs. " ++ awayTeamName f)
    (center (str (goalsNum . goalsHomeTeam . result $ f)) <+>
     vBorder <+> center (str (goalsNum . goalsAwayTeam . result $ f)))

goalsNum :: Maybe Int -> String
goalsNum (Just x) = show x
goalsNum Nothing  = "-"

draw :: Form AppInfo e Name -> [Widget Name]
draw f = case formState f of
  AppInfo { _league = Champions, _fixtures = Just (Fixtures xs) } -> [resizingGrid 60 xs]
  _ -> [renderForm f]

theMap :: AttrMap
theMap = attrMap V.defAttr [(focusedFormInputAttr, V.black `on` V.yellow)]

app :: App (Form AppInfo e Name) e Name
app =
  App { appDraw = draw
      , appHandleEvent = \s ev ->
          case ev of
            VtyEvent V.EvResize {} -> continue s
            VtyEvent (V.EvKey V.KEsc []) -> halt s
            VtyEvent (V.EvKey V.KEnter []) -> do
              fxt <- liftIO getFixtures
              s' <- handleFormEvent ev s
              let s'' = formState s' & fixtures .~ fxt
              continue (s' { formState = s'' })
            _ -> do
              s' <- handleFormEvent ev s
              continue s'
      , appChooseCursor = focusRingCursor formFocus
      , appStartEvent = return
      , appAttrMap = const theMap
      }

main :: IO ()
main = do
  let buildVty = do
        v <- V.mkVty =<< V.standardIOConfig
        V.setMode (V.outputIface v) V.Mouse True
        return v
      initialAppInfo = AppInfo { _league = Premier, _fixtures = Nothing }
      form = mkLeagueSelection initialAppInfo
  void $ customMain buildVty Nothing app form
