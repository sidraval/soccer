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
import qualified Graphics.Vty as V
import           Lens.Micro
import           Lens.Micro.TH
import           Types

data AppInfo = AppInfo  { _league           :: League
                        , _fixtures         :: [Fixture]
                        , _appInfoFilter    :: String
                        , _filteredFixtures :: [Fixture]
                        , _currentScreen    :: Screen } deriving (Show)

data Screen = Menu | Fixtures deriving (Show)

makeLenses ''AppInfo

data Name = PremierField
          | ChampionsField
          deriving (Eq, Ord, Show)

mkLeagueSelection :: AppInfo -> Form AppInfo e Name
mkLeagueSelection = newForm [ radioField league [ (Premier, PremierField, "Premier League")
                                                , (Champions, ChampionsField, "Champions League")
                                                ]
                            ]

drawRows :: Columns -> [Fixture] -> Widget n
drawRows _ [] = emptyWidget
drawRows columns xs = hCenter (hBox (displaySizedGame <$> take columns xs))
  <=> drawRows columns (drop columns xs)

resizingGrid :: Width -> [Fixture] -> Widget n
resizingGrid width xs =
    Widget Fixed Fixed $ do
        ctx <- getContext
        let thing = ctx ^. availWidthL `div` width
        render $ drawRows thing xs

displaySizedGame :: Fixture -> Widget n
displaySizedGame f = padBottom (Pad 2) $ padRight (Pad 5) $ setAvailableSize (55, 5) $ displayGame f

displayGame :: Fixture -> Widget n
displayGame f =
  withBorderStyle unicode $
  borderWithLabel
    (str $ homeTeamName f ++ " vs. " ++ awayTeamName f)
    (center (str (goalsNum . goalsHomeTeam . result $ f)) <+>
     vBorder <+> center (str (goalsNum . goalsAwayTeam . result $ f)))

goalsNum :: Maybe Int -> String
goalsNum (Just x) = show x
goalsNum Nothing  = "-"

draw :: (Eq n) => Form AppInfo e n -> Widget n
draw f = case formState f of
  a@AppInfo { _currentScreen = Main.Fixtures } -> drawFixtures a
  AppInfo { _currentScreen = Menu }            -> center $ renderForm f

drawFixtures :: AppInfo -> Widget n
drawFixtures a = str (a ^. appInfoFilter) <=> resizingGrid 60 (a ^. filteredFixtures)

theMap :: AttrMap
theMap = attrMap V.defAttr [(focusedFormInputAttr, V.black `on` V.yellow)]

app :: (Eq n) => App (Form AppInfo e n) e n
app =
  App { appDraw = return . draw
      , appHandleEvent = \s ev ->
          case ev of
            VtyEvent V.EvResize {} -> continue s
            VtyEvent (V.EvKey V.KEsc []) -> halt s
            VtyEvent (V.EvKey V.KBS []) -> route Main.Fixtures $ removeFilter s
            VtyEvent (V.EvKey (V.KChar c) []) | c /= ' ' -> route Main.Fixtures $ filteredFormState c s
            VtyEvent (V.EvKey V.KEnter []) -> route Main.Fixtures =<< liftIO (fetchAndDisplayFixtures s)
            _ -> continue =<< handleFormEvent ev s
      , appChooseCursor = focusRingCursor formFocus
      , appStartEvent = return
      , appAttrMap = const theMap
      }

route :: Screen -> Form AppInfo e n -> EventM n (Next (Form AppInfo e n))
route s f = do
  let s' = formState f & currentScreen .~ s
  continue $ f { formState = s' }


fetchAndDisplayFixtures :: Form AppInfo e n -> IO (Form AppInfo e n)
fetchAndDisplayFixtures s = do
  fxt <- getFixtures $ formState s ^. league
  let s' = formState s & fixtures .~ fxt
                       & filteredFixtures .~ fxt
  return (s { formState = s' })

removeFilter :: Form AppInfo e n -> Form AppInfo e n
removeFilter s = s { formState = s' }
  where s' = formState s & filteredFixtures .~ (formState s ^. fixtures)
                         & appInfoFilter .~ ""

filteredFormState :: Char -> Form AppInfo e n -> Form AppInfo e n
filteredFormState c s = s { formState = s' }
  where filterString = (formState s ^. appInfoFilter) ++ [ c ]
        ff = (formState s ^. fixtures) & (`filterFixtures` filterString)
        s' = formState s & filteredFixtures .~ ff
                         & appInfoFilter .~ filterString
main :: IO ()
main = do
  let buildVty = do
        v <- V.mkVty =<< V.standardIOConfig
        V.setMode (V.outputIface v) V.Mouse True
        return v
      initialAppInfo = AppInfo { _league = Premier
                               , _fixtures = []
                               , _appInfoFilter = ""
                               , _filteredFixtures = []
                               , _currentScreen = Menu }
      form = mkLeagueSelection initialAppInfo
  void $ customMain buildVty Nothing app form
