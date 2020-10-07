module Main where

import           Control.Monad               (void)
import           Data.Default                (def)
import qualified Graphics.UI.Threepenny      as UI
import           Graphics.UI.Threepenny      (set, attr, title, text, value,
                                              (#), (#+), (<@),
                                              Event, Element, UI, Window,
                                              sink, accumB)
import           Graphics.UI.Threepenny.Core (startGUI, defaultConfig)

import           Calc

main :: IO ()
main = startGUI defaultConfig setup

setup :: Window -> UI ()
setup win = void $ do
  void $ pure win
    # set title "ReaCalc"

  out <- UI.input
    # set (attr "readonly") "true"
    # set (attr "style") "text-align: right; min-width: 240px"
    # set value "0"

  buttons <- mapM (mapM mkButton) buttonLabels

  let clicks = buttonClicks (zip (concat buttons) (concat buttonLabels))
  let commands = fmap populate clicks
  calcBehaviour <- accumB (def :: State) commands
  void $ pure out # sink value (fmap display calcBehaviour)

  UI.getBody win
    #+ [return out]
    #+ map UI.row (map (map return) buttons)

  where
    mkButton s = UI.input
      # set text s
      # set value s
      # set (attr "type") "button"
      # set (attr "style") "min-width: 50px"

    buttonClicks :: [(Element, String)] -> Event String
    buttonClicks = foldr1 (UI.unionWith const) . map makeClick
      where
        makeClick (e, s) = (UI.pure s) <@ (UI.click e)


buttonLabels :: [[String]]
buttonLabels = map words $ lines "7 8 9 CE C\n4 5 6 + -\n1 2 3 * /\n . 0 ="
