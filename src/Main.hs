module Main where

import Control.Monad (void)
import Data.Default (def)
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import Reactive.Threepenny ()

import Calc

main :: IO ()
main = startGUI defaultConfig setup

setup :: Window -> UI ()
setup win = void $ do
  return win # set title "ReaCalc"

  out <- UI.input # set (attr "readonly") "true"
                  # set (attr "style") "text-align: right; min-width: 240px"
                  # set UI.value "0"

  buttons <- mapM (mapM mkButton) buttonLabels

  let clicks = buttonClicks (zip (concat buttons) (concat buttonLabels))
      commands = fmap populate clicks
  calcBehaviour <- accumB (def :: State) commands
  return out # sink value (fmap display calcBehaviour)

  getBody win #+ [return out]
              #+ map UI.row (map (map return) buttons)

  where
    mkButton s = UI.input # set text s
                          # set value s
                          # set (attr "type") "button"
                          # set (attr "style") "min-width: 50px"

    buttonClicks :: [(Element, String)] -> Event String
    buttonClicks = foldr1 (unionWith (++)) . map makeClick
      where
        makeClick :: (Element, String) -> Event String
        makeClick (e, s) = (pure s) <@ (UI.click e)


buttonLabels :: [[String]]
buttonLabels = map words $ lines "7 8 9 CE C\n4 5 6 + -\n1 2 3 * /\n . 0 ="
