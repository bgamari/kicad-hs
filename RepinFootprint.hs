{-# LANGUAGE RankNTypes #-}
import Data.Foldable (fold)
import Data.List (isPrefixOf, stripPrefix)
import Data.Maybe
import Data.Monoid
import Control.Lens
import Data.Scientific

import Kicad.SExpr
import Kicad.SExpr.Parse

main :: IO ()
main = do
    Just sexpr <- parseSExprFromFile "hi"
    let nPins = 140
    let sexpr' = over (tagged "footprint" . traverse . tagged "pad") (renumber nPins) sexpr
    print $ printSExpr sexpr'

renumber' :: Int -> Int -> Int
renumber' nPins n = 1 + col + 2*row
  where
    nPins2 = nPins `div` 2
    col = (n-1) `div` nPins2
    row
      | n-1 < nPins2 = n - 1
      | otherwise    = nPins - n

renumber :: Int -> [SExpr] -> [SExpr]
renumber nPins (TString q s : xs)
  | (n, ""):_ <- reads s =
      TString q (show $ renumber' nPins n) : xs
renumber _ xs = xs

tagged :: String -> Prism' SExpr [SExpr]
tagged tag = prism' to from
  where
    to xs = TChild $ TString Unquoted tag : xs
    from (TChild (TString Unquoted tag' : xs))
      | tag == tag' = Just xs
    from _ = Nothing
