{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Netlist where

import Control.Lens
import Control.Monad.Trans.Maybe
import Data.Scientific
import SExpr
import Pcb
import Text.Trifecta (parseFromFile)
import Data.Maybe

newtype SheetPath = SheetPath String
                  deriving (Show)

newtype TstampPath = TstampPath String
                  deriving (Show)

data Sheet = Sheet { sheetNum :: Int
                   , sheetName :: SheetPath
                   , sheetTstamps :: String
                   }
           deriving (Show)

sheets :: Netlist -> [Sheet]
sheets (Netlist netlist) =
    netlist ^. each . tag "design" . each . tag "sheet" . to toSheet
  where
    toSheet :: [SExpr] -> [Sheet]
    toSheet e = fromMaybe [] $ do
        sheetNum <- e ^? each . tag "number" . _head . number . to floatingOrInteger . _Right
        sheetName <- e ^? each . tag "name" . _head . string . to SheetPath
        sheetTstamps <- e ^? each . tag "tstamps" . _head . string
        return [Sheet {..}]

newtype RefDesig = RefDesig String
                 deriving (Show)

components :: Netlist -> [(RefDesig, SheetPath)]
components (Netlist netlist) =
    netlist ^. each . tag "components" . each . tag "comp" . to f
  where
    f :: [SExpr] -> [(RefDesig, SheetPath)]
    f e = fromMaybe [] $ do
        ref <- e ^? each . tag "ref" . _head . string . to RefDesig
        sheetPath <- e ^? each . tag "sheetpath" . each . tag "names" . _head . string . to SheetPath
        return [(ref, sheetPath)]


newtype Netlist = Netlist [SExpr]

parseNetlist :: FilePath -> IO (Maybe Netlist)
parseNetlist path = runMaybeT $ do
    sexpr <- MaybeT $ parseFromFile parseSExpr path
    Netlist <$> MaybeT (pure $ sexpr ^? tag "export")
