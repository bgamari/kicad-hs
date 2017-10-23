{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}

module Netlist where

import Control.Lens
import Control.Monad.Trans.Maybe
import Data.Scientific
import Text.Trifecta (parseFromFile)
import Data.Maybe

import SExpr
import SExpr.Parse
import Pcb

data Sheet = Sheet { sheetNum     :: Int
                   , sheetName    :: SheetPath 'TargetSheet
                   , sheetTstamps :: TstampPath 'TargetSheet
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
        sheetTstamps <- e ^? each . tag "tstamps" . _head . string . to TstampPath
        return [Sheet {..}]

data Component = Component { compRef        :: RefDesig
                           , compSheetPath  :: SheetPath 'TargetSheet
                           , compTstampSheetPath :: TstampPath 'TargetSheet
                           , compTstampPath :: TstampPath 'TargetModule
                           }
               deriving (Show)

components :: Netlist -> [Component]
components (Netlist netlist) =
    netlist ^. each . tag "components" . each . tag "comp" . to f
  where
    f :: [SExpr] -> [Component]
    f e = fromMaybe [] $ do
        ref <- e ^? each . tag "ref" . _head . string . to RefDesig
        sheetPath <- e ^? each . tag "sheetpath" . each . tag "names" . _head . string . to SheetPath
        tstampSheetPath <- e ^? each . tag "sheetpath" . each . tag "tstamps" . _head . string . to TstampPath
        tstamp <- e ^? each . tag "tstamp" . _head . string
        let tstampPath = TstampPath $ getTstampPath tstampSheetPath ++ tstamp
        return [Component ref sheetPath tstampSheetPath tstampPath]


newtype Netlist = Netlist [SExpr]

parseNetlistFromFile :: FilePath -> IO (Maybe Netlist)
parseNetlistFromFile path = runMaybeT $ do
    sexpr <- MaybeT $ parseSExprFromFile path
    Netlist <$> MaybeT (pure $ sexpr ^? tag "export")
