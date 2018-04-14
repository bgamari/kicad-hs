{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.List (isPrefixOf, stripPrefix)
import Data.Maybe
import Data.Monoid
import Data.Yaml
import Control.Lens
import Data.Scientific
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Debug.Trace

import Pcb
import Netlist
import SExpr
import SExpr.Parse
import SExpr.Class

data Clone = Clone { cloneSheet :: SheetPath 'TargetSheet
                   , cloneOffset :: (Scientific, Scientific)
                   }

data Unit = Unit { unitTemplate :: SheetPath 'TargetSheet
                 , clones :: [Clone]
                 }

data Config = Config { pcbFile :: FilePath
                     , netlistFile :: FilePath
                     , units :: [Unit]
                     }

instance FromJSON (SheetPath t) where
    parseJSON = withText "sheet path" $ \t -> pure $ SheetPath $ T.unpack t

instance FromJSON Clone where
    parseJSON = withObject "clone" $ \o ->
      Clone <$> o .: "sheet"
            <*> o .: "offset"

instance FromJSON Unit where
    parseJSON = withObject "unit" $ \o ->
      Unit <$> o .: "template"
           <*> o .: "clones"

instance FromJSON Config where
    parseJSON = withObject "configuration" $ \o ->
      Config <$> o .: "pcb-file"
             <*> o .: "netlist-file"
             <*> o .: "units"

main :: IO ()
main = do
    config <- either (fail . show) pure =<< Data.Yaml.decodeFileEither "config.yaml"
    Just netlist <- parseNetlistFromFile $ netlistFile config
    pcb <- either error id <$> parsePcbFromFile (pcbFile config)
    let transform = foldMap (applyUnit netlist) (units config)
    writePcbToFile "new.kicad_pcb" $ appEndo transform pcb

applyUnit :: Netlist -> Unit -> Endo Pcb
applyUnit netlist unit =
    foldMap doClone (zip cloneTstamps (clones unit))
    <> removeOlds
  where
    Just templateTstampPath = findSheetByName netlist (unitTemplate unit)
    Just templateComps = unitTemplate unit `M.lookup` componentsBySheet netlist
    cloneTstamps :: [TstampPath 'TargetSheet]
    cloneTstamps = map findClone (clones unit)
      where
        findClone :: Clone -> TstampPath 'TargetSheet
        findClone clone = fromMaybe uhOh $ findSheetByName netlist (cloneSheet clone)
          where uhOh = error $ "failed to find path of sheet " ++ show (cloneSheet clone)

    doClone (clonePath, Clone _ offset) = Endo $ \pcb ->
        let templateMods :: [Module]
            templateMods = [ mod
                           | c <- templateComps
                           , Just mod <- pure $ compTstampPath c `M.lookup` modulesByPath
                           ]
            modulesByPath :: M.Map (TstampPath 'TargetModule) Module
            modulesByPath = M.fromList
                [ (_modulePath m, m)
                | Module' m <- _pcbNodes pcb
                ]
        in appEndo (addClone netlist templateTstampPath clonePath offset templateMods) pcb

    removeOlds = foldMap removeSheetModules cloneTstamps


findSheetByName :: Netlist -> SheetPath 'TargetSheet -> Maybe (TstampPath 'TargetSheet)
findSheetByName netlist sheetPath =
    case filter (\s -> sheetName s == sheetPath) $ sheets netlist of
      []  -> Nothing
      [x] -> Just $ sheetTstamps x
      _   -> error "Multiple sheets by same name"

componentsBySheet :: Netlist -> M.Map (SheetPath 'TargetSheet) [Component]
componentsBySheet netlist =
    let comps = Netlist.components netlist
    in M.fromListWith (++) $ map (\c -> (compSheetPath c, [c])) comps

removeSheetModules :: TstampPath 'TargetSheet-> Endo Pcb
removeSheetModules sheetPath =
    Endo $ pcbNodes %~ filter onSheet
  where
    onSheet (Module' mod)
      | path <- _modulePath mod
      = not $ getTstampPath sheetPath `isPrefixOf` getTstampPath path
    onSheet _ = True

addClone :: Netlist
         -> TstampPath 'TargetSheet   -- ^ template path
         -> TstampPath 'TargetSheet   -- ^ clone path
         -> (Scientific, Scientific)  -- ^ clone offset
         -> [Module]                  -- ^ template modules
         -> Endo Pcb
addClone netlist templateTstampPath clonePath offset mods =
    traceShow sheetComponents $ foldMap addCloneModule mods
  where
    sheetComponents :: M.Map (TstampPath 'TargetModule) RefDesig
    sheetComponents =
        M.fromList
        [ (compTstampPath comp, compRef comp)
        | comp <- components netlist
        , compTstampSheetPath comp == clonePath
        ]

    addCloneModule :: Module -> Endo Pcb
    addCloneModule mod =
        Endo $ pcbNodes %~ (++ [Module' $ fixupRef cloneMod])
      where
        cloneMod =
              dropPadNets
            . fixupRef
            . (modulePath .~ path')
            . translateModule offset
            $ mod

        path' :: TstampPath 'TargetModule
        path'
          | Just stem <- stripPrefix (getTstampPath templateTstampPath) (getTstampPath $ _modulePath mod)
          = TstampPath (getTstampPath clonePath ++ stem)
          | otherwise = error "uh oh"

        cloneRefDesig =
            case path' `M.lookup` sheetComponents of
              Nothing -> error $ "Failed to find component for "++views modulePath show mod
              Just x -> x
        fixupRef = set (moduleOthers . each . fp_text_reference) cloneRefDesig

dropPadNets :: Module -> Module
dropPadNets =
    moduleOthers . each . tag "pad" %~ filter (isn't $ tag "net")

translateModule :: (Scientific, Scientific) -> Module -> Module
translateModule (dx,dy) = modulePosition %~ f
  where f (x,y,theta) = (dx+x, dy+y, theta)

fp_text_reference :: Traversal' SExpr RefDesig
fp_text_reference =
    tag "fp_text" . filtered isRef . ix 1 . string . _Unwrapped'
  where
    isRef (TString _ "reference" : _) = True
    isRef _ = False
