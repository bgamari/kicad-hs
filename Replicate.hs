{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Foldable (fold)
import Data.List (isPrefixOf, stripPrefix)
import Data.Maybe
import Data.Monoid
import Data.Yaml
import Control.Lens
import Data.Scientific
import Linear
import Linear.Affine
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Debug.Trace

import Pcb
import Netlist
import SExpr
import SExpr.Parse
import SExpr.Class

data Clone = Clone { cloneSheet :: SheetPath 'TargetSheet
                   , cloneOffset :: V2 Scientific
                   }

data Unit = Unit { unitTemplate :: SheetPath 'TargetSheet
                 , traceRegions :: [(Point V2 Scientific, Point V2 Scientific)]
                 , clones :: [Clone]
                 }

data Config = Config { pcbFile :: FilePath
                     , netlistFile :: FilePath
                     , units :: [Unit]
                     }

instance FromJSON (SheetPath t) where
    parseJSON = withText "sheet path" $ \t -> pure $ SheetPath $ T.unpack t

instance FromJSON a => FromJSON (Point V2 a) where
    parseJSON = fmap (\(x,y) -> P (V2 x y)) . parseJSON

instance FromJSON a => FromJSON (V2 a) where
    parseJSON = fmap (uncurry V2) . parseJSON

instance FromJSON Clone where
    parseJSON = withObject "clone" $ \o ->
      Clone <$> o .: "sheet"
            <*> o .: "offset"

instance FromJSON Unit where
    parseJSON = withObject "unit" $ \o ->
      Unit <$> o .: "template"
           <*> o .:? "trace-regions" .!= []
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
    let transform = foldMap (applyUnitComponents netlist) (units config)
                 <> foldMap applyUnitTraces (units config)
    writePcbToFile "new.kicad_pcb" $ appEndo transform pcb

applyUnitTraces :: Unit -> Endo Pcb
applyUnitTraces unit = Endo $ \pcb ->
    let templateTraces :: [Segment]
        templateTraces = [ seg
                         | Segment' seg <- _pcbNodes pcb
                         , segmentInTraceRegion seg
                         ]
        templateVias :: [Via]
        templateVias = [ via
                       | Via' via <- _pcbNodes pcb
                       , inTraceRegion (_viaAt via)
                       ]

        cloneNodes :: Clone -> [Node]
        cloneNodes clone = map Via' vias ++ map Segment' traces
          where
            offsetPoint = (.+^ cloneOffset clone)
            vias = [ via & viaAt %~ offsetPoint
                   | via <- templateVias
                   ]
            traces = [ seg & (segmentStart %~ offsetPoint)
                           . (segmentEnd   %~ offsetPoint)
                     | seg <- templateTraces
                     ]

    in Pcb $ _pcbNodes pcb ++ foldMap cloneNodes (clones unit)
  where
    inRegion (P (V2 x0 y0), P (V2 x1 y1)) (P (V2 x y)) =
        x >= x0 && x <= x1 && y >= y0 && y <= y1

    inTraceRegion :: Point V2 Scientific -> Bool
    inTraceRegion p = any (`inRegion` p) (traceRegions unit)

    segmentInTraceRegion :: Segment -> Bool
    segmentInTraceRegion seg =
        inTraceRegion (_segmentStart seg) && inTraceRegion (_segmentEnd seg)

applyUnitComponents :: Netlist -> Unit -> Endo Pcb
applyUnitComponents netlist unit =
    fold [ applyClone path clone <> removeSheetModules path
         | clone <- clones unit
         , let path = findCloneTstamp clone
         ]
  where
    templateComps :: [Component]
    templateComps = case unitTemplate unit `M.lookup` componentsBySheet netlist of
      Nothing -> error $ "failed to find template " ++ show (unitTemplate unit)
      Just x -> x

    Just templateTstampPath = findSheetByName netlist (unitTemplate unit)

    findCloneTstamp :: Clone -> TstampPath 'TargetSheet
    findCloneTstamp clone = fromMaybe uhOh $ findSheetByName netlist (cloneSheet clone)
      where uhOh = error $ "failed to find path of sheet " ++ show (cloneSheet clone)

    applyClone :: TstampPath 'TargetSheet -> Clone -> Endo Pcb
    applyClone clonePath (Clone _ offset) = Endo $ \pcb ->
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
         -> V2 Scientific             -- ^ clone offset
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

translateModule :: V2 Scientific -> Module -> Module
translateModule offset = modulePosition %~ (.+^ offset)

fp_text_reference :: Traversal' SExpr RefDesig
fp_text_reference =
    tag "fp_text" . filtered isRef . ix 1 . string . _Unwrapped'
  where
    isRef (TString _ "reference" : _) = True
    isRef _ = False
