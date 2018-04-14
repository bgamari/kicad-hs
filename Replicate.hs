{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

import Data.List (isPrefixOf, stripPrefix)
import Data.Maybe
import Data.Monoid
import Control.Lens
import Data.Scientific
import qualified Data.Map.Strict as M
import Debug.Trace

import Pcb
import Netlist
import SExpr
import SExpr.Parse
import SExpr.Class

template :: SheetPath 'TargetSheet
template = SheetPath "/chanH/"

clones :: [(SheetPath 'TargetSheet, (Scientific, Scientific))]
clones =
    [ SheetPath "/chanA/" `offsetBy` (22, 30)
    , SheetPath "/chanB/" `offsetBy` (0,  30)
    , SheetPath "/chanC/" `offsetBy` (22, 20)
    , SheetPath "/chanD/" `offsetBy` (0,  20)
    , SheetPath "/chanE/" `offsetBy` (22, 10)
    , SheetPath "/chanF/" `offsetBy` (0,  10)
    , SheetPath "/chanG/" `offsetBy` (22,  0)
    ]
  where offsetBy path offset = (path, offset)

main :: IO ()
main = do
    Just netlist <- parseNetlistFromFile "../adc-pmod.net"
    let sheets = Netlist.sheets netlist
        Just templateTstampPath = findSheetByName netlist template
    print templateTstampPath
    --print $ Netlist.components netlist

    pcb <- either error id <$> parsePcbFromFile "../adc-pmod.kicad_pcb"
    let modulesByPath :: M.Map (TstampPath 'TargetModule) Module
        modulesByPath = M.fromList
            [ (_modulePath m, m)
            | Module' m <- _pcbNodes pcb
            ]
    --mapM_ print $ M.keys modulesByPath

    let Just templateComps = template `M.lookup` componentsBySheet netlist
        templateMods :: [Module]
        templateMods = [ mod
                       | c <- templateComps
                       , Just mod <- pure $ compTstampPath c `M.lookup` modulesByPath
                       ]
    --print templateComps

    let cloneTstamps = map (fromMaybe (error "failed to find") . findSheetByName netlist . fst) clones
    print cloneTstamps
    let removeOlds = foldMap removeSheetModules cloneTstamps

        transform :: Endo Pcb
        transform =
            foldMap (\(clonePath, (_, offset)) -> addClone netlist templateTstampPath clonePath offset templateMods)
                    (zip cloneTstamps clones)
            <> removeOlds
    writePcbToFile "new.kicad_pcb" $ appEndo transform pcb
    return ()

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
