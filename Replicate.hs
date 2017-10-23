{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

import Data.List (isPrefixOf, stripPrefix)
import Data.Maybe
import Data.Monoid
import Control.Lens
import Data.Scientific
import qualified Data.Map.Strict as M

import Pcb
import Netlist
import SExpr
import SExpr.Parse
import SExpr.Class

template :: SheetPath
template = SheetPath "/chanC/"

clones :: [(SheetPath, (Scientific, Scientific))]
clones =
    [ SheetPath "/chanA/" `offsetBy` (0, -10)
    , SheetPath "/chanB/" `offsetBy` (0, -20)
    , SheetPath "/chanD/" `offsetBy` (0, -30)
    , SheetPath "/chanE/" `offsetBy` (0, -40)
    , SheetPath "/chanF/" `offsetBy` (0, -50)
    , SheetPath "/chanG/" `offsetBy` (0, -60)
    , SheetPath "/chanH/" `offsetBy` (0, -70)
    ]
  where offsetBy path offset = (path, offset)

main :: IO ()
main = do
    Just netlist <- parseNetlistFromFile "../adc-pmod.net"
    let sheets = Netlist.sheets netlist
        Just templateTstampPath = findSheetByName netlist template
    print templateTstampPath
    --print $ Netlist.components netlist

    Right pcb <- parsePcbFromFile "../adc-pmod.kicad_pcb"
    let modulesByPath :: M.Map TstampPath [Module]
        modulesByPath = M.fromListWith (++)
            [ (_modulePath m, [m])
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
        addClone :: TstampPath -> (Scientific, Scientific) -> [Module] -> Endo Pcb
        addClone clonePath offset mods =
            foldMap addCloneModule mods
          where
            modules :: [Module]
            Just modules = clonePath `M.lookup` modulesByPath

            addCloneModule :: Module -> Endo Pcb
            addCloneModule mod = Endo $ pcbNodes %~ (++ [Module' cloneMod])
              where cloneMod = cloneModule templateTstampPath offset clonePath mod

        transform :: Endo Pcb
        transform = foldMap (\(clonePath, (_, offset)) -> addClone clonePath offset templateMods)
                            (zip cloneTstamps clones)
                    <> removeOlds
    writeFile "new.kicad_pcb" $ show $ printSExpr $ toSExpr $ appEndo transform pcb
    return ()

findSheetByName :: Netlist -> SheetPath -> Maybe TstampPath
findSheetByName netlist sheetPath =
    case filter (\s -> sheetName s == sheetPath) $ sheets netlist of
      []  -> Nothing
      [x] -> Just $ sheetTstamps x
      _   -> error "Multiple sheets by same name"

componentsBySheet :: Netlist -> M.Map SheetPath [Component]
componentsBySheet netlist =
    let comps = Netlist.components netlist
    in M.fromListWith (++) $ map (\c -> (compSheetPath c, [c])) comps

removeSheetModules :: TstampPath -> Endo Pcb
removeSheetModules sheetPath =
    Endo $ pcbNodes %~ filter onSheet
  where
    onSheet (Module' mod)
      | path <- _modulePath mod
      = not $ getTstampPath sheetPath `isPrefixOf` getTstampPath path
    onSheet _ = True

cloneModule :: TstampPath -> (Scientific, Scientific) -> TstampPath
            -> Module -> Module
cloneModule templatePath offset clonePath =
    fixupRef . fixupPath . translateModule offset
  where
    fixupRef = set (each . fp_text_reference) cloneRefDesig

    fixupPath mod
      | Just stem <- stripPrefix (getTstampPath templatePath) (getTstampPath $ _modulePath mod)
      = mod & modulePath .~ TstampPath (getTstampPath clonePath ++ stem)
      | otherwise = error "uh oh"

translateModule :: (Scientific, Scientific) -> Module -> Module
translateModule (dx,dy) = modulePosition %~ f
  where f (x,y,theta) = (dx+x, dy+y, theta)

fp_text_reference :: Traversal' SExpr RefDesig
fp_text_reference =
    tag "fp_text" . filtered isRef . ix 2 . string . _Unwrapped'
  where
    isRef (TString _ "reference" : _) = True
    isRef _ = False
