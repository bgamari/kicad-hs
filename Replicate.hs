{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

import Pcb
import Netlist
import SExpr
import SExpr.Class

import qualified Data.Map.Strict as M

template = SheetPath "/chanC/"

main :: IO ()
main = do
    Just netlist <- parseNetlistFromFile "../adc-pmod.net"
    let sheets = Netlist.sheets netlist
    --print $ findSheetByName netlist template
    --print $ Netlist.components netlist

    Right pcb <- parsePcbFromFile "../adc-pmod.kicad_pcb"
    let modulesByPath :: M.Map TstampPath Module
        modulesByPath = M.fromList [ (_modulePath m, m)
                                   | Module' m <- pcbNodes pcb
                                   ]
    --mapM_ print $ M.keys modulesByPath

    let Just templateComps = template `M.lookup` componentsBySheet netlist
    --print templateComps
    print [ mod
          | c <- templateComps
          , Just mod <- pure $ compTstampPath c `M.lookup` modulesByPath
          ]

    writeFile "new.kicad_pcb" $ show $ printSExpr $ toSExpr pcb
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

--findComponentModules :: Pcb -> [Component] -> 
