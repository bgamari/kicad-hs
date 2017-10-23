{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

import SExpr
import Pcb
import Netlist

main :: IO ()
main = do
    Just net <- parseNetlist "adc-pmod.net"
    --print $ net ^. tag "export" . each . tag "components"
    let sheets = Netlist.sheets net
    print sheets
    print $ Netlist.components net

