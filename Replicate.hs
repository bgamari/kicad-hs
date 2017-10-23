{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

import Pcb
import Netlist

main :: IO ()
main = do
    Just net <- parseNetlist "adc-pmod.net"
    let sheets = Netlist.sheets net
    print sheets
    print $ Netlist.components net

