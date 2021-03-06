{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Kicad.SExpr.Class where

import Data.Scientific
import Kicad.SExpr

class ToSExpr a where
    toSExpr :: a -> SExpr

instance ToSExpr String where
    toSExpr = stringSExpr

instance ToSExpr Int where
    toSExpr = TNum . fromIntegral

instance ToSExpr Integer where
    toSExpr = TNum . fromIntegral

instance ToSExpr Double where
    toSExpr = TNum . realToFrac

instance ToSExpr Scientific where
    toSExpr = TNum
