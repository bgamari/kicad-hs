{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

module Kicad.Symbol where

import Control.Lens
import qualified Data.Text as T
import qualified Data.Map.Strict as M
import Linear
import Linear.Affine
import Data.Scientific

import Kicad.SExpr
import Kicad.SExpr.Class

withTag :: String -> [SExpr] -> SExpr
withTag tag vals = TChild $ toSExpr tag : vals

v2WithTag :: String -> V2 Scientific -> SExpr
v2WithTag tag (V2 x y) = withTag tag [TNum x, TNum y]

data PinType
    = PTPassive
    | PTOutput
    | PTInput
    | PTTristate
    | PTBidirectional
    | PTUnspecified
    | PTFree
    | PTPowerIn
    | PTPowerOut
    | PTOpenCollector
    | PTOpenEmitter
    | PTNoConnect
    deriving (Eq, Ord, Enum, Bounded, Show)
makePrisms ''PinType

instance ToSExpr PinType where
    toSExpr pt = TString Unquoted $
        case pt of
          PTPassive       -> "passive"
          PTOutput        -> "output"
          PTInput         -> "input"
          PTTristate      -> "tri_state"
          PTBidirectional -> "bidirectional"
          PTUnspecified   -> "unspecified"
          PTFree          -> "free"
          PTPowerIn       -> "power_in"
          PTPowerOut      -> "power_out"
          PTOpenCollector -> "open_collector"
          PTOpenEmitter   -> "open_emitter"
          PTNoConnect     -> "no_connect"

data PinStyle
    = PSLine
    deriving (Eq, Ord, Enum, Bounded, Show)
makePrisms ''PinStyle

instance ToSExpr PinStyle where
    toSExpr pt = TString Unquoted $
        case pt of
          PSLine -> "line"

data TextEffects
    = TextEffects { _fontSize :: V2 Scientific
                  }
    deriving (Eq, Show)
makeLenses ''TextEffects

instance ToSExpr TextEffects where
    toSExpr (TextEffects {..}) = withTag "effects" 
        [ withTag "font"
          [ v2WithTag "size" _fontSize
          ]
        ]

data Pin
    = Pin { _pinType :: PinType
          , _pinStyle :: PinStyle
          , _pinPosition :: Point V2 Scientific
          , _pinHidden :: Bool
          , _pinAngle :: Scientific
          , _pinLength :: Scientific
          , _pinName :: (String, TextEffects)
          , _pinNumber :: (String, TextEffects)
          }
    deriving (Eq, Show)

instance ToSExpr Pin where
    toSExpr (Pin {..}) = withTag "pin" $
        [ toSExpr _pinType
        , toSExpr _pinStyle
        , let P (V2 x y) = _pinPosition
          in withTag "at" [TNum x, TNum y, TNum _pinAngle]
        , withTag "length" [TNum _pinLength]
        , withTag "name"
          [ TString Quoted (fst _pinName)
          , toSExpr (snd _pinName)
          ]
        , withTag "number"
          [ TString Quoted (fst _pinNumber)
          , toSExpr (snd _pinNumber)
          ]
        ] ++ [ TString Unquoted "hide" | _pinHidden ]

data Symbol
    = Symbol { _symbolName :: String
             , _symbolPins :: [Pin]
             , _symbolUnits:: [Symbol]
             }
    deriving (Eq, Show)
makeLenses ''Symbol

instance ToSExpr Symbol where
    toSExpr (Symbol {..}) = withTag "symbol" $
        [ TString Quoted _symbolName
        ] ++ map toSExpr _symbolPins
          ++ map toSExpr _symbolUnits

data SymbolLib
    = SymbolLib { _symlibVersion :: Scientific
                , _symlibGenerator :: String
                , _symlibSymbols :: [Symbol]
                }
    deriving (Eq, Show)
makeLenses ''SymbolLib

instance ToSExpr SymbolLib where
    toSExpr (SymbolLib {..}) = withTag "kicad_symbol_lib" $
        [ withTag "version" [ TNum _symlibVersion ]
        , withTag "generator" [ TString Quoted _symlibGenerator ]
        ] ++ map toSExpr _symlibSymbols

