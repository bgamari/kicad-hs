{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}

import qualified Data.ByteString.Lazy as BSL
import Kicad.Symbol
import Data.List (sortOn)
import Data.Scientific
import Linear
import Linear.Affine
import Kicad.SExpr.Class (ToSExpr(toSExpr))
import Kicad.SExpr
import Data.Csv as Csv
import Data.Foldable
import Control.Monad (mzero)
import Data.Char (ord)
import qualified Data.Map.Strict as M

main :: IO ()
main = do
    pins <- readPins "pins.csv"
    let sym = mkSymbol pins
        slib = SymbolLib 20220914 "kicad_symbol_editor" [sym]
    print $ printSExpr $ toSExpr slib

readPins :: FilePath -> IO [PinDesc]
readPins fname = do
    res <- Csv.decodeWith opts NoHeader <$> BSL.readFile fname
    res <- either fail (return . toList) res
    return [ PinDesc {..}
           | (pdName, pdNumber) <- res
           , let pdSide =
                     if | pdName == "GND" -> SBottom
                        | 'V':_ <- pdName -> STop
                        | even pdNumber   -> SLeft 
                        | otherwise       -> SRight
           , let pdType =
                     if | pdName == "GND" -> PTPowerIn
                        | 'V':_ <- pdName -> PTPowerIn
                        | otherwise       -> PTUnspecified
           , let pdHidden = pdType == PTPowerIn
           ]
  where
    opts = defaultDecodeOptions {
      decDelimiter = fromIntegral (ord '\t')
    }

data Side = STop | SBottom | SLeft | SRight
    deriving (Eq, Ord, Show)

instance FromField Side where
    parseField s
        | s == "T"  = pure STop
        | s == "B"  = pure SBottom
        | s == "L"  = pure SLeft
        | s == "R"  = pure SRight
        | otherwise = mzero

data PinDesc
    = PinDesc { pdNumber :: Int
              , pdSide :: Side
              , pdName :: String
              , pdType :: PinType
              , pdHidden :: Bool
              }

groupsBy :: Ord b => (a -> b) -> [a] -> M.Map b [a]
groupsBy f xs = M.fromListWith (++) [ (f x, [x]) | x <- xs ]

mkSymbol :: [PinDesc] -> Symbol
mkSymbol pins = Symbol 
    { _symbolName = "test"
    , _symbolPins = pins'
    , _symbolUnits = []
    }
  where
    sidePins :: Side -> [Pin]
    sidePins side =
        concat
      $ zipWith (\i -> map (mkPin (2.54 *^ size') side i)) [0..]
      $ reverse
      $ sortOn (pdNumber . head)
      $ pinsOnSide side

    pinsOnSide :: Side -> [[PinDesc]]
    pinsOnSide side = M.elems $ groupsBy pdName
        [ pd
        | pd <- pins
        , pdSide pd == side 
        ]

    width  = maximum $ map (length . pinsOnSide) [STop, SBottom]
    height = maximum $ map (length . pinsOnSide) [SLeft, SRight]

    size = V2 (3 + fromIntegral width)
              (3 + fromIntegral height)
    size' = max <$> minSize <*> size
    minSize = V2 20 15

    pins' :: [Pin]
    pins' = foldMap sidePins [ STop, SBottom, SLeft, SRight ]

mkPin :: V2 Scientific -> Side -> Int -> PinDesc -> Pin
mkPin (V2 w h) side i pd = Pin {..}
  where
    s = 2.54 :: Scientific
    te = TextEffects { _fontSize = V2 1.27 1.27 }

    _pinType = pdType pd

    _pinStyle = PSLine
    
    _pinHidden = pdHidden pd

    _pinPosition =
        case side of
          STop    -> P (V2 (fromIntegral i*s - w/2) (h/2))
          SBottom -> P (V2 (fromIntegral i*s - w/2) (-h/2))
          SLeft   -> P (V2 (w/2)  (fromIntegral i*s - h/2))
          SRight  -> P (V2 (-w/2) (fromIntegral i*s - h/2))

    _pinAngle =
        case side of
          STop -> 270
          SBottom -> 90
          SLeft -> 180
          SRight -> 0

    _pinName = (pdName pd, te)
    _pinNumber = (show (pdNumber pd), te)

    _pinLength = 3 * 1.27

