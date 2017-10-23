{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MonadFailDesugaring #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}

module Pcb
    ( -- * Basic types
      NetId(..)
    , NetName(..)
    , NetClassName(..)
    , ModuleName(..)
    , LayerName(..)
    , SheetPath(..)
    , TstampPath(..)
    , RefDesig(..)
      -- * Nodes
    , Node(..)
      -- ** Vias
    , Via(..)
      -- ** Segments
    , Segment(..)
      -- ** Modules
    , Module(..), moduleName, modulePosition, modulePath
      -- * PCB
    , Pcb(..), pcbNodes
    , parsePcb
    , parsePcbFromFile
    ) where

import Prelude hiding (fail)
import Data.Scientific
import Data.String (IsString)
import Control.Applicative
import Control.Monad.Fail
import Control.Lens

import SExpr
import SExpr.Class
import SExpr.Parse

newtype NetId = NetId Int
              deriving (Show, Eq, Ord, ToSExpr)

newtype NetName = NetName String
                deriving (Show, Eq, Ord, ToSExpr)

newtype NetClassName = NetClassName String
                     deriving (Show, Eq, Ord, ToSExpr)

newtype ModuleName = ModuleName String
                   deriving (Show, Eq, Ord, ToSExpr)

newtype TStamp = TStamp String
               deriving (Show, Eq, Ord, ToSExpr)

newtype SheetPath = SheetPath {getSheetPath :: String}
                  deriving (Show, Eq, Ord, ToSExpr, IsString)
makeWrapped ''SheetPath

newtype TstampPath = TstampPath {getTstampPath :: String}
                  deriving (Show, Eq, Ord, ToSExpr)

newtype LayerName = LayerName String
                  deriving (Show, Eq, Ord, ToSExpr)

newtype RefDesig = RefDesig String
                 deriving (Show)
makeWrapped ''RefDesig


data Via = Via { _viaAt     :: (Scientific, Scientific)
               , _viaSize   :: Scientific
               , _viaDrill  :: Scientific
               , _viaLayers :: [LayerName]
               , _viaNet    :: NetId
               }
         deriving (Show)
makeLenses ''Via

data Segment = Segment { _segmentStart :: (Scientific, Scientific)
                       , _segmentEnd   :: (Scientific, Scientific)
                       , _segmentWidth :: Scientific
                       , _segmentLayer :: LayerName
                       , _segmentNet   :: NetId
                       }
             deriving (Show)
makeLenses ''Segment

data Module = Module { _moduleName     :: ModuleName
                     , _moduleLayer    :: LayerName
                     , _moduleEditTime :: TStamp
                     , _moduleTStamp   :: TStamp
                     , _modulePosition :: (Scientific, Scientific, Scientific)
                     , _modulePath     :: TstampPath
                     , _moduleOthers   :: [SExpr]
                     }
            deriving (Show)
makeLenses ''Module

data Node = Passthru SExpr
          | Layers [(Int, LayerName, String, Bool)]
          | Net NetId NetName
          | NetClass NetClassName [SExpr]
          | Module' Module
          | Via' Via
          | Segment' Segment
          deriving (Show)

data Pcb = Pcb { _pcbNodes :: [Node] }
         deriving (Show)
makeLenses ''Pcb

instance ToSExpr Pcb where
    toSExpr (Pcb nodes) = withTag "kicad_pcb" (map toSExpr nodes)

instance ToSExpr Node where
    toSExpr (Passthru x) = x
    toSExpr (Layers layers) = withTag "layers" $ map f layers
      where
        f (layerN, LayerName layerName, layerType, hidden) =
            TChild $ [ toSExpr layerN
                     , toSExpr layerName
                     , toSExpr layerType
                    ] ++ (if hidden then [stringSExpr "hide"] else [])
    toSExpr (Net netId netName) =
        withTag "net" [toSExpr netId, toSExpr netName]
    toSExpr (NetClass className rest) =
        withTag "net_class" $ toSExpr className : rest
    toSExpr (Module' mod) =
        withTag "module"
        $ [ toSExpr $ _moduleName mod
          , withTag "layer"  [toSExpr $ _moduleLayer mod]
          , withTag "tedit"  [toSExpr $ _moduleEditTime mod]
          , withTag "tstamp" [toSExpr $ _moduleTStamp mod]
          , withTag "at"     [toSExpr x, toSExpr y, toSExpr theta]
          , withTag "path"   [toSExpr $ _modulePath mod]
          ] ++ _moduleOthers mod
      where (x,y,theta) = _modulePosition mod
    toSExpr (Via' via) =
        withTag "via"
        $ [ withTag "at"     [toSExpr x, toSExpr y]
          , withTag "size"   [toSExpr $ _viaSize via]
          , withTag "drill"  [toSExpr $ _viaDrill via]
          , withTag "layers" $ map toSExpr $ _viaLayers via
          , withTag "net"    [toSExpr $ _viaNet via]
          ]
      where (x,y) = _viaAt via
    toSExpr (Segment' seg) =
        withTag "segment"
        $ [ withTag "start" [toSExpr sx, toSExpr sy]
          , withTag "end"   [toSExpr ex, toSExpr ey]
          , withTag "width" [toSExpr $ _segmentWidth seg]
          , withTag "layer" [toSExpr $ _segmentLayer seg]
          , withTag "net"   [toSExpr $ _segmentNet   seg]
          ]
      where (sx,sy) = _segmentStart seg
            (ex,ey) = _segmentEnd seg

withTag :: String -> [SExpr] -> SExpr
withTag tag vals = TChild $ toSExpr tag : vals

choice = foldr1 (<|>)

parseNode :: SExpr -> SExprP Node
parseNode e =
    choice $ map ($ e)
    [ taggedP "net_class" $ \rest -> do
          name:rest' <- pure rest
          name' <- NetClassName <$> stringP name
          pure $ NetClass name' rest'
    , taggedP "net" $ \rest -> do
          case rest of
            [netId, name] -> Net <$> parseNetId netId <*> fmap NetName (stringP name)
            _ -> fail "bad net"
    , fmap (fmap Module') parseModule
    , fmap (fmap Via') parseVia
    , fmap (fmap Segment') parseSegment
    , known "zone"
    , known "version"
    , known "dimension"
    , known "host"
    , known "general"
    , known "page"
    , known "layers"
    , known "setup"
    ]
  where
    known tag x@(TChild (TString _ ty : _))
      | ty == tag = pure $ Passthru x
    known tag x = expected ("tag "++tag) x

parseVia :: SExpr -> SExprP Via
parseVia = taggedP "via" $ runParseFields $
    Via <$> field "at" (n2 id)
        <*> field "size" (n1 id)
        <*> field "drill" (n1 id)
        <*> field "layers" (mapM parseLayerName)
        <*> field "net" (expectOne parseNetId)

parseSegment :: SExpr -> SExprP Segment
parseSegment = taggedP "segment" $ runParseFields $
    Segment
      <$> field "start" (n2 id)
      <*> field "end" (n2 id)
      <*> field "width" (n1 id)
      <*> field "layer" (expectOne parseLayerName)
      <*> field "net" (expectOne parseNetId)

parseModule :: SExpr -> SExprP Module
parseModule = taggedP "module" $ \rest -> do
    name:rest' <- pure rest
    moduleName <- ModuleName <$> stringP name
    (rest'', mod) <- flip runParseFields' rest' $
        Module moduleName
          <$> field "layer" (expectOne $ fmap LayerName . stringP)
          <*> field "tedit" (expectOne $ fmap TStamp . stringP)
          <*> field "tstamp" (expectOne $ fmap TStamp . stringP)
          <*> field "at" (\xs -> case xs of
                                   [a,b]   -> (,,) <$> numberP a <*> numberP b <*> pure 0
                                   [a,b,c] -> (,,) <$> numberP a <*> numberP b <*> numberP c
                                   _       -> fail "bad at")
          <*> field "path" (expectOne $ fmap TstampPath . stringP)

    return $ mod rest''

n3 :: ((Scientific,Scientific,Scientific) -> a) -> [SExpr] -> SExprP a
n3 f [a,b,c] = do
    na <- numberP a
    nb <- numberP b
    nc <- numberP c
    return $ f (na,nb,nc)
n3 _ _ = fail "n3"

n2 :: ((Scientific,Scientific) -> a) -> [SExpr] -> SExprP a
n2 f [a,b] = do
    na <- numberP a
    nb <- numberP b
    return $ f (na,nb)
n2 _ _ = fail "n2"

n1 :: (Scientific -> a) -> [SExpr] -> SExprP a
n1 f [a] = do
    na <- numberP a
    return $ f na
n1 _ _ = fail "n1"

parseLayerName :: SExpr -> SExprP LayerName
parseLayerName e = LayerName <$> stringP e

parseNetId :: SExpr -> SExprP NetId
parseNetId e = do
    n <- numberP e
    case floatingOrInteger n of
      Left _ -> fail "parseNetId: expected integer"
      Right i -> pure $ NetId i

parsePcb :: SExpr -> SExprP Pcb
parsePcb = taggedP "kicad_pcb" $ \xs -> Pcb <$> mapM parseNode xs

parsePcbFromFile :: FilePath -> IO (Either String Pcb)
parsePcbFromFile path = do
    Just sexpr <- parseSExprFromFile path
    return $ runSExprP $ parsePcb sexpr
