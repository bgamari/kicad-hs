{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MonadFailDesugaring #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}

module Pcb
    ( NetId(..)
    , NetName(..)
    , NetClassName(..)
    , ModuleName(..)
    , LayerName(..)
    , Via(..)
    , Segment(..)
    , Module(..)
    , Node(..)
    , Pcb(..)
    , parsePcb
    , runSExprP, taggedP
      -- * Lenses
    , number, string, tag
    ) where

import Prelude hiding (fail)
import Data.Scientific
import Data.Either
import SExpr
import Control.Applicative
import Control.Monad (MonadPlus, when)
import Control.Monad.Fail
import Control.Lens

newtype NetId = NetId Int
              deriving (Show)
newtype NetName = NetName String
                deriving (Show)
newtype NetClassName = NetClassName String
                     deriving (Show)
newtype ModuleName = ModuleName String
                   deriving (Show)
newtype TStamp = TStamp String
               deriving (Show)
data LayerName = LayerName String
               deriving (Show)


data Via = Via { _viaAt :: (Scientific, Scientific)
               , _viaSize :: Scientific
               , _viaDrill :: Scientific
               , _viaLayers :: [LayerName]
               , _viaNet :: NetId
               }
         deriving (Show)
makeLenses ''Via

data Segment = Segment { _segmentStart :: (Scientific, Scientific)
                       , _segmentEnd :: (Scientific, Scientific)
                       , _segmentWidth :: Scientific
                       , _segmentLayer :: LayerName
                       , _segmentNet :: NetId
                       }
             deriving (Show)
makeLenses ''Segment

data Module = Module { _moduleName :: ModuleName
                     , _moduleLayer :: LayerName
                     , _moduleEditTime :: TStamp
                     , _moduleTStamp :: TStamp
                     , _modulePosition :: (Scientific, Scientific, Scientific)
                     , _moduleOthers :: [SExpr]
                     }
            deriving (Show)
makeLenses ''Module

data Node = Passthru SExpr
          | Layers [(Int, LayerName, String, Bool)]
          | Setup [SExpr]
          | Net NetId NetName
          | NetClass NetClassName [SExpr]
          | Module' Module
          | Via' Via
          | Segment' Segment
          deriving (Show)

data Pcb = Pcb [Node]
         deriving (Show)

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

expectOne :: (SExpr -> SExprP a) -> [SExpr] -> SExprP a
expectOne m [x] = m x
expectOne _ _   = fail "expected one"

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
                                   _       -> fail "bad at"
                         )
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
parsePcb  = taggedP "kicad_pcb" $ \xs -> Pcb <$> mapM parseNode xs

----------------------------------------------------------------------------
-- S-expression parsing infrastructure
----------------------------------------------------------------------------

newtype ParseFieldsM a = PFM ([SExpr] -> SExprP ([SExpr], a))
                    deriving (Functor)

instance Applicative ParseFieldsM where
    pure x = PFM $ \es -> pure (es, x)
    PFM f <*> PFM g = PFM $ \es -> do
        (es', x) <- f es
        (es'', y) <- g es'
        pure (es'', x y)

field :: String -> ([SExpr] -> SExprP a) -> ParseFieldsM a
field key f = PFM $ \es ->
    case partitionEithers $ map (matching (tag key)) es of
      (_, [])       -> fail $ "Key "++key++" not found"
      (rest, [es']) -> do r <- f es'
                          pure (rest, r)
      (_, _)        -> fail $ "Too many keys "++key++" found"

-- | Parse fields, returning unparsed 'SExpr's.
runParseFields' :: ParseFieldsM a -> [SExpr]
                -> SExprP ([SExpr], a) -- ^ the unparsed fields and the result
runParseFields' (PFM f) = f

-- | Parse fields, expecting all input 'SExpr's to be consumed.
runParseFields :: ParseFieldsM a -> [SExpr]
               -> SExprP a
runParseFields pfm es = do
    (es', r) <- runParseFields' pfm es
    when (not $ null es') $ fail "not all inputs parsed"
    return r


newtype SExprP a = SExprP { runSExprP :: Either String a }
                 deriving (Functor, Applicative, Monad, Alternative, MonadPlus)

instance MonadFail SExprP where
    fail = SExprP . Left

expected :: String -> SExpr -> SExprP a
expected exp found =
    fail $ "expected "++exp++"; found "++show found

taggedP :: String -> ([SExpr] -> SExprP a) -> SExpr -> SExprP a
taggedP tag f (TChild (TString _ ty:rest))
  | ty == tag = f rest
taggedP tag _ (TChild (x:_)) = expected ("tag string \""++tag++"\"") x
taggedP _ _ x = expected "Child node" x

numberP :: SExpr -> SExprP Scientific
numberP (TNum n) = pure n
numberP _ = fail "expected number"

stringP :: SExpr -> SExprP String
stringP (TString _ s) = pure s
stringP _ = fail "expected string"

tag :: String -> Prism' SExpr [SExpr]
tag t = prism' to from
  where
    to rest = TChild (TString Unquoted t : rest)
    from = either (const Nothing) Just . runSExprP . taggedP t pure

string :: Prism' SExpr String
string = prism' stringSExpr from
  where
    from (TString _ s) = Just s
    from _ = Nothing

number :: Prism' SExpr Scientific
number = prism' TNum from
  where
    from (TNum n) = Just n
    from _ = Nothing

