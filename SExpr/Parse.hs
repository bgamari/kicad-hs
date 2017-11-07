{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}

module SExpr.Parse
    ( -- * Parsing expressions
      SExprP
    , runSExprP
    , numberP
    , taggedP
    , stringP
    , expectOne
    , expected
      -- * Parsing fields
    , ParseFieldsM
    , field
    , optionalField
    , remainingFields
    , runParseFields'
    , runParseFields
      -- * Lenses
    , tag
    , string
    , number
    )
where

import Control.Monad.Fail
import Data.Scientific
import Control.Lens
import Control.Monad (MonadPlus, unless)
import Control.Applicative
import Data.Either
import Prelude hiding (fail)

import SExpr

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

optionalField :: String -> ([SExpr] -> SExprP a) -> ParseFieldsM (Maybe a)
optionalField key f = PFM $ \es ->
    case partitionEithers $ map (matching (tag key)) es of
      (_, [])       -> pure (es, Nothing)
      (rest, [es']) -> do r <- f es'
                          pure (rest, Just r)
      (_, _)        -> fail $ "Too many keys "++key++" found"

remainingFields :: ParseFieldsM [SExpr]
remainingFields = PFM $ \es -> pure ([], es)

-- | Parse fields, returning unparsed 'SExpr's.
runParseFields' :: ParseFieldsM a -> [SExpr]
                -> SExprP ([SExpr], a) -- ^ the unparsed fields and the result
runParseFields' (PFM f) = f

-- | Parse fields, expecting all input 'SExpr's to be consumed.
runParseFields :: ParseFieldsM a -> [SExpr]
               -> SExprP a
runParseFields pfm es = do
    (es', r) <- runParseFields' pfm es
    unless (null es') $ fail "not all inputs parsed"
    return r


newtype SExprP a = SExprP { runSExprP :: Either String a }
                 deriving (Functor, Applicative, Monad, Alternative, MonadPlus)

instance MonadFail SExprP where
    fail = SExprP . Left

expected :: String -> SExpr -> SExprP a
expected exp found =
    fail $ "expected "++exp++"; found "++show found

expectOne :: (SExpr -> SExprP a) -> [SExpr] -> SExprP a
expectOne m [x] = m x
expectOne _ _   = fail "expected one"

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
