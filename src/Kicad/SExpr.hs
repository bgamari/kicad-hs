{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Kicad.SExpr where

import Data.Maybe
import qualified Data.CharSet as CS
import qualified Data.CharSet.Unicode as CSU
import qualified Data.CharSet.Posix.Ascii as CS
import Data.Scientific
import Text.Trifecta
import Data.Semigroup
import qualified Data.Text.Prettyprint.Doc as PP
import Control.Applicative
import Control.Lens
import Numeric

data IsQuoted = Quoted | Unquoted
              deriving (Show)

data SExpr = TString IsQuoted String
           | TNum Scientific
           | THexInt Integer
           | TChild [SExpr]
           deriving (Show)
makePrisms ''SExpr

parseSExpr :: Parser SExpr
parseSExpr =
    choice
    [ hexNum <?> "hexadecimal number"
    , num  <?> "nubmer"
    , child <?> "child S-expression"
    , quotedString <?> "quoted string"
    , unquotedString <?> "unquoted string"
    ]
  where
    hexNum = try $ do
        text "0x"
        digits <- some $ oneOfSet $ CS.xdigit <> CS.singleton '_'
        (n,""):_ <- pure $ reads $ "0x" ++ filter (/= '_') digits
        pure $ THexInt n
    num = try $ token $ do
        let parseNum = do
                digits <- some digit
                f <- fmap (fromMaybe id) $ optional $ do
                    char '.'
                    frac <- some digit
                    return $ (++ '.':frac)
                return $ read $ f digits
        r <- choice
            [ try $ do char '-'
                       TNum . negate <$> parseNum
            , TNum <$> parseNum
            ]
        notFollowedBy $ oneOfSet unquotedChars
        return r
    quotedString = TString Quoted <$> stringLiteral
    unquotedString = token $ do
        TString Unquoted <$> some (oneOfSet unquotedChars)
    child = TChild <$> parens (many parseSExpr)

unquotedChars :: CS.CharSet
unquotedChars = CSU.letter <> CSU.number <> CS.fromList ".,#_/+-~:%*${}?&="

stringSExpr :: String -> SExpr
stringSExpr s = TString quote s
  where
    quote
      | all (`CS.member` unquotedChars) s = Unquoted
      | otherwise = Quoted

escapeString :: String -> String
escapeString = foldMap f
  where
    f '\n' = "\\n"
    f c    = [c]

printSExpr :: SExpr -> PP.Doc ()
printSExpr (TString _ "") = PP.dquotes mempty
printSExpr (TString Quoted s) = PP.dquotes $ PP.pretty $ escapeString s
printSExpr (TString Unquoted s) = PP.pretty s
printSExpr (TNum n)
  | Right int <- floatingOrInteger n
  = PP.pretty (int :: Integer)
  | otherwise
  = PP.pretty (formatScientific Fixed Nothing n)
printSExpr (THexInt n) = "0x" <> PP.pretty (showHex n "")
printSExpr (TChild xs) = PP.parens $ PP.hang 1 $ PP.sep $ map printSExpr xs

parseSExprFromFile :: FilePath -> IO (Maybe SExpr)
parseSExprFromFile path = parseFromFile parseSExpr path
