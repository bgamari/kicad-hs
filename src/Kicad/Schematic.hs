{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Kicad.Schematic where

import Control.Applicative
import Text.Trifecta
import qualified Data.Text as T
import Linear
import Linear.Affine

data Schematic
    = Schematic { schemLibs :: T.Text
                , schemEELayer :: (Integer, Integer)
                , schemSheetSize :: T.Text
                , schemDims :: V2 Integer
                , schemDescription :: T.Text
                , schemNodes :: [Node]
                }
    deriving (Show)

data Component
    = Component { compName :: T.Text
                , compRef  :: T.Text
                , compN    :: Integer
                , compMM   :: Integer
                , compTimestamp :: T.Text
                , compPos  :: Point V2 Integer
                , compFields :: [CompField]
                }
    deriving (Show)

data FieldOrientation = Horizontal | Vertical
                      deriving (Show)

data CompField
    = CompField { cFieldNum :: Integer
                , cFieldText :: T.Text
                , cFieldOrientation :: FieldOrientation
                , cFieldPos :: Point V2 Integer
                , cFieldFlags :: Integer
                , cFieldHJustify :: Char
                , cFieldVJustify :: Char
                , cFieldItalic :: Char
                , cFieldBold :: Char
                , cFieldName :: Maybe T.Text
                }
    deriving (Show)

data HierSheet
    = HierSheet { sheetPos :: Point V2 Integer
                , sheetSize :: V2 Integer
                , sheetFields :: [SheetField]
                }
    deriving (Show)

data SheetField
    = SheetField { sFieldNum :: Integer
                 , sFieldText :: T.Text
                 , sFieldForm :: Char
                 , sFieldSize :: Char
                 , sFieldPos :: Point V2 Integer
                 , sFieldDim :: Integer
                 }
    deriving (Show)

data Node = Component' Component
          | NoConn' (Point V2 Integer)
          | HierSheet' HierSheet
          | Text' { textType :: T.Text
                  , textPos :: Point V2 Integer
                  , textOrientation :: Integer
                  , textDimension :: Integer
                  , textShape :: T.Text
                  , textText :: T.Text
                  }
          | Wire' { wireType :: T.Text
                  , wireShape :: T.Text
                  , wireStart :: Point V2 Integer
                  , wireEnd :: Point V2 Integer
                  }
          | Connection' { connPos :: Point V2 Integer }
          deriving (Show)

textP :: Parser Node
textP = named "text" $ do
    text "Text "
    textType <- unquotedString
    textPos <- P <$> parseV2
    textOrientation <- decimal <* spaces
    textDimension <- decimal <* spaces
    textShape <- unquotedString
    textUnknown <- unquotedString
    textUnknown2 <- optional unquotedString
    newline
    textText <- T.pack <$> manyTill anyChar newline
    return Text' {..}

connectionP :: Parser Node
connectionP = named "connection" $ do
    text "Connection ~ "
    connPos <- P <$> parseV2
    return Connection' {..}

wireP :: Parser Node
wireP = named "wire" $ do
    text "Wire "
    wireType <- unquotedString
    wireShape <- unquotedString
    spaces
    wireStart <- P <$> parseV2
    wireEnd <- P <$> parseV2
    return Wire' {..}

componentP :: Parser Component
componentP = named "component" $ do
    text "$Comp"
    newline
    text "L "
    compName <- T.pack <$> manyTill anyChar space
    spaces
    compRef <- T.pack <$> manyTill anyChar newline

    text "U "
    compN <- decimal <* space
    compMM <- decimal <* space
    compTimestamp <- T.pack <$> manyTill anyChar space

    text "P "
    compPos <- P <$> parseV2

    compFields <- many parseField
    char '\t' *> manyTill anyChar newline
    char '\t' *> manyTill anyChar newline
    text "$EndComp"
    optional newline
    return Component {..}
  where
    parseField = do
        text "F "
        cFieldNum <- decimal <* space
        cFieldText <- quotedString
        space
        cFieldOrientation <- (Horizontal <$ char 'H') <|> (Vertical <$ char 'V')
        space
        cFieldPos <- P <$> parseV2
        cFieldSize <- decimal
        spaces
        cFieldFlags <- decimal
        space
        cFieldHJustify <- anyChar
        space
        cFieldVJustify <- anyChar
        cFieldItalic <- anyChar
        cFieldBold <- anyChar
        cFieldName <- optional $ try $ space *> quotedString
        newline
        return CompField {..}

hierSheetP :: Parser HierSheet
hierSheetP = named "hierarchical sheet" $ do
    text "$Sheet"
    newline
    text "S "
    sheetPos <- P <$> parseV2
    sheetSize <- parseV2
    newline
    sheetFields <- manyTill sheetField (text "$EndSheet")
    return HierSheet {..}
  where
    sheetField = do
        text "F"
        sFieldNum <- decimal
        sFieldText <- quotedString
        undefined -- TODO
        return SheetField {..}


schematicP :: Parser Schematic
schematicP = named "schematic" $ do
    text "EESchema Schematic File Version 4"
    newline
    text "LIBS:"
    schemLibs <- T.pack <$> manyTill anyChar newline
    text "EELAYER "
    schemEELayer <- (,) <$> decimal <* spaces <*> decimal
    newline
    text "EELAYER END"
    newline
    text "$Descr "
    schemSheetSize <- unquotedString
    schemDims <- parseV2
    schemDescription <- T.pack <$> manyTill anyChar (text "$EndDescr")
    newline
    schemNodes <- many nodeP
    text "$EndSCHEMATIC"
    eof
    return Schematic {..}

nodeP :: Parser Node
nodeP =
    choice [ Component' <$> componentP
           , HierSheet' <$> hierSheetP
           , textP
           , wireP
           , connectionP
           ]

quotedString :: Parser T.Text
quotedString = named "quoted string" $ do
    char '"'
    T.pack <$> manyTill anyChar (char '"')

-- | Eats trailing space but not newline
unquotedString :: Parser T.Text
unquotedString = named "unquoted string" $ do
    x <- T.pack <$> many (noneOf " \n")
    optional $ char ' '
    return x

parseV2 :: Parser (V2 Integer)
parseV2 = V2 <$> integer <*> integer

named lbl = (<?> lbl)
