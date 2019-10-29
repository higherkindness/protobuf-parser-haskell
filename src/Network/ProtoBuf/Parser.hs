{-# language FlexibleContexts, OverloadedStrings #-}
module Network.ProtoBuf.Parser (
  parseProtoBufFile
, parseProtoBuf
, parseProtoBufDeclarations
) where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Network.ProtoBuf.Types

parseProtoBufFile :: FilePath -> IO (Either (ParseErrorBundle T.Text Char) ProtoBuf)
parseProtoBufFile p = parseProtoBuf <$> T.readFile p
parseProtoBuf :: T.Text -> Either (ParseErrorBundle T.Text Char) ProtoBuf
parseProtoBuf = parse wholeProtoBuf ""
parseProtoBufDeclarations :: T.Text -> Either (ParseErrorBundle T.Text Char) [Declaration]
parseProtoBufDeclarations = parse (many declaration) ""

spaceConsumer :: MonadParsec Char T.Text m => m ()
spaceConsumer = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

lexeme :: MonadParsec Char T.Text m => m a -> m a
lexeme  = L.lexeme spaceConsumer
symbol :: MonadParsec Char T.Text m => T.Text -> m T.Text
symbol = L.symbol spaceConsumer
reserved :: MonadParsec Char T.Text m => T.Text -> m T.Text
reserved = lexeme . chunk

number :: (MonadParsec Char T.Text m, Integral a) => m a 
number =   L.signed spaceConsumer (lexeme L.decimal)
       <|> lexeme L.octal
       <|> lexeme L.hexadecimal
floating :: (MonadParsec Char T.Text m, RealFloat a) => m a
floating = L.signed spaceConsumer (lexeme L.float)
stringLiteral :: MonadParsec Char T.Text m => m T.Text
stringLiteral = T.pack <$> (char '"' >> manyTill L.charLiteral (char '"'))

betweenBraces :: MonadParsec Char T.Text m => m a -> m a
betweenBraces = between (symbol "{") (symbol "}")
betweenSquares :: MonadParsec Char T.Text m => m a -> m a
betweenSquares = between (symbol "[") (symbol "]")
betweenParens :: MonadParsec Char T.Text m => m a -> m a
betweenParens = between (symbol "(") (symbol ")")

ident :: MonadParsec Char T.Text m => m T.Text
ident = (\h t -> T.pack (h:t))
        <$> letterChar
        <*> many (alphaNumChar <|> char '_')
identifier :: MonadParsec Char T.Text m => m T.Text
identifier = lexeme ident
fullIdentifier :: MonadParsec Char T.Text m => m FullIdentifier
fullIdentifier = lexeme $ sepBy1 identifier (char '.')

constant :: MonadParsec Char T.Text m => m Constant
constant 
  =   KFloat  <$> floating
  <|> KInt    <$> number
  <|> KString <$> stringLiteral
  <|> KBool True  <$ reserved "true"
  <|> KBool False <$ reserved "false"
  <|> KIdentifier <$> fullIdentifier

optionName :: MonadParsec Char T.Text m => m FullIdentifier
optionName = (++) <$> ((: []) <$> identifier <|> betweenParens fullIdentifier)
                  <*> many (char '.' >> identifier)

topOption :: MonadParsec Char T.Text m => m Option
topOption
  = Option <$  reserved "option"
           <*> optionName
           <*  symbol "="
           <*> constant
           <*  symbol ";"
innerOption :: MonadParsec Char T.Text m => m Option
innerOption
  = Option <$> optionName
           <*  symbol "="
           <*> constant

wholeProtoBuf :: MonadParsec Char T.Text m => m ProtoBuf
wholeProtoBuf = declsToProtoBuf <$> many declaration

declaration :: MonadParsec Char T.Text m => m Declaration
declaration
  =   DSyntax  <$ reserved "syntax" <* symbol "=" <*> stringLiteral <* symbol ";"
  <|> DImport  <$ reserved "import"
               <*> (Weak <$ reserved "weak" <|> Public <$ reserved "public" <|> pure Normal)
               <*> stringLiteral <* symbol ";"
  <|> DPackage <$ reserved "package" <*> fullIdentifier <* symbol ";"
  <|> DOption  <$> topOption
  <|> DType    <$> typeDeclaration
  <|> DService <$> serviceDeclaration

typeDeclaration :: MonadParsec Char T.Text m => m TypeDeclaration
typeDeclaration
  =   buildEnum <$  reserved "enum"
                <*> identifier
                <*> betweenBraces (many enumThing)
  <|> buildMessage <$  reserved "message"
                   <*> identifier
                   <*> betweenBraces (many msgThing)
  where
    buildEnum :: T.Text -> [EnumThing] -> TypeDeclaration
    buildEnum name things
      = DEnum name [o | EnumThingOption o <- things]
                   [f | EnumThingField  f <- things]
    buildMessage :: T.Text -> [MsgThing] -> TypeDeclaration
    buildMessage name things
      = DMessage name [o | MsgThingOption   o <- things]
                      [r | MsgThingReserved r <- things]
                      [f | MsgThingField    f <- things]
                      [i | MsgThingInner    i <- things]

data EnumThing
  = EnumThingOption Option | EnumThingField EnumField
enumThing :: MonadParsec Char T.Text m => m EnumThing
enumThing
  =   EnumThingOption <$> topOption
  <|> EnumThingField  <$> enumField
  where
    enumField :: MonadParsec Char T.Text m => m EnumField
    enumField = EnumField <$> identifier
                          <*  symbol "="
                          <*> number
                          <*> (betweenSquares (sepBy innerOption (symbol ",")) <|> pure [])
                          <*  symbol ";"

data MsgThing
  = MsgThingOption   Option
  | MsgThingReserved Reserved
  | MsgThingField    MessageField
  | MsgThingInner    TypeDeclaration
msgThing :: MonadParsec Char T.Text m => m MsgThing
msgThing
  =   MsgThingOption   <$> topOption
  <|> MsgThingReserved <$> reservedOption
  <|> MsgThingInner    <$> typeDeclaration
  <|> MsgThingField    <$> msgField

reservedOption :: MonadParsec Char T.Text m => m Reserved
reservedOption
  = id <$  reserved "reserved"
       <*> sepBy1 reservedValue (symbol ",")
       <*  symbol ";"
  where
    reservedValue :: MonadParsec Char T.Text m => m ReservedValue
    reservedValue =   RRange <$> number <* reserved "to" <*> number
                  <|> RInt   <$> number
                  <|> RName  <$> stringLiteral

msgField :: MonadParsec Char T.Text m => m MessageField
msgField
  =   OneOfField  <$  reserved "oneof"
                  <*> identifier
                  <*> betweenBraces (some msgField)
  <|> MapField    <$  reserved "map"
                  <*  symbol "<"
                  <*> fieldType
                  <*  symbol ","
                  <*> fieldType
                  <*  symbol ">"
                  <*> identifier
                  <*  symbol "="
                  <*> number
                  <*> (betweenSquares (sepBy innerOption (symbol ",")) <|> pure [])
                  <*  symbol ";"
  <|> NormalField <$> (Repeated <$ reserved "repeated" <|> pure Single)
                  <*> fieldType
                  <*> identifier
                  <*  symbol "="
                  <*> number
                  <*> (betweenSquares (sepBy innerOption (symbol ",")) <|> pure [])
                  <*  symbol ";"

serviceDeclaration :: MonadParsec Char T.Text m => m ServiceDeclaration
serviceDeclaration
  = buildService <$  reserved "service"
                 <*> identifier
                 <*> betweenBraces (many serviceThing)
  where
    buildService :: T.Text -> [ServiceThing] -> ServiceDeclaration
    buildService name things
      = Service name [o | ServiceThingOption o <- things]
                     [m | ServiceThingMethod m <- things]

data ServiceThing
  = ServiceThingOption Option | ServiceThingMethod Method
serviceThing :: MonadParsec Char T.Text m => m ServiceThing
serviceThing
  =   ServiceThingOption <$> topOption
  <|> ServiceThingMethod <$> method
  where
    method = Method <$  reserved "rpc"
                    <*> identifier
                    <*  symbol "("
                    <*> (Stream <$ reserved "stream" <|> pure Single)
                    <*> fieldType
                    <*  symbol ")"
                    <*  reserved "returns"
                    <*  symbol "("
                    <*> (Stream <$ reserved "stream" <|> pure Single)
                    <*> fieldType
                    <*  symbol ")"
                    <*> (betweenBraces (many topOption) <|> [] <$ symbol ";")

fieldType :: MonadParsec Char T.Text m => m FieldType
fieldType
  =   TInt32 <$ reserved "int32"
  <|> TInt64 <$ reserved "int64"
  <|> TUInt32 <$ reserved "uint32"
  <|> TUInt64 <$ reserved "uint64"
  <|> TSInt32 <$ reserved "sint32"
  <|> TSInt64 <$ reserved "sint64"
  <|> TFixed32 <$ reserved "fixed32"
  <|> TFixed64 <$ reserved "fixed64"
  <|> TSFixed32 <$ reserved "sfixed32"
  <|> TSFixed64 <$ reserved "sfixed64"
  <|> TDouble <$ reserved "double"
  <|> TBool <$ reserved "bool"
  <|> TString <$ reserved "string"
  <|> TBytes <$ reserved "bytes"
  <|> TOther <$> fullIdentifier