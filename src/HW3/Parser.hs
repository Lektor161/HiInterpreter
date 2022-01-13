module HW3.Parser where

import qualified Text.Megaparsec.Char.Lexer as L
import HW3.Base
import Data.Void (Void)
import Text.Megaparsec as TM
import Text.Megaparsec.Char
import Data.Text as DT
import Control.Monad.Except
import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Data.ByteString as DB
import qualified Data.Text.Encoding as TE


type Parser = TM.Parsec Void String

parse :: String -> Either (ParseErrorBundle String Void) HiExpr
parse  = TM.runParser (between skipSpace eof infixParser) ""
parseT = TM.parseTest infixParser

sb :: Parser String -> Parser String -> Parser [String]
sb = sepBy

parseAB :: Parser String
parseAB = string "aba"

skipSpace :: Parser ()
skipSpace = L.space
  -- Like `space`, but skips 1 or more space characters.
  space1
  -- Skip from ;; until a newline.
  (L.skipLineComment ";;")
  -- Skip from /* until */. There is also `skipBlockComment`, but it doesn't handle nested comments.
  (L.skipBlockCommentNested "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme skipSpace
symbol = L.symbol skipSpace

funParser :: Parser HiFun
funParser = HiFunDiv            <$ string "div"              <|>
            HiFunMul            <$ string "mul"              <|>
            HiFunAdd            <$ string "add"              <|>
            HiFunSub            <$ string "sub"              <|>
            HiFunNot            <$ string "not"              <|>
            HiFunAnd            <$ string "and"              <|>
            HiFunOr             <$ string "or"               <|>
            HiFunLessThan       <$ string "less-than"        <|>
            HiFunGreaterThan    <$ string "greater-than"     <|>
            HiFunEquals         <$ string "equals"           <|>
            HiFunNotLessThan    <$ string "not-less-than"    <|>
            HiFunNotGreaterThan <$ string "not-greater-than" <|>
            HiFunNotEquals      <$ string "not-equals"       <|>
            HiFunIf             <$ string "if"               <|>
            HiFunLength         <$ string "length"           <|>
            HiFunToUpper        <$ string "to-upper"         <|>
            HiFunToLower        <$ string "to-lower"         <|>
            HiFunReverse        <$ string "reverse"          <|>
            HiFunTrim           <$ string "trim"             <|>
            HiFunList           <$ string "list"             <|>
            HiFunRange          <$ string "range"            <|>
            HiFunFold           <$ string "fold"             <|>
            HiFunPackBytes      <$ string "pack-bytes"       <|>
            HiFunUnpackBytes    <$ string "unpack-bytes"     <|>
            HiFunEncodeUtf8     <$ string "encode-utf8"      <|>
            HiFunDecodeUtf8     <$ string "decode-utf8"      <|>
            HiFunZip            <$ string "zip"              <|>
            HiFunUnzip          <$ string "unzip"            <|>
            HiFunSerialise      <$ string "serialise"        <|>
            HiFunParseTime      <$ string "parse-time"


literalsParser :: Parser HiValue
literalsParser = HiValueNumber  <$> toRational <$> L.signed skipSpace L.scientific                       <|>
                 HiValueFunction   <$> funParser                                                         <|>
                 HiValueBool False <$  string "false"                                                    <|>
                 HiValueBool True  <$  string "true"                                                     <|>
                 HiValueNull       <$  string "null"                                                     <|>
                 HiValueString     <$> DT.pack <$> ((string "\"") *> manyTill L.charLiteral (char '\"')) <|>
                 HiValueAction     <$> cwdParser                                                         <|>
                 HiValueAction   HiActionNow   <$  (string "now")                                        <|>
                 HiValueAction . HiActionChDir <$> ((lexeme $ string "cd")    *> stringArgParser)        <|>
                 HiValueAction . HiActionMkDir <$> ((lexeme $ string "mkdir") *> stringArgParser)        <|>
                 HiValueAction . HiActionRead  <$> ((lexeme $ string "read")  *> stringArgParser)        <|>
                 HiValueAction                 <$>  writeOp                                              <|>
                 HiValueBytes      <$> bytesParser


writeOp :: Parser HiAction
writeOp = do
  _    <- lexeme $ string "write"
  _    <- lexeme $ string "("
  str1 <- lexeme ((string "\"") *> manyTill L.charLiteral (char '\"'))
  _    <- lexeme $ string ","
  str2 <- lexeme bytesParser <|> (lexeme $ TE.encodeUtf8 <$> DT.pack <$> ((string "\"") *> manyTill L.charLiteral (char '\"')))
  _    <- lexeme $ string ")"
  pure $ HiActionWrite str1 str2

cwdParser :: Parser HiAction
cwdParser = HiActionCwd   <$ string "cwd"

stringArgParser :: Parser String
stringArgParser = do
  _   <- (lexeme $ string "(")
  _   <- (lexeme $ char '\"')
  str <- (lexeme $ manyTill L.charLiteral (char '\"'))
  _   <- (string ")")
  pure str

word8Parser :: Parser Int
word8Parser = let
  helpParser :: Parser Int
  helpParser = (0 <$ string "0")  <|> (1 <$ string "1")  <|> (2 <$ string "2")  <|> (3 <$ string "3")  <|>
               (4 <$ string "4")  <|> (5 <$ string "5")  <|> (6 <$ string "6")  <|> (7 <$ string "7")  <|>
               (8 <$ string "8")  <|> (9 <$ string "9")  <|> (10 <$ string "a") <|> (11 <$ string "b") <|>
               (12 <$ string "c") <|> (13 <$ string "d") <|> (14 <$ string "e") <|> (15 <$ string "f")
                  in do
                    a <- helpParser
                    b <- helpParser
                    pure $ a * 16 + b

bytesParser :: Parser ByteString
bytesParser = do
  _    <- lexeme $ string "[#"
  nums <- many $ lexeme word8Parser
  _    <- lexeme $ string "#]"
  pure $ DB.pack $ fmap toEnum nums

listParser :: Parser HiExpr
listParser = do
  _    <- lexeme $ string "["
  vals <- sepBy (lexeme infixParser) (lexeme $ string ",")
  _    <- string "]"
  pure $ HiExprApply (HiExprValue (HiValueFunction HiFunList)) vals


betweenBrackets :: Parser a -> Parser a
betweenBrackets = between (lexeme $ string "(") (string ")")

argsParser :: Parser [HiExpr]
argsParser = betweenBrackets $ sepBy (lexeme infixParser) (lexeme $ string ",")

parseVal :: Parser HiExpr
parseVal = betweenBrackets infixParser               <|>
           (lexeme $ HiExprValue <$> literalsParser) <|>
           (lexeme listParser)

prefixParser :: Parser HiExpr
prefixParser = do
    val  <- parseVal
    args <- many $ lexeme argsParser
    pure $ helper val args where
      helper val []       = val
      helper val (a : as) = helper (HiExprApply val a) as

parseTokens :: Parser HiExpr
parseTokens = do
  val <- parseVal
  args <- many $ lexeme argsParser
  pure $ helper val args where
        helper val []       = val
        helper val (a : as) = helper (HiExprApply val a) as

infixParser = makeExprParser parseTokens table

table = [ [
            binary "*"  HiFunMul,
            InfixL ((\a b -> HiExprApply (HiExprValue $ HiValueFunction HiFunDiv ) [a, b]) <$ (try ( (lexeme $ string "/") <* notFollowedBy (string "="))))
          ], [
            binary "+"  HiFunAdd,
            binary "-"  HiFunSub
          ], [
            binary "/=" HiFunNotEquals,
            binary "==" HiFunEquals,
            binary ">=" HiFunNotLessThan,
            binary "<=" HiFunNotGreaterThan,
            binary ">"  HiFunGreaterThan,
            binary "<"  HiFunLessThan
          ], [
            binary "&&" HiFunAnd,
            binary "||" HiFunOr
          ], [
            Postfix (HiExprRun  <$ (symbol "!"))
          ] ]

binary name fun = InfixL ((\a b -> HiExprApply (HiExprValue $ HiValueFunction fun ) [a, b]) <$ (lexeme $ string name))
--postfix name f = Postfix (f <$ symbol name)


--qwe :: Parser String
--qwe = manyTill L.charLiteral (char '\"')

