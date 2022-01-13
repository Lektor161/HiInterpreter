module HW3.Pretty where

import HW3.Base

import Prettyprinter.Render.Terminal
import Prettyprinter
import Data.Scientific
import Data.Ratio
import qualified Data.Text as T
import Data.Sequence
import qualified Data.List as L
import qualified Data.ByteString as DB
import qualified Data.Word as DW

prettyValue :: HiValue -> Doc AnsiStyle

prettyValue (HiValueNumber val) =
  let num = numerator val
      den = denominator val in pretty $ case den of
        1         -> show $ numerator val
        otherwise -> case fromRationalRepetendUnlimited val of
          (res, Nothing) ->  Data.Scientific.formatScientific Data.Scientific.Fixed Nothing res
          _ -> case quotRem num den of
            (x, 0) -> show x
            (0, y) -> show y ++ "/" ++ show den
            (x, y) -> show x ++ (if (signum y == 1) then " + " else " - ") ++ show (abs y) ++ "/" ++ show den

--prettyValue (HiValueNumber val) = pretty $ case fromRationalRepetendUnlimited val of
--  (res, Nothing) ->  Data.Scientific.formatScientific Data.Scientific.Fixed Nothing res
--  _ -> let num = numerator   val
--           den = denominator val in case quotRem num den of
--             (x, 0) -> show x
--             (0, y) -> show y ++ "/" ++ show den
--             (x, y) -> show x ++ (if (signum y == 1) then " + " else " - ") ++ show (abs y) ++ "/" ++ show den

prettyValue (HiValueAction HiActionNow)         = pretty "now"
prettyValue (HiValueAction HiActionCwd)         = pretty "cwd"
prettyValue (HiValueAction (HiActionChDir str)) = pretty $ "cd(\"" ++ str ++ "\")"
prettyValue (HiValueAction (HiActionMkDir str)) = pretty $ "mkdir(\"" ++ str ++ "\")"
prettyValue (HiValueAction (HiActionRead str))  = pretty $ "read(\"" ++ str ++ "\")"
prettyValue (HiValueAction (HiActionWrite str byts))
        = pretty $ "write(\"" ++ str ++ "," ++ (show $ prettyValue $ HiValueBytes byts) ++ "\")"


prettyValue (HiValueBool False)  = pretty "false"
prettyValue (HiValueBool True)   = pretty "true"
prettyValue (HiValueString text) = pretty $ "\"" ++ (T.unpack text) ++ "\""
prettyValue HiValueNull          = pretty "null"
prettyValue (HiValueList seq)    = pretty $ "[ " <> (helper "" (fmap (show . prettyValue) seq)) <> " ]" where
  helper :: String -> Seq String -> String
  helper s Empty         = s
  helper s (a :<| Empty) = s <> a
  helper s (a :<| as)    = helper (s <> a <> ", ") as

prettyValue (HiValueBytes byteStr) = pretty $ "[# " <> (L.intercalate " " (map helper (DB.unpack byteStr))) <> " #]" where
  helper  :: DW.Word8 -> String
  helper w = (helper2 $ div n 16) <> (helper2 $ mod n 16) where n = toInteger w
  helper2 :: Integer  -> String
  helper2 10 = "a"
  helper2 11 = "b"
  helper2 12 = "c"
  helper2 13 = "d"
  helper2 14 = "e"
  helper2 15 = "f"
  helper2 n  = show n

prettyValue (HiValueFunction fun) = pretty $ case fun of
  HiFunDiv            -> "div"
  HiFunMul            -> "mul"
  HiFunAdd            -> "add"
  HiFunSub            -> "sub"
  HiFunNot            -> "not"
  HiFunAnd            -> "and"
  HiFunOr             -> "or"
  HiFunLessThan       -> "less-than"
  HiFunGreaterThan    -> "greater-than"
  HiFunEquals         -> "equals"
  HiFunNotLessThan    -> "not-less-than"
  HiFunNotGreaterThan -> "not-greater-than"
  HiFunNotEquals      -> "not-equals"
  HiFunIf             -> "if"
  HiFunLength         -> "length"
  HiFunToUpper        -> "to-upper"
  HiFunToLower        -> "to-lower"
  HiFunReverse        -> "reverse"
  HiFunTrim           -> "trim"
  HiFunList           -> "list"
  HiFunRange          -> "range"
  HiFunFold           -> "fold"
  HiFunPackBytes      -> "pack-bytes"
  HiFunUnpackBytes    -> "unpack-bytes"
  HiFunEncodeUtf8     -> "encode-utf8"
  HiFunDecodeUtf8     -> "decode-utf8"
  HiFunZip            -> "zip"
  HiFunUnzip          -> "unzip"
  HiFunSerialise      -> "serialise"
  HiFunDeserialise    -> "deserialise"
  HiFunParseTime      -> "parse-time"