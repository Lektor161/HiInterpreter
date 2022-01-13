module HW3.Evaluator where

import HW3.Base
import HW3.Parser
import HW3.Action

import qualified Data.Set.Internal as Set

import Control.Monad.Cont (liftIO)
--import qualified Data.Serialize as SER
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Sequence as DS
import Data.Ratio
import qualified Data.ByteString as DB
import qualified Data.ByteString.Lazy as DBLazy
import qualified Data.Char as DC
import qualified Codec.Compression.Zlib as Z

import Control.Monad.Except(throwError)

eval :: HiMonad m => HiExpr -> m (Either HiError HiValue)
eval (HiExprValue x) = return $ Right x
eval (HiExprRun      (HiExprValue (HiValueAction action))) = Right <$> (runAction action)

eval (HiExprRun expr@(HiExprApply _ _)) = do
  exprEval <- eval expr
  case exprEval of
    Right (HiValueAction action) -> Right <$> (runAction action)
    otherwise                          -> return $ Left HiErrorInvalidArgument

eval (HiExprApply expr vals) = do
  exprEval <- eval expr
  case exprEval of
    Left err                    -> return $ Left err
    Right (HiValueNumber _)     -> return $ Left HiErrorInvalidFunction
    Right (HiValueBool   _)     -> return $ Left HiErrorInvalidFunction
    Right (HiValueString t)     -> hiStringOp t (fmap eval vals)
    Right (HiValueList seq)     -> hiListOp seq  (fmap eval vals)
    Right (HiValueBytes byts)   -> hiBytsOp byts (fmap eval vals)
    Right (HiValueFunction fun) -> (case fun of
      HiFunMul            -> hiBinOp mulOp
      HiFunAdd            -> hiBinOp addOp
      HiFunSub            -> hiBinOp (makeBinNumOp  (-)  )
      HiFunNot            -> hiUnOp  (makeUnBoolOp  (not))
      HiFunAnd            -> hiBinOp (makeBinBoolOp (&&) )
      HiFunOr             -> hiBinOp (makeBinBoolOp (||) )
      HiFunDiv            -> hiBinOp hiDivOp
      HiFunEquals         -> hiBinOp equalsOp
      HiFunLessThan       -> hiBinOp lessOp
      HiFunGreaterThan    -> hiBinOp greatOp
      HiFunNotEquals      -> hiBinOp $ invertBoolBinOp equalsOp
      HiFunNotLessThan    -> hiBinOp $ invertBoolBinOp lessOp
      HiFunNotGreaterThan -> hiBinOp $ invertBoolBinOp greatOp
      HiFunIf             -> ifOp
      HiFunLength         -> hiUnOp lengthOp
      HiFunToUpper        -> textOp $ HiValueString . T.toUpper
      HiFunToLower        -> textOp $ HiValueString . T.toLower
      HiFunReverse        -> hiUnOp reverseOp
      HiFunTrim           -> textOp $ HiValueString . T.strip
      HiFunList           -> buildList
      HiFunRange          -> hiBinOp rangeOp
      HiFunFold           -> hiBinOp foldOp
      HiFunPackBytes      -> hiUnOp packOp
      HiFunUnpackBytes    -> hiUnOp unpackOp
      HiFunEncodeUtf8     -> hiUnOp encodeUtfOp
      HiFunDecodeUtf8     -> hiUnOp decodeUtfOp
      HiFunZip            -> hiUnOp zipOp
      HiFunUnzip          -> hiUnOp unzipOp
      HiFunParseTime      -> hiUnOp timeOp
      )  (fmap eval vals)

timeOp :: HiValue -> Either HiError HiValue
timeOp (HiValueString t) =

buildList :: HiMonad m => [m (Either HiError HiValue)] -> m (Either HiError HiValue)
buildList ms = foldr helper (pure $ Right $ HiValueList DS.Empty) ms where
  helper :: HiMonad m => m (Either HiError HiValue) -> m (Either HiError HiValue) -> m (Either HiError HiValue)
  helper m1 m2 = (helper2 <$> m1) <*> m2 where
    helper2 :: Either HiError HiValue -> Either HiError HiValue -> Either HiError HiValue
    helper2 (Left err) _ = Left err
    helper2 _ (Left err) = Left err
    helper2 (Right a) (Right (HiValueList as)) = Right $ HiValueList $ a :<| as


zipOp :: HiValue -> Either HiError HiValue
zipOp (HiValueBytes bytes) = Right $ HiValueBytes $ DBLazy.toStrict $ Z.compress $ DBLazy.fromStrict bytes
zipOp _ = Left HiErrorInvalidArgument

unzipOp :: HiValue -> Either HiError HiValue
unzipOp (HiValueBytes bytes) = Right $ HiValueBytes $ DBLazy.toStrict $ Z.decompress $ DBLazy.fromStrict bytes
unzipOp _ = Left HiErrorInvalidArgument


seqToArr :: Seq a -> [a]
seqToArr Empty = []
seqToArr (a :<| as) = a : (seqToArr as)

decodeUtfOp :: HiValue -> Either HiError HiValue
decodeUtfOp (HiValueBytes bytes) = case TE.decodeUtf8' bytes of
  (Right t) -> Right $ HiValueString t
  (Left _) -> Left HiErrorInvalidArgument
decodeUtfOp _ = Left HiErrorInvalidArgument


encodeUtfOp :: HiValue -> Either HiError HiValue
encodeUtfOp (HiValueString t) =  Right $ HiValueBytes $ TE.encodeUtf8 t
encodeUtfOp _ = Left HiErrorInvalidArgument

unpackOp :: HiValue -> Either HiError HiValue
unpackOp (HiValueBytes bytes) = Right $ HiValueList $ DS.fromList $ map (HiValueNumber . toRational . toInteger) (DB.unpack bytes)
unpackOp _ = Left HiErrorInvalidArgument

packOp :: HiValue -> Either HiError HiValue
packOp (HiValueList list) = Right $ HiValueBytes $ DB.pack $ fmap toEnum (fmap helper (seqToArr list)) where
  helper :: HiValue -> Int
  helper (HiValueNumber n) = rationalToInt n
packOp _ = Left HiErrorInvalidArgument

reverseOp :: HiValue -> Either HiError HiValue
reverseOp (HiValueList seq) = Right $ HiValueList $ DS.reverse seq
reverseOp (HiValueString t) = Right $ HiValueString $ T.reverse t
reverseOp _ = Left HiErrorInvalidArgument

lengthOp :: HiValue -> Either HiError HiValue
lengthOp (HiValueList seq) = Right $ HiValueNumber $ toRational $ DS.length seq
lengthOp (HiValueString t) = Right $ HiValueNumber $ toRational $ T.length t
lengthOp _ = Left HiErrorInvalidArgument

foldOp :: HiValue -> HiValue -> Either HiError HiValue
foldOp (HiValueFunction HiFunAdd) (HiValueList seq) = foldSeq addOp                   seq
foldOp (HiValueFunction HiFunMul) (HiValueList seq) = foldSeq mulOp                   seq
foldOp (HiValueFunction HiFunSub) (HiValueList seq) = foldSeq (makeBinNumOp  (-))     seq
foldOp (HiValueFunction HiFunDiv) (HiValueList seq) = foldSeq hiDivOp                 seq
foldOp (HiValueFunction HiFunAnd) (HiValueList seq) = foldSeq ((makeBinBoolOp  (&&))) seq
foldOp (HiValueFunction HiFunOr)  (HiValueList seq) = foldSeq ((makeBinBoolOp  (||))) seq
foldOp _ _ = Left HiErrorInvalidArgument


foldSeq :: (HiValue -> HiValue -> Either HiError HiValue) -> Seq HiValue -> Either HiError HiValue
foldSeq _ DS.Empty = Right HiValueNull
foldSeq op (a :<| as) = foldl helper (Right a) (fmap Right as) where
  helper :: Either HiError HiValue -> Either HiError HiValue ->  Either HiError HiValue
  helper (Right a)  (Right b) = op a b
  helper (Left err) _         = Left err
  helper _         (Left err) = Left err

foldBoolSeq :: (Bool -> Bool -> Bool) -> Bool -> Seq HiValue -> Either HiError HiValue
foldBoolSeq _ _ DS.Empty = Right HiValueNull
foldBoolSeq op zero seq = foldl helper (Right (HiValueBool zero)) (fmap Right seq) where
  helper :: Either HiError HiValue -> Either HiError HiValue ->  Either HiError HiValue
  helper (Left err) _ = Left err
  helper _ (Left err) = Left err
  helper (Right (HiValueBool a)) (Right (HiValueBool b)) = Right $ HiValueBool $ op a b
  helper _          _ = Left HiErrorInvalidArgument

foldNumSeq :: (Rational -> Rational -> Rational) -> Rational -> Seq HiValue -> Either HiError HiValue
foldNumSeq _ _ DS.Empty = Right HiValueNull
foldNumSeq op zero seq = foldl helper (Right (HiValueNumber zero)) (fmap Right seq) where
  helper :: Either HiError HiValue -> Either HiError HiValue ->  Either HiError HiValue
  helper (Left err) _ = Left err
  helper _ (Left err) = Left err
  helper (Right (HiValueNumber a)) (Right (HiValueNumber b)) = Right $ HiValueNumber $ op a b
  helper _          _ = Left HiErrorInvalidArgument

rangeOp :: HiValue -> HiValue -> Either HiError HiValue
rangeOp (HiValueNumber a) (HiValueNumber b) = Right $ HiValueList $ fmap HiValueNumber (helper a) where
  helper :: Rational -> Seq Rational
  helper i | i > b = Empty
           | otherwise  = i :<| (helper $ i + 1)
rangeOp _ _ = Left HiErrorInvalidArgument

hiBytsOp :: HiMonad m => DB.ByteString -> [m (Either HiError HiValue)] -> m (Either HiError HiValue)
hiBytsOp bs = abstractSubstr (DB.length bs) (\i j -> HiValueBytes $ DB.take (j - i) $ DB.drop i bs)

hiListOp :: HiMonad m => Seq HiValue -> [m (Either HiError HiValue)] -> m (Either HiError HiValue)
hiListOp seq [mi] = do
  val <- mi
  case val of
    (Right (HiValueNumber i)) -> if isInt i then case DS.lookup (rationalToInt i) seq of
                                                            Just x  -> return $ Right x
                                                            Nothing -> return $ Left HiErrorInvalidArgument
                                 else return $ Left HiErrorInvalidArgument

hiListOp seq ms = (abstractSubstr (DS.length seq) (\i j -> HiValueList $ DS.take (j - i) $ DS.drop i seq)) ms

hiStringOp :: HiMonad m => T.Text -> [m (Either HiError HiValue)] -> m (Either HiError HiValue)
hiStringOp t = abstractSubstr (T.length t) (\i j -> HiValueString $ T.take (j - i) $ T.drop i t)

abstractSubstr :: HiMonad m => Int -> (Int -> Int -> HiValue) -> [m (Either HiError HiValue)] -> m (Either HiError HiValue)
abstractSubstr l g [m] = abstractSubstr l g [m, inc <$> m] where
  inc :: Either HiError HiValue -> Either HiError HiValue
  inc (Right (HiValueNumber x)) = Right $ HiValueNumber $ x + 1
  inc (Left err)                = Left err
  inc _                         = Left HiErrorInvalidArgument

abstractSubstr len get [m1, m2] = do
  val1 <- m1
  val2 <- m2
  case (val1, val2) of
    (Right (HiValueNumber i), Right (HiValueNumber j)) ->
                if isInt i && isInt j then let iInt = rationalToInt i
                                               jInt = rationalToInt j in
                                       if iInt >= 0 && iInt <= jInt && jInt <= len then return $ Right $ get iInt jInt
                                                                                   else return $ Right $ HiValueNull
                else return $ Left HiErrorInvalidArgument
    otherwise -> return $ Left HiErrorInvalidArgument
abstractSubstr _ _ _ = return $ Left HiErrorArityMismatch

addOp :: HiValue -> HiValue -> Either HiError HiValue
addOp (HiValueNumber a) (HiValueNumber b) = Right $ HiValueNumber $ a + b
addOp (HiValueString a) (HiValueString b) = Right $ HiValueString $ a <> b
addOp (HiValueList a)   (HiValueList b)   = Right $ HiValueList   $ a <> b
addOp (HiValueBytes a)  (HiValueBytes b)  = Right $ HiValueBytes  $ a <> b
addOp _ _ = Left HiErrorInvalidArgument

mulOp :: HiValue -> HiValue -> Either HiError HiValue
mulOp (HiValueNumber a) (HiValueNumber b) = Right $ HiValueNumber $ a * b
mulOp (HiValueString s) (HiValueNumber k) | (not $ isInt k) || k <= 0 = Left HiErrorInvalidArgument
                                          | otherwise               = Right $ HiValueString $ T.replicate (rationalToInt k) s
mulOp (HiValueList seq) (HiValueNumber k)
  | (not $ isInt k) || k <= 0 = Left HiErrorInvalidArgument
  | otherwise               = Right $ HiValueList $ cycleTaking ((rationalToInt k) * (foldl (\i _ -> i + 1) 0 seq))
                                                      seq
mulOp (HiValueBytes seq) (HiValueNumber k)
  | (not $ isInt k) || k <= 0 = Left HiErrorInvalidArgument
  | otherwise               = Right $ HiValueBytes $ helper (rationalToInt k) seq where
    helper :: Int -> DB.ByteString -> DB.ByteString
    helper 0 _ = DB.empty
    helper 1 x = x
    helper n x | mod n 2 == 0 = helper (div n 2) (x <> x)
               | otherwise    = x <> (helper (n - 1) x)

mulOp _ _ = Left HiErrorInvalidArgument


textOp :: HiMonad m => (T.Text -> HiValue) -> [m (Either HiError HiValue)] -> m(Either HiError HiValue)
textOp f = hiUnOp helper where
  helper :: HiValue -> Either HiError HiValue
  helper (HiValueString text) = Right $ f text
  helper _                    = Left HiErrorInvalidArgument

ifOp :: HiMonad m => [m (Either HiError HiValue)] -> m (Either HiError HiValue)
ifOp [m1, m2, m3] = do
  flag <- m1
  case flag of
    (Right (HiValueBool True))  -> m2
    (Right (HiValueBool False)) -> m3
    (Left err)                  -> return $ Left err
    _                           -> return $ Left HiErrorInvalidArgument

invertBoolBinOp :: (HiValue -> HiValue -> Either HiError HiValue) -> HiValue -> HiValue -> Either HiError HiValue
invertBoolBinOp op val1 val2 = case op val1 val2 of
  Left err -> Left err
  Right (HiValueBool res) -> Right $ HiValueBool $ not res

equalsOp :: HiValue -> HiValue -> Either HiError HiValue
equalsOp a b = Right . HiValueBool $ a == b

lessOp :: HiValue -> HiValue -> Either HiError HiValue
lessOp a b = Right . HiValueBool $ a < b

greatOp :: HiValue -> HiValue -> Either HiError HiValue
greatOp a b = Right . HiValueBool $ a > b

hiDivOp :: HiValue -> HiValue -> Either HiError HiValue
hiDivOp (HiValueString t1) (HiValueString t2) = Right $ HiValueString $ t1 <> (T.pack "/") <> t2
hiDivOp _                  (HiValueNumber 0)  = Left $ HiErrorDivideByZero
hiDivOp (HiValueNumber x)  (HiValueNumber y)  = Right $ HiValueNumber $ x / y
hiDivOp _                   _                 = Left HiErrorInvalidArgument

makeBinNumOp :: (Rational -> Rational -> Rational) -> HiValue -> HiValue -> Either HiError HiValue
makeBinNumOp op (HiValueNumber val1) (HiValueNumber val2) = Right $ HiValueNumber $ op val1 val2
makeBinNumOp _ _ _ = Left HiErrorInvalidArgument

makeUnBoolOp :: (Bool -> Bool) -> HiValue -> Either HiError HiValue
makeUnBoolOp op (HiValueBool b) = Right $ HiValueBool $ op b
makeUnBoolOp op _               = Left HiErrorInvalidArgument

makeBinBoolOp :: (Bool -> Bool -> Bool) -> HiValue -> HiValue -> Either HiError HiValue
makeBinBoolOp op (HiValueBool a) (HiValueBool b) = Right $ HiValueBool $ op a b
makeBinBBoolOp _ _ _ = Left HiErrorInvalidArgument

hiUnOp :: HiMonad m => (HiValue -> Either HiError HiValue) -> [m (Either HiError HiValue)] -> m (Either HiError HiValue)
hiUnOp op [m1] = (value1FunToEither op) <$> m1
hiUnOp _  _    = pure $ Left HiErrorArityMismatch

hiBinOp :: HiMonad m => (HiValue -> HiValue -> Either HiError HiValue) -> [m (Either HiError HiValue)] -> m (Either HiError HiValue)
hiBinOp op [m1, m2] = ((value2FunToEither op) <$> m1) <*> m2
hiBinOp _ _ = pure $ Left HiErrorArityMismatch

value1FunToEither :: (HiValue -> Either HiError HiValue) -> Either HiError HiValue -> Either HiError HiValue
value1FunToEither _  (Left err)  = Left err
value1FunToEither op (Right val) = op val

value2FunToEither :: (HiValue -> HiValue -> Either HiError HiValue) -> Either HiError HiValue -> Either HiError HiValue -> Either HiError HiValue
value2FunToEither _  (Left err)    _           = Left err
value2FunToEither _  _            (Left err)   = Left err
value2FunToEither op (Right val1) (Right val2) = op val1 val2

isInt :: Rational -> Bool
isInt t = (denominator t) == 1

rationalToInt :: Rational -> Int
rationalToInt = fromIntegral . numerator

