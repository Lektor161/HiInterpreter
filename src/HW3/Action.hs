{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingVia        #-}
{-# LANGUAGE InstanceSigs #-}
module HW3.Action where


import HW3.Base

import Control.Exception (Exception)
import Data.Set (Set, member)
import GHC.IO.Exception (IOErrorType (PermissionDenied))
import Control.Arrow
import System.Directory
import Data.Text as T
import Data.ByteString as DB
import Data.Text.Encoding as ENC
import Control.Exception as EX

data HiPermission =
    AllowRead
  | AllowWrite
  | AllowTime
  deriving (Show, Eq, Ord, Enum, Bounded)

data PermissionException =
  PermissionRequired HiPermission
  deriving Show

instance Exception PermissionException

--newtype HIO a = HIO { runHIO :: Set HiPermission -> IO a }

newtype HIO a = HIO {runHIO :: Set HiPermission -> IO a}
  deriving
    (Functor, Applicative, Monad)
    via (Kleisli IO (Set HiPermission))

instance HiMonad HIO where
    runAction :: HiAction -> HIO HiValue
    runAction HiActionCwd           = HIO (\ps -> if member AllowRead ps  then HiValueString    <$> T.pack <$> getCurrentDirectory
                                                                          else throwIO $ PermissionRequired AllowRead )
    runAction (HiActionChDir fp)    = HIO (\ps -> if member AllowRead ps  then HiValueNull      <$  setCurrentDirectory fp
                                                                          else throwIO $ PermissionRequired AllowWrite)
    runAction (HiActionMkDir fp)    = HIO (\ps -> if member AllowWrite ps then HiValueNull      <$  createDirectory fp
                                                                          else throwIO $ PermissionRequired AllowWrite)
    runAction (HiActionRead fp)     = HIO (\ps -> if member AllowRead ps  then bytesToUtf8MayBe <$> DB.readFile fp
                                                                          else throwIO $ PermissionRequired AllowRead)
    runAction (HiActionWrite fp bs) = HIO (\ps -> if member AllowWrite ps then HiValueNull      <$  DB.writeFile fp bs
                                                                          else throwIO $ PermissionRequired AllowWrite)


bytesToUtf8MayBe :: ByteString -> HiValue
bytesToUtf8MayBe bs = case ENC.decodeUtf8' bs of
  (Left _)  -> HiValueBytes bs
  (Right t) -> HiValueString t