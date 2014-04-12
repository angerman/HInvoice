{-# LANGUAGE OverloadedStrings #-}
module Model where

-- Decimal support for SQLite Simple
import Data.Decimal
import Database.SQLite.Simple.FromField
import Data.Word
import Data.Typeable (Typeable)
import Database.SQLite.Simple.Ok
import Database.SQLite3 as Base (SQLData(..))
import Database.SQLite.Simple.Internal (Field(..))
-- for decimal we need word8 reading
-- takeInt is not exported by SQLite.Simple.FromField
takeInt :: (Num a, Typeable a) => Field -> Ok a
takeInt (Field (SQLInteger i) _) = Ok . fromIntegral $ i
takeInt f                        = returnError ConversionFailed f "need an int"
instance FromField Word8 where
  fromField = takeInt

