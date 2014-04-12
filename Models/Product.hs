{-# LANGUAGE OverloadedStrings #-}
module Models.Product where

import Model

import Data.Decimal
import Control.Applicative
import Database.SQLite.Simple (execute, query_, lastInsertRowId)
import Database.SQLite.Simple.FromRow

data Product = Product { productPK :: Int
                       , name :: String
                       , price :: Decimal
                       , base :: Int
                       , currency :: String }
               deriving (Show)

instance FromRow Product where
  fromRow = Product <$> field <*> field <*> (Decimal <$> field <*> field) <*> field <*> field

mkProduct = Product

insertProduct conn prod = do
  execute conn "insert into products (name,price,priceDecimalPlaces,base,currency) values (?,?,?,?,?)" (n,p,pdp,b,c)
  id <- lastInsertRowId conn
  return prod{ productPK = fromIntegral id }
  where (Product _ n (Decimal pdp p) b c) = prod

updateProduct conn prod = do
  execute conn "update products set name=?,price=?,priceDecimalPlaces=?,base=?,currency=? where pk=?" (n,p,pdp,b,c,pk)
  return prod
  where (Product pk n (Decimal pdp p) b c) = prod

allProducts conn = query_ conn "select * from products" :: IO [Product]
