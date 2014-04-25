{-# LANGUAGE OverloadedStrings #-}
module Models.Invoice where

import Control.Applicative
import Database.SQLite.Simple (execute, query_, lastInsertRowId)
import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.ToRow
import Database.SQLite.Simple.ToField (toField)

import Models.Client
import Models.Product
import Data.Time.Calendar (Day)
import Data.Decimal

data Period = Period Day Day deriving (Show)
data ProductItem = ProductItem { qty :: Int
                               , product :: Product
                               , comment :: String }
                 deriving (Show)

data Invoice = Invoice { invoicePK :: Int
                       , invoiceID :: Int
                       , client :: Client -- (to one)
                       , period :: Period
                       , date :: Day
                       , due :: Day
                       , items :: [ProductItem]
                       , vat :: Decimal
                       , discount :: Decimal
                       , cashback :: Decimal }
               deriving (Show)

name inv = (show . date $ inv) ++ " " ++ (Models.Client.name . client $ inv) -- (total)

instance FromRow Invoice where
  fromRow = mkInvoice' <$> field <*> field
            <*> fromRow
            <*> (Period <$> field <*> field)
            <*> field <*> field
            <*> (Decimal <$> field <*> field)
            <*> (Decimal <$> field <*> field)
            <*> (Decimal <$> field <*> field)

instance ToRow Invoice where
  toRow inv = [toField i, toField cpk -- id, client pk
              ,toField pf, toField pt -- from, to
              ,toField dt, toField du -- date, due
              ,toField vdp, toField v -- vat
              ,toField ddp, toField d -- discount
              ,toField cdp, toField c -- cashback
              ]
    where (Invoice _ i (Client { clientPK = cpk }) (Period pf pt) dt du items (Decimal vdp v) (Decimal ddp d) (Decimal cdp c)) = inv

mkInvoice = Invoice
mkInvoice' a b c d e f g h i = mkInvoice a b c d e f [] g h i

insertInvoice conn inv = do
  execute conn "insert into invoices (id,client,`from`,`to`,`date`,`due`,vatDecimalPlaces,vat,discountDecimalPlaces,discount,cashbackDecimalPlaces,cashback) values (?,?,?,?,?,?,?,?,?,?,?,?)" inv
  id <- lastInsertRowId conn
  mapM_ (\(ProductItem q p _) -> execute conn "insert into invoices_products (invoice, product, quantity) values (?,?,?)" (id, productPK p, q)) (items inv)
  return inv{ invoicePK = fromIntegral id }

allInvoices conn = query_ conn "SELECT i.pk, i.id, c.*, i.`from`, i.`to`, i.date, i.due, i.vatDecimalPlaces, i.vat, i.discountDecimalPlaces, i.discount, i.cashbackDecimalPlaces, i.cashback FROM invoices AS i join clients AS c ON (i.client=c.pk)" :: IO [Invoice]
