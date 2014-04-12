{-# LANGUAGE OverloadedStrings #-}
module Models.Invoice where

import Control.Applicative
import Database.SQLite.Simple (execute, query_, lastInsertRowId)
import Database.SQLite.Simple.FromRow

import Models.Client
import Models.Product
import Data.Decimal

data Invoice = Invoice { invoicePK :: Int
                       , invoiceID :: Int
                       , client :: Client -- (to one)
                       , products :: [(Product, Int)]
                       , vat :: Decimal
                       , discount :: Decimal
                       , cashback :: Decimal }
               deriving (Show)


instance FromRow Invoice where
  fromRow = mkInvoice' <$> field <*> field
            <*> fromRow
            <*> (Decimal <$> field <*> field)
            <*> (Decimal <$> field <*> field)
            <*> (Decimal <$> field <*> field)

mkInvoice = Invoice
mkInvoice' a b c d e f = mkInvoice a b c [] d e f

insertInvoice conn inv = do
  execute conn "insert into invoices (id,client,vatDecimalPlaces,vat,discountDecimalPlaces,discount,cashbackDecimalPlaces,cashback) values (?,?,?,?,?,?,?,?)" (i,cpk,vdp,v,ddp,d,cdp,c)
  id <- lastInsertRowId conn
  return inv{ invoicePK = fromIntegral id }
  where (Invoice _ i (Client { clientPK = cpk }) products (Decimal vdp v) (Decimal ddp d) (Decimal cdp c)) = inv

allInvoices conn = query_ conn "SELECT i.pk, i.id, c.*, i.vatDecimalPlaces, i.vat, i.discountDecimalPlaces, i.discount, i.cashbackDecimalPlaces, i.cashback FROM invoices AS i join clients AS c ON (i.client=c.pk)" :: IO [Invoice]
