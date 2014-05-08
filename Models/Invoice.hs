{-# LANGUAGE OverloadedStrings #-}
module Models.Invoice where

import Control.Applicative
import Database.SQLite.Simple (Only(..), execute, query, query_, lastInsertRowId, (:.)(..))
import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.ToRow
import Database.SQLite.Simple.ToField (toField)

import Models.Client
import Models.Product
import Data.Time.Calendar (Day)
import Data.Decimal

data Period = Period (Maybe Day) (Maybe Day) deriving (Show)
data ProductItem = ProductItem { qty :: Int
                               , product :: Product
                               , comment :: String }
                 deriving (Show)

productItemTotal pi@(ProductItem q p _) = (fromIntegral q) * (price p)

instance FromRow ProductItem where
  fromRow = ProductItem <$> field <*> fromRow <*> field -- Product supports fromRow!

-- no ToRow support for now. This is implicitly handled in @insertInvoice@.

data Invoice = Invoice { invoicePK :: Maybe Int
                       , invoiceID :: Int
                       , client :: Client -- (to one)
                       , period :: Period
                       , date :: Maybe Day
                       , due :: Maybe Day
                       , items :: [ProductItem]
                       , vat :: Decimal
                       , discount :: Decimal
                       , cashback :: Decimal }
               deriving (Show)

mkNewInvoice = Invoice Nothing (-1) EmptyClient (Period Nothing Nothing) Nothing Nothing [] 0.19 0.0 0.0

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
  mapM_ (\(ProductItem q p c) -> execute conn "insert into invoices_products (invoice, product, quantity, comment) values (?,?,?,?)" (id, productPK p, q, c)) (items inv)
  return inv{ invoicePK = Just $ fromIntegral id }

updateOrInsertInvoice conn inv@(Invoice { invoicePK = Nothing }) = insertInvoice conn inv
updateOrInsertInvoice conn inv@(Invoice { invoicePK = Just pk }) = do
  execute conn "update invoices set id=?, client=?, `from`=?, `to`=?, `date`=?, `due`=?, vatDecimalPlaces=?, vat=?, discountDecimalPlaces=?, discount=?, cashbackDecimalPlaces=?, cashback=? where pk=?" (inv :. (Only pk))
  execute conn "delete from invoices_products where invoice=?" (Only pk)
  mapM_ (\(ProductItem q p c) -> execute conn "insert into invoices_products (invoice, product, quantity, comment) values (?,?,?,?)" (pk, productPK p, q, c)) (items inv)
  return inv

allInvoices conn = do
  invoices <- query_ conn "SELECT i.pk, i.id, c.*, i.`from`, i.`to`, i.date, i.due, i.vatDecimalPlaces, i.vat, i.discountDecimalPlaces, i.discount, i.cashbackDecimalPlaces, i.cashback FROM invoices AS i join clients AS c ON (i.client=c.pk)" :: IO [Invoice]
  -- I would have prefered to see this *in* the fromRow logic, such that fromRow would pull the required items from the database.
  -- Another solution to explore would be to test if we can lazy load the data? Provide the items piece with a funtion returning a list,
  -- but prepopulated with the connection and the invoice itself, thus being able to pull the data on demand. Would probably require items
  -- to be of type IO [ProductItems] (?).
  --
  -- Anyway, now we need to populate every invoice with it's product items.
  mapM addItemsToInvoice invoices
  where
    addItemsToInvoice inv = do
      productItems <- query conn "SELECT ip.quantity, p.*, IFNULL(ip.comment,'') FROM invoices_products AS ip JOIN products p ON (ip.product=p.pk) WHERE ip.invoice=?" (Only (invoicePK inv)) :: IO [ProductItem]
      return $ inv { items = productItems }
