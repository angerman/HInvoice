{-# LANGUAGE OverloadedStrings #-}
module UI where

import Graphics.Vty hiding (Button)
import Graphics.Vty.Widgets.All
import Database.SQLite.Simple (Connection)
import qualified Data.Text as T

import Models.Client
import Models.Product
import Models.Invoice
import Views.AddClient
import Views.AddProduct
import Views.AddInvoice

companyName = "HInvoice"
version = "v0.1"

selAttr = black `on` yellow

data ClientsUI =
  ClientsUI { getClList :: Widget (List Client FormattedText) }

data ProductsUI =
  ProductsUI { getProdList :: Widget (List Product FormattedText) }

data InvoicesUI =
  InvoicesUI { getInvList :: Widget (List Invoice FormattedText) }

data AppUI = AppUI { getConn :: Connection
                   , getClients :: ClientsUI
                   , getProducts :: ProductsUI
                   , getInvoices :: InvoicesUI
                   , getUIs :: Collection }

mkClientsUI = do
  lst <- newList selAttr 1
  return $ ClientsUI lst

mkProductsUI = do
  lst <- newList selAttr 1
  return $ ProductsUI lst

mkInvoicesUI = do
  lst <- newList selAttr 1
  return $ InvoicesUI lst

mkAppUI conn = do
  clients <- mkClientsUI
  products <- mkProductsUI
  invoices <- mkInvoicesUI
  c <- newCollection
  return $ AppUI conn clients products invoices c

--------------------------------------------------------------------------------
-- | Clients View
getClientsList = getClList . getClients

clientListUI app = do
  populateClients app
  lstFooter <- plainText "add" --  delete"
  ui <- (((return (getClientsList app)) >>= bordered) <--> (return lstFooter))
  fg <- newFocusGroup
  _ <- addToFocusGroup fg (getClientsList app)

  header <- plainText "Clients" <++> hFill ' ' 1 <++> plainText companyName
  footer <- plainText "Clients | Products | Invoices" <++> hFill ' ' 1 <++> plainText version
  dui <- (return header) <--> (return ui) <--> (return footer)

  return $ (dui, fg)
  
  where populateClients app = do
          clients <- allClients $ getConn app
          mapM_ (\c -> addToList (getClientsList app) c =<< (plainText . T.pack $ Models.Client.name c)) clients

showClientAddUI app completion = do
  (caui, cafg) <- newClientAddDialog onAccept onCancel
  switchToAdd <- addToCollection (getUIs app) caui cafg
  switchToAdd
  where onAccept ws = \_ -> do
          client <- toClient ws
          client2 <- insertClient (getConn app) client
          addToList (getClientsList app) client2 =<< (plainText . T.pack $ Models.Client.name client2)
          completion
        onCancel _ = const completion
--------------------------------------------------------------------------------
-- | Products View
getProductsList = getProdList . getProducts
productListUI app = do
  populateProducts app
  lstFooter <- plainText "add" --  delete"
  ui <- (((return (getProductsList app)) >>= bordered) <--> (return lstFooter))
  fg <- newFocusGroup
  _ <- addToFocusGroup fg (getProductsList app)

  header <- plainText "Products" <++> hFill ' ' 1 <++> plainText companyName
  footer <- plainText "Clients | Products | Invoices" <++> hFill ' ' 1 <++> plainText version
  dui <- (return header) <--> (return ui) <--> (return footer)

  return $ (dui, fg)

  where populateProducts app = do
          products <- allProducts $ getConn app
          mapM_ (\c -> addToList (getProductsList app) c =<< (plainText . T.pack $ Models.Product.name c)) products

showProductAddUI app completion = do
  (paui, pafg) <- newProductAddDialog onAccept onCancel
  switchToAdd <- addToCollection (getUIs app) paui pafg
  switchToAdd
  where onAccept ws = \_ -> do
          product <- toProduct ws
          product2 <- insertProduct (getConn app) product
          addToList (getProductsList app) product2 =<< (plainText . T.pack $ Models.Product.name product2)
          completion
        onCancel _ = const completion
--------------------------------------------------------------------------------
-- | Invoices View
getInvoiceList = getInvList . getInvoices
invoiceListUI app = do
  populateInvoices app
  lstFooter <- plainText "add" --  delete"
  ui <- (((return (getInvoiceList app)) >>= bordered) <--> (return lstFooter))
  fg <- newFocusGroup
  _ <- addToFocusGroup fg (getInvoiceList app)

  header <- plainText "Invoices" <++> hFill ' ' 1 <++> plainText companyName
  footer <- plainText "Clients | Products | Invoices" <++> hFill ' ' 1 <++> plainText version
  dui <- (return header) <--> (return ui) <--> (return footer)

  return $ (dui, fg)

  where populateInvoices app = do
          invoices <- allInvoices $ getConn app
          mapM_ (\c -> addToList (getInvoiceList app) c =<< (plainText . T.pack $ Models.Invoice.name c)) invoices

showInvoiceAddUI app completion = do
  (iaui, iafg) <- newInvoiceAddDialog onAccept onCancel
  switchToAdd <- addToCollection (getUIs app) iaui iafg
  switchToAdd
  where onAccept ws = \_ -> do
          -- TODO: add invoice
          completion
        onCancel _ = const completion
--------------------------------------------------------------------------------
mainUI conn = do

  app <- mkAppUI conn

  (clui, clfg) <- clientListUI app
  (pdui, pdfg) <- productListUI app
  (inui, infg) <- invoiceListUI app

  switchToClientList <- addToCollection (getUIs app) clui clfg
  switchToProductList <- addToCollection (getUIs app) pdui pdfg
  switchToInvoiceList <- addToCollection (getUIs app) inui infg

--  (caui, cafg) <- clientAddUI (const $ switchToClientList) (const $ switchToClientList)
--  switchToClientAdd  <- addToCollection c caui cafg

  -- client list can be quit from with q and esc.
  clfg `onKeyPressed` \_ k mods ->
    case (k, mods) of
      (KASCII 'n', [MCtrl]) -> switchToProductList >> return True
      (KASCII 'p', [MCtrl]) -> switchToInvoiceList >> return True      
      (KASCII 'a', _) -> showClientAddUI app switchToClientList >> return True
      (KASCII 'q', _) -> shutdownUi >> return True
      _ -> return False

  pdfg `onKeyPressed` \_ k mods ->
    case (k, mods) of
      (KASCII 'p', [MCtrl]) -> switchToClientList >> return True
      (KASCII 'n', [MCtrl]) -> switchToInvoiceList >> return True      
      (KASCII 'a', _) -> showProductAddUI app switchToProductList >> return True
      (KASCII 'q', _) -> shutdownUi >> return True
      _ -> return False

  infg `onKeyPressed` \_ k mods ->
    case (k, mods) of
      (KASCII 'p', [MCtrl]) -> switchToProductList >> return True
      (KASCII 'n', [MCtrl]) -> switchToClientList >> return True
      (KASCII 'a', _) -> showInvoiceAddUI app switchToInvoiceList >> return True
      (KASCII 'q', _) -> shutdownUi >> return True
      _ -> return False

  runUi (getUIs app) defaultContext
