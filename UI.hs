{-# LANGUAGE OverloadedStrings #-}
module UI where

import Graphics.Vty hiding (Button,(<|>))
import Graphics.Vty.Widgets.All
import Database.SQLite.Simple (Connection)
import qualified Data.Text as T

import Models.Client
import Models.Product
import Models.Invoice
import Views.AddClient
import Views.AddProduct
import Views.Invoice (mkInvoiceController, UI(..), Controller(..))
import Control.Applicative ((<|>),(<$>),(<*>),liftA2)

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

{-
showInvoiceAddUI app completion = do
  (iaui, iafg) <- newInvoiceAddDialog (getConn app) (allProducts $ getConn app) onAccept onCancel
  switchToAdd <- addToCollection (getUIs app) iaui iafg
  switchToAdd
  where onAccept inv = \_ -> do
          -- TODO: add invoice
          insertInvoice (getConn app) =<< inv
          completion
        onCancel _ = const completion
-}
--------------------------------------------------------------------------------
-- invoice validation
validatePeriod :: Invoice -> Maybe T.Text
validatePeriod (Invoice { period = p }) =
  case p of
    (Period Nothing _) -> Just "From Date must be set!"
    (Period _ Nothing) -> Just "To Date must be set!"
    _ -> Nothing
validateDate :: Invoice -> Maybe T.Text
validateDate (Invoice { Models.Invoice.date = d }) =
  case d of
    Nothing -> Just "A Date must be set!"
    _ -> Nothing
validateDue :: Invoice -> Maybe T.Text
validateDue (Invoice { Models.Invoice.due = d }) =
  case d of
    Nothing -> Just "A Due Date must be set!"
    _ -> Nothing
validateItems :: Invoice -> Maybe T.Text
validateItems (Invoice { items = i }) =
  if length i == 0
  then Just "An Invoice should have at least one position"
  else Nothing
  
--------------------------------------------------------------------------------
mainUI conn = do

  app <- mkAppUI conn

  (clui, clfg) <- clientListUI app
  (pdui, pdfg) <- productListUI app
  (inui, infg) <- invoiceListUI app
  invoiceC <- do
    products <- (allProducts $ getConn app)
    clients  <- (allClients $ getConn app)
    mkInvoiceController products clients

  let invoiceUI = ui invoiceC
      
  -- add the containing widget
  -- first let's add a plaintext widget, so that we can show issues if they arise on pressing "ok"
  errorLabel <- plainText ""
  -- inject the error label below the invoiceUI widget but above the OK/Cancel buttons.
  dialogBody <- (return $ widget invoiceUI) <--> (return errorLabel)
  (dlg, dfg) <- newDialog dialogBody "New Invoice"
  mfg <- mergeFocusGroups (fg invoiceUI) dfg

  switchToClientList  <- addToCollection (getUIs app) clui clfg
  switchToProductList <- addToCollection (getUIs app) pdui pdfg
  switchToInvoiceList <- addToCollection (getUIs app) inui infg
  switchToInvoiceView  <- addToCollection (getUIs app) (dialogWidget dlg) mfg


  let switchToInvoice inv = vbind invoiceC inv >> switchToInvoiceView
      switchToNewInvoice = switchToInvoice mkNewInvoice
      clearErrorAndSwitchToInvoiceList = setText errorLabel "" >> switchToInvoiceList


  (getInvList (getInvoices app)) `onItemActivated` \(ActivateItemEvent _ inv _) -> switchToInvoice inv
  
  dlg `onDialogAccept` \_ -> do
    -- check the invoice
    inv <- vreturn invoiceC
    case validatePeriod inv <|> validateDate inv <|> validateDue inv <|> validateItems inv of
      Just t -> setText errorLabel t
      Nothing -> do
        updateOrInsertInvoice (getConn app) inv
        clearErrorAndSwitchToInvoiceList
  dlg `onDialogCancel` \_ -> clearErrorAndSwitchToInvoiceList

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
      (KASCII 'a', _) -> switchToNewInvoice >> return True
      (KASCII 'q', _) -> shutdownUi >> return True
      _ -> return False

  runUi (getUIs app) defaultContext
