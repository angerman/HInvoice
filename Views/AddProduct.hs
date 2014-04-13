{-# LANGUAGE OverloadedStrings #-}
module Views.AddProduct (toProduct, newProductAddDialog) where 

import Graphics.Vty.Widgets.All (Widget, Edit,
                                 editWidget,getEditText,
                                 plainText,
                                 centered,
                                 (<-->),(<++>),
                                 hFill,hLimit,
                                 boxFixed,
                                 withPadding,padRight,padLeft,
                                 newFocusGroup,addToFocusGroup,mergeFocusGroups)
import Graphics.Vty.Widgets.Dialog
import qualified Data.Text as T

import Control.Monad (liftM)
import Models.Product (Product, mkProduct)

data AddProductUI =
  AddProductUI { name :: Widget Edit
               , price :: Widget Edit
               , base :: Widget Edit
               , currency :: Widget Edit }

mkAddProductUI = do
  name     <- editWidget
  price    <- editWidget
  base     <- editWidget
  currency <- editWidget
  return $ AddProductUI name price base currency

toProduct st = do
  name'     <- get $ name st
  price'    <- liftM read . get $ price st
  base'     <- liftM read . get $ base st
  currency' <- get $ currency st
  return $ mkProduct 0 name' price' base' currency'
  where get = liftM T.unpack . getEditText

productAddUI st = do
  fg <- newFocusGroup
  mapM_ (addToFocusGroup fg) $ [name', currency', price', base']

  ui <- (plainText "Product:") <--> (boxFixed 40 1 name') <-->
        (plainText "Currency, Price / Base") <-->
        (((boxFixed 4 1 currency') >>= withPadding (padRight 2))
         <++>
         (boxFixed 28 1 price')
         <++>
         ((boxFixed 4 1 base') >>= withPadding (padLeft 2)))

  return (ui, fg)
  where AddProductUI name' price' base' currency' = st
        
newProductAddDialog onAccept onCancel = do
  header <- plainText "Add Product" <++> hFill ' ' 1 <++> plainText "Invoice Terminal"
  footer <- plainText "Clients | Products | Invoices" <++> hFill ' ' 1 <++> plainText "v0.1"

  st <- mkAddProductUI
  (ui, fg) <- productAddUI st
  (dlg, dfg) <- newDialog ui "New Product"

  mfg <- mergeFocusGroups fg dfg
  mui <- (return header) <--> (centered =<< hLimit 55 =<< (return (dialogWidget dlg))) <--> (return footer)

  dlg `onDialogAccept` onAccept st
  dlg `onDialogCancel` onCancel st

  return (mui, mfg)
