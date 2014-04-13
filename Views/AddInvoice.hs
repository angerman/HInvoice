{-# LANGUAGE OverloadedStrings #-}
module Views.AddInvoice (--toInvoice,
  newInvoiceAddDialog) where
import Graphics.Vty
import Graphics.Vty.Widgets.All (Widget, Edit,
                                 editWidget,getEditText,
                                 plainText,
                                 centered,
                                 (<-->),(<++>),on,List,newList,FormattedText,
                                 hFill,hLimit,
                                 boxFixed,
                                 withPadding,padRight,
                                 newFocusGroup,addToFocusGroup,mergeFocusGroups)
import Graphics.Vty.Widgets.Dialog
import qualified Data.Text as T
import Control.Applicative ((<$>),(<*>))
import Control.Monad (liftM)
import Models.Product (Product)
{-| sketch

.-------- New Invoice -------------------------------------------------------------------.
| .--- client ---.                                                                       |
| | <name>       | [ select client ]                                                     |
| | <street>     |                                                                       |
| | <zip> <city> |                                                                       |
| | <country>    |                                                                       |
| '--------------'                                                                       |
|                                                                                        |
|  Date: [___ __ __] Due: [____ __ __]                                                   |
|                                                                                        |
|  Period                                                                                |
|  From: [___ __ __] to: [____ __ __]                                                    |
|                                                                                        |
|  Products                                                                              |
|  | [______] [Product name (enter to chooe a different product)]   [sum prod x quant] | |
|  |                                                                                   | |
|  |                                                                                   | |
|  use + / - to add / remove a product                               Subtotal: XXXXXXX   |
|                                                                                        |
|                                                            vat: [________]%  XXXXXXX   |
|                                                       discount: [________]%  XXXXXXX   |
|                                                       cashback: [________]%  XXXXXXX   |
|                                                                                        |
|                                                                      total:  XXXXXXX   |
|                                                                                        |
|                            [      OK      ] [   Cancel   ]                             |
'----------------------------------------------------------------------------------------'
- At first The client will have to be entered by his id.
- Later the more fancy UI will be build.
-}

selAttr = black `on` yellow

data AddInvoiceUI =
  AddInvoiceUI { client :: Widget Edit
               , date :: Widget Edit
               , due :: Widget Edit
               , from :: Widget Edit
               , to :: Widget Edit
               , products :: Widget (List (Int, Product) FormattedText)
               , vat :: Widget Edit
               , discount :: Widget Edit
               , cashback :: Widget Edit }
  
mkAddInvoiceUI = do
  client    <- editWidget
  date      <- editWidget
  due       <- editWidget
  from      <- editWidget
  to        <- editWidget
  products  <- newList selAttr 1
  vat       <- editWidget
  discount  <- editWidget
  cashback  <- editWidget
  return $ AddInvoiceUI client date due from to products vat discount cashback

invoiceAddUI st = do
  fg <- newFocusGroup
  mapM_ (addToFocusGroup fg) $ [client', date', due', from', to', vat', discount', cashback']

  ui <- (plainText "Client:") <--> (boxFixed 5 1 client') <-->
        ((plainText "Date:") <++> (boxFixed 11 1 date') <++>
         (plainText " Due:") <++> (boxFixed 11 1 due')) <-->
        (plainText "Period") <-->
        ((plainText "From:") <++> (boxFixed 11 1 from') <++>
         (plainText " to:")  <++> (boxFixed 11 1 to')) <-->
        (return products') <-->
        ((plainText "Use + / - to add / remove a product") <++> (plainText "SubTotal: XXXXXXX"))
        <-->
        ((plainText "vat:") <++> (boxFixed 8 1 vat') <++> (plainText "% XXXXXXXX")) <-->
        ((plainText "discount:") <++> (boxFixed 8 1 discount') <++> (plainText "% XXXXXXXX")) <-->
        ((plainText "cashback:") <++> (boxFixed 8 1 cashback') <++> (plainText "% XXXXXXXX"))
        
  return (ui, fg)
  where AddInvoiceUI client' date' due' from' to' products' vat' discount' cashback'= st

newInvoiceAddDialog onAccept onCancel = do
  header <- plainText "Add Invoice" <++> hFill ' ' 1 <++> plainText "HInvoice"
  footer <- plainText "Clients | Products | Invoices" <++> hFill ' ' 1 <++> plainText "v0.1"

  st <- mkAddInvoiceUI
  (ui, fg) <- invoiceAddUI st
  (dlg, dfg) <- newDialog ui "New Invoice"

  mfg <- mergeFocusGroups fg dfg
  mui <- (return header) <--> (centered =<< hLimit 55 =<< (return (dialogWidget dlg))) <--> (return footer)

  dlg `onDialogAccept` onAccept st
  dlg `onDialogCancel` onCancel st

  return (mui, mfg)
