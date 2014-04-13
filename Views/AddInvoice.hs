{-# LANGUAGE OverloadedStrings #-}
module Views.AddInvoice (--toInvoice,
  newInvoiceAddDialog) where
import Graphics.Vty
import Graphics.Vty.Widgets.All
import Graphics.Vty.Widgets.Dialog
import qualified Data.Text as T
import Control.Applicative ((<$>),(<*>))
import Control.Monad (liftM)
import Models.Product (Product(..))
import Text.Read (readMaybe)
import Data.Decimal
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
               , products :: Widget (List String (Box (Box (Box (VFixed (HFixed Edit))
                                                       (VFixed (List Product FormattedText)))
                                                       HFill)
                                                  FormattedText))
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
  mapM_ (addToFocusGroup fg) $ [client', date', due', from', to']
  _ <-   addToFocusGroup fg     products'
  mapM_ (addToFocusGroup fg) $ [vat', discount', cashback']

  prodList <- vBorder <++> (return products') <++> vBorder
  ui <- (((plainText "Client:") <++> (boxFixed 5 1 client')) >>= withPadding (padBottom 1)) <-->
        (((plainText "Date:") <++> (boxFixed 11 1 date') <++>
          (plainText " Due:") <++> (boxFixed 11 1 due')) >>= withPadding (padBottom 1)) <-->
        (plainText "Period") <-->
        (((plainText "From:") <++> (boxFixed 11 1 from') <++>
         (plainText " to:")  <++> (boxFixed 11 1 to')) >>= withPadding (padBottom 1))  <-->
        ((plainText "Quantity ") <++> (plainText "Product") <++> (hFill ' '1) <++> (plainText "Total")) <-->
        (vFixed 8 prodList) <-->
        ((plainText "Use + / - to add / remove a product") <++> (hFill ' ' 1) <++> (plainText "SubTotal: XXXXXXX"))
        <-->
        ((hFill ' ' 1) <++> (plainText "vat:") <++> (boxFixed 8 1 vat') <++> (plainText "% XXXXXXX")) <-->
        ((hFill ' ' 1) <++> (plainText "discount:") <++> (boxFixed 8 1 discount') <++> (plainText "% XXXXXXX")) <-->
        ((hFill ' ' 1) <++> (plainText "cashback:") <++> (boxFixed 8 1 cashback') <++> (plainText "% XXXXXXX")) <-->
        (((hFill ' ' 1) <++> (plainText "total: XXXXXXX")) >>= withPadding (padTop 1))

  -- padd the invoice ui with left +2 and right +2 char.
  paddedUI <- (return ui) >>= withPadding (padLeft 2) >>= withPadding (padRight 2)
  
  return (paddedUI, fg)
  where AddInvoiceUI client' date' due' from' to' products' vat' discount' cashback'= st

-- | returns the decimal representation of the text or the first argument (default)
toDecimal :: Decimal -> T.Text -> Decimal
toDecimal d = maybe d id . readMaybe . T.unpack

newInvoiceAddDialog prods onAccept onCancel = do
  header <- plainText "Add Invoice" <++> hFill ' ' 1 <++> plainText "HInvoice"
  footer <- plainText "Clients | Products | Invoices" <++> hFill ' ' 1 <++> plainText "v0.1"

  st <- mkAddInvoiceUI
  (ui, fg) <- invoiceAddUI st
  (dlg, dfg) <- newDialog ui "New Invoice"

  mfg <- mergeFocusGroups fg dfg
  mui <- (return header) <--> (centered =<< hLimit 75 =<< (return (dialogWidget dlg))) <--> (return footer)

  let addProduct l = do
        e <- editWidget
        setEditText e $ T.pack "1"
        lst <- newList selAttr 1
        t <- plainText ""
        p <- prods -- products
        -- add products to lst
        let formatProductPrice q p = (show $ q * (price p)) ++ " " ++ (currency p)
            updatePriceFormatted q p = setText t . T.pack $ formatProductPrice q p
            updatePrice = do
              q <- getEditText e
              r <- getSelected lst -- the result (idx, (a, b))
              case r of
                Just (_, (p, _)) -> updatePriceFormatted (toDecimal 1 q) p
                Nothing -> setText t . T.pack $ ""
        -- on selection or quantity change, update the price.
        lst `onSelectionChange` \_ -> updatePrice
        e `onChange` \_ -> updatePrice

        -- on C-p, C-n, cycle the products
        e `onKeyPressed` \_ k m ->
          case (k, m) of
            (KASCII 'p', [MCtrl]) -> scrollUp lst >> return True
            (KASCII 'n', [MCtrl]) -> scrollDown lst >> return True
            _ -> return False

        -- populate the product list, this should trigger the update event as well.
        mapM_ (\p -> addToList lst p =<< (plainText . T.pack $ Models.Product.name p)) p

        ((boxFixed 5 1 e) <++> (vFixed 1 lst) <++> hFill ' ' 1 <++> (return t))  >>= addToList l "unused"
      remProduct = return ()
  (products st) `onKeyPressed` \l k _ -> do
    case k of
      (KASCII '+') -> addProduct l >> return True
      (KASCII '-') -> remProduct >> return True
      _ -> return False

  dlg `onDialogAccept` onAccept st
  dlg `onDialogCancel` onCancel st

  return (mui, mfg)
