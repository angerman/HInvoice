{-# LANGUAGE OverloadedStrings #-}
module Views.Invoice where
import Graphics.Vty
import Graphics.Vty.Widgets.All
import Models.Product (Product(..))
import Models.Invoice (mkInvoice, ProductItem(..), Period(..), Invoice(..))
import ListUtils (dropSelected)
import Data.IORef

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
|  |          [Comment                                          ]                      | |
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

selAttr = black `on` yellow -- XXX This should probably to into a separate (Configuration file)

-- | Let us first define the UI elements of this view. A suffix W will indicat it's
--   a widget type.
-- | First: the ProductListItem: [Qty (Edit)] [Product Name (List)] [Sum (Text)]
type ProductItemW = Box (Box (Box (VFixed (HFixed Edit))
                              (VFixed (List Product FormattedText)))
                         HFill)
                    FormattedText
-- | Next: The Product list, is a List of ProductItems.
type ProductListW = List ProductItem ProductItemW
data UI a = InvoiceUI { client   :: Widget Edit
                      , date     :: Widget Edit
                      , due      :: Widget Edit
                      , from     :: Widget Edit
                      , to       :: Widget Edit
                      , products :: Widget ProductListW
                      , vat      :: Widget Edit
                      , discount :: Widget Edit
                      , cashback :: Widget Edit
                    -- labels
                      , subtotal :: Widget FormattedText
                      , totalVAT :: Widget FormattedText
                      , totalDiscount :: Widget FormattedText
                      , total    :: Widget FormattedText
                      , widget   :: Widget a
                      , fg       :: Widget FocusGroup}
          | Empty

data Controller a = InvoiceController { model :: IORef Invoice
                                      , ui :: UI a
                                      , vbind :: Invoice -> IO ()
                                      , vreturn :: IO Invoice }
                                    
-- mkUI :: IO (UI a)
mkInvoiceUI = do
  -- create the edit widget
  client:date:due:from:to:vat:discount:cashback:_ <- (sequence . take 8 . repeat) editWidget
  -- create the plain text widgets (Labels)
  subtotal:totalVAT:totalDiscount:total:_ <- sequence . take 4 . repeat $ plainText "0.00 XXX"
  products <- newList selAttr 1
  -- setup the focus group
  fg <- newFocusGroup
  mapM_ (addToFocusGroup fg) [client, date, due, from, to]
  _ <- addToFocusGroup fg products
  mapM_ (addToFocusGroup fg) [discount, vat]
  -- build the widget
  widget <- ((((plainText "Client:") <++> (boxFixed 5 1 client)) >>= withPadding (padBottom 1)) <-->
            (((plainText "Date:") <++> (boxFixed 11 1 date) <++>
              (plainText " Due:") <++> (boxFixed 11 1 due)) >>= withPadding (padBottom 1)) <-->
            (plainText "Period") <-->
            (((plainText "From:") <++> (boxFixed 11 1 from) <++>
              (plainText " to:")  <++> (boxFixed 11 1 to)) >>= withPadding (padBottom 1))  <-->
            (((plainText " Qty") >>= boxFixed 6 1) <++> (plainText "Product") <++> (hFill ' '1) <++> (plainText "Total ")) <-->
        (vFixed 8 =<< (vBorder <++> return products <++> vBorder))
        <-->
        ((plainText "+/- to add / remove products") <++>
         (hFill ' ' 1) <++>
         (plainText "Subtotal: ") <++>
         (boxFixed 10 1 subtotal))
        <-->
        ((hFill ' ' 1) <++>
         (plainText "Discount:") <++>
         (boxFixed 4 1 discount) <++>
         (plainText "% ") <++>
         (boxFixed 10 1 totalDiscount))
        <-->
        ((hFill ' ' 1) <++>
         (plainText "VAT:") <++>
         (boxFixed 4 1 vat) <++>
         (plainText "% ") <++>
         (boxFixed 10 1 totalVAT))
        <-->
        -- ((hFill ' ' 1) <++>
        --  (plainText "cashback:") <++>
        --  (boxFixed 8 1 cashback') <++>
        --  (plainText "%") <++>
        --  (boxFixed 7 1 totalCashback'))
        -- <-->
        (((hFill ' ' 1) <++> (plainText "Total: ") <++> (boxFixed 10 1 total))
         >>= withPadding (padTop 1))) >>= withPadding (padLeft 2) >>= withPadding (padRight 2)
  -- return the invoiceUI
  return $ InvoiceUI client date due from to products vat discount cashback subtotal totalVAT totalDiscount total widget fg

mkInvoiceController = do
  -- create the backing ref
  ref <- newIORef (Invoice undefined undefined undefined undefined undefined undefined []
                   undefined undefined undefined)
  -- crete the UI
  invoiceUI <- mkInvoiceUI
  -- setup UI interactions
  e <- editWidget
  lst <- newList selAttr 1
  t <- plainText "foo"
  products invoiceUI `onKeyPressed` \l k _ -> do
    case k of
      (KASCII '+') -> ((boxFixed 5 1 e) <++> (vFixed 1 lst) <++> hFill ' ' 1 <++> (return t)) >>= addToList l undefined >> return True
      (KASCII '-') -> dropSelected l >> return True
      _ -> return False
  -- setup the bindings
  let bind' inv = do
        writeIORef ref inv
        return ()
  return $ InvoiceController ref invoiceUI bind' (readIORef ref)

-- Test
main = do
  invoiceC <- mkInvoiceController
  let invoiceUI = ui invoiceC
      
  -- add the containing widget
  (dlg, dfg) <- newDialog (widget invoiceUI) "New Invoice"
  mfg <- mergeFocusGroups (fg invoiceUI) dfg

  c <- newCollection
  _ <- addToCollection c (dialogWidget dlg) mfg
  mfg `onKeyPressed` \_ k _ -> case k of
    KASCII 'q' -> shutdownUi >> return True
    _ -> return False
  runUi c defaultContext
