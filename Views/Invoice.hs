{-# LANGUAGE OverloadedStrings #-}
module Views.Invoice where
import Graphics.Vty
import Graphics.Vty.Widgets.All
import Models.Product (Product(..))
import Models.Invoice (mkInvoice, ProductItem(..), Period(..), Invoice(..))
import Control.Monad (liftM, forM_, replicateM)
import Control.Applicative ((<$>))
import qualified ListUtils as LU
import qualified Data.Text as T
import Data.IORef
import Data.Decimal
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)

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
type ProductItemW = Box (Box (Box (Box (VFixed (HFixed Edit))
                                   (VFixed (List Product FormattedText)))
                              HFill)
                         FormattedText)
                    Edit
-- | Next: The Product list, is a List of ProductItems.
type ProductListW = List (IORef ProductItem) ProductItemW
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

toDecimal :: Decimal -> T.Text -> Decimal
toDecimal d = fromMaybe d . readMaybe . T.unpack
                                    
-- mkUI :: IO (UI a)
mkInvoiceUI = do
  -- create the edit widget
  client:date:due:from:to:vat:discount:cashback:_ <- replicateM 8 editWidget
  -- create the plain text widgets (Labels)
  subtotal:totalVAT:totalDiscount:total:_ <- replicateM 4 $ plainText "0.00 XXX"
  products <- newList selAttr 2
  -- setup the focus group
  fg <- newFocusGroup
  mapM_ (addToFocusGroup fg) [client, date, due, from, to]
  _ <- addToFocusGroup fg products
  mapM_ (addToFocusGroup fg) [discount, vat]
  -- build the widget
  widget <- (((plainText "Client:" <++> boxFixed 5 1 client) >>= withPadding (padBottom 1)) <-->
            ((plainText "Date:" <++> boxFixed 11 1 date <++>
              plainText " Due:" <++> boxFixed 11 1 due) >>= withPadding (padBottom 1)) <-->
            plainText "Period" <-->
            ((plainText "From:" <++> boxFixed 11 1 from <++>
              plainText " to:"  <++> boxFixed 11 1 to) >>= withPadding (padBottom 1))  <-->
            ((plainText " Qty" >>= boxFixed 6 1) <++> plainText "Product" <++> hFill ' '1 <++> plainText "Total ") <-->
        (vFixed 8 =<< (vBorder <++> return products <++> vBorder))
        <-->
        (plainText "+/- to add / remove products" <++>
         hFill ' ' 1 <++>
         plainText "Subtotal: " <++>
         boxFixed 10 1 subtotal)
        <-->
        (hFill ' ' 1 <++>
         plainText "Discount:" <++>
         boxFixed 4 1 discount <++>
         plainText "% " <++>
         boxFixed 10 1 totalDiscount)
        <-->
        (hFill ' ' 1 <++>
         plainText "VAT:" <++>
         boxFixed 4 1 vat <++>
         plainText "% " <++>
         boxFixed 10 1 totalVAT)
        <-->
        -- ((hFill ' ' 1) <++>
        --  (plainText "cashback:") <++>
        --  (boxFixed 8 1 cashback') <++>
        --  (plainText "%") <++>
        --  (boxFixed 7 1 totalCashback'))
        -- <-->
        ((hFill ' ' 1 <++> plainText "Total: " <++> boxFixed 10 1 total)
         >>= withPadding (padTop 1))) >>= withPadding (padLeft 2) >>= withPadding (padRight 2)
  -- return the invoiceUI
  return $ InvoiceUI client date due from to products vat discount cashback subtotal totalVAT totalDiscount total widget fg

mkProductItem products product = do
  ref <- newIORef product
  qtyW <- editWidget
  -- populate the product list with the products
  productW <- newList selAttr 1
  forM_ products $ \p@Product{name=n} ->
    plainText (T.pack n) >>= addToList productW p
    
  totalW <- plainText ""
  commentW <- editWidget

  fg <- newFocusGroup
  _ <- addToFocusGroup fg qtyW
  _ <- addToFocusGroup fg commentW

  -- update logic
  let updateTotal = do
        q <- fromIntegral . qty <$> readIORef ref
        p <- (price . Models.Invoice.product) <$> readIORef ref
        c <- (currency . Models.Invoice.product) <$> readIORef ref
        setText totalW $ T.pack $ show (p * q) ++ " " ++ c
  -- key events
  qtyW `onKeyPressed` \w k m -> do
    foc <- focused <~ w
    if foc
       then case (k,m) of
         (KASCII 'n', [MCtrl]) -> focus commentW >> return True
         (KASCII '\8220', []) -> scrollUp   productW >> return True -- Opt + [
         (KASCII '\8216', []) -> scrollDown productW >> return True -- Opt + ]
         _ -> return False
      else handleKeyEvent fg k m
  commentW `onKeyPressed` \w k m -> do
    foc <- focused <~ w
    if foc
       then case (k,m) of
         (KASCII 'p', [MCtrl]) -> focus qtyW >> return True
         (KASCII '\8220', []) -> scrollUp   productW >> return True -- Opt + [
         (KASCII '\8216', []) -> scrollDown productW >> return True -- Opt + ]
         _ -> return False
      else handleKeyEvent fg k m

  -- text events
  qtyW `onChange` \t -> do
    modifyIORef' ref $ \x -> x{ qty = (fromMaybe 1 . readMaybe . T.unpack) t }
    updateTotal
  commentW `onChange` \t -> modifyIORef' ref $ \x -> x{ comment = T.unpack t }

  -- list events
  productW `onSelectionChange` \(SelectionOn _ p _) -> do
    modifyIORef' ref $ \x -> x{ Models.Invoice.product = p }
    updateTotal

  -- set initial values
  readIORef ref >>= \x -> setEditText qtyW (T.pack . show . qty $ x)
  readIORef ref >>= \x -> setEditText commentW (T.pack . comment $ x)

  widget <- (boxFixed 5 1 qtyW <++> vFixed 1 productW <++> hFill ' ' 1 <++> return totalW)
            <--> return commentW

  return (ref, widget)

mkInvoiceController prods@(p0:_) = do
  -- create the backing ref
  -- XXX: I hate this.  Having to instantiate an "empty" Invoice is kinda stupid.
  ref <- newIORef (Invoice undefined undefined undefined undefined undefined undefined []
                   undefined undefined undefined)
  -- crete the UI
  invoiceUI <- mkInvoiceUI
  -- setup UI interactions
  e <- editWidget
  t <- plainText "foo"
  let pct = (/100) . toDecimal 0
      getVAT = liftM pct . getEditText $ Views.Invoice.vat invoiceUI
      getDiscount = liftM (negate . pct) . getEditText $ Views.Invoice.discount invoiceUI
      
  products invoiceUI `onKeyPressed` \l k _ ->
    case k of
      (KASCII '+') -> mkProductItem prods (ProductItem 1 p0 "") >>= uncurry (addToList l) >> return True
      (KASCII '-') -> LU.dropSelected l >> return True
      _ -> return False
  -- setup the bindings
  let bind' inv = do
        writeIORef ref inv
        return ()
  return $ InvoiceController ref invoiceUI bind' (readIORef ref)

-- Test
main = do
  let products = [Product undefined "Product A" 75.00 1 "EUR"
                 ,Product undefined "Product B" 35.00 2 "EUR"]
  invoiceC <- mkInvoiceController products
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
