{-# LANGUAGE OverloadedStrings #-}
module Views.Invoice where
import Graphics.Vty hiding (Button)
import Graphics.Vty.Widgets.All
import Models.Product (Product(..))
import Models.Invoice (mkNewInvoice, ProductItem(..), productItemTotal, Period(..), Invoice(..))
import Models.Client (Client(..))
import Control.Monad (liftM, forM_, replicateM)
import Control.Applicative ((<$>),(<*>))
import qualified ListUtils as LU
import qualified Data.Text as T
import Data.IORef
import Data.Decimal
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe, catMaybes)
import Data.Time.Calendar (addDays)

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
data UI a = InvoiceUI { clientList :: Widget (List Client FormattedText)
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
                      , msgW     :: Widget FormattedText
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
  date:due:from:to:vat:discount:cashback:_ <- replicateM 7 editWidget
  clientList <- newList selAttr 1
  -- create the plain text widgets (Labels)
  subtotal:totalVAT:totalDiscount:total:_ <- replicateM 4 $ plainText "0.00 XXX"
  msgW <- plainText ""
  products <- newList selAttr 2
  -- setup the focus group
  fg <- newFocusGroup
  addToFocusGroup fg clientList
  mapM_ (addToFocusGroup fg) [date, due, from, to]
  _ <- addToFocusGroup fg products
  mapM_ (addToFocusGroup fg) [discount, vat]
  -- build the widget
  widget <- (((plainText "Client:" <++> vFixed 1 clientList) >>= withPadding (padBottom 1)) <-->
            ((plainText "Date:" <++> boxFixed 11 1 date <++>
              plainText " Due:" <++> boxFixed 11 1 due) >>= withPadding (padBottom 1)) <-->
            plainText "Period" <-->
            ((plainText "From:" <++> boxFixed 11 1 from <++>
              plainText " to:"  <++> boxFixed 11 1 to) >>= withPadding (padBottom 1))  <-->
            ((plainText " Qty" >>= boxFixed 6 1) <++> plainText "Product" <++> hFill ' '1 <++> plainText "Total ") <-->
        (vFixed 8 =<< (vBorder <++> return products <++> vBorder))
        <-->
        (
        plainText "+ / - to add / remove products\nC-[ / C-] to change product\nC-p / C-n to cycle "
        <++>
        ((hFill ' ' 1 <++>
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
         boxFixed 10 1 totalVAT)))
        <-->
        -- ((hFill ' ' 1) <++>
        --  (plainText "cashback:") <++>
        --  (boxFixed 8 1 cashback') <++>
        --  (plainText "%") <++>
        --  (boxFixed 7 1 totalCashback'))
        -- <-->
        (((hFill ' ' 1 <++> plainText "Total: " <++> boxFixed 10 1 total)
         <--> (return msgW))
         >>= withPadding (padTop 1))) >>= withPadding (padLeft 2) >>= withPadding (padRight 2)
  -- return the invoiceUI
  return $ InvoiceUI clientList date due from to products vat discount cashback subtotal totalVAT totalDiscount total widget msgW fg

mkProductItem products product changeCB = do
  ref <- newIORef product
  qtyW <- editWidget
  -- populate the product list with the products
  productW <- newList selAttr 1
  forM_ products $ \p@Product{Models.Product.name=n} ->
    plainText (T.pack n) >>= addToList productW p

  totalW <- plainText ""
  commentW <- editWidget

  fg <- newFocusGroup
  _ <- addToFocusGroup fg qtyW
  _ <- addToFocusGroup fg commentW

  -- update logic
  let updateTotal = do
        t <- productItemTotal <$> readIORef ref
        c <- (currency . Models.Invoice.product) <$> readIORef ref
        setText totalW $ T.pack $ show t ++ " " ++ c
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
    -- call the callback
    changeCB
  
  commentW `onChange` \t -> do
    modifyIORef' ref $ \x -> x{ comment = T.unpack t }
    -- call the callback
    changeCB

  -- list events
  productW `onSelectionChange` \(SelectionOn _ p _) -> do
    modifyIORef' ref $ \x -> x{ Models.Invoice.product = p }
    updateTotal >> changeCB

  -- set initial values
  readIORef ref >>= \x -> setEditText qtyW (T.pack . show . qty $ x)
  readIORef ref >>= \x -> setEditText commentW (T.pack . comment $ x)
  readIORef ref >>= \x -> do
    let prod = Models.Invoice.product x in do
      pos <- head' <$> productW `getListIndicesOf` prod
      productW `setSelected` (fromMaybe 0 pos)

  widget <- (boxFixed 5 1 qtyW <++> vFixed 1 productW <++> hFill ' ' 1 <++> return totalW)
            <--> return commentW

  return (ref, widget)

mkInvoiceController prods@(p0:_) clients = do
  -- create the backing ref
  -- XXX: I hate this.  Having to instantiate an "empty" Invoice is kinda stupid.
  let mkNewInvoiceRef :: IO (IORef Invoice)
      mkNewInvoiceRef = newIORef $ mkNewInvoice
  ref <- mkNewInvoiceRef
                   
  -- crete the UI
  invoiceUI <- mkInvoiceUI

  forM_ clients $ \c@Client{Models.Client.name=n} ->
    plainText (T.pack n) >>= addToList (clientList invoiceUI) c

  (clientList invoiceUI) `onSelectionChange` \e -> case e of
    SelectionOn _ c _ -> modifyIORef' ref $ \x -> x{Models.Invoice.client = c }
    SelectionOff -> return ()

  -- helper
  let pct = (/100) . toDecimal 0
      getVAT = liftM pct . getEditText $ Views.Invoice.vat invoiceUI
      getDiscount = liftM (negate . pct) . getEditText $ Views.Invoice.discount invoiceUI
      allItems = do
        len <- getListSize $ products invoiceUI
        items <- catMaybes <$> mapM (getListItem (products invoiceUI)) [0..len]
        mapM (readIORef . fst) items
      computeProductsTotal = do
        pis <- allItems
        return $ sum (map productItemTotal pis)
      productsCurrency = do
        -- we only check the currency for the first product.
        pis <- allItems
        case pis of
          pi:_ -> return $ currency (Models.Invoice.product pi)
          _ -> return ""
        
      computeDiscount = (*) <$> getDiscount <*> computeProductsTotal
      computeVAT      = (\d v t -> v * (1+d) * t) <$> getDiscount <*> getVAT <*> computeProductsTotal      
      computeTotal    = (\d v t -> (1+v) * (1+d) * t) <$> getDiscount <*> getVAT <*> computeProductsTotal

      formatPrice p c = show p ++ " " ++ c
      updateVAT =
        formatPrice <$> computeVAT <*> productsCurrency
        >>= setText (totalVAT invoiceUI) . T.pack
      updateDiscount =
        formatPrice <$> computeDiscount <*> productsCurrency
        >>= setText (totalDiscount invoiceUI) . T.pack
      updateSubtotal =
        formatPrice <$> computeProductsTotal <*> productsCurrency
        >>= setText (subtotal invoiceUI) . T.pack
      updateTotal =
        formatPrice <$> computeTotal <*> productsCurrency
        >>= setText (total invoiceUI) . T.pack

      updateItems :: [ProductItem] -> Invoice -> Invoice
      updateItems i p = p{Models.Invoice.items = i}
      updateRef = do
        itms <- allItems
        modifyIORef' ref (updateItems itms)
        return ()
  
      updateAll = updateRef >> updateSubtotal >> updateDiscount >> updateVAT >> updateTotal
  
  -- setup UI interactions
  products invoiceUI `onKeyPressed` \l k _ ->
    case k of
      (KASCII '+') -> mkProductItem prods (ProductItem 1 p0 "") updateAll >>= uncurry (addToList l) >> return True
      (KASCII '-') -> LU.dropSelected l >> return True
      _ -> return False
  -- list events
  products invoiceUI `onItemAdded` const updateAll
  products invoiceUI `onItemRemoved` const updateAll
  -- key events.
  Views.Invoice.vat invoiceUI `onChange` const updateAll
  Views.Invoice.discount invoiceUI `onChange` const updateAll
  -- text events
  Views.Invoice.date invoiceUI `onChange` \t ->
    case readMaybe (T.unpack t) of
      Just d -> do
        modifyIORef' ref $ \x -> x{ Models.Invoice.date = Just d }
        setEditText (Views.Invoice.due invoiceUI) $ T.pack . show $ addDays 14 d
      Nothing -> return ()
  Views.Invoice.due invoiceUI `onChange` \t ->
    modifyIORef' ref $ \x -> x{ Models.Invoice.due = readMaybe (T.unpack t) }
  Views.Invoice.from invoiceUI `onChange` \t ->
    modifyIORef' ref $ \x@(Invoice{ period = (Period _ b) }) ->
    x{period = Period (readMaybe (T.unpack t)) b}
  Views.Invoice.to invoiceUI `onChange` \t ->
    modifyIORef' ref $ \x@(Invoice{ period = (Period a _) }) ->
    x{period = (Period a (readMaybe (T.unpack t))) }
  -- set default values
  _ <- setEditText (Views.Invoice.discount invoiceUI) $ T.pack (show 0)
  _ <- setEditText (Views.Invoice.vat invoiceUI) $ T.pack (show 19)
  
  -- setup the bindings
  let bind' inv = do
        writeIORef ref inv
        -- set client
        case (client inv) of
          EmptyClient -> (clientList invoiceUI) `setSelected` 0
          c -> do
            pos <- fromMaybe 0 <$> (getFirstListIndexOf (clientList invoiceUI) c)
            (clientList invoiceUI) `setSelected` pos
        -- set Date, Due
        setEditText (Views.Invoice.date invoiceUI) $ fromMaybe "" $ T.pack . show <$> Models.Invoice.date inv
        setEditText (Views.Invoice.due  invoiceUI) $ fromMaybe "" $ T.pack . show <$> Models.Invoice.due  inv
        -- set Period
        let (Period f t) = (Models.Invoice.period inv)
        setEditText (Views.Invoice.from invoiceUI) $ fromMaybe "" $ T.pack . show <$> f
        setEditText (Views.Invoice.to   invoiceUI) $ fromMaybe "" $ T.pack . show <$> t
        clearList (products invoiceUI)
        forM_ (Models.Invoice.items inv) $ \item -> mkProductItem prods item updateAll >>= uncurry (addToList (products invoiceUI))
--        setText (msgW invoiceUI) $ T.pack . show $ length (Models.Invoice.items inv)
        setEditText (Views.Invoice.vat      invoiceUI) $ T.pack . show $ 100 * Models.Invoice.vat      inv
        setEditText (Views.Invoice.discount invoiceUI) $ T.pack . show $ 100 * Models.Invoice.discount inv
        -- focus!
        focus (clientList invoiceUI)
        return ()  
  
  return $ InvoiceController ref invoiceUI bind' (readIORef ref)

-- Test
main = do
  let products = [Product (-1) "Product A" 75.00 1 "EUR"
                 ,Product (-1) "Product B" 35.00 2 "EUR"]
  invoiceC <- mkInvoiceController products []
  let invoiceUI = ui invoiceC
      
  -- add the containing widget
  (dlg, dfg) <- newDialog (widget invoiceUI) "New Invoice"
  mfg <- mergeFocusGroups (fg invoiceUI) dfg

  c <- newCollection
  _ <- addToCollection c (dialogWidget dlg) mfg
  mfg `onKeyPressed` \_ k _ -> case k of
    KASCII 'q' -> do
      shutdownUi
      inv <- vreturn invoiceC
      _ <- putStrLn $ show inv
      return True
    _ -> return False
  runUi c defaultContext
