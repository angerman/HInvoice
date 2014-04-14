{-# LANGUAGE OverloadedStrings #-}
module Views.AddInvoice (--toInvoice,
  newInvoiceAddDialog) where
import Graphics.Vty
import Graphics.Vty.Widgets.All
import Graphics.Vty.Widgets.Dialog
import qualified Data.Text as T
import Control.Applicative ((<$>),(<*>))
import Control.Monad (liftM)
import Text.Read (readMaybe)
import Data.Decimal

import Models.Product (Product(..))
import Models.Invoice (mkInvoice, Period(..))
import Models.Client (getClient)

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

selAttr = black `on` yellow

data AddInvoiceUI =
  AddInvoiceUI { client :: Widget Edit
               , date :: Widget Edit
               , due :: Widget Edit
               , from :: Widget Edit
               , to :: Widget Edit
               , products :: Widget (List ((Widget Edit, Widget (List Product FormattedText)))
                                     (Box (Box (Box (VFixed (HFixed Edit))
                                                (VFixed (List Product FormattedText)))
                                           HFill)
                                      FormattedText))
               , vat :: Widget Edit
               , discount :: Widget Edit
               , cashback :: Widget Edit
               -- display fields
               , subtotal :: Widget FormattedText
               , totalVAT :: Widget FormattedText
               , totalDiscount :: Widget FormattedText
               , totalCashback :: Widget FormattedText
               , total :: Widget FormattedText }

getProductListItem we pl = do
  qty'     <- liftM read . liftM T.unpack . getEditText $ we
  r        <- getSelected pl
  case r of
    Just (_, (p, _)) -> return (qty', p)


getProducts :: AddInvoiceUI -> IO [(Int, Product)]
getProducts st = getProducts' [] (products st) 0
  where getProducts' acc l i = do
          e <- getListItem l i
          case e of
            Nothing -> return acc
            Just ((we,pl),_) -> do
              qp <- getProductListItem we pl
              getProducts' (qp:acc) l $ i+1
  

toInvoice conn st = do
  client'   <- liftM read . get $ client st
  date'     <- liftM read . get $ date st
  due'      <- liftM read . get $ due st
  from'     <- liftM read . get $ from st
  to'       <- liftM read . get $ to st
  products' <- getProducts st
  vat'      <- getVAT $ vat st
  discount' <- getDiscount $ discount st
  c <- getClient conn client'

  case c of
    Just c -> return $ mkInvoice 0 0 c (Period from' to') date' due' products' vat' discount' 0
  
  where get = liftM T.unpack . getEditText
        getVAT = liftM ((/100) . toDecimal 0) . getEditText
        getDiscount = liftM (negate . (/100) . toDecimal 0) . getEditText

showCurrency :: Decimal -> String -> String
showCurrency d c = (show $ roundTo 2 d) ++ " " ++ c

mkAddInvoiceUI prods = do
  client    <- editWidget
  date      <- editWidget
  due       <- editWidget
  from      <- editWidget
  to        <- editWidget
  products  <- newList selAttr 1
  vat       <- editWidget
  discount  <- editWidget
  cashback  <- editWidget
  subtotal  <- plainText "0.00 XXX"
  totalVAT  <- plainText "0.00 XXX"
  totalDiscount <- plainText "0.00 XXX"
  totalCashback <- plainText "0.00 XXX"
  total     <- plainText "0.00 XXX"
  -- helper
  let getVAT = (/100) . toDecimal 0 <$> getEditText vat
      getDiscount = negate . (/100) . toDecimal 0 <$> getEditText discount
      
  
  -- add events
  let updateVAT = do
        (st_, cur) <- computeSubTotal (0,"---") products 0
        d <- getDiscount
        v <- getVAT
        setText totalVAT $ T.pack $ showCurrency (st_ * (1 + d) * v) cur
      
      updateDiscount = do
        (st_, cur) <- computeSubTotal (0,"---") products 0
        d <- getDiscount
        setText totalDiscount $ T.pack $ showCurrency (st_ * d) cur

      updateTotal = do
        (st_, cur) <- computeSubTotal (0,"---") products 0
        d <- getDiscount
        v <- getVAT
        setText total $ T.pack $ showCurrency (st_ * (1 + d) * (1 + v)) cur

      updateSubtotal = do
        (st_, cur) <- computeSubTotal (0,"---") products 0
        setText subtotal $ T.pack $ showCurrency st_ cur
        
      addProduct l = do
        e <- editWidget
        setEditText e $ T.pack "1"
        lst <- newList selAttr 1
        t <- plainText ""
        p <- prods -- products
        -- add products to lst
        let formatProductPrice q p = showCurrency (q * (price p)) (currency p)
            updatePriceFormatted q p = setText t . T.pack $ formatProductPrice q p
            updatePrice = do
              -- XXX: replace with computeProductTotal
              q <- getEditText e
              r <- getSelected lst -- the result (idx, (a, b))
              case r of
                Just (_, (p, _)) -> updatePriceFormatted (toDecimal 1 q) p
                Nothing -> setText t . T.pack $ ""
              
        -- on selection or quantity change, update the price.
        lst `onSelectionChange` \_ -> updatePrice
                                      >> updateSubtotal
                                      >> updateDiscount
                                      >> updateVAT
                                      >> updateTotal
        e `onChange` \_ -> updatePrice
                           >> updateSubtotal
                           >> updateDiscount
                           >> updateVAT
                           >> updateTotal

        -- on C-p, C-n, cycle the products
        e `onKeyPressed` \_ k m ->
          case (k, m) of
            (KASCII 'p', [MCtrl]) -> scrollUp lst >> return True
            (KASCII 'n', [MCtrl]) -> scrollDown lst >> return True
            _ -> return False

        -- populate the product list, this should trigger the update event as well.
        mapM_ (\p -> addToList lst p =<< (plainText . T.pack $ Models.Product.name p)) p
        -- add [ <qty> ] [List: products] [<total> <cur>] to the list
        ((boxFixed 5 1 e) <++> (vFixed 1 lst) <++> hFill ' ' 1 <++> (return t))  >>= addToList l (e,lst)
      remProduct l = do
        r <- getSelected l
        case r of
          Nothing -> return ()
          Just (i, _) -> removeFromList l i >> return ()

  
  vat `onChange` \_ -> updateVAT >> updateTotal
  discount `onChange` \_ -> updateDiscount >> updateVAT >> updateTotal
  
  products `onKeyPressed` \l k _ -> do
    case k of
      (KASCII '+') -> addProduct l
                      >> updateSubtotal
                      >> updateDiscount
                      >> updateVAT
                      >> updateTotal                      
                      >> return True
      (KASCII '-') -> remProduct l
                      >> updateSubtotal
                      >> updateDiscount
                      >> updateVAT
                      >> updateTotal
                      >> return True
      _ -> return False

  
  -- set default values.
  setEditText vat $ T.pack "19"
  setEditText discount $ T.pack "0"
  setEditText cashback $ T.pack "0"
  return $ AddInvoiceUI client date due from to products vat discount cashback
    subtotal totalVAT totalDiscount totalCashback total

invoiceAddUI st = do
  fg <- newFocusGroup
  mapM_ (addToFocusGroup fg) $ [client', date', due', from', to']
  _ <-   addToFocusGroup fg     products'
  mapM_ (addToFocusGroup fg) $ [discount'
                               , vat'
--                               , cashback'
                               ]

  prodList <- vBorder <++> (return products') <++> vBorder
  ui <- (((plainText "Client:") <++> (boxFixed 5 1 client')) >>= withPadding (padBottom 1)) <-->
        (((plainText "Date:") <++> (boxFixed 11 1 date') <++>
          (plainText " Due:") <++> (boxFixed 11 1 due')) >>= withPadding (padBottom 1)) <-->
        (plainText "Period") <-->
        (((plainText "From:") <++> (boxFixed 11 1 from') <++>
         (plainText " to:")  <++> (boxFixed 11 1 to')) >>= withPadding (padBottom 1))  <-->
        (((plainText " Qty") >>= boxFixed 6 1) <++> (plainText "Product") <++> (hFill ' '1) <++> (plainText "Total ")) <-->
        (vFixed 8 prodList)
        <-->
        ((plainText "+/- to add / remove products") <++>
         (hFill ' ' 1) <++>
         (plainText "Subtotal: ") <++>
         (boxFixed 10 1 subtotal'))
        <-->
        ((hFill ' ' 1) <++>
         (plainText "Discount:") <++>
         (boxFixed 4 1 discount') <++>
         (plainText "% ") <++>
         (boxFixed 10 1 totalDiscount'))
        <-->
        ((hFill ' ' 1) <++>
         (plainText "VAT:") <++>
         (boxFixed 4 1 vat') <++>
         (plainText "% ") <++>
         (boxFixed 10 1 totalVAT'))
        <-->
        -- ((hFill ' ' 1) <++>
        --  (plainText "cashback:") <++>
        --  (boxFixed 8 1 cashback') <++>
        --  (plainText "%") <++>
        --  (boxFixed 7 1 totalCashback'))
        -- <-->
        (((hFill ' ' 1) <++> (plainText "Total: ") <++> (boxFixed 10 1 total'))
         >>= withPadding (padTop 1))

  -- padd the invoice ui with left +2 and right +2 char.
  paddedUI <- (return ui) >>= withPadding (padLeft 2) >>= withPadding (padRight 2)
  
  return (paddedUI, fg)
  where AddInvoiceUI client' date' due' from' to' products' vat' discount' cashback'
          subtotal' totalVAT' totalDiscount' totalCashback' total' = st

-- | returns the decimal representation of the text or the first argument (default)
toDecimal :: Decimal -> T.Text -> Decimal
toDecimal d = maybe d id . readMaybe . T.unpack

-- | computes the qty times unit price for one product (widget / item of the list)
computeProductTotal :: Widget Edit -> Widget (List Product b) -> IO (Decimal, String)
computeProductTotal edit productList = do
  q <- getEditText edit
  r <- getSelected productList
  case r of
    Just (_, (p, _)) -> return ((toDecimal 1 q) * (price p), (currency p))
    Nothing -> return (0, "---")

-- | Compute the sum of qty times unit price from the list of products.
computeSubTotal :: (Decimal, String)
                   -> Widget (List (Widget Edit, Widget (List Product b)) c)
                   -> Int -> IO (Decimal, String)
computeSubTotal acc l i = do
  e <- getListItem l i
  case e of
    Nothing -> return acc
    Just ((we,pl),_) -> do
      (x,cur) <- computeProductTotal we pl
      computeSubTotal (s+x,cur) l $ i+1
  where (s,_) = acc

newInvoiceAddDialog conn prods onAccept onCancel = do
  header <- plainText "Add Invoice" <++> hFill ' ' 1 <++> plainText "HInvoice"
  footer <- plainText "Clients | Products | Invoices" <++> hFill ' ' 1 <++> plainText "v0.1"

  st <- mkAddInvoiceUI prods
  (ui, fg) <- invoiceAddUI st
  (dlg, dfg) <- newDialog ui "New Invoice"

  mfg <- mergeFocusGroups fg dfg
  mui <- (return header) <--> (centered =<< hLimit 75 =<< (return (dialogWidget dlg))) <--> (return footer)

  dlg `onDialogAccept` onAccept (toInvoice conn st)
  dlg `onDialogCancel` onCancel st

  return (mui, mfg)
