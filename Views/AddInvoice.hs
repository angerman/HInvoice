{-# LANGUAGE OverloadedStrings #-}
module Views.AddClient (toInvoice, newInvoiceAddDialog) where 

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

data addInvoiceUI =
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
