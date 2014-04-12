{-# LANGUAGE OverloadedStrings #-}
module Views.AddClient (toClient, newClientAddDialog) where 

import Graphics.Vty.Widgets.All (Widget, Edit,
                                 editWidget,getEditText,
                                 plainText,
                                 centered,
                                 (<-->),(<++>),
                                 hFill,hLimit,
                                 boxFixed,
                                 withPadding,padRight,
                                 newFocusGroup,addToFocusGroup,mergeFocusGroups)
import Graphics.Vty.Widgets.Dialog
import qualified Data.Text as T
import Control.Applicative ((<$>),(<*>))
import Control.Monad (liftM)
import Models.Client (Client, mkClient, mkAddress)

data AddClientUI =
  AddClientUI { no :: Widget Edit
              , name :: Widget Edit
              , street :: Widget Edit
              , zipcode :: Widget Edit
              , city :: Widget Edit
              , state :: Widget Edit
              , country :: Widget Edit
              , vat :: Widget Edit }

mkAddClientUI = do
  no      <- editWidget
  name    <- editWidget
  street  <- editWidget
  zipcode <- editWidget
  city    <- editWidget
  state   <- editWidget
  country <- editWidget
  vat     <- editWidget
  return $ AddClientUI no name street zipcode city state country vat

toClient st = do
  no'      <- get $ no st
  name'    <- get $ name st
  street'  <- get $ street st
  zipcode' <- get $ zipcode st
  city'    <- get $ city st
  state'   <- get $ state st
  country' <- get $ country st
  vat'     <- get $ vat st
  let address = mkAddress street' zipcode' city' (Just state') country'
  return $ mkClient 0 (read no') name' address (Just vat')
  where get = liftM T.unpack . getEditText

clientAddUI st = do
  fg <- newFocusGroup
  mapM_ (addToFocusGroup fg) $ [no', name', vat', street', zipcode', city', state', country']

  ui <- (plainText "ID:")     <--> (boxFixed  5 1     no') <-->
        (plainText "Name:")   <--> (boxFixed 40 1   name') <-->
        (plainText "VAT ID:") <--> (boxFixed 40 1    vat') <-->
        (plainText "Street:") <--> (boxFixed 40 1 street') <-->
        (plainText "Zipcode, City:") <-->
        (((boxFixed 10 1 zipcode') >>= withPadding (padRight 2)) <++> (boxFixed 28 1 city')) <-->
        (plainText "State, Country:") <-->
        (((boxFixed 10 1 state') >>= withPadding (padRight 2)) <++> (boxFixed 28 1 country'))

  return (ui, fg)
  where AddClientUI no' name' street' zipcode' city' state' country' vat' = st

newClientAddDialog onAccept onCancel = do
  header <- plainText "Add Client" <++> hFill ' ' 1 <++> plainText "lichtzwerge GmbH"
  footer <- plainText "Clients | Invoices" <++> hFill ' ' 1 <++> plainText "v0.1"

  st <- mkAddClientUI
  (ui, fg) <- clientAddUI st
  (dlg, dfg) <- newDialog ui "New Client"

  mfg <- mergeFocusGroups fg dfg
  mui <- (return header) <--> (centered =<< hLimit 55 =<< (return (dialogWidget dlg))) <--> (return footer)

  dlg `onDialogAccept` onAccept st
  dlg `onDialogCancel` onCancel st

  return (mui, mfg)
