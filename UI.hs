{-# LANGUAGE OverloadedStrings #-}
module UI where

import Graphics.Vty hiding (Button)
import Graphics.Vty.Widgets.All
import Database.SQLite.Simple (Connection)
import qualified Data.Text as T

import Model
import Views.AddClient

companyName = "lichtzwerge GmbH"
version = "v0.1"

selAttr = black `on` yellow

data ClientsUI =
  ClientsUI { getList :: Widget (List Client FormattedText) }

data AppUI = AppUI { getConn :: Connection
                   , getClients :: ClientsUI
                   , getUIs :: Collection }

mkClientsUI = do
  lst <- newList selAttr
  return $ ClientsUI lst

mkAppUI conn = do
  clients <- mkClientsUI
  c <- newCollection
  return $ AppUI conn clients c

getClientsList = getList . getClients

clientListUI app = do
  header <- plainText "Clients" <++> hFill ' ' 1 <++> plainText companyName
  footer <- plainText "Clients | Invoices" <++> hFill ' ' 1 <++> plainText version

  populateClients app
  
  lstFooter <- plainText "add  delete"

  lstUi <- (((return (getClientsList $ app)) >>= bordered) <--> (return lstFooter))

  ui <- (return header) <--> (return lstUi) <--> (return footer)

  fg <- newFocusGroup
  _ <- addToFocusGroup fg (getClientsList app)

  return (ui, fg)
  where populateClients app = do
          clients <- allClients $ getConn app
          mapM_ (\c -> addToList (getClientsList app) c =<< (plainText . T.pack $ name c)) clients

showClientAddUI app completion = do
  (caui, cafg) <- newClientAddDialog onAccept onCancel
  switchToAdd <- addToCollection (getUIs app) caui cafg
  switchToAdd
  where onAccept ws = \_ -> do
          client <- toClient ws
          client2 <- insertClient (getConn app) client
          addToList (getClientsList app) client2 =<< (plainText . T.pack $ name client2)
          completion
        onCancel _ = const completion

mainUI conn = do

  app <- mkAppUI conn

  (clui, clfg) <- clientListUI app

  switchToClientList <- addToCollection (getUIs app) clui clfg

--  (caui, cafg) <- clientAddUI (const $ switchToClientList) (const $ switchToClientList)
--  switchToClientAdd  <- addToCollection c caui cafg

  -- client list can be quit from with q and esc.
  clfg `onKeyPressed` \_ k _ ->
    case k of
      (KASCII 'a') -> showClientAddUI app switchToClientList >> return True
      (KASCII 'q') -> shutdownUi >> return True
      KEsc -> shutdownUi >> return True
      _ -> return False

  runUi (getUIs app) defaultContext
