{-# LANGUAGE OverloadedStrings #-}
module Models.Client where

import Control.Applicative
import Database.SQLite.Simple (Connection, execute, query, query_, lastInsertRowId)
import Database.SQLite.Simple.FromRow
import Data.Maybe (listToMaybe)

data Address  = Address  { street :: String
                         , zipcode :: String
                         , city :: String
                         , state :: Maybe String
                         , country :: String }
              deriving (Show)
                        
data Client = Client { clientPK :: Int
                     , clientID :: Int
                     , name :: String
                     , address :: Address
                     , vatNo :: Maybe String }
            | EmptyClient
              deriving (Show)
                       
instance FromRow Client where
  fromRow = Client <$> field <*> field <*> field <*> (Address <$> field <*> field <*> field <*> field <*> field) <*> field

mkAddress = Address
mkClient = Client

insertClient conn cust = do
  execute conn "insert into clients (id,name,street,zip,city,state,country,vatNo) values (?,?,?,?,?,?,?,?)" (i,n,s,z,c,st,co,v)
  id <- lastInsertRowId conn
  return cust{ clientPK = fromIntegral id }
  where (Client _ i n (Address s z c st co) v) = cust

getClient :: Connection -> Int -> IO (Maybe Client)
getClient conn pk = listToMaybe <$> query conn "select * from clients  where pk=?" [pk]
  

allClients conn = query_ conn "SELECT * from clients" :: IO [Client]
