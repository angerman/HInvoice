{-# LANGUAGE OverloadedStrings #-}

import Model
import UI

import Database.SQLite.Simple (withConnection)

main = withConnection "invoice.db" mainUI
