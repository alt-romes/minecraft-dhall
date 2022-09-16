{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
module Main where

import GHC.Generics

import qualified Data.Text.IO
import Data.Aeson
import Dhall
import Dhall.JSON

import Control.Monad

import System.FilePath
import System.Directory

type ModId = String

data Item = Item
              { fpath :: FilePath
              , val   :: Value
              } deriving (Generic, Show)

instance FromJSON Item

main :: IO ()
main = do
  putStrLn "Welcome to the Minecraft Mod developer toolsuite!"

  putStrLn "This program will generate all model JSONs for items listed in Items.dhall"

  Right itemsJSON <- dhallToJSON <$> (inputExpr =<< Data.Text.IO.readFile "Items.dhall")

  Success items <- pure $ fromJSON @[Item] itemsJSON

  forM_ items $ \i -> do
    createDirectoryIfMissing True (takeDirectory i.fpath)
    encodeFile i.fpath i.val


  print items


-- data Command = NewItem
--              | NewBlock
--              | Unknown
--              deriving Read

-- eval :: ModId -> Command -> IO ()
-- eval modid = \case
--   NewItem -> do
--       putStrLn "Creating a new item..."
--       putStrLn "What's the name of the new item?"
--       itemName <- getLine

--       let
--           model_path   = "/resources/assets/" <> modid <> "/models/item/" <> itemName <> ".json"
--           texture_path = "/resources/assets/" <> modid <> "/textures/item/" <> itemName <> ".png"

--       putStrLn $ "Make sure that the ./tmp/ folder has the texture named " <> itemName <> ".png, then press enter."
--       _ <- getLine

--       putStrLn $ "Adding item model at " <> model_path

--       putStrLn $ "Adding item texture at " <> texture_path

--       putStrLn "Register the item as desired. Here's a base:"
--       putStrLn $ "public static final Item " <> map C.toUpper itemName <> "= new Item(new FabricItemSettings());"
--       putStrLn $ "public static final Item " <> map C.toUpper itemName <> "= new Item(new FabricItemSettings());"

--   NewBlock -> undefined
--   Unknown -> putStrLn "Unknown command"



-- main :: IO ()
-- main = do
--   -- clearScreen
--   -- setCursorPosition 0 0

--   -- setSGR [SetColor Foreground Vivid Green]
--   putStrLn "Welcome to the Minecraft Mod developer tool suite!"
--   -- setSGR [Reset]

--   putStrLn "What's the name of your mod?"
--   modName <- getLine
  
--   loop modName

--   where

--     loop :: String -> IO ()
--     loop modName = do

--       putStrLn "Choose an action."

--       putStrLn "Available actions: new_item"

--       eval modName . fromMaybe Unknown . readMaybe =<< getLine

--       loop modName

-- -- blue :: String -> IO ()
-- -- blue s = do
-- --   setSGR [SetColor Foreground Vivid Blue]
-- --   putStrLn s
-- --   setSGR [Reset]
-- --   putStr ""

