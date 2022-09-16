{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
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
              { model_path   :: FilePath
              , texture_path :: FilePath
              , model        :: Value
              } deriving (Generic, Show)

data Block = Block
              { blockstate_path :: FilePath
              , blockstate      :: Value
              , model_path      :: FilePath
              , model           :: Value
              , item_model_path :: FilePath
              , item_model      :: Value
              , texture_path    :: FilePath
              } deriving (Generic, Show)

instance FromJSON Item
instance FromJSON Block

main :: IO ()
main = do
  putStrLn "Welcome to the Minecraft Mod developer toolsuite!"

  putStrLn "This program will generate all model JSONs for items listed in Items.dhall and Blocks.dhall"

  Right itemsJSON <- dhallToJSON <$> (inputExpr =<< Data.Text.IO.readFile "Items.dhall")
  Success items <- pure $ fromJSON @[Item] itemsJSON

  forM_ items $ \i -> do
    createDirectoryIfMissing True (takeDirectory i.model_path)
    encodeFile i.model_path i.model

    texExists <- doesFileExist i.texture_path
    unless texExists $ do
      putStrLn $ "Warning: Texture file " <> i.texture_path <> " doesn't exist."

  Right blocksJSON <- dhallToJSON <$> (inputExpr =<< Data.Text.IO.readFile "Blocks.dhall")
  Success blocks <- pure $ fromJSON @[Block] blocksJSON

  forM_ blocks $ \b -> do
    -- blockstate
    createDirectoryIfMissing True (takeDirectory b.blockstate_path)
    encodeFile b.blockstate_path b.blockstate

    -- model
    createDirectoryIfMissing True (takeDirectory b.model_path)
    encodeFile b.model_path b.model

    -- item model
    createDirectoryIfMissing True (takeDirectory b.item_model_path)
    encodeFile b.item_model_path b.item_model

    texExists <- doesFileExist b.texture_path
    unless texExists $ do
      putStrLn $ "Warning: Texture file " <> b.texture_path <> " doesn't exist."


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

