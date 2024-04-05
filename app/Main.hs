{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
module Main where

import qualified Data.Map as M

import qualified Data.Text.IO
import Data.Aeson
import Dhall hiding (map, void)
import Dhall.JSON

import Data.Foldable
import Control.Monad
import qualified Control.Exception
import Control.Arrow ((>>>))
import Control.Monad.IO.Class

import qualified System.IO
import qualified System.Environment

import System.FilePath
import System.Directory

import Minecraft.Dhall

-- TODO: Move path calculations here?

createDirAndEncodeFile :: ToJSON a => FilePath -> a -> IO ()
createDirAndEncodeFile fp (toJSON -> val) = do
  createDirectoryIfMissing True (takeDirectory fp)
  encodeFile fp val

delFile :: FilePath -> IO ()
delFile p = do
  fileExists <- doesFileExist p
  when fileExists (removeFile p)

loadDhallAsJSON :: FromJSON a => FilePath -> IO a
loadDhallAsJSON fp = do
  Data.Text.IO.readFile fp >>= inputExpr >>= (dhallToJSON >>> \case
    Left  e      -> Control.Exception.throwIO e
    Right asJSON ->
      case fromJSON asJSON of
        Success val -> pure val
        Error   e   -> fail e)

validateTexture :: MonadIO m => Maybe FilePath -> m ()
validateTexture = \case
  Nothing -> pure ()
  Just tp -> liftIO $ doesFileExist tp >>= flip unless (System.IO.hPutStrLn System.IO.stderr $ "Warning: Texture file " <> tp <> " doesn't exist.")

main :: IO ()
main = do
  putStrLn "Welcome to the Minecraft Mod developer toolsuite!"

  args <- System.Environment.getArgs

  let
    -- Loads a list of @a@'s from the .dhall file
    load :: forall a. FromJSON a => FilePath -> IO [a]
    load file = doesFileExist file >>= \case
      True -> do
        putStrLn $ "Loading " ++ file ++ "..."
        loadDhallAsJSON @[a] file
      False -> pure []

    -- TODO: Don't overwrite existing tags!!

    loadItems  = load @Item "Items.dhall"
    loadBlocks = load @Block "Blocks.dhall"

    -- TODO:
    -- baseTagDefs <- load @TagDef "Tags.dhall"

  case args of
    "generate":_ -> do

       items <- loadItems
       blocks <- loadBlocks

       files <- runMinecraftWriterT $ do

         forM_ blocks $ \b -> do
           liftIO $ putStrLn $ "Generating block " <> b.modId <> ":" <> b.name
           processBlock b
           validateTexture b.texture_path

         forM_ items $ \i -> do
           liftIO $ putStrLn $ "Generating item " <> i.modId <> ":" <> i.name
           processItem i
           validateTexture i.texture_path

       _ <- M.traverseWithKey createDirAndEncodeFile files
       pure ()

    "clean":_  -> do

       items <- loadItems
       blocks <- loadBlocks

       files <-
         runMinecraftWriterT $ do
           forM_ blocks processBlock
           forM_ items  processItem

       _ <- M.traverseWithKey (\fp _ -> delFile fp) files

       pure ()

    _ -> do

      putStrLn
        "Valid args:\n\
        \  generate: generate all JSONs for items listed in Items.dhall and blocks listed Blocks.dhall\n\
        \  clean: delete all files for Blocks.dhall and Items.dhall by minecraft-dhall"



