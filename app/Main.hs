{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
module Main where

import GHC.Generics

import qualified Data.Map as M

import qualified Data.Text.IO
import Data.Aeson
import Dhall hiding (map)
import Dhall.JSON

import Control.Monad

import System.FilePath
import System.Directory

type ModId = String

data Item = Item
              { model_path   :: FilePath
              , texture_path :: FilePath
              , model        :: Value
              , tags            :: [Tag]
              } deriving (Generic, Show)

data Block = Block
              { blockstate_path :: FilePath
              , blockstate      :: Value
              , model_path      :: FilePath
              , model           :: Value
              , item_model_path :: FilePath
              , item_model      :: Value
              , texture_path    :: FilePath
              , tags            :: [Tag]
              } deriving (Generic, Show)

data Tag = Tag
              { tag_path :: String
              , value    :: String
              } deriving (Generic, Show)

data TagDef = TagDef
              { tag_path :: FilePath
              , replace  :: Bool
              , values   :: [String]
              } deriving (Generic, Show)

instance FromJSON Item
instance FromJSON Block
instance FromJSON Tag
instance FromJSON TagDef

instance ToJSON TagDef where
  toJSON (TagDef _ r v) = object [ "replace" .= r, "values" .= v ]

createDirAndEncodeFile :: FilePath -> Value -> IO ()
createDirAndEncodeFile fp val = do
  createDirectoryIfMissing True (takeDirectory fp)
  encodeFile fp val

loadDhallAsJSON :: FromJSON a => FilePath -> IO a
loadDhallAsJSON fp = do
  Right asJSON  <- dhallToJSON <$> (inputExpr =<< Data.Text.IO.readFile fp)
  Success val <- pure $ fromJSON asJSON
  pure val

createTagDefs :: [TagDef] -> [Tag] -> IO ()
createTagDefs defs tags =
  let
      tagMap  = M.fromList $ map (\d -> (d.tag_path, d)) defs
      tagMap' = foldr (\t -> M.alter (insertOrUpdateTag t) t.tag_path) tagMap tags
   in do
      print tags
      print tagMap'
      forM_ (M.elems tagMap') $ \d -> do
        print d.tag_path
        createDirAndEncodeFile d.tag_path (toJSON d)

  where
    insertOrUpdateTag :: Tag -> Maybe TagDef -> Maybe TagDef
    insertOrUpdateTag t = \case
      Nothing -> Just (TagDef t.tag_path False [t.value])
      Just (TagDef p r vals) -> Just (TagDef p r (t.value:vals))

      




main :: IO ()
main = do
  putStrLn "Welcome to the Minecraft Mod developer toolsuite!"

  putStrLn "This program will generate all model JSONs for items listed in Items.dhall and Blocks.dhall"

  -- TODO:
  -- baseTagDefs <- loadDhallAsJSON @[TagDef] "Tags.dhall"
  let baseTagDefs = []

  items  <- loadDhallAsJSON @[Item]  "Items.dhall"
  blocks <- loadDhallAsJSON @[Block] "Blocks.dhall"

  let allTags = concatMap (.tags) items <> concatMap (.tags) blocks

  createTagDefs baseTagDefs allTags

  forM_ items $ \i -> do
    createDirAndEncodeFile i.model_path i.model

    texExists <- doesFileExist i.texture_path
    unless texExists $ do
      putStrLn $ "Warning: Texture file " <> i.texture_path <> " doesn't exist."

  forM_ blocks $ \b -> do

    -- blockstate
    createDirAndEncodeFile b.blockstate_path b.blockstate

    -- model
    createDirAndEncodeFile b.model_path b.model

    -- item model
    createDirAndEncodeFile b.item_model_path b.item_model

    texExists <- doesFileExist b.texture_path
    unless texExists $ do
      putStrLn $ "Warning: Texture file " <> b.texture_path <> " doesn't exist."

