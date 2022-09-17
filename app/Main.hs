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

import qualified System.IO
import qualified System.Environment
import System.FilePath
import System.Directory

type ModId = String

data Item = Item
              { modId        :: String
              , name         :: String
              , model_path   :: FilePath
              , texture_path :: Maybe FilePath
              , model        :: Value
              , tags         :: [Tag]
              } deriving (Generic, Show)

data Block = Block
              { modId           :: String
              , name            :: String
              , blockstate_path :: FilePath
              , model_path      :: FilePath
              , texture_path    :: Maybe FilePath
              , blockstate      :: Value
              , model           :: Value
              , assoc_item      :: Item
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


makeTagDefs :: [TagDef] -> [Tag] -> [TagDef]
makeTagDefs defs tags =
  let tagMap  = M.fromList $ map (\d -> (d.tag_path, d)) defs
   in M.elems $ foldr (\t -> M.alter (insertOrUpdateTag t) t.tag_path) tagMap tags
  where
    insertOrUpdateTag :: Tag -> Maybe TagDef -> Maybe TagDef
    insertOrUpdateTag t = \case
      Nothing -> Just (TagDef t.tag_path False [t.value])
      Just (TagDef p r vals) -> Just (TagDef p r (t.value:vals))




validateTexture :: Maybe FilePath -> IO ()
validateTexture = \case
  Nothing -> pure ()
  Just tp -> doesFileExist tp >>= flip unless (System.IO.hPutStrLn System.IO.stderr $ "Warning: Texture file " <> tp <> " doesn't exist.")


processItem :: Item -> IO ()
processItem i = do

    putStrLn $ "Generating item " <> i.modId <> ":" <> i.name

    createDirAndEncodeFile i.model_path i.model

    validateTexture i.texture_path


processBlock :: Block -> IO ()
processBlock b = do

    putStrLn $ "Generating block " <> b.modId <> ":" <> b.name

    -- blockstate
    createDirAndEncodeFile b.blockstate_path b.blockstate

    -- model
    createDirAndEncodeFile b.model_path b.model

    -- associated item
    processItem (b.assoc_item)

    validateTexture b.texture_path

delFile :: FilePath -> IO ()
delFile p = do
  fileExists <- doesFileExist p
  when fileExists (removeFile p)


main :: IO ()
main = do
  putStrLn "Welcome to the Minecraft Mod developer toolsuite!"

  -- TODO:
  -- baseTagDefs <- loadDhallAsJSON @[TagDef] "Tags.dhall"
  let baseTagDefs = []

  items  <- loadDhallAsJSON @[Item]  "Items.dhall"
  blocks <- loadDhallAsJSON @[Block] "Blocks.dhall"

  let allTags = concatMap (.tags) items <> concatMap (.tags) blocks
      tagDefs = makeTagDefs baseTagDefs allTags

  [arg] <- System.Environment.getArgs

  case arg of
    "generate" -> do

      forM_ tagDefs $ \d ->
        createDirAndEncodeFile d.tag_path (toJSON d)

      forM_ items processItem

      forM_ blocks processBlock

    "clean"    -> do

      forM_ tagDefs (delFile . (.tag_path))

      forM_ items $ \i -> delFile i.model_path

      forM_ blocks $ \b -> do
        delFile b.model_path
        delFile b.blockstate_path
        delFile b.assoc_item.model_path

    _ -> do
      putStrLn
        "Valid args:\
        \  generate: generate all JSONs for items listed in Items.dhall and blocks listed Blocks.dhall"



