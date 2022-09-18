{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
module Main where

import Type.Reflection

import GHC.Generics

import Data.Bifunctor

import qualified Data.Map as M

import qualified Data.Text.IO
import Data.Aeson
import Dhall hiding (map, void)
import Dhall.JSON

import Data.Foldable
import Control.Monad
import qualified Control.Exception

import qualified System.IO
import qualified System.Environment
import System.FilePath
import System.Directory

type ModId = String

newtype LangMap = LangMap { unLM :: M.Map String String }
                  deriving Show

data Item = Item
              { modId        :: String
              , name         :: String
              , model_path   :: FilePath
              , texture_path :: Maybe FilePath
              , model        :: Model
              , tags         :: [Tag]
              , lang         :: LangMap
              } deriving (Generic, Show)

data Block = Block
              { modId           :: String
              , name            :: String
              , blockstate_path :: FilePath
              , model_path      :: FilePath
              , texture_path    :: Maybe FilePath
              , blockstate      :: Value
              , model           :: Model
              , assoc_item      :: Item
              , loot_table_path :: FilePath
              , loot_table      :: Value
              , tags            :: [Tag]
              , lang            :: LangMap
              } deriving (Generic, Show)

data Model = Model
              { parent :: String
              , textures :: Maybe Value
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

type LangDefs = M.Map FilePath (M.Map String String)

-- TODO: Move path calculations here?

instance FromJSON Item
instance FromJSON Block
instance FromJSON Model
instance FromJSON Tag
instance FromJSON TagDef

instance ToJSON Model where
  toJSON (Model p Nothing)   = object [ "parent" .= p ]
  toJSON (Model p (Just ts)) = object [ "parent" .= p, "textures" .= ts ]

instance ToJSON TagDef where
  toJSON (TagDef _ r v) = object [ "replace" .= r, "values" .= v ]

instance ToJSON LangMap where
  toJSON (LangMap m) = toJSON m

instance FromJSON LangMap where
  parseJSON = fmap LangMap . withArray "lang map"
                              (foldlM (\acc -> withObject "lang map obj" $ \o -> do
                                  k <- o .: "mapKey"
                                  v <- o .: "mapValue"
                                  pure (M.insert k v acc)) mempty)

langPath :: ModId -> FilePath
langPath modId = "src/main/resources/assets/" <> modId <> "/lang/"

createDirAndEncodeFile :: ToJSON a => FilePath -> a -> IO ()
createDirAndEncodeFile fp (toJSON -> val) = do
  createDirectoryIfMissing True (takeDirectory fp)
  encodeFile fp val

loadDhallAsJSON :: FromJSON a => FilePath -> IO a
loadDhallAsJSON fp = do
  dhallToJSON <$> (inputExpr =<< Data.Text.IO.readFile fp) >>= \case
    Left  e      -> Control.Exception.throwIO e
    Right asJSON ->
      case fromJSON asJSON of
        Success val -> pure val
        Error   e   -> fail e

makeTagDefs :: [TagDef] -> [Tag] -> [TagDef]
makeTagDefs defs tags =
  let tagMap  = M.fromList $ map (\d -> (d.tag_path, d)) defs
   in M.elems $ foldr (\t -> M.alter (insertOrUpdateTag t) t.tag_path) tagMap tags
  where
    insertOrUpdateTag :: Tag -> Maybe TagDef -> Maybe TagDef
    insertOrUpdateTag t = \case
      Nothing -> Just (TagDef t.tag_path False [t.value])
      Just (TagDef p r vals) -> Just (TagDef p r (t.value:vals))


makeLangDefs :: [Item] -> [Block] -> LangDefs
makeLangDefs items blocks
  = M.fromListWith (<>)
      (concatMap transform items <>
       concatMap transform blocks)
  where
    -- transform :: (HasField "modId" r String, HasField "name" r String, HasField "lang" r LangMap) => r -> [(String, M.Map String String)]
    transform x = bimap ((langPath x.modId <>) . (<> ".json")) (M.singleton (typeStr x <> x.modId <> "." <> x.name)) <$> M.toList (unLM x.lang)
      where
        typeStr :: Typeable a => a -> String
        typeStr t
          | Just HRefl <- typeOf t `eqTypeRep` typeRep @Item = "item."
          | Just HRefl <- typeOf t `eqTypeRep` typeRep @Block = "block."
          | otherwise = error "unexpected unsupported type"


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

    -- loot table
    createDirAndEncodeFile b.loot_table_path b.loot_table


    validateTexture b.texture_path

-- TODO: Take into consideration existing lang files. Most files shouldn't be overwritten?
processLangs :: LangDefs -> IO ()
processLangs = (() <$) . M.traverseWithKey createDirAndEncodeFile


delFile :: FilePath -> IO ()
delFile p = do
  fileExists <- doesFileExist p
  when fileExists (removeFile p)

main :: IO ()
main = do
  putStrLn "Welcome to the Minecraft Mod developer toolsuite!"

  -- TODO: Don't overwrite existing tags!!

  items  <- loadDhallAsJSON @[Item]  "Items.dhall"
  blocks <- loadDhallAsJSON @[Block] "Blocks.dhall"

  -- TODO:
  -- baseTagDefs <- loadDhallAsJSON @[TagDef] "Tags.dhall"
  let baseTagDefs = []

  let allTags  = concatMap (.tags) items <> concatMap (.tags) blocks
      tagDefs  = makeTagDefs baseTagDefs allTags
      langDefs = makeLangDefs items blocks

  args <- System.Environment.getArgs

  case args of
    "generate":_ -> do

      forM_ tagDefs $ \d ->
        -- TODO: Don't overwrite existing tags!! rather, extend them
        createDirAndEncodeFile d.tag_path d

      forM_ items processItem

      forM_ blocks processBlock

      processLangs langDefs

    "clean":_  -> do

      forM_ tagDefs (delFile . (.tag_path))

      forM_ items $ \i -> delFile i.model_path

      forM_ blocks $ \b -> do
        delFile b.model_path
        delFile b.blockstate_path
        delFile b.assoc_item.model_path

      forM_ (M.keys langDefs) delFile

    _ -> do
      putStrLn
        "Valid args:\n\
        \  generate: generate all JSONs for items listed in Items.dhall and blocks listed Blocks.dhall"



