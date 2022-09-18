{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DerivingVia #-}

{-| A module for processing miencraft configurations in a specific dhall
 -}
module Minecraft.Dhall where

import GHC.Generics
import GHC.Records

import Type.Reflection

import Data.Bifunctor
import Data.Foldable
import qualified Data.Map as M

import Data.Aeson

import Control.Monad.Writer

-- import System.FilePath

type ModId = String

-- TODO: Error on overlap using newtype instance for Semigroup!
type FilesToWrite = M.Map FilePath Value

-- terrible name
class FileProducer a where
  produceFilesToWrite :: a -> FilesToWrite

instance FileProducer Langs where
  produceFilesToWrite (Langs m) = toJSON <$> m

instance FileProducer Tags where
  produceFilesToWrite (Tags m) = toJSON <$> m

-- | To accumulate language information thorought processing
-- | e.g. @fromList ("src/main/resources/assets/tutorial/lang/en_us.json", fromList [("item.modid.mod_sword", "Modded Sword"), ("item.modid.mod_axe", "Special Axe"])@
newtype Langs = Langs (M.Map FilePath (M.Map String String))

newtype LangAssociations = LA { unLA :: M.Map String String }
                         deriving Show
                         deriving ToJSON via M.Map String String

instance Semigroup Langs where
  -- Assumes the associations don't collide, but has, otherwise, a left bias.
  -- (<>) (Langs a) (Langs b) = Langs $ M.merge M.preserveMissing M.preserveMissing (M.zipWithMatched (const (<>))) a b
  (<>) (Langs a) (Langs b) = Langs $ M.unionWith (<>) a b

instance Monoid Langs where
  mempty = Langs mempty


newtype Tags = Tags (M.Map FilePath TagDef)

instance Semigroup Tags where
  -- Assumes the associations don't collide, but has, otherwise, a right bias.
  (<>) (Tags a) (Tags b) = Tags $ M.unionWith (<>) a b

instance Monoid Tags where
  mempty = Tags mempty

data TagDef = TagDef { replace :: Bool
                     , values  :: [String] } deriving Generic

instance Semigroup TagDef where
  -- | Join values and has a left bias when merging replace
  (<>) (TagDef r v) (TagDef _ v') = TagDef r (v <> v')


data Item = Item
              { modId        :: String
              , name         :: String
              , model_path   :: FilePath
              , texture_path :: Maybe FilePath
              , model        :: Model
              , tags         :: [Tag]
              , lang         :: LangAssociations
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
              , lang            :: LangAssociations
              } deriving (Generic, Show)

data Model = Model
              { parent :: String
              , textures :: Maybe Value
              } deriving (Generic, Show)

data Tag = Tag
              { tag_path :: String
              , value    :: String
              } deriving (Generic, Show)


instance FromJSON Item
instance FromJSON Block
instance FromJSON Model
instance FromJSON Tag
instance FromJSON TagDef
instance ToJSON TagDef

instance ToJSON Model where
  toJSON (Model p Nothing)   = object [ "parent" .= p ]
  toJSON (Model p (Just ts)) = object [ "parent" .= p, "textures" .= ts ]

instance FromJSON LangAssociations where
  parseJSON = fmap LA . withArray "lang map"
                              (foldlM (\acc -> withObject "lang map obj" $ \o -> do
                                  k <- o .: "mapKey"
                                  v <- o .: "mapValue"
                                  pure (M.insert k v acc)) mempty)

type MinecraftWriter = Writer (Tags, Langs, FilesToWrite)

runMinecraftWriter :: MinecraftWriter a -> FilesToWrite
runMinecraftWriter act =
  let (tags, langs, files) = execWriter act
   in (produceFilesToWrite tags <> produceFilesToWrite langs <> files)

langPath :: ModId -> FilePath
langPath modId = "./src/main/resources/assets/" <> modId <> "/lang/"

makeLangsFromXWithAssocs :: (Typeable x, HasField "modId" x String, HasField "name" x String, HasField "lang" x LangAssociations) => x -> Langs
makeLangsFromXWithAssocs = Langs . M.fromListWith (<>) . transform
  where
    transform :: (Typeable r, HasField "modId" r String, HasField "name" r String, HasField "lang" r LangAssociations) => r -> [(String, M.Map String String)]
    transform x = bimap ((langPath x.modId <>) . (<> ".json")) (M.singleton (typeStr x <> x.modId <> "." <> x.name)) <$> M.toList (unLA x.lang)

    typeStr :: Typeable a => a -> String
    typeStr t
      | Just HRefl <- typeOf t `eqTypeRep` typeRep @Item = "item."
      | Just HRefl <- typeOf t `eqTypeRep` typeRep @Block = "block."
      | otherwise = error "unexpected unsupported type"


processItem :: Item -> MinecraftWriter ()
processItem i =
  let
      tags  = Tags $ M.fromList (map (\(Tag p v) -> (p, TagDef False [v])) i.tags)
      langs = makeLangsFromXWithAssocs i
      files = M.singleton i.model_path (toJSON i.model)
   in
      tell (tags, langs, files)


processBlock :: Block -> MinecraftWriter ()
processBlock b =
  let
      tags  = Tags $ M.fromList (map (\(Tag p v) -> (p, TagDef False [v])) b.tags)
      langs = makeLangsFromXWithAssocs b
      files = M.fromList [ (b.model_path, toJSON b.model)
                         , (b.blockstate_path, b.blockstate)
                         , (b.loot_table_path, b.loot_table) ]
   in do
      tell (tags, langs, files)
      processItem (b.assoc_item)


