let JSON = https://prelude.dhall-lang.org/JSON/package.dhall
let Map = https://prelude.dhall-lang.org/Map/Type.dhall
let map = https://prelude.dhall-lang.org/List/map.dhall

let ModId : Type = Text

let assets_path = "./src/main/resources/assets"
let data_path = "./src/main/resources/data"

let Model : Type = { parent   : Text
                   , textures : Optional JSON.Type }

let Tag : Type = { tag_path : Text
                 , value    : Text
                 }

let Item : Type
  = { modId        : Text
    , name         : Text
    , model_path   : Text
    , texture_path : Optional Text
    , model        : Model
    , tags         : List Tag
    , lang         : Map Text Text
    }

let Block : Type = { modId           : Text
                   , name            : Text
                   , blockstate_path : Text
                   , model_path      : Text
                   , texture_path    : Optional Text
                   , blockstate      : { variants : JSON.Type }
                   , model           : Model
                   , assoc_item      : Item
                   , loot_table_path : Text
                   , loot_table      : JSON.Type
                   , tags            : List Tag
                   , lang            : Map Text Text
                   }

-- | Given a 'ModId' (also known as 'namespace'), a list of `tag name`s, and a block, add the tags to the block.
-- e.g. addTagToBlock woodBlock "minecraft" ["mineable/axe", "needs_diamond_tool"]
let addTagsToBlock : ModId -> List Text -> Block -> Block
  = λ(namespace : Text) -> λ(tagNames : List Text) -> λ(b : Block) ->
  let toTag = \(name : Text) ->
                { tag_path = "${data_path}/${namespace}/tags/blocks/${name}.json"
                , value    = "${b.modId}:${b.name}" }
   in b // {tags = (map Text Tag toTag tagNames) # b.tags }

-- | TODO: Like addTagsToBlock
let addTagsToItem : ModId -> List Text -> Item -> Item
  = λ(namespace : Text) -> λ(tagNames : List Text) -> λ(i : Item) ->
  let toTag = \(name : Text) ->
                { tag_path = "${data_path}/${namespace}/tags/blocks/${name}.json"
                , value    = "${i.modId}:${i.name}" }
   in i // {tags = (map Text Tag toTag tagNames) # i.tags }

-- TODO: Call them just makeItem and makeBlock

-- | Make a simple item given a ModId and an item name
let makeSimpleItem : ModId -> Text -> Item
  = λ(modId : Text) -> λ(itemName : Text) ->
    { modId        = modId
    , name         = itemName
    , model_path   = "${assets_path}/${modId}/models/item/${itemName}.json"
    , texture_path = Some "${assets_path}/${modId}/textures/item/${itemName}.png"
    , model = { parent = "item/generated"
              , textures = Some (JSON.object (toMap { layer0 = JSON.string "${modId}:item/${itemName}" }))
              }
    , tags  = [] : List Tag
    , lang  = toMap {en_us = "${itemName}"}
    }

-- | Make a simple block given a ModId and a block name
let makeSimpleBlock : ModId -> Text -> Block
  = λ(modId : Text) -> λ(blockName : Text) ->
    { modId           = modId
    , name            = blockName
    , blockstate_path = "${assets_path}/${modId}/blockstates/${blockName}.json"
    , model_path      = "${assets_path}/${modId}/models/block/${blockName}.json"
    , texture_path    = Some "${assets_path}/${modId}/textures/block/${blockName}.png"
    , loot_table_path = "${data_path}/${modId}/loot_tables/blocks/${blockName}.json"
    , blockstate      = {variants = JSON.object [{mapKey = "", mapValue = JSON.object (toMap {model = JSON.string "${modId}:block/${blockName}"})}]}
    , model           = {parent = "block/cube_all", textures = Some (JSON.object (toMap {all = JSON.string "${modId}:block/${blockName}"}))}
    , assoc_item      = makeSimpleItem modId blockName // {model = {parent = "${modId}:block/${blockName}", textures = None JSON.Type}, texture_path = None Text}
    , loot_table      = JSON.object (toMap { type = JSON.string "minecraft:block"
                                           , pools = JSON.array [JSON.object (toMap { rolls = JSON.natural 1
                                                                                    , entries = JSON.array [JSON.object (toMap { type = JSON.string "minecraft:item"
                                                                                                                               , name = JSON.string "${modId}:${blockName}" })] })] })
    , tags            = [] : List Tag
    , lang  = toMap {en_us = "${blockName}"}
    }

-- TODO: Needed for Tags.dhall which should define the base tags to which the block values are appended
let TagDef : Type = {replace : Bool, values : List Text}
  
in {ModId, Model, Tag, Item, Block, makeSimpleItem, makeSimpleBlock, addTagsToBlock, addTagsToItem}
