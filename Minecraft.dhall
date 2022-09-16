-- TODO: Can use:
-- let JSON/Type = https://prelude.dhall-lang.org/JSON/Type
-- to type arbitrary json

let JSON = https://prelude.dhall-lang.org/JSON/package.dhall

let ModId : Type = Text

let assets_path = "./src/main/resources/assets"

let Item : Type
  = { model_path : Text
    , texture_path : Text
    , model   : { parent : Text , textures : { layer0 : Text } }
    }

-- | Make a simple item given a ModId and an item name
let makeSimpleItem : ModId -> Text -> Item
  = λ(modId : Text) -> λ(itemName : Text) ->
    { model_path   = "${assets_path}/${modId}/models/item/${itemName}.json"
    , texture_path = "${assets_path}/${modId}/textures/item/${itemName}.png"
    , model = { parent = "item/generated"
              , textures = { layer0 = "${modId}:item/${itemName}" }
              }
    }

let Block : Type
  = { blockstate_path : Text
    , blockstate : { variants : JSON.Type }
    , model_path : Text
    , model : { parent : Text, textures : JSON.Type }
    , item_model_path : Text
    , item_model : { parent : Text }
    , texture_path : Text
    }


let makeSimpleBlock : ModId -> Text -> Block
  = λ(modId : Text) -> λ(blockName : Text) ->
  { blockstate_path = "${assets_path}/${modId}/blockstates/${blockName}.json"
  , blockstate      = {variants = JSON.object [{mapKey = "", mapValue = JSON.object [{mapKey = "model", mapValue = JSON.string "${modId}:block/${blockName}"}]}]}
  , model_path      = "${assets_path}/${modId}/models/block/${blockName}.json"
  , model           = {parent = "block/cube_all", textures = JSON.object [{mapKey = "all", mapValue = JSON.string "${modId}:block/${blockName}"}]}
  , item_model_path = "${assets_path}/${modId}/models/item/${blockName}.json"
  , item_model      = {parent = "${modId}:block/${blockName}"}
  , texture_path    = "${assets_path}/${modId}/textures/block/${blockName}.png"
  }
  
in {ModId, Item, makeSimpleItem, Block, makeSimpleBlock}
