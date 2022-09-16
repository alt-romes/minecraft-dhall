let ModId : Type = Text

let Item : Type
  = { fpath : Text
    , val   : { parent : Text
              , textures : { layer0 : Text }
              }
    }

-- | Make a simple item given a ModId and an item name
let makeSimpleItem : ModId -> Text -> Item
    = λ(modId : Text) -> λ(itemName : Text) ->
    { fpath = "./resources/assets/${modId}/models/item/${itemName}.json"
    , val  = { parent = "item/generated"
             , textures = { layer0 = "${modId}:item/${itemName}" }
             }
    }

  
in {ModId, Item, makeSimpleItem}
