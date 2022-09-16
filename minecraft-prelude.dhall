let Item : Type
  = { parent : Text
    , textures : { layer0 : Text }
    }

let makeItem : Text -> Text -> Item
    = λ(modId : Text) -> λ(itemName : Text) ->
    { parent   = "item/generated"
    , textures = { layer0 = "${modId}:item/${itemName}" } }
  
in {Item, makeItem}
