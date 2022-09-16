let map = https://prelude.dhall-lang.org/v21.0.0/List/map.dhall
let keyValue = https://prelude.dhall-lang.org/v21.0.0/JSON/keyValue.dhall
let JSON = https://prelude.dhall-lang.org/v21.0.0/JSON/package.dhall

let MC = ./minecraft-prelude.dhall

let modId = "dimod"

let items = ["teto_do_127", "ruby", "salsichas"]

in map Text
       (List { mapKey : Text, mapValue : MC.Item })
       (\(itemName : Text) ->
        let item = MC.makeItem modId itemName
         in [keyValue MC.Item "/resources/assets/${modId}/models/item/${itemName}.json" item])
       items
