let MC = ./Minecraft.dhall

let map = https://prelude.dhall-lang.org/v21.0.0/List/map.dhall

let modId = "dimod"

let simpleItems = ["teto_do_127", "ruby", "salsichas"]

in map Text MC.Item (MC.makeSimpleItem modId) simpleItems
