let MC = ./Minecraft.dhall

let modId = "dimod"

let block_n2 = MC.addTagsToBlock "minecraft" ["mineable/pickaxe", "needs_diamond_tool"]
              (MC.makeSimpleBlock modId "block_with_tags")

in [block_n2]
