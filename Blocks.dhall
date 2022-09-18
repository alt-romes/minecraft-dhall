let MC = ./Minecraft.dhall

let modId = "tutorial"

in [ -- Simple block
     MC.makeSimpleBlock modId "new_dirt"

     -- Block with added tags to be mineable with an iron axe
   , MC.addTagsToBlock "minecraft" ["mineable/axe", "needs_iron_tool"]
       (MC.makeSimpleBlock modId "special_log")

     -- Block whose associated item has a custom translation
   , let b = MC.makeSimpleBlock modId "sand_rock" in b // { assoc_item = b.assoc_item // {lang = toMap { en_us = "Sand Rock Item", pt_pt = "Areia de Pedra" } }}

   -- TODO: overwrite loot_table example. (toJSON would be useful! (check issue))
   ]
