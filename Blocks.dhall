let MC = ./Minecraft.dhall

let modId = "tutorial"

in [ -- Simple block
     MC.makeSimpleBlock modId "new_dirt"

     -- Block with added tags to be mineable with an iron axe
   , MC.addTagsToBlock "minecraft" ["mineable/axe", "needs_iron_tool"]
       (MC.makeSimpleBlock modId "special_log")

   -- TODO: overwrite loot_table example. (toJSON would be useful! (check issue))
   ]
