let MC = ./Minecraft.dhall
let JSON = https://prelude.dhall-lang.org/JSON/package.dhall

let modId = "tutorial"

in [ -- Simple item
   , MC.makeSimpleItem modId "ruby"

   , MC.makeSimpleItem modId "silver" // { lang = toMap {en_us = "Silver", pt_pt = "Prata"} }

     -- Simple item, but the model is overwritten
   , MC.makeSimpleItem modId "silver_ore_item" // { model = { parent = "..."
                                                            , textures = Some (JSON.object (toMap {all=JSON.string "..."}))} }
   ]
