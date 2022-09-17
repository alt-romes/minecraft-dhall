Minecraft Dhall
===

A configuration tool for developing minecraft mods without JSON hell.

This is a draft/work in progress!

## How to use

At the root of your minecraft mod you should add the following files.

* `Items.dhall` describes all mod items. It should return a list of `Item`s. One
    can create `Item`s by hand, or using `makeSimpleItem` and passing the
    `ModId` (just a string) and item name to it.

Example `Items.dhall`:
```dhall
let JSON = https://prelude.dhall-lang.org/JSON/package.dhall

let MC = https://raw.githubusercontent.com/alt-romes/minecraft-dhall/master/Minecraft.dhall

let modId = "tutorial"

in [ -- Simple item
   , MC.makeSimpleItem modId "ruby"

     -- Simple item, but the model is overwritten
   , MC.makeSimpleItem modId "silver_ore_item" // { model = {parent = "...", textures = Some (JSON.object (toMap {all=JSON.string "..."}))} }


   ]
```

* `Blocks.dhall` describes all mod blocks. It should return a list of `Block`s.
    They can be created by hand or using `makeSimpleBlock` and passing a `ModId`
    and a block name.

Example `Blocks.dhall`:
```dhall
let MC = https://raw.githubusercontent.com/alt-romes/minecraft-dhall/master/Minecraft.dhall

let modId = "tutorial"

in [ -- Simple block
     MC.makeSimpleBlock modId "new_dirt"

     -- Block with added tags to be mineable with an iron axe
   , MC.addTagsToBlock "minecraft" ["mineable/axe", "needs_iron_tool"]
       (MC.makeSimpleBlock modId "special_log")
   ]
```

Finally, run `minecraft-dhall` to generate the directory tree and the items and
blocks configuration JSONs. Make sure you add the textures as needed, but don't
worry if you make a mistake in doing so, for `minecraft-dhall` shall warn you
about the missing textures.
