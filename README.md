Minecraft Dhall
===

A configuration tool for developing minecraft mods without JSON hell.

## How to use

At the root of your minecraft mod you should add the following files.

* `Items.dhall` describes all mod items. It should return a list of `Item`s. One
    can create `Item`s by hand, or using `makeSimpleItem` and passing the
    `ModId` (just a string) and item name to it.

Example `Items.dhall`:
```dhall
let MC = https://raw.githubusercontent.com/alt-romes/minecraft-dhall/master/Minecraft.dhall

let modId = "tutorial"

in [makeSimpleItem modId "item_n1", makeSimpleItem modId "item_n2"]
```

* `Blocks.dhall` describes all mod blocks. It should return a list of `Block`s.
    They can be created by hand or using `makeSimpleBlock` and passing a `ModId`
    and a block name.
```dhall
let MC = https://raw.githubusercontent.com/alt-romes/minecraft-dhall/master/Minecraft.dhall

let modId = "tutorial"

in [makeSimpleBlock modId "block_n1", makeSimpleItem modBlock "block_n2"]
```

Finally, run `minecraft-dhall` to generate the directory tree and the items and
blocks configuration JSONs. Make sure you add the textures as needed, but don't
worry if you make a mistake in doing so, for `minecraft-dhall` shall warn you
about the missing textures.
