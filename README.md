# voxel-radiant

Create maps like dretchstorm for Unvanquished, without using netradiant.

## Dependencies

To run this, you will need:
`ocaml`

## Quick Start

Use this command:

```sh
./voxel-radiant.ml input.vox map-example_src.dpkdir
```

This will create a file `map-example_src.dpkdir/maps/example.map`, and
some files `map-example_src.dpkdir/maps/example*.navcon`. You can open
`map-example_src.dpkdir/maps/example.map` in netradiant,
and compile it with q3map2.

## Tutorial

A map is specified in a `.vox` file. Let us look at a few examples in
detail. Create a file `tutorial.vox`, with this content:

```
++
 +
 ++
```

Then use this command to create a file
`map-example_src.dpkdir/maps/example.map`: 

```sh
./voxel-radiant.ml input.vox map-example_src.dpkdir
```

The map is composed of 5 basic rooms which we shall call "cells". By
default, each cell is a cube 256 quake units wide. A
`+` character denotes an empty cell. A space character (or no more
characters in a line) denotes the abscence of a cell.

Once the file `example.map` is created, you can compile it to a playable
map using [q3map2](https://gitlab.com/xonotic/netradiant). Look at the
file `run-q3map2.sh` for details on how to do this on Linux. If you are
on Windows or MaxOS, the commands will only require slight adjustment
of the paths.

After compilation, you can load this map with the Daemon Engine and
Unvanquished with this command:

```sh
daemon -set g_neverend 1 +devmap example
```

Notice that we had to force the game to never end, as there are no
spawns yet. The Reactor is placed with the character `R`.
Some Telenodes are automatically placed next to it.
Eggs are placed with the characters `< > ^ v`, indicating
at which of the 4 walls the eggs should be placed. The Overmind is
created placed with `O`. Change `tutorial.vox`:

```
+R
 +
 vO
```

Load the map with

```
daemon +devmap example
```

Here is a screenshot:

![screenshot](https://github.com/sweet235/voxel-radiant/blob/main/doc/screenshot0.jpg?raw=true)

Let us make the map bigger now. Change `tutorial.vox`:
```
    +RA+
     ++
     ++
   ^^++
   ++++
   +++
   ++
   ++^^
   ++++
     ++
    ^++
   ^+++
   ++++
   ++ 
   +++
   +++^^
   +++++
O> ++  +
 >     +
 > ^+> +
 B + + +
 ++> +^+
```

Here, we used the characters `A` to place an armoury, and `B` to place
a booster. The human side of the map looks like this now:

![screenshot](https://github.com/sweet235/voxel-radiant/blob/main/doc/screenshot1.jpg?raw=true)

This map is still two dimensional in essence. You can add vertical
layers by separating them with the keyword `#ply`:

```
    
     ++
     ++
     + 
   ^
   +
   +
#ply
    +RA+
     ++
     ++
   ^^++
   ++++
   +++
   ++
   ++^^
   ++++
     ++
    ^++
   ^+++
   ++++
   ++ 
   +++
   +++^^
   +++++
O> ++  +
 >     +
 > ^+> +
 B + + +
 ++> +^+
```

Ladders are added automatically to aid human movement. The map looks
like this now:

![screenshot](https://github.com/sweet235/voxel-radiant/blob/main/doc/screenshot2.jpg?raw=true)

## File Syntax

At the start of a `.vox` file, there may be configuration lines to
change some aspects of the generated map. These are:

```
#cell_dim 512 256 256
```

Change the size of the cells.

```
#wall_tex shared_pk02/wall04b 0.25 0.25 0 0
```

Change the wall texture. Use scaling 0.25 in both directions, and
offsets of 0.

```
#floor_tex shared_tech/floortile1b 0.5 0.5 0 0
#ceiling_tex shared_tech/floortile1b 0.5 0.5 0 0
#sky_tex shared_space/sky25 1 1 0 0
```

Change textures for floor, ceiling and sky.

```
#ladders off
```

Do not create ladders.
