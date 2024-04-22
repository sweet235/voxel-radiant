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

For technical reasons, the generated map does not pass q3map2's leak
test. This is because it is missing the "intermission". You need to
place a reactor on a cell for voxel-radiant to place the intermission
there. This is done by replacing a `+` by `R`:

```
+R
 +
 ++
```

After compilation, you can load this map with the Daemon Engine and
Unvanquished with this command:

```sh
daemon -set g_neverend 1 +devmap example
```

Notice that we had to force the game to never end, as there are no
eggs yet. There are some telenodes, as these are placed automatically
next to the reactor. Eggs are placed with the characters `< > ^ v`, indicating
at which of the 4 walls the eggs should be placed. The Overmind is
created placed with `O`. Change `tutorial.vox`:

```
+R
 +
 vO
```

![screenshot](https://github.com/sweet235/voxel-radiant/blob/master/doc/screenshot0.jpg?raw=true)
