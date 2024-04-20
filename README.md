# voxel-radiant

Create maps like dretchstorm for Unvanquished, without using netradiant.

## Dependencies

To run this, you will need:
`ocaml`

## How to create a .map file

Use these commands:

```sh
mkdir map-example_src.dpkdir/maps
./voxel-radiant.ml input.vox map-example_src.dpkdir
```

This will create a file `map-example_src.dpkdir/maps/example.map`, and
some files `map-example_src.dpkdir/maps/example*.navcon`. You can open
`map-example_src.dpkdir/maps/example.map` in netradiant,
and compile it with q3map2.

