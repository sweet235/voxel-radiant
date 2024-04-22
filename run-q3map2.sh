mapname="example"
map="map-${mapname}_src.dpkdir/maps/${mapname}.map"
pakpath="$HOME/.local/share/unvanquished/pkg"
threads=`nproc`

q3map2 -threads $threads -game "unvanquished" -fs_pakpath $pakpath -fs_pakpath map-${mapname}_src.dpkdir -fs_nohomepath -bsp -leaktest -custinfoparms -meta -maxarea -samplesize 8 "$map"
q3map2 -threads $threads -game "unvanquished" -fs_pakpath $pakpath -fs_pakpath map-${mapname}_src.dpkdir -fs_nohomepath -vis -saveprt "$map"
q3map2 -threads $threads -game "unvanquished" -fs_pakpath $pakpath -fs_pakpath map-${mapname}_src.dpkdir -fs_nohomepath -light -nocollapse -fastbounce -fastallocate -nobouncestore -shade -dirty -dirtscale 0.8 -dirtdepth 32 -patchshadows -samples 3 -samplesize $threads -randomsamples -bouncegrid -bounce 16 -deluxe -lightmapsize 1024 -external "$map"
q3map2 -game "unvanquished" -fs_pakpath $pakpath -fs_nohomepath -minimap "$map"
