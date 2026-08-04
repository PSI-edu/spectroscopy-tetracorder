
# LP DAAC

EMIT reflectance data can be obtained from the Land Processes Distributed Active
Archive Center (LP DAAC), https://www.earthdata.nasa.gov

Specifically: https://www.earthdata.nasa.gov/data/catalog/lpcloud-emitl2arfl-001

This link gets to the global map for viewing:

https://search.earthdata.nasa.gov/search?fi=EMIT%20Imaging%20Spectrometer&fpb0=Space-based%20Platforms

In the search bar on the left below  EARTHDATE SEARCH, enter: EMIT

Then select on the search results:

EMIT L2A Estimated Surface Reflectance and Uncertainty and Masks 60 m V001

# EMIT reflectance cubes

As of July 2026 the EMIT reflectance data is the version 1 reflectance.
The EMIT team at JPL has done an amazing job with the atmospheric corrections. 
It is a very tough problem.  But a new version will be deployed soon
(version 2 reflectance).  The Version 1 reflectance suppressed some REE absorptions,
so REE group 21 rarely found any REE (mostly high concentrations in exposed
REE mines).  Reflectance version 2 fixes this problem so group 21 has
a better chance to detect trace REE with fewer false positives.

Both version 1 and 2 reflectance models have an artifact in some scenes near
2.3 microns resulting ine positive prehnite detections.  A simple check
for this problem is the view the

color.results+labels-jpegs/*prehnite-chlorite-mix+perchlorate+labels.jpg

image and see if any prehnite mapped.  If it does, it is most likely
due to the 2.3 micron artifact and is not real.  Where present, this artifact
may skew other mineral detections with ~2.3 micron absorptions.
This problem affects version 1 and 2 reflectance, but should be reduced
in version 2.  Because montmorillonite and muscovite have very similar 
2.2 micron absorptions, but montmorillonite does not have a ~2.35 micron
absorption but muscovite does, presence of the 2.3 micron artifact
may cause misidentification of montmorillonite as muscovite.


# EMIT processing

EMIT scenes from the LP DAAC are in *.nc format and must be converted
for use in tetracorder.

Download and install the EMIT utilities:

https://github.com/emit-sds/emit-utils

The emit utils repository provides general convenience utilities used
broadly throughout the emit-sds. 

Converting .nc files to envi files:

mkdir outputdirname

python3 /src/local/emit-utils/emit_utils/reformat.py --orthorectify  input-nc-cube  outputdirname

Example:

mkdir cuprite-emit-20230427T173257

python3 /src/local/emit-utils/emit_utils/reformat.py --orthorectify EMIT_L2A_RFL_001_20230427T173257_2311711_009.nc cuprite-emit-20230427T173257


Leave off the orthorectify flag if you do not want the cube rectified.

The derived envi scene contains an envi header that may cause
davinci to crash due to the description field that is too long.

The fix is the edit the hdr file to shorten the description field.
The script fix-emit-hdr-for-davinci does that for you.


A second problem you may encounter is that the path plus file name
may be too long for tetracorder.  Tetracorder reads command lines
capped at 80 characters and one must specify the keyword cube
before the file name.  The python script, reformat.py, produces
a long file name with RFL and reflectance in the name.
The fix-emit-hdr-for-davinci script has the option for 
deleting the _reflectance string from the filename to shorten the
total path length.

Example

    mkdir cubes-nc
    mkdir cubes

download cubes from the LP DAAC to cubes-nc

Say you downloaded EMIT_L2A_RFL_001_20230427T173257_2311711_009.nc

Convert with:

    cd cubes-nc

    python3 /src/local/emit-utils/emit_utils/reformat.py --orthorectify EMIT_L2A_RFL_001_20240805T172949_2421812_012.nc  ../cubes

    cd ../cubes

    ls -l

-rw-rw-r-- 1 rclark users 5256316560 Jul 29 12:25 EMIT_L2A_RFL_001_20240805T172949_2421812_012_reflectance

-rw-rw-r-- 1 rclark users      14473 Jul 29 12:24 EMIT_L2A_RFL_001_20240805T172949_2421812_012_reflectance.hdr

Next run fix-emit-hdr-for-davinci:

    fix-emit-hdr-for-davinci -refl EMIT_L2A_RFL_001_20240805T172949_2421812_012_reflectance.hdr

This creates:

EMIT_L2A_RFL_001_20240805T172949_2421812_012.hdr

and

EMIT_L2A_RFL_001_20240805T172949_2421812_012.hdr-orig


Next run tetracorder:

cd ..  (this puts you above the cubes directory)

    /t1/tetracorder.cmds/tetracorder6.00a.cmds/cmd-setup-tetrun  map_emit20240805T172949_lake-powell \
    emit_c  cube ../cubes/EMIT_L2A_RFL_001_20240805T172949_2421812_012   1.0 \
    -T -3 50 C  -P  0.8 1.1 bar image png  shortcubeid emlkpowl  \
    longcubeid emit-lake-powell-20240805T172949-tet6.00a5  geology

The setup will tell you how to change directory and run tetracorder:

    cd map_emit20240805T172949_lake-powell
    time  ./cmd.runtet cube  ../cubes/EMIT_L2A_RFL_001_20240805T172949_2421812_012 band 20

It is generally good to direct output to a file in case you need to check diagnostics.

For cshell or bash: 

    time ./cmd.runtet cube ../cubes/EMIT_L2A_RFL_001_20240805T172949_2421812_012 band 20 >& cmd.runtet.out &

and if you put the command in background you can follow results with

    tail tetracorder.out

or the tetracorder monitor:

    tetmon
