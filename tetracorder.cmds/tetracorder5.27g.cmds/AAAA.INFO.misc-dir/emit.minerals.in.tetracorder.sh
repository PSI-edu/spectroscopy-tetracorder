#!/bin/sh

expsys=cmd.lib.setup.t5.15a2

# EMIT to detect 10 key minerals:
# Montmorillonite, Kaolinite, Gypsum, Goethite, Calcite,
# Dolomite, Hematite, Illite, Chlorite, and Vermiculite


for i in montmorillonite kaolinite halloysite gypsum goethite calcite dolomite hematite illite muscovite chlorite vermiculite
do

	echo "============== $i ================="

	grep -i $i $expsys | grep -i  title

	echo " "

done
