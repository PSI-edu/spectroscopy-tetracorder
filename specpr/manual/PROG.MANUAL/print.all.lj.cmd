#!/bin/sh

# Print commands on HP-UX.  Roger N. Clark Feb 28. 2008

for i in 	\
	Cover	\
	sec1	\
	sec2	\
	sec3	\
	sec4	\
	sec5	\
	sec6	\
	sec7	\
	sec8
do
	echo "tbl $i | neqn | nroff -ms -Tlj | lz5rc "
	tbl $i | neqn | nroff -ms -Tlj | lz5rc 
done
