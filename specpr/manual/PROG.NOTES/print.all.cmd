#!/bin/sh

# Roger N. Clark 2/28/2008

# -l60 is for Roger's lz5rc printer

pr -o8 -l60  Contents 		| lz5rc
pr -o8 -l60  alias		| lz5rc
pr -o8 -l60  print.all.cmd	| lz5rc
pr -o8 -l60  specpr_file_or	| lz5rc
pr -o8 -l60  spform		| lz5rc
