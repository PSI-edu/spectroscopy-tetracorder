
opalpy.py is a python program that reads specpr files and will:

   1) plot spectra
   2) show contents of ascii records.

Contribution by Eric Livo

To start, run:

python3 opalpy.py

Simplest is to make a shell script with the path to opalpy.py.

For example:

/usr/local/bin/opalpy:
#!/bin/sh

python3  /src/local/specpr/src.opal-py-python-specpr-reader-plots/opalpy/opalpy.py &

Then one can start opalpy easily from anywhere.  In the window that
pops up, select a specpr file to read.

The code also includes a specpr read routine in python.
