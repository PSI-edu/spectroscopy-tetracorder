
The code here is covered by the GNU General Public License
[https://www.gnu.org/licenses/gpl.html](https://www.gnu.org/licenses/gpl.html)

# Tetracorder, Specpr, and spectral libraries

**Tetracorder** is a spectral identification and mapping system designed for 
mapping materials on solid surfaces in the Solar System, including the Earth,
other planets and satellites, asteroids and comets.  Tetracorder can also be
used in laboratory spectroscopy.  Tetracorder has 2 modes: 1) identification
of components in a single spectrum, and 2) identification and spatial mapping
using imaging spectrometer data.

**Specpr** (Spectrum Processing Routines) is a spectral analysis system
for analyzing single spectra.  Tetracorder uses many specpr subroutines
and it is required to be compiled before tetracorder.

**Training Videos** are available on the Planetary Science Institute
youtube channel:
[https://www.youtube.com/user/PSITucson/featured](https://www.youtube.com/user/PSITucson/featured).
Go down to "Spectroscopy and Tetracorder Training" and click on "PLAY ALL"
to see all the training videos (listed on the right).

The source code is located in the tetracorder5.26 and specpr directories.

The required spectral libraries are in the sl1 directory and includes 
code and instructions to convolve the spectral library to other instruments.

The etc directory contains environment variable definitions for bash and tcsh.

The tetracorder.cmds directory includes the tetracorder expert system that does the
identification, and all the files needed to do an analysis on a dataset.
For the system to operate, the spectral libraries must be convolved to the
spectral range and resolution of the instrument supplying the data, including
files with pointers to the convolved libraries so that tetracorder knows
which libraries to use for a given instrument.

The cuprite95 directory contains the results from a tetracorder run on NASA AVIRIS
cuprite 1995 data that was calibrated to apparent surface reflectance.  You can get the
image cube from the USGS spectroscopy lab ftp site (see the README-image-cube.txt
file in the cuprite95 directory for the location).  Then you can run tetracorder
and confirm that you get the same results.  See the training videos for how to evaluate
results.

The tetracorder system also requires
[Davinci, available from Arizona State U here](http://davinci.asu.edu/index.php?title=Main_Page)

Tetracorder, Specpr, and support programs run on linux and unix.

# Source Code

Github does not have the languages right.  The specpr and tetracorder programs are
fortran, ratfor, and C.  Support programs are mostly davinci and shell scripts, with
some fortran/ratfor and C programs.  HTML is documentation in the spectral libraries.
Spectral libraries are binary data.

# Background

Detection and mapping of minerals, vegetation species, chemicals,
liquids, and solids is being done through the field of imaging
spectroscopy. Imaging spectrometers are deployed on aircraft, spacecraft
(throughout the Solar System) in the field, and in laboratories. Imaging
spectrometers are narrow-band imagers with hundreds of wavelengths
(bands) and usually include ultraviolet to infrared. Imaging spectrometers
collect a spectrum at each image pixel with enough spectral resolution to
resolve absorption and emission features in spectra of compounds, whether
crystalline or amorphous solids, liquids and gases. What can be detected
remotely is mainly a function of spectral range and resolution. Analysis
of imaging spectrometer data sets is quite complex.
The Tetracorder system was first described in
detail in Clark, R.N., Swayze, G.A., Livo, K.E., Kokaly, R.F.,
Sutley, S.J., Dalton, J.B., McDougal, R.R., and Gent, C.A., 2003,
Imaging spectroscopy: Earth and planetary remote sensing with the
USGS Tetracorder and expert systems, Journal of Geophysical Research,
Vol. 108(E12), 5131, doi:10.1029/2002JE001847, p. 5-1 to 5-44.

Tetracorder is in use analyzing data from all over the Solar System,
including mapping ice and other compounds on icy satellite surfaces in
the Saturn and Jupiter systems, minerals on Mars, and was critical in
making the discovery of widespread water on the Moon possible. Tetracorder
is used for mapping ecosystems, and in rapid response to environmental
disasters. It was used in assessing the environmental damage from the
World Trade Center disaster and the the 2010 Deepwater Horizon oil spill
in the Gulf of Mexico.

Tetracorder uses multiple algorithms, including spectral fitting
procedures to identify materials, and derives feature strengths (relative
abundance) of those materials. It has demonstrated discrimination
of grain sizes for some materials using the shape of the absorption
features. The grain sizes and feature strengths can then be fed into
radiative transfer models to derive component abundances.

Tetracorder 5.x+ adapts to both environmental conditions as well
as instrument capability. Previous versions had to be adapted by an
expert spectroscopist for each sensor and environment the data came
from. Tetracorder produces maps of hundreds of materials, including
chemical substitutions in some minerals. Imagine doing chemistry
remotely, whether across a canyon, from a high point overlooking
an environmental disaster, or a studying remote planets. This is now
possible. Further, Tetracorder results can be made into custom color-coded
maps automatically.

Tetracorder is the mineral identification used used for the NASA
[Earth Surface Mineral Dust Source Investigation (EMIT)](https://earth.jpl.nasa.gov/emit/)
imaging spectrometer
instrument that will go on the International Space Station in June
2022. EMIT will determine the mineral composition of natural sources
that produce dust aerosols around the world. By measuring in detail which
minerals make up the dust, EMIT will help to answer the essential question
of whether this type of aerosol warms or cools the atmosphere.

# Tetracorder 5.27

June 2022: Tetracorder 5.27 released.  The 5.27 code introduces a new spectral feature class,
class M.  Class D, diagnostic, could still find a material based on other features if
a diagnostic feature was disabled.  The result may not be correct.  The condition was
discovered in AVIRIS data when the reflectance calibration had a large deleted zone around
the 1.4 and 1.9-micron telluric water bands.  For example, in group 1, the right continuum
point for azurite (copper carbonate) was inside the deleted channel zone and the main
diagnostic feature was disabled.  Identification fell to a less diagnostic and less unique
feature resulting in widespread mapping of azurite, but that mapping was a false positive.
Tetracorder 5.27 with the M spectral class is Must Have Diagnostic feature and if the
feature is disabled, the material will not be found.  The 5.27 expert system has been updated
where multiple materials now use the class M diagnostic.  Testing in multiple geologic 
environments shows the false positive rate is vastly reduced.

A study was conducted on the ID of snow+vegetation and the expert system was adjusted.  Some
plants have shifted water bands that are similar to those in snow+vegetation spectra.
The false positive rate for snow+vegetation is now reduced.  If you know the temperature
range of your scene, setting the temperature will help reduce the false positive snow
detection if temperatures are above freezing.

Tetracorder 5.27 includes additional spectra for mapping materials in the 3 to 4-micron
range.  Specifically weak carbonate signatures are now included.

The color map products for the 2-micron spectral features have also been improved
with better color to distinguish minerals better.  A map of muscovite composition has been added.
