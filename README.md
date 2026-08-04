The code here is covered by the GNU General Public License
[https://www.gnu.org/licenses/gpl.html](https://www.gnu.org/licenses/gpl.html)

# Contents

Tetracorder, Specpr, and spectral libraries

Source Code

Background

Tetracorder 6.00, expert system 6.00a5 and spectral library update

OLDER:

Tetracorder 5.27

Tetracorder 5.27, expert system 5.27c1, and spectral library update

Tetracorder 5.27, expert system 5.27e1, and spectral library update

December 2024: New Tetracorder paper is published (open paper).

January 15, 2025:

January 15, 2025 added opal-py

April 2, 2026


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

Each Tetracorder expert system is a numbered match to the same number
source code.  For example, expert system files tetracorder5.27a2.cmds,
tetracorder5.27a.cmds, tetracorder5.27c.cmds, and tetracorder5.27e.cmds
in the tetracorder.cmds directory go with the Tetracorder 5.27 source
code and both these are frozen except for cosmetic changes that to
no affect results (e.g. spelling or printing more information to the
terminal).  If expert system content changes, a new file name with a
new directory will be added (for example after tetracorder5.27e.cmds
would be tetracorder5.27f.cmds).  Any code changes that do not parse
the expert system will result in a new series, e.g.5.28, thus tetracorder5.28
and expert system, e.g. tetracorder5.28a.cmds.

Spectral library additions will only be additions and are fully backward
compatible.


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

# Tetracorder 6.00, expert system 6.00a5 and spectral library update

Tetracorder 6 now includes derivation of geologic origins
and mineral groups and classifications.  For each reference
spectrum a list of components is given, along with a material
classification.  If classification is missing it is obtained
from geologic-origins/material-classification.txt A confidence
score is given for the identification of that mineral set, and the
classification.  confidence is for material and classification with
two numbers  N  M  with  0 to 9 scale.  A value of 1  = position
and shape match only, while 2 -9 = increasing diagnostic confidence.
Output geologic origin, group and class images are scaled 8-bit
0 - 198, 9=198 (multiply by 22).  More water, snow and melting snow
spectra in group 0 were added.  Refined thresholds in group 0.
In t6.00a5 removed entries in group 20 (REE) for a different future use.
Use group 21 for more robust REE detection.  Group 20 included
single feature REE detection which was prone to false positives from noise.

Added not features in group 4 to better distinguish between water
and broad fe2+ absorptions.

Geologic origins is an expansion of the origins first described in
Noe Dobrea et a., 2025.

Noe Dobrea, Eldar Z., Maria E. Banks, Roger N. Clark, David
Wettergreen, Amanda Hendrix, Caitlin Ahrens, Ernie Bell,
Abigail Breitfeld, Thomas F. Bristow, Sanlyn Buxner, Alberto
Candela, Margaret Hansen, Greg Holsclaw, Paul Knightly, Georgiana
Kramer, Nandita Kumari, Melissa D. Lane, Audrey Martin, McKayla
Meier, Ruby Patterson, Neil Pearson, Thomas Prettyman, Greg
A. Swayze, David Vaniman, Srinivasan Vijayarangan, Faith Vilas,
Shawn P. Wright, 2025, Rover Science Autonomy in Planetary
Exploration: Field Analog Tests, Planetary Science Journal
6:51. https://iopscience.iop.org/article/10.3847/PSJ/adaa78

Accurate geologic origins requires excellent calibration to apparent
surface reflectance.  For example, geologic origins does not work
with Moon Mineralogy Mapper (M3) data due to scattered light in the
instrument which mimics iron oxides (Clark et al., 2024)

Clark, R. N., N. C. Pearson, T. B. McCord, D. L. Domingue,
K. E. Livo, J. W. Boardman, D. P. Moriarty, A. R. Hendrix, G. Kramer,
and M. E. Banks, 2024, The Global Distribution of Water and Hydroxyl
on the Moon as Seen by the Moon Mineralogy Mapper (M3), Planetary
Science Journal, 5:198, 32pp. https://doi.org/10.3847/PSJ/ad5837

The tetracorder setup includes new parameters.  Please read
tetracorder.cmds/tetracorder6.00a.cmds/AAAAA.example.commands.to.analyze.image.cubes.txt




# OLDER:

# Tetracorder 5.27

June 2022: Tetracorder 5.27 released.  The 5.27 code introduces a new
spectral feature class, class M.  Class D, diagnostic, could still find
a material based on other features if a diagnostic feature was disabled.
The result may not be correct.  The condition was discovered in AVIRIS
data when the reflectance calibration had a large deleted zone around
the 1.4 and 1.9-micron telluric water bands.  For example, in group 1,
the right continuum point for azurite (copper carbonate) was inside
the deleted channel zone and the main diagnostic feature was disabled.
Identification fell to a less diagnostic and less unique feature resulting
in widespread mapping of azurite, but that mapping was a false positive.
Tetracorder 5.27 with the M spectral class is Must Have Diagnostic
feature and if the feature is disabled, the material will not be found.
The 5.27 expert system has been updated where multiple materials now
use the class M diagnostic.  Testing in multiple geologic environments
shows the false positive rate is vastly reduced.

A study was conducted on the ID of snow+vegetation and the expert system
was adjusted.  Some plants have shifted water bands that are similar
to those in snow+vegetation spectra.  The false positive rate for
snow+vegetation is now reduced.  If you know the temperature range of
your scene, setting the temperature will help reduce the false positive
snow detection if temperatures are above freezing.

Tetracorder 5.27 includes additional spectra for mapping materials in
the 3 to 4-micron range.  Specifically weak carbonate signatures are
now included.

The color map products for the 2-micron spectral features have also
been improved with better color to distinguish minerals better.  A map
of muscovite composition has been added.

# Tetracorder 5.27, expert system 5.27c1, and spectral library update

January 2023: 
Minor updates to Tetracorder 5.27, mostly debugging and printing
information for single-spectrum mode including improvements for audio
output for what tetracorder finds in a spectrum.  Before audio output
is effective, the wav files for each found material needs to be updated
(planned for this year).

This update also expanded the number of spectral groups and the spectral range
out to 5 microns with definitions to go to 20+ microns.  To do this,
the group arrays needed to be increased and that made tetracorder go
beyond the small memory model in linux.  The linux memory model has a
2-gigabyte limit in default compile mode and I hit that.  Before crossing
that limit with new compiler flags and new memory model, I want to to do
some performance testing below and above the limit.  In the meantime, I
reduced some array sizes that were far larger than needed, so everything
fits just fine.

The 5.27c1 expert system in the tetracorder.cmds directory has been
expanded to 5 microns, though more population of reference features
is needed.  Also many improvements were made based on mapping with
the first EMIT data from the International Space Station, and the NASA
SSERVI TREX field campaigns from October 2022, including results from
the Carnegie Melon Zoe rover which is running tetracorder (with Ubuntu
Linux) providing real-time analysis from a visible-near-IR spectrometer.

BE SURE TO READ THE AAAAA.KNOWN-ISSUES.txt file:

    t1/tetracorder.cmds/tetracorder5.27c.cmds/AAAAA.KNOWN-ISSUES.txt

And if you find errors or ways to improve the expert system, please let me know.

For the 5.27c1 improvements, several spectra were added to the sprlb06a spectral
library.  That meant the spectral libraries needed to be be re-convolved.
Rather than do updates that can take extra space, I have elected to try
a new method: delete the old libraries from github and install the new.
The new libraries are completely compatible will all older versions of
tetracorder and expert systems, because we only add new spectra and never
modify existing ones.

April 26, 2023: A condition was found when mapping an EMIT scene in Morocco, 
and a larger than plausible amount of jarosite and gypsum.
See the tetracorder.cmds/tetracorder5.27c.cmds/AAAAA.KNOWN-ISSUES.txt
file for details.  A mitigation study is underway.

NOTE on adding entries to the spectral libraries.
When new entries are added to the spectral libraries, the convolved libraries
need to be regenerated.  There is no need to store changes, as only additions
are added and they are always fully backward compatible.  Thus to update
the libraries, the entire library directory is removed from github
and the new one added.  This prevents huge data volume growth that is not needed.

April 28, 2023: updated the rlib06 library and convolved libraries by adding
3 new spectra.

# Tetracorder 5.27, expert system 5.27e1, and spectral library update

November 30, 2023: Updated the spectral libraries, including more REE
reference spectra and other minerals to solve issues identified in EMIT
mapping in various locations around the Earth.  To use the new spectra,
and mitigate the known problems, Tetracorder Expert System 5.27e1 is
released with this update.  For example, some areas were found to map
as jarosite when the spectra showed a mixture of hematite and goethite
of specific fine grain sizes.  One reference spectrum of such a mixture
was added that greatly reduced the misidentification, but more reference
spectra are needed and will be added when appropriate samples are found.
REE mapping is significantly improved with the new 5.27e1 expert system.
Also new in the 5.27e release is the 4 abundance models developed by
the EMIT team and documented in Clark et al, 2023.  Model 4 is being
adopted by the EMIT team moving forward with the reprocessing of EMIT
starting in December.

Clark, R. N., Swayze, G. A., Livo, K. E., Brodrick, P., Noe Dobrea, E.,
Vijayarangan, S., Green, R.  O., Wettergreen, D., Garza, A. C., Hendrix,
A., García-Pando, C. P., Pearson, N., Lane, M., González- Romero, A.,
Querol, X. and the EMIT and TREX teams. 2023, Imaging spectroscopy: Earth
and planetary remote sensing with the PSI Tetracorder and expert systems:
from Rovers to EMIT and Beyond, Planetary Science Journal, in review.


# December 2024: New Tetracorder paper is published (open paper).

Clark, R. N., G. A. Swayze, K. E. Livo, P. Brodrick, E. Noe
Dobrea, S. Vijayarangan, R. O. Green, D. Wettergreen,
A. Candela, A. Hendrix, C. P. Garcia-Pando, N. Pearson,
M. D. Lane, A. Gonzalez-Romero, X. Querol, and the EMIT and
TREX teams, 2024, Imaging spectroscopy: Earth and planetary
remote sensing with the PSI Tetracorder and expert systems
from Rovers to EMIT and Beyond Planetary Science Journal 5:276,
48pp. https://iopscience.iop.org/article/10.3847/PSJ/ad6c3a

# January 15, 2025:

Updated the spectral libraries.  Added new reference spectra to the rlib series
and added AVIRIS3 2025 convolved libraries.

Updated tetracorder 5.27: Improved diagnostic output in single spectrum mode
and added a disable groups directory that the user can add groups they
deem unimportant.  This will speed run times by not doing calculations
that are not useful.  For example, imaging spectrometers measuring through
the Earth's atmosphere, like AVIRIS or EMIT can't measure quality data
in the 1.4 and 1.9 micron atmospheric water bands.  Therefore any 
material ID using these wavelength ranges (e.g. groups 13, 14, 19)
do not need to be calculated, and if results are reported, the answer
is likely biased and wrong.  With the disable feature, these groups
(and others not needed) can be disabled before the image cube
analysis is started.  While the disable file will be automatically
implemented in expert system 5.27f1, it can be added to previous expert
systems by adding in the mapping directory a file called:

DISABLE/force-disable.txt

with entries like:

DISABLE grp 14

DISABLE cse  7

Changed Tetracorder makefile to use medium memory model so larger arrays and more reference
materials can be used.

# January 15, 2025 added opal-py

The opal-py is a new program contributed by Eric Livo for reading
specpr format files, making plots of spectra and displaying contents
of specpr text records.

If you want a python specpr file reading routine, check here:

specpr/src.opal-py-python-specpr-reader-plots

# April 2, 2026

Spectral libraries for several instruments added, including AVIRIS classic 2019 thru 2024.

Tetracorder source code expanded to handle the increased size of Tetracorder
the expert system 5.27g series.

Added Tetracorder expert system 5.27g.  This expert system is the starting candidate
for EMIT recalculations that will use an updated reflectance model.  Expert system
5.27g2 includes fixes to improve identification and mapping accuracy from our experience
mapping the world with EMIT data, and further verifications with AVIRIS data.
Expert system 5.27g2 is the starting point for Tetracorder 6.00 that will be
used for the EMIT recalculations with new capabilities based on
Noe Dobrea et al., 2025.  REE Group 20 in 5.27g2 will be removed in
6.00a3 and used for something else in the future, and REE group 21 will
remain as the main REE detection group.

AS ALWAYS, all Tetracorder results should be verified for accuracy, and the 
AAAAA.KNOWN-ISSUES.txt file read in full to understand limitations.
Small errors in reflectance model accuracy can affect identifications.

Noe Dobrea, Eldar Z., Maria E. Banks, Roger N. Clark, David
Wettergreen, Amanda Hendrix, Caitlin Ahrens, Ernie Bell,
Abigail Breitfeld, Thomas F. Bristow, Sanlyn Buxner, Alberto
Candela, Margaret Hansen, Greg Holsclaw, Paul Knightly, Georgiana
Kramer, Nandita Kumari, Melissa D. Lane, Audrey Martin, McKayla
Meier, Ruby Patterson, Neil Pearson, Thomas Prettyman, Greg
A. Swayze, David Vaniman, Srinivasan Vijayarangan, Faith Vilas,
Shawn P. Wright, 2025, Rover Science Autonomy in Planetary
Exploration: Field Analog Tests, Planetary Science Journal
6:51. https://iopscience.iop.org/article/10.3847/PSJ/adaa78

