



     SPECPR User's Manual                      Page 1.1


_1.  _I_N_T_R_O_D_U_C_T_I_O_N

     The SPECtrum Processing Routines  (SPECPR)  is  a  very
large  scale interactive program for general one dimensional
array processing.  The program began at the  M.I.T.  Wallace
Observatory,  on  a Harris 2024 computer, as a short routine
to subtract and divide 2 spectra  from  the  Remote  Sensing
Laboratory   Circular  Variable  filter  spectrometer  ("The
Wedge").   Due to lack of processing software,  the  program
grew to fill the Wedge data reduction needs.  In June, 1977,
the Remote Sensing Lab (RSL)  moved  to  the  Institute  for
Astronomy  of the University of Hawaii.  At that time, there
was no software for reduction  of  spectral  data.   It  was
decided  that the Wedge data reduction program be put on the
new computer (TI 980 B) as a general spectrum and one dimen-
sional array processing system.  In 1980 it was decided that
the TI 980 B was overworked and that SPECPR would  be  moved
to  the  newly  acquired  LSI  11/23 system running the UNIX
operating system.

     The TI 980 B and LSI 11/23 versions are based on 16 bit
words  whereas  the  MIT  version was based on a 24 bit word
machine.  Also, since the program was not planned completely
from  the  start, some of the letter codes may seem obscure.
During the change to the TI 980 B, many of the letter  codes
were  changed to make more sense.  Also during the change to
the LSI version some of the routines were rewritten in  RAT-
FOR  and  the  program  was broken up into about 25 separate
programs due to the lack of a decent overlay linker for  the
fortran compiler.

     One of the main goals of the program is to be  uncrash-
able.   Since  users sit at the terminal for many hours pro-
cessing data, they are bound to  make  mistakes  so  another
goal  is for the program to be somewhat "intelligent" and to
try and catch mistakes.  The program is completely free for-
mat  allowing  quick and easy input of commands.  Spaces are
only required between two numbers where there is no  charac-
ter  between them.  Otherwise, spaces can be completely left
out or inserted wherever the user wishes.  Most commands are
a  single  character.  In many batch processing systems, the
user types commands on cards, and then  feeds  them  into  a
machine  for  processing.   If  there was a mistake, the job
must be run again, and this wastes time.  In the interactive
version,  as  the  commands  are typed in, they are executed
immediately and the results displayed.   In  this  way,  all
intermediate  steps  are  seen, and decisions can be made to
change the processing in order to obtain the  best  results.
If viewing intermediate steps is not required, many parts of
the program can be run in a semi-batch mode which is  nearly
as fast as the user can type.  In the Unix version, commands
can be read from a file  starting  and  terminating  at  any
point   in   the  program,  thus  giving  batch  capability.


                      January 26, 1984







     SPECPR User's Manual                      Page 1.2


Commands can be saved in a text file as they are  typed  in.
Then,  if a mistake is made, the text file can be edited and
executed again.  Frequently, when processing is complete and
the  user  is  looking  at  the  data,  he/she needs to know
exactly what the program did or was told  to  do.   Thus,  a
complete  history (within reason) is kept of each operation.
This has proved invaluable for figuring out what was done to
some data whether it is yours or someone else's.

     An interactive "smart" program of this nature  requires
an  almost  shocking  amount  of  code.   SPECPR is now over
26,000 lines of FORTRAN in length, contains about  313  sub-
routines,  and  is linked in an overlay structure containing
over 100 overlays in about 25 programs that call each other.
However, to the user, Specpr appears as one program.

     The following article describes the spectrum processing
system  as of 1979.  Since 1979, several major features have
been added and are discussed below.  These new features have
been  added since the program was converted to run under the
UNIX operating system on a Digital Equipment Corp. (DEC) LSI
11/23 computer.

                   New Features of SPECPR

     During the changeover from the TI-980 computer  to  the
LSI  11/23 running the UNIX operating system, several impor-
tant new features have been added to SPECPR.  These  changes
include modifications to the file assignment procedures, the
addition of a mathematical parsing system, and modifications
to  the  free format input routine to allow the user to save
often used commands in a command file, to read input from  a
disk file, and to copy all user input to a disk file.

                Mathematical Parsing System

     A mathematical parsing system has been added  to  allow
the  user  to  type in complicated equations for evaluation.
For example, one would type in
                 (v322^2+v327^2+v326^2)^0.5
to calculate the RMS value of three spectra.

                 Free Format Input Routine

     The free format input routine has been modified so that
it  keeps  a  record of the last twenty commands typed in by
the user, and allows the user to  re-execute  any  of  these
commands.  The system also allows the user to store up to 80
commonly used commands so that the user  can  execute  these
with  a minimum of typing.  Every point of input to the pro-
gram (e.g. a title input for a  spectrum)  is  performed  by
this  routine.   Another feature of the input routine allows
the user to specify a  disk  file  from  which  the  program


                      January 26, 1984







     SPECPR User's Manual                      Page 1.3


should read commands.  This allows batch input to begin any-
where in the program.  Another important feature is variable
substitution.   Anywhere on an input line "variables" may be
substituted.  The "variable" is one  of  100  saved  command
lines.   All  command  files  and  the saved commands can be
edited with any of the UNIX system editors.  Also any  other
program (system or user) can be executed at any point in the
program (e.g. mail a message,  edit  a  file,  or  even  run
another  SPECPR).  When the program is finished, the user is
returned to the exact point at which he left SPECPR.   These
new features provide extremely powerful processing capabili-
ties, save typing, increase speed, and reduce mistakes.

              Special Function for Data Input

     A special function was created to read data from a text
file  in  any format and also write data to a text file in a
known format.  The advantage of this routine is  that  users
can  write  new spcialized programs without learning SPECPR.
For example, after entering the  function,  the  user  would
instruct  the  program  to write the spectral data to a file
(say its name is "filea").  Then he could  run  the  program
(e.g. called "program") by:
                   !program <filea >fileb
Program reads data from "filea" and writes data to  "fileb".
The  "!"  is  the  escape from SPECPR to run system and user
programs.  The program can be as short as 5  lines  in  FOR-
TRAN:   a read, compute, write, a format statement, and end.
Of course, the program can  be  any  language  (Basic,  "C",
etc.)

     Once "program" has written the data, the SPECPR routine
is  then  instructed to read fileb, the data is then plotted
and written.  New applications can be programmed in minutes.

                      File Assignment

     The version of SPECPR which runs on the TI-980  used  a
set  of  fixed  file names for storing the users data.  This
restriction has been removed in the UNIX version of  SPECPR.
The user can now access any file on the UNIX system.











9

9                      January 26, 1984



