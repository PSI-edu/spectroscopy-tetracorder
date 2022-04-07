



     SPECPR User's Manual                      Page 9.1


_9.  _C_R_T _P_L_O_T _R_O_U_T_I_N_E_S

_9._1.  _C_R_T _P_l_o_t _R_o_u_t_i_n_e_s

     The CRT plot routines are called from the completion of
a  Math  Operation  (section  8)  or  from the File Display,
Transfer and Overlay routines (section  10).   Data  can  be
plotted  in channel, wavelength (axis labeled:  microns), or
energy (axis labeled: inverse microns) space.  The data  can
be  plotted on the line printer in channel space or the data
listed.  Spikes or glitches can be removed, and  the  header
information can be displayed and changed.



_9._2.  _P_l_o_t_t_i_n_g _S_p_a_c_e

     The plot mode is selected by the letters _h for channel,
_a  for  wavelength,  or  _n  for energy space followed by the
wavelength record number.  If no number  is  specified,  the
default  is  the current record number.  A wavelength record
number is given for channel space since the wavelength  data
set  also contains the number of channels.  Note the charac-
ters chosen are the second letter  of  channel,  wavelength,
and  energy  since the first letters are used for other com-
mands.  The program tries to choose round numbers  and  tick
marks for the horizontal and vertical axes regardless of the
data range.



_9._3.  _C_h_a_n_g_i_n_g _S_c_a_l_e

     The minimum and maximum of the vertical axis (scale) or
the horizontal axis can be changed by typing _c.  The minimum
and maximum are  free  format  integers  or  floating  point
numbers  (such  as  _0   _1._2,  _0.   ._5  or -_0._3  _2_5).  If the
minimum and maximum wavelengths  are  to  be  changed,  they
should be changed first because when the vertical values are
changed, the routine automatically exits  back  to  the  CRT
plot.    To  change  the  horizontal  values,  type  _w  (for
wavelength) and then the lower and  upper  values.   If  you
type  _w  and  then press return, the minimum and maximum are
automatically scaled from the wavelength file in  use.   The
routine  then  reasks  for input so you may then type in the
vertical minimum and maximum.   For  changing  the  vertical
minimum and maximum,  you may type _a instead of numbers; the
data will be auto scaled.  If _a or return is typed, the  old
minimum and maximum will be used.



9

9                      January 26, 1984







     SPECPR User's Manual                      Page 9.2


_9._4.  _P_r_i_n_t_e_r _L_i_s_t_i_n_g_s _a_n_d _P_l_o_t_s

     The entire header information and data  (in  scientific
notation)  can  be  listed  on the lineprinter by typing _p_d.
The data can be plotted  as  a  lineprinter  plot  with  the
current  scale  by  typing _p and the number of copies (10 or
less).  The data can be plotted in a lineprinter  plot  only
in  channel space.  The (y-axis) resolution is 1 part in 100
(100 print positions for the plot).   For  each  data  point
printed, the wavelength, channel number, and data number are
given.  Four pages are required for 256  data  points  while
only 2 pages for 120 points.  When the number of channels is
less than 120, the plot is scaled to fit from 1 to 2 pages.



_9._5.  _I_n_f_o_r_m_a_t_i_o_n _C_h_a_n_g_e

     The title only can be changed by typing _t.  The program
will  then  draw  a box where the title is to fit.  If other
information is to be changed (including the title), type  _i.
The  header  information is contained on 3 pages of the CRT.
Pressing return goes to the next page, and typing _r  returns
to  the  first  page.  To change information, type the indi-
cated letter.  You will then  be  instructed  to  input  the
appropriate data.

     In the case of the manual history, it is displayed in 4
lines  and can be changed one line at a time (type _m and the
line number or simply the line number 1, 2, 3, or 4) or  all
four lines (type _m and no line number).

     In the case of  the  Band  Normalization  factor,  scan
time,  or total integrating time, the number can be integer,
floating point, or scientific  notation.   In  the  case  of
scientific notation, the number is typed in as an integer or
real number, then the letter e, then the  exponent  (to  the
power of 10, an integer).

     There are 3 ways to exit the  information  change  rou-
tine.   When  at  page 3, pressing return with no input will
exit to the CRT plot.  Typing _g will also exit  to  the  CRT
(graph)  plot.   Typing _x will return to the calling routine
(Math Operations or File Display and Transfer).  The  option
e  exit  will  not terminate other processing; it only skips
the CRT plot.  Thus, if a file write is pending (as  in  the
Math  Operation,  section  8, or file transfer with display,
section 10), it will be completed in the type  e  exit.   If
you  wish  to exit and terminate pending file writes, type _x
for a hard exit. Note that, from file display routine  (sec-
tion  10),  no  information  is  changed  on the stored data
unless there is a transfer involved (see section 10).
9

9                      January 26, 1984







     SPECPR User's Manual                      Page 9.3


_9._6.  _G_l_i_t_c_h _R_e_m_o_v_a_l

     Typing _g from the CRT plot will call the glitch removal
routine.   The routine tries to identify glitches by looking
for data points which are greater  than  6  percent  of  the
total  data range and, by the use of a simple pattern recog-
nition routine, checks 4 conditions.  The data points  which
are thought to be glitches are identified by a small diamond
shaped symbol.  The user can then select which  data  points
are  "actual"  glitches  and  can  then  correct  them.  The
glitches are assumed to be wrong by a some power of  2  from
the true data.  This follows from a binary counter where one
of the bits has been set wrong.  This  routine  was  written
specifically  for  the  "Wedge"  CVF  spectrometer which has
these type of counters.  Thus, the  glitch  removal  routine
should be used only on raw or object-sky data with data sys-
tems containing such counters.  Once the data has been  mul-
tiplied or divided, the glitch removal is not strictly valid
and is "fudging" the data.

     The user indicates which channels are to be deglitched.
By  typing _a, the user tells the program that All the points
identified plus the  channels  typed  in  after  the  a  are
glitches  to  be corrected.  By typing _o, the user tells the
program Only those channels typed in after the letter o  are
to  be corrected as glitches.  By typing _b, all the channels
identified by the program are glitches except those channels
which  are  typed  in  after the b.  At least one space must
occur between channel numbers.  The glitch routine  searches
for  15  glitches  at  one time so it may take more than one
pass to remove a lot of glitches.  Sometimes more  than  one
bit is wrong, and it will take more than one pass.  Glitches
which occur next to each other are not recoverable  by  this
routine  since the routine tries to correct the point to the
surrounding data using the nearest power of 2.  If  after  2
passes  on  the  same  point the data is not restored, it is
probably lost.

     A note on removing glitches:  you are fudging the data.
If  you  are  not very careful and use the utmost restraint,
you may create some absorption or emission features you  had
not counted on!


_9._7.  _L_i_n_e _T_y_p_e

     Typing _l followed by either 0, 1, 2, or 3  will  change
the  line type used in the CRT plot. The following gives the
characteristics of the four types.


            0               error bars included, points not connected
            1               error bars included, points connected


                      January 26, 1984







     SPECPR User's Manual                      Page 9.4


            2               error bars excluded, points connected
            3               error bars excluded, points not connected




_9._8.  _P_o_i_n_t _D_e_l_e_t_i_o_n_s   By typing _r followed by  a  list  of
channel  numbers  followed by _c you can Remove the points in
the list. The deletion is done by setting the value  of  the
data in the specified channels to -1.23x108349 which indicates
deleted points. The list  of  channel  numbers  consists  of
numbers   separated  by  spaces  and  by  pairs  of  numbers
separated by _t to indicate the deletion of a range of  chan-
nels.


_9._9.  _E_x_i_t_i_n_g _C_R_T _P_l_o_t

     There are 3 commands for CRT plot exiting.   The  first
applies  to  the  Math Operations routine only.  If the Band
Normalization option was not turned on, the user may type _b.
This  will  turn  on the Band Normalization option, exit the
CRT plot, and go to the Band Normalization routine  (section
8.9).  When the Band Normalization routine is left, the pro-
gram will return to the CRT plot.  If _b is  typed  while  in
the  file  display, transfer, and overlay, the CRT plot will
soft exit (as a type e exit below).  By typing  _e  the  user
soft  exits  from the CRT plot in the normal fashion and the
program executes the next command.  By typing  _x,  the  user
hard  exits  from  the  CRT  plot,  all pending commands are
ignored, and the program  returns  to  the  calling  routine
(which  is either Math Operations or File Display, Transfer,
and Overlay).

     After a soft exit (e) from  the  CRT  plot  under  Math
Operations,  the  data  is written to the requested location
(see section 8.7).  If  errors  are  included,  the  program
writes  on  the CRT where they will be written and gives the
user the option of not to write the errors  (by  typing  _x).
Otherwise, press return or type _c to continue.

     After a soft exit from the CRT plot under File  Display
and  Transfer,  the  data  is  written  only  if  there is a
transfer (see section 10); otherwise, the  next  command  is
executed.



_9._1_0.  _M_u_l_t_i_p_l_e _C_o_m_m_a_n_d_s _i_n _C_R_T _P_l_o_t

     One entire line (80 characters) can be input to the CRT
plot  routine  at  one time for execution.   For example, to
change to wavelength space, record 2, change  scale,  change


                      January 26, 1984







     SPECPR User's Manual                      Page 9.5


information,  print  header  information and data twice, and
make 2 lineprinter plots, you would type:

        a2cipdpdp2  or  a2 ci pd pd pp.

Spaces can be inserted wherever desired but are  not  neces-
sary.   This  greatly  speeds  up  processing since it takes
about 12 seconds to plot 120 channels on the CRT  and  would
take a long time to replot the CRT after each command.











































9

9                      January 26, 1984



