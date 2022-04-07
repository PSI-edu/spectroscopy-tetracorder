



     SPECPR User's Manual                      Page 8.5


_8._6.  _S_p_e_c_i_a_l _F_u_n_c_t_i_o_n_s

     Special function subroutines  are  additional  routines
for operating on any kind of data.  Subroutine calls already
exist in the program for many  as  yet  unwritten  routines.
These  routines are designed for easy linking to the present
program.  New routines can be user written with less  effort
than  a  completely  separate  program since, in most cases,
SPECPR handles all the data management.   The  routines  are
called  by the letter _f and the function number.  An example
would be to call function 2 to operate on file v record  10:
_v_1_0_f_2  Note  there  is no colon as in the algebraic and trig
routines.  Some special functions do not require a  file  to
be  input  before the function command.  An example is func-
tion 1 which is a list of all available functions  (type  _f_1
only).   Functions  which  require  a  file input before the
function call will have an f in parenthesis in the  list  of
functions (f1).  Similarly, an e will also be in parenthesis
if an error analysis can be included.  The  available  func-
tions are described below.
































9

9                      January 26, 1984







     SPECPR User's Manual                      Page 8.6


_8._6._1.  _F_1:  _L_i_s_t _o_f _S_p_e_c_i_a_l _F_u_n_c_t_i_o_n_s

     List of all available special functions.

















































9

9                      January 26, 1984







     SPECPR User's Manual                      Page 8.7


_8._6._2.  _F_2:  _S_h_i_f_t

     f2 shifts data left or right an integer  or  fractional
number of channels.  A file input is required and errors can
be included.  Fractional shifts are done by linear  interpo-
lation.   When  the  routine  has  been entered, type in the
number of channels to shift or type e or x to exit.  A posi-
tive  number (integer or real) indicates a shift right and a
negative number left.

     If X is the number of channels to shift, the value of X
is  split  into the integer part (n) and the fractional part
(XN).  First, the integer part is shifted

                                                 eqn 8.6.2.1
7                          D9i+n8=D9i
9
where 0<_i+n<_257 and D9i8 is each data point in  the  spectrum.
Next, the fractional part is interpolated


                                                 eqn 8.6.2.2
7                XN'>0.0;  D9i8=D9i+18-D9i8*XN8'9+D9i
9
                                                 eqn 8.6.2.3
7                XN'<0.0;  D9i8=D9i8-D9i-18*XN8'9+D9i
9
where XN' = -1.0 * XN.  Data channels  not  in  the  shifted
region  are set to zero.  The errors to the data are shifted
in a similar manner.  Before June  24,  1979,  there  was  a
slight error in the program (XN' = XN) making the fractional
shift appear to go in a different direction than the integer
part  shift.   The  history has been modified to signal this
new change

     Old history (X = + 2.5 operation on WDG536 file 238):

"F2:  WDG536 FILE 238 SHIFT RIGHT 2.5 CHANNELS"

New History:

"F2:  WDG536 FILE 238 SHIFT RIGHT (+) 2.5 CHANNELS".

The subtle change is the sign (+ or -) in parentheses signi-
fying the direction of the shift.







9

9                      January 26, 1984







     SPECPR User's Manual                      Page 8.8


_8._6._3.  _F_3:  _S_e_q_u_e_n_t_i_a_l _P_r_o_c_e_s_s_o_r

     This routine is very useful for repetitive calculations
on  many  records  for multiplication, division, subtraction
and special functions only.  For example, if you  wanted  to
multiply file v records 10 to 23 by the constant 1.3452, you
would type f3 from Math Operations.  The  program  will  ask
for  the  operation.   You  would type *.  (Other operations
would be /, -, f2, f6 or any other function number except  3
and  4).   Next  the program will ask for the first file ID,
beginning record number, ending record number and the record
number  increment.   For this example, you would type _v_1_0 _2_3
_1.  Record increments from 0 to  2000  are  valid.   If  the
record  increment was negative, it is set to zero.  Next you
will be asked to type in the second file ID,  beginning  and
ending  record  numbers  and  increment.  For this case, you
would type _c_1._3_4_5_2.  You will  then  be  asked  to  type  in
options.   If  you wanted errors included, the record incre-
ment should have been 2 since the data and errors take up  2
records.  Note you can get in an infinite loop if the record
increment is set to zero.  However, you  can  exit  back  to
Math  Operations by typing _x at any point.  If you type _e at
any point in the processing, that operation will be  skipped
and  the  program  will  go  on  to the next operation.  The
operations are performed just as if you  had  typed  in  the
commands  one  at a time or all one line separated by commas
in Math Operations.  This function should save a lot of time
and typing.
























9

9                      January 26, 1984







     SPECPR User's Manual                      Page 8.9


_8._6._4.  _F_4:  _S_e_q_u_e_n_t_i_a_l _P_r_o_c_e_s_s_o_r (_N_o _U_s_e_r _A_c_c_e_s_s)

     This function sets up the commands from function 3  for
execution and thus has no user access.
















































9

9                      January 26, 1984







     SPECPR User's Manual                     Page 8.10


_8._6._5.  _F_5: _C_o_n_t_i_n_u_u_m _R_e_m_o_v_a_l
        _a_u_t_h_o_r: _L_u_c_y _M_c_f_a_d_d_e_n

     Type f5.

     The directions follow on the  CRT  and  will  be  given
here.

     "THIS ROUTINE MATCHES A CONTINUUM TO TWO  GIVEN  POINTS
OF A SPECTRUM AND REMOVES THE CONTINUUM BY DIVIDING."

     A continuum is any spectrum which the user generates by
whatever  means  available.   However,  the user must have a
concrete file with a device name and number available before
using  this  routine.  You can generate a continuum by using
the special math functions or  using  Michael  Gaffey's  Fe2
modified spectrum or whatever you choose.

     "ENTER TWO VALUES TO WHICH CONTINUUM IS TO  BE  MATCHED
FOLLOWED  BY  H,  A,  OR  N  REPRESENTING  UNITS OF CHANNEL,
WAVELENGTH, AND ENERGY SPACE,  AND  THE  WAVELENGTH  FILE  #
(DEFAULT = H1).  ENTER E OR X TO EXIT."

     The two points can be entered  in  whatever  units  are
convenient  for you, in free format (spaces between numbers,
no commas).  The wavelength file is read, and the two values
entered  are  correlated with the appropriate channel in the
wavelength file.  If you do not specify a unit's code  or  a
wavelength  file, the program accesses wavelength file 1 and
assumes your numbers entered are channels.  You can bail out
of the routine here if you wish.

     "ENTER SPECTRUM FILE ID AND #, E TO INCLUDE ERRORS FOL-
LOWED BY CONTINUUM FILE ID AND #.  TYPE X TO EXIT."

     The message on the CRT will say "WORKING" when you  hit
the return button after entering the information.  The title
of the files used will be printed on the  CRT  as  they  are
accessed  by  the program.  The error file with the spectrum
is accessed if asked for.  The continuum file is  scaled  to
the  spectrum  file at the two given points by calculating a
scaled slope and an intercept (or vertical offset) and  con-
verting  each data point of the continuum to the scaled con-
tinuum through the relationship Y = mx + b where  Y  is  the
scaled  continuum, m is the slope, x is the original contin-
uum value at a give channel, and b is  the  intercept.   The
spectrum  is  then  divided  by  the  scaled continuum.  The
errors are scaled by a factor of (scaled spectrum / original
spectrum).

     Finally, the title and history are made and control  is
transferred back to the main program to write the file.
9

9                      January 26, 1984







     SPECPR User's Manual                     Page 8.11


_8._6._6.  _F_6:  _B_l_a_c_k _B_o_d_y _C_o_m_p_u_t_a_t_i_o_n

     This function computes the Planck black  body  function
at   a   given  temperature  (in  degrees  Kelvin)  and  any
wavelength set.  Upon entering the routine, you are asked to
type  in  the  temperature and the wavelength record number.
If no wavelength file number is specified,  the  default  is
record 1.  The units are computed in Watts per meter squared
per micron.  The wavelength values must be in microns.  When
using  extreme  temperatures  or  extreme  wavelengths,  you
should check the results for underflow or  overflow  in  the
CRT plot routines (section 9).  No file or errors are needed
since this routine generates a new file instead of an opera-
tion on an old one.  The equation used is




9                                               (eqn 8.6.6.1)
77778



99999                  =99    L8-571.4966x108-59___________78|99|99|99|7e8 LT713586_____999-177|99|99|99|777-1777E9i8(T)=99 L8578Jhc829_____78|99|99|99|7e8LkT7hc___999-177|99|99|99|777-1









where E is the energy for each channel at wavelength  L  (in
microns),  T  is  the temperature (degrees Kelvin), c is the
speed of light, h  is  the  Planck  constant  (=6.6252x108-34
9Joule-seconds),    and   k   is   the   Boltzmann   constant
(=1.3806x108-239 Joule/K).

























9                      January 26, 1984







     SPECPR User's Manual                     Page 8.12


_8._6._7.  _F_7:  _S_m_o_o_t_h_i_n_g _F_u_n_c_t_i_o_n

     This  routine  smoothes  a  spectrum  by  performing  a
weighted  smoothing  of adjacent channels.  First, the chan-
nels are sorted into increasing wavelengths; note  that  the
smoothing  is done in channel space.  The user then requests
how many channels to include  on  each  side  of  each  data
point.   Each  point  is  weighted by the inverse power of 2
with the power increasing with increasing distance from  the
data  point  to  be  smoothed.   If the parameter giving the
number of channels is n, then for n = 1 data point D is com-
puted by

                                               (eqn 8.6.7.1)
7                  D9i8=8|99|8D9i-18/2+D9i8+D9i+18/28|99|8/2.

For n = 2

                                               (eqn 8.6.7.2)
7          D9i8=8|99|8D9i-28/4+D9i-18/2+D9i8+D9i+18/2+D9i+28/48|99|8/2.5.

In general


9                                               (eqn 8.6.7.3)
778

        D9i8=99              (1+9928172__9+...+9928n72__9)7778|99|99|7D9i-n8/28n9+...D9i-18/2+D9i8+D9i+18/2+...+D9i+n8/28n8|99|99|_________________________________________



and




                                               (eqn 8.6.7.4)
7777




9            Y9i8=99        (1+2/2819+...2/28n9)777778|99|99|99|99|7 28n78Y9i-n7299____9+...+9 278Y9i-17299____9+Y9i729+9 278Y9i+17299____9+...+99 28n78Y9i+n7299____78|99|99|99|99|7778271_999999999_________________________________



     The parameter n can vary from 1 to the number of  chan-
nels divided by 2.  When the smoothing is finished, the data
is sorted into the original order.   The  wavelength  record
default is the current wavelength set in use (and is printed
on the  CRT).   If  another  is  desired,  type  _a  and  the
wavelength  file  number on the same line as the parameter n
(which must be first on the line).  Also, if data only in  a
certain  range is to be considered, on the same line, type _l
and the minimum and maximum data  numbers  to  be  included.
Default  limits on L are -1.0x108349 to +1.0x108349.  If certain
channels should be deleted, type _d to delete points.  A mes-
sage  will  then  be  printed  to  type  in the points to be
deleted.  When you are  finished  typing  in  points  to  be
deleted,  type  _c  to  continue.   A  file must be specified
before the f7 command, and  errors  can  be  included.   The


9                      January 26, 1984







     SPECPR User's Manual                     Page 8.13


parameter  n seems to give excellent results when it is 1 or
2.  When n gets large, there  seems  little  change  in  the
smoothed result.

















































9

9                      January 26, 1984







     SPECPR User's Manual                     Page 8.14


_8._6._8.  _F_8:  _C_h_a_n_n_e_l-_R_e_c_o_r_d _T_r_a_n_s_p_o_s_e

     This routine transposes the array of  channel  (column)
versus  spectrum  (rows)  to  spectrum  versus channel.  For
example, if you had 10 spectra of  50  channels  each  where
each  spectrum  was  taken at 1 hour intervals, this routine
transposes the matrix so that there are 50 output  "spectra"
of 10 channels each where each "spectrum" is intensity at 10
different times.  The request for input  spectra  are  input
one  per  line (file letter ID and record number).  When all
are typed in, type _b to begin transpose.  The  program  will
then  ask  for  the  beginning output location (type in file
letter ID and record number).  The output  spectra  will  be
output  sequentially  beginning at that point (but first the
program asks for a common title  for  the  output  spectra).
The number of output spectra is equal to the number of chan-
nels in use.



































9

9                      January 26, 1984







     SPECPR User's Manual                     Page 8.15


_8._6._9.  _F_9:  _B_a_n_d _R_e_m_o_v_a_l (_R_e_f_l_e_c_t_i_o_n _M_e_t_h_o_d)
        _a_u_t_h_o_r: _L_u_c_y _M_c_f_a_d_d_e_n

     Type f9.

     This subroutine takes one half of  an  absorption  band
and  reflects it about the band minimum then divides a given
reflection spectrum by the calculated band.

     "TYPE IN THE FILE ID AND FILE # TO BE PROCESSED,  E  TO
INCLUDE  ERRORS,  FOLLOWED BY L OR R FOR LEFT- OR RIGHT-SIDE
OF BAND TO BE REFLECTED.  TYPE E OR X TO EXIT."

     L or R refers  to  left-  or  right-side  of  the  band
minimum when the spectrum is stored in increasing wavelength
from left to right.  It refers to the side of the band  that
you want to be reflected to the other side.

     "ENTER OUTER LIMITS OF BAND (2 VALUES), ESTIMATED  HALF
HEIGHT  POINT  ON  SIDE  TO BE REFLECTED FOLLOWED BY H, A, N
(CHANNEL, WAVELENGTH, ENERGY), AND WAVELENGTH  FILE  NUMBER.
(DEFAULT = H1), TYPE E OR X TO EXIT."

     The two outer limits are on either  side  of  the  band
minimum.  The first limit tells where to stop the reflection
process, the second tells where to stop looking for the band
minimum.   The program types "WORKING" and is doing the fol-
lowing:


1:   accessing the wavelength file, default is 1.


2:   correlating the given band limits to  channels  in  the
     wavelength  file,  default is that the limits are given
     in terms of channels.


3:   searching for the band minimum by looking for  a  datum
     that is smaller than the three values on either side of
     it, between the limits of the 1/2 height and the second
     outer band limit.


4:   puts values of one side of band into the position  sym-
     metrically opposite to it relative to the band minimum.


5:   divides spectrum by reflected band.


6:   scales the errors by a factor of (the  residual  /  the
     original spectrum).


                      January 26, 1984







     SPECPR User's Manual                     Page 8.16


7:   calculates title and history.


8:   returns to the main program.


     This is a primitive means of removing a band.  If there
are any suggestions or improvements, please let me know.












































9

9                      January 26, 1984







     SPECPR User's Manual                     Page 8.17


_8._6._1_0.  _F_1_0:  _S_o_r_t_i_n_g _R_o_u_t_i_n_e

     The function arranges data into  increasing  wavelength
order.   Entry is by the usual method for special functions.
Errors may be included.  After entry, the files to  be  used
are  printed.   The  user  may change the wavelength file at
this point.  Only the data file is sorted, but by specifying
the wavelength file when calling f10, the wavelengths may be
sorted also.











































9

9                      January 26, 1984







     SPECPR User's Manual                     Page 8.18


_8._6._1_1.  _F_1_1:  _L_u_n_a_r _T_h_e_r_m_a_l _R_e_m_o_v_a_l

     This routine removes the  thermal  component  from  the
reflectance object spectrum using the equation:


                                              (eqn 8.6.11.1)
77
               R908=78|99|99|99|78R907'9+9   R9s8F78R907'9(1-R9s8)P9s__________9-9F78P90__7|99|99|99|778|99|99|71-9F78P90__8|99|99|778-1


9

     Refer to Roger Clark's  paper,  "Planetary  Reflectance
Measurements  in  the Region of Planetary Thermal Emission,"
equation 14 ( _I_c_a_r_u_s, _4_0, _9_4-_1_0_3, _1_9_7_9).

_V_a_r_i_a_b_l_e _d_e_f_i_n_i_t_i_o_n_s:

        R908 = _r_e_f_l_e_c_t_a_n_c_e _o_f _o_b_j_e_c_t  _w_i_t_h  _t_h_e_r_m_a_l  _c_o_m_p_o_n_e_n_t
_r_e_m_o_v_e_d

        R907'9 = _r_e_f_l_e_c_t_a_n_c_e _o_f _o_b_j_e_c_t _w_i_t_h _t_h_e_r_m_a_l _c_o_m_p_o_n_e_n_t

        R9s8 = _r_e_f_l_e_c_t_a_n_c_e _o_f _s_t_a_n_d_a_r_d _w_i_t_h  _n_o  _t_h_e_r_m_a_l  _c_o_m_-
_p_o_n_e_n_t

        P908 = _t_e_m_p_e_r_a_t_u_r_e _f_o_r _P_l_a_n_c_k _f_u_n_c_t_i_o_n _o_f  _o_b_j_e_c_t  (_i_n
_D_e_g_r_e_e_s
                _K_e_l_v_i_n)

        P9s8 = _t_e_m_p_e_r_a_t_u_r_e _f_o_r _P_l_a_n_c_k _f_u_n_c_t_i_o_n _o_f _s_t_a_n_d_a_r_d (_i_n
_D_e_g_r_e_e_s
                _K_e_l_v_i_n)

        _F = (_s_o_l_a_r _f_l_u_x/_J)/(_d_i_s_t_a_n_c_e _f_r_o_m _s_u_n _i_n _A._U.)82

9
     The program requires that the file ID and record number
of R907'9 and the e to include errors, if any, be specified when
called from Math operations.  The program will ask for:

        R9s8 file ID and number

        Albedo at normalization for R908 and R9s8(0.0<A<_1.0)

        Wavelength record number (1<_N<_99)

        P908 and P9s8 in degrees Kelvin (10<T<1085)

9        Solar flux/J file ID and number

        Distance from sun in A.U. (>0.0)


9

9                      January 26, 1984







     SPECPR User's Manual                     Page 8.19


     After entering albedo at normalization, the user has an
option  of  either continuing, exiting, or changing what has
been input so far  (for  the  current  screen  information).
User  also has a similar option after entering distance from
the sun.  In calculating the  thermal  removal,  R908  and  R9s
8values are multiplied by their respective albedo at the nor-
malization point.   Errors  are  propagated  by  multiplying
individual  error  values by the R908 albedo at the normaliza-
tion point.











































9

9                      January 26, 1984



