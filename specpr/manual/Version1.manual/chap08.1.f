



     SPECPR User's Manual                      Page 8.1


_8.  _M_A_T_H _O_P_E_R_A_T_I_O_N_S

_8._1.  _I_n_t_r_o_d_u_c_t_i_o_n

     All mathematical operations are  carried  out  in  Math
Operations  except for extinction analysis.  One entire line
(80 characters) of commands may be typed  in  (separated  by
commas)  for  execution.   After each math operation is com-
pleted, you will be asked where the result is to be  written
(v,  w,  d,  u, or y).  In the case of a positive protection
value, the record number is set to the  protection  plus  1.
Next  you will be asked to type in a title.  See section 8.8
for the  title  routines.   Finally  the  program  will  (if
requested)  go  to  band normalization routine (section 8.9)
and the CRT plot routines (section 9).  The Math  Operations
information  on  the  CRT  can be turned off by typing _i_n or
turned back on by typing _i.

_8._2.  _S_u_b_t_r_a_c_t_i_o_n, _M_u_l_t_i_p_l_i_c_a_t_i_o_n, _a_n_d _D_i_v_i_s_i_o_n

     Generally these operations contain a file ID (v, w,  u,
d,  or y), record number, operation sign (- for subtraction,
* for multiplication, and / for division), then  the  second
file  ID  (v,  w, d, c, s, u, y, or s; s is used in division
only), the second record number, and the  options.   Options
include: _e for include 1 sigma error bars, _n for subtraction
without airmass calculation, _b for turn on  band  normaliza-
tion  (section  8.9),  _b_n for turn off band normalization, _t
for change sidereal time in subtraction with airmass  calcu-
lation,  _r for change RA and declination in subtraction cal-
culation, and _p for production processing.   The  production
processing option stops only to request the title and writes
the result to device v without doing  the  CRT  plot.   This
option  (p) makes processing equivalent to the time spent in
performing batch processing.  The _c in place of  the  second
file  ID is for constant.  The value of the constant is then
placed where the second record number is located.

     Example:

     w10-11, w12-13, v10*c1.3, v23/46eb, v12-d10np bn

Here file w record 11 is subtracted  from  10  (the  default
when  the  second file ID is left out is the first file ID).
Next w13 is subtracted from w12, then v10 is  multiplied  by
the  constant  1.3,  next v23 is divided by v46 with 1 sigma
standard deviation (errors) included and also a band normal-
ization  performed.   Spaces  may  be  included at any point
except within a number, or they may be completely left  out.
The  program processes the commands sequentially.  When per-
forming each operation, the program enters  the  appropriate
routine,  writes  the  titles  of the requested files on the
CRT,  and  pauses  for  a  continue  command   (unless   the


                      January 26, 1984







     SPECPR User's Manual                      Page 8.2


Production  option  has  been set).  If you type _x here, the
program exits directly to math operations.  If you  type  _e,
the  program  exits  that operation and begins processing of
the next operation if one exists.  The production option  is
set  to  no  production at the end of each operation whereas
the band normalization option stays on until turned  off  by
_b_n.   When  the  operation is completed, the program goes to
section 8.1.  Division of two zeros is defined  as  zero  by
this program.

     Math Operations can be used to  change  the  wavelength
file  set  in  the  options  section.  Example: "v23/v45 a2,
v86*v27 a4" where a followed by a number 1 to 99  calls  the
corresponding  wavelength set (with its associated number of
channels).

_A_n_n_o_t_a_t_i_o_n_s _o_n _S_u_b_t_r_a_c_t_i_o_n _R_o_u_t_i_n_e (by P. Owensby--8/20/80)

     Normal subtraction  calculates  airmass  based  on  the
coordinates found in the first 13 characters of the title of
the first input file; if these coordinates are  not  entered
in  the correct format (HHMMSS+DDMMSS no spaces, declination
sign must be explicitly specified as plus or minus, and  any
one  of  the first six digits must be non-zero), the routine
issues the command _T_Y_P_E _I_N _R_I_G_H_T _A_S_C_E_N_S_I_O_N  _A_N_D  _D_E_C_L_I_N_A_T_I_O_N
and  awaits  a  correct  response.  The routine repeats this
message until a correct response is encountered--e and x are
NOT  correct  responses.   The correct response is up to six
numbers separated by spaces, any one of the first  three  of
which must be non-zero, the fourth of which should be signed
only if negative, and  all  the  rest  of  which  should  be
unsigned.

TO GET OUT OF THIS LOOP, enter any single  non-zero  number,
and type e or x at the next possible opportunity.

(This format may be  confusing  because  the  interpretation
routine  from the first 13 characters requires an explicitly
specified plus or minus sign for the  declination,  and  the
request for RA and DEC cannot interpret plus signs.)



_8._3.  _A_d_d_i_t_i_o_n _R_o_u_t_i_n_e

     The addition routine can add from 2 to 128 spectra with
deletion  of zeros or requested points.  To access the addi-
tion routine, type + and any options (e for errors) with  no
other  math operations after that on the input line (because
they will not be processed due  to  a  programming  restric-
tion).   Upon  entering  the  addition  routine, you will be
asked whether you want to average or sum.  Type _a to Average
or  _s  to Sum and, on the same line, _d if you wish to Delete


                      January 26, 1984







     SPECPR User's Manual                      Page 8.3


zeros.  Next you will be given instructions on inputting the
files  to be added or summed.  Type in the file ID [v, w, d,
u, y or c (for constant)] and the record number or constant.
If  you  wish  to delete points from this record, type _d and
then the channel numbers.  You may use more than one line to
input these numbers, but each channel to be deleted from the
sum or average must be listed.  When you are through  input-
ting  numbers  to  be deleted, type _c to continue.  When you
are through  inputting  record  numbers,  type  _b  to  Begin
analysis.   Note only 1 constant can be entered per addition
run.  If you type _e or _x, the  program  will  exit  directly
back  to  math  operations; otherwise, after the analysis is
complete, the program goes to section 8.7.


_8._4.  _E_r_r_o_r _A_n_a_l_y_s_i_s

     Error analyses are included in addition, multiplication
and  division, and subtraction and many of the special func-
tions (see individual functions  -  section  8.4.6)  In  all
cases  of  error analysis, the data numbers should be within
the range of 1.0x108-159 to 1.0x108+159 to avoid  overflow.   If
overflow  occurs,  the error is set to zero.  If the data is
zero in the multiplication or division routines, when  using
the propagation of errors, the error is set to zero for that
channel.  The errors represent 1 sigma standard deviation of
the mean and are first computed in the addition routine as a
standard deviation of the mean computation.   The  equations
used in propagating errors are as follows:
        Given a+_Y9a8, b+_Y9b8, c+_Y9c8 and the result x+_Y9x8 the equa-
tions used are:

_d_i_v_i_s_i_o_n:




                                                 (eqn 8.4.1)
7777
99999999                     Y9X8=78|99|99|99|778|99|99|8a78Y9a__8|99|99|7782999+7|99|99|8b78Y9b__8|99|99|7782|99|99|99|77271_999X77777X=9b7a_






9
_m_u_l_t_i_p_l_i_c_a_t_i_o_n:



9                                                 (eqn 8.4.2)
777899999999                     Y9X8=78|99|99|99|778|99|99|8a78Y9a__8|99|99|7782999+7|99|99|8b78Y9b__8|99|99|7782|99|99|99|77271_999X77778X=ab





9
_s_u_b_t_r_a_c_t_i_o_n:
9

9                      January 26, 1984







     SPECPR User's Manual                      Page 8.4


9999999                        Y9X8=7|99|99|7Y9a729+Y9b728|99|99|78271_778X=a-b




9
_a_d_d_i_t_i_o_n (when errors already exist):






                                                 (eqn 8.4.3)
777777




9999999999                   Y9x8=7|99|99|7Y9a729+Y9b729+...+Y9c728|99|99|78271_999/n7777(average of n spectra):  X=(a+b+...c)/n778Y9x8=7|99|99|7Y9a729+Y9b729+...+Y9c728|99|99|78271_778(sum):  X=a+b+...+c,













standard deviation of the mean  is  derived  from  (generate
errors for the first time):




9                    Y9x8=7778|99|99|99|99|99|7  (n-1)7i=17R7n99 (x9i8-x7_99)82999__________777|99|99|99|99|99|7778271_999999n7-9271_



9
where n = number of spectra, x9i8 = each spectrum, and

9                          x7_99=9n71_999i=17R7n99 x9i



_8._5.  _A_l_g_e_b_r_a_i_c _a_n_d _T_r_i_g_o_n_o_m_e_t_r_i_c _F_u_n_c_t_i_o_n_s

     A complete set of algebraic, trigonometric,  and  power
functions exists as can be found on a scientific calculator.
No error analysis is included with any of  these  functions.
The  functions  are  accessed by file ID, file number, colon
(:), and the name of the function.  Functions are:


                            Exponential:    exp
                      Natural logarithm:    ln
                       Common logarithm:    log
                  Ten to spectrum power:    10**x



                      January 26, 1984







     SPECPR User's Manual                      Page 8.5


                                Inverse:    1/x
         Spectrum to the constant power:    x**c
         Constant to the spectrum power:    c**x
                      Sine (in radians):    sin
                    Cosine (in radians):    cos
                   Tangent (in radians):    tan
                      Sine (in degrees):    sind
                    Cosine (in degrees):    cosd
                   Tangent (in degrees):    tand
                 Inverse sine (radians):    invsin
               Inverse cosine (radians):    invcos
              Inverse tangent (radians):    invtan
                 Inverse sine (degrees):    invsind
               Inverse cosine (degrees):    invcosd
              Inverse tangent (degrees):    invtand
              Hyperbolic sine (radians):    sinh
            Hyperbolic cosine (radians):    cosh
           Hyperbolic tangent (radians):    tanh
                         Absolute value:    abs
                          Integer value:    int
                        Fractional part:    frac



     In both the spectrum to the constant power and constant
to  the  spectrum  power, the value of the constant is after
the command (x**c or c**x).  _N_o _o_p_t_i_o_n_s _a_r_e _v_a_l_i_d  _w_i_t_h  _a_n_y
_o_f  _t_h_e_s_e  _o_p_e_r_a_t_i_o_n_s  including errors, band normalization,
and production processing.  This is due to programming  lim-
its  (there  has  to be some).  Since these routines are not
used too often, this should make  little  difference.   What
are they used for?  Normal spectral processing does not need
them, but say you wanted to make a plot of a phase  function
for a Lambert sphere:

                                                 (eqn 8.5.1)
7                  F(A)=9J71_|99|8sin A+(J-A)cos A8|99|
9
     You could generate data for A increasing from 0 to  180
using  the  wavelength routines and then this function using
the math functions.  The result can then be plotted  on  the
Gould printer plotter using the plot routines.










9

9                      January 26, 1984



