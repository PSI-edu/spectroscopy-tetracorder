



     SPECPR User's Manual                     Page 8.17


_8._6._1_2.  _F_1_2:  _C_u_b_i_c _S_p_l_i_n_e _I_n_t_e_r_p_o_l_a_t_i_o_n

     This routine interpolates to find data values in a  new
wavelength  file  given  a  different  data-wavelength  set.
Errors, if specified, are also  interpolated.   The  program
requires  that the file and e to include errors be specified
when called from Math Operations.  User  will  be  asked  to
input two wavelength files, the first for the data file, the
second for interpolation.  Since glitches in data could lead
to  erroneous  results,  user  should delete those channels.
Either individual channels or a range can be specified (type
in the minimum channel number, the letter t, and the maximum
channel number in the range).  Deletions may  be  placed  on
one  or  more  lines,  with a c to terminate delete mode and
continue  processing.   Spaces  are  needed   only   between
numbers.  For example

1 t 3 4 5 6 7c is the same as 1 2 3 4 as is 1 t 7
                              5 t 7c        c


     The cubic spline routine used requires each  wavelength
file  to be sorted in increasing order, and the interpolated
wavelength to be within the bounds of the  other  wavelength
file.  F12 handles this, minimizing errors that could occur.
If user gets one of the following  "F12  errors,"  note  the
number and other pertinent information (e.g. files using) in
mail to specpr on UNIX.


33   found value(s) in  interpolated  wavelength  file  less
     than other file


34   found value(s) in interpolated wavelength file  greater
     than other file


129  dimension of spline coefficient matrix  less  than  one
     less the number of channels


130  number of channels  less  than  2  (user  should  check
     wavelength files in use)


131  wavelength files are not ordered.


     Note:  None of these errors should ever happen  so,  if
they do, something is wrong.

9

9                      January 26, 1984







     SPECPR User's Manual                     Page 8.18


     The spline function is very  useful  for  interpolating
data  to  new wavelength values and also for generating con-
tinua.   When interpolating to a  new  wavelength  set,  the
resolution of the spectra should be equal (use the smoothing
function of F17 to degrade a  high  resolution  spectrum  to
that  of the low resolution spectrum).  Spurious data points
should be deleted.  For generating a continuum  to  a  spec-
trum,  specify  the  same  wavelength file for the input and
output spectra, then delete data points which are not on the
"continuum."   This  can be tricky depending on the data and
what you are trying to do so talk to people  who  have  used
the routine first (e.g. Roger Clark or Bob Singer).

     _H_o_w _t_h_e _C_u_b_i_c _S_p_l_i_n_e _I_n_t_e_r_p_o_l_a_t_i_n_g _F_u_n_c_t_i_o_n _W_o_r_k_s

     The cubic spline interpolating function may be  visual-
ized  as  follows.   Bend  a  flexible strip (like a plastic
ruler) so that it passes through each of the data points  in
the spectrum to be interpolated8 |99|8f(x918),f(x928),...f(x9n8)8|99|8.  The
physics of the bent strip shows that  the  equation  of  the
strip  can  be  represented as a series of cubic polynomials
with appropriate boundary  conditions.   A  different  cubic
polynomial  is  calculated for each interval in the spectrum
[i.e. S918(x) on the interval (x918,x928), S928(x) on (x928,x938) . .  .
S9n-18(x)  on (x9n-18,x9n8) where S9n8 represents a cubic polynomial
A9n8x839+B9n8x829+C9n8x+D9n8=S9n8(x)].

     The boundary conditions are that  the  spline  must  go
through      each      function     value     [S918(x918)=f(x918),
S918(x928)=f(x928)=S928(x928)  =  etc.],  and  the  first  and  second
derivatives   of   the   cubic  polynomials  are  continuous
[S9m-17'9  (x9m8)=S9m7'9(x9m8), S9m-17"9  (x9m8)=S9m7"9(x9m8) for m on the interval 2 to
(n-1)  (inclusive)].   Finally,  the  curvature is forced to
zero at the endpoints [S917"9(x918)=0, S9n-17"9  (x9n8)=0].

     The spline used  here  is  _N_O_T  a  smoothing  function.
Therefore,  data  with  large noise spikes present should be
pre-smoothed before fitting with  the  interpolating  spline
(or   the  data  points  with  anomalous  values  should  be
deleted).  For a more complete discussion  see  B.  Carnahan
and  J.  O. Wilkes, _D_i_g_i_t_a_l _C_o_m_p_u_t_i_n_g _a_n_d _N_u_m_e_r_i_c_a_l _M_e_t_h_o_d_s,
John Wiley & Sons, New York, New York. p 307. (1973).










9

9                      January 26, 1984







     SPECPR User's Manual                     Page 8.19


_8._6._1_3.  _F_1_3:  _M_e_r_g_e _T_w_o _S_p_e_c_t_r_a _t_o _O_n_e

     This function will combine  two  input  data  sets  (of
presumably  different wavelengths) into a single output data
set.  It will not automatically merge  the  two  input  data
sets  according  to their respective wavelengths; rather, it
will combine the two sets according to  a  channel  sequence
specified  by  the user.  (Errors will be included if speci-
fied.)

The following files must exist prior to  calling  the  func-
tion:


A)   a first input data set (+ errors)


B)   a second input data set (+ errors)


C)   a wavelength set with enough  channels  to  accommodate
     the   desired   output  data  set  (the  value  of  the
     wavelengths is  inconsequential;  only  the  number  of
     channels matters--256 maximum)


     The first input data set must be specified when f13  is
called.  The  wavelength  set must also be specified at call
time unless the last operation or  display  in  the  program
used a wavelength set with the same number of channels.

Example of use:


Given two input data sets of different wavelengths:

        A)  w127  (25 channels) + errors
        B)  v321 (120 channels) + errors


     1)   Decide how the two data sets should  be  combined.
          For  this  case, combine the two data sets so that
          w127 channels 1-24 are followed by  v321  channels
          4-57  and channels 63-115; all other channels will
          be omitted.


     2)   Create a wavelength set with  enough  channels  to
          accommodate  the  output data set.  For this case,
          create a6 with 131 channels.


9

9                      January 26, 1984







     SPECPR User's Manual                     Page 8.20


          If necessary, create a specific wavelength set  to
     match  the desired output data set.  For this case, the
     wavelengths corresponding to w127 have been written  to
     v444;  the  wavelengths  for  v321 have been written to
     v445.  The specific wavelength set to match the desired
     output data set may be created (via f13) as follows:


          a)   In Math Operations, type
                       v444f13a6


          b)   When requested to enter File B, type
                       v445


          c)   When requested to enter the channel sequence,
               type
                       a 1 24 b 4 57 b 63 115


          d)   When the operation is  completed,  write  the
               result.


          e)   Go to Program Operations and change the  ini-
               tialization parameters.


          f)   Change the wavelength calibration.


          g)   Read  the  file   just   written   into   the
               wavelength   file;   for  this  example,  use
               wavelength set 6, 131 channels.


          h)   Return to Math Operations.


     3)   Combine the two data sets, using f13, as follows:


          a)   In Math Operations, type
                       w127 f13 e

               (the last wavelength set  used  was  a6,  131
               channels,  so  a  wavelength  set need not be
               specified this time)


          b)   Ignore any messages until requested to  enter
               the second input file; then type


                      January 26, 1984







     SPECPR User's Manual                     Page 8.21


                       v321


          c)   When requested, enter  the  channel  sequence
               desired  for  the output file as follows: a 1
               24 b 4 57 b  63  115  c  (Note:   The  string
               specified here can be no longer than 74 char-
               acters;  spaces  are  required  only  between
               numbers.)


          d)   When the operation is  completed,  write  the
               results (and errors) as you see fit.


     4)   If desired, the output data sets (and  the  output
          wavelengths)  may  now  be  sorted into increasing
          wavelength order using f10.


































9

9                      January 26, 1984







     SPECPR User's Manual                     Page 8.22


_8._6._1_4.  _F_1_4:  _E_d_i_t_s _S_p_e_c_t_r_a_l _D_a_t_a _a_n_d _E_r_r_o_r _V_a_l_u_e

(Note:  This function sets the HP terminal into alpha scrol-
ling  mode  to  avoid  the necessity paging; as the function
exits, the terminal is returned to SPECPR graphics mode.)


     This function allows editing of a data set and, if e is
specified at call time, its associated error set as well.


     To call f14, specify the file and record  number,  f14,
and, if errors are desired, e.

Example:

        v317f14e                    or               v317f14


     f14 accepts the following formatted commands to perform
particular tasks:


channel data/e error                   change
8___________________________________________________________
d[c] channel [channel1 t channel2] c   delete
i channel                              insert after
l channel1 channel2                    list (crt)
pd                                     print data (lp)
c                                      continue
e or x                                 soft exit, hard
                                         exit, respectively



     To CHANGE a data value (and  its  error,  or  just  its
error),  specify  the  channel  number  of  the  value to be
changed and the new data value (and the  new  error  value);
should  you wish to change only the error, specify the chan-
nel number, e, and the new error  value.  (If  you  wish  to
change only the data value but you specified e at call time,
you must specify three parameters; the channel  number,  the
new  data  value,  and  the  old error value.)  Incorrect or
uninterpretable format will cause an  error  message  to  be
printed  on the terminal screen.  (Parameters may be entered
either as integers or as real numbers but not in exponential
format.)

Examples:


Call setup   Command      Task enacted



9                      January 26, 1984







     SPECPR User's Manual                     Page 8.23



v317f14      11 13.567    change channel 11 data value to 13.567
v317f14e     11 13.4 .2   change ch. 11 data to 13.4, error to .2
v317f14e     11 e .2      change ch. 11 error value to .2
v317f14e     11 13.567
7                          ILLEGAL ENTRY (errors were specified at
                          call  time,  but no error value was en-
                          tered)



     To DELETE data values, specify d (and the option c  for
compress);  the routine will then accept only channel number
or channel ranges until the parameter c [1] is  encountered,
at  which time the delete task is performed.  Without option
c, the data values of all specified channels will be set  to
-1.23e+34  (the  standard  value  for SPECPR deleted points)
[and the associated errors will be set to 0.0].  However, if
the  option  c is specified, the specified points will first
be deleted, and then the channel space will be compressed to
totally  eliminate the specified channels.  (Note:  Compres-
sion of the channel space does not  take  effect  until  all
specified  points  have first been deleted.) Channel numbers
for the points to be deleted may be specified either indivi-
dually  or  as  ranges; the character t specifies a range of
channels, starting with (and including) the number preceding
t,  and  ending with (and including) the number following t,
the command d, [the option c,] individual channels,  channel
ranges,  and  the  parameter  c  may all be strung together;
spaces are required as delimiters only between two numbers.

Examples:


               Command            Task enacted
          d6 7 12t13 18t21c
7                              Data   values    for
                              channels  6,  7, 12,
                              13, 18, 19,  20,  21
                              are      set      to
                              -1.23e34:[errors for
                              same channels are et
                              to 0.0]
          dc1t3 7c
7                              channels 1, 2, 3,  7
                              are deleted totally;
                              old  ch.  4  becomes
                              new ch. 1; old ch. 5
                              becomes new 2; old 8
                              becomes new 4.

____________________
9   [1] Do not confuse the parameter c with the option c.




9                      January 26, 1984







     SPECPR User's Manual                     Page 8.24


          dc
7                              routine     requests
                              "enter   more  dele-
                              tions or type  c  to
                              continue"



     To INSERT data [and error]  values  after  a  specified
channel,  specify  i  and the channel number after which you
wish to insert values.  (If no  channel  is  specified,  the
routine  defaults to channel 0, in which case the insertions
occur at the start of a spectrum.)  The  routine  will  then
accept  only  data  [and error] values, one data [and error]
value per line, until either a blank line  or  two  carriage
returns  in  a  row are encountered; either integers or real
numbers (but not exponential format) are acceptable for data
and errors.

Example:


Call setup   Command sequence   Task enacted
v317f14e     i
7                                routine prepares to accept  data  &
                                errors, starting at channel 1
             12 1.3
7                                new channel 1: data = 12;  error  =
                                1.3;  original  ch.  1  becomes new
                                channel 2
             15.6 .9
7                                new channel 2: data = 15.6; error =
                                nel 3
             <return>           insertion routine stops
             i2
7                                routine prepares to accept  data  &
                                errors starting after current chan-
                                nel 2
             16 2               new channel 3: data = 16; error = 2
             17.9
7                                ILLEGAL ENTRY--no error was  speci-
                                fied; routine requests last line to
                                be retyped
             17.9 1.6
7                                new channel 4: data = 17.9; error =
                                1.6; original ch.1 is now ch.5
             <return>           insertion routine stops



     To LIST data and error values on the  terminal  screen,
specify  l  and  either a channel number or else two channel
limits.  Data [and error]  values will be listed in exponen-
tial   format  along  with  channel  numbers.   (Spaces  are
required as delimiters between  limits.)   All  data  values
within  the  channel  limits  are listed first; error values
follow.

Example:
9

9                      January 26, 1984







     SPECPR User's Manual                     Page 8.25


l 1 60   channel number & data value for channels 1 through
                60 are listed on the terminal [followed by channel
                number & error value for channels 1 through 60]
l 5      5 and data value for ch. 5 [and 5 and error value for
                ch. 5] are listed on the terminal



     To PRINT the  data  [and  error]  values  on  the  line
printer,  specify pd.  The output format is the same as when
pd is specified in the CRT plot routine.


     To CONTINUE normally and write the edited file, specify
the  command  c;  the routine will exit to the write-to-file
routine.  Note:  You cannot return to f14  and  issue  addi-
tional commands once CONTINUE is enacted.)


     A SOFT or HARD EXIT will occur if at any time the  com-
mands  e  or  x, respectively, are encountered; control will
return to MATH OPERATIONS.  (Exception:  In CHANGE, e is not
interpreted  as  an  EXIT  command  if  it follows a channel
specification.)


     IMPORTANT NOTE:  This routine does NOT record a history
of  the  edit commands performed; should you desire a record
of your editing commands, you must create a  manual  history
yourself.  However edited changes can be easily seen by com-
paring with the original data set which is specified in  the
history.

Minor notes:


     All channel numbers should be positive integers;  if  a
real number is entered by mistake, the routine will truncate
it to integer form.  All channel range limits must list  the
lower bound first and the upper bound last.


     If at any time during this routine  you  find  yourself
lost  or confused, try pressing <return> once or twice until
the EDIT COMMAND FORMAT message appears.  Alternatively, you
may type e or x to exit f14.


     You may insert spaces  as  delimiters  to  improve  the
clarity  of commands; use of such spaces will not affect the
operation of f14.

9

9                      January 26, 1984



