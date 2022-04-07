



     SPECPR User's Manual                     Page 8.28


_8._6._1_7.  _F_1_7:  _H_i_g_h _T_o _L_o_w _R_e_s_o_l_u_t_i_o_n _C_o_n_v_o_l_u_t_i_o_n

     Subroutine f17 inputs up to 256 spectral bandpass files
and  convolves each bandpass file around the high resolution
input spectrum that is entered from SPECPR  before  entering
f17.   The routine outputs two data records:  (1) the resul-
tant wavelength values (center values) and (2) the convolved
spectrum.  The output of the center values is optional.

     To run the function, go to the SPECPR math routine  and
enter  the file ID and number for the input spectrum and f17
(example v23f17).  The routine starts with a query  for  the
use of the default wavelength assignment, which is displayed
at the top of the screen.  To change the default assignment,
the  user  enters  "a" followed by the wavelength new record
number.  Next, the spectral bandpass information is entered.
Start with the desired file ID, beginning record number, and
ending record number.  The records are then read, the output
spectrum  is  calculated,  and  then  the user is queried on
whether or not to output the center value  wavelengths.   If
you wish to write the center values, type in the file id and
record number of the file  you  wish  to  write  the  center
values,  or type return to ignore writing the center values.
Then enter the title for the center values, the file ID  and
number  for output when asked by the program.  Note that f17
cannot include errors in its calculations.

The equation used to calculate the resultant is


                         R9j8=99x9078^78x9l99SB9j8dx


Where R9j8 is a data point in the resultant  spectrum,  x908  is
the  first value of the input spectrum, x9l8 is the last value
of the input spectrum, S is the input spectrum,  and  B9j8  is
the spectral bandpasses.

     The center values are calculated by the following:


                        C9j8=99x9078^78x9l99xSB9j8dx



     Where C9j8 is the center value for that particular  spec-
tral  bandpass, x9l8, S, B9j8 and x908 are the same as first equa-
tion, x is the wavelength.

     The integrals are  approximated  by  summing  the  data
points as follows for each convolved channel (j).

9

9                      January 26, 1984







     SPECPR User's Manual                     Page 8.29


9                      R9j8=99i=17R7n99 S9i8B9j,i8Dx9i

Where n is the number of channels and Dx9i8  is  the  spectral
bandpass of channel i



9                  Dx9i8 =777 |99|99|99|99|99|99|7x9n-18-x9n,778x9i+18-x9i-18,77x928-x918,9999999      i=n781<i<n78i=1





and similarly, the center values (wavelength) of  each  con-
volved channel is computed from the equation



                    c9j8 =9       R9j78i=17R7n99 x9i8S9i8B9j,i8Dx9i9______________



     During the calculations, divide operations are  checked
for  a  zero  denominator.  If this were to happen, an error
message is printed for the  user,  and  the  denominator  is
reset to 1.0x108-369 instead of crashing the program.

     If no file has been input to the program at the  start,
an error message will be printed, and the routine exits.

     Note that for the spectral  bandpass  information,  the
difference  between  the  beginning and ending record number
cannot be greater than 256.  Also, note that, if the  number
of  bandpass  files times five is greater than the number of
channels, a warning message will be printed explaining that,
with  data arranged in such a manner, the resulting accuracy
of the data is questionable.  This is due to using the  sum-
mations  (equations  3 and 5) and the equation 4 to approxi-
mate the integrals.  The message is only a warning--you must
determine if the bandpasses are complex enough that accuracy
may actually be in error due to a small sample.














9                      January 26, 1984



