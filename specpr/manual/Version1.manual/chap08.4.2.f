



     SPECPR User's Manual                     Page 8.26


_8._6._1_6.  _F_1_6:  _L_i_n_e _S_e_g_m_e_n_t _G_e_n_e_r_a_t_o_r

     Given data values at specific wavelengths  or  channels
typed  in  by  the user, this routine computes line segments
between the points the user has input.   Upon  entering  the
program,  the  user  has  the option of changing the current
wavelength set in use by  typing  the  letter  "a"  and  the
record   number,   or   assigning  the  channel  numbers  as
"wavelengths values" with the  maximum  number  of  channels
given  by the wavelength record.  To do this, type h and the
wavelength record number to assign channel numbers with  the
number of channels given by the wavelength record number.

     After this initialization, the  program  asks  for  the
first  two data values--the coordinates for point #1.  Enter
the data values separated by a space.  Now, the program will
be in a loop asking for the point coordinates Xn, Yn where n
is in a loop going from 1 to 256.  At this stage,  a  number
of control options can be entered:


1)   _e or _x will cause the program to exit.


2)   _r_n - return to step n, where n is the step number (1 to
     256).   This  allows  the  return to any previous input
     step so it may be changed (and then you may  return  to
     the  last  step  you  were  at).   Note that you cannot
     change an X value (wavelength) to a value greater  than
     that  in the next step--to do that, you must delete the
     following steps.


3)   _d_n - delete steps.  Using this command, all data values
     after  and  including  step  n will be deleted; program
     control returns to the first step deleted.


4)   _l_n is a list command which will list up to  25  entries
     previously  entered  with the step number.  Here (n) is
     optional.  If it is used, the program lists  from  that
     step number to the 24th step following.


5)   _b for begin  analysis.   Program  calculates  all  data
     values in the array between each set of ordered pairs.

     No input spectrum is required since  you  are  creating
data,  not  operating on data.  If an input file was called,
an error message is printed, and program will hard exit.

     All operations are checked for validity.   If  a  given
operation  produces  an  error,  a  message  will be printed


                      January 26, 1984







     SPECPR User's Manual                     Page 8.27


stating what caused the error and whether or not the program
can  continue.   If it can, the program reasks the data.  If
not, the program will exit, hard or soft, depending  on  the
severity of the condition.  Note that all X data values must
be in increasing order, or error messages will result.

Sample outputs are included (see next pages).

Graph #1 was done in wavelength space with Planetary Geosci-
ences cooled IR CVF default wavelength values using the fol-
lowing input values:

 .5   .2
 .8   .9
1.6   .4
2.7   .6


     Graph #2 was done  in  channel  space  but  plotted  in
wavelength  space  using the Planetary Geosciences cooled IR
CVF default wavelength values.

  1   .2
 17   .9
 74   .4
120   .6


























9

9                      January 26, 1984



