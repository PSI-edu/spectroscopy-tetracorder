



     SPECPR User's Manual                     Page 8.25


_8._6._1_5.  _F_1_5:  _F_o_r_m_a_t_s _G_a_u_s_s_i_a_n _P_a_r_a_m_e_t_e_r _F_i_l_e

     This program takes GFIT data from the SPECPR files  and
formats  the data in a neat, legible manner.  If no file was
input, an error message is written,  and  the  program  soft
exits.   The program also checks to make sure that the input
file is the correct one.  If not, the program soft exits.

     First, the program writes out the  gaussian  terms  (if
there  are  any),  followed by the continuum terms (if there
are any); these are followed by the  integrated  intensities
which  are  equal to the height times the width of the gaus-
sian and percent error of the gaussians and the  mean  value
and sigma.

     The program then checks the manual history  to  see  if
the fit was in inverse wavelength space or natural logarithm
space.  If the fit was in inverse wavelength space, then the
data  is  converted to wavelength space for the center posi-
tions and widths.  If the fit was done in  log  space,  then
the  items  printed are (1) the relative band depth which is
equal to 1 - EXP (Height), (2) Band depth - Error = 1 -  EXP
(Height  -  Error)  and  (3)  Band  depth  + Error = 1 - EXP
(Height + Error).  In  all  divisions,  the  denominator  is
checked  for zero.  If it is zero, it is reset to 1.0x108-369,
and an error message is printed on user's terminal.


























9

9                      January 26, 1984



