



     SPECPR User's Manual                     Page 8.36


_8._6._2_3.  _F_2_3: _M_a_t_h_m_a_t_i_c_a_l _P_a_r_s_e_r

     This routine allows the user to type in arbitrary math-
matical  expressions  involving  spectral  data,  wavelength
records, and constants. The expressions allowed are any com-
bination of addition, subtraction, multiplication, division,
and exponentiation (exponentiation is represented by ^).

     To use f23 type f22  (or  f22e  if  errors  are  to  be
included  in the computation) from math operations. The pro-
gram will list which wavelength record  and  the  number  of
channels  it  will be using. To change the wavelength record
at this point type _a  followed  by  the  desired  wavelength
record  number.  If the default wavelength record is accept-
able just type a carriage return. If you wish to  abort  the
routine  type  _e  or  _x.   After  choosing  the  appropriate
wavelength record the routine will  print  out  the  current
file  protections and allow you to type in the expression to
be evaluated. For example, If  you  wanted  to  add  file  v
record  21  to  a gaussian with a height -1.5, half width of
2.3 microns (at 1/e point), centered  at  1.32  microns  you
would type:
_v_2_1+-_1._5*_2._7_1_8_2_8_e_0^(-((_a_3-_1._3_2)/_2._3)^_2)
(assuming you are using wavelength record 3). When you  type
a  carriage  return at the end of the line. The routine will
return to math operations and ask where the data  is  to  be
written (see section 8.7).

























9

9                      January 26, 1984



