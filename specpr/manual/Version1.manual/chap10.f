



     SPECPR User's Manual                     Page 10.1


_1_0.  _F_I_L_E _D_I_S_P_L_A_Y, _T_R_A_N_S_F_E_R, _A_N_D _O_V_E_R_L_A_Y

_1_0._1.  _F_i_l_e _D_i_s_p_l_a_y

     A spectrum can be displayed on the CRT  using  the  CRT
plot routines (section 9) by typing in the file ID (v, w, d,
u, or y), the record number, and any options.  Options are e
for  errors,  a  for auto scale, b for auto scale with line-
printer plot, a followed by the  wavelength  record  number.
If many records in sequence are to be plotted, then directly
after the record number type + and the number of records  to
be plotted.  Thus, to plot spectra from device v, records 10
to 22, type v10+12.  The +12 means the next 12 records after
the  requested  record  should also be processed.  Note that
the + specification will yield strange results with option e
(include  errors)  since  the  record increment is always 1.
Thus, after plotting data with errors  (e.g.  record  v10  =
data  and record v11 = errors), the record increment will be
one, and the errors will be plotted as data  with  the  next
record  plotted  as  the errors to the errors!  To plot many
spectra in sequence which all have error records, they  must
be  typed  in  sequentially such as v10e v12e v14e v16e v18e
v20e v22e.  Note no spaces are required,  but  they  can  be
inserted  if desired.  No commas are necessary (this is dif-
ferent from Math Operations).


_1_0._1._1.  _F_i_l_e _T_r_a_n_s_f_e_r

     Files can be transferred by  specifying  the  file  ID,
record number, the transfer specification _t, and the file ID
and record number of where the  transfer  is  to  go.   Many
records  may  be  transferred sequentially by using the plus
specification.  For example, v10td23 transfers vfile  10  to
dfile  23.   Also,  v10 + 99td23 transfers vfile 10 plus the
next 99 records to dfile 23  plus  the  99  records  respec-
tively.  Thus, vfile 109 will be the last record transferred
and will go to dfile 99 + 23 = 122.


_1_0._1._2.  _F_i_l_e _T_r_a_n_f_e_r _w_i_t_h _P_l_o_t _o_r _I_n_f_o_r_m_a_t_i_o_n _C_h_a_n_g_e

     Records which are going to be transferred can be  plot-
ted  (and  thus  points  deleted, deglitched, or information
changed, see section 9) before the transfer by the option _c.
The letter c stands for CRT plot routine.  Thus, to transfer
v10 to d23 plus the next 5 records and display  them  before
transferring,  type:  _v_1_0  +  _5 _c_t _d_2_3.  If information only
needs to be changed, the CRT plot can  be  skipped  and  the
information  change routine called by a CIT specification as
in _v_1_0 + _5 _c_i_t _d_2_3.  The c is required since the information
change routine is part of the CRT plot routines.  The _i then
signals the CRT plot routines that  the  information  change


                      January 26, 1984







     SPECPR User's Manual                     Page 10.2


routine  only is requested.  Note, when transferring data to
a protected (positive or zero protection  value)  file,  the
record  number  to transfer to must be 1 plus the protection
value.  A record number must be given  so,  if  you  say  to
transfer  to  v1  but 100 records are protected, the program
will say there is a protection violation.

     Starpack transfers  involve  3  regular  data  records.
Thus,  to  transfer  starpack  1  (s1)  to v10 (s1tv10) will
result in the starpack being put into  v10,  v11,  and  v12.
When the starpack is transferred back, the program automati-
cally collects the 3 records  for  starpack  s1.   Thus,  to
transfer 6 starpacks (1 - 6) to Vfile 29+, type s1 + 5t v29.
The starpack will be put in savefiles  29  through  46.   To
transfer  these starpacks back, type v29 + 5 t s1.  When the
program sees that starpacks are being transferred, it  knows
the +5 means 5 starpacks at 3 regular records per starpack.


_1_0._2.  _O_v_e_r_l_a_y_i_n_g _S_p_e_c_t_r_a

     Overlaying spectra on the CRT is an  excellent  way  to
compare  data  for  reproducability.  For example, before an
extinction analysis, you should overlay your spectra to look
for  inconsistencies  in  the  data.  You can then throw out
obviously bad data.  The overlay is an option used  in  file
display  (see  section  10.1).  To overlay spectra, type the
file ID, record number, and the option letter o.  The o must
be included in every spectrum to be overlaid.  Errors can be
included, and the overlay can be in channel, wavelength,  or
energy space.

     The space type (wavelength, energy, or channel) can  be
set  as  an  option on the first file to be overlaid.  Thus,
the overlay region for different types  of  spectra  may  be
easily  compared.   However,  when  in wavelength space, you
should not be in the auto-scale mode.  An  example  of  this
occurs  when  trying  to overlay the visible Photometer Data
(25 channels from 0.35 to 1.1 Mm) and "Wedge" Circular Vari-
able  Filter  data  (120 channels from 0.62 to 2.5 Mm).  The
minimum and maximum wavelengths are different  for  the  two
data  sets;  thus  the  CRT plot routine will scale the data
differently if the wavelengths are auto  scaled.   To  avoid
this problem, simply set the minimum and maximum wavelengths
to accept the range of your data sets.

     Many spectra in sequence can be plotted and overlaid by
using  the  plus specification as in section 10.1.  The pro-
gram tells you when the last file in a sequence is  overlaid
on  the  CRT.   The  plus  specification  will  give strange
results when including errors as noted in section 10.1.

9

9                      January 26, 1984







     SPECPR User's Manual                     Page 10.3


     An example of overlaying spectra is: _v_1_0_o _v_1_1_e_o  _v_2_3  +
_5_O  _v_5_1  _o.   Note that the letter o is required on the last
file to be overlaid.  If it is not included, the screen will
be  erased, and the spectrum will be displayed as in section
10.1 (see section 10.4).  To use different  wavelength  data
sets, one might type

_v_1_0_1_a_3_o_v_1_0_2_e_o_v_1_4_3_e_o_v_1_4_5_e_o_v_2_0_8+_1_0_a_4_O.     Note    that    the
wavelength set stays the same until it is changed again.


_1_0._3.  _M_u_l_t_i_p_l_e _C_o_m_m_a_n_d_s _i_n _F_i_l_e _D_i_s_p_l_a_y _T_r_a_n_s_f_e_r _a_n_d  _O_v_e_r_-
_l_a_y

     One entire line (80 characters) can be input for execu-
tion.   These  commands  can  be  mixed  freely between file
displays, file transfers, or file overlays.  Spaces  may  be
included  anywhere  on the line, but none are necessary.  An
example of multiple commands  is:  _v_1+_3_c_t_d_1  _w_3_4+_1_0  _v_2_3+_1_4_o
_v_3_8_e_o   _v_1_0_2+_2_4_3_t_v_2_3_6  _v_3_4_6+_1_0_c_i_t_d_4_8_0.   First  there  is  a
display and transfer of  4  spectra,  then  11  spectra  are
displayed,  then  16  spectra  overlaid (the last with error
bars),  next  244  records  are  transferred,  then  11  are
transferred with information change first.

     In any of these routines, type e will exit to the  next
command,  (including  the next one in sequence) and typing x
will exit back to File Display, Transfer and Overlay (except
in  the information change, see section 9) without execution
of the remaining commands.






















9

9                      January 26, 1984



