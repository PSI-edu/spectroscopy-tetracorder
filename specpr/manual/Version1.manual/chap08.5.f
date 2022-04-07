



     SPECPR User's Manual                     Page 8.37


_8._7.  _R_e_t_u_r_n _f_r_o_m _a _M_a_t_h _O_p_e_r_a_t_i_o_n _o_r _F_u_n_c_t_i_o_n

     When a routine such as addition,  subtraction,  special
function,  trig.  function,  etc., has finished, the program
asks the user where he/she wishes the data  to  be  written.
The data can be written to any of the files:  v, w, d, u, or
y if allowed by the protection.  If the  protection  on  the
device is positive or zero, the data can only be written the
protection value plus 1.  Thus, it is not necessary to  type
in  this  value since it is the only value possible.  Simply
type the file letter ID and return; the record  number  will
be  set  automatically.   The  data  is  not written at this
point.  It is written after the CRT plot routines.  The user
may  also  exit  the  current operation from this point.  By
typing _e, the current operation is terminated and  the  pro-
gram begins execution of the next command if there is one or
returns to Math Operations if there  are  no  other  command
requests.   If  the  user types _x, all processing stops, and
the program returns directly to Math Operations.

     If the user requests the data to be  stored,  then  the
program continues to the titles routine (section 8.8).

_8._8.  _T_h_e _T_i_t_l_e_s _R_o_u_t_i_n_e

     The titles routine displays 2 titles on the  CRT  which
may  be  selected for the new data, or one of 25 user stored
titles can be recalled, or the user can type in a new  title
and  store  it.   The  2  titles displayed are the "option p
title" (which is the present title of  the  first  data  set
read  in for 2 file operations or the title of the last data
set in the addition routine) and the "option l title" (which
is the last title requested for the last operation).  In the
subtraction routine, the  program  decodes  an  "object-sky"
title  as  the option p title.  This is based on inputs from
the "Wedge" CVF spectrometer data  system  and  may  not  be
valid  with  other  types  of  subtraction.  The title is 40
characters long, so to select the option p or l titles, type
_p or _l with no other characters in columns to 40.  To recall
one of the stored titles, type _t and the title number (1  to
25) in columns 1 to 40 with no other characters in columns 1
to 40.  If other characters are detected here,  the  program
will  think this is a new title and use it.  The title used,
whether recalled or new, is stored in  title  file  t1.   To
list  the  contents of the title file, type _t_l with no other
characters on the line.  The titles will be listed  and  the
program  will  again ask for a title.  Once a title has been
selected (recalled or new), further commands can  be  placed
in  columns 40 or greater.  To store a title, type _t and the
title number in columns 41 or  greater  with  the  requested
title  (recalled  or new) in columns 1 through 40.  The Band
normalization or Production processing options can  also  be
turned  on  or  off  by  control characters in columns 41 or


                      January 26, 1984







     SPECPR User's Manual                     Page 8.38


greater.  Type _b to turn on or _b_n to turn off Band  normali-
zation.   Type _p to turn on or _p_n to turn off the Production
processing option (the n is for none).

     The current operation can be terminated as  in  section
8.7  by  typing _e or _x with no other characters on the line.
Typing _e will terminate the present operation, and the  pro-
gram  will begin the next operation if there is one.  Other-
wise, it will return to Math Operations.  Typing _x will ter-
minate  all  operations  and will go directly to Math Opera-
tions.

     If a title is requested, the program will  then  go  to
the  Band  Normalization  Routine  (section 8.9) if the Band
Normalization option is on or to the CRT  plot  routines  if
the  Band  Normalization and Production options are off.  If
the Production option is on, the program will go to the Band
Normalization;  otherwise,  it will write the data and begin
processing the next command.



_8._9.  _B_a_n_d _N_o_r_m_a_l_i_z_a_t_i_o_n

     The Band Normalization is a least squares analysis over
a  user selected band with the spectrum scaled so the middle
of the band is scaled to unity.  The band can be one channel
or  however many channels are in the spectrum (256 maximum).
The Band Normalization routine is called after  each  opera-
tion  if  the Band Normalization option is on.  Channels can
be deleted by typing _d.   The  program  then  asks  for  the
points.   One  line  can be filled with points to be deleted
(numbers, no characters).  The band limits can be  moved  by
typing  _m.   Deleted channels can be reinserted by typing _r.
Plotted on the CRT is the band area  with  the  fitted  line
along  with  the  correlation coefficient, the previous Band
Normalization factor (the normalization factor of  the  data
before this normalization), the current normalization factor
(as determined by this normalization), and the  future  nor-
malization  factor  (if  this normalization is carried out).
The future normalization factor equals  the  previous  times
the  current  factors.  If the previous normalization factor
was zero when the Band Normalization routine is entered,  it
is reset to 1.0.

     To perform the normalization of the data, type  _b.   To
exit the routine, type _e.

     Any exit  from  the  Band  Normalization  routine  then
enters  to the CRT plot routines (section 9) unless the Pro-
duction processing option is on or  a  hard  exit  was  per-
formed.
9

9                      January 26, 1984







     SPECPR User's Manual                     Page 8.39


     Default Band Normalization limits when program is begun
are 30 to 38.


















































9

9                      January 26, 1984



