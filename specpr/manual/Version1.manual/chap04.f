



     SPECPR User's Manual                      Page 4.1


_4.  _P_R_O_G_R_A_M _I_N_I_T_I_A_L_I_Z_A_T_I_O_N


_4._1.  _B_e_g_i_n_n_i_n_g

     As the program is started, messages relating to  recent
changes  to  specpr  are  printed. If you wish to skip these
messages hit the DEL key (shifted _ on HP terminals) and the
SPECPR  version  date  will  be  written on the screen.  The
program waits for a carriage return before continuing.   Two
options are available at this point:

     Type _u <_f_i_l_e_n_a_m_e> to create a new restart file or  type
_r <_f_i_l_e_n_a_m_e> to use an existing restart file.

     A restart routine is used so that SPECPR may be  exited
and later restarted with the same files assigned and protec-
tion and other parameters the same as the time of the  exit.
This  routine  was written specifically for calling sons and
daughters of SPECPR but is also useful for general use.  The
restart parameters are stored in a disc file and are updated
each time the program operations control routine is entered.
Thus,  when  doing math operations or transferring, the res-
tart parameter file is not updated until the entry  to  pro-
gram operations control.


_4._2.  _P_r_o_t_e_c_t_i_o_n

     All files may be totally  or  partially  protected,  or
completely  unprotected.   The fourth line of the CRT header
gives the protection status for all six devices (devices  v,
w, d, u, v, and s).  If the protection number is positive or
zero, the device is a read/write device where you  can  read
up  to  and including the protection, but you can only write
to the protection +1 file.  A protection number of -1  means
the  device  is  a read-only device where you can read up to
the absolute value of the protection number.    The  protec-
tion  numbers are typed in on one line in the initialization
routine as the file ID and protection number.  Example:  "v0
d-1  u432  y-600  w-600  s-50",  where v0 means to protect 0
files on device v (write to file 1  only,  0  files  can  be
read);  u432  means  to protect files up 732 files (write to
file 433); y-600 means that y is a read-only device with 600
files;  similarly for w; and s is a read only device with 50
files.  If the protection number is zero or positive, it  is
incremented each time the device is written to.  One or more
device protection numbers may be changed on the  same  line.
See section 2.3 for more details.



9

9                      January 26, 1984







     SPECPR User's Manual                      Page 4.2


_4._3.  _O_b_s_e_r_v_a_t_o_r_y _L_o_c_a_t_i_o_n

     Three letter codes are shown for  often  used  observa-
tories.   If  you wish another location, type _t and then the
program will  request  the  latitude  in  degrees,  minutes,
seconds,  free  format.  Longitude is not needed because the
sidereal time is contained with each spectrum.  The observa-
tory  location  is used only in computing the airmass in the
object-sky subtraction routine.



_4._4.  _D_e_v_i_c_e _N_a_m_e_s

     The device names are the names of the  mag  tapes  from
which the data comes or will be written.  The tape names are
a maximum of 8 characters.  You must be certain these  names
are correct if you want the histories to be correct.


_4._5.  _F_i_l_e _N_a_m_e_s

     The file names are the UNIX system file names. Refer to
the  UNIX  documentation for a discussion of UNIX files. Due
to programming restrictions in SPECPR these file  names  are
restricted  to a maximum of 40 characters each.  Normally it
is the practice that the name of the file (not including the
full  UNIX  directory  path)  is  equal to the tape names as
entered in section 4.5.4.


_4._6.  _G_r_a_p_h_i_c_s _O_p_t_i_o_n_s

     There are several graphics options  available  in  this
program.   They  were  implemented to allow specpr to be run
from different terminals.  (HP2623A,  HP2648A,  non-graphics
terminals)  Please  note  that the graphics options does not
show up at the beginning of a new restart, but,  has  to  be
set from the Initialize Parameter Routine via Program Opera-
tions.  These are the options available:



        g2    Set White on Black mode (default)
        g3    Compliment, White on Black mode
        g11   Set Black on White mode
        g13   Compliment, Black on White mode
        g99   Scrolling mode (non-graphics terminal)
        g#
7              # < 10, Jam mode White on Black # >
              10, Jam mode, Black on White


9

9                      January 26, 1984



