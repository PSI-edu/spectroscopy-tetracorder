



     SPECPR User's Manual                      Page 5.1


_5.  _D_E_V_I_C_E _A_N_D _F_I_L_E _A_S_S_I_G_N_M_E_N_T_S


_5._1.  _A_s_s_i_g_n_m_e_n_t_s

     The device and file assignment status is  displayed  on
the CRT.  Throughout the program, the device and file status
in short form is displayed in the top 4 lines  on  the  CRT.
This  tells  whether  or not the devices are assigned, where
assigned, the current file pointer position,  savefile  pro-
tection, wavelength file in use, number of channels, and the
device names.

     Devices assigned to /dev/null means these  devices  are
not  accessed.   If  access  is  requested  when assigned to
/dev/null,  the program cannot respond and will tell you so.

     Typing _e or _x will cause the program  to  exit  to  the
next routine.  Multiple commands are allowed in the and file
assignment section.

     Example: Assign v to file lab501, d to file  lab502,  u
to mt0, and l to the spooler.

_v_l_a_b_5_0_1 _d_l_a_b_5_0_2 _u/_d_e_v/_m_t_0 _l_s_p_o_o_l_e_r

(note: spaces are allowable between the device names and the
file  names.  spaces  are  necessary before the device names
except the first.)

     assigning the list device to "spooler"  will  automati-
cally  dump  the  listings  to  the line printer as they are
created. If you assign the list device to a file  the  list-
ings  will  be  appended  to that file so that you can print
them later.

















9

9                      January 26, 1984



