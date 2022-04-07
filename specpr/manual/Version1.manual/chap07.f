



     SPECPR User's Manual                      Page 7.1


_7.  _P_R_O_G_R_A_M _O_P_E_R_A_T_I_O_N_S _C_O_N_T_R_O_L


_7._1.  _I_n_t_r_o_d_u_c_t_i_o_n

     The Program Operations Control is the main part of  the
program  which  allows access to math operations, file list,
overlay, transfer, display, plot, file assignments, initial-
ization  and  other  applications  programs such as gaussian
fitting and the extinction routines.  This is also the  only
point where the program should be terminated.


_7._2.  _S_c_r_e_e_n _I_n_f_o_r_m_a_t_i_o_n

     Information on the screen can be suppressed  by  typing
_i_n or restored by typing _i.


_7._3.  _C_a_l_l_i_n_g _F_i_l_e _L_i_s_t

     To list the contents of the devices (v, w, d, u, or y),
type l followed by the corresponding letter.  See section 11
for further instructions.


_7._4.  _T_e_r_m_i_n_a_t_i_n_g _P_r_o_g_r_a_m

     To terminate the program, type _e_x.  All files and  dev-
ices will be closed properly.


_7._5.  _C_a_l_l_i_n_g _F_i_l_e _D_i_s_p_l_a_y, _T_r_a_n_s_f_e_r, _a_n_d _O_v_e_r_l_a_p

     To transfer files, or display or overlap spectra on the
CRT, type _t.  See section 10 for further information.


_7._6.  _C_h_a_n_g_i_n_g _I_n_i_t_i_a_l_i_z_a_t_i_o_n _P_a_r_a_m_e_t_e_r_s

     To change the initialization parameters  or  return  to
the  wavelength  assignment routines, type _b.  the b is left
over from the MIT program version meaning  go  back  to  the
Beginning  of  the  program.   From  here you can change the
observatory location (section 4.3),  device  names  (section
4.4),  return  to  the  wavelength  assignments (section 6),
change the device protection (section  4.2),  or  return  to
Device and File Assignments (section 5).  A multiple command
is allowed to go from Program Operations Control directly to
the  initialization parameter of interest.  To do this, type
_b and then the control letter(s)  of  the  parameter  to  be
changed;  then press return.  For example, to change protec-
tion, you could type  _b,  return,  then  the  program  lists


                      January 26, 1984







     SPECPR User's Manual                      Page 7.2


options, then you type _c_p to change protection, or you could
type _b_c_p from Program Operations Control to get to the  same
point faster (see Figure 2.1-1 for the control structure).


_7._7.  _C_a_l_l_i_n_g _D_e_v_i_c_e _a_n_d _F_i_l_e _A_s_s_i_g_n_m_e_n_t_s

     To go the Device and File Assignments, type _r  (section
5).


_7._8.  _C_a_l_l_i_n_g _E_x_t_i_n_c_t_i_o_n _R_o_u_t_i_n_e_s

     To create a starpack  or  list  the  starpack  file  by
titles, type _s (section 12).


_7._9.  _C_a_l_l_i_n_g _G_o_u_l_d _P_l_o_t _R_o_u_t_i_n_e_s

     To plot spectra on the Gould Printer  Plotter,  type  _p
(section 13).


_7._1_0.  _C_a_l_l_i_n_g _M_a_t_h _O_p_e_r_a_t_i_o_n_s

     Type _m to go to math operations (section 8).


_7._1_1.  _C_a_l_l_i_n_g _G_a_u_s_s_i_a_n _F_i_t_t_i_n_g _R_o_u_t_i_n_e_s

     Type _g to startup the gauss fit routine. Refer to  sec-
tion 14 for more information on this routine.


_7._1_2.  _C_a_l_l_i_n_g _r_e_s_t_a_r_t _s_u_m_m_a_r_y

     Typing _f will list the contents of the current  restart
file on the listing device.














9

9                      January 26, 1984



