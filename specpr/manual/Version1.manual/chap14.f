



     SPECPR User's Manual                     Page 14.1


_1_4.  _G_a_u_s_s_i_a_n _F_i_t_t_i_n_g _P_r_o_g_r_a_m

     The Gaussian Fitting Program or GFIT was  adapted  from
the version currently on the Texas Instrument 980B computer.
It is now available on the LSI 11/23 computer.  The TI  ver-
sion  of  GFIT  was mainly written to be dependent on the HP
graphics terminal. The LSI version was rewritten so that the
plotting  package  was  the  only  section that was terminal
dependant. Thus, you must be on the HP graphics terminal  to
be plotting but not if doing just fitting.  For those of you
who are familiar with the TI version, the  LSI  version  has
been changed in different areas but clarity was kept in mind
while rewriting. The difference will probably  not  be  that
drastic  for  your  use.  The plotting package has been also
changed to make it easier  to  understand.  I  will  explain
these changes later as we get to them.

     The algorithm is based on the paper by Kaper _e_t. _a_l.  (
_B_u_l_l.  _A_s_t_r. _I_n_s_t. _N_e_t_h_e_r_l_a_n_d_s.  _1_8 465-487. 1966). The main
computation routine was obtained from  Dr.  Chase  Biechman,
now  at  the California Institute of Technology. For a clear
discussion of nonlinear least squares fitting algorithms see
Bevington()

_1_4._1.  _G_F_I_T _S_t_a_r_t_u_p

     To start GFIT you must do some preparations in specpr.

        Check Number of Channels
        Check Wavelength Assignment
        Check File Assignments
        Make sure you are in graphics mode

     Files in GFIT  are  referred  by  there  assignment  in
specpr, using v, w, d, u, or y as file id and record number.
So it is important that the  correct  file  assignments  are
done in specpr.

     There is also a GFIT restart file. If it is your  first
time using GFIT, the program still create a raw restart file
for you.  IF a restart file exists then a restart  is  done.
The  GFIT restart file is denoted by the name of your specpr
restart file with a ".gf" appended to it.  If  your  restart
file  name  is  "spstrt",  your  GFIT  restart  file name is
"spstrt.gf".  If you access GFIT with another specpr restart
file then the GFIT restart file will also change.  GFIT res-
tart files associate  themselves  with  the  specpr  restart
files and are unique to them.

_1_4._2.  _P_a_r_a_m_e_t_e_r_s


9

9                      January 26, 1984







     SPECPR User's Manual                     Page 14.2


_1_4._2._1.  _E_s_t_i_m_a_t_e _P_a_r_a_m_e_t_e_r_s

     In order to fit gaussians to your data you  must  first
set  up  certain parameters for GFIT.  This is done by going
to the ESTIMATE PARAMETER routine by typing  ep  from main.

Type

a   and the title of the output specpr file
b   and the file id and record number of the input specpr file
c   and the wavelength record number
d   and the number of gaussians to ge fitted
e   and the number of  continuua
f   and the number of iterations
g   and the accuracy (real number)
h   toggle errors if included
i   toggle invert wavelength
j   toggle log space
k   delete points
x   EXIT


     When deleting points, you can specify a range of points
by  giving  the  first number then a negative (-) number for
the ending point.  (i.e. 59 -65  deletes points  59  through
65) ** When entering deleted points in the routine make sure
you separate the numbers by spaces is you enter them on  one
line.

_1_4._2._2.  _E_s_t_i_m_a_t_e _G_a_u_s_s_i_a_n_s

     You now must guess the values of the gaussians you want
fitted.  Type  eg  to go to the ESTIMATE GAUSSIANS routine.

     To change the gaussian parameter values.

     Type

   a
7       and the gaussian number to change all the  parame-
       ters of that gaussian
   h   and the gaussian number to change height value
   w   and the gaussian number to change the width value
   c   and the gaussian number to change the center value

Do this for each gaussian.

     To enter the continuum values type  l  then  enter  the
values for the continuua. (const, x , x**2, x**3, x**4, x**5
order continuua possible) Or, press return  to  let  program
calculate  the  continuum  for you.  at this point you enter
the end points of the line segment and the program will cal-
culate  the  values for you, but only for a two term contin-
uum.


                      January 26, 1984







     SPECPR User's Manual                     Page 14.3


     Type e or x to exit

_1_4._2._3.  _F_i_t _P_a_r_a_m_e_t_e_r _R_e_v_i_e_w

     Type  fp to review the parameters passed  back  by  the
fitting  routine.  This routine is for review only, you can-
not change any parameters in this routine.

_1_4._2._4.  _F_i_t _G_a_u_s_s_i_a_n_s _R_e_v_i_e_w

     This routine  is  for  reviewing  the  gaussian  values
passed  back  by  the fitting routine.  If you desire to use
these values for your next fit guesses you  can  move  these
values into the guess area.  See Transfer data section.

_1_4._3.  _F_i_t

     This section is for the fitting of your guesses to  the
input data.  To fit guesses type  fi  then return.  The pro-
gram will return with a message saying if you were  success-
ful  or  not.   The  program   will return with a message of
unable to converge after given iterations  if  the  fit  was
unsuccessful.  The fit recap will be printed out on the line
printer.  This includes the  fit,  and  gaussian  parameters
derived  in  the fit, base line results, fitting errors, sum
of the residuals,  mean  value  of  the  residuals  and  the
dispersion, and the integrated line intensities.

_1_4._4.  _T_r_a_n_s_f_e_r _D_a_t_a

     There are three places that parameters of a  given  fit
can  be  stored.   These  parameters  are  title, file name,
number of wavelengths, wavelength number, number  of  itera-
tions,  number  of gaussians, number of continuua, accuracy,
etc.

     The parameters that you set up in the ESTIMATE  PARAME-
TER  section  is put into the estimate area.  The parameters
derived in the fit routines are put into the fit area.   The
third  area  is saved for a scratch area.  The parameters in
the estimate area  are  used  by  the  fit  routine  as  the
guesses,  and  the generated gaussian values, continuua, and
the parameters are put into the fit area.  The need  to  use
the  parameters  in  the  fit  area as guesses allows you to
transfer parameter data from one area  to  the  other.   The
scratch area allows you to save one area if you wish to have
it around for future use.

     The transfer commands are:

            es   transfer estimate to scratch
            se   transfer scratch to estimate



                      January 26, 1984







     SPECPR User's Manual                     Page 14.4


            fs   transfer fit to scratch
            sf   transfer scratch to fit
            fe   transfer fit to estimate
            xe   exchange estimate  and scratch
            xf   exchange fit and scratch


_1_4._5.  _S_a_v_e, _R_e_c_a_l_l _G_F_I_T _P_a_r_a_m_e_t_e_r_s

     The above section dealt with  parameters  for  a  given
fit.   This  section  deals with the saving and recalling of
these parameters as a group.  The estimate, fit and  scratch
areas  are  in  one  group  which is involved usually with a
given input data set.  If you want to do fits on other  sets
of  data, it is possible to save these parameters and create
a new set of parameters for a new data set.  This means that
the  estimate,  fit and scratch areas are saved allowing you
to create parameters for the new data set and still have the
old  parameters  around.  These sets of parameter groups are
saved in the GFIT restart file and will be around  when  you
logon  later.   These parameters are stored by record number
and can be recalled by the correct record number.  All areas
can be recalled or just the estimate or fit parameters.

     The commands are:

              sa   save all parameter areas
              re   recall all parameter areas
              rf   recall fit parameters
              rs   recall estimate parameters


_1_4._6.  _W_r_i_t_e _T_o _S_P_E_C_P_R _F_i_l_e

     GFIT writes a set of specpr  records.   To  write  data
type  wr.

     The data sets written are:

        record:
           1      Original data with points deleted
           2      Errors (if any)
           3      Fitted continuum
           4      Individual gaussians
           5      Fit
           6      Fit minus continuum
           7      Input data  minus continuum
           8      Errors (if included)
           9      Residual errors
          10      Parameter values


9

9                      January 26, 1984







     SPECPR User's Manual                     Page 14.5


     The parameter values are stored in the data array.   To
see  the the parameter values you must print the data on the
line printer from specpr.

_1_4._7.  _G_F_I_T _P_l_o_t_t_i_n_g _P_a_c_k_a_g_e

     The plotting section has two parts.  First  part  plots
the estimates, second, plots the fit.  Automatically plotted
when you enter the plotting section is the input  data  with
points deleted and the output data.

     To enter plotting section type  pe  for plotting  esti-
mates or pf  for plotting fit.

     *note*  be sure that you have done  a  fit  before  you
type  pf to plot fit.

     The log space and inverse wavelength switches are apart
from  the  estimate  parameters.   So  to  plot in log space
and/or inverse wavelength you must set the mode by resetting
the  modes  in the plot section.  This is done by typing  il
and then answer the prompts with a  y  for  yes  and  an   n
for no  .

     Plotting commands are:

eg   erase graphics
g-   and gaussian number to plot gaussian - continuum (SEE note)
ga   and a gaussian number to plot gaussian + continuum,
co   plot continuum
o-   output fit - continuum
i-   input - continuum
ie   input errors (not implemented)
oe   input - continuum errors (not implemented)
e    exit from plotting
li   list plotting commands
il   modify logp and invp
sc   change scale of plot


-- note --  You can give a range of  gaussians  to  plot  by
typing  the  command g- or ga followed by the starting gaus-
sian number,  t  , then the ending gaussian number (i.e.  ga
1 t 5   for gaussians 1 to 5)

_1_4._8.  _M_i_s_c. _C_o_m_m_a_n_d_s

         li   list commands in main
         lf   list titles of the saved parameters
         e    exit from GFIT
         pr   print fit parameters on line printer

9

9                      January 26, 1984



