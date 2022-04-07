



     SPECPR User's Manual                     Page 13.1


_1_3.  _G_O_U_L_D _P_L_O_T _R_O_U_T_I_N_E_S

_1_3._1.  _T_h_e _G_o_u_l_d _P_l_o_t _R_o_u_t_i_n_e_s

     The Gould 5200 plot routines are  designed  to  provide
nearly  all  types  of plotting capability.  Plots can be in
wavelength space, energy space  (energy  increasing  to  the
right),  reverse  energy  space  (energy  increasing  to the
left), or linear space with your own  label.   The  vertical
scale  can  be  either  linear or logarithmic with the label
selected from a list or typed in by the user.  Text  can  be
inserted  on  the plot.  There are many options for control-
ling the type of plot.  The plot is built up on the disc  as
a  binary  image containing 5.0x10869 bits.  The resolution of
the plot is 200 lines per inch.  Each  plot  takes  6  to  8
minutes  or  more  depending  upon the number of jobs in the
system.  The plot routines are called  from  Program  Opera-
tions by typing _p.

_1_3._2.  _P_l_o_t_t_i_n_g _S_p_a_c_e _T_y_p_e

     Upon entering the  Plot  routine,  the  plotting  space
options are given.  Type _w to plot in wavelength space, _n to
plot in energy space (energy increasing to the right), _r  to
plot  in  reverse  energy  space  (energy  increasing to the
left), or _y to type in your own horizontal axis  label  (the
plot  will  be linear).  If you type _y, you will be asked to
type in the horizontal axis label (24 characters).  Here you
might type: _T_I_M_E (_S_E_C_O_N_D_S) or whatever your data corresponds
to.  Thus, you may do any kind of one dimensional array pro-
cessing you wish.

     _U_s_e_r _s_e_l_e_c_t_e_d _w_a_v_e_l_e_n_g_t_h _l_i_m_i_t_s:  You  may  select  the
wavelength limits for the plot, regardless of the wavelength
files.  When you type in the "w" for wavelength space (or n,
r,  or y), also type (on the same line) the minimum and max-
imum wavelengths for the  plot.   Type  in  wavelengths  for
energy space also.  Example:


            w 0.325        2.60
            n 0.325        2.60
            r 0.325        2.60
            y 0.325        2.60



The actual limits on the plot will be 2%  greater  than  the
range  (maximum-minimum)  on each side (see section 13.5 for
details).

     The  left  bound  is  then  minimum  minus  2%  of  the
maximum-minimum  range,  and  the right bound is the maximum


                      January 26, 1984







     SPECPR User's Manual                     Page 13.2


plus 2% of the maximum-minimum range.

     To exit the plot routine, type _e or _x.  If you choose a
space, the program will proceed to the vertical axis label.



_1_3._3.  _V_e_r_t_i_c_a_l _A_x_i_s _L_a_b_e_l

     Five vertical scale labels are  displayed  for  you  to
select.   They are numbered 1 to 5.  To select one of these,
type in the number.  If you wish to type in your own  label,
type  _6, and you will be instructed to type in the label (60
characters, maximum).  If you wish to plot the vertical axis
in  log  (base  10) space, type _7, and you must type in your
own vertical axis label.  The program will then  proceed  to
the  Delete From All Spectra section.  Type _e or _x to return
to the beginning (section 13.2).



_1_3._4.  _D_e_l_e_t_e _F_r_o_m _A_l_l _S_p_e_c_t_r_a

     If, from all the spectra you are  going  to  plot,  you
wish  to  delete one or more channels common to all, type _d.
You will then be instructed to type in  the  channels  which
will  be  deleted  from all spectra to be plotted.  At least
one space should be left between each pair of channels,  and
more  than one line can be used for input.  All 256 channels
may be deleted if desired.  When you are through, type _c  to
continue to the scale section.  Type _e or _x to return to the
beginning (section 13.2).  When  typing  in  a  sequence  of
channels,  you  may give the beginning channel, a _t, and the
end channel; for instance, to delete channels 1, 2, 3, 4, 5,
6, 7, 8, 15, 16, 17, 23, and 24, type _d_1_t_8 _1_5_t_1_7 _2_3 _2_4_c.



_1_3._5.  _S_c_a_l_e _o_f _P_l_o_t

     The vertical scale of the plot is selected by the user.
Type  in  the  lower  and upper bound of the plot.  The plot
size is 17 centimeters (vertical axis)  and  21  centimeters
(horizontal axis) for the data plot area.

     If your data is to be plotted in log space on the vert-
ical  axis,  the  lower  and  upper bound should be input as
ordinary numbers.  The program will convert the scale limits
and  data  to logs.  The scale is then labeled as the log of
the number.  Type _e or _x to return to the beginning  of  the
plot routines (section 13.2); otherwise, the program will go
to the file input section.
9

9                      January 26, 1984







     SPECPR User's Manual                     Page 13.3


_1_3._6.  _F_i_l_e _I_n_p_u_t _a_n_d _O_p_t_i_o_n_s

     The data sets to be plotted are input as  the  file  ID
(v, w, u, y, or d), the record number, and the options.  For
all data files input, the wavelength set assumed  is  record
1.   If  another  wavelength set is to be used, this must be
requested as an option.  A maximum  of  50  spectra  may  be
plotted  at  one  time.   These  may  be all on one graph or
separated into many plots by an option request.  The options
are listed below.

     Option e       is include Errors.


     Option c       means to Connect the  data  points.   If
                    the  wavelengths are not in sequence, or
                    a channel has been deleted,  the  points
                    involved are not connected.


     Option C       means to Connect the data points.   This
                    option permits the wavelengths to be NOT
                    in sequence, but will connect them  any-
                    way.


     Option n       means that, if a point lies outside  the
                    lower  or  upper  bound,  it  should  be
                    deleted.  If this specification  is  not
                    given,  the point will be plotted at the
                    limit of the graph.


     Option a       followed by a number (1 to 99) means the
                    wavelength   set  is  the  number.   The
                    default record number is 1.


     Option p       followed by a number is the point  size.
                    This  number  can  be  0  to  5.  If the
                    number is zero, no point will  be  plot-
                    ted.   Point size 1 is about 1.27mm on a
                    side, size 2 is twice this,  size  3  is
                    three  times,  etc.  Note, if point size
                    zero is specified  and  points  are  not
                    connected,  no data will be plotted, and
                    you will have  a  blank  plot!   A  line
                    drawing  can be made by specifying point
                    size zero and connect points.  The point
                    size   default  varies  with  the  total
                    number of channels per spectrum.  It  is
                    size  3  for 30 channels or less, size 2
                    for 31 to 100 channels, and size  1  for


                      January 26, 1984







     SPECPR User's Manual                     Page 13.4


                    more than 100 channels.  The symbol type
                    changes with each spectrum plotted.  See
                    Table 13.6.1.


     Option l       followed by a  number  1,  3,  5,  or  7
                    selects  the line size for the plot. The
                    line size for the axis is  fixed  at  3.
                    The number 1, 3, 5, or 7 is the width of
                    the line in Gould units. Size 1 is about
                    1/200  of  an inch (0.127 mm).  The line
                    size changes error  bars  and  the  line
                    connecting  points.  The default is size
                    1.


     Option g       means the spectrum after this one is  to
                    be  plotted  on  a  new graph.  Thus, 15
                    spectra may be plotted on a single graph
                    (overlaid),   or  they  may  be  plotted
                    singly or any combination.  The  g  (new
                    Gould  plot)  selects when a new plot is
                    to  begin.    Fifteen   plots,   plotted
                    singly, may take 90 minutes or more.


     Option d       means to delete points from  this  spec-
                    trum only (as opposed to delete from all
                    spectra input as in section 13.4).   The
                    program  will  ask  for  channels  to be
                    typed in when it is  ready.   More  than
                    one  line may be used to input the chan-
                    nels to be deleted  (256  channels  max-
                    imum).   When you are finished inputting
                    channels, type _c to continue.


     Option m       More copies:  In  the  options,  specify
                    "m"  and the number of copies desired (1
                    to 9) default=1.   Note  you  need  this
                    option  only  on the last spectrum to be
                    put on the plot.


     Option s       Symbol type:   To  change  symbol  type,
                    specify  "s" and the symbol number (1 to
                    8). See table 13.6.1.  The  symbol  size
                    is given by the "p" specification (point
                    symbol size).  The symbol type is incre-
                    mented  for  each  spectrum input and is
                    reset to 1 for each new Gould plot  ("g"
                    specification).
9

9                      January 26, 1984







     SPECPR User's Manual                     Page 13.5


     Option r       Since mistakes are bound to happen  when
                    typing  in  all  these  spectra, you may
                    return to the last step for re-inputting
                    your  data.   This  is done by typing an
                    "r" in place of a  file  ID  and  record
                    number.  Example:  You type in the spec-
                    tra


                                1)  "v23ea2p3"
                                2)  "v29ep4"
                                3)  "v30a3p1c"
                                4)  "v33a5".


                    You now notice the step #3  should  have
                    been  "v31a3p1c".   You  are  at step 5:
                    you type "r"; the computer says

                            "RETURN TO 4; CONTINUE".

                    You then type "r" again,  and  the  com-
                    puter responds

                            "RETURN TO 3; CONTINUE".

                    You may then retype step 3 (and also all
                    steps after 3).  You may not return to a
                    step before the first spectrum input (if
                    you  need  to,  you  must  "e" or "x" to
                    exit).


     Option t       means to  include  text  on  the  graph.
                    Text  may  be  written  as 11 sets of 20
                    characters per  set  for  each  spectrum
                    entered.   The text position is given in
                    centimeters [horizontal  direction  (x),
                    then  vertical (y)] from the lower left-
                    hand corner of the  axes  of  the  plot.
                    After  the  x  and y position, input the
                    text angle (90 is horizontal  and  0  is
                    vertical)  and then the text to be input
                    (20 characters maximum).  All that  goes
                    on  one  line.   When  you  are finished
                    inputting text, type _c in place  of  the
                    horizontal  text  position  number.  One
                    character=0.40 cm wide use line feed  of
                    0.70 cm.


     _T_e_x_t _e_n_t_r_y: You may also return to  the  previous  text
entry  using  the  "r" specification as with the file input.


                      January 26, 1984







     SPECPR User's Manual                     Page 13.6


You may not return to a previous entry before text entry  1.
If  you  have an error in a previous file entry, type "c" to
continue to next step after text entry; then return to  last
file input as described above.  The "s" in the text entry is
used for symbol entry.  When at a text entry point, type "s"
then  the following numbers:  symbol type (1 to 8), the sym-
bol size (1 to 6), and the x (0-21 centimeters) and y  (0-17
centimeters)  coordinates  of the symbol position.  Separate
numbers by spaces.

     The SPECPR  Gould  plot  routines  allow  subscripting,
superscripting, backspacing and greek and math characters in
the text.  Special characters are used  to  enter  the  dif-
ferent modes and can be used wherever text is written on the
plot (horizontal or vertical axis  labels  or  other  text).
The characters are:


7_C_h_a_r_a_c_t_e_r
7                         _E_f_f_e_c_t
\ (Backslash)            Backspace to previous character
{ (Left Brace)           Go into subscript mode
} (Right Brace)          Go into superscript mode
@ (At sign)              End last scripting mode
! (Exclaimation Point)   Toggle greek/normal mode (see table 13.6.2)
| (Vertical Bar)         Toggle math/normal mode (see table 13.6.3)


                        Table 13.6.2

8                  _______________________
                    Greek character set
8                  _______________________
                   A A   A a   N @   N n
                   B B   B b    X    C c
                   G \   G g   O o   O o
                   W D   D d   P J   P p
                   E S   E e   P K   R r
                   Z Q   Z z   R Y   S s
                   H N   Y y   T I   T t
                   T O   H h   Y v   U u
                   I i   I i   F U   F f
                   K k   K k   X x   X x
                   E L   L l   Z C   W w
                   M M   M m
8                  _______________________
7                 |7|7|7|7|7|7|7|7|7|7|7|7|7|












                            |8|7|7|7|7|7|7|7|7|7|7|7|










9                                        |7|7|7|7|7|7|7|7|7|7|7|7|7|














Example:

        1.04Mm would be inputed as: 1.04!m!m


                        Table 13.6.3





9                      January 26, 1984







     SPECPR User's Manual                     Page 13.7


8_________________________________________________________________
                       Mathmatical Symbols
8_________________________________________________________________
 A          infinity (oo)            _              _
 B        improper subset ()         a              |^
 C         proportional ()           b
 D            union (U)              c          section ()
 E         root extender ()          d              @
 F             bell ()               e       double dagger ()
 G        plus or minus (+_)          f              |
 H    less than or equal to (<_)      g      improper subset ()
 I   greater than or equal to (>_)    h              \
 J          square root ()           i          circle (O)
 K        terminal sigma ()          j    partial derivative (])
 L        integral sign (^)          k         empty set ()
 M            subset ()              l              {
 N           superset ()             m              }
 O         intersection ()           /              /
 P             not (_)               "              "
 Q         clover leaf (c/)           #              #
 R           angstrom ()             '              '
 S           gradient ([)            +              +
 T            times (x)              -              -
 U            divide (/)             *              *
 V      identically equal (=_)        <              <
 W     approximately equal (=~)       =              =
 X          not equal (=/)            >              >
 Y                <-                 ^              ^
 Z                ->                 `              `
8_________________________________________________________________
7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|


























                                 |8|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|
























9                                                                |7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|






























     It was assumed that these characters would not need  to
be  printed so they have been reserved for the above special
use.  Backslash can be used to cause characters to be  over-
printed.   An  "at sign" terminates the last use of a brace.
All characters in between are printed in script mode.

Examples:

    syntax error
    file -, between lines 1183 and 1186
    \*(11




     Typing _x in place of a file ID will cause  the  program
to  return  to  the beginning of the routine (section 13.2).
Typing _b will begin building the vector and  text  files  on
the disc.  Note you must run the plotdaemon program and turn
on the Gould printer plotter.



9                      January 26, 1984







     SPECPR User's Manual                     Page 13.8


_1_3._7.  _B_u_i_l_d_i_n_g _t_h_e _V_e_c_t_o_r _a_n_d _T_e_x_t _f_i_l_e_s

     The gould plot routine builds a set of  two  files  for
each  plot  to be done. One of these files contains the vec-
tors to be plotted and the other contains all the  text  and
symbols  to  be  plotted.   Each  stage  of the plot will be
printed  on  the   CRT.   These   files   are   located   in
/usr/spool/gplot.


_1_3._8.  _R_u_n_n_i_n_g _t_h_e _p_l_o_t_d_a_e_m_o_n _r_o_u_t_i_n_e

     After building the text and vector files you  must  run
the  plotdaemon program which takes these files and converts
them into a raster file and dumps it onto the gould plotter.
To run this routine from specpr type !_p_l_o_t_d_a_e_m_o_n.  This will
start up the routine and return to specpr. Be sure that  the
program  LSA is running on the TI and that the gould plotter
is turned on and is online. Also be sure that there is  suf-
ficient  paper  in  the gould for all the plots that will be
built. (If you run out of paper you may lose  some  of  your
plots, and possibly someone elses plots too!)






























9

9                      January 26, 1984



