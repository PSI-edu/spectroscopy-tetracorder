



     SPECPR User's Manual                     Page 11.1


_1_1.  _F_I_L_E _L_I_S_T

_1_1._1.  _F_i_l_e _L_i_s_t

     From the Program Operations Control, file transfer, and
Math  operations, type _l and the device file ID (v, w, d, u,
or y) to call the file list  routines.   You  will  then  be
given  a  list  of  options.  The CRT list contains only the
record   number,   title,   the   number   of   revolutions,
civil/Universal  time, sidereal time, date, and the airmass.
The options are for the  lineprinter  list  (make  sure  the
spooler is assigned if you want a lineprinter list).  Press-
ing return will list only on the CRT.

     Typing _p will list on the  lineprinter,  and  typing  _a
will do an "automatic" list on the lineprinter.  The differ-
ence between the p and a specification is that, with  the  p
specification,  the  program  halts after every 25 lines and
asks the user to type _c to continue, _x to exit, _t to  go  to
file  Transfer,  _m  to go to math operations, or type in new
file limits to list (see below) or a new p or  a  specifica-
tion.   With  the a specification, the screen is erased when
the screen is full, and the list continues until  the  limit
is reached or an I/O error occurs.  If this limit is reached
and the user  typed  c  to  continue,  the  a  specification
changes  to  a  p specification so that the program will not
list forever.  Typing _e or _x will exit the routine.   Typing
an  option  character after the p or a specification (on the
same line) will change what is printed on  the  lineprinter.
No  option  prints  the  record  number, title, revolutions,
integration time, civil/Universal time, sidereal time, date,
airmass,  and  the  normalization factor on one line and the
history just below the title.  For the case  of  average  or
summation in the history, the manual history is also printed
since this contains an extension of the history.   Option  _b
prints  all header information except the manual history and
the data.  Option _c prints all the  header  information  but
not  the  data.   Option _d prints all the header information
and the data. The manual history will always be  printed  on
the  line  printer if there are 2 asterisks in the first two
columns of each line. Only the lines with the asterisks will
be printed.

     After this, you will be asked to type in the  beginning
and  ending  records  to be listed.  Typing _e or _x will exit
the routine.  If the second number is less than or equal  to
zero,  the  routine will exit as with typing e or x.  If the
first record number is less than or equal to zero, it is set
to 1.

     When the program halts for input, you  may  type  _c  to
continue, _e or _x to exit, _t to go to File Display, Transfer,
and Overlay, _m to go to Math operations.  You may also  type


                      January 26, 1984







     SPECPR User's Manual                     Page 11.2


in new limits (beginning and ending record numbers) to list,
or you may type the _p or _a specification with options _b,  _c,
or  _d  to  change  the  type of list depending on the record
number.  If you type _p or _a, you will be asked  to  type  in
new  record limits.  The line printer may also be turned off
(no list on lineprinter) by typing  _n.   You  will  then  be
instructed to type in new record limits again.

     Thus, when listing an entire data file, you may  change
the  information printed to save time and paper and suppress
information which will never be looked at or is not needed.









































9

9                      January 26, 1984



