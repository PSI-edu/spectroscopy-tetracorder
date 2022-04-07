	SUBROUTINE RMPATH (chbuff,name)

#ccc  name: rmpath
#ccc  version date: 1988
#ccc  author(s): Barry Middlebrook
#ccc  language: ratfor
#ccc
#ccc  short description:
#ccc
#ccc  algorithm description:
#ccc  system requirements:
#ccc  subroutines called:
#ccc  argument list description:
#ccc  parameter description:
#ccc  common description:
#ccc  message files referenced:
#ccc  internal variables:
#ccc  file description:
#ccc  user command lines:
#ccc  update information:
#ccc  NOTES:
#ccc

#*******************************************************************
#   TITLE:             REMOVE PATH ROUTINE                         *
#   PROGRAMMER:        Barry J. Middlebrook                        *
#------------------------------------------------------------------*
#   DESCRIPTION:                                                   *
#               This algorithm is designed to remove the path name *
#               from the full file designation.  It accomplishes   *
#               this by finding the last slash and the first space *
#               in the character buffer holding the file designa-  *
#               tion.  The file name is extracted by setting the   *
#               file name variable equal to the character string   *
#               bounded by last slash position plus 1 and first    *
#               space position minus 1 in the input character buf- *
#               fer.  As an example, consider the file designation *
#               given here:                                        *
#               	   u/src/filename                          *
#               	   ^^^^^^^......^                          *
#               	   1234567      14                         *
#                                                                  *
#               The routine finds the last slash index value which *
#               is equal to 6 and the first space index value      *
#               which is equal to 15.  It would then conclude that *
#               the file name is positioned in the input character *
#               buffer from the index 7 to 14.  Therefore,         *
#               <file name> = <character buffer (7:14)>            *
#                                                                  *
#               NOTE:  Path name spaces are allowed, however, the  *
#                      file name will be incorrect if it has       *
#                      any spaces within the name or more than one *
#                      preceding it.                               *
#                                                                  *
#------------------------------------------------------------------*
#   VARIABLES:                                                     *
#             maxlen - maximum number of characters in input char- *
#                    acter buffer                                  *
#             i      - integer value for positioning the charac-   *
#                    ter indexer to the next character after the   *
#                    slash found                                   *
#             chtest - integer value that temporarily stores the   *
#                    index of the character searched for           *
#             spapos - first space found position index in input   *
#                    character buffer                              *
#             slapos - last slash found position index in input    *
#             test   - logical variable controlling loop to find   *
#                    last slash                                    *
#             chbuff - input character buffer containing the input *
#                    file designation (path/filename)              *
#             name   - file name without path                      *
#             slash  - character variable containing a slash       *
#             space  - character variable containing a space       *
#                                                                  *
#*******************************************************************

#  Set variable type
	IMPLICIT INTEGER*4 (i-n)
	INTEGER*4       maxlen,i,chtest,spapos,slapos
	PARAMETER       (maxlen=80)
	LOGICAL         test
	CHARACTER       chbuff*80,name*8,space,slash
	INCLUDE         "../common/lundefs"

#  Decode input string comprised of path name and file name 
	#  Initialize variables for test
	space=' '
	slash='/'
	test=.TRUE.
	slapos = 0
	i=1

#  Test to search through character string for the last slash 
	while (test & i <= maxlen)  {
	   chtest=INDEX(chbuff(i:maxlen),slash)

	   if (chtest == 0)  {
	      test=.FALSE.
	   }
	   else  {
	      slapos=chtest+slapos
	      i=slapos+1
	   }
	}

#  Test to find position of first space
	spapos=INDEX(chbuff,space)

#  Tests to check for errors in indexing
	if (spapos <= slapos)  {
	   WRITE (ttyout,*)'WARNING - space detected before slash,'
	   WRITE (ttyout,*)'* name may be incorrect *'
	   i=spapos+1
	   chtest=1

	   while (chtest != 0 & spapos < slapos+1)  {
	      chtest=INDEX(chbuff(i:maxlen),space)
	      spapos=spapos+chtest 
	      i=slapos+1
	   }
	}
	else if (spapos == 0)  {
	   WRITE (ttyout,*)'WARNING - no space character detected'
	   spapos=maxlen
	}
	else  {
	   CONTINUE
	}

#  Extract file name from full path name
	chtest=(spapos-1)-(slapos+1)
	if (chtest > 8) spapos=slapos+9
	name=chbuff(slapos+1:spapos-1)   

#  End program and return to calling main
	RETURN
	END
