      subroutine rmpath (chbuff,name)
c*******************************************************************
c   TITLE:             REMOVE PATH ROUTINE                         *
c   PROGRAMMER:        Barry J. Middlebrook                        *
c------------------------------------------------------------------*
c   DESCRIPTION:                                                   *
c               This algorithm is designed to remove the path name *
c               from the full file designation.  It accomplishes   *
c               this by finding the last slash and the first space *
c               in the character buffer holding the file designa-  *
c               tion.  The file name is extracted by setting the   *
c               file name variable equal to the character string   *
c               bounded by last slash position plus 1 and first    *
c               space position minus 1 in the input character buf- *
c               fer.  As an example, consider the file designation *
c                                                                  *
c               u/src/filename                                     *
c               1234567      14                                    *
c                                                                  *
c               The routine finds the last slash index value which *
c               is equal to 6 and the first space index value      *
c               which is equal to 15.  It would then conclude that *
c               the file name is positioned in the input character *
c               buffer from the index 7 to 14.  Therefore,         *
c               <file name> = <character buffer (7:14)>            *
c                                                                  *
c               NOTE:  Path name spaces are allowed, however, the  *
c                      file name will be incorrect if it has       *
c                      any spaces within the name or more than one *
c                      preceding it.                               *
c                                                                  *
c------------------------------------------------------------------*
c   VARIABLES:                                                     *
c             maxlen - maximum number of characters in input char- *
c                    acter buffer                                  *
c             i      - integer value for positioning the charac-   *
c                    ter indexer to the next character after the   *
c                    slash found                                   *
c             chtest - integer value that temporarily stores the   *
c                    index of the character searched for           *
c             spapos - first space found position index in input   *
c                    character buffer                              *
c             slapos - last slash found position index in input    *
c             test   - logical variable controlling loop to find   *
c                    last slash                                    *
c             chbuff - input character buffer containing the input *
c                    file designation (path/filename)              *
c             name   - file name without path                      *
c             slash  - character variable containing a slash       *
c             space  - character variable containing a space       *
c                                                                  *
c*******************************************************************
c  Set variable type
      implicit integer*4 (i-n)
      integer*4 maxlen,i,chtest,spapos,slapos
      parameter (maxlen=80)
      logical test
      character chbuff*maxlen,name*8,space,slash
      integer*4 luntxt,ulun,ylun,ttyin,ttyout,dlun,vlun,wlun
      integer*4 addlun,wavlun,lstlun,pltlun,wvhlun
      integer*4 ttllun,cmdlun,slun,rlun,cpylun,wrtlun,redlun
      parameter (luntxt=1)
      parameter (ulun=3)
      parameter (ylun=4)
      parameter (ttyin=5)
      parameter (ttyout=6)
      parameter (dlun=7)
      parameter (vlun=8)
      parameter (wlun=9)
      parameter (addlun=10)
      parameter (redlun=10)
      parameter (wrtlun=11)
      parameter (wavlun=11)
      parameter (lstlun=12)
      parameter (pltlun=13)
      parameter (wvhlun=14)
      parameter (ttllun=15)
      parameter (cmdlun=16)
      parameter (slun=17)
      parameter (rlun=18)
      parameter (cpylun=19)
c  Decode input string comprised of path name and file name 
c  Initialize variables for test
      space=' '
      slash='/'
      test=.true.
      i=1
c  Test to search through character string for the last slash 
c     while
23000 if(.not.(test))goto 23001
         chtest=index(chbuff(i:maxlen),slash)
         if(.not.(chtest .eq. 0))goto 23002
            test=.false.
            goto 23003
c        else
23002       continue
            slapos=chtest+slapos
            i=slapos+1
23003    continue
         goto 23000
c     endwhile
23001 continue
c  Test to find position of first space
      spapos=index(chbuff,space)
c  Tests to check for errors in indexing
      if(.not.(spapos .le. slapos))goto 23004
         write (ttyout,*)'WARNING - space detected before slash,'
         write (ttyout,*)'* name may be incorrect *'
         i=spapos+1
         chtest=1
c        while
23006    if(.not.(chtest .ne. 0 .and. spapos .lt. slapos+1))goto 23007
            chtest=index(chbuff(i:maxlen),space)
            spapos=spapos+chtest
            i=slapos+1
            goto 23006
c        endwhile
23007    continue
         goto 23005
c     else
23004    continue
         if(.not.(spapos .eq. 0))goto 23008
            write (ttyout,*)'WARNING - no space character detected'
            spapos=maxlen
            goto 23009
c        else
23008       continue
            continue
23009    continue
23005 continue
c  Extract file name from full path name
      name=chbuff(slapos+1:spapos-1)
c  End program and return to calling main
      return
      end
