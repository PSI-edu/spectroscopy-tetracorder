      subroutine Qsmnh (nminer,ws,d,dens,weight, 
                        wsmean,wband,currch,aflag)
        implicit none
 
#     this subroutine computes the mean single scattering albedo
#     for nminer components.  see subroutine mrefl for definitions.
 

	include "../../src.specpr/common/spmaxes"   # max parameters, must be first

        include "defs.h"
        include "../../src.specpr/common/lundefs"


      integer*4 currch,aflag
      integer*4 i, nminer

      real*4 d(NMINL), weight(NMINL)
      real*4 ws(NMINL),dens(NMINL),wband(SPMAXCHAN,NMINL)
      real*4 wsmean, dd, denom

 
      wsmean = 0.0
      denom = 0.0
 
#       write (6,*) "DEBUG: Qsmnh aflag=", aflag
#       write (6,*) "DEBUG: Qsmnh nminer=", nminer

      do i = aflag, nminer {
 
          dd = dens(i)*d(i)
          denom = denom + weight(i)/dd
 
          wsmean = wsmean + weight(i)*ws(i)/dd

#	  write (6,*) ' DEBUG: Qsmnh i=',i,' dens(i)=',dens(i),' d(i)=',d(i),' dd=',dd
#	  write (6,*) ' DEBUG: Qsmnh       denom=',denom
#	  write (6,*) ' DEBUG: Qsmnh       weight(i)=',weight(i),' ws(i)=',ws(i),' wsmean=',wsmean
 
          if (currch != 0) {
               wband(currch,i) = ws(i)
          }

      }
 
      wsmean = wsmean/denom
 
      return 
      end
