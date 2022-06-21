#cccVARIABLES:
#ccc            i4buff       - Integer*4 array used in 3d data cube
#ccc                         reading routines and contains band
#ccc                         depth values on output (equiv'd to
#ccc                         chbuff)
#ccc            i4buf2       - Integer*4 array used in 3d data cube
#ccc                         reading routines and contains quality
#ccc                         of fit values for output (equiv'd to
#ccc                         chbuf2)
#ccc            fcb          - File control block array (I*4) used
#ccc                         in generating remapp headers (equi-
#ccc                         valenced to cfcb)
#ccc            work2        - Working array (equivalenced to
#ccc                         chdata) passed to cdiskio3.  Returns
#ccc                         header file name.
#ccc            i2sht        - Integer*2 array for holding 3d data
#ccc                         sheets (equivalenced to chidata)
#ccc            fname        - File name (char) variable returned
#ccc                         from namdev (dev -> name) routine
#ccc            bdhdr        - Band depth header file name (char)
#ccc            bdfile       - Band depth file name (char)
#ccc            reflib       - Reference spectrum from specpr lib,
#ccc                         equivalenced to datsc1 (R*4)
#ccc            refcrm       - Reference spectrum with continuum
#ccc                         removed, equiv'd to datsc2 (R*4)
#ccc            wavlen       - Wavelengths array, equiv'd to wdata
#ccc                         (R*4)
#ccc            obscrm       - Observed (from 3d data file) con-
#ccc                         tinuum removed, eqv'd to datab (R*4)
#ccc            bdarr        - Band depth array, equiv'd to datsc3
#ccc                         (R*4)
#ccc            rfid         - Reflectance file id (u,v,w or y)
#ccc                         (I*4)
#ccc            rrecno       - Reflectance file record number (I*4)
#ccc            rflun        - Reflectance file unit number (I*4)
#ccc            wfid         - Wavelength file id (use upper case)
#ccc                         (I*4)
#ccc            wrecno       - Wavelength file record number (I*4)
#ccc            wflun        - Wavelength file unit number (I*4)
#ccc            f3did        - 3d data file id (I*4)
#ccc            qlun         - 3d data file unit number (I*4)
#ccc            minch        - Channel (of band) with minimum value
#ccc                         (I*4)
#ccc            dx,y,z       - Dimensions of 3d data file (I*4)
#ccc            dx2          - x-dimension divided by 2 (indicates
#ccc                         the x coordinate of pixel to print
#ccc                         diagnostic info on) (I*4)
#ccc            flag         - Indicates extraction direction (not
#ccc                         relevant in this application) (I*4)
#ccc            ier/ioerr    - Error flag variables (I*4)
#ccc            bdrecl       - Band depth file record length (I*4)
#ccc            icrst        - Update restart file routine code
#ccc                         (I*4)
#ccc            bndstb,e     - Band start ranges (continuum begin.)
#ccc                         (I*4)
#ccc            bndenb,e     - Band end ranges (continuum end)
#ccc                         (I*4)
#ccc            emtogl       - Error message toggle (bandmp arg)
#ccc                         (I*4)
#ccc            imgflg       - Image file flag for bandmp (should=1)
#ccc                         (I*4)
#ccc            reclen       - 3d data file record length (I*4)
#ccc            rechdr       - 3d data file record header length
#ccc            nrecshdr     - 3d data file header length (in records) (I*4)
#ccc            diaflg       - Diagnostics print flag (I*4)
#ccc            nth          - Diagnostics printed every nth line
#ccc                         (I*4)
#ccc            filorg       - File organization (BIL, BSQ, BIP)
#ccc                         (I*4)
#ccc            i2pack       - Packing routine (written in C)
#ccc                         (I*4)
#ccc            bdnlen       - Band depth file name length (bytes)
#ccc                         (I*4)
#ccc            paksiz       - Number of bits to pack number to
#ccc                         (I*4)
#ccc            outfil       - Band depth output file unit number
#ccc                         (I*4)
#ccc            key          - Record number of 3d data file (obs)
#ccc                         (I*4)
#ccc            idnoff       - DN offset (y=mx+b, where b=idnoff)
#ccc                         (I*4)
#ccc            itmp*        - Temporary variables (I*4)
#ccc            iptdrop      - Point drop value (I*4)
#ccc            oper         - Operation value for cdiskio3 (I*4)
#ccc            indx         - Index for determining # of chars
#ccc                         in a string (I*4)
#ccc            thrshmin   minimum threshold value, if < this, delete.
#ccc            thrshmax   maximum threshold value, if > this, delete.
#ccc            
#ccc            
#ccc            
#ccc            
#ccc            
#ccc            
#ccc---------------------------------------------------------------

	common /ctry1/	i2sht
	common /ctry1/ 	chbuff,chbuf2,chbuf3
	common /ctry1/  specdat          #extracted spectral sheet, BIP format
	common /ctry1/ 	chdata,fname,bdfile,errfil,rcfile
	common /ctry1/ 	cbdfcb,cerfcb,crcfcb

#########################################################################
#######
#######
#######   NOTE:  this file, tricube.h must come after multmap.h
#######
#######
#######
#######
####### NOTE: if you change imaxch or maxpix in multmap.h you must
#######                    also edit read2sheet.r and  tricube.h
#######                    if you are reading cubes
#######                    i2sht and chibuff sizes must match
#######                    i4buf array size = maxpix
### NOTES: if you change:
###
###            maxpix  parameter                   in multmap.h
###            imaxch  parameter                   in multmap.h
###
###    then you must also change:
###
###            i4buf*  arrays should = maxpix      in tricube.h
###            chbuf*  #chars should = maxpix*4    in tricube.h
###            i2sht   array  should = maxpix      in tricube.h
###            chibuff #chars should = maxpix*2    in tricube.h
###
###            chibuff #chars should = maxpix*2    in read2sheet.r
###            i2sht   array   should = maxpix     in read2sheet.r
###            specdat array  should = maxpix      in read2sheet.r

	integer*4	i4buff(maxpix),i4buf2(maxpix),i4buf3(maxpix),i4buf0(maxpix)

######## select one of the following:
#########################################################################
	integer*2	i2sht(maxpi2,imaxch)
#	integer*2	i2sht(1,4852)

	integer*4	bdfcb(40),erfcb(40),rcfcb(40)
	integer*4	work2(maxpix)
#########################################################################

        ### NOTE: if maxpi4 in multmap.h is increased above 131060, below needs to be
        ###       increased to match

	character*131060  	chbuff,chbuf2,chbuf3,chbuf0
	character*131060  	chibuff(imaxch), chdata

#	character*4 		chibuff(4852)

#########################################################################
	real*4          specdat(imaxch,maxpix) #extracted spectral sheet, BIP format
#	real*4          specdat(4852,1)   #extracted spectral sheet, BIP format


##### changes above here for any change in pixels or channels in spectrum
##########################################################################

	character 	fname*8,bdfile*20,errfil*20,rcfile*20
	character 	cbdfcb*160,cerfcb*160,crcfcb*160

	equivalence	(work2,chdata)
	equivalence	(bdfcb,cbdfcb)
	equivalence	(erfcb,cerfcb)
	equivalence	(rcfcb,crcfcb)
 	equivalence 	(i2sht,chibuff)
 	equivalence 	(i4buff,chbuff)
 	equivalence 	(i4buf2,chbuf2)
 	equivalence 	(i4buf3,chbuf3)
 	equivalence 	(i4buf0,chbuf0)

	real*4		refcrm(4864)
 	equivalence 	(refcrm,datsc2)

	real*4		obscrm(4864)
	equivalence 	(obscrm,datab)

#######	real*4		work(4864)
#######	real*4		reflib(4864)
#######	real*4		obscrm(4864),wavlen(4864),bdarr(4864)
#######	equivalence 	(reflib,datsc1)
#######	equivalence 	(bdarr,datsc3)
#######	equivalence 	(work,datsc4)
#######	equivalence 	(wavlen,wdata)

# RED 02/12/2008
# Removed i from ...,itmp1,i,itmp2,... below	
#ccc            
#ccc            xel,yel,zel = x, y, z pixel coordinates of the pixel from the cube being analyzed
#ccc            
#ccc            
	common /ctry2/ 	rfid,wfid,f3did,rrecno,wrecno,rflun,wflun,qlun,minch
	common /ctry2/	maxch, ifeattype
	common /ctry2/ 	dx,dx2,dy,dz,flag,ier,ioerr,bdrecl,icrst,bndstb,emtogl
	common /ctry2/	xel,yel,zel,outfil,nrec,bndste,bndenb,bndene,imgflg
	common /ctry2/	reclen,rechdr,diaflg,nth,filorg,pack4,bdnlen,paksiz
	common /ctry2/	nrecshdr
	common /ctry2/	oreclen
	common /ctry2/ 	bdepth,kfactr,minqerr,qfit,scale,reclin,rcscal
	common /ctry2/	rflctn,maxrc,minrc,intcpt,slope,yinter
	common /ctry2/	key,idnoff,itmp,itmp1,itmp2,itmp3,iptdrop,oper,indx
	common /ctry2/	indx2,lastsl,filhdr,cdiskio3,dot
	common /ctry2/	idlt
	common /ctry2/ 	thrshmin,thrshmax

	integer*4 	rfid,wfid,f3did,rrecno,wrecno,rflun,wflun,qlun,minch
	integer*4	maxch, ifeattype
	integer*4 	dx,dx2,dy,dz,flag,ier,ioerr,bdrecl,icrst,bndstb,emtogl
	integer*4	xel,yel,zel,outfil,nrec,bndste,bndenb,bndene,imgflg
	integer*4	reclen,rechdr,diaflg,nth,filorg,pack4,bdnlen,paksiz
	integer*4	nrecshdr
	integer*4	oreclen # output file record length (=reclen/2)
	real*4 		bdepth,kfactr,minqerr,qfit,scale,reclin,rcscal
	real*4		rflctn,maxrc,minrc,intcpt,slope,yinter
	integer*4	key,idnoff,itmp,itmp1,itmp2,itmp3,iptdrop,oper,indx
	integer*4	indx2,lastsl,filhdr,cdiskio3,dot
	integer*4	idlt(4864) # deleted points array
	real*4		thrshmin,thrshmax

