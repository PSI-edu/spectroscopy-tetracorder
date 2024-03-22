C
C________________________________________________________________
C
C     SUBROUTINE R S P 2
C________________________________________________________________
C
C SUBROUTINE RSP2 IS A TEST SUBROUTINE THAT CALCULATES THE
C REFLECTIVITIES AND THE TANGENTS OF THEIR ASSOCIATED PHASE
C ANGLES FOR BOTH PERPENDICULAR (S) AND PARALLEL (P) INCIDENT
C RAYS, USING HAPKE'S EQUATIONS ON PG 59, WHICH ARE THE BASIC
C EQUATIONS FROM WHICH THE EQUATIONS ON PG 60 (SEE RSP1) WERE
C DERIVED.
C
C

      subroutine rsp2(theta ,n1r,n1i,n2r,n2i  ,rs,rp,tanps,tanpp)
C
C     INPUT ARGUMENTS
      real*8 theta
      real*8 n1r,n1i,n2r,n2i
C
C     OUTPUT ARGUMENTS
      real*8 rs,rp,tanps,tanpp
C
C     PI CONVERSION FACTORS
      real*8 PIE,D2R,R2D
      parameter( PIE = 3.1415 92653 58979 32384 62643d0 )
      parameter( D2R = PIE/180.d0 , R2D = 180.d0/PIE)
C
C
C     LOCAL DECLARATIONS
      real*8 nr,ni
      real*8 sint ,cost
      real*8 sintp,costp
C
C     COMPLEX EQUIVALENTS
      complex*16 n1,n2,nc
      complex*16 rsc,rpc
C
C
C
C FIND NR AND NI = REAL AND IMAGINARY PARTS OF N = N2/N1 
C
C     CALCULATE NR AND NI USING REAL ARITHMETIC
      nr=(n1r*n2r+n1i*n2i)/(n1r*n1r+n1i*n1i)
      ni=(n1r*n2i-n2r*n1i)/(n1r*n1r+n1i*n1i)
C
C     CALCULATE NR AND NI IMPLICITLY USING COMPLEX ARITHMETIC
      n1=dcmplx(n1r,n1i)
      n2=dcmplx(n2r,n2i)
      nc=n2/n1
C
C     COMPARE NR & NI TO COMPLEX EQUIVALENT
c     if(nr.eq.0.d0.or.dabs(dreal(nc)-nr)/nr.gt.1.d-14)
c    & call errfor(0,'(rsp2) dreal(nc) calc has problems')
      if(nr.eq.0.d0.or.dabs(dreal(nc)-nr)/nr.gt.1.d-14)
     & write (*,*) '0','(rsp2) dreal(nc) calc has problems'


      if(dabs(ni).gt.1.d-14) then
        if(dabs(dimag(nc)-ni)/ni.gt.1.d-14)
     &   write (*,*) '0','(rsp2) dimag(nc) calc has problems'
c    &   call errfor(0,'(rsp2) dimag(nc) calc has problems')
      else if(dabs(dimag(nc)).gt.1.d-14) then
        write (*,*) '0','(rsp2) dimag(nc) should be 0, but is not'
c       call errfor(0,'(rsp2) dimag(nc) should be 0, but is not')
      endif
C
C
C FIND G1 AND G2
C
      sint=dsin(theta)
      cost=dcos(theta)
C
C     TYPE 1
C     (THIS METHOD IS ACTUALLY SNELL'S LAW, FOR REAL OR COMPLEX)
      sintp=n1r*sint/n2r
cC
cC     TYPE 2
cC     (THIS METHOD IS CLOSEST TO HAPKE PG 60, BUT STILL NOT
cC     EXACTLY WHAT HAPKE GOT)
c      sintp=dreal(n1/n2)*sint
cC
cC     TYPE 3
c      sintp=(1.d0/dreal(n2/n1))*sint
cC
cC     TYPE 4
c      sintp=dreal(cdsqrt(n1/n2))*sint/dreal(cdsqrt(n2/n1))

      if(sintp.gt. 1.d0) sintp= 1.d0
      if(sintp.lt.-1.d0) sintp=-1.d0
      costp=dsqrt(1.d0-sintp*sintp)
C
C
C FIND RS,TANPS ,RP,TANPP
C
C     PERPENDICULAR
      rsc=(cost-nc*costp)/(cost+nc*costp)
      rs=dreal(rsc*dconjg(rsc))
      tanps=dimag(rsc)/dreal(rsc)
C
C     PARALLEL
      rpc=(nc*cost-costp)/(nc*cost+costp)
      rp=dreal(rpc*dconjg(rpc))
      tanpp=dimag(rpc)/dreal(rpc)
C
C
C
C EXIT PROCEDURE
C
  990 return
      end
