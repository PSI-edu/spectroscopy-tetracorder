      function i4pack4(ibuff,npix,numsiz)
      integer*4 i4pack4
#
#  this function packs 32 bits/pixel files to 8 or 16 bits/pixel
#
#  Nov. 2008   K. Eric Livo
#  16-bit fixed;  Jan. 21, 2011 - Livo
#
#     ibuff is 32bit (I*4) data
#     npix = number of pixels
#     numsiz = 8 or 16 (bit)
#
#     isys = system; 0=IEEE (MSB), 1=INTEL (LSB)
#
#     8bit, IEEE:isys1=0;  INTEL:isys1=3
#     16 bit, IEEE:isys1=-1, isys2=0
#             INTEL:isys1=-3, isys2=-2
#
      logical*1 ibuff(1)
      integer*4 npix, numsiz
      integer*4 i, j, isys, isys1, isys2, n16pix

#  set system to IEEE or INTEL
#HPUX      isys = 0
#IA64HPUX      isys = 0
#LINUX      isys = 1

#  8 bit packs
      if (numsiz == 8) {
         isys1 = isys*3

         do i=1, npix {
            ibuff(i)=ibuff(i*4 - isys1)
         }
      }

#  16 bit packs
      if (numsiz == 16) {
         isys1 = 1 +(isys * 2)
         isys2 = (isys * 2)
         i=1

         do j=1, npix {
            ibuff(i)=ibuff(j*4 - isys1)
            ibuff(i+1)=ibuff(j*4 - isys2)
            i=i+2
         }
      }

      i4pack4 = 0
      return
      end
