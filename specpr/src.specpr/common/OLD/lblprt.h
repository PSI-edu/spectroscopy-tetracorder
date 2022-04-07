#     /lblprt/ common:
#        device protection:  .ge. 0 = protected ( read .le. protection write
#                                     to protection + 1 ).
#                            .eq. 0 = no protection.
#                            .le. 0 = read only ( up to absolute value of
#                                     protection ).
#
      common /lblprt/ iprtu,iprty,iprtd,iprtv,iprtw,iprts
      integer*4 iprtu,iprty,iprtd,iprtv,iprtw,iprts
