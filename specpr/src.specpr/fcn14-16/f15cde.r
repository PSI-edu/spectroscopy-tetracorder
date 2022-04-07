	subroutine f15cde(alpcde,k)
	implicit integer*4 (i-n)

	character*6 alpcde
#
#     *** determine alpcde ***
      if (k==1) alpcde = 'st. g:'
      if (k==2) alpcde = 'nd. g:'
      if (k==3) alpcde = 'rd. g:'
      if (k>=4) alpcde = 'th. g:'

      return
#     *** alpcde is ready ***
#
#
      end
