      subroutine tocent(down,across,dncm,accm,isyst)
#
# converts coordinates to centimeters
#
	integer*4 isyst
	
	include "../common/plot02"
	
	if (isyst<=0) {
		isyst=-isyst
		dncm=down
		accm=across
	} else {
		dncm=(down-dmin)*dscale
		accm=(across-amin)*ascale
	}
	return
	end
