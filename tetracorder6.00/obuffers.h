#
# obuffers.h MUST come after multmap.h beacuse it uses parameters
#            declared in multmap.h:  maxmat and maxpix

	integer*4	bufferlines
	parameter	(bufferlines=6)

	integer*4 olinb(maxmat)      # output buffer line.

	integer*4 ofitbuff   (maxmat, bufferlines, maxpix)
	integer*4 odepthbuff (maxmat, bufferlines, maxpix)
	integer*4 ofdbuff    (maxmat, bufferlines, maxpix)

					# the following set the begin and end 
					# of a material whose output is all
					# zero for that line.
	integer*4 izeroline(maxmat)     #   first line of all zero data.
	integer*4 nzeroline(maxmat)     #   last  line of all zero data.
				# zeroline used to not output to file
				# untill there is some real data.


	common /olinebuf/ olinb, ofitbuff, odepthbuff, ofdbuff
	common /olinebuf/ izeroline,nzeroline


