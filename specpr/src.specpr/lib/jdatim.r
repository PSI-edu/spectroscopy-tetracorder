	subroutine jdatim(jda,isec)
#ccc  name: jdatim
#ccc  version date: 
#ccc  author(s): Wendy Calvin
#ccc  language:  Ratfor
#ccc
#ccc  short description: get time from system and convert to julian
#ccc                     day and UT.
#ccc
#ccc  algorithm description:
#ccc  system requirements:
#ccc  subroutines called: time
#ccc  argument list description: jda = output = Julian day * 10
#ccc                             isec = UT time in seconds
#ccc  parameter description:
#ccc  common description: none
#ccc  message files referenced: none
#ccc  internal variables:
#ccc  file description: none
#ccc  user command lines:
#ccc  update information:
#ccc  NOTES:
#ccc	sets date as Julian day * 10: and returns # of secs since
#ccc						0:00 hours UT
#ccc

	integer*4 jda, time, isec, nday, jsec

# unix system time gives time in seconds since 1/1/1970

	jda = 24405875		#Julian day * 10 for 1-1-1970

# original is following.  linux is following line resolve?
#	jsec = time(0)		#returns time in secs since 1-1-1970
	jsec = time()		#returns time in secs since 1-1-1970

	nday = jsec/(3600*24)
	isec = jsec - (nday*3600*24)

	jda = jda + nday*10


	return
	end
