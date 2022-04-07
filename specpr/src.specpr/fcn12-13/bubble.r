	subroutine bubble(x,isort,npts)
	implicit integer*4 (i-n)

#ccc  version date: 06/01/83
#ccc  author(s): Roger Clark & Jeff Hoover
#ccc  language:  Ratfor
#ccc
#ccc  short description:
#ccc                   This subroutine does a bubble sort on a given
#ccc                   array, x, having npts number of points. returns
#ccc                   an array of sorted pointers.
#ccc  algorithm description:
#ccc                   algorithms + data structures = programs
#ccc                   by niklaus wirth
#ccc                   copyright 1976 by prentice hall inc
#ccc                   chapter 2, 'sorting', page 60.
#ccc  system requirements: none
#ccc  subroutines called:
#ccc                    none
#ccc  argument list description:
#ccc      arguments: x,isort,npts
#ccc  parameter description:
#ccc  common description:
#ccc  message files referenced:
#ccc  internal variables:
#ccc  file description:
#ccc  user command lines:
#ccc  update information:
#ccc  NOTES:
#ccc

#     **********************************************************
#     *
#     * routine does a bubble sort on a given array, x, having
#     * npts number of points. returns an array of sorted
#     * pointers.
#     * bubble sort algorithm from:
#     *    algorithms + data structures = programs
#     *    by niklaus wirth
#     *    copyright 1976 by prentice-hall, inc.
#     *    chapter 2, 'sorting', page 60
#     *
#     **********************************************************

	include "../common/spmaxes"   # max parameters, must be first

	real*4    x(SPMAXCHAN)
	integer*4 isort(SPMAXCHAN)

#     *** initialize array pointers ***
	do i = 1,npts
		isort(i) = i


#     *** sort ***
	do i1 = 2,npts {
		j = npts
		do i2 = i1,npts {
			if (x(isort(j-1)) > x(isort(j))) {
				i = isort(j-1)
				isort(j-1) = isort(j)
				isort(j) = i
			}
			j = j - 1
		}
	}
	return
	end
