	subroutine glist
	implicit integer*4 (i-n)

	write(6,10)
	write(6,20)
	write(6,30)
	write(6,40)
	write(6,50)
	write(6,60)
	write(6,70)
	write(6,80)
	write(6,90)
	write(6,100)

10      format(1x,'Commands:')
20      format(/,1x,'1. Change Parameters')
30      format(8x,'ep - estimate parameters',3x,'fp - fit parameters',/,
	       8x,'eg - estimate gaussians ',3x,'fg - fit gaussians')
40      format(1x,'2. Plotting ')
50      format(8x,'pe - plot estimates     ',3x,'pf - plot fit')
60      format(1x,'3. Fit',
	     /,8x,'fi - fit')
70      format(1x,'4. Transfer Data',/,
	       8x,'es - estimate to srcatch',3x,'se - scratch to est.',/,
	       8x,'fs - fit to scratch     ',3x,'sf - scratch to fit',/,
	       8x,'fe - fit to estimate    ',3x,'xe - exch est. and scr.',/,
	       8x,'xf - exch fit and scr.  ')
80      format(1x,'5. Save, Recall Gfit Parameters',/,
	       8x,'sa - save all parameters',3x,'re - recall estimates',/,
	       8x,'rf - recall fit         ',3x,'ra - recall all param.')
90      format(1x,'6. Write to SPECPR Files',/,
	       8x,'wr - write to specpr files')
100     format(1x,'7. Misc.',/,
	       8x,'li - list commands      ',3x,'e  - exit from Gfit',/,
	       8x,'lf - print titles       ',3x,'pr - print fit param')
	return
	end
