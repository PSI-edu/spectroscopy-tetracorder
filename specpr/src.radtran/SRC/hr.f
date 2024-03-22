c      This function finds the value of the Chandrasekhar
c      H-function by table interpolation.
c
c      The table used is Table XI page 125 of:
c      Chandasekhar, S. "Radiative Transfer", Dover Publications
c      New York, 1960.
c
c
c      code by Roger N. Clark  April, 1984.
c      revised by Marcia L. Nelson to work with less continuation lines
c
c
c      time to execute
c             on VAX 750 w/FPA = 156.   microsec (VMS 3.5, R. Clark, 1985)
c             on HP9000/827    =   3.78 microsec (HPUX 9.0, compiled with
c                                                +O3 +OP4 optizimation:
c                                                     (R. Clark 4/16/93)
c
       subroutine hr (mu, w, wx, Hn)
       implicit none
c
c      input values:
c           mu= cosine of the angle on incidence or emission
c           w = single scattering albedo.
c           wx= (1.0-w)**0.5
c
c      internal variables
c           tmu = the values of mu in the table
c           tw  = the values of w in the table
c           twx = the values of wx in the table
c           imu1= table index of tmu where tmu(imu1) < mu
c           imu2=   "     "   "   "    "   tmu(imu2) > mu
c           iw1 =   "     "   "   w    "   w(iw1) < w
c           iw2 =   "     "   "   "    "   w(iw2) > w
c              note: these previous table indices are to
c                    find the table values which bound w and mu
c           h1 = h value interpolated to mu along w(iw1)
c           h2 = h value interpolated to mu alonh w(iw2)
c
c      the table contains 15 values of w from 0.0 to 1.0
c                     and 21 values of mu from 0.0 to 1.0
c      The data values are entered as 4 seperate arrays, due to
c      limitations on the number of continuation lines allowed for
c      one statement.  The seperate arrays are "assigned" to the value
c      array using an equivalence statement.      
c
       real*4 tw(15), tmu(21), value(15,21), value1(15,6)
       real*4 value2(15,6), value3(15,6), value4(15,3), twx(15)
       real*4 mu, w, wx, Hn
       real*4 h1, h2, hx

       integer*4 imu1, imu2, iw1, iw2

CCCCC       double precision mu, w

       equivalence (value(1,1), value1), (value(1,7), value2),
     1          (value(1,13), value3), (value(1,19),value4)
c
       data tw/0.0,0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,
     1         0.85,0.90,0.925,0.950,0.975,1.00/
c
       data twx / 1.000000,.948683,.894427,.836660,.774597,
     1      .707107,.632456,.547723,.447214,.387298,.316228,
     2      .273861,.223607,.158114,.000000 /
c
       data tmu/0.0,0.05,0.10,0.15,0.20,0.25,0.30,0.35,0.40,
     1         0.45,0.50,0.55,0.60,0.65,0.70,0.75,0.80,0.85,
     2         0.90,0.95,1.00/
c
       data value1/1.0,1.00000,1.00000,1.00000,1.00000,
     1       1.00000,1.00000,1.00000,1.00000,1.00000,1.00000,
     2       1.00000,1.00000,1.00000,1.00000,
c mu=0.05
     3   1.0,1.00783,1.01608,1.02484,1.03422,1.04439,
     4       1.05544,1.06780,1.0820 ,1.0903 ,1.0999 ,
     5       1.1053 ,1.1117 ,1.1196 ,1.1368 ,
c mu=0.10
     6   1.0,1.01238,1.02562,1.03989,1.05535,1.07241,
     7       1.09137,1.11306,1.1388 ,1.1541 ,1.1722 ,
     8       1.1828 ,1.1952 ,1.2111 ,1.2474 ,
c mu=0.15
     9   1.0,1.01584,1.03295,1.05155,1.07196,1.09474,
     x       1.12045,1.15036,1.1886 ,1.2086 ,1.2349 ,
     1       1.2506 ,1.2693 ,1.2936 ,1.3508 ,
c mu=0.20
     2   1.0,1.01864,1.03893,1.06115,1.08577,1.11349,
     3       1.14517,1.18253,1.2286 ,1.2570 ,1.2914 ,
     4       1.3123 ,1.3373 ,1.3703 ,1.4503 ,
c mu=0.25
     5   1.0,1.02099,1.04396,1.06930,1.09758,1.12968,
     6       1.16674,1.21095,1.2663 ,1.3009 ,1.3433 ,
     7       1.3692 ,1.4008 ,1.4427 ,1.5473 /
c mu=0.30
         data value2/1.0,1.02300,1.04829,1.07637,1.10789,1.14391,
     1       1.18587,1.23643,1.3006 ,1.3411 ,1.3914 ,
     2       1.4224 ,1.4604 ,1.5117 ,1.6425 ,
c mu=0.35
     3   1.0,1.02475,1.05209,1.08259,1.11700,1.15659,
     4       1.20304,1.25951,1.3320 ,1.3783 ,1.4363 ,
     5       1.4724 ,1.5170 ,1.5778 ,1.7364 ,
c mu=0.40
     6   1.0,1.02630,1.05546,1.08811,1.12516,1.16800,
     7       1.21861,1.28063,1.3611 ,1.4129 ,1.4785 ,
     8       1.5197 ,1.5709 ,1.6414 ,1.8293 ,
c mu=0.45
     9   1.0,1.02768,1.05847,1.09308,1.13251,1.17833,
     x       1.23280,1.30003,1.3881 ,1.4453 ,1.5183 ,
     1       1.5646 ,1.6224 ,1.7027 ,1.9213 ,
c mu=0.50
     2   1.0,1.02892,1.06117,1.09756,1.13918,1.18776,
     3       1.24581,1.31796,1.4132 ,1.4758 ,1.5560 ,
     4       1.6073 ,1.6718 ,1.7621 ,2.0128 ,
c mu=0.55
     5   1.0,1.03004,1.06363,1.10164,1.14528,1.19640,
     6       1.25781,1.33459,1.4368 ,1.5044 ,1.5918 ,
     7       1.6480 ,1.7191 ,1.8195 ,2.1037 /
c mu=0.60
        data value3/1.0,1.03106,1.06587,1.10538,1.15087,1.20436,
     1       1.26893,1.35009,1.4590 ,1.5315 ,1.6259 ,
     2       1.6869 ,1.7647 ,1.8753 ,2.1941 ,
c mu=0.65
     3   1.0,1.03199,1.06793,1.10881,1.15602,1.21173,
     4       1.27925,1.36457,1.4798 ,1.5571 ,1.6583 ,
     5       1.7242 ,1.8086 ,1.9295 ,2.2842 ,
c mu=0.70
     6   1.0,1.03284,1.06982,1.11198,1.16080,1.21858,
     7       1.28888,1.37815,1.4995 ,1.5814 ,1.6893 ,
     8       1.7600 ,1.8509 ,1.9822 ,2.3740 ,
c mu=0.75
     9   1.0,1.03363,1.07157,1.11491,1.16523,1.22495,
     x       1.29788,1.39090,1.5182 ,1.6045 ,1.7190 ,
     1       1.7943 ,1.8918 ,2.0334 ,2.4635 ,
c mu=0.80
     2   1.0,1.03436,1.07319,1.11763,1.16935,1.23091,
     3       1.30631,1.40291,1.5358 ,1.6265 ,1.7474 ,
     4       1.8274 ,1.9313 ,2.0833 ,2.5527 ,
c mu=0.85
     5   1.0,1.03504,1.07469,1.12017,1.17320,1.23648,
     6       1.31424,1.41425,1.5526 ,1.6475 ,1.7746 ,
     7       1.8592 ,1.9695 ,2.1320 ,2.6417 /
c mu=0.90
        data value4/1.0,1.03567,1.07610,1.12254,1.17681,
     1       1.24171,1.32171,1.42497,1.5685 ,1.6675 ,1.8008 ,
     2       1.8898 ,2.0065 ,2.1795 ,2.7306 ,
c mu=0.95
     3   1.0,1.03626,1.07741,1.12476,1.18019,1.24664,
     4       1.32875,1.43512,1.5837 ,1.6867 ,1.8259 ,
     5       1.9194 ,2.0423 ,2.2258 ,2.8193 ,
c mu=1.00
     6   1.0,1.03682,1.07864,1.12685,1.18337,1.25128,
     7       1.33541,1.44476,1.5982 ,1.7050 ,1.8501 ,
     8       1.9479 ,2.0771 ,2.2710 ,2.9078 /
c
c      The table has finer resulotion in w when w > 0.80,
c      and even finer when w > 0.90,
c      thus algorithm must take this into account.
c
c**********************************************************************
c      find upper and lower mu indices.
c
c       write (6, 4) mu, w, wx
c4      format (' DEBUG hr: mu, w, wx=', f10.5, f10.5, f10.5)

       imu1 = mu/0.05 +1
       imu2 = imu1+1
       if(imu2.gt.21) imu2=21
c
c      find lower and upper w indices
c
       if (w.lt.0.80) then
           iw1 = w/0.1 + 1
        else if (w.lt.0.90) then
           iw1 = (w-0.8)/0.05 + 9
        else
           iw1 = (w-0.9)/0.025 + 11
       end if

c       write(6,5) iw1, iw2
c5      format (' DEBUG hr: iw1, iw2=', i9, i9)
c
       iw2 = iw1 + 1
       if (iw2.gt.15) iw2=15
c
c      first interpolate h along the two mu values
c
       hx = (mu-tmu(imu1))/0.05
       h1 = hx*(value(iw1,imu2) -
     1       value(iw1,imu1)) + value(iw1,imu1)
c
       h2 = hx*(value(iw2,imu2) - 
     1        value(iw2,imu1)) + value(iw2,imu1)
c
c      now linearly interpolate along wx direction
c
       if (w.lt.1.0) then
           Hn = ((wx-twx(iw1))/(twx(iw2)-twx(iw1))) * (h2-h1) +h1
         else
           Hn = h1
       end if
c---------- test --------
c       write (6,20) mu,w,imu1,imu2,iw1,iw2,hx,h1,h2,
c     1      value(iw1,imu1),value(iw2,imu1),value(iw2,imu1),
c     2      value(iw2,imu2)
c20     format (1x,'DEBUG hr: ',f5.3,1x,f8.5,4(i4),1x,7(f8.4,1x))
c----- end test -----------
c
       return
       end



