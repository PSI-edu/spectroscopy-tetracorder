C     THIS IS THE WEDGE TAPE FORMATTING PROGRAM. FILE=(RNC,WJFMT)
C********************************************************************
C     TI 980 B VERSION BY ROGER CLARK AND KARL HINCK                *
C     LATEST REVISION 11/21/80                                      *
C********************************************************************
      COMMON/LABEL1/ITL(20),ICTA(3),ICTB(3),ISTA(3),ISTB(3),IDTA(3)
      COMMON/LABEL1/IDTB(3),IRVS,IFLN,IRA(3),IDEC(3),IRMAS,IFUT(7)
      COMMON/LABEL1/ITIMCH,IHIST(30),MHIST(148),NRUNS,IEROS,IWTRNS
      COMMON/LABEL1/XNRM,SCATIM,TIMINT,XFUT(7),ADATA(256)
      COMMON /IB/ IB(256),M
      INTEGER CTA(3),CTB(3),STA(3),STB(3),DATEA(3),DATEB(3)
      INTEGER REVS,FILNO,HIST(23)
      INTEGER MTLUN,TLUN,PLUN
       COMMON /DATA/ ITITL(16),CTA,CTB,STA,STB,DATEA,DATEB,DATA(120)
       COMMON /DATA/ REVS,FILNO,RA,DEC,AIRMAS,HIST
       DIMENSION ITX(32)
      DATA IHC,IHE,IH /1HC,1HE,1H /
      DATA IHP,IHT,IHR/1HP,1HT,1HR/
      DATA IH2B/2H  /
      DATA IHY,MTLUN1/1HY,1/
      DATA IHV,IHI,IHS/1HV,1HI,1HS/
       DATA MTLUN,TLUN,PLUN/6,4,5/
       DATA IHL/1HL/
       DATA  IH1,IH2,IH3,IH4,IH5/1H1,1H2,1H3,1H4,1H5/
C********************************************************************
C     PROCEDURE FOR COMPILING, LINKING, AND EXECUTING WJFMT         *
C    TO COMPILE:                                                    *
C       RUN FTNUP DMSG=ME  FSRC=(RNC,WJFMT) DLST=LP FUPD=(RNC,WJLPF)*
C                                                                   *
C    TO LINK:                                                       *
C       RUN DXOLE FOB1=(RNC,WJLPF) DLST=ME  FLM=(RNC,WEJLM) RLM     *
C          LLM=(6,0,32)                                             *
C       CONTROL FILE:    (ENTER AFTER RUN DXOLE STATEMENT)          *
C           ROOT MAIN                                               *
C           INCLUDE 1                                               *
C           INCLUDE 2(IO)                                           *
C          /*                                                       *
C    TO RUN:                                                        *
C       RUN WEJFMT DRAW=       DFORM=       DLP=DUMMY               *
C********************************************************************
      KEY=-1
      ICNT=512
      CALL IO(MTLUN,24583,ICNT,IB,KEY,IER)
      IF (IER.NE.0) WRITE (TLUN,400) MTLUN,IER
      ICNT=1536
      CALL IO(MTLUN1,24583,ICNT,ITL,KEY,IER)
      IF (IER.NE.0) WRITE (TLUN,400) MTLUN1,IER
400   FORMAT (1X, '***OPEN ERROR NUMBER', I4, ' DEVICE',I3,'***',/)
C
101   WRITE(TLUN,2)
2     FORMAT(1X,79(1H*),/,10(1H*),'WEDGE DUMP AND TAPE TRANSFER '
     1,'PROGRAM',34(1H*)/)
      WRITE(TLUN,3)
3     FORMAT(5X,'TYPE  P  TO DUMP THE WEDGE FORMAT DATA TAPES ON THE '
     2,'LINEPRINTER',/,5X,'TYPE  T  TO TRANSFER THE WEDGE FORMAT ',
     2'TO STANDARD FORMAT ',/,5X,'TYPE  R  TO REWIND TAPES',
     3 5X, 'TYPE  E  TO EXIT PROGRAM',/)
      READ(TLUN,4)ICON
4     FORMAT(A1)
C      IF (ICON.EQ.IHP) GO TO 1
      IF (ICON.EQ.IHT) GO TO 200
      IF (ICON.EQ.IHR) GO TO 300
      IF (ICON.EQ.IHE) GO TO 2000
      GO TO 101
200   WRITE(TLUN,201)
201   FORMAT (5X,'*** TAPE TRANSFER ROUTINE ***',/,1X,'THE RAW WEDGE',
     1 ' FORMAT DATA TAPE MUST BE ON DEVICE=DRAW AND',/,1X,'THE ',
     2 'STANDARD SPECPR FORMAT TAPE MUST BE ON DEVICE=DFORM',//,2X,
     3 'IF THIS IS CORRECT, TYPE "Y".',/)
      READ(TLUN,4) ICON
      IF (ICON.NE.IHY) GO TO 101
      IFJ=0
C
209   WRITE (TLUN,210)
210   FORMAT (1X,'TYPE IN THE WEDGE DATA SYSTEM VERSION TYPE:',/,
     1 1X,'TYPE:  1  OLD WEDGE SYSTEM.  (BEFORE DEC. 1979)',/,11X,
     2 '10 SECONDS PER REV., 16MS PER HALF CHOP.',/,8X,'2  WEDGE',
     3 ' SYSTEM AFTER DEC. 1979',/11X,'20 SECONDS PER REV, 32 MS',
     4 ' PER HALF CHOP.',/,8X,'3  WEDGE SYSTEM AFTER NOV. 1, 1980',/,
     5 11X,'80 SECONDS PER REV, 128 MS PER HALF CHOP',/,8X,
     6 '4  COMBINATIONS 1 AND 3 ABOVE, 1 WHEN "VIS" IS FOUND IN TITLE',
     7 /,1X,'DEFAULT = #3',/)
C
      READ (TLUN,4) ICON
      IF (ICON.EQ.IH ) ICON=IH3
      IF ((ICON.LT.IH1).OR.(ICON.GT.IH4)) GO TO 209
      ICONTL = ICON
C
C
      IF (ICON.EQ.IH1) GO TO 211
      IF (ICON.EQ.IH2) GO TO 212
      IF (ICON.EQ.IH3) GO TO 213
      IF (ICON.EQ.IH4) GO TO 213
211   JTIMCH=16
      ASCATM=10.0
      GO TO 220
212   JTIMCH=32
      ASCATM=20.0
      GO TO 220
213   JTIMCH = 128
      ASCATM = 80.0
C
C
C*********** BEGIN DO LOOP *****************************************
C
220   DO 290 J=1,2000
      DO 242 I= 17,20
      ITL(I)=IH2B
242   CONTINUE
      DO 245 I= 24,30
      IHIST(I)=IH2B
245   CONTINUE
      DO 346 I= 1,148
      MHIST(I)=IH2B
346   CONTINUE
      IRFLG= 0
230   CALL REDFIL(MTLUN,IER,ICNT)
      IF (IER.EQ.32) IRFLG=IRFLG+1
      IF((IER.EQ.32).AND.(IRFLG.LE.1)) GO TO 230
      IF ((IER.NE.0).OR.(ICNT.LT.511)) WRITE (TLUN,229) J,ICNT
229   FORMAT (1X,'FILE',I5,':',I6,' CHARACTERS READ')
      ICNTX=512
      IF (ICNT.EQ.511) ICNTX=511
      CALL ERORED (ICNT,IER,ICNTX)
      IF(IER.NE.0) GO TO 235
      GO TO 240
235   WRITE (TLUN, 331)
331   FORMAT (1X,//, '*TAPE READ ERROR ON FILE:', I4)
      WRITE (TLUN,1004) ITITL,CTA,CTB,STA,STB,DATEA,DATEB,REVS,FILNO
      WRITE (TLUN,1005) RA,DEC,AIRMAS,HIST
      WRITE (-1,700)
700   FORMAT ('*WJ ERR*')
      READ (-1,701) (ITL(I), I=17,20)
701   FORMAT (4A2)
      WRITE (-1,702)
702   FORMAT ('TRANSFER ERROR')
      READ (-1,703) (IHIST(I),I=24,30)
703   FORMAT (7A2)
      WRITE (-1,704)
704   FORMAT ('WEDGE FORMATTING PROGRAM RAW TAPE READ ERROR,  THE DATA'
     1  , ' MAY BE BAD', 20(1H ))
      READ (-1,705) (MHIST(I), I=1,37)
705   FORMAT (37A2)
      WRITE (TLUN, 332)
332   FORMAT (1X, 'PRESS RETURN TO CONTINUE, L TO LIST DATA',
     1        /,1X,' OR E TO EXIT',/)
      READ (TLUN,4) ICON
      IF (ICON.EQ.IHE) GO TO 295
      IF (ICON.NE.IHL) GO TO 240
      LC=0
      LB=-4
      DO 335 LCH=1, 24
      LB=LB+ 5
      LC=LC+ 5
      WRITE (TLUN,333) (LA, DATA(LA),LA=LB,LC)
333   FORMAT ( 5(I4, E10.3, 1X))
335   CONTINUE
      READ (TLUN,4) ICON
      WRITE(TLUN,236)IER
236   FORMAT(1X,'**ERROR #'I4,/,1X,'TYPE  E  TO STOP, OR PRESS RETURN'
     1, ' TO CONTINUE OR  S  TO SKIP AND CONTINUE',/)
      READ(TLUN,4) ICON
      IF (ICON.EQ.IHE) GO TO 295
      IF ((IER.NE.0).AND.(ICON.EQ.IHS)) GO TO 285
      IF (IER.EQ.7) GO TO 285
240   IERR=0
      WRITE (TLUN,341) J, ITITL
341   FORMAT (6X, 'TRANSFERING FILE', I4, 8X, 16A2)
      DO 241 I=1,16
      ITL(I)=ITITL(I)
241   CONTINUE
      DO 243 I=1,18
      ICTA(I)=CTA(I)
243   CONTINUE
      ITIMCH= JTIMCH
      SCATIM= ASCATM
      IRVS=REVS
      IFLN=J
      IFJ=IFJ+1
      IF(FILNO.EQ.0) IFILN=IFJ
      CALL DECRA(RA,IRA(1),IRA(2),IRA(3))
      CALL DECRA(DEC,IDEC(1),IDEC(2),IDEC(3))
      IRMAS=AIRMAS/10.
      DO 244 I=1,23
      IHIST(I)=HIST(I)
244   CONTINUE
      WRITE (-1,2441) HIST(23)
      READ  (-1,2442) IHIST(23)
2441  FORMAT (A1, 1X)
2442  FORMAT (A2)
C
C
      IF (ICONTL.NE.IH4) GO TO 2448
      WRITE (-1,705) (ITL(I), I=1,16)
      READ (-1,706) ITX
706   FORMAT (32A1)
      ICHEK = 0
      DO 2444 I = 1,30
      IF ((ITX(I).EQ.IHV).AND.(ITX(I+1).EQ.IHI).AND.(ITX(I+2).EQ.IHS))
     1     ICHEK=1
      IF (ICHEK.EQ.1) GO TO 2445
2444  CONTINUE
      IF (ICHEK.NE.1) GO TO 2448
2445  ITIMCH = 16
      SCATIM = 10.0
      WRITE (TLUN,2446)
2446  FORMAT (1H+,'VIS1')
C------------------------------------------------
2448  NRUNS=1
      IEROS=0
      IWTRNS=1
      TIMINT=SCATIM*IRVS
      XNRM=1.0
      DO 246 I=1,7
      XFUT(I)=0.0
      IFUT(I)=0
246   CONTINUE
      DO 247 I=1,120
      ADATA(I)=DATA(I)*1.0
247   CONTINUE
      DO 248 I=121,256
      ADATA(I)=0.0
248   CONTINUE
      ICNT=1536
      IERR=0
      KEY=-1
249   FORMAT (1X,'FILE',I5,':',I6, ' CHARACTERS WRITTEN')
      CALL IO(MTLUN1,3,ICNT,ITL,KEY,IERR)
      IF ((IERR.NE.0).OR.(ICNT.LT.1536)) WRITE (TLUN,249) IFILN,ICNT
      CALL ERORED (ICNT,IERR,1536)
      IF(IERR.EQ.0) GO TO 290
      WRITE(TLUN,255) IERR
255   FORMAT(1X,'**WRITE ERROR #',I4,/)
      GO TO 290
285   WRITE (-1, 286)
286   FORMAT (' DATA LOST IN TRANSFER', 20(1H ))
      READ (-1,287) ITITL
287   FORMAT (23A2)
      WRITE (-1,286)
      READ (-1,287) HIST
      GO TO 240
290   CONTINUE
      GO TO 101
295   ICNT=1536
      KEY=-1
      CALL IO (MTLUN1,11,ICNT,ITL,KEY,IER)
      CALL ERORED (ICNT,IER,1536)
300   KEY=-1

C     REWIND TAPES

      IERR=0
      ICNT=512
      CALL IO (MTLUN,4,ICNT,IB,KEY,IERR)
      KEY=-1
      ICNT=1536
      CALL IO (MTLUN1,4,ICNT,ITL,KEY,IERR)
      GO TO 101
      WRITE (TLUN,1001)
1001   FORMAT (' ENTER NUMBER OF FILES TO DUMP',/)
      READ  (TLUN,1002) NFILES
1002   FORMAT (I8)
      IF (NFILES .LT. 1) GO TO 101
      DO 100 I=1,NFILES
      IRFLG= 0
      CALL REDFIL (MTLUN,IERR,ICNT)
      IF (IERR.EQ.32) IRFLG=IRFLG+1
C      IF ((IERR.EQ.32).AND.(IRFLG.LE.1)) GO TO 9
      IF (IERR) 10,20,10
10     WRITE (PLUN,21) IERR,I
      WRITE (TLUN,21) IERR, I
21     FORMAT (1X, '**TAPE READ ERROR #', I3, ' ON FILE', I5)
1003   FORMAT (' ERROR #',I3)
      GO TO 100
20    WRITE (PLUN,22) I
      WRITE (TLUN,22) I
22    FORMAT (//,1X, '  FILE #', I5, ' READ OK')
      WRITE (PLUN,1004) ITITL,CTA,CTB,STA,STB,DATEA,DATEB,REVS,FILNO
1004  FORMAT (/,1X,'TITLE=',16A2,/,1X,'CTA=',2(A2,':'),A2,3X,
     C   'CTB=',2(A2,':'),A2, 3X,'STA=',
     1   2(A2,':'),A2,3X,'STB=',2(A2,':'),A2,/,1X,'DATEA=',2(A2,'/'),
     2   A2,3X,'DATEB=',2(A2,'/'),A2,3X,'REVS=',I5,5X,'FILE=',I4)
      CALL DECRA (RA, IRA(1),IRA(2),IRA(3))
      CALL DECRA (DEC, IDEC(1),IDEC(2),IDEC(3))
      WRITE (PLUN, 1007) IRA, IDEC
1007  FORMAT (1X,'RA=',I3,'H',I3,'M',I3,'S',3X,'DEC=',I3,'D',I3,'M',I3,
     1   'S' )
      WRITE (PLUN,1005) RA,DEC,AIRMAS,HIST
1005  FORMAT (1X,'RA=',F7.0, 5X,'DEC='F7.0, 5X,'AIRMAS=',F7.0,/, 1X,
     1 'HISTORY=',  22A2, A1, /, 1X, 119(1H-))
       LC=0
       LB=-4
       DO 1050 LCH=1, 24
       LB=LB+5
       LC=LC+5
       WRITE (PLUN,1006) (LA, DATA(LA), LA=LB,LC)
1006  FORMAT (3X, 5(I4, 1X,E13.6, 5X))
1050  CONTINUE
100    CONTINUE
C       GO TO 1
2000   STOP
       END

      SUBROUTINE DECRA(X,ID,IM,IS)
C
C     THIS SUBROUTINE CONVERTS THE VALUE X IN  ARC-SECONDS
C     (OR TIME SECONDS) TO DEGREES, MINUTES, SECONDS
C     (OR HOURS, MINUTES, SECONDS).
C
      ID=INT(X/3600.)
      Y=(X/3600.-ID)*60.
      IM=INT(Y)
      Z=(Y-IM)*60.
      IS=INT(Z+0.5)
      IM=IABS(IM)
      IS=IABS(IS)
      RETURN
      END

      SUBROUTINE ERORED (ICNT,IER,I)
      IW=4
      IC=I-ICNT
      IF ((IER.EQ.0).AND.(IC.LE.1)) RETURN
      WRITE (IW,10) IER
      IF (IER.NE.0)GO TO 100
10    FORMAT (1X, '*** FILE OPERATION ERROR',I5, ' ***')
      WRITE (IW,11) IC
11    FORMAT (1X, '***', I5, ' CHARACTERS NOT TRANSFERED ***',/)
100   IF (IER.EQ.1) WRITE (IW,12)
12    FORMAT (1X, '***READ/WRITE ROUTINE TERMINATION REQUESTED ***',/)
      IF (IER.EQ.4) WRITE (IW, 13)
13    FORMAT (1X, '*** BEGINING OF FILE MEDIUM ENCOUNTERED ***',/)
      IF (IER.EQ.4) IER=0
      IF (IER.EQ.0) RETURN
      IF (IER.EQ.2) WRITE (IW,14)
14    FORMAT (1X, '*** LOGICAL ERROR ***',/)
      IF (IER.EQ.8) WRITE (IW,15)
15    FORMAT (1X, '*** END OF FILE MEDIUM ENCOUNTERED ***',/)
      IF (IER.EQ.16) WRITE (IW,16)
16    FORMAT (1X, '*** FILE OPERATION IGNORED ***',/)
      IF (IER.EQ.32) WRITE (IW,32) IC
      IF (IER.EQ.32) GO TO 200
32    FORMAT (1X, '*** EOF ENCOUNTERED,',I5,' CHARACTERS NOT TRANSFERED',
     1   ' ***', /)
      IF (IER.EQ.64) WRITE (IW,64) IC
64    FORMAT (1X, '*** ERROR NUMBER', I5, ' ***',/)
      IF (IER.EQ.1) IER=4
      IF (IER.EQ.16) IER=1
      IF ((IER.EQ.1).OR.(IER.EQ.4)) RETURN
200   IER=3
      RETURN
      END

       SUBROUTINE GNIB(I)
       COMMON /IB/IB(256),M
C***************************************************************
C*     THIS SUBROUTINE GETS NIBBLE M FROM BUFFER IB AND   *
C*     PUT IT INTO I.                                     **
C**********************************************************
       MM=M
       N=M/4+1
       IBN=IB(N)
CA      LDA   MM
CA      AND   3,M
CA      LDE   3,M
CA      REO   E,A
CA      LLA   2
CA     @IOR   >C840,M
CA      STA   &1
CA      LDA   IBN
CA1     DATA  0
CA      AND   15,M
CA      STA   MM
       I=MM
       M=M+1
       RETURN
       END

       SUBROUTINE REDFIL(LUN,IERR,ICNT)
       COMMON /IB/ IB(256),M
       INTEGER CTA(3),CTB(3),STA(3),STB(3),DATEA(3),DATEB(3)
       INTEGER REVS,FILNO,HIST(23)
       COMMON /DATA/ ITITL(16),CTA,CTB,STA,STB,DATEA,DATEB,DATA(120)
       COMMON /DATA/ REVS,FILNO,RA,DEC,AIRMAS,HIST
       DOUBLE PRECISION DTEMP
       DIMENSION IEQ(3)
       EQUIVALENCE (DTEMP,IEQ(1),IT),(IEQ(2),IU),(IEQ(3),IV)
       DATA IH2/ 1H2/
      KEY=-1
      IERR=0
      ICNT=512
      CALL IO(LUN,1,ICNT,IB,KEY,IERR)
       IF (IERR.EQ.32) RETURN
       M = 0
       IN = 0
4      DO 5 I = 1,28
       ISW = IN
       CALL GNIB(IN)
       IF (IN .EQ. 15) GO TO 7
5      CONTINUE
6      IERR = 7
       RETURN
7      CALL GNIB(IN)
       IF (IN .NE. 15) GO TO 5
       CALL GNIB (IBEST)
       CALL GNIB (IN)
       IBEST = IBEST*16 + IN
C**********************************************************
C*   IBEST = ONE LESS THAN THE NUMBER OF BITS TO THE LEFT *
C*           OF THE BINARY POINT.                         *
C*   ISW = 9 FOR A COMPUTER GENERATED TAPE, 0 OTHERWISE   *
C**********************************************************
       IF (ISW .NE. 9) IBEST=23
      IF (ISW.EQ.9) GO TO 32
C
C     SET RA,DEC, AND AIRMAS TO 0.0 FOR RAW DATA TAPE
C     PUT  'RAW DATA' IN HISTORY.
C
      RA= 0
      DEC=0
      AIRMAS=0
      WRITE (-1,23)
23    FORMAT (3(1H ), 'RAW DATA', 35(1H ))
      READ (-1,24) HIST
24    FORMAT (23A2)
      M=M+18
      GO TO 41
C
C     DECODE RA, DEC, AIRMAS, HISTORY.
C
32    CALL ITOFP (RA,151)
      CALL ITOFP (DEC,151)
      CALL ITOFP (AIRMAS,151)
      DO 33 I=1,23
      IT=IB(I+9)
CA      LDA   IT
CA     @IOR   >8080,M
CA      STA   IT
33    HIST(I)=IT
41    M= M+ 156
       DO 10 I = 1,16
       IT= IB(I+32)
CA      LDA   IT
CA     @IOR   >8080,M
CA      STA   IT
10     ITITL(I) = IT
C********************************
C*   READ IN DATES AND TIMES    *
C********************************
       DO 13 I = 1,6
       DO 12 J = 1,3
       DO 11 K = 1,2
       CALL GNIB(IN)
       INN=IN
CA      LDA   IT
CA      LLA   8
CA      IOR   INN
CA      IOR   -80,M
CA      STA   IT
11     CONTINUE
       K = I*3 + J - 3
12     CTA(K) = IT
13     M = M+2
C******************************************
C*   READ IN THE NUMBER OF REVOLUTIONS    *
C******************************************
       REVS=0
       DO 14 I = 1,4
       CALL GNIB(IN)
14     REVS = REVS*10 + IN
       M = M + 4
C***************************
C*   READ IN FILE NUMBER   *
C***************************
       FILNO=0
       DO 16 I = 1,3
       CALL GNIB(IN)
16     FILNO = FILNO*10 + IN
      IF(ISW.NE.9)FILNO=0
       M = M + 5
C*****************************
C*   READ IN THE DATA VALUES *
C*****************************
       IBEST = IBEST + 128
       DO 19 I = 1,120
       CALL ITOFP (DATA(I), IBEST)
19     CONTINUE
	   RETURN
       END

       SUBROUTINE ITOFP (DATA,IBEST)
       DIMENSION IEQ(3)
       DOUBLE PRECISION DTEMP
       EQUIVALENCE (DTEMP,IEQ(1),IT),(IEQ(2),IU),(IEQ(3),IV)
       DO 18 J = 1,4
       CALL GNIB(INN)
       IN=INN
CA      LDA   IT
CA      LLA   4
CA      IOR   IN
CA      STA   IT
18     CONTINUE
       CALL GNIB(IUU)
       IU=IUU
       CALL GNIB(INN)
       IN=INN
CA      LDA   IU
CA      LLA   4
CA      IOR   IN
CA      LLA   7
CA      STA   IU
CA      DLD   IT
CA      SZE   A
CA      BRU   &183
CA      SNZ   E
CA      BRU   &184
CA183   SMI   A
CA      BRU   &181
CA     @LDM   >8000,M
CA      ROR   M,E
CA      BRU   &181
184    DTEMP= 0.D0
       GO TO 185
CA181   DST   IT
185    IV = IBEST
       DTEMP = DTEMP + 0.D0
       DATA = DTEMP
       RETURN
       END
