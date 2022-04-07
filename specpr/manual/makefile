EQN=neqn
ROFF=nroff
FLAGS=-ms
LROFF=lroffI1
LFLAGS=
PRINTER=/usr/local/securebin/laser3ne -os

CHAP= \
chap01 	\
chap02 	\
chap03 	\
chap04 	\
chap05  \
chap06 	\
chap07 	\
chap08 	\
chap08.f1	\
chap08.f2	\
chap08.f3	\
chap08.f4	\
chap08.f5	\
chap08.f6	\
chap08.f7	\
chap08.f8	\
chap08.f9	\
chap08.f10	\
chap08.f11	\
chap08.f12	\
chap08.f13	\
chap08.f14	\
chap08.f15	\
chap08.f16	\
chap08.f17	\
chap08.f18	\
chap08.f19	\
chap08.f20	\
chap08.f21	\
chap08.f22	\
chap08.f23	\
chap08.f24	\
chap08.f25	\
chap08.f39	\
chap08.f40	\
chap08.f41	\
chap08.f42	\
chap08.f43	\
chap08.f44	\
chap08.f45	\
chap08.f46	\
chap08.f47	\
chap08.f48	\
chap08.f49	\
chap08.f50	\
chap09 	\
chap10 	\
chap11 	\
chap12  \
chap13 	\
chap14 

FMT= \
chap01.f	\
chap02.f	\
chap03.f	\
chap04.f	\
chap05.f 	\
chap06.f	\
chap07.f	\
chap08.f	\
chap08.f1.f	\
chap08.f2.f	\
chap08.f3.f	\
chap08.f4.f	\
chap08.f5.f	\
chap08.f6.f	\
chap08.f7.f	\
chap08.f8.f	\
chap08.f9.f	\
chap08.f10.f	\
chap08.f11.f	\
chap08.f12.f	\
chap08.f13.f	\
chap08.f14.f	\
chap08.f15.f	\
chap08.f16.f	\
chap08.f17.f	\
chap08.f18.f	\
chap08.f19.f	\
chap08.f20.f	\
chap08.f21.f	\
chap08.f22.f	\
chap08.f23.f	\
chap08.f24.f	\
chap08.f25.f	\
chap08.f39.f	\
chap08.f40.f	\
chap08.f41.f	\
chap08.f42.f	\
chap08.f43.f	\
chap08.f44.f	\
chap08.f45.f	\
chap08.f46.f	\
chap08.f47.f	\
chap08.f48.f	\
chap08.f49.f	\
chap08.f50.f	\
chap09.f	\
chap10.f	\
chap11.f	\
chap12.f  	\
chap13.f	\
chap14.f

SRC=	\
contents.n	\
titlepage.n	\
chap01.n	\
chap02.n	\
chap03.n	\
chap04.n	\
chap05.n	\
chap06.n	\
chap07.n	\
chap08.n	\
chap08.f1.n	\
chap08.f2.n	\
chap08.f3.n	\
chap08.f4.n	\
chap08.f5.n	\
chap08.f6.n	\
chap08.f7.n	\
chap08.f8.n	\
chap08.f9.n	\
chap08.f10.n	\
chap08.f11.n	\
chap08.f12.n	\
chap08.f13.n	\
chap08.f14.n	\
chap08.f15.n	\
chap08.f16.n	\
chap08.f17.n	\
chap08.f18.n	\
chap08.f19.n	\
chap08.f20.n	\
chap08.f21.n	\
chap08.f22.n	\
chap08.f23.n	\
chap08.f24.n	\
chap08.f25.n	\
chap08.f39.n	\
chap08.f40.n	\
chap08.f41.n	\
chap08.f42.n	\
chap08.f43.n	\
chap08.f44.n	\
chap08.f45.n	\
chap08.f46.n	\
chap08.f47.n	\
chap08.f48.n	\
chap08.f49.n	\
chap08.f50.n	\
chap09.n	\
chap10.n	\
chap11.n	\
chap12.n	\
chap13.n	\
chap14.n	\
chap15.n

HELP= \
HELP/1	\
HELP/2	\
HELP/3	\
HELP/4	\
HELP/5	\
HELP/6	\
HELP/7	\
HELP/8	\
HELP/9	\
HELP/10	\
HELP/11	\
HELP/12	\
HELP/13	\
HELP/14	\
HELP/15	\
HELP/bugs	\
HELP/cmd	\
HELP/f1		\
HELP/f2		\
HELP/f3		\
HELP/f4		\
HELP/f5		\
HELP/f6		\
HELP/f7		\
HELP/f8		\
HELP/f9		\
HELP/f10	\
HELP/f11	\
HELP/f12	\
HELP/f13	\
HELP/f14	\
HELP/f15	\
HELP/f16	\
HELP/f17	\
HELP/f18	\
HELP/f19	\
HELP/f20	\
HELP/f21	\
HELP/f22	\
HELP/f23	\
HELP/f24	\
HELP/f25	\
HELP/f39	\
HELP/f40	\
HELP/f41	\
HELP/f42	\
HELP/f43	\
HELP/f44	\
HELP/f45	\
HELP/f46	\
HELP/f47	\
HELP/f48	\
HELP/f49	\
HELP/f50	\
HELP/locks	\
HELP/protection

.SUFFIXES:
.SUFFIXES:	.f .n

#.n.f:
#	tbl common.nr $*.n | $(EQN) | $(ROFF) $(FLAGS) >$*.f
#
#vax:	$(FMT) titlepage.f
#

help:	HELP/help.done

printed:	$(SRC)
	makeroff $(SRC)

#titlepage.f:	titlepage.n
#	$(ROFF) $(FLAGS) titlepage.n  >titlepage.f

HELP/help.done:		$(HELP)
	cat /dev/null > HELP/help.done

HELP/1:		chap01.n
	sed	-e 's/^\.nr LL 6.3i/.nr LL 7.56i/' \
		-e 's/^\.ll 6.3i/.ll 7.56i/' \
		-e 's/^\.nr PO 1.0i/.nr PO 0.0i/' \
		-e 's/^\.po 1.0i/.po 0.0i/' \
		chap01.n | \
	nroff -ms -o2-99 > HELP/1

HELP/2:		chap02.n
	sed	-e 's/^\.nr LL 6.3i/.nr LL 7.56i/' \
		-e 's/^\.ll 6.3i/.ll 7.56i/' \
		-e 's/^\.nr PO 1.0i/.nr PO 0.0i/' \
		-e 's/^\.po 1.0i/.po 0.0i/' \
		chap02.n | \
	nroff -ms -o2-99 > HELP/2

HELP/3:		chap03.n
	sed	-e 's/^\.nr LL 6.3i/.nr LL 7.56i/' \
		-e 's/^\.ll 6.3i/.ll 7.56i/' \
		-e 's/^\.nr PO 1.0i/.nr PO 0.0i/' \
		-e 's/^\.po 1.0i/.po 0.0i/' \
		chap03.n | \
	nroff -ms -o2-99 > HELP/3

HELP/4:		chap04.n
	sed	-e 's/^\.nr LL 6.3i/.nr LL 7.56i/' \
		-e 's/^\.ll 6.3i/.ll 7.56i/' \
		-e 's/^\.nr PO 1.0i/.nr PO 0.0i/' \
		-e 's/^\.po 1.0i/.po 0.0i/' \
		chap04.n | \
	nroff -ms -o2-99 > HELP/4

HELP/5:		chap05.n
	sed	-e 's/^\.nr LL 6.3i/.nr LL 7.56i/' \
		-e 's/^\.ll 6.3i/.ll 7.56i/' \
		-e 's/^\.nr PO 1.0i/.nr PO 0.0i/' \
		-e 's/^\.po 1.0i/.po 0.0i/' \
		chap05.n | \
	nroff -ms -o2-99 > HELP/5

HELP/6:		chap06.n
	sed	-e 's/^\.nr LL 6.3i/.nr LL 7.56i/' \
		-e 's/^\.ll 6.3i/.ll 7.56i/' \
		-e 's/^\.nr PO 1.0i/.nr PO 0.0i/' \
		-e 's/^\.po 1.0i/.po 0.0i/' \
		chap06.n | \
	nroff -ms -o2-99 > HELP/6

HELP/7:		chap07.n
	sed	-e 's/^\.nr LL 6.3i/.nr LL 7.56i/' \
		-e 's/^\.ll 6.3i/.ll 7.56i/' \
		-e 's/^\.nr PO 1.0i/.nr PO 0.0i/' \
		-e 's/^\.po 1.0i/.po 0.0i/' \
		chap07.n | \
	nroff -ms -o2-99 > HELP/7

HELP/8:		chap08.n
	sed	-e 's/^\.nr LL 6.3i/.nr LL 7.56i/' \
		-e 's/^\.ll 6.3i/.ll 7.56i/' \
		-e 's/^\.nr PO 1.0i/.nr PO 0.0i/' \
		-e 's/^\.po 1.0i/.po 0.0i/' \
		chap08.n | \
	nroff -ms -o2-99 > HELP/8

HELP/bugs:	chap04.n
	sed	-e 's/^\.nr LL 6.3i/.nr LL 7.56i/' \
		-e 's/^\.ll 6.3i/.ll 7.56i/' \
		-e 's/^\.nr PO 1.0i/.nr PO 0.0i/' \
		-e 's/^\.po 1.0i/.po 0.0i/' \
		-e '/^.\\" setup complete/ s//\
.\\" setup complete\
.ds LF "ZZYYXX\
.ds LH "XXYYZZ/'	\
		chap04.n  | \
	nroff -ms -o2-99  | \
	sed -n -e \
		'/^..4\...4  ..B..u..g ..R/,/^..4\...5  ..K..e..e..p ..T/p' | \
	grep -v '^..4\...5  ..K..e..e..p ..T' | \
	sed -e '/^ZZYYXX/,/^XXYYZZ/d' > HELP/bugs

HELP/cmd:	chap02.n
	sed	-e 's/^\.nr LL 6.3i/.nr LL 7.56i/' \
		-e 's/^\.ll 6.3i/.ll 7.56i/' \
		-e 's/^\.nr PO 1.0i/.nr PO 0.0i/' \
		-e 's/^\.po 1.0i/.po 0.0i/' \
		-e '/^.\\" setup complete/ s//\
.\\" setup complete\
.ds LF "ZZYYXX\
.ds LH "XXYYZZ/'	\
		chap02.n  | \
	nroff -ms -o2-99  | \
	sed -n -e \
		'/^..2\...4  ..C..o..m..m/,/^..2...5  ..A..l..i..a..s ..D/p' | \
	grep -v '^..2...5  ..A..l..i..a..s ..D' | \
	sed -e '/^ZZYYXX/,/^XXYYZZ/d' > HELP/cmd

HELP/locks:	chap04.n
	sed	-e 's/^\.nr LL 6.3i/.nr LL 7.56i/' \
		-e 's/^\.ll 6.3i/.ll 7.56i/' \
		-e 's/^\.nr PO 1.0i/.nr PO 0.0i/' \
		-e 's/^\.po 1.0i/.po 0.0i/' \
		-e '/^.\\" setup complete/ s//\
.\\" setup complete\
.ds LF "ZZYYXX\
.ds LH "XXYYZZ/'	\
		chap04.n  | \
	nroff -ms -o2-99  | \
	sed -n -e \
		'/^..4\...2  ..P..r..o..t/,/^..4\...3  ..M..a..k..e ..S/p' | \
	grep -v '^..4\...3  ..M..a..k..e ..S' | \
	sed -e '/^ZZYYXX/,/^XXYYZZ/d' > HELP/locks

HELP/protection:	chap02.n
	sed	-e 's/^\.nr LL 6.3i/.nr LL 7.56i/' \
		-e 's/^\.ll 6.3i/.ll 7.56i/' \
		-e 's/^\.nr PO 1.0i/.nr PO 0.0i/' \
		-e 's/^\.po 1.0i/.po 0.0i/' \
		-e '/^.\\" setup complete/ s//\
.\\" setup complete\
.ds LF "ZZYYXX\
.ds LH "XXYYZZ/'	\
		chap02.n  | \
	nroff -ms -o2-99  | \
	sed -n -e \
		'/^..2\...3  ..F..i..l..e/,/^..2\...4  ..C..o..m..m..a/p' | \
	grep -v '^..2\...4  ..C..o..m..m..a' | \
	sed -e '/^ZZYYXX/,/^XXYYZZ/d' > HELP/protection

HELP/f1:	chap08.f1.n
	sed	-e 's/^\.nr LL 6.3i/.nr LL 7.56i/' \
		-e 's/^\.ll 6.3i/.ll 7.56i/' \
		-e 's/^\.nr PO 1.0i/.nr PO 0.0i/' \
		-e 's/^\.po 1.0i/.po 0.0i/' \
		-e '/^.\\" setup complete/ s//\
.\\" setup complete\
.ds LF "ZZYYXX\
.ds LH "XXYYZZ/'	\
		chap08.f1.n  | \
	nroff -ms -o1-99  | \
	sed -e '/^ZZYYXX/,/^XXYYZZ/d' > HELP/f1

HELP/f2:	chap08.f2.n
	sed	-e 's/^\.nr LL 6.3i/.nr LL 7.56i/' \
		-e 's/^\.ll 6.3i/.ll 7.56i/' \
		-e 's/^\.nr PO 1.0i/.nr PO 0.0i/' \
		-e 's/^\.po 1.0i/.po 0.0i/' \
		-e '/^.\\" setup complete/ s//\
.\\" setup complete\
.ds LF "ZZYYXX\
.ds LH "XXYYZZ/'	\
		chap08.f2.n  | \
	nroff -ms -o1-99  | \
	sed -e '/^ZZYYXX/,/^XXYYZZ/d' > HELP/f2

HELP/f3:	chap08.f3.n
	sed	-e 's/^\.nr LL 6.3i/.nr LL 7.56i/' \
		-e 's/^\.ll 6.3i/.ll 7.56i/' \
		-e 's/^\.nr PO 1.0i/.nr PO 0.0i/' \
		-e 's/^\.po 1.0i/.po 0.0i/' \
		-e '/^.\\" setup complete/ s//\
.\\" setup complete\
.ds LF "ZZYYXX\
.ds LH "XXYYZZ/'	\
		chap08.f3.n  | \
	nroff -ms -o1-99  | \
	sed -e '/^ZZYYXX/,/^XXYYZZ/d' > HELP/f3

HELP/f4:	chap08.f4.n
	sed	-e 's/^\.nr LL 6.3i/.nr LL 7.56i/' \
		-e 's/^\.ll 6.3i/.ll 7.56i/' \
		-e 's/^\.nr PO 1.0i/.nr PO 0.0i/' \
		-e 's/^\.po 1.0i/.po 0.0i/' \
		-e '/^.\\" setup complete/ s//\
.\\" setup complete\
.ds LF "ZZYYXX\
.ds LH "XXYYZZ/'	\
		chap08.f4.n  | \
	nroff -ms -o1-99  | \
	sed -e '/^ZZYYXX/,/^XXYYZZ/d' > HELP/f4

HELP/f5:	chap08.f5.n
	sed	-e 's/^\.nr LL 6.3i/.nr LL 7.56i/' \
		-e 's/^\.ll 6.3i/.ll 7.56i/' \
		-e 's/^\.nr PO 1.0i/.nr PO 0.0i/' \
		-e 's/^\.po 1.0i/.po 0.0i/' \
		-e '/^.\\" setup complete/ s//\
.\\" setup complete\
.ds LF "ZZYYXX\
.ds LH "XXYYZZ/'	\
		chap08.f5.n  | \
	nroff -ms -o1-99  | \
	sed -e '/^ZZYYXX/,/^XXYYZZ/d' > HELP/f5

HELP/f6:	chap08.f6.n
	sed	-e 's/^\.nr LL 6.3i/.nr LL 7.56i/' \
		-e 's/^\.ll 6.3i/.ll 7.56i/' \
		-e 's/^\.nr PO 1.0i/.nr PO 0.0i/' \
		-e 's/^\.po 1.0i/.po 0.0i/' \
		-e '/^.\\" setup complete/ s//\
.\\" setup complete\
.ds LF "ZZYYXX\
.ds LH "XXYYZZ/'	\
		chap08.f6.n  | \
	nroff -ms -o1-99  | \
	sed -e '/^ZZYYXX/,/^XXYYZZ/d' > HELP/f6

HELP/f7:	chap08.f7.n
	sed	-e 's/^\.nr LL 6.3i/.nr LL 7.56i/' \
		-e 's/^\.ll 6.3i/.ll 7.56i/' \
		-e 's/^\.nr PO 1.0i/.nr PO 0.0i/' \
		-e 's/^\.po 1.0i/.po 0.0i/' \
		-e '/^.\\" setup complete/ s//\
.\\" setup complete\
.ds LF "ZZYYXX\
.ds LH "XXYYZZ/'	\
		chap08.f7.n  | \
	nroff -ms -o1-99  | \
	sed -e '/^ZZYYXX/,/^XXYYZZ/d' > HELP/f7

HELP/f8:	chap08.f8.n
	sed	-e 's/^\.nr LL 6.3i/.nr LL 7.56i/' \
		-e 's/^\.ll 6.3i/.ll 7.56i/' \
		-e 's/^\.nr PO 1.0i/.nr PO 0.0i/' \
		-e 's/^\.po 1.0i/.po 0.0i/' \
		-e '/^.\\" setup complete/ s//\
.\\" setup complete\
.ds LF "ZZYYXX\
.ds LH "XXYYZZ/'	\
		chap08.f8.n  | \
	nroff -ms -o1-99  | \
	sed -e '/^ZZYYXX/,/^XXYYZZ/d' > HELP/f8

HELP/f9:	chap08.f9.n
	sed	-e 's/^\.nr LL 6.3i/.nr LL 7.56i/' \
		-e 's/^\.ll 6.3i/.ll 7.56i/' \
		-e 's/^\.nr PO 1.0i/.nr PO 0.0i/' \
		-e 's/^\.po 1.0i/.po 0.0i/' \
		-e '/^.\\" setup complete/ s//\
.\\" setup complete\
.ds LF "ZZYYXX\
.ds LH "XXYYZZ/'	\
		chap08.f9.n  | \
	nroff -ms -o1-99  | \
	sed -e '/^ZZYYXX/,/^XXYYZZ/d' > HELP/f9

HELP/f10:	chap08.f10.n
	sed	-e 's/^\.nr LL 6.3i/.nr LL 7.56i/' \
		-e 's/^\.ll 6.3i/.ll 7.56i/' \
		-e 's/^\.nr PO 1.0i/.nr PO 0.0i/' \
		-e 's/^\.po 1.0i/.po 0.0i/' \
		-e '/^.\\" setup complete/ s//\
.\\" setup complete\
.ds LF "ZZYYXX\
.ds LH "XXYYZZ/'	\
		chap08.f10.n  | \
	nroff -ms -o1-99  | \
	sed -e '/^ZZYYXX/,/^XXYYZZ/d' > HELP/f10

HELP/f11:	chap08.f11.n
	sed	-e 's/^\.nr LL 6.3i/.nr LL 7.56i/' \
		-e 's/^\.ll 6.3i/.ll 7.56i/' \
		-e 's/^\.nr PO 1.0i/.nr PO 0.0i/' \
		-e 's/^\.po 1.0i/.po 0.0i/' \
		-e '/^.\\" setup complete/ s//\
.\\" setup complete\
.ds LF "ZZYYXX\
.ds LH "XXYYZZ/'	\
		chap08.f11.n  | \
	nroff -ms -o1-99  | \
	sed -e '/^ZZYYXX/,/^XXYYZZ/d' > HELP/f11

HELP/f12:	chap08.f12.n
	sed	-e 's/^\.nr LL 6.3i/.nr LL 7.56i/' \
		-e 's/^\.ll 6.3i/.ll 7.56i/' \
		-e 's/^\.nr PO 1.0i/.nr PO 0.0i/' \
		-e 's/^\.po 1.0i/.po 0.0i/' \
		-e '/^.\\" setup complete/ s//\
.\\" setup complete\
.ds LF "ZZYYXX\
.ds LH "XXYYZZ/'	\
		chap08.f12.n  | \
	nroff -ms -o1-99  | \
	sed -e '/^ZZYYXX/,/^XXYYZZ/d' > HELP/f12

HELP/f13:	chap08.f13.n
	sed	-e 's/^\.nr LL 6.3i/.nr LL 7.56i/' \
		-e 's/^\.ll 6.3i/.ll 7.56i/' \
		-e 's/^\.nr PO 1.0i/.nr PO 0.0i/' \
		-e 's/^\.po 1.0i/.po 0.0i/' \
		-e '/^.\\" setup complete/ s//\
.\\" setup complete\
.ds LF "ZZYYXX\
.ds LH "XXYYZZ/'	\
		chap08.f13.n  | \
	nroff -ms -o1-99  | \
	sed -e '/^ZZYYXX/,/^XXYYZZ/d' > HELP/f13

HELP/f14:	chap08.f14.n
	sed	-e 's/^\.nr LL 6.3i/.nr LL 7.56i/' \
		-e 's/^\.ll 6.3i/.ll 7.56i/' \
		-e 's/^\.nr PO 1.0i/.nr PO 0.0i/' \
		-e 's/^\.po 1.0i/.po 0.0i/' \
		-e '/^.\\" setup complete/ s//\
.\\" setup complete\
.ds LF "ZZYYXX\
.ds LH "XXYYZZ/'	\
		chap08.f14.n  | \
	nroff -ms -o1-99  | \
	sed -e '/^ZZYYXX/,/^XXYYZZ/d' > HELP/f14

HELP/f15:	chap08.f15.n
	sed	-e 's/^\.nr LL 6.3i/.nr LL 7.56i/' \
		-e 's/^\.ll 6.3i/.ll 7.56i/' \
		-e 's/^\.nr PO 1.0i/.nr PO 0.0i/' \
		-e 's/^\.po 1.0i/.po 0.0i/' \
		-e '/^.\\" setup complete/ s//\
.\\" setup complete\
.ds LF "ZZYYXX\
.ds LH "XXYYZZ/'	\
		chap08.f15.n  | \
	nroff -ms -o1-99  | \
	sed -e '/^ZZYYXX/,/^XXYYZZ/d' > HELP/f15

HELP/f16:	chap08.f16.n
	sed	-e 's/^\.nr LL 6.3i/.nr LL 7.56i/' \
		-e 's/^\.ll 6.3i/.ll 7.56i/' \
		-e 's/^\.nr PO 1.0i/.nr PO 0.0i/' \
		-e 's/^\.po 1.0i/.po 0.0i/' \
		-e '/^.\\" setup complete/ s//\
.\\" setup complete\
.ds LF "ZZYYXX\
.ds LH "XXYYZZ/'	\
		chap08.f16.n  | \
	nroff -ms -o1-99  | \
	sed -e '/^ZZYYXX/,/^XXYYZZ/d' > HELP/f16

HELP/f17:	chap08.f17.n
	sed	-e 's/^\.nr LL 6.3i/.nr LL 7.56i/' \
		-e 's/^\.ll 6.3i/.ll 7.56i/' \
		-e 's/^\.nr PO 1.0i/.nr PO 0.0i/' \
		-e 's/^\.po 1.0i/.po 0.0i/' \
		-e '/^.\\" setup complete/ s//\
.\\" setup complete\
.ds LF "ZZYYXX\
.ds LH "XXYYZZ/'	\
		chap08.f17.n  | \
	nroff -ms -o1-99  | \
	sed -e '/^ZZYYXX/,/^XXYYZZ/d' > HELP/f17

HELP/f18:	chap08.f18.n
	sed	-e 's/^\.nr LL 6.3i/.nr LL 7.56i/' \
		-e 's/^\.ll 6.3i/.ll 7.56i/' \
		-e 's/^\.nr PO 1.0i/.nr PO 0.0i/' \
		-e 's/^\.po 1.0i/.po 0.0i/' \
		-e '/^.\\" setup complete/ s//\
.\\" setup complete\
.ds LF "ZZYYXX\
.ds LH "XXYYZZ/'	\
		chap08.f18.n  | \
	nroff -ms -o1-99  | \
	sed -e '/^ZZYYXX/,/^XXYYZZ/d' > HELP/f18

HELP/f19:	chap08.f19.n
	sed	-e 's/^\.nr LL 6.3i/.nr LL 7.56i/' \
		-e 's/^\.ll 6.3i/.ll 7.56i/' \
		-e 's/^\.nr PO 1.0i/.nr PO 0.0i/' \
		-e 's/^\.po 1.0i/.po 0.0i/' \
		-e '/^.\\" setup complete/ s//\
.\\" setup complete\
.ds LF "ZZYYXX\
.ds LH "XXYYZZ/'	\
		chap08.f19.n  | \
	nroff -ms -o1-99  | \
	sed -e '/^ZZYYXX/,/^XXYYZZ/d' > HELP/f19

HELP/f20:	chap08.f20.n
	sed	-e 's/^\.nr LL 6.3i/.nr LL 7.56i/' \
		-e 's/^\.ll 6.3i/.ll 7.56i/' \
		-e 's/^\.nr PO 1.0i/.nr PO 0.0i/' \
		-e 's/^\.po 1.0i/.po 0.0i/' \
		-e '/^.\\" setup complete/ s//\
.\\" setup complete\
.ds LF "ZZYYXX\
.ds LH "XXYYZZ/'	\
		chap08.f20.n  | \
	nroff -ms -o1-99  | \
	sed -e '/^ZZYYXX/,/^XXYYZZ/d' > HELP/f20

HELP/f21:	chap08.f21.n
	sed	-e 's/^\.nr LL 6.3i/.nr LL 7.56i/' \
		-e 's/^\.ll 6.3i/.ll 7.56i/' \
		-e 's/^\.nr PO 1.0i/.nr PO 0.0i/' \
		-e 's/^\.po 1.0i/.po 0.0i/' \
		-e '/^.\\" setup complete/ s//\
.\\" setup complete\
.ds LF "ZZYYXX\
.ds LH "XXYYZZ/'	\
		chap08.f21.n  | \
	nroff -ms -o1-99  | \
	sed -e '/^ZZYYXX/,/^XXYYZZ/d' > HELP/f21

HELP/f22:	chap08.f22.n
	sed	-e 's/^\.nr LL 6.3i/.nr LL 7.56i/' \
		-e 's/^\.ll 6.3i/.ll 7.56i/' \
		-e 's/^\.nr PO 1.0i/.nr PO 0.0i/' \
		-e 's/^\.po 1.0i/.po 0.0i/' \
		-e '/^.\\" setup complete/ s//\
.\\" setup complete\
.ds LF "ZZYYXX\
.ds LH "XXYYZZ/'	\
		chap08.f22.n  | \
	nroff -ms -o1-99  | \
	sed -e '/^ZZYYXX/,/^XXYYZZ/d' > HELP/f22

HELP/f23:	chap08.f23.n
	sed	-e 's/^\.nr LL 6.3i/.nr LL 7.56i/' \
		-e 's/^\.ll 6.3i/.ll 7.56i/' \
		-e 's/^\.nr PO 1.0i/.nr PO 0.0i/' \
		-e 's/^\.po 1.0i/.po 0.0i/' \
		-e '/^.\\" setup complete/ s//\
.\\" setup complete\
.ds LF "ZZYYXX\
.ds LH "XXYYZZ/'	\
		chap08.f23.n  | \
	nroff -ms -o1-99  | \
	sed -e '/^ZZYYXX/,/^XXYYZZ/d' > HELP/f23

HELP/f24:	chap08.f24.n
	sed	-e 's/^\.nr LL 6.3i/.nr LL 7.56i/' \
		-e 's/^\.ll 6.3i/.ll 7.56i/' \
		-e 's/^\.nr PO 1.0i/.nr PO 0.0i/' \
		-e 's/^\.po 1.0i/.po 0.0i/' \
		-e '/^.\\" setup complete/ s//\
.\\" setup complete\
.ds LF "ZZYYXX\
.ds LH "XXYYZZ/'	\
		chap08.f24.n  | \
	nroff -ms -o1-99  | \
	sed -e '/^ZZYYXX/,/^XXYYZZ/d' > HELP/f24

HELP/f25:	chap08.f25.n
	sed	-e 's/^\.nr LL 6.3i/.nr LL 7.56i/' \
		-e 's/^\.ll 6.3i/.ll 7.56i/' \
		-e 's/^\.nr PO 1.0i/.nr PO 0.0i/' \
		-e 's/^\.po 1.0i/.po 0.0i/' \
		-e '/^.\\" setup complete/ s//\
.\\" setup complete\
.ds LF "ZZYYXX\
.ds LH "XXYYZZ/'	\
		chap08.f25.n  | \
	nroff -ms -o1-99  | \
	sed -e '/^ZZYYXX/,/^XXYYZZ/d' > HELP/f25

HELP/f39:	chap08.f39.n
	sed	-e 's/^\.nr LL 6.3i/.nr LL 7.56i/' \
		-e 's/^\.ll 6.3i/.ll 7.56i/' \
		-e 's/^\.nr PO 1.0i/.nr PO 0.0i/' \
		-e 's/^\.po 1.0i/.po 0.0i/' \
		-e '/^.\\" setup complete/ s//\
.\\" setup complete\
.ds LF "ZZYYXX\
.ds LH "XXYYZZ/'	\
		chap08.f39.n  | \
	nroff -ms -o1-99  | \
	sed -e '/^ZZYYXX/,/^XXYYZZ/d' > HELP/f39

HELP/f40:	chap08.f40.n
	sed	-e 's/^\.nr LL 6.3i/.nr LL 7.56i/' \
		-e 's/^\.ll 6.3i/.ll 7.56i/' \
		-e 's/^\.nr PO 1.0i/.nr PO 0.0i/' \
		-e 's/^\.po 1.0i/.po 0.0i/' \
		-e '/^.\\" setup complete/ s//\
.\\" setup complete\
.ds LF "ZZYYXX\
.ds LH "XXYYZZ/'	\
		chap08.f40.n  | \
	nroff -ms -o1-99  | \
	sed -e '/^ZZYYXX/,/^XXYYZZ/d' > HELP/f40

HELP/f41:	chap08.f41.n
	sed	-e 's/^\.nr LL 6.3i/.nr LL 7.56i/' \
		-e 's/^\.ll 6.3i/.ll 7.56i/' \
		-e 's/^\.nr PO 1.0i/.nr PO 0.0i/' \
		-e 's/^\.po 1.0i/.po 0.0i/' \
		-e '/^.\\" setup complete/ s//\
.\\" setup complete\
.ds LF "ZZYYXX\
.ds LH "XXYYZZ/'	\
		chap08.f41.n  | \
	nroff -ms -o1-99  | \
	sed -e '/^ZZYYXX/,/^XXYYZZ/d' > HELP/f41

HELP/f42:	chap08.f42.n
	sed	-e 's/^\.nr LL 6.3i/.nr LL 7.56i/' \
		-e 's/^\.ll 6.3i/.ll 7.56i/' \
		-e 's/^\.nr PO 1.0i/.nr PO 0.0i/' \
		-e 's/^\.po 1.0i/.po 0.0i/' \
		-e '/^.\\" setup complete/ s//\
.\\" setup complete\
.ds LF "ZZYYXX\
.ds LH "XXYYZZ/'	\
		chap08.f42.n  | \
	nroff -ms -o1-99  | \
	sed -e '/^ZZYYXX/,/^XXYYZZ/d' > HELP/f42

HELP/f43:	chap08.f43.n
	sed	-e 's/^\.nr LL 6.3i/.nr LL 7.56i/' \
		-e 's/^\.ll 6.3i/.ll 7.56i/' \
		-e 's/^\.nr PO 1.0i/.nr PO 0.0i/' \
		-e 's/^\.po 1.0i/.po 0.0i/' \
		-e '/^.\\" setup complete/ s//\
.\\" setup complete\
.ds LF "ZZYYXX\
.ds LH "XXYYZZ/'	\
		chap08.f43.n  | \
	nroff -ms -o1-99  | \
	sed -e '/^ZZYYXX/,/^XXYYZZ/d' > HELP/f43

HELP/f44:	chap08.f44.n
	sed	-e 's/^\.nr LL 6.3i/.nr LL 7.56i/' \
		-e 's/^\.ll 6.3i/.ll 7.56i/' \
		-e 's/^\.nr PO 1.0i/.nr PO 0.0i/' \
		-e 's/^\.po 1.0i/.po 0.0i/' \
		-e '/^.\\" setup complete/ s//\
.\\" setup complete\
.ds LF "ZZYYXX\
.ds LH "XXYYZZ/'	\
		chap08.f44.n  | \
	nroff -ms -o1-99  | \
	sed -e '/^ZZYYXX/,/^XXYYZZ/d' > HELP/f44

HELP/f45:	chap08.f45.n
	sed	-e 's/^\.nr LL 6.3i/.nr LL 7.56i/' \
		-e 's/^\.ll 6.3i/.ll 7.56i/' \
		-e 's/^\.nr PO 1.0i/.nr PO 0.0i/' \
		-e 's/^\.po 1.0i/.po 0.0i/' \
		-e '/^.\\" setup complete/ s//\
.\\" setup complete\
.ds LF "ZZYYXX\
.ds LH "XXYYZZ/'	\
		chap08.f45.n  | \
	nroff -ms -o1-99  | \
	sed -e '/^ZZYYXX/,/^XXYYZZ/d' > HELP/f45

HELP/f46:	chap08.f46.n
	sed	-e 's/^\.nr LL 6.3i/.nr LL 7.56i/' \
		-e 's/^\.ll 6.3i/.ll 7.56i/' \
		-e 's/^\.nr PO 1.0i/.nr PO 0.0i/' \
		-e 's/^\.po 1.0i/.po 0.0i/' \
		-e '/^.\\" setup complete/ s//\
.\\" setup complete\
.ds LF "ZZYYXX\
.ds LH "XXYYZZ/'	\
		chap08.f46.n  | \
	nroff -ms -o1-99  | \
	sed -e '/^ZZYYXX/,/^XXYYZZ/d' > HELP/f46

HELP/f47:	chap08.f47.n
	sed	-e 's/^\.nr LL 6.3i/.nr LL 7.56i/' \
		-e 's/^\.ll 6.3i/.ll 7.56i/' \
		-e 's/^\.nr PO 1.0i/.nr PO 0.0i/' \
		-e 's/^\.po 1.0i/.po 0.0i/' \
		-e '/^.\\" setup complete/ s//\
.\\" setup complete\
.ds LF "ZZYYXX\
.ds LH "XXYYZZ/'	\
		chap08.f47.n  | \
	nroff -ms -o1-99  | \
	sed -e '/^ZZYYXX/,/^XXYYZZ/d' > HELP/f47

HELP/f48:	chap08.f48.n
	sed	-e 's/^\.nr LL 6.3i/.nr LL 7.56i/' \
		-e 's/^\.ll 6.3i/.ll 7.56i/' \
		-e 's/^\.nr PO 1.0i/.nr PO 0.0i/' \
		-e 's/^\.po 1.0i/.po 0.0i/' \
		-e '/^.\\" setup complete/ s//\
.\\" setup complete\
.ds LF "ZZYYXX\
.ds LH "XXYYZZ/'	\
		chap08.f48.n  | \
	nroff -ms -o1-99  | \
	sed -e '/^ZZYYXX/,/^XXYYZZ/d' > HELP/f48

HELP/f49:	chap08.f49.n
	sed	-e 's/^\.nr LL 6.3i/.nr LL 7.56i/' \
		-e 's/^\.ll 6.3i/.ll 7.56i/' \
		-e 's/^\.nr PO 1.0i/.nr PO 0.0i/' \
		-e 's/^\.po 1.0i/.po 0.0i/' \
		-e '/^.\\" setup complete/ s//\
.\\" setup complete\
.ds LF "ZZYYXX\
.ds LH "XXYYZZ/'	\
		chap08.f49.n  | \
	nroff -ms -o1-99  | \
	sed -e '/^ZZYYXX/,/^XXYYZZ/d' > HELP/f49

HELP/f50:	chap08.f50.n
	sed	-e 's/^\.nr LL 6.3i/.nr LL 7.56i/' \
		-e 's/^\.ll 6.3i/.ll 7.56i/' \
		-e 's/^\.nr PO 1.0i/.nr PO 0.0i/' \
		-e 's/^\.po 1.0i/.po 0.0i/' \
		-e '/^.\\" setup complete/ s//\
.\\" setup complete\
.ds LF "ZZYYXX\
.ds LH "XXYYZZ/'	\
		chap08.f50.n  | \
	nroff -ms -o1-99  | \
	sed -e '/^ZZYYXX/,/^XXYYZZ/d' > HELP/f50

