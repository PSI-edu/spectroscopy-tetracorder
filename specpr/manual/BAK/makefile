EQN=neqn
ROFF=nroff
FLAGS=-ms
LROFF=lroff
LFLAGS=
PRINTER=laser

CHAP= \
chap01 		chap02 		chap03 		chap04 		chap05  \
chap06 		chap07 		chap08.1 	chap08.2 	chap08.3  \
chap08.4.1 	chap08.4.2 	chap08.4.3 	chap08.4.4 	chap08.4.5  \
chap08.5 	chap09 		chap10 		chap11 		chap12  \
chap13 		chap14 

FMT= \
chap01.f		chap02.f		chap03.f		chap04.f		\
chap05.f 		chap06.f		chap07.f		chap08.1.f		\
chap08.2.f		chap08.3.f  	chap08.4.1.f	chap08.4.2.f	\
chap08.4.3.f	chap08.4.4.f	chap08.4.5.f  	chap08.5.f		\
chap09.f		chap10.f		chap11.f		chap12.f  		\
chap13.f		chap14.f

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
chap08.1.n	\
chap08.2.n	\
chap08.3.n	\
chap08.4.1.n	\
chap08.4.2.n	\
chap08.4.3.n	\
chap08.4.4.n	\
chap08.4.5.n	\
chap08.5.n	\
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
HELP/locks	\
HELP/protection

.SUFFIXES:
.SUFFIXES:	.f .n

.n.f:
	tbl common.nr $*.n | $(EQN) | $(ROFF) $(FLAGS) >$*.f

vax:	$(FMT) titlepage.f

printed:	$(SRC)
	$(LROFF) $* | laser

titlepage.f:	titlepage.n
	$(ROFF) $(FLAGS) titlepage.n  >titlepage.f

help:	HELP/help.done

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
		chap05.n | \
	nroff -ms -o2-99 > HELP/6

HELP/7:		chap07.n
	echo "not written yet" > HELP/7
#	nroff -ms -o2-99 chap07.n > HELP/7

HELP/8:		chap08.1.n
	echo "not written yet" > HELP/8
#	nroff -ms -o2-99 chap08.n > HELP/8

HELP/9:		chap09.n
	echo "not written yet" > HELP/9
#	nroff -ms -o2-99 chap09.n > HELP/9

HELP/10:	chap10.n
	echo "not written yet" > HELP/10
#	nroff -ms -o2-99 chap10.n > HELP/10

HELP/11:	chap11.n
	echo "not written yet" > HELP/11
#	nroff -ms -o2-99 chap11.n > HELP/11

HELP/12:	chap12.n
	echo "not written yet" > HELP/12
#	nroff -ms -o2-99 chap12.n > HELP/12

HELP/13:	chap13.n
	echo "not written yet" > HELP/13
#	nroff -ms -o2-99 chap13.n > HELP/13

HELP/14:	chap14.n
	echo "not written yet" > HELP/14
#	nroff -ms -o2-99 chap14.n > HELP/14

HELP/15:	chap15.n
	echo "not written yet" > HELP/15
#	nroff -ms -o2-99 chap15.n > HELP/15

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

