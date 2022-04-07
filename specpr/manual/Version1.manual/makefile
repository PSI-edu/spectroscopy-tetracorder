EQN=neqn
ROFF=nroff
FLAGS=-me

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

.SUFFIXES:
.SUFFIXES:	.f .n

.n.f:
	tbl common.nr $*.n | $(EQN) | $(ROFF) $(FLAGS) >$*.f

vax:	$(FMT) titlepage.f

titlepage.f:	titlepage.n
	$(ROFF) $(FLAGS) titlepage.n  >titlepage.f
