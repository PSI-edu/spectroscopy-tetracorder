#include "psdaemon.h"

/*
 *	greek.c
 */

greek(line)
char	*line;
{
	int	iangle;


	while (*line) {
		switch (*line++) {
		case 'a': /* alpha */
			TEXT("(a) show\n");
			break;
		case 'A': /* alpha */
			TEXT("(A) show\n");
			break;
		case 'b': /* beta */
			TEXT("(b) show\n");
			break;
		case 'B': /* beta */
			TEXT("(B) show\n");
			break;
		case 'g': /* gamma */
			TEXT("(g) show\n");
			break;
		case 'G': /* gamma */
			TEXT("(G) show\n");
			break;
		case 'd': /* delta */
			TEXT("(d) show\n");
			break;
		case 'D': /* delta */
			TEXT("(D) show\n");
			break;
		case 'e': /* epsilon */	
			TEXT("(e) show\n");
			break;
		case 'E': /* epsilon */
			TEXT("(E) show\n");
			break;
		case 'z': /* zeta */
			TEXT("(z) show\n");
			break;
		case 'Z': /* zeta */
			TEXT("(Z) show\n");
			break;
		case 'y': /* eta */
			TEXT("(y) show\n");
			break;
		case 'Y': /* eta */
			TEXT("(Y) show\n");
			break;
		case 'h': /* theta */
			TEXT("(h) show\n");
			break;
		case 'H': /* theta */
			TEXT("(H) show\n");
			break;
		case 'i': /* iota */
			TEXT("(i) show\n");
			break;
		case 'I': /* iota */	
			TEXT("(I) show\n");
			break;
		case 'k': /* kappa */
			TEXT("(k) show\n");
			break;
		case 'K': /* kappa */
			TEXT("(K) show\n");
			break;
		case 'l': /* lambda */
			TEXT("(l) show\n");
			break;
		case 'L': /* lambda */
			TEXT("(L) show\n");
			break;
		case 'm': /* mu */
			TEXT("(m) show\n");
			break;
		case 'M': /* mu */
			TEXT("(M) show\n");
			break;
		case 'n': /* nu */
			TEXT("(n) show\n");
			break;
		case 'N': /* nu */
			TEXT("(N) show\n");
			break;
		case 'c': /* xi */
			TEXT("(c) show\n");
			break;
		case 'C': /* xi */
			TEXT("(C) show\n");
			break;
		case 'o':  /* omicron */
			TEXT("(o) show\n");
			break;
		case 'O': /* omicron */
			TEXT("(O) show\n");
			break;
		case 'p': /* pi */
			TEXT("(p) show\n");
			break;
		case 'P': /* pi */
			TEXT("(P) show\n");
			break;
		case 'r': /* rho */
			TEXT("(r) show\n");
			break;
		case 'R': /* rho */
			TEXT("(R) show\n");
			break;
		case 's': /* sigma */
			TEXT("(s) show\n");
			break;
		case 'S': /* sigma */
			TEXT("(S) show\n");
			break;
		case 't': /* tau */
			TEXT("(t) show\n");
			break;
		case 'T': /* tau */
			TEXT("(T) show\n");
			break;
		case 'u': /* upsilon */
			TEXT("(u) show\n");
			break;
		case 'U': /* upsilon */
			TEXT("(U) show\n");
			break;
		case 'f': /* phi */
			TEXT("(f) show\n");
			break;
		case 'F': /* phi */
			TEXT("(F) show\n");
			break;
		case 'x':  /* chi */
			TEXT("(x) show\n");
			break;
		case 'X': /* chi */
			TEXT("(X) show\n");
			break;
		case 'q': /* psi */
			TEXT("(q) show\n");
			break;
		case 'Q': /* psi */
			TEXT("(Q) show\n");
			break;
		case 'w': /* omega */
			TEXT("(w) show\n");
			break;
		case 'W': /* omega */
			TEXT("(W) show\n");
			break;
		default: /* default */
			break;
		}
	}
}
