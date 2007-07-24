/* adapt.f -- translated by f2c (version 20031025).
   You must link the resulting object file with libf2c:
	on Microsoft Windows system, link with libf2c.lib;
	on Linux or Unix systems, link with .../path/to/libf2c.a -lm
	or, if you install libf2c.a in a standard place, with -lf2c -lm
	-- in that order, at the end of the command line, as in
		cc *.o -lf2c -lm
	Source for libf2c is in /netlib/f2c/libf2c.zip, e.g.,

		http://www.netlib.org/f2c/libf2c.zip
*/

#include "f2c.h"

/* Table of constant values */

static integer c__2 = 2;
static integer c__200 = 200;

/*<    >*/
/* Subroutine */ int adapt_(integer *ndim, doublereal *a, doublereal *b, 
	integer *minpts, integer *maxpts, doublereal (*functn)(integer *ndim, doublereal *z), doublereal *eps, 
	doublereal *relerr, integer *lenwrk, doublereal *wrkstr, doublereal *
	finest, integer *ifail)
{
    /* System generated locals */
    integer i__1;

    /* Builtin functions */
    integer pow_ii(integer *, integer *);

    /* Local variables */
    static integer j, k;
    static doublereal one, two, half;
    extern /* Subroutine */ int bsrl_(integer *, doublereal *, doublereal *, 
	    D_fp, integer *, integer *, doublereal *, doublereal *, 
	    doublereal *, integer *, integer *);
    static doublereal zero, width[20];
    static integer index1, index2, divflg;
    static doublereal center[20];
    static integer maxcls;
    static doublereal rgnval;
    static integer divaxo;
    static doublereal errmin;
    static integer divaxn;
    static doublereal rgnerr;
    static integer funcls, sbrgns, subrgn, rulcls, sbtmpp, subtmp, rgnstr;

/* ***BEGIN PROLOGUE ADAPT */
/*  ADAPTIVE MULTIDIMENSIONAL INTEGRATION SUBROUTINE */
/*           AUTHOR: A. C. GENZ, Washington State University */
/*                    19 March 1984 */
/* **************  PARAMETERS FOR ADAPT  ******************************** */
/* ***** INPUT PARAMETERS */
/*  NDIM    NUMBER OF VARIABLES, MUST EXCEED 1, BUT NOT EXCEED 20 */
/*  A       REAL ARRAY OF LOWER LIMITS, WITH DIMENSION NDIM */
/*  B       REAL ARRAY OF UPPER LIMITS, WITH DIMENSION NDIM */
/*  MINPTS  MINIMUM NUMBER OF FUNCTION EVALUATIONS TO BE ALLOWED. */
/*          ON THE FIRST CALL TO ADAPT MINPTS SHOULD BE SET TO A */
/*          NON NEGATIVE VALUE. (CAUTION... MINPTS IS ALTERED BY ADAPT) */
/*          IT IS POSSIBLE TO CONTINUE A CALCULATION TO GREATER ACCURACY */
/*          BY CALLING ADAPT AGAIN BY DECREASING EPS (DESCRIBED BELOW) */
/*          AND RESETTING MINPTS TO ANY NEGATIVE VALUE. */
/*          MINPTS MUST NOT EXCEED MAXPTS. */
/*  MAXPTS  MAXIMUM NUMBER OF FUNCTION EVALUATIONS TO BE ALLOWED, */
/*          WHICH MUST BE AT LEAST RULCLS, WHERE */
/*          RULCLS =  2**NDIM+2*NDIM**2+6*NDIM+1 */

/*            FOR NDIM =  2   3   4   5   6   7   8   9   10 */
/*            MAXPTS >=  25  45  73 113 173 269 433 729 1285 */
/*         A suggested value for MAXPTS is 100 times the above values. */

/*  FUNCTN  EXTERNALLY DECLARED USER DEFINED FUNCTION TO BE INTEGRATED. */
/*          IT MUST HAVE PARAMETERS (NDIM,Z), WHERE Z IS A REAL ARRAY */
/*          OF DIMENSION NDIM. */
/*  EPS     REQUIRED RELATIVE ACCURACY */
/*  LENWRK  LENGTH OF ARRAY WRKSTR OF WORKING STORAGE, THE ROUTINE */
/*          NEEDS (2*NDIM+3)*(1+MAXPTS/RULCLS)/2 FOR LENWRK IF */
/*          MAXPTS FUNCTION CALLS ARE USED. */
/*          FOR GUIDANCE, IF YOU SET MAXPTS TO 100*RULCLS (SEE TABLE */
/*          ABOVE) THEN ACCEPTABLE VALUES FOR LENWRK ARE */

/*            FOR NDIM = 2    3    4    5    6    7    8     9 */
/*            LENWRK =  357  561  1785 3417 6681 13209 26265 52377 */

/* ***** OUTPUT PARAMETERS */
/*  MINPTS  ACTUAL NUMBER OF FUNCTION EVALUATIONS USED BY ADAPT */
/*  WRKSTR  REAL ARRAY OF WORKING STORAGE OF DIMENSION (LENWRK). */
/*  RELERR  ESTIMATED RELATIVE ACCURACY OF FINEST */
/*  FINEST  ESTIMATED VALUE OF INTEGRAL */
/*  IFAIL   IFAIL=0 FOR NORMAL EXIT, WHEN ESTIMATED RELATIVE ACCURACY */
/*                  RELERR IS LESS THAN EPS WITH MAXPTS OR LESS FUNCTION */
/*                  CALLS MADE. */
/*          IFAIL=1 IF MAXPTS WAS TOO SMALL FOR ADAPT TO OBTAIN THE */
/*                  REQUIRED RELATIVE ACCURACY EPS.  IN THIS CASE ADAPT */
/*                  RETURNS A VALUE OF FINEST WITH ESTIMATED RELATIVE */
/*                  ACCURACY RELERR. */
/*          IFAIL=2 IF LENWRK TOO SMALL FOR MAXPTS FUNCTION CALLS.  IN */
/*                  THIS CASE ADAPT RETURNS A VALUE OF FINEST WITH */
/*                  ESTIMATED ACCURACY RELERR USING THE WORKING STORAGE */
/*                  AVAILABLE, BUT RELERR WILL BE GREATER THAN EPS. */
/*          IFAIL=3 IF NDIM ) 2, NDIM \ 20, MINPTS \ MAXPTS, */
/*                  OR MAXPTS ) RULCLS. */
/* *********************************************************************** */
/* ***END PROLOGUE ADAPT */
/*<       EXTERNAL FUNCTN >*/
/* *****  FOR DOUBLE PRECISION CHANGE REAL TO DOUBLE PRECISION IN THE */
/*        NEXT STATEMENT. */
/*<    >*/
/*<    >*/
/*<       IFAIL=3 >*/
    /* Parameter adjustments */
    --b;
    --a;
    --wrkstr;

    /* Function Body */
    *ifail = 3;
/*<       RELERR=1 >*/
    *relerr = 1.;
/*<       FUNCLS=0 >*/
    funcls = 0;
/*<       IF(NDIM.LT.2.OR.NDIM.GT.20) GOTO 300 >*/
    if (*ndim < 2 || *ndim > 20) {
	goto L300;
    }
/*<       IF(MINPTS.GT.MAXPTS) GOTO 300 >*/
    if (*minpts > *maxpts) {
	goto L300;
    }

/* *****  INITIALISATION OF SUBROUTINE */

/*<       ZERO=0 >*/
    zero = 0.;
/*<       ONE=1 >*/
    one = 1.;
/*<       TWO=2 >*/
    two = 2.;
/*<       HALF=ONE/TWO >*/
    half = one / two;
/*<       RGNSTR=2*NDIM+3 >*/
    rgnstr = (*ndim << 1) + 3;
/*<       ERRMIN = ZERO >*/
    errmin = zero;
/*<       MAXCLS =  2**NDIM+2*NDIM**2+6*NDIM+1 >*/
/* Computing 2nd power */
    i__1 = *ndim;
    maxcls = pow_ii(&c__2, ndim) + (i__1 * i__1 << 1) + *ndim * 6 + 1;
/*<       MAXCLS = MIN0(MAXCLS,MAXPTS) >*/
    maxcls = min(maxcls,*maxpts);
/*<       DIVAXO=0 >*/
    divaxo = 0;

/* *****  END SUBROUTINE INITIALISATION */
/*<       IF(MINPTS.LT.0) SBRGNS=WRKSTR(LENWRK-1) >*/
    if (*minpts < 0) {
	sbrgns = (integer) wrkstr[*lenwrk - 1];
    }
/*<       IF(MINPTS.LT.0) GOTO 280 >*/
    if (*minpts < 0) {
	goto L280;
    }
/*<       DO 30 J=1,NDIM >*/
    i__1 = *ndim;
    for (j = 1; j <= i__1; ++j) {
/*<         WIDTH(J)=(B(J)-A(J))*HALF >*/
	width[j - 1] = (b[j] - a[j]) * half;
/*<    30   CENTER(J)=A(J)+WIDTH(J) >*/
/* L30: */
	center[j - 1] = a[j] + width[j - 1];
    }
/*<       FINEST=ZERO >*/
    *finest = zero;
/*<       WRKSTR(LENWRK)=ZERO >*/
    wrkstr[*lenwrk] = zero;
/*<       DIVFLG=1 >*/
    divflg = 1;
/*<       SUBRGN=RGNSTR >*/
    subrgn = rgnstr;
/*<       SBRGNS=RGNSTR >*/
    sbrgns = rgnstr;
/*<    >*/
L40:
    bsrl_(ndim, center, width, (D_fp)functn, &maxcls, &rulcls, &errmin, &
	    rgnerr, &rgnval, &divaxo, &divaxn);
/*<       FINEST=FINEST+RGNVAL >*/
    *finest += rgnval;
/*<       WRKSTR(LENWRK)=WRKSTR(LENWRK)+RGNERR >*/
    wrkstr[*lenwrk] += rgnerr;
/*<       FUNCLS = FUNCLS + RULCLS >*/
    funcls += rulcls;

/* *****  PLACE RESULTS OF BASIC RULE INTO PARTIALLY ORDERED LIST */
/* *****  ACCORDING TO SUBREGION ERROR */
/*<       IF(DIVFLG.EQ.1) GO TO 230 >*/
    if (divflg == 1) {
	goto L230;
    }

/* *****  WHEN DIVFLG=0 START AT TOP OF LIST AND MOVE DOWN LIST TREE TO */
/*       FIND CORRECT POSITION FOR RESULTS FROM FIRST HALF OF RECENTLY */
/*       DIVIDED SUBREGION */
/*<   200 SUBTMP=2*SUBRGN >*/
L200:
    subtmp = subrgn << 1;
/*<       IF(SUBTMP.GT.SBRGNS) GO TO 250 >*/
    if (subtmp > sbrgns) {
	goto L250;
    }
/*<        IF(SUBTMP.EQ.SBRGNS) GO TO 210 >*/
    if (subtmp == sbrgns) {
	goto L210;
    }
/*<        SBTMPP=SUBTMP+RGNSTR >*/
    sbtmpp = subtmp + rgnstr;
/*<        IF(WRKSTR(SUBTMP).LT.WRKSTR(SBTMPP)) SUBTMP=SBTMPP >*/
    if (wrkstr[subtmp] < wrkstr[sbtmpp]) {
	subtmp = sbtmpp;
    }
/*<   210  IF(RGNERR.GE.WRKSTR(SUBTMP)) GO TO 250 >*/
L210:
    if (rgnerr >= wrkstr[subtmp]) {
	goto L250;
    }
/*<         DO 220 K=1,RGNSTR >*/
    i__1 = rgnstr;
    for (k = 1; k <= i__1; ++k) {
/*<           INDEX1=SUBRGN-K+1 >*/
	index1 = subrgn - k + 1;
/*<           INDEX2=SUBTMP-K+1 >*/
	index2 = subtmp - k + 1;
/*<   220     WRKSTR(INDEX1)=WRKSTR(INDEX2) >*/
/* L220: */
	wrkstr[index1] = wrkstr[index2];
    }
/*<         SUBRGN=SUBTMP >*/
    subrgn = subtmp;
/*<       GOTO 200 >*/
    goto L200;

/* *****  WHEN DIVFLG=1 START AT BOTTOM RIGHT BRANCH AND MOVE UP LIST */
/*       TREE TO FIND CORRECT POSITION FOR RESULTS FROM SECOND HALF OF */
/*       RECENTLY DIVIDED SUBREGION */
/*<   230 SUBTMP=(SUBRGN/(RGNSTR*2))*RGNSTR >*/
L230:
    subtmp = subrgn / (rgnstr << 1) * rgnstr;
/*<       IF(SUBTMP.LT.RGNSTR) GO TO 250 >*/
    if (subtmp < rgnstr) {
	goto L250;
    }
/*<       IF(RGNERR.LE.WRKSTR(SUBTMP)) GO TO 250 >*/
    if (rgnerr <= wrkstr[subtmp]) {
	goto L250;
    }
/*<        DO 240 K=1,RGNSTR >*/
    i__1 = rgnstr;
    for (k = 1; k <= i__1; ++k) {
/*<          INDEX1=SUBRGN-K+1 >*/
	index1 = subrgn - k + 1;
/*<          INDEX2=SUBTMP-K+1 >*/
	index2 = subtmp - k + 1;
/*<   240    WRKSTR(INDEX1)=WRKSTR(INDEX2) >*/
/* L240: */
	wrkstr[index1] = wrkstr[index2];
    }
/*<        SUBRGN=SUBTMP >*/
    subrgn = subtmp;
/*<       GOTO 230 >*/
    goto L230;
/* *****  STORE RESULTS OF BASIC RULE IN CORRECT POSITION IN LIST */
/*<   250 WRKSTR(SUBRGN)=RGNERR >*/
L250:
    wrkstr[subrgn] = rgnerr;
/*<       WRKSTR(SUBRGN-1)=RGNVAL >*/
    wrkstr[subrgn - 1] = rgnval;
/*<       WRKSTR(SUBRGN-2)=DIVAXN >*/
    wrkstr[subrgn - 2] = (doublereal) divaxn;
/*<       DO 260 J=1,NDIM >*/
    i__1 = *ndim;
    for (j = 1; j <= i__1; ++j) {
/*<         SUBTMP=SUBRGN-2*(J+1) >*/
	subtmp = subrgn - (j + 1 << 1);
/*<         WRKSTR(SUBTMP+1)=CENTER(J) >*/
	wrkstr[subtmp + 1] = center[j - 1];
/*<   260   WRKSTR(SUBTMP)=WIDTH(J) >*/
/* L260: */
	wrkstr[subtmp] = width[j - 1];
    }
/*<       IF(DIVFLG.EQ.1) GO TO 270 >*/
    if (divflg == 1) {
	goto L270;
    }
/* *****  WHEN DIVFLG=0 PREPARE FOR SECOND APPLICATION OF BASIC RULE */
/*<       CENTER(DIVAXO)=CENTER(DIVAXO)+TWO*WIDTH(DIVAXO) >*/
    center[divaxo - 1] += two * width[divaxo - 1];
/*<       SBRGNS=SBRGNS+RGNSTR >*/
    sbrgns += rgnstr;
/*<       SUBRGN=SBRGNS >*/
    subrgn = sbrgns;
/*<       DIVFLG=1 >*/
    divflg = 1;
/* *****  LOOP BACK TO APPLY BASIC RULE TO OTHER HALF OF SUBREGION */
/*<       GO TO 40 >*/
    goto L40;

/* *****  END ORDERING AND STORAGE OF BASIC RULE RESULTS */
/* *****  MAKE CHECKS FOR POSSIBLE TERMINATION OF ROUTINE */

/* ******  FOR DOUBLE PRECISION CHANGE ABS TO DABS IN THE NEXT STATEMENT */
/*<   270 RELERR=ONE >*/
L270:
    *relerr = one;
/*<       IF(WRKSTR(LENWRK).LE.ZERO) WRKSTR(LENWRK)=ZERO >*/
    if (wrkstr[*lenwrk] <= zero) {
	wrkstr[*lenwrk] = zero;
    }
/*<       IF(ABS(FINEST).NE.ZERO) RELERR=WRKSTR(LENWRK)/ABS(FINEST) >*/
    if (abs(*finest) != zero) {
	*relerr = wrkstr[*lenwrk] / abs(*finest);
    }
/*<       IF(RELERR.GT.ONE) RELERR=ONE >*/
    if (*relerr > one) {
	*relerr = one;
    }
/*<       IF(SBRGNS+RGNSTR.GT.LENWRK-2) IFAIL=2 >*/
    if (sbrgns + rgnstr > *lenwrk - 2) {
	*ifail = 2;
    }
/*<       IF(FUNCLS+FUNCLS*RGNSTR/SBRGNS.GT.MAXPTS) IFAIL=1 >*/
    if (funcls + funcls * rgnstr / sbrgns > *maxpts) {
	*ifail = 1;
    }
/*<       IF(RELERR.LT.EPS.AND.FUNCLS.GE.MINPTS) IFAIL=0 >*/
    if (*relerr < *eps && funcls >= *minpts) {
	*ifail = 0;
    }
/*<       IF(IFAIL.LT.3) GOTO 300 >*/
    if (*ifail < 3) {
	goto L300;
    }

/* *****  PREPARE TO USE BASIC RULE ON EACH HALF OF SUBREGION WITH LARGEST */
/*       ERROR */
/*<   280 DIVFLG=0 >*/
L280:
    divflg = 0;
/*<       SUBRGN=RGNSTR >*/
    subrgn = rgnstr;
/*<       SUBTMP = 2*SBRGNS/RGNSTR >*/
    subtmp = (sbrgns << 1) / rgnstr;
/*<       MAXCLS = MAXPTS/SUBTMP >*/
    maxcls = *maxpts / subtmp;
/*<       ERRMIN = ABS(FINEST)*EPS/FLOAT(SUBTMP) >*/
    errmin = abs(*finest) * *eps / (doublereal) subtmp;
/*<       WRKSTR(LENWRK)=WRKSTR(LENWRK)-WRKSTR(SUBRGN) >*/
    wrkstr[*lenwrk] -= wrkstr[subrgn];
/*<       FINEST=FINEST-WRKSTR(SUBRGN-1) >*/
    *finest -= wrkstr[subrgn - 1];
/*<       DIVAXO=WRKSTR(SUBRGN-2) >*/
    divaxo = (integer) wrkstr[subrgn - 2];
/*<       DO 290 J=1,NDIM >*/
    i__1 = *ndim;
    for (j = 1; j <= i__1; ++j) {
/*<         SUBTMP=SUBRGN-2*(J+1) >*/
	subtmp = subrgn - (j + 1 << 1);
/*<         CENTER(J)=WRKSTR(SUBTMP+1) >*/
	center[j - 1] = wrkstr[subtmp + 1];
/*<   290   WIDTH(J)=WRKSTR(SUBTMP) >*/
/* L290: */
	width[j - 1] = wrkstr[subtmp];
    }
/*<       WIDTH(DIVAXO)=WIDTH(DIVAXO)*HALF >*/
    width[divaxo - 1] *= half;
/*<       CENTER(DIVAXO)=CENTER(DIVAXO)-WIDTH(DIVAXO) >*/
    center[divaxo - 1] -= width[divaxo - 1];

/* *****  LOOP BACK TO APPLY BASIC RULE */

/*<       GOTO 40 >*/
    goto L40;

/* *****  TERMINATION POINT */

/*<   300 MINPTS=FUNCLS >*/
L300:
    *minpts = funcls;
/*<       WRKSTR(LENWRK-1)=SBRGNS >*/
    wrkstr[*lenwrk - 1] = (doublereal) sbrgns;
/*<       RETURN >*/
    return 0;
/*<       END >*/
} /* adapt_ */

/*<    >*/
/* Subroutine */ int bsrl_(integer *s, doublereal *center, doublereal *hwidth,
	 D_fp f, integer *maxvls, integer *funcls, doublereal *errmin, 
	doublereal *errest, doublereal *basest, integer *divaxo, integer *
	divaxn)
{
    /* System generated locals */
    integer i__1;
    doublereal d__1, d__2, d__3;

    /* Local variables */
    static integer i__;
    static doublereal z__[20], x1, x2, dif, one, ten, two, sum0, sum1, sum2, 
	    five, zero;
    static integer ifail;
    static doublereal three;
    extern /* Subroutine */ int symrl_(integer *, doublereal *, doublereal *, 
	    D_fp, integer *, integer *, doublereal *, integer *, integer *, 
	    doublereal *, doublereal *, integer *);
    static integer mindeg, maxdeg;
    static doublereal difmax;
    static integer minord, intcls, maxord;
    static doublereal weghts[200], errorm, fulsms[200], intvls[20];

/*<       EXTERNAL F >*/
/*<    >*/
/*<    >*/
/*<       MAXDEG = 12 >*/
    /* Parameter adjustments */
    --hwidth;
    --center;

    /* Function Body */
    maxdeg = 12;
/*<       MINDEG = 4 >*/
    mindeg = 4;
/*<       MINORD = 0 >*/
    minord = 0;
/*<       ZERO = 0 >*/
    zero = 0.;
/*<       ONE = 1 >*/
    one = 1.;
/*<       TWO = 2 >*/
    two = 2.;
/*<       THREE = 3 >*/
    three = 3.;
/*<       FIVE = 5 >*/
    five = 5.;
/*<       TEN = 10 >*/
    ten = 10.;
/*<       DO 10 MAXORD = MINDEG,MAXDEG >*/
    i__1 = maxdeg;
    for (maxord = mindeg; maxord <= i__1; ++maxord) {
/*<    >*/
	symrl_(s, &center[1], &hwidth[1], (D_fp)f, &minord, &maxord, intvls, &
		intcls, &c__200, weghts, fulsms, &ifail);
/*<         IF (IFAIL.EQ.2) GOTO 20 >*/
	if (ifail == 2) {
	    goto L20;
	}
/*<         ERREST = ABS(INTVLS(MAXORD)-INTVLS(MAXORD-1)) >*/
	*errest = (d__1 = intvls[maxord - 1] - intvls[maxord - 2], abs(d__1));
/*<         ERRORM = ABS(INTVLS(MAXORD-1)-INTVLS(MAXORD-2)) >*/
	errorm = (d__1 = intvls[maxord - 2] - intvls[maxord - 3], abs(d__1));
/*<    >*/
	if (*errest != zero) {
/* Computing MAX */
/* Computing MAX */
	    d__3 = *errest / two;
	    d__1 = one / ten, d__2 = *errest / max(d__3,errorm);
	    *errest *= max(d__1,d__2);
	}
/*<         IF (ERRORM.LE.FIVE*ERREST) GOTO 20 >*/
	if (errorm <= five * *errest) {
	    goto L20;
	}
/*<         IF (2*INTCLS.GT.MAXVLS) GOTO 20 >*/
	if (intcls << 1 > *maxvls) {
	    goto L20;
	}
/*<         IF (ERREST.LT.ERRMIN) GOTO 20 >*/
	if (*errest < *errmin) {
	    goto L20;
	}
/*<    10   CONTINUE >*/
/* L10: */
    }
/*<    20 DIFMAX = -1 >*/
L20:
    difmax = -1.;
/*<       X1 = ONE/TWO**2 >*/
/* Computing 2nd power */
    d__1 = two;
    x1 = one / (d__1 * d__1);
/*<       X2 = THREE*X1 >*/
    x2 = three * x1;
/*<       DO 30 I = 1,S >*/
    i__1 = *s;
    for (i__ = 1; i__ <= i__1; ++i__) {
/*<        Z(I) = CENTER(I) >*/
	z__[i__ - 1] = center[i__];
/*<    30  CONTINUE >*/
/* L30: */
    }
/*<       SUM0 = F(S,Z) >*/
    sum0 = (*f)(s, z__);
/*<       DO 40 I = 1,S >*/
    i__1 = *s;
    for (i__ = 1; i__ <= i__1; ++i__) {
/*<        Z(I) = CENTER(I) - X1*HWIDTH(I) >*/
	z__[i__ - 1] = center[i__] - x1 * hwidth[i__];
/*<        SUM1 = F(S,Z) >*/
	sum1 = (*f)(s, z__);
/*<        Z(I) = CENTER(I) + X1*HWIDTH(I) >*/
	z__[i__ - 1] = center[i__] + x1 * hwidth[i__];
/*<        SUM1 = SUM1 + F(S,Z) >*/
	sum1 += (*f)(s, z__);
/*<        Z(I) = CENTER(I) - X2*HWIDTH(I) >*/
	z__[i__ - 1] = center[i__] - x2 * hwidth[i__];
/*<        SUM2 = F(S,Z) >*/
	sum2 = (*f)(s, z__);
/*<        Z(I) = CENTER(I) + X2*HWIDTH(I) >*/
	z__[i__ - 1] = center[i__] + x2 * hwidth[i__];
/*<        SUM2 = SUM2 + F(S,Z) >*/
	sum2 += (*f)(s, z__);
/*<        Z(I) = CENTER(I) >*/
	z__[i__ - 1] = center[i__];
/*<        DIF = ABS((SUM1-TWO*SUM0) - (X1/X2)**2*(SUM2-TWO*SUM0)) >*/
/* Computing 2nd power */
	d__2 = x1 / x2;
	dif = (d__1 = sum1 - two * sum0 - d__2 * d__2 * (sum2 - two * sum0), 
		abs(d__1));
/*<        IF (DIF.LT.DIFMAX) GOTO 40 >*/
	if (dif < difmax) {
	    goto L40;
	}
/*<         DIFMAX = DIF >*/
	difmax = dif;
/*<         DIVAXN = I >*/
	*divaxn = i__;
/*<    40  CONTINUE >*/
L40:
	;
    }
/*<        IF (SUM0.EQ.SUM0+DIFMAX/TWO) DIVAXN = MOD(DIVAXO,S) + 1 >*/
    if (sum0 == sum0 + difmax / two) {
	*divaxn = *divaxo % *s + 1;
    }
/*<       BASEST = INTVLS(MINORD) >*/
    *basest = intvls[minord - 1];
/*<       FUNCLS = INTCLS + 4*S >*/
    *funcls = intcls + (*s << 2);
/*<       RETURN >*/
    return 0;
/*<       END >*/
} /* bsrl_ */

/*<    >*/
/* Subroutine */ int symrl_(integer *s, doublereal *center, doublereal *
	hwidth, D_fp f, integer *minord, integer *maxord, doublereal *intvls, 
	integer *intcls, integer *numsms, doublereal *weghts, doublereal *
	fulsms, integer *fail)
{
    /* Initialized data */

    static doublereal g[20] = { 0.,.7745966692414833,.9604912687080202,
	    .4342437493468025,.9938319632127549,.8884592328722569,
	    .6211029467372263,.2233866864289668,.1,.2,.3,.4 };

    /* System generated locals */
    integer i__1, i__2;
    doublereal d__1, d__2;

    /* Local variables */
    static integer d__, i__, k[20], l, m[20];
    static doublereal one;
    extern doublereal wht_(integer *, doublereal *, integer *, integer *, 
	    integer *, integer *, integer *, doublereal *);
    static doublereal two;
    extern doublereal flsm_(integer *, doublereal *, doublereal *, doublereal 
	    *, integer *, integer *, integer *, doublereal *, D_fp, integer *)
	    ;
    static integer maxs;
    static doublereal zero;
    extern /* Subroutine */ int nxprt_(integer *, integer *, integer *);
    static integer modofm;
    static doublereal floatl, hundrd, gisqrd, intmpa, glsqrd, intmpb, intval, 
	    momnkn, momprd[400]	/* was [20][20] */, moment[20];
    static integer maxrdm, sumcls;
    static doublereal momtol, fulwgt;
    static integer prtcnt;

/*  MULTIDIMENSIONAL FULLY SYMMETRIC RULE INTEGRATION SUBROUTINE */

/*   THIS SUBROUTINE COMPUTES A SEQUENCE OF FULLY SYMMETRIC RULE */
/*   APPROXIMATIONS TO A FULLY SYMMETRIC MULTIPLE INTEGRAL. */
/*   WRITTEN BY A. GENZ, MATHEMATICAL INSTITUTE, UNIVERSITY OF KENT, */
/*   CANTERBURY, KENT CT2 7NF, ENGLAND */

/* **************  PARAMETERS FOR SYMRL  ******************************** */
/* *****INPUT PARAMETERS */
/*  S       INTEGER NUMBER OF VARIABLES, MUST EXCEED 0 BUT NOT EXCEED 20 */
/*  F       EXTERNALLY DECLARED USER DEFINED REAL FUNCTION INTEGRAND. */
/*          IT MUST HAVE PARAMETERS (S,X), WHERE X IS A REAL ARRAY */
/*          WITH DIMENSION S. */
/*  MINORD  INTEGER MINIMUM ORDER PARAMETER.  ON ENTRY MINORD SPECIFIES */
/*          THE CURRENT HIGHEST ORDER APPROXIMATION TO THE INTEGRAL, */
/*          AVAILABLE IN THE ARRAY INTVLS.  FOR THE FIRST CALL OF SYMRL */
/*          MINORD SHOULD BE SET TO 0.  OTHERWISE A PREVIOUS CALL IS */
/*          ASSUMED THAT COMPUTED INTVLS(1), ... , INTVLS(MINORD). */
/*          ON EXIT MINORD IS SET TO MAXORD. */
/*  MAXORD  INTEGER MAXIMUM ORDER PARAMETER, MUST BE GREATER THAN MINORD */
/*          AND NOT EXCEED 20. THE SUBROUTINE COMPUTES INTVLS(MINORD+1), */
/*          INTVLS(MINORD+2),..., INTVLS(MAXORD). */
/*  G       REAL ARRAY OF DIMENSION(MAXORD) OF GENERATORS. */
/*          ALL GENERATORS MUST BE DISTINCT AND NONNEGATIVE. */
/*  NUMSMS  INTEGER LENGTH OF ARRAY FULSMS, MUST BE AT LEAST THE SUM OF */
/*          THE NUMBER OF DISTINCT PARTITIONS OF LENGTH AT MOST S */
/*          OF THE INTEGERS 0,1,...,MAXORD-1.  AN UPPER BOUND FOR NUMSMS */
/*          WHEN S+MAXORD IS LESS THAN 19 IS 200 */
/* ******OUTPUT PARAMETERS */
/*  INTVLS  REAL ARRAY OF DIMENSION(MAXORD).  UPON SUCCESSFUL EXIT */
/*          INTVLS(1), INTVLS(2),..., INTVLS(MAXORD) ARE APPROXIMATIONS */
/*          TO THE INTEGRAL.  INTVLS(D+1) WILL BE AN APPROXIMATION OF */
/*          POLYNOMIAL DEGREE 2D+1. */
/*  INTCLS  INTEGER TOTAL NUMBER OF F VALUES NEEDED FOR INTVLS(MAXORD) */
/*  WEGHTS  REAL WORKING STORAGE ARRAY WITH DIMENSION (NUMSMS). ON EXIT */
/*          WEGHTS(J) CONTAINS THE WEIGHT FOR FULSMS(J). */
/*  FULSMS  REAL WORKING STORAGE ARRAY WITH DIMENSION (NUMSMS). ON EXIT */
/*          FULSMS(J) CONTAINS THE FULLY SYMMETRIC BASIC RULE SUM */
/*          INDEXED BY THE JTH S-PARTITION OF THE INTEGERS */
/*          0,1,...,MAXORD-1. */
/*  FAIL    INTEGER FAILURE OUTPUT PARAMETER */
/*          FAIL=0 FOR SUCCESSFUL TERMINATION OF THE SUBROUTINE */
/*          FAIL=1 WHEN NUMSMS IS TOO SMALL FOR THE SUBROUTINE TO */
/*                  CONTINUE.  IN THIS CASE WEGHTS(1), WEGHTS(2), ..., */
/*                  WEGHTS(NUMSMS), FULSMS(1), FULSMS(2), ..., */
/*                  FULSMS(NUMSMS) AND INTVLS(1), INTVLS(2),..., */
/*                  INTVLS(J) ARE RETURNED, WHERE J IS MAXIMUM VALUE OF */
/*                  MAXORD COMPATIBLE WITH THE GIVEN VALUE OF NUMSMS. */
/*          FAIL=2 WHEN PARAMETERS S,MINORD, MAXORD OR G ARE OUT OF */
/*                  RANGE */
/* *********************************************************************** */
/*<       EXTERNAL F >*/
/* ***  FOR DOUBLE PRECISION CHANGE REAL TO DOUBLE PRECISION */
/*      IN THE NEXT STATEMENT */
/*<    >*/
/*<    >*/
/*       PATTERSON GENERATORS */
/*<       DATA G(1), G(2) /0.0000000000000000,0.7745966692414833/ >*/
    /* Parameter adjustments */
    --hwidth;
    --center;
    --intvls;
    --fulsms;
    --weghts;

    /* Function Body */
/*<       DATA G(3), G(4) /0.9604912687080202,0.4342437493468025/ >*/
/*<       DATA G(5), G(6) /0.9938319632127549,0.8884592328722569/ >*/
/*<       DATA G(7), G(8) /0.6211029467372263,0.2233866864289668/ >*/
/*<       DATA G(9), G(10), G(11), G(12) /0.1, 0.2, 0.3, 0.4/ >*/

/* ***  PARAMETER CHECKING AND INITIALISATION */
/*<       FAIL = 2 >*/
    *fail = 2;
/*<       MAXRDM = 20 >*/
    maxrdm = 20;
/*<       MAXS = 20 >*/
    maxs = 20;
/*<       IF (S.GT.MAXS .OR. S.LT.1) RETURN >*/
    if (*s > maxs || *s < 1) {
	return 0;
    }
/*<       IF (MINORD.LT.0 .OR. MINORD.GE.MAXORD) RETURN >*/
    if (*minord < 0 || *minord >= *maxord) {
	return 0;
    }
/*<       IF (MAXORD.GT.MAXRDM) RETURN >*/
    if (*maxord > maxrdm) {
	return 0;
    }
/*<       ZERO = 0 >*/
    zero = 0.;
/*<       ONE = 1 >*/
    one = 1.;
/*<       TWO = 2 >*/
    two = 2.;
/*<       MOMTOL = ONE >*/
    momtol = one;
/*<    10 MOMTOL = MOMTOL/TWO >*/
L10:
    momtol /= two;
/*<       IF (MOMTOL+ONE.GT.ONE) GO TO 10 >*/
    if (momtol + one > one) {
	goto L10;
    }
/*<       HUNDRD = 100 >*/
    hundrd = 100.;
/*<       MOMTOL = HUNDRD*TWO*MOMTOL >*/
    momtol = hundrd * two * momtol;
/*<       D = MINORD >*/
    d__ = *minord;
/*<       IF (D.EQ.0) INTCLS = 0 >*/
    if (d__ == 0) {
	*intcls = 0;
    }
/* ***  CALCULATE MOMENTS AND MODIFIED MOMENTS */
/*<       DO 20 L=1,MAXORD >*/
    i__1 = *maxord;
    for (l = 1; l <= i__1; ++l) {
/*<         FLOATL = L + L - 1 >*/
	floatl = (doublereal) (l + l - 1);
/*<         MOMENT(L) = TWO/FLOATL >*/
	moment[l - 1] = two / floatl;
/*<    20 CONTINUE >*/
/* L20: */
    }
/*<       IF (MAXORD.EQ.1) GO TO 50 >*/
    if (*maxord == 1) {
	goto L50;
    }
/*<       DO 40 L=2,MAXORD >*/
    i__1 = *maxord;
    for (l = 2; l <= i__1; ++l) {
/*<         INTMPA = MOMENT(L-1) >*/
	intmpa = moment[l - 2];
/*<         GLSQRD = G(L-1)**2 >*/
/* Computing 2nd power */
	d__1 = g[l - 2];
	glsqrd = d__1 * d__1;
/*<         DO 30 I=L,MAXORD >*/
	i__2 = *maxord;
	for (i__ = l; i__ <= i__2; ++i__) {
/*<           INTMPB = MOMENT(I) >*/
	    intmpb = moment[i__ - 1];
/*<           MOMENT(I) = MOMENT(I) - GLSQRD*INTMPA >*/
	    moment[i__ - 1] -= glsqrd * intmpa;
/*<           INTMPA = INTMPB >*/
	    intmpa = intmpb;
/*<    30   CONTINUE >*/
/* L30: */
	}
/*<         IF (MOMENT(L)**2.LT.(MOMTOL*MOMENT(1))**2) MOMENT(L) = ZERO >*/
/* Computing 2nd power */
	d__1 = moment[l - 1];
/* Computing 2nd power */
	d__2 = momtol * moment[0];
	if (d__1 * d__1 < d__2 * d__2) {
	    moment[l - 1] = zero;
	}
/*<    40 CONTINUE >*/
/* L40: */
    }
/*<    50 DO 70 L=1,MAXORD >*/
L50:
    i__1 = *maxord;
    for (l = 1; l <= i__1; ++l) {
/*<         IF (G(L).LT.ZERO) RETURN >*/
	if (g[l - 1] < zero) {
	    return 0;
	}
/*<         MOMNKN = ONE >*/
	momnkn = one;
/*<         MOMPRD(L,1) = MOMENT(1) >*/
	momprd[l - 1] = moment[0];
/*<         IF (MAXORD.EQ.1) GO TO 70 >*/
	if (*maxord == 1) {
	    goto L70;
	}
/*<         GLSQRD = G(L)**2 >*/
/* Computing 2nd power */
	d__1 = g[l - 1];
	glsqrd = d__1 * d__1;
/*<         DO 60 I=2,MAXORD >*/
	i__2 = *maxord;
	for (i__ = 2; i__ <= i__2; ++i__) {
/*<           IF (I.LE.L) GISQRD = G(I-1)**2 >*/
	    if (i__ <= l) {
/* Computing 2nd power */
		d__1 = g[i__ - 2];
		gisqrd = d__1 * d__1;
	    }
/*<           IF (I.GT.L) GISQRD = G(I)**2 >*/
	    if (i__ > l) {
/* Computing 2nd power */
		d__1 = g[i__ - 1];
		gisqrd = d__1 * d__1;
	    }
/*<           IF (GLSQRD.EQ.GISQRD) RETURN >*/
	    if (glsqrd == gisqrd) {
		return 0;
	    }
/*<           MOMNKN = MOMNKN/(GLSQRD-GISQRD) >*/
	    momnkn /= glsqrd - gisqrd;
/*<           MOMPRD(L,I) = MOMNKN*MOMENT(I) >*/
	    momprd[l + i__ * 20 - 21] = momnkn * moment[i__ - 1];
/*<    60   CONTINUE >*/
/* L60: */
	}
/*<    70 CONTINUE >*/
L70:
	;
    }
/*<       FAIL = 1 >*/
    *fail = 1;

/* ***  BEGIN LOOP FOR EACH D */
/*      FOR EACH D FIND ALL DISTINCT PARTITIONS M WITH MOD(M))=D */

/*<    80 PRTCNT = 0 >*/
L80:
    prtcnt = 0;
/*<       INTVAL = ZERO >*/
    intval = zero;
/*<       MODOFM = 0 >*/
    modofm = 0;
/*<       CALL NXPRT(PRTCNT, S, M) >*/
    nxprt_(&prtcnt, s, m);
/*<    90 IF (PRTCNT.GT.NUMSMS) RETURN >*/
L90:
    if (prtcnt > *numsms) {
	return 0;
    }

/* ***  CALCULATE WEIGHT FOR PARTITION M AND FULLY SYMMETRIC SUMS */
/* ***     WHEN NECESSARY */

/*<       IF (D.EQ.MODOFM) WEGHTS(PRTCNT) = ZERO >*/
    if (d__ == modofm) {
	weghts[prtcnt] = zero;
    }
/*<       IF (D.EQ.MODOFM) FULSMS(PRTCNT) = ZERO >*/
    if (d__ == modofm) {
	fulsms[prtcnt] = zero;
    }
/*<       FULWGT = WHT(S,MOMENT,M,K,MODOFM,D,MAXRDM,MOMPRD) >*/
    fulwgt = wht_(s, moment, m, k, &modofm, &d__, &maxrdm, momprd);
/*<       SUMCLS = 0 >*/
    sumcls = 0;
/*<    >*/
    if (weghts[prtcnt] == zero && fulwgt != zero) {
	fulsms[prtcnt] = flsm_(s, &center[1], &hwidth[1], moment, m, k, 
		maxord, g, (D_fp)f, &sumcls);
    }
/*<       INTCLS = INTCLS + SUMCLS >*/
    *intcls += sumcls;
/*<       INTVAL = INTVAL + FULWGT*FULSMS(PRTCNT) >*/
    intval += fulwgt * fulsms[prtcnt];
/*<       WEGHTS(PRTCNT) = WEGHTS(PRTCNT) + FULWGT >*/
    weghts[prtcnt] += fulwgt;
/*<       CALL NXPRT(PRTCNT, S, M) >*/
    nxprt_(&prtcnt, s, m);
/*<       IF (M(1).GT.MODOFM) MODOFM = MODOFM + 1 >*/
    if (m[0] > modofm) {
	++modofm;
    }
/*<       IF (MODOFM.LE.D) GO TO 90 >*/
    if (modofm <= d__) {
	goto L90;
    }

/* ***  END LOOP FOR EACH D */
/*<       IF (D.GT.0) INTVAL = INTVLS(D) + INTVAL >*/
    if (d__ > 0) {
	intval = intvls[d__] + intval;
    }
/*<       INTVLS(D+1) = INTVAL >*/
    intvls[d__ + 1] = intval;
/*<       D = D + 1 >*/
    ++d__;
/*<       IF (D.LT.MAXORD) GO TO 80 >*/
    if (d__ < *maxord) {
	goto L80;
    }

/* ***  SET FAILURE PARAMETER AND RETURN */
/*<       FAIL = 0 >*/
    *fail = 0;
/*<       MINORD = MAXORD >*/
    *minord = *maxord;
/*<       RETURN >*/
    return 0;
/*<       END >*/
} /* symrl_ */

/*<    >*/
doublereal wht_(integer *s, doublereal *intrps, integer *m, integer *k, 
	integer *modofm, integer *d__, integer *maxrdm, doublereal *momprd)
{
    /* System generated locals */
    integer momprd_dim1, momprd_offset, i__1;
    doublereal ret_val;

    /* Local variables */
    static integer i__, k1, m1, ki, mi;
    static doublereal zero;

/* ***  SUBROUTINE TO CALCULATE WEIGHT FOR PARTITION M */

/*<       INTEGER S, M(S), K(S), D, MAXRDM, MI, KI, M1, K1, MODOFM >*/
/*<    >*/
/*<       ZERO = 0 >*/
    /* Parameter adjustments */
    --k;
    --m;
    --intrps;
    momprd_dim1 = *maxrdm;
    momprd_offset = 1 + momprd_dim1;
    momprd -= momprd_offset;

    /* Function Body */
    zero = 0.;
/*<       DO 10 I=1,S >*/
    i__1 = *s;
    for (i__ = 1; i__ <= i__1; ++i__) {
/*<         INTRPS(I) = ZERO >*/
	intrps[i__] = zero;
/*<         K(I) = 0 >*/
	k[i__] = 0;
/*<    10 CONTINUE >*/
/* L10: */
    }
/*<       M1 = M(1) + 1 >*/
    m1 = m[1] + 1;
/*<       K1 = D - MODOFM + M1 >*/
    k1 = *d__ - *modofm + m1;
/*<    20 INTRPS(1) = MOMPRD(M1,K1) >*/
L20:
    intrps[1] = momprd[m1 + k1 * momprd_dim1];
/*<       IF (S.EQ.1) GO TO 40 >*/
    if (*s == 1) {
	goto L40;
    }
/*<       DO 30 I=2,S >*/
    i__1 = *s;
    for (i__ = 2; i__ <= i__1; ++i__) {
/*<         MI = M(I) + 1 >*/
	mi = m[i__] + 1;
/*<         KI = K(I) + MI >*/
	ki = k[i__] + mi;
/*<         INTRPS(I) = INTRPS(I) + MOMPRD(MI,KI)*INTRPS(I-1) >*/
	intrps[i__] += momprd[mi + ki * momprd_dim1] * intrps[i__ - 1];
/*<         INTRPS(I-1) = ZERO >*/
	intrps[i__ - 1] = zero;
/*<         K1 = K1 - 1 >*/
	--k1;
/*<         K(I) = K(I) + 1 >*/
	++k[i__];
/*<         IF (K1.GE.M1) GO TO 20 >*/
	if (k1 >= m1) {
	    goto L20;
	}
/*<         K1 = K1 + K(I) >*/
	k1 += k[i__];
/*<         K(I) = 0 >*/
	k[i__] = 0;
/*<    30 CONTINUE >*/
/* L30: */
    }
/*<    40 WHT = INTRPS(S) >*/
L40:
    ret_val = intrps[*s];
/*<       RETURN >*/
    return ret_val;
/*<       END >*/
} /* wht_ */

/*<    >*/
doublereal flsm_(integer *s, doublereal *center, doublereal *hwidth, 
	doublereal *x, integer *m, integer *mp, integer *maxord, doublereal *
	g, D_fp f, integer *sumcls)
{
    /* System generated locals */
    integer i__1, i__2;
    doublereal ret_val;

    /* Local variables */
    static integer i__, l;
    static doublereal one;
    static integer mpi, mpl;
    static doublereal two, zero;
    static integer ihalf, ixchng, lxchng, imnusl;
    static doublereal intwgt, intsum;


/* ***  FUNCTION TO COMPUTE FULLY SYMMETRIC BASIC RULE SUM */

/*<    >*/
/*<    >*/
/*<       ZERO = 0 >*/
    /* Parameter adjustments */
    --mp;
    --m;
    --x;
    --hwidth;
    --center;
    --g;

    /* Function Body */
    zero = 0.;
/*<       ONE = 1 >*/
    one = 1.;
/*<       TWO = 2 >*/
    two = 2.;
/*<       INTWGT = ONE >*/
    intwgt = one;
/*<       DO 10 I=1,S >*/
    i__1 = *s;
    for (i__ = 1; i__ <= i__1; ++i__) {
/*<         MP(I) = M(I) >*/
	mp[i__] = m[i__];
/*<         IF (M(I).NE.0) INTWGT = INTWGT/TWO >*/
	if (m[i__] != 0) {
	    intwgt /= two;
	}
/*<         INTWGT = INTWGT*HWIDTH(I) >*/
	intwgt *= hwidth[i__];
/*<    10 CONTINUE >*/
/* L10: */
    }
/*<       SUMCLS = 0 >*/
    *sumcls = 0;
/*<       FLSM = ZERO >*/
    ret_val = zero;

/* *******  COMPUTE CENTRALLY SYMMETRIC SUM FOR PERMUTATION MP */
/*<    20 INTSUM = ZERO >*/
L20:
    intsum = zero;
/*<       DO 30 I=1,S >*/
    i__1 = *s;
    for (i__ = 1; i__ <= i__1; ++i__) {
/*<         MPI = MP(I) + 1 >*/
	mpi = mp[i__] + 1;
/*<         X(I) = CENTER(I) + G(MPI)*HWIDTH(I) >*/
	x[i__] = center[i__] + g[mpi] * hwidth[i__];
/*<    30 CONTINUE >*/
/* L30: */
    }
/*<    40 SUMCLS = SUMCLS + 1 >*/
L40:
    ++(*sumcls);
/*<       INTSUM = INTSUM + F(S,X) >*/
    intsum += (*f)(s, &x[1]);
/*<       DO 50 I=1,S >*/
    i__1 = *s;
    for (i__ = 1; i__ <= i__1; ++i__) {
/*<         MPI = MP(I) + 1 >*/
	mpi = mp[i__] + 1;
/*<         IF(G(MPI).NE.ZERO) HWIDTH(I) = -HWIDTH(I) >*/
	if (g[mpi] != zero) {
	    hwidth[i__] = -hwidth[i__];
	}
/*<         X(I) = CENTER(I) + G(MPI)*HWIDTH(I) >*/
	x[i__] = center[i__] + g[mpi] * hwidth[i__];
/*<         IF (X(I).LT.CENTER(I)) GO TO 40 >*/
	if (x[i__] < center[i__]) {
	    goto L40;
	}
/*<    50 CONTINUE >*/
/* L50: */
    }
/* *******  END INTEGRATION LOOP FOR MP */

/*<       FLSM = FLSM + INTWGT*INTSUM >*/
    ret_val += intwgt * intsum;
/*<       IF (S.EQ.1) RETURN >*/
    if (*s == 1) {
	return ret_val;
    }

/* *******  FIND NEXT DISTINCT PERMUTATION OF M AND LOOP BACK */
/*          TO COMPUTE NEXT CENTRALLY SYMMETRIC SUM */
/*<       DO 80 I=2,S >*/
    i__1 = *s;
    for (i__ = 2; i__ <= i__1; ++i__) {
/*<         IF (MP(I-1).LE.MP(I)) GO TO 80 >*/
	if (mp[i__ - 1] <= mp[i__]) {
	    goto L80;
	}
/*<         MPI = MP(I) >*/
	mpi = mp[i__];
/*<         IXCHNG = I - 1 >*/
	ixchng = i__ - 1;
/*<         IF (I.EQ.2) GO TO 70 >*/
	if (i__ == 2) {
	    goto L70;
	}
/*<         IHALF = IXCHNG/2 >*/
	ihalf = ixchng / 2;
/*<         DO 60 L=1,IHALF >*/
	i__2 = ihalf;
	for (l = 1; l <= i__2; ++l) {
/*<           MPL = MP(L) >*/
	    mpl = mp[l];
/*<           IMNUSL = I - L >*/
	    imnusl = i__ - l;
/*<           MP(L) = MP(IMNUSL) >*/
	    mp[l] = mp[imnusl];
/*<           MP(IMNUSL) = MPL >*/
	    mp[imnusl] = mpl;
/*<           IF (MPL.LE.MPI) IXCHNG = IXCHNG - 1 >*/
	    if (mpl <= mpi) {
		--ixchng;
	    }
/*<           IF (MP(L).GT.MPI) LXCHNG = L >*/
	    if (mp[l] > mpi) {
		lxchng = l;
	    }
/*<    60   CONTINUE >*/
/* L60: */
	}
/*<         IF (MP(IXCHNG).LE.MPI) IXCHNG = LXCHNG >*/
	if (mp[ixchng] <= mpi) {
	    ixchng = lxchng;
	}
/*<    70   MP(I) = MP(IXCHNG) >*/
L70:
	mp[i__] = mp[ixchng];
/*<         MP(IXCHNG) = MPI >*/
	mp[ixchng] = mpi;
/*<         GO TO 20 >*/
	goto L20;
/*<    80 CONTINUE >*/
L80:
	;
    }
/* *****  END LOOP FOR PERMUTATIONS OF M AND ASSOCIATED SUMS */

/*<       RETURN >*/
    return ret_val;
/*<       END >*/
} /* flsm_ */

/*<       SUBROUTINE NXPRT(PRTCNT, S, M) >*/
/* Subroutine */ int nxprt_(integer *prtcnt, integer *s, integer *m)
{
    /* System generated locals */
    integer i__1, i__2;

    /* Local variables */
    static integer i__, l, msum;


/* ***  SUBROUTINE TO COMPUTE THE NEXT S PARTITION */

/*<       INTEGER S, M(S), PRTCNT, I, MSUM >*/
/*<       IF (PRTCNT.GT.0) GO TO 20 >*/
    /* Parameter adjustments */
    --m;

    /* Function Body */
    if (*prtcnt > 0) {
	goto L20;
    }
/*<       DO 10 I=1,S >*/
    i__1 = *s;
    for (i__ = 1; i__ <= i__1; ++i__) {
/*<         M(I) = 0 >*/
	m[i__] = 0;
/*<    10 CONTINUE >*/
/* L10: */
    }
/*<       PRTCNT = 1 >*/
    *prtcnt = 1;
/*<       RETURN >*/
    return 0;
/*<    20 PRTCNT = PRTCNT + 1 >*/
L20:
    ++(*prtcnt);
/*<       MSUM = M(1) >*/
    msum = m[1];
/*<       IF (S.EQ.1) GO TO 60 >*/
    if (*s == 1) {
	goto L60;
    }
/*<       DO 50 I=2,S >*/
    i__1 = *s;
    for (i__ = 2; i__ <= i__1; ++i__) {
/*<         MSUM = MSUM + M(I) >*/
	msum += m[i__];
/*<         IF (M(1).LE.M(I)+1) GO TO 40 >*/
	if (m[1] <= m[i__] + 1) {
	    goto L40;
	}
/*<         M(1) = MSUM - (I-1)*(M(I)+1) >*/
	m[1] = msum - (i__ - 1) * (m[i__] + 1);
/*<         DO 30 L=2,I >*/
	i__2 = i__;
	for (l = 2; l <= i__2; ++l) {
/*<           M(L) = M(I) + 1 >*/
	    m[l] = m[i__] + 1;
/*<    30   CONTINUE >*/
/* L30: */
	}
/*<         RETURN >*/
	return 0;
/*<    40   M(I) = 0 >*/
L40:
	m[i__] = 0;
/*<    50 CONTINUE >*/
/* L50: */
    }
/*<    60 M(1) = MSUM + 1 >*/
L60:
    m[1] = msum + 1;
/*<       RETURN >*/
    return 0;
/*<       END >*/
} /* nxprt_ */

