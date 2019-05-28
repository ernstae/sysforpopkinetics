      SUBROUTINE ADAPT(NDIM,A,B,MINPTS,MAXPTS,FUNCTN,EPS,RELERR,LENWRK,
     * WRKSTR,FINEST,IFAIL)
C***BEGIN PROLOGUE ADAPT
C  ADAPTIVE MULTIDIMENSIONAL INTEGRATION SUBROUTINE
C           AUTHOR: A. C. GENZ, Washington State University
C                    19 March 1984
C**************  PARAMETERS FOR ADAPT  ********************************
C***** INPUT PARAMETERS
C  NDIM    NUMBER OF VARIABLES, MUST EXCEED 1, BUT NOT EXCEED 20
C  A       REAL ARRAY OF LOWER LIMITS, WITH DIMENSION NDIM
C  B       REAL ARRAY OF UPPER LIMITS, WITH DIMENSION NDIM
C  MINPTS  MINIMUM NUMBER OF FUNCTION EVALUATIONS TO BE ALLOWED.
C          ON THE FIRST CALL TO ADAPT MINPTS SHOULD BE SET TO A
C          NON NEGATIVE VALUE. (CAUTION... MINPTS IS ALTERED BY ADAPT)
C          IT IS POSSIBLE TO CONTINUE A CALCULATION TO GREATER ACCURACY
C          BY CALLING ADAPT AGAIN BY DECREASING EPS (DESCRIBED BELOW)
C          AND RESETTING MINPTS TO ANY NEGATIVE VALUE.
C          MINPTS MUST NOT EXCEED MAXPTS.
C  MAXPTS  MAXIMUM NUMBER OF FUNCTION EVALUATIONS TO BE ALLOWED,
C          WHICH MUST BE AT LEAST RULCLS, WHERE
C          RULCLS =  2**NDIM+2*NDIM**2+6*NDIM+1
C
C            FOR NDIM =  2   3   4   5   6   7   8   9   10
C            MAXPTS >=  25  45  73 113 173 269 433 729 1285
C         A suggested value for MAXPTS is 100 times the above values.
C
C  FUNCTN  EXTERNALLY DECLARED USER DEFINED FUNCTION TO BE INTEGRATED.
C          IT MUST HAVE PARAMETERS (NDIM,Z), WHERE Z IS A REAL ARRAY
C          OF DIMENSION NDIM.
C  EPS     REQUIRED RELATIVE ACCURACY
C  LENWRK  LENGTH OF ARRAY WRKSTR OF WORKING STORAGE, THE ROUTINE
C          NEEDS (2*NDIM+3)*(1+MAXPTS/RULCLS)/2 FOR LENWRK IF
C          MAXPTS FUNCTION CALLS ARE USED.
C          FOR GUIDANCE, IF YOU SET MAXPTS TO 100*RULCLS (SEE TABLE
C          ABOVE) THEN ACCEPTABLE VALUES FOR LENWRK ARE
C
C            FOR NDIM = 2    3    4    5    6    7    8     9
C            LENWRK =  357  561  1785 3417 6681 13209 26265 52377
C
C***** OUTPUT PARAMETERS
C  MINPTS  ACTUAL NUMBER OF FUNCTION EVALUATIONS USED BY ADAPT
C  WRKSTR  REAL ARRAY OF WORKING STORAGE OF DIMENSION (LENWRK).
C  RELERR  ESTIMATED RELATIVE ACCURACY OF FINEST
C  FINEST  ESTIMATED VALUE OF INTEGRAL
C  IFAIL   IFAIL=0 FOR NORMAL EXIT, WHEN ESTIMATED RELATIVE ACCURACY
C                  RELERR IS LESS THAN EPS WITH MAXPTS OR LESS FUNCTION
C                  CALLS MADE.
C          IFAIL=1 IF MAXPTS WAS TOO SMALL FOR ADAPT TO OBTAIN THE
C                  REQUIRED RELATIVE ACCURACY EPS.  IN THIS CASE ADAPT
C                  RETURNS A VALUE OF FINEST WITH ESTIMATED RELATIVE
C                  ACCURACY RELERR.
C          IFAIL=2 IF LENWRK TOO SMALL FOR MAXPTS FUNCTION CALLS.  IN
C                  THIS CASE ADAPT RETURNS A VALUE OF FINEST WITH
C                  ESTIMATED ACCURACY RELERR USING THE WORKING STORAGE
C                  AVAILABLE, BUT RELERR WILL BE GREATER THAN EPS.
C          IFAIL=3 IF NDIM ) 2, NDIM \ 20, MINPTS \ MAXPTS,
C                  OR MAXPTS ) RULCLS.
C***********************************************************************
C***END PROLOGUE ADAPT
      EXTERNAL FUNCTN
C*****  FOR DOUBLE PRECISION CHANGE REAL TO DOUBLE PRECISION IN THE
C        NEXT STATEMENT.
      DOUBLE PRECISION
     +     A(NDIM), B(NDIM), CENTER(20),
     * DIFMAX, EPS, ERRMIN, FINEST, HALF,
     * ONE, RELERR, RGNERR, RGNVAL,
     * TWO, WIDTH(20), WRKSTR(LENWRK), ZERO
      INTEGER DIVAXO, DIVAXN, DIVFLG, FUNCLS, IFAIL, INDEX1,
     * INDEX2, J, K, LENWRK, MAXCLS, MAXPTS, MINPTS, NDIM,
     * RGNSTR, RULCLS, SBRGNS, SBTMPP, SUBRGN, SUBTMP
      IFAIL=3
      RELERR=1
      FUNCLS=0
      IF(NDIM.LT.2.OR.NDIM.GT.20) GOTO 300
      IF(MINPTS.GT.MAXPTS) GOTO 300
C
C*****  INITIALISATION OF SUBROUTINE
C
      ZERO=0
      ONE=1
      TWO=2
      HALF=ONE/TWO
      RGNSTR=2*NDIM+3
      ERRMIN = ZERO
      MAXCLS =  2**NDIM+2*NDIM**2+6*NDIM+1
      MAXCLS = MIN0(MAXCLS,MAXPTS)
      DIVAXO=0
C
C*****  END SUBROUTINE INITIALISATION
      IF(MINPTS.LT.0) SBRGNS=WRKSTR(LENWRK-1)
      IF(MINPTS.LT.0) GOTO 280
      DO 30 J=1,NDIM
        WIDTH(J)=(B(J)-A(J))*HALF
   30   CENTER(J)=A(J)+WIDTH(J)
      FINEST=ZERO
      WRKSTR(LENWRK)=ZERO
      DIVFLG=1
      SUBRGN=RGNSTR
      SBRGNS=RGNSTR
   40 CALL BSRL(NDIM,CENTER,WIDTH,FUNCTN,MAXCLS,RULCLS,
     *            ERRMIN,RGNERR,RGNVAL,DIVAXO,DIVAXN)
      FINEST=FINEST+RGNVAL
      WRKSTR(LENWRK)=WRKSTR(LENWRK)+RGNERR
      FUNCLS = FUNCLS + RULCLS
C
C*****  PLACE RESULTS OF BASIC RULE INTO PARTIALLY ORDERED LIST
C*****  ACCORDING TO SUBREGION ERROR
      IF(DIVFLG.EQ.1) GO TO 230
C
C*****  WHEN DIVFLG=0 START AT TOP OF LIST AND MOVE DOWN LIST TREE TO
C       FIND CORRECT POSITION FOR RESULTS FROM FIRST HALF OF RECENTLY
C       DIVIDED SUBREGION
  200 SUBTMP=2*SUBRGN
      IF(SUBTMP.GT.SBRGNS) GO TO 250
       IF(SUBTMP.EQ.SBRGNS) GO TO 210
       SBTMPP=SUBTMP+RGNSTR
       IF(WRKSTR(SUBTMP).LT.WRKSTR(SBTMPP)) SUBTMP=SBTMPP
  210  IF(RGNERR.GE.WRKSTR(SUBTMP)) GO TO 250
        DO 220 K=1,RGNSTR
          INDEX1=SUBRGN-K+1
          INDEX2=SUBTMP-K+1
  220     WRKSTR(INDEX1)=WRKSTR(INDEX2)
        SUBRGN=SUBTMP
      GOTO 200
C
C*****  WHEN DIVFLG=1 START AT BOTTOM RIGHT BRANCH AND MOVE UP LIST
C       TREE TO FIND CORRECT POSITION FOR RESULTS FROM SECOND HALF OF
C       RECENTLY DIVIDED SUBREGION
  230 SUBTMP=(SUBRGN/(RGNSTR*2))*RGNSTR
      IF(SUBTMP.LT.RGNSTR) GO TO 250
      IF(RGNERR.LE.WRKSTR(SUBTMP)) GO TO 250
       DO 240 K=1,RGNSTR
         INDEX1=SUBRGN-K+1
         INDEX2=SUBTMP-K+1
  240    WRKSTR(INDEX1)=WRKSTR(INDEX2)
       SUBRGN=SUBTMP
      GOTO 230
C*****  STORE RESULTS OF BASIC RULE IN CORRECT POSITION IN LIST
  250 WRKSTR(SUBRGN)=RGNERR
      WRKSTR(SUBRGN-1)=RGNVAL
      WRKSTR(SUBRGN-2)=DIVAXN
      DO 260 J=1,NDIM
        SUBTMP=SUBRGN-2*(J+1)
        WRKSTR(SUBTMP+1)=CENTER(J)
  260   WRKSTR(SUBTMP)=WIDTH(J)
      IF(DIVFLG.EQ.1) GO TO 270
C*****  WHEN DIVFLG=0 PREPARE FOR SECOND APPLICATION OF BASIC RULE
      CENTER(DIVAXO)=CENTER(DIVAXO)+TWO*WIDTH(DIVAXO)
      SBRGNS=SBRGNS+RGNSTR
      SUBRGN=SBRGNS
      DIVFLG=1
C*****  LOOP BACK TO APPLY BASIC RULE TO OTHER HALF OF SUBREGION
      GO TO 40
C
C*****  END ORDERING AND STORAGE OF BASIC RULE RESULTS
C*****  MAKE CHECKS FOR POSSIBLE TERMINATION OF ROUTINE
C
C******  FOR DOUBLE PRECISION CHANGE ABS TO DABS IN THE NEXT STATEMENT
  270 RELERR=ONE
      IF(WRKSTR(LENWRK).LE.ZERO) WRKSTR(LENWRK)=ZERO
      IF(ABS(FINEST).NE.ZERO) RELERR=WRKSTR(LENWRK)/ABS(FINEST)
      IF(RELERR.GT.ONE) RELERR=ONE
      IF(SBRGNS+RGNSTR.GT.LENWRK-2) IFAIL=2
      IF(FUNCLS+FUNCLS*RGNSTR/SBRGNS.GT.MAXPTS) IFAIL=1
      IF(RELERR.LT.EPS.AND.FUNCLS.GE.MINPTS) IFAIL=0
      IF(IFAIL.LT.3) GOTO 300
C
C*****  PREPARE TO USE BASIC RULE ON EACH HALF OF SUBREGION WITH LARGEST
C       ERROR
  280 DIVFLG=0
      SUBRGN=RGNSTR
      SUBTMP = 2*SBRGNS/RGNSTR
      MAXCLS = MAXPTS/SUBTMP
      ERRMIN = ABS(FINEST)*EPS/FLOAT(SUBTMP)
      WRKSTR(LENWRK)=WRKSTR(LENWRK)-WRKSTR(SUBRGN)
      FINEST=FINEST-WRKSTR(SUBRGN-1)
      DIVAXO=WRKSTR(SUBRGN-2)
      DO 290 J=1,NDIM
        SUBTMP=SUBRGN-2*(J+1)
        CENTER(J)=WRKSTR(SUBTMP+1)
  290   WIDTH(J)=WRKSTR(SUBTMP)
      WIDTH(DIVAXO)=WIDTH(DIVAXO)*HALF
      CENTER(DIVAXO)=CENTER(DIVAXO)-WIDTH(DIVAXO)
C
C*****  LOOP BACK TO APPLY BASIC RULE
C
      GOTO 40
C
C*****  TERMINATION POINT
C
  300 MINPTS=FUNCLS
      WRKSTR(LENWRK-1)=SBRGNS
      RETURN
      END
      SUBROUTINE BSRL(S,CENTER,HWIDTH,F,MAXVLS,FUNCLS,
     *                  ERRMIN,ERREST,BASEST,DIVAXO,DIVAXN)
      EXTERNAL F
      INTEGER S, DIVAXN, DIVAXO, FUNCLS, INTCLS, I, MINDEG, MAXDEG,
     * MAXORD, MINORD, MAXCLS
      DOUBLE PRECISION
     +     ONE, TWO, THREE, FIVE, TEN, DIF, ERRORM, ERRMIN,
     * CENTER(S), HWIDTH(S), SUM0, SUM1, SUM2, DIFMAX, X1, X2,
     * INTVLS(20), Z(20), FULSMS(200), WEGHTS(200), ERREST, BASEST
      MAXDEG = 12
      MINDEG = 4
      MINORD = 0
      ZERO = 0
      ONE = 1
      TWO = 2
      THREE = 3
      FIVE = 5
      TEN = 10
      DO 10 MAXORD = MINDEG,MAXDEG
        CALL SYMRL(S, CENTER, HWIDTH, F, MINORD, MAXORD, INTVLS,
     *   INTCLS, 200, WEGHTS, FULSMS, IFAIL)
        IF (IFAIL.EQ.2) GOTO 20
        ERREST = ABS(INTVLS(MAXORD)-INTVLS(MAXORD-1))
        ERRORM = ABS(INTVLS(MAXORD-1)-INTVLS(MAXORD-2))
        IF (ERREST.NE.ZERO)
     *  ERREST = ERREST*DMAX1(ONE/TEN,ERREST/DMAX1(ERREST/TWO,ERRORM))
        IF (ERRORM.LE.FIVE*ERREST) GOTO 20
        IF (2*INTCLS.GT.MAXVLS) GOTO 20
        IF (ERREST.LT.ERRMIN) GOTO 20
   10   CONTINUE
   20 DIFMAX = -1
      X1 = ONE/TWO**2
      X2 = THREE*X1
      DO 30 I = 1,S
       Z(I) = CENTER(I)
   30  CONTINUE
      SUM0 = F(S,Z)
      DO 40 I = 1,S
       Z(I) = CENTER(I) - X1*HWIDTH(I)
       SUM1 = F(S,Z)
       Z(I) = CENTER(I) + X1*HWIDTH(I)
       SUM1 = SUM1 + F(S,Z)
       Z(I) = CENTER(I) - X2*HWIDTH(I)
       SUM2 = F(S,Z)
       Z(I) = CENTER(I) + X2*HWIDTH(I)
       SUM2 = SUM2 + F(S,Z)
       Z(I) = CENTER(I)
       DIF = ABS((SUM1-TWO*SUM0) - (X1/X2)**2*(SUM2-TWO*SUM0))
       IF (DIF.LT.DIFMAX) GOTO 40
        DIFMAX = DIF
        DIVAXN = I
   40  CONTINUE
       IF (SUM0.EQ.SUM0+DIFMAX/TWO) DIVAXN = MOD(DIVAXO,S) + 1
      BASEST = INTVLS(MINORD)
      FUNCLS = INTCLS + 4*S
      RETURN
      END
      SUBROUTINE SYMRL(S, CENTER, HWIDTH, F, MINORD, MAXORD, INTVLS,
     * INTCLS, NUMSMS, WEGHTS, FULSMS, FAIL)
C  MULTIDIMENSIONAL FULLY SYMMETRIC RULE INTEGRATION SUBROUTINE
C
C   THIS SUBROUTINE COMPUTES A SEQUENCE OF FULLY SYMMETRIC RULE
C   APPROXIMATIONS TO A FULLY SYMMETRIC MULTIPLE INTEGRAL.
C   WRITTEN BY A. GENZ, MATHEMATICAL INSTITUTE, UNIVERSITY OF KENT,
C   CANTERBURY, KENT CT2 7NF, ENGLAND
C
C**************  PARAMETERS FOR SYMRL  ********************************
C*****INPUT PARAMETERS
C  S       INTEGER NUMBER OF VARIABLES, MUST EXCEED 0 BUT NOT EXCEED 20
C  F       EXTERNALLY DECLARED USER DEFINED REAL FUNCTION INTEGRAND.
C          IT MUST HAVE PARAMETERS (S,X), WHERE X IS A REAL ARRAY
C          WITH DIMENSION S.
C  MINORD  INTEGER MINIMUM ORDER PARAMETER.  ON ENTRY MINORD SPECIFIES
C          THE CURRENT HIGHEST ORDER APPROXIMATION TO THE INTEGRAL,
C          AVAILABLE IN THE ARRAY INTVLS.  FOR THE FIRST CALL OF SYMRL
C          MINORD SHOULD BE SET TO 0.  OTHERWISE A PREVIOUS CALL IS
C          ASSUMED THAT COMPUTED INTVLS(1), ... , INTVLS(MINORD).
C          ON EXIT MINORD IS SET TO MAXORD.
C  MAXORD  INTEGER MAXIMUM ORDER PARAMETER, MUST BE GREATER THAN MINORD
C          AND NOT EXCEED 20. THE SUBROUTINE COMPUTES INTVLS(MINORD+1),
C          INTVLS(MINORD+2),..., INTVLS(MAXORD).
C  G       REAL ARRAY OF DIMENSION(MAXORD) OF GENERATORS.
C          ALL GENERATORS MUST BE DISTINCT AND NONNEGATIVE.
C  NUMSMS  INTEGER LENGTH OF ARRAY FULSMS, MUST BE AT LEAST THE SUM OF
C          THE NUMBER OF DISTINCT PARTITIONS OF LENGTH AT MOST S
C          OF THE INTEGERS 0,1,...,MAXORD-1.  AN UPPER BOUND FOR NUMSMS
C          WHEN S+MAXORD IS LESS THAN 19 IS 200
C******OUTPUT PARAMETERS
C  INTVLS  REAL ARRAY OF DIMENSION(MAXORD).  UPON SUCCESSFUL EXIT
C          INTVLS(1), INTVLS(2),..., INTVLS(MAXORD) ARE APPROXIMATIONS
C          TO THE INTEGRAL.  INTVLS(D+1) WILL BE AN APPROXIMATION OF
C          POLYNOMIAL DEGREE 2D+1.
C  INTCLS  INTEGER TOTAL NUMBER OF F VALUES NEEDED FOR INTVLS(MAXORD)
C  WEGHTS  REAL WORKING STORAGE ARRAY WITH DIMENSION (NUMSMS). ON EXIT
C          WEGHTS(J) CONTAINS THE WEIGHT FOR FULSMS(J).
C  FULSMS  REAL WORKING STORAGE ARRAY WITH DIMENSION (NUMSMS). ON EXIT
C          FULSMS(J) CONTAINS THE FULLY SYMMETRIC BASIC RULE SUM
C          INDEXED BY THE JTH S-PARTITION OF THE INTEGERS
C          0,1,...,MAXORD-1.
C  FAIL    INTEGER FAILURE OUTPUT PARAMETER
C          FAIL=0 FOR SUCCESSFUL TERMINATION OF THE SUBROUTINE
C          FAIL=1 WHEN NUMSMS IS TOO SMALL FOR THE SUBROUTINE TO
C                  CONTINUE.  IN THIS CASE WEGHTS(1), WEGHTS(2), ...,
C                  WEGHTS(NUMSMS), FULSMS(1), FULSMS(2), ...,
C                  FULSMS(NUMSMS) AND INTVLS(1), INTVLS(2),...,
C                  INTVLS(J) ARE RETURNED, WHERE J IS MAXIMUM VALUE OF
C                  MAXORD COMPATIBLE WITH THE GIVEN VALUE OF NUMSMS.
C          FAIL=2 WHEN PARAMETERS S,MINORD, MAXORD OR G ARE OUT OF
C                  RANGE
C***********************************************************************
      EXTERNAL F
C***  FOR DOUBLE PRECISION CHANGE REAL TO DOUBLE PRECISION
C      IN THE NEXT STATEMENT
      INTEGER D, I, FAIL, K(20), INTCLS, PRTCNT, L, M(20), MAXORD,
     * MINORD, MODOFM, NUMSMS, S, SUMCLS
      DOUBLE PRECISION
     +     INTVLS(MAXORD), CENTER(S), HWIDTH(S), GISQRD, GLSQRD,
     * INTMPA, INTMPB, INTVAL, ONE, FULSMS(NUMSMS), WEGHTS(NUMSMS),
     * TWO, MOMTOL, MOMNKN, MOMPRD(20,20), MOMENT(20), ZERO, G(20)
C       PATTERSON GENERATORS
      DATA G(1), G(2) /0.0000000000000000,0.7745966692414833/
      DATA G(3), G(4) /0.9604912687080202,0.4342437493468025/
      DATA G(5), G(6) /0.9938319632127549,0.8884592328722569/
      DATA G(7), G(8) /0.6211029467372263,0.2233866864289668/
      DATA G(9), G(10), G(11), G(12) /0.1, 0.2, 0.3, 0.4/
C
C***  PARAMETER CHECKING AND INITIALISATION
      FAIL = 2
      MAXRDM = 20
      MAXS = 20
      IF (S.GT.MAXS .OR. S.LT.1) RETURN
      IF (MINORD.LT.0 .OR. MINORD.GE.MAXORD) RETURN
      IF (MAXORD.GT.MAXRDM) RETURN
      ZERO = 0
      ONE = 1
      TWO = 2
      MOMTOL = ONE
   10 MOMTOL = MOMTOL/TWO
      IF (MOMTOL+ONE.GT.ONE) GO TO 10
      HUNDRD = 100
      MOMTOL = HUNDRD*TWO*MOMTOL
      D = MINORD
      IF (D.EQ.0) INTCLS = 0
C***  CALCULATE MOMENTS AND MODIFIED MOMENTS
      DO 20 L=1,MAXORD
        FLOATL = L + L - 1
        MOMENT(L) = TWO/FLOATL
   20 CONTINUE
      IF (MAXORD.EQ.1) GO TO 50
      DO 40 L=2,MAXORD
        INTMPA = MOMENT(L-1)
        GLSQRD = G(L-1)**2
        DO 30 I=L,MAXORD
          INTMPB = MOMENT(I)
          MOMENT(I) = MOMENT(I) - GLSQRD*INTMPA
          INTMPA = INTMPB
   30   CONTINUE
        IF (MOMENT(L)**2.LT.(MOMTOL*MOMENT(1))**2) MOMENT(L) = ZERO
   40 CONTINUE
   50 DO 70 L=1,MAXORD
        IF (G(L).LT.ZERO) RETURN
        MOMNKN = ONE
        MOMPRD(L,1) = MOMENT(1)
        IF (MAXORD.EQ.1) GO TO 70
        GLSQRD = G(L)**2
        DO 60 I=2,MAXORD
          IF (I.LE.L) GISQRD = G(I-1)**2
          IF (I.GT.L) GISQRD = G(I)**2
          IF (GLSQRD.EQ.GISQRD) RETURN
          MOMNKN = MOMNKN/(GLSQRD-GISQRD)
          MOMPRD(L,I) = MOMNKN*MOMENT(I)
   60   CONTINUE
   70 CONTINUE
      FAIL = 1
C
C***  BEGIN LOOP FOR EACH D
C      FOR EACH D FIND ALL DISTINCT PARTITIONS M WITH MOD(M))=D
C
   80 PRTCNT = 0
      INTVAL = ZERO
      MODOFM = 0
      CALL NXPRT(PRTCNT, S, M)
   90 IF (PRTCNT.GT.NUMSMS) RETURN
C
C***  CALCULATE WEIGHT FOR PARTITION M AND FULLY SYMMETRIC SUMS
C***     WHEN NECESSARY
C
      IF (D.EQ.MODOFM) WEGHTS(PRTCNT) = ZERO
      IF (D.EQ.MODOFM) FULSMS(PRTCNT) = ZERO
      FULWGT = WHT(S,MOMENT,M,K,MODOFM,D,MAXRDM,MOMPRD)
      SUMCLS = 0
      IF (WEGHTS(PRTCNT).EQ.ZERO .AND. FULWGT.NE.ZERO) FULSMS(PRTCNT) =
     * FLSM(S, CENTER, HWIDTH, MOMENT, M, K, MAXORD, G, F, SUMCLS)
      INTCLS = INTCLS + SUMCLS
      INTVAL = INTVAL + FULWGT*FULSMS(PRTCNT)
      WEGHTS(PRTCNT) = WEGHTS(PRTCNT) + FULWGT
      CALL NXPRT(PRTCNT, S, M)
      IF (M(1).GT.MODOFM) MODOFM = MODOFM + 1
      IF (MODOFM.LE.D) GO TO 90
C
C***  END LOOP FOR EACH D
      IF (D.GT.0) INTVAL = INTVLS(D) + INTVAL
      INTVLS(D+1) = INTVAL
      D = D + 1
      IF (D.LT.MAXORD) GO TO 80
C
C***  SET FAILURE PARAMETER AND RETURN
      FAIL = 0
      MINORD = MAXORD
      RETURN
      END
      DOUBLE PRECISION
     +     FUNCTION WHT(S, INTRPS, M, K, MODOFM, D, MAXRDM, MOMPRD)
C***  SUBROUTINE TO CALCULATE WEIGHT FOR PARTITION M
C
      INTEGER S, M(S), K(S), D, MAXRDM, MI, KI, M1, K1, MODOFM
      DOUBLE PRECISION
     +     INTRPS(S), ZERO, MOMPRD(MAXRDM,MAXRDM)
      ZERO = 0
      DO 10 I=1,S
        INTRPS(I) = ZERO
        K(I) = 0
   10 CONTINUE
      M1 = M(1) + 1
      K1 = D - MODOFM + M1
   20 INTRPS(1) = MOMPRD(M1,K1)
      IF (S.EQ.1) GO TO 40
      DO 30 I=2,S
        MI = M(I) + 1
        KI = K(I) + MI
        INTRPS(I) = INTRPS(I) + MOMPRD(MI,KI)*INTRPS(I-1)
        INTRPS(I-1) = ZERO
        K1 = K1 - 1
        K(I) = K(I) + 1
        IF (K1.GE.M1) GO TO 20
        K1 = K1 + K(I)
        K(I) = 0
   30 CONTINUE
   40 WHT = INTRPS(S)
      RETURN
      END
      DOUBLE PRECISION
     +     FUNCTION FLSM(S,CENTER,HWIDTH,X,M,MP,MAXORD,G,F,SUMCLS)
C
C***  FUNCTION TO COMPUTE FULLY SYMMETRIC BASIC RULE SUM
C
      INTEGER S, M(S), MP(S), MAXORD, SUMCLS, IXCHNG, LXCHNG, I, L,
     * IHALF, MPI, MPL
      DOUBLE PRECISION
     +     G(MAXORD), X(S), INTWGT, ZERO, ONE, TWO, INTSUM,
     * CENTER(S), HWIDTH(S)
      ZERO = 0
      ONE = 1
      TWO = 2
      INTWGT = ONE
      DO 10 I=1,S
        MP(I) = M(I)
        IF (M(I).NE.0) INTWGT = INTWGT/TWO
        INTWGT = INTWGT*HWIDTH(I)
   10 CONTINUE
      SUMCLS = 0
      FLSM = ZERO
C
C*******  COMPUTE CENTRALLY SYMMETRIC SUM FOR PERMUTATION MP
   20 INTSUM = ZERO
      DO 30 I=1,S
        MPI = MP(I) + 1
        X(I) = CENTER(I) + G(MPI)*HWIDTH(I)
   30 CONTINUE
   40 SUMCLS = SUMCLS + 1
      INTSUM = INTSUM + F(S,X)
      DO 50 I=1,S
        MPI = MP(I) + 1
        IF(G(MPI).NE.ZERO) HWIDTH(I) = -HWIDTH(I)
        X(I) = CENTER(I) + G(MPI)*HWIDTH(I)
        IF (X(I).LT.CENTER(I)) GO TO 40
   50 CONTINUE
C*******  END INTEGRATION LOOP FOR MP
C
      FLSM = FLSM + INTWGT*INTSUM
      IF (S.EQ.1) RETURN
C
C*******  FIND NEXT DISTINCT PERMUTATION OF M AND LOOP BACK
C          TO COMPUTE NEXT CENTRALLY SYMMETRIC SUM
      DO 80 I=2,S
        IF (MP(I-1).LE.MP(I)) GO TO 80
        MPI = MP(I)
        IXCHNG = I - 1
        IF (I.EQ.2) GO TO 70
        IHALF = IXCHNG/2
        DO 60 L=1,IHALF
          MPL = MP(L)
          IMNUSL = I - L
          MP(L) = MP(IMNUSL)
          MP(IMNUSL) = MPL
          IF (MPL.LE.MPI) IXCHNG = IXCHNG - 1
          IF (MP(L).GT.MPI) LXCHNG = L
   60   CONTINUE
        IF (MP(IXCHNG).LE.MPI) IXCHNG = LXCHNG
   70   MP(I) = MP(IXCHNG)
        MP(IXCHNG) = MPI
        GO TO 20
   80 CONTINUE
C*****  END LOOP FOR PERMUTATIONS OF M AND ASSOCIATED SUMS
C
      RETURN
      END
      SUBROUTINE NXPRT(PRTCNT, S, M)
C
C***  SUBROUTINE TO COMPUTE THE NEXT S PARTITION
C
      INTEGER S, M(S), PRTCNT, I, MSUM
      IF (PRTCNT.GT.0) GO TO 20
      DO 10 I=1,S
        M(I) = 0
   10 CONTINUE
      PRTCNT = 1
      RETURN
   20 PRTCNT = PRTCNT + 1
      MSUM = M(1)
      IF (S.EQ.1) GO TO 60
      DO 50 I=2,S
        MSUM = MSUM + M(I)
        IF (M(1).LE.M(I)+1) GO TO 40
        M(1) = MSUM - (I-1)*(M(I)+1)
        DO 30 L=2,I
          M(L) = M(I) + 1
   30   CONTINUE
        RETURN
   40   M(I) = 0
   50 CONTINUE
   60 M(1) = MSUM + 1
      RETURN
      END