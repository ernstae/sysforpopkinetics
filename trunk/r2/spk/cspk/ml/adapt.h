extern "C" int adapt_(
	integer *ndim,
	 doublereal *a,
	 doublereal *b,
	 integer *minpts,
	 integer *maxpts,
	 doublereal (*functn)(integer *ndim, doublereal *z),
	 doublereal *eps,
	 doublereal *relerr,
	 integer *lenwrk,
	 doublereal *wrkstr,
	 doublereal *finest,
	 integer *ifail
);
extern "C" int bsrl_(integer *s,
	 doublereal *center,
	 doublereal *hwidth,
	 D_fp f,
	 integer *maxvls,
	 integer *funcls,
	 doublereal *errmin,
	 doublereal *errest,
	 doublereal *basest,
	 integer *divaxo,
	 integer *divaxn
);
extern int symrl_(integer *s,
	 doublereal *center,
	 doublereal *hwidth,
	 D_fp f,
	 integer *minord,
	 integer *maxord,
	 doublereal *intvls,
	 integer *intcls,
	 integer *numsms,
	 doublereal *weghts,
	 doublereal *fulsms,
	 integer *fail
);
extern doublereal wht_(integer *s,
	 doublereal *intrps,
	 integer *m,
	 integer *k,
	 integer *modofm,
	 integer *d__,
	 integer *maxrdm,
	 doublereal *momprd
);
extern doublereal flsm_(integer *s,
	 doublereal *center,
	 doublereal *hwidth,
	 doublereal *x,
	 integer *m,
	 integer *mp,
	 integer *maxord,
	 doublereal *g,
	 D_fp f,
	 integer *sumcls
);
extern int nxprt_(integer *prtcnt,
	 integer *s,
	 integer *m
);
/*:ref: bsrl_ 14 11 4 7 7 207 4 4 7 7 7 4 4 */
/* Rerunning f2c -P may change prototypes or declarations. */
