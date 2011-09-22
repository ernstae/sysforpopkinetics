      double precision function functn(ndim, z)
      integer ndim
      double precision alpha(4)
      double precision z(ndim)
      data alpha /2d0, 10d0, 5d0, 3d0/
      functn = exp( alpha(1) * z(1) ) 
     +       * cos( alpha(2) * z(2) ) 
     +       * sin( alpha(3) * z(3) ) 
     +       * alpha(4) * z(4) * z(4)
      return
      end
      program test
      integer ndim, minpts, maxpts, rulcls, lenwrk, maxfac, ifail
      double precision eps, relerr, finest, check
      parameter(ndim = 4, maxfac = 100)
      parameter(rulcls =  2**ndim + 2 * ndim**2 + 6 * ndim + 1)
      parameter(lenwrk = (2 * ndim + 3) * (1 + maxfac) / 2)
      double precision alpha(4)
      double precision a(ndim)
      double precision b(ndim)
      double precision wrkstr(lenwrk)
      external functn
      data alpha /2d0, 10d0, 5d0, 3d0/

c dimension of the space we are integrating with respect to
      if( ndim .lt. 2 ) stop "ndim must be > 1"

c lower limits for integration
      do 10 i = 1, ndim
          a(i) = 0.
          b(i) = 1.
10    continue

c minimum number of function evaluations to use
      minpts = 5000

c maximum number of function evaluatiosn to use
      maxpts = maxfac * rulcls

c requested relative accuracy
      eps = 1e-4

c compute estiamte of the integral
      call ADAPT(
     +      ndim,
     +      a,
     +      b,
     +      minpts,
     +      maxpts,
     +      functn,
     +      eps,
     +      relerr,
     +      lenwrk,
     *      wrkstr,
     +      finest,
     +      ifail
     + )


c compute true value of integeral
      check = ( ( exp(alpha(1)*b(1)) - exp(alpha(1)*a(1)) ) / alpha(1) ) 
     +      * ( ( sin(alpha(2)*b(2)) - sin(alpha(2)*a(2)) ) / alpha(2) ) 
     +      * ( ( cos(alpha(3)*a(3)) - cos(alpha(3)*b(3)) ) / alpha(3) )
     +      * ( alpha(4) * (b(4)**3 - a(4)**3) / 3d0 ); 

      print *, "ifail =", ifail
      print *, "maxpts =", maxpts
      print *, "minpts =", minpts
      print *, "relerr =", relerr
      print *, "check  =", check
      print *, "finest =", finest
      print *, "(check - finest) / check =", (check - finest) / check
      end
