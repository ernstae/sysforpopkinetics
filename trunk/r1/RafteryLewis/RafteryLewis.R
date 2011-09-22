# $OMhelpKeyCharacter=@
# @begin RafteryLewis@@ @wspace #@@
# @spell
#	Raftery
#	quantile
#	Gilks
# @@
#
# @section Raftery and Lewis Diagnostic for Markov Chains@@
#
# @table
# @bold Syntax@@
# @cnext @syntax%list(M, N, S) <- RafteryLewis(%C%, %q%, %r%, %s%, %epsilon%)%@@
# @tend
#
# @fend 25@@
#
# @head Reference@@
# This routine uses the notation in the article
# @italic Implementing MCMC@@,
# A.E. Raftery and S.M. Lewis,
# Markov Chain Monte Carlo in Practice,
# W.R. Gilks et. al.
#
# @head Introduction@@
# We are given a finite sample @latex C@@ of a Markov chain
# that has a limiting distribution equal to the distribution
# for the random variable @latex U@@.
# The value @italic n@@ is defined to be the number of elements in @italic C@@.
# Given a value @latex u@@ we define the zero one sequence @latex D(u)@@ by
# @latex \[
#	D(u)_k = \left\{ \begin{array}{ll}
#	1 & {\rm if} \; C_k \leq u \\
#	0 & {\rm otherwise} 
#	\end{array} \right.
# \] @@
#
# @head q@@
# The value @italic q@@ is a value for the cumulative probability
# We use the following implicit definition for
# @latex \bar{u}@@ 
# @latex \[
#	q = \frac{1}{n} \; \sum_{k=1}^n  \; D( \bar{u} )_k
# \] @@
# (Note that the standard notation, as apposed to that in Raftery and Lewis
#  is that @latex \bar{u}@@ is the quantile and @italic q@@ 
# is the corresponding value for the cumulative probability.)
# Our problem is to determine the post burn in length @latex N@@
# and burn in period @italic M@@ for an accurate approximation of the form
# @latex \[
# P( U \leq \bar{u} ) \approx
# \frac{1}{N} \; \sum_{k=M+1}^{M+N} \; D( \bar{u} )_k 
# \] @@.
#
# @head r@@
# The scalar @italic r@@ 
# ( @latex 0 < r < 1@@ ) 
# is the accuracy with which we want to determine
# @latex P( U \leq \bar{u} )@@ 
#
# @head s@@
# The scalar @italic s@@
# ( @latex 0 < s < 1@@ )
# is the allowable probability that 
# @latex \[
# \left| 
#	P( U \leq \bar{u} ) 
#	\; - \; 
# 	\frac{1}{N} \; \sum_{k=M+1}^{M+N} \; D( \bar{u} )_k 
# \right| > r 
# \] @@.
#
# @head epsilon@@
# The scalar @latex \varepsilon@@ specifies a bound of the form
# @latex \[
#	\varepsilon \geq | P( U \leq \bar{u} ) - E [ D( \bar{u} )_k ] |
# \] @@
# for @latex k = M , \ldots , M+N@@.
#
# @head M@@
# If the string @italic S@@ is equal to @code "ok"@@,
# the scalar @italic M@@ is the number of elements of
# @italic C@@ required in the burn in period; i.e., to obtain 
# inequality listed above under the heading
# @xref/RafteryLewis/epsilon/epsilon/@@.
#
# @head N@@
# If the string @italic S@@ is equal to @code "ok"@@,
# the scalar @italic N@@ is the number of elements of
# @italic C@@ required in the post burn in period; i.e., to obtain
# inequality listed above under the heading @xref/RafteryLewis/s/s/@@.
#
# @head S@@
# The string @italic S@@ specifies the status of the return.
# If @syntax%%S% == "ok"%@@ is true, @code RafteryLewis@@ completed
# successfuly. Otherwise, @italic S@@ is a description of the 
# reason for failure of the routine.
# 
# @head Subroutines@@
# @children%
# 	InvSampleCum.R%
#	Dichotomize.R%
#	Thin.R%
#	McTest.R%
#	McTransition.R%
#	IndTest.R%
#	Hastings.R
# %@@
# @table
# @rref InvSampleCum@@
# @rref Dichotomize@@
# @rref Thin@@
# @rref McTest@@
# @rref McTransition@@
# @rref IndTest@@
# @rref Hastings@@
# @tend
# 
# @head Example@@
# @wspace @@ @codep
sample <- function(X)
{	# generate a uniform on [X-.5, X+.5] 
	Y  <- X + runif(1) - .5
	return ( Y )
}
proposal <- function(Y, X)
{	if( abs(Y - X) < .5 )
		r <- 1. 
	else	r <- 0.
	return( r )
}
target   <- function(X)
{	dnorm(X) }		
RafteryLewisOk <- function()
{
	# simulate: uniform as proposal and standard normal as asymptotic
	N     <- 10000
	C     <- array(0, N)
	C[0]  <- 5
	for( i in 2:N )
		C[i]     <- Hastings(C[i-1], sample, proposal, target)
	# determine burn in and required number of samples
	q       <-  .02
	r       <-  .005
	s       <-  .95
	epsilon <-  .0005
	List    <- RafteryLewis(C, q, r, s, epsilon)
	print(List)
}
# @@ @wspace #@@
# @end
# ----------------------------------------------------------------------------
source("InvSampleCum.R")
source("Dichotomize.R")
source("Thin.R")
source("McTransition.R")
source("McTest.R")
source("IndTest.R")
source("Hastings.R")
#
RafteryLewis <- function(C, q, r, s, epsilon) {
	# number of points in C
	n <- length(C)
	# determine the sample value corresponding to q 
	ubar <- InvSampleCum(C, q)
	# create the zero one sequence corresponding to ubar
	D    <- Dichotomize(C, ubar)
	# initialize for loop to determine Markov spacing
	Bic       <- -1  # BIC for first order Markov hypothesis
	iMarkov   <-  0  # sampling interval
	while( Bic < 0 )
	{	# next sampling interval
		iMarkov   <- iMarkov+1
		if( iMarkov * iMarkov > n )
		{	S <- "Interval in C for First order Markov is to large"
			return ( list(M=0, N=0, S=S) )
		}
		# subsample the chain on this interval
		Markov    <- Thin(D, iMarkov)
		# Bishop's G2 statistic for first order Markov hypothesis
		G2        <- McTest(Markov)
		# corresponding Bayesian information criteria
		Bic       <- G2 - 2 * log( length(Markov) )
	}
	# number of elements in Markov Chain
	nMarkov <- length(Markov)
	# transition probabilities in first order Markov process
	List <- McTransition(Markov)
	Q    <- List$Q
	# initialize for loop to determin independent spacing
	Bic    <- -1  # BIC for first order Markov hypothesis
	iInd   <-  0  # sampling interval
	while( Bic < 0 )
	{	# next sampling interval
		iInd   <- iInd+1
		# subsample the chain on this interval
		Ind    <- Thin(Markov, iInd)
		if( iMarkov * iInd > n )
		{	S <- "Interval in C for Independent chain is to large"
			return ( list(M=0, N=0, S=S) )
		}
		# Bishop's G2 statistic for first order Markov hypothesis
		G2     <- IndTest(Ind)
		# corresponding Bayesian information criteria
		Bic    <- G2 - log( length(Ind) )
	}
	# compute burn in period
	alpha  <- Q[1,2]                  # probability 0 -> 1
	beta   <- Q[2,1]                  # probability 1 -> 0
	lambda <- 1 - alpha - beta
	if( lambda <= 0 )
	{	S <- "alpha + beta >= 1"
		return ( list(M=0, N=0, S=S) )
	}
	Sum    <- alpha + beta
	Max    <- max( alpha, beta )
	M    <- log(epsilon * Sum / Max) / log(lambda)
	M    <- floor( M + 1 ) * iMarkov
	# compute the post burn in period
	PhiInv <- qnorm(.5 * (s + 1)) 
	factor <- alpha * beta * (2 - alpha - beta) / (alpha + beta)^3
	N      <- factor * (PhiInv / r)^2
	N      <- floor(N + 1) * iInd * iMarkov
	# status flag
	S      <- "ok"
	return( list(M=M, N=N, S=S) )
}
