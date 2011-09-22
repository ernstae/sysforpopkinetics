# $OMhelpKeyCharacter=@
# @begin IndTest@@ @wspace #@@
# @spell
#	Ind
#	Bayes
#	Mcmc
#	Raftery
#	runif
#	Bic
# @@
# 
# @section Compare Independent to First order Markov for a Discrete Chain@@
# 
# @table
# @bold Syntax@@
# @cnext @syntax%%G2% <- IndTest(%C%)%@@ 
# @tend
# 
# @fend 20@@
#
# @head C@@
# is a sample of a discrete random process that takes on all 
# integer values between the minimum value in @italic C@@ and the 
# maximum value in @italic C@@.
# We use  @latex n@@ to denote the length of the vector @italic C@@
# and @latex m@@ to denote the number of possible states; i.e.,
# @latex \[
#	m = 1
#	  + \max \{ C_l : l = 1 , \ldots , n \} 
#	  - \min \{ C_l : l = 1 , \ldots , n \} 
# \] @@
#
# @head Assumption@@
# The vector @italic C@@ is a sample of a first order Markov sequence; i.e.,
# @latex \[
# 	P( C_l | C_{l-1} , \ldots , C_1 ) = P( C_l | C_{l-1} )
# \] @@
#
# @head G2@@
# The return a scalar @italic G2@@ is asymptotically (in @latex n@@)
# distributed as a chi-squared random variable with one degrees of freedom
# @latex \[ 
#	G^2 = 2 \sum_{i,j} x_{i,j} \log 
#		\left[ 	
#			\frac{ x_{i,j} } { x_{i,+} x_{+,j} / x_{+,+} }
#		\right]
# \] @@
# where @latex x_{i,j}@@ is the number of times the transition
# @latex i \rightarrow j@@ occurred in @italic C@@.
# See page 271 of Bishop's Discrete Multivariate Analysis [QA 278 B57]
# for a discussion of @latex G^2@@.
#
# @head Bic@@
# The Bayesian Information Criteria (Bic) is given in the Raftery
# 1986 article @italic A Note on Bayes Factors for 
# Log-Linear Contingency Table Models with Vague Prior Information@@ as
# @latex \[
# \begin{array}{rcl}
#	Bic   
#	& = & \left[  
#		\frac{ P ( x | M_0 ) }{ P ( x | M_1 ) }
#		\right]
#	\\
#	-2 \log ( Bic )   
#	& \approx & G^2 - (m - 1)  \log (n)
# \end{array}
# \] @@
# where @latex M_0@@ is the independent model and @latex M_1@@ is the first
# order Markov model and the approximation above is asymptotic in @latex n@@.
# We note that the difference in degrees of freedom between the saturated 
# model (Markov) is and the independent model is @latex m-1@@.
# 
# @head Example@@
# The function call @code IndTest()@@
# is both an example and test of @code IndTest@@.
# It returns @code TRUE@@ if the test passes, and @code FALSE@@ otherwise:
# @wspace @@ @codep 
#
IndTestOk <- function() {
	p     <- c(.3, .5) # transition probability for same value
	n     <- 500       # length of the chain
	# uniform random numbers between 0 and 1
	u     <- runif(n)
	# initialize the first and second order chains as zero
	Independent <- array(0, n)
	Markov      <- array(0, n)
	#
	# construct an independent and Markov chain with 
	# specified probabilities
	for( i in 2:n) {
		if( u[i] <= p[1] )
			Independent[i] <- 0
		else	Independent[i] <- 1
		if( u[i] <= p[Markov[i-1] + 1] )
			Markov[i] <- Markov[i-1] 
		else	Markov[i] <- 1 - Markov[i-1]
	} 
	G2Independent  <- IndTest(Independent)
	G2Markov       <- IndTest(Markov)
	# 
	# check that the Bayesian information criteria is <= 0
	BicIndependent <- G2Independent - log(n)
	BicMarkov      <- G2Markov - log(n)
	return ( (BicIndependent <= 0) & (BicMarkov >= 0) )
}
# @@ @wspace #@@ 
# 
# @end
#
IndTest <- function(C) {
	# some scalar constants
	n1    <- length(C) - 1
	lower <- C[ which.min(C) ]
	upper <- C[ which.max(C) ]
	m     <- upper - lower + 1
	# version of chain that is between 1 and m
	C     <- C - (lower - 1) 
	# place where we will sum the counts
	x     <- array(0, c(m, m) )
	# sum the counts
	for( l in 1:n1 ) {
		x[ C[l], C[l+1] ] <- x[ C[l], C[l+1] ] + 1
	}
	# x_{i,+}
	xi    <- rowSums(x)
	# x_{+,j}
	xj    <- colSums(x)
	# model value corresponding to each element
	G2  <- 0
	y   <- array(0, c(m, m) )
	for( i in 1:m ) {
		for( j in 1:m ) {
			data  <- x[i,j]
			model <- xi[i] * xj[j] / n1
			if( data > 0 )
				G2    <- G2 + data * log( data / model )
		}
	}
	return( 2 * G2 )
}
