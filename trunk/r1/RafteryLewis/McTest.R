# $OMhelpKeyCharacter=@
# @begin McTest@@ @wspace #@@
# @spell
#	Bayes
#	Mcmc
#	Raftery
#	runif
#	Bic
# @@
# 
# @section Compare First to Second Order Markov for a Discrete Chain@@
# 
# @table
# @bold Syntax@@
# @cnext @syntax%%G2% <- McTest(%C%)%@@ 
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
# The vector @italic C@@ is a sample of a second order Markov sequence; i.e.,
# @latex \[
# 	P( C_l | C_{l-1} , \ldots , C_1 ) = P( C_l | C_{l-1} , C_{l-2} )
# \] @@
#
# @head G2@@
# The return a scalar @italic G2@@ is asymptotically (in @latex n@@)
# distributed as a chi-squared random variable with two degrees of freedom
# @latex \[ 
#	G^2 = 2 \sum_{i,j,k} x_{i,j,k} \log 
#		\left[ 	
#			\frac{ x_{i,j,k} } { x_{i,j,+} x_{+,j,k} / x_{+,j,+} }
#		\right]
# \] @@
# where @latex x_{i,j,k}@@ is the number of times the transition
# @latex i \rightarrow j \rightarrow k@@ occurred in @italic C@@.
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
#		\frac{ P ( x | M_1 ) }{ P ( x | M_2 ) }
#		\right]
#	\\
#	-2 \log ( Bic )   
#	& \approx & G^2 - m (m-1)^2  \log (n)
# \end{array}
# \] @@
# where @latex M_1@@ is the first order model and @latex M_2@@ is the second
# order model and the approximation above is asymptotic in @latex n@@.
# We note that the saturated model has @latex m^3@@ parameters,
# there are @latex 1 + 3(m-1) + 2(m-1)(m-1)@@ parameters in the model
# given by Equation 7.3-17 of Bishop and 
# @latex  \[
#	m(m-1)^2 = m^3 - [ 1 + 3(m-1) + 2(m-1)(m-1) ]
# \] @@
#
# @head Example@@
# The function call @code McTestOk()@@
# is both an example and test of @code McTest@@.
# It returns @code TRUE@@ if the test passes, and @code FALSE@@ otherwise:
# @wspace @@ @codep 
#
McTestOk <- function() {
	p     <- c(.3, .5) # transition probability for same value
	n     <- 200       # length of the chain
	# uniform random numbers between 0 and 1
	u     <- runif(n)
	# initialize the first and second order chains as zero
	Markov      <- array(0, n)
	SecondOrder <- array(0, n)
	#
	# construct a first and second order Markov chain with specified
	# transition probabilities
	if( u[2] <= p[Markov[1] + 1] )
		Markov[2] <- Markov[1] 
	else	Markov[2] <- 1 - Markov[1]
	for( i in 3:n) {
		if( u[i] <= p[Markov[i-1] + 1] )
			Markov[i] <- Markov[i-1] 
		else	Markov[i] <- 1 - Markov[i-1]
		if( u[i] <= p[SecondOrder[i-2] + 1] )
			SecondOrder[i] <- SecondOrder[i-2]
		else	SecondOrder[i] <- 1 - SecondOrder[i-2]
	} 
	G2Markov   <- McTest(Markov)
	G2SecondOrder  <- McTest(SecondOrder)
	# 
	# check that the Bayesian information criteria is <= 0
	BicMarkov      <- G2Markov - 2 * log(n)
	BicSecondOrder <- G2SecondOrder - 2 * log(n)
	return ( (BicMarkov <= 0) & (BicSecondOrder >= 0) )
}
# @@ @wspace #@@ 
# 
# @end
#
McTest <- function(C) {
	# some scalar constants
	n2    <- length(C) - 2
	lower <- C[ which.min(C) ]
	upper <- C[ which.max(C) ]
	m     <- upper - lower + 1
	# version of chain that is between 1 and m
	C     <- C - (lower - 1) 
	# place where we will sum the counts
	x     <- array(0, c(m, m, m) )
	# sum the counts
	for( l in 1:n2 ) {
		x[ C[l], C[l+1], C[l+2] ] <- x[ C[l], C[l+1], C[l+2] ] + 1
	}
	# Values in Table 7.4.1b of Bishop where G2 should equal 36.53
	# x     = c( 90, 184, 263, 64, 184, 143, 64, 8 )
	# dim(x) <- c(2, 2, 2)
	#
	# x_{i,j,+}
	xij <- rowSums(x, FALSE, 2)
	# x_{+,j,k}
	xjk <- colSums(x, FALSE, 1)
	# x_{+,j,+}
	xj  <- colSums(xij, FALSE, 1)
	# model value corresponding to each element
	G2  <- 0
	y   <- array(0, c(m, m, m) )
	for( i in 1:m ) {
		for( j in 1:m ) {
			for( k in 1:m ) {
				data  <- x[i,j,k]
				model <- xij[i,j] * xjk[j,k] / xj[j]
				if( data > 0 )
					G2    <- G2 + data * log( data / model )
			}
		}
	}
	return( 2 * G2 )
}
