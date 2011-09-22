# $OMhelpKeyCharacter=@
# @begin McTransition@@ @wspace #@@
# @latex \newcommand{\p}{ {\bf p} }@@
# @spell
#	runif
# @@
# 
# @section Estimate Transition Matrix for a Discrete Markov Chain@@
# 
# @table
# @bold syntax@@
# @cnext @syntax%list(%Q%,%S%) <- McTransition(%C%)%@@ 
# @tend
# 
# @fend 20@@
#
# @head Assumption@@
# the vector @italic C@@ is a sample of a first order markov sequence; i.e.,
# @latex \[
# 	\p( C_l | C_{l-1} , \ldots , C_1 ) = \p( C_l | C_{l-1} )
# \] @@
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
# @head Q@@
# the return matrix @latex Q \in R^{m \times m}@@ 
# is an approximation for the transition matrix; i.e.,
# @latex \[
#	Q_{i,j} \approx \p( C_l = j | C_{l-1} = i )
# \] @@
# the sum of the elements in each row of @italic Q@@ is equal to one.
#
# @head S@@
# the return matrix @latex S \in R^{m \times m}@@ 
# is an approximation for the standard deviation of the corresponding
# elements of @latex Q@@; i.e.,
# @latex \[
#	S_{i,j}^2 \approx E [ Q_{i,j} - \p( C_l = j | C_{l-1} = i ) ]^2
# \] @@
#
# @head Example@@
# the function call @code McTransitionOk()@@
# is both an example and test of @code McTransition@@.
# It returns @code TRUE@@ if the test passes, and @code FALSE@@ otherwise:
# @wspace @@ @codep 
McTransitionOk <- function() {
	p     <- c(.3, .5) # transition probability for same value
	n     <- 10000     # length of the chain
	# uniform random numbers between 0 and 1
	u     <- runif(n)
	# initialize the chain as zero
	C     <- array(0, n)
	#
	# construct a first and second order markov chain with specified
	# transition probabilities
	for( l in 2:n) {
		if( u[l] <= p[C[l-1] + 1] )
			C[l] <- C[l-1] 
		else	C[l] <- 1 - C[l-1]
	} 
	# transition probabilities and standard deviation of estimate
	List <- McTransition(C)
	Q    <- List$Q
	S    <- List$S
	# true transition probabilities in column major order
	QTrue <- array( c(p[1], 1-p[2], 1-p[1], p[2]), c(2,2) )
	# check that estimate is with in two standard deviations
	return( all( (2 * S) >= abs( QTrue - Q ) ) )
}
# @@ @wspace #@@ 
# 
# @end
#
McTransition <- function(C) {
	# some scalar constants
	n     <- length(C) - 1
	lower <- C[ which.min(C) ]
	upper <- C[ which.max(C) ]
	m     <- upper - lower + 1
	# version of chain that is between 1 and m
	C     <- C - (lower - 1) 
	# place where we will sum the counts
	X     <- array(0, c(m, m) )
	# sum the counts
	for( l in 1:n ) {
		X[C[l], C[l+1]] <- X[C[l], C[l+1]] + 1 
	}
	#
	# estimate of the multinomial probabilities
	P = X / n
	#
	# estimate of the variance of P using diagonal of
	# formula 14.3-38 of Bishop
	V <- ( P - P^2 ) /  n
	#
	Q <- array(0, c(m,m))
	for( i in 1:m ) {
		rowi    <- row(P) == i
		sumi    <- sum( P[rowi] )
		Q[rowi] <- P[rowi] / sumi
		V[rowi] <- V[rowi] / (sumi * sumi)
	}
	S <- sqrt( V )
	return( list(Q=Q, S=S) ) 
}
