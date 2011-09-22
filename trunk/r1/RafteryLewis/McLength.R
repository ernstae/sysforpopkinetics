# $begin McLength$$ $wspace #$$
# $spell
#	Cox
#	mu
#	runif
# $$
# 
# $section Determine Length of Two State Chain For a Certain Accuracy$$
# 
# $table
# $bold Syntax$$
# $cnext $syntax%%N% <- McLength(%alpha%, %beta%, %r%, %s%)%$$ 
# $tend
# 
# $fend 20$$
#
# $head alpha$$
# is the transition probability for the transition from state 1 to 2; i.e.,
# $latex \[
#	\alpha = p( C_l = 2 | C_{l-1} = 1 )
# \] $$
# 
# $head beta$$
# is the transition probability for the transition from state 2 to 1; i.e.,
# $latex \[
#	\beta = p( C_l = 1 | C_{l-1} = 2 )
# \] $$
# 
# $head N$$
# We define the random variable
# $latex \[
#	Y_N = \frac{1}{N} \sum_{l=1}^N ( C_l - 1 )  
# \] $$ 
# This random variable is asymptotically normal with approximate 
# mean and variance given by
# $latex \[
# \begin{array}{rcl}
#	E [ Y_N ] & \approx & \frac{ \alpha }{ \alpha + \beta } \\
#	V [ Y_N ] & \approx & 
#	\frac{1}{N} 
#		\frac{ \alpha \beta ( 2 - \alpha - \beta ) }
#			{ ( \alpha + \beta )^3 } 
# \end{array}
# \] $$
# (see Equation 220 of The Theory of Stochastic Processes by Cox.)
#
# $head r, s$$
# The return value of $italic N$$ is such that with asymptotic
# probability $italic s$$ the random variable $latex Y_N$$ is 
# within $italic r$$ of  its expected value; i.e.,
# $latex \[
#	P[ | Y_N - E [ Y_N ] | \leq r ] = s
# \] $$
#
# $head Example$$
# The function call $code McLengthOk()$$
# is both an example and test of $code McLength$$.
# It returns $code TRUE$$ if the test passes, and $code FALSE$$ otherwise:
# $wspace $$ $codep 
McLengthOk <- function() {
	# this test should fail once every 1 / 10 times
	ok    <- TRUE
	alpha <- .3
	beta  <- .5
	s     <- 1 - .1 / (7 - 2 + 1)
	mu    <- alpha / (alpha + beta)
	for( i in 2:7 ) {
		# value of r for this case
		r = 1 / 2^i
		# length necessary for this accuracy
		N     <- McLength(alpha, beta, r, s)
		# 
		# uniform random numbers between 0 and 1
		u     <- runif(N)
		# initialize the chain as zero
		C     <- array(0, N)
		#
		# construct a first and second order markov chain with specified
		# transition probabilities
		p = c(alpha, beta)
		for( l in 2:N) {
			if( u[l] <=  p[C[l-1] + 1] )
				C[l] <- (1 - C[l-1]) 
			else	C[l] <- C[l-1]
		} 
		# compute Y_N
		Y <- sum(C) / N
		# check with in bounds
		ok <- ok && ( abs(Y - mu) <= r )
	}
	return(ok)
}
# $$ $wspace #$$
# 
# $end
#
McLength <- function(alpha, beta, r, s) {
	# use Raftery and Lewis formula on page 118 of Markov Chain Monte
	# Carlo in Partice (Gilks et al.)
	#
	# inverse of the cumulative for an N(0,1) distribution
	# divided by r and then squared
	PhiInvSq <- ( qnorm( (s + 1) / 2 ) / r )^2
	# corresponding value of N such that conditions are satisfied
	N <- PhiInvSq * (2 - alpha - beta) * alpha * beta / (alpha + beta)^3
	return( floor(N) + 1 )
}
