# $OMhelpKeyCharacter=@
# @begin Hastings@@ @wspace #@@
# @spell
#	Xn
#	Xp
#	runif
#	dnorm
#	Sturges
#	sqrt
#	Hastings
# @@
# 
# @section A Hastings-Metropolis Sampler@@
# 
# @table
# @bold Syntax@@
# @cnext @syntax%%Xn% <- Hastings(%Xp%, %sample%, %q%, %pi%)%@@
# @tend
# 
# @fend 20@@
# 
# @head Xp@@
# The value of the simulated sequence at the previous time point @italic t@@.
# 
# @head sample@@
# The procedure call
# @syntax% %sample%(%X%) %@@
# samples a value @italic Y@@
# from the probability density function 
# @latex \[
# 	q(Y | X) / \int_Y q( Y | X ) d Y
# \] @@.
# 
# @head q@@
# The procedure call
# @syntax% %q%(%Y%, %X%) %@@
# returns the value @latex q(Y | X)@@.
# 
# @head pi@@
# The procedure call
# @syntax% %pi%(%X%)%@@ returns the value of the target 
# density function.
# 
# @head Xn@@
# Is the value of the simulated sequence at the next time point 
# @latex t + 1@@.
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
		r = 1. 
	else	r = 0.
	return( r )
}
target   <- function(X)
{	dnorm(X) }		
HastingsOk <- function()
{
	N     <- 10000
	X     <- array(0, N)
	for( i in 2:N )
		X[i]     <- Hastings(X[i-1], sample, proposal, target)
	SampleHist = hist(X, breaks="Sturges", probability=TRUE, plot=FALSE)
	x = SampleHist$mids
	y = SampleHist$density
	ok = all( abs(y - dnorm(x)) < 10. / sqrt(N) )
	return( ok )
}
# @@ @wspace #@@
# @end
# ----------------------------------------------------------------------------
Hastings <- function(Xp, sample, q, pi)
{	Y           <- sample(Xp)
	U           <- runif(1)
	numerator   <- pi(Y) * q(Xp, Y)
	denominator <- pi(Xp) * q(Y, Xp)
	alpha       <- min(1, numerator / denominator)
	if( U <= alpha )
		Xn <- Y
	else	Xn <- Xp
	return ( Xn )
}
