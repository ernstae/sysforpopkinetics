# $OMhelpKeyCharacter=@
# @begin InvSampleCum@@ @wspace #@@
# @spell
#	Cum
#	Inv
#	quantile
# @@
# 
# @section Sample Value Corresponding to a Quantile@@
# 
# @table
# @bold Syntax@@
# @cnext @syntax%%u% <- InvSampleCum(%C%, %q%)%@@ 
# @tend
# 
# @fend 20@@
#
# @head C@@
# is the vector of sample values.
# We use @latex n@@ to denote the length of @italic C@@ which must
# be greater than or equal two. 
#
# @head q@@
# is a scalar between zero and one and it represents a quantile; i.e., a value
# for the sample cumulative distribution.
# 
# @head u@@
# We use @italic S@@ to denote a copy of @italic C@@ sorted 
# in ascending order.
# The return value @italic u@@ is specified as follows:
# If @latex q < 1/n@@, @italic u = S_1@@.
# Otherwise there is an integer index @latex i@@ such that
# @latex 0 < k < n@@ and @latex \[
# 	k/n \leq q \leq (k+1)/n
# \]@@
# In this case, the value @italic u@@ is 
# @latex \[
# 	u = S_k * (k + 1 - q * n ) + S_{k+1} ( q * n - k ) 
# \] @@
#
# @head Example@@
# The function call @code InvSampleCumOk()@@
# is both an example and test of @code InvSampleCum@@.
# It return @code TRUE@@ if the test passes, and @code FALSE@@ otherwise:
# @codep 
InvSampleCumOk <- function() {
	ok   <- TRUE
	C    <- c(1, 10, 2, 9, 3, 8, 4, 7, 5, 6)
	ok   <- ok & InvSampleCum(C, .1)  == 1
	ok   <- ok & InvSampleCum(C, 1.)  == 10.
	ok   <- ok & InvSampleCum(C, .35) == 3.5
	retrun( ok )
}
# @@ 
# 
# @end
#
InvSampleCum <- function(C, q) {
	# sorted version of the data values
	S <- sort(C)
	# number of data values 
	n <- length(C)
	#
	# if q < 1/N then k == 1
	# otherwise k/N <= q <= (k+1)/N
	k <- floor(q * n)
	k <- max(k, 1)
	k <- min(k, n-1)
	#
	u  <- S[k] * (k + 1 - q * n) + S[k+1] * (q * n - k)
	return( u )
}
