# $OMhelpKeyCharacter=@
# @begin Thin@@ @wspace #@@
# @spell
#
# @@
# 
# @section Thin a Vector of Samples@@
# 
# @table
# @bold Syntax@@
# @cnext @syntax%%T% <- Thin(%C%, %i%)%@@ 
# @tend
# 
# @fend 20@@
#
# @head C@@
# is the vector of samples and we use @latex n@@ to denote its length.
#
# @head i@@
# is a positive integer that specifies the interval in the original data
# that corresponds to successive values in the thinned data
# 
# @head T@@
# The return vector @latex T@@ has length equal to
# @syntax%
#	1 + floor( (%n%-1) / %i% )
# %@@ 
# and is defined follows:
# @syntax%
# 	%T%[%k%] = %C%[1 + (%k%-1) * %i% ]
# %@@
#
# @head Example@@
# The function call @code ThinOk()@@
# is both an example and test of @code Thin@@.
# It returns @code TRUE@@ if the test passes, and @code FALSE@@ otherwise:
# @codep 
ThinOk <- function() {
	C  <- c(1, 10, 2, 9, 3, 8, 4, 7, 5, 6)
	i  <- 3
	T  <- Thin(C, i)
	ok <- c(1,        9,       4,       6) == T
	ok <- sum(ok) == length(T)
	ok <- ok & (length(T) == 4)
	return( ok )
}
# @@ 
# 
# @end
# ---------------------------------------------------------------------------
#
Thin <- function(C, i) {
	n     <- length(C)
	first <- 1
	last  <- first + floor( (n-first) / i ) * i 
	index <- seq(first, last, i)
	T     <- C[index]
	return ( T )
}
