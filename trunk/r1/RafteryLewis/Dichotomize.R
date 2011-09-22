# $begin Dichotomize$$ $wspace #$$
# $spell
#	Dichotomize
# $$
# 
# $section Create a Zero One Sequence From a Vector of Samples$$
# 
# $table
# $bold Syntax$$
# $cnext $syntax%%D% <- Dichotomize(%C%, %u%)%$$ 
# $tend
# 
# $fend 20$$
#
# $head C$$
# is a vector of sample values.
# We use $latex n$$ to denote the length of $italic C$$.
#
# $head u$$
# is a scalar that represents the upper limit for a value in $italic C$$
# that corresponds to the value one in $italic D$$.
# 
# $head D$$
# The vector $latex D$$ has length $latex n$$ 
# and is defined follows:
# $latex \[
# 	D_k = \left\{ \begin{array}{ll}
#	1 & {\rm if} \; C_k \leq  u \\
#	0 & {\rm otherwise}
#	\end{array} \right. 
# \] $$
#
# $head Example$$
# The function call $code DichotomizeOk()$$
# is both an example and test of $code Dichotomize$$.
# It return $code TRUE$$ if the test passes, and $code FALSE$$ otherwise:
# $codep 
DichotomizeOk <- function() {
	C     <- c(1, 10, 2, 9, 3, 8, 4, 7, 5, 6)
	u     <- 4
	D     <- Dichotomize(C, u)
	ok    <- c(1,  0, 1, 0, 1, 0, 1, 0, 0, 0) == D
	ok    <- sum(ok) == length(C)
	return( ok )
}
# $$ 
# 
# $end
#
Dichotomize <- function(C, upper) {
	# logical version
	Flag <- C <= upper
	# numeric version
	R <- 1 * Flag
	return( R )
}
