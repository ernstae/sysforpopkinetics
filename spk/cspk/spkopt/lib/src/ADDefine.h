# ifndef CppADADMacroIncluded
# define CppADADMacroIncluded

/*
$begin ADMacro$$
$spell
	Cpp
	inline
	namespace
	std
	const
	Op
$$
$aindex head$$

$mindex macro definition$$
$section Macros Used by CppAD Implementation$$ 

$head CppADVersion$$
The preprocessor symbol $code CppADVersion$$
contains a $code '\0'$$ terminated
$code const char *$$ string that identifies the current version of
$code CppAD$$.

$head CppADNull$$
The preprocessor symbol $code CppADNull$$
defines the null pointer used by $code CppAD$$.

$head  CppADMinusOne$$
The preprocessor symbol $code CppADMinusOne$$
can be used for the value minus one in the current base type; i.e., 
$italic Base$$.

$head  CppADZero$$
The preprocessor symbol $code CppADZero$$
can be used for the value zero in the current base type.

$head  CppADOne$$
The preprocessor symbol $code CppADOne$$
can be used for the value zero in the current base type.

$head  CppADTwo$$
The preprocessor symbol $code CppADTwo$$
can be used for the value zero in the current base type.


$head CppADStandardMathFun$$
The macro call
$syntax%
	CppADStandardMathFun(%Name%)
%$$
defines an inline function in the
current namespace that is a link to the corresponding
standard mathematical functions
$syntax%
	float           %name%(float x)
	double          %name%(double x)
	complex<float>  %name%(std::complex<float> x)
	complex<double> %name%(std::complex<double> x)
%$$
Note that you should not place a semi-colon directly after the use
of this macro.

$head CppADStandardMathBinaryFun$$
The macro call
$syntax%
	CppADStandardMathBinaryFun(%Name%)
%$$
defines an inline function in the
current namespace that is a link to the corresponding
standard mathematical functions
$syntax%
	float           %name%(float x, float y)
	double          %name%(double x, double y)
	complex<float>  %name%(std::complex<float> x, std::complex<float> y)
	complex<double> %name%(std::complex<double> x, std::complex<double> y)
%$$
Note that you should not place a semi-colon directly after the use
of this macro.

$head CppADNoTemplateParameter$$
Some compilers require a special specification 
after an operator when the template resolution is just on the operands;
i.e., the type does not follow the operand during usage.
This preprocessor symbol is the required extra specification. 

$head CppADBinaryFriend$$
The $syntax%
	CppADBinaryFriend(%Op%)
%$$
declares the binary operator $italic Op$$ as a friend
where the left and right operands are $italic Base$$ 
or $syntax%AD<%Base%>%$$ objects and at least one of the operands a
$syntax%AD<%Base%>%$$ object.

$end
*/

# define CppADVersion  "YY-MM-DD"
# define CppADNull      0
# define CppADMinusOne static_cast<Base>( -1 )
# define CppADZero     static_cast<Base>( 0  )
# define CppADOne      static_cast<Base>( 1  )
# define CppADTwo      static_cast<Base>( 2  )



# ifdef  WIN32
# define CppADNoTemplateParameter
# else
# define CppADNoTemplateParameter <>
# endif

// use this macro for all the Base binary operators
# define CppADBinaryFriend(Op)                                 \
                                                               \
	friend AD<Base> operator Op CppADNoTemplateParameter   \
		(const AD<Base> &left, const AD<Base> &right); \
                                                               \
	friend AD<Base> operator Op CppADNoTemplateParameter   \
		(const Base &left, const AD<Base> &right);     \
                                                               \
	friend AD<Base> operator Op CppADNoTemplateParameter   \
		(const AD<Base> &left, const Base &right)


# ifdef WIN32

# define CppADStandardMathFun(Name)                                       \
                                                                          \
	inline float Name(float x)                                        \
	{	return ::Name(x); }                                       \
                                                                          \
	inline std::complex<float> Name(std::complex<float> x)            \
	{	return std::Name(x); }                                    \
                                                                          \
	inline double Name(double x)                                      \
	{	return ::Name(x); }                                       \
                                                                          \
	inline std::complex<double> Name(std::complex<double> x)          \
	{	return std::Name(x); }

# else

# define CppADStandardMathFun(Name)                                       \
                                                                          \
	inline float Name(float x)                                        \
	{	return std::Name(x); }                                    \
                                                                          \
	inline std::complex<float> Name(std::complex<float> x)            \
	{	return std::Name(x); }                                    \
                                                                          \
	inline double Name(double x)                                      \
	{	return std::Name(x); }                                    \
                                                                          \
	inline std::complex<double> Name(std::complex<double> x)          \
	{	return std::Name(x); }

# endif

# ifdef WIN32

# define CppADStandardMathBinaryFun(Name)                                 \
                                                                          \
	inline float Name(float x, float y)                               \
	{	return ::Name(x, y); }                                    \
                                                                          \
	inline std::complex<float> Name(                                  \
		std::complex<float> x, std::complex<float> y)             \
	{	return std::Name(x, y); }                                 \
                                                                          \
	inline double Name(double x, double y)                            \
	{	return ::Name(x, y); }                                    \
                                                                          \
	inline std::complex<double> Name(                                 \
		std::complex<double> x, std::complex<double> y)           \
	{	return std::Name(x, y); }

# else

# define CppADStandardMathBinaryFun(Name)                                 \
                                                                          \
	inline float Name(float x, float y)                               \
	{	return std::Name(x, y); }                                 \
                                                                          \
	inline std::complex<float> Name(                                  \
		std::complex<float> x, std::complex<float> y)             \
	{	return std::Name(x, y); }                                 \
                                                                          \
	inline double Name(double x, double y)                            \
	{	return std::Name(x, y); }                                 \
                                                                          \
	inline std::complex<double> Name(                                 \
		std::complex<double> x, std::complex<double> y)           \
	{	return std::Name(x, y); }

# endif


# endif
