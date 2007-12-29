// ------------------------------------------------------- 
// Copyright (C) Bradley M. Bell 2003, All rights reserved 
// ------------------------------------------------------- 
Keyword = 
[
"CppAD  CppAD: A Package for Differentiation of C++ Algorithms   algorithmic automatic derivative version introduction include cppad.hpp symbol preprocessor namespace",
"Install  CppAD Download, Test, and Installation Instructions  ",
"InstallUnix  Unix Download, Test and Installation   cppad free tar file configure prefix directory documentation introduction get_started exp_apx example more speed double profile utility print forward mode std::vector adolc fadbad sacado boost compile flags",
"subversion  Using Subversion To Download Source Code  ",
"InstallWindows  Windows Download and Test   free cppad zip file getstarted introduction example more print forward mode speed double utility",
"Introduction  An Introduction by Example to Algorithmic Differentiation   ad automatic",
"get_started.cpp  A Simple Program Using CppAD to Compute Derivatives   getstarted example",
"exp_2  Second Order Exponential Approximation   example algorithm",
"exp_2.hpp  exp_2: Implementation  ",
"exp_2.cpp  exp_2: Test  ",
"exp_2_for0  exp_2: Operation Sequence and Zero Order Forward Mode   example expansion",
"exp_2_for1  exp_2: First Order Forward Mode   example expansion",
"exp_2_rev1  exp_2: First Order Reverse Mode   example",
"exp_2_for2  exp_2: Second Order Forward Mode   example expansion",
"exp_2_rev2  exp_2: Second Order Reverse Mode   example",
"exp_2_for0.cpp  exp_2: Verify Zero Order Forward Sweep  ",
"exp_2_for1.cpp  exp_2: Verify First Order Forward Sweep  ",
"exp_2_rev1.cpp  exp_2: Verify First Order Reverse Sweep   mode",
"exp_2_for2.cpp  exp_2: Verify Second Order Forward Sweep  ",
"exp_2_rev2.cpp  exp_2: Verify Second Order Reverse Sweep   mode",
"exp_2_cppad  exp_2: CppAD Forward and Reverse Sweeps  ",
"exp_eps  An Epsilon Accurate Exponential Approximation   example algorithm",
"exp_eps.hpp  exp_eps: Implementation  ",
"exp_eps.cpp  exp_eps: Test of exp_eps  ",
"exp_eps_for0  exp_eps: Operation Sequence and Zero Order Forward Sweep   example",
"exp_eps_for1  exp_eps: First Order Forward Sweep   expansion",
"exp_eps_rev1  exp_eps: First Order Reverse Sweep   mode example",
"exp_eps_for2  exp_eps: Second Order Forward Mode   example expansion",
"exp_eps_rev2  exp_eps: Second Order Reverse Sweep   mode example",
"exp_eps_for0.cpp  exp_eps: Verify Zero Order Forward Sweep  ",
"exp_eps_for1.cpp  exp_eps: Verify First Order Forward Sweep   exp_2",
"exp_eps_rev1.cpp  exp_eps: Verify First Order Reverse Sweep  ",
"exp_eps_for2.cpp  exp_eps: Verify Second Order Forward Sweep   first exp_2",
"exp_eps_rev2.cpp  exp_eps: Verify Second Order Reverse Sweep  ",
"exp_eps_cppad  exp_eps: CppAD Forward and Reverse Sweeps  ",
"exp_apx_main.cpp  Run the exp_2 and exp_eps Tests  ",
"AD  AD Objects   base require",
"Default  AD Default Constructor  ",
"Default.cpp  Default AD Constructor: Example and Test  ",
"ad_copy  AD Copy Constructor and Assignment Operator   convert base double vecad",
"CopyAD.cpp  AD Copy Constructor: Example and Test   object",
"CopyBase.cpp  AD Constructor From Base Type: Example and Test   convert",
"Eq.cpp  AD Assignment Operator: Example and Test  ",
"Convert  Conversion and Printing of AD Objects   from",
"Value  Convert From an AD Type to its Base Type  ",
"Value.cpp  Convert From AD to its Base Type: Example and Test   record",
"Integer  Convert From AD to Integer  ",
"Integer.cpp  Convert From AD to Integer: Example and Test  ",
"Output  AD Output Stream Operator   << write",
"Output.cpp  AD Output Operator: Example and Test   <<",
"PrintFor  Printing AD Values During Forward Mode  ",
"PrintFor.cpp  Printing During Forward Mode: Example and Test  ",
"Var2Par  Convert an AD Variable to a Parameter   from value_ obtain during taping",
"Var2Par.cpp  Convert an AD Variable to a Parameter: Example and Test   value during taping",
"ADValued  AD Valued Operations and Functions  ",
"Arithmetic  AD Arithmetic Operators and Computed Assignments  ",
"UnaryPlus  AD Unary Plus Operator   +",
"UnaryPlus.cpp  AD Unary Plus Operator: Example and Test  ",
"UnaryMinus  AD Unary Minus Operator   -",
"UnaryMinus.cpp  AD Unary Minus Operator: Example and Test  ",
"ad_binary  AD Binary Arithmetic Operators   + add plus - subtract minus * multiply times / divide",
"Add.cpp  AD Binary Addition: Example and Test   + plus",
"Sub.cpp  AD Binary Subtraction: Example and Test   - minus",
"Mul.cpp  AD Binary Multiplication: Example and Test   * multiply times",
"Div.cpp  AD Binary Division: Example and Test   / divide quotient",
"compute_assign  AD Computed Assignment Operators   + add plus - subtract minus * multiply times / divide multiple",
"AddEq.cpp  AD Computed Assignment Addition: Example and Test   += plus",
"SubEq.cpp  AD Computed Assignment Subtraction: Example and Test   -= plus add",
"MulEq.cpp  AD Computed Assignment Multiplication: Example and Test   *= multiply plus add",
"DivEq.cpp  AD Computed Assignment Division: Example and Test   /= divide plus add",
"std_math_ad  AD Standard Math Unary Functions   acos asin atan cosh exp log log10 sinh sqrt",
"Acos.cpp  The AD acos Function: Example and Test  ",
"Asin.cpp  The AD asin Function: Example and Test  ",
"Atan.cpp  The AD atan Function: Example and Test  ",
"Cos.cpp  The AD cos Function: Example and Test  ",
"Cosh.cpp  The AD cosh Function: Example and Test  ",
"Exp.cpp  The AD exp Function: Example and Test  ",
"Log.cpp  The AD log Function: Example and Test  ",
"Log10.cpp  The AD log10 Function: Example and Test  ",
"Sin.cpp  The AD sin Function: Example and Test  ",
"Sinh.cpp  The AD sinh Function: Example and Test  ",
"Sqrt.cpp  The AD sqrt Function: Example and Test  ",
"Tan.cpp  The AD tan Function: Example and Test  ",
"MathOther  Other AD Math Functions   atan2",
"abs  AD Absolute Value Function   value_ directional derivative",
"Abs.cpp  AD Absolute Value Function: Example and Test   derivative directional",
"atan2  AD Two Argument Inverse Tangent Function  ",
"Atan2.cpp  The AD atan2 Function: Example and Test  ",
"erf  The AD Error Function  ",
"Erf.cpp  The AD erf Function: Example and Test  ",
"pow  The AD Power Function   exponent",
"Pow.cpp  The AD Power Function: Example and Test  ",
"pow_int.cpp  The Pow Integer Exponent: Example and Test  ",
"CondExp  AD Conditional Expressions   assign",
"CondExp.cpp  Conditional Expressions: Example and Test  ",
"Discrete  Discrete AD Functions   cppad_discrete_function cppadcreatediscrete deprecated",
"TapeIndex.cpp  Taping Array Index Operation: Example and Test  ",
"interp_onetape.cpp  Interpolation With Out Retaping: Example and Test   interpolate retape",
"interp_retape.cpp  Interpolation With Retaping: Example and Test   interpolate",
"BoolValued  Bool Valued Operations and Functions with AD Arguments  ",
"Compare  AD Binary Comparison Operators   < <= > >= == !=",
"Compare.cpp  AD Binary Comparison Operators: Example and Test   < <= > >= == !=",
"NearEqualExt  Compare AD and Base Objects for Nearly Equal   with",
"NearEqualExt.cpp  Compare AD with Base Objects: Example and Test  ",
"BoolFun  AD Boolean Functions   unary binary cppad_bool_unary cppad_bool_binary",
"BoolFun.cpp  AD Boolean Functions: Example and Test  ",
"ParVar  Is an AD Object a Parameter or Variable  ",
"ParVar.cpp  AD Parameter and Variable Functions: Example and Test  ",
"EqualOpSeq  Check if Equal and Correspond to Same Operation Sequence  ",
"EqualOpSeq.cpp  EqualOpSeq: Example and Test  ",
"VecAD  AD Vectors that Record Index Operations   tape vecad<base>::reference",
"VecAD.cpp  AD Vectors that Record Index Operations: Example and Test  ",
"base_require  AD<Base> Requirements for Base Type   abs condexp equalopseq erf identical integer greaterthanzero lessthanorzero pow math",
"base_complex.hpp  Enable use of AD<Base> where Base is std::complex<double>  ",
"ComplexPoly.cpp  Complex Polynomial: Example and Test  ",
"not_complex_ad.cpp  Not Complex Differentiable: Example and Test   polynomial imag() real()",
"base_adolc.hpp  Enable use of AD<Base> where Base is Adolc's adouble Type  ",
"mul_level_adolc.cpp  Using Adolc with Multiple Levels of Taping: Example and Test  ",
"ADFun  ADFun Objects  ",
"Independent  Declare Independent Variables and Start Recording   openmp",
"Independent.cpp  Independent and ADFun Constructor: Example and Test  ",
"FunConstruct  Construct an ADFun Object and Stop Recording   tape openmp dependent",
"Dependent  Stop Recording and Store Operation Sequence   adfun tape",
"SeqProperty  ADFun Sequence Properties   domain range parameter use_vecad size_var",
"SeqProperty.cpp  ADFun Sequence Properties: Example and Test   domain range parameter size_var",
"FunEval  Evaluate ADFun Functions, Derivatives, and Sparsity Patterns  ",
"Forward  Forward Mode  ",
"ForwardZero  Zero Order Forward Mode: Function Values  ",
"ForwardOne  First Order Forward Mode: Derivative Values  ",
"ForwardAny  Any Order Forward Mode   derivative calculate",
"size_taylor  Number Taylor Coefficients, Per Variable, Currently Stored  ",
"CompareChange  Comparison Changes During Zero Order Forward Mode   adfun ndebug",
"CompareChange.cpp  CompareChange and Re-Tape: Example and Test  ",
"capacity_taylor  Controlling Taylor Coefficients Memory Allocation   forward",
"Forward.cpp  Forward Mode: Example and Test  ",
"Reverse  Reverse Mode  ",
"reverse_one  First Order Reverse Mode   derivative",
"reverse_two  Second Order Reverse Mode   derivative",
"reverse_any  Any Order Reverse Mode   derivative",
"reverse_one.cpp  First Order Reverse Mode: Example and Test  ",
"reverse_two.cpp  Second Order Reverse ModeExample and Test  ",
"HesTimesDir.cpp  Hessian Times Direction: Example and Test  ",
"reverse_any.cpp  Any Order Reverse Mode: Example and Test  ",
"Sparse  Calculating Sparsity Patterns  ",
"ForSparseJac  Jacobian Sparsity Pattern: Forward Mode  ",
"ForSparseJac.cpp  Forward Mode Jacobian Sparsity: Example and Test  ",
"RevSparseJac  Jacobian Sparsity Pattern: Reverse Mode  ",
"RevSparseJac.cpp  Reverse Mode Jacobian Sparsity: Example and Test  ",
"RevSparseHes  Hessian Sparsity Pattern: Reverse Mode  ",
"RevSparseHes.cpp  Reverse Mode Hessian Sparsity: Example and Test  ",
"Drivers  First and Second Derivatives: Easy Drivers   forward reverse",
"Jacobian  Jacobian: Driver Routine   first derivative",
"Jacobian.cpp  Jacobian: Example and Test  ",
"ForOne  First Order Partial Derivative: Driver Routine   easy",
"ForOne.cpp  First Order Partial Driver: Example and Test  ",
"RevOne  First Order Derivative: Driver Routine   easy",
"RevOne.cpp  First Order Derivative Driver: Example and Test  ",
"Hessian  Hessian: Easy Driver   second derivative",
"Hessian.cpp  Hessian: Example and Test  ",
"HesLagrangian.cpp  Hessian of Lagrangian and ADFun Default Constructor: Example and Test  ",
"ForTwo  Forward Mode Second Partial Derivative Driver   order easy",
"ForTwo.cpp  Subset of Second Order Partials: Example and Test  ",
"RevTwo  Reverse Mode Second Partial Derivative Driver   order easy",
"RevTwo.cpp  Second Partials Reverse Driver: Example and Test  ",
"FunCheck  Check an ADFun Sequence of Operations  ",
"FunCheck.cpp  ADFun Check and Re-Tape: Example and Test   dependent",
"omp_max_thread  OpenMP Maximum Thread Number   multiple cppad",
"openmp_run.sh  Compile and Run the OpenMP Test   example",
"example_a11c.cpp  A Simple Parallel Loop   openmp a.1.1c",
"multi_newton.cpp  Multi-Threaded Newton's Method Main Program   openmp example",
"multi_newton  Multi-Threaded Newton's Method Routine   openmp example",
"multi_newton.hpp  OpenMP Multi-Threading Newton's Method Source Code   example",
"sum_i_inv.cpp  Sum of 1/i Main Program   openmp example",
"FunDeprecated  ADFun Object Deprecated Member Functions   dependent order memory size taylor_size",
"library  The CppAD General Purpose Library   numerical c++ template concept",
"ErrorHandler  Replacing the CppAD Error Handler   replace assert exception",
"ErrorHandler.cpp  Replacing The CppAD Error Handler: Example and Test  ",
"cppad_assert  CppAD Assertions During Execution   error macro cppad_assert_known cppad_assert_unknown",
"NearEqual  Determine if Two Values Are Nearly Equal   absolute difference relative exercise",
"Near_Equal.cpp  NearEqual Function: Example and Test  ",
"speed_test  Run One Speed Test and Return Results  ",
"SpeedTest  Run One Speed Test and Print Results  ",
"speed_test.cpp  speed_test: Example and test  ",
"SpeedTest.cpp  Example Use of SpeedTest  ",
"NumericType  Definition of a Numeric Type   default constructor int copy exercise",
"NumericType.cpp  The NumericType: Example and Test  ",
"CheckNumericType  Check NumericType Class Concept  ",
"CheckNumericType.cpp  The CheckNumericType Function: Example and Test  ",
"SimpleVector  Definition of a Simple Vector   class template default constructor size copy element destructor assignment resize value_type [] exercise ndebug",
"SimpleVector.cpp  Simple Vector Template Class: Example and Test  ",
"CheckSimpleVector  Check Simple Vector Concept  ",
"CheckSimpleVector.cpp  The CheckSimpleVector Function: Example and Test  ",
"nan  Obtain Nan and Determine if a Value is Nan   isnan hasnan macro",
"nan.cpp  nan: Example and Test  ",
"pow_int  The Integer Power Function   exponent",
"Poly  Evaluate a Polynomial or its Derivative   template",
"Poly.cpp  Polynomial Evaluation: Example and Test  ",
"poly.hpp  Source: Poly  ",
"LuDetAndSolve  Compute Determinants and Solve Equations by LU Factorization   matrix linear",
"LuSolve  Compute Determinant and Solve Linear Equations  ",
"LuSolve.cpp  LuSolve With Complex Arguments: Example and Test  ",
"lu_solve.hpp  Source: LuSolve  ",
"LuFactor  LU Factorization of A Square Matrix   linear equation determinant solve",
"LuFactor.cpp  LuFactor: Example and Test  ",
"lu_factor.hpp  Source: LuFactor  ",
"LuInvert  Invert an LU Factored Equation   linear",
"LuInvert.cpp  LuInvert: Example and Test  ",
"lu_invert.hpp  Source: LuInvert  ",
"RombergOne  One DimensionalRomberg Integration   integrate",
"RombergOne.cpp  One Dimensional Romberg Integration: Example and Test  ",
"RombergMul  Multi-dimensional Romberg Integration   integrate",
"RombergMul.cpp  One Dimensional Romberg Integration: Example and Test  ",
"Runge45  An Embedded 4th and 5th Order Runge-Kutta ODE Solver   differential equation",
"Runge45.cpp  Runge45: Example and Test  ",
"Rosen34  A 3rd and 4th Order Rosenbrock ODE Solver   stiff differential equation",
"Rosen34.cpp  Rosen34: Example and Test  ",
"OdeErrControl  An Error Controller for ODE Solvers   differential equation",
"OdeErrControl.cpp  OdeErrControl: Example and Test  ",
"OdeErrMaxabs.cpp  OdeErrControl: Example and Test Using Maxabs Argument  ",
"OdeGear  An Arbitrary Order Gear Method   stiff differential equation",
"OdeGear.cpp  OdeGear: Example and Test  ",
"OdeGearControl  An Error Controller for Gear's Ode Solvers   differential equation",
"OdeGearControl.cpp  OdeGearControl: Example and Test  ",
"BenderQuad  Computing Jacobian and Hessian of Bender's Reduced Objective  ",
"BenderQuad.cpp  BenderQuad: Example and Test  ",
"LuRatio  LU Factorization of A Square Matrix and Stability Calculation   linear equation determinant solve",
"LuRatio.cpp  LuRatio: Example and Test  ",
"std_math_unary  Float and Double Standard Math Unary Functions   abs acos asin atan cosh exp log log10 sinh sqrt tanh",
"CppAD_vector  The CppAD::vector Template Class   assignment [] push_back vectorbool exercise",
"CppAD_vector.cpp  CppAD::vector Template Class: Example and Test  ",
"vectorBool.cpp  CppAD::vectorBool Class: Example and Test  ",
"TrackNewDel  Routines That Track Use of New and Delete   memory openmp tracknewvec ndebug cppad_track_new_vec cppadtracknewvec trackdelvec cppad_track_del_vec cppadtrackdelvec trackextend cppad_track_extend cppadtrackextend trackcount",
"TrackNewDel.cpp  Tracking Use of New and Delete: Example and Test  ",
"preprocessor  Preprocessor Definitions Used by CppAD   symbol package version",
"Example  Examples  ",
"General  General Examples   realistic",
"ExampleUtility  Utility Routines used by CppAD Examples  ",
"ListAllExamples  List of All the CppAD Examples  ",
"Interface2C.cpp  Interfacing to C: Example and Test   difference central",
"JacMinorDet.cpp  Gradient of Determinant Using Expansion by Minors: Example and Test  ",
"JacLuDet.cpp  Gradient of Determinant Using Lu Factorization: Example and Test  ",
"HesMinorDet.cpp  Gradient of Determinant Using Expansion by Minors: Example and Test  ",
"HesLuDet.cpp  Gradient of Determinant Using LU Factorization: Example and Test  ",
"OdeStiff.cpp  A Stiff Ode: Example and Test  ",
"ode_taylor.cpp  Taylor's Ode Solver: An Example and Test  ",
"ode_taylor_adolc.cpp  Using Adolc with Taylor's Ode Solver: An Example and Test  ",
"StackMachine.cpp  Example Differentiating a Stack Machine Interpreter   test",
"mul_level  Using Multiple Levels of AD  ",
"mul_level.cpp  Multiple Tapes: Example and Test   ad",
"Example.cpp  Program That Runs the CppAD Examples   all",
"speed_example.cpp  Program That Runs the Speed Examples  ",
"LuVecAD  Lu Factor and Solve with Recorded Pivoting   linear equation determinant",
"LuVecADOk.cpp  Lu Factor and Solve With Recorded Pivoting: Example and Test  ",
"test_vector  Choosing The Vector Testing Template Class   cppad_test_vector cppadvector deprecated",
"Appendix  Appendix  ",
"Faq  Frequently Asked Questions and Answers   assignment operator independent bugs reporting comparechange ndebug complex test exception errorhandler variable tape avoid record speed taping math functions inverse matrix forward reverse mode namespace cppad_test_vector preprocessor symbols standard using storage memory disk",
"speed  Speed Test Routines   windows",
"speed_main  Speed Testing Main Program   cppad det_lu correct det_minor poly uniform_01",
"speed_utility  Speed Testing Utilities  ",
"uniform_01  Simulate a [0,1] Uniform Random Variate   vector",
"uniform_01.hpp  Source: uniform_01  ",
"det_of_minor  Determinant of a Minor   matrix",
"det_of_minor.cpp  Determinant of a Minor: Example and Test  ",
"det_of_minor.hpp  Source: det_of_minor  ",
"det_by_minor  Determinant Using Expansion by Minors  ",
"det_by_minor.cpp  Determinant Using Expansion by Minors: Example and Test  ",
"det_by_minor.hpp  Source: det_by_minor  ",
"det_by_lu  Determinant Using Expansion by Lu Factorization  ",
"det_by_lu.cpp  Determinant Using Lu Factorization: Example and Test   minors",
"det_by_lu.hpp  Source: det_by_lu  ",
"det_33  Check Determinant of 3 by 3 matrix   correct",
"det_33.hpp  Source: det_33  ",
"det_grad_33  Check Gradient of Determinant of 3 by 3 matrix   correct",
"det_grad_33.hpp  Source: det_grad_33  ",
"speed_double  Speed Test Functions in Double  ",
"double_det_minor.cpp  Double Speed: Determinant Using Expansion by Minors   cppad compute_det_minor",
"double_det_lu.cpp  Double Speed: Determinant Using Lu Factorization   compute_det_lu",
"double_poly.cpp  Double Speed: Evaluate a Polynomial   cppad compute_poly",
"speed_adolc  Speed Test Derivatives Using Adolc  ",
"adolc_det_minor.cpp  Adolc Speed: Gradient of Determinant Using Expansion by Minors   compute_det_minor",
"adolc_det_lu.cpp  Adolc Speed: Gradient of Determinant Using Lu Factorization   compute_det_lu",
"adolc_poly.cpp  Adolc Speed: Second Derivative of a Polynomial   compute_poly",
"speed_cppad  Speed Test Derivatives Using CppAD  ",
"cppad_det_minor.cpp  CppAD Speed: Gradient of Determinant Using Expansion by Minors   compute_det_minor",
"cppad_det_lu.cpp  CppAD Speed: Gradient of Determinant Using Lu Factorization   compute_det_lu",
"cppad_poly.cpp  CppAD Speed: Second Derivative of a Polynomial   compute_poly",
"speed_fadbad  Speed Test Derivatives Using Fadbad  ",
"fadbad_det_minor.cpp  Fadbad Speed: Gradient of Determinant Using Expansion by Minors   compute_det_minor",
"fadbad_det_lu.cpp  Fadbad Speed: Gradient of Determinant Using Lu Factorization   compute_det_lu",
"fadbad_poly.cpp  Fadbad Speed: Second Derivative of a Polynomial   compute_poly",
"speed_sacado  Speed Test Derivatives Using Sacado  ",
"sacado_det_minor.cpp  Sacado Speed: Gradient of Determinant Using Expansion by Minors   compute_det_minor",
"sacado_det_lu.cpp  Sacado Speed: Gradient of Determinant Using Lu Factorization   compute_det_lu",
"sacado_poly.cpp  Sacado Speed: Second Derivative of a Polynomial   compute_poly",
"Theory  The Theory of Derivative Calculations  ",
"ForwardTheory  The Theory of Forward Mode  ",
"ExpForward  Exponential Function Forward Taylor Polynomial Theory  ",
"LogForward  Logarithm Function Forward Taylor Polynomial Theory  ",
"SqrtForward  Square Root Function Forward Taylor Polynomial Theory  ",
"SinCosForward  Trigonometric and Hyperbolic Sine and Cosine Forward Theory   sinh cosh",
"AtanForward  Arctangent Function Forward Taylor Polynomial Theory  ",
"AsinForward  Arcsine Function Forward Taylor Polynomial Theory  ",
"AcosForward  Arccosine Function Forward Taylor Polynomial Theory  ",
"ReverseTheory  The Theory of Reverse Mode  ",
"ExpReverse  Exponential Function Reverse Mode Theory  ",
"LogReverse  Logarithm Function Reverse Mode Theory  ",
"SqrtReverse  Square Root Function Reverse Mode Theory  ",
"SinCosReverse  Trigonometric and Hyperbolic Sine and Cosine Reverse Theory   sinh cosh",
"AtanReverse  Arctangent Function Reverse Mode Theory  ",
"AsinReverse  Arcsine Function Reverse Mode Theory  ",
"AcosReverse  Arccosine Function Reverse Mode Theory  ",
"reverse_identity  An Important Reverse Mode Identity  ",
"glossary  Glossary   ad function of base levels above type elementary vector operation atomic sequence dependent independent parameter sparsity pattern efficient tape active inactive variable variables taylor coefficient",
"Bib  Bibliography  ",
"Bugs  Know Bugs and Problems Using CppAD   gcc 3.4.4",
"WishList  The CppAD Wish List   new features atan2 condexp sequence operation optimize tape operations",
"whats_new  Changes and Additions to CppAD  ",
"whats_new_07  Changes and Additions to CppAD During 2007  ",
"whats_new_06  Changes and Additions to CppAD During 2006   aclocal",
"whats_new_05  Changes and Additions to CppAD During 2005  ",
"whats_new_04  Changes and Additions to CppAD During 2004  ",
"whats_new_03  Changes and Additions to CppAD During 2003  ",
"include_deprecated  Deprecated Include Files  ",
"License  Your License for the CppAD Software  "
]

var MaxList = 100;
var Choice  = "";
var Nstring = -1;
var Nkeyword = Keyword.length;
Initialize();

function Initialize()
{
	var i;
	var line;
	for(i = 0; (i < Nkeyword) && (i < MaxList) ; i++)
	{
		line       = Keyword[i].split(/\s+/)
		line[0]    = line[0].toUpperCase();
		line       = line.join(" ");
		Keyword[i] = line;
	}
	Search();
	document.search.string.focus();
}
function Search()
{
	var string  = document.search.string.value;
	if( Nstring == string.length )
		return;
	Nstring     = string.length;

	var word    = string.match(/\S+/g);
	var nword   = 0;
	if(word != null )
		nword   = word.length;

	var pattern = new Array(nword);
	for(var j = 0; j < nword; j++)
		pattern[j] = new RegExp(word[j], "i");

	var nlist = 0;
	var list  = "";
	Choice    = "";

	for(i = 0; (i < Nkeyword) && (nlist < MaxList) ; i++)
	{
		var match = true;
		for(j = 0; j < nword; j++)
			match = match && pattern[j].test(Keyword[i]);

		if( match )
		{
			line     = Keyword[i].split(/\s+/);

			if( Choice == "" )
				Choice = line[0];

			line  = line.join(" ");
			list  = list + line + "\n";
			nlist = nlist + 1;
		}
	}
	document.search.choice.value  = Choice.toLowerCase();
	document.search.list.value    = list;
}
function choose()
{
parent.location = document.search.choice.value.toLowerCase() + ".xml";
}
