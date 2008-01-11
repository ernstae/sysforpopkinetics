// ------------------------------------------------------- 
// Copyright (C) Bradley M. Bell 2003, All rights reserved 
// ------------------------------------------------------- 
Keyword = 
[
"mat2cpp  Example Conversion of Matlab or Octave to C++ using Ublas   library link",
"License  The mat2cpp Software License  ",
"getstarted  Getting Started   output matrix",
"operation  Elementary Operations  ",
"elementwise  Elementwise Operations  ",
"add  Elementwise Addition  ",
"element_div  Elementwise Division  ",
"element_prod  Elementwise Product  ",
"element_unary  Matlab or Octave Elementwise Unary Functions   math",
"element_unary.cpp  Elementwise Unary Functions Source Code  ",
"scalar_div  Scalar Division  ",
"scalar_prod  Scalar Product  ",
"sub  Elementwise Subtraction  ",
"matrixwise  Matrixwise Operations  ",
"diag_prod  Diagonal Matrix Product with General Matrix  ",
"diag_prod.cpp  Diagonal Matrix Product with General Matrix Source Code  ",
"cholesky  Cholesky Factor a Matrix Division  ",
"cholesky.cpp  Cholesky Factor a Matrix Source Code  ",
"matrix_div  Matrix Division  ",
"matrix_div.cpp  Matrix Division Source Code  ",
"matrix_prod  Matrix Product   multiply times",
"transpose  Matrix Transpose  ",
"scalar_valued  Scalar Valued Operations  ",
"max  Maximum Element of a Matrix  ",
"max.cpp  Maximum Element of a Matrix Source code  ",
"min  Minimum Element of a Matrix  ",
"min.cpp  Minimum Element of a Matrix Source Code  ",
"scalar  Convert Matrix to Scalar  ",
"scalar.cpp  Convert Matrix to Scalar Source Code  ",
"sum  Sum The Elements of a Matlab  ",
"sum.cpp  Sum The Elements of a Matlab Source Code  ",
"other  Other Operations  ",
"extend  Extending a Vector  ",
"ones  Matrix of Ones  ",
"rand  Uniform Random Matrix  ",
"rand.cpp  Uniform Random Matrix Source Code  ",
"randn  Normal Random Matrix   simulate gaussian",
"randn.cpp  Normal Random Matrix Source Code  ",
"size  Matrix Size  ",
"slice  Matrix Slices   sub-matrix",
"zeros  Matrix of Zeros  ",
"mat2cpp.hpp  The Matlab or Octave to C++ Using Ublas Include File  ",
"mat2cpp_ok.m  Run Matlab or Octave Version of Examples  ",
"mat2cpp_ok.cpp  Run C++ Version of Examples  ",
"News  The mat2cpp News file  ",
"library  Adding a New Routine to the Mat2cpp Library  "
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
	Search()
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
parent.location = document.search.choice.value.toLowerCase() + ".htm";
}
