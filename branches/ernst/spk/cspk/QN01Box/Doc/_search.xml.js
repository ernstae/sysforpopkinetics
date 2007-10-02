// ------------------------------------------------------- 
// Copyright (C) Bradley M. Bell 2003, All rights reserved 
// ------------------------------------------------------- 
Keyword = 
[
"QN01Box  A Box Constrained, Interior Point, Trust Region, Quasi-Newton Method   namespace qnotbox",
"License  Your License To Use This Program  ",
"Install  Install QN01Box  ",
"QuasiNewton01Box  Nonlinear Optimization with [0, 1] Box Constraints  ",
"QuadBox  Quadratic Problem with Box Constraints: Approximate Complementarity  ",
"Next  Next Interior Point Iterate  ",
"Delta  First Order Approximation Step  ",
"Residual  Central Path Residual  ",
"Bfgs  The Broyden-Fletcher-Goldfarb-Shanno Update  ",
"Converge  Choosing a Scaled Projected Gradient Convergence Criteria  ",
"Utility  General Purpose Utilities  ",
"Memory  A Fast and Simple Memory Management Template Class  ",
"MaxAbs  Maximum Absolute Value of an Element of a Vector  ",
"SumAbs  Sum of Absolute Value of Elements of a Vector  ",
"PlusInfinity  Returns the IEEE Floating Point Value Plus Infinity  ",
"ScaleProjGrad  Compute the Scaled Projected Gradient  ",
"PositiveMatrix  Ensure that A Symmetric Matrix is Positive Definite  ",
"zero_one_scale  Zero One Scale a Function's Argument Vector  ",
"Error  QN01Box Error Messages and Traps   handler assert exception",
"glossary  Glossary   avg: euclidean average c: the complementarity conditions d: diagonal matrix of vector norm exception safe infinity l-one and correspondence e: ones max: maximum element min: minimum l: lagrangian lagrange p: quadratic problem with box constraints trust region scaled projected gradient residual function",
"WhatsNew  Record of New Features and Bug Fixes For QN01Box  "
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
	UpdateList();
	document.search.string.focus();
}
function UpdateList(event)
{
	key = 0;
	if( window.event )
		key = window.event.keyCode;
	else if( event )
		key = event.which;
	if( key == 13 )
	{	Choose();
		return;
	}
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
function Choose()
{
parent.location = document.search.choice.value.toLowerCase() + ".xml";
}
