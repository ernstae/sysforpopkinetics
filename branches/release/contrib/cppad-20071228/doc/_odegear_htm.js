var list_across0 = [
'_contents.htm',
'_reference.htm',
'_index.htm',
'_search.htm',
'_external.htm'
];
var list_up0 = [
'cppad.htm',
'library.htm',
'odegear.htm'
];
var list_down2 = [
'install.htm',
'introduction.htm',
'ad.htm',
'adfun.htm',
'library.htm',
'preprocessor.htm',
'example.htm',
'appendix.htm'
];
var list_down1 = [
'errorhandler.htm',
'nearequal.htm',
'speed_test.htm',
'speedtest.htm',
'numerictype.htm',
'checknumerictype.htm',
'simplevector.htm',
'checksimplevector.htm',
'nan.htm',
'pow_int.htm',
'poly.htm',
'ludetandsolve.htm',
'rombergone.htm',
'rombergmul.htm',
'runge45.htm',
'rosen34.htm',
'odeerrcontrol.htm',
'odegear.htm',
'odegearcontrol.htm',
'benderquad.htm',
'luratio.htm',
'std_math_unary.htm',
'cppad_vector.htm',
'tracknewdel.htm'
];
var list_down0 = [
'odegear.cpp.htm'
];
var list_current0 = [
'odegear.htm#Syntax',
'odegear.htm#Purpose',
'odegear.htm#Include',
'odegear.htm#Fun',
'odegear.htm#Fun.t',
'odegear.htm#Fun.x',
'odegear.htm#Fun.f',
'odegear.htm#Fun.f_x',
'odegear.htm#Fun.Warning',
'odegear.htm#m',
'odegear.htm#n',
'odegear.htm#T',
'odegear.htm#X',
'odegear.htm#e',
'odegear.htm#Scalar',
'odegear.htm#Vector',
'odegear.htm#Example',
'odegear.htm#Source Code',
'odegear.htm#Theory',
'odegear.htm#Gear\'s Method'
];
function choose_across0(item)
{	var index          = item.selectedIndex;
	item.selectedIndex = 0;
	if(index > 0)
		document.location = list_across0[index-1];
}
function choose_up0(item)
{	var index          = item.selectedIndex;
	item.selectedIndex = 0;
	if(index > 0)
		document.location = list_up0[index-1];
}
function choose_down2(item)
{	var index          = item.selectedIndex;
	item.selectedIndex = 0;
	if(index > 0)
		document.location = list_down2[index-1];
}
function choose_down1(item)
{	var index          = item.selectedIndex;
	item.selectedIndex = 0;
	if(index > 0)
		document.location = list_down1[index-1];
}
function choose_down0(item)
{	var index          = item.selectedIndex;
	item.selectedIndex = 0;
	if(index > 0)
		document.location = list_down0[index-1];
}
function choose_current0(item)
{	var index          = item.selectedIndex;
	item.selectedIndex = 0;
	if(index > 0)
		document.location = list_current0[index-1];
}
