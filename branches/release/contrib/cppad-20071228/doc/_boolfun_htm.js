var list_across0 = [
'_contents.htm',
'_reference.htm',
'_index.htm',
'_search.htm',
'_external.htm'
];
var list_up0 = [
'cppad.htm',
'ad.htm',
'boolvalued.htm',
'boolfun.htm'
];
var list_down3 = [
'install.htm',
'introduction.htm',
'ad.htm',
'adfun.htm',
'library.htm',
'preprocessor.htm',
'example.htm',
'appendix.htm'
];
var list_down2 = [
'default.htm',
'ad_copy.htm',
'convert.htm',
'advalued.htm',
'boolvalued.htm',
'vecad.htm',
'base_require.htm'
];
var list_down1 = [
'compare.htm',
'nearequalext.htm',
'boolfun.htm',
'parvar.htm',
'equalopseq.htm'
];
var list_down0 = [
'boolfun.cpp.htm'
];
var list_current0 = [
'boolfun.htm#Syntax',
'boolfun.htm#Purpose',
'boolfun.htm#unary_name',
'boolfun.htm#u',
'boolfun.htm#x',
'boolfun.htm#b',
'boolfun.htm#Create Unary',
'boolfun.htm#binary_name',
'boolfun.htm#v',
'boolfun.htm#y',
'boolfun.htm#Create Binary',
'boolfun.htm#Operation Sequence',
'boolfun.htm#Example',
'boolfun.htm#Deprecated'
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
function choose_down3(item)
{	var index          = item.selectedIndex;
	item.selectedIndex = 0;
	if(index > 0)
		document.location = list_down3[index-1];
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
