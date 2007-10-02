var list_across0 = [
'_contents_xml.htm',
'_reference.xml',
'_index.xml',
'_search_xml.htm',
'_external.xml'
];
var list_up0 = [
'qn01box.xml',
'quasinewton01box.xml',
'quadbox.xml'
];
var list_down2 = [
'license.xml',
'install.xml',
'quasinewton01box.xml',
'converge.xml',
'utility.xml',
'glossary.xml',
'whatsnew.xml'
];
var list_down1 = [
'quadbox.xml',
'bfgs.xml'
];
var list_down0 = [
'next.xml',
'residual.xml'
];
var list_current0 = [
'quadbox.xml#Exceptions',
'quadbox.xml#Description',
'quadbox.xml#Return Value',
'quadbox.xml#Input Arguments',
'quadbox.xml#n, Q, r, l, u',
'quadbox.xml#n, Q, r, l, u.level',
'quadbox.xml#n, Q, r, l, u.os',
'quadbox.xml#n, Q, r, l, u.eIn',
'quadbox.xml#Input and Output Arguments',
'quadbox.xml#Input and Output Arguments.k',
'quadbox.xml#Input and Output Arguments.x,a,b',
'quadbox.xml#eOut',
'quadbox.xml#Subroutines',
'quadbox.xml#Example'
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
