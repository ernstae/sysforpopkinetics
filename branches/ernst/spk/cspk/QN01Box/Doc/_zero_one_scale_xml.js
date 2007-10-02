var list_across0 = [
'_contents_xml.htm',
'_reference.xml',
'_index.xml',
'_search_xml.htm',
'_external.xml'
];
var list_up0 = [
'qn01box.xml',
'utility.xml',
'zero_one_scale.xml'
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
'memory.xml',
'maxabs.xml',
'sumabs.xml',
'plusinfinity.xml',
'scaleprojgrad.xml',
'positivematrix.xml',
'zero_one_scale.xml',
'error.xml'
];
var list_current0 = [
'zero_one_scale.xml#Constructor',
'zero_one_scale.xml#Purpose',
'zero_one_scale.xml#Notation',
'zero_one_scale.xml#fun',
'zero_one_scale.xml#m',
'zero_one_scale.xml#n',
'zero_one_scale.xml#a',
'zero_one_scale.xml#b',
'zero_one_scale.xml#to_zero_one',
'zero_one_scale.xml#from_zero_one',
'zero_one_scale.xml#function',
'zero_one_scale.xml#gradient',
'zero_one_scale.xml#Hessian',
'zero_one_scale.xml#msg',
'zero_one_scale.xml#Example'
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
