var list_across0 = [
'_contents_xml.htm',
'_reference.xml',
'_index.xml',
'_search_xml.htm',
'_external.xml'
];
var list_up0 = [
'qn01box.xml',
'glossary.xml'
];
var list_down1 = [
'license.xml',
'install.xml',
'quasinewton01box.xml',
'converge.xml',
'utility.xml',
'glossary.xml',
'whatsnew.xml'
];
var list_current0 = [
'glossary.xml#avg: Euclidean Average',
'glossary.xml#C: The Complementarity Conditions',
'glossary.xml#D: Diagonal Matrix of a Vector',
'glossary.xml#Euclidean Norm',
'glossary.xml#Exception Safe',
'glossary.xml#Infinity Norm',
'glossary.xml#L-one Norm',
'glossary.xml#Matrix and Vector Correspondence',
'glossary.xml#e: Vector of Ones',
'glossary.xml#max: Maximum Element of a Vector',
'glossary.xml#min: Minimum Element of a Vector',
'glossary.xml#L: The Lagrangian',
'glossary.xml#P: Quadratic Problem with Box Constraints',
'glossary.xml#p: Scaled Projected Gradient',
'glossary.xml#Residual Function'
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
