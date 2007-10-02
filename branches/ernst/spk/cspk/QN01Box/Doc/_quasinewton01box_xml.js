var list_across0 = [
'_contents_xml.htm',
'_reference.xml',
'_index.xml',
'_search_xml.htm',
'_external.xml'
];
var list_up0 = [
'qn01box.xml',
'quasinewton01box.xml'
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
var list_down0 = [
'quadbox.xml',
'bfgs.xml'
];
var list_current0 = [
'quasinewton01box.xml#Original Problem',
'quasinewton01box.xml#Convention',
'quasinewton01box.xml#Convention.Arguments',
'quasinewton01box.xml#Convention.Exceptions',
'quasinewton01box.xml#Return Value',
'quasinewton01box.xml#ItrMax',
'quasinewton01box.xml#QuadMax',
'quasinewton01box.xml#n',
'quasinewton01box.xml#norm',
'quasinewton01box.xml#delta',
'quasinewton01box.xml#Fun obj',
'quasinewton01box.xml#Fun obj.Objective Function',
'quasinewton01box.xml#Fun obj.Gradient',
'quasinewton01box.xml#Fun obj.Hessian',
'quasinewton01box.xml#sOkCur',
'quasinewton01box.xml#ItrCur',
'quasinewton01box.xml#QuadCur',
'quasinewton01box.xml#BfgsCur',
'quasinewton01box.xml#rCur',
'quasinewton01box.xml#fCur',
'quasinewton01box.xml#xCur',
'quasinewton01box.xml#sCur',
'quasinewton01box.xml#sCur.Quadratic Subproblem',
'quasinewton01box.xml#sCur.Discussion',
'quasinewton01box.xml#gCur',
'quasinewton01box.xml#HCur',
'quasinewton01box.xml#os',
'quasinewton01box.xml#level',
'quasinewton01box.xml#level.level &gt; 0',
'quasinewton01box.xml#level.abs(level) &gt;= 1',
'quasinewton01box.xml#level.abs(level) &gt;= 2',
'quasinewton01box.xml#level.abs(level) &gt;= 3',
'quasinewton01box.xml#level.abs(level) &gt;= 4',
'quasinewton01box.xml#level.abs(level) &gt;= 5',
'quasinewton01box.xml#Subroutines',
'quasinewton01box.xml#Example'
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
