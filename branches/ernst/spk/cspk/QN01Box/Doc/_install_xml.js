var list_across0 = [
'_contents_xml.htm',
'_reference.xml',
'_index.xml',
'_search_xml.htm',
'_external.xml'
];
var list_up0 = [
'qn01box.xml',
'install.xml'
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
'install.xml#Get Tar File',
'install.xml#Extraction',
'install.xml#Configure',
'install.xml#PrefixDir and PostfixDir',
'install.xml#CppADPrefixDir and CppADPostfixDir',
'install.xml#CppErrorsAndWarnings',
'install.xml#make',
'install.xml#Installation'
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
