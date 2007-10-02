var list_across0 = [
'_contents_xml.htm',
'_reference.xml',
'_index.xml',
'_search_xml.htm',
'_external.xml'
];
var list_up0 = [
'qn01box.xml',
'whatsnew.xml'
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
'whatsnew.xml#06-07-28',
'whatsnew.xml#06-05-04',
'whatsnew.xml#05-12-12',
'whatsnew.xml#05-12-08',
'whatsnew.xml#05-12-07',
'whatsnew.xml#05-12-05',
'whatsnew.xml#05-11-21',
'whatsnew.xml#05-11-15',
'whatsnew.xml#05-11-14',
'whatsnew.xml#05-10-25',
'whatsnew.xml#05-10-28'
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
