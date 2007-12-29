var list_across0 = [
'_contents_xml.htm',
'_reference.xml',
'_index.xml',
'_search_xml.htm',
'_external.xml'
];
var list_up0 = [
'cppad.xml',
'appendix.xml',
'speed.xml',
'speed_main.xml'
];
var list_down3 = [
'install.xml',
'introduction.xml',
'ad.xml',
'adfun.xml',
'library.xml',
'preprocessor.xml',
'example.xml',
'appendix.xml'
];
var list_down2 = [
'faq.xml',
'speed.xml',
'theory.xml',
'glossary.xml',
'bib.xml',
'bugs.xml',
'wishlist.xml',
'whats_new.xml',
'include_deprecated.xml',
'license.xml'
];
var list_down1 = [
'speed_main.xml',
'speed_utility.xml',
'speed_double.xml',
'speed_adolc.xml',
'speed_cppad.xml',
'speed_fadbad.xml',
'speed_sacado.xml'
];
var list_current0 = [
'speed_main.xml#Syntax',
'speed_main.xml#Purpose',
'speed_main.xml#package',
'speed_main.xml#package.AD Package',
'speed_main.xml#package.double',
'speed_main.xml#package.profile',
'speed_main.xml#option',
'speed_main.xml#option.correct',
'speed_main.xml#option.speed',
'speed_main.xml#det_lu',
'speed_main.xml#det_lu.size',
'speed_main.xml#det_lu.repeat',
'speed_main.xml#det_lu.matrix',
'speed_main.xml#det_lu.gradient',
'speed_main.xml#det_minor',
'speed_main.xml#det_minor.size',
'speed_main.xml#det_minor.repeat',
'speed_main.xml#det_minor.matrix',
'speed_main.xml#det_minor.gradient',
'speed_main.xml#poly',
'speed_main.xml#poly.size',
'speed_main.xml#poly.repeat',
'speed_main.xml#poly.a',
'speed_main.xml#poly.z',
'speed_main.xml#poly.ddp',
'speed_main.xml#seed',
'speed_main.xml#Correctness Results',
'speed_main.xml#Speed Results'
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
