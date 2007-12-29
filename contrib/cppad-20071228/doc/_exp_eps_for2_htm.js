var list_across0 = [
'_contents.htm',
'_reference.htm',
'_index.htm',
'_search.htm',
'_external.htm'
];
var list_up0 = [
'cppad.htm',
'introduction.htm',
'exp_eps.htm',
'exp_eps_for2.htm'
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
'get_started.cpp.htm',
'exp_2.htm',
'exp_eps.htm',
'exp_apx_main.cpp.htm'
];
var list_down1 = [
'exp_eps.hpp.htm',
'exp_eps.cpp.htm',
'exp_eps_for0.htm',
'exp_eps_for1.htm',
'exp_eps_rev1.htm',
'exp_eps_for2.htm',
'exp_eps_rev2.htm',
'exp_eps_cppad.htm'
];
var list_down0 = [
'exp_eps_for2.cpp.htm'
];
var list_current0 = [
'exp_eps_for2.htm#Second Order Expansion',
'exp_eps_for2.htm#Purpose',
'exp_eps_for2.htm#Mathematical Form',
'exp_eps_for2.htm#Operation Sequence',
'exp_eps_for2.htm#Operation Sequence.Index',
'exp_eps_for2.htm#Operation Sequence.Zero',
'exp_eps_for2.htm#Operation Sequence.Operation',
'exp_eps_for2.htm#Operation Sequence.First',
'exp_eps_for2.htm#Operation Sequence.Derivative',
'exp_eps_for2.htm#Operation Sequence.Second',
'exp_eps_for2.htm#Operation Sequence.Sweep',
'exp_eps_for2.htm#Return Value',
'exp_eps_for2.htm#Verification',
'exp_eps_for2.htm#Exercises'
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
