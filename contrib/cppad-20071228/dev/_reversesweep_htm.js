var list_across0 = [
'_contents.htm',
'_reference.htm',
'_index.htm',
'_search.htm',
'_external.htm'
];
var list_down1 = [
'distribute.htm',
'newfeature.htm',
'define.htm',
'greaterthanzero.htm',
'greaterthanorzero.htm',
'lessthanzero.htm',
'lessthanorzero.htm',
'identicalpar.htm',
'identicalzero.htm',
'identicalone.htm',
'identicalequalpar.htm',
'opcode.htm',
'printop.htm',
'numind.htm',
'numvar.htm',
'tape_link.htm',
'taperec.htm',
'adtape.htm',
'boolfunlink.htm',
'op.htm',
'forwardsweep.htm',
'reversesweep.htm',
'forjacsweep.htm',
'revjacsweep.htm'
];
var list_current0 = [
'reversesweep.htm#Syntax',
'reversesweep.htm#Rec',
'reversesweep.htm#G',
'reversesweep.htm#numvar',
'reversesweep.htm#J',
'reversesweep.htm#K',
'reversesweep.htm#Taylor',
'reversesweep.htm#On Input',
'reversesweep.htm#On Input.Dependent Variables',
'reversesweep.htm#On Input.Other Variables',
'reversesweep.htm#On Output',
'reversesweep.htm#On Output.Independent Variables',
'reversesweep.htm#On Output.Other Variables'
];
function choose_across0(item)
{	var index          = item.selectedIndex;
	item.selectedIndex = 0;
	if(index > 0)
		document.location = list_across0[index-1];
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
