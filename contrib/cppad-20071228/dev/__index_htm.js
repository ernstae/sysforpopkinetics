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
'_index.htm#A',
'_index.htm#B',
'_index.htm#C',
'_index.htm#D',
'_index.htm#E',
'_index.htm#F',
'_index.htm#G',
'_index.htm#I',
'_index.htm#L',
'_index.htm#M',
'_index.htm#N',
'_index.htm#O',
'_index.htm#P',
'_index.htm#R',
'_index.htm#S',
'_index.htm#T',
'_index.htm#U',
'_index.htm#Z'
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
