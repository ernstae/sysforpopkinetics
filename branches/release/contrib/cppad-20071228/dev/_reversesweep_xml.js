var list_across0 = [
'_contents_xml.htm',
'_reference.xml',
'_index.xml',
'_search_xml.htm',
'_external.xml'
];
var list_down1 = [
'distribute.xml',
'newfeature.xml',
'define.xml',
'greaterthanzero.xml',
'greaterthanorzero.xml',
'lessthanzero.xml',
'lessthanorzero.xml',
'identicalpar.xml',
'identicalzero.xml',
'identicalone.xml',
'identicalequalpar.xml',
'opcode.xml',
'printop.xml',
'numind.xml',
'numvar.xml',
'tape_link.xml',
'taperec.xml',
'adtape.xml',
'boolfunlink.xml',
'op.xml',
'forwardsweep.xml',
'reversesweep.xml',
'forjacsweep.xml',
'revjacsweep.xml'
];
var list_current0 = [
'reversesweep.xml#Syntax',
'reversesweep.xml#Rec',
'reversesweep.xml#G',
'reversesweep.xml#numvar',
'reversesweep.xml#J',
'reversesweep.xml#K',
'reversesweep.xml#Taylor',
'reversesweep.xml#On Input',
'reversesweep.xml#On Input.Dependent Variables',
'reversesweep.xml#On Input.Other Variables',
'reversesweep.xml#On Output',
'reversesweep.xml#On Output.Independent Variables',
'reversesweep.xml#On Output.Other Variables'
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
