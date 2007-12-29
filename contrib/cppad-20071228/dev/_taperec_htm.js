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
'taperec.htm#Syntax',
'taperec.htm#Default Constructors',
'taperec.htm#Copy Constructor',
'taperec.htm#Erase',
'taperec.htm#Put',
'taperec.htm#Put.Op',
'taperec.htm#Put.Ind',
'taperec.htm#Put.Par',
'taperec.htm#Put.VecInd',
'taperec.htm#Get',
'taperec.htm#Get.Op',
'taperec.htm#Get.VecInd',
'taperec.htm#Get.Ind',
'taperec.htm#Get.Par',
'taperec.htm#Num',
'taperec.htm#Num.Op',
'taperec.htm#Num.Ind',
'taperec.htm#Num.Par',
'taperec.htm#Replace',
'taperec.htm#Replace.Ind',
'taperec.htm#Memory'
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
