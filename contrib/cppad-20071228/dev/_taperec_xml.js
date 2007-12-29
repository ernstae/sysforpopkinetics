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
'taperec.xml#Syntax',
'taperec.xml#Default Constructors',
'taperec.xml#Copy Constructor',
'taperec.xml#Erase',
'taperec.xml#Put',
'taperec.xml#Put.Op',
'taperec.xml#Put.Ind',
'taperec.xml#Put.Par',
'taperec.xml#Put.VecInd',
'taperec.xml#Get',
'taperec.xml#Get.Op',
'taperec.xml#Get.VecInd',
'taperec.xml#Get.Ind',
'taperec.xml#Get.Par',
'taperec.xml#Num',
'taperec.xml#Num.Op',
'taperec.xml#Num.Ind',
'taperec.xml#Num.Par',
'taperec.xml#Replace',
'taperec.xml#Replace.Ind',
'taperec.xml#Memory'
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
