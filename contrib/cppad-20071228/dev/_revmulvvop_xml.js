var list_across0 = [
'_contents_xml.htm',
'_reference.xml',
'_index.xml',
'_search_xml.htm',
'_external.xml'
];
var list_down2 = [
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
var list_down1 = [
'forabsop.xml',
'revabsop.xml',
'foraddop.xml',
'revaddop.xml',
'foracosop.xml',
'revacosop.xml',
'forasinop.xml',
'revasinop.xml',
'foratanop.xml',
'revatanop.xml',
'fordivvvop.xml',
'revdivvvop.xml',
'forexpop.xml',
'revexpop.xml',
'forlogop.xml',
'revlogop.xml',
'formulvvop.xml',
'revmulvvop.xml',
'forsincos.xml',
'revsincos.xml',
'forsqrtop.xml',
'revsqrtop.xml',
'forsubvvop.xml',
'revsubvvop.xml'
];
var list_current0 = [
'revmulvvop.xml#Syntax',
'revmulvvop.xml#Description',
'revmulvvop.xml#z',
'revmulvvop.xml#x',
'revmulvvop.xml#y',
'revmulvvop.xml#p',
'revmulvvop.xml#pz',
'revmulvvop.xml#On Input',
'revmulvvop.xml#On Input.px',
'revmulvvop.xml#On Input.py',
'revmulvvop.xml#On Output',
'revmulvvop.xml#On Output.px',
'revmulvvop.xml#On Output.py'
];
function choose_across0(item)
{	var index          = item.selectedIndex;
	item.selectedIndex = 0;
	if(index > 0)
		document.location = list_across0[index-1];
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
