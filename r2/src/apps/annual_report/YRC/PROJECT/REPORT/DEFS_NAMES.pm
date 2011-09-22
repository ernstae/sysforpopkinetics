package YRC::PROJECT::REPORT::DEFS;

## MAKE THESE VARIABLES AVAILABLE
require Exporter;
our @ISA = ("Exporter");
our @EXPORT = qw($STARTDATE $ENDDATE $HOST_NAME @LABS %MEMBERS);

## $STARTDATE DECLARATION
## THIS IS THE EARLIEST DATE FROM WHICH WE WILL INCLUDE PROJECTS
$STARTDATE = '2001-01-01';

## $ENDDATE DECLARATION
## THIS IS THE LATEST DATE FROM WHICH WE WILL INCLUDE PROJECTS
$ENDDATE = '2003-06-11';

## THE NAME OF OUR LOCAL HOST IN THE REPORT
## THIS WILL NOT BE PRINTED IN THE NONHOST PORTION OF THE
## REPORT, IF THE RESEARCHER IS FROM THIS HOST
$HOST_NAME = 'University of Washington';

## @LABS DECLARATION
## THIS IS USED TO DETERMINE THE ORDER WHICH PROJECTS ARE   
## PRESENTED IN THE REPORT, AS WELL AS THOSE AFFILIATED WITH
## THE PROJECT FROM THE LOCAL RESOURCE CENTER
## THE GROUPS ARE LISTED THE ORDER WHICH THEY APPEAR IN THIS ARRAY
## ONLY PROJECTS AFFILIATED WITH THESE GROUPS IN THE DB WILL APPEAR IN THE REPORT   
@LABS = ('IntMetab', 'LipidMetab', 'PKPD', 'EnvTox', 'CBNet', 'PMImageD', 'SysMod', 'StatMod', 'SoftDev');

## %MEMBERS DECLARATION
## THIS IS A LIST OF RESEARCHERS ASSOCIATED WITH THE RESOURCE CENTER GROUP LABS
## THE RESEARCHERS ARE LISTED IN ARRAY FOR EACH GROUP, WHICH REPRESENTS THE ORDER
## WHICH THEY WILL APPEAR ON THE REPORT
## THE 6 RESEARCHER FIELDS ARE: NAME, DEGREE, DEPARTMENT, ORGANIZATION, CITY, STATE
## THIS SHOULD REALLY BE MOVED TO XML OR A DATABASE
%MEMBERS = (
   IntMetab=>[
	["Cobelli, Claudio","Ph.D.","Information Engineering","University of Padova","Padova","Italy"],
	["Toffolo, Gianna","Ph.D.","Information Engineering","University of Padova","Padova","Italy"],
	["Caumo, Andrea","Ph.D.","Biostatistics","San Raffaele Hospital","Milano","Italy"],
	["Vicini, Paolo","Ph.D.","Bioengineering","University of Washington","Seattle","WA"], 
   ],
   LipidMetab=>[
        ["Barrett, P. Hugh R.","Ph.D.","Medicine","University of Western Australia","Perth","Western Australia"],
	["Vicini, Paolo","Ph.D.","Bioengineering","University of Washington","Seattle","WA"],
   ],
   PKPD=>[
        ["Bies, Robert R.","Ph.D., Pharm.D.","Pharmacy","University of Pittsburgh","Pittsburgh","PA"],
	["Gastonguay, Marc R.","Ph.D.","Pharmacy","University of Connecticut","Storrs","CT"],
	["Vicini, Paolo","Ph.D.","Bioengineering","University of Washington","Seattle","WA"],

   ],
   EnvTox=>[
        ["Pierce, Crispin H.","Ph.D.","Environmental Health","University of Washington","Seattle","WA"],
	["Vicini, Paolo","Ph.D.","Bioengineering","University of Washington","Seattle","WA"],
   ],
   CBNet=>[
        ["Beard, Daniel","Ph.D.","Bioengineering","University of Washington","Seattle","WA"],
	["Vicini, Paolo","Ph.D.","Bioengineering","University of Washington","Seattle","WA"],
        ["Dash, Ranjan","Ph.D.","Bioengineering","University of Washington","Seattle","WA"],
   ],
   PMImageD=>[
        ["Vicini, Paolo","Ph.D.","Bioengineering","University of Washington","Seattle","WA"],
	["Cobelli, Claudio","Ph.D.","Information Engineering","University of Padova","Padova","Italy"],
	["Bertoldo, Alessandra","Ph.D.","Information Engineering","University of Padova","Padova","Italy"],
   ],
   SysMod=>[
	["Cobelli, Claudio","Ph.D.","Information Engineering","University of Padova","Padova","Italy"],
	["Vicini, Paolo","Ph.D.","Bioengineering","University of Washington","Seattle","WA"], 
	["Foster, David M.","Ph.D.","Bioengineering","University of Washington","Seattle","WA"],
	["Bell, Bradley M."."Ph.D."."Applied Physics Laboratory","University of Washington","Seattle","WA"],
	["Burke, James V.","Ph.D.","Mathematics","University of Washington","Seattle","WA"],
	["Toffolo, Gianna","Ph.D.","Information Engineering","University of Padova","Padova","Italy"],
	["Sparacino, Giovanni","Ph.D.","Information Engineering","University of Padova","Padova","Italy"],
        ["Dash, Ranjan","Ph.D.","Bioengineering","University of Washington","Seattle","WA"],
   ],
   StatMod=>[
	["Burke, James V.","Ph.D.","Mathematics","University of Washington","Seattle","WA"],
	["Cobelli, Claudio","Ph.D.","Information Engineering","University of Padova","Padova","Italy"],
	["Sparacino, Giovanni","Ph.D.","Information Engineering","University of Padova","Padova","Italy"],
	["Vicini, Paolo","Ph.D.","Bioengineering","University of Washington","Seattle","WA"], 
	["Foster, David M.","Ph.D.","Bioengineering","University of Washington","Seattle","WA"],
	["Bell, Bradley M."."Ph.D."."Applied Physics Laboratory","University of Washington","Seattle","WA"],
	["Westhagen, Alan, F.","Ph.D.","Bioengineering","University of Washington","Seattle","WA"],
   ],
   SoftDev=>[
	["Westhagen, Alan, F.","Ph.D.","Bioengineering","University of Washington","Seattle","WA"],
	["Vicini, Paolo","Ph.D.","Bioengineering","University of Washington","Seattle","WA"], 
	["Watrous, Mitchell","Ph.D.","Bioengineering","University of Washington","Seattle","WA"],
	["Honda, Sachiko","Ph.D.","Bioengineering","University of Washington","Seattle","WA"],
	["Foster, David M.","Ph.D.","Bioengineering","University of Washington","Seattle","WA"],
	["Bell, Bradley M."."Ph.D."."Applied Physics Laboratory","University of Washington","Seattle","WA"],
	["Cobelli, Claudio","Ph.D.","Information Engineering","University of Padova","Padova","Italy"],
	["Sparacino, Giovanni","Ph.D.","Information Engineering","University of Padova","Padova","Italy"],
	["Burke, James V.","Ph.D.","Mathematics","University of Washington","Seattle","WA"],
   ]
);

1;

