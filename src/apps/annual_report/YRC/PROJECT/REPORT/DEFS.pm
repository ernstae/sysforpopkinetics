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
@LABS = ('IntMetab', 'LipidMetab', 'DynandKin', 'EnvTox', 'CBNet', 'PMImageD');

## %MEMBERS DECLARATION
## THIS IS A LIST OF RESEARCHERS ASSOCIATED WITH THE RESOURCE CENTER GROUP LABS
## THE RESEARCHERS ARE LISTED IN ARRAY FOR EACH GROUP, WHICH REPRESENTS THE ORDER
## WHICH THEY WILL APPEAR ON THE REPORT
## THE 6 RESEARCHER FIELDS ARE: NAME, DEGREE, DEPARTMENT, ORGANIZATION, CITY, STATE
## THIS SHOULD REALLY BE MOVED TO XML OR A DATABASE
%MEMBERS = (
   IntMetab=>[
        ["Yates III, John R.", "Ph.D.", "Cell Biology", "Scripps Research Institute", "La Jolla", "CA"],
        ["McDonald, W. Hayes", "Ph.D.", "Cell Biology", "Scripps Research Institute", "La Jolla", "CA"],
        ["Anderson, Scott D.", "B.S.", "Cell Biology", "Scripps Research Institute", "La Jolla", "CA"], 
   ],
   LipidMetab=>[
        ["Aebersold, Rudolph H.", "Ph.D.", "", "Institute for Systems Biology", "Seattle", "WA"],
        ["Newitt, Richard A.", "Ph.D.", "", "Institute for Systems Biology", "Seattle", "WA"],   
   ],
   DynandKin=>[
        ["Fields, Stanley", "Ph.D", "Genome Sciences", "University of Washington", "Seattle", "WA"],
        ["Hazbun, Tony R.", "Ph.D", "Genome Sciences", "University of Washington", "Seattle", "WA"],
        ["Aranda, Jennifer R.", "B.A.", "Genome Sciences", "University of Washington", "Seattle", "WA"],
   ],
   EnvTox=>[
        ["Davis, Trisha N.", "Ph.D.", "Biochemistry", "University of Washington", "Seattle", "WA"],
        ["Muller, Eric G. D.", "Ph.D.", "Biochemistry", "University of Washington", "Seattle", "WA"],
        ["Sundin, Bryan A.", "B.S.", "Biochemistry", "University of Washington", "Seattle", "WA"],   
        ["Snydsman, Brian E.", "B.S.", "Biochemistry", "University of Washington", "Seattle", "WA"], 
   ],
   CBNet=>[
        ["Baker, David", "Ph.D.", "Biochemistry", "University of Washington", "Seattle", "WA"],
        ["Malmstroem, Lars G.", "M.S.", "Biochemistry", "University of Washington", "Seattle", "WA"],
        ["Schief, William R.", "Ph.D.", "Biochemistry", "University of Washington", "Seattle", "WA"],
   ],
   PMImageD=>[
        ["Riffle, Michael E.", "B.S.", "Biochemistry", "University of Washington", "Seattle", "WA"],
   ]
);

1;
