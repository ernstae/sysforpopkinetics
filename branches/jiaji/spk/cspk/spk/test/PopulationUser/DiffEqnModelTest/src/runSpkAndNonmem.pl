#! /usr/bin/perl

use strict;
use warnings;
use convert;

my $skipNonmemSimulation  = 1;
my $skipNonmemEstimation  = 0;
my $skipSpkEstimation     = 0;
my $simulationCompleted   = 0;
my $isNonmemEstCompleted  = 0;
my $isSpkEstCompleted     = 0;
my $isComparizonCompleted = 0;
my $simControl            = "-";
my $estControl            = "-";
my $simData               = "-";
my $estData               = "-";
my $xmlInput              = "-";
my $simLog                = "-";
my $compResults           = "-";
my $spkExecFullpath       = "-";
my $inputFullpath         = "-";
my $inputFilename         = "-";

#------------------------------------------------------------------
#
# Usage
#
#------------------------------------------------------------------
sub usage
{
  print( "\n\n" );
  print( "Usage: perl runSpkAndNonmem.pl INPUT SPK_EXEC [ /simulate ]\n" );
  print( "\n" );
  print( "INPUT:     A file that specifies parameters to drive SPK and NONMEM.\n" );
  print( "           The file format must be either the XML or the raw.\n" );
  print( "\n" );
  print( "SPK_EXEC:  A path to an executable form of \"genericDriver.cpp\".\n" );
  print( "           For example, release/AnalyticalDiffEqnModelTest.exe.\n" );
  print( "\n" );
  print( "/simulate: The exact literal \"/simulate\" must be typed in when requesting\n" );
  print( "           data simulation using the data file specified in INPUT as the base.\n" );
  print( "           This switch is optional.  If omitted, no simulation is performed.\n" );
  print( "\n\n" );
}

#------------------------------------------------------------------
# PRECONDITION
# * An input file name is given.
#
# Check if the input file given by user is in xml format.
# If not, assume it is in the raw data format and
# conver it to a xml. Prompt user for typing a new (xml) file name
# or overwrite the raw data file.
#------------------------------------------------------------------
if( scalar(@ARGV) < 2 )
{
  die usage();
}

$inputFullpath = $ARGV[0];
-f $inputFullpath or die "\nERROR!!!  Failed to find file \"$inputFullpath\"!!!\n";

$inputFilename = $inputFullpath;

# removing file extensions
$inputFilename =~ s/\.[\w]+//;

# removing the leading . or ..
$inputFilename =~ s/\.+([\/\\]+)/$1/;

# removing the leading / or \
$inputFilename =~ s/[\/\\]([\w\d]+)/$1/;

# removing the xxx/ or xxx\ patterns
$inputFilename =~ s/^(\S*\/)*//;

$spkExecFullpath = $ARGV[1];
-f $spkExecFullpath or die "\nERROR!!!  Failed to find file \"$spkExecFullpath\"!!!\n";

if( scalar( @ARGV ) == 3 && $ARGV[2] eq "/simulate" )
{
  $skipNonmemSimulation = 0;
  
  # In particular, NONMEM doesn't take a long name for the name of simulation output data file.
  # For that reason, we use a fixed filename for a new data set.
  $simData = "simulated.dat";
  
  $simLog     = $inputFilename.".simlog";
  $simControl = $inputFilename.".sim";
}
else
{
  $skipNonmemSimulation = 1;
}
#------------------------------------------------------------------
# PRECONDITION
# * Some sort of an input file is given.
#
# Convert the input file to xml if that is not already in xml.
#
#------------------------------------------------------------------
print( "===========================================================\n" );
print( " Processing the input file...                              \n" );
print( "===========================================================\n" );
open( IFH, $inputFullpath );
my $first_line = <IFH>;
close( IFH );
$xmlInput = $inputFullpath;
if( not $first_line =~ /<?xml/ )
{
  $xmlInput = $inputFilename.".xml";
  print( "\n... \"$inputFullpath\" has to be converted to a valid xml format.  " );
  print( "I'll create a new file $xmlInput for you...\n" );
  -f $xmlInput or unlink( $xmlInput );
  convert::convertRawInputToXml( $inputFullpath, $xmlInput, $inputFullpath, "ADVAN2" );
  -f $xmlInput or die "\n\nERROR!!! Failed to convert $inputFullpath to xml!!!\n";
  print( "\n... Created $xmlInput for you...\n" );
}
#------------------------------------------------------------------
# PRECONDITION
# * An valid xml input file exists.
#
# Parse the input file and generate a tree.
#
#------------------------------------------------------------------
# Parse the XML file that contains the initial parameter values.
use XML::Simple;

my $inputTree = XMLin( $xmlInput, forcearray => 1 )
   or die "\nERROR!!!  Failed parsing file \"$xmlInput\"!!!\n";

use Data::Dumper;
#print Dumper( $inputTree );

#------------------------------------------------------------------
# PRECONDITION
# * A parse tree is given.
#
# Copy node values to proper variables.
#
#   reference to arrays
#         $xxx = $$tree{yyy}->[0]        --- @$xxx
#   reference to references of arrays
#         $xxx = $$tree->{yyy}           --- @{$xxx->[0]}
#
#------------------------------------------------------------------
use trim;

my $title            = $inputTree->{title}->[0];

# a reference to an array
#our $subroutines    = $$inputTree{subroutines}->[0];

my $baseData         = $inputTree->{base_data}->[0];
trim( $baseData );

my $nonmemOutputFile = $inputTree->{nonmem_outfile}->[0];
trim( $nonmemOutputFile );

my $spkOutputFile    = $inputTree->{spk_outfile}->[0];
trim( $spkOutputFile );

my $pkModel          = $inputTree->{pk_model}->[0];
trim( $pkModel );

my $errorModel       = $inputTree->{error_model}->[0];
trim( $errorModel );

my $approximation    = $inputTree->{approximation}->[0];
trim( $approximation );

# theta vector
# these are each a reference to an array
our $pThetaParam     = $$inputTree{theta}->[0]->{nonmem_param}->[0];
our $pThetaLow       = $$inputTree{theta}->[0]->{low}->[0];
our $pThetaIn        = $$inputTree{theta}->[0]->{in }->[0];
our $pThetaUp        = $$inputTree{theta}->[0]->{up }->[0];
our $thetaLen        = scalar( @{$pThetaParam} );

# omega matrix
my $OmegaStruct      = $inputTree->{Omega}->[0]->{structure}->[0];
trim( $OmegaStruct );
# these are each a reference to an array of arrays
my $ppOmegaParam     = $$inputTree{Omega} ->[0]->{nonmem_param};
my $ppOmegaIn        = $$inputTree{Omega} ->[0]->{in};
my $OmegaDim         = scalar( @$ppOmegaParam );

# sigma matrix structure
my $SigmaStruct    = $inputTree->{Sigma}->[0]->{structure}->[0];
trim( $SigmaStruct );
# these are each a reference to an array of arrays
my $ppSigmaParam     = $$inputTree{Sigma} ->[0]->{nonmem_param};
my $ppSigmaIn        = $$inputTree{Sigma} ->[0]->{in};
my $SigmaDim         = scalar( @$ppSigmaParam );

# eta vector
my $pEtaLow          = $inputTree->{eta}->[0]->{low}->[0];
#our $pEtaIn         = $inputTree->{eta}->[0]->{in}->[0];
my $pEtaUp           = $inputTree->{eta}->[0]->{up}->[0];

# ststatistical form
my $covForm          = $inputTree->{statistics}->[0]->{population}->[0]->{covariance}->[0];
trim( $covForm );

# Max iterations
my $popMitr          = $inputTree->{max_iterations}->[0]->{population}->[0];
my $indMitr          = $inputTree->{max_iterations}->[0]->{individual}->[0];
#------------------------------------------------------------------
# PRECONDITION
# * All variables contain valid data
#
# Find out if user has requested to simulate a data set.
# If not, skip this data simulation process.
# If yes, generate a NONMEM control file for simulation and
# simulate data.
#
#------------------------------------------------------------------
use nonmem;
if( not $skipNonmemSimulation )
{
  print( "===========================================================\n" );
  print( " Simulating data with NONMEM ...                           \n" );
  print( "===========================================================\n" );
  if( -f $simControl )
  {
    print( "... Found an old $simControl.  I must delete it...\n" );
    unlink( $simControl );
  }
  if( -f $simData )
  {
    print( "... Found an old $simData.  I must delete it...\n" );
    unlink( $simData );
  }
  nonmem::generateADVAN2basedSimControl(
                              $simControl,
                              $title,
                              $pkModel,
                              $errorModel,
                              $approximation,
                              $baseData,
                              $simData,
                              $thetaLen,
                              $pThetaLow,
                              $pThetaIn,
                              $pThetaUp,
                              $OmegaDim,
                              $OmegaStruct,
                              $ppOmegaIn,
                              $SigmaDim,
                              $SigmaStruct,
                              $ppSigmaIn
                            );
   if( nonmem::simulate( $simControl, $simData, $simLog ) )
   {
      $estData = $simData;
      $simulationCompleted = 1;
      print ( "... Log is saved in $simLog ...\n" );
   }
   else
   {
      print( ">>> Do you want to use $baseData instead? [Y/N]: " );
      my $yn = <STDIN>;
      if( $yn =~ /^[yY]/ )
      {
        $estData = $baseData;
      }
      else
      {
        print( "... then we have to skip NONMEM completely... Now executes $spkExecFullpath...\n" );
        $skipNonmemEstimation = 1;
      }
   }
}
else
{
  $estData = $baseData;
}
#------------------------------------------------------------------
#
# * A data file for estimation exists.
#
# Generate a NONMEM control file for estimation.
#
#------------------------------------------------------------------

if( not $skipNonmemEstimation )
{
  print( "===========================================================\n" );
  print( " NONMEM's estimation ...                                   \n" );
  print( "===========================================================\n" );
  $estControl = $inputFilename.".est";
  if( -f $estControl )
  {
    print( "... Found an old $estControl.  I must delete it...\n" );
    unlink( $estControl );
  }
  nonmem::generateADVAN2basedEstControl(
                              $estControl,
                              $title,
                              $pkModel,
                              $errorModel,
                              $approximation,
                              $estData,
                              $thetaLen,
                              $pThetaLow,
                              $pThetaIn,
                              $pThetaUp,
                              $OmegaDim,
                              $OmegaStruct,
                              $ppOmegaIn,
                              $SigmaDim,
                              $SigmaStruct,
                              $ppSigmaIn,
                              $covForm,
                              $popMitr,
                            );

  print( "... Created a control file for estimation: $estControl...\n" );
  if( nonmem::estimate( $estControl, $nonmemOutputFile ) )
  {
    $isNonmemEstCompleted = 1;
    print ( "... The output is saved in $nonmemOutputFile...\n" );
  }
}
#------------------------------------------------------------------
#
# * A NONMEM control file for estimation exists.
#
# Run NONMEM for estimation.
#
#------------------------------------------------------------------

#----------------------------------------------------------------
# PRECONDITION
# * A SPK driver which produces a valid xml file exists.
#
# Run the SPK driver for estimation.
#
#------------------------------------------------------------------
print( "===========================================================\n" );
print( " SPK estimation ...                                        \n" );
print( "===========================================================\n" );

die "\nERROR!!! The dimension of Sigma must be 1.  Other dimensions are not supported yet!!!\n\n"
    unless $SigmaDim == 1;
-f $estData or die "\nERROR!!!  Cannot open $estData!!!\n";

if( not $skipSpkEstimation )
{
  my @spkCommand = (
                    $spkExecFullpath,
                    "-data",        $estData,
                    "-out",         $spkOutputFile,
                    "-method",      $approximation,
                    "-pk",          $pkModel,
                    "-error",       $errorModel,
                    "-form",        $ppSigmaParam->[0][0],
                    "-sigma",       $ppSigmaIn->[0][0],
                    "-popmitr",     $popMitr,
                    "-indmitr",     $indMitr,
                    "-theta",       $$pThetaLow[0],
                                    $$pThetaIn[0],
                                    $$pThetaUp[0],
                                    $$pThetaLow[1],
                                    $$pThetaIn[1],
                                    $$pThetaUp[1],
                                    $$pThetaLow[2],
                                    $$pThetaIn[2],
                                    $$pThetaUp[2],
                    "-formulation", $covForm,
                    "-eta",         $$pEtaLow[0],
                                    $$pEtaUp[0],
                                    $$pEtaLow[1],
                                    $$pEtaUp[1],
                                    $$pEtaLow[2],
                                    $$pEtaUp[2]
                  );
  push( @spkCommand, "-omega" );
  push( @spkCommand, $OmegaStruct );
  if( $OmegaStruct eq $convert::FULL )
  {
    push( @spkCommand, $ppOmegaIn->[0][0] );
    push( @spkCommand, $ppOmegaIn->[1][0] );
    push( @spkCommand, $ppOmegaIn->[1][1] );
    push( @spkCommand, $ppOmegaIn->[2][0] );
    push( @spkCommand, $ppOmegaIn->[2][1] );
    push( @spkCommand, $ppOmegaIn->[2][2] );
  }
  else
  {
    push( @spkCommand, $ppOmegaIn->[0][0] );
    push( @spkCommand, $ppOmegaIn->[1][0] );
    push( @spkCommand, $ppOmegaIn->[2][0] );
  }

  print( join( " ", @spkCommand ), "\n" );
  system( @spkCommand );
  if( -f $spkOutputFile.".xml" )
  {
    $isSpkEstCompleted = 1;
  }
  else
  {
    die "\nERROR!!!  Spk did not produce an xml output file!!!\n";
  }
  unlink( $spkOutputFile );
  rename( $spkOutputFile.".xml", $spkOutputFile );

  print( "... The output is saved in $spkOutputFile ...\n" );
}

#------------------------------------------------------------------
# PRECONDITION
# * Both a SPK output and a NONMEM output file exist.
#
# Run alphaTestPostprocessor.
#
#------------------------------------------------------------------
print( "===========================================================\n" );
print( " Compare results ...                                       \n" );
print( "===========================================================\n" );
$compResults = $inputFilename.".cmp";
print( "... An old $compResults exists... I must deleted it...\n" );
unlink( $compResults );
-f $spkOutputFile  or die "\n\nERROR!!!  No SPK result file $spkOutputFile found!!!\n";
-f $nonmemOutputFile or die "\n\nERROR!!!  No SPK result file $nonmemOutputFile found!!!\n";
my @compareCommand = ( "./compareSpkToNonmem.pl",
                        $spkOutputFile,
                        $nonmemOutputFile,
                        $compResults );
                      
system( @compareCommand );
if( not -f $compResults )
{
 print( STDERR "\n\nERROR!!!  Comparison failed!  Check out the preceeding error messages!!!\n" );
}
else
{
  $isComparizonCompleted = 1;
  print ( "\n... $compResults ... saved.\n" );
}
#------------------------------------------------------------------
#
# Remind the user of files that were generated during the process.
#
#------------------------------------------------------------------
print( "\n\n\n" );
print( "*****************************************************************************************\n" );
print( "*                                                                                       *\n" );
print( "* Files used/generated during the session ...                                           *\n" );
print( "*                                                                                       *\n" );
print( "*****************************************************************************************\n" );
print( "Input file:                                    ", $inputFullpath, "\n" );
print( "Xml input file:                                $xmlInput\n" );
print( "NONMEM control file for simulating a data set: $simControl\n" );
print( "Simulated data:                                ", $simulationCompleted?   $simData : "-", "\n" );
print( "Simulation log:                                ", $simulationCompleted?   $simLog  : "-", "\n" );
print( "NONMEM control file for estimating parameters: $estControl\n" );
print( "NONMEM output/log file:                        ", $isNonmemEstCompleted?  $nonmemOutputFile : "-", "\n" );
print( "Data file used for estimation:                 $estData\n" );
print( "SPK output file:                               ", $isSpkEstCompleted?     $spkOutputFile : "-", "\n" );
print( "Comparizon results:                            ", $isComparizonCompleted? $compResults   : "-", "\n" );
print( "*****************************************************************************************\n" );
print( "\n\n\n" );
