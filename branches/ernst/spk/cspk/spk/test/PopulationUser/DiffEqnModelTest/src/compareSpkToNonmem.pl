#!/usr/bin/perl -w

#*********************************************************************************
#
# File: compareSpkToNonmem.pl
#
#
# Parses an XML file that contains values calculated using SPK, reads in values
# from a NONMEM output file, and then compares the values to see if they agree.
# 
# Author: Mitch Watrous
#
#
# Arguments
# ---------
#
# $ARGV[0]
#
# Path and name for the XML file that contains the values calculated using SPK.
#
#
# $ARGV[1]
#
# Path and name for the NONMEM output file.
#
#
# $ARGV[2]
#
# Path and name for the file that contains the results from the comparison
# of the SPK and NONMEM values.
#
#*********************************************************************************

#------------------------------------------------------------------------
# Preliminaries.
#------------------------------------------------------------------------

use strict;

use RfpkUtilities;

# Get the paths and names of the files specified by the input arguments.
my $spkValuesFile    = shift;
my $nonmemOutputFile = shift;
my $resultsFile      = shift;

my $i;
my $j;
my $k;
my $col;
my $row;

my @headingsNonmem;
my @headingsSpk;


#------------------------------------------------------------------------
# Get the values.
#------------------------------------------------------------------------

# Get the SPK values.
use SpkOutput;
SpkOutput::parseSpkValues( $spkValuesFile );

# Get the NONMEM values.
use NonmemOutput;
NonmemOutput::readNonmemOutput( $nonmemOutputFile );


#------------------------------------------------------------------------
# Check the dimensions of the various quantities.
#------------------------------------------------------------------------

my $nTheta = scalar( @$SpkOutput::thetaOut );
my $nOmega = scalar( @$SpkOutput::OmegaOut );
my $nSigma = scalar( @$SpkOutput::SigmaOut );
my $nEta   = scalar( @$SpkOutput::etaForAllOut );
my $nInd   = scalar( @{$SpkOutput::etaForAllOut->[ 0 ]} );

# Determine the total number of fixed effects parameters.
my $nFixedEffects = $nTheta;
for ( $j = 0; $j < $nOmega; $j++ )
{
  $nFixedEffects += scalar( @{$SpkOutput::OmegaOut->[ $j ]} );
}
for ( $j = 0; $j < $nSigma; $j++ )
{
  $nFixedEffects += scalar( @{$SpkOutput::SigmaOut->[ $j ]} );
}

# Check the theta related quantities.
scalar( @$SpkOutput::thetaSE )     == $nTheta or die( badSE(  "SPK",    "theta" ) );
scalar( @$NonmemOutput::thetaOut ) == $nTheta or die( badVal( "NONMEM", "theta" ) );
scalar( @$NonmemOutput::thetaSE )  == $nTheta or die( badSE(  "NONMEM", "theta" ) );

# Check the Omega related quantities.
scalar( @$SpkOutput::OmegaSE )     == $nOmega or die( badSE(  "SPK",    "Omega" ) );
scalar( @$NonmemOutput::OmegaOut ) == $nOmega or die( badVal( "NONMEM", "Omega" ) );
scalar( @$NonmemOutput::OmegaSE )  == $nOmega or die( badSE(  "NONMEM", "Omega" ) );
for ( $j = 0; $j < $nOmega; $j++ )
{
  scalar( @{$SpkOutput::OmegaOut->[ $j ]} ) == scalar( @{$NonmemOutput::OmegaOut->[ $j ]} )
    or die( badVal( "SPK and NONMEM", "Omega" ) );
  scalar( @{$SpkOutput::OmegaSE->[ $j ]} )  == scalar( @{$NonmemOutput::OmegaSE->[ $j ]} )
    or die( badSE( "SPK and NONMEM", "Omega" ) );
}

# Check the Sigma related quantities.
scalar( @$SpkOutput::SigmaSE )     == $nSigma or die( badSE(  "SPK",    "Sigma" ) );
scalar( @$NonmemOutput::SigmaOut ) == $nSigma or die( badVal( "NONMEM", "Sigma" ) );
scalar( @$NonmemOutput::SigmaSE )  == $nSigma or die( badSE(  "NONMEM", "Sigma" ) );
for ( $j = 0; $j < $nSigma; $j++ )
{
  scalar( @{$SpkOutput::SigmaOut->[ $j ]} ) == scalar( @{$NonmemOutput::SigmaOut->[ $j ]} )
    or die( badVal( "SPK and NONMEM", "Sigma" ) );
  scalar( @{$SpkOutput::SigmaSE->[ $j ]} )  == scalar( @{$NonmemOutput::SigmaSE->[ $j ]} )
    or die( badSE( "SPK and NONMEM", "Sigma" ) );
}


#------------------------------------------------------------------------
# Check the structures of the covariance matrices.
#------------------------------------------------------------------------

#[Remove]================================
#
# TO DO:  FINISH THIS
#
#[Remove]================================

my $isOmegaDiag = $SpkOutput::OmegaStructure eq 'diagonal';
my $isSigmaDiag = $SpkOutput::SigmaStructure eq 'diagonal';


#[Remove]================================
#
# TO DO:
#
#   [] HANDLE BAD NUMBERS LIKE INF AND NAN
#
#   [] GIVE EXECUTIVE SUMMARY OF TEST RESULTS
#
#   [] INCLUDE IN THE REPORT:
#
#           $parEstCalc
#           $parEstOk
#           $SECalc
#           $SEOk
#           $messages
#           $nsig
#
#      AND ANY OTHER FIELDS OF INTEREST 
#
#   [] MAKE TEST AUTOMATIC? (OR AT LEAST INCLUDE THE AUTOMATIC PASS/FAIL VALUE IN THE SUMMARY?)
#
#   [] MAKE THE REL. TOL FOR COV, CORR, ETAS, AND PREDS BE 3 SIG DIGITS?
#
#[Remove]================================


#------------------------------------------------------------------------
# Prepare the test results file.
#------------------------------------------------------------------------

# Open the results file for output and then check that it is writable.
my $resultsFileHandle;
open( $resultsFileHandle, '>', $resultsFile ) or die "Can't open $resultsFile.\n";
-w $resultsFile or die "$resultsFile is not writable.\n";

my $solidAsterisks      = "*********************************************************************************************\n";
my $solidLine           = "--------------------------------------------------------------------------------------------\n";
my $crossedLine         = "----------------+----------------------+-------------+----------+-------------+-------------\n";
my $emptyLine           = "                |                      |             |          |             |             \n";
my $endOfLine           = "\n";
my $sectionTitleFormat  = "     %-40s \n";

my $sectionTitle = "Population Parameters (Fixed Effects) Tests" ;

printf( $resultsFileHandle $solidAsterisks );
printf( $resultsFileHandle $endOfLine );
printf( $resultsFileHandle $sectionTitleFormat, $sectionTitle );
printf( $resultsFileHandle $endOfLine );
printf( $resultsFileHandle $solidAsterisks );
printf( $resultsFileHandle $endOfLine );
printf( $resultsFileHandle $solidLine );
printf( $resultsFileHandle "   Quantity     |    Final Value       |                       Tests                         \n" );
printf( $resultsFileHandle "                |                      |                                                     \n" );
printf( $resultsFileHandle "                |                      |---------------------------------------------------- \n" );
printf( $resultsFileHandle "                |                      |  Relative   |          | Meet Specs  |  Standard    \n" );
printf( $resultsFileHandle "                |                      |  Diff Test  |          |    Test     | Errors Test  \n" );
printf( $resultsFileHandle $crossedLine );
printf( $resultsFileHandle "NONMEM          |  NONMEM Value (SE)   |             |          |  Max        |  Max         \n" );
printf( $resultsFileHandle "SPK             |  SPK    Value (SE)   |Rel Diff Pass| Abs Diff |Abs Diff Pass|Abs Diff Pass \n" );
printf( $resultsFileHandle $crossedLine );
printf( $resultsFileHandle $emptyLine );


#------------------------------------------------------------------------
# Prepare to print some of the values to STDOUT.
#------------------------------------------------------------------------

my $stdoutHandle;
open( $stdoutHandle, ">&STDOUT" ) or die "Can't get a file handle for STDOUT.";

my $oneLineRelDiffSolidLine   = "------------------------------------------------------------------------------\n";
my $oneLineRelDiffCrossedLine = "--------------------------------------+-----------------------+---------------\n";
my $oneLineRelDiffEmptyLine   = "                  |                   |           |           | \n";

printf( $stdoutHandle $oneLineRelDiffSolidLine );
printf( $stdoutHandle "              Quantity                |      Final Value      | Rel Diff Test \n" );
printf( $stdoutHandle "                                      |                       |               \n" );
printf( $stdoutHandle "NONMEM              SPK               |   NONMEM       SPK    | Rel Diff Pass \n" );
printf( $stdoutHandle $oneLineRelDiffCrossedLine );


#------------------------------------------------------------------------
# Print the theta values to the file and STDOUT.
#------------------------------------------------------------------------

for( $j = 0; $j < $nTheta; $j++ )
{
  $row = $j + 1;

  push @headingsNonmem, 'theta(' . ( $row ) . ')';
  push @headingsSpk,    'alpha(' . ( $row ) . ')';

  # Print to the file.
  doAllTests(
    $resultsFileHandle,
    $headingsNonmem[ $#headingsNonmem ],
    $headingsSpk[ $#headingsSpk ],
    $NonmemOutput::thetaOut->[ $j ],
    $NonmemOutput::thetaSE->[ $j ],
    $NonmemOutput::SEOk,
    $NonmemOutput::nsig,
    $SpkOutput::thetaNonPar->[ $j ],
    $SpkOutput::thetaLow->[ $j ],
    $SpkOutput::thetaUp->[ $j ],
    $SpkOutput::thetaOut->[ $j ],
    $SpkOutput::thetaSE->[ $j ],
    $SpkOutput::SEOk,
    $SpkOutput::popEpsilon, 
    $SpkOutput::thetaRelTol );

  # Print to STDOUT.
  oneLineRelDiffTest(
    $stdoutHandle,
    $headingsNonmem[ $#headingsNonmem ],
    $headingsSpk[ $#headingsSpk ],
    $NonmemOutput::thetaOut->[ $j ],
    $SpkOutput::thetaOut->[ $j ],
    $SpkOutput::thetaRelTol );
}
printf( $resultsFileHandle $crossedLine );
printf( $resultsFileHandle $emptyLine );

printf( $stdoutHandle $oneLineRelDiffEmptyLine );


#------------------------------------------------------------------------
# Print the Omega values to the file and STDOUT.
#------------------------------------------------------------------------

for ( $j = 0; $j < $nOmega; $j++ )
{
  for ( $k = 0; $k < scalar( @{$SpkOutput::OmegaOut->[ $j ]} ); $k++ )
  {
    if ( $isOmegaDiag )
    {
      $row = $j + 1;
      $col = $row;
    }
    else
    {
      $row = $j + 1;
      $col = $k + 1;
    }

    push @headingsNonmem, 'Omega(' . ( $row ) . ', ' . ( $col ) . ')';
    push @headingsSpk,    'D(' . ( $row ) . ', ' . ( $col ) . ')';

    # Print to the file.
    doAllTests(
      $resultsFileHandle,
      $headingsNonmem[ $#headingsNonmem ],
      $headingsSpk[ $#headingsSpk ],
      $NonmemOutput::OmegaOut->[ $j ][ $k ],
      $NonmemOutput::OmegaSE->[ $j ][ $k ],
      $NonmemOutput::SEOk,
      $NonmemOutput::nsig,
      $SpkOutput::OmegaNonPar->[ $j ][ $k ],
      $SpkOutput::OmegaLow->[ $j ][ $k ],
      $SpkOutput::OmegaUp->[ $j ][ $k ],
      $SpkOutput::OmegaOut->[ $j ][ $k ],
      $SpkOutput::OmegaSE->[ $j ][ $k ],
      $SpkOutput::SEOk,
      $SpkOutput::popEpsilon, 
      $SpkOutput::OmegaRelTol );

    # Print to STDOUT.
    oneLineRelDiffTest(
      $stdoutHandle,
      $headingsNonmem[ $#headingsNonmem ],
      $headingsSpk[ $#headingsSpk ],
      $NonmemOutput::OmegaOut->[ $j ][ $k ],
      $SpkOutput::OmegaOut->[ $j ][ $k ],
      $SpkOutput::OmegaRelTol );
  }
}
printf( $resultsFileHandle $crossedLine );
printf( $resultsFileHandle $emptyLine );

printf( $stdoutHandle $oneLineRelDiffEmptyLine );


#------------------------------------------------------------------------
# Print the Sigma values to the file and STDOUT.
#------------------------------------------------------------------------

for ( $j = 0; $j < $nSigma; $j++ )
{
  for ( $k = 0; $k < scalar( @{$SpkOutput::SigmaOut->[ $j ]} ); $k++ )
  {
    if ( $isSigmaDiag )
    {
      $row = $j + 1;
      $col = $row;
    }
    else
    {
      $row = $j + 1;
      $col = $k + 1;
    }

    push @headingsNonmem, 'Sigma(' . ( $row ) . ', ' . ( $col ) . ')';
    push @headingsSpk,    'Sigma Equiv';

    # Print to the file.
    doAllTests(
      $resultsFileHandle,
      $headingsNonmem[ $#headingsNonmem ],
      $headingsSpk[ $#headingsSpk ],
      $NonmemOutput::SigmaOut->[ $j ][ $k ],
      $NonmemOutput::SigmaSE->[ $j ][ $k ],
      $NonmemOutput::SEOk,
      $NonmemOutput::nsig,
      $SpkOutput::SigmaNonPar->[ $j ][ $k ],
      $SpkOutput::SigmaLow->[ $j ][ $k ],
      $SpkOutput::SigmaUp->[ $j ][ $k ],
      $SpkOutput::SigmaOut->[ $j ][ $k ],
      $SpkOutput::SigmaSE->[ $j ][ $k ],
      $SpkOutput::SEOk,
      $SpkOutput::popEpsilon, 
      $SpkOutput::SigmaRelTol );

    # Print to STDOUT.
    oneLineRelDiffTest(
      $stdoutHandle,
      $headingsNonmem[ $#headingsNonmem ],
      $headingsSpk[ $#headingsSpk ],
      $NonmemOutput::SigmaOut->[ $j ][ $k ],
      $SpkOutput::SigmaOut->[ $j ][ $k ],
      $SpkOutput::SigmaRelTol );
  }
}
printf( $resultsFileHandle $crossedLine );
printf( $resultsFileHandle $emptyLine );

printf( $stdoutHandle $oneLineRelDiffEmptyLine );


#------------------------------------------------------------------------
# Print the objective function values and information to the file and STDOUT.
#------------------------------------------------------------------------

# Print to the file.
relDiffOnlyTest(
  $resultsFileHandle,
  'NONMEM Objective',
  'SPK Equiv Obj',
  $NonmemOutput::objVal,
  $SpkOutput::objVal,
  $SpkOutput::objRelTol );

# Print to STDOUT.
oneLineRelDiffTest(
  $stdoutHandle,
  'NONMEM Objective',
  'SPK Equiv Obj',
  $NonmemOutput::objVal,
  $SpkOutput::objVal,
  $SpkOutput::objRelTol );

printf( $stdoutHandle $oneLineRelDiffSolidLine );

printPairOfValues(
  $resultsFileHandle,
  'NONMEM Method',
  'SPK Obj Approx',
  $NonmemOutput::objApproxMethod,
  $SpkOutput::objApproxMethod );

printf( $resultsFileHandle $solidLine );
printf( $resultsFileHandle $endOfLine );
printf( $resultsFileHandle $endOfLine );


#------------------------------------------------------------------------
# Prepare the parameter estimate covariance and correlation matrices headings.
#------------------------------------------------------------------------

my @covCorrHeadingsNonmem;
my @covCorrHeadingsSpk;
my $nElem;

# Because the NONMEM parameter estimate covariance and correlation matrices
# contain the elements of the upper triangles of Omega and Sigma (rather than
# the lower triangles that are used for the rest of the NONMEM output), i.e.,
# the headings for the NONMEM report have the following form:
#
#     TH 1        TH 2        TH 3        OM11        OM12        OM13        OM22        OM23        OM33      SG11
#
# the headings for their values in this report must be redefined.
for( $j = 0; $j < $nTheta; $j++ )
{
  $row = $j + 1;

  push @covCorrHeadingsNonmem, 'theta(' . ( $row ) . ')';
  push @covCorrHeadingsSpk,    'alpha(' . ( $row ) . ')';
}
for ( $j = 0; $j < $nOmega; $j++ )
{
  if ( $isOmegaDiag )
  {
    $nElem = 1;
  }
  else
  {
    $nElem = $nOmega - $j;
  }

  for ( $k = $j; $k < $j + $nElem; $k++ )
  {
    $row = $j + 1;
    $col = $k + 1;

    push @covCorrHeadingsNonmem, 'Omega(' . ( $row ) . ', ' . ( $col ) . ')';
    push @covCorrHeadingsSpk,    'D(' . ( $row ) . ', ' . ( $col ) . ')';
  }
}
for ( $j = 0; $j < $nSigma; $j++ )
{
  if ( $isSigmaDiag )
  {
    $nElem = 1;
  }
  else
  {
    $nElem = $nSigma - $j;
  }

  for ( $k = $j; $k < $j + $nElem; $k++ )
  {
    $row = $j + 1;
    $col = $k + 1;

    push @covCorrHeadingsNonmem, 'Sigma(' . ( $row ) . ', ' . ( $col ) . ')';
    push @covCorrHeadingsSpk,    'Sigma Equiv';
  }
}


#------------------------------------------------------------------------
# Print the parameter estimate covariance and correlation matrices to the file.
#------------------------------------------------------------------------

my $headingFormat         = "%-11s ";
my $rightSetHeadingFormat = "%11s ";
my $threeSigDigitFormat   = "  %9.2e ";
my $relDiffFormat         = "   %8.2e ";
my $noValFormat           = "   %8s ";

my $relDiffStr;
my $valStr;

for ( my $isCov = 1; $isCov >= 0; $isCov-- )
{
  if ( $isCov )
  {
    $sectionTitle = "Covariance of the Parameter Estimates";
  }
  else
  {
    $sectionTitle = "Correlation of the Parameter Estimates";
  }

  printf( $resultsFileHandle $solidAsterisks );
  printf( $resultsFileHandle $endOfLine );
  printf( $resultsFileHandle $sectionTitleFormat, $sectionTitle );
  printf( $resultsFileHandle $endOfLine );
  printf( $resultsFileHandle $solidAsterisks );
  printf( $resultsFileHandle $endOfLine );

  printf( $resultsFileHandle $headingFormat, '' );
  for ( $j = 0; $j < $nFixedEffects; $j++ )
  {
    printf( $resultsFileHandle $rightSetHeadingFormat, $covCorrHeadingsNonmem[ $j ] );
  }
  printf( $resultsFileHandle $endOfLine );

  printf( $resultsFileHandle $headingFormat, '' );
  for ( $j = 0; $j < $nFixedEffects; $j++ )
  {
    printf( $resultsFileHandle $rightSetHeadingFormat, $covCorrHeadingsSpk[ $j ] );
  }
  printf( $resultsFileHandle $endOfLine );
  printf( $resultsFileHandle $endOfLine );

  for ( $j = 0; $j < $nFixedEffects; $j++ )
  {
    printf( $resultsFileHandle $headingFormat, $covCorrHeadingsNonmem[ $j ] );
    for ( $k = 0; $k <= $j; $k++ )
    {
      $valStr = formatPotMissVal(
        $isCov ? $NonmemOutput::covOfEstOut->[ $j ][ $k ] : $NonmemOutput::corrOfEstOut->[ $j ][ $k ],
        $threeSigDigitFormat,
        $noValFormat );

      printf( $resultsFileHandle $valStr );
    }
    printf( $resultsFileHandle $endOfLine );

    printf( $resultsFileHandle $headingFormat, $covCorrHeadingsSpk[ $j ] );
    for ( $k = 0; $k <= $j; $k++ )
    {
      $valStr = formatPotMissVal(
        $isCov ? $SpkOutput::covOfEstOut->[ $j ][ $k ] : $SpkOutput::corrOfEstOut->[ $j ][ $k ],
        $threeSigDigitFormat,
        $noValFormat );

      printf( $resultsFileHandle $valStr );
    }
    printf( $resultsFileHandle $endOfLine );

    printf( $resultsFileHandle $headingFormat, 'Rel Diff' );
    for ( $k = 0; $k <= $j; $k++ )
    {
      $relDiffStr =  calcRelDiff(
        $isCov ? $NonmemOutput::covOfEstOut->[ $j ][ $k ] : $NonmemOutput::corrOfEstOut->[ $j ][ $k ],
        $isCov ? $SpkOutput::covOfEstOut   ->[ $j ][ $k ] : $SpkOutput::corrOfEstOut   ->[ $j ][ $k ],
        $relDiffFormat,
        $noValFormat );

      printf( $resultsFileHandle $relDiffStr );
    }
    printf( $resultsFileHandle $endOfLine );
    printf( $resultsFileHandle $endOfLine );
  }
  printf( $resultsFileHandle $endOfLine );
}


#------------------------------------------------------------------------
# Print the individuals' objective function minimizers to the file.
#------------------------------------------------------------------------

my $indFormat = "%7i     ";
$sectionTitle = "Individuals' Objective Function Minimizers";

printf( $resultsFileHandle $solidAsterisks );
printf( $resultsFileHandle $endOfLine );
printf( $resultsFileHandle $sectionTitleFormat, $sectionTitle );
printf( $resultsFileHandle $endOfLine );
printf( $resultsFileHandle $solidAsterisks );
printf( $resultsFileHandle $endOfLine );

printf( $resultsFileHandle $headingFormat, 'Individual' );
printf( $resultsFileHandle $headingFormat, 'Source' );
for ( $j = 0; $j < $nEta; $j++ )
{
  $row = $j + 1;
  printf( $resultsFileHandle $rightSetHeadingFormat, 'eta(' . ( $row ) . ')' );
}
printf( $resultsFileHandle $endOfLine );

printf( $resultsFileHandle $headingFormat, '' );
printf( $resultsFileHandle $headingFormat, '' );
for ( $j = 0; $j < $nEta; $j++ )
{
  $row = $j + 1;
  printf( $resultsFileHandle $rightSetHeadingFormat, 'b(' . ( $row ) . ')' );
}
printf( $resultsFileHandle $endOfLine );
printf( $resultsFileHandle $endOfLine );

my $ind;

for ( $i = 0; $i < $nInd; $i++ )
{
  $ind = $i + 1;
  printf( $resultsFileHandle $indFormat, $ind );
  printf( $resultsFileHandle $headingFormat, 'NONMEM' );
  for ( $j = 0; $j < $nEta; $j++ )
  {
    $valStr = formatPotMissVal(
      $NonmemOutput::etaForAllOut->[ $j ][ $i ],
      $threeSigDigitFormat,
      $noValFormat );

    printf( $resultsFileHandle $valStr );
  }
  printf( $resultsFileHandle $endOfLine );

  printf( $resultsFileHandle $headingFormat, '' );
  printf( $resultsFileHandle $headingFormat, 'SPK' );
  for ( $j = 0; $j < $nEta; $j++ )
  {
    $valStr = formatPotMissVal(
      $SpkOutput::etaForAllOut->[ $j ][ $i ],
      $threeSigDigitFormat,
      $noValFormat );

    printf( $resultsFileHandle $valStr );
  }
  printf( $resultsFileHandle $endOfLine );

  printf( $resultsFileHandle $headingFormat, '' );
  printf( $resultsFileHandle $headingFormat, 'Rel Diff' );
  for ( $j = 0; $j < $nEta; $j++ )
  {
    $relDiffStr =  calcRelDiff(
      $NonmemOutput::etaForAllOut->[ $j ][ $i ],
      $SpkOutput::etaForAllOut->[ $j ][ $i ],
      $relDiffFormat,
      $noValFormat );

    printf( $resultsFileHandle $relDiffStr );
  }
  printf( $resultsFileHandle $endOfLine );
  printf( $resultsFileHandle $endOfLine );
}
printf( $resultsFileHandle $endOfLine );


#------------------------------------------------------------------------
# Print the individuals' data mean model values to the file.
#------------------------------------------------------------------------

$sectionTitle       = "Individuals' Data Mean Model Conditional Values";
my $sectionSubTitle = "(NONMEM's IPRED vs. SPK's f_i(alphaOut, bOut_i))";

printf( $resultsFileHandle $solidAsterisks );
printf( $resultsFileHandle $endOfLine );
printf( $resultsFileHandle $sectionTitleFormat, $sectionTitle );
printf( $resultsFileHandle $endOfLine );
printf( $resultsFileHandle $sectionTitleFormat, $sectionSubTitle );
printf( $resultsFileHandle $endOfLine );
printf( $resultsFileHandle $solidAsterisks );
printf( $resultsFileHandle $endOfLine );

printf( $resultsFileHandle $headingFormat, 'Individual' );
printf( $resultsFileHandle $rightSetHeadingFormat, 'NONMEM' );
printf( $resultsFileHandle $rightSetHeadingFormat, 'SPK' );
printf( $resultsFileHandle $rightSetHeadingFormat, 'Rel Diff' );
printf( $resultsFileHandle $endOfLine );
printf( $resultsFileHandle $headingFormat, '' );
printf( $resultsFileHandle $rightSetHeadingFormat, '(IPRED)' );
printf( $resultsFileHandle $rightSetHeadingFormat, '(f_i)' );
printf( $resultsFileHandle $headingFormat, '' );
printf( $resultsFileHandle $endOfLine );
printf( $resultsFileHandle $endOfLine );

my $nY_i;

for ( $i = 0; $i < $nInd; $i++ )
{
  # Determine the number of data values for this individual.
  $nY_i = scalar( @{$SpkOutput::predForAllOut->[ $i ]} );

  $ind = $i + 1;
  for ( $j = 0; $j < $nY_i; $j++ )
  {
    printf( $resultsFileHandle $indFormat, $ind );

    $valStr = formatPotMissVal(
      $NonmemOutput::predForAllOut->[ $i ][ $j ],
      $threeSigDigitFormat,
      $noValFormat );
    printf( $resultsFileHandle $valStr );

    $valStr = formatPotMissVal(
      $SpkOutput::predForAllOut->[ $i ][ $j ],
      $threeSigDigitFormat,
      $noValFormat );
    printf( $resultsFileHandle $valStr );

    $relDiffStr =  calcRelDiff(
      $NonmemOutput::predForAllOut->[ $i ][ $j ],
      $SpkOutput::predForAllOut->[ $i ][ $j ],
      $relDiffFormat,
      $noValFormat );
    printf( $resultsFileHandle $relDiffStr );

    printf( $resultsFileHandle $endOfLine );
  }
  printf( $resultsFileHandle $endOfLine );
}
printf( $resultsFileHandle $endOfLine );


#*****************************************************************
#
# Subroutine: badVal
#
#
# Returns a message regarding the dimensions of a value.
#
#*****************************************************************

sub badVal
{
  return "The dimensions of the $_[0] $_[1] value are not correct.\n";
}


#*****************************************************************
#
# Subroutine: badSE
#
#
# Returns a message regarding the dimensions of some standard errors.
#
#*****************************************************************

sub badSE
{
  return "The dimensions of the $_[0] $_[1] standard errors are not correct.\n";
}


#*****************************************************************
#
# Subroutine: doAllTests
#
#
# Performs all of the tests on a pair of values.
#
#
# Arguments
# ---------
# 
# $_[0]
# 
# Output file for printing results.
# 
# 
# $_[1]
# 
# NONMEM name for the value.
# 
# 
# $_[2]
# 
# SPK name for the value.
# 
# 
# $_[3]
# 
# NONMEM final estimate for the parameter x.
# 
# 
# $_[4]
# 
# NONMEM standard error for the final estimate for the parameter x.
#
#
# $_[5]
# 
# True if the calculation of the NONMEM standard errors succeeded.  
# Otherwise, false.
#
#
# $_[6]
# 
# NSIG, the desired number of significant in the final NONMEM 
# parameter estimate for x.
#
#
# $_[7]
# 
# True if the SPK test driver parameterizes this parameter the same way as
# its NONMEM counterpart.  Otherwise, false.
#
#
# $_[8]
# 
# SPK lower limit for the parameter x.
#
#
# $_[9]
# 
# SPK upper limit for the parameter x.
#
#
# $_[10]
# 
# SPK final estimate for the parameter x.
#
#
# $_[11]
# 
# SPK standard error for the final estimate for the parameter x.
#
#
# $_[12]
# 
# True if the calculation of the SPK standard errors succeeded.  
# Otherwise, false.
#
#
# $_[13]
# 
# The SPK parameter that is used to determine if an acceptable 
# final estimate has been found for x.
#
#
# $_[14]
# 
# Tolerance for the Relative Difference test.
#
#
# Return Values
# -------------
# 
# None.
# 
#*****************************************************************

sub doAllTests
{
  # Get the arguments.
  my $fileHandle     = shift;
  my $nameNonmem     = shift;
  my $nameSpk        = shift;
  my $xOutNonmem     = shift;
  my $xSENonmem      = shift;
  my $SEOkNonmem     = shift;
  my $nsig           = shift;
  my $xNonParSpk     = shift;
  my $xLowSpk        = shift;
  my $xUpSpk         = shift;
  my $xOutSpk        = shift;
  my $xSESpk         = shift;
  my $SEOkSpk        = shift;
  my $popEpsilon     = shift; 
  my $xRelTol        = shift;

  my $emptyLine           = "                |                      |             |          |             |             \n";
  my $emptyTests          = "|             |          |             |             \n";
  my $emptySpecAndSETests = "|             |             \n";
  my $endOfLine           = "\n";

  my $quantity            = "%-16s";
  my $valAndSEFormat      = "| %9.2e (%8.2e) ";
  my $noValButSEFormat    = "| %9s (%8.2e) ";
  my $valButNoSEFormat    = "| %9.2e (%8s) ";
  my $noValAndNoSEFormat  = "| %9s (%8s) ";
  my $relDiffFormat       = "| %8.2e  %1s ";
  my $absDiffFormat       = "| %8.2e ";
  my $meetSpecFormat      = "| %8.2e  %1s ";
  my $withinSEFormat      = "| %8.2e  %1s ";
  my $skipTestFormat      = "| %11s ";
  my $skipAbsDiffFormat   = "| %8s ";

  my $valAndSENonmemStr;
  my $valAndSESpkStr;
  my $relDiffStr;
  my $absDiffStr;
  my $meetSpecStr;
  my $withinSEStr;

  $valAndSENonmemStr = formatValAndSE(
    $xOutNonmem,
    $xSENonmem,
    $SEOkNonmem,
    $valAndSEFormat,
    $noValButSEFormat,
    $valButNoSEFormat,
    $noValAndNoSEFormat );

  $valAndSESpkStr = formatValAndSE(
    $xOutSpk,
    $xSESpk,
    $SEOkSpk,
    $valAndSEFormat,
    $noValButSEFormat,
    $valButNoSEFormat,
    $noValAndNoSEFormat );

  $relDiffStr = relDiffTest(
    $xOutNonmem,
    $xOutSpk,
    $xRelTol,
    $relDiffFormat,
    $skipTestFormat );

  $absDiffStr = calcAbsDiff(
    $xOutNonmem,
    $xOutSpk,
    $absDiffFormat,
    $skipAbsDiffFormat );

  $meetSpecStr = meetSpecTest(
    $xOutNonmem,
    $nsig,
    $xOutSpk,
    $xLowSpk,
    $xUpSpk,
    $popEpsilon, 
    $xNonParSpk,
    $meetSpecFormat,
    $skipTestFormat );

  $withinSEStr = withinSETest(
    $xOutNonmem,
    $xSENonmem,
    $SEOkNonmem,
    $xOutSpk,
    $xSESpk,
    $SEOkSpk,
    $withinSEFormat,
    $skipTestFormat );

  printf( $fileHandle $quantity,      $nameNonmem );
  printf( $fileHandle $valAndSENonmemStr );
  printf( $fileHandle $relDiffStr );
  printf( $fileHandle $absDiffStr );
  printf( $fileHandle $meetSpecStr );
  printf( $fileHandle $withinSEStr );
  printf( $fileHandle $endOfLine );
  printf( $fileHandle $quantity,      $nameSpk );
  printf( $fileHandle $valAndSESpkStr );
  printf( $fileHandle $emptyTests );
  printf( $fileHandle $emptyLine );
}


#*****************************************************************
#
# Subroutine: relDiffOnlyTest
#
#
# Performs the relative tolerance test only on a pair of values.
#
#
# Arguments
# ---------
# 
# $_[0]
# 
# Output file for printing results.
# 
# 
# $_[1]
# 
# NONMEM name for the value.
# 
# 
# $_[2]
# 
# SPK name for the value.
# 
# 
# $_[3]
# 
# NONMEM final estimate for the parameter x.
# 
# 
# $_[4]
# 
# SPK final estimate for the parameter x.
#
#
# $_[5]
# 
# Tolerance for the test.
#
#
# Return Values
# -------------
# 
# None.
# 
#*****************************************************************

sub relDiffOnlyTest
{
  # Get the arguments.
  my $fileHandle     = shift;
  my $nameNonmem     = shift;
  my $nameSpk        = shift;
  my $xOutNonmem     = shift;
  my $xOutSpk        = shift;
  my $xRelTol        = shift;

  my $emptyLine      = "                |                      |             |          |             |             \n";
  my $emptyTests     = "|             |          |             |             \n";
  my $someEmptyTests = "|          |             |             \n";

  my $quantity       = "%-16s";
  my $valFormat      = "| %9.2e            ";
  my $noValFormat    = "| %9s            ";
  my $relDiffFormat  = "| %8.2e  %1s ";
  my $skipTestFormat = "| %11s ";

  my $valNonmemStr;
  my $valSpkStr;
  my $relDiffStr;

  $valNonmemStr = formatPotMissVal( 
    $xOutNonmem ,
    $valFormat,
    $noValFormat );

  $valSpkStr = formatPotMissVal( 
    $xOutSpk ,
    $valFormat,
    $noValFormat );

  $relDiffStr = relDiffTest(
    $xOutNonmem,
    $xOutSpk,
    $xRelTol,
    $relDiffFormat,
    $skipTestFormat );

  printf( $fileHandle $quantity,      $nameNonmem );
  printf( $fileHandle $valNonmemStr );
  printf( $fileHandle $relDiffStr );
  printf( $fileHandle $someEmptyTests );
  printf( $fileHandle $quantity,      $nameSpk );
  printf( $fileHandle $valSpkStr );
  printf( $fileHandle $emptyTests );
  printf( $fileHandle $emptyLine );
}


#*****************************************************************
#
# Subroutine: oneLineRelDiffTest
#
#
# Performs a one line version of the relative tolerance test 
# on a pair of values.
#
#
# Arguments
# ---------
# 
# $_[0]
# 
# Output file for printing results.
# 
# 
# $_[1]
# 
# NONMEM name for the value.
# 
# 
# $_[2]
# 
# SPK name for the value.
# 
# 
# $_[3]
# 
# NONMEM final estimate for the parameter x.
# 
# 
# $_[4]
# 
# SPK final estimate for the parameter x.
#
#
# $_[5]
# 
# Tolerance for the test.
#
#
# Return Values
# -------------
# 
# None.
# 
#*****************************************************************

sub oneLineRelDiffTest
{
  # Get the arguments.
  my $fileHandle     = shift;
  my $nameNonmem     = shift;
  my $nameSpk        = shift;
  my $xOutNonmem     = shift;
  my $xOutSpk        = shift;
  my $xRelTol        = shift;

  my $quantity       = "%-16s  ";
  my $valFormat      = "| %9.2e ";
  my $noValFormat    = "| %9s ";
  my $relDiffFormat  = "| %8.2e  %1s";
  my $newLine        = "\n";
  my $vertLine       = "| ";
  my $skipTestFormat = "|%-11s";

  my $valNonmemStr;
  my $valSpkStr;
  my $relDiffStr;

  $valNonmemStr = formatPotMissVal( 
    $xOutNonmem ,
    $valFormat,
    $noValFormat );

  $valSpkStr = formatPotMissVal( 
    $xOutSpk ,
    $valFormat,
    $noValFormat );

  $relDiffStr = relDiffTest(
    $xOutNonmem,
    $xOutSpk,
    $xRelTol,
    $relDiffFormat,
    $skipTestFormat );

  printf( $fileHandle $quantity,      $nameNonmem );
  printf( $fileHandle $vertLine );
  printf( $fileHandle $quantity,      $nameSpk );
  printf( $fileHandle $valNonmemStr );
  printf( $fileHandle $valSpkStr );
  printf( $fileHandle $relDiffStr );
  printf( $fileHandle $newLine );
}


#*****************************************************************
#
# Subroutine: printPairOfValues
#
#
# Prints a pair of values.
#
#
# Arguments
# ---------
# 
# $_[0]
# 
# Output file for printing results.
# 
# 
# $_[1]
# 
# NONMEM name for the value.
# 
# 
# $_[2]
# 
# SPK name for the value.
# 
# 
# $_[3]
# 
# NONMEM value.
# 
# 
# $_[4]
# 
# SPK value.
# 
# 
# Return Values
# -------------
# 
# None.
# 
#*****************************************************************

sub printPairOfValues
{
  # Get the arguments.
  my $fileHandle  = shift;
  my $nameNonmem  = shift;
  my $nameSpk     = shift;
  my $xNonmem     = shift;
  my $xSpk        = shift;

  my $emptyLine      = "                |                      |             |          |             |             \n";
  my $emptyTests     = "|             |          |             |             \n";

  my $quantity       = "%-16s";
  my $valFormat      = "| %-20s ";

  my $valNonmemStr;
  my $valSpkStr;

  $valNonmemStr = sprintf( $valFormat, $xNonmem );
  $valSpkStr    = sprintf( $valFormat, $xSpk );

  printf( $fileHandle $quantity,      $nameNonmem );
  printf( $fileHandle $valNonmemStr );
  printf( $fileHandle $emptyTests );
  printf( $fileHandle $quantity,      $nameSpk );
  printf( $fileHandle $valSpkStr );
  printf( $fileHandle $emptyTests );
  printf( $fileHandle $emptyLine );
}


#*****************************************************************
#
# Subroutine: formatValAndSE
#
#
# Formats a parameter value and its standard error.
#
#
# Arguments
# ---------
# 
# $_[0]
# 
# Value for the parameter x.
# 
# 
# $_[1]
# 
# Standard error for the value for the parameter x.
#
#
# $_[2]
# 
# True if the calculation of the standard errors succeeded.  
# Otherwise, false.
#
#
# $_[3]
# 
# 
# Format string for the sprintf function.  Its first field is for
# the parameter value; its second field is for the standard error.
#
#
# $_[4]
# 
# Format string for the sprintf function for the case where the 
# parameter value is missing or ill-defined.  Its first field is 
# a text version of the value; its second field is for the standard 
# error.
# 
# 
# $_[5]
# 
# Format string for the sprintf function for the case where the 
# standard error value is missing or ill-defined.  Its first field
# is for the parameter value; its second field a text version 
# of the standard error value.
# 
# 
# $_[6]
# 
# Format string for the sprintf function for the case where the 
# parameter value and the standard error value are missing or 
# ill-defined.  Its first field is a text version of the value; 
# its second field a text version of the standard error value.
# 
# 
# Return Values
# -------------
# 
# The return value is a string formatted using sprintf that 
# contains the number and its standard error.
# 
#*****************************************************************

sub formatValAndSE
{
  # Get the arguments.
  my $xOut            = shift;
  my $xSE             = shift;
  my $SEOk            = shift;
  my $format          = shift;
  my $noValFormat     = shift;
  my $noSEFormat      = shift;
  my $noValOrSEFormat = shift;

  # If either of the values is not a well formatted number, i.e., 
  # if it contains two or more consecutive nondigit characters), 
  # then return a text version of the value.
  if ( $xOut =~ /[a-zA-Z]{2,}/ and $xSE =~ /[a-zA-Z]{2,}/ )
  {
    return sprintf( $noValOrSEFormat, $xOut, $xSE );
  }
  if ( $xOut =~ /[a-zA-Z]{2,}/ )
  {
    return sprintf( $noValFormat, $xOut, $xSE );
  }
  if ( $xSE =~ /[a-zA-Z]{2,}/ )
  {
    return sprintf( $noSEFormat, $xOut, $xSE );
  }

  # Return a no value indicator if the standard errors are missing.
  if ( $SEOk eq 'false' )
  {
    return sprintf( $noSEFormat, $xOut, 'no_value' );
  }

  # Return the formatted results.
  return sprintf( $format, $xOut, $xSE );
}


#*****************************************************************
#
# Subroutine: formatPotMissVal
#
#
# Formats a numeric value that is potentially missing or ill-defined.
#
#
# Arguments
# ---------
# 
# $_[0]
# 
# Value to format.
# 
# 
# $_[1]
# 
# 
# Format string for the sprintf function.  Its only field is for
# the value.
#
#
# $_[2]
# 
# Format string for the sprintf function for the case where the 
# value is missing or ill-defined.  Its only field is for a text
# version of the value.
# 
# 
# Return Values
# -------------
# 
# The return value is a string formatted using sprintf that 
# contains the value.
# 
#*****************************************************************

sub formatPotMissVal
{
  # Get the arguments.
  my $x           = shift;
  my $format      = shift;
  my $noValFormat = shift;

  # If the value is not a well formatted number, i.e., if it contains
  # two or more consecutive nondigit characters), then return a text
  # version of the value.
  if ( $x =~ /[a-zA-Z]{2,}/ )
  {
    return sprintf( $noValFormat, $x );
  }

  # Return the formatted results.
  return sprintf( $format, $x );
}


#*****************************************************************
#
# Subroutine: relDiffTest
#
#
# Performs the relative difference test that checks to see if 
# two numbers are equal up to relative tolerance.
#
#
# Arguments
# ---------
# 
# $_[0]
# 
# First number for the test.
# 
# 
# $_[1]
# 
# Second number for the test.
#
#
# $_[2]
# 
# Tolerance for the test.
#
#
# $_[3]
# 
# Format string for the sprintf function.  Its first field is 
# for the relative difference of the numbers; its second field
# will contain 'Y' or 'N' depending on the results of the test.
# 
# 
# $_[4]
# 
# Format string for the sprintf function for the case where the 
# test cannot be performed.  Its only field is a string that will 
# contain the reason that the test was not performed.
# 
# 
# Return Values
# -------------
# 
# The return value is a string formatted using sprintf that 
# contains the results of the test.
# 
#*****************************************************************

sub relDiffTest
{
  # Get the arguments.
  my $x              = shift;
  my $y              = shift;
  my $tol            = shift;
  my $format         = shift;
  my $skipTestFormat = shift;

  # If either value is not a well formatted number, i.e., if it 
  # contains two or more consecutive nondigit characters), then 
  # don't perform the test.
  if ( $x =~ /[a-zA-Z]{2,}/ or $y =~ /[a-zA-Z]{2,}/ )
  {
    return sprintf( $skipTestFormat, 'missing_val' );
  }

  # Determine if x and y are equal within relative tolerance.
  my $pass;
  if ( isRelTolEqual( $x, $y, $tol ) )
  {
    $pass = 'Y';
  }
  else
  {
    $pass = 'N';
  }

  # Set the scale for calculating the relative difference.
  my $scale;
  if ( $x == 0 and $y == 0 )
  {
    $scale = 1.0;
  }
  elsif ( abs( $x ) > abs( $y ) )
  {
    $scale = abs( $x );
  }
  else
  {
    $scale = abs( $y );
  }

  # Calculate the relative difference.
  my $relDiff = abs( $x - $y ) / $scale;

  # Return the formatted results.
  return sprintf( $format, $relDiff, $pass );
}


#*****************************************************************
#
# Subroutine: calcRelDiff
#
#
# Calculates the relative difference between two numbers.
#
#
# Arguments
# ---------
# 
# $_[0]
# 
# First number.
# 
# 
# $_[1]
# 
# Second number.
#
#
# $_[3]
# 
# Format string for the sprintf function.  Its only field is 
# for the relative difference of the numbers.
# 
# 
# $_[4]
# 
# Format string for the sprintf function for the case where one
# of the values is missing.  Its only field is for the reason that
# the value is missing.
# 
# 
# Return Values
# -------------
# 
# The return value is a string formatted using sprintf that 
# contains the relative difference of the numbers.
# 
#*****************************************************************

sub calcRelDiff
{
  # Get the arguments.
  my $x           = shift;
  my $y           = shift;
  my $format      = shift;
  my $noValFormat = shift;

  # If either value is not a well formatted number, i.e., if it 
  # contains two or more consecutive nondigit characters), then 
  # don't calculate the relative difference.
  if ( $x =~ /[a-zA-Z]{2,}/ or $y =~ /[a-zA-Z]{2,}/ )
  {
    return sprintf( $noValFormat, 'miss_val' );
  }

  # Set the scale for calculating the relative difference.
  my $scale;
  if ( $x == 0 and $y == 0 )
  {
    $scale = 1.0;
  }
  elsif ( abs( $x ) > abs( $y ) )
  {
    $scale = abs( $x );
  }
  else
  {
    $scale = abs( $y );
  }

  # Calculate the relative difference.
  my $relDiff = abs( $x - $y ) / $scale;

  # Return the formatted results.
  return sprintf( $format, $relDiff );
}


#*****************************************************************
#
# Subroutine: calcAbsDiff
#
#
# Calculates the absolute difference between two numbers.
#
#
# Arguments
# ---------
# 
# $_[0]
# 
# First number.
# 
# 
# $_[1]
# 
# Second number.
#
#
# $_[3]
# 
# Format string for the sprintf function.  Its only field is 
# for the absolute difference of the numbers.
# 
# 
# $_[4]
# 
# Format string for the sprintf function for the case where the 
# test cannot be performed.  Its only field is a string that will 
# contain the reason that the test was not performed.
# 
# 
# Return Values
# -------------
# 
# The return value is a string formatted using sprintf that 
# contains the absolute difference of the numbers.
# 
#*****************************************************************

sub calcAbsDiff
{
  # Get the arguments.
  my $x              = shift;
  my $y              = shift;
  my $format         = shift;
  my $skipTestFormat = shift;

  # If either value is not a well formatted number, i.e., if it 
  # contains two or more consecutive nondigit characters), then 
  # don't perform the test.
  if ( $x =~ /[a-zA-Z]{2,}/ or $y =~ /[a-zA-Z]{2,}/ )
  {
    return sprintf( $skipTestFormat, 'miss_val' );
  }

  # Calculate the absolute difference.
  my $absDiff = abs( $x - $y );

  # Return the formatted results.
  return sprintf( $format, $absDiff );
}


#*****************************************************************
#
# Subroutine: meetSpecTest
#
#
# Performs the "Meet Specification" test that checks to see if the 
# NONMEM and SPK final estimates for a parameter are equal up to
# the precision they should have if they meet their source 
# program's specifications.
#
#
# Arguments
# ---------
# 
# $_[0]
# 
# NONMEM final estimate for the parameter x.
# 
# 
# $_[1]
# 
# NSIG, the desired number of significant in the final NONMEM 
# parameter estimate for x.
#
#
# $_[2]
# 
# SPK final estimate for the parameter x.
#
#
# $_[3]
# 
# SPK lower limit for the parameter x.
#
#
# $_[4]
# 
# SPK upper limit for the parameter x.
#
#
# $_[5]
# 
# The SPK parameter that is used to determine if an acceptable 
# final estimate has been found for x.
#
#
# $_[6]
# 
# True if the SPK test driver parameterizes this parameter the same way as
# its NONMEM counterpart.  Otherwise, false.
#
#
# $_[7]
# 
# Format string for the sprintf function.  Its first field is 
# for the maximum allowed difference between the NONMEM and SPK
# final estimates for x; its second field will contain 'Y' or 'N'
# depending on the results of the test.
# 
# 
# $_[8]
# 
# Format string for the sprintf function for the case where the 
# test cannot be performed.  Its only field is a string that will 
# contain the reason that the test was not performed.
# 
# 
# Return Values
# -------------
# 
# The return value is a string formatted using sprintf that 
# contains the results of the test.
# 
#*****************************************************************

sub meetSpecTest
{
  # Get the arguments.
  my $xOutNonmem      = shift;
  my $nsig            = shift;
  my $xOutSpk         = shift;
  my $xLowSpk         = shift;
  my $xUpSpk          = shift;
  my $epsilon         = shift;
  my $sameParam       = shift;
  my $format          = shift;
  my $skipTestFormat  = shift;

  # If either value is not a well formatted number, i.e., if it 
  # contains two or more consecutive nondigit characters), then 
  # don't perform the test.
  if ( $xOutNonmem =~ /[a-zA-Z]{2,}/ or $xOutSpk =~ /[a-zA-Z]{2,}/ )
  {
    return sprintf( $skipTestFormat, 'missing_val' );
  }

  # Don't perform the test if SPK uses a different parameterization 
  # than NONMEM does for this parameter.
  if ( $sameParam eq 'false' )
  {
    return sprintf( $skipTestFormat, 'diff_par' );
  }

  # Check the SPK limits.
  $xUpSpk >= $xLowSpk or die "Upper limit for x is smaller than its lower limit.";

  # In NONMEM, a parameter value xOut is accepted as an estimate 
  # for xHat, the true minimizer of the objective function, if
  # it has NSIG significant digits, i.e., if
  #
  #                                  -NSIG
  #     | xOut - xHat |  <=  0.5 * 10       xOut  .
  #
  # Calculate the maximum difference between an xOut value and 
  # xHat if the xOut value meets the NONMEM specifications.
  my $maxNonmemAbsDiff = abs( $xOutNonmem ) * 0.5 * 10.0 ** ( -$nsig );

  # In SPK, a parameter value xOut is accepted as an estimate 
  # for xHat, the true minimizer of the objective function, if
  #
  #     | xOut - xHat |  <=  epsilon ( xUp - xLow )  .
  #
  # Calculate the maximum difference between an xOut value and 
  # xHat if the xOut value meets the SPK specifications.
  my $maxSpkAbsDiff = $epsilon * ( $xUpSpk - $xLowSpk );

  # Calculate the largest absolute difference between the NONMEM and 
  # SPK xOut values that meets the specifications of both programs.
  my $maxAbsDiff = $maxNonmemAbsDiff + $maxSpkAbsDiff;

  # Determine if the xOut values meet their specifications.
  my $pass;
  if ( abs( $xOutNonmem - $xOutSpk ) <= $maxAbsDiff )
  {
    $pass = 'Y';
  }
  else
  {
    $pass = 'N';
  }

  # Return the formatted results.
  return sprintf( $format, $maxAbsDiff, $pass );
}


#*****************************************************************
#
# Subroutine: withinSETest
#
#
# Performs the "Within Standard Errors" test that checks to see if 
# the NONMEM and SPK final estimates for a parameter are equal up to
# the precision they should have based on their standard errors.
#
#
# Arguments
# ---------
# 
# $_[0]
# 
# NONMEM final estimate for the parameter x.
# 
# 
# $_[1]
# 
# NONMEM standard error for the final estimate for the parameter x.
#
#
# $_[2]
# 
# True if the calculation of the NONMEM standard errors succeeded.  
# Otherwise, false.
#
#
# $_[3]
# 
# SPK final estimate for the parameter x.
#
#
# $_[4]
# 
# SPK standard error for the final estimate for the parameter x.
#
#
# $_[5]
# 
# True if the calculation of the SPK standard errors succeeded.  
# Otherwise, false.
#
#
# $_[6]
# 
# Format string for the sprintf function.  Its first field is 
# for the maximum allowed difference between the NONMEM and SPK
# final estimates for x; its second field will contain 'Y' or 'N'
# depending on the results of the test.
#
#
# $_[7]
# 
# Format string for the sprintf function for the case where the 
# test cannot be performed.  Its only field is a string that will 
# contain the reason that the test was not performed.
# 
# 
# Return Values
# -------------
# 
# The return value is a string formatted using sprintf that 
# contains the results of the test.
# 
#*****************************************************************

sub withinSETest
{
  # Get the arguments.
  my $xOutNonmem      = shift;
  my $xSENonmem       = shift;
  my $SEOkNonmem      = shift;
  my $xOutSpk         = shift;
  my $xSESpk          = shift;
  my $SEOkSpk         = shift;
  my $format          = shift;
  my $skipTestFormat  = shift;

  # If either value is not a well formatted number, i.e., if it 
  # contains two or more consecutive nondigit characters), then 
  # don't perform the test.
  if ( $xOutNonmem =~ /[a-zA-Z]{2,}/ or $xOutSpk =~ /[a-zA-Z]{2,}/ )
  {
    return sprintf( $skipTestFormat, 'missing_val' );
  }

  # Don't perform the test if either of the standard errors is missing.
  if ( $SEOkNonmem eq 'false' or $SEOkSpk eq 'false' or
    $xSENonmem =~ /[a-zA-Z]{2,}/ or $xSESpk =~ /[a-zA-Z]{2,}/ )
  {
    return sprintf( $skipTestFormat, 'missing_SE' );
  }

  # Check the signs of the standard errors.
  $xSENonmem >= 0 or die "NONMEM standard errors are negative.";
  $xSESpk >= 0 or die "SPK standard errors are negative.";

  # This test assumes that if a parameter value xOut is accepted 
  # as an estimate for xHat, the true minimizer of the objective 
  # function, and if the xOut value has a standard error associated
  # with it, then that value is within one standard error multiple
  # of xHat, i.e., that
  #
  #     | xOut - xHat |  <=  xSE  .
  #
  # Calculate the maximum difference between the NONMEM and SPK xOut 
  # values if both values are within one standard error multiple of xHat.
  my $maxAbsDiff = $xSENonmem + $xSESpk;

  # Determine if the xOut values meet their specifications.
  my $pass;
  if ( abs( $xOutNonmem - $xOutSpk ) <= $maxAbsDiff )
  {
    $pass = 'Y';
  }
  else
  {
    $pass = 'N';
  }

  # Return the formatted results.
  return sprintf( $format, $maxAbsDiff, $pass );
}

