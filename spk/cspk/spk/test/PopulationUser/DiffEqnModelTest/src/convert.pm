use strict;
use warnings;
package convert;

our $PK_ADD      = "additive";
our $PK_EXP      = "exponential";
our $PK_PROP     = "proportional";
our $ERR_ADD     = "additive";
our $ERR_DATA    = "data_based";
our $ERR_MODEL   = "model_based";
our $FO          = "fo";
our $NAIVE_FO    = "naive_fo";
our $FOCE        = "foce";
our $LAPLACE     = "laplace";
our $DIRECT      = "direct";
our $EXPONENTIAL = "exponential";
our $DIAG        = "diagonal";
our $FULL        = "block";

require Exporter;
our @ISA    = qw( Exporter );
our @EXPORT = qw{ $ERR_ADD
                  $ERR_DATA
                  $ERR_MODEL
                  $PK_ADD
                  $PK_EXP
                  $PK_PROP
                  $FO
                  $NAIVE_FO
                  $FOCE
                  $LAPLACE
                  $SIGMA_EXP
                  $SIGMA_DIRECT
                  $DIAG
                  $FULL
                  $DIRECT
                  $EXPONENTIAL
                   };

our $title;
our $subroutines;
our $baseData;
our $nonmemControl;
our $nonmemOut;
our $spkOut;
our $pkModel;
our $errorModel;
our $approximation;
our @thetaParam;
our @theta1;
our @theta2;
our @theta3;
our $thetaRelTol = 2.0e-003;
our $OmegaStruct;
our @OmegaParam1;
our @OmegaParam2;
our @OmegaParam3;
our @Omega1;
our @Omega2;
our @Omega3;
our $OmegaRelTol = 2.0e-003;
our $SigmaStruct;
our @SigmaParam1;
our @SigmaParam2;
our @SigmaParam3;
our @Sigma1;
our @Sigma2;
our @Sigma3;
our $SigmaRelTol = 2.0e-003;
our @eta1;
our @eta2;
our @eta3;
our $covForm;
our $popMitr = 500;
our $indMitr = 500;
#-------------------------------------------------------------------------------
#
# convertRawInputToXml( RAW_FILE, XML_FILE, TITLE, @SUBROUTINES )
#
# RAW_FILE:     the name of raw input file to be converted
# XML_FILE:     the name of will-be-generated XMl file.
# TITLE:        arbtrary text serving as a title or brief description
# @SUBROUTINES: an array of NONMEM canned-prediction routines
#
#
# XML_FILE will be in the following format:
#
# <?xml version 1.0>
# <CompareSpkToNonmemInput>
# <title>$title</title>
# <subroutines><anon>SUB1</anon><anon>SUB2</anon>...</subroutines>
# <base_data>$baseData</base_data>
# <nonmem_outfile>$nonmemOut</nonmem_outfile>
# <spk_outfile>$spkOut</spk_outfile>
# <pk_model>$pkModel</pk_model>
# <error_model>$errorModel</error_model>
# <approximation>$approximation</approximation>
# <theta>
#   <nonmem_param><anon>$thetaParam[0]</anon><anon>$thetaParam[1]</anon><anon>$thetaParam[2]</anon></nonmem_param>
#   <low><anon>$theta1[0]</anon><anon>$theta2[0]</anon><anon>$theta3[0]</anon></low>
#   <in><anon>$theta1[1]</anon><anon>$theta2[1]</anon><anon>$theta3[1]</anon></in>
#   <up><anon>$theta1[2]</anon><anon>$theta2[2]</anon><anon>$theta3[2]</anon></up>
#   <relTol>$thetaRelTol</relTol>
# </theta>
# <Omega>
#   <structure>$OmegaStruct</structure>
#   <nonmem_param><anon>$OmegaParam1[0]</anon></nonmem_param>
#   <nonmem_param><anon>$OmegaParam2[0]</anon><anon>$OmegaParam2[1]</anon></nonmem_param>
#   <nonmem_param><anon>$OmegaParam3[0]</anon><anon>$OmegaParam3[1]</anon><anon>$OmegaParam3[2]</anon></nonmem_param>
#   <in><anon>$Omega1[0]</anon></in>
#   <in><anon>$Omega2[0]</anon><anon>$Omega2[1]</anon></in>
#   <in><anon>$Omega3[0]</anon><anon>$Omega3[1]</anon><anon>$Omega3[2]</anon></in>
#   <relTol>$OmegaRelTol</relTol>
# </Omega>
# <Sigma>
#   <structure>$SigmaStruct</structure>
#   <nonmem_param><anon>$SigmaParam1[0]</anon></nonmem_param>
#   <in><anon>$Sigma1[0]</anon></in>
#   <relTol>$SigmaRelTol</relTol>
# </Sigma>
# <eta>
#   <low><anon>$eta1[0]</anon><anon>$eta2[0]</anon><anon>$eta3[0]</anon></low>
#   <in><anon>$eta1[1]</anon><anon>$eta2[1]</anon><anon>$eta3[1]</anon></in>
#   <up><anon>$eta1[2]</anon><anon>$eta2[2]</anon><anon>$eta3[2]</anon></up>
# <statistics>
#   <population>
#     <covariance>$covForm</covariance>
#   </population>
# </statistics>
# <maxIterations>
#   <population>100</population>
#   <individual>100</individual>
# </maxIterations>
# </CompareSpkToNonmemInput>
#
#-------------------------------------------------------------------------------
sub convertRawInputToXml
{
  scalar( @_ ) == 4
     or die "\nERROR!!!  convertToXml() takes an input file name as the 1st argument!!!\n";
  my $input_file  = $_[0];
  my $xml_file    = $_[1];
  my $title       = $_[2];
  my @subroutines = $_[3];

  ###################################################################
  #
  # Read the raw file and fill global variables.
  #
  ###################################################################
  readRawAndFillGlobalVars( $input_file );

  ###################################################################
  #
  # Write to a xml file.
  #
  ###################################################################
  open( OFH, '>', $xml_file ) or die "\nERROR!!!  Failed to create file \"$xml_file\"\n";

  printf( OFH "<?xml version = \"1.0\"?>\n" );
  printf( OFH "<CompareSpkToNonmemInput>\n" );
  
  printf( OFH "<title>$title</title>\n" );
  printf( OFH "<subroutines>\n" );
  foreach my $subroutine (@subroutines)
  {
    print( OFH "\t<anon>$subroutine</anon>" );
  }
  print( OFH "\n</subroutines>\n" );
  
  printf( OFH "<base_data>$baseData</base_data>\n" );
  
  printf( OFH "<nonmem_outfile>$nonmemOut</nonmem_outfile>\n" );
  
  printf( OFH "<spk_outfile>$spkOut</spk_outfile>\n" );
  
  printf( OFH "<pk_model>$pkModel</pk_model>\n" );
  
  printf( OFH "<error_model>$errorModel</error_model>\n" );
  
  printf( OFH "<approximation>$approximation</approximation>\n" );
  
  printf( OFH "<theta>\n" );
  printf( OFH "\t<nonmem_param><anon>$thetaParam[0]</anon><anon>$thetaParam[1]</anon><anon>$thetaParam[2]</anon></nonmem_param>\n" );
  printf( OFH "\t<low><anon>$theta1[0]</anon><anon>$theta2[0]</anon><anon>$theta3[0]</anon></low>\n" );
  printf( OFH "\t<in><anon>$theta1[1]</anon><anon>$theta2[1]</anon><anon>$theta3[1]</anon></in>\n" );
  printf( OFH "\t<up><anon>$theta1[2]</anon><anon>$theta2[2]</anon><anon>$theta3[2]</anon></up>\n" );
  printf( OFH "\t<relTol>$thetaRelTol</relTol>\n" );
  printf( OFH "</theta>\n" );

  printf( OFH "<Omega>\n" );
  printf( OFH "\t<structure>$OmegaStruct</structure>\n" );
  if( $OmegaStruct eq $DIAG )
  {
    printf( OFH "\t<nonmem_param><anon>$OmegaParam1[0]</anon></nonmem_param>\n" );
    printf( OFH "\t<nonmem_param><anon>$OmegaParam2[0]</anon></nonmem_param>\n" );
    printf( OFH "\t<nonmem_param><anon>$OmegaParam3[0]</anon></nonmem_param>\n" );

    printf( OFH "\t<in><anon>$Omega1[0]</anon></in>\n" );
    printf( OFH "\t<in><anon>$Omega2[0]</anon></in>\n" );
    printf( OFH "\t<in><anon>$Omega3[0]</anon></in>\n" );
  }
  else
  {
    printf( OFH "\t<nonmem_param><anon>$OmegaParam1[0]</anon></nonmem_param>\n" );
    printf( OFH "\t<nonmem_param><anon>$OmegaParam2[0]</anon><anon>$OmegaParam2[1]</anon></nonmem_param>\n" );
    printf( OFH "\t<nonmem_param><anon>$OmegaParam3[0]</anon><anon>$OmegaParam3[1]</anon><anon>$OmegaParam3[2]</anon></nonmem_param>\n" );

    printf( OFH "\t<in><anon>$Omega1[0]</anon></in>\n" );
    printf( OFH "\t<in><anon>$Omega2[0]</anon><anon>$Omega2[1]</anon></in>\n" );
    printf( OFH "\t<in><anon>$Omega3[0]</anon><anon>$Omega3[1]</anon><anon>$Omega3[2]</anon></in>\n" );
  }
  printf( OFH "\t<relTol>$OmegaRelTol</relTol>\n" );
  printf( OFH "</Omega>\n" );
  
  printf( OFH "<Sigma>\n" );
  printf( OFH "\t<structure>$SigmaStruct</structure>\n" );
  printf( OFH "\t<nonmem_param><anon>$SigmaParam1[0]</anon></nonmem_param>\n" );
  printf( OFH "\t<in><anon>$Sigma1[0]</anon></in>\n" );
  printf( OFH "\t<relTol>$SigmaRelTol</relTol>\n" );
  printf( OFH "</Sigma>\n" );
  
  printf( OFH "<eta>\n" );
  printf( OFH "\t<low><anon>$eta1[0]</anon><anon>$eta2[0]</anon><anon>$eta3[0]</anon></low>\n" );
  printf( OFH "\t<in><anon>$eta1[1]</anon><anon>$eta2[1]</anon><anon>$eta3[1]</anon></in>\n" );
  printf( OFH "\t<up><anon>$eta1[2]</anon><anon>$eta2[2]</anon><anon>$eta3[2]</anon></up>\n" );
  printf( OFH "</eta>\n" );
  
  printf( OFH "<statistics>\n" );
  printf( OFH "\t<population>\n" );
  printf( OFH "\t\t<covariance>$covForm</covariance>\n" );
  printf( OFH "\t</population>\n" );
  printf( OFH "</statistics>\n" );
  
  printf( OFH "<max_iterations>\n" );
  printf( OFH "\t<population>$popMitr</population>\n" );
  printf( OFH "\t<individual>$indMitr</individual>\n" );
  printf( OFH "</max_iterations>\n" );
  printf( OFH "</CompareSpkToNonmemInput>\n" );
  close( OFH );

  return $xml_file;
}
#-------------------------------------------------------------------------------
#
# readRawAndFillGlobalVars( RAW_FILE )
# This routine reads RAW_FILE and places read values to variables.
#
# RAW_FILE:  A file listing the initial parameters in the strict order as
#            follows:
#
#        line#
#            1: the name of data file
#            2: the name of will-be-generated NONMEM ctonrol file for estimation.
#            3: the name of will-be-genreated NONEMM output file from estimation.
#            4: the name of will-be-generated SPK output file from estimation.
#            5: the type of PK model [ $convert::PK_ADD | $covert::PK_PROP | $convert::PK_EXP ]
#            6: the type of ERROR model [ $convert::ERROR_ADD | $convert::ERROR_DATA | $convert::ERROR_MODEL ]
#            7: the type of approximation [ $convert::FO | $convert::FOCE | $convert::LAPLACE ]
#            8: the lower, initial and upper boundary values for the first row of theta, theta(1)
#            9: the lower, initial and upper boundary values for the second row of theta, theta(2)
#           10: the lower, initial and upper boundary values for the third row of theta, theta(3)
#           11: the structure of Omega matrix [ b | d ], "b" for block (full), "d" for diagonal
#           12: the initial value for the elements of the first row of Omega
#           13: the initial value(s) for the elements of the second row of Omega
#           14: the initial value(s) for the elements of the third row of Omega
#           15: the way Sigma is parameterized [ sigma_exp | sigma_direct ]
#           16: the sigma(1,1) value.
#           17: the lower, initial and upper boundary values for the first row of eta, eta(1).
#           18: the lower, initial and upper boundary values for the second row of eta, eta(2).
#           19: the lower, initial and upper boundary values for the third row of eta, eta(3).
#           20: the type of covariance matrix of the population parameters.
#               [ 1 | 2 | 3 ], where 1 for MATRIX=D, 2 for MATRIX=R, 3 for MATRIX=S.
#
#-------------------------------------------------------------------------------
sub readRawAndFillGlobalVars
{
  @_ or die "\nERROR!!!  readRaw() takes a file name as an argument!!!\n";
  my $raw_file = $_[0];
  open( IFH, $raw_file ) or die "\n!!!Failed to open \"$raw_file\"!!!\n";
  
  ###################################################################
  # Ask for a data file
  ###################################################################
  $baseData = <IFH>;  chomp( $baseData ); chomp( $baseData );
  
  #print "base data file is ", $baseData, "\n";
  ###################################################################
  # Ask for a NONMEM control file name
  ###################################################################
  $nonmemControl = <IFH>;  chomp( $nonmemControl ); chomp( $nonmemControl );

  #print "nonmem control file is: ", $nonmemControl, "\n";
  ###################################################################
  # Ask for a NONMEM output file name
  ###################################################################
  $nonmemOut = <IFH>;  chomp( $nonmemOut );  chomp( $nonmemOut );
  #print "Nonmem output file name would be: ", $nonmemOut, "\n";

  ###################################################################
  # Ask for a SPK output file name
  ###################################################################
  our $spkOut = <IFH>;  chomp( $spkOut );   chomp( $spkOut );
  #print "Spk output file name would be: ", $spkOut, "\n";

  ###################################################################
  # Ask for a PK model type
  ###################################################################
  my $temp = <IFH>;  chomp( $temp );  chomp( $temp );
  $pkModel = '-';
  if   ( $temp =~ /pk_add/  ){ $pkModel = our $PK_ADD; }
  elsif( $temp =~ /pk_prop/ ){ $pkModel = our $PK_PROP; }
  elsif( $temp =~ /pk_exp/  ){ $pkModel = our $PK_EXP; }
  else { die "\nERROR!!!  Invalid PK model specified in file \"$raw_file\"!!!\n"; }
  
  #print "PK model is: ", $pkModel, "\n";
  ###################################################################
  # Ask for an Error weighting type
  ###################################################################
  $temp = <IFH>;  chomp( $temp );  chomp( $temp );
  $errorModel = '-';
  if   ( $temp =~ /err_add/ )  { $errorModel = our $ERR_ADD; }
  elsif( $temp =~ /err_data/ ) { $errorModel = our $ERR_DATA; }
  elsif( $temp =~ /err_model/ ){ $errorModel = our $ERR_MODEL; }
  else { die "\nERROR!!!  Invalid ERROR model specified in file \"$raw_file\"!!!\n"; }
  
  #print "ERROR model is: ",  $errorModel, "\n";
  ###################################################################
  # Ask for an objective
  ###################################################################
  $temp = <IFH>;  chomp( $temp );  chomp( $temp );
  $approximation = '-';
  if   ( $temp =~ /NAIVE_FO/ ){ $approximation = $NAIVE_FO; }
  elsif( $temp =~ /FOCE/ )    { $approximation = $FOCE; }
  elsif( $temp =~ /$FO/ )     { $approximation = $FO; }
  elsif( $temp =~ /LAPLACE/ ) { $approximation = $LAPLACE; }
  else { die "\nERROR!!!  Invalid approximation type specified in file \"$raw_file\"!!!\n"; }
  
  #print "Appximation is: ", $approximation, "\n";
  ###################################################################
  # theta vector
  ###################################################################
  @theta1 = split( /\s+/, <IFH> );    # theta(1) <- ( low, in, up )
  @theta2 = split( /\s+/, <IFH> );    # theta(2) <- ( low, in, up )
  @theta3 = split( /\s+/, <IFH> );    # theta(3) <- ( low, in, up )

  $thetaParam[0] = $DIRECT;               # theta(1) = alp(1)
  $thetaParam[1] = $DIRECT;               # theta(2) = alp(2)
  $thetaParam[2] = $DIRECT;               # theta(3) = alp(3)

  #print "theta(1): ", join( "\t", @theta1 ), "\n";
  #print "theta(2): ", join( "\t", @theta2 ), "\n";
  #print "theta(3): ", join( "\t", @theta3 ), "\n";
  ###################################################################
  # Omega matrix
  ###################################################################
  $temp = <IFH>;  chomp( $temp );   chomp( $temp );
  if( $temp eq "diag" ){ $OmegaStruct = $DIAG; }
  else{ $OmegaStruct = $FULL; }
  
  @Omega1 = split( /\s+/, <IFH> );
  @Omega2 = split( /\s+/, <IFH> );
  @Omega3 = split( /\s+/, <IFH> );

  if( $OmegaStruct =~ /$DIAG/ )
  {
    $OmegaParam1[0] = $EXPONENTIAL;
    $OmegaParam2[0] = $EXPONENTIAL;
    $OmegaParam3[0] = $EXPONENTIAL;
  }
  else
  {
    $OmegaParam1[0] = $EXPONENTIAL;
    $OmegaParam2[0] = $EXPONENTIAL;  $OmegaParam2[1] = $EXPONENTIAL;
    $OmegaParam3[0] = $EXPONENTIAL;  $OmegaParam3[1] = $EXPONENTIAL;  $OmegaParam3[2] = $EXPONENTIAL;
  }
  #print "Omega(1): ", join( "\t", @Omega1 ), "\n";
  #print "Omega(2): ", join( "\t", @Omega2 ), "\n";
  #print "Omega(3): ", join( "\t", @Omega3 ), "\n";
  ###################################################################
  # Sigma matrix
  ###################################################################
  $temp = <IFH>;  chomp( $temp );  chomp( $temp );
  my $SigmaParamVal = "-";
  if   ( $temp =~ /sigma_direct/ ){ $SigmaParamVal = $DIRECT; }
  elsif( $temp =~ /sigma_exp/ )   { $SigmaParamVal = $EXPONENTIAL; }
  else{ die "\nERROR!!!  Invalid Sigma form!!!\n"; }

  my $SigmaVal = <IFH>;  chomp( $SigmaVal );  chomp( $SigmaVal );

  $SigmaStruct = $DIAG;
  if( $SigmaStruct =~ /$DIAG/ )
  {
    $Sigma1[0] = $SigmaVal;
    $SigmaParam1[0] = $SigmaParamVal;
  }
  else
  {
    $Sigma1[0] = $SigmaVal;
    $SigmaParam1[0] = $SigmaParamVal;
  }

  #print "Sigma form: ", $SigmaParam, "\n";
  #print "Sigma: ", $Sigma, "\n";
  ###################################################################
  # eta vector
  ###################################################################
  @eta1 = split( /\s+/, <IFH> );    # eta(1) <- (low, in, up )

  if( scalar( @eta1 ) == 0 )
  {
    print( "\nWARNING!  eta (low, in, up) values were not provided.\n" );
    print( "... I'll set them to default values (-1e4, 0.0, +1e4) for all ...\n" );
    $eta1[0] = -1e4;
    $eta1[1] =  0.0;
    $eta1[2] = +1e4;
    $eta2[0] = -1e4;
    $eta2[1] =  0.0;
    $eta2[2] = +1e4;
    $eta3[0] = -1e4;
    $eta3[1] =  0.0;
    $eta3[2] = +1e4;
  }
  else
  {
    @eta2 = split( /\s+/, <IFH> );    # eta(2) <- (low, in, up )
    @eta3 = split( /\s+/, <IFH> );    # eta(3) <- (low, in, up )
  }

  #print "eta1 = ", join( "\t", @eta1 ), "\n";
  #print "eta2 = ", join( "\t", @eta2 ), "\n";
  #print "eta3 = ", join( "\t", @eta3 ), "\n";
  ###################################################################
  # Ask for a statistics form
  ###################################################################
  $covForm = <IFH>;
  if( not $covForm )
  {
    $covForm = 2;
    printf ( "\nWARNING! Statistics form is not specified in file \"$raw_file\".\n" );
    printf ( "... I'll substitute it with 2 (R) for you ...\n" );
  }
  #print "Covariance form: ", $covForm, "\n";

  ###################################################################
  #
  # Close input file
  #
  ###################################################################
  close( IFH );
  return;
}
1;

