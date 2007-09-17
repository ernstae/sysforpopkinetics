#! /usr/bin/perl

use strict;
use warnings;
use convert;
package nonmem;

#-------------------------------------------------------------------------------
#
# nonmem::simulate( CONTROL, NEWDATA, LOG )
#
# CONTROL:  A nonmem control file for simulation.
# NEWDATA:  The name of a will-be-simulated data file.
# LOG:      The name of a will-be-generated log file.
#
# Returns 1 if suceeds.  0 otherwise.
#
#-------------------------------------------------------------------------------
sub simulate
{
  my $control = $_[0];
  my $newdata = $_[1];
  my $log     = $_[2];
  if( -f $newdata )
  {
    print ( "... An old $newdata was found... I must delete it...\n" );
    unlink( $newdata );
  }
  my @command = ( "/bin/csh", "./nmfe5", $control, $log );
  
  print( "\n\n\n" );
  if ( system( @command ) != 0 )
  {
    die "\nERROR!!! NONMEM simulation failed for some reason!!!\n\n";
  }
  print( "\n\n\n" );
  
  if( not -f $newdata )
  {
     print( "\nERROR!!! NONMEM simulation did not produce a new data set for some reason!!!\n\n" );
     return 0;
  }
  return 1;
}

#
# Returns 1 if succeeds.  0 otherwise.
#
sub estimate
{
  my $control = $_[0];
  my $output  = $_[1];
  if( -f $output )
  {
    print ( "... An old $output was found... I must delete it...\n" );
    unlink( $output );
  }
  my @command = ( "/bin/csh", "./nmfe5", $control, $output );

#  print( "> ", join( " ", @command ) );

  print( "\n\n\n" );
  if ( system( @command ) != 0 )
  {
    die "\nERROR!!! NONMEM estimation failed for some reason!!!\n\n";
  }
  print( "\n\n\n" );

  if( not -f $output )
  {
    print( "\nERROR!!! NONMEM estimation did not produce an output file for some reason!!!\n\n" );
    return 0;
  }
  return 1;
}

#-------------------------------------------------------------------------------
#
# generateADVAN2basedSimControl( CONTROL,
#                                TITLE,
#                                PK_MODEL,
#                                ERROR_MODEL,
#                                APPROXIMATION,
#                                BASEDATA_FILE,
#                                SIMULATED_DATA_FILE,
#                                THETA_LEN,
#                                pTHETA_LOW,
#                                pTHETA_IN,
#                                pTHETA_UP,
#                                OMEGA_DIM,
#                                OMEGA_STRUCTURE,
#                                ppOMEGA_IN,
#                                SIGMA_DIM,
#                                SIGMA_STRUCTURE,
#                                ppSIGMA_IN,
#                              )
#
# CONTROL:       The name of a will-be-generated control file.
#
# TITLE:         A brief description of the estimation that will appear in $PROB
#                section of the generated NONMEM control file.
#
# PK_MODEL:      A string specifying the type of PK model.
#                Valid values are [ $convert::PK_ADD | $convert::PK_PROP | $convert::PK_EXP ].
#
# ERROR_MODEL:   A string specifying the type of ERROR model.
#                Valid values are [ $convert::ERROR_ADD | $convert::ERROR_DATA | $convert::ERROR_MODEL ].
#
# APPROXIMAITON: A string specifying the type of approxmiation (method).
#                Valid values are [ $convert::LAPLACE | $convert::FOCE | $convert::FO ].
#
# BASEDATA_FILE: A string specifying the name of the data file that will serve as the base
#                for simlating a new data set.
#
# SIMULATED_DATA_FILE:
#                A string specifying the filename of the newly simulated data set.
#
# THETA_LEN:     A number specifying the length of theta vector.
#
# pTHETA_LOW:    A reference to an array containing the lower boundary values for theta.
#
# pTHETA_IN:     A reference to an array containing the initial values for theta.
#
# pTHETA_UP:     A reference to an array containing the upper boundary values for theta.
#
# OMEGA_DIM:     A number specifying the dimension of the symmetic matrix Omega.
#                If 3 by 3, give 3.
#
# OMEGA_STRUCTURE:
#                A string specifying the structure of the symmetric Omega matrix.
#                Valid values are [ $convert::DIAG | $convert::FULL ].
#
# ppOMEGA_IN:    A reference to an array of references to other arrays,
#                each containing the i-th row of initial values for Omega.
#
#                ex. When Omega is full.
#                ppOMEGA_IN -> { pROW_1, pROW_2, pROW_3 }
#                                 |         |       |
#                                 |         |       |-> { o31, o32, o33 }
#                                 |         |-> { o21, o22 }
#                                 |-> { o11 }
#
# SIGMA_DIM:     A number specifying the dimension of symmetric matrix Sigma.
#                If 3 by 3, say 3.
#
# SIGMA_STRUCTURE:
#                A string specifying the structure of the symmetric Sigma matrix.
#                Valid values are [ $convert::DIAG | $convert::FULL ].
#
# ppSIGMA_IN:    A reference to an array of references to other arrays,
#                each containing the i-th row of initial values for Sigma.
#
#                ex. When Omega is full.
#                ppOMEGA_IN -> { pROW_1, pROW_2, pROW_3 }
#                                 |         |       |
#                                 |         |       |-> { s31, s32, s33 }
#                                 |         |-> { s21, s22 }
#                                 |-> { s11 }
#
# Returns 1 if suceeds.  0 otherwise.
#
#-------------------------------------------------------------------------------
sub generateADVAN2basedSimControl
{
  my $control         = shift;
  my $title           = shift;
  my $pkModel         = shift;
  my $errorModel      = shift;
  my $approximation   = shift;
  my $base_data       = shift;
  my $simulated_data  = shift;
  my $thetaLen        = shift;
  my $pThetaLow       = shift;   #reference to an array of numbers
  my $pThetaIn        = shift;   #reference to an array of numbers
  my $pThetaUp        = shift;   #reference to an array of numbers
  my $omegaDim        = shift;
  my $omegaStruct     = shift;
  my $ppOmegaIn       = shift;   #reference to an array of references to arrays
  my $sigmaDim        = shift;
  my $sigmaStruct     = shift;
  my $ppSigmaIn       = shift;   #reference to an array of references to arrays

  writeCommon( $control,
               "Data simulation for: ".$title,
               $pkModel,
               $errorModel,
               $approximation,
               $base_data,
               $thetaLen,
               $pThetaLow,
               $pThetaIn,
               $pThetaUp,
               $omegaDim,
               $omegaStruct,
               $ppOmegaIn,
               $sigmaDim,
               $sigmaStruct,
               $ppSigmaIn
             );
  
  my $random = 50000;
  open( OFH, ">>".$control ) or die return 0;
  
  print( OFH "\$SIMULATION( $random )\n" );
  print( OFH "\$TABLE NOAPPEND NOPRINT NOHEADER FILE = $simulated_data ID DOSE TIME DV WT\n" );
  
  close( OFH );
  return 1;
}

#-------------------------------------------------------------------------------
#
# generateADVAN2basedEstControl( CONTROL,
#                                TITLE,
#                                PK_MODEL,
#                                ERROR_MODEL,
#                                APPROXIMATION,
#                                BASEDATA_FILE,
#                                THETA_LEN,
#                                pTHETA_LOW,
#                                pTHETA_IN,
#                                pTHETA_UP,
#                                OMEGA_DIM,
#                                OMEGA_STRUCTURE,
#                                ppOMEGA_IN,
#                                SIGMA_DIM,
#                                SIGMA_STRUCTURE,
#                                ppSIGMA_IN,
#                              )
#
# CONTROL:       The name of a will-be-generated control file.
#
# TITLE:         A brief description of the estimation that will appear in $PROB
#                section of the generated NONMEM control file.
#
# PK_MODEL:      A string specifying the type of PK model.
#                Valid values are [ $convert::PK_ADD | $convert::PK_PROP | $convert::PK_EXP ].
#
# ERROR_MODEL:   A string specifying the type of ERROR model.
#                Valid values are [ $convert::ERROR_ADD | $convert::ERROR_DATA | $convert::ERROR_MODEL ].
#
# APPROXIMAITON: A string specifying the type of approxmiation (method).
#                Valid values are [ $convert::LAPLACE | $convert::FOCE | $convert::FO ].
#
# BASEDATA_FILE: A string specifying the name of the data file that will serve as the base
#                for simlating a new data set.
#
# THETA_LEN:     A number specifying the length of theta vector.
#
# pTHETA_LOW:    A reference to an array containing the lower boundary values for theta.
#
# pTHETA_IN:     A reference to an array containing the initial values for theta.
#
# pTHETA_UP:     A reference to an array containing the upper boundary values for theta.
#
# OMEGA_DIM:     A number specifying the dimension of the symmetic matrix Omega.
#                If 3 by 3, give 3.
#
# OMEGA_STRUCTURE:
#                A string specifying the structure of the symmetric Omega matrix.
#                Valid values are [ $convert::DIAG | $convert::FULL ].
#
# ppOMEGA_IN:    A reference to an array of references to other arrays,
#                each containing the i-th row of initial values for Omega.
#
#                ex. When Omega is full.
#                ppOMEGA_IN -> { pROW_1, pROW_2, pROW_3 }
#                                 |         |       |
#                                 |         |       |-> { o31, o32, o33 }
#                                 |         |-> { o21, o22 }
#                                 |-> { o11 }
#
# SIGMA_DIM:     A number specifying the dimension of symmetric matrix Sigma.
#                If 3 by 3, say 3.
#
# SIGMA_STRUCTURE:
#                A string specifying the structure of the symmetric Sigma matrix.
#                Valid values are [ $convert::DIAG | $convert::FULL ].
#
# ppSIGMA_IN:    A reference to an array of references to other arrays,
#                each containing the i-th row of initial values for Sigma.
#
#                ex. When Omega is full.
#                ppOMEGA_IN -> { pROW_1, pROW_2, pROW_3 }
#                                 |         |       |
#                                 |         |       |-> { s31, s32, s33 }
#                                 |         |-> { s21, s22 }
#                                 |-> { s11 }
#
# POP_MAX_ITERATION: A number specifying the maximum number of iterations
#                allowed for population parameter estimate.
#
#
# Returns 1 if suceeds.  0 otherwise.
#
#-------------------------------------------------------------------------------
sub generateADVAN2basedEstControl
{
  my $control         = shift;
  my $title           = shift;
  my $pkModel         = shift;
  my $errorModel      = shift;
  my $approximation   = shift;
  my $base_data       = shift;
  my $thetaLen        = shift;
  my $pThetaLow       = shift;   #reference to an array of numbers
  my $pThetaIn        = shift;   #reference to an array of numbers
  my $pThetaUp        = shift;   #reference to an array of numbers
  my $omegaDim        = shift;
  my $omegaStruct     = shift;
  my $ppOmegaIn       = shift;   #reference to an array of references to arrays
  my $sigmaDim        = shift;
  my $sigmaStruct     = shift;
  my $ppSigmaIn       = shift;   #reference to an array of references to arrays
  my $covForm         = shift;
  my $maxItr          = shift;

  writeCommon( $control,
               "Parameter estimation: ".$title,
               $pkModel,
               $errorModel,
               $approximation,
               $base_data,
               $thetaLen,
               $pThetaLow,
               $pThetaIn,
               $pThetaUp,
               $omegaDim,
               $omegaStruct,
               $ppOmegaIn,
               $sigmaDim,
               $sigmaStruct,
               $ppSigmaIn,
               $covForm
             );

  my $random = rand();
  open( OFH, ">>".$control );

  print( OFH "\$EST MAXEVAL=$maxItr PRINT=5 POSTHOC " );
  if( $approximation =~ /$convert::LAPLACE/ )
  {
    if( $errorModel =~ /$convert::ERR_MODEL/ )
    {
      print( OFH "METHOD=COND LAPLACE -2LL NOABORT" );
    }
    else
    {
      print( OFH "METHOD=COND LAPLACE NOABORT" );
    }
  }
  elsif( $approximation =~ /$convert::FOCE/ )
  {
    if( $errorModel =~ /$convert::ERR_MODEL/ )
    {
      print( OFH "METHOD=COND INTERACTION NOABORT" );
    }
    else
    {
      print( OFH "METHOD=COND NOABORT" );
    }
  }
  print( OFH "\n" );
  print( OFH "\$COV " );
  if( $covForm == 1 )
  {
    print( OFH "\n" );  #nonmem default (D)
  }
  elsif( $covForm == 3 )
  {
    print( OFH "MATRIX=S\n" );
  }
  else
  {
    print( OFH "MATRIX=R\n" );
  }

  print( OFH "\$TABLE ID ETA(1) ETA(2) ETA(3) IPRED\n" );
  close( OFH );
}
#-------------------------------------------------------------------------------
#
# writeCommon ( CONTROL,
#               TITLE,
#               PK_MODEL,
#               ERROR_MODEL,
#               APPROXIMATION,
#               BASEDATA_FILE,
#               THETA_LEN,
#               pTHETA_LOW,
#               pTHETA_IN,
#               pTHETA_UP,
#               OMEGA_DIM,
#               OMEGA_STRUCTURE,
#               ppOMEGA_IN,
#               SIGMA_DIM,
#               SIGMA_STRUCTURE,
#               ppSIGMA_IN,
#             )
#
# CONTROL:       The name of a will-be-generated control file.
#
# TITLE:         A brief description of the estimation that will appear in $PROB
#                section of the generated NONMEM control file.
#
# PK_MODEL:      A string specifying the type of PK model.
#                Valid values are [ $convert::PK_ADD | $convert::PK_PROP | $convert::PK_EXP ].
#
# ERROR_MODEL:   A string specifying the type of ERROR model.
#                Valid values are [ $convert::ERROR_ADD | $convert::ERROR_DATA | $convert::ERROR_MODEL ].
#
# APPROXIMAITON: A string specifying the type of approxmiation (method).
#                Valid values are [ $convert::LAPLACE | $convert::FOCE | $convert::FO ].
#
# BASEDATA_FILE: A string specifying the name of the data file that will serve as the base
#                for simlating a new data set.
#
# THETA_LEN:     A number specifying the length of theta vector.
#
# pTHETA_LOW:    A reference to an array containing the lower boundary values for theta.
#
# pTHETA_IN:     A reference to an array containing the initial values for theta.
#
# pTHETA_UP:     A reference to an array containing the upper boundary values for theta.
#
# OMEGA_DIM:     A number specifying the dimension of the symmetic matrix Omega.
#                If 3 by 3, give 3.
#                Only 3 by 3 is supported at this point.
#
# OMEGA_STRUCTURE:
#                A string specifying the structure of the symmetric Omega matrix.
#                Valid values are [ $convert::DIAG | $convert::FULL ].
#
# ppOMEGA_IN:    A reference to an array of references to other arrays,
#                each containing the i-th row of initial values for Omega.
#
#                ex. When Omega is full.
#                ppOMEGA_IN -> { pROW_1, pROW_2, pROW_3 }
#                                 |         |       |
#                                 |         |       |-> { o31, o32, o33 }
#                                 |         |-> { o21, o22 }
#                                 |-> { o11 }
#
# SIGMA_DIM:     A number specifying the dimension of symmetric matrix Sigma.
#                If 1 by 1, say 1.
#                Only 1 by 1 is supported at this point.
#
# SIGMA_STRUCTURE:
#                A string specifying the structure of the symmetric Sigma matrix.
#                Valid values are [ $convert::DIAG | $convert::FULL ].
#
# ppSIGMA_IN:    A reference to an array of references to other arrays,
#                each containing the i-th row of initial values for Sigma.
#
#                ex. When Omega is full.
#                ppOMEGA_IN -> { pROW_1, pROW_2, pROW_3 }
#                                 |         |       |
#                                 |         |       |-> { s31, s32, s33 }
#                                 |         |-> { s21, s22 }
#                                 |-> { s11 }
#
# Returns 1 if suceeds.  0 otherwise.
#
#-------------------------------------------------------------------------------
sub writeCommon
{
  my $output_filename = shift;
  my $title           = shift;
  my $pkModel         = shift;
  my $errorModel      = shift;
  my $approximation   = shift;
  my $data_file       = shift;
  my $thetaLen        = shift;
  my $pThetaLow       = shift;  #reference to an array of numbers
  my $pThetaIn        = shift;  #reference to an array of numbers
  my $pThetaUp        = shift;  #reference to an array of numbers
  my $OmegaDim        = shift;
  my $OmegaStruct     = shift;
  my $ppOmegaIn       = shift;   #reference to an array of references to arrays
  my $SigmaDim        = shift;
  my $SigmaStruct     = shift;
  my $ppSigmaIn       = shift;   #reference to an array of references to arrays

  $thetaLen == 3 or die "\n\nERROR!!!  The (theta) length of $thetaLen is not supported at this point.  Must be 3.\n";
  $OmegaDim == 3 or die "\n\nERROR!!!  The (Omega) dimension of $OmegaDim is not supported at this point.  Must be 3.\n";
  $SigmaDim == 1 or die "\n\nERROR!!!  The (Sigma) dimension of $SigmaDim is not supported at this point.  Must be 1.\n";
  open( OFH, ">".$output_filename );
  
  print( OFH "\$PROB $title\n" );
  print( OFH "\$INPUT ID DOSE=AMT TIME CP=DV WT\n" );
  print( OFH "\$SUBROUTINES ADVAN2\n" );
  print( OFH "\$PK\n" );
  print( OFH "CALLFL = 1\n" );

  if( $pkModel =~ /$convert::PK_ADD/ )
  {
    print( OFH "KA = THETA(1) + ETA(1)\n" );
    print( OFH "K  = THETA(2) + ETA(2)\n" );
    print( OFH "CL = THETA(3) * WT + ETA(3)\n" );
  }
  elsif( $pkModel =~ /$convert::PK_PROP/ )
  {
    print( OFH "KA = THETA(1) * (1.0 + ETA(1))\n" );
    print( OFH "K  = THETA(2) * (1.0 + ETA(2))\n" );
    print( OFH "CL = THETA(3) * WT * (1.0 + ETA(3))\n" );
  }
  elsif( $pkModel =~ /$convert::PK_EXP/ )
  {
    print( OFH "KA = THETA(1) * EXP(ETA(1))\n" );
    print( OFH "K  = THETA(2) * EXP(ETA(2))\n" );
    print( OFH "CL = THETA(3) * WT * EXP(ETA(3))\n" );
  }
  else
  {
    die "ERROR!!!  Invalid PK model (you gave me type $pkModel)!!!\n";
  }
  print( OFH "SC=CL/K/WT\n" );
  
  print( OFH "\$ERROR\n" );
  print( OFH "IPRED=F  ; This will contain the conditional value for F.\n" );
  if( $errorModel =~ /$convert::ERR_ADD/ )
  {
    print( OFH "Y = F + EPS(1)\n" );
  }
  elsif( $errorModel =~ /$convert::ERR_DATA/ )
  {
    print( OFH "Y = F + DV * EPS(1)\n" );
  }
  elsif( $errorModel =~ /$convert::ERR_MODEL/ )
  {
    if( $approximation =~ /$convert::LAPLACE/ )
    {
      print( OFH "VAR=F**2*THETA(4)\n" );
      print( OFH "Y=LOG(VAR)+(DV-F)**2/VAR\n" );
    }
    else
    {
      print( OFH "Y= F * (1.0 + EPS(1))\n" );
    }
  }
  else
  {
    die "ERROR!!!  Invalid ERROR model (you game me type $errorModel)!!!\n";
  }
  print( OFH "\$THETA" );
  for( my $i=0; $i<$thetaLen; $i++ )
  {
    print( OFH " (" );
    print( OFH $$pThetaLow[$i] );
    print( OFH ", " );
    print( OFH $$pThetaIn[$i] );
    print( OFH ", " );
    print( OFH $$pThetaUp[$i] );
    print( OFH ")" );
  }
  if( $approximation =~ /$convert::LAPLACE/ and $errorModel =~ /$convert::ERR_MODEL/ )
  {
    print( OFH " (" );
    print( OFH $ppSigmaIn->[0][0] / 5.0 );
    print( OFH ", " );
    print( OFH $ppSigmaIn->[0][0] * 1.0 );
    print( OFH ", " );
    print( OFH $ppSigmaIn->[0][0] * 5.0 );
    print( OFH ") " );
  }

  print( OFH "\n" );
  print( OFH "\$OMEGA" );
  if( $OmegaStruct =~ /$convert::DIAG/ )
  {
    print( OFH " DIAG($OmegaDim) " );
    print( OFH $ppOmegaIn->[0][0] );
    print( OFH ", " );
    print( OFH $ppOmegaIn->[1][0] );
    print( OFH ", " );
    print( OFH $ppOmegaIn->[2][0] );
    print( OFH "\n" );
  }
  else
  {
    print( OFH " BLOCK($OmegaDim) " );
    print( OFH join( " ", @{$ppOmegaIn->[0]} ) );
    print( OFH " " );
    print( OFH join( " ", @{$ppOmegaIn->[1]} ) );
    print( OFH " " );
    print( OFH join( " ", @{$ppOmegaIn->[2]} ) );
    print( OFH "\n" );
  }

  if( !( $approximation =~ /$convert::LAPLACE/ and $errorModel =~ /$convert::ERR_MODEL/ ) )
  {
    print( OFH  "\$SIGMA " );
    if( $SigmaDim == 1 )
    {
      print( OFH $ppSigmaIn->[0][0] );
    }
    else
    {
      for( my $i=0; $i<$SigmaDim; $i++ )
      {
        print( OFH join( " ", @{$ppSigmaIn->[$i]} ) );
        print( OFH " " );
      }
    }
    print( OFH "\n" );
  }

  print( OFH "\$DATA $data_file\n" );
  
  close( OFH );
}
1;
