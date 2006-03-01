/** 
 * @file NM_generateMontePars.cpp
 * Define NonmemTranslator::generateMonteParsNamespace().
 */
#include <fstream>

#include "NonmemTranslator.h"
#include "SpkCompilerException.h"

using namespace std;
using namespace xercesc;

void NonmemTranslator::generateMonteParsNamespace() const
{
  //---------------------------------------------------------------------------------------
  // Generate the MontePars namespace if Monte is requested.
  //---------------------------------------------------------------------------------------
  if( !myIsMonte )
    {
      return;
    }
  ofstream oMontePars( fMontePars_h );
  if( !oMontePars.good() )
    {
      char mess[ SpkCompilerError::maxMessageLen() ];
      snprintf( mess, 
		SpkCompilerError::maxMessageLen(),
		"Failed to create %s file.", fMontePars_h );
      SpkCompilerException e( SpkCompilerError::ASPK_STD_ERR, mess, __LINE__, __FILE__ );
      throw e;
    }
  oMontePars << "//==============================================================================" << endl;
  oMontePars << "// " << endl;
  oMontePars << "// " << myDescription << endl;
  oMontePars << "// " << endl;
  oMontePars << "// The namespace MontePars exports the values needed by monteDriver.cpp." << endl;
  oMontePars << "// " << endl;
  oMontePars << "// The user requested the " << (getTarget()==POP? "population":"individual");
  oMontePars << " analysis." << endl;
  oMontePars << "// " << endl;
  oMontePars << "//==============================================================================" << endl;

  oMontePars << "#ifndef MONTEPARS_H" << endl;
  oMontePars << "#define MONTEPARS_H" << endl;
  oMontePars << endl;

  oMontePars << "#include <spk/SpkValarray.h>" << endl;
  oMontePars << endl;

  oMontePars << "namespace MontePars{" << endl;
  oMontePars << "   enum METHOD { adapt, grid, plain, miser, monte, vegas };" << endl;
  oMontePars << "   const enum METHOD method = ";
  if( myIntegMethod == GRID )
    oMontePars << "grid;" << endl;
  else if( myIntegMethod == MISER )
    oMontePars << "miser;" << endl;
  else if( myIntegMethod == ADAPT )
    oMontePars << "adapt;" << endl;
  else if( myIntegMethod == VEGAS )
    oMontePars << "vegas;" << endl;
  else //if( myIntegMethod == PLAIN )
    oMontePars << "plain;" << endl;

  oMontePars << "   const int nEval = " << myIntegNEvals << ";" << endl;
  oMontePars << "   const int c_numberEval[ nEval ] = { ";
  for( int i=0; i<myIntegNEvals; i++ )
    {
      if( i > 0 )
        oMontePars << ", ";
      oMontePars << myIntegNumberEvals[i];
    }
  oMontePars << " };" << endl;
  oMontePars << "   const SPK_VA::valarray<int> numberEval( c_numberEval, nEval );" << endl;
  oMontePars << "};" << endl;

  oMontePars << endl;

  oMontePars << "#endif" << endl;
}
