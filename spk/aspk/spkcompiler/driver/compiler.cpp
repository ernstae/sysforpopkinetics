/** 
 @file compiler.cpp
 * --- UNDER CONSTRUCTION!!! ---
 * This is supposed to be the SPK Compliler driver that
 * makes an input SpkML document translated to C++ source
 * code and builds an executable for the target machine.
 */
#include <iostream>
#include "../libnonmem/NonmemCompiler.h"
using namespace std;

/**
 * The global counter for errors encountered during expression parsing.
 */
int errors = 0;

/**
 * Usage
 */
void usage()
{
  cout << "Usage: compiler XML [-print]" << endl;
  cout << endl;
  cout << "   XML    --- input XML document file name" << endl;
  cout << "   -print --- request output to the standard output " << endl;
  return;
}

int main( int argc, char * argv[] )
{
  if (argc < 2)
  {
    usage();
    return 1;
  }  
  const char * gXmlFile = argv[1];
  bool isPrint = false;
  if( argc == 3 && strcmp( argv[2], "-print") == 0 )
    {
      isPrint = true;
    }


  NonmemCompiler compiler( gXmlFile );

  compiler.parse();
  compiler.interpret();
  compiler.emit();
  
  if( isPrint )
    compiler.printTree();
  
  return 0;
}
