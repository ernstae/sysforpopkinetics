#include <vector>
#include <xercesc/dom/DOM.hpp>

#include "ExpNodeCarrier.h"

static std::vector<ExpNodeCarrier *> nodeList;

struct ExpNodeCarrier * createExpNodeCarrier()
{
  struct ExpNodeCarrier * n = new ExpNodeCarrier;
  nodeList.push_back( n );
  return n;
}

int releaseExpNodeCarriers()
{
  int n = nodeList.size();
  for( int i=0; i<n; i++ )
  {
    delete nodeList[i];
  }

  return n;
}

#ifndef NDEFINE
int numExpNodeCarriers()
{
  return nodeList.size();
}
#endif
