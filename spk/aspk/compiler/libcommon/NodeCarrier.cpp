#include <vector>
#include "NodeCarrier.h"
#include <xercesc/dom/DOM.hpp>

static std::vector<NodeCarrier *> nodeList;

struct NodeCarrier * createNodeCarrier()
{
  struct NodeCarrier * n = new NodeCarrier;
  nodeList.push_back( n );
  return n;
}

int releaseNodeCarriers()
{
  int n = nodeList.size();
  for( int i=0; i<n; i++ )
  {
    delete nodeList[i];
  }

  return n;
}

#ifndef NDEFINE
int numNodeCarriers()
{
  return nodeList.size();
}
#endif
