#ifndef MYNODE_H
#define MYNODE_H
#include <xercesc/dom/DOM.hpp>

/**
 * Data structure that serves as the carrier of nodes in parse tree.
 *
 * The Node data structure is a dummy strcuture that wrapps a true node, in a sense, which
 * is xerces::DOMElement written in C++.  
 * These nodes are created and attached to a parse tree during
 * syntax analysis.  
 * 
 * \note This data structure was need to fool FLEX (ie. LEX) generated C lexical analyzer.
 * FLEX can generate C++ code but in reality it fails to recoginize
 * C++ objects specified in BISON (ie. YACC) specification files.
 */
struct Node{
    xercesc::DOMElement * dom;
};

/**
 * Allocates memory for a Node object and returns a point to it.
 *
 * This function ensures the allocated memory persist until releaseNode() is called.
 * \return a pointer to a Node object.
 */
struct Node * createNode();

/**
 * Releases the resources allocated for Node objects created through the createNode() function.
 *
 * \return the number of nodes whose memory have been released.
 */
int releaseNodes();

#ifndef NDEBUG
/**
 * Reports the number of Node objects that have been created (debug use).
 *
 * \return the number of nodes that have been created.
 */
int numNodes();
#endif

#endif
