#ifndef EXPNODECARRIER_H
#define EXPNODECARRIER_H
#include <xercesc/dom/DOM.hpp>

/**
 * @file ExpNodeCarrier.h
 *
 * Declares struct ExpNodeCarrier
 */
/**
 * Data structure that serves as a carrier of node during the process of building a parse tree.
 *
 * The ExpNodeCarrier data structure is a dummy strcuture that wrapps a true node, in a sense, which
 * is xerces::DOMElement written in C++ so that the object can be carried within C code. 
 * 
 * @note This data structure was need to fool FLEX (ie. LEX) generated C lexical analyzer.
 * FLEX can generate C++ code but in reality it fails to recoginize
 * C++ objects specified in BISON (ie. YACC) specification files.
 */
struct ExpNodeCarrier{

  /*
   *@class This is the node to carry around.
   */
   xercesc::DOMElement * node;
};

/*
struct ExpNodeCarrier * createExpNodeCarrier();
int releaseExpNodeCarriers();

   #ifndef NDEFINE
int numExpNodeCarriers();
   #endif
*/
#endif
