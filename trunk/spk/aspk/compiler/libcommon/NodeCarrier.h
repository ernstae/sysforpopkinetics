#ifndef NODECARRIER_H
#define NODECARRIER_H
#include <xercesc/dom/DOM.hpp>

/**
 * @file NodeCarrier.h
 *
 * Declares struct NodeCarrier
 */
/**
 * Data structure that serves as a carrier of node during the process of building a parse tree.
 *
 * The NodeCarrier data structure is a dummy strcuture that wrapps a true node, in a sense, which
 * is xerces::DOMElement written in C++ so that the object can be carried within C code. 
 * 
 * @note This data structure was need to fool FLEX (ie. LEX) generated C lexical analyzer.
 * FLEX can generate C++ code but in reality it fails to recoginize
 * C++ objects specified in BISON (ie. YACC) specification files.
 */
struct NodeCarrier{

  /*
   *@class This is the node to carry around.
   */
   xercesc::DOMElement * node;
};

#endif
