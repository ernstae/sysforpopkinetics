#ifndef EMIT_INDDATACLASS_H
#define EMIT_INDDATACLASS_H

#include <string>
#include <map>
#include <vector>
#include <spk/SpkValarray.h>

/**
 * @file emit_IndData.h
 *
 * Declare emit_IndData().
 * 
 */
/**
 * emit_IndData() generates C++ source code for a class structure, IndData,
 * that organizes a set of measurements and other information 
 * associated with a particular subject and another class, IndDataSet,
 * structure to organizes a group of such.
 *
 * A data set (observation) associated with a subject, @a foo, and
 * refered to as @a X and also @a Y shall be publicly accessible 
 * as member variables of IndData object.  The i-th subject of such
 * an IndData object shall be accessbile as the i-th element of
 * IndDataSet object via operator[](int i).  An IndDataSet shall
 * be able to be created though the default constructor.
 *
 * @return void
 *
 * @param header A FILE handler for an open writable file to which
 * the declarations of dynamically generated @a IndData and
 * @a IndDataSet classes shall be appended.
 * 
 * @param cpp A FILE hander for an open writable file to which
 * the definitions of @a IndData and @a IndDataSet classes shall be
 * appended.
 *
 * @param label_alias A map between two labels
 * that refer to an identical set of data (observation).  
 * The key element is considered as a primary label for the data set and
 * thus must not be empty.  The entry is considered as an alias/synonym
 * and may be an empty string.  The keys must be unique.
 *
 * @param data_for An ordered list (vector) of maps, where i-th map 
 * in the list is associated with the subject who will be
 * processed i-th during optimization.  Each map associates
 * a label and an array of double-precision values.  A label, key,
 * must be found in @a label_alias as either a key or an entry.
 * The labels, keys, must be unique.
 *
 * @param IDs An order list (array) of strings associated with subjects.
 * The i-th ID in the list corresponds to the subject who will
 * be processed i-th during optimization.
 * 
 */
void emit_IndData( FILE * header, FILE * cpp,
		   const std::map<std::string, std::string> & label_alias,
		   const std::vector< std::map<std::string, SPK_VA::valarray<double> > > & data_for,
		   const std::string IDs[] );
/**
 * @example emit_IndDataTest.cpp
 */
#endif
