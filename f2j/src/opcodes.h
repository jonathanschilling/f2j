/*
 * $Source$
 * $Revision$
 * $Date$
 * $Author$
 */

/*****************************************************************************
 * opcodes.h                                                                 *
 *                                                                           *
 * Definitions of opcodes related to code generation.                        *
 *                                                                           *
 *****************************************************************************/

#ifndef _OPCODES_H
#define _OPCODES_H

/*****************************************************************************
 * MAX_RETURNS is the number of data types.                                  *
 * OBJECT_TYPE identifies the type 'Object'.                                 *
 *****************************************************************************/

#define OBJECT_TYPE 7
#define MAX_RETURNS 7

#define JSTR     "Ljava/lang/String;"
#define JSTR_ARR "[Ljava/lang/String;"
#define JOBJ     "Ljava/lang/Object;"
#define JOBJ_ARR "[Ljava/lang/Object;"

/* data types for f2java primitives: */
extern char *returnstring[MAX_RETURNS+1];

/* Mapping between f2java data types and array data types.. */
extern u2 jvm_array_type[MAX_RETURNS+1];

/* descriptors for the valueOf() method for the various wrapper classes.     */
extern char * wrapper_valueOf_descriptor[MAX_RETURNS+1];

/* descriptors for java/lang/String's valueOf() methods                      */
extern char * string_valueOf_descriptor[MAX_RETURNS+1];

/* descriptors for the StringBuffer.append() methods                      */
extern char * append_descriptor[MAX_RETURNS+1];

/* descriptors for the numeric wrapper classes' toString() methods           */
extern char * toString_descriptor[MAX_RETURNS+1];

/* descriptors of PrintStream's print() and println() methods */
extern char * println_descriptor[MAX_RETURNS+1];

/* table of numericValue methods (e.g. doubleValue(), intValue(), etc. */
extern char * numericValue_method[MAX_RETURNS+1];

/* method descriptors corresponding to the above methods.                    */
extern char * numericValue_descriptor[MAX_RETURNS+1];
extern char *field_descriptor[MAX_RETURNS+1][2];
extern char *wrapped_field_descriptor[MAX_RETURNS+1][2];

/* types for scalars passed by reference:    */
extern char *wrapper_returns[MAX_RETURNS+1];

/* fully qualified wrapper names:   */
extern char *full_wrappername[MAX_RETURNS+1];

/* descriptors of the wrappers' .val fields   */
extern char *val_descriptor[MAX_RETURNS+1];

/* descriptors for the wrapper classes' constructors:         */
extern char *wrapper_descriptor[MAX_RETURNS+1];

/* names of the standard Java wrappers:  */
extern char *java_wrapper[MAX_RETURNS+1];

/* opcodes to push initial primitive values:   */
extern enum _opcode init_opcodes[MAX_RETURNS+1];

/* opcodes to return a value from a function:  */
extern enum _opcode return_opcodes[MAX_RETURNS+1];

/* initial values for above data types:  */
extern char *init_vals[MAX_RETURNS+1];

/* descriptors for EasyIn's read methods */
extern char *input_descriptors[MAX_RETURNS+1];

/* input functions to read various data types:   */
extern char *input_func[MAX_RETURNS+1];

/* input functions that detect EOF:    */
extern char *input_func_eof[MAX_RETURNS+1];

/* addition opcodes, indexed by vartype:   */
extern enum _opcode add_opcode[MAX_RETURNS+1];

/* subtraction opcodes, indexed by vartype:  */
extern enum _opcode sub_opcode[MAX_RETURNS+1];

/* division opcodes, indexed by vartype:   */
extern enum _opcode div_opcode[MAX_RETURNS+1];

/* multiplication opcodes, indexed by vartype:   */
extern enum _opcode mul_opcode[MAX_RETURNS+1];

/* negation opcodes, indexed by vartype:    */
extern enum _opcode neg_opcode[MAX_RETURNS+1];

/* integer comparison opcodes, indexed by vartype.        */
extern enum _opcode icmp_opcode[];

/* comparison ops for relational expressions.  */
extern enum _opcode dcmp_opcode[];

/* The following is a table of type conversion opcodes. */
extern enum _opcode typeconv_matrix[MAX_RETURNS+1][MAX_RETURNS+1];

/* mapping of f2j data types to jvm data types. */
extern enum jvm_data_type jvm_data_types[MAX_RETURNS+1];

/* table of Java's wrapper classes.  we only expect to use the numeric ones  */
extern char * numeric_wrapper[MAX_RETURNS+1];

#endif
