/*****************************************************************************
 * The following structures represent a graph of the bytecode generated      *
 * by f2java.  Creating a graph allows making an accurate calculation of     *
 * the maximum stack size (taking branches into account) and allows easy     *
 * calculation of goto branch target offsets.                                *
 *****************************************************************************/

#ifndef _GRAPH_H
#define _GRAPH_H

#include "codegen.h"

typedef struct _code_graph_node {
  enum _opcode op;          /* the opcode for this instruction               */
  u4 pc;                    /* the address in bytecode of this instruction   */
  u4 operand;               /* this opcode's operand (may be u1, u2, u4)     */

  struct _code_graph_node
     * branch_target,       /* the node to which we might optionally branch  *
                             * (comparison ops) or unconditionally branch    */

     * next,                /* next op in code, but not necessarily next to  *
                             * execute since we may branch over it.          */

     ** optional_targets;   /* list of branch targets for instructions which *
                             * have multiple targets (tableswitch).          */

  u4 branch_label;          /* f77 label to which this instruction branches  */
  u4 stack_depth;           /* stack depth prior to execution of this opcode */
  BOOLEAN visited;          /* for traversal - has this node been visited?   */
} CodeGraphNode;

typedef struct _exception_table_entry {
  CodeGraphNode 
     * from,                /* PC at which the try block begins              */
     * to,                  /* PC at which the try block ends                */
     * target;              /* PC at which the exception handler begins      */
} ExceptionTableEntry;

#endif
