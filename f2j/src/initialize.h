/*
 * $Source$
 * $Revision$
 * $Date$
 * $Author$
 */


#ifndef _INITIALIZE_H
#define _INITIALIZE_H

/*****************************************************************************
 * initialize.h                                                              *
 *                                                                           *
 * Header file containing initialization of f2java's translation tables.     *
 * See globals.c for more detailed descriptions of each table.               *
 *                                                                           *
 *****************************************************************************/


#include<stdio.h>
#include<string.h>
#include"f2j.h"
#include"f2jparse.tab.h" 

extern KWDTAB tab_stmt[];                  /* statement starting keywords    */

extern KWDTAB tab_type[];                  /* TYPE tokens                    */

extern KWDTAB tab_toks[];                  /* misc tokens                    */

extern KWDTAB read_toks[];                 /* tokens found in read stmts     */

extern KWDTAB open_toks[];                 /* tokens found in open stmts     */

extern KWDTAB assign_toks[];               /* tokens found in ASSIGN stmts   */

extern METHODTAB intrinsic_toks[];         /* fortran intrinsic functions    */

extern char *generic_intrinsics[];         /* table of 'generic' intrinsics  */

extern METHODTAB jasmin_intrinsic_toks[];  /* mapping intrinsics to jasmin   */

extern char *java_reserved_words[];        /* list of Java reserved words    */

extern char *jasmin_reserved_words[];      /* list of Jasmin reserved words  */

extern char *blas_routines[];              /* list of the routines in BLAS   */

extern enum returntype default_implicit_table[];  /* letters -> data types   */

#endif
