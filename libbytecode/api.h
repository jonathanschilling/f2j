#ifndef _API_H
#define _API_H

#include<stdio.h>
#include<stdlib.h>
#include<stdarg.h>
#include<string.h>
#include<ctype.h>
#include<sys/stat.h>
#include<errno.h>

#ifdef _WIN32
#include<dir.h>
#else
#include<unistd.h>
#endif

#ifdef _WIN32
#define BC_FILE_DELIM "\\"
#else
#define BC_FILE_DELIM "/"
#endif

#include "bytecode.h"

static JVM_ATTRIBUTE
  *find_attribute(JVM_CLASS *, Dlist, char *),
  *new_code_attr(JVM_CLASS *);

static JVM_CODE_GRAPH_NODE
  *bytecode0(JVM_METHOD *, JVM_OPCODE),
  *bytecode1(JVM_METHOD *, JVM_OPCODE, u4);

static void
  updateMaxLocals(JVM_METHOD *, unsigned int, JVM_DATA_TYPE);

static int
  num_locals_in_descriptor(char *);

static char
  *char_substitute(char *, int, int);

#endif
