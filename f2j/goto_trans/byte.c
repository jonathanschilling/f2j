

/* *************
   *** JAVAB ***
   ****************************************************
   *** Copyright (c) 1997                           ***
   *** Aart J.C. Bik             Indiana University *** 
   *** All Rights Reserved                          ***
   ****************************************************
   *** Please refer to the LICENSE file distributed ***
   *** with this software for further details on    ***
   *** the licensing terms and conditions.          ***
   ***                                              ***
   *** Please, report all bugs, comments, etc.      ***
   *** to: ajcbik@extreme.indiana.edu               ***
   ****************************************************
   *** byte.c : bytecode manipulations
   ***
   ***
   *** Your courtesy in mentioning the use of this bytecode tool
   *** in any scientific work that presents results obtained
   *** by using (extensions or modifications of) the tool
   *** is highly appreciated.
   ***
   *** */

/* ********************************************************
   *** INCLUDE FILES and DEFINITIONS                    ***
   ********************************************************  */

#include "class.h"

#define CHECK_TABLE
#define GET_IT(a,b) if (valid_cp_entry((a), entry, (b))) {          \
                      n = constant_pool[entry] -> u.indices.index2; \
                      d = constant_pool[n]     -> u.indices.index2; \
                      s = constant_pool[d]     -> u.utf8.s;         \
                    }                                               \
		    else break; 
#define HAS_TARGET(b) (((b)>=1u)&&((b)<=3u))

/* ********************************************************
   *** EXTERNAL VARIABLES                               ***
   ******************************************************** */

/* global information 
   ****************** */

static attribute_ptr att;

static u4_int        len;
static u1_int       *byt, opc, bra, exc;
static u2_int        pre, pos;
static char         *mem;

static u1_int        is_wide;
static u1_int        is_instm;

static u4_int        target, next;

static u2_int        glo_sta, glo_pad, glo_loc, glo_stm;
static s4_int        glo_def, glo_npa, glo_low, glo_hig;

static u2_int        cur_sp;

#ifndef TRANS_DEBUG
#define TRANS_DEBUG 0
#endif

static int trdebug = TRANS_DEBUG;

/* reaching definitions and uses
   ***************************** */

static char          rd_buf[510];
static char         *rd_sig[255];     /* fixed arrays */

/* bytecode table
   ************** */

static struct bytecode_node {

   u1_int opcode;     /* redundant verify field: 
			 bytecode[i].opcode == i 
			 *********************** */
   char  *mnemonic;

   u1_int operands;   /* 9 == lookup */
   u1_int stack_pre;  /* 9 == lookup */
   u1_int stack_post; /* 9 == lookup */
		      /* *********** */ 

   u1_int exception;  /* 0: no exception 
			 1: pot. RUN-TIME exception 
			 2: pot. RUN-TIME exception + has c.p.-entry
			 3: pot. LINKING exception 
			 4: pot. pot. LINKING exception + has c.p.-entry
			 *********************************************** */

   u1_int branch;     /* 0: no branch,   
			 ---------------------------
			 1: cond.   branch + target, 
			 2: uncond. branch + target, 
			 3: jsr/jsr_w      + target,
			 ---------------------------
			 4: special        + continue next
			 5: special        + no-continue next
			 ************************************ */
} bytecode[] = {

/*  ***--------------------------------------------> opcode
         **************----------------------------> mnemonic
                          **-----------------------> #operands      (in bytes)
                              **-------------------> stack     pre  (in words)
                                  **---------------> stack     post (in words)
                                      **-----------> exception
                                          **-------> branch     */
/*  ***  **************   **  **  **  **  **  */

  {   0, "nop",            0,  0,  0,  0,  0  },

/*  ***  **************   **  **  **  **  **  */

  {   1, "aconst_null",    0,  0,  1,  0,  0  },

/*  ***  **************   **  **  **  **  **  */

  {   2, "iconst_m1",      0,  0,  1,  0,  0  },
  {   3, "iconst_0",       0,  0,  1,  0,  0  },
  {   4, "iconst_1",       0,  0,  1,  0,  0  },
  {   5, "iconst_2",       0,  0,  1,  0,  0  },
  {   6, "iconst_3",       0,  0,  1,  0,  0  },
  {   7, "iconst_4",       0,  0,  1,  0,  0  },
  {   8, "iconst_5",       0,  0,  1,  0,  0  },

/*  ***  **************   **  **  **  **  **  */

  {   9, "lconst_0",       0,  0,  2,  0,  0  },
  {  10, "lconst_1",       0,  0,  2,  0,  0  },

/*  ***  **************   **  **  **  **  **  */

  {  11, "fconst_0",       0,  0,  1,  0,  0  },
  {  12, "fconst_1",       0,  0,  1,  0,  0  },
  {  13, "fconst_2",       0,  0,  1,  0,  0  },

/*  ***  **************   **  **  **  **  **  */

  {  14, "dconst_0",       0,  0,  2,  0,  0  },
  {  15, "dconst_1",       0,  0,  2,  0,  0  },

/*  ***  **************   **  **  **  **  **  */

  {  16, "bipush",         1,  0,  1,  0,  0  },
  {  17, "sipush",         2,  0,  1,  0,  0  },

/*  ***  **************   **  **  **  **  **  */

  {  18, "ldc",            1,  0,  1,  4,  0  },
  {  19, "ldc_w",          2,  0,  1,  4,  0  },
  {  20, "ldc2_w",         2,  0,  2,  0,  0  },

/*  ***  **************   **  **  **  **  **  */

  {  21, "iload",          1,  0,  1,  0,  0  },
  {  22, "lload",          1,  0,  2,  0,  0  },
  {  23, "fload",          1,  0,  1,  0,  0  },
  {  24, "dload",          1,  0,  2,  0,  0  },
  {  25, "aload",          1,  0,  1,  0,  0  },

/*  ***  **************   **  **  **  **  **  */

  {  26, "iload_0",        0,  0,  1,  0,  0  },
  {  27, "iload_1",        0,  0,  1,  0,  0  },
  {  28, "iload_2",        0,  0,  1,  0,  0  },
  {  29, "iload_3",        0,  0,  1,  0,  0  },

/*  ***  **************   **  **  **  **  **  */

  {  30, "lload_0",        0,  0,  2,  0,  0  },
  {  31, "lload_1",        0,  0,  2,  0,  0  },
  {  32, "lload_2",        0,  0,  2,  0,  0  },
  {  33, "lload_3",        0,  0,  2,  0,  0  },

/*  ***  **************   **  **  **  **  **  */

  {  34, "fload_0",        0,  0,  1,  0,  0  },
  {  35, "fload_1",        0,  0,  1,  0,  0  },
  {  36, "fload_2",        0,  0,  1,  0,  0  },
  {  37, "fload_3",        0,  0,  1,  0,  0  },

/*  ***  **************   **  **  **  **  **  */

  {  38, "dload_0",        0,  0,  2,  0,  0  },
  {  39, "dload_1",        0,  0,  2,  0,  0  },
  {  40, "dload_2",        0,  0,  2,  0,  0  },
  {  41, "dload_3",        0,  0,  2,  0,  0  },

/*  ***  **************   **  **  **  **  **  */

  {  42, "aload_0",        0,  0,  1,  0,  0  },
  {  43, "aload_1",        0,  0,  1,  0,  0  },
  {  44, "aload_2",        0,  0,  1,  0,  0  },
  {  45, "aload_3",        0,  0,  1,  0,  0  },

/*  ***  **************   **  **  **  **  **  */

  {  46, "iaload",         0,  2,  1,  1,  0  },
  {  47, "laload",         0,  2,  2,  1,  0  },
  {  48, "faload",         0,  2,  1,  1,  0  },
  {  49, "daload",         0,  2,  2,  1,  0  },
  {  50, "aaload",         0,  2,  1,  1,  0  },

/*  ***  **************   **  **  **  **  **  */

  {  51, "baload",         0,  2,  1,  1,  0  },
  {  52, "caload",         0,  2,  1,  1,  0  },
  {  53, "saload",         0,  2,  1,  1,  0  },

/*  ***  **************   **  **  **  **  **  */

  {  54, "istore",         1,  1,  0,  0,  0  },
  {  55, "lstore",         1,  2,  0,  0,  0  },
  {  56, "fstore",         1,  1,  0,  0,  0  },
  {  57, "dstore",         1,  2,  0,  0,  0  },
  {  58, "astore",         1,  1,  0,  0,  0  },

/*  ***  **************   **  **  **  **  **  */

  {  59, "istore_0",       0,  1,  0,  0,  0  },
  {  60, "istore_1",       0,  1,  0,  0,  0  },
  {  61, "istore_2",       0,  1,  0,  0,  0  },
  {  62, "istore_3",       0,  1,  0,  0,  0  },

/*  ***  **************   **  **  **  **  **  */

  {  63, "lstore_0",       0,  2,  0,  0,  0  },
  {  64, "lstore_1",       0,  2,  0,  0,  0  },
  {  65, "lstore_2",       0,  2,  0,  0,  0  },
  {  66, "lstore_3",       0,  2,  0,  0,  0  },

/*  ***  **************   **  **  **  **  **  */

  {  67, "fstore_0",       0,  1,  0,  0,  0  },
  {  68, "fstore_1",       0,  1,  0,  0,  0  },
  {  69, "fstore_2",       0,  1,  0,  0,  0  },
  {  70, "fstore_3",       0,  1,  0,  0,  0  },

/*  ***  **************   **  **  **  **  **  */

  {  71, "dstore_0",       0,  2,  0,  0,  0  },
  {  72, "dstore_1",       0,  2,  0,  0,  0  },
  {  73, "dstore_2",       0,  2,  0,  0,  0  },
  {  74, "dstore_3",       0,  2,  0,  0,  0  },

/*  ***  **************   **  **  **  **  **  */

  {  75, "astore_0",       0,  1,  0,  0,  0  },
  {  76, "astore_1",       0,  1,  0,  0,  0  },
  {  77, "astore_2",       0,  1,  0,  0,  0  },
  {  78, "astore_3",       0,  1,  0,  0,  0  },

/*  ***  **************   **  **  **  **  **  */

  {  79, "iastore",        0,  3,  0,  1,  0  },
  {  80, "lastore",        0,  4,  0,  1,  0  },
  {  81, "fastore",        0,  3,  0,  1,  0  },
  {  82, "dastore",        0,  4,  0,  1,  0  },

/*  ***  **************   **  **  **  **  **  */

  {  83, "aastore",        0,  3,  0,  1,  0  },
  {  84, "bastore",        0,  3,  0,  1,  0  },
  {  85, "castore",        0,  3,  0,  1,  0  },
  {  86, "sastore",        0,  3,  0,  1,  0  },

/*  ***  **************   **  **  **  **  **  */

  {  87, "pop",            0,  1,  0,  0,  0  },
  {  88, "pop2",           0,  2,  0,  0,  0  },

/*  ***  **************   **  **  **  **  **  */

  {  89, "dup",            0,  1,  2,  0,  0  },
  {  90, "dup_x1",         0,  2,  3,  0,  0  },
  {  91, "dup_x2",         0,  3,  4,  0,  0  },
  {  92, "dup2",           0,  2,  4,  0,  0  },
  {  93, "dup2_x1",        0,  3,  5,  0,  0  },
  {  94, "dup2_x2",        0,  4,  6,  0,  0  },

/*  ***  **************   **  **  **  **  **  */

  {  95, "swap",           0,  2,  2,  0,  0  },

/*  ***  **************   **  **  **  **  **  */

  {  96, "iadd",           0,  2,  1,  0,  0  },
  {  97, "ladd",           0,  4,  2,  0,  0  },
  {  98, "fadd",           0,  2,  1,  0,  0  },
  {  99, "dadd",           0,  4,  2,  0,  0  },

/*  ***  **************   **  **  **  **  **  */

  { 100, "isub",           0,  2,  1,  0,  0  },
  { 101, "lsub",           0,  4,  2,  0,  0  },
  { 102, "fsub",           0,  2,  1,  0,  0  },
  { 103, "dsub",           0,  4,  2,  0,  0  },

/*  ***  **************   **  **  **  **  **  */

  { 104, "imul",           0,  2,  1,  0,  0  },
  { 105, "lmul",           0,  4,  2,  0,  0  },
  { 106, "fmul",           0,  2,  1,  0,  0  },
  { 107, "dmul",           0,  4,  2,  0,  0  },

/*  ***  **************   **  **  **  **  **  */

  { 108, "idiv",           0,  2,  1,  1,  0  },
  { 109, "ldiv",           0,  4,  2,  1,  0  },
  { 110, "fdiv",           0,  2,  1,  0,  0  },
  { 111, "ddiv",           0,  4,  2,  0,  0  },

/*  ***  **************   **  **  **  **  **  */

  { 112, "irem",           0,  2,  1,  1,  0  },
  { 113, "lrem",           0,  4,  2,  1,  0  },
  { 114, "frem",           0,  2,  1,  0,  0  },
  { 115, "drem",           0,  4,  2,  0,  0  },

/*  ***  **************   **  **  **  **  **  */

  { 116, "ineg",           0,  1,  1,  0,  0  },
  { 117, "lneg",           0,  2,  2,  0,  0  },
  { 118, "fneg",           0,  1,  1,  0,  0  },
  { 119, "dneg",           0,  2,  2,  0,  0  },

/*  ***  **************   **  **  **  **  **  */

  { 120, "ishl",           0,  2,  1,  0,  0  },
  { 121, "lshl",           0,  3,  2,  0,  0  },
  { 122, "ishr",           0,  2,  1,  0,  0  },
  { 123, "lshr",           0,  3,  2,  0,  0  },
  { 124, "iushr",          0,  2,  1,  0,  0  },
  { 125, "lushr",          0,  3,  2,  0,  0  },

/*  ***  **************   **  **  **  **  **  */

  { 126, "iand",           0,  2,  1,  0,  0  },
  { 127, "land",           0,  4,  2,  0,  0  },

/*  ***  **************   **  **  **  **  **  */

  { 128, "ior",            0,  2,  1,  0,  0  },
  { 129, "lor",            0,  4,  2,  0,  0  },
  { 130, "ixor",           0,  2,  1,  0,  0  },
  { 131, "lxor",           0,  4,  2,  0,  0  },

/*  ***  **************   **  **  **  **  **  */

  { 132, "iinc",           2,  0,  0,  0,  0  },

/*  ***  **************   **  **  **  **  **  */

  { 133, "i2l",            0,  1,  2,  0,  0  },
  { 134, "i2f",            0,  1,  1,  0,  0  },
  { 135, "i2d",            0,  1,  2,  0,  0  },

/*  ***  **************   **  **  **  **  **  */

  { 136, "l2i",            0,  2,  1,  0,  0  },
  { 137, "l2f",            0,  2,  1,  0,  0  },
  { 138, "l2d",            0,  2,  2,  0,  0  },

/*  ***  **************   **  **  **  **  **  */

  { 139, "f2i",            0,  1,  1,  0,  0  },
  { 140, "f2l",            0,  1,  2,  0,  0  },
  { 141, "f2d",            0,  1,  2,  0,  0  },

/*  ***  **************   **  **  **  **  **  */

  { 142, "d2i",            0,  2,  1,  0,  0  },
  { 143, "d2l",            0,  2,  2,  0,  0  },
  { 144, "d2f",            0,  2,  1,  0,  0  },

/*  ***  **************   **  **  **  **  **  */

  { 145, "i2b",            0,  1,  1,  0,  0  },
  { 146, "i2c",            0,  1,  1,  0,  0  },
  { 147, "i2s",            0,  1,  1,  0,  0  },

/*  ***  **************   **  **  **  **  **  */

  { 148, "lcmp",           0,  4,  1,  0,  0  },
  { 149, "fcmpl",          0,  2,  1,  0,  0  },
  { 150, "fcmpg",          0,  2,  1,  0,  0  },
  { 151, "dcmpl",          0,  4,  1,  0,  0  },
  { 152, "dcmpg",          0,  4,  1,  0,  0  },

/*  ***  **************   **  **  **  **  **  */

  { 153, "ifeq",           2,  1,  0,  0,  1  },
  { 154, "ifne",           2,  1,  0,  0,  1  },
  { 155, "iflt",           2,  1,  0,  0,  1  },
  { 156, "ifge",           2,  1,  0,  0,  1  },
  { 157, "ifgt",           2,  1,  0,  0,  1  },
  { 158, "ifle",           2,  1,  0,  0,  1  },

/*  ***  **************   **  **  **  **  **  */

  { 159, "if_icmpeq",      2,  2,  0,  0,  1  },
  { 160, "if_icmpne",      2,  2,  0,  0,  1  },
  { 161, "if_icmplt",      2,  2,  0,  0,  1  },
  { 162, "if_icmpge",      2,  2,  0,  0,  1  },
  { 163, "if_icmpgt",      2,  2,  0,  0,  1  },
  { 164, "if_icmple",      2,  2,  0,  0,  1  },
  { 165, "if_acmpeq",      2,  2,  0,  0,  1  },
  { 166, "if_acmpne",      2,  2,  0,  0,  1  },

/*  ***  **************   **  **  **  **  **  */

  { 167, "goto",           2,  0,  0,  0,  2  },
  { 168, "jsr",            2,  0,  1,  0,  3  },
  { 169, "ret",            1,  0,  0,  0,  5  },

/*  ***  **************   **  **  **  **  **  */

  { 170, "tableswitch",    9,  1,  0,  0,  5  },
  { 171, "lookupswitch",   9,  1,  0,  0,  5  },

/*  ***  **************   **  **  **  **  **  */

  { 172, "ireturn",        0,  1,  0,  0,  5  },
  { 173, "lreturn",        0,  2,  0,  0,  5  },
  { 174, "freturn",        0,  1,  0,  0,  5  },
  { 175, "dreturn",        0,  2,  0,  0,  5  },
  { 176, "areturn",        0,  1,  0,  0,  5  },
  { 177, "return",         0,  0,  0,  0,  5  },

/*  ***  **************   **  **  **  **  **  */

  { 178, "getstatic",      2,  0,  9,  4,  0  },
  { 179, "putstatic",      2,  9,  0,  4,  0  },

/*  ***  **************   **  **  **  **  **  */

  { 180, "getfield",       2,  1,  9,  2,  0  },
  { 181, "putfield",       2,  9,  0,  2,  0  },

/*  ***  **************   **  **  **  **  **  */

  { 182, "invokevirtual",  2,  9,  9,  2,  4  },
  { 183, "invokespecial",  2,  9,  9,  2,  4  },
  { 184, "invokestatic",   2,  9,  9,  4,  4  },
  { 185, "invokeinterface",4,  9,  9,  2,  4  },

/*  ***  **************   **  **  **  **  **  */

  { 186, "xxxunusedxxx",   0,  0,  0,  0,  0  },

/*  ***  **************   **  **  **  **  **  */

  { 187, "new",            2,  0,  1,  4,  0  },
  { 188, "newarray",       1,  1,  1,  1,  0  },
  { 189, "anewarray",      2,  1,  1,  2,  0  },

/*  ***  **************   **  **  **  **  **  */

  { 190, "arraylength",    0,  1,  1,  1,  0   },
  { 191, "athrow",         0,  1,  0,  1,  5   },

/*  ***  **************   **  **  **  **  **  */

  { 192, "checkcast",      2,  1,  1,  2,  0   },
  { 193, "instanceof",     2,  1,  1,  4,  0   },

/*  ***  **************   **  **  **  **  **  */

  { 194, "monitorenter",   0,  1,  0,  1,  0   },
  { 195, "monitorexit",    0,  1,  0,  1,  0   },

/*  ***  **************   **  **  **  **  **  */

  { 196, "wide",           0,  0,  0,  0,  0  },

/*  ***  **************   **  **  **  **  **  */

  { 197, "multianewarray", 3,  9,  1,  2,  0  },

/*  ***  **************   **  **  **  **  **  */

  { 198, "ifnull",         2,  1,  0,  0,  1  },
  { 199, "ifnonnull",      2,  1,  0,  0,  1  },

/*  ***  **************   **  **  **  **  **  */

  { 200, "goto_w",         4,  0,  0,  0,  2  },
  { 201, "jsr_w",          4,  0,  1,  0,  3  },

/*  ***  **************   **  **  **  **  **  */
  
    /* reserved opcode: break */

  { 202, "???",            0,  0,  0,  0,  0  },   

/*  ***  **************   **  **  **  **  **  */

    /* _quick opcodes */

  { 203, "???",            0,  0,  0,  0,  0  },  
  { 204, "???",            0,  0,  0,  0,  0  },
  { 205, "???",            0,  0,  0,  0,  0  },
  { 206, "???",            0,  0,  0,  0,  0  },
  { 207, "???",            0,  0,  0,  0,  0  },
  { 208, "???",            0,  0,  0,  0,  0  },
  { 209, "???",            0,  0,  0,  0,  0  },
  { 210, "???",            0,  0,  0,  0,  0  },
  { 211, "???",            0,  0,  0,  0,  0  },
  { 212, "???",            0,  0,  0,  0,  0  },
  { 213, "???",            0,  0,  0,  0,  0  },
  { 214, "???",            0,  0,  0,  0,  0  },
  { 215, "???",            0,  0,  0,  0,  0  },
  { 216, "???",            0,  0,  0,  0,  0  },
  { 217, "???",            0,  0,  0,  0,  0  },
  { 218, "???",            0,  0,  0,  0,  0  },
  { 219, "???",            0,  0,  0,  0,  0  },
  { 220, "???",            0,  0,  0,  0,  0  },
  { 221, "???",            0,  0,  0,  0,  0  },
  { 222, "???",            0,  0,  0,  0,  0  },
  { 223, "???",            0,  0,  0,  0,  0  },
  { 224, "???",            0,  0,  0,  0,  0  },
  { 225, "???",            0,  0,  0,  0,  0  },
  { 226, "???",            0,  0,  0,  0,  0  },
  { 227, "???",            0,  0,  0,  0,  0  },
  { 228, "???",            0,  0,  0,  0,  0  },
 
/*  ***  **************   **  **  **  **  **  */
  
    /* unused */

  { 229, "???",            0,  0,  0,  0,  0  },  
  { 230, "???",            0,  0,  0,  0,  0  },
  { 231, "???",            0,  0,  0,  0,  0  },
  { 232, "???",            0,  0,  0,  0,  0  },
  { 233, "???",            0,  0,  0,  0,  0  },
  { 234, "???",            0,  0,  0,  0,  0  },
  { 235, "???",            0,  0,  0,  0,  0  },
  { 236, "???",            0,  0,  0,  0,  0  },
  { 237, "???",            0,  0,  0,  0,  0  },
  { 238, "???",            0,  0,  0,  0,  0  },
  { 239, "???",            0,  0,  0,  0,  0  },
  { 240, "???",            0,  0,  0,  0,  0  },
  { 241, "???",            0,  0,  0,  0,  0  },
  { 242, "???",            0,  0,  0,  0,  0  },
  { 243, "???",            0,  0,  0,  0,  0  },
  { 244, "???",            0,  0,  0,  0,  0  },
  { 245, "???",            0,  0,  0,  0,  0  },
  { 246, "???",            0,  0,  0,  0,  0  },
  { 247, "???",            0,  0,  0,  0,  0  },
  { 248, "???",            0,  0,  0,  0,  0  },
  { 249, "???",            0,  0,  0,  0,  0  },
  { 250, "???",            0,  0,  0,  0,  0  },
  { 251, "???",            0,  0,  0,  0,  0  },
  { 252, "???",            0,  0,  0,  0,  0  },
  { 253, "???",            0,  0,  0,  0,  0  },

/*  ***  **************   **  **  **  **  **  */
  
    /* reserved opcodes: impdep1 impdep1 */

  { 254, "???",            0,  0,  0,  0,  0  },  
  { 255, "???",            0,  0,  0,  0,  0  }
} ;


/* ********************************************************
   *** PRIVATE FUNCTIONS                                ***
   ******************************************************** */

/* ****************************************
   *** Processing of Method Descriptors ***
   **************************************** */

static u2_int res_width(u1_int *s) {
  u2_int p = 1u;
  u2_int r = 1u;

  if ((! s) || (s[0] != '('))
    javab_out(-1, "invalid method descriptor");

  while (s[p++] != ')') ;

  if (s[p] == 'V')
    r = 0u;
  else if ((s[p] == 'D') || (s[p] == 'J'))
    r = 2u;

  return r;
}

static u2_int arg_width(u1_int *s, u1_int set) {
  u2_int p = 1u, i;
  u2_int r = (set == 2u) ? 1u : 0u;
  u2_int b = 0u;

  if ((! s) || (s[0] != '('))
    javab_out(-1, "invalid method descriptor");

  while (s[p] != ')') {

    u2_int oldp = p; 

    if (set)
      rd_sig[r] = rd_buf + b;
    r++;

    switch (s[p]) {

      case 'D':
      case 'J':

         /* additional word */

	 if (set)
	   rd_sig[r] = NULL; 
         r++;

	 p++;
	 break;

      case 'L':

	 while (s[p++] != ';');    /* skip <classname> */
	 break;

      case '[':

	 while (s[++p] == '[');    /* skip [[[[[ */
	 if (s[p++] == 'L')     
	   while (s[p++] != ';');  /* skip <classname> */
         break;

       case 'B':
       case 'C':
       case 'F':
       case 'I':
       case 'S':
       case 'Z':

         p++;
	 break;

      default:

	javab_out(0, "invalid character %c (=%i) in method descriptor", 
		     s[p], s[p]);
	return r;
    }

    if (set) {
      for (i = oldp; i < p; i++)
        rd_buf[b++] = s[i];
      rd_buf[b++] = '\0';
    }
  }
  return r;
}

/* ********************************************************
   *** Computation of ByteContext Sensitive Information ***
   *** (next is (mis-)used as an error flag)            ***
   ******************************************************** */

static u2_int det_ops(u4_int i) {

  u4_int j, lf, lb;

  switch (byt[i]) {

     case 170u: /* tableswitch  */

       glo_pad = (u2_int) (3u - (i % 4u)); /* zero padding */

       for (j = i+1u; j <= glo_pad; j++)
	 if (byt[j]) {
	   next = 1;
	   javab_out(0, "invalid padding in 'tableswitch' at %u", j);
         }

       glo_def = B2S4(byt[i+glo_pad+1], byt[i+glo_pad+2], 
		      byt[i+glo_pad+3], byt[i+glo_pad+4]);
       glo_low = B2S4(byt[i+glo_pad+5], byt[i+glo_pad+6], 
                      byt[i+glo_pad+7], byt[i+glo_pad+8]);
       glo_hig = B2S4(byt[i+glo_pad+9], byt[i+glo_pad+10],
                      byt[i+glo_pad+11],byt[i+glo_pad+12]);

       /* Check Validity of all targets */
  
       if (((u4_int) (i+glo_def)) >= len) {
         next = 1;
	 javab_out(0, "invalid default target in 'tableswitch' at %u", i);
       }

       lf = i+glo_pad+13u; lb = glo_hig-glo_low+1u; 

        for (j = 0u; j < lb; j++, lf += 4u) {
          s4_int loc_off = B2S4(byt[lf],byt[lf+1],byt[lf+2],byt[lf+3]);

	  if (((u4_int) (i+loc_off)) >= len)  {
            next = 1u;
	    javab_out(0, "invalid target in '%s' at %u", mem, i);
	  }
       }

       /* Return number of operands (in bytes) */

       return ((u2_int) (glo_pad+16u+(glo_hig-glo_low)*4u));

     case 171u: /* lookupswitch */

       glo_pad = (u2_int) (3u - (i % 4u)); /* zero padding */

       for (j = i+1u; j <= glo_pad; j++)
	 if (byt[j]) {
	   next = 1u;
	   javab_out(0, "invalid padding in 'lookupswitch' at %u", j);
         }

       glo_def = B2S4(byt[i+glo_pad+1],byt[i+glo_pad+2],
		      byt[i+glo_pad+3],byt[i+glo_pad+4]);
       glo_npa = B2S4(byt[i+glo_pad+5],byt[i+glo_pad+6], 
  		      byt[i+glo_pad+7],byt[i+glo_pad+8]);

       /* Check Validity of all targets */

       if (((u4_int) (i + glo_def)) >= len) {
         next = 1u;
	 javab_out(0, "invalid default target in '%s' at %u", mem, i);
       }

       lf = i+glo_pad+9u; lb = glo_npa;

       for (j = 0u; j < lb; j++, lf += 8u) {
          s4_int loc_off = B2S4(byt[lf+4],byt[lf+5],byt[lf+6],byt[lf+7]);
	  if (((u4_int) (i+loc_off)) >= len)  {
            next = 1;
	    javab_out(0, "invalid target in '%s' at %u", mem, i);
	  }
       }

       /* Return number of operands (in bytes) */

       return ((u2_int) (glo_pad+8u+glo_npa*8u));

    default:
      javab_out(-1, "error in det_ops %u at %u", byt[i], i);
  }
  return 0u;  /* dummy return */
}

static u2_int det_pre(u4_int i) {

  u2_int  entry = B2U2(byt[i+1],byt[i+2]);
  u2_int  n, d;
  u1_int *s;

  switch(byt[i]) {

     case 181u: /* putfield  */

      GET_IT(CONSTANT_Fieldref, mem)

      return (u2_int) ((s[0] == 'D') || (s[0] == 'J')) ? 3u : 2u;

     case 179u: /* putstatic */

      GET_IT(CONSTANT_Fieldref, mem)

      return (u2_int) ((s[0] == 'D') || (s[0] == 'J')) ? 2u : 1u;

     case 185u: /* invokeinterface */

      GET_IT(CONSTANT_InterfaceMethodref, mem)

      if (byt[i+3u] != (1u+arg_width(s, 0u)))
	javab_out(0, "nargs differs from method descriptor at %u", i);

      return (u2_int) (byt[i+3u]);

     case 183u: /* invokespecial   */
     case 182u: /* invokevirtual   */

       GET_IT(CONSTANT_Methodref, mem)

       return (u2_int) (1u+arg_width(s, 0u));

     case 184u: /* invokestatic    */

       GET_IT(CONSTANT_Methodref, mem)

       return arg_width(s, 0u);

     case 197u: /* multianewarray */

      valid_cp_entry(CONSTANT_Class, entry, "multianewarray");

      return (u2_int) (byt[i+3]);

    default:
      javab_out(-1, "error in det_pre %u at %u", byt[i], i);
  }
  return 0u; 
}

static u2_int det_pos(u4_int i) {

  u2_int  entry = B2U2(byt[i+1],byt[i+2]);
  u2_int  n, d;
  u1_int *s;

  switch(byt[i]) {

    case 180u: /* getfield  */
    case 178u: /* getstatic */

      GET_IT(CONSTANT_Fieldref, mem)

      return (u2_int) ((s[0] == 'D') || (s[0] == 'J')) ? 2u : 1u;

     case 185u: /* invokeinterface */

      GET_IT(CONSTANT_InterfaceMethodref, mem)

      return (res_width(s));

     case 183u: /* invokespecial */
     case 184u: /* invokestatic  */
     case 182u: /* invokevirtual */

      GET_IT(CONSTANT_Methodref, mem);

      return (res_width(s));

    default:
      javab_out(-1, "error in det_pos %u at %u", byt[i], i);
  }
  return 0u; 
}

/* **********************************
   *** General bytecode traversal ***
   ********************************** */

static void byte_trav(u4_int offset) {

   u4_int i;        /* wide counter */
   u2_int ops = 0u;
   u2_int last_op = -1, prev_op = -1;
   int last_offset = 0, prev_offset = 0;
   int branch_label, idx, inst_size;
   char lbuf[100];
   int hash(char *);
   void type_insert(HASHNODE **, int, char *);
   void bzero(void *, size_t);
   void bcopy(const void *, void *, size_t);
   char *strdup(char *);

   len = att -> code_length;
   byt = &(att -> info[8u]);

   for (i = offset; i < len; i += (1u+ops)) {

     /* Determine opcode Information */

     is_wide = 0u;

back:
     opc = byt[i];               

     bra = bytecode[opc].branch;
     ops = bytecode[opc].operands;
     exc = bytecode[opc].exception;

     pre = bytecode[opc].stack_pre;
     pos = bytecode[opc].stack_post;

     mem = bytecode[opc].mnemonic;

     /* instruction 'wide'-handling
	*************************** */

     if (is_wide) {
       if (opc == 132u)                       /* wide + iinc  */
	 ops = 4u;
       else if ((21u <= opc) && (opc <= 25u)) /* wide + load  */
	 ops = 2u;
       else if ((54u <= opc) && (opc <= 58u)) /* wide + store */ 
	 ops = 2u;
       else if (opc == 169u)                  /* wide + ret   */
         ops = 2u;
       else {
	 javab_out(0, "invalid operand '%s' of 'wide' at %u", mem, i);
         return;
       }
     }

     if (HAS_TARGET(bra)) { 
     
       /* Compute target from 2-, or 4-byte offset
          **************************************** */

       s4_int off = ((opc == 200u) || (opc == 201u))
                  ? B2S4(byt[i+1],byt[i+2],byt[i+3],byt[i+4])  /* 4-bytes */
		  : B2S2(byt[i+1],byt[i+2]);                   /* 2-bytes */
       target = (u4_int) (i + off);

       if ((target >= len) && (att -> reachable[i] == 1u)) {
	 javab_out(0, "invalid target %u in '%s' at %u", target, mem, i);
	 return; 
       }
     }

     /* Determine Context Sensitive Information
	*************************************** */

     glo_pad = 0u;
     glo_def = glo_npa = glo_low = glo_hig = 0u;

     next = 0u; /* (mis-)uses as error flag */

     if (ops == 9u) 
       ops = det_ops(i); 

     if (pre == 9u) 
       pre = det_pre(i); 

     if (pos == 9u) 
       pos = det_pos(i); 

     if(!is_wide)
     {
       if( (last_op == 18u) 
             && 
           (((prev_op >= 3u) && (prev_op <= 8u)) 
                || (prev_op == 16u) || (prev_op == 17u) || (prev_op == 18u)) 
             && 
           (opc == 184u))
       {
         u2_int  e   = B2U2(byt[i+1u], byt[i+2u]);
         u2_int  c1  = constant_pool[e] -> u.indices.index1;
         u2_int  n   = constant_pool[e] -> u.indices.index2;
         u2_int  d   = constant_pool[n] -> u.indices.index1;
         u2_int  c2  = constant_pool[c1] -> u.indices.index1;
  
         char *cla   = (char *) constant_pool[c2] -> u.utf8.s;
         char *met   = (char *) constant_pool[d] -> u.utf8.s;
         char *op;

         if(( !strcmp(cla,"Dummy") || !strcmp(cla,"org/netlib/util/Dummy"))
          && !strcmp(met,"label"))
            op = "label";
         else if(( !strcmp(cla,"Dummy") || !strcmp(cla,"org/netlib/util/Dummy"))
          && !strcmp(met,"go_to"))
            op = "goto";
         else
         {
           fprintf(stderr,"Error: encountered unknown Dummy method! (%s.%s)\n",
              cla,met);
           op = "unknown";
         }

         switch(prev_op) {
           case 3u:   /* iconst_0 */
             if(trdebug) printf("%d: %s %d.\n",prev_offset, op,0);
             branch_label = 0;
             inst_size = 1;
             break;
           case 4u:   /* iconst_1 */
             if(trdebug) printf("%d: %s %d.\n",prev_offset, op,1);
             branch_label = 1;
             inst_size = 1;
             break;
           case 5u:   /* iconst_2 */
             if(trdebug) printf("%d: %s %d.\n",prev_offset, op,2);
             branch_label = 2;
             inst_size = 1;
             break;
           case 6u:   /* iconst_3 */
             if(trdebug) printf("%d: %s %d.\n",prev_offset, op,3);
             branch_label = 3;
             inst_size = 1;
             break;
           case 7u:   /* iconst_4 */
             if(trdebug) printf("%d: %s %d.\n",prev_offset, op,4);
             branch_label = 4;
             inst_size = 1;
             break;
           case 8u:   /* iconst_5 */
             if(trdebug) printf("%d: %s %d.\n",prev_offset, op,5);
             branch_label = 5;
             inst_size = 1;
             break;
           case 16u:  /* bipush */
             if(trdebug) 
               printf("%d: %s %d\n", prev_offset, op, byt[prev_offset+1u]);
             branch_label = byt[prev_offset+1u];
             inst_size = 2;
             break;
           case 17u:  /* sipush */
             if(trdebug) 
               printf("%d: %s %d\n", prev_offset, op, 
                 B2U4(0,0,byt[prev_offset+1u],byt[prev_offset+2u]));
             branch_label = B2U4(0,0,byt[prev_offset+1u],byt[prev_offset+2u]);
             inst_size = 3;
             break;
           case 18u:  /* ldc */
             if(trdebug) 
               printf("%d: %s %d\n", prev_offset, op,
                 constant_pool[byt[prev_offset+1u]] -> u.data.val1);
             branch_label = constant_pool[byt[prev_offset+1u]] -> u.data.val1;
             inst_size = 2;
             break;
           default:
             fprintf(stderr,"Bad opcode encountered, output may be incorrect.\n");
             branch_label = 0;
             inst_size = 0;
         }

         sprintf(lbuf,"%d",branch_label);

         if(!strcmp(op,"label"))
         {
           if(type_lookup(att->label_table,lbuf) != NULL)
             fprintf(stderr,"Warning: duplicate label: %s\n",lbuf);

           idx = hash(lbuf) % att->label_table->num_entries;
           type_insert(&(att->label_table->entry[idx]), prev_offset, strdup(lbuf));

           bzero(byt+last_offset, inst_size + 5);
         }
         else if(!strcmp(op,"goto"))
         {
           HASHNODE *ht;

           if(trdebug)
             printf("ok, I'm looking at a goto branching to label %d\n",
               branch_label);

           if((ht=type_lookup(att->label_table,lbuf)) != NULL)
           {
             int temp = ht->val - i;

             if(trdebug)
               printf("Found the label! offset = %d\n", ht->val);

              /* zero out the 2 previous instructions.  the
                 first 'ldc' is always 2 bytes, so add that
                 to the size of the previous instruction. */

             bzero(byt+last_offset, inst_size + 2 - 2);

              /* use the goto_w opcode just to be sure we
                 have enough space for the branchoffset */

             byt[i-2] = 200;

             if(trdebug)
               printf("copying %d (%x) into byt\n",temp,temp);

             bcopy(&temp,byt+i-1, 4);
           }
           else
           {
             if(trdebug)
               printf("did NOT find the label!\n");
           }
         }
         else if(!strcmp(op,"unknown"))
           fprintf(stderr,"Skipping unknown method invokation at offset %d\n",
              i);
         else
           fprintf(stderr,"Weird, op not set properly.\n");
          
       }

       last_op = prev_op;
       last_offset = prev_offset;
       prev_op = opc;
       prev_offset = i;
     }

     if (next)
       return;

     /* Compute Address of next Opcode
	****************************** */

     next = (u4_int) (i+ops+1u);

     if (next > len) {
       javab_out(0, "invalid implicit target %u in '%s' at %u", next, mem, i);
       return;
     }

     /* instruction 'wide'-handling
	*************************** */

     if (opc == 196u) { 
       if (i+1 < len)  {
	 i++;
	 is_wide = 1;
	 goto back;
       }
       else {
	 javab_out(0, "invalid occurrence of '%s' at %u", mem, i);
         return;
       }
     }
  }
}

/* *******************************************************
   *** The actual actions (PRIVATE TRAVERSAL ROUTINES) ***
   ******************************************************** */

/* *******************************************************
   *** Process a Single Code Attributes in .class file ***
   ******************************************************* */

static void byte_codeattr(attribute_ptr a, u2_int w_arg, 
			    u1_int *nm, u1_int *tp, u2_int w_res) {
  static u1_int comp_stuff(u4_int);
  u4_int  i; /* wide_counter */
  u1_int *bytes       = a -> info;
  u2_int  max_stack   = B2U2(bytes[0],bytes[1]);
  u2_int  max_locals  = B2U2(bytes[2],bytes[3]);
  u4_int  code_length = B2U4(bytes[4],bytes[5],bytes[6],bytes[7]);
  u2_int  exc_table_l;

  a->label_table = new_symtable(211);

  if (a -> attribute_length < 12u + code_length) {
    javab_out(0, "corrupt code atttribute given for %s%s code_length = %u",
	      nm, tp, code_length);
    return;
  }

  exc_table_l = B2U2(bytes[8+code_length],bytes[9+code_length]);

  if (code_length + 10u + exc_table_l * 8u >= a -> attribute_length)  {
    javab_out(0, "corrupt exception handler table");
    return;
  }

  /* Set global attribute (for all subsequent processing!) 
     ***************************************************** */

  att = a;

  /* Quit for empty method body (or for large codelength)
     or in case too many parameters are passed to method
     **************************************************** */

  if (code_length == 0u) {
    javab_out(2, "  + empty method %s()", nm);
    return;
  }
  else if (code_length >= (U4MAX-1)) {
    javab_out(2, "  + skipping method %s() (cannot be processed internally)", nm);
    return;
  }
  else if (w_arg > max_locals) {
    javab_out(0, "%u parameter words exceed %u local words of method %s()", 
	  	  w_arg, max_locals, nm);
    return;
  }

  /* Allocate Memory for BYTECODE Information
     **************************************** */

  a -> code_length = code_length;
  a -> is_leader   = (u1_int *)     make_mem((code_length+1) * sizeof(u1_int));
  a -> my_bb       = (bb_ptr *)     make_mem((code_length+1) * sizeof(bb_ptr));
  a -> reachable   = (u1_int *)     make_mem(code_length * sizeof(u1_int));
  a -> sp_before   = (u2_int *)     make_mem(code_length * sizeof(u2_int));
  a -> st_state    = (state_ptr **) make_mem(code_length * sizeof(state_ptr *));

  for (i = 0u; i <= code_length; i++) {
     a -> is_leader[i] = 0u;             
     a -> my_bb[i]     = NULL;
  }
  for (i = 0u; i < code_length; i++) {
     a -> reachable[i] = 2u;
     a -> sp_before[i] = 0u;
     a -> st_state[i]  = NULL;
  }

  /* Compute Stack Information:
     traverse entry point of method     (with sp==0 on entry)
     *and* entry point of every handler (with sp==1 on entry)
     ******************************************************** */

  glo_sta = max_stack;
  glo_stm = 0u;
  glo_loc = max_locals;

  /* Empty Stack */

  cur_sp = 0u;

  byte_trav(0u);
  byte_trav(0u);
}

/* ********************************************************
   *** PUBLIC FUNCTIONS                                 ***
   ******************************************************** */

/* ***************************
   *** Bytecode Processing ***
   *************************** */

void byte_proc(void) {
  
  u4_int i, j;       /* wide counters */

#ifdef CHECK_TABLE

  /* Verify bytecode table */
   
  for (i = 0u; i < 256u; i++)
    if (bytecode[i].opcode != i)
      javab_out(-1, "invalid bytecode initialization at %u", i);

#endif

  /* Scan over methods, and process code-attributes */

  for (i = 0u; i < methods_count; i++) {

    fm_ptr         m      = methods[i];
    u1_int       *nm      = constant_pool[m -> name_index]  -> u.utf8.s;
    u1_int       *tp      = constant_pool[m -> descr_index] -> u.utf8.s;
    u1_int        is_inst = (m -> access_flags & ACC_STATIC) ? 0u : 1u;
    attribute_ptr my_code = NULL;
    attribute_ptr my_exc  = NULL;

    char         *this_arg_type = NULL;

    /* Determine number of locals that are defined
       (for Instance Methods: `this' is first-word argument)
       and number of words pushed back on the caller's operand stack */

    u2_int w_arg = arg_width(tp, (is_inst) ? 2u : 1u);
    u2_int w_res = res_width(tp);

    if (is_inst) /* Determine type of `this': set to java.lang.Object */ {

      u2_int  e = constant_pool[this_class] -> u.indices.index1;
      char   *s = (char *) constant_pool[e]          -> u.utf8.s;
      u2_int  l = strlen(s);

      this_arg_type = (char *) make_mem((l+2u) * sizeof(char));
      sprintf(this_arg_type, "L%s;", s);
      rd_sig[0u] = this_arg_type;
    }

    is_instm = is_inst;

    javab_out(2, "  - processing %s method %s()", 
		  (is_inst) ? "instance" : "class", nm);

    /* Scan Attributes */

    for (j = 0u; j < m -> attributes_count; j++) {
      attribute_ptr a  = m -> attributes[j];
      constant_ptr  ua = constant_pool[a -> attribute_name_index];
 
      if (strcmp((char *) ua -> u.utf8.s, "Code") == 0)  {
	if (my_code)
	  javab_out(0, "multiple code attributes given for %s()", nm);
        else
	  my_code = a;
      }
      else if (strcmp((char *) ua -> u.utf8.s, "Exceptions") == 0)  {
	if (my_exc)
	  javab_out(0, "multiple exception attributes given for %s()", nm);
        else
	  my_exc = a;
      }
    }

    /* Process Code Attribute */

    if (my_code) {
      if (my_code -> attribute_length < 12u)
	javab_out(0, "corrupt code attribute given for %s()", nm);
      else
        byte_codeattr(my_code, w_arg, nm, tp, w_res);
    } 
    else
      javab_out(2, "  + no code attribute given for %s()", nm);

    if (this_arg_type)
      free(this_arg_type);

    if (error)
      break;   /* otherwise, a list of method 
		  headers appears for switch `-d' */
  }
}
