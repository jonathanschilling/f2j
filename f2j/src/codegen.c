/*
 * $Source$
 * $Revision$
 * $Date$
 * $Author$
 */


/*****************************************************************************
 * codegen.c                                                                 *
 *                                                                           *
 * Generates Java source code from the AST representation of a Fortran       *
 * program.                                                                  *
 *                                                                           *
 *****************************************************************************/

#include"codegen.h"

/*****************************************************************************
 *   Global variables, a necessary evil when working with yacc.              *
 *****************************************************************************/

int
  gendebug = TRUE;     /* set to TRUE to generate debugging output          */

char 
  *unit_name,           /* name of this function/subroutine                  */
  *returnname,          /* return type of this prog. unit                    */
  *cur_filename,        /* name of the class file currently writing          */
  *method_desc = NULL,  /* descriptor for method representing this prog unit */
  **funcname=input_func;/* input functions, EOF-detecting or non-detecting   */

Dlist 
  dummy_nodes = NULL,   /* list of dummy graph nodes to free later           */
  doloop = NULL,        /* stack of do loop labels                           */
  while_list = NULL,    /* stack of while loop labels                        */
  adapter_list = NULL,  /* list of adapter functions (see tech report)       */
  methcall_list = NULL, /* list of methods to be called by reflection        */
  label_list = NULL,    /* list of statements with label numbers             */
  exc_table = NULL;     /* list of exception table entries                   */

SUBSTITUTION 
  global_sub={NULL,0};  /* substitution used for implied loops               */

FILE 
  *javafp,              /* the class file currently generating               */
  *curfp,               /* the file currently being written to               */
  *savefp;              /* temp var for saving the current file pointer      */

SYMTABLE                /* Symbol tables containing...                       */
  *cur_type_table,      /* type information                                  */
  *cur_external_table,  /* external functions                                */
  *cur_intrinsic_table, /* intrinsic functions                               */
  *cur_args_table,      /* variables which are arguments                     */
  *cur_array_table,     /* variables which are arrays                        */
  *cur_format_table,    /* format statements                                 */
  *cur_data_table,      /* variables contained in DATA stmts                 */
  *cur_save_table,      /* variables contained in SAVE stmts                 */
  *cur_common_table,    /* variables contained in COMMON stmts               */
  *cur_param_table,     /* variables which are parameters                    */
  *cur_equiv_table;     /* variables which are equivalenced                  */

Dlist
  cur_const_table;      /* constants designated to go into the constant pool */

struct ClassFile
  *cur_class_file;      /* class file for the current program unit           */

struct attribute_info
  *cur_code;            /* current code attr. to which f2j writes code       */

AST 
  *cur_equivList,       /* list of equivalences                              */
  *cur_unit;            /* program unit currently being translated.          */

BOOLEAN 
  import_reflection,    /* does this class need to import reflection         */
  import_blas,          /* does it need to import the BLAS library           */
  bytecode_gen=TRUE;    /* is bytecode generation currently enabled          */

unsigned int 
  pc,                   /* current program counter                           */
  cur_local,            /* current local variable number                     */
  num_locals,           /* number of locals needed for this method           */
  stdin_lvar,           /* local var number of the EasyIn object             */
  num_handlers;         /* number of exception handlers in this method       */

struct method_info
  *clinit_method,       /* special class initialization method <clinit>      */
  *main_method;         /* the primary method for this fortran program unit  */

enum _opcode
  lastOp = jvm_nop;     /* the last opcode emitted (avoids dup return stmt)  */

ExceptionTableEntry
  * reflect_entry,      /* exception table entry for reflection exceptions.  */
  * access_entry;       /* exception table entry for access exceptions.      */

extern METHODTAB intrinsic_toks[];

extern FILE *devnull;   /* file pointer to /dev/null, opened in f2jmain.c    */

/*****************************************************************************
 *                                                                           *
 * emit                                                                      *
 *                                                                           *
 * This is the main code generation function.  We traverse the               *
 * AST and recursively call emit() on each node.  This                       *
 * function figures out what kind of node it's looking at and                *
 * calls the appropriate function to handle the code generation.             *
 *                                                                           *
 *****************************************************************************/

void
emit (AST * root)
{
    CPNODE *c;

    switch (root->nodetype)
    {
      case 0:
        if (gendebug)
          fprintf (stderr,"Bad node\n");

        emit (root->nextstmt);
        break;
      case Progunit:
        {
          char *tmpname;
          char *classname;
          char *methodname;

          if (gendebug)
            printf ("Source.\n");

          tmpname = root->astnode.source.progtype->
                       astnode.source.name->astnode.ident.name;

          classname = strdup(tmpname);
          lowercase(classname);

          /* check if this program unit is a PROGRAM.  if so, the
           * method name is "main" and we have to set the max. locals
           * to 1 because Java's main always takes a string array arg.
           */
           
          if(root->astnode.source.progtype->nodetype == Program) {
            /* dup so that we can free() later & 
             * not try to free non-heap memory
             */
            methodname = strdup("main");
            locals = 1;
          }
          else
            methodname = strdup(classname);

          classname[0] = toupper(classname[0]);

          cur_filename = get_full_classname(classname);

          /* needs initializing before creating the <init> method */
          exc_table = make_dl();

          /* First set up the local hash tables. */

          cur_type_table = root->astnode.source.type_table;
          cur_external_table = root->astnode.source.external_table;
          cur_intrinsic_table = root->astnode.source.intrinsic_table;
          cur_args_table = root->astnode.source.args_table;
          cur_array_table = root->astnode.source.array_table;
          cur_format_table = root->astnode.source.format_table;
          cur_data_table = root->astnode.source.data_table;
          cur_save_table = root->astnode.source.save_table;
          cur_common_table = root->astnode.source.common_table;
          cur_param_table = root->astnode.source.parameter_table;
          cur_equiv_table = root->astnode.source.equivalence_table;
          cur_equivList = root->astnode.source.equivalences;
          cur_const_table = root->astnode.source.constants_table;
          cur_class_file = root->astnode.source.class = 
                newClassFile(classname,inputfilename);
       
          if(gendebug)
            print_equivalences(cur_equivList);

          initialize_lists();

          assign_local_vars(
             root->astnode.source.progtype->astnode.source.args); 

          num_locals = cur_local = locals;

          /* needs_reflection is determined during typecheck */

          if(root->astnode.source.progtype->astnode.source.needs_reflection)
            import_reflection = TRUE;
          else
            import_reflection = FALSE; 

          /* needs_blas is also determined during typecheck */

          if(root->astnode.source.progtype->astnode.source.needs_blas &&
             !type_lookup(blas_routine_table,tmpname))
            import_blas = TRUE;
          else
            import_blas = FALSE; 

          open_output_file(root->astnode.source.progtype, classname);

          savefp = curfp;
          set_bytecode_status(JAVA_AND_JVM);

          if(root->astnode.source.prologComments != NULL)
            emit_prolog_comments(root);

          insert_fields(root);

          /* as part of creating a new classfile structure, we have 
           * already created an <init> method, the default constructor.
           * the class may also need a <clinit> method, the class
           * initializer.  the <clinit> method initializes any static
           * fields, DATA stmts, Strings which require new objects to
           * be created, etc.  here we create an empty CodeAttribute
           * structure and then emit the typedecs.  afterwards, we
           * check to see if any code was generated for <clinit>.
           * if so, we must create a method_info structure and add
           * that to the current classfile structure.  if not, we do
           * nothing.
           */

          clinit_method = beginNewMethod((u2)(ACC_PUBLIC | ACC_STATIC)); 
          
          emit (root->astnode.source.typedecs);
 
          emit (root->astnode.source.progtype);

          /* check whether any class initialization code was generated.
           * if so, finish initializing the method and insert it into this
           * class.
           */
          if(pc > 0) {
            bytecode0(jvm_return);
            endNewMethod(cur_class_file, clinit_method, "<clinit>", "()V", 1, NULL);
          }
          else {
            free_method_info(clinit_method);
            free_code_attribute(cur_code, NULL);
          }

          main_method = beginNewMethod(ACC_PUBLIC | ACC_STATIC);

          /* If this program unit does any reading, we declare an instance of
           * the EasyIn class.   grab a local var for this, but dont worry about
           * releasing it, since we might need it throughout the life of the
           * method.
           */

          if(root->astnode.source.progtype->astnode.source.needs_input) {
            fprintf(curfp,"  EasyIn _f2j_stdin = new EasyIn();\n");
            stdin_lvar = getNextLocal(Object);

            c = cp_find_or_insert(cur_const_table,CONSTANT_Class, EASYIN_CLASS);
            bytecode1(jvm_new,c->index);
            bytecode0(jvm_dup);

            c = newMethodref(cur_const_table, EASYIN_CLASS, "<init>", 
                   EASYIN_DESC);
            bytecode1(jvm_invokespecial, c->index);
            gen_store_op(stdin_lvar, Object);
          }

          if(type_lookup(cur_external_table,"etime") != NULL)
          {
            fprintf(curfp, "  Etime.etime();\n");

            c = newMethodref(cur_const_table, ETIME_CLASS, 
                        "etime",ETIME_DESC);
 
            bytecode1(jvm_invokestatic, c->index);
          }

          /* if one of the arguments is a function, we must use the
           * reflection mechanism to perform the method call.
           */

          if(import_reflection) {
            reflect_declarations_emit(
               root->astnode.source.progtype->astnode.source.args);

            /* The 'catch' corresponding to the following try is generated
             * in case End. 
             */

            fprintf(curfp,"try {\n");

            /* start the exception handler from the next opcode */
            reflect_entry = (ExceptionTableEntry *) 
                 f2jalloc(sizeof(ExceptionTableEntry));
            reflect_entry->from = bytecode0(jvm_impdep1);

            access_entry = (ExceptionTableEntry *) 
                 f2jalloc(sizeof(ExceptionTableEntry));
            access_entry->from = reflect_entry->from;
          }

          emit(root->astnode.source.statements);

          /* check if code was generated for this program unit's method.
           * if so, finish initializing the method and insert it into this
           * class.
           */

          if(pc > 0) {
            endNewMethod(cur_class_file, main_method,methodname,method_desc,num_locals,NULL);
          }

          f2jfree(methodname, strlen(methodname)+1);

          emit_invocations(root->astnode.source.progtype);

          emit_adapters();

          fprintf(curfp,"} // End class.\n");
          fclose(curfp);

          cur_class_file->constant_pool_count = 
             (u2) ((CPNODE *)dl_val(dl_last(cur_const_table)))->index + 1;
          cur_class_file->constant_pool = cur_const_table;

          write_class(cur_class_file);
 
          cp_dump(cur_const_table);

          free_class(cur_class_file);
          cur_class_file = NULL;
          cur_const_table = NULL;

          free_lists();

          f2jfree(classname, strlen(classname)+1);
          f2jfree(cur_filename, strlen(cur_filename)+1);

          break;
        }
      case Subroutine:
        if (gendebug)
	        printf ("Subroutine.\n");

        returnname = NULL;	/* Subroutines return void. */
        cur_unit = root;
        unit_name = root->astnode.source.name->astnode.ident.name;

        if(gendebug)
          printf ("Subroutine name: %s\n",  unit_name);

        constructor (root);
        break;
      case Function:
        if (gendebug)
          printf ("Function.\n");

        returnname = root->astnode.source.name->astnode.ident.name;
        cur_unit = root;
        unit_name = root->astnode.source.name->astnode.ident.name;

        if(gendebug)
          printf ("Function name: %s\n",  unit_name);

        constructor (root);
        break;
      case Program:
        if (gendebug)
          printf ("Program.\n");

        returnname = NULL;	/* programs return void. */
        cur_unit = root;
        unit_name = root->astnode.source.name->astnode.ident.name;

        if (gendebug)
          printf ("Program name: %s\n", unit_name);

        constructor(root);
        break;
      case Typedec:
        if (gendebug)
          printf ("Typedec.\n");

        typedec_emit (root);
        if (root->nextstmt != NULL)	/* End of typestmt list. */
          emit (root->nextstmt);
        break;
      case DataList:
        if (gendebug)
          printf ("Data.\n");

        data_emit (root);
        if (root->nextstmt != NULL)	/* End of data list. */
          emit (root->nextstmt);
        break;
      case Specification:
        if (gendebug)
          printf ("Specification.\n");

        spec_emit (root);
        if (root->nextstmt != NULL)	/* End of typestmt list. */
          emit (root->nextstmt);
        break;
      case Equivalence:
        if (gendebug)
          printf ("Equivalence.\n");

        equiv_emit (root);
        if (root->nextstmt != NULL)
          emit (root->nextstmt);
        break;
      case Statement:
        if (gendebug)
          printf ("Statement.\n");

        if (root->nextstmt != NULL)	/* End of typestmt list. */
          emit (root->nextstmt);
        break;
      case Assignment:
        if (gendebug)
          printf ("Assignment.\n");

        assign_emit (root);
        fprintf (curfp, ";\n");
        if (root->nextstmt != NULL)
          emit (root->nextstmt);
        break;
      case Call:
        if (gendebug)
          printf ("Call.\n");

        call_emit (root);
        if (root->nextstmt != NULL)	/* End of typestmt list. */
          emit (root->nextstmt);
        break;
      case Forloop:
        if (gendebug)
          printf ("Forloop.\n");

        forloop_emit (root);
        if (root->nextstmt != NULL)	/* End of typestmt list. */
          emit (root->nextstmt);
        break;
      case Blockif:
        if (gendebug)
          printf ("Blockif.\n");

        blockif_emit (root);
        if (root->nextstmt != NULL)	/* End of typestmt list. */
          emit (root->nextstmt);
        break;
      case Elseif:
        if (gendebug)
          printf ("Elseif.\n");

        elseif_emit (root);
        if (root->nextstmt != NULL)	/* End of typestmt list. */
          emit (root->nextstmt);
        break;
      case Else:
        if (gendebug)
          printf ("Else.\n");

        else_emit (root);
        if (root->nextstmt != NULL)	/* End of typestmt list. */
          emit (root->nextstmt);
        break;
      case Logicalif:
        if (gendebug)
          printf ("Logicalif.\n");

        logicalif_emit (root);
        if (root->nextstmt != NULL)	/* End of typestmt list. */
          emit (root->nextstmt);
        break;
      case Arithmeticif:
        if (gendebug)
          printf ("Arithmeticif.\n");

        arithmeticif_emit (root);
        if (root->nextstmt != NULL)	/* End of typestmt list. */
          emit (root->nextstmt);
        break;
      case Return:
        if(gendebug)
          printf("Return: %s.\n", returnname != NULL ? returnname : "void");

        /*
         * According to the f77 spec, labels cannot contain more
         * than five digits, so we use six nines as the label
         * for the final return statement to avoid conflicts with
         * labels that already exist in the program.
         */

        fprintf(curfp,"Dummy.go_to(\"%s\",999999);\n",cur_filename);

        return_emit();
        if (root->nextstmt != NULL)	/* End of typestmt list. */
          emit (root->nextstmt);
        break;
      case Goto:
        if (gendebug)
          printf ("Goto.\n");

        goto_emit (root);
        if (root->nextstmt != NULL)
          emit (root->nextstmt);
        break;
      case ComputedGoto:
        if (gendebug)
          printf ("Goto.\n");

        computed_goto_emit (root);
        if (root->nextstmt != NULL)
          emit (root->nextstmt);
        break;
      case Label:
        if (gendebug)
          printf ("Label.\n");

        label_emit (root);
        if (root->nextstmt != NULL)	/* End of typestmt list. */
          emit (root->nextstmt);
        break;
      case Write:
        if (gendebug)
          printf ("Write statement.\n");

        write_emit (root);
        if (root->nextstmt != NULL)
          emit (root->nextstmt);
        break;
      case Read:
        if (gendebug)
          printf ("Read statement.\n");

        read_emit (root);
        if (root->nextstmt != NULL)
          emit (root->nextstmt);
        break;
      case Format:
        if (gendebug)
          printf("skipping format statement\n");

        if (root->nextstmt != NULL)
          emit (root->nextstmt);
        break;
      case Stop:
        if (gendebug)
          printf ("Stop.\n");

        fprintf (curfp, "System.exit(1);\n");

        if (root->nextstmt != NULL)
          emit (root->nextstmt);
        break;
      case End:
        if (gendebug)
          printf ("End.\n");
        end_emit(root);
        break;
      case Save:
        if (gendebug)
          printf ("Save (ignoring).\n");

        if (root->nextstmt != NULL)
          emit (root->nextstmt);
        break;
      case Common:
        fprintf(stderr,"Warning: hit case Common in emit()\n");
        if (root->nextstmt != NULL)
          emit (root->nextstmt);
        break;
      case CommonList:
        if (gendebug)
          printf ("Common.\n");

        common_emit(root);
        if (root->nextstmt != NULL)
          emit (root->nextstmt);
        break;
      case MainComment:
        while(root->nextstmt != NULL && root->nextstmt->nodetype == Comment)
          root = root->nextstmt;

        if (root->nextstmt != NULL)
          emit (root->nextstmt);
        break;
      case Comment:
        if (gendebug)
          printf ("Comment.\n");

        if(curfp != NULL)
          fprintf(curfp,"// %s", root->astnode.ident.name);

        if (root->nextstmt != NULL)
          emit (root->nextstmt);
        break;
      case Dimension:
        if(gendebug)
          printf("Dimension\n");
     
        /* ignore */

        if (root->nextstmt != NULL)
          emit (root->nextstmt);
        break;
      case Unimplemented:
        fprintf (curfp, 
           " ; // WARNING: Unimplemented statement in Fortran source.\n");
        if (root->nextstmt != NULL)
          emit (root->nextstmt);
        break;
      case Constant:
      default:
        fprintf(stderr,"emit(): Error, bad nodetype (%s)\n",
          print_nodetype(root));
        if (root->nextstmt != NULL)
          emit (root->nextstmt);
        break;
    }				/* switch on nodetype.  */
}

/*****************************************************************************
 *                                                                           *
 * initialize_lists                                                          *
 *                                                                           *
 * initializes new list instances for the current program unit.              *
 *                                                                           *
 *****************************************************************************/

void
initialize_lists()
{

  /* Initialize the lists. */

  dummy_nodes = make_dl();
  while_list = make_dl();
  doloop = make_dl();
  adapter_list = make_dl();
  methcall_list = make_dl();
  label_list = make_dl();
}

/*****************************************************************************
 *                                                                           *
 * free_lists                                                                *
 *                                                                           *
 * frees memory associated with the global lists.                            *
 *                                                                           *
 *****************************************************************************/

void
free_lists()
{
  Dlist tmp;

  /* free memory from previous program units. */

  if(dummy_nodes) {
    dl_traverse(tmp, dummy_nodes)
      f2jfree(dl_val(tmp), sizeof(CodeGraphNode));
    dl_delete_list(dummy_nodes);
  }

  if(while_list) {
    dl_traverse(tmp, while_list)
      f2jfree(dl_val(tmp), sizeof(int));
    dl_delete_list(while_list);
  }

  dl_delete_list(doloop);
  dl_delete_list(adapter_list);

  if(methcall_list) {
    dl_traverse(tmp, methcall_list)
      dl_delete_list((Dlist)dl_val(tmp));
    dl_delete_list(methcall_list);
  }

  dl_delete_list(label_list);
}

/*****************************************************************************
 *                                                                           *
 * get_full_classname                                                        *
 *                                                                           *
 * returns the fully-qualified class name for the given class.               *
 *                                                                           *
 *****************************************************************************/

char *
get_full_classname(char *thisclass)
{
  char * pname, *t;

  if(package_name != NULL) {
    pname = (char *)f2jalloc(strlen(thisclass) + strlen(package_name) + 2);

    /* issue a warning if the package name has some trailing junk. */
    if(!isalnum((int)*(package_name + (strlen(package_name)-1))))
      fprintf(stderr,"WARNING: last char of package name not alphanumeric.\n");

    t = char_substitution(package_name, '.', '/');

    strcpy(pname, t);
    strcat(pname, "/");
    strcat(pname, thisclass);

    f2jfree(t, strlen(t)+1);
    return pname;
  }
  else
    return strdup(thisclass);
}

/*****************************************************************************
 *                                                                           *
 * set_bytecode_status                                                       *
 *                                                                           *
 * allow temporarily suspending generation of bytecode for situations where  *
 * the code generation ordering is very different between Java source and    *
 * JVM bytecode.  this way, f2java may suspend bytecode, generate the java   *
 * source, then generate the JVM bytecode differently.                       *
 *                                                                           *
 *****************************************************************************/

void
set_bytecode_status(int mode)
{
  switch(mode) {
    case JVM_ONLY:
      bytecode_gen=TRUE;
      savefp = curfp;
      curfp = devnull;
printf("set_bytecode_status.1: set curfp = %p\n", curfp);
      break;
    case JAVA_ONLY:
      bytecode_gen=FALSE;
      curfp = savefp;
printf("set_bytecode_status.2: set curfp = %p\n", curfp);
      break;
    case JAVA_AND_JVM:
    default:
      bytecode_gen=TRUE;
      curfp = savefp;
printf("set_bytecode_status.3: set curfp = %p\n", curfp);
      break;
  }
}

/*****************************************************************************
 *                                                                           *
 * reflect_declarations_emit                                                 *
 *                                                                           *
 * this function emits declarations for each function passed in as an arg.   *
 * the arg type is Object, so we call Object.getClass().getDeclaredMethods() *
 * to get the Method array of that object.  then we assign the first method  *
 * to the next available local variable.                                     *
 *                                                                           *
 *****************************************************************************/

void
reflect_declarations_emit(AST *root)
{
  HASHNODE *hashtemp, *ht2;
  AST *tempnode;
  CPNODE *c;
  int meth_var_num = 0;

  for(tempnode = root; tempnode != NULL; tempnode = tempnode->nextstmt)
  {
    hashtemp = type_lookup(cur_external_table, tempnode->astnode.ident.name);
    if(hashtemp)
    {
      hashtemp->variable->astnode.ident.localvnum = getNextLocal(Object);

      fprintf(curfp,"  java.lang.reflect.Method _%s_meth ", 
        tempnode->astnode.ident.name);
      fprintf(curfp," = %s.getClass().getDeclaredMethods()[0];\n",
        tempnode->astnode.ident.name);

      ht2 = type_lookup(cur_type_table, tempnode->astnode.ident.name);

      if(ht2) {
        meth_var_num = ht2->variable->astnode.ident.localvnum;

        if(gendebug)
          printf("found '%s' in type table, using localvnum = %d\n",
             tempnode->astnode.ident.name, meth_var_num);
      }
      else {
        ht2 = type_lookup(cur_args_table, tempnode->astnode.ident.name);

        if(ht2) {
          meth_var_num = ht2->variable->astnode.ident.localvnum;
          if(gendebug)
            printf("found '%s' in args table, using localvnum = %d\n",
               tempnode->astnode.ident.name, meth_var_num);
        }
        else {
          fprintf(stderr,"(1)Error: expected to find %s in symbol table.\n",
            tempnode->astnode.ident.name);
          exit(-1);
        }
      }

      gen_load_op(meth_var_num, Object);

      c = newMethodref(cur_const_table, JL_OBJECT, "getClass",
            GETCLASS_DESC);
      bytecode1(jvm_invokevirtual, c->index);

      c = newMethodref(cur_const_table, JL_CLASS, "getDeclaredMethods",
            GETMETHODS_DESC);
      bytecode1(jvm_invokevirtual, c->index);

      pushIntConst(0);
      bytecode0(jvm_aaload);
      gen_store_op(hashtemp->variable->astnode.ident.localvnum, Object);
    }
  }
}

/*****************************************************************************
 *                                                                           *
 * invocation_exception_handler_emit                                         *
 *                                                                           *
 * this function emits the bytecode for the two exception handlers that are  *
 * generated when the program unit invokes a method on a passed-in function. *
 *                                                                           *
 *****************************************************************************/

void
invocation_exception_handler_emit(ExceptionTableEntry *et)
{
  CPNODE *c;
  unsigned int vnum;

  vnum = getNextLocal(Object);

  /* emit handler for InvocationTargetException */
  et->target = gen_store_op(vnum, Object);

  c = newFieldref(cur_const_table, JL_SYSTEM, "err", OUT_DESC);
  bytecode1(jvm_getstatic, c->index);

  c = cp_find_or_insert(cur_const_table,CONSTANT_Class, STRINGBUFFER);
  bytecode1(jvm_new,c->index);
  bytecode0(jvm_dup);

  pushStringConst("Error Calling Method: ");

  c = newMethodref(cur_const_table, STRINGBUFFER, "<init>", STRBUF_DESC);
  bytecode1(jvm_invokespecial, c->index);

  gen_load_op(vnum,Object);

  c = newMethodref(cur_const_table,THROWABLE_CLASS,
           "getMessage", GETMSG_DESC);
  bytecode1(jvm_invokevirtual, c->index);

  c = newMethodref(cur_const_table,STRINGBUFFER,
           "append", append_descriptor[String]);
  bytecode1(jvm_invokevirtual, c->index);

  c = newMethodref(cur_const_table,STRINGBUFFER,
           "toString", TOSTRING_DESC);
  bytecode1(jvm_invokevirtual, c->index);

  c = newMethodref(cur_const_table, PRINTSTREAM, "println",
        println_descriptor[String]);
  bytecode1(jvm_invokevirtual, c->index);

  /* artificially set stack depth at beginning of exception
   * handler to 1.
   */
  et->target->stack_depth = 1;

  releaseLocal(Object);
}

/*****************************************************************************
 *                                                                           *
 * end_emit                                                                  *
 *                                                                           *
 * We only generate one real return statement.  The other return statements  *
 * are emitted as gotos to the end of the code.  See the tech report for the *
 * reasoning behind this decision.  Anyway, here at the end, we emit the     *
 * real return statement.  We use six nines as the label to avoid conflicts  *
 * with other labels.  See comment above in the Return case.                 *
 *                                                                           *
 *****************************************************************************/

void
end_emit(AST *root)
{
  CodeGraphNode *goto_node, *goto_node2;
  CPNODE *c;

  if(import_reflection) {
    /* this goto skips the execption handlers under normal execution */
    goto_node = bytecode0(jvm_goto);

    /* set the end point for the exception handlers. */
    reflect_entry->to = goto_node;
    access_entry->to = goto_node;

    invocation_exception_handler_emit(reflect_entry);
    goto_node2 = bytecode0(jvm_goto);
    invocation_exception_handler_emit(access_entry);

    c = cp_find_or_insert(cur_const_table,CONSTANT_Class, INVOKE_EXCEPTION);
    reflect_entry->catch_type = c->index;

    c = cp_find_or_insert(cur_const_table,CONSTANT_Class, ACCESS_EXCEPTION);
    access_entry->catch_type = c->index;

    dl_insert_b(exc_table, reflect_entry);
    dl_insert_b(exc_table, access_entry);

    goto_node->branch_target = bytecode0(jvm_impdep1);
    goto_node2->branch_target = bytecode0(jvm_impdep1);

    fprintf(curfp, "%s%s%s%s%s%s%s",
       "} catch (java.lang.reflect.InvocationTargetException _e) {\n",
       "   System.err.println(\"Error calling method.", 
       "  \"+ _e.getMessage());\n",
       "} catch (java.lang.IllegalAccessException _e2) {\n",
       "   System.err.println(\"Error calling method.",
       "  \"+ _e2.getMessage());\n",
       "}\n");
  }

  fprintf(curfp,"Dummy.label(\"%s\",999999);\n",cur_filename); 

  if (returnname != NULL) {
    if(omitWrappers && !cgPassByRef(returnname))
      fprintf (curfp, "return %s;\n", returnname);
    else
      fprintf (curfp, "return %s.val;\n", returnname);
  }
  else
    fprintf (curfp, "return;\n");

  fprintf (curfp, "   }\n");

  /* in Fortran if the program unit is a PROGRAM, it has no explicit
   * return statement.  however, Java bytecode requires an explicit return
   * instruction even if the method returns void.  also, if I remember
   * correctly from the F77 spec, FUNCTIONs and SUBROUTINEs do not 
   * require an explicit return statement, but the END statement acts
   * as an implicit return in these cases.   here we must generate a
   * return statement however we want to avoid generating two return
   * statements because then the bytecode verifier will reject the class.
   * to avoid duplicates, check whether the last opcode generated was
   * a return.  if so, do not generate another one here. 
   */
  switch(lastOp) {
    case jvm_ireturn:
    case jvm_lreturn:
    case jvm_freturn:
    case jvm_dreturn:
    case jvm_areturn:
    case jvm_return:
      /* do nothing */
      break;
    default:
      return_emit();
      break;  /* ansi compliance */
  }
}

/*****************************************************************************
 *                                                                           *
 * return_emit                                                               *
 *                                                                           *
 * This function generates code to return from a method.  Fortran program    *
 * units PROGRAM and SUBROUTINE both return void, while FUNCTIONs return     *
 * the Java type corresponding to their original Fortran declaration.        *
 *                                                                           *
 *****************************************************************************/

void
return_emit()
{
  /* for bytecode, check if the current program unit is a
   * Function.  if so, we push the implicit return value
   * on the stack and return.  otherwise, just return void.
   */

  if(returnname) {
    if(omitWrappers && !cgPassByRef(returnname))
      pushVar(cur_unit->vartype, FALSE, cur_filename,
              returnname, field_descriptor[cur_unit->vartype][0],
              0, FALSE);
    else
      pushVar(cur_unit->vartype, FALSE, cur_filename,
              returnname, wrapped_field_descriptor[cur_unit->vartype][0],
              0, TRUE);
    bytecode0(return_opcodes[cur_unit->vartype]);
  }
  else
    bytecode0(jvm_return);
}

/*****************************************************************************
 *                                                                           *
 * field_emit                                                                *
 *                                                                           *
 * This function is called by insert_fields to create a new field_info       *
 * structure for the given variable.                                         *
 *                                                                           *
 *****************************************************************************/

void
field_emit(AST *root)
{
  char * desc, * name;
  HASHNODE *ht;

  /* check if this variable has a merged name.  if so,
   * use that name instead.
   */
  ht = type_lookup(cur_equiv_table,root->astnode.ident.name);

  if(ht && ht->variable->astnode.ident.merged_name)
    name = ht->variable->astnode.ident.merged_name;
  else {
    ht = type_lookup(cur_type_table,root->astnode.ident.name);

    if(ht && ht->variable->astnode.ident.merged_name) 
      name = ht->variable->astnode.ident.merged_name;
    else
      name = root->astnode.ident.name;
  }

  desc = getVarDescriptor(root);

  if(ht)
    ht->variable->astnode.ident.descriptor = desc;
  else {
    if((ht = type_lookup(cur_type_table,root->astnode.ident.name)) != NULL)
      ht->variable->astnode.ident.descriptor = desc;
    else
      fprintf(stderr,"WARNING: can't find ident to set descriptor\n"); 
  }

  if(gendebug) {
    printf("going to emit field %s\n",name);
    printf("\ttype: %s (%d)\n",returnstring[root->vartype], root->vartype);
    printf("\t dim: %d\n",root->astnode.ident.dim);
    printf("\tdesc: %s\n",desc);
  }

  addField(name,desc);
}

/*****************************************************************************
 *                                                                           *
 * addField                                                                  *
 *                                                                           *
 * this code creates the field_info structure, assigns the                   *
 * appropriate values into it, and inserts it into the field list.           *
 *                                                                           *
 *****************************************************************************/

void
addField(char *name, char *desc)
{
  struct field_info * tmpfield;
  CPNODE *c;

  printf("addField() creating new field for %s - %s\n",name,desc);

  tmpfield = (struct field_info *) f2jalloc(sizeof(struct field_info));
  tmpfield->access_flags = ACC_PUBLIC | ACC_STATIC;

  c = cp_find_or_insert(cur_const_table, CONSTANT_Utf8, name);
  tmpfield->name_index = c->index;

  c = cp_find_or_insert(cur_const_table, CONSTANT_Utf8, desc);
  tmpfield->descriptor_index = c->index;

  tmpfield->attributes_count = 0;
  tmpfield->attributes = NULL;

  dl_insert_b(cur_class_file->fields, tmpfield);

  cur_class_file->fields_count++;
}

/*****************************************************************************
 *                                                                           *
 * insert_fields                                                             *
 *                                                                           *
 * Each variable in the program unit is generated as a static field in the   *
 * current class.  Loop through all the type declarations, inserting each    *
 * variable into the list of fields.  ignore all specification statements    *
 * except for actual type declarations.  also ignore arguments to this       *
 * program unit since they will be declared as local variables, not fields.  *
 * we will go back later and generate code to initialize everything, but     *
 * first we need to get all the field names in the constant pool.            *
 *                                                                           *
 *****************************************************************************/

void
insert_fields(AST *root)
{
  AST *temp, *dec, *etmp;
  HASHNODE *hashtemp;

  /* for every spec statement */
  for(temp = root->astnode.source.typedecs; temp; temp = temp->nextstmt) {
    if(temp->nodetype == Typedec) {
      /* for every variable in this specification stmt */
      for(dec = temp->astnode.typeunit.declist; dec; dec = dec->nextstmt) {
        if(  ! type_lookup (cur_external_table, dec->astnode.ident.name)
          && ! type_lookup (cur_intrinsic_table, dec->astnode.ident.name)
          && ! type_lookup (cur_args_table, dec->astnode.ident.name)
          && ! type_lookup (cur_param_table, dec->astnode.ident.name)
          && ! type_lookup (cur_equiv_table, dec->astnode.ident.name)
          && ! type_lookup (cur_common_table, dec->astnode.ident.name))
        {
          field_emit(dec);
        }
      }
    } 
    else if(temp->nodetype == Equivalence) {
      /* for each group of equivalenced variables... */

      for(etmp = temp->astnode.equiv.nlist; etmp != NULL; etmp = etmp->nextstmt)
      {
        /* only generate a field entry for the first node. */

        if(etmp->astnode.equiv.clist != NULL) {
          hashtemp = type_lookup(cur_type_table,
                   etmp->astnode.equiv.clist->astnode.ident.name);

          if(hashtemp)
            field_emit(hashtemp->variable);
          else
            fprintf(stderr,"insert_fields(): can't find data type for %s\n" ,
               etmp->astnode.equiv.clist->astnode.ident.name);
        }
      }
    }
  }
}

/*****************************************************************************
 *                                                                           *
 * print_equivalences                                                        *
 *                                                                           *
 * Print the variables that are equivalenced.                                *
 * This routine is used only for debugging                                   *
 *                                                                           *
 *****************************************************************************/

void
print_equivalences(AST *root)
{
  AST *temp;

  printf("M_EQV  Equivalences:\n");
  for(temp=root; temp != NULL; temp = temp->nextstmt) {
    printf("M_EQV (%d)", temp->token);
    print_eqv_list(temp,stdout);
  }
}

/*****************************************************************************
 *                                                                           *
 * print_eqv_list                                                            *
 *                                                                           *
 * This function prints the equivalence list to the file                     *
 * pointed to by fptr.                                                       *
 *                                                                           *
 *****************************************************************************/

void 
print_eqv_list(AST *root, FILE *fptr)
{
  AST *temp;

  for(temp = root->astnode.equiv.clist;temp!=NULL;temp=temp->nextstmt)
    fprintf(fptr," %s, ", temp->astnode.ident.name);
  fprintf(fptr,"\n");
}

/*****************************************************************************
 *                                                                           *
 * emit_prolog_comments                                                      *
 *                                                                           *
 * 'Prolog' refers to those comments found before the                        *
 * function/subroutine declaration.  Here we emit those                      *
 * comments.                                                                 *
 *                                                                           *
 *****************************************************************************/

void 
emit_prolog_comments(AST *root)
{
  AST *temp;

  temp = root->astnode.source.prologComments;

  if(temp == NULL)
    return;

  while( (temp != NULL) && (temp->nodetype == Comment))
  {
    fprintf(curfp,"// %s",temp->astnode.ident.name);
    temp = temp->nextstmt;
  }
}

/*****************************************************************************
 *                                                                           *
 * emit_javadoc_comments                                                     *
 *                                                                           *
 * generate comments in javadoc format.                                      *
 *                                                                           *
 *****************************************************************************/

void 
emit_javadoc_comments(AST *root)
{
  AST *temp;

  temp = root->astnode.source.javadocComments;

  if(temp == NULL)
    return;

  fprintf(curfp,"/**\n");
  fprintf(curfp,"*<pre>\n");
  fprintf(curfp,"*Following is the description from the original\n");
  fprintf(curfp,"*Fortran source.  For each array argument, the Java\n");
  fprintf(curfp,"*version will include an integer offset parameter, so\n");
  fprintf(curfp,"*the arguments may not match the description exactly.\n");
  fprintf(curfp,"*Contact <a href=\"mailto:seymour@cs.utk.edu\">");
  fprintf(curfp,"seymour@cs.utk.edu</a> with any");
  fprintf(curfp," questions.\n");
  fprintf(curfp,"*<p>\n");
  fprintf(curfp,"*\n");
  while( (temp != NULL) && (temp->nodetype == MainComment ||
                            temp->nodetype == Comment))
  {
    fprintf(curfp,"* %s",temp->astnode.ident.name);
    temp = temp->nextstmt;
  }
  fprintf(curfp,"*</pre>\n");
  fprintf(curfp,"**/\n");
}

/*****************************************************************************
 *                                                                           *
 * equiv_emit                                                                *
 *                                                                           *
 * Generate declarations for equivalenced variables.  This handles           *
 * only a very restricted set of equivalences.  Scalars can be               *
 * equivalenced and arrays can be equivalenced, but only if the              *
 * starting points are the same.                                             *
 *                                                                           *
 * To translate equivalences, we just merge the equivalenced names           *
 * into one name and generate one Java declaration.                          *
 *                                                                           *
 *****************************************************************************/


void 
equiv_emit (AST *root)
{
  HASHNODE *ht;
  AST *temp;
  enum returntype curType;

  /* for each group of equivalenced variables... */

  for(temp = root->astnode.equiv.nlist; temp != NULL; temp = temp->nextstmt)
  {

    /* just check the first variable since we're only going to emit
     * one declaration.
     */

    if(temp->astnode.equiv.clist != NULL) {
      ht = type_lookup(cur_type_table,
               temp->astnode.equiv.clist->astnode.ident.name);

      if(ht) {
        curType = ht->variable->vartype;

        if(gendebug)
          if(ht->variable->astnode.ident.arraylist != NULL)
            printf("EQV looks like %s is an array\n",
               ht->variable->astnode.ident.name);
      }
      else {
        fprintf(stderr,"equiv_emit(): can't find data type for %s\n" ,
           temp->astnode.equiv.clist->astnode.ident.name);
        curType = 0;
      }

      /* now emit the declaration as with any other variable.  */

      if(temp->astnode.equiv.clist->astnode.ident.merged_name != NULL)
        vardec_emit(ht->variable, curType);
    }
  }
}

/*****************************************************************************
 *                                                                           *
 *  common_emit                                                              *
 *                                                                           *
 *  This function emits common blocks as a static class containing           *
 *  the variables specified in the COMMON statement.  Currently,             *
 *  each COMMON statement must specify the same variable names for           *
 *  the translation to work reliably.     10/9/97   --Keith                  *
 *                                                                           *
 *  Now COMMON statements may use different variable names and               *
 *  f2java attempts to merge the names into one.  --Keith                    *
 *                                                                           *
 *****************************************************************************/

void
common_emit(AST *root)
{
  HASHNODE *hashtemp;
  AST *Ctemp, *Ntemp, *temp;
  char *common_classname=NULL, *filename=NULL;
  FILE *commonfp;
  char * prefix = strtok(strdup(inputfilename),".");
  int needs_dec = FALSE;
  Dlist save_const_table;
  struct ClassFile *save_class_file;
  struct attribute_info *save_code;
  int save_stack, save_pc, save_handlers;
  char *save_filename;
  struct method_info *save_clinit;

  /* save the current global variables pointing to the class file.  this is
   * necessary because we're in the middle of generating the class file
   * for the current fortran program unit, but now we need to generate some
   * classes to hold COMMON blocks and we dont want to alter the pc, stack,
   * etc for the current class.
   */
  save_const_table = cur_const_table;
  save_class_file = cur_class_file; 
  save_handlers = num_handlers;
  save_filename = cur_filename; 
  save_clinit = clinit_method;
  save_code = cur_code;
  save_stack = stacksize;
  save_pc = pc;

  /*
   * Ctemp loops through each common block name specified
   * in the COMMON statement and Ntemp loops through each
   * variable in each common block. 
   */

  for(Ctemp=root->astnode.common.nlist;Ctemp!=NULL;Ctemp=Ctemp->nextstmt)
  {
    if(Ctemp->astnode.common.name != NULL) 
    {
      /* common block filename will be a concatenation of
       * the original input filename and the name of this
       * common block.
       */
      common_classname = (char *)f2jrealloc(common_classname,
         strlen(prefix) + strlen(Ctemp->astnode.common.name) + 2);
      sprintf(common_classname,"%s_%s",prefix,Ctemp->astnode.common.name);

      if(gendebug)
        printf("emitting common block '%s'\n",common_classname);


      cur_filename = get_full_classname(common_classname);

      filename = (char *)f2jrealloc(filename,
                                    strlen(cur_filename) + 6);
      sprintf(filename,"%s.java", cur_filename);

      cur_const_table = make_dl();
      cur_class_file = newClassFile(common_classname,inputfilename);
      clinit_method = beginNewMethod(ACC_PUBLIC | ACC_STATIC);

      if(gendebug)
        printf("## going to open file: '%s'\n", filename);

      if((commonfp = fopen_fullpath(filename,"w"))==NULL) 
      {
        fprintf(stderr,"Cannot open output file '%s'.\n",filename);
        perror("Reason");
        exit(1);
      }
  
      curfp = commonfp;
printf("common_emit.1: set curfp = %p\n", curfp);
      
      /* import util package for object wrapper classes */

      fprintf(curfp,"import org.netlib.util.*;\n\n");

      if(Ctemp->astnode.common.name != NULL)
        fprintf(curfp,"public class %s_%s\n{\n",prefix,
          Ctemp->astnode.common.name);

      for(Ntemp=Ctemp->astnode.common.nlist;Ntemp!=NULL;Ntemp=Ntemp->nextstmt)
      {
        needs_dec = FALSE;

        if(gendebug)
        {
          printf("Common block %s -- %s\n",Ctemp->astnode.common.name,
            Ntemp->astnode.ident.name);
          printf("Looking up %s in the type table\n",Ntemp->astnode.ident.name);
        }
  
        /* each variable in the common block should have a type
         * declaration associated with it.
         */

        if((hashtemp=type_lookup(cur_type_table,Ntemp->astnode.ident.name))==NULL)
        {
          fprintf(stderr,"Error: can't find type for common %s\n",
            Ntemp->astnode.ident.name);
          if(gendebug)
            printf("Not Found\n");
          continue;
        }

        if(gendebug)
          printf("Found\n");

        temp = hashtemp->variable;

        field_emit(temp);

        if(temp->astnode.ident.needs_declaration)
          needs_dec = TRUE;

        /* now emit the variable declaration as with any
         * other variable.
         */

        vardec_emit(temp, temp->vartype);
      }
      if(Ctemp->astnode.common.name != NULL)
        fprintf(curfp,"}\n");
  
      fclose(curfp);

      /* check whether any class initialization code was generated.
       * if so, finish initializing the method and insert it into this
       * class.
       */
      if(pc > 0) {
        bytecode0(jvm_return);
        endNewMethod(cur_class_file, clinit_method, "<clinit>", "()V", 1, NULL);
      }

      cur_class_file->constant_pool_count = 
         (u2) ((CPNODE *)dl_val(dl_last(cur_const_table)))->index + 1;
      cur_class_file->constant_pool = cur_const_table;

      write_class(cur_class_file);
    }
  }

  curfp = javafp;
printf("common_emit.2: set curfp = %p\n", curfp);

  /* restore previously saved globals */

  cur_const_table = save_const_table;
  cur_class_file = save_class_file;
  num_handlers = save_handlers;
  cur_filename = save_filename; 
  clinit_method = save_clinit;
  stacksize = save_stack;
  cur_code = save_code;
  pc = save_pc;
}

/*****************************************************************************
 *                                                                           *
 * typedec_emit                                                              *
 *                                                                           *
 * Emit all the type declarations.  This procedure checks                    *
 * whether variables are typed in the argument list, and                     *
 * does not redeclare those arguments.                                       *
 *                                                                           *
 *****************************************************************************/

void
typedec_emit (AST * root)
{
  AST *temp;
  HASHNODE *hashtemp, *ht;
  enum returntype returns;
  char *tempname;

  /* 
   *  This may have to be moved into the looop also.  Could be
   *  why I have had problems with this stuff.  
   * 
   * commented out 3/6/98 -- keith
   *
   * hashtemp = type_lookup (cur_external_table, temp->astnode.ident.name);
   * if (hashtemp)
   *  return;
   */

  returns = root->astnode.typeunit.returns;

  /*  
   * Somewhere in here I need to do a table lookup
   * to see whether the variable is in the argument
   * list for the method.  If so, it takes the type
   * in the argument list and is not retyped here. 
   */

  for(temp=root->astnode.typeunit.declist; temp != NULL; temp = temp->nextstmt)
  {

    if(omitWrappers) {
      if(gendebug)
        printf("vardec %s\n", temp->astnode.ident.name);
      if((ht= type_lookup(cur_type_table,temp->astnode.ident.name)) != NULL)
      {
        if(gendebug)
          printf("%s should be %s\n", temp->astnode.ident.name,
            ht->variable->astnode.ident.passByRef ? "WRAPPED" : "PRIMITIVE");
      }
      else
        fprintf(stderr,"could not find %s\n", temp->astnode.ident.name);
    }

    /* 
     * If there is a corresponding data statement for this
     * variable, don't emit anything here.  Just wait and
     * let the whole thing get emitted when we come across
     * the DATA node.  --9/22/97,  Keith 
     */

    if(type_lookup(cur_data_table,temp->astnode.ident.name)) {
      if(gendebug)
        printf("@@ Variable %s: Found corresponding data stmt\n",
          temp->astnode.ident.name);

      ht = type_lookup(cur_type_table,temp->astnode.ident.name);

      if(ht == NULL)
        continue;
      
      if( ! ht->variable->astnode.ident.needs_declaration)
        continue;
    }
    else
      if(gendebug)
        printf("@@ Variable %s: Corresponding data stmt not found\n",
          temp->astnode.ident.name);

    /*
     *  dont worry about checking the save table now since we're 
     *  going to emit everything as static variables.  --keith
     *
     *    if(type_lookup(cur_save_table,temp->astnode.ident.name))
     *      continue;
     */
 

    /* 
     * check to se if this variable is equivalenced with some
     * other variable(s).  if so, do not emit a variable 
     * declaration here.
     */

    hashtemp = type_lookup(cur_equiv_table,temp->astnode.ident.name);
    if(hashtemp) {
      if(type_lookup(cur_common_table,temp->astnode.ident.name)) {
        fprintf(stderr,"Please dont mix COMMON and EQUIVALENCE.  ");
        fprintf(stderr,"I dont like it.  It scares me.\n");
      } else {
        fprintf(curfp,"  // %s equivalenced to %s\n",
          temp->astnode.ident.name, 
          hashtemp->variable->astnode.ident.merged_name);
      }
      continue;
    }

    /* 
     * also do not try to redefine a 'common' variable since
     * they are placed in their own classes.  10-8-97 -- Keith 
     */

    if(type_lookup(cur_common_table,temp->astnode.ident.name))
      continue;

    /* 
     * Dont emit anything for intrinsic functions.
     */

    tempname = strdup(temp->astnode.ident.name);
    uppercase(tempname);

    if(( methodscan (intrinsic_toks, tempname) != NULL)
     && (type_lookup(cur_intrinsic_table,temp->astnode.ident.name) != NULL))
    {
      f2jfree(tempname,strlen(tempname)+1);
      continue;
    }

    f2jfree(tempname,strlen(tempname)+1);

     /* 
      * Let's do the argument lookup first. No need to retype variables
      * that are already declared in the argument list, or declared
      * as externals.  So if it is already declared, loop again.   
      */

    hashtemp = type_lookup (cur_args_table, temp->astnode.ident.name);
    if (hashtemp)
    {
      if(gendebug)
        printf("### %s is in the args_table, so I'm skipping it.\n",
           temp->astnode.ident.name);
      continue;
    }

    if(type_lookup(cur_external_table, temp->astnode.ident.name) != NULL)
    {
      /* skip externals */
      continue;
    }

    if(gendebug)
      printf("### calling vardec_emit on %s\n",temp->astnode.ident.name);

    vardec_emit(temp, returns);
  }
}				/* Close typedec_emit(). */

/*****************************************************************************
 *                                                                           *
 * newarray_emit                                                             *
 *                                                                           *
 * this function emits the newarray instruction appropriate to the data type *
 * of the given node.                                                        *
 *                                                                           *
 *****************************************************************************/

void
newarray_emit(AST *root)
{
  CPNODE *c;

  switch(root->vartype) {
    case String:
    case Character:
      c = cp_find_or_insert(cur_const_table, CONSTANT_Class, "java/lang/String");
      bytecode1(jvm_anewarray, c->index);
      break;
    case Complex:
    case Double:
    case Float:
    case Integer:
    case Logical:
      bytecode1(jvm_newarray, jvm_array_type[root->vartype]);
      break;
    default:
      fprintf(stderr,"WARNING: newarray_emit() unknown vartype\n");
  }
}

/*****************************************************************************
 *                                                                           *
 * vardec_emit                                                               *
 *                                                                           *
 * the body of this function used to be in typedec_emit, but                 *
 * I moved it so that I could use the same code to emit static               *
 * or nonstatic variables.   10/3/97  -- Keith                               *
 *                                                                           *
 * This could probably be simplified somewhat now that all                   *
 * variables are emitted 'static'.   1/27/98 -- Keith                        *
 * ...done 3/26/98 -- Keith                                                  *
 *                                                                           *
 *****************************************************************************/

void
vardec_emit(AST *root, enum returntype returns)
{
  char *prefix, *name, *desc;
  HASHNODE *hashtemp, *ht2;
  int count=0;
  AST *temp2;
  CPNODE *c;

  prefix = "static ";

  if(gendebug)
    printf("ident = %s, prefix = %s\n",root->astnode.ident.name,prefix);

  /* the top of the stack now contains the array we just created.
   * now issue the putstatic instruction to store the array reference
   * into the static variable.  if this ident is equivalenced, we
   * need to get the name/descriptor from the merged variable.
   */

  if((hashtemp = type_lookup(cur_common_table,root->astnode.ident.name))) {
    ht2 = type_lookup(cur_type_table,root->astnode.ident.name);

    name = ht2->variable->astnode.ident.merged_name;
    desc = ht2->variable->astnode.ident.descriptor;
  }
  else if((hashtemp = type_lookup(cur_equiv_table,root->astnode.ident.name))) {
    name = hashtemp->variable->astnode.ident.merged_name;
    desc = hashtemp->variable->astnode.ident.descriptor;
  }
  else {
    name = root->astnode.ident.name;

    ht2 = type_lookup(cur_type_table,root->astnode.ident.name);

    if(ht2 && ht2->variable->astnode.ident.descriptor)
      desc = ht2->variable->astnode.ident.descriptor;
    else {
      desc = field_descriptor[returns][root->astnode.ident.dim];
    }
  }

  /* 
   * check to see if this is an array declaration or not. 
   * if so, we must generate the appropriate "new" statement.
   * otherwise, just declare & initialize in one statement. --keith 
   */

  if(root->astnode.ident.arraylist != NULL) {
    fprintf (curfp, "%s%s [] ",prefix, returnstring[returns]);

    if (gendebug)
      printf ("%s\n", returnstring[returns]);
    name_emit (root);

    if (returns == Integer)
      fprintf (curfp, "= new int[");
    else if (returns == Double)
      fprintf (curfp, "= new double[");
    else if (returns == Logical)
      fprintf (curfp, "= new boolean[");
    else if ((returns == String) || (returns == Character))
      fprintf (curfp, "= new String[");
    else
      fprintf(stderr,"vardec_emit():  Unknown type (%d)!\n",returns);
       
    /* make sure this variable is in the array table */

    hashtemp = type_lookup(cur_array_table,root->astnode.ident.name);
    if(hashtemp != NULL) 
    {
      /* loop through each dimension of the array */

      temp2=root->astnode.ident.arraylist;
      for(count=0 ; temp2!=NULL ; temp2=temp2->nextstmt, count++) 
      {
        if(temp2 != root->astnode.ident.arraylist)
          fprintf(curfp, " * ");   /* if not the first iteration */

        fprintf(curfp,"(");

        if(temp2->nodetype == ArrayIdxRange)
        {
          /* if we have a range of indices (e.g. integer a(0:12))
           * then we must allocate (end - start + 1) elements. 
           */

          expr_emit(temp2->astnode.expression.rhs);
          fprintf(curfp," - ");
          expr_emit(temp2->astnode.expression.lhs);
          fprintf(curfp," + 1");

          /* at this point, we've pushed the end and start onto the
           * stack, so now we just subtract start from end and increment
           * by one as described above.
           */
          bytecode0(jvm_isub);
          bytecode0(jvm_iconst_1);
          bytecode0(jvm_iadd);
        }
        else
          expr_emit(temp2);

        /* if this isn't the first iteration, then we must multiply
         * the dimensions to get the total size of the array.
         */
        if(temp2 != root->astnode.ident.arraylist)
          bytecode0(jvm_imul);

        fprintf(curfp,")");
      }
    }
    else
      fprintf(stderr,"vardec_emit: Can't find %s in array table!\n",
         root->astnode.ident.name);

    fprintf (curfp, "];\n");

    /* now the stack contains the number of elements for this
     * array, so now we issue a newarray instruction to create the
     * new array.  we have to distinguish between arrays of
     * primitives and arrays of references because there are
     * different opcodes for creating these arrays.
     */

    newarray_emit(root);

    c = newFieldref(cur_const_table,cur_filename, name, desc); 
    bytecode1(jvm_putstatic, c->index);

  } else {    /* this is not an array declaration */

    if(!type_lookup(cur_param_table, root->astnode.ident.name))
    {
      if(omitWrappers && !cgPassByRef(root->astnode.ident.name))
        fprintf (curfp, "%s%s ", prefix, returnstring[returns]);
      else
        fprintf (curfp, "%s%s ", prefix, wrapper_returns[returns]);

      if (gendebug)
        printf ("%s\n", returnstring[returns]);

      name_emit (root);

      /* this variable is not declared as a parameter, so
       * initialize it with an initial value depending on
       * its data type.
       */

      if ((returns == String) || (returns == Character))
      {
        print_string_initializer(root);
        fprintf(curfp,";\n");

        if(gendebug) {
          printf("new fieldref:\n");
          printf("\tclass: %s\n", cur_filename);
          printf("\tname:  %s\n", name);
          printf("\tdesc:  %s\n", desc);
        }

        c = newFieldref(cur_const_table,cur_filename,name,desc); 
        bytecode1(jvm_putstatic, c->index);
      }
      else {
        if(omitWrappers && !cgPassByRef(root->astnode.ident.name)) {
            fprintf(curfp,"= %s;\n", init_vals[returns]);
        }
        else
        {
          c = cp_find_or_insert(cur_const_table,CONSTANT_Class,
                    full_wrappername[returns]);

          bytecode1(jvm_new,c->index);
          bytecode0(jvm_dup);

          bytecode0(init_opcodes[returns]);

          c = newMethodref(cur_const_table,full_wrappername[returns],
                 "<init>", wrapper_descriptor[returns]);

          bytecode1(jvm_invokespecial, c->index);

          c = newFieldref(cur_const_table,cur_filename,name,desc); 
          bytecode1(jvm_putstatic, c->index);

          fprintf(curfp,"= new %s(%s);\n",wrapper_returns[returns],
            init_vals[returns]);
        }
      }
    }
  }
}

/*****************************************************************************
 *                                                                           *
 * print_string_initializer                                                  *
 *                                                                           *
 * This function prints the initialization code for a                        *
 * String object.  If we know how long the string is supposed to             *
 * be, then we can generate a blank string of that length.  Thus             *
 * any length operations on the 'uninitialized' string would be              *
 * correct.                                                                  *
 *                                                                           *
 *****************************************************************************/

void
print_string_initializer(AST *root)
{
  char *src_initializer, *bytecode_initializer;
  AST *tempnode;
  HASHNODE *ht;

  if(gendebug)
    printf("in print_string_initializer()\n");

  ht = type_lookup(cur_type_table,root->astnode.ident.name);
  if(ht == NULL)
  {
    fprintf(stderr,"Weird...can't find '%s' in type_table\n",
      root->astnode.ident.name);

    /* We can't find this variable in the hash table, 
     * so just initialize the string to the standard initial
     * value found in init_vals.
     */

    src_initializer = init_vals[String];
  }
  else
  {
    /* We know how long this string is supposed to be, so we
     * allocate a blank string with that many characters.  For
     * example, CHARACTER*5 blah is translated to:
     *   String blah = new String("     ");
     * assuming it has not been declared with a DATA statement.
     */

    src_initializer = (char *)f2jalloc( ht->variable->astnode.ident.len + 3);

    sprintf(src_initializer,"\"%*s\"",ht->variable->astnode.ident.len," ");

  }

  /* we've created the initializer for java source code generation,
   * but for JVM opcode, we do not need the quotes within the string. 
   * here we remove them and create a bytecode initializer. 
   */

  bytecode_initializer = (char *)f2jalloc(strlen(src_initializer) - 1);
  strncpy(bytecode_initializer, src_initializer + 1, strlen(src_initializer) -2);
  bytecode_initializer[strlen(src_initializer) - 2] = '\0';

  tempnode = addnode();
  tempnode->token = STRING;
  strcpy(tempnode->astnode.constant.number, bytecode_initializer);

  if(omitWrappers && !cgPassByRef(root->astnode.ident.name)) {
    fprintf(curfp,"= new String(%s)", src_initializer);
    invoke_constructor(JL_STRING, tempnode, STR_CONST_DESC);
  }
  else {
    fprintf(curfp,"= new StringW(%s)", src_initializer);
    invoke_constructor(full_wrappername[String], tempnode,
       wrapper_descriptor[String]);
  }
}

/*****************************************************************************
 *                                                                           *
 * data_emit                                                                 *
 *                                                                           *
 * This function handles emitting DATA statements, which consist of a        *
 * list of names and a list of data items.  We start with the first name     *
 * and assign as many data items from the list as the size allows.  for      *
 * example if the first name is a 5 element array, we assign the first 5     *
 * data items to the first name.  then we go to the second name, third       *
 * name, etc. and assign values in the same way.     10/3/97  --Keith        *
 *                                                                           *
 *****************************************************************************/

void
data_emit(AST *root)
{
  AST * Dtemp, *Ntemp, *Ctemp;
  HASHNODE *hashtemp;

  /* foreach Data spec... */
  for(Dtemp = root->astnode.label.stmt; Dtemp != NULL; Dtemp = Dtemp->prevstmt) 
  {
    Ctemp = Dtemp->astnode.data.clist;

    /* foreach variable... */    
    for(Ntemp = Dtemp->astnode.data.nlist;Ntemp != NULL;Ntemp=Ntemp->nextstmt) 
    {
      /* check to see if we're looking at an implied do loop */

      if(Ntemp->nodetype == DataImpliedLoop) 
      {
        data_implied_loop_emit(Ntemp, Ctemp);
        continue;
      }

      /* This variable should have a type declaration associated with it */

      hashtemp = type_lookup(cur_type_table,Ntemp->astnode.ident.name);

      if(hashtemp == NULL)
      {
        fprintf(stderr,"No typedec associated with this DATA variable: %s\n",
          Ntemp->astnode.ident.name);
        continue;
      }

      if(hashtemp->variable == NULL)
      {
        fprintf(stderr,"Wow, hashtemp->variable is NULL!\n");
        continue;
      }

      /* check to see if this variable is also part of a common block */

      if(type_lookup(cur_common_table, Ntemp->astnode.ident.name))
      {
        fprintf(stderr,"Warning: can't handle COMMON vars w/DATA statements.\n");
        continue;
      }

      Ctemp = data_var_emit(Ntemp,Ctemp,hashtemp);
    }
  }
}

/*****************************************************************************
 *                                                                           *
 * data_implied_loop_emit                                                    *
 *                                                                           *
 * This function generates the code for implied do loops in DATA             *
 * statements.  The initialization is done in Java within a static           *
 * block.  For example, the following fortran statements:                    *
 *                                                                           *
 *    integer x                                                              *
 *    data (x(j),j=1,4)/5,6,7,8/                                             *
 *                                                                           *
 * would be emitted in Java as:                                              *
 *                                                                           *
 *    static int [] x= new int[(4)];                                         *
 *    static {                                                               *
 *    x[( 1 )- 1] = 5;                                                       *
 *    x[( 2 )- 1] = 6;                                                       *
 *    x[( 3 )- 1] = 7;                                                       *
 *    x[( 4 )- 1] = 8;                                                       *
 *    }                                                                      *
 *                                                                           *
 *****************************************************************************/

AST *
data_implied_loop_emit(AST * root, AST *Clist)
{
  AST * loop_var, * lhs;
  int start, stop, incr, i;
  HASHNODE *ht;

 
  if(gendebug) {
    printf("/* \n");
    printf("* looking at an implied data loop...\n");
    printf("*\n");
  }

  start = atoi(root->astnode.forloop.start->astnode.constant.number);

  if(gendebug)
    printf("* the start is: %d\n",start);

  stop = atoi(root->astnode.forloop.stop->astnode.constant.number);

  if(gendebug)
    printf("* the stop is: %d\n",stop);

  if(root->astnode.forloop.incr != NULL)
    incr = atoi(root->astnode.forloop.incr->astnode.constant.number);
  else
    incr = 1;

  if(gendebug)
    printf("* the increment is: %d\n",incr);

  loop_var = root->astnode.forloop.counter;

  if(gendebug)
    printf("* the name for the loop var is: %s\n", 
      loop_var->astnode.ident.name);

  lhs = root->astnode.forloop.Label;

  if(gendebug)
  { 
    AST *temp;
  
    printf("* the Lhs for this data stmt is: %s\n", 
      lhs->astnode.ident.name);

    printf("* lets see whats in Clist\n");
    for(temp=Clist;temp!=NULL;temp=temp->nextstmt)
      printf("* temp: %s\n", temp->astnode.constant.number);
  }

  ht = type_lookup(cur_type_table,lhs->astnode.ident.name);
  if(ht)
    lhs->vartype = ht->variable->vartype;
  else
    fprintf(stderr,"WARNING: [DATA] couldn't get vartype of '%s'\n", 
       lhs->astnode.ident.name);

  global_sub.name = loop_var->astnode.ident.name;

  /* emit the static initialization block */

  fprintf(curfp,"static {\n");
  for(i = start; i <= stop; i += incr)
  {
    global_sub.val = i;
    name_emit(lhs);
    fprintf(curfp, " = ");
    expr_emit(Clist);
    fprintf(curfp, ";\n");
    Clist = Clist->nextstmt;
    bytecode0(array_store_opcodes[ht->variable->vartype]);
  }
  fprintf(curfp,"}\n");

  if(gendebug)
    printf("*/ \n");

  global_sub.name = NULL;

  return Clist;
}

/*****************************************************************************
 *                                                                           *
 * data_var_emit                                                             *
 *                                                                           *
 * This function emits variable declarations for those variables             *
 * originally contained in DATA statements in the fortran source.            *
 *                                                                           *
 *****************************************************************************/

AST *
data_var_emit(AST *Ntemp, AST *Ctemp, HASHNODE *hashtemp)
{
  int length=1, is_array=FALSE, needs_dec = FALSE;

  if(gendebug)
    printf("VAR here we are emitting data for %s\n",
      Ntemp->astnode.ident.name);

  /* check to see whether we're going to be assigning to
   * an array element.  If so, the declaration for the array
   * would have already been emitted, so we dont need a
   * declaration here - just assign the value.  Otherwise,
   * we do need a declaration. 
   * (my gut feeling is that for bytecode generation, needs_dec
   *  is irrelevant.  we shall see.)
   */

  if(Ntemp->astnode.ident.arraylist == NULL)
    needs_dec = FALSE;
  else
    needs_dec = TRUE;

  /* here we determine whether this variable was declared as
   * an array or not.  hashtemp points to the symtable info.
   */
  if((hashtemp->variable->astnode.ident.arraylist != NULL ) && !needs_dec)
    is_array = TRUE;
  else
    is_array = FALSE;

  if( hashtemp->variable->astnode.ident.leaddim != NULL )
  {
    if(gendebug)
      printf("VAR leaddim not NULL\n");

    /* Check for attempts to initialize dummy argument.  we can't
     * determine the number of elements in a dummy arg. 
     */
    if(hashtemp->variable->astnode.ident.leaddim[0] == '*')
    {
      fprintf(stderr,"Attempt to initialize dummy argument: %s\n",
        hashtemp->variable->astnode.ident.name);
      return Ctemp;
    }
    else if (type_lookup(cur_args_table,Ntemp->astnode.ident.name))
    {
      fprintf(stderr,"Attempt to initialize argument: %s\n",
        hashtemp->variable->astnode.ident.name);
      return Ctemp;
    }
  }

  if(is_array)
  {
    /* determine how many elements are in this array so that
     * we know how many items from the DATA statement to assign
     * to this variable.
     */

    length = determine_var_length(hashtemp);

    if(gendebug)
      printf("VAR length = %d\n",length);

    fprintf(curfp,"static %s ", returnstring[ hashtemp->type]);

    if(gendebug)
      printf("VAR going to data_array_emit\n");

    Ctemp = data_array_emit(length, Ctemp, Ntemp, needs_dec);
  }
  else 
  {
    if(!needs_dec)
    {
      if(omitWrappers && !cgPassByRef(Ntemp->astnode.ident.name))
        fprintf(curfp,"static %s ", returnstring[ hashtemp->type]);
      else
        fprintf(curfp,"static %s ", wrapper_returns[ hashtemp->type]);

      data_scalar_emit(hashtemp->type, Ctemp, Ntemp, needs_dec);
    }
    else 
    {
      fprintf(curfp,"static {\n");
      data_scalar_emit(hashtemp->type, Ctemp, Ntemp, needs_dec);
      fprintf(curfp,"}\n");
    }

    Ctemp = Ctemp->nextstmt;
  }

  return Ctemp;
}

/*****************************************************************************
 * determine_var_length                                                      *
 *                                                                           *
 * Determine the number of elements in this array variable.                  *
 *                                                                           *
 *****************************************************************************/

int
determine_var_length(HASHNODE *var)
{
  AST *temp2;
  int length = 1;
  int dims = var->variable->astnode.ident.dim;

  if(gendebug) {
    printf("determining length of %s\n", var->variable->astnode.ident.name);
    printf("dim = %d\n", dims);
  }
 
  /* loop through each dimension of the array and evaluate it.
   * multiply the length of each dimension as we go.
   */

  temp2=var->variable->astnode.ident.arraylist;
  for( ; temp2 != NULL ; temp2=temp2->nextstmt ) {

    if(temp2->nodetype == ArrayIdxRange) {

      if(idxNeedsDecr(temp2))
        length *= (int)eval_const_expr(temp2->astnode.expression.rhs);
      else
        length *= (int)eval_const_expr(temp2->astnode.expression.rhs) + 1;

      if(gendebug)
        printf("VAR now length = %d\n", length);
    }
    else if(temp2->nodetype != Constant) {

      length = -1;
      break;
    }
    else {
      length *= atoi(temp2->astnode.constant.number);
    }
  }

  if(gendebug)
    printf("VAR returning length = %d\n", length);

  return length;
}

/*****************************************************************************
 *                                                                           *
 * data_array_emit                                                           *
 *                                                                           *
 * This function generates array declarations which are contained in         *
 * DATA statements.                                                          *
 *                                                                           *
 *****************************************************************************/

AST *
data_array_emit(int length, AST *Ctemp, AST *Ntemp, int needs_dec)
{
  unsigned int count = 1, size = 0;
  HASHNODE *ht;
  CPNODE *c;
  int i;

  if(gendebug)
    printf("VAR here we are in data_array_emit, length = %d\n",length);

  ht=type_lookup(cur_type_table, Ntemp->astnode.ident.name);
  if(!ht) {
    fprintf(stderr,"type table may be screwed.  Can't find '%s'.",
            Ntemp->astnode.ident.name);
    exit(-1);
  }

  fprintf(curfp,"[] ");

  /* 
   * if this variable is static, we can't declare it here 
   * because it has been declared already as a class variable.
   * so we use the "_temp_" prefix and emit the initialization.
   * later we assign the temp variable to the class variable.
   * 10/3/97  --Keith
   *
   * i think the above comment is out of date. there is really
   * no distinction between static/nonstatic anymore.  --kgs 5/15/00
   */

  fprintf(curfp,"%s = {\n",Ntemp->astnode.ident.name);

  /* for bytecode, we have to determine the number of elements
   * prior to emitting the elements themselves because we must
   * push the array size on the stack first.  if the length is
   * not known, we count the number of actual data items.
   * otherwise, we set the array size equal to the given length.  
   */
  if(length == -1) {
    AST *tmp;
    for(tmp = Ctemp;tmp != NULL;tmp=tmp->nextstmt)
      size++;
  }
  else
    size = length;
  
  pushIntConst(size);
  newarray_emit(ht->variable);

  for(i=0,count=0;(length==-1)?(Ctemp != NULL):(i< length);i++) {

    if(Ctemp->nodetype == Binaryop) 
      count = data_repeat_emit(Ctemp, count);
    else {
      bytecode0(jvm_dup);
      pushIntConst(count++);

      if(Ctemp->token == STRING) {
        fprintf(curfp,"\"%s\" ",Ctemp->astnode.constant.number);
        invoke_constructor(JL_STRING, Ctemp, STR_CONST_DESC);
      }
      else {
        fprintf(curfp,"%s ", Ctemp->astnode.constant.number);
        pushConst(Ctemp);
      }

      bytecode0(array_store_opcodes[ht->variable->vartype]);

      /* 
       * Every now and then, emit a newline for readability.
       * I have run across some lines that end up so long that
       * they screw up 'vi'.   9/30/97  --Keith 
       */
      if( (count+1) % 5 == 0 )
        fprintf(curfp,"\n");
    }

    if( (Ctemp = Ctemp->nextstmt) == NULL )
      break;
    else {
      if(length == -1)
      {
        if (Ctemp != NULL)
          fprintf(curfp,", ");
      }
      else 
        if(i != length -1 )
          fprintf(curfp,", ");
    }
  }
   
  fprintf(curfp,"};\n");

  c = newFieldref(cur_const_table,cur_filename,Ntemp->astnode.ident.name,
        field_descriptor[ht->variable->vartype][ht->variable->astnode.ident.dim]);

  bytecode1(jvm_putstatic, c->index);

  return Ctemp;
}

/*****************************************************************************
 *                                                                           *
 * data_repeat_emit                                                          *
 *                                                                           *
 * This function generates repeated DATA specifications, for example:        *
 *   INTEGER x(30)                                                           *
 *   DATA x/30*1/                                                            *
 *                                                                           *
 * For bytecode generation, we must keep track of which index we're emitting *
 * so we return the int value of the next array index to emit.               *
 *                                                                           *
 *****************************************************************************/

int
data_repeat_emit(AST *root, unsigned int idx)
{
  int j, repeat;
  char *ditem;
  
  if((root->astnode.expression.lhs == NULL) || 
     (root->astnode.expression.rhs == NULL))
  {
    fprintf(stderr,"Bad data statement!\n");
    exit(-1);
  }

  if((root->astnode.expression.lhs->nodetype != Constant) || 
     (root->astnode.expression.rhs->nodetype != Constant))
  {
    fprintf(stderr,"Error: Data items must be constants.\n");
    exit(-1);
  }

  repeat = atoi(root->astnode.expression.lhs->astnode.constant.number);
  ditem = root->astnode.expression.rhs->astnode.constant.number;

  /* emit the all but the last with a comma.. the last one without */

  for(j=0;j<repeat-1;j++) {
    fprintf(curfp,"%s, ", ditem);
    bytecode0(jvm_dup);
    pushIntConst(idx++);
    pushConst(root->astnode.expression.rhs);
    bytecode0(array_store_opcodes[root->astnode.expression.rhs->vartype]);
  }

  fprintf(curfp,"%s ", ditem);
  bytecode0(jvm_dup);
  pushIntConst(idx++);
  pushConst(root->astnode.expression.rhs);
  bytecode0(array_store_opcodes[root->astnode.expression.rhs->vartype]);

  return idx;
}

/*****************************************************************************
 *                                                                           *
 * data_scalar_emit                                                          *
 *                                                                           *
 * This function generates declarations of scalar items which are            *
 * contained in DATA statements.                                             *
 *                                                                           *
 *****************************************************************************/

void
data_scalar_emit(enum returntype type, AST *Ctemp, AST *Ntemp, int needs_dec)
{
  CPNODE *c;

  if(Ctemp->nodetype == Binaryop)
  {
    fprintf(stderr,"Attempt to assign more than one value to a scalar.\n");
    return;
  }

  if(Ctemp->token == STRING) 
  {
    HASHNODE *ht;
    int len;

    /* find this string in the symbol table */
    ht = type_lookup(cur_type_table,Ntemp->astnode.ident.name);

    /* determine the length of the string (as declared in the fortran source) */
    if(ht == NULL)
      len = 1;
    else
      len = Ntemp->astnode.ident.len;

    /* now initialize the string to all blanks.  but we try to keep the length
     * of the string constant, otherwise some subscript operations get screwed
     * up.  so we initialize the string to n blanks, where n is the original 
     * string length.
     * ..i dont think this code is working as described above.  however, it
     * doesn't seem to be hurting anything currently.  --kgs
     */

    if(!needs_dec)
    {
      /* assigning to a scalar element.  call invoke_constructor() to push
       * the new string object onto the stack and then emit a putstatic 
       * instruction to store it into the scalar variable.  we can safely
       * assume that it is not an argument to this program unit because
       * you cannot use the DATA statement to initialize an argument.
       */

      if(omitWrappers && !cgPassByRef(Ntemp->astnode.ident.name)) {
        fprintf(curfp,"%s = new String(\"%*s\");\n",
          Ntemp->astnode.ident.name, len,
          Ctemp->astnode.constant.number);

        invoke_constructor(JL_STRING, Ctemp, STR_CONST_DESC);
        c = newFieldref(cur_const_table,cur_filename,Ntemp->astnode.ident.name,
              field_descriptor[String][0]);
      }
      else {
        fprintf(curfp,"%s = new StringW(\"%*s\");\n",
          Ntemp->astnode.ident.name, len,
          Ctemp->astnode.constant.number);

        invoke_constructor(full_wrappername[type], Ctemp, STR_CONST_DESC);
        c = newFieldref(cur_const_table,cur_filename,Ntemp->astnode.ident.name,
              wrapped_field_descriptor[String][0]);
      }

      bytecode1(jvm_putstatic, c->index);
    }
    else
    {
      /* assigning to an array element.  first, call expr_emit() which will
       * push a reference to the array & the array index onto the stack.
       * then call invoke_constructor() to push a new string object onto
       * the stack.  finally, emit an array store instruction to store the
       * string into the array element.
       */

      expr_emit(Ntemp);
      fprintf(curfp," = \"%*s\";\n", len, Ctemp->astnode.constant.number);

      invoke_constructor(JL_STRING, Ctemp, STR_CONST_DESC);

      bytecode0(array_store_opcodes[Ntemp->vartype]);
    }
  }
  else 
  {
    /* this is not a string, so the declaration/initialization is
     * pretty straightforward.
     */

    if(!needs_dec)
    {
      /* as above in the string case, we are assigning to a scalar
       * variable, which we may safely assume is not an argument.
       * if it does not need to be wrapped, just push the constant
       * onto the stack.  otherwise, call invoke_constructor() to
       * create the appropriate wrapper object.
       */
      if(omitWrappers && !cgPassByRef(Ntemp->astnode.ident.name)) {
        fprintf(curfp,"%s = %s;\n",Ntemp->astnode.ident.name,
          Ctemp->astnode.constant.number);
        pushConst(Ctemp);
        c = newFieldref(cur_const_table,cur_filename,Ntemp->astnode.ident.name,
              field_descriptor[type][0]);
      }
      else {
        fprintf(curfp,"%s = new %s(%s);\n",Ntemp->astnode.ident.name,
          wrapper_returns[ type], Ctemp->astnode.constant.number);
        invoke_constructor(full_wrappername[type], Ctemp, wrapper_descriptor[type]);
        c = newFieldref(cur_const_table,cur_filename,Ntemp->astnode.ident.name,
              wrapped_field_descriptor[type][0]);
      }

      bytecode1(jvm_putstatic, c->index);
    }
    else
    {
      /* as above in string case, we are assigning to an array element.
       * the individual elements of an array are never wrapped, so we
       * just push the constant onto the stack and issue an array store
       * instruction.
       */
      expr_emit(Ntemp);
      fprintf(curfp," = %s;\n", Ctemp->astnode.constant.number);
      pushConst(Ctemp);
      bytecode0(array_store_opcodes[type]);
    }
  }
}

/*****************************************************************************
 *                                                                           *
 * invoke_constructor                                                        *
 *                                                                           *
 * invokes the <init> method of the given class constructor.  used for the   *
 * numeric & string classes (one-arg constructors).  the AST node 'constant' *
 * should represent a constant value of course (i.e. dont pass idents).      *
 *                                                                           *
 *****************************************************************************/

void
invoke_constructor(char *classname, AST *constant, char *desc)
{
  CPNODE *c;

  if(gendebug)
    printf("invoke_constructor(): classname = %s, constant = '%s'\n", 
           classname, constant->astnode.constant.number);

  c = cp_find_or_insert(cur_const_table,CONSTANT_Class, classname);

  bytecode1(jvm_new,c->index);
  bytecode0(jvm_dup);
  pushConst(constant);

  c = newMethodref(cur_const_table, classname, "<init>", desc);

  bytecode1(jvm_invokespecial, c->index);
}

/*****************************************************************************
 *                                                                           *
 * name_emit                                                                 *
 *                                                                           *
 * A name will either fly solo or lead off                                   *
 * a named array.  So far, this code will emit                               *
 * a name or an array with integer indices.  The                             *
 * procedure also needs to check all relevant tables                         *
 * to determine whether the name is an array or                              *
 * a procedure (i.e. Class.method) call, and whether                         *
 * the name is a STRING, CHAR, etc.  Frankly, this is                        *
 * a hideous procedure and really needs to                                   *
 * be rewritten.                                                             *
 *                                                                           *
 * ...and it's getting worse by the day  --Keith                             *
 *                                                                           *
 *  Heh... gotta love it...  -dmd  9/26/97                                   *
 *                                                                           *
 *  Started cleaning up name_emit  10/10/97  --Keith                         *
 *                                                                           *
 *****************************************************************************/


void
name_emit (AST * root)
{
  HASHNODE *hashtemp;
  char * tempname;

  if(gendebug)
    printf("entering name_emit\n");

  /*  
   *  Check to see whether name is in external table.  Names are
   *  loaded into the external table from the parser.   
   */

  if(root->nodetype == Identifier)
    if(root->token == STRING)
      printf("** string literal (this case should NOT be reached)\n");

  tempname = strdup(root->astnode.ident.name);
  uppercase(tempname);

  if(gendebug)
    if(type_lookup(cur_equiv_table, root->astnode.ident.name))
      printf("EQV %s is equivalenced\n",root->astnode.ident.name);

  /* 
   * If the name is in the external table, then check to see if
   * it is an intrinsic function instead (e.g. SQRT, ABS, etc).  
   */

  if (type_lookup (cur_external_table, root->astnode.ident.name) != NULL)
    external_emit(root);  /* handles LSAME, LSAMEN */
  else if(( methodscan (intrinsic_toks, tempname) != NULL) 
     && ( (type_lookup(cur_intrinsic_table, root->astnode.ident.name) != NULL)
       || (type_lookup(cur_type_table, root->astnode.ident.name) == NULL)))
    intrinsic_emit(root);
  else
    switch (root->token)
    {
      /* 
       * I think the first case (STRING/CHAR) is obsolete now since string 
       * and char constants were moved to the Constant production.  
       * 9/23/97, Keith 
       */

      case STRING:
      case CHAR:
        if(gendebug)
          printf("** emit String/char literal!  (should this case be reached?)\n");
        fprintf (curfp, "\"%s\"", root->astnode.constant.number);
        break;
      case INTRINSIC: 
        /* do nothing */
        break;
      case NAME:
      default:

        hashtemp = type_lookup (cur_array_table, root->astnode.ident.name);

        /* depending on whether this name is an array, scalar, substring,
         * or function/subroutine call, we call scalar_emit, array_emit,
         * substring_emit, or subcall_emit, respectively.
         */

        if (root->astnode.ident.arraylist == NULL)
          scalar_emit(root, hashtemp);
        else if (hashtemp != NULL)
          array_emit(root, hashtemp);
        else if (root->nodetype == Substring)
          substring_emit(root);
        else
          subcall_emit(root);
        break;
    }

  f2jfree(tempname,strlen(tempname)+1);
  if(gendebug)
    printf("leaving name_emit\n");
}

/*****************************************************************************
 *                                                                           *
 * substring_emit                                                            *
 *                                                                           *
 * This function emits substring operations.                                 * 
 *                                                                           *
 *****************************************************************************/

void
substring_emit(AST *root)
{
  HASHNODE *hashtemp;

  hashtemp = type_lookup (cur_array_table, root->astnode.ident.name);

  if(hashtemp)
    fprintf(stderr,"WARNING: substring on array element not supported.\n");

  scalar_emit(root, hashtemp);

  if((root->parent->nodetype == Assignment) && 
     (root->parent->astnode.assignment.lhs == root))
  {
    /* in this case we are assigning TO a substring, so we
     * do not want to generate the calls to substring() because
     * we will create a new string and assign it to this variable.
     */

    return;
  }

  fprintf(curfp,".substring(");

  return;
}

/*****************************************************************************
 *                                                                           *
 * subcall_emit                                                              *
 *                                                                           *
 *  This function emits a function call.  I think this function              *
 * is only called in cases where the function or subroutine is               *
 * not declared external or intrinsic and we dont know what                  *
 * else to do with it.                                                       *
 *                                                                           *
 *****************************************************************************/

void 
subcall_emit(AST *root)
{
  METHODREF *mref;
  AST *temp;
  char *tempstr, * t;
  char *desc;
  CPNODE *c;

  fprintf(stderr,"WARNING: undeclared function call: %s",
    root->astnode.ident.name);
  fprintf(stderr," (likely to be emitted wrong)\n");

  if(gendebug) {
    printf("@##@ in subcall_emit, %s\n",root->astnode.ident.name);

    if(type_lookup(cur_args_table, root->astnode.ident.name))
      printf("@@ calling passed-in func %s\n",root->astnode.ident.name);
  }

  /* captialize the first letter of the subroutine name to get the 
   * class name. 
   */

  tempstr = strdup (root->astnode.ident.name);
  *tempstr = toupper (*tempstr);

  mref = get_method_name(root, FALSE);

  /* mref should always be non-null, though i guess it's
   * possible that the elements may be null.
   */

  if((mref->classname != NULL) && (strlen(mref->classname) > 0)) {
    t = char_substitution(mref->classname, '/', '.');
    fprintf (curfp, "%s.%s", t, root->astnode.ident.name);
    f2jfree(t, strlen(t)+1);
  }
  else
    fprintf (curfp, "%s.%s", tempstr, root->astnode.ident.name);

  temp = root->astnode.ident.arraylist;
  desc = get_desc_from_arglist(temp);

  /* Loop through the argument list and emit each one. */

  fprintf (curfp, "(");
  if(temp->nodetype != EmptyArgList)
    for (; temp != NULL; temp = temp->nextstmt)
    {
      if(temp != root->astnode.ident.arraylist)
        fprintf (curfp, ",");  /* if not first iteration */
                        
      if (*temp->astnode.ident.name != '*')
        expr_emit (temp);
    }

  c = newMethodref(cur_const_table, get_full_classname(tempstr),
                   root->astnode.ident.name, desc);

  bytecode1(jvm_invokestatic, c->index);

  fprintf (curfp, ")");

  free_fieldref(mref);
}

/*****************************************************************************
 *                                                                           *
 * idxNeedsDecr                                                              *
 *                                                                           *
 * This function returns a boolean value depending on whether                *
 * the array pointed to by alist needs to have its index (dims)              *
 * decremented by one or not.  This allows arrays to start                   *
 * indexing at an arbitrary point.  If we recognize that the                 *
 * indexing starts at 0 then we dont have to decrement and we                *
 * return FALSE.  If indexing begins at 1 (the default in Fortran),          *
 * then we must decrement since Java indexing begins at 0.                   *
 *                                                                           *
 *****************************************************************************/

int
idxNeedsDecr(AST *alist)
{
  AST *startIdx = NULL;
  int eval;

  if( (alist != NULL) && (alist->nodetype == ArrayIdxRange))
  {
    if((startIdx = alist->astnode.expression.lhs) != NULL)
    {
      /* evaluate the start index.  we dont really care about the
       * end index at this point.
       */

      eval = (int)eval_const_expr(startIdx);
    
      if(gendebug)
        printf("VAR eval returns %d\n",eval);

      if(eval == 0)
        return FALSE;
      else if(eval == 1)
        return TRUE;
      else
        fprintf(stderr,"Can't handle array starting at arbitrary index\n");
    }
    else
      fprintf(stderr,"NULL lhs in array dec!\n");
  }
  return TRUE;
}

/*****************************************************************************
 *                                                                           *
 * func_array_emit                                                           *
 *                                                                           *
 * This function emits the index to an array.  The boolean argument          *
 * is_arg represents whether the array is an argument to the current         *
 * function or subroutine and the boolean is_ext represents whether          *
 * the array is being passed to an external function.                        *
 *                                                                           *
 *****************************************************************************/

void
func_array_emit(AST *root, HASHNODE *hashtemp, char *arrayname, int is_arg, 
  int is_ext)
{
  int needs_cast = FALSE;

  HASHNODE *ht;

  if(is_ext)
    fprintf (curfp, ",");
  else
    fprintf (curfp, "[");

  /* if the index is not an integer value, then it needs a cast to int.  for
   * bytecode generation, we cast the indices as we emit them, so a final
   * cast should not be necessary.
   */

  needs_cast = root->vartype != Integer;

  if(needs_cast)
    fprintf(curfp,"(int)(");

  if(gendebug)
    printf("~looking up %s in the array table\n", arrayname);

  /* find this variable in the array table */
  ht = type_lookup(cur_array_table, arrayname);

  if(ht == NULL)
  {
    if(gendebug)
      printf("~Could not find!\n");
  }
  else if(ht->variable->astnode.ident.dim == 3)
  {
    unsigned int d1, d0, offset;

    /* This section handles 3 dimensional array access.  we should already
     * know the dimensions of this array.
     */

    if(gendebug) {
      printf("~found %s, has dim %d\n",ht->variable->astnode.ident.name,
         ht->variable->astnode.ident.dim);

      printf("Ok, the dims are %d,%d,%d\n",
        ht->variable->astnode.ident.D[0],
        ht->variable->astnode.ident.D[1],
        ht->variable->astnode.ident.D[2]);
    }

    d0 = ht->variable->astnode.ident.D[0];
    d1 = ht->variable->astnode.ident.D[1];

    if(!idxNeedsDecr(ht->variable->astnode.ident.arraylist))
      d0 = ht->variable->astnode.ident.D[0] + 1;

    if(!idxNeedsDecr(ht->variable->astnode.ident.arraylist->nextstmt))
      d1 = ht->variable->astnode.ident.D[1] + 1;
        
    offset = 1 + ( (1 + d1) * d0);

    fprintf (curfp, "(");
    expr_emit(root);
    if(root->vartype != Integer)
      bytecode0(typeconv_matrix[root->vartype][Integer]);

    if(d0 != ht->variable->astnode.ident.D[0]) {
      fprintf (curfp, "+1");
      bytecode0(jvm_iconst_1);
      bytecode0(jvm_iadd);
    }
    fprintf (curfp, ")");
    
    fprintf (curfp, "+((");

    fprintf (curfp, "(");
    expr_emit(root->nextstmt);
    if(root->nextstmt->vartype != Integer)
      bytecode0(typeconv_matrix[root->nextstmt->vartype][Integer]);

    if(d1 != ht->variable->astnode.ident.D[1]) {
      fprintf (curfp, "+1");
      bytecode0(jvm_iconst_1);
      bytecode0(jvm_iadd);
    }
    fprintf (curfp, ")");
    
    fprintf (curfp, "+(");

    fprintf (curfp, "(");
    expr_emit(root->nextstmt->nextstmt);
    if(root->nextstmt->nextstmt->vartype != Integer)
      bytecode0(typeconv_matrix[root->nextstmt->nextstmt->vartype][Integer]);

    if(!idxNeedsDecr(ht->variable->astnode.ident.arraylist->nextstmt->nextstmt)) {
      fprintf (curfp, "+1");
      bytecode0(jvm_iconst_1);
      bytecode0(jvm_iadd);
    }
    fprintf (curfp, ")");
    
    fprintf (curfp, " * %d)) *%d) - %d", d1, d0,offset);

    pushIntConst(d1);
    bytecode0(jvm_imul);
    bytecode0(jvm_iadd);
    pushIntConst(d0);
    bytecode0(jvm_imul);
    bytecode0(jvm_iadd);
    pushIntConst(offset);
    bytecode0(jvm_isub);
  }
  else 
  {
    /* if this isn't a 3 dimensional array, it is handled here */

    int decrementIndex = idxNeedsDecr(ht->variable->astnode.ident.arraylist);

    fprintf (curfp, "(");
    expr_emit (root);

    if(root->vartype != Integer)
      bytecode0(typeconv_matrix[root->vartype][Integer]);

    if(decrementIndex) {
      fprintf (curfp, ")- 1");
      bytecode0(jvm_iconst_1);
      bytecode0(jvm_isub);
    }
    else
      fprintf (curfp, ")");

    if((hashtemp->variable->astnode.ident.lead_expr != NULL)
         && root->nextstmt != NULL)
    {
      root = root->nextstmt;
      decrementIndex = TRUE;

      if(ht->variable->astnode.ident.arraylist->nextstmt == NULL)
        fprintf(stderr,"Error: array %s doesn't have that many dimensions\n",
          root->astnode.ident.name);
      else
        decrementIndex = 
           idxNeedsDecr(ht->variable->astnode.ident.arraylist->nextstmt);

      fprintf (curfp, "+");
      fprintf (curfp, "(");
      expr_emit (root);
      if(root->vartype != Integer)
        bytecode0(typeconv_matrix[root->vartype][Integer]);

      if(decrementIndex) {
        fprintf (curfp, "- 1)");
        bytecode0(jvm_iconst_1);
        bytecode0(jvm_isub);
      }
      else
        fprintf (curfp, ")");

      fprintf (curfp, "* (");
      if(hashtemp->variable->astnode.ident.lead_expr->nodetype == ArrayIdxRange)
      {
        AST * lhs = hashtemp->variable->astnode.ident.lead_expr->astnode.expression.lhs;
        AST * rhs = hashtemp->variable->astnode.ident.lead_expr->astnode.expression.rhs;

        expr_emit(rhs);
        if(rhs->vartype != Integer)
          bytecode0(typeconv_matrix[rhs->vartype][Integer]);
        fprintf (curfp, " - ");
        expr_emit(lhs);
        if(lhs->vartype != Integer)
          bytecode0(typeconv_matrix[lhs->vartype][Integer]);
        fprintf (curfp, " + 1 ");
        bytecode0(jvm_isub);
        bytecode0(jvm_iconst_1);
        bytecode0(jvm_iadd);
      }
      else {
        AST * lead_exp = hashtemp->variable->astnode.ident.lead_expr;

printf("going to emit lead_exp...\n");
        expr_emit(lead_exp);
printf("done emitting lead_exp...\n");
        if(lead_exp->vartype != Integer)
          bytecode0(typeconv_matrix[lead_exp->vartype][Integer]);
      }

      fprintf (curfp, ")");

      bytecode0(jvm_imul);
      bytecode0(jvm_iadd);
    }
    else if((hashtemp->variable->astnode.ident.leaddim != NULL)
         && (hashtemp->variable->astnode.ident.leaddim[0] != '*')
         && (root->nextstmt != NULL))
    {
      root = root->nextstmt;
      decrementIndex = TRUE;

      if(ht->variable->astnode.ident.arraylist->nextstmt == NULL)
        fprintf(stderr,"Error: array %s doesn't have that many dimensions\n",
          root->astnode.ident.name);
      else
        decrementIndex = 
           idxNeedsDecr(ht->variable->astnode.ident.arraylist->nextstmt);

      fprintf (curfp, "+");
      fprintf (curfp, "(");
      expr_emit (root);
      if(root->vartype != Integer)
        bytecode0(typeconv_matrix[root->vartype][Integer]);

      if(decrementIndex) {
        fprintf (curfp, "- 1)");
        bytecode0(jvm_iconst_1);
        bytecode0(jvm_isub);
      }
      else
        fprintf (curfp, ")");

      fprintf (curfp, "*");

      if(gendebug)
        printf("leaddim = %s\n",hashtemp->variable->astnode.ident.leaddim);

      ht = type_lookup(cur_type_table, hashtemp->variable->astnode.ident.leaddim);

      if(isalpha((int) hashtemp->variable->astnode.ident.leaddim[0])) {

        /* ht should be non-NULL here. */
        if(!ht) {
          fprintf(stderr,"func_array_emit(): Type table is screwed!\n");
          fprintf(stderr,"   looked up %s\n",hashtemp->variable->astnode.ident.leaddim);
          exit(-1);
        }

        if(omitWrappers && !cgPassByRef(hashtemp->variable->astnode.ident.leaddim)) {
          fprintf(curfp,  "%s", hashtemp->variable->astnode.ident.leaddim);
          pushVar(ht->variable->vartype,is_arg,cur_filename,
                  hashtemp->variable->astnode.ident.leaddim,
                  field_descriptor[ht->variable->vartype][0],
                  ht->variable->astnode.ident.localvnum, FALSE);
        }
        else {
          fprintf(curfp,  "%s.val", hashtemp->variable->astnode.ident.leaddim);
          pushVar(ht->variable->vartype,is_arg,cur_filename,
                  hashtemp->variable->astnode.ident.leaddim,
                  field_descriptor[ht->variable->vartype][0],
                  ht->variable->astnode.ident.localvnum, TRUE);
        }
      }
      else {
        fprintf(curfp,  "%s", hashtemp->variable->astnode.ident.leaddim);
        pushIntConst(atoi(hashtemp->variable->astnode.ident.leaddim));
      }
      bytecode0(jvm_imul);
      bytecode0(jvm_iadd);
    }  /* Multi dimension.  */
  }

  if(is_arg) {
    int varnum;

    fprintf(curfp,  "+ _%s_offset",arrayname);

    /* locate the array's symtable entry and assign the varnum
     * of the offset arg to be one greater than the array's varnum.
     */
    ht = type_lookup(cur_type_table, arrayname);
    if(!ht) {
      fprintf(stderr,"WARNING: type table screwed.");
      fprintf(stderr,"  looking for localvarnum for '_%s_offset'\n",
        arrayname);
      varnum = 1;
    }
    else
      varnum = ht->variable->astnode.ident.localvnum + 1;

    pushVar(Integer,is_arg,cur_filename,
            "dummy string...is this significant?",
            "I", varnum , FALSE);
    bytecode0(jvm_iadd);
  }

  if(needs_cast)
    fprintf(curfp,")");

  if(!is_ext) {
    fprintf(curfp, "]");
  }
}

/*****************************************************************************
 *                                                                           *
 * cgPassByRef                                                               *
 *                                                                           *
 * wrapper around isPassByRef() for codegen routines.   this is just to      *
 * make the code a bit more compact.  we could have used a #define but they  *
 * can be annoying sometimes.                                                *
 *                                                                           *
 *****************************************************************************/

int
cgPassByRef(char *name)
{
  return isPassByRef(name, cur_type_table, cur_common_table, 
    cur_external_table);
}

/*****************************************************************************
 *                                                                           *
 * isPassByRef                                                               *
 *                                                                           *
 * Given the name of a variable, this function returns                       *
 * TRUE if the variable is passed by reference, FALSE                        *
 * otherwise.  Generally, being passed by reference                          *
 * means that the variable will be wrapped in an object.                     *
 *                                                                           *
 *****************************************************************************/

int
isPassByRef(char *name, SYMTABLE *ttable, SYMTABLE *ctable, SYMTABLE *etable)
{
  HASHNODE *ht, *ht2, *ht3;
  char *blockName;
  int pos, i;
  AST *temp;

  /* First look up the variable name in the main hash table. */

  ht = type_lookup(ttable,name);
  if(ht) {

    if(gendebug)
      printf("isPassByRef(): found '%s' in type table\n", name);

    if(ht->variable->nodetype != Identifier) {
      fprintf(stderr,"isPassByRef():  non-ident node found (%s).\n", name);
      return FALSE;
    }

    if(ht->variable->astnode.ident.passByRef)
    {
      /* simple case.  if the variable is tagged as pass-by-reference
       * in the hash table, then return TRUE.
       */
      if(gendebug)
        printf("isPassByRef(): '%s' is tagged pass-by-ref\n", name);

      return TRUE;
    }
    else {
      /* otherwise, we look up the variable name in the table of
       * COMMON variables.
       */
      if(gendebug)
        printf("isPassByRef(): '%s' is not tagged pass-by-ref\n", name);

      ht2 = type_lookup(ctable,name);
      if(ht2) {

        /* since different declarations of the same common block
         * may use different variable names for the members, we
         * use the position of the variable in the common block
         * to look up the actual variable.
         */

        pos = ht2->variable->astnode.ident.position;
        blockName = ht2->variable->astnode.ident.commonBlockName;

        ht3 = type_lookup(global_common_table, blockName);
        if(ht3) {
          
          /* after getting a pointer to the common block, we loop
           * through the entries until we get to the Nth entry, where
           * N = pos, or until the pointer is NULL.
           */
 
          i = 0;
          temp = ht3->variable->astnode.common.nlist;

          while((i < pos) && (temp != NULL)) {
            i++;
            temp = temp->nextstmt;
          }

          if(temp != NULL)
            return temp->astnode.ident.passByRef;
          else
            fprintf(stderr,"isPassByRef(): mismatch in common block size\n");
        }
        else
          fprintf(stderr, "isPassByRef(): cant find common block %s\n",
            blockName);

        return TRUE;
      }
      else {
        return FALSE;
      }
    }
  }
  else if(type_lookup(etable, name)) {
    if(gendebug)
      printf("isPassByRef(): '%s' not found in type table, but found in external table\n", name);

    return FALSE;
  }
  else {
    fprintf(stderr,"isPassByRef(): variable %s not found.\n", name);
    return TRUE;
  }

  return TRUE;
}

/*****************************************************************************
 *                                                                           *
 * array_emit                                                                *
 *                                                                           *
 * Here we emit array variables.  actually we first determine                *
 * the context in which the array access is found and then call              *
 * func_array_emit() to emit the array index.                                *
 * 10/10/97 --Keith                                                          *
 *                                                                           *
 *****************************************************************************/

void
array_emit(AST *root, HASHNODE *hashtemp)
{
  AST *temp;
  struct var_info *arrayinf;

  if (gendebug)
    printf ("Array... %s, My node type is %s\n", 
      root->astnode.ident.name,
      print_nodetype(root));

  arrayinf = push_array_var(root);

  temp = root->astnode.ident.arraylist;

  if(root->parent == NULL) {

    /* Under normal circumstances, I dont think this should 
     * be reached.
     */

    fprintf (stderr,"Array... %s, NO PARENT - ", arrayinf->name);
    fprintf (stderr,"This is not good!\n");
  } else {
    if(gendebug)
      printf ("Array... %s, Parent node type... %s\n", 
        arrayinf->name, print_nodetype(root->parent));
    printf ("Array... %s, Parent node type... %s\n", 
      arrayinf->name, print_nodetype(root->parent));

    if((root->parent->nodetype == Call)) 
    {
      /* following is a LAPACK specific hack.  we dont want to treat
       * calls to LSAME or LSAMEN as real external calls since we
       * translate them to inline expressions.    3-9-98 -- Keith
       */

      if((type_lookup(cur_external_table, root->parent->astnode.ident.name) 
       && strcmp(root->parent->astnode.ident.name,"lsame") 
       && strcmp(root->parent->astnode.ident.name,"lsamen"))
       && !type_lookup(cur_args_table,root->parent->astnode.ident.name) )
      {
        func_array_emit(temp, hashtemp, root->astnode.ident.name, 
           arrayinf->is_arg, TRUE);
      }
      else {
        func_array_emit(temp, hashtemp, root->astnode.ident.name, 
           arrayinf->is_arg,FALSE);
        bytecode0(array_load_opcodes[root->vartype]);
      }
    }
    else if(((root->parent->nodetype == Assignment) &&
             (root->parent->astnode.assignment.lhs == root)) ||
            (root->parent->nodetype == DataStmt) ||
            (root->parent->nodetype == DataImpliedLoop))
    {
      func_array_emit(temp, hashtemp, root->astnode.ident.name, 
         arrayinf->is_arg, FALSE);
    }
    else if((root->parent->nodetype == Typedec)) 
    {
      /*  Just a declaration, don't emit index. */
      if(gendebug)
        printf("I guess this is just an array declaration\n");
    }
    else {
      func_array_emit(temp, hashtemp, root->astnode.ident.name, 
         arrayinf->is_arg, FALSE);
      bytecode0(array_load_opcodes[root->vartype]);
    }
  }

  free_var_info(arrayinf);
}

/*****************************************************************************
 *                                                                           *
 * push_array_var                                                            *
 *                                                                           *
 * this function pushes a reference to the array variable onto the stack.    *
 *                                                                           *
 *****************************************************************************/

struct var_info *
push_array_var(AST *root)
{
  struct var_info *ainf;

  ainf = get_var_info(root);

  /* 
   * Now, what needs to happen here is the context of the
   * array needs to be determined.  If the array is being
   * passed as a parameter to a method, then the array index
   * needs to be passed separately and the array passed as
   * itself.  If not, then an array value is being set,
   * so dereference with index arithmetic.  
   */

  if((root->parent != NULL) && (root->parent->nodetype == Typedec))
    fprintf (curfp, "%s", ainf->name);   /* for typedec, generate no bytecode */
  else {
    char *com_prefix;

    com_prefix = get_common_prefix(root->astnode.ident.name);

    fprintf (curfp, "%s%s", com_prefix, ainf->name);
    pushVar(root->vartype, ainf->is_arg, ainf->class, ainf->name,
        ainf->desc, ainf->localvar, FALSE);
  }

  if(gendebug)
    printf("push_array_var(%s) - '%s' -> %d\n", cur_filename,
       root->astnode.ident.name, ainf->localvar);

  return ainf;
}

/*****************************************************************************
 *                                                                           *
 * get_var_info                                                              *
 *                                                                           *
 * this function returns information about an identifier (name, desc, etc).  *
 *                                                                           *
 *****************************************************************************/

struct var_info *
get_var_info(AST *root)
{
  int is_arg=FALSE;
  unsigned int varnum=0;
  char *com_prefix;
  char *name, *tmpclass, *desc;
  HASHNODE *ht;
  struct var_info *new_array_inf;

  new_array_inf = (struct var_info *)f2jalloc(sizeof(struct var_info));

  /* find the descriptor & local var number (if applicable) for this var   */

  if((ht = type_lookup(cur_type_table, root->astnode.ident.name)) != NULL) {
    desc = getVarDescriptor(ht->variable);
    varnum = ht->variable->astnode.ident.localvnum;
  }
  else {
    fprintf(stderr,"WARNING: get_var_info() '%s' not in hash table!\n",
      root->astnode.ident.name);
    desc = "asdfjkl";
  }

  /* If this is a COMMON variable, get the prefix for the common
   * class name.
   */

  com_prefix = get_common_prefix(root->astnode.ident.name);
  tmpclass = cur_filename;
  name = root->astnode.ident.name;

  if(com_prefix[0] != '\0')
  {
    /* if this is a COMMON variable, find out the merged
     * name, if any, that we should use instead.  Names are
     * merged when different declarations of a common
     * block use different variable names.
     */

    ht = type_lookup(cur_type_table,root->astnode.ident.name);
    if (ht == NULL)
      fprintf(stderr,"get_var_info:Cant find %s in type_table\n",
          root->astnode.ident.name);

    if(ht->variable->astnode.ident.merged_name != NULL)
      name = ht->variable->astnode.ident.merged_name;

    /* tmpclass = strdup(com_prefix); */
    tmpclass = get_full_classname(com_prefix);
    tmpclass[strlen(tmpclass)-1] = '\0';
  }

  /* if this is an equivalenced variable, find out the merged
   * name that we should use instead.  Equivalenced names are
   * always merged.
   */

  if((ht = type_lookup(cur_equiv_table,root->astnode.ident.name)))
    name = ht->variable->astnode.ident.merged_name;

  if (name == NULL)
  {
    fprintf(stderr,"get_var_info: setting name to NULL!\n");
    name = root->astnode.ident.name;
  }

  if(gendebug)
    printf("### #in get_var_info, setting name = %s\n",name);

  /* Determine whether this variable is an argument to the current
   * program unit.
   */

  if( type_lookup(cur_args_table,root->astnode.ident.name) != NULL )
    is_arg = TRUE;
  else
    is_arg = FALSE;

  new_array_inf->name = strdup(name);
  new_array_inf->desc = strdup(desc);
  new_array_inf->localvar = varnum;
  new_array_inf->is_arg = is_arg;
  new_array_inf->class = strdup(tmpclass);

  return new_array_inf;
}

/*****************************************************************************
 *                                                                           *
 * get_common_prefix                                                         *
 *                                                                           *
 * If the variable is in a common block, this function returns the name of   *
 * the class file in which it is declared.  Otherwise, it returns a blank    *
 * string.                                                                   *
 *                                                                           *
 *****************************************************************************/


char *
get_common_prefix(char *varname)
{
  HASHNODE *ht;
  char * inf = strdup(inputfilename);
  char * prefix = strtok(inf,".");
  static char * cprefix;

  /* Look up this variable name in the table of COMMON variables */

  ht = type_lookup(cur_common_table, varname);

  if(ht == NULL)
    cprefix = strdup("");  /* dup so we can free() later */
  else {
    cprefix = (char *) f2jalloc(
       strlen(ht->variable->astnode.ident.commonBlockName) +
       strlen(prefix) + 3);

    sprintf(cprefix,"%s_%s.", prefix,
      ht->variable->astnode.ident.commonBlockName);
  }

  f2jfree(inf, strlen(inf)+1);
  return(cprefix);
}

/*****************************************************************************
 *                                                                           *
 * getVarDescriptor                                                          *
 *                                                                           *
 * Returns the descriptor for this variable.                                 *
 *                                                                           *
 *****************************************************************************/

char *
getVarDescriptor(AST *root)
{

  if(omitWrappers && !cgPassByRef(root->astnode.ident.name))
    return field_descriptor[root->vartype][root->astnode.ident.dim];
  else
    return wrapped_field_descriptor[root->vartype][root->astnode.ident.dim];
}

/*****************************************************************************
 *                                                                           *
 * pushConst                                                                 *
 *                                                                           *
 * this function pushes the constant value pointed to by root onto the       *
 * jvm stack.                                                                *
 *                                                                           *
 *****************************************************************************/

void
pushConst(AST *root) {
  switch(root->token) {
    case INTEGER:
      pushIntConst(atoi(root->astnode.constant.number));
      break;
    case EXPONENTIAL:
    case DOUBLE:
      pushDoubleConst(atof(root->astnode.constant.number));
      break;
    case TrUE:   /* dont expect to find booleans anyway, so dont try */
      bytecode0(jvm_iconst_1);
      break;
    case FaLSE:
      bytecode0(jvm_iconst_0);
      break;
    case STRING:
      pushStringConst(root->astnode.constant.number);
      break;
    default:
      break;
  }
}

/*****************************************************************************
 *                                                                           *
 * pushIntConst                                                              *
 *                                                                           *
 * pushes an integer constant onto the stack.                                *
 *                                                                           *
 *****************************************************************************/

void
pushIntConst(int ival)
{
  CPNODE *ct;

  ct=cp_find_or_insert(cur_const_table,CONSTANT_Integer,(void*)&ival);

  if(ct) {
    if(ct->index > CPIDX_MAX)
      bytecode1(jvm_ldc_w,ct->index);
    else
      bytecode1(jvm_ldc,ct->index);
  } else {   /* not found, use literal */
    if((ival < JVM_SHORT_MIN) || (ival > JVM_SHORT_MAX)) {
      fprintf(stderr,"WARNING:expr_emit() bad int literal: %d\n", ival);
      return;
    }
    else if((ival < JVM_BYTE_MIN) || (ival > JVM_BYTE_MAX))
      bytecode1(jvm_sipush, ival);
    else if((ival < JVM_ICONST_MIN) || (ival > JVM_ICONST_MAX))
      bytecode1(jvm_bipush, ival);
    else
      bytecode0(iconst_opcodes[ival+1]);
  }
}

/*****************************************************************************
 *                                                                           *
 * pushDoubleConst                                                           *
 *                                                                           *
 * pushes a double constant onto the stack.                                  *
 *                                                                           *
 *****************************************************************************/

void
pushDoubleConst(double dval)
{
  CPNODE *ct;

  ct=cp_find_or_insert(cur_const_table,CONSTANT_Double,(void*)&dval);

  if(ct)
    bytecode1(jvm_ldc2_w, ct->index);
  else if(dval == 0.0)
    bytecode0(jvm_dconst_0);
  else if(dval == 1.0)
    bytecode0(jvm_dconst_1);
  else
    fprintf(stderr,"WARNING: bad double-prec literal in expr_emit()\n");
}

/*****************************************************************************
 *                                                                           *
 * pushStringConst                                                           *
 *                                                                           *
 * pushes a string constant onto the stack.                                  *
 *                                                                           *
 *****************************************************************************/

void
pushStringConst(char *str)
{
  CPNODE *ct;

  ct=cp_find_or_insert(cur_const_table,CONSTANT_String, (void*)str);

  if(ct->index > CPIDX_MAX)
    bytecode1(jvm_ldc_w, ct->index);
  else
    bytecode1(jvm_ldc, ct->index);
}

/*****************************************************************************
 *                                                                           *
 * pushVar                                                                   *
 *                                                                           *
 * pushes a local variable or field onto the stack.                          *
 *                                                                           *
 *****************************************************************************/

void
pushVar(enum returntype vt, BOOLEAN isArg, char *class, char *name, char *desc, 
   unsigned int lv, BOOLEAN deref)
{
  CPNODE *c;

  if(gendebug) {
    printf("in pushvar, vartype is %s\n", returnstring[vt]);
    printf("               desc is %s\n", desc);
    printf("       local varnum is %d\n", lv);
  }

  if(isArg) {
    /* for reference types, always use aload */
    if((desc[0] == 'L') || (desc[0] == '['))
      gen_load_op(lv, Object);
    else
      gen_load_op(lv, vt);
  }
  else {
    c = newFieldref(cur_const_table, class, name, desc);
    bytecode1(jvm_getstatic, c->index);
  }

  if(deref) {
    c = newFieldref(cur_const_table, full_wrappername[vt], "val", 
           val_descriptor[vt]);
    bytecode1(jvm_getfield, c->index);
  }
}

/*****************************************************************************
 *                                                                           *
 * scalar_emit                                                               *
 *                                                                           *
 * This function emits a scalar variable.  The first thing that needs        *
 * to be checked here is whether the variable is part of a common block.     *
 * If so, we need to emit the common block name followed by a dot and        *
 * the variable name.  Otherwise, just emit the variable name.   If using    *
 * object wrappers, the nodetype of the parent node must be checked.  If the *
 * parent node is a 'call' to an external function then the variables must   *
 * be passed as objects.  Otherwise, the value from the wrapper should be    *
 * obtained by appending .val to the variable name.   10/10/97  -- Keith     *
 *                                                                           *
 * (note: this function also emits array variables which do not have         *
 *  indices since they look like scalars to the parser)                      *
 *                                                                           *
 *****************************************************************************/

void
scalar_emit(AST *root, HASHNODE *hashtemp)
{
  char *com_prefix, *desc, *name, *scalar_class;
  HASHNODE *ht, *isArg, *typenode;


  /* determine descriptor */
  if((typenode = type_lookup(cur_type_table, root->astnode.ident.name)) != NULL)
    desc = getVarDescriptor(typenode->variable);
  else {
    fprintf(stderr,"ERROR: can't find '%s' in hash table\n", 
       root->astnode.ident.name);
    exit(-1);
  }

  printf("in scalar_emit, name = %s, desc = %s\n",root->astnode.ident.name, desc);

  /* get the name of the common block class file, if applicable */

  com_prefix = get_common_prefix(root->astnode.ident.name);

  name = root->astnode.ident.name;

  isArg = type_lookup(cur_args_table,name);

  if(com_prefix[0] != '\0')
  {
    /* if this is a COMMON variable, find out the merged
     * name, if any, that we should use instead.  Names are
     * merged when different declarations of a common
     * block use different variable names.
     */

    ht = type_lookup(cur_type_table,root->astnode.ident.name);
    if (ht == NULL)
      fprintf(stderr,"scalar_emit:Cant find %s in type_table\n",
          root->astnode.ident.name);
    else if(ht->variable->astnode.ident.merged_name != NULL)
      name = ht->variable->astnode.ident.merged_name;

    scalar_class = get_full_classname(com_prefix);
    scalar_class[strlen(scalar_class)-1] = '\0';
  }
  else
    scalar_class = strdup(cur_filename);

  if(gendebug)
    printf("scalar_emit: scalar_class is '%s'\n",scalar_class);

  /* if this is an equivalenced variable, find out the merged
   * name that we should use instead.  Equivalenced names are
   * always merged.
   */

  if((ht = type_lookup(cur_equiv_table,root->astnode.ident.name))) {
    name = ht->variable->astnode.ident.merged_name;
    printf("%s -> %s\n",root->astnode.ident.name,name);
  }

  if (name == NULL)
  {
    fprintf(stderr,"scalar_emit: name was NULL!\n");
    name = root->astnode.ident.name;
  }

  if(hashtemp == NULL) {
    /* if hashtemp is NULL, then this variable is not in the
     * array table (i.e. it is not an array).
     */

    if(gendebug) {
      printf("here we are emitting a scalar: %s, len = %d, ",
        root->astnode.ident.name, root->astnode.ident.len);
      printf("The parent node is : %s\n",print_nodetype(root->parent));
    }
 
    if(gendebug)
      printf("### #in scalar_emit, setting name = %s\n",name);

    if(root->parent == NULL) {
      /* not good. */
      fprintf(stderr,"scalar_emit(): NO PARENT! (%s)\n", name);
    } else {
      if (root->parent->nodetype == Call) {
        char *tempname;

        if(gendebug)
          printf("in scalar_emit CALL, '%s' <- '%s'\n", 
            root->parent->astnode.ident.name,
            name);

        tempname = strdup(root->parent->astnode.ident.name);
        uppercase(tempname);

        /* Determine whether the parent (a call) is an intrinsic or an 
         * array access.  If neither, we pass the scalar as is - wrapped
         * in an object if necessary.  This provides the ability to simulate 
         * pass by reference in Java.  If the parent is either an intrinsic 
         * function call or an array access, we must pass the actual value.
         * Fortran intrinsics are implemented using functions from the core
         * Java API which only take primitive types as arguments.  And arrays
         * must always be indexed using primitive integers.  Therefore, in
         * those two cases, we must emit the primitive value, in some cases
         * obtained by appending ".val" to the wrapper object.
         */

        if((methodscan (intrinsic_toks, tempname) == NULL) &&
           (type_lookup(cur_array_table, root->parent->astnode.ident.name) == NULL))
        {
          /* parent is not a call to an intrinsic and not an array access */

          if(gendebug)
            printf("did not find %s in intrinsics table\n",
               root->parent->astnode.ident.name);

          fprintf (curfp, "%s%s", com_prefix, name);

          pushVar(root->vartype, isArg!=NULL, scalar_class, name, desc,
             typenode->variable->astnode.ident.localvnum, FALSE);
        }
        else
        {
          if(gendebug)
            printf("found %s in intrinsics or array table\n",
               root->parent->astnode.ident.name);

          if(omitWrappers && !cgPassByRef(root->astnode.ident.name)) {
            fprintf (curfp, "%s%s", com_prefix,name);
            pushVar(root->vartype, isArg!=NULL, scalar_class, name, desc,
               typenode->variable->astnode.ident.localvnum, FALSE);
          }
          else {
            fprintf (curfp, "%s%s.val", com_prefix,name);
            pushVar(root->vartype, isArg!=NULL, scalar_class, name, desc,
               typenode->variable->astnode.ident.localvnum, TRUE);
          }
        }

        f2jfree(tempname, strlen(tempname)+1);
      }
      else if(root->parent->nodetype == Typedec) {

        /* Parent is a type declaration - just emit the name itself.
         *
         * For bytecode generation, nothing needs to be done here
         * because insert_fields() handles all typedecs.
         */

        if(gendebug)
          printf("Emitting typedec name: %s\n", name);
        fprintf (curfp, "%s", name);
      }
      else if(root->parent->nodetype == Equivalence) {

        /* Parent is an EQUIVALENCE statement.  This is handled the 
         * same as a type declaration, except we emit the merged name.
         *
         * Nothing needs to be done here for bytecode generation.
         */

        if(gendebug)
          printf("Emitting equivalenced name: %s\n", 
             root->astnode.ident.merged_name);
        fprintf (curfp, "%s", root->astnode.ident.merged_name);
      }
      else if(root->parent->nodetype == ArrayDec) {

        /* Parent is an array declaration, but we know that the
         * variable we're emitting is not an array, so this must
         * be the size of the array.
         *
         * Nothing needs to be done here for bytecode generation.
         */

        if(omitWrappers && !cgPassByRef(root->astnode.ident.name))
          fprintf (curfp, "%s%s", com_prefix, name);
        else
          fprintf (curfp, "%s%s.val", com_prefix, name);
      }
      else if((root->parent->nodetype == Assignment) &&
              (root->parent->astnode.assignment.lhs == root)) {
        /* this is the LHS of some assignment.  this is only an
         * issue for bytecode generation since we don't want to
         * generate a load instruction for the LHS of an assignment.
         * for Java source, generate as usual.
         */

        if((global_sub.name != NULL) && 
            !strcmp(global_sub.name, name))
          fprintf (curfp, " %d ", global_sub.val);
        else {
          if(omitWrappers && !cgPassByRef(root->astnode.ident.name))
            fprintf (curfp, "%s%s", com_prefix, name);
          else {
            fprintf (curfp, "%s%s.val", com_prefix, name);
            pushVar(root->vartype, isArg!=NULL, scalar_class, name, desc,
               typenode->variable->astnode.ident.localvnum, FALSE);
          }
        }
      }
      else {
       
        /* General case - just generate the name, with the 
         * .val suffix if applicable.  the global_sub stuff is
         * for implied DO loops in data statements.  in that
         * case, we dont want to actually emit a variable name,
         * so we substitute its corresponding number.
         */

        if((global_sub.name != NULL) && 
            !strcmp(global_sub.name, name))
        {
          fprintf (curfp, " %d ", global_sub.val);
          
          pushIntConst(global_sub.val);
        }
        else {
          if(omitWrappers && !cgPassByRef(root->astnode.ident.name)) {
            fprintf (curfp, "%s%s", com_prefix, name);
            pushVar(root->vartype, isArg!=NULL, scalar_class, name, desc,
               typenode->variable->astnode.ident.localvnum, FALSE);
          }
          else {
            fprintf (curfp, "%s%s.val", com_prefix, name);
            pushVar(root->vartype, isArg!=NULL, scalar_class, name, desc,
               typenode->variable->astnode.ident.localvnum, TRUE);
          }
        }
      }
    }
  }
  else 
  {
    /* 
     * if we reach this case, we are emitting an array, but there
     * is no index specified.  Normally, we would just emit the variable
     * name, but we must also check the parent nodetype.  If it is a
     * call to an external function, then we have to emit the variable
     * name followed by ",0" to signify that the offset into this array
     * is 0.   10/10/97  --Keith 
     */

    if(root->parent == NULL) 
    {
      fprintf(stderr,"scalar_emit(): NO PARENT!\n");
    } 
    else 
    {
      if(gendebug) {
        printf("here we are emitting a scalar: %s,",name);
        printf("The parent node is : %s\n",print_nodetype(root->parent));
      }

      if(root->parent->nodetype == Call)
      {
        if(type_lookup(cur_args_table, root->parent->astnode.ident.name) &&
            !type_lookup(cur_type_table, root->parent->astnode.ident.name)) {
          /* if the parent is a subroutine passed as an arg to this function,
           * then we do not append the offset.
           */
          fprintf (curfp, "%s%s", com_prefix, name);
          pushVar(root->vartype, isArg!=NULL, scalar_class, name, desc,
             typenode->variable->astnode.ident.localvnum, FALSE);
        }
        else if(type_lookup(cur_args_table,root->astnode.ident.name)) {
          fprintf (curfp, "%s%s,_%s_offset", com_prefix, name, name);
          pushVar(root->vartype, isArg!=NULL, scalar_class, name, desc,
             typenode->variable->astnode.ident.localvnum, FALSE);
          gen_load_op(typenode->variable->astnode.ident.localvnum + 1, Integer);
        }
        else {
          fprintf (curfp, "%s%s,0", com_prefix, name);
          pushVar(root->vartype, isArg!=NULL, scalar_class, name, desc,
             typenode->variable->astnode.ident.localvnum, FALSE);
          bytecode0(jvm_iconst_0);
        }
      }
      else if((root->parent->nodetype == Assignment) &&
              (root->parent->astnode.assignment.lhs == root)) {
        /* LHS of assignment.  do not generate any bytecode. */
        fprintf (curfp, "%s%s", com_prefix, name);
      }
      else {
        fprintf (curfp, "%s%s", com_prefix, name);
        pushVar(root->vartype, isArg!=NULL, scalar_class, name, desc,
           typenode->variable->astnode.ident.localvnum, FALSE);
      }
    }
  }

  f2jfree(scalar_class, strlen(scalar_class)+1);
  f2jfree(com_prefix, strlen(com_prefix)+1);
}

/*****************************************************************************
 *                                                                           *
 * external_emit                                                             *
 *                                                                           *
 * This function translates calls to external functions.  First,             *
 * check whether we are translating a call to LSAME or LSAMEN.               *
 * LSAME is from BLAS and LSAMEN is from LAPACK.  Instead of translating     *
 * the actual files lsame.f and lsamen.f to java, we just translate          *
 * the calls to equivalent java method calls (String.equalsIgnoreCase        *
 * and String.regionMatches respectively).   If we're not translating        *
 * a call to LSAME or LSAMEN, use the function call_emit().  --Keith         *
 *                                                                           *
 * changing equalsIgnoreCase() to a character comparison since the           *
 * LAPACK routine only compares the first character.  12/4/97 --Keith        *
 *                                                                           *
 *****************************************************************************/

void
external_emit(AST *root)
{
  char *tempname, *javaname;
  METHODTAB *entry;
  AST *temp;
  CPNODE *c;

  if(gendebug) {
    printf("here we are in external_emit (%s)\n", root->astnode.ident.name);
    printf("nodetype = %s, parent nodetype = %s\n",
      print_nodetype(root),print_nodetype(root->parent));
  }

  /*
   * If we encounter this external variable within a
   * function/subroutine call, but the name itself is not
   * being used as a call, then we know that the function
   * is being passed as a parameter.
   */

  if( (root->parent->nodetype == Call) && 
      (root->astnode.ident.arraylist == NULL))
  {
    HASHNODE *ht;

    if(gendebug)
      printf("unit %s: EXTERNAL has parent CALL\n", unit_name);
   
    tempname = strdup(root->astnode.ident.name);
    *tempname = toupper(*tempname);

    /* if this external function is also an argument to the
     * current unit, we already have an Object reference to
     * it, so just pass that.  If not, we create a new 
     * instance of whatever class we want to pass.
     */

    if(type_lookup(cur_args_table,root->astnode.ident.name)) {

      ht=type_lookup(cur_type_table,root->astnode.ident.name);

      if(ht)
        gen_load_op(ht->variable->astnode.ident.localvnum, Object);
      else
        gen_load_op(0, Object);

      fprintf(curfp,"%s", root->astnode.ident.name);
    }
    else {
      CPNODE *c;
      char *fc;

      fprintf(curfp," new %s() ",tempname);

      fc = get_full_classname(tempname);

      c = cp_find_or_insert(cur_const_table,CONSTANT_Class, fc);
      bytecode1(jvm_new,c->index);
      bytecode0(jvm_dup);

      c = newMethodref(cur_const_table,fc, "<init>", "()V");
      bytecode1(jvm_invokespecial, c->index);
    }

    return;
  }

  tempname = strdup(root->astnode.ident.name);
  uppercase(tempname);

  entry = methodscan (intrinsic_toks, tempname);

  /*  
   *  This block of code is only called if the identifier
   *  absolutely does not have an entry in any table,
   *  and corresponds to a method invocation of
   *  something in the blas or lapack packages.  
   */

  if (entry == NULL)
  {
    if (root->astnode.ident.arraylist != NULL)
      call_emit (root);
    f2jfree(tempname, strlen(tempname)+1);
    return;
  }

  javaname = entry->java_method;

printf("javaname = %s\n",javaname);
printf("args = %p\n", root->astnode.ident.arraylist);

  /* Ensure that the call has arguments */

  if (root->astnode.ident.arraylist != NULL)
  {
    CodeGraphNode *cmp_node, *goto_node, *iconst_node, *next_node;

    temp = root->astnode.ident.arraylist;

    if (!strcmp (tempname, "LSAME"))
    {
      /* LSAME should return TRUE if the two character arguments are
       * the same letter, regardless of case.
       */ 

      if(gendebug)
        printf("emitting a call to LSAME...first nodetype = %s, next = %s\n",
          print_nodetype(temp), print_nodetype(temp->nextstmt));

      if(temp == NULL) {
        fprintf(stderr,"No args to LSAME\n");
        f2jfree(tempname, strlen(tempname)+1);
        return;
      } 
      else if(temp->nextstmt == NULL) {
        fprintf(stderr,"Not enough args to LSAME\n");
        f2jfree(tempname, strlen(tempname)+1);
        return;
      }

      fprintf(curfp, "(");
      expr_emit(temp);

      if((temp->vartype != String) && (temp->vartype != Character)) {
        fprintf(stderr,"WARNING: non-string arg to LSAME");
        fprintf(stderr," -- typecast not yet implemented.\n");
      }
      fprintf(curfp, ".toLowerCase().charAt(0) == ");

      c = newMethodref(cur_const_table,JL_STRING,
             "toLowerCase", TOLOWER_DESC);
      bytecode1(jvm_invokevirtual, c->index);

      bytecode0(jvm_iconst_0);

      c = newMethodref(cur_const_table,JL_STRING,
             "charAt", CHARAT_DESC);
      bytecode1(jvm_invokevirtual, c->index);

      expr_emit(temp->nextstmt);

      if((temp->nextstmt->vartype != String)  &&
         (temp->nextstmt->vartype != Character))
      {
        fprintf(stderr,"WARNING: non-string arg to LSAME");
        fprintf(stderr," -- typecast not yet implemented.\n");
      }
      fprintf(curfp, ".toLowerCase().charAt(0))");

      c = newMethodref(cur_const_table,JL_STRING,
             "toLowerCase", TOLOWER_DESC);
      bytecode1(jvm_invokevirtual, c->index);

      bytecode0(jvm_iconst_0);

      c = newMethodref(cur_const_table,JL_STRING,
             "charAt", CHARAT_DESC);
      bytecode1(jvm_invokevirtual, c->index);

      cmp_node = bytecode0(jvm_if_icmpeq);
      bytecode0(jvm_iconst_0);
      goto_node = bytecode0(jvm_goto);
      iconst_node = bytecode0(jvm_iconst_1);
      cmp_node->branch_target = iconst_node;

      /* create a dummy instruction node following the iconst so that
       * we have a branch target for the goto statement.  it'll be
       * removed later.
       */
      next_node = bytecode0(jvm_impdep1);
      goto_node->branch_target = next_node;

      f2jfree(tempname, strlen(tempname)+1);
      return;
    }
    else if (!strcmp (tempname, "LSAMEN"))
    {
      /* LSAMEN should return TRUE if the first N characters of the
       * two arguments are the same, regardless of case.  Currently
       * this is mapped to java.lang.String.regionMatches().
       */

      /* first, make sure there are enough args to work with */
      if(temp == NULL) {
        fprintf(stderr,"No args to LSAMEN\n");
        f2jfree(tempname, strlen(tempname)+1);
        return;
      } 
      else if(temp->nextstmt == NULL) {
        fprintf(stderr,"Not enough args to LSAMEN\n");
        f2jfree(tempname, strlen(tempname)+1);
        return;
      }
      else if(temp->nextstmt->nextstmt == NULL) {
        fprintf(stderr,"Not enough args to LSAMEN\n");
        f2jfree(tempname, strlen(tempname)+1);
        return;
      }

      expr_emit(temp->nextstmt);
      fprintf (curfp, "%s(true,0,", javaname);
      bytecode0(jvm_iconst_1);
      bytecode0(jvm_iconst_0);
      expr_emit (temp->nextstmt->nextstmt);
      fprintf (curfp, ",0,");
      bytecode0(jvm_iconst_0);
      expr_emit (temp);
      fprintf (curfp, ")");

      c = newMethodref(cur_const_table,entry->class_name, 
                        entry->method_name, entry->descriptor);

      bytecode1(jvm_invokevirtual, c->index);

      f2jfree(tempname, strlen(tempname)+1);
      return;
    }
    else if(!strcmp(tempname, "ETIME")) {
      /* first, make sure there are enough args to work with */
      if(temp == NULL) {
        fprintf(stderr,"No args to ETIME\n");
        f2jfree(tempname, strlen(tempname)+1);
        return;
      } 

      printf("emitting ETIME...\n");

      fprintf (curfp, "Etime.etime(");
      expr_emit(temp);
      fprintf (curfp, ")");

      c = newMethodref(cur_const_table,entry->class_name, 
                        entry->method_name, entry->descriptor);

      bytecode1(jvm_invokestatic, c->index);
    }
    else if(!strcmp(tempname, "SECOND")) {
      fprintf(curfp, "(System.currentTimeMillis() / 1000.0)");

      c = newMethodref(cur_const_table,entry->class_name, 
                        entry->method_name, entry->descriptor);

      bytecode1(jvm_invokestatic, c->index);
      bytecode0(jvm_l2d);
      pushDoubleConst(1000.0);
      bytecode0(jvm_ddiv);
    }
  }

  f2jfree(tempname, strlen(tempname)+1);
}

/*****************************************************************************
 *                                                                           *
 * intrinsic_emit                                                            *
 *                                                                           *
 * This function generates calls to intrinsic functions.  Basically we just  *
 * map fortran intrinsics to equivalent functions in the core Java API.      *
 * It might be a good idea to write separate handlers for each intrinsic.    *
 * Many intrinsics can be handled with a generic handler, so we could have   *
 * a generic one-argument handler, a generic two-argument handler, etc.      *
 * Intrinsics that need more specialized handling, such as LOG10, would need *
 * their own handler.  Because of the need for specialized handlers, the     *
 * commented-out loop below may not ever really work.                        *
 *    (6/2000 removed loop - kgs).                                           *
 *                                                                           *
 *****************************************************************************/

void
intrinsic_emit(AST *root)
{
  AST *temp;
  HASHNODE *ht;
  CPNODE *c;
  METHODTAB *entry;
  char *tempname, *javaname;
  enum _intrinsics id;

  if(gendebug)
    printf("entering intrinsic_emit\n");

  tempname = strdup(root->astnode.ident.name);
  uppercase(tempname);

  entry = methodscan (intrinsic_toks, tempname);

  if(!entry) {
    fprintf(stderr,"Error: not expecting null entry at this point.\n");
    exit(-1);
  }

  javaname = entry->java_method;
  id = entry->intrinsic;

  switch(id) {
      /* numeric type conversion intrinsics.  */
    case ifunc_INT:
    case ifunc_IFIX:
    case ifunc_IDINT:
    case ifunc_REAL:
    case ifunc_FLOAT:
    case ifunc_SNGL:
    case ifunc_DBLE:
    case ifunc_CMPLX:
      temp = root->astnode.ident.arraylist;

       /* for Java source, we just emit a cast.  */
      fprintf (curfp, "%s(", javaname);
      expr_emit (temp);
      fprintf (curfp, ")");

      /* for bytecode, we emit the appropriate conversion opcode.  */
      if(temp->vartype != root->vartype)
        bytecode0(typeconv_matrix[temp->vartype][root->vartype]);

      break;

      /* conversion to integer */
    case ifunc_ICHAR:
      temp = root->astnode.ident.arraylist;
      fprintf (curfp, "%s(", javaname);
      expr_emit (temp);
      fprintf (curfp, ".charAt(0))");

      bytecode0(jvm_iconst_0);
      c = newMethodref(cur_const_table,JL_STRING,
             "charAt", CHARAT_DESC);
      bytecode1(jvm_invokevirtual, c->index);
      break;

      /* conversion to character */
    case ifunc_CHAR:
      c = cp_find_or_insert(cur_const_table,CONSTANT_Class,
                JL_CHAR);
      bytecode1(jvm_new,c->index);
      bytecode0(jvm_dup);

      temp = root->astnode.ident.arraylist;
      fprintf (curfp, "%s(", javaname);
      expr_emit (temp);
      fprintf (curfp, ")");

      c = newMethodref(cur_const_table,JL_CHAR,
             "<init>", "(C)V");
      bytecode1(jvm_invokespecial, c->index);
      c = newMethodref(cur_const_table, JL_CHAR, "toString", 
             TOSTRING_DESC);
      bytecode1(jvm_invokevirtual, c->index);
      break;

      /* truncation */
    case ifunc_AINT:
    case ifunc_DINT:
      if((root->astnode.ident.arraylist->vartype == Float) &&
         (id==ifunc_AINT))
        aint_intrinsic_emit(root, entry);
      else
        dint_intrinsic_emit(root, entry);
      break;

      /* nearest whole number */
    case ifunc_ANINT:
    case ifunc_DNINT:
      if(root->astnode.ident.arraylist->vartype == Double) {
        entry = &intrinsic_toks[ifunc_DNINT];
        fprintf (curfp, "(double)%s(", entry->java_method);
      }
      else
        fprintf (curfp, "(float)%s(", entry->java_method);

      expr_emit (root->astnode.ident.arraylist);
      fprintf (curfp, ")");

      c = newMethodref(cur_const_table,entry->class_name, 
                        entry->method_name, entry->descriptor);

      bytecode1(jvm_invokestatic, c->index);

      if(root->astnode.ident.arraylist->vartype == Double)
        bytecode0(jvm_i2d);
      else
        bytecode0(jvm_i2f);

      break;

      /* nearest integer */
    case ifunc_NINT:
    case ifunc_IDNINT:
      if(root->astnode.ident.arraylist->vartype == Double)
        entry = &intrinsic_toks[ifunc_IDNINT];

      fprintf (curfp, "%s(", entry->java_method);
      expr_emit (root->astnode.ident.arraylist);
      fprintf (curfp, ")");

      c = newMethodref(cur_const_table,entry->class_name, 
                        entry->method_name, entry->descriptor);

      bytecode1(jvm_invokestatic, c->index);

      break;

      /* absolute value */
    case ifunc_ABS:
      if(root->astnode.ident.arraylist->vartype == Integer)
        entry = &intrinsic_toks[ifunc_IABS];
      else if(root->astnode.ident.arraylist->vartype == Double)
        entry = &intrinsic_toks[ifunc_DABS];
      else if(root->astnode.ident.arraylist->vartype == Complex)
        entry = &intrinsic_toks[ifunc_CABS];
    case ifunc_DABS:
    case ifunc_IABS:
    case ifunc_CABS:
      temp = root->astnode.ident.arraylist;

      fprintf (curfp, "%s(", entry->java_method);
      expr_emit (temp);
      fprintf (curfp, ")");

      c = newMethodref(cur_const_table,entry->class_name, 
                        entry->method_name, entry->descriptor);

      bytecode1(jvm_invokestatic, c->index);
      break;

      /* remainder */
    case ifunc_MOD:
    case ifunc_AMOD:
    case ifunc_DMOD:
      temp = root->astnode.ident.arraylist;
      fprintf(curfp,"(");
      expr_emit (temp);
      fprintf(curfp,")%%("); 

      if(temp->vartype > root->vartype)
        bytecode0(
          typeconv_matrix[temp->vartype][root->vartype]);
      
      expr_emit (temp->nextstmt);
      fprintf(curfp,")");
      
      if(temp->nextstmt->vartype > root->vartype)
        bytecode0(
          typeconv_matrix[temp->nextstmt->vartype][root->vartype]);
      
      if(root->vartype == Float)
        bytecode0(jvm_frem);
      else if(root->vartype == Integer)
        bytecode0(jvm_irem);
      else
        bytecode0(jvm_drem);

      break;
      
      /* transfer of sign */
    case ifunc_SIGN:
      if(root->vartype == Integer)
        entry = &intrinsic_toks[ifunc_ISIGN];
      else if(root->vartype == Double)
        entry = &intrinsic_toks[ifunc_DSIGN];
    case ifunc_ISIGN:
    case ifunc_DSIGN:
      intrinsic2_call_emit(root,entry, root->vartype);
      break;

      /* positive difference */
    case ifunc_DIM:
      if(root->vartype == Integer)
        entry = &intrinsic_toks[ifunc_IDIM];
      else if(root->vartype == Double)
        entry = &intrinsic_toks[ifunc_DDIM];
    case ifunc_IDIM:
    case ifunc_DDIM:
      intrinsic2_call_emit(root,entry, root->vartype);
      break;
      
      /* double precision product of two reals */
    case ifunc_DPROD:
      temp = root->astnode.ident.arraylist;

      fprintf(curfp, "((double)(");
      expr_emit (temp);
      bytecode0(jvm_f2d);
      fprintf(curfp, ") * (double)(");
      expr_emit (temp->nextstmt);
      bytecode0(jvm_f2d);
      fprintf(curfp, "))");
      bytecode0(jvm_dmul);
      break;

      /* real AMAX0(integer) */
    case ifunc_AMAX0:
      fprintf(curfp,"(float)(");
      max_intrinsic_emit(root, tempname, entry);
      fprintf(curfp,")");
      bytecode0(typeconv_matrix[Integer][Float]);
      break;

      /* integer MAX1(real) */
    case ifunc_MAX1:
      fprintf(curfp,"(int)(");
      max_intrinsic_emit(root, tempname, entry);
      fprintf(curfp,")");
      bytecode0(typeconv_matrix[Float][Integer]);
      break;

      /* generic maximum or MAX that returns same type as args */
    case ifunc_MAX:
    case ifunc_MAX0:
    case ifunc_AMAX1:
    case ifunc_DMAX1:
      max_intrinsic_emit(root, tempname, entry);
      break;

      /* real AMIN0(integer) */
    case ifunc_AMIN0: 
      fprintf(curfp,"(float)(");
      min_intrinsic_emit(root, tempname, entry);
      fprintf(curfp,")");
      bytecode0(typeconv_matrix[Integer][Float]);
      break;

      /* integer MIN1(real) */
    case ifunc_MIN1:
      fprintf(curfp,"(int)(");
      min_intrinsic_emit(root, tempname, entry);
      fprintf(curfp,")");
      bytecode0(typeconv_matrix[Float][Integer]);
      break;

      /* generic minimum or MIN that returns same type as args */
    case ifunc_MIN:
    case ifunc_MIN0:
    case ifunc_AMIN1:
    case ifunc_DMIN1:
      min_intrinsic_emit(root, tempname, entry);
      break;
      
      /* length of a character entity */
    case ifunc_LEN:
      temp = root->astnode.ident.arraylist;

      if(temp != NULL) {
        if( (ht=type_lookup(cur_type_table,temp->astnode.ident.name)) != NULL)
        {
          fprintf (curfp, " %d ", ht->variable->astnode.ident.len);

          pushIntConst(ht->variable->astnode.ident.len);

          if(gendebug)
            printf("LEN(%s) = %d\n",temp->astnode.ident.name,
              ht->variable->astnode.ident.len);
        }
        else
        {
          fprintf (curfp, " 1 ");

          bytecode0(jvm_iconst_1);

          if(gendebug)
            printf("LEN(%s) = 1\n",temp->astnode.ident.name);
        }
      }
      break;

      /* Index of substring */
    case ifunc_INDEX:
    case ifunc_AIMAG:
    case ifunc_CONJG:
      /* Unimplemented!
       *
       * INDEX returns the location of a substring within another
       * string.  however fortran and java treat strings quite differently
       * so implementing INDEX properly isn't as straightforward as it seems
       * at first.  at this point, it's not that important, so I'll leave
       * it for later.   --kgs 6/14/00
       *
       * AIMAG and CONJG operate on complex numbers, which are not yet
       * supported.
       */
      fprintf(stderr,"WARNING: intrinsic %s not yet implemented!\n", 
              entry->fortran_name);
      break;

      /* square root */
    case ifunc_SQRT:
      if(root->vartype == Double)
        entry = &intrinsic_toks[ifunc_DSQRT];
      else if(root->vartype == Complex)
        entry = &intrinsic_toks[ifunc_CSQRT];
    case ifunc_DSQRT:
    case ifunc_CSQRT:
      intrinsic_call_emit(root,entry,Double);
      break;

      /* exponential */
    case ifunc_EXP:
      if(root->vartype == Double)
        entry = &intrinsic_toks[ifunc_DEXP];
      else if(root->vartype == Complex)
        entry = &intrinsic_toks[ifunc_CEXP];
    case ifunc_DEXP:
    case ifunc_CEXP:
      intrinsic_call_emit(root,entry,Double);
      break;

      /* natural logarithm */
    case ifunc_LOG:
      if(root->vartype == Double)
        entry = &intrinsic_toks[ifunc_DLOG];
      else if(root->vartype == Float)
        entry = &intrinsic_toks[ifunc_ALOG];
      else if(root->vartype == Complex)
        entry = &intrinsic_toks[ifunc_CLOG];
    case ifunc_ALOG:
    case ifunc_DLOG:
    case ifunc_CLOG:
      intrinsic_call_emit(root,entry,Double);
      break;

      /* common logarithm */
    case ifunc_LOG10:
      if(root->vartype == Double)
        entry = &intrinsic_toks[ifunc_DLOG10];
      else if(root->vartype == Float)
        entry = &intrinsic_toks[ifunc_ALOG10];
    case ifunc_ALOG10:
    case ifunc_DLOG10:
      intrinsic_call_emit(root, entry, Double);
      break;

      /* sine */
    case ifunc_SIN:
      if(root->vartype == Double)
        entry = &intrinsic_toks[ifunc_DSIN];
      else if(root->vartype == Complex)
        entry = &intrinsic_toks[ifunc_CSIN];
    case ifunc_DSIN:
    case ifunc_CSIN:
      intrinsic_call_emit(root, entry, Double);
      break;

      /* cosine */
    case ifunc_COS:
      if(root->vartype == Double)
        entry = &intrinsic_toks[ifunc_DCOS];
      else if(root->vartype == Complex)
        entry = &intrinsic_toks[ifunc_CCOS];
    case ifunc_DCOS:
    case ifunc_CCOS:
      intrinsic_call_emit(root, entry, Double);
      break;

      /* tangent */
    case ifunc_TAN:
      if(root->vartype == Double)
        entry = &intrinsic_toks[ifunc_DTAN];
    case ifunc_DTAN:
      intrinsic_call_emit(root, entry, Double);
      break;

      /* arcsine */
    case ifunc_ASIN:
      if(root->vartype == Double)
        entry = &intrinsic_toks[ifunc_DASIN];
    case ifunc_DASIN:
      intrinsic_call_emit(root, entry, Double);
      break;

      /* arccosine */
    case ifunc_ACOS:
      if(root->vartype == Double)
        entry = &intrinsic_toks[ifunc_DACOS];
    case ifunc_DACOS:
      intrinsic_call_emit(root, entry, Double);
      break;

      /* arctangent */
    case ifunc_ATAN:
      if(root->vartype == Double)
        entry = &intrinsic_toks[ifunc_DATAN];
    case ifunc_DATAN:
      intrinsic_call_emit(root, entry, Double);
      break;

      /* arctangent (2 arg) */
    case ifunc_ATAN2:
      if(root->vartype == Double)
        entry = &intrinsic_toks[ifunc_DATAN2];
    case ifunc_DATAN2:
      intrinsic2_call_emit(root, entry, Double);
      break;

      /* Hyperbolic sine */
    case ifunc_SINH:
      if(root->vartype == Double)
        entry = &intrinsic_toks[ifunc_DSINH];
    case ifunc_DSINH:
      intrinsic_call_emit(root, entry, Double);
      break;

      /* Hyperbolic cosine */
    case ifunc_COSH:
      if(root->vartype == Double)
        entry = &intrinsic_toks[ifunc_DCOSH];
    case ifunc_DCOSH:
      intrinsic_call_emit(root, entry, Double);
      break;

      /* Hyperbolic tangent */
    case ifunc_TANH:
      if(root->vartype == Double)
        entry = &intrinsic_toks[ifunc_DTANH];
    case ifunc_DTANH:
      intrinsic_call_emit(root, entry, Double);
      break;

    case ifunc_LGE: /* lexically greater than/equal */
    case ifunc_LGT: /* lexically greater than */
    case ifunc_LLE: /* lexically less than/equal */
    case ifunc_LLT: /* lexically less than */
      intrinsic_lexical_compare_emit(root, entry);
      break;

    default:
      fprintf(stderr,"WARNING: codegen() unimplemented intrinsic!\n");
      break; /* ansi c */
  }

  f2jfree(tempname, strlen(tempname)+1);

  if(gendebug)
    printf("leaving intrinsic_emit\n");
}


/*****************************************************************************
 *                                                                           *
 * intrinsic_lexical_compare_emit                                            *
 *                                                                           *
 * generates code for LGE, LGT, LLE, adn LLT intrinsics.   these intrinsics  *
 * perform lexical comparison of strings.                                    *
 *                                                                           *
 *****************************************************************************/

void
intrinsic_lexical_compare_emit(AST *root, METHODTAB *entry)
{
  CodeGraphNode *goto_node, *if_node;
  AST *temp;
  CPNODE *c;

  temp = root->astnode.ident.arraylist;
  fprintf(curfp,"((");
  expr_emit(temp);
  fprintf(curfp,").compareTo(");
  expr_emit(temp->nextstmt);

  c = newMethodref(cur_const_table, JL_STRING, "compareTo", COMPARE_DESC);
  bytecode1(jvm_invokevirtual, c->index);

  if(entry->intrinsic == ifunc_LGE) {
    fprintf(curfp,") >= 0 ? true : false)");
    if_node = bytecode0(jvm_ifge);
  }
  else if(entry->intrinsic == ifunc_LGT) {
    fprintf(curfp,") > 0 ? true : false)");
    if_node = bytecode0(jvm_ifgt);
  }
  else if(entry->intrinsic == ifunc_LLE) {
    fprintf(curfp,") <= 0 ? true : false)");
    if_node = bytecode0(jvm_ifle);
  }
  else if(entry->intrinsic == ifunc_LLT) {
    fprintf(curfp,") < 0 ? true : false)");
    if_node = bytecode0(jvm_iflt);
  }
  else
    fprintf(stderr,"intrinsic_lexical_compare_emit(): bad tag!\n");

  bytecode0(jvm_iconst_0);
  goto_node = bytecode0(jvm_goto);
  if_node->branch_target = bytecode0(jvm_iconst_1);

  /* create a dummy instruction node following the stmts so that
   * we have a branch target for the goto statement.  it'll be
   * removed later.
   */
  goto_node->branch_target = bytecode0(jvm_impdep1);
}


/*****************************************************************************
 *                                                                           *
 * intrinsic0_call_emit                                                      *
 *                                                                           *
 * generates a call to an intrinsic which has no args.                       *
 *                                                                           *
 *****************************************************************************/

void
intrinsic0_call_emit(AST *root, METHODTAB *entry)
{
  CPNODE *c;

  if(entry->ret != root->vartype)
    fprintf(curfp, "(%s)", returnstring[root->vartype]);

  fprintf (curfp, "%s()", entry->java_method);

  c = newMethodref(cur_const_table,entry->class_name, 
                    entry->method_name, entry->descriptor);

  bytecode1(jvm_invokestatic, c->index);

  if(entry->ret != root->vartype)
    bytecode0(typeconv_matrix[entry->ret][root->vartype]);
}

/*****************************************************************************
 *                                                                           *
 * intrinsic_call_emit                                                       *
 *                                                                           *
 * generates a call to a single-arg intrinsic.                               *
 *                                                                           *
 *****************************************************************************/

void
intrinsic_call_emit(AST *root, METHODTAB *entry, enum returntype argtype)
{
  CPNODE *c;

  /* entry->ret should represent the return type of the equivalent JAva
   * function, while root->vartype should represent the return type of
   * the fortran intrinsic.  e.g. fortan's EXP may return Real but JAva's
   * Math.exp() always returns double.  in these cases we must cast.
   */
  if(entry->ret != root->vartype)
    fprintf(curfp, "(%s)", returnstring[root->vartype]);

  fprintf (curfp, "%s(", entry->java_method);
  intrinsic_arg_emit(root->astnode.ident.arraylist, argtype);
  fprintf (curfp, ")");

  c = newMethodref(cur_const_table,entry->class_name, 
                    entry->method_name, entry->descriptor);

  bytecode1(jvm_invokestatic, c->index);

  if(entry->ret != root->vartype)
    bytecode0(typeconv_matrix[entry->ret][root->vartype]);
}

/*****************************************************************************
 *                                                                           *
 * intrinsic2_call_emit                                                      *
 *                                                                           *
 * generates a call to a two-arg intrinsic.                                  *
 *                                                                           *
 *****************************************************************************/

void
intrinsic2_call_emit(AST *root, METHODTAB *entry, enum returntype argtype)
{
  AST * temp = root->astnode.ident.arraylist;
  CPNODE *c;

  fprintf (curfp, "%s(", entry->java_method);
  intrinsic_arg_emit (temp, argtype);
  fprintf (curfp, ",");
  intrinsic_arg_emit (temp->nextstmt, argtype);
  fprintf (curfp, ")");

  c = newMethodref(cur_const_table,entry->class_name, 
                    entry->method_name, entry->descriptor);

  bytecode1(jvm_invokestatic, c->index);
}

/*****************************************************************************
 *                                                                           *
 * aint_intrinsic_emit                                                       *
 *                                                                           *
 * this function handles calls to the AINT intrinsic function.  AINT returns *
 * the floor of a single precision floating point number.                    *
 *                                                                           *
 *****************************************************************************/

void
aint_intrinsic_emit(AST *root, METHODTAB * entry)
{
  fprintf(curfp,"(float)(%s(",entry->java_method);

  expr_emit(root->astnode.ident.arraylist);

  fprintf(curfp,"))");

  /* convert to integer to truncate, then back to float */
  bytecode0(jvm_f2i);
  bytecode0(jvm_i2f);
}

/*****************************************************************************
 *                                                                           *
 * dint_intrinsic_emit                                                       *
 *                                                                           *
 * this function handles calls to the DINT intrinsic function.  DINT returns *
 * the floor of a double precision floating point number.                    *
 *                                                                           *
 *****************************************************************************/

void
dint_intrinsic_emit(AST *root, METHODTAB *entry)
{
  fprintf(curfp,"(double)(%s(",entry->java_method);
  expr_emit(root->astnode.ident.arraylist);
  fprintf(curfp,"))");

  /* convert to integer to truncate, then back to double */
  bytecode0(jvm_d2i);  
  bytecode0(jvm_i2d);
}

/*****************************************************************************
 *                                                                           *
 * intrinsic_arg_emit                                                        *
 *                                                                           *
 * this function emits the arg to an intrinsic function, making type casts   *
 * as necessary.                                                             *
 *                                                                           *
 *****************************************************************************/

void
intrinsic_arg_emit(AST *node, enum returntype this_type)
{
  printf("intrinsic_arg_emit, node type = %s, this type = %s\n",
         returnstring[node->vartype], returnstring[this_type]);

  if(node->vartype > this_type) {
    fprintf(curfp," (%s)",returnstring[this_type]);
    expr_emit (node);
    bytecode0(typeconv_matrix[node->vartype][this_type]);
  }
  else
    expr_emit(node);
}

/*****************************************************************************
 *                                                                           *
 * max_intrinsic_emit                                                        *
 *                                                                           *
 * This function handles calls to the MAX intrinsic function.  here we just  *
 * check if the generic form is used and then call maxmin_intrinsic_emit().  *
 *                                                                           *
 *****************************************************************************/

void
max_intrinsic_emit(AST *root, char *tempname, METHODTAB *entry)
{
  METHODTAB *tmpentry = entry;
  char *desc = "(DDD)D";

  if(entry->intrinsic == ifunc_MAX) {
    switch(root->vartype) {
      case Integer:
        tmpentry = &intrinsic_toks[ifunc_MAX0];
        desc = "(III)I";
        break;
      case Float:
        tmpentry = &intrinsic_toks[ifunc_AMAX1];
        desc = "(FFF)F";
        break;
      case Double:
        tmpentry = &intrinsic_toks[ifunc_DMAX1];
        desc = "(DDD)D";
        break;
      default:
        fprintf(stderr,"WARNING: generic MAX used, but data type is bad!\n");
        break;
    }
  }
  else if((entry->intrinsic == ifunc_MAX0) || (entry->intrinsic == ifunc_AMAX0))
    desc = "(III)I";
  else if((entry->intrinsic == ifunc_AMAX1) || (entry->intrinsic == ifunc_MAX1))
    desc = "(FFF)F";
  else if(entry->intrinsic == ifunc_DMAX1)
    desc = "(DDD)D";
  else
    fprintf(stderr,"WARNING: bad intrinsic tag in max_intrinsic_emit()\n");

  maxmin_intrinsic_emit(root,tempname,tmpentry,THREEARG_MAX_FUNC, desc);
}

/*****************************************************************************
 *                                                                           *
 * min_intrinsic_emit                                                        *
 *                                                                           *
 * This function handles calls to the MIN intrinsic function.  here we just  *
 * check if the generic form is used and then call maxmin_intrinsic_emit().  *
 *                                                                           *
 *****************************************************************************/

void
min_intrinsic_emit(AST *root, char *tempname, METHODTAB *entry)
{
  METHODTAB *tmpentry = entry;
  char *desc = "(DDD)D";

  if(entry->intrinsic == ifunc_MIN) {
    switch(root->vartype) {
      case Integer:
        tmpentry = &intrinsic_toks[ifunc_MIN0];
        desc = "(III)I";
        break;
      case Float:
        tmpentry = &intrinsic_toks[ifunc_AMIN1];
        desc = "(FFF)F";
        break;
      case Double:
        tmpentry = &intrinsic_toks[ifunc_DMIN1];
        desc = "(DDD)D";
        break;
      default:
        fprintf(stderr,"WARNING: generic MIN used, but data type is bad!\n");
        break;  /* ansi c */
    }
  }
  else if((entry->intrinsic == ifunc_MIN0) || (entry->intrinsic == ifunc_AMIN0))
    desc = "(III)I";
  else if((entry->intrinsic == ifunc_AMIN1) || (entry->intrinsic == ifunc_MIN1))
    desc = "(FFF)F";
  else if(entry->intrinsic == ifunc_DMIN1)
    desc = "(DDD)D";
  else
    fprintf(stderr,"WARNING: bad intrinsic tag in min_intrinsic_emit()\n");

  printf("MIN vartype = %s, %s %s %s\n", returnstring[root->vartype], 
         entry->class_name, entry->method_name, entry->descriptor);
  maxmin_intrinsic_emit(root,tempname,tmpentry,THREEARG_MIN_FUNC, desc);
}

/*****************************************************************************
 *                                                                           *
 * maxmin_intrinsic_emit                                                     *
 *                                                                           *
 * This function handles calls to the MAX and MIN intrinsic functions. these *
 * functions take a variable number of arguments, which is not easily        *
 * accomplished in Java, so we generate multiple calls to Math.max/Math.min  *
 * in case there are more than 2 args.                                       *
 *                                                                           *
 *****************************************************************************/

void
maxmin_intrinsic_emit(AST *root, char *tempname, METHODTAB *entry,
                      char *threearg, char *three_desc)
{
  int ii, arg_count = 0;
  char *javaname = entry->java_method, *method;
  CPNODE *c;
  AST *temp;

  /* figure out how many args we need to handle */
  for(temp = root->astnode.ident.arraylist; temp != NULL; temp = temp->nextstmt)
    arg_count++;

  /* If we only have one arg, just emit that expression.  This should not
   * happen since it's invalid to call MAX with only one arg.
   */

  if(arg_count == 1) {
    fprintf (curfp, "(");
    intrinsic_arg_emit(temp,entry->ret);
    fprintf (curfp, ")");
  }

  /* typical situation, two args */

  else if(arg_count == 2) {
    temp = root->astnode.ident.arraylist;
    fprintf(curfp, "%s(", javaname);
    intrinsic_arg_emit(temp,entry->ret);
    fprintf (curfp, ", ");
    intrinsic_arg_emit(temp->nextstmt,entry->ret);
    fprintf (curfp, ")");
    c = newMethodref(cur_const_table,entry->class_name, 
                      entry->method_name, entry->descriptor);

    bytecode1(jvm_invokestatic, c->index);
  }

  /* special handling of common situation in which MAX or MIN has three args. */

  else if(arg_count == 3) {
    temp = root->astnode.ident.arraylist;
    fprintf(curfp, "%s(", threearg);
    intrinsic_arg_emit(temp,entry->ret);
    fprintf (curfp, ", ");
    intrinsic_arg_emit(temp->nextstmt,entry->ret);
    fprintf (curfp, ", ");
    intrinsic_arg_emit(temp->nextstmt->nextstmt,entry->ret);
    fprintf (curfp, ")");

    method = strtok(strdup(threearg),".");
    method = strtok(NULL,".");
    c = newMethodref(cur_const_table,UTIL_CLASS, method, three_desc);

    bytecode1(jvm_invokestatic, c->index);
  }

  /*
   * For cases in which MAX or MIN has more than three args, we generate n-1
   * method calls, where n is the number of args.  For example, MAX(a,b,c,d,e)
   * would be translated to:
   *   Math.max(Math.max(Math.max(Math.max(a,b),c),d),e)
   * I dont think this situation is very common (in LAPACK/BLAS at least).
   *
   * changed this slightly to make the inner call a three-arg Util.max call.
   *  e.g.  Math.max(Math.max(Util.max(a,b,c),d),e)
   * --kgs 6/13/00
   */

  else {
    for(ii=0;ii<arg_count -3;ii++)
      fprintf(curfp,"%s(",javaname);
    fprintf(curfp,"%s(",threearg);

    temp = root->astnode.ident.arraylist;
    intrinsic_arg_emit(temp, entry->ret);
    fprintf (curfp, ", ");
    temp = temp->nextstmt;
    intrinsic_arg_emit(temp, entry->ret);
    fprintf (curfp, ", ");
    temp = temp->nextstmt;
    intrinsic_arg_emit(temp, entry->ret);

    method = strtok(strdup(threearg),".");
    method = strtok(NULL,".");
    c = newMethodref(cur_const_table,UTIL_CLASS, method, three_desc);

    bytecode1(jvm_invokestatic, c->index);

    c = newMethodref(cur_const_table,entry->class_name, 
                      entry->method_name, entry->descriptor);

    for(temp = temp->nextstmt; temp != NULL; temp = temp->nextstmt) {
      intrinsic_arg_emit(temp, entry->ret);
      if(temp->nextstmt != NULL)
        fprintf (curfp, "), ");
      else
        fprintf (curfp, ") ");
      bytecode1(jvm_invokestatic, c->index);
    }
  }
}

/*****************************************************************************
 *                                                                           *
 * get_type                                                                  *
 *                                                                           *
 * This function tries to guess the type of a value contained                *
 * in a string.  If we find a '.' in the string, we guess that               *
 * it's a floating point number.  If the string contains 'true'              *
 * or 'false', we guess that it's a boolean value.  Otherwise                *
 * we guess that it's an integer value.  Not very sophisticated,             *
 * but it works most of the time.                                            *
 *                                                                           *
 *****************************************************************************/

enum returntype
get_type(char *num)
{
  int idx;

  for(idx = 0;idx < strlen(num);idx++) 
    if(num[idx] == '.') {
      return Double;
      break;
    }

  if( !strcmp(num,"false") || !strcmp(num,"true"))
    return Logical;

  return Integer;
}

/*****************************************************************************
 *                                                                           *
 * expr_emit                                                                 *
 *                                                                           *
 * This function traverses an expression subtree and emits code for simple   *
 * operations.  For more complex operations, we call the appropriate code    *
 * generation routine.                                                       *
 *                                                                           *
 *****************************************************************************/

void
expr_emit (AST * root)
{
  char *tempname = NULL;
  CPNODE * ct;
  int cur_vt;

  if(root == NULL)
  {
    /* We should not have a NULL expression */

    fprintf(stderr,"Warning: NULL root in expr_emit (%s)\n", cur_filename);
    return;
  }

  if(gendebug) {
    printf("expr_emit(): nodetype = %s\n", print_nodetype(root));
    if(root->nodetype == Binaryop)
      printf("\toptype = %c\n",root->astnode.expression.optype);
  }

  switch (root->nodetype)
  {
    case Identifier:
      name_emit (root);
      break;
    case Expression:
      if (root->astnode.expression.parens)
        fprintf (curfp, "(");

      /* is expression.lhs ever really non-null? i dont think so.
       * in any case, for bytecode generation, we are not concerned
       * with parens, so it should be ok to just call expr_emit. (kgs)
       */

      if (root->astnode.expression.lhs != NULL)
        expr_emit (root->astnode.expression.lhs);

      expr_emit (root->astnode.expression.rhs);

      if (root->astnode.expression.parens)
        fprintf (curfp, ")");
      break;
    case Power:
      {
        /* hack alert: determine whether this expression is used as the size
         *   in an array declaration.  if so, it must be integer, but java's
         *   pow() method returns double.  so we add a cast.  it would probably
         *   be better to detect this elsewhere (e.g. in the code that emits
         *   array declarations).
         */
        BOOLEAN gencast = (root->parent != NULL) && (root->parent->nodetype == ArrayDec);

        fprintf (curfp, "%sMath.pow(", gencast ? "(int) " : "");
           
        /* the args to pow must be doubles, so cast if necessary */

        expr_emit (root->astnode.expression.lhs);

        if(root->astnode.expression.lhs->vartype != Double)
          bytecode0(typeconv_matrix[root->astnode.expression.lhs->vartype][Double]);
        fprintf (curfp, ", ");
        expr_emit (root->astnode.expression.rhs);
        if(root->astnode.expression.rhs->vartype != Double)
          bytecode0(typeconv_matrix[root->astnode.expression.rhs->vartype][Double]);
        fprintf (curfp, ")");

        ct = newMethodref(cur_const_table,"java/lang/Math", "pow", "(DD)D");

        bytecode1(jvm_invokestatic, ct->index);
 
        if(gencast)
          bytecode0(jvm_d2i);
        else if(root->vartype != Double)
          bytecode0(typeconv_matrix[root->vartype][Double]);
      }
      break;
    case Binaryop:
      /* handle special case for string concatenation in bytecode..   we
       * must create a new StringBuffer which contains the LHS and append
       * the RHS to the STringBuffer.
       */
      if(root->token == CAT)
      {
        ct = cp_find_or_insert(cur_const_table,CONSTANT_Class,
                  STRINGBUFFER);

        bytecode1(jvm_new,ct->index);
        bytecode0(jvm_dup);
        expr_emit (root->astnode.expression.lhs);
        if((root->astnode.expression.lhs->vartype != String) &&
           (root->astnode.expression.lhs->vartype != Character) )
        {
          fprintf(stderr,"WARNING, string cat with non-string types unsupported\n");
        }
        ct = newMethodref(cur_const_table,STRINGBUFFER, "<init>", STRBUF_DESC);

        bytecode1(jvm_invokespecial, ct->index);
        expr_emit (root->astnode.expression.rhs);
        if((root->astnode.expression.rhs->vartype != String) &&
           (root->astnode.expression.rhs->vartype != Character) )
        {
          fprintf(stderr,"WARNING, string cat with non-string types unsupported\n");
        }
        ct = newMethodref(cur_const_table,STRINGBUFFER, "append", 
                          append_descriptor[String]);
        bytecode1(jvm_invokevirtual, ct->index);
        ct = newMethodref(cur_const_table,STRINGBUFFER, "toString", 
                          TOSTRING_DESC);
        bytecode1(jvm_invokevirtual, ct->index);
      }
      else {
        expr_emit (root->astnode.expression.lhs);

        if(root->astnode.expression.lhs->vartype > root->vartype)
          bytecode0(
            typeconv_matrix[root->astnode.expression.lhs->vartype][root->vartype]);

        fprintf (curfp, "%c", root->astnode.expression.optype);
        expr_emit (root->astnode.expression.rhs);

        if(root->astnode.expression.rhs->vartype > root->vartype)
          bytecode0(
            typeconv_matrix[root->astnode.expression.rhs->vartype][root->vartype]);

        switch(root->astnode.expression.optype) {
          case '+':
            bytecode0(add_opcode[root->vartype]);
            break;
          case '-':
            bytecode0(sub_opcode[root->vartype]);
            break;
          case '/':
            bytecode0(div_opcode[root->vartype]);
            break;
          case '*':
            bytecode0(mul_opcode[root->vartype]);
            break;
          default:
            fprintf(stderr,"WARNING: unsupported optype\n");
            break;  /* for ANSI C compliance */
        }
      }

      break;
    case Unaryop:
      fprintf (curfp, "%c", root->astnode.expression.minus);
      expr_emit (root->astnode.expression.rhs);

      if(root->astnode.expression.minus == '-')
        bytecode0(neg_opcode[root->vartype]);

      break;
    case Constant:
      if(root->parent != NULL)
      {
        tempname = strdup(root->parent->astnode.ident.name);
        uppercase(tempname);
      }

     /* 
      * here we need to determine if this is a parameter to a function
      * or subroutine.  if so, and we are using wrappers, then we need
      * to create a temporary wrapper and pass that in instead of the
      * constant.   10/9/97  -- Keith 
      */

      if( (root->parent != NULL) &&
          (root->parent->nodetype == Call) &&
          (type_lookup(cur_array_table,root->parent->astnode.ident.name) == NULL) &&
          (methodscan(intrinsic_toks, tempname) == NULL))
      {
        if(root->token == STRING) {
          if(omitWrappers) {

            pushConst(root);

            fprintf (curfp, "\"%s\"", root->astnode.constant.number);
          }
          else
          {
            invoke_constructor(full_wrappername[root->vartype], root, 
              wrapper_descriptor[root->vartype]);

            fprintf (curfp, "new StringW(\"%s\")", root->astnode.constant.number);
          }
        }
        else {     /* non-string constant argument to a function call */
          if(omitWrappers) {
            pushConst(root);

            fprintf (curfp, "%s", root->astnode.constant.number);
          }
          else
          {
            invoke_constructor(full_wrappername[root->vartype], root, 
              wrapper_descriptor[root->vartype]);

            fprintf (curfp, "new %s(%s)", 
              wrapper_returns[get_type(root->astnode.constant.number)],
              root->astnode.constant.number);
          }
        }
      }
      else  /* this constant is not an argument to a function call */
      {

        pushConst(root);

        if(root->token == STRING)
          fprintf (curfp, "\"%s\"", root->astnode.constant.number);
        else
          fprintf (curfp, "%s", root->astnode.constant.number);
      }

      if(tempname != NULL)
        f2jfree(tempname, strlen(tempname)+1);
      break;
    case Logicalop:
      /* 
       * Change all of this code to switch on the tokens.
       * The parser code will have to store the NOT token.
       */
      if (root->astnode.expression.lhs != NULL)
        expr_emit (root->astnode.expression.lhs);
      else
        fprintf (curfp, "!");

      if (root->token == AND)
        fprintf (curfp, " && ");

      if (root->token == OR)
        fprintf (curfp, " || ");

      expr_emit (root->astnode.expression.rhs);

      switch(root->token) {
        case NOT:
          bytecode0(jvm_iconst_1);
          bytecode0(jvm_ixor);
          break;
        case AND:
          bytecode0(jvm_iand);
          break;
        case OR:
          bytecode0(jvm_ior);
          break;
      }
      break;
    case Relationalop:

      cur_vt = MIN(root->astnode.expression.lhs->vartype,
                   root->astnode.expression.rhs->vartype);

      if(((root->astnode.expression.lhs->vartype == String) ||
          (root->astnode.expression.lhs->vartype == Character)) &&
         ((root->astnode.expression.rhs->vartype == String) ||
          (root->astnode.expression.rhs->vartype == Character)))
      {
        CPNODE *c;

        if((root->token != rel_eq) && (root->token != rel_ne)) {
          fprintf(stderr,"WARNING: didn't expect this relop on a STring type!\n");
          return;
        }

        cur_vt = root->astnode.expression.lhs->vartype;

        c = newMethodref(cur_const_table,JL_STRING,
               "trim", TRIM_DESC);

        if(root->token == rel_ne)
          fprintf(curfp,"!");

        expr_emit (root->astnode.expression.lhs);
        bytecode1(jvm_invokevirtual, c->index);  /* call trim() */
      
        fprintf(curfp,".trim().equalsIgnoreCase(");

        expr_emit (root->astnode.expression.rhs);
        bytecode1(jvm_invokevirtual, c->index);  /* call trim() */

        fprintf(curfp,".trim())");

        c = newMethodref(cur_const_table,JL_STRING,
               "equalsIgnoreCase", STREQV_DESC);
        bytecode1(jvm_invokevirtual, c->index);  /* equalsIgnoreCase() */

        /* now check the op type & reverse if .NE. */
        if(root->token == rel_ne) {
          bytecode0(jvm_iconst_1);
          bytecode0(jvm_ixor);
        }

        return;   /* nothing more to do for strings here. */
      }

      switch (root->token)
      {
        case rel_eq:

          if(gendebug) {
            if(root->astnode.expression.lhs->nodetype == Identifier)
              printf("##@@ lhs ident %s has type %s\n", 
                 root->astnode.expression.lhs->astnode.ident.name,
                 returnstring[root->astnode.expression.lhs->vartype]);

            if(root->astnode.expression.rhs->nodetype == Identifier)
              printf("##@@ rhs ident %s has type %s\n", 
                root->astnode.expression.rhs->astnode.ident.name,
                returnstring[root->astnode.expression.rhs->vartype]);
          }

          expr_emit (root->astnode.expression.lhs);

          if(root->astnode.expression.lhs->vartype > cur_vt) {
            bytecode0(
              typeconv_matrix[root->astnode.expression.lhs->vartype][cur_vt]);
          }

          fprintf (curfp, " == ");

          expr_emit (root->astnode.expression.rhs);

          if(root->astnode.expression.rhs->vartype > cur_vt) {
            bytecode0(
              typeconv_matrix[root->astnode.expression.rhs->vartype][cur_vt]);
          }

          break;
        case rel_ne:

          expr_emit (root->astnode.expression.lhs);
          if(root->astnode.expression.lhs->vartype > cur_vt) {
            bytecode0(
              typeconv_matrix[root->astnode.expression.lhs->vartype][cur_vt]);
          }
          fprintf (curfp, " != ");
          expr_emit (root->astnode.expression.rhs);
          if(root->astnode.expression.rhs->vartype > cur_vt) {
            bytecode0(
              typeconv_matrix[root->astnode.expression.rhs->vartype][cur_vt]);
          }
          break;
        case rel_lt:
          expr_emit (root->astnode.expression.lhs);
          if(root->astnode.expression.lhs->vartype > cur_vt) {
            bytecode0(
              typeconv_matrix[root->astnode.expression.lhs->vartype][cur_vt]);
          }
          fprintf (curfp, " < ");
          expr_emit (root->astnode.expression.rhs);
          if(root->astnode.expression.rhs->vartype > cur_vt) {
            bytecode0(
              typeconv_matrix[root->astnode.expression.rhs->vartype][cur_vt]);
          }
          break;
        case rel_le:
          expr_emit (root->astnode.expression.lhs);
          if(root->astnode.expression.lhs->vartype > cur_vt) {
            bytecode0(
              typeconv_matrix[root->astnode.expression.lhs->vartype][cur_vt]);
          }
          fprintf (curfp, " <= ");
          expr_emit (root->astnode.expression.rhs);
          if(root->astnode.expression.rhs->vartype > cur_vt) {
            bytecode0(
              typeconv_matrix[root->astnode.expression.rhs->vartype][cur_vt]);
          }
          break;
        case rel_gt:
          expr_emit (root->astnode.expression.lhs);
          if(root->astnode.expression.lhs->vartype > cur_vt) {
            bytecode0(
              typeconv_matrix[root->astnode.expression.lhs->vartype][cur_vt]);
          }
          fprintf (curfp, " > ");
          expr_emit (root->astnode.expression.rhs);
          if(root->astnode.expression.rhs->vartype > cur_vt) {
            bytecode0(
              typeconv_matrix[root->astnode.expression.rhs->vartype][cur_vt]);
          }
          break;
        case rel_ge:
          expr_emit (root->astnode.expression.lhs);
          if(root->astnode.expression.lhs->vartype > cur_vt) {
            bytecode0(
              typeconv_matrix[root->astnode.expression.lhs->vartype][cur_vt]);
          }
          fprintf (curfp, " >= ");
          expr_emit (root->astnode.expression.rhs);
          if(root->astnode.expression.rhs->vartype > cur_vt) {
            bytecode0(
              typeconv_matrix[root->astnode.expression.rhs->vartype][cur_vt]);
          }
          break;
      }

      switch(cur_vt) {
        case String: 
        case Character: 
          /* we dont need to do anything here because strings were handled
           * above already.
           */
          break;
        case Complex: 
          fprintf(stderr,"WARNING: complex relop not supported yet!\n");
          break;
        case Logical:
          fprintf(stderr,"WARNING: relop not supported on logicals!\n");
          break;
        case Float: 
          fprintf(stderr,"WARNING: single precision not supported!\n");
          break;
        case Double: 
          {
            CodeGraphNode *cmp_node, *goto_node, *iconst_node, *next_node;

            /* the only difference between dcmpg and dcmpl is the handling of NaN
             * value.  for .lt. and .le. we use dcmpg, otherwise use dcmpl.
             * this mirrors the behavior of javac.
             */
            if((root->token == rel_lt) || (root->token == rel_le))
              bytecode0(jvm_dcmpg);
            else
              bytecode0(jvm_dcmpl);

            cmp_node = bytecode0(dcmp_opcode[root->token]);
            bytecode0(jvm_iconst_0);
            goto_node = bytecode0(jvm_goto);
            iconst_node = bytecode0(jvm_iconst_1);
            cmp_node->branch_target = iconst_node;

            /* create a dummy instruction node following the iconst so that
             * we have a branch target for the goto statement.  it'll be
             * removed later.
             */
            next_node = bytecode0(jvm_impdep1);
            goto_node->branch_target = next_node;
          }
          break;
        case Integer: 
          {
            CodeGraphNode *cmp_node, *goto_node, *iconst_node, *next_node;

            cmp_node = bytecode0(icmp_opcode[root->token]);
            bytecode0(jvm_iconst_0);
            goto_node = bytecode0(jvm_goto);
            iconst_node = bytecode0(jvm_iconst_1);
            cmp_node->branch_target = iconst_node;

            /* create a dummy instruction node following the iconst so that
             * we have a branch target for the goto statement.  it'll be
             * removed later.
             */
            next_node = bytecode0(jvm_impdep1);
            goto_node->branch_target = next_node;
          }
          break;
        default:
          fprintf(stderr,"WARNING: hit default, relop .eq.\n");
          break;
      }

      break;
    case Substring:
      {
        CPNODE *c;

        /* Substring operations are handled with java.lang.String.substring */

        name_emit(root);

        fprintf(curfp,"(");
        expr_emit(root->astnode.ident.arraylist);
        fprintf(curfp,")-1,");

        bytecode0(jvm_iconst_m1);  /* decrement start idx by one */
        bytecode0(jvm_iadd);

        expr_emit(root->astnode.ident.arraylist->nextstmt);
        fprintf(curfp,")");

        c = newMethodref(cur_const_table,JL_STRING,
                 "substring", SUBSTR_DESC);
        bytecode1(jvm_invokevirtual, c->index);

      }
      break;
    default:
      fprintf(stderr,"Warning: Unknown nodetype in expr_emit(): %s\n",
        print_nodetype(root));
  }
}

/*****************************************************************************
 *                                                                           *
 * open_output_file                                                          *
 *                                                                           *
 * This function attempts to open the output file and write the              *
 * header.                                                                   *
 *                                                                           *
 *****************************************************************************/

void
open_output_file(AST *root, char *classname)
{
  char * filename;
  char import_stmt[60];
  
  filename = (char *) f2jalloc(strlen(cur_filename) + 6);
  strcpy(filename, cur_filename);
  strcat(filename,".java");

  if(gendebug)
    printf("filename is %s\n",filename);

  if(gendebug)
    printf("## going to open file: '%s'\n", filename);

  if((javafp = fopen_fullpath(filename,"w"))==NULL) {
    fprintf(stderr,"Cannot open output file '%s'.\n",filename);
    perror("Reason");
    exit(1);
  }

  curfp = javafp;  /* set global pointer to output file */
printf("open_output_file.1: set curfp = %p\n", curfp);

  /* add import statements if necessary */

  import_stmt[0] = '\0';
  
  if(import_reflection)
    strcat(import_stmt,"import java.lang.reflect.*;\n");

/*
 *if(import_blas)
 *  strcat(import_stmt,"import org.netlib.blas.*;\n");
 */

  javaheader(javafp,import_stmt);

  if(genJavadoc)
    emit_javadoc_comments(root);

  fprintf(javafp,"public class %s {\n\n", classname);

  f2jfree(filename, strlen(cur_filename) + 6);
}

/*****************************************************************************
 *                                                                           *
 * constructor                                                               *
 *                                                                           *
 * This function generates the method header for the current                 *
 * function or subroutine.                                                   *
 *                                                                           *
 *****************************************************************************/

void
constructor (AST * root)
{
  enum returntype returns;
  AST *tempnode;
  char *tempstring;
  HASHNODE *hashtemp;

  /* set global descriptor variable (method_desc) */

/*
 *if(root->nodetype == Program)
 *  method_desc = MAIN_DESCRIPTOR;
 *else
 */
    method_desc = root->astnode.source.descriptor;

  /* 
   * In fortran, functions return a value implicitly
   * associated with their own name. In java, we declare a
   * variable in the constructor that shadows the class
   * (function) name and returns the same type. 
   */

  if (root->nodetype == Function)
  {
    char *name, *desc;
    CPNODE *c;

    returns = root->astnode.source.returns;
    name = root->astnode.source.name->astnode.ident.name;

    if(omitWrappers && !cgPassByRef(name)) {
      addField( name, field_descriptor[returns][0]);
      desc = field_descriptor[returns][0];
    }
    else {
      addField(name, wrapped_field_descriptor[returns][0]);
      desc = wrapped_field_descriptor[returns][0];
    }

    printf("this is a Function, needs implicit variable\n");
    printf("method name = %s\n", name);
    printf("implicit var desc = %s\n", desc);

    /* Test code.... */
    if ((returns == String) || (returns == Character))
    {
      fprintf(curfp, "static %s %s ", 
           returnstring[returns], name);

      print_string_initializer(root->astnode.source.name);

      fprintf(curfp, ";\n\n");

      c = newFieldref(cur_const_table,cur_filename, name, desc);
      bytecode1(jvm_putstatic, c->index);
    }
    else
    {
      if(omitWrappers && 
        !cgPassByRef(root->astnode.source.name->astnode.ident.name))
      {
          fprintf (curfp, "static %s %s = %s;\n\n", 
            returnstring[returns],
            root->astnode.source.name->astnode.ident.name,
            init_vals[returns]);
      }
      else
      {
        c = cp_find_or_insert(cur_const_table,CONSTANT_Class,
                  full_wrappername[returns]);

        bytecode1(jvm_new,c->index);
        bytecode0(jvm_dup);

        bytecode0(init_opcodes[returns]);

        c = newMethodref(cur_const_table,full_wrappername[returns],
               "<init>", wrapper_descriptor[returns]);

        bytecode1(jvm_invokespecial, c->index);

        c = newFieldref(cur_const_table,cur_filename,name,desc); 
        bytecode1(jvm_putstatic, c->index);

        fprintf (curfp, "static %s %s = new %s(%s);\n\n", 
          wrapper_returns[returns],
          root->astnode.source.name->astnode.ident.name,
          wrapper_returns[returns],
          init_vals[returns]);
      }
    }

    /* Define the constructor for the class. */

    fprintf (curfp, "\npublic static %s %s (",
      returnstring[returns],
      root->astnode.source.name->astnode.ident.name);

    if(genInterfaces)
      emit_interface(root);
  }
  /* Else we have a subroutine, which returns void. */
  else if(root->nodetype == Subroutine)
  {
    fprintf (curfp, "\npublic static void %s (",
      root->astnode.source.name->astnode.ident.name);

    if(genInterfaces)
      emit_interface(root);
  }
  else  /* Else we have a program, create a main() function */
  {
    fprintf (curfp, "\npublic static void main (String [] args");
  }

  /*
   *  Now traverse the list of constructor arguments for either
   *  functions or subroutines.   This is where I will
   *  have to check what the variable type is in the
   *  symbol table. 
   */

  tempnode = root->astnode.source.args;

  for (; tempnode != NULL; tempnode = tempnode->nextstmt)
  {
    hashtemp = type_lookup (cur_type_table, tempnode->astnode.ident.name);
    if (hashtemp == NULL)
    {
      if( type_lookup (cur_external_table, tempnode->astnode.ident.name) ) {
        fprintf (curfp, "Object %s", tempnode->astnode.ident.name);

        if (tempnode->nextstmt)
          fprintf (curfp, ",\n");
        continue;
      }
      else {
        fprintf (stderr,"Type table is screwed (codegen.c).\n");
        fprintf (stderr,"  (looked up: %s)\n", tempnode->astnode.ident.name);
        exit (-1);
      }
    }

    /* If this variable is declared external and it is an argument to
     * this program unit, it must be declared as Object in Java.
     */

    if(type_lookup(cur_external_table, tempnode->astnode.ident.name) != NULL)
      returns = OBJECT_TYPE;
    else
      returns = hashtemp->type;

    /* 
     * Check the numerical value returns.  It should not 
     * exceed the value of the enum returntypes.  
     */

    if (returns > MAX_RETURNS)
      fprintf (stderr,"Bad return value, check types.\n");

    if(omitWrappers) {
      if((hashtemp->variable->astnode.ident.arraylist == NULL) &&
        cgPassByRef(tempnode->astnode.ident.name))
        tempstring = wrapper_returns[returns];
      else
        tempstring = returnstring[returns];
    }
    else
    {
      if (hashtemp->variable->astnode.ident.arraylist == NULL)
        tempstring = wrapper_returns[returns];
      else
        tempstring = returnstring[returns];
    }

    /* 
     * I haven't yet decided how the pass-by-reference
     * pass-by-value problem will be resolved.  It may
     * not be an issue at all in a java calling java
     * situation.  The next line, when used, will list
     * all the arguments to the method as references.
     * This means that primitives such as int and
     * double are wrapped as objects.
     *
     * *tempstring = toupper (*tempstring);
     *
     * To save storage space, I'm wrapping the primitives with
     * special-purpose wrappers (intW, doubleW, etc.).
     * 10/8/97  --Keith 
     */

    fprintf (curfp, "%s ", tempstring);

    if (hashtemp->variable->astnode.ident.arraylist == NULL)
      fprintf (curfp, "%s", tempnode->astnode.ident.name);
    else {
      /* Declare as array variables.  */
      char *temp2;
      fprintf (curfp, "[]");
      fprintf (curfp, " %s", tempnode->astnode.ident.name);

      /* 
       * for arrays, add a parameter representing the base 
       * index.   -- Keith 
       */

      temp2 = (char *)f2jalloc(strlen(tempnode->astnode.ident.name) + 9);
      strcpy( temp2, "_");
      strcat( temp2, tempnode->astnode.ident.name);
      strcat( temp2, "_offset");
      fprintf(curfp, ", int %s",temp2);
      f2jfree(temp2, strlen(temp2)+1);
    }

    /* Don't emit a comma on the last iteration. */
    if (tempnode->nextstmt)
      fprintf (curfp, ",\n");
  }

  fprintf (curfp, ")  {\n\n");
    
}				/*  Close  constructor(). */

/*****************************************************************************
 *                                                                           *
 * emit_interface                                                            *
 *                                                                           *
 * This function generates a simplified interface to the underlying          *
 * numerical routine.  This simplification includes:                         *
 *   . accepting Java row-major 2D arrays                                    *
 *   . omitting leading dimension parameters                                 *
 *   . omitting offset parameters                                            *
 * The interface will have the same name as the numerical routine, but       *
 * it will be in all caps.                                                   *
 *                                                                           *
 *****************************************************************************/

void
emit_interface(AST *root)
{
  enum returntype returns;
  AST *tempnode, *prev;
  char *tempstring;
  HASHNODE *hashtemp;
  FILE *intfp;
  char *intfilename;
  char *classname;
  Dlist decs, rest, tmp;
  int i;
  BOOLEAN skipped;

  decs = make_dl();
  rest = make_dl();

  classname = strdup(root->astnode.source.name->astnode.ident.name);
  intfilename = f2jalloc( strlen(classname) + 6 );
  uppercase(classname);
  strcpy(intfilename,classname);
  strcat(intfilename,".java");

  /* need to get full path name here.. unfinished */
  intfp = fopen_fullpath(intfilename,"w");
  if(!intfp) {
    perror("Unable to open file");
    exit(-1);
  }

  javaheader(intfp, "");

  if(genJavadoc) {
    fprintf(intfp,"/**\n");
    fprintf(intfp,"*<pre>\n");
    fprintf(intfp,"*<b>%s</b> is a simplified interface to the JLAPACK",
        classname);
    fprintf(intfp," routine <b>%s</b>.\n",
        root->astnode.source.name->astnode.ident.name);
    fprintf(intfp,"*This interface converts Java-style 2D row-major arrays");
    fprintf(intfp," into\n*the 1D column-major linearized arrays expected by");
    fprintf(intfp," the lower\n*level JLAPACK routines.  Using this interface");
    fprintf(intfp," also allows you\n*to omit offset and leading dimension");
    fprintf(intfp," arguments.  However, because\n*of these conversions,");
    fprintf(intfp," these routines will be slower than the low\n*level ones.");
    fprintf(intfp,"  Following is the description from the original ");
    fprintf(intfp,"Fortran\n*source.  Contact ");
    fprintf(intfp,"<a href=\"mailto:seymour@cs.utk.edu\">");
    fprintf(intfp,"seymour@cs.utk.edu</a> with any questions.\n");
    fprintf(intfp,"*<p>\n");
    tempnode = root->astnode.source.javadocComments;
    while( (tempnode != NULL) && (tempnode->nodetype == MainComment ||
                                  tempnode->nodetype == Comment))
    {
      fprintf(intfp,"* %s",tempnode->astnode.ident.name);
      tempnode = tempnode->nextstmt;
    }
    fprintf(intfp,"*</pre>\n");
    fprintf(intfp,"**/\n");
  }

  fprintf(intfp,"public class %s {\n\n", classname);

  if (root->nodetype == Function)
    fprintf (intfp, "\npublic static %s %s (",
      returnstring[root->astnode.source.returns], classname);
  else if(root->nodetype == Subroutine)
    fprintf (intfp, "\npublic static void %s (", classname);
  else
    fprintf (stderr, "emit_interface called with bad nodetype.");

  prev = NULL;
  tempnode = root->astnode.source.args;

  for (; tempnode != NULL; tempnode = tempnode->nextstmt)
  {
    skipped = FALSE;

    hashtemp = type_lookup (cur_type_table, tempnode->astnode.ident.name);
    if (hashtemp == NULL)
    {
      fprintf (stderr,"Type table is screwed (codegen.c).\n");
      fprintf (stderr,"  (looked up: %s)\n", tempnode->astnode.ident.name);
      exit (-1);
    }

    if(type_lookup(cur_external_table, tempnode->astnode.ident.name) != NULL)
      returns = OBJECT_TYPE;
    else
      returns = hashtemp->type;

    /* 
     * Check the numerical value returns.  It should not 
     * exceed the value of the enum returntypes.  
     */

    if (returns > MAX_RETURNS)
      fprintf (stderr,"Bad return value, check types.\n");

    if(omitWrappers) {
      if((hashtemp->variable->astnode.ident.arraylist == NULL) &&
        cgPassByRef(tempnode->astnode.ident.name))
          tempstring = wrapper_returns[returns];
      else
        tempstring = returnstring[returns];
    }
    else
    {
      if (hashtemp->variable->astnode.ident.arraylist == NULL)
        tempstring = wrapper_returns[returns];
      else
        tempstring = returnstring[returns];
    }

    if (hashtemp->variable->astnode.ident.arraylist == NULL) {
      if((prev != NULL) && (prev->astnode.ident.dim > 1) &&
         !strcmp(tempnode->astnode.ident.name,prev->astnode.ident.leaddim))
      {
        skipped = TRUE;
      }
      else 
      {
        if(prev != NULL)
          fprintf (intfp, ",\n");
        fprintf (intfp, "%s %s", tempstring, tempnode->astnode.ident.name);
      }
    }
    else {
      char *decstr;

      if(prev != NULL)
        fprintf (intfp, ",\n");

      /* allocate enough room for:                                          */
      /*                                                                    */
      /* the data type ('double' etc.)               strlen(tempstring)     */
      /* plus a space                                                 1     */
      /* two for the brackets: "[]"                                   2     */
      /* plus a space                                                 1     */
      /* one for the leading "_"                                      1     */
      /* plus the var name                                 strlen(name)     */
      /* five for the "_copy"                                         5     */
      /* plus a space                                                 1     */
      /* the equals sign                                              1     */
      /* plus a space                                                 1     */
      /* plus the "TwoDtoOneD" call                                  28     */
      /* open paren                                                   1     */
      /* argument name                                     strlen(name)     */ 
      /* close paren                                                  1     */
      /* semicolon                                                    1     */
      /* NULL termination                                             1     */
      /* ----------------------------------------------------------------   */
      /* Total             45 + (2 * strlen(name)) + strlen(tempstring)     */

      if(hashtemp->variable->astnode.ident.dim > 1) {
        decstr = (char *) f2jalloc(45 + (2 * strlen(tempnode->astnode.ident.name)) 
          + strlen(tempstring));
        sprintf(decstr,"%s [] _%s_copy = MatConv.%sTwoDtoOneD(%s);",
          tempstring, tempnode->astnode.ident.name, 
          returnstring[returns], tempnode->astnode.ident.name);

        dl_insert_b(decs, (void *) strdup(decstr));

        if(cgPassByRef(tempnode->astnode.ident.name)) {
          /* decstr should already have enough storage for the following string.  */

          sprintf(decstr,"MatConv.copyOneDintoTwoD(%s,_%s_copy);",
            tempnode->astnode.ident.name, tempnode->astnode.ident.name);

          dl_insert_b(rest, (void *) strdup(decstr));
        }
      }

      if(hashtemp->variable->astnode.ident.dim > 2)
        fprintf(stderr,
           "Cant correctly generate interface with array over 2 dimensions\n");

      fprintf (intfp, "%s ", tempstring);

      for(i = 0; i < hashtemp->variable->astnode.ident.dim; i++ )
        fprintf(intfp,"[]");

      fprintf(intfp, " %s", tempnode->astnode.ident.name);

      if(!noOffset && (hashtemp->variable->astnode.ident.dim == 1)) {
        char * temp2 = (char *) f2jalloc(strlen(tempnode->astnode.ident.name) + 9);
                
        strcpy( temp2, "_");
        strcat( temp2, tempnode->astnode.ident.name);
        strcat( temp2, "_offset");
        fprintf(intfp, ", int %s",temp2);
      }
    }

    prev = hashtemp->variable;
  }

  fprintf (intfp, ")  {\n\n");
    
  if (root->nodetype == Function)
    fprintf (intfp, "\n%s _retval;\n",
      returnstring[root->astnode.source.returns]);

  /* Emit all the 2D -> 1D conversion method calls */

  dl_traverse (tmp, decs)
    fprintf(intfp,"%s\n", (char *) dl_val(tmp));

  emit_methcall(intfp,root);

  /* Now emit all the 1D -> 2D conversion method calls */

  dl_traverse (tmp, rest)
    fprintf(intfp,"%s\n", (char *) dl_val(tmp));

  if (root->nodetype == Function)
    fprintf (intfp, "\nreturn _retval;\n");

  fprintf (intfp, "}\n");
  fprintf (intfp, "}\n");

  fclose(intfp);
}

/*****************************************************************************
 *                                                                           *
 * emit_methcall                                                             *
 *                                                                           *
 * This routine generates the call to a 'raw' numerical routine.             *
 * Normally this is written to the file containing the simplified            *
 * interface for that routine.                                               *
 *                                                                           *
 *****************************************************************************/

void
emit_methcall(FILE *intfp, AST *root)
{
  enum returntype returns;
  AST *tempnode, *prev;
  char *tempstring;
  HASHNODE *hashtemp;
  BOOLEAN skipped;

  if (root->nodetype == Function)
    fprintf (intfp, "_retval = ");

  tempstring = strdup(root->astnode.source.name->astnode.ident.name);
  *tempstring = toupper(*tempstring);

  fprintf(intfp,"%s.%s( ", tempstring, root->astnode.source.name->astnode.ident.name);

  prev = NULL;
  tempnode = root->astnode.source.args;

  /* for each argument */
  for (; tempnode != NULL; tempnode = tempnode->nextstmt)
  {
    skipped = FALSE;

    hashtemp = type_lookup (cur_type_table, tempnode->astnode.ident.name);
    if (hashtemp == NULL)
    {
      fprintf (stderr,"Type table is screwed (codegen.c).\n");
      fprintf (stderr,"  (looked up: %s)\n", tempnode->astnode.ident.name);
      exit (-1);
    }

    if(type_lookup(cur_external_table, tempnode->astnode.ident.name) != NULL)
      returns = OBJECT_TYPE;
    else
      returns = hashtemp->type;

    if(omitWrappers) {
      if((hashtemp->variable->astnode.ident.arraylist == NULL) &&
        cgPassByRef(tempnode->astnode.ident.name))
          tempstring = wrapper_returns[returns];
      else
        tempstring = returnstring[returns];
    }
    else
    {
      if (hashtemp->variable->astnode.ident.arraylist == NULL)
        tempstring = wrapper_returns[returns];
      else
        tempstring = returnstring[returns];
    }

    if (hashtemp->variable->astnode.ident.arraylist == NULL) {
      if((prev != NULL) && (prev->astnode.ident.dim > 1) &&
         !strcmp(tempnode->astnode.ident.name,prev->astnode.ident.leaddim))
      {
        /* If this arg follows a 2D array, pass the array's .length as the
         * leading dimension to the numerical routine.
         */

        skipped = TRUE;
        fprintf(intfp, "%s.length" , prev->astnode.ident.name);
      }
      else 
      {
        fprintf (intfp, "%s", tempnode->astnode.ident.name);
      }
    }
    else {

      if(hashtemp->variable->astnode.ident.dim > 2)
        fprintf(stderr,
           "Cant correctly generate interface with array over 2 dimensions\n");
     
      if(hashtemp->variable->astnode.ident.dim == 1)
        fprintf(intfp, " %s", tempnode->astnode.ident.name);
      else if(hashtemp->variable->astnode.ident.dim == 2)
        fprintf(intfp, " _%s_copy", tempnode->astnode.ident.name);

      if(!noOffset && (hashtemp->variable->astnode.ident.dim == 1)) {
        char * temp2 = (char *) f2jalloc(strlen(tempnode->astnode.ident.name) + 9);
                
        strcpy( temp2, "_");
        strcat( temp2, tempnode->astnode.ident.name);
        strcat( temp2, "_offset");
        fprintf(intfp, ", %s",temp2);
      }
      else
        fprintf(intfp, ", 0");
    }

    prev = hashtemp->variable;

    /* Don't emit a comma on the last iteration. */
    if(tempnode->nextstmt)
      fprintf (intfp, ", ");
  }

  fprintf (intfp, ");\n\n");
}

/*****************************************************************************
 *                                                                           *
 * forloop_emit                                                              *
 *                                                                           *
 * This function generates code to implement the fortran DO loop.            *
 * naturally, we use Java's 'for' loop for this purpose.                     *
 *                                                                           *
 * We also keep track of the nesting of for loops so that if we              *
 * encounter a goto statement within a loop, we can generate a               *
 * java 'break' or 'continue' statement.                                     *
 *                                                                           *
 * We should change the generation of for loops to match the Fortran77       *
 * spec.  For instance, the spec calls for computing the number of           *
 * iterations before the loop with the following formula:                    *
 *    MAX( INT( (stop - start + increment)/increment), 0)                    *
 * that would simplify the code in this routine a lot.  kgs 4/4/00           *
 *                                                                           *
 *****************************************************************************/

void
forloop_emit (AST * root)
{
  char *indexname;

  forloop_bytecode_emit(root);

  /* push this do loop's AST node on the stack */
  dl_insert_b(doloop, root);

  set_bytecode_status(JAVA_ONLY);

   /*  
    *  Some point I will need to test whether this is really a name
    *  because it will crash if not.  
    */
  indexname = 
      root->astnode.forloop.start->astnode.assignment.lhs->astnode.ident.name;

  fprintf(curfp, "{\n");

  if(root->astnode.forloop.incr != NULL)
  {
    fprintf(curfp,"int _%s_inc = ", indexname);
    expr_emit (root->astnode.forloop.incr);
    fprintf(curfp, ";\n");
  }

   /* print out a label for this for loop */

  fprintf(curfp, "forloop%s:\n",
     root->astnode.forloop.Label->astnode.constant.number);
   
   /* This block writes out the loop parameters.  */

  fprintf (curfp, "for (");

  assign_emit (root->astnode.forloop.start);

  fprintf(curfp, "; ");

  if(root->astnode.forloop.incr == NULL)
  {
    name_emit(root->astnode.forloop.start->astnode.assignment.lhs);

    fprintf(curfp, " <= ");
    expr_emit (root->astnode.forloop.stop);

    fprintf (curfp, "; ");

    name_emit(root->astnode.forloop.start->astnode.assignment.lhs);

    fprintf (curfp, "++");
  }
  else
  {
    /* if there is an increment the code should use >= if the
     * increment is negative and <= if the increment is positive.
     * If we determine that the increment is a constant, then
     * we can simplify the code a little by generating the correct
     * operator now.
     */

    if(root->astnode.forloop.incr->nodetype == Constant)
    {
      int increment =  atoi(root->astnode.forloop.incr->astnode.constant.number);

      name_emit(root->astnode.forloop.start->astnode.assignment.lhs);
      if(increment > 0)
        fprintf(curfp," <= ");
      else if(increment < 0)
        fprintf(curfp," >= ");
      else {
        fprintf(stderr,"WARNING: Zero increment in do loop\n");
        fprintf(curfp," /* ERROR, zero increment..following op incorrect */ <= ");
      }

      expr_emit (root->astnode.forloop.stop);

      fprintf (curfp, "; ");
      name_emit(root->astnode.forloop.start->astnode.assignment.lhs);
      fprintf (curfp, " += _%s_inc",indexname);
    }
    else {
      fprintf(curfp,"(_%s_inc < 0) ? ",indexname);
      name_emit(root->astnode.forloop.start->astnode.assignment.lhs);
      fprintf(curfp," >= ");
      expr_emit (root->astnode.forloop.stop);
      fprintf(curfp," : ");
      name_emit(root->astnode.forloop.start->astnode.assignment.lhs);
      fprintf(curfp," <= ");
      expr_emit (root->astnode.forloop.stop);
      fprintf (curfp, "; ");

      name_emit(root->astnode.forloop.start->astnode.assignment.lhs);
      fprintf (curfp, " += _%s_inc",indexname);
    }
  }

  fprintf (curfp, ") {\n");

  set_bytecode_status(JAVA_AND_JVM);
   /*  Done with loop parameters.  */
}

/*****************************************************************************
 *                                                                           *
 * forloop_bytecode_emit                                                     *
 *                                                                           *
 * this function emits the bytecode to begin a for loop.  here we only       *
 * generate the initial code that comes before the body of the loop:         *
 *   - initialization of loop variable                                       *
 *   - calculation of increment count                                        *
 *   - goto (branch to end of loop to test for loop completion)              *
 *                                                                           *
 *****************************************************************************/

void
forloop_bytecode_emit(AST *root) 
{
  set_bytecode_status(JVM_ONLY);

  /* emit the initialization assignment for the loop variable */
  assign_emit(root->astnode.forloop.start);

  /* now emit the expression to calculate the number of 
   * iterations that this loop should make and store the result
   * into the next available local variable.
   */
  expr_emit(root->astnode.forloop.iter_expr);
  root->astnode.forloop.localvar = getNextLocal(Integer);
  gen_store_op(root->astnode.forloop.localvar, Integer);

  /* goto the end of the loop where we test for completion */
  root->astnode.forloop.goto_node = bytecode0(jvm_goto);

  set_bytecode_status(JAVA_AND_JVM);
}

/*****************************************************************************
 *                                                                           *
 * goto_emit                                                                 *
 *                                                                           *
 * Since gotos aren't supported by java, we can't just emit a goto here.     *
 * labeled continues and breaks are supported in java, but only in certain   *
 * cases.  so, if we are within a loop, and we are trying to goto the        *
 * CONTINUE statement of an enclosing loop, then we can just emit a labeled  *
 * continue statement.  --Keith                                              *
 *                                                                           *
 * I think I fixed a previous problem emitting gotos within nested           *
 * simulated while loops by keeping track of all if statements rather than   *
 * just the ones identified as while statements.   10/3/97 -- Keith          *
 *                                                                           *
 *****************************************************************************/

void
goto_emit (AST * root)
{
  CodeGraphNode *goto_node;
  AST *loop;

  /* for bytecode, maintain a list of the gotos so that we can come back
   * later and resolve the branch targets.
   */
  goto_node = bytecode0(jvm_goto);
  goto_node->branch_target = NULL;
  goto_node->branch_label = root->astnode.go_to.label;
   
printf("## setting branch_label of this node to %d\n", goto_node->branch_label);

  if( (loop = label_search(doloop, root->astnode.go_to.label)) != NULL)
  {
    /*
     *  we are inside a do loop and we are looking at a goto
     *  statement to the 'continue' statement of an enclosing loop.
     *  what we want to do here is just emit a 'labeled continue' 
     */ 

    /*
     * fprintf(curfp,"continue forloop%d;\n",root->astnode.go_to.label);
     */

    /* well... in order to allow the continuation statement of the DO loop
     * to be any arbitrary statement, we cannot translate this to a labeled
     * continue because the statement must be executed before continuing
     * the loop (and JAva's continue statement will not do that for us).
     */
    fprintf(curfp,"Dummy.go_to(\"%s\",%d);\n",cur_filename,
        root->astnode.go_to.label);
  }
  else if((!dl_empty(while_list)) && 
     (dl_int_examine(while_list) == root->astnode.go_to.label ))
  {
    /* 
     *  we are inside a simulated while loop and we are looking at 
     *  a goto statement to the 'beginning' statement of the most
     *  enclosing if statment.  Since we are translating this to an 
     *  actual while loop, we ignore this goto statement 
     */

    fprintf(curfp,"// goto %d (end while)\n",root->astnode.go_to.label);
  }
  else 
  {
    /*  
     *  otherwise, not quite sure what to do with this one, so
     *  we'll just emit a dummy goto 
     */

    fprintf(curfp,"Dummy.go_to(\"%s\",%d);\n",cur_filename,
        root->astnode.go_to.label);
  }
}

/*****************************************************************************
 *                                                                           *
 * computed_goto_emit                                                        *
 *                                                                           *
 * This function generates code to implement fortran's computed              *
 * GOTO statement.   we simply use a series of if-else statements            *
 * to implement the computed goto.                                           *
 *                                                                           *
 *****************************************************************************/

void
computed_goto_emit (AST *root)
{
  CodeGraphNode *if_node, *goto_node;
  AST *temp;
  unsigned int lvar, count = 1;

  lvar = getNextLocal(Integer);

  fprintf(curfp,"{\n");
  fprintf(curfp,"  int _cg_tmp = ");

  if(root->astnode.computed_goto.name->vartype != Integer) {
    fprintf(curfp,"(int)( ");
    expr_emit(root->astnode.computed_goto.name);
    bytecode0(typeconv_matrix[root->astnode.computed_goto.name->vartype][Integer]);
    fprintf(curfp,")");
  }
  else
    expr_emit(root->astnode.computed_goto.name);
  
  gen_store_op(lvar, Integer);
  fprintf(curfp,";\n");

  for(temp = root->astnode.computed_goto.intlist;temp!=NULL;temp=temp->nextstmt)
  {
    if(temp != root->astnode.computed_goto.intlist)
      fprintf(curfp,"else ");
    fprintf(curfp,"if (_cg_tmp == %d) \n", count);
    fprintf(curfp,"  Dummy.go_to(\"%s\",%s);\n", cur_filename, 
      temp->astnode.constant.number);
    gen_load_op(lvar, Integer);
    pushIntConst(count);
    if_node = bytecode0(jvm_if_icmpne);

    goto_node = bytecode0(jvm_goto);
    goto_node->branch_target = NULL;
    goto_node->branch_label = atoi(temp->astnode.constant.number);

    if_node->branch_target = bytecode0(jvm_impdep1);

    count++;
  }
  fprintf(curfp,"}\n");

  releaseLocal(Integer);
}

/*****************************************************************************
 *                                                                           *
 * logicalif_emit                                                            *
 *                                                                           *
 * This function generates code for IF statements.  Java and Fortran have    *
 * pretty similar if statements, so this one is simple.                      *
 *                                                                           *
 *****************************************************************************/

void
logicalif_emit (AST * root)
{
  CodeGraphNode *if_node, *next_node;

  fprintf (curfp, "if (");

  if (root->astnode.logicalif.conds != NULL)
    expr_emit (root->astnode.logicalif.conds);

  if_node = bytecode0(jvm_ifeq);

  fprintf (curfp, ")  \n    ");

  emit (root->astnode.logicalif.stmts);

  /* create a dummy instruction node following the stmts so that
   * we have a branch target for the goto statement.  it'll be
   * removed later.
   */
  next_node = bytecode0(jvm_impdep1);
  if_node->branch_target = next_node;
}

/*****************************************************************************
 *                                                                           *
 * arithmeticif_emit                                                         *
 *                                                                           *
 * This function generates code for arithmetic IF statements.                *
 *                                                                           *
 *****************************************************************************/

void
arithmeticif_emit (AST * root)
{
  CodeGraphNode *if_node, *goto_node;
  unsigned int lvar;

  lvar = getNextLocal(root->astnode.arithmeticif.cond->vartype);

  fprintf (curfp, "{\n");
  fprintf (curfp, "  %s _arif_tmp = ", 
     returnstring[root->astnode.arithmeticif.cond->vartype]);
  expr_emit(root->astnode.arithmeticif.cond);
  gen_store_op(lvar, root->astnode.arithmeticif.cond->vartype);

  fprintf (curfp, ";\n");

  fprintf (curfp, "if (_arif_tmp < 0)  \n    ");
  fprintf(curfp,"  Dummy.go_to(\"%s\",%d);\n", cur_filename,
    root->astnode.arithmeticif.neg_label);
  fprintf (curfp, "else if (_arif_tmp == 0)  \n    ");
  fprintf(curfp,"  Dummy.go_to(\"%s\",%d);\n", cur_filename,
    root->astnode.arithmeticif.zero_label);
  fprintf (curfp, "else ");
  fprintf(curfp,"  Dummy.go_to(\"%s\",%d);\n", cur_filename,
    root->astnode.arithmeticif.pos_label);

  fprintf (curfp, "}\n");

  /* arithmetic ifs may have an integer,real,or double expression.
   * since the conditionals are handled differently for integer,
   * we split the cases into integer and non-integer.
   */
  if(root->astnode.arithmeticif.cond->vartype == Integer) {
    gen_load_op(lvar, Integer);
    if_node = bytecode0(jvm_ifge);

    goto_node = bytecode0(jvm_goto);
    goto_node->branch_target = NULL;
    goto_node->branch_label = root->astnode.arithmeticif.neg_label;

    if_node->branch_target = gen_load_op(lvar, Integer);
  }
  else {
    gen_load_op(lvar, root->astnode.arithmeticif.cond->vartype);
    bytecode0(jvm_dconst_0);
    bytecode0(jvm_dcmpg);
    if_node = bytecode0(jvm_ifge);

    goto_node = bytecode0(jvm_goto);
    goto_node->branch_target = NULL;
    goto_node->branch_label = root->astnode.arithmeticif.neg_label;

    if_node->branch_target = 
       gen_load_op(lvar, root->astnode.arithmeticif.cond->vartype);
    bytecode0(jvm_dconst_0);
    bytecode0(jvm_dcmpg);
  }

  if_node = bytecode0(jvm_ifne);

  goto_node = bytecode0(jvm_goto);
  goto_node->branch_target = NULL;
  goto_node->branch_label = root->astnode.arithmeticif.zero_label;

  goto_node = bytecode0(jvm_goto);
  goto_node->branch_target = NULL;
  goto_node->branch_label = root->astnode.arithmeticif.pos_label;

  if_node->branch_target = goto_node;

  releaseLocal(root->astnode.arithmeticif.cond->vartype);
}

/*****************************************************************************
 *                                                                           *
 * label_emit                                                                *
 *                                                                           *
 * This function generates labels.  We generate both a java label            *
 * and a call to the Dummy.label() method for goto translation.              *
 *                                                                           *
 *****************************************************************************/

void
label_emit (AST * root)
{
  AST *loop;
  int num;

  num = root->astnode.label.number;

  printf("looking at label %d, pc is %d\n", num, pc);

  /* if the last node was impdep1, then that node will be replaced with
   * whatever is the next generated opcode, so we set the PC appropriately.
   */
  if(lastOp == jvm_impdep1)
    root->astnode.label.pc = pc - opWidth(jvm_impdep1);
  else
    root->astnode.label.pc = pc;


  /* if this continue statement corresponds with the most
   * recent DO loop, then this is the end of the loop - pop
   * the label off the doloop list.
   */
  loop = dl_astnode_examine(doloop);

  if((loop != NULL) &&
     (atoi(loop->astnode.forloop.Label->astnode.constant.number) == num))
  {
    /*
     * finally pop this loop's label number off the stack and
     * emit the label (for experimental goto resolution)
     */

    fprintf(curfp,"Dummy.label(\"%s\",%d);\n",cur_filename,num);
    dl_pop(doloop);

    if((root->astnode.label.stmt != NULL) &&
       (root->astnode.label.stmt->nodetype != Format))
      emit (root->astnode.label.stmt);

    fprintf(curfp, "}              //  Close for() loop. \n");
    fprintf(curfp, "}\n");

    forloop_end_bytecode(loop);
  }
  else {
    /* this labeled statement is not associated with a DO loop */

    fprintf (curfp, "label%d:\n   ", num);
    fprintf(curfp,"Dummy.label(\"%s\",%d);\n",cur_filename, num);

    if((root->astnode.label.stmt != NULL) &&
       (root->astnode.label.stmt->nodetype != Format))
    {
      emit (root->astnode.label.stmt);
    }
  }

  dl_insert_b(label_list, root);
}

/*****************************************************************************
 *                                                                           *
 * forloop_end_bytecode                                                      *
 *                                                                           *
 * bytecode-only generation of the final components of a DO loop:            *
 *  - increment loop variable                                                *
 *  - decrement and check the iteration count                                *
 *                                                                           *
 *****************************************************************************/

void
forloop_end_bytecode(AST *root)
{
  CodeGraphNode *if_node, *iload_node;
  unsigned int icount;
   
  icount = root->astnode.forloop.localvar;

  set_bytecode_status(JVM_ONLY);

  /* increment loop variable */
  assign_emit(root->astnode.forloop.incr_expr);

  /* decrement iteration count */
  iinc_emit(icount, -1);

  iload_node = gen_load_op(icount, Integer);

  root->astnode.forloop.goto_node->branch_target = iload_node;

  if_node = bytecode0(jvm_ifgt);
  if_node->branch_target = root->astnode.forloop.goto_node->next;

  releaseLocal(Integer);

  set_bytecode_status(JAVA_AND_JVM);
}

/*****************************************************************************
 *                                                                           *
 * read_emit                                                                 *
 *                                                                           *
 * This function generates READ statements.  We generate calls to a          *
 * Java class called EasyIn to perform the I/O.  Also emit a try-catch       *
 * to trap IOExceptions.                                                     *
 *                                                                           *
 *****************************************************************************/

void
read_emit (AST * root)
{
  CodeGraphNode *goto_node1, *goto_node2, *try_start, *pop_node;
  ExceptionTableEntry *et_entry;
  AST *assign_temp;
  AST *temp;
  CPNODE *c;

  /* if the READ statement has no args, just read a line and
   * ignore it.
   */

  if(root->astnode.io_stmt.arg_list == NULL) {
    fprintf(curfp,"_f2j_stdin.readString();  // skip a line\n");
    gen_load_op(stdin_lvar, Object);
    c = newMethodref(cur_const_table, EASYIN_CLASS, "readString",
          "()Ljava/lang/String;");
    bytecode1(jvm_invokevirtual, c->index);
    return;
  }

  /* if the READ statement includes an END label, then we
   * use a try block to determine EOF.  the catch block, emitted
   * below, just contains the GOTO. 
   */

  if(root->astnode.io_stmt.end_num > 0 )
  {
    fprintf(curfp,"try {\n");
    funcname = input_func_eof;
    try_start = bytecode0(jvm_impdep1);
  }
  else
    funcname = input_func;

  assign_temp = addnode();
  assign_temp->nodetype = Assignment;

  for(temp=root->astnode.io_stmt.arg_list;temp!=NULL;temp=temp->nextstmt)
  {
    if(temp->nodetype == IoImpliedLoop)
      implied_loop_emit(temp, read_implied_loop_bytecode_emit,
            read_implied_loop_sourcecode_emit);
    else if(temp->nodetype == Identifier)
    {
      temp->parent = assign_temp;
      assign_temp->astnode.assignment.lhs = temp;

      name_emit(assign_temp->astnode.assignment.lhs);

      gen_load_op(stdin_lvar, Object);
      if( (temp->vartype == Character) || (temp->vartype == String) ) {
        fprintf(curfp," = _f2j_stdin.%s(%d);\n",funcname[temp->vartype],
           temp->astnode.ident.len);
        pushIntConst(temp->astnode.ident.len);
      }
      else {
        fprintf(curfp," = _f2j_stdin.%s();\n",funcname[temp->vartype]);
      }

      c = newMethodref(cur_const_table, EASYIN_CLASS, funcname[temp->vartype],
            input_descriptors[temp->vartype]);
      bytecode1(jvm_invokevirtual, c->index);

      LHS_bytecode_emit(assign_temp);
    }
    else
    {
      fprintf(stderr,"Read list must consist of idents or implied loops\n");
      fprintf(stderr,"   nodetype is %s\n", print_nodetype(temp));
      continue;
    }
  }

  fprintf(curfp,"_f2j_stdin.skipRemaining();\n");
  gen_load_op(stdin_lvar, Object);
  c = newMethodref(cur_const_table, EASYIN_CLASS, "skipRemaining", "()V");
  bytecode1(jvm_invokevirtual, c->index);

  /* Emit the catch block for when we hit EOF.  We only care if
   * the READ statement has an END label.
   */

  if(root->astnode.io_stmt.end_num > 0)
  {
    fprintf(curfp,"} catch (java.io.IOException e) {\n");
    fprintf(curfp,"Dummy.go_to(\"%s\",%d);\n",cur_filename,
      root->astnode.io_stmt.end_num);
    fprintf(curfp,"}\n");

    goto_node1 = bytecode0(jvm_goto);  /* skip the exception handler */

    /* following is the exception handler for IOException.  this
     * implements Fortrans END specifier (eg READ(*,*,END=100)).
     * the exception handler just consists of a pop to get the stack
     * back to normal and a goto to branch to the label specified
     * in the END spec.
     */
    pop_node = bytecode0(jvm_pop);

    /* artificially set stack depth at beginning of exception
     * handler to 1.
     */
    pop_node->stack_depth = 1;

    goto_node2 = bytecode0(jvm_goto);
    goto_node2->branch_target = NULL;
    goto_node2->branch_label = root->astnode.io_stmt.end_num;

    goto_node1->branch_target = bytecode0(jvm_impdep1);

    et_entry = (ExceptionTableEntry *) f2jalloc(sizeof(ExceptionTableEntry));
    et_entry->from = try_start;
    et_entry->to = pop_node;
    et_entry->target = pop_node;
    c = cp_find_or_insert(cur_const_table,CONSTANT_Class, IOEXCEPTION);
    et_entry->catch_type = c->index;

    dl_insert_b(exc_table, et_entry);
  }
}

/*****************************************************************************
 *                                                                           *
 * read_implied_loop_bytecode_emit                                           *
 *                                                                           *
 * This function generates code for implied DO loops contained in READ       *
 * statements.  We dont handle any FORMAT statements.                        *
 *                                                                           *
 *****************************************************************************/

void
read_implied_loop_bytecode_emit(AST *node)
{
  AST *assign_temp, *temp;
  CPNODE *c;

  if(node->astnode.forloop.Label->nodetype != Identifier) {
    fprintf(stderr,"unit %s:Cant handle this nodetype (%s) ",
      unit_name,print_nodetype(node->astnode.forloop.Label));
    fprintf(stderr," in implied loop (read stmt)\n");
  }
  else {
    fprintf(curfp," = _f2j_stdin.%s();\n",
       funcname[node->astnode.forloop.Label->vartype]);
    assign_temp = addnode();
    assign_temp->nodetype = Assignment;

    temp = node->astnode.forloop.Label;
    temp->parent = assign_temp;
    assign_temp->astnode.assignment.lhs = temp;

    name_emit(assign_temp->astnode.assignment.lhs);

    gen_load_op(stdin_lvar, Object);

    if( (temp->vartype == Character) || (temp->vartype == String) )
      pushIntConst(temp->astnode.ident.len);

    c = newMethodref(cur_const_table, EASYIN_CLASS, funcname[temp->vartype],
          input_descriptors[temp->vartype]);
    bytecode1(jvm_invokevirtual, c->index);

    LHS_bytecode_emit(assign_temp);
  }
}

/*****************************************************************************
 *                                                                           *
 * read_implied_loop_sourcecode_emit                                         *
 *                                                                           *
 * This function generates code for implied DO loops contained in READ       *
 * statements.  We dont handle any FORMAT statements.                        *
 *                                                                           *
 *****************************************************************************/

void
read_implied_loop_sourcecode_emit(AST *node)
{
  if(node->astnode.forloop.Label->nodetype != Identifier) {
    fprintf(stderr,"unit %s:Cant handle this nodetype (%s) ",
      unit_name,print_nodetype(node->astnode.forloop.Label));
    fprintf(stderr," in implied loop (read stmt)\n");
  }
  else {
    name_emit(node->astnode.forloop.Label);
    fprintf(curfp," = _f2j_stdin.%s();\n",
       funcname[node->astnode.forloop.Label->vartype]);
  }
}

/*****************************************************************************
 *                                                                           *
 * one_arg_write_emit                                                        *
 *                                                                           *
 * emit write statements which have only one argument.                       *
 *                                                                           *
 *****************************************************************************/

void
one_arg_write_emit(AST *root)
{
  CPNODE *c;

  /* if the only arg is an implied loop, emit that and return...
   * nothing more to do here.
   */
  if((root->astnode.io_stmt.arg_list != NULL) &&
     (root->astnode.io_stmt.arg_list->nodetype == IoImpliedLoop)) {
    implied_loop_emit(root->astnode.io_stmt.arg_list, 
         write_implied_loop_bytecode_emit,
         write_implied_loop_sourcecode_emit);
    fprintf(curfp, "System.out.println();\n");
    c = newFieldref(cur_const_table, JL_SYSTEM, "out", OUT_DESC);
    bytecode1(jvm_getstatic, c->index);
    c = newMethodref(cur_const_table, PRINTSTREAM, "println", "()V");
    bytecode1(jvm_invokevirtual, c->index);
    return;
  }

  c = newFieldref(cur_const_table, JL_SYSTEM, "out", OUT_DESC);
  bytecode1(jvm_getstatic, c->index);

  fprintf(curfp, "System.out.println(");
  if(root->astnode.io_stmt.arg_list) {
    expr_emit(root->astnode.io_stmt.arg_list);
    if(isArrayNoIdx(root->astnode.io_stmt.arg_list)) 
      c = newMethodref(cur_const_table, PRINTSTREAM, "println",
            println_descriptor[Object]);
    else
      c = newMethodref(cur_const_table, PRINTSTREAM, "println",
            println_descriptor[root->astnode.io_stmt.arg_list->vartype]);
  }
  else {
    inline_format_emit(root, FALSE);
    c = newMethodref(cur_const_table, PRINTSTREAM, "println",
          println_descriptor[String]);
  }
  fprintf(curfp, ");\n");
  bytecode1(jvm_invokevirtual, c->index);

  return;
}

/*****************************************************************************
 *                                                                           *
 * isArrayNoIdx                                                              *
 *                                                                           *
 * returns TRUE if this is an array reference which is not indexed.          *
 *                                                                           *
 *****************************************************************************/

BOOLEAN
isArrayNoIdx(AST *var)
{
  return( (var->token == NAME) && 
        (type_lookup(cur_array_table, var->astnode.ident.name) != NULL) &&
        (var->astnode.ident.arraylist == NULL) );

}

/*****************************************************************************
 *                                                                           *
 * write_emit                                                                *
 *                                                                           *
 * This function handles WRITE statements.  It is FAR from complete,         *
 * but it is usually good enough to test the numerical routines.             *
 *                                                                           *
 *****************************************************************************/

void
write_emit(AST * root)
{
  BOOLEAN implied_loop = FALSE;
  AST *nodeptr, *temp, *prev;
  HASHNODE *hnode;
  char tmp[100];
  CPNODE *c;

  /* look for a format statement */
  sprintf(tmp,"%d", root->astnode.io_stmt.format_num);
  if(gendebug)
    printf("***Looking for format statement number: %s\n",tmp);

  hnode = format_lookup(cur_format_table,tmp);

  /* check if there are no args to this WRITE statement */
  if((root->astnode.io_stmt.arg_list == NULL) &&
     (root->astnode.io_stmt.fmt_list == NULL))
  {
    c = newFieldref(cur_const_table, JL_SYSTEM, "out", OUT_DESC);
    bytecode1(jvm_getstatic, c->index);

    if(hnode) {
      nodeptr = root->astnode.io_stmt.arg_list; 

      fprintf (curfp, "System.out.println(");
      format_emit(hnode->variable->astnode.label.stmt,&nodeptr);
      fprintf(curfp,");\n");

      c = newMethodref(cur_const_table, STRINGBUFFER, "toString", 
             TOSTRING_DESC);
      bytecode1(jvm_invokevirtual, c->index);

      c = newMethodref(cur_const_table, PRINTSTREAM, "println",
          println_descriptor[String]);
    }
    else {
      fprintf(curfp,"System.out.println();\n");
      c = newMethodref(cur_const_table, PRINTSTREAM, "println", "()V");
    }

    bytecode1(jvm_invokevirtual, c->index);

    return;
  }

  /* 
   * Check to see if there are any implied DO loops in this WRITE
   * statement.  If so, we'll have to generate a for loop to write
   * the data.  Since in that case we will have separate print
   * statements for each iteration of the for loop, we dont want
   * to use println. 
   */

  for(temp=root->astnode.io_stmt.arg_list;temp!=NULL;temp=temp->nextstmt)
    if(temp->nodetype == IoImpliedLoop) {
      implied_loop = TRUE;
      break;
    }
      
  /* check if this WRITE statement has only one arg.  treat this as
   * a special case because we do not need to generate a StringBuffer
   * if there's only one arg.
   */

  if( !hnode &&
      (((root->astnode.io_stmt.arg_list != NULL) &&
        (root->astnode.io_stmt.arg_list->nextstmt == NULL) &&
        (root->astnode.io_stmt.fmt_list == NULL))
      ||
       ((root->astnode.io_stmt.arg_list == NULL) &&
        (root->astnode.io_stmt.fmt_list != NULL))) )
  {
    one_arg_write_emit(root);
    return;
  }

  if(root->astnode.io_stmt.arg_list->nodetype != IoImpliedLoop) {
    c = newFieldref(cur_const_table, JL_SYSTEM, "out", OUT_DESC);
    bytecode1(jvm_getstatic, c->index);
  }

  if(implied_loop)
    fprintf (curfp, "System.out.print(");
  else
    fprintf (curfp, "System.out.println(");

  if(root->astnode.io_stmt.fmt_list != NULL)
    inline_format_emit(root, TRUE);

  /* if there's formatting information for this write statement, use it
   * unless the write statement has an implied do loop.  in that case,
   * we dont know how to handle the formatting, so we ignore it.
   */

  if(hnode != NULL && !implied_loop) {
    if(gendebug)
      printf("****FOUND****\n");

    nodeptr = root->astnode.io_stmt.arg_list; 

    format_emit(hnode->variable->astnode.label.stmt,&nodeptr);
  }
  else {
    if(gendebug)
      printf("****NOT FOUND****\n");

    /* if there's a FORMAT statement and an implied loop, ignore the
     * formatting and increment a counter of the number of FORMAT
     * statements that we have ignored.
     */

    if(hnode && implied_loop)
      ignored_formatting++;

    for( temp = root->astnode.io_stmt.arg_list, 
         prev = root->astnode.io_stmt.arg_list; 
         temp != NULL; 
         temp = temp->nextstmt) 
    {
      if(temp->nodetype == IoImpliedLoop)
      {
        if( temp == root->astnode.io_stmt.arg_list )
          fprintf(curfp,"\"\");\n");

        implied_loop_emit(temp, write_implied_loop_bytecode_emit,
                                write_implied_loop_sourcecode_emit);
        if(temp->nextstmt != NULL)
          if(temp->nextstmt->nodetype != IoImpliedLoop) {
            fprintf(curfp,"System.out.print(");
            c = newFieldref(cur_const_table, JL_SYSTEM, "out", OUT_DESC);
            bytecode1(jvm_getstatic, c->index);
          }
      }
      else
      {
        fprintf(curfp,"(");

        if(((temp == root->astnode.io_stmt.arg_list) &&
           (root->astnode.io_stmt.fmt_list == NULL)) 
           || prev->nodetype == IoImpliedLoop)
        {
          /* this is the first item in the list and we do not have an inline
           * format spec, therefore we must create the new StringBuffer now.
           * The StringBuffer constructor only takes a String argument, so
           * if the first item is not a string, we must convert it to String
           * before calling the constructor.
           */

          c = cp_find_or_insert(cur_const_table,CONSTANT_Class, STRINGBUFFER);
          bytecode1(jvm_new,c->index);
          bytecode0(jvm_dup);

          expr_emit (temp);

          if((temp->vartype != String) && (temp->vartype != Character)) {
            /* call String.valueOf() to convert this numeric type to string */
            if(isArrayNoIdx(temp)) {
              c = newMethodref(cur_const_table, JL_OBJECT, "toString",
                     TOSTRING_DESC);
              bytecode1(jvm_invokevirtual, c->index);
            }
            else {
              c = newMethodref(cur_const_table, JL_STRING, "valueOf", 
                     string_valueOf_descriptor[temp->vartype]);
              bytecode1(jvm_invokestatic, c->index);
            }
          }

          c = newMethodref(cur_const_table, STRINGBUFFER, "<init>", STRBUF_DESC);
          bytecode1(jvm_invokespecial, c->index);
        }
        else {
          expr_emit (temp);

          if(isArrayNoIdx(temp))
            c = newMethodref(cur_const_table, STRINGBUFFER, "append", 
                    append_descriptor[Object]);
          else
            c = newMethodref(cur_const_table, STRINGBUFFER, "append", 
                    append_descriptor[temp->vartype]);

          bytecode1(jvm_invokevirtual, c->index);
        }
        fprintf(curfp,")");

        if(temp->nextstmt != NULL) 
        {
          pushStringConst(" ");
          c = newMethodref(cur_const_table, STRINGBUFFER, "append", 
                append_descriptor[String]);

          bytecode1(jvm_invokevirtual, c->index);

          if(temp->nextstmt->nodetype == IoImpliedLoop) {
            /* next item is an implied loop.  finish up this print statement. */
            fprintf (curfp, " + \" \");\n");

            c = newMethodref(cur_const_table, STRINGBUFFER, "toString", 
                  TOSTRING_DESC);
            bytecode1(jvm_invokevirtual, c->index);

            c = newMethodref(cur_const_table, PRINTSTREAM, "print", 
                  println_descriptor[String]);
            bytecode1(jvm_invokevirtual, c->index);
          }
          else {
            /* bytecode for this is above (pushStringConst(" "); etc.)  */
            fprintf (curfp, " + \" \" + ");
          }
        }
        else if(implied_loop) {
          fprintf (curfp, ");\n");

          c = newMethodref(cur_const_table, STRINGBUFFER, "toString", 
                TOSTRING_DESC);
          bytecode1(jvm_invokevirtual, c->index);

          c = newMethodref(cur_const_table, PRINTSTREAM, "print", 
                println_descriptor[String]);
          bytecode1(jvm_invokevirtual, c->index);
        }
      }
      prev = temp;
    }
  }

  if(implied_loop) {
    fprintf (curfp, "\nSystem.out.println();\n");
    c = newFieldref(cur_const_table, JL_SYSTEM, "out", OUT_DESC);
    bytecode1(jvm_getstatic, c->index);
    c = newMethodref(cur_const_table, PRINTSTREAM, "println", "()V");
    bytecode1(jvm_invokevirtual, c->index);
  }
  else {
    fprintf (curfp, ");\n");
    c = newMethodref(cur_const_table, STRINGBUFFER, "toString", 
          TOSTRING_DESC);
    bytecode1(jvm_invokevirtual, c->index);

    c = newMethodref(cur_const_table, PRINTSTREAM, "println", 
           println_descriptor[String]);
    bytecode1(jvm_invokevirtual, c->index);
  }
}

/*****************************************************************************
 *                                                                           *
 * inline_format_emit                                                        *
 *                                                                           *
 * The following is a cheesy workaround to handle the following type of      * 
 * statement:                                                                *
 *         write(*, FMT = '( '' Matrix types:'' )' )                         * 
 * eventually, we should handle any kind of format spec within the           * 
 * quotes, but for now we just treat the whole thing as a string.            * 
 * 12/4/97 --Keith                                                           * 
 *                                                                           * 
 *****************************************************************************/

void
inline_format_emit(AST *root, BOOLEAN use_stringbuffer)
{
  CPNODE *c;

  fprintf(curfp, "\"%s\"", root->astnode.io_stmt.fmt_list->astnode.constant.number);
    
  if(use_stringbuffer) {
    c = cp_find_or_insert(cur_const_table,CONSTANT_Class, STRINGBUFFER);
    bytecode1(jvm_new,c->index);
    bytecode0(jvm_dup);
  }

  pushStringConst(root->astnode.io_stmt.fmt_list->astnode.constant.number);

  if(root->astnode.io_stmt.arg_list != NULL)
    fprintf(curfp, " + ");

  if (use_stringbuffer) {
    c = newMethodref(cur_const_table, STRINGBUFFER, "<init>", STRBUF_DESC);
    bytecode1(jvm_invokespecial, c->index);
  }
}

/*****************************************************************************
 *                                                                           *
 * implied_loop_emit                                                         *
 *                                                                           *
 * This function generates code for implied DO loops in I/O statements.      *
 * Dont worry about FORMAT statements.                                       *
 *                                                                           *
 *****************************************************************************/

void
implied_loop_emit(AST *node, void loop_body_bytecode_emit(AST *),
                             void loop_body_sourcecode_emit(AST *) )
{
  CodeGraphNode *if_node, *goto_node, *iload_node;
  AST *temp;
  unsigned int icount;

  temp = addnode();
  temp->nodetype = Assignment;
  temp->astnode.assignment.lhs = node->astnode.forloop.counter;
  temp->astnode.assignment.lhs->parent = temp;
  temp->astnode.assignment.rhs = node->astnode.forloop.start;
  temp->astnode.assignment.rhs->parent = temp;

  set_bytecode_status(JAVA_ONLY);

  fprintf(curfp,"for("); 

  assign_emit(temp);

  fprintf(curfp,"; ");

  expr_emit(node->astnode.forloop.counter);
  fprintf(curfp," <= "); 
  expr_emit(node->astnode.forloop.stop);

  if(node->astnode.forloop.incr == NULL) {
    fprintf(curfp,"; "); 
    expr_emit(node->astnode.forloop.counter);
    fprintf(curfp,"++)\n"); 
  }
  else
  {
    fprintf(curfp,"; "); 
    expr_emit(node->astnode.forloop.counter);
    fprintf(curfp," += "); 
    expr_emit(node->astnode.forloop.incr);
    fprintf(curfp,")\n"); 
  }

  loop_body_sourcecode_emit(node);
  set_bytecode_status(JVM_ONLY);

  /* the rest of this code is only generated as bytecode.
   * first emit the initial assignment.
   */
  assign_emit(temp);

  /* now emit the expression to calculate the number of 
   * iterations that this loop should make and store the result
   * into the next available local variable.
   */
  expr_emit(node->astnode.forloop.iter_expr);
  icount = getNextLocal(Integer);
  gen_store_op(icount, Integer);

  /* goto the end of the loop where we test for completion */
  goto_node = bytecode0(jvm_goto);

  loop_body_bytecode_emit(node);

  /* increment loop variable */
  assign_emit(node->astnode.forloop.incr_expr);

  /* decrement iteration count */
  iinc_emit(icount, -1);

  iload_node = gen_load_op(icount, Integer);

  goto_node->branch_target = iload_node;

  if_node = bytecode0(jvm_ifgt);
  if_node->branch_target = goto_node->next;

  releaseLocal(Integer);
  set_bytecode_status(JAVA_AND_JVM);
}

/*****************************************************************************
 *                                                                           *
 * write_implied_loop_sourcecode_emit                                        *
 *                                                                           *
 * this function emits the body of an implied loop (basically just the       *
 * StringBuffer.append() method invocations. (Java source only)              *
 *                                                                           *
 *****************************************************************************/

void
write_implied_loop_sourcecode_emit(AST *node)
{
  if(node->astnode.forloop.Label->nodetype == Identifier) {
    fprintf(curfp,"  System.out.print(");
    name_emit(node->astnode.forloop.Label);
    fprintf(curfp," + \" \");\n");
  }
  else if(node->astnode.forloop.Label->nodetype == Constant) {
    fprintf(curfp,"  System.out.print(\"%s \");\n", 
      node->astnode.forloop.Label->astnode.constant.number);
  }
  else {
    fprintf(stderr,"unit %s:Cant handle this nodetype (%s) ",
      unit_name,print_nodetype(node->astnode.forloop.Label));
    fprintf(stderr," in implied loop (write stmt).  Exiting.\n");
    exit(-1);
  }
}

/*****************************************************************************
 *                                                                           *
 * write_implied_loop_bytecode_emit                                          *
 *                                                                           *
 * this function emits the body of an implied loop (basically just the       *
 * StringBuffer.append() method invocations. (JVM bytecode only)             *
 *                                                                           *
 *****************************************************************************/

void
write_implied_loop_bytecode_emit(AST *node)
{
  CPNODE *c;

  /* emit loop body */
  c = newFieldref(cur_const_table, JL_SYSTEM, "out", OUT_DESC);
  bytecode1(jvm_getstatic, c->index);

  c = cp_find_or_insert(cur_const_table,CONSTANT_Class, STRINGBUFFER);
  bytecode1(jvm_new,c->index);
  bytecode0(jvm_dup);

  if(node->astnode.forloop.Label->nodetype == Identifier) {
    name_emit(node->astnode.forloop.Label);
  }
  else if(node->astnode.forloop.Label->nodetype == Constant) {
    pushConst(node->astnode.forloop.Label);
  }
  else {
    fprintf(stderr,"unit %s:Cant handle this nodetype (%s) ",
      unit_name,print_nodetype(node->astnode.forloop.Label));
    fprintf(stderr," in implied loop (write stmt).  Exiting.\n");
    exit(-1);
  }

  if((node->astnode.forloop.Label->vartype != String) && 
     (node->astnode.forloop.Label->vartype != Character))
  {
    /* call String.valueOf() to convert this numeric type to string */
    c = newMethodref(cur_const_table, JL_STRING, "valueOf", 
           string_valueOf_descriptor[node->astnode.forloop.Label->vartype]);
    bytecode1(jvm_invokestatic, c->index);
  }

  c = newMethodref(cur_const_table, STRINGBUFFER, "<init>", STRBUF_DESC);
  bytecode1(jvm_invokespecial, c->index);

  pushStringConst(" ");
  c = newMethodref(cur_const_table, STRINGBUFFER, "append", 
        append_descriptor[String]);
  bytecode1(jvm_invokevirtual, c->index);

  c = newMethodref(cur_const_table, STRINGBUFFER, "toString", 
        TOSTRING_DESC);
  bytecode1(jvm_invokevirtual, c->index);

  c = newMethodref(cur_const_table, PRINTSTREAM, "print", 
        println_descriptor[String]);
  bytecode1(jvm_invokevirtual, c->index);
}

/*****************************************************************************
 *                                                                           *
 * iinc_emit                                                                 *
 *                                                                           *
 * generates an iinc instruction.  iinc takes two one-byte operands, which   *
 * we join into a single operand here.                                       *
 *                                                                           *
 *****************************************************************************/

void
iinc_emit(unsigned int idx, int inc_const)
{
  unsigned int operand;

  if((idx > 255) || (inc_const < -128) || (inc_const > 127)) {
    u2 short_const;

    bytecode0(jvm_wide);
   
    short_const = (u2) inc_const;
    operand = ((idx & 0xFFFF) << 16) | (short_const & 0xFFFF);
  }
  else
    operand = ((idx & 0xFF) << 8) | (inc_const & 0xFF);

  bytecode1(jvm_iinc, operand);
}

/*****************************************************************************
 *                                                                           *
 * gen_store_op                                                              *
 *                                                                           *
 * given the local variable number, this function generates a store opcode   *
 * to store a value to the local var.                                        *
 *                                                                           *
 *****************************************************************************/

CodeGraphNode *
gen_store_op(unsigned int lvnum, enum returntype rt)
{
  CodeGraphNode *node;

  if(lvnum > 255) {
    node = bytecode0(jvm_wide); 
    bytecode1(store_opcodes[rt], lvnum);
  } else if((lvnum >= 0) && (lvnum <= 3))
    node = bytecode0(short_store_opcodes[rt][lvnum]);
  else
    node = bytecode1(store_opcodes[rt], lvnum);

  return node;
}

/*****************************************************************************
 *                                                                           *
 * gen_load_op                                                               *
 *                                                                           *
 * given the local variable number, this function generates a load opcode    *
 * to load a value from the local var.                                       *
 *                                                                           *
 *****************************************************************************/

CodeGraphNode *
gen_load_op(unsigned int lvnum, enum returntype rt)
{
  CodeGraphNode *node;

  if(lvnum > 255) {
    node = bytecode0(jvm_wide); 
    bytecode1(load_opcodes[rt], lvnum);
  } else if((lvnum >= 0) && (lvnum <= 3))
    node = bytecode0(short_load_opcodes[rt][lvnum]);
  else
    node = bytecode1(load_opcodes[rt], lvnum);

  return node;
}

/*****************************************************************************
 *                                                                           *
 * format_emit                                                               *
 *                                                                           *
 * this function sets up the StringBuffer to hold this WRITE statement's     *
 * text and calls format_list_emit() to emit the string.                     *
 *                                                                           *
 *****************************************************************************/

void
format_emit(AST *node, AST **nptr)
{
  CPNODE *c;

  /* create a new stringbuffer with no initial value.  */
  c = cp_find_or_insert(cur_const_table,CONSTANT_Class, STRINGBUFFER);
  bytecode1(jvm_new,c->index);
  bytecode0(jvm_dup);
  c = newMethodref(cur_const_table, STRINGBUFFER, "<init>", "()V");
  bytecode1(jvm_invokespecial, c->index);

  format_list_emit(node,nptr);
}

/*****************************************************************************
 *                                                                           *
 * format_list_emit                                                          *
 *                                                                           *
 * This function loops through each format item and generates the            *
 * code to print the appropriate value(s).                                   *
 *                                                                           *
 *****************************************************************************/

void
format_list_emit(AST *node, AST **nptr)
{
  AST *temp = node;

  while(temp != NULL)
    temp = format_item_emit(temp,nptr);
}

/*****************************************************************************
 *                                                                           *
 * format_item_emit                                                          *
 *                                                                           *
 * This function generates the code to print item(s) from the                *
 * format list.                                                              *
 *                                                                           *
 *****************************************************************************/

AST *
format_item_emit(AST *temp, AST **nodeptr)
{
  CPNODE *c;
  int i;

  switch(temp->token) {
    case EDIT_DESC:
    case NAME:
      if(gendebug)
        printf("NAme/EDIT_DESC\n");
      format_name_emit(*nodeptr);
      if(*nodeptr != NULL) {
        if(gendebug)
          printf("** Advancing nodeptr ** \n");
        *nodeptr = (*nodeptr)->nextstmt;
      }
      if(temp->nextstmt != NULL)
        fprintf(curfp," + ");
      return(temp->nextstmt);
      break;
    case STRING:
      if(gendebug)
        printf("STring: %s\n",temp->astnode.constant.number);
      fprintf(curfp,"\"%s\" ",temp->astnode.constant.number);

      pushStringConst(temp->astnode.constant.number);
      c = newMethodref(cur_const_table, STRINGBUFFER, "append",
            append_descriptor[String]);
      bytecode1(jvm_invokevirtual, c->index);

      if(temp->nextstmt != NULL)
        fprintf(curfp," + ");
      return(temp->nextstmt);
      break;
    case REPEAT:
      if(gendebug)
        printf("Repeat %d\n",temp->astnode.label.number);
      for(i=0;i<temp->astnode.label.number;i++) {
        format_list_emit(temp->astnode.label.stmt,nodeptr);

        if((i < temp->astnode.label.number -1) || 
          ((i == temp->astnode.label.number -1) && (temp->nextstmt != NULL)))
              fprintf(curfp," + ");
      }
      return(temp->nextstmt);
      break;
    case INTEGER:
      if(gendebug)
        printf("INteger %d\n",atoi(temp->astnode.constant.number));

      if(temp->nextstmt != NULL) {
        if(temp->nextstmt->token != REPEAT) {
          if(temp->nextstmt->astnode.ident.name[0] == 'X') {
            char *tmpbuf, *bi;
 
            /* allocate enough space for the given repeat spec, plus
             * 2 quotes, plus a null terminator.
             */
            tmpbuf = (char *)f2jalloc((unsigned int)atoi(temp->astnode.constant.number)+3);

            sprintf(tmpbuf,"\"%*s\"",atoi(temp->astnode.constant.number)," ");

            fprintf(curfp,"%s",tmpbuf);

            bi = (char *)f2jalloc(strlen(tmpbuf) - 1);
            strncpy(bi, tmpbuf + 1, strlen(tmpbuf) -2);
            bi[strlen(tmpbuf) - 2] = '\0';

            pushStringConst(bi);
            c = newMethodref(cur_const_table, STRINGBUFFER, "append",
                  append_descriptor[String]);
            bytecode1(jvm_invokevirtual, c->index);

            f2jfree(tmpbuf, strlen(tmpbuf)+1);
            f2jfree(bi, strlen(bi)+1);

            if(temp->nextstmt->nextstmt != NULL)
              fprintf(curfp," + ");
            temp=temp->nextstmt;  /* consume edit desc */
          }
          else if(temp->nextstmt->astnode.ident.name[0] == 'P') {
            temp=temp->nextstmt;  /* consume edit desc */
          }
        }
      }
      else {
        fprintf(stderr,"Bad format spec!\n");
        fprintf(curfp," );\n");
        return(temp->nextstmt);
      }

      return(temp->nextstmt);
      break;
    case CM:
      if(gendebug)
        printf("Comma\n");
      return(temp->nextstmt);
      break;
    case DIV:
      if(gendebug)
        printf("Div\n");
      fprintf(curfp,"\"\\n\" ");
      pushStringConst("\n ");
      c = newMethodref(cur_const_table, STRINGBUFFER, "append",
            append_descriptor[String]);
      bytecode1(jvm_invokevirtual, c->index);
      if(temp->nextstmt != NULL)
        fprintf(curfp," + ");
      return(temp->nextstmt);
      break;
    case CAT:
      if(gendebug)
        printf("two divs\n");
      fprintf(curfp,"\"\\n\\n\" ");
      pushStringConst("\n\n ");
      c = newMethodref(cur_const_table, STRINGBUFFER, "append",
            append_descriptor[String]);
      bytecode1(jvm_invokevirtual, c->index);
      if(temp->nextstmt != NULL)
        fprintf(curfp," + ");
      return(temp->nextstmt);
      break;
    default:
      fprintf(stderr,"format_item_emit: Unknown token!!! %d (%s) - ",
         temp->token, tok2str(temp->token));
      if(gendebug)
        printf("this node type %s\n",print_nodetype(temp));
      return(temp->nextstmt);
      break;
  }
}

/*****************************************************************************
 *                                                                           *
 * format_name_emit                                                          *
 *                                                                           *
 * This function generates the code to print a Name from the                 *
 * format list.                                                              *
 *                                                                           *
 *****************************************************************************/

void
format_name_emit(AST *node)
{
  CPNODE *c;

  if(node == NULL) {
    if(gendebug)
      printf("*** BAD FORMATTING\n");
    bad_format_count++;
    fprintf(curfp,"\" NULL \"");
    pushStringConst(" NULL ");
    c = newMethodref(cur_const_table, STRINGBUFFER, "append",
          append_descriptor[String]);
  }
  else {
      /* 
       * in the write statement is an array, with no index specified.
       * so we will keep grabbing data from the array until the end
       * of the format specification 
       */

/*  gotta get this part finished someday   10/3/97 -- Keith
     ... still not written - 
            relatively low on the priority list... 12/8/97 -- Keith

    if( (node->token == NAME) && 
        (type_lookup(cur_array_table, root->astnode.ident.name) != NULL) &&
        (root->astnode.ident.arraylist == NULL) )
    {

    
    }
    else

    ..still unfinished, but the following hack is necessary to 
    make the bytecode valid (since we're pushing an array, we should
    use the descriptor with the Object argument).
*/
    fprintf(curfp,"(");
    expr_emit(node);

    if( (node->token == NAME) && 
        (type_lookup(cur_array_table, node->astnode.ident.name) != NULL) &&
        (node->astnode.ident.arraylist == NULL) )
      c = newMethodref(cur_const_table, STRINGBUFFER, "append",
            append_descriptor[Object]);
    else
      c = newMethodref(cur_const_table, STRINGBUFFER, "append",
            append_descriptor[node->vartype]);
    fprintf(curfp,")");
  }
  bytecode1(jvm_invokevirtual, c->index);

  pushStringConst(" ");
  c = newMethodref(cur_const_table, STRINGBUFFER, "append",
        append_descriptor[String]);
  bytecode1(jvm_invokevirtual, c->index);

  fprintf(curfp," + \" \" ");
}

/*****************************************************************************
 *                                                                           *
 * blockif_emit                                                              *
 *                                                                           *
 * This function generates the code which implements fortran's               *
 * block if.  This could also be a simulated while loop, which               *
 * is why we push this loop's number on the while_list.  This                *
 * way we can generate a java 'while' loop instead of the                    *
 * simulated while loop using gotos.                                         *
 *                                                                           *
 *****************************************************************************/

void
blockif_emit (AST * root)
{
  CodeGraphNode *if_node, *next_node, *goto_node;
  AST *prev = root->prevstmt;
  int *tmp_int;
  Dlist gotos, lptr;
  AST *temp;

  /* in bytecode, each if-block and elseif-block must have a goto at
   * the end to branch to the statement following the end if.  since we
   * cannot know the PC of that statement until we've generated all
   * the if-blocks, elseif-blocks, and else-block, we maintain a list
   * of the gotos so that we may go back and fill in the branch targets.
   */
  gotos = make_dl();

  /* first check if the if-block is NULL.  if so, this cannot be a
   * simulated while loop because the existence of a goto would cause
   * the if-block to be non-null.
   */
  if(root->astnode.blockif.stmts != NULL) {
    /* if the previous node was a label, this could be a simulated
     * while loop.
     */
    if(prev != NULL) {
      if(prev->nodetype == Label) {
        tmp_int = (int*)f2jalloc(sizeof(int));

        *tmp_int = root->prevstmt->astnode.label.number;
  
        /* push this while loop's number on the stack */
    
        dl_insert_b(while_list, tmp_int);
  
        if(prev->astnode.label.stmt == NULL)
          if((root->astnode.blockif.elseifstmts == NULL) &&
             (root->astnode.blockif.elsestmts == NULL))
          {
             /* it appears that we are looking at a simulated while loop.
              * bypass all the statements in the body of this if block 
              * and look at the last one.  if it is a goto and the
              * target is the label of the current if statement, then
              * we generate a Java while loop.  otherwise, we generate
              * an if statement.
              */
  
            for
             (
              temp=root->astnode.blockif.stmts;
              temp->nextstmt!=NULL;
              temp = temp->nextstmt
             )
                ; /* do nothing */
  
            if(temp->nodetype == Goto)
              if(temp->astnode.go_to.label == prev->astnode.label.number) {
                while_emit(root);
                return;
              }
          }
  
        /* pop this while loop's label number off the stack */
        tmp_int = (int *)dl_pop(while_list);
        f2jfree(tmp_int, sizeof(int));
      }
    }
  }

  fprintf (curfp, "if (");
  if(root->astnode.blockif.conds != NULL)
    expr_emit (root->astnode.blockif.conds);

  if_node = bytecode0(jvm_ifeq);

  fprintf (curfp, ")  {\n    ");
  if(root->astnode.blockif.stmts != NULL)
    emit (root->astnode.blockif.stmts);
  goto_node = bytecode0(jvm_goto);

  dl_insert_b(gotos, goto_node);

  fprintf (curfp, "}              // Close if()\n");

  /* create a dummy instruction node so that
   * we have a branch target for the goto statement.
   * it will be removed later.
   */
  next_node = bytecode0(jvm_impdep1);
  if_node->branch_target = next_node;

  for(temp = root->astnode.blockif.elseifstmts; temp != NULL; temp = temp->nextstmt)
  {
    goto_node = elseif_emit (temp);
    dl_insert_b(gotos, goto_node);
  }

  if(root->astnode.blockif.elsestmts != NULL)
    else_emit (root->astnode.blockif.elsestmts);

  next_node = bytecode0(jvm_impdep1);

  dl_traverse(lptr, gotos) {
    goto_node = (CodeGraphNode *) lptr->val;
    goto_node->branch_target = next_node;
  }

  dl_delete_list(gotos);
}

/*****************************************************************************
 *                                                                           *
 * while_emit                                                                *
 *                                                                           *
 * while_emit() is called when an if statement has been identified           *
 * as a simulated while loop, e.g.:                                          *
 *                                                                           *
 *   10 continue                                                             *
 *      if(x < 10) then                                                      *
 *         do something                                                      *
 *         x = x+1                                                           *
 *      goto 10                                                              *
 *                                                                           *
 * this can be translated into java as:                                      *
 *                                                                           *
 *   while(x<10) {                                                           *
 *     do something                                                          *
 *     x = x+1                                                               *
 *   }                                                                       *
 *                                                                           *
 * that gives us one less goto statement to worry about.  --Keith            *
 *                                                                           *
 *****************************************************************************/

void 
while_emit(AST *root)
{
  CodeGraphNode *if_node, *next_node;

  fprintf(curfp, "while (");
  if (root->astnode.blockif.conds != NULL)
    expr_emit (root->astnode.blockif.conds);
  fprintf (curfp, ")  {\n    ");
  if_node = bytecode0(jvm_ifeq);
  emit (root->astnode.blockif.stmts);

  /* create a dummy instruction node so that
   * we have a branch target for the goto statement.
   * it will be removed later.
   */
  next_node = bytecode0(jvm_impdep1);
  if_node->branch_target = next_node;

  fprintf (curfp, "}              // end while()\n");

}

/*****************************************************************************
 *                                                                           *
 * elseif_emit                                                               *
 *                                                                           *
 * This function generates the code for the fortran 'else if'                *
 * construct.                                                                *
 *                                                                           *
 *****************************************************************************/

CodeGraphNode *
elseif_emit (AST * root)
{
  CodeGraphNode *if_node, *next_node, *goto_node;

  fprintf (curfp, "else if (");
  if (root->astnode.blockif.conds != NULL)
    expr_emit (root->astnode.blockif.conds);
  if_node = bytecode0(jvm_ifeq);
  fprintf (curfp, ")  {\n    ");
  emit (root->astnode.blockif.stmts);
  fprintf (curfp, "}              // Close else if()\n");

  goto_node = bytecode0(jvm_goto);

  /* create a dummy instruction node so that we have a branch target 
   * for the conditional statement. it will be removed later.
   */
  next_node = bytecode0(jvm_impdep1);
  if_node->branch_target = next_node;

  return goto_node;
}

/*****************************************************************************
 *                                                                           *
 * else_emit                                                                 *
 *                                                                           *
 * This function generates the code for the fortran 'else'                   *
 * construct.                                                                *
 *                                                                           *
 *****************************************************************************/

void
else_emit (AST * root)
{
  fprintf (curfp, "else  {\n  ");
  emit (root->astnode.blockif.stmts);
  fprintf (curfp, "}              //  Close else.\n");
}

/*****************************************************************************
 *                                                                           *
 * method_name_emit                                                          *
 *                                                                           *
 * This function generates the correct method name for this function call.   *
 * Depending on whether adapters are necessary, we may emit the name of the  *
 * Fortran function, the name of a reflective method invocation, or an       *
 * adapter method.                                                           *
 *                                                                           *
 * Returns 1 if the Call is completely generated here, 0 otherwise.          *
 *                                                                           *
 *****************************************************************************/

int
method_name_emit (AST *root, BOOLEAN adapter)
{
  char *tempname;
  HASHNODE *ht;
  AST *temp;
  CPNODE *c;

  /* shouldn't be necessary to lowercase the name
   *   lowercase (root->astnode.ident.name);
   */

  tempname = strdup (root->astnode.ident.name);
  *tempname = toupper (*tempname);

  /* If this function was passed in as an argument, we call an
   * 'adapter' which performs the reflective method invocation..
   */

  if(type_lookup(cur_args_table, root->astnode.ident.name)) {
    if(gendebug)
      printf("@@ calling passed-in func %s\n",root->astnode.ident.name);

    /* if this function has no args, we can simplify the calling
     * process by not creating an argument array or calling a
     * method adapter.
     */

    if((root->astnode.ident.arraylist == NULL) ||
       (root->astnode.ident.arraylist->nodetype == EmptyArgList))
    {

      /* no args.  either function or subroutine. */

      ht = type_lookup(cur_external_table, root->astnode.ident.name);
      if(!ht) {
        fprintf(stderr,"(2)Error: expected to find '%s' in external table.\n",
            root->astnode.ident.name);
        exit(-1);
      }
 
      gen_load_op(ht->variable->astnode.ident.localvnum, Object);
      bytecode0(jvm_aconst_null);
      bytecode0(jvm_aconst_null);

      c = newMethodref(cur_const_table, METHOD_CLASS, "invoke",
            INVOKE_DESC);
      bytecode1(jvm_invokevirtual, c->index);

      if(root->nodetype == Call) {
        /* already called invoke().  for CALL, ignore the return value. */
        bytecode0(jvm_pop);

        fprintf(curfp,"_%s_meth.invoke(null,null);\n", root->astnode.ident.name);
      }
      else {

        c = cp_find_or_insert(cur_const_table,CONSTANT_Class,
              numeric_wrapper[root->vartype]);
        bytecode1(jvm_checkcast, c->index);

        if((root->vartype == String) || (root->vartype == Character)) {
          fprintf(curfp,"(%s)_%s_meth.invoke(null,null)",
            java_wrapper[root->vartype], root->astnode.ident.name);
        }
        else {
          fprintf(curfp,"((%s)_%s_meth.invoke(null,null)).%s()",
            java_wrapper[root->vartype], root->astnode.ident.name, 
            numericValue_method[root->vartype]);
          
          c = newMethodref(cur_const_table, numeric_wrapper[root->vartype],
                numericValue_method[root->vartype], 
                numericValue_descriptor[root->vartype]);
          bytecode1(jvm_invokevirtual, c->index);
        }
      }

      f2jfree(tempname, strlen(tempname)+1);
      return 1;
    }
    else if (root->nodetype == Call) {

      /* subroutine with args.  */

      unsigned int cnt = 0, arr_local;

      for( temp = root->astnode.ident.arraylist; temp; temp = temp->nextstmt) {
        cnt++;

        if((temp->nodetype == Identifier) && 
           (temp->astnode.ident.arraylist == NULL) &&
           type_lookup(cur_array_table, temp->astnode.ident.name))
          cnt++;
      }
      
      /* create object array to hold the args */

      fprintf(curfp," Object [] _%s_args = new Object[%d];\n",
         root->astnode.ident.name, cnt);

      pushIntConst(cnt);

      c = cp_find_or_insert(cur_const_table,CONSTANT_Class,
                "java/lang/Object");

      bytecode1(jvm_anewarray, c->index);
      arr_local = getNextLocal(Object);
      gen_store_op(arr_local,Object);

      /* foreach arg, assign that arg to an element of the object array */

      cnt = 0;
      for( temp = root->astnode.ident.arraylist; temp; temp = temp->nextstmt)
      {
        fprintf(curfp,"_%s_args[%d] = ", root->astnode.ident.name, cnt);

        gen_load_op(arr_local,Object);
        pushIntConst(cnt);

        if((temp->nodetype == Identifier) && 
           (temp->astnode.ident.arraylist == NULL) &&
           type_lookup(cur_array_table, temp->astnode.ident.name))
        {
          expr_emit (temp);
          bytecode0(jvm_aastore);

          fprintf(curfp,";\n");
          fprintf(curfp,"_%s_args[%d] = new Integer(0);\n", 
             root->astnode.ident.name, ++cnt);

          gen_load_op(arr_local,Object);
          pushIntConst(cnt);  /* incremented 2 lines above */

          c = cp_find_or_insert(cur_const_table,CONSTANT_Class,
                numeric_wrapper[Integer]);

          bytecode1(jvm_new,c->index);
          bytecode0(jvm_dup);

          c = newMethodref(cur_const_table,numeric_wrapper[Integer],
                "<init>", wrapper_descriptor[Integer]);
          pushIntConst(0);

          bytecode1(jvm_invokespecial, c->index);
        }
        else
        {
          fprintf(curfp,"new %s(", java_wrapper[temp->vartype]);

          c = cp_find_or_insert(cur_const_table,CONSTANT_Class,
                numeric_wrapper[temp->vartype]);

          bytecode1(jvm_new,c->index);
          bytecode0(jvm_dup);

          c = newMethodref(cur_const_table,numeric_wrapper[temp->vartype],
                "<init>", wrapper_descriptor[temp->vartype]);

          expr_emit (temp);
          fprintf(curfp,");\n");

          bytecode1(jvm_invokespecial, c->index);
        }

        bytecode0(jvm_aastore);

        cnt++;
      }

      ht = type_lookup(cur_external_table, root->astnode.ident.name);
      if(!ht) {
        fprintf(stderr,"(3)Error: expected to find '%s' in external table.\n",
            root->astnode.ident.name);
        exit(-1);
      }

      gen_load_op(ht->variable->astnode.ident.localvnum, Object);
      bytecode0(jvm_aconst_null);
      gen_load_op(arr_local, Object);

      c = newMethodref(cur_const_table, METHOD_CLASS, "invoke",
            INVOKE_DESC);
      bytecode1(jvm_invokevirtual, c->index);

      fprintf(curfp,"_%s_meth.invoke(null,_%s_args);\n",
        root->astnode.ident.name, root->astnode.ident.name);

      releaseLocal(Object);

      bytecode0(jvm_pop);
      f2jfree(tempname, strlen(tempname)+1);
      return 1;
    }
    else   /* function with args. */
    {
      /* add this call to the list of calls which need adapters */

      insert_methcall(methcall_list,root);

      /* no bytecode to be emitted here */

      fprintf(curfp,"%s_methcall",root->astnode.ident.name);
    }
  }
  else if( adapter )
  {
    /* we need to generate an 'adapter' which will simulate
     * passing array elements by reference.
     */

    if(gendebug)
      printf("wow, guess we need an adapter for %s.\n", 
        root->astnode.ident.name);

    insert_adapter(root);

    /* Assume all methods that are invoked are static.  */
    fprintf (curfp, "%s_adapter", root->astnode.ident.name);
  }
  else {
    METHODREF *mref = get_method_name(root, adapter);

    /* mref should always be non-null, though i guess it's
     * possible that the elements may be null.
     */

    if((mref->classname != NULL) && (strlen(mref->classname) > 0)) {
      char *t;

      t = char_substitution(mref->classname, '/', '.');
      fprintf (curfp, "%s.%s", t, root->astnode.ident.name);
      f2jfree(t, strlen(t)+1);
    }
    else
      fprintf (curfp, "%s.%s", tempname, root->astnode.ident.name);

    free_fieldref(mref);
  }

  f2jfree(tempname, strlen(tempname)+1);
  return 0;
}

/*****************************************************************************
 *                                                                           *
 * get_method_name                                                           *
 *                                                                           *
 * the method that we call depends on whether this function needs an         *
 * adapter, reflection, etc.  this function determines the correct method    *
 * name and returns it as a string.                                          *
 *                                                                           *
 *****************************************************************************/

METHODREF *
get_method_name(AST *root, BOOLEAN adapter)
{
  char *buf, *tempname;
  char *tmpdesc;
  METHODREF *newmeth;

  tempname = strdup (root->astnode.ident.name);
  *tempname = toupper (*tempname);

  buf = (char *)f2jalloc(
    MAX((strlen(tempname) + strlen(root->astnode.ident.name)), 
        (strlen(root->astnode.ident.name) + 9)) + 5);
  buf[0] = '\0';

  if(type_lookup(cur_args_table, root->astnode.ident.name)) {
    if((root->astnode.ident.arraylist->nodetype == EmptyArgList) ||
       (root->astnode.ident.arraylist == NULL)) {
      /* should not hit this */
    }
    else if (root->nodetype == Call) {
      /* should not hit this */
    }
    else {
      sprintf(buf,"%s_methcall",root->astnode.ident.name);
      newmeth = (METHODREF *)f2jalloc(sizeof(METHODREF));

      newmeth->classname = strdup(cur_filename);
      newmeth->methodname = strdup(buf);

      tmpdesc = get_desc_from_arglist(root->astnode.ident.arraylist);

      newmeth->descriptor = (char*)f2jalloc(strlen(tmpdesc) + 
        strlen(METHOD_CLASS) + strlen(field_descriptor[root->vartype][0]) + 10);
      strcpy(newmeth->descriptor, "(");
      strcat(newmeth->descriptor, "L");
      strcat(newmeth->descriptor, METHOD_CLASS);
      strcat(newmeth->descriptor, ";");
      strcat(newmeth->descriptor, tmpdesc);
      strcat(newmeth->descriptor, ")");
      strcat(newmeth->descriptor, field_descriptor[root->vartype][0]);

      printf("methcall descriptor = %s\n",newmeth->descriptor);
    }
  }
  else if(adapter)
  {
    HASHNODE *hashtemp;

    sprintf (buf, "%s_adapter", root->astnode.ident.name);
    newmeth = (METHODREF *)f2jalloc(sizeof(METHODREF));
    newmeth->classname = strdup(cur_filename);
    newmeth->methodname = strdup(buf);

    hashtemp = type_lookup(function_table, root->astnode.ident.name);

    if(hashtemp) {
      tmpdesc = get_adapter_desc(hashtemp->variable->astnode.source.descriptor,
         root->astnode.ident.arraylist);
    }
    else {
      METHODREF *mref;

      mref = find_method(root->astnode.ident.name, descriptor_table);
      if(mref)
        tmpdesc = get_adapter_desc(mref->descriptor,
           root->astnode.ident.arraylist);
      else {
        fprintf(stderr, "WARNING: could not find method descriptor\n");
        tmpdesc = "IIIIIII";  /* just some junk */
      }
    }

    newmeth->descriptor = (char*)f2jalloc(strlen(tmpdesc) + 
      strlen(field_descriptor[root->vartype][0]) + 10);
    strcpy(newmeth->descriptor, "(");
    strcat(newmeth->descriptor, tmpdesc);
    strcat(newmeth->descriptor, ")");
    if(!type_lookup(cur_type_table, root->astnode.ident.name))
      strcat(newmeth->descriptor, "V");
    else
      strcat(newmeth->descriptor, field_descriptor[root->vartype][0]);

    printf("get_method_name:  descriptor = '%s'\n",newmeth->descriptor);
  }
  else
  {
    newmeth = get_methodref(root);
  }

  f2jfree(buf,
    MAX((strlen(tempname) + strlen(root->astnode.ident.name)), 
        (strlen(root->astnode.ident.name) + 9)) + 5);
  f2jfree(tempname, strlen(tempname)+1);

  return newmeth;
}

/*****************************************************************************
 *                                                                           *
 * get_methodref                                                             *
 *                                                                           *
 * looks for a method with the given name in the function table and returns  *
 * a methodref with the appropriate class, method, and descriptor.           *
 *                                                                           *
 *****************************************************************************/

METHODREF *
get_methodref(AST *node)
{
  METHODREF *new_mref = NULL, *srch_mref;
  HASHNODE *ht;
  char *tempname;

  new_mref = (METHODREF *)f2jalloc(sizeof(METHODREF));

  /* first check the symbol table for information about this function.  */

  if( (ht = type_lookup(function_table, node->astnode.ident.name)) != NULL)
  {
    /* we found this method in the symbol table, so now we fill out the
     * methodref structure based on the symtable info. 
     */
    tempname = strdup (node->astnode.ident.name);
    *tempname = toupper (*tempname);

    new_mref->classname  = get_full_classname(tempname);
    new_mref->methodname = strdup(node->astnode.ident.name);
    new_mref->descriptor = strdup(ht->variable->astnode.source.descriptor);
  }
  else
  {
    /* we cannot find this method in the symbol table, so now we look
     * in the descriptor table, which is generated from reading the .f2j
     * files.
     */

    srch_mref = find_method(node->astnode.ident.name, descriptor_table);
    if(!srch_mref)
    {
      /* if we reach this, then we cannot find this method anywhere.
       * try to guess at the descriptor.
       */
      tempname = strdup (node->astnode.ident.name);
      *tempname = toupper (*tempname);

      new_mref->classname  = get_full_classname(tempname);
      new_mref->methodname = strdup(node->astnode.ident.name);

      tempname = get_desc_from_arglist(node->astnode.ident.arraylist);

      new_mref->descriptor = (char *)f2jalloc(strlen(tempname) + 10);

      strcpy(new_mref->descriptor,"(");
      strcat(new_mref->descriptor,tempname);
      strcat(new_mref->descriptor,")V");  /* assume void return type */
    }
    else {
      f2jfree(new_mref, sizeof(METHODREF));
      new_mref = srch_mref;
    }
  }

  if(tempname != NULL)
    f2jfree(tempname, strlen(tempname)+1);

  return new_mref;
}

/*****************************************************************************
 *                                                                           *
 * call_emit                                                                 *
 *                                                                           *
 * This procedure implements Lapack and Blas type methods.                   *
 * They are translated to static method invocations.                         *
 * This is not a portable solution, it is specific to                        *
 * routines generated by f2java.                                             *
 *                                                                           *
 *****************************************************************************/

void
call_emit (AST * root)
{
  BOOLEAN adapter = FALSE;
  METHODREF *mref;
  CPNODE *c;

  assert (root != NULL);

  if(gendebug)
    printf("@##@ in call_emit, %s\n",root->astnode.ident.name);

  adapter = needs_adapter(root);

  /* if method_name_emit() already completely generated the call, return now */

  if( method_name_emit(root, adapter) )
    return;

  if(gendebug)
    printf("@##@ call_emit, %s not already emitted\n",root->astnode.ident.name);

  if((root->astnode.ident.arraylist == NULL) ||
     (root->astnode.ident.arraylist->nodetype == EmptyArgList))
  {
    /* the arg list is empty, just emit "()" and return */

    mref = get_method_name(root, adapter);

    printf("call_emit (type: %s), got class = '%s', name = '%s'\n", 
      returnstring[root->vartype], mref->classname, mref->methodname);

    c = newMethodref(cur_const_table,mref->classname, mref->methodname,
                     mref->descriptor);

    bytecode1(jvm_invokestatic, c->index);

    if(root->nodetype == Call)
      fprintf (curfp, "();\n");
    else
      fprintf (curfp, "()");

    free_fieldref(mref);

    return;
  }

  fprintf (curfp, "(");

  /* for reflective method call adapters, the first paramter should
   * be the method to invoke.
   */

  if(type_lookup(cur_args_table, root->astnode.ident.name)) {
    HASHNODE *ht;

    fprintf(curfp,"_%s_meth",root->astnode.ident.name);

    if(root->astnode.ident.arraylist != NULL)
      fprintf(curfp,",");

    ht = type_lookup(cur_external_table, root->astnode.ident.name);
    if(!ht) {
      fprintf(stderr,"(4)Error: expected to find '%s' in external table.\n",
          root->astnode.ident.name);
      exit(-1);
    }

    gen_load_op(ht->variable->astnode.ident.localvnum, Object);
  }

  emit_call_arguments(root, adapter);

  mref = get_method_name(root, adapter);

  c = newMethodref(cur_const_table,mref->classname, mref->methodname,
                   mref->descriptor);

  bytecode1(jvm_invokestatic, c->index);

  /*  
   *  Problem here, depends on who called this procedure.
   *  When this is used by the CALL keyword, it works as
   *  written.  When used to create an external function call,
   *  it adds an extra ; and \n to the output.  Might be
   *  able to fix this by checking the nodetype. 
   */

  if(root->nodetype == Call)
    fprintf (curfp, ");\n");
  else
    fprintf (curfp, ")");

  free_fieldref(mref);
}				/*  Close call_emit().  */

/*****************************************************************************
 *                                                                           *
 * emit_call_arguments                                                       *
 *                                                                           *
 * this function attempts to find the method descriptor for the fortran      *
 * subroutine or function that we are calling.                               *
 *                                                                           *
 *****************************************************************************/

void
emit_call_arguments(AST *root, BOOLEAN adapter)
{
  METHODREF *mref;

  /* look up the function that we are calling so that we may compare
   * the parameters.
   */

  mref = get_methodref(root);

  if(gendebug)
    printf("Looking up function name %s...%s\n", root->astnode.ident.name,
       mref ? "Found" : "Not found");

  if(mref != NULL)
    emit_call_args_known(root, mref->descriptor, adapter);
  else
    emit_call_args_unknown(root);

  free_fieldref(mref);
}

/*****************************************************************************
 *                                                                           *
 * emit_call_args_known                                                      *
 *                                                                           *
 * this function emits the arguments to a method call when we know the       *
 * descriptor for the method.  in this case we can determine whether each    *
 * arg needs to be passed by reference or not.  e.g. if you pass a constant  *
 * to a method expecting an intW object, then the constant must be wrapped   *
 * in an intW before calling the method.                                     *
 *                                                                           *
 *****************************************************************************/

void
emit_call_args_known(AST *root, char *desc, BOOLEAN adapter)
{
  char *com_prefix, *dptr;
  HASHNODE *ht;
  AST *temp;

  if(gendebug)
    printf("emit_call_args_known: desc = '%s'\n", desc);

  temp = root->astnode.ident.arraylist;
  dptr = skipToken(desc);

  for( ; temp != NULL; temp = temp->nextstmt)
  {

    com_prefix = get_common_prefix(temp->astnode.ident.name);

      /* 
       * if the arg is an identifier  AND
       *    it looks like an array access AND
       *    it is in the array table
       */

    if((temp->nodetype == Identifier) && 
       (temp->astnode.ident.arraylist != NULL) && 
       (ht=type_lookup(cur_array_table, temp->astnode.ident.name)) )
    {
      arrayacc_arg_emit(temp, dptr, com_prefix, adapter);
    }

      /* 
       * else if the arg is an identifier AND
       *      it does not look like an array access AND
       *      it is in the array table
       */

    else if((temp->nodetype == Identifier) &&
            (temp->astnode.ident.arraylist == NULL) && 
            type_lookup(cur_array_table, temp->astnode.ident.name) )
    {
      arrayref_arg_emit(temp, dptr, com_prefix);
    }

      /* 
       * else if the arg is an identifier AND
       *      it does not look like an array access AND
       *      it is not in the array table
       */

    else if(omitWrappers && ((temp->nodetype == Identifier) &&
            (temp->astnode.ident.arraylist == NULL) && 
            !type_lookup(cur_array_table, temp->astnode.ident.name) ))
    {
      scalar_arg_emit(temp, dptr, com_prefix);
    }
    else if(omitWrappers && (temp->nodetype == Constant))
    {
      if(isPassByRef_desc(dptr))
      {
        CPNODE *c;

        fprintf(curfp,"new %s(", 
           wrapper_returns[get_type_from_field_desc(dptr)]);

        c = cp_find_or_insert(cur_const_table,CONSTANT_Class,
              full_wrappername[temp->vartype]);

        bytecode1(jvm_new,c->index);
        bytecode0(jvm_dup);

        c = newMethodref(cur_const_table,full_wrappername[temp->vartype], "<init>",
               wrapper_descriptor[temp->vartype]);

        expr_emit (temp);
        fprintf(curfp,")");

        bytecode1(jvm_invokespecial, c->index);
      }
      else
        expr_emit(temp);
    }
    else if(
      ((temp->nodetype == Identifier) &&
       (temp->astnode.ident.arraylist == NULL) )
       || (temp->nodetype == Constant) )
    {
      expr_emit(temp);
    }
    else if(temp->nodetype != EmptyArgList)
    {
      wrapped_arg_emit(temp, dptr);
    }
   
    /* if this arg is an array, then skip an extra token to compensate
     * for the additional integer offset arg.
     */

    if(dptr[0] == '[')
      dptr = skipToken(dptr);

    dptr = skipToken(dptr);

    if(temp->nextstmt != NULL)
      fprintf(curfp, ",");
  }
}

/*****************************************************************************
 *                                                                           *
 * arrayacc_arg_emit                                                         *
 *                                                                           *
 * this function emits an argument to a method call when the arg:            *
 *   - is an identifier  AND                                                 *
 *   - it looks like an array access AND                                     *
 *   - it is in the array table                                              *
 *                                                                           *
 *****************************************************************************/

void
arrayacc_arg_emit(AST *temp, char *dptr, char *com_prefix, BOOLEAN adapter)
{
  HASHNODE *ht;
  BOOLEAN isarg, isext;
  struct var_info *vtemp;

  isarg = type_lookup(cur_args_table, temp->astnode.ident.name) != NULL;

  ht = type_lookup(cur_array_table, temp->astnode.ident.name);

  if(gendebug)
    printf("arrayacc_arg_emit() %s - %s\n", temp->astnode.ident.name, dptr);

  vtemp = push_array_var(temp);

  if(dptr[0] == '[')     /* it is expecting an array */
  {

    func_array_emit(temp->astnode.ident.arraylist,ht,
       temp->astnode.ident.name, isarg, TRUE);
  }
  else                                /* it is not expecting an array */
  {
    /* In this case we are passing the array element to the
     * adapter, so we dont wrap it in an object.
     */

    if(omitWrappers) {
      if(adapter && isPassByRef_desc(dptr))
        isext = TRUE;
      else
        isext = FALSE;
    }
    else {
      if(adapter)
        isext = TRUE;
      else
        isext = FALSE;
    }

    func_array_emit (temp->astnode.ident.arraylist,ht, 
      temp->astnode.ident.name, isarg, isext);

    if(!isext)
      bytecode0(array_load_opcodes[temp->vartype]);
  }

  free_var_info(vtemp);
}

/*****************************************************************************
 *                                                                           *
 * arrayref_arg_emit                                                         *
 *                                                                           *
 * this function emits an argument to a method call when the arg:            *
 *    - the arg is an identifier AND                                         *
 *    - it does not look like an array access AND                            *
 *    - it is in the array table                                             *
 *                                                                           *
 *****************************************************************************/

void
arrayref_arg_emit(AST *temp, char *dptr, char *com_prefix)
{

  if(dptr[0] == '[')     /* it is expecting an array */
  {
    if(gendebug)
      printf("expecting array\n");

    expr_emit(temp);
  }
  else
  {
    struct var_info *vtemp;

    if(gendebug)
      printf("NOT expecting array\n");

    vtemp = push_array_var(temp);

    if(omitWrappers && !isPassByRef_desc(dptr)) {
      /* fprintf(curfp,"%s%s[0]",com_prefix, temp->astnode.ident.name); */
      fprintf(curfp,"[0]");
      pushIntConst(0);
      bytecode0(array_load_opcodes[temp->vartype]);
    }
    else
    {
      /* in this case, the array has no index and the corresponding
       * parameter is pass-by-reference, so we assume an index of 0
       * which would be the behavior of fortran.
       */

      pushIntConst(0);
      fprintf(curfp,",0");
      /* 
       * fprintf(curfp,"new %s(",
       *      wrapper_returns[get_type_from_field_desc(dptr)]);
       * fprintf(curfp,"%s%s[0]", com_prefix,temp->astnode.ident.name);
       * fprintf(curfp,")");
       */
    }

    free_var_info(vtemp);
  }
}

/*****************************************************************************
 *                                                                           *
 * scalar_arg_emit                                                           *
 *                                                                           *
 * this function emits an argument to a method call when the arg:            *
 *    - the arg is an identifier AND                                         *
 *    - it does not look like an array access AND                            *
 *    - it is not in the array table                                         *
 *                                                                           *
 *****************************************************************************/

void
scalar_arg_emit(AST *temp, char *dptr, char *com_prefix)
{
  if(gendebug)
    printf("scalar_arg_emit.. name = %s, dptr = %s, pass by ref = %s\n",
      temp->astnode.ident.name, dptr, cgPassByRef(temp->astnode.ident.name)?
      "yes" : "no");

  if(isPassByRef_desc(dptr) != cgPassByRef(temp->astnode.ident.name))
  {

    if(cgPassByRef(temp->astnode.ident.name)) {
      struct var_info *ainf;
      BOOLEAN isarg;

      isarg = type_lookup(cur_args_table, temp->astnode.ident.name) != NULL;

      fprintf(curfp,"%s%s.val",com_prefix,temp->astnode.ident.name);

      ainf = get_var_info(temp);

      pushVar(temp->vartype, ainf->is_arg, ainf->class, ainf->name,
        ainf->desc, ainf->localvar, TRUE);

      free_var_info(ainf);
    }
    else if(type_lookup(cur_external_table, temp->astnode.ident.name)) {
      external_emit(temp);
    }
    else
      fprintf(stderr,"Internal error: %s should not be primitive\n",
        temp->astnode.ident.name);
  }
  else
  {
    if( temp->vartype != get_type_from_field_desc(dptr) )
      fprintf(curfp,"(%s) ( ",returnstring[get_type_from_field_desc(dptr)]);

    expr_emit(temp);

    if( temp->vartype != get_type_from_field_desc(dptr) ) {
      fprintf(curfp,")");
      bytecode0(typeconv_matrix[temp->vartype][get_type_from_field_desc(dptr)]);
    }
  }
}

/*****************************************************************************
 *                                                                           *
 * wrapped_arg_emit                                                          *
 *                                                                           *
 * this function emits an argument to a method call when the arg does not    *
 * really fall into the other categories.                                    *
 *                                                                           *
 *****************************************************************************/

void
wrapped_arg_emit(AST *temp, char *dptr)
{
  enum returntype vtype = get_type_from_field_desc(dptr);
  CPNODE *c;

  /* 
   * Otherwise, use wrappers.
   */
  if(omitWrappers) {
    if(isPassByRef_desc(dptr)) {
      fprintf(curfp,"new %s(", wrapper_returns[vtype]);
      c = cp_find_or_insert(cur_const_table,CONSTANT_Class,
            full_wrappername[temp->vartype]);

      bytecode1(jvm_new,c->index);
      bytecode0(jvm_dup);

      c = newMethodref(cur_const_table,full_wrappername[temp->vartype], "<init>",
             wrapper_descriptor[temp->vartype]);
    }
  }
  else
  {
    fprintf(curfp,"new %s(", wrapper_returns[vtype]);
    c = cp_find_or_insert(cur_const_table,CONSTANT_Class,
          full_wrappername[temp->vartype]);

    bytecode1(jvm_new,c->index);
    bytecode0(jvm_dup);

    c = newMethodref(cur_const_table,full_wrappername[temp->vartype], "<init>",
           wrapper_descriptor[temp->vartype]);
  }

  if(gendebug) {
    printf("emitting wrapped expr...\n");
    printf("   wrapper type is %s\n",wrapper_returns[vtype]);
    printf("   data type is %s\n",returnstring[temp->vartype]);
  }

  /* emit a cast if necessary */

  if( temp->vartype != vtype )
    fprintf(curfp,"(%s) ( ",returnstring[vtype]);

  expr_emit(temp);

  if( temp->vartype != vtype ) {
    fprintf(curfp,")");
    bytecode0(typeconv_matrix[temp->vartype][vtype]);
  }

  if(omitWrappers) {
    if(isPassByRef_desc(dptr)) {
      fprintf(curfp,")");
      bytecode1(jvm_invokespecial, c->index);
    }
  }
  else
  {
    fprintf(curfp,")");
    bytecode1(jvm_invokespecial, c->index);
  }
}

/*****************************************************************************
 *                                                                           *
 * emit_call_args_unknown                                                    *
 *                                                                           *
 * this function emits the arguments to a method call when the descriptor    *
 * of the method is unknown.  in this case, we must guess at the appropriate *
 * types - sometimes we are correct but most of the time, there is an error. *
 *                                                                           *
 *****************************************************************************/

void
emit_call_args_unknown(AST *root)
{
  AST *temp;

  temp = root->astnode.ident.arraylist;

  for( ; temp != NULL; temp = temp->nextstmt)
  {
    if(((temp->nodetype == Identifier) && 
        (temp->astnode.ident.arraylist == NULL))
       ||
        (temp->nodetype == Constant))
    {
      expr_emit (temp);
    }
    else
    {
      if(omitWrappers) {
        expr_emit (temp);
      }
      else
      {
        fprintf(curfp,"new %s(", wrapper_returns[temp->vartype]);
        expr_emit (temp);
        fprintf(curfp,")");
      }
    }

    if(temp->nextstmt != NULL)
      fprintf(curfp, ",");
  }
}

/*****************************************************************************
 *                                                                           *
 * insert_methcall                                                           *
 *                                                                           *
 * Insert this method call into the list.   We are keeping track of          *
 * the method calls in order to generate adapter functions later.            *
 *                                                                           *
 *****************************************************************************/

void
insert_methcall(Dlist mlist, AST *root)
{
  Dlist new, p, tmplist;
  AST *temp;
  char * root_name;

  if(gendebug)
    printf("MTH: here i am in insert_methcall.  name = %s\n",
      root->astnode.ident.name);

  /* if the list of lists is empty, create a new list to
   * hold this node and insert it in the main list.
   */

  if(dl_empty(mlist)) {
    if(gendebug)
      printf("MTH: list is empty, create new one.\n");

    new = make_dl();
    dl_insert_b(new,root);
    dl_insert_b(mlist,new);
    return;
  }

  /* otherwise we must determine whether there is already
   * a call to this function in the current program unit.
   * if not, we create a new list which hangs off the main
   * list.  This new list contains pointers to all the calls
   * to that function.  if there is already a list corresponding
   * to the function, we insert this node into that list. 
   * the reason we keep _all_ the calls is because we cannot
   * know the parameters of some function that is passed in
   * as an argument.  So we must guess (we also have to guess
   * at its return type).  therefore, we keep around as many
   * calls as possible to help clear up any ambiguity.  for
   * example, if the fortran source contains a call like:
   *    x = func(12)
   * we must assume that since the constant is an integer, func
   * must take an integer parameter.  however, if there is
   * another call to func later on in the program like this:
   *    x = func(y)
   * then we can resolve the ambiguity by assuming that func's
   * parameter should have the same type as the variable y.
   */

  root_name = root->astnode.ident.name;

  dl_traverse(p,mlist) {
    tmplist = (Dlist) dl_val(p);
    temp = dl_val(dl_first(tmplist));

    if(gendebug)
      printf("MTH: temp name is %s.\n", temp->astnode.ident.name);

    if(!strcmp(temp->astnode.ident.name,root_name)) {
      /* found another function call... insert this node
       * into the current list.
       */
      if(gendebug)
        printf("MTH: found %s...inserting.\n", temp->astnode.ident.name);

      dl_insert_b(tmplist,root);
      return;
    }
  }
  
  /* we did not find another call to this function.  create
   * a new list for it.
   */

  if(gendebug)
    printf("MTH: could not find %s.\n", root->astnode.ident.name);

  new = make_dl();
  dl_insert_b(new,root);
  dl_insert_b(mlist,new);
}

/*****************************************************************************
 *                                                                           *
 * needs_adapter                                                             *
 *                                                                           *
 * This function compares the expressions in the function call with          *
 * the arguments of the function to find one specific case: attempting       *
 * to pass an array element to a function that expects a scalar.  If         *
 * we find such a case, we must generate an adapter that allows              *
 * pass by reference of the array element.  Returns 1 if this function       *
 * call needs an adapter.  If no adapter is needed or if we dont have        *
 * enough info to determine whether one is needed, this function             *
 * returns 0.                                                                *
 *                                                                           *
 *****************************************************************************/

int
needs_adapter(AST *root)
{
  HASHNODE *hashtemp;
  METHODREF *mtmp;
  AST *temp;

  /* first, check for a null parameter list.  if there are no parameters, 
   * we certainly wont need an adapter.
   */
  if((root->astnode.ident.arraylist->nodetype == EmptyArgList) ||
     (root->astnode.ident.arraylist == NULL))
    return 0;

  if(gendebug)
    printf("in needs_adapter: Looking up function name %s..\n", 
      root->astnode.ident.name);

  if((hashtemp=type_lookup(function_table, root->astnode.ident.name)) != NULL)
  {
    AST *t2;

    temp = root->astnode.ident.arraylist;
    t2=hashtemp->variable->astnode.source.args;

    for( ; temp != NULL; temp = temp->nextstmt)
    {
       if(t2 == NULL)
         break;

         /*
          * if the arg is an identifier  AND
          *    it is in the array table  AND
          *    the function is not expecting an array
          */
       if(omitWrappers) {
         if((temp->nodetype == Identifier) && 
             type_lookup(cur_array_table, temp->astnode.ident.name) &&
             !t2->astnode.ident.arraylist &&
             t2->astnode.ident.passByRef)
                return 1;
       }
       else
       {
         if((temp->nodetype == Identifier) && 
           type_lookup(cur_array_table, temp->astnode.ident.name) &&
           !t2->astnode.ident.arraylist)
              return 1;
       }

       if(t2 != NULL)
         t2 = t2->nextstmt;
    }
  }
  else {
    char *dptr;

    mtmp = find_method(root->astnode.ident.name, descriptor_table);
 
    if(mtmp) {

      if(gendebug)
        printf("needs_adapter: found descriptor '%s'\n", mtmp->descriptor);

      dptr = skipToken(mtmp->descriptor);

      temp = root->astnode.ident.arraylist;

      for( ; temp != NULL; temp = temp->nextstmt)
      {
        if(dptr == NULL)
          break;

          /*
           * if the arg is an identifier  AND
           *    it is in the array table  AND
           *    the function is not expecting an array
           */
        if(omitWrappers) {
          if((temp->nodetype == Identifier) && 
              type_lookup(cur_array_table, temp->astnode.ident.name) &&
              (dptr[0] != '[') && isPassByRef_desc(dptr))
                 return 1;
        }
        else
        {
          if((temp->nodetype == Identifier) && 
            type_lookup(cur_array_table, temp->astnode.ident.name) &&
            (dptr[0] != '['))
               return 1;
        }

        dptr = skipToken(dptr);
      }
    }
  }

  return 0;
}

/*****************************************************************************
 *                                                                           *
 * spec_emit                                                                 *
 *                                                                           *
 * This function handles code generation for specification statements.       *
 * Actually, there isn't a whole lot to do for spec statements.              *
 *                                                                           *
 * I really think this routine is obsolete and useless now.  it should       *
 * probably be removed.  --keith                                             *
 *                                                                           *
 *****************************************************************************/

void
spec_emit (AST * root)
{
  /* I am reaching every case in this switch.  */

  switch (root->astnode.typeunit.specification)
  {
    /* 
     * PARAMETER in fortran corresponds to a class
     * constant in java, that has to be declared
     * class wide outside of any method.  This is
     * currently not implemented, but the assignment
     * is made.  
     */
    case Parameter:

     /*
      * now handling parameters as part of vardec_emit.  
      * 11/3/97 --Keith
      */

      break;

     /*  
      * I am reaching these next two cases. Intrinsic, for
      * example handles stuff like Math.max, etc. 
      */
    case Intrinsic:
      name_emit (root);
      break;
    case External:
    case Implicit:
      /* do nothing for external or implicit */
      break;
  }
}

/*****************************************************************************
 *                                                                           *
 * assign_emit                                                               *
 *                                                                           *
 * This function generates the code for assignment statements.               *
 * If it looks like the lhs and rhs have different types, we                 *
 * try to provide the appropriate cast, but in some cases the                *
 * resulting code may need to be modified slightly.                          *
 *                                                                           *
 * to generate an assignment statement in bytecode, we consider              *
 * three cases:                                                              *
 *  1. LHS is a scalar, not wrapped in an object  (e.g.      a = expr)       *
 *       in this case, the RHS should be emitted first, followed by          *
 *       a store instruction to the LHS (unlike Java source where we         *
 *       generate the LHS followed by the RHS).                              *
 *  2. LHS is a scalar, wrapped in an object      (e.g.  a.val = expr)       *
 *       in this case, we push a reference to the LHS on the stack           *
 *       then emit the RHS as usual, followed by a putfield opcode           *
 *       to store the value to the 'val' field.                              *
 *  3. LHS is an array access                     (e.g.   a[x] = expr)       *
 *       in this case, we push a reference to the LHS then emit the          *
 *       index expression.  next emit the RHS and generate an                *
 *       array store instruction (e.g. iastore).                             *
 *                                                                           *
 *****************************************************************************/

void
assign_emit (AST * root)
{
  enum returntype ltype, rtype;
  CPNODE *c;

  /* this used to be a pretty simple procedure:
   *    emit LHS
   *    print = 
   *    emit RHS
   * and that was it.  but it turns out that Fortran doesn't really
   * care much if the LHS and RHS are different types.  However, Java
   * doesn't like that, so we have to insert the appropriate cast or
   * conversion if the types do not agree.
   */

  ltype = root->astnode.assignment.lhs->vartype;
  rtype = root->astnode.assignment.rhs->vartype;

  if(gendebug) {
    printf("## ## codegen: ltype = %s (%d)\n",returnstring[ltype], ltype);
    printf("## ## codegen: rtype = %s (%d)\n",returnstring[rtype], rtype);
  }

  /* handle lhs substring operations elsewhere */
  if(root->astnode.assignment.lhs->nodetype == Substring) {
    substring_assign_emit(root);
  }
  else {
    name_emit (root->astnode.assignment.lhs);
    fprintf (curfp, " = ");

    if(ltype != rtype)    /* lhs and rhs have different types */
    {

      if((ltype != String) && ((rtype == String)||(rtype==Character)))
      {
        /* non-String = String */
        fprintf(curfp,"%s.valueOf(",java_wrapper[ltype]);
        expr_emit (root->astnode.assignment.rhs);
        fprintf(curfp,").%sValue()",returnstring[ltype]);

        c = newMethodref(cur_const_table,numeric_wrapper[ltype], "valueOf",
                        wrapper_valueOf_descriptor[ltype]);

        bytecode1(jvm_invokestatic, c->index);

        c = newMethodref(cur_const_table,numeric_wrapper[ltype], 
                         numericValue_method[ltype],
                        numericValue_descriptor[ltype]);

        bytecode1(jvm_invokevirtual, c->index);
      }
      else if( (ltype == Logical) && (rtype != String) )
      {
        CodeGraphNode *if_node, *goto_node, *iconst_node, *next_node;

        /* boolean = numeric value */
        expr_emit (root->astnode.assignment.rhs);
        fprintf(curfp," == 0 ? false : true");
        if(rtype == Integer) {
          if_node = bytecode0(jvm_ifeq);
          bytecode0(jvm_iconst_0);
          goto_node = bytecode0(jvm_goto);
          iconst_node = bytecode0(jvm_iconst_1);
        }
        else if(rtype == Double) {
          bytecode0(jvm_dconst_0);
          bytecode0(jvm_dcmpl);
          if_node = bytecode0(jvm_ifne);
          bytecode0(jvm_iconst_1);
          goto_node = bytecode0(jvm_goto);
          iconst_node = bytecode0(jvm_iconst_0);
        }
        else
          fprintf(stderr,"WARNING: unsupported cast.\n");

        if_node->branch_target = iconst_node;

        /* create a dummy instruction node following the iconst so that
         * we have a branch target for the goto statement.  it'll be
         * removed later.
         */
        next_node = bytecode0(jvm_impdep1);
        goto_node->branch_target = next_node;
      }
      else
      {
        if(typeconv_matrix[rtype][ltype] == jvm_nop)
          fprintf(stderr,"WARNING: unable to handle this cast!\n");

        /* numeric value = numeric value of some other type */
        fprintf(curfp,"(%s)(",returnstring[ltype]);
        expr_emit (root->astnode.assignment.rhs);
        fprintf(curfp,")");
        bytecode0(typeconv_matrix[rtype][ltype]);
      }
    }
    else   /* lhs and rhs have same types, everything is cool */
      expr_emit (root->astnode.assignment.rhs);
  }

  LHS_bytecode_emit(root);
}

/*****************************************************************************
 *                                                                           *
 * LHS_bytecode_emit                                                         *
 *                                                                           *
 * emit the store op(s) required to store a value to the LHS of some         *
 * assignment statement.   note: this has no effect on Java source...        *
 * this is only for bytecode since we have to emit a store op after the      *
 * RHS (and possibly a LHS array ref).                                       *
 *                                                                           *
 *****************************************************************************/

void
LHS_bytecode_emit(AST *root)
{
  char *name, *class, *desc, *com_prefix;
  HASHNODE *isArg, *typenode, *ht;
  CPNODE *c;

  name = root->astnode.assignment.lhs->astnode.ident.name;

  if((typenode = type_lookup(cur_type_table, name)) != NULL)
    desc = getVarDescriptor(typenode->variable);
  else
    desc = "asdf";

  /* get the name of the common block class file, if applicable */

  com_prefix = get_common_prefix(name);

  class = cur_filename;

  isArg = type_lookup(cur_args_table,name);

  if(com_prefix[0] != '\0')
  {
    /* if this is a COMMON variable, find out the merged
     * name, if any, that we should use instead.  Names are
     * merged when different declarations of a common
     * block use different variable names.
     */

    ht = type_lookup(cur_type_table,name);
    if (ht == NULL)
      fprintf(stderr,"assign_emit:Cant find %s in type_table\n", name);
    else if(ht->variable->astnode.ident.merged_name != NULL)
      name = ht->variable->astnode.ident.merged_name;

    /* class = strdup(com_prefix); */
    class = get_full_classname(com_prefix);
    class[strlen(class)-1] = '\0';
  }

  printf("in assign_emit, class = %s, name = %s, desc = %s\n",class, name, desc);
  
  if((root->astnode.assignment.lhs->astnode.ident.arraylist == NULL) ||
     (root->astnode.assignment.lhs->nodetype == Substring))
  {
    /* LHS is not an array reference (note that the variable may be
     * an array, but it isn't being indexed here).  for bytecode,
     * we now generate a store or putfield instruction, depending
     * on whether the variable is wrapped or not.
     */
    if(omitWrappers && 
       !cgPassByRef(root->astnode.assignment.lhs->astnode.ident.name)) 
    {
      /* we know that this cannot be a local variable because otherwise it
       * would be pass by reference, given that it is the LHS of an
       * assignment.  thus, we generate a putstatic instruction.
       */
      printf("generating LHS...\n");
      printf("lhs descriptor = %s\n",desc);
      printf("isArg = %s\n",isArg?"Yes":"No");
      printf("local var #%d\n",root->astnode.assignment.lhs->astnode.ident.localvnum);

      c = newFieldref(cur_const_table, class, name, desc);
      bytecode1(jvm_putstatic, c->index);
    }
    else {
      int vt = root->astnode.assignment.lhs->vartype;
      /* this is a wrapped primitive.  the objectref and value should
       * already be sitting on the stack, so now we generate a putfield
       * instruction.
       */
      c = newFieldref(cur_const_table, full_wrappername[vt], "val", 
             val_descriptor[vt]);
      bytecode1(jvm_putfield, c->index);
    }
  }
  else {
    /* the LHS is an array access.  currently the stack holds a reference
     * to the array, the array index, and the RHS expression.  all we need
     * to do now is generate an array store instruction (e.g. iastore).
     */
    bytecode0(array_store_opcodes[root->astnode.assignment.lhs->vartype]);
  }
}

/*****************************************************************************
 *                                                                           *
 * substring_assign_emit                                                     *
 *                                                                           *
 * once upon a time, we generated some funky inline code to handle substring *
 * ops on the LHS of an assignment.  we moved that code to a method in       *
 * org.netlib.util.Util called insertString(), which takes the LHS string,   *
 * the RHS string, and the substring indices and returns the altered string. *
 *                                                                           *
 *****************************************************************************/

void
substring_assign_emit(AST *root)
{
  AST *lhs = root->astnode.assignment.lhs;
  AST *rhs = root->astnode.assignment.rhs;
  CPNODE *c;

  if(gendebug)
    printf("substring_assign_emit\n");

  name_emit(lhs);

  fprintf(curfp,"= Util.stringInsert("); 

  /* we want to call name_emit() on lhs again, but in this
   * case we don't want it treated like an lvalue, so we'll
   * just set root->astnode.assignment.lhs = NULL here
   * and call scalar_emit() directly instead.
   */
  root->astnode.assignment.lhs = NULL;
  scalar_emit(lhs, NULL);
  fprintf(curfp,",");

  /* now reset the value just in case we need it later. */
  root->astnode.assignment.lhs = lhs;

  if(rhs->vartype == Character)
  {
    /* 
     * Java's Character class doesn't have a static toString
     * method, so we have to create a new character object first.
     * 
     * currently I dont think we ever hit this case, so the code
     * here may be superfluous and is definitely untested.
     */

    /*
     *    c = cp_find_or_insert(cur_const_table,CONSTANT_Class,
     *              "java/lang/Character");
     *
     *    bytecode1(jvm_new,c->index);
     *    bytecode0(jvm_dup);
     *
     *    c = newMethodref(cur_const_table,"java/lang/Character", "<init>", "(C)V");
     *
     *    fprintf(curfp,"new Character(");
     *    expr_emit(rhs);
     *    bytecode1(jvm_invokespecial, c->index);
     *    fprintf(curfp,").toString(),");
     *    c = newMethodref(cur_const_table,"java/lang/Character", "toString",
     *                     "()Ljava/lang/String;");
     *    bytecode1(jvm_invokestatic, c->index);
     */

    /* code above is broken, use code for STring */
    expr_emit(rhs);
    fprintf(curfp,",");
  }
  else if(rhs->vartype == String)
  {
    expr_emit(rhs);
    fprintf(curfp,",");
  }
  else
  {
    fprintf(curfp,"%s.toString(", java_wrapper[rhs->vartype]);
    expr_emit(rhs);
    c = newMethodref(cur_const_table,numeric_wrapper[rhs->vartype],
                     "toString", toString_descriptor[rhs->vartype]);
    bytecode1(jvm_invokestatic, c->index);
    fprintf(curfp,"),");
  }

  expr_emit(lhs->astnode.ident.arraylist);
  fprintf(curfp,",");
  expr_emit(lhs->astnode.ident.arraylist->nextstmt);

  fprintf(curfp,")");

  c = newMethodref(cur_const_table,UTIL_CLASS, "stringInsert", INS_DESC);
  bytecode1(jvm_invokestatic, c->index);
}

/*****************************************************************************
 *                                                                           *
 * dl_int_examine                                                            *
 *                                                                           *
 * This function returns the last item in a dlist of integers.               *
 *                                                                           *
 *****************************************************************************/

int
dl_int_examine(Dlist l)
{
  return ( *( (int *) dl_val(dl_last(l)) ) );
}

/*****************************************************************************
 *                                                                           *
 * dl_astnode_examine                                                        *
 *                                                                           *
 * This function returns the last item in a dlist of astnodes.               *
 *                                                                           *
 *****************************************************************************/

AST *
dl_astnode_examine(Dlist l)
{
  if(dl_empty(l))
    return NULL;

  return ( (AST *) dl_val(dl_last(l)) );
}

/*****************************************************************************
 *                                                                           *
 * find_label                                                                *
 *                                                                           *
 * searches a list of Label nodes for the one corresponding to the given     *
 * number.  from this label node, we can get the PC of the statement         *
 * corresponding to this node.                                               *
 *                                                                           *
 *****************************************************************************/

AST *
find_label(Dlist l, int val)
{
  Dlist tmp;
  AST *v;

  dl_traverse(tmp,l) {
    v = (AST *) tmp->val;
    if(v->astnode.label.number == val)
      return v;
  }

  return NULL;
}

/*****************************************************************************
 *                                                                           *
 * label_search                                                              *
 *                                                                           *
 * searches a list of Forloop nodes for the one corresponding to the given   *
 * label (val).  returns NULL if the node is not found.                      *
 *                                                                           *
 *****************************************************************************/

AST *
label_search(Dlist l, int val)
{
  Dlist p;
  AST *v;

  dl_traverse(p,l) {
    v = (AST *) p->val;
    if( atoi( v->astnode.forloop.Label->astnode.constant.number ) == val )
      return v;
  }

  return NULL;
}

/*****************************************************************************
 *                                                                           *
 * dl_name_search                                                            *
 *                                                                           *
 * This function searches for a value in a dlist of                          *
 * AST nodes.  Returns the node if it is found, NULL                         *
 * otherwise.                                                                *
 *                                                                           *
 *****************************************************************************/

AST *
dl_name_search(Dlist l, char *name)
{
  Dlist p;

  dl_traverse(p,l)
    if( !strcmp(((AST *)p->val)->astnode.ident.name,name) )
      return p->val;

  return NULL;
}

/*****************************************************************************
 *                                                                           *
 * insert_adapter                                                            *
 *                                                                           *
 * Insert this method call into the list.   We are keeping track of          *
 * the method calls in order to generate adapter functions later.            *
 *                                                                           *
 *****************************************************************************/

void
insert_adapter(AST *node)
{
  HASHNODE *ht;
  METHODREF *tmp;
  AST *ptr;
  int found = FALSE;
  Dlist p;

  /* if there is not an adapter for this function call already in the list,
   * insert it now 
   */

  if(gendebug) {
    printf("** here we are in insert_adapter\n");
    printf("** \n");
  }

  dl_traverse(p, adapter_list )
  {
    ptr = (AST *) dl_val(p);

    if( !strcmp(ptr->astnode.ident.name, node->astnode.ident.name) )
    {
      found = TRUE;

      /* this function call is already in the list.  now we must determine
       * whether the prototypes of the adapters would be the same.  If so,
       * there's no need to insert this node in the adapter list.  If the
       * prototypes would be different, then we must insert this node.
       */
  
      if(gendebug)
        printf("** %s is already in adapter_list.  now checking args.\n",
          node->astnode.ident.name);

      if((ht=type_lookup(function_table, node->astnode.ident.name)) != NULL)
      {
        if(!adapter_insert_from_descriptor(node,ptr,ht->variable->astnode.source.descriptor)) {
          if(gendebug)
            printf("** found an equivalent adapter.  no need to insert.\n");

          return;
        }
      }
      else {
        tmp = find_method(node->astnode.ident.name, descriptor_table);

        if(tmp)
          adapter_insert_from_descriptor(node, ptr, tmp->descriptor);
        else {
          if(gendebug)
            printf("** cant find prototype...returning.\n");  
        }
                      /* cant find the prototype.  normally, I dont think */
        return;       /* this case will be reached.                       */
      }
    }
  }

  if(gendebug)
    printf("** inserting '%s' into adapter_list now.\n",
      node->astnode.ident.name);

  dl_insert_b(adapter_list,node);
}

/*****************************************************************************
 *                                                                           *
 * adapter_insert_from_descriptor                                            *
 *                                                                           *
 * this function determines whether the call pointed to by node is different *
 * from the call pointed to by ptr.                                          *
 *                                                                           *
 *****************************************************************************/

BOOLEAN
adapter_insert_from_descriptor(AST *node, AST *ptr, char *desc)
{
  int this_arg_is_arrayacc, other_arg_is_arrayacc, i;
  AST *this_call, *other_call;
  BOOLEAN diff;
  char *dptr;

  if(gendebug)
    printf("adapter_insert_from_descriptor: desc = '%s'\n", desc);

  this_call = node->astnode.ident.arraylist;
  other_call = ptr->astnode.ident.arraylist;

  dptr = skipToken(desc);

  diff = FALSE;

  for(i=0 ; this_call != NULL; this_call = this_call->nextstmt, i++)
  {
    if(dptr == NULL)
      break;

    if( other_call == NULL )
    {
      fprintf(stderr,"2:Function calls to %s in unit %s ", 
        node->astnode.ident.name, unit_name);
      fprintf(stderr,"don't have same number of params\n");
      return TRUE;
    }

    this_arg_is_arrayacc = (this_call->nodetype == Identifier) &&
          (this_call->astnode.ident.arraylist != NULL) &&
          type_lookup(cur_array_table, this_call->astnode.ident.name);

    other_arg_is_arrayacc = (other_call->nodetype == Identifier) &&
          (other_call->astnode.ident.arraylist != NULL) &&
          type_lookup(cur_array_table, other_call->astnode.ident.name);

    /* if( (dptr[0] != '[') && */
    if( (dptr[0] == 'L') &&
        (this_arg_is_arrayacc != other_arg_is_arrayacc ))
    {
      diff = TRUE;
    }

    other_call = other_call->nextstmt;

    dptr = skipToken(dptr);
  }

  return diff;
}

/*****************************************************************************
 *                                                                           *
 * emit_adapters                                                             *
 *                                                                           *
 * This function generates any adapters necessary to                         *
 * allow functions to pass array elements by reference.                      *
 *                                                                           *
 *****************************************************************************/

void
emit_adapters()
{
  char *tempname, *ret, *tmpdesc, *ret_desc, *cur_name = NULL, *cur_desc=NULL;
  struct method_info *adapter_method;
  HASHNODE *hashtemp;
  METHODREF *mref;
  Dlist p;
  AST *cval;

  dl_traverse(p,adapter_list)
  {
    cval = (AST *)dl_val(p);

    cur_name=(char *)f2jrealloc(cur_name,strlen(cval->astnode.ident.name)+10);

    strcpy(cur_name, cval->astnode.ident.name);
    strcat(cur_name, "_adapter");

    adapter_method = beginNewMethod(ACC_PRIVATE | ACC_STATIC);

    hashtemp = type_lookup(function_table, cval->astnode.ident.name);

    if(hashtemp) {
      mref = (METHODREF *)f2jalloc(sizeof(METHODREF));

      tmpdesc = get_adapter_desc(hashtemp->variable->astnode.source.descriptor, cval->astnode.ident.arraylist);

      if(hashtemp->variable->nodetype == Function)
        ret_desc = field_descriptor[hashtemp->variable->astnode.source.returns][0];
      else
        ret_desc = "V";

      cur_desc = (char *)f2jrealloc(cur_desc, strlen(tmpdesc) +
        strlen(ret_desc) + 10);

      strcpy(cur_desc,"(");
      strcat(cur_desc,tmpdesc);
      strcat(cur_desc,")");
      strcat(cur_desc,ret_desc);

      tempname = strdup( cval->astnode.ident.name );
      *tempname = toupper(*tempname);

      mref->classname = get_full_classname(tempname);
      mref->methodname = strdup(hashtemp->variable->astnode.source.name->astnode.ident.name);
      mref->descriptor = strdup(hashtemp->variable->astnode.source.descriptor);

      adapter_emit_from_descriptor(mref, cval);
    }
    else {
      printf("looking up descriptor for %s\n",cval->astnode.ident.name);

      mref = find_method(cval->astnode.ident.name, descriptor_table);

      if(mref) {
        ret = get_return_type_from_descriptor(mref->descriptor);

        printf("--- ret is '%s'\n", ret);

        if(ret[0] == 'V')
          ret_desc = "V";
        else
          ret_desc = field_descriptor[get_type_from_field_desc(ret)][0];

        /* tmpdesc = get_desc_from_arglist(cval->astnode.ident.arraylist); */
        tmpdesc = get_adapter_desc(mref->descriptor,cval->astnode.ident.arraylist);

        cur_desc = (char *)f2jrealloc(cur_desc, strlen(tmpdesc) +
          strlen(ret_desc) + 10);

        strcpy(cur_desc,"(");
        strcat(cur_desc,tmpdesc);
        strcat(cur_desc,")");
        strcat(cur_desc,ret_desc);

        adapter_emit_from_descriptor(mref, cval);
      }
      else {
        fprintf(stderr,"Could not generate adapter for '%s'\n",
           cval->astnode.ident.name);
        cur_name = "BAD_ADAPTER";
        cur_desc = "()V";
      }
    }

    endNewMethod(cur_class_file, adapter_method, cur_name, cur_desc,
         num_locals, NULL );
  }
}

/*****************************************************************************
 *                                                                           *
 * adapter_emit_from_descriptor                                              *
 *                                                                           *
 * This function generates an adapters, in situations where the prototype    *
 * cannot be found in the symbol table.  instead, we look for the descriptor *
 * in any .f2j files in F2J_SEARCH_PATH.                                     *
 *                                                                           *
 *****************************************************************************/

void
adapter_emit_from_descriptor(METHODREF *mref, AST *node)
{
  enum returntype ret_type;
  char *ret;
  int lv_temp, retval_varnum = 0;

  fprintf(curfp,"// adapter for %s%s\n", 
    node->astnode.ident.name, mref->descriptor);

  ret = get_return_type_from_descriptor(mref->descriptor);

  if((ret == NULL) || (ret[0] == '[') || (ret[0] == 'L')) {
    fprintf(stderr,"Not expecting NULL, reference, or array return type ");
    fprintf(stderr,"for adapter '%s'\n", node->astnode.ident.name);
    return;
  }

  if(ret[0] == 'V')
    fprintf(curfp,"private static void %s_adapter(", 
      node->astnode.ident.name);
  else {
    fprintf(curfp,"private static %s %s_adapter(", 
      returnstring[get_type_from_field_desc(ret)],
      node->astnode.ident.name);
    ret_type = get_type_from_field_desc(ret);
  }

  adapter_args_emit_from_descriptor( node->astnode.ident.arraylist,
    mref->descriptor);

  fprintf(curfp,")\n{\n");

  lv_temp = cur_local;

  adapter_temps_emit_from_descriptor(node->astnode.ident.arraylist,
     mref->descriptor);

  adapter_methcall_emit_from_descriptor(node, lv_temp, mref, ret);

  if(ret[0] != 'V') {
    retval_varnum = getNextLocal(ret_type);
    gen_store_op(retval_varnum, ret_type);
  }

  adapter_assign_emit_from_descriptor(node->astnode.ident.arraylist,
     lv_temp, mref->descriptor);

  if(ret[0] != 'V')
  {
    fprintf(curfp,"\nreturn %s_retval;\n",
      node->astnode.ident.name);

    gen_load_op(retval_varnum, ret_type);
    bytecode0(return_opcodes[ret_type]);
  }
  else
    bytecode0(jvm_return);

  fprintf(curfp,"}\n\n");
}

/*****************************************************************************
 *                                                                           *
 * adapter_args_emit_from_descriptor                                         *
 *                                                                           *
 * this function generates the argument list for an adapter, when the        *
 * prototype cannot be found in the symbol table.                            *
 *                                                                           *
 *****************************************************************************/

void
adapter_args_emit_from_descriptor(AST *arg, char *desc)
{
  enum returntype ctype;
  char *dptr;
  int i, lvnum;

  dptr = skipToken(desc);

  lvnum = 0;

  for(i = 0; arg != NULL ; arg = arg->nextstmt, i++)
  {
    arg->astnode.ident.localvnum = lvnum;

    if(dptr == NULL) {
      fprintf(stderr,"adapter_args_emit_from_descriptor():");
      fprintf(stderr,"mismatch between adapter call and prototype\n");
      break;
    }

    ctype = get_type_from_field_desc(dptr);

printf("adapter_args.. arg=%s dptr = '%s'\n",arg->astnode.ident.name,dptr);

    if(dptr[0] == '[') {
      fprintf(curfp,"%s [] arg%d , int arg%d_offset ",
        returnstring[get_type_from_field_desc(dptr+1)], i, i);
      lvnum += 2;
      
      /* consume the offset arg */
      dptr = skipToken(dptr);
    }
    else if ( (arg->nodetype == Identifier) &&
              /* (arg->astnode.ident.arraylist != NULL) && */
              type_lookup(cur_array_table,arg->astnode.ident.name) &&
              (dptr[0] != '[') )
    {
      if(omitWrappers && !isPassByRef_desc(dptr)) {
        fprintf(curfp,"%s arg%d ", returnstring[ctype], i);
        if(ctype == Double)
          lvnum += 2;
        else
          lvnum++;
      }
      else {
        fprintf(curfp,"%s [] arg%d , int arg%d_offset ", 
          returnstring[ctype], i, i);
        lvnum += 2;
      }
    }
    else if( type_lookup(cur_external_table, arg->astnode.ident.name) )
    {
      fprintf(curfp,"Object arg%d ", i);
      lvnum++;
    }
    else
    {
      if(omitWrappers && !isPassByRef_desc(dptr)) {
        fprintf(curfp,"%s arg%d ", returnstring[ctype], i);
        if(ctype == Double)
          lvnum += 2;
        else
          lvnum++;
      }
      else {
        fprintf(curfp,"%s arg%d ", wrapper_returns[ctype], i);
        lvnum++;
      }
    }

    dptr = skipToken(dptr);

    if(arg->nextstmt != NULL)
      fprintf(curfp,",");
  }

  num_locals = cur_local = lvnum;
}

/*****************************************************************************
 *                                                                           *
 * adapter_tmp_assign_emit                                                   *
 *                                                                           *
 * this function generates the bytecode for the assignment to a temp         *
 * variable in the adapter.   for example:                                   *
 *          _f2j_tmp3 = new intW(arg3[arg3_offset])                          *
 *                                                                           *
 *****************************************************************************/

void
adapter_tmp_assign_emit(int arglocal, enum returntype argtype)
{
  CPNODE *c;
  char *classname, *desc;

  classname = full_wrappername[argtype]; 
  desc = wrapper_descriptor[argtype];

  c = cp_find_or_insert(cur_const_table,CONSTANT_Class, classname);

  bytecode1(jvm_new,c->index);
  bytecode0(jvm_dup);

  /* emit arg%d[arg%d_offset] */
  gen_load_op(arglocal, Object);
  gen_load_op(arglocal + 1, Integer);
  bytecode0(array_load_opcodes[argtype]);

  c = newMethodref(cur_const_table, classname, "<init>", desc);

  bytecode1(jvm_invokespecial, c->index);

  /* now assign value to next local */
  gen_store_op(getNextLocal(Object), Object);
}

/*****************************************************************************
 *                                                                           *
 * adapter_temps_emit_from_descriptor                                        *
 *                                                                           *
 * this function generates the temporary variable declarations for an        *
 * adapter, when the prototype cannot be found in the symbol table.          *
 *                                                                           *
 *****************************************************************************/

void
adapter_temps_emit_from_descriptor(AST *arg, char *desc)
{
  char *dptr, *wrapper;
  int i;

  dptr = skipToken(desc);

  for(i = 0; arg != NULL ; arg = arg->nextstmt, i++)
  {
    if(dptr == NULL)
      break;

    if((arg->nodetype == Identifier) &&
       /* (arg->astnode.ident.arraylist != NULL) && */
       (type_lookup(cur_array_table,arg->astnode.ident.name) != NULL) &&
       (dptr[0] != '['))
    {
      wrapper = get_wrapper_from_desc(dptr);

      if(omitWrappers) {
        if(isPassByRef_desc(dptr)) {
          fprintf(curfp,"%s _f2j_tmp%d = new %s(arg%d[arg%d_offset]);\n", 
            wrapper, i, wrapper, i, i);
          adapter_tmp_assign_emit(arg->astnode.ident.localvnum, 
            get_type_from_field_desc(dptr));
        }
      }
      else
      {
        fprintf(curfp,"%s _f2j_tmp%d = new %s(arg%d[arg%d_offset]);\n", 
          wrapper, i, wrapper, i, i);
        adapter_tmp_assign_emit(arg->astnode.ident.localvnum, 
          get_type_from_field_desc(dptr));
      }
    }
    else if(dptr[0] == '[')
      dptr = skipToken(dptr);

    dptr = skipToken(dptr);
  }
}

/*****************************************************************************
 *                                                                           *
 * adapter_methcall_emit_from_descriptor                                     *
 *                                                                           *
 * this function generates the actual method call within the adapter.        *
 * used in the case when the prototype is not found in the symbol table.     *
 *                                                                           *
 *****************************************************************************/

void
adapter_methcall_emit_from_descriptor(AST *node, int lv_temp,
  METHODREF *mref, char *ret)
{
  char *tempname, *dptr;
  CPNODE *c;
  AST *arg;
  int i;

  tempname = strdup( node->astnode.ident.name );
  *tempname = toupper(*tempname);

  if(ret[0] == 'V')
    fprintf(curfp,"\n%s.%s(",tempname,  node->astnode.ident.name );
  else
  {
    fprintf(curfp,"%s %s_retval;\n\n", ret,
        node->astnode.ident.name);

    fprintf(curfp,"%s_retval = %s.%s(", node->astnode.ident.name,
       tempname,  node->astnode.ident.name );
  }

  dptr = skipToken(mref->descriptor);
  arg = node->astnode.ident.arraylist;

  for(i = 0; arg != NULL ; arg = arg->nextstmt, i++)
  {
    if(dptr == NULL)
      break;

    lv_temp = adapter_methcall_arg_emit(arg, i, lv_temp, dptr);

    /* skip extra field desc to compensate for offset arg */
    if(dptr[0] == '[')
      dptr = skipToken(dptr);

    dptr = skipToken(dptr);

    if(arg->nextstmt != NULL)
      fprintf(curfp,",");
  }

  fprintf(curfp,");\n\n");

  c = newMethodref(cur_const_table, mref->classname, 
            mref->methodname,mref->descriptor);
 
  bytecode1(jvm_invokestatic, c->index);
}

/*****************************************************************************
 *                                                                           *
 * adapter_methcall_arg_emit                                                 *
 *                                                                           *
 * emit the argument to an adapter methodcall.                               *
 *                                                                           *
 *****************************************************************************/

int
adapter_methcall_arg_emit(AST *arg, int i, int lv, char *dptr)
{
  if((arg->nodetype == Identifier) &&
     /* (arg->astnode.ident.arraylist != NULL) && */
     (type_lookup(cur_array_table,arg->astnode.ident.name) != NULL) &&
     (dptr[0] != '['))
  {
    if(omitWrappers && !isPassByRef_desc(dptr)) {
      fprintf(curfp,"arg%d",i);
      gen_load_op(arg->astnode.ident.localvnum, get_type_from_field_desc(dptr));
    }
    else {
      fprintf(curfp,"_f2j_tmp%d",i);
      gen_load_op(lv++, Object);
    }
  }
  else if((arg->nodetype == Identifier) &&
          (type_lookup(cur_array_table,arg->astnode.ident.name) != NULL) &&
          (dptr[0] == '['))
  {
    fprintf(curfp,"arg%d, arg%d_offset",i,i);
    gen_load_op(arg->astnode.ident.localvnum, Object);
    gen_load_op(arg->astnode.ident.localvnum+1, Integer);
  }
  else
  {
    fprintf(curfp,"arg%d",i);
    if(isPassByRef_desc(dptr))
      gen_load_op(arg->astnode.ident.localvnum, Object);
    else
      gen_load_op(arg->astnode.ident.localvnum, get_type_from_field_desc(dptr));
  }

  return lv;
}

/*****************************************************************************
 *                                                                           *
 * adapter_assign_emit_from_descriptor                                       *
 *                                                                           *
 * this function emits the final assignments back to the array elements      *
 * after the call (when we cannot find the prototype in the sybmol table).   *
 *                                                                           *
 *****************************************************************************/

void
adapter_assign_emit_from_descriptor(AST *arg, int lv_temp, char *desc)
{
  char *dptr;
  int i;

  dptr = skipToken(desc);

  for(i = 0; arg != NULL ; arg = arg->nextstmt, i++)
  {
    if(dptr == NULL)
      break;

    if((arg->nodetype == Identifier) &&
       /* (arg->astnode.ident.arraylist != NULL) && */
       (type_lookup(cur_array_table,arg->astnode.ident.name) != NULL) &&
       (dptr[0] != '['))
    {
      if(omitWrappers) {
        if(isPassByRef_desc(dptr))
          adapter_assign_emit(i, arg->astnode.ident.localvnum, lv_temp++, dptr);
      }
      else
      {
        adapter_assign_emit(i, arg->astnode.ident.localvnum, lv_temp++, dptr);
      }
    }
      /* skip extra field desc to compensate for offset arg */
    else if(dptr[0] == '[')
      dptr = skipToken(dptr);

    dptr = skipToken(dptr);
  }
}

/*****************************************************************************
 *                                                                           *
 * adapter_assign_emit                                                       *
 *                                                                           *
 * emit the assignment back to the array element.                            *
 *                                                                           *
 *****************************************************************************/

void
adapter_assign_emit(int i, int argvnum, int lv, char *dptr)
{
  enum returntype vt;
  CPNODE *c;

  fprintf(curfp,"arg%d[arg%d_offset] = _f2j_tmp%d.val;\n",i,i,i);

  vt = get_type_from_field_desc(dptr);

  gen_load_op(argvnum, Object);
  gen_load_op(argvnum+1, Integer);

  gen_load_op(lv, Object);
  c = newFieldref(cur_const_table, full_wrappername[vt], "val", 
         val_descriptor[vt]);
  bytecode1(jvm_getfield, c->index);

  bytecode0(array_store_opcodes[vt]);
}

/*****************************************************************************
 *                                                                           *
 * get_desc_from_arglist                                                     *
 *                                                                           *
 * this function generates the argument descriptors based on an the list     *
 * of arguments. note that the descriptor returned does not include the      *
 * parens or return type because in some cases, we need to prepend or append *
 * args to the descriptor.                                                   *
 *                                                                           *
 *****************************************************************************/

char *
get_desc_from_arglist(AST *list)
{
  struct _str * temp_desc = NULL;
  HASHNODE *ht;
  AST *arg;
  int dim;

  for(arg = list; arg != NULL; arg = arg->nextstmt) {
    dim = 0;

    if(omitWrappers) {
      if( arg->nodetype == Identifier ) {
        ht = type_lookup(cur_type_table,arg->astnode.ident.name);
        if(ht) {
          dim = ht->variable->astnode.ident.dim;

          temp_desc = strAppend(temp_desc, field_descriptor[ht->variable->vartype][dim]);
        }
        else {
          dim = arg->astnode.ident.dim;

          temp_desc = strAppend(temp_desc, field_descriptor[arg->vartype][dim]);
        }

      }
      else if( arg->nodetype == Constant )
        temp_desc = strAppend(temp_desc,
          field_descriptor[get_type(arg->astnode.constant.number)][0]);
      else
        temp_desc = strAppend(temp_desc,
          field_descriptor[arg->vartype][0]);
    }
    else
    {
      if( arg->nodetype == Identifier ) {
        ht = type_lookup(cur_type_table,arg->astnode.ident.name);
        if(ht) {
          dim = ht->variable->astnode.ident.dim;

          temp_desc = strAppend(temp_desc, wrapped_field_descriptor[ht->variable->vartype][dim]);
        }
        else {
          dim = arg->astnode.ident.dim;

          temp_desc = strAppend(temp_desc, wrapped_field_descriptor[arg->vartype][dim]);
        }
      }
      else if( arg->nodetype == Constant )
        temp_desc = strAppend(temp_desc,
           wrapped_field_descriptor[get_type(arg->astnode.constant.number)][0]);
      else
        temp_desc = strAppend(temp_desc,
           wrapped_field_descriptor[arg->vartype][0]);
    }

    if(dim > 0)
      temp_desc = strAppend(temp_desc, "I");
  }

  return temp_desc->val;
}


/*****************************************************************************
 *                                                                           *
 * emit_invocations                                                          *
 *                                                                           *
 * This function generates adapter functions which use reflection to         *
 * call another method.  This is used to implement passing functions as      *
 * arguments.                                                                *
 *                                                                           *
 *****************************************************************************/

void
emit_invocations(AST *root)
{
  struct method_info *inv_method;
  Dlist p, tmplist, exc_list;
  int count = 0, obj_array_varnum=0;
  char *cur_name=NULL, *cur_desc=NULL, *tmpdesc=NULL;
  CPNODE *c;
  AST *temp;

  exc_list = make_dl();
  dl_insert_b(exc_list, "java/lang/reflect/InvocationTargetException");
  dl_insert_b(exc_list, "java/lang/IllegalAccessException");

  dl_traverse(p,methcall_list) {
    tmplist = (Dlist) dl_val(p);
    
    temp = (AST *) dl_val(dl_first(tmplist));

    inv_method = beginNewMethod(ACC_PRIVATE | ACC_STATIC);

    /* allocate enough space for the name + "_methcall" and null-term */

    cur_name = (char *)f2jrealloc(cur_name,
      strlen(temp->astnode.ident.name) + 10);
    strcpy(cur_name, temp->astnode.ident.name);
    strcat(cur_name, "_methcall");

    fprintf(curfp,"// reflective method invocation for %s\n",
       temp->astnode.ident.name);
    fprintf(curfp,"private static %s %s(",
       returnstring[temp->vartype], cur_name);
    fprintf(curfp,"java.lang.reflect.Method _funcptr");

    tmpdesc = get_desc_from_arglist(temp->astnode.ident.arraylist);
    cur_desc = (char *)f2jrealloc(cur_desc, strlen(tmpdesc) +
      strlen(METHOD_CLASS) + strlen(field_descriptor[temp->vartype][0]) + 10);

    strcpy(cur_desc, "(");
    strcat(cur_desc, "L");
    strcat(cur_desc, METHOD_CLASS);
    strcat(cur_desc, ";");
    strcat(cur_desc, tmpdesc);
    strcat(cur_desc, ")");
    strcat(cur_desc, field_descriptor[temp->vartype][0]);

    /* set global variables */
    num_locals = cur_local = num_locals_in_descriptor(cur_desc);

    count = methcall_arglist_emit(temp);

    fprintf(curfp,")\n throws java.lang.reflect.InvocationTargetException,\n");
    fprintf(curfp,"          java.lang.IllegalAccessException\n{\n"); 

    fprintf(curfp,"Object [] _funcargs = new Object [%d];\n", count);
    fprintf(curfp,"%s _retval;\n", returnstring[temp->vartype]);

    /* create a new object array and store it in the first local var */
    pushIntConst(count);
    c = cp_find_or_insert(cur_const_table, CONSTANT_Class, "java/lang/Object");
    bytecode1(jvm_anewarray, c->index);
    obj_array_varnum = getNextLocal(Object);
    gen_store_op(obj_array_varnum, Object);

    methcall_obj_array_emit(temp, obj_array_varnum);

    fprintf(curfp,
      "_retval = ( (%s) _funcptr.invoke(null,_funcargs)).%sValue();\n",
      java_wrapper[temp->vartype], returnstring[temp->vartype]);

    /* load _funcptr, which should always be local var 0 */
    gen_load_op(0, Object);
    bytecode0(jvm_aconst_null);
    gen_load_op(obj_array_varnum, Object);

    c = newMethodref(cur_const_table, METHOD_CLASS, "invoke",
          INVOKE_DESC);
    bytecode1(jvm_invokevirtual, c->index);

    c = cp_find_or_insert(cur_const_table,CONSTANT_Class,
          numeric_wrapper[temp->vartype]);
    bytecode1(jvm_checkcast, c->index);

    c = newMethodref(cur_const_table,numeric_wrapper[temp->vartype], 
                     numericValue_method[temp->vartype],
                     numericValue_descriptor[temp->vartype]);

    bytecode1(jvm_invokevirtual, c->index);

    bytecode0(return_opcodes[temp->vartype]);

    fprintf(curfp,"return _retval;\n");
    fprintf(curfp,"}\n"); 

    endNewMethod(cur_class_file, inv_method, cur_name, cur_desc,
         num_locals, exc_list );
  }

  dl_delete_list(exc_list);
}

/*****************************************************************************
 *                                                                           *
 * methcall_arglist_emit                                                     *
 *                                                                           *
 * This function generates the list of arguments to the method adapter.      *
 * the return value is an integer representing the number of arguments.      *
 *                                                                           *
 *****************************************************************************/

int
methcall_arglist_emit(AST *temp)
{
  enum returntype rtype;
  HASHNODE *ht;
  int count = 0, dim = 0;
  AST *arg;

  for(arg = temp->astnode.ident.arraylist; arg != NULL; arg = arg->nextstmt) {
    fprintf(curfp,",");

    dim = arg->astnode.ident.dim;

    if(omitWrappers) {
      if( arg->nodetype == Identifier ) {
        ht = type_lookup(cur_type_table,arg->astnode.ident.name);

        if(ht) {
          rtype = ht->variable->vartype;
          dim = ht->variable->astnode.ident.dim;
        }
        else
          rtype = arg->vartype;
      }
      else if( arg->nodetype == Constant )
        rtype = get_type(arg->astnode.constant.number);
      else
        rtype = arg->vartype;

      if(dim >0)
        fprintf(curfp," %s [] _arg%d ", returnstring[rtype], count);
      else
        fprintf(curfp," %s _arg%d ", returnstring[rtype], count);
    }
    else
    {
      if( arg->nodetype == Identifier ) {
        ht = type_lookup(cur_type_table,arg->astnode.ident.name);

        if(ht) {
          rtype = ht->variable->vartype;
          dim = ht->variable->astnode.ident.dim;
        }
        else
          rtype = arg->vartype;
      }
      else if( arg->nodetype == Constant )
        rtype = get_type(arg->astnode.constant.number);
      else
        rtype = arg->vartype;

      if(dim >0)
        fprintf(curfp," %s [] _arg%d ", wrapper_returns[rtype], count);
      else
        fprintf(curfp," %s _arg%d ", wrapper_returns[rtype], count);
    }

    if(dim > 0) {
      fprintf(curfp,", int _arg%d_offset ", count);
      /* normally, we'd increment count by two, but i'm hacking this
       * a bit so that the lapack tester works correctly.
       */
      /* count += 2;  */
    }

    count++;
  }

  return count;
}

/*****************************************************************************
 *                                                                           *
 * methcall_obj_array_emit                                                   *
 *                                                                           *
 * This function generates the initialization of the object array which we   *
 * must pass to the reflective invoke call.                                  *
 *                                                                           *
 *****************************************************************************/

void
methcall_obj_array_emit(AST *temp, int lv)
{
  enum returntype rtype;
  HASHNODE *ht;
  int ai = 0, vi = 1, dim = 0;
  AST *arg;

  for(arg=temp->astnode.ident.arraylist;arg!=NULL;arg=arg->nextstmt,ai++,vi++)
  {
    dim = arg->astnode.ident.dim;

    if(omitWrappers) {
      if( arg->nodetype == Identifier ) {
        ht = type_lookup(cur_type_table,arg->astnode.ident.name);

        if(ht) {
          rtype = ht->variable->vartype;
          dim = ht->variable->astnode.ident.dim;
        }
        else
          rtype = arg->vartype;
      }
      else if( arg->nodetype == Constant )
        rtype = get_type(arg->astnode.constant.number);
      else
        rtype = arg->vartype;


      fprintf(curfp," _funcargs[%d] = new %s(", ai,java_wrapper[rtype]);

      if(dim > 0) {
        fprintf(curfp,"_arg%d[_arg%d_offset]);\n", ai, ai);

        arg_array_assign_emit(lv, ai, vi, rtype);
        vi++;
      }
      else {
        fprintf(curfp,"_arg%d);\n", ai);
        arg_assignment_emit(lv, ai, vi, TRUE, rtype);
      }
    }
    else
    {
      if(dim > 0) {
        fprintf(curfp," _funcargs[%d] = _arg%d[_arg%d_offset];\n",ai,ai,ai);
        arg_array_assign_emit(lv, ai, vi, rtype);
        vi++;
      }
      else {
        fprintf(curfp," _funcargs[%d] = _arg%d;\n",ai,ai);
        arg_assignment_emit(lv, ai, vi, FALSE, rtype);
      }
    }

    if((rtype == Double) && (dim == 0))
      vi++;
  }
}

/*****************************************************************************
 *                                                                           *
 * arg_array_assign_emit                                                     *
 *                                                                           *
 * this function emits the bytecode for an assignment of an argument to the  *
 * object array (e.g. _funcargs[%d] = _arg%d[_arg%d_offset]).                *
 *                                                                           *
 *****************************************************************************/

void
arg_array_assign_emit(int array_vnum, int array_idx, int arg_vnum,
  enum returntype argtype)
{
  CPNODE *c;

  gen_load_op(array_vnum, Object);
  pushIntConst(array_idx);

  c = cp_find_or_insert(cur_const_table,CONSTANT_Class, 
          numeric_wrapper[argtype]);

  bytecode1(jvm_new,c->index);
  bytecode0(jvm_dup);
  gen_load_op(arg_vnum, Object);
  gen_load_op(arg_vnum + 1, Integer);
  bytecode0(array_load_opcodes[argtype]);

  c = newMethodref(cur_const_table, numeric_wrapper[argtype],
           "<init>", wrapper_descriptor[argtype]);
  bytecode1(jvm_invokespecial, c->index);

  bytecode0(array_store_opcodes[Object]);
}

/*****************************************************************************
 *                                                                           *
 * arg_assignment_emit                                                       *
 *                                                                           *
 * this function emits the bytecode for an assignment of an argument to the  *
 * object array (e.g. _funcargs[%d] = _arg%d).                               *
 *                                                                           *
 *****************************************************************************/

void
arg_assignment_emit(int array_vnum, int array_idx, int arg_vnum, BOOLEAN wrap,
  enum returntype argtype)
{
  CPNODE *c;

  gen_load_op(array_vnum, Object);
  pushIntConst(array_idx);

  if(wrap) {
    c = cp_find_or_insert(cur_const_table,CONSTANT_Class, 
            numeric_wrapper[argtype]);

    bytecode1(jvm_new,c->index);
    bytecode0(jvm_dup);
    gen_load_op(arg_vnum, argtype);

    c = newMethodref(cur_const_table, numeric_wrapper[argtype],
             "<init>", wrapper_descriptor[argtype]);

    bytecode1(jvm_invokespecial, c->index);
  }
  else
    gen_load_op(arg_vnum, argtype);

  bytecode0(jvm_aastore);
}

/*****************************************************************************
 *                                                                           *
 * inc_stack                                                                 *
 *                                                                           *
 * Increment the stacksize by the specified amount.  If this is the highest  *
 * stack value encountered, set max_stack to the current stacksize.          *
 *                                                                           *
 *****************************************************************************/

void
inc_stack(int inc)
{
  stacksize += inc;
  
  if(stacksize > cur_code->attr.Code->max_stack)
    cur_code->attr.Code->max_stack = stacksize;
}

/*****************************************************************************
 *                                                                           *
 * dec_stack                                                                 *
 *                                                                           *
 * Decrement the stacksize by the specified amount.                          *
 *                                                                           *
 *****************************************************************************/

void
dec_stack(int dec) {
  stacksize -= dec;

  if(stacksize < 0)
    fprintf(stderr,"WARNING: negative stacksize! (%s)\n", cur_filename);
}

/*****************************************************************************
 *                                                                           *
 * newCodeAttribute                                                          *
 *                                                                           *
 * creates a new attribute_info structure and initializes the Code_attribute *
 * section with some initial values.                                         *
 *                                                                           *
 *****************************************************************************/

struct attribute_info *
newCodeAttribute()
{
  struct attribute_info * tmp;
  CPNODE *c;

  tmp = (struct attribute_info *)f2jalloc(sizeof(struct attribute_info));

  c = cp_find_or_insert(cur_const_table, CONSTANT_Utf8, "Code");
  tmp->attribute_name_index = c->index;
  tmp->attribute_length = 0;
  tmp->attr.Code = (struct Code_attribute *)f2jalloc(sizeof(struct Code_attribute));
  tmp->attr.Code->max_stack = 0;
  tmp->attr.Code->max_locals = 0;
  tmp->attr.Code->code_length = 0;
  tmp->attr.Code->code = make_dl();
  tmp->attr.Code->exception_table_length = 0;
  tmp->attr.Code->exception_table = NULL;
  tmp->attr.Code->attributes_count = 0;
  tmp->attr.Code->attributes = NULL;

  return tmp;
}

/*****************************************************************************
 *                                                                           *
 * newExceptionsAttribute                                                    *
 *                                                                           *
 * creates a new attribute_info structure and initializes the                *
 * Exception_attribute section with some initial values.                     *
 *                                                                           *
 *****************************************************************************/

struct attribute_info *
newExceptionsAttribute(Dlist exc)
{
  struct attribute_info * tmp;
  CPNODE *c;
  Dlist dtmp, new;
  int cnt=0;
  char *entry;
  int *copy;

  tmp = (struct attribute_info *)f2jalloc(sizeof(struct attribute_info));

  c = cp_find_or_insert(cur_const_table, CONSTANT_Utf8, "Exceptions");
  tmp->attribute_name_index = c->index;

  tmp->attr.Exceptions = (struct Exceptions_attribute *)
                 f2jalloc(sizeof(struct Exceptions_attribute));

  new = make_dl();

  dl_traverse(dtmp, exc) {
    entry = (char *) dtmp->val;

    cnt++;
    c = cp_find_or_insert(cur_const_table, CONSTANT_Class, entry);

    copy = (int *)f2jalloc(sizeof(int));
    *copy = c->index;

    dl_insert_b(new, copy);
  }

  tmp->attribute_length = 2 + (cnt * 2);

  tmp->attr.Exceptions->number_of_exceptions = cnt;
  tmp->attr.Exceptions->exception_index_table = new;

  return tmp;
}

/*****************************************************************************
 *                                                                           *
 * newClassFile                                                              *
 *                                                                           *
 * Creates a new class file structure.                                       *
 *                                                                           *
 *****************************************************************************/

struct ClassFile *
newClassFile(char *name, char *srcFile)
{
  struct attribute_info * attr_temp;
  struct method_info * meth_tmp;
  struct cp_info *newnode;
  struct ClassFile * tmp;
  char * fullclassname;
  CPNODE *c;

  tmp = (struct ClassFile *)f2jalloc(sizeof(struct ClassFile));
 
  tmp->magic = JVM_MAGIC;
  tmp->minor_version = JVM_MINOR_VER;
  tmp->major_version = JVM_MAJOR_VER;

  /* we'll fill out the constant pool and fields later. */
  tmp->constant_pool_count = 0;
  tmp->constant_pool = NULL;
  tmp->fields_count = 0;
  tmp->fields = make_dl();

  tmp->access_flags = ACC_PUBLIC | ACC_FINAL | ACC_SUPER;

  /* first create an entry for 'this'.  the class file variable this_class
   * points to a CONSTANT_Class_info entry in the constant pool, which in
   * turn points to a CONSTANT_Utf8_info entry representing the name of
   * this class.  so, first we create the Utf8 entry, then the Class entry.
   */

  fullclassname = get_full_classname(name);
printf("##creating new entry, this -> %s\n",fullclassname);

  newnode = (struct cp_info *)f2jalloc(sizeof(struct cp_info));
  newnode->tag = CONSTANT_Utf8;
  newnode->cpnode.Utf8.length = strlen(fullclassname);
  newnode->cpnode.Utf8.bytes = (u1 *)f2jalloc(newnode->cpnode.Utf8.length);
  strncpy((char *)newnode->cpnode.Utf8.bytes, fullclassname, 
    newnode->cpnode.Utf8.length);

  c = cp_insert(cur_const_table,newnode,1);

  newnode = (struct cp_info *)f2jalloc(sizeof(struct cp_info));
  newnode->tag = CONSTANT_Class;
  newnode->cpnode.Class.name_index = c->index;

  c = cp_insert(cur_const_table,newnode,1);
  tmp->this_class = c->index;

  c = cp_find_or_insert(cur_const_table, CONSTANT_Class, "java/lang/Object");
  tmp->super_class = c->index;

  /* f2java generated code does not implement any interfaces */
  tmp->interfaces_count = 0;
  tmp->interfaces = NULL;

  /* the only attributes allowed for a class file are SourceFile and
   * Deprecated.  we don't want to generate deprecated classes, so
   * the only one we're interested in setting here is the SourceFile.
   * SourceFile refers to the name of the original source code file,
   * which in this case should be some Fortran code.
   */
  tmp->attributes_count = 1;
  tmp->attributes = make_dl();
 
  attr_temp = (struct attribute_info *)f2jalloc(sizeof(struct attribute_info));

  c = cp_find_or_insert(cur_const_table,CONSTANT_Utf8, "SourceFile");
  attr_temp->attribute_name_index = c->index;
  attr_temp->attribute_length = 2;  /* SourceFile attr length always 2 */
  attr_temp->attr.SourceFile = 
    (struct SourceFile_attribute *) f2jalloc(sizeof(struct SourceFile_attribute));
  c = cp_find_or_insert(cur_const_table,CONSTANT_Utf8, srcFile);
  attr_temp->attr.SourceFile->sourcefile_index = c->index;

  dl_insert_b(tmp->attributes,attr_temp);

  /* every f2java generated class will have a default constructor "<init>"
   * which simply calls the constructor for java.lang.Object.  here we create
   * a method entry for the default constructor.
   */

  tmp->methods_count = 0;
  tmp->methods = make_dl();

  meth_tmp = beginNewMethod(ACC_PUBLIC);

  c = newMethodref(cur_const_table,"java/lang/Object", "<init>", "()V");

  bytecode0(jvm_aload_0);
  bytecode1(jvm_invokespecial, c->index);
  bytecode0(jvm_return);
  
  endNewMethod(tmp, meth_tmp, "<init>", "()V", 1, NULL);

  f2jfree(fullclassname, strlen(fullclassname)+1);

  return tmp;
}

/*****************************************************************************
 *                                                                           *
 * char_substitution                                                         *
 *                                                                           *
 * this function substitutes every occurrence of 'from_char' with 'to_char'  *
 * typically this is used to convert package names:                          *
 *                                                                           *
 *   e.g.     "java.lang.whatever" -> "java/lang/whatever"                   *
 *                                                                           *
 *****************************************************************************/

char *
char_substitution(char *str, int from_char, int to_char)
{
  char *newstr = strdup(str);
  char *idx;

  while( (idx = strchr(newstr, from_char)) != NULL )
    *idx = to_char;                                                                 

  return newstr;
}

/*****************************************************************************
 *                                                                           *
 * beginNewMethod                                                            *
 *                                                                           *
 * Creates a new method structure with the given access flags.               *
 *                                                                           *
 *****************************************************************************/

struct method_info *
beginNewMethod(unsigned int flags)
{
  struct method_info *tmp;
  u2 acc;

  acc = (u2) flags;

  if((int)acc != flags)
    fprintf(stderr,"Warning: possible truncation in beginNewMethod.\n");

  tmp = (struct method_info *)f2jalloc(sizeof(struct method_info));
  tmp->access_flags = acc;

printf("access flags = %d\n", flags);

  tmp->attributes = make_dl();
  tmp->attributes_count = 1;

  cur_code = newCodeAttribute();
  if(exc_table != NULL)  {
    Dlist tmp;

    dl_traverse(tmp, exc_table)
      f2jfree(tmp->val, sizeof(ExceptionTableEntry));

    dl_delete_list(exc_table);
  }

  exc_table = make_dl();

  stacksize = pc = num_handlers = 0;

  return tmp;
}

/*****************************************************************************
 *                                                                           *
 * endNewMethod                                                              *
 *                                                                           *
 * Finishes initialization of the new method structure.                      *
 *                                                                           *
 *****************************************************************************/

void
endNewMethod(struct ClassFile *cclass, struct method_info * meth, char * name, char * desc,
  unsigned int mloc, Dlist exceptions)
{
  ExceptionTableEntry *et_entry;
  CPNODE *c;
  Dlist tmp;
  int idx;
  u2 maxloc;

  if(gendebug)
    printf("endNewMethod(): %s - %s\n", name, desc);

  if(exceptions != NULL) {
    dl_insert_b(meth->attributes, newExceptionsAttribute(exceptions));

    meth->attributes_count += 1;
  }

  maxloc = (u2)mloc;

  if((int)maxloc != mloc)
    fprintf(stderr,"Warning: possible truncation in endNewMethod.\n");

  fprintf(indexfp,"%s:%s:%s\n",cur_filename, name, desc);

  /* at the end of the method, the stacksize should always be zero.
   * if not, we're gonna have verification problems at the very least.
   * at this point, there's not much we can do about it, but issue a
   * warning.
   */
  if(stacksize != 0)
    fprintf(stderr,"WARNING: ending method with stacksize = %d\n",stacksize);

  /* here we insert the name and descriptor of this method.  the reason that
   * we didn't insert this into the constant pool in the beginNewMethod()
   * function is for the <clinit> case.  we dont always need a <clinit> 
   * method because we might not have any array, object, or other static
   * initialization to do.  in that case, there's no need to put these
   * entries into the constant pool.
   */
  c = cp_find_or_insert(cur_const_table,CONSTANT_Utf8, name);

  meth->name_index = c->index;

  c = cp_find_or_insert(cur_const_table,CONSTANT_Utf8, desc);

  meth->descriptor_index = c->index;

  traverse_code(cur_code->attr.Code->code);

#ifdef VCG_CONTROL_FLOW
  cfg_emit(cur_code->attr.Code->code, name);
#endif

  cur_code->attr.Code->exception_table_length = num_handlers;

  if(num_handlers > 0) {
    cur_code->attr.Code->exception_table = 
      (struct ExceptionTable *) f2jalloc(sizeof(struct ExceptionTable) * num_handlers);

    printf("Code set exception_table_length = %d\n",num_handlers);
    idx = 0;
    dl_traverse(tmp, exc_table) {
      et_entry = (ExceptionTableEntry *) tmp->val;

      cur_code->attr.Code->exception_table[idx].start_pc = et_entry->from->pc;
      cur_code->attr.Code->exception_table[idx].end_pc = et_entry->to->pc;
      cur_code->attr.Code->exception_table[idx].handler_pc = et_entry->target->pc;
      cur_code->attr.Code->exception_table[idx].catch_type = et_entry->catch_type;
      idx++;

      f2jfree(et_entry, sizeof(ExceptionTableEntry));
    }
  }

  dl_delete_list(exc_table);
  exc_table = NULL;

  /* attribute_length is calculated as follows:
   *   max_stack               =  2 bytes
   *   max_locals              =  2 bytes
   *   code_length             =  4 bytes
   *   code                    = pc bytes
   *   exception_table_length  =  2 bytes
   *   exception_table         =  exception_table_length * sizeof(exception table) bytes
   *   attributes_count        =  2 bytes
   *   attributes              =  0 bytes  (no attributes generated)
   *  ---------------------------------
   *   total                   =  12 + exception_table_length * sizeof(exception table) bytes
   */
  cur_code->attribute_length = pc + 12 + num_handlers * sizeof(struct ExceptionTable);
  cur_code->attr.Code->max_locals = maxloc;
  cur_code->attr.Code->code_length = pc;
  printf("Code: set code_length = %d\n",pc);

  dl_insert_b(meth->attributes, cur_code);

  cclass->methods_count++;
  dl_insert_b(cclass->methods, meth);
}

/*****************************************************************************
 *                                                                           *
 * getNextLocal                                                              *
 *                                                                           *
 * this function returns the next available local variable number and        *
 * updates the max if necessary.                                             *
 *                                                                           *
 *****************************************************************************/

int
getNextLocal(enum returntype vtype)
{
  if(vtype == Double)
    cur_local+=2;
  else
    cur_local++;

  if(cur_local > num_locals)
    num_locals = cur_local;

  return cur_local - ((vtype == Double) ? 2 : 1);
}

/*****************************************************************************
 *                                                                           *
 * releaseLocal                                                              *
 *                                                                           *
 * this function 'releases' a local variable.  that is, calling this         *
 * function signifies that we no longer need this local variable.            *
 *                                                                           *
 *****************************************************************************/

void
releaseLocal(enum returntype vtype)
{
  if(vtype == Double)
    cur_local-=2;
  else
    cur_local--;
}

/*****************************************************************************
 *                                                                           *
 * nodeAtPC                                                                  *
 *                                                                           *
 * this function searches the list of nodes for the given PC.  returns the   *
 * node if found, otherwise NULL.
 *                                                                           *
 *****************************************************************************/

CodeGraphNode *
nodeAtPC(int num)
{
  CodeGraphNode *nodeptr;
  Dlist tmp;

  dl_traverse(tmp, cur_code->attr.Code->code) {
    nodeptr = (CodeGraphNode *)tmp->val;
    if(nodeptr->pc == num)
      return nodeptr;
    if(nodeptr->pc > num)
      return NULL;
  }

  return NULL;
}

/*****************************************************************************
 *                                                                           *
 * traverse_code                                                             *
 *                                                                           *
 * this function traverses the code graph and determines the max stack and   *
 * assigns branch target offsets.                                            *
 *                                                                           *
 *****************************************************************************/

void
traverse_code(Dlist cgraph) 
{
  ExceptionTableEntry *et_entry;
  CodeGraphNode *val;
  char *warn;
  Dlist tmp;

  if(dl_empty(cgraph))
    return;

  /* set initial stack depth to zero */
  val = (CodeGraphNode *) dl_val(dl_first(cgraph));
  val->stack_depth = 0;

  /* traverse the whole graph calculating branch target offsets. */
  calcOffsets(val);

  /* now traverse paths originating from exception handlers */
  num_handlers = 0;
  dl_traverse(tmp,exc_table) {
    /* count number of handlers.. we'll use this info later */
    num_handlers++;
    et_entry = (ExceptionTableEntry *) tmp->val;
    calcOffsets(et_entry->target);
  }

  if(pc > MAX_CODE_LEN)
    fprintf(stderr,"WARNING: code length (%d) exceeds max of %d\n",
       pc, MAX_CODE_LEN);

  /* now print the instructions */
  dl_traverse(tmp,cgraph) {
    val = (CodeGraphNode *) tmp->val;
  
    if(!val->visited)
      warn = "(UNVISITED!!)";
    else
      warn = "";

    if(opWidth(val->op) > 1)
      printf("%d: %s %d %s\n", val->pc, jvm_opcode[val->op].op, 
         val->operand, warn);
    else
      printf("%d: %s %s\n", val->pc, jvm_opcode[val->op].op, warn);
  }

}

#ifdef VCG_CONTROL_FLOW

/*****************************************************************************
 *                                                                           *
 * cfg_emit                                                                  *
 *                                                                           *
 * this function generates a VCG (visualization of compiler graphs) file     *
 * containing a representation of the control flow graph.                    *
 *                                                                           *
 *****************************************************************************/

void
cfg_emit(Dlist cgraph, char *mname)
{
  CodeGraphNode *val;
  char *filename, *warn;
  char node_label[200];
  FILE *v;
  Dlist tmp;
  
  filename = (char *)f2jalloc(strlen(cur_filename) + strlen(mname) + 10);
  sprintf(filename, "%s_%s.cfg", cur_filename, mname);

  v = fopen(filename,"w");

  if(v) {
  
    print_vcg_header(v, "Control Flow Graph");

    dl_traverse(tmp,cgraph) {
      val = (CodeGraphNode *) tmp->val;
  
      if(!val->visited)
        warn = "(UNVISITED!!)";
      else
        warn = "";

      sprintf(node_label,"%d: %s %s\nstack_pre: %d", val->pc, 
         jvm_opcode[val->op].op, warn, val->stack_depth);

      print_vcg_node(v, val->pc, node_label);
      if((val->next != NULL) && (val->op != jvm_goto))
        print_vcg_nearedge(v, val->pc, val->next->pc);

      if(val->branch_target != NULL)
        print_vcg_edge(v, val->pc, val->branch_target->pc);
    }

    print_vcg_trailer(v);
    fclose(v);
  }
  else
    fprintf(stderr, "couldn't open vcg file: '%s'\n",filename);
}
#endif

/*****************************************************************************
 * checkDistance                                                             *
 *                                                                           *
 * checks whether a branch is too far.  currently the branch target offset   *
 * is a signed 16-bit integer, so the maximum branch is -2^15..2^15-1.       *
 *                                                                           *
 *****************************************************************************/

BOOLEAN
checkDistance(int dest, int src)
{
  int distance;

  distance = dest - src;
  if((distance > ((int)mypow( 2.0, 15.0 ) - 1)) ||
     (distance < ((int)-mypow( 2.0, 15.0 )))) 
  {
    fprintf(stderr,"Warning: branch target too far away.\n");
    return FALSE;
  }
  else
    return TRUE;
}

/*****************************************************************************
 *                                                                           *
 * calcOffsets                                                               *
 *                                                                           *
 * This function calculates the branch target offsets for instructions that  *
 * branch (gotos, compares, etc).  also set the stack depth for the          *
 * instruction(s) following this one.  also perform sanity checks on the     *
 * stack values to make sure that we aren't hitting some instruction from    * 
 * different places with different stack depths.                             *
 *                                                                           *
 *****************************************************************************/

void
calcOffsets(CodeGraphNode *val)
{
  /* if we already visited this node, then do not visit again. */
  printf("in calcoffsets, before op = %s, stack_Depth = %d\n",
         jvm_opcode[val->op].op,val->stack_depth);

  if(val->visited)
    return;

  val->visited = TRUE;

  stacksize = val->stack_depth;

  dec_stack(getStackDecrement(val->op, val->operand));
  inc_stack(getStackIncrement(val->op, val->operand));

  /* special handling for return instructions? */

  if((val->op == jvm_goto) || (val->op == jvm_goto_w)) {
    /* there's a lot of stuff to do/check for goto statements. 
     *  - calculate the branch target offset (remember that the operand to
     *      a goto statement is a signed offset, not an absolute address).
     *  - if this is a goto and the branch_target is NULL, then that means
     *      this is a fortran goto (as opposed to a goto generated as part
     *      of some valid java construct).  we need to find the instruction
     *      corresponding to the branch label and set offset based on that.
     *  - set/check the stack depth for the target of this goto
     */
    if(val->branch_target == NULL) {
      CodeGraphNode *label_node;
      AST *label_ast;

      printf("looking at GOTO %d\n", val->branch_label);
      
      if( (label_ast = find_label(label_list, val->branch_label)) != NULL)
      {
        label_node = nodeAtPC(label_ast->astnode.label.pc);

        if(label_node != NULL) {
          printf(" **found** target pc is %d\n", label_node->pc); 
          if(label_node->stack_depth == -1)
            label_node->stack_depth = stacksize;
          else if(label_node->stack_depth != stacksize) {
            fprintf(stderr,"WARNING: hit pc %d with diff stack sizes (%s)\n",
                    label_node->pc, cur_filename);
            printf("WARNING: hit pc %d with diff stack sizes (%s)\n",
                    label_node->pc, cur_filename);
          }

          checkDistance(label_node->pc, val->pc);

          val->operand = label_node->pc - val->pc;
          val->branch_target = label_node;
          calcOffsets(label_node);
        }
        else 
          fprintf(stderr,"WARNING: cannot find node for pc %d\n", 
                  label_ast->astnode.label.pc);
      }
      else
        fprintf(stderr,"WARNING: cannot find label %d\n", val->branch_label);
    }
    else {
      printf("goto branching to pc %d\n", val->branch_target->pc);

      if(val->branch_target->stack_depth == -1)
        val->branch_target->stack_depth = stacksize;
      else if (val->branch_target->stack_depth != stacksize) {
        fprintf(stderr,"WARNING: hit pc %d with diff stack sizes (%s).\n",
                val->branch_target->pc, cur_filename);
        printf("WARNING: hit pc %d with diff stack sizes (%s).\n",
                val->branch_target->pc, cur_filename);
      }

      checkDistance(val->branch_target->pc, val->pc);

      val->operand = val->branch_target->pc - val->pc;
      calcOffsets(val->branch_target);
    }
  }
  else if ( val->branch_target != NULL) {
    /* if this is not a goto, but the branch target is non-null, then it
     * must be a comparison instruction.  in this case we can either
     * branch to the next instruction or to the branch target, so we will
     * set the stack depth for both instructions.
     */
    if(val->next != NULL)
      val->next->stack_depth = stacksize;

    checkDistance(val->branch_target->pc, val->pc);

    val->branch_target->stack_depth = stacksize;
    val->operand = val->branch_target->pc - val->pc;

    if(val->next != NULL)
      calcOffsets(val->next);
    calcOffsets(val->branch_target);
  }
  else {
    /* null branch target, set stack depth for following instruction only. */
    if(val->next != NULL) {
      val->next->stack_depth = stacksize;
      calcOffsets(val->next);
    }
  }
}

/*****************************************************************************
 *                                                                           *
 * getStackIncrement                                                         *
 *                                                                           *
 * determines the number of bytes that this instruction leaves on the stack  *
 * after execution.  this depends on the instruction and on the data types.  *
 * e.g. for a method invoke instruction, the number of bytes depends on the  *
 * return type of the method (double/long = 2 stack entries).                *
 *                                                                           *
 *****************************************************************************/

int
getStackIncrement(enum _opcode op, u4 index)
{
  char *this_desc;
  CPNODE *c;
  int stack_increment;

  if((op == jvm_invokespecial) || (op == jvm_invokevirtual)
   || (op == jvm_invokestatic))
  {
    struct stack_info *stackinf;

    /* now we need to determine how many parameters are sitting on the stack */
    c = cp_entry_by_index(cur_const_table, index);
    c = cp_entry_by_index(cur_const_table,
                          c->val->cpnode.Methodref.name_and_type_index);
    c = cp_entry_by_index(cur_const_table,
                          c->val->cpnode.NameAndType.descriptor_index);
    this_desc = null_term(c->val->cpnode.Utf8.bytes, c->val->cpnode.Utf8.length);
    stackinf = calcStack(this_desc);
    /* if the opcode is invokespecial or invokevirtual, then there is one
     * object reference + parameters on the stack.  if this is an invokestatic
     * instruction, then there's just parameters. 
     */

    stack_increment = stackinf->ret_len;

    f2jfree(stackinf, sizeof(struct stack_info));
    f2jfree(this_desc, strlen(this_desc)+1);
  }
  else if((op == jvm_putstatic) || (op == jvm_getstatic) || 
          (op == jvm_putfield)  || (op == jvm_getfield))
  {
    int tmpsize;

    c = cp_entry_by_index(cur_const_table, index);
    c = cp_entry_by_index(cur_const_table,
                          c->val->cpnode.Methodref.name_and_type_index);
    c = cp_entry_by_index(cur_const_table,
                          c->val->cpnode.NameAndType.descriptor_index);
    this_desc = null_term(c->val->cpnode.Utf8.bytes, c->val->cpnode.Utf8.length);

    if((this_desc[0] == 'D') || (this_desc[0] == 'J'))
      tmpsize = 2;
    else 
      tmpsize = 1;

    switch(op) {
      case jvm_getstatic:
        stack_increment = tmpsize;
        break;
      case jvm_putstatic:
        stack_increment = 0;
        break;
      case jvm_getfield:
        stack_increment = tmpsize;
        break;
      case jvm_putfield:
        stack_increment = 0;
        break;
      default:
        fprintf(stderr,"getSTackIncrement(): unexpected op type\n");
        break; /* ansi compliance */
    }
    f2jfree(this_desc, strlen(this_desc)+1);
  }
  else {
    /* else we can determine the stack increment from a table.  */
    stack_increment = jvm_opcode[op].stack_post;
  }

  return stack_increment;
}

/*****************************************************************************
 *                                                                           *
 * getStackDecrement                                                         *
 *                                                                           *
 * determines the number of bytes that this instruction removes from the     *
 * stack prior to execution.  this depends on the instruction and on the     *
 * data types involved.  e.g. a method invoke instruction will remove one or *
 * two entries per argument, depending on the data type.                     *
 *                                                                           *
 *                                                                           *
 *****************************************************************************/

int
getStackDecrement(enum _opcode op, u4 index)
{
  char *this_desc;
  CPNODE *c;
  int stack_decrement;

  if((op == jvm_invokespecial) || (op == jvm_invokevirtual)
   || (op == jvm_invokestatic))
  {
    struct stack_info *stackinf;

    /* now we need to determine how many parameters are sitting on the stack */
    c = cp_entry_by_index(cur_const_table, index);
    c = cp_entry_by_index(cur_const_table,
                          c->val->cpnode.Methodref.name_and_type_index);
    c = cp_entry_by_index(cur_const_table,
                          c->val->cpnode.NameAndType.descriptor_index);
    this_desc = null_term(c->val->cpnode.Utf8.bytes, c->val->cpnode.Utf8.length);
    stackinf = calcStack(this_desc);
    /* if the opcode is invokespecial or invokevirtual, then there is one
     * object reference + parameters on the stack.  if this is an invokestatic
     * instruction, then there's just parameters. 
     */
    if(op == jvm_invokestatic)
      stack_decrement = stackinf->arg_len;
    else
      stack_decrement = stackinf->arg_len + 1;

    f2jfree(stackinf, sizeof(struct stack_info));
    f2jfree(this_desc, strlen(this_desc)+1);
  }
  else if((op == jvm_putstatic) || (op == jvm_getstatic) || 
          (op == jvm_putfield)  || (op == jvm_getfield))
  {
    int tmpsize;

    c = cp_entry_by_index(cur_const_table, index);
    c = cp_entry_by_index(cur_const_table,
                          c->val->cpnode.Methodref.name_and_type_index);
    c = cp_entry_by_index(cur_const_table,
                          c->val->cpnode.NameAndType.descriptor_index);
    this_desc = null_term(c->val->cpnode.Utf8.bytes, c->val->cpnode.Utf8.length);

    if((this_desc[0] == 'D') || (this_desc[0] == 'J'))
      tmpsize = 2;
    else 
      tmpsize = 1;

    switch(op) {
      case jvm_getstatic:
        stack_decrement = 0;
        break;
      case jvm_putstatic:
        stack_decrement = tmpsize;
        break;
      case jvm_getfield:
        stack_decrement = 1;
        break;
      case jvm_putfield:
        stack_decrement = tmpsize + 1;
        break;
      default:
        fprintf(stderr,"getSTackDecrement(): unexpected op type\n");
        break; /* ansi compliance */
    }
    f2jfree(this_desc, strlen(this_desc)+1);
  }
  else {
    /* else we can determine the stack decrement from a table.  */
    stack_decrement = jvm_opcode[op].stack_pre;
  }

  return stack_decrement;
}

/*****************************************************************************
 *                                                                           *
 * calcStack                                                                 *
 *                                                                           *
 * given a method descriptor, this function returns the number of arguments  *
 * it takes.  we use this value to determine how much to decrement the stack *
 * after a method invocation.                                                *
 *                                                                           *
 *****************************************************************************/

struct stack_info *
calcStack(char *d)
{
  struct stack_info *tmp;
  int len = strlen(d);
  char *ptr, *tstr;

  tmp = (struct stack_info *)f2jalloc(sizeof(struct stack_info));
  tmp->arg_len = 1;
  tmp->ret_len = 1;

  /* the shortest possible method descriptor should be 3 characters: ()V
   * thus, if the given string is < 3 characters, it must be in error.
   */

  if(len < 3) {
    fprintf(stderr,"WARNING: invalid descriptor '%s' (len < 3).\n", d);
    return tmp;
  }

  if(d[0] != '(') {
    fprintf(stderr,"WARNING: invalid descriptor '%s' (bad 1st char).\n", d);
    return tmp;
  }

  ptr = d;

  /* start at -1 because the opening paren will contribute 1 to
   * the count.
   */
  tmp->arg_len = -1;

  while((ptr = skipToken(ptr)) != NULL) {
    tmp->arg_len++;

    /* check if this is a double or long type.  if so, increment
     * again because these data types take up two stack entries.
     */
    if( (*ptr ==  'D') || (*ptr == 'J') )
      tmp->arg_len++;
  }

  tstr = strdup(d);
  ptr = strtok(tstr,")");
  ptr = strtok(NULL,")");
  if( (*ptr ==  'D') || (*ptr == 'J') )
    tmp->ret_len = 2;
  else if(*ptr == 'V')
    tmp->ret_len = 0;
  else
    tmp->ret_len = 1;

  f2jfree(tstr, strlen(tstr)+1);
  return tmp;
}

/*****************************************************************************
 *                                                                           *
 * skipToken                                                                 *
 *                                                                           *
 * helper routine for calcStack().  this function returns a pointer to       *
 * the next field type in this descriptor.  if there are no more field types *
 * this function returns NULL.  on error, this function also returns NULL.   *
 *                                                                           *
 *****************************************************************************/

char *
skipToken(char *str)
{
  char *p = str;

  switch(*p) {
    case 'B': case 'C': case 'D': case 'F':
    case 'I': case 'J': case 'S': case 'Z':
      return p+1;

    case 'L':
      while((*p != ';') && (*p != '\0'))
        p++;

      if(*p == '\0') {
        fprintf(stderr,"WARNING: skipToken() incomplete classname in descriptor\n");
        return NULL;
      }

      return p+1;

    case '[':
      return skipToken(p+1);

    case '(':
      /* we should hit this case at the beginning of the descriptor */
      return p+1;

    case ')':
      return NULL;

    default:
      fprintf(stderr,"WARNING: skipToken() unrecognized char in desc:%s\n",
        str);
      return NULL;
  }

  /* should never reach here */
  return NULL;
}

/*****************************************************************************
 *                                                                           *
 * num_locals_in_descriptor                                                  *
 *                                                                           *
 * given a method descriptor, this function returns the number of local      *
 * variables needed to hold the arguments.  doubles and longs use 2 local    *
 * vars, while every other data type only uses 1 local.                      *
 *                                                                           *
 *****************************************************************************/

int
num_locals_in_descriptor(char *d)
{
  int vlen = 0;

  d = skipToken(d);
  while( (d = skipToken(d)) != NULL) {
    if(d[0] == 'D')
      vlen += 2;
    else
      vlen++;
  }

  return vlen;
}


/*****************************************************************************
 *                                                                           *
 * assign_local_vars                                                         *
 *                                                                           *
 * This routine numbers the local variables for generating Jasmin            *
 * assembly code.                                                            *
 *                                                                           *
 * Horribly kludged routines with massive loop of                            *
 * duplicated code.                                                          *
 *                                                                           *
 * ...cleaned this routine up somewhat.  --kgs 5/5/00                        *
 *                                                                           *
 *****************************************************************************/

void
assign_local_vars(AST * root)
{
  AST * locallist;
  HASHNODE * hashtemp, * ht2;
  int localnum = 0;

  /* if root is NULL, this is probably a PROGRAM (no args) */
  if(root == NULL)
    return;

  /* This loop takes care of the stuff coming in from the
   * argument list.  
   */
  for (locallist = root ; locallist; locallist = locallist->nextstmt)
  {
    if(gendebug)
      printf("assign_local_vars(%s): arg list name: %s, local varnum: %d\n",cur_filename, 
         locallist->astnode.ident.name, localnum);

    hashtemp = type_lookup(cur_type_table, locallist->astnode.ident.name);
    if(hashtemp == NULL)
    {
      ht2=type_lookup(cur_args_table, locallist->astnode.ident.name);
      if(ht2) {
        if(gendebug)
          printf("assign_local_vars(%s): %s in args table, setting local varnum: %d\n",cur_filename, locallist->astnode.ident.name, localnum);
        ht2->variable->astnode.ident.localvnum = localnum;
        localnum++;
        continue;
      }
      else {
        fprintf(stderr,"Type table is screwed in assign locals.\n");
        fprintf(stderr,"could not find %s\n", locallist->astnode.ident.name);
        exit(-1);
      }
    }

    hashtemp->variable->astnode.ident.localvnum = localnum;

    /* Check to see if it is a double or if it is an array declaration.
     * Doubles take up two stack entries, so we increment by 2.  Arrays
     * only take up one stack entry, but we add an integer offset 
     * parameter which takes up an additional entry.
     *
     * also check whether this is pass by reference, because objects
     * always occupy 1 stack entry, even if the data type is double.
     */
printf("assign_local_vars(%s): name: %s, pass by ref: %s\n", cur_filename,
 locallist->astnode.ident.name, hashtemp->variable->astnode.ident.passByRef ? "yes" : "no");

    if((hashtemp->type == Double ||
        hashtemp->variable->astnode.ident.arraylist != NULL) &&
       (!hashtemp->variable->astnode.ident.passByRef))
      localnum += 2;
    else
      localnum++;

    if(gendebug)
      printf("ARG %s %d\n", hashtemp->variable->astnode.ident.name, 
        hashtemp->variable->astnode.ident.localvnum); 
  }

  locals = localnum;
} /* Close assign_local_vars().  */

/*****************************************************************************
 *                                                                           *
 * print_nodetype                                                            *
 *                                                                           *
 * This is primarily a debugging tool.  Given a node, it returns a           *
 * string containing the node type.                                          *
 *                                                                           *
 *****************************************************************************/

char * 
print_nodetype (AST *root) 
{
  static char temp[100];

  if(root == NULL) {
    return("print_nodetpe: NULL root");
  }

  switch (root->nodetype)
  {
    case Source:
      return("Source");
    case Progunit:
      return("Progunit");
    case Subroutine:
      return("Subroutine");
    case Function:
      return("Function");
    case Program:
      return("Program");
    case Blockif:
      return("Blockif");
    case Common:
      return("Common");
    case CommonList:
      return("CommonList");
    case DataStmt:
      return("DataStmt");
    case DataList:
      return("DataList");
    case Elseif:
      return("Elseif");
    case Else:
      return("Else");
    case Forloop:
      return("Forloop");
    case Format:
      return("Format");
    case Constant:
      return("Constant");
    case Method:
      return("Method");
    case Identifier:
      return("Identifier");
    case Label:
      return("Label");
    case Logicalif:
      return("Logicalif");
    case Typedec:
      return("Typedec");
    case Assignment:
      return("Assignment");
    case Expression:
      return("Expression");
    case Return:
      return("Return");
    case Goto:
      return("Goto");
    case Call:
      return("Call");
    case Statement:
      return("Statement");
    case Relationalop:
      return("Relationalop");
    case Logicalop:
      return("Logicalop");
    case Binaryop:
      return("Binaryop");
    case Power:
      return("Power");
    case Unaryop:
      return("Unaryop");
    case Save:
      return("Save");
    case Specification:
      return("Specification");
    case Substring:
      return("Substring");
    case End:
      return("End");
    case Write:
      return("Write");
    case Stop:
      return("Stop");
    case ComputedGoto:
      return("ComputedGoto");
    case ArrayAccess:
      return("ArrayAccess");
    case ArrayDec:
      return("ArrayDec");
    case Read:
      return("Read");
    case EmptyArgList:
      return("EmptyArgList");
    case IoExplist:
      return("IoExplist");
    case IoImpliedLoop:
      return("IoImpliedLoop");
    case DataImpliedLoop:
      return("DataImpliedLoop");
    case Unimplemented:
      return("Unimplemented");
    case Equivalence:
      return("Equivalence");
    case Comment:
      return("Comment");
    case MainComment:
      return("MainComment");
    case Dimension:
      return("Dimension");
    default:
      sprintf(temp, "print_nodetype(): Unknown Node: %d", root->nodetype);
      return(temp);
  }
}

/*****************************************************************************
 *                                                                           *
 * newGraphNode                                                              *
 *                                                                           *
 * returns a new code graph node initialized with the given opcode, operand, *
 * and pc.                                                                   *
 *                                                                           *
 *****************************************************************************/

CodeGraphNode *
newGraphNode(enum _opcode op, u4 operand)
{
  CodeGraphNode *tmp = (CodeGraphNode *)f2jalloc(sizeof(CodeGraphNode));
  
  tmp->op = op;
  tmp->operand = operand;
  tmp->width = opWidth(op);

  /* set pc and branch targets later */
  tmp->pc = pc;
  tmp->branch_target = NULL;
  tmp->next = NULL;
  tmp->optional_targets = NULL;
  tmp->branch_label = -1;
  tmp->stack_depth = -1;
  tmp->visited = FALSE;

  return tmp;
}

/*****************************************************************************
 *                                                                           *
 * bytecode1                                                                 *
 *                                                                           *
 * inserts the given instruction into the code graph.                        *
 *                                                                           *
 *****************************************************************************/

CodeGraphNode *
bytecode1(enum _opcode op, u4 operand)
{
  CodeGraphNode *tmp, *prev;

  /* if we should not generate bytecode, then just return a dummy node */
  if(!bytecode_gen) {
    CodeGraphNode *g;

    /* keep track of the dummy node so that we may reclaim the memory later. */
    g = newGraphNode(op, operand);
    dl_insert_b(dummy_nodes, g);
    return g;
  }

  printf("bytecode: %s %d\n", jvm_opcode[op].op, operand);

  lastOp = op;

  if(cur_code->attr.Code->code == NULL)
    fprintf(stderr,"ERROR: null code graph.\n");

  prev = (CodeGraphNode *) dl_val(dl_last(cur_code->attr.Code->code));

  if((prev != NULL) && (prev->op == jvm_impdep1)) {
    prev->op = op;
    prev->operand = operand;
    prev->width = opWidth(op);
    pc += opWidth(op) - opWidth(jvm_impdep1);
    return prev;
  }

  tmp = newGraphNode(op, operand);

  if(prev != NULL)
    prev->next = tmp;

  dl_insert_b(cur_code->attr.Code->code, tmp);

  /* if the previous instruction was 'wide', then we need to
   * increase the width of this instruction. 
   */
  if((prev != NULL) && (prev->op == jvm_wide)) {
    if( (op == jvm_iload) || (op == jvm_fload) || (op == jvm_aload) || 
        (op == jvm_lload) || (op == jvm_dload) || (op == jvm_istore) || 
        (op == jvm_fstore) || (op == jvm_astore) || (op == jvm_lstore) || 
        (op == jvm_dstore) || (op == jvm_ret))
      tmp->width = opWidth(op) + 1;
    else if(op == jvm_iinc)
      tmp->width = opWidth(op) + 2;
    else
      fprintf(stderr,"Error: bad op used after wide instruction (%s)\n",
          jvm_opcode[op].op);
  }

  pc += tmp->width;

  return tmp;
}

/*****************************************************************************
 *                                                                           *
 * bytecode0                                                                 *
 *                                                                           *
 * inserts the given instruction into the code graph.                        *
 *                                                                           *
 *****************************************************************************/

CodeGraphNode *
bytecode0(enum _opcode op)
{
  return bytecode1(op,0);
}

/*****************************************************************************
 *                                                                           *
 * opWidth                                                                   *
 *                                                                           *
 * returns the width in bytes of this op, including operands.                *
 *                                                                           *
 *****************************************************************************/

u1
opWidth(enum _opcode op)
{
  return jvm_opcode[op].width;
}

/*****************************************************************************
 *                                                                           *
 * get_return_type_from_descriptor                                           *
 *                                                                           *
 * given a method descriptor, this function returns the string representing  *
 * the return type of the method.                                            *
 *                                                                           *
 *****************************************************************************/

char *
get_return_type_from_descriptor(char *desc)
{
  char *dptr;

  dptr = desc;

  /* skip characters until we hit the closing paren, making sure that
   * we dont go beyond the end of hte string. 
   */

  while(*dptr != ')') {
    if((*dptr == '\0') || (*(dptr+1) == '\0')) {
      fprintf(stderr,"Could not determine return type for descriptor '%s'\n",
         desc);
      return NULL;
    }

    dptr++;
  }

  /* now skip over the closing paren and return the remaining portion
   * of the descriptor */

  return strdup(dptr+1);
}

/*****************************************************************************
 *                                                                           *
 * get_retstring_from_field_desc                                             *
 *                                                                           *
 * given a field descriptor, this function returns the string representation *
 * of the appropriate java data type.                                        *
 *                                                                           *
 *****************************************************************************/

enum returntype
get_type_from_field_desc(char * fd)
{
  char * wrap;

  switch(fd[0]) {
    case 'B':
      return Integer;
    case 'C':
      return Character;
    case 'D':
      return Double;
    case 'F':
      return Float;
    case 'I':
      return Integer;
    case 'J':
      return Integer;
    case 'S':
      return Integer;
    case 'Z':
      return Logical;
    case 'V':
      return Object; /* no void in the array, so use object instead */ 
    case '[':
      return get_type_from_field_desc(fd+1);
    case 'L':
      wrap = get_wrapper_from_desc(fd);

      if(!strcmp(wrap, "StringW"))
        return String;
      else if(!strcmp(wrap, "complexW"))
        return Complex;
      else if(!strcmp(wrap, "intW"))
        return Integer;
      else if(!strcmp(wrap, "doubleW"))
        return Double;
      else if(!strcmp(wrap, "floatW"))
        return Float;
      else if(!strcmp(wrap, "booleanW"))
        return Logical;
      else if(!strcmp(wrap, "String"))
        return String;
      else if(!strcmp(wrap, "Object"))
        return Object;
      /* else drop to default case.  break intentionally missing. */
    default:
      fprintf(stderr,"get_type_from_field_desc() hit default case '%s'!!\n",
        fd);
      return Integer;
  }
}

/*****************************************************************************
 *                                                                           *
 * get_wrapper_from_desc                                                     *
 *                                                                           *
 * given the descriptor of one of the numeric wrappers, return just the      *
 * last part (e.g. Integer, Double, etc).  assume that desc points to the    *
 * initial 'L' of the field descriptor, but may contain extraneous chars     *
 * after the final ';'.                                                      *
 *                                                                           *
 *****************************************************************************/

char *
get_wrapper_from_desc(char *desc)
{
  char *ls, *dptr, *new;
 
  ls = dptr = desc;

  while( *dptr != ';' ) {
    if(*dptr == '\0')
      return desc;

    if(*dptr == '/')
      ls = dptr;

    dptr++;
  }

  new = strdup(ls+1);
  new[dptr-ls-1] = '\0';

  return new;
}

/*****************************************************************************
 *                                                                           *
 * get_field_desc_from_ident                                                 *
 *                                                                           *
 * given the AST node of some identifier, return the appropriate field       *
 * descriptor.                                                               *
 *                                                                           *
 *****************************************************************************/

char *
get_field_desc_from_ident(AST *node)
{
  char *fdesc;

  if(omitWrappers && !node->astnode.ident.passByRef)
    fdesc = field_descriptor[node->vartype][node->astnode.ident.dim];
  else
    fdesc = wrapped_field_descriptor[node->vartype][node->astnode.ident.dim];

  return fdesc;
}

/*****************************************************************************
 *                                                                           *
 * get_adapter_desc                                                          *
 *                                                                           *
 * given a pointer to the function arg list, this function returns the       *
 * corresponding descriptor.                                                 *
 *                                                                           *
 *****************************************************************************/

char *
get_adapter_desc(char *dptr, AST *arg)
{
  struct _str * temp_desc = NULL;
  int i;

  dptr = skipToken(dptr);

  for(i = 0; arg != NULL ; arg = arg->nextstmt, i++)
  {
    if(dptr == NULL) {
      fprintf(stderr,"get_adapter_desc():");
      fprintf(stderr,"mismatch between adapter call and prototype\n");
      break;
    }

    if(dptr[0] == '[') {
      temp_desc = strAppend(temp_desc, 
           field_descriptor[get_type_from_field_desc(dptr+1)][1]);
      temp_desc = strAppend(temp_desc, "I");
      dptr = skipToken(dptr);
    }
    else if ( (arg->nodetype == Identifier) && 
              type_lookup(cur_array_table,arg->astnode.ident.name))
    {
      if(omitWrappers && !isPassByRef_desc(dptr)) {
        temp_desc = strAppend(temp_desc, 
            field_descriptor[get_type_from_field_desc(dptr)][0]);
      }
      else {
        temp_desc = strAppend(temp_desc,
            field_descriptor[get_type_from_field_desc(dptr)][1]);
        temp_desc = strAppend(temp_desc, "I");
      }
    }
    else if( type_lookup(cur_external_table, arg->astnode.ident.name) )
    {
      temp_desc = strAppend(temp_desc, field_descriptor[Object][0]);
    }
    else
    {
      if(omitWrappers && !isPassByRef_desc(dptr)) {
        temp_desc = strAppend(temp_desc,
            field_descriptor[get_type_from_field_desc(dptr)][0]);
      }
      else {
        temp_desc = strAppend(temp_desc, 
            wrapped_field_descriptor[get_type_from_field_desc(dptr)][0]);
      }
    }

    dptr = skipToken(dptr);
  }

  return temp_desc->val;
}
