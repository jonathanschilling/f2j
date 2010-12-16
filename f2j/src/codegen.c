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
#include"f2j_externs.h"

/*****************************************************************************
 *   Global variables, a necessary evil when working with yacc.              *
 *****************************************************************************/

char 
  *unit_name,           /* name of this function/subroutine                  */
  *returnname,          /* return type of this prog. unit                    */
  *cur_filename,        /* name of the class file currently writing          */
  **funcname=input_func;/* input functions, EOF-detecting or non-detecting   */

Dlist 
  cur_assign_list = NULL, /* list of labels used in ASSIGN TO statements     */
  dummy_nodes = NULL,   /* list of dummy graph nodes to free later           */
  doloop = NULL,        /* stack of do loop labels                           */
  while_list = NULL,    /* stack of while loop labels                        */
  adapter_list = NULL,  /* list of adapter functions (see tech report)       */
  methcall_list = NULL; /* list of methods to be called by reflection        */

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

JVM_CLASS
  *cur_class_file;      /* class file for the current program unit           */

AST 
  *cur_equivList,       /* list of equivalences                              */
  *cur_unit;            /* program unit currently being translated.          */

BOOL 
  import_reflection,    /* does this class need to import reflection         */
  bytecode_gen=TRUE,    /* is bytecode generation currently enabled          */
  save_all_locals;      /* should all locals be declared static?             */

unsigned int 
  iostat_lvar = -1,     /* local var number of the temp iostat variable      */
  stdin_lvar = -1,      /* local var number of the EasyIn object             */
  iovec_lvar = -1,      /* local var number of the input/output Vector       */
  fmt_tab_lvar = -1,    /* local var number of the format stmt hashtable     */
  filemgr_lvar = -1;    /* locar var number of the fortran file manager      */

JVM_METHOD
  *main_method,         /* the primary method for this fortran program unit  */
  *cur_method;

JVM_EXCEPTION_TABLE_ENTRY
  * reflect_entry,      /* exception table entry for reflection exceptions.  */
  * access_entry;       /* exception table entry for access exceptions.      */

extern METHODTAB 
  intrinsic_toks[];     /* Fortran intrinsic function names.                 */

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
emit(AST * root)
{
    int c;
    int locals;

    switch(root->nodetype)
    {
      case 0:
        if(gendebug)
          fprintf(stderr,"Bad node\n");

        emit(root->nextstmt);
        break;
      case Progunit:
        {
          JVM_METHOD *clinit_method;
          HASHNODE *hashtemp;
          char *tmp_method_desc;
          char *methodname;
          char *classname;
          char *tmpname;

          if(gendebug)
            printf("Source.\n");

          save_all_locals = root->astnode.source.save_all;

          tmpname = root->astnode.source.progtype->
                       astnode.source.name->astnode.ident.name;

          classname = strdup(tmpname);
          lowercase(classname);

          /* check if this program unit is a PROGRAM.  if so, the
           * method name is "main".
           */
           
          if(root->astnode.source.progtype->nodetype == Program) {
            /* dup constant "main" so that we can free() later & won't
             * be trying to free non-heap memory
             */
            methodname = strdup("main");
          }
          else
            methodname = strdup(classname);

          classname[0] = toupper(classname[0]);

          cur_filename = bc_get_full_classname(classname, package_name);

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
          cur_assign_list = root->astnode.source.stmt_assign_list;
          cur_class_file = root->astnode.source.class = 
            bc_new_class(classname,inputfilename, "java.lang.Object",
                         package_name, F2J_CLASS_ACC); 

          bc_add_default_constructor(cur_class_file, F2J_INIT_ACC);
       
          if(gendebug)
            print_equivalences(cur_equivList);

          initialize_lists();

          clinit_method = bc_new_method(cur_class_file, "<clinit>", "()V", 
             strictFp ? F2J_STRICT_ACC : F2J_NORMAL_ACC);
          cur_method = clinit_method;

          locals = assign_varnums_to_arguments(
              root->astnode.source.progtype->astnode.source.args); 

          /* needs_reflection is determined during typecheck */

          if(root->astnode.source.progtype->astnode.source.needs_reflection)
            import_reflection = TRUE;
          else
            import_reflection = FALSE; 

          prepare_comments(root);

          open_output_file(root->astnode.source.progtype, classname);

          savefp = curfp;
          set_bytecode_status(cur_method, JAVA_AND_JVM);

          if(root->astnode.source.prologComments != NULL)
            emit_prolog_comments(root);

          if((hashtemp=type_lookup(function_table, tmpname)) != NULL)
            tmp_method_desc = hashtemp->variable->astnode.source.descriptor;
          else
            tmp_method_desc = MAIN_DESCRIPTOR;

          main_method = bc_new_method(cur_class_file, methodname, 
            tmp_method_desc, strictFp ? F2J_STRICT_ACC : F2J_NORMAL_ACC);

          if(!save_all_override)
            assign_varnums_to_locals(main_method, 
              root->astnode.source.typedecs);

          insert_fields(root);

          /* as part of creating a new classfile structure, we have 
           * already created an <init> method, the default constructor.
           * the class may also need a <clinit> method, the class
           * initializer.  the <clinit> method initializes any static
           * fields, DATA stmts, Strings which require new objects to
           * be created, etc.  here we create an empty CodeAttribute
           * structure and then emit the typedecs.  afterwards, we
           * check to see if any code was generated for <clinit>.
           * if so, we must create a method structure and add
           * that to the current classfile structure.  if not, we do
           * nothing.
           */

          if(root->astnode.source.typedecs)
            emit(root->astnode.source.typedecs);

          emit(root->astnode.source.progtype);

          /* check whether any class initialization code was generated.
           * if so, finish initializing the method and insert it into this
           * class.
           */

          if(bc_get_code_length(cur_method) > 0) {
            bc_append(cur_method, jvm_return);
            fprintf(indexfp,"%s:%s:%s\n",cur_filename, "<clinit>", "()V");
          }
          else {
            bc_remove_method(cur_method);
            bc_free_method(cur_method);
          }

          /* if this program unit is a function, then assign a local 
           * variable number to the implicit return variable.
           */

          if(root->astnode.source.progtype->nodetype == Function) {
            hashtemp=type_lookup(cur_type_table, unit_name);
            if(hashtemp)
              hashtemp->variable->astnode.ident.localvnum =
                bc_get_next_local(main_method, 
                   jvm_data_types[root->astnode.source.progtype->astnode.source.returns]);
          }

          cur_method = main_method;

          /* return stuff */
          if(!save_all_override)
            local_emit(cur_method, root->astnode.source.typedecs);

          /* If this program unit does any reading, we declare an instance of
           * the EasyIn class.   grab a local var for this, but dont worry
           * about releasing it, since we might need it throughout the life
           * of the method.
           */

          if(root->astnode.source.progtype->astnode.source.needs_input) {
            fprintf(curfp,"  EasyIn %s = new EasyIn();\n", F2J_STDIN);
            stdin_lvar = bc_get_next_local(cur_method, jvm_Object);

            c = cp_find_or_insert(cur_class_file, CONSTANT_Class, EASYIN_CLASS);
            bc_append(cur_method, jvm_new,c);
            bc_append(cur_method, jvm_dup);

            c = bc_new_methodref(cur_class_file, EASYIN_CLASS, "<init>", 
                   EASYIN_DESC);
            bc_append(cur_method, jvm_invokespecial, c);
            bc_gen_store_op(cur_method, stdin_lvar, jvm_Object);
          }

          /* Initialize a vector to be used for storing arguments to the
           * formatted write routine (f77write).
           */
          if(root->astnode.source.progtype->astnode.source.needs_output) {
            fprintf(curfp,"  IOVector %s = new IOVector();\n", F2J_IO_VEC);
            iovec_lvar = bc_get_next_local(cur_method, jvm_Object);

            c = cp_find_or_insert(cur_class_file, CONSTANT_Class, IOVECTOR_CLASS);
            bc_append(cur_method, jvm_new,c);
            bc_append(cur_method, jvm_dup);

            c = bc_new_methodref(cur_class_file, IOVECTOR_CLASS, "<init>", 
                   VECTOR_DESC);
            bc_append(cur_method, jvm_invokespecial, c);
            bc_gen_store_op(cur_method, iovec_lvar, jvm_Object);
          }

          /* initialize a variable for holding the return value of READ()
           * statements when the user has not specified IOSTAT=var.
           */
          if(root->astnode.source.progtype->astnode.source.needs_iostat) {
            fprintf(curfp, "  int %s = 0;\n", F2J_TMP_IOSTAT);
            iostat_lvar = bc_get_next_local(cur_method, jvm_Int);
            bc_push_int_const(cur_method, 0);
            bc_gen_store_op(cur_method, iostat_lvar, jvm_Int);
          }

          /* Initialize the file manager instance if necessary */

          if(root->astnode.source.progtype->astnode.source.needs_files) {
            fprintf(curfp, "  FortranFileMgr %s ", F2J_FILE_MGR);
            fprintf(curfp, "= FortranFileMgr.getInstance();\n");

            filemgr_lvar = bc_get_next_local(cur_method, jvm_Object);

            c = bc_new_methodref(cur_class_file, FILEMGR_CLASS, "getInstance",
                   FILEMGR_DESC);
            bc_append(cur_method, jvm_invokestatic, c);
            bc_gen_store_op(cur_method, filemgr_lvar, jvm_Object);
          }

          if(root->astnode.source.progtype->nodetype == Program) {
            fprintf(curfp, "  Etime.etime();\n");

            c = bc_new_methodref(cur_class_file, ETIME_CLASS, 
                      "etime", ETIME_DESC);
            bc_append(cur_method, jvm_invokestatic, c);
          }

          if(root->astnode.source.progtype->nodetype != Program)
            emit_string_length_hack(cur_method, root->astnode.source.progtype);

          if(root->astnode.source.progtype->astnode.source.needs_fmt_hashtab)
            fmt_tab_init(cur_method);

          /* if one of the arguments is a function, we must use the
           * reflection mechanism to perform the method call.
           */

          if(import_reflection) {
            reflect_declarations_emit(cur_method, 
               root->astnode.source.progtype->astnode.source.args);

            /* The 'catch' corresponding to the following try is generated
             * in case End. 
             */

            fprintf(curfp,"try {\n");

            /* start the exception handler from the next opcode */
            reflect_entry = (JVM_EXCEPTION_TABLE_ENTRY *) 
                 f2jalloc(sizeof(JVM_EXCEPTION_TABLE_ENTRY));
            reflect_entry->from = bc_append(cur_method, jvm_xxxunusedxxx);

            access_entry = (JVM_EXCEPTION_TABLE_ENTRY *) 
                 f2jalloc(sizeof(JVM_EXCEPTION_TABLE_ENTRY));
            access_entry->from = reflect_entry->from;
          }

          emit(root->astnode.source.statements);

          /* check if code was generated for this program unit's method.
           * if so, finish initializing the method and insert it into this
           * class.
           */

          if(bc_get_code_length(cur_method) > 0)
            fprintf(indexfp,"%s:%s:%s\n",cur_filename, methodname, 
                tmp_method_desc);

          f2jfree(methodname, strlen(methodname)+1);

          emit_invocations();

          emit_adapters();

          fprintf(curfp,"} // End class.\n");
          fclose(curfp);

          bc_write_class(cur_class_file, output_dir);
 
          if(gendebug)
            cp_dump(cur_class_file);
          
          bc_free_class(cur_class_file);

          free_lists();

          f2jfree(classname, strlen(classname)+1);
          f2jfree(cur_filename, strlen(cur_filename)+1);

          break;
        }
      case Subroutine:
        if(gendebug)
          printf("Subroutine.\n");

        returnname = NULL;	/* Subroutines return void. */
        cur_unit = root;
        unit_name = root->astnode.source.name->astnode.ident.name;

        if(gendebug)
          printf("Subroutine name: %s\n",  unit_name);

        constructor(root);
        break;
      case Function:
        if(gendebug)
          printf("Function.\n");

        returnname = root->astnode.source.name->astnode.ident.name;
        cur_unit = root;
        unit_name = root->astnode.source.name->astnode.ident.name;

        if(gendebug)
          printf("Function name: %s\n",  unit_name);

        constructor(root);
        break;
      case Program:
        if(gendebug)
          printf("Program.\n");

        returnname = NULL;	/* programs return void. */
        cur_unit = root;
        unit_name = root->astnode.source.name->astnode.ident.name;

        if(gendebug)
          printf("Program name: %s\n", unit_name);

        constructor(root);
        break;
      case Typedec:
        if(gendebug)
          printf("Typedec.\n");

        if(save_all_override)
          typedec_emit_all_static(cur_method, root);
        else
          typedec_emit(cur_method, root);

        if(root->nextstmt != NULL)	/* End of typestmt list. */
          emit(root->nextstmt);
        break;
      case DataList:
        if(gendebug)
          printf("Data.\n");

        data_emit(cur_method, root);
        if(root->nextstmt != NULL)	/* End of data list. */
          emit(root->nextstmt);
        break;
      case Specification:
        if(gendebug)
          printf("Specification.\n");

        if(root->nextstmt != NULL)	/* End of typestmt list. */
          emit(root->nextstmt);
        break;
      case Equivalence:
        if(gendebug)
          printf("Equivalence.\n");

        equiv_emit(cur_method, root);
        if(root->nextstmt != NULL)
          emit(root->nextstmt);
        break;
      case Statement:
        if(gendebug)
          printf("Statement.\n");

        if(root->nextstmt != NULL)	/* End of typestmt list. */
          emit(root->nextstmt);
        break;
      case Assignment:
        if(gendebug)
          printf("Assignment.\n");

        assign_emit(cur_method, root);
        fprintf(curfp, ";\n");
        if(root->nextstmt != NULL)
          emit(root->nextstmt);
        break;
      case StmtFuncDecl:
        if(gendebug)
          printf("StmtFuncDecl.\n");

        /* during codegen, statement function declarations are ignored.
         * in the future, it could be useful to emit some sort of comment
         * in the java source showing the declaration.
         */

        if(root->nextstmt != NULL)
          emit(root->nextstmt);
        break;
      case StmtLabelAssign:
        if(gendebug)
          printf("StmtLabelAssign.\n");

        assign_emit(cur_method, root);
        fprintf(curfp, ";\n");
        
        if(root->nextstmt != NULL)
          emit(root->nextstmt);
        break;
      case Call:
        if(gendebug)
          printf("Call.\n");

        call_emit(cur_method, root);
        if(root->nextstmt != NULL)	/* End of typestmt list. */
          emit(root->nextstmt);
        break;
      case Forloop:
        if(gendebug)
          printf("Forloop.\n");

        forloop_emit(cur_method, root);
        if(root->nextstmt != NULL)	/* End of typestmt list. */
          emit(root->nextstmt);
        break;
      case Blockif:
        if(gendebug)
          printf("Blockif.\n");

        blockif_emit(cur_method, root);
        if(root->nextstmt != NULL)	/* End of typestmt list. */
          emit(root->nextstmt);
        break;
      case Elseif:
        if(gendebug)
          printf("Elseif.\n");

        elseif_emit(cur_method, root);
        if(root->nextstmt != NULL)	/* End of typestmt list. */
          emit(root->nextstmt);
        break;
      case Else:
        if(gendebug)
          printf("Else.\n");

        else_emit(root);
        if(root->nextstmt != NULL)	/* End of typestmt list. */
          emit(root->nextstmt);
        break;
      case Logicalif:
        if(gendebug)
          printf("Logicalif.\n");

        logicalif_emit(cur_method, root);
        if(root->nextstmt != NULL)	/* End of typestmt list. */
          emit(root->nextstmt);
        break;
      case Arithmeticif:
        if(gendebug)
          printf("Arithmeticif.\n");

        arithmeticif_emit(cur_method, root);
        if(root->nextstmt != NULL)	/* End of typestmt list. */
          emit(root->nextstmt);
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

        return_emit(cur_method);
        if(root->nextstmt != NULL)	/* End of typestmt list. */
          emit(root->nextstmt);
        break;
      case Goto:
        if(gendebug)
          printf("Goto.\n");

        goto_emit(cur_method, root);
        if(root->nextstmt != NULL)
          emit(root->nextstmt);
        break;
      case ComputedGoto:
        if(gendebug)
          printf("Computed Goto.\n");

        computed_goto_emit(cur_method, root);
        if(root->nextstmt != NULL)
          emit(root->nextstmt);
        break;
      case AssignedGoto:
        if(gendebug)
          printf("Assigned Goto.\n");

        assigned_goto_emit(cur_method, root);
        if(root->nextstmt != NULL)
          emit(root->nextstmt);
        break;
      case Label:
        if(gendebug)
          printf("Label.\n");

        label_emit(cur_method, root);
        if(root->nextstmt != NULL)	/* End of typestmt list. */
          emit(root->nextstmt);
        break;
      case Write:
        if(gendebug)
          printf("Write statement.\n");

        write_emit(cur_method, root);
        if(root->nextstmt != NULL)
          emit(root->nextstmt);
        break;
      case Read:
        if(gendebug)
          printf("Read statement.\n");

        read_emit(cur_method, root);
        if(root->nextstmt != NULL)
          emit(root->nextstmt);
        break;
      case Format:
        if(gendebug)
          printf("skipping format statement\n");

        if(root->nextstmt != NULL)
          emit(root->nextstmt);
        break;
      case Stop:
        if(gendebug)
          printf("Stop.\n");

        stop_emit(cur_method, root);

        if(root->nextstmt != NULL)
          emit(root->nextstmt);
        break;
      case Pause:
        if(gendebug)
          printf("Pause.\n");

        pause_emit(cur_method, root);

        if(root->nextstmt != NULL)
          emit(root->nextstmt);
        break;
      case End:
        if(gendebug)
          printf("End.\n");
        end_emit(cur_method);
        break;
      case Save:
        if(gendebug)
          printf("Save (ignoring).\n");

        if(root->nextstmt != NULL)
          emit(root->nextstmt);
        break;
      case Common:
        fprintf(stderr,"Warning: hit case Common in emit()\n");
        if(root->nextstmt != NULL)
          emit(root->nextstmt);
        break;
      case CommonList:
        if(gendebug)
          printf("Common.\n");

        common_emit(root);
        if(root->nextstmt != NULL)
          emit(root->nextstmt);
        break;
      case MainComment:
        while(root->nextstmt != NULL && root->nextstmt->nodetype == Comment)
          root = root->nextstmt;

        if(root->nextstmt != NULL)
          emit(root->nextstmt);
        break;
      case Comment:
        if(gendebug)
          printf("Comment.\n");

        comment_emit(root);

        if(root->nextstmt != NULL)
          emit(root->nextstmt);
        break;
      case Dimension:
        if(gendebug)
          printf("Dimension\n");
     
        /* ignore */

        if(root->nextstmt != NULL)
          emit(root->nextstmt);
        break;
      case Open:
        if(gendebug)
          printf("Open\n");
     
        open_emit(cur_method, root);
        
        if(root->nextstmt != NULL)
          emit(root->nextstmt);
        break;
      case Close:
        if(gendebug)
          printf("Close\n");

        close_emit(cur_method, root);

        if(root->nextstmt != NULL)
          emit(root->nextstmt);
        break;
      case Rewind:
        if(gendebug)
          printf("Rewind\n");

        reb_emit(cur_method, root);

        if(root->nextstmt != NULL)
          emit(root->nextstmt);
        break;
      case Backspace:
        if(gendebug)
          printf("Backspace\n");

        reb_emit(cur_method, root);

        if(root->nextstmt != NULL)
          emit(root->nextstmt);
        break;
      case Flush:
        if(gendebug)
          printf("Flush\n");

        reb_emit(cur_method, root);

        if(root->nextstmt != NULL)
          emit(root->nextstmt);
        break;
      case Endfile:
        if(gendebug)
          printf("Endfile\n");

        reb_emit(cur_method, root);

        if(root->nextstmt != NULL)
          emit(root->nextstmt);
        break;
      case Unimplemented:
        fprintf(curfp, 
           " ; // WARNING: Unimplemented statement in Fortran source.\n");
        if(root->nextstmt != NULL)
          emit(root->nextstmt);
        break;
      case Constant:
      default:
        fprintf(stderr,"emit(): Error, bad nodetype (%s)\n",
          print_nodetype(root));
        if(root->nextstmt != NULL)
          emit(root->nextstmt);
        break;
    }				/* switch on nodetype.  */

}

/*****************************************************************************
 *                                                                           *
 * fmt_tab_init                                                              *
 *                                                                           *
 * emit code to set up the hash table of format statements.                  *
 *                                                                           *
 *****************************************************************************/

void
fmt_tab_init(JVM_METHOD *meth)
{
  Dlist f_table, tmp;
  char *fstr;
  AST *node;
  int c;

  fprintf(curfp,"  java.util.Hashtable %s = new java.util.Hashtable();\n",
     F2J_FMT_TAB);
  fmt_tab_lvar = bc_get_next_local(cur_method, jvm_Object);

  c = cp_find_or_insert(cur_class_file, CONSTANT_Class, HASHTAB_CLASS);
  bc_append(cur_method, jvm_new,c);
  bc_append(cur_method, jvm_dup);

  c = bc_new_methodref(cur_class_file, HASHTAB_CLASS, "<init>",
         HASHTAB_DESC);
  bc_append(cur_method, jvm_invokespecial, c);
  bc_gen_store_op(cur_method, fmt_tab_lvar, jvm_Object);

  fprintf(curfp, "// Register format statetments\n");
  f_table = enumerate_symtable(cur_format_table);
  dl_traverse(tmp, f_table) {
    node = (AST *)dl_val(tmp);
    fstr = format2str(node->astnode.label.stmt);
    fprintf(curfp, "%s.put(new Integer(%d), ", F2J_FMT_TAB, 
        node->astnode.label.number);
    fprintf(curfp, "\"%s\");\n", fstr);
    bc_gen_load_op(meth, fmt_tab_lvar, jvm_Object);
    c = cp_find_or_insert(cur_class_file, CONSTANT_Class, JL_INTEGER);
    bc_append(meth, jvm_new, c);
    bc_append(meth, jvm_dup);
    bc_push_int_const(meth, node->astnode.label.number);
    c = bc_new_methodref(cur_class_file, JL_INTEGER, "<init>",
            NEW_INTEGER_DESC);
    bc_append(meth, jvm_invokespecial, c);
    bc_push_string_const(meth, fstr);
    c = bc_new_methodref(cur_class_file, HASHTAB_CLASS, "put",
            HASHTAB_PUT_DESC);
    bc_append(meth, jvm_invokevirtual, c);
    bc_append(meth, jvm_pop);
  }

  fprintf(curfp, "\n");
}

/*****************************************************************************
 *                                                                           *
 * prepare_comments                                                          *
 *                                                                           *
 * Here we check whether there was a block of prologue comment statements.   *
 * If that block is longer than the current javadoc comment block (or if     *
 * there is no javadoc comment block) then use the prologue instead.         *
 *                                                                           *
 *****************************************************************************/

void
prepare_comments(AST *root)
{
  AST *pc, *jc;

  if(genJavadoc) {
    pc = root->astnode.source.prologComments;
    jc = root->astnode.source.progtype->astnode.source.javadocComments;

    if(pc) {
      if(jc) {
        if(pc->astnode.ident.len > jc->astnode.ident.len) {
          jc->nodetype = Comment;
          pc->nodetype = MainComment;
          root->astnode.source.progtype->astnode.source.javadocComments = pc;
          root->astnode.source.prologComments = NULL;
        }
      }
      else {
        pc->nodetype = MainComment;
        root->astnode.source.progtype->astnode.source.javadocComments = pc;
        root->astnode.source.prologComments = NULL;
      }
    }
  }
}

/*****************************************************************************
 *                                                                           *
 * comment_emit                                                              *
 *                                                                           *
 * Handles generating comments.  We will either have a single-line comment   *
 * in root->astnode.ident.name or a comment block in                         *
 * root->astnode.ident.buffered_comments.                                    *
 *                                                                           *
 *****************************************************************************/

void
comment_emit(AST *root)
{
  if(curfp != NULL) {
    if(root->astnode.ident.name[0] != '\0') {
      fprintf(curfp, "// %s", root->astnode.ident.name);
      if(root->astnode.ident.name[strlen(root->astnode.ident.name)-1] != '\n')
        fprintf(curfp, "\n");
    }

    if(root->astnode.ident.buffered_comments) {
      char *cp;

      /* sanitize comment to avoid early termination of the comment.
       * basically just strip the trailing slash.
       */
      for(cp=root->astnode.ident.buffered_comments;*(cp+1) != '\0';cp++)
        if((*cp == '*') && (*(cp+1) == '/'))
          *cp = '_';

      fprintf(curfp, "/***\n");
      fprintf(curfp, "%s", root->astnode.ident.buffered_comments);
      fprintf(curfp, "***/\n");
    }
  }
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
}

/*****************************************************************************
 *                                                                           *
 * free_lists                                                                *
 *                                                                           *
 * frees memory associated with the global lists.                            *
 *                                                                           *
 *****************************************************************************/

void
free_lists(JVM_METHOD *meth)
{
  Dlist tmp;

  /* free memory from previous program units. */

  if(dummy_nodes) {
    dl_traverse(tmp, dummy_nodes)
      f2jfree(dl_val(tmp), sizeof(JVM_CODE_GRAPH_NODE));
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
set_bytecode_status(JVM_METHOD *meth, int mode)
{
  switch(mode) {
    case JVM_ONLY:
      bc_set_gen_status(meth, TRUE);
      if(curfp != devnull)
        savefp = curfp;
      curfp = devnull;
      break;
    case JAVA_ONLY:
      bc_set_gen_status(meth, FALSE);
      curfp = savefp;
      break;
    case JAVA_AND_JVM:
    default:
      bc_set_gen_status(meth, TRUE);
      bytecode_gen=TRUE;
      curfp = savefp;
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
reflect_declarations_emit(JVM_METHOD *meth, AST *root)
{
  HASHNODE *hashtemp, *ht2;
  AST *tempnode;
  int c;
  int meth_var_num = 0;

  for(tempnode = root; tempnode != NULL; tempnode = tempnode->nextstmt)
  {
    hashtemp = type_lookup(cur_external_table, tempnode->astnode.ident.name);
    if(hashtemp)
    {
      hashtemp->variable->astnode.ident.localvnum = 
          bc_get_next_local(meth, jvm_Object);

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
          exit(EXIT_FAILURE);
        }
      }

      bc_gen_load_op(meth, meth_var_num, jvm_Object);

      c = bc_new_methodref(cur_class_file, JL_OBJECT, "getClass",
            GETCLASS_DESC);
      bc_append(meth, jvm_invokevirtual, c);

      c = bc_new_methodref(cur_class_file, JL_CLASS, "getDeclaredMethods",
            GETMETHODS_DESC);
      bc_append(meth, jvm_invokevirtual, c);

      bc_push_int_const(meth, 0);
      bc_append(meth, jvm_aaload);
      bc_gen_store_op(meth, hashtemp->variable->astnode.ident.localvnum, 
         jvm_Object);
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
invocation_exception_handler_emit(JVM_CLASS *cclass, 
  JVM_METHOD *meth, JVM_EXCEPTION_TABLE_ENTRY *et)
{
  int c;
  unsigned int vnum;

  vnum = bc_get_next_local(meth, jvm_Object);

  /* emit handler for InvocationTargetException */
  et->target = bc_gen_store_op(meth, vnum, jvm_Object);

  c = bc_new_fieldref(cclass, JL_SYSTEM, "err", OUT_DESC);
  bc_append(meth, jvm_getstatic, c);

  c = cp_find_or_insert(cclass, CONSTANT_Class, STRINGBUFFER);
  bc_append(meth, jvm_new,c);
  bc_append(meth, jvm_dup);

  bc_push_string_const(meth, "Error Calling Method: ");

  c = bc_new_methodref(cclass, STRINGBUFFER, "<init>", STRBUF_DESC);
  bc_append(meth, jvm_invokespecial, c);

  bc_gen_load_op(meth, vnum,jvm_Object);

  c = bc_new_methodref(cclass, THROWABLE_CLASS, "getMessage", GETMSG_DESC);
  bc_append(meth, jvm_invokevirtual, c);

  c = bc_new_methodref(cclass, STRINGBUFFER, "append", 
        append_descriptor[String]);
  bc_append(meth, jvm_invokevirtual, c);

  c = bc_new_methodref(cclass, STRINGBUFFER, "toString", TOSTRING_DESC);
  bc_append(meth, jvm_invokevirtual, c);

  c = bc_new_methodref(cclass, PRINTSTREAM, "println",
        println_descriptor[String]);
  bc_append(meth, jvm_invokevirtual, c);

  /* artificially set stack depth at beginning of exception
   * handler to 1.
   */
  bc_set_stack_depth(et->target, 1);

  bc_release_local(meth, jvm_Object);
}

/*****************************************************************************
 *                                                                           *
 * pause_emit                                                                *
 *                                                                           *
 * Generate the code for a PAUSE statement.  If the statement has an         *
 * argument, print it to stderr before querying the user about continuing.   *
 * The PAUSE statement pauses the program and asks the user whether or not   *
 * to continue.                                                              *
 *                                                                           *
 *****************************************************************************/

void
pause_emit(JVM_METHOD *meth, AST *root)
{
  int c;

  if(root->astnode.constant.number[0] != 0) {
    fprintf(curfp,"org.netlib.util.Util.pause(\"%s\");\n",
       escape_double_quotes(root->astnode.constant.number));

    bc_push_string_const(meth, root->astnode.constant.number);
    c = bc_new_methodref(cur_class_file, UTIL_CLASS, "pause", PAUSE_DESC);
    bc_append(meth, jvm_invokestatic, c);
  }
  else {
    fprintf(curfp,"org.netlib.util.Util.pause();\n");

    c = bc_new_methodref(cur_class_file, UTIL_CLASS, "pause",
       PAUSE_NOARG_DESC);
    bc_append(meth, jvm_invokestatic, c);
  }
}

/*****************************************************************************
 *                                                                           *
 * stop_emit                                                                 *
 *                                                                           *
 * Generate the code for a STOP statement.  If the statement has an argument *
 * print it to stderr before exiting.                                        *
 *                                                                           *
 *****************************************************************************/

void
stop_emit(JVM_METHOD *meth, AST *root)
{
  int c;

  if(root->astnode.constant.number[0] != 0) {
    char *stop_msg;

    stop_msg = (char *)malloc(strlen(root->astnode.constant.number) + 7);

    if(!stop_msg) {
      fprintf(stderr, "malloc failed in stop_emit()\n");
      exit(EXIT_FAILURE);
    }

    strcpy(stop_msg, "STOP: ");
    strncat(stop_msg, root->astnode.constant.number, MAX_CONST_LEN);

    c = bc_new_fieldref(cur_class_file, JL_SYSTEM, "err", OUT_DESC);
    bc_append(meth, jvm_getstatic, c);

    bc_push_string_const(meth, stop_msg);
    c = bc_new_methodref(cur_class_file, PRINTSTREAM, "println",
          println_descriptor[String]);
    bc_append(meth, jvm_invokevirtual, c);

    fprintf(curfp, "System.err.println(\"STOP: %s\");\n",
       escape_double_quotes(root->astnode.constant.number));

    free(stop_msg);
  }

  fprintf (curfp, "System.exit(0);\n");
  bc_append(meth, jvm_iconst_0);
  c = bc_new_methodref(cur_class_file, JL_SYSTEM, "exit",
       EXIT_DESC);
  bc_append(meth, jvm_invokestatic, c);
}

/*****************************************************************************
 *                                                                           *
 * open_emit                                                                 *
 *                                                                           *
 * This emits code to open a file (i.e. Fortran OPEN statements).            *
 *                                                                           *
 *****************************************************************************/

void
open_emit(JVM_METHOD *meth, AST *root)
{
  JVM_CODE_GRAPH_NODE *if_node, *goto_node;
  int open_ref;

  /* if ERR is set, then generate the conditional branch in case of error */
  if(root->astnode.open.err > 0)
    fprintf(curfp, "  if((");

  bc_gen_load_op(meth, filemgr_lvar, jvm_Object);

  if(root->astnode.open.iostat) {
    name_emit (meth, root->astnode.open.iostat);
    fprintf(curfp, " =");
  }

  fprintf(curfp, "  %s.open(", F2J_FILE_MGR);
  expr_emit(meth, root->astnode.open.unit_expr);
  fprintf(curfp, ", ");

  if(root->astnode.open.file_expr) {
    expr_emit(meth, root->astnode.open.file_expr);
  }
  else {
    fprintf(curfp, "null");
    bc_append(meth, jvm_aconst_null);
  }

  fprintf(curfp, ", ");

  if(root->astnode.open.status) {
    expr_emit(meth, root->astnode.open.status);
  }
  else {
    fprintf(curfp, "null");
    bc_append(meth, jvm_aconst_null);
  }

  fprintf(curfp, ", ");

  if(root->astnode.open.access) {
    expr_emit(meth, root->astnode.open.access);
  }
  else {
    fprintf(curfp, "null");
    bc_append(meth, jvm_aconst_null);
  }

  fprintf(curfp, ", ");

  if(root->astnode.open.form) {
    expr_emit(meth, root->astnode.open.form);
  }
  else {
    fprintf(curfp, "null");
    bc_append(meth, jvm_aconst_null);
  }

  fprintf(curfp, ", ");

  if(root->astnode.open.recl) {
    expr_emit(meth, root->astnode.open.recl);
  }
  else {
    fprintf(curfp, "0");
    bc_append(meth, jvm_iconst_0);
  }

  fprintf(curfp, ", ");

  if(root->astnode.open.blank) {
    expr_emit(meth, root->astnode.open.blank);
  }
  else {
    fprintf(curfp, "null");
    bc_append(meth, jvm_aconst_null);
  }

  fprintf(curfp, ", ");

  if(root->astnode.open.iostat || (root->astnode.open.err > 0)) {
    bc_append(meth, jvm_iconst_0);
    fprintf(curfp, "false");
  }
  else {
    bc_append(meth, jvm_iconst_1);
    fprintf(curfp, "true");
  }

  open_ref = bc_new_methodref(cur_class_file, FILEMGR_CLASS,
               "open", OPEN_DESC);
  bc_append(meth, jvm_invokevirtual, open_ref);

  if((root->astnode.open.err > 0) && root->astnode.open.iostat)
    bc_append(meth, jvm_dup);

  /* if IOSTAT is set, then emit the LHS assignment to set the ret value */
  if(root->astnode.open.iostat)
    LHS_bytecode_emit(meth, root->astnode.open.iostat->parent);

  if(root->astnode.open.err > 0) {
    if_node = bc_append(meth, jvm_ifeq);

    goto_node = bc_append(meth, jvm_goto);

    bc_set_integer_branch_label(goto_node, root->astnode.open.err);

    bc_set_branch_target(if_node, bc_append(meth, jvm_xxxunusedxxx));

    fprintf(curfp, ")) != 0)\n");
    fprintf(curfp,"    Dummy.go_to(\"%s\",%d);\n",cur_filename,
        root->astnode.open.err);
  }
  else {
    fprintf(curfp, ");\n");
    if(!root->astnode.open.iostat)
      bc_append(meth, jvm_pop);
  }
}

/*****************************************************************************
 *                                                                           *
 * reb_emit                                                                  *
 *                                                                           *
 * This emits code to handle REWIND/ENDFILE/BACKSPACE statements.            *
 * the codegen for all three is basically the same, so they are grouped here *
 *                                                                           *
 *****************************************************************************/

void
reb_emit(JVM_METHOD *meth, AST *root)
{
  JVM_CODE_GRAPH_NODE *if_node, *goto_node;
  int mref;
  char *func, *desc;

  if(gendebug)
    printf("in reb_emit(), nodetype is %s\n", print_nodetype(root));

  switch(root->nodetype) {
    case Rewind:
      func = "rewind";
      desc = REWIND_DESC;
      break;
    case Endfile:
      func = "endfile";
      desc = ENDFILE_DESC;
      break;
    case Backspace:
      func = "backspace";
      desc = BACKSPACE_DESC;
      break;
    case Flush:
      func = "flush";
      desc = FLUSH_DESC;
      break;
    default:
      fprintf(stderr, "Internal error: hit unexepected case in reb_emit()\n");
      exit(EXIT_FAILURE);
  }

  /* if ERR is set, then generate the conditional branch in case of error */
  if(root->astnode.reb.err > 0)
    fprintf(curfp, "  if((");

  bc_gen_load_op(meth, filemgr_lvar, jvm_Object);

  if(root->astnode.reb.iostat) {
    name_emit (meth, root->astnode.reb.iostat);
    fprintf(curfp, " =");
  }

  fprintf(curfp, "  %s.%s(", F2J_FILE_MGR, func);
  expr_emit(meth, root->astnode.reb.unit_expr);
  fprintf(curfp, ", ");

  if(root->astnode.reb.iostat || (root->astnode.reb.err > 0)) {
    bc_append(meth, jvm_iconst_0);
    fprintf(curfp, "false");
  }
  else {
    bc_append(meth, jvm_iconst_1);
    fprintf(curfp, "true");
  }

  mref = bc_new_methodref(cur_class_file, FILEMGR_CLASS,
               func, desc);
  bc_append(meth, jvm_invokevirtual, mref);

  if((root->astnode.reb.err > 0) && root->astnode.reb.iostat)
    bc_append(meth, jvm_dup);

  /* if IOSTAT is set, then emit the LHS assignment to set the ret value */
  if(root->astnode.reb.iostat)
    LHS_bytecode_emit(meth, root->astnode.reb.iostat->parent);

  if(root->astnode.reb.err > 0) {
    if_node = bc_append(meth, jvm_ifeq);

    goto_node = bc_append(meth, jvm_goto);

    bc_set_integer_branch_label(goto_node, root->astnode.reb.err);

    bc_set_branch_target(if_node, bc_append(meth, jvm_xxxunusedxxx));

    fprintf(curfp, ")) != 0)\n");
    fprintf(curfp,"    Dummy.go_to(\"%s\",%d);\n",cur_filename,
        root->astnode.reb.err);
  }
  else {
    fprintf(curfp, ");\n");
    if(!root->astnode.reb.iostat)
      bc_append(meth, jvm_pop);
  }
}

/*****************************************************************************
 *                                                                           *
 * close_emit                                                                *
 *                                                                           *
 * This emits code to close a file (i.e. Fortran CLOSE statements).          *
 *                                                                           *
 *****************************************************************************/

void
close_emit(JVM_METHOD *meth, AST *root)
{
  JVM_CODE_GRAPH_NODE *if_node, *goto_node;
  int close_ref;

  /* if ERR is set, then generate the conditional branch in case of error */
  if(root->astnode.close.err > 0)
    fprintf(curfp, "  if((");

  bc_gen_load_op(meth, filemgr_lvar, jvm_Object);

  if(root->astnode.close.iostat) {
    name_emit (meth, root->astnode.close.iostat);
    fprintf(curfp, " =");
  }

  fprintf(curfp, "  %s.close(", F2J_FILE_MGR);
  expr_emit(meth, root->astnode.close.unit_expr);
  fprintf(curfp, ", ");

  if(root->astnode.close.status) {
    expr_emit(meth, root->astnode.close.status);
  }
  else {
    fprintf(curfp, "null");
    bc_append(meth, jvm_aconst_null);
  }

  fprintf(curfp, ", ");

  if(root->astnode.close.iostat || (root->astnode.close.err > 0)) {
    bc_append(meth, jvm_iconst_0);
    fprintf(curfp, "false");
  }
  else {
    bc_append(meth, jvm_iconst_1);
    fprintf(curfp, "true");
  }

  close_ref = bc_new_methodref(cur_class_file, FILEMGR_CLASS,
               "close", CLOSE_DESC);
  bc_append(meth, jvm_invokevirtual, close_ref);

  if((root->astnode.close.err > 0) && root->astnode.close.iostat)
    bc_append(meth, jvm_dup);

  /* if IOSTAT is set, then emit the LHS assignment to set the ret value */
  if(root->astnode.close.iostat)
    LHS_bytecode_emit(meth, root->astnode.close.iostat->parent);

  if(root->astnode.close.err > 0) {
    if_node = bc_append(meth, jvm_ifeq);

    goto_node = bc_append(meth, jvm_goto);

    bc_set_integer_branch_label(goto_node, root->astnode.close.err);

    bc_set_branch_target(if_node, bc_append(meth, jvm_xxxunusedxxx));

    fprintf(curfp, ")) != 0)\n");
    fprintf(curfp,"    Dummy.go_to(\"%s\",%d);\n",cur_filename,
        root->astnode.close.err);
  }
  else {
    fprintf(curfp, ");\n");
    if(!root->astnode.close.iostat)
      bc_append(meth, jvm_pop);
  }
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
end_emit(JVM_METHOD *meth)
{
  JVM_CODE_GRAPH_NODE *goto_node, *goto_node2;
  int c;

  if(import_reflection) {
    /* this goto skips the execption handlers under normal execution */
    goto_node = bc_append(meth, jvm_goto);

    /* set the end point for the exception handlers. */
    reflect_entry->to = goto_node;
    access_entry->to = goto_node;

    invocation_exception_handler_emit(cur_class_file, meth, reflect_entry);
    goto_node2 = bc_append(meth, jvm_goto);
    invocation_exception_handler_emit(cur_class_file, meth, access_entry);

    c = cp_find_or_insert(cur_class_file, CONSTANT_Class, INVOKE_EXCEPTION);
    reflect_entry->catch_type = c;

    c = cp_find_or_insert(cur_class_file, CONSTANT_Class, ACCESS_EXCEPTION);
    access_entry->catch_type = c;

    bc_add_exception_handler(meth, reflect_entry);
    bc_add_exception_handler(meth, access_entry);

    bc_set_branch_target(goto_node, bc_append(meth, jvm_xxxunusedxxx));
    bc_set_branch_target(goto_node2, bc_append(meth, jvm_xxxunusedxxx));

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
  switch(bc_get_last_opcode(meth)) {
    case jvm_ireturn:
    case jvm_lreturn:
    case jvm_freturn:
    case jvm_dreturn:
    case jvm_areturn:
    case jvm_return:
      /* do nothing */
      break;
    default:
      return_emit(meth);
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
return_emit(JVM_METHOD *meth)
{
  /* for bytecode, check if the current program unit is a
   * Function.  if so, we push the implicit return value
   * on the stack and return.  otherwise, just return void.
   */

  if(returnname) {
    HASHNODE *ht;
    int rlv=0;

    ht = type_lookup(cur_type_table, returnname);
    if(!ht) {
      fprintf(stderr,"Bad news: can't find return name '%s' in symtab.\n",
         returnname);
      rlv = 0;
    }
    else
      rlv = ht->variable->astnode.ident.localvnum;

    if(omitWrappers && !cgPassByRef(returnname))
      pushVar(cur_class_file, meth, cur_unit->vartype, FALSE, cur_filename,
              returnname, field_descriptor[cur_unit->vartype][0],
              rlv, FALSE);
    else
      pushVar(cur_class_file, meth, cur_unit->vartype, FALSE, cur_filename,
              returnname, wrapped_field_descriptor[cur_unit->vartype][0],
              rlv, TRUE);
    bc_append(meth, return_opcodes[cur_unit->vartype]);
  }
  else
    bc_append(meth, jvm_return);
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

  if(!type_lookup(cur_type_table, root->astnode.ident.name))
    return;

  /* check whether this is a local var.  if so, then it does not need to
   * be emitted as a static field of this class, so just return now.
   */

  if(gendebug){
     printf("field_emit: %s localvnum=%d\n", root->astnode.ident.name, 
                                             root->astnode.ident.localvnum);
  }
  if(root->astnode.ident.localvnum != -1){
    return;
  }

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

  bc_add_field(cur_class_file, name, desc, F2J_NORMAL_ACC);
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
          if(gendebug){
             printf("calling field_emit from insert_fields\n");
          }
          field_emit(dec);
        }
      }
    } 
    else if(temp->nodetype == Equivalence) {
      /* for each group of equivalenced variables... */

      for(etmp = temp->astnode.equiv.nlist;etmp != NULL;etmp = etmp->nextstmt)
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
    if(temp->astnode.ident.name[strlen(temp->astnode.ident.name)-1] != '\n')
      fprintf(curfp, "\n");
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
equiv_emit(JVM_METHOD *meth, AST *root)
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
        vardec_emit(meth, ht->variable, curType, "public static ");
    }
  }
}

/*****************************************************************************
 *                                                                           *
 * find_commonblock                                                          *
 *                                                                           *
 * finds a common block entry in the .f2j file.                              *
 *                                                                           *
 *****************************************************************************/

JVM_METHODREF *
find_commonblock(char *cblk_name, Dlist dt)
{
  char *temp_commonblockname;
  JVM_METHODREF *mtmp;

  temp_commonblockname = (char *) f2jalloc(strlen(cblk_name) +
      strlen(CB_PREFIX) + 1);

  sprintf(temp_commonblockname, "%s%s", CB_PREFIX, cblk_name);

  if(gendebug)
    printf("#@#@ looking for temp_commonblockname = '%s'\n",
      temp_commonblockname);

  mtmp = find_method(temp_commonblockname, dt);

  f2jfree(temp_commonblockname, strlen(temp_commonblockname)+1);

  return mtmp;
}

/*****************************************************************************
 *                                                                           *
 *  skipCommonVarEntry                                                       *
 *                                                                           *
 *  This function returns a pointer to the next common block variable in     *
 *  the common block entry of an .f2j file.                                  *
 *                                                                           *
 *****************************************************************************/

char *
skipCommonVarEntry(char *p)
{
  if(!p || (*p == '\0')) return NULL;
 
  p++;  /* skip over CB_DELIMITER */
 
  while(*p != CB_DELIMITER)
    if(*p == '\0')
      return NULL;
    else
      p++;
 
  return p;
}
 
/*****************************************************************************
 *                                                                           *
 *  getVarDescFromCommonEntry                                                *
 *                                                                           *
 *  This function returns the descriptor from a common block entry obtained  *
 *  an .f2j file.                                                            *
 *                                                                           *
 *****************************************************************************/

char *
getVarDescFromCommonEntry(const char *p)
{
  char *newdesc = (char *) f2jalloc(strlen(p) + 1);  /* upper bound on len */
  char *np = newdesc;
 
  p++;  /* skip over CB_DELIMITER */
 
  while((*p != '\0') && (*p != CB_SEPARATOR))
    *np++ = *p++;
 
  *np = '\0';
 
  return newdesc;
}
 
/*****************************************************************************
 *                                                                           *
 *  getVarNameFromCommonEntry                                                *
 *                                                                           *
 *  This function returns the name from a common block entry obtained from   *
 *  an .f2j file.                                                            *
 *                                                                           *
 *****************************************************************************/

char *
getVarNameFromCommonEntry(const char *p)
{
  char *newdesc = (char *) f2jalloc(strlen(p) + 1);  /* upper bound on len */
  char *np = newdesc;
 
  while((*p != '\0') && (*p++ != CB_SEPARATOR))
    /* spin */ ;
 
  while((*p != '\0') && (*p != CB_DELIMITER))
    *np++ = *p++;
 
  *np = '\0';
 
  return newdesc;
}

/*****************************************************************************
 *                                                                           *
 *  assign_merged_names                                                      *
 *                                                                           *
 *  This function loops through all the variables in a given COMMON block    *
 *  declaration and assigns the 'merged_name' and 'descriptor' fields to     *
 *  the values found in the .f2j files.  This allows having a COMMON block   *
 *  split across multiple Java packages.  Our current need for this feature  *
 *  stems from the fact that to allow for a user-specifiable XERBLA error    *
 *  reporting routine, we had to put it in another package.  Since the       *
 *  LAPACK testers use their own XERBLA which contains a COMMON block that   *
 *  is shared with the rest of the tester source, we needed this feature     *
 *  in order to run the "error-exits" tests.     3/14/01 --keith             *
 *                                                                           *
 *****************************************************************************/

void
assign_merged_names(AST *Ctemp, char *dp)
{
  HASHNODE *ht;
  AST *Ntemp;

  if(!dp) return;

  for(Ntemp=Ctemp->astnode.common.nlist;Ntemp!=NULL;Ntemp=Ntemp->nextstmt)
  {
    if((ht=type_lookup(cur_type_table, Ntemp->astnode.ident.name))==NULL)
    {
      if(gendebug)
        printf("assign_merged_names: Var Not Found\n");
      continue;
    }

    if(!dp) {
      fprintf(stderr, "Error: descriptor mismatch for common block '%s'\n",
        Ctemp->astnode.common.name);
      fprintf(stderr, "  make sure that there aren't any conflicting .f2j\n");
      fprintf(stderr, "  files containing this common block.\n"); 
      exit(EXIT_FAILURE);
    }

    ht->variable->astnode.ident.merged_name = getVarNameFromCommonEntry(dp);
    ht->variable->astnode.ident.descriptor = getVarDescFromCommonEntry(dp);
    
    dp = skipCommonVarEntry(dp);
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
  JVM_METHOD *clinit_method;
  HASHNODE *hashtemp, *cbht, *ht3;
  JVM_METHODREF *mtmp;
  AST *Ctemp, *Ntemp, *temp;
  char *common_classname=NULL, *filename=NULL, *varname;
  FILE *commonfp;
  char * prefix = strtok(strdup(inputfilename),".");
  JVM_CLASS *save_class_file;
  char *save_filename;
  int i, local_count, table_count;

  /* save the current global variables pointing to the class file.  this is
   * necessary because we're in the middle of generating the class file
   * for the current fortran program unit, but now we need to generate some
   * classes to hold COMMON blocks and we dont want to alter the pc, stack,
   * etc for the current class.
   */
  save_class_file = cur_class_file; 
  save_filename = cur_filename; 

  /* set cur_filename to NULL in case we decide not to reset it here and
   * end up trying to free it later.  then we don't blow away the
   * original memory.
   */
  cur_filename = NULL;

  /*
   * Ctemp loops through each common block name specified
   * in the COMMON statement and Ntemp loops through each
   * variable in each common block. 
   */

  for(Ctemp=root->astnode.common.nlist;Ctemp!=NULL;Ctemp=Ctemp->nextstmt)
  {
    if(Ctemp->astnode.common.name != NULL) 
    {
      if(gendebug)
        printf("common_emit.2: lookin for common block '%s'\n", 
          Ctemp->astnode.common.name);

      mtmp = find_commonblock(Ctemp->astnode.common.name, descriptor_table);
      if(mtmp) {
        if(gendebug)
          printf("common_emit.3: block found.  %s,%s,%s\n", mtmp->classname,
            mtmp->methodname, mtmp->descriptor);
        
        assign_merged_names(Ctemp, mtmp->descriptor);
        continue;
      }
      else {
        if(gendebug)
          printf("common name not found in descriptor table\n");
      }

      ht3 = type_lookup(global_common_table, Ctemp->astnode.common.name);
      if(ht3) {
        if(gendebug) {
          printf("found %s in the global_common_table\n",
            Ctemp->astnode.common.name);
          if(ht3->variable->astnode.common.descriptor)
            printf("  desc = %s\n", ht3->variable->astnode.common.descriptor);
          else
            printf("  desc is null\n");
        }

        if(ht3->variable->astnode.common.descriptor) {
          assign_merged_names(Ctemp, ht3->variable->astnode.common.descriptor);
          continue;
        }
      }

      cbht = type_lookup(common_block_table, Ctemp->astnode.common.name);

      /* common block filename will be a concatenation of
       * the original input filename and the name of this
       * common block.
       */
      common_classname = (char *)f2jrealloc(common_classname,
         strlen(prefix) + strlen(Ctemp->astnode.common.name) + 2);
      sprintf(common_classname,"%s_%s",prefix,Ctemp->astnode.common.name);

      if(gendebug)
        printf("emitting common block '%s'\n",common_classname);

      cur_filename = bc_get_full_classname(common_classname, package_name);

      filename = (char *)f2jrealloc(filename,
                                    strlen(cur_filename) + 6);
      sprintf(filename,"%s.java", cur_filename);
     
      cur_class_file = bc_new_class(common_classname,inputfilename, 
          "java.lang.Object", package_name, F2J_CLASS_ACC); 

      bc_add_default_constructor(cur_class_file, F2J_INIT_ACC);
      
      clinit_method = bc_new_method(cur_class_file, "<clinit>", "()V", 
         strictFp ? F2J_STRICT_ACC : F2J_NORMAL_ACC);

      if(gendebug)
        printf("## going to open file: '%s'\n", filename);

      if((commonfp = bc_fopen_fullpath(filename,"w", output_dir))==NULL) 
      {
        fprintf(stderr,"Cannot open output file '%s'.\n",filename);
        perror("Reason");
        exit(EXIT_FAILURE);
      }
  
      curfp = commonfp;
      
      if(package_name != NULL)
        fprintf(curfp,"package %s;\n",package_name);

      /* import util package for object wrapper classes */

      fprintf(curfp,"import org.netlib.util.*;\n\n");

      if(Ctemp->astnode.common.name != NULL)
        fprintf(curfp,"public class %s_%s\n{\n",prefix,
          Ctemp->astnode.common.name);

      fprintf(indexfp,"%s:common_block/%s:",cur_filename,
        Ctemp->astnode.common.name);

      local_count = table_count = 0;
      for(Ntemp=Ctemp->astnode.common.nlist;Ntemp!=NULL;Ntemp=Ntemp->nextstmt)
        local_count++;

      if(cbht)
        while(((char **)cbht->variable)[table_count])
          table_count++;

      if(gendebug)
        printf("common block variable count: %d local, %d from table\n",
          local_count, table_count);
     
      Ntemp = Ctemp->astnode.common.nlist;
      for(i=0;i<MAX(local_count,table_count);i++)
      {
        if(Ntemp) {
          varname = Ntemp->astnode.ident.name;
          Ntemp = Ntemp->nextstmt;
        }
        else {
          if(cbht)
            varname = ((char **)cbht->variable)[i];
          else
            break;  /* don't really expect to hit this case */
        }

        if(gendebug)
        {
          printf("Common block %s -- %s\n",Ctemp->astnode.common.name, varname);
          printf("Looking up %s in the type table\n", varname);
        }

        /* each variable in the common block should have a type
         * declaration associated with it.
         */

        if((hashtemp=type_lookup(cur_type_table, varname)) == NULL) {
          fprintf(stderr,"Error: can't find type for common %s\n", varname);
          if(gendebug)
            printf("Not Found\n");
          continue;
        }

        if(gendebug)
          printf("Found\n");

        temp = hashtemp->variable;

        if(gendebug)
          printf("drew field_emit: %c%s, %s (parent=%p)\n", CB_DELIMITER, 
             getVarDescriptor(temp), getCommonVarName(varname),
             (void *)temp->parent);

        fprintf(indexfp,"%c%s,%s",CB_DELIMITER, getVarDescriptor(temp),
            getCommonVarName(varname));

        if(ht3) {
          char *str1, *str2, *desctmp;
          AST *ht3var;
          int slen;

          ht3var = ht3->variable;

          str1 = getVarDescriptor(temp);
          str2 = getCommonVarName(varname);

          slen = strlen(str1) + strlen(str2) + 3;

          if(ht3var->astnode.common.descriptor)
            slen += strlen(ht3var->astnode.common.descriptor);

          desctmp = (char *)f2jalloc(slen);

          if(ht3var->astnode.common.descriptor)
            sprintf(desctmp,"%s%c%s,%s",ht3var->astnode.common.descriptor,
               CB_DELIMITER, str1, str2);
          else
            sprintf(desctmp,"%c%s,%s",CB_DELIMITER, str1, str2);

          ht3->variable->astnode.common.descriptor = desctmp;
        }

        field_emit(temp);

        /* now emit the variable declaration as with any
         * other variable.
         */

        vardec_emit(clinit_method, temp, temp->vartype, "public static ");
      }
      fprintf(indexfp,"\n");

      if(Ctemp->astnode.common.name != NULL)
        fprintf(curfp,"}\n");
  
      fclose(curfp);

      /* check whether any class initialization code was generated.
       * if so, finish initializing the method and insert it into this
       * class.
       */
      if(bc_get_code_length(clinit_method) > 0) {
        bc_append(clinit_method, jvm_return);
        fprintf(indexfp,"%s:%s:%s\n",cur_filename, "<clinit>", "()V");
      }
      else {
        bc_remove_method(clinit_method);
        bc_free_method(clinit_method);
      }

      bc_write_class(cur_class_file, output_dir);
      bc_free_class(cur_class_file);
    }
  }

  curfp = javafp;

  if(prefix) f2jfree(prefix,strlen(prefix)+1);
  if(common_classname) f2jfree(common_classname,strlen(common_classname)+1);
  if(filename) f2jfree(filename,strlen(filename)+1);
  if(cur_filename) f2jfree(cur_filename,strlen(cur_filename)+1);

  /* restore previously saved globals */
  cur_class_file = save_class_file;
  cur_filename = save_filename; 
}

/*****************************************************************************
 *                                                                           *
 * getNameFromCommonDesc                                                     *
 *                                                                           *
 * given a common block 'descriptor' (as found in the .f2j file), we return  *
 * the variable name  corresponding to the Nth variable in the common block. *
 *                                                                           *
 *****************************************************************************/

char *
getNameFromCommonDesc(char *desc, int idx)
{
  int len = 0, del_count = 0;
  char *p, *name;

  /* skip initial delimiter */
  p = desc + 1;

  while(del_count < idx) {
    p = bc_next_desc_token(p);      /* skip the descriptor */
    p++;                   /* skip the comma */

    /* skip until next descriptor */
    while((*p != CB_DELIMITER) && (*p != '\0'))
      p++;

    del_count++;
    p++;                   /* skip the delimiter */
  }

  if(p == '\0')
    return NULL;

  p = bc_next_desc_token(p);
  p++;

  while((*(p+len) != CB_DELIMITER) && (*(p+len) != '\0'))
    len++;

  name = (char *) f2jalloc(len+2);
  strncpy(name, p, len+1);
  name[len] = '\0';

  return name;
}

/*****************************************************************************
 *                                                                           *
 * getFieldDescFromCommonDesc                                                *
 *                                                                           *
 * given a common block 'descriptor' (as found in the .f2j file), we return  *
 * the descriptor corresponding to the Nth variable in the common block.     *
 *                                                                           *
 *****************************************************************************/

char *
getFieldDescFromCommonDesc(char *desc, int idx)
{
  int len = 0, del_count = 0;
  char *p, *name;

  /* skip initial delimiter */
  p = desc + 1;

  while(del_count < idx) {
    /* skip until next descriptor */
    while((*p != CB_DELIMITER) && (*p != '\0'))
      p++;

    del_count++;
    p++;                   /* skip the delimiter */
  }

  if(p == '\0')
    return NULL;

  while((*(p+len) != CB_SEPARATOR) && (*(p+len) != '\0'))
    len++;

  name = (char *) f2jalloc(len+2);
  strncpy(name, p, len+1);
  name[len] = '\0';

  return name;
}

/*****************************************************************************
 *                                                                           *
 * getCommonVarName                                                          *
 *                                                                           *
 * Given a node, this function returns the merged name of this variable in   *
 * the common block.  if the variable is not in a common block or if we      *
 * can't find the variable in the symbol table, return "unknown".            *
 *                                                                           *
 *****************************************************************************/

char *
getCommonVarName(char *varname)
{
  HASHNODE *ht2;

  if(type_lookup(cur_common_table, varname) != NULL) {
    ht2 = type_lookup(cur_type_table, varname);

    return ht2->variable->astnode.ident.merged_name;
  }

  return "Unknown";
}

/*****************************************************************************
 *                                                                           *
 * typedec_emit                                                              *
 *                                                                           *
 * this procedure only emits static variables, data and save. (drew)         *
 *                                                                           *
 *****************************************************************************/

void
typedec_emit (JVM_METHOD *meth, AST * root)
{
  AST *temp;
  HASHNODE *ht;
  enum returntype returns;

  returns = root->astnode.typeunit.returns;

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
      else {
        char *tempname;

        /* if this is an intrinsic then don't emit any warning since we
         * didn't want to emit a real declaration for this anyway.
         */
        tempname = strdup(temp->astnode.ident.name);
        uppercase(tempname);

        if(methodscan(intrinsic_toks, tempname)) {
          free(tempname);
          continue;
        }

        fprintf(stderr,"could not find %s\n", temp->astnode.ident.name);

        free(tempname);
      }
    }

    if(is_static(temp))
      vardec_emit(meth, temp, returns, "public static ");
  }
}                               /* Close typedec_emit(). */

/*****************************************************************************
 *                                                                           *
 * is_static                                                                 *
 *                                                                           *
 * this functions returns true if the stmt is a data or save and has not     *
 * been declared.(drew)                                                      *
 *                                                                           *
 *****************************************************************************/

BOOL
is_static(AST *root)
{
  AST *temp;
  HASHNODE *ht;

  temp = root;

  if(type_lookup(cur_args_table,temp->astnode.ident.name)) {
    if(gendebug)
      printf("@@ is_static(): %s: not static (is arg)\n",
        temp->astnode.ident.name);
    return FALSE;
  }
  else if(type_lookup(cur_data_table,temp->astnode.ident.name)) {
    if(gendebug)
      printf("@@ Variable %s: Found corresponding data stmt\n",
        temp->astnode.ident.name);

    ht = type_lookup(cur_type_table,temp->astnode.ident.name);

    if(ht == NULL)
      return FALSE;

    if(!ht->variable->astnode.ident.needs_declaration) {
      if(gendebug)
        printf("is_static: declared data statement\n");
      return FALSE;
    }

    if(gendebug)
      printf("is_static: undeclared data statement\n");

    return TRUE;
  }
  else if(type_lookup(cur_save_table,temp->astnode.ident.name)) {
    if(gendebug)
      printf("@@ Variable %s: Found corresponding SAVE stmt\n",
        temp->astnode.ident.name);
    return TRUE;
  }
  else if(type_lookup (cur_external_table, temp->astnode.ident.name)
          || type_lookup (cur_intrinsic_table, temp->astnode.ident.name)
          || type_lookup (cur_args_table, temp->astnode.ident.name)
          || type_lookup (cur_param_table, temp->astnode.ident.name)
          || type_lookup (cur_equiv_table, temp->astnode.ident.name)
          || type_lookup (cur_common_table, temp->astnode.ident.name)) {
    if(gendebug)
      printf("@@ is_static %s: no, it's a spec stmt\n",
        temp->astnode.ident.name);
    return FALSE;
  }
  else if(save_all_locals) {
    if(gendebug)
      printf("@@ Save Variable %s: SAVE all\n",
        temp->astnode.ident.name);
    return TRUE;
  }
  else{
    if(gendebug)
      printf("@@ Variable %s: Corresponding data stmt not found\n",
        temp->astnode.ident.name);

    if(type_lookup (cur_array_table, temp->astnode.ident.name)
         && f2j_arrays_static)
      return TRUE;
    else
      return FALSE;
  }
}

/*****************************************************************************
 *                                                                           *
 * is_local                                                                  *
 *                                                                           *
 * this function checks to see if the varibles are local and returns         *
 * true if they are. (drew)                                                  *
 *                                                                           *
 *****************************************************************************/

BOOL
is_local(AST *root){

  AST *temp;
  HASHNODE *hashtemp;
  char *tempname;
  BOOL isarg;

  temp = root;
  hashtemp = type_lookup (cur_args_table, temp->astnode.ident.name);
  isarg = hashtemp != NULL;

  if(f2j_arrays_static) {
    if(type_lookup (cur_array_table, temp->astnode.ident.name)
       && !type_lookup (cur_args_table, temp->astnode.ident.name)) {

      return FALSE;
    }
  }

  if(type_lookup(cur_data_table,temp->astnode.ident.name)) {
    if(gendebug)
      printf("@@ Variable %s: Found corresponding data stmt\n",
        temp->astnode.ident.name);

    return FALSE;
  }

  hashtemp = type_lookup(cur_equiv_table, temp->astnode.ident.name);
  if(hashtemp) {
    if(type_lookup(cur_common_table,temp->astnode.ident.name)) {
      fprintf(stderr,"Please dont mix COMMON and EQUIVALENCE.  ");
      fprintf(stderr,"I dont like it.  It scares me.\n");
    }else {
      fprintf(curfp,"  // %s equivalenced to %s\n",
        temp->astnode.ident.name,
        hashtemp->variable->astnode.ident.merged_name);
    }
    return FALSE;
  }

  if(type_lookup(cur_save_table,temp->astnode.ident.name))
    return FALSE;

  if(type_lookup(cur_common_table,temp->astnode.ident.name))
    return FALSE;

  /*
   * Dont emit anything for intrinsic functions.
   */

  tempname = strdup(temp->astnode.ident.name);
  uppercase(tempname);

  if(( methodscan (intrinsic_toks, tempname) != NULL)
   && (type_lookup(cur_intrinsic_table,temp->astnode.ident.name) != NULL))
  {
    f2jfree(tempname,strlen(tempname)+1);
    return FALSE;
  }

  f2jfree(tempname,strlen(tempname)+1);

   /*
    * Let's do the argument lookup first. No need to retype variables
    * that are already declared in the argument list, or declared
    * as externals.  So if it is already declared, loop again.
    */

  if (isarg)
  {
    if(gendebug)
      printf("### %s is in the args_table, so I'm skipping it.\n",
         temp->astnode.ident.name);
    return FALSE;
  }

  if(type_lookup(cur_external_table, temp->astnode.ident.name) != NULL)
  {
    /* skip externals */
    return FALSE;
  }

  if(save_all_locals && !isarg)
    return FALSE;

  if(gendebug)
     printf("Returning TRUE from is_local\n");

  return TRUE;
}

/*****************************************************************************
 *                                                                           *
 * local_emit                                                                *
 *                                                                           *
 * This function calls vardec_emit on local variables (drew)                 *
 *                                                                           *
 *****************************************************************************/

void
local_emit(JVM_METHOD *meth, AST *root)
{
  AST *temp, *temp2;
  HASHNODE *ht;
  enum returntype returns;

  if(gendebug)printf("in local_emit\n");

  temp2 = root;

  while(temp2 != NULL) {
    if(temp2->nodetype != Typedec) {
      temp2 = temp2->nextstmt;
      continue;
    }

    returns = temp2->astnode.typeunit.returns;
    if(gendebug)printf("in local_emit, returns=%s\n", returnstring[returns]);

    for(temp=temp2->astnode.typeunit.declist;temp!=NULL;temp=temp->nextstmt)
    {
      if(is_local(temp)==TRUE) {
        /* emit if it is local variable */
        if(gendebug) 
          printf("local variable found\n");

        ht = type_lookup(cur_type_table,temp->astnode.ident.name);
        if(!ht) {
          char *tempname;

          /* if this is an intrinsic then don't emit any warning since we
           * didn't want to emit a real declaration for this anyway.
           */
          tempname = strdup(temp->astnode.ident.name);
          uppercase(tempname);

          if(!methodscan(intrinsic_toks, tempname)) {
            fprintf(stderr,"Warning: local_emit() could not find '%s'\n",
                 temp->astnode.ident.name);
            fprintf(stderr,"vartype is: %s\n",returnstring[temp->vartype]);
          }

          free(tempname);
          continue;
        }

        if(gendebug)
          printf("Emitting local variable %s\n", temp->astnode.ident.name);
        vardec_emit(meth, temp, returns, "");
      }
    }

    temp2=temp2->nextstmt;
  }
}

/*****************************************************************************
 *                                                                           *
 * assign_varnums_to_locals                                                  *
 *                                                                           *
 * This routine assigns a local variable (aka register) number to every      *
 * variable that should not be static.                                       *
 *                                                                           *
 *****************************************************************************/

void
assign_varnums_to_locals(JVM_METHOD *meth, AST *root)
{
  AST *temp, *temp2;
  HASHNODE *ht;

  temp2 = root;

  while(temp2 != NULL) {
    if(temp2->nodetype != Typedec) {
      temp2 = temp2->nextstmt;
      continue;
    }

    for(temp=temp2->astnode.typeunit.declist;temp!=NULL;temp=temp->nextstmt)
    {
      if(is_local(temp)==TRUE) {
        ht = type_lookup(cur_type_table,temp->astnode.ident.name);
        if(!ht) {
          char *tempname;

          /* if this is an intrinsic then don't emit any warning since we
           * didn't want to emit a real declaration for this anyway.
           */
          tempname = strdup(temp->astnode.ident.name);
          uppercase(tempname);

          if(!methodscan(intrinsic_toks, tempname)) {
            fprintf(stderr,"assign_varnums_to_locals() could not find '%s'\n",
                 temp->astnode.ident.name);
            fprintf(stderr,"vartype is: %s\n",returnstring[temp->vartype]);
          }

          free(tempname);
          continue;
        }

        /* might want to check whether it's a double precision array & only 
         * grab one register in that case... kgs
         */
        ht->variable->astnode.ident.localvnum = 
           bc_get_next_local(meth, jvm_data_types[temp->vartype]);
        temp->astnode.ident.localvnum = ht->variable->astnode.ident.localvnum;

        if(gendebug)
          printf("assign_varnums_to_locals: %s -> slot %d %d\n", 
            temp->astnode.ident.name, ht->variable->astnode.ident.localvnum,
                                      temp->astnode.ident.localvnum);
      }
    }

    temp2=temp2->nextstmt;
  }

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
typedec_emit_all_static (JVM_METHOD *meth, AST * root)
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

  for(temp=root->astnode.typeunit.declist;temp != NULL;temp = temp->nextstmt)
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
      else {
        char *tempname;

        /* if this is an intrinsic then don't emit any warning since we
         * didn't want to emit a real declaration for this anyway.
         */
        tempname = strdup(temp->astnode.ident.name);
        uppercase(tempname);

        if(methodscan(intrinsic_toks, tempname)) {
          free(tempname);
          continue;
        }

        free(tempname);

        fprintf(stderr,"could not find %s\n", temp->astnode.ident.name);
      }
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

    hashtemp = type_lookup(cur_equiv_table, temp->astnode.ident.name);
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

    vardec_emit(meth, temp, returns, "public static ");
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
newarray_emit(JVM_METHOD *meth, enum returntype vtype)
{
  int c;

  switch(vtype) {
    case String:
    case Character:
      c = cp_find_or_insert(cur_class_file, CONSTANT_Class, "java/lang/String");
      bc_append(meth, jvm_anewarray, c);
      break;
    case Complex:
    case Double:
    case Float:
    case Integer:
    case Logical:
      bc_append(meth, jvm_newarray, jvm_array_type[vtype]);
      break;
    default:
      fprintf(stderr,"WARNING: newarray_emit() unknown vartype\n");
  }
}

/*****************************************************************************
 *                                                                           *
 * getMergedName                                                             *
 *                                                                           *
 * given an ident, return the merged name.                                   *
 *                                                                           *
 *****************************************************************************/

char *
getMergedName(AST *root)
{
  HASHNODE *ht, *ht2;
  char *name;

  if(type_lookup(cur_common_table,root->astnode.ident.name) != NULL) {
    ht2 = type_lookup(cur_type_table,root->astnode.ident.name);

    name = ht2->variable->astnode.ident.merged_name;
  }
  else if((ht=type_lookup(cur_equiv_table, root->astnode.ident.name))!=NULL)
    name = ht->variable->astnode.ident.merged_name;
  else
    name = root->astnode.ident.name;

  return name;
}

/*****************************************************************************
 *                                                                           *
 * getMergedDescriptor                                                       *
 *                                                                           *
 * given an ident, return the descriptor.                                    *
 *                                                                           *
 *****************************************************************************/

char *
getMergedDescriptor(AST *root, enum returntype returns)
{
  HASHNODE *ht, *ht2;
  char *desc;

  if(gendebug){
     printf("@@## looking for '%s' in common table\n", 
     root->astnode.ident.name); 
  }

  if(type_lookup(cur_common_table,root->astnode.ident.name)!=NULL) {
    if(gendebug){
        printf("@@## found! in common table\n");
    }
    ht2 = type_lookup(cur_type_table,root->astnode.ident.name);

    if(gendebug)printf("@@## ht2 is '%s'\n", ht2 ? "non-null": "NULL");
    desc = ht2->variable->astnode.ident.descriptor;
    if(gendebug)printf("@@## desc is '%s'\n", desc ? desc: "NULL");
  }
  else if((ht=type_lookup(cur_equiv_table, root->astnode.ident.name))!=NULL) {
    desc = ht->variable->astnode.ident.descriptor;
  }
  else {
    ht2 = type_lookup(cur_type_table,root->astnode.ident.name);

    if(ht2 && ht2->variable->astnode.ident.descriptor)
      desc = ht2->variable->astnode.ident.descriptor;
    else {
      desc = field_descriptor[returns][(root->astnode.ident.dim > 0)];
    }
  }

  return desc;
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
vardec_emit(JVM_METHOD *meth, AST *root, enum returntype returns, 
    char *prefix)
{
  char *name, *desc;
  HASHNODE *hashtemp;
  int count;
  AST *temp2;
  int c;
  struct var_info *ainf;

  if(type_lookup(cur_external_table, root->astnode.ident.name))
    return;

  ainf = get_var_info(root);

  if(gendebug) {
    printf("vardec emit %s\n", root->astnode.ident.name);
    printf("ident = %s, prefix = %s\n",root->astnode.ident.name,prefix);
  } 

  /* the top of the stack now contains the array we just created.
   * now issue the store instruction to store the array reference
   * into the static variable.  if this ident is equivalenced, we
   * need to get the name/descriptor from the merged variable.
   */

  name = getMergedName(root);
  desc = getMergedDescriptor(root, returns);
  if(gendebug) {
    if(!name) printf("!name\n");
    if(!desc) printf("!desc\n");
  }
  /* 
   * check to see if this is an array declaration or not. 
   * if so, we must generate the appropriate "new" statement.
   * otherwise, just declare & initialize in one statement. --keith 
   */

  if(root->astnode.ident.arraylist != NULL) {
    fprintf (curfp, "%s%s [] ",prefix, returnstring[returns]);

    if (gendebug)
      printf ("found array %s, calling name_emit\n", returnstring[returns]); 
    name_emit (meth, root); 

    if (returns == Integer)
      fprintf (curfp, "= new int[");
    else if (returns == Float)
      fprintf (curfp, "= new float[");
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

          expr_emit(meth, temp2->astnode.expression.rhs);
          fprintf(curfp," - ");
          expr_emit(meth, temp2->astnode.expression.lhs);
          fprintf(curfp," + 1");

          /* at this point, we've pushed the end and start onto the
           * stack, so now we just subtract start from end and increment
           * by one as described above.
           */
          bc_append(meth, jvm_isub);
          bc_append(meth, jvm_iconst_1);
          bc_append(meth, jvm_iadd);
        }
        else
          expr_emit(meth, temp2);

        /* if this isn't the first iteration, then we must multiply
         * the dimensions to get the total size of the array.
         */
        if(temp2 != root->astnode.ident.arraylist)
          bc_append(meth, jvm_imul);

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

    newarray_emit(meth, root->vartype);

    storeVar(cur_class_file, meth, root->vartype, ainf->is_arg, ainf->class, ainf->name,
      ainf->desc, ainf->localvar, FALSE);
  } else {    /* this is not an array declaration */

    if(!type_lookup(cur_param_table, root->astnode.ident.name))
    {
      if(omitWrappers && !cgPassByRef(root->astnode.ident.name))
        fprintf (curfp, "%s%s ", prefix, returnstring[returns]);
      else
        fprintf (curfp, "%s%s ", prefix, wrapper_returns[returns]);

      if (gendebug)
        printf ("%s\n", returnstring[returns]);

      name_emit (meth, root);

      /* this variable is not declared as a parameter, so
       * initialize it with an initial value depending on
       * its data type.
       */

      if ((returns == String) || (returns == Character))
      {
        print_string_initializer(meth, root);
        fprintf(curfp,";\n");

        if(gendebug) {
          printf("new fieldref:\n");
          printf("\tclass: %s\n", cur_filename);
          printf("\tname:  %s\n", name);
          printf("\tdesc:  %s\n", desc ? desc : "NULL");
        }

        storeVar(cur_class_file, meth, root->vartype, ainf->is_arg, ainf->class, ainf->name,
          ainf->desc, ainf->localvar, FALSE);
      }
      else {
        if(omitWrappers && !cgPassByRef(root->astnode.ident.name)) {
          fprintf(curfp,"= %s;\n", init_vals[returns]);
          bc_append(meth, init_opcodes[returns]);
          storeVar(cur_class_file, meth, root->vartype, ainf->is_arg, ainf->class, ainf->name,
             ainf->desc, ainf->localvar, FALSE);
        }
        else
        {
          c = cp_find_or_insert(cur_class_file,CONSTANT_Class,
                    full_wrappername[returns]);

          bc_append(meth, jvm_new,c);
          bc_append(meth, jvm_dup);

          bc_append(meth, init_opcodes[returns]);

          c = bc_new_methodref(cur_class_file,full_wrappername[returns],
                 "<init>", wrapper_descriptor[returns]);

          bc_append(meth, jvm_invokespecial, c);

          storeVar(cur_class_file, meth, root->vartype, ainf->is_arg, ainf->class, ainf->name,
             ainf->desc, ainf->localvar, FALSE);

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
print_string_initializer(JVM_METHOD *meth, AST *root)
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
     * value found in init_vals.   dup this constant string
     * so that we can always free() later regardless of
     * whether we hit this case or the latter case.
     */

    src_initializer = strdup(init_vals[String]);
  }
  else
  {
    /* check if this is a Fortran character array.  it will have been
     * allocated as a Java String, so don't treat it as an array.
     */

    if((ht->variable->astnode.ident.len == 1) &&
       (ht->variable->astnode.ident.dim == 0) &&
       (ht->variable->astnode.ident.arraylist == NULL) &&
       (ht->variable->astnode.ident.startDim[2] != NULL))
    {
      AST *temp_node, *save_parent;
      int c;

      temp_node = addnode();
      if(!temp_node) {
        fprintf(stderr, "Internal error: Failed to alloc temporary node.\n");
        exit(EXIT_FAILURE);
      }

      save_parent = ht->variable->astnode.ident.startDim[2]->parent;

      ht->variable->astnode.ident.startDim[2]->parent = temp_node;

      temp_node->astnode.expression.rhs = ht->variable->astnode.ident.startDim[2];
      temp_node->astnode.expression.lhs = NULL;
      temp_node->astnode.expression.minus = '+';
      temp_node->nodetype = Unaryop;
      temp_node->vartype = ht->variable->astnode.ident.startDim[2]->vartype;

      fprintf(curfp, "= new String(new char[");

      c = cp_find_or_insert(cur_class_file, CONSTANT_Class, JL_STRING);

      bc_append(meth, jvm_new,c);
      bc_append(meth, jvm_dup);
      expr_emit (meth, ht->variable->astnode.ident.startDim[2]);
      bc_append(meth, jvm_newarray, JVM_T_CHAR);

      c = bc_new_methodref(cur_class_file, JL_STRING, "<init>", CHAR_ARRAY_DESC);

      bc_append(meth, jvm_invokespecial, c);

      fprintf(curfp, "])");

      ht->variable->astnode.ident.startDim[2]->parent = save_parent;

      f2jfree(temp_node, sizeof(AST));

      return;
    }

    /* We know how long this string is supposed to be, so we
     * allocate a blank string with that many characters.  For
     * example, CHARACTER*5 blah is translated to:
     *   String blah = new String("     ");
     * assuming it has not been declared with a DATA statement.
     */

    if(ht->variable->astnode.ident.len < 0) {
      src_initializer = (char *)f2jalloc(5);

      sprintf(src_initializer,"\"  \"");
    }
    else {
      src_initializer = (char *)f2jalloc(ht->variable->astnode.ident.len+4);

      sprintf(src_initializer,"\"%*s\"",ht->variable->astnode.ident.len," ");
    }

  }

  /* we've created the initializer for java source code generation,
   * but for JVM opcode, we do not need the quotes within the string. 
   * here we remove them and create a bytecode initializer. 
   */

  bytecode_initializer = (char *)f2jalloc(strlen(src_initializer) - 1);
  strncpy(bytecode_initializer,src_initializer+1,strlen(src_initializer)-2);
  bytecode_initializer[strlen(src_initializer) - 2] = '\0';

  tempnode = addnode();
  tempnode->token = STRING;
  tempnode->astnode.constant.number = strdup(bytecode_initializer);

  if(omitWrappers && !cgPassByRef(root->astnode.ident.name)) {
    fprintf(curfp,"= new String(%s)", src_initializer);
    invoke_constructor(meth, JL_STRING, tempnode, STR_CONST_DESC);
  }
  else {
    fprintf(curfp,"= new StringW(%s)", src_initializer);
    invoke_constructor(meth, full_wrappername[String], tempnode,
       wrapper_descriptor[String]);
  }

  f2jfree(bytecode_initializer, strlen(bytecode_initializer)+1);
  f2jfree(src_initializer, strlen(src_initializer)+1);
  f2jfree(tempnode, sizeof(AST));
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
data_emit(JVM_METHOD *meth, AST *root)
{
  AST * Dtemp, *Ntemp, *Ctemp;
  HASHNODE *hashtemp;

  /* foreach Data spec... */
  for(Dtemp = root->astnode.label.stmt;Dtemp != NULL;Dtemp = Dtemp->prevstmt) 
  {
    Ctemp = Dtemp->astnode.data.clist;

    /* foreach variable... */    
    for(Ntemp = Dtemp->astnode.data.nlist;Ntemp != NULL;Ntemp=Ntemp->nextstmt) 
    {
      /* check to see if we're looking at an implied do loop */

      if(Ntemp->nodetype == DataImpliedLoop) 
      {
        data_implied_loop_emit(meth, Ntemp, Ctemp);
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
        fprintf(stderr,"Warning: can't handle COMMON varables");
        fprintf(stderr," w/DATA statements.\n");
        continue;
      }

      if((hashtemp->variable->vartype == String) &&
         (hashtemp->variable->astnode.ident.len == 1) &&
         (hashtemp->variable->astnode.ident.dim == 0) &&
         (hashtemp->variable->astnode.ident.arraylist == NULL) &&
         (hashtemp->variable->astnode.ident.startDim[2] != NULL))
      {
        int i, length;

        /* this is a Fortran character array generated as a Java String.
         * copy the original dimension info to the arraylist field and
         * call determine_var_length(), then set it back to NULL before
         * emitting the string initializer.
         */
        hashtemp->variable->astnode.ident.arraylist = 
           hashtemp->variable->astnode.ident.startDim[2];
        length = determine_var_length(hashtemp);
        hashtemp->variable->astnode.ident.arraylist = NULL;

        Ctemp = data_var_emit(meth, Ntemp, Ctemp, hashtemp, length);

        if(Ntemp->astnode.ident.arraylist) {
          /* 
           * if Ntemp is a single element of a character array, e.g.:
           *   DATA ICOL( 1 ), ICOL( 2 ), ICOL( 3 ) / 'C', 'o', 'l'/
           * then the whole thing would have been emitted above in the call
           * to data_var_emit().  So, here we skip the remaining single
           * element references so that we don't try to emit them again.
           */
          for(i=0;i<length-1;i++)
            Ntemp = Ntemp->nextstmt;
        }
      }
      else
        Ctemp = data_var_emit(meth, Ntemp, Ctemp, hashtemp, -1);
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
data_implied_loop_emit(JVM_METHOD *meth, AST * root, AST *Clist)
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
    name_emit(meth, lhs);
    fprintf(curfp, " = ");
    expr_emit(meth, Clist);
    fprintf(curfp, ";\n");
    Clist = Clist->nextstmt;
    bc_gen_array_store_op(meth, jvm_data_types[ht->variable->vartype]);
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
data_var_emit(JVM_METHOD *meth, AST *Ntemp, AST *Ctemp, HASHNODE *hashtemp,
  int java_str_len)
{
  int length, is_array, needs_dec;

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

  if(java_str_len >= 0)
  {
    fprintf(curfp,"public static %s ",
       returnstring[ hashtemp->variable->vartype]);

    if(gendebug)
      printf("VAR STRING going to data_string_emit\n");

    Ctemp = data_string_emit(meth, java_str_len, Ctemp, Ntemp);

    return Ctemp;
  }

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

    fprintf(curfp,"public static %s ", returnstring[ hashtemp->variable->vartype]);

    if(gendebug)
      printf("VAR going to data_array_emit\n");

    Ctemp = data_array_emit(meth, length, Ctemp, Ntemp);
  }
  else 
  {
    if(!needs_dec)
    {
      if(omitWrappers && !cgPassByRef(Ntemp->astnode.ident.name))
        fprintf(curfp,"public static %s ", returnstring[ hashtemp->variable->vartype]);
      else
        fprintf(curfp,"public static %s ", wrapper_returns[ hashtemp->variable->vartype]);

      data_scalar_emit(meth, hashtemp->variable->vartype, Ctemp, Ntemp, needs_dec);
    }
    else 
    {
      fprintf(curfp,"static {\n");
      data_scalar_emit(meth, hashtemp->variable->vartype, Ctemp, Ntemp, needs_dec);
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
 * data_string_emit                                                          *
 *                                                                           *
 * This function generates data statements that are used to initialize       *
 * character arrays, e.g.:                                                   *
 *                                                                           *
 *      CHARACTER          TRANSS( NTRAN )                                   *
 *      DATA TRANSS / 'N', 'T', 'C' /                                        *
 *                                                                           *
 * This is a horrible hack and probably won't work well for most things.     *
 * I think the character handling needs to be totally rewritten.             *
 *                                                                           *
 *****************************************************************************/

AST *
data_string_emit(JVM_METHOD *meth, int length, AST *Ctemp, AST *Ntemp)
{
  unsigned int count, size = 0;
  HASHNODE *ht;
  int i, str_idx;
  struct var_info *ainf;
  char *init_string;

  ainf = get_var_info(Ntemp);

  if(gendebug)
    printf("VAR here we are in data_string_emit, length = %d\n",length);

  ht=type_lookup(cur_type_table, Ntemp->astnode.ident.name);
  if(!ht) {
    fprintf(stderr,"type table may be screwed.  Can't find '%s'.",
            Ntemp->astnode.ident.name);
    exit(EXIT_FAILURE);
  }

  fprintf(curfp,"%s = \"",Ntemp->astnode.ident.name);

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
  
  init_string = (char *)f2jalloc(size+1);

  str_idx = 0;
  init_string[str_idx] = 0;

  for(i=0,count=0;(length==-1)?(Ctemp != NULL):(i< length);i++) {

    if(Ctemp->nodetype == Binaryop) {
      /* just skip this case.  decrement i since we didn't use an entry
       * and it'll be incremented on the next iteration.
       */
      i--;
    }
    else {

      if(Ctemp->token == STRING) {
        init_string[str_idx] = Ctemp->astnode.constant.number[0];
      }
      else {
        init_string[str_idx] = '?';
        fprintf(stderr, "expected a string constant in data statement\n");
      }

      str_idx++;
    }

    if((Ctemp = Ctemp->nextstmt) == NULL)
      break;
  }

  init_string[str_idx] = 0;
 
  fprintf(curfp,"%s\";\n", escape_double_quotes(init_string));

  bc_push_string_const(meth, init_string);

  storeVar(cur_class_file, meth, Ntemp->vartype, ainf->is_arg, ainf->class,
    ainf->name, "Ljava/lang/String;", ainf->localvar, FALSE);

  return Ctemp;
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
data_array_emit(JVM_METHOD *meth, int length, AST *Ctemp, AST *Ntemp)
{
  unsigned int count, size = 0;
  HASHNODE *ht;
  int i;
  struct var_info *ainf;

  ainf = get_var_info(Ntemp);

  if(gendebug)
    printf("VAR here we are in data_array_emit, length = %d\n",length);

  ht=type_lookup(cur_type_table, Ntemp->astnode.ident.name);
  if(!ht) {
    fprintf(stderr,"type table may be screwed.  Can't find '%s'.",
            Ntemp->astnode.ident.name);
    exit(EXIT_FAILURE);
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
  
  bc_push_int_const(meth, size);
  newarray_emit(meth, ht->variable->vartype);

  for(i=0,count=0;(length==-1)?(Ctemp != NULL):(i< length);i++) {

    if(Ctemp->nodetype == Binaryop) 
      count = data_repeat_emit(meth, Ctemp, Ntemp, count);
    else {
      bc_append(meth, jvm_dup);
      bc_push_int_const(meth, count++);

      if(Ctemp->token == STRING) {
        fprintf(curfp,"\"%s\" ", 
           escape_double_quotes(Ctemp->astnode.constant.number));
        invoke_constructor(meth, JL_STRING, Ctemp, STR_CONST_DESC);
      }
      else {
        fprintf(curfp,"%s ", Ctemp->astnode.constant.number);
        pushConst(meth, Ctemp);
      }

      bc_gen_array_store_op(meth, jvm_data_types[ht->variable->vartype]);

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

  storeVar(cur_class_file, meth, Ntemp->vartype, ainf->is_arg, ainf->class, ainf->name,
    ainf->desc, ainf->localvar, FALSE);

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
data_repeat_emit(JVM_METHOD *meth, AST *root, AST *Ntemp, unsigned int idx)
{
  int j, repeat;
  char *ditem;
  BOOL keep_going = FALSE;
  
  if((root->astnode.expression.lhs == NULL) || 
     (root->astnode.expression.rhs == NULL))
  {
    fprintf(stderr,"Bad data statement!\n");
    exit(EXIT_FAILURE);
  }

  if((root->astnode.expression.lhs->nodetype != Constant) || 
     (root->astnode.expression.rhs->nodetype != Constant))
  {
    fprintf(stderr,"Error: Data items must be constants.\n");
    exit(EXIT_FAILURE);
  }

  repeat = atoi(root->astnode.expression.lhs->astnode.constant.number);
  ditem = root->astnode.expression.rhs->astnode.constant.number;

  /* emit the all but the last with a comma.. the last one without */
  for(j=0;j<repeat-1;j++) {
    /* This code checks to see if the value we are putting in the array
     * index matches the type of the array. If the values don't match
     * we must cast the array.
     */
 
    if((Ntemp->vartype != root->astnode.expression.rhs->vartype)||(keep_going)) {
      root->astnode.expression.rhs->token = cast_data_stmt(Ntemp,
        root->astnode.expression.rhs->token);
      root->astnode.expression.rhs->vartype = Ntemp->vartype;
      keep_going = TRUE;    /* Used because the vartype is the same now */
    }

    fprintf(curfp,"%s, ", ditem);
    bc_append(meth, jvm_dup);
    bc_push_int_const(meth, idx++);
    pushConst(meth, root->astnode.expression.rhs);
    bc_gen_array_store_op(meth, jvm_data_types[root->astnode.expression.rhs->vartype]);
  }

  if((Ntemp->vartype != root->astnode.expression.rhs->vartype)||(keep_going)) {
    root->astnode.expression.rhs->token = cast_data_stmt(Ntemp,
      root->astnode.expression.rhs->token);
    root->astnode.expression.rhs->vartype = Ntemp->vartype;
  }

  fprintf(curfp,"%s ", ditem);
  bc_append(meth, jvm_dup);
  bc_push_int_const(meth, idx++);
  pushConst(meth, root->astnode.expression.rhs);
  bc_gen_array_store_op(meth, jvm_data_types[root->astnode.expression.rhs->vartype]);
 
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
data_scalar_emit(JVM_METHOD *meth, enum returntype type, AST *Ctemp, AST *Ntemp, 
  int needs_dec)
{
  int c;

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

    /* determine the length of the string (as declared in the fortran src) */
    if(ht == NULL)
      len = 1;
    else {
      if(Ntemp->astnode.ident.len < 0)
        len = 1;
      else
        len = Ntemp->astnode.ident.len;
    }

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
          escape_double_quotes(Ctemp->astnode.constant.number));

        invoke_constructor(meth, JL_STRING, Ctemp, STR_CONST_DESC);
        c = bc_new_fieldref(cur_class_file,cur_filename,Ntemp->astnode.ident.name,
              field_descriptor[String][0]);
      }
      else {
        fprintf(curfp,"%s = new StringW(\"%*s\");\n",
          Ntemp->astnode.ident.name, len,
          escape_double_quotes(Ctemp->astnode.constant.number));

        invoke_constructor(meth, full_wrappername[type], Ctemp, STR_CONST_DESC);
        c = bc_new_fieldref(cur_class_file,cur_filename,Ntemp->astnode.ident.name,
              wrapped_field_descriptor[String][0]);
      }

      bc_append(meth, jvm_putstatic, c);
    }
    else
    {
      /* assigning to an array element.  first, call expr_emit() which will
       * push a reference to the array & the array index onto the stack.
       * then call invoke_constructor() to push a new string object onto
       * the stack.  finally, emit an array store instruction to store the
       * string into the array element.
       */

      expr_emit(meth, Ntemp);
      fprintf(curfp," = \"%*s\";\n", len, 
         escape_double_quotes(Ctemp->astnode.constant.number));

      invoke_constructor(meth, JL_STRING, Ctemp, STR_CONST_DESC);

      bc_gen_array_store_op(meth, jvm_data_types[Ntemp->vartype]);
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
        fprintf(curfp, "%s = ", Ntemp->astnode.ident.name);
        if(Ntemp->vartype != Ctemp->vartype){
           Ctemp->token = cast_data_stmt(Ntemp, Ctemp->token);  
           Ctemp->vartype = Ntemp->vartype;
        }
        fprintf(curfp, "%s;\n", Ctemp->astnode.constant.number);
        pushConst(meth, Ctemp);
        c = bc_new_fieldref(cur_class_file,cur_filename,Ntemp->astnode.ident.name,
              field_descriptor[type][0]);
      }
      else {
        fprintf(curfp,"%s = new %s(%s);\n",Ntemp->astnode.ident.name,
          wrapper_returns[ type], 
          Ctemp->astnode.constant.number);
        invoke_constructor(meth, full_wrappername[type], Ctemp,
          wrapper_descriptor[type]);
        c = bc_new_fieldref(cur_class_file,cur_filename,Ntemp->astnode.ident.name,
              wrapped_field_descriptor[type][0]);
      }

      bc_append(meth, jvm_putstatic, c);
    }
    else
    {
      /* as above in string case, we are assigning to an array element.
       * the individual elements of an array are never wrapped, so we
       * just push the constant onto the stack and issue an array store
       * instruction.
       */
      expr_emit(meth, Ntemp);
      fprintf(curfp, " = ");
      if(Ntemp->vartype != Ctemp->vartype){
         Ctemp->token = cast_data_stmt(Ntemp, Ctemp->token);  
         Ctemp->vartype = Ntemp->vartype;
      }
      fprintf(curfp,"%s;\n", Ctemp->astnode.constant.number);
      pushConst(meth, Ctemp);
      bc_gen_array_store_op(meth, jvm_data_types[type]);
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
invoke_constructor(JVM_METHOD *meth, char *classname, AST *constant, char *desc)
{
  int c;

  if(gendebug)
    printf("invoke_constructor(): classname = %s, constant = '%s'\n", 
           classname, constant->astnode.constant.number);

  c = cp_find_or_insert(cur_class_file, CONSTANT_Class, classname);

  bc_append(meth, jvm_new,c);
  bc_append(meth, jvm_dup);
  pushConst(meth, constant);

  c = bc_new_methodref(cur_class_file, classname, "<init>", desc);

  bc_append(meth, jvm_invokespecial, c);
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
name_emit (JVM_METHOD *meth, AST * root)
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
      fprintf(stderr,"** string literal (this case should NOT be reached)\n");

  tempname = strdup(root->astnode.ident.name);
  uppercase(tempname);

  if(gendebug)
    if(type_lookup(cur_equiv_table, root->astnode.ident.name))
      printf("EQV %s is equivalenced\n",root->astnode.ident.name);

  /* 
   * If this is not a substring operation and the name is in the 
   * external table, then check to see if it is an intrinsic function
   * instead (e.g. SQRT, ABS, etc).  
   */

  if(root->nodetype != Substring) {
    hashtemp = type_lookup (cur_array_table, root->astnode.ident.name); 
    if((root->astnode.ident.arraylist == NULL)
      && (!type_lookup(cur_external_table, root->astnode.ident.name))) {
        scalar_emit(meth, root, hashtemp);
        return;
    }
    else if(hashtemp || (!hashtemp && (root->astnode.ident.arraylist != NULL)
      && (root->vartype == String))) {
        array_emit(meth, root);
        return;
    }
  }
  
  /* 
   * If the name is in the external table, then check to see if
   * it is an intrinsic function instead (e.g. SQRT, ABS, etc).  
   */
 
  if(type_lookup(cur_external_table, root->astnode.ident.name)
         || type_lookup(function_table, root->astnode.ident.name)
         || find_method(root->astnode.ident.name, descriptor_table))
  {
    hashtemp = type_lookup(cur_type_table, root->astnode.ident.name);
    if(hashtemp)
      root->vartype = hashtemp->variable->vartype;
    external_emit(meth, root);
  }
  else if((type_lookup(function_table, root->astnode.ident.name) == NULL)
         && (find_method(root->astnode.ident.name, descriptor_table) == NULL)
         && (type_lookup(cur_type_table, root->astnode.ident.name) == NULL)
         && (methodscan(intrinsic_toks, tempname) != NULL))
  {
    if(gendebug)
      printf("calling intrinsic emit %s\n", root->astnode.ident.name);
    intrinsic_emit(meth, root);
  } 
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
        if(gendebug) {
          printf("** emit String/char literal!");
          printf("  (should this case be reached?)\n");
        }

        fprintf (curfp, "\"%s\"", 
           escape_double_quotes(root->astnode.constant.number));
        break;
      case INTRINSIC: 
        break;
      case NAME:
      default:
        if (root->nodetype == Substring)
          substring_emit(meth, root);
        else{
          subcall_emit(meth, root);    
        }
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
substring_emit(JVM_METHOD *meth, AST *root)
{
  HASHNODE *hashtemp;

  hashtemp = type_lookup (cur_array_table, root->astnode.ident.name);

  if(hashtemp)
    fprintf(stderr,"WARNING: substring on array element not supported.\n");

  scalar_emit(meth, root, hashtemp);

  if((root->parent->nodetype == Assignment) && 
     (root->parent->astnode.assignment.lhs == root))
  {
    /* in this case we are assigning TO a substring, so we
     * do not want to generate the calls to substring() because
     * we will create a new string and assign it to this variable.
     */

    return;
  }

  if(root->astnode.ident.startDim[0] || root->astnode.ident.endDim[0])
    fprintf(curfp,".substring(");

  return;
}

/*****************************************************************************
 *                                                                           *
 * subcall_emit                                                              *
 *                                                                           *
 * This function emits a function call.  I think this function               *
 * is only called in cases where the function or subroutine is               *
 * not declared external or intrinsic and we dont know what                  *
 * else to do with it.                                                       *
 *                                                                           *
 *****************************************************************************/

void 
subcall_emit(JVM_METHOD *meth, AST *root)
{
  JVM_METHODREF *mref;
  AST *temp;
  char *tempstr, *t;
  char *desc;
  HASHNODE *ht;
  int c;

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
  ht = type_lookup(cur_type_table, root->astnode.ident.name);

  if(gendebug){
    printf("codegen: function return type: %s\n", 
       returnstring[ht->variable->vartype]);
  }

  /* Loop through the argument list and emit each one. */

  fprintf (curfp, "(");
  if(temp->nodetype != EmptyArgList)
    for (; temp != NULL; temp = temp->nextstmt)
    {
      if(temp != root->astnode.ident.arraylist)
        fprintf (curfp, ",");  /* if not first iteration */
                        
      if (*temp->astnode.ident.name != '*')
        expr_emit (meth, temp);
    }

  c = bc_new_methodref(cur_class_file, 
         bc_get_full_classname(tempstr, package_name),
         root->astnode.ident.name, desc);

  bc_append(meth, jvm_invokestatic, c);

  fprintf (curfp, ")");

  bc_free_fieldref(mref);
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
  AST *startIdx;
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
func_array_emit(JVM_METHOD *meth, AST *root, char *arrayname, int is_arg, 
  int is_ext)
{
  int needs_cast;

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
  else
  {
    enum _nodetype save_nodetype = 0;
    AST *tmp;
    int i,j;

    /* hack alert!  what i'm doing here is changing the
     * nodetype of the array dimension expression's parent.
     * the reason being that when we emit the start and
     * end dimensions, if the parent nodetype is ArrayDec
     * then nothing will be emitted for bytecode.  --keith
     * p.s. note that we only need to set this for one
     * dimension since they all share the same parent node.
     */

    if(ht->variable->astnode.ident.endDim[0]) {
      save_nodetype = ht->variable->astnode.ident.endDim[0]->parent->nodetype;
      ht->variable->astnode.ident.endDim[0]->parent->nodetype = Identifier;
    }

    tmp = root;
    for(i=0;i<ht->variable->astnode.ident.dim;i++) {
      AST *start, *end;

      if(tmp != root)
        fprintf(curfp,"+");

      fprintf(curfp,"(");
      expr_emit(meth, tmp);
      if(tmp->vartype != Integer)
        bc_append(meth, typeconv_matrix[tmp->vartype][Integer]);
      fprintf(curfp,"-(");

      start = ht->variable->astnode.ident.startDim[i];

      if(start != NULL) {
        expr_emit(meth, start);
        if(start->vartype != Integer)
          bc_append(meth, typeconv_matrix[start->vartype][Integer]);
      }
      else {
        fprintf(curfp,"1");
        bc_push_int_const(meth, 1);
      }
      fprintf(curfp,"))");
      bc_append(meth, jvm_isub);

      for(j=i-1;j>=0;j--) {
        fprintf(curfp," * ");
        fprintf(curfp,"(");

        start = ht->variable->astnode.ident.startDim[j];
        end = ht->variable->astnode.ident.endDim[j];

        if(start != NULL) {
          expr_emit(meth, end);
          if(end->vartype != Integer)
            bc_append(meth, typeconv_matrix[end->vartype][Integer]);
          fprintf(curfp," - ");
          expr_emit(meth, start);
          if(start->vartype != Integer)
            bc_append(meth, typeconv_matrix[start->vartype][Integer]);
          bc_append(meth, jvm_isub);
          fprintf(curfp," + 1");
          bc_push_int_const(meth, 1);
          bc_append(meth, jvm_iadd);
        }
        else {
          expr_emit(meth, end);
          if(end->vartype != Integer)
            bc_append(meth, typeconv_matrix[end->vartype][Integer]);
        }
        fprintf(curfp,")");
        bc_append(meth, jvm_imul);
      }

      if(tmp != root)
        bc_append(meth, jvm_iadd);
      tmp = tmp->nextstmt;
    }

    if(ht->variable->astnode.ident.endDim[0])
      ht->variable->astnode.ident.endDim[0]->parent->nodetype = save_nodetype;
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

    pushVar(cur_class_file, meth, Integer,is_arg,cur_filename,
            "dummy string...is this significant?",
            "I", varnum , FALSE);
    bc_append(meth, jvm_iadd);
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
      fprintf(stderr, "    node type is: %s\n", print_nodetype(ht->variable));
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
      JVM_METHODREF * mtmp;

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
      else if((mtmp=find_commonblock(name, descriptor_table)) != NULL) {
        char * temp_desc;

        /** TODO: 'pos' was being used here uninitialized, but I can't
         * remember the circumstances that would drop us into this
         * case anyway.  it seems common block variables are always
         * tagged pass-by-ref, so this is never executed (at least
         * compiling all blas, lapack, testers, etc never result in
         * this case being executed).
         *
         * For now, just set pos to 0 and figure it out later.
         **/

        pos = 0;
      
        temp_desc = getFieldDescFromCommonDesc(mtmp->descriptor, pos);
        
        return isPassByRef_desc(temp_desc);
      }
      else {
        return FALSE;
      }
    }
  }
  else if(type_lookup(etable, name)) {
    if(gendebug) {
      printf("isPassByRef(): '%s' not found in type table,", name);
      printf(" but found in external table\n");
    }

    return FALSE;
  }
  else {
    fprintf(stderr,"isPassByRef(): variable %s not found (unit: %s)\n", 
          name, unit_name);

    return TRUE;
  }

  /* should not reach this point */
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
array_emit(JVM_METHOD *meth, AST *root)
{
  AST *temp;
  struct var_info *arrayinf;

  if(gendebug)
    printf ("Array... %s, My node type is %s\n", root->astnode.ident.name,
      print_nodetype(root));

  temp = root->astnode.ident.arraylist;

  if((root->vartype == String) && temp && !temp->nextstmt && 
     !type_lookup(cur_array_table, root->astnode.ident.name))
  {
    int c, charat_ref;

    /* special handling for single dimension string array reference */

    fprintf(curfp, "String.valueOf(");
    arrayinf = push_array_var(meth, root);

    fprintf(curfp, ".charAt((");

    c = bc_new_methodref(cur_class_file, "java/lang/String",
              "valueOf", "(C)Ljava/lang/String;");

    expr_emit(meth, temp);
    bc_append(meth, jvm_iconst_1);
    bc_append(meth, jvm_isub);

    charat_ref = bc_new_methodref(cur_class_file,JL_STRING,
               "charAt", CHARAT_DESC);
    bc_append(meth, jvm_invokevirtual, charat_ref);

    bc_append(cur_method, jvm_invokestatic, c);

    fprintf(curfp, ")-1))");

    return;
  }

  arrayinf = push_array_var(meth, root);

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

    if((root->parent->nodetype == Call)) 
    {
      if(type_lookup(cur_external_table, root->parent->astnode.ident.name) 
       && !type_lookup(cur_args_table,root->parent->astnode.ident.name) )
      {
        func_array_emit(meth, temp, root->astnode.ident.name, 
           arrayinf->is_arg, TRUE);
      }
      else {
        func_array_emit(meth, temp, root->astnode.ident.name, 
           arrayinf->is_arg, FALSE);
        bc_gen_array_load_op(meth, jvm_data_types[root->vartype]);
      }
    }
    else if(((root->parent->nodetype == Assignment) &&
             (root->parent->astnode.assignment.lhs == root)) ||
            (root->parent->nodetype == DataStmt) ||
            (root->parent->nodetype == DataImpliedLoop))
    {
      func_array_emit(meth, temp, root->astnode.ident.name, 
         arrayinf->is_arg, FALSE);
    }
    else if((root->parent->nodetype == Typedec)) 
    {
      /*  Just a declaration, don't emit index. */
      if(gendebug)
        printf("I guess this is just an array declaration\n");
    }
    else {
      func_array_emit(meth, temp, root->astnode.ident.name, 
         arrayinf->is_arg, FALSE);
      bc_gen_array_load_op(meth, jvm_data_types[root->vartype]);
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
push_array_var(JVM_METHOD *meth, AST *root)
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

  /* for typedec, generate no bytecode */
  if((root->parent != NULL) && (root->parent->nodetype == Typedec))
    fprintf (curfp, "%s", ainf->name);
  else {
    char *com_prefix;

    com_prefix = get_common_prefix(root->astnode.ident.name);

    fprintf (curfp, "%s%s", com_prefix, ainf->name);
    pushVar(cur_class_file, meth, root->vartype, ainf->is_arg, ainf->class, ainf->name,
        ainf->desc, ainf->localvar, FALSE);

    f2jfree(com_prefix, strlen(com_prefix)+1);
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
  int is_arg;
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
  name = root->astnode.ident.name;

  if(com_prefix[0] != '\0')
  {
    char *idx;

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

    tmpclass = strdup(com_prefix);
    while( (idx = strchr(tmpclass, '.')) != NULL )
      *idx = '/';
    tmpclass[strlen(tmpclass)-1] = '\0';
  }
  else
    tmpclass = strdup(cur_filename);

  /* if this is an equivalenced variable, find out the merged
   * name that we should use instead.  Equivalenced names are
   * always merged.
   */

  if((ht = type_lookup(cur_equiv_table,root->astnode.ident.name)) != NULL)
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

  f2jfree(com_prefix, strlen(com_prefix)+1);
  f2jfree(tmpclass, strlen(tmpclass)+1);
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
  JVM_METHODREF *mtmp;
  char * idx;

  /* Look up this variable name in the table of COMMON variables */

  ht = type_lookup(cur_common_table, varname);

  if(gendebug)
    printf("in get_common_prefix, name = '%s'\n",varname);

  if(ht) {
    if(gendebug)
      printf("commonblockname = '%s'\n",
        ht->variable->astnode.ident.commonBlockName);

    if((mtmp = find_commonblock(ht->variable->astnode.ident.commonBlockName,
        descriptor_table)) != NULL)
    {
      cprefix = (char *) f2jalloc( strlen(mtmp->classname) + 3);

      sprintf(cprefix,"%s.", mtmp->classname);
    }
    else {
      char * full_prefix = bc_get_full_classname(prefix, package_name);

      cprefix = (char *) f2jalloc(
         strlen(ht->variable->astnode.ident.commonBlockName) +
         strlen(full_prefix) + 3);

      sprintf(cprefix,"%s_%s.", full_prefix,
        ht->variable->astnode.ident.commonBlockName);
    }
  }
  else
    cprefix = strdup("");  /* dup so we can free() later */

  /* convert fully-qualified class name to dotted notation */
  while( (idx = strchr(cprefix, '/')) != NULL )
    *idx = '.';                                                              

  if(gendebug)
    if(cprefix && strlen(cprefix) > 0)
       printf("get_common_prefix returning '%s'\n", cprefix);

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
    return field_descriptor[root->vartype][(root->astnode.ident.dim > 0)];
  else
    return wrapped_field_descriptor[root->vartype]
                                   [(root->astnode.ident.dim > 0)];
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
pushConst(JVM_METHOD *meth, AST *root) {
  switch(root->token) {
    case INTEGER:
      bc_push_int_const(meth, atoi(root->astnode.constant.number));
      break;
    case E_EXPONENTIAL:
    case FLOAT:
      bc_push_float_const(meth, atof(root->astnode.constant.number));
      break;
    case D_EXPONENTIAL:
    case DOUBLE:
      bc_push_double_const(meth, atof(root->astnode.constant.number));
      break;
    case TrUE:   /* dont expect to find booleans anyway, so dont try */
      bc_append(meth, jvm_iconst_1);
      break;
    case FaLSE:
      bc_append(meth, jvm_iconst_0);
      break;
    case STRING:
      bc_push_string_const(meth, root->astnode.constant.number);
      break;
    default:
      break;
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
scalar_emit(JVM_METHOD *meth, AST *root, HASHNODE *hashtemp)
{
  char *com_prefix, *desc, *name, *scalar_class;
  HASHNODE *ht, *isArg, *typenode;

  /* determine descriptor */
  if((typenode = type_lookup(cur_type_table,root->astnode.ident.name))!=NULL)
    desc = getVarDescriptor(typenode->variable);
  else {
    fprintf(stderr,"ERROR: can't find '%s' in hash table\n", 
       root->astnode.ident.name);
    exit(EXIT_FAILURE);
  }

  if(gendebug)
    printf("in scalar_emit, name = %s, desc = %s\n",
      root->astnode.ident.name, desc);

  /* get the name of the common block class file, if applicable */

  com_prefix = get_common_prefix(root->astnode.ident.name);

  name = root->astnode.ident.name;

  isArg = type_lookup(cur_args_table,name);

  if(com_prefix[0] != '\0')
  {
    char *idx;

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

    scalar_class = strdup(com_prefix);
    while( (idx = strchr(scalar_class, '.')) != NULL )
      *idx = '/';
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

  if((ht = type_lookup(cur_equiv_table,root->astnode.ident.name))!=NULL) {
    name = ht->variable->astnode.ident.merged_name;

    if(gendebug)
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
        JVM_METHODREF *user_method;
        char *tempname;

        if(gendebug)
          printf("in scalar_emit CALL, '%s' <- '%s'\n", 
            root->parent->astnode.ident.name,
            name);

        user_method = find_method(root->parent->astnode.ident.name, descriptor_table);

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

        if(((methodscan(intrinsic_toks, tempname) == NULL) || user_method) &&
            (type_lookup(cur_array_table,
               root->parent->astnode.ident.name) == NULL))
        {
          /* parent is not a call to an intrinsic and not an array access */

          if(gendebug)
            printf("did not find %s in intrinsics table\n",
               root->parent->astnode.ident.name);

          fprintf (curfp, "%s%s", com_prefix, name);

          pushVar(cur_class_file, meth,  root->vartype, isArg!=NULL, scalar_class, name, desc,
             typenode->variable->astnode.ident.localvnum, FALSE);
        }
        else
        {
          if(gendebug)
            printf("found %s in intrinsics or array table\n",
               root->parent->astnode.ident.name);

          if(omitWrappers && !cgPassByRef(root->astnode.ident.name)) {
            fprintf (curfp, "%s%s", com_prefix,name);
            pushVar(cur_class_file, meth, root->vartype, isArg!=NULL, scalar_class, name, desc,
               typenode->variable->astnode.ident.localvnum, FALSE);
          }
          else {
            fprintf (curfp, "%s%s.val", com_prefix,name);
            pushVar(cur_class_file, meth, root->vartype, isArg!=NULL, scalar_class, name, desc,
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
         * --not sure why I wrote that nothing needs to be done
         *   for bytecode generation.  fixing that.  --kgs 6/09
         */

        if(omitWrappers && !cgPassByRef(root->astnode.ident.name)) {
          fprintf (curfp, "%s%s", com_prefix, name);
          pushVar(cur_class_file, meth, root->vartype, isArg!=NULL, scalar_class, name, desc,
             typenode->variable->astnode.ident.localvnum, FALSE);
        }
        else {
          fprintf (curfp, "%s%s.val", com_prefix, name);
          pushVar(cur_class_file,  meth, root->vartype, isArg!=NULL, scalar_class, name, desc,
             typenode->variable->astnode.ident.localvnum, TRUE);
        }
      }
      else if(((root->parent->nodetype == Assignment) ||
               (root->parent->nodetype == StmtLabelAssign))
            && (root->parent->astnode.assignment.lhs == root)) {
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
            pushVar(cur_class_file, meth, root->vartype, isArg!=NULL, scalar_class, name, desc,
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
          
          bc_push_int_const(meth, global_sub.val);
        }
        else {
          if(omitWrappers && !cgPassByRef(root->astnode.ident.name)) {
            fprintf (curfp, "%s%s", com_prefix, name);
            pushVar(cur_class_file, meth, root->vartype, isArg!=NULL, scalar_class, name, desc,
               typenode->variable->astnode.ident.localvnum, FALSE);
          }
          else {
            fprintf (curfp, "%s%s.val", com_prefix, name);
            pushVar(cur_class_file,  meth, root->vartype, isArg!=NULL, scalar_class, name, desc,
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
          pushVar(cur_class_file, meth, root->vartype, isArg!=NULL, scalar_class, name, desc,
             typenode->variable->astnode.ident.localvnum, FALSE);
        }
        else if(type_lookup(cur_args_table,root->astnode.ident.name)) {
          fprintf (curfp, "%s%s,_%s_offset", com_prefix, name, name);
          pushVar(cur_class_file, meth, root->vartype, isArg!=NULL, scalar_class, name, desc,
             typenode->variable->astnode.ident.localvnum, FALSE);
          bc_gen_load_op(meth, typenode->variable->astnode.ident.localvnum+1, jvm_Int);
        }
        else {
          fprintf (curfp, "%s%s,0", com_prefix, name);
          pushVar(cur_class_file, meth, root->vartype, isArg!=NULL, scalar_class, name, desc,
             typenode->variable->astnode.ident.localvnum, FALSE);
          bc_append(meth, jvm_iconst_0);
        }
      }
      else if(root->parent->nodetype == Write) {
        if(type_lookup(cur_args_table,root->astnode.ident.name)) {
          fprintf (curfp, "%s%s,_%s_offset", com_prefix, name, name);
          pushVar(cur_class_file, meth, root->vartype, isArg!=NULL, scalar_class, name, desc,
             typenode->variable->astnode.ident.localvnum, FALSE);
          bc_gen_load_op(meth, typenode->variable->astnode.ident.localvnum+1, jvm_Int);
        }
        else {
          fprintf (curfp, "%s%s,0", com_prefix, name);
          pushVar(cur_class_file, meth, root->vartype, isArg!=NULL, scalar_class, name, desc,
             typenode->variable->astnode.ident.localvnum, FALSE);
          bc_append(meth, jvm_iconst_0);
        }
      }
      else if(((root->parent->nodetype == Assignment) ||
               (root->parent->nodetype == StmtLabelAssign))
            && (root->parent->astnode.assignment.lhs == root)) {
        /* LHS of assignment.  do not generate any bytecode. */
        fprintf (curfp, "%s%s", com_prefix, name);
      }
      else {
        fprintf (curfp, "%s%s", com_prefix, name);
        pushVar(cur_class_file, meth, root->vartype, isArg!=NULL, scalar_class, name, desc,
           typenode->variable->astnode.ident.localvnum, FALSE);
      }
    }
  }

  f2jfree(scalar_class, strlen(scalar_class)+1);
  f2jfree(com_prefix, strlen(com_prefix)+1);
}

/*****************************************************************************
 * etime_sub_emit                                                            *
 *                                                                           *
 * This function emits a call to ETIME(), which returns the number of        *
 * seconds of runtime since the start of the process's execution.            *
 *                                                                           *
 * This emits the subroutine call to ETIME().  For the function call version *
 * see etime_func_emit().                                                    *
 *                                                                           *
 *****************************************************************************/

void
etime_sub_emit(JVM_METHOD *meth, METHODTAB *entry, AST *arglist)
{
  AST *assign_temp, *temp;
  int c;

  if(gendebug)
    printf("emitting ETIME subroutine call...\n");

  /* first, make sure there are enough args to work with */
  if((arglist == NULL) && (arglist->nextstmt == NULL)) {
    fprintf(stderr, "ETIME subroutine requires two args.\n");
    exit(EXIT_FAILURE);
  }

  temp = arglist->nextstmt;

  /* make a dummy assignment node so we emit the correct LHS store code */
  assign_temp = addnode();
  assign_temp->nodetype = Assignment;
  temp->parent = assign_temp;
  assign_temp->astnode.assignment.lhs = temp;

  name_emit(meth, assign_temp->astnode.assignment.lhs);

  fprintf(curfp, " = Etime.etime(");
  expr_emit(meth, arglist);
  fprintf(curfp, ");\n");

  c = bc_new_methodref(cur_class_file, entry->class_name,
                    entry->method_name, entry->descriptor);

  bc_append(meth, jvm_invokestatic, c);

  LHS_bytecode_emit(meth, assign_temp);
}

/*****************************************************************************
 * etime_func_emit                                                           *
 *                                                                           *
 * This function emits a call to ETIME(), which returns the number of        *
 * seconds of runtime since the start of the process's execution.            *
 *                                                                           *
 * This emits the function call to ETIME().  For the subroutine call version *
 * see etime_sub_emit().                                                     *
 *                                                                           *
 *****************************************************************************/

void
etime_func_emit(JVM_METHOD *meth, METHODTAB *entry, AST *arglist)
{
  int c;

  /* first, make sure there are enough args to work with */
  if(arglist == NULL) {
    fprintf(stderr,"No args to ETIME\n");
    exit(EXIT_FAILURE);
  }

  if(gendebug)
    printf("emitting ETIME function call...\n");

  fprintf(curfp, "Etime.etime(");
  expr_emit(meth, arglist);
  fprintf(curfp, ")");

  c = bc_new_methodref(cur_class_file, entry->class_name,
                    entry->method_name, entry->descriptor);

  bc_append(meth, jvm_invokestatic, c);
}

/*****************************************************************************
 *                                                                           *
 * external_emit                                                             *
 *                                                                           *
 * This function translates calls to external functions.  First,             *
 * check whether we are translating a call to ETIME or SECOND.               *
 * We have implemented java versions of these pseduo intrinsics.             *
 * If we're not translating  a call to ETIME or SECOND, use the              *
 * function call_emit().  --Keith                                            *
 *                                                                           *
 *****************************************************************************/

void
external_emit(JVM_METHOD *meth, AST *root)
{
  char *tempname, *javaname;
  METHODTAB *entry;
  AST *temp;
  int c;

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
        bc_gen_load_op(meth, ht->variable->astnode.ident.localvnum,  jvm_Object);
      else
        bc_gen_load_op(meth, 0,  jvm_Object);

      fprintf(curfp,"%s", root->astnode.ident.name);
    }
    else {
      int c;
      char *fc;

      fprintf(curfp," new %s() ",tempname);

      fc = bc_get_full_classname(tempname, package_name);

      c = cp_find_or_insert(cur_class_file,CONSTANT_Class, fc);
      bc_append(meth, jvm_new,c);
      bc_append(meth, jvm_dup);

      c = bc_new_methodref(cur_class_file,fc, "<init>", "()V");
      bc_append(meth, jvm_invokespecial, c);
    }

    return;
  }

  tempname = strdup(root->astnode.ident.name);
  uppercase(tempname);

  entry = methodscan(intrinsic_toks, tempname);

  /*  
   *  This block of code is only called if the identifier
   *  absolutely does not have an entry in any table,
   *  and corresponds to a method invocation of
   *  something in the blas or lapack packages.  
   */

  if(entry == NULL)
  {
    if (root->astnode.ident.arraylist != NULL)
      call_emit (meth, root);
    f2jfree(tempname, strlen(tempname)+1);
    return;
  }

  javaname = entry->java_method;

  if(gendebug)
  {
    printf("javaname = %s\n",javaname);
    printf("args = %p\n", (void*)root->astnode.ident.arraylist);
  }

  /* Ensure that the call has arguments */

  if (root->astnode.ident.arraylist != NULL)
  {
    temp = root->astnode.ident.arraylist;

    if(!strcmp(tempname, "ETIME")) {
      etime_func_emit(meth, entry, temp);
    }
    else if(!strcmp(tempname, "SECOND")) {
      if(gendebug)
        printf("emitting SECOND...\n");

      fprintf(curfp, "Second.second()");

      c = bc_new_methodref(cur_class_file, entry->class_name,
                        entry->method_name, entry->descriptor);

      bc_append(meth, jvm_invokestatic, c);
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
intrinsic_emit(JVM_METHOD *meth, AST *root)
{
  AST *temp;
  HASHNODE *ht;
  int c;
  METHODTAB *entry;
  char *tempname, *javaname;
  enum _intrinsics id;

  if(gendebug)
    printf("entering intrinsic_emit\n");

  tempname = strdup(root->astnode.ident.name);
  uppercase(tempname);

  entry = methodscan(intrinsic_toks, tempname);

  if(!entry) {
    fprintf(stderr,"Error: not expecting null entry at this point.\n");
    exit(EXIT_FAILURE);
  }

  /* if strict floating-point is enabled and the intrinsic has a
   * strict version, then use it for generating the call.
   */

  if(strictMath && entry->strict_java_method)
    javaname = entry->strict_java_method;
  else
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
    case ifunc_DFLOAT:
    case ifunc_CMPLX:
      temp = root->astnode.ident.arraylist;

       /* for Java source, we just emit a cast.  */
      fprintf (curfp, "%s(", javaname);
      expr_emit (meth, temp);
      fprintf (curfp, ")");

      /* for bytecode, we emit the appropriate conversion opcode.  */
      if(temp->vartype != root->vartype)
        bc_append(meth, typeconv_matrix[temp->vartype][root->vartype]);

      break;

      /* conversion to integer */
    case ifunc_ICHAR:
      temp = root->astnode.ident.arraylist;
      fprintf (curfp, "%s(", javaname);
      expr_emit (meth, temp);
      fprintf (curfp, ".charAt(0))");

      bc_append(meth, jvm_iconst_0);
      c = bc_new_methodref(cur_class_file,JL_STRING,
             "charAt", CHARAT_DESC);
      bc_append(meth, jvm_invokevirtual, c);
      break;

      /* conversion to character */
    case ifunc_CHAR:
      c = cp_find_or_insert(cur_class_file,CONSTANT_Class,
                JL_CHAR);
      bc_append(meth, jvm_new,c);
      bc_append(meth, jvm_dup);

      temp = root->astnode.ident.arraylist;
      fprintf (curfp, "new Character( %s(", javaname);
      expr_emit (meth, temp);
      fprintf (curfp, ") ).toString()");

      c = bc_new_methodref(cur_class_file,JL_CHAR,
             "<init>", "(C)V");
      bc_append(meth, jvm_invokespecial, c);
      c = bc_new_methodref(cur_class_file, JL_CHAR, "toString", 
             TOSTRING_DESC);
      bc_append(meth, jvm_invokevirtual, c);
      break;

      /* truncation */
    case ifunc_AINT:
    case ifunc_DINT:
      if((root->astnode.ident.arraylist->vartype == Float) &&
         (id==ifunc_AINT))
        aint_intrinsic_emit(meth, root, entry);
      else
        dint_intrinsic_emit(meth, root, entry);
      break;

      /* nearest whole number */
    case ifunc_ANINT:
    case ifunc_DNINT:
      if(root->astnode.ident.arraylist->vartype == Double) {
        entry = &intrinsic_toks[ifunc_DNINT];

        if(strictMath && entry->strict_java_method)
          javaname = entry->strict_java_method;
        else
          javaname = entry->java_method;

        fprintf (curfp, "(double)%s(", javaname);
      }
      else
        fprintf (curfp, "(float)%s(", javaname);

      expr_emit (meth, root->astnode.ident.arraylist);
      fprintf (curfp, ")");

      if(strictMath && entry->strict_class_name)
        c = bc_new_methodref(cur_class_file, entry->strict_class_name, 
                        entry->method_name, entry->descriptor);
      else
        c = bc_new_methodref(cur_class_file, entry->class_name, 
                        entry->method_name, entry->descriptor);

      bc_append(meth, jvm_invokestatic, c);

      if(root->astnode.ident.arraylist->vartype == Double)
        bc_append(meth, jvm_i2d);
      else
        bc_append(meth, jvm_i2f);

      break;

      /* nearest integer */
    case ifunc_NINT:
    case ifunc_IDNINT:
      if(root->astnode.ident.arraylist->vartype == Double)
        entry = &intrinsic_toks[ifunc_IDNINT];

      if(strictMath && entry->strict_java_method)
        javaname = entry->strict_java_method;
      else
        javaname = entry->java_method;

      fprintf (curfp, "%s(", javaname);
      expr_emit (meth, root->astnode.ident.arraylist);
      fprintf (curfp, ")");

      if(strictMath && entry->strict_class_name)
        c = bc_new_methodref(cur_class_file, entry->strict_class_name, 
                        entry->method_name, entry->descriptor);
      else
        c = bc_new_methodref(cur_class_file, entry->class_name, 
                        entry->method_name, entry->descriptor);

      bc_append(meth, jvm_invokestatic, c);

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
      if(strictMath && entry->strict_java_method)
        javaname = entry->strict_java_method;
      else
        javaname = entry->java_method;

      temp = root->astnode.ident.arraylist;

      fprintf (curfp, "%s(", javaname);
      expr_emit (meth, temp);
      fprintf (curfp, ")");

      if(strictMath && entry->strict_class_name)
        c = bc_new_methodref(cur_class_file, entry->strict_class_name, 
                        entry->method_name, entry->descriptor);
      else
        c = bc_new_methodref(cur_class_file, entry->class_name, 
                        entry->method_name, entry->descriptor);

      bc_append(meth, jvm_invokestatic, c);
      break;

      /* remainder */
    case ifunc_MOD:
    case ifunc_AMOD:
    case ifunc_DMOD:
      temp = root->astnode.ident.arraylist;
      fprintf(curfp,"(");
      expr_emit (meth, temp);
      fprintf(curfp,")%%("); 

      if(temp->vartype > root->vartype)
        bc_append(meth, 
          typeconv_matrix[temp->vartype][root->vartype]);
      
      expr_emit (meth, temp->nextstmt);
      fprintf(curfp,")");
      
      if(temp->nextstmt->vartype > root->vartype)
        bc_append(meth, 
          typeconv_matrix[temp->nextstmt->vartype][root->vartype]);
      
      if(root->vartype == Float)
        bc_append(meth, jvm_frem);
      else if(root->vartype == Integer)
        bc_append(meth, jvm_irem);
      else
        bc_append(meth, jvm_drem);

      break;
      
      /* transfer of sign */
    case ifunc_SIGN:
      if(root->vartype == Integer)
        entry = &intrinsic_toks[ifunc_ISIGN];
      else if(root->vartype == Double)
        entry = &intrinsic_toks[ifunc_DSIGN];
    case ifunc_ISIGN:
    case ifunc_DSIGN:
      intrinsic2_call_emit(meth, root,entry, root->vartype);
      break;

      /* positive difference */
    case ifunc_DIM:
      if(root->vartype == Integer)
        entry = &intrinsic_toks[ifunc_IDIM];
      else if(root->vartype == Double)
        entry = &intrinsic_toks[ifunc_DDIM];
    case ifunc_IDIM:
    case ifunc_DDIM:
      intrinsic2_call_emit(meth, root,entry, root->vartype);
      break;
      
      /* double precision product of two reals */
    case ifunc_DPROD:
      temp = root->astnode.ident.arraylist;

      fprintf(curfp, "((double)(");
      expr_emit (meth, temp);
      bc_append(meth, jvm_f2d);
      fprintf(curfp, ") * (double)(");
      expr_emit (meth, temp->nextstmt);
      bc_append(meth, jvm_f2d);
      fprintf(curfp, "))");
      bc_append(meth, jvm_dmul);
      break;

      /* real AMAX0(integer) */
    case ifunc_AMAX0:
      max_intrinsic_emit(meth, root, entry);
      break;

      /* integer MAX1(real) */
    case ifunc_MAX1:
      fprintf(curfp,"(int)(");
      max_intrinsic_emit(meth, root, entry);
      fprintf(curfp,")");
      bc_append(meth, typeconv_matrix[Float][Integer]);
      break;

      /* generic maximum or MAX that returns same type as args */
    case ifunc_MAX:
    case ifunc_MAX0:
    case ifunc_AMAX1:
    case ifunc_DMAX1:
      max_intrinsic_emit(meth, root, entry);
      break;

      /* real AMIN0(integer) */
    case ifunc_AMIN0: 
      min_intrinsic_emit(meth, root, entry);
      break;

      /* integer MIN1(real) */
    case ifunc_MIN1:
      fprintf(curfp,"(int)(");
      min_intrinsic_emit(meth, root, entry);
      fprintf(curfp,")");
      bc_append(meth, typeconv_matrix[Float][Integer]);
      break;

      /* generic minimum or MIN that returns same type as args */
    case ifunc_MIN:
    case ifunc_MIN0:
    case ifunc_AMIN1:
    case ifunc_DMIN1:
      min_intrinsic_emit(meth, root, entry);
      break;
      
      /* length of a character entity */
    case ifunc_LEN:
      temp = root->astnode.ident.arraylist;

      /* the handling of the LEN intrinsic here is really a hack..
       * LEN(x) should return the declared length of x, but if x
       * was passed in as an argument, we may not know the declared
       * length of x at compile-time.  In this case, we just use
       * the length() method at run-time.  That's pretty bad, but
       * the alternative is to create some sort of fortran string
       * class that keeps track of the declared length - definitely
       * a hassle to implement and also makes the API nastier by
       * not allowing the user to pass String constants..  -keith
       */

      if(temp != NULL) {
        if(gendebug)
          printf("ifunc_LEN: looking up arg '%s'\n", temp->astnode.ident.name);

        if((ht=type_lookup(cur_type_table,temp->astnode.ident.name)) != NULL)
        {
          if(ht->variable->astnode.ident.len > 0) {
            if(gendebug)
              printf("LEN(%s) = %d\n",temp->astnode.ident.name,
                ht->variable->astnode.ident.len);

            fprintf(curfp, " Math.max(%d, ", ht->variable->astnode.ident.len);

            bc_push_int_const(meth, ht->variable->astnode.ident.len);
            expr_emit(meth, temp);
            fprintf(curfp, ".length())");

            c = bc_new_methodref(cur_class_file, JL_STRING,
                 "length", STRLEN_DESC);
            bc_append(meth, jvm_invokevirtual, c);

            c = bc_new_methodref(cur_class_file, JL_MATH, 
                 "max", "(II)I");

            bc_append(meth, jvm_invokestatic, c);
          }
          else {
            int c;

            expr_emit(meth, temp);
            fprintf(curfp,".length()");

            c = bc_new_methodref(cur_class_file,JL_STRING,
                 "length", STRLEN_DESC);
            bc_append(meth, jvm_invokevirtual, c);
          }
        }
        else {
          fprintf(curfp, " 1 ");

          bc_append(meth, jvm_iconst_1);

          if(gendebug)
            printf("LEN(%s) = 1 (default, not found in type table)\n",
               temp->astnode.ident.name);
        }
      }
      else
        fprintf(stderr, "Warning: ignoring empty LEN() intrinsic call\n");

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
      /* the java sqrt only supports double, so use that entry for
       * either double or float .
       */
      if((root->vartype == Double) || (root->vartype == Float))
        entry = &intrinsic_toks[ifunc_DSQRT];
      else if(root->vartype == Complex)
        entry = &intrinsic_toks[ifunc_CSQRT];
    case ifunc_DSQRT:
    case ifunc_CSQRT:
      intrinsic_call_emit(meth, root,entry,Double);
      break;

      /* exponential */
    case ifunc_EXP:
      /* the java exp only supports double, so use that entry for
       * either double or float .
       */
      if((root->vartype == Double) || (root->vartype == Float))
        entry = &intrinsic_toks[ifunc_DEXP];
      else if(root->vartype == Complex)
        entry = &intrinsic_toks[ifunc_CEXP];
    case ifunc_DEXP:
    case ifunc_CEXP:
      intrinsic_call_emit(meth, root,entry,Double);
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
      intrinsic_call_emit(meth, root,entry,Double);
      break;

      /* common logarithm */
    case ifunc_LOG10:
      if(root->vartype == Double)
        entry = &intrinsic_toks[ifunc_DLOG10];
      else if(root->vartype == Float)
        entry = &intrinsic_toks[ifunc_ALOG10];
    case ifunc_ALOG10:
    case ifunc_DLOG10:
      intrinsic_call_emit(meth, root, entry, Double);
      break;

      /* sine */
    case ifunc_SIN:
      if(root->vartype == Double)
        entry = &intrinsic_toks[ifunc_DSIN];
      else if(root->vartype == Complex)
        entry = &intrinsic_toks[ifunc_CSIN];
    case ifunc_DSIN:
    case ifunc_CSIN:
      intrinsic_call_emit(meth, root, entry, Double);
      break;

      /* cosine */
    case ifunc_COS:
      if(root->vartype == Double)
        entry = &intrinsic_toks[ifunc_DCOS];
      else if(root->vartype == Complex)
        entry = &intrinsic_toks[ifunc_CCOS];
    case ifunc_DCOS:
    case ifunc_CCOS:
      intrinsic_call_emit(meth, root, entry, Double);
      break;

      /* tangent */
    case ifunc_TAN:
      if(root->vartype == Double)
        entry = &intrinsic_toks[ifunc_DTAN];
    case ifunc_DTAN:
      intrinsic_call_emit(meth, root, entry, Double);
      break;

      /* arcsine */
    case ifunc_ASIN:
      if(root->vartype == Double)
        entry = &intrinsic_toks[ifunc_DASIN];
    case ifunc_DASIN:
      intrinsic_call_emit(meth, root, entry, Double);
      break;

      /* arccosine */
    case ifunc_ACOS:
      if(root->vartype == Double)
        entry = &intrinsic_toks[ifunc_DACOS];
    case ifunc_DACOS:
      intrinsic_call_emit(meth, root, entry, Double);
      break;

      /* arctangent */
    case ifunc_ATAN:
      if(root->vartype == Double)
        entry = &intrinsic_toks[ifunc_DATAN];
    case ifunc_DATAN:
      intrinsic_call_emit(meth, root, entry, Double);
      break;

      /* arctangent (2 arg) */
    case ifunc_ATAN2:
      if(root->vartype == Double)
        entry = &intrinsic_toks[ifunc_DATAN2];
    case ifunc_DATAN2:
      intrinsic2_call_emit(meth, root, entry, Double);
      break;

      /* Hyperbolic sine */
    case ifunc_SINH:
      if(root->vartype == Double)
        entry = &intrinsic_toks[ifunc_DSINH];
    case ifunc_DSINH:
      intrinsic_call_emit(meth, root, entry, Double);
      break;

      /* Hyperbolic cosine */
    case ifunc_COSH:
      if(root->vartype == Double)
        entry = &intrinsic_toks[ifunc_DCOSH];
    case ifunc_DCOSH:
      intrinsic_call_emit(meth, root, entry, Double);
      break;

      /* Hyperbolic tangent */
    case ifunc_TANH:
      if(root->vartype == Double)
        entry = &intrinsic_toks[ifunc_DTANH];
    case ifunc_DTANH:
      intrinsic_call_emit(meth, root, entry, Double);
      break;

    case ifunc_LGE: /* lexically greater than/equal */
    case ifunc_LGT: /* lexically greater than */
    case ifunc_LLE: /* lexically less than/equal */
    case ifunc_LLT: /* lexically less than */
      intrinsic_lexical_compare_emit(meth, root, entry);
      break;

    case ifunc_ETIME:
      etime_func_emit(meth, entry, root->astnode.ident.arraylist);
      break;
    default:
      fprintf(stderr,"WARNING: codegen() unimplemented intrinsic: '%s'\n",
         tempname);
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
intrinsic_lexical_compare_emit(JVM_METHOD *meth, AST *root, METHODTAB *entry)
{
  JVM_CODE_GRAPH_NODE *goto_node, *if_node = NULL;
  AST *temp;
  int c;

  temp = root->astnode.ident.arraylist;
  fprintf(curfp,"((");
  expr_emit(meth, temp);
  fprintf(curfp,").compareTo(");
  expr_emit(meth, temp->nextstmt);

  c = bc_new_methodref(cur_class_file, JL_STRING, "compareTo", COMPARE_DESC);
  bc_append(meth, jvm_invokevirtual, c);

  if(entry->intrinsic == ifunc_LGE) {
    fprintf(curfp,") >= 0 ? true : false)");
    if_node = bc_append(meth, jvm_ifge);
  }
  else if(entry->intrinsic == ifunc_LGT) {
    fprintf(curfp,") > 0 ? true : false)");
    if_node = bc_append(meth, jvm_ifgt);
  }
  else if(entry->intrinsic == ifunc_LLE) {
    fprintf(curfp,") <= 0 ? true : false)");
    if_node = bc_append(meth, jvm_ifle);
  }
  else if(entry->intrinsic == ifunc_LLT) {
    fprintf(curfp,") < 0 ? true : false)");
    if_node = bc_append(meth, jvm_iflt);
  }
  else
    fprintf(stderr,"intrinsic_lexical_compare_emit(): bad tag!\n");

  bc_append(meth, jvm_iconst_0);
  goto_node = bc_append(meth, jvm_goto);
  bc_set_branch_target(if_node, bc_append(meth, jvm_iconst_1));

  /* create a dummy instruction node following the stmts so that
   * we have a branch target for the goto statement.  it'll be
   * removed later.
   */
  bc_set_branch_target(goto_node, bc_append(meth, jvm_xxxunusedxxx));
}


/*****************************************************************************
 *                                                                           *
 * intrinsic0_call_emit                                                      *
 *                                                                           *
 * generates a call to an intrinsic which has no args.                       *
 *                                                                           *
 *****************************************************************************/

void
intrinsic0_call_emit(JVM_METHOD *meth, AST *root, METHODTAB *entry)
{
  int c;

  if(entry->ret != root->vartype)
    fprintf(curfp, "(%s)", returnstring[root->vartype]);

  if(strictMath && entry->strict_java_method)
    fprintf (curfp, "%s()", entry->strict_java_method);
  else
    fprintf (curfp, "%s()", entry->java_method);

  if(strictMath && entry->strict_class_name)
    c = bc_new_methodref(cur_class_file, entry->strict_class_name, 
                    entry->method_name, entry->descriptor);
  else
    c = bc_new_methodref(cur_class_file, entry->class_name, 
                    entry->method_name, entry->descriptor);

  bc_append(meth, jvm_invokestatic, c);

  if(entry->ret != root->vartype)
    bc_append(meth, typeconv_matrix[entry->ret][root->vartype]);
}

/*****************************************************************************
 *                                                                           *
 * intrinsic_call_emit                                                       *
 *                                                                           *
 * generates a call to a single-arg intrinsic.                               *
 *                                                                           *
 *****************************************************************************/

void
intrinsic_call_emit(JVM_METHOD *meth, AST *root, METHODTAB *entry, 
  enum returntype argtype)
{
  int c;

  /* entry->ret should represent the return type of the equivalent JAva
   * function, while root->vartype should represent the return type of
   * the fortran intrinsic.  e.g. fortan's EXP may return Real but JAva's
   * Math.exp() always returns double.  in these cases we must cast.
   */
  if(entry->ret != root->vartype)
    fprintf(curfp, "(%s)", returnstring[root->vartype]);

  if(strictMath && entry->strict_java_method)
    fprintf (curfp, "%s(", entry->strict_java_method);
  else
    fprintf (curfp, "%s(", entry->java_method);

  intrinsic_arg_emit(meth, root->astnode.ident.arraylist, argtype);
  fprintf (curfp, ")");

  if(strictMath && entry->strict_class_name)
    c = bc_new_methodref(cur_class_file,entry->strict_class_name, 
                    entry->method_name, entry->descriptor);
  else
    c = bc_new_methodref(cur_class_file,entry->class_name, 
                    entry->method_name, entry->descriptor);

  bc_append(meth, jvm_invokestatic, c);

  if(entry->ret != root->vartype)
    bc_append(meth, typeconv_matrix[entry->ret][root->vartype]);
}

/*****************************************************************************
 *                                                                           *
 * intrinsic2_call_emit                                                      *
 *                                                                           *
 * generates a call to a two-arg intrinsic.                                  *
 *                                                                           *
 *****************************************************************************/

void
intrinsic2_call_emit(JVM_METHOD *meth, AST *root, METHODTAB *entry, 
  enum returntype argtype)
{
  AST * temp = root->astnode.ident.arraylist;
  int c;

  /* entry->ret should represent the return type of the equivalent JAva
   * function, while root->vartype should represent the return type of
   * the fortran intrinsic.  e.g. fortan's EXP may return Real but JAva's
   * Math.exp() always returns double.  in these cases we must cast.
   */

  if(entry->ret != root->vartype)
    fprintf(curfp, "(%s)", returnstring[root->vartype]);

  if(strictMath && entry->strict_java_method)
    fprintf (curfp, "%s(", entry->strict_java_method);
  else
    fprintf (curfp, "%s(", entry->java_method);
  intrinsic_arg_emit (meth, temp, argtype);
  fprintf (curfp, ",");
  intrinsic_arg_emit (meth, temp->nextstmt, argtype);
  fprintf (curfp, ")");

  if(strictMath && entry->strict_class_name)
    c = bc_new_methodref(cur_class_file, entry->strict_class_name, 
                    entry->method_name, entry->descriptor);
  else
    c = bc_new_methodref(cur_class_file, entry->class_name, 
                    entry->method_name, entry->descriptor);

  bc_append(meth, jvm_invokestatic, c);

  if(entry->ret != root->vartype)
    bc_append(meth, typeconv_matrix[entry->ret][root->vartype]);
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
aint_intrinsic_emit(JVM_METHOD *meth, AST *root, METHODTAB * entry)
{
  if(strictMath && entry->strict_java_method)
    fprintf(curfp,"(float)(%s(",entry->strict_java_method);
  else
    fprintf(curfp,"(float)(%s(",entry->java_method);

  expr_emit(meth, root->astnode.ident.arraylist);

  fprintf(curfp,"))");

  /* convert to integer to truncate, then back to float */
  bc_append(meth, jvm_f2i);
  bc_append(meth, jvm_i2f);
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
dint_intrinsic_emit(JVM_METHOD *meth, AST *root, METHODTAB *entry)
{
  if(strictMath && entry->strict_java_method)
    fprintf(curfp,"(double)(%s(",entry->strict_java_method);
  else
    fprintf(curfp,"(double)(%s(",entry->java_method);
  expr_emit(meth, root->astnode.ident.arraylist);
  fprintf(curfp,"))");

  /* convert to integer to truncate, then back to double */
  bc_append(meth, jvm_d2i);  
  bc_append(meth, jvm_i2d);
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
intrinsic_arg_emit(JVM_METHOD *meth, AST *node, enum returntype this_type)
{
  
  if(gendebug){
    printf("intrinsic_arg_emit, node type = %s, this type = %s\n",
         returnstring[node->vartype], returnstring[this_type]);
  }
 
  if(node->vartype > this_type) {
    fprintf(curfp," (%s)",returnstring[this_type]);
    expr_emit (meth, node);
    bc_append(meth, typeconv_matrix[node->vartype][this_type]);
  }
  else
    expr_emit(meth, node);
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
max_intrinsic_emit(JVM_METHOD *meth, AST *root, METHODTAB *entry)
{
  METHODTAB *tmpentry = entry;
  char *desc = "(DDD)D", *f;

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
  else if(entry->intrinsic==ifunc_MAX0)
    desc = "(III)I";
  else if((entry->intrinsic==ifunc_AMAX1) || (entry->intrinsic==ifunc_MAX1))
    desc = "(FFF)F";
  else if(entry->intrinsic==ifunc_AMAX0)
    desc = "(FFF)F";
  else if(entry->intrinsic==ifunc_DMAX1)
    desc = "(DDD)D";
  else
    fprintf(stderr,"WARNING: bad intrinsic tag in max_intrinsic_emit()\n");

  f = strictMath ? THREEARG_MAX_FUNC_STRICT : THREEARG_MAX_FUNC;

  maxmin_intrinsic_emit(meth, root, tmpentry, f, desc);
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
min_intrinsic_emit(JVM_METHOD *meth, AST *root, METHODTAB *entry)
{
  METHODTAB *tmpentry = entry;
  char *desc = "(DDD)D", *f;

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
  else if(entry->intrinsic==ifunc_MIN0)
    desc = "(III)I";
  else if((entry->intrinsic==ifunc_AMIN1) || (entry->intrinsic==ifunc_MIN1))
    desc = "(FFF)F";
  else if(entry->intrinsic==ifunc_AMIN0)
    desc = "(FFF)F";
  else if(entry->intrinsic==ifunc_DMIN1)
    desc = "(DDD)D";
  else
    fprintf(stderr,"WARNING: bad intrinsic tag in min_intrinsic_emit()\n");

  if(gendebug)
    printf("MIN vartype = %s, %s %s %s\n", returnstring[root->vartype], 
         entry->class_name, entry->method_name, entry->descriptor);

  f = strictMath ? THREEARG_MIN_FUNC_STRICT : THREEARG_MIN_FUNC;

  maxmin_intrinsic_emit(meth, root, tmpentry, f, desc);
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
maxmin_intrinsic_emit(JVM_METHOD *meth, AST *root, METHODTAB *entry,
                      char *threearg, char *three_desc)
{
  int ii, arg_count = 0;
  char *javaname, *method, *util_class;
  int c;
  AST *temp;

  if(strictMath && entry->strict_java_method)
    javaname = entry->strict_java_method;
  else
    javaname = entry->java_method;

  util_class = strictMath ? STRICT_UTIL_CLASS : UTIL_CLASS;

  /* figure out how many args we need to handle */
  for(temp = root->astnode.ident.arraylist;temp!=NULL;temp = temp->nextstmt)
    arg_count++;

  /* If we only have one arg, just emit that expression.  This should not
   * happen since it's invalid to call MAX with only one arg.
   */

  if(arg_count == 1) {
    temp = root->astnode.ident.arraylist;

    fprintf (curfp, "(");
    intrinsic_arg_emit(meth, temp, entry->ret);
    fprintf (curfp, ")");
  }

  /* typical situation, two args */

  else if(arg_count == 2) {
    temp = root->astnode.ident.arraylist;
    fprintf(curfp, "%s(", javaname);
    intrinsic_arg_emit(meth, temp, entry->ret);
    fprintf (curfp, ", ");
    intrinsic_arg_emit(meth, temp->nextstmt, entry->ret);
    fprintf (curfp, ")");
    if(strictMath && entry->strict_class_name)
      c = bc_new_methodref(cur_class_file,entry->strict_class_name, 
                      entry->method_name, entry->descriptor);
    else
      c = bc_new_methodref(cur_class_file,entry->class_name, 
                      entry->method_name, entry->descriptor);

    bc_append(meth, jvm_invokestatic, c);
  }

  /* special handling of situation in which MAX or MIN has three args */

  else if(arg_count == 3) {
    char *ta_tmp;

    temp = root->astnode.ident.arraylist;
    fprintf(curfp, "%s(", threearg);
    intrinsic_arg_emit(meth, temp,entry->ret);
    fprintf (curfp, ", ");
    intrinsic_arg_emit(meth, temp->nextstmt,entry->ret);
    fprintf (curfp, ", ");
    intrinsic_arg_emit(meth, temp->nextstmt->nextstmt,entry->ret);
    fprintf (curfp, ")");

    ta_tmp = strdup(threearg);

    strtok(ta_tmp,".");
    method = strtok(NULL,".");
    c = bc_new_methodref(cur_class_file, util_class, method, three_desc);

    bc_append(meth, jvm_invokestatic, c);

    f2jfree(ta_tmp, strlen(ta_tmp)+1);
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
    char *ta_tmp;

    for(ii=0;ii<arg_count -3;ii++)
      fprintf(curfp,"%s(", javaname);
    fprintf(curfp,"%s(", threearg);

    temp = root->astnode.ident.arraylist;
    intrinsic_arg_emit(meth, temp, entry->ret);
    fprintf (curfp, ", ");
    temp = temp->nextstmt;
    intrinsic_arg_emit(meth, temp, entry->ret);
    fprintf (curfp, ", ");
    temp = temp->nextstmt;
    intrinsic_arg_emit(meth, temp, entry->ret);
    fprintf (curfp, "), ");

    ta_tmp = strdup(threearg);

    strtok(ta_tmp,".");
    method = strtok(NULL,".");
    c = bc_new_methodref(cur_class_file, util_class, method, three_desc);

    bc_append(meth, jvm_invokestatic, c);

    if(strictMath && entry->strict_class_name)
      c = bc_new_methodref(cur_class_file,entry->strict_class_name, 
                      entry->method_name, entry->descriptor);
    else
      c = bc_new_methodref(cur_class_file,entry->class_name, 
                      entry->method_name, entry->descriptor);

    for(temp = temp->nextstmt; temp != NULL; temp = temp->nextstmt) {
      intrinsic_arg_emit(meth, temp, entry->ret);
      if(temp->nextstmt != NULL)
        fprintf (curfp, "), ");
      else
        fprintf (curfp, ") ");
      bc_append(meth, jvm_invokestatic, c);
    }

    f2jfree(ta_tmp, strlen(ta_tmp)+1);
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
  unsigned int idx;
  int contains_dot = FALSE, contains_f = FALSE;

  for(idx = 0;idx < strlen(num);idx++) 
    if(num[idx] == '.')
      contains_dot = TRUE;
    else if(num[idx] == 'f')
      contains_f = TRUE;

  if(contains_dot && contains_f)
    return Float;

  if(contains_dot && !contains_f)
    return Double;

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
expr_emit (JVM_METHOD *meth, AST * root)
{
  if(root == NULL)
  {
    /* We should not have a NULL expression */
    fprintf(stderr,"Warning: NULL root in expr_emit (%s)\n", cur_filename);
    return;
  }

  if(gendebug) {
    printf("expr_emit(): nodetype = %s\n", print_nodetype(root));
    printf("%s\n", root->astnode.ident.name);
    if(root->nodetype == Binaryop)
      printf("\toptype = %c\n",root->astnode.expression.optype);
  }

  switch (root->nodetype)
  {
    case Identifier:
      name_emit (meth, root);
      break;
    case Expression:
      parenthesized_expr_emit(meth, root);
      break;
    case Power:
      fprintf (curfp, "(");
      power_emit(meth, root);
      fprintf (curfp, ")");
      break;
    case Binaryop:
      fprintf (curfp, "(");
      binaryop_emit(meth, root);
      fprintf (curfp, ")");
      break;
    case Unaryop:
      fprintf (curfp, "(");
      unaryop_emit(meth, root);
      fprintf (curfp, ")");
      break;
    case Constant:
      constant_expr_emit(meth, root);
      break;
    case Logicalop:
      fprintf (curfp, "(");
      logicalop_emit(meth, root);
      fprintf (curfp, ")");
      break;
    case Relationalop:
      fprintf (curfp, "(");
      relationalop_emit(meth, root);
      fprintf (curfp, ")");
      break;
    case Substring:
      substring_expr_emit(meth, root);
      break;
    default:
      fprintf(stderr,"Warning: Unknown nodetype in expr_emit(): %s\n",
        print_nodetype(root));
  }

  if(gendebug)printf("leaving-expr emit\n");

  return;
}

/*****************************************************************************
 *                                                                           *
 * parenthesized_expr_emit                                                   *
 *                                                                           *
 * This function handles any expression surrounded by parens - really no     *
 * need to do anything here, just call expr_emit() to emit the expression.   *
 *                                                                           *
 *****************************************************************************/

void
parenthesized_expr_emit(JVM_METHOD *meth, AST *root)
{
  AST *rhs;

  fprintf (curfp, "(");

  rhs = root->astnode.expression.rhs;

  /* is expression.lhs ever really non-null? i dont think so.
   * in any case, for bytecode generation, we are not concerned
   * with parens, so it should be ok to just call expr_emit. (kgs)
   */

  if (root->astnode.expression.lhs != NULL)
    expr_emit (meth, root->astnode.expression.lhs);

  if(rhs->vartype != root->vartype) {
    fprintf(curfp," (%s)", returnstring[root->vartype]);
    expr_emit (meth, root->astnode.expression.rhs);
    bc_append(meth, typeconv_matrix[rhs->vartype][root->vartype]);
  }
  else
    expr_emit (meth, root->astnode.expression.rhs);

  fprintf (curfp, ")");
  
  return;
}

/*****************************************************************************
 *                                                                           *
 * power_emit                                                                *
 *                                                                           *
 * This function generates code for exponential expressions (e.g. x**y).     *
 * We use java.lang.Math.pow().                                              *
 *                                                                           *
 *****************************************************************************/

void
power_emit(JVM_METHOD *meth, AST *root)
{
  int ct;

  /* hack alert: determine whether this expression is used as the size
   *   in an array declaration.  if so, it must be integer, but java's
   *   pow() method returns double.  so we add a cast.  it would probably
   *   be better to detect this elsewhere (e.g. in the code that emits
   *   array declarations).
   */
  BOOL gencast = (root->parent != NULL)
             && (root->parent->nodetype == ArrayDec);
  char pow_cast[32];

  if(gencast)
    sprintf(pow_cast, "(int)");
  else if(root->vartype != Double)
    sprintf(pow_cast, "(%s)", returnstring[root->vartype]);
  else
    sprintf(pow_cast, " ");

  if(strictMath)
    fprintf(curfp, "%sStrictMath.pow(", pow_cast);
  else
    fprintf(curfp, "%sMath.pow(", pow_cast);

  /* the args to pow must be doubles, so cast if necessary */

  expr_emit(meth, root->astnode.expression.lhs);

  if(root->astnode.expression.lhs->vartype != Double)
    bc_append(meth, typeconv_matrix[root->astnode.expression.lhs->vartype][Double]);
  fprintf(curfp, ", ");
  expr_emit(meth, root->astnode.expression.rhs);
  if(root->astnode.expression.rhs->vartype != Double)
    bc_append(meth, typeconv_matrix[root->astnode.expression.rhs->vartype][Double]);
  fprintf(curfp, ")");

  if(strictMath)
    ct = bc_new_methodref(cur_class_file, "java/lang/StrictMath", "pow", "(DD)D");
  else
    ct = bc_new_methodref(cur_class_file, "java/lang/Math", "pow", "(DD)D");

  bc_append(meth, jvm_invokestatic, ct);

  if(gencast)
    bc_append(meth, jvm_d2i);
  else if(root->vartype != Double)
    bc_append(meth, typeconv_matrix[Double][root->vartype]);

  return;
}

/*****************************************************************************
 *                                                                           *
 * binaryop_emit                                                             *
 *                                                                           *
 * This function generates code for binary operations (mul, add, etc).       *
 *                                                                           *
 *****************************************************************************/

void
binaryop_emit(JVM_METHOD *meth, AST *root)
{
  int ct;

  /* handle special case for string concatenation in bytecode..   we
   * must create a new StringBuffer which contains the LHS and append
   * the RHS to the STringBuffer.
   */
  if(root->token == CAT)
  {
    ct = cp_find_or_insert(cur_class_file,CONSTANT_Class,
              STRINGBUFFER);

    bc_append(meth, jvm_new,ct);
    bc_append(meth, jvm_dup);
    expr_emit (meth, root->astnode.expression.lhs);
    if((root->astnode.expression.lhs->vartype != String) &&
       (root->astnode.expression.lhs->vartype != Character) )
    {
      fprintf(stderr,"ERROR:str cat with non-string types unsupported\n");
    }
    ct = bc_new_methodref(cur_class_file,STRINGBUFFER, "<init>", STRBUF_DESC);

    fprintf (curfp, "%c", root->astnode.expression.optype);

    bc_append(meth, jvm_invokespecial, ct);
    expr_emit (meth, root->astnode.expression.rhs);
    if((root->astnode.expression.rhs->vartype != String) &&
       (root->astnode.expression.rhs->vartype != Character) )
    {
      fprintf(stderr,"ERROR:str cat with non-string types unsupported\n");
    }
    ct = bc_new_methodref(cur_class_file,STRINGBUFFER, "append",
                      append_descriptor[String]);
    bc_append(meth, jvm_invokevirtual, ct);
    ct = bc_new_methodref(cur_class_file,STRINGBUFFER, "toString",
                      TOSTRING_DESC);
    bc_append(meth, jvm_invokevirtual, ct);
  }
  else {
    expr_emit (meth, root->astnode.expression.lhs);

    if(root->astnode.expression.lhs->vartype > root->vartype)
      bc_append(meth,
        typeconv_matrix[root->astnode.expression.lhs->vartype]
                       [root->vartype]);

    fprintf (curfp, "%c", root->astnode.expression.optype);
    expr_emit (meth, root->astnode.expression.rhs);

    if(root->astnode.expression.rhs->vartype > root->vartype)
      bc_append(meth, 
        typeconv_matrix[root->astnode.expression.rhs->vartype]
                       [root->vartype]);

    switch(root->astnode.expression.optype) {
      case '+':
        bc_append(meth, add_opcode[root->vartype]);
        break;
      case '-':
        bc_append(meth, sub_opcode[root->vartype]);
        break;
      case '/':
        bc_append(meth, div_opcode[root->vartype]);
        break;
      case '*':
        bc_append(meth, mul_opcode[root->vartype]);
        break;
      default:
        fprintf(stderr,"WARNING: unsupported optype\n");
        break;  /* for ANSI C compliance */
    }
  }

  return;
}

/*****************************************************************************
 *                                                                           *
 * unaryop_emit                                                              *
 *                                                                           *
 * This function emits the code for a unary expression.  I think the only    *
 * unary op we handle here is unary minus.  Unary negation gets handled in   *
 * logicalop_emit().                                                         *
 *                                                                           *
 *****************************************************************************/

void
unaryop_emit(JVM_METHOD *meth, AST *root)
{
  fprintf (curfp, "%c(", root->astnode.expression.minus);
  expr_emit (meth, root->astnode.expression.rhs);
  fprintf (curfp, ")");

  if(root->astnode.expression.minus == '-')
    bc_append(meth, neg_opcode[root->vartype]);

  return;
}

/*****************************************************************************
 *                                                                           *
 * constant_expr_emit                                                        *
 *                                                                           *
 * This function emits the code for a constant expression.                   *
 *                                                                           *
 *****************************************************************************/

void
constant_expr_emit(JVM_METHOD *meth, AST *root)
{
  char *tempname = NULL;

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
      (type_lookup(cur_array_table,root->parent->astnode.ident.name)
          == NULL) &&
      (methodscan(intrinsic_toks, tempname) == NULL))
  {
    if(root->token == STRING) {
      if(omitWrappers) {

        pushConst(meth, root);

        fprintf (curfp, "\"%s\"", 
          escape_double_quotes(root->astnode.constant.number));
      }
      else
      {
        invoke_constructor(meth, full_wrappername[root->vartype], root,
          wrapper_descriptor[root->vartype]);

        fprintf (curfp, "new StringW(\"%s\")",
          escape_double_quotes(root->astnode.constant.number));
      }
    }
    else {     /* non-string constant argument to a function call */
      if(omitWrappers) {
        pushConst(meth, root);

        fprintf (curfp, "%s", root->astnode.constant.number);
      }
      else
      {
        invoke_constructor(meth, full_wrappername[root->vartype], root,
          wrapper_descriptor[root->vartype]);

        fprintf (curfp, "new %s(%s)",
          wrapper_returns[get_type(root->astnode.constant.number)],
          root->astnode.constant.number);
      }
    }
  }
  else  /* this constant is not an argument to a function call */
  {

    pushConst(meth, root);

    if(root->token == STRING)
      fprintf (curfp, "\"%s\"", 
         escape_double_quotes(root->astnode.constant.number));
    else
      fprintf (curfp, "%s", root->astnode.constant.number);
  }

  if(tempname != NULL)
    f2jfree(tempname, strlen(tempname)+1);

  return;
}

/*****************************************************************************
 *                                                                           *
 * logicalop_emit                                                            *
 *                                                                           *
 * This function emits the code for a logical expression (i.e. boolean).     *
 *                                                                           *
 *****************************************************************************/

void
logicalop_emit(JVM_METHOD *meth, AST *root)
{
  JVM_CODE_GRAPH_NODE *if_node1, *if_node2, *goto_node, *next_node;

  switch(root->token) {
    case NOT:
      fprintf (curfp, "!");
      expr_emit (meth, root->astnode.expression.rhs);

      bc_append(meth, jvm_iconst_1);
      bc_append(meth, jvm_ixor);
      break;
    case AND:
      expr_emit (meth, root->astnode.expression.lhs);
      if_node1 = bc_append(meth, jvm_ifeq);

      fprintf (curfp, " && ");

      expr_emit (meth, root->astnode.expression.rhs);
      if_node2 = bc_append(meth, jvm_ifeq);

      bc_append(meth, jvm_iconst_1);
      goto_node = bc_append(meth, jvm_goto);
      next_node = bc_append(meth, jvm_iconst_0);

      bc_set_branch_target(if_node1, next_node);
      bc_set_branch_target(if_node2, next_node);

      next_node = bc_append(meth, jvm_xxxunusedxxx);

      bc_set_branch_target(goto_node, next_node);

      break;
    case OR:
      expr_emit (meth, root->astnode.expression.lhs);
      if_node1 = bc_append(meth, jvm_ifne);

      fprintf (curfp, " || ");

      expr_emit (meth, root->astnode.expression.rhs);
      if_node2 = bc_append(meth, jvm_ifne);

      bc_append(meth, jvm_iconst_0);
      goto_node = bc_append(meth, jvm_goto);
      next_node = bc_append(meth, jvm_iconst_1);

      bc_set_branch_target(if_node1, next_node);
      bc_set_branch_target(if_node2, next_node);

      next_node = bc_append(meth, jvm_xxxunusedxxx);

      bc_set_branch_target(goto_node, next_node);

      break;
  }

  return;
}

/*****************************************************************************
 *                                                                           *
 * relationalop_emit                                                         *
 *                                                                           *
 * This function emits the code for a relational expression (e.g. .lt., .gt. *
 * etc).                                                                     *
 *                                                                           *
 *****************************************************************************/

void
relationalop_emit(JVM_METHOD *meth, AST *root)
{
  int cur_vt;

  cur_vt = MIN(root->astnode.expression.lhs->vartype,
               root->astnode.expression.rhs->vartype);

  if(((root->astnode.expression.lhs->vartype == String) ||
      (root->astnode.expression.lhs->vartype == Character)) &&
     ((root->astnode.expression.rhs->vartype == String) ||
      (root->astnode.expression.rhs->vartype == Character)))
  {
    char *relfunc = NULL;
    int c;

    switch (root->token)
    {
      case rel_eq:
        relfunc = STREQ_FUNC;
        break;
      case rel_ne:
        relfunc = STRNE_FUNC;
        break;
      case rel_lt:
        relfunc = STRLT_FUNC;
        break;
      case rel_le:
        relfunc = STRLE_FUNC;
        break;
      case rel_gt:
        relfunc = STRGT_FUNC;
        break;
      case rel_ge:
        relfunc = STRGE_FUNC;
        break;
      default:
        fprintf(stderr, "internal error: unexpected string relop\n");
        exit(EXIT_FAILURE);
    }

    c = bc_new_methodref(cur_class_file, UTIL_CLASS, relfunc,
               STR_RELOP_DESC);
    fprintf(curfp,"Util.%s(", relfunc);
    expr_emit (meth, root->astnode.expression.lhs);
    fprintf(curfp,", ");
    expr_emit (meth, root->astnode.expression.rhs);
    fprintf(curfp,")");
    bc_append(meth, jvm_invokestatic, c);

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

      expr_emit (meth, root->astnode.expression.lhs);

      if(root->astnode.expression.lhs->vartype > cur_vt) {
        bc_append(meth, 
          typeconv_matrix[root->astnode.expression.lhs->vartype][cur_vt]);
      }

      fprintf (curfp, " == ");

      expr_emit (meth, root->astnode.expression.rhs);

      if(root->astnode.expression.rhs->vartype > cur_vt) {
        bc_append(meth,
          typeconv_matrix[root->astnode.expression.rhs->vartype][cur_vt]);
      }

      break;
    case rel_ne:

      expr_emit (meth, root->astnode.expression.lhs);
      if(root->astnode.expression.lhs->vartype > cur_vt) {
        bc_append(meth,
          typeconv_matrix[root->astnode.expression.lhs->vartype][cur_vt]);
      }
      fprintf (curfp, " != ");
      expr_emit (meth, root->astnode.expression.rhs);
      if(root->astnode.expression.rhs->vartype > cur_vt) {
        bc_append(meth,
          typeconv_matrix[root->astnode.expression.rhs->vartype][cur_vt]);
      }
      break;
    case rel_lt:
      expr_emit (meth, root->astnode.expression.lhs);
      if(root->astnode.expression.lhs->vartype > cur_vt) {
        bc_append(meth,
          typeconv_matrix[root->astnode.expression.lhs->vartype][cur_vt]);
      }
      fprintf (curfp, " < ");
      expr_emit (meth, root->astnode.expression.rhs);
      if(root->astnode.expression.rhs->vartype > cur_vt) {
        bc_append(meth,
          typeconv_matrix[root->astnode.expression.rhs->vartype][cur_vt]);
      }
      break;
    case rel_le:
      expr_emit (meth, root->astnode.expression.lhs);
      if(root->astnode.expression.lhs->vartype > cur_vt) {
        bc_append(meth,
          typeconv_matrix[root->astnode.expression.lhs->vartype][cur_vt]);
      }
      fprintf (curfp, " <= ");
      expr_emit (meth, root->astnode.expression.rhs);
      if(root->astnode.expression.rhs->vartype > cur_vt) {
        bc_append(meth,
          typeconv_matrix[root->astnode.expression.rhs->vartype][cur_vt]);
      }
      break;
    case rel_gt:
      expr_emit (meth, root->astnode.expression.lhs);
      if(root->astnode.expression.lhs->vartype > cur_vt) {
        bc_append(meth,
          typeconv_matrix[root->astnode.expression.lhs->vartype][cur_vt]);
      }
      fprintf (curfp, " > ");
      expr_emit (meth, root->astnode.expression.rhs);
      if(root->astnode.expression.rhs->vartype > cur_vt) {
        bc_append(meth,
          typeconv_matrix[root->astnode.expression.rhs->vartype][cur_vt]);
      }
      break;
    case rel_ge:
      expr_emit (meth, root->astnode.expression.lhs);
      if(root->astnode.expression.lhs->vartype > cur_vt) {
        bc_append(meth,
          typeconv_matrix[root->astnode.expression.lhs->vartype][cur_vt]);
      }
      fprintf (curfp, " >= ");
      expr_emit (meth, root->astnode.expression.rhs);
      if(root->astnode.expression.rhs->vartype > cur_vt) {
        bc_append(meth,
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
      {
        JVM_CODE_GRAPH_NODE *cmp_node, *goto_node, *iconst_node, *next_node;

        /* the only difference between fcmpg and fcmpl is the handling
         * of the NaN value.  for .lt. and .le. we use fcmpg, otherwise
         * use fcmpl.  this mirrors the behavior of javac.
         */
        if((root->token == rel_lt) || (root->token == rel_le))
          bc_append(meth, jvm_fcmpg);
        else
          bc_append(meth, jvm_fcmpl);

        cmp_node = bc_append(meth, dcmp_opcode[root->token]);
        bc_append(meth, jvm_iconst_0);
        goto_node = bc_append(meth, jvm_goto);
        iconst_node = bc_append(meth, jvm_iconst_1);
        bc_set_branch_target(cmp_node, iconst_node);

        /* create a dummy instruction node following the iconst so that
         * we have a branch target for the goto statement.  it'll be
         * removed later.
         */
        next_node = bc_append(meth, jvm_xxxunusedxxx);
        bc_set_branch_target(goto_node, next_node);
      }
      break;
    case Double: 
      {
        JVM_CODE_GRAPH_NODE *cmp_node, *goto_node, *iconst_node, *next_node;

        /* the only difference between dcmpg and dcmpl is the handling
         * of the NaN value.  for .lt. and .le. we use dcmpg, otherwise
         * use dcmpl.  this mirrors the behavior of javac.
         */
        if((root->token == rel_lt) || (root->token == rel_le))
          bc_append(meth, jvm_dcmpg);
        else
          bc_append(meth, jvm_dcmpl);

        cmp_node = bc_append(meth, dcmp_opcode[root->token]);
        bc_append(meth, jvm_iconst_0);
        goto_node = bc_append(meth, jvm_goto);
        iconst_node = bc_append(meth, jvm_iconst_1);
        bc_set_branch_target(cmp_node, iconst_node);

        /* create a dummy instruction node following the iconst so that
         * we have a branch target for the goto statement.  it'll be
         * removed later.
         */
        next_node = bc_append(meth, jvm_xxxunusedxxx);
        bc_set_branch_target(goto_node, next_node);
      }
      break;
    case Integer: 
      {
        JVM_CODE_GRAPH_NODE *cmp_node, *goto_node, *iconst_node, *next_node;

        cmp_node = bc_append(meth, icmp_opcode[root->token]);
        bc_append(meth, jvm_iconst_0);
        goto_node = bc_append(meth, jvm_goto);
        iconst_node = bc_append(meth, jvm_iconst_1);
        bc_set_branch_target(cmp_node, iconst_node);

        /* create a dummy instruction node following the iconst so that
         * we have a branch target for the goto statement.  it'll be
         * removed later.
         */
        next_node = bc_append(meth, jvm_xxxunusedxxx);
        bc_set_branch_target(goto_node, next_node);
      }
      break;
    default:
      fprintf(stderr,"WARNING: hit default, relop .eq.\n");
      break;
  }

  return;
}

/*****************************************************************************
 *                                                                           *
 * emit_default_substring_start                                              *
 *                                                                           *
 * This handles substring operations with an unspecified starting index.     *
 * For example, "str(:10)".  The implicit starting index is 1.               *
 *                                                                           *
 *****************************************************************************/

void
emit_default_substring_start(JVM_METHOD *meth, AST *root)
{
  fprintf(curfp, "1");
  bc_append(meth, jvm_iconst_1);
}

/*****************************************************************************
 *                                                                           *
 * emit_default_substring_end                                                *
 *                                                                           *
 * This handles substring operations with an unspecified ending index.       *
 * For example, "str(5:)".  The implicit ending index is the last character  *
 * of the string.                                                            *
 *                                                                           *
 *****************************************************************************/

void
emit_default_substring_end(JVM_METHOD *meth, AST *root)
{
  int c;
  AST *tmp_parent, *tmp_node;

  /* For a substring operation of the form "str(5:)", here we are trying to
   * emit the implicit end index expression, which would be "str.length()".
   * The problem is that when we pass the root node to scalar_emit() to emit
   * the instruction to load "str" (necessary before the method can be
   * invoked), it can get confused since the parent node type could be
   * something like 'Assignment', so it's thinking that we're looking at the
   * LHS of an assignment and therefore it erroneously omits the load
   * instruction.
   *
   * To get around that, we just fudge things a bit here and duplicate the
   * root node, make a dummy parent node of type 'Write', and set it as the
   * new node's parent.
   */

  tmp_node = clone_ident(root);

  tmp_parent = addnode();
  tmp_parent->nodetype = Write;
  tmp_parent->astnode.io_stmt.arg_list = tmp_node;

  tmp_node->parent = tmp_parent;

  scalar_emit(meth, tmp_node, NULL);

  fprintf(curfp, ".length()");
  c = bc_new_methodref(cur_class_file, JL_STRING,
         "length", STRLEN_DESC);
  bc_append(meth, jvm_invokevirtual, c);

  f2jfree(tmp_node, sizeof(AST));
  f2jfree(tmp_parent, sizeof(AST));
}

/*****************************************************************************
 *                                                                           *
 * substring_expr_emit                                                       *
 *                                                                           *
 * This function emits the code for a substring expression.  I think this    *
 * only handles RHS substring expressions.  Use java.lang.String.substring() *
 *                                                                           *
 *****************************************************************************/

void
substring_expr_emit(JVM_METHOD *meth, AST *root)
{
  int c;

  /* Check if this is a single character substring */
  if((root->astnode.ident.startDim[0] == NULL) &&
     (root->astnode.ident.endDim[0] == NULL) &&
     (root->astnode.ident.startDim[1] != NULL))
  {
    fprintf(curfp, "Util.strCharAt(");    
    name_emit(meth, root);
    fprintf(curfp,",");
    expr_emit(meth, root->astnode.ident.startDim[1]);
    fprintf(curfp,")");

    c = bc_new_methodref(cur_class_file, UTIL_CLASS,
           "strCharAt", STRCHARAT_DESC);
    bc_append(meth, jvm_invokestatic, c);

    return;
  }

  /* Substring operations are handled with java.lang.String.substring */

  name_emit(meth, root);

  fprintf(curfp,"(");
  if(root->astnode.ident.startDim[0])
    expr_emit(meth, root->astnode.ident.startDim[0]);
  else
    emit_default_substring_start(meth, root);
  fprintf(curfp,")-1,");

  bc_append(meth, jvm_iconst_m1);  /* decrement start idx by one */
  bc_append(meth, jvm_iadd);

  if(root->astnode.ident.endDim[0])
    expr_emit(meth, root->astnode.ident.endDim[0]);
  else
    emit_default_substring_end(meth, root);
  fprintf(curfp,")");

  c = bc_new_methodref(cur_class_file,JL_STRING,
           "substring", SUBSTR_DESC);
  bc_append(meth, jvm_invokevirtual, c);

  return;
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

#ifdef _WIN32
  filename = char_substitution(filename, '/', '\\');
#endif

  if(gendebug)
    printf("filename is %s\n",filename);

  if(gendebug)
    printf("## going to open file: '%s'\n", filename);

  if((javafp = bc_fopen_fullpath(filename,"w", output_dir))==NULL) {
    fprintf(stderr,"Cannot open output file '%s'.\n",filename);
    perror("Reason");
    exit(EXIT_FAILURE);
  }

  curfp = javafp;  /* set global pointer to output file */

  /* add import statements if necessary */

  import_stmt[0] = '\0';
  
  if(import_reflection)
    strcat(import_stmt,"import java.lang.reflect.*;\n");

  javaheader(javafp,import_stmt);

  if(genJavadoc)
    emit_javadoc_comments(root);

  if(strictFp)
    fprintf(javafp,"public strictfp class %s {\n\n", classname);
  else
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
constructor(AST *root)
{
  enum returntype returns;
  AST *tempnode;
  char *tempstring;
  HASHNODE *hashtemp;

  if(root->nodetype == Function)
  {
    char *name;

    returns = root->astnode.source.returns;
    name = root->astnode.source.name->astnode.ident.name;

    /* Define the constructor for the class. */

    fprintf (curfp, "\npublic static %s %s (",
      returnstring[returns], name);

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

  for(; tempnode != NULL; tempnode = tempnode->nextstmt)
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
        exit(EXIT_FAILURE);
      }
    }

    /* If this variable is declared external and it is an argument to
     * this program unit, it must be declared as Object in Java.
     */

    if(type_lookup(cur_external_table, tempnode->astnode.ident.name) != NULL)
      returns = OBJECT_TYPE;
    else{
      returns = hashtemp->variable->vartype;
    }

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
}
    
/*****************************************************************************
 *                                                                           *
 * emit_string_length_hack                                                   *
 *                                                                           *
 * emits a hack for setting string args to the proper length.  for example   *
 * if function A passes a character*6 to function B which declares the arg   *
 * as character*1, then we cut off the extra characters.                     *
 *                                                                           *
 *****************************************************************************/

void
emit_string_length_hack(JVM_METHOD *meth, AST *root)
{
  AST *tempnode;
  HASHNODE *hashtemp;

  tempnode = root->astnode.source.args;

  for(; tempnode != NULL; tempnode = tempnode->nextstmt)
  {
    hashtemp = type_lookup(cur_type_table, tempnode->astnode.ident.name);
    if(hashtemp) {
      if(hashtemp->variable->vartype == String) {
        if(hashtemp->variable->astnode.ident.arraylist == NULL) {
          AST *assign_temp, *left, *right, *start, *end;

          if(gendebug) {
            printf("looking at String arg local len = %d\n", 
              hashtemp->variable->astnode.ident.len);
            printf("..so emit arg passing hack\n");
          }

          if(hashtemp->variable->astnode.ident.len > 0) {
            char len_str[256];

            sprintf(len_str, "%d", hashtemp->variable->astnode.ident.len);

            assign_temp = addnode();
            assign_temp->nodetype = Assignment;

            left = clone_ident(hashtemp->variable);
            left->parent = assign_temp;

            right = addnode();
            right->parent = assign_temp;
            right->nodetype = Substring;
            right->token = NAME;
            strcpy(right->astnode.ident.name, left->astnode.ident.name);

            start = addnode();
            start->token = INTEGER;
            start->nodetype = Constant;
            start->astnode.constant.number = strdup("1");
            start->vartype = Integer;

            end = addnode();
            end->token = INTEGER;
            end->nodetype = Constant;
            end->astnode.constant.number = strdup(len_str);
            end->vartype = Integer;

            right->astnode.ident.startDim[0] = start;
            right->astnode.ident.endDim[0] = end;

            assign_temp->astnode.assignment.lhs = left;
            assign_temp->astnode.assignment.rhs = right;

            assign_emit(meth, assign_temp);
            fprintf(curfp, ";\n");
          }
        }
      }
    }
  }
}

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
  /* BOOL skipped; */

  decs = make_dl();
  rest = make_dl();

  classname = strdup(root->astnode.source.name->astnode.ident.name);
  uppercase(classname);

  tempstring = bc_get_full_classname(classname, package_name);
  intfilename = f2jalloc( strlen(tempstring) + 6 );
  strcpy(intfilename, tempstring);
  strcat(intfilename,".java");

  intfp = bc_fopen_fullpath(intfilename,"w", output_dir);
  if(!intfp) {
    perror("Unable to open file");
    exit(EXIT_FAILURE);
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
    /* skipped = FALSE; */

    hashtemp = type_lookup (cur_type_table, tempnode->astnode.ident.name);
    if (hashtemp == NULL)
    {
      fprintf (stderr,"Type table is screwed (codegen.c).\n");
      fprintf (stderr,"  (looked up: %s)\n", tempnode->astnode.ident.name);
      exit(EXIT_FAILURE);
    }

    if(type_lookup(cur_external_table, tempnode->astnode.ident.name) != NULL)
      returns = OBJECT_TYPE;
    else
      returns = hashtemp->variable->vartype;

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
        /* skipped = TRUE; */
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
        decstr = (char *) f2jalloc(45 + (2 * 
          strlen(tempnode->astnode.ident.name)) + strlen(tempstring));
        sprintf(decstr,"%s [] _%s_copy = MatConv.%sTwoDtoOneD(%s);",
          tempstring, tempnode->astnode.ident.name, 
          returnstring[returns], tempnode->astnode.ident.name);

        dl_insert_b(decs, (void *) strdup(decstr));

        /* decstr should already have enough storage for the
         * following string.
         */

        sprintf(decstr,"MatConv.copyOneDintoTwoD(%s,_%s_copy);",
          tempnode->astnode.ident.name, tempnode->astnode.ident.name);

        dl_insert_b(rest, (void *) strdup(decstr));
      }

      if(hashtemp->variable->astnode.ident.dim > 2)
        fprintf(stderr,
           "Cant correctly generate interface with array over 2 dimensions\n");

      fprintf (intfp, "%s ", tempstring);

      for(i = 0; i < hashtemp->variable->astnode.ident.dim; i++ )
        fprintf(intfp,"[]");

      fprintf(intfp, " %s", tempnode->astnode.ident.name);

      if(!noOffset && (hashtemp->variable->astnode.ident.dim == 1)) {
        char * temp2 = (char *) f2jalloc(
           strlen(tempnode->astnode.ident.name) + 9);
                
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
  AST *tempnode, *prev;
  char *tempstring;
  HASHNODE *hashtemp;
  /* BOOL skipped; */

  if (root->nodetype == Function)
    fprintf (intfp, "_retval = ");

  tempstring = strdup(root->astnode.source.name->astnode.ident.name);
  *tempstring = toupper(*tempstring);

  fprintf(intfp,"%s.%s( ", tempstring, 
    root->astnode.source.name->astnode.ident.name);

  prev = NULL;
  tempnode = root->astnode.source.args;

  /* for each argument */
  for (; tempnode != NULL; tempnode = tempnode->nextstmt)
  {
    /* skipped = FALSE; */

    hashtemp = type_lookup (cur_type_table, tempnode->astnode.ident.name);
    if (hashtemp == NULL)
    {
      fprintf (stderr,"Type table is screwed (codegen.c).\n");
      fprintf (stderr,"  (looked up: %s)\n", tempnode->astnode.ident.name);
      exit(EXIT_FAILURE);
    }

    if (hashtemp->variable->astnode.ident.arraylist == NULL) {
      if((prev != NULL) && (prev->astnode.ident.dim > 1) &&
         !strcmp(tempnode->astnode.ident.name,prev->astnode.ident.leaddim))
      {
        /* If this arg follows a 2D array, pass the array's .length as the
         * leading dimension to the numerical routine.
         */

        /* skipped = TRUE; */
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
        char * temp2 = (char *) f2jalloc(
           strlen(tempnode->astnode.ident.name) + 9);
                
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
forloop_emit (JVM_METHOD *meth, AST * root)
{
  char *indexname;

  forloop_bytecode_emit(meth, root);

  /* push this do loop's AST node on the stack */
  dl_insert_b(doloop, root);

  set_bytecode_status(meth, JAVA_ONLY);

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
    expr_emit (meth, root->astnode.forloop.incr);
    fprintf(curfp, ";\n");
  }

   /* print out a label for this for loop */

  /* commented out the forloop label since it is not used anymore.
   * see the comment in goto_emit().  --keith
   *
   *  fprintf(curfp, "forloop%s:\n",
   *    root->astnode.forloop.Label->astnode.constant.number);
   */
   
   /* This block writes out the loop parameters.  */

  fprintf (curfp, "for (");

  assign_emit (meth, root->astnode.forloop.start);

  fprintf(curfp, "; ");

  if(root->astnode.forloop.incr == NULL)
  {

    name_emit(meth, root->astnode.forloop.start->astnode.assignment.lhs);

    fprintf(curfp, " <= ");
    if(gendebug)printf("forloop stop\n");
    expr_emit (meth, root->astnode.forloop.stop);

    fprintf (curfp, "; ");

    name_emit(meth, root->astnode.forloop.start->astnode.assignment.lhs);

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
      int increment=atoi(root->astnode.forloop.incr->astnode.constant.number);
      
      name_emit(meth, root->astnode.forloop.start->astnode.assignment.lhs);
      if(increment > 0)
        fprintf(curfp," <= ");
      else if(increment < 0)
        fprintf(curfp," >= ");
      else {
        fprintf(stderr,"WARNING: Zero increment in do loop\n");
        fprintf(curfp," /* ERR:zero increment..next op incorrect */ <= ");
      }

      if(gendebug)printf("forloop stop\n"); 
      expr_emit (meth, root->astnode.forloop.stop);

      fprintf (curfp, "; ");
      name_emit(meth, root->astnode.forloop.start->astnode.assignment.lhs);
      fprintf (curfp, " += _%s_inc",indexname);
    }
    else {
      fprintf(curfp,"(_%s_inc < 0) ? ",indexname);
      name_emit(meth, root->astnode.forloop.start->astnode.assignment.lhs);
      fprintf(curfp," >= ");
      if(gendebug)printf("forloop stop\n");
      expr_emit (meth, root->astnode.forloop.stop);
      fprintf(curfp," : ");
      name_emit(meth, root->astnode.forloop.start->astnode.assignment.lhs);
      fprintf(curfp," <= ");
      expr_emit (meth, root->astnode.forloop.stop);
      fprintf (curfp, "; ");

      name_emit(meth, root->astnode.forloop.start->astnode.assignment.lhs);
      fprintf (curfp, " += _%s_inc",indexname);
    }
  }

  fprintf (curfp, ") {\n");

  set_bytecode_status(meth, JAVA_AND_JVM);
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
forloop_bytecode_emit(JVM_METHOD *meth, AST *root) 
{
  set_bytecode_status(meth, JVM_ONLY);

  /* emit the initialization assignment for the loop variable */
  assign_emit(meth, root->astnode.forloop.start);

  /* now emit the expression to calculate the number of 
   * iterations that this loop should make and store the result
   * into the next available local variable.
   */
  expr_emit(meth, root->astnode.forloop.iter_expr);
  root->astnode.forloop.localvar = bc_get_next_local(meth, jvm_Int);
  bc_gen_store_op(meth, root->astnode.forloop.localvar, jvm_Int);

  /* goto the end of the loop where we test for completion */
  root->astnode.forloop.goto_node = bc_append(meth, jvm_goto);

  set_bytecode_status(meth, JAVA_AND_JVM);
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
goto_emit (JVM_METHOD *meth, AST * root)
{
  JVM_CODE_GRAPH_NODE *goto_node;

  /* for bytecode, maintain a list of the gotos so that we can come back
   * later and resolve the branch targets.
   */
  goto_node = bc_append(meth, jvm_goto);

  bc_set_integer_branch_label(goto_node, root->astnode.go_to.label);
   
  if(gendebug)
    printf("## setting branch_label of this node to %d\n",
      root->astnode.go_to.label);

  if(label_search(doloop, root->astnode.go_to.label) != NULL)
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
computed_goto_emit(JVM_METHOD *meth, AST *root)
{
  JVM_CODE_GRAPH_NODE *if_node, *goto_node;
  AST *temp;
  unsigned int lvar, count = 1;

  lvar = bc_get_next_local(meth, jvm_Int);

  fprintf(curfp,"{\n");
  fprintf(curfp,"  int _cg_tmp = ");

  if(root->astnode.computed_goto.name->vartype != Integer) {
    fprintf(curfp,"(int)( ");
    expr_emit(meth, root->astnode.computed_goto.name);
    bc_append(meth, typeconv_matrix[root->astnode.computed_goto.name->vartype]
                             [Integer]);
    fprintf(curfp,")");
  }
  else
    expr_emit(meth, root->astnode.computed_goto.name);
  
  bc_gen_store_op(meth, lvar, jvm_Int);
  fprintf(curfp,";\n");

  for(temp=root->astnode.computed_goto.intlist;temp!=NULL;temp=temp->nextstmt)
  {
    if(temp != root->astnode.computed_goto.intlist)
      fprintf(curfp,"else ");
    fprintf(curfp,"if (_cg_tmp == %d) \n", count);
    fprintf(curfp,"  Dummy.go_to(\"%s\",%s);\n", cur_filename, 
      temp->astnode.constant.number);
    bc_gen_load_op(meth, lvar,  jvm_Int);
    bc_push_int_const(meth, count);
    if_node = bc_append(meth, jvm_if_icmpne);

    goto_node = bc_append(meth, jvm_goto);
    bc_set_branch_label(goto_node, temp->astnode.constant.number);

    bc_set_branch_target(if_node, bc_append(meth, jvm_xxxunusedxxx));

    count++;
  }
  fprintf(curfp,"}\n");

  bc_release_local(meth, jvm_Int);
}

/*****************************************************************************
 *                                                                           *
 * assigned_goto_emit                                                        *
 *                                                                           *
 * This function generates code to implement fortran's assigned              *
 * GOTO statement.   we simply use a series of if-else statements            *
 * to implement the assigned goto.                                           *
 *                                                                           *
 *****************************************************************************/

void
assigned_goto_emit(JVM_METHOD *meth, AST *root)
{
  JVM_CODE_GRAPH_NODE *if_node, *goto_node;
  AST *temp;
  unsigned int lvar;
  int i, count;
  char **labels;
  Dlist tmp;

  count = 0;

  /* if this assigned goto has an integer list of possible targets, e.g.:
   *     GOTO x (10, 20, 30)
   * then root->astnode.computed_goto.intlist should be non-null and will
   * contain a list of AST nodes.
   *
   * if there is no list of targets, e.g.:
   *     GOTO x
   * then we fall back on the list of all possible targets created during
   * parsing.
   *
   * Since these lists are stored in different data structures, we will
   * just convert them to an array of strings here so that we can just
   * write one loop to do the code generation.
   */

  if(root->astnode.computed_goto.intlist) {
    for(temp=root->astnode.computed_goto.intlist;temp!=NULL;temp=temp->nextstmt)
      count++;
  }
  else {
    dl_traverse (tmp, cur_assign_list)
      count++;
  }

  if(count == 0) {
    fprintf(stderr, "Warning: didn't expect empty list of statement labels\n");
    return;
  }

  labels = (char **) f2jalloc(count * sizeof(char *));

  i = 0;

  if(root->astnode.computed_goto.intlist) {
    for(temp=root->astnode.computed_goto.intlist;temp!=NULL;temp=temp->nextstmt)
      labels[i++] = temp->astnode.constant.number;
  }
  else {
    dl_traverse (tmp, cur_assign_list)
      labels[i++] = ((AST *)dl_val(tmp))->astnode.constant.number;
  }

  /* Now the array of integer targets has been built. */

  lvar = bc_get_next_local(meth, jvm_Int);

  fprintf(curfp,"{\n");
  fprintf(curfp,"  int _cg_tmp = ");

  expr_emit(meth, root->astnode.computed_goto.name);

  bc_gen_store_op(meth, lvar, jvm_Int);
  fprintf(curfp,";\n");

  for(i=0;i<count;i++)
  {
    if(i != 0)
      fprintf(curfp,"else ");
    fprintf(curfp,"if (_cg_tmp == %s) \n", labels[i]);
    fprintf(curfp,"  Dummy.go_to(\"%s\",%s);\n", cur_filename, labels[i]);
    bc_gen_load_op(meth, lvar,  jvm_Int);
    bc_push_int_const(meth, atoi(labels[i]));
    if_node = bc_append(meth, jvm_if_icmpne);

    goto_node = bc_append(meth, jvm_goto);
    bc_set_branch_label(goto_node, labels[i]);

    bc_set_branch_target(if_node, bc_append(meth, jvm_xxxunusedxxx));
  }
  fprintf(curfp,"}\n");

  bc_release_local(meth, jvm_Int);
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
logicalif_emit(JVM_METHOD *meth, AST * root)
{
  JVM_CODE_GRAPH_NODE *if_node, *next_node;

  fprintf (curfp, "if (");

  if (root->astnode.logicalif.conds != NULL)
    expr_emit (meth, root->astnode.logicalif.conds);

  if_node = bc_append(meth, jvm_ifeq);

  fprintf (curfp, ") {\n    ");

  emit (root->astnode.logicalif.stmts);

  fprintf (curfp, "}\n    ");

  /* create a dummy instruction node following the stmts so that
   * we have a branch target for the goto statement.  it'll be
   * removed later.
   */
  next_node = bc_append(meth, jvm_xxxunusedxxx);
  bc_set_branch_target(if_node, next_node);
}

/*****************************************************************************
 *                                                                           *
 * arithmeticif_emit                                                         *
 *                                                                           *
 * This function generates code for arithmetic IF statements.                *
 *                                                                           *
 *****************************************************************************/

void
arithmeticif_emit (JVM_METHOD *meth, AST * root)
{
  JVM_CODE_GRAPH_NODE *if_node, *goto_node;
  unsigned int lvar;

  lvar = bc_get_next_local(meth, 
     jvm_data_types[root->astnode.arithmeticif.cond->vartype]);

  fprintf (curfp, "{\n");
  fprintf (curfp, "  %s _arif_tmp = ", 
     returnstring[root->astnode.arithmeticif.cond->vartype]);
  expr_emit(meth, root->astnode.arithmeticif.cond);
  bc_gen_store_op(meth, lvar, 
     jvm_data_types[root->astnode.arithmeticif.cond->vartype]);

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
    bc_gen_load_op(meth, lvar,  jvm_Int);
    if_node = bc_append(meth, jvm_ifge);

    goto_node = bc_append(meth, jvm_goto);
    bc_set_integer_branch_label(goto_node, 
       root->astnode.arithmeticif.neg_label);

    bc_set_branch_target(if_node, bc_gen_load_op(meth, lvar,  jvm_Int));
  }
  else {
    bc_gen_load_op(meth, lvar, jvm_data_types[root->astnode.arithmeticif.cond->vartype]);
    bc_append(meth, init_opcodes[root->astnode.arithmeticif.cond->vartype]);
    bc_append(meth, cmpg_opcode[root->astnode.arithmeticif.cond->vartype]);
    if_node = bc_append(meth, jvm_ifge);

    goto_node = bc_append(meth, jvm_goto);
    bc_set_integer_branch_label(goto_node, 
      root->astnode.arithmeticif.neg_label);

    bc_set_branch_target(if_node, 
       bc_gen_load_op(meth, lvar, jvm_data_types[root->astnode.arithmeticif.cond->vartype]));
    bc_append(meth, init_opcodes[root->astnode.arithmeticif.cond->vartype]);
    bc_append(meth, cmpg_opcode[root->astnode.arithmeticif.cond->vartype]);
  }

  if_node = bc_append(meth, jvm_ifne);

  goto_node = bc_append(meth, jvm_goto);
  bc_set_integer_branch_label(goto_node,root->astnode.arithmeticif.zero_label);

  goto_node = bc_append(meth, jvm_goto);
  bc_set_integer_branch_label(goto_node, root->astnode.arithmeticif.pos_label);

  bc_set_branch_target(if_node, goto_node);

  bc_release_local(meth, jvm_data_types[root->astnode.arithmeticif.cond->vartype]);
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
label_emit (JVM_METHOD *meth, AST * root)
{
  AST *loop;
  int num;

  num = root->astnode.label.number;

  if(gendebug)
    printf("looking at label %d\n", num);

  /* skip CONTINUE statements with no label */
  if(num < 0)
    return;

  root->astnode.label.instr = bc_append(meth, jvm_xxxunusedxxx);

  /* if this continue statement corresponds with the most
   * recent DO loop, then this is the end of the loop - pop
   * the label off the doloop list.
   */
  loop = dl_astnode_examine(doloop);

  if((loop != NULL) &&
     (atoi(loop->astnode.forloop.Label->astnode.constant.number) == num))
  {
    int first = 1;

    do {
      /*
       * finally pop this loop's label number off the stack and
       * emit the label (for experimental goto resolution)
       */

      fprintf(curfp,"Dummy.label(\"%s\",%d);\n",cur_filename,num);

      dl_pop(doloop);
 
      /* the "first" check here is to avoid emitting duplicate
       * statements when the do loop's target is not a CONTINUE.
       */
      if(first && (root->astnode.label.stmt != NULL) &&
         (root->astnode.label.stmt->nodetype != Format))
        emit(root->astnode.label.stmt);

      fprintf(curfp, "}              //  Close for() loop. \n");
      fprintf(curfp, "}\n");

      forloop_end_bytecode(meth, loop);

      loop = dl_astnode_examine(doloop);
      first = 0;
    } while((loop != NULL) &&
       (atoi(loop->astnode.forloop.Label->astnode.constant.number) == num));
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

  bc_associate_integer_branch_label(meth, root->astnode.label.instr, 
     root->astnode.label.number);
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
forloop_end_bytecode(JVM_METHOD *meth, AST *root)
{
  JVM_CODE_GRAPH_NODE *if_node, *iload_node;
  unsigned int icount;
   
  icount = root->astnode.forloop.localvar;

  set_bytecode_status(meth, JVM_ONLY);

  /* increment loop variable */
  assign_emit(meth, root->astnode.forloop.incr_expr);

  /* decrement iteration count */
  bc_gen_iinc(meth, icount, -1);

  iload_node = bc_gen_load_op(meth, icount, jvm_Int);

  bc_set_branch_target(root->astnode.forloop.goto_node, iload_node);

  if_node = bc_append(meth, jvm_ifgt);
  bc_set_branch_target(if_node, 
     bc_get_next_instr(root->astnode.forloop.goto_node));

  bc_release_local(meth, jvm_Int);

  set_bytecode_status(meth, JAVA_AND_JVM);
}

/*****************************************************************************
 *                                                                           *
 * read_emit                                                                 *
 *                                                                           *
 * Emit a READ statement.  Calls formatted_read_emit() or                    *
 * unformatted_read_emit(), depending on whether there is a                  *
 * corresponding FORMAT statement.                                           *
 *                                                                           *
 *****************************************************************************/

void
read_emit(JVM_METHOD *meth, AST * root)
{
  char *fmt_str, tmp[100];
  HASHNODE *hnode;

  /* look for a format statement */
  sprintf(tmp,"%d", root->astnode.io_stmt.format_num);
  if(gendebug)
    printf("***Looking for format statement number: %s\n",tmp);

  hnode = format_lookup(cur_format_table,tmp);

  if(hnode)
    fmt_str = format2str(hnode->variable->astnode.label.stmt);
  else if(root->astnode.io_stmt.fmt_list != NULL)
    fmt_str = strdup(root->astnode.io_stmt.fmt_list->astnode.constant.number);
  else
    fmt_str = NULL;

  if(fmt_str)
    formatted_read_emit(meth, root, fmt_str);
  else
    unformatted_read_emit(meth, root);
}

/*****************************************************************************
 *                                                                           *
 * emit_unit_desc                                                            *
 *                                                                           *
 * This function generates the unit descriptor expression for READ           *
 * statements.  If no unit descriptor was specified, then we just            *
 * emit the default F77 descriptor for stdin.                                *
 *                                                                           *
 *****************************************************************************/

void
emit_unit_desc(JVM_METHOD *meth, AST *root)
{
  if(root->astnode.io_stmt.unit_desc &&
     root->astnode.io_stmt.unit_desc->token != STAR)
  {
    expr_emit(meth, root->astnode.io_stmt.unit_desc);
  }
  else {
    fprintf(curfp, "%d", F77_STDIN);
    bc_push_int_const(meth, F77_STDIN);
  }
}

/*****************************************************************************
 *                                                                           *
 * emit_skip_line                                                            *
 *                                                                           *
 * When we have a READ statement with no arguments, this code will get the   *
 * whole line and throw it away.                                             *
 *                                                                           *
 *****************************************************************************/

void
emit_skip_line(JVM_METHOD *meth, AST *root)
{
  int c;

  bc_gen_load_op(meth, stdin_lvar, jvm_Object);
  fprintf(curfp, "%s.readString(", F2J_STDIN);
  emit_unit_desc(meth, root);
  fprintf(curfp, ");  // skip a line\n");
  c = bc_new_methodref(cur_class_file, EASYIN_CLASS, "readString",
        "(I)Ljava/lang/String;");
  bc_append(meth, jvm_invokevirtual, c);

  /* we don't use the return value, so just pop it off the stack */
  bc_append(meth, jvm_pop);
}

/*****************************************************************************
 *                                                                           *
 * unformatted_read_emit                                                     *
 *                                                                           *
 * This function generates unformatted READ statements.  We generate calls   *
 * to a Java class called EasyIn to perform the I/O.  Also emit a try-catch  *
 * to trap IOExceptions.                                                     *
 *                                                                           *
 *****************************************************************************/

void
unformatted_read_emit(JVM_METHOD *meth, AST * root)
{
  JVM_CODE_GRAPH_NODE *goto_node1, *goto_node2, *try_start, *pop_node;
  JVM_EXCEPTION_TABLE_ENTRY *et_entry;
  AST *assign_temp;
  AST *temp;
  int c;

  try_start = NULL;

  /* if the READ statement has no args, just read a line and
   * ignore it.
   */

  if(root->astnode.io_stmt.arg_list == NULL) {
    emit_skip_line(meth, root);
    return;
  }

  /* if the READ statement includes an END or ERR specifier, then we
   * use a try block to determine EOF and/or error.  the catch block(s),
   * emitted below, just branch to the target(s) of the END and/or ERR. 
   */

  if((root->astnode.io_stmt.end_num > 0) ||
     (root->astnode.io_stmt.err > 0))
  {
    fprintf(curfp,"try {\n");
    funcname = input_func_eof;
    try_start = bc_append(meth, jvm_xxxunusedxxx);
  }
  else
    funcname = input_func;

  assign_temp = addnode();
  assign_temp->nodetype = Assignment;

  for(temp=root->astnode.io_stmt.arg_list;temp!=NULL;temp=temp->nextstmt)
  {
    if(temp->nodetype == IoImpliedLoop)
      implied_loop_emit(meth, temp, read_implied_loop_bytecode_emit,
             read_implied_loop_sourcecode_emit);
    else if(temp->nodetype == Identifier)
    {
      temp->parent = assign_temp;
      assign_temp->astnode.assignment.lhs = temp;

      name_emit(meth, assign_temp->astnode.assignment.lhs);

      bc_gen_load_op(meth, stdin_lvar, jvm_Object);
      if( (temp->vartype == Character) || (temp->vartype == String) ) {
        int len;

        len = temp->astnode.ident.len < 0 ? 1 : temp->astnode.ident.len;

        fprintf(curfp," = %s.%s(", F2J_STDIN, funcname[temp->vartype]);
        emit_unit_desc(meth, root);
        fprintf(curfp,", %d);\n", len);
        bc_push_int_const(meth, len);
      }
      else {
        fprintf(curfp," = %s.%s(", F2J_STDIN, funcname[temp->vartype]);
        emit_unit_desc(meth, root);
        fprintf(curfp,");\n");
      }

      c = bc_new_methodref(cur_class_file, EASYIN_CLASS, funcname[temp->vartype],
            input_descriptors[temp->vartype]);
      bc_append(meth, jvm_invokevirtual, c);

      LHS_bytecode_emit(meth, assign_temp);
    }
    else
    {
      fprintf(stderr,"Read list must consist of idents or implied loops\n");
      fprintf(stderr,"   nodetype is %s\n", print_nodetype(temp));
      continue;
    }
  }

  free_ast_node(assign_temp);

  fprintf(curfp,"%s.skipRemaining();\n", F2J_STDIN);
  bc_gen_load_op(meth, stdin_lvar, jvm_Object);
  c = bc_new_methodref(cur_class_file, EASYIN_CLASS, "skipRemaining", "()V");
  bc_append(meth, jvm_invokevirtual, c);

  /* last statement before catch block(s).  if IOSTAT was specified, then
   * set that variable to 0 here (signifying no error).
   */
  if(root->astnode.io_stmt.iostat) {
    name_emit(meth, root->astnode.io_stmt.iostat);
    fprintf(curfp, " = 0;\n");
    bc_append(meth, jvm_iconst_0);
    LHS_bytecode_emit(meth, root->astnode.io_stmt.iostat->parent);
  }

  if((root->astnode.io_stmt.end_num > 0) ||
     (root->astnode.io_stmt.err > 0))
    fprintf(curfp,"}\n");

  /* Emit the catch block for when we hit EOF */

  if(root->astnode.io_stmt.end_num > 0)
  {
    fprintf(curfp,"catch (java.io.EOFException e) {\n");
    if(root->astnode.io_stmt.iostat) {
      name_emit(meth, root->astnode.io_stmt.iostat);
      fprintf(curfp, " = -1;\n");
    }
    fprintf(curfp,"Dummy.go_to(\"%s\",%d);\n",cur_filename,
      root->astnode.io_stmt.end_num);
    fprintf(curfp,"}\n");

    goto_node1 = bc_append(meth, jvm_goto);  /* skip the exception handler */

    /* following is the exception handler for EOFException.  this
     * implements Fortrans END specifier (eg READ(*,*,END=100)).
     * the exception handler just consists of a pop to get the stack
     * back to normal and a goto to branch to the label specified
     * in the END spec.
     */
    pop_node = bc_append(meth, jvm_pop);

    /* artificially set stack depth at beginning of exception
     * handler to 1.
     */
    bc_set_stack_depth(pop_node, 1);

    if(root->astnode.io_stmt.iostat) {
      bc_append(meth, jvm_iconst_m1);
      LHS_bytecode_emit(meth, root->astnode.io_stmt.iostat->parent);
    }

    goto_node2 = bc_append(meth, jvm_goto);
    bc_set_integer_branch_label(goto_node2, root->astnode.io_stmt.end_num);

    bc_set_branch_target(goto_node1, bc_append(meth, jvm_xxxunusedxxx));

    et_entry = bc_new_exception_table_entry(meth, try_start, pop_node,
      pop_node, EOFEXCEPTION);
    bc_add_exception_handler(meth, et_entry);
  }

  /* Emit the catch block for when we hit a read error */

  if(root->astnode.io_stmt.err > 0)
  {
    fprintf(curfp,"catch (java.lang.Exception e) {\n");
    if(root->astnode.io_stmt.iostat) {
      name_emit(meth, root->astnode.io_stmt.iostat);
      fprintf(curfp, " = 1;\n");
    }
    fprintf(curfp,"Dummy.go_to(\"%s\",%d);\n",cur_filename,
      root->astnode.io_stmt.err);
    fprintf(curfp,"}\n");

    goto_node1 = bc_append(meth, jvm_goto);  /* skip the exception handler */

    /* following is the exception handler for IOException.  this
     * implements Fortrans ERR specifier (eg READ(*,*,ERR=100)).
     * the exception handler just consists of a pop to get the stack
     * back to normal and a goto to branch to the label specified
     * in the ERR spec.
     */
    pop_node = bc_append(meth, jvm_pop);

    /* artificially set stack depth at beginning of exception
     * handler to 1.
     */
    bc_set_stack_depth(pop_node, 1);

    if(root->astnode.io_stmt.iostat) {
      bc_append(meth, jvm_iconst_1);
      LHS_bytecode_emit(meth, root->astnode.io_stmt.iostat->parent);
    }

    goto_node2 = bc_append(meth, jvm_goto);
    bc_set_integer_branch_label(goto_node2, root->astnode.io_stmt.err);

    bc_set_branch_target(goto_node1, bc_append(meth, jvm_xxxunusedxxx));

    et_entry = bc_new_exception_table_entry(meth, try_start, pop_node,
      pop_node, JLEXCEPTION);
    bc_add_exception_handler(meth, et_entry);
  }
}

/*****************************************************************************
 *                                                                           *
 * formatted_read_assign_emit                                                *
 *                                                                           *
 * Emits the assignment statement of an implied loop in a READ statement.    *
 * If emit_source is TRUE, emits both bytecode and source code.              *
 *                                                                           *
 *****************************************************************************/

void
formatted_read_assign_emit(JVM_METHOD *meth, AST *temp,
  int emit_source, int idx)
{
  AST *assign_temp, *idx_temp = NULL;
  int c;

  assign_temp = addnode();
  assign_temp->nodetype = Assignment;

  if(idx >= 0) {
    idx_temp = addnode();
    idx_temp->token = INTEGER;
    idx_temp->nodetype = Constant;
    idx_temp->astnode.constant.number = (char *)malloc(MAX_CONST_LEN);
    if(!idx_temp->astnode.constant.number) {
      fprintf(stderr, "malloc failed in formatted_read_assign_emit()\n");
      exit(EXIT_FAILURE);
    }

    sprintf(idx_temp->astnode.constant.number, "%d", idx);
    idx_temp->vartype = Integer;
    idx_temp->nextstmt = NULL;
    temp->astnode.ident.arraylist = idx_temp;
  }

  temp->parent = assign_temp;
  assign_temp->astnode.assignment.lhs = temp;

  name_emit(meth, assign_temp->astnode.assignment.lhs);

  bc_gen_load_op(meth, iovec_lvar, jvm_Object);
  bc_append(meth, jvm_iconst_0);
  if((temp->vartype == Character) || (temp->vartype == String)) {
    if(temp->astnode.ident.len > 0)
      bc_push_int_const(meth, temp->astnode.ident.len);
    else
      bc_append(meth, jvm_iconst_0);

    c = bc_new_methodref(cur_class_file, IOVECTOR_CLASS, "remove",
            VEC_REMOVE_PAD_DESC);
  }
  else
    c = bc_new_methodref(cur_class_file, IOVECTOR_CLASS, "remove",
            VEC_REMOVE_DESC);

  bc_append(meth, jvm_invokevirtual, c);

  if((temp->vartype == Character) || (temp->vartype == String)) {
    /* special case for string since we don't need to call any method
     * to get the value as with other primitive types (e.g. intValue,
     * doubleValue, etc).
     */

    c = cp_find_or_insert(cur_class_file, CONSTANT_Class,
          numeric_wrapper[temp->vartype]);
    bc_append(meth, jvm_checkcast, c);

    if(emit_source) {
      fprintf(curfp, " = (%s) %s.remove(0", java_wrapper[temp->vartype],
         F2J_IO_VEC);
      if(temp->astnode.ident.len > 0)
        fprintf(curfp, ", %d);\n", temp->astnode.ident.len);
      else
        fprintf(curfp, ", 0);\n");
    }
  }
  else if(temp->vartype == Logical) {
    /* special case for boolean since java.lang.Boolean can't be cast
     * to java.lang.Number.
     */

    c = cp_find_or_insert(cur_class_file, CONSTANT_Class, 
       numeric_wrapper[temp->vartype]);
    bc_append(meth, jvm_checkcast, c);

    if(emit_source)
      fprintf(curfp," = ((Boolean) %s.remove(0)).booleanValue();\n",
         F2J_IO_VEC);
    c = bc_new_methodref(cur_class_file, numeric_wrapper[temp->vartype],
          numericValue_method[temp->vartype],
          numericValue_descriptor[temp->vartype]);
    bc_append(meth, jvm_invokevirtual, c);
  }
  else {
    c = cp_find_or_insert(cur_class_file, CONSTANT_Class, JL_NUMBER);
    bc_append(meth, jvm_checkcast, c);

    if(emit_source)
      fprintf(curfp," = ((Number) %s.remove(0)).%s();\n",
         F2J_IO_VEC, numericValue_method[temp->vartype]);
    c = bc_new_methodref(cur_class_file, JL_NUMBER,
          numericValue_method[temp->vartype],
          numericValue_descriptor[temp->vartype]);
    bc_append(meth, jvm_invokevirtual, c);
  }

  LHS_bytecode_emit(meth, assign_temp);

  free_ast_node(assign_temp);
  if(idx_temp) {
    free_ast_node(idx_temp);
    temp->astnode.ident.arraylist = NULL;
  }
}

void
gen_io_arg_count_expr(JVM_METHOD *meth, AST *root)
{
  AST *temp;
  int c;

  for(temp=root;temp!=NULL;temp=temp->nextstmt) {
    if(temp->nodetype == IoImpliedLoop) {
      if(temp->astnode.forloop.Label) {
        fprintf(curfp, "(");
        fprintf(curfp, "Util.getIterationCount(");
        expr_emit(meth, temp->astnode.forloop.start);
        fprintf(curfp, ",");
        expr_emit(meth, temp->astnode.forloop.stop);
        fprintf(curfp, ",");
        if(!temp->astnode.forloop.incr) {
          fprintf(curfp, "1");
          bc_push_int_const(meth, 1);
        }
        else
          expr_emit(meth, temp->astnode.forloop.incr);
        fprintf(curfp, ")");

        c = bc_new_methodref(cur_class_file, UTIL_CLASS, "getIterationCount",
              ITER_COUNT_DESC);
        bc_append(meth, jvm_invokestatic, c);

        if(temp->astnode.forloop.Label) {
          fprintf(curfp, " * (");
          gen_io_arg_count_expr(meth, temp->astnode.forloop.Label);
          fprintf(curfp, ")");
          bc_append(meth, jvm_imul);
        }

        fprintf(curfp, ")");
      }
    }
    else {
      fprintf(curfp, "1");
      bc_push_int_const(meth, 1);
    }

    if(temp->nextstmt)
      fprintf(curfp, " + ");

    if(temp != root)
      bc_append(meth, jvm_iadd);
  }
}

/*****************************************************************************
 *                                                                           *
 * read_emit                                                                 *
 *                                                                           *
 * This function generates formatted READ statements.  J.Paine's formatter   *
 * is used behind the scenes.                                                *
 *                                                                           *
 *****************************************************************************/

void
formatted_read_emit(JVM_METHOD *meth, AST *root, char *fmt_str)
{
  AST *temp;
  int c;

  /* if the READ statement has no args, just read a line and
   * ignore it.
   */

  if(root->astnode.io_stmt.arg_list == NULL) {
    emit_skip_line(meth, root);
    return;
  }

  gen_clear_io_vec(meth);

  if(root->astnode.io_stmt.arg_list) {
    fprintf(curfp, "Util.setIoArgHint(");
    gen_io_arg_count_expr(meth, root->astnode.io_stmt.arg_list);
    fprintf(curfp, ");\n");
    c = bc_new_methodref(cur_class_file, UTIL_CLASS, "setIoArgHint",
           ARG_HINT_DESC);
    bc_append(meth, jvm_invokestatic, c);
  }

  if((root->astnode.io_stmt.end_num > 0) ||
     (root->astnode.io_stmt.err > 0))
  {
    JVM_CODE_GRAPH_NODE *if_node, *goto_node;

    if(root->astnode.io_stmt.iostat) {
      name_emit(meth, root->astnode.io_stmt.iostat);
      fprintf(curfp, " =");
    }
    else {
      fprintf(curfp, "%s =", F2J_TMP_IOSTAT);
    }

    /* the READ statement includes an END label, so we
     * test the return value to determine EOF.
     */
    fprintf(curfp, " Util.f77read(");
    emit_unit_desc(meth, root);
    fprintf(curfp, ", ");

    bc_push_string_const(meth, fmt_str);
    bc_gen_load_op(meth, iovec_lvar, jvm_Object);
    c = bc_new_methodref(cur_class_file, UTIL_CLASS, "f77read", F77_READ_DESC);
    bc_append(meth, jvm_invokestatic, c);
    
    fprintf(curfp, "\"%s\", %s);\n", fmt_str, F2J_IO_VEC);

    if((root->astnode.io_stmt.end_num > 0) ||
       (root->astnode.io_stmt.err > 0))
    {
      if(root->astnode.io_stmt.iostat)
        LHS_bytecode_emit(meth, root->astnode.io_stmt.iostat->parent);
      else
        bc_gen_store_op(meth, iostat_lvar, jvm_Int);
    }

    if(root->astnode.io_stmt.iostat) {
      /* iostat's parent is a dummy assignment whose lhs is the iostat var.
       * that allows correct bytecode generation since iostat will be used
       * in a lhs context.  however, after this point we will use iostat in
       * a rhs context, so we just set parent's lhs to null.  --kgs
       */
      root->astnode.io_stmt.iostat->parent->astnode.assignment.lhs = NULL;
    }

    if(root->astnode.io_stmt.end_num > 0) {
      fprintf(curfp, "   if(");
      if(root->astnode.io_stmt.iostat)
        name_emit(meth, root->astnode.io_stmt.iostat);
      else {
        fprintf(curfp, "%s", F2J_TMP_IOSTAT);
        bc_gen_load_op(meth, iostat_lvar, jvm_Int);
      }
      fprintf(curfp, " == -1)\n");
      fprintf(curfp, "     Dummy.go_to(\"%s\",%d);\n",cur_filename,
         root->astnode.io_stmt.end_num);

      bc_append(meth, jvm_iconst_m1);
      if_node = bc_append(meth, jvm_if_icmpne);
      goto_node = bc_append(meth, jvm_goto);
      bc_set_integer_branch_label(goto_node, root->astnode.io_stmt.end_num);
      bc_set_branch_target(if_node, bc_append(meth, jvm_xxxunusedxxx));
    }

    if(root->astnode.io_stmt.err > 0) {
      fprintf(curfp, "   if(");
      if(root->astnode.io_stmt.iostat)
        name_emit(meth, root->astnode.io_stmt.iostat);
      else {
        fprintf(curfp, "%s", F2J_TMP_IOSTAT);
        bc_gen_load_op(meth, iostat_lvar, jvm_Int);
      }
      fprintf(curfp, " > 0)\n");
      fprintf(curfp, "     Dummy.go_to(\"%s\",%d);\n",cur_filename,
         root->astnode.io_stmt.err);

      if_node = bc_append(meth, jvm_ifle);
      goto_node = bc_append(meth, jvm_goto);
      bc_set_integer_branch_label(goto_node, root->astnode.io_stmt.err);
      bc_set_branch_target(if_node, bc_append(meth, jvm_xxxunusedxxx));
    }
  }
  else {
    fprintf(curfp, "Util.f77read(");
    emit_unit_desc(meth, root);
    fprintf(curfp, ", ");

    bc_push_string_const(meth, fmt_str);
    bc_gen_load_op(meth, iovec_lvar, jvm_Object);
    c = bc_new_methodref(cur_class_file, UTIL_CLASS, "f77read", F77_READ_DESC);
    bc_append(meth, jvm_invokestatic, c);

    fprintf(curfp, "\"%s\", %s);\n", fmt_str, F2J_IO_VEC);
    /* return value is unused, so pop it off the stack */
    bc_append(meth, jvm_pop);
  }

  for(temp=root->astnode.io_stmt.arg_list;temp!=NULL;temp=temp->nextstmt)
  {
    HASHNODE *ht;

    if(temp->nodetype == IoImpliedLoop)
      implied_loop_emit(meth, temp, formatted_read_implied_loop_bytecode_emit,
             formatted_read_implied_loop_sourcecode_emit);
    else if((temp->nodetype == Identifier) &&
        (ht=type_lookup(cur_array_table, temp->astnode.ident.name)) &&
        (temp->astnode.ident.arraylist == NULL))
    {
      if(ht->variable->astnode.ident.array_len == -1) {
        fprintf(stderr, "Warning: passing implied size array to formatted read.\n");
        fprintf(stderr, "         this won't work properly.\n");

        formatted_read_assign_emit(meth, temp, TRUE, -1);
      }
      else {
        int i;

        for(i=0; i < ht->variable->astnode.ident.array_len; i++) {
          formatted_read_assign_emit(meth, temp, TRUE, i+1);
        }
      }
    }
    else if(temp->nodetype == Identifier)
      formatted_read_assign_emit(meth, temp, TRUE, -1);
    else
    {
      fprintf(stderr,"Read list must consist of idents or implied loops\n");
      fprintf(stderr,"   nodetype is %s\n", print_nodetype(temp));
      continue;
    }
  }
}

/*****************************************************************************
 *                                                                           *
 * formatted_read_implied_loop_bytecode_emit                                 *
 *                                                                           *
 * This function generates code for implied DO loops contained in READ       *
 * statements including FORMAT statements.                                   *
 *                                                                           *
 *****************************************************************************/

void
formatted_read_implied_loop_bytecode_emit(JVM_METHOD *meth, AST *node)
{ 
  AST *iot;
  
  for(iot = node->astnode.forloop.Label; iot != NULL; iot = iot->nextstmt)
  { 
    if(iot->nodetype == IoImpliedLoop) {
      implied_loop_bytecode_emit(meth, iot, formatted_read_implied_loop_bytecode_emit);
    }
    else if(iot->nodetype != Identifier) {
      fprintf(stderr,"unit %s:Cant handle this nodetype (%s) ",
        unit_name,print_nodetype(iot));
      fprintf(stderr," in implied loop (read stmt)\n");
    }
    else {
      formatted_read_assign_emit(meth, iot, FALSE, -1);
    }
  }
}

/*****************************************************************************
 *                                                                           *
 * formatted_read_implied_loop_sourcecode_emit                               *
 *                                                                           *
 * This function generates code for implied DO loops contained in READ       *
 * statements including FORMAT statements.                                   *
 *                                                                           *
 *****************************************************************************/

void
formatted_read_implied_loop_sourcecode_emit(JVM_METHOD *meth, AST *node)
{
  AST *iot;

  fprintf(curfp,"{\n");
  for(iot = node->astnode.forloop.Label; iot != NULL; iot = iot->nextstmt)
  {
    if(iot->nodetype == IoImpliedLoop) {
      implied_loop_sourcecode_emit(meth, iot, formatted_read_implied_loop_sourcecode_emit);
    }
    else if(iot->nodetype != Identifier) {
      fprintf(stderr,"unit %s:Cant handle this nodetype (%s) ",
        unit_name,print_nodetype(iot));
      fprintf(stderr," in implied loop (read stmt)\n");
    }
    else {
      name_emit(meth, iot);

      if((iot->vartype == Character) || (iot->vartype == String))
        fprintf(curfp," = (%s) %s.remove(0);\n", java_wrapper[iot->vartype],
           F2J_IO_VEC);
      else if(iot->vartype == Logical)
        fprintf(curfp," = ((Boolean) %s.remove(0)).booleanValue();\n",
           F2J_IO_VEC);
      else
        fprintf(curfp," = ((Number) %s.remove(0)).%s();\n",
           F2J_IO_VEC, numericValue_method[iot->vartype]);
    }
  }
  fprintf(curfp,"}\n");
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
read_implied_loop_bytecode_emit(JVM_METHOD *meth, AST *node)
{
  AST *assign_temp, *temp, *iot;
  int c;
  
  for(iot = node->astnode.forloop.Label; iot != NULL; iot = iot->nextstmt)
  {
    if(iot->nodetype == IoImpliedLoop) {
      implied_loop_bytecode_emit(meth, iot, read_implied_loop_bytecode_emit);
    }
    else if(iot->nodetype != Identifier) {
      fprintf(stderr,"unit %s:Cant handle this nodetype (%s) ",
        unit_name,print_nodetype(iot));
      fprintf(stderr," in implied loop (read stmt)\n");
    }
    else {
      assign_temp = addnode();
      assign_temp->nodetype = Assignment;

      temp = iot;
      temp->parent = assign_temp;
      assign_temp->astnode.assignment.lhs = temp;

      name_emit(meth, assign_temp->astnode.assignment.lhs);

      bc_gen_load_op(meth, stdin_lvar, jvm_Object);
      fprintf(curfp," = %s.%s(", F2J_STDIN, funcname[iot->vartype]);
      emit_unit_desc(meth, node->parent);
      fprintf(curfp,");\n");

      if( (temp->vartype == Character) || (temp->vartype == String) ) {
        if(temp->astnode.ident.len < 0)
          bc_push_int_const(meth, 1);
        else
          bc_push_int_const(meth, temp->astnode.ident.len);
      }

      c = bc_new_methodref(cur_class_file, EASYIN_CLASS, funcname[temp->vartype],
            input_descriptors[temp->vartype]);
      bc_append(meth, jvm_invokevirtual, c);

      LHS_bytecode_emit(meth, assign_temp);
    }
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
read_implied_loop_sourcecode_emit(JVM_METHOD *meth, AST *node)
{
  AST *iot;

  fprintf(curfp,"{\n");
  for(iot = node->astnode.forloop.Label; iot != NULL; iot = iot->nextstmt)
  {
    if(iot->nodetype == IoImpliedLoop) {
      implied_loop_sourcecode_emit(meth, iot, read_implied_loop_sourcecode_emit);
    }
    else if(iot->nodetype != Identifier) {
      fprintf(stderr,"unit %s:Cant handle this nodetype (%s) ",
        unit_name,print_nodetype(iot));
      fprintf(stderr," in implied loop (read stmt)\n");
    }
    else {
      name_emit(meth, iot);
      fprintf(curfp," = %s.%s(", F2J_STDIN, funcname[iot->vartype]);
      emit_unit_desc(meth, node->parent);
      fprintf(curfp,");\n");
    }
  }
  fprintf(curfp,"}\n");
}

/*****************************************************************************
 *                                                                           *
 * isArrayNoIdx                                                              *
 *                                                                           *
 * returns TRUE if this is an array reference which is not indexed.          *
 *                                                                           *
 *****************************************************************************/

BOOL
isArrayNoIdx(AST *var)
{
  return( (var->token == NAME) && 
        (type_lookup(cur_array_table, var->astnode.ident.name) != NULL) &&
        (var->astnode.ident.arraylist == NULL) );

}

/*****************************************************************************
 *                                                                           *
 * format2str                                                                *
 *                                                                           *
 * Converts a list of format items to a format string.                       *
 *                                                                           *
 *****************************************************************************/

char *
format2str(AST *node)
{
  char buf[8192], *tmpstr;
  AST *temp;
  int i, j;

  buf[0] = 0;

  for(temp = node; temp; temp=temp->nextstmt) {
    switch(temp->token) {
      case EDIT_DESC:
      case NAME:
        strcat(buf, temp->astnode.ident.name);
        break;
      case STRING:
        /* escaping quotes in the string to be passed to the Formatter.
         * largest temp can be is 2 * len + 1 (if every char is a quote)
         */

        tmpstr = malloc(2 * strlen(temp->astnode.constant.number) + 1);
        if(!tmpstr)
          return NULL;

        for(i = j = 0; i < strlen(temp->astnode.constant.number); i++) {
          if(temp->astnode.constant.number[i] == '\'') {
            tmpstr[j] = '\'';
            j++;
            tmpstr[j] = '\'';
            j++;
          }
          else {
            tmpstr[j] = temp->astnode.constant.number[i];
            j++;
          } 
        }
        tmpstr[j] = 0;

        strcat(buf, "'");
        strcat(buf, tmpstr);
        strcat(buf, "'");

        free(tmpstr);
        break;
      case INTEGER:
        strcat(buf, temp->astnode.constant.number);
        break;
      case REPEAT:
        tmpstr = format2str(temp->astnode.label.stmt);
        strcat(buf, "(");
        strcat(buf, tmpstr);
        strcat(buf, ")");
        free(tmpstr);
        break;
      case CM:
        strcat(buf, ",");
        break;
      case DIV:
        strcat(buf, "/");
        break;
      case CAT:
        strcat(buf, "//");
        break;
      case COLON:
        strcat(buf, ":");
        break;
      default:
        fprintf(stderr,"formatitem2str: Unknown token!!! %d (%s) - ",
           temp->token, tok2str(temp->token));
        if(gendebug)
          printf("this node type %s\n",print_nodetype(temp));
        break;
    }
  }

  return strdup(buf);
}

/*****************************************************************************
 *                                                                           *
 * gen_clear_io_vec                                                          *
 *                                                                           *
 * Generates code to clear the Vector used for formatted I/O calls.          *
 *                                                                           *
 *****************************************************************************/

void
gen_clear_io_vec(JVM_METHOD *meth)
{
  int c;

  fprintf(curfp, "%s.clear();\n", F2J_IO_VEC);

  bc_gen_load_op(meth, iovec_lvar, jvm_Object);
  c = bc_new_methodref(cur_class_file, IOVECTOR_CLASS, "clear", "()V");
  bc_append(meth, jvm_invokevirtual, c);
}

/*****************************************************************************
 *                                                                           *
 * write_argument_emit                                                       *
 *                                                                           *
 * generates a single argument of a WRITE statement.                         *
 *                                                                           *
 *****************************************************************************/

void
write_argument_emit(JVM_METHOD *meth, AST *root)
{
  HASHNODE *ht;
  int c;

  if((root->nodetype == Identifier) && 
      (ht=type_lookup(cur_array_table, root->astnode.ident.name)) &&
      (root->astnode.ident.arraylist == NULL))
  {
    bc_gen_load_op(meth, iovec_lvar, jvm_Object);
    c = cp_find_or_insert(cur_class_file, CONSTANT_Class, ARRAY_SPEC_CLASS);
    bc_append(cur_method, jvm_new,c);
    bc_append(cur_method, jvm_dup);

    fprintf(curfp, "  %s.addElement(new ArraySpec(", F2J_IO_VEC);

    if(ht->variable->astnode.ident.array_len == -1) {
      fprintf(stderr, "Warning: passing implied size array to formatted write\n");
      fprintf(stderr, "         only using first element\n");
      root->parent->nodetype = Call;
      expr_emit(meth, root);
      root->parent->nodetype = Write;
    }
    else
      expr_emit(meth, root);

    fprintf(curfp, ", %d));\n", ht->variable->astnode.ident.array_len);

    bc_push_int_const(meth, ht->variable->astnode.ident.array_len);

    c = bc_new_methodref(cur_class_file, ARRAY_SPEC_CLASS, "<init>",
           array_spec_descriptor[root->vartype]);

    bc_append(cur_method, jvm_invokespecial, c);
    c = bc_new_methodref(cur_class_file, IOVECTOR_CLASS, "addElement",
        VEC_ADD_DESC);
    bc_append(meth, jvm_invokevirtual, c);
  }
  else {
    ht = type_lookup(cur_type_table, root->astnode.ident.name);

    if(ht && (root->vartype == String) &&
       (root->astnode.ident.len == 1) &&
       (root->astnode.ident.dim == 0) &&
       (root->astnode.ident.arraylist == NULL) &&
       (ht->variable->astnode.ident.startDim[2] != NULL))
    {
      bc_gen_load_op(meth, iovec_lvar, jvm_Object);
      c = cp_find_or_insert(cur_class_file, CONSTANT_Class, ARRAY_SPEC_CLASS);
      bc_append(cur_method, jvm_new,c);
      bc_append(cur_method, jvm_dup);

      fprintf(curfp, "  %s.addElement(new ArraySpec(", F2J_IO_VEC);

      expr_emit(meth, root);

      fprintf(curfp, "));\n");

      c = bc_new_methodref(cur_class_file, ARRAY_SPEC_CLASS, "<init>",
           "(Ljava/lang/String;)V");
    }
    else {
      bc_gen_load_op(meth, iovec_lvar, jvm_Object);
      c = cp_find_or_insert(cur_class_file, CONSTANT_Class,
                numeric_wrapper[root->vartype]);
      bc_append(meth, jvm_new,c);
      bc_append(meth, jvm_dup);

      c = bc_new_methodref(cur_class_file,numeric_wrapper[root->vartype],
            "<init>", wrapper_descriptor[root->vartype]);

      fprintf(curfp, "  %s.addElement(new %s(", F2J_IO_VEC,
          java_wrapper[root->vartype]);
      expr_emit(meth, root);
      fprintf(curfp,"));\n");
    }

    bc_append(meth, jvm_invokespecial, c);
    c = bc_new_methodref(cur_class_file, IOVECTOR_CLASS, "addElement",
        VEC_ADD_DESC);
    bc_append(meth, jvm_invokevirtual, c);
  }
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
write_emit(JVM_METHOD *meth, AST * root)
{
  JVM_CODE_GRAPH_NODE *if_node, *goto_node;
  char *fmt_str, tmp[100];
  HASHNODE *hnode;
  AST *temp;
  int c;

  /* look for a format statement */
  sprintf(tmp,"%d", root->astnode.io_stmt.format_num);
  if(gendebug)
    printf("***Looking for format statement number: %s\n",tmp);

  hnode = format_lookup(cur_format_table, tmp);

  if(hnode)
    fmt_str = format2str(hnode->variable->astnode.label.stmt);
  else if((root->astnode.io_stmt.fmt_list != NULL) &&
          (root->astnode.io_stmt.fmt_list->nodetype == Constant))
    fmt_str = strdup(root->astnode.io_stmt.fmt_list->astnode.constant.number);
  else
    fmt_str = NULL;

  gen_clear_io_vec(meth);

  for(temp=root->astnode.io_stmt.arg_list;temp!=NULL;temp=temp->nextstmt) {
    if(temp->nodetype == IoImpliedLoop) {
      implied_loop_emit(meth, temp, write_implied_loop_bytecode_emit,
          write_implied_loop_sourcecode_emit);
    }
    else
      write_argument_emit(meth, temp);
  }

  /* if ERR is set, then generate the conditional branch in case of error */
  if(root->astnode.io_stmt.err > 0)
    fprintf(curfp, "  if((");

  if(root->astnode.io_stmt.iostat) {
    name_emit(meth, root->astnode.io_stmt.iostat);
    fprintf(curfp, " = ");
  }

  fprintf(curfp, "Util.f77write(");

  if(root->astnode.io_stmt.unit_desc && 
     root->astnode.io_stmt.unit_desc->token != STAR)
  {
    expr_emit(meth, root->astnode.io_stmt.unit_desc);
    fprintf(curfp, ", ");
  }
  else {
    fprintf(curfp, "%d, ", F77_STDOUT);
    bc_push_int_const(meth, F77_STDOUT);
  }

  if(fmt_str) {
    fprintf(curfp, "\"%s\", %s)", fmt_str, F2J_IO_VEC);
    bc_push_string_const(meth, fmt_str);
  }
  else {
    if((root->astnode.io_stmt.fmt_list != NULL) &&
          (root->astnode.io_stmt.fmt_list->nodetype != Constant))
    {
      bc_gen_load_op(meth, fmt_tab_lvar, jvm_Object);
      c = cp_find_or_insert(cur_class_file, CONSTANT_Class, JL_INTEGER);
      bc_append(meth, jvm_new, c);
      bc_append(meth, jvm_dup);
      fprintf(curfp, "(String)%s.get(new Integer(", F2J_FMT_TAB);
      expr_emit(meth, root->astnode.io_stmt.fmt_list);
      fprintf(curfp, ")), %s)", F2J_IO_VEC);
      c = bc_new_methodref(cur_class_file, JL_INTEGER, "<init>",
            NEW_INTEGER_DESC);
      bc_append(meth, jvm_invokespecial, c);
      c = bc_new_methodref(cur_class_file, HASHTAB_CLASS, "get",
            HASHTAB_GET_DESC);
      bc_append(meth, jvm_invokevirtual, c);
      c = cp_find_or_insert(cur_class_file, CONSTANT_Class, JL_STRING);
      bc_append(meth, jvm_checkcast, c);
    }
    else {
      fprintf(curfp, "null, %s)", F2J_IO_VEC);
      bc_append(meth, jvm_aconst_null);
    }
  }

  bc_gen_load_op(meth, iovec_lvar, jvm_Object);

  c = bc_new_methodref(cur_class_file, UTIL_CLASS, "f77write", F77_WRITE_DESC);
  bc_append(meth, jvm_invokestatic, c);

  if((root->astnode.io_stmt.err > 0) && root->astnode.io_stmt.iostat)
    bc_append(meth, jvm_dup);

  /* if IOSTAT is set, then emit the LHS assignment to set the ret value */
  if(root->astnode.io_stmt.iostat)
    LHS_bytecode_emit(meth, root->astnode.io_stmt.iostat->parent);

  if(root->astnode.io_stmt.err > 0) {
    if_node = bc_append(meth, jvm_ifle);

    goto_node = bc_append(meth, jvm_goto);

    bc_set_integer_branch_label(goto_node, root->astnode.io_stmt.err);

    bc_set_branch_target(if_node, bc_append(meth, jvm_xxxunusedxxx));

    fprintf(curfp, ") > 0)\n");
    fprintf(curfp,"    Dummy.go_to(\"%s\",%d);\n",cur_filename,
        root->astnode.io_stmt.err);
  }
  else {
    fprintf(curfp, ";\n");
    if(!root->astnode.io_stmt.iostat)
      bc_append(meth, jvm_pop);
  }
}

/*****************************************************************************
 *                                                                           *
 * implied_loop_bytecode_emit                                                *
 *                                                                           *
 * Emits the JVM bytecode for an implied loop.  The code gen order is        *
 * a bit different so we split this into two functions: one for bytecode and *
 * one for source code.                                                      *
 *                                                                           *
 *****************************************************************************/

void
implied_loop_bytecode_emit(JVM_METHOD *meth, AST *node,
    void loop_body_bytecode_emit(JVM_METHOD *, AST *))
{
  JVM_CODE_GRAPH_NODE *if_node, *goto_node, *iload_node;
  AST *temp;
  unsigned int icount;

  temp = addnode();
  temp->nodetype = Assignment;
  temp->astnode.assignment.lhs = node->astnode.forloop.counter;
  temp->astnode.assignment.lhs->parent = temp;
  temp->astnode.assignment.rhs = node->astnode.forloop.start;
  temp->astnode.assignment.rhs->parent = temp;

  /* the rest of this code is only generated as bytecode.
   * first emit the initial assignment.
   */
  assign_emit(meth, temp);

  /* now emit the expression to calculate the number of
   * iterations that this loop should make and store the result
   * into the next available local variable.
   */
  expr_emit(meth, node->astnode.forloop.iter_expr);
  icount = bc_get_next_local(meth, jvm_Int);
  bc_gen_store_op(meth, icount, jvm_Int);

  /* goto the end of the loop where we test for completion */
  goto_node = bc_append(meth, jvm_goto);

  loop_body_bytecode_emit(meth, node);

  /* increment loop variable */
  assign_emit(meth, node->astnode.forloop.incr_expr);

  /* decrement iteration count */
  bc_gen_iinc(meth, icount, -1);

  iload_node = bc_gen_load_op(meth, icount, jvm_Int);

  bc_set_branch_target(goto_node, iload_node);

  if_node = bc_append(meth, jvm_ifgt);
  bc_set_branch_target(if_node, bc_get_next_instr(goto_node));

  bc_release_local(meth, jvm_Int);
}

/*****************************************************************************
 *                                                                           *
 * implied_loop_sourcecode_emit                                              *
 *                                                                           *
 * Emits the Java source code for an implied loop.  The code gen order is    *
 * a bit different so we split this into two functions: one for bytecode and *
 * one for source code.                                                      *
 *                                                                           *
 *****************************************************************************/

void
implied_loop_sourcecode_emit(JVM_METHOD *meth, AST *node,
    void loop_body_sourcecode_emit(JVM_METHOD *, AST *))
{
  AST *temp;

  temp = addnode();
  temp->nodetype = Assignment;
  temp->astnode.assignment.lhs = node->astnode.forloop.counter;
  temp->astnode.assignment.lhs->parent = temp;
  temp->astnode.assignment.rhs = node->astnode.forloop.start;
  temp->astnode.assignment.rhs->parent = temp;

  fprintf(curfp,"for(");

  assign_emit(meth, temp);

  fprintf(curfp,"; ");

  expr_emit(meth, node->astnode.forloop.counter);
  fprintf(curfp," <= ");
  expr_emit(meth, node->astnode.forloop.stop);

  if(node->astnode.forloop.incr == NULL) {
    fprintf(curfp,"; ");
    expr_emit(meth, node->astnode.forloop.counter);
    fprintf(curfp,"++)\n");
  }
  else
  {
    fprintf(curfp,"; ");
    expr_emit(meth, node->astnode.forloop.counter);
    fprintf(curfp," += ");
    expr_emit(meth, node->astnode.forloop.incr);
    fprintf(curfp,")\n");
  }

  loop_body_sourcecode_emit(meth, node);
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
implied_loop_emit(JVM_METHOD *meth, AST *node,
        void loop_body_bytecode_emit(JVM_METHOD *, AST *),
        void loop_body_sourcecode_emit(JVM_METHOD *, AST *))
{
  set_bytecode_status(meth, JAVA_ONLY);

  implied_loop_sourcecode_emit(meth, node, loop_body_sourcecode_emit);

  set_bytecode_status(meth, JVM_ONLY);

  implied_loop_bytecode_emit(meth, node, loop_body_bytecode_emit);

  set_bytecode_status(meth, JAVA_AND_JVM);
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
write_implied_loop_sourcecode_emit(JVM_METHOD *meth, AST *node)
{
  AST *temp;

  fprintf(curfp,"{\n");
  for(temp = node->astnode.forloop.Label; temp != NULL; temp = temp->nextstmt)
  {
    if(temp->nodetype == Identifier) {
      write_argument_emit(meth, temp);
    }
    else if(temp->nodetype == Constant) {
      fprintf(curfp, "  %s.addElement(new %s(", F2J_IO_VEC,
          java_wrapper[temp->vartype]);
      expr_emit(meth, temp);
      fprintf(curfp,"));\n");
    }
    else if(temp->nodetype == IoImpliedLoop) {
      implied_loop_sourcecode_emit(meth, temp, write_implied_loop_sourcecode_emit);
    }
    else {
      fprintf(stderr,"unit %s:Cant handle this nodetype (%s) ",
        unit_name,print_nodetype(temp));
      fprintf(stderr," in implied loop (write stmt).  Exiting.\n");
      exit(EXIT_FAILURE);
    }
  }
  fprintf(curfp,"}\n");
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
write_implied_loop_bytecode_emit(JVM_METHOD *meth, AST *node)
{
  AST *temp;
  int c;

  for(temp = node->astnode.forloop.Label; temp != NULL; temp = temp->nextstmt)
  {
    /* emit loop body */

    if(temp->nodetype == Identifier) {
      write_argument_emit(meth, temp);
    }
    else if(temp->nodetype == Constant) {
      bc_gen_load_op(meth, iovec_lvar, jvm_Object);
      c = cp_find_or_insert(cur_class_file, CONSTANT_Class,
                numeric_wrapper[temp->vartype]);
      bc_append(meth, jvm_new,c);
      bc_append(meth, jvm_dup);

      c = bc_new_methodref(cur_class_file,numeric_wrapper[temp->vartype],
            "<init>", wrapper_descriptor[temp->vartype]);

      pushConst(meth, temp);

      bc_append(meth, jvm_invokespecial, c);
      c = bc_new_methodref(cur_class_file, IOVECTOR_CLASS, "addElement", 
          VEC_ADD_DESC);
      bc_append(meth, jvm_invokevirtual, c);
    }
    else if(temp->nodetype == IoImpliedLoop) {
      implied_loop_bytecode_emit(meth, temp, write_implied_loop_bytecode_emit);
    }
    else {
      fprintf(stderr,"unit %s:Cant handle this nodetype (%s) ",
        unit_name,print_nodetype(temp));
      fprintf(stderr," in implied loop (write stmt).  Exiting.\n");
      exit(EXIT_FAILURE);
    }
  }
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
blockif_emit (JVM_METHOD *meth, AST * root)
{
  JVM_CODE_GRAPH_NODE *if_node, *next_node, *goto_node;
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
                while_emit(meth, root);
                dl_delete_list(gotos);
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
    expr_emit (meth, root->astnode.blockif.conds);

  if_node = bc_append(meth, jvm_ifeq);

  fprintf (curfp, ")  {\n    ");
  if(root->astnode.blockif.stmts != NULL)
    emit (root->astnode.blockif.stmts);
  fprintf (curfp, "}\n");

  if(root->astnode.blockif.elseifstmts || root->astnode.blockif.elsestmts)
  {
    goto_node = bc_append(meth, jvm_goto);

    dl_insert_b(gotos, goto_node);

    /* create a dummy instruction node so that
     * we have a branch target for the goto statement.
     * it will be removed later.
     */
    next_node = bc_append(meth, jvm_xxxunusedxxx);
    bc_set_branch_target(if_node, next_node);

    for(temp = root->astnode.blockif.elseifstmts; 
        temp != NULL;
        temp = temp->nextstmt)
    {
      goto_node = elseif_emit (meth, temp);
      dl_insert_b(gotos, goto_node);
    }

    if(root->astnode.blockif.elsestmts != NULL)
      else_emit (root->astnode.blockif.elsestmts);

    next_node = bc_append(meth, jvm_xxxunusedxxx);

    dl_traverse(lptr, gotos) {
      goto_node = (JVM_CODE_GRAPH_NODE *) lptr->val;
      bc_set_branch_target(goto_node, next_node);
    }

    dl_delete_list(gotos);
  }
  else {
    /* Else there are no else or elseif blocks, so we do not need
     * any gotos to branch from the end of the blocks to the statement
     * following the block if.  All we need to do is set the if_node
     * branch target to the opcode to which we should branch if the
     * conditional expression is false.
     */

    next_node = bc_append(meth, jvm_xxxunusedxxx);
    bc_set_branch_target(if_node, next_node);
  }

  /* If the endif has a statement label, create a new Label node
   * and add it as the next statement.  It will get emitted on the
   * next call to emit().
   */

  if(root->astnode.blockif.endif_label >= 0) {
    AST *newnode;

    newnode = addnode();
    newnode->nodetype = Label;
    newnode->astnode.label.number = root->astnode.blockif.endif_label;
    newnode->astnode.label.stmt = NULL;

    newnode->nextstmt = root->nextstmt;
    root->nextstmt = newnode;
  }
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
while_emit(JVM_METHOD *meth, AST *root)
{
  JVM_CODE_GRAPH_NODE *if_node, *next_node;

  fprintf(curfp, "while (");
  if (root->astnode.blockif.conds != NULL)
    expr_emit (meth, root->astnode.blockif.conds);
  fprintf (curfp, ")  {\n    ");
  if_node = bc_append(meth, jvm_ifeq);
  emit (root->astnode.blockif.stmts);

  /* create a dummy instruction node so that
   * we have a branch target for the goto statement.
   * it will be removed later.
   */
  next_node = bc_append(meth, jvm_xxxunusedxxx);
  bc_set_branch_target(if_node, next_node);

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

JVM_CODE_GRAPH_NODE *
elseif_emit (JVM_METHOD *meth, AST * root)
{
  JVM_CODE_GRAPH_NODE *if_node, *next_node, *goto_node;

  if(gendebug)printf("in else if\n");
  fprintf (curfp, "else if (");
  
  if (root->astnode.blockif.conds != NULL)
    expr_emit (meth, root->astnode.blockif.conds);
  if_node = bc_append(meth, jvm_ifeq);
  fprintf (curfp, ")  {\n    ");
  emit (root->astnode.blockif.stmts);
  fprintf (curfp, "}              // Close else if()\n");

  goto_node = bc_append(meth, jvm_goto);

  /* create a dummy instruction node so that we have a branch target 
   * for the conditional statement. it will be removed later.
   */
  next_node = bc_append(meth, jvm_xxxunusedxxx);
  bc_set_branch_target(if_node, next_node);

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
method_name_emit(JVM_METHOD *meth, AST *root, BOOL adapter)
{
  char *tempname;
  HASHNODE *ht;
  AST *temp;
  int c;

  /* shouldn't be necessary to lowercase the name
   *   lowercase (root->astnode.ident.name);
   */

  tempname = strdup(root->astnode.ident.name);
  *tempname = toupper(*tempname);

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
        exit(EXIT_FAILURE);
      }
 
      bc_gen_load_op(meth, ht->variable->astnode.ident.localvnum, jvm_Object);
      bc_append(meth, jvm_aconst_null);
      bc_append(meth, jvm_aconst_null);

      c = bc_new_methodref(cur_class_file, METHOD_CLASS, "invoke",
            INVOKE_DESC);
      bc_append(meth, jvm_invokevirtual, c);

      if(root->nodetype == Call) {
        /* already called invoke().  for CALL, ignore the return value. */
        bc_append(meth, jvm_pop);

        fprintf(curfp,"_%s_meth.invoke(null,null);\n",
           root->astnode.ident.name);
      }
      else {

        c = cp_find_or_insert(cur_class_file,CONSTANT_Class,
              numeric_wrapper[root->vartype]);
        bc_append(meth, jvm_checkcast, c);

        if((root->vartype == String) || (root->vartype == Character)) {
          fprintf(curfp,"(%s)_%s_meth.invoke(null,null)",
            java_wrapper[root->vartype], root->astnode.ident.name);
        }
        else {
          fprintf(curfp,"((%s)_%s_meth.invoke(null,null)).%s()",
            java_wrapper[root->vartype], root->astnode.ident.name, 
            numericValue_method[root->vartype]);
          
          c = bc_new_methodref(cur_class_file, numeric_wrapper[root->vartype],
                numericValue_method[root->vartype], 
                numericValue_descriptor[root->vartype]);
          bc_append(meth, jvm_invokevirtual, c);
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

      bc_push_int_const(meth, cnt);

      c = cp_find_or_insert(cur_class_file,CONSTANT_Class,
                "java/lang/Object");

      bc_append(meth, jvm_anewarray, c);
      arr_local = bc_get_next_local(meth, jvm_Object);
      bc_gen_store_op(meth, arr_local,jvm_Object);

      /* foreach arg, assign that arg to an element of the object array */

      cnt = 0;
      for( temp = root->astnode.ident.arraylist; temp; temp = temp->nextstmt)
      {
        fprintf(curfp,"_%s_args[%d] = ", root->astnode.ident.name, cnt);

        bc_gen_load_op(meth, arr_local,jvm_Object);
        bc_push_int_const(meth, cnt);

        if((temp->nodetype == Identifier) && 
           (temp->astnode.ident.arraylist == NULL) &&
           type_lookup(cur_array_table, temp->astnode.ident.name))
        {
          expr_emit (meth, temp);
          bc_append(meth, jvm_aastore);

          fprintf(curfp,";\n");
          fprintf(curfp,"_%s_args[%d] = new Integer(0);\n", 
             root->astnode.ident.name, ++cnt);

          bc_gen_load_op(meth, arr_local,jvm_Object);
          bc_push_int_const(meth, cnt);  /* incremented 2 lines above */

          c = cp_find_or_insert(cur_class_file,CONSTANT_Class,
                numeric_wrapper[Integer]);

          bc_append(meth, jvm_new,c);
          bc_append(meth, jvm_dup);

          c = bc_new_methodref(cur_class_file,numeric_wrapper[Integer],
                "<init>", wrapper_descriptor[Integer]);
          bc_push_int_const(meth, 0);

          bc_append(meth, jvm_invokespecial, c);
        }
        else
        {
          fprintf(curfp,"new %s(", java_wrapper[temp->vartype]);

          c = cp_find_or_insert(cur_class_file,CONSTANT_Class,
                numeric_wrapper[temp->vartype]);

          bc_append(meth, jvm_new,c);
          bc_append(meth, jvm_dup);

          c = bc_new_methodref(cur_class_file,numeric_wrapper[temp->vartype],
                "<init>", wrapper_descriptor[temp->vartype]);

          expr_emit (meth, temp);
          fprintf(curfp,");\n");

          bc_append(meth, jvm_invokespecial, c);
        }

        bc_append(meth, jvm_aastore);

        cnt++;
      }

      ht = type_lookup(cur_external_table, root->astnode.ident.name);
      if(!ht) {
        fprintf(stderr,"(3)Error: expected to find '%s' in external table.\n",
            root->astnode.ident.name);
        exit(EXIT_FAILURE);
      }

      bc_gen_load_op(meth, ht->variable->astnode.ident.localvnum, jvm_Object);
      bc_append(meth, jvm_aconst_null);
      bc_gen_load_op(meth, arr_local, jvm_Object);

      c = bc_new_methodref(cur_class_file, METHOD_CLASS, "invoke",
            INVOKE_DESC);
      bc_append(meth, jvm_invokevirtual, c);

      fprintf(curfp,"_%s_meth.invoke(null,_%s_args);\n",
        root->astnode.ident.name, root->astnode.ident.name);

      bc_release_local(meth, jvm_Object);

      bc_append(meth, jvm_pop);
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
    JVM_METHODREF *mref = get_method_name(root, adapter);

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

    bc_free_fieldref(mref);
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

JVM_METHODREF *
get_method_name(AST *root, BOOL adapter)
{
  char *buf, *tempname;
  char *tmpdesc;
  JVM_METHODREF *newmeth = NULL;

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
      newmeth = (JVM_METHODREF *)f2jalloc(sizeof(JVM_METHODREF));

      newmeth->classname = strdup(cur_filename);
      newmeth->methodname = strdup(buf);

      tmpdesc = get_desc_from_arglist(root->astnode.ident.arraylist);

      newmeth->descriptor = (char*)f2jalloc(strlen(tmpdesc) + 
        strlen(METHOD_CLASS) + 
        strlen(field_descriptor[root->vartype][0]) + 10);
      strcpy(newmeth->descriptor, "(");
      strcat(newmeth->descriptor, "L");
      strcat(newmeth->descriptor, METHOD_CLASS);
      strcat(newmeth->descriptor, ";");
      strcat(newmeth->descriptor, tmpdesc);
      strcat(newmeth->descriptor, ")");
      strcat(newmeth->descriptor, field_descriptor[root->vartype][0]);

      f2jfree(tmpdesc, strlen(tmpdesc)+1);

      if(gendebug)
        printf("methcall descriptor = %s\n",newmeth->descriptor);
    }
  }
  else if(adapter)
  {
    HASHNODE *hashtemp;

    sprintf (buf, "%s_adapter", root->astnode.ident.name);
    newmeth = (JVM_METHODREF *)f2jalloc(sizeof(JVM_METHODREF));
    newmeth->classname = strdup(cur_filename);
    newmeth->methodname = strdup(buf);

    hashtemp = type_lookup(function_table, root->astnode.ident.name);

    if(hashtemp) {
      tmpdesc = get_adapter_desc(hashtemp->variable->astnode.source.descriptor,
         root->astnode.ident.arraylist);
    }
    else {
      JVM_METHODREF *mref;

      mref = find_method(root->astnode.ident.name, descriptor_table);
      if(mref)
        tmpdesc = get_adapter_desc(mref->descriptor,
           root->astnode.ident.arraylist);
      else {
        fprintf(stderr, "WARNING: could not find method descriptor\n");
        tmpdesc = strdup("IIIIIII");  /* just some junk */
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

    f2jfree(tmpdesc, strlen(tmpdesc)+1);

    if(gendebug)
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

JVM_METHODREF *
get_methodref(AST *node)
{
  JVM_METHODREF *new_mref, *srch_mref;
  HASHNODE *ht;
  char *tempname = NULL;

  new_mref = (JVM_METHODREF *)f2jalloc(sizeof(JVM_METHODREF));

  /* first check the symbol table for information about this function.  */

  if( (ht = type_lookup(function_table, node->astnode.ident.name)) != NULL)
  {
    /* we found this method in the symbol table, so now we fill out the
     * methodref structure based on the symtable info. 
     */
    tempname = strdup (node->astnode.ident.name);
    *tempname = toupper (*tempname);

    new_mref->classname  = bc_get_full_classname(tempname, package_name);
    new_mref->methodname = strdup(node->astnode.ident.name);
    if(ht->variable->astnode.source.descriptor == NULL) {
      fprintf(stderr, "Warning: null descriptor for %s...", 
          new_mref->methodname);
      fprintf(stderr, "probably not declared EXTERNAL\n");
    }
    else
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
      METHODTAB *entry;

      /* if we reach this, then we cannot find this method anywhere.
       * try to guess at the descriptor.  Since the guess is likely to
       * be wrong, generate a warning message (unless this is a function
       * passed in as an argument).
       */

      tempname = strdup(node->astnode.ident.name);
      uppercase(tempname);

      entry = methodscan(intrinsic_toks, tempname);

      if(entry) {
        new_mref->classname = strdup(entry->class_name);
        new_mref->methodname = strdup(entry->method_name);
        new_mref->descriptor = strdup(entry->descriptor);

        return(new_mref);
      }

      if(type_lookup(cur_args_table, node->astnode.ident.name) == NULL) {
        fprintf(stderr, "WARNING: could not resolve call to '%s'.\n",
           node->astnode.ident.name);
        fprintf(stderr, "  This will probably result in incorrect code generation.\n");
        fprintf(stderr, "  Make sure the external function was compiled already and\n");
        fprintf(stderr, "  check the paths specified using the -c flag.\n");
      }

      tempname = strdup (node->astnode.ident.name);
      *tempname = toupper (*tempname);

      new_mref->classname  = bc_get_full_classname(tempname, package_name);
      new_mref->methodname = strdup(node->astnode.ident.name);

      f2jfree(tempname, strlen(tempname)+1);
      tempname = get_desc_from_arglist(node->astnode.ident.arraylist);

      new_mref->descriptor = (char *)f2jalloc(strlen(tempname) + 10);

      strcpy(new_mref->descriptor,"(");
      strcat(new_mref->descriptor,tempname);
      strcat(new_mref->descriptor,")V");  /* assume void return type */
    }
    else {
      /* we may later free the mref, so dup the table entry */

      new_mref->classname = strdup(srch_mref->classname);
      new_mref->methodname = strdup(srch_mref->methodname);
      new_mref->descriptor = strdup(srch_mref->descriptor);
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
call_emit(JVM_METHOD *meth, AST * root)
{
  BOOL adapter;
  JVM_METHODREF *mref;
  int c;

  assert(root != NULL);

  if(gendebug)
    printf("@##@ in call_emit, %s\n",root->astnode.ident.name);

  if(!type_lookup(function_table, root->astnode.ident.name) &&
     !find_method(root->astnode.ident.name, descriptor_table))
  {
    METHODTAB *entry;
    char *tempname;

    tempname = strdup(root->astnode.ident.name);
    uppercase(tempname);

    entry = methodscan(intrinsic_toks, tempname);

    if(entry) {
      if(!strcmp("ETIME", tempname)) {
        etime_sub_emit(meth, entry, root->astnode.ident.arraylist);
        return;
      }
    }
  }

  adapter = needs_adapter(root);

  /* if method_name_emit() already completely generated the call, return now */

  if(method_name_emit(meth, root, adapter))
    return;

  if(gendebug)
    printf("call_emit, %s not already emitted\n",root->astnode.ident.name);

  if((root->astnode.ident.arraylist == NULL) ||
     (root->astnode.ident.arraylist->nodetype == EmptyArgList))
  {
    /* the arg list is empty, just emit "()" and return */

    mref = get_method_name(root, adapter);

    if(gendebug)
      printf("call_emit (type: %s), got class = '%s', name = '%s'\n", 
        returnstring[root->vartype], mref->classname, mref->methodname);

    c = bc_new_methodref(cur_class_file,mref->classname, mref->methodname,
                     mref->descriptor);

    bc_append(meth, jvm_invokestatic, c);

    if(root->nodetype == Call)
      fprintf(curfp, "();\n");
    else
      fprintf(curfp, "()");

    bc_free_fieldref(mref);

    return;
  }

  fprintf(curfp, "(");

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
      exit(EXIT_FAILURE);
    }

    bc_gen_load_op(meth, ht->variable->astnode.ident.localvnum, jvm_Object);
  }

  emit_call_arguments(meth, root, adapter);

  mref = get_method_name(root, adapter);

  c = bc_new_methodref(cur_class_file,mref->classname, mref->methodname,
                   mref->descriptor);

  bc_append(meth, jvm_invokestatic, c);

  /*  
   *  Problem here, depends on who called this procedure.
   *  When this is used by the CALL keyword, it works as
   *  written.  When used to create an external function call,
   *  it adds an extra ; and \n to the output.  Might be
   *  able to fix this by checking the nodetype. 
   */

  if(root->nodetype == Call)
    fprintf(curfp, ");\n");
  else
    fprintf(curfp, ")");

  if(gendebug)printf("leaving-call emit\n");
  bc_free_fieldref(mref);
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
emit_call_arguments(JVM_METHOD *meth, AST *root, BOOL adapter)
{
  JVM_METHODREF *mref;

  /* look up the function that we are calling so that we may compare
   * the parameters.
   */

  mref = get_methodref(root);

  if(gendebug)
    printf("Looking up function name %s...%s\n", root->astnode.ident.name,
       mref ? "Found" : "Not found");

  if(mref != NULL)
    emit_call_args_known(meth, root, mref->descriptor, adapter);
  else
    emit_call_args_unknown(meth, root);

  bc_free_fieldref(mref);
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
emit_call_args_known(JVM_METHOD *meth, AST *root, char *desc, BOOL adapter)
{
  char *com_prefix, *dptr;
  AST *temp;

  if(gendebug)
    printf("emit_call_args_known: desc = '%s'\n", desc);

  temp = root->astnode.ident.arraylist;
  dptr = bc_next_desc_token(desc);

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
       (type_lookup(cur_array_table, temp->astnode.ident.name)!=NULL))
    {
      arrayacc_arg_emit(meth, temp, dptr, adapter);
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
      arrayref_arg_emit(meth, temp, dptr);
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
      scalar_arg_emit(meth, temp, dptr, com_prefix);
    }
    else if(omitWrappers && (temp->nodetype == Constant))
    {
      if(isPassByRef_desc(dptr) || (dptr[0] == '['))
      {
        int c;

        fprintf(curfp,"new %s(", 
           wrapper_returns[get_type_from_field_desc(dptr)]);

        c = cp_find_or_insert(cur_class_file,CONSTANT_Class,
              full_wrappername[temp->vartype]);

        bc_append(meth, jvm_new,c);
        bc_append(meth, jvm_dup);

        c = bc_new_methodref(cur_class_file,full_wrappername[temp->vartype],
               "<init>", wrapper_descriptor[temp->vartype]);

        expr_emit(meth, temp);
        fprintf(curfp,")");

        bc_append(meth, jvm_invokespecial, c);
      }
      else
        expr_emit(meth, temp);
    }
    else if(
      ((temp->nodetype == Identifier) &&
       (temp->astnode.ident.arraylist == NULL) )
       || (temp->nodetype == Constant) )
    {
      expr_emit(meth, temp);
    }
    else if(temp->nodetype != EmptyArgList)
    {
      wrapped_arg_emit(meth, temp, dptr);
    }
   
    /* if this arg is an array, then skip an extra token to compensate
     * for the additional integer offset arg.
     */

    if(dptr[0] == '[')
      dptr = bc_next_desc_token(dptr);

    dptr = bc_next_desc_token(dptr);

    if(temp->nextstmt != NULL)
      fprintf(curfp, ",");

    f2jfree(com_prefix, strlen(com_prefix)+1);
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
arrayacc_arg_emit(JVM_METHOD *meth, AST *temp, char *dptr, BOOL adapter)
{
  BOOL isarg, isext;
  struct var_info *vtemp;

  isarg = type_lookup(cur_args_table, temp->astnode.ident.name) != NULL;

  if(gendebug)
    printf("arrayacc_arg_emit() %s - %s\n", temp->astnode.ident.name, dptr);

  vtemp = push_array_var(meth, temp);

  if(dptr[0] == '[')     /* it is expecting an array */
  {

    func_array_emit(meth,  temp->astnode.ident.arraylist,
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

    func_array_emit(meth, temp->astnode.ident.arraylist, 
      temp->astnode.ident.name, isarg, isext);

    if(!isext)
      bc_gen_array_load_op(meth, jvm_data_types[temp->vartype]);
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
arrayref_arg_emit(JVM_METHOD *meth, AST *temp, char *dptr)
{

  if(dptr[0] == '[')     /* it is expecting an array */
  {
    if(gendebug)
      printf("expecting array\n");

    expr_emit(meth, temp);
  }
  else
  {
    struct var_info *vtemp;

    if(gendebug)
      printf("NOT expecting array\n");

    vtemp = push_array_var(meth, temp);

    if(omitWrappers && !isPassByRef_desc(dptr)) {
      /* fprintf(curfp,"%s%s[0]",com_prefix, temp->astnode.ident.name); */
      fprintf(curfp,"[0]");
      bc_push_int_const(meth, 0);
      bc_gen_array_load_op(meth, jvm_data_types[temp->vartype]);
    }
    else
    {
      /* in this case, the array has no index and the corresponding
       * parameter is pass-by-reference, so we assume an index of 0
       * which would be the behavior of fortran.
       */

      bc_push_int_const(meth, 0);
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
scalar_arg_emit(JVM_METHOD *meth, AST *temp, char *dptr, char *com_prefix)
{
  if(gendebug) {
    printf("scalar_arg_emit: ");
    printf("name = %s (pass by ref = %s), dptr = %s (pass by ref = %s)\n",
      temp->astnode.ident.name, cgPassByRef(temp->astnode.ident.name)?
      "yes" : "no", dptr, isPassByRef_desc(dptr) ? "yes" : "no");
  }

  if(isPassByRef_desc(dptr) != cgPassByRef(temp->astnode.ident.name))
  {

    if(cgPassByRef(temp->astnode.ident.name)) {
      struct var_info *ainf;

      if(dptr[0] == '[')
        fprintf(curfp,"%s%s",com_prefix,temp->astnode.ident.name);
      else
        fprintf(curfp,"%s%s.val",com_prefix,temp->astnode.ident.name);

      ainf = get_var_info(temp);

      if(dptr[0] == '[')
        pushVar(cur_class_file, meth, temp->vartype, ainf->is_arg, ainf->class, ainf->name,
          ainf->desc, ainf->localvar, FALSE);
      else
        pushVar(cur_class_file, meth, temp->vartype, ainf->is_arg, ainf->class, ainf->name,
          ainf->desc, ainf->localvar, TRUE);

      free_var_info(ainf);
    }
    else if(type_lookup(cur_external_table, temp->astnode.ident.name)) {
      external_emit(meth, temp);
    }
    else
      fprintf(stderr,"Internal error: %s should not be primitive\n",
        temp->astnode.ident.name);
  }
  else
  {
    if( temp->vartype != get_type_from_field_desc(dptr) )
      fprintf(curfp,"(%s) ( ",returnstring[get_type_from_field_desc(dptr)]);

    expr_emit(meth, temp);

    if( temp->vartype != get_type_from_field_desc(dptr) ) {
      fprintf(curfp,")");
      bc_append(meth, typeconv_matrix[temp->vartype]
                               [get_type_from_field_desc(dptr)]);
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
wrapped_arg_emit(JVM_METHOD *meth, AST *temp, char *dptr)
{
  enum returntype vtype = get_type_from_field_desc(dptr);
  int c = 0;

  /* 
   * Otherwise, use wrappers.
   */
  if(omitWrappers) {
    if(isPassByRef_desc(dptr)) {
      fprintf(curfp,"new %s(", wrapper_returns[vtype]);
      c = cp_find_or_insert(cur_class_file,CONSTANT_Class,
            full_wrappername[temp->vartype]);

      bc_append(meth, jvm_new,c);
      bc_append(meth, jvm_dup);

      c = bc_new_methodref(cur_class_file,full_wrappername[temp->vartype],
             "<init>", wrapper_descriptor[temp->vartype]);
    }
  }
  else
  {
    fprintf(curfp,"new %s(", wrapper_returns[vtype]);
    c = cp_find_or_insert(cur_class_file,CONSTANT_Class,
          full_wrappername[temp->vartype]);

    bc_append(meth, jvm_new,c);
    bc_append(meth, jvm_dup);

    c = bc_new_methodref(cur_class_file,full_wrappername[temp->vartype], 
           "<init>", wrapper_descriptor[temp->vartype]);
  }

  if(gendebug) {
    printf("emitting wrapped expr...\n");
    printf("   wrapper type is %s\n",wrapper_returns[vtype]);
    printf("   data type is %s\n",returnstring[temp->vartype]);
  }

  /* emit a cast if necessary */

  if( temp->vartype != vtype )
    fprintf(curfp,"(%s) ( ",returnstring[vtype]);

  expr_emit(meth, temp);

  if( temp->vartype != vtype ) {
    fprintf(curfp,")");
    bc_append(meth, typeconv_matrix[temp->vartype][vtype]);
  }

  if(omitWrappers) {
    if(isPassByRef_desc(dptr)) {
      fprintf(curfp,")");
      bc_append(meth, jvm_invokespecial, c);
    }
  }
  else
  {
    fprintf(curfp,")");
    bc_append(meth, jvm_invokespecial, c);
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
emit_call_args_unknown(JVM_METHOD *meth, AST *root)
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
      expr_emit(meth, temp);
    }
    else
    {
      if(omitWrappers) {
        expr_emit(meth, temp);
      }
      else
      {
        fprintf(curfp,"new %s(", wrapper_returns[temp->vartype]);
        expr_emit(meth, temp);
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
  JVM_METHODREF *mtmp;
  AST *temp;
  char *dptr, *current_descriptor;

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
    current_descriptor = hashtemp->variable->astnode.source.descriptor;
  else if((mtmp=find_method(root->astnode.ident.name,descriptor_table))!=NULL)
    current_descriptor = mtmp->descriptor;
  else 
    return 0;

  /* if for some reason current_descriptor is null, just return false now */
  if(!current_descriptor)
    return 0;

  if(gendebug)
    printf("needs_adapter: got descriptor '%s'\n", current_descriptor);

  dptr = bc_next_desc_token(current_descriptor);

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
    else {
      if((temp->nodetype == Identifier) && 
        type_lookup(cur_array_table, temp->astnode.ident.name) &&
        (dptr[0] != '['))
           return 1;
    }

      /*
       * if the arg is an identifier  AND
       *    it is in the array table  AND
       *    the function is expecting an array AND
       *    the data types are different
       */
    if((temp->nodetype == Identifier) && 
       type_lookup(cur_array_table, temp->astnode.ident.name) &&
       (dptr[0] == '[') && (get_type_from_field_desc(dptr+1) != temp->vartype))
    {
      fprintf(stderr, "Warning: in unit '%s', in call to '%s':\n", 
          unit_name, root->astnode.ident.name);
      fprintf(stderr, "    Array argument '%s' has wrong type.\n", 
          temp->astnode.ident.name);
      fprintf(stderr, "    A dummy array of the correct type will be passed.\n");
      fprintf(stderr, "    This should be ok for passing workspace arrays.\n");
      fprintf(stderr, "    Otherwise, there could be problems.\n");
      return 1;
    }

    /*
     * otherwise...
     * if the arg is NOT in the array table  AND
     *    the function IS expecting an array
     */
    if( ! type_lookup(cur_array_table, temp->astnode.ident.name)  &&
          dptr[0] == '[')
      return 1;

    /* consume the offset arg if necessary */
    if(dptr[0] == '[')
      dptr = bc_next_desc_token(dptr);
    dptr = bc_next_desc_token(dptr);
  }

  if(gendebug)
    printf("needs_adapter:returning 0\n");

  return 0;
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
assign_emit(JVM_METHOD *meth, AST * root)
{
  enum returntype ltype, rtype;
  int c;
  HASHNODE *hashtemp;

  /* this used to be a pretty simple procedure:
   *    emit LHS
   *    print = 
   *    emit RHS
   * and that was it.  but it turns out that Fortran doesn't really
   * care much if the LHS and RHS are different types.  However, Java
   * doesn't like that, so we have to insert the appropriate cast or
   * conversion if the types do not agree.
   */
 
  hashtemp = type_lookup(cur_type_table, root->astnode.assignment.lhs->astnode.ident.name);
     if(hashtemp)
        root->astnode.assignment.lhs->vartype = hashtemp->variable->vartype;
  hashtemp = type_lookup(cur_type_table, root->astnode.assignment.rhs->astnode.ident.name);
     if(hashtemp)
       root->astnode.assignment.rhs->vartype = hashtemp->variable->vartype;

  ltype = root->astnode.assignment.lhs->vartype;
  rtype = root->astnode.assignment.rhs->vartype;

  if(gendebug) {
    printf("## ## codegen: ltype = %s (%d)\n",returnstring[ltype], ltype);
    printf("## ## codegen: rtype = %s (%d)\n",returnstring[rtype], rtype);
    printf("## ##   lhs name '%s' in equiv table?  %s\n", 
      root->astnode.assignment.lhs->astnode.ident.name,
      type_lookup(cur_equiv_table, 
        root->astnode.assignment.lhs->astnode.ident.name) ? "yes" : "no");
  }

  /* handle lhs substring operations elsewhere */
  if(root->astnode.assignment.lhs->nodetype == Substring) {
    substring_assign_emit(meth, root);
  }
  else if((root->astnode.assignment.lhs->vartype == String) 
    && root->astnode.assignment.lhs->astnode.ident.arraylist 
    && !root->astnode.assignment.lhs->astnode.ident.arraylist->nextstmt 
    && !type_lookup(cur_array_table, root->astnode.assignment.lhs->astnode.ident.name))
  {
     /* this handles cases like:
      *   character a(1)
      *   a(1) = 'x'
      * which technically isn't a substring operation, but we treat it as such.
      */
     root->astnode.assignment.lhs->astnode.ident.startDim[1] = 
            root->astnode.assignment.lhs->astnode.ident.arraylist;
     substring_assign_emit(meth, root);
  }
  else {
    name_emit(meth, root->astnode.assignment.lhs);
    fprintf(curfp, " = ");

    if(ltype != rtype)    /* lhs and rhs have different types */
    {

      if((ltype != String) && ((rtype == String)||(rtype==Character)))
      {
        /* non-String = String */
        fprintf(curfp,"%s.valueOf(",java_wrapper[ltype]);
        expr_emit(meth, root->astnode.assignment.rhs);
        fprintf(curfp,").%sValue()",returnstring[ltype]);

        c = bc_new_methodref(cur_class_file,numeric_wrapper[ltype], "valueOf",
                        wrapper_valueOf_descriptor[ltype]);

        bc_append(meth, jvm_invokestatic, c);

        c = bc_new_methodref(cur_class_file,numeric_wrapper[ltype], 
                         numericValue_method[ltype],
                        numericValue_descriptor[ltype]);

        bc_append(meth, jvm_invokevirtual, c);
      }
      else if((ltype == Logical) && (rtype != String) )
      {
        JVM_CODE_GRAPH_NODE *if_node = NULL, *goto_node = NULL, 
            *iconst_node = NULL, *next_node = NULL;

        /* boolean = numeric value */
        expr_emit(meth, root->astnode.assignment.rhs);
        fprintf(curfp," == 0 ? false : true");
        if(rtype == Integer) {
          if_node = bc_append(meth, jvm_ifeq);
          bc_append(meth, jvm_iconst_1);
          goto_node = bc_append(meth, jvm_goto);
          iconst_node = bc_append(meth, jvm_iconst_0);
        }
        else if(rtype == Float) {
          bc_append(meth, jvm_fconst_0);
          bc_append(meth, jvm_fcmpl);
          if_node = bc_append(meth, jvm_ifne);
          bc_append(meth, jvm_iconst_0);
          goto_node = bc_append(meth, jvm_goto);
          iconst_node = bc_append(meth, jvm_iconst_1);
        }
        else if(rtype == Double) {
          bc_append(meth, jvm_dconst_0);
          bc_append(meth, jvm_dcmpl);
          if_node = bc_append(meth, jvm_ifne);
          bc_append(meth, jvm_iconst_0);
          goto_node = bc_append(meth, jvm_goto);
          iconst_node = bc_append(meth, jvm_iconst_1);
        }
        else
          fprintf(stderr,"WARNING: unsupported cast.\n");

        bc_set_branch_target(if_node, iconst_node);

        /* create a dummy instruction node following the iconst so that
         * we have a branch target for the goto statement.  it'll be
         * removed later.
         */
        next_node = bc_append(meth, jvm_xxxunusedxxx);
        bc_set_branch_target(goto_node, next_node);
      }
      else
      {
        if(typeconv_matrix[rtype][ltype] == jvm_nop) {
          if((ltype != String && ltype != Character) ||
             (rtype != String && rtype != Character))
            fprintf(stderr,"WARNING: unable to handle cast (%s->%s)!\n",
              returnstring[rtype], returnstring[ltype]);
        }

        /* numeric value = numeric value of some other type */
        fprintf(curfp,"(%s)(",returnstring[ltype]);
        expr_emit(meth, root->astnode.assignment.rhs);
        fprintf(curfp,")");
        bc_append(meth, typeconv_matrix[rtype][ltype]);
      }
    }
    else   /* lhs and rhs have same types, everything is cool */
      expr_emit(meth, root->astnode.assignment.rhs);
  }

  LHS_bytecode_emit(meth, root);
  if(gendebug)printf("leaving-assign emit\n");
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
LHS_bytecode_emit(JVM_METHOD *meth, AST *root)
{
  char *name, *class, *desc, *com_prefix;
  HASHNODE *isArg, *typenode, *ht;
  int c;

  name = root->astnode.assignment.lhs->astnode.ident.name;

  if((typenode = type_lookup(cur_type_table, name)) != NULL)
    desc = getVarDescriptor(typenode->variable);
  else
    desc = "asdf";

  /* get the name of the common block class file, if applicable */

  com_prefix = get_common_prefix(name);

  isArg = type_lookup(cur_args_table,name);

  if(com_prefix[0] != '\0')
  {
    char *idx;

    /* if this is a COMMON variable, find out the merged
     * name, if any, that we should use instead.  Names are
     * merged when different declarations of a common
     * block use different variable names.
     */

    ht = type_lookup(cur_type_table,name);
    if(ht == NULL)
      fprintf(stderr,"assign_emit:Cant find %s in type_table\n", name);
    else if(ht->variable->astnode.ident.merged_name != NULL)
      name = ht->variable->astnode.ident.merged_name;

    class = strdup(com_prefix);
    while((idx = strchr(class, '.')) != NULL )
      *idx = '/';
    class[strlen(class)-1] = '\0';
  }
  else {
    /* want to be able to free() class later, so we must assign malloc'd
     * memory to it in both cases.
     */
    class = strdup(cur_filename);
  } 

  name = getMergedName(root->astnode.assignment.lhs);
  desc = getMergedDescriptor(root->astnode.assignment.lhs,
            root->astnode.assignment.lhs->vartype);

  if(gendebug) {
    printf("in assign_emit, class = %s, name = %s, desc = %s\n",
      class, name, desc);
    printf("     merged name = '%s'\n",
      getMergedName(root->astnode.assignment.lhs));
    printf("     in equiv table?  %s\n", 
      type_lookup(cur_equiv_table, name) ? "yes" : "no");
  }
  
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
      if(gendebug) {
        printf("generating LHS...\n");
        printf("lhs descriptor = %s\n",desc);
        printf("isArg = %s\n",isArg?"Yes":"No");
        printf("local var #%d\n",
          root->astnode.assignment.lhs->astnode.ident.localvnum);
      }

      storeVar(cur_class_file, meth, root->astnode.assignment.lhs->vartype, 
           isArg ? TRUE : FALSE, class, name, desc,
           typenode->variable->astnode.ident.localvnum, FALSE);
    }
    else {
      int vt = root->astnode.assignment.lhs->vartype;
      /* this is a wrapped primitive.  the objectref and value should
       * already be sitting on the stack, so now we generate a putfield
       * instruction.
       */
      c = bc_new_fieldref(cur_class_file, full_wrappername[vt], "val", 
             val_descriptor[vt]);
      bc_append(meth, jvm_putfield, c);
    }
  }
  else {
    /* the LHS is an array access.  currently the stack holds a reference
     * to the array, the array index, and the RHS expression.  all we need
     * to do now is generate an array store instruction (e.g. iastore).
     */
    bc_gen_array_store_op(meth, jvm_data_types[root->astnode.assignment.lhs->vartype]);
  }

  f2jfree(com_prefix, strlen(com_prefix)+1);
  f2jfree(class, strlen(class)+1);
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
substring_assign_emit(JVM_METHOD *meth, AST *root)
{
  AST *lhs = root->astnode.assignment.lhs;
  AST *rhs = root->astnode.assignment.rhs;
  int c, single_sub = 0;

  if(gendebug)
    printf("substring_assign_emit\n");

  /* check if this is a single character array reference, e.g.:
   *    character x(10)
   *    x(3) = 'f'
   */
  if((lhs->astnode.ident.startDim[0] == NULL) &&
     (lhs->astnode.ident.endDim[0] == NULL) &&
     (lhs->astnode.ident.startDim[1] != NULL))
    single_sub = 1;

  lhs->nodetype = Substring;

  name_emit(meth, lhs);

  fprintf(curfp,"= Util.stringInsert("); 

  /* we want to call name_emit() on lhs again, but in this
   * case we don't want it treated like an lvalue, so we'll
   * just set root->astnode.assignment.lhs = NULL here
   * and call scalar_emit() directly instead.
   */
  root->astnode.assignment.lhs = NULL;
  scalar_emit(meth, lhs, NULL);
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
     *    c = cp_find_or_insert(cur_class_file,CONSTANT_Class,
     *              "java/lang/Character");
     *
     *    bc_append(jvm_new,c);
     *    bc_append(jvm_dup);
     *
     *    c = bc_new_methodref(cur_class_file,"java/lang/Character",
     *            "<init>", "(C)V");
     *
     *    fprintf(curfp,"new Character(");
     *    expr_emit(rhs);
     *    bc_append(jvm_invokespecial, c);
     *    fprintf(curfp,").toString(),");
     *    c = bc_new_methodref(cur_class_file,"java/lang/Character", "toString",
     *                     "()Ljava/lang/String;");
     *    bc_append(jvm_invokestatic, c);
     */

    /* code above is broken, use code for STring */
    expr_emit(meth, rhs);
    fprintf(curfp,",");
  }
  else if(rhs->vartype == String)
  {
    expr_emit(meth, rhs);
    fprintf(curfp,",");
  }
  else
  {
    fprintf(curfp,"%s.toString(", java_wrapper[rhs->vartype]);
    expr_emit(meth, rhs);
    c = bc_new_methodref(cur_class_file,numeric_wrapper[rhs->vartype],
                     "toString", toString_descriptor[rhs->vartype]);
    bc_append(meth, jvm_invokestatic, c);
    fprintf(curfp,"),");
  }

  if(single_sub) {
    expr_emit(meth, lhs->astnode.ident.startDim[1]);
  }
  else {
    if(lhs->astnode.ident.startDim[0])
      expr_emit(meth, lhs->astnode.ident.startDim[0]);
    else
      emit_default_substring_start(meth, lhs);
    fprintf(curfp,",");
    if(lhs->astnode.ident.endDim[0])
      expr_emit(meth, lhs->astnode.ident.endDim[0]);
    else
      emit_default_substring_end(meth, lhs);
  }

  fprintf(curfp,")");

  if(single_sub)
    c = bc_new_methodref(cur_class_file, UTIL_CLASS, "stringInsert", SINGLE_INS_DESC);
  else
    c = bc_new_methodref(cur_class_file, UTIL_CLASS, "stringInsert", INS_DESC);
  bc_append(meth, jvm_invokestatic, c);
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
  return( *( (int *) dl_val(dl_last(l)) ) );
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

  return( (AST *) dl_val(dl_last(l)) );
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
  JVM_METHODREF *tmp;
  AST *ptr;
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
        if(!adapter_insert_from_descriptor(node,ptr,
               ht->variable->astnode.source.descriptor))
        {
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

BOOL
adapter_insert_from_descriptor(AST *node, AST *ptr, char *desc)
{
  int this_arg_is_arrayacc, other_arg_is_arrayacc, i;
  int this_arg_is_scalar, other_arg_is_scalar;
  AST *this_call, *other_call;
  BOOL diff;
  char *dptr;

  if(gendebug)
    printf("adapter_insert_from_descriptor: desc = '%s'\n", desc);

  this_call = node->astnode.ident.arraylist;
  other_call = ptr->astnode.ident.arraylist;

  dptr = bc_next_desc_token(desc);

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
          /* (this_call->astnode.ident.arraylist != NULL) && */
          type_lookup(cur_array_table, this_call->astnode.ident.name);

    other_arg_is_arrayacc = (other_call->nodetype == Identifier) &&
          /* (other_call->astnode.ident.arraylist != NULL) && */
          type_lookup(cur_array_table, other_call->astnode.ident.name);

    if((dptr[0] == 'L') &&
        (this_arg_is_arrayacc != other_arg_is_arrayacc ))
    {
      diff = TRUE;
    }

    this_arg_is_scalar = !type_lookup(cur_array_table, 
        this_call->astnode.ident.name);
    other_arg_is_scalar = !type_lookup(cur_array_table, 
        other_call->astnode.ident.name);

    if((dptr[0] == '[') && (this_arg_is_scalar != other_arg_is_scalar ))
    {
      diff = TRUE;
    }

    other_call = other_call->nextstmt;

    dptr = bc_next_desc_token(dptr);
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
  char *tmpdesc, *ret_desc, *cur_name = NULL, *cur_desc=NULL;
  JVM_METHOD *adapter_method;
  HASHNODE *hashtemp;
  JVM_METHODREF *mref;
  Dlist p;
  AST *cval;

  dl_traverse(p,adapter_list)
  {
    cval = (AST *)dl_val(p);

    cur_name=(char *)f2jrealloc(cur_name,strlen(cval->astnode.ident.name)+10);

    strcpy(cur_name, cval->astnode.ident.name);
    strcat(cur_name, "_adapter");

    adapter_method = bc_new_method(cur_class_file, cur_name, NULL, 
       F2J_ADAPTER_ACC);

    hashtemp = type_lookup(function_table, cval->astnode.ident.name);

    if(hashtemp) {
      char *tempname;

      mref = (JVM_METHODREF *)f2jalloc(sizeof(JVM_METHODREF));

      tmpdesc = get_adapter_desc(hashtemp->variable->astnode.source.descriptor,
                     cval->astnode.ident.arraylist);

      if(hashtemp->variable->nodetype == Function)
        ret_desc = 
           field_descriptor[hashtemp->variable->astnode.source.returns][0];
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

      mref->classname = bc_get_full_classname(tempname, package_name);
      mref->methodname = strdup(
         hashtemp->variable->astnode.source.name->astnode.ident.name);
      mref->descriptor = strdup(hashtemp->variable->astnode.source.descriptor);

      adapter_emit_from_descriptor(adapter_method, mref, cval);

      bc_free_fieldref(mref);
      f2jfree(tmpdesc, strlen(tmpdesc)+1);
      f2jfree(tempname, strlen(tempname)+1);
    }
    else {
      if(gendebug)
        printf("looking up descriptor for %s\n",cval->astnode.ident.name);

      mref = find_method(cval->astnode.ident.name, descriptor_table);

      if(mref) {
        char *ret = get_return_type_from_descriptor(mref->descriptor);

        if(gendebug)
          printf("--- ret is '%s'\n", ret);

        if(ret[0] == 'V')
          ret_desc = "V";
        else
          ret_desc = field_descriptor[get_type_from_field_desc(ret)][0];

        /* tmpdesc = get_desc_from_arglist(cval->astnode.ident.arraylist); */
        tmpdesc = get_adapter_desc(mref->descriptor,
                       cval->astnode.ident.arraylist);

        cur_desc = (char *)f2jrealloc(cur_desc, strlen(tmpdesc) +
          strlen(ret_desc) + 10);

        strcpy(cur_desc,"(");
        strcat(cur_desc,tmpdesc);
        strcat(cur_desc,")");
        strcat(cur_desc,ret_desc);

        adapter_emit_from_descriptor(adapter_method, mref, cval);

        f2jfree(tmpdesc, strlen(tmpdesc)+1);
        f2jfree(ret, strlen(ret)+1);
      }
      else {
        fprintf(stderr,"Could not generate adapter for '%s'\n",
           cval->astnode.ident.name);
 
        /* assume that since cur_name was already allocated strlen(var)+10
         * bytes and "BAD_ADAP" requires less than 10 bytes, there's no need
         * to realloc here.  but if we hit this case, then cur_desc may not
         * have any memory allocated yet, so call realloc here.
         */

        strcpy(cur_name, "BAD_ADAP");

        cur_desc=(char *)f2jrealloc(cur_name,4);
        strcpy(cur_desc, "()V");
      }
    }

    fprintf(indexfp,"%s:%s:%s\n",cur_filename, cur_name, cur_desc);
    
    /* Now we know the descriptor for this adapter, so set the field in 
     * the method struct accordingly.
     */

    bc_set_method_descriptor(adapter_method, cur_desc);
  }

  if(cur_desc)
    f2jfree(cur_desc, strlen(cur_desc)+1);
  if(cur_name)
    f2jfree(cur_name, strlen(cur_name)+1);
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
adapter_emit_from_descriptor(JVM_METHOD *meth, JVM_METHODREF *mref, AST *node)
{
  enum returntype ret_type;
  char *ret;
  int lv_temp, retval_varnum = 0;

  ret_type = Integer;  /* init just to quiet a compiler warning */

  fprintf(curfp,"// adapter for %s%s\n", 
    node->astnode.ident.name, mref->descriptor);

  ret = get_return_type_from_descriptor(mref->descriptor);

  if((ret == NULL) || (ret[0] == '[') || (ret[0] == 'L')) {
    fprintf(stderr,"Not expecting NULL, reference, or array return type ");
    fprintf(stderr,"for adapter '%s'\n", node->astnode.ident.name);
    f2jfree(ret,strlen(ret)+1);
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

  adapter_args_emit_from_descriptor(meth, node->astnode.ident.arraylist,
    mref->descriptor);

  fprintf(curfp,")\n{\n");

  lv_temp = meth->cur_local_number;

  adapter_temps_emit_from_descriptor(meth, node->astnode.ident.arraylist,
     mref->descriptor);

  adapter_methcall_emit_from_descriptor(meth, node, lv_temp, mref, ret);

  if(ret[0] != 'V') {
    retval_varnum = bc_get_next_local(meth, jvm_data_types[ret_type]);
    bc_gen_store_op(meth, retval_varnum, jvm_data_types[ret_type]);
  }

  adapter_assign_emit_from_descriptor(meth, node->astnode.ident.arraylist,
     lv_temp, mref->descriptor);

  if(ret[0] != 'V')
  {
    fprintf(curfp,"\nreturn %s_retval;\n",
      node->astnode.ident.name);

    bc_gen_load_op(meth, retval_varnum, jvm_data_types[ret_type]);
    bc_append(meth, return_opcodes[ret_type]);
  }
  else
    bc_append(meth, jvm_return);

  fprintf(curfp,"}\n\n");
  f2jfree(ret,strlen(ret)+1);
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
adapter_args_emit_from_descriptor(JVM_METHOD *meth, AST *arg, 
  char *desc)
{
  enum returntype ctype;
  char *dptr;
  int i, lvnum;

  dptr = bc_next_desc_token(desc);

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

    if(gendebug)
      printf("adapter_args.. arg=%s dptr = '%s'\n",
        arg->astnode.ident.name,dptr);

    if(dptr[0] == '[') {
      if(type_lookup(cur_array_table,arg->astnode.ident.name)) {
        if(get_type_from_field_desc(dptr+1) == arg->vartype) {
          fprintf(curfp,"%s [] arg%d , int arg%d_offset ",
            returnstring[get_type_from_field_desc(dptr+1)], i, i);
          lvnum += 2;
        }
        else {
          fprintf(curfp,"%s [] arg%d , int arg%d_offset ",
            returnstring[arg->vartype], i, i);
          lvnum += 2;
        }
      }
      else {
        fprintf(curfp,"%s arg%d ",
          wrapper_returns[get_type_from_field_desc(dptr+1)], i);

        lvnum++;
      }
      
      /* consume the offset arg */
      dptr = bc_next_desc_token(dptr);
    }
    else if((arg->nodetype == Identifier) &&
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

    dptr = bc_next_desc_token(dptr);

    if(arg->nextstmt != NULL)
      fprintf(curfp,",");
  }

  /* set current local variable number to compensate for the method's
   * arguments. 
   */
  bc_set_cur_local_num(meth, lvnum);
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
adapter_tmp_assign_emit(JVM_METHOD *meth, int arglocal, enum returntype argtype)
{
  int c;
  char *classname, *desc;

  classname = full_wrappername[argtype]; 
  desc = wrapper_descriptor[argtype];

  c = cp_find_or_insert(cur_class_file,CONSTANT_Class, classname);

  bc_append(meth, jvm_new,c);
  bc_append(meth, jvm_dup);

  /* emit arg%d[arg%d_offset] */
  bc_gen_load_op(meth, arglocal, jvm_Object);
  bc_gen_load_op(meth, arglocal + 1, jvm_Int);
  bc_gen_array_load_op(meth, jvm_data_types[argtype]);

  c = bc_new_methodref(cur_class_file, classname, "<init>", desc);

  bc_append(meth, jvm_invokespecial, c);

  /* now assign value to next local */
  bc_gen_store_op(meth, bc_get_next_local(meth, jvm_Object), jvm_Object);
}

/*****************************************************************************
 *                                                                           *
 * adapter_tmp_array_assign_emit                                             *
 *                                                                           *
 * this function generates the bytecode for the assignment to a temp         *
 * variable in the adapter.   for example:                                   *
 *          int [] _f2j_tmp3 = new int[1];                                   *
 *                                                                           *
 *****************************************************************************/

void
adapter_tmp_array_assign_emit(JVM_METHOD *meth, int arglocal, enum returntype argtype)
{
  int c;

  bc_append(meth, jvm_iconst_1);
  newarray_emit(meth, argtype);
  bc_append(meth, jvm_dup);
  bc_append(meth, jvm_iconst_0);
  bc_gen_load_op(meth, arglocal, jvm_Object);
  c = bc_new_fieldref(cur_class_file, full_wrappername[argtype], "val",
         val_descriptor[argtype]);
  bc_append(meth, jvm_getfield, c);
  bc_gen_array_store_op(meth, jvm_data_types[argtype]);
  bc_gen_store_op(meth, bc_get_next_local(meth, jvm_Object), jvm_Object);
}

/*****************************************************************************
 *                                                                           *
 * adapter_tmp_array_new_emit                                                *
 *                                                                           *
 * this function generates the bytecode for the assignment to a temp         *
 * variable in the adapter.   for example:                                   *
 *          int [] _f2j_tmp3 = new int[arg3.length];                         *
 *                                                                           *
 *****************************************************************************/

void
adapter_tmp_array_new_emit(JVM_METHOD *meth, int arglocal, enum returntype argtype)
{
  bc_gen_load_op(meth, arglocal, jvm_Object);
  bc_append(meth, jvm_arraylength);
  newarray_emit(meth, argtype);
  bc_gen_store_op(meth, bc_get_next_local(meth, jvm_Object), jvm_Object);
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
adapter_temps_emit_from_descriptor(JVM_METHOD *meth, AST *arg, char *desc)
{
  char *dptr, *wrapper;
  int i;

  dptr = bc_next_desc_token(desc);

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
          adapter_tmp_assign_emit(meth, arg->astnode.ident.localvnum, 
            get_type_from_field_desc(dptr));
        }
      }
      else
      {
        fprintf(curfp,"%s _f2j_tmp%d = new %s(arg%d[arg%d_offset]);\n", 
          wrapper, i, wrapper, i, i);
        adapter_tmp_assign_emit(meth, arg->astnode.ident.localvnum, 
          get_type_from_field_desc(dptr));
      }

      f2jfree(wrapper, strlen(wrapper)+1);
    }
    else if(dptr[0] == '[') {
      if(! type_lookup(cur_array_table,arg->astnode.ident.name)) {
        enum returntype ctype = get_type_from_field_desc(dptr);

        fprintf(curfp,"%s [] _f2j_tmp%d = { arg%d.val };\n",
           returnstring[ctype], i, i);

        adapter_tmp_array_assign_emit(meth, arg->astnode.ident.localvnum,
          ctype);
      }
      else if(get_type_from_field_desc(dptr+1) != arg->vartype) {
        enum returntype ctype = get_type_from_field_desc(dptr);

        fprintf(curfp,"%s [] _f2j_tmp%d = new %s[arg%d.length];\n",
           returnstring[ctype], i, returnstring[ctype], i);

        adapter_tmp_array_new_emit(meth, arg->astnode.ident.localvnum,
          ctype);
      }

      dptr = bc_next_desc_token(dptr);
    }

    dptr = bc_next_desc_token(dptr);
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
adapter_methcall_emit_from_descriptor(JVM_METHOD *meth, AST *node, int lv_temp,
  JVM_METHODREF *mref, char *ret)
{
  char *tempname, *dptr;
  int c;
  AST *arg;
  int i;

  if((mref->classname != NULL) && (strlen(mref->classname) > 0))
    tempname = char_substitution(mref->classname, '/', '.');
  else {
    tempname = strdup( node->astnode.ident.name );
    *tempname = toupper(*tempname);
  }

  if(ret[0] == 'V')
    fprintf(curfp,"\n%s.%s(",tempname,  node->astnode.ident.name );
  else
  {
    fprintf(curfp,"%s %s_retval;\n\n", ret,
        node->astnode.ident.name);

    fprintf(curfp,"%s_retval = %s.%s(", node->astnode.ident.name,
       tempname,  node->astnode.ident.name );
  }

  dptr = bc_next_desc_token(mref->descriptor);
  arg = node->astnode.ident.arraylist;

  for(i = 0; arg != NULL ; arg = arg->nextstmt, i++)
  {
    if(dptr == NULL)
      break;

    lv_temp = adapter_methcall_arg_emit(meth, arg, i, lv_temp, dptr);

    /* skip extra field desc to compensate for offset arg */
    if(dptr[0] == '[')
      dptr = bc_next_desc_token(dptr);

    dptr = bc_next_desc_token(dptr);

    if(arg->nextstmt != NULL)
      fprintf(curfp,",");
  }

  fprintf(curfp,");\n\n");

  c = bc_new_methodref(cur_class_file, mref->classname, 
            mref->methodname,mref->descriptor);
 
  bc_append(meth, jvm_invokestatic, c);

  f2jfree(tempname, strlen(tempname)+1);
}

/*****************************************************************************
 *                                                                           *
 * adapter_methcall_arg_emit                                                 *
 *                                                                           *
 * emit the argument to an adapter methodcall.                               *
 *                                                                           *
 *****************************************************************************/

int
adapter_methcall_arg_emit(JVM_METHOD *meth, AST *arg, int i, int lv, char *dptr)
{
  if((arg->nodetype == Identifier) &&
     /* (arg->astnode.ident.arraylist != NULL) && */
     (type_lookup(cur_array_table,arg->astnode.ident.name) != NULL) &&
     (dptr[0] != '['))
  {
    if(omitWrappers && !isPassByRef_desc(dptr)) {
      fprintf(curfp,"arg%d",i);
      bc_gen_load_op(meth, arg->astnode.ident.localvnum,
          jvm_data_types[get_type_from_field_desc(dptr)]);
    }
    else {
      fprintf(curfp,"_f2j_tmp%d",i);
      bc_gen_load_op(meth, lv++, jvm_Object);
    }
  }
  else if( ! type_lookup(cur_array_table,arg->astnode.ident.name) &&
          (dptr[0] == '['))
  {
    fprintf(curfp,"_f2j_tmp%d, 0",i);
    bc_gen_load_op(meth, lv++, jvm_Object);
    bc_append(meth, jvm_iconst_0);
  }
  else if((arg->nodetype == Identifier) &&
          (type_lookup(cur_array_table,arg->astnode.ident.name) != NULL) &&
          (dptr[0] == '['))
  {
    if(get_type_from_field_desc(dptr+1) == arg->vartype) {
      fprintf(curfp,"arg%d, arg%d_offset",i,i);
      bc_gen_load_op(meth, arg->astnode.ident.localvnum, jvm_Object);
      bc_gen_load_op(meth, arg->astnode.ident.localvnum+1, jvm_Int);
    }
    else {
      fprintf(curfp,"_f2j_tmp%d, arg%d_offset",i,i);
      bc_gen_load_op(meth, lv++, jvm_Object);
      bc_gen_load_op(meth, arg->astnode.ident.localvnum+1, jvm_Int);
    }
  }
  else
  {
    fprintf(curfp,"arg%d",i);
    if(isPassByRef_desc(dptr))
      bc_gen_load_op(meth, arg->astnode.ident.localvnum, jvm_Object);
    else
      bc_gen_load_op(meth, arg->astnode.ident.localvnum,
            jvm_data_types[get_type_from_field_desc(dptr)]);
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
adapter_assign_emit_from_descriptor(JVM_METHOD *meth, AST *arg, int lv_temp, char *desc)
{
  char *dptr;
  int i;

  dptr = bc_next_desc_token(desc);

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
          adapter_assign_emit(meth, i,arg->astnode.ident.localvnum,lv_temp++,dptr);
      }
      else
      {
        adapter_assign_emit(meth, i,arg->astnode.ident.localvnum,lv_temp++,dptr);
      }
    }
    else if(dptr[0] == '[') {

      if( !type_lookup(cur_array_table,arg->astnode.ident.name) )
      {
        adapter_array_assign_emit(meth, i,arg->astnode.ident.localvnum,
          lv_temp++,dptr);
      }
      else if(get_type_from_field_desc(dptr+1) != arg->vartype) {
        lv_temp++;
      }
      
      /* skip extra field desc to compensate for offset arg */

      dptr = bc_next_desc_token(dptr);
    }

    dptr = bc_next_desc_token(dptr);
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
adapter_assign_emit(JVM_METHOD *meth, int i, int argvnum, int lv, char *dptr)
{
  enum returntype vt;
  int c;

  fprintf(curfp,"arg%d[arg%d_offset] = _f2j_tmp%d.val;\n",i,i,i);

  vt = get_type_from_field_desc(dptr);

  bc_gen_load_op(meth, argvnum, jvm_Object);
  bc_gen_load_op(meth, argvnum+1, jvm_Int);

  bc_gen_load_op(meth, lv, jvm_Object);
  c = bc_new_fieldref(cur_class_file, full_wrappername[vt], "val", 
         val_descriptor[vt]);
  bc_append(meth, jvm_getfield, c);

  bc_gen_array_store_op(meth, jvm_data_types[vt]);
}

/*****************************************************************************
 *                                                                           *
 * adapter_array_assign_emit                                                 *
 *                                                                           *
 * emit the assignment back to the wrapper from the array element.           *
 *                                                                           *
 *   arg3.val = _f2j_tmp3[0];                                                *
 *                                                                           *
 *****************************************************************************/

void
adapter_array_assign_emit(JVM_METHOD *meth, int i, int argvnum, int lv, char *dptr)
{
  enum returntype vt;
  int c;

  fprintf(curfp,"arg%d.val = _f2j_tmp%d[0];\n",i,i);

  if(gendebug)
    printf("#@@# calling get_type_from_field_desc(%s) = ", dptr);

  vt = get_type_from_field_desc(dptr);

  if(gendebug)
    printf(" '%s'\n", returnstring[vt]);

  bc_gen_load_op(meth, argvnum, jvm_Object);
  bc_gen_load_op(meth, lv, jvm_Object);
  bc_append(meth, jvm_iconst_0);
  bc_gen_array_load_op(meth, jvm_data_types[vt]);

  c = bc_new_fieldref(cur_class_file, full_wrappername[vt], "val",
         val_descriptor[vt]);
  bc_append(meth, jvm_putfield, c);
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
  char *p;
  int dim;

  for(arg = list; arg != NULL; arg = arg->nextstmt) {
    dim = 0;

    if(omitWrappers) {
      if( arg->nodetype == Identifier ) {
        ht = type_lookup(cur_type_table,arg->astnode.ident.name);
        if(ht) {
          dim = ht->variable->astnode.ident.dim > 0;

          temp_desc = strAppend(temp_desc, 
                          field_descriptor[ht->variable->vartype][dim]);
        }
        else {
          dim = arg->astnode.ident.dim > 0;

          temp_desc = strAppend(temp_desc, 
                          field_descriptor[arg->vartype][dim]);
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
          dim = ht->variable->astnode.ident.dim > 0;

          temp_desc = strAppend(temp_desc, 
              wrapped_field_descriptor[ht->variable->vartype][dim]);
        }
        else {
          dim = arg->astnode.ident.dim > 0;

          temp_desc = strAppend(temp_desc,
              wrapped_field_descriptor[arg->vartype][dim]);
        }
      }
      else if( arg->nodetype == Constant )
        temp_desc = strAppend(temp_desc,
          wrapped_field_descriptor[get_type(arg->astnode.constant.number)][0]);
      else
        temp_desc = strAppend(temp_desc,
          wrapped_field_descriptor[arg->vartype][0]);
    }

    if(dim)
      temp_desc = strAppend(temp_desc, "I");
  }

  p = temp_desc->val;

  f2jfree(temp_desc, sizeof(struct _str));

  return p;
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
emit_invocations()
{
  JVM_METHOD *meth;
  Dlist p, tmplist;
  int count, obj_array_varnum;
  char *cur_name=NULL, *cur_desc=NULL, *tmpdesc;
  int c;
  AST *temp;

  dl_traverse(p,methcall_list) {
    tmplist = (Dlist) dl_val(p);
    
    temp = (AST *) dl_val(dl_first(tmplist));

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

    meth = bc_new_method(cur_class_file, cur_name, cur_desc, 
      F2J_ADAPTER_ACC);

    bc_add_method_exception(meth, "java.lang.reflect.InvocationTargetException");
    bc_add_method_exception(meth, "java.lang.IllegalAccessException");

    count = methcall_arglist_emit(temp);

    fprintf(curfp,")\n throws java.lang.reflect.InvocationTargetException,\n");
    fprintf(curfp,"          java.lang.IllegalAccessException\n{\n"); 

    fprintf(curfp,"Object [] _funcargs = new Object [%d];\n", count);
    fprintf(curfp,"%s _retval;\n", returnstring[temp->vartype]);

    /* create a new object array and store it in the first local var */
    bc_push_int_const(meth, count);
    c = cp_find_or_insert(cur_class_file, CONSTANT_Class, "java/lang/Object");
    bc_append(meth, jvm_anewarray, c);
    obj_array_varnum = bc_get_next_local(meth, jvm_Object);
    bc_gen_store_op(meth, obj_array_varnum, jvm_Object);

    methcall_obj_array_emit(meth, temp, obj_array_varnum);

    fprintf(curfp,
      "_retval = ( (%s) _funcptr.invoke(null,_funcargs)).%sValue();\n",
      java_wrapper[temp->vartype], returnstring[temp->vartype]);

    /* load _funcptr, which should always be local var 0 */
    bc_gen_load_op(meth, 0, jvm_Object);
    bc_append(meth, jvm_aconst_null);
    bc_gen_load_op(meth, obj_array_varnum, jvm_Object);

    c = bc_new_methodref(cur_class_file, METHOD_CLASS, "invoke",
          INVOKE_DESC);
    bc_append(meth, jvm_invokevirtual, c);

    c = cp_find_or_insert(cur_class_file,CONSTANT_Class,
          numeric_wrapper[temp->vartype]);
    bc_append(meth, jvm_checkcast, c);

    c = bc_new_methodref(cur_class_file,numeric_wrapper[temp->vartype], 
                     numericValue_method[temp->vartype],
                     numericValue_descriptor[temp->vartype]);

    bc_append(meth, jvm_invokevirtual, c);

    bc_append(meth, return_opcodes[temp->vartype]);

    fprintf(curfp,"return _retval;\n");
    fprintf(curfp,"}\n"); 

    fprintf(indexfp,"%s:%s:%s\n",cur_filename, cur_name, cur_desc);

    f2jfree(tmpdesc, strlen(tmpdesc)+1);
  }

  if(cur_name) f2jfree(cur_name, strlen(cur_name)+1);
  if(cur_desc) f2jfree(cur_desc, strlen(cur_desc)+1);
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
  int count=0, dim;
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
methcall_obj_array_emit(JVM_METHOD *meth, AST *temp, int lv)
{
  enum returntype rtype;
  HASHNODE *ht;
  int ai = 0, vi = 1, dim;
  AST *arg;

  rtype = Integer;  /* just here to quiet a compiler warning */

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

        arg_array_assign_emit(cur_class_file, meth, lv, ai, vi, rtype);
        vi++;
      }
      else {
        fprintf(curfp,"_arg%d);\n", ai);
        arg_assignment_emit(cur_class_file, meth, lv, ai, vi, TRUE, rtype);
      }
    }
    else
    {
      if(dim > 0) {
        fprintf(curfp," _funcargs[%d] = _arg%d[_arg%d_offset];\n",ai,ai,ai);
        arg_array_assign_emit(cur_class_file, meth, lv, ai, vi, rtype);
        vi++;
      }
      else {
        fprintf(curfp," _funcargs[%d] = _arg%d;\n",ai,ai);
        arg_assignment_emit(cur_class_file, meth, lv, ai, vi, FALSE, rtype);
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
arg_array_assign_emit(JVM_CLASS *cclass, JVM_METHOD *meth,
             int array_vnum, int array_idx, int arg_vnum, enum returntype argtype)
{
  int c;

  bc_gen_load_op(meth, array_vnum, jvm_Object);
  bc_push_int_const(meth, array_idx);

  c = cp_find_or_insert(cclass,CONSTANT_Class,
          numeric_wrapper[argtype]);

  bc_append(meth, jvm_new,c);
  bc_append(meth, jvm_dup);
  bc_gen_load_op(meth, arg_vnum, jvm_Object);
  bc_gen_load_op(meth, arg_vnum + 1, jvm_Int);
  bc_gen_array_load_op(meth, jvm_data_types[argtype]);

  c = bc_new_methodref(cclass, numeric_wrapper[argtype],
           "<init>", wrapper_descriptor[argtype]);
  bc_append(meth, jvm_invokespecial, c);

  bc_gen_array_store_op(meth, jvm_data_types[Object]);
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
arg_assignment_emit(JVM_CLASS *cclass, JVM_METHOD *meth, 
    int array_vnum, int array_idx, int arg_vnum, BOOL wrap, 
    enum returntype argtype)
{
  int c;

  bc_gen_load_op(meth, array_vnum, jvm_Object);
  bc_push_int_const(meth, array_idx);

  if(wrap) {
    c = cp_find_or_insert(cclass,CONSTANT_Class,
            numeric_wrapper[argtype]);

    bc_append(meth, jvm_new,c);
    bc_append(meth, jvm_dup);
    bc_gen_load_op(meth, arg_vnum, jvm_data_types[argtype]);

    c = bc_new_methodref(cclass, numeric_wrapper[argtype],
             "<init>", wrapper_descriptor[argtype]);

    bc_append(meth, jvm_invokespecial, c);
  }
  else
    bc_gen_load_op(meth, arg_vnum, jvm_data_types[argtype]);

  bc_append(meth, jvm_aastore);
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
    
  while((idx = strchr(newstr, from_char)) != NULL )
    *idx = to_char; 

  return newstr;
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
  JVM_CODE_GRAPH_NODE *val;
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
      val = (JVM_CODE_GRAPH_NODE *) tmp->val;
  
      if(!val->visited)
        warn = "(UNVISITED!!)";
      else
        warn = "";

      sprintf(node_label,"%d: %s %s\nstack_pre: %d", val->pc, 
         jvm_opcode[val->op].op, warn, val->stack_depth);

      print_vcg_node(v, val->pc, node_label);
      if((val->next != NULL) && (val->op != jvm_goto)
       && (val->op != jvm_goto_w))
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
 *                                                                           *
 * assign_varnums_to_arguments                                               *
 *                                                                           *
 * This routine numbers the local variables for generating bytecode.         *
 *                                                                           *
 * Horribly kludged routines with massive loop of                            *
 * duplicated code.                                                          *
 *                                                                           *
 * ...cleaned this routine up somewhat.  --kgs 5/5/00                        *
 *                                                                           *
 *****************************************************************************/

int
assign_varnums_to_arguments(AST * root)
{
  AST * locallist;
  HASHNODE * hashtemp, * ht2;
  int localnum = 0;

  /* if root is NULL, this is probably a PROGRAM (no args) */
  if(root == NULL)
    return 1;

  /* This loop takes care of the stuff coming in from the
   * argument list.  
   */
  for(locallist = root ; locallist; locallist = locallist->nextstmt)
  {
    if(gendebug)
      printf("assign_varnums_to_arguments(%s): arg list name: %s, local varnum: %d\n",
         cur_filename, locallist->astnode.ident.name, localnum);

    hashtemp = type_lookup(cur_type_table, locallist->astnode.ident.name);
    if(hashtemp == NULL)
    {
      ht2=type_lookup(cur_args_table, locallist->astnode.ident.name);
      if(ht2) {
        if(gendebug)
          printf("assign_varnums_to_arguments(%s):%s in args table, set lvnum: %d\n",
            cur_filename, locallist->astnode.ident.name, localnum);

        ht2->variable->astnode.ident.localvnum = localnum;
        localnum++;
        continue;
      }
      else {
        fprintf(stderr,"Type table is screwed in assign locals.\n");
        fprintf(stderr,"could not find %s\n", locallist->astnode.ident.name);
        exit(EXIT_FAILURE);
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

    if(gendebug)
      printf("assign_varnums_to_arguments(%s): name: %s, pass by ref: %s\n",
        cur_filename, locallist->astnode.ident.name,
        hashtemp->variable->astnode.ident.passByRef ? "yes" : "no");

    if((hashtemp->variable->vartype == Double ||
        hashtemp->variable->astnode.ident.arraylist != NULL) &&
       (!hashtemp->variable->astnode.ident.passByRef))
      localnum += 2;
    else
      localnum++;

    if(gendebug)
      printf("ARG %s %d\n", hashtemp->variable->astnode.ident.name, 
        hashtemp->variable->astnode.ident.localvnum); 
  }

  return localnum;
} /* Close assign_varnums_to_arguments().  */

/*****************************************************************************
 *                                                                           *
 * print_nodetype                                                            *
 *                                                                           *
 * This is primarily a debugging tool.  Given a node, it returns a           *
 * string containing the node type.                                          *
 *                                                                           *
 *****************************************************************************/

char * 
print_nodetype(AST *root) 
{
  static char temp[100];

  if(root == NULL) {
    return("print_nodetpe: NULL root");
  }

  switch(root->nodetype)
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
    case StmtFuncDecl:
      return("StmtFuncDecl");
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
    case Pause:
      return("Pause");
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
    case UnitExp:
      return("UnitExp");
    case OpenFileSpec:
      return("OpenFileSpec");
    case Open:
      return("Open");
    case Close:
      return("Close");
    case Rewind:
      return("Rewind");
    case Backspace:
      return("Backspace");
    case Endfile:
      return("Endfile");
    case CharExp:
      return("CharExp");
    case FormatOrUnknownSpec:
      return("FormatOrUnknownSpec");
    default:
      sprintf(temp, "print_nodetype(): Unknown Node: %d", root->nodetype);
      return(temp);
  }
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
  enum returntype rt = Integer;
  char * wrap;

  switch(fd[0]) {
    case 'B':
      rt = Integer;
      break;
    case 'C':
      rt = Character;
      break;
    case 'D':
      rt = Double;
      break;
    case 'F':
      rt = Float;
      break;
    case 'I':
      rt = Integer;
      break;
    case 'J':
      rt = Integer;
      break;
    case 'S':
      rt = Integer;
      break;
    case 'Z':
      rt = Logical;
      break;
    case 'V':
      rt = Object; /* no void in the array, so use object instead */ 
      break;
    case '[':
      rt = get_type_from_field_desc(fd+1);
      break;
    case 'L':
      wrap = get_wrapper_from_desc(fd);

      if(!strcmp(wrap, "StringW"))
        rt = String;
      else if(!strcmp(wrap, "complexW"))
        rt = Complex;
      else if(!strcmp(wrap, "intW"))
        rt = Integer;
      else if(!strcmp(wrap, "doubleW"))
        rt = Double;
      else if(!strcmp(wrap, "floatW"))
        rt = Float;
      else if(!strcmp(wrap, "booleanW"))
        rt = Logical;
      else if(!strcmp(wrap, "String"))
        rt = String;
      else if(!strcmp(wrap, "Object"))
        rt = Object;
      else
        fprintf(stderr,"get_type_from_field_desc() hit default case '%s'!!\n",
          fd);

      f2jfree(wrap, strlen(wrap)+1);
      break;
    default:
      fprintf(stderr,"get_type_from_field_desc() hit default case '%s'!!\n",
        fd);
      rt = Integer;
  }

  return rt;
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
  new[(int)(dptr-ls-1)] = '\0';

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
  int isArray = node->astnode.ident.dim > 0;

  if(omitWrappers && !node->astnode.ident.passByRef)
    fdesc = field_descriptor[node->vartype][isArray];
  else
    fdesc = wrapped_field_descriptor[node->vartype][isArray];

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
  char *p;
  int i;

  dptr = bc_next_desc_token(dptr);

  for(i = 0; arg != NULL ; arg = arg->nextstmt, i++)
  {
    if(dptr == NULL) {
      fprintf(stderr,"get_adapter_desc():");
      fprintf(stderr,"mismatch between adapter call and prototype\n");
      break;
    }

    if(dptr[0] == '[') {
      if(!type_lookup(cur_array_table,arg->astnode.ident.name)) {
        temp_desc = strAppend(temp_desc,
            wrapped_field_descriptor[get_type_from_field_desc(dptr+1)][0]);
      }
      else {
        if(arg->vartype == get_type_from_field_desc(dptr+1)) {
          temp_desc = strAppend(temp_desc, 
             field_descriptor[get_type_from_field_desc(dptr+1)][1]);
          temp_desc = strAppend(temp_desc, "I");
        }
        else {
          temp_desc = strAppend(temp_desc, field_descriptor[arg->vartype][1]);
          temp_desc = strAppend(temp_desc, "I");
        }
      }

      dptr = bc_next_desc_token(dptr);
    }
    else if((arg->nodetype == Identifier) && 
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

    dptr = bc_next_desc_token(dptr);
  }

  p = temp_desc->val;

  f2jfree(temp_desc, sizeof(struct _str));

  return p;
}

/*****************************************************************************
 *                                                                           *
 * cast_data_stmt                                                            *
 *                                                                           *
 * function prints a cast for a data statement and returns the token         *
 * vartype to be pushed onto the stack.                                      *
 *                                                                           *
 * called from: data_scalar_emit                                             *
 *****************************************************************************/

int
cast_data_stmt(AST  *LHS, int no_change){
  int tok = no_change;

  if(LHS->vartype == Integer)
      tok = INTEGER;
  else if(LHS->vartype == Float)
      tok = FLOAT;
  else if(LHS->vartype == Double)
      tok = DOUBLE;

  fprintf(curfp, "(%s) ", returnstring[LHS->vartype]);

  return tok;    
}


/**
 ** below are functions that we might want to move to the bytecode library
 ** but the dependency on returntype enum would have to be eliminated.
 **/

/*****************************************************************************
 *                                                                           *
 * pushVar                                                                   *
 *                                                                           *
 * pushes a local variable or field onto the stack.                          *
 *                                                                           *
 *****************************************************************************/

void
pushVar(JVM_CLASS *cclass, JVM_METHOD *meth, enum returntype vt,
               BOOL isArg, char *class, char *name, char *desc, int lv, BOOL deref)
{
  int c;

  if(gendebug) {
    /* printf("in pushvar, vartype is %s\n", returnstring[vt]); */
    printf("               desc is %s\n", desc);
    printf("       local varnum is %d\n", lv);
  }

  if(isArg || (lv != -1)) {
    /* for reference types, always use aload */
    if((desc[0] == 'L') || (desc[0] == '['))
      bc_gen_load_op(meth, lv, jvm_Object);
    else
      bc_gen_load_op(meth, lv, jvm_data_types[vt]);
  }
  else {
    c = bc_new_fieldref(cclass, class, name, desc);
    bc_append(meth, jvm_getstatic, c);
  }

  if(deref) {
    c = bc_new_fieldref(cclass, full_wrappername[vt], "val",
           val_descriptor[vt]);
    bc_append(meth, jvm_getfield, c);
  }
}

/*****************************************************************************
 *                                                                           *
 * storeVar                                                                  *
 *                                                                           *
 * stores a value from the stack to a local variable.                        *
 *                                                                           *
 *****************************************************************************/

void
storeVar(JVM_CLASS *cclass, JVM_METHOD *meth, 
  enum returntype vt, BOOL isArg, char *class, char *name, char *desc, 
  int lv, BOOL deref)
{
  int c;

  if(gendebug) {
    /* printf("in store, vartype is %s\n", returnstring[vt]); */
    printf("             desc is %s\n", desc);
    printf("     local varnum is %d\n", lv);
  }

  if(isArg || (lv != -1)) {
    /* for reference types, always use aload */
    if((desc[0] == 'L') || (desc[0] == '['))
      bc_gen_store_op(meth, lv, jvm_Object);
    else
      bc_gen_store_op(meth, lv, jvm_data_types[vt]);
  }
  else {
    c = bc_new_fieldref(cclass, class, name, desc);
    bc_append(meth, jvm_putstatic, c);
  }

  if(deref) {
    c = bc_new_fieldref(cclass, full_wrappername[vt], "val",
           val_descriptor[vt]);
    bc_append(meth,  jvm_putfield, c);
  }
}

/*****************************************************************************
 *                                                                           *
 * escape_double_quotes                                                      *
 *                                                                           *
 * Adds backslash escapes to strings that are to be emitted in Java source.  *
 * For example, 'string "with" quotes' -> 'string \"with\" quotes'           *
 *                                                                           *
 *****************************************************************************/

char *
escape_double_quotes(char *str)
{
  char *newstr;
  int i, ni;

  newstr = (char *)malloc(strlen(str) * 2 + 1);

  if(!newstr) return NULL;

  ni = 0;

  for(i=0;i<strlen(str);i++) {
    if(str[i] != '"') {
      newstr[ni] = str[i];
      ni++;
    }
    else {
      newstr[ni] = '\\';
      newstr[ni+1] = '"';
      ni += 2;
    }
  }

  newstr[ni] = 0;

  return newstr;
}
