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

#include<stdio.h>
#include<stdlib.h>
#include<string.h>
#include<ctype.h>
#include"f2j.h"
#include"f2jparse.tab.h"
#include"class.h"
#include"constant_pool.h"
#include"codegen.h"

/*****************************************************************************
 * Function prototypes:                                                      *
 *****************************************************************************/

char 
  * strdup ( const char * ),
  * print_nodetype ( AST * ),
  * lowercase ( char * ),
  * methodscan (METHODTAB * , char * ),
  * getVarDescriptor(AST *);

void 
  code_zero_op(enum _opcode),
  code_one_op(enum _opcode, int),
  code_one_op_w(enum _opcode, u2),
  pushConst(AST *),
  pushIntConst(int),
  pushDoubleConst(double),
  pushStringConst(char *),
  pushVar(enum returntype, BOOLEAN, char *, char *, char *, int, int);

int
  isPassByRef(char *);

HASHNODE 
  * format_lookup(SYMTABLE *, char *);

struct ClassFile 
  * newClassFile(char *,char *);

struct method_info 
  * beginNewMethod(u2);

void
  endNewMethod(struct method_info *, char *,char *, u2);

/*****************************************************************************
 *   Global variables, a necessary evil when working with yacc.              *
 *****************************************************************************/

int
  gendebug = FALSE;     /* set to TRUE to generate debugging output          */

extern int 
  ignored_formatting,   /* number of FORMAT statements ignored               */
  bad_format_count,     /* number of bad FORMAT statements encountered       */
  stacksize;            /* current stacksize at some point in execution      */

char 
  *unit_name,           /* name of this function/subroutine                  */
  *returnname,          /* return type of this prog. unit                    */
  *cur_filename,        /* name of the class file currently writing          */
  *method_desc = NULL;  /* descriptor for method representing this prog unit */

Dlist 
  doloop = NULL,        /* stack of do loop labels                           */
  while_list = NULL,    /* stack of while loop labels                        */
  adapter_list = NULL,  /* list of adapter functions (see tech report)       */
  methcall_list = NULL; /* list of methods to be called by reflection        */

SUBSTITUTION 
  global_sub={NULL,0};  /* substitution used for implied loops               */

extern char 
  *inputfilename;       /* name of the fortran input file                    */

FILE 
  *javafp,              /* the class file currently generating               */
  *curfp;               /* the file currently being written to               */

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
  import_blas;          /* does it need to import the BLAS library           */

int pc;                 /* current program counter                           */

struct method_info
  *clinit_method,       /* special class initialization method <clinit>      */
  *main_method;         /* the primary method for this fortran program unit  */

enum _opcode
  lastOp = jvm_nop;     /* the last opcode emitted (avoids dup return stmt)  */

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
    void 
       open_output_file(AST *),
       emit_adapters(),
       constructor (AST *),
       typedec_emit (AST *),
       data_emit(AST *),
       spec_emit (AST *),
       assign_emit (AST *),
       equiv_emit (AST *),
       call_emit (AST *),
       forloop_emit (AST *),
       blockif_emit (AST *),
       logicalif_emit (AST *),
       arithmeticif_emit (AST *),
       goto_emit (AST *),
       computed_goto_emit (AST *),
       label_emit (AST *),
       write_emit (AST *),
       common_emit(AST *),
       read_emit (AST *),
       emit_invocations(AST *),
       merge_equivalences(AST *),
       print_equivalences(AST *),
       emit_prolog_comments(AST *),
       emit_javadoc_comments(AST *),
       insert_fields(AST *),
       assign_local_vars(AST *),
       return_emit(),
       end_emit(AST *);

    struct attribute_info * newCodeAttribute();
    char * tok2str(int);
    extern int locals;

    switch (root->nodetype)
    {
      case 0:
        if (gendebug)
          fprintf (stderr,"Bad node\n");

        emit (root->nextstmt);
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
            methodname = "main";
            locals = 1;
          }
          else
            methodname = strdup(classname);

          classname[0] = toupper(classname[0]);

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

          /* Initialize the lists. */

          while_list = make_dl();
          doloop = make_dl();
          adapter_list = make_dl();
          methcall_list = make_dl();

          assign_local_vars(
             root->astnode.source.progtype->astnode.source.args); 

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

          open_output_file(root->astnode.source.progtype);

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

          clinit_method = beginNewMethod(ACC_PUBLIC | ACC_STATIC);
          
          emit (root->astnode.source.typedecs);
          
          emit (root->astnode.source.progtype);

          /* check whether any class initialization code was generated.
           * if so, finish initializing the method and insert it into this
           * class.
           */
          if(pc > 0) {
            code_zero_op(jvm_return);
            endNewMethod(clinit_method, "<clinit>", "()V", 1);
            cur_class_file->methods_count++;
            dl_insert_b(cur_class_file->methods, clinit_method);
          }

          main_method = beginNewMethod(ACC_PUBLIC | ACC_STATIC);

          /* The 'catch' corresponding to the following try is generated
           * in case End. 
           */

          if(import_reflection) 
            fprintf(curfp,"try {\n");

          emit(root->astnode.source.statements);

          /* check if code was generated for this program unit's method.
           * if so, finish initializing the method and insert it into this
           * class.
           */

          if(pc > 0) {
            endNewMethod(main_method, methodname, method_desc, locals);
            cur_class_file->methods_count++;
            dl_insert_b(cur_class_file->methods, main_method);
          }

          /* following line is only temporary... */
          main_method = beginNewMethod(ACC_PUBLIC);

          emit_invocations(root->astnode.source.progtype);

          emit_adapters();

          fprintf(curfp,"} // End class.\n");
          fclose(curfp);

/*
 *        cp_dump(cur_const_table);
 *        fields_dump(cur_class_file->fields, cur_const_table);
 */
 
          cur_class_file->constant_pool_count = 
             (u2) ((CPNODE *)dl_val(dl_last(cur_const_table)))->index + 1;
          cur_class_file->constant_pool = cur_const_table;

          write_class(cur_class_file);
          break;
        }
      case Subroutine:
        if (gendebug)
	        printf ("Subroutine.\n");

        returnname = NULL;	/* Subroutines return void. */
        cur_unit = root;
        unit_name = root->astnode.source.name->astnode.ident.name;
        constructor (root);
        break;
      case Function:
        if (gendebug)
          printf ("Function.\n");

        returnname = root->astnode.source.name->astnode.ident.name;
        cur_unit = root;
        unit_name = root->astnode.source.name->astnode.ident.name;

        if(gendebug)
          printf ("Function name: %s\n", 
            root->astnode.source.name->astnode.ident.name);

        constructor (root);
        break;
      case Program:
        if (gendebug)
          printf ("Program.\n");

        returnname = NULL;	/* programs return void. */
        cur_unit = root;
        unit_name = root->astnode.source.name->astnode.ident.name;

        if (gendebug)
          printf ("Program name: %s\n", 
             root->astnode.source.name->astnode.ident.name);

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
    }				/* switch on nodetype.  */
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
  void return_emit();

  fprintf(curfp,"Dummy.label(\"%s\",999999);\n",cur_filename); 

  if (returnname != NULL) {
    if(omitWrappers && !isPassByRef(returnname))
      fprintf (curfp, "return %s;\n", returnname);
    else
      fprintf (curfp, "return %s.val;\n", returnname);
  }
  else
    fprintf (curfp, "return;\n");

  if(import_reflection) {
    fprintf(curfp, "%s%s%s%s%s%s%s",
       "} catch (java.lang.reflect.InvocationTargetException _e) {\n",
       "   System.err.println(\"Error calling method.", 
       "  \"+ _e.getMessage());\n",
       "} catch (java.lang.IllegalAccessException _e2) {\n",
       "   System.err.println(\"Error calling method.",
       "  \"+ _e2.getMessage());\n",
       "}\n");
  }

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
    if(omitWrappers && !isPassByRef(returnname))
      pushVar(cur_unit->vartype, FALSE, cur_filename,
              returnname, field_descriptor[cur_unit->vartype][0],
              0, FALSE);
    else
      pushVar(cur_unit->vartype, FALSE, cur_filename,
              returnname, wrapped_field_descriptor[cur_unit->vartype][0],
              0, TRUE);
    code_zero_op(return_opcodes[cur_unit->vartype]);
  }
  else
    code_zero_op(jvm_return);
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
  void addField(char *, char *);
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
  void print_eqv_list(AST *, FILE *);

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
  void vardec_emit(AST *, enum returntype);

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
  void vardec_emit(AST *, enum returntype);
  Dlist save_const_table;
  struct ClassFile *save_class_file;
  struct attribute_info *save_code;
  int save_stack, save_pc;
  char *get_common_prefix(char *), *save_filename;

  /* save the current global variables pointing to the class file.  this is
   * necessary because we're in the middle of generating the class file
   * for the current fortran program unit, but now we need to generate some
   * classes to hold COMMON blocks and we dont want to alter the pc, stack,
   * etc for the current class.
   */
  save_const_table = cur_const_table;
  save_class_file = cur_class_file; 
  save_filename = cur_filename; 
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

      filename = (char *)f2jrealloc(filename,
                                    strlen(common_classname) + 6);
      sprintf(filename,"%s.java", common_classname);

      cur_filename = common_classname;
      cur_const_table = make_dl();
      cur_class_file = newClassFile(common_classname,inputfilename);
      clinit_method = beginNewMethod(ACC_PUBLIC | ACC_STATIC);

      if((commonfp = fopen(filename,"w"))==NULL) 
      {
        fprintf(stderr,"Cannot open output file '%s'.\n",filename);
        perror("Reason");
        exit(1);
      }
  
      curfp = commonfp;
      
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
        code_zero_op(jvm_return);
        endNewMethod(clinit_method, "<clinit>", "()V", 1);
        cur_class_file->methods_count++;
        dl_insert_b(cur_class_file->methods, clinit_method);
      }

      cur_class_file->constant_pool_count = 
         (u2) ((CPNODE *)dl_val(dl_last(cur_const_table)))->index + 1;
      cur_class_file->constant_pool = cur_const_table;

      write_class(cur_class_file);
    }
  }

  curfp = javafp;

  /* restore previously saved globals */

  cur_const_table = save_const_table;
  cur_class_file = save_class_file;
  cur_filename = save_filename; 
  cur_code = save_code;
  stacksize = save_stack;
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
  extern METHODTAB intrinsic_toks[];
  AST *temp;
  HASHNODE *hashtemp, *ht;
  enum returntype returns;
  char *tempname;
  void vardec_emit(AST *, enum returntype);

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
      continue;

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

  void name_emit (AST *);
  void expr_emit (AST *);
  void print_string_initializer(AST *);

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
          code_zero_op(jvm_isub);
          code_zero_op(jvm_iconst_1);
          code_zero_op(jvm_iadd);
        }
        else
          expr_emit(temp2);

        /* if this isn't the first iteration, then we must multiply
         * the dimensions to get the total size of the array.
         */
        if(temp2 != root->astnode.ident.arraylist)
          code_zero_op(jvm_imul);

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

    switch(root->vartype) {
      case String:
      case Character:
        {
          CPNODE *c;

          c = cp_find_or_insert(cur_const_table, CONSTANT_Class, "java/lang/String");
          code_one_op_w(jvm_anewarray, c->index);
        }
        break;
      case Complex:
      case Double:
      case Float:
      case Integer:
      case Logical:
        code_one_op(jvm_newarray, jvm_array_type[root->vartype]);
        break;
      default:
        fprintf(stderr,"WARNING: vardec_emit() unknown vartype\n");
    }

    c = newFieldref(cur_const_table,cur_filename, name, desc); 
    code_one_op_w(jvm_putstatic, c->index);

  } else {    /* this is not an array declaration */

    if(!type_lookup(cur_param_table, root->astnode.ident.name))
    {
      if(omitWrappers && !isPassByRef(root->astnode.ident.name))
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
        if(omitWrappers && !isPassByRef(root->astnode.ident.name))
          c = cp_find_or_insert(cur_const_table,CONSTANT_Class,
                  JL_STRING);
        else
          c = cp_find_or_insert(cur_const_table,CONSTANT_Class,
                  full_wrappername[returns]);

        code_one_op_w(jvm_new,c->index);
        code_zero_op(jvm_dup);

        print_string_initializer(root);
        fprintf(curfp,";\n");

        if(gendebug) {
          printf("new fieldref:\n");
          printf("\tclass: %s\n", cur_filename);
          printf("\tname:  %s\n", name);
          printf("\tdesc:  %s\n", desc);
        }

        c = newFieldref(cur_const_table,cur_filename,name,desc); 
        code_one_op_w(jvm_putstatic, c->index);
      }
      else {
        if(omitWrappers && !isPassByRef(root->astnode.ident.name)) {
            fprintf(curfp,"= %s;\n", init_vals[returns]);
        }
        else
        {
          c = cp_find_or_insert(cur_const_table,CONSTANT_Class,
                    full_wrappername[returns]);

          code_one_op_w(jvm_new,c->index);
          code_zero_op(jvm_dup);

          code_zero_op(init_opcodes[returns]);

          c = newMethodref(cur_const_table,full_wrappername[returns],
                 "<init>", wrapper_descriptor[returns]);

          code_one_op_w(jvm_invokespecial, c->index);

          c = newFieldref(cur_const_table,cur_filename,name,desc); 
          code_one_op_w(jvm_putstatic, c->index);

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
  HASHNODE *ht;
  CPNODE *c;

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

  /* c = cp_find_or_insert(cur_const_table, CONSTANT_Utf8, bytecode_initializer); */
  c = cp_find_or_insert(cur_const_table, CONSTANT_String, bytecode_initializer);

  if(c->index > CPIDX_MAX)
    code_one_op_w(jvm_ldc_w, c->index);
  else
    code_one_op(jvm_ldc, c->index);

  if(omitWrappers && !isPassByRef(root->astnode.ident.name))
  {
    fprintf(curfp,"= new String(%s)", src_initializer);
    c = newMethodref(cur_const_table,JL_STRING,
               "<init>", STR_CONST_DESC);
  }
  else
  {
    fprintf(curfp,"= new StringW(%s)", src_initializer);
    c = newMethodref(cur_const_table,full_wrappername[String],
               "<init>", wrapper_descriptor[String]);
  }

  code_one_op_w(jvm_invokespecial, c->index);
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
  AST * data_var_emit(AST *, AST *, HASHNODE *);
  AST * data_implied_loop_emit(AST * , AST *);
  HASHNODE *hashtemp;

  /* foreach Data spec... */
  for(Dtemp = root->astnode.label.stmt; Dtemp != NULL; Dtemp = Dtemp->prevstmt) 
  {
    Ctemp = Dtemp->astnode.data.clist;

    /* foreach variable... */    
    for(Ntemp = Dtemp->astnode.data.nlist;Ntemp != NULL;Ntemp=Ntemp->nextstmt) 
    {
      /* check to see if we're looking at an implied do loop */

      if(Ntemp->nodetype == Forloop) 
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
  void name_emit (AST *);
  void expr_emit (AST *);
 
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

  AST * data_array_emit(int , AST *, AST *, int );
  void data_scalar_emit(enum returntype, AST *, AST *, int);
  int determine_var_length(HASHNODE *);

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
         /*  can't remember why this code was here....
          *
          * if(type_lookup(cur_save_table,Ntemp->astnode.ident.name))
          *   fprintf(curfp,"static %s ", returnstring[ hashtemp->type]);
          * else
          */
      if(omitWrappers && !isPassByRef(Ntemp->astnode.ident.name))
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

  double eval_const_expr(AST *);
  int idxNeedsDecr(AST *);

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
  int i, count =1;

  if(gendebug)
    printf("VAR here we are in data_array_emit, length = %d\n",length);

  fprintf(curfp,"[] ");

  /* 
   * if this variable is static, we can't declare it here 
   * because it has been declared already as a class variable.
   * so we use the "_temp_" prefix and emit the initialization.
   * later we assign the temp variable to the class variable.
   * 10/3/97  --Keith
   */

  fprintf(curfp,"%s = {",Ntemp->astnode.ident.name);

  for(i=0,count=0;(length==-1)?(Ctemp != NULL):(i< length);i++,count++) {
    if(Ctemp->token == STRING)
      fprintf(curfp,"\"%s\" ",Ctemp->astnode.constant.number);
    else {
      if(Ctemp->nodetype == Binaryop)
      {
        int j, repeat;
        char *ditem;
  
        if((Ctemp->astnode.expression.lhs == NULL) || 
           (Ctemp->astnode.expression.rhs == NULL))
        {
          fprintf(stderr,"Bad data statement!\n");
          return Ctemp;
        }

        if((Ctemp->astnode.expression.lhs->nodetype != Constant) || 
           (Ctemp->astnode.expression.rhs->nodetype != Constant))
        {
          fprintf(stderr,"Error: Data items must be constants.\n");
          return Ctemp;
        }

        repeat = atoi(Ctemp->astnode.expression.lhs->astnode.constant.number);
        ditem = Ctemp->astnode.expression.rhs->astnode.constant.number;

        /* emit the all but the last with a comma.. the last one without */

        for(j=0;j<repeat-1;j++)
          fprintf(curfp,"%s, ", ditem);
        fprintf(curfp,"%s ", ditem);
      }
      else
        fprintf(curfp,"%s ", Ctemp->astnode.constant.number);
    }

    /* 
     * Every now and then, emit a newline for readability.
     * I have run across some lines that end up so long that
     * they screw up 'vi'.   9/30/97  --Keith 
     */
    if( count % 5 == 0 )
      fprintf(curfp,"\n");

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

  return Ctemp;
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
  void expr_emit (AST *);

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
     */

    if(!needs_dec)
    {
      if(omitWrappers && !isPassByRef(Ntemp->astnode.ident.name))
        fprintf(curfp,"%s = new String(\"%*s\");\n",
          Ntemp->astnode.ident.name, len,
          Ctemp->astnode.constant.number);
      else
        fprintf(curfp,"%s = new StringW(\"%*s\");\n",
          Ntemp->astnode.ident.name, len,
          Ctemp->astnode.constant.number);
    }
    else
    {
      expr_emit(Ntemp);
      fprintf(curfp," = \"%*s\";\n", len, Ctemp->astnode.constant.number);
    }
  }
  else 
  {
    /* this is not a string, so the declaration/initialization is
     * pretty straightforward.
     */

    if(!needs_dec)
    {
      if(omitWrappers && !isPassByRef(Ntemp->astnode.ident.name))
        fprintf(curfp,"%s = %s;\n",Ntemp->astnode.ident.name,
          Ctemp->astnode.constant.number);
      else
        fprintf(curfp,"%s = new %s(%s);\n",Ntemp->astnode.ident.name,
          wrapper_returns[ type], Ctemp->astnode.constant.number);
    }
    else
    {
      expr_emit(Ntemp);
      fprintf(curfp," = %s;\n", Ctemp->astnode.constant.number);
    }
  }
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
  extern METHODTAB intrinsic_toks[];
  void external_emit(AST *);
  void intrinsic_emit(AST *);
  void scalar_emit(AST *, HASHNODE *);
  void array_emit(AST *, HASHNODE *);
  void subcall_emit(AST *);

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

        /* depending on whether this name is an array, scalar, or
         * function/subroutine call, we call scalar_emit, array_emit,
         * or subcall_emit, respectively.
         */

        if (root->astnode.ident.arraylist == NULL)
          scalar_emit(root, hashtemp);
        else if (hashtemp != NULL)
          array_emit(root, hashtemp);
        else
          subcall_emit(root);
        break;
    }

  if(gendebug)
    printf("leaving name_emit\n");
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
  AST *temp;
  char *tempstr;
  void expr_emit (AST *);

  /* check whether this node is a Substring operation.  if so, just emit
   * the variable name becuase the calls to substring() are generated in
   * expr_emit().
   */
  if(root->nodetype == Substring) {
    fprintf(curfp,"%s",root->astnode.ident.name);

    if(omitWrappers && !isPassByRef(root->astnode.ident.name))
      fprintf(curfp,".substring((");
    else
      fprintf(curfp,".val.substring((");

    return;
  }

  /* captialize the first letter of the subroutine name to get the 
   * class name. 
   */

  tempstr = strdup (root->astnode.ident.name);
  *tempstr = toupper (*tempstr);

  if(gendebug) {
    printf("@##@ in subcall_emit, %s\n",root->astnode.ident.name);

    if(type_lookup(cur_args_table, root->astnode.ident.name))
      printf("@@ calling passed-in func %s\n",root->astnode.ident.name);
  }

  fprintf (curfp, "%s.%s", tempstr,root->astnode.ident.name);
  temp = root->astnode.ident.arraylist;

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
  fprintf (curfp, ")");
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
  double eval_const_expr(AST *);
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
  void expr_emit (AST *);
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
    int offset;
    int d1, d0;

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
      code_zero_op(typeconv_matrix[root->vartype][Integer]);

    if(d0 != ht->variable->astnode.ident.D[0]) {
      fprintf (curfp, "+1");
      code_zero_op(jvm_iconst_1);
      code_zero_op(jvm_iadd);
    }
    fprintf (curfp, ")");
    
    fprintf (curfp, "+((");

    fprintf (curfp, "(");
    expr_emit(root->nextstmt);
    if(root->nextstmt->vartype != Integer)
      code_zero_op(typeconv_matrix[root->nextstmt->vartype][Integer]);

    if(d1 != ht->variable->astnode.ident.D[1]) {
      fprintf (curfp, "+1");
      code_zero_op(jvm_iconst_1);
      code_zero_op(jvm_iadd);
    }
    fprintf (curfp, ")");
    
    fprintf (curfp, "+(");

    fprintf (curfp, "(");
    expr_emit(root->nextstmt->nextstmt);
    if(root->nextstmt->nextstmt->vartype != Integer)
      code_zero_op(typeconv_matrix[root->nextstmt->nextstmt->vartype][Integer]);

    if(!idxNeedsDecr(ht->variable->astnode.ident.arraylist->nextstmt->nextstmt)) {
      fprintf (curfp, "+1");
      code_zero_op(jvm_iconst_1);
      code_zero_op(jvm_iadd);
    }
    fprintf (curfp, ")");
    
    fprintf (curfp, " * %d)) *%d) - %d", d1, d0,offset);

    pushIntConst(d1);
    code_zero_op(jvm_imul);
    pushIntConst(d0);
    code_zero_op(jvm_imul);
    code_zero_op(jvm_iadd);
    code_zero_op(jvm_iadd);
    pushIntConst(offset);
    code_zero_op(jvm_isub);
  }
  else 
  {
    /* if this isn't a 3 dimensional array, it is handled here */

    int decrementIndex = idxNeedsDecr(ht->variable->astnode.ident.arraylist);

    fprintf (curfp, "(");
    expr_emit (root);
    if(root->vartype != Integer)
      code_zero_op(typeconv_matrix[root->vartype][Integer]);

    if(decrementIndex) {
      fprintf (curfp, ")- 1");
      code_zero_op(jvm_iconst_1);
      code_zero_op(jvm_isub);
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
        code_zero_op(typeconv_matrix[root->vartype][Integer]);

      if(decrementIndex) {
        fprintf (curfp, "- 1)");
        code_zero_op(jvm_iconst_1);
        code_zero_op(jvm_isub);
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
          code_zero_op(typeconv_matrix[rhs->vartype][Integer]);
        fprintf (curfp, " - ");
        expr_emit(lhs);
        if(lhs->vartype != Integer)
          code_zero_op(typeconv_matrix[lhs->vartype][Integer]);
        fprintf (curfp, " + 1 ");
        code_zero_op(jvm_isub);
        code_zero_op(jvm_iconst_1);
        code_zero_op(jvm_iadd);
      }
      else {
        AST * lead_exp = hashtemp->variable->astnode.ident.lead_expr;

        expr_emit(lead_exp);
        if(lead_exp->vartype != Integer)
          code_zero_op(typeconv_matrix[lead_exp->vartype][Integer]);
      }

      fprintf (curfp, ")");

      code_zero_op(jvm_imul);
      code_zero_op(jvm_iadd);
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
        code_zero_op(typeconv_matrix[root->vartype][Integer]);

      if(decrementIndex) {
        fprintf (curfp, "- 1)");
        code_zero_op(jvm_iconst_1);
        code_zero_op(jvm_isub);
      }
      else
        fprintf (curfp, ")");

      fprintf (curfp, "*");

      if(gendebug)
        printf("leaddim = %s\n",hashtemp->variable->astnode.ident.leaddim);

      ht = type_lookup(cur_type_table, hashtemp->variable->astnode.ident.leaddim);
      if(!ht) {
        fprintf(stderr,"func_array_emit(): Type table is screwed!\n");
        fprintf(stderr,"   looked up %s\n",hashtemp->variable->astnode.ident.leaddim);
        exit(-1);
      }

      if(isalpha((int) hashtemp->variable->astnode.ident.leaddim[0])) {
        if(omitWrappers && !isPassByRef(hashtemp->variable->astnode.ident.leaddim)) {
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
        pushVar(ht->variable->vartype,is_arg,cur_filename,
                hashtemp->variable->astnode.ident.leaddim,
                field_descriptor[ht->variable->vartype][0],
                ht->variable->astnode.ident.localvnum, FALSE);
      }
      code_zero_op(jvm_imul);
      code_zero_op(jvm_iadd);
    }  /* Multi dimension.  */
  }

  if(is_arg) {
    fprintf(curfp,  "+ _%s_offset",arrayname);
    pushVar(Integer,is_arg,cur_filename,
            "dummy string...is this significant?",
            "I", root->astnode.ident.localvnum + 1 , FALSE);
    code_zero_op(jvm_iadd);
  }

  if(needs_cast)
    fprintf(curfp,")");

  if(!is_ext) {
    fprintf(curfp, "]");
  }
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
isPassByRef(char *name)
{
  HASHNODE *ht, *ht2, *ht3;
  char *blockName;
  int pos, i;
  AST *temp;

  /* First look up the variable name in the main hash table. */

  ht = type_lookup(cur_type_table,name);
  if(ht) {

    if(ht->variable->nodetype != Identifier)
      fprintf(stderr,"isPassByRef():  non-ident node found.\n");

    if(ht->variable->astnode.ident.passByRef)
    {
      /* simple case.  if the variable is tagged as pass-by-reference
       * in the hash table, then return TRUE.
       */

      return TRUE;
    }
    else {
      /* otherwise, we look up the variable name in the table of
       * COMMON variables.
       */

      ht2 = type_lookup(cur_common_table,name);
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
  int is_arg=FALSE, varnum=0;
  char *get_common_prefix(char *);
  char *com_prefix;
  char *name, *tmpclass, *desc;
  HASHNODE *ht;

  if (gendebug)
    printf ("Array... %s, My node type is %s\n", 
      root->astnode.ident.name,
      print_nodetype(root));

  /* find the descriptor & local var number (if applicable) for this var   */

  if((ht = type_lookup(cur_type_table, root->astnode.ident.name)) != NULL) {
    desc = getVarDescriptor(ht->variable);
    varnum = ht->variable->astnode.ident.localvnum;
  }
  else {
    fprintf(stderr,"WARNING: array_emit() can't find '%s' in hash table!\n",
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
      fprintf(stderr,"array_emit:Cant find %s in type_table\n",
          root->astnode.ident.name);

    if(ht->variable->astnode.ident.merged_name != NULL)
      name = ht->variable->astnode.ident.merged_name;

    tmpclass = strdup(com_prefix);
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
    fprintf(stderr,"array_emit: setting name to NULL!\n");
    name = root->astnode.ident.name;
  }

  if(gendebug)
    printf("### #in array_emit, setting name = %s\n",name);

  /* Determine whether this variable is an argument to the current
   * program unit.
   */

  if( type_lookup(cur_args_table,root->astnode.ident.name) != NULL )
    is_arg = TRUE;
  else
    is_arg = FALSE;

  /* 
   * Now, what needs to happen here is the context of the
   * array needs to be determined.  If the array is being
   * passed as a parameter to a method, then the array index
   * needs to be passed separately and the array passed as
   * itself.  If not, then an array value is being set,
   * so dereference with index arithmetic.  
   */

  if((root->parent != NULL) && (root->parent->nodetype == Typedec))
    fprintf (curfp, "%s", name);   /* for typedec, generate no bytecode */
  else {
    fprintf (curfp, "%s%s", com_prefix, name);
    pushVar(root->vartype,is_arg,tmpclass,name,desc,varnum,FALSE);
  }

  temp = root->astnode.ident.arraylist;

  if(root->parent == NULL) {

    /* Under normal circumstances, I dont think this should 
     * be reached.
     */

    fprintf (stderr,"Array... %s, NO PARENT - ", name);
    fprintf (stderr,"This is not good!\n");
  } else {
    if(gendebug)
      printf ("Array... %s, Parent node type... %s\n", 
        name, print_nodetype(root->parent));

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
        func_array_emit(temp, hashtemp, root->astnode.ident.name, is_arg, TRUE);
      }
      else {
        func_array_emit(temp, hashtemp, root->astnode.ident.name, is_arg,FALSE);
        code_zero_op(array_load_opcodes[root->vartype]);
      }
    }
    else if((root->parent->nodetype == Assignment) &&
            (root->parent->astnode.assignment.lhs == root))
    {
      func_array_emit(temp, hashtemp, root->astnode.ident.name, is_arg, FALSE);
    }
    else if((root->parent->nodetype == Typedec)) 
    {
      /*  Just a declaration, don't emit index. */
      if(gendebug)
        printf("I guess this is just an array declaration\n");
    }
    else {
      func_array_emit(temp, hashtemp, root->astnode.ident.name, is_arg, FALSE);
      code_zero_op(array_load_opcodes[root->vartype]);
    }
  }
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
  char * prefix = strtok(strdup(inputfilename),".");
  static char * cprefix;

  /* Look up this variable name in the table of COMMON variables */

  ht = type_lookup(cur_common_table, varname);

  if(ht == NULL)
    cprefix = "";
  else {
    cprefix = (char *) f2jalloc(
       strlen(ht->variable->astnode.ident.commonBlockName) +
       strlen(prefix) + 3);

    sprintf(cprefix,"%s_%s.", prefix,
      ht->variable->astnode.ident.commonBlockName);
  }

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
  if(omitWrappers && !root->astnode.ident.passByRef)
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
      code_zero_op(jvm_iconst_1);
      break;
    case FaLSE:
      code_zero_op(jvm_iconst_0);
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
      code_one_op_w(jvm_ldc_w,ct->index);
    else
      code_one_op(jvm_ldc,ct->index);
  } else {   /* not found, use literal */
    if((ival < JVM_SHORT_MIN) || (ival > JVM_SHORT_MAX)) {
      fprintf(stderr,"WARNING:expr_emit() bad int literal: %d\n", ival);
      return;
    }
    else if((ival < JVM_BYTE_MIN) || (ival > JVM_BYTE_MAX))
      code_one_op_w(jvm_sipush, ival);
    else if((ival < JVM_ICONST_MIN) || (ival > JVM_ICONST_MAX))
      code_one_op(jvm_bipush, ival);
    else
      code_zero_op(iconst_opcodes[ival+1]);
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
    code_one_op_w(jvm_ldc2_w, ct->index);
  else if(dval == 0.0)
    code_zero_op(jvm_dconst_0);
  else if(dval == 1.0)
    code_zero_op(jvm_dconst_1);
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
    code_one_op_w(jvm_ldc_w, ct->index);
  else
    code_one_op(jvm_ldc, ct->index);
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
   int lv, int deref)
{
  CPNODE *c;

  if(gendebug) {
    printf("in pushvar, vartype is %s\n", returnstring[vt]);
    printf("               desc is %s\n", desc);
    printf("       local varnum is %d\n", lv);
  }

  if(isArg) {
    if((desc[0] == 'L') || (desc[0] == '[')) {
      /* this is a reference type, so always use aload */
      if(lv > 3)
        code_one_op(jvm_aload, lv);
      else
        code_zero_op(short_load_opcodes[0][lv]);
    } else {
      if(lv > 3)
        code_one_op(load_opcodes[vt], lv);
      else
        code_zero_op(short_load_opcodes[vt][lv]);
    }
  }
  else {
    c = newFieldref(cur_const_table, class, name, desc);
    code_one_op_w(jvm_getstatic, c->index);
  }

  if(deref) {
    c = newFieldref(cur_const_table, full_wrappername[vt], "val", 
           val_descriptor[vt]);
    code_one_op_w(jvm_getfield, c->index);
  }
}

/*****************************************************************************
 *                                                                           *
 * pushOffsetArg                                                             *
 *                                                                           *
 * pushes an integer offset to an array onto the stack.                      *
 *                                                                           *
 *****************************************************************************/

void
pushOffsetArg(int lv)
{
  if(lv > 3)
    code_one_op(jvm_iload, lv);
  else
    code_zero_op(short_load_opcodes[Integer][lv]);
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
  extern METHODTAB intrinsic_toks[];
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
  scalar_class = cur_filename;

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

    scalar_class = com_prefix;
  }

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
      printf("here we are emitting a scalar: %s, len = %d",
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
          printf("in CALL, '%s' <- '%s'\n", 
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

          if(omitWrappers && !isPassByRef(root->astnode.ident.name)) {
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

        if(omitWrappers && !isPassByRef(root->astnode.ident.name))
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
          if(omitWrappers && !isPassByRef(root->astnode.ident.name))
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
          if(omitWrappers && !isPassByRef(root->astnode.ident.name)) {
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

      if((root->parent->nodetype == Call) && 
         (type_lookup(cur_external_table, root->parent->astnode.ident.name) != NULL))
      {
        if( type_lookup(cur_args_table,root->astnode.ident.name) != NULL ) {
          fprintf (curfp, "%s,_%s_offset", name, name);
          pushVar(root->vartype, isArg!=NULL, scalar_class, name, desc,
             typenode->variable->astnode.ident.localvnum, FALSE);
          pushOffsetArg(typenode->variable->astnode.ident.localvnum + 1);
        }
        else {
          fprintf (curfp, "%s,0", name);
          pushVar(root->vartype, isArg!=NULL, scalar_class, name, desc,
             typenode->variable->astnode.ident.localvnum, FALSE);
          code_zero_op(jvm_iconst_0);
        }
      }
      else if((root->parent->nodetype == Assignment) &&
              (root->parent->astnode.assignment.lhs == root)) {
        /* LHS of assignment.  do not generate any bytecode. */
        fprintf (curfp, "%s", name);
      }
      else {
        fprintf (curfp, "%s", name);
        pushVar(root->vartype, isArg!=NULL, scalar_class, name, desc,
           typenode->variable->astnode.ident.localvnum, FALSE);
      }
    }
  }
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
  extern METHODTAB intrinsic_toks[];
  char *tempname, *javaname;
  AST *temp;
  void call_emit (AST *);
  void expr_emit (AST *);

  if(gendebug) {
    printf("here we are in external_emit\n");
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
    if(gendebug)
      printf("unit %s: EXTERNAL has parent CALL\n", unit_name);
   
    tempname = strdup(root->astnode.ident.name);
    *tempname = toupper(*tempname);

    /* if this external function is also an argument to the
     * current unit, we already have an Object reference to
     * it, so just pass that.  If not, we create a new 
     * instance of whatever class we want to pass.
     */

    if(type_lookup(cur_args_table,root->astnode.ident.name))
      fprintf(curfp,"%s", root->astnode.ident.name);
    else
      fprintf(curfp," new %s() ",tempname);

    return;
  }

  tempname = strdup(root->astnode.ident.name);
  uppercase(tempname);

  javaname = (char *) methodscan (intrinsic_toks, tempname);

  /*  
   *  This block of code is only called if the identifier
   *  absolutely does not have an entry in any table,
   *  and corresponds to a method invocation of
   *  something in the blas or lapack packages.  
   */

  if (javaname == NULL)
  {
    if (root->astnode.ident.arraylist != NULL)
      call_emit (root);
    return;
  }

  /* Ensure that the call has arguments */

  if (root->astnode.ident.arraylist != NULL)
  {
    if (!strcmp (tempname, "LSAME"))
    {
      /* LSAME should return TRUE if the two character arguments are
       * the same letter, regardless of case.
       */ 

      temp = root->astnode.ident.arraylist;

      if(gendebug)
        printf("emitting a call to LSAME...first nodetype = %s, next = %s\n",
          print_nodetype(temp), print_nodetype(temp->nextstmt));

      fprintf(curfp, "(");
      expr_emit(temp);
      fprintf(curfp, ".toLowerCase().charAt(0) == ");
      expr_emit(temp->nextstmt);
      fprintf(curfp, ".toLowerCase().charAt(0))");

      return;
    }
    else if (!strcmp (tempname, "LSAMEN"))
    {
      /* LSAMEN should return TRUE if the first N characters of the
       * two arguments are the same, regardless of case.  Currently
       * this is mapped to java.lang.String.regionMatches().
       */

      temp = root->astnode.ident.arraylist;

      /* first, make sure there are enough args to work with */
      if(temp == NULL) {
        fprintf(stderr,"No args to LSAMEN\n");
        return;
      } 
      else if(temp->nextstmt == NULL) {
        fprintf(stderr,"Not enough args to LSAMEN\n");
        return;
      }
      else if(temp->nextstmt->nextstmt == NULL) {
        fprintf(stderr,"Not enough args to LSAMEN\n");
        return;
      }

      expr_emit(temp->nextstmt);
      fprintf (curfp, "%s(true,0,", javaname);
      expr_emit (temp->nextstmt->nextstmt);
      fprintf (curfp, ",0,");
      expr_emit (temp);
      fprintf (curfp, ")");
      return;
    }
  }
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
 *                                                                           *
 *****************************************************************************/

void
intrinsic_emit(AST *root)
{
  extern METHODTAB intrinsic_toks[];
  AST *temp;
  char *tempname, *javaname;
  void expr_emit (AST *);

  if(gendebug)
    printf("entering intrinsic_emit\n");

  tempname = strdup(root->astnode.ident.name);
  uppercase(tempname);

  javaname = (char *)methodscan (intrinsic_toks, tempname);

  /* 
   * This goes into an infinite loop. I don't
   * know why.  Might be a result from parsing.
   * It will replace the entire lower block
   * when I get it to work.  
   */

#ifdef  KJGKJKJKKJH
  temp = root->astnode.ident.arraylist;
  assert(temp != NULL);
  fprintf (curfp, "%s(", javaname);

  for (temp; temp != NULL; temp->nextstmt)
  {
    if(gendebug)
      printf("Yoikes\n");
    expr_emit (temp);
    if(temp->nextstmt)
      fprintf (curfp, ", ");
  }
  fprintf (curfp, ")");
  return;
  /*put end brace here when KJGKJKJKKJH defined*/ 
#endif	  

  if( !strcmp (tempname, "MAX") || !strcmp (tempname, "MIN"))
  {
    int ii,arg_count = 0;

    for(temp = root->astnode.ident.arraylist; temp != NULL; temp = temp->nextstmt)
      arg_count++;

    /* If we only have one arg, just emit that expression.  This should not
     * normally happen
     */

    if(arg_count == 1) {
      fprintf (curfp, "(");
      expr_emit (temp);
      fprintf (curfp, ")");
    }

    /* special handling of common situation in which MAX or MIN has three args.
     * instead of two method calls, we use one call and one '?' operator.
     * for example, MAX(a,b,c) would be translated to:
     *  Math.max(a>b?a:b,c)
     */

    else if(arg_count == 3) {
      temp = root->astnode.ident.arraylist;
      fprintf (curfp, "%s((", javaname);
      expr_emit (temp);
      if( !strcmp (tempname, "MAX"))
        fprintf (curfp, ") > (");
      else
        fprintf (curfp, ") < (");
      expr_emit (temp->nextstmt);
      fprintf (curfp, ") ? (");
      expr_emit (temp);
      fprintf (curfp, ") : (");
      expr_emit (temp->nextstmt);
      fprintf (curfp, "), ");
      expr_emit (temp->nextstmt->nextstmt);
      fprintf (curfp, ")");
    }

    /*
     * For cases in which MAX or MIN has more than three args, we generate n-1
     * method calls, where n is the number of args.  For example, MAX(a,b,c,d,e)
     * would be translated to:
     *   Math.max(Math.max(Math.max(Math.max(a,b),c),d),e)
     * I dont think this situation is very common (in LAPACK/BLAS at least).
     */

    else {
      for(ii=0;ii<arg_count -1;ii++)
        fprintf(curfp,"%s(",javaname);
      
      temp = root->astnode.ident.arraylist;
      expr_emit (temp);
      fprintf (curfp, ", ");

      for(temp = temp->nextstmt; temp != NULL; temp = temp->nextstmt) {
        expr_emit(temp);
        if(temp->nextstmt != NULL)
          fprintf (curfp, "), ");
        else
          fprintf (curfp, ") ");
      }
    }

    return;
  }

  if ( (!strcmp (tempname, "DSQRT")) ||
       (!strcmp (tempname, "SQRT"))  ||
       (!strcmp (tempname, "DABS"))  ||
       (!strcmp (tempname, "DBLE"))  ||
       (!strcmp (tempname, "LOG"))  ||
       (!strcmp (tempname, "SIN"))  ||
       (!strcmp (tempname, "COS"))  ||
       (!strcmp (tempname, "EXP"))  ||
       (!strcmp (tempname, "ABS")))
  {
    temp = root->astnode.ident.arraylist;
    fprintf (curfp, "%s(", javaname);
    expr_emit (temp);
    fprintf (curfp, ")");
    return;
  }

  if(!strcmp(tempname,"LOG10"))
  {
    temp = root->astnode.ident.arraylist;
    fprintf (curfp, "(%s(", javaname);
    expr_emit (temp);
    fprintf (curfp, ") / 2.30258509)");
    return;
  }

  if(!strcmp(tempname,"LEN"))
  {
    /* LEN is a fortran intrinsic which returns the number of characters 
     * allocated to a string.
     */

    HASHNODE *ht;

    temp = root->astnode.ident.arraylist;

    if(temp != NULL) {
      if( (ht=type_lookup(cur_type_table,temp->astnode.ident.name)) != NULL)
      {
        fprintf (curfp, " %d ", ht->variable->astnode.ident.len);

        if(gendebug)
          printf("LEN(%s) = %d\n",temp->astnode.ident.name,
            ht->variable->astnode.ident.len);
      }
      else
      {
        fprintf (curfp, " 1 ");

        if(gendebug)
          printf("LEN(%s) = 1\n",temp->astnode.ident.name);
      }
    }
    return;
  }

  if (!strcmp (tempname, "MOD"))
  {
    /* integer mod.  Perhaps here we should check the types of the
     * operands and emit a % for integers and Math.IEEERemainder
     * for doubles.
     */

    temp = root->astnode.ident.arraylist;
    fprintf(curfp,"(");
    expr_emit(temp);
    fprintf(curfp,")%%(");
    expr_emit(temp->nextstmt);
    fprintf(curfp,") ");

    /*  
     *  this chunk of code will emit a call to MOD as a call to
     *  Math.IEEERemainder().  usually that is not appropriate
     *  since IEEERemainder returns double, whereas in FOTRAN,
     *  the expected type is int.   -- keith
     *
     * fprintf (curfp, "%s(", javaname);
     * expr_emit (temp);
     * fprintf (curfp, ", ");
     * expr_emit (temp->nextstmt);
     * fprintf (curfp, ")");
     */

    return;
  }

  if (!strcmp (tempname, "ICHAR"))
  {
    temp = root->astnode.ident.arraylist;
    fprintf (curfp, "%s(", javaname);
    expr_emit (temp);
    fprintf (curfp, ".charAt(0))");
    return;
  }

  if (!strcmp (tempname, "CHAR"))
  {
    temp = root->astnode.ident.arraylist;
    fprintf (curfp, "%s(", javaname);
    expr_emit (temp);
    fprintf (curfp, ")");
    return;
  }

  if((!strcmp (tempname, "INT")) ||
     (!strcmp (tempname, "REAL")))
  {
    temp = root->astnode.ident.arraylist;
    fprintf (curfp, "%s(", javaname);
    expr_emit (temp);
    fprintf (curfp, ")");
    return;
  }

  if(!strcmp(tempname,"NINT"))
  {
    temp = root->astnode.ident.arraylist;

    fprintf(curfp,"%s((",javaname);
    expr_emit(temp);
    fprintf(curfp,") >= 0 ? (");
    expr_emit(temp);
    fprintf(curfp,") + .5 : (");
    expr_emit(temp);
    fprintf(curfp,") - .5)");
    return;
  }

  if(!strcmp(tempname,"SIGN"))
  {
    temp = root->astnode.ident.arraylist;

    fprintf(curfp,"((");
    expr_emit(temp->nextstmt);
    fprintf(curfp,") >= 0 ? Math.abs(");
    expr_emit(temp);
    fprintf(curfp,") : -Math.abs(");
    expr_emit(temp);
    fprintf(curfp,"))");
    return;
  }

  if(gendebug)
    printf("leaving intrinsic_emit\n");
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
  extern METHODTAB intrinsic_toks[];
  char *tempname;
  CPNODE * ct;
  int cur_vt;

  void name_emit (AST *);

  if(root == NULL)
  {
    /* We should not have a NULL expression */

    fprintf(stderr,"Warning: NULL root in expr_emit\n");
    return;
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
           
        expr_emit (root->astnode.expression.lhs);
        fprintf (curfp, ", ");
        expr_emit (root->astnode.expression.rhs);
        fprintf (curfp, ")");

        ct = newMethodref(cur_const_table,"java/lang/Math", "pow", "(DD)D");

        code_one_op_w(jvm_invokestatic, ct->index);
 
        if(gencast)
          code_zero_op(jvm_d2i);

      }
      break;
    case Binaryop:
      expr_emit (root->astnode.expression.lhs);

      if(root->astnode.expression.lhs->vartype > root->vartype)
        code_zero_op(
          typeconv_matrix[root->astnode.expression.lhs->vartype][root->vartype]);

      fprintf (curfp, "%c", root->astnode.expression.optype);
      expr_emit (root->astnode.expression.rhs);

      if(root->astnode.expression.rhs->vartype > root->vartype)
        code_zero_op(
          typeconv_matrix[root->astnode.expression.rhs->vartype][root->vartype]);

      switch(root->astnode.expression.optype) {
        case '+':
          code_zero_op(add_opcode[root->vartype]);
          break;
        case '-':
          code_zero_op(sub_opcode[root->vartype]);
          break;
        case '/':
          code_zero_op(div_opcode[root->vartype]);
          break;
        case '*':
          code_zero_op(mul_opcode[root->vartype]);
          break;
        default:
          fprintf(stderr,"WARNING: unsupported optype\n");
          break;  /* for ANSI C compliance */
      }

      break;
    case Unaryop:
      fprintf (curfp, "%c", root->astnode.expression.minus);
      expr_emit (root->astnode.expression.rhs);

      if(root->astnode.expression.minus == '-')
        code_zero_op(neg_opcode[root->vartype]);

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
            CPNODE * c;

            c = cp_find_or_insert(cur_const_table,CONSTANT_Class,
                    full_wrappername[root->vartype]);

            code_one_op_w(jvm_new,c->index);
            code_zero_op(jvm_dup);
            pushConst(root);

            c = newMethodref(cur_const_table,full_wrappername[root->vartype], 
                   "<init>", wrapper_descriptor[root->vartype]);

            code_one_op_w(jvm_invokespecial, c->index);

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
            CPNODE * c;

            c = cp_find_or_insert(cur_const_table,CONSTANT_Class,
                    full_wrappername[root->vartype]);

            code_one_op_w(jvm_new,c->index);
            code_zero_op(jvm_dup);

            pushConst(root);
            
            c = newMethodref(cur_const_table,full_wrappername[root->vartype], "<init>",
                   wrapper_descriptor[root->vartype]);

            code_one_op_w(jvm_invokespecial, c->index);

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
          code_zero_op(jvm_iconst_1);
          code_zero_op(jvm_ixor);
          break;
        case AND:
          code_zero_op(jvm_iand);
          break;
        case OR:
          code_zero_op(jvm_ior);
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
        code_one_op_w(jvm_invokevirtual, c->index);  /* call trim() */
      
        /* after the call to trim, we now have a new string
         * sitting on top of the stack with our second string
         * sitting underneath.  so, we issue a swap instruction
         * and then call trim() on the second string.
         */
        code_zero_op(jvm_swap);

        fprintf(curfp,".trim().equalsIgnoreCase(");

        expr_emit (root->astnode.expression.rhs);
        code_one_op_w(jvm_invokevirtual, c->index);  /* call trim() */

        fprintf(curfp,".trim())");

        c = newMethodref(cur_const_table,JL_STRING,
               "equalsIgnoreCase", STREQV_DESC);
        code_one_op_w(jvm_invokevirtual, c->index);  /* equalsIgnoreCase() */

        /* now check the op type & reverse if .NE. */
        if(root->token == rel_ne) {
          code_zero_op(jvm_iconst_1);
          code_zero_op(jvm_ixor);
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
            code_zero_op(
              typeconv_matrix[root->astnode.expression.lhs->vartype][cur_vt]);
          }

          fprintf (curfp, " == ");

          expr_emit (root->astnode.expression.rhs);

          if(root->astnode.expression.rhs->vartype > cur_vt) {
            code_zero_op(
              typeconv_matrix[root->astnode.expression.rhs->vartype][cur_vt]);
          }

          break;
        case rel_ne:

          expr_emit (root->astnode.expression.lhs);
          if(root->astnode.expression.lhs->vartype > cur_vt) {
            code_zero_op(
              typeconv_matrix[root->astnode.expression.lhs->vartype][cur_vt]);
          }
          fprintf (curfp, " != ");
          expr_emit (root->astnode.expression.rhs);
          if(root->astnode.expression.rhs->vartype > cur_vt) {
            code_zero_op(
              typeconv_matrix[root->astnode.expression.rhs->vartype][cur_vt]);
          }
          break;
        case rel_lt:
          expr_emit (root->astnode.expression.lhs);
          if(root->astnode.expression.lhs->vartype > cur_vt) {
            code_zero_op(
              typeconv_matrix[root->astnode.expression.lhs->vartype][cur_vt]);
          }
          fprintf (curfp, " < ");
          expr_emit (root->astnode.expression.rhs);
          if(root->astnode.expression.rhs->vartype > cur_vt) {
            code_zero_op(
              typeconv_matrix[root->astnode.expression.rhs->vartype][cur_vt]);
          }
          break;
        case rel_le:
          expr_emit (root->astnode.expression.lhs);
          if(root->astnode.expression.lhs->vartype > cur_vt) {
            code_zero_op(
              typeconv_matrix[root->astnode.expression.lhs->vartype][cur_vt]);
          }
          fprintf (curfp, " <= ");
          expr_emit (root->astnode.expression.rhs);
          if(root->astnode.expression.rhs->vartype > cur_vt) {
            code_zero_op(
              typeconv_matrix[root->astnode.expression.rhs->vartype][cur_vt]);
          }
          break;
        case rel_gt:
          expr_emit (root->astnode.expression.lhs);
          if(root->astnode.expression.lhs->vartype > cur_vt) {
            code_zero_op(
              typeconv_matrix[root->astnode.expression.lhs->vartype][cur_vt]);
          }
          fprintf (curfp, " > ");
          expr_emit (root->astnode.expression.rhs);
          if(root->astnode.expression.rhs->vartype > cur_vt) {
            code_zero_op(
              typeconv_matrix[root->astnode.expression.rhs->vartype][cur_vt]);
          }
          break;
        case rel_ge:
          expr_emit (root->astnode.expression.lhs);
          if(root->astnode.expression.lhs->vartype > cur_vt) {
            code_zero_op(
              typeconv_matrix[root->astnode.expression.lhs->vartype][cur_vt]);
          }
          fprintf (curfp, " >= ");
          expr_emit (root->astnode.expression.rhs);
          if(root->astnode.expression.rhs->vartype > cur_vt) {
            code_zero_op(
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
          /* the only difference between dcmpg and dcmpl is the handling of NaN
           * value.  for .lt. and .le. we use dcmpg, otherwise use dcmpl.
           * this mirrors the behavior of javac.
           */
          if((root->token == rel_lt) || (root->token == rel_le))
            code_zero_op(jvm_dcmpg);
          else
            code_zero_op(jvm_dcmpl);

          code_one_op_w(dcmp_opcode[root->token], 
                   jvm_opcode[dcmp_opcode[root->token]].width +
                   jvm_opcode[jvm_iconst_1].width +
                   jvm_opcode[jvm_goto].width);
          code_zero_op(jvm_iconst_0);
          code_one_op_w(jvm_goto, jvm_opcode[jvm_goto].width +
                        jvm_opcode[jvm_iconst_0].width);
          code_zero_op(jvm_iconst_1);
          break;
        case Integer: 
          code_one_op_w(icmp_opcode[root->token], 
                   jvm_opcode[icmp_opcode[root->token]].width +
                   jvm_opcode[jvm_iconst_0].width +
                   jvm_opcode[jvm_goto].width);
          code_zero_op(jvm_iconst_0);
          code_one_op_w(jvm_goto, jvm_opcode[jvm_goto].width +
                        jvm_opcode[jvm_iconst_1].width);
          code_zero_op(jvm_iconst_1);
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

        expr_emit(root->astnode.ident.arraylist);
        fprintf(curfp,")-1,");

        code_zero_op(jvm_iconst_m1);  /* decrement start idx by one */
        code_zero_op(jvm_iadd);

        expr_emit(root->astnode.ident.arraylist->nextstmt);
        fprintf(curfp,")");

        c = newMethodref(cur_const_table,JL_STRING,
                 "substring", SUBSTR_DESC);
        code_one_op_w(jvm_invokevirtual, c->index);  /* equalsIgnoreCase() */

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
open_output_file(AST *root)
{
  char * filename;
  char * classname;
  char import_stmt[60];
  
  /* allocate some space for the filename */

  filename = (char *)
     f2jalloc(strlen(root->astnode.source.name->astnode.ident.name) + 10);

  strcpy(filename,lowercase(strdup(root->astnode.source.name->astnode.ident.name)));
  *filename = toupper (*filename);
  classname = strdup(filename);
  cur_filename = classname;
  strcat(filename,".java");

  if(gendebug)
    printf("filename is %s\n",filename);

  if((javafp = fopen(filename,"w"))==NULL) {
    fprintf(stderr,"Cannot open output file '%s'.\n",filename);
    perror("Reason");
    exit(1);
  }

  curfp = javafp;  /* set global pointer to output file */

  /* add import statements if necessary */

  import_stmt[0] = '\0';
  
  if(import_reflection)
    strcat(import_stmt,"import java.lang.reflect.*;\n");

  if(import_blas)
    strcat(import_stmt,"import org.netlib.blas.*;\n");

  javaheader(javafp,import_stmt);

  if(genJavadoc)
    emit_javadoc_comments(root);

  fprintf(javafp,"public class %s {\n\n", classname);
}

/*****************************************************************************
 *                                                                           *
 * strAppend                                                                 *
 *                                                                           *
 * Append the given string value (new) to the expandable string (str),       *
 * allocating more memory if necessary.                                      *
 *                                                                           *
 *****************************************************************************/

struct _str *
strAppend(struct _str *str, char *new)
{
  if(str == NULL) {
    str = (struct _str *)f2jalloc(sizeof (struct _str));
    str->size = STR_INIT;
    str->val = (char *)f2jalloc(STR_INIT);
    str->val[0] = '\0';
  }

  if(strlen(new) + strlen(str->val) >= str->size) {
    if(strlen(new) > STR_CHUNK) {
      str->val = (char *)f2jrealloc(str->val, str->size + strlen(new));
      str->size += strlen(new);
    }
    else {
      str->val = (char *)f2jrealloc(str->val, str->size + STR_CHUNK);
      str->size += STR_CHUNK;
    }
  }

  strcat(str->val, new);

  return str;
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
  extern char *returnstring[];
  AST *tempnode;
  char *tempstring, *ret_desc;
  HASHNODE *hashtemp;
  void print_string_initializer(AST *);
  void emit_interface(AST *);
  struct _str * temp_desc = NULL;
  int isArray = 0;

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

    if(omitWrappers && !isPassByRef(name)) {
      addField( name, field_descriptor[returns][0]);
      desc = field_descriptor[returns][0];
    }
    else {
      addField(name, wrapped_field_descriptor[returns][0]);
      desc = wrapped_field_descriptor[returns][0];
    }

    ret_desc = field_descriptor[returns][0];

    printf("this is a Function, needs implicit variable\n");
    printf("method name = %s\n", name);
    printf("implicit var desc = %s\n", desc);

    /* Test code.... */
    if ((returns == String) || (returns == Character))
    {
      if(omitWrappers && !isPassByRef(name))
        c = cp_find_or_insert(cur_const_table,CONSTANT_Class,
                JL_STRING);
      else
        c = cp_find_or_insert(cur_const_table,CONSTANT_Class,
                full_wrappername[returns]);

      code_one_op_w(jvm_new,c->index);
      code_zero_op(jvm_dup);

      fprintf(curfp, "static %s %s ", 
           returnstring[returns], name);

      print_string_initializer(root->astnode.source.name);

      fprintf(curfp, ";\n\n");

      c = newFieldref(cur_const_table,cur_filename, name, desc);
      code_one_op_w(jvm_putstatic, c->index);
    }
    else
    {
      if(omitWrappers && 
        !isPassByRef(root->astnode.source.name->astnode.ident.name))
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

        code_one_op_w(jvm_new,c->index);
        code_zero_op(jvm_dup);

        code_zero_op(init_opcodes[returns]);

        c = newMethodref(cur_const_table,full_wrappername[returns],
               "<init>", wrapper_descriptor[returns]);

        code_one_op_w(jvm_invokespecial, c->index);

        c = newFieldref(cur_const_table,cur_filename,name,desc); 
        code_one_op_w(jvm_putstatic, c->index);

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

    ret_desc = "V";

    if(genInterfaces)
      emit_interface(root);
  }
  else  /* Else we have a program, create a main() function */
  {
    ret_desc = "V";
    fprintf (curfp, "\npublic static void main (String [] args");
  }

  temp_desc = strAppend(temp_desc, "(");

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
      fprintf (stderr,"Type table is screwed (codegen.c).\n");
      fprintf (stderr,"  (looked up: %s)\n", tempnode->astnode.ident.name);
      exit (-1);
    }

    isArray = hashtemp->variable->astnode.ident.arraylist != NULL;

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
        isPassByRef(tempnode->astnode.ident.name))
      {
        tempstring = wrapper_returns[returns];
        temp_desc = strAppend(temp_desc, 
                      wrapped_field_descriptor[returns][isArray]);
      }
      else {
        tempstring = returnstring[returns];
        temp_desc = strAppend(temp_desc, field_descriptor[returns][isArray]);
      }
    }
    else
    {
      if (hashtemp->variable->astnode.ident.arraylist == NULL) {
        tempstring = wrapper_returns[returns];
        temp_desc = strAppend(temp_desc, 
                      wrapped_field_descriptor[returns][isArray]);
      }
      else {
        tempstring = returnstring[returns];
        temp_desc = strAppend(temp_desc, field_descriptor[returns][isArray]);
      }
    }

    /* if this is an array, then append an I to the descriptor to
     * represent the integer offset arg.
     */

    if(isArray)
      temp_desc = strAppend(temp_desc, "I");

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
      free(temp2);
    }

    /* Don't emit a comma on the last iteration. */
    if (tempnode->nextstmt)
      fprintf (curfp, ",\n");
  }

  fprintf (curfp, ")  {\n\n");
    
  /* finish off the method descriptor.
   * for Functions, use the return descriptor calculated above.
   * for Programs, the descriptor must be ([Ljava/lang/String;)V.
   * for Subroutines, use void as the return type.
   */

  if(root->nodetype == Function) {
    temp_desc = strAppend(temp_desc, ")");
    temp_desc = strAppend(temp_desc, ret_desc);
  }
  else if(root->nodetype == Program) {
    temp_desc = strAppend(temp_desc, "[Ljava/lang/String;)V");
  }
  else {
    temp_desc = strAppend(temp_desc, ")V");
  }

  method_desc = temp_desc->val;

  /* if one of the arguments is a function, we must use the
   * reflection mechanism to perform the method call.
   */

  if(import_reflection) {
    fprintf(stderr,"WARNING: reflection stuff not implemented for bytecode\n");
    tempnode = root->astnode.source.args;
    for (; tempnode != NULL; tempnode = tempnode->nextstmt)
    {
      if(type_lookup(cur_external_table, tempnode->astnode.ident.name) != NULL)
      {
        fprintf(curfp,"  java.lang.reflect.Method _%s_meth ", 
          tempnode->astnode.ident.name);
        fprintf(curfp," = %s.getClass().getDeclaredMethods()[0];\n",
          tempnode->astnode.ident.name);
      }
    }
  }

  /* If this program unit does any reading, we declare an instance of
   * the EasyIn class.
   */

  if(root->astnode.source.needs_input)
    fprintf(curfp,"  EasyIn _f2j_in = new EasyIn();\n");

  if(type_lookup(cur_external_table,"etime") != NULL)
    fprintf(curfp, "  Etime.etime();\n");
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
  extern char *returnstring[];
  AST *tempnode, *prev;
  char *tempstring;
  HASHNODE *hashtemp;
  FILE *intfp;
  char *intfilename;
  char *classname;
  Dlist decs, rest, tmp;
  int i;
  BOOLEAN skipped;
  void emit_methcall(FILE *, AST *);

  decs = make_dl();
  rest = make_dl();

  classname = strdup(root->astnode.source.name->astnode.ident.name);
  intfilename = f2jalloc( strlen(classname) + 6 );
  uppercase(classname);
  strcpy(intfilename,classname);
  strcat(intfilename,".java");

  intfp = fopen(intfilename,"w");
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
        isPassByRef(tempnode->astnode.ident.name))
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

        if(isPassByRef(tempnode->astnode.ident.name)) {
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
  extern char *returnstring[];
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
        isPassByRef(tempnode->astnode.ident.name))
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
  int *tmp_int;
  void name_emit (AST *);
  void assign_emit (AST *);

  tmp_int = (int*)f2jalloc(sizeof(int));

  *tmp_int = atoi(root->astnode.forloop.Label->astnode.constant.number);

  /* push this do loop's number on the stack */
  dl_insert_b(doloop, tmp_int);

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
   /*  Done with loop parameters.  */
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
  int dl_int_search(Dlist, int);
  int dl_int_examine(Dlist);

  if( (!dl_empty(doloop)) && dl_int_search(doloop, root->astnode.go_to.label) )
  {
     /*
      *  we are inside a do loop and we are looking at a goto
      *  statement to the 'continue' statement of an enclosing loop.
      *  what we want to do here is just emit a 'labeled continue' 
      */ 

    fprintf(curfp,"continue forloop%d;\n",root->astnode.go_to.label);
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
  AST *temp;
  int count = 1;

  for(temp = root->astnode.computed_goto.intlist;temp!=NULL;temp=temp->nextstmt)
  {
    if(temp != root->astnode.computed_goto.intlist)
      fprintf(curfp,"else ");
    fprintf(curfp,"if (");
    expr_emit(root->astnode.computed_goto.name);
    fprintf(curfp," == %d) \n", count);
    fprintf(curfp,"  Dummy.go_to(\"%s\",%s);\n", cur_filename, 
      temp->astnode.constant.number);
    count++;
  }
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
  fprintf (curfp, "if (");
  if (root->astnode.logicalif.conds != NULL)
    expr_emit (root->astnode.logicalif.conds);
  fprintf (curfp, ")  \n    ");
  emit (root->astnode.logicalif.stmts);
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
  fprintf (curfp, "if ((");
  if (root->astnode.arithmeticif.cond != NULL)
    expr_emit (root->astnode.arithmeticif.cond);
  fprintf (curfp, ") < 0)  \n    ");
  fprintf(curfp,"  Dummy.go_to(\"%s\",%d);\n", cur_filename,
    root->astnode.arithmeticif.neg_label);
  fprintf (curfp, "else if ((");
  if (root->astnode.arithmeticif.cond != NULL)
    expr_emit (root->astnode.arithmeticif.cond);
  fprintf (curfp, ") == 0)  \n    ");
  fprintf(curfp,"  Dummy.go_to(\"%s\",%d);\n", cur_filename,
    root->astnode.arithmeticif.zero_label);
  fprintf (curfp, "else ");
  fprintf(curfp,"  Dummy.go_to(\"%s\",%d);\n", cur_filename,
    root->astnode.arithmeticif.pos_label);
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
  int dl_int_examine(Dlist);

  if (root->astnode.label.stmt != NULL) {
    if (root->astnode.label.stmt->nodetype != Format) {
      fprintf (curfp, "label%d:\n   ", root->astnode.label.number);
      fprintf(curfp,"Dummy.label(\"%s\",%d);\n",cur_filename,
        root->astnode.label.number);
      emit (root->astnode.label.stmt);
    }
  } 
  else {
    /* if this continue statement corresponds with the most
     * recent DO loop, then this is the end of the loop - pop
     * the label off the doloop list.
     */

    if(!dl_empty(doloop) && 
       (dl_int_examine(doloop) == root->astnode.label.number))
    {
      /*
       * finally pop this loop's label number off the stack and
       * emit the label (for experimental goto resolution)
       */

      fprintf(curfp,"Dummy.label(\"%s\",%d);\n",cur_filename,
        *((int *) dl_pop(doloop)));

      fprintf (curfp, "}              //  Close for() loop. \n");
      fprintf(curfp, "}\n");
    }
    else {
      /* since the stmt pointer is null, this node must be
       * a CONTINUE statement (but not associated with a
       * DO loop).
       */

      fprintf (curfp, "label%d:\n   ", root->astnode.label.number);
      fprintf(curfp,"Dummy.label(\"%s\",%d);\n",cur_filename,
        root->astnode.label.number);
    }
  }
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
  AST *temp;
  void read_implied_loop_emit(AST *, char **);
  char **funcname;

  /* if the READ statement has no args, just read a line and
   * ignore it.
   */

  if(root->astnode.io_stmt.arg_list == NULL) {
    fprintf(curfp,"_f2j_in.readString();  // skip a line\n");
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
  }
  else
    funcname = input_func;

  for(temp=root->astnode.io_stmt.arg_list;temp!=NULL;temp=temp->nextstmt)
  {
    if(temp->nodetype == ImpliedLoop)
      read_implied_loop_emit(temp, funcname);
    else if(temp->nodetype == Identifier)
    {
      name_emit(temp);
      if( (temp->vartype == Character) || (temp->vartype == String) )
        fprintf(curfp," = _f2j_in.%s(%d);\n",funcname[temp->vartype],
           temp->astnode.ident.len);
      else
        fprintf(curfp," = _f2j_in.%s();\n",funcname[temp->vartype]);
    }
    else
    {
      fprintf(stderr,"Read list must consist of idents or implied loops\n");
      fprintf(stderr,"   nodetype is %s\n", print_nodetype(temp));
      continue;
    }
  }
  fprintf(curfp,"_f2j_in.skipRemaining();\n");

  /* Emit the catch block for when we hit EOF.  We only care if
   * the READ statement has an END label.
   */

  if(root->astnode.io_stmt.end_num > 0 )
  {
    fprintf(curfp,"} catch (java.io.IOException e) {\n");
    fprintf(curfp,"Dummy.go_to(\"%s\",%d);\n",cur_filename,
      root->astnode.io_stmt.end_num);
    fprintf(curfp,"}\n");
  }
}

/*****************************************************************************
 *                                                                           *
 * read_implied_loop_emit                                                    *
 *                                                                           *
 * This function generates code for implied DO loops contained in READ       *
 * statements.  We dont handle any FORMAT statements.                        *
 *                                                                           *
 *****************************************************************************/

void
read_implied_loop_emit(AST *node, char **func)
{
  char *loopvar = node->astnode.forloop.counter->astnode.ident.name;
  char *valSuffix = ".val";

  if(omitWrappers) {
    if( !isPassByRef(loopvar) )
      valSuffix = "";

    fprintf(curfp,"for(%s%s = ",loopvar, valSuffix); 
    expr_emit(node->astnode.forloop.start);
    fprintf(curfp,"; %s%s <= ",loopvar, valSuffix); 
    expr_emit(node->astnode.forloop.stop);
    if(node->astnode.forloop.incr == NULL)
      fprintf(curfp,"; %s%s++)\n",loopvar, valSuffix); 
    else
    {
      fprintf(curfp,"; %s%s += ",loopvar, valSuffix); 
      expr_emit(node->astnode.forloop.incr);
      fprintf(curfp,")\n"); 
    }
  }
  else
  {
    fprintf(curfp,"for(%s.val = ",loopvar); 
    expr_emit(node->astnode.forloop.start);
    fprintf(curfp,"; %s.val <= ",loopvar); 
    expr_emit(node->astnode.forloop.stop);
    if(node->astnode.forloop.incr == NULL)
      fprintf(curfp,"; %s.val++)\n",loopvar); 
    else
    {
      fprintf(curfp,"; %s.val += ",loopvar); 
      expr_emit(node->astnode.forloop.incr);
      fprintf(curfp,")\n"); 
    }
  }

  if(node->astnode.forloop.Label->nodetype != Identifier) {
    fprintf(stderr,"unit %s:Cant handle this nodetype (%s) ",
      unit_name,print_nodetype(node->astnode.forloop.Label));
    fprintf(stderr," in implied loop (read stmt)\n");
  }
  else {
    name_emit(node->astnode.forloop.Label);
    fprintf(curfp," = _f2j_in.%s();\n",
       func[node->astnode.forloop.Label->vartype]);
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
write_emit (AST * root)
{
  HASHNODE *hnode;
  AST *temp;
  AST *nodeptr;
  char tmp[100];
  void format_list_emit(AST *, AST **);
  void write_implied_loop_emit(AST *);
  int implied_loop = FALSE;
  extern int ignored_formatting;

  /* 
   * Check to see if there are any implied DO loops in this WRITE
   * statement.  If so, we'll have to generate a for loop to write
   * the data.  Since in that case we will have separate print
   * statements for each iteration of the for loop, we dont want
   * to use println. 
   */

  for(temp=root->astnode.io_stmt.arg_list;temp!=NULL;temp=temp->nextstmt)
    if(temp->nodetype == ImpliedLoop)
    {
      implied_loop = TRUE;
      break;
    }
      
  if(implied_loop)
    fprintf (curfp, "System.out.print(");
  else
    fprintf (curfp, "System.out.println(");

  /* 
   * The following is a cheesy workaround to handle the following type of
   * statement:
   *         write(*, FMT = '( '' Matrix types:'' )' ) 
   * eventually, we should handle any kind of format spec within the
   * quotes, but for now we just treat the whole thing as a string.  
   * 12/4/97 --Keith
   */

  if(root->astnode.io_stmt.fmt_list != NULL)
  {
    fprintf(curfp, "\"%s\"", root->astnode.io_stmt.fmt_list->astnode.constant.number);
    if(root->astnode.io_stmt.arg_list != NULL)
      fprintf(curfp, " + ");
  }

  sprintf(tmp,"%d", root->astnode.io_stmt.format_num);
  if(gendebug)
    printf("***Looking for format statement number: %s\n",tmp);

  /* if there's formatting information for this write statement, use it
   * unless the write statement has an implied do loop.  in that case,
   * we dont know how to handle the formatting, so we ignore it.
   */

  hnode = format_lookup(cur_format_table,tmp);

  if(hnode != NULL && !implied_loop) {
    if(gendebug)
      printf("****FOUND****\n");

    nodeptr = root->astnode.io_stmt.arg_list; 

    format_list_emit(hnode->variable->astnode.label.stmt,&nodeptr);
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

    for( temp = root->astnode.io_stmt.arg_list; 
      temp != NULL; 
      temp = temp->nextstmt) 
    {
      if(temp->nodetype == ImpliedLoop)
      {
        if( temp == root->astnode.io_stmt.arg_list )
          fprintf(curfp,"\"\");\n");

        write_implied_loop_emit(temp);
        if(temp->nextstmt != NULL)
          if(temp->nextstmt->nodetype != ImpliedLoop)
            fprintf(curfp,"System.out.print(");
      }
      else
      {
        fprintf(curfp,"(");
        expr_emit (temp);
        fprintf(curfp,")");
        if(temp->nextstmt != NULL) 
        {
          if(temp->nextstmt->nodetype == ImpliedLoop)
            fprintf (curfp, " + \" \");\n");
          else
            fprintf (curfp, " + \" \" + ");
        }
        else if(implied_loop)
          fprintf (curfp, ");\n");
      }
    }
  }

  if(implied_loop)
    fprintf (curfp, "\nSystem.out.println();\n");
  else
    fprintf (curfp, ");\n");
}

/*****************************************************************************
 *                                                                           *
 * write_implied_loop_emit                                                   *
 *                                                                           *
 * This function generates code for implied DO loops in WRITE statements.    *
 * Dont worry about FORMAT statements.                                       *
 *                                                                           *
 *****************************************************************************/

void
write_implied_loop_emit(AST *node)
{
  char *loopvar = node->astnode.forloop.counter->astnode.ident.name;
  char *valSuffix = ".val";

  if(omitWrappers) {
    if( !isPassByRef(loopvar) )
      valSuffix = "";

    fprintf(curfp,"for(%s%s = ",loopvar,valSuffix); 
    expr_emit(node->astnode.forloop.start);
    fprintf(curfp,"; %s%s <= ",loopvar,valSuffix); 
    expr_emit(node->astnode.forloop.stop);
    if(node->astnode.forloop.incr == NULL)
      fprintf(curfp,"; %s%s++)\n",loopvar,valSuffix); 
    else
    {
      fprintf(curfp,"; %s%s += ",loopvar,valSuffix); 
      expr_emit(node->astnode.forloop.incr);
      fprintf(curfp,")\n"); 
    }
  }
  else
  {
    fprintf(curfp,"for(%s.val = ",loopvar); 
    expr_emit(node->astnode.forloop.start);
    fprintf(curfp,"; %s.val <= ",loopvar); 
    expr_emit(node->astnode.forloop.stop);
    if(node->astnode.forloop.incr == NULL)
      fprintf(curfp,"; %s.val++)\n",loopvar); 
    else
    {
      fprintf(curfp,"; %s.val += ",loopvar); 
      expr_emit(node->astnode.forloop.incr);
      fprintf(curfp,")\n"); 
    }
  }

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
    fprintf(stderr," in implied loop (write stmt)\n");
  }
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
  int i;
  void format_list_emit(AST *, AST **);
  void format_name_emit(AST *);
  char * tok2str(int);

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
            fprintf(curfp,"\"");
            for(i=0;i< atoi(temp->astnode.constant.number);i++)
              fprintf(curfp," ");
            fprintf(curfp,"\"");
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
      if(temp->nextstmt != NULL)
        fprintf(curfp," + ");
      return(temp->nextstmt);
      break;
    case CAT:
      if(gendebug)
        printf("two divs\n");
      fprintf(curfp,"\"\\n\\n\" ");
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
  extern int bad_format_count;

  if(node == NULL) {
    if(gendebug)
      printf("*** BAD FORMATTING\n");
    bad_format_count++;
    fprintf(curfp,"\" NULL \"");
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
*/

  fprintf(curfp,"(");
      expr_emit(node);
  fprintf(curfp,")");
  }
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
  AST *prev = root->prevstmt;
  AST *temp;
  int *tmp_int;

  tmp_int = (int*)f2jalloc(sizeof(int));

  /* if the previous node was a label, this could be a simulated
   * while loop.
   */
  if(prev != NULL)
    if(prev->nodetype == Label)
    {
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
      dl_pop(while_list);
    }

  fprintf (curfp, "if (");
  if (root->astnode.blockif.conds != NULL)
    expr_emit (root->astnode.blockif.conds);

  fprintf (curfp, ")  {\n    ");
  emit (root->astnode.blockif.stmts);
  fprintf (curfp, "}              // Close if()\n");

  if (root->astnode.blockif.elseifstmts != NULL)
    emit (root->astnode.blockif.elseifstmts);

  if (root->astnode.blockif.elsestmts != NULL)
    emit (root->astnode.blockif.elsestmts);
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

  fprintf(curfp, "while (");
  if (root->astnode.blockif.conds != NULL)
    expr_emit (root->astnode.blockif.conds);
  fprintf (curfp, ")  {\n    ");
  emit (root->astnode.blockif.stmts);
  fprintf (curfp, "}              // Close if()\n");

}

/*****************************************************************************
 *                                                                           *
 * elseif_emit                                                               *
 *                                                                           *
 * This function generates the code for the fortran 'else if'                *
 * construct.                                                                *
 *                                                                           *
 *****************************************************************************/

void
elseif_emit (AST * root)
{
  fprintf (curfp, "else if (");
  if (root->astnode.blockif.conds != NULL)
    expr_emit (root->astnode.blockif.conds);
  fprintf (curfp, ")  {\n    ");
  emit (root->astnode.blockif.stmts);
  fprintf (curfp, "}              // Close else if()\n");
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
 * call_emit                                                                 *
 *                                                                           *
 * This procedure implements Lapack and Blas type methods.                   *
 * They are translated to static method invocations.                         *
 * This is not a portable solution, it is specific to                        *
 * the Blas and Lapack.                                                      *
 *                                                                           *
 *****************************************************************************/

void
call_emit (AST * root)
{
  AST *temp;
  char *tempname, *com_prefix;
  HASHNODE *hashtemp, *ht, *ht2;
  int adapter = FALSE;
  int needs_adapter(AST *);
  void insert_adapter(AST *);
  void insert_methcall(Dlist, AST *);
  char *get_common_prefix(char *);

  assert (root != NULL);

  if(gendebug)
    printf("@##@ in call_emit, %s\n",root->astnode.ident.name);

  adapter = needs_adapter(root);

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

    if((root->astnode.ident.arraylist->nodetype == EmptyArgList) ||
       (root->astnode.ident.arraylist == NULL)) {

      /* no args.  either function or subroutine. */

      fprintf(curfp,"((%s)_%s_meth.invoke(null,null)).%sValue()",
        java_wrapper[root->vartype], root->astnode.ident.name, 
        returnstring[root->vartype]);

      if(root->nodetype == Call)
        fprintf(curfp,";");

      return;
    }
    else if (root->nodetype == Call) {

      /* subroutine with args.  */

      int cnt = 0;

      for( temp = root->astnode.ident.arraylist; temp; temp = temp->nextstmt)
        cnt++;
      
      /* create object array to hold the args */

      fprintf(curfp," Object [] _%s_args = new Object[%d];\n",
         root->astnode.ident.name, cnt);

      /* foreach arg, assign that arg to an element of the object array */

      cnt = 0;
      for( temp = root->astnode.ident.arraylist; temp; temp = temp->nextstmt)
      {
        fprintf(curfp,"_%s_args[%d] = ", root->astnode.ident.name, cnt);

        if((temp->nodetype == Identifier) && (temp->astnode.ident.arraylist == NULL))
        {
          expr_emit (temp);
        }
        else
        {
          fprintf(curfp,"new %s(", java_wrapper[temp->vartype]);
          expr_emit (temp);
          fprintf(curfp,")");
        }

        fprintf(curfp, ";\n");

        cnt++;
      }

      fprintf(curfp,"_%s_meth.invoke(null,_%s_args);\n",
        root->astnode.ident.name, root->astnode.ident.name);
      return;
    }
    else   /* function with args. */
    {
      /* add this call to the list of calls which need adapters */

      insert_methcall(methcall_list,root);

      fprintf(curfp,"%s_methcall",root->astnode.ident.name);
    }
  }

  /* analyze this function call to determine if we need to generate an 
   * 'adapter' which will simulate passing array elements by reference.
   */

  else if( adapter )
  {
    if(gendebug)
      printf("wow, guess we need an adapter for %s.\n", 
        root->astnode.ident.name);

    insert_adapter(root);

    /* Assume all methods that are invoked are static.  */
    fprintf (curfp, "%s_adapter", root->astnode.ident.name);
  }
  else
    fprintf (curfp, "%s.%s", tempname, root->astnode.ident.name);

  if((root->astnode.ident.arraylist->nodetype == EmptyArgList) ||
     (root->astnode.ident.arraylist == NULL))
  {
    /* the arg list is empty, just emit "()" and return */

    if(root->nodetype == Call)
      fprintf (curfp, "();\n");
    else
      fprintf (curfp, "()");
    return;
  }

  fprintf (curfp, "(");

  /* for reflective method call adapters, the first paramter should
   * be the method to invoke.
   */

  if(type_lookup(cur_args_table, root->astnode.ident.name)) {
    fprintf(curfp,"_%s_meth",root->astnode.ident.name);
    if(root->astnode.ident.arraylist != NULL)
      fprintf(curfp,",");
  }

  /* look up the function that we are calling so that we may compare
   * the parameters.
   */

  if(gendebug)
    printf("Looking up function name %s, ", root->astnode.ident.name);

  if((hashtemp=type_lookup(function_table, root->astnode.ident.name)) != NULL)
  {
    AST *t2;

    if(gendebug)
      printf("Found!!\n");

    temp = root->astnode.ident.arraylist;
    t2=hashtemp->variable->astnode.source.args;

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
         ht2 = type_lookup(cur_args_table, temp->astnode.ident.name);

         if(gendebug)
           printf("CAlling func-array_emit\n");

         if(t2->astnode.ident.arraylist)     /* it is expecting an array */
         {
           fprintf(curfp,"%s%s",com_prefix,temp->astnode.ident.name);
           func_array_emit(temp->astnode.ident.arraylist,ht,
              temp->astnode.ident.name, ht2!=NULL, TRUE);
         }
         else                                /* it is not expecting an array */
         {
           /* In this case we are passing the array element to the
            * adapter, so we dont wrap it in an object.
            */

           fprintf(curfp,"%s%s",com_prefix,temp->astnode.ident.name);

           if(omitWrappers) {
             if(adapter && t2->astnode.ident.passByRef)
               func_array_emit(temp->astnode.ident.arraylist,ht,
                 temp->astnode.ident.name, ht2!=NULL, TRUE);
             else
               func_array_emit(temp->astnode.ident.arraylist,ht,
                 temp->astnode.ident.name, ht2!=NULL, FALSE);
           }
           else
           {
             if(adapter)
               func_array_emit(temp->astnode.ident.arraylist,ht,
                 temp->astnode.ident.name, ht2!=NULL, TRUE);
             else
               func_array_emit(temp->astnode.ident.arraylist,ht,
                 temp->astnode.ident.name, ht2!=NULL, FALSE);
           }
         }
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
         if(t2->astnode.ident.arraylist)     /* it is expecting an array */
         {
           if(gendebug)
             printf("expecting array\n");

           expr_emit(temp);
         }
         else
         {
           if(gendebug)
             printf("NOT expecting array\n");

           if(omitWrappers && !t2->astnode.ident.passByRef) {
             fprintf(curfp,"%s%s[0]",com_prefix, temp->astnode.ident.name);
           }
           else
           {
             fprintf(curfp,"new %s(", wrapper_returns[t2->vartype]);
             fprintf(curfp,"%s%s[0]", com_prefix,temp->astnode.ident.name);
             fprintf(curfp,")");
           }
         }
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
         if(t2->astnode.ident.passByRef != 
            isPassByRef(temp->astnode.ident.name))
         {
           if(isPassByRef(temp->astnode.ident.name))
             fprintf(curfp,"%s%s.val",com_prefix,temp->astnode.ident.name);
           else
             fprintf(stderr,"Internal error: %s should not be primitive\n",
               temp->astnode.ident.name);
         }
         else
         {
           if( temp->vartype != t2->vartype )
             fprintf(curfp,"(%s) ( ",returnstring[t2->vartype]);

           expr_emit(temp);

           if( temp->vartype != t2->vartype )
             fprintf(curfp,")");
         }
       }
       else if(omitWrappers && (temp->nodetype == Constant))
       {
         if(t2->astnode.ident.passByRef) {
           fprintf(curfp,"new %s(", wrapper_returns[t2->vartype]);
           expr_emit(temp);
           fprintf(curfp,")"); 
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
       else if(temp->nodetype == EmptyArgList)
       {
          ;  /* do nothing */
       }
         /* 
          * Otherwise, use wrappers.
          */
       else 
       {
         if(omitWrappers) {
           if(t2->astnode.ident.passByRef)
             fprintf(curfp,"new %s(", wrapper_returns[t2->vartype]);
         }
         else
         {
           fprintf(curfp,"new %s(", wrapper_returns[t2->vartype]);
         }

         if(gendebug) {
           printf("emitting wrapped expr...\n");
           printf("   wrapper type is %s\n",wrapper_returns[t2->vartype]);
           printf("   data type is %s\n",returnstring[temp->vartype]);
         }
  
         /* emit a cast if necessary */

         if( temp->vartype != t2->vartype )
           fprintf(curfp,"(%s) ( ",returnstring[t2->vartype]);

         expr_emit(temp);

         if( temp->vartype != t2->vartype )
           fprintf(curfp,")");

         if(omitWrappers) {
           if(t2->astnode.ident.passByRef)
             fprintf(curfp,")");
         }
         else
         {
           fprintf(curfp,")");
         }
       }
       if(t2 != NULL)
         t2 = t2->nextstmt;
       if(temp->nextstmt != NULL)
         fprintf(curfp, ",");
    }
  }
  else
  {
    /* General case.  we dont know what the parameters are supposed to be
     * so we have to guess here.
     */

    temp = root->astnode.ident.arraylist;

    for( ; temp != NULL; temp = temp->nextstmt)
    {
      if(((temp->nodetype == Identifier) && (temp->astnode.ident.arraylist == NULL)) ||
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
}				/*  Close call_emit().  */

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
  AST *temp;
  HASHNODE *hashtemp;

  /* first, check for a null parameter list.  if there are no parameters, 
   * we certainly wont need an adapter.
   */
  if((root->astnode.ident.arraylist->nodetype == EmptyArgList) ||
     (root->astnode.ident.arraylist == NULL))
    return 0;

  if(gendebug)
    printf("in needs_adapter: Looking up function name %s, ", 
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

  return 0;
}

/*****************************************************************************
 *                                                                           *
 * spec_emit                                                                 *
 *                                                                           *
 * This function handles code generation for specification statements.       *
 * Actually, there isn't a whole lot to do for spec statements.              *
 *                                                                           *
 *****************************************************************************/

void
spec_emit (AST * root)
{
  void name_emit (AST *);

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
  char *name, *class, *desc, *com_prefix;
  HASHNODE *isArg, *typenode, *ht;
  enum returntype ltype, rtype;
  CPNODE *c;

  void substring_assign_emit(AST *);
  void name_emit (AST *);

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
  printf("## ## codegen: ltype = %s (%d)\n",returnstring[ltype], ltype);
  printf("## ## codegen: rtype = %s (%d)\n",returnstring[rtype], rtype);

  /* handle lhs substring operations elsewhere */
  if(root->astnode.assignment.lhs->nodetype == Substring)
  {
    substring_assign_emit(root);
    return;
  }

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
                      valueOf_descriptor[ltype]);

      code_one_op_w(jvm_invokestatic, c->index);

      c = newMethodref(cur_const_table,numeric_wrapper[ltype], 
                       numericValue_method[ltype],
                      numericValue_descriptor[ltype]);

      code_one_op_w(jvm_invokevirtual, c->index);
    }
    else if( (ltype == Logical) && (rtype != String) )
    {
      /* boolean = numeric value */
      expr_emit (root->astnode.assignment.rhs);
      fprintf(curfp," == 0 ? false : true");
      if(rtype == Integer) {
        code_one_op_w(jvm_ifeq, 7);
        code_zero_op(jvm_iconst_0);
        code_one_op_w(jvm_goto, 4);
        code_zero_op(jvm_iconst_1);
      }
      else if(rtype == Double) {
        code_zero_op(jvm_dconst_0);
        code_zero_op(jvm_dcmpl);
        code_one_op_w(jvm_ifne, 7);
        code_zero_op(jvm_iconst_1);
        code_one_op_w(jvm_goto, 4);
        code_zero_op(jvm_iconst_0);
      }
      else
        fprintf(stderr,"WARNING: unsupported cast.\n");
    }
    else
    {
      if(typeconv_matrix[rtype][ltype] == jvm_nop)
        fprintf(stderr,"WARNING: unable to handle this cast!\n");

      /* numeric value = numeric value of some other type */
      fprintf(curfp,"(%s)(",returnstring[ltype]);
      expr_emit (root->astnode.assignment.rhs);
      fprintf(curfp,")");
      code_zero_op(typeconv_matrix[rtype][ltype]);
    }
  }
  else   /* lhs and rhs have same types, everything is cool */
    expr_emit (root->astnode.assignment.rhs);

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

    class = strdup(com_prefix);
    class[strlen(class)-1] = '\0';
  }

  printf("in assign_emit, class = %s, name = %s, desc = %s\n",class, name, desc);
  
  if(root->astnode.assignment.lhs->astnode.ident.arraylist == NULL) {
    /* LHS is not an array reference (note that the variable may be
     * an array, but it isn't being indexed here).  for bytecode,
     * we now generate a store or putfield instruction, depending
     * on whether the variable is wrapped or not.
     */
    if(omitWrappers && 
       !isPassByRef(root->astnode.assignment.lhs->astnode.ident.name)) 
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
      code_one_op_w(jvm_putstatic, c->index);
    }
    else {
      int vt = root->astnode.assignment.lhs->vartype;
      /* this is a wrapped primitive.  the objectref and value should
       * already be sitting on the stack, so now we generate a putfield
       * instruction.
       */
      c = newFieldref(cur_const_table, full_wrappername[vt], "val", 
             val_descriptor[vt]);
      code_one_op_w(jvm_putfield, c->index);
    }
  }
  else {
    /* the LHS is an array access.  currently the stack holds a reference
     * to the array, the array index, and the RHS expression.  all we need
     * to do now is generate an array store instruction (e.g. iastore).
     */
    code_zero_op(array_store_opcodes[root->astnode.assignment.lhs->vartype]);
  }
}

/*****************************************************************************
 *                                                                           *
 * substring_assign_emit                                                     *
 *                                                                           *
 * This function handles situations in which the lhs of an                   *
 * assignment statement is a substring operation.  For example:              *
 *   a(3:4) = 'hi'                                                           *
 * We haven't figured out an elegant way to handle this in Java,             *
 * but we do handle it, as follows:                                          *
 *                                                                           *
 *  int E1, E2;                                                              *
 *  E1 = 3;                                                                  *
 *  E2 = 4;                                                                  *
 *  a = new StringW(                                                         *
 *        a.val.substring(0,E1-1) +                                          *
 *        "hi".substring(0,E2-E1+1) +                                        *
 *        a.val.substring(E2,a.val.length())                                 *
 *      );                                                                   *
 *                                                                           *
 * The resulting code looks pretty bad because we have to be                 *
 * prepared to handle rhs strings that are too big to fit in                 *
 * the lhs substring.                                                        *
 *                                                                           *
 * luckily, java provides the String.substring() method, which               *
 * helps out a lot.                                                          *
 *                                                                           *
 *****************************************************************************/

void
substring_assign_emit(AST *root)
{
  AST *lhs = root->astnode.assignment.lhs;
  AST *rhs = root->astnode.assignment.rhs;
  char *lname = lhs->astnode.ident.name;

  if(gendebug)
    printf("substring_assign_emit\n");

  fprintf(curfp,"{\n  int E1, E2;\n");
  fprintf(curfp,"  E1 = ");
  expr_emit(lhs->astnode.ident.arraylist);
  fprintf(curfp,";\n");

  fprintf(curfp,"  E2 = ");
  expr_emit(lhs->astnode.ident.arraylist->nextstmt);
  fprintf(curfp,";\n");

  if(omitWrappers) {
    fprintf(curfp,"%s = new String(",lname);
    if(isPassByRef(lname))
      fprintf(curfp,"%s.val.substring(0,E1-1) + ", lname);
    else
      fprintf(curfp,"%s.substring(0,E1-1) + ", lname);
  }
  else
  {
    fprintf(curfp,"%s = new StringW(",lname);
    fprintf(curfp,"%s.val.substring(0,E1-1) + ", lname);
  }

  if(rhs->vartype == Character)
  {
    /* 
     * Java's Character class doesn't have a static toString
     * method, so we have to create a new character object first.
     */

    fprintf(curfp,"new Character(");
    expr_emit(rhs);
    fprintf(curfp,").toString().substring(0,E2-E1+1) + ");
  }
  else if(rhs->vartype == String)
  {
    expr_emit(rhs);
    fprintf(curfp,".substring(0,E2-E1+1) + ");
  }
  else
  {
    char *tempstring = strdup(returnstring[rhs->vartype]);

    *tempstring = toupper (*tempstring);

    fprintf(curfp,"%s.toString(", tempstring);
    expr_emit(rhs);
    fprintf(curfp,").substring(0,E2-E1+1) + ");
  }

  if(omitWrappers && !isPassByRef(lname))
    fprintf(curfp,"%s.substring(E2,%s.length())",lname,lname);
  else
    fprintf(curfp,"%s.val.substring(E2,%s.val.length())",lname,lname);

  fprintf(curfp,");\n");

  fprintf(curfp,"}\n");
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
  return ( (AST *) dl_val(dl_last(l)) );
}

/*****************************************************************************
 *                                                                           *
 * dl_int_search                                                             *
 *                                                                           *
 * This function searches for a value in a dlist of                          *
 * integers.  Returns TRUE if the value is found, FALSE                      *
 * otherwise.                                                                *
 *                                                                           *
 *****************************************************************************/

int
dl_int_search(Dlist l, int val)
{
  Dlist p;

  dl_traverse(p,l)
    if( *((int *)p->val) == val )
      return TRUE;

  return FALSE;
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
  HASHNODE *hashtemp;
  AST *ptr, *t2, *this_call, *other_call;
  int i, found = FALSE, diff = FALSE;
  int this_arg_is_arrayacc, other_arg_is_arrayacc;
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

      /* this function call is already in the list.  now we must determine whether
       * the prototypes of the adapters would be the same.  If so, there's no need
       * to insert this node in the adapter list.  If the prototypes would be 
       * different, then we must insert this node.
       */
  
      if(gendebug)
        printf("** %s is already in adapter_list.  now checking args.\n",
          node->astnode.ident.name);

      if((hashtemp=type_lookup(function_table, node->astnode.ident.name)) != NULL)
      {
        if(gendebug) {
          printf("** \n");
          printf("** found prototype.\n");
        }
  
        this_call = node->astnode.ident.arraylist;
        other_call = ptr->astnode.ident.arraylist;
  
        t2 = hashtemp->variable->astnode.source.args;
  
        diff = FALSE;

        for(i=0 ; this_call != NULL; this_call = this_call->nextstmt, i++)
        {
          if(t2 == NULL)
            break;

          if(gendebug)
            printf("** arg %d\n",i);
  
          if( other_call == NULL )
          {
            fprintf(stderr,"2:Function calls to %s in unit %s ", 
              node->astnode.ident.name, unit_name);
            fprintf(stderr,"don't have same number of params\n");
            return;
          }

          this_arg_is_arrayacc = (this_call->nodetype == Identifier) &&
                (this_call->astnode.ident.arraylist != NULL) &&
                type_lookup(cur_array_table, this_call->astnode.ident.name);

          if(gendebug)
            printf("** this_arg_is_arrayacc = %d\n",this_arg_is_arrayacc);

          other_arg_is_arrayacc = (other_call->nodetype == Identifier) &&
                (other_call->astnode.ident.arraylist != NULL) &&
                type_lookup(cur_array_table, other_call->astnode.ident.name);

          if(gendebug)
            printf("** other_arg_is_arrayacc = %d\n",other_arg_is_arrayacc);

          if( (! t2->astnode.ident.arraylist) &&
              (this_arg_is_arrayacc != other_arg_is_arrayacc ))
          {
            if(gendebug)
              printf("** setting diff = TRUE\n");

            diff = TRUE;
          }
  
          if(gendebug)
            printf("** blah\n");

          other_call = other_call->nextstmt;

          if(t2 != NULL)
            t2 = t2->nextstmt;
        }
  
        if(!diff) {
          if(gendebug)
            printf("** found an equivalent adapter.  no need to insert.\n");

          return;
        }
      }
      else {
        if(gendebug)
          printf("** cant find prototype...returning.\n");  
  
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
 * emit_adapters                                                             *
 *                                                                           *
 * This function generates any adapters necessary to                         *
 * allow functions to pass array elements by reference.                      *
 *                                                                           *
 *****************************************************************************/

void
emit_adapters()
{
  HASHNODE *hashtemp;
  AST * arg, * temp;
  char *tempname;
  Dlist p;
  int i;


  dl_traverse(p,adapter_list)
  {
    hashtemp = type_lookup(function_table, 
        ((AST *)dl_val(p))->astnode.ident.name);

    if(hashtemp == NULL) {
      fprintf(stderr,"Error: cant generate adapter for %s\n",
         ( (AST *) dl_val(p) )->astnode.ident.name);
      continue;
    }

    fprintf(curfp,"// adapter for %s\n", 
      ( (AST *) dl_val(p) )->astnode.ident.name);
  
    /* first generate the method header */

    if(hashtemp->variable->nodetype == Function)
      fprintf(curfp,"private static %s %s_adapter(", 
          returnstring[hashtemp->variable->astnode.source.returns],
          hashtemp->variable->astnode.source.name->astnode.ident.name);
    else
      fprintf(curfp,"private static void %s_adapter(", 
          hashtemp->variable->astnode.source.name->astnode.ident.name);

    temp = hashtemp->variable->astnode.source.args;
    arg = ((AST *)dl_val(p))->astnode.ident.arraylist;
    
    for(i = 0; arg != NULL ; arg = arg->nextstmt, i++)
    {
      if(temp == NULL) {
        fprintf(stderr,"Error: mismatch between call to %s and prototype\n",
           ( (AST *) dl_val(p) )->astnode.ident.name);
        break;
      }

      if(temp->astnode.ident.arraylist) {
        fprintf(curfp,"%s [] arg%d , int arg%d_offset ", 
          returnstring[temp->vartype], i, i);
      }
      else if ( (arg->nodetype == Identifier) && 
                (arg->astnode.ident.arraylist != NULL) &&
                type_lookup(cur_array_table,arg->astnode.ident.name) )
      {
        if(omitWrappers && !temp->astnode.ident.passByRef)
          fprintf(curfp,"%s arg%d ", returnstring[temp->vartype], i);
        else
          fprintf(curfp,"%s [] arg%d , int arg%d_offset ", 
            returnstring[temp->vartype], i, i);
      }
      else if( type_lookup(cur_external_table, arg->astnode.ident.name) )
      {
        fprintf(curfp,"Object arg%d ", i);
      }
      else
      {
        if(omitWrappers && !temp->astnode.ident.passByRef)
          fprintf(curfp,"%s arg%d ", returnstring[temp->vartype], i);
        else
          fprintf(curfp,"%s arg%d ", wrapper_returns[temp->vartype], i);
      }

      if(temp != NULL)
        temp = temp->nextstmt;
      if(arg->nextstmt != NULL)
        fprintf(curfp,",");
    }

    fprintf(curfp,")\n{\n");

    temp = hashtemp->variable->astnode.source.args;
    arg = ((AST *)dl_val(p))->astnode.ident.arraylist;
    
    for(i = 0; arg != NULL ; arg = arg->nextstmt, i++)
    {
      if(temp == NULL)
        break;

      if((arg->nodetype == Identifier) && (arg->astnode.ident.arraylist != NULL) &&
            type_lookup(cur_array_table,arg->astnode.ident.name) &&
            !temp->astnode.ident.arraylist)
      {
        if(omitWrappers) {
          if(temp->astnode.ident.passByRef)
            fprintf(curfp,"%s _f2j_tmp%d = new %s(arg%d[arg%d_offset]);\n", 
              wrapper_returns[temp->vartype], i, wrapper_returns[temp->vartype], i, i);
        }
        else
        {
          fprintf(curfp,"%s _f2j_tmp%d = new %s(arg%d[arg%d_offset]);\n", 
            wrapper_returns[temp->vartype], i, wrapper_returns[temp->vartype], i, i);
        }
      }

      if(temp != NULL)
        temp = temp->nextstmt;
    }

    /*  now emit the call here */

    tempname = strdup( ((AST *) dl_val(p))->astnode.ident.name );
    *tempname = toupper(*tempname);

    if(hashtemp->variable->nodetype == Function)
    {
      fprintf(curfp,"%s %s_retval;\n\n", 
          returnstring[hashtemp->variable->astnode.source.returns],
          hashtemp->variable->astnode.source.name->astnode.ident.name);

      fprintf(curfp,"%s_retval = %s.%s(", ((AST *) dl_val(p))->astnode.ident.name,
         tempname,  ((AST *) dl_val(p))->astnode.ident.name );
    }
    else
      fprintf(curfp,"\n%s.%s(",tempname,  ((AST *) dl_val(p))->astnode.ident.name );
    
    temp = hashtemp->variable->astnode.source.args;
    arg = ((AST *)dl_val(p))->astnode.ident.arraylist;
    
    for(i = 0; arg != NULL ; arg = arg->nextstmt, i++)
    {
      if(temp == NULL)
        break;

      if((arg->nodetype == Identifier) && (arg->astnode.ident.arraylist != NULL) &&
            type_lookup(cur_array_table,arg->astnode.ident.name) &&
            !temp->astnode.ident.arraylist)
      {
        if(omitWrappers && !temp->astnode.ident.passByRef)
          fprintf(curfp,"arg%d",i);
        else
          fprintf(curfp,"_f2j_tmp%d",i);
      }
      else if((arg->nodetype == Identifier) &&
            type_lookup(cur_array_table,arg->astnode.ident.name) &&
            temp->astnode.ident.arraylist)
         fprintf(curfp,"arg%d, arg%d_offset",i,i);
      else
         fprintf(curfp,"arg%d",i);

      if(temp != NULL)
        temp = temp->nextstmt;
      if(arg->nextstmt != NULL)
        fprintf(curfp,",");
    }

    fprintf(curfp,");\n\n");

    /*  assign the temp variable to the array element */

    temp = hashtemp->variable->astnode.source.args;
    arg = ((AST *)dl_val(p))->astnode.ident.arraylist;
    
    for(i = 0; arg != NULL ; arg = arg->nextstmt, i++)
    {
      if(temp == NULL)
        break;

      if((arg->nodetype == Identifier) && (arg->astnode.ident.arraylist != NULL) &&
            type_lookup(cur_array_table,arg->astnode.ident.name) &&
            !temp->astnode.ident.arraylist)
      {
        if(omitWrappers) {
          if(temp->astnode.ident.passByRef)
            fprintf(curfp,"arg%d[arg%d_offset] = _f2j_tmp%d.val;\n",i,i,i);
        }
        else
        {
          fprintf(curfp,"arg%d[arg%d_offset] = _f2j_tmp%d.val;\n",i,i,i);
        }
      }

      if(temp != NULL)
        temp = temp->nextstmt;
    }

    if(hashtemp->variable->nodetype == Function)
    {
      fprintf(curfp,"\nreturn %s_retval;\n", 
          hashtemp->variable->astnode.source.name->astnode.ident.name);
    }

    fprintf(curfp,"}\n\n");
  }
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
  HASHNODE *ht;
  AST *temp, *arg;
  Dlist p, tmplist;
  int i, count = 0;

  dl_traverse(p,methcall_list) {
    tmplist = (Dlist) dl_val(p);
    
    temp = (AST *) dl_val(dl_first(tmplist));

    fprintf(curfp,"// reflective method invocation for %s\n",temp->astnode.ident.name);
    fprintf(curfp,"private static %s %s_methcall( java.lang.reflect.Method _funcptr", 
       returnstring[temp->vartype], temp->astnode.ident.name);

    count = 0;

    for(arg = temp->astnode.ident.arraylist; arg != NULL; arg = arg->nextstmt) {
      fprintf(curfp,",");

      if(omitWrappers) {
        if( arg->nodetype == Identifier ) {
          ht = type_lookup(cur_type_table,arg->astnode.ident.name);
          if(ht)
            fprintf(curfp," %s _arg%d ", returnstring[ht->variable->vartype], count);
          else
            fprintf(curfp," %s _arg%d ", returnstring[arg->vartype], count);
        }
        else if( arg->nodetype == Constant )
          fprintf(curfp," %s _arg%d ", 
            returnstring[get_type(arg->astnode.constant.number)], count);
        else
          fprintf(curfp," %s _arg%d ", returnstring[arg->vartype], count);
      }
      else
      {
        if( arg->nodetype == Identifier ) {
          ht = type_lookup(cur_type_table,arg->astnode.ident.name);
          if(ht)
            fprintf(curfp," %s _arg%d ", wrapper_returns[ht->variable->vartype], count);
          else
            fprintf(curfp," %s _arg%d ", wrapper_returns[arg->vartype], count);
        }
        else if( arg->nodetype == Constant )
          fprintf(curfp," %s _arg%d ", 
            wrapper_returns[get_type(arg->astnode.constant.number)], count);
        else
          fprintf(curfp," %s _arg%d ", wrapper_returns[arg->vartype], count);
      }

      count++;
    }

    fprintf(curfp,")\n   throws java.lang.reflect.InvocationTargetException,\n");
    fprintf(curfp,"          java.lang.IllegalAccessException\n{\n"); 

    fprintf(curfp,"Object [] _funcargs = new Object [%d];\n", count);
    fprintf(curfp,"%s _retval;\n", returnstring[temp->vartype]);

    i = 0;
    for(arg = temp->astnode.ident.arraylist; arg != NULL; arg = arg->nextstmt, i++) {
      if(omitWrappers) {
        if( arg->nodetype == Identifier ) {
          ht = type_lookup(cur_type_table,arg->astnode.ident.name);
          if(ht)
            fprintf(curfp," _funcargs[%d] = new %s(_arg%d);\n",
              i,java_wrapper[ht->variable->vartype], i);
          else
            fprintf(curfp," _funcargs[%d] = new %s(_arg%d);\n",
              i,java_wrapper[arg->vartype], i);
        }
        else if( arg->nodetype == Constant )
            fprintf(curfp," _funcargs[%d] = new %s(_arg%d);\n",
              i,java_wrapper[get_type(arg->astnode.constant.number)], i);
        else
            fprintf(curfp," _funcargs[%d] = new %s(_arg%d);\n",
              i,java_wrapper[arg->vartype], i);
      }
      else
      {
        fprintf(curfp," _funcargs[%d] = _arg%d;\n",i,i);
      }
    }

    fprintf(curfp,"_retval = ( (%s) _funcptr.invoke(null,_funcargs)).%sValue();\n",
      java_wrapper[temp->vartype], returnstring[temp->vartype]);

    fprintf(curfp,"return _retval;\n");
    fprintf(curfp,"}\n"); 
  } 
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
}

/*****************************************************************************
 *                                                                           *
 * check_code_size                                                           *
 *                                                                           *
 * determines whether the byte array is large enough to include an           *
 * instruction with the given width.  if so...fine, just return.  if not...  *
 * then we must reallocate cur_code with more memory.                        *
 *                                                                           *
 *****************************************************************************/

void
check_code_size(int width)
{
  /* note that we're somewhat misusing the cur_code->code_length variable.
   * it should hold the length of the code, not the amount of memory allocated.
   * however, we will temporarily use it to keep track of memory allocation
   * and after generating all the code for this method, we will replace the
   * code_length variable with the correct number.  --kgs 4/26/00
   */
  if(cur_code->attr.Code->code == NULL) {
    cur_code->attr.Code->code = (u1 *)f2jalloc(CODE_ALLOC_INIT * sizeof(u1));
    cur_code->attr.Code->code_length = CODE_ALLOC_INIT;
    return;
  }

  if(pc+width > cur_code->attr.Code->code_length) {
    cur_code->attr.Code->code_length += CODE_ALLOC_CHUNK;
    cur_code->attr.Code->code = (u1 *)f2jrealloc(cur_code->attr.Code->code,
                  cur_code->attr.Code->code_length);
  }
}

/*****************************************************************************
 *                                                                           *
 * code_zero_op                                                              *
 *                                                                           *
 * generate code for instructions with no operands (e.g. type conversions).  *
 *                                                                           *
 *****************************************************************************/

void
code_zero_op(enum _opcode op)
{
  u1 this_opcode = op;

  dec_stack(jvm_opcode[op].stack_pre);
  check_code_size(jvm_opcode[op].width);
  lastOp = op;
  memcpy(cur_code->attr.Code->code + pc, &this_opcode, sizeof(this_opcode));
  printf("%d %s\n", pc, jvm_opcode[op].op);
  pc += jvm_opcode[op].width;
  inc_stack(jvm_opcode[op].stack_post);
}

/*****************************************************************************
 *                                                                           *
 * code_one_op                                                               *
 *                                                                           *
 * generate code for instructions with a single one-byte operand.            *
 * (e.g. loads,stores,etc).                                                  *
 *                                                                           *
 *****************************************************************************/

void
code_one_op(enum _opcode op, int opval)
{
  u1 this_opcode = op;
  u1 this_operand = (u1)opval;

  /* if this is a 'wide' op, then call code_one_op_w() */

  if(jvm_opcode[op].width > 2) {
    code_one_op_w(op,opval); 
  }
  else {
    /* check for loss of information in the int->u1 cast */
    if( (int)this_operand != opval ) {
      fprintf(stderr,"WARNING: code_one_op() opval lost information.  ");
      fprintf(stderr,"  opcode = %s, ", jvm_opcode[op].op);
      fprintf(stderr,"  index = %d\n", opval);
    }

    dec_stack(jvm_opcode[op].stack_pre);
    check_code_size(jvm_opcode[op].width);
    lastOp = op;
    memcpy(cur_code->attr.Code->code + pc,
           &this_opcode, sizeof(this_opcode));
    memcpy(cur_code->attr.Code->code + pc + 1, 
           &this_operand, sizeof(this_operand));
    printf("%d %s %d\n",pc, jvm_opcode[op].op, this_operand);
    pc += jvm_opcode[op].width;
    inc_stack(jvm_opcode[op].stack_post);
  }
}

/*****************************************************************************
 *                                                                           *
 * code_one_op_w                                                             *
 *                                                                           *
 * generate code for instructions with a single two-byte operand.            *
 * (e.g. invokevirtual, invokestatic, invokespecial).                        *
 *                                                                           *
 *****************************************************************************/

void
code_one_op_w(enum _opcode op, u2 index)
{
  u1 this_opcode = op;
  u2 u2BigEndian(u2);
  u2 this_operand = u2BigEndian(index);
  char *this_desc;
  CPNODE *c;
  struct stack_info * calcStack(char *), *stackinf;
  int stack_increment, stack_decrement;

  /* unfortunately, must first check some special cases to determine proper
   * stack increment and decrement.
   */

  if((op == jvm_invokespecial) || (op == jvm_invokevirtual)
   || (op == jvm_invokestatic))
  {
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

    stack_increment = stackinf->ret_len;
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
        stack_decrement = 0;
        break;
      case jvm_putstatic:
        stack_increment = 0;
        stack_decrement = tmpsize;
        break;
      case jvm_getfield:
        stack_increment = tmpsize;
        stack_decrement = 1;
        break;
      case jvm_putfield:
        stack_increment = 0;
        stack_decrement = tmpsize + 1;
        break;
      default:
        fprintf(stderr,"code_one_op_w(): unexpected op type\n");
        break;  /* ansi compliance */
    }
  }
  else {
    /* else we can determine the stack decrement from a table.  */
    stack_decrement = jvm_opcode[op].stack_pre;
    stack_increment = jvm_opcode[op].stack_post;
  }

  dec_stack(stack_decrement);
  check_code_size(jvm_opcode[op].width);
  lastOp = op;
  memcpy(cur_code->attr.Code->code + pc, &this_opcode, sizeof(this_opcode));
  memcpy(cur_code->attr.Code->code + pc + 1, &this_operand, sizeof(this_operand));
  printf("%d %s #%d\n", pc, jvm_opcode[op].op, index);
  pc += jvm_opcode[op].width;
  inc_stack(stack_increment);
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
  tmp->attr.Code->code = NULL;
  tmp->attr.Code->exception_table_length = 0;
  tmp->attr.Code->exception_table = NULL;
  tmp->attr.Code->attributes_count = 0;
  tmp->attr.Code->attributes = NULL;

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
  u4 u4BigEndian(u4);
  u2 u2BigEndian(u2);
  CPNODE *c;

  CPNODE* cp_insert(Dlist, struct cp_info *, char);
  char *strdup(const char *), *lowercase(char *);
  void cp_dump(Dlist);
 
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

printf("creating new entry, this -> %s\n",name);

  newnode = (struct cp_info *)f2jalloc(sizeof(struct cp_info));
  newnode->tag = CONSTANT_Utf8;
  newnode->cpnode.Utf8.length = strlen(name);
  newnode->cpnode.Utf8.bytes = (u1 *)f2jalloc(newnode->cpnode.Utf8.length);
  strncpy((char *)newnode->cpnode.Utf8.bytes, name, newnode->cpnode.Utf8.length);

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

  tmp->methods_count = 1;
  tmp->methods = make_dl();

  meth_tmp = beginNewMethod(ACC_PUBLIC);

  c = newMethodref(cur_const_table,"java/lang/Object", "<init>", "()V");

  code_zero_op(jvm_aload_0);
  code_one_op_w(jvm_invokespecial, c->index);
  code_zero_op(jvm_return);
  
  endNewMethod(meth_tmp, "<init>", "()V", 1);

  dl_insert_b(tmp->methods, meth_tmp);

  return tmp;
}

/*****************************************************************************
 *                                                                           *
 * beginNewMethod                                                            *
 *                                                                           *
 * Creates a new method structure with the given access flags.               *
 *                                                                           *
 *****************************************************************************/

struct method_info *
beginNewMethod(u2 flags)
{
  struct method_info *tmp;

  tmp = (struct method_info *)f2jalloc(sizeof(struct method_info));
  tmp->access_flags = flags;

  tmp->attributes_count = 1;
  tmp->attributes = make_dl();

  cur_code = newCodeAttribute();

  stacksize = pc = 0;

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
endNewMethod(struct method_info * meth, char * name, char * desc, u2 mloc)
{
  CPNODE *c;

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

  /* attribute_length is calculated as follows:
   *   max_stack               2 bytes
   *   max_locals              2 bytes
   *   code_length             4 bytes
   *   code                   pc bytes
   *   exception_table_length  2 bytes
   *   exception_table         0 bytes  (no exceptions generated)
   *   attributes_count        2 bytes
   *   attributes              0 bytes  (no attributes generated)
   *  ---------------------------------
   *   total             pc + 12 bytes
   */
  cur_code->attribute_length = pc + 12;
  cur_code->attr.Code->max_locals = mloc;
  cur_code->attr.Code->code_length = pc;

  dl_insert_b(meth->attributes, cur_code);
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
  char * skipToken(char *);

  struct stack_info *tmp;
  int len = strlen(d);
  char *ptr;

  tmp = (struct stack_info *)f2jalloc(sizeof(struct stack_info));
  tmp->arg_len = 1;
  tmp->ret_len = 1;

  /* the shortest method descriptor should be 3 characters: ()V
   * thus, if the given string is < 3 characters, it must be in error.
   */

  if(len < 3) {
    fprintf(stderr,"WARNING: invalid descriptor.\n");
    return tmp;
  }

  if(d[0] != '(') {
    fprintf(stderr,"WARNING: invalid descriptor.\n");
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

  ptr = strtok(strdup(d),")");
  ptr = strtok(NULL,")");
  if( (*ptr ==  'D') || (*ptr == 'J') )
    tmp->ret_len = 2;
  else if(*ptr == 'V')
    tmp->ret_len = 0;
  else
    tmp->ret_len = 1;

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
      fprintf(stderr,"WARNING: skipToken() unrecognized character in descriptor\n");
      return NULL;
  }

  /* should never reach here */
  return NULL;
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
  HASHNODE * hashtemp;
  static int localnum = 0;
  extern int locals;

  /* if root is NULL, this is probably a PROGRAM (no args) */
  if(root == NULL)
    return;

  /* This loop takes care of the stuff coming in from the
   * argument list.  
   */
  for (locallist = root ; locallist; locallist = locallist->nextstmt)
  {
    if(gendebug)
      printf("arg list name: %s\n", locallist->astnode.ident.name);

    hashtemp = type_lookup(cur_type_table, locallist->astnode.ident.name);
    if(hashtemp == NULL)
    {
      fprintf(stderr,"Type table is screwed in assign locals.\n");
      fprintf(stderr,"could not find %s\n", locallist->astnode.ident.name);
      exit(-1);
    }

    /* Check to see if it is a double or if it is an array declaration.
     * Doubles take up two stack entries, so we increment by 2.  Arrays
     * only take up one stack entry, but we add an integer offset 
     * parameter which takes up an additional entry.
     */

    if (hashtemp->type == Double ||
        hashtemp->variable->astnode.ident.arraylist != NULL)
    {
      hashtemp->variable->astnode.ident.localvnum = localnum;
      if(gendebug)
        printf("%s %d\n", hashtemp->variable->astnode.ident.name, localnum);
      localnum += 2;
    }
    else
    {
      hashtemp->variable->astnode.ident.localvnum = localnum;
      if(gendebug)
        printf("%s %d\n", hashtemp->variable->astnode.ident.name, localnum); 
      localnum++;
    }
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
    case ImpliedLoop:
      return("ImpliedLoop");
    case Unimplemented:
      return("Unimplemented");
    case Equivalence:
      return("Equivalence");
    case Comment:
      return("Comment");
    case MainComment:
      return("MainComment");
    default:
      sprintf(temp, "print_nodetype(): Unknown Node: %d", root->nodetype);
      return(temp);
  }
}
