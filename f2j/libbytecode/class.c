/** @file class.c
 * Routines for writing the class file to disk.
 */

#include "class.h"

/**
 * Given a pointer to a classfile structure, this function writes the class
 * file to disk.
 *
 * @param class -- The class structure to be written.
 * @param output_dir -- The name of the output directory to which the class file
 *    should be written.  If NULL, the class file is written to the current
 *    directory.
 *
 * @returns 0 on success, -1 on failure.
 */

int
bc_write_class(JVM_CLASS *class, char *output_dir)
{
  Dlist tmpPtr;
  FILE *cfp;

  if(!class) {
    BAD_ARG();
    return -1;
  }

  dl_traverse(tmpPtr,class->methods) {
    if(finalizeMethod((JVM_METHOD *) tmpPtr->val))
      return -1;
  }

  class->constant_pool_count = 
     (u2) ((CP_NODE *)dl_val(dl_last(class->constant_pool)))->next_idx;

  cfp = open_output_classfile(class, output_dir);

  if(!cfp) return -1;

  clearerr(cfp);

  write_u4(class->magic, cfp);

  write_u2(class->minor_version, cfp);

  write_u2(class->major_version, cfp);

  write_u2(class->constant_pool_count, cfp);

  write_constant_pool(class, cfp);

  write_u2(class->access_flags, cfp);

  write_u2(class->this_class, cfp);

  write_u2(class->super_class, cfp);

  write_u2(class->interfaces_count, cfp);

  write_interfaces(class,cfp);

  write_u2(class->fields_count, cfp);

  write_fields(class,cfp);

  write_u2(class->methods_count, cfp);

  write_methods(class,cfp);

  write_u2(class->attributes_count, cfp);

  write_attributes(class, class->attributes, cfp);

  if(ferror(cfp)) {
    fclose(cfp);
    return -1;
  }

  fclose(cfp);

  return 0;
}

/*****************************************************************************
 *****************************************************************************
 **                                                                         **
 ** Functions after this point are not exposed as part of the API.          **
 **                                                                         **
 *****************************************************************************
 *****************************************************************************/


/**
 * This function writes the all the constants to disk.  this could be more
 * efficient if we could assume that there was no padding in the structures.
 * then it would just be a matter of writing out however many bytes is
 * allocated.  but i'm not really sure how different compilers might pad
 * structures, so i'm going to play it safe here and just write each item
 * individually.  --kgs 4/25/00
 *
 * @param class -- The class structure to be written.
 * @param out -- File pointer to which the data should be written.
 */

static void
write_constant_pool(JVM_CLASS *class, FILE *out)
{
  CP_NODE * tmpconst;
  Dlist tmpPtr;

  if(!class || !out) {
    BAD_ARG();
    return;
  }

  dl_traverse(tmpPtr,class->constant_pool) {
    tmpconst = (CP_NODE *) tmpPtr->val;

    debug_msg("write_constant_pool() - tag = %d\n",tmpconst->val->tag);

    write_u1(tmpconst->val->tag, out);

    switch(tmpconst->val->tag) {
      case CONSTANT_Utf8:
        write_u2(tmpconst->val->cpnode.Utf8.length,out);
        fwrite(tmpconst->val->cpnode.Utf8.bytes,
           tmpconst->val->cpnode.Utf8.length,1,out);
        break;
      case CONSTANT_Integer:
        fwrite(&(tmpconst->val->cpnode.Integer.bytes),
           sizeof(tmpconst->val->cpnode.Integer.bytes),1,out);
        break;
      case CONSTANT_Float:
        fwrite(&(tmpconst->val->cpnode.Float.bytes),
           sizeof(tmpconst->val->cpnode.Float.bytes),1,out);
        break;
      case CONSTANT_Long:
        fwrite(&(tmpconst->val->cpnode.Long.high_bytes),
           sizeof(tmpconst->val->cpnode.Long.high_bytes),1,out);
        fwrite(&(tmpconst->val->cpnode.Long.low_bytes),
           sizeof(tmpconst->val->cpnode.Long.low_bytes),1,out);
        break;
      case CONSTANT_Double:
        fwrite(&(tmpconst->val->cpnode.Double.high_bytes),
           sizeof(tmpconst->val->cpnode.Double.high_bytes),1,out);
        fwrite(&(tmpconst->val->cpnode.Double.low_bytes),
           sizeof(tmpconst->val->cpnode.Double.low_bytes),1,out);
        break;
      case CONSTANT_Class:
        write_u2(tmpconst->val->cpnode.Class.name_index,out);
        break;
      case CONSTANT_String:
        write_u2(tmpconst->val->cpnode.String.string_index, out);
        break;
      case CONSTANT_Fieldref:
      case CONSTANT_Methodref:
      case CONSTANT_InterfaceMethodref:
        write_u2(tmpconst->val->cpnode.Methodref.class_index,out);
        write_u2(tmpconst->val->cpnode.Methodref.name_and_type_index,out);
        break;
      case CONSTANT_NameAndType:
        write_u2(tmpconst->val->cpnode.NameAndType.name_index,out);
        write_u2(tmpconst->val->cpnode.NameAndType.descriptor_index,out);
        break;
      default:
        debug_err("WARNING: unknown tag in write_constant_pool()\n");
        break;  /* ANSI requirement */
    }
  }
}

/**
 * This function writes the all the interfaces to disk.
 *
 * @param class -- The class structure to be written.
 * @param out -- File pointer to which the data should be written.
 */

static void
write_interfaces(JVM_CLASS *class, FILE *out)
{
  int i=0, ival;
  Dlist tmpPtr;

  if(!class || !out) {
    BAD_ARG();
    return;
  }

  debug_msg("in write_interfaces %p %p\n", (void*)class, (void*)out);

  dl_traverse(tmpPtr,class->interfaces) {
    ival = *((int *) tmpPtr->val);
    write_u2((u2)ival,out);
    i++;
  }

  if(i != class->interfaces_count)
    debug_err("Warning: expected to write %d interfaces, but wrote %d.\n",
       class->interfaces_count, i);
}

/**
 * This function writes the all the fields to disk.
 *
 * @param class -- The class structure to be written.
 * @param out -- File pointer to which the data should be written.
 */

static void
write_fields(JVM_CLASS *class, FILE *out)
{
  JVM_FIELD *tmpfield;
  Dlist tmpPtr;
  int cnt;

  if(!class || !out) {
    BAD_ARG();
    return;
  }

  dl_traverse(tmpPtr,class->fields) {
    tmpfield = (JVM_FIELD *) tmpPtr->val;

    debug_msg("write_fields() %d, %d, %d\n",
        tmpfield->access_flags, tmpfield->name_index,
        tmpfield->descriptor_index);

    write_u2(tmpfield->access_flags,out);
    write_u2(tmpfield->name_index,out);
    write_u2(tmpfield->descriptor_index,out);

    write_u2(tmpfield->attributes_count,out);

    cnt = write_attributes(class, tmpfield->attributes, out);

    if(tmpfield->attributes_count != cnt) {
      debug_err("WARNING: expected to write %d attributes,",
          tmpfield->attributes_count);
      debug_err("but actually wrote %d attributes.", cnt);
    }
  }
}

/**
 * This function writes the all the methods to disk.
 *
 * @param class -- The class structure to be written.
 * @param out -- File pointer to which the data should be written.
 */

static void
write_methods(JVM_CLASS *class, FILE *out)
{
  JVM_METHOD *tmpmeth;
  Dlist tmpPtr;
  int cnt;

  if(!class || !out) {
    BAD_ARG();
    return;
  }

  dl_traverse(tmpPtr,class->methods) {
    tmpmeth = (JVM_METHOD *) tmpPtr->val;

    write_u2(tmpmeth->access_flags,out);

    write_u2(tmpmeth->name_index,out);

    write_u2(tmpmeth->descriptor_index,out);

    write_u2(tmpmeth->attributes_count,out);

    cnt = write_attributes(class, tmpmeth->attributes, out);

    if(tmpmeth->attributes_count != cnt) {
      debug_err("WARNING: expected to write %d attributes,",
          tmpmeth->attributes_count);
      debug_err("but actually wrote %d attributes.", cnt);
    }
  }
}

/**
 * This function writes the all the attributes in the given list
 * to disk.   Even though the first argument is a class structure, this
 * function can be used to write attributes of a class, method, or field.
 * In any case the class structure is needed for access to its constant
 * pool.
 *
 * @param class -- The class structure to be written.
 * @param attr_list -- The list of attributes to be written.  It should
 *    be a Dlist of JVM_ATTRIBUTE pointers.
 * @param out -- File pointer to which the data should be written.
 *
 * @returns Number of attributes written or -1 on failure.
 */

static int
write_attributes(JVM_CLASS *class, Dlist attr_list, FILE *out)
{
  JVM_ATTRIBUTE *tmpattr;
  char *attr_name;
  Dlist tmpPtr, tmpPtr2;
  CP_NODE *c;
  int cnt = 0;

  if(!attr_list || !class || !out) {
    BAD_ARG();
    return cnt;
  }

  dl_traverse(tmpPtr,attr_list) {
    tmpattr = (JVM_ATTRIBUTE *) tmpPtr->val;

    c = cp_entry_by_index(class, tmpattr->attribute_name_index);

    if(c==NULL) {
      debug_err("WARNING: write_attributes() can't find attribute name\n");
      continue;
    }

    attr_name = cp_null_term_utf8(c->val);

    if(!attr_name)
      return -1;

    debug_msg("attribute name = '%s'\n", attr_name);

    write_u2(tmpattr->attribute_name_index,out);

    write_u4(tmpattr->attribute_length,out);

    debug_msg("write_attributes() - attribute length: %d, idx: %d\n",
        tmpattr->attribute_length, tmpattr->attribute_name_index);

    if(!strcmp(attr_name,"SourceFile")) {
      write_u2(tmpattr->attr.SourceFile->sourcefile_index,out);
    }
    else if(!strcmp(attr_name,"ConstantValue")) {
      write_u2(tmpattr->attr.ConstantValue->constantvalue_index,out);
    }
    else if(!strcmp(attr_name,"Deprecated")) {
      /* The Deprecated attribute has length 0, so there is nothing to write */
    }
    else if(!strcmp(attr_name,"Synthetic")) {
      /* The Synthetic attribute has length 0, so there is nothing to write */
    }
    else if(!strcmp(attr_name,"Code")) {
      write_u2(tmpattr->attr.Code->max_stack,out);

      write_u2(tmpattr->attr.Code->max_locals,out);

      write_u4(tmpattr->attr.Code->code_length,out);

      write_code(tmpattr->attr.Code->code, out);

      write_u2(tmpattr->attr.Code->exception_table_length,out);

      if(tmpattr->attr.Code->exception_table_length > 0) {

        write_exception_table(tmpattr->attr.Code->exception_table,
          tmpattr->attr.Code->exception_table_length, out);
      }

      debug_msg("code attributes count = %d\n", tmpattr->attr.Code->attributes_count);

      write_u2(tmpattr->attr.Code->attributes_count,out);

      if(tmpattr->attr.Code->attributes_count > 0) {
        write_attributes(class, tmpattr->attr.Code->attributes, out);
      }
    }
    else if(!strcmp(attr_name,"Exceptions")) {
      int *idx;

      write_u2(tmpattr->attr.Exceptions->number_of_exceptions, out);

      dl_traverse(tmpPtr2, tmpattr->attr.Exceptions->exception_index_table) {
        idx = (int *) tmpPtr2->val;

        write_u2(*idx, out);
      }
    }
    else if(!strcmp(attr_name,"LineNumberTable")) {
      JVM_LINE_NUMBER_TABLE_ENTRY *entry;

      write_u2(tmpattr->attr.LineNumberTable->line_number_table_length, out);

      dl_traverse(tmpPtr2, tmpattr->attr.LineNumberTable->line_number_table) {
        entry = (JVM_LINE_NUMBER_TABLE_ENTRY *) tmpPtr2->val;

        write_u2(entry->op->pc, out);
        write_u2(entry->line_number, out);
      }
    }
    else if(!strcmp(attr_name,"LocalVariableTable")) {
      JVM_LOCAL_VARIABLE_TABLE_ENTRY *entry;
      int len;

      write_u2(tmpattr->attr.LocalVariableTable->local_variable_table_length, 
        out);

      dl_traverse(tmpPtr2, tmpattr->attr.LocalVariableTable->local_variable_table) {
        entry = (JVM_LOCAL_VARIABLE_TABLE_ENTRY *) tmpPtr2->val;
        len = (entry->end->pc - entry->start->pc) + entry->end->width;

        write_u2(entry->start->pc, out);
        write_u2(len, out);
        write_u2(entry->name_index, out);
        write_u2(entry->descriptor_index, out);
        write_u2(entry->index, out);
      }
    }
    else if(!strcmp(attr_name,"InnerClasses")) {
      struct InnerClassEntry *entry;

      write_u2(tmpattr->attr.InnerClasses->number_of_classes, out);

      dl_traverse(tmpPtr2, tmpattr->attr.InnerClasses->classes) {
        entry = (struct InnerClassEntry *)tmpPtr2->val;

        write_u2(entry->inner_class_info_index, out);
        write_u2(entry->outer_class_info_index, out);
        write_u2(entry->inner_name_index, out);
        write_u2(entry->inner_class_access_flags, out);
      }
    }
    else {
      /* Don't recognize this attribute, so it must be user-defined. */
      
      fwrite(tmpattr->attr.UserDefined->data,1,tmpattr->attribute_length,out);
    }

    free(attr_name);
    cnt++;
  }

  return cnt;
}

/**
 * This function writes the exception table to disk.
 *
 * @param et -- Array of exception table structures to be written.
 * @param len -- The number of exception table entries in the array.
 * @param out -- File pointer to which the data should be written.
 */

static void
write_exception_table(struct ExceptionTable *et, int len, FILE *out)
{
  int i;

  if(!et || !out) {
    BAD_ARG();
    return;
  }

  for(i=0;i<len;i++) {
    write_u2( et[i].start_pc, out );
    write_u2( et[i].end_pc, out );
    write_u2( et[i].handler_pc, out );
    write_u2( et[i].catch_type, out );
  }
}

/**
 * Traverse the code graph and write each opcode to disk.
 *
 * @param g -- List of pointers to JVM_CODE_GRAPH_NODE structures,
 *    each node representing one instruction.
 * @param out -- File pointer to which the data should be written.
 */

static void
write_code(Dlist g, FILE *out)
{
  Dlist tmp;
  JVM_CODE_GRAPH_NODE *node;
  u1 op;
  u1 op1;
  u2 op2;
  u4 op4;

  if(!g || !out) {
    BAD_ARG();
    return;
  }

  dl_traverse(tmp, g) {
    node = (JVM_CODE_GRAPH_NODE *) dl_val(tmp);

    op = (u1) node->op;
    write_u1(op,out);

    switch(node->width) {
      case 1:
        /* if the width is 1, then there is no operand */
        break;
      case 2:
        op1 = (u1) node->operand;
        write_u1(op1,out);
        break;
      case 3:
        op2 = (u2) node->operand;
        write_u2(op2,out);
        break;
      case 4:
        op4 = (u4) node->operand;
        write_u3(op4,out);
        break;
      case 5:
        op4 = (u4) node->operand;
        write_u4(op4,out);
        break;
      default:
        if(op == jvm_tableswitch)
          write_tableswitch(node, out);
        else if(op == jvm_lookupswitch)
          write_lookupswitch(node, out); 
        else
          debug_err( "write_code(): hit default unexpectedly\n");

        break;
    }
  }
}

/**
 * This function opens the file to which we write the bytecode.
 * We derive the name of the class by looking at the "this_class" entry
 * in the class file's constant pool.
 *
 * @param class -- The class structure to be written.
 * @param output_dir -- The name of the output directory to which the class file
 *    should be written.  If NULL, the class file is written to the current
 *    directory.
 *
 * @returns Pointer to the opened file.  Returns NULL on error.
 */

static FILE *
open_output_classfile(JVM_CLASS *class, char *output_dir)
{
  char *filename;
  FILE *newfp;
  CP_NODE *c;

  if(!class) {
    BAD_ARG();
    return NULL;
  }

  c = cp_entry_by_index(class, class->this_class);
  if(!c) return NULL;

  c = cp_entry_by_index(class, c->val->cpnode.Class.name_index);
  if(!c) return NULL;

  /* malloc enough characters in the filename for:
   *  - the class name
   *  - plus 6 chars for ".class"
   *  - plus 1 char for the null terminator
   */

  filename = (char *)malloc(c->val->cpnode.Utf8.length +  7);
  if(!filename) return NULL;

  strncpy(filename, (char *)c->val->cpnode.Utf8.bytes, c->val->cpnode.Utf8.length);
  filename[c->val->cpnode.Utf8.length] = '\0';
  strcat(filename,".class");

  debug_msg("going to write class file: '%s'\n", filename);

  newfp = bc_fopen_fullpath(filename,"wb", output_dir);

  free(filename);
  return newfp;
}

/**
 * Finishes initialization of the method structure.  Before writing, the
 * method requires some preparation.  This involves:
 *  -# Setting up the Line Number Table, Local Variable Table, and 
 *       Exception table.
 *  -# Computing the code attribute length.
 *  -# Inserting the cur_code list as an attribute of this method.
 *
 * @param meth -- The method to be finalized.
 *
 * @returns 0 on success, -1 on failure.
 */

static int
finalizeMethod(JVM_METHOD *meth)
{
  JVM_EXCEPTION_TABLE_ENTRY *et_entry;
  Dlist tmp;
  int idx, code_attr_len;

  if(!meth) {
    BAD_ARG();
    return -1;
  }

  /* at the end of the method, the stacksize should always be zero.
   * if not, we're gonna have verification problems at the very least.
   * at this point, there's not much we can do about it, but issue a
   * warning.
   */
  if(meth->stacksize != 0)
    debug_err("WARNING: ending method with stacksize = %d\n",
       meth->stacksize);

  if(traverse_code(meth) < 0) {
    debug_err("Error: failure finalizing method\n");
    return -1;
  }

  meth->cur_code->attr.Code->exception_table_length = meth->num_handlers;

  if(meth->num_handlers > 0) {
    meth->cur_code->attr.Code->exception_table = (struct ExceptionTable *)
        malloc(sizeof(struct ExceptionTable) * meth->num_handlers);

    if(!meth->cur_code->attr.Code->exception_table) return -1;

    debug_msg("Code set exception_table_length = %d\n",meth->num_handlers);

    idx = 0;
    dl_traverse(tmp, meth->exc_table) {
      et_entry = (JVM_EXCEPTION_TABLE_ENTRY *) tmp->val;

      meth->cur_code->attr.Code->exception_table[idx].start_pc = et_entry->from->pc;
      meth->cur_code->attr.Code->exception_table[idx].end_pc = et_entry->to->pc;
      meth->cur_code->attr.Code->exception_table[idx].handler_pc =
         et_entry->target->pc;
      meth->cur_code->attr.Code->exception_table[idx].catch_type =
         et_entry->catch_type;
      idx++;

      free(et_entry);
    }
  }

  dl_delete_list(meth->exc_table);
  meth->exc_table = NULL;

  /* check if there were any line number table entries created.
   * if so, create the LineNumberTable attribute.
   */

  if(!dl_empty(meth->line_table)) {
    JVM_ATTRIBUTE *lnt = bc_new_line_number_table_attr(meth);

    if(!lnt) return -1;

    dl_insert_b(meth->cur_code->attr.Code->attributes, lnt);
    meth->cur_code->attr.Code->attributes_count++;
  }

  /* check if there were any local variable table entries created.
   * if so, create the LocalVariableTable attribute.
   */

  if(!dl_empty(meth->locals_table)) {
    JVM_ATTRIBUTE *lvt = bc_new_local_variable_table_attr(meth);

    if(!lvt) return -1;

    dl_insert_b(meth->cur_code->attr.Code->attributes, lvt);
    meth->cur_code->attr.Code->attributes_count++;
  }

  /* calculate the size of the code attribute's attributes */

  code_attr_len = 0;
  dl_traverse(tmp, meth->cur_code->attr.Code->attributes) {
    JVM_ATTRIBUTE *attr = (JVM_ATTRIBUTE *) tmp->val;

    code_attr_len += attr->attribute_length + 6;
  }

  /* attribute_length is calculated as follows:
   *   max_stack               =  2 bytes
   *   max_locals              =  2 bytes
   *   code_length             =  4 bytes
   *   code                    = pc bytes
   *   exception_table_length  =  2 bytes
   *   exception_table         =  exc_table_len * sizeof(exc table) bytes
   *   attributes_count        =  2 bytes
   *   attributes              =  code_attr_len bytes
   *  ---------------------------------
   *   total (in bytes)        =  12 + exc_table_length * sizeof(exc table) + code_attr_len
   */

  meth->cur_code->attribute_length = meth->pc + 12 + meth->num_handlers *
               sizeof(struct ExceptionTable) + code_attr_len;

  meth->cur_code->attr.Code->max_locals = (u2)meth->max_locals;
  meth->cur_code->attr.Code->code_length = meth->pc;

  debug_msg("Code: set code_length = %d\n",meth->pc);

  /*
   * If the method was declared abstract or native, then it should not 
   * have a Code attribute.
   */

  if((meth->access_flags & JVM_ACC_ABSTRACT) || 
     (meth->access_flags & JVM_ACC_NATIVE))
  {
    if(meth->cur_code->attr.Code->code_length > 0) {
      debug_err("Warning: code_length > 0 for abstract method '%s'.\n",
         meth->name);
    }
  }
  else {
    meth->attributes_count++;
    dl_insert_b(meth->attributes, meth->cur_code);
  }

  return 0;
}

/**
 * Writes an unsigned byte to the specified file pointer.  there are no
 * issues with endianness here, but this function is included for
 * consistency.
 *
 * @param num -- The unsigned byte to be written.
 * @param out -- File pointer to which the data should be written.
 */

static void
write_u1(u1 num, FILE *out)
{
  if(!out) {
    BAD_ARG();
    return;
  }

  fwrite(&num, sizeof(num), 1, out);
}

/**
 * Writes an unsigned short to the specified file pointer, changing
 * endianness if necessary.
 *
 * @param num -- The unsigned short to be written.
 * @param out -- File pointer to which the data should be written.
 */

static void
write_u2(u2 num, FILE *out)
{
  if(!out) {
    BAD_ARG();
    return;
  }

  num = cp_big_endian_u2(num);
  fwrite(&num, sizeof(num), 1, out);
}

/**
 * Writes an unsigned short and then an unsigned byte to the
 * specified file pointer, changing endianness if necessary.
 *
 * @param num -- The short/byte pair to be written.  The parameter holds
 *    four bytes, but only the low-order three bytes are used.  First the two
 *    low-order bytes of (num>>8) are written (endianness adjusted as 
 *    necessary) followed by the low-order byte of num.
 *     
 * @param out -- File pointer to which the data should be written.
 */

static void
write_u3(u4 num, FILE *out){
  u2 u2tmp;
  u1 u1tmp;

  if(!out) {
    BAD_ARG();
    return;
  }

  u1tmp = u2tmp = (u2)(num>>8);
  u2tmp = cp_big_endian_u2(u2tmp);
  fwrite(&u2tmp, sizeof(u2tmp), 1, out);

  u1tmp = (u1)(num - (u1tmp<<8));
  fwrite(&u1tmp, sizeof(u1tmp), 1, out);
}

/**
 * Writes an unsigned int to the specified file pointer, changing endianness
 * if necessary.
 *
 * @param num -- The unsigned int to be written.
 * @param out -- File pointer to which the data should be written.
 */

static void
write_u4(u4 num, FILE *out)
{
  if(!out) {
    BAD_ARG();
    return;
  }

  num = cp_big_endian_u4(num);
  fwrite(&num, sizeof(num), 1, out);
}

/**
 * Writes a tableswitch instruction.  First writes any necessary padding
 * followed by the variable-length instruction.
 *
 * @param node -- The instruction node to be written.
 * @param out -- File pointer to which the data should be written.
 */

static void
write_tableswitch(JVM_CODE_GRAPH_NODE *node, FILE *out)
{
  int i, n, zero = 0;
  
  if(!node || !out) {
    BAD_ARG();
    return;
  }

  fwrite(&zero, 1, node->switch_info->cell_padding, out);

  if(node->switch_info->default_case)
    write_u4(node->switch_info->default_case->pc - node->pc, out);
  else
    debug_err("warning, unspecified default not implemented yet.\n");

  write_u4(node->switch_info->low, out);
  write_u4(node->switch_info->high, out);

  n = node->switch_info->high - node->switch_info->low + 1;

  for(i = 0; i < n; i++)
    write_u4(node->switch_info->sorted_entries[i]->instr->pc - node->pc, out);
}

/**
 * Writes a lookupswitch instruction.  First writes any necessary padding
 * followed by the variable-length instruction.
 *
 * @param node -- The instruction node to be written.
 * @param out -- File pointer to which the data should be written.
 */

static void 
write_lookupswitch(JVM_CODE_GRAPH_NODE *node, FILE *out){
  int i, zero = 0;

  if(!node || !out) {
    BAD_ARG();
    return;
  }

  fwrite(&zero, 1, node->switch_info->cell_padding, out);

  if(node->switch_info->default_case)
    write_u4(node->switch_info->default_case->pc - node->pc, out);
  else
    debug_err("warning, unspecified default not implemented yet.\n");

  write_u4(node->switch_info->num_entries, out);

  for(i = 0; i < node->switch_info->num_entries; i++) {
    write_u4(node->switch_info->sorted_entries[i]->case_num, out);
    write_u4(node->switch_info->sorted_entries[i]->instr->pc - node->pc, out);
  }
}

/**
 * This function traverses the code graph, determines the maximum stack size, 
 * and assigns branch target offsets to each instruction node.  Also handles
 * recalculating all branch target offsets in case the addresses shift (due to
 * changing a goto to goto_w for example).  Address shift also requires 
 * recomputing the cell padding for switch instructions.
 *
 * @param meth -- The method to be traversed.
 *
 * @returns 0 on success, -1 on failure.
 */

static int
traverse_code(JVM_METHOD *meth)
{
  JVM_EXCEPTION_TABLE_ENTRY *et_entry;
  JVM_CODE_GRAPH_NODE *val;
  Dlist tmp, cgraph;

  if(!meth) {
    BAD_ARG(); 
    return -1;
  }

  cgraph = meth->cur_code->attr.Code->code;

  if(dl_empty(cgraph))
    return 0;

  /* set initial stack depth to zero */
  val = (JVM_CODE_GRAPH_NODE *) dl_val(dl_first(cgraph));
  val->stack_depth = 0;

  meth->reCalcAddr = FALSE;

  /* traverse the whole graph calculating branch target offsets. */
  calc_offsets(meth, val);

  /* now traverse paths originating from exception handlers */
  meth->num_handlers = 0;
  dl_traverse(tmp, meth->exc_table) {
    /* count number of handlers.. we'll use this info later */
    meth->num_handlers++;
    et_entry = (JVM_EXCEPTION_TABLE_ENTRY *) tmp->val;

    /* 
     * set stack depth for the beginning of the exception handler to
     * the depth of the stack at the beginning of the 'try' block plus 1
     * (to account for the reference to the exception which is sitting on
     * the stack now).
     */

    et_entry->target->stack_depth = et_entry->from->stack_depth + 1;

    calc_offsets(meth, et_entry->target);
  }

  /*
   * if there was a branch offset that exceeds the JVM instruction's
   * limit (signed 16-bit value), then the width of that instruction
   * must change (e.g. from goto to goto_w), thus altering the
   * addresses of all instructions following that one.  here we are
   * recalculating the PCs and all branch target offsets (only if
   * necessary though).  there are only a few instances in the LAPACK
   * code where the branch exceeds the limits, so this shouldn't
   * increase the compilation time very much.
   */

  if(meth->reCalcAddr) {
    int tmpPC = 0;

    dl_traverse(tmp,cgraph) {
      val = (JVM_CODE_GRAPH_NODE *) tmp->val;

      val->pc = tmpPC;

      /* if this is a switch, then the cell padding and op width need
       * to be recalculated based on the pc of this instruction.
       */

      if(val->op == jvm_tableswitch) {
        val->switch_info->cell_padding = 3-(val->pc%4);
        val->width = 1 + val->switch_info->cell_padding + 12 +
          (val->switch_info->high - val->switch_info->low + 1) * 4;
      }
      else if(val->op == jvm_lookupswitch) {
        val->switch_info->cell_padding = 3-(val->pc%4);
        val->width = 1 + val->switch_info->cell_padding + 8 +
          val->switch_info->num_entries * 8;
      }

      tmpPC += val->width;
    }

    /* now that all the instruction addresses are correct, recalculate
     * the branch target offsets.
     */

    meth->reCalcAddr = FALSE;
    dl_traverse(tmp,cgraph) {
      val = (JVM_CODE_GRAPH_NODE *) tmp->val;

      if ( val->branch_target != NULL) {
        meth->reCalcAddr =
            check_distance(val->op, val->branch_target->pc, val->pc);

        val->operand = val->branch_target->pc - val->pc;
      }
    }

    if(meth->reCalcAddr) {
      debug_err("BAD NEWS - things are still screwed.\n");
      return -1;
    }

    meth->pc = tmpPC;
  }

  if(meth->pc > JVM_MAX_CODE_LEN)
    debug_err("WARNING: code length (%d) exceeds max of %d\n",
       meth->pc, JVM_MAX_CODE_LEN);

  /* print the instructions if debugging is enabled */
#ifdef BC_DEBUG
  dl_traverse(tmp,cgraph) {
    char *warn; 

    val = (JVM_CODE_GRAPH_NODE *) tmp->val;

    if(!val->visited)
      warn = "(UNVISITED!!)";
    else
      warn = "";

    if(bc_op_width(val->op) > 1)
      debug_msg("%d: %s %d %s\n", val->pc, jvm_opcode[val->op].op,
         val->operand, warn);
    else
      debug_msg("%d: %s %s\n", val->pc, jvm_opcode[val->op].op, warn);
  }
#endif

  return 0;
}

/**
 * This function calculates the branch target offsets for instructions that
 * branch (gotos, compares, etc).  Also set the stack depth for the
 * instruction(s) following this one.  Also perform sanity checks on the
 * stack values to make sure that we aren't hitting some instruction from
 * different places with different stack depths.
 *
 * @param meth -- The method to be traversed.
 * @param val -- The node in the code graph to start traversing from.
 */

static void
calc_offsets(JVM_METHOD *meth, JVM_CODE_GRAPH_NODE *val)
{
  JVM_CODE_GRAPH_NODE *label_node;
  Dlist cgraph;
  int temp_pc, stack_inc, stack_dec;

  if(!meth || !val)  {
    BAD_ARG();
    return;
  }

  cgraph = meth->cur_code->attr.Code->code;

  debug_msg("in calc_offsets, before op %d : %s, stack_Depth = %d\n",
     val->pc, jvm_opcode[val->op].op,val->stack_depth);

  if(val->next == NULL)
    debug_msg("next is NULL\n");
  else
    debug_msg("next is %s\n", jvm_opcode[val->next->op].op);

  if(val->visited)
    return;

  val->visited = TRUE;

  meth->stacksize = val->stack_depth;

  stack_dec = get_stack_decrement(meth, val->op, val->operand);
  stack_inc = get_stack_increment(meth, val->op, val->operand);

  if((stack_dec < 0) || (stack_inc < 0)) {
    debug_err("Could not determine stack inc/dec\n");
    stack_dec = 0;
    stack_inc = 0;
  }

  if(dec_stack(meth, stack_dec) == -1) {
    debug_err("WARNING: negative stack in method '%s'!  ", meth->name);
    debug_err("pc = %d\n", val->pc);
  }

  inc_stack(meth, stack_inc);

  if((val->op == jvm_tableswitch) || (val->op == jvm_lookupswitch)) {
    int i=0;

    meth->reCalcAddr = TRUE;

    if(num_empty_switch_cases(val) > JVM_SWITCH_FILL_THRESH)
      i = setup_lookupswitch(val);
    else
      i = setup_tableswitch(val);

    if(i < 0) {
      debug_err("Error setting up switch\n");
      return;
    }

    /* now visit the code for each case in this switch */

    for(i = 0; i < val->switch_info->num_entries; i++) {
      JVM_SWITCH_ENTRY *entry = val->switch_info->sorted_entries[i];

      if(entry->instr->stack_depth == -1)
        entry->instr->stack_depth = meth->stacksize;

      calc_offsets(meth, entry->instr);
    }

    calc_offsets(meth, val->switch_info->default_case);

    return;
  }else if((val->op == jvm_goto) || (val->op == jvm_goto_w) ||
           (val->op == jvm_jsr) || (val->op == jvm_jsr_w)) {
    if(val->branch_target == NULL) {

      debug_msg("looking at GOTO %s\n", val->branch_label);

      if( (temp_pc = find_label(meth->label_list, val->branch_label)) != -1)
      {
        label_node = bc_node_at_pc(meth, temp_pc);

        if(label_node != NULL) {

          debug_msg(" **found** target pc is %d\n", label_node->pc);

          if(label_node->stack_depth == -1)
            label_node->stack_depth = meth->stacksize;
          else if(label_node->stack_depth != meth->stacksize)
            debug_err("WARNING: hit pc %d with diff stack sizes (%s)\n",
                    label_node->pc, meth->name);

          if(check_distance(val->op, label_node->pc, val->pc)) {
            meth->reCalcAddr = TRUE;
            if(val->op == jvm_goto) {
              val->op = jvm_goto_w;
              val->width = bc_op_width(jvm_goto_w);
            }
            else if(val->op == jvm_jsr) {
              val->op = jvm_jsr_w;
              val->width = bc_op_width(jvm_jsr_w);
            }
            else
              debug_err("did not expect to be here\n");
          }

          val->operand = label_node->pc - val->pc;
          val->branch_target = label_node;
          calc_offsets(meth, label_node);
        }
        else
          debug_err("WARNING: cannot find node for pc %d\n", temp_pc);
      }
      else
        debug_err("WARNING: cannot find label %s\n", val->branch_label);
    }
    else {
      debug_msg("goto branching to pc %d\n", val->branch_target->pc);

      if(val->branch_target->stack_depth == -1)
        val->branch_target->stack_depth = meth->stacksize;
      else if (val->branch_target->stack_depth != meth->stacksize)
        debug_err("WARNING: hit pc %d with diff stack sizes (%s).\n",
                val->branch_target->pc, meth->name);

      if(check_distance(val->op, val->branch_target->pc, val->pc)) {
        meth->reCalcAddr = TRUE;
        if(val->op == jvm_goto) {
          val->op = jvm_goto_w;
          val->width = bc_op_width(jvm_goto_w);
        }
        else if(val->op == jvm_jsr) { 
          val->op = jvm_jsr_w;
          val->width = bc_op_width(jvm_jsr_w);
        }
        else
          debug_err("did not expect to be here\n");
      }

      val->operand = val->branch_target->pc - val->pc;
      calc_offsets(meth, val->branch_target); 
    }

    /* if this is a jsr, then the subroutine will return back to the
     * instruction following the jsr, so continue visiting those nodes now.
     */

    if((val->op == jvm_jsr) || (val->op == jvm_jsr_w)) {
      if(val->next != NULL) {
        val->next->stack_depth = meth->stacksize;
        calc_offsets(meth, val->next);
      }
    }
  }
  else if ( val->branch_target != NULL) {
    if(val->next != NULL)
      val->next->stack_depth = meth->stacksize;

    if(check_distance(val->op, val->branch_target->pc, val->pc)) {
      JVM_CODE_GRAPH_NODE *gotoNode, *wideGotoNode;
      Dlist listNode;

      meth->reCalcAddr = TRUE;

      val->branch_target->stack_depth = meth->stacksize;
      val->operand = val->branch_target->pc - val->pc;

      gotoNode = bc_new_graph_node(meth, jvm_goto, 0);
      wideGotoNode = bc_new_graph_node(meth, jvm_goto_w, 0);

      if(!gotoNode || !wideGotoNode)
        return;

      gotoNode->visited = TRUE;
      wideGotoNode->visited = TRUE;

      gotoNode->branch_target = val->next;
      wideGotoNode->next = val->next;
      gotoNode->next = wideGotoNode;
      val->next = gotoNode;
      wideGotoNode->branch_target = val->branch_target;
      val->branch_target = wideGotoNode;

      listNode = get_list_node(cgraph, val);
      dl_insert_a(listNode, gotoNode);
      listNode = dl_next(listNode);
      dl_insert_a(listNode, wideGotoNode);

      if(gotoNode->branch_target != NULL){
        calc_offsets(meth, gotoNode->branch_target);
      }
      calc_offsets(meth, wideGotoNode->branch_target);
    }
    else {

      val->branch_target->stack_depth = meth->stacksize;
      val->operand = val->branch_target->pc - val->pc;

      if(val->next != NULL){
        calc_offsets(meth, val->next);
      }
      calc_offsets(meth, val->branch_target);
    }
  }
  else {
    if(val->next != NULL) {
      if((val->op != jvm_return) && (val->op != jvm_areturn) &&
         (val->op != jvm_dreturn) && (val->op != jvm_freturn) &&
         (val->op != jvm_ireturn) && (val->op != jvm_areturn) &&
         (val->op != jvm_ret))
      {
        val->next->stack_depth = meth->stacksize;
        calc_offsets(meth, val->next);
      }
    }

    /* if this is a return statement, then reset the opcode to
     * the one matching the method descriptor. 
     */

    if((val->op == jvm_return) || (val->op == jvm_areturn) ||
       (val->op == jvm_dreturn) || (val->op == jvm_freturn) ||
       (val->op == jvm_ireturn) || (val->op == jvm_areturn))
    {
      if(meth->descriptor_index != 0) {
        CP_NODE *c; 
        char *desc;

        c = cp_entry_by_index(meth->class, meth->descriptor_index);

        if(c) {
          desc = cp_null_term_utf8(c->val);

          if(desc)
            val->op = get_method_return_op(desc);

          free(desc);
        }
      }
      else
        debug_err("warning: method descriptor still unspecified!!\n");
    }
  }

  return;
}

/**
 * Calculates the number of empty cases in a switch instruction.
 * This information is used to determine whether to use the
 * tableswitch or lookupswitch instruction.  If there are a lot
 * of empty cases, then the lookupswitch is preferred.
 *
 * @param switch_instr -- The switch instruction to examine.
 *
 * @returns The number of empty switch cases.
 */

static int
num_empty_switch_cases(JVM_CODE_GRAPH_NODE *switch_instr)
{
  Dlist tmp;
  int n, cnt=0;

  if(!switch_instr)  {
    BAD_ARG();
    return 0;
  }

  n = switch_instr->switch_info->high - switch_instr->switch_info->low + 1;

  dl_traverse(tmp, switch_instr->switch_info->offsets) {
    cnt++;
  }

  return n-cnt;
}

/**
 * Determines the number of bytes that this instruction removes from the
 * stack prior to execution.  This depends on the instruction and on the
 * data types involved.  e.g. a method invoke instruction will remove one or
 * two entries per argument, depending on the data type.
 *
 * @param meth -- The method in which this instruction is located.
 * @param op -- The instruction opcode.
 * @param index -- The operand to the instruction.
 * 
 * @returns The number of bytes removed from the stack before execution.
 */

static int
get_stack_decrement(JVM_METHOD *meth, JVM_OPCODE op, u4 index)
{
  int stack_decrement;
  Dlist const_table;

  if(!meth) {
    BAD_ARG(); 
    return 0;
  }

  const_table = meth->class->constant_pool;

  switch(op) {
    case jvm_multianewarray:
      stack_decrement = index-((index>>8) * 256);
      break; 
    case jvm_invokespecial:
    case jvm_invokevirtual:
    case jvm_invokestatic:
    case jvm_invokeinterface:
      stack_decrement = get_stack_dec_invocation(meth->class, op, index);
      break;
    case jvm_putstatic:
    case jvm_getstatic:
    case jvm_putfield:
    case jvm_getfield:
      stack_decrement = get_stack_dec_field_acc(meth->class, op, index);
      break;
    default:
      /* else we can determine the stack decrement from a table.  */
      stack_decrement = jvm_opcode[op].stack_pre;
  }

  return stack_decrement;
}

/**
 * Determines the number of bytes that this field access instruction 
 * (getfield, putfield, getstatic, putstatic) removes from the
 * stack prior to execution.  
 *
 * @param class -- The class containing the constant pool relevant to
 *   this instruction (i.e. the class containing the method containing the
 *   instruction).
 * @param op -- The instruction opcode.
 * @param index -- The operand to the instruction.
 * 
 * @returns The number of bytes removed from the stack before execution.
 */

static int
get_stack_dec_field_acc(JVM_CLASS *class, JVM_OPCODE op, u4 index)
{
  int stack_decrement;
  char *this_desc;
  int tmpsize;
  CP_NODE *c;

  if(!class) {
    BAD_ARG(); 
    return 0;
  }

  c = cp_entry_by_index(class, index);
  if(!c) return -1;

  c = cp_entry_by_index(class, c->val->cpnode.Methodref.name_and_type_index);
  if(!c) return -1;

  c = cp_entry_by_index(class, c->val->cpnode.NameAndType.descriptor_index);
  if(!c) return -1;

  this_desc = cp_null_term_utf8(c->val);
  if(!this_desc) return -1;

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
      debug_err("get_stack_decrement(): unexpected op type\n");
      free(this_desc);
      return -1;
  }

  free(this_desc);

  return stack_decrement;
}

/**
 * Determines the number of bytes that this method invocation instruction 
 * (invokespecial, invokevirtual, invokestatic, invokeinterface) removes
 * from the stack prior to execution.  
 *
 * @param class -- The class containing the constant pool relevant to
 *   this instruction (i.e. the class containing the method containing the
 *   instruction).
 * @param op -- The instruction opcode.
 * @param index -- The operand to the instruction.
 * 
 * @returns The number of bytes removed from the stack before execution.
 */

static int
get_stack_dec_invocation(JVM_CLASS *class, JVM_OPCODE op, u4 index)
{
  JVM_STACK_INFO *stackinf;
  int stack_decrement;
  char *this_desc;
  int int_idx;
  CP_NODE *c;

  if(!class) {
    BAD_ARG(); 
    return 0;
  }

  int_idx = (int)index;

  if(op == jvm_invokeinterface)
    int_idx >>= 16;

  /* now we need to determine how many parameters are sitting on the stack */
  c = cp_entry_by_index(class, int_idx);
  if(!c) return -1;

  c = cp_entry_by_index(class, c->val->cpnode.Methodref.name_and_type_index);
  if(!c) return -1;

  c = cp_entry_by_index(class, c->val->cpnode.NameAndType.descriptor_index);
  if(!c) return -1;

  this_desc = cp_null_term_utf8(c->val);
  if(!this_desc) return -1;

  stackinf = calc_stack(this_desc);
  if(!stackinf) {
    free(this_desc);
    return -1;
  }

  /* if the opcode is invokespecial or invokevirtual, then there is one
   * object reference + parameters on the stack.  if this is an invokestatic
   * instruction, then there's just parameters.
   */
  if(op == jvm_invokestatic)
    stack_decrement = stackinf->arg_len;
  else
    stack_decrement = stackinf->arg_len + 1;

  free(stackinf);
  free(this_desc);

  return stack_decrement;
}

/**
 * Determines the number of bytes that this instruction leaves on the stack
 * after execution.  this depends on the instruction and on the data types.
 * e.g. for a method invoke instruction, the number of bytes depends on the
 * return type of the method (double/long = 2 stack entries).
 *
 * @param meth -- The method in which this instruction is located.
 * @param op -- The instruction opcode.
 * @param index -- The operand to the instruction.
 * 
 * @returns The number of bytes added to the stack after execution.
 */

static int
get_stack_increment(JVM_METHOD *meth, JVM_OPCODE op, u4 index)
{
  int stack_increment;
  Dlist const_table;

  if(!meth) {
    BAD_ARG(); 
    return 0;
  }

  const_table = meth->class->constant_pool;

  switch(op) {
    case jvm_invokespecial:
    case jvm_invokevirtual:
    case jvm_invokestatic:
    case jvm_invokeinterface:
      stack_increment = get_stack_inc_invocation(meth->class, op, index);
      break;
    case jvm_putstatic:
    case jvm_getstatic:
    case jvm_putfield:
    case jvm_getfield:
      stack_increment = get_stack_inc_field_acc(meth->class, op, index);
      break;
    default:
      /* else we can determine the stack increment from a table.  */
      stack_increment = jvm_opcode[op].stack_post;
  }
  
  return stack_increment;
}


/**
 * Determines the number of bytes that this method invocation instruction 
 * (invokespecial, invokevirtual, invokestatic, invokeinterface) leaves
 * on the stack after execution.  
 *
 * @param class -- The class containing the constant pool relevant to
 *   this instruction (i.e. the class containing the method containing the
 *   instruction).
 * @param op -- The instruction opcode.
 * @param index -- The operand to the instruction.
 * 
 * @returns The number of bytes left on the stack after execution.
 */

static int
get_stack_inc_invocation(JVM_CLASS *class, JVM_OPCODE op, u4 index)
{
  JVM_STACK_INFO *stackinf;
  int stack_increment;
  char *this_desc;
  CP_NODE *c;
  int int_idx;

  if(!class) {
    BAD_ARG(); 
    return 0;
  }

  int_idx = index;

  if(op == jvm_invokeinterface)
    int_idx >>= 16;
    
  /* now we need to determine how many parameters are sitting on the stack */
  c = cp_entry_by_index(class, int_idx);
  if(!c) return -1;

  c = cp_entry_by_index(class, c->val->cpnode.Methodref.name_and_type_index);
  if(!c) return -1;

  c = cp_entry_by_index(class, c->val->cpnode.NameAndType.descriptor_index);
  if(!c) return -1;

  this_desc = cp_null_term_utf8(c->val);
  if(!this_desc) return -1;

  stackinf = calc_stack(this_desc);
  if(!stackinf) {
    free(this_desc);
    return -1;
  }

  /* if the opcode is invokespecial, invokevirtual, or invokeinterface then 
   * there is one object reference + parameters on the stack.  if this is an 
   * invokestatic instruction, then there's just parameters.
   */

  stack_increment = stackinf->ret_len;

  free(stackinf);
  free(this_desc);

  return stack_increment;
}

/**
 * Determines the number of bytes that this field access instruction 
 * (getfield, putfield, getstatic, putstatic) leaves on the stack
 * after execution.  
 *
 * @param class -- The class containing the constant pool relevant to
 *   this instruction (i.e. the class containing the method containing the
 *   instruction).
 * @param op -- The instruction opcode.
 * @param index -- The operand to the instruction.
 * 
 * @returns The number of bytes left on the stack after execution.
 */

static int
get_stack_inc_field_acc(JVM_CLASS *class, JVM_OPCODE op, u4 index)
{
  int stack_increment;
  char *this_desc;
  CP_NODE *c;
  int tmpsize;

  if(!class) {
    BAD_ARG(); 
    return 0;
  }

  c = cp_entry_by_index(class, index);
  if(!c) return -1;

  c = cp_entry_by_index(class, c->val->cpnode.Methodref.name_and_type_index);
  if(!c) return -1;

  c = cp_entry_by_index(class, c->val->cpnode.NameAndType.descriptor_index);
  if(!c) return -1;

  this_desc = cp_null_term_utf8(c->val);
  if(!this_desc) return -1;

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
      debug_err("get_stack_increment(): unexpected op type\n");
      free(this_desc);
      return -1;
  }

  free(this_desc);

  return stack_increment;
}

/**
 * Given a method descriptor, this function returns the number of arguments
 * it takes (actually the number returned may differ from the number of args
 * because doubles and longs take two stack entries per argument).  This value 
 * is used to determine how much to decrement the stack after a method 
 * invocation.
 *
 * @param d -- The method descriptor to analyze.
 *
 * @returns The number of stack entries used for the arguments of the method 
 *    with the given descriptor.
 */

static JVM_STACK_INFO *
calc_stack(char *d)
{
  JVM_STACK_INFO *tmp;
  int len = strlen(d);
  char *ptr, *tstr;

  if(!d) {
    BAD_ARG(); 
    return NULL;
  }

  debug_msg("in calc_stack, the desc = '%s'\n", d);

  tmp = (JVM_STACK_INFO *)malloc(sizeof(JVM_STACK_INFO));

  if(!tmp) return NULL;

  tmp->arg_len = 1;
  tmp->ret_len = 1;

  /* the shortest possible method descriptor should be 3 characters: ()V
   * thus, if the given string is < 3 characters, it must be in error.
   */

  if(len < 3) {
    debug_err("WARNING: invalid descriptor '%s' (len < 3).\n", d);
    return tmp;
  }

  if(d[0] != '(') {
    debug_err("WARNING: invalid descriptor '%s' (bad 1st char).\n", d);
    return tmp;
  }

  ptr = d;

  /* start at -1 because the opening paren will contribute 1 to
   * the count.
   */
  tmp->arg_len = -1;

  while((ptr = bc_next_desc_token(ptr)) != NULL) {
    tmp->arg_len++;

    /* check if this is a double or long type.  if so, increment
     * again because these data types take up two stack entries.
     */
    if( (*ptr ==  'D') || (*ptr == 'J') )
      tmp->arg_len++;
  }

  tstr = strdup(d);
  if(!tstr) {
    debug_err("WARNING: could not dup descriptor.\n");
    return tmp;
  }

  strtok(tstr,")");
  ptr = strtok(NULL,")");
  if( (*ptr ==  'D') || (*ptr == 'J') )
    tmp->ret_len = 2;
  else if(*ptr == 'V')
    tmp->ret_len = 0;
  else
    tmp->ret_len = 1;

  free(tstr);

  debug_msg("calc_stack arg_len = %d, ret_len = %d\n",
      tmp->arg_len, tmp->ret_len);

  return tmp;
}

/**
 * Increment the stacksize by the specified amount.  If this is the highest
 * stack value encountered, set max_stack to the current stacksize.
 *
 * @param meth -- The method whose stack should be increased.
 * @param inc -- The amount to increase the stack.
 */

static void
inc_stack(JVM_METHOD *meth, int inc)
{
  if(!meth) {
    BAD_ARG(); 
    return;
  }

  meth->stacksize += inc;

  if(meth->stacksize > meth->cur_code->attr.Code->max_stack)
    meth->cur_code->attr.Code->max_stack = (u2)meth->stacksize;
}

/**
 * Decrement the stacksize by the specified amount.
 *
 * @param meth -- The method whose stack should be decreased.
 * @param dec -- The amount to decrease the stack.
 *
 * @returns 0 on success, -2 on error, -1 on negative stack.
 */

static int
dec_stack(JVM_METHOD *meth, int dec) {
  if(!meth) {
    BAD_ARG(); 
    return -2;
  }

  meth->stacksize -= dec;

  if(meth->stacksize < 0)
    return -1;

  return 0;
}

  
/**
 * Prepares a tableswitch instruction to be written out.  This involves:
 *   -# Calculating the cell padding.
 *   -# Setting up the array of cases.
 *   -# Sorting the switch cases.
 *   -# Filling in missing cases with default information.
 *
 * @param val -- The tableswitch instruction node.
 *
 * @returns 0 on success, -1 on failure.
 */

static int
setup_tableswitch(JVM_CODE_GRAPH_NODE *val)
{ 
  Dlist tmp;
  int i, n;

  if(!val) {
    BAD_ARG(); 
    return -1;
  }

  val->op = jvm_tableswitch;
  
  n = val->switch_info->high - val->switch_info->low + 1;

  val->switch_info->sorted_entries =
     (JVM_SWITCH_ENTRY **)malloc(sizeof(JVM_SWITCH_ENTRY *) * n);
  if(!val->switch_info->sorted_entries) return -1;

  val->switch_info->num_entries = n;

  for(i = 0; i < n; i++)
    val->switch_info->sorted_entries[i] = NULL;

  /* set up the array of branch targets to be sorted */

  dl_traverse(tmp, val->switch_info->offsets) {
    JVM_SWITCH_ENTRY *entry = (JVM_SWITCH_ENTRY *) tmp->val;
    int idx;
  
    idx = entry->case_num - val->switch_info->low;
    val->switch_info->sorted_entries[idx] = entry;
    i++;
  }

  /* fill in any missing cases with the default branch target */

  for(i = 0; i < n; i++) {
    if(!val->switch_info->sorted_entries[i]) {
      JVM_SWITCH_ENTRY *new_entry =
         (JVM_SWITCH_ENTRY *)malloc(sizeof(JVM_SWITCH_ENTRY));

      if(!new_entry) return -1;

      new_entry->instr = val->switch_info->default_case;
      new_entry->case_num = val->switch_info->low + i;

      val->switch_info->sorted_entries[i] = new_entry;
    }
  }

  /* sort the switch cases */

  qsort(val->switch_info->sorted_entries, n, sizeof(JVM_SWITCH_ENTRY *),
     switch_entry_compare);

  /* need to calculate instruction width */

  val->switch_info->cell_padding = 3-(val->pc%4);
  val->width = 1 + val->switch_info->cell_padding + 12 + n * 4;

  return 0;
}

/**
 * Prepares a lookupswitch instruction to be written out.  This involves:
 *   -# Calculating the cell padding.
 *   -# Sorting the switch cases.
 *  
 * @param val -- The tableswitch instruction node.
 *
 * @returns 0 on success, -1 on failure.
 */

static int
setup_lookupswitch(JVM_CODE_GRAPH_NODE *val)
{
  Dlist tmp;
  int i, n;

  if(!val) {
    BAD_ARG(); 
    return -1;
  }

  val->op = jvm_lookupswitch;

  n = val->switch_info->num_entries;

  val->switch_info->sorted_entries =
     (JVM_SWITCH_ENTRY **)malloc(sizeof(JVM_SWITCH_ENTRY *) * n);

  if(!val->switch_info->sorted_entries) return -1;

  i = 0;

  /* set up the array of branch targets to be sorted */

  dl_traverse(tmp, val->switch_info->offsets) {
    val->switch_info->sorted_entries[i] = (JVM_SWITCH_ENTRY *) tmp->val;
    i++;
  }

  /* sort the switch cases */

  qsort(val->switch_info->sorted_entries, n, sizeof(JVM_SWITCH_ENTRY *),
     switch_entry_compare);

  /* need to calculate instruction width */

  val->switch_info->cell_padding = 3-(val->pc%4);
  val->width = 1 + val->switch_info->cell_padding + 8 + n * 8;

  return 0;
}

/**
 * Compares two switch entries.  This is used as an argument to qsort
 * when sorting the array of switch cases.
 *
 * @param e1 -- Switch entry.
 * @param e2 -- Switch entry.
 *
 * @returns 
 *    -# if e1 < e2, return -1
 *    -# if e1 == e2, return 0
 *    -# if e1 > e2, return  1
 */

static int
switch_entry_compare(const void *e1, const void *e2)
{
  JVM_SWITCH_ENTRY *s1, *s2;

  if(!e1 || !e2) {
    BAD_ARG(); 
    return 0;
  }

  s1 = *((JVM_SWITCH_ENTRY **)e1);
  s2 = *((JVM_SWITCH_ENTRY **)e2);

  if(s1->case_num < s2->case_num)
    return -1;

  if(s1->case_num == s2->case_num)
    return 0;

  return 1;
}


/**
 * Given a list and a graph node, this function returns the list node which
 * contains the graph node.
 *
 * @param cgraph -- The code graph (list).
 * @param n -- The node to find.
 *
 * @returns The list node containing the given instruction node.
 */

static Dlist
get_list_node(Dlist cgraph, JVM_CODE_GRAPH_NODE *n)
{
  Dlist tmp;

  if(!cgraph || !n) {
    BAD_ARG();
    return NULL;
  }

  dl_traverse(tmp,cgraph) {
    if((JVM_CODE_GRAPH_NODE *) tmp->val == n)
      return tmp;
  }

  return NULL;
}

/**
 * Checks whether a branch is too far.  currently the branch target offset
 * is a signed 16-bit integer, so the maximum branch is -2^15..2^15-1.
 *
 * @param op -- The branching opcode to be checked.
 * @param dest -- The branch destination address.
 * @param src -- The current address (i.e. the source of the branch).
 *
 * @returns TRUE if the branch is too far away, FALSE otherwise.
 */

static BOOL
check_distance(JVM_OPCODE op, int dest, int src)
{
  int distance;
  
  /* if it's a wide goto, then it'll always be ok.. otherwise check */
  if((op == jvm_goto_w) || (op == jvm_jsr_w))
    return FALSE;
  
  distance = dest - src;
  if((distance > ((int)math_pow( 2.0, 15.0 ) - 1)) ||
     (distance < ((int)-math_pow( 2.0, 15.0 ))))
    return TRUE;
  else
    return FALSE; 
}

/**
 * Simple double-precision power function.  Just writing this here so that we
 * dont have to link in the math library.
 *
 * @param x -- The base.
 * @param y -- The exponent.
 *
 * @returns x raised to the power of y.
 */

static double
math_pow(double x, double y)
{
  double result;
  int i;

  if(y < 0)
  {
    debug_err("Warning: got negative exponent in math_pow!\n");
    return 0.0;
  }

  if(y == 0)
    return 1.0;

  if(y == 1)
    return x;

  result = x;

  for(i=0;i<y-1;i++)
    result *= x;

  return result;
}

/**
 * Gets the proper return opcode for the given method descriptor.
 *
 * @param desc -- The complete method descriptor.
 *
 * @returns The proper type-specific return opcode (e.g. dreturn)
 *   for the method descriptor.
 */

static JVM_OPCODE
get_method_return_op(char *desc) {
  char *p = desc;

  while( p && (*p != ')') )
    p++;

  if(!p) {
    debug_err("get_method_return_type: screwed up descriptor.\n");
    return jvm_return;
  }

  /* skip the ')' */
  p++;

  switch (*p) {
    case 'B': return jvm_ireturn;
    case 'C': return jvm_ireturn;
    case 'D': return jvm_dreturn;
    case 'F': return jvm_freturn;
    case 'I':
    case 'Z': return jvm_ireturn;
    case 'J': return jvm_lreturn;
    case 'L':
    case '[': return jvm_areturn;
    case 'S': return jvm_ireturn;
    case 'V': return jvm_return;
    default:
      debug_err("get_method_return_type: did not expect to be here.\n");
  }

  return jvm_return;
}

/**
 * Searches a list of Label nodes for the one corresponding to the given
 * label.  From this label node, we can get the PC of the statement
 * corresponding to this node.
 *
 * @param l -- The list of labels in some method.
 * @param val -- The label string.
 *
 * @returns The address of the instruction corresponding to the given
 *    label.  If the label is not found, return -1.
 */

static int
find_label(Dlist l, const char *val)
{
  Dlist tmp;
  JVM_BRANCH_PC *bp;

  if(!l) {
    BAD_ARG(); 
    return -1;
  }

  dl_traverse(tmp,l) {
    bp = (JVM_BRANCH_PC *) tmp->val;
    if(!strcmp(bp->label, val))
      return bp->instr->pc;
  }

  return -1;
}
