/** @file api.c
 * Contains an API for generating Java bytecode.
 */

#include "api.h"

/**
 * This code creates the JVM_FIELD structure, assigns the
 * appropriate values into it, and inserts it into the field list.
 *
 * @param cclass -- The class to which the field should be added.
 * @param name -- The name of the field.
 * @param desc -- The field descriptor.
 * @param acc_flag -- The access flags for this field (for example
 *     JVM_ACC_PUBLIC, JVM_ACC_STATIC, etc)
 *
 * @returns The new JVM_FIELD structure.
 */

JVM_FIELD *
bc_add_field(JVM_CLASS *cclass, char *name, char *desc, u2 acc_flag)
{
  JVM_FIELD * tmpfield;
  int c;

  if(!cclass || !name || !desc) {
    BAD_ARG();
    return NULL;
  }

  debug_msg("bc_add_field() creating new field for %s - %s\n",name,desc);

  tmpfield = (JVM_FIELD *) malloc(sizeof(JVM_FIELD));
  
  if(!tmpfield)
    return NULL;

  tmpfield->access_flags = acc_flag;
  tmpfield->class = cclass;

  c = cp_find_or_insert(cclass, CONSTANT_Utf8, name);
  if(c < 0) {
    free(tmpfield);
    return NULL;
  }

  tmpfield->name_index = c;

  c = cp_find_or_insert(cclass, CONSTANT_Utf8, desc);
  if(c < 0) {
    free(tmpfield);
    return NULL;
  }

  tmpfield->descriptor_index = c;

  tmpfield->attributes_count = 0;
  tmpfield->attributes = make_dl();

  if(!tmpfield->attributes) {
    free(tmpfield);
    return NULL;
  }

  dl_insert_b(cclass->fields, tmpfield);

  cclass->fields_count++;

  return tmpfield;
}

/**
 * Returns the fully-qualified class name for the given class.
 * Generally this is the package name followed by the class name,
 * however the class name could already be a qualified name.
 *
 * @param thisclass -- The name of the class.
 * @param package_name -- The name of the package.  If NULL, the
 *   fully-qualified name is just the class name.
 *
 * @returns The fully-qualified class name.  Returns NULL on error.
 */

char *
bc_get_full_classname(char *thisclass, char *package_name)
{
  char *pname, *t;

  if(!thisclass) {
    BAD_ARG();
    return NULL;
  }
    
  /* maybe this is already qualified.  if so, just return a dup of the
   * class name.
   */
  for(t = thisclass; *t != '\0'; t++)
    if( (*t == '/') || (*t == '.') )
      return char_substitute(thisclass, '.', '/');

  if(package_name != NULL) {
    pname = (char *)malloc(strlen(thisclass) + strlen(package_name) + 2);

    if(!pname)
      return NULL;

    /* issue a warning if the package name has some trailing junk. */
    if(!isalnum((int)*(package_name + (strlen(package_name)-1))))
      debug_err("WARNING: last char of package name not alphanumeric.\n");

    t = char_substitute(package_name, '.', '/');

    if(!t) {
      free(pname);
      return NULL;
    }

    strcpy(pname, t);
    strcat(pname, "/");
    strcat(pname, thisclass);

    free(t);
    return pname;
  }
  else
    return strdup(thisclass);
}

/**
 * Creates a new class file structure.
 *
 * @param name -- The name of the class.
 * @param srcFile -- The name of the source code file from which this
 *   class was compiled.  If NULL, no SourceFile attribute will be created
 *   for this class.
 * @param super_class -- The name of the superclass for this class.  If NULL,
 *   the superclass is set to java.lang.Object.
 * @param package_name -- The name of the package this class file belongs to.
 *   If NULL, no package will be specified.
 * @param acc_flag -- The access flags for this class (for example
 *     JVM_ACC_PUBLIC, etc)
 *
 * @returns The new class file structure.
 */

JVM_CLASS *
bc_new_class(char *name, char *srcFile, char *super_class, 
   char *package_name, u2 acc_flag)
{
  CP_INFO *utf8node = NULL, *classnode = NULL;
  JVM_CLASS * tmp = NULL;
  char * fullclassname = NULL;
  int c;

#define err_new_class() \
      if(tmp->constant_pool) dl_delete_list(tmp->constant_pool); \
      tmp->constant_pool = NULL; \
      if(tmp->fields) dl_delete_list(tmp->fields); \
      tmp->fields = NULL; \
      if(tmp->interfaces) dl_delete_list(tmp->interfaces); \
      tmp->interfaces = NULL; \
      if(tmp->attributes) dl_delete_list(tmp->attributes); \
      tmp->attributes = NULL; \
      if(tmp->methods) dl_delete_list(tmp->methods); \
      tmp->methods = NULL; \
      if(fullclassname) free(fullclassname); \
      if(tmp) free(tmp); \
      if(utf8node && utf8node->cpnode.Utf8.bytes) \
         free(utf8node->cpnode.Utf8.bytes); \
      if(classnode && classnode->cpnode.Utf8.bytes) \
         free(classnode->cpnode.Utf8.bytes);
  
  if(!name) {
    BAD_ARG();
    return NULL;
  }

  tmp = (JVM_CLASS *)malloc(sizeof(JVM_CLASS));

  if(!tmp)
    return NULL;

  tmp->magic = JVM_MAGIC;

  bc_set_class_version(tmp, JVM_MAJOR_VER, JVM_MINOR_VER);

  /* we'll fill out the constant pool and fields later. */
  tmp->constant_pool_count = 0;
  tmp->constant_pool = make_dl();

  tmp->fields_count = 0;
  tmp->fields = make_dl();

  tmp->interfaces_count = 0;
  tmp->interfaces = make_dl();

  tmp->attributes_count = 0;
  tmp->attributes = make_dl();

  tmp->methods_count = 0;
  tmp->methods = make_dl();

  tmp->access_flags = acc_flag;

  if(!tmp->constant_pool || !tmp->fields || !tmp->interfaces || 
     !tmp->attributes || !tmp->methods) 
  {
    err_new_class();
    return NULL;
  }

  /* first create an entry for 'this'.  the class file variable this_class
   * points to a CONSTANT_Class_info entry in the constant pool, which in
   * turn points to a CONSTANT_Utf8_info entry representing the name of
   * this class.  so, first we create the Utf8 entry, then the Class entry.
   */

  fullclassname = bc_get_full_classname(name, package_name);

  if(!fullclassname) {
    err_new_class();
    return NULL;
  }

  debug_msg("##creating new entry, this -> %s\n",fullclassname);

  c = cp_find_or_insert(tmp, CONSTANT_Class, fullclassname);
  if(c < 0) {
    err_new_class();
    return NULL;
  }

  tmp->this_class = c;

  /* if a superclass was specified, then insert an entry for it into
   * the constant pool and set the superclass field in the class struct.
   * otherwise, set the superclass to java.lang.Object.
   */

  if(super_class) {
    char *sc;

    sc = char_substitute(super_class, '.', '/');

    if(!sc) {
      err_new_class();
      return NULL;
    }

    c = cp_find_or_insert(tmp, CONSTANT_Class, sc);

    free(sc);

    if(c < 0) {
      err_new_class();
      if(sc) free(sc);
      return NULL;
    }

    tmp->super_class = c;
  }
  else {
    c = cp_find_or_insert(tmp, CONSTANT_Class, "java/lang/Object");

    if(c < 0) {
      err_new_class();
      return NULL;
    }

    tmp->super_class = c;
  }

  /* the only attributes allowed for a class file are SourceFile and
   * Deprecated.  if srcFile was supplied by the user, then add a
   * SourceFile attribute to this class.
   */

  if(srcFile) {
    if(bc_add_source_file_attr(tmp, srcFile)) {
      err_new_class();
      return NULL;
    }
  }

  free(fullclassname);

  return tmp;
}

/**
 * Sets the version for this class file.  From the JVM Spec:
 *
 * The Java virtual machine implementation of Sun's JDK release 1.0.2
 * supports class file format versions 45.0 through 45.3 inclusive.
 * Sun's JDK releases 1.1.X can support class file formats of versions
 * in the range 45.0 through 45.65535 inclusive. Implementations of
 * version 1.2 of the Java 2 platform can support class file formats
 * of versions in the range 45.0 through 46.0 inclusive.
 *
 * @param class -- The class file whose version is to be set.
 * @param major -- The major version.
 * @param minor -- The minor version.
 *
 * @returns 0 on success, -1 on failure.
 */

int
bc_set_class_version(JVM_CLASS *class, int major, int minor)
{
  if(!class) {
    BAD_ARG();
    return -1;
  }

  class->major_version = (u2)major;
  class->minor_version = (u2)minor;

  if((unsigned int)class->major_version != major)
    debug_err("Warning: possible truncation in bc_set_class_version.\n");

  if((unsigned int)class->minor_version != minor)
    debug_err("Warning: possible truncation in bc_set_class_version.\n");

  return 0;
}

/**
 * Creates a SourceFile attribute containing the specified name and adds it
 * to the given class file.
 *
 * @param class -- The class to which the SourceFile attribute should be
 *    added.
 * @param filename -- The name of the source code file from which this
 *   class was compiled.
 *
 * @returns 0 on success, -1 on failure.
 */

int
bc_add_source_file_attr(JVM_CLASS *class, char *filename)
{
  JVM_ATTRIBUTE *attr_temp;
  int c;

  if(!class || !filename) {
    BAD_ARG();
    return -1;
  }

  class->attributes_count++;

  attr_temp = (JVM_ATTRIBUTE *)malloc(sizeof(JVM_ATTRIBUTE));

  if(!attr_temp)
    return -1;

  c = cp_find_or_insert(class, CONSTANT_Utf8, "SourceFile");

  if(c < 0) {
    free(attr_temp);
    return -1;
  }

  attr_temp->attribute_name_index = c;
  attr_temp->attribute_length = 2;  /* SourceFile attr length always 2 */
  attr_temp->attr.SourceFile = (struct SourceFile_attribute *)
       malloc(sizeof(struct SourceFile_attribute));

  if(!attr_temp->attr.SourceFile) {
    free(attr_temp);
    return -1;
  }

  c = cp_find_or_insert(class, CONSTANT_Utf8, filename);

  if(c < 0) {
    free(attr_temp);
    free(attr_temp->attr.SourceFile);
    return -1;
  }

  attr_temp->attr.SourceFile->sourcefile_index = c;

  dl_insert_b(class->attributes,attr_temp);

  return 0;
}

/**
 * Lets the user define their own attribute and add it to the class file.
 *
 * @param class -- The class to which this attribute should be added.
 * @param attribute_name -- The name of the attribute.
 * @param attribute_length -- The length of the attribute pointed to by the
 *    'attribute_data' parameter.
 * @param attribute_data -- Pointer to the attribute contents.
 *
 * @returns 0 on success, -1 on failure.
 */

int
bc_add_user_defined_class_attr(JVM_CLASS *class, char *attribute_name,
  int attribute_length, void *attribute_data)
{
  JVM_ATTRIBUTE *attr_temp;
  int c;

  if(!class || !attribute_name || !attribute_data) {
    BAD_ARG();
    return -1;
  }

  attr_temp = (JVM_ATTRIBUTE *)malloc(sizeof(JVM_ATTRIBUTE));

  if(!attr_temp)
    return -1;

  c = cp_find_or_insert(class, CONSTANT_Utf8, attribute_name);

  if(c < 0) {
    free(attr_temp);
    return -1;
  }

  attr_temp->attribute_name_index = c;
  attr_temp->attribute_length = attribute_length;
  attr_temp->attr.UserDefined = (struct UserDefined_attribute *)
       malloc(sizeof(struct UserDefined_attribute));

  if(!attr_temp->attr.UserDefined) {
    free(attr_temp);
    return -1;
  }

  attr_temp->attr.UserDefined->data = (void *)malloc(attribute_length);

  if(!attr_temp->attr.UserDefined->data) {
    free(attr_temp->attr.UserDefined);
    free(attr_temp);
    return -1;
  }

  memcpy(attr_temp->attr.UserDefined->data, attribute_data, attribute_length);

  class->attributes_count++;

  dl_insert_b(class->attributes,attr_temp);

  return 0;
}

/**
 * Adds the "Deprecated" attribute to the specified class.
 *
 * @param class -- The class to be set as deprecated.
 *
 * @returns 0 on success, -1 on failure.
 */

int
bc_set_class_deprecated(JVM_CLASS *class)
{
  JVM_ATTRIBUTE *attr_temp;

  if(!class) {
    BAD_ARG();
    return -1;
  }

  attr_temp = bc_new_deprecated_attr(class);

  if(!attr_temp)
    return -1;

  class->attributes_count++;

  dl_insert_b(class->attributes,attr_temp);

  return 0;
}

/**
 * Adds the specified interface to the list of interfaces that
 * this class implements.
 *
 * @param class -- The class to which the interface should be added.
 * @param interface -- The name of the interface that this class implements.
 *
 * @returns 0 on success, -1 on failure.
 */

int
bc_add_class_interface(JVM_CLASS *class, char *interface)
{
  int *copy;
  int c;
  char *t;

  if(!class || !interface) {
    BAD_ARG();
    return -1;
  }

  t = char_substitute(interface, '.', '/');

  if(!t)
    return -1;

  c = cp_find_or_insert(class, CONSTANT_Class, t);

  free(t);

  if(c < 0) {
    free(t);
    return -1;
  }

  copy = (int *)malloc(sizeof(int));

  if(!copy) {
    free(t);
    return -1;
  }

  *copy = c;

  class->interfaces_count++;

  dl_insert_b(class->interfaces, copy);

  return 0;
}

/**
 * Adds the "ConstantValue" attribute to the specified field.  This allows
 * specifying the value that the field should have when the class containing
 * it is initialized.  Since a field with a ConstantValue attribue must be
 * static, this function will set the JVM_ACC_STATIC flag in the field's
 * access flags.
 *
 * @param field -- The field to which the ConstantValue attribute should be
 *   added.
 * @param tag -- The type of this constant (e.g. CONSTANT_Integer, 
 *   CONSTANT_Utf8, etc).  See the JVM_CONSTANT enum for the possible
 *   data types.
 * @param value -- Pointer to the constant value.
 *
 * @returns 0 on success, -1 on failure.
 */

int
bc_set_constant_value_attr(JVM_FIELD *field, 
  JVM_CONSTANT tag, const void *value)
{
  JVM_ATTRIBUTE *attr_temp;
  int c;
  int val_idx;

  if(!field || !value) {
    BAD_ARG();
    return -1;
  }

  c = cp_manual_insert(field->class, tag, value);

  if(c < 0)
    return -1;

  val_idx = c;  

  /* JVM spec says that the ACC_STATIC flag must be set for a field
   * which has a ConstantValue attribute.
   */
  field->access_flags |= JVM_ACC_STATIC;

  attr_temp = (JVM_ATTRIBUTE *)malloc(sizeof(JVM_ATTRIBUTE));

  if(!attr_temp)
    return -1;

  c = cp_find_or_insert(field->class, CONSTANT_Utf8, "ConstantValue");
  if(c < 0) {
    free(attr_temp);
    return -1;
  }

  attr_temp->attribute_name_index = c;
  attr_temp->attribute_length = 2;  /* ConstantValue attr length always 2 */
  attr_temp->attr.ConstantValue = (struct ConstantValue_attribute *)
       malloc(sizeof(struct ConstantValue_attribute));

  if(!attr_temp->attr.ConstantValue) {
    free(attr_temp);
    return -1;
  }

  attr_temp->attr.ConstantValue->constantvalue_index = val_idx;

  field->attributes_count++;

  dl_insert_b(field->attributes,attr_temp);

  return 0;
}

/**
 * Adds the "Synthetic" attribute to the specified field.  The Synthetic
 * attribute is used for class members that do not appear in the source code.
 *
 * @param field -- The field to which the Synthetic attribute should be
 *   added.
 *
 * @returns 0 on success, -1 on failure.
 */

int
bc_set_field_synthetic(JVM_FIELD *field)
{
  JVM_ATTRIBUTE *attr_temp;

  if(!field) {
    BAD_ARG();
    return -1;
  }

  attr_temp = bc_new_synthetic_attr(field->class);

  if(!attr_temp)
    return -1;

  field->attributes_count++;

  dl_insert_b(field->attributes,attr_temp);

  return 0;
}

/**
 * Adds the "Deprecated" attribute to the specified field.
 *
 * @param field -- The field to which the Deprecated attribute should be
 *   added.
 *
 * @returns 0 on success, -1 on failure.
 */

int
bc_set_field_deprecated(JVM_FIELD *field)
{
  JVM_ATTRIBUTE *attr_temp;

  if(!field) {
    BAD_ARG();
    return -1;
  }

  attr_temp = bc_new_deprecated_attr(field->class);

  if(!attr_temp)
    return -1;

  field->attributes_count++;

  dl_insert_b(field->attributes,attr_temp);

  return 0;
}

/**
 * Adds the "Deprecated" attribute to the specified method.
 *
 * @param meth -- The method to which the Deprecated attribute should be
 *   added.
 *
 * @returns 0 on success, -1 on failure.
 */

int
bc_set_method_deprecated(JVM_METHOD *meth)
{
  JVM_ATTRIBUTE *attr_temp;

  if(!meth) {
    BAD_ARG();
    return -1;
  }

  attr_temp = bc_new_deprecated_attr(meth->class);

  if(!attr_temp)
    return -1;

  meth->attributes_count++;

  dl_insert_b(meth->attributes,attr_temp);

  return 0;
}

/**
 * Creates a new "Deprecated" attribute.  This attribute can be
 * added to a class, field, or method.
 *
 * @param class -- Class containing the constant pool where this
 *   attribute will be stored.
 *
 * @returns Pointer to the new JVM_ATTRIBUTE.
 *    Returns NULL on error.
 */

JVM_ATTRIBUTE *
bc_new_deprecated_attr(JVM_CLASS *class) 
{
  JVM_ATTRIBUTE *attr_temp;
  int c;

  if(!class) {
    BAD_ARG();
    return NULL;
  }

  attr_temp = (JVM_ATTRIBUTE *)malloc(sizeof(JVM_ATTRIBUTE));

  if(!attr_temp)
    return NULL;

  c = cp_find_or_insert(class, CONSTANT_Utf8, "Deprecated");

  if(c < 0) {
    free(attr_temp);
    return NULL;
  }

  attr_temp->attribute_name_index = c;
  attr_temp->attribute_length = 0;  /* Deprecated attr length always 0 */

  return attr_temp;
}

/**
 * Creates a new "Synthetic" attribute.  This attribute can be
 * added to a field or method.
 *
 * @param class -- Class containing the constant pool where this
 *   attribute will be stored.
 *
 * @returns Pointer to the new JVM_ATTRIBUTE.
 *    Returns NULL on error.
 */

JVM_ATTRIBUTE *
bc_new_synthetic_attr(JVM_CLASS *class) 
{
  JVM_ATTRIBUTE *attr_temp;
  int c;

  if(!class) {
    BAD_ARG();
    return NULL;
  }

  attr_temp = (JVM_ATTRIBUTE *)malloc(sizeof(JVM_ATTRIBUTE));

  if(!attr_temp)
    return NULL;

  c = cp_find_or_insert(class, CONSTANT_Utf8, "Synthetic");

  if(c < 0) {
    free(attr_temp);
    return NULL;
  }

  attr_temp->attribute_name_index = c;
  attr_temp->attribute_length = 0;  /* Synthetic attr length always 0 */

  return attr_temp;
}

/**
 * Adds the "Synthetic" attribute to the specified method of the specified
 * class.
 *
 * @returns 0 on success, -1 on failure.
 */

int
bc_set_method_synthetic(JVM_METHOD *meth)
{
  JVM_ATTRIBUTE *attr_temp;

  if(!meth) {
    BAD_ARG();
    return -1;
  }

  attr_temp = bc_new_synthetic_attr(meth->class);

  if(!attr_temp)
    return -1;

  meth->attributes_count++;

  dl_insert_b(meth->attributes,attr_temp);

  return 0;
}

/**
 * Adds an exception that this method could throw.
 *
 * @param meth -- The method to which the exception should be added.
 * @param exception -- The name of the exception that this method
 *   may throw.
 *
 * @returns 0 on success, -1 on failure.
 */

int
bc_add_method_exception(JVM_METHOD *meth, char *exception)
{
  JVM_ATTRIBUTE *attr;
  int *copy;
  int c;
  char *t;

  if(!meth || !exception) {
    BAD_ARG();
    return -1;
  }

  t = char_substitute(exception, '.', '/');

  if(!t) return -1;

  c = cp_find_or_insert(meth->class, CONSTANT_Class, t);

  free(t);

  if(c < 0) return -1;

  copy = (int *)malloc(sizeof(int));

  if(!copy) return -1;

  *copy = c;

  attr = find_attribute(meth->class, meth->attributes, "Exceptions");

  if(!attr) {
    attr = bc_new_exceptions_attr(meth->class);

    if(!attr) {
      free(copy);
      return -1;
    }

    meth->attributes_count++;
    dl_insert_b(meth->attributes, attr);
  }

  attr->attribute_length+=2;
  attr->attr.Exceptions->number_of_exceptions++;

  dl_insert_b(attr->attr.Exceptions->exception_index_table, copy);

  return 0;
}

/**
 * Adds the "InnerClasses" attribute to the specified class.
 *
 * @param class -- The class to which the attribute should be added.
 * @param inner_class -- The name of the inner class.
 * @param outer_class -- The name of the class containing the inner class.
 * @param inner_name -- Specify NULL for an anonymous inner class.  Otherwise
 *   this is the simple name of the inner class.
 * @param acc_flags -- The access flags for the inner class (for example
 *     JVM_ACC_PUBLIC, JVM_ACC_STATIC, etc)
 *
 * @returns 0 on success, -1 on failure.
 */

int
bc_add_inner_classes_attr(JVM_CLASS *class, char *inner_class,
  char *outer_class, char *inner_name, int acc_flags)
{
  struct InnerClassEntry *entry;
  JVM_ATTRIBUTE *attr;
  int c;
  char *t;

  if(!class) {
    BAD_ARG();
    return -1;
  }

  attr = find_attribute(class, class->attributes, "InnerClasses");

  if(!attr) {
    attr = bc_new_inner_classes_attr(class);
    if(!attr) return -1;
    class->attributes_count++;
    dl_insert_b(class->attributes, attr);
  }

  /* increment the length by the size of one entry in the inner class list */

  entry = (struct InnerClassEntry *)malloc(sizeof(struct InnerClassEntry));

  if(!entry) return -1;

  entry->inner_class_info_index = 0;
  entry->outer_class_info_index = 0;
  entry->inner_name_index = 0;

  entry->inner_class_access_flags = acc_flags;

  if(inner_class) {
    t = char_substitute(inner_class, '.', '/');
    if(!t) {
      free(entry);
      return -1;
    }

    c = cp_find_or_insert(class, CONSTANT_Class, t);

    free(t);

    if(c < 0) {
      free(entry);
      return -1;
    }

    entry->inner_class_info_index = c;
  }

  if(outer_class) {
    t = char_substitute(outer_class, '.', '/');
    if(!t) {
      free(entry);
      return -1;
    }

    c = cp_find_or_insert(class, CONSTANT_Class, t);

    free(t);

    if(c < 0) {
      free(entry);
      return -1;
    }

    entry->outer_class_info_index = c;
  }

  if(inner_name) {
    t = char_substitute(inner_name, '.', '/');
    if(!t) {
      free(entry);
      return -1;
    }

    c = cp_find_or_insert(class, CONSTANT_Utf8, t);

    free(t);

    if(c < 0) {
      free(entry);
      return -1;
    }

    entry->inner_name_index = c;
  }

  attr->attribute_length+=8;
  attr->attr.InnerClasses->number_of_classes++;

  dl_insert_b(attr->attr.InnerClasses->classes, entry);

  return 0;
}

/**
 * Sets the name of a local variable in the specified method.
 *
 * @param meth -- The method containing the local variable.
 * @param num -- The local variable number whose name should be set.
 * @param name -- The name of the variable.
 * @param desc -- The descriptor of the variable.
 *
 * @returns Pointer to the local variable table entry created for
 *   this variable.  Returns NULL on error.
 */

JVM_LOCAL_VARIABLE_TABLE_ENTRY *
bc_set_local_var_name(JVM_METHOD *meth, int num, char *name, char *desc)
{
  JVM_LOCAL_VARIABLE_TABLE_ENTRY *loc;

  if(!meth || !name || !desc) {
    BAD_ARG();
    return NULL;
  }

  loc = (JVM_LOCAL_VARIABLE_TABLE_ENTRY *) 
    malloc(sizeof(JVM_LOCAL_VARIABLE_TABLE_ENTRY));

  if(!loc) return NULL;

  loc->index = num;
  loc->name = strdup(name);
  loc->name_index = 0;
  loc->descriptor = char_substitute(desc, '.', '/');
  loc->descriptor_index = 0;
  loc->start = NULL;
  loc->end = NULL;

  if(!loc->descriptor || !loc->name) {
    if(loc->name) free(loc->name);
    if(loc->descriptor) free(loc->descriptor);
    free(loc); 
    return NULL;
  }

  dl_insert_b(meth->locals_table, loc);

  return loc;
}

/**
 * Sets the start of this named local variable.  That is, the instruction from
 * which the given local variable table entry is valid.
 *
 * @param loc -- The local variable table entry for the variable.
 * @param instr -- The first instruction for which this variable is defined.
 *
 * @returns 0 on success, -1 on failure.
 */

int
bc_set_local_var_start(JVM_LOCAL_VARIABLE_TABLE_ENTRY *loc, JVM_CODE_GRAPH_NODE *instr)
{
  if(!loc || !instr) {
    BAD_ARG();
    return -1;
  }

  loc->start = instr;

  return 0;
}

/**
 * Sets the end of this named local variable.  That is, the instruction after
 * which the given local variable table entry would not be valid.
 *
 * @param loc -- The local variable table entry for the variable.
 * @param instr -- The last instruction for which this variable is defined.
 *
 * @returns 0 on success, -1 on failure.
 */

int
bc_set_local_var_end(JVM_LOCAL_VARIABLE_TABLE_ENTRY *loc, JVM_CODE_GRAPH_NODE *instr)
{
  if(!loc || !instr) {
    BAD_ARG();
    return -1;
  }

  loc->end = instr;

  return 0;
}

/**
 * Sets the line number (from the original source file) for the given
 * JVM instruction.
 *
 * @param meth -- The method containing the line number table to be updated.
 * @param instr -- The instruction corresponding to the given line number.
 * @param lnum -- The line number from the original source code.
 *
 * @returns 0 on success, -1 on failure.
 */

int
bc_set_line_number(JVM_METHOD *meth, JVM_CODE_GRAPH_NODE *instr, int lnum)
{
  JVM_LINE_NUMBER_TABLE_ENTRY *tmp;

  if(!meth || !instr) {
    BAD_ARG();
    return -1;
  }

  tmp = (JVM_LINE_NUMBER_TABLE_ENTRY *) malloc(sizeof(JVM_LINE_NUMBER_TABLE_ENTRY));

  if(!tmp) return -1;
  
  tmp->op = instr;
  tmp->line_number = lnum;

  dl_insert_b(meth->line_table, tmp);

  return 0;
}

/**
 * Creates a new exception table entry.  The exception table entry
 * represents the range of instructions for which the given exception
 * should be trapped.
 *
 * @param meth -- The method containing the following instructions.
 * @param from -- The first instruction from which the exception should be
 *   caught.
 * @param to -- The last instruction to which the exception applies.
 * @param target -- The first instruction of the catch block.  This is where
 *   the JVM branches when the exception is caught.
 * @param exc_class -- The name of the exception class which should be caught.
 *
 * @returns The exception table entry.
 */

JVM_EXCEPTION_TABLE_ENTRY *
bc_new_exception_table_entry(JVM_METHOD *meth, JVM_CODE_GRAPH_NODE *from, 
  JVM_CODE_GRAPH_NODE * to, JVM_CODE_GRAPH_NODE * target, char *exc_class)
{
  JVM_EXCEPTION_TABLE_ENTRY *new_et;

  if(!meth || !from || !to || !target) {
    BAD_ARG();
    return NULL;
  }

  new_et = (JVM_EXCEPTION_TABLE_ENTRY *)malloc(sizeof(JVM_EXCEPTION_TABLE_ENTRY));

  if(!new_et) return NULL;

  new_et->from = from;
  new_et->to = to;
  new_et->target = target;

  /* check if the exception type was specified, then insert an entry
   * in the constant pool if necessary and set the catch_type field.
   * otherwise it should be set to 0.
   */

  if(exc_class) {
    char *etmp;
    int c;

    etmp = char_substitute(exc_class, '.', '/');
    if(!etmp) { 
      free(new_et);
      return NULL;
    }

    c = cp_find_or_insert(meth->class, CONSTANT_Class, etmp);

    free(etmp);

    if(c < 0) { 
      free(new_et);
      return NULL;
    }

    new_et->catch_type = c;
  }
  else
    new_et->catch_type = 0;

  return new_et;
}

/**
 * Adds the specified exception table entry to the specified method.
 *
 * @param meth -- The method to which the exception table entry should be
 *   added.
 * @param et_entry -- The exception table entry to add to this method.
 *
 * @returns 0 on success, -1 on failure.
 */

int
bc_add_exception_handler(JVM_METHOD *meth,
   JVM_EXCEPTION_TABLE_ENTRY *et_entry)
{
  if(!meth || !et_entry) {
    BAD_ARG();
    return -1;
  }

  dl_insert_b(meth->exc_table, et_entry);

  return 0;
}

/**
 * Returns a new code graph node initialized with the given opcode, operand,
 * and pc.
 *
 * @param meth -- The method containing the instruction.
 * @param op -- The opcode of the instruction.
 * @param operand -- The instruction's operand.
 *
 * @returns The new code graph node with this opcode.
 */

JVM_CODE_GRAPH_NODE *
bc_new_graph_node(JVM_METHOD *meth, JVM_OPCODE op, u4 operand)
{
  JVM_CODE_GRAPH_NODE *tmp;
  
  if(!meth) {
    BAD_ARG();
    return NULL;
  }

  tmp = (JVM_CODE_GRAPH_NODE *)malloc(sizeof(JVM_CODE_GRAPH_NODE));

  if(!tmp) return NULL;
 
  tmp->op = op;
  tmp->operand = operand;
  tmp->width = bc_op_width(op);

  /* set pc and branch targets later */
  tmp->pc = meth->pc;
  tmp->branch_target = NULL;
  tmp->next = NULL;
  tmp->branch_label = NULL;
  tmp->stack_depth = -1;
  tmp->visited = FALSE;

  return tmp;
}

/**
 * Creates a new method structure with the given access flags.
 *
 * @param cclass -- The class to which the new method should be added.
 * @param name -- The name of the method.
 * @param desc -- The method descriptor.  This can be NULL initially but
 *   the method descriptor must be set before calling bc_write_class().
 * @param flags -- The access flags for the method.
 *
 * @returns Pointer to the new method structure.
 *    Returns NULL on error.
 */

JVM_METHOD *
bc_new_method(JVM_CLASS *cclass, char *name, char *desc, unsigned int flags)
{
  JVM_METHOD *meth;
  int lv_start;
  int c;
  u2 acc;

  if(!cclass || !name) {
    BAD_ARG();
    return NULL;
  }

#define err_new_meth() \
     if(meth && meth->name) free(meth->name); \
     free(meth);

  acc = (u2) flags;

  if((unsigned int)acc != flags)
    debug_err("Warning: possible truncation in bc_new_method.\n");

  meth = (JVM_METHOD *)malloc(sizeof(JVM_METHOD));

  if(!meth) return NULL;

  meth->access_flags = acc;

  meth->class = cclass;

  meth->gen_bytecode = TRUE;

  /* if this is a static method, then local variables are numbered 
   * starting at 0, otherwise they start at 1.
   */

  if(acc & JVM_ACC_STATIC)
    lv_start = 0;
  else
    lv_start = 1;

  debug_msg("access flags = %d\n", flags);

  meth->name = strdup(name);

  if(!meth->name) {
    err_new_meth();
    return NULL;
  }

  c = cp_find_or_insert(cclass, CONSTANT_Utf8, name);

  if(c < 0) {
    err_new_meth();
    return NULL;
  }

  meth->name_index = c;

  if(desc) {
    c = cp_find_or_insert(cclass, CONSTANT_Utf8, desc);

    if(c < 0) {
      err_new_meth();
      return NULL;
    }

    meth->descriptor_index = c;

    /* if there was a descriptor specified, then go ahead and
     * set the current local and maximum variable numbers.
     */

    meth->cur_local_number = lv_start + num_locals_in_descriptor(desc);
    meth->max_locals = meth->cur_local_number;
  }
  else {
    /* no descriptor specified yet.  we will rely on the user to set
     * it later.  for now set the index to 0 which should cause a
     * verification error in case the user forgets to set a proper
     * descriptor index.
     */

    meth->descriptor_index = 0;
    meth->cur_local_number = 1; 
    meth->max_locals = 1;
  }

  meth->attributes = make_dl();
  meth->attributes_count = 0;

  meth->cur_code = new_code_attr(cclass);

  meth->line_table = make_dl();
  meth->locals_table = make_dl();
  meth->label_list = make_dl();
  meth->exc_table = make_dl();

  if(!meth->attributes || !meth->line_table || !meth->locals_table || 
     !meth->label_list || !meth->exc_table) 
  {
    if(meth->attributes) dl_delete_list(meth->attributes);
    if(meth->line_table) dl_delete_list(meth->line_table);
    if(meth->locals_table) dl_delete_list(meth->locals_table);
    if(meth->label_list) dl_delete_list(meth->label_list);
    if(meth->exc_table) dl_delete_list(meth->exc_table);

    if(meth->cur_code)
      bc_free_code_attribute(cclass, meth->cur_code);

    meth->attributes = NULL;
    meth->line_table = NULL;
    meth->locals_table = NULL;
    meth->label_list = NULL;
    meth->exc_table = NULL;
    meth->cur_code = NULL;

    err_new_meth();
    return NULL;
  }

  meth->lastOp = jvm_nop;

  meth->stacksize = meth->pc = meth->num_handlers = 0;

  cclass->methods_count++;
  dl_insert_b(cclass->methods, meth);

  return meth;
}

/**
 * Removes the specified method from its containing class.
 *
 * @param meth -- The method to be removed.
 *
 * @returns 0 on success, -1 on failure.
 */

int
bc_remove_method(JVM_METHOD *meth)
{
  JVM_METHOD *tmpmeth;
  Dlist tmpPtr;

  if(!meth) {
    BAD_ARG();
    return -1;
  }

  dl_traverse(tmpPtr,meth->class->methods) {
    tmpmeth = (JVM_METHOD *) tmpPtr->val;

    if(tmpmeth == meth) {
      meth->class->methods_count--;
      dl_delete_node(tmpPtr);
      return 0;
    }
  }

  return -1;
}

/**
 * Gets the number of bytes of code in this method.
 *
 * @param meth -- The method whose length should be returned.
 *
 * @returns The code length (in bytes).  Returns -1 on failure.
 */

int
bc_get_code_length(JVM_METHOD *meth)
{
  if(!meth) {
    BAD_ARG();
    return -1;
  }

  return meth->pc;
}

/**
 * Gets the instruction following this instruction.
 *
 * @param node -- Pointer to an instruction node.
 *
 * @returns The next instruction.
 */

JVM_CODE_GRAPH_NODE *
bc_get_next_instr(JVM_CODE_GRAPH_NODE *node)
{
  if(!node) {
    BAD_ARG();
    return NULL;
  }

  return node->next;
}

/**
 * Sets the stack depth at the given instruction.  Most of the
 * time it won't be necessary to use this call, however there
 * may be some exceptional circumstances that require manually
 * setting the stack depth.
 *
 * @param node -- The instruction node for which the stack depth
 *   should be set.
 * @param depth -- The depth in number of stack entries.
 *
 * @returns 0 on success, -1 on failure.
 */

int
bc_set_stack_depth(JVM_CODE_GRAPH_NODE *node, int depth)
{
  if(!node) {
    BAD_ARG();
    return -1;
  }

  node->stack_depth = depth;
  return 0;
}

/**
 * Gets the last opcode in the given method.
 *
 * @param meth -- Pointer to a method structure.
 *
 * @returns The last opcode (see the JVM_OPCODE enum).  Returns -1 on failure.
 */

JVM_OPCODE 
bc_get_last_opcode(JVM_METHOD *meth)
{
  if(!meth) {
    BAD_ARG();
    return -1;
  }

  return meth->lastOp;
}

/**
 * Sets the method descriptor index in the specified method.  This would be
 * useful in situations where you don't know the descriptor when the method
 * is first created.
 *
 * @param meth -- The method whose descriptor should be set.
 * @param desc -- The method descriptor.
 *
 * @returns 0 on success, -1 on failure.
 */

int
bc_set_method_descriptor(JVM_METHOD *meth, char *desc)
{
  int c;

  if(!meth) {
    BAD_ARG();
    return -1;
  }

  if(desc) {
    c = cp_find_or_insert(meth->class, CONSTANT_Utf8, desc);

    if(c < 0)
      return -1;

    meth->descriptor_index = c;
  }
   
  return 0;
}

/**
 * Creates a new local variable table attribute.
 *
 * @param meth -- The method which will contain the local variable table.
 *
 * @returns Pointer to the new local variable table attribute.
 *    Returns NULL on error.
 */

JVM_ATTRIBUTE *
bc_new_local_variable_table_attr(JVM_METHOD *meth)
{
  JVM_ATTRIBUTE * tmp;
  int c;
  Dlist list_tmp, entries, const_table;

  if(!meth) {
    BAD_ARG();
    return NULL;
  }

  const_table = meth->class->constant_pool;
  entries = meth->locals_table;

  tmp = (JVM_ATTRIBUTE *)malloc(sizeof(JVM_ATTRIBUTE));

  if(!tmp) return NULL;

  c = cp_find_or_insert(meth->class, CONSTANT_Utf8, "LocalVariableTable");

  if(c < 0) {
    free(tmp);
    return NULL;
  }

  tmp->attribute_name_index = c;

  tmp->attribute_length = 0;
  tmp->attr.LocalVariableTable = (struct LocalVariableTable_attribute *)
         malloc(sizeof(struct LocalVariableTable_attribute));

  if(!tmp->attr.LocalVariableTable) {
    free(tmp);
    return NULL;
  }

  tmp->attr.LocalVariableTable->local_variable_table_length = 0;

  dl_traverse(list_tmp, entries) {
    JVM_LOCAL_VARIABLE_TABLE_ENTRY *entry;

    entry = (JVM_LOCAL_VARIABLE_TABLE_ENTRY *)list_tmp->val;

    c = cp_find_or_insert(meth->class, CONSTANT_Utf8, entry->name);

    if(c < 0) {
      free(tmp);
      return NULL;
    }

    entry->name_index = c;

    c = cp_find_or_insert(meth->class, CONSTANT_Utf8, entry->descriptor);

    if(c < 0) {
      free(tmp);
      return NULL;
    }

    entry->descriptor_index = c;

    if(!entry->end)
      entry->end = dl_last(meth->cur_code->attr.Code->code)->val;

    tmp->attr.LocalVariableTable->local_variable_table_length++;
  }
 
  /* each local var table entry is 10 bytes, plus 2 bytes for the length */
  tmp->attribute_length = 
    (tmp->attr.LocalVariableTable->local_variable_table_length * 10) + 2;

  tmp->attr.LocalVariableTable->local_variable_table = entries;

  return tmp;
}

/**
 * Creates a new line number table attribute.
 *
 * @param meth -- The method which will contain the line number table.
 *
 * @returns Pointer to the new line number table attribute.
 *    Returns NULL on error.
 */

JVM_ATTRIBUTE *
bc_new_line_number_table_attr(JVM_METHOD *meth)
{
  JVM_ATTRIBUTE * tmp;
  int c;
  Dlist list_tmp, entries;

  if(!meth) {
    BAD_ARG();
    return NULL;
  }

  entries = meth->line_table;

  tmp = (JVM_ATTRIBUTE *)malloc(sizeof(JVM_ATTRIBUTE));

  if(!tmp) return NULL;

  c = cp_find_or_insert(meth->class, CONSTANT_Utf8, "LineNumberTable");

  if(c < 0) {
    free(tmp);
    return NULL;
  }

  tmp->attribute_name_index = c;

  tmp->attribute_length = 0;
  tmp->attr.LineNumberTable = (struct LineNumberTable_attribute *)
         malloc(sizeof(struct LineNumberTable_attribute));

  if(!tmp->attr.LineNumberTable) {
    free(tmp);
    return NULL;
  }

  tmp->attr.LineNumberTable->line_number_table_length = 0;

  dl_traverse(list_tmp, entries) {
    tmp->attr.LineNumberTable->line_number_table_length++;
  }
 
  /* each line number table entry is 4 bytes, plus 2 bytes for the length */
  tmp->attribute_length = 
    (tmp->attr.LineNumberTable->line_number_table_length * 4) + 2;

  tmp->attr.LineNumberTable->line_number_table = entries;

  return tmp;
}

/**
 * Creates a new attribute structure and initializes the
 * Exception_attribute section with some initial values.
 *
 * @param cclass -- The class which will contain the attribute.
 *
 * @returns Pointer to the new exceptions attribute.
 *    Returns NULL on error.
 */

JVM_ATTRIBUTE *
bc_new_exceptions_attr(JVM_CLASS *cclass)
{
  JVM_ATTRIBUTE * tmp;
  int c;

  if(!cclass) {
    BAD_ARG();
    return NULL;
  }

  tmp = (JVM_ATTRIBUTE *)malloc(sizeof(JVM_ATTRIBUTE));

  if(!tmp) return NULL;

  c = cp_find_or_insert(cclass, CONSTANT_Utf8, "Exceptions");

  if(c < 0) {
    free(tmp);
    return NULL;
  }

  tmp->attribute_name_index = c;

  tmp->attr.Exceptions = (struct Exceptions_attribute *)
                 malloc(sizeof(struct Exceptions_attribute));

  if(!tmp->attr.Exceptions) {
    free(tmp);
    return NULL;
  }

  /* initially the attribute length is 2 which covers the size of the
   * 2-byte length field.
   */

  tmp->attribute_length = 2;
  tmp->attr.Exceptions->number_of_exceptions = (u2) 0;
  tmp->attr.Exceptions->exception_index_table = make_dl();

  if(!tmp->attr.Exceptions->exception_index_table) {
    free(tmp->attr.Exceptions);
    free(tmp);
    return NULL;
  }

  return tmp;
}

/**
 * Creates a new InnerClasses attribute structure.
 *
 * @param cclass -- The class which will contain the attribute.
 *
 * @returns Pointer to the new InnerClasses attribute.
 *    Returns NULL on error.
 */

JVM_ATTRIBUTE *
bc_new_inner_classes_attr(JVM_CLASS *cclass)
{
  JVM_ATTRIBUTE * tmp;
  int c;

  if(!cclass) {
    BAD_ARG();
    return NULL;
  }

  tmp = (JVM_ATTRIBUTE *)malloc(sizeof(JVM_ATTRIBUTE));

  if(!tmp) return NULL;

  c = cp_find_or_insert(cclass, CONSTANT_Utf8, "InnerClasses");

  if(c < 0) {
    free(tmp);
    return NULL;
  }

  tmp->attribute_name_index = c;

  tmp->attr.InnerClasses = (struct InnerClasses_attribute *)
                 malloc(sizeof(struct InnerClasses_attribute));

  if(!tmp->attr.InnerClasses) {
    free(tmp);
    return NULL;
  }

  /* initially the attribute length is 2 which covers the size of the
   * 2-byte length field.
   */

  tmp->attribute_length = 2;
  tmp->attr.InnerClasses->number_of_classes = (u2) 0;
  tmp->attr.InnerClasses->classes = make_dl();

  if(!tmp->attr.InnerClasses->classes) {
    free(tmp->attr.InnerClasses);
    free(tmp);
    return NULL;
  }

  return tmp;
}

/**
 * This function 'releases' a local variable.  That is, calling this
 * function signifies that we no longer need this local variable.
 *
 * @param meth -- The method containing the local variable.
 * @param vtype -- The JVM data type of the variable (see the JVM_DATA_TYPE
 *   enum).
 *
 * @returns The current local variable number.  Returns -1 on error.
 */

int
bc_release_local(JVM_METHOD *meth, JVM_DATA_TYPE vtype)
{
  if(!meth) {
    BAD_ARG();
    return -1;
  }

  if((vtype == jvm_Double) || (vtype == jvm_Long))
    meth->cur_local_number-=2;
  else
    meth->cur_local_number--;

  return meth->cur_local_number;
}

/**
 * This function returns the next available local variable number and
 * updates the max if necessary.
 *
 * @param meth -- The method containing the local variable.
 * @param vtype -- The JVM data type of the variable (see the JVM_DATA_TYPE
 *   enum).
 *
 * @returns The next local variable number.  Returns -1 on error.
 */

int
bc_get_next_local(JVM_METHOD *meth, JVM_DATA_TYPE vtype)
{
  if(!meth) {
    BAD_ARG();
    return -1;
  }

  if((vtype == jvm_Double) || (vtype == jvm_Long))
    meth->cur_local_number+=2;
  else
    meth->cur_local_number++;

  if(meth->cur_local_number > meth->max_locals)
    meth->max_locals = meth->cur_local_number;

  return meth->cur_local_number - 
      (((vtype == jvm_Double) || (vtype == jvm_Long)) ? 2 : 1);
}

/**
 * Sets the current local variable number for this method.  If the new value
 * is greater than the current maximum number of locals, then the max_locals
 * field is set also.
 *
 * @param meth -- The method whose local variable number should be set.
 * @param curlocal -- The current local variable number.
 *
 * @returns 0 on success, -1 on failure.
 */

int
bc_set_cur_local_num(JVM_METHOD *meth, unsigned int curlocal) {
  if(!meth) {
    BAD_ARG();
    return -1;
  }

  meth->cur_local_number = curlocal;

  if(curlocal > meth->max_locals)
    meth->max_locals = curlocal;

  return 0;
}

/**
 * Allow suspending the generation of bytecode for situations in which the
 * code generation ordering is very different between java source and JVM
 * bytecode.
 * 
 * @param meth -- The method to suspend/enable bytecode generation.
 * @param value -- If TRUE, calls which generate code (e.g. bc_append())
 *   will actually add the instructions to the code graph.  If FALSE, you
 *   can still call these routines, but they will have no effect.
 *
 * @returns 0 on success, -1 on failure.
 */

int 
bc_set_gen_status(JVM_METHOD *meth, BOOL value) {
  if(!meth) {
    BAD_ARG();
    return -1;
  }

  meth->gen_bytecode = value;
  return 0;
}

/**
 * Creates the bytecode for a new default constructor and adds it to the
 * given class.
 *
 * @param cur_class -- The class for which the default constructor should
 *    be created.
 * @param acc_flag -- The access flags for the constructor (for example
 *     JVM_ACC_PUBLIC, JVM_ACC_STATIC, etc)
 *
 * @returns Pointer to the new constructor (a JVM_METHOD structure).
 *    Returns NULL on error.
 */

JVM_METHOD *
bc_add_default_constructor(JVM_CLASS *cur_class, u2 acc_flag)
{
  JVM_METHOD *meth_tmp;
  char *cur_sc;
  CP_NODE *c;
  int idx;

  if(!cur_class) {
    BAD_ARG();
    return NULL;
  }

  c = cp_entry_by_index(cur_class, cur_class->super_class);

  if(!c) return NULL;

  c = cp_entry_by_index(cur_class, c->val->cpnode.Class.name_index);

  if(!c) return NULL;

  cur_sc = cp_null_term_utf8(c->val);

  if(!cur_sc) return NULL;

  meth_tmp = bc_new_method(cur_class, "<init>", "()V", acc_flag);
 
  if(!meth_tmp) {
    free(cur_sc);
    return NULL;
  }

  idx = bc_new_methodref(cur_class, cur_sc, "<init>", "()V");

  if(idx < 0) {
    free(cur_sc);
    return NULL;
  }

  bytecode0(meth_tmp, jvm_aload_0);
  bytecode1(meth_tmp, jvm_invokespecial, idx);
  bytecode0(meth_tmp, jvm_return);

  bc_set_cur_local_num(meth_tmp, 1);

  free(cur_sc);

  return meth_tmp;
}

/**
 * Creates bytecode for a new multi dimensional array.
 *
 * @param meth -- The method to which this instruction should be added.
 * @param dimensions -- The number of dimensions to be created.
 * @param desc -- The descriptor of the array.
 *
 * @returns Pointer to the instruction node.
 *    Returns NULL on error.
 */

JVM_CODE_GRAPH_NODE *
bc_new_multi_array(JVM_METHOD *meth, u4 dimensions, char *desc)
{
  u4 operand;
  int c;

  if(!meth || !desc) {
    BAD_ARG();
    return NULL;
  }

  c = cp_find_or_insert(meth->class, CONSTANT_Class, desc);

  if(c < 0) return NULL;

  operand = (c<<8) | dimensions;
  return bytecode1(meth, jvm_multianewarray, operand);
}

/**
 * Generates an instruction to load the specified field onto the
 * stack (jvm_getfield).
 *
 * @param meth -- The method to which this instruction should be added.
 * @param class -- The name of the class containing the field.
 * @param field -- The field name.
 * @param desc -- The field descriptor.
 *
 * @returns Pointer to the instruction node.
 *    Returns NULL on error.
 */

JVM_CODE_GRAPH_NODE *
bc_get_field(JVM_METHOD *meth, char *class, char *field, char *desc)
{
  int field_idx;

  if(!meth || !class || !field || !desc) {
    BAD_ARG();
    return NULL;
  }

  field_idx = bc_new_fieldref(meth->class, class, field, desc);

  if(field_idx < 0) return NULL;

  return bytecode1(meth, jvm_getfield, field_idx);
}

/**
 * Generates an instruction to store the top stack value to the
 * specified field (jvm_putfield).
 *
 * @param meth -- The method to which this instruction should be added.
 * @param class -- The name of the class containing the field.
 * @param field -- The field name.
 * @param desc -- The field descriptor.
 *
 * @returns Pointer to the instruction node.
 *    Returns NULL on error.
 */

JVM_CODE_GRAPH_NODE *
bc_put_field(JVM_METHOD *meth, char *class, char *field, char *desc)
{
  int field_idx;

  if(!meth || !class || !field || !desc) {
    BAD_ARG();
    return NULL;
  }

  field_idx = bc_new_fieldref(meth->class, class, field, desc);

  if(field_idx < 0) return NULL;

  return bytecode1(meth, jvm_putfield, field_idx);
}

/**
 * Generates an instruction to load the specified static field onto the
 * stack (jvm_getstatic).
 *
 * @param meth -- The method to which this instruction should be added.
 * @param class -- The name of the class containing the field.
 * @param field -- The field name.
 * @param desc -- The field descriptor.
 *
 * @returns Pointer to the instruction node.
 *    Returns NULL on error.
 */

JVM_CODE_GRAPH_NODE *
bc_get_static(JVM_METHOD *meth, char *class, char *field, char *desc)
{
  int field_idx;

  if(!meth || !class || !field || !desc) {
    BAD_ARG();
    return NULL;
  }

  field_idx = bc_new_fieldref(meth->class, class, field, desc);

  if(field_idx < 0) return NULL;

  return bytecode1(meth, jvm_getstatic, field_idx);
}

/**
 * Generates an instruction to store the top stack value to the
 * specified static field (jvm_putstatic).
 *
 * @param meth -- The method to which this instruction should be added.
 * @param class -- The name of the class containing the field.
 * @param field -- The field name.
 * @param desc -- The field descriptor. 
 *
 * @returns Pointer to the instruction node.
 *    Returns NULL on error.
 */

JVM_CODE_GRAPH_NODE *
bc_put_static(JVM_METHOD *meth, char *class, char *field, char *desc)
{
  int field_idx;

  if(!meth || !class || !field || !desc) {
    BAD_ARG();
    return NULL;
  }

  field_idx = bc_new_fieldref(meth->class, class, field, desc);

  if(field_idx < 0) return NULL;

  return bytecode1(meth, jvm_putstatic, field_idx);
}

/**
 * Generates an "instanceof" instruction which determines whether the
 * operand on top of the stack is an instance of the specified class.
 *
 * @param meth -- The method to which this instruction should be added.
 * @param class -- The name of the class which the object might be an
 *    instance of.
 *
 * @returns Pointer to the instruction node.
 *    Returns NULL on error.
 */

JVM_CODE_GRAPH_NODE *
bc_gen_instanceof(JVM_METHOD *meth, char *class)
{
  int c;

  if(!meth || !class) {
    BAD_ARG();
    return NULL;
  }

  c = cp_find_or_insert(meth->class, CONSTANT_Class, class);

  if(c < 0) return NULL;

  return bytecode1(meth, jvm_instanceof, c);
}

/**
 * Generates a "checkcast" instruction which determines whether the
 * operand on top of the stack is of the specified type.
 *
 * @param meth -- The method to which this instruction should be added.
 * @param class -- The name of the class which might be the object's type.
 *
 * @returns Pointer to the instruction node.
 *    Returns NULL on error.
 */

JVM_CODE_GRAPH_NODE *
bc_gen_checkcast(JVM_METHOD *meth, char *class)
{
  int c;

  if(!meth || !class) {
    BAD_ARG();
    return NULL;
  }

  c = cp_find_or_insert(meth->class, CONSTANT_Class, class);

  if(c < 0) return NULL;

  return bytecode1(meth, jvm_checkcast, c);
}

/**
 * Generates a switch instruction.  This will either be a "tableswitch" or
 * a "lookupswitch" depending on how many empty cases there are after all
 * cases have been specified.  When most of the cases are specified, then
 * the "tableswitch" instruction is used, but if the switch is more sparsely
 * filled with cases, the "lookupswitch" would use less space.  The value 
 * defined for JVM_SWITCH_FILL_THRESH in bytecode.h determines how many empty
 * cases there must be before the "lookupswitch" is used.
 *
 * @param meth -- The method to which this instruction should be added.
 *
 * @returns Pointer to the instruction node.
 *    Returns NULL on error.
 */

JVM_CODE_GRAPH_NODE *
bc_gen_switch(JVM_METHOD *meth) {
  JVM_CODE_GRAPH_NODE *instr;

  if(!meth) {
    BAD_ARG();
    return NULL;
  }

  instr = bytecode0(meth, jvm_tableswitch);

  instr->switch_info = 
     (JVM_SWITCH_INFO *)malloc(sizeof(JVM_SWITCH_INFO));

  if(!instr->switch_info) return NULL;

  /* we will calculate the cell padding, and low/high case numbers later */

  instr->switch_info->cell_padding = 0;
  instr->switch_info->low = 0;
  instr->switch_info->high = 0;

  instr->switch_info->offsets = make_dl();
  instr->switch_info->num_entries = 0;

  if(!instr->switch_info->offsets) {
    free(instr->switch_info);
    return NULL;
  }
 
  /* the width is unknown at this time, but it doesn't matter because 
   * the real width will be calculated later.
   */
  instr->width = bc_op_width(jvm_tableswitch);

  return instr;
}

/**
 * Adds another case to the given switch instruction.
 * 
 * @param instr -- The node of the switch instruction.
 * @param target -- The node of the first instruction in the case to be added.
 * @param case_num -- The integer corresponding to this case.
 *
 * @returns 0 on success, -1 on failure.
 */

int
bc_add_switch_case(JVM_CODE_GRAPH_NODE *instr, JVM_CODE_GRAPH_NODE *target,
  int case_num)
{
  JVM_SWITCH_ENTRY *newcase;

  if(!instr || !target) {
    BAD_ARG();
    return -1;
  }

  if(dl_empty(instr->switch_info->offsets)) {
    instr->switch_info->low = case_num;
    instr->switch_info->high = case_num;
  }
  else {
    if(case_num < instr->switch_info->low)
      instr->switch_info->low = case_num;

    if(case_num > instr->switch_info->high)
      instr->switch_info->high = case_num;
  }

  newcase = (JVM_SWITCH_ENTRY *)malloc(sizeof(JVM_SWITCH_ENTRY));

  if(!newcase) return -1;

  newcase->instr = target;
  newcase->case_num = case_num;

  dl_insert_b(instr->switch_info->offsets, newcase);

  instr->switch_info->num_entries++;

  return 0;
}

/**
 * Specifies the default case for the given switch instruction.
 *
 * @param instr -- The node of the switch instruction.
 * @param target -- The node of the first instruction in the default 
 *    case to be added.
 *
 * @returns 0 on success, -1 on failure.
 */

int
bc_add_switch_default(JVM_CODE_GRAPH_NODE *instr, JVM_CODE_GRAPH_NODE *target)
{
  if(!instr || !target) {
    BAD_ARG();
    return -1;
  }

  instr->switch_info->default_case = target;

  return 0;
}

/**
 * Sets the branch target of a JVM_CODE_GRAPH_NODE (that is, which instruction
 * this instruction branches to, either conditionally or unconditionally).
 *
 * @param node -- The node of the conditional or unconditional branching
 *    instruction.
 * @param target -- The target of the branch instruction.
 *
 * @returns 0 on success, -1 on failure.
 */

int
bc_set_branch_target(JVM_CODE_GRAPH_NODE *node, JVM_CODE_GRAPH_NODE *target)
{
  if(!node || !target) {
    BAD_ARG();
    return -1;
  }

  node->branch_target = target;

  return 0;
}

/**
 * Sets the label to which this instruction branches.  This is used
 * when implementing languages which can branch forward to labeled
 * statements.  Thus the forward instruction does not need to have been
 * emitted when the branch target is set.  Later the address will be
 * resolved.
 *
 * @param node -- The node of the branch instruction.
 * @param label -- The label to which the instruction branches.
 *
 * @returns 0 on success, -1 on failure.
 */

int
bc_set_branch_label(JVM_CODE_GRAPH_NODE *node, const char *label)
{
  if(!node || !label) {
    BAD_ARG();
    return -1;
  }

  node->branch_label = strdup(label);

  if(!node->branch_label) return -1;

  return 0;
}

/**
 * Same as bc_set_branch_label() except that the label is specified
 * as an integer instead of a string.
 *
 * @param node -- The node of the branch instruction.
 * @param label -- The label to which the instruction branches.
 *
 * @returns 0 on success, -1 on failure.
 */

int
bc_set_integer_branch_label(JVM_CODE_GRAPH_NODE *node, int label_num)
{
  char label[20];

  if(!node) {
    BAD_ARG();
    return -1;
  }

  sprintf(label, "%d", label_num);

  return bc_set_branch_label(node, label);
}


/**
 * Generates an iinc instruction.  First check if the iinc needs to be
 * preceeded by a jvm_wide opcode and generate that if necessary.  The wide
 * instruction is required if the local variable index or the immediate
 * operand would exceed a one-byte value.
 *
 * @param meth -- The method to which this instruction should be added.
 * @param idx -- The index of the local variable to be incremented.
 * @param inc_const -- The constant value to add to the specified local variable.
 *
 * @returns Pointer to the instruction node.
 *    Returns NULL on error.
 */

JVM_CODE_GRAPH_NODE *
bc_gen_iinc(JVM_METHOD *meth, unsigned int idx, int inc_const)
{
  unsigned int operand;

  if(!meth) {
    BAD_ARG();
    return NULL;
  }

  if((idx > 255) || (inc_const < -128) || (inc_const > 127)) {
    bytecode0(meth, jvm_wide);
    operand = ((idx & 0xFFFF) << 16) | ((u2)inc_const & 0xFFFF);
  }
  else
    operand = ((idx & 0xFF) << 8) | (inc_const & 0xFF);

  return bytecode1(meth, jvm_iinc, operand);
}

/**
 * This function returns a pointer to the next field type in this descriptor.
 *
 * @param str -- The descriptor to be parsed.
 *
 * @returns Pointer to the beginning of the next field in the descriptor.
 *    If there are no more field types this function returns NULL.  On error, 
 *    this function also returns NULL.
 */

char *
bc_next_desc_token(char *str)
{
  char *p = str;

  if(!str) {
    BAD_ARG();
    return NULL;
  }

  switch(*p) {
    case 'B': case 'C': case 'D': case 'F':
    case 'I': case 'J': case 'S': case 'Z':
      return p+1;

    case 'L':
      while((*p != ';') && (*p != '\0'))
        p++;

      if(*p == '\0') {
        debug_err("bc_next_desc_token() incomplete classname in desc\n");
        return NULL;
      }

      return p+1;

    case '[':
      return bc_next_desc_token(p+1);

    case '(':
      /* we should hit this case at the beginning of the descriptor */
      return p+1;

    case ')':
      return NULL;

    default:
      debug_err("bc_next_desc_token() unrecognized char in desc:%s\n",str);
      return NULL;
  }

  /* should never reach here */
}

/**
 * Generates a return instruction.  This can be used in a generic way
 * and when the class is emitted, the proper type-specific return
 * instruction is generated based on the method descriptor.
 *
 * @param meth -- The method to which this instruction should be added.
 *
 * @returns Pointer to the instruction node.
 *    Returns NULL on error.
 */

JVM_CODE_GRAPH_NODE *
bc_gen_return(JVM_METHOD *meth)
{
  if(!meth) {
    BAD_ARG();
    return NULL;
  }

  return bytecode0(meth, jvm_return);
}

/**
 * Pushes an integer constant onto the stack.  The exact instruction
 * generated depends on the value of the constant (sipush, bipush, etc).
 *
 * @param meth -- The method to which this instruction should be added.
 * @param ival -- The integer constant to be loaded.
 *
 * @returns Pointer to the instruction node.
 *    Returns NULL on error.
 */

JVM_CODE_GRAPH_NODE *
bc_push_int_const(JVM_METHOD *meth, int ival)
{
  JVM_CODE_GRAPH_NODE *node = NULL;
  int ct;

  if(!meth) {
    BAD_ARG();
    return NULL;
  }

  ct = cp_find_or_insert(meth->class, CONSTANT_Integer, (void*)&ival);

  if(ct >= 0) {
    if(ct > CP_IDX_MAX)
      node = bytecode1(meth, jvm_ldc_w,ct);
    else
      node = bytecode1(meth, jvm_ldc,ct);
  } else {   /* not found, use literal */
    if((ival < JVM_SHORT_MIN) || (ival > JVM_SHORT_MAX)) {
      debug_err("WARNING:expr_emit() bad int literal: %d\n", ival);
      return NULL;
    }
    else if((ival < JVM_BYTE_MIN) || (ival > JVM_BYTE_MAX))
      node = bytecode1(meth, jvm_sipush, ival);
    else if((ival < JVM_ICONST_MIN) || (ival > JVM_ICONST_MAX))
      node = bytecode1(meth, jvm_bipush, ival);
    else
      node = bytecode0(meth, jvm_iconst_op[ival+1]);
  }

  return node;
}

/**
 * Pushes a null object value onto the stack.
 *
 * @param meth -- The method to which this instruction should be added.
 *
 * @returns Pointer to the instruction node.
 *    Returns NULL on error.
 */

JVM_CODE_GRAPH_NODE *
bc_push_null_const(JVM_METHOD *meth)
{
  if(!meth) {
    BAD_ARG();
    return NULL;
  }

  return bytecode0(meth, jvm_aconst_null);
}

/**
 * Pushes a float constant onto the stack.
 *
 * @param meth -- The method to which this instruction should be added.
 * @param fval -- The floating point value to be loaded.
 *
 * @returns Pointer to the instruction node.
 *    Returns NULL on error.
 */

JVM_CODE_GRAPH_NODE *
bc_push_float_const(JVM_METHOD *meth, float fval)
{
  JVM_CODE_GRAPH_NODE *node = NULL;
  int ct;

  if(!meth) {
    BAD_ARG();
    return NULL;
  }

  ct = cp_find_or_insert(meth->class, CONSTANT_Float, (void*)&fval);

  if(ct >= 0) {
    if(ct > CP_IDX_MAX)
      node = bytecode1(meth, jvm_ldc_w,ct);
    else
      node = bytecode1(meth, jvm_ldc,ct);
  }
  else if(fval == 0.0)
    node = bytecode0(meth, jvm_fconst_0);
  else if(fval == 1.0)
    node = bytecode0(meth, jvm_fconst_1);
  else if(fval == 2.0)
    node = bytecode0(meth, jvm_fconst_2);
  else
    debug_err("bc_push_float_const(): bad float precision literal\n");

  return node;
}

/**
 * Pushes a double constant onto the stack.
 *
 * @param meth -- The method to which this instruction should be added.
 * @param dval -- The double precision floating point value to be loaded.
 *
 * @returns Pointer to the instruction node.
 *    Returns NULL on error.
 */

JVM_CODE_GRAPH_NODE *
bc_push_double_const(JVM_METHOD *meth, double dval)
{
  JVM_CODE_GRAPH_NODE *node = NULL;
  int ct;

  if(!meth) {
    BAD_ARG();
    return NULL;
  }

  ct = cp_find_or_insert(meth->class, CONSTANT_Double, (void*)&dval);

  if(ct >= 0)
    node = bytecode1(meth, jvm_ldc2_w, ct);
  else if(dval == 0.0)
    node = bytecode0(meth, jvm_dconst_0);
  else if(dval == 1.0)
    node = bytecode0(meth, jvm_dconst_1);
  else
    debug_err("bc_push_double_const(): bad double precision literal\n");

  return node;
}

/**
 * Pushes a long constant onto the stack.
 *
 * @param meth -- The method to which this instruction should be added.
 * @param lval -- The long constant to be loaded.
 *
 * @returns Pointer to the instruction node.
 *    Returns NULL on error.
 */

JVM_CODE_GRAPH_NODE *
bc_push_long_const(JVM_METHOD *meth, long long lval)
{
  JVM_CODE_GRAPH_NODE *node = NULL;
  int ct;

  if(!meth) {
    BAD_ARG();
    return NULL;
  }

  ct = cp_find_or_insert(meth->class, CONSTANT_Long, (void*)&lval);

  if(ct >= 0)
    node = bytecode1(meth, jvm_ldc2_w, ct);
  else if(lval == 0)
    node = bytecode0(meth, jvm_lconst_0);
  else if(lval == 1)
    node = bytecode0(meth, jvm_lconst_1);
  else
    debug_err("bc_push_long_const(): bad literal\n");

  return node;
}

/**
 * Pushes a string constant onto the stack.
 *
 * @param meth -- The method to which this instruction should be added.
 * @param str -- The string value to be loaded.
 *
 * @returns Pointer to the instruction node.
 *    Returns NULL on error.
 */

JVM_CODE_GRAPH_NODE *
bc_push_string_const(JVM_METHOD *meth, char *str)
{
  JVM_CODE_GRAPH_NODE *node = NULL;
  int ct;

  if(!meth || !str) {
    BAD_ARG();
    return NULL;
  }

  ct = cp_find_or_insert(meth->class, CONSTANT_String, (void*)str);

  if(ct < 0) return NULL;

  if(ct > CP_IDX_MAX)
    node = bytecode1(meth, jvm_ldc_w, ct);
  else
    node = bytecode1(meth, jvm_ldc, ct);

  return node;
}

/**
 * This function searches the list of nodes for the given PC.  Returns the
 * node if found, otherwise NULL.  This is not very efficient - we should
 * probably modify it eventually if it becomes an issue.
 *
 * @param meth -- The method to which this instruction should be added.
 * @param num -- The address of the node to find.
 *
 * @returns Pointer to the instruction node with the specified address.
 *    Returns NULL on error.
 */

JVM_CODE_GRAPH_NODE *
bc_node_at_pc(JVM_METHOD *meth, int num)
{
  JVM_CODE_GRAPH_NODE *nodeptr;
  Dlist tmp;

  if(!meth) {
    BAD_ARG();
    return NULL;
  }

  dl_traverse(tmp, meth->cur_code->attr.Code->code) {
    nodeptr = (JVM_CODE_GRAPH_NODE *)tmp->val;
    if(nodeptr->pc == (unsigned int)num)
      return nodeptr;
    if(nodeptr->pc > (unsigned int)num)
      return NULL;
  }

  return NULL;
}

/**
 * Get the width of the specified op.
 *
 * @param op -- The op to return the length of.
 *
 * @returns The width in bytes of this op, including operands.
 */

u1
bc_op_width(JVM_OPCODE op)
{
  return jvm_opcode[op].width;
}

/**
 * Given the local variable number, this function generates a store opcode
 * to store a value to the local var.
 *
 * @param meth -- The method to which this instruction should be added.
 * @param lvnum -- The local variable number to which the value should
 *   be stored.
 * @param rt -- The JVM data type of the local variable (see the enumeration
 *   JVM_DATA_TYPE).
 *
 * @returns Pointer to the instruction node.
 *    Returns NULL on error.
 */

JVM_CODE_GRAPH_NODE *
bc_gen_store_op(JVM_METHOD *meth, unsigned int lvnum, 
  JVM_DATA_TYPE rt)
{
  JVM_CODE_GRAPH_NODE *node;

  if(!meth) {
    BAD_ARG();
    return NULL;
  }

  if(lvnum > 255) {
    node = bytecode0(meth, jvm_wide);
    bytecode1(meth, jvm_store_op[rt], lvnum);
  }
  else if(lvnum <= 3)
    node = bytecode0(meth, jvm_short_store_op[rt][lvnum]);
  else
    node = bytecode1(meth, jvm_store_op[rt], lvnum);

  updateMaxLocals(meth, lvnum, rt);

  return node;
}

/**
 * Given the local variable number, this function generates a load opcode
 * to load a value from the local var.
 *
 * @param meth -- The method to which this instruction should be added.
 * @param lvnum -- The local variable from which the value should be loaded.
 * @param rt -- The JVM data type of the local variable (see the enumeration
 *   JVM_DATA_TYPE).
 *
 * @returns Pointer to the instruction node.
 *    Returns NULL on error.
 */

JVM_CODE_GRAPH_NODE *
bc_gen_load_op(JVM_METHOD *meth, unsigned int lvnum, 
  JVM_DATA_TYPE rt)
{
  JVM_CODE_GRAPH_NODE *node;

  if(!meth) {
    BAD_ARG();
    return NULL;
  }

  if(lvnum > 255) {
    node = bytecode0(meth, jvm_wide);
    bytecode1(meth, jvm_load_op[rt], lvnum);
  }
  else if(lvnum <= 3)
    node = bytecode0(meth, jvm_short_load_op[rt][lvnum]);
  else
    node = bytecode1(meth, jvm_load_op[rt], lvnum);

  updateMaxLocals(meth, lvnum, rt);

  return node;
}

/**
 * This function generates a load opcode to load a value from an array.
 *
 * @param meth -- The method to which this instruction should be added.
 * @param rt -- The JVM data type of the array (see the enumeration
 *   JVM_DATA_TYPE).
 *
 * @returns Pointer to the instruction node.
 *    Returns NULL on error.
 */

JVM_CODE_GRAPH_NODE *
bc_gen_array_load_op(JVM_METHOD *meth, JVM_DATA_TYPE rt)
{
  if(!meth) {
    BAD_ARG();
    return NULL;
  }

  return bytecode0(meth, jvm_array_load_op[rt]);
}

/**
 * This function generates a store opcode to store a value to an array.
 *
 * @param meth -- The method to which this instruction should be added.
 * @param rt -- The JVM data type of the array (see the enumeration
 *   JVM_DATA_TYPE).
 *
 * @returns Pointer to the instruction node.
 *    Returns NULL on error.
 */

JVM_CODE_GRAPH_NODE *
bc_gen_array_store_op(JVM_METHOD *meth, JVM_DATA_TYPE rt)
{
  if(!meth) {
    BAD_ARG();
    return NULL;
  }

  return bytecode0(meth, jvm_array_store_op[rt]);
}

/**
 * Generates an instruction to create a new object of the specified class.
 * Note: this does not completely create a new instance.  For that, you will
 * still need to call the constructor.
 *
 * @param meth -- The method to which this instruction should be added.
 * @param classname -- The name of the class to be created.
 *
 * @returns Pointer to the instruction node.
 *    Returns NULL on error.
 */

JVM_CODE_GRAPH_NODE *
bc_gen_new_obj(JVM_METHOD *meth, char *classname)
{
  int c;
  char *class;
  
  if(!meth || !classname) {
    BAD_ARG();
    return NULL;
  }

  class = char_substitute(classname, '.', '/');

  if(!class) return NULL;

  c = cp_find_or_insert(meth->class, CONSTANT_Class, class);

  free(class);

  if(c < 0) return NULL;

  return bc_append(meth, jvm_new, c);
}

/**
 * Generates two instructions.  The first creates a new object of the 
 * specified class.  The second instruction duplicates the new object.
 *
 * @param meth -- The method to which this instruction should be added.
 * @param classname -- The name of the class to be created.
 *
 * @returns Pointer to the instruction node (the first instruction).
 *    Returns NULL on error.
 */

JVM_CODE_GRAPH_NODE *
bc_gen_new_obj_dup(JVM_METHOD *meth, char *classname)
{
  JVM_CODE_GRAPH_NODE *newobj;

  if(!meth || !classname) {
    BAD_ARG();
    return NULL;
  }

  newobj = bc_gen_new_obj(meth, classname);

  if(!newobj) return NULL;

  bc_append(meth, jvm_dup);

  return newobj;
}

/**
 * Generates a sequence of instructions which completely creates a new
 * instance of the specified class which must have a constructor with no 
 * arguments.
 *
 * @param meth -- The method to which this instruction should be added.
 * @param classname -- The name of the class to be created.
 *
 * @returns Pointer to the first instruction node in the sequence (it will
 *    be the jvm_new instruction).  Returns NULL on error.
 */

JVM_CODE_GRAPH_NODE *
bc_gen_obj_instance_default(JVM_METHOD *meth, char *classname)
{
  JVM_CODE_GRAPH_NODE *newobj;
  int meth_idx;

  if(!meth || !classname) {
    BAD_ARG();
    return NULL;
  }

  newobj = bc_gen_new_obj_dup(meth, classname);

  if(!newobj) return NULL;

  meth_idx = bc_new_methodref(meth->class, classname, "<init>", "()V");

  if(meth_idx < 0) return NULL;

  bc_append(meth, jvm_invokespecial, meth_idx);

  return newobj;
}

/**
 * Generates the instructions to create a new array for any type except
 * objects (use bc_gen_new_object_array() for objects).
 *
 * This will generate an instruction to push the specified size onto the 
 * stack.  If you want to omit that instruction (if you're pushing the
 * size yourself before calling this function), then just specify -1 as
 * the size.
 *
 * @param meth -- The method to which this instruction should be added.
 * @param size -- The size of the array to be created (-1 to omit the
 *   instruction to push this value).
 * @param rt -- The JVM data type of the array (see the enumeration
 *   JVM_DATA_TYPE).
 *
 * @returns Pointer to the first instruction node emitted.  This will
 *    either be an integer load (if the size was specified) or the
 *    jvm_newarray instruction.  Returns NULL on error.
 */

JVM_CODE_GRAPH_NODE *
bc_gen_new_array(JVM_METHOD *meth, int size, JVM_DATA_TYPE rt)
{
  JVM_CODE_GRAPH_NODE *node, *first;

  if(!meth) {
    BAD_ARG();
    return NULL;
  }

  first = NULL;

  if(size >= 0)
    first = bc_push_int_const(meth, size);

  if(rt == jvm_Object)
    debug_err(
       "Warning: bc_gen_new_array() shouldn't be used for objects\n");

  node = bytecode1(meth, jvm_newarray, jvm_newarray_type[rt]);  

  if(first)
    return first;
  else
    return node;
}

/**
 * Generates the instructions to create a new object array.
 *
 * This will push the specified size onto the stack.  If you want to omit
 * that instruction (if you're pushing the size yourself before calling this
 * function), then just specify -1 as the size.
 *
 * @param meth -- The method to which this instruction should be added.
 * @param size -- The size of the array to be created (-1 to omit the
 *   instruction to push this value).
 * @param class -- The name of the class which represents the data type of
 *   the array elements.
 *
 * @returns Pointer to the first instruction node emitted.  This will
 *    either be an integer load (if the size was specified) or the
 *    jvm_newarray instruction.  Returns NULL on error.
 */

JVM_CODE_GRAPH_NODE *
bc_gen_new_object_array(JVM_METHOD *meth, int size, char *class)
{
  JVM_CODE_GRAPH_NODE *node, *first;
  int c;
  char *tmp;

  if(!meth || !class) {
    BAD_ARG();
    return NULL;
  }

  first = NULL;

  if(size >= 0)
    first = bc_push_int_const(meth, size);

  tmp = char_substitute(class, '.', '/');

  if(!tmp) return NULL;

  c = cp_find_or_insert(meth->class, CONSTANT_Class, tmp);

  free(tmp);

  if(c < 0) return NULL;

  node = bytecode1(meth, jvm_anewarray, c);  

  if(first)
    return first;
  else
    return node;
}

/**
 * This function creates a new method reference and inserts it into the
 * constant pool if necessary.  The return value is a pointer to the
 * constant pool node containing the method reference.
 *
 * @param class -- Class containing the constant pool where this
 *   method reference will be stored.
 * @param cname -- The name of the class.
 * @param mname -- The name of the method.
 * @param dnmae -- The method descriptor.
 *
 * @returns The constant pool index of the method reference.
 *    Returns -1 on failure.
 */

int
bc_new_methodref(JVM_CLASS *class, char *cname, char *mname, char *dname)
{
  JVM_METHODREF *methodref;
  int retval;

  if(!class || !cname || !mname || !dname) {
    BAD_ARG();
    return -1;
  }

  methodref = bc_new_method_node(cname,mname,dname);

  if(!methodref) return -1;

  retval = cp_find_or_insert(class, CONSTANT_Methodref, methodref);

  bc_free_fieldref(methodref);

  return retval;
}

/**
 * This function creates a new interface method reference and inserts it
 * into the constant pool if necessary.  The return value is a pointer to
 * the constant pool node containing the interface method reference.
 *
 * @param class -- Class containing the constant pool where this
 *   interface reference will be stored.
 * @param cname -- The name of the class.
 * @param mname -- The name of the method.
 * @param dnmae -- The method descriptor.
 *
 * @returns The constant pool index of the interface reference.
 *    Returns -1 on failure.
 */

int
bc_new_interface_methodref(JVM_CLASS *class, char *cname, char *mname, 
  char *dname)
{
  JVM_METHODREF *interfaceref;
  int retval;

  if(!class || !cname || !mname || !dname) {
    BAD_ARG();
    return -1;
  }

  interfaceref = bc_new_method_node(cname,mname,dname);

  if(!interfaceref) return -1;

  retval = cp_find_or_insert(class, CONSTANT_InterfaceMethodref, interfaceref);

  bc_free_interfaceref(interfaceref);

  return retval;
}

/**
 * This function creates a new method 'node' initialized with the given
 * values for class name, method name, and descriptor.
 *
 * @param cname -- The name of the class.
 * @param mname -- The name of the method.
 * @param dnmae -- The method descriptor.
 *
 * @returns Pointer to the created method reference node.
 */
 
JVM_METHODREF *
bc_new_method_node(char *cname, char *mname, char *dname)
{
  JVM_METHODREF *methodref;
  
  if(!cname || !mname || !dname) {
    BAD_ARG();
    return NULL;
  }

  debug_msg("%%%% new node '%s','%s','%s'\n", cname,mname,dname);

  methodref = (JVM_METHODREF *)malloc(sizeof(JVM_METHODREF));

  if(!methodref) return NULL;

  methodref->classname = char_substitute(cname, '.', '/');
  methodref->methodname = strdup(mname);
  methodref->descriptor = char_substitute(dname, '.', '/');

  if(!methodref->classname || !methodref->methodname || !methodref->descriptor)
  {
    if(methodref->classname) free(methodref->classname);
    if(methodref->methodname) free(methodref->methodname);
    if(methodref->descriptor) free(methodref->descriptor);

    free(methodref);
    return NULL;
  }

  return methodref;
}

/**
 * This function creates a new reference to a name and descriptor in the
 * constant pool.
 *
 * @param class -- Class containing the constant pool where this
 *   namd-and-type reference will be stored.
 * @param name -- The name of the item.
 * @param desc -- The descriptor of the item.
 *
 * @returns The constant pool index of the name-and-type reference.
 *    Returns -1 on failure.
 */

int
bc_new_name_and_type(JVM_CLASS *class, char *name, char *desc)
{
  JVM_METHODREF *nameref;
  int retval;

  if(!class || !name || !desc) {
    BAD_ARG();
    return -1;
  }

  nameref = (JVM_METHODREF *)malloc(sizeof(JVM_METHODREF));

  if(!nameref) return -1;

  nameref->classname = NULL;
  nameref->methodname = strdup(name);
  nameref->descriptor = char_substitute(desc, '.', '/');

  if(!nameref->methodname || !nameref->descriptor)
  {
    bc_free_nameandtype(nameref);
    return -1;
  }

  retval = cp_find_or_insert(class, CONSTANT_NameAndType, nameref);

  bc_free_nameandtype(nameref);

  return retval;
}

/**
 * This function creates a new field reference and inserts it into the
 * constant pool if necessary.  The return value is a pointer to the
 * constant pool node containing the field reference.
 *
 * @param class -- Class containing the constant pool where this
 *   field reference will be stored.
 * @param cname -- The name of the class.
 * @param mname -- The name of the field.
 * @param dnmae -- The field descriptor.
 *
 * @returns The constant pool index of the interface reference.
 *    Returns -1 on failure.
 */

int
bc_new_fieldref(JVM_CLASS *class, char *cname, char *mname, char *dname)
{ 
  JVM_METHODREF *fieldref;
  int retval;
  
  if(!class || !cname || !mname || !dname) {
    BAD_ARG();
    return -1;
  }

  fieldref = bc_new_method_node(cname, mname, dname);
  
  if(!fieldref) return -1;

  retval = cp_find_or_insert(class, CONSTANT_Fieldref, fieldref);
  
  bc_free_fieldref(fieldref);
  
  return retval;
}

/**
 * This function associates a label with a particular instruction.
 * This information is used later to calculate the branch target
 * offsets for branch instructions whose targets were labels.
 * See the bc_set_branch_label() function.
 *
 * Misc notes: this function creates a JVM_BRANCH_PC struct and fills 
 * it in with the pc and label number. This is then inserted into the 
 * method info struct.  Used later by calc_offsets for goto stmts.
 *
 * @param meth -- The method containing the branch and target instructions.
 * @param node -- The node of the target (that is, the instruction which
 *   corresponds to the label in the source code).
 * @param label -- The label specified for this instruction.
 * 
 * @returns 0 on success, -1 on failure.
 */

int
bc_associate_branch_label(JVM_METHOD *meth, JVM_CODE_GRAPH_NODE *node,
  const char *label) 
{
  JVM_BRANCH_PC *bp;

  if(!meth || !node) {
    BAD_ARG();
    return -1;
  }

  bp = (JVM_BRANCH_PC *)malloc(sizeof(JVM_BRANCH_PC));

  if(!bp) return -1;

  bp->instr = node;
  bp->label = strdup(label);

  dl_insert_b(meth->label_list, bp);

  return 0;
}

/**
 * This function associates a label with a particular instruction.
 * Same as bc_associate_branch_label() except that the label is
 * specified as an integer rather than string.
 *
 * @param meth -- The method containing the branch and target instructions.
 * @param node -- The node of the target (that is, the instruction which
 *   corresponds to the label in the source code).
 * @param label_num -- The label number specified for this instruction.
 * 
 * @returns 0 on success, -1 on failure.
 */

int
bc_associate_integer_branch_label(JVM_METHOD *meth, JVM_CODE_GRAPH_NODE *node,
  int label_num)
{
  char label[20];

  if(!meth || !node) {
    BAD_ARG();
    return -1;
  }

  sprintf(label, "%d", label_num);

  return bc_associate_branch_label(meth, node, label);
}

/**
 * This function gets a variable length argument and calls the appropriate
 * routine.  All routines deal with appending an opcode instruction to a
 * methods code array.
 *
 * @param meth -- The method to which this instruction should be added.
 * @param op -- The opcode to be generated.
 * @param ... -- The remaining arguments represent the operands of the
 *   instruction.
 *
 * @returns Pointer to the instruction node.
 *    Returns NULL on error.
 */

JVM_CODE_GRAPH_NODE *
bc_append(JVM_METHOD *meth, JVM_OPCODE op, ...)
{
  JVM_CODE_GRAPH_NODE *cgNode;
  int inv_idx, inv_cnt;
  va_list pvar;
  u1 index, value;
  u2 dimensions, idx2;
  u4 operand; 

  if(!meth) {
    BAD_ARG();
    return NULL;
  }

  va_start(pvar, op);

  switch(op) {
    case jvm_multianewarray:
      idx2 = (u2)va_arg(pvar, int);
      dimensions = (u1)va_arg(pvar, int);
      operand =(idx2<<8) | dimensions;
      cgNode =  bytecode1(meth, jvm_multianewarray, operand);
      break;
    case jvm_tableswitch:
    case jvm_lookupswitch:
      cgNode = bc_gen_switch(meth);  
      break;
    case jvm_invokeinterface:
      inv_idx = va_arg(pvar, int);
      inv_cnt = va_arg(pvar, int);

      operand = (inv_idx << 16) | (inv_cnt << 8);
      cgNode = bytecode1(meth, op, operand);
      break;
    case jvm_xxxunusedxxx:
      cgNode = bytecode0(meth, op);
      break;
    case jvm_goto:
      cgNode = bytecode0(meth, op);
      break;
    case jvm_jsr:
      cgNode = bytecode0(meth, op);
      break;
    case jvm_iinc:
      index = (u1)va_arg(pvar, int);
      value = (u1)va_arg(pvar, int);
      cgNode = bc_gen_iinc(meth, index, value);
      break;
    default:
      if(jvm_opcode[op].width <= 1) {
        cgNode = bytecode0(meth, op);
      }
      else if(jvm_opcode[op].width > 1) {
        operand = (u4)va_arg(pvar, int);
        cgNode = bytecode1(meth, op, operand);
      }
  }

  va_end(pvar);

  return cgNode;
} 

/**
 * Given a file path, open and create directories along the way, if needed.
 *
 * @param file -- The name of the file to be opened.
 * @param mode -- The file creation mode (man fopen(3)).
 * @param output_dir -- The prefix for the full file name (if NULL, just
 *   open the file in the current directory).
 *
 * @returns A file pointer to the created file.
 */
  
FILE *
bc_fopen_fullpath(char *file, char *mode, char *output_dir)
{
  char *pwd = NULL, *prev = NULL, *segment = NULL, *full_file = NULL;
  struct stat *buf = NULL;
  int cur_size;
  FILE *f;

#define err_fopen_full() \
    if(buf) free(buf); \
    if(pwd) free(pwd); \
    if(segment) free(segment); \
    if(full_file) free(full_file);

  if(!file) {
    BAD_ARG();
    return NULL;
  }

  if(!mode) mode = "wb";

  cur_size = 2;
  pwd = (char *)malloc(cur_size);
  if(!pwd) return NULL;

  buf = (struct stat *)malloc(sizeof(struct stat));
  if(!buf) {
    err_fopen_full();
    return NULL;
  }

  while(getcwd(pwd, cur_size) == NULL) {
    char *tmp;

    cur_size *= 2;
 
    tmp = pwd;
    pwd = (char *)realloc(pwd,cur_size);

    if(!pwd) {
      free(tmp);
      err_fopen_full();
      return NULL;
    }
  }

  if(output_dir != NULL) {
    full_file = (char *)malloc(strlen(output_dir) + strlen(file) + 3);

    strcpy(full_file, output_dir);
    if(output_dir[strlen(output_dir)-1] != BC_FILE_DELIM[0])
      strcat(full_file, BC_FILE_DELIM);
    strcat(full_file, file);
  }
  else
    full_file = strdup(file);

  if(!full_file) {
    err_fopen_full();
    return NULL;
  }

  debug_msg("full_file = '%s'\n", full_file);

  if( stat(full_file, buf) == 0)
    if(! S_ISREG(buf->st_mode) ) {
      err_fopen_full();
      return NULL;
    }

  if( (f = fopen(full_file, mode)) != NULL ) {
    err_fopen_full();
    return f;
  }

  if(full_file[0] == BC_FILE_DELIM[0])
    chdir(BC_FILE_DELIM);

  prev = strtok(full_file, BC_FILE_DELIM);

  while( (segment = strtok(NULL,BC_FILE_DELIM)) != NULL ) {

    if( stat(prev, buf) == -1) {
      if(errno == ENOENT) {
#ifdef _WIN32
        if(mkdir(prev) == -1) {
#else
        if(mkdir(prev, 0755) == -1) {
#endif
          chdir(pwd);
          err_fopen_full();
          return NULL;
        }
      }
      else {
        chdir(pwd);
        err_fopen_full();
        return NULL;
      }
    }
    else {
      if(! S_ISDIR(buf->st_mode)) {
        chdir(pwd);
        err_fopen_full();
        return NULL;
      }
    }

    if(chdir(prev) == -1) {
      chdir(pwd);
      err_fopen_full();
      return NULL;
    }

    prev = segment;
  } 
    
  if( (f = fopen(prev, mode)) != NULL ) {
    chdir(pwd);
    err_fopen_full();
    return f;
  }
  
  chdir(pwd);
  free(full_file);
  free(buf);
  free(pwd);
  return NULL; 
}

/**
 * Frees a method info structure.
 *
 * @param m -- The method to be freed.
 */

void
bc_free_method(JVM_METHOD *m)
{
  JVM_ATTRIBUTE *code_attr = NULL;

  if(!m) {
    BAD_ARG();
    return;
  }

  code_attr = find_attribute(m->class,m->attributes,"Code");

  bc_free_attributes(m->class, m->attributes);
  m->attributes = NULL;

  /* if this method was abstract or native, then the code graph would not
   * have been inserted as an attribute to this method (because such methods
   * do not have any code).  therefore the code graph (actually a Dlist) 
   * would not have been freed yet, so we free it here.
   */

  if(!code_attr)
  {
    bc_free_code_attribute(m->class, m->cur_code);
    m->cur_code = NULL;
  }

  if(m->exc_table) dl_delete_list(m->exc_table);

  bc_free_locals_table(m);
  bc_free_line_number_table(m);
  bc_free_label_list(m);

  m->attributes = NULL;
  m->exc_table = NULL;
  m->label_list = NULL;
  m->line_table = NULL;

  if(m->name) free(m->name);
  free(m);
}

/**
 * Frees a line number table.
 *
 * @param m -- The method containing the line number table.
 */

void
bc_free_line_number_table(JVM_METHOD *m)
{
  Dlist tmp;

  dl_traverse(tmp, m->line_table) {
    free(dl_val(tmp));
  }
  dl_delete_list(m->line_table);
  m->line_table = NULL;
}

/**
 * Frees a local variable table.
 *
 * @param m -- The method containing the local variable table.
 */

void
bc_free_locals_table(JVM_METHOD *m)
{
  Dlist tmp;

  dl_traverse(tmp, m->locals_table) {
    JVM_LOCAL_VARIABLE_TABLE_ENTRY * loc;

    loc = (JVM_LOCAL_VARIABLE_TABLE_ENTRY *) dl_val(tmp);

    if(loc->name) free(loc->name);
    if(loc->descriptor) free(loc->descriptor);
    free(loc);
  }

  dl_delete_list(m->locals_table);
  m->locals_table = NULL;
}

/**
 * Frees the list of branch labels in a method.
 *
 * @param m -- The method containing the local variable table.
 */

void
bc_free_label_list(JVM_METHOD *m)
{
  Dlist tmp;

  dl_traverse(tmp, m->label_list) {
    JVM_BRANCH_PC *bp = (JVM_BRANCH_PC *)dl_val(tmp);
    free(bp->label);
    free(bp);
  }
  dl_delete_list(m->label_list);
  m->label_list = NULL;
}

/**
 * Frees a class (and frees all fields of the class file structure).
 *
 * @param class -- The class to be freed.
 */

void
bc_free_class(JVM_CLASS *class)
{
  if(!class) {
    BAD_ARG();
    return;
  }

  bc_free_interfaces(class);
  bc_free_fields(class);
  bc_free_methods(class);
  bc_free_attributes(class, class->attributes);

  /* NOTE: free constant pool last. */
  bc_free_constant_pool(class);

  free(class);
}

/**
 * Frees the list of interfaces the class implements.
 *
 * @param class -- The class containing the list of interfaces.
 */

void
bc_free_interfaces(JVM_CLASS *class)
{
  int * tmpconst;
  Dlist tmpPtr;

  if(!class) {
    BAD_ARG();
    return;
  }

  dl_traverse(tmpPtr,class->interfaces) {
    tmpconst = (int *) tmpPtr->val;
    free(tmpconst);
  }

  dl_delete_list(class->interfaces);
  class->interfaces = NULL;
}

/**
 * Frees the constant pool.
 *
 * @param class -- The class containing the constant pool.
 */

void
bc_free_constant_pool(JVM_CLASS *class)
{
  CP_NODE * tmpconst;
  Dlist tmpPtr;

  if(!class) {
    BAD_ARG();
    return;
  }

  dl_traverse(tmpPtr,class->constant_pool) {
    tmpconst = (CP_NODE *) tmpPtr->val;

    if(tmpconst->val->tag == CONSTANT_Utf8)
      free(tmpconst->val->cpnode.Utf8.bytes);
    free(tmpconst->val);
    free(tmpconst);
  }

  dl_delete_list(class->constant_pool);
  class->constant_pool = NULL;
}

/**
 * Frees the list of fields of this class.
 *
 * @param class -- The class containing the list of fields.
 */

void
bc_free_fields(JVM_CLASS *class)
{
  JVM_FIELD *tmpfield;
  Dlist tmpPtr;

  if(!class) {
    BAD_ARG();
    return;
  }

  dl_traverse(tmpPtr,class->fields) {
    tmpfield = (JVM_FIELD *) tmpPtr->val;

    bc_free_attributes(class, tmpfield->attributes);
    free(tmpfield);
  }

  dl_delete_list(class->fields);
  class->fields = NULL;
}

/**
 * Frees the list of methods of this class.
 *
 * @param class -- The class containing the list of methods.
 */

void
bc_free_methods(JVM_CLASS *class)
{
  Dlist tmpPtr;

  if(!class) {
    BAD_ARG();
    return;
  }

  dl_traverse(tmpPtr,class->methods) {
    bc_free_method((JVM_METHOD *) tmpPtr->val);
  }

  dl_delete_list(class->methods);
  class->methods = NULL;
}

/**
 * Frees a list of attributes.  The attribute list may correspond to
 * a class, method, or field.
 *
 * @param class -- The class containing the constant pool relevant to
 *   the attributes.
 * @param attr_list -- The attribute list to be freed.
 */

void
bc_free_attributes(JVM_CLASS *class, Dlist attr_list)
{
  JVM_ATTRIBUTE *tmpattr;
  char *attr_name;
  Dlist tmpPtr, tmpPtr2;
  CP_NODE *c;

  if(!attr_list || !class) {
    BAD_ARG();
    return;
  }

  dl_traverse(tmpPtr,attr_list) {
    tmpattr = (JVM_ATTRIBUTE *) tmpPtr->val;

    c = cp_entry_by_index(class, tmpattr->attribute_name_index);
    if(c==NULL) {
      debug_err("WARNING: bc_free_attributes() can't find attr name\n");
      continue;
    }

    attr_name = cp_null_term_utf8(c->val);
    if(!attr_name)
      continue;

    if(!strcmp(attr_name,"SourceFile")) {
      free(tmpattr->attr.SourceFile);
      free(tmpattr);
    }
    else if(!strcmp(attr_name,"Deprecated") ||
            !strcmp(attr_name,"Synthetic")) {
      free(tmpattr);
    }
    else if(!strcmp(attr_name,"LocalVariableTable")) {
      free(tmpattr->attr.LocalVariableTable);
      free(tmpattr);
    }
    else if(!strcmp(attr_name,"LineNumberTable")) {
      free(tmpattr->attr.LineNumberTable);
      free(tmpattr);
    }
    else if(!strcmp(attr_name,"InnerClasses")) {
      dl_traverse(tmpPtr2, tmpattr->attr.InnerClasses->classes)
        free(tmpPtr2->val);

      dl_delete_list(tmpattr->attr.InnerClasses->classes);
      free(tmpattr->attr.InnerClasses);
      free(tmpattr);
    }
    else if(!strcmp(attr_name,"ConstantValue")) {
      free(tmpattr->attr.ConstantValue);
      free(tmpattr);
    }
    else if(!strcmp(attr_name,"Code")) {
      bc_free_code_attribute(class, tmpattr);
    }
    else if(!strcmp(attr_name,"Exceptions")) {
      dl_traverse(tmpPtr2, tmpattr->attr.Exceptions->exception_index_table)
        free(tmpPtr2->val);

      dl_delete_list(tmpattr->attr.Exceptions->exception_index_table);
      tmpattr->attr.Exceptions->exception_index_table = NULL;
      free(tmpattr->attr.Exceptions);
      free(tmpattr);
    }
    else {
      /* if the attribute name doesn't match any of the known attributes
       * then assume it's a user defined attribute.
       */
      free(tmpattr->attr.UserDefined->data);
      free(tmpattr->attr.UserDefined);
      free(tmpattr);
    }

    free(attr_name);
  }

  dl_delete_list(attr_list);
}

/**
 * Frees a code attribute.
 *
 * @param class -- The class containing the constant pool relevant to
 *   the code attribute.
 * @param attr -- The code attribute to be freed.
 */

void
bc_free_code_attribute(JVM_CLASS *class, JVM_ATTRIBUTE *attr)
{
  if(!attr) {
    BAD_ARG();
    return;
  }

  bc_free_code(attr->attr.Code->code);

  if(attr->attr.Code->exception_table_length > 0)
    free(attr->attr.Code->exception_table);

  if((attr->attr.Code->attributes_count > 0) && (class != NULL))
    bc_free_attributes(class, attr->attr.Code->attributes);
  else
    dl_delete_list(attr->attr.Code->attributes);

  attr->attr.Code->attributes = NULL;

  free(attr->attr.Code);
  free(attr);
}

/**
 * Frees the list of instruction nodes.
 *
 * @param g -- The list of instructions to be freed.
 */

void
bc_free_code(Dlist g)
{
  Dlist tmp;
  int i;

  if(!g) {
    BAD_ARG();
    return;
  }

  dl_traverse(tmp, g) {
    JVM_CODE_GRAPH_NODE *instr = (JVM_CODE_GRAPH_NODE *)dl_val(tmp);

    if((instr->op == jvm_tableswitch) ||
       (instr->op == jvm_lookupswitch))
    {
      dl_delete_list(instr->switch_info->offsets);

      for(i=0;i<instr->switch_info->num_entries;i++)
        free(instr->switch_info->sorted_entries[i]);
      free(instr->switch_info->sorted_entries);
      free(instr->switch_info);
    }

    if(instr->branch_label) free(instr->branch_label);

    free(tmp->val);
  }

  dl_delete_list(g);
}

/**
 * This function frees memory previously allocated for a fieldref.
 * 
 * @param fieldref -- The field reference to be freed.
 */

void
bc_free_fieldref(JVM_METHODREF *fieldref)
{
  if(!fieldref) {
    BAD_ARG();
    return;
  }

  free(fieldref->classname);
  free(fieldref->methodname);
  free(fieldref->descriptor);
  free(fieldref);
}

/**
 * This function frees memory previously allocated for a methodref.
 * 
 * @param methodref -- The method reference to be freed.
 */

void
bc_free_methodref(JVM_METHODREF *methodref)
{
  if(!methodref) {
    BAD_ARG();
    return;
  }

  bc_free_fieldref(methodref);
}

/**
 * This function frees memory previously allocated for an interface method
 * reference.
 * 
 * @param interfaceref -- The interface reference to be freed.
 */

void
bc_free_interfaceref(JVM_METHODREF *interfaceref)
{
  if(!interfaceref) {
    BAD_ARG();
    return;
  }

  bc_free_fieldref(interfaceref);
}

/**
 * This function frees memory previously allocated for a name and descriptor
 * reference.
 * 
 * @param nameref -- The name-and-type reference to be freed.
 */

void
bc_free_nameandtype(JVM_METHODREF *nameref)
{
  if(!nameref) {
    BAD_ARG();
    return;
  }

  bc_free_fieldref(nameref);
}

/*****************************************************************************
 *****************************************************************************
 **                                                                         **
 ** Functions after this point are not exposed as part of the API.          **
 **                                                                         **
 *****************************************************************************
 *****************************************************************************/


/**
 * Finds the given attribute in an attribute list.
 * Returns NULL if the attribute cannot be found.
 *
 * @param class -- The class containing the constant pool relevant to
 *   the attribute.
 * @param attr_list -- The list of attributes to be searched.
 * @param attr -- The name of the attribute to find.
 *
 * @returns Pointer to the attribute, if found.  If the attribute is not
 *   found, returns NULL.
 */
  
static JVM_ATTRIBUTE *
find_attribute(JVM_CLASS *class, Dlist attr_list, char *attr)
{
  JVM_ATTRIBUTE *tmpattr;
  char *attr_name;
  Dlist tmpPtr;
  CP_NODE *c;

  if(!attr_list || !class || !attr) {
    BAD_ARG();
    return NULL;
  }

  dl_traverse(tmpPtr,attr_list) {
    tmpattr = (JVM_ATTRIBUTE *) tmpPtr->val;

    c = cp_entry_by_index(class, tmpattr->attribute_name_index);

    if(c == NULL) {
      debug_err("WARNING: find_attribute() can't find attr name\n");
      continue;
    }

    attr_name = cp_null_term_utf8(c->val);
    if(!attr_name)
      continue;

    if(!strcmp(attr_name,attr)) {
      free(attr_name);
      return tmpattr;
    }

    free(attr_name);
  }

  return NULL;
}

/**
 * Creates a new attribute structure and initializes the Code_attribute
 * section with some initial values.
 *
 * @param cclass -- The class containing the constant pool relevant to
 *   the attribute.
 *
 * @returns Pointer to the new attribute.
 */

static JVM_ATTRIBUTE *
new_code_attr(JVM_CLASS *cclass)
{
  JVM_ATTRIBUTE * tmp;
  int c;

  if(!cclass) {
    BAD_ARG();
    return NULL;
  }

  tmp = (JVM_ATTRIBUTE *)malloc(sizeof(JVM_ATTRIBUTE));

  if(!tmp) return NULL;

  c = cp_find_or_insert(cclass, CONSTANT_Utf8, "Code");

  if(c < 0) {
    free(tmp);
    return NULL;
  }

  tmp->attribute_name_index = c;
  tmp->attribute_length = 0;
  tmp->attr.Code = (struct Code_attribute *)
        malloc(sizeof(struct Code_attribute));

  if(!tmp->attr.Code) {
    free(tmp);
    return NULL;
  }

  tmp->attr.Code->max_stack = 0;
  tmp->attr.Code->max_locals = 0;
  tmp->attr.Code->code_length = 0;
  tmp->attr.Code->code = make_dl();
  tmp->attr.Code->exception_table_length = 0;
  tmp->attr.Code->exception_table = NULL;
  tmp->attr.Code->attributes_count = 0;
  tmp->attr.Code->attributes = make_dl();
  
  if(!tmp->attr.Code->code || !tmp->attr.Code->attributes) {
    if(tmp->attr.Code->code) dl_delete_list(tmp->attr.Code->code);
    if(tmp->attr.Code->attributes) dl_delete_list(tmp->attr.Code->attributes);

    tmp->attr.Code->code = NULL;
    tmp->attr.Code->attributes = NULL;

    free(tmp->attr.Code);
    tmp->attr.Code = NULL;

    free(tmp);
    return NULL;
  }

  return tmp;
}

/**
 * Inserts the given instruction into the code graph.
 *
 * @param meth -- The method to which this instruction should be added.
 * @param op -- The opcode to be generated.
 *
 * @returns Pointer to the instruction node.
 *    Returns NULL on error.
 */

static JVM_CODE_GRAPH_NODE *
bytecode0(JVM_METHOD *meth, JVM_OPCODE op)
{
  if(!meth) {
    BAD_ARG();
    return NULL;
  }

  return bytecode1(meth, op,0);
}

/**
 * Inserts the given instruction into the code graph.
 *
 * @param meth -- The method to which this instruction should be added.
 * @param op -- The opcode to be generated.
 * @param operand -- The operand to this instruction.
 *
 * @returns Pointer to the instruction node.
 *    Returns NULL on error.
 */

static JVM_CODE_GRAPH_NODE *
bytecode1(JVM_METHOD *meth, JVM_OPCODE op, u4 operand)
{
  JVM_CODE_GRAPH_NODE *tmp, *prev;

  if(!meth) {
    BAD_ARG();
    return NULL;
  }

  /* if we should not generate bytecode, then just return a dummy node */
  if(!meth->gen_bytecode) {
    JVM_CODE_GRAPH_NODE *g;

    /* keep track of the dummy node so that we may reclaim the memory later. */
    g = bc_new_graph_node(meth, op, operand);
    return g;
  }

  meth->lastOp = op;

  if(meth->cur_code->attr.Code->code == NULL)
    debug_err("ERROR: null code graph.\n");

  prev = (JVM_CODE_GRAPH_NODE *) dl_val(dl_last(meth->cur_code->attr.Code->code));

  if((prev != NULL) && (prev->op == jvm_xxxunusedxxx)) {
    prev->op = op;
    prev->operand = operand;
    prev->width = bc_op_width(op);
    meth->pc += bc_op_width(op) - bc_op_width(jvm_xxxunusedxxx);
    return prev;
  }

  tmp = bc_new_graph_node(meth, op, operand);

  if(!tmp) return NULL;

  if(prev != NULL)
    prev->next = tmp;

  dl_insert_b(meth->cur_code->attr.Code->code, tmp);

  /* if the previous instruction was 'wide', then we need to
   * increase the width of this instruction.
   */
  if((prev != NULL) && (prev->op == jvm_wide)) {
    if( (op == jvm_iload) || (op == jvm_fload) || (op == jvm_aload) ||
        (op == jvm_lload) || (op == jvm_dload) || (op == jvm_istore) ||
        (op == jvm_fstore) || (op == jvm_astore) || (op == jvm_lstore) ||
        (op == jvm_dstore) || (op == jvm_ret))
      tmp->width = bc_op_width(op) + 1;
    else if(op == jvm_iinc)
      tmp->width = bc_op_width(op) + 2;
    else
      debug_err("Error: bad op used after wide instruction (%s)\n",
          jvm_opcode[op].op);
  }

  meth->pc += tmp->width;

  return tmp;
}

/**
 * Given a local variable number (which presumably is the target of some
 * load/store or other instruction that uses a local), make sure that the
 * total number of local variables for this method is large enough to
 * accommodate the specified local variable.  If not, then update it based
 * on the given number.
 *
 * @param meth -- The current method.
 * @param lvnum -- The local variable number being used in some instruction.
 * @param rt -- The JVM data type of the local variable (see the enumeration
 *   JVM_DATA_TYPE).
 */

static void
updateMaxLocals(JVM_METHOD *meth, unsigned int lvnum,
  JVM_DATA_TYPE rt)
{
  int max = lvnum + jvm_localvar_width[rt];

  if(!meth) {
    BAD_ARG();
    return;
  }

  if(max > meth->max_locals)
    meth->max_locals = max;
}

/**
 * Given a method descriptor, this function returns the number of local
 * variables needed to hold the arguments.  doubles and longs use 2 local
 * vars, while every other data type only uses 1 local.
 * 
 * @param d -- The method descriptor.
 *
 * @returns The number of local variables in this descriptor.
 */

static int
num_locals_in_descriptor(char *d)
{
  int vlen = 0;

  if(!d) {
    BAD_ARG();
    return 0;
  }

  while( (d = bc_next_desc_token(d)) != NULL) {

    /* if the next token is NULL, then we have no more useful tokens in
     * this descriptor.
     */
    if(bc_next_desc_token(d) == NULL)
      break;

    if((d[0] == 'D') || (d[0] == 'J'))
      vlen += 2;
    else
      vlen++;
  }

  return vlen;
}

/**
 * This function substitutes every occurrence of 'from_char' with 'to_char'
 * typically this is used to convert package names:
 *
 *   e.g.     "java.lang.whatever" -> "java/lang/whatever"
 *
 * Space for the modified string is allocated by this function.
 *
 * @param str -- The string to be converted.
 * @param from_char -- The character to change from.
 * @param to_char -- The character to change to.
 *
 * @returns The modified string (in newly allocated memory).
 */

static char *
char_substitute(char *str, int from_char, int to_char)
{
  char *newstr, *idx;

  if(!str) {
    BAD_ARG();
    return NULL;
  }

  newstr = strdup(str);
  if(!newstr) return NULL;

  while( (idx = strchr(newstr, from_char)) != NULL )
    *idx = to_char;

  return newstr;
}
