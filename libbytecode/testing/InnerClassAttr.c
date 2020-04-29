#include <stdio.h>
#include "bytecode.h"

/*
 * Generates a class with an InnerClasses attribute.
 */

int main(){
  JVM_METHOD *main_method;
  JVM_CLASS *cur_class_file;
  JVM_CLASS *inner_class_file;
  JVM_CODE_GRAPH_NODE *op;

  /* first generate a class to represent the referenced inner class */

  inner_class_file = bc_new_class("inner", "asdf.f", NULL,
      NULL, JVM_ACC_PUBLIC|JVM_ACC_SUPER);
  bc_add_default_constructor(inner_class_file, JVM_ACC_PUBLIC);

  bc_write_class(inner_class_file, ".");
  bc_free_class(inner_class_file);


  /* now create the class which has the InnerClasses attribute */

  cur_class_file = bc_new_class("InnerClassAttr", "asdf.f", NULL, 
      NULL, JVM_ACC_PUBLIC|JVM_ACC_SUPER);
  bc_add_default_constructor(cur_class_file, JVM_ACC_PUBLIC);

  main_method = bc_new_method(cur_class_file, "main",
     "([Ljava/lang/String;)V", JVM_ACC_PUBLIC|JVM_ACC_STATIC);

  op = bc_push_int_const(main_method, 12);
  bc_gen_store_op(main_method, 1, jvm_Int);

  op = bc_gen_return(main_method);

  bc_add_inner_classes_attr(cur_class_file, "inner", "InnerClassAttr",
     "foo", JVM_ACC_PUBLIC);

  bc_write_class(cur_class_file, ".");
  bc_free_class(cur_class_file);

  return 0;
}
