#include <stdio.h>
#include "bytecode.h"

/*
 * Generates the various object creation and manipulation instructions 
 * mentioned in section 3.11.5 of the JVM spec (except for creating 
 * one-dimensional arrays and array load/store instructions which are 
 * tested in LoadStore.c).
 */

int main(){
  JVM_METHOD *main_method;
  JVM_METHOD *foo_method;
  JVM_CLASS *cur_class_file;
  int meth_idx;
  int i, obj_var, arr_var;

  cur_class_file = bc_new_class("ObjectCreation", "asdf.f", NULL, 
      NULL, JVM_ACC_PUBLIC|JVM_ACC_SUPER);
  bc_add_default_constructor(cur_class_file, JVM_ACC_PUBLIC);
  bc_add_field(cur_class_file, "x", "I", JVM_ACC_PUBLIC);
  bc_add_field(cur_class_file, "y", "I", JVM_ACC_PUBLIC|JVM_ACC_STATIC);

  foo_method = bc_new_method(cur_class_file, "foo",
     "()V", JVM_ACC_PUBLIC);
  bc_gen_return(foo_method);

  main_method = bc_new_method(cur_class_file, "main",
     "([Ljava/lang/String;)V", JVM_ACC_PUBLIC|JVM_ACC_STATIC);

  /* create a new instance of this class and call the method "foo" */

  obj_var = bc_get_next_local(main_method, jvm_Object);

  bc_gen_obj_instance_default(main_method, "ObjectCreation");

  bc_gen_store_op(main_method, obj_var, jvm_Object);
  bc_gen_load_op(main_method, obj_var, jvm_Object);

  meth_idx = bc_new_methodref(cur_class_file, "ObjectCreation",
     "foo", "()V");

  bc_append(main_method, jvm_invokevirtual, meth_idx);

  /* create new multi-dimensional array */
  
  arr_var = bc_get_next_local(main_method, jvm_Object);
  bc_push_int_const(main_method, 6);
  bc_push_int_const(main_method, 2);
  bc_new_multi_array(main_method, 2, "[[I");
  bc_gen_store_op(main_method, arr_var, jvm_Object);
  bc_gen_load_op(main_method, arr_var, jvm_Object);
  bc_push_int_const(main_method, 3);
  bc_append(main_method, jvm_aaload);
  bc_push_int_const(main_method, 1);
  bc_push_int_const(main_method, 5);
  bc_append(main_method, jvm_iastore);
  
  /* getting array length */
  bc_gen_load_op(main_method, arr_var, jvm_Object);
  bc_append(main_method, jvm_arraylength);

  /* accessing fields */

  bc_gen_load_op(main_method, obj_var, jvm_Object);
  bc_gen_load_op(main_method, obj_var, jvm_Object);
  bc_get_field(main_method, "ObjectCreation", "x", "I");
  bc_push_int_const(main_method,666);
  bc_append(main_method, jvm_iadd);
  bc_put_field(main_method, "ObjectCreation", "x", "I");

  bc_get_static(main_method, "ObjectCreation", "y", "I");
  bc_push_int_const(main_method,666);
  bc_append(main_method, jvm_iadd);
  bc_put_static(main_method, "ObjectCreation", "y", "I");

  /* bc_gen_instanceof */
  bc_gen_load_op(main_method, obj_var, jvm_Object);
  bc_gen_instanceof(main_method, "java.lang.Object");

  /* bc_gen_checkcast */
  bc_gen_load_op(main_method, obj_var, jvm_Object);
  bc_gen_checkcast(main_method, "java.lang.Object");

  bc_gen_return(main_method);

  bc_write_class(cur_class_file, ".");
  bc_free_class(cur_class_file);

  return 0;
}
