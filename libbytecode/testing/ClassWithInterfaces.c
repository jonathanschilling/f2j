#include <stdio.h>
#include "bytecode.h"

/* 
 * Generates a class with some interfaces.
 */

int main(){
  JVM_METHOD *main_method, *run_method, *comp_method;
  JVM_CLASS *cur_class_file;

  cur_class_file = bc_new_class("ClassWithInterfaces", "asdf.f", 
      "java.lang.Thread", NULL, JVM_ACC_PUBLIC|JVM_ACC_SUPER);

  bc_add_default_constructor(cur_class_file, JVM_ACC_PUBLIC);

  bc_add_class_interface(cur_class_file, "java.lang.Runnable");
  bc_add_class_interface(cur_class_file, "java.lang.Cloneable");
  bc_add_class_interface(cur_class_file, "java.lang.Comparable");
     
  run_method = bc_new_method(cur_class_file, "run",
     "()V", JVM_ACC_PUBLIC);
  bc_gen_return(run_method);

  comp_method = bc_new_method(cur_class_file, "compareTo",
     "(Ljava/lang/Object;)I", JVM_ACC_PUBLIC);
  bc_append(comp_method, jvm_iconst_0);
  bc_gen_return(comp_method);

  main_method = bc_new_method(cur_class_file, "main",
     "([Ljava/lang/String;)V", JVM_ACC_PUBLIC|JVM_ACC_STATIC);

  bc_gen_return(main_method);

  bc_write_class(cur_class_file, ".");
  bc_free_class(cur_class_file);

  return 0;
}
