#include <stdio.h>
#include "bytecode.h"

/* 
 * Generates a class with a deprecated method and a method with the
 * "Synthetic" attribute set.
 */

int main() {
  JVM_METHOD *main_method, *dep_method, *syn_method;
  JVM_CLASS *cur_class_file;

  cur_class_file = bc_new_class("MethodDeprecated", "asdf.f", NULL, NULL, 
      JVM_ACC_PUBLIC|JVM_ACC_SUPER);

  bc_add_default_constructor(cur_class_file, JVM_ACC_PUBLIC);

  dep_method = bc_new_method(cur_class_file, "foo",
     "()V", JVM_ACC_PUBLIC);
  bc_set_method_deprecated(dep_method);
  bc_gen_return(dep_method);

  syn_method = bc_new_method(cur_class_file, "synth",
     "()V", JVM_ACC_PUBLIC);
  bc_set_method_synthetic(syn_method);
  bc_gen_return(syn_method);

  main_method = bc_new_method(cur_class_file, "main",
     "([Ljava/lang/String;)V", JVM_ACC_PUBLIC|JVM_ACC_STATIC);

  bc_gen_return(main_method);

  bc_write_class(cur_class_file, ".");
  bc_free_class(cur_class_file);

  return 0;
}
