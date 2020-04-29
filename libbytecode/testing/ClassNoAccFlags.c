#include <stdio.h>
#include "bytecode.h"

/* 
 * Generates a class with no access modifiers set.
 */

int main(){
  JVM_METHOD *main_method;
  JVM_CLASS *cur_class_file;

  cur_class_file = bc_new_class("ClassNoAccFlags", "asdf.f", NULL, 
      NULL, 0x0);

  bc_add_default_constructor(cur_class_file, JVM_ACC_PUBLIC);

  main_method = bc_new_method(cur_class_file, "main",
     "([Ljava/lang/String;)V", JVM_ACC_PUBLIC|JVM_ACC_STATIC);

  bc_gen_return(main_method);

  bc_write_class(cur_class_file, ".");
  bc_free_class(cur_class_file);

  return 0;
}
