#include <stdio.h>
#include "bytecode.h"

/* 
 * Generates an interface with one abstract method.
 */

int main(){
  JVM_METHOD *abstract_method;
  JVM_CLASS *cur_class_file;

  cur_class_file = bc_new_class("ClassNonEmptyInterface", "asdf.f", NULL,
      NULL, JVM_ACC_PUBLIC|JVM_ACC_SUPER|JVM_ACC_ABSTRACT|JVM_ACC_INTERFACE);

  abstract_method = bc_new_method(cur_class_file, "foo",
     "([Ljava/lang/String;)V", JVM_ACC_PUBLIC|JVM_ACC_ABSTRACT);

  bc_write_class(cur_class_file, ".");
  bc_free_class(cur_class_file);

  return 0;
}
