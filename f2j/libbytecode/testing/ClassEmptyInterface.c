#include <stdio.h>
#include "bytecode.h"

/* 
 * Generates an interface with the JVM_ACC_ABSTRACT and JVM_ACC_INTERFACE
 * access modifiers set.  Since an interface cannot implement any
 * methods, we do not create any methods here.
 */

int main(){
  JVM_CLASS *cur_class_file;

  cur_class_file = bc_new_class("ClassEmptyInterface", "asdf.f", NULL, 
      NULL, JVM_ACC_PUBLIC|JVM_ACC_SUPER|JVM_ACC_ABSTRACT|JVM_ACC_INTERFACE);

  bc_write_class(cur_class_file, ".");
  bc_free_class(cur_class_file);

  return 0;
}
