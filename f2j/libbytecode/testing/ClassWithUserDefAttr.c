#include <stdio.h>
#include "bytecode.h"

/* 
 * Shows how to add a user-defined attribute to a class.
 */

int main() {
  char *attr_text = "this is a user-defined attribute";
  JVM_METHOD *main_method;
  JVM_FIELD *field;
  JVM_CLASS *cur_class_file;
  int intval;
  int c;

  cur_class_file = bc_new_class("ClassWithUserDefAttr", NULL, NULL, NULL, 
      JVM_ACC_PUBLIC|JVM_ACC_SUPER);

  bc_add_user_defined_class_attr(cur_class_file, "MyAttribute", 
     strlen(attr_text), attr_text);

  bc_add_default_constructor(cur_class_file, JVM_ACC_PUBLIC);

  main_method = bc_new_method(cur_class_file, "main",
     "([Ljava/lang/String;)V", JVM_ACC_PUBLIC|JVM_ACC_STATIC);

  bc_gen_return(main_method);

  bc_write_class(cur_class_file, ".");
  bc_free_class(cur_class_file);

  return 0;
}
