#include <stdio.h>
#include "bytecode.h"

/* 
 * Generates several methods which are declared to throw exceptions.
 */

int main() {
  JVM_METHOD *main_method, *no_exc_meth, *one_exc_meth, *two_exc_meth;
  JVM_CLASS *cur_class_file;

  cur_class_file = bc_new_class("MethodWithExceptions", "asdf.f", NULL, NULL, 
      JVM_ACC_PUBLIC|JVM_ACC_SUPER);

  bc_add_default_constructor(cur_class_file, JVM_ACC_PUBLIC);

  no_exc_meth = bc_new_method(cur_class_file, "no_exceptions",
     "()V", JVM_ACC_PUBLIC);
  bc_gen_return(no_exc_meth);

  one_exc_meth = bc_new_method(cur_class_file, "one_exception",
     "()V", JVM_ACC_PUBLIC);
  bc_add_method_exception(one_exc_meth, "java.lang.IllegalAccessException");
  bc_gen_return(one_exc_meth);

  two_exc_meth = bc_new_method(cur_class_file, "two_exceptions",
     "()V", JVM_ACC_PUBLIC);
  bc_add_method_exception(two_exc_meth, "java.lang.reflect.InvocationTargetException");
  bc_add_method_exception(two_exc_meth, "java.lang.IllegalAccessException");
  bc_gen_return(two_exc_meth);

  main_method = bc_new_method(cur_class_file, "main",
     "([Ljava/lang/String;)V", JVM_ACC_PUBLIC|JVM_ACC_STATIC);

  bc_gen_return(main_method);

  bc_write_class(cur_class_file, ".");
  bc_free_class(cur_class_file);

  return 0;
}
