#include <stdio.h>
#include "bytecode.h"

/*
 * Generates all the various load/store instructions mentioned in
 * section 3.11.2 of the JVM spec.
 */

int main(){
  JVM_METHOD *main_method;
  JVM_METHOD *method;
  JVM_CLASS *cur_class_file;
  int i;

  cur_class_file = bc_new_class("LoadStore", "asdf.f", NULL, 
      NULL, JVM_ACC_PUBLIC|JVM_ACC_SUPER);
  bc_add_default_constructor(cur_class_file, JVM_ACC_PUBLIC);

  main_method = bc_new_method(cur_class_file, "main",
     "([Ljava/lang/String;)V", JVM_ACC_PUBLIC|JVM_ACC_STATIC);
  bc_gen_return(main_method);

  method = bc_new_method(cur_class_file, "loadstore",
     "()V", JVM_ACC_PUBLIC|JVM_ACC_STATIC);

  /* Loading integer constants */

  bc_push_int_const(method, 10);                  /* bipush */
  bc_push_int_const(method, 300);                 /* sipush */
  bc_push_int_const(method, 999999);              /* ldc */
  bc_push_int_const(method, -1);                  /* iconst_m1 */
  bc_push_int_const(method, 0);                   /* iconst_0 */
  bc_push_int_const(method, 1);                   /* iconst_1 */
  bc_push_int_const(method, 2);                   /* iconst_2 */
  bc_push_int_const(method, 3);                   /* iconst_3 */
  bc_push_int_const(method, 4);                   /* iconst_4 */
  bc_push_int_const(method, 5);                   /* iconst_5 */

  /* Loading object constant */
  bc_push_null_const(method);                     /* aconst_null */

  /* Loading float constants */
  bc_push_float_const(method, 0.0);               /* fconst_0 */
  bc_push_float_const(method, 1.0);               /* fconst_1 */
  bc_push_float_const(method, 2.0);               /* fconst_2 */
  bc_push_float_const(method, 9.9);               /* ldc */

  /* Loading string constants */
  bc_push_string_const(method, "foo");            /* ldc */

  /* Loading double constants */
  bc_push_double_const(method, 0.0);              /* dconst_0 */
  bc_push_double_const(method, 1.0);              /* dconst_1 */
  bc_push_double_const(method, 3.3);              /* ldc2_w */

  /* Loading long constants */
  bc_push_long_const(method, 0);                  /* lconst_0 */
  bc_push_long_const(method, 1);                  /* lconst_1 */
  bc_push_long_const(method, 55);                 /* ldc2_w */

  /* Force a bunch of junk into the constant pool to test
   * whether the wide load instruction gets generated on
   * subsequent calls to bc_push_int_const, etc.
   */

  for(i=3;i<255;i++) {
    double dval;

    dval = (double)i;
    cp_find_or_insert(cur_class_file, CONSTANT_Double, (void*)&dval);
  }

  /* now the following calls should generate "ldc_w". 
   * double and long always generate the wide "ldc2_w", 
   * so don't worry about testing them here as they
   * were tested above.
   */

  bc_push_int_const(method, 888888);              /* ldc_w */
  bc_push_float_const(method, 8.8);               /* ldc_w */
  bc_push_string_const(method, "bar");            /* ldc_w */

  /* Storing/Loading integer local variables */

  bc_push_int_const(method, 0);                   /* iconst_0 */
  bc_gen_store_op(method, 0, jvm_Int);          /* istore_0 */
  bc_gen_load_op(method, 0, jvm_Int);           /* iload_0 */

  bc_push_int_const(method, 0);                   /* iconst_0 */
  bc_gen_store_op(method, 1, jvm_Int);          /* istore_1 */
  bc_gen_load_op(method, 1, jvm_Int);           /* iload_1 */

  bc_push_int_const(method, 0);                   /* iconst_0 */
  bc_gen_store_op(method, 2, jvm_Int);          /* istore_2 */
  bc_gen_load_op(method, 2, jvm_Int);           /* iload_2 */

  bc_push_int_const(method, 0);                   /* iconst_0 */
  bc_gen_store_op(method, 3, jvm_Int);          /* istore_3 */
  bc_gen_load_op(method, 3, jvm_Int);           /* iload_3 */

  bc_push_int_const(method, 0);                   /* iconst_0 */
  bc_gen_store_op(method, 4, jvm_Int);          /* istore */
  bc_gen_load_op(method, 4, jvm_Int);           /* iload */

  /* Storing/Loading integer local variables */

  bc_push_long_const(method, 0);                  /* lconst_0 */
  bc_gen_store_op(method, 0, jvm_Long);         /* lstore_0 */
  bc_gen_load_op(method, 0, jvm_Long);          /* lload_0 */

  bc_push_long_const(method, 0);                  /* lconst_0 */
  bc_gen_store_op(method, 1, jvm_Long);         /* lstore_1 */
  bc_gen_load_op(method, 1, jvm_Long);          /* lload_1 */

  bc_push_long_const(method, 0);                  /* lconst_0 */
  bc_gen_store_op(method, 2, jvm_Long);         /* lstore_2 */
  bc_gen_load_op(method, 2, jvm_Long);          /* lload_2 */

  bc_push_long_const(method, 0);                  /* lconst_0 */
  bc_gen_store_op(method, 3, jvm_Long);         /* lstore_3 */
  bc_gen_load_op(method, 3, jvm_Long);          /* lload_3 */

  bc_push_long_const(method, 0);                  /* lconst_0 */
  bc_gen_store_op(method, 4, jvm_Long);         /* lstore */
  bc_gen_load_op(method, 4, jvm_Long);          /* lload */

  /* Storing/Loading float local variables */

  bc_push_float_const(method, 0.0);               /* fconst_0 */
  bc_gen_store_op(method, 0, jvm_Float);        /* fstore_0 */
  bc_gen_load_op(method, 0, jvm_Float);         /* fload_0 */

  bc_push_float_const(method, 0.0);               /* fconst_0 */
  bc_gen_store_op(method, 1, jvm_Float);        /* fstore_1 */
  bc_gen_load_op(method, 1, jvm_Float);         /* fload_1 */
  
  bc_push_float_const(method, 0.0);               /* fconst_0 */
  bc_gen_store_op(method, 2, jvm_Float);        /* fstore_2 */
  bc_gen_load_op(method, 2, jvm_Float);         /* fload_2 */

  bc_push_float_const(method, 0.0);               /* fconst_0 */
  bc_gen_store_op(method, 3, jvm_Float);        /* fstore_3 */
  bc_gen_load_op(method, 3, jvm_Float);         /* fload_3 */

  bc_push_float_const(method, 0.0);               /* fconst_0 */
  bc_gen_store_op(method, 4, jvm_Float);        /* fstore */
  bc_gen_load_op(method, 4, jvm_Float);         /* fload */

  /* Storing/Loading double local variables */

  bc_push_double_const(method, 0.0);              /* dconst_0 */
  bc_gen_store_op(method, 0, jvm_Double);       /* dstore_0 */
  bc_gen_load_op(method, 0, jvm_Double);        /* dload_0 */

  bc_push_double_const(method, 0.0);              /* dconst_0 */
  bc_gen_store_op(method, 1, jvm_Double);       /* dstore_1 */
  bc_gen_load_op(method, 1, jvm_Double);        /* dload_1 */

  bc_push_double_const(method, 0.0);              /* dconst_0 */
  bc_gen_store_op(method, 2, jvm_Double);       /* dstore_2 */
  bc_gen_load_op(method, 2, jvm_Double);        /* dload_2 */

  bc_push_double_const(method, 0.0);              /* dconst_0 */
  bc_gen_store_op(method, 3, jvm_Double);       /* dstore_3 */
  bc_gen_load_op(method, 3, jvm_Double);        /* dload_3 */

  bc_push_double_const(method, 0.0);              /* dconst_0 */
  bc_gen_store_op(method, 4, jvm_Double);       /* dstore */
  bc_gen_load_op(method, 4, jvm_Double);        /* dload */

  /* Storing/Loading object local variables */

  bc_push_null_const(method);                     /* aconst_null */
  bc_gen_store_op(method, 0, jvm_Object);       /* astore_0 */
  bc_gen_load_op(method, 0, jvm_Object);        /* aload_0 */

  bc_push_null_const(method);                     /* aconst_null */
  bc_gen_store_op(method, 1, jvm_Object);       /* astore_1 */
  bc_gen_load_op(method, 1, jvm_Object);        /* aload_1 */

  bc_push_null_const(method);                     /* aconst_null */
  bc_gen_store_op(method, 2, jvm_Object);       /* astore_2 */
  bc_gen_load_op(method, 2, jvm_Object);        /* aload_2 */

  bc_push_null_const(method);                     /* aconst_null */
  bc_gen_store_op(method, 3, jvm_Object);       /* astore_3 */
  bc_gen_load_op(method, 3, jvm_Object);        /* aload_3 */

  bc_push_null_const(method);                     /* aconst_null */
  bc_gen_store_op(method, 4, jvm_Object);       /* astore */
  bc_gen_load_op(method, 4, jvm_Object);        /* aload */

  /* Array store/load instructions */

  bc_gen_new_array(method, 10, jvm_Byte);
  bc_append(method, jvm_dup);       /* dup for load */
  bc_push_int_const(method, 0);
  bc_push_int_const(method, 22);
  bc_gen_array_store_op(method, jvm_Byte);
  bc_push_int_const(method, 0);
  bc_gen_array_load_op(method, jvm_Byte);

  bc_gen_new_array(method, 10, jvm_Short);
  bc_append(method, jvm_dup);       /* dup for load */
  bc_push_int_const(method, 0);
  bc_push_int_const(method, 22);
  bc_gen_array_store_op(method, jvm_Short);
  bc_push_int_const(method, 0);
  bc_gen_array_load_op(method, jvm_Short);

  bc_gen_new_array(method, 10, jvm_Int);
  bc_append(method, jvm_dup);       /* dup for load */
  bc_push_int_const(method, 0);
  bc_push_int_const(method, 22);
  bc_gen_array_store_op(method, jvm_Int);
  bc_push_int_const(method, 0);
  bc_gen_array_load_op(method, jvm_Int);

  bc_gen_new_array(method, 10, jvm_Long);
  bc_append(method, jvm_dup);       /* dup for load */
  bc_push_int_const(method, 0);
  bc_push_long_const(method, 22);
  bc_gen_array_store_op(method, jvm_Long);
  bc_push_int_const(method, 0);
  bc_gen_array_load_op(method, jvm_Long);

  bc_gen_new_array(method, 10, jvm_Char);
  bc_append(method, jvm_dup);       /* dup for load */
  bc_push_int_const(method, 0);
  bc_push_int_const(method, 22);
  bc_gen_array_store_op(method, jvm_Char);
  bc_push_int_const(method, 0);
  bc_gen_array_load_op(method, jvm_Char);

  bc_gen_new_array(method, 10, jvm_Float);
  bc_append(method, jvm_dup);       /* dup for load */
  bc_push_int_const(method, 0);
  bc_push_float_const(method, 22.2);
  bc_gen_array_store_op(method, jvm_Float);
  bc_push_int_const(method, 0);
  bc_gen_array_load_op(method, jvm_Float);

  bc_gen_new_array(method, 10, jvm_Double);
  bc_append(method, jvm_dup);       /* dup for load */
  bc_push_int_const(method, 0);
  bc_push_double_const(method, 22.2);
  bc_gen_array_store_op(method, jvm_Double);
  bc_push_int_const(method, 0);
  bc_gen_array_load_op(method, jvm_Double);

  bc_gen_new_object_array(method, 10, "java.lang.String");
  bc_append(method, jvm_dup);       /* dup for load */
  bc_push_int_const(method, 0);
  bc_push_null_const(method);
  bc_gen_array_store_op(method, jvm_Object);
  bc_push_int_const(method, 0);
  bc_gen_array_load_op(method, jvm_Object);

  /* Testing whether wide instruction is generated for large local var nums */

  bc_push_int_const(method, 0);                     /* iconst_0 */
  bc_gen_store_op(method, 300, jvm_Int);          /* istore */
  bc_gen_load_op(method, 300, jvm_Int);           /* iload */

  bc_push_long_const(method, 0);                    /* lconst_0 */
  bc_gen_store_op(method, 300, jvm_Long);         /* lstore */
  bc_gen_load_op(method, 300, jvm_Long);          /* lload */

  bc_push_float_const(method, 0.0);                 /* fconst_0 */
  bc_gen_store_op(method, 300, jvm_Float);        /* fstore */
  bc_gen_load_op(method, 300, jvm_Float);         /* fload */

  bc_push_double_const(method, 0.0);                /* dconst_0 */
  bc_gen_store_op(method, 300, jvm_Double);       /* dstore */
  bc_gen_load_op(method, 300, jvm_Double);        /* dload */

  bc_push_null_const(method);                       /* aconst_null */
  bc_gen_store_op(method, 300, jvm_Object);       /* astore */
  bc_gen_load_op(method, 300, jvm_Object);        /* aload */
  
  bc_gen_return(method);

  bc_write_class(cur_class_file, ".");
  bc_free_class(cur_class_file);

  return 0;
}
