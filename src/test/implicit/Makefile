runtest: array_common array_main call_sub call_sub2 casting commonblock \
	commontest data_stmt dim foo imfc implicit_function_args implicit_function_call \
	implicit_subroutine_call intrinsic_call intrinsic_named_array intrinsic_named_var \
	local parameter wrap  wrap_func1 implicit_function_cast  passing_functions

array_common:
	f2java array_common.f
	java Array_common > array_common.bc.out
	javac *.java
	javab *.class
	java Array_common > array_common.java.out
	rm -f *.f2j *.class *.java debug *.old

array_main:
	f2java array_sub1.f
	f2java array_main.f
	java Array_common > array_main.bc.out
	javac *.java
	javab *.class
	java Array_common> array_main.java.out
	rm -f *.f2j *.class *.java debug *.old

call_sub:
	f2java call_sub.f
	java C_sub > call_sub.bc.out
	javac *.java
	javab *.class
	java C_sub > call_sub.java.out
	rm -f *.f2j *.class *.java debug *.old

call_sub2:
	f2java call_sub2.f
	javac *.java
	javab *.class
	rm -f *.f2j *.class *.java debug *.old

casting:
	f2java casting.f
	java Small > casting.bc.out
	javac *.java
	javab *.class
	java Small > casting.java.out
	rm -f *.f2j *.class *.java debug *.old

commonblock:
	f2java commonblock.f
	java Commonblock > commonblock.bc.out
	javac *.java
	javab *.class
	java Commonblock > commonblock.java.out
	rm -f *.f2j *.class *.java debug *.old

commontest:
	f2java commontest.f
	java Commontest > commontest.bc.out
	javac *.java
	javab *.class
	java Commontest > commontest.java.out
	rm -f *.f2j *.class *.java debug *.old

data_stmt:
	f2java data_stmt.f
	java Data_stmt > data_stmt.bc.out
	javac *.java
	javab *.class
	java Data_stmt > data_stmt.java.out
	rm -f *.f2j *.class *.java debug *.old

dim:
	f2java dim.f
	java Dim > dim.bc.out
	javac *.java
	javab *.class
	java Dim > dim.java.out
	rm -f *.f2j *.class *.java debug *.old

foo:
	f2java foo.f
	java Asdf > foo.bc.out
	javac *.java
	javab *.class
	java Asdf > foo.java.out
	rm -f *.f2j *.class *.java debug *.old

imfc:
	f2java imfc_func1.f
	f2java imfc_func2.f
	f2java imfc_main.f
	java Rain > imfc.bc.out
	javac *.java
	javab *.class 
	java Rain > imfc.java.out
	rm -f *.f2j *.class *.java debug *.old

implicit_function_args:
	f2java implicit_function_args.f
	java Rain > implicit_function_args.bc.out
	javac *.java
	javab *.class
	java Rain > implicit_function_args.java.out
	rm -f *.f2j *.class *.java debug *.old

implicit_function_call:
	f2java implicit_function_call.f
	java Rain > implicit_function_call.bc.out
	javac *.java
	javab *.class
	java Rain > implicit_function_call.java.out
	rm -f *.f2j *.class *.java debug *.old

implicit_subroutine_call:
	f2java implicit_subroutine_call.f 
	java Implicit_sub > implicit_subroutine_call.bc.out
	javac *.java
	javab *.class
	java Implicit_sub > implicit_subroutine_call.java.out
	rm -f *.f2j *.class *.java debug *.old

intrinsic_call:
	f2java intrinsic_call.f
	java Intrinsic_call > intrinsic_call.bc.out
	javac *.java
	javab *.class
	java Intrinsic_call > intrinsic_call.java.out
	rm -f *.f2j *.class *.java debug *.old

intrinsic_named_array:
	f2java intrinsic_named_array.f
	java Intrinsic_named_array > intrinsic_named_array.bc.out
	javac *.java
	javab *.class
	java Intrinsic_named_array > intrinsic_named_array.java.out
	rm -f *.f2j *.class *.java debug *.old

intrinsic_named_var:
	f2java intrinsic_named_var.f
	java Intrinsic_named_var > intrinsic_named_var.bc.out
	javac *.java
	javab *.class
	java Intrinsic_named_var > intrinsic_named_var.java.out
	rm -f *.f2j *.class *.java debug *.old

local:	
	f2java local.f
	java Local > local.bc.out
	javac *.java
	javab *.class
	java Local > local.java.out
	rm -f *.f2j *.class *.java debug *.old

parameter:
	f2java parameter.f
	java Circle > parameter.bc.out
	javac *.java
	javab *.class
	java Circle > parameter.java.out
	rm -f *.f2j *.class *.java debug *.old

wrap:
	f2java wrap.f
	java Rain > wrap.bc.out
	javac *.java
	javab *.class
	java Rain > wrap.java.out
	rm -f *.f2j *.class *.java debug *.old

wrap_func1:
	f2java wrap_func1.f
	f2java wrap_main.f
	java Test > wrap_func1.bc.out
	javac *.java
	javab *.class
	java Test > wrap_func1.bc.out
	rm -f *.f2j *.class *.java debug *.old

implicit_function_cast:
	f2java implicit_function_cast.f
	java Implicit_function_cast > implicit_function_cast.bc.out
	javac *.java
	javab *.class
	java Implicit_function_cast > implicit_function_cast.java.out
	rm -f *.f2j *.class *.java debug *.old

passing_functions:
	f2java passing_functions.f
	java Passing_functions > passing_functions.bc.out
	javac *.java
	javab *.class
	java Passing_functions > passing_functions.java.out
	rm -f *.f2j *.class *.java debug *.old

clean:
	rm -f *.f2j *.class *.java debug *.old a.out
