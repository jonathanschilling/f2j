runtest: array_common array_main call_sub call_sub2 casting commonblock \
	commontest data_stmt dim foo imfc implicit_function_args implicit_function_call \
	implicit_subroutine_call intrinsic_call intrinsic_named_array intrinsic_named_var \
	local parameter wrap  wrap_func1 implicit_function_cast  passing_functions

array_common:
	f2java array_common.f
	java Array_common
	javac *.java
	javab *.class
	java Array_common
	rm -f *.f2j *.class *.java debug *.old

array_main:
	f2java array_sub1.f
	f2java array_main.f
	java Array_common 
	javac *.java
	javab *.class
	java Array_common
	rm -f *.f2j *.class *.java debug *.old

call_sub:
	f2java call_sub.f
	java C_sub
	javac *.java
	javab *.class
	java C_sub
	rm -f *.f2j *.class *.java debug *.old

call_sub2:
	f2java call_sub2.f
	javac *.java
	javab *.class
	rm -f *.f2j *.class *.java debug *.old

casting:
	f2java casting.f
	java Small
	javac *.java
	javab *.class
	java Small
	rm -f *.f2j *.class *.java debug *.old

commonblock:
	f2java commonblock.f
	java Commonblock
	javac *.java
	javab *.class
	java Commonblock
	rm -f *.f2j *.class *.java debug *.old

commontest:
	f2java commontest.f
	java Commontest
	javac *.java
	javab *.class
	java Commontest
	rm -f *.f2j *.class *.java debug *.old

data_stmt:
	f2java data_stmt.f
	java Data_stmt
	javac *.java
	javab *.class
	java Data_stmt
	rm -f *.f2j *.class *.java debug *.old

dim:
	f2java dim.f
	java Dim
	javac *.java
	javab *.class
	java Dim
	rm -f *.f2j *.class *.java debug *.old

foo:
	f2java foo.f
	java Asdf
	javac *.java
	javab *.class
	java Asdf
	rm -f *.f2j *.class *.java debug *.old

imfc:
	f2java imfc_func1.f
	f2java imfc_func2.f
	f2java imfc_main.f
	java Rain
	javac *.java
	javab *.class 
	java Rain
	rm -f *.f2j *.class *.java debug *.old

implicit_function_args:
	f2java implicit_function_args.f
	java Rain
	javac *.java
	javab *.class
	java Rain
	rm -f *.f2j *.class *.java debug *.old

implicit_function_call:
	f2java implicit_function_call.f
	java Rain
	javac *.java
	javab *.class
	java Rain
	rm -f *.f2j *.class *.java debug *.old

implicit_subroutine_call:
	f2java implicit_subroutine_call.f 
	java Implicit_sub
	javac *.java
	javab *.class
	java Implicit_sub
	rm -f *.f2j *.class *.java debug *.old

intrinsic_call:
	f2java intrinsic_call.f
	java Intrinsic_call
	javac *.java
	javab *.class
	java Intrinsic_call
	rm -f *.f2j *.class *.java debug *.old

intrinsic_named_array:
	f2java intrinsic_named_array.f
	java Intrinsic_named_array
	javac *.java
	javab *.class
	java Intrinsic_named_array
	rm -f *.f2j *.class *.java debug *.old

intrinsic_named_var:
	f2java intrinsic_named_var.f
	java Intrinsic_named_var
	javac *.java
	javab *.class
	java Intrinsic_named_var
	rm -f *.f2j *.class *.java debug *.old

local:	
	f2java local.f
	java Local
	javac *.java
	javab *.class
	java Local
	rm -f *.f2j *.class *.java debug *.old

parameter:
	f2java parameter.f
	java Circle
	javac *.java
	javab *.class
	java Circle
	rm -f *.f2j *.class *.java debug *.old

wrap:
	f2java wrap.f
	java Rain
	javac *.java
	javab *.class
	java Rain
	rm -f *.f2j *.class *.java debug *.old

wrap_func1:
	f2java wrap_func1.f
	f2java wrap_main.f
	java Test
	javac *.java
	javab *.class
	java Test
	rm -f *.f2j *.class *.java debug *.old

implicit_function_cast:
	f2java implicit_function_cast.f
	java Implicit_function_cast 
	javac *.java
	javab *.class
	java Implicit_function_cast 
	rm -f *.f2j *.class *.java debug *.old

passing_functions:
	f2java passing_functions.f
	java Passing_functions
	javac *.java
	javab *.class
	java Passing_functions
	rm -f *.f2j *.class *.java debug *.old

clean:
	rm -f *.f2j *.class *.java debug *.old a.out
