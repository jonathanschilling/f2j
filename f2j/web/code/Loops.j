;  Produced by f2jas.  f2jas is part of the Fortran-
;  -to-Java project at the University of Tennessee Netlib
;  numerical software repository.
;  David M. Doolin, doolin@cs.utk.edu


; Conventions:
;	1. S_label<n> refers to a label n from the Fortran source.
;	2. Variable names, constants and operators from the Fortran
;	   are listed when possible as comments to each instruction.
;	3. Jasmin directives start with a `.' and are not indented.

.class public Loops
.super java/lang/Object

; The instance initialization method.
.method public <init>()V
   ;  Just call the initializer for Object.
   aload_0
   invokespecial java/lang/Object/<init>()V
   return
.end method


.method public static loops(II)V

.limit stack 5
.limit locals 5


; do loop.
; Initialize counter.
   ldc 1	; 1
   istore 4	; = j
   goto Label4

Label1:
; Executable statements.

; do loop.
; Initialize counter.
   ldc 1	; 1
   istore 3	; = i
   goto Label3

Label2:
; Executable statements.
   iload 1	; z
   ldc 1	; 1
   isub		; -
   istore 2	; = y

; Increment counter.
   iinc 3 1	; Increment counter i.

Label3:
; Compare, jump to Label2 to iterate.
   ldc 10	; 10
   iload 3	; i
   if_icmplt Label2

; Increment counter.
   iinc 4 1	; Increment counter j.

Label4:
; Compare, jump to Label1 to iterate.
   ldc 100	; 100
   iload 4	; j
   if_icmplt Label1

; do loop.
; Initialize counter.
   ldc 1	; 1
   istore 4	; = j
   goto Label6

Label5:
; Executable statements.
   iload 2	; y
   ldc 1	; 1
   iadd		; +
   istore 2	; = y
   goto S_label40

; Increment counter.
   iinc 4 1	; Increment counter j.

Label6:
; Compare, jump to Label5 to iterate.
   ldc 20	; 20
   iload 4	; j
   if_icmplt Label5

S_label40:
   iload 2	; y
   ldc 8	; 8
   idiv		; /
   istore 1	; = z
   return

.end method
