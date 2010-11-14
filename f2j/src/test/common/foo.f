      subroutine foo(a,b)
      integer a,b
      integer ARGl, ARG2(10), ARG3, ARG4
      COMMON /ARGMNT/ ARGl, ARG2, ARG3, ARG4
* this is a ! comment
      write(*,*) 'hello from foo'
*
      return
      end
