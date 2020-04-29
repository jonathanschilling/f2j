      program test
      integer x
      external foo
      x = 12
      call foo(x)
      end
*
      subroutine foo(n)
      integer n
      double precision work(n*3)
*
      write(*,*) 'crap'
*
      return
      end
     
