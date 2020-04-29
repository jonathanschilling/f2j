      program asdf
      integer x
      x = foo()
      call foo_sub()
      write(*,*)'x=',x
      end
      double precision function foo()
      integer dummy
      foo = 666
      return
      end
      subroutine foo_sub()
      integer dummy
      foo = 666
      end
