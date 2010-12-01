      external foo
      call foo(666)
      end
      subroutine foo(x)
      integer x
      write(*,*) 'x = ', x
      return
      end
