      program test
      external foo, bar
      integer bar
      integer x(10)
*
      x(5)=3
*
      call foo(x(5))
      x(6)=bar(x(5))
*
      end
*     integer function bar(y)
*     integer y(5)
*
*     write(*,*) 'hi'
*
*     bar = 12
*     return
*     end
