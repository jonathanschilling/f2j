      program strdec
      character*10 blah
      blah = 'foo'
      call foo(blah)
      stop
      end
*
      SUBROUTINE foo(EI)
      CHARACTER  c, EI( * )
      c = ei(2)
*     write(*,*) 'foo =', EI(2)
      write(*,*) 'foo =', c
      return
      end
