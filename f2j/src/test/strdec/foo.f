      program foo
      integer x
      external crap
      write(*,*) 'hi'
      call crap(2)
      stop
      end
*
      subroutine crap(n)
      character asdf(n)
*     integer asdf(n)
*
*     write(*,*) 'cxrap', asdf
      write(*,*) 'cxrap'
*
      return
      end
