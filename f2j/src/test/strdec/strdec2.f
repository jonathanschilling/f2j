      program strdec
      character*10 asdf(2)
      asdf(1) = 'foo'
      asdf(2) = 'bar'
      write(*,*) asdf(1),asdf(2)
      call bar(asdf)
      stop
      end
*
      SUBROUTINE bar(k)
      character*10 k(*)
      write(*,*) k(1), k(2)
      return
      end
