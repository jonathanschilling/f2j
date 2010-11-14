      program strdec
      character blah(10)
      character*10 asdf(2)
      character x
      blah(2) = 'b'
*     call foo(blah)
      x = 'x'
      asdf(1) = 'foo'
      asdf(2) = 'bar'
      write(*,*) asdf(1),asdf(2)
      call bar(x,asdf)
      call bar(blah(2),asdf)
      stop
      end
*
*     SUBROUTINE foo(EI)
*     CHARACTER          EI( * )
*     write(*,*) 'foo ', EI(1)
*     return
*     end
*
      SUBROUTINE bar(a,k)
      character*10 k(*)
      character a
*     character*2 b
*     character*(*) c
*     character d*5
*     character e*(*)
*     character*2 f*5
*     character*(*) g*(*)
*     character h(3)*5
*     character*2 j(3)*5
      write(*,*) a, k(1), k(2)
      return
      end
