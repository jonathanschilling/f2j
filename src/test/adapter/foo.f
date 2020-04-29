      program foo
      integer x
      call crap(x)
      write(*,*) 'done'
      end
*
      subroutine crap(a)
      integer a, x
      double precision y
      logical z
      real arr(20)
*
      call blah(x, y, arr, z)
*
      return
      end
*
      subroutine blah(a, b, bar, c)
      double precision bar(10)
      integer a
      double precision b
      logical c
*
      bar(3) = 3.5d0
*
      return
      end
