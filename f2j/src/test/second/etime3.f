      program etimetest3
      real elapsed(2)     ! For receiving user and system time
      real total
      real crap(10)
      call foo(elapsed, total)
      call foo(elapsed, crap(4))
      print *, 'End: total=', total, ' user=', elapsed(1),
     &         ' system=', elapsed(2), ' ignore this: ',
     &         j
      print *, 'crap = ', crap(4)
      end
      subroutine foo(el, xt)
      real el(2)
      real xt          ! For receiving total time
      integer i, j

      print *, 'Start'

      j = 0
      do i = 1, 50000000   ! That's fifty million
         j = j + 1
      end do

      call etime(el, xt)
      return
      end
