      program etimetest
      real etime          ! Declare the type of etime()
      real elapsed(2)     ! For receiving user and system time
      real total          ! For receiving total time
      integer i, j

      print *, 'Start'

      j = 0
      do i = 1, 50000000   ! That's fifty million
         j = j + 1
      end do

      total = etime(elapsed)
      print *, 'End: total=', total, ' user=', elapsed(1),
     &         ' system=', elapsed(2), ' ignore this: ',
     &         j
      stop
      end
