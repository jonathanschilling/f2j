      program crap
      integer x
      x = 23
      write(*,*) x
      if (x .gt. 100) then
        stop 'foo'
      else
        stop 111
      endif
      end
