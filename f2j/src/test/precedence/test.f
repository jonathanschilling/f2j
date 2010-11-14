      program test
      double precision x
*
      x = 12.0d0
      if(.not.x.lt.3.0d0) then
        write(*,*) 'cool'
      else
        write(*,*) 'bad'
      endif
*
      end
