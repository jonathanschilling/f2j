      program form6
      integer x,y
c
      x = 1234
      y = 666
c
      write(*,*) x,y
      write(*, '(2I6)') x,y
      write(FMT='(2I6)',UNIT=*) x,y
c
      stop
      end
