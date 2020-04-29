      program crap
      character*6 x
      character*2 y
*
      x='abcdef'
      y='xy'
*
      write(*,*) x
      x(3:3) = y
*
      write(*,*) x
*
      stop
      end
