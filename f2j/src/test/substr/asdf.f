      program asdf
      character*5 x
      integer y,z
*
      x='asdfj'
      y = 2
      z = 4
      write(*,*) x(y:z)
*
      stop
      end
