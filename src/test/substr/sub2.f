      program sub2
      character x
*
      x = 'z'
      write(*,*) x
      x(1:) = 'A'
*
      write(*,*) x
      x(1:) = '#'
      write(*,*) x
*
      stop
      end
