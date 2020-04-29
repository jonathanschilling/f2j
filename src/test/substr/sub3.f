      program sub3
      character x(10)
      integer i
*
      do 10 i=1,10
        x(i) = '_'
 10   continue
      x(4) = 'z'
      write(*,*) x
      x(4) = 'A'
*
      write(*,*) x
      x(4) = '#'
      write(*,*) x
*
      stop
      end
