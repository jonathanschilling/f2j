      program form3
      real x,y
      integer z
c
      x = 12.3
      y = 98.7
      z = 666
c
      write(*,100) x,y,z
      stop
 100  format (1x, F7.2, T12, F7.1, T24, I6)
      end
