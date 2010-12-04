      program simple3
      real x,y
      x= 9.2
      y= 83.5
      write(*,100) x
      write(*,100) y
      write(*,200) x
      write(*,200) y
      write(*,300) x
      write(*,300) y
 100  format(F2.0)
 200  format(F3.1)
 300  format(F2.1)
      end
