      program crap
      integer x,y
      integer dlaneg
      external dlaneg
*
      x = 12
      y = dlaneg(x)
*
      write(*,*) 'y = ', y
*
      end
*
      function dlaneg(n)
      implicit none
      integer a,b,c
      data a/3/
      intrinsic sqrt
      integer d,e,f,g,dlaneg,h,i,j
      integer n
*
      write(*,*) 'foo..',n
*
      dlaneg = n+2
      return
      end
