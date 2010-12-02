      program test2
      integer a(10),b(10),c(9),p
      parameter (p=9)
      data a,b/1234,5*5,10,11,12,p*3,666,777/
      data c/p*12/
*
      write(*,100) (a(i),i=1,10)
      write(*,100) (b(i),i=1,10)
      write(*,100) (c(i),i=1,9)
*
      stop
  100 format(10(I5))
      end
