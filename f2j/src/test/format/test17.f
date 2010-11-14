      program test17
      integer a,b,c,d,e,f
*
      a = 1
      b = 2
      c = 3
      d = 4
      e = 5
      f = 6
      write(*,fmt=996) a,b,c,d,e,f
      write(*,*) '=========================='
      write(*,fmt=997) a,b,c,d,e,f
      write(*,*) '=========================='
      write(*,fmt=998) a,b,c,d,e,f
      write(*,*) '=========================='
      write(*,fmt=999) a,b,c,d,e,f
*
  996 format(2(I6, I6, I6))
  997 format(I6, I6, I6, I6, I6, I6)
  998 format(I6, I6, I6)
  999 format(I6, I6)
      end
