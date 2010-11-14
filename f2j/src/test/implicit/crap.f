      program crap
      IMPLICIT REAL*8 (A-H,O-Z)
      external blah
      a = blah(1.D0)
      write(*,*)'a=',a
      end
*
      function blah(x)
      IMPLICIT REAL*8 (A-H,O-Z)
*
      blah = x* 2.D0
*
      return
      end
