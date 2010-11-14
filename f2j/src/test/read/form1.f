      program form1
      CHARACTER*10   NAME
      REAL SCORE1, SCORE2
*
      read(*,100) NAME, SCORE1, SCORE2
      write(*,150) NAME
      stop
 100  format(A10,1x,f6.1,1x,f6.2)
 150  format(1x,'Last name = ', A10)
      end
