      program test14
      double precision eps
      eps = 1.1e-16
      WRITE( *, FMT = 9991 )'precision', EPS
      write(*,*) 'no format, eps = ', eps
*
 9991 FORMAT( ' Relative machine ', A, ' is taken to be', D16.6 )
      end
