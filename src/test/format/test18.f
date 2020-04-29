      program test18
      double precision x(4), y
      integer i,j
*
      do 10 i=1,4
        x(i) = 12.3d0 + i
 10   continue
      j = 12
      y = 666.0d0
*
      write(*, fmt=9993) j,  ( x( I ), I = 1, 4 )
      write(*,*) '============='
      write(*, fmt=9994) j,  ( x( I ), I = 1, 4 )
      write(*,*) '============='
      write(*, fmt=9995) j,  ( x( I ), I = 1, 4 )
*
 9993 FORMAT( 1X, ' Row', I4, ':', 1X, 1P 4D14.5 )
 9994 FORMAT( 1X, ' Row', I4, ':', 1X, 1P4D14.5 )
 9995 FORMAT( 1X, ' Row', I4, ':', 1X, 1P, 4D14.5 )
      end
