      program test15
      double precision y
      CHARACTER ICOL( 3 ), jCOL( 3 ), kCOL( 3 )
      character*3 crap(3)
      data crap(1), crap(2), crap(3) /'foo', 'bar', 'gah'/
      DATA ICOL( 1 ), ICOL( 2 ), ICOL( 3 ) / 'C', 'o', 'l' /
      DATA jCOL( 1 ), jCOL( 2 ), jCOL( 3 ) / 'X', 'y', 'z' /
      DATA kCOL( 1 ), kCOL( 2 ), kCOL( 3 ) / 'B', 'a', 'r' /
      character*3 asd,jkl,qwe
      data asd/'asd'/
      data jkl/'jkl'/
      data qwe/'qwe'/
*
      n = 33
      y = 9.66d0
*     write(*,fmt=9993) n
*     write(*,fmt=9993) y
      write(*,fmt=9994) asd
      write(*,fmt=9994) icol
      write(*,fmt=9994) crap
      write(*,fmt=9995) asd, jkl, qwe, n
      write(*,fmt=9995) icol, jcol, kcol, n
      write(*,fmt=9996) icol, n
      write(*,fmt=9996) icol(1), icol(2), icol(3), n
      WRITE( *, FMT = 9997 )( ICOL, I, I = 1, 3 )
*     WRITE( *, FMT = 9997 )( ICOL, I, I = K1, K2 )
*
 9993 format(a3)
 9994 format(a3)
 9995 FORMAT( 3A3, I4 )
 9996 FORMAT( 3A1, I4 )
 9997 FORMAT( 10X, 8( 5X, 3A1, I4, 2X ) )
      end
