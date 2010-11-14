* tests write() with no args, one formatted and one not
      program test1
      PARAMETER          ( NINMAX = 7 )
      integer x, ninc, INC( NINMAX )
      logical l
      real rmax
      data inc/7,6,5,4,3,2,1/
*
      rmax = 0.000000
      ninc = 4
      WRITE( *, FMT = 9998 )RMAX
      WRITE( *, FMT = 9990 )( INC( I ), I = 1, NINC )
*
      write(*, 996) 'hello'
      write(*, 998)
      write(*, *) 'hello'
      x = 12
      write(*, 999) x
      l = .true.
      write(*, 997) l
*
      stop
 9990 FORMAT( '   FOR INCX AND INCY  ', 7I6 )
 9998 FORMAT( 1X, 'value of largest test error             = ', E12.3 )
  996 format (a3)
  997 FORMAT (l3)
  998 FORMAT (' ''foo''')
  999 format (I6)
      end
