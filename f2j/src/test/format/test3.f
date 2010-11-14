* tests write() with one implied loop argument
      program test3
      double precision nbval(5)
      integer nnb
      nnb=5
      do 10 i=1,5
        nbval(i) = 3.0d0+i
  10  continue
*
      WRITE(*, FMT = 9993 ) ( NBVAL( I ), I = 1, NNB )
      WRITE(*, FMT = 9994 ) ( 7.0d0, I = 1, NNB )
      WRITE(*, FMT = 9995 ) 1234, ( 7.0d0, I = 1, NNB ), 666
*
      stop
 9993 FORMAT( 4X, ':  ', 3D9.1, / 11X, 2D9.1 )
 9994 FORMAT( 4X, ':  ', 3F9.1, / 11X, 2F9.1 )
 9995 FORMAT( I6, 4X, ':  ', 3F9.1, / 11X, 2F9.1, I6 )
      end
