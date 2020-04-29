* tests write() with one implied loop argument
      program test2
      integer nbval(5), nnb
      nnb=5
      do 10 i=1,5
        nbval(i) = 3+i
  10  continue
*
      WRITE(*, FMT = 9993 ) ( NBVAL( I ), I = 1, NNB )
      WRITE(*, FMT = 9993 ) ( 7, I = 1, NNB )
*
      stop
 9993 FORMAT( 4X, ':  ', 2I6, / 11X, 3I6 )
      end
