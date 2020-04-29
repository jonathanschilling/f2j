      program test6
      double precision arr(10)
      integer nnb, sd(8)
      data sd /4,3,2,1,9,8,7,6/
      nnb =10
      do 10 i = 1, nnb
        arr(i) = dble(2*i)
  10  continue
*
      call AsDfG(1234, 9, nnb, arr, sd(5))
*
      stop
 9993 FORMAT( 3I4 )
      end
*
      subroutine AsDfG(n, imat, ntests, result, iseed)
      integer imat, iseed(4), ifoo(4)
      DOUBLE PRECISION   RESULT( * )
      data ifoo/12,13,14,15/
*
      integer k
      k = 3
*
      WRITE( *, FMT = 9999 )N, IMAT, ISEED, K,
     $  RESULT( K )
      WRITE( *, FMT = 9999 )N, IMAT, ifoo, K,
     $  RESULT( K )
*     write(*, FMT=9998) result
*
 9998 format(F8.2)
 9999             FORMAT( ' Matrix order=', I5, ', type=', I2,
     $                  ', seed=', 4( I4, ',' ), ' result ', I3, ' is',
     $                  0P, F8.2 )
      return
      end
