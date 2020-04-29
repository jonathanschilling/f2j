      program test7
      double precision arr(10)
      integer nnb, sd(8), crap(4,4)
      data sd /4,3,2,1,9,8,7,6/
      nnb =10
      do 10 i = 1, nnb
        arr(i) = dble(2*i)
  10  continue
      cnt = 1
      do 30 i = 1, 4
      do 20 j = 1, 4
        crap(i,j) = cnt
        cnt = cnt +1
  20  continue
  30  continue
*
      call foo7(1234, 9, nnb, arr, sd)
      call foo7(1234, 9, nnb, arr, sd(5))
      call foo7(1234, 9, nnb, arr, crap)
*
      stop
 9993 FORMAT( 3I4 )
      end
*
      subroutine foo7(n, imat, ntests, result, iseed)
      integer crap
      parameter (crap = 3-1)
      integer imat, iseed(1+1,crap)
      DOUBLE PRECISION   RESULT( * )
*
      integer k
      k = 3
*
      WRITE( *, FMT = 9999 )N, IMAT, ISEED, K,
     $  RESULT( K )
*
 9999             FORMAT( ' Matrix order=', I5, ', type=', I2,
     $                  ', seed=', 4( I4, ',' ), ' result ', I3, ' is',
     $                  0P, F8.2 )
      return
      end
