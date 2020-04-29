      program test5
      integer arr(10)
      integer nnb
      nnb =10 
      do 10 i = 1, nnb
        arr(i) = i*3
  10  continue
*
      WRITE(*, FMT = 9993 ) arr(3)
*
      stop
 9993 FORMAT( 3I4 )
      end
