      program test4
      integer nnb
*
      WRITE(*, FMT = 9993 ) 'foo'
      WRITE(*, FMT = 9994 ) 'foo'
*
      stop
 9993 FORMAT( A3 )
 9994 FORMAT( A )
      end
