      program simple
*
      INTEGER NIN
      PARAMETER          ( NIN = 5 )
      INTEGER            LDA
      PARAMETER          ( LDA = 20 )
*     ..
      INTEGER            I, J, N
      REAL               A( LDA, LDA )
*
      n = 5
      do 10 i=1,3
        READ( NIN, FMT = * )( A( i, J ), J = 1, N )
  10  continue
      do 20 i=1,3
        WRITE( *, * )( A( i, J ), J = 1, N )
  20  continue
      stop
      end
