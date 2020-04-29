      program foo
*
      INTEGER NIN
      PARAMETER          ( NIN = 5 )
      INTEGER            LDA
      PARAMETER          ( LDA = 20 )
*     ..
*     .. Local Scalars ..
      INTEGER            I, IHI, IHIIN, ILO, ILOIN, INFO, J, KNT, N,
     $                   NINFO, KK
      REAL               ANORM, MEPS, RMAX, SFMIN, TEMP, VMAX
*     ..
*     .. Local Arrays ..
      INTEGER            LMAX( 3 )
      REAL               A( LDA, LDA ), AIN( LDA, LDA ), DUMMY( 1 ),
     $                   SCALE( LDA ), SCALIN( LDA )
*
  10  continue
      READ( NIN, FMT = * )N
      write (*,*) 'n = ', n
      IF( N.EQ.0 )
     $   GO TO 70
      write(*,*) '1'
      DO 20 I = 1, N
         READ( NIN, FMT = *,IOSTAT=KK,END=80,ERR=80 )
     +     ( A( I, J ), J = 1, N )
   20 CONTINUE
      write(*,*) '2'
*
      READ( NIN, FMT = * )ILOIN, IHIIN
      write(*,*) '3'
      DO 30 I = 1, N
      write(*,*) '3.1'
         READ( NIN, FMT = *,IOSTAT=KK,END=80,ERR=80 )
     +     ( AIN( I, J ), J = 1, N )
      write(*,*) '3.2'
   30 CONTINUE
      write(*,*) '4'
      READ( NIN, FMT = *,IOSTAT=KK,END=80,ERR=80 )
     +     ( SCALIN( I ), I = 1, N )
      write(*,*) '5'
*
      go to 10
   70 write(*,*) 'done'
      stop
   80 write(*,*) 'terminate on error'
      end
