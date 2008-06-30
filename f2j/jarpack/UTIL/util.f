*-----------------------------------------------------------------------
*  Routine:    DMOUT
*
*  Purpose:    Real matrix output routine.
*
*  Usage:      CALL DMOUT (LOUT, M, N, A, LDA, IDIGIT, IFMT)
*
*  Arguments
*     M      - Number of rows of A.  (Input)
*     N      - Number of columns of A.  (Input)
*     A      - Real M by N matrix to be printed.  (Input)
*     LDA    - Leading dimension of A exactly as specified in the
*              dimension statement of the calling program.  (Input)
*     IFMT   - Format to be used in printing matrix A.  (Input)
*     IDIGIT - Print up to IABS(IDIGIT) decimal digits per number.  (In)
*              If IDIGIT .LT. 0, printing is done with 72 columns.
*              If IDIGIT .GT. 0, printing is done with 132 columns.
*
*-----------------------------------------------------------------------
*
      SUBROUTINE DMOUT( LOUT, M, N, A, LDA, IDIGIT, IFMT )
*     ...
*     ... SPECIFICATIONS FOR ARGUMENTS
*     ...
*     ... SPECIFICATIONS FOR LOCAL VARIABLES
*     .. Scalar Arguments ..
      CHARACTER*( * )    IFMT
      INTEGER            IDIGIT, LDA, LOUT, M, N
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * )
*     ..
*     .. Local Scalars ..
      CHARACTER*80       LINE
      INTEGER            I, J, K1, K2, LLL, NDIGIT
*     ..
*     .. Local Arrays ..
      CHARACTER          ICOL( 3 )
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          LEN, MIN, MIN0
*     ..
*     .. Data statements ..
      DATA               ICOL( 1 ), ICOL( 2 ), ICOL( 3 ) / 'C', 'o',
     $                   'l' /
*     ..
*     .. Executable Statements ..
*     ...
*     ... FIRST EXECUTABLE STATEMENT
*
      LLL = MIN( LEN( IFMT ), 80 )
      DO 10 I = 1, LLL
         LINE( I: I ) = '-'
   10 CONTINUE
*
      DO 20 I = LLL + 1, 80
         LINE( I: I ) = ' '
   20 CONTINUE
*
      WRITE( LOUT, FMT = 9999 )IFMT, LINE( 1: LLL )
 9999 FORMAT( / 1X, A, / 1X, A )
*
      IF( M.LE.0 .OR. N.LE.0 .OR. LDA.LE.0 )
     $   RETURN
      NDIGIT = IDIGIT
      IF( IDIGIT.EQ.0 )
     $   NDIGIT = 4
*
*=======================================================================
*             CODE FOR OUTPUT USING 72 COLUMNS FORMAT
*=======================================================================
*
      IF( IDIGIT.LT.0 ) THEN
         NDIGIT = -IDIGIT
         IF( NDIGIT.LE.4 ) THEN
            DO 40 K1 = 1, N, 5
               K2 = MIN0( N, K1+4 )
               WRITE( LOUT, FMT = 9998 )( ICOL, I, I = K1, K2 )
               DO 30 I = 1, M
                  WRITE( LOUT, FMT = 9994 )I, ( A( I, J ), J = K1, K2 )
   30          CONTINUE
   40       CONTINUE
*
         ELSE IF( NDIGIT.LE.6 ) THEN
            DO 60 K1 = 1, N, 4
               K2 = MIN0( N, K1+3 )
               WRITE( LOUT, FMT = 9997 )( ICOL, I, I = K1, K2 )
               DO 50 I = 1, M
                  WRITE( LOUT, FMT = 9993 )I, ( A( I, J ), J = K1, K2 )
   50          CONTINUE
   60       CONTINUE
*
         ELSE IF( NDIGIT.LE.10 ) THEN
            DO 80 K1 = 1, N, 3
               K2 = MIN0( N, K1+2 )
               WRITE( LOUT, FMT = 9996 )( ICOL, I, I = K1, K2 )
               DO 70 I = 1, M
                  WRITE( LOUT, FMT = 9992 )I, ( A( I, J ), J = K1, K2 )
   70          CONTINUE
   80       CONTINUE
*
         ELSE
            DO 100 K1 = 1, N, 2
               K2 = MIN0( N, K1+1 )
               WRITE( LOUT, FMT = 9995 )( ICOL, I, I = K1, K2 )
               DO 90 I = 1, M
                  WRITE( LOUT, FMT = 9991 )I, ( A( I, J ), J = K1, K2 )
   90          CONTINUE
  100       CONTINUE
         END IF
*
*=======================================================================
*             CODE FOR OUTPUT USING 132 COLUMNS FORMAT
*=======================================================================
*
      ELSE
         IF( NDIGIT.LE.4 ) THEN
            DO 120 K1 = 1, N, 10
               K2 = MIN0( N, K1+9 )
               WRITE( LOUT, FMT = 9998 )( ICOL, I, I = K1, K2 )
               DO 110 I = 1, M
                  WRITE( LOUT, FMT = 9994 )I, ( A( I, J ), J = K1, K2 )
  110          CONTINUE
  120       CONTINUE
*
         ELSE IF( NDIGIT.LE.6 ) THEN
            DO 140 K1 = 1, N, 8
               K2 = MIN0( N, K1+7 )
               WRITE( LOUT, FMT = 9997 )( ICOL, I, I = K1, K2 )
               DO 130 I = 1, M
                  WRITE( LOUT, FMT = 9993 )I, ( A( I, J ), J = K1, K2 )
  130          CONTINUE
  140       CONTINUE
*
         ELSE IF( NDIGIT.LE.10 ) THEN
            DO 160 K1 = 1, N, 6
               K2 = MIN0( N, K1+5 )
               WRITE( LOUT, FMT = 9996 )( ICOL, I, I = K1, K2 )
               DO 150 I = 1, M
                  WRITE( LOUT, FMT = 9992 )I, ( A( I, J ), J = K1, K2 )
  150          CONTINUE
  160       CONTINUE
*
         ELSE
            DO 180 K1 = 1, N, 5
               K2 = MIN0( N, K1+4 )
               WRITE( LOUT, FMT = 9995 )( ICOL, I, I = K1, K2 )
               DO 170 I = 1, M
                  WRITE( LOUT, FMT = 9991 )I, ( A( I, J ), J = K1, K2 )
  170          CONTINUE
  180       CONTINUE
         END IF
      END IF
      WRITE( LOUT, FMT = 9990 )
*
 9998 FORMAT( 10X, 10( 4X, 3A1, I4, 1X ) )
 9997 FORMAT( 10X, 8( 5X, 3A1, I4, 2X ) )
 9996 FORMAT( 10X, 6( 7X, 3A1, I4, 4X ) )
 9995 FORMAT( 10X, 5( 9X, 3A1, I4, 6X ) )
 9994 FORMAT( 1X, ' Row', I4, ':', 1X, 1P, 10D12.3 )
 9993 FORMAT( 1X, ' Row', I4, ':', 1X, 1P, 8D14.5 )
 9992 FORMAT( 1X, ' Row', I4, ':', 1X, 1P, 6D18.9 )
 9991 FORMAT( 1X, ' Row', I4, ':', 1X, 1P, 5D22.13 )
 9990 FORMAT( 1X, ' ' )
*
      RETURN
      END
*-----------------------------------------------------------------------
*  Routine:    DVOUT
*
*  Purpose:    Real vector output routine.
*
*  Usage:      CALL DVOUT (LOUT, N, SX, IDIGIT, IFMT)
*
*  Arguments
*     N      - Length of array SX.  (Input)
*     SX     - Real array to be printed.  (Input)
*     IFMT   - Format to be used in printing array SX.  (Input)
*     IDIGIT - Print up to IABS(IDIGIT) decimal digits per number.  (In)
*              If IDIGIT .LT. 0, printing is done with 72 columns.
*              If IDIGIT .GT. 0, printing is done with 132 columns.
*
*-----------------------------------------------------------------------
*
      SUBROUTINE DVOUT( LOUT, N, SX, IDIGIT, IFMT )
*     ...
*     ... SPECIFICATIONS FOR ARGUMENTS
*     ...
*     ... SPECIFICATIONS FOR LOCAL VARIABLES
*     .. Scalar Arguments ..
      CHARACTER*( * )    IFMT
      INTEGER            IDIGIT, LOUT, N
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   SX( * )
*     ..
*     .. Local Scalars ..
      CHARACTER*80       LINE
      INTEGER            I, K1, K2, LLL, NDIGIT
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          LEN, MIN, MIN0
*     ..
*     .. Executable Statements ..
*     ...
*     ... FIRST EXECUTABLE STATEMENT
*
*
      LLL = MIN( LEN( IFMT ), 80 )
      DO 10 I = 1, LLL
         LINE( I: I ) = '-'
   10 CONTINUE
*
      DO 20 I = LLL + 1, 80
         LINE( I: I ) = ' '
   20 CONTINUE
*
      WRITE( LOUT, FMT = 9999 )IFMT, LINE( 1: LLL )
 9999 FORMAT( / 1X, A, / 1X, A )
*
      IF( N.LE.0 )
     $   RETURN
      NDIGIT = IDIGIT
      IF( IDIGIT.EQ.0 )
     $   NDIGIT = 4
*
*=======================================================================
*             CODE FOR OUTPUT USING 72 COLUMNS FORMAT
*=======================================================================
*
      IF( IDIGIT.LT.0 ) THEN
         NDIGIT = -IDIGIT
         IF( NDIGIT.LE.4 ) THEN
            DO 30 K1 = 1, N, 5
               K2 = MIN0( N, K1+4 )
               WRITE( LOUT, FMT = 9998 )K1, K2, ( SX( I ), I = K1, K2 )
   30       CONTINUE
         ELSE IF( NDIGIT.LE.6 ) THEN
            DO 40 K1 = 1, N, 4
               K2 = MIN0( N, K1+3 )
               WRITE( LOUT, FMT = 9997 )K1, K2, ( SX( I ), I = K1, K2 )
   40       CONTINUE
         ELSE IF( NDIGIT.LE.10 ) THEN
            DO 50 K1 = 1, N, 3
               K2 = MIN0( N, K1+2 )
               WRITE( LOUT, FMT = 9996 )K1, K2, ( SX( I ), I = K1, K2 )
   50       CONTINUE
         ELSE
            DO 60 K1 = 1, N, 2
               K2 = MIN0( N, K1+1 )
               WRITE( LOUT, FMT = 9995 )K1, K2, ( SX( I ), I = K1, K2 )
   60       CONTINUE
         END IF
*
*=======================================================================
*             CODE FOR OUTPUT USING 132 COLUMNS FORMAT
*=======================================================================
*
      ELSE
         IF( NDIGIT.LE.4 ) THEN
            DO 70 K1 = 1, N, 10
               K2 = MIN0( N, K1+9 )
               WRITE( LOUT, FMT = 9998 )K1, K2, ( SX( I ), I = K1, K2 )
   70       CONTINUE
         ELSE IF( NDIGIT.LE.6 ) THEN
            DO 80 K1 = 1, N, 8
               K2 = MIN0( N, K1+7 )
               WRITE( LOUT, FMT = 9997 )K1, K2, ( SX( I ), I = K1, K2 )
   80       CONTINUE
         ELSE IF( NDIGIT.LE.10 ) THEN
            DO 90 K1 = 1, N, 6
               K2 = MIN0( N, K1+5 )
               WRITE( LOUT, FMT = 9996 )K1, K2, ( SX( I ), I = K1, K2 )
   90       CONTINUE
         ELSE
            DO 100 K1 = 1, N, 5
               K2 = MIN0( N, K1+4 )
               WRITE( LOUT, FMT = 9995 )K1, K2, ( SX( I ), I = K1, K2 )
  100       CONTINUE
         END IF
      END IF
      WRITE( LOUT, FMT = 9994 )
      RETURN
 9998 FORMAT( 1X, I4, ' - ', I4, ':', 1P, 10D12.3 )
 9997 FORMAT( 1X, I4, ' - ', I4, ':', 1X, 1P, 8D14.5 )
 9996 FORMAT( 1X, I4, ' - ', I4, ':', 1X, 1P, 6D18.9 )
 9995 FORMAT( 1X, I4, ' - ', I4, ':', 1X, 1P, 5D24.13 )
 9994 FORMAT( 1X, ' ' )
      END
c
c-----------------------------------------------------------------------
c
c     Count the number of elements equal to a specified integer value.
c
      integer function icnteq (n, array, value)
c
      integer    n, value
      integer    array(*)
c
      k = 0
      do 10 i = 1, n
         if (array(i) .eq. value) k = k + 1
   10 continue
      icnteq = k
c
      return
      end
*--------------------------------------------------------------------
*\Documentation
*
*\Name: ICOPY
*
*\Description:
*     ICOPY copies an integer vector lx to an integer vector ly.
*
*\Usage:
*     call icopy ( n, lx, inc, ly, incy )
*
*\Arguments:
*    n        integer (input)
*             On entry, n is the number of elements of lx to be
c             copied to ly.
*
*    lx       integer array (input)
*             On entry, lx is the integer vector to be copied.
*
*    incx     integer (input)
*             On entry, incx is the increment between elements of lx.
*
*    ly       integer array (input)
*             On exit, ly is the integer vector that contains the
*             copy of lx.
*
*    incy     integer (input)
*             On entry, incy is the increment between elements of ly.
*
*\Enddoc
*
*--------------------------------------------------------------------
*
      subroutine icopy( n, lx, incx, ly, incy )
*
*     ----------------------------
*     Specifications for arguments
*     ----------------------------
      integer    incx, incy, n
      integer    lx( 1 ), ly( 1 )
*
*     ----------------------------------
*     Specifications for local variables
*     ----------------------------------
      integer           i, ix, iy
*
*     --------------------------
*     First executable statement
*     --------------------------
      if( n.le.0 )
     $   return
      if( incx.eq.1 .and. incy.eq.1 )
     $   go to 20
c
c.....code for unequal increments or equal increments
c     not equal to 1
      ix = 1
      iy = 1
      if( incx.lt.0 )
     $   ix = ( -n+1 )*incx + 1
      if( incy.lt.0 )
     $   iy = ( -n+1 )*incy + 1
      do 10 i = 1, n
         ly( iy ) = lx( ix )
         ix = ix + incx
         iy = iy + incy
   10 continue
      return
c
c.....code for both increments equal to 1
c
   20 continue
      do 30 i = 1, n
         ly( i ) = lx( i )
   30 continue
      return
      end
c
c-----------------------------------------------------------------------
c
c     Only work with increment equal to 1 right now.
c
      subroutine iset (n, value, array, inc)
c
      integer    n, value, inc
      integer    array(*)
c
      do 10 i = 1, n
         array(i) = value
   10 continue
c
      return
      end
      subroutine iswap (n,sx,incx,sy,incy)
c
c     interchanges two vectors.
c     uses unrolled loops for increments equal to 1.
c     jack dongarra, linpack, 3/11/78.
c
      integer sx(1),sy(1),stemp
      integer i,incx,incy,ix,iy,m,mp1,n
c
      if(n.le.0)return
      if(incx.eq.1.and.incy.eq.1)go to 20
c
c       code for unequal increments or equal increments not equal
c         to 1
c
      ix = 1
      iy = 1
      if(incx.lt.0)ix = (-n+1)*incx + 1
      if(incy.lt.0)iy = (-n+1)*incy + 1
      do 10 i = 1,n
        stemp = sx(ix)
        sx(ix) = sy(iy)
        sy(iy) = stemp
        ix = ix + incx
        iy = iy + incy
   10 continue
      return
c
c       code for both increments equal to 1
c
c
c       clean-up loop
c
   20 m = mod(n,3)
      if( m .eq. 0 ) go to 40
      do 30 i = 1,m
        stemp = sx(i)
        sx(i) = sy(i)
        sy(i) = stemp
   30 continue
      if( n .lt. 3 ) return
   40 mp1 = m + 1
      do 50 i = mp1,n,3
        stemp = sx(i)
        sx(i) = sy(i)
        sy(i) = stemp
        stemp = sx(i + 1)
        sx(i + 1) = sy(i + 1)
        sy(i + 1) = stemp
        stemp = sx(i + 2)
        sx(i + 2) = sy(i + 2)
        sy(i + 2) = stemp
   50 continue
      return
      end
C-----------------------------------------------------------------------
C  Routine:    IVOUT
C
C  Purpose:    Integer vector output routine.
C
C  Usage:      CALL IVOUT (LOUT, N, IX, IDIGIT, IFMT)
C
C  Arguments
C     N      - Length of array IX. (Input)
C     IX     - Integer array to be printed. (Input)
C     IFMT   - Format to be used in printing array IX. (Input)
C     IDIGIT - Print up to ABS(IDIGIT) decimal digits / number. (Input)
C              If IDIGIT .LT. 0, printing is done with 72 columns.
C              If IDIGIT .GT. 0, printing is done with 132 columns.
C
C-----------------------------------------------------------------------
C
      SUBROUTINE IVOUT (LOUT, N, IX, IDIGIT, IFMT)
C     ...
C     ... SPECIFICATIONS FOR ARGUMENTS
      INTEGER    IX(*), N, IDIGIT, LOUT
      CHARACTER  IFMT*(*)
C     ...
C     ... SPECIFICATIONS FOR LOCAL VARIABLES
      INTEGER    I, NDIGIT, K1, K2, LLL
      CHARACTER*80 LINE
*     ...
*     ... SPECIFICATIONS INTRINSICS
      INTRINSIC          MIN
*
C
      LLL = MIN ( LEN ( IFMT ), 80 )
      DO 1 I = 1, LLL
          LINE(I:I) = '-'
    1 CONTINUE
C
      DO 2 I = LLL+1, 80
          LINE(I:I) = ' '
    2 CONTINUE
C
      WRITE ( LOUT, 2000 ) IFMT, LINE(1:LLL)
 2000 FORMAT ( /1X, A  /1X, A )
C
      IF (N .LE. 0) RETURN
      NDIGIT = IDIGIT
      IF (IDIGIT .EQ. 0) NDIGIT = 4
C
C=======================================================================
C             CODE FOR OUTPUT USING 72 COLUMNS FORMAT
C=======================================================================
C
      IF (IDIGIT .LT. 0) THEN
C
      NDIGIT = -IDIGIT
      IF (NDIGIT .LE. 4) THEN
         DO 10 K1 = 1, N, 10
            K2 = MIN0(N,K1+9)
            WRITE(LOUT,1000) K1,K2,(IX(I),I=K1,K2)
   10    CONTINUE
C
      ELSE IF (NDIGIT .LE. 6) THEN
         DO 30 K1 = 1, N, 7
            K2 = MIN0(N,K1+6)
            WRITE(LOUT,1001) K1,K2,(IX(I),I=K1,K2)
   30    CONTINUE
C
      ELSE IF (NDIGIT .LE. 10) THEN
         DO 50 K1 = 1, N, 5
            K2 = MIN0(N,K1+4)
            WRITE(LOUT,1002) K1,K2,(IX(I),I=K1,K2)
   50    CONTINUE
C
      ELSE
         DO 70 K1 = 1, N, 3
            K2 = MIN0(N,K1+2)
            WRITE(LOUT,1003) K1,K2,(IX(I),I=K1,K2)
   70    CONTINUE
      END IF
C
C=======================================================================
C             CODE FOR OUTPUT USING 132 COLUMNS FORMAT
C=======================================================================
C
      ELSE
C
      IF (NDIGIT .LE. 4) THEN
         DO 90 K1 = 1, N, 20
            K2 = MIN0(N,K1+19)
            WRITE(LOUT,1000) K1,K2,(IX(I),I=K1,K2)
   90    CONTINUE
C
      ELSE IF (NDIGIT .LE. 6) THEN
         DO 110 K1 = 1, N, 15
            K2 = MIN0(N,K1+14)
            WRITE(LOUT,1001) K1,K2,(IX(I),I=K1,K2)
  110    CONTINUE
C
      ELSE IF (NDIGIT .LE. 10) THEN
         DO 130 K1 = 1, N, 10
            K2 = MIN0(N,K1+9)
            WRITE(LOUT,1002) K1,K2,(IX(I),I=K1,K2)
  130    CONTINUE
C
      ELSE
         DO 150 K1 = 1, N, 7
            K2 = MIN0(N,K1+6)
            WRITE(LOUT,1003) K1,K2,(IX(I),I=K1,K2)
  150    CONTINUE
      END IF
      END IF
      WRITE (LOUT,1004)
C
 1000 FORMAT(1X,I4,' - ',I4,':',20(1X,I5))
 1001 FORMAT(1X,I4,' - ',I4,':',15(1X,I7))
 1002 FORMAT(1X,I4,' - ',I4,':',10(1X,I11))
 1003 FORMAT(1X,I4,' - ',I4,':',7(1X,I15))
 1004 FORMAT(1X,' ')
C
      RETURN
      END
      SUBROUTINE SECOND( T )
*
      REAL       T
*
*  -- LAPACK auxiliary routine (preliminary version) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     July 26, 1991
*
*  Purpose
*  =======
*
*  SECOND returns the user time for a process in seconds.
*  This version gets the time from the system function ETIME.
*
*     .. Local Scalars ..
      REAL               T1
*     ..
*     .. Local Arrays ..
      REAL               TARRAY( 2 )
*     ..
*     .. External Functions ..
      REAL               ETIME
      EXTERNAL           ETIME
*     ..
*     .. Executable Statements ..
*

      T1 = ETIME( TARRAY )
      T  = TARRAY( 1 )

      RETURN
*
*     End of SECOND
*
      END
*-----------------------------------------------------------------------
*  Routine:    SMOUT
*
*  Purpose:    Real matrix output routine.
*
*  Usage:      CALL SMOUT (LOUT, M, N, A, LDA, IDIGIT, IFMT)
*
*  Arguments
*     M      - Number of rows of A.  (Input)
*     N      - Number of columns of A.  (Input)
*     A      - Real M by N matrix to be printed.  (Input)
*     LDA    - Leading dimension of A exactly as specified in the
*              dimension statement of the calling program.  (Input)
*     IFMT   - Format to be used in printing matrix A.  (Input)
*     IDIGIT - Print up to IABS(IDIGIT) decimal digits per number.  (In)
*              If IDIGIT .LT. 0, printing is done with 72 columns.
*              If IDIGIT .GT. 0, printing is done with 132 columns.
*
*-----------------------------------------------------------------------
*
      SUBROUTINE SMOUT( LOUT, M, N, A, LDA, IDIGIT, IFMT )
*     ...
*     ... SPECIFICATIONS FOR ARGUMENTS
      INTEGER            M, N, IDIGIT, LDA, LOUT
      REAL               A( LDA, * )
      CHARACTER          IFMT*( * )
*     ...
*     ... SPECIFICATIONS FOR LOCAL VARIABLES
      INTEGER            I, J, NDIGIT, K1, K2, LLL
      CHARACTER*1        ICOL( 3 )
      CHARACTER*80       LINE
*     ...
*     ... SPECIFICATIONS INTRINSICS
      INTRINSIC          MIN
*
      DATA               ICOL( 1 ), ICOL( 2 ), ICOL( 3 ) / 'C', 'o',
     $                   'l' /
*     ...
*     ... FIRST EXECUTABLE STATEMENT
*
      LLL = MIN( LEN( IFMT ), 80 )
      DO 10 I = 1, LLL
         LINE( I: I ) = '-'
   10 CONTINUE
*
      DO 20 I = LLL + 1, 80
         LINE( I: I ) = ' '
   20 CONTINUE
*
      WRITE( LOUT, 9999 )IFMT, LINE( 1: LLL )
 9999 FORMAT( / 1X, A / 1X, A )
*
      IF( M.LE.0 .OR. N.LE.0 .OR. LDA.LE.0 )
     $   RETURN
      NDIGIT = IDIGIT
      IF( IDIGIT.EQ.0 )
     $   NDIGIT = 4
*
*=======================================================================
*             CODE FOR OUTPUT USING 72 COLUMNS FORMAT
*=======================================================================
*
      IF( IDIGIT.LT.0 ) THEN
         NDIGIT = -IDIGIT
         IF( NDIGIT.LE.4 ) THEN
            DO 40 K1 = 1, N, 5
               K2 = MIN0( N, K1+4 )
               WRITE( LOUT, 9998 )( ICOL, I, I = K1, K2 )
               DO 30 I = 1, M
                  WRITE( LOUT, 9994 )I, ( A( I, J ), J = K1, K2 )
   30          CONTINUE
   40       CONTINUE
*
         ELSE IF( NDIGIT.LE.6 ) THEN
            DO 60 K1 = 1, N, 4
               K2 = MIN0( N, K1+3 )
               WRITE( LOUT, 9997 )( ICOL, I, I = K1, K2 )
               DO 50 I = 1, M
                  WRITE( LOUT, 9993 )I, ( A( I, J ), J = K1, K2 )
   50          CONTINUE
   60       CONTINUE
*
         ELSE IF( NDIGIT.LE.10 ) THEN
            DO 80 K1 = 1, N, 3
               K2 = MIN0( N, K1+2 )
               WRITE( LOUT, 9996 )( ICOL, I, I = K1, K2 )
               DO 70 I = 1, M
                  WRITE( LOUT, 9992 )I, ( A( I, J ), J = K1, K2 )
   70          CONTINUE
   80       CONTINUE
*
         ELSE
            DO 100 K1 = 1, N, 2
               K2 = MIN0( N, K1+1 )
               WRITE( LOUT, 9995 )( ICOL, I, I = K1, K2 )
               DO 90 I = 1, M
                  WRITE( LOUT, 9991 )I, ( A( I, J ), J = K1, K2 )
   90          CONTINUE
  100       CONTINUE
         END IF
*
*=======================================================================
*             CODE FOR OUTPUT USING 132 COLUMNS FORMAT
*=======================================================================
*
      ELSE
         IF( NDIGIT.LE.4 ) THEN
            DO 120 K1 = 1, N, 10
               K2 = MIN0( N, K1+9 )
               WRITE( LOUT, 9998 )( ICOL, I, I = K1, K2 )
               DO 110 I = 1, M
                  WRITE( LOUT, 9994 )I, ( A( I, J ), J = K1, K2 )
  110          CONTINUE
  120       CONTINUE
*
         ELSE IF( NDIGIT.LE.6 ) THEN
            DO 140 K1 = 1, N, 8
               K2 = MIN0( N, K1+7 )
               WRITE( LOUT, 9997 )( ICOL, I, I = K1, K2 )
               DO 130 I = 1, M
                  WRITE( LOUT, 9993 )I, ( A( I, J ), J = K1, K2 )
  130          CONTINUE
  140       CONTINUE
*
         ELSE IF( NDIGIT.LE.10 ) THEN
            DO 160 K1 = 1, N, 6
               K2 = MIN0( N, K1+5 )
               WRITE( LOUT, 9996 )( ICOL, I, I = K1, K2 )
               DO 150 I = 1, M
                  WRITE( LOUT, 9992 )I, ( A( I, J ), J = K1, K2 )
  150          CONTINUE
  160       CONTINUE
*
         ELSE
            DO 180 K1 = 1, N, 5
               K2 = MIN0( N, K1+4 )
               WRITE( LOUT, 9995 )( ICOL, I, I = K1, K2 )
               DO 170 I = 1, M
                  WRITE( LOUT, 9991 )I, ( A( I, J ), J = K1, K2 )
  170          CONTINUE
  180       CONTINUE
         END IF
      END IF
      WRITE( LOUT, 9990 )
*
 9998 FORMAT( 10X, 10( 4X, 3A1, I4, 1X ) )
 9997 FORMAT( 10X, 8( 5X, 3A1, I4, 2X ) )
 9996 FORMAT( 10X, 6( 7X, 3A1, I4, 4X ) )
 9995 FORMAT( 10X, 5( 9X, 3A1, I4, 6X ) )
 9994 FORMAT( 1X, ' Row', I4, ':', 1X, 1P10E12.3 )
 9993 FORMAT( 1X, ' Row', I4, ':', 1X, 1P8E14.5 )
 9992 FORMAT( 1X, ' Row', I4, ':', 1X, 1P6E18.9 )
 9991 FORMAT( 1X, ' Row', I4, ':', 1X, 1P5E22.13 )
 9990 FORMAT( 1X, ' ' )
*
      RETURN
      END
*-----------------------------------------------------------------------
*  Routine:    SVOUT
*
*  Purpose:    Real vector output routine.
*
*  Usage:      CALL SVOUT (LOUT, N, SX, IDIGIT, IFMT)
*
*  Arguments
*     N      - Length of array SX.  (Input)
*     SX     - Real array to be printed.  (Input)
*     IFMT   - Format to be used in printing array SX.  (Input)
*     IDIGIT - Print up to IABS(IDIGIT) decimal digits per number.  (In)
*              If IDIGIT .LT. 0, printing is done with 72 columns.
*              If IDIGIT .GT. 0, printing is done with 132 columns.
*
*-----------------------------------------------------------------------
*
      SUBROUTINE SVOUT( LOUT, N, SX, IDIGIT, IFMT )
*     ...
*     ... SPECIFICATIONS FOR ARGUMENTS
      INTEGER            N, IDIGIT, LOUT
      REAL               SX( * )
      CHARACTER          IFMT*( * )
*     ...
*     ... SPECIFICATIONS FOR LOCAL VARIABLES
      INTEGER            I, NDIGIT, K1, K2, LLL
      CHARACTER*80       LINE
*     ...
*     ... FIRST EXECUTABLE STATEMENT
*
*
      LLL = MIN( LEN( IFMT ), 80 )
      DO 10 I = 1, LLL
         LINE( I: I ) = '-'
   10 CONTINUE
*
      DO 20 I = LLL + 1, 80
         LINE( I: I ) = ' '
   20 CONTINUE
*
      WRITE( LOUT, 9999 )IFMT, LINE( 1: LLL )
 9999 FORMAT( / 1X, A / 1X, A )
*
      IF( N.LE.0 )
     $   RETURN
      NDIGIT = IDIGIT
      IF( IDIGIT.EQ.0 )
     $   NDIGIT = 4
*
*=======================================================================
*             CODE FOR OUTPUT USING 72 COLUMNS FORMAT
*=======================================================================
*
      IF( IDIGIT.LT.0 ) THEN
         NDIGIT = -IDIGIT
         IF( NDIGIT.LE.4 ) THEN
            DO 30 K1 = 1, N, 5
               K2 = MIN0( N, K1+4 )
               WRITE( LOUT, 9998 )K1, K2, ( SX( I ), I = K1, K2 )
   30       CONTINUE
         ELSE IF( NDIGIT.LE.6 ) THEN
            DO 40 K1 = 1, N, 4
               K2 = MIN0( N, K1+3 )
               WRITE( LOUT, 9997 )K1, K2, ( SX( I ), I = K1, K2 )
   40       CONTINUE
         ELSE IF( NDIGIT.LE.10 ) THEN
            DO 50 K1 = 1, N, 3
               K2 = MIN0( N, K1+2 )
               WRITE( LOUT, 9996 )K1, K2, ( SX( I ), I = K1, K2 )
   50       CONTINUE
         ELSE
            DO 60 K1 = 1, N, 2
               K2 = MIN0( N, K1+1 )
               WRITE( LOUT, 9995 )K1, K2, ( SX( I ), I = K1, K2 )
   60       CONTINUE
         END IF
*
*=======================================================================
*             CODE FOR OUTPUT USING 132 COLUMNS FORMAT
*=======================================================================
*
      ELSE
         IF( NDIGIT.LE.4 ) THEN
            DO 70 K1 = 1, N, 10
               K2 = MIN0( N, K1+9 )
               WRITE( LOUT, 9998 )K1, K2, ( SX( I ), I = K1, K2 )
   70       CONTINUE
         ELSE IF( NDIGIT.LE.6 ) THEN
            DO 80 K1 = 1, N, 8
               K2 = MIN0( N, K1+7 )
               WRITE( LOUT, 9997 )K1, K2, ( SX( I ), I = K1, K2 )
   80       CONTINUE
         ELSE IF( NDIGIT.LE.10 ) THEN
            DO 90 K1 = 1, N, 6
               K2 = MIN0( N, K1+5 )
               WRITE( LOUT, 9996 )K1, K2, ( SX( I ), I = K1, K2 )
   90       CONTINUE
         ELSE
            DO 100 K1 = 1, N, 5
               K2 = MIN0( N, K1+4 )
               WRITE( LOUT, 9995 )K1, K2, ( SX( I ), I = K1, K2 )
  100       CONTINUE
         END IF
      END IF
      WRITE( LOUT, 9994 )
      RETURN
 9998 FORMAT( 1X, I4, ' - ', I4, ':', 1P10E12.3 )
 9997 FORMAT( 1X, I4, ' - ', I4, ':', 1X, 1P8E14.5 )
 9996 FORMAT( 1X, I4, ' - ', I4, ':', 1X, 1P6E18.9 )
 9995 FORMAT( 1X, I4, ' - ', I4, ':', 1X, 1P5E24.13 )
 9994 FORMAT( 1X, ' ' )
      END
