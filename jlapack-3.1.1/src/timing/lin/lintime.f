      SUBROUTINE ATIMCK( ICHK, SUBNAM, NN, NVAL, NLDA, LDAVAL, NOUT,
     $                   INFO )
*
*  -- LAPACK timing routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     March 31, 1993
*
*     .. Scalar Arguments ..
      CHARACTER*6        SUBNAM
      INTEGER            ICHK, INFO, NLDA, NN, NOUT
*     ..
*     .. Array Arguments ..
      INTEGER            LDAVAL( * ), NVAL( * )
*     ..
*
*  Purpose
*  =======
*
*  ATIMCK checks the input values of M, N, or K and LDA to determine
*  if they are valid for type TYPE.  The tests to be performed are
*  specified in the option variable ICHK.
*
*  On exit, INFO contains a count of the number of pairs (N,LDA) that
*  were invalid.
*
*  Arguments
*  =========
*
*  ICHK    (input) INTEGER
*          Specifies the type of comparison
*          = 1:  M <= LDA
*          = 2:  N <= LDA
*          = 3:  K <= LDA
*          = 4:  N*(N+1)/2 <= LA
*          = 0 or other value:  Determined from name passed in SUBNAM
*
*  SUBNAM  (input) CHARACTER*6
*          The name of the subroutine or path for which the input
*          values are to be tested.
*
*  NN      (input) INTEGER
*          The number of values of N contained in the vector NVAL.
*
*  NVAL    (input) INTEGER array, dimension( NN )
*          The values of the matrix size N.
*
*  NLDA    (input) INTEGER
*          The number of values of LDA contained in the vector LDAVAL.
*
*  LDAVAL  (input) INTEGER array, dimension( NLDA )
*          The values of the leading dimension of the array A.
*
*  NOUT    (input) INTEGER
*          The unit number for output.
*
*  INFO    (output) INTEGER
*          The number of pairs (N, LDA) that were invalid.
*
*  =====================================================================
*
*     .. Local Scalars ..
      CHARACTER*2        TYPE
      INTEGER            I, J, LDA, N
*     ..
*     .. External Functions ..
      LOGICAL            LSAMEN
      EXTERNAL           LSAMEN
*     ..
*     .. Executable Statements ..
*
      TYPE = SUBNAM( 2: 3 )
      INFO = 0
*
*     M, N, or K must be less than or equal to LDA.
*
      IF( ICHK.EQ.1 .OR. ICHK.EQ.2 .OR. ICHK.EQ.3 ) THEN
         DO 20 J = 1, NLDA
            LDA = LDAVAL( J )
            DO 10 I = 1, NN
               IF( NVAL( I ).GT.LDA ) THEN
                  INFO = INFO + 1
                  IF( NOUT.GT.0 ) THEN
                     IF( ICHK.EQ.1 ) THEN
                        WRITE( NOUT, FMT = 9999 )SUBNAM, NVAL( I ), LDA
                     ELSE IF( ICHK.EQ.2 ) THEN
                        WRITE( NOUT, FMT = 9998 )SUBNAM, NVAL( I ), LDA
                     ELSE
                        WRITE( NOUT, FMT = 9997 )SUBNAM, NVAL( I ), LDA
                     END IF
                  END IF
               END IF
   10       CONTINUE
   20    CONTINUE
*
*     IF TYPE = 'PP', 'SP', or 'HP',
*     then N*(N+1)/2 must be less than or equal to LA = LDAVAL(1).
*
      ELSE IF( ICHK.EQ.4 ) THEN
         LDA = LDAVAL( 1 )
         DO 30 I = 1, NN
            N = NVAL( I )
            IF( N*( N+1 ) / 2.GT.LDA ) THEN
               INFO = INFO + 1
               IF( NOUT.GT.0 )
     $            WRITE( NOUT, FMT = 9996 )SUBNAM, N, LDA
            END IF
   30    CONTINUE
*
*     IF TYPE = 'GB', then K must satisfy
*        2*K+1 <= LDA,  if SUBNAM = 'xGBMV'
*        3*K+1 <= LDA,  otherwise.
*
      ELSE IF( LSAMEN( 2, TYPE, 'GB' ) ) THEN
         IF( LSAMEN( 3, SUBNAM( 4: 6 ), 'MV ' ) ) THEN
            DO 50 J = 1, NLDA
               LDA = LDAVAL( J )
               DO 40 I = 1, NN
                  IF( 2*NVAL( I )+1.GT.LDA ) THEN
                     INFO = INFO + 1
                     IF( NOUT.GT.0 )
     $                  WRITE( NOUT, FMT = 9994 )SUBNAM, NVAL( I ),
     $                  LDA, 2*NVAL( I ) + 1
                  END IF
   40          CONTINUE
   50       CONTINUE
         ELSE
            DO 70 J = 1, NLDA
               LDA = LDAVAL( J )
               DO 60 I = 1, NN
                  IF( 3*NVAL( I )+1.GT.LDA ) THEN
                     INFO = INFO + 1
                     IF( NOUT.GT.0 )
     $                  WRITE( NOUT, FMT = 9995 )SUBNAM, NVAL( I ),
     $                  LDA, 3*NVAL( I ) + 1
                  END IF
   60          CONTINUE
   70       CONTINUE
         END IF
*
*     IF TYPE = 'PB' or 'TB', then K must satisfy
*        K+1 <= LDA.
*
      ELSE IF( LSAMEN( 2, TYPE, 'PB' ) .OR. LSAMEN( 2, TYPE, 'TB' ) )
     $          THEN
         DO 90 J = 1, NLDA
            LDA = LDAVAL( J )
            DO 80 I = 1, NN
               IF( NVAL( I )+1.GT.LDA ) THEN
                  INFO = INFO + 1
                  IF( NOUT.GT.0 )
     $               WRITE( NOUT, FMT = 9993 )SUBNAM, NVAL( I ), LDA
               END IF
   80       CONTINUE
   90    CONTINUE
*
*     IF TYPE = 'SB' or 'HB', then K must satisfy
*        K+1   <= LDA,  if SUBNAM = 'xxxMV '
*
      ELSE IF( LSAMEN( 2, TYPE, 'SB' ) .OR. LSAMEN( 2, TYPE, 'HB' ) )
     $          THEN
         IF( LSAMEN( 3, SUBNAM( 4: 6 ), 'MV ' ) ) THEN
            DO 110 J = 1, NLDA
               LDA = LDAVAL( J )
               DO 100 I = 1, NN
                  IF( NVAL( I )+1.GT.LDA ) THEN
                     INFO = INFO + 1
                     IF( NOUT.GT.0 )
     $                  WRITE( NOUT, FMT = 9992 )SUBNAM, NVAL( I ), LDA
                  END IF
  100          CONTINUE
  110       CONTINUE
         END IF
*
      END IF
 9999 FORMAT( ' *** Error for ', A6, ':  M > LDA for M =', I6,
     $      ', LDA =', I7 )
 9998 FORMAT( ' *** Error for ', A6, ':  N > LDA for N =', I6,
     $      ', LDA =', I7 )
 9997 FORMAT( ' *** Error for ', A6, ':  K > LDA for K =', I6,
     $      ', LDA =', I7 )
 9996 FORMAT( ' *** Error for ', A6, ':  N*(N+1)/2 > LA for N =', I6,
     $      ', LA =', I7 )
 9995 FORMAT( ' *** Error for ', A6, ':  3*K+1 > LDA for K =', I6,
     $      ', LDA =', I7, / ' --> Increase LDA to at least ', I7 )
 9994 FORMAT( ' *** Error for ', A6, ':  2*K+1 > LDA for K =', I6,
     $      ', LDA =', I7, / ' --> Increase LDA to at least ', I7 )
 9993 FORMAT( ' *** Error for ', A6, ':  K+1 > LDA for K =', I6, ', LD',
     $      'A =', I7 )
 9992 FORMAT( ' *** Error for ', A6, ':  2*K+2 > LDA for K =', I6, ', ',
     $      'LDA =', I7 )
*
      RETURN
*
*     End of ATIMCK
*
      END
      SUBROUTINE ATIMIN( PATH, LINE, NSUBS, NAMES, TIMSUB, NOUT, INFO )
*
*  -- LAPACK timing routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     February 29, 1992
*
*     .. Scalar Arguments ..
      CHARACTER*80       LINE
      CHARACTER*( * )    PATH
      INTEGER            INFO, NOUT, NSUBS
*     ..
*     .. Array Arguments ..
      LOGICAL            TIMSUB( * )
      CHARACTER*( * )    NAMES( * )
*     ..
*
*  Purpose
*  =======
*
*  ATIMIN interprets the input line for the timing routines.
*  The LOGICAL array TIMSUB returns .true. for each routine to be
*  timed and .false. for the routines which are not to be timed.
*
*  Arguments
*  =========
*
*  PATH    (input) CHARACTER*(*)
*          The LAPACK path name of the calling routine.  The path name
*          may be at most 6 characters long.  If LINE(1:LEN(PATH)) is
*          the same as PATH, then the input line is searched for NSUBS
*          non-blank characters, otherwise, the input line is assumed to
*          specify a single subroutine name.
*
*  LINE    (input) CHARACTER*80
*          The input line to be evaluated.  The path or subroutine name
*          must begin in column 1 and the part of the line after the
*          name is used to indicate the routines to be timed.
*          See below for further details.
*
*  NSUBS   (input) INTEGER
*          The number of subroutines in the LAPACK path name of the
*          calling routine.
*
*  NAMES   (input) CHARACTER*(*) array, dimension (NSUBS)
*          The names of the subroutines in the LAPACK path name of the
*          calling routine.
*
*  TIMSUB  (output) LOGICAL array, dimension (NSUBS)
*          For each I from 1 to NSUBS, TIMSUB( I ) is set to .true. if
*          the subroutine NAMES( I ) is to be timed; otherwise,
*          TIMSUB( I ) is set to .false.
*
*  NOUT    (input) INTEGER
*          The unit number on which error messages will be printed.
*
*  INFO    (output) INTEGER
*          The return status of this routine.
*          = -1:  Unrecognized path or subroutine name
*          =  0:  Normal return
*          =  1:  Name was recognized, but no timing requested
*
*  Further Details
*  ======= =======
*
*  An input line begins with a subroutine or path name, optionally
*  followed by one or more non-blank characters indicating the specific
*  routines to be timed.
*
*  If the character string in PATH appears at the beginning of LINE,
*  up to NSUBS routines may be timed.  If LINE is blank after the path
*  name, all the routines in the path will be timed.  If LINE is not
*  blank after the path name, the rest of the line is searched
*  for NSUBS nonblank characters, and if the i-th such character is
*  't' or 'T', then the i-th subroutine in this path will be timed.
*  For example, the input line
*     SGE    T T T T
*  requests timing of the first 4 subroutines in the SGE path.
*
*  If the character string in PATH does not appear at the beginning of
*  LINE, then LINE is assumed to begin with a subroutine name.  The name
*  is assumed to end in column 6 or in column i if column i+1 is blank
*  and i+1 <= 6.  If LINE is completely blank after the subroutine name,
*  the routine will be timed.  If LINE is not blank after the subroutine
*  name, then the subroutine will be timed if the first non-blank after
*  the name is 't' or 'T'.
*
*  =====================================================================
*
*     .. Local Scalars ..
      LOGICAL            REQ
      CHARACTER*6        CNAME
      INTEGER            I, ISTART, ISTOP, ISUB, LCNAME, LNAMES, LPATH
*     ..
*     .. External Functions ..
      LOGICAL            LSAME, LSAMEN
      EXTERNAL           LSAME, LSAMEN
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          LEN, MIN
*     ..
*     .. Executable Statements ..
*
*
*     Initialize
*
      INFO = 0
      LCNAME = 1
      DO 10 I = 2, 6
         IF( LINE( I: I ).EQ.' ' )
     $      GO TO 20
         LCNAME = I
   10 CONTINUE
   20 CONTINUE
      LPATH = MIN( LCNAME+1, LEN( PATH ) )
      LNAMES = MIN( LCNAME+1, LEN( NAMES( 1 ) ) )
      CNAME = LINE( 1: LCNAME )
*
      DO 30 I = 1, NSUBS
         TIMSUB( I ) = .FALSE.
   30 CONTINUE
      ISTOP = 0
*
*     Check for a valid path or subroutine name.
*
      IF( LCNAME.LE.LEN( PATH ) .AND. LSAMEN( LPATH, CNAME, PATH ) )
     $     THEN
         ISTART = 1
         ISTOP = NSUBS
      ELSE IF( LCNAME.LE.LEN( NAMES( 1 ) ) ) THEN
         DO 40 I = 1, NSUBS
            IF( LSAMEN( LNAMES, CNAME, NAMES( I ) ) ) THEN
               ISTART = I
               ISTOP = I
            END IF
   40    CONTINUE
      END IF
*
      IF( ISTOP.EQ.0 ) THEN
         WRITE( NOUT, FMT = 9999 )CNAME
 9999    FORMAT( 1X, A, ':  Unrecognized path or subroutine name', / )
         INFO = -1
         GO TO 110
      END IF
*
*     Search the rest of the input line for 1 or NSUBS nonblank
*     characters, where 'T' or 't' means 'Time this routine'.
*
      ISUB = ISTART
      DO 50 I = LCNAME + 1, 80
         IF( LINE( I: I ).NE.' ' ) THEN
            TIMSUB( ISUB ) = LSAME( LINE( I: I ), 'T' )
            ISUB = ISUB + 1
            IF( ISUB.GT.ISTOP )
     $         GO TO 60
         END IF
   50 CONTINUE
   60 CONTINUE
*
*     If no characters appear after the routine or path name, then
*     time the routine or all the routines in the path.
*
      IF( ISUB.EQ.ISTART ) THEN
         DO 70 I = ISTART, ISTOP
            TIMSUB( I ) = .TRUE.
   70    CONTINUE
      ELSE
*
*        Test to see if any timing was requested.
*
         REQ = .FALSE.
         DO 80 I = ISTART, ISUB - 1
            REQ = REQ .OR. TIMSUB( I )
   80    CONTINUE
         IF( .NOT.REQ ) THEN
            WRITE( NOUT, FMT = 9998 )CNAME
 9998       FORMAT( 1X, A, ' was not timed', / )
            INFO = 1
            GO TO 110
         END IF
   90    CONTINUE
*
*       If fewer than NSUBS characters are specified for a path name,
*       the rest are assumed to be 'F'.
*
         DO 100 I = ISUB, ISTOP
            TIMSUB( I ) = .FALSE.
  100    CONTINUE
      END IF
  110 CONTINUE
      RETURN
*
*     End of ATIMIN
*
      END
      SUBROUTINE ORTHES(NM,N,LOW,IGH,A,ORT)
C
      INTEGER I,J,M,N,II,JJ,LA,MP,NM,IGH,KP1,LOW
      DOUBLE PRECISION A(NM,N),ORT(IGH)
      DOUBLE PRECISION F,G,H,SCALE
C
C     THIS SUBROUTINE IS A TRANSLATION OF THE ALGOL PROCEDURE ORTHES,
C     NUM. MATH. 12, 349-368(1968) BY MARTIN AND WILKINSON.
C     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 339-358(1971).
C
C     GIVEN A REAL GENERAL MATRIX, THIS SUBROUTINE
C     REDUCES A SUBMATRIX SITUATED IN ROWS AND COLUMNS
C     LOW THROUGH IGH TO UPPER HESSENBERG FORM BY
C     ORTHOGONAL SIMILARITY TRANSFORMATIONS.
C
C     ON INPUT
C
C        NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL
C          ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM
C          DIMENSION STATEMENT.
C
C        N IS THE ORDER OF THE MATRIX.
C
C        LOW AND IGH ARE INTEGERS DETERMINED BY THE BALANCING
C          SUBROUTINE  BALANC.  IF  BALANC  HAS NOT BEEN USED,
C          SET LOW=1, IGH=N.
C
C        A CONTAINS THE INPUT MATRIX.
C
C     ON OUTPUT
C
C        A CONTAINS THE HESSENBERG MATRIX.  INFORMATION ABOUT
C          THE ORTHOGONAL TRANSFORMATIONS USED IN THE REDUCTION
C          IS STORED IN THE REMAINING TRIANGLE UNDER THE
C          HESSENBERG MATRIX.
C
C        ORT CONTAINS FURTHER INFORMATION ABOUT THE TRANSFORMATIONS.
C          ONLY ELEMENTS LOW THROUGH IGH ARE USED.
C
C     QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO BURTON S. GARBOW,
C     MATHEMATICS AND COMPUTER SCIENCE DIV, ARGONNE NATIONAL LABORATORY
C
C     THIS VERSION DATED AUGUST 1983.
C
C     ------------------------------------------------------------------
C
      LA = IGH - 1
      KP1 = LOW + 1
      IF (LA .LT. KP1) GO TO 200
C
      DO 180 M = KP1, LA
         H = 0.0D0
         ORT(M) = 0.0D0
         SCALE = 0.0D0
C     .......... SCALE COLUMN (ALGOL TOL THEN NOT NEEDED) ..........
         DO 90 I = M, IGH
   90    SCALE = SCALE + DABS(A(I,M-1))
C
         IF (SCALE .EQ. 0.0D0) GO TO 180
         MP = M + IGH
C     .......... FOR I=IGH STEP -1 UNTIL M DO -- ..........
         DO 100 II = M, IGH
            I = MP - II
            ORT(I) = A(I,M-1) / SCALE
            H = H + ORT(I) * ORT(I)
  100    CONTINUE
C
         G = -DSIGN(DSQRT(H),ORT(M))
         H = H - ORT(M) * G
         ORT(M) = ORT(M) - G
C     .......... FORM (I-(U*UT)/H) * A ..........
         DO 130 J = M, N
            F = 0.0D0
C     .......... FOR I=IGH STEP -1 UNTIL M DO -- ..........
            DO 110 II = M, IGH
               I = MP - II
               F = F + ORT(I) * A(I,J)
  110       CONTINUE
C
            F = F / H
C
            DO 120 I = M, IGH
  120       A(I,J) = A(I,J) - F * ORT(I)
C
  130    CONTINUE
C     .......... FORM (I-(U*UT)/H)*A*(I-(U*UT)/H) ..........
         DO 160 I = 1, IGH
            F = 0.0D0
C     .......... FOR J=IGH STEP -1 UNTIL M DO -- ..........
            DO 140 JJ = M, IGH
               J = MP - JJ
               F = F + ORT(J) * A(I,J)
  140       CONTINUE
C
            F = F / H
C
            DO 150 J = M, IGH
  150       A(I,J) = A(I,J) - F * ORT(J)
C
  160    CONTINUE
C
         ORT(M) = SCALE * ORT(M)
         A(M,M-1) = SCALE * G
  180 CONTINUE
C
  200 RETURN
      END
      SUBROUTINE TRED1(NM,N,A,D,E,E2)
C
      INTEGER I,J,K,L,N,II,NM,JP1
      DOUBLE PRECISION A(NM,N),D(N),E(N),E2(N)
      DOUBLE PRECISION F,G,H,SCALE
C
C     THIS SUBROUTINE IS A TRANSLATION OF THE ALGOL PROCEDURE TRED1,
C     NUM. MATH. 11, 181-195(1968) BY MARTIN, REINSCH, AND WILKINSON.
C     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 212-226(1971).
C
C     THIS SUBROUTINE REDUCES A REAL SYMMETRIC MATRIX
C     TO A SYMMETRIC TRIDIAGONAL MATRIX USING
C     ORTHOGONAL SIMILARITY TRANSFORMATIONS.
C
C     ON INPUT
C
C        NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL
C          ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM
C          DIMENSION STATEMENT.
C
C        N IS THE ORDER OF THE MATRIX.
C
C        A CONTAINS THE REAL SYMMETRIC INPUT MATRIX.  ONLY THE
C          LOWER TRIANGLE OF THE MATRIX NEED BE SUPPLIED.
C
C     ON OUTPUT
C
C        A CONTAINS INFORMATION ABOUT THE ORTHOGONAL TRANS-
C          FORMATIONS USED IN THE REDUCTION IN ITS STRICT LOWER
C          TRIANGLE.  THE FULL UPPER TRIANGLE OF A IS UNALTERED.
C
C        D CONTAINS THE DIAGONAL ELEMENTS OF THE TRIDIAGONAL MATRIX.
C
C        E CONTAINS THE SUBDIAGONAL ELEMENTS OF THE TRIDIAGONAL
C          MATRIX IN ITS LAST N-1 POSITIONS.  E(1) IS SET TO ZERO.
C
C        E2 CONTAINS THE SQUARES OF THE CORRESPONDING ELEMENTS OF E.
C          E2 MAY COINCIDE WITH E IF THE SQUARES ARE NOT NEEDED.
C
C     QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO BURTON S. GARBOW,
C     MATHEMATICS AND COMPUTER SCIENCE DIV, ARGONNE NATIONAL LABORATORY
C
C     THIS VERSION DATED AUGUST 1983.
C
C     ------------------------------------------------------------------
C
      DO 100 I = 1, N
         D(I) = A(N,I)
         A(N,I) = A(I,I)
  100 CONTINUE
C     .......... FOR I=N STEP -1 UNTIL 1 DO -- ..........
      DO 300 II = 1, N
         I = N + 1 - II
         L = I - 1
         H = 0.0D0
         SCALE = 0.0D0
         IF (L .LT. 1) GO TO 130
C     .......... SCALE ROW (ALGOL TOL THEN NOT NEEDED) ..........
         DO 120 K = 1, L
  120    SCALE = SCALE + DABS(D(K))
C
         IF (SCALE .NE. 0.0D0) GO TO 140
C
         DO 125 J = 1, L
            D(J) = A(L,J)
            A(L,J) = A(I,J)
            A(I,J) = 0.0D0
  125    CONTINUE
C
  130    E(I) = 0.0D0
         E2(I) = 0.0D0
         GO TO 300
C
  140    DO 150 K = 1, L
            D(K) = D(K) / SCALE
            H = H + D(K) * D(K)
  150    CONTINUE
C
         E2(I) = SCALE * SCALE * H
         F = D(L)
         G = -DSIGN(DSQRT(H),F)
         E(I) = SCALE * G
         H = H - F * G
         D(L) = F - G
         IF (L .EQ. 1) GO TO 285
C     .......... FORM A*U ..........
         DO 170 J = 1, L
  170    E(J) = 0.0D0
C
         DO 240 J = 1, L
            F = D(J)
            G = E(J) + A(J,J) * F
            JP1 = J + 1
            IF (L .LT. JP1) GO TO 220
C
            DO 200 K = JP1, L
               G = G + A(K,J) * D(K)
               E(K) = E(K) + A(K,J) * F
  200       CONTINUE
C
  220       E(J) = G
  240    CONTINUE
C     .......... FORM P ..........
         F = 0.0D0
C
         DO 245 J = 1, L
            E(J) = E(J) / H
            F = F + E(J) * D(J)
  245    CONTINUE
C
         H = F / (H + H)
C     .......... FORM Q ..........
         DO 250 J = 1, L
  250    E(J) = E(J) - H * D(J)
C     .......... FORM REDUCED A ..........
         DO 280 J = 1, L
            F = D(J)
            G = E(J)
C
            DO 260 K = J, L
  260       A(K,J) = A(K,J) - F * E(K) - G * D(K)
C
  280    CONTINUE
C
  285    DO 290 J = 1, L
            F = D(J)
            D(J) = A(L,J)
            A(L,J) = A(I,J)
            A(I,J) = F * SCALE
  290    CONTINUE
C
  300 CONTINUE
C
      RETURN
      END
      SUBROUTINE DLAORD( JOB, N, X, INCX )
*
*  -- LAPACK auxiliary routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     February 29, 1992
*
*     .. Scalar Arguments ..
      CHARACTER          JOB
      INTEGER            INCX, N
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   X( * )
*     ..
*
*  Purpose
*  =======
*
*  DLAORD sorts the elements of a vector x in increasing or decreasing
*  order.
*
*  Arguments
*  =========
*
*  JOB     (input) CHARACTER
*          = 'I':  Sort in increasing order
*          = 'D':  Sort in decreasing order
*
*  N       (input) INTEGER
*          The length of the vector X.
*
*  X       (input/output) DOUBLE PRECISION array, dimension
*                         (1+(N-1)*INCX)
*          On entry, the vector of length n to be sorted.
*          On exit, the vector x is sorted in the prescribed order.
*
*  INCX    (input) INTEGER
*          The spacing between successive elements of X.  INCX >= 0.
*
*  =====================================================================
*
*     .. Local Scalars ..
      INTEGER            I, INC, IX, IXNEXT
      DOUBLE PRECISION   TEMP
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS
*     ..
*     .. Executable Statements ..
*
      INC = ABS( INCX )
      IF( LSAME( JOB, 'I' ) ) THEN
*
*        Sort in increasing order
*
         DO 20 I = 2, N
            IX = 1 + ( I-1 )*INC
   10       CONTINUE
            IF( IX.EQ.1 )
     $         GO TO 20
            IXNEXT = IX - INC
            IF( X( IX ).GT.X( IXNEXT ) ) THEN
               GO TO 20
            ELSE
               TEMP = X( IX )
               X( IX ) = X( IXNEXT )
               X( IXNEXT ) = TEMP
            END IF
            IX = IXNEXT
            GO TO 10
   20    CONTINUE
*
      ELSE IF( LSAME( JOB, 'D' ) ) THEN
*
*        Sort in decreasing order
*
         DO 40 I = 2, N
            IX = 1 + ( I-1 )*INC
   30       CONTINUE
            IF( IX.EQ.1 )
     $         GO TO 40
            IXNEXT = IX - INC
            IF( X( IX ).LT.X( IXNEXT ) ) THEN
               GO TO 40
            ELSE
               TEMP = X( IX )
               X( IX ) = X( IXNEXT )
               X( IXNEXT ) = TEMP
            END IF
            IX = IXNEXT
            GO TO 30
   40    CONTINUE
      END IF
      RETURN
*
*     End of DLAORD
*
      END
      SUBROUTINE DGEFA(A,LDA,N,IPVT,INFO)
      INTEGER LDA,N,IPVT(*),INFO
      DOUBLE PRECISION A(LDA,*)
C
C     DGEFA FACTORS A DOUBLE PRECISION MATRIX BY GAUSSIAN ELIMINATION.
C
C     DGEFA IS USUALLY CALLED BY DGECO, BUT IT CAN BE CALLED
C     DIRECTLY WITH A SAVING IN TIME IF  RCOND  IS NOT NEEDED.
C     (TIME FOR DGECO) = (1 + 9/N)*(TIME FOR DGEFA) .
C
C     ON ENTRY
C
C        A       DOUBLE PRECISION(LDA, N)
C                THE MATRIX TO BE FACTORED.
C
C        LDA     INTEGER
C                THE LEADING DIMENSION OF THE ARRAY  A .
C
C        N       INTEGER
C                THE ORDER OF THE MATRIX  A .
C
C     ON RETURN
C
C        A       AN UPPER TRIANGULAR MATRIX AND THE MULTIPLIERS
C                WHICH WERE USED TO OBTAIN IT.
C                THE FACTORIZATION CAN BE WRITTEN  A = L*U  WHERE
C                L  IS A PRODUCT OF PERMUTATION AND UNIT LOWER
C                TRIANGULAR MATRICES AND  U  IS UPPER TRIANGULAR.
C
C        IPVT    INTEGER(N)
C                AN INTEGER VECTOR OF PIVOT INDICES.
C
C        INFO    INTEGER
C                = 0  NORMAL VALUE.
C                = K  IF  U(K,K) .EQ. 0.0 .  THIS IS NOT AN ERROR
C                     CONDITION FOR THIS SUBROUTINE, BUT IT DOES
C                     INDICATE THAT DGESL OR DGEDI WILL DIVIDE BY ZERO
C                     IF CALLED.  USE  RCOND  IN DGECO FOR A RELIABLE
C                     INDICATION OF SINGULARITY.
C
C     LINPACK. THIS VERSION DATED 08/14/78 .
C     CLEVE MOLER, UNIVERSITY OF NEW MEXICO, ARGONNE NATIONAL LAB.
C
C     SUBROUTINES AND FUNCTIONS
C
C     BLAS DAXPY,DSCAL,IDAMAX
C
C     INTERNAL VARIABLES
C
      DOUBLE PRECISION T
      INTEGER IDAMAX,J,K,KP1,L,NM1
      EXTERNAL IDAMAX
C
C
C     GAUSSIAN ELIMINATION WITH PARTIAL PIVOTING
C
      INFO = 0
      NM1 = N - 1
      IF (NM1 .LT. 1) GO TO 70
      DO 60 K = 1, NM1
         KP1 = K + 1
C
C        FIND L = PIVOT INDEX
C
         L = IDAMAX(N-K+1,A(K,K),1) + K - 1
         IPVT(K) = L
C
C        ZERO PIVOT IMPLIES THIS COLUMN ALREADY TRIANGULARIZED
C
         IF (A(L,K) .EQ. 0.0D0) GO TO 40
C
C           INTERCHANGE IF NECESSARY
C
            IF (L .EQ. K) GO TO 10
               T = A(L,K)
               A(L,K) = A(K,K)
               A(K,K) = T
   10       CONTINUE
C
C           COMPUTE MULTIPLIERS
C
            T = -1.0D0/A(K,K)
            CALL DSCAL(N-K,T,A(K+1,K),1)
C
C           ROW ELIMINATION WITH COLUMN INDEXING
C
            DO 30 J = KP1, N
               T = A(L,J)
               IF (L .EQ. K) GO TO 20
                  A(L,J) = A(K,J)
                  A(K,J) = T
   20          CONTINUE
               CALL DAXPY(N-K,T,A(K+1,K),1,A(K+1,J),1)
   30       CONTINUE
         GO TO 50
   40    CONTINUE
            INFO = K
   50    CONTINUE
   60 CONTINUE
   70 CONTINUE
      IPVT(N) = N
      IF (A(N,N) .EQ. 0.0D0) INFO = N
      RETURN
      END
      SUBROUTINE DPOFA(A,LDA,N,INFO)
      INTEGER LDA,N,INFO
      DOUBLE PRECISION A(LDA,*)
C
C     DPOFA FACTORS A DOUBLE PRECISION SYMMETRIC POSITIVE DEFINITE
C     MATRIX.
C
C     DPOFA IS USUALLY CALLED BY DPOCO, BUT IT CAN BE CALLED
C     DIRECTLY WITH A SAVING IN TIME IF  RCOND  IS NOT NEEDED.
C     (TIME FOR DPOCO) = (1 + 18/N)*(TIME FOR DPOFA) .
C
C     ON ENTRY
C
C        A       DOUBLE PRECISION(LDA, N)
C                THE SYMMETRIC MATRIX TO BE FACTORED.  ONLY THE
C                DIAGONAL AND UPPER TRIANGLE ARE USED.
C
C        LDA     INTEGER
C                THE LEADING DIMENSION OF THE ARRAY  A .
C
C        N       INTEGER
C                THE ORDER OF THE MATRIX  A .
C
C     ON RETURN
C
C        A       AN UPPER TRIANGULAR MATRIX  R  SO THAT  A = TRANS(R)*R
C                WHERE  TRANS(R)  IS THE TRANSPOSE.
C                THE STRICT LOWER TRIANGLE IS UNALTERED.
C                IF  INFO .NE. 0 , THE FACTORIZATION IS NOT COMPLETE.
C
C        INFO    INTEGER
C                = 0  FOR NORMAL RETURN.
C                = K  SIGNALS AN ERROR CONDITION.  THE LEADING MINOR
C                     OF ORDER  K  IS NOT POSITIVE DEFINITE.
C
C     LINPACK.  THIS VERSION DATED 08/14/78 .
C     CLEVE MOLER, UNIVERSITY OF NEW MEXICO, ARGONNE NATIONAL LAB.
C
C     SUBROUTINES AND FUNCTIONS
C
C     BLAS DDOT
C     FORTRAN DSQRT
C
C     INTERNAL VARIABLES
C
      DOUBLE PRECISION DDOT,T
      DOUBLE PRECISION S
      INTEGER J,JM1,K
      EXTERNAL DDOT
C     BEGIN BLOCK WITH ...EXITS TO 40
C
C
         DO 30 J = 1, N
            INFO = J
            S = 0.0D0
            JM1 = J - 1
            IF (JM1 .LT. 1) GO TO 20
            DO 10 K = 1, JM1
               T = A(K,J) - DDOT(K-1,A(1,K),1,A(1,J),1)
               T = T/A(K,K)
               A(K,J) = T
               S = S + T*T
   10       CONTINUE
   20       CONTINUE
            S = A(J,J) - S
C     ......EXIT
            IF (S .LE. 0.0D0) GO TO 40
            A(J,J) = DSQRT(S)
   30    CONTINUE
         INFO = 0
   40 CONTINUE
      RETURN
      END
      SUBROUTINE DQRDC(X,LDX,N,P,QRAUX,JPVT,WORK,JOB)
      INTEGER LDX,N,P,JOB
      INTEGER JPVT(*)
      DOUBLE PRECISION X(LDX,*),QRAUX(*),WORK(*)
C
C     DQRDC USES HOUSEHOLDER TRANSFORMATIONS TO COMPUTE THE QR
C     FACTORIZATION OF AN N BY P MATRIX X.  COLUMN PIVOTING
C     BASED ON THE 2-NORMS OF THE REDUCED COLUMNS MAY BE
C     PERFORMED AT THE USERS OPTION.
C
C     ON ENTRY
C
C        X       DOUBLE PRECISION(LDX,P), WHERE LDX .GE. N.
C                X CONTAINS THE MATRIX WHOSE DECOMPOSITION IS TO BE
C                COMPUTED.
C
C        LDX     INTEGER.
C                LDX IS THE LEADING DIMENSION OF THE ARRAY X.
C
C        N       INTEGER.
C                N IS THE NUMBER OF ROWS OF THE MATRIX X.
C
C        P       INTEGER.
C                P IS THE NUMBER OF COLUMNS OF THE MATRIX X.
C
C        JPVT    INTEGER(P).
C                JPVT CONTAINS INTEGERS THAT CONTROL THE SELECTION
C                OF THE PIVOT COLUMNS.  THE K-TH COLUMN X(K) OF X
C                IS PLACED IN ONE OF THREE CLASSES ACCORDING TO THE
C                VALUE OF JPVT(K).
C
C                   IF JPVT(K) .GT. 0, THEN X(K) IS AN INITIAL
C                                      COLUMN.
C
C                   IF JPVT(K) .EQ. 0, THEN X(K) IS A FREE COLUMN.
C
C                   IF JPVT(K) .LT. 0, THEN X(K) IS A FINAL COLUMN.
C
C                BEFORE THE DECOMPOSITION IS COMPUTED, INITIAL COLUMNS
C                ARE MOVED TO THE BEGINNING OF THE ARRAY X AND FINAL
C                COLUMNS TO THE END.  BOTH INITIAL AND FINAL COLUMNS
C                ARE FROZEN IN PLACE DURING THE COMPUTATION AND ONLY
C                FREE COLUMNS ARE MOVED.  AT THE K-TH STAGE OF THE
C                REDUCTION, IF X(K) IS OCCUPIED BY A FREE COLUMN
C                IT IS INTERCHANGED WITH THE FREE COLUMN OF LARGEST
C                REDUCED NORM.  JPVT IS NOT REFERENCED IF
C                JOB .EQ. 0.
C
C        WORK    DOUBLE PRECISION(P).
C                WORK IS A WORK ARRAY.  WORK IS NOT REFERENCED IF
C                JOB .EQ. 0.
C
C        JOB     INTEGER.
C                JOB IS AN INTEGER THAT INITIATES COLUMN PIVOTING.
C                IF JOB .EQ. 0, NO PIVOTING IS DONE.
C                IF JOB .NE. 0, PIVOTING IS DONE.
C
C     ON RETURN
C
C        X       X CONTAINS IN ITS UPPER TRIANGLE THE UPPER
C                TRIANGULAR MATRIX R OF THE QR FACTORIZATION.
C                BELOW ITS DIAGONAL X CONTAINS INFORMATION FROM
C                WHICH THE ORTHOGONAL PART OF THE DECOMPOSITION
C                CAN BE RECOVERED.  NOTE THAT IF PIVOTING HAS
C                BEEN REQUESTED, THE DECOMPOSITION IS NOT THAT
C                OF THE ORIGINAL MATRIX X BUT THAT OF X
C                WITH ITS COLUMNS PERMUTED AS DESCRIBED BY JPVT.
C
C        QRAUX   DOUBLE PRECISION(P).
C                QRAUX CONTAINS FURTHER INFORMATION REQUIRED TO RECOVER
C                THE ORTHOGONAL PART OF THE DECOMPOSITION.
C
C        JPVT    JPVT(K) CONTAINS THE INDEX OF THE COLUMN OF THE
C                ORIGINAL MATRIX THAT HAS BEEN INTERCHANGED INTO
C                THE K-TH COLUMN, IF PIVOTING WAS REQUESTED.
C
C     LINPACK. THIS VERSION DATED 08/14/78 .
C     G.W. STEWART, UNIVERSITY OF MARYLAND, ARGONNE NATIONAL LAB.
C
C     DQRDC USES THE FOLLOWING FUNCTIONS AND SUBPROGRAMS.
C
C     BLAS DAXPY,DDOT,DSCAL,DSWAP,DNRM2
C     FORTRAN DABS,DMAX1,MIN0,DSQRT
C
C     INTERNAL VARIABLES
C
      INTEGER J,JJ,JP,L,LP1,LUP,MAXJ,PL,PU
      DOUBLE PRECISION MAXNRM,DNRM2,TT
      DOUBLE PRECISION DDOT,NRMXL,T
      LOGICAL NEGJ,SWAPJ
      EXTERNAL DAXPY, DDOT, DSCAL, DSWAP, DNRM2
C
C
      PL = 1
      PU = 0
      IF (JOB .EQ. 0) GO TO 60
C
C        PIVOTING HAS BEEN REQUESTED.  REARRANGE THE COLUMNS
C        ACCORDING TO JPVT.
C
         DO 20 J = 1, P
            SWAPJ = JPVT(J) .GT. 0
            NEGJ = JPVT(J) .LT. 0
            JPVT(J) = J
            IF (NEGJ) JPVT(J) = -J
            IF (.NOT.SWAPJ) GO TO 10
               IF (J .NE. PL) CALL DSWAP(N,X(1,PL),1,X(1,J),1)
               JPVT(J) = JPVT(PL)
               JPVT(PL) = J
               PL = PL + 1
   10       CONTINUE
   20    CONTINUE
         PU = P
         DO 50 JJ = 1, P
            J = P - JJ + 1
            IF (JPVT(J) .GE. 0) GO TO 40
               JPVT(J) = -JPVT(J)
               IF (J .EQ. PU) GO TO 30
                  CALL DSWAP(N,X(1,PU),1,X(1,J),1)
                  JP = JPVT(PU)
                  JPVT(PU) = JPVT(J)
                  JPVT(J) = JP
   30          CONTINUE
               PU = PU - 1
   40       CONTINUE
   50    CONTINUE
   60 CONTINUE
C
C     COMPUTE THE NORMS OF THE FREE COLUMNS.
C
      IF (PU .LT. PL) GO TO 80
      DO 70 J = PL, PU
         QRAUX(J) = DNRM2(N,X(1,J),1)
         WORK(J) = QRAUX(J)
   70 CONTINUE
   80 CONTINUE
C
C     PERFORM THE HOUSEHOLDER REDUCTION OF X.
C
      LUP = MIN0(N,P)
      DO 200 L = 1, LUP
         IF (L .LT. PL .OR. L .GE. PU) GO TO 120
C
C           LOCATE THE COLUMN OF LARGEST NORM AND BRING IT
C           INTO THE PIVOT POSITION.
C
            MAXNRM = 0.0D0
            MAXJ = L
            DO 100 J = L, PU
               IF (QRAUX(J) .LE. MAXNRM) GO TO 90
                  MAXNRM = QRAUX(J)
                  MAXJ = J
   90          CONTINUE
  100       CONTINUE
            IF (MAXJ .EQ. L) GO TO 110
               CALL DSWAP(N,X(1,L),1,X(1,MAXJ),1)
               QRAUX(MAXJ) = QRAUX(L)
               WORK(MAXJ) = WORK(L)
               JP = JPVT(MAXJ)
               JPVT(MAXJ) = JPVT(L)
               JPVT(L) = JP
  110       CONTINUE
  120    CONTINUE
         QRAUX(L) = 0.0D0
         IF (L .EQ. N) GO TO 190
C
C           COMPUTE THE HOUSEHOLDER TRANSFORMATION FOR COLUMN L.
C
            NRMXL = DNRM2(N-L+1,X(L,L),1)
            IF (NRMXL .EQ. 0.0D0) GO TO 180
               IF (X(L,L) .NE. 0.0D0) NRMXL = DSIGN(NRMXL,X(L,L))
               CALL DSCAL(N-L+1,1.0D0/NRMXL,X(L,L),1)
               X(L,L) = 1.0D0 + X(L,L)
C
C              APPLY THE TRANSFORMATION TO THE REMAINING COLUMNS,
C              UPDATING THE NORMS.
C
               LP1 = L + 1
               IF (P .LT. LP1) GO TO 170
               DO 160 J = LP1, P
                  T = -DDOT(N-L+1,X(L,L),1,X(L,J),1)/X(L,L)
                  CALL DAXPY(N-L+1,T,X(L,L),1,X(L,J),1)
                  IF (J .LT. PL .OR. J .GT. PU) GO TO 150
                  IF (QRAUX(J) .EQ. 0.0D0) GO TO 150
                     TT = 1.0D0 - (DABS(X(L,J))/QRAUX(J))**2
                     TT = DMAX1(TT,0.0D0)
                     T = TT
                     TT = 1.0D0 + 0.05D0*TT*(QRAUX(J)/WORK(J))**2
                     IF (TT .EQ. 1.0D0) GO TO 130
                        QRAUX(J) = QRAUX(J)*DSQRT(T)
                     GO TO 140
  130                CONTINUE
                        QRAUX(J) = DNRM2(N-L,X(L+1,J),1)
                        WORK(J) = QRAUX(J)
  140                CONTINUE
  150             CONTINUE
  160          CONTINUE
  170          CONTINUE
C
C              SAVE THE TRANSFORMATION.
C
               QRAUX(L) = X(L,L)
               X(L,L) = -NRMXL
  180       CONTINUE
  190    CONTINUE
  200 CONTINUE
      RETURN
      END
      SUBROUTINE DGTSL(N,C,D,E,B,INFO)
      INTEGER N,INFO
      DOUBLE PRECISION C(*),D(*),E(*),B(*)
C
C     DGTSL GIVEN A GENERAL TRIDIAGONAL MATRIX AND A RIGHT HAND
C     SIDE WILL FIND THE SOLUTION.
C
C     ON ENTRY
C
C        N       INTEGER
C                IS THE ORDER OF THE TRIDIAGONAL MATRIX.
C
C        C       DOUBLE PRECISION(N)
C                IS THE SUBDIAGONAL OF THE TRIDIAGONAL MATRIX.
C                C(2) THROUGH C(N) SHOULD CONTAIN THE SUBDIAGONAL.
C                ON OUTPUT C IS DESTROYED.
C
C        D       DOUBLE PRECISION(N)
C                IS THE DIAGONAL OF THE TRIDIAGONAL MATRIX.
C                ON OUTPUT D IS DESTROYED.
C
C        E       DOUBLE PRECISION(N)
C                IS THE SUPERDIAGONAL OF THE TRIDIAGONAL MATRIX.
C                E(1) THROUGH E(N-1) SHOULD CONTAIN THE SUPERDIAGONAL.
C                ON OUTPUT E IS DESTROYED.
C
C        B       DOUBLE PRECISION(N)
C                IS THE RIGHT HAND SIDE VECTOR.
C
C     ON RETURN
C
C        B       IS THE SOLUTION VECTOR.
C
C        INFO    INTEGER
C                = 0 NORMAL VALUE.
C                = K IF THE K-TH ELEMENT OF THE DIAGONAL BECOMES
C                    EXACTLY ZERO.  THE SUBROUTINE RETURNS WHEN
C                    THIS IS DETECTED.
C
C     LINPACK. THIS VERSION DATED 08/14/78 .
C     JACK DONGARRA, ARGONNE NATIONAL LABORATORY.
C
C     NO EXTERNALS
C     FORTRAN DABS
C
C     INTERNAL VARIABLES
C
      INTEGER K,KB,KP1,NM1,NM2
      DOUBLE PRECISION T
C     BEGIN BLOCK PERMITTING ...EXITS TO 100
C
         INFO = 0
         C(1) = D(1)
         NM1 = N - 1
         IF (NM1 .LT. 1) GO TO 40
            D(1) = E(1)
            E(1) = 0.0D0
            E(N) = 0.0D0
C
            DO 30 K = 1, NM1
               KP1 = K + 1
C
C              FIND THE LARGEST OF THE TWO ROWS
C
               IF (DABS(C(KP1)) .LT. DABS(C(K))) GO TO 10
C
C                 INTERCHANGE ROW
C
                  T = C(KP1)
                  C(KP1) = C(K)
                  C(K) = T
                  T = D(KP1)
                  D(KP1) = D(K)
                  D(K) = T
                  T = E(KP1)
                  E(KP1) = E(K)
                  E(K) = T
                  T = B(KP1)
                  B(KP1) = B(K)
                  B(K) = T
   10          CONTINUE
C
C              ZERO ELEMENTS
C
               IF (C(K) .NE. 0.0D0) GO TO 20
                  INFO = K
C     ............EXIT
                  GO TO 100
   20          CONTINUE
               T = -C(KP1)/C(K)
               C(KP1) = D(KP1) + T*D(K)
               D(KP1) = E(KP1) + T*E(K)
               E(KP1) = 0.0D0
               B(KP1) = B(KP1) + T*B(K)
   30       CONTINUE
   40    CONTINUE
         IF (C(N) .NE. 0.0D0) GO TO 50
            INFO = N
         GO TO 90
   50    CONTINUE
C
C           BACK SOLVE
C
            NM2 = N - 2
            B(N) = B(N)/C(N)
            IF (N .EQ. 1) GO TO 80
               B(NM1) = (B(NM1) - D(NM1)*B(N))/C(NM1)
               IF (NM2 .LT. 1) GO TO 70
               DO 60 KB = 1, NM2
                  K = NM2 - KB + 1
                  B(K) = (B(K) - D(K)*B(K+1) - E(K)*B(K+2))/C(K)
   60          CONTINUE
   70          CONTINUE
   80       CONTINUE
   90    CONTINUE
  100 CONTINUE
C
      RETURN
      END
      SUBROUTINE DPTSL(N,D,E,B)
      INTEGER N
      DOUBLE PRECISION D(*),E(*),B(*)
C
C     DPTSL GIVEN A POSITIVE DEFINITE TRIDIAGONAL MATRIX AND A RIGHT
C     HAND SIDE WILL FIND THE SOLUTION.
C
C     ON ENTRY
C
C        N        INTEGER
C                 IS THE ORDER OF THE TRIDIAGONAL MATRIX.
C
C        D        DOUBLE PRECISION(N)
C                 IS THE DIAGONAL OF THE TRIDIAGONAL MATRIX.
C                 ON OUTPUT D IS DESTROYED.
C
C        E        DOUBLE PRECISION(N)
C                 IS THE OFFDIAGONAL OF THE TRIDIAGONAL MATRIX.
C                 E(1) THROUGH E(N-1) SHOULD CONTAIN THE
C                 OFFDIAGONAL.
C
C        B        DOUBLE PRECISION(N)
C                 IS THE RIGHT HAND SIDE VECTOR.
C
C     ON RETURN
C
C        B        CONTAINS THE SOULTION.
C
C     LINPACK. THIS VERSION DATED 08/14/78 .
C     JACK DONGARRA, ARGONNE NATIONAL LABORATORY.
C
C     NO EXTERNALS
C     FORTRAN MOD
C
C     INTERNAL VARIABLES
C
      INTEGER K,KBM1,KE,KF,KP1,NM1,NM1D2
      DOUBLE PRECISION T1,T2
C
C     CHECK FOR 1 X 1 CASE
C
      IF (N .NE. 1) GO TO 10
         B(1) = B(1)/D(1)
      GO TO 70
   10 CONTINUE
         NM1 = N - 1
         NM1D2 = NM1/2
         IF (N .EQ. 2) GO TO 30
            KBM1 = N - 1
C
C           ZERO TOP HALF OF SUBDIAGONAL AND BOTTOM HALF OF
C           SUPERDIAGONAL
C
            DO 20 K = 1, NM1D2
               T1 = E(K)/D(K)
               D(K+1) = D(K+1) - T1*E(K)
               B(K+1) = B(K+1) - T1*B(K)
               T2 = E(KBM1)/D(KBM1+1)
               D(KBM1) = D(KBM1) - T2*E(KBM1)
               B(KBM1) = B(KBM1) - T2*B(KBM1+1)
               KBM1 = KBM1 - 1
   20       CONTINUE
   30    CONTINUE
         KP1 = NM1D2 + 1
C
C        CLEAN UP FOR POSSIBLE 2 X 2 BLOCK AT CENTER
C
         IF (MOD(N,2) .NE. 0) GO TO 40
            T1 = E(KP1)/D(KP1)
            D(KP1+1) = D(KP1+1) - T1*E(KP1)
            B(KP1+1) = B(KP1+1) - T1*B(KP1)
            KP1 = KP1 + 1
   40    CONTINUE
C
C        BACK SOLVE STARTING AT THE CENTER, GOING TOWARDS THE TOP
C        AND BOTTOM
C
         B(KP1) = B(KP1)/D(KP1)
         IF (N .EQ. 2) GO TO 60
            K = KP1 - 1
            KE = KP1 + NM1D2 - 1
            DO 50 KF = KP1, KE
               B(K) = (B(K) - E(K)*B(K+1))/D(K)
               B(KF+1) = (B(KF+1) - E(KF)*B(KF))/D(KF+1)
               K = K - 1
   50       CONTINUE
   60    CONTINUE
         IF (MOD(N,2) .EQ. 0) B(1) = (B(1) - E(1)*B(2))/D(1)
   70 CONTINUE
      RETURN
      END
      DOUBLE PRECISION FUNCTION DMFLOP( OPS, TIME, INFO )
*
*  -- LAPACK timing routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     March 31, 1993
*
*     .. Scalar Arguments ..
      INTEGER            INFO
      DOUBLE PRECISION   OPS, TIME
*     ..
*
*  Purpose
*  =======
*
*  DMFLOP computes the megaflop rate given the number of operations
*  and time in seconds.  This is basically just a divide operation,
*  but care is taken not to divide by zero.
*
*  Arguments
*  =========
*
*  OPS     (input) DOUBLE PRECISION
*          The number of floating point operations.
*          performed by the timed routine.
*
*  TIME    (input) DOUBLE PRECISION
*          The total time in seconds.
*
*  INFO    (input) INTEGER
*          The return code from the timed routine.  If INFO is not 0,
*          then DMFLOP returns a negative value, indicating an error.
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO
      PARAMETER          ( ZERO = 0.0D+0 )
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, DBLE
*     ..
*     .. Executable Statements ..
*
      IF( TIME.LE.ZERO ) THEN
         DMFLOP = ZERO
      ELSE
         DMFLOP = OPS / ( 1.0D6*TIME )
      END IF
      IF( INFO.NE.0 )
     $   DMFLOP = -ABS( DBLE( INFO ) )
      RETURN
*
*     End of DMFLOP
*
      END
      DOUBLE PRECISION FUNCTION DOPAUX( SUBNAM, M, N, KL, KU, NB )
*
*  -- LAPACK timing routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     March 31, 1993
*
*     .. Scalar Arguments ..
      CHARACTER*6        SUBNAM
      INTEGER            KL, KU, M, N, NB
*     ..
*
*  Purpose
*  =======
*
*  DOPAUX computes an approximation of the number of floating point
*  operations used by the subroutine SUBNAM with the given values
*  of the parameters M, N, KL, KU, and NB.
*
*  This version counts operations for the LAPACK auxiliary routines.
*
*  Arguments
*  =========
*
*  SUBNAM  (input) CHARACTER*6
*          The name of the subroutine.
*
*  M       (input) INTEGER
*          The number of rows of the coefficient matrix.  M >= 0.
*
*  N       (input) INTEGER
*          The number of columns of the coefficient matrix.
*          If the matrix is square (such as in a solve routine) then
*          N is the number of right hand sides.  N >= 0.
*
*  KL      (input) INTEGER
*          The lower band width of the coefficient matrix.
*          If needed, 0 <= KL <= M-1.
*
*  KU      (input) INTEGER
*          The upper band width of the coefficient matrix.
*          If needed, 0 <= KU <= N-1.
*
*  NB      (input) INTEGER
*          The block size.  If needed, NB >= 1.
*
*  =====================================================================
*
*     .. Local Scalars ..
      CHARACTER          C1
      CHARACTER*2        C2
      CHARACTER*3        C3
      DOUBLE PRECISION   ADDFAC, ADDS, EK, EM, EN, ENB, MULFAC, MULTS
*     ..
*     .. External Functions ..
      LOGICAL            LSAME, LSAMEN
      EXTERNAL           LSAME, LSAMEN
*     ..
*     .. Executable Statements ..
*
      DOPAUX = 0
      MULTS = 0
      ADDS = 0
      C1 = SUBNAM( 1: 1 )
      C2 = SUBNAM( 2: 3 )
      C3 = SUBNAM( 4: 6 )
      IF( M.LE.0 .OR. .NOT.( LSAME( C1, 'S' ) .OR. LSAME( C1,
     $    'D' ) .OR. LSAME( C1, 'C' ) .OR. LSAME( C1, 'Z' ) ) ) THEN
         RETURN
      END IF
      IF( LSAME( C1, 'S' ) .OR. LSAME( C1, 'D' ) ) THEN
         MULFAC = 1
         ADDFAC = 1
      ELSE
         MULFAC = 6
         ADDFAC = 2
      END IF
      EM = M
      EN = N
      ENB = NB
*
      IF( LSAMEN( 2, C2, 'LA' ) ) THEN
*
*        xLAULM:  N  =>  M
*
         IF( LSAMEN( 3, C3, 'ULM' ) .OR. LSAMEN( 3, C3, 'UL2' ) ) THEN
            MULTS = ( 1.D0 / 3.D0 )*EM*( -1.D0+EM*EM )
            ADDS = EM*( 1.D0 / 6.D0+EM*( -1.D0 / 2.D0+EM*( 1.D0 /
     $             3.D0 ) ) )
*
*        xLAUUM:  N  =>  M
*
         ELSE IF( LSAMEN( 3, C3, 'UUM' ) .OR. LSAMEN( 3, C3, 'UU2' ) )
     $             THEN
            MULTS = EM*( 1.D0 / 3.D0+EM*( 1.D0 / 2.D0+EM*( 1.D0 /
     $              6.D0 ) ) )
            ADDS = ( 1.D0 / 6.D0 )*EM*( -1.D0+EM*EM )
*
*        xLACON:  N  =>  M
*
         ELSE IF( LSAMEN( 3, C3, 'CON' ) ) THEN
            MULTS = 3.D0*EM + 3.D0
            ADDS = 4.D0*EM - 3.D0
*
*        xLARF:  M, N  =>  M, N
*
         ELSE IF( LSAMEN( 3, C3, 'RF ' ) ) THEN
            MULTS = 2.D0*EM*EN + EN
            ADDS = 2.D0*EM*EN
*
*        xLARFB:  M, N, SIDE, NB  =>  M, N, KL, NB
*           where KL <= 0 indicates SIDE = 'L'
*           and   KL > 0  indicates SIDE = 'R'
*
         ELSE IF( LSAMEN( 3, C3, 'RFB' ) ) THEN
*
*           KL <= 0:  Code requiring local array
*
            IF( KL.LE.0 ) THEN
               MULTS = EN*ENB*( 2.D0*EM+( ENB+1.D0 ) / 2.D0 )
               ADDS = EN*ENB*( 2.D0*EM+( ENB-1.D0 ) / 2.D0 )
*
*           KL > 0:  Code not requiring local array
*
            ELSE
               MULTS = EN*ENB*( 2.D0*EM+( -ENB / 2.D0+5.D0 / 2.D0 ) )
               ADDS = EN*ENB*( 2.D0*EM+( -ENB / 2.D0-1.D0 / 2.D0 ) )
            END IF
*
*        xLARFG:  N  =>  M
*
         ELSE IF( LSAMEN( 3, C3, 'RFG' ) ) THEN
            MULTS = 2.D0*EM + 4.D0
            ADDS = EM + 1.D0
*
*        xLARFT:  M, NB  =>  M, N
*
         ELSE IF( LSAMEN( 3, C3, 'RFT' ) ) THEN
            MULTS = EN*( ( -5.D0 / 6.D0+EN*( 1.D0+EN*( -1.D0 /
     $              6.D0 ) ) )+( EM / 2.D0 )*( EN-1.D0 ) )
            ADDS = EN*( ( 1.D0 / 6.D0 )*( 1.D0-EN*EN )+( EM / 2.D0 )*
     $             ( EN-1.D0 ) )
*
*        xLATRD:  N, K  =>  M, N
*
         ELSE IF( LSAMEN( 3, C3, 'TRD' ) ) THEN
            EK = N
            MULTS = EK*( ( 25.D0 / 6.D0-EK*( 3.D0 / 2.D0+( 5.D0 /
     $              3.D0 )*EK ) )+EM*( 2.D0+2.D0*EK+EM ) )
            ADDS = EK*( ( -1.D0 / 3.D0-( 5.D0 / 3.D0 )*EK*EK )+EM*
     $             ( -1.D0+2.D0*EK+EM ) )
         END IF
*
      END IF
*
      DOPAUX = MULFAC*MULTS + ADDFAC*ADDS
*
      RETURN
*
*     End of DOPAUX
*
      END
      DOUBLE PRECISION FUNCTION DOPBL2( SUBNAM, M, N, KKL, KKU )
*
*  -- LAPACK timing routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     June 30, 1999
*
*     .. Scalar Arguments ..
      CHARACTER*6        SUBNAM
      INTEGER            KKL, KKU, M, N
*     ..
*
*  Purpose
*  =======
*
*  DOPBL2 computes an approximation of the number of floating point
*  operations used by a subroutine SUBNAM with the given values
*  of the parameters M, N, KL, and KU.
*
*  This version counts operations for the Level 2 BLAS.
*
*  Arguments
*  =========
*
*  SUBNAM  (input) CHARACTER*6
*          The name of the subroutine.
*
*  M       (input) INTEGER
*          The number of rows of the coefficient matrix.  M >= 0.
*
*  N       (input) INTEGER
*          The number of columns of the coefficient matrix.
*          If the matrix is square (such as in a solve routine) then
*          N is the number of right hand sides.  N >= 0.
*
*  KKL     (input) INTEGER
*          The lower band width of the coefficient matrix.
*          KL is set to max( 0, min( M-1, KKL ) ).
*
*  KKU     (input) INTEGER
*          The upper band width of the coefficient matrix.
*          KU is set to max( 0, min( N-1, KKU ) ).
*
*  =====================================================================
*
*     .. Local Scalars ..
      CHARACTER          C1
      CHARACTER*2        C2
      CHARACTER*3        C3
      DOUBLE PRECISION   ADDS, EK, EM, EN, KL, KU, MULTS
*     ..
*     .. External Functions ..
      LOGICAL            LSAME, LSAMEN
      EXTERNAL           LSAME, LSAMEN
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX, MIN
*     ..
*     .. Executable Statements ..
*
*     Quick return if possible
*
      IF( M.LE.0 .OR. .NOT.( LSAME( SUBNAM, 'S' ) .OR. LSAME( SUBNAM,
     $    'D' ) .OR. LSAME( SUBNAM, 'C' ) .OR. LSAME( SUBNAM, 'Z' ) ) )
     $     THEN
         DOPBL2 = 0
         RETURN
      END IF
*
      C1 = SUBNAM( 1: 1 )
      C2 = SUBNAM( 2: 3 )
      C3 = SUBNAM( 4: 6 )
      MULTS = 0
      ADDS = 0
      KL = MAX( 0, MIN( M-1, KKL ) )
      KU = MAX( 0, MIN( N-1, KKU ) )
      EM = M
      EN = N
      EK = KL
*
*     -------------------------------
*     Matrix-vector multiply routines
*     -------------------------------
*
      IF( LSAMEN( 3, C3, 'MV ' ) ) THEN
*
         IF( LSAMEN( 2, C2, 'GE' ) ) THEN
*
            MULTS = EM*( EN+1.D0 )
            ADDS = EM*EN
*
*        Assume M <= N + KL and KL < M
*               N <= M + KU and KU < N
*        so that the zero sections are triangles.
*
         ELSE IF( LSAMEN( 2, C2, 'GB' ) ) THEN
*
            MULTS = EM*( EN+1.D0 ) - ( EM-1.D0-KL )*( EM-KL ) / 2.D0 -
     $              ( EN-1.D0-KU )*( EN-KU ) / 2.D0
            ADDS = EM*( EN+1.D0 ) - ( EM-1.D0-KL )*( EM-KL ) / 2.D0 -
     $             ( EN-1.D0-KU )*( EN-KU ) / 2.D0
*
         ELSE IF( LSAMEN( 2, C2, 'SY' ) .OR. LSAMEN( 2, C2, 'SP' ) .OR.
     $            LSAMEN( 3, SUBNAM, 'CHE' ) .OR.
     $            LSAMEN( 3, SUBNAM, 'ZHE' ) .OR.
     $            LSAMEN( 3, SUBNAM, 'CHP' ) .OR.
     $            LSAMEN( 3, SUBNAM, 'ZHP' ) ) THEN
*
            MULTS = EM*( EM+1.D0 )
            ADDS = EM*EM
*
         ELSE IF( LSAMEN( 2, C2, 'SB' ) .OR.
     $            LSAMEN( 3, SUBNAM, 'CHB' ) .OR.
     $            LSAMEN( 3, SUBNAM, 'ZHB' ) ) THEN
*
            MULTS = EM*( EM+1.D0 ) - ( EM-1.D0-EK )*( EM-EK )
            ADDS = EM*EM - ( EM-1.D0-EK )*( EM-EK )
*
         ELSE IF( LSAMEN( 2, C2, 'TR' ) .OR. LSAMEN( 2, C2, 'TP' ) )
     $             THEN
*
            MULTS = EM*( EM+1.D0 ) / 2.D0
            ADDS = ( EM-1.D0 )*EM / 2.D0
*
         ELSE IF( LSAMEN( 2, C2, 'TB' ) ) THEN
*
            MULTS = EM*( EM+1.D0 ) / 2.D0 -
     $              ( EM-EK-1.D0 )*( EM-EK ) / 2.D0
            ADDS = ( EM-1.D0 )*EM / 2.D0 -
     $             ( EM-EK-1.D0 )*( EM-EK ) / 2.D0
*
         END IF
*
*     ---------------------
*     Matrix solve routines
*     ---------------------
*
      ELSE IF( LSAMEN( 3, C3, 'SV ' ) ) THEN
*
         IF( LSAMEN( 2, C2, 'TR' ) .OR. LSAMEN( 2, C2, 'TP' ) ) THEN
*
            MULTS = EM*( EM+1.D0 ) / 2.D0
            ADDS = ( EM-1.D0 )*EM / 2.D0
*
         ELSE IF( LSAMEN( 2, C2, 'TB' ) ) THEN
*
            MULTS = EM*( EM+1.D0 ) / 2.D0 -
     $              ( EM-EK-1.D0 )*( EM-EK ) / 2.D0
            ADDS = ( EM-1.D0 )*EM / 2.D0 -
     $             ( EM-EK-1.D0 )*( EM-EK ) / 2.D0
*
         END IF
*
*     ----------------
*     Rank-one updates
*     ----------------
*
      ELSE IF( LSAMEN( 3, C3, 'R  ' ) ) THEN
*
         IF( LSAMEN( 3, SUBNAM, 'SGE' ) .OR.
     $       LSAMEN( 3, SUBNAM, 'DGE' ) ) THEN
*
            MULTS = EM*EN + MIN( EM, EN )
            ADDS = EM*EN
*
         ELSE IF( LSAMEN( 2, C2, 'SY' ) .OR. LSAMEN( 2, C2, 'SP' ) .OR.
     $            LSAMEN( 3, SUBNAM, 'CHE' ) .OR.
     $            LSAMEN( 3, SUBNAM, 'CHP' ) .OR.
     $            LSAMEN( 3, SUBNAM, 'ZHE' ) .OR.
     $            LSAMEN( 3, SUBNAM, 'ZHP' ) ) THEN
*
            MULTS = EM*( EM+1.D0 ) / 2.D0 + EM
            ADDS = EM*( EM+1.D0 ) / 2.D0
*
         END IF
*
      ELSE IF( LSAMEN( 3, C3, 'RC ' ) .OR. LSAMEN( 3, C3, 'RU ' ) ) THEN
*
         IF( LSAMEN( 3, SUBNAM, 'CGE' ) .OR.
     $       LSAMEN( 3, SUBNAM, 'ZGE' ) ) THEN
*
            MULTS = EM*EN + MIN( EM, EN )
            ADDS = EM*EN
*
         END IF
*
*     ----------------
*     Rank-two updates
*     ----------------
*
      ELSE IF( LSAMEN( 3, C3, 'R2 ' ) ) THEN
         IF( LSAMEN( 2, C2, 'SY' ) .OR. LSAMEN( 2, C2, 'SP' ) .OR.
     $       LSAMEN( 3, SUBNAM, 'CHE' ) .OR.
     $       LSAMEN( 3, SUBNAM, 'CHP' ) .OR.
     $       LSAMEN( 3, SUBNAM, 'ZHE' ) .OR.
     $       LSAMEN( 3, SUBNAM, 'ZHP' ) ) THEN
*
            MULTS = EM*( EM+1.D0 ) + 2.D0*EM
            ADDS = EM*( EM+1.D0 )
*
         END IF
      END IF
*
*     ------------------------------------------------
*     Compute the total number of operations.
*     For real and double precision routines, count
*        1 for each multiply and 1 for each add.
*     For complex and complex*16 routines, count
*        6 for each multiply and 2 for each add.
*     ------------------------------------------------
*
      IF( LSAME( C1, 'S' ) .OR. LSAME( C1, 'D' ) ) THEN
*
         DOPBL2 = MULTS + ADDS
*
      ELSE
*
         DOPBL2 = 6*MULTS + 2*ADDS
*
      END IF
*
      RETURN
*
*     End of DOPBL2
*
      END
      DOUBLE PRECISION FUNCTION DOPBL3( SUBNAM, M, N, K )
*
*  -- LAPACK timing routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     June 30, 1999
*
*     .. Scalar Arguments ..
      CHARACTER*6        SUBNAM
      INTEGER            K, M, N
*     ..
*
*  Purpose
*  =======
*
*  DOPBL3 computes an approximation of the number of floating point
*  operations used by a subroutine SUBNAM with the given values
*  of the parameters M, N, and K.
*
*  This version counts operations for the Level 3 BLAS.
*
*  Arguments
*  =========
*
*  SUBNAM  (input) CHARACTER*6
*          The name of the subroutine.
*
*  M       (input) INTEGER
*  N       (input) INTEGER
*  K       (input) INTEGER
*          M, N, and K contain parameter values used by the Level 3
*          BLAS.  The output matrix is always M x N or N x N if
*          symmetric, but K has different uses in different
*          contexts.  For example, in the matrix-matrix multiply
*          routine, we have
*             C = A * B
*          where C is M x N, A is M x K, and B is K x N.
*          In xSYMM, xTRMM, and xTRSM, K indicates whether the matrix
*          A is applied on the left or right.  If K <= 0, the matrix
*          is applied on the left, if K > 0, on the right.
*
*  =====================================================================
*
*     .. Local Scalars ..
      CHARACTER          C1
      CHARACTER*2        C2
      CHARACTER*3        C3
      DOUBLE PRECISION   ADDS, EK, EM, EN, MULTS
*     ..
*     .. External Functions ..
      LOGICAL            LSAME, LSAMEN
      EXTERNAL           LSAME, LSAMEN
*     ..
*     .. Executable Statements ..
*
*     Quick return if possible
*
      IF( M.LE.0 .OR. .NOT.( LSAME( SUBNAM, 'S' ) .OR. LSAME( SUBNAM,
     $    'D' ) .OR. LSAME( SUBNAM, 'C' ) .OR. LSAME( SUBNAM, 'Z' ) ) )
     $     THEN
         DOPBL3 = 0
         RETURN
      END IF
*
      C1 = SUBNAM( 1: 1 )
      C2 = SUBNAM( 2: 3 )
      C3 = SUBNAM( 4: 6 )
      MULTS = 0
      ADDS = 0
      EM = M
      EN = N
      EK = K
*
*     ----------------------
*     Matrix-matrix products
*        assume beta = 1
*     ----------------------
*
      IF( LSAMEN( 3, C3, 'MM ' ) ) THEN
*
         IF( LSAMEN( 2, C2, 'GE' ) ) THEN
*
            MULTS = EM*EK*EN
            ADDS = EM*EK*EN
*
         ELSE IF( LSAMEN( 2, C2, 'SY' ) .OR.
     $            LSAMEN( 3, SUBNAM, 'CHE' ) .OR.
     $            LSAMEN( 3, SUBNAM, 'ZHE' ) ) THEN
*
*           IF K <= 0, assume A multiplies B on the left.
*
            IF( K.LE.0 ) THEN
               MULTS = EM*EM*EN
               ADDS = EM*EM*EN
            ELSE
               MULTS = EM*EN*EN
               ADDS = EM*EN*EN
            END IF
*
         ELSE IF( LSAMEN( 2, C2, 'TR' ) ) THEN
*
            IF( K.LE.0 ) THEN
               MULTS = EN*EM*( EM+1.D0 ) / 2.D0
               ADDS = EN*EM*( EM-1.D0 ) / 2.D0
            ELSE
               MULTS = EM*EN*( EN+1.D0 ) / 2.D0
               ADDS = EM*EN*( EN-1.D0 ) / 2.D0
            END IF
*
         END IF
*
*     ------------------------------------------------
*     Rank-K update of a symmetric or Hermitian matrix
*     ------------------------------------------------
*
      ELSE IF( LSAMEN( 3, C3, 'RK ' ) ) THEN
*
         IF( LSAMEN( 2, C2, 'SY' ) .OR. LSAMEN( 3, SUBNAM, 'CHE' ) .OR.
     $       LSAMEN( 3, SUBNAM, 'ZHE' ) ) THEN
*
            MULTS = EK*EM*( EM+1.D0 ) / 2.D0
            ADDS = EK*EM*( EM+1.D0 ) / 2.D0
         END IF
*
*     ------------------------------------------------
*     Rank-2K update of a symmetric or Hermitian matrix
*     ------------------------------------------------
*
      ELSE IF( LSAMEN( 3, C3, 'R2K' ) ) THEN
*
         IF( LSAMEN( 2, C2, 'SY' ) .OR. LSAMEN( 3, SUBNAM, 'CHE' ) .OR.
     $       LSAMEN( 3, SUBNAM, 'ZHE' ) ) THEN
*
            MULTS = EK*EM*EM
            ADDS = EK*EM*EM + EM
         END IF
*
*     -----------------------------------------
*     Solving system with many right hand sides
*     -----------------------------------------
*
      ELSE IF( LSAMEN( 5, SUBNAM( 2: 6 ), 'TRSM ' ) ) THEN
*
         IF( K.LE.0 ) THEN
            MULTS = EN*EM*( EM+1.D0 ) / 2.D0
            ADDS = EN*EM*( EM-1.D0 ) / 2.D0
         ELSE
            MULTS = EM*EN*( EN+1.D0 ) / 2.D0
            ADDS = EM*EN*( EN-1.D0 ) / 2.D0
         END IF
*
      END IF
*
*     ------------------------------------------------
*     Compute the total number of operations.
*     For real and double precision routines, count
*        1 for each multiply and 1 for each add.
*     For complex and complex*16 routines, count
*        6 for each multiply and 2 for each add.
*     ------------------------------------------------
*
      IF( LSAME( C1, 'S' ) .OR. LSAME( C1, 'D' ) ) THEN
*
         DOPBL3 = MULTS + ADDS
*
      ELSE
*
         DOPBL3 = 6*MULTS + 2*ADDS
*
      END IF
*
      RETURN
*
*     End of DOPBL3
*
      END
      DOUBLE PRECISION FUNCTION DOPGB( SUBNAM, M, N, KL, KU, IPIV )
*
*  -- LAPACK timing routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     June 30, 1999
*
*     .. Scalar Arguments ..
      CHARACTER*6        SUBNAM
      INTEGER            KL, KU, M, N
*     ..
*     .. Array Arguments ..
      INTEGER            IPIV( * )
*     ..
*
*  Purpose
*  =======
*
*  DOPGB counts operations for the LU factorization of a band matrix
*  xGBTRF.
*
*  Arguments
*  =========
*
*  SUBNAM  (input) CHARACTER*6
*          The name of the subroutine.
*
*  M       (input) INTEGER
*          The number of rows of the coefficient matrix.  M >= 0.
*
*  N       (input) INTEGER
*          The number of columns of the coefficient matrix.  N >= 0.
*
*  KL      (input) INTEGER
*          The number of subdiagonals of the matrix.  KL >= 0.
*
*  KU      (input) INTEGER
*          The number of superdiagonals of the matrix.  KU >= 0.
*
*  IPIV    (input)  INTEGER array, dimension (min(M,N))
*          The vector of pivot indices from DGBTRF or ZGBTRF.
*
*  =====================================================================
*
*     .. Local Scalars ..
      LOGICAL            CORZ, SORD
      CHARACTER          C1
      CHARACTER*2        C2
      CHARACTER*3        C3
      INTEGER            I, J, JP, JU, KM
      DOUBLE PRECISION   ADDFAC, ADDS, MULFAC, MULTS
*     ..
*     .. External Functions ..
      LOGICAL            LSAME, LSAMEN
      EXTERNAL           LSAME, LSAMEN
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX, MIN
*     ..
*     .. Executable Statements ..
*
      DOPGB = 0
      MULTS = 0
      ADDS = 0
      C1 = SUBNAM( 1: 1 )
      C2 = SUBNAM( 2: 3 )
      C3 = SUBNAM( 4: 6 )
      SORD = LSAME( C1, 'S' ) .OR. LSAME( C1, 'D' )
      CORZ = LSAME( C1, 'C' ) .OR. LSAME( C1, 'Z' )
      IF( .NOT.( SORD .OR. CORZ ) )
     $   RETURN
      IF( LSAME( C1, 'S' ) .OR. LSAME( C1, 'D' ) ) THEN
         ADDFAC = 1
         MULFAC = 1
      ELSE
         ADDFAC = 2
         MULFAC = 6
      END IF
*
*     --------------------------
*     GB:  General Band matrices
*     --------------------------
*
      IF( LSAMEN( 2, C2, 'GB' ) ) THEN
*
*        xGBTRF:  M, N, KL, KU  =>  M, N, KL, KU
*
         IF( LSAMEN( 3, C3, 'TRF' ) ) THEN
            JU = 1
            DO 10 J = 1, MIN( M, N )
               KM = MIN( KL, M-J )
               JP = IPIV( J )
               JU = MAX( JU, MIN( JP+KU, N ) )
               IF( KM.GT.0 ) THEN
                  MULTS = MULTS + KM*( 1+JU-J )
                  ADDS = ADDS + KM*( JU-J )
               END IF
   10       CONTINUE
         END IF
*
*     ---------------------------------
*     GT:  General Tridiagonal matrices
*     ---------------------------------
*
      ELSE IF( LSAMEN( 2, C2, 'GT' ) ) THEN
*
*        xGTTRF:  N  =>  M
*
         IF( LSAMEN( 3, C3, 'TRF' ) ) THEN
            MULTS = 2*( M-1 )
            ADDS = M - 1
            DO 20 I = 1, M - 2
               IF( IPIV( I ).NE.I )
     $            MULTS = MULTS + 1
   20       CONTINUE
*
*        xGTTRS:  N, NRHS  =>  M, N
*
         ELSE IF( LSAMEN( 3, C3, 'TRS' ) ) THEN
            MULTS = 4*N*( M-1 )
            ADDS = 3*N*( M-1 )
*
*        xGTSV:   N, NRHS  =>  M, N
*
         ELSE IF( LSAMEN( 3, C3, 'SV ' ) ) THEN
            MULTS = ( 4*N+2 )*( M-1 )
            ADDS = ( 3*N+1 )*( M-1 )
            DO 30 I = 1, M - 2
               IF( IPIV( I ).NE.I )
     $            MULTS = MULTS + 1
   30       CONTINUE
         END IF
      END IF
*
      DOPGB = MULFAC*MULTS + ADDFAC*ADDS
      RETURN
*
*     End of DOPGB
*
      END
      DOUBLE PRECISION FUNCTION DOPLA( SUBNAM, M, N, KL, KU, NB )
*
*  -- LAPACK timing routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     June 30, 1999
*
*     .. Scalar Arguments ..
      CHARACTER*6        SUBNAM
      INTEGER            KL, KU, M, N, NB
*     ..
*
*  Purpose
*  =======
*
*  DOPLA computes an approximation of the number of floating point
*  operations used by the subroutine SUBNAM with the given values
*  of the parameters M, N, KL, KU, and NB.
*
*  This version counts operations for the LAPACK subroutines.
*
*  Arguments
*  =========
*
*  SUBNAM  (input) CHARACTER*6
*          The name of the subroutine.
*
*  M       (input) INTEGER
*          The number of rows of the coefficient matrix.  M >= 0.
*
*  N       (input) INTEGER
*          The number of columns of the coefficient matrix.
*          For solve routine when the matrix is square,
*          N is the number of right hand sides.  N >= 0.
*
*  KL      (input) INTEGER
*          The lower band width of the coefficient matrix.
*          If needed, 0 <= KL <= M-1.
*          For xGEQRS, KL is the number of right hand sides.
*
*  KU      (input) INTEGER
*          The upper band width of the coefficient matrix.
*          If needed, 0 <= KU <= N-1.
*
*  NB      (input) INTEGER
*          The block size.  If needed, NB >= 1.
*
*  Notes
*  =====
*
*  In the comments below, the association is given between arguments
*  in the requested subroutine and local arguments.  For example,
*
*  xGETRS:  N, NRHS  =>  M, N
*
*  means that arguments N and NRHS in DGETRS are passed to arguments
*  M and N in this procedure.
*
*  =====================================================================
*
*     .. Local Scalars ..
      LOGICAL            CORZ, SORD
      CHARACTER          C1
      CHARACTER*2        C2
      CHARACTER*3        C3
      INTEGER            I
      DOUBLE PRECISION   ADDFAC, ADDS, EK, EM, EMN, EN, MULFAC, MULTS,
     $                   WL, WU
*     ..
*     .. External Functions ..
      LOGICAL            LSAME, LSAMEN
      EXTERNAL           LSAME, LSAMEN
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX, MIN
*     ..
*     .. Executable Statements ..
*
*     --------------------------------------------------------
*     Initialize DOPLA to 0 and do a quick return if possible.
*     --------------------------------------------------------
*
      DOPLA = 0
      MULTS = 0
      ADDS = 0
      C1 = SUBNAM( 1: 1 )
      C2 = SUBNAM( 2: 3 )
      C3 = SUBNAM( 4: 6 )
      SORD = LSAME( C1, 'S' ) .OR. LSAME( C1, 'D' )
      CORZ = LSAME( C1, 'C' ) .OR. LSAME( C1, 'Z' )
      IF( M.LE.0 .OR. .NOT.( SORD .OR. CORZ ) )
     $   RETURN
*
*     ---------------------------------------------------------
*     If the coefficient matrix is real, count each add as 1
*     operation and each multiply as 1 operation.
*     If the coefficient matrix is complex, count each add as 2
*     operations and each multiply as 6 operations.
*     ---------------------------------------------------------
*
      IF( LSAME( C1, 'S' ) .OR. LSAME( C1, 'D' ) ) THEN
         ADDFAC = 1
         MULFAC = 1
      ELSE
         ADDFAC = 2
         MULFAC = 6
      END IF
      EM = M
      EN = N
      EK = KL
*
*     ---------------------------------
*     GE:  GEneral rectangular matrices
*     ---------------------------------
*
      IF( LSAMEN( 2, C2, 'GE' ) ) THEN
*
*        xGETRF:  M, N  =>  M, N
*
         IF( LSAMEN( 3, C3, 'TRF' ) ) THEN
            EMN = MIN( M, N )
            ADDS = EMN*( EM*EN-( EM+EN )*( EMN+1.D0 ) / 2.D0+
     $             ( EMN+1.D0 )*( 2.D0*EMN+1.D0 ) / 6.D0 )
            MULTS = ADDS + EMN*( EM-( EMN+1.D0 ) / 2.D0 )
*
*        xGETRS:  N, NRHS  =>  M, N
*
         ELSE IF( LSAMEN( 3, C3, 'TRS' ) ) THEN
            MULTS = EN*EM*EM
            ADDS = EN*( EM*( EM-1.D0 ) )
*
*        xGETRI:  N  =>  M
*
         ELSE IF( LSAMEN( 3, C3, 'TRI' ) ) THEN
            MULTS = EM*( 5.D0 / 6.D0+EM*( 1.D0 / 2.D0+EM*( 2.D0 /
     $              3.D0 ) ) )
            ADDS = EM*( 5.D0 / 6.D0+EM*( -3.D0 / 2.D0+EM*( 2.D0 /
     $             3.D0 ) ) )
*
*        xGEQRF or xGEQLF:  M, N  =>  M, N
*
         ELSE IF( LSAMEN( 3, C3, 'QRF' ) .OR.
     $            LSAMEN( 3, C3, 'QR2' ) .OR.
     $            LSAMEN( 3, C3, 'QLF' ) .OR. LSAMEN( 3, C3, 'QL2' ) )
     $             THEN
            IF( M.GE.N ) THEN
               MULTS = EN*( ( ( 23.D0 / 6.D0 )+EM+EN / 2.D0 )+EN*
     $                 ( EM-EN / 3.D0 ) )
               ADDS = EN*( ( 5.D0 / 6.D0 )+EN*
     $                ( 1.D0 / 2.D0+( EM-EN / 3.D0 ) ) )
            ELSE
               MULTS = EM*( ( ( 23.D0 / 6.D0 )+2.D0*EN-EM / 2.D0 )+EM*
     $                 ( EN-EM / 3.D0 ) )
               ADDS = EM*( ( 5.D0 / 6.D0 )+EN-EM / 2.D0+EM*
     $                ( EN-EM / 3.D0 ) )
            END IF
*
*        xGERQF or xGELQF:  M, N  =>  M, N
*
         ELSE IF( LSAMEN( 3, C3, 'RQF' ) .OR.
     $            LSAMEN( 3, C3, 'RQ2' ) .OR.
     $            LSAMEN( 3, C3, 'LQF' ) .OR. LSAMEN( 3, C3, 'LQ2' ) )
     $             THEN
            IF( M.GE.N ) THEN
               MULTS = EN*( ( ( 29.D0 / 6.D0 )+EM+EN / 2.D0 )+EN*
     $                 ( EM-EN / 3.D0 ) )
               ADDS = EN*( ( 5.D0 / 6.D0 )+EM+EN*
     $                ( -1.D0 / 2.D0+( EM-EN / 3.D0 ) ) )
            ELSE
               MULTS = EM*( ( ( 29.D0 / 6.D0 )+2.D0*EN-EM / 2.D0 )+EM*
     $                 ( EN-EM / 3.D0 ) )
               ADDS = EM*( ( 5.D0 / 6.D0 )+EM / 2.D0+EM*
     $                ( EN-EM / 3.D0 ) )
            END IF
*
*        xGEQPF: M, N => M, N
*
         ELSE IF( LSAMEN( 3, C3, 'QPF' ) ) THEN
            EMN = MIN( M, N )
            MULTS = 2*EN*EN + EMN*( 3*EM+5*EN+2*EM*EN-( EMN+1 )*
     $              ( 4+EN+EM-( 2*EMN+1 ) / 3 ) )
            ADDS = EN*EN + EMN*( 2*EM+EN+2*EM*EN-( EMN+1 )*
     $             ( 2+EN+EM-( 2*EMN+1 ) / 3 ) )
*
*        xGEQRS or xGERQS:  M, N, NRHS  =>  M, N, KL
*
         ELSE IF( LSAMEN( 3, C3, 'QRS' ) .OR. LSAMEN( 3, C3, 'RQS' ) )
     $             THEN
            MULTS = EK*( EN*( 2.D0-EK )+EM*
     $              ( 2.D0*EN+( EM+1.D0 ) / 2.D0 ) )
            ADDS = EK*( EN*( 1.D0-EK )+EM*
     $             ( 2.D0*EN+( EM-1.D0 ) / 2.D0 ) )
*
*        xGELQS or xGEQLS:  M, N, NRHS  =>  M, N, KL
*
         ELSE IF( LSAMEN( 3, C3, 'LQS' ) .OR. LSAMEN( 3, C3, 'QLS' ) )
     $             THEN
            MULTS = EK*( EM*( 2.D0-EK )+EN*
     $              ( 2.D0*EM+( EN+1.D0 ) / 2.D0 ) )
            ADDS = EK*( EM*( 1.D0-EK )+EN*
     $             ( 2.D0*EM+( EN-1.D0 ) / 2.D0 ) )
*
*        xGEBRD:  M, N  =>  M, N
*
         ELSE IF( LSAMEN( 3, C3, 'BRD' ) ) THEN
            IF( M.GE.N ) THEN
               MULTS = EN*( 20.D0 / 3.D0+EN*
     $                 ( 2.D0+( 2.D0*EM-( 2.D0 / 3.D0 )*EN ) ) )
               ADDS = EN*( 5.D0 / 3.D0+( EN-EM )+EN*
     $                ( 2.D0*EM-( 2.D0 / 3.D0 )*EN ) )
            ELSE
               MULTS = EM*( 20.D0 / 3.D0+EM*
     $                 ( 2.D0+( 2.D0*EN-( 2.D0 / 3.D0 )*EM ) ) )
               ADDS = EM*( 5.D0 / 3.D0+( EM-EN )+EM*
     $                ( 2.D0*EN-( 2.D0 / 3.D0 )*EM ) )
            END IF
*
*        xGEHRD:  N  =>  M
*
         ELSE IF( LSAMEN( 3, C3, 'HRD' ) ) THEN
            IF( M.EQ.1 ) THEN
               MULTS = 0.D0
               ADDS = 0.D0
            ELSE
               MULTS = -13.D0 + EM*( -7.D0 / 6.D0+EM*
     $                 ( 0.5D0+EM*( 5.D0 / 3.D0 ) ) )
               ADDS = -8.D0 + EM*( -2.D0 / 3.D0+EM*
     $                ( -1.D0+EM*( 5.D0 / 3.D0 ) ) )
            END IF
*
         END IF
*
*     ----------------------------
*     GB:  General Banded matrices
*     ----------------------------
*        Note:  The operation count is overestimated because
*        it is assumed that the factor U fills in to the maximum
*        extent, i.e., that its bandwidth goes from KU to KL + KU.
*
      ELSE IF( LSAMEN( 2, C2, 'GB' ) ) THEN
*
*        xGBTRF:  M, N, KL, KU  =>  M, N, KL, KU
*
         IF( LSAMEN( 3, C3, 'TRF' ) ) THEN
            DO 10 I = MIN( M, N ), 1, -1
               WL = MAX( 0, MIN( KL, M-I ) )
               WU = MAX( 0, MIN( KL+KU, N-I ) )
               MULTS = MULTS + WL*( 1.D0+WU )
               ADDS = ADDS + WL*WU
   10       CONTINUE
*
*        xGBTRS:  N, NRHS, KL, KU  =>  M, N, KL, KU
*
         ELSE IF( LSAMEN( 3, C3, 'TRS' ) ) THEN
            WL = MAX( 0, MIN( KL, M-1 ) )
            WU = MAX( 0, MIN( KL+KU, M-1 ) )
            MULTS = EN*( EM*( WL+1.D0+WU )-0.5D0*
     $              ( WL*( WL+1.D0 )+WU*( WU+1.D0 ) ) )
            ADDS = EN*( EM*( WL+WU )-0.5D0*
     $             ( WL*( WL+1.D0 )+WU*( WU+1.D0 ) ) )
*
         END IF
*
*     --------------------------------------
*     PO:  POsitive definite matrices
*     PP:  Positive definite Packed matrices
*     --------------------------------------
*
      ELSE IF( LSAMEN( 2, C2, 'PO' ) .OR. LSAMEN( 2, C2, 'PP' ) ) THEN
*
*        xPOTRF:  N  =>  M
*
         IF( LSAMEN( 3, C3, 'TRF' ) ) THEN
            MULTS = EM*( 1.D0 / 3.D0+EM*( 1.D0 / 2.D0+EM*( 1.D0 /
     $              6.D0 ) ) )
            ADDS = ( 1.D0 / 6.D0 )*EM*( -1.D0+EM*EM )
*
*        xPOTRS:  N, NRHS  =>  M, N
*
         ELSE IF( LSAMEN( 3, C3, 'TRS' ) ) THEN
            MULTS = EN*( EM*( EM+1.D0 ) )
            ADDS = EN*( EM*( EM-1.D0 ) )
*
*        xPOTRI:  N  =>  M
*
         ELSE IF( LSAMEN( 3, C3, 'TRI' ) ) THEN
            MULTS = EM*( 2.D0 / 3.D0+EM*( 1.D0+EM*( 1.D0 / 3.D0 ) ) )
            ADDS = EM*( 1.D0 / 6.D0+EM*( -1.D0 / 2.D0+EM*( 1.D0 /
     $             3.D0 ) ) )
*
         END IF
*
*     ------------------------------------
*     PB:  Positive definite Band matrices
*     ------------------------------------
*
      ELSE IF( LSAMEN( 2, C2, 'PB' ) ) THEN
*
*        xPBTRF:  N, K  =>  M, KL
*
         IF( LSAMEN( 3, C3, 'TRF' ) ) THEN
            MULTS = EK*( -2.D0 / 3.D0+EK*( -1.D0+EK*( -1.D0 / 3.D0 ) ) )
     $               + EM*( 1.D0+EK*( 3.D0 / 2.D0+EK*( 1.D0 / 2.D0 ) ) )
            ADDS = EK*( -1.D0 / 6.D0+EK*( -1.D0 / 2.D0+EK*( -1.D0 /
     $             3.D0 ) ) ) + EM*( EK / 2.D0*( 1.D0+EK ) )
*
*        xPBTRS:  N, NRHS, K  =>  M, N, KL
*
         ELSE IF( LSAMEN( 3, C3, 'TRS' ) ) THEN
            MULTS = EN*( ( 2*EM-EK )*( EK+1.D0 ) )
            ADDS = EN*( EK*( 2*EM-( EK+1.D0 ) ) )
*
         END IF
*
*     ----------------------------------
*     PT:  Positive definite Tridiagonal
*     ----------------------------------
*
      ELSE IF( LSAMEN( 2, C2, 'PT' ) ) THEN
*
*        xPTTRF:  N  =>  M
*
         IF( LSAMEN( 3, C3, 'TRF' ) ) THEN
            MULTS = 2*( EM-1 )
            ADDS = EM - 1
*
*        xPTTRS:  N, NRHS  =>  M, N
*
         ELSE IF( LSAMEN( 3, C3, 'TRS' ) ) THEN
            MULTS = EN*( 3*EM-2 )
            ADDS = EN*( 2*( EM-1 ) )
*
*        xPTSV:  N, NRHS  =>  M, N
*
         ELSE IF( LSAMEN( 3, C3, 'SV ' ) ) THEN
            MULTS = 2*( EM-1 ) + EN*( 3*EM-2 )
            ADDS = EM - 1 + EN*( 2*( EM-1 ) )
         END IF
*
*     --------------------------------------------------------
*     SY:  SYmmetric indefinite matrices
*     SP:  Symmetric indefinite Packed matrices
*     HE:  HErmitian indefinite matrices (complex only)
*     HP:  Hermitian indefinite Packed matrices (complex only)
*     --------------------------------------------------------
*
      ELSE IF( LSAMEN( 2, C2, 'SY' ) .OR. LSAMEN( 2, C2, 'SP' ) .OR.
     $         LSAMEN( 3, SUBNAM, 'CHE' ) .OR.
     $         LSAMEN( 3, SUBNAM, 'ZHE' ) .OR.
     $         LSAMEN( 3, SUBNAM, 'CHP' ) .OR.
     $         LSAMEN( 3, SUBNAM, 'ZHP' ) ) THEN
*
*        xSYTRF:  N  =>  M
*
         IF( LSAMEN( 3, C3, 'TRF' ) ) THEN
            MULTS = EM*( 10.D0 / 3.D0+EM*
     $              ( 1.D0 / 2.D0+EM*( 1.D0 / 6.D0 ) ) )
            ADDS = EM / 6.D0*( -1.D0+EM*EM )
*
*        xSYTRS:  N, NRHS  =>  M, N
*
         ELSE IF( LSAMEN( 3, C3, 'TRS' ) ) THEN
            MULTS = EN*EM*EM
            ADDS = EN*( EM*( EM-1.D0 ) )
*
*        xSYTRI:  N  =>  M
*
         ELSE IF( LSAMEN( 3, C3, 'TRI' ) ) THEN
            MULTS = EM*( 2.D0 / 3.D0+EM*EM*( 1.D0 / 3.D0 ) )
            ADDS = EM*( -1.D0 / 3.D0+EM*EM*( 1.D0 / 3.D0 ) )
*
*        xSYTRD, xSYTD2:  N  =>  M
*
         ELSE IF( LSAMEN( 3, C3, 'TRD' ) .OR. LSAMEN( 3, C3, 'TD2' ) )
     $             THEN
            IF( M.EQ.1 ) THEN
               MULTS = 0.D0
               ADDS = 0.D0
            ELSE
               MULTS = -15.D0 + EM*( -1.D0 / 6.D0+EM*
     $                 ( 5.D0 / 2.D0+EM*( 2.D0 / 3.D0 ) ) )
               ADDS = -4.D0 + EM*( -8.D0 / 3.D0+EM*
     $                ( 1.D0+EM*( 2.D0 / 3.D0 ) ) )
            END IF
         END IF
*
*     -------------------
*     Triangular matrices
*     -------------------
*
      ELSE IF( LSAMEN( 2, C2, 'TR' ) .OR. LSAMEN( 2, C2, 'TP' ) ) THEN
*
*        xTRTRS:  N, NRHS  =>  M, N
*
         IF( LSAMEN( 3, C3, 'TRS' ) ) THEN
            MULTS = EN*EM*( EM+1.D0 ) / 2.D0
            ADDS = EN*EM*( EM-1.D0 ) / 2.D0
*
*        xTRTRI:  N  =>  M
*
         ELSE IF( LSAMEN( 3, C3, 'TRI' ) ) THEN
            MULTS = EM*( 1.D0 / 3.D0+EM*( 1.D0 / 2.D0+EM*( 1.D0 /
     $              6.D0 ) ) )
            ADDS = EM*( 1.D0 / 3.D0+EM*( -1.D0 / 2.D0+EM*( 1.D0 /
     $             6.D0 ) ) )
*
         END IF
*
      ELSE IF( LSAMEN( 2, C2, 'TB' ) ) THEN
*
*        xTBTRS:  N, NRHS, K  =>  M, N, KL
*
         IF( LSAMEN( 3, C3, 'TRS' ) ) THEN
            MULTS = EN*( EM*( EM+1.D0 ) / 2.D0-( EM-EK-1.D0 )*
     $              ( EM-EK ) / 2.D0 )
            ADDS = EN*( EM*( EM-1.D0 ) / 2.D0-( EM-EK-1.D0 )*( EM-EK ) /
     $             2.D0 )
         END IF
*
*     --------------------
*     Trapezoidal matrices
*     --------------------
*
      ELSE IF( LSAMEN( 2, C2, 'TZ' ) ) THEN
*
*        xTZRQF:  M, N => M, N
*
         IF( LSAMEN( 3, C3, 'RQF' ) ) THEN
            EMN = MIN( M, N )
            MULTS = 3*EM*( EN-EM+1 ) + ( 2*EN-2*EM+3 )*
     $              ( EM*EM-EMN*( EMN+1 ) / 2 )
            ADDS = ( EN-EM+1 )*( EM+2*EM*EM-EMN*( EMN+1 ) )
         END IF
*
*     -------------------
*     Orthogonal matrices
*     -------------------
*
      ELSE IF( ( SORD .AND. LSAMEN( 2, C2, 'OR' ) ) .OR.
     $         ( CORZ .AND. LSAMEN( 2, C2, 'UN' ) ) ) THEN
*
*        -MQR, -MLQ, -MQL, or -MRQ:  M, N, K, SIDE  =>  M, N, KL, KU
*           where KU<= 0 indicates SIDE = 'L'
*           and   KU> 0  indicates SIDE = 'R'
*
         IF( LSAMEN( 3, C3, 'MQR' ) .OR. LSAMEN( 3, C3, 'MLQ' ) .OR.
     $       LSAMEN( 3, C3, 'MQL' ) .OR. LSAMEN( 3, C3, 'MRQ' ) ) THEN
            IF( KU.LE.0 ) THEN
               MULTS = EK*EN*( 2.D0*EM+2.D0-EK )
               ADDS = EK*EN*( 2.D0*EM+1.D0-EK )
            ELSE
               MULTS = EK*( EM*( 2.D0*EN-EK )+
     $                 ( EM+EN+( 1.D0-EK ) / 2.D0 ) )
               ADDS = EK*EM*( 2.D0*EN+1.D0-EK )
            END IF
*
*        -GQR or -GQL:  M, N, K  =>  M, N, KL
*
         ELSE IF( LSAMEN( 3, C3, 'GQR' ) .OR. LSAMEN( 3, C3, 'GQL' ) )
     $             THEN
            MULTS = EK*( -5.D0 / 3.D0+( 2.D0*EN-EK )+
     $              ( 2.D0*EM*EN+EK*( ( 2.D0 / 3.D0 )*EK-EM-EN ) ) )
            ADDS = EK*( 1.D0 / 3.D0+( EN-EM )+
     $             ( 2.D0*EM*EN+EK*( ( 2.D0 / 3.D0 )*EK-EM-EN ) ) )
*
*        -GLQ or -GRQ:  M, N, K  =>  M, N, KL
*
         ELSE IF( LSAMEN( 3, C3, 'GLQ' ) .OR. LSAMEN( 3, C3, 'GRQ' ) )
     $             THEN
            MULTS = EK*( -2.D0 / 3.D0+( EM+EN-EK )+
     $              ( 2.D0*EM*EN+EK*( ( 2.D0 / 3.D0 )*EK-EM-EN ) ) )
            ADDS = EK*( 1.D0 / 3.D0+( EM-EN )+
     $             ( 2.D0*EM*EN+EK*( ( 2.D0 / 3.D0 )*EK-EM-EN ) ) )
*
         END IF
*
      END IF
*
      DOPLA = MULFAC*MULTS + ADDFAC*ADDS
*
      RETURN
*
*     End of DOPLA
*
      END
      SUBROUTINE DPRTB2( LAB1, LAB2, LAB3, NN, NVAL, NLDA, RESLTS, LDR1,
     $                   LDR2, NOUT )
*
*  -- LAPACK timing routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     March 31, 1993
*
*     .. Scalar Arguments ..
      CHARACTER*( * )    LAB1, LAB2, LAB3
      INTEGER            LDR1, LDR2, NLDA, NN, NOUT
*     ..
*     .. Array Arguments ..
      INTEGER            NVAL( NN )
      DOUBLE PRECISION   RESLTS( LDR1, LDR2, * )
*     ..
*
*  Purpose
*  =======
*
*  DPRTB2 prints a table of timing data for the solve routines.
*  There are 4 rows to each table, corresponding to
*  NRHS = 1, 2, N/2, and N,  or  NRHS = 1, 2, K/2, K for the
*  band routines.
*
*  Arguments
*  =========
*
*  LAB1    (input) CHARACTER*(*)
*          The label for the rows.
*
*  LAB2    (input) CHARACTER*(*)
*          The label for the columns.
*
*  LAB3    CHARACTER*(*)
*          The name of the variable used in the row headers (usually
*          N or K).
*
*  NN      (input) INTEGER
*          The number of values of NVAL, and also the number of columns
*          of the table.
*
*  NVAL    (input) INTEGER array, dimension (NN)
*          The values of LAB2 used for the data in each column.
*
*  NLDA    (input) INTEGER
*          The number of values of LDA, hence the number of rows for
*          each value of NRHS.
*
*  RESLTS  (input) DOUBLE PRECISION array, dimension (LDR1, LDR2, NLDA)
*          The timing results for each value of N, K, and LDA.
*
*  LDR1    (input) INTEGER
*          The first dimension of RESLTS.  LDR1 >= 4.
*
*  LDR2    (input) INTEGER
*          The second dimension of RESLTS.  LDR2 >= max( 1, NN ).
*
*  NOUT    (input) INTEGER
*          The unit number on which the table is to be printed.
*          NOUT >= 0.
*
*  =====================================================================
*
*     .. Local Scalars ..
      CHARACTER*6        COLLAB
      INTEGER            I, IC, INB, J, K, LNB
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          LEN, MAX
*     ..
*     .. Executable Statements ..
*
      IF( NOUT.LE.0 )
     $   RETURN
      WRITE( NOUT, FMT = 9999 )LAB2, ( NVAL( I ), I = 1, NN )
      WRITE( NOUT, FMT = 9998 )LAB1
*
*     Find the first and last non-blank characters in LAB3.
*
      INB = 0
      DO 10 I = 1, LEN( LAB3 )
         IF( INB.EQ.0 .AND. LAB3( I: I ).NE.' ' )
     $      INB = I
         IF( LAB3( I: I ).NE.' ' )
     $      LNB = I
   10 CONTINUE
      IF( INB.EQ.0 ) THEN
         INB = 1
         LNB = 1
      END IF
*
      DO 50 I = 1, 4
         IF( I.EQ.1 ) THEN
            COLLAB = '     1'
         ELSE IF( I.EQ.2 ) THEN
            COLLAB = '     2'
         ELSE IF( I.EQ.3 ) THEN
            COLLAB = '    /2'
            DO 20 J = LNB, MAX( INB, LNB-3 ), -1
               IC = 4 - ( LNB-J )
               COLLAB( IC: IC ) = LAB3( J: J )
   20       CONTINUE
         ELSE IF( I.EQ.4 ) THEN
            COLLAB = ' '
            DO 30 J = LNB, MAX( INB, LNB-5 ), -1
               IC = 6 - ( LNB-J )
               COLLAB( IC: IC ) = LAB3( J: J )
   30       CONTINUE
         END IF
         WRITE( NOUT, FMT = 9997 )COLLAB,
     $      ( RESLTS( I, J, 1 ), J = 1, NN )
         DO 40 K = 2, NLDA
            WRITE( NOUT, FMT = 9996 )( RESLTS( I, J, K ), J = 1, NN )
   40    CONTINUE
         IF( NLDA.GT.1 )
     $      WRITE( NOUT, FMT = * )
   50 CONTINUE
      IF( NLDA.EQ.1 )
     $   WRITE( NOUT, FMT = * )
*
 9999 FORMAT( 6X, A4, I6, 11I8 )
 9998 FORMAT( 3X, A4 )
 9997 FORMAT( 1X, A6, 1X, 12F8.1 )
 9996 FORMAT( 8X, 12F8.1 )
*
      RETURN
*
*     End of DPRTB2
*
      END
      SUBROUTINE DPRTB3( LAB1, LAB2, NK, KVAL, LVAL, NN, NVAL, NLDA,
     $                   RESLTS, LDR1, LDR2, NOUT )
*
*  -- LAPACK timing routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     March 31, 1993
*
*     .. Scalar Arguments ..
      CHARACTER*( * )    LAB1, LAB2
      INTEGER            LDR1, LDR2, NK, NLDA, NN, NOUT
*     ..
*     .. Array Arguments ..
      INTEGER            KVAL( NK ), LVAL( NK ), NVAL( NN )
      DOUBLE PRECISION   RESLTS( LDR1, LDR2, * )
*     ..
*
*  Purpose
*  =======
*
*  DPRTB3 prints a table of timing data for the timing programs.
*  The table has NK block rows and NN columns, with NLDA
*  individual rows in each block row.  Each block row depends on two
*  parameters K and L, specified as an ordered pair in the arrays KVAL
*  and LVAL.
*
*  Arguments
*  =========
*
*  LAB1    (input) CHARACTER*(*)
*          The label for the rows.
*
*  LAB2    (input) CHARACTER*(*)
*          The label for the columns.
*
*  NK      (input) INTEGER
*          The number of values of KVAL, and also the number of block
*          rows of the table.
*
*  KVAL    (input) INTEGER array, dimension (NK)
*          The values of the parameter K.  Each block row depends on
*          the pair of parameters (K, L).
*
*  LVAL    (input) INTEGER array, dimension (NK)
*          The values of the parameter L.  Each block row depends on
*          the pair of parameters (K, L).
*
*  NN      (input) INTEGER
*          The number of values of NVAL, and also the number of columns
*          of the table.
*
*  NVAL    (input) INTEGER array, dimension (NN)
*          The values of N used for the data in each column.
*
*  NLDA    (input) INTEGER
*          The number of values of LDA, hence the number of rows for
*          each value of KVAL.
*
*  RESLTS  (input) DOUBLE PRECISION array, dimension (LDR1, LDR2, NLDA)
*          The timing results for each value of N, K, and LDA.
*
*  LDR1    (input) INTEGER
*          The first dimension of RESLTS.  LDR1 >= max(1,NK).
*
*  LDR2    (input) INTEGER
*          The second dimension of RESLTS.  LDR2 >= max(1,NN).
*
*  NOUT    (input) INTEGER
*          The unit number on which the table is to be printed.
*          NOUT >= 0.
*
*  =====================================================================
*
*     .. Local Scalars ..
      INTEGER            I, J, K
*     ..
*     .. Executable Statements ..
*
      IF( NOUT.LE.0 )
     $   RETURN
      WRITE( NOUT, FMT = 9999 )LAB2, ( NVAL( I ), I = 1, NN )
      WRITE( NOUT, FMT = 9998 )LAB1
*
      DO 20 I = 1, NK
         IF( LAB1.EQ.' ' ) THEN
            WRITE( NOUT, FMT = 9996 )( RESLTS( 1, J, 1 ), J = 1, NN )
         ELSE
            WRITE( NOUT, FMT = 9997 )KVAL( I ), LVAL( I ),
     $         ( RESLTS( I, J, 1 ), J = 1, NN )
         END IF
         DO 10 K = 2, NLDA
            WRITE( NOUT, FMT = 9996 )( RESLTS( I, J, K ), J = 1, NN )
   10    CONTINUE
         IF( NLDA.GT.1 )
     $      WRITE( NOUT, FMT = * )
   20 CONTINUE
      IF( NLDA.EQ.1 )
     $   WRITE( NOUT, FMT = * )
      RETURN
*
 9999 FORMAT( 10X, A4, I7, 11I8 )
 9998 FORMAT( 1X, A11 )
 9997 FORMAT( 1X, '(', I4, ',', I4, ') ', 12F8.1 )
 9996 FORMAT( 13X, 12F8.1 )
*
*     End of DPRTB3
*
      END
      SUBROUTINE DPRTB4( LAB1, LABM, LABN, NK, KVAL, LVAL, NM, MVAL,
     $                   NVAL, NLDA, RESLTS, LDR1, LDR2, NOUT )
*
*  -- LAPACK timing routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     March 31, 1993
*
*     .. Scalar Arguments ..
      CHARACTER*( * )    LAB1, LABM, LABN
      INTEGER            LDR1, LDR2, NK, NLDA, NM, NOUT
*     ..
*     .. Array Arguments ..
      INTEGER            KVAL( NK ), LVAL( NK ), MVAL( NM ), NVAL( NM )
      DOUBLE PRECISION   RESLTS( LDR1, LDR2, * )
*     ..
*
*  Purpose
*  =======
*
*  DPRTB4 prints a table of timing data for the timing programs.
*  The table has NK block rows and NM columns, with NLDA
*  individual rows in each block row.  Each block row depends on two
*  parameters K and L, specified as an ordered pair in the arrays KVAL
*  and LVAL, and each column depends on two parameters M and N,
*  specified as an ordered pair in the arrays MVAL and NVAL.
*
*  Arguments
*  =========
*
*  LAB1    (input) CHARACTER*(*)
*          The label for the rows.
*
*  LABM    (input) CHARACTER*(*)
*          The first label for the columns.
*
*  LABN    (input) CHARACTER*(*)
*          The second label for the columns.
*
*  NK      (input) INTEGER
*          The number of values of KVAL and LVAL, and also the number of
*          block rows of the table.  Each block row depends on the pair
*          of parameters (K,L).
*
*  KVAL    (input) INTEGER array, dimension (NK)
*          The values of the parameter K.
*
*  LVAL    (input) INTEGER array, dimension (NK)
*          The values of the parameter L.
*
*  NM      (input) INTEGER
*          The number of values of MVAL and NVAL, and also the number of
*          columns of the table.  Each column depends on the pair of
*          parameters (M,N).
*
*  MVAL    (input) INTEGER array, dimension (NM)
*          The values of the parameter M.
*
*  NVAL    (input) INTEGER array, dimension (NM)
*          The values of the parameter N.
*
*  NLDA    (input) INTEGER
*          The number of values of LDA, hence the number of rows for
*          each pair of values (K,L).
*
*  RESLTS  (input) DOUBLE PRECISION array, dimension (LDR1, LDR2, NLDA)
*          The timing results for each value of (M,N), (K,L), and LDA.
*
*  LDR1    (input) INTEGER
*          The first dimension of RESLTS.  LDR1 >= max(1,NK).
*
*  LDR2    (input) INTEGER
*          The second dimension of RESLTS.  LDR2 >= max(1,NM).
*
*  NOUT    (input) INTEGER
*          The unit number on which the table is to be printed.
*          NOUT >= 0.
*
*  =====================================================================
*
*     .. Local Scalars ..
      INTEGER            I, J, K
*     ..
*     .. Executable Statements ..
*
      IF( NOUT.LE.0 )
     $   RETURN
      WRITE( NOUT, FMT = 9999 )LABM, ( MVAL( I ), I = 1, NM )
      WRITE( NOUT, FMT = 9999 )LABN, ( NVAL( I ), I = 1, NM )
      WRITE( NOUT, FMT = 9998 )LAB1
*
      DO 20 I = 1, NK
         IF( LAB1.EQ.' ' ) THEN
            WRITE( NOUT, FMT = 9996 )( RESLTS( 1, J, 1 ), J = 1, NM )
         ELSE
            WRITE( NOUT, FMT = 9997 )KVAL( I ), LVAL( I ),
     $         ( RESLTS( I, J, 1 ), J = 1, NM )
         END IF
         DO 10 K = 2, NLDA
            WRITE( NOUT, FMT = 9996 )( RESLTS( I, J, K ), J = 1, NM )
   10    CONTINUE
         IF( NLDA.GT.1 )
     $      WRITE( NOUT, FMT = * )
   20 CONTINUE
      IF( NLDA.EQ.1 )
     $   WRITE( NOUT, FMT = * )
      RETURN
*
 9999 FORMAT( 10X, A4, I7, 11I8 )
 9998 FORMAT( 1X, A11 )
 9997 FORMAT( 1X, '(', I4, ',', I4, ') ', 12F8.1 )
 9996 FORMAT( 13X, 12F8.1 )
*
*     End of DPRTB4
*
      END
      SUBROUTINE DPRTB5( LAB1, LABM, LABN, NK, KVAL, NM, MVAL, NVAL,
     $                   NLDA, RESLTS, LDR1, LDR2, NOUT )
*
*  -- LAPACK timing routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     March 31, 1993
*
*     .. Scalar Arguments ..
      CHARACTER*( * )    LAB1, LABM, LABN
      INTEGER            LDR1, LDR2, NK, NLDA, NM, NOUT
*     ..
*     .. Array Arguments ..
      INTEGER            KVAL( NK ), MVAL( NM ), NVAL( NM )
      DOUBLE PRECISION   RESLTS( LDR1, LDR2, * )
*     ..
*
*  Purpose
*  =======
*
*  DPRTB5 prints a table of timing data for the timing programs.
*  The table has NK block rows and NM columns, with NLDA
*  individual rows in each block row.  Each column depends on two
*  parameters M and N, specified as an ordered pair in the arrays MVAL
*  and NVAL.
*
*  Arguments
*  =========
*
*  LAB1    (input) CHARACTER*(*)
*          The label for the rows.
*
*  LABM    (input) CHARACTER*(*)
*          The first label for the columns.
*
*  LABN    (input) CHARACTER*(*)
*          The second label for the columns.
*
*  NK      (input) INTEGER
*          The number of values of KVAL, and also the number of block
*          rows of the table.
*
*  KVAL    (input) INTEGER array, dimension (NK)
*          The values of LAB1 used for the data in each block row.
*
*  NM      (input) INTEGER
*          The number of values of MVAL and NVAL, and also the number of
*          columns of the table.  Each column depends on the pair of
*          parameters (M,N).
*
*  MVAL    (input) INTEGER array, dimension (NM)
*          The values of the parameter M.
*
*  NVAL    (input) INTEGER array, dimension (NM)
*          The values of the parameter N.
*
*  NLDA    (input) INTEGER
*          The number of values of LDA, hence the number of rows for
*          each value of KVAL.
*
*  RESLTS  (input) DOUBLE PRECISION array, dimension (LDR1, LDR2, NLDA)
*          The timing results for each value of N, K, and LDA.
*
*  LDR1    (input) INTEGER
*          The first dimension of RESLTS.  LDR1 >= max(1,NK).
*
*  LDR2    (input) INTEGER
*          The second dimension of RESLTS.  LDR2 >= max(1,NM).
*
*  NOUT    (input) INTEGER
*          The unit number on which the table is to be printed.
*          NOUT >= 0.
*
*  =====================================================================
*
*     .. Local Scalars ..
      INTEGER            I, J, K
*     ..
*     .. Executable Statements ..
*
      IF( NOUT.LE.0 )
     $   RETURN
      WRITE( NOUT, FMT = 9999 )LABM, ( MVAL( I ), I = 1, NM )
      WRITE( NOUT, FMT = 9999 )LABN, ( NVAL( I ), I = 1, NM )
      WRITE( NOUT, FMT = 9998 )LAB1
*
      DO 20 I = 1, NK
         IF( LAB1.EQ.' ' ) THEN
            WRITE( NOUT, FMT = 9996 )( RESLTS( 1, J, 1 ), J = 1, NM )
         ELSE
            WRITE( NOUT, FMT = 9997 )KVAL( I ),
     $         ( RESLTS( I, J, 1 ), J = 1, NM )
         END IF
         DO 10 K = 2, NLDA
            WRITE( NOUT, FMT = 9996 )( RESLTS( I, J, K ), J = 1, NM )
   10    CONTINUE
         IF( NLDA.GT.1 )
     $      WRITE( NOUT, FMT = * )
   20 CONTINUE
      IF( NLDA.EQ.1 )
     $   WRITE( NOUT, FMT = * )
      RETURN
*
 9999 FORMAT( 6X, A4, I6, 11I8 )
 9998 FORMAT( 3X, A4 )
 9997 FORMAT( 1X, I6, 1X, 12F8.1 )
 9996 FORMAT( 8X, 12F8.1 )
*
*     End of DPRTB5
*
      END
      SUBROUTINE DPRTBL( LAB1, LAB2, NK, KVAL, NN, NVAL, NLDA, RESLTS,
     $                   LDR1, LDR2, NOUT )
*
*  -- LAPACK timing routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     March 31, 1993
*
*     .. Scalar Arguments ..
      CHARACTER*( * )    LAB1, LAB2
      INTEGER            LDR1, LDR2, NK, NLDA, NN, NOUT
*     ..
*     .. Array Arguments ..
      INTEGER            KVAL( NK ), NVAL( NN )
      DOUBLE PRECISION   RESLTS( LDR1, LDR2, * )
*     ..
*
*  Purpose
*  =======
*
*  DPRTBL prints a table of timing data for the timing programs.
*  The table has NK block rows and NN columns, with NLDA
*  individual rows in each block row.
*
*  Arguments
*  =========
*
*  LAB1    (input) CHARACTER*(*)
*          The label for the rows.
*
*  LAB2    (input) CHARACTER*(*)
*          The label for the columns.
*
*  NK      (input) INTEGER
*          The number of values of KVAL, and also the number of block
*          rows of the table.
*
*  KVAL    (input) INTEGER array, dimension (NK)
*          The values of LAB1 used for the data in each block row.
*
*  NN      (input) INTEGER
*          The number of values of NVAL, and also the number of columns
*          of the table.
*
*  NVAL    (input) INTEGER array, dimension (NN)
*          The values of LAB2 used for the data in each column.
*
*  NLDA    (input) INTEGER
*          The number of values of LDA, hence the number of rows for
*          each value of KVAL.
*
*  RESLTS  (input) DOUBLE PRECISION array, dimension (LDR1, LDR2, NLDA)
*          The timing results for each value of N, K, and LDA.
*
*  LDR1    (input) INTEGER
*          The first dimension of RESLTS.  LDR1 >= max( 1, NK ).
*
*  LDR2    (input) INTEGER
*          The second dimension of RESLTS.  LDR2 >= max( 1, NN ).
*
*  NOUT    (input) INTEGER
*          The unit number on which the table is to be printed.
*          NOUT >= 0.
*
*  =====================================================================
*
*     .. Local Scalars ..
      INTEGER            I, J, K
*     ..
*     .. Executable Statements ..
*
      IF( NOUT.LE.0 )
     $   RETURN
      WRITE( NOUT, FMT = 9999 )LAB2, ( NVAL( I ), I = 1, NN )
      WRITE( NOUT, FMT = 9998 )LAB1
*
      DO 20 I = 1, NK
         IF( LAB1.EQ.' ' ) THEN
            WRITE( NOUT, FMT = 9996 )( RESLTS( 1, J, 1 ), J = 1, NN )
         ELSE
            WRITE( NOUT, FMT = 9997 )KVAL( I ),
     $         ( RESLTS( I, J, 1 ), J = 1, NN )
         END IF
         DO 10 K = 2, NLDA
            WRITE( NOUT, FMT = 9996 )( RESLTS( I, J, K ), J = 1, NN )
   10    CONTINUE
         IF( NLDA.GT.1 )
     $      WRITE( NOUT, FMT = * )
   20 CONTINUE
      IF( NLDA.EQ.1 )
     $   WRITE( NOUT, FMT = * )
      RETURN
*
 9999 FORMAT( 6X, A4, I6, 11I8 )
 9998 FORMAT( 3X, A4 )
 9997 FORMAT( 1X, I6, 1X, 12F8.1 )
 9996 FORMAT( 8X, 12F8.1 )
*
*     End of DPRTBL
*
      END
      SUBROUTINE DPRTLS( ISUB, SUBNAM, NDATA, NM, MVAL, NN, NVAL,
     $                   NNS, NSVAL, NNB, NBVAL, NXVAL, NLDA, LDAVAL, 
     $                   MTYPE, RSLTS, NOUT )
*
*  -- LAPACK timing routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     June 30, 1999
*
*     .. Scalar Arguments ..
      CHARACTER*6        SUBNAM
      INTEGER            ISUB, MTYPE, NDATA, NLDA, NM, NN, NNB,
     $                   NNS, NOUT
*     ..
*     .. Array Arguments ..
      INTEGER            LDAVAL( * ), MVAL( * ), NBVAL( * ),
     $                   NSVAL( * ), NVAL( * ), NXVAL( * )
      DOUBLE PRECISION   RSLTS( 6, 6, * ) 
*     ..
*
*  Purpose
*  =======
*
*  DPRTLS prints a table of timing data for the least squares routines.
*
*  Arguments
*  =========
*
*  ISUB    (input) INTEGER
*          Subroutine index.
*
*  SUBNAM  (input) CHARACTER*6
*          Subroutine name. 
*
*  NDATA   (input) INTEGER
*          Number of components for subroutine SUBNAM.
*
*  NM      (input) INTEGER
*          The number of values of M contained in the vector MVAL.
*
*  MVAL    (input) INTEGER array, dimension (NM)
*          The values of the matrix row dimension M.
*
*  NN      (input) INTEGER
*          The number of values of N contained in the vector NVAL.
*
*  NVAL    (input) INTEGER array, dimension (NN)
*          The values of the matrix column dimension N.
*
*  NNS     (input) INTEGER
*          The number of values of NRHS contained in the vector NSVAL.
*
*  NSVAL   (input) INTEGER array, dimension (NNS)
*          The values of the number of right hand sides NRHS.
*
*  NNB     (input) INTEGER
*          The number of values of NB and NX contained in the
*          vectors NBVAL and NXVAL.  The blocking parameters are used
*          in pairs (NB,NX).
*
*  NBVAL   (input) INTEGER array, dimension (NNB)
*          The values of the blocksize NB.
*
*  NXVAL   (input) INTEGER array, dimension (NNB)
*          The values of the crossover point NX.
*
*  NLDA    (input) INTEGER
*          The number of values of LDA contained in the vector LDAVAL.
*
*  LDAVAL  (input) INTEGER array, dimension (NLDA)
*          The values of the leading dimension of the array A.
*
*  MTYPE   (input) INTEGER
*          Number of matrix types.
*
*  RSLTS   (workspace) DOUBLE PRECISION array
*          dimension( 6, 6, number of runs )
*
*  NOUT    (input) INTEGER
*          The unit number for output.
*
*  =====================================================================
*
*     .. Local Scalars ..
      INTEGER            ICASE, IDATA, ILDA, IM, IN, INB, INS,
     $                   ITYPE, LDA, M, N, NB, NRHS, NX
*     ..
*     .. Executable Statements ..
*
      ICASE = 1
*
      DO 70 IM = 1, NM
         M = MVAL( IM )
         DO 60 IN = 1, NN
            N = NVAL( IN )
            DO 50 INS = 1, NNS
               NRHS = NSVAL( INS )
               DO 40 ILDA = 1, NLDA
                  LDA = MAX( 1, LDAVAL( ILDA ) )
                  IF( ISUB.EQ.2 ) THEN
                     WRITE( NOUT, FMT = 9999 ) M, N, NRHS, LDA
                     WRITE( NOUT, FMT = 9998 ) SUBNAM, ( IDATA,
     $                    IDATA = 1, NDATA-1 )
                     DO 10 ITYPE = 1, MTYPE
                        WRITE( NOUT, FMT = 9997 ) ITYPE,
     $                       ( RSLTS( IDATA, ITYPE, ICASE ),
     $                       IDATA = 1, NDATA )
   10                CONTINUE
                     ICASE = ICASE + 1
                  ELSE
                     DO 30 INB = 1, NNB
                        NB = NBVAL( INB )
                        NX = NXVAL( INB )
                        WRITE( NOUT, FMT = 9996 ) M, N, NRHS, LDA,
     $                       NB, NX               
                        WRITE( NOUT, FMT = 9998 ) SUBNAM, ( IDATA,
     $                       IDATA = 1, NDATA-1 )
                        DO 20 ITYPE = 1, MTYPE
                           WRITE( NOUT, FMT = 9997 ) ITYPE,
     $                          ( RSLTS( IDATA, ITYPE, ICASE ),
     $                          IDATA = 1, NDATA )
   20                   CONTINUE
                        ICASE = ICASE + 1
   30                CONTINUE
                  END IF
   40          CONTINUE
   50       CONTINUE
   60    CONTINUE
   70 CONTINUE
*   
 9999 FORMAT( / ' M = ', I5, ', N = ', I5, ', NRHS = ', I5,
     $        ', LDA = ', I5 )
 9998 FORMAT( / ' TYPE ', 4X, A6, 1X, 8( 4X, 'comp.', I2, : ) )
 9997 FORMAT( I5, 2X, 1P, 6G11.2 )
 9996 FORMAT( / ' M = ', I5, ', N = ', I5, ', NRHS = ', I5,
     $        ', LDA = ', I5, ', NB = ', I3, ', NX = ', I3 )
      RETURN
*
*     End of DPRTLS
*
      END
      SUBROUTINE DQRT13( SCALE, M, N, A, LDA, NORMA, ISEED )
*
*  -- LAPACK test routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     February 29, 1992
*
*     .. Scalar Arguments ..
      INTEGER            LDA, M, N, SCALE
      DOUBLE PRECISION   NORMA
*     ..
*     .. Array Arguments ..
      INTEGER            ISEED( 4 )
      DOUBLE PRECISION   A( LDA, * )
*     ..
*
*  Purpose
*  =======
*
*  DQRT13 generates a full-rank matrix that may be scaled to have large
*  or small norm.
*
*  Arguments
*  =========
*
*  SCALE   (input) INTEGER
*          SCALE = 1: normally scaled matrix
*          SCALE = 2: matrix scaled up
*          SCALE = 3: matrix scaled down
*
*  M       (input) INTEGER
*          The number of rows of the matrix A.
*
*  N       (input) INTEGER
*          The number of columns of A.
*
*  A       (output) DOUBLE PRECISION array, dimension (LDA,N)
*          The M-by-N matrix A.
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A.
*
*  NORMA   (output) DOUBLE PRECISION
*          The one-norm of A.
*
*  ISEED   (input/output) integer array, dimension (4)
*          Seed for random number generator
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ONE
      PARAMETER          ( ONE = 1.0D0 )
*     ..
*     .. Local Scalars ..
      INTEGER            INFO, J
      DOUBLE PRECISION   BIGNUM, SMLNUM
*     ..
*     .. External Functions ..
      DOUBLE PRECISION   DASUM, DLAMCH, DLANGE
      EXTERNAL           DASUM, DLAMCH, DLANGE
*     ..
*     .. External Subroutines ..
      EXTERNAL           DLABAD, DLARNV, DLASCL
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          SIGN
*     ..
*     .. Local Arrays ..
      DOUBLE PRECISION   DUMMY( 1 )
*     ..
*     .. Executable Statements ..
*
      IF( M.LE.0 .OR. N.LE.0 )
     $   RETURN
*
*     benign matrix
*
      DO 10 J = 1, N
         CALL DLARNV( 2, ISEED, M, A( 1, J ) )
         IF( J.LE.M ) THEN
            A( J, J ) = A( J, J ) + SIGN( DASUM( M, A( 1, J ), 1 ),
     $                  A( J, J ) )
         END IF
   10 CONTINUE
*
*     scaled versions
*
      IF( SCALE.NE.1 ) THEN
         NORMA = DLANGE( 'Max', M, N, A, LDA, DUMMY )
         SMLNUM = DLAMCH( 'Safe minimum' )
         BIGNUM = ONE / SMLNUM
         CALL DLABAD( SMLNUM, BIGNUM )
         SMLNUM = SMLNUM / DLAMCH( 'Epsilon' )
         BIGNUM = ONE / SMLNUM
*
         IF( SCALE.EQ.2 ) THEN
*
*           matrix scaled up
*
            CALL DLASCL( 'General', 0, 0, NORMA, BIGNUM, M, N, A, LDA,
     $                   INFO )
         ELSE IF( SCALE.EQ.3 ) THEN
*
*           matrix scaled down
*
            CALL DLASCL( 'General', 0, 0, NORMA, SMLNUM, M, N, A, LDA,
     $                   INFO )
         END IF
      END IF
*
      NORMA = DLANGE( 'One-norm', M, N, A, LDA, DUMMY )
      RETURN
*
*     End of DQRT13
*
      END
      SUBROUTINE DQRT15( SCALE, RKSEL, M, N, NRHS, A, LDA, B, LDB, S,
     $                   RANK, NORMA, NORMB, ISEED, WORK, LWORK )
*
*  -- LAPACK test routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     September 30, 1994
*
*     .. Scalar Arguments ..
      INTEGER            LDA, LDB, LWORK, M, N, NRHS, RANK, RKSEL, SCALE
      DOUBLE PRECISION   NORMA, NORMB
*     ..
*     .. Array Arguments ..
      INTEGER            ISEED( 4 )
      DOUBLE PRECISION   A( LDA, * ), B( LDB, * ), S( * ), WORK( LWORK )
*     ..
*
*  Purpose
*  =======
*
*  DQRT15 generates a matrix with full or deficient rank and of various
*  norms.
*
*  Arguments
*  =========
*
*  SCALE   (input) INTEGER
*          SCALE = 1: normally scaled matrix
*          SCALE = 2: matrix scaled up
*          SCALE = 3: matrix scaled down
*
*  RKSEL   (input) INTEGER
*          RKSEL = 1: full rank matrix
*          RKSEL = 2: rank-deficient matrix
*
*  M       (input) INTEGER
*          The number of rows of the matrix A.
*
*  N       (input) INTEGER
*          The number of columns of A.
*
*  NRHS    (input) INTEGER
*          The number of columns of B.
*
*  A       (output) DOUBLE PRECISION array, dimension (LDA,N)
*          The M-by-N matrix A.
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A.
*
*  B       (output) DOUBLE PRECISION array, dimension (LDB, NRHS)
*          A matrix that is in the range space of matrix A.
*
*  LDB     (input) INTEGER
*          The leading dimension of the array B.
*
*  S       (output) DOUBLE PRECISION array, dimension MIN(M,N)
*          Singular values of A.
*
*  RANK    (output) INTEGER
*          number of nonzero singular values of A.
*
*  NORMA   (output) DOUBLE PRECISION
*          one-norm of A.
*
*  NORMB   (output) DOUBLE PRECISION
*          one-norm of B.
*
*  ISEED   (input/output) integer array, dimension (4)
*          seed for random number generator.
*
*  WORK    (workspace) DOUBLE PRECISION array, dimension (LWORK)
*
*  LWORK   (input) INTEGER
*          length of work space required.
*          LWORK >= MAX(M+MIN(M,N),NRHS*MIN(M,N),2*N+M)
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE, TWO, SVMIN
      PARAMETER          ( ZERO = 0.0D0, ONE = 1.0D0, TWO = 2.0D0,
     $                   SVMIN = 0.1D0 )
*     ..
*     .. Local Scalars ..
      INTEGER            INFO, J, MN
      DOUBLE PRECISION   BIGNUM, EPS, SMLNUM, TEMP
*     ..
*     .. Local Arrays ..
      DOUBLE PRECISION   DUMMY( 1 )
*     ..
*     .. External Functions ..
      DOUBLE PRECISION   DASUM, DLAMCH, DLANGE, DLARND, DNRM2
      EXTERNAL           DASUM, DLAMCH, DLANGE, DLARND, DNRM2
*     ..
*     .. External Subroutines ..
      EXTERNAL           DGEMM, DLAORD, DLARF, DLARNV, DLAROR, DLASCL,
     $                   DLASET, DSCAL, XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, MAX, MIN
*     ..
*     .. Executable Statements ..
*
      MN = MIN( M, N )
      IF( LWORK.LT.MAX( M+MN, MN*NRHS, 2*N+M ) ) THEN
         CALL XERBLA( 'DQRT15', 16 )
         RETURN
      END IF
*
      SMLNUM = DLAMCH( 'Safe minimum' )
      BIGNUM = ONE / SMLNUM
      EPS = DLAMCH( 'Epsilon' )
      SMLNUM = ( SMLNUM / EPS ) / EPS
      BIGNUM = ONE / SMLNUM
*
*     Determine rank and (unscaled) singular values
*
      IF( RKSEL.EQ.1 ) THEN
         RANK = MN
      ELSE IF( RKSEL.EQ.2 ) THEN
         RANK = ( 3*MN ) / 4
         DO 10 J = RANK + 1, MN
            S( J ) = ZERO
   10    CONTINUE
      ELSE
         CALL XERBLA( 'DQRT15', 2 )
      END IF
*
      IF( RANK.GT.0 ) THEN
*
*        Nontrivial case
*
         S( 1 ) = ONE
         DO 30 J = 2, RANK
   20       CONTINUE
            TEMP = DLARND( 1, ISEED )
            IF( TEMP.GT.SVMIN ) THEN
               S( J ) = ABS( TEMP )
            ELSE
               GO TO 20
            END IF
   30    CONTINUE
         CALL DLAORD( 'Decreasing', RANK, S, 1 )
*
*        Generate 'rank' columns of a random orthogonal matrix in A
*
         CALL DLARNV( 2, ISEED, M, WORK )
         CALL DSCAL( M, ONE / DNRM2( M, WORK, 1 ), WORK, 1 )
         CALL DLASET( 'Full', M, RANK, ZERO, ONE, A, LDA )
         CALL DLARF( 'Left', M, RANK, WORK, 1, TWO, A, LDA,
     $               WORK( M+1 ) )
*
*        workspace used: m+mn
*
*        Generate consistent rhs in the range space of A
*
         CALL DLARNV( 2, ISEED, RANK*NRHS, WORK )
         CALL DGEMM( 'No transpose', 'No transpose', M, NRHS, RANK, ONE,
     $               A, LDA, WORK, RANK, ZERO, B, LDB )
*
*        work space used: <= mn *nrhs
*
*        generate (unscaled) matrix A
*
         DO 40 J = 1, RANK
            CALL DSCAL( M, S( J ), A( 1, J ), 1 )
   40    CONTINUE
         IF( RANK.LT.N )
     $      CALL DLASET( 'Full', M, N-RANK, ZERO, ZERO, A( 1, RANK+1 ),
     $                   LDA )
         CALL DLAROR( 'Right', 'No initialization', M, N, A, LDA, ISEED,
     $                WORK, INFO )
*
      ELSE
*
*        work space used 2*n+m
*
*        Generate null matrix and rhs
*
         DO 50 J = 1, MN
            S( J ) = ZERO
   50    CONTINUE
         CALL DLASET( 'Full', M, N, ZERO, ZERO, A, LDA )
         CALL DLASET( 'Full', M, NRHS, ZERO, ZERO, B, LDB )
*
      END IF
*
*     Scale the matrix
*
      IF( SCALE.NE.1 ) THEN
         NORMA = DLANGE( 'Max', M, N, A, LDA, DUMMY )
         IF( NORMA.NE.ZERO ) THEN
            IF( SCALE.EQ.2 ) THEN
*
*              matrix scaled up
*
               CALL DLASCL( 'General', 0, 0, NORMA, BIGNUM, M, N, A,
     $                      LDA, INFO )
               CALL DLASCL( 'General', 0, 0, NORMA, BIGNUM, MN, 1, S,
     $                      MN, INFO )
               CALL DLASCL( 'General', 0, 0, NORMA, BIGNUM, M, NRHS, B,
     $                      LDB, INFO )
            ELSE IF( SCALE.EQ.3 ) THEN
*
*              matrix scaled down
*
               CALL DLASCL( 'General', 0, 0, NORMA, SMLNUM, M, N, A,
     $                      LDA, INFO )
               CALL DLASCL( 'General', 0, 0, NORMA, SMLNUM, MN, 1, S,
     $                      MN, INFO )
               CALL DLASCL( 'General', 0, 0, NORMA, SMLNUM, M, NRHS, B,
     $                      LDB, INFO )
            ELSE
               CALL XERBLA( 'DQRT15', 1 )
               RETURN
            END IF
         END IF
      END IF
*
      NORMA = DASUM( MN, S, 1 )
      NORMB = DLANGE( 'One-norm', M, NRHS, B, LDB, DUMMY )
*
      RETURN
*
*     End of DQRT15
*
      END
      PROGRAM DTIMAA
*
*  -- LAPACK timing routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     June 30, 1999
*
*  Purpose
*  =======
*
*  DTIMAA is the timing program for the DOUBLE PRECISION LAPACK
*  routines.  This program collects performance data for the factor,
*  solve, and inverse routines used in solving systems of linear
*  equations, and also for the orthogonal factorization and reduction
*  routines used in solving least squares problems and matrix eigenvalue
*  problems.
*
*  The subprograms call a DOUBLE PRECISION function DSECND with no
*  arguments which is assumed to return the central-processor time in
*  seconds from some fixed starting time.
*
*  The program is driven by a short data file, which specifies values
*  for the matrix dimensions M, N and K, for the blocking parameters
*  NB and NX, and for the leading array dimension LDA.  A minimum time
*  for each subroutine is included for timing small problems or for
*  obtaining results on a machine with an inaccurate DSECND function.
*
*  The matrix dimensions M, N, and K correspond to the three dimensions
*  m, n, and k in the Level 3 BLAS.  When timing the LAPACK routines for
*  square matrices, M and N correspond to the matrix dimensions m and n,
*  and K is the number of right-hand sides (nrhs) for the solves.  When
*  timing the LAPACK routines for band matrices, M is the matrix order
*  m, N is the half-bandwidth (kl, ku, or kd in the LAPACK notation),
*  and K is again the number of right-hand sides.
*
*  The first 13 records of the data file are read using list-directed
*  input.  The first line of input is printed as the first line of
*  output and can be used to identify different sets of results.  To
*  assist with debugging an input file, the values are printed out as
*  they are read in.
*
*  The following records are read using the format (A).  For these
*  records, the first 6 characters are reserved for the path or
*  subroutine name.  If a path name is used, the characters after the
*  path name indicate the routines in the path to be timed, where
*  'T' or 't' means 'Time this routine'.  If the line is blank after the
*  path name, all routines in the path are timed.  If fewer characters
*  appear than routines in a path, the remaining characters are assumed
*  to be 'F'.  For example, the following 3 lines are equivalent ways of
*  requesting timing of DGETRF:
*  DGE    T F F
*  DGE    T
*  DGETRF
*
*  An annotated example of a data file can be obtained by deleting the
*  first 3 characters from the following 30 lines:
*  LAPACK timing, DOUBLE PRECISION square matrices
*  5                                Number of values of M
*  100 200 300 400 500              Values of M (row dimension)
*  5                                Number of values of N
*  100 200 300 400 500              Values of N (column dimension)
*  2                                Number of values of K
*  100 400                          Values of K
*  5                                Number of values of NB
*  1 16  32  48  64                 Values of NB (blocksize)
*  0 48 128 128 128                 Values of NX (crossover point)
*  2                                Number of values of LDA
*  512 513                          Values of LDA (leading dimension)
*  0.0                              Minimum time in seconds
*  DGE    T T T
*  DPO    T T T
*  DPP    T T T
*  DSY    T T T
*  DSP    T T T
*  DTR    T T
*  DTP    T T
*  DQR    T T F
*  DLQ    T T F
*  DQL    T T F
*  DRQ    T T F
*  DQP    T
*  DHR    T T F F
*  DTD    T T F F
*  DBR    T F F
*  DLS    T T T T T T
*
*  The routines are timed for all combinations of applicable values of
*  M, N, K, NB, NX, and LDA, and for all combinations of options such as
*  UPLO and TRANS.  For Level 2 BLAS timings, values of NB are used for
*  INCX.  Certain subroutines, such as the QR factorization, treat the
*  values of M and N as ordered pairs and operate on M x N matrices.
*
*  Internal Parameters
*  ===================
*
*  NMAX    INTEGER
*          The maximum value of M or N for square matrices.
*
*  LDAMAX  INTEGER
*          The maximum value of LDA.
*
*  NMAXB   INTEGER
*          The maximum value of N for band matrices.
*
*  MAXVAL  INTEGER
*          The maximum number of values that can be read in for M, N,
*          K, NB, or NX.
*
*  MXNLDA  INTEGER
*          The maximum number of values that can be read in for LDA.
*
*  NIN     INTEGER
*          The unit number for input.  Currently set to 5 (std input).
*
*  NOUT    INTEGER
*          The unit number for output.  Currently set to 6 (std output).
*
*  =====================================================================
*
*     .. Parameters ..
      INTEGER            NMAX, LDAMAX, NMAXB
      PARAMETER          ( NMAX = 512, LDAMAX = NMAX+20, NMAXB = 5000 )
      INTEGER            LA
      PARAMETER          ( LA = NMAX*LDAMAX )
      INTEGER            MAXVAL, MXNLDA
      PARAMETER          ( MAXVAL = 12, MXNLDA = 4 )
      INTEGER            MAXPRM
      PARAMETER          ( MAXPRM = MXNLDA*(MAXVAL+1) )
      INTEGER            MAXSZS
      PARAMETER          ( MAXSZS = MAXVAL*MAXVAL*MAXVAL )
      INTEGER            NIN, NOUT
      PARAMETER          ( NIN = 5, NOUT = 6 )
*     ..
*     .. Local Scalars ..
      LOGICAL            BLAS, LDAMOK, LDANOK, LDAOK, MOK, NOK, NXNBOK
      CHARACTER          C1
      CHARACTER*2        C2
      CHARACTER*3        C3
      CHARACTER*80       LINE
      INTEGER            I, I2, J2, L, LDR1, LDR2, LDR3, MAXK, MAXLDA,
     $                   MAXM, MAXN, MAXNB, MKMAX, NEED, NK, NLDA, NM,
     $                   NN, NNB
      DOUBLE PRECISION   S1, S2, TIMMIN
*     ..
*     .. Local Arrays ..
      INTEGER            IWORK( 3*NMAXB ), KVAL( MAXVAL ),
     $                   LDAVAL( MXNLDA ), MVAL( MAXVAL ),
     $                   NBVAL( MAXVAL ), NVAL( MAXVAL ),
     $                   NXVAL( MAXVAL )
      DOUBLE PRECISION   A( LA, 4 ), D( 2*NMAX, 2 ),
     $                   FLPTBL( 6*6*MAXSZS*MAXPRM*5 ),
     $                   OPCTBL( 6*6*MAXSZS*MAXPRM*5 ),
     $                   RESLTS( MAXVAL, MAXVAL, 2*MXNLDA, 4*MAXVAL ),
     $                   S( NMAX*2 ), TIMTBL( 6*6*MAXSZS*MAXPRM*5 ),
     $                   WORK( NMAX, NMAX+MAXVAL+30 )
*     ..
*     .. External Functions ..
      LOGICAL            LSAME, LSAMEN
      DOUBLE PRECISION   DSECND
      EXTERNAL           LSAME, LSAMEN, DSECND
*     ..
*     .. External Subroutines ..
      EXTERNAL           DTIMB2, DTIMB3, DTIMBR, DTIMGB, DTIMGE, DTIMGT,
     $                   DTIMHR, DTIMLQ, DTIMLS, DTIMMM, DTIMMV, DTIMPB,
     $                   DTIMPO, DTIMPP, DTIMPT, DTIMQ3, DTIMQL, DTIMQP,
     $                   DTIMQR, DTIMRQ, DTIMSP, DTIMSY, DTIMTB, DTIMTD,
     $                   DTIMTP, DTIMTR
*     ..
*     .. Scalars in Common ..
      INTEGER            NB, NEISPK, NPROC, NSHIFT
*     ..
*     .. Common blocks ..
      COMMON             / CENVIR / NB, NPROC, NSHIFT, NEISPK
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX
*     ..
*     .. Executable Statements ..
*
      S1 = DSECND( )
      LDR1 = MAXVAL
      LDR2 = MAXVAL
      LDR3 = 2*MXNLDA
      WRITE( NOUT, FMT = 9983 )
*
*     Read the first line.  The first four characters must be 'BLAS'
*     for the BLAS data file format to be used.  Otherwise, the LAPACK
*     data file format is assumed.
*
      READ( NIN, FMT = '( A80 )' )LINE
      BLAS = LSAMEN( 4, LINE, 'BLAS' )
*
*     Find the last non-blank and print the first line of input as the
*     first line of output.
*
      DO 10 L = 80, 1, -1
         IF( LINE( L: L ).NE.' ' )
     $      GO TO 20
   10 CONTINUE
      L = 1
   20 CONTINUE
      WRITE( NOUT, FMT = '( 1X, A, / )' )LINE( 1: L )
      WRITE( NOUT, FMT = 9992 )
*
*     Read in NM and the values for M.
*
      READ( NIN, FMT = * )NM
      IF( NM.GT.MAXVAL ) THEN
         WRITE( NOUT, FMT = 9999 )'M', 'NM', MAXVAL
         NM = MAXVAL
      END IF
      READ( NIN, FMT = * )( MVAL( I ), I = 1, NM )
      WRITE( NOUT, FMT = 9991 )'M:     ', ( MVAL( I ), I = 1, NM )
*
*     Check that  M <= NMAXB for all values of M.
*
      MOK = .TRUE.
      MAXM = 0
      DO 30 I = 1, NM
         MAXM = MAX( MVAL( I ), MAXM )
         IF( MVAL( I ).GT.NMAXB ) THEN
            WRITE( NOUT, FMT = 9997 )'M', MVAL( I ), NMAXB
            MOK = .FALSE.
         END IF
   30 CONTINUE
      IF( .NOT.MOK )
     $   WRITE( NOUT, FMT = * )
*
*     Read in NN and the values for N.
*
      READ( NIN, FMT = * )NN
      IF( NN.GT.MAXVAL ) THEN
         WRITE( NOUT, FMT = 9999 )'N', 'NN', MAXVAL
         NN = MAXVAL
      END IF
      READ( NIN, FMT = * )( NVAL( I ), I = 1, NN )
      WRITE( NOUT, FMT = 9991 )'N:     ', ( NVAL( I ), I = 1, NN )
*
*     Check that  N <= NMAXB for all values of N.
*
      NOK = .TRUE.
      MAXN = 0
      DO 40 I = 1, NN
         MAXN = MAX( NVAL( I ), MAXN )
         IF( NVAL( I ).GT.NMAXB ) THEN
            WRITE( NOUT, FMT = 9997 )'N', NVAL( I ), NMAXB
            NOK = .FALSE.
         END IF
   40 CONTINUE
      IF( .NOT.NOK )
     $   WRITE( NOUT, FMT = * )
*
*     Read in NK and the values for K.
*
      READ( NIN, FMT = * )NK
      IF( NK.GT.MAXVAL ) THEN
         WRITE( NOUT, FMT = 9999 )'K', 'NK', MAXVAL
         NK = MAXVAL
      END IF
      READ( NIN, FMT = * )( KVAL( I ), I = 1, NK )
      WRITE( NOUT, FMT = 9991 )'K:     ', ( KVAL( I ), I = 1, NK )
*
*     Find the maximum value of K (= NRHS).
*
      MAXK = 0
      DO 50 I = 1, NK
         MAXK = MAX( KVAL( I ), MAXK )
   50 CONTINUE
      MKMAX = MAXM*MAX( 2, MAXK )
*
*     Read in NNB and the values for NB.  For the BLAS input files,
*     NBVAL is used to store values for INCX and INCY.
*
      READ( NIN, FMT = * )NNB
      IF( NNB.GT.MAXVAL ) THEN
         WRITE( NOUT, FMT = 9999 )'NB', 'NNB', MAXVAL
         NNB = MAXVAL
      END IF
      READ( NIN, FMT = * )( NBVAL( I ), I = 1, NNB )
*
*     Find the maximum value of NB.
*
      MAXNB = 0
      DO 60 I = 1, NNB
         MAXNB = MAX( NBVAL( I ), MAXNB )
   60 CONTINUE
*
      IF( BLAS ) THEN
         WRITE( NOUT, FMT = 9991 )'INCX:  ', ( NBVAL( I ), I = 1, NNB )
         DO 70 I = 1, NNB
            NXVAL( I ) = 0
   70    CONTINUE
      ELSE
*
*        LAPACK data files:  Read in the values for NX.
*
         READ( NIN, FMT = * )( NXVAL( I ), I = 1, NNB )
*
         WRITE( NOUT, FMT = 9991 )'NB:    ', ( NBVAL( I ), I = 1, NNB )
         WRITE( NOUT, FMT = 9991 )'NX:    ', ( NXVAL( I ), I = 1, NNB )
      END IF
*
*     Read in NLDA and the values for LDA.
*
      READ( NIN, FMT = * )NLDA
      IF( NLDA.GT.MXNLDA ) THEN
         WRITE( NOUT, FMT = 9999 )'LDA', 'NLDA', MXNLDA
         NLDA = MXNLDA
      END IF
      READ( NIN, FMT = * )( LDAVAL( I ), I = 1, NLDA )
      WRITE( NOUT, FMT = 9991 )'LDA:   ', ( LDAVAL( I ), I = 1, NLDA )
*
*     Check that LDA >= 1 for all values of LDA.
*
      LDAOK = .TRUE.
      MAXLDA = 0
      DO 80 I = 1, NLDA
         MAXLDA = MAX( LDAVAL( I ), MAXLDA )
         IF( LDAVAL( I ).LE.0 ) THEN
            WRITE( NOUT, FMT = 9998 )LDAVAL( I )
            LDAOK = .FALSE.
         END IF
   80 CONTINUE
      IF( .NOT.LDAOK )
     $   WRITE( NOUT, FMT = * )
*
*     Check that MAXLDA*MAXN <= LA (for the dense routines).
*
      LDANOK = .TRUE.
      NEED = MAXLDA*MAXN
      IF( NEED.GT.LA ) THEN
         WRITE( NOUT, FMT = 9995 )MAXLDA, MAXN, NEED
         LDANOK = .FALSE.
      END IF
*
*     Check that MAXLDA*MAXM + MAXM*MAXK <= 3*LA (for band routines).
*
      LDAMOK = .TRUE.
      NEED = MAXLDA*MAXM + MAXM*MAXK
      IF( NEED.GT.3*LA ) THEN
         NEED = ( NEED+2 ) / 3
         WRITE( NOUT, FMT = 9994 )MAXLDA, MAXM, MAXK, NEED
         LDAMOK = .FALSE.
      END IF
*
*     Check that MAXN*MAXNB (or MAXN*INCX) <= LA.
*
      NXNBOK = .TRUE.
      NEED = MAXN*MAXNB
      IF( NEED.GT.LA ) THEN
         WRITE( NOUT, FMT = 9996 )MAXN, MAXNB, NEED
         NXNBOK = .FALSE.
      END IF
*
      IF( .NOT.( MOK .AND. NOK .AND. LDAOK .AND. LDANOK .AND. NXNBOK ) )
     $     THEN
         WRITE( NOUT, FMT = 9984 )
         GO TO 110
      END IF
      IF( .NOT.LDAMOK )
     $   WRITE( NOUT, FMT = * )
*
*     Read the minimum time to time a subroutine.
*
      WRITE( NOUT, FMT = * )
      READ( NIN, FMT = * )TIMMIN
      WRITE( NOUT, FMT = 9993 )TIMMIN
      WRITE( NOUT, FMT = * )
*
*     Read the first input line.
*
      READ( NIN, FMT = '(A)', END = 100 )LINE
*
*     If the first record is the special signal 'NONE', then get the
*     next line but don't time DGEMV and SGEMM.
*
      IF( LSAMEN( 4, LINE, 'NONE' ) ) THEN
         READ( NIN, FMT = '(A)', END = 100 )LINE
      ELSE
         WRITE( NOUT, FMT = 9990 )
*
*        If the first record is the special signal 'BAND', then time
*        the band routine DGBMV and DGEMM with N = K.
*
         IF( LSAMEN( 4, LINE, 'BAND' ) ) THEN
            IF( LDAMOK ) THEN
               IF( MKMAX.GT.LA ) THEN
                  I2 = 2*LA - MKMAX + 1
                  J2 = 2
               ELSE
                  I2 = LA - MKMAX + 1
                  J2 = 3
               END IF
               CALL DTIMMV( 'DGBMV ', NM, MVAL, NN, NVAL, NLDA, LDAVAL,
     $                      TIMMIN, A( 1, 1 ), MKMAX / 2, A( I2, J2 ),
     $                      A( LA-MKMAX / 2+1, 3 ), RESLTS, LDR1, LDR2,
     $                      NOUT )
            ELSE
               WRITE( NOUT, FMT = 9989 )'DGBMV '
            END IF
            CALL DTIMMM( 'DGEMM ', 'K', NN, NVAL, NLDA, LDAVAL, TIMMIN,
     $                   A( 1, 1 ), A( 1, 2 ), A( 1, 3 ), RESLTS, LDR1,
     $                   LDR2, NOUT )
            READ( NIN, FMT = '(A)', END = 100 )LINE
*
         ELSE
*
*           Otherwise time DGEMV and SGEMM.
*
            CALL DTIMMV( 'DGEMV ', NN, NVAL, NNB, NBVAL, NLDA, LDAVAL,
     $                   TIMMIN, A( 1, 1 ), LA, A( 1, 2 ), A( 1, 3 ),
     $                   RESLTS, LDR1, LDR2, NOUT )
            CALL DTIMMM( 'DGEMM ', 'N', NN, NVAL, NLDA, LDAVAL, TIMMIN,
     $                   A( 1, 1 ), A( 1, 2 ), A( 1, 3 ), RESLTS, LDR1,
     $                   LDR2, NOUT )
         END IF
      END IF
*
*     Call the appropriate timing routine for each input line.
*
      WRITE( NOUT, FMT = 9988 )
   90 CONTINUE
      C1 = LINE( 1: 1 )
      C2 = LINE( 2: 3 )
      C3 = LINE( 4: 6 )
*
*     Check first character for correct precision.
*
      IF( .NOT.LSAME( C1, 'Double precision' ) ) THEN
         WRITE( NOUT, FMT = 9987 )LINE( 1: 6 )
*
      ELSE IF( LSAMEN( 2, C2, 'B2' ) .OR. LSAMEN( 3, C3, 'MV ' ) .OR.
     $         LSAMEN( 3, C3, 'SV ' ) .OR. LSAMEN( 3, C3, 'R  ' ) .OR.
     $         LSAMEN( 3, C3, 'RC ' ) .OR. LSAMEN( 3, C3, 'RU ' ) .OR.
     $         LSAMEN( 3, C3, 'R2 ' ) ) THEN
*
*        Level 2 BLAS
*
         CALL DTIMB2( LINE, NM, MVAL, NN, NVAL, NK, KVAL, NNB, NBVAL,
     $                NLDA, LDAVAL, LA, TIMMIN, A( 1, 1 ), A( 1, 2 ),
     $                A( 1, 3 ), RESLTS, LDR1, LDR2, NOUT )
*
      ELSE IF( LSAMEN( 2, C2, 'B3' ) .OR. LSAMEN( 3, C3, 'MM ' ) .OR.
     $         LSAMEN( 3, C3, 'SM ' ) .OR. LSAMEN( 3, C3, 'RK ' ) .OR.
     $         LSAMEN( 3, C3, 'R2K' ) ) THEN
*
*        Level 3 BLAS
*
         CALL DTIMB3( LINE, NM, MVAL, NN, NVAL, NK, KVAL, NLDA, LDAVAL,
     $                TIMMIN, A( 1, 1 ), A( 1, 2 ), A( 1, 3 ), RESLTS,
     $                LDR1, LDR2, NOUT )
*
      ELSE IF( LSAMEN( 2, C2, 'QR' ) .OR. LSAMEN( 2, C3, 'QR' ) .OR.
     $         LSAMEN( 2, C3( 2: 3 ), 'QR' ) ) THEN
*
*        QR routines
*
         CALL DTIMQR( LINE, NN, MVAL, NVAL, NK, KVAL, NNB, NBVAL, NXVAL,
     $                NLDA, LDAVAL, TIMMIN, A( 1, 1 ), D, A( 1, 2 ),
     $                A( 1, 3 ), RESLTS, LDR1, LDR2, LDR3, NOUT )
*
      ELSE IF( LSAMEN( 2, C2, 'LQ' ) .OR. LSAMEN( 2, C3, 'LQ' ) .OR.
     $         LSAMEN( 2, C3( 2: 3 ), 'LQ' ) ) THEN
*
*        LQ routines
*
         CALL DTIMLQ( LINE, NN, MVAL, NVAL, NK, KVAL, NNB, NBVAL, NXVAL,
     $                NLDA, LDAVAL, TIMMIN, A( 1, 1 ), D, A( 1, 2 ),
     $                A( 1, 3 ), RESLTS, LDR1, LDR2, LDR3, NOUT )
*
      ELSE IF( LSAMEN( 2, C2, 'QL' ) .OR. LSAMEN( 2, C3, 'QL' ) .OR.
     $         LSAMEN( 2, C3( 2: 3 ), 'QL' ) ) THEN
*
*        QL routines
*
         CALL DTIMQL( LINE, NN, MVAL, NVAL, NK, KVAL, NNB, NBVAL, NXVAL,
     $                NLDA, LDAVAL, TIMMIN, A( 1, 1 ), D, A( 1, 2 ),
     $                A( 1, 3 ), RESLTS, LDR1, LDR2, LDR3, NOUT )
*
      ELSE IF( LSAMEN( 2, C2, 'RQ' ) .OR. LSAMEN( 2, C3, 'RQ' ) .OR.
     $         LSAMEN( 2, C3( 2: 3 ), 'RQ' ) ) THEN
*
*        RQ routines
*
         CALL DTIMRQ( LINE, NN, MVAL, NVAL, NK, KVAL, NNB, NBVAL, NXVAL,
     $                NLDA, LDAVAL, TIMMIN, A( 1, 1 ), D, A( 1, 2 ),
     $                A( 1, 3 ), RESLTS, LDR1, LDR2, LDR3, NOUT )
*
      ELSE IF( LSAMEN( 2, C2, 'QP' ) .OR. LSAMEN( 3, C3, 'QPF' ) ) THEN
*
*        QR with column pivoting
*
         CALL DTIMQP( LINE, NM, MVAL, NVAL, NLDA, LDAVAL, TIMMIN,
     $                A( 1, 1 ), A( 1, 2 ), D( 1, 1 ), A( 1, 3 ), IWORK,
     $                RESLTS, LDR1, LDR2, NOUT )
*
*        Blas-3 QR with column pivoting
*
         CALL DTIMQ3( LINE, NM, MVAL, NVAL, NNB, NBVAL, NXVAL, NLDA,
     $                LDAVAL, TIMMIN, A( 1, 1 ), A( 1, 2 ), D( 1, 1 ),
     $                A( 1, 3 ), IWORK, RESLTS, LDR1, LDR2, NOUT )
*
      ELSE IF( LSAMEN( 2, C2, 'HR' ) .OR. LSAMEN( 3, C3, 'HRD' ) .OR.
     $         LSAMEN( 2, C3( 2: 3 ), 'HR' ) ) THEN
*
*        Reduction to Hessenberg form
*
         CALL DTIMHR( LINE, NN, NVAL, NK, KVAL, NNB, NBVAL, NXVAL, NLDA,
     $                LDAVAL, TIMMIN, A( 1, 1 ), D, A( 1, 2 ),
     $                A( 1, 3 ), RESLTS, LDR1, LDR2, LDR3, NOUT )
*
      ELSE IF( LSAMEN( 2, C2, 'TD' ) .OR. LSAMEN( 3, C3, 'TRD' ) .OR.
     $         LSAMEN( 2, C3( 2: 3 ), 'TR' ) ) THEN
*
*        Reduction to tridiagonal form
*
         CALL DTIMTD( LINE, NN, NVAL, NK, KVAL, NNB, NBVAL, NXVAL, NLDA,
     $                LDAVAL, TIMMIN, A( 1, 1 ), A( 1, 2 ), D( 1, 1 ),
     $                D( 1, 2 ), A( 1, 3 ), RESLTS, LDR1, LDR2, LDR3,
     $                NOUT )
*
      ELSE IF( LSAMEN( 2, C2, 'BR' ) .OR. LSAMEN( 3, C3, 'BRD' ) .OR.
     $         LSAMEN( 2, C3( 2: 3 ), 'BR' ) ) THEN
*
*        Reduction to bidiagonal form
*
         CALL DTIMBR( LINE, NN, MVAL, NVAL, NK, KVAL, NNB, NBVAL, NXVAL,
     $                NLDA, LDAVAL, TIMMIN, A( 1, 1 ), A( 1, 2 ),
     $                D( 1, 1 ), D( 1, 2 ), A( 1, 3 ), RESLTS, LDR1,
     $                LDR2, LDR3, NOUT )
*
      ELSE IF( LSAMEN( 2, C2, 'GE' ) ) THEN
*
*        Routines for general matrices
*
         CALL DTIMGE( LINE, NN, NVAL, NK, KVAL, NNB, NBVAL, NLDA,
     $                LDAVAL, TIMMIN, A( 1, 1 ), A( 1, 2 ), A( 1, 3 ),
     $                IWORK, RESLTS, LDR1, LDR2, LDR3, NOUT )
*
      ELSE IF( LSAMEN( 2, C2, 'GB' ) ) THEN
*
*        General band matrices
*
         IF( LDAMOK ) THEN
            CALL DTIMGB( LINE, NM, MVAL, NN, NVAL, NK, KVAL, NNB, NBVAL,
     $                   NLDA, LDAVAL, TIMMIN, A( 1, 1 ),
     $                   A( LA-MKMAX+1, 3 ), IWORK, RESLTS, LDR1, LDR2,
     $                   LDR3, NOUT )
         ELSE
            WRITE( NOUT, FMT = 9989 )LINE( 1: 6 )
         END IF
*
      ELSE IF( LSAMEN( 2, C2, 'GT' ) ) THEN
*
*        Routines for general tridiagonal matrices
*
         CALL DTIMGT( LINE, NN, NVAL, NK, KVAL, NLDA, LDAVAL, TIMMIN,
     $                A( 1, 1 ), A( 1, 2 ), IWORK, RESLTS, LDR1, LDR2,
     $                LDR3, NOUT )
*
      ELSE IF( LSAMEN( 2, C2, 'PO' ) ) THEN
*
*        Positive definite matrices
*
         CALL DTIMPO( LINE, NN, NVAL, NK, KVAL, NNB, NBVAL, NLDA,
     $                LDAVAL, TIMMIN, A( 1, 1 ), A( 1, 2 ), IWORK,
     $                RESLTS, LDR1, LDR2, LDR3, NOUT )
*
      ELSE IF( LSAMEN( 2, C2, 'PP' ) ) THEN
*
*        Positive definite packed matrices
*
         CALL DTIMPP( LINE, NN, NVAL, NK, KVAL, LA, TIMMIN, A( 1, 1 ),
     $                A( 1, 2 ), IWORK, RESLTS, LDR1, LDR2, LDR3, NOUT )
*
      ELSE IF( LSAMEN( 2, C2, 'PB' ) ) THEN
*
*        Positive definite banded matrices
*
         IF( LDAMOK ) THEN
            IF( MKMAX.GT.LA ) THEN
               J2 = 2
               I2 = 2*LA - MKMAX + 1
            ELSE
               J2 = 3
               I2 = LA - MKMAX + 1
            END IF
            CALL DTIMPB( LINE, NM, MVAL, NN, NVAL, NK, KVAL, NNB, NBVAL,
     $                   NLDA, LDAVAL, TIMMIN, A( 1, 1 ), A( I2, J2 ),
     $                   IWORK, RESLTS, LDR1, LDR2, LDR3, NOUT )
         ELSE
            WRITE( NOUT, FMT = 9989 )LINE( 1: 6 )
         END IF
*
      ELSE IF( LSAMEN( 2, C2, 'PT' ) ) THEN
*
*        Routines for positive definite tridiagonal matrices
*
         CALL DTIMPT( LINE, NN, NVAL, NK, KVAL, NLDA, LDAVAL, TIMMIN,
     $                A( 1, 1 ), A( 1, 2 ), RESLTS, LDR1, LDR2, LDR3,
     $                NOUT )
*
      ELSE IF( LSAMEN( 2, C2, 'SY' ) ) THEN
*
*        Symmetric indefinite matrices
*
         CALL DTIMSY( LINE, NN, NVAL, NK, KVAL, NNB, NBVAL, NLDA,
     $                LDAVAL, TIMMIN, A( 1, 1 ), A( 1, 2 ), A( 1, 3 ),
     $                IWORK, RESLTS, LDR1, LDR2, LDR3, NOUT )
*
      ELSE IF( LSAMEN( 2, C2, 'SP' ) ) THEN
*
*        Symmetric indefinite packed matrices
*
         CALL DTIMSP( LINE, NN, NVAL, NK, KVAL, LA, TIMMIN, A( 1, 1 ),
     $                A( 1, 2 ), A( 1, 3 ), IWORK, RESLTS, LDR1, LDR2,
     $                LDR3, NOUT )
*
      ELSE IF( LSAMEN( 2, C2, 'TR' ) ) THEN
*
*        Triangular matrices
*
         CALL DTIMTR( LINE, NN, NVAL, NK, KVAL, NNB, NBVAL, NLDA,
     $                LDAVAL, TIMMIN, A( 1, 1 ), A( 1, 2 ), RESLTS,
     $                LDR1, LDR2, LDR3, NOUT )
*
      ELSE IF( LSAMEN( 2, C2, 'TP' ) ) THEN
*
*        Triangular packed matrices
*
         CALL DTIMTP( LINE, NN, NVAL, NK, KVAL, LA, TIMMIN, A( 1, 1 ),
     $                A( 1, 2 ), RESLTS, LDR1, LDR2, LDR3, NOUT )
*
      ELSE IF( LSAMEN( 2, C2, 'TB' ) ) THEN
*
*        Triangular band matrices
*
         IF( LDAMOK ) THEN
            IF( MKMAX.GT.LA ) THEN
               J2 = 2
               I2 = 2*LA - MKMAX + 1
            ELSE
               J2 = 3
               I2 = LA - MKMAX + 1
            END IF
            CALL DTIMTB( LINE, NM, MVAL, NN, NVAL, NK, KVAL, NLDA,
     $                   LDAVAL, TIMMIN, A( 1, 1 ), A( I2, J2 ), RESLTS,
     $                   LDR1, LDR2, LDR3, NOUT )
         ELSE
            WRITE( NOUT, FMT = 9989 )LINE( 1: 6 )
         END IF
*
      ELSE IF( LSAMEN( 2, C2, 'LS' ) ) THEN
*
*        Least squares drivers
*
         CALL DTIMLS( LINE, NM, MVAL, NN, NVAL, NK, KVAL, NNB, NBVAL,
     $                NXVAL, NLDA, LDAVAL, TIMMIN, A( 1, 1 ), A( 1, 2 ),
     $                A( 1, 3 ), A( 1, 4 ), S, S( NMAX+1 ), OPCTBL,
     $                TIMTBL, FLPTBL, WORK, IWORK, NOUT )
*
      ELSE
*
         WRITE( NOUT, FMT = 9987 )LINE( 1: 6 )
      END IF
*
*     Read the next line of the input file.
*
      READ( NIN, FMT = '(A)', END = 100 )LINE
      GO TO 90
*
*     Branch to this line when the last record is read.
*
  100 CONTINUE
      S2 = DSECND( )
      WRITE( NOUT, FMT = 9986 )
      WRITE( NOUT, FMT = 9985 )S2 - S1
  110 CONTINUE
*
 9999 FORMAT( ' Too many values of ', A, ' using ', A, ' = ', I2 )
 9998 FORMAT( ' *** LDA = ', I7, ' is too small, must have ',
     $      'LDA > 0.' )
 9997 FORMAT( ' *** ', A1, ' = ', I7, ' is too big:  ',
     $      'maximum allowed is', I7 )
 9996 FORMAT( ' *** N*NB is too big for N =', I6, ', NB =', I6,
     $      / ' --> Increase LA to at least ', I8 )
 9995 FORMAT( ' *** LDA*N is too big for the dense routines ', '(LDA =',
     $      I6, ', N =', I6, ')', / ' --> Increase LA to at least ',
     $      I8 )
 9994 FORMAT( ' *** (LDA+K)*M is too big for the band routines ',
     $      '(LDA=', I6, ', M=', I6, ', K=', I6, ')',
     $      / ' --> Increase LA to at least ', I8 )
 9993 FORMAT( ' The minimum time a subroutine will be timed = ', F6.3,
     $      ' seconds' )
 9992 FORMAT( ' The following parameter values will be used:' )
 9991 FORMAT( 4X, A7, 1X, 10I6, / 12X, 10I6 )
 9990 FORMAT( / ' ------------------------------',
     $      / ' >>>>>    Sample BLAS     <<<<<',
     $      / ' ------------------------------' )
 9989 FORMAT( 1X, A6, ' not timed due to input errors', / )
 9988 FORMAT( / ' ------------------------------',
     $      / ' >>>>>    Timing data     <<<<<',
     $      / ' ------------------------------' )
 9987 FORMAT( 1X, A6, ':  Unrecognized path or subroutine name', / )
 9986 FORMAT( ' End of tests' )
 9985 FORMAT( ' Total time used = ', F12.2, ' seconds' )
 9984 FORMAT( / ' Tests not done due to input errors' )
 9983 FORMAT( ' LAPACK VERSION 3.0, released June 30, 1999 ', / )
*
*     End of DTIMAA
*
      END
      SUBROUTINE DTIMB2( LINE, NM, MVAL, NN, NVAL, NK, KVAL, NINC,
     $                   INCVAL, NLDA, LDAVAL, LA, TIMMIN, A, X, Y,
     $                   RESLTS, LDR1, LDR2, NOUT )
*
*  -- LAPACK timing routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     March 31, 1993
*
*     .. Scalar Arguments ..
      CHARACTER*( * )    LINE
      INTEGER            LA, LDR1, LDR2, NINC, NK, NLDA, NM, NN, NOUT
      DOUBLE PRECISION   TIMMIN
*     ..
*     .. Array Arguments ..
      INTEGER            INCVAL( * ), KVAL( * ), LDAVAL( * ), MVAL( * ),
     $                   NVAL( * )
      DOUBLE PRECISION   A( * ), RESLTS( LDR1, LDR2, * ), X( * ), Y( * )
*     ..
*
*  Purpose
*  =======
*
*  DTIMB2 times the BLAS 2 routines.
*
*  Arguments
*  =========
*
*  LINE    (input) CHARACTER*80
*          The input line that requested this routine.  The first six
*          characters contain either the name of a subroutine or a
*          generic path name.  The remaining characters may be used to
*          specify the individual routines to be timed.  See ATIMIN for
*          a full description of the format of the input line.
*
*  NM      (input) INTEGER
*          The number of values of M contained in the vector MVAL.
*
*  MVAL    (input) INTEGER array, dimension (NM)
*          The values of the matrix row dimension M.
*
*  NN      (input) INTEGER
*          The number of values of N contained in the vector NVAL.
*
*  NVAL    (input) INTEGER array, dimension (NN)
*          The values of the matrix column dimension N.
*
*  NK      (input) INTEGER
*          The number of values of K contained in the vector KVAL.
*
*  KVAL    (input) INTEGER array, dimension (NK)
*          The values of the band width K.
*
*  NINC    (input) INTEGER
*          The number of values of INCX contained in the vector INCVAL.
*
*  INCVAL  (input) INTEGER array, dimension (NINC)
*          The values of INCX, the increment between successive values
*          of the vector X.
*
*  NLDA    (input) INTEGER
*          The number of values of LDA contained in the vector LDAVAL.
*
*  LDAVAL  (input) INTEGER array, dimension (NLDA)
*          The values of the leading dimension of the array A.
*
*  LA      (input) INTEGER
*          The size of the array A.
*
*  TIMMIN  (input) DOUBLE PRECISION
*          The minimum time a subroutine will be timed.
*
*  A       (workspace) DOUBLE PRECISION array, dimension (LA)
*
*  X       (workspace) DOUBLE PRECISION array, dimension (NMAX*INCMAX)
*             where NMAX and INCMAX are the maximum values permitted
*             for N and INCX.
*
*  Y       (workspace) DOUBLE PRECISION array, dimension (NMAX*INCMAX)
*             where NMAX and INCMAX are the maximum values permitted
*             for N and INCX.
*
*  RESLTS  (output) DOUBLE PRECISION array, dimension (LDR1,LDR2,p),
*             where p = NLDA*NINC.
*          The timing results for each subroutine over the relevant
*          values of M, N, K, INCX, and LDA.
*
*  LDR1    (input) INTEGER
*          The first dimension of RESLTS.  LDR1 >= max(1,NM,NK).
*
*  LDR2    (input) INTEGER
*          The second dimension of RESLTS.  LDR2 >= max(1,NN).
*
*  NOUT    (input) INTEGER
*          The unit number for output.
*
*  =====================================================================
*
*     .. Parameters ..
      INTEGER            NSUBS
      PARAMETER          ( NSUBS = 16 )
      INTEGER            NTRANS, NUPLOS
      PARAMETER          ( NTRANS = 2, NUPLOS = 2 )
      DOUBLE PRECISION   ALPHA, BETA
      PARAMETER          ( ALPHA = 1.0D0, BETA = 1.0D0 )
*     ..
*     .. Local Scalars ..
      LOGICAL            IXANDY
      CHARACTER          TRANSA, UPLO
      CHARACTER*3        PATH
      CHARACTER*6        CNAME
      INTEGER            I, I3, IC, ICL, IINC, IK, ILDA, IM, IMAT, IN,
     $                   INCX, INFO, ISUB, ITA, IUPLO, J, K, LDA, M, N,
     $                   NX, NY
      DOUBLE PRECISION   OPS, S1, S2, TIME, UNTIME
*     ..
*     .. Local Arrays ..
      LOGICAL            TIMSUB( NSUBS )
      CHARACTER          TRANS( NTRANS ), UPLOS( NUPLOS )
      CHARACTER*6        NAMES( NSUBS )
      INTEGER            LAVAL( 1 )
*     ..
*     .. External Functions ..
      DOUBLE PRECISION   DMFLOP, DOPBL2, DSECND
      EXTERNAL           DMFLOP, DOPBL2, DSECND
*     ..
*     .. External Subroutines ..
      EXTERNAL           ATIMCK, ATIMIN, DGBMV, DGEMV, DGER, DPRTBL,
     $                   DSBMV, DSPMV, DSPR, DSPR2, DSYMV, DSYR, DSYR2,
     $                   DTBMV, DTBSV, DTIMMG, DTPMV, DTPSV, DTRMV,
     $                   DTRSV
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          DBLE
*     ..
*     .. Data statements ..
      DATA               TRANS / 'N', 'T' /
      DATA               UPLOS / 'U', 'L' /
      DATA               NAMES / 'DGEMV ', 'DGBMV ', 'DSYMV ', 'DSBMV ',
     $                   'DSPMV ', 'DTRMV ', 'DTBMV ', 'DTPMV ',
     $                   'DTRSV ', 'DTBSV ', 'DTPSV ', 'DGER  ',
     $                   'DSYR  ', 'DSPR  ', 'DSYR2 ', 'DSPR2 ' /
*     ..
*     .. Executable Statements ..
*
*     Extract the timing request from the input line.
*
      PATH( 1: 1 ) = 'Double precision'
      PATH( 2: 3 ) = 'B2'
      CALL ATIMIN( PATH, LINE, NSUBS, NAMES, TIMSUB, NOUT, INFO )
      IF( INFO.NE.0 )
     $   GO TO 1070
*
*     Time each routine
*
      DO 1060 ISUB = 1, NSUBS
         IF( .NOT.TIMSUB( ISUB ) )
     $      GO TO 1060
*
*        Check the input values.  The conditions are
*           M <= LDA for general storage
*           K <= LDA for banded storage
*           N*(N+1)/2 <= LA  for packed storage
*
         CNAME = NAMES( ISUB )
         IF( CNAME( 2: 3 ).EQ.'GE' ) THEN
            CALL ATIMCK( 1, CNAME, NM, MVAL, NLDA, LDAVAL, NOUT, INFO )
         ELSE IF( CNAME( 3: 3 ).EQ.'B' ) THEN
            CALL ATIMCK( 0, CNAME, NK, KVAL, NLDA, LDAVAL, NOUT, INFO )
         ELSE IF( CNAME( 3: 3 ).EQ.'P' ) THEN
            LAVAL( 1 ) = LA
            CALL ATIMCK( 4, CNAME, NN, NVAL, 1, LAVAL, NOUT, INFO )
         ELSE
            CALL ATIMCK( 2, CNAME, NN, NVAL, NLDA, LDAVAL, NOUT, INFO )
         END IF
         IF( INFO.GT.0 ) THEN
            WRITE( NOUT, FMT = 9999 )CNAME
            GO TO 1060
         END IF
*
*        Print header.
*
         WRITE( NOUT, FMT = 9998 )CNAME
         IXANDY = ISUB.LE.5 .OR. ISUB.EQ.12 .OR. ISUB.EQ.15 .OR.
     $            ISUB.EQ.16
         IF( CNAME( 3: 3 ).NE.'P' ) THEN
            IF( NLDA*NINC.EQ.1 ) THEN
               IF( IXANDY ) THEN
                  WRITE( NOUT, FMT = 9997 )LDAVAL( 1 ), INCVAL( 1 )
               ELSE
                  WRITE( NOUT, FMT = 9996 )LDAVAL( 1 ), INCVAL( 1 )
               END IF
            ELSE
               DO 20 I = 1, NLDA
                  DO 10 J = 1, NINC
                     IF( IXANDY ) THEN
                        WRITE( NOUT, FMT = 9993 )( I-1 )*NINC + J,
     $                     LDAVAL( I ), INCVAL( J )
                     ELSE
                        WRITE( NOUT, FMT = 9992 )( I-1 )*NINC + J,
     $                     LDAVAL( I ), INCVAL( J )
                     END IF
   10             CONTINUE
   20          CONTINUE
            END IF
         ELSE
            IF( NINC.EQ.1 ) THEN
               IF( IXANDY ) THEN
                  WRITE( NOUT, FMT = 9995 )INCVAL( 1 )
               ELSE
                  WRITE( NOUT, FMT = 9994 )INCVAL( 1 )
               END IF
            ELSE
               DO 30 J = 1, NINC
                  IF( IXANDY ) THEN
                     WRITE( NOUT, FMT = 9991 )J, INCVAL( J )
                  ELSE
                     WRITE( NOUT, FMT = 9990 )J, INCVAL( J )
                  END IF
   30          CONTINUE
            END IF
         END IF
*
*        Time DGEMV
*
         IF( CNAME.EQ.'DGEMV ' ) THEN
            DO 100 ITA = 1, NTRANS
               TRANSA = TRANS( ITA )
               I3 = 0
               DO 90 ILDA = 1, NLDA
                  LDA = LDAVAL( ILDA )
                  DO 80 IINC = 1, NINC
                     INCX = INCVAL( IINC )
                     I3 = I3 + 1
                     DO 70 IM = 1, NM
                        M = MVAL( IM )
                        DO 60 IN = 1, NN
                           N = NVAL( IN )
                           IF( TRANSA.EQ.'N' ) THEN
                              NX = N
                              NY = M
                           ELSE
                              NX = M
                              NY = N
                           END IF
                           CALL DTIMMG( 1, M, N, A, LDA, 0, 0 )
                           CALL DTIMMG( 0, 1, NX, X, INCX, 0, 0 )
                           CALL DTIMMG( 0, 1, NY, Y, INCX, 0, 0 )
                           IC = 0
                           S1 = DSECND( )
   40                      CONTINUE
                           CALL DGEMV( TRANSA, M, N, ALPHA, A, LDA, X,
     $                                 INCX, BETA, Y, INCX )
                           S2 = DSECND( )
                           TIME = S2 - S1
                           IC = IC + 1
                           IF( TIME.LT.TIMMIN ) THEN
                              CALL DTIMMG( 0, 1, NY, Y, INCX, 0, 0 )
                              GO TO 40
                           END IF
*
*                          Subtract the time used in DTIMMG.
*
                           ICL = 1
                           S1 = DSECND( )
   50                      CONTINUE
                           S2 = DSECND( )
                           UNTIME = S2 - S1
                           ICL = ICL + 1
                           IF( ICL.LE.IC ) THEN
                              CALL DTIMMG( 0, 1, NY, Y, INCX, 0, 0 )
                              GO TO 50
                           END IF
*
                           TIME = ( TIME-UNTIME ) / DBLE( IC )
                           OPS = DOPBL2( CNAME, M, N, 0, 0 )
                           RESLTS( IM, IN, I3 ) = DMFLOP( OPS, TIME, 0 )
   60                   CONTINUE
   70                CONTINUE
   80             CONTINUE
   90          CONTINUE
               WRITE( NOUT, FMT = 9989 )TRANSA
               CALL DPRTBL( 'M', 'N', NM, MVAL, NN, NVAL, NINC*NLDA,
     $                      RESLTS, LDR1, LDR2, NOUT )
  100       CONTINUE
*
         ELSE IF( CNAME.EQ.'DGBMV ' ) THEN
*
*           Time DGBMV
*
            DO 170 ITA = 1, NTRANS
               TRANSA = TRANS( ITA )
               I3 = 0
               DO 160 ILDA = 1, NLDA
                  LDA = LDAVAL( ILDA )
                  DO 150 IINC = 1, NINC
                     INCX = INCVAL( IINC )
                     I3 = I3 + 1
                     DO 140 IK = 1, NK
                        K = KVAL( IK )
                        DO 130 IN = 1, NN
                           N = NVAL( IN )
                           M = N
                           CALL DTIMMG( -2, M, N, A, LDA, K, K )
                           CALL DTIMMG( 0, 1, N, X, INCX, 0, 0 )
                           CALL DTIMMG( 0, 1, M, Y, INCX, 0, 0 )
                           IC = 0
                           S1 = DSECND( )
  110                      CONTINUE
                           CALL DGBMV( TRANSA, M, N, K, K, ALPHA, A,
     $                                 LDA, X, INCX, BETA, Y, INCX )
                           S2 = DSECND( )
                           TIME = S2 - S1
                           IC = IC + 1
                           IF( TIME.LT.TIMMIN ) THEN
                              CALL DTIMMG( 0, 1, M, Y, INCX, 0, 0 )
                              GO TO 110
                           END IF
*
*                          Subtract the time used in DTIMMG.
*
                           ICL = 1
                           S1 = DSECND( )
  120                      CONTINUE
                           S2 = DSECND( )
                           UNTIME = S2 - S1
                           ICL = ICL + 1
                           IF( ICL.LE.IC ) THEN
                              CALL DTIMMG( 0, 1, M, Y, INCX, 0, 0 )
                              GO TO 120
                           END IF
*
                           TIME = ( TIME-UNTIME ) / DBLE( IC )
                           OPS = DOPBL2( CNAME, M, N, K, K )
                           RESLTS( IK, IN, I3 ) = DMFLOP( OPS, TIME, 0 )
  130                   CONTINUE
  140                CONTINUE
  150             CONTINUE
  160          CONTINUE
               WRITE( NOUT, FMT = 9988 )TRANSA
               CALL DPRTBL( 'K', 'N', NK, KVAL, NN, NVAL, NINC*NLDA,
     $                      RESLTS, LDR1, LDR2, NOUT )
  170       CONTINUE
*
         ELSE IF( CNAME.EQ.'DSYMV ' ) THEN
*
*           Time DSYMV
*
            DO 230 IUPLO = 1, NUPLOS
               UPLO = UPLOS( IUPLO )
               IMAT = 6
               IF( UPLO.EQ.'L' )
     $            IMAT = -6
               I3 = 0
               DO 220 ILDA = 1, NLDA
                  LDA = LDAVAL( ILDA )
                  DO 210 IINC = 1, NINC
                     INCX = INCVAL( IINC )
                     I3 = I3 + 1
                     DO 200 IN = 1, NN
                        N = NVAL( IN )
                        CALL DTIMMG( IMAT, N, N, A, LDA, 0, 0 )
                        CALL DTIMMG( 0, 1, N, X, INCX, 0, 0 )
                        CALL DTIMMG( 0, 1, N, Y, INCX, 0, 0 )
                        IC = 0
                        S1 = DSECND( )
  180                   CONTINUE
                        CALL DSYMV( UPLO, N, ALPHA, A, LDA, X, INCX,
     $                              BETA, Y, INCX )
                        S2 = DSECND( )
                        TIME = S2 - S1
                        IC = IC + 1
                        IF( TIME.LT.TIMMIN ) THEN
                           CALL DTIMMG( 0, 1, N, Y, INCX, 0, 0 )
                           GO TO 180
                        END IF
*
*                       Subtract the time used in DTIMMG.
*
                        ICL = 1
                        S1 = DSECND( )
  190                   CONTINUE
                        S2 = DSECND( )
                        UNTIME = S2 - S1
                        ICL = ICL + 1
                        IF( ICL.LE.IC ) THEN
                           CALL DTIMMG( 0, 1, N, Y, INCX, 0, 0 )
                           GO TO 190
                        END IF
*
                        TIME = ( TIME-UNTIME ) / DBLE( IC )
                        OPS = DOPBL2( CNAME, N, N, 0, 0 )
                        RESLTS( 1, IN, I3 ) = DMFLOP( OPS, TIME, 0 )
  200                CONTINUE
  210             CONTINUE
  220          CONTINUE
               WRITE( NOUT, FMT = 9986 )CNAME, UPLO
               CALL DPRTBL( ' ', 'N', 1, NVAL, NN, NVAL, NINC*NLDA,
     $                      RESLTS, LDR1, LDR2, NOUT )
  230       CONTINUE
*
         ELSE IF( CNAME.EQ.'DSBMV ' ) THEN
*
*           Time DSBMV
*
            DO 300 IUPLO = 1, NUPLOS
               UPLO = UPLOS( IUPLO )
               IMAT = 8
               IF( UPLO.EQ.'L' )
     $            IMAT = -8
               I3 = 0
               DO 290 ILDA = 1, NLDA
                  LDA = LDAVAL( ILDA )
                  DO 280 IINC = 1, NINC
                     INCX = INCVAL( IINC )
                     I3 = I3 + 1
                     DO 270 IK = 1, NK
                        K = KVAL( IK )
                        DO 260 IN = 1, NN
                           N = NVAL( IN )
                           CALL DTIMMG( IMAT, N, N, A, LDA, K, K )
                           CALL DTIMMG( 0, 1, N, X, INCX, 0, 0 )
                           CALL DTIMMG( 0, 1, N, Y, INCX, 0, 0 )
                           IC = 0
                           S1 = DSECND( )
  240                      CONTINUE
                           CALL DSBMV( UPLO, N, K, ALPHA, A, LDA, X,
     $                                 INCX, BETA, Y, INCX )
                           S2 = DSECND( )
                           TIME = S2 - S1
                           IC = IC + 1
                           IF( TIME.LT.TIMMIN ) THEN
                              CALL DTIMMG( 0, 1, N, Y, INCX, 0, 0 )
                              GO TO 240
                           END IF
*
*                          Subtract the time used in DTIMMG.
*
                           ICL = 1
                           S1 = DSECND( )
  250                      CONTINUE
                           S2 = DSECND( )
                           UNTIME = S2 - S1
                           ICL = ICL + 1
                           IF( ICL.LE.IC ) THEN
                              CALL DTIMMG( 0, 1, N, Y, INCX, 0, 0 )
                              GO TO 250
                           END IF
*
                           TIME = ( TIME-UNTIME ) / DBLE( IC )
                           OPS = DOPBL2( CNAME, N, N, K, K )
                           RESLTS( IK, IN, I3 ) = DMFLOP( OPS, TIME, 0 )
  260                   CONTINUE
  270                CONTINUE
  280             CONTINUE
  290          CONTINUE
               WRITE( NOUT, FMT = 9986 )CNAME, UPLO
               CALL DPRTBL( 'K', 'N', NK, KVAL, NN, NVAL, NINC*NLDA,
     $                      RESLTS, LDR1, LDR2, NOUT )
  300       CONTINUE
*
         ELSE IF( CNAME.EQ.'DSPMV ' ) THEN
*
*           Time DSPMV
*
            DO 350 IUPLO = 1, NUPLOS
               UPLO = UPLOS( IUPLO )
               IMAT = 7
               IF( UPLO.EQ.'L' )
     $            IMAT = -7
               ILDA = 1
               LDA = LDAVAL( ILDA )
               DO 340 IINC = 1, NINC
                  INCX = INCVAL( IINC )
                  DO 330 IN = 1, NN
                     N = NVAL( IN )
                     CALL DTIMMG( IMAT, N, N, A, N*( N+1 ) / 2, 0, 0 )
                     CALL DTIMMG( 0, 1, N, X, INCX, 0, 0 )
                     CALL DTIMMG( 0, 1, N, Y, INCX, 0, 0 )
                     IC = 0
                     S1 = DSECND( )
  310                CONTINUE
                     CALL DSPMV( UPLO, N, ALPHA, A, X, INCX, BETA, Y,
     $                           INCX )
                     S2 = DSECND( )
                     TIME = S2 - S1
                     IC = IC + 1
                     IF( TIME.LT.TIMMIN ) THEN
                        CALL DTIMMG( 0, 1, N, Y, INCX, 0, 0 )
                        GO TO 310
                     END IF
*
*                    Subtract the time used in DTIMMG.
*
                     ICL = 1
                     S1 = DSECND( )
  320                CONTINUE
                     S2 = DSECND( )
                     UNTIME = S2 - S1
                     ICL = ICL + 1
                     IF( ICL.LE.IC ) THEN
                        CALL DTIMMG( 0, 1, N, Y, INCX, 0, 0 )
                        GO TO 320
                     END IF
*
                     TIME = ( TIME-UNTIME ) / DBLE( IC )
                     OPS = DOPBL2( CNAME, N, N, 0, 0 )
                     RESLTS( 1, IN, IINC ) = DMFLOP( OPS, TIME, 0 )
  330             CONTINUE
  340          CONTINUE
               WRITE( NOUT, FMT = 9986 )CNAME, UPLO
               CALL DPRTBL( ' ', 'N', 1, NVAL, NN, NVAL, NINC, RESLTS,
     $                      LDR1, LDR2, NOUT )
  350       CONTINUE
*
         ELSE IF( CNAME.EQ.'DTRMV ' ) THEN
*
*           Time DTRMV
*
            DO 420 IUPLO = 1, NUPLOS
               UPLO = UPLOS( IUPLO )
               IMAT = 9
               IF( UPLO.EQ.'L' )
     $            IMAT = -9
               DO 410 ITA = 1, NTRANS
                  TRANSA = TRANS( ITA )
                  I3 = 0
                  DO 400 ILDA = 1, NLDA
                     LDA = LDAVAL( ILDA )
                     DO 390 IINC = 1, NINC
                        INCX = INCVAL( IINC )
                        I3 = I3 + 1
                        DO 380 IN = 1, NN
                           N = NVAL( IN )
                           CALL DTIMMG( IMAT, N, N, A, LDA, 0, 0 )
                           CALL DTIMMG( 0, 1, N, X, INCX, 0, 0 )
                           IC = 0
                           S1 = DSECND( )
  360                      CONTINUE
                           CALL DTRMV( UPLO, TRANSA, 'Non-unit', N, A,
     $                                 LDA, X, INCX )
                           S2 = DSECND( )
                           TIME = S2 - S1
                           IC = IC + 1
                           IF( TIME.LT.TIMMIN ) THEN
                              CALL DTIMMG( 0, 1, N, X, INCX, 0, 0 )
                              GO TO 360
                           END IF
*
*                          Subtract the time used in DTIMMG.
*
                           ICL = 1
                           S1 = DSECND( )
  370                      CONTINUE
                           S2 = DSECND( )
                           UNTIME = S2 - S1
                           ICL = ICL + 1
                           IF( ICL.LE.IC ) THEN
                              CALL DTIMMG( 0, 1, N, X, INCX, 0, 0 )
                              GO TO 370
                           END IF
*
                           TIME = ( TIME-UNTIME ) / DBLE( IC )
                           OPS = DOPBL2( CNAME, N, N, 0, 0 )
                           RESLTS( 1, IN, I3 ) = DMFLOP( OPS, TIME, 0 )
  380                   CONTINUE
  390                CONTINUE
  400             CONTINUE
                  WRITE( NOUT, FMT = 9987 )CNAME, UPLO, TRANSA
                  CALL DPRTBL( ' ', 'N', 1, NVAL, NN, NVAL, NINC*NLDA,
     $                         RESLTS, LDR1, LDR2, NOUT )
  410          CONTINUE
  420       CONTINUE
*
         ELSE IF( CNAME.EQ.'DTRSV ' ) THEN
*
*           Time DTRSV
*
            DO 490 IUPLO = 1, NUPLOS
               UPLO = UPLOS( IUPLO )
               IMAT = 9
               IF( UPLO.EQ.'L' )
     $            IMAT = -9
               DO 480 ITA = 1, NTRANS
                  TRANSA = TRANS( ITA )
                  I3 = 0
                  DO 470 ILDA = 1, NLDA
                     LDA = LDAVAL( ILDA )
                     DO 460 IINC = 1, NINC
                        INCX = INCVAL( IINC )
                        I3 = I3 + 1
                        DO 450 IN = 1, NN
                           N = NVAL( IN )
                           CALL DTIMMG( IMAT, N, N, A, LDA, 0, 0 )
                           CALL DTIMMG( 0, 1, N, X, INCX, 0, 0 )
                           IC = 0
                           S1 = DSECND( )
  430                      CONTINUE
                           CALL DTRSV( UPLO, TRANSA, 'Non-unit', N, A,
     $                                 LDA, X, INCX )
                           S2 = DSECND( )
                           TIME = S2 - S1
                           IC = IC + 1
                           IF( TIME.LT.TIMMIN ) THEN
                              CALL DTIMMG( 0, 1, N, X, INCX, 0, 0 )
                              GO TO 430
                           END IF
*
*                          Subtract the time used in DTIMMG.
*
                           ICL = 1
                           S1 = DSECND( )
  440                      CONTINUE
                           S2 = DSECND( )
                           UNTIME = S2 - S1
                           ICL = ICL + 1
                           IF( ICL.LE.IC ) THEN
                              CALL DTIMMG( 0, 1, N, X, INCX, 0, 0 )
                              GO TO 440
                           END IF
*
                           TIME = ( TIME-UNTIME ) / DBLE( IC )
                           OPS = DOPBL2( CNAME, N, N, 0, 0 )
                           RESLTS( 1, IN, I3 ) = DMFLOP( OPS, TIME, 0 )
  450                   CONTINUE
  460                CONTINUE
  470             CONTINUE
                  WRITE( NOUT, FMT = 9987 )CNAME, UPLO, TRANSA
                  CALL DPRTBL( ' ', 'N', 1, NVAL, NN, NVAL, NINC*NLDA,
     $                         RESLTS, LDR1, LDR2, NOUT )
  480          CONTINUE
  490       CONTINUE
*
         ELSE IF( CNAME.EQ.'DTBMV ' ) THEN
*
*           Time DTBMV
*
            DO 570 IUPLO = 1, NUPLOS
               UPLO = UPLOS( IUPLO )
               IMAT = 11
               IF( UPLO.EQ.'L' )
     $            IMAT = -11
               DO 560 ITA = 1, NTRANS
                  TRANSA = TRANS( ITA )
                  I3 = 0
                  DO 550 ILDA = 1, NLDA
                     LDA = LDAVAL( ILDA )
                     DO 540 IINC = 1, NINC
                        INCX = INCVAL( IINC )
                        I3 = I3 + 1
                        DO 530 IK = 1, NK
                           K = KVAL( IK )
                           DO 520 IN = 1, NN
                              N = NVAL( IN )
                              CALL DTIMMG( IMAT, N, N, A, LDA, K, K )
                              CALL DTIMMG( 0, 1, N, X, INCX, 0, 0 )
                              IC = 0
                              S1 = DSECND( )
  500                         CONTINUE
                              CALL DTBMV( UPLO, TRANSA, 'Non-unit', N,
     $                                    K, A, LDA, X, INCX )
                              S2 = DSECND( )
                              TIME = S2 - S1
                              IC = IC + 1
                              IF( TIME.LT.TIMMIN ) THEN
                                 CALL DTIMMG( 0, 1, N, X, INCX, 0, 0 )
                                 GO TO 500
                              END IF
*
*                             Subtract the time used in DTIMMG.
*
                              ICL = 1
                              S1 = DSECND( )
  510                         CONTINUE
                              S2 = DSECND( )
                              UNTIME = S2 - S1
                              ICL = ICL + 1
                              IF( ICL.LE.IC ) THEN
                                 CALL DTIMMG( 0, 1, N, X, INCX, 0, 0 )
                                 GO TO 510
                              END IF
*
                              TIME = ( TIME-UNTIME ) / DBLE( IC )
                              OPS = DOPBL2( CNAME, N, N, K, K )
                              RESLTS( IK, IN, I3 ) = DMFLOP( OPS, TIME,
     $                           0 )
  520                      CONTINUE
  530                   CONTINUE
  540                CONTINUE
  550             CONTINUE
                  WRITE( NOUT, FMT = 9987 )CNAME, UPLO, TRANSA
                  CALL DPRTBL( 'K', 'N', NK, KVAL, NN, NVAL, NINC*NLDA,
     $                         RESLTS, LDR1, LDR2, NOUT )
  560          CONTINUE
  570       CONTINUE
*
         ELSE IF( CNAME.EQ.'DTBSV ' ) THEN
*
*           Time DTBSV
*
            DO 650 IUPLO = 1, NUPLOS
               UPLO = UPLOS( IUPLO )
               IMAT = 11
               IF( UPLO.EQ.'L' )
     $            IMAT = -11
               DO 640 ITA = 1, NTRANS
                  TRANSA = TRANS( ITA )
                  I3 = 0
                  DO 630 ILDA = 1, NLDA
                     LDA = LDAVAL( ILDA )
                     DO 620 IINC = 1, NINC
                        INCX = INCVAL( IINC )
                        I3 = I3 + 1
                        DO 610 IK = 1, NK
                           K = KVAL( IK )
                           DO 600 IN = 1, NN
                              N = NVAL( IN )
                              CALL DTIMMG( IMAT, N, N, A, LDA, K, K )
                              CALL DTIMMG( 0, 1, N, X, INCX, 0, 0 )
                              IC = 0
                              S1 = DSECND( )
  580                         CONTINUE
                              CALL DTBSV( UPLO, TRANSA, 'Non-unit', N,
     $                                    K, A, LDA, X, INCX )
                              S2 = DSECND( )
                              TIME = S2 - S1
                              IC = IC + 1
                              IF( TIME.LT.TIMMIN ) THEN
                                 CALL DTIMMG( 0, 1, N, X, INCX, 0, 0 )
                                 GO TO 580
                              END IF
*
*                             Subtract the time used in DTIMMG.
*
                              ICL = 1
                              S1 = DSECND( )
  590                         CONTINUE
                              S2 = DSECND( )
                              UNTIME = S2 - S1
                              ICL = ICL + 1
                              IF( ICL.LE.IC ) THEN
                                 CALL DTIMMG( 0, 1, N, X, INCX, 0, 0 )
                                 GO TO 590
                              END IF
*
                              TIME = ( TIME-UNTIME ) / DBLE( IC )
                              OPS = DOPBL2( CNAME, N, N, K, K )
                              RESLTS( IK, IN, I3 ) = DMFLOP( OPS, TIME,
     $                           0 )
  600                      CONTINUE
  610                   CONTINUE
  620                CONTINUE
  630             CONTINUE
                  WRITE( NOUT, FMT = 9987 )CNAME, UPLO, TRANSA
                  CALL DPRTBL( 'K', 'N', NK, KVAL, NN, NVAL, NINC*NLDA,
     $                         RESLTS, LDR1, LDR2, NOUT )
  640          CONTINUE
  650       CONTINUE
*
         ELSE IF( CNAME.EQ.'DTPMV ' ) THEN
*
*           Time DTPMV
*
            DO 710 IUPLO = 1, NUPLOS
               UPLO = UPLOS( IUPLO )
               IMAT = 10
               IF( UPLO.EQ.'L' )
     $            IMAT = -10
               DO 700 ITA = 1, NTRANS
                  TRANSA = TRANS( ITA )
                  ILDA = 1
                  LDA = LDAVAL( ILDA )
                  DO 690 IINC = 1, NINC
                     INCX = INCVAL( IINC )
                     DO 680 IN = 1, NN
                        N = NVAL( IN )
                        CALL DTIMMG( IMAT, N, N, A, N*( N+1 ) / 2, 0,
     $                               0 )
                        CALL DTIMMG( 0, 1, N, X, INCX, 0, 0 )
                        IC = 0
                        S1 = DSECND( )
  660                   CONTINUE
                        CALL DTPMV( UPLO, TRANSA, 'Non-unit', N, A, X,
     $                              INCX )
                        S2 = DSECND( )
                        TIME = S2 - S1
                        IC = IC + 1
                        IF( TIME.LT.TIMMIN ) THEN
                           CALL DTIMMG( 0, 1, N, X, INCX, 0, 0 )
                           GO TO 660
                        END IF
*
*                       Subtract the time used in DTIMMG.
*
                        ICL = 1
                        S1 = DSECND( )
  670                   CONTINUE
                        S2 = DSECND( )
                        UNTIME = S2 - S1
                        ICL = ICL + 1
                        IF( ICL.LE.IC ) THEN
                           CALL DTIMMG( 0, 1, N, X, INCX, 0, 0 )
                           GO TO 670
                        END IF
*
                        TIME = ( TIME-UNTIME ) / DBLE( IC )
                        OPS = DOPBL2( CNAME, N, N, 0, 0 )
                        RESLTS( 1, IN, IINC ) = DMFLOP( OPS, TIME, 0 )
  680                CONTINUE
  690             CONTINUE
                  WRITE( NOUT, FMT = 9987 )CNAME, UPLO, TRANSA
                  CALL DPRTBL( ' ', 'N', 1, NVAL, NN, NVAL, NINC,
     $                         RESLTS, LDR1, LDR2, NOUT )
  700          CONTINUE
  710       CONTINUE
*
         ELSE IF( CNAME.EQ.'DTPSV ' ) THEN
*
*           Time DTPSV
*
            DO 770 IUPLO = 1, NUPLOS
               UPLO = UPLOS( IUPLO )
               IMAT = 10
               IF( UPLO.EQ.'L' )
     $            IMAT = -10
               DO 760 ITA = 1, NTRANS
                  TRANSA = TRANS( ITA )
                  ILDA = 1
                  LDA = LDAVAL( ILDA )
                  DO 750 IINC = 1, NINC
                     INCX = INCVAL( IINC )
                     DO 740 IN = 1, NN
                        N = NVAL( IN )
                        CALL DTIMMG( IMAT, N, N, A, N*( N+1 ) / 2, 0,
     $                               0 )
                        CALL DTIMMG( 0, 1, N, X, INCX, 0, 0 )
                        IC = 0
                        S1 = DSECND( )
  720                   CONTINUE
                        CALL DTPSV( UPLO, TRANSA, 'Non-unit', N, A, X,
     $                              INCX )
                        S2 = DSECND( )
                        TIME = S2 - S1
                        IC = IC + 1
                        IF( TIME.LT.TIMMIN ) THEN
                           CALL DTIMMG( 0, 1, N, X, INCX, 0, 0 )
                           GO TO 720
                        END IF
*
*                       Subtract the time used in DTIMMG.
*
                        ICL = 1
                        S1 = DSECND( )
  730                   CONTINUE
                        S2 = DSECND( )
                        UNTIME = S2 - S1
                        ICL = ICL + 1
                        IF( ICL.LE.IC ) THEN
                           CALL DTIMMG( 0, 1, N, X, INCX, 0, 0 )
                           GO TO 730
                        END IF
*
                        TIME = ( TIME-UNTIME ) / DBLE( IC )
                        OPS = DOPBL2( CNAME, N, N, 0, 0 )
                        RESLTS( 1, IN, IINC ) = DMFLOP( OPS, TIME, 0 )
  740                CONTINUE
  750             CONTINUE
                  WRITE( NOUT, FMT = 9987 )CNAME, UPLO, TRANSA
                  CALL DPRTBL( ' ', 'N', 1, NVAL, NN, NVAL, NINC,
     $                         RESLTS, LDR1, LDR2, NOUT )
  760          CONTINUE
  770       CONTINUE
*
         ELSE IF( CNAME.EQ.'DGER  ' ) THEN
*
*           Time DGER
*
            I3 = 0
            DO 830 ILDA = 1, NLDA
               LDA = LDAVAL( ILDA )
               DO 820 IINC = 1, NINC
                  INCX = INCVAL( IINC )
                  I3 = I3 + 1
                  DO 810 IM = 1, NM
                     M = MVAL( IM )
                     DO 800 IN = 1, NN
                        N = NVAL( IN )
                        CALL DTIMMG( 0, 1, M, X, INCX, 0, 0 )
                        CALL DTIMMG( 0, 1, N, Y, INCX, 0, 0 )
                        CALL DTIMMG( 1, M, N, A, LDA, 0, 0 )
                        IC = 0
                        S1 = DSECND( )
  780                   CONTINUE
                        CALL DGER( M, N, ALPHA, X, INCX, Y, INCX, A,
     $                             LDA )
                        S2 = DSECND( )
                        TIME = S2 - S1
                        IC = IC + 1
                        IF( TIME.LT.TIMMIN ) THEN
                           CALL DTIMMG( 1, M, N, A, LDA, 0, 0 )
                           GO TO 780
                        END IF
*
*                       Subtract the time used in DTIMMG.
*
                        ICL = 1
                        S1 = DSECND( )
  790                   CONTINUE
                        S2 = DSECND( )
                        UNTIME = S2 - S1
                        ICL = ICL + 1
                        IF( ICL.LE.IC ) THEN
                           CALL DTIMMG( 1, M, N, A, LDA, 0, 0 )
                           GO TO 790
                        END IF
*
                        TIME = ( TIME-UNTIME ) / DBLE( IC )
                        OPS = DOPBL2( CNAME, M, N, 0, 0 )
                        RESLTS( IM, IN, I3 ) = DMFLOP( OPS, TIME, 0 )
  800                CONTINUE
  810             CONTINUE
  820          CONTINUE
  830       CONTINUE
            WRITE( NOUT, FMT = 9985 )
            CALL DPRTBL( 'M', 'N', NM, MVAL, NN, NVAL, NINC*NLDA,
     $                   RESLTS, LDR1, LDR2, NOUT )
*
         ELSE IF( CNAME.EQ.'DSYR  ' ) THEN
*
*           Time DSYR
*
            DO 890 IUPLO = 1, NUPLOS
               UPLO = UPLOS( IUPLO )
               IMAT = 6
               IF( UPLO.EQ.'L' )
     $            IMAT = -6
               I3 = 0
               DO 880 ILDA = 1, NLDA
                  LDA = LDAVAL( ILDA )
                  DO 870 IINC = 1, NINC
                     INCX = INCVAL( IINC )
                     I3 = I3 + 1
                     DO 860 IN = 1, NN
                        N = NVAL( IN )
                        CALL DTIMMG( 0, 1, N, X, INCX, 0, 0 )
                        CALL DTIMMG( 0, 1, N, Y, INCX, 0, 0 )
                        CALL DTIMMG( IMAT, N, N, A, LDA, 0, 0 )
                        IC = 0
                        S1 = DSECND( )
  840                   CONTINUE
                        CALL DSYR( UPLO, N, ALPHA, X, INCX, A, LDA )
                        S2 = DSECND( )
                        TIME = S2 - S1
                        IC = IC + 1
                        IF( TIME.LT.TIMMIN ) THEN
                           CALL DTIMMG( IMAT, N, N, A, LDA, 0, 0 )
                           GO TO 840
                        END IF
*
*                       Subtract the time used in DTIMMG.
*
                        ICL = 1
                        S1 = DSECND( )
  850                   CONTINUE
                        S2 = DSECND( )
                        UNTIME = S2 - S1
                        ICL = ICL + 1
                        IF( ICL.LE.IC ) THEN
                           CALL DTIMMG( IMAT, N, N, A, LDA, 0, 0 )
                           GO TO 850
                        END IF
*
                        TIME = ( TIME-UNTIME ) / DBLE( IC )
                        OPS = DOPBL2( CNAME, N, N, 0, 0 )
                        RESLTS( 1, IN, I3 ) = DMFLOP( OPS, TIME, 0 )
  860                CONTINUE
  870             CONTINUE
  880          CONTINUE
               WRITE( NOUT, FMT = 9986 )CNAME, UPLO
               CALL DPRTBL( ' ', 'N', 1, NVAL, NN, NVAL, NINC*NLDA,
     $                      RESLTS, LDR1, LDR2, NOUT )
  890       CONTINUE
*
         ELSE IF( CNAME.EQ.'DSYR2 ' ) THEN
*
*           Time DSYR2
*
            DO 950 IUPLO = 1, NUPLOS
               UPLO = UPLOS( IUPLO )
               IMAT = 6
               IF( UPLO.EQ.'L' )
     $            IMAT = -6
               I3 = 0
               DO 940 ILDA = 1, NLDA
                  LDA = LDAVAL( ILDA )
                  DO 930 IINC = 1, NINC
                     INCX = INCVAL( IINC )
                     I3 = I3 + 1
                     DO 920 IN = 1, NN
                        N = NVAL( IN )
                        CALL DTIMMG( 0, 1, N, X, INCX, 0, 0 )
                        CALL DTIMMG( 0, 1, N, Y, INCX, 0, 0 )
                        CALL DTIMMG( IMAT, N, N, A, LDA, 0, 0 )
                        IC = 0
                        S1 = DSECND( )
  900                   CONTINUE
                        CALL DSYR2( UPLO, N, ALPHA, X, INCX, Y, INCX, A,
     $                              LDA )
                        S2 = DSECND( )
                        TIME = S2 - S1
                        IC = IC + 1
                        IF( TIME.LT.TIMMIN ) THEN
                           CALL DTIMMG( IMAT, N, N, A, LDA, 0, 0 )
                           GO TO 900
                        END IF
*
*                       Subtract the time used in DTIMMG.
*
                        ICL = 1
                        S1 = DSECND( )
  910                   CONTINUE
                        S2 = DSECND( )
                        UNTIME = S2 - S1
                        ICL = ICL + 1
                        IF( ICL.LE.IC ) THEN
                           CALL DTIMMG( IMAT, N, N, A, LDA, 0, 0 )
                           GO TO 910
                        END IF
*
                        TIME = ( TIME-UNTIME ) / DBLE( IC )
                        OPS = DOPBL2( CNAME, N, N, 0, 0 )
                        RESLTS( 1, IN, I3 ) = DMFLOP( OPS, TIME, 0 )
  920                CONTINUE
  930             CONTINUE
  940          CONTINUE
               WRITE( NOUT, FMT = 9986 )CNAME, UPLO
               CALL DPRTBL( ' ', 'N', 1, NVAL, NN, NVAL, NINC*NLDA,
     $                      RESLTS, LDR1, LDR2, NOUT )
  950       CONTINUE
*
         ELSE IF( CNAME.EQ.'DSPR  ' ) THEN
*
*           Time DSPR
*
            DO 1000 IUPLO = 1, NUPLOS
               UPLO = UPLOS( IUPLO )
               IMAT = 7
               IF( UPLO.EQ.'L' )
     $            IMAT = -7
               ILDA = 1
               LDA = LDAVAL( ILDA )
               DO 990 IINC = 1, NINC
                  INCX = INCVAL( IINC )
                  DO 980 IN = 1, NN
                     N = NVAL( IN )
                     CALL DTIMMG( 0, 1, N, X, INCX, 0, 0 )
                     CALL DTIMMG( 0, 1, N, Y, INCX, 0, 0 )
                     CALL DTIMMG( IMAT, N, N, A, N*( N+1 ) / 2, 0, 0 )
                     IC = 0
                     S1 = DSECND( )
  960                CONTINUE
                     CALL DSPR( UPLO, N, ALPHA, X, INCX, A )
                     S2 = DSECND( )
                     TIME = S2 - S1
                     IC = IC + 1
                     IF( TIME.LT.TIMMIN ) THEN
                        CALL DTIMMG( IMAT, N, N, A, N*( N+1 ) / 2, 0,
     $                               0 )
                        GO TO 960
                     END IF
*
*                    Subtract the time used in DTIMMG.
*
                     ICL = 1
                     S1 = DSECND( )
  970                CONTINUE
                     S2 = DSECND( )
                     UNTIME = S2 - S1
                     ICL = ICL + 1
                     IF( ICL.LE.IC ) THEN
                        CALL DTIMMG( IMAT, N, N, A, N*( N+1 ) / 2, 0,
     $                               0 )
                        GO TO 970
                     END IF
*
                     TIME = ( TIME-UNTIME ) / DBLE( IC )
                     OPS = DOPBL2( CNAME, N, N, 0, 0 )
                     RESLTS( 1, IN, IINC ) = DMFLOP( OPS, TIME, 0 )
  980             CONTINUE
  990          CONTINUE
               WRITE( NOUT, FMT = 9986 )CNAME, UPLO
               CALL DPRTBL( ' ', 'N', 1, NVAL, NN, NVAL, NINC, RESLTS,
     $                      LDR1, LDR2, NOUT )
 1000       CONTINUE
*
         ELSE IF( CNAME.EQ.'DSPR2 ' ) THEN
*
*           Time DSPR2
*
            DO 1050 IUPLO = 1, NUPLOS
               UPLO = UPLOS( IUPLO )
               IMAT = 7
               IF( UPLO.EQ.'L' )
     $            IMAT = -7
               ILDA = 1
               LDA = LDAVAL( ILDA )
               DO 1040 IINC = 1, NINC
                  INCX = INCVAL( IINC )
                  DO 1030 IN = 1, NN
                     N = NVAL( IN )
                     CALL DTIMMG( 0, 1, N, X, INCX, 0, 0 )
                     CALL DTIMMG( 0, 1, N, Y, INCX, 0, 0 )
                     CALL DTIMMG( IMAT, N, N, A, N*( N+1 ) / 2, 0, 0 )
                     IC = 0
                     S1 = DSECND( )
 1010                CONTINUE
                     CALL DSPR2( UPLO, N, ALPHA, X, INCX, Y, INCX, A )
                     S2 = DSECND( )
                     TIME = S2 - S1
                     IC = IC + 1
                     IF( TIME.LT.TIMMIN ) THEN
                        CALL DTIMMG( IMAT, N, N, A, N*( N+1 ) / 2, 0,
     $                               0 )
                        GO TO 1010
                     END IF
*
*                    Subtract the time used in DTIMMG.
*
                     ICL = 1
                     S1 = DSECND( )
 1020                CONTINUE
                     S2 = DSECND( )
                     UNTIME = S2 - S1
                     ICL = ICL + 1
                     IF( ICL.LE.IC ) THEN
                        CALL DTIMMG( IMAT, N, N, A, N*( N+1 ) / 2, 0,
     $                               0 )
                        GO TO 1020
                     END IF
*
                     TIME = ( TIME-UNTIME ) / DBLE( IC )
                     OPS = DOPBL2( CNAME, N, N, 0, 0 )
                     RESLTS( 1, IN, IINC ) = DMFLOP( OPS, TIME, 0 )
 1030             CONTINUE
 1040          CONTINUE
               WRITE( NOUT, FMT = 9986 )CNAME, UPLO
               CALL DPRTBL( ' ', 'N', 1, NVAL, NN, NVAL, NINC, RESLTS,
     $                      LDR1, LDR2, NOUT )
 1050       CONTINUE
         END IF
         WRITE( NOUT, FMT = 9984 )
 1060 CONTINUE
 1070 CONTINUE
*
 9999 FORMAT( 1X, A6, ' timing run not attempted', / )
 9998 FORMAT( / ' *** Speed of ', A6, ' in megaflops ***' )
 9997 FORMAT( 5X, 'with LDA = ', I5, ' and INCX = INCY = ', I5 )
 9996 FORMAT( 5X, 'with LDA = ', I5, ' and INCX = ', I5 )
 9995 FORMAT( 5X, 'with INCX = INCY = ', I5 )
 9994 FORMAT( 5X, 'with INCX = ', I5 )
 9993 FORMAT( 5X, 'line ', I2, ' with LDA = ', I5,
     $      ' and INCX = INCY = ', I5 )
 9992 FORMAT( 5X, 'line ', I2, ' with LDA = ', I5, ' and INCX = ', I5 )
 9991 FORMAT( 5X, 'line ', I2, ' with INCX = INCY = ', I5 )
 9990 FORMAT( 5X, 'line ', I2, ' with INCX = ', I5 )
 9989 FORMAT( / 1X, 'DGEMV  with TRANS = ''', A1, '''', / )
 9988 FORMAT( / 1X, 'DGBMV  with TRANS = ''', A1,
     $      ''', M = N and KL = K', 'U ', '= K', / )
 9987 FORMAT( / 1X, A6, ' with UPLO = ''', A1, ''', TRANS = ''', A1,
     $      '''', / )
 9986 FORMAT( / 1X, A6, ' with UPLO = ''', A1, '''', / )
 9985 FORMAT( / 1X, 'DGER', / )
 9984 FORMAT( / / / / / )
      RETURN
*
*     End of DTIMB2
*
      END
      SUBROUTINE DTIMB3( LINE, NM, MVAL, NN, NVAL, NK, KVAL, NLDA,
     $                   LDAVAL, TIMMIN, A, B, C, RESLTS, LDR1, LDR2,
     $                   NOUT )
*
*  -- LAPACK timing routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     March 31, 1993
*
*     .. Scalar Arguments ..
      CHARACTER*( * )    LINE
      INTEGER            LDR1, LDR2, NK, NLDA, NM, NN, NOUT
      DOUBLE PRECISION   TIMMIN
*     ..
*     .. Array Arguments ..
      INTEGER            KVAL( * ), LDAVAL( * ), MVAL( * ), NVAL( * )
      DOUBLE PRECISION   A( * ), B( * ), C( * ), RESLTS( LDR1, LDR2, * )
*     ..
*
*  Purpose
*  =======
*
*  DTIMB3 times the Level 3 BLAS routines.
*
*  Arguments
*  =========
*
*  LINE    (input) CHARACTER*80
*          The input line that requested this routine.  The first six
*          characters contain either the name of a subroutine or a
*          generic path name.  The remaining characters may be used to
*          specify the individual routines to be timed.  See ATIMIN for
*          a full description of the format of the input line.
*
*  NM      (input) INTEGER
*          The number of values of M contained in the vector MVAL.
*
*  MVAL    (input) INTEGER array, dimension (NM)
*          The values of the matrix row dimension M.
*
*  NN      (input) INTEGER
*          The number of values of N contained in the vector NVAL.
*
*  NVAL    (input) INTEGER array, dimension (NN)
*          The values of the matrix column dimension N.
*
*  NK      (input) INTEGER
*          The number of values of K contained in the vector KVAL.
*
*  KVAL    (input) INTEGER array, dimension (NK)
*          The values of K.  K is used as the intermediate matrix
*          dimension for DGEMM (the product of an M x K matrix and a
*          K x N matrix) and as the dimension of the rank-K update in
*          DSYRK and SSYR2K.
*
*  NLDA    (input) INTEGER
*          The number of values of LDA contained in the vector LDAVAL.
*
*  LDAVAL  (input) INTEGER array, dimension (NLDA)
*          The values of the leading dimension of the array A.
*
*  TIMMIN  (input) DOUBLE PRECISION
*          The minimum time a subroutine will be timed.
*
*  A       (workspace) DOUBLE PRECISION array, dimension (LDAMAX*NMAX)
*             where LDAMAX and NMAX are the maximum values permitted
*             for LDA and N.
*
*  B       (workspace) DOUBLE PRECISION array, dimension (LDAMAX*NMAX)
*
*  C       (workspace) DOUBLE PRECISION array, dimension (LDAMAX*NMAX)
*
*  RESLTS  (output) DOUBLE PRECISION array, dimension (LDR1,LDR2,NLDA)
*          The timing results for each subroutine over the relevant
*          values of M, N, K, and LDA.
*
*  LDR1    (input) INTEGER
*          The first dimension of RESLTS.  LDR1 >= max(1,NM,NK).
*
*  LDR2    (input) INTEGER
*          The second dimension of RESLTS.  LDR2 >= max(1,NN).
*
*  NOUT    (input) INTEGER
*          The unit number for output.
*
*  =====================================================================
*
*     .. Parameters ..
      INTEGER            NSUBS
      PARAMETER          ( NSUBS = 6 )
      INTEGER            NTRANS, NSIDES, NUPLOS
      PARAMETER          ( NTRANS = 2, NSIDES = 2, NUPLOS = 2 )
      DOUBLE PRECISION   ALPHA, BETA
      PARAMETER          ( ALPHA = 1.0D0, BETA = 1.0D0 )
*     ..
*     .. Local Scalars ..
      CHARACTER          SIDE, TRANSA, TRANSB, UPLO
      CHARACTER*3        PATH
      CHARACTER*6        CNAME
      INTEGER            I, IC, ICL, IK, ILDA, IM, IMAT, IN, INFO,
     $                   ISIDE, ISUB, ITA, ITB, IUPLO, K, LDA, M, N
      DOUBLE PRECISION   OPS, S1, S2, TIME, UNTIME
*     ..
*     .. Local Arrays ..
      LOGICAL            TIMSUB( NSUBS )
      CHARACTER          SIDES( NSIDES ), TRANS( NTRANS ),
     $                   UPLOS( NUPLOS )
      CHARACTER*6        NAMES( NSUBS )
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      DOUBLE PRECISION   DMFLOP, DOPBL3, DSECND
      EXTERNAL           LSAME, DMFLOP, DOPBL3, DSECND
*     ..
*     .. External Subroutines ..
      EXTERNAL           ATIMCK, ATIMIN, DGEMM, DPRTBL, DSYMM, DSYR2K,
     $                   DSYRK, DTIMMG, DTRMM, DTRSM
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          DBLE
*     ..
*     .. Data statements ..
      DATA               NAMES / 'DGEMM ', 'DSYMM ', 'DSYRK ', 'DSYR2K',
     $                   'DTRMM ', 'DTRSM ' /
      DATA               TRANS / 'N', 'T' /
      DATA               SIDES / 'L', 'R' /
      DATA               UPLOS / 'U', 'L' /
*     ..
*     .. Executable Statements ..
*
*     Extract the timing request from the input line.
*
      PATH( 1: 1 ) = 'Double precision'
      PATH( 2: 3 ) = 'B3'
      CALL ATIMIN( PATH, LINE, NSUBS, NAMES, TIMSUB, NOUT, INFO )
      IF( INFO.NE.0 )
     $   GO TO 480
*
*     Check that M <= LDA.
*
      CNAME = LINE( 1: 6 )
      CALL ATIMCK( 1, CNAME, NM, MVAL, NLDA, LDAVAL, NOUT, INFO )
      IF( INFO.GT.0 ) THEN
         WRITE( NOUT, FMT = 9999 )CNAME
         GO TO 480
      END IF
*
*     Time each routine.
*
      DO 470 ISUB = 1, NSUBS
         IF( .NOT.TIMSUB( ISUB ) )
     $      GO TO 470
*
*        Print header.
*
         CNAME = NAMES( ISUB )
         WRITE( NOUT, FMT = 9998 )CNAME
         IF( NLDA.EQ.1 ) THEN
            WRITE( NOUT, FMT = 9997 )LDAVAL( 1 )
         ELSE
            DO 10 I = 1, NLDA
               WRITE( NOUT, FMT = 9996 )I, LDAVAL( I )
   10       CONTINUE
         END IF
*
*        Time DGEMM
*
         IF( CNAME.EQ.'DGEMM ' ) THEN
            DO 90 ITA = 1, NTRANS
               TRANSA = TRANS( ITA )
               DO 80 ITB = 1, NTRANS
                  TRANSB = TRANS( ITB )
                  DO 70 IK = 1, NK
                     K = KVAL( IK )
                     DO 60 ILDA = 1, NLDA
                        LDA = LDAVAL( ILDA )
                        DO 50 IM = 1, NM
                           M = MVAL( IM )
                           DO 40 IN = 1, NN
                              N = NVAL( IN )
                              IF( TRANSA.EQ.'N' ) THEN
                                 CALL DTIMMG( 1, M, K, A, LDA, 0, 0 )
                              ELSE
                                 CALL DTIMMG( 1, K, M, A, LDA, 0, 0 )
                              END IF
                              IF( TRANSB.EQ.'N' ) THEN
                                 CALL DTIMMG( 0, K, N, B, LDA, 0, 0 )
                              ELSE
                                 CALL DTIMMG( 0, N, K, B, LDA, 0, 0 )
                              END IF
                              CALL DTIMMG( 1, M, N, C, LDA, 0, 0 )
                              IC = 0
                              S1 = DSECND( )
   20                         CONTINUE
                              CALL DGEMM( TRANSA, TRANSB, M, N, K,
     $                                    ALPHA, A, LDA, B, LDA, BETA,
     $                                    C, LDA )
                              S2 = DSECND( )
                              TIME = S2 - S1
                              IC = IC + 1
                              IF( TIME.LT.TIMMIN ) THEN
                                 CALL DTIMMG( 1, M, N, C, LDA, 0, 0 )
                                 GO TO 20
                              END IF
*
*                             Subtract the time used in DTIMMG.
*
                              ICL = 1
                              S1 = DSECND( )
   30                         CONTINUE
                              S2 = DSECND( )
                              UNTIME = S2 - S1
                              ICL = ICL + 1
                              IF( ICL.LE.IC ) THEN
                                 CALL DTIMMG( 1, M, N, C, LDA, 0, 0 )
                                 GO TO 30
                              END IF
*
                              TIME = ( TIME-UNTIME ) / DBLE( IC )
                              OPS = DOPBL3( CNAME, M, N, K )
                              RESLTS( IM, IN, ILDA ) = DMFLOP( OPS,
     $                           TIME, 0 )
   40                      CONTINUE
   50                   CONTINUE
   60                CONTINUE
                     IF( IK.EQ.1 )
     $                  WRITE( NOUT, FMT = 9995 )TRANSA, TRANSB
                     WRITE( NOUT, FMT = 9994 )KVAL( IK )
                     CALL DPRTBL( 'M', 'N', NM, MVAL, NN, NVAL, NLDA,
     $                            RESLTS, LDR1, LDR2, NOUT )
   70             CONTINUE
   80          CONTINUE
   90       CONTINUE
*
         ELSE IF( CNAME.EQ.'DSYMM ' ) THEN
*
*           Time DSYMM
*
            DO 160 ISIDE = 1, NSIDES
               SIDE = SIDES( ISIDE )
               DO 150 IUPLO = 1, NUPLOS
                  UPLO = UPLOS( IUPLO )
                  IF( LSAME( UPLO, 'U' ) ) THEN
                     IMAT = 6
                  ELSE
                     IMAT = -6
                  END IF
                  DO 140 ILDA = 1, NLDA
                     LDA = LDAVAL( ILDA )
                     DO 130 IM = 1, NM
                        M = MVAL( IM )
                        DO 120 IN = 1, NN
                           N = NVAL( IN )
                           IF( ISIDE.EQ.1 ) THEN
                              CALL DTIMMG( IMAT, M, M, A, LDA, 0, 0 )
                              CALL DTIMMG( 0, M, N, B, LDA, 0, 0 )
                           ELSE
                              CALL DTIMMG( 0, M, N, B, LDA, 0, 0 )
                              CALL DTIMMG( IMAT, N, N, A, LDA, 0, 0 )
                           END IF
                           CALL DTIMMG( 1, M, N, C, LDA, 0, 0 )
                           IC = 0
                           S1 = DSECND( )
  100                      CONTINUE
                           CALL DSYMM( SIDE, UPLO, M, N, ALPHA, A, LDA,
     $                                 B, LDA, BETA, C, LDA )
                           S2 = DSECND( )
                           TIME = S2 - S1
                           IC = IC + 1
                           IF( TIME.LT.TIMMIN ) THEN
                              CALL DTIMMG( 1, M, N, C, LDA, 0, 0 )
                              GO TO 100
                           END IF
*
*                          Subtract the time used in DTIMMG.
*
                           ICL = 1
                           S1 = DSECND( )
  110                      CONTINUE
                           S2 = DSECND( )
                           UNTIME = S2 - S1
                           ICL = ICL + 1
                           IF( ICL.LE.IC ) THEN
                              CALL DTIMMG( 1, M, N, C, LDA, 0, 0 )
                              GO TO 110
                           END IF
*
                           TIME = ( TIME-UNTIME ) / DBLE( IC )
                           OPS = DOPBL3( CNAME, M, N, ISIDE-1 )
                           RESLTS( IM, IN, ILDA ) = DMFLOP( OPS, TIME,
     $                        0 )
  120                   CONTINUE
  130                CONTINUE
  140             CONTINUE
                  WRITE( NOUT, FMT = 9993 )SIDE, UPLO
                  CALL DPRTBL( 'M', 'N', NM, MVAL, NN, NVAL, NLDA,
     $                         RESLTS, LDR1, LDR2, NOUT )
  150          CONTINUE
  160       CONTINUE
*
         ELSE IF( CNAME.EQ.'DSYRK ' ) THEN
*
*           Time DSYRK
*
            DO 230 IUPLO = 1, NUPLOS
               UPLO = UPLOS( IUPLO )
               IF( LSAME( UPLO, 'U' ) ) THEN
                  IMAT = 6
               ELSE
                  IMAT = -6
               END IF
               DO 220 ITA = 1, NTRANS
                  TRANSA = TRANS( ITA )
                  DO 210 ILDA = 1, NLDA
                     LDA = LDAVAL( ILDA )
                     DO 200 IK = 1, NK
                        K = KVAL( IK )
                        IF( TRANSA.EQ.'N' ) THEN
                           CALL DTIMMG( 1, N, K, A, LDA, 0, 0 )
                        ELSE
                           CALL DTIMMG( 1, K, N, A, LDA, 0, 0 )
                        END IF
                        DO 190 IN = 1, NN
                           N = NVAL( IN )
                           CALL DTIMMG( IMAT, N, N, C, LDA, 0, 0 )
                           IC = 0
                           S1 = DSECND( )
  170                      CONTINUE
                           CALL DSYRK( UPLO, TRANSA, N, K, ALPHA, A,
     $                                 LDA, BETA, C, LDA )
                           S2 = DSECND( )
                           TIME = S2 - S1
                           IC = IC + 1
                           IF( TIME.LT.TIMMIN ) THEN
                              CALL DTIMMG( IMAT, N, N, C, LDA, 0, 0 )
                              GO TO 170
                           END IF
*
*                          Subtract the time used in DTIMMG.
*
                           ICL = 1
                           S1 = DSECND( )
  180                      CONTINUE
                           S2 = DSECND( )
                           UNTIME = S2 - S1
                           ICL = ICL + 1
                           IF( ICL.LE.IC ) THEN
                              CALL DTIMMG( IMAT, N, N, C, LDA, 0, 0 )
                              GO TO 180
                           END IF
*
                           TIME = ( TIME-UNTIME ) / DBLE( IC )
                           OPS = DOPBL3( CNAME, N, N, K )
                           RESLTS( IK, IN, ILDA ) = DMFLOP( OPS, TIME,
     $                        0 )
  190                   CONTINUE
  200                CONTINUE
  210             CONTINUE
                  WRITE( NOUT, FMT = 9992 )CNAME, UPLO, TRANSA
                  CALL DPRTBL( 'K', 'N', NK, KVAL, NN, NVAL, NLDA,
     $                         RESLTS, LDR1, LDR2, NOUT )
  220          CONTINUE
  230       CONTINUE
*
         ELSE IF( CNAME.EQ.'DSYR2K' ) THEN
*
*           Time DSYR2K
*
            DO 300 IUPLO = 1, NUPLOS
               UPLO = UPLOS( IUPLO )
               IF( LSAME( UPLO, 'U' ) ) THEN
                  IMAT = 6
               ELSE
                  IMAT = -6
               END IF
               DO 290 ITB = 1, NTRANS
                  TRANSB = TRANS( ITB )
                  DO 280 ILDA = 1, NLDA
                     LDA = LDAVAL( ILDA )
                     DO 270 IK = 1, NK
                        K = KVAL( IK )
                        IF( TRANSB.EQ.'N' ) THEN
                           CALL DTIMMG( 1, N, K, A, LDA, 0, 0 )
                           CALL DTIMMG( 0, N, K, B, LDA, 0, 0 )
                        ELSE
                           CALL DTIMMG( 1, K, N, A, LDA, 0, 0 )
                           CALL DTIMMG( 0, K, N, B, LDA, 0, 0 )
                        END IF
                        DO 260 IN = 1, NN
                           N = NVAL( IN )
                           CALL DTIMMG( IMAT, N, N, C, LDA, 0, 0 )
                           IC = 0
                           S1 = DSECND( )
  240                      CONTINUE
                           CALL DSYR2K( UPLO, TRANSB, N, K, ALPHA, A,
     $                                  LDA, B, LDA, BETA, C, LDA )
                           S2 = DSECND( )
                           TIME = S2 - S1
                           IC = IC + 1
                           IF( TIME.LT.TIMMIN ) THEN
                              CALL DTIMMG( IMAT, N, N, C, LDA, 0, 0 )
                              GO TO 240
                           END IF
*
*                          Subtract the time used in DTIMMG.
*
                           ICL = 1
                           S1 = DSECND( )
  250                      CONTINUE
                           S2 = DSECND( )
                           UNTIME = S2 - S1
                           ICL = ICL + 1
                           IF( ICL.LE.IC ) THEN
                              CALL DTIMMG( IMAT, N, N, C, LDA, 0, 0 )
                              GO TO 250
                           END IF
*
                           TIME = ( TIME-UNTIME ) / DBLE( IC )
                           OPS = DOPBL3( CNAME, N, N, K )
                           RESLTS( IK, IN, ILDA ) = DMFLOP( OPS, TIME,
     $                        0 )
  260                   CONTINUE
  270                CONTINUE
  280             CONTINUE
                  WRITE( NOUT, FMT = 9992 )CNAME, UPLO, TRANSB
                  CALL DPRTBL( 'K', 'N', NK, KVAL, NN, NVAL, NLDA,
     $                         RESLTS, LDR1, LDR2, NOUT )
  290          CONTINUE
  300       CONTINUE
*
         ELSE IF( CNAME.EQ.'DTRMM ' ) THEN
*
*           Time DTRMM
*
            DO 380 ISIDE = 1, NSIDES
               SIDE = SIDES( ISIDE )
               DO 370 IUPLO = 1, NUPLOS
                  UPLO = UPLOS( IUPLO )
                  IF( LSAME( UPLO, 'U' ) ) THEN
                     IMAT = 9
                  ELSE
                     IMAT = -9
                  END IF
                  DO 360 ITA = 1, NTRANS
                     TRANSA = TRANS( ITA )
                     DO 350 ILDA = 1, NLDA
                        LDA = LDAVAL( ILDA )
                        DO 340 IM = 1, NM
                           M = MVAL( IM )
                           DO 330 IN = 1, NN
                              N = NVAL( IN )
                              IF( ISIDE.EQ.1 ) THEN
                                 CALL DTIMMG( IMAT, M, M, A, LDA, 0, 0 )
                              ELSE
                                 CALL DTIMMG( IMAT, N, N, A, LDA, 0, 0 )
                              END IF
                              CALL DTIMMG( 0, M, N, B, LDA, 0, 0 )
                              IC = 0
                              S1 = DSECND( )
  310                         CONTINUE
                              CALL DTRMM( SIDE, UPLO, TRANSA,
     $                                    'Non-unit', M, N, ALPHA, A,
     $                                    LDA, B, LDA )
                              S2 = DSECND( )
                              TIME = S2 - S1
                              IC = IC + 1
                              IF( TIME.LT.TIMMIN ) THEN
                                 CALL DTIMMG( 0, M, N, B, LDA, 0, 0 )
                                 GO TO 310
                              END IF
*
*                             Subtract the time used in DTIMMG.
*
                              ICL = 1
                              S1 = DSECND( )
  320                         CONTINUE
                              S2 = DSECND( )
                              UNTIME = S2 - S1
                              ICL = ICL + 1
                              IF( ICL.LE.IC ) THEN
                                 CALL DTIMMG( 0, M, N, B, LDA, 0, 0 )
                                 GO TO 320
                              END IF
*
                              TIME = ( TIME-UNTIME ) / DBLE( IC )
                              OPS = DOPBL3( CNAME, M, N, ISIDE-1 )
                              RESLTS( IM, IN, ILDA ) = DMFLOP( OPS,
     $                           TIME, 0 )
  330                      CONTINUE
  340                   CONTINUE
  350                CONTINUE
                     WRITE( NOUT, FMT = 9991 )CNAME, SIDE, UPLO, TRANSA
                     CALL DPRTBL( 'M', 'N', NM, MVAL, NN, NVAL, NLDA,
     $                            RESLTS, LDR1, LDR2, NOUT )
  360             CONTINUE
  370          CONTINUE
  380       CONTINUE
*
         ELSE IF( CNAME.EQ.'DTRSM ' ) THEN
*
*           Time DTRSM
*
            DO 460 ISIDE = 1, NSIDES
               SIDE = SIDES( ISIDE )
               DO 450 IUPLO = 1, NUPLOS
                  UPLO = UPLOS( IUPLO )
                  IF( LSAME( UPLO, 'U' ) ) THEN
                     IMAT = 9
                  ELSE
                     IMAT = -9
                  END IF
                  DO 440 ITA = 1, NTRANS
                     TRANSA = TRANS( ITA )
                     DO 430 ILDA = 1, NLDA
                        LDA = LDAVAL( ILDA )
                        DO 420 IM = 1, NM
                           M = MVAL( IM )
                           DO 410 IN = 1, NN
                              N = NVAL( IN )
                              IF( ISIDE.EQ.1 ) THEN
                                 CALL DTIMMG( IMAT, M, M, A, LDA, 0, 0 )
                              ELSE
                                 CALL DTIMMG( IMAT, N, N, A, LDA, 0, 0 )
                              END IF
                              CALL DTIMMG( 0, M, N, B, LDA, 0, 0 )
                              IC = 0
                              S1 = DSECND( )
  390                         CONTINUE
                              CALL DTRSM( SIDE, UPLO, TRANSA,
     $                                    'Non-unit', M, N, ALPHA, A,
     $                                    LDA, B, LDA )
                              S2 = DSECND( )
                              TIME = S2 - S1
                              IC = IC + 1
                              IF( TIME.LT.TIMMIN ) THEN
                                 CALL DTIMMG( 0, M, N, B, LDA, 0, 0 )
                                 GO TO 390
                              END IF
*
*                             Subtract the time used in DTIMMG.
*
                              ICL = 1
                              S1 = DSECND( )
  400                         CONTINUE
                              S2 = DSECND( )
                              UNTIME = S2 - S1
                              ICL = ICL + 1
                              IF( ICL.LE.IC ) THEN
                                 CALL DTIMMG( 0, M, N, B, LDA, 0, 0 )
                                 GO TO 400
                              END IF
*
                              TIME = ( TIME-UNTIME ) / DBLE( IC )
                              OPS = DOPBL3( CNAME, M, N, ISIDE-1 )
                              RESLTS( IM, IN, ILDA ) = DMFLOP( OPS,
     $                           TIME, 0 )
  410                      CONTINUE
  420                   CONTINUE
  430                CONTINUE
                     WRITE( NOUT, FMT = 9991 )CNAME, SIDE, UPLO, TRANSA
                     CALL DPRTBL( 'M', 'N', NM, MVAL, NN, NVAL, NLDA,
     $                            RESLTS, LDR1, LDR2, NOUT )
  440             CONTINUE
  450          CONTINUE
  460       CONTINUE
         END IF
         WRITE( NOUT, FMT = 9990 )
  470 CONTINUE
  480 CONTINUE
*
 9999 FORMAT( 1X, A6, ' timing run not attempted', / )
 9998 FORMAT( / ' *** Speed of ', A6, ' in megaflops ***' )
 9997 FORMAT( 5X, 'with LDA = ', I5 )
 9996 FORMAT( 5X, 'line ', I2, ' with LDA = ', I5 )
 9995 FORMAT( / 1X, 'DGEMM  with TRANSA = ''', A1, ''', TRANSB = ''',
     $      A1, '''' )
 9994 FORMAT( / 1X, 'K = ', I4, / )
 9993 FORMAT( / 1X, 'DSYMM  with SIDE = ''', A1, ''', UPLO = ''', A1,
     $      '''', / )
 9992 FORMAT( / 1X, A6, ' with UPLO = ''', A1, ''', TRANS = ''', A1,
     $      '''', / )
 9991 FORMAT( / 1X, A6, ' with SIDE = ''', A1, ''', UPLO = ''', A1,
     $      ''',', ' TRANS = ''', A1, '''', / )
 9990 FORMAT( / / / / / )
      RETURN
*
*     End of DTIMB3
*
      END
      SUBROUTINE DTIMBR( LINE, NM, MVAL, NVAL, NK, KVAL, NNB, NBVAL,
     $                   NXVAL, NLDA, LDAVAL, TIMMIN, A, B, D, TAU,
     $                   WORK, RESLTS, LDR1, LDR2, LDR3, NOUT )
*
*  -- LAPACK timing routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     March 31, 1993
*
*     .. Scalar Arguments ..
      CHARACTER*80       LINE
      INTEGER            LDR1, LDR2, LDR3, NK, NLDA, NM, NNB, NOUT
      DOUBLE PRECISION   TIMMIN
*     ..
*     .. Array Arguments ..
      INTEGER            KVAL( * ), LDAVAL( * ), MVAL( * ), NBVAL( * ),
     $                   NVAL( * ), NXVAL( * )
      DOUBLE PRECISION   A( * ), B( * ), D( * ),
     $                   RESLTS( LDR1, LDR2, LDR3, * ), TAU( * ),
     $                   WORK( * )
*     ..
*
*  Purpose
*  =======
*
*  DTIMBR times DGEBRD, DORGBR, and DORMBR.
*
*  Arguments
*  =========
*
*  LINE    (input) CHARACTER*80
*          The input line that requested this routine.  The first six
*          characters contain either the name of a subroutine or a
*          generic path name.  The remaining characters may be used to
*          specify the individual routines to be timed.  See ATIMIN for
*          a full description of the format of the input line.
*
*  NM      (input) INTEGER
*          The number of values of M and N contained in the vectors
*          MVAL and NVAL.  The matrix sizes are used in pairs (M,N).
*
*  MVAL    (input) INTEGER array, dimension (NM)
*          The values of the matrix row dimension M.
*
*  NVAL    (input) INTEGER array, dimension (NM)
*          The values of the matrix column dimension N.
*
*  NK      (input) INTEGER
*          The number of values of K contained in the vector KVAL.
*
*  KVAL    (input) INTEGER array, dimension (NK)
*          The values of the matrix dimension K.
*
*  NNB     (input) INTEGER
*          The number of values of NB and NX contained in the
*          vectors NBVAL and NXVAL.  The blocking parameters are used
*          in pairs (NB,NX).
*
*  NBVAL   (input) INTEGER array, dimension (NNB)
*          The values of the blocksize NB.
*
*  NXVAL   (input) INTEGER array, dimension (NNB)
*          The values of the crossover point NX.
*
*  NLDA    (input) INTEGER
*          The number of values of LDA contained in the vector LDAVAL.
*
*  LDAVAL  (input) INTEGER array, dimension (NLDA)
*          The values of the leading dimension of the array A.
*
*  TIMMIN  (input) DOUBLE PRECISION
*          The minimum time a subroutine will be timed.
*
*  A       (workspace) DOUBLE PRECISION array, dimension (LDAMAX*NMAX)
*          where LDAMAX and NMAX are the maximum values of LDA and N.
*
*  B       (workspace) DOUBLE PRECISION array, dimension (LDAMAX*NMAX)
*
*  D       (workspace) DOUBLE PRECISION array, dimension
*                      (2*max(min(M,N))-1)
*
*  TAU     (workspace) DOUBLE PRECISION array, dimension
*                      (2*max(min(M,N)))
*
*  WORK    (workspace) DOUBLE PRECISION array, dimension (LDAMAX*NBMAX)
*          where NBMAX is the maximum value of NB.
*
*  RESLTS  (output) DOUBLE PRECISION array, dimension (LDR1,LDR2,LDR3,6)
*          The timing results for each subroutine over the relevant
*          values of (M,N), (NB,NX), and LDA.
*
*  LDR1    (input) INTEGER
*          The first dimension of RESLTS.  LDR1 >= max(1,NNB).
*
*  LDR2    (input) INTEGER
*          The second dimension of RESLTS.  LDR2 >= max(1,NM).
*
*  LDR3    (input) INTEGER
*          The third dimension of RESLTS.  LDR3 >= max(1,NLDA).
*
*  NOUT    (input) INTEGER
*          The unit number for output.
*
*  Internal Parameters
*  ===================
*
*  MODE    INTEGER
*          The matrix type.  MODE = 3 is a geometric distribution of
*          eigenvalues.  See ZLATMS for further details.
*
*  COND    DOUBLE PRECISION
*          The condition number of the matrix.  The singular values are
*          set to values from DMAX to DMAX/COND.
*
*  DMAX    DOUBLE PRECISION
*          The magnitude of the largest singular value.
*
*  =====================================================================
*
*     .. Parameters ..
      INTEGER            NSUBS
      PARAMETER          ( NSUBS = 3 )
      INTEGER            MODE
      DOUBLE PRECISION   COND, DMAX
      PARAMETER          ( MODE = 3, COND = 100.0D0, DMAX = 1.0D0 )
*     ..
*     .. Local Scalars ..
      CHARACTER          LABK, LABM, LABN, SIDE, TRANS, VECT
      CHARACTER*3        PATH
      CHARACTER*6        CNAME
      INTEGER            I, I3, I4, IC, ICL, IK, ILDA, IM, INB, INFO,
     $                   INFO2, ISIDE, ISUB, ITOFF, ITRAN, IVECT, K, K1,
     $                   LDA, LW, M, M1, MINMN, N, N1, NB, NQ, NX
      DOUBLE PRECISION   OPS, S1, S2, TIME, UNTIME
*     ..
*     .. Local Arrays ..
      LOGICAL            TIMSUB( NSUBS )
      CHARACTER          SIDES( 2 ), TRANSS( 2 ), VECTS( 2 )
      CHARACTER*6        SUBNAM( NSUBS )
      INTEGER            ISEED( 4 ), RESEED( 4 )
*     ..
*     .. External Functions ..
      DOUBLE PRECISION   DMFLOP, DOPLA, DSECND
      EXTERNAL           DMFLOP, DOPLA, DSECND
*     ..
*     .. External Subroutines ..
      EXTERNAL           ATIMCK, ATIMIN, DGEBRD, DLACPY, DLATMS, DORGBR,
     $                   DORMBR, DPRTB4, DPRTB5, DTIMMG, ICOPY, XLAENV
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          DBLE, MAX, MIN
*     ..
*     .. Data statements ..
      DATA               SUBNAM / 'DGEBRD', 'DORGBR', 'DORMBR' / ,
     $                   SIDES / 'L', 'R' / , VECTS / 'Q', 'P' / ,
     $                   TRANSS / 'N', 'T' /
      DATA               ISEED / 0, 0, 0, 1 /
*     ..
*     .. Executable Statements ..
*
*     Extract the timing request from the input line.
*
      PATH( 1: 1 ) = 'Double precision'
      PATH( 2: 3 ) = 'BR'
      CALL ATIMIN( PATH, LINE, NSUBS, SUBNAM, TIMSUB, NOUT, INFO )
      IF( INFO.NE.0 )
     $   GO TO 220
*
*     Check that M <= LDA for the input values.
*
      CNAME = LINE( 1: 6 )
      CALL ATIMCK( 1, CNAME, NM, MVAL, NLDA, LDAVAL, NOUT, INFO )
      IF( INFO.GT.0 ) THEN
         WRITE( NOUT, FMT = 9999 )CNAME
         GO TO 220
      END IF
*
*     Check that N <= LDA and K <= LDA for DORMBR
*
      IF( TIMSUB( 3 ) ) THEN
         CALL ATIMCK( 2, CNAME, NM, NVAL, NLDA, LDAVAL, NOUT, INFO )
         CALL ATIMCK( 3, CNAME, NK, KVAL, NLDA, LDAVAL, NOUT, INFO2 )
         IF( INFO.GT.0 .OR. INFO2.GT.0 ) THEN
            WRITE( NOUT, FMT = 9999 )SUBNAM( 3 )
            TIMSUB( 3 ) = .FALSE.
         END IF
      END IF
*
*     Do for each pair of values (M,N):
*
      DO 140 IM = 1, NM
         M = MVAL( IM )
         N = NVAL( IM )
         MINMN = MIN( M, N )
         CALL ICOPY( 4, ISEED, 1, RESEED, 1 )
*
*        Do for each value of LDA:
*
         DO 130 ILDA = 1, NLDA
            LDA = LDAVAL( ILDA )
*
*           Do for each pair of values (NB, NX) in NBVAL and NXVAL.
*
            DO 120 INB = 1, NNB
               NB = NBVAL( INB )
               CALL XLAENV( 1, NB )
               NX = NXVAL( INB )
               CALL XLAENV( 3, NX )
               LW = MAX( M+N, MAX( 1, NB )*( M+N ) )
*
*              Generate a test matrix of size M by N.
*
               CALL ICOPY( 4, RESEED, 1, ISEED, 1 )
               CALL DLATMS( M, N, 'Uniform', ISEED, 'Nonsym', TAU, MODE,
     $                      COND, DMAX, M, N, 'No packing', B, LDA,
     $                      WORK, INFO )
*
               IF( TIMSUB( 1 ) ) THEN
*
*                 DGEBRD:  Block reduction to bidiagonal form
*
                  CALL DLACPY( 'Full', M, N, B, LDA, A, LDA )
                  IC = 0
                  S1 = DSECND( )
   10             CONTINUE
                  CALL DGEBRD( M, N, A, LDA, D, D( MINMN ), TAU,
     $                         TAU( MINMN+1 ), WORK, LW, INFO )
                  S2 = DSECND( )
                  TIME = S2 - S1
                  IC = IC + 1
                  IF( TIME.LT.TIMMIN ) THEN
                     CALL DLACPY( 'Full', M, N, B, LDA, A, LDA )
                     GO TO 10
                  END IF
*
*                 Subtract the time used in DLACPY.
*
                  ICL = 1
                  S1 = DSECND( )
   20             CONTINUE
                  S2 = DSECND( )
                  UNTIME = S2 - S1
                  ICL = ICL + 1
                  IF( ICL.LE.IC ) THEN
                     CALL DLACPY( 'Full', M, N, A, LDA, B, LDA )
                     GO TO 20
                  END IF
*
                  TIME = ( TIME-UNTIME ) / DBLE( IC )
                  OPS = DOPLA( 'DGEBRD', M, N, 0, 0, NB )
                  RESLTS( INB, IM, ILDA, 1 ) = DMFLOP( OPS, TIME, INFO )
               ELSE
*
*                 If DGEBRD was not timed, generate a matrix and reduce
*                 it using DGEBRD anyway so that the orthogonal
*                 transformations may be used in timing the other
*                 routines.
*
                  CALL DLACPY( 'Full', M, N, B, LDA, A, LDA )
                  CALL DGEBRD( M, N, A, LDA, D, D( MINMN ), TAU,
     $                         TAU( MINMN+1 ), WORK, LW, INFO )
*
               END IF
*
               IF( TIMSUB( 2 ) ) THEN
*
*                 DORGBR:  Generate one of the orthogonal matrices Q or
*                 P' from the reduction to bidiagonal form
*                 A = Q * B * P'.
*
                  DO 50 IVECT = 1, 2
                     IF( IVECT.EQ.1 ) THEN
                        VECT = 'Q'
                        M1 = M
                        N1 = MIN( M, N )
                        K1 = N
                     ELSE
                        VECT = 'P'
                        M1 = MIN( M, N )
                        N1 = N
                        K1 = M
                     END IF
                     I3 = ( IVECT-1 )*NLDA
                     LW = MAX( 1, MAX( 1, NB )*MIN( M, N ) )
                     CALL DLACPY( 'Full', M, N, A, LDA, B, LDA )
                     IC = 0
                     S1 = DSECND( )
   30                CONTINUE
                     CALL DORGBR( VECT, M1, N1, K1, B, LDA, TAU, WORK,
     $                            LW, INFO )
                     S2 = DSECND( )
                     TIME = S2 - S1
                     IC = IC + 1
                     IF( TIME.LT.TIMMIN ) THEN
                        CALL DLACPY( 'Full', M, N, A, LDA, B, LDA )
                        GO TO 30
                     END IF
*
*                    Subtract the time used in DLACPY.
*
                     ICL = 1
                     S1 = DSECND( )
   40                CONTINUE
                     S2 = DSECND( )
                     UNTIME = S2 - S1
                     ICL = ICL + 1
                     IF( ICL.LE.IC ) THEN
                        CALL DLACPY( 'Full', M, N, A, LDA, B, LDA )
                        GO TO 40
                     END IF
*
                     TIME = ( TIME-UNTIME ) / DBLE( IC )
*
*                    Op count for DORGBR:
*
                     IF( IVECT.EQ.1 ) THEN
                        IF( M1.GE.K1 ) THEN
                           OPS = DOPLA( 'DORGQR', M1, N1, K1, -1, NB )
                        ELSE
                           OPS = DOPLA( 'DORGQR', M1-1, M1-1, M1-1, -1,
     $                           NB )
                        END IF
                     ELSE
                        IF( K1.LT.N1 ) THEN
                           OPS = DOPLA( 'DORGLQ', M1, N1, K1, -1, NB )
                        ELSE
                           OPS = DOPLA( 'DORGLQ', N1-1, N1-1, N1-1, -1,
     $                           NB )
                        END IF
                     END IF
*
                     RESLTS( INB, IM, I3+ILDA, 2 ) = DMFLOP( OPS, TIME,
     $                  INFO )
   50             CONTINUE
               END IF
*
               IF( TIMSUB( 3 ) ) THEN
*
*                 DORMBR:  Multiply an m by n matrix B by one of the
*                 orthogonal matrices Q or P' from the reduction to
*                 bidiagonal form A = Q * B * P'.
*
                  DO 110 IVECT = 1, 2
                     IF( IVECT.EQ.1 ) THEN
                        VECT = 'Q'
                        K1 = N
                        NQ = M
                     ELSE
                        VECT = 'P'
                        K1 = M
                        NQ = N
                     END IF
                     I3 = ( IVECT-1 )*NLDA
                     I4 = 2
                     DO 100 ISIDE = 1, 2
                        SIDE = SIDES( ISIDE )
                        DO 90 IK = 1, NK
                           K = KVAL( IK )
                           IF( ISIDE.EQ.1 ) THEN
                              M1 = NQ
                              N1 = K
                              LW = MAX( 1, MAX( 1, NB )*N1 )
                           ELSE
                              M1 = K
                              N1 = NQ
                              LW = MAX( 1, MAX( 1, NB )*M1 )
                           END IF
                           ITOFF = 0
                           DO 80 ITRAN = 1, 2
                              TRANS = TRANSS( ITRAN )
                              CALL DTIMMG( 0, M1, N1, B, LDA, 0, 0 )
                              IC = 0
                              S1 = DSECND( )
   60                         CONTINUE
                              CALL DORMBR( VECT, SIDE, TRANS, M1, N1,
     $                                     K1, A, LDA, TAU, B, LDA,
     $                                     WORK, LW, INFO )
                              S2 = DSECND( )
                              TIME = S2 - S1
                              IC = IC + 1
                              IF( TIME.LT.TIMMIN ) THEN
                                 CALL DTIMMG( 0, M1, N1, B, LDA, 0, 0 )
                                 GO TO 60
                              END IF
*
*                             Subtract the time used in DTIMMG.
*
                              ICL = 1
                              S1 = DSECND( )
   70                         CONTINUE
                              S2 = DSECND( )
                              UNTIME = S2 - S1
                              ICL = ICL + 1
                              IF( ICL.LE.IC ) THEN
                                 CALL DTIMMG( 0, M1, N1, B, LDA, 0, 0 )
                                 GO TO 70
                              END IF
*
                              TIME = ( TIME-UNTIME ) / DBLE( IC )
                              IF( IVECT.EQ.1 ) THEN
*
*                                Op count for DORMBR, VECT = 'Q':
*
                                 IF( NQ.GE.K1 ) THEN
                                    OPS = DOPLA( 'DORMQR', M1, N1, K1,
     $                                    ISIDE-1, NB )
                                 ELSE IF( ISIDE.EQ.1 ) THEN
                                    OPS = DOPLA( 'DORMQR', M1-1, N1,
     $                                    NQ-1, ISIDE-1, NB )
                                 ELSE
                                    OPS = DOPLA( 'DORMQR', M1, N1-1,
     $                                    NQ-1, ISIDE-1, NB )
                                 END IF
                              ELSE
*
*                                Op count for DORMBR, VECT = 'P':
*
                                 IF( NQ.GT.K1 ) THEN
                                    OPS = DOPLA( 'DORMLQ', M1, N1, K1,
     $                                    ISIDE-1, NB )
                                 ELSE IF( ISIDE.EQ.1 ) THEN
                                    OPS = DOPLA( 'DORMLQ', M1-1, N1,
     $                                    NQ-1, ISIDE-1, NB )
                                 ELSE
                                    OPS = DOPLA( 'DORMLQ', M1, N1-1,
     $                                    NQ-1, ISIDE-1, NB )
                                 END IF
                              END IF
*
                              RESLTS( INB, IM, I3+ILDA,
     $                           I4+ITOFF+IK ) = DMFLOP( OPS, TIME,
     $                           INFO )
                              ITOFF = NK
   80                      CONTINUE
   90                   CONTINUE
                        I4 = 2*NK + 2
  100                CONTINUE
  110             CONTINUE
               END IF
  120       CONTINUE
  130    CONTINUE
  140 CONTINUE
*
*     Print a table of results for each timed routine.
*
      DO 210 ISUB = 1, NSUBS
         IF( .NOT.TIMSUB( ISUB ) )
     $      GO TO 210
         WRITE( NOUT, FMT = 9998 )SUBNAM( ISUB )
         IF( NLDA.GT.1 ) THEN
            DO 150 I = 1, NLDA
               WRITE( NOUT, FMT = 9997 )I, LDAVAL( I )
  150       CONTINUE
         END IF
         IF( ISUB.EQ.1 ) THEN
            WRITE( NOUT, FMT = * )
            CALL DPRTB4( '(  NB,  NX)', 'M', 'N', NNB, NBVAL, NXVAL, NM,
     $                   MVAL, NVAL, NLDA, RESLTS( 1, 1, 1, ISUB ),
     $                   LDR1, LDR2, NOUT )
         ELSE IF( ISUB.EQ.2 ) THEN
            DO 160 IVECT = 1, 2
               I3 = ( IVECT-1 )*NLDA + 1
               IF( IVECT.EQ.1 ) THEN
                  LABK = 'N'
                  LABM = 'M'
                  LABN = 'K'
               ELSE
                  LABK = 'M'
                  LABM = 'K'
                  LABN = 'N'
               END IF
               WRITE( NOUT, FMT = 9996 )SUBNAM( ISUB ), VECTS( IVECT ),
     $            LABK, LABM, LABN
               CALL DPRTB4( '(  NB,  NX)', LABM, LABN, NNB, NBVAL,
     $                      NXVAL, NM, MVAL, NVAL, NLDA,
     $                      RESLTS( 1, 1, I3, ISUB ), LDR1, LDR2, NOUT )
  160       CONTINUE
         ELSE IF( ISUB.EQ.3 ) THEN
            DO 200 IVECT = 1, 2
               I3 = ( IVECT-1 )*NLDA + 1
               I4 = 3
               DO 190 ISIDE = 1, 2
                  IF( ISIDE.EQ.1 ) THEN
                     IF( IVECT.EQ.1 ) THEN
                        LABM = 'M'
                        LABN = 'K'
                     ELSE
                        LABM = 'K'
                        LABN = 'M'
                     END IF
                     LABK = 'N'
                  ELSE
                     IF( IVECT.EQ.1 ) THEN
                        LABM = 'N'
                        LABN = 'K'
                     ELSE
                        LABM = 'K'
                        LABN = 'N'
                     END IF
                     LABK = 'M'
                  END IF
                  DO 180 ITRAN = 1, 2
                     DO 170 IK = 1, NK
                        WRITE( NOUT, FMT = 9995 )SUBNAM( ISUB ),
     $                     VECTS( IVECT ), SIDES( ISIDE ),
     $                     TRANSS( ITRAN ), LABK, KVAL( IK )
                        CALL DPRTB5( 'NB', LABM, LABN, NNB, NBVAL, NM,
     $                               MVAL, NVAL, NLDA,
     $                               RESLTS( 1, 1, I3, I4 ), LDR1, LDR2,
     $                               NOUT )
                        I4 = I4 + 1
  170                CONTINUE
  180             CONTINUE
  190          CONTINUE
  200       CONTINUE
         END IF
  210 CONTINUE
  220 CONTINUE
 9999 FORMAT( 1X, A6, ' timing run not attempted', / )
 9998 FORMAT( / ' *** Speed of ', A6, ' in megaflops ***' )
 9997 FORMAT( 5X, 'line ', I2, ' with LDA = ', I5 )
 9996 FORMAT( / 5X, A6, ' with VECT = ''', A1, ''', ', A1, ' = MIN(',
     $      A1, ',', A1, ')', / )
 9995 FORMAT( / 5X, A6, ' with VECT = ''', A1, ''', SIDE = ''', A1,
     $      ''', TRANS = ''', A1, ''', ', A1, ' =', I6, / )
      RETURN
*
*     End of DTIMBR
*
      END
      SUBROUTINE DTIMGB( LINE, NM, MVAL, NK, KVAL, NNS, NSVAL, NNB,
     $                   NBVAL, NLDA, LDAVAL, TIMMIN, A, B, IWORK,
     $                   RESLTS, LDR1, LDR2, LDR3, NOUT )
*
*  -- LAPACK timing routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     March 31, 1993
*
*     .. Scalar Arguments ..
      CHARACTER*80       LINE
      INTEGER            LDR1, LDR2, LDR3, NK, NLDA, NM, NNB, NNS, NOUT
      DOUBLE PRECISION   TIMMIN
*     ..
*     .. Array Arguments ..
      INTEGER            IWORK( * ), KVAL( * ), LDAVAL( * ), MVAL( * ),
     $                   NBVAL( * ), NSVAL( * )
      DOUBLE PRECISION   A( * ), B( * ), RESLTS( LDR1, LDR2, LDR3, * )
*     ..
*
*  Purpose
*  =======
*
*  DTIMGB times DGBTRF and -TRS.
*
*  Arguments
*  =========
*
*  LINE    (input) CHARACTER*80
*          The input line that requested this routine.  The first six
*          characters contain either the name of a subroutine or a
*          generic path name.  The remaining characters may be used to
*          specify the individual routines to be timed.  See ATIMIN for
*          a full description of the format of the input line.
*
*  NM      (input) INTEGER
*          The number of values of M contained in the vector MVAL.
*
*  MVAL    (input) INTEGER array, dimension (NM)
*          The values of the matrix size M.
*
*  NK      (input) INTEGER
*          The number of values of K contained in the vector KVAL.
*
*  KVAL    (input) INTEGER array, dimension (NK)
*          The values of the band width K.
*
*  NNS     (input) INTEGER
*          The number of values of NRHS contained in the vector NSVAL.
*
*  NSVAL   (input) INTEGER array, dimension (NNS)
*          The values of the number of right hand sides NRHS.
*
*  NNB     (input) INTEGER
*          The number of values of NB contained in the vector NBVAL.
*
*  NBVAL   (input) INTEGER array, dimension (NNB)
*          The values of the blocksize NB.
*
*  NLDA    (input) INTEGER
*          The number of values of LDA contained in the vector LDAVAL.
*
*  LDAVAL  (input) INTEGER array, dimension (NLDA)
*          The values of the leading dimension of the array A.
*
*  TIMMIN  (input) DOUBLE PRECISION
*          The minimum time a subroutine will be timed.
*
*  A       (workspace) DOUBLE PRECISION array, dimension (LDAMAX*NMAX)
*          where LDAMAX and NMAX are the maximum values permitted
*          for LDA and N.
*
*  B       (workspace) DOUBLE PRECISION array, dimension (LDAMAX*NMAX)
*
*  IWORK   (workspace) INTEGER array, dimension (NMAX)
*
*  RESLTS  (output) DOUBLE PRECISION array, dimension
*                   (LDR1,LDR2,LDR3,NSUBS)
*          The timing results for each subroutine over the relevant
*          values of N, K, NB, and LDA.
*
*  LDR1    (input) INTEGER
*          The first dimension of RESLTS.  LDR1 >= max(4,NNB).
*
*  LDR2    (input) INTEGER
*          The second dimension of RESLTS.  LDR2 >= max(1,NK).
*
*  LDR3    (input) INTEGER
*          The third dimension of RESLTS.  LDR3 >= max(1,NLDA).
*
*  NOUT    (input) INTEGER
*          The unit number for output.
*
*  =====================================================================
*
*     .. Parameters ..
      INTEGER            NSUBS
      PARAMETER          ( NSUBS = 2 )
*     ..
*     .. Local Scalars ..
      CHARACTER*3        PATH
      CHARACTER*6        CNAME
      INTEGER            I, IC, ICL, IK, ILDA, IM, INB, INFO, ISUB, K,
     $                   KL, KU, LDA, LDB, M, N, NB, NRHS
      DOUBLE PRECISION   OPS, S1, S2, TIME, UNTIME
*     ..
*     .. Local Arrays ..
      LOGICAL            TIMSUB( NSUBS )
      CHARACTER*6        SUBNAM( NSUBS )
*     ..
*     .. External Functions ..
      DOUBLE PRECISION   DMFLOP, DOPLA, DSECND, DOPGB
      EXTERNAL           DMFLOP, DOPLA, DSECND, DOPGB
*     ..
*     .. External Subroutines ..
      EXTERNAL           ATIMCK, ATIMIN, DGBTRF, DGBTRS, DPRTBL, DTIMMG,
     $                   XLAENV
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          DBLE, MAX, MIN
*     ..
*     .. Data statements ..
      DATA               SUBNAM / 'DGBTRF', 'DGBTRS' /
*     ..
*     .. Executable Statements ..
*
*     Extract the timing request from the input line.
*
      PATH( 1: 1 ) = 'Double precision'
      PATH( 2: 3 ) = 'GB'
      CALL ATIMIN( PATH, LINE, NSUBS, SUBNAM, TIMSUB, NOUT, INFO )
      IF( INFO.NE.0 )
     $   GO TO 120
*
*     Check that 3*K+1 <= LDA for the input values.
*
      CNAME = LINE( 1: 6 )
      CALL ATIMCK( 0, CNAME, NK, KVAL, NLDA, LDAVAL, NOUT, INFO )
      IF( INFO.GT.0 ) THEN
         WRITE( NOUT, FMT = 9999 )CNAME
         GO TO 120
      END IF
*
*     Do for each value of the matrix size M:
*
      DO 110 IM = 1, NM
         M = MVAL( IM )
         N = M
*
*        Do for each value of LDA:
*
         DO 80 ILDA = 1, NLDA
            LDA = LDAVAL( ILDA )
*
*           Do for each value of the band width K:
*
            DO 70 IK = 1, NK
               K = KVAL( IK )
               KL = MAX( 0, MIN( K, M-1 ) )
               KU = MAX( 0, MIN( K, N-1 ) )
*
*              Time DGBTRF
*
               IF( TIMSUB( 1 ) ) THEN
*
*                 Do for each value of NB in NBVAL.  Only DGBTRF is
*                 timed in this loop since the other routines are
*                 independent of NB.
*
                  DO 30 INB = 1, NNB
                     NB = NBVAL( INB )
                     CALL XLAENV( 1, NB )
                     IC = 0
                     CALL DTIMMG( 2, M, N, A, LDA, KL, KU )
                     S1 = DSECND( )
   10                CONTINUE
                     CALL DGBTRF( M, N, KL, KU, A, LDA, IWORK, INFO )
                     S2 = DSECND( )
                     TIME = S2 - S1
                     IC = IC + 1
                     IF( TIME.LT.TIMMIN ) THEN
                        CALL DTIMMG( 2, M, N, A, LDA, KL, KU )
                        GO TO 10
                     END IF
*
*                    Subtract the time used in DTIMMG.
*
                     ICL = 1
                     S1 = DSECND( )
   20                CONTINUE
                     S2 = DSECND( )
                     UNTIME = S2 - S1
                     ICL = ICL + 1
                     IF( ICL.LE.IC ) THEN
                        CALL DTIMMG( 2, M, N, A, LDA, KL, KU )
                        GO TO 20
                     END IF
*
                     TIME = ( TIME-UNTIME ) / DBLE( IC )
                     OPS = DOPGB( 'DGBTRF', M, N, KL, KU, IWORK )
                     RESLTS( INB, IK, ILDA, 1 ) = DMFLOP( OPS, TIME,
     $                  INFO )
   30             CONTINUE
               ELSE
                  IC = 0
                  CALL DTIMMG( 2, M, N, A, LDA, KL, KU )
               END IF
*
*              Generate another matrix and factor it using DGBTRF so
*              that the factored form can be used in timing the other
*              routines.
*
               NB = 1
               CALL XLAENV( 1, NB )
               IF( IC.NE.1 )
     $            CALL DGBTRF( M, N, KL, KU, A, LDA, IWORK, INFO )
*
*              Time DGBTRS
*
               IF( TIMSUB( 2 ) ) THEN
                  DO 60 I = 1, NNS
                     NRHS = NSVAL( I )
                     LDB = N
                     IC = 0
                     CALL DTIMMG( 0, N, NRHS, B, LDB, 0, 0 )
                     S1 = DSECND( )
   40                CONTINUE
                     CALL DGBTRS( 'No transpose', N, KL, KU, NRHS, A,
     $                            LDA, IWORK, B, LDB, INFO )
                     S2 = DSECND( )
                     TIME = S2 - S1
                     IC = IC + 1
                     IF( TIME.LT.TIMMIN ) THEN
                        CALL DTIMMG( 0, N, NRHS, B, LDB, 0, 0 )
                        GO TO 40
                     END IF
*
*                    Subtract the time used in DTIMMG.
*
                     ICL = 1
                     S1 = DSECND( )
   50                CONTINUE
                     S2 = DSECND( )
                     UNTIME = S2 - S1
                     ICL = ICL + 1
                     IF( ICL.LE.IC ) THEN
                        CALL DTIMMG( 0, N, NRHS, B, LDB, 0, 0 )
                        GO TO 50
                     END IF
*
                     TIME = ( TIME-UNTIME ) / DBLE( IC )
                     OPS = DOPLA( 'DGBTRS', N, NRHS, KL, KU, 0 )
                     RESLTS( I, IK, ILDA, 2 ) = DMFLOP( OPS, TIME,
     $                  INFO )
   60             CONTINUE
               END IF
   70       CONTINUE
   80    CONTINUE
*
*        Print a table of results for each routine
*
         DO 100 ISUB = 1, NSUBS
            IF( .NOT.TIMSUB( ISUB ) )
     $         GO TO 100
*
*           Print header for routine names.
*
            IF( IM.EQ.1 .OR. CNAME.EQ.'DGB   ' ) THEN
               WRITE( NOUT, FMT = 9998 )SUBNAM( ISUB )
               IF( NLDA.EQ.1 ) THEN
                  WRITE( NOUT, FMT = 9997 )LDAVAL( 1 )
               ELSE
                  DO 90 I = 1, NLDA
                     WRITE( NOUT, FMT = 9996 )I, LDAVAL( I )
   90             CONTINUE
               END IF
            END IF
*
            WRITE( NOUT, FMT = 9995 )SUBNAM( ISUB ), N
            IF( ISUB.EQ.1 ) THEN
               CALL DPRTBL( 'NB', 'K', NNB, NBVAL, NK, KVAL, NLDA,
     $                      RESLTS( 1, 1, 1, 1 ), LDR1, LDR2, NOUT )
            ELSE IF( ISUB.EQ.2 ) THEN
               CALL DPRTBL( 'NRHS', 'K', NNS, NSVAL, NK, KVAL, NLDA,
     $                      RESLTS( 1, 1, 1, 2 ), LDR1, LDR2, NOUT )
            END IF
  100    CONTINUE
  110 CONTINUE
  120 CONTINUE
*
 9999 FORMAT( 1X, A6, ' timing run not attempted', / )
 9998 FORMAT( / ' *** Speed of ', A6, ' in megaflops ***' )
 9997 FORMAT( 5X, 'with LDA = ', I5 )
 9996 FORMAT( 5X, 'line ', I2, ' with LDA = ', I5 )
 9995 FORMAT( / 5X, A6, ' with M =', I6, / )
*
      RETURN
*
*     End of DTIMGB
*
      END
      SUBROUTINE DTIMGE( LINE, NM, MVAL, NNS, NSVAL, NNB, NBVAL, NLDA,
     $                   LDAVAL, TIMMIN, A, B, WORK, IWORK, RESLTS,
     $                   LDR1, LDR2, LDR3, NOUT )
*
*  -- LAPACK timing routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     March 31, 1993
*
*     .. Scalar Arguments ..
      CHARACTER*80       LINE
      INTEGER            LDR1, LDR2, LDR3, NLDA, NM, NNB, NNS, NOUT
      DOUBLE PRECISION   TIMMIN
*     ..
*     .. Array Arguments ..
      INTEGER            IWORK( * ), LDAVAL( * ), MVAL( * ), NBVAL( * ),
     $                   NSVAL( * )
      DOUBLE PRECISION   A( * ), B( * ), RESLTS( LDR1, LDR2, LDR3, * ),
     $                   WORK( * )
*     ..
*
*  Purpose
*  =======
*
*  DTIMGE times DGETRF, -TRS, and -TRI.
*
*  Arguments
*  =========
*
*  LINE    (input) CHARACTER*80
*          The input line that requested this routine.  The first six
*          characters contain either the name of a subroutine or a
*          generic path name.  The remaining characters may be used to
*          specify the individual routines to be timed.  See ATIMIN for
*          a full description of the format of the input line.
*
*  NM      (input) INTEGER
*          The number of values of M contained in the vector MVAL.
*
*  MVAL    (input) INTEGER array, dimension (NM)
*          The values of the matrix size M.
*
*  NNS     (input) INTEGER
*          The number of values of NRHS contained in the vector NSVAL.
*
*  NSVAL   (input) INTEGER array, dimension (NNS)
*          The values of the number of right hand sides NRHS.
*
*  NNB     (input) INTEGER
*          The number of values of NB contained in the vector NBVAL.
*
*  NBVAL   (input) INTEGER array, dimension (NNB)
*          The values of the blocksize NB.
*
*  NLDA    (input) INTEGER
*          The number of values of LDA contained in the vector LDAVAL.
*
*  LDAVAL  (input) INTEGER array, dimension (NLDA)
*          The values of the leading dimension of the array A.
*
*  TIMMIN  (input) DOUBLE PRECISION
*          The minimum time a subroutine will be timed.
*
*  A       (workspace) DOUBLE PRECISION array, dimension (LDAMAX*NMAX)
*          where LDAMAX and NMAX are the maximum values permitted
*          for LDA and N.
*
*  B       (workspace) DOUBLE PRECISION array, dimension (LDAMAX*NMAX)
*
*  WORK    (workspace) DOUBLE PRECISION array, dimension (LDAMAX*NBMAX)
*          where NBMAX is the maximum value of the block size NB.
*
*  IWORK   (workspace) INTEGER array, dimension (NMAX)
*
*  RESLTS  (output) DOUBLE PRECISION array, dimension
*                   (LDR1,LDR2,LDR3,NSUBS)
*          The timing results for each subroutine over the relevant
*          values of N and NB.
*
*  LDR1    (input) INTEGER
*          The first dimension of RESLTS.  LDR1 >= max(4,NNB).
*
*  LDR2    (input) INTEGER
*          The second dimension of RESLTS.  LDR2 >= max(1,NM).
*
*  LDR3    (input) INTEGER
*          The third dimension of RESLTS.  LDR3 >= max(1,NLDA).
*
*  NOUT    (input) INTEGER
*          The unit number for output.
*
*  =====================================================================
*
*     .. Parameters ..
      INTEGER            NSUBS
      PARAMETER          ( NSUBS = 3 )
*     ..
*     .. Local Scalars ..
      CHARACTER*3        PATH
      CHARACTER*6        CNAME
      INTEGER            I, IC, ICL, ILDA, IM, INB, INFO, ISUB, LDA,
     $                   LDB, M, N, NB, NRHS
      DOUBLE PRECISION   OPS, S1, S2, TIME, UNTIME
*     ..
*     .. Local Arrays ..
      LOGICAL            TIMSUB( NSUBS )
      CHARACTER*6        SUBNAM( NSUBS )
*     ..
*     .. External Functions ..
      DOUBLE PRECISION   DMFLOP, DOPLA, DSECND
      EXTERNAL           DMFLOP, DOPLA, DSECND
*     ..
*     .. External Subroutines ..
      EXTERNAL           ATIMCK, ATIMIN, DGETRF, DGETRI, DGETRS, DLACPY,
     $                   DPRTBL, DTIMMG, XLAENV
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          DBLE
*     ..
*     .. Data statements ..
      DATA               SUBNAM / 'DGETRF', 'DGETRS', 'DGETRI' /
*     ..
*     .. Executable Statements ..
*
*     Extract the timing request from the input line.
*
      PATH( 1: 1 ) = 'Double precision'
      PATH( 2: 3 ) = 'GE'
      CALL ATIMIN( PATH, LINE, NSUBS, SUBNAM, TIMSUB, NOUT, INFO )
      IF( INFO.NE.0 )
     $   GO TO 130
*
*     Check that N <= LDA for the input values.
*
      CNAME = LINE( 1: 6 )
      CALL ATIMCK( 2, CNAME, NM, MVAL, NLDA, LDAVAL, NOUT, INFO )
      IF( INFO.GT.0 ) THEN
         WRITE( NOUT, FMT = 9999 )CNAME
         GO TO 130
      END IF
*
*     Do for each value of M:
*
      DO 100 IM = 1, NM
*
         M = MVAL( IM )
         N = M
*
*        Do for each value of LDA:
*
         DO 90 ILDA = 1, NLDA
            LDA = LDAVAL( ILDA )
*
*           Do for each value of NB in NBVAL.  Only the blocked
*           routines are timed in this loop since the other routines
*           are independent of NB.
*
            DO 50 INB = 1, NNB
               NB = NBVAL( INB )
               CALL XLAENV( 1, NB )
*
*              Time DGETRF
*
               IF( TIMSUB( 1 ) ) THEN
                  CALL DTIMMG( 1, M, N, A, LDA, 0, 0 )
                  IC = 0
                  S1 = DSECND( )
   10             CONTINUE
                  CALL DGETRF( M, N, A, LDA, IWORK, INFO )
                  S2 = DSECND( )
                  TIME = S2 - S1
                  IC = IC + 1
                  IF( TIME.LT.TIMMIN ) THEN
                     CALL DTIMMG( 1, M, N, A, LDA, 0, 0 )
                     GO TO 10
                  END IF
*
*                 Subtract the time used in DTIMMG.
*
                  ICL = 1
                  S1 = DSECND( )
   20             CONTINUE
                  S2 = DSECND( )
                  UNTIME = S2 - S1
                  ICL = ICL + 1
                  IF( ICL.LE.IC ) THEN
                     CALL DTIMMG( 1, M, N, A, LDA, 0, 0 )
                     GO TO 20
                  END IF
*
                  TIME = ( TIME-UNTIME ) / DBLE( IC )
                  OPS = DOPLA( 'DGETRF', M, N, 0, 0, NB )
                  RESLTS( INB, IM, ILDA, 1 ) = DMFLOP( OPS, TIME, INFO )
*
               ELSE
                  IC = 0
                  CALL DTIMMG( 1, M, N, A, LDA, 0, 0 )
               END IF
*
*              Generate another matrix and factor it using DGETRF so
*              that the factored form can be used in timing the other
*              routines.
*
               IF( IC.NE.1 )
     $            CALL DGETRF( M, N, A, LDA, IWORK, INFO )
*
*              Time DGETRI
*
               IF( TIMSUB( 3 ) ) THEN
                  CALL DLACPY( 'Full', M, M, A, LDA, B, LDA )
                  IC = 0
                  S1 = DSECND( )
   30             CONTINUE
                  CALL DGETRI( M, B, LDA, IWORK, WORK, LDA*NB, INFO )
                  S2 = DSECND( )
                  TIME = S2 - S1
                  IC = IC + 1
                  IF( TIME.LT.TIMMIN ) THEN
                     CALL DLACPY( 'Full', M, M, A, LDA, B, LDA )
                     GO TO 30
                  END IF
*
*                 Subtract the time used in DLACPY.
*
                  ICL = 1
                  S1 = DSECND( )
   40             CONTINUE
                  S2 = DSECND( )
                  UNTIME = S2 - S1
                  ICL = ICL + 1
                  IF( ICL.LE.IC ) THEN
                     CALL DLACPY( 'Full', M, M, A, LDA, B, LDA )
                     GO TO 40
                  END IF
*
                  TIME = ( TIME-UNTIME ) / DBLE( IC )
                  OPS = DOPLA( 'DGETRI', M, M, 0, 0, NB )
                  RESLTS( INB, IM, ILDA, 3 ) = DMFLOP( OPS, TIME, INFO )
               END IF
   50       CONTINUE
*
*           Time DGETRS
*
            IF( TIMSUB( 2 ) ) THEN
               DO 80 I = 1, NNS
                  NRHS = NSVAL( I )
                  LDB = LDA
                  CALL DTIMMG( 0, M, NRHS, B, LDB, 0, 0 )
                  IC = 0
                  S1 = DSECND( )
   60             CONTINUE
                  CALL DGETRS( 'No transpose', M, NRHS, A, LDA, IWORK,
     $                         B, LDB, INFO )
                  S2 = DSECND( )
                  TIME = S2 - S1
                  IC = IC + 1
                  IF( TIME.LT.TIMMIN ) THEN
                     CALL DTIMMG( 0, M, NRHS, B, LDB, 0, 0 )
                     GO TO 60
                  END IF
*
*                 Subtract the time used in DTIMMG.
*
                  ICL = 1
                  S1 = DSECND( )
   70             CONTINUE
                  S2 = DSECND( )
                  UNTIME = S2 - S1
                  ICL = ICL + 1
                  IF( ICL.LE.IC ) THEN
                     CALL DTIMMG( 0, M, NRHS, B, LDB, 0, 0 )
                     GO TO 70
                  END IF
*
                  TIME = ( TIME-UNTIME ) / DBLE( IC )
                  OPS = DOPLA( 'DGETRS', M, NRHS, 0, 0, 0 )
                  RESLTS( I, IM, ILDA, 2 ) = DMFLOP( OPS, TIME, INFO )
   80          CONTINUE
            END IF
   90    CONTINUE
  100 CONTINUE
*
*     Print a table of results for each timed routine.
*
      DO 120 ISUB = 1, NSUBS
         IF( .NOT.TIMSUB( ISUB ) )
     $      GO TO 120
         WRITE( NOUT, FMT = 9998 )SUBNAM( ISUB )
         IF( NLDA.GT.1 ) THEN
            DO 110 I = 1, NLDA
               WRITE( NOUT, FMT = 9997 )I, LDAVAL( I )
  110       CONTINUE
         END IF
         WRITE( NOUT, FMT = * )
         IF( ISUB.EQ.1 ) THEN
            CALL DPRTBL( 'NB', 'N', NNB, NBVAL, NM, MVAL, NLDA, RESLTS,
     $                   LDR1, LDR2, NOUT )
         ELSE IF( ISUB.EQ.2 ) THEN
            CALL DPRTBL( 'NRHS', 'N', NNS, NSVAL, NM, MVAL, NLDA,
     $                   RESLTS( 1, 1, 1, 2 ), LDR1, LDR2, NOUT )
         ELSE IF( ISUB.EQ.3 ) THEN
            CALL DPRTBL( 'NB', 'N', NNB, NBVAL, NM, MVAL, NLDA,
     $                   RESLTS( 1, 1, 1, 3 ), LDR1, LDR2, NOUT )
         END IF
  120 CONTINUE
*
  130 CONTINUE
 9999 FORMAT( 1X, A6, ' timing run not attempted', / )
 9998 FORMAT( / ' *** Speed of ', A6, ' in megaflops ***' )
 9997 FORMAT( 5X, 'line ', I2, ' with LDA = ', I5 )
      RETURN
*
*     End of DTIMGE
*
      END
      SUBROUTINE DTIMGT( LINE, NM, MVAL, NNS, NSVAL, NLDA, LDAVAL,
     $                   TIMMIN, A, B, IWORK, RESLTS, LDR1, LDR2, LDR3,
     $                   NOUT )
*
*  -- LAPACK timing routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     June 30, 1999
*
*     .. Scalar Arguments ..
      CHARACTER*80       LINE
      INTEGER            LDR1, LDR2, LDR3, NLDA, NM, NNS, NOUT
      DOUBLE PRECISION   TIMMIN
*     ..
*     .. Array Arguments ..
      INTEGER            IWORK( * ), LDAVAL( * ), MVAL( * ), NSVAL( * )
      DOUBLE PRECISION   A( * ), B( * ), RESLTS( LDR1, LDR2, LDR3, * )
*     ..
*
*  Purpose
*  =======
*
*  DTIMGT times DGTTRF, -TRS, -SV, and -SL.
*
*  Arguments
*  =========
*
*  LINE    (input) CHARACTER*80
*          The input line that requested this routine.  The first six
*          characters contain either the name of a subroutine or a
*          generic path name.  The remaining characters may be used to
*          specify the individual routines to be timed.  See ATIMIN for
*          a full description of the format of the input line.
*
*  NM      (input) INTEGER
*          The number of values of M contained in the vector MVAL.
*
*  MVAL    (input) INTEGER array, dimension (NM)
*          The values of the matrix size M.
*
*  NNS     (input) INTEGER
*          The number of values of NRHS contained in the vector NSVAL.
*
*  NSVAL   (input) INTEGER array, dimension (NNS)
*          The values of the number of right hand sides NRHS.
*
*  NLDA    (input) INTEGER
*          The number of values of LDA contained in the vector LDAVAL.
*
*  LDAVAL  (input) INTEGER array, dimension (NLDA)
*          The values of the leading dimension of the array A.
*
*  TIMMIN  (input) DOUBLE PRECISION
*          The minimum time a subroutine will be timed.
*
*  A       (workspace) DOUBLE PRECISION array, dimension (NMAX*4)
*          where NMAX is the maximum value permitted for N.
*
*  B       (workspace) DOUBLE PRECISION array, dimension (LDAMAX*NMAX)
*
*  IWORK   (workspace) INTEGER array, dimension (NMAX)
*
*  RESLTS  (output) DOUBLE PRECISION array, dimension
*                   (LDR1,LDR2,LDR3,NSUBS+1)
*          The timing results for each subroutine over the relevant
*          values of N.
*
*  LDR1    (input) INTEGER
*          The first dimension of RESLTS.  LDR1 >= 1.
*
*  LDR2    (input) INTEGER
*          The second dimension of RESLTS.  LDR2 >= max(1,NM).
*
*  LDR3    (input) INTEGER
*          The third dimension of RESLTS.  LDR3 >= max(1,NLDA).
*
*  NOUT    (input) INTEGER
*          The unit number for output.
*
*  =====================================================================
*
*     .. Parameters ..
      INTEGER            NSUBS
      PARAMETER          ( NSUBS = 4 )
*     ..
*     .. Local Scalars ..
      CHARACTER          TRANS
      CHARACTER*3        PATH
      CHARACTER*6        CNAME
      INTEGER            I, IC, ICL, ILDA, IM, INFO, ISUB, ITRAN, LDB,
     $                   M, N, NRHS
      DOUBLE PRECISION   OPS, S1, S2, TIME, UNTIME
*     ..
*     .. Local Arrays ..
      LOGICAL            TIMSUB( NSUBS )
      CHARACTER          TRANSS( 2 )
      CHARACTER*6        SUBNAM( NSUBS )
      INTEGER            LAVAL( 1 )
*     ..
*     .. External Functions ..
      DOUBLE PRECISION   DMFLOP, DSECND, DOPGB
      EXTERNAL           DMFLOP, DSECND, DOPGB
*     ..
*     .. External Subroutines ..
      EXTERNAL           ATIMCK, ATIMIN, DGTSL, DGTSV, DGTTRF, DGTTRS,
     $                   DPRTBL, DTIMMG
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          DBLE, MAX
*     ..
*     .. Data statements ..
      DATA               SUBNAM / 'DGTTRF', 'DGTTRS', 'DGTSV ',
     $                   'DGTSL ' /
      DATA               TRANSS / 'N', 'T' /
*     ..
*     .. Executable Statements ..
*
*     Extract the timing request from the input line.
*
      PATH( 1: 1 ) = 'Double precision'
      PATH( 2: 3 ) = 'GT'
      CALL ATIMIN( PATH, LINE, NSUBS, SUBNAM, TIMSUB, NOUT, INFO )
      IF( INFO.NE.0 )
     $   GO TO 180
*
*     Check that N <= LDA for the input values.
*
      DO 10 ISUB = 2, NSUBS
         IF( .NOT.TIMSUB( ISUB ) )
     $      GO TO 10
         CNAME = SUBNAM( ISUB )
         CALL ATIMCK( 2, CNAME, NM, MVAL, NLDA, LDAVAL, NOUT, INFO )
         IF( INFO.GT.0 ) THEN
            WRITE( NOUT, FMT = 9998 )CNAME
            TIMSUB( ISUB ) = .FALSE.
         END IF
   10 CONTINUE
*
*     Do for each value of M:
*
      DO 150 IM = 1, NM
*
         M = MVAL( IM )
         N = MAX( M, 1 )
*
*        Time DGTTRF
*
         IF( TIMSUB( 1 ) ) THEN
            CALL DTIMMG( 12, M, M, A, 3*N, 0, 0 )
            IC = 0
            S1 = DSECND( )
   20       CONTINUE
            CALL DGTTRF( M, A, A( N ), A( 2*N ), A( 3*N-2 ), IWORK,
     $                   INFO )
            S2 = DSECND( )
            TIME = S2 - S1
            IC = IC + 1
            IF( TIME.LT.TIMMIN ) THEN
               CALL DTIMMG( 12, M, M, A, 3*N, 0, 0 )
               GO TO 20
            END IF
*
*           Subtract the time used in DTIMMG.
*
            ICL = 1
            S1 = DSECND( )
   30       CONTINUE
            S2 = DSECND( )
            UNTIME = S2 - S1
            ICL = ICL + 1
            IF( ICL.LE.IC ) THEN
               CALL DTIMMG( 12, M, M, A, 3*N, 0, 0 )
               GO TO 30
            END IF
*
            TIME = ( TIME-UNTIME ) / DBLE( IC )
            OPS = DOPGB( 'DGTTRF', M, M, 1, 1, IWORK )
            RESLTS( 1, IM, 1, 1 ) = DMFLOP( OPS, TIME, INFO )
*
         ELSE IF( TIMSUB( 2 ) ) THEN
            CALL DTIMMG( 12, M, M, A, 3*N, 0, 0 )
         END IF
*
*        Generate another matrix and factor it using DGTTRF so
*        that the factored form can be used in timing the other
*        routines.
*
         IF( IC.NE.1 )
     $      CALL DGTTRF( M, A, A( N ), A( 2*N ), A( 3*N-2 ), IWORK,
     $                   INFO )
*
*        Time DGTTRS
*
         IF( TIMSUB( 2 ) ) THEN
            DO 80 ITRAN = 1, 2
               TRANS = TRANSS( ITRAN )
               DO 70 ILDA = 1, NLDA
                  LDB = LDAVAL( ILDA )
                  DO 60 I = 1, NNS
                     NRHS = NSVAL( I )
                     CALL DTIMMG( 0, M, NRHS, B, LDB, 0, 0 )
                     IC = 0
                     S1 = DSECND( )
   40                CONTINUE
                     CALL DGTTRS( TRANS, M, NRHS, A, A( N ), A( 2*N ),
     $                            A( 3*N-2 ), IWORK, B, LDB, INFO )
                     S2 = DSECND( )
                     TIME = S2 - S1
                     IC = IC + 1
                     IF( TIME.LT.TIMMIN ) THEN
                        CALL DTIMMG( 0, M, NRHS, B, LDB, 0, 0 )
                        GO TO 40
                     END IF
*
*                 Subtract the time used in DTIMMG.
*
                     ICL = 1
                     S1 = DSECND( )
   50                CONTINUE
                     S2 = DSECND( )
                     UNTIME = S2 - S1
                     ICL = ICL + 1
                     IF( ICL.LE.IC ) THEN
                        CALL DTIMMG( 0, M, NRHS, B, LDB, 0, 0 )
                        GO TO 50
                     END IF
*
                     TIME = ( TIME-UNTIME ) / DBLE( IC )
                     OPS = DOPGB( 'DGTTRS', M, NRHS, 0, 0, IWORK )
                     IF( ITRAN.EQ.1 ) THEN
                        RESLTS( I, IM, ILDA, 2 ) = DMFLOP( OPS, TIME,
     $                     INFO )
                     ELSE
                        RESLTS( I, IM, ILDA, 5 ) = DMFLOP( OPS, TIME,
     $                     INFO )
                     END IF
   60             CONTINUE
   70          CONTINUE
   80       CONTINUE
         END IF
*
         IF( TIMSUB( 3 ) ) THEN
            DO 120 ILDA = 1, NLDA
               LDB = LDAVAL( ILDA )
               DO 110 I = 1, NNS
                  NRHS = NSVAL( I )
                  CALL DTIMMG( 12, M, M, A, 3*N, 0, 0 )
                  CALL DTIMMG( 0, M, NRHS, B, LDB, 0, 0 )
                  IC = 0
                  S1 = DSECND( )
   90             CONTINUE
                  CALL DGTSV( M, NRHS, A, A( N ), A( 2*N ), B, LDB,
     $                        INFO )
                  S2 = DSECND( )
                  TIME = S2 - S1
                  IC = IC + 1
                  IF( TIME.LT.TIMMIN ) THEN
                     CALL DTIMMG( 12, M, M, A, 3*N, 0, 0 )
                     CALL DTIMMG( 0, M, NRHS, B, LDB, 0, 0 )
                     GO TO 90
                  END IF
*
*                 Subtract the time used in DTIMMG.
*
                  ICL = 1
                  S1 = DSECND( )
  100             CONTINUE
                  S2 = DSECND( )
                  UNTIME = S2 - S1
                  ICL = ICL + 1
                  IF( ICL.LE.IC ) THEN
                     CALL DTIMMG( 12, M, M, A, 3*N, 0, 0 )
                     CALL DTIMMG( 0, M, NRHS, B, LDB, 0, 0 )
                     GO TO 100
                  END IF
*
                  TIME = ( TIME-UNTIME ) / DBLE( IC )
                  OPS = DOPGB( 'DGTSV ', M, NRHS, 0, 0, IWORK )
                  RESLTS( I, IM, ILDA, 3 ) = DMFLOP( OPS, TIME, INFO )
  110          CONTINUE
  120       CONTINUE
         END IF
*
         IF( TIMSUB( 4 ) ) THEN
            CALL DTIMMG( 12, M, M, A, 3*N, 0, 0 )
            CALL DTIMMG( 0, M, 1, B, N, 0, 0 )
            IC = 0
            S1 = DSECND( )
  130       CONTINUE
            CALL DGTSL( M, A, A( N ), A( 2*N ), B, INFO )
            S2 = DSECND( )
            TIME = S2 - S1
            IC = IC + 1
            IF( TIME.LT.TIMMIN ) THEN
               CALL DTIMMG( 12, M, M, A, 3*N, 0, 0 )
               CALL DTIMMG( 0, M, 1, B, LDB, 0, 0 )
               GO TO 130
            END IF
*
*           Subtract the time used in DTIMMG.
*
            ICL = 1
            S1 = DSECND( )
  140       CONTINUE
            S2 = DSECND( )
            UNTIME = S2 - S1
            ICL = ICL + 1
            IF( ICL.LE.IC ) THEN
               CALL DTIMMG( 12, M, M, A, 3*N, 0, 0 )
               CALL DTIMMG( 0, M, 1, B, LDB, 0, 0 )
               GO TO 140
            END IF
*
            TIME = ( TIME-UNTIME ) / DBLE( IC )
            OPS = DOPGB( 'DGTSV ', M, 1, 0, 0, IWORK )
            RESLTS( 1, IM, 1, 4 ) = DMFLOP( OPS, TIME, INFO )
         END IF
  150 CONTINUE
*
*     Print a table of results for each timed routine.
*
      DO 170 ISUB = 1, NSUBS
         IF( .NOT.TIMSUB( ISUB ) )
     $      GO TO 170
         WRITE( NOUT, FMT = 9997 )SUBNAM( ISUB )
         IF( NLDA.GT.1 .AND. ( TIMSUB( 2 ) .OR. TIMSUB( 3 ) ) ) THEN
            DO 160 I = 1, NLDA
               WRITE( NOUT, FMT = 9996 )I, LDAVAL( I )
  160       CONTINUE
         END IF
         WRITE( NOUT, FMT = * )
         IF( ISUB.EQ.1 ) THEN
            CALL DPRTBL( ' ', 'N', 1, LAVAL, NM, MVAL, 1, RESLTS, LDR1,
     $                   LDR2, NOUT )
         ELSE IF( ISUB.EQ.2 ) THEN
            WRITE( NOUT, FMT = 9999 )'N'
 9999       FORMAT( ' DGTTRS with TRANS = ''', A1, '''', / )
            CALL DPRTBL( 'NRHS', 'N', NNS, NSVAL, NM, MVAL, NLDA,
     $                   RESLTS( 1, 1, 1, 2 ), LDR1, LDR2, NOUT )
            WRITE( NOUT, FMT = 9999 )'T'
            CALL DPRTBL( 'NRHS', 'N', NNS, NSVAL, NM, MVAL, NLDA,
     $                   RESLTS( 1, 1, 1, 5 ), LDR1, LDR2, NOUT )
         ELSE IF( ISUB.EQ.3 ) THEN
            CALL DPRTBL( 'NRHS', 'N', NNS, NSVAL, NM, MVAL, NLDA,
     $                   RESLTS( 1, 1, 1, 3 ), LDR1, LDR2, NOUT )
         ELSE IF( ISUB.EQ.4 ) THEN
            CALL DPRTBL( ' ', 'N', 1, LAVAL, NM, MVAL, 1,
     $                   RESLTS( 1, 1, 1, 4 ), LDR1, LDR2, NOUT )
         END IF
  170 CONTINUE
*
  180 CONTINUE
 9998 FORMAT( 1X, A6, ' timing run not attempted', / )
 9997 FORMAT( / ' *** Speed of ', A6, ' in megaflops ***' )
 9996 FORMAT( 5X, 'line ', I2, ' with LDA = ', I5 )
      RETURN
*
*     End of DTIMGT
*
      END
      SUBROUTINE DTIMHR( LINE, NM, MVAL, NN, NVAL, NNB, NBVAL, NXVAL,
     $                   NLDA, LDAVAL, TIMMIN, A, TAU, B, WORK, RESLTS,
     $                   LDR1, LDR2, LDR3, NOUT )
*
*  -- LAPACK timing routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     March 31, 1993
*
*     .. Scalar Arguments ..
      CHARACTER*80       LINE
      INTEGER            LDR1, LDR2, LDR3, NLDA, NM, NN, NNB, NOUT
      DOUBLE PRECISION   TIMMIN
*     ..
*     .. Array Arguments ..
      INTEGER            LDAVAL( * ), MVAL( * ), NBVAL( * ), NVAL( * ),
     $                   NXVAL( * )
      DOUBLE PRECISION   A( * ), B( * ), RESLTS( LDR1, LDR2, LDR3, * ),
     $                   TAU( * ), WORK( * )
*     ..
*
*  Purpose
*  =======
*
*  DTIMHR times the LAPACK routines DGEHRD, DORGHR, and DORMHR and the
*  EISPACK routine ORTHES.
*
*  Arguments
*  =========
*
*  LINE    (input) CHARACTER*80
*          The input line that requested this routine.  The first six
*          characters contain either the name of a subroutine or a
*          generic path name.  The remaining characters may be used to
*          specify the individual routines to be timed.  See ATIMIN for
*          a full description of the format of the input line.
*
*  NM      (input) INTEGER
*          The number of values of M contained in the vector MVAL.
*
*  MVAL    (input) INTEGER array, dimension (NM)
*          The values of the matrix size M.
*
*  NN      (input) INTEGER
*          The number of values of N contained in the vector NVAL.
*
*  NVAL    (input) INTEGER array, dimension (NN)
*          The values of the matrix column dimension N.
*
*  NNB     (input) INTEGER
*          The number of values of NB and NX contained in the
*          vectors NBVAL and NXVAL.  The blocking parameters are used
*          in pairs (NB,NX).
*
*  NBVAL   (input) INTEGER array, dimension (NNB)
*          The values of the blocksize NB.
*
*  NXVAL   (input) INTEGER array, dimension (NNB)
*          The values of the crossover point NX.
*
*  NLDA    (input) INTEGER
*          The number of values of LDA contained in the vector LDAVAL.
*
*  LDAVAL  (input) INTEGER array, dimension (NLDA)
*          The values of the leading dimension of the array A.
*
*  TIMMIN  (input) DOUBLE PRECISION
*          The minimum time a subroutine will be timed.
*
*  A       (workspace) DOUBLE PRECISION array, dimension (LDAMAX*NMAX)
*          where LDAMAX and NMAX are the maximum values of LDA and N.
*
*  TAU     (workspace) DOUBLE PRECISION array, dimension (min(M,N))
*
*  B       (workspace) DOUBLE PRECISION array, dimension (LDAMAX*NMAX)
*
*  WORK    (workspace) DOUBLE PRECISION array, dimension (LDAMAX*NBMAX)
*          where NBMAX is the maximum value of NB.
*
*  RESLTS  (workspace) DOUBLE PRECISION array, dimension
*                      (LDR1,LDR2,LDR3,4*NN+3)
*          The timing results for each subroutine over the relevant
*          values of M, (NB,NX), LDA, and N.
*
*  LDR1    (input) INTEGER
*          The first dimension of RESLTS.  LDR1 >= max(1,NNB).
*
*  LDR2    (input) INTEGER
*          The second dimension of RESLTS.  LDR2 >= max(1,NM).
*
*  LDR3    (input) INTEGER
*          The third dimension of RESLTS.  LDR3 >= max(1,NLDA).
*
*  NOUT    (input) INTEGER
*          The unit number for output.
*
*  Internal Parameters
*  ===================
*
*  MODE    INTEGER
*          The matrix type.  MODE = 3 is a geometric distribution of
*          eigenvalues.  See ZLATMS for further details.
*
*  COND    DOUBLE PRECISION
*          The condition number of the matrix.  The singular values are
*          set to values from DMAX to DMAX/COND.
*
*  DMAX    DOUBLE PRECISION
*          The magnitude of the largest singular value.
*
*  =====================================================================
*
*     .. Parameters ..
      INTEGER            NSUBS
      PARAMETER          ( NSUBS = 4 )
      INTEGER            MODE
      DOUBLE PRECISION   COND, DMAX
      PARAMETER          ( MODE = 3, COND = 100.0D0, DMAX = 1.0D0 )
*     ..
*     .. Local Scalars ..
      CHARACTER          LAB1, LAB2, SIDE, TRANS
      CHARACTER*3        PATH
      CHARACTER*6        CNAME
      INTEGER            I, I4, IC, ICL, IHI, ILDA, ILO, IM, IN, INB,
     $                   INFO, ISIDE, ISUB, ITOFF, ITRAN, LDA, LW, M,
     $                   M1, N, N1, NB, NX
      DOUBLE PRECISION   OPS, S1, S2, TIME, UNTIME
*     ..
*     .. Local Arrays ..
      LOGICAL            TIMSUB( NSUBS )
      CHARACTER          SIDES( 2 ), TRANSS( 2 )
      CHARACTER*6        SUBNAM( NSUBS )
      INTEGER            ISEED( 4 ), RESEED( 4 )
*     ..
*     .. External Functions ..
      DOUBLE PRECISION   DMFLOP, DOPLA, DSECND
      EXTERNAL           DMFLOP, DOPLA, DSECND
*     ..
*     .. External Subroutines ..
      EXTERNAL           ATIMCK, ATIMIN, DGEHRD, DLACPY, DLATMS, DORGHR,
     $                   DORMHR, DPRTB3, DPRTBL, DTIMMG, ICOPY, ORTHES,
     $                   XLAENV
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          DBLE, MAX
*     ..
*     .. Data statements ..
      DATA               SUBNAM / 'DGEHRD', 'ORTHES', 'DORGHR',
     $                   'DORMHR' /
      DATA               SIDES / 'L', 'R' / , TRANSS / 'N', 'T' /
      DATA               ISEED / 0, 0, 0, 1 /
*     ..
*     .. Executable Statements ..
*
*     Extract the timing request from the input line.
*
      PATH( 1: 1 ) = 'Double precision'
      PATH( 2: 3 ) = 'HR'
      CALL ATIMIN( PATH, LINE, NSUBS, SUBNAM, TIMSUB, NOUT, INFO )
      IF( INFO.NE.0 )
     $   GO TO 210
*
*     Check that N <= LDA for the input values.
*
      CNAME = LINE( 1: 6 )
      CALL ATIMCK( 2, CNAME, NM, MVAL, NLDA, LDAVAL, NOUT, INFO )
      IF( INFO.GT.0 ) THEN
         WRITE( NOUT, FMT = 9999 )CNAME
         GO TO 210
      END IF
*
*     Check that K <= LDA for DORMHR
*
      IF( TIMSUB( 4 ) ) THEN
         CALL ATIMCK( 3, CNAME, NN, NVAL, NLDA, LDAVAL, NOUT, INFO )
         IF( INFO.GT.0 ) THEN
            WRITE( NOUT, FMT = 9999 )SUBNAM( 4 )
            TIMSUB( 4 ) = .FALSE.
         END IF
      END IF
*
*     Do for each value of M:
*
      DO 140 IM = 1, NM
         M = MVAL( IM )
         ILO = 1
         IHI = M
         CALL ICOPY( 4, ISEED, 1, RESEED, 1 )
*
*        Do for each value of LDA:
*
         DO 130 ILDA = 1, NLDA
            LDA = LDAVAL( ILDA )
*
*           Do for each pair of values (NB, NX) in NBVAL and NXVAL.
*
            DO 120 INB = 1, NNB
               NB = NBVAL( INB )
               CALL XLAENV( 1, NB )
               NX = NXVAL( INB )
               CALL XLAENV( 3, NX )
               LW = MAX( 1, M*MAX( 1, NB ) )
*
*              Generate a test matrix of size M by M.
*
               CALL ICOPY( 4, RESEED, 1, ISEED, 1 )
               CALL DLATMS( M, M, 'Uniform', ISEED, 'Nonsym', TAU, MODE,
     $                      COND, DMAX, M, M, 'No packing', B, LDA,
     $                      WORK, INFO )
*
               IF( TIMSUB( 2 ) .AND. INB.EQ.1 ) THEN
*
*                 ORTHES:  Eispack reduction using orthogonal
*                 transformations.
*
                  CALL DLACPY( 'Full', M, M, B, LDA, A, LDA )
                  IC = 0
                  S1 = DSECND( )
   10             CONTINUE
                  CALL ORTHES( LDA, M, 1, IHI, A, TAU )
                  S2 = DSECND( )
                  TIME = S2 - S1
                  IC = IC + 1
                  IF( TIME.LT.TIMMIN ) THEN
                     CALL DLACPY( 'Full', M, M, B, LDA, A, LDA )
                     GO TO 10
                  END IF
*
*                 Subtract the time used in DLACPY.
*
                  ICL = 1
                  S1 = DSECND( )
   20             CONTINUE
                  S2 = DSECND( )
                  UNTIME = S2 - S1
                  ICL = ICL + 1
                  IF( ICL.LE.IC ) THEN
                     CALL DLACPY( 'Full', M, M, B, LDA, A, LDA )
                     GO TO 20
                  END IF
*
                  TIME = ( TIME-UNTIME ) / DBLE( IC )
                  OPS = DOPLA( 'DGEHRD', M, ILO, IHI, 0, NB )
                  RESLTS( INB, IM, ILDA, 2 ) = DMFLOP( OPS, TIME, INFO )
               END IF
*
               IF( TIMSUB( 1 ) ) THEN
*
*                 DGEHRD:  Reduction to Hesenberg form
*
                  CALL DLACPY( 'Full', M, M, B, LDA, A, LDA )
                  IC = 0
                  S1 = DSECND( )
   30             CONTINUE
                  CALL DGEHRD( M, ILO, IHI, A, LDA, TAU, WORK, LW,
     $                         INFO )
                  S2 = DSECND( )
                  TIME = S2 - S1
                  IC = IC + 1
                  IF( TIME.LT.TIMMIN ) THEN
                     CALL DLACPY( 'Full', M, M, B, LDA, A, LDA )
                     GO TO 30
                  END IF
*
*                 Subtract the time used in DLACPY.
*
                  ICL = 1
                  S1 = DSECND( )
   40             CONTINUE
                  S2 = DSECND( )
                  UNTIME = S2 - S1
                  ICL = ICL + 1
                  IF( ICL.LE.IC ) THEN
                     CALL DLACPY( 'Full', M, M, A, LDA, B, LDA )
                     GO TO 40
                  END IF
*
                  TIME = ( TIME-UNTIME ) / DBLE( IC )
                  OPS = DOPLA( 'DGEHRD', M, ILO, IHI, 0, NB )
                  RESLTS( INB, IM, ILDA, 1 ) = DMFLOP( OPS, TIME, INFO )
               ELSE
*
*                 If DGEHRD was not timed, generate a matrix and factor
*                 it using DGEHRD anyway so that the factored form of
*                 the matrix can be used in timing the other routines.
*
                  CALL DLACPY( 'Full', M, M, B, LDA, A, LDA )
                  CALL DGEHRD( M, ILO, IHI, A, LDA, TAU, WORK, LW,
     $                         INFO )
               END IF
*
               IF( TIMSUB( 3 ) ) THEN
*
*                 DORGHR:  Generate the orthogonal matrix Q from the
*                 reduction to Hessenberg form A = Q*H*Q'
*
                  CALL DLACPY( 'Full', M, M, A, LDA, B, LDA )
                  IC = 0
                  S1 = DSECND( )
   50             CONTINUE
                  CALL DORGHR( M, ILO, IHI, B, LDA, TAU, WORK, LW,
     $                         INFO )
                  S2 = DSECND( )
                  TIME = S2 - S1
                  IC = IC + 1
                  IF( TIME.LT.TIMMIN ) THEN
                     CALL DLACPY( 'Full', M, M, A, LDA, B, LDA )
                     GO TO 50
                  END IF
*
*                 Subtract the time used in DLACPY.
*
                  ICL = 1
                  S1 = DSECND( )
   60             CONTINUE
                  S2 = DSECND( )
                  UNTIME = S2 - S1
                  ICL = ICL + 1
                  IF( ICL.LE.IC ) THEN
                     CALL DLACPY( 'Full', M, M, A, LDA, B, LDA )
                     GO TO 60
                  END IF
*
                  TIME = ( TIME-UNTIME ) / DBLE( IC )
*
*                 Op count for DORGHR:  same as
*                    DORGQR( IHI-ILO, IHI-ILO, IHI-ILO, ... )
*
                  OPS = DOPLA( 'DORGQR', IHI-ILO, IHI-ILO, IHI-ILO, 0,
     $                  NB )
                  RESLTS( INB, IM, ILDA, 3 ) = DMFLOP( OPS, TIME, INFO )
               END IF
*
               IF( TIMSUB( 4 ) ) THEN
*
*                 DORMHR:  Multiply by Q stored as a product of
*                 elementary transformations
*
                  I4 = 3
                  DO 110 ISIDE = 1, 2
                     SIDE = SIDES( ISIDE )
                     DO 100 IN = 1, NN
                        N = NVAL( IN )
                        LW = MAX( 1, MAX( 1, NB )*N )
                        IF( ISIDE.EQ.1 ) THEN
                           M1 = M
                           N1 = N
                        ELSE
                           M1 = N
                           N1 = M
                        END IF
                        ITOFF = 0
                        DO 90 ITRAN = 1, 2
                           TRANS = TRANSS( ITRAN )
                           CALL DTIMMG( 0, M1, N1, B, LDA, 0, 0 )
                           IC = 0
                           S1 = DSECND( )
   70                      CONTINUE
                           CALL DORMHR( SIDE, TRANS, M1, N1, ILO, IHI,
     $                                  A, LDA, TAU, B, LDA, WORK, LW,
     $                                  INFO )
                           S2 = DSECND( )
                           TIME = S2 - S1
                           IC = IC + 1
                           IF( TIME.LT.TIMMIN ) THEN
                              CALL DTIMMG( 0, M1, N1, B, LDA, 0, 0 )
                              GO TO 70
                           END IF
*
*                          Subtract the time used in DTIMMG.
*
                           ICL = 1
                           S1 = DSECND( )
   80                      CONTINUE
                           S2 = DSECND( )
                           UNTIME = S2 - S1
                           ICL = ICL + 1
                           IF( ICL.LE.IC ) THEN
                              CALL DTIMMG( 0, M1, N1, B, LDA, 0, 0 )
                              GO TO 80
                           END IF
*
                           TIME = ( TIME-UNTIME ) / DBLE( IC )
*
*                          Op count for DORMHR, SIDE='L':  same as
*                          DORMQR( 'L', TRANS, IHI-ILO, N, IHI-ILO, ...)
*
*                          Op count for DORMHR, SIDE='R':  same as
*                          DORMQR( 'R', TRANS, M, IHI-ILO, IHI-ILO, ...)
*
                           IF( ISIDE.EQ.1 ) THEN
                              OPS = DOPLA( 'DORMQR', IHI-ILO, N1,
     $                              IHI-ILO, -1, NB )
                           ELSE
                              OPS = DOPLA( 'DORMQR', M1, IHI-ILO,
     $                              IHI-ILO, 1, NB )
                           END IF
*
                           RESLTS( INB, IM, ILDA,
     $                        I4+ITOFF+IN ) = DMFLOP( OPS, TIME, INFO )
                           ITOFF = NN
   90                   CONTINUE
  100                CONTINUE
                     I4 = I4 + 2*NN
  110             CONTINUE
               END IF
*
  120       CONTINUE
  130    CONTINUE
  140 CONTINUE
*
*     Print tables of results for DGEHRD, ORTHES, and DORGHR
*
      DO 160 ISUB = 1, NSUBS - 1
         IF( .NOT.TIMSUB( ISUB ) )
     $      GO TO 160
         WRITE( NOUT, FMT = 9998 )SUBNAM( ISUB )
         IF( NLDA.GT.1 ) THEN
            DO 150 I = 1, NLDA
               WRITE( NOUT, FMT = 9997 )I, LDAVAL( I )
  150       CONTINUE
         END IF
         WRITE( NOUT, FMT = 9995 )
         IF( ISUB.EQ.2 ) THEN
            CALL DPRTB3( ' ', 'N', 1, NBVAL, NXVAL, NM, MVAL, NLDA,
     $                   RESLTS( 1, 1, 1, ISUB ), LDR1, LDR2, NOUT )
         ELSE
            CALL DPRTB3( '(  NB,  NX)', 'N', NNB, NBVAL, NXVAL, NM,
     $                   MVAL, NLDA, RESLTS( 1, 1, 1, ISUB ), LDR1,
     $                   LDR2, NOUT )
         END IF
  160 CONTINUE
*
*     Print tables of results for DORMHR
*
      ISUB = 4
      IF( TIMSUB( ISUB ) ) THEN
         I4 = 3
         DO 200 ISIDE = 1, 2
            IF( ISIDE.EQ.1 ) THEN
               LAB1 = 'M'
               LAB2 = 'N'
               IF( NLDA.GT.1 ) THEN
                  WRITE( NOUT, FMT = 9998 )SUBNAM( ISUB )
                  DO 170 I = 1, NLDA
                     WRITE( NOUT, FMT = 9997 )I, LDAVAL( I )
  170             CONTINUE
                  WRITE( NOUT, FMT = 9994 )
               END IF
            ELSE
               LAB1 = 'N'
               LAB2 = 'M'
            END IF
            DO 190 ITRAN = 1, 2
               DO 180 IN = 1, NN
                  WRITE( NOUT, FMT = 9996 )SUBNAM( ISUB ),
     $               SIDES( ISIDE ), TRANSS( ITRAN ), LAB2, NVAL( IN )
                  CALL DPRTBL( 'NB', LAB1, NNB, NBVAL, NM, MVAL, NLDA,
     $                         RESLTS( 1, 1, 1, I4+IN ), LDR1, LDR2,
     $                         NOUT )
  180          CONTINUE
               I4 = I4 + NN
  190       CONTINUE
  200    CONTINUE
      END IF
  210 CONTINUE
*
*     Print a table of results for each timed routine.
*
 9999 FORMAT( 1X, A6, ' timing run not attempted', / )
 9998 FORMAT( / ' *** Speed of ', A6, ' in megaflops *** ' )
 9997 FORMAT( 5X, 'line ', I2, ' with LDA = ', I5 )
 9996 FORMAT( / 5X, A6, ' with SIDE = ''', A1, ''', TRANS = ''', A1,
     $      ''', ', A1, ' =', I6, / )
 9995 FORMAT( / 5X, 'ILO = 1, IHI = N', / )
 9994 FORMAT( / 5X, 'ILO = 1, IHI = M if SIDE = ''L''', / 5X,
     $      '             = N if SIDE = ''R''' )
      RETURN
*
*     End of DTIMHR
*
      END
      SUBROUTINE DTIMLQ( LINE, NM, MVAL, NVAL, NK, KVAL, NNB, NBVAL,
     $                   NXVAL, NLDA, LDAVAL, TIMMIN, A, TAU, B, WORK,
     $                   RESLTS, LDR1, LDR2, LDR3, NOUT )
*
*  -- LAPACK timing routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     March 31, 1993
*
*     .. Scalar Arguments ..
      CHARACTER*80       LINE
      INTEGER            LDR1, LDR2, LDR3, NK, NLDA, NM, NNB, NOUT
      DOUBLE PRECISION   TIMMIN
*     ..
*     .. Array Arguments ..
      INTEGER            KVAL( * ), LDAVAL( * ), MVAL( * ), NBVAL( * ),
     $                   NVAL( * ), NXVAL( * )
      DOUBLE PRECISION   A( * ), B( * ), RESLTS( LDR1, LDR2, LDR3, * ),
     $                   TAU( * ), WORK( * )
*     ..
*
*  Purpose
*  =======
*
*  DTIMLQ times the LAPACK routines to perform the LQ factorization of
*  a DOUBLE PRECISION general matrix.
*
*  Arguments
*  =========
*
*  LINE    (input) CHARACTER*80
*          The input line that requested this routine.  The first six
*          characters contain either the name of a subroutine or a
*          generic path name.  The remaining characters may be used to
*          specify the individual routines to be timed.  See ATIMIN for
*          a full description of the format of the input line.
*
*  NM      (input) INTEGER
*          The number of values of M and N contained in the vectors
*          MVAL and NVAL.  The matrix sizes are used in pairs (M,N).
*
*  MVAL    (input) INTEGER array, dimension (NM)
*          The values of the matrix row dimension M.
*
*  NVAL    (input) INTEGER array, dimension (NM)
*          The values of the matrix column dimension N.
*
*  NK      (input) INTEGER
*          The number of values of K in the vector KVAL.
*
*  KVAL    (input) INTEGER array, dimension (NK)
*          The values of the matrix dimension K, used in DORMLQ.
*
*  NNB     (input) INTEGER
*          The number of values of NB and NX contained in the
*          vectors NBVAL and NXVAL.  The blocking parameters are used
*          in pairs (NB,NX).
*
*  NBVAL   (input) INTEGER array, dimension (NNB)
*          The values of the blocksize NB.
*
*  NXVAL   (input) INTEGER array, dimension (NNB)
*          The values of the crossover point NX.
*
*  NLDA    (input) INTEGER
*          The number of values of LDA contained in the vector LDAVAL.
*
*  LDAVAL  (input) INTEGER array, dimension (NLDA)
*          The values of the leading dimension of the array A.
*
*  TIMMIN  (input) DOUBLE PRECISION
*          The minimum time a subroutine will be timed.
*
*  A       (workspace) DOUBLE PRECISION array, dimension (LDAMAX*NMAX)
*          where LDAMAX and NMAX are the maximum values of LDA and N.
*
*  TAU     (workspace) DOUBLE PRECISION array, dimension (min(M,N))
*
*  B       (workspace) DOUBLE PRECISION array, dimension (LDAMAX*NMAX)
*
*  WORK    (workspace) DOUBLE PRECISION array, dimension (LDAMAX*NBMAX)
*          where NBMAX is the maximum value of NB.
*
*  RESLTS  (workspace) DOUBLE PRECISION array, dimension
*                      (LDR1,LDR2,LDR3,2*NK)
*          The timing results for each subroutine over the relevant
*          values of (M,N), (NB,NX), and LDA.
*
*  LDR1    (input) INTEGER
*          The first dimension of RESLTS.  LDR1 >= max(1,NNB).
*
*  LDR2    (input) INTEGER
*          The second dimension of RESLTS.  LDR2 >= max(1,NM).
*
*  LDR3    (input) INTEGER
*          The third dimension of RESLTS.  LDR3 >= max(1,NLDA).
*
*  NOUT    (input) INTEGER
*          The unit number for output.
*
*  Internal Parameters
*  ===================
*
*  MODE    INTEGER
*          The matrix type.  MODE = 3 is a geometric distribution of
*          eigenvalues.  See DLATMS for further details.
*
*  COND    DOUBLE PRECISION
*          The condition number of the matrix.  The singular values are
*          set to values from DMAX to DMAX/COND.
*
*  DMAX    DOUBLE PRECISION
*          The magnitude of the largest singular value.
*
*  =====================================================================
*
*     .. Parameters ..
      INTEGER            NSUBS
      PARAMETER          ( NSUBS = 3 )
      INTEGER            MODE
      DOUBLE PRECISION   COND, DMAX
      PARAMETER          ( MODE = 3, COND = 100.0D0, DMAX = 1.0D0 )
*     ..
*     .. Local Scalars ..
      CHARACTER          LABM, SIDE, TRANS
      CHARACTER*3        PATH
      CHARACTER*6        CNAME
      INTEGER            I, I4, IC, ICL, IK, ILDA, IM, IMX, INB, INFO,
     $                   ISIDE, ISUB, ITOFF, ITRAN, K, K1, LDA, LW, M,
     $                   M1, MINMN, N, N1, NB, NX
      DOUBLE PRECISION   OPS, S1, S2, TIME, UNTIME
*     ..
*     .. Local Arrays ..
      LOGICAL            TIMSUB( NSUBS )
      CHARACTER          SIDES( 2 ), TRANSS( 2 )
      CHARACTER*6        SUBNAM( NSUBS )
      INTEGER            ISEED( 4 ), MUSE( 12 ), NUSE( 12 ), RESEED( 4 )
*     ..
*     .. External Functions ..
      DOUBLE PRECISION   DMFLOP, DOPLA, DSECND
      EXTERNAL           DMFLOP, DOPLA, DSECND
*     ..
*     .. External Subroutines ..
      EXTERNAL           ATIMCK, ATIMIN, DGELQF, DLACPY, DLATMS, DORGLQ,
     $                   DORMLQ, DPRTB4, DPRTB5, DTIMMG, ICOPY, XLAENV
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          DBLE, MAX, MIN
*     ..
*     .. Data statements ..
      DATA               SUBNAM / 'DGELQF', 'DORGLQ', 'DORMLQ' /
      DATA               SIDES / 'L', 'R' / , TRANSS / 'N', 'T' /
      DATA               ISEED / 0, 0, 0, 1 /
*     ..
*     .. Executable Statements ..
*
*     Extract the timing request from the input line.
*
      PATH( 1: 1 ) = 'Double precision'
      PATH( 2: 3 ) = 'LQ'
      CALL ATIMIN( PATH, LINE, NSUBS, SUBNAM, TIMSUB, NOUT, INFO )
      IF( INFO.NE.0 )
     $   GO TO 230
*
*     Check that M <= LDA for the input values.
*
      CNAME = LINE( 1: 6 )
      CALL ATIMCK( 1, CNAME, NM, MVAL, NLDA, LDAVAL, NOUT, INFO )
      IF( INFO.GT.0 ) THEN
         WRITE( NOUT, FMT = 9999 )CNAME
         GO TO 230
      END IF
*
*     Do for each pair of values (M,N):
*
      DO 70 IM = 1, NM
         M = MVAL( IM )
         N = NVAL( IM )
         MINMN = MIN( M, N )
         CALL ICOPY( 4, ISEED, 1, RESEED, 1 )
*
*        Do for each value of LDA:
*
         DO 60 ILDA = 1, NLDA
            LDA = LDAVAL( ILDA )
*
*           Do for each pair of values (NB, NX) in NBVAL and NXVAL.
*
            DO 50 INB = 1, NNB
               NB = NBVAL( INB )
               CALL XLAENV( 1, NB )
               NX = NXVAL( INB )
               CALL XLAENV( 3, NX )
               LW = MAX( 1, M*MAX( 1, NB ) )
*
*              Generate a test matrix of size M by N.
*
               CALL ICOPY( 4, RESEED, 1, ISEED, 1 )
               CALL DLATMS( M, N, 'Uniform', ISEED, 'Nonsym', TAU, MODE,
     $                      COND, DMAX, M, N, 'No packing', B, LDA,
     $                      WORK, INFO )
*
               IF( TIMSUB( 1 ) ) THEN
*
*                 DGELQF:  LQ factorization
*
                  CALL DLACPY( 'Full', M, N, B, LDA, A, LDA )
                  IC = 0
                  S1 = DSECND( )
   10             CONTINUE
                  CALL DGELQF( M, N, A, LDA, TAU, WORK, LW, INFO )
                  S2 = DSECND( )
                  TIME = S2 - S1
                  IC = IC + 1
                  IF( TIME.LT.TIMMIN ) THEN
                     CALL DLACPY( 'Full', M, N, B, LDA, A, LDA )
                     GO TO 10
                  END IF
*
*                 Subtract the time used in DLACPY.
*
                  ICL = 1
                  S1 = DSECND( )
   20             CONTINUE
                  S2 = DSECND( )
                  UNTIME = S2 - S1
                  ICL = ICL + 1
                  IF( ICL.LE.IC ) THEN
                     CALL DLACPY( 'Full', M, N, A, LDA, B, LDA )
                     GO TO 20
                  END IF
*
                  TIME = ( TIME-UNTIME ) / DBLE( IC )
                  OPS = DOPLA( 'DGELQF', M, N, 0, 0, NB )
                  RESLTS( INB, IM, ILDA, 1 ) = DMFLOP( OPS, TIME, INFO )
               ELSE
*
*                 If DGELQF was not timed, generate a matrix and factor
*                 it using DGELQF anyway so that the factored form of
*                 the matrix can be used in timing the other routines.
*
                  CALL DLACPY( 'Full', M, N, B, LDA, A, LDA )
                  CALL DGELQF( M, N, A, LDA, TAU, WORK, LW, INFO )
               END IF
*
               IF( TIMSUB( 2 ) ) THEN
*
*                 DORGLQ:  Generate orthogonal matrix Q from the LQ
*                 factorization
*
                  CALL DLACPY( 'Full', MINMN, N, A, LDA, B, LDA )
                  IC = 0
                  S1 = DSECND( )
   30             CONTINUE
                  CALL DORGLQ( MINMN, N, MINMN, B, LDA, TAU, WORK, LW,
     $                         INFO )
                  S2 = DSECND( )
                  TIME = S2 - S1
                  IC = IC + 1
                  IF( TIME.LT.TIMMIN ) THEN
                     CALL DLACPY( 'Full', MINMN, N, A, LDA, B, LDA )
                     GO TO 30
                  END IF
*
*                 Subtract the time used in DLACPY.
*
                  ICL = 1
                  S1 = DSECND( )
   40             CONTINUE
                  S2 = DSECND( )
                  UNTIME = S2 - S1
                  ICL = ICL + 1
                  IF( ICL.LE.IC ) THEN
                     CALL DLACPY( 'Full', MINMN, N, A, LDA, B, LDA )
                     GO TO 40
                  END IF
*
                  TIME = ( TIME-UNTIME ) / DBLE( IC )
                  OPS = DOPLA( 'DORGLQ', MINMN, N, MINMN, 0, NB )
                  RESLTS( INB, IM, ILDA, 2 ) = DMFLOP( OPS, TIME, INFO )
               END IF
*
   50       CONTINUE
   60    CONTINUE
   70 CONTINUE
*
*     Print tables of results
*
      DO 90 ISUB = 1, NSUBS - 1
         IF( .NOT.TIMSUB( ISUB ) )
     $      GO TO 90
         WRITE( NOUT, FMT = 9998 )SUBNAM( ISUB )
         IF( NLDA.GT.1 ) THEN
            DO 80 I = 1, NLDA
               WRITE( NOUT, FMT = 9997 )I, LDAVAL( I )
   80       CONTINUE
         END IF
         WRITE( NOUT, FMT = * )
         IF( ISUB.EQ.2 )
     $      WRITE( NOUT, FMT = 9996 )
         CALL DPRTB4( '(  NB,  NX)', 'M', 'N', NNB, NBVAL, NXVAL, NM,
     $                MVAL, NVAL, NLDA, RESLTS( 1, 1, 1, ISUB ), LDR1,
     $                LDR2, NOUT )
   90 CONTINUE
*
*     Time DORMLQ separately.  Here the starting matrix is M by N, and
*     K is the free dimension of the matrix multiplied by Q.
*
      IF( TIMSUB( 3 ) ) THEN
*
*        Check that K <= LDA for the input values.
*
         CALL ATIMCK( 3, CNAME, NK, KVAL, NLDA, LDAVAL, NOUT, INFO )
         IF( INFO.GT.0 ) THEN
            WRITE( NOUT, FMT = 9999 )SUBNAM( 3 )
            GO TO 230
         END IF
*
*        Use only the pairs (M,N) where M <= N.
*
         IMX = 0
         DO 100 IM = 1, NM
            IF( MVAL( IM ).LE.NVAL( IM ) ) THEN
               IMX = IMX + 1
               MUSE( IMX ) = MVAL( IM )
               NUSE( IMX ) = NVAL( IM )
            END IF
  100    CONTINUE
*
*        DORMLQ:  Multiply by Q stored as a product of elementary
*        transformations
*
*        Do for each pair of values (M,N):
*
         DO 180 IM = 1, IMX
            M = MUSE( IM )
            N = NUSE( IM )
*
*           Do for each value of LDA:
*
            DO 170 ILDA = 1, NLDA
               LDA = LDAVAL( ILDA )
*
*              Generate an M by N matrix and form its LQ decomposition.
*
               CALL DLATMS( M, N, 'Uniform', ISEED, 'Nonsymm', TAU,
     $                      MODE, COND, DMAX, M, N, 'No packing', A,
     $                      LDA, WORK, INFO )
               LW = MAX( 1, M*MAX( 1, NB ) )
               CALL DGELQF( M, N, A, LDA, TAU, WORK, LW, INFO )
*
*              Do first for SIDE = 'L', then for SIDE = 'R'
*
               I4 = 0
               DO 160 ISIDE = 1, 2
                  SIDE = SIDES( ISIDE )
*
*                 Do for each pair of values (NB, NX) in NBVAL and
*                 NXVAL.
*
                  DO 150 INB = 1, NNB
                     NB = NBVAL( INB )
                     CALL XLAENV( 1, NB )
                     NX = NXVAL( INB )
                     CALL XLAENV( 3, NX )
*
*                    Do for each value of K in KVAL
*
                     DO 140 IK = 1, NK
                        K = KVAL( IK )
*
*                       Sort out which variable is which
*
                        IF( ISIDE.EQ.1 ) THEN
                           K1 = M
                           M1 = N
                           N1 = K
                           LW = MAX( 1, N1*MAX( 1, NB ) )
                        ELSE
                           K1 = M
                           N1 = N
                           M1 = K
                           LW = MAX( 1, M1*MAX( 1, NB ) )
                        END IF
*
*                       Do first for TRANS = 'N', then for TRANS = 'T'
*
                        ITOFF = 0
                        DO 130 ITRAN = 1, 2
                           TRANS = TRANSS( ITRAN )
                           CALL DTIMMG( 0, M1, N1, B, LDA, 0, 0 )
                           IC = 0
                           S1 = DSECND( )
  110                      CONTINUE
                           CALL DORMLQ( SIDE, TRANS, M1, N1, K1, A, LDA,
     $                                  TAU, B, LDA, WORK, LW, INFO )
                           S2 = DSECND( )
                           TIME = S2 - S1
                           IC = IC + 1
                           IF( TIME.LT.TIMMIN ) THEN
                              CALL DTIMMG( 0, M1, N1, B, LDA, 0, 0 )
                              GO TO 110
                           END IF
*
*                          Subtract the time used in DTIMMG.
*
                           ICL = 1
                           S1 = DSECND( )
  120                      CONTINUE
                           S2 = DSECND( )
                           UNTIME = S2 - S1
                           ICL = ICL + 1
                           IF( ICL.LE.IC ) THEN
                              CALL DTIMMG( 0, M1, N1, B, LDA, 0, 0 )
                              GO TO 120
                           END IF
*
                           TIME = ( TIME-UNTIME ) / DBLE( IC )
                           OPS = DOPLA( 'DORMLQ', M1, N1, K1, ISIDE-1,
     $                           NB )
                           RESLTS( INB, IM, ILDA,
     $                        I4+ITOFF+IK ) = DMFLOP( OPS, TIME, INFO )
                           ITOFF = NK
  130                   CONTINUE
  140                CONTINUE
  150             CONTINUE
                  I4 = 2*NK
  160          CONTINUE
  170       CONTINUE
  180    CONTINUE
*
*        Print tables of results
*
         ISUB = 3
         I4 = 1
         IF( IMX.GE.1 ) THEN
            DO 220 ISIDE = 1, 2
               SIDE = SIDES( ISIDE )
               IF( ISIDE.EQ.1 ) THEN
                  WRITE( NOUT, FMT = 9998 )SUBNAM( ISUB )
                  IF( NLDA.GT.1 ) THEN
                     DO 190 I = 1, NLDA
                        WRITE( NOUT, FMT = 9997 )I, LDAVAL( I )
  190                CONTINUE
                  END IF
               END IF
               DO 210 ITRAN = 1, 2
                  TRANS = TRANSS( ITRAN )
                  DO 200 IK = 1, NK
                     IF( ISIDE.EQ.1 ) THEN
                        N = KVAL( IK )
                        WRITE( NOUT, FMT = 9995 )SUBNAM( ISUB ), SIDE,
     $                     TRANS, 'N', N
                        LABM = 'M'
                     ELSE
                        M = KVAL( IK )
                        WRITE( NOUT, FMT = 9995 )SUBNAM( ISUB ), SIDE,
     $                     TRANS, 'M', M
                        LABM = 'N'
                     END IF
                     CALL DPRTB5( 'NB', 'K', LABM, NNB, NBVAL, IMX,
     $                            MUSE, NUSE, NLDA,
     $                            RESLTS( 1, 1, 1, I4 ), LDR1, LDR2,
     $                            NOUT )
                     I4 = I4 + 1
  200             CONTINUE
  210          CONTINUE
  220       CONTINUE
         ELSE
            WRITE( NOUT, FMT = 9994 )SUBNAM( ISUB )
         END IF
      END IF
  230 CONTINUE
 9999 FORMAT( 1X, A6, ' timing run not attempted', / )
 9998 FORMAT( / ' *** Speed of ', A6, ' in megaflops ***' )
 9997 FORMAT( 5X, 'line ', I2, ' with LDA = ', I5 )
 9996 FORMAT( 5X, 'K = min(M,N)', / )
 9995 FORMAT( / 5X, A6, ' with SIDE = ''', A1, ''', TRANS = ''', A1,
     $      ''', ', A1, ' =', I6, / )
 9994 FORMAT( ' *** No pairs (M,N) found with M <= N:  ', A6,
     $      ' not timed' )
      RETURN
*
*     End of DTIMLQ
*
      END
      SUBROUTINE DTIMLS( LINE, NM, MVAL, NN, NVAL, NNS, NSVAL, NNB,
     $                   NBVAL, NXVAL, NLDA, LDAVAL, TIMMIN, A, COPYA,
     $                   B, COPYB, S, COPYS, OPCTBL, TIMTBL, FLPTBL,
     $                   WORK, IWORK, NOUT )
*
*  -- LAPACK timing routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     December 22, 1999
*
*     .. Scalar Arguments ..
      CHARACTER*80       LINE
      INTEGER            NLDA, NM, NN, NNB, NNS, NOUT
      DOUBLE PRECISION   TIMMIN
*     ..
*     .. Array Arguments ..
      INTEGER            IWORK( * ), LDAVAL( * ), MVAL( * ), NBVAL( * ),
     $                   NSVAL( * ), NVAL( * ), NXVAL( * )
      DOUBLE PRECISION   A( * ), B( * ), COPYA( * ), COPYB( * ),
     $                   COPYS( * ), FLPTBL( 6, 6,
     $                   NM*NN*NNS*NLDA*( NNB+1 ), * ),
     $                   OPCTBL( 6, 6, NM*NN*NNS*NLDA*( NNB+1 ), * ),
     $                   S( * ), TIMTBL( 6, 6, NM*NN*NNS*NLDA*( NNB+1 ),
     $                   * ), WORK( * )
*     ..
*     .. Common blocks ..
      COMMON             / INFOC / INFOT, IOUNIT, OK, LERR
      COMMON             / LSTIME / OPCNT, TIMNG
      COMMON             / SRNAMC / SRNAMT
*     ..
*     .. Arrays in Common ..
      DOUBLE PRECISION   OPCNT( 6 ), TIMNG( 6 )
*     ..
*
*  Purpose
*  =======
*
*  DTIMLS times the least squares driver routines DGELS, SGELSS, SGELSX,
*  DGELSY and SGELSD.
*
*  Arguments
*  =========
*
*  LINE    (input) CHARACTER*80
*          The input line that requested this routine.  The first six
*          characters contain either the name of a subroutine or a
*          generic path name.  The remaining characters may be used to
*          specify the individual routines to be timed.  See ATIMIN for
*          a full description of the format of the input line.
*
*  NM      (input) INTEGER
*          The number of values of M contained in the vector MVAL.
*
*  MVAL    (input) INTEGER array, dimension (NM)
*          The values of the matrix row dimension M.
*
*  NN      (input) INTEGER
*          The number of values of N contained in the vector NVAL.
*
*  NVAL    (input) INTEGER array, dimension (NN)
*          The values of the matrix column dimension N.
*
*  NNS     (input) INTEGER
*          The number of values of NRHS contained in the vector NSVAL.
*
*  NSVAL   (input) INTEGER array, dimension (NNS)
*          The values of the number of right hand sides NRHS.
*
*  NNB     (input) INTEGER
*          The number of values of NB and NX contained in the
*          vectors NBVAL and NXVAL.  The blocking parameters are used
*          in pairs (NB,NX).
*
*  NBVAL   (input) INTEGER array, dimension (NNB)
*          The values of the blocksize NB.
*
*  NXVAL   (input) INTEGER array, dimension (NNB)
*          The values of the crossover point NX.
*
*  NLDA    (input) INTEGER
*          The number of values of LDA contained in the vector LDAVAL.
*
*  LDAVAL  (input) INTEGER array, dimension (NLDA)
*          The values of the leading dimension of the array A.
*
*  TIMMIN  (input) DOUBLE PRECISION
*          The minimum time a subroutine will be timed.
*
*  A       (workspace) DOUBLE PRECISION array, dimension (MMAX*NMAX)
*          where MMAX is the maximum value of M in MVAL and NMAX is the
*          maximum value of N in NVAL.
*
*  COPYA   (workspace) DOUBLE PRECISION array, dimension (MMAX*NMAX)
*
*  B       (workspace) DOUBLE PRECISION array, dimension (MMAX*NSMAX)
*          where MMAX is the maximum value of M in MVAL and NSMAX is the
*          maximum value of NRHS in NSVAL.
*
*  COPYB   (workspace) DOUBLE PRECISION array, dimension (MMAX*NSMAX)
*
*  S       (workspace) DOUBLE PRECISION array, dimension
*                      (min(MMAX,NMAX))
*
*  COPYS   (workspace) DOUBLE PRECISION array, dimension
*                      (min(MMAX,NMAX))
*
*  OPZTBL  (workspace) DOUBLE PRECISION array, dimension
*                      (6,6,(NNB+1)*NLDA,NM*NN*NNS,5)
*
*  TIMTBL  (workspace) DOUBLE PRECISION array, dimension
*                      (6,6,(NNB+1)*NLDA,NM*NN*NNS,5)
*
*  FLPTBL  (workspace) DOUBLE PRECISION array, dimension
*                      (6,6,(NNB+1)*NLDA,NM*NN*NNS,5)
*
*  WORK    (workspace) DOUBLE PRECISION array,
*                      dimension (MMAX*NMAX + 4*NMAX + MMAX).
*
*  IWORK   (workspace) INTEGER array, dimension (NMAX)
*
*  NOUT    (input) INTEGER
*          The unit number for output.
*
*  =====================================================================
*
*     .. Parameters ..
      INTEGER            MTYPE, NSUBS
      PARAMETER          ( MTYPE = 6, NSUBS = 5 )
      INTEGER            SMLSIZ
      PARAMETER          ( SMLSIZ = 25 )
      DOUBLE PRECISION   ONE, TWO, ZERO
      PARAMETER          ( ONE = 1.0D0, TWO = 2.0D0, ZERO = 0.0D0 )
*     ..
*     .. Local Scalars ..
      CHARACTER          TRANS
      CHARACTER*3        PATH
      INTEGER            CRANK, I, ILDA, IM, IN, INB, INFO, INS, IRANK,
     $                   ISCALE, ISUB, ITBL, ITRAN, ITYPE, LDA, LDB,
     $                   LDWORK, LWLSY, LWORK, M, MNMIN, N, NB, NCALL,
     $                   NCLS, NCLSD, NCLSS, NCLSX, NCLSY, NCOLS, NLVL,
     $                   NRHS, NROWS, RANK
      DOUBLE PRECISION   EPS, NORMA, NORMB, RCOND, S1, S2, TIME
*     ..
*     .. Local Arrays ..
      LOGICAL            TIMSUB( NSUBS )
      CHARACTER*6        SUBNAM( NSUBS )
      INTEGER            ISEED( 4 ), ISEEDY( 4 ), NDATA( NSUBS )
*     ..
*     .. External Functions ..
      DOUBLE PRECISION   DASUM, DLAMCH, DMFLOP, DSECND
      EXTERNAL           DASUM, DLAMCH, DMFLOP, DSECND
*     ..
*     .. External Subroutines ..
      EXTERNAL           ATIMIN, DCOPY, DGELS, DGELSD, DGELSS, DGELSX,
     $                   DGELSY, DGEMM, DLACPY, DLARNV, DLASET, DQRT13,
     $                   DQRT15, DSCAL, DPRTLS, XLAENV
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          DBLE, LOG, MAX, MIN, SQRT
*     ..
*     .. Scalars in Common ..
      LOGICAL            LERR, OK
      CHARACTER*6        SRNAMT
      INTEGER            INFOT, IOUNIT
*     ..
*     .. Common blocks ..
*     ..
*     .. Data statements ..
      DATA               SUBNAM / 'DGELS ', 'DGELSX', 'DGELSY',
     $                   'DGELSS', 'DGELSD' /
      DATA               ISEEDY / 1988, 1989, 1990, 1991 /
      DATA               NDATA / 4, 6, 6, 6, 5 /
*     ..
*     .. Executable Statements ..
*
*     Extract the timing request from the input line.
*
      PATH( 1: 1 ) = 'Double precision'
      PATH( 2: 3 ) = 'LS'
      CALL ATIMIN( PATH, LINE, NSUBS, SUBNAM, TIMSUB, NOUT, INFO )
      IF( INFO.NE.0 )
     $   GO TO 230
*
*     Initialize constants and the random number seed.
*
      NCLS = 0
      NCLSD = 0
      NCLSS = 0
      NCLSX = 0
      NCLSY = 0
      DO 10 I = 1, 4
         ISEED( I ) = ISEEDY( I )
   10 CONTINUE
      EPS = DLAMCH( 'Epsilon' )
*
*     Threshold for rank estimation
*
      RCOND = SQRT( EPS ) - ( SQRT( EPS )-EPS ) / 2
*
      INFOT = 0
      CALL XLAENV( 2, 2 )
      CALL XLAENV( 9, SMLSIZ )
*
      DO 200 IM = 1, NM
         M = MVAL( IM )
*
         DO 190 IN = 1, NN
            N = NVAL( IN )
            MNMIN = MIN( M, N )
*
            DO 180 INS = 1, NNS
               NRHS = NSVAL( INS )
               NLVL = MAX( INT( LOG( MAX( ONE, DBLE( MNMIN ) ) /
     $                DBLE( SMLSIZ+1 ) ) / LOG( TWO ) ) + 1, 0 )
               LWORK = MAX( 1, ( M+NRHS )*( N+2 ), ( N+NRHS )*( M+2 ),
     $                 M*N+4*MNMIN+MAX( M, N ), 12*MNMIN+2*MNMIN*SMLSIZ+
     $                 8*MNMIN*NLVL+MNMIN*NRHS+(SMLSIZ+1)**2 )
*
               DO 170 ILDA = 1, NLDA
                  LDA = MAX( 1, LDAVAL( ILDA ) )
                  LDB = MAX( 1, LDAVAL( ILDA ), M, N )
*
                  DO 160 IRANK = 1, 2
*
                     DO 150 ISCALE = 1, 3
*
                        IF( IRANK.EQ.1 .AND. TIMSUB( 1 ) ) THEN
*
*                          Time DGELS
*
*                          Generate a matrix of scaling type ISCALE
*
                           CALL DQRT13( ISCALE, M, N, COPYA, LDA, NORMA,
     $                                  ISEED )
                           DO 50 INB = 1, NNB
                              NB = NBVAL( INB )
                              CALL XLAENV( 1, NB )
                              CALL XLAENV( 3, NXVAL( INB ) )
*
                              DO 40 ITRAN = 1, 2
                                 ITYPE = ( ITRAN-1 )*3 + ISCALE
                                 IF( ITRAN.EQ.1 ) THEN
                                    TRANS = 'N'
                                    NROWS = M
                                    NCOLS = N
                                 ELSE
                                    TRANS = 'T'
                                    NROWS = N
                                    NCOLS = M
                                 END IF
                                 LDWORK = MAX( 1, NCOLS )
*
*                                Set up a consistent rhs
*
                                 IF( NCOLS.GT.0 ) THEN
                                    CALL DLARNV( 2, ISEED, NCOLS*NRHS,
     $                                           WORK )
                                    CALL DSCAL( NCOLS*NRHS,
     $                                          ONE / DBLE( NCOLS ),
     $                                          WORK, 1 )
                                 END IF
                                 CALL DGEMM( TRANS, 'No transpose',
     $                                       NROWS, NRHS, NCOLS, ONE,
     $                                       COPYA, LDA, WORK, LDWORK,
     $                                       ZERO, B, LDB )
                                 CALL DLACPY( 'Full', NROWS, NRHS, B,
     $                                        LDB, COPYB, LDB )
*
*                                Solve LS or overdetermined system
*
                                 NCALL = 0
                                 TIME = ZERO
                                 CALL DLASET( 'Full', NDATA( 1 ), 1,
     $                                        ZERO, ZERO, OPCNT,
     $                                        NDATA( 1 ) )
                                 CALL DLASET( 'Full', NDATA( 1 ), 1,
     $                                        ZERO, ZERO, TIMNG,
     $                                        NDATA( 1 ) )
   20                            CONTINUE
                                 IF( M.GT.0 .AND. N.GT.0 ) THEN
                                    CALL DLACPY( 'Full', M, N, COPYA,
     $                                           LDA, A, LDA )
                                    CALL DLACPY( 'Full', NROWS, NRHS,
     $                                           COPYB, LDB, B, LDB )
                                 END IF
                                 SRNAMT = 'DGELS '
                                 NCALL = NCALL + 1
                                 S1 = DSECND( )
                                 CALL DGELS( TRANS, M, N, NRHS, A, LDA,
     $                                       B, LDB, WORK, LWORK, INFO )
                                 S2 = DSECND( )
                                 TIME = TIME + ( S2-S1 )
                                 IF( INFO.EQ.0 .AND. TIME.LT.TIMMIN )
     $                              GO TO 20
                                 TIMNG( 1 ) = TIME
                                 OPCNT( 1 ) = DASUM( NDATA( 1 ), OPCNT,
     $                                        1 )
                                 CALL DSCAL( NDATA( 1 ),
     $                                       ONE / DBLE( NCALL ), OPCNT,
     $                                       1 )
                                 CALL DSCAL( NDATA( 1 ),
     $                                       ONE / DBLE( NCALL ), TIMNG,
     $                                       1 )
                                 CALL DCOPY( NDATA( 1 ), OPCNT, 1,
     $                                       OPCTBL( 1, ITYPE, NCLS+INB,
     $                                       1 ), 1 )
                                 CALL DCOPY( NDATA( 1 ), TIMNG, 1,
     $                                       TIMTBL( 1, ITYPE, NCLS+INB,
     $                                       1 ), 1 )
                                 DO 30 I = 1, NDATA( 1 )
                                    FLPTBL( I, ITYPE, NCLS+INB,
     $                                 1 ) = DMFLOP( OPCNT( I ),
     $                                 TIMNG( I ), INFO )
   30                            CONTINUE
   40                         CONTINUE
   50                      CONTINUE
*
                        END IF
*
*                       Generate a matrix of scaling type ISCALE and
*                       rank type IRANK.
*
                        ITYPE = ( IRANK-1 )*3 + ISCALE
                        CALL DQRT15( ISCALE, IRANK, M, N, NRHS, COPYA,
     $                               LDA, COPYB, LDB, COPYS, RANK,
     $                               NORMA, NORMB, ISEED, WORK, LWORK )
*
                        IF( TIMSUB( 2 ) ) THEN
*
*                       Time DGELSX
*
*                       workspace used:
*                       MAX(M+MIN(M,N),NRHS*MIN(M,N),2*N+M)
*
                           LDWORK = MAX( 1, M )
*
*                       DGELSX:  Compute the minimum-norm
*                       solution X to min( norm( A * X - B ) )
*                       using a complete orthogonal factorization.
*
                           NCALL = 0
                           TIME = ZERO
                           CALL DLASET( 'Full', NDATA( 2 ), 1, ZERO,
     $                                  ZERO, OPCNT, NDATA( 2 ) )
                           CALL DLASET( 'Full', NDATA( 2 ), 1, ZERO,
     $                                  ZERO, TIMNG, NDATA( 2 ) )
   60                      CONTINUE
                           CALL DLACPY( 'Full', M, N, COPYA, LDA, A,
     $                                  LDA )
                           CALL DLACPY( 'Full', M, NRHS, COPYB, LDB, B,
     $                                  LDB )
                           SRNAMT = 'DGELSX'
                           NCALL = NCALL + 1
                           S1 = DSECND( )
                           CALL DGELSX( M, N, NRHS, A, LDA, B, LDB,
     $                                  IWORK, RCOND, CRANK, WORK,
     $                                  INFO )
                           S2 = DSECND( )
                           TIME = TIME + ( S2-S1 )
                           IF( INFO.EQ.0 .AND. TIME.LT.TIMMIN )
     $                        GO TO 60
                           TIMNG( 1 ) = TIME
                           OPCNT( 1 ) = DASUM( NDATA( 2 ), OPCNT, 1 )
                           CALL DSCAL( NDATA( 2 ), ONE / DBLE( NCALL ),
     $                                 OPCNT, 1 )
                           CALL DSCAL( NDATA( 2 ), ONE / DBLE( NCALL ),
     $                                 TIMNG, 1 )
                           CALL DCOPY( NDATA( 2 ), OPCNT, 1,
     $                                 OPCTBL( 1, ITYPE, NCLSX+1, 2 ),
     $                                 1 )
                           CALL DCOPY( NDATA( 2 ), TIMNG, 1,
     $                                 TIMTBL( 1, ITYPE, NCLSX+1, 2 ),
     $                                 1 )
                           DO 70 I = 1, NDATA( 2 )
                              FLPTBL( I, ITYPE, NCLSX+1,
     $                           2 ) = DMFLOP( OPCNT( I ), TIMNG( I ),
     $                           INFO )
   70                      CONTINUE
*
                        END IF
*
*                       Loop for timing different block sizes.
*
                        DO 140 INB = 1, NNB
                           NB = NBVAL( INB )
                           CALL XLAENV( 1, NB )
                           CALL XLAENV( 3, NXVAL( INB ) )
*
                           IF( TIMSUB( 3 ) ) THEN
*
*                          Time DGELSY
*
*                          DGELSY:  Compute the minimum-norm solution X
*                          to min( norm( A * X - B ) ) using the
*                          rank-revealing orthogonal factorization.
*
*                          Set LWLSY to the adequate value.
*
                              LWLSY = MAX( 1, MNMIN+2*N+NB*( N+1 ),
     $                                2*MNMIN+NB*NRHS )
*
                              NCALL = 0
                              TIME = ZERO
                              CALL DLASET( 'Full', NDATA( 3 ), 1, ZERO,
     $                                     ZERO, OPCNT, NDATA( 3 ) )
                              CALL DLASET( 'Full', NDATA( 3 ), 1, ZERO,
     $                                     ZERO, TIMNG, NDATA( 3 ) )
   80                         CONTINUE
                              CALL DLACPY( 'Full', M, N, COPYA, LDA, A,
     $                                     LDA )
                              CALL DLACPY( 'Full', M, NRHS, COPYB, LDB,
     $                                     B, LDB )
                              SRNAMT = 'DGELSY'
                              NCALL = NCALL + 1
                              S1 = DSECND( )
                              CALL DGELSY( M, N, NRHS, A, LDA, B, LDB,
     $                                     IWORK, RCOND, CRANK, WORK,
     $                                     LWLSY, INFO )
                              S2 = DSECND( )
                              TIME = TIME + ( S2-S1 )
                              IF( INFO.EQ.0 .AND. TIME.LT.TIMMIN )
     $                           GO TO 80
                              TIMNG( 1 ) = TIME
                              OPCNT( 1 ) = DASUM( NDATA( 3 ), OPCNT, 1 )
                              CALL DSCAL( NDATA( 3 ),
     $                                    ONE / DBLE( NCALL ), OPCNT,
     $                                    1 )
                              CALL DSCAL( NDATA( 3 ),
     $                                    ONE / DBLE( NCALL ), TIMNG,
     $                                    1 )
                              CALL DCOPY( NDATA( 3 ), OPCNT, 1,
     $                                    OPCTBL( 1, ITYPE, NCLSY+INB,
     $                                    3 ), 1 )
                              CALL DCOPY( NDATA( 3 ), TIMNG, 1,
     $                                    TIMTBL( 1, ITYPE, NCLSY+INB,
     $                                    3 ), 1 )
                              DO 90 I = 1, NDATA( 3 )
                                 FLPTBL( I, ITYPE, NCLSY+INB,
     $                              3 ) = DMFLOP( OPCNT( I ),
     $                              TIMNG( I ), INFO )
   90                         CONTINUE
*
                           END IF
*
                           IF( TIMSUB( 4 ) ) THEN
*
*                          Time DGELSS
*
*                          DGELSS:  Compute the minimum-norm solution X
*                          to min( norm( A * X - B ) ) using the SVD.
*
                              NCALL = 0
                              TIME = ZERO
                              CALL DLASET( 'Full', NDATA( 4 ), 1, ZERO,
     $                                     ZERO, OPCNT, NDATA( 4 ) )
                              CALL DLASET( 'Full', NDATA( 4 ), 1, ZERO,
     $                                     ZERO, TIMNG, NDATA( 4 ) )
  100                         CONTINUE
                              CALL DLACPY( 'Full', M, N, COPYA, LDA, A,
     $                                     LDA )
                              CALL DLACPY( 'Full', M, NRHS, COPYB, LDB,
     $                                     B, LDB )
                              SRNAMT = 'DGELSS'
                              NCALL = NCALL + 1
                              S1 = DSECND( )
                              CALL DGELSS( M, N, NRHS, A, LDA, B, LDB,
     $                                     S, RCOND, CRANK, WORK, LWORK,
     $                                     INFO )
                              S2 = DSECND( )
                              TIME = TIME + ( S2-S1 )
                              IF( INFO.EQ.0 .AND. TIME.LT.TIMMIN )
     $                           GO TO 100
                              TIMNG( 1 ) = TIME
                              OPCNT( 1 ) = DASUM( NDATA( 4 ), OPCNT, 1 )
                              CALL DSCAL( NDATA( 4 ),
     $                                    ONE / DBLE( NCALL ), OPCNT,
     $                                    1 )
                              CALL DSCAL( NDATA( 4 ),
     $                                    ONE / DBLE( NCALL ), TIMNG,
     $                                    1 )
                              CALL DCOPY( NDATA( 4 ), OPCNT, 1,
     $                                    OPCTBL( 1, ITYPE, NCLSS+INB,
     $                                    4 ), 1 )
                              CALL DCOPY( NDATA( 4 ), TIMNG, 1,
     $                                    TIMTBL( 1, ITYPE, NCLSS+INB,
     $                                    4 ), 1 )
                              DO 110 I = 1, NDATA( 4 )
                                 FLPTBL( I, ITYPE, NCLSS+INB,
     $                              4 ) = DMFLOP( OPCNT( I ),
     $                              TIMNG( I ), INFO )
  110                         CONTINUE
*
                           END IF
*
                           IF( TIMSUB( 5 ) ) THEN
*
*                          Time DGELSD
*
*                          DGELSD:  Compute the minimum-norm solution X
*                          to min( norm( A * X - B ) ) using a
*                          divide-and-conquer SVD.
*
                              NCALL = 0
                              TIME = ZERO
                              CALL DLASET( 'Full', NDATA( 5 ), 1, ZERO,
     $                                     ZERO, OPCNT, NDATA( 5 ) )
                              CALL DLASET( 'Full', NDATA( 5 ), 1, ZERO,
     $                                     ZERO, TIMNG, NDATA( 5 ) )
  120                         CONTINUE
                              CALL DLACPY( 'Full', M, N, COPYA, LDA, A,
     $                                     LDA )
                              CALL DLACPY( 'Full', M, NRHS, COPYB, LDB,
     $                                     B, LDB )
                              SRNAMT = 'DGELSD'
                              NCALL = NCALL + 1
                              S1 = DSECND( )
                              CALL DGELSD( M, N, NRHS, A, LDA, B, LDB,
     $                                     S, RCOND, CRANK, WORK, LWORK,
     $                                     IWORK, INFO )
                              S2 = DSECND( )
                              TIME = TIME + ( S2-S1 )
                              IF( INFO.EQ.0 .AND. TIME.LT.TIMMIN )
     $                           GO TO 120
                              TIMNG( 1 ) = TIME
                              OPCNT( 1 ) = DASUM( NDATA( 5 ), OPCNT, 1 )
                              CALL DSCAL( NDATA( 5 ),
     $                                    ONE / DBLE( NCALL ), OPCNT,
     $                                    1 )
                              CALL DSCAL( NDATA( 5 ),
     $                                    ONE / DBLE( NCALL ), TIMNG,
     $                                    1 )
                              CALL DCOPY( NDATA( 5 ), OPCNT, 1,
     $                                    OPCTBL( 1, ITYPE, NCLSD+INB,
     $                                    5 ), 1 )
                              CALL DCOPY( NDATA( 5 ), TIMNG, 1,
     $                                    TIMTBL( 1, ITYPE, NCLSD+INB,
     $                                    5 ), 1 )
                              DO 130 I = 1, NDATA( 5 )
                                 FLPTBL( I, ITYPE, NCLSD+INB,
     $                              5 ) = DMFLOP( OPCNT( I ),
     $                              TIMNG( I ), INFO )
  130                         CONTINUE
*
                           END IF
*
  140                   CONTINUE
  150                CONTINUE
  160             CONTINUE
                  NCLS = NCLS + NNB
                  NCLSY = NCLSY + NNB
                  NCLSS = NCLSS + NNB
                  NCLSD = NCLSD + NNB
  170          CONTINUE
               NCLSX = NCLSX + 1
  180       CONTINUE
  190    CONTINUE
  200 CONTINUE
*
*     Print a summary of the results.
*
      DO 220 ISUB = 1, NSUBS
         IF( TIMSUB( ISUB ) ) THEN
            WRITE( NOUT, FMT = 9999 )SUBNAM( ISUB )
            IF( ISUB.EQ.1 ) THEN
               WRITE( NOUT, FMT = 9998 )
            ELSE IF( ISUB.EQ.2 ) THEN
               WRITE( NOUT, FMT = 9997 )
            ELSE IF( ISUB.EQ.3 ) THEN
               WRITE( NOUT, FMT = 9996 )
            ELSE IF( ISUB.EQ.4 ) THEN
               WRITE( NOUT, FMT = 9995 )
            ELSE IF( ISUB.EQ.5 ) THEN
               WRITE( NOUT, FMT = 9994 )
            END IF
            DO 210 ITBL = 1, 3
               IF( ITBL.EQ.1 ) THEN
                  WRITE( NOUT, FMT = 9993 )
                  CALL DPRTLS( ISUB, SUBNAM( ISUB ), NDATA( ISUB ), NM,
     $                         MVAL, NN, NVAL, NNS, NSVAL, NNB, NBVAL,
     $                         NXVAL, NLDA, LDAVAL, MTYPE,
     $                         TIMTBL( 1, 1, 1, ISUB ), NOUT )
               ELSE IF( ITBL.EQ.2 ) THEN
                  WRITE( NOUT, FMT = 9992 )
                  CALL DPRTLS( ISUB, SUBNAM( ISUB ), NDATA( ISUB ), NM,
     $                         MVAL, NN, NVAL, NNS, NSVAL, NNB, NBVAL,
     $                         NXVAL, NLDA, LDAVAL, MTYPE,
     $                         OPCTBL( 1, 1, 1, ISUB ), NOUT )
               ELSE IF( ITBL.EQ.3 ) THEN
                  WRITE( NOUT, FMT = 9991 )
                  CALL DPRTLS( ISUB, SUBNAM( ISUB ), NDATA( ISUB ), NM,
     $                         MVAL, NN, NVAL, NNS, NSVAL, NNB, NBVAL,
     $                         NXVAL, NLDA, LDAVAL, MTYPE,
     $                         FLPTBL( 1, 1, 1, ISUB ), NOUT )
               END IF
  210       CONTINUE
         END IF
  220 CONTINUE
*
  230 CONTINUE
 9999 FORMAT( / / / ' ****** Results for ', A, ' ******' )
 9998 FORMAT( / ' DGELS   : overall performance',
     $      / ' comp. 1 : if M>=N, DGEQRF, QR factorization',
     $      / '           if M< N, DGELQF, QR factorization',
     $      / ' comp. 2 : if M>=N, DORMQR, multiplication by',
     $      ' reflectors', /
     $      '           if M< N, DORMLQ, multiplication by',
     $      ' reflectors', /
     $      ' comp. 3 : DTRSM, solution of the triangular', ' system',
     $      / / ' Types 4 to 6 are the transpose', ' of types 1 to 3' )
 9997 FORMAT( / ' DGELSX  : overall performance',
     $      / ' comp. 1 : DGEQPF, QR factorization with column',
     $      ' pivoting', / ' comp. 2 : if RANK<N, DTZRQF, reduction to',
     $      ' triangular form', /
     $      ' comp. 3 : DORM2R, multiplication by reflectors',
     $      / ' comp. 4 : DTRSM, solution of the triangular', ' system',
     $      / ' comp. 5 : if RANK<N, DLATZM, multiplication by',
     $      ' reflectors' )
 9996 FORMAT( / ' DGELSY  : overall performance',
     $      / ' comp. 1 : DGEQP3, QR factorization with column',
     $      ' pivoting', / ' comp. 2 : if RANK<N, DTZRZF, reduction to',
     $      ' triangular form', /
     $      ' comp. 3 : DORMQR, multiplication by reflectors',
     $      / ' comp. 4 : DTRSM, solution of the triangular', ' system',
     $      / ' comp. 5 : if RANK<N, DORMRZ, multiplication by',
     $      ' reflectors' )
 9995 FORMAT( / ' DGELSS: overall performance',
     $      / ' comp. 1 : if M>>N, DGEQRF, QR factorization',
     $      / '                    DORMQR, multiplication by',
     $      ' reflectors', /
     $      '           if N>>M, DGELQF, QL factorization',
     $      / ' comp. 2 : DGEBRD, reduction to bidiagonal form',
     $      / ' comp. 3 : DORMBR, multiplication by left',
     $      ' bidiagonalizing vectors', /
     $      '           DORGBR, generation of right',
     $      ' bidiagonalizing vectors', /
     $      ' comp. 4 : DBDSQR, singular value decomposition',
     $      ' of the bidiagonal matrix',
     $      / ' comp. 5 : multiplication by right bidiagonalizing',
     $      ' vectors', /
     $      '           (DGEMM or SGEMV, and DORMLQ if N>>M)' )
 9994 FORMAT( / ' DGELSD: overall performance',
     $      / ' comp. 1 : if M>>N, DGEQRF, QR factorization',
     $      / '                    DORMQR, multiplication by',
     $      ' reflectors', /
     $      '           if N>>M, DGELQF, QL factorization',
     $      / ' comp. 2 : DGEBRD, reduction to bidiagonal form',
     $      / ' comp. 3 : DORMBR, multiplication by left ',
     $      ' bidiagonalizing vectors', /
     $      '                   multiplication by right',
     $      ' bidiagonalizing vectors', /
     $      ' comp. 4 : DLALSD, singular value decomposition',
     $      ' of the bidiagonal matrix' )
 9993 FORMAT( / / ' *** Time in seconds *** ' )
 9992 FORMAT( / / ' *** Number of floating-point operations *** ' )
 9991 FORMAT( / / ' *** Speed in megaflops *** ' )
      RETURN
*
*     End of DTIMLS
*
      END
      SUBROUTINE DTIMMG( IFLAG, M, N, A, LDA, KL, KU )
*
*  -- LAPACK timing routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     September 30, 1994
*
*     .. Scalar Arguments ..
      INTEGER            IFLAG, KL, KU, LDA, M, N
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * )
*     ..
*
*  Purpose
*  =======
*
*  DTIMMG generates a real test matrix whose type is given by IFLAG.
*  All the matrices are Toeplitz (constant along a diagonal), with
*  random elements on each diagonal.
*
*  Arguments
*  =========
*
*  IFLAG   (input) INTEGER
*          The type of matrix to be generated.
*          = 0 or 1:   General matrix
*          = 2 or -2:  General banded matrix
*          = 3 or -3:  Symmetric positive definite matrix
*          = 4 or -4:  Symmetric positive definite packed
*          = 5 or -5:  Symmetric positive definite banded
*          = 6 or -6:  Symmetric indefinite matrix
*          = 7 or -7:  Symmetric indefinite packed
*          = 8 or -8:  Symmetric indefinite banded
*          = 9 or -9:  Triangular
*          = 10 or -10:  Triangular packed
*          = 11 or -11:  Triangular banded
*          = 12:         General tridiagonal
*          = 13 or -13:  Positive definite tridiagonal
*          For symmetric or triangular matrices, IFLAG > 0 indicates
*          upper triangular storage and IFLAG < 0 indicates lower
*          triangular storage.
*
*  M       (input) INTEGER
*          The number of rows of the matrix to be generated.
*
*  N       (input) INTEGER
*          The number of columns of the matrix to be generated.
*
*  A       (output) DOUBLE PRECISION array, dimension (LDA,N)
*          The generated matrix.
*
*          If the absolute value of IFLAG is 1, 3, or 6, the leading
*          M x N (or N x N) subblock is used to store the matrix.
*          If the matrix is symmetric, only the upper or lower triangle
*          of this block is referenced.
*
*          If the absolute value of IFLAG is 4 or 7, the matrix is
*          symmetric and packed storage is used for the upper or lower
*          triangle.  The triangular matrix is stored columnwise as a
*          inear array, and the array A is treated as a vector of
*          length LDA.  LDA must be set to at least N*(N+1)/2.
*
*          If the absolute value of IFLAG is 2 or 5, the matrix is
*          returned in band format.  The columns of the matrix are
*          specified in the columns of A and the diagonals of the
*          matrix are specified in the rows of A, with the leading
*          diagonal in row
*              KL + KU + 1,  if IFLAG = 2
*              KU + 1,       if IFLAG = 5 or -2
*              1,            if IFLAG = -5
*          If IFLAG = 2, the first KL rows are not used to leave room
*          for pivoting in DGBTRF.
*
*  LDA     (input) INTEGER
*          The leading dimension of A.  If the generated matrix is
*          packed, LDA >= N*(N+1)/2, otherwise LDA >= max(1,M).
*
*  KL      (input) INTEGER
*          The number of subdiagonals if IFLAG = 2, 5, or -5.
*
*  KU      (input) INTEGER
*          The number of superdiagonals if IFLAG = 2, 5, or -5.
*
*  =====================================================================
*
*     .. Local Scalars ..
      INTEGER            I, J, JJ, JN, K, MJ, MU
*     ..
*     .. Local Arrays ..
      INTEGER            ISEED( 4 )
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          DBLE, MIN, SIGN
*     ..
*     .. External Subroutines ..
      EXTERNAL           DCOPY, DLARNV
*     ..
*     .. Data statements ..
      DATA               ISEED / 0, 0, 0, 1 /
*     ..
*     .. Executable Statements ..
*
      IF( M.LE.0 .OR. N.LE.0 ) THEN
         RETURN
*
      ELSE IF( IFLAG.EQ.0 .OR. IFLAG.EQ.1 ) THEN
*
*        General matrix
*
*        Set first column and row to random values.
*
         CALL DLARNV( 2, ISEED, M, A( 1, 1 ) )
         DO 10 J = 2, N, M
            MJ = MIN( M, N-J+1 )
            CALL DLARNV( 2, ISEED, MJ, A( 1, J ) )
            IF( MJ.GT.1 )
     $         CALL DCOPY( MJ-1, A( 2, J ), 1, A( 1, J+1 ), LDA )
   10    CONTINUE
*
*        Fill in the rest of the matrix.
*
         DO 30 J = 2, N
            DO 20 I = 2, M
               A( I, J ) = A( I-1, J-1 )
   20       CONTINUE
   30    CONTINUE
*
      ELSE IF( IFLAG.EQ.2 .OR. IFLAG.EQ.-2 ) THEN
*
*        General band matrix
*
         IF( IFLAG.EQ.2 ) THEN
            K = KL + KU + 1
         ELSE
            K = KU + 1
         END IF
         CALL DLARNV( 2, ISEED, MIN( M, KL+1 ), A( K, 1 ) )
         MU = MIN( N-1, KU )
         CALL DLARNV( 2, ISEED, MU+1, A( K-MU, N ) )
         DO 40 J = 2, N - 1
            MU = MIN( J-1, KU )
            CALL DCOPY( MU, A( K-MU, N ), 1, A( K-MU, J ), 1 )
            CALL DCOPY( MIN( M-J+1, KL+1 ), A( K, 1 ), 1, A( K, J ), 1 )
   40    CONTINUE
*
      ELSE IF( IFLAG.EQ.3 ) THEN
*
*        Symmetric positive definite, upper triangle
*
         CALL DLARNV( 2, ISEED, N-1, A( 1, N ) )
         A( N, N ) = DBLE( N )
         DO 50 J = N - 1, 1, -1
            CALL DCOPY( J, A( N-J+1, N ), 1, A( 1, J ), 1 )
   50    CONTINUE
*
      ELSE IF( IFLAG.EQ.-3 ) THEN
*
*        Symmetric positive definite, lower triangle
*
         A( 1, 1 ) = DBLE( N )
         IF( N.GT.1 )
     $      CALL DLARNV( 2, ISEED, N-1, A( 2, 1 ) )
         DO 60 J = 2, N
            CALL DCOPY( N-J+1, A( 1, 1 ), 1, A( J, J ), 1 )
   60    CONTINUE
*
      ELSE IF( IFLAG.EQ.4 ) THEN
*
*        Symmetric positive definite packed, upper triangle
*
         JN = ( N-1 )*N / 2 + 1
         CALL DLARNV( 2, ISEED, N-1, A( JN, 1 ) )
         A( JN+N-1, 1 ) = DBLE( N )
         JJ = JN
         DO 70 J = N - 1, 1, -1
            JJ = JJ - J
            JN = JN + 1
            CALL DCOPY( J, A( JN, 1 ), 1, A( JJ, 1 ), 1 )
   70    CONTINUE
*
      ELSE IF( IFLAG.EQ.-4 ) THEN
*
*        Symmetric positive definite packed, lower triangle
*
         A( 1, 1 ) = DBLE( N )
         IF( N.GT.1 )
     $      CALL DLARNV( 2, ISEED, N-1, A( 2, 1 ) )
         JJ = N + 1
         DO 80 J = 2, N
            CALL DCOPY( N-J+1, A( 1, 1 ), 1, A( JJ, 1 ), 1 )
            JJ = JJ + N - J + 1
   80    CONTINUE
*
      ELSE IF( IFLAG.EQ.5 ) THEN
*
*        Symmetric positive definite banded, upper triangle
*
         K = KL
         MU = MIN( N-1, K )
         CALL DLARNV( 2, ISEED, MU, A( K+1-MU, N ) )
         A( K+1, N ) = DBLE( N )
         DO 90 J = N - 1, 1, -1
            MU = MIN( J, K+1 )
            CALL DCOPY( MU, A( K+2-MU, N ), 1, A( K+2-MU, J ), 1 )
   90    CONTINUE
*
      ELSE IF( IFLAG.EQ.-5 ) THEN
*
*        Symmetric positive definite banded, lower triangle
*
         K = KL
         A( 1, 1 ) = DBLE( N )
         CALL DLARNV( 2, ISEED, MIN( N-1, K ), A( 2, 1 ) )
         DO 100 J = 2, N
            CALL DCOPY( MIN( N-J+1, K+1 ), A( 1, 1 ), 1, A( 1, J ), 1 )
  100    CONTINUE
*
      ELSE IF( IFLAG.EQ.6 ) THEN
*
*        Symmetric indefinite, upper triangle
*
         CALL DLARNV( 2, ISEED, N, A( 1, N ) )
         DO 110 J = N - 1, 1, -1
            CALL DCOPY( J, A( N-J+1, N ), 1, A( 1, J ), 1 )
  110    CONTINUE
*
      ELSE IF( IFLAG.EQ.-6 ) THEN
*
*        Symmetric indefinite, lower triangle
*
         CALL DLARNV( 2, ISEED, N, A( 1, 1 ) )
         DO 120 J = 2, N
            CALL DCOPY( N-J+1, A( 1, 1 ), 1, A( J, J ), 1 )
  120    CONTINUE
*
      ELSE IF( IFLAG.EQ.7 ) THEN
*
*        Symmetric indefinite packed, upper triangle
*
         JN = ( N-1 )*N / 2 + 1
         CALL DLARNV( 2, ISEED, N, A( JN, 1 ) )
         JJ = JN
         DO 130 J = N - 1, 1, -1
            JJ = JJ - J
            JN = JN + 1
            CALL DCOPY( J, A( JN, 1 ), 1, A( JJ, 1 ), 1 )
  130    CONTINUE
*
      ELSE IF( IFLAG.EQ.-7 ) THEN
*
*        Symmetric indefinite packed, lower triangle
*
         CALL DLARNV( 2, ISEED, N, A( 1, 1 ) )
         JJ = N + 1
         DO 140 J = 2, N
            CALL DCOPY( N-J+1, A( 1, 1 ), 1, A( JJ, 1 ), 1 )
            JJ = JJ + N - J + 1
  140    CONTINUE
*
      ELSE IF( IFLAG.EQ.8 ) THEN
*
*        Symmetric indefinite banded, upper triangle
*
         K = KL
         MU = MIN( N, K+1 )
         CALL DLARNV( 2, ISEED, MU, A( K+2-MU, N ) )
         DO 150 J = N - 1, 1, -1
            MU = MIN( J, K+1 )
            CALL DCOPY( MU, A( K+2-MU, N ), 1, A( K+2-MU, J ), 1 )
  150    CONTINUE
*
      ELSE IF( IFLAG.EQ.-8 ) THEN
*
*        Symmetric indefinite banded, lower triangle
*
         K = KL
         CALL DLARNV( 2, ISEED, MIN( N, K+1 ), A( 1, 1 ) )
         DO 160 J = 2, N
            CALL DCOPY( MIN( N-J+1, K+1 ), A( 1, 1 ), 1, A( 1, J ), 1 )
  160    CONTINUE
*
      ELSE IF( IFLAG.EQ.9 ) THEN
*
*        Upper triangular
*
         CALL DLARNV( 2, ISEED, N, A( 1, N ) )
         A( N, N ) = SIGN( DBLE( N ), A( N, N ) )
         DO 170 J = N - 1, 1, -1
            CALL DCOPY( J, A( N-J+1, N ), 1, A( 1, J ), 1 )
  170    CONTINUE
*
      ELSE IF( IFLAG.EQ.-9 ) THEN
*
*        Lower triangular
*
         CALL DLARNV( 2, ISEED, N, A( 1, 1 ) )
         A( 1, 1 ) = SIGN( DBLE( N ), A( 1, 1 ) )
         DO 180 J = 2, N
            CALL DCOPY( N-J+1, A( 1, 1 ), 1, A( J, J ), 1 )
  180    CONTINUE
*
      ELSE IF( IFLAG.EQ.10 ) THEN
*
*        Upper triangular packed
*
         JN = ( N-1 )*N / 2 + 1
         CALL DLARNV( 2, ISEED, N, A( JN, 1 ) )
         A( JN+N-1, 1 ) = SIGN( DBLE( N ), A( JN+N-1, 1 ) )
         JJ = JN
         DO 190 J = N - 1, 1, -1
            JJ = JJ - J
            JN = JN + 1
            CALL DCOPY( J, A( JN, 1 ), 1, A( JJ, 1 ), 1 )
  190    CONTINUE
*
      ELSE IF( IFLAG.EQ.-10 ) THEN
*
*        Lower triangular packed
*
         CALL DLARNV( 2, ISEED, N, A( 1, 1 ) )
         A( 1, 1 ) = SIGN( DBLE( N ), A( 1, 1 ) )
         JJ = N + 1
         DO 200 J = 2, N
            CALL DCOPY( N-J+1, A( 1, 1 ), 1, A( JJ, 1 ), 1 )
            JJ = JJ + N - J + 1
  200    CONTINUE
*
      ELSE IF( IFLAG.EQ.11 ) THEN
*
*        Upper triangular banded
*
         K = KL
         MU = MIN( N, K+1 )
         CALL DLARNV( 2, ISEED, MU, A( K+2-MU, N ) )
         A( K+1, N ) = SIGN( DBLE( K+1 ), A( K+1, N ) )
         DO 210 J = N - 1, 1, -1
            MU = MIN( J, K+1 )
            CALL DCOPY( MU, A( K+2-MU, N ), 1, A( K+2-MU, J ), 1 )
  210    CONTINUE
*
      ELSE IF( IFLAG.EQ.-11 ) THEN
*
*        Lower triangular banded
*
         K = KL
         CALL DLARNV( 2, ISEED, MIN( N, K+1 ), A( 1, 1 ) )
         A( 1, 1 ) = SIGN( DBLE( K+1 ), A( 1, 1 ) )
         DO 220 J = 2, N
            CALL DCOPY( MIN( N-J+1, K+1 ), A( 1, 1 ), 1, A( 1, J ), 1 )
  220    CONTINUE
*
      ELSE IF( IFLAG.EQ.12 ) THEN
*
*        General tridiagonal
*
         CALL DLARNV( 2, ISEED, 3*N-2, A )
*
      ELSE IF( IFLAG.EQ.13 .OR. IFLAG.EQ.-13 ) THEN
*
*        Positive definite tridiagonal
*
         DO 230 J = 1, N
            A( J, 1 ) = 2.0D0
  230    CONTINUE
         CALL DLARNV( 2, ISEED, N-1, A( N+1, 1 ) )
      END IF
*
      RETURN
*
*     End of DTIMMG
*
      END
      SUBROUTINE DTIMMM( VNAME, LAB2, NN, NVAL, NLDA, LDAVAL, TIMMIN, A,
     $                   B, C, RESLTS, LDR1, LDR2, NOUT )
*
*  -- LAPACK timing routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     March 31, 1993
*
*     .. Scalar Arguments ..
      CHARACTER*( * )    LAB2, VNAME
      INTEGER            LDR1, LDR2, NLDA, NN, NOUT
      DOUBLE PRECISION   TIMMIN
*     ..
*     .. Array Arguments ..
      INTEGER            LDAVAL( * ), NVAL( * )
      DOUBLE PRECISION   A( * ), B( * ), C( * ), RESLTS( LDR1, LDR2, * )
*     ..
*
*  Purpose
*  =======
*
*  DTIMMM times DGEMM.
*
*  Arguments
*  =========
*
*  VNAME   (input) CHARACTER*(*)
*          The name of the Level 3 BLAS routine to be timed.
*
*  LAB2    (input) CHARACTER*(*)
*          The name of the variable given in NVAL.
*
*  NN      (input) INTEGER
*          The number of values of N contained in the vector NVAL.
*
*  NVAL    (input) INTEGER array, dimension (NN)
*          The values of the matrix dimension N.
*
*  NLDA    (input) INTEGER
*          The number of values of LDA contained in the vector LDAVAL.
*
*  LDAVAL  (input) INTEGER array, dimension (NLDA)
*          The values of the leading dimension of the array A.
*
*  TIMMIN  (input) DOUBLE PRECISION
*          The minimum time a subroutine will be timed.
*
*  A       (workspace) DOUBLE PRECISION array, dimension (LDAMAX*NMAX)
*             where LDAMAX and NMAX are the maximum values permitted
*             for LDA and N.
*
*  B       (workspace) DOUBLE PRECISION array, dimension (LDAMAX*NMAX)
*
*  C       (workspace) DOUBLE PRECISION array, dimension (LDAMAX*NMAX)
*
*  RESLTS  (output) DOUBLE PRECISION array, dimension (LDR1,LDR2,NLDA)
*          The timing results for each subroutine over the relevant
*          values of N and LDA.
*
*  LDR1    (input) INTEGER
*          The first dimension of RESLTS.  LDR1 >= 1.
*
*  LDR2    (input) INTEGER
*          The second dimension of RESLTS.  LDR2 >= max(1,NN).
*
*  NOUT    (input) INTEGER
*          The unit number for output.
*
*  =====================================================================
*
*     .. Parameters ..
      INTEGER            NSUBS
      DOUBLE PRECISION   ONE
      PARAMETER          ( NSUBS = 1, ONE = 1.0D+0 )
*     ..
*     .. Local Scalars ..
      CHARACTER*6        CNAME
      INTEGER            I, IC, ICL, ILDA, IN, INFO, ISUB, LDA, N
      DOUBLE PRECISION   OPS, S1, S2, TIME, UNTIME
*     ..
*     .. Local Arrays ..
      LOGICAL            TIMSUB( NSUBS )
      CHARACTER*6        SUBNAM( NSUBS )
      INTEGER            IDUMMY( 1 )
*     ..
*     .. External Functions ..
      LOGICAL            LSAMEN
      DOUBLE PRECISION   DMFLOP, DOPBL3, DSECND
      EXTERNAL           LSAMEN, DMFLOP, DOPBL3, DSECND
*     ..
*     .. External Subroutines ..
      EXTERNAL           ATIMCK, DGEMM, DPRTBL, DTIMMG
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          DBLE
*     ..
*     .. Data statements ..
      DATA               SUBNAM / 'DGEMM ' /
*     ..
*     .. Executable Statements ..
*
      CNAME = VNAME
      DO 10 ISUB = 1, NSUBS
         TIMSUB( ISUB ) = LSAMEN( 6, CNAME, SUBNAM( ISUB ) )
         IF( TIMSUB( ISUB ) )
     $      GO TO 20
   10 CONTINUE
      WRITE( NOUT, FMT = 9999 )CNAME
      GO TO 80
   20 CONTINUE
*
*     Check that N <= LDA for the input values.
*
      CALL ATIMCK( 2, CNAME, NN, NVAL, NLDA, LDAVAL, NOUT, INFO )
      IF( INFO.GT.0 ) THEN
         WRITE( NOUT, FMT = 9998 )CNAME
         GO TO 80
      END IF
*
      DO 60 ILDA = 1, NLDA
         LDA = LDAVAL( ILDA )
         DO 50 IN = 1, NN
            N = NVAL( IN )
*
*           Time DGEMM
*
            CALL DTIMMG( 1, N, N, A, LDA, 0, 0 )
            CALL DTIMMG( 0, N, N, B, LDA, 0, 0 )
            CALL DTIMMG( 1, N, N, C, LDA, 0, 0 )
            IC = 0
            S1 = DSECND( )
   30       CONTINUE
            CALL DGEMM( 'No transpose', 'No transpose', N, N, N, ONE, A,
     $                  LDA, B, LDA, ONE, C, LDA )
            S2 = DSECND( )
            TIME = S2 - S1
            IC = IC + 1
            IF( TIME.LT.TIMMIN ) THEN
               CALL DTIMMG( 1, N, N, C, LDA, 0, 0 )
               GO TO 30
            END IF
*
*           Subtract the time used in DTIMMG.
*
            ICL = 1
            S1 = DSECND( )
   40       CONTINUE
            S2 = DSECND( )
            UNTIME = S2 - S1
            ICL = ICL + 1
            IF( ICL.LE.IC ) THEN
               CALL DTIMMG( 1, N, N, C, LDA, 0, 0 )
               GO TO 40
            END IF
*
            TIME = ( TIME-UNTIME ) / DBLE( IC )
            OPS = DOPBL3( 'DGEMM ', N, N, N )
            RESLTS( 1, IN, ILDA ) = DMFLOP( OPS, TIME, 0 )
   50    CONTINUE
   60 CONTINUE
*
*     Print the table of results on unit NOUT.
*
      WRITE( NOUT, FMT = 9997 )VNAME
      IF( NLDA.EQ.1 ) THEN
         WRITE( NOUT, FMT = 9996 )LDAVAL( 1 )
      ELSE
         DO 70 I = 1, NLDA
            WRITE( NOUT, FMT = 9995 )I, LDAVAL( I )
   70    CONTINUE
      END IF
      WRITE( NOUT, FMT = * )
      CALL DPRTBL( ' ', LAB2, 1, IDUMMY, NN, NVAL, NLDA, RESLTS, LDR1,
     $             LDR2, NOUT )
*
   80 CONTINUE
      RETURN
 9999 FORMAT( 1X, A6, ':  Unrecognized path or subroutine name', / )
 9998 FORMAT( 1X, A6, ' timing run not attempted', / )
 9997 FORMAT( / ' *** Speed of ', A6, ' in megaflops ***' )
 9996 FORMAT( 5X, 'with LDA = ', I5 )
 9995 FORMAT( 5X, 'line ', I2, ' with LDA = ', I5 )
*
*     End of DTIMMM
*
      END
      SUBROUTINE DTIMMV( VNAME, NN, NVAL, NK, KVAL, NLDA, LDAVAL,
     $                   TIMMIN, A, LB, B, C, RESLTS, LDR1, LDR2, NOUT )
*
*  -- LAPACK timing routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     March 31, 1993
*
*     .. Scalar Arguments ..
      CHARACTER*( * )    VNAME
      INTEGER            LB, LDR1, LDR2, NK, NLDA, NN, NOUT
      DOUBLE PRECISION   TIMMIN
*     ..
*     .. Array Arguments ..
      INTEGER            KVAL( * ), LDAVAL( * ), NVAL( * )
      DOUBLE PRECISION   A( * ), B( * ), C( * ), RESLTS( LDR1, LDR2, * )
*     ..
*
*  Purpose
*  =======
*
*  DTIMMV times individual BLAS 2 routines.
*
*  Arguments
*  =========
*
*  VNAME   (input) CHARACTER*(*)
*          The name of the Level 2 BLAS routine to be timed.
*
*  NN      (input) INTEGER
*          The number of values of N contained in the vector NVAL.
*
*  NVAL    (input) INTEGER array, dimension (NN)
*          The values of the matrix dimension N.
*
*  NK      (input) INTEGER
*          The number of values of K contained in the vector KVAL.
*
*  KVAL    (input) INTEGER array, dimension (NK)
*          The values of the bandwidth K.
*
*  NLDA    (input) INTEGER
*          The number of values of LDA contained in the vector LDAVAL.
*
*  LDAVAL  (input) INTEGER array, dimension (NLDA)
*          The values of the leading dimension of the array A.
*
*  TIMMIN  (input) DOUBLE PRECISION
*          The minimum time a subroutine will be timed.
*
*  A       (workspace) DOUBLE PRECISION array, dimension (LDAMAX*NMAX)
*             where LDAMAX and NMAX are the maximum values permitted
*             for LDA and N.
*
*  LB      (input) INTEGER
*          The length of B and C, needed when timing DGBMV.  If timing
*          DGEMV, LB >= LDAMAX*NMAX.
*
*  B       (workspace) DOUBLE PRECISION array, dimension (LB)
*
*  C       (workspace) DOUBLE PRECISION array, dimension (LB)
*
*  RESLTS  (output) DOUBLE PRECISION array, dimension (LDR1,LDR2,NLDA)
*          The timing results for each subroutine over the relevant
*          values of N and LDA.
*
*  LDR1    (input) INTEGER
*          The first dimension of RESLTS.  LDR1 >= max(1,NK).
*
*  LDR2    (input) INTEGER
*          The second dimension of RESLTS.  LDR2 >= max(1,NN).
*
*  NOUT    (input) INTEGER
*          The unit number for output.
*
*  =====================================================================
*
*     .. Parameters ..
      INTEGER            NSUBS
      DOUBLE PRECISION   ONE
      PARAMETER          ( NSUBS = 2, ONE = 1.0D+0 )
*     ..
*     .. Local Scalars ..
      CHARACTER          LAB1, LAB2
      CHARACTER*6        CNAME
      INTEGER            I, IB, IC, ICL, IK, ILDA, IN, INFO, ISUB, K,
     $                   KL, KU, LDA, LDB, N, NRHS
      DOUBLE PRECISION   OPS, S1, S2, TIME, UNTIME
*     ..
*     .. Local Arrays ..
      LOGICAL            TIMSUB( NSUBS )
      CHARACTER*6        SUBNAM( NSUBS )
*     ..
*     .. External Functions ..
      LOGICAL            LSAME, LSAMEN
      DOUBLE PRECISION   DMFLOP, DOPBL2, DSECND
      EXTERNAL           LSAME, LSAMEN, DMFLOP, DOPBL2, DSECND
*     ..
*     .. External Subroutines ..
      EXTERNAL           ATIMCK, DGBMV, DGEMV, DPRTBL, DTIMMG
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          DBLE, MAX, MIN
*     ..
*     .. Data statements ..
      DATA               SUBNAM / 'DGEMV ', 'DGBMV ' /
*     ..
*     .. Executable Statements ..
*
      CNAME = VNAME
      DO 10 ISUB = 1, NSUBS
         TIMSUB( ISUB ) = LSAMEN( 6, CNAME, SUBNAM( ISUB ) )
         IF( TIMSUB( ISUB ) )
     $      GO TO 20
   10 CONTINUE
      WRITE( NOUT, FMT = 9999 )CNAME
      GO TO 150
   20 CONTINUE
*
*     Check that N or K <= LDA for the input values.
*
      IF( LSAME( CNAME( 3: 3 ), 'B' ) ) THEN
         CALL ATIMCK( 0, CNAME, NK, KVAL, NLDA, LDAVAL, NOUT, INFO )
         LAB1 = 'M'
         LAB2 = 'K'
      ELSE
         CALL ATIMCK( 2, CNAME, NN, NVAL, NLDA, LDAVAL, NOUT, INFO )
         LAB1 = ' '
         LAB2 = 'N'
      END IF
      IF( INFO.GT.0 ) THEN
         WRITE( NOUT, FMT = 9998 )CNAME
         GO TO 150
      END IF
*
*     Print the table header on unit NOUT.
*
      WRITE( NOUT, FMT = 9997 )VNAME
      IF( NLDA.EQ.1 ) THEN
         WRITE( NOUT, FMT = 9996 )LDAVAL( 1 )
      ELSE
         DO 30 I = 1, NLDA
            WRITE( NOUT, FMT = 9995 )I, LDAVAL( I )
   30    CONTINUE
      END IF
      WRITE( NOUT, FMT = * )
*
*     Time DGEMV
*
      IF( TIMSUB( 1 ) ) THEN
         DO 80 ILDA = 1, NLDA
            LDA = LDAVAL( ILDA )
            DO 70 IN = 1, NN
               N = NVAL( IN )
               NRHS = N
               LDB = LDA
               CALL DTIMMG( 1, N, N, A, LDA, 0, 0 )
               CALL DTIMMG( 0, N, NRHS, B, LDB, 0, 0 )
               CALL DTIMMG( 1, N, NRHS, C, LDB, 0, 0 )
               IC = 0
               S1 = DSECND( )
   40          CONTINUE
               IB = 1
               DO 50 I = 1, NRHS
                  CALL DGEMV( 'No transpose', N, N, ONE, A, LDA,
     $                        B( IB ), 1, ONE, C( IB ), 1 )
                  IB = IB + LDB
   50          CONTINUE
               S2 = DSECND( )
               TIME = S2 - S1
               IC = IC + 1
               IF( TIME.LT.TIMMIN ) THEN
                  CALL DTIMMG( 1, N, NRHS, C, LDB, 0, 0 )
                  GO TO 40
               END IF
*
*              Subtract the time used in DTIMMG.
*
               ICL = 1
               S1 = DSECND( )
   60          CONTINUE
               S2 = DSECND( )
               UNTIME = S2 - S1
               ICL = ICL + 1
               IF( ICL.LE.IC ) THEN
                  CALL DTIMMG( 1, N, NRHS, C, LDB, 0, 0 )
                  GO TO 60
               END IF
*
               TIME = ( TIME-UNTIME ) / DBLE( IC )
               OPS = NRHS*DOPBL2( 'DGEMV ', N, N, 0, 0 )
               RESLTS( 1, IN, ILDA ) = DMFLOP( OPS, TIME, 0 )
   70       CONTINUE
   80    CONTINUE
*
         CALL DPRTBL( LAB1, LAB2, 1, NVAL, NN, NVAL, NLDA, RESLTS, LDR1,
     $                LDR2, NOUT )
*
      ELSE IF( TIMSUB( 2 ) ) THEN
*
*        Time DGBMV
*
         DO 140 ILDA = 1, NLDA
            LDA = LDAVAL( ILDA )
            DO 130 IN = 1, NN
               N = NVAL( IN )
               DO 120 IK = 1, NK
                  K = MIN( N-1, MAX( 0, KVAL( IK ) ) )
                  KL = K
                  KU = K
                  LDB = N
                  CALL DTIMMG( 2, N, N, A, LDA, KL, KU )
                  NRHS = MIN( K, LB / LDB )
                  CALL DTIMMG( 0, N, NRHS, B, LDB, 0, 0 )
                  CALL DTIMMG( 1, N, NRHS, C, LDB, 0, 0 )
                  IC = 0
                  S1 = DSECND( )
   90             CONTINUE
                  IB = 1
                  DO 100 I = 1, NRHS
                     CALL DGBMV( 'No transpose', N, N, KL, KU, ONE,
     $                           A( KU+1 ), LDA, B( IB ), 1, ONE,
     $                           C( IB ), 1 )
                     IB = IB + LDB
  100             CONTINUE
                  S2 = DSECND( )
                  TIME = S2 - S1
                  IC = IC + 1
                  IF( TIME.LT.TIMMIN ) THEN
                     CALL DTIMMG( 1, N, NRHS, C, LDB, 0, 0 )
                     GO TO 90
                  END IF
*
*                 Subtract the time used in DTIMMG.
*
                  ICL = 1
                  S1 = DSECND( )
  110             CONTINUE
                  S2 = DSECND( )
                  UNTIME = S2 - S1
                  ICL = ICL + 1
                  IF( ICL.LE.IC ) THEN
                     CALL DTIMMG( 1, N, NRHS, C, LDB, 0, 0 )
                     GO TO 110
                  END IF
*
                  TIME = ( TIME-UNTIME ) / DBLE( IC )
                  OPS = NRHS*DOPBL2( 'DGBMV ', N, N, KL, KU )
                  RESLTS( IN, IK, ILDA ) = DMFLOP( OPS, TIME, 0 )
  120          CONTINUE
  130       CONTINUE
  140    CONTINUE
*
         CALL DPRTBL( LAB1, LAB2, NN, NVAL, NK, KVAL, NLDA, RESLTS,
     $                LDR1, LDR2, NOUT )
      END IF
*
  150 CONTINUE
 9999 FORMAT( 1X, A6, ':  Unrecognized path or subroutine name', / )
 9998 FORMAT( 1X, A6, ' timing run not attempted', / )
 9997 FORMAT( / ' *** Speed of ', A6, ' in megaflops ***' )
 9996 FORMAT( 5X, 'with LDA = ', I5 )
 9995 FORMAT( 5X, 'line ', I2, ' with LDA = ', I5 )
      RETURN
*
*     End of DTIMMV
*
      END
      SUBROUTINE DTIMPB( LINE, NN, NVAL, NK, KVAL, NNS, NSVAL, NNB,
     $                   NBVAL, NLDA, LDAVAL, TIMMIN, A, B, IWORK,
     $                   RESLTS, LDR1, LDR2, LDR3, NOUT )
*
*  -- LAPACK timing routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     March 31, 1993
*
*     .. Scalar Arguments ..
      CHARACTER*80       LINE
      INTEGER            LDR1, LDR2, LDR3, NK, NLDA, NN, NNB, NNS, NOUT
      DOUBLE PRECISION   TIMMIN
*     ..
*     .. Array Arguments ..
      INTEGER            IWORK( * ), KVAL( * ), LDAVAL( * ), NBVAL( * ),
     $                   NSVAL( * ), NVAL( * )
      DOUBLE PRECISION   A( * ), B( * ), RESLTS( LDR1, LDR2, LDR3, * )
*     ..
*
*  Purpose
*  =======
*
*  DTIMPB times DPBTRF and -TRS.
*
*  Arguments
*  =========
*
*  LINE    (input) CHARACTER*80
*          The input line that requested this routine.  The first six
*          characters contain either the name of a subroutine or a
*          generic path name.  The remaining characters may be used to
*          specify the individual routines to be timed.  See ATIMIN for
*          a full description of the format of the input line.
*
*  NN      (input) INTEGER
*          The number of values of N contained in the vector NVAL.
*
*  NVAL    (input) INTEGER array, dimension (NN)
*          The values of the matrix size N.
*
*  NK      (input) INTEGER
*          The number of values of K contained in the vector KVAL.
*
*  KVAL    (input) INTEGER array, dimension (NK)
*          The values of the band width K.
*
*  NNS     (input) INTEGER
*          The number of values of NRHS contained in the vector NSVAL.
*
*  NSVAL   (input) INTEGER array, dimension (NNS)
*          The values of the number of right hand sides NRHS.
*
*  NNB     (input) INTEGER
*          The number of values of NB contained in the vector NBVAL.
*
*  NBVAL   (input) INTEGER array, dimension (NNB)
*          The values of the blocksize NB.
*
*  NLDA    (input) INTEGER
*          The number of values of LDA contained in the vector LDAVAL.
*
*  LDAVAL  (input) INTEGER array, dimension (NLDA)
*          The values of the leading dimension of the array A.
*
*  TIMMIN  (input) DOUBLE PRECISION
*          The minimum time a subroutine will be timed.
*
*  A       (workspace) DOUBLE PRECISION array, dimension (LDAMAX*NMAX)
*          where LDAMAX and NMAX are the maximum values permitted
*          for LDA and N.
*
*  B       (workspace) DOUBLE PRECISION array, dimension (LDAMAX*NMAX)
*
*  IWORK   (workspace) INTEGER array, dimension (NMAX)
*
*  RESLTS  (output) DOUBLE PRECISION array, dimension
*                   (LDR1,LDR2,LDR3,NSUBS)
*          The timing results for each subroutine over the relevant
*          values of N, K, NB, and LDA.
*
*  LDR1    (input) INTEGER
*          The first dimension of RESLTS.  LDR1 >= max(4,NNB).
*
*  LDR2    (input) INTEGER
*          The second dimension of RESLTS.  LDR2 >= max(1,NK).
*
*  LDR3    (input) INTEGER
*          The third dimension of RESLTS.  LDR3 >= max(1,2*NLDA).
*
*  NOUT    (input) INTEGER
*          The unit number for output.
*
*  =====================================================================
*
*     .. Parameters ..
      INTEGER            NSUBS
      PARAMETER          ( NSUBS = 2 )
*     ..
*     .. Local Scalars ..
      CHARACTER          UPLO
      CHARACTER*3        PATH
      CHARACTER*6        CNAME
      INTEGER            I, I3, IC, ICL, IK, ILDA, IN, INB, INFO, ISUB,
     $                   IUPLO, K, LDA, LDB, MAT, N, NB, NRHS
      DOUBLE PRECISION   OPS, S1, S2, TIME, UNTIME
*     ..
*     .. Local Arrays ..
      LOGICAL            TIMSUB( NSUBS )
      CHARACTER          UPLOS( 2 )
      CHARACTER*6        SUBNAM( NSUBS )
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      DOUBLE PRECISION   DMFLOP, DOPLA, DSECND
      EXTERNAL           LSAME, DMFLOP, DOPLA, DSECND
*     ..
*     .. External Subroutines ..
      EXTERNAL           ATIMCK, ATIMIN, DPBTRF, DPBTRS, DPRTBL, DTIMMG,
     $                   XLAENV
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          DBLE, MAX, MIN
*     ..
*     .. Data statements ..
      DATA               UPLOS / 'U', 'L' /
      DATA               SUBNAM / 'DPBTRF', 'DPBTRS' /
*     ..
*     .. Executable Statements ..
*
*     Extract the timing request from the input line.
*
      PATH( 1: 1 ) = 'Double precision'
      PATH( 2: 3 ) = 'PB'
      CALL ATIMIN( PATH, LINE, NSUBS, SUBNAM, TIMSUB, NOUT, INFO )
      IF( INFO.NE.0 )
     $   GO TO 140
*
*     Check that K+1 <= LDA for the input values.
*
      CNAME = LINE( 1: 6 )
      CALL ATIMCK( 0, CNAME, NK, KVAL, NLDA, LDAVAL, NOUT, INFO )
      IF( INFO.GT.0 ) THEN
         WRITE( NOUT, FMT = 9999 )CNAME
         GO TO 140
      END IF
*
*     Do for each value of the matrix size N:
*
      DO 130 IN = 1, NN
         N = NVAL( IN )
*
*        Do first for UPLO = 'U', then for UPLO = 'L'
*
         DO 90 IUPLO = 1, 2
            UPLO = UPLOS( IUPLO )
            IF( LSAME( UPLO, 'U' ) ) THEN
               MAT = 5
            ELSE
               MAT = -5
            END IF
*
*           Do for each value of LDA:
*
            DO 80 ILDA = 1, NLDA
               LDA = LDAVAL( ILDA )
               I3 = ( IUPLO-1 )*NLDA + ILDA
*
*              Do for each value of the band width K:
*
               DO 70 IK = 1, NK
                  K = KVAL( IK )
                  K = MAX( 0, MIN( K, N-1 ) )
*
*                 Time DPBTRF
*
                  IF( TIMSUB( 1 ) ) THEN
*
*                    Do for each value of NB in NBVAL.  Only DPBTRF is
*                    timed in this loop since the other routines are
*                    independent of NB.
*
                     DO 30 INB = 1, NNB
                        NB = NBVAL( INB )
                        CALL XLAENV( 1, NB )
                        CALL DTIMMG( MAT, N, N, A, LDA, K, K )
                        IC = 0
                        S1 = DSECND( )
   10                   CONTINUE
                        CALL DPBTRF( UPLO, N, K, A, LDA, INFO )
                        S2 = DSECND( )
                        TIME = S2 - S1
                        IC = IC + 1
                        IF( TIME.LT.TIMMIN ) THEN
                           CALL DTIMMG( MAT, N, N, A, LDA, K, K )
                           GO TO 10
                        END IF
*
*                       Subtract the time used in DTIMMG.
*
                        ICL = 1
                        S1 = DSECND( )
   20                   CONTINUE
                        CALL DTIMMG( MAT, N, N, A, LDA, K, K )
                        S2 = DSECND( )
                        UNTIME = S2 - S1
                        ICL = ICL + 1
                        IF( ICL.LE.IC )
     $                     GO TO 20
*
                        TIME = ( TIME-UNTIME ) / DBLE( IC )
                        OPS = DOPLA( 'DPBTRF', N, N, K, K, NB )
                        RESLTS( INB, IK, I3, 1 ) = DMFLOP( OPS, TIME,
     $                     INFO )
   30                CONTINUE
                  ELSE
                     IC = 0
                     CALL DTIMMG( MAT, N, N, A, LDA, K, K )
                  END IF
*
*                 Generate another matrix and factor it using DPBTRF so
*                 that the factored form can be used in timing the other
*                 routines.
*
                  NB = 1
                  CALL XLAENV( 1, NB )
                  IF( IC.NE.1 )
     $               CALL DPBTRF( UPLO, N, K, A, LDA, INFO )
*
*                 Time DPBTRS
*
                  IF( TIMSUB( 2 ) ) THEN
                     DO 60 I = 1, NNS
                        NRHS = NSVAL( I )
                        LDB = N
                        CALL DTIMMG( 0, N, NRHS, B, LDB, 0, 0 )
                        IC = 0
                        S1 = DSECND( )
   40                   CONTINUE
                        CALL DPBTRS( UPLO, N, K, NRHS, A, LDA, B, LDB,
     $                               INFO )
                        S2 = DSECND( )
                        TIME = S2 - S1
                        IC = IC + 1
                        IF( TIME.LT.TIMMIN ) THEN
                           CALL DTIMMG( 0, N, NRHS, B, LDB, 0, 0 )
                           GO TO 40
                        END IF
*
*                       Subtract the time used in DTIMMG.
*
                        ICL = 1
                        S1 = DSECND( )
   50                   CONTINUE
                        S2 = DSECND( )
                        UNTIME = S2 - S1
                        ICL = ICL + 1
                        IF( ICL.LE.IC ) THEN
                           CALL DTIMMG( 0, N, NRHS, B, LDB, 0, 0 )
                           GO TO 50
                        END IF
*
                        TIME = ( TIME-UNTIME ) / DBLE( IC )
                        OPS = DOPLA( 'DPBTRS', N, NRHS, K, K, 0 )
                        RESLTS( I, IK, I3, 2 ) = DMFLOP( OPS, TIME,
     $                     INFO )
   60                CONTINUE
                  END IF
   70          CONTINUE
   80       CONTINUE
   90    CONTINUE
*
*        Print tables of results for each timed routine.
*
         DO 120 ISUB = 1, NSUBS
            IF( .NOT.TIMSUB( ISUB ) )
     $         GO TO 120
*
*           Print header for routine names.
*
            IF( IN.EQ.1 .OR. CNAME.EQ.'DPB   ' ) THEN
               WRITE( NOUT, FMT = 9998 )SUBNAM( ISUB )
               IF( NLDA.GT.1 ) THEN
                  DO 100 I = 1, NLDA
                     WRITE( NOUT, FMT = 9997 )I, LDAVAL( I )
  100             CONTINUE
               END IF
            END IF
            WRITE( NOUT, FMT = * )
            DO 110 IUPLO = 1, 2
               WRITE( NOUT, FMT = 9996 )SUBNAM( ISUB ), N,
     $            UPLOS( IUPLO )
               I3 = ( IUPLO-1 )*NLDA + 1
               IF( ISUB.EQ.1 ) THEN
                  CALL DPRTBL( 'NB', 'K', NNB, NBVAL, NK, KVAL, NLDA,
     $                         RESLTS( 1, 1, I3, 1 ), LDR1, LDR2, NOUT )
               ELSE IF( ISUB.EQ.2 ) THEN
                  CALL DPRTBL( 'NRHS', 'K', NNS, NSVAL, NK, KVAL, NLDA,
     $                         RESLTS( 1, 1, I3, 2 ), LDR1, LDR2, NOUT )
               END IF
  110       CONTINUE
  120    CONTINUE
  130 CONTINUE
*
  140 CONTINUE
 9999 FORMAT( 1X, A6, ' timing run not attempted', / )
 9998 FORMAT( / ' *** Speed of ', A6, ' in megaflops ***' )
 9997 FORMAT( 5X, 'line ', I2, ' with LDA = ', I5 )
 9996 FORMAT( 5X, A6, ' with M =', I6, ', UPLO = ''', A1, '''', / )
      RETURN
*
*     End of DTIMPB
*
      END
      SUBROUTINE DTIMPO( LINE, NN, NVAL, NNS, NSVAL, NNB, NBVAL, NLDA,
     $                   LDAVAL, TIMMIN, A, B, IWORK, RESLTS, LDR1,
     $                   LDR2, LDR3, NOUT )
*
*  -- LAPACK timing routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     March 31, 1993
*
*     .. Scalar Arguments ..
      CHARACTER*80       LINE
      INTEGER            LDR1, LDR2, LDR3, NLDA, NN, NNB, NNS, NOUT
      DOUBLE PRECISION   TIMMIN
*     ..
*     .. Array Arguments ..
      INTEGER            IWORK( * ), LDAVAL( * ), NBVAL( * ),
     $                   NSVAL( * ), NVAL( * )
      DOUBLE PRECISION   A( * ), B( * ), RESLTS( LDR1, LDR2, LDR3, * )
*     ..
*
*  Purpose
*  =======
*
*  DTIMPO times DPOTRF, -TRS, and -TRI.
*
*  Arguments
*  =========
*
*  LINE    (input) CHARACTER*80
*          The input line that requested this routine.  The first six
*          characters contain either the name of a subroutine or a
*          generic path name.  The remaining characters may be used to
*          specify the individual routines to be timed.  See ATIMIN for
*          a full description of the format of the input line.
*
*  NN      (input) INTEGER
*          The number of values of N contained in the vector NVAL.
*
*  NVAL    (input) INTEGER array, dimension (NN)
*          The values of the matrix size N.
*
*  NNS     (input) INTEGER
*          The number of values of NRHS contained in the vector NSVAL.
*
*  NSVAL   (input) INTEGER array, dimension (NNS)
*          The values of the number of right hand sides NRHS.
*
*  NNB     (input) INTEGER
*          The number of values of NB contained in the vector NBVAL.
*
*  NBVAL   (input) INTEGER array, dimension (NNB)
*          The values of the blocksize NB.
*
*  NLDA    (input) INTEGER
*          The number of values of LDA contained in the vector LDAVAL.
*
*  LDAVAL  (input) INTEGER array, dimension (NLDA)
*          The values of the leading dimension of the array A.
*
*  TIMMIN  (input) DOUBLE PRECISION
*          The minimum time a subroutine will be timed.
*
*  A       (workspace) DOUBLE PRECISION array, dimension (LDAMAX*NMAX)
*          where LDAMAX and NMAX are the maximum values permitted
*          for LDA and N.
*
*  B       (workspace) DOUBLE PRECISION array, dimension (LDAMAX*NMAX)
*
*  IWORK   (workspace) INTEGER array, dimension (NMAX)
*
*  RESLTS  (output) DOUBLE PRECISION array, dimension
*                   (LDR1,LDR2,LDR3,NSUBS)
*          The timing results for each subroutine over the relevant
*          values of N, NB, and LDA.
*
*  LDR1    (input) INTEGER
*          The first dimension of RESLTS.  LDR1 >= max(4,NNB).
*
*  LDR2    (input) INTEGER
*          The second dimension of RESLTS.  LDR2 >= max(1,NN).
*
*  LDR3    (input) INTEGER
*          The third dimension of RESLTS.  LDR3 >= max(1,2*NLDA).
*
*  NOUT    (input) INTEGER
*          The unit number for output.
*
*  =====================================================================
*
*     .. Parameters ..
      INTEGER            NSUBS
      PARAMETER          ( NSUBS = 3 )
*     ..
*     .. Local Scalars ..
      CHARACTER          UPLO
      CHARACTER*3        PATH
      CHARACTER*6        CNAME
      INTEGER            I, I3, IC, ICL, ILDA, IN, INB, INFO, ISUB,
     $                   IUPLO, LDA, LDB, MAT, N, NB, NRHS
      DOUBLE PRECISION   OPS, S1, S2, TIME, UNTIME
*     ..
*     .. Local Arrays ..
      LOGICAL            TIMSUB( NSUBS )
      CHARACTER          UPLOS( 2 )
      CHARACTER*6        SUBNAM( NSUBS )
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      DOUBLE PRECISION   DMFLOP, DOPLA, DSECND
      EXTERNAL           LSAME, DMFLOP, DOPLA, DSECND
*     ..
*     .. External Subroutines ..
      EXTERNAL           ATIMCK, ATIMIN, DLACPY, DPOTRF, DPOTRI, DPOTRS,
     $                   DPRTBL, DTIMMG, XLAENV
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          DBLE
*     ..
*     .. Data statements ..
      DATA               UPLOS / 'U', 'L' /
      DATA               SUBNAM / 'DPOTRF', 'DPOTRS', 'DPOTRI' /
*     ..
*     .. Executable Statements ..
*
*     Extract the timing request from the input line.
*
      PATH( 1: 1 ) = 'Double precision'
      PATH( 2: 3 ) = 'PO'
      CALL ATIMIN( PATH, LINE, NSUBS, SUBNAM, TIMSUB, NOUT, INFO )
      IF( INFO.NE.0 )
     $   GO TO 150
*
*     Check that N <= LDA for the input values.
*
      CNAME = LINE( 1: 6 )
      CALL ATIMCK( 2, CNAME, NN, NVAL, NLDA, LDAVAL, NOUT, INFO )
      IF( INFO.GT.0 ) THEN
         WRITE( NOUT, FMT = 9999 )CNAME
         GO TO 150
      END IF
*
*     Do first for UPLO = 'U', then for UPLO = 'L'
*
      DO 110 IUPLO = 1, 2
         UPLO = UPLOS( IUPLO )
         IF( LSAME( UPLO, 'U' ) ) THEN
            MAT = 3
         ELSE
            MAT = -3
         END IF
*
*        Do for each value of N in NVAL.
*
         DO 100 IN = 1, NN
            N = NVAL( IN )
*
*           Do for each value of LDA:
*
            DO 90 ILDA = 1, NLDA
               LDA = LDAVAL( ILDA )
               I3 = ( IUPLO-1 )*NLDA + ILDA
*
*              Do for each value of NB in NBVAL.  Only the blocked
*              routines are timed in this loop since the other routines
*              are independent of NB.
*
               DO 50 INB = 1, NNB
                  NB = NBVAL( INB )
                  CALL XLAENV( 1, NB )
*
*                 Time DPOTRF
*
                  IF( TIMSUB( 1 ) ) THEN
                     CALL DTIMMG( MAT, N, N, A, LDA, 0, 0 )
                     IC = 0
                     S1 = DSECND( )
   10                CONTINUE
                     CALL DPOTRF( UPLO, N, A, LDA, INFO )
                     S2 = DSECND( )
                     TIME = S2 - S1
                     IC = IC + 1
                     IF( TIME.LT.TIMMIN ) THEN
                        CALL DTIMMG( MAT, N, N, A, LDA, 0, 0 )
                        GO TO 10
                     END IF
*
*                    Subtract the time used in DTIMMG.
*
                     ICL = 1
                     S1 = DSECND( )
   20                CONTINUE
                     S2 = DSECND( )
                     UNTIME = S2 - S1
                     ICL = ICL + 1
                     IF( ICL.LE.IC ) THEN
                        CALL DTIMMG( MAT, N, N, A, LDA, 0, 0 )
                        GO TO 20
                     END IF
*
                     TIME = ( TIME-UNTIME ) / DBLE( IC )
                     OPS = DOPLA( 'DPOTRF', N, N, 0, 0, NB )
                     RESLTS( INB, IN, I3, 1 ) = DMFLOP( OPS, TIME,
     $                  INFO )
*
                  ELSE
                     IC = 0
                     CALL DTIMMG( MAT, N, N, A, LDA, 0, 0 )
                  END IF
*
*                 Generate another matrix and factor it using DPOTRF so
*                 that the factored form can be used in timing the other
*                 routines.
*
                  IF( IC.NE.1 )
     $               CALL DPOTRF( UPLO, N, A, LDA, INFO )
*
*                 Time DPOTRI
*
                  IF( TIMSUB( 3 ) ) THEN
                     CALL DLACPY( UPLO, N, N, A, LDA, B, LDA )
                     IC = 0
                     S1 = DSECND( )
   30                CONTINUE
                     CALL DPOTRI( UPLO, N, B, LDA, INFO )
                     S2 = DSECND( )
                     TIME = S2 - S1
                     IC = IC + 1
                     IF( TIME.LT.TIMMIN ) THEN
                        CALL DLACPY( UPLO, N, N, A, LDA, B, LDA )
                        GO TO 30
                     END IF
*
*                    Subtract the time used in DLACPY.
*
                     ICL = 1
                     S1 = DSECND( )
   40                CONTINUE
                     S2 = DSECND( )
                     UNTIME = S2 - S1
                     ICL = ICL + 1
                     IF( ICL.LE.IC ) THEN
                        CALL DLACPY( UPLO, N, N, A, LDA, B, LDA )
                        GO TO 40
                     END IF
*
                     TIME = ( TIME-UNTIME ) / DBLE( IC )
                     OPS = DOPLA( 'DPOTRI', N, N, 0, 0, NB )
                     RESLTS( INB, IN, I3, 3 ) = DMFLOP( OPS, TIME,
     $                  INFO )
                  END IF
   50          CONTINUE
*
*              Time DPOTRS
*
               IF( TIMSUB( 2 ) ) THEN
                  DO 80 I = 1, NNS
                     NRHS = NSVAL( I )
                     LDB = LDA
                     CALL DTIMMG( 0, N, NRHS, B, LDB, 0, 0 )
                     IC = 0
                     S1 = DSECND( )
   60                CONTINUE
                     CALL DPOTRS( UPLO, N, NRHS, A, LDA, B, LDB, INFO )
                     S2 = DSECND( )
                     TIME = S2 - S1
                     IC = IC + 1
                     IF( TIME.LT.TIMMIN ) THEN
                        CALL DTIMMG( 0, N, NRHS, B, LDB, 0, 0 )
                        GO TO 60
                     END IF
*
*                    Subtract the time used in DTIMMG.
*
                     ICL = 1
                     S1 = DSECND( )
   70                CONTINUE
                     S2 = DSECND( )
                     UNTIME = S2 - S1
                     ICL = ICL + 1
                     IF( ICL.LE.IC ) THEN
                        CALL DTIMMG( 0, N, NRHS, B, LDB, 0, 0 )
                        GO TO 70
                     END IF
*
                     TIME = ( TIME-UNTIME ) / DBLE( IC )
                     OPS = DOPLA( 'DPOTRS', N, NRHS, 0, 0, 0 )
                     RESLTS( I, IN, I3, 2 ) = DMFLOP( OPS, TIME, INFO )
   80             CONTINUE
               END IF
   90       CONTINUE
  100    CONTINUE
  110 CONTINUE
*
*     Print tables of results for each timed routine.
*
      DO 140 ISUB = 1, NSUBS
         IF( .NOT.TIMSUB( ISUB ) )
     $      GO TO 140
         WRITE( NOUT, FMT = 9998 )SUBNAM( ISUB )
         IF( NLDA.GT.1 ) THEN
            DO 120 I = 1, NLDA
               WRITE( NOUT, FMT = 9997 )I, LDAVAL( I )
  120       CONTINUE
         END IF
         WRITE( NOUT, FMT = * )
         DO 130 IUPLO = 1, 2
            WRITE( NOUT, FMT = 9996 )SUBNAM( ISUB ), UPLOS( IUPLO )
            I3 = ( IUPLO-1 )*NLDA + 1
            IF( ISUB.EQ.1 ) THEN
               CALL DPRTBL( 'NB', 'N', NNB, NBVAL, NN, NVAL, NLDA,
     $                      RESLTS( 1, 1, I3, 1 ), LDR1, LDR2, NOUT )
            ELSE IF( ISUB.EQ.2 ) THEN
               CALL DPRTBL( 'NRHS', 'N', NNS, NSVAL, NN, NVAL, NLDA,
     $                      RESLTS( 1, 1, I3, 2 ), LDR1, LDR2, NOUT )
            ELSE IF( ISUB.EQ.3 ) THEN
               CALL DPRTBL( 'NB', 'N', NNB, NBVAL, NN, NVAL, NLDA,
     $                      RESLTS( 1, 1, I3, 3 ), LDR1, LDR2, NOUT )
            END IF
  130    CONTINUE
  140 CONTINUE
*
  150 CONTINUE
 9999 FORMAT( 1X, A6, ' timing run not attempted', / )
 9998 FORMAT( / ' *** Speed of ', A6, ' in megaflops ***' )
 9997 FORMAT( 5X, 'line ', I2, ' with LDA = ', I5 )
 9996 FORMAT( 5X, A6, ' with UPLO = ''', A1, '''', / )
      RETURN
*
*     End of DTIMPO
*
      END
      SUBROUTINE DTIMPP( LINE, NN, NVAL, NNS, NSVAL, LA, TIMMIN, A, B,
     $                   IWORK, RESLTS, LDR1, LDR2, LDR3, NOUT )
*
*  -- LAPACK timing routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     March 31, 1993
*
*     .. Scalar Arguments ..
      CHARACTER*80       LINE
      INTEGER            LA, LDR1, LDR2, LDR3, NN, NNS, NOUT
      DOUBLE PRECISION   TIMMIN
*     ..
*     .. Array Arguments ..
      INTEGER            IWORK( * ), NSVAL( * ), NVAL( * )
      DOUBLE PRECISION   A( * ), B( * ), RESLTS( LDR1, LDR2, LDR3, * )
*     ..
*
*  Purpose
*  =======
*
*  DTIMPP times DPPTRF, -TRS, and -TRI.
*
*  Arguments
*  =========
*
*  LINE    (input) CHARACTER*80
*          The input line that requested this routine.  The first six
*          characters contain either the name of a subroutine or a
*          generic path name.  The remaining characters may be used to
*          specify the individual routines to be timed.  See ATIMIN for
*          a full description of the format of the input line.
*
*  NN      (input) INTEGER
*          The number of values of N contained in the vector NVAL.
*
*  NVAL    (input) INTEGER array, dimension (NN)
*          The values of the matrix size N.
*
*  NNS     (input) INTEGER
*          The number of values of NRHS contained in the vector NSVAL.
*
*  NSVAL   (input) INTEGER array, dimension (NNS)
*          The values of the number of right hand sides NRHS.
*
*  LA      (input) INTEGER
*          The size of the arrays A, B, and C.
*
*  TIMMIN  (input) DOUBLE PRECISION
*          The minimum time a subroutine will be timed.
*
*  A       (workspace) DOUBLE PRECISION array, dimension (LA)
*
*  B       (workspace) DOUBLE PRECISION array, dimension (LA)
*
*  IWORK   (workspace) INTEGER array, dimension (NMAX)
*          where NMAX is the maximum value of N permitted.
*
*  RESLTS  (output) DOUBLE PRECISION array, dimension
*                   (LDR1,LDR2,LDR3,NSUBS)
*          The timing results for each subroutine over the relevant
*          values of N.
*
*  LDR1    (input) INTEGER
*          The first dimension of RESLTS.  LDR1 >= max(4,NNB).
*
*  LDR2    (input) INTEGER
*          The second dimension of RESLTS.  LDR2 >= max(1,NN).
*
*  LDR3    (input) INTEGER
*          The third dimension of RESLTS.  LDR3 >= 2.
*
*  NOUT    (input) INTEGER
*          The unit number for output.
*
*  =====================================================================
*
*     .. Parameters ..
      INTEGER            NSUBS
      PARAMETER          ( NSUBS = 3 )
*     ..
*     .. Local Scalars ..
      CHARACTER          UPLO
      CHARACTER*3        PATH
      CHARACTER*6        CNAME
      INTEGER            I, IC, ICL, IN, INFO, ISUB, IUPLO, LDA, LDB,
     $                   MAT, N, NRHS
      DOUBLE PRECISION   OPS, S1, S2, TIME, UNTIME
*     ..
*     .. Local Arrays ..
      LOGICAL            TIMSUB( NSUBS )
      CHARACTER          UPLOS( 2 )
      CHARACTER*6        SUBNAM( NSUBS )
      INTEGER            LAVAL( 1 )
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      DOUBLE PRECISION   DMFLOP, DOPLA, DSECND
      EXTERNAL           LSAME, DMFLOP, DOPLA, DSECND
*     ..
*     .. External Subroutines ..
      EXTERNAL           ATIMCK, ATIMIN, DCOPY, DPPTRF, DPPTRI, DPPTRS,
     $                   DPRTBL, DTIMMG
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          DBLE, MOD
*     ..
*     .. Data statements ..
      DATA               UPLOS / 'U', 'L' /
      DATA               SUBNAM / 'DPPTRF', 'DPPTRS', 'DPPTRI' /
*     ..
*     .. Executable Statements ..
*
*     Extract the timing request from the input line.
*
      PATH( 1: 1 ) = 'Double precision'
      PATH( 2: 3 ) = 'PP'
      CALL ATIMIN( PATH, LINE, NSUBS, SUBNAM, TIMSUB, NOUT, INFO )
      IF( INFO.NE.0 )
     $   GO TO 120
*
*     Check that N*(N+1)/2 <= LA for the input values.
*
      CNAME = LINE( 1: 6 )
      LAVAL( 1 ) = LA
      CALL ATIMCK( 4, CNAME, NN, NVAL, 1, LAVAL, NOUT, INFO )
      IF( INFO.GT.0 ) THEN
         WRITE( NOUT, FMT = 9999 )CNAME
         GO TO 120
      END IF
*
*     Do first for UPLO = 'U', then for UPLO = 'L'
*
      DO 90 IUPLO = 1, 2
         UPLO = UPLOS( IUPLO )
         IF( LSAME( UPLO, 'U' ) ) THEN
            MAT = 4
         ELSE
            MAT = -4
         END IF
*
*        Do for each value of N in NVAL.
*
         DO 80 IN = 1, NN
            N = NVAL( IN )
            LDA = N*( N+1 ) / 2
*
*           Time DPPTRF
*
            IF( TIMSUB( 1 ) ) THEN
               CALL DTIMMG( MAT, N, N, A, LDA, 0, 0 )
               IC = 0
               S1 = DSECND( )
   10          CONTINUE
               CALL DPPTRF( UPLO, N, A, INFO )
               S2 = DSECND( )
               TIME = S2 - S1
               IC = IC + 1
               IF( TIME.LT.TIMMIN ) THEN
                  CALL DTIMMG( MAT, N, N, A, LDA, 0, 0 )
                  GO TO 10
               END IF
*
*              Subtract the time used in DTIMMG.
*
               ICL = 1
               S1 = DSECND( )
   20          CONTINUE
               S2 = DSECND( )
               UNTIME = S2 - S1
               ICL = ICL + 1
               IF( ICL.LE.IC ) THEN
                  CALL DTIMMG( MAT, N, N, A, LDA, 0, 0 )
                  GO TO 20
               END IF
*
               TIME = ( TIME-UNTIME ) / DBLE( IC )
               OPS = DOPLA( 'DPPTRF', N, N, 0, 0, 0 )
               RESLTS( 1, IN, IUPLO, 1 ) = DMFLOP( OPS, TIME, INFO )
*
            ELSE
               IC = 0
               CALL DTIMMG( MAT, N, N, A, LDA, 0, 0 )
            END IF
*
*           Generate another matrix and factor it using DPPTRF so
*           that the factored form can be used in timing the other
*           routines.
*
            IF( IC.NE.1 )
     $         CALL DPPTRF( UPLO, N, A, INFO )
*
*           Time DPPTRI
*
            IF( TIMSUB( 3 ) ) THEN
               CALL DCOPY( LDA, A, 1, B, 1 )
               IC = 0
               S1 = DSECND( )
   30          CONTINUE
               CALL DPPTRI( UPLO, N, B, INFO )
               S2 = DSECND( )
               TIME = S2 - S1
               IC = IC + 1
               IF( TIME.LT.TIMMIN ) THEN
                  CALL DCOPY( LDA, A, 1, B, 1 )
                  GO TO 30
               END IF
*
*              Subtract the time used in DLACPY.
*
               ICL = 1
               S1 = DSECND( )
   40          CONTINUE
               S2 = DSECND( )
               UNTIME = S2 - S1
               ICL = ICL + 1
               IF( ICL.LE.IC ) THEN
                  CALL DCOPY( LDA, A, 1, B, 1 )
                  GO TO 40
               END IF
*
               TIME = ( TIME-UNTIME ) / DBLE( IC )
               OPS = DOPLA( 'DPPTRI', N, N, 0, 0, 0 )
               RESLTS( 1, IN, IUPLO, 3 ) = DMFLOP( OPS, TIME, INFO )
            END IF
*
*           Time DPPTRS
*
            IF( TIMSUB( 2 ) ) THEN
               DO 70 I = 1, NNS
                  NRHS = NSVAL( I )
                  LDB = N
                  IF( MOD( LDB, 2 ).EQ.0 )
     $               LDB = LDB + 1
                  CALL DTIMMG( 0, N, NRHS, B, LDB, 0, 0 )
                  IC = 0
                  S1 = DSECND( )
   50             CONTINUE
                  CALL DPPTRS( UPLO, N, NRHS, A, B, LDB, INFO )
                  S2 = DSECND( )
                  TIME = S2 - S1
                  IC = IC + 1
                  IF( TIME.LT.TIMMIN ) THEN
                     CALL DTIMMG( 0, N, NRHS, B, LDB, 0, 0 )
                     GO TO 50
                  END IF
*
*                 Subtract the time used in DTIMMG.
*
                  ICL = 1
                  S1 = DSECND( )
   60             CONTINUE
                  S2 = DSECND( )
                  UNTIME = S2 - S1
                  ICL = ICL + 1
                  IF( ICL.LE.IC ) THEN
                     CALL DTIMMG( 0, N, NRHS, B, LDB, 0, 0 )
                     GO TO 60
                  END IF
*
                  TIME = ( TIME-UNTIME ) / DBLE( IC )
                  OPS = DOPLA( 'DPPTRS', N, NRHS, 0, 0, 0 )
                  RESLTS( I, IN, IUPLO, 2 ) = DMFLOP( OPS, TIME, INFO )
   70          CONTINUE
            END IF
   80    CONTINUE
   90 CONTINUE
*
*     Print tables of results for each timed routine.
*
      DO 110 ISUB = 1, NSUBS
         IF( .NOT.TIMSUB( ISUB ) )
     $      GO TO 110
         WRITE( NOUT, FMT = 9998 )SUBNAM( ISUB )
         DO 100 IUPLO = 1, 2
            WRITE( NOUT, FMT = 9997 )SUBNAM( ISUB ), UPLOS( IUPLO )
            IF( ISUB.EQ.1 ) THEN
               CALL DPRTBL( ' ', 'N', 1, LAVAL, NN, NVAL, 1,
     $                      RESLTS( 1, 1, IUPLO, 1 ), LDR1, LDR2, NOUT )
            ELSE IF( ISUB.EQ.2 ) THEN
               CALL DPRTBL( 'NRHS', 'N', NNS, NSVAL, NN, NVAL, 1,
     $                      RESLTS( 1, 1, IUPLO, 2 ), LDR1, LDR2, NOUT )
            ELSE IF( ISUB.EQ.3 ) THEN
               CALL DPRTBL( ' ', 'N', 1, LAVAL, NN, NVAL, 1,
     $                      RESLTS( 1, 1, IUPLO, 3 ), LDR1, LDR2, NOUT )
            END IF
  100    CONTINUE
  110 CONTINUE
  120 CONTINUE
 9999 FORMAT( 1X, A6, ' timing run not attempted', / )
 9998 FORMAT( / ' *** Speed of ', A6, ' in megaflops ***', / )
 9997 FORMAT( 5X, A6, ' with UPLO = ''', A1, '''', / )
      RETURN
*
*     End of DTIMPP
*
      END
      SUBROUTINE DTIMPT( LINE, NM, MVAL, NNS, NSVAL, NLDA, LDAVAL,
     $                   TIMMIN, A, B, RESLTS, LDR1, LDR2, LDR3, NOUT )
*
*  -- LAPACK timing routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     March 31, 1993
*
*     .. Scalar Arguments ..
      CHARACTER*80       LINE
      INTEGER            LDR1, LDR2, LDR3, NLDA, NM, NNS, NOUT
      DOUBLE PRECISION   TIMMIN
*     ..
*     .. Array Arguments ..
      INTEGER            LDAVAL( * ), MVAL( * ), NSVAL( * )
      DOUBLE PRECISION   A( * ), B( * ), RESLTS( LDR1, LDR2, LDR3, * )
*     ..
*
*  Purpose
*  =======
*
*  DTIMPT times DPTTRF, -TRS, -SV, and -SL.
*
*  Arguments
*  =========
*
*  LINE    (input) CHARACTER*80
*          The input line that requested this routine.  The first six
*          characters contain either the name of a subroutine or a
*          generic path name.  The remaining characters may be used to
*          specify the individual routines to be timed.  See ATIMIN for
*          a full description of the format of the input line.
*
*  NM      (input) INTEGER
*          The number of values of M contained in the vector MVAL.
*
*  MVAL    (input) INTEGER array, dimension (NM)
*          The values of the matrix size M.
*
*  NNS     (input) INTEGER
*          The number of values of NRHS contained in the vector NSVAL.
*
*  NSVAL   (input) INTEGER array, dimension (NNS)
*          The values of the number of right hand sides NRHS.
*
*  NLDA    (input) INTEGER
*          The number of values of LDA contained in the vector LDAVAL.
*
*  LDAVAL  (input) INTEGER array, dimension (NLDA)
*          The values of the leading dimension of the array A.
*
*  TIMMIN  (input) DOUBLE PRECISION
*          The minimum time a subroutine will be timed.
*
*  A       (workspace) DOUBLE PRECISION array, dimension (NMAX*2)
*          where NMAX is the maximum value permitted for N.
*
*  B       (workspace) DOUBLE PRECISION array, dimension (LDAMAX*NMAX)
*
*  RESLTS  (output) DOUBLE PRECISION array, dimension
*                   (LDR1,LDR2,LDR3,NSUBS)
*          The timing results for each subroutine over the relevant
*          values of N.
*
*  LDR1    (input) INTEGER
*          The first dimension of RESLTS.  LDR1 >= 1.
*
*  LDR2    (input) INTEGER
*          The second dimension of RESLTS.  LDR2 >= max(1,NM).
*
*  LDR3    (input) INTEGER
*          The third dimension of RESLTS.  LDR3 >= max(1,NLDA).
*
*  NOUT    (input) INTEGER
*          The unit number for output.
*
*  =====================================================================
*
*     .. Parameters ..
      INTEGER            NSUBS
      PARAMETER          ( NSUBS = 4 )
*     ..
*     .. Local Scalars ..
      CHARACTER*3        PATH
      CHARACTER*6        CNAME
      INTEGER            I, IC, ICL, ILDA, IM, INFO, ISUB, LDB, M, N,
     $                   NRHS
      DOUBLE PRECISION   OPS, S1, S2, TIME, UNTIME
*     ..
*     .. Local Arrays ..
      LOGICAL            TIMSUB( NSUBS )
      CHARACTER*6        SUBNAM( NSUBS )
      INTEGER            LAVAL( 1 )
*     ..
*     .. External Functions ..
      DOUBLE PRECISION   DMFLOP, DOPLA, DSECND
      EXTERNAL           DMFLOP, DOPLA, DSECND
*     ..
*     .. External Subroutines ..
      EXTERNAL           ATIMCK, ATIMIN, DPRTBL, DPTSV, DPTTRF, DPTTRS,
     $                   DTIMMG, DPTSL
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          DBLE, MAX
*     ..
*     .. Data statements ..
      DATA               SUBNAM / 'DPTTRF', 'DPTTRS', 'DPTSV ',
     $                   'DPTSL ' /
*     ..
*     .. Executable Statements ..
*
*     Extract the timing request from the input line.
*
      PATH( 1: 1 ) = 'Double precision'
      PATH( 2: 3 ) = 'PT'
      CALL ATIMIN( PATH, LINE, NSUBS, SUBNAM, TIMSUB, NOUT, INFO )
      IF( INFO.NE.0 )
     $   GO TO 170
*
*     Check that N <= LDA for the input values.
*
      DO 10 ISUB = 2, NSUBS
         IF( .NOT.TIMSUB( ISUB ) )
     $      GO TO 10
         CNAME = SUBNAM( ISUB )
         CALL ATIMCK( 2, CNAME, NM, MVAL, NLDA, LDAVAL, NOUT, INFO )
         IF( INFO.GT.0 ) THEN
            WRITE( NOUT, FMT = 9999 )CNAME
            TIMSUB( ISUB ) = .FALSE.
         END IF
   10 CONTINUE
*
*     Do for each value of M:
*
      DO 140 IM = 1, NM
*
         M = MVAL( IM )
         N = MAX( M, 1 )
*
*        Time DPTTRF
*
         IF( TIMSUB( 1 ) ) THEN
            CALL DTIMMG( 13, M, M, A, 2*N, 0, 0 )
            IC = 0
            S1 = DSECND( )
   20       CONTINUE
            CALL DPTTRF( M, A, A( N+1 ), INFO )
            S2 = DSECND( )
            TIME = S2 - S1
            IC = IC + 1
            IF( TIME.LT.TIMMIN ) THEN
               CALL DTIMMG( 13, M, M, A, 2*N, 0, 0 )
               GO TO 20
            END IF
*
*           Subtract the time used in DTIMMG.
*
            ICL = 1
            S1 = DSECND( )
   30       CONTINUE
            S2 = DSECND( )
            UNTIME = S2 - S1
            ICL = ICL + 1
            IF( ICL.LE.IC ) THEN
               CALL DTIMMG( 13, M, M, A, 2*N, 0, 0 )
               GO TO 30
            END IF
*
            TIME = ( TIME-UNTIME ) / DBLE( IC )
            OPS = DOPLA( 'DPTTRF', M, 0, 0, 0, 0 )
            RESLTS( 1, IM, 1, 1 ) = DMFLOP( OPS, TIME, INFO )
*
         ELSE
            IC = 0
            CALL DTIMMG( 13, M, M, A, 2*N, 0, 0 )
         END IF
*
*        Generate another matrix and factor it using DPTTRF so
*        that the factored form can be used in timing the other
*        routines.
*
         IF( IC.NE.1 )
     $      CALL DPTTRF( M, A, A( N+1 ), INFO )
*
*        Time DPTTRS
*
         IF( TIMSUB( 2 ) ) THEN
            DO 70 ILDA = 1, NLDA
               LDB = LDAVAL( ILDA )
               DO 60 I = 1, NNS
                  NRHS = NSVAL( I )
                  CALL DTIMMG( 0, M, NRHS, B, LDB, 0, 0 )
                  IC = 0
                  S1 = DSECND( )
   40             CONTINUE
                  CALL DPTTRS( M, NRHS, A, A( N+1 ), B, LDB, INFO )
                  S2 = DSECND( )
                  TIME = S2 - S1
                  IC = IC + 1
                  IF( TIME.LT.TIMMIN ) THEN
                     CALL DTIMMG( 0, M, NRHS, B, LDB, 0, 0 )
                     GO TO 40
                  END IF
*
*                 Subtract the time used in DTIMMG.
*
                  ICL = 1
                  S1 = DSECND( )
   50             CONTINUE
                  S2 = DSECND( )
                  UNTIME = S2 - S1
                  ICL = ICL + 1
                  IF( ICL.LE.IC ) THEN
                     CALL DTIMMG( 0, M, NRHS, B, LDB, 0, 0 )
                     GO TO 50
                  END IF
*
                  TIME = ( TIME-UNTIME ) / DBLE( IC )
                  OPS = DOPLA( 'DPTTRS', M, NRHS, 0, 0, 0 )
                  RESLTS( I, IM, ILDA, 2 ) = DMFLOP( OPS, TIME, INFO )
   60          CONTINUE
   70       CONTINUE
         END IF
*
         IF( TIMSUB( 3 ) ) THEN
            DO 110 ILDA = 1, NLDA
               LDB = LDAVAL( ILDA )
               DO 100 I = 1, NNS
                  NRHS = NSVAL( I )
                  CALL DTIMMG( 13, M, M, A, 2*N, 0, 0 )
                  CALL DTIMMG( 0, M, NRHS, B, LDB, 0, 0 )
                  IC = 0
                  S1 = DSECND( )
   80             CONTINUE
                  CALL DPTSV( M, NRHS, A, A( N+1 ), B, LDB, INFO )
                  S2 = DSECND( )
                  TIME = S2 - S1
                  IC = IC + 1
                  IF( TIME.LT.TIMMIN ) THEN
                     CALL DTIMMG( 13, M, M, A, 2*N, 0, 0 )
                     CALL DTIMMG( 0, M, NRHS, B, LDB, 0, 0 )
                     GO TO 80
                  END IF
*
*                 Subtract the time used in DTIMMG.
*
                  ICL = 1
                  S1 = DSECND( )
   90             CONTINUE
                  S2 = DSECND( )
                  UNTIME = S2 - S1
                  ICL = ICL + 1
                  IF( ICL.LE.IC ) THEN
                     CALL DTIMMG( 13, M, M, A, 2*N, 0, 0 )
                     CALL DTIMMG( 0, M, NRHS, B, LDB, 0, 0 )
                     GO TO 90
                  END IF
*
                  TIME = ( TIME-UNTIME ) / DBLE( IC )
                  OPS = DOPLA( 'DPTSV ', M, NRHS, 0, 0, 0 )
                  RESLTS( I, IM, ILDA, 3 ) = DMFLOP( OPS, TIME, INFO )
  100          CONTINUE
  110       CONTINUE
         END IF
*
         IF( TIMSUB( 4 ) ) THEN
            CALL DTIMMG( 13, M, M, A, 2*N, 0, 0 )
            CALL DTIMMG( 0, M, 1, B, N, 0, 0 )
            IC = 0
            S1 = DSECND( )
  120       CONTINUE
            CALL DPTSL( M, A, A( N+1 ), B )
            S2 = DSECND( )
            TIME = S2 - S1
            IC = IC + 1
            IF( TIME.LT.TIMMIN ) THEN
               CALL DTIMMG( 13, M, M, A, 2*N, 0, 0 )
               CALL DTIMMG( 0, M, 1, B, N, 0, 0 )
               GO TO 120
            END IF
*
*           Subtract the time used in DTIMMG.
*
            ICL = 1
            S1 = DSECND( )
  130       CONTINUE
            S2 = DSECND( )
            UNTIME = S2 - S1
            ICL = ICL + 1
            IF( ICL.LE.IC ) THEN
               CALL DTIMMG( 13, M, M, A, 2*N, 0, 0 )
               CALL DTIMMG( 0, M, 1, B, N, 0, 0 )
               GO TO 130
            END IF
*
            TIME = ( TIME-UNTIME ) / DBLE( IC )
            OPS = DOPLA( 'DPTSV ', M, 1, 0, 0, 0 )
            RESLTS( 1, IM, 1, 4 ) = DMFLOP( OPS, TIME, INFO )
         END IF
  140 CONTINUE
*
*     Print a table of results for each timed routine.
*
      DO 160 ISUB = 1, NSUBS
         IF( .NOT.TIMSUB( ISUB ) )
     $      GO TO 160
         WRITE( NOUT, FMT = 9998 )SUBNAM( ISUB )
         IF( NLDA.GT.1 .AND. ( TIMSUB( 2 ) .OR. TIMSUB( 3 ) ) ) THEN
            DO 150 I = 1, NLDA
               WRITE( NOUT, FMT = 9997 )I, LDAVAL( I )
  150       CONTINUE
         END IF
         WRITE( NOUT, FMT = * )
         IF( ISUB.EQ.1 ) THEN
            CALL DPRTBL( ' ', 'N', 1, LAVAL, NM, MVAL, 1, RESLTS, LDR1,
     $                   LDR2, NOUT )
         ELSE IF( ISUB.EQ.2 ) THEN
            CALL DPRTBL( 'NRHS', 'N', NNS, NSVAL, NM, MVAL, NLDA,
     $                   RESLTS( 1, 1, 1, 2 ), LDR1, LDR2, NOUT )
         ELSE IF( ISUB.EQ.3 ) THEN
            CALL DPRTBL( 'NRHS', 'N', NNS, NSVAL, NM, MVAL, NLDA,
     $                   RESLTS( 1, 1, 1, 3 ), LDR1, LDR2, NOUT )
         ELSE IF( ISUB.EQ.4 ) THEN
            CALL DPRTBL( ' ', 'N', 1, LAVAL, NM, MVAL, 1,
     $                   RESLTS( 1, 1, 1, 4 ), LDR1, LDR2, NOUT )
         END IF
  160 CONTINUE
*
  170 CONTINUE
 9999 FORMAT( 1X, A6, ' timing run not attempted', / )
 9998 FORMAT( / ' *** Speed of ', A6, ' in megaflops ***' )
 9997 FORMAT( 5X, 'line ', I2, ' with LDA = ', I5 )
      RETURN
*
*     End of DTIMPT
*
      END
      SUBROUTINE DTIMQ3( LINE, NM, MVAL, NVAL, NNB, NBVAL, NXVAL, NLDA,
     $                   LDAVAL, TIMMIN, A, COPYA, TAU, WORK, IWORK,
     $                   RESLTS, LDR1, LDR2, NOUT )
*
*  -- LAPACK timing routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     December 22, 1999
*
*     Rewritten to time qp3 code.
*
*     .. Scalar Arguments ..
      CHARACTER*80       LINE
      INTEGER            LDR1, LDR2, NLDA, NM, NNB, NOUT
      DOUBLE PRECISION   TIMMIN
*     ..
*     .. Array Arguments ..
      INTEGER            IWORK( * ), LDAVAL( * ), MVAL( * ), NBVAL( * ),
     $                   NVAL( * ), NXVAL( * )
      DOUBLE PRECISION   A( * ), COPYA( * ), RESLTS( LDR1, LDR2, * ),
     $                   TAU( * ), WORK( * )
*     ..
*
*  Purpose
*  =======
*
*  DTIMQ3 times the routines to perform the Rank-Revealing QR
*  factorization of a DOUBLE PRECISION general matrix.
*
*  Two matrix types may be used for timing.  The number of types is
*  set in the parameter NMODE and the matrix types are set in the vector
*  MODES, using the following key:
*     2.  BREAK1    D(1:N-1)=1 and D(N)=1.0/COND in DLATMS
*     3.  GEOM      D(I)=COND**(-(I-1)/(N-1)) in DLATMS
*  These numbers are chosen to correspond with the matrix types in the
*  test code.
*
*  Arguments
*  =========
*
*  LINE    (input) CHARACTER*80
*          The input line that requested this routine.  The first six
*          characters contain either the name of a subroutine or a
*          generic path name.  The remaining characters may be used to
*          specify the individual routines to be timed.  See ATIMIN for
*          a full description of the format of the input line.
*
*  NM      (input) INTEGER
*          The number of values of M and N contained in the vectors
*          MVAL and NVAL.  The matrix sizes are used in pairs (M,N).
*
*  MVAL    (input) INTEGER array, dimension (NM)
*          The values of the matrix row dimension M.
*
*  NVAL    (input) INTEGER array, dimension (NM)
*          The values of the matrix column dimension N.
*
*  NNB     (input) INTEGER
*          The number of values of NB and NX contained in the
*          vectors NBVAL and NXVAL.  The blocking parameters are used
*          in pairs (NB,NX).
*
*  NBVAL   (input) INTEGER array, dimension (NNB)
*          The values of the blocksize NB.
*
*  NXVAL   (input) INTEGER array, dimension (NNB)
*          The values of the crossover point NX.
*
*  NLDA    (input) INTEGER
*          The number of values of LDA contained in the vector LDAVAL.
*
*  LDAVAL  (input) INTEGER array, dimension (NLDA)
*          The values of the leading dimension of the array A.
*
*  TIMMIN  (input) DOUBLE PRECISION
*          The minimum time a subroutine will be timed.
*
*  A       (workspace) DOUBLE PRECISION array, dimension (LDAMAX*NMAX)
*          where LDAMAX and NMAX are the maximum values of LDA and N.
*
*  COPYA   (workspace) DOUBLE PRECISION array, dimension (LDAMAX*NMAX)
*
*  TAU     (workspace) DOUBLE PRECISION array, dimension (MINMN)
*
*  WORK    (workspace) DOUBLE PRECISION array, dimension (3*NMAX)
*
*  IWORK   (workspace) INTEGER array, dimension (2*NMAX)
*
*  RESLTS  (workspace) DOUBLE PRECISION array, dimension
*                      (LDR1,LDR2,NLDA)
*          The timing results for each subroutine over the relevant
*          values of MODE, (M,N), and LDA.
*
*  LDR1    (input) INTEGER
*          The first dimension of RESLTS.  LDR1 >= max(1,NM).
*
*  LDR2    (input) INTEGER
*          The second dimension of RESLTS.  LDR2 >= max(1,NM).
*
*  NOUT    (input) INTEGER
*          The unit number for output.
*
*  =====================================================================
*
*     .. Parameters ..
*
*
      INTEGER            NSUBS, NMODE
      PARAMETER          ( NSUBS = 1, NMODE = 2 )
      DOUBLE PRECISION   ONE
      PARAMETER          ( ONE = 1.0D+0 )
*     ..
*     .. Local Scalars ..
      CHARACTER*3        PATH
      CHARACTER*6        CNAME
      INTEGER            I, IC, ICL, ILDA, IM, IMODE, INB, INFO, LDA,
     $                   LW, M, MINMN, MODE, N, NB, NX
      DOUBLE PRECISION   COND, DMAX, OPS, S1, S2, TIME, UNTIME
*     ..
*     .. Local Arrays ..
      LOGICAL            TIMSUB( NSUBS )
      CHARACTER*6        SUBNAM( NSUBS )
      INTEGER            ISEED( 4 ), MODES( NMODE )
*     ..
*     .. External Functions ..
      DOUBLE PRECISION   DLAMCH, DMFLOP, DOPLA, DSECND
      EXTERNAL           DLAMCH, DMFLOP, DOPLA, DSECND
*     ..
*     .. External Subroutines ..
      EXTERNAL           ATIMCK, ATIMIN, DGEQP3, DLACPY, DLATMS, DPRTB4,
     $                   ICOPY, XLAENV
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          DBLE, MAX, MIN
*     ..
*     .. Data statements ..
      DATA               SUBNAM / 'DGEQP3' /
      DATA               MODES / 2, 3 /
      DATA               ISEED / 0, 0, 0, 1 /
*     ..
*     .. Executable Statements ..
*
*     Extract the timing request from the input line.
*
      PATH( 1: 1 ) = 'Double precision'
      PATH( 2: 3 ) = 'QP'
      CALL ATIMIN( PATH, LINE, NSUBS, SUBNAM, TIMSUB, NOUT, INFO )
      IF( .NOT.TIMSUB( 1 ) .OR. INFO.NE.0 )
     $   GO TO 90
*
*     Check that M <= LDA for the input values.
*
      CNAME = LINE( 1: 6 )
      CALL ATIMCK( 1, CNAME, NM, MVAL, NLDA, LDAVAL, NOUT, INFO )
      IF( INFO.GT.0 ) THEN
         WRITE( NOUT, FMT = 9996 )CNAME
         GO TO 90
      END IF
*
*     Set the condition number and scaling factor for the matrices
*     to be generated.
*
      DMAX = ONE
      COND = ONE / DLAMCH( 'Precision' )
*
*     Do for each type of matrix:
*
      DO 80 IMODE = 1, NMODE
         MODE = MODES( IMODE )
*
*
*        *****************
*        * Timing xGEQP3 *
*        *****************
*
*        Do for each value of LDA:
*
         DO 60 ILDA = 1, NLDA
            LDA = LDAVAL( ILDA )
*
*           Do for each pair of values (M,N):
*
            DO 50 IM = 1, NM
               M = MVAL( IM )
               N = NVAL( IM )
               MINMN = MIN( M, N )
*
*              Generate a test matrix of size m by n using the
*              singular value distribution indicated by MODE.
*
               CALL DLATMS( M, N, 'Uniform', ISEED, 'Nonsymm', TAU,
     $                      MODE, COND, DMAX, M, N, 'No packing', COPYA,
     $                      LDA, WORK, INFO )
*
*              Do for each pair of values (NB,NX) in NBVAL and NXVAL:
*
               DO 40 INB = 1, NNB
                  NB = NBVAL( INB )
                  CALL XLAENV( 1, NB )
                  NX = NXVAL( INB )
                  CALL XLAENV( 3, NX )
*
*
*                 DGEQP3
*
                  LW = MAX( 1, 2*N+( N+1 )*NB )
                  DO 10 I = 1, N
                     IWORK( N+I ) = 0
   10             CONTINUE
*
                  CALL DLACPY( 'All', M, N, COPYA, LDA, A, LDA )
                  CALL ICOPY( N, IWORK( N+1 ), 1, IWORK, 1 )
                  IC = 0
                  S1 = DSECND( )
   20             CONTINUE
                  CALL DGEQP3( M, N, A, LDA, IWORK, TAU, WORK, LW,
     $                         INFO )
                  S2 = DSECND( )
*
                  IF( INFO.NE.0 ) THEN
                     WRITE( *, FMT = * )'>>>Warning: INFO returned by ',
     $                  'DGEQPX is:', INFO
                     INFO = 0
                  END IF
*
                  TIME = S2 - S1
                  IC = IC + 1
                  IF( TIME.LT.TIMMIN ) THEN
                     CALL DLACPY( 'All', M, N, COPYA, LDA, A, LDA )
                     CALL ICOPY( N, IWORK( N+1 ), 1, IWORK, 1 )
                     GO TO 20
                  END IF
*
*                 Subtract the time used in DLACPY.
*
                  ICL = 1
                  S1 = DSECND( )
   30             CONTINUE
                  S2 = DSECND( )
                  UNTIME = S2 - S1
                  ICL = ICL + 1
                  IF( ICL.LE.IC ) THEN
                     CALL DLACPY( 'All', M, N, COPYA, LDA, A, LDA )
                     CALL ICOPY( N, IWORK( N+1 ), 1, IWORK, 1 )
                     GO TO 30
                  END IF
*
*                 The number of flops of xGEQP3 is approximately the
*                 the number of flops of xGEQPF.
*
                  TIME = ( TIME-UNTIME ) / DBLE( IC )
*
                  OPS = DOPLA( 'DGEQPF', M, N, 0, 0, NB )
                  RESLTS( INB, IM, ILDA ) = DMFLOP( OPS, TIME, INFO )
*
   40          CONTINUE
   50       CONTINUE
   60    CONTINUE
*
*        Print the results for each matrix type.
*
         WRITE( NOUT, FMT = 9999 )SUBNAM( 1 )
         WRITE( NOUT, FMT = 9998 )IMODE
         DO 70 I = 1, NLDA
            WRITE( NOUT, FMT = 9997 )I, LDAVAL( I )
   70    CONTINUE
         WRITE( NOUT, FMT = * )
         CALL DPRTB4( '(  NB,  NX)', 'M', 'N', NNB, NBVAL, NXVAL, NM,
     $                MVAL, NVAL, NLDA, RESLTS( 1, 1, 1 ), LDR1, LDR2,
     $                NOUT )
*
   80 CONTINUE
*
 9999 FORMAT( / ' *** Speed of ', A6, ' in megaflops ***' )
 9998 FORMAT( 5X, 'type of matrix:', I4 )
 9997 FORMAT( 5X, 'line ', I4, ' with LDA = ', I4 )
 9996 FORMAT( 1X, A6, ' timing run not attempted', / )
*
   90 CONTINUE
      RETURN
*
*     End of DTIMQ3
*
      END
      SUBROUTINE DTIMQL( LINE, NM, MVAL, NVAL, NK, KVAL, NNB, NBVAL,
     $                   NXVAL, NLDA, LDAVAL, TIMMIN, A, TAU, B, WORK,
     $                   RESLTS, LDR1, LDR2, LDR3, NOUT )
*
*  -- LAPACK timing routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     March 31, 1993
*
*     .. Scalar Arguments ..
      CHARACTER*80       LINE
      INTEGER            LDR1, LDR2, LDR3, NK, NLDA, NM, NNB, NOUT
      DOUBLE PRECISION   TIMMIN
*     ..
*     .. Array Arguments ..
      INTEGER            KVAL( * ), LDAVAL( * ), MVAL( * ), NBVAL( * ),
     $                   NVAL( * ), NXVAL( * )
      DOUBLE PRECISION   A( * ), B( * ), RESLTS( LDR1, LDR2, LDR3, * ),
     $                   TAU( * ), WORK( * )
*     ..
*
*  Purpose
*  =======
*
*  DTIMQL times the LAPACK routines to perform the QL factorization of
*  a DOUBLE PRECISION general matrix.
*
*  Arguments
*  =========
*
*  LINE    (input) CHARACTER*80
*          The input line that requested this routine.  The first six
*          characters contain either the name of a subroutine or a
*          generic path name.  The remaining characters may be used to
*          specify the individual routines to be timed.  See ATIMIN for
*          a full description of the format of the input line.
*
*  NM      (input) INTEGER
*          The number of values of M and N contained in the vectors
*          MVAL and NVAL.  The matrix sizes are used in pairs (M,N).
*
*  MVAL    (input) INTEGER array, dimension (NM)
*          The values of the matrix row dimension M.
*
*  NVAL    (input) INTEGER array, dimension (NM)
*          The values of the matrix column dimension N.
*
*  NK      (input) INTEGER
*          The number of values of K in the vector KVAL.
*
*  KVAL    (input) INTEGER array, dimension (NK)
*          The values of the matrix dimension K, used in DORMQL.
*
*  NNB     (input) INTEGER
*          The number of values of NB and NX contained in the
*          vectors NBVAL and NXVAL.  The blocking parameters are used
*          in pairs (NB,NX).
*
*  NBVAL   (input) INTEGER array, dimension (NNB)
*          The values of the blocksize NB.
*
*  NXVAL   (input) INTEGER array, dimension (NNB)
*          The values of the crossover point NX.
*
*  NLDA    (input) INTEGER
*          The number of values of LDA contained in the vector LDAVAL.
*
*  LDAVAL  (input) INTEGER array, dimension (NLDA)
*          The values of the leading dimension of the array A.
*
*  TIMMIN  (input) DOUBLE PRECISION
*          The minimum time a subroutine will be timed.
*
*  A       (workspace) DOUBLE PRECISION array, dimension (LDAMAX*NMAX)
*          where LDAMAX and NMAX are the maximum values of LDA and N.
*
*  TAU     (workspace) DOUBLE PRECISION array, dimension (min(M,N))
*
*  B       (workspace) DOUBLE PRECISION array, dimension (LDAMAX*NMAX)
*
*  WORK    (workspace) DOUBLE PRECISION array, dimension (LDAMAX*NBMAX)
*          where NBMAX is the maximum value of NB.
*
*  RESLTS  (workspace) DOUBLE PRECISION array, dimension
*                      (LDR1,LDR2,LDR3,2*NK)
*          The timing results for each subroutine over the relevant
*          values of (M,N), (NB,NX), and LDA.
*
*  LDR1    (input) INTEGER
*          The first dimension of RESLTS.  LDR1 >= max(1,NNB).
*
*  LDR2    (input) INTEGER
*          The second dimension of RESLTS.  LDR2 >= max(1,NM).
*
*  LDR3    (input) INTEGER
*          The third dimension of RESLTS.  LDR3 >= max(1,NLDA).
*
*  NOUT    (input) INTEGER
*          The unit number for output.
*
*  Internal Parameters
*  ===================
*
*  MODE    INTEGER
*          The matrix type.  MODE = 3 is a geometric distribution of
*          eigenvalues.  See DLATMS for further details.
*
*  COND    DOUBLE PRECISION
*          The condition number of the matrix.  The singular values are
*          set to values from DMAX to DMAX/COND.
*
*  DMAX    DOUBLE PRECISION
*          The magnitude of the largest singular value.
*
*  =====================================================================
*
*     .. Parameters ..
      INTEGER            NSUBS
      PARAMETER          ( NSUBS = 3 )
      INTEGER            MODE
      DOUBLE PRECISION   COND, DMAX
      PARAMETER          ( MODE = 3, COND = 100.0D0, DMAX = 1.0D0 )
*     ..
*     .. Local Scalars ..
      CHARACTER          LABM, SIDE, TRANS
      CHARACTER*3        PATH
      CHARACTER*6        CNAME
      INTEGER            I, I4, IC, ICL, IK, ILDA, IM, IMX, INB, INFO,
     $                   ISIDE, ISUB, ITOFF, ITRAN, K, K1, LDA, LW, M,
     $                   M1, MINMN, N, N1, NB, NX
      DOUBLE PRECISION   OPS, S1, S2, TIME, UNTIME
*     ..
*     .. Local Arrays ..
      LOGICAL            TIMSUB( NSUBS )
      CHARACTER          SIDES( 2 ), TRANSS( 2 )
      CHARACTER*6        SUBNAM( NSUBS )
      INTEGER            ISEED( 4 ), MUSE( 12 ), NUSE( 12 ), RESEED( 4 )
*     ..
*     .. External Functions ..
      DOUBLE PRECISION   DMFLOP, DOPLA, DSECND
      EXTERNAL           DMFLOP, DOPLA, DSECND
*     ..
*     .. External Subroutines ..
      EXTERNAL           ATIMCK, ATIMIN, DGEQLF, DLACPY, DLATMS, DORGQL,
     $                   DORMQL, DPRTB4, DPRTB5, DTIMMG, ICOPY, XLAENV
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          DBLE, MAX, MIN
*     ..
*     .. Data statements ..
      DATA               SUBNAM / 'DGEQLF', 'DORGQL', 'DORMQL' /
      DATA               SIDES / 'L', 'R' / , TRANSS / 'N', 'T' /
      DATA               ISEED / 0, 0, 0, 1 /
*     ..
*     .. Executable Statements ..
*
*     Extract the timing request from the input line.
*
      PATH( 1: 1 ) = 'Double precision'
      PATH( 2: 3 ) = 'QL'
      CALL ATIMIN( PATH, LINE, NSUBS, SUBNAM, TIMSUB, NOUT, INFO )
      IF( INFO.NE.0 )
     $   GO TO 230
*
*     Check that M <= LDA for the input values.
*
      CNAME = LINE( 1: 6 )
      CALL ATIMCK( 1, CNAME, NM, MVAL, NLDA, LDAVAL, NOUT, INFO )
      IF( INFO.GT.0 ) THEN
         WRITE( NOUT, FMT = 9999 )CNAME
         GO TO 230
      END IF
*
*     Do for each pair of values (M,N):
*
      DO 70 IM = 1, NM
         M = MVAL( IM )
         N = NVAL( IM )
         MINMN = MIN( M, N )
         CALL ICOPY( 4, ISEED, 1, RESEED, 1 )
*
*        Do for each value of LDA:
*
         DO 60 ILDA = 1, NLDA
            LDA = LDAVAL( ILDA )
*
*           Do for each pair of values (NB, NX) in NBVAL and NXVAL.
*
            DO 50 INB = 1, NNB
               NB = NBVAL( INB )
               CALL XLAENV( 1, NB )
               NX = NXVAL( INB )
               CALL XLAENV( 3, NX )
               LW = MAX( 1, N*MAX( 1, NB ) )
*
*              Generate a test matrix of size M by N.
*
               CALL ICOPY( 4, RESEED, 1, ISEED, 1 )
               CALL DLATMS( M, N, 'Uniform', ISEED, 'Nonsymm', TAU,
     $                      MODE, COND, DMAX, M, N, 'No packing', B,
     $                      LDA, WORK, INFO )
*
               IF( TIMSUB( 1 ) ) THEN
*
*                 DGEQLF:  QL factorization
*
                  CALL DLACPY( 'Full', M, N, B, LDA, A, LDA )
                  IC = 0
                  S1 = DSECND( )
   10             CONTINUE
                  CALL DGEQLF( M, N, A, LDA, TAU, WORK, LW, INFO )
                  S2 = DSECND( )
                  TIME = S2 - S1
                  IC = IC + 1
                  IF( TIME.LT.TIMMIN ) THEN
                     CALL DLACPY( 'Full', M, N, B, LDA, A, LDA )
                     GO TO 10
                  END IF
*
*                 Subtract the time used in DLACPY.
*
                  ICL = 1
                  S1 = DSECND( )
   20             CONTINUE
                  S2 = DSECND( )
                  UNTIME = S2 - S1
                  ICL = ICL + 1
                  IF( ICL.LE.IC ) THEN
                     CALL DLACPY( 'Full', M, N, A, LDA, B, LDA )
                     GO TO 20
                  END IF
*
                  TIME = ( TIME-UNTIME ) / DBLE( IC )
                  OPS = DOPLA( 'DGEQLF', M, N, 0, 0, NB )
                  RESLTS( INB, IM, ILDA, 1 ) = DMFLOP( OPS, TIME, INFO )
               ELSE
*
*                 If DGEQLF was not timed, generate a matrix and factor
*                 it using DGEQLF anyway so that the factored form of
*                 the matrix can be used in timing the other routines.
*
                  CALL DLACPY( 'Full', M, N, B, LDA, A, LDA )
                  CALL DGEQLF( M, N, A, LDA, TAU, WORK, LW, INFO )
               END IF
*
               IF( TIMSUB( 2 ) ) THEN
*
*                 DORGQL:  Generate orthogonal matrix Q from the QL
*                 factorization
*
                  CALL DLACPY( 'Full', M, MINMN, A, LDA, B, LDA )
                  IC = 0
                  S1 = DSECND( )
   30             CONTINUE
                  CALL DORGQL( M, MINMN, MINMN, B, LDA, TAU, WORK, LW,
     $                         INFO )
                  S2 = DSECND( )
                  TIME = S2 - S1
                  IC = IC + 1
                  IF( TIME.LT.TIMMIN ) THEN
                     CALL DLACPY( 'Full', M, MINMN, A, LDA, B, LDA )
                     GO TO 30
                  END IF
*
*                 Subtract the time used in DLACPY.
*
                  ICL = 1
                  S1 = DSECND( )
   40             CONTINUE
                  S2 = DSECND( )
                  UNTIME = S2 - S1
                  ICL = ICL + 1
                  IF( ICL.LE.IC ) THEN
                     CALL DLACPY( 'Full', M, MINMN, A, LDA, B, LDA )
                     GO TO 40
                  END IF
*
                  TIME = ( TIME-UNTIME ) / DBLE( IC )
                  OPS = DOPLA( 'DORGQL', M, MINMN, MINMN, 0, NB )
                  RESLTS( INB, IM, ILDA, 2 ) = DMFLOP( OPS, TIME, INFO )
               END IF
*
   50       CONTINUE
   60    CONTINUE
   70 CONTINUE
*
*     Print tables of results
*
      DO 90 ISUB = 1, NSUBS - 1
         IF( .NOT.TIMSUB( ISUB ) )
     $      GO TO 90
         WRITE( NOUT, FMT = 9998 )SUBNAM( ISUB )
         IF( NLDA.GT.1 ) THEN
            DO 80 I = 1, NLDA
               WRITE( NOUT, FMT = 9997 )I, LDAVAL( I )
   80       CONTINUE
         END IF
         WRITE( NOUT, FMT = * )
         IF( ISUB.EQ.2 )
     $      WRITE( NOUT, FMT = 9996 )
         CALL DPRTB4( '(  NB,  NX)', 'M', 'N', NNB, NBVAL, NXVAL, NM,
     $                MVAL, NVAL, NLDA, RESLTS( 1, 1, 1, ISUB ), LDR1,
     $                LDR2, NOUT )
   90 CONTINUE
*
*     Time DORMQL separately.  Here the starting matrix is M by N, and
*     K is the free dimension of the matrix multiplied by Q.
*
      IF( TIMSUB( 3 ) ) THEN
*
*        Check that K <= LDA for the input values.
*
         CALL ATIMCK( 3, CNAME, NK, KVAL, NLDA, LDAVAL, NOUT, INFO )
         IF( INFO.GT.0 ) THEN
            WRITE( NOUT, FMT = 9999 )SUBNAM( 3 )
            GO TO 230
         END IF
*
*        Use only the pairs (M,N) where M >= N.
*
         IMX = 0
         DO 100 IM = 1, NM
            IF( MVAL( IM ).GE.NVAL( IM ) ) THEN
               IMX = IMX + 1
               MUSE( IMX ) = MVAL( IM )
               NUSE( IMX ) = NVAL( IM )
            END IF
  100    CONTINUE
*
*        DORMQL:  Multiply by Q stored as a product of elementary
*        transformations
*
*        Do for each pair of values (M,N):
*
         DO 180 IM = 1, IMX
            M = MUSE( IM )
            N = NUSE( IM )
*
*           Do for each value of LDA:
*
            DO 170 ILDA = 1, NLDA
               LDA = LDAVAL( ILDA )
*
*              Generate an M by N matrix and form its QL decomposition.
*
               CALL DLATMS( M, N, 'Uniform', ISEED, 'Nonsymm', TAU,
     $                      MODE, COND, DMAX, M, N, 'No packing', A,
     $                      LDA, WORK, INFO )
               LW = MAX( 1, N*MAX( 1, NB ) )
               CALL DGEQLF( M, N, A, LDA, TAU, WORK, LW, INFO )
*
*              Do first for SIDE = 'L', then for SIDE = 'R'
*
               I4 = 0
               DO 160 ISIDE = 1, 2
                  SIDE = SIDES( ISIDE )
*
*                 Do for each pair of values (NB, NX) in NBVAL and
*                 NXVAL.
*
                  DO 150 INB = 1, NNB
                     NB = NBVAL( INB )
                     CALL XLAENV( 1, NB )
                     NX = NXVAL( INB )
                     CALL XLAENV( 3, NX )
*
*                    Do for each value of K in KVAL
*
                     DO 140 IK = 1, NK
                        K = KVAL( IK )
*
*                       Sort out which variable is which
*
                        IF( ISIDE.EQ.1 ) THEN
                           M1 = M
                           K1 = N
                           N1 = K
                           LW = MAX( 1, N1*MAX( 1, NB ) )
                        ELSE
                           N1 = M
                           K1 = N
                           M1 = K
                           LW = MAX( 1, M1*MAX( 1, NB ) )
                        END IF
*
*                       Do first for TRANS = 'N', then for TRANS = 'T'
*
                        ITOFF = 0
                        DO 130 ITRAN = 1, 2
                           TRANS = TRANSS( ITRAN )
                           CALL DTIMMG( 0, M1, N1, B, LDA, 0, 0 )
                           IC = 0
                           S1 = DSECND( )
  110                      CONTINUE
                           CALL DORMQL( SIDE, TRANS, M1, N1, K1, A, LDA,
     $                                  TAU, B, LDA, WORK, LW, INFO )
                           S2 = DSECND( )
                           TIME = S2 - S1
                           IC = IC + 1
                           IF( TIME.LT.TIMMIN ) THEN
                              CALL DTIMMG( 0, M1, N1, B, LDA, 0, 0 )
                              GO TO 110
                           END IF
*
*                          Subtract the time used in DTIMMG.
*
                           ICL = 1
                           S1 = DSECND( )
  120                      CONTINUE
                           S2 = DSECND( )
                           UNTIME = S2 - S1
                           ICL = ICL + 1
                           IF( ICL.LE.IC ) THEN
                              CALL DTIMMG( 0, M1, N1, B, LDA, 0, 0 )
                              GO TO 120
                           END IF
*
                           TIME = ( TIME-UNTIME ) / DBLE( IC )
                           OPS = DOPLA( 'DORMQL', M1, N1, K1, ISIDE-1,
     $                           NB )
                           RESLTS( INB, IM, ILDA,
     $                        I4+ITOFF+IK ) = DMFLOP( OPS, TIME, INFO )
                           ITOFF = NK
  130                   CONTINUE
  140                CONTINUE
  150             CONTINUE
                  I4 = 2*NK
  160          CONTINUE
  170       CONTINUE
  180    CONTINUE
*
*        Print tables of results
*
         ISUB = 3
         I4 = 1
         IF( IMX.GE.1 ) THEN
            DO 220 ISIDE = 1, 2
               SIDE = SIDES( ISIDE )
               IF( ISIDE.EQ.1 ) THEN
                  WRITE( NOUT, FMT = 9998 )SUBNAM( ISUB )
                  IF( NLDA.GT.1 ) THEN
                     DO 190 I = 1, NLDA
                        WRITE( NOUT, FMT = 9997 )I, LDAVAL( I )
  190                CONTINUE
                  END IF
               END IF
               DO 210 ITRAN = 1, 2
                  TRANS = TRANSS( ITRAN )
                  DO 200 IK = 1, NK
                     IF( ISIDE.EQ.1 ) THEN
                        N = KVAL( IK )
                        WRITE( NOUT, FMT = 9995 )SUBNAM( ISUB ), SIDE,
     $                     TRANS, 'N', N
                        LABM = 'M'
                     ELSE
                        M = KVAL( IK )
                        WRITE( NOUT, FMT = 9995 )SUBNAM( ISUB ), SIDE,
     $                     TRANS, 'M', M
                        LABM = 'N'
                     END IF
                     CALL DPRTB5( 'NB', LABM, 'K', NNB, NBVAL, IMX,
     $                            MUSE, NUSE, NLDA,
     $                            RESLTS( 1, 1, 1, I4 ), LDR1, LDR2,
     $                            NOUT )
                     I4 = I4 + 1
  200             CONTINUE
  210          CONTINUE
  220       CONTINUE
         ELSE
            WRITE( NOUT, FMT = 9994 )SUBNAM( ISUB )
         END IF
      END IF
  230 CONTINUE
 9999 FORMAT( 1X, A6, ' timing run not attempted', / )
 9998 FORMAT( / ' *** Speed of ', A6, ' in megaflops ***' )
 9997 FORMAT( 5X, 'line ', I2, ' with LDA = ', I5 )
 9996 FORMAT( 5X, 'K = min(M,N)', / )
 9995 FORMAT( / 5X, A6, ' with SIDE = ''', A1, ''', TRANS = ''', A1,
     $      ''', ', A1, ' =', I6, / )
 9994 FORMAT( ' *** No pairs (M,N) found with M >= N:  ', A6,
     $      ' not timed' )
      RETURN
*
*     End of DTIMQL
*
      END
      SUBROUTINE DTIMQP( LINE, NM, MVAL, NVAL, NLDA, LDAVAL, TIMMIN, A,
     $                   COPYA, TAU, WORK, IWORK, RESLTS, LDR1, LDR2,
     $                   NOUT )
*
*  -- LAPACK timing routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     June 30, 1999
*
*     .. Scalar Arguments ..
      CHARACTER*80       LINE
      INTEGER            LDR1, LDR2, NLDA, NM, NOUT
      DOUBLE PRECISION   TIMMIN
*     ..
*     .. Array Arguments ..
      INTEGER            IWORK( * ), LDAVAL( * ), MVAL( * ), NVAL( * )
      DOUBLE PRECISION   A( * ), COPYA( * ), RESLTS( LDR1, LDR2, * ),
     $                   TAU( * ), WORK( * )
*     ..
*
*  Purpose
*  =======
*
*  DTIMQP times the LAPACK routines to perform the QR factorization with
*  column pivoting of a DOUBLE PRECISION general matrix.
*
*  Two matrix types may be used for timing.  The number of types is
*  set in the parameter NMODE and the matrix types are set in the vector
*  MODES, using the following key:
*     2.  BREAK1    D(1:N-1)=1 and D(N)=1.0/COND in DLATMS
*     3.  GEOM      D(I)=COND**(-(I-1)/(N-1)) in DLATMS
*  These numbers are chosen to correspond with the matrix types in the
*  test code.
*
*  Arguments
*  =========
*
*  LINE    (input) CHARACTER*80
*          The input line that requested this routine.  The first six
*          characters contain either the name of a subroutine or a
*          generic path name.  The remaining characters may be used to
*          specify the individual routines to be timed.  See ATIMIN for
*          a full description of the format of the input line.
*
*  NM      (input) INTEGER
*          The number of values of M and N contained in the vectors
*          MVAL and NVAL.  The matrix sizes are used in pairs (M,N).
*
*  MVAL    (input) INTEGER array, dimension (NM)
*          The values of the matrix row dimension M.
*
*  NVAL    (input) INTEGER array, dimension (NM)
*          The values of the matrix column dimension N.
*
*  NLDA    (input) INTEGER
*          The number of values of LDA contained in the vector LDAVAL.
*
*  LDAVAL  (input) INTEGER array, dimension (NLDA)
*          The values of the leading dimension of the array A.
*
*  TIMMIN  (input) DOUBLE PRECISION
*          The minimum time a subroutine will be timed.
*
*  A       (workspace) DOUBLE PRECISION array, dimension (LDAMAX*NMAX)
*          where LDAMAX and NMAX are the maximum values of LDA and N.
*
*  COPYA   (workspace) DOUBLE PRECISION array, dimension (LDAMAX*NMAX)
*
*  TAU     (workspace) DOUBLE PRECISION array, dimension (min(M,N))
*
*  WORK    (workspace) DOUBLE PRECISION array, dimension (3*NMAX)
*
*  IWORK   (workspace) INTEGER array, dimension (2*NMAX)
*
*  RESLTS  (workspace) DOUBLE PRECISION array, dimension
*                      (LDR1,LDR2,NLDA)
*          The timing results for each subroutine over the relevant
*          values of MODE, (M,N), and LDA.
*
*  LDR1    (input) INTEGER
*          The first dimension of RESLTS.  LDR1 >= max(1,NM).
*
*  LDR2    (input) INTEGER
*          The second dimension of RESLTS.  LDR2 >= max(1,NM).
*
*  NOUT    (input) INTEGER
*          The unit number for output.
*
*  =====================================================================
*
*     .. Parameters ..
      INTEGER            NSUBS, NMODE
      PARAMETER          ( NSUBS = 1, NMODE = 2 )
      DOUBLE PRECISION   ONE
      PARAMETER          ( ONE = 1.0D+0 )
*     ..
*     .. Local Scalars ..
      CHARACTER*3        PATH
      CHARACTER*6        CNAME
      INTEGER            I, IC, ICL, ILDA, IM, IMODE, INFO, LDA, M,
     $                   MINMN, MODE, N
      DOUBLE PRECISION   COND, DMAX, OPS, S1, S2, TIME, UNTIME
*     ..
*     .. Local Arrays ..
      LOGICAL            TIMSUB( NSUBS )
      CHARACTER*6        SUBNAM( NSUBS )
      INTEGER            ISEED( 4 ), MODES( NMODE )
*     ..
*     .. External Functions ..
      DOUBLE PRECISION   DLAMCH, DMFLOP, DOPLA, DSECND
      EXTERNAL           DLAMCH, DMFLOP, DOPLA, DSECND
*     ..
*     .. External Subroutines ..
      EXTERNAL           ATIMCK, ATIMIN, DGEQPF, DLACPY, DLATMS, DPRTB5,
     $                   ICOPY
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          DBLE, MIN
*     ..
*     .. Data statements ..
      DATA               SUBNAM / 'DGEQPF' /
      DATA               MODES / 2, 3 /
      DATA               ISEED / 0, 0, 0, 1 /
*     ..
*     .. Executable Statements ..
*
*     Extract the timing request from the input line.
*
      PATH( 1: 1 ) = 'Double precision'
      PATH( 2: 3 ) = 'QP'
      CALL ATIMIN( PATH, LINE, NSUBS, SUBNAM, TIMSUB, NOUT, INFO )
      IF( .NOT.TIMSUB( 1 ) .OR. INFO.NE.0 )
     $   GO TO 80
*
*     Check that M <= LDA for the input values.
*
      CNAME = LINE( 1: 6 )
      CALL ATIMCK( 1, CNAME, NM, MVAL, NLDA, LDAVAL, NOUT, INFO )
      IF( INFO.GT.0 ) THEN
         WRITE( NOUT, FMT = 9999 )CNAME
         GO TO 80
      END IF
*
*     Set the condition number and scaling factor for the matrices
*     to be generated.
*
      DMAX = ONE
      COND = ONE / DLAMCH( 'Precision' )
*
*     Do for each pair of values (M,N):
*
      DO 60 IM = 1, NM
         M = MVAL( IM )
         N = NVAL( IM )
         MINMN = MIN( M, N )
*
*        Do for each value of LDA:
*
         DO 50 ILDA = 1, NLDA
            LDA = LDAVAL( ILDA )
            DO 40 IMODE = 1, NMODE
               MODE = MODES( IMODE )
*
*              Generate a test matrix of size m by n using the
*              singular value distribution indicated by MODE.
*
               DO 10 I = 1, N
                  IWORK( N+I ) = 0
   10          CONTINUE
               CALL DLATMS( M, N, 'Uniform', ISEED, 'Nonsymm', TAU,
     $                      MODE, COND, DMAX, M, N, 'No packing', COPYA,
     $                      LDA, WORK, INFO )
*
*              DGEQPF:  QR factorization with column pivoting
*
               CALL DLACPY( 'All', M, N, COPYA, LDA, A, LDA )
               CALL ICOPY( N, IWORK( N+1 ), 1, IWORK, 1 )
               IC = 0
               S1 = DSECND( )
   20          CONTINUE
               CALL DGEQPF( M, N, A, LDA, IWORK, TAU, WORK, INFO )
               S2 = DSECND( )
               TIME = S2 - S1
               IC = IC + 1
               IF( TIME.LT.TIMMIN ) THEN
                  CALL DLACPY( 'All', M, N, COPYA, LDA, A, LDA )
                  CALL ICOPY( N, IWORK( N+1 ), 1, IWORK, 1 )
                  GO TO 20
               END IF
*
*              Subtract the time used in DLACPY and ICOPY.
*
               ICL = 1
               S1 = DSECND( )
   30          CONTINUE
               S2 = DSECND( )
               UNTIME = S2 - S1
               ICL = ICL + 1
               IF( ICL.LE.IC ) THEN
                  CALL DLACPY( 'All', M, N, COPYA, LDA, A, LDA )
                  CALL ICOPY( N, IWORK( N+1 ), 1, IWORK, 1 )
                  GO TO 30
               END IF
*
               TIME = ( TIME-UNTIME ) / DBLE( IC )
               OPS = DOPLA( 'DGEQPF', M, N, 0, 0, 1 )
               RESLTS( IMODE, IM, ILDA ) = DMFLOP( OPS, TIME, INFO )
*
   40       CONTINUE
   50    CONTINUE
   60 CONTINUE
*
*     Print tables of results
*
      WRITE( NOUT, FMT = 9998 )SUBNAM( 1 )
      IF( NLDA.GT.1 ) THEN
         DO 70 I = 1, NLDA
            WRITE( NOUT, FMT = 9997 )I, LDAVAL( I )
   70    CONTINUE
      END IF
      WRITE( NOUT, FMT = * )
      CALL DPRTB5( 'Type', 'M', 'N', NMODE, MODES, NM, MVAL, NVAL, NLDA,
     $             RESLTS, LDR1, LDR2, NOUT )
   80 CONTINUE
 9999 FORMAT( 1X, A6, ' timing run not attempted', / )
 9998 FORMAT( / ' *** Speed of ', A6, ' in megaflops ***' )
 9997 FORMAT( 5X, 'line ', I2, ' with LDA = ', I5 )
      RETURN
*
*     End of DTIMQP
*
      END
      SUBROUTINE DTIMQR( LINE, NM, MVAL, NVAL, NK, KVAL, NNB, NBVAL,
     $                   NXVAL, NLDA, LDAVAL, TIMMIN, A, TAU, B, WORK,
     $                   RESLTS, LDR1, LDR2, LDR3, NOUT )
*
*  -- LAPACK timing routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     March 31, 1993
*
*     .. Scalar Arguments ..
      CHARACTER*80       LINE
      INTEGER            LDR1, LDR2, LDR3, NK, NLDA, NM, NNB, NOUT
      DOUBLE PRECISION   TIMMIN
*     ..
*     .. Array Arguments ..
      INTEGER            KVAL( * ), LDAVAL( * ), MVAL( * ), NBVAL( * ),
     $                   NVAL( * ), NXVAL( * )
      DOUBLE PRECISION   A( * ), B( * ), RESLTS( LDR1, LDR2, LDR3, * ),
     $                   TAU( * ), WORK( * )
*     ..
*
*  Purpose
*  =======
*
*  DTIMQR times the LAPACK routines to perform the QR factorization of
*  a DOUBLE PRECISION general matrix.
*
*  Arguments
*  =========
*
*  LINE    (input) CHARACTER*80
*          The input line that requested this routine.  The first six
*          characters contain either the name of a subroutine or a
*          generic path name.  The remaining characters may be used to
*          specify the individual routines to be timed.  See ATIMIN for
*          a full description of the format of the input line.
*
*  NM      (input) INTEGER
*          The number of values of M and N contained in the vectors
*          MVAL and NVAL.  The matrix sizes are used in pairs (M,N).
*
*  MVAL    (input) INTEGER array, dimension (NM)
*          The values of the matrix row dimension M.
*
*  NVAL    (input) INTEGER array, dimension (NM)
*          The values of the matrix column dimension N.
*
*  NK      (input) INTEGER
*          The number of values of K in the vector KVAL.
*
*  KVAL    (input) INTEGER array, dimension (NK)
*          The values of the matrix dimension K, used in DORMQR.
*
*  NNB     (input) INTEGER
*          The number of values of NB and NX contained in the
*          vectors NBVAL and NXVAL.  The blocking parameters are used
*          in pairs (NB,NX).
*
*  NBVAL   (input) INTEGER array, dimension (NNB)
*          The values of the blocksize NB.
*
*  NXVAL   (input) INTEGER array, dimension (NNB)
*          The values of the crossover point NX.
*
*  NLDA    (input) INTEGER
*          The number of values of LDA contained in the vector LDAVAL.
*
*  LDAVAL  (input) INTEGER array, dimension (NLDA)
*          The values of the leading dimension of the array A.
*
*  TIMMIN  (input) DOUBLE PRECISION
*          The minimum time a subroutine will be timed.
*
*  A       (workspace) DOUBLE PRECISION array, dimension (LDAMAX*NMAX)
*          where LDAMAX and NMAX are the maximum values of LDA and N.
*
*  TAU     (workspace) DOUBLE PRECISION array, dimension (min(M,N))
*
*  B       (workspace) DOUBLE PRECISION array, dimension (LDAMAX*NMAX)
*
*  WORK    (workspace) DOUBLE PRECISION array, dimension (LDAMAX*NBMAX)
*          where NBMAX is the maximum value of NB.
*
*  RESLTS  (workspace) DOUBLE PRECISION array, dimension
*                      (LDR1,LDR2,LDR3,2*NK)
*          The timing results for each subroutine over the relevant
*          values of (M,N), (NB,NX), and LDA.
*
*  LDR1    (input) INTEGER
*          The first dimension of RESLTS.  LDR1 >= max(1,NNB).
*
*  LDR2    (input) INTEGER
*          The second dimension of RESLTS.  LDR2 >= max(1,NM).
*
*  LDR3    (input) INTEGER
*          The third dimension of RESLTS.  LDR3 >= max(1,NLDA).
*
*  NOUT    (input) INTEGER
*          The unit number for output.
*
*  Internal Parameters
*  ===================
*
*  MODE    INTEGER
*          The matrix type.  MODE = 3 is a geometric distribution of
*          eigenvalues.  See DLATMS for further details.
*
*  COND    DOUBLE PRECISION
*          The condition number of the matrix.  The singular values are
*          set to values from DMAX to DMAX/COND.
*
*  DMAX    DOUBLE PRECISION
*          The magnitude of the largest singular value.
*
*  =====================================================================
*
*     .. Parameters ..
      INTEGER            NSUBS
      PARAMETER          ( NSUBS = 3 )
      INTEGER            MODE
      DOUBLE PRECISION   COND, DMAX
      PARAMETER          ( MODE = 3, COND = 100.0D0, DMAX = 1.0D0 )
*     ..
*     .. Local Scalars ..
      CHARACTER          LABM, SIDE, TRANS
      CHARACTER*3        PATH
      CHARACTER*6        CNAME
      INTEGER            I, I4, IC, ICL, IK, ILDA, IM, IMX, INB, INFO,
     $                   ISIDE, ISUB, ITOFF, ITRAN, K, K1, LDA, LW, M,
     $                   M1, MINMN, N, N1, NB, NX
      DOUBLE PRECISION   OPS, S1, S2, TIME, UNTIME
*     ..
*     .. Local Arrays ..
      LOGICAL            TIMSUB( NSUBS )
      CHARACTER          SIDES( 2 ), TRANSS( 2 )
      CHARACTER*6        SUBNAM( NSUBS )
      INTEGER            ISEED( 4 ), MUSE( 12 ), NUSE( 12 ), RESEED( 4 )
*     ..
*     .. External Functions ..
      DOUBLE PRECISION   DMFLOP, DOPLA, DSECND
      EXTERNAL           DMFLOP, DOPLA, DSECND
*     ..
*     .. External Subroutines ..
      EXTERNAL           ATIMCK, ATIMIN, DGEQRF, DLACPY, DLATMS, DORGQR,
     $                   DORMQR, DPRTB4, DPRTB5, DTIMMG, ICOPY, XLAENV
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          DBLE, MAX, MIN
*     ..
*     .. Data statements ..
      DATA               SUBNAM / 'DGEQRF', 'DORGQR', 'DORMQR' /
      DATA               SIDES / 'L', 'R' / , TRANSS / 'N', 'T' /
      DATA               ISEED / 0, 0, 0, 1 /
*     ..
*     .. Executable Statements ..
*
*     Extract the timing request from the input line.
*
      PATH( 1: 1 ) = 'Double precision'
      PATH( 2: 3 ) = 'QR'
      CALL ATIMIN( PATH, LINE, NSUBS, SUBNAM, TIMSUB, NOUT, INFO )
      IF( INFO.NE.0 )
     $   GO TO 230
*
*     Check that M <= LDA for the input values.
*
      CNAME = LINE( 1: 6 )
      CALL ATIMCK( 1, CNAME, NM, MVAL, NLDA, LDAVAL, NOUT, INFO )
      IF( INFO.GT.0 ) THEN
         WRITE( NOUT, FMT = 9999 )CNAME
         GO TO 230
      END IF
*
*     Do for each pair of values (M,N):
*
      DO 70 IM = 1, NM
         M = MVAL( IM )
         N = NVAL( IM )
         MINMN = MIN( M, N )
         CALL ICOPY( 4, ISEED, 1, RESEED, 1 )
*
*        Do for each value of LDA:
*
         DO 60 ILDA = 1, NLDA
            LDA = LDAVAL( ILDA )
*
*           Do for each pair of values (NB, NX) in NBVAL and NXVAL.
*
            DO 50 INB = 1, NNB
               NB = NBVAL( INB )
               CALL XLAENV( 1, NB )
               NX = NXVAL( INB )
               CALL XLAENV( 3, NX )
               LW = MAX( 1, N*MAX( 1, NB ) )
*
*              Generate a test matrix of size M by N.
*
               CALL ICOPY( 4, RESEED, 1, ISEED, 1 )
               CALL DLATMS( M, N, 'Uniform', ISEED, 'Nonsymm', TAU,
     $                      MODE, COND, DMAX, M, N, 'No packing', B,
     $                      LDA, WORK, INFO )
*
               IF( TIMSUB( 1 ) ) THEN
*
*                 DGEQRF:  QR factorization
*
                  CALL DLACPY( 'Full', M, N, B, LDA, A, LDA )
                  IC = 0
                  S1 = DSECND( )
   10             CONTINUE
                  CALL DGEQRF( M, N, A, LDA, TAU, WORK, LW, INFO )
                  S2 = DSECND( )
                  TIME = S2 - S1
                  IC = IC + 1
                  IF( TIME.LT.TIMMIN ) THEN
                     CALL DLACPY( 'Full', M, N, B, LDA, A, LDA )
                     GO TO 10
                  END IF
*
*                 Subtract the time used in DLACPY.
*
                  ICL = 1
                  S1 = DSECND( )
   20             CONTINUE
                  S2 = DSECND( )
                  UNTIME = S2 - S1
                  ICL = ICL + 1
                  IF( ICL.LE.IC ) THEN
                     CALL DLACPY( 'Full', M, N, A, LDA, B, LDA )
                     GO TO 20
                  END IF
*
                  TIME = ( TIME-UNTIME ) / DBLE( IC )
                  OPS = DOPLA( 'DGEQRF', M, N, 0, 0, NB )
                  RESLTS( INB, IM, ILDA, 1 ) = DMFLOP( OPS, TIME, INFO )
               ELSE
*
*                 If DGEQRF was not timed, generate a matrix and factor
*                 it using DGEQRF anyway so that the factored form of
*                 the matrix can be used in timing the other routines.
*
                  CALL DLACPY( 'Full', M, N, B, LDA, A, LDA )
                  CALL DGEQRF( M, N, A, LDA, TAU, WORK, LW, INFO )
               END IF
*
               IF( TIMSUB( 2 ) ) THEN
*
*                 DORGQR:  Generate orthogonal matrix Q from the QR
*                 factorization
*
                  CALL DLACPY( 'Full', M, MINMN, A, LDA, B, LDA )
                  IC = 0
                  S1 = DSECND( )
   30             CONTINUE
                  CALL DORGQR( M, MINMN, MINMN, B, LDA, TAU, WORK, LW,
     $                         INFO )
                  S2 = DSECND( )
                  TIME = S2 - S1
                  IC = IC + 1
                  IF( TIME.LT.TIMMIN ) THEN
                     CALL DLACPY( 'Full', M, MINMN, A, LDA, B, LDA )
                     GO TO 30
                  END IF
*
*                 Subtract the time used in DLACPY.
*
                  ICL = 1
                  S1 = DSECND( )
   40             CONTINUE
                  S2 = DSECND( )
                  UNTIME = S2 - S1
                  ICL = ICL + 1
                  IF( ICL.LE.IC ) THEN
                     CALL DLACPY( 'Full', M, MINMN, A, LDA, B, LDA )
                     GO TO 40
                  END IF
*
                  TIME = ( TIME-UNTIME ) / DBLE( IC )
                  OPS = DOPLA( 'DORGQR', M, MINMN, MINMN, 0, NB )
                  RESLTS( INB, IM, ILDA, 2 ) = DMFLOP( OPS, TIME, INFO )
               END IF
*
   50       CONTINUE
   60    CONTINUE
   70 CONTINUE
*
*     Print tables of results
*
      DO 90 ISUB = 1, NSUBS - 1
         IF( .NOT.TIMSUB( ISUB ) )
     $      GO TO 90
         WRITE( NOUT, FMT = 9998 )SUBNAM( ISUB )
         IF( NLDA.GT.1 ) THEN
            DO 80 I = 1, NLDA
               WRITE( NOUT, FMT = 9997 )I, LDAVAL( I )
   80       CONTINUE
         END IF
         WRITE( NOUT, FMT = * )
         IF( ISUB.EQ.2 )
     $      WRITE( NOUT, FMT = 9996 )
         CALL DPRTB4( '(  NB,  NX)', 'M', 'N', NNB, NBVAL, NXVAL, NM,
     $                MVAL, NVAL, NLDA, RESLTS( 1, 1, 1, ISUB ), LDR1,
     $                LDR2, NOUT )
   90 CONTINUE
*
*     Time DORMQR separately.  Here the starting matrix is M by N, and
*     K is the free dimension of the matrix multiplied by Q.
*
      IF( TIMSUB( 3 ) ) THEN
*
*        Check that K <= LDA for the input values.
*
         CALL ATIMCK( 3, CNAME, NK, KVAL, NLDA, LDAVAL, NOUT, INFO )
         IF( INFO.GT.0 ) THEN
            WRITE( NOUT, FMT = 9999 )SUBNAM( 3 )
            GO TO 230
         END IF
*
*        Use only the pairs (M,N) where M >= N.
*
         IMX = 0
         DO 100 IM = 1, NM
            IF( MVAL( IM ).GE.NVAL( IM ) ) THEN
               IMX = IMX + 1
               MUSE( IMX ) = MVAL( IM )
               NUSE( IMX ) = NVAL( IM )
            END IF
  100    CONTINUE
*
*        DORMQR:  Multiply by Q stored as a product of elementary
*        transformations
*
*        Do for each pair of values (M,N):
*
         DO 180 IM = 1, IMX
            M = MUSE( IM )
            N = NUSE( IM )
*
*           Do for each value of LDA:
*
            DO 170 ILDA = 1, NLDA
               LDA = LDAVAL( ILDA )
*
*              Generate an M by N matrix and form its QR decomposition.
*
               CALL DLATMS( M, N, 'Uniform', ISEED, 'Nonsymm', TAU,
     $                      MODE, COND, DMAX, M, N, 'No packing', A,
     $                      LDA, WORK, INFO )
               LW = MAX( 1, N*MAX( 1, NB ) )
               CALL DGEQRF( M, N, A, LDA, TAU, WORK, LW, INFO )
*
*              Do first for SIDE = 'L', then for SIDE = 'R'
*
               I4 = 0
               DO 160 ISIDE = 1, 2
                  SIDE = SIDES( ISIDE )
*
*                 Do for each pair of values (NB, NX) in NBVAL and
*                 NXVAL.
*
                  DO 150 INB = 1, NNB
                     NB = NBVAL( INB )
                     CALL XLAENV( 1, NB )
                     NX = NXVAL( INB )
                     CALL XLAENV( 3, NX )
*
*                    Do for each value of K in KVAL
*
                     DO 140 IK = 1, NK
                        K = KVAL( IK )
*
*                       Sort out which variable is which
*
                        IF( ISIDE.EQ.1 ) THEN
                           M1 = M
                           K1 = N
                           N1 = K
                           LW = MAX( 1, N1*MAX( 1, NB ) )
                        ELSE
                           N1 = M
                           K1 = N
                           M1 = K
                           LW = MAX( 1, M1*MAX( 1, NB ) )
                        END IF
*
*                       Do first for TRANS = 'N', then for TRANS = 'T'
*
                        ITOFF = 0
                        DO 130 ITRAN = 1, 2
                           TRANS = TRANSS( ITRAN )
                           CALL DTIMMG( 0, M1, N1, B, LDA, 0, 0 )
                           IC = 0
                           S1 = DSECND( )
  110                      CONTINUE
                           CALL DORMQR( SIDE, TRANS, M1, N1, K1, A, LDA,
     $                                  TAU, B, LDA, WORK, LW, INFO )
                           S2 = DSECND( )
                           TIME = S2 - S1
                           IC = IC + 1
                           IF( TIME.LT.TIMMIN ) THEN
                              CALL DTIMMG( 0, M1, N1, B, LDA, 0, 0 )
                              GO TO 110
                           END IF
*
*                          Subtract the time used in DTIMMG.
*
                           ICL = 1
                           S1 = DSECND( )
  120                      CONTINUE
                           S2 = DSECND( )
                           UNTIME = S2 - S1
                           ICL = ICL + 1
                           IF( ICL.LE.IC ) THEN
                              CALL DTIMMG( 0, M1, N1, B, LDA, 0, 0 )
                              GO TO 120
                           END IF
*
                           TIME = ( TIME-UNTIME ) / DBLE( IC )
                           OPS = DOPLA( 'DORMQR', M1, N1, K1, ISIDE-1,
     $                           NB )
                           RESLTS( INB, IM, ILDA,
     $                        I4+ITOFF+IK ) = DMFLOP( OPS, TIME, INFO )
                           ITOFF = NK
  130                   CONTINUE
  140                CONTINUE
  150             CONTINUE
                  I4 = 2*NK
  160          CONTINUE
  170       CONTINUE
  180    CONTINUE
*
*        Print tables of results
*
         ISUB = 3
         I4 = 1
         IF( IMX.GE.1 ) THEN
            DO 220 ISIDE = 1, 2
               SIDE = SIDES( ISIDE )
               IF( ISIDE.EQ.1 ) THEN
                  WRITE( NOUT, FMT = 9998 )SUBNAM( ISUB )
                  IF( NLDA.GT.1 ) THEN
                     DO 190 I = 1, NLDA
                        WRITE( NOUT, FMT = 9997 )I, LDAVAL( I )
  190                CONTINUE
                  END IF
               END IF
               DO 210 ITRAN = 1, 2
                  TRANS = TRANSS( ITRAN )
                  DO 200 IK = 1, NK
                     IF( ISIDE.EQ.1 ) THEN
                        N = KVAL( IK )
                        WRITE( NOUT, FMT = 9995 )SUBNAM( ISUB ), SIDE,
     $                     TRANS, 'N', N
                        LABM = 'M'
                     ELSE
                        M = KVAL( IK )
                        WRITE( NOUT, FMT = 9995 )SUBNAM( ISUB ), SIDE,
     $                     TRANS, 'M', M
                        LABM = 'N'
                     END IF
                     CALL DPRTB5( 'NB', LABM, 'K', NNB, NBVAL, IMX,
     $                            MUSE, NUSE, NLDA,
     $                            RESLTS( 1, 1, 1, I4 ), LDR1, LDR2,
     $                            NOUT )
                     I4 = I4 + 1
  200             CONTINUE
  210          CONTINUE
  220       CONTINUE
         ELSE
            WRITE( NOUT, FMT = 9994 )SUBNAM( ISUB )
         END IF
      END IF
  230 CONTINUE
 9999 FORMAT( 1X, A6, ' timing run not attempted', / )
 9998 FORMAT( / ' *** Speed of ', A6, ' in megaflops ***' )
 9997 FORMAT( 5X, 'line ', I2, ' with LDA = ', I5 )
 9996 FORMAT( 5X, 'K = min(M,N)', / )
 9995 FORMAT( / 5X, A6, ' with SIDE = ''', A1, ''', TRANS = ''', A1,
     $      ''', ', A1, ' =', I6, / )
 9994 FORMAT( ' *** No pairs (M,N) found with M >= N:  ', A6,
     $      ' not timed' )
      RETURN
*
*     End of DTIMQR
*
      END
      SUBROUTINE DTIMRQ( LINE, NM, MVAL, NVAL, NK, KVAL, NNB, NBVAL,
     $                   NXVAL, NLDA, LDAVAL, TIMMIN, A, TAU, B, WORK,
     $                   RESLTS, LDR1, LDR2, LDR3, NOUT )
*
*  -- LAPACK timing routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     March 31, 1993
*
*     .. Scalar Arguments ..
      CHARACTER*80       LINE
      INTEGER            LDR1, LDR2, LDR3, NK, NLDA, NM, NNB, NOUT
      DOUBLE PRECISION   TIMMIN
*     ..
*     .. Array Arguments ..
      INTEGER            KVAL( * ), LDAVAL( * ), MVAL( * ), NBVAL( * ),
     $                   NVAL( * ), NXVAL( * )
      DOUBLE PRECISION   A( * ), B( * ), RESLTS( LDR1, LDR2, LDR3, * ),
     $                   TAU( * ), WORK( * )
*     ..
*
*  Purpose
*  =======
*
*  DTIMRQ times the LAPACK routines to perform the RQ factorization of
*  a DOUBLE PRECISION general matrix.
*
*  Arguments
*  =========
*
*  LINE    (input) CHARACTER*80
*          The input line that requested this routine.  The first six
*          characters contain either the name of a subroutine or a
*          generic path name.  The remaining characters may be used to
*          specify the individual routines to be timed.  See ATIMIN for
*          a full description of the format of the input line.
*
*  NM      (input) INTEGER
*          The number of values of M and N contained in the vectors
*          MVAL and NVAL.  The matrix sizes are used in pairs (M,N).
*
*  MVAL    (input) INTEGER array, dimension (NM)
*          The values of the matrix row dimension M.
*
*  NVAL    (input) INTEGER array, dimension (NM)
*          The values of the matrix column dimension N.
*
*  NK      (input) INTEGER
*          The number of values of K in the vector KVAL.
*
*  KVAL    (input) INTEGER array, dimension (NK)
*          The values of the matrix dimension K, used in DORMRQ.
*
*  NNB     (input) INTEGER
*          The number of values of NB and NX contained in the
*          vectors NBVAL and NXVAL.  The blocking parameters are used
*          in pairs (NB,NX).
*
*  NBVAL   (input) INTEGER array, dimension (NNB)
*          The values of the blocksize NB.
*
*  NXVAL   (input) INTEGER array, dimension (NNB)
*          The values of the crossover point NX.
*
*  NLDA    (input) INTEGER
*          The number of values of LDA contained in the vector LDAVAL.
*
*  LDAVAL  (input) INTEGER array, dimension (NLDA)
*          The values of the leading dimension of the array A.
*
*  TIMMIN  (input) DOUBLE PRECISION
*          The minimum time a subroutine will be timed.
*
*  A       (workspace) DOUBLE PRECISION array, dimension (LDAMAX*NMAX)
*          where LDAMAX and NMAX are the maximum values of LDA and N.
*
*  TAU     (workspace) DOUBLE PRECISION array, dimension (min(M,N))
*
*  B       (workspace) DOUBLE PRECISION array, dimension (LDAMAX*NMAX)
*
*  WORK    (workspace) DOUBLE PRECISION array, dimension (LDAMAX*NBMAX)
*          where NBMAX is the maximum value of NB.
*
*  RESLTS  (workspace) DOUBLE PRECISION array, dimension
*                      (LDR1,LDR2,LDR3,2*NK)
*          The timing results for each subroutine over the relevant
*          values of (M,N), (NB,NX), and LDA.
*
*  LDR1    (input) INTEGER
*          The first dimension of RESLTS.  LDR1 >= max(1,NNB).
*
*  LDR2    (input) INTEGER
*          The second dimension of RESLTS.  LDR2 >= max(1,NM).
*
*  LDR3    (input) INTEGER
*          The third dimension of RESLTS.  LDR3 >= max(1,NLDA).
*
*  NOUT    (input) INTEGER
*          The unit number for output.
*
*  Internal Parameters
*  ===================
*
*  MODE    INTEGER
*          The matrix type.  MODE = 3 is a geometric distribution of
*          eigenvalues.  See DLATMS for further details.
*
*  COND    DOUBLE PRECISION
*          The condition number of the matrix.  The singular values are
*          set to values from DMAX to DMAX/COND.
*
*  DMAX    DOUBLE PRECISION
*          The magnitude of the largest singular value.
*
*  =====================================================================
*
*     .. Parameters ..
      INTEGER            NSUBS
      PARAMETER          ( NSUBS = 3 )
      INTEGER            MODE
      DOUBLE PRECISION   COND, DMAX
      PARAMETER          ( MODE = 3, COND = 100.0D0, DMAX = 1.0D0 )
*     ..
*     .. Local Scalars ..
      CHARACTER          LABM, SIDE, TRANS
      CHARACTER*3        PATH
      CHARACTER*6        CNAME
      INTEGER            I, I4, IC, ICL, IK, ILDA, IM, IMX, INB, INFO,
     $                   ISIDE, ISUB, ITOFF, ITRAN, K, K1, LDA, LW, M,
     $                   M1, MINMN, N, N1, NB, NX
      DOUBLE PRECISION   OPS, S1, S2, TIME, UNTIME
*     ..
*     .. Local Arrays ..
      LOGICAL            TIMSUB( NSUBS )
      CHARACTER          SIDES( 2 ), TRANSS( 2 )
      CHARACTER*6        SUBNAM( NSUBS )
      INTEGER            ISEED( 4 ), MUSE( 12 ), NUSE( 12 ), RESEED( 4 )
*     ..
*     .. External Functions ..
      DOUBLE PRECISION   DMFLOP, DOPLA, DSECND
      EXTERNAL           DMFLOP, DOPLA, DSECND
*     ..
*     .. External Subroutines ..
      EXTERNAL           ATIMCK, ATIMIN, DGERQF, DLACPY, DLATMS, DORGRQ,
     $                   DORMRQ, DPRTB4, DPRTB5, DTIMMG, ICOPY, XLAENV
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          DBLE, MAX, MIN
*     ..
*     .. Data statements ..
      DATA               SUBNAM / 'DGERQF', 'DORGRQ', 'DORMRQ' /
      DATA               SIDES / 'L', 'R' / , TRANSS / 'N', 'T' /
      DATA               ISEED / 0, 0, 0, 1 /
*     ..
*     .. Executable Statements ..
*
*     Extract the timing request from the input line.
*
      PATH( 1: 1 ) = 'Double precision'
      PATH( 2: 3 ) = 'RQ'
      CALL ATIMIN( PATH, LINE, NSUBS, SUBNAM, TIMSUB, NOUT, INFO )
      IF( INFO.NE.0 )
     $   GO TO 230
*
*     Check that M <= LDA for the input values.
*
      CNAME = LINE( 1: 6 )
      CALL ATIMCK( 1, CNAME, NM, MVAL, NLDA, LDAVAL, NOUT, INFO )
      IF( INFO.GT.0 ) THEN
         WRITE( NOUT, FMT = 9999 )CNAME
         GO TO 230
      END IF
*
*     Do for each pair of values (M,N):
*
      DO 70 IM = 1, NM
         M = MVAL( IM )
         N = NVAL( IM )
         MINMN = MIN( M, N )
         CALL ICOPY( 4, ISEED, 1, RESEED, 1 )
*
*        Do for each value of LDA:
*
         DO 60 ILDA = 1, NLDA
            LDA = LDAVAL( ILDA )
*
*           Do for each pair of values (NB, NX) in NBVAL and NXVAL.
*
            DO 50 INB = 1, NNB
               NB = NBVAL( INB )
               CALL XLAENV( 1, NB )
               NX = NXVAL( INB )
               CALL XLAENV( 3, NX )
               LW = MAX( 1, M*MAX( 1, NB ) )
*
*              Generate a test matrix of size M by N.
*
               CALL ICOPY( 4, RESEED, 1, ISEED, 1 )
               CALL DLATMS( M, N, 'Uniform', ISEED, 'Nonsymm', TAU,
     $                      MODE, COND, DMAX, M, N, 'No packing', B,
     $                      LDA, WORK, INFO )
*
               IF( TIMSUB( 1 ) ) THEN
*
*                 DGERQF:  RQ factorization
*
                  CALL DLACPY( 'Full', M, N, B, LDA, A, LDA )
                  IC = 0
                  S1 = DSECND( )
   10             CONTINUE
                  CALL DGERQF( M, N, A, LDA, TAU, WORK, LW, INFO )
                  S2 = DSECND( )
                  TIME = S2 - S1
                  IC = IC + 1
                  IF( TIME.LT.TIMMIN ) THEN
                     CALL DLACPY( 'Full', M, N, B, LDA, A, LDA )
                     GO TO 10
                  END IF
*
*                 Subtract the time used in DLACPY.
*
                  ICL = 1
                  S1 = DSECND( )
   20             CONTINUE
                  S2 = DSECND( )
                  UNTIME = S2 - S1
                  ICL = ICL + 1
                  IF( ICL.LE.IC ) THEN
                     CALL DLACPY( 'Full', M, N, A, LDA, B, LDA )
                     GO TO 20
                  END IF
*
                  TIME = ( TIME-UNTIME ) / DBLE( IC )
                  OPS = DOPLA( 'DGERQF', M, N, 0, 0, NB )
                  RESLTS( INB, IM, ILDA, 1 ) = DMFLOP( OPS, TIME, INFO )
               ELSE
*
*                 If DGERQF was not timed, generate a matrix and factor
*                 it using DGERQF anyway so that the factored form of
*                 the matrix can be used in timing the other routines.
*
                  CALL DLACPY( 'Full', M, N, B, LDA, A, LDA )
                  CALL DGERQF( M, N, A, LDA, TAU, WORK, LW, INFO )
               END IF
*
               IF( TIMSUB( 2 ) ) THEN
*
*                 DORGRQ:  Generate orthogonal matrix Q from the RQ
*                 factorization
*
                  CALL DLACPY( 'Full', MINMN, N, A, LDA, B, LDA )
                  IC = 0
                  S1 = DSECND( )
   30             CONTINUE
                  CALL DORGRQ( MINMN, N, MINMN, B, LDA, TAU, WORK, LW,
     $                         INFO )
                  S2 = DSECND( )
                  TIME = S2 - S1
                  IC = IC + 1
                  IF( TIME.LT.TIMMIN ) THEN
                     CALL DLACPY( 'Full', MINMN, N, A, LDA, B, LDA )
                     GO TO 30
                  END IF
*
*                 Subtract the time used in DLACPY.
*
                  ICL = 1
                  S1 = DSECND( )
   40             CONTINUE
                  S2 = DSECND( )
                  UNTIME = S2 - S1
                  ICL = ICL + 1
                  IF( ICL.LE.IC ) THEN
                     CALL DLACPY( 'Full', MINMN, N, A, LDA, B, LDA )
                     GO TO 40
                  END IF
*
                  TIME = ( TIME-UNTIME ) / DBLE( IC )
                  OPS = DOPLA( 'DORGRQ', MINMN, N, MINMN, 0, NB )
                  RESLTS( INB, IM, ILDA, 2 ) = DMFLOP( OPS, TIME, INFO )
               END IF
*
   50       CONTINUE
   60    CONTINUE
   70 CONTINUE
*
*     Print tables of results
*
      DO 90 ISUB = 1, NSUBS - 1
         IF( .NOT.TIMSUB( ISUB ) )
     $      GO TO 90
         WRITE( NOUT, FMT = 9998 )SUBNAM( ISUB )
         IF( NLDA.GT.1 ) THEN
            DO 80 I = 1, NLDA
               WRITE( NOUT, FMT = 9997 )I, LDAVAL( I )
   80       CONTINUE
         END IF
         WRITE( NOUT, FMT = * )
         IF( ISUB.EQ.2 )
     $      WRITE( NOUT, FMT = 9996 )
         CALL DPRTB4( '(  NB,  NX)', 'M', 'N', NNB, NBVAL, NXVAL, NM,
     $                MVAL, NVAL, NLDA, RESLTS( 1, 1, 1, ISUB ), LDR1,
     $                LDR2, NOUT )
   90 CONTINUE
*
*     Time DORMRQ separately.  Here the starting matrix is M by N, and
*     K is the free dimension of the matrix multiplied by Q.
*
      IF( TIMSUB( 3 ) ) THEN
*
*        Check that K <= LDA for the input values.
*
         CALL ATIMCK( 3, CNAME, NK, KVAL, NLDA, LDAVAL, NOUT, INFO )
         IF( INFO.GT.0 ) THEN
            WRITE( NOUT, FMT = 9999 )SUBNAM( 3 )
            GO TO 230
         END IF
*
*        Use only the pairs (M,N) where M <= N.
*
         IMX = 0
         DO 100 IM = 1, NM
            IF( MVAL( IM ).LE.NVAL( IM ) ) THEN
               IMX = IMX + 1
               MUSE( IMX ) = MVAL( IM )
               NUSE( IMX ) = NVAL( IM )
            END IF
  100    CONTINUE
*
*        DORMRQ:  Multiply by Q stored as a product of elementary
*        transformations
*
*        Do for each pair of values (M,N):
*
         DO 180 IM = 1, IMX
            M = MUSE( IM )
            N = NUSE( IM )
*
*           Do for each value of LDA:
*
            DO 170 ILDA = 1, NLDA
               LDA = LDAVAL( ILDA )
*
*              Generate an M by N matrix and form its RQ decomposition.
*
               CALL DLATMS( M, N, 'Uniform', ISEED, 'Nonsymm', TAU,
     $                      MODE, COND, DMAX, M, N, 'No packing', A,
     $                      LDA, WORK, INFO )
               LW = MAX( 1, M*MAX( 1, NB ) )
               CALL DGERQF( M, N, A, LDA, TAU, WORK, LW, INFO )
*
*              Do first for SIDE = 'L', then for SIDE = 'R'
*
               I4 = 0
               DO 160 ISIDE = 1, 2
                  SIDE = SIDES( ISIDE )
*
*                 Do for each pair of values (NB, NX) in NBVAL and
*                 NXVAL.
*
                  DO 150 INB = 1, NNB
                     NB = NBVAL( INB )
                     CALL XLAENV( 1, NB )
                     NX = NXVAL( INB )
                     CALL XLAENV( 3, NX )
*
*                    Do for each value of K in KVAL
*
                     DO 140 IK = 1, NK
                        K = KVAL( IK )
*
*                       Sort out which variable is which
*
                        IF( ISIDE.EQ.1 ) THEN
                           K1 = M
                           M1 = N
                           N1 = K
                           LW = MAX( 1, N1*MAX( 1, NB ) )
                        ELSE
                           K1 = M
                           N1 = N
                           M1 = K
                           LW = MAX( 1, M1*MAX( 1, NB ) )
                        END IF
*
*                       Do first for TRANS = 'N', then for TRANS = 'T'
*
                        ITOFF = 0
                        DO 130 ITRAN = 1, 2
                           TRANS = TRANSS( ITRAN )
                           CALL DTIMMG( 0, M1, N1, B, LDA, 0, 0 )
                           IC = 0
                           S1 = DSECND( )
  110                      CONTINUE
                           CALL DORMRQ( SIDE, TRANS, M1, N1, K1, A, LDA,
     $                                  TAU, B, LDA, WORK, LW, INFO )
                           S2 = DSECND( )
                           TIME = S2 - S1
                           IC = IC + 1
                           IF( TIME.LT.TIMMIN ) THEN
                              CALL DTIMMG( 0, M1, N1, B, LDA, 0, 0 )
                              GO TO 110
                           END IF
*
*                          Subtract the time used in DTIMMG.
*
                           ICL = 1
                           S1 = DSECND( )
  120                      CONTINUE
                           S2 = DSECND( )
                           UNTIME = S2 - S1
                           ICL = ICL + 1
                           IF( ICL.LE.IC ) THEN
                              CALL DTIMMG( 0, M1, N1, B, LDA, 0, 0 )
                              GO TO 120
                           END IF
*
                           TIME = ( TIME-UNTIME ) / DBLE( IC )
                           OPS = DOPLA( 'DORMRQ', M1, N1, K1, ISIDE-1,
     $                           NB )
                           RESLTS( INB, IM, ILDA,
     $                        I4+ITOFF+IK ) = DMFLOP( OPS, TIME, INFO )
                           ITOFF = NK
  130                   CONTINUE
  140                CONTINUE
  150             CONTINUE
                  I4 = 2*NK
  160          CONTINUE
  170       CONTINUE
  180    CONTINUE
*
*        Print tables of results
*
         ISUB = 3
         I4 = 1
         IF( IMX.GE.1 ) THEN
            DO 220 ISIDE = 1, 2
               SIDE = SIDES( ISIDE )
               IF( ISIDE.EQ.1 ) THEN
                  WRITE( NOUT, FMT = 9998 )SUBNAM( ISUB )
                  IF( NLDA.GT.1 ) THEN
                     DO 190 I = 1, NLDA
                        WRITE( NOUT, FMT = 9997 )I, LDAVAL( I )
  190                CONTINUE
                  END IF
               END IF
               DO 210 ITRAN = 1, 2
                  TRANS = TRANSS( ITRAN )
                  DO 200 IK = 1, NK
                     IF( ISIDE.EQ.1 ) THEN
                        N = KVAL( IK )
                        WRITE( NOUT, FMT = 9995 )SUBNAM( ISUB ), SIDE,
     $                     TRANS, 'N', N
                        LABM = 'M'
                     ELSE
                        M = KVAL( IK )
                        WRITE( NOUT, FMT = 9995 )SUBNAM( ISUB ), SIDE,
     $                     TRANS, 'M', M
                        LABM = 'N'
                     END IF
                     CALL DPRTB5( 'NB', 'K', LABM, NNB, NBVAL, IMX,
     $                            MUSE, NUSE, NLDA,
     $                            RESLTS( 1, 1, 1, I4 ), LDR1, LDR2,
     $                            NOUT )
                     I4 = I4 + 1
  200             CONTINUE
  210          CONTINUE
  220       CONTINUE
         ELSE
            WRITE( NOUT, FMT = 9994 )SUBNAM( ISUB )
         END IF
      END IF
  230 CONTINUE
 9999 FORMAT( 1X, A6, ' timing run not attempted', / )
 9998 FORMAT( / ' *** Speed of ', A6, ' in megaflops ***' )
 9997 FORMAT( 5X, 'line ', I2, ' with LDA = ', I5 )
 9996 FORMAT( 5X, 'K = min(M,N)', / )
 9995 FORMAT( / 5X, A6, ' with SIDE = ''', A1, ''', TRANS = ''', A1,
     $      ''', ', A1, ' =', I6, / )
 9994 FORMAT( ' *** No pairs (M,N) found with M <= N:  ', A6,
     $      ' not timed' )
      RETURN
*
*     End of DTIMRQ
*
      END
      SUBROUTINE DTIMSP( LINE, NN, NVAL, NNS, NSVAL, LA, TIMMIN, A, B,
     $                   WORK, IWORK, RESLTS, LDR1, LDR2, LDR3, NOUT )
*
*  -- LAPACK timing routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     March 31, 1993
*
*     .. Scalar Arguments ..
      CHARACTER*80       LINE
      INTEGER            LA, LDR1, LDR2, LDR3, NN, NNS, NOUT
      DOUBLE PRECISION   TIMMIN
*     ..
*     .. Array Arguments ..
      INTEGER            IWORK( * ), NSVAL( * ), NVAL( * )
      DOUBLE PRECISION   A( * ), B( * ), RESLTS( LDR1, LDR2, LDR3, * ),
     $                   WORK( * )
*     ..
*
*  Purpose
*  =======
*
*  DTIMSP times DSPTRF, -TRS, and -TRI.
*
*  Arguments
*  =========
*
*  LINE    (input) CHARACTER*80
*          The input line that requested this routine.  The first six
*          characters contain either the name of a subroutine or a
*          generic path name.  The remaining characters may be used to
*          specify the individual routines to be timed.  See ATIMIN for
*          a full description of the format of the input line.
*
*  NN      (input) INTEGER
*          The number of values of N contained in the vector NVAL.
*
*  NVAL    (input) INTEGER array, dimension (NN)
*          The values of the matrix size N.
*
*  NNS     (input) INTEGER
*          The number of values of NRHS contained in the vector NSVAL.
*
*  NSVAL   (input) INTEGER array, dimension (NNS)
*          The values of the number of right hand sides NRHS.
*
*  LA      (input) INTEGER
*          The size of the arrays A, B, and C.
*
*  TIMMIN  (input) DOUBLE PRECISION
*          The minimum time a subroutine will be timed.
*
*  A       (workspace) DOUBLE PRECISION array, dimension (LA)
*
*  B       (workspace) DOUBLE PRECISION array, dimension (LA)
*
*  WORK    (workspace) DOUBLE PRECISION array, dimension (NMAX)
*
*  IWORK   (workspace) INTEGER array, dimension (NMAX)
*          where NMAX is the maximum value of N permitted.
*
*  RESLTS  (output) DOUBLE PRECISION array, dimension
*                   (LDR1,LDR2,LDR3,NSUBS)
*          The timing results for each subroutine over the relevant
*          values of N.
*
*  LDR1    (input) INTEGER
*          The first dimension of RESLTS.  LDR1 >= max(4,NNB).
*
*  LDR2    (input) INTEGER
*          The second dimension of RESLTS.  LDR2 >= max(1,NN).
*
*  LDR3    (input) INTEGER
*          The third dimension of RESLTS.  LDR3 >= 2.
*
*  NOUT    (input) INTEGER
*          The unit number for output.
*
*  =====================================================================
*
*     .. Parameters ..
      INTEGER            NSUBS
      PARAMETER          ( NSUBS = 3 )
*     ..
*     .. Local Scalars ..
      CHARACTER          UPLO
      CHARACTER*3        PATH
      CHARACTER*6        CNAME
      INTEGER            I, IC, ICL, IN, INFO, ISUB, IUPLO, LDA, LDB,
     $                   MAT, N, NRHS
      DOUBLE PRECISION   OPS, S1, S2, TIME, UNTIME
*     ..
*     .. Local Arrays ..
      LOGICAL            TIMSUB( NSUBS )
      CHARACTER          UPLOS( 2 )
      CHARACTER*6        SUBNAM( NSUBS )
      INTEGER            LAVAL( 1 )
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      DOUBLE PRECISION   DMFLOP, DOPLA, DSECND
      EXTERNAL           LSAME, DMFLOP, DOPLA, DSECND
*     ..
*     .. External Subroutines ..
      EXTERNAL           ATIMCK, ATIMIN, DCOPY, DPRTBL, DSPTRF, DSPTRI,
     $                   DSPTRS, DTIMMG
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          DBLE, MOD
*     ..
*     .. Data statements ..
      DATA               UPLOS / 'U', 'L' /
      DATA               SUBNAM / 'DSPTRF', 'DSPTRS', 'DSPTRI' /
*     ..
*     .. Executable Statements ..
*
*     Extract the timing request from the input line.
*
      PATH( 1: 1 ) = 'Double precision'
      PATH( 2: 3 ) = 'SP'
      CALL ATIMIN( PATH, LINE, NSUBS, SUBNAM, TIMSUB, NOUT, INFO )
      IF( INFO.NE.0 )
     $   GO TO 120
*
*     Check that N*(N+1)/2 <= LA for the input values.
*
      CNAME = LINE( 1: 6 )
      LAVAL( 1 ) = LA
      CALL ATIMCK( 4, CNAME, NN, NVAL, 1, LAVAL, NOUT, INFO )
      IF( INFO.GT.0 ) THEN
         WRITE( NOUT, FMT = 9999 )CNAME
         GO TO 120
      END IF
*
*     Do first for UPLO = 'U', then for UPLO = 'L'
*
      DO 90 IUPLO = 1, 2
         UPLO = UPLOS( IUPLO )
         IF( LSAME( UPLO, 'U' ) ) THEN
            MAT = 7
         ELSE
            MAT = -7
         END IF
*
*        Do for each value of N in NVAL.
*
         DO 80 IN = 1, NN
            N = NVAL( IN )
            LDA = N*( N+1 ) / 2
*
*           Time DSPTRF
*
            IF( TIMSUB( 1 ) ) THEN
               CALL DTIMMG( MAT, N, N, A, LDA, 0, 0 )
               IC = 0
               S1 = DSECND( )
   10          CONTINUE
               CALL DSPTRF( UPLO, N, A, IWORK, INFO )
               S2 = DSECND( )
               TIME = S2 - S1
               IC = IC + 1
               IF( TIME.LT.TIMMIN ) THEN
                  CALL DTIMMG( MAT, N, N, A, LDA, 0, 0 )
                  GO TO 10
               END IF
*
*              Subtract the time used in DTIMMG.
*
               ICL = 1
               S1 = DSECND( )
   20          CONTINUE
               S2 = DSECND( )
               UNTIME = S2 - S1
               ICL = ICL + 1
               IF( ICL.LE.IC ) THEN
                  CALL DTIMMG( MAT, N, N, A, LDA, 0, 0 )
                  GO TO 20
               END IF
*
               TIME = ( TIME-UNTIME ) / DBLE( IC )
               OPS = DOPLA( 'DSPTRF', N, N, 0, 0, 0 )
               RESLTS( 1, IN, IUPLO, 1 ) = DMFLOP( OPS, TIME, INFO )
*
            ELSE
               IC = 0
               CALL DTIMMG( MAT, N, N, A, LDA, 0, 0 )
            END IF
*
*           Generate another matrix and factor it using DSPTRF so
*           that the factored form can be used in timing the other
*           routines.
*
            IF( IC.NE.1 )
     $         CALL DSPTRF( UPLO, N, A, IWORK, INFO )
*
*           Time DSPTRI
*
            IF( TIMSUB( 3 ) ) THEN
               CALL DCOPY( LDA, A, 1, B, 1 )
               IC = 0
               S1 = DSECND( )
   30          CONTINUE
               CALL DSPTRI( UPLO, N, B, IWORK, WORK, INFO )
               S2 = DSECND( )
               TIME = S2 - S1
               IC = IC + 1
               IF( TIME.LT.TIMMIN ) THEN
                  CALL DCOPY( LDA, A, 1, B, 1 )
                  GO TO 30
               END IF
*
*              Subtract the time used in DCOPY.
*
               ICL = 1
               S1 = DSECND( )
   40          CONTINUE
               S2 = DSECND( )
               UNTIME = S2 - S1
               ICL = ICL + 1
               IF( ICL.LE.IC ) THEN
                  CALL DCOPY( LDA, A, 1, B, 1 )
                  GO TO 40
               END IF
*
               TIME = ( TIME-UNTIME ) / DBLE( IC )
               OPS = DOPLA( 'DSPTRI', N, N, 0, 0, 0 )
               RESLTS( 1, IN, IUPLO, 3 ) = DMFLOP( OPS, TIME, INFO )
            END IF
*
*           Time DSPTRS
*
            IF( TIMSUB( 2 ) ) THEN
               DO 70 I = 1, NNS
                  NRHS = NSVAL( I )
                  LDB = N
                  IF( MOD( LDB, 2 ).EQ.0 )
     $               LDB = LDB + 1
                  CALL DTIMMG( 0, N, NRHS, B, LDB, 0, 0 )
                  IC = 0
                  S1 = DSECND( )
   50             CONTINUE
                  CALL DSPTRS( UPLO, N, NRHS, A, IWORK, B, LDB, INFO )
                  S2 = DSECND( )
                  TIME = S2 - S1
                  IC = IC + 1
                  IF( TIME.LT.TIMMIN ) THEN
                     CALL DTIMMG( 0, N, NRHS, B, LDB, 0, 0 )
                     GO TO 50
                  END IF
*
*                 Subtract the time used in DTIMMG.
*
                  ICL = 1
                  S1 = DSECND( )
   60             CONTINUE
                  S2 = DSECND( )
                  UNTIME = S2 - S1
                  ICL = ICL + 1
                  IF( ICL.LE.IC ) THEN
                     CALL DTIMMG( 0, N, NRHS, B, LDB, 0, 0 )
                     GO TO 60
                  END IF
*
                  TIME = ( TIME-UNTIME ) / DBLE( IC )
                  OPS = DOPLA( 'DSPTRS', N, NRHS, 0, 0, 0 )
                  RESLTS( I, IN, IUPLO, 2 ) = DMFLOP( OPS, TIME, INFO )
   70          CONTINUE
            END IF
   80    CONTINUE
   90 CONTINUE
*
*     Print tables of results for each timed routine.
*
      DO 110 ISUB = 1, NSUBS
         IF( .NOT.TIMSUB( ISUB ) )
     $      GO TO 110
         WRITE( NOUT, FMT = 9998 )SUBNAM( ISUB )
         DO 100 IUPLO = 1, 2
            WRITE( NOUT, FMT = 9997 )SUBNAM( ISUB ), UPLOS( IUPLO )
            IF( ISUB.EQ.1 ) THEN
               CALL DPRTBL( ' ', 'N', 1, LAVAL, NN, NVAL, 1,
     $                      RESLTS( 1, 1, IUPLO, 1 ), LDR1, LDR2, NOUT )
            ELSE IF( ISUB.EQ.2 ) THEN
               CALL DPRTBL( 'NRHS', 'N', NNS, NSVAL, NN, NVAL, 1,
     $                      RESLTS( 1, 1, IUPLO, 2 ), LDR1, LDR2, NOUT )
            ELSE IF( ISUB.EQ.3 ) THEN
               CALL DPRTBL( ' ', 'N', 1, LAVAL, NN, NVAL, 1,
     $                      RESLTS( 1, 1, IUPLO, 3 ), LDR1, LDR2, NOUT )
            END IF
  100    CONTINUE
  110 CONTINUE
  120 CONTINUE
 9999 FORMAT( 1X, A6, ' timing run not attempted', / )
 9998 FORMAT( / ' *** Speed of ', A6, ' in megaflops ***', / )
 9997 FORMAT( 5X, A6, ' with UPLO = ''', A1, '''', / )
      RETURN
*
*     End of DTIMSP
*
      END
      SUBROUTINE DTIMSY( LINE, NN, NVAL, NNS, NSVAL, NNB, NBVAL, NLDA,
     $                   LDAVAL, TIMMIN, A, B, WORK, IWORK, RESLTS,
     $                   LDR1, LDR2, LDR3, NOUT )
*
*  -- LAPACK timing routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     March 31, 1993
*
*     .. Scalar Arguments ..
      CHARACTER*80       LINE
      INTEGER            LDR1, LDR2, LDR3, NLDA, NN, NNB, NNS, NOUT
      DOUBLE PRECISION   TIMMIN
*     ..
*     .. Array Arguments ..
      INTEGER            IWORK( * ), LDAVAL( * ), NBVAL( * ),
     $                   NSVAL( * ), NVAL( * )
      DOUBLE PRECISION   A( * ), B( * ), RESLTS( LDR1, LDR2, LDR3, * ),
     $                   WORK( * )
*     ..
*
*  Purpose
*  =======
*
*  DTIMSY times DSYTRF, -TRS, and -TRI.
*
*  Arguments
*  =========
*
*  LINE    (input) CHARACTER*80
*          The input line that requested this routine.  The first six
*          characters contain either the name of a subroutine or a
*          generic path name.  The remaining characters may be used to
*          specify the individual routines to be timed.  See ATIMIN for
*          a full description of the format of the input line.
*
*  NN      (input) INTEGER
*          The number of values of N contained in the vector NVAL.
*
*  NVAL    (input) INTEGER array, dimension (NN)
*          The values of the matrix size N.
*
*  NNS     (input) INTEGER
*          The number of values of NRHS contained in the vector NSVAL.
*
*  NSVAL   (input) INTEGER array, dimension (NNS)
*          The values of the number of right hand sides NRHS.
*
*  NNB     (input) INTEGER
*          The number of values of NB contained in the vector NBVAL.
*
*  NBVAL   (input) INTEGER array, dimension (NNB)
*          The values of the blocksize NB.
*
*  NLDA    (input) INTEGER
*          The number of values of LDA contained in the vector LDAVAL.
*
*  LDAVAL  (input) INTEGER array, dimension (NLDA)
*          The values of the leading dimension of the array A.
*
*  TIMMIN  (input) DOUBLE PRECISION
*          The minimum time a subroutine will be timed.
*
*  A       (workspace) DOUBLE PRECISION array, dimension (LDAMAX*NMAX)
*          where LDAMAX and NMAX are the maximum values permitted
*          for LDA and N.
*
*  B       (workspace) DOUBLE PRECISION array, dimension (LDAMAX*NMAX)
*
*  WORK    (workspace) DOUBLE PRECISION array, dimension (NMAX)
*
*  IWORK   (workspace) INTEGER array, dimension (NMAX)
*
*  RESLTS  (output) DOUBLE PRECISION array, dimension
*                   (LDR1,LDR2,LDR3,NSUBS)
*          The timing results for each subroutine over the relevant
*          values of N, NB, and LDA.
*
*  LDR1    (input) INTEGER
*          The first dimension of RESLTS.  LDR1 >= max(4,NNB).
*
*  LDR2    (input) INTEGER
*          The second dimension of RESLTS.  LDR2 >= max(1,NN).
*
*  LDR3    (input) INTEGER
*          The third dimension of RESLTS.  LDR3 >= max(1,2*NLDA).
*
*  NOUT    (input) INTEGER
*          The unit number for output.
*
*  =====================================================================
*
*     .. Parameters ..
      INTEGER            NSUBS
      PARAMETER          ( NSUBS = 3 )
*     ..
*     .. Local Scalars ..
      CHARACTER          UPLO
      CHARACTER*3        PATH
      CHARACTER*6        CNAME
      INTEGER            I, I3, IC, ICL, ILDA, IN, INB, INFO, ISUB,
     $                   IUPLO, LDA, LDB, LWORK, MAT, N, NB, NRHS
      DOUBLE PRECISION   OPS, S1, S2, TIME, UNTIME
*     ..
*     .. Local Arrays ..
      LOGICAL            TIMSUB( NSUBS )
      CHARACTER          UPLOS( 2 )
      CHARACTER*6        SUBNAM( NSUBS )
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      DOUBLE PRECISION   DMFLOP, DOPLA, DSECND
      EXTERNAL           LSAME, DMFLOP, DOPLA, DSECND
*     ..
*     .. External Subroutines ..
      EXTERNAL           ATIMCK, ATIMIN, DLACPY, DPRTBL, DSYTRF, DSYTRI,
     $                   DSYTRS, DTIMMG, XLAENV
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          DBLE, MAX
*     ..
*     .. Data statements ..
      DATA               UPLOS / 'U', 'L' /
      DATA               SUBNAM / 'DSYTRF', 'DSYTRS', 'DSYTRI' /
*     ..
*     .. Executable Statements ..
*
*     Extract the timing request from the input line.
*
      PATH( 1: 1 ) = 'Double precision'
      PATH( 2: 3 ) = 'SY'
      CALL ATIMIN( PATH, LINE, NSUBS, SUBNAM, TIMSUB, NOUT, INFO )
      IF( INFO.NE.0 )
     $   GO TO 150
*
*     Check that N <= LDA for the input values.
*
      CNAME = LINE( 1: 6 )
      CALL ATIMCK( 2, CNAME, NN, NVAL, NLDA, LDAVAL, NOUT, INFO )
      IF( INFO.GT.0 ) THEN
         WRITE( NOUT, FMT = 9999 )CNAME
         GO TO 150
      END IF
*
*     Do first for UPLO = 'U', then for UPLO = 'L'
*
      DO 110 IUPLO = 1, 2
         UPLO = UPLOS( IUPLO )
         IF( LSAME( UPLO, 'U' ) ) THEN
            MAT = 6
         ELSE
            MAT = -6
         END IF
*
*        Do for each value of N in NVAL.
*
         DO 100 IN = 1, NN
            N = NVAL( IN )
*
*           Do for each value of LDA:
*
            DO 90 ILDA = 1, NLDA
               LDA = LDAVAL( ILDA )
               I3 = ( IUPLO-1 )*NLDA + ILDA
*
*              Do for each value of NB in NBVAL.  Only the blocked
*              routines are timed in this loop since the other routines
*              are independent of NB.
*
               IF( TIMSUB( 1 ) ) THEN
*
*                 Time DSYTRF
*
                  DO 30 INB = 1, NNB
                     NB = NBVAL( INB )
                     CALL XLAENV( 1, NB )
                     LWORK = MAX( 2*N, NB*N )
                     CALL DTIMMG( MAT, N, N, A, LDA, 0, 0 )
                     IC = 0
                     S1 = DSECND( )
   10                CONTINUE
                     CALL DSYTRF( UPLO, N, A, LDA, IWORK, B, LWORK,
     $                            INFO )
                     S2 = DSECND( )
                     TIME = S2 - S1
                     IC = IC + 1
                     IF( TIME.LT.TIMMIN ) THEN
                        CALL DTIMMG( MAT, N, N, A, LDA, 0, 0 )
                        GO TO 10
                     END IF
*
*                    Subtract the time used in DTIMMG.
*
                     ICL = 1
                     S1 = DSECND( )
   20                CONTINUE
                     S2 = DSECND( )
                     UNTIME = S2 - S1
                     ICL = ICL + 1
                     IF( ICL.LE.IC ) THEN
                        CALL DTIMMG( MAT, N, N, B, LDA, 0, 0 )
                        GO TO 20
                     END IF
*
                     TIME = ( TIME-UNTIME ) / DBLE( IC )
                     OPS = DOPLA( 'DSYTRF', N, N, 0, 0, NB )
                     RESLTS( INB, IN, I3, 1 ) = DMFLOP( OPS, TIME,
     $                  INFO )
*
   30             CONTINUE
               ELSE
*
*                 If DSYTRF was not timed, generate a matrix and
*                 factor it using DSYTRF anyway so that the factored
*                 form of the matrix can be used in timing the other
*                 routines.
*
                  CALL DTIMMG( MAT, N, N, A, LDA, 0, 0 )
                  NB = 1
                  CALL XLAENV( 1, NB )
                  CALL DSYTRF( UPLO, N, A, LDA, IWORK, B, LWORK, INFO )
               END IF
*
*              Time DSYTRI
*
               IF( TIMSUB( 3 ) ) THEN
                  CALL DLACPY( UPLO, N, N, A, LDA, B, LDA )
                  IC = 0
                  S1 = DSECND( )
   40             CONTINUE
                  CALL DSYTRI( UPLO, N, B, LDA, IWORK, WORK, INFO )
                  S2 = DSECND( )
                  TIME = S2 - S1
                  IC = IC + 1
                  IF( TIME.LT.TIMMIN ) THEN
                     CALL DLACPY( UPLO, N, N, A, LDA, B, LDA )
                     GO TO 40
                  END IF
*
*                 Subtract the time used in DLACPY.
*
                  ICL = 1
                  S1 = DSECND( )
   50             CONTINUE
                  S2 = DSECND( )
                  UNTIME = S2 - S1
                  ICL = ICL + 1
                  IF( ICL.LE.IC ) THEN
                     CALL DLACPY( UPLO, N, N, A, LDA, B, LDA )
                     GO TO 50
                  END IF
*
                  TIME = ( TIME-UNTIME ) / DBLE( IC )
                  OPS = DOPLA( 'DSYTRI', N, N, 0, 0, 0 )
                  RESLTS( 1, IN, I3, 3 ) = DMFLOP( OPS, TIME, INFO )
               END IF
*
*              Time DSYTRS
*
               IF( TIMSUB( 2 ) ) THEN
                  DO 80 I = 1, NNS
                     NRHS = NSVAL( I )
                     LDB = LDA
                     CALL DTIMMG( 0, N, NRHS, B, LDB, 0, 0 )
                     IC = 0
                     S1 = DSECND( )
   60                CONTINUE
                     CALL DSYTRS( UPLO, N, NRHS, A, LDA, IWORK, B, LDB,
     $                            INFO )
                     S2 = DSECND( )
                     TIME = S2 - S1
                     IC = IC + 1
                     IF( TIME.LT.TIMMIN ) THEN
                        CALL DTIMMG( 0, N, NRHS, B, LDB, 0, 0 )
                        GO TO 60
                     END IF
*
*                    Subtract the time used in DTIMMG.
*
                     ICL = 1
                     S1 = DSECND( )
   70                CONTINUE
                     S2 = DSECND( )
                     UNTIME = S2 - S1
                     ICL = ICL + 1
                     IF( ICL.LE.IC ) THEN
                        CALL DTIMMG( 0, N, NRHS, B, LDB, 0, 0 )
                        GO TO 70
                     END IF
*
                     TIME = ( TIME-UNTIME ) / DBLE( IC )
                     OPS = DOPLA( 'DSYTRS', N, NRHS, 0, 0, 0 )
                     RESLTS( I, IN, I3, 2 ) = DMFLOP( OPS, TIME, INFO )
   80             CONTINUE
               END IF
   90       CONTINUE
  100    CONTINUE
  110 CONTINUE
*
*     Print tables of results for each timed routine.
*
      DO 140 ISUB = 1, NSUBS
         IF( .NOT.TIMSUB( ISUB ) )
     $      GO TO 140
         WRITE( NOUT, FMT = 9998 )SUBNAM( ISUB )
         IF( NLDA.GT.1 ) THEN
            DO 120 I = 1, NLDA
               WRITE( NOUT, FMT = 9997 )I, LDAVAL( I )
  120       CONTINUE
         END IF
         WRITE( NOUT, FMT = * )
         DO 130 IUPLO = 1, 2
            WRITE( NOUT, FMT = 9996 )SUBNAM( ISUB ), UPLOS( IUPLO )
            I3 = ( IUPLO-1 )*NLDA + 1
            IF( ISUB.EQ.1 ) THEN
               CALL DPRTBL( 'NB', 'N', NNB, NBVAL, NN, NVAL, NLDA,
     $                      RESLTS( 1, 1, I3, 1 ), LDR1, LDR2, NOUT )
            ELSE IF( ISUB.EQ.2 ) THEN
               CALL DPRTBL( 'NRHS', 'N', NNS, NSVAL, NN, NVAL, NLDA,
     $                      RESLTS( 1, 1, I3, 2 ), LDR1, LDR2, NOUT )
            ELSE IF( ISUB.EQ.3 ) THEN
               CALL DPRTBL( ' ', 'N', 1, NBVAL, NN, NVAL, NLDA,
     $                      RESLTS( 1, 1, I3, 3 ), LDR1, LDR2, NOUT )
            END IF
  130    CONTINUE
  140 CONTINUE
*
  150 CONTINUE
 9999 FORMAT( 1X, A6, ' timing run not attempted' )
 9998 FORMAT( / ' *** Speed of ', A6, ' in megaflops ***' )
 9997 FORMAT( 5X, 'line ', I2, ' with LDA = ', I5 )
 9996 FORMAT( 5X, A6, ' with UPLO = ''', A1, '''', / )
      RETURN
*
*     End of DTIMSY
*
      END
      SUBROUTINE DTIMTB( LINE, NN, NVAL, NK, KVAL, NNS, NSVAL, NLDA,
     $                   LDAVAL, TIMMIN, A, B, RESLTS, LDR1, LDR2, LDR3,
     $                   NOUT )
*
*  -- LAPACK timing routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     March 31, 1993
*
*     .. Scalar Arguments ..
      CHARACTER*80       LINE
      INTEGER            LDR1, LDR2, LDR3, NK, NLDA, NN, NNS, NOUT
      DOUBLE PRECISION   TIMMIN
*     ..
*     .. Array Arguments ..
      INTEGER            KVAL( * ), LDAVAL( * ), NSVAL( * ), NVAL( * )
      DOUBLE PRECISION   A( * ), B( * ), RESLTS( LDR1, LDR2, LDR3, * )
*     ..
*
*  Purpose
*  =======
*
*  DTIMTB times DTBTRS.
*
*  Arguments
*  =========
*
*  LINE    (input) CHARACTER*80
*          The input line that requested this routine.  The first six
*          characters contain either the name of a subroutine or a
*          generic path name.  The remaining characters may be used to
*          specify the individual routines to be timed.  See ATIMIN for
*          a full description of the format of the input line.
*
*  NN      (input) INTEGER
*          The number of values of N contained in the vector NVAL.
*
*  NVAL    (input) INTEGER array, dimension (NN)
*          The values of the matrix size N.
*
*  NK      (input) INTEGER
*          The number of values of K contained in the vector KVAL.
*
*  KVAL    (input) INTEGER array, dimension (NK)
*          The values of the band width K.
*
*  NNS     (input) INTEGER
*          The number of values of NRHS contained in the vector NSVAL.
*
*  NSVAL   (input) INTEGER array, dimension (NNS)
*          The values of the number of right hand sides NRHS.
*
*  NLDA    (input) INTEGER
*          The number of values of LDA contained in the vector LDAVAL.
*
*  LDAVAL  (input) INTEGER array, dimension (NLDA)
*          The values of the leading dimension of the array A.
*
*  TIMMIN  (input) DOUBLE PRECISION
*          The minimum time a subroutine will be timed.
*
*  A       (workspace) DOUBLE PRECISION array, dimension (LDAMAX*NMAX)
*          where LDAMAX and NMAX are the maximum values permitted
*          for LDA and N.
*
*  B       (workspace) DOUBLE PRECISION array, dimension (LDAMAX*NMAX)
*
*  RESLTS  (output) DOUBLE PRECISION array, dimension
*                   (LDR1,LDR2,LDR3,NSUBS)
*          The timing results for each subroutine over the relevant
*          values of N, NB, and LDA.
*
*  LDR1    (input) INTEGER
*          The first dimension of RESLTS.  LDR1 >= max(1,NNB).
*
*  LDR2    (input) INTEGER
*          The second dimension of RESLTS.  LDR2 >= max(1,NN).
*
*  LDR3    (input) INTEGER
*          The third dimension of RESLTS.  LDR3 >= max(1,2*NLDA).
*
*  NOUT    (input) INTEGER
*          The unit number for output.
*
*  =====================================================================
*
*     .. Parameters ..
      INTEGER            NSUBS
      PARAMETER          ( NSUBS = 1 )
*     ..
*     .. Local Scalars ..
      CHARACTER          UPLO
      CHARACTER*3        PATH
      CHARACTER*6        CNAME
      INTEGER            I, I3, IC, ICL, IK, ILDA, IN, INFO, ISUB,
     $                   IUPLO, K, LDA, LDB, MAT, N, NRHS
      DOUBLE PRECISION   OPS, S1, S2, TIME, UNTIME
*     ..
*     .. Local Arrays ..
      LOGICAL            TIMSUB( NSUBS )
      CHARACTER          UPLOS( 2 )
      CHARACTER*6        SUBNAM( NSUBS )
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      DOUBLE PRECISION   DMFLOP, DOPLA, DSECND
      EXTERNAL           LSAME, DMFLOP, DOPLA, DSECND
*     ..
*     .. External Subroutines ..
      EXTERNAL           ATIMCK, ATIMIN, DPRTBL, DTBTRS, DTIMMG
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          DBLE, MAX, MIN
*     ..
*     .. Data statements ..
      DATA               SUBNAM / 'DTBTRS' /
      DATA               UPLOS / 'U', 'L' /
*     ..
*     .. Executable Statements ..
*
*     Extract the timing request from the input line.
*
      PATH( 1: 1 ) = 'Double precision'
      PATH( 2: 3 ) = 'TB'
      CALL ATIMIN( PATH, LINE, NSUBS, SUBNAM, TIMSUB, NOUT, INFO )
      IF( INFO.NE.0 )
     $   GO TO 110
*
*     Check that K+1 <= LDA for the input values.
*
      CNAME = LINE( 1: 6 )
      CALL ATIMCK( 0, CNAME, NK, KVAL, NLDA, LDAVAL, NOUT, INFO )
      IF( INFO.GT.0 ) THEN
         WRITE( NOUT, FMT = 9999 )CNAME
         GO TO 110
      END IF
*
*     Do for each value of N:
*
      DO 100 IN = 1, NN
         N = NVAL( IN )
         LDB = N
*
*        Do first for UPLO = 'U', then for UPLO = 'L'
*
         DO 60 IUPLO = 1, 2
            UPLO = UPLOS( IUPLO )
            IF( LSAME( UPLO, 'U' ) ) THEN
               MAT = 11
            ELSE
               MAT = -11
            END IF
*
*           Do for each value of LDA:
*
            DO 50 ILDA = 1, NLDA
               LDA = LDAVAL( ILDA )
               I3 = ( IUPLO-1 )*NLDA + ILDA
*
*              Do for each value of the band width K:
*
               DO 40 IK = 1, NK
                  K = KVAL( IK )
                  K = MAX( 0, MIN( K, N-1 ) )
*
*                 Time DTBTRS
*
                  IF( TIMSUB( 1 ) ) THEN
                     CALL DTIMMG( MAT, N, N, A, LDA, K, K )
                     DO 30 I = 1, NNS
                        NRHS = NSVAL( I )
                        CALL DTIMMG( 0, N, NRHS, B, LDB, 0, 0 )
                        IC = 0
                        S1 = DSECND( )
   10                   CONTINUE
                        CALL DTBTRS( UPLO, 'No transpose', 'Non-unit',
     $                               N, K, NRHS, A, LDA, B, LDB, INFO )
                        S2 = DSECND( )
                        TIME = S2 - S1
                        IC = IC + 1
                        IF( TIME.LT.TIMMIN ) THEN
                           CALL DTIMMG( 0, N, NRHS, B, LDB, 0, 0 )
                           GO TO 10
                        END IF
*
*                       Subtract the time used in DTIMMG.
*
                        ICL = 1
                        S1 = DSECND( )
   20                   CONTINUE
                        S2 = DSECND( )
                        UNTIME = S2 - S1
                        ICL = ICL + 1
                        IF( ICL.LE.IC ) THEN
                           CALL DTIMMG( 0, N, NRHS, B, LDB, 0, 0 )
                           GO TO 20
                        END IF
*
                        TIME = ( TIME-UNTIME ) / DBLE( IC )
                        OPS = DOPLA( 'DTBTRS', N, NRHS, K, K, 0 )
                        RESLTS( I, IK, I3, 1 ) = DMFLOP( OPS, TIME,
     $                     INFO )
   30                CONTINUE
                  END IF
   40          CONTINUE
   50       CONTINUE
   60    CONTINUE
*
*        Print a table of results.
*
         DO 90 ISUB = 1, NSUBS
            IF( .NOT.TIMSUB( ISUB ) )
     $         GO TO 90
*
*           Print header for routine names.
*
            IF( IN.EQ.1 .OR. CNAME.EQ.'DTB   ' ) THEN
               WRITE( NOUT, FMT = 9998 )SUBNAM( ISUB )
               IF( NLDA.EQ.1 ) THEN
                  WRITE( NOUT, FMT = 9997 )LDAVAL( 1 )
               ELSE
                  DO 70 I = 1, NLDA
                     WRITE( NOUT, FMT = 9996 )I, LDAVAL( I )
   70             CONTINUE
               END IF
            END IF
*
            DO 80 IUPLO = 1, 2
               WRITE( NOUT, FMT = 9995 )SUBNAM( ISUB ), N,
     $            UPLOS( IUPLO )
               I3 = ( IUPLO-1 )*NLDA + 1
               IF( ISUB.EQ.1 ) THEN
                  CALL DPRTBL( 'NRHS', 'K', NNS, NSVAL, NK, KVAL, NLDA,
     $                         RESLTS( 1, 1, I3, 1 ), LDR1, LDR2, NOUT )
               END IF
   80       CONTINUE
   90    CONTINUE
  100 CONTINUE
*
  110 CONTINUE
 9999 FORMAT( 1X, A6, ' timing run not attempted', / )
 9998 FORMAT( / ' *** Speed of ', A6, ' in megaflops ***' )
 9997 FORMAT( 5X, 'with LDA = ', I5 )
 9996 FORMAT( 5X, 'line ', I2, ' with LDA = ', I5 )
 9995 FORMAT( / 5X, A6, ' with M =', I6, ', UPLO = ''', A1, '''', / )
      RETURN
*
*     End of DTIMTB
*
      END
      SUBROUTINE DTIMTD( LINE, NM, MVAL, NN, NVAL, NNB, NBVAL, NXVAL,
     $                   NLDA, LDAVAL, TIMMIN, A, B, D, TAU, WORK,
     $                   RESLTS, LDR1, LDR2, LDR3, NOUT )
*
*  -- LAPACK timing routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     March 31, 1993
*
*     .. Scalar Arguments ..
      CHARACTER*80       LINE
      INTEGER            LDR1, LDR2, LDR3, NLDA, NM, NN, NNB, NOUT
      DOUBLE PRECISION   TIMMIN
*     ..
*     .. Array Arguments ..
      INTEGER            LDAVAL( * ), MVAL( * ), NBVAL( * ), NVAL( * ),
     $                   NXVAL( * )
      DOUBLE PRECISION   A( * ), B( * ), D( * ),
     $                   RESLTS( LDR1, LDR2, LDR3, * ), TAU( * ),
     $                   WORK( * )
*     ..
*
*  Purpose
*  =======
*
*  DTIMTD times the LAPACK routines DSYTRD, DORGTR, and DORMTR and the
*  EISPACK routine TRED1.
*
*  Arguments
*  =========
*
*  LINE    (input) CHARACTER*80
*          The input line that requested this routine.  The first six
*          characters contain either the name of a subroutine or a
*          generic path name.  The remaining characters may be used to
*          specify the individual routines to be timed.  See ATIMIN for
*          a full description of the format of the input line.
*
*  NM      (input) INTEGER
*          The number of values of M contained in the vector MVAL.
*
*  MVAL    (input) INTEGER array, dimension (NM)
*          The values of the matrix size M.
*
*  NN      (input) INTEGER
*          The number of values of N contained in the vector NVAL.
*
*  NVAL    (input) INTEGER array, dimension (NN)
*          The values of the matrix column dimension N.
*
*  NNB     (input) INTEGER
*          The number of values of NB and NX contained in the
*          vectors NBVAL and NXVAL.  The blocking parameters are used
*          in pairs (NB,NX).
*
*  NBVAL   (input) INTEGER array, dimension (NNB)
*          The values of the blocksize NB.
*
*  NXVAL   (input) INTEGER array, dimension (NNB)
*          The values of the crossover point NX.
*
*  NLDA    (input) INTEGER
*          The number of values of LDA contained in the vector LDAVAL.
*
*  LDAVAL  (input) INTEGER array, dimension (NLDA)
*          The values of the leading dimension of the array A.
*
*  TIMMIN  (input) DOUBLE PRECISION
*          The minimum time a subroutine will be timed.
*
*  A       (workspace) DOUBLE PRECISION array, dimension (LDAMAX*NMAX)
*          where LDAMAX and NMAX are the maximum values of LDA and N.
*
*  B       (workspace) DOUBLE PRECISION array, dimension (LDAMAX*NMAX)
*
*  D       (workspace) DOUBLE PRECISION array, dimension (2*NMAX-1)
*
*  TAU     (workspace) DOUBLE PRECISION array, dimension (NMAX)
*
*  WORK    (workspace) DOUBLE PRECISION array, dimension (NMAX*NBMAX)
*          where NBMAX is the maximum value of NB.
*
*  RESLTS  (workspace) DOUBLE PRECISION array, dimension
*                      (LDR1,LDR2,LDR3,4*NN+3)
*          The timing results for each subroutine over the relevant
*          values of M, (NB,NX), LDA, and N.
*
*  LDR1    (input) INTEGER
*          The first dimension of RESLTS.  LDR1 >= max(1,NNB).
*
*  LDR2    (input) INTEGER
*          The second dimension of RESLTS.  LDR2 >= max(1,NM).
*
*  LDR3    (input) INTEGER
*          The third dimension of RESLTS.  LDR3 >= max(1,2*NLDA).
*
*  NOUT    (input) INTEGER
*          The unit number for output.
*
*  Internal Parameters
*  ===================
*
*  MODE    INTEGER
*          The matrix type.  MODE = 3 is a geometric distribution of
*          eigenvalues.  See DLATMS for further details.
*
*  COND    DOUBLE PRECISION
*          The condition number of the matrix.  The singular values are
*          set to values from DMAX to DMAX/COND.
*
*  DMAX    DOUBLE PRECISION
*          The magnitude of the largest singular value.
*
*  =====================================================================
*
*     .. Parameters ..
      INTEGER            NSUBS
      PARAMETER          ( NSUBS = 4 )
      INTEGER            MODE
      DOUBLE PRECISION   COND, DMAX
      PARAMETER          ( MODE = 3, COND = 100.0D0, DMAX = 1.0D0 )
*     ..
*     .. Local Scalars ..
      CHARACTER          LAB1, LAB2, SIDE, TRANS, UPLO
      CHARACTER*3        PATH
      CHARACTER*6        CNAME
      INTEGER            I, I3, I4, IC, ICL, ILDA, IM, IN, INB, INFO,
     $                   ISIDE, ISUB, ITOFF, ITRAN, IUPLO, LDA, LW, M,
     $                   M1, N, N1, NB, NX
      DOUBLE PRECISION   OPS, S1, S2, TIME, UNTIME
*     ..
*     .. Local Arrays ..
      LOGICAL            TIMSUB( NSUBS )
      CHARACTER          SIDES( 2 ), TRANSS( 2 ), UPLOS( 2 )
      CHARACTER*6        SUBNAM( NSUBS )
      INTEGER            ISEED( 4 ), RESEED( 4 )
*     ..
*     .. External Functions ..
      DOUBLE PRECISION   DMFLOP, DOPLA, DSECND
      EXTERNAL           DMFLOP, DOPLA, DSECND
*     ..
*     .. External Subroutines ..
      EXTERNAL           ATIMCK, ATIMIN, DLACPY, DLATMS, DORGTR, DORMTR,
     $                   DPRTB3, DPRTBL, DSYTRD, DTIMMG, ICOPY, TRED1,
     $                   XLAENV
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          DBLE, MAX
*     ..
*     .. Data statements ..
      DATA               SUBNAM / 'DSYTRD', 'TRED1', 'DORGTR',
     $                   'DORMTR' /
      DATA               SIDES / 'L', 'R' / , TRANSS / 'N', 'T' / ,
     $                   UPLOS / 'U', 'L' /
      DATA               ISEED / 0, 0, 0, 1 /
*     ..
*     .. Executable Statements ..
*
*     Extract the timing request from the input line.
*
      PATH( 1: 1 ) = 'Double precision'
      PATH( 2: 3 ) = 'TD'
      CALL ATIMIN( PATH, LINE, NSUBS, SUBNAM, TIMSUB, NOUT, INFO )
      IF( INFO.NE.0 )
     $   GO TO 240
*
*     Check that M <= LDA for the input values.
*
      CNAME = LINE( 1: 6 )
      CALL ATIMCK( 2, CNAME, NM, MVAL, NLDA, LDAVAL, NOUT, INFO )
      IF( INFO.GT.0 ) THEN
         WRITE( NOUT, FMT = 9999 )CNAME
         GO TO 240
      END IF
*
*     Check that K <= LDA for DORMTR
*
      IF( TIMSUB( 4 ) ) THEN
         CALL ATIMCK( 3, CNAME, NN, NVAL, NLDA, LDAVAL, NOUT, INFO )
         IF( INFO.GT.0 ) THEN
            WRITE( NOUT, FMT = 9999 )SUBNAM( 4 )
            TIMSUB( 4 ) = .FALSE.
         END IF
      END IF
*
*     Do first for UPLO = 'U', then for UPLO = 'L'
*
      DO 150 IUPLO = 1, 2
         UPLO = UPLOS( IUPLO )
*
*        Do for each value of M:
*
         DO 140 IM = 1, NM
            M = MVAL( IM )
            CALL ICOPY( 4, ISEED, 1, RESEED, 1 )
*
*           Do for each value of LDA:
*
            DO 130 ILDA = 1, NLDA
               LDA = LDAVAL( ILDA )
               I3 = ( IUPLO-1 )*NLDA + ILDA
*
*              Do for each pair of values (NB, NX) in NBVAL and NXVAL.
*
               DO 120 INB = 1, NNB
                  NB = NBVAL( INB )
                  CALL XLAENV( 1, NB )
                  NX = NXVAL( INB )
                  CALL XLAENV( 3, NX )
                  LW = MAX( 1, M*MAX( 1, NB ) )
*
*                 Generate a test matrix of order M.
*
                  CALL ICOPY( 4, RESEED, 1, ISEED, 1 )
                  CALL DLATMS( M, M, 'Uniform', ISEED, 'Symmetric', TAU,
     $                         MODE, COND, DMAX, M, M, 'No packing', B,
     $                         LDA, WORK, INFO )
*
                  IF( TIMSUB( 2 ) .AND. INB.EQ.1 .AND. IUPLO.EQ.2 ) THEN
*
*                    TRED1:  Eispack reduction using orthogonal
*                    transformations.
*
                     CALL DLACPY( UPLO, M, M, B, LDA, A, LDA )
                     IC = 0
                     S1 = DSECND( )
   10                CONTINUE
                     CALL TRED1( LDA, M, A, D, D( M+1 ), D( M+1 ) )
                     S2 = DSECND( )
                     TIME = S2 - S1
                     IC = IC + 1
                     IF( TIME.LT.TIMMIN ) THEN
                        CALL DLACPY( UPLO, M, M, B, LDA, A, LDA )
                        GO TO 10
                     END IF
*
*                    Subtract the time used in DLACPY.
*
                     ICL = 1
                     S1 = DSECND( )
   20                CONTINUE
                     S2 = DSECND( )
                     UNTIME = S2 - S1
                     ICL = ICL + 1
                     IF( ICL.LE.IC ) THEN
                        CALL DLACPY( UPLO, M, M, B, LDA, A, LDA )
                        GO TO 20
                     END IF
*
                     TIME = ( TIME-UNTIME ) / DBLE( IC )
                     OPS = DOPLA( 'DSYTRD', M, M, -1, -1, NB )
                     RESLTS( INB, IM, ILDA, 2 ) = DMFLOP( OPS, TIME,
     $                  INFO )
                  END IF
*
                  IF( TIMSUB( 1 ) ) THEN
*
*                    DSYTRD:  Reduction to tridiagonal form
*
                     CALL DLACPY( UPLO, M, M, B, LDA, A, LDA )
                     IC = 0
                     S1 = DSECND( )
   30                CONTINUE
                     CALL DSYTRD( UPLO, M, A, LDA, D, D( M+1 ), TAU,
     $                            WORK, LW, INFO )
                     S2 = DSECND( )
                     TIME = S2 - S1
                     IC = IC + 1
                     IF( TIME.LT.TIMMIN ) THEN
                        CALL DLACPY( UPLO, M, M, B, LDA, A, LDA )
                        GO TO 30
                     END IF
*
*                    Subtract the time used in DLACPY.
*
                     ICL = 1
                     S1 = DSECND( )
   40                CONTINUE
                     S2 = DSECND( )
                     UNTIME = S2 - S1
                     ICL = ICL + 1
                     IF( ICL.LE.IC ) THEN
                        CALL DLACPY( UPLO, M, M, A, LDA, B, LDA )
                        GO TO 40
                     END IF
*
                     TIME = ( TIME-UNTIME ) / DBLE( IC )
                     OPS = DOPLA( 'DSYTRD', M, M, -1, -1, NB )
                     RESLTS( INB, IM, I3, 1 ) = DMFLOP( OPS, TIME,
     $                  INFO )
                  ELSE
*
*                    If DSYTRD was not timed, generate a matrix and
*                    factor it using DSYTRD anyway so that the factored
*                    form of the matrix can be used in timing the other
*                    routines.
*
                     CALL DLACPY( UPLO, M, M, B, LDA, A, LDA )
                     CALL DSYTRD( UPLO, M, A, LDA, D, D( M+1 ), TAU,
     $                            WORK, LW, INFO )
                  END IF
*
                  IF( TIMSUB( 3 ) ) THEN
*
*                    DORGTR:  Generate the orthogonal matrix Q from the
*                    reduction to Hessenberg form A = Q*H*Q'
*
                     CALL DLACPY( UPLO, M, M, A, LDA, B, LDA )
                     IC = 0
                     S1 = DSECND( )
   50                CONTINUE
                     CALL DORGTR( UPLO, M, B, LDA, TAU, WORK, LW, INFO )
                     S2 = DSECND( )
                     TIME = S2 - S1
                     IC = IC + 1
                     IF( TIME.LT.TIMMIN ) THEN
                        CALL DLACPY( UPLO, M, M, A, LDA, B, LDA )
                        GO TO 50
                     END IF
*
*                    Subtract the time used in DLACPY.
*
                     ICL = 1
                     S1 = DSECND( )
   60                CONTINUE
                     S2 = DSECND( )
                     UNTIME = S2 - S1
                     ICL = ICL + 1
                     IF( ICL.LE.IC ) THEN
                        CALL DLACPY( UPLO, M, M, A, LDA, B, LDA )
                        GO TO 60
                     END IF
*
                     TIME = ( TIME-UNTIME ) / DBLE( IC )
*
*                    Op count for DORGTR:  same as
*                       DORGQR( N-1, N-1, N-1, ... )
*
                     OPS = DOPLA( 'DORGQR', M-1, M-1, M-1, -1, NB )
                     RESLTS( INB, IM, I3, 3 ) = DMFLOP( OPS, TIME,
     $                  INFO )
                  END IF
*
                  IF( TIMSUB( 4 ) ) THEN
*
*                    DORMTR:  Multiply by Q stored as a product of
*                    elementary transformations
*
                     I4 = 3
                     DO 110 ISIDE = 1, 2
                        SIDE = SIDES( ISIDE )
                        DO 100 IN = 1, NN
                           N = NVAL( IN )
                           LW = MAX( 1, MAX( 1, NB )*N )
                           IF( ISIDE.EQ.1 ) THEN
                              M1 = M
                              N1 = N
                           ELSE
                              M1 = N
                              N1 = M
                           END IF
                           ITOFF = 0
                           DO 90 ITRAN = 1, 2
                              TRANS = TRANSS( ITRAN )
                              CALL DTIMMG( 0, M1, N1, B, LDA, 0, 0 )
                              IC = 0
                              S1 = DSECND( )
   70                         CONTINUE
                              CALL DORMTR( SIDE, UPLO, TRANS, M1, N1, A,
     $                                     LDA, TAU, B, LDA, WORK, LW,
     $                                     INFO )
                              S2 = DSECND( )
                              TIME = S2 - S1
                              IC = IC + 1
                              IF( TIME.LT.TIMMIN ) THEN
                                 CALL DTIMMG( 0, M1, N1, B, LDA, 0, 0 )
                                 GO TO 70
                              END IF
*
*                             Subtract the time used in DTIMMG.
*
                              ICL = 1
                              S1 = DSECND( )
   80                         CONTINUE
                              S2 = DSECND( )
                              UNTIME = S2 - S1
                              ICL = ICL + 1
                              IF( ICL.LE.IC ) THEN
                                 CALL DTIMMG( 0, M1, N1, B, LDA, 0, 0 )
                                 GO TO 80
                              END IF
*
                              TIME = ( TIME-UNTIME ) / DBLE( IC )
*
*                             Op count for DORMTR, SIDE='L':  same as
*                                DORMQR( 'L', TRANS, M-1, N, M-1, ...)
*
*                             Op count for DORMTR, SIDE='R':  same as
*                                DORMQR( 'R', TRANS, M, N-1, N-1, ...)
*
                              IF( ISIDE.EQ.1 ) THEN
                                 OPS = DOPLA( 'DORMQR', M1-1, N1, M1-1,
     $                                 -1, NB )
                              ELSE
                                 OPS = DOPLA( 'DORMQR', M1, N1-1, N1-1,
     $                                 1, NB )
                              END IF
*
                              RESLTS( INB, IM, I3,
     $                           I4+ITOFF+IN ) = DMFLOP( OPS, TIME,
     $                           INFO )
                              ITOFF = NN
   90                      CONTINUE
  100                   CONTINUE
                        I4 = I4 + 2*NN
  110                CONTINUE
                  END IF
*
  120          CONTINUE
  130       CONTINUE
  140    CONTINUE
  150 CONTINUE
*
*     Print tables of results for DSYTRD, TRED1, and DORGTR
*
      DO 180 ISUB = 1, NSUBS - 1
         IF( .NOT.TIMSUB( ISUB ) )
     $      GO TO 180
         WRITE( NOUT, FMT = 9998 )SUBNAM( ISUB )
         IF( NLDA.GT.1 ) THEN
            DO 160 I = 1, NLDA
               WRITE( NOUT, FMT = 9997 )I, LDAVAL( I )
  160       CONTINUE
         END IF
         IF( ISUB.EQ.2 ) THEN
            WRITE( NOUT, FMT = * )
            CALL DPRTB3( ' ', 'N', 1, NBVAL, NXVAL, NM, MVAL, NLDA,
     $                   RESLTS( 1, 1, 1, ISUB ), LDR1, LDR2, NOUT )
         ELSE
            I3 = 1
            DO 170 IUPLO = 1, 2
               WRITE( NOUT, FMT = 9996 )SUBNAM( ISUB ), UPLOS( IUPLO )
               CALL DPRTB3( '(  NB,  NX)', 'N', NNB, NBVAL, NXVAL, NM,
     $                      MVAL, NLDA, RESLTS( 1, 1, I3, ISUB ), LDR1,
     $                      LDR2, NOUT )
               I3 = I3 + NLDA
  170       CONTINUE
         END IF
  180 CONTINUE
*
*     Print tables of results for DORMTR
*
      ISUB = 4
      IF( TIMSUB( ISUB ) ) THEN
         I4 = 3
         DO 230 ISIDE = 1, 2
            IF( ISIDE.EQ.1 ) THEN
               LAB1 = 'M'
               LAB2 = 'N'
               IF( NLDA.GT.1 ) THEN
                  WRITE( NOUT, FMT = 9998 )SUBNAM( ISUB )
                  DO 190 I = 1, NLDA
                     WRITE( NOUT, FMT = 9997 )I, LDAVAL( I )
  190             CONTINUE
               END IF
            ELSE
               LAB1 = 'N'
               LAB2 = 'M'
            END IF
            DO 220 ITRAN = 1, 2
               DO 210 IN = 1, NN
                  I3 = 1
                  DO 200 IUPLO = 1, 2
                     WRITE( NOUT, FMT = 9995 )SUBNAM( ISUB ),
     $                  SIDES( ISIDE ), UPLOS( IUPLO ), TRANSS( ITRAN ),
     $                  LAB2, NVAL( IN )
                     CALL DPRTBL( 'NB', LAB1, NNB, NBVAL, NM, MVAL,
     $                            NLDA, RESLTS( 1, 1, I3, I4+IN ), LDR1,
     $                            LDR2, NOUT )
                     I3 = I3 + NLDA
  200             CONTINUE
  210          CONTINUE
               I4 = I4 + NN
  220       CONTINUE
  230    CONTINUE
      END IF
  240 CONTINUE
*
*     Print a table of results for each timed routine.
*
 9999 FORMAT( 1X, A6, ' timing run not attempted', / )
 9998 FORMAT( / ' *** Speed of ', A6, ' in megaflops *** ' )
 9997 FORMAT( 5X, 'line ', I2, ' with LDA = ', I5 )
 9996 FORMAT( / 5X, A6, ' with UPLO = ''', A1, '''', / )
 9995 FORMAT( / 5X, A6, ' with SIDE = ''', A1, ''', UPLO = ''', A1,
     $      ''', TRANS = ''', A1, ''', ', A1, ' =', I6, / )
      RETURN
*
*     End of DTIMTD
*
      END
      SUBROUTINE DTIMTP( LINE, NN, NVAL, NNS, NSVAL, LA, TIMMIN, A, B,
     $                   RESLTS, LDR1, LDR2, LDR3, NOUT )
*
*  -- LAPACK timing routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     March 31, 1993
*
*     .. Scalar Arguments ..
      CHARACTER*80       LINE
      INTEGER            LA, LDR1, LDR2, LDR3, NN, NNS, NOUT
      DOUBLE PRECISION   TIMMIN
*     ..
*     .. Array Arguments ..
      INTEGER            NSVAL( * ), NVAL( * )
      DOUBLE PRECISION   A( * ), B( * ), RESLTS( LDR1, LDR2, LDR3, * )
*     ..
*
*  Purpose
*  =======
*
*  DTIMTP times DTPTRI and -TRS.
*
*  Arguments
*  =========
*
*  LINE    (input) CHARACTER*80
*          The input line that requested this routine.  The first six
*          characters contain either the name of a subroutine or a
*          generic path name.  The remaining characters may be used to
*          specify the individual routines to be timed.  See ATIMIN for
*          a full description of the format of the input line.
*
*  NN      (input) INTEGER
*          The number of values of N contained in the vector NVAL.
*
*  NVAL    (input) INTEGER array, dimension (NN)
*          The values of the matrix size N.
*
*  NNS     (input) INTEGER
*          The number of values of NRHS contained in the vector NSVAL.
*
*  NSVAL   (input) INTEGER array, dimension (NNS)
*          The values of the number of right hand sides NRHS.
*
*  LA      (input) INTEGER
*          The size of the arrays A and B.
*
*  TIMMIN  (input) DOUBLE PRECISION
*          The minimum time a subroutine will be timed.
*
*  A       (workspace) DOUBLE PRECISION array, dimension (LA)
*
*  B       (workspace) DOUBLE PRECISION array, dimension (NMAX*NMAX)
*          where NMAX is the maximum value of N in NVAL.
*
*  RESLTS  (output) DOUBLE PRECISION array, dimension
*                   (LDR1,LDR2,LDR3,NSUBS)
*          The timing results for each subroutine over the relevant
*          values of N.
*
*  LDR1    (input) INTEGER
*          The first dimension of RESLTS.  LDR1 >= 1.
*
*  LDR2    (input) INTEGER
*          The second dimension of RESLTS.  LDR2 >= max(1,NN).
*
*  LDR3    (input) INTEGER
*          The third dimension of RESLTS.  LDR3 >= 2.
*
*  NOUT    (input) INTEGER
*          The unit number for output.
*
*  =====================================================================
*
*     .. Parameters ..
      INTEGER            NSUBS
      PARAMETER          ( NSUBS = 2 )
*     ..
*     .. Local Scalars ..
      CHARACTER          UPLO
      CHARACTER*3        PATH
      CHARACTER*6        CNAME
      INTEGER            I, IC, ICL, IN, INFO, ISUB, IUPLO, LDA, LDB,
     $                   MAT, N, NRHS
      DOUBLE PRECISION   OPS, S1, S2, TIME, UNTIME
*     ..
*     .. Local Arrays ..
      LOGICAL            TIMSUB( NSUBS )
      CHARACTER          UPLOS( 2 )
      CHARACTER*6        SUBNAM( NSUBS )
      INTEGER            IDUMMY( 1 ), LAVAL( 1 )
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      DOUBLE PRECISION   DMFLOP, DOPLA, DSECND
      EXTERNAL           LSAME, DMFLOP, DOPLA, DSECND
*     ..
*     .. External Subroutines ..
      EXTERNAL           ATIMCK, ATIMIN, DPRTBL, DTIMMG, DTPTRI, DTPTRS
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          DBLE, MOD
*     ..
*     .. Data statements ..
      DATA               SUBNAM / 'DTPTRI', 'DTPTRS' /
      DATA               UPLOS / 'U', 'L' /
*     ..
*     .. Executable Statements ..
*
*     Extract the timing request from the input line.
*
      PATH( 1: 1 ) = 'Double precision'
      PATH( 2: 3 ) = 'TP'
      CALL ATIMIN( PATH, LINE, NSUBS, SUBNAM, TIMSUB, NOUT, INFO )
      IF( INFO.NE.0 )
     $   GO TO 100
*
*     Check that N*(N+1)/2 <= LA for the input values.
*
      CNAME = LINE( 1: 6 )
      LAVAL( 1 ) = LA
      CALL ATIMCK( 4, CNAME, NN, NVAL, 1, LAVAL, NOUT, INFO )
      IF( INFO.GT.0 ) THEN
         WRITE( NOUT, FMT = 9999 )CNAME
         GO TO 100
      END IF
*
*     Do first for UPLO = 'U', then for UPLO = 'L'
*
      DO 70 IUPLO = 1, 2
         UPLO = UPLOS( IUPLO )
         IF( LSAME( UPLO, 'U' ) ) THEN
            MAT = 10
         ELSE
            MAT = -10
         END IF
*
*        Do for each value of N:
*
         DO 60 IN = 1, NN
            N = NVAL( IN )
            LDA = N*( N+1 ) / 2
            LDB = N
            IF( MOD( N, 2 ).EQ.0 )
     $         LDB = LDB + 1
*
*           Time DTPTRI
*
            IF( TIMSUB( 1 ) ) THEN
               CALL DTIMMG( MAT, N, N, A, LDA, 0, 0 )
               IC = 0
               S1 = DSECND( )
   10          CONTINUE
               CALL DTPTRI( UPLO, 'Non-unit', N, A, INFO )
               S2 = DSECND( )
               TIME = S2 - S1
               IC = IC + 1
               IF( TIME.LT.TIMMIN ) THEN
                  CALL DTIMMG( MAT, N, N, A, LDA, 0, 0 )
                  GO TO 10
               END IF
*
*              Subtract the time used in DTIMMG.
*
               ICL = 1
               S1 = DSECND( )
   20          CONTINUE
               S2 = DSECND( )
               UNTIME = S2 - S1
               ICL = ICL + 1
               IF( ICL.LE.IC ) THEN
                  CALL DTIMMG( MAT, N, N, A, LDA, 0, 0 )
                  GO TO 20
               END IF
*
               TIME = ( TIME-UNTIME ) / DBLE( IC )
               OPS = DOPLA( 'DTPTRI', N, N, 0, 0, 0 )
               RESLTS( 1, IN, IUPLO, 1 ) = DMFLOP( OPS, TIME, INFO )
            ELSE
*
*              Generate a triangular matrix A.
*
               CALL DTIMMG( MAT, N, N, A, LDA, 0, 0 )
            END IF
*
*           Time DTPTRS
*
            IF( TIMSUB( 2 ) ) THEN
               DO 50 I = 1, NNS
                  NRHS = NSVAL( I )
                  CALL DTIMMG( 0, N, NRHS, B, LDB, 0, 0 )
                  IC = 0
                  S1 = DSECND( )
   30             CONTINUE
                  CALL DTPTRS( UPLO, 'No transpose', 'Non-unit', N,
     $                         NRHS, A, B, LDB, INFO )
                  S2 = DSECND( )
                  TIME = S2 - S1
                  IC = IC + 1
                  IF( TIME.LT.TIMMIN ) THEN
                     CALL DTIMMG( 0, N, NRHS, B, LDB, 0, 0 )
                     GO TO 30
                  END IF
*
*                 Subtract the time used in DTIMMG.
*
                  ICL = 1
                  S1 = DSECND( )
   40             CONTINUE
                  S2 = DSECND( )
                  UNTIME = S2 - S1
                  ICL = ICL + 1
                  IF( ICL.LE.IC ) THEN
                     CALL DTIMMG( 0, N, NRHS, B, LDB, 0, 0 )
                     GO TO 40
                  END IF
*
                  TIME = ( TIME-UNTIME ) / DBLE( IC )
                  OPS = DOPLA( 'DTPTRS', N, NRHS, 0, 0, 0 )
                  RESLTS( I, IN, IUPLO, 2 ) = DMFLOP( OPS, TIME, INFO )
   50          CONTINUE
            END IF
   60    CONTINUE
   70 CONTINUE
*
*     Print a table of results.
*
      DO 90 ISUB = 1, NSUBS
         IF( .NOT.TIMSUB( ISUB ) )
     $      GO TO 90
         WRITE( NOUT, FMT = 9998 )SUBNAM( ISUB )
         DO 80 IUPLO = 1, 2
            WRITE( NOUT, FMT = 9997 )SUBNAM( ISUB ), UPLOS( IUPLO )
            IF( ISUB.EQ.1 ) THEN
               CALL DPRTBL( ' ', 'N', 1, IDUMMY, NN, NVAL, 1,
     $                      RESLTS( 1, 1, IUPLO, 1 ), LDR1, LDR2, NOUT )
            ELSE IF( ISUB.EQ.2 ) THEN
               CALL DPRTBL( 'NRHS', 'N', NNS, NSVAL, NN, NVAL, 1,
     $                      RESLTS( 1, 1, IUPLO, 2 ), LDR1, LDR2, NOUT )
            END IF
   80    CONTINUE
   90 CONTINUE
*
  100 CONTINUE
 9999 FORMAT( 1X, A6, ' timing run not attempted', / )
 9998 FORMAT( / ' *** Speed of ', A6, ' in megaflops ***', / )
 9997 FORMAT( 5X, A6, ' with UPLO = ''', A1, '''', / )
      RETURN
*
*     End of DTIMTP
*
      END
      SUBROUTINE DTIMTR( LINE, NN, NVAL, NNS, NSVAL, NNB, NBVAL, NLDA,
     $                   LDAVAL, TIMMIN, A, B, RESLTS, LDR1, LDR2, LDR3,
     $                   NOUT )
*
*  -- LAPACK timing routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     March 31, 1993
*
*     .. Scalar Arguments ..
      CHARACTER*80       LINE
      INTEGER            LDR1, LDR2, LDR3, NLDA, NN, NNB, NNS, NOUT
      DOUBLE PRECISION   TIMMIN
*     ..
*     .. Array Arguments ..
      INTEGER            LDAVAL( * ), NBVAL( * ), NSVAL( * ), NVAL( * )
      DOUBLE PRECISION   A( * ), B( * ), RESLTS( LDR1, LDR2, LDR3, * )
*     ..
*
*  Purpose
*  =======
*
*  DTIMTR times DTRTRI and -TRS.
*
*  Arguments
*  =========
*
*  LINE    (input) CHARACTER*80
*          The input line that requested this routine.  The first six
*          characters contain either the name of a subroutine or a
*          generic path name.  The remaining characters may be used to
*          specify the individual routines to be timed.  See ATIMIN for
*          a full description of the format of the input line.
*
*  NN      (input) INTEGER
*          The number of values of N contained in the vector NVAL.
*
*  NVAL    (input) INTEGER array, dimension (NN)
*          The values of the matrix size N.
*
*  NNS     (input) INTEGER
*          The number of values of NRHS contained in the vector NSVAL.
*
*  NSVAL   (input) INTEGER array, dimension (NNS)
*          The values of the number of right hand sides NRHS.
*
*  NNB     (input) INTEGER
*          The number of values of NB contained in the vector NBVAL.
*
*  NBVAL   (input) INTEGER array, dimension (NNB)
*          The values of the blocksize NB.
*
*  NLDA    (input) INTEGER
*          The number of values of LDA contained in the vector LDAVAL.
*
*  LDAVAL  (input) INTEGER array, dimension (NLDA)
*          The values of the leading dimension of the array A.
*
*  TIMMIN  (input) DOUBLE PRECISION
*          The minimum time a subroutine will be timed.
*
*  A       (workspace) DOUBLE PRECISION array, dimension (LDAMAX*NMAX)
*          where LDAMAX and NMAX are the maximum values permitted
*          for LDA and N.
*
*  B       (workspace) DOUBLE PRECISION array, dimension (LDAMAX*NMAX)
*
*  RESLTS  (output) DOUBLE PRECISION array, dimension
*                   (LDR1,LDR2,LDR3,NSUBS)
*          The timing results for each subroutine over the relevant
*          values of N, NB, and LDA.
*
*  LDR1    (input) INTEGER
*          The first dimension of RESLTS.  LDR1 >= max(1,NNB).
*
*  LDR2    (input) INTEGER
*          The second dimension of RESLTS.  LDR2 >= max(1,NN).
*
*  LDR3    (input) INTEGER
*          The third dimension of RESLTS.  LDR3 >= max(1,2*NLDA).
*
*  NOUT    (input) INTEGER
*          The unit number for output.
*
*  =====================================================================
*
*     .. Parameters ..
      INTEGER            NSUBS
      PARAMETER          ( NSUBS = 2 )
*     ..
*     .. Local Scalars ..
      CHARACTER          UPLO
      CHARACTER*3        PATH
      CHARACTER*6        CNAME
      INTEGER            I, I3, IC, ICL, ILDA, IN, INB, INFO, ISUB,
     $                   IUPLO, LDA, LDB, MAT, N, NB, NRHS
      DOUBLE PRECISION   OPS, S1, S2, TIME, UNTIME
*     ..
*     .. Local Arrays ..
      LOGICAL            TIMSUB( NSUBS )
      CHARACTER          UPLOS( 2 )
      CHARACTER*6        SUBNAM( NSUBS )
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      DOUBLE PRECISION   DMFLOP, DOPLA, DSECND
      EXTERNAL           LSAME, DMFLOP, DOPLA, DSECND
*     ..
*     .. External Subroutines ..
      EXTERNAL           ATIMCK, ATIMIN, DPRTBL, DTIMMG, DTRTRI, DTRTRS,
     $                   XLAENV
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          DBLE
*     ..
*     .. Data statements ..
      DATA               SUBNAM / 'DTRTRI', 'DTRTRS' /
      DATA               UPLOS / 'U', 'L' /
*     ..
*     .. Executable Statements ..
*
*     Extract the timing request from the input line.
*
      PATH( 1: 1 ) = 'Double precision'
      PATH( 2: 3 ) = 'TR'
      CALL ATIMIN( PATH, LINE, NSUBS, SUBNAM, TIMSUB, NOUT, INFO )
      IF( INFO.NE.0 )
     $   GO TO 130
*
*     Check that N <= LDA for the input values.
*
      CNAME = LINE( 1: 6 )
      CALL ATIMCK( 2, CNAME, NN, NVAL, NLDA, LDAVAL, NOUT, INFO )
      IF( INFO.GT.0 ) THEN
         WRITE( NOUT, FMT = 9999 )CNAME
         GO TO 130
      END IF
*
*     Do first for UPLO = 'U', then for UPLO = 'L'
*
      DO 90 IUPLO = 1, 2
         UPLO = UPLOS( IUPLO )
         IF( LSAME( UPLO, 'U' ) ) THEN
            MAT = 9
         ELSE
            MAT = -9
         END IF
*
*        Do for each value of N:
*
         DO 80 IN = 1, NN
            N = NVAL( IN )
*
*           Do for each value of LDA:
*
            DO 70 ILDA = 1, NLDA
               LDA = LDAVAL( ILDA )
               I3 = ( IUPLO-1 )*NLDA + ILDA
*
*              Do for each value of NB in NBVAL.  Only the blocked
*              routines are timed in this loop since the other routines
*              are independent of NB.
*
               IF( TIMSUB( 1 ) ) THEN
                  DO 30 INB = 1, NNB
                     NB = NBVAL( INB )
                     CALL XLAENV( 1, NB )
*
*                    Time DTRTRI
*
                     CALL DTIMMG( MAT, N, N, A, LDA, 0, 0 )
                     IC = 0
                     S1 = DSECND( )
   10                CONTINUE
                     CALL DTRTRI( UPLO, 'Non-unit', N, A, LDA, INFO )
                     S2 = DSECND( )
                     TIME = S2 - S1
                     IC = IC + 1
                     IF( TIME.LT.TIMMIN ) THEN
                        CALL DTIMMG( MAT, N, N, A, LDA, 0, 0 )
                        GO TO 10
                     END IF
*
*                    Subtract the time used in DTIMMG.
*
                     ICL = 1
                     S1 = DSECND( )
   20                CONTINUE
                     S2 = DSECND( )
                     UNTIME = S2 - S1
                     ICL = ICL + 1
                     IF( ICL.LE.IC ) THEN
                        CALL DTIMMG( MAT, N, N, A, LDA, 0, 0 )
                        GO TO 20
                     END IF
*
                     TIME = ( TIME-UNTIME ) / DBLE( IC )
                     OPS = DOPLA( 'DTRTRI', N, N, 0, 0, NB )
                     RESLTS( INB, IN, I3, 1 ) = DMFLOP( OPS, TIME,
     $                  INFO )
   30             CONTINUE
               ELSE
*
*                 Generate a triangular matrix A.
*
                  CALL DTIMMG( MAT, N, N, A, LDA, 0, 0 )
               END IF
*
*              Time DTRTRS
*
               IF( TIMSUB( 2 ) ) THEN
                  DO 60 I = 1, NNS
                     NRHS = NSVAL( I )
                     LDB = LDA
                     CALL DTIMMG( 0, N, NRHS, B, LDB, 0, 0 )
                     IC = 0
                     S1 = DSECND( )
   40                CONTINUE
                     CALL DTRTRS( UPLO, 'No transpose', 'Non-unit', N,
     $                            NRHS, A, LDA, B, LDB, INFO )
                     S2 = DSECND( )
                     TIME = S2 - S1
                     IC = IC + 1
                     IF( TIME.LT.TIMMIN ) THEN
                        CALL DTIMMG( 0, N, NRHS, B, LDB, 0, 0 )
                        GO TO 40
                     END IF
*
*                    Subtract the time used in DTIMMG.
*
                     ICL = 1
                     S1 = DSECND( )
   50                CONTINUE
                     S2 = DSECND( )
                     UNTIME = S2 - S1
                     ICL = ICL + 1
                     IF( ICL.LE.IC ) THEN
                        CALL DTIMMG( 0, N, NRHS, B, LDB, 0, 0 )
                        GO TO 50
                     END IF
*
                     TIME = ( TIME-UNTIME ) / DBLE( IC )
                     OPS = DOPLA( 'DTRTRS', N, NRHS, 0, 0, 0 )
                     RESLTS( I, IN, I3, 2 ) = DMFLOP( OPS, TIME, INFO )
   60             CONTINUE
               END IF
   70       CONTINUE
   80    CONTINUE
   90 CONTINUE
*
*     Print a table of results.
*
      DO 120 ISUB = 1, NSUBS
         IF( .NOT.TIMSUB( ISUB ) )
     $      GO TO 120
         WRITE( NOUT, FMT = 9998 )SUBNAM( ISUB )
         IF( NLDA.GT.1 ) THEN
            DO 100 I = 1, NLDA
               WRITE( NOUT, FMT = 9997 )I, LDAVAL( I )
  100       CONTINUE
         END IF
         WRITE( NOUT, FMT = * )
         DO 110 IUPLO = 1, 2
            WRITE( NOUT, FMT = 9996 )SUBNAM( ISUB ), UPLOS( IUPLO )
            I3 = ( IUPLO-1 )*NLDA + 1
            IF( ISUB.EQ.1 ) THEN
               CALL DPRTBL( 'NB', 'N', NNB, NBVAL, NN, NVAL, NLDA,
     $                      RESLTS( 1, 1, I3, 1 ), LDR1, LDR2, NOUT )
            ELSE IF( ISUB.EQ.2 ) THEN
               CALL DPRTBL( 'NRHS', 'N', NNS, NSVAL, NN, NVAL, NLDA,
     $                      RESLTS( 1, 1, I3, 2 ), LDR1, LDR2, NOUT )
            END IF
  110    CONTINUE
  120 CONTINUE
*
  130 CONTINUE
 9999 FORMAT( 1X, A6, ' timing run not attempted', / )
 9998 FORMAT( / ' *** Speed of ', A6, ' in megaflops ***' )
 9997 FORMAT( 5X, 'line ', I2, ' with LDA = ', I5 )
 9996 FORMAT( 5X, A6, ' with UPLO = ''', A1, '''', / )
      RETURN
*
*     End of DTIMTR
*
      END
      SUBROUTINE ICOPY( N, SX, INCX, SY, INCY )
*
*  -- LAPACK auxiliary test routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     February 29, 1992
*
*     .. Scalar Arguments ..
      INTEGER            INCX, INCY, N
*     ..
*     .. Array Arguments ..
      INTEGER            SX( * ), SY( * )
*     ..
*
*  Purpose
*  =======
*
*  ICOPY copies an integer vector x to an integer vector y.
*  Uses unrolled loops for increments equal to 1.
*
*  Arguments
*  =========
*
*  N       (input) INTEGER
*          The length of the vectors SX and SY.
*
*  SX      (input) INTEGER array, dimension (1+(N-1)*abs(INCX))
*          The vector X.
*
*  INCX    (input) INTEGER
*          The spacing between consecutive elements of SX.
*
*  SY      (output) INTEGER array, dimension (1+(N-1)*abs(INCY))
*          The vector Y.
*
*  INCY    (input) INTEGER
*          The spacing between consecutive elements of SY.
*
*  =====================================================================
*
*     .. Local Scalars ..
      INTEGER            I, IX, IY, M, MP1
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MOD
*     ..
*     .. Executable Statements ..
*
      IF( N.LE.0 )
     $   RETURN
      IF( INCX.EQ.1 .AND. INCY.EQ.1 )
     $   GO TO 20
*
*     Code for unequal increments or equal increments not equal to 1
*
      IX = 1
      IY = 1
      IF( INCX.LT.0 )
     $   IX = ( -N+1 )*INCX + 1
      IF( INCY.LT.0 )
     $   IY = ( -N+1 )*INCY + 1
      DO 10 I = 1, N
         SY( IY ) = SX( IX )
         IX = IX + INCX
         IY = IY + INCY
   10 CONTINUE
      RETURN
*
*     Code for both increments equal to 1
*
*     Clean-up loop
*
   20 CONTINUE
      M = MOD( N, 7 )
      IF( M.EQ.0 )
     $   GO TO 40
      DO 30 I = 1, M
         SY( I ) = SX( I )
   30 CONTINUE
      IF( N.LT.7 )
     $   RETURN
   40 CONTINUE
      MP1 = M + 1
      DO 50 I = MP1, N, 7
         SY( I ) = SX( I )
         SY( I+1 ) = SX( I+1 )
         SY( I+2 ) = SX( I+2 )
         SY( I+3 ) = SX( I+3 )
         SY( I+4 ) = SX( I+4 )
         SY( I+5 ) = SX( I+5 )
         SY( I+6 ) = SX( I+6 )
   50 CONTINUE
      RETURN
*
*     End of ICOPY
*
      END
