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
      SUBROUTINE CDIV(AR,AI,BR,BI,CR,CI)
      DOUBLE PRECISION AR,AI,BR,BI,CR,CI
C
C     COMPLEX DIVISION, (CR,CI) = (AR,AI)/(BR,BI)
C
      DOUBLE PRECISION S,ARS,AIS,BRS,BIS
      S = DABS(BR) + DABS(BI)
      ARS = AR/S
      AIS = AI/S
      BRS = BR/S
      BIS = BI/S
      S = BRS**2 + BIS**2
      CR = (ARS*BRS + AIS*BIS)/S
      CI = (AIS*BRS - ARS*BIS)/S
      RETURN
      END
      DOUBLE PRECISION FUNCTION EPSLON (X)
      DOUBLE PRECISION X
C
C     ESTIMATE UNIT ROUNDOFF IN QUANTITIES OF SIZE X.
C
      DOUBLE PRECISION A,B,C,EPS
C
C     THIS PROGRAM SHOULD FUNCTION PROPERLY ON ALL SYSTEMS
C     SATISFYING THE FOLLOWING TWO ASSUMPTIONS,
C        1.  THE BASE USED IN REPRESENTING FLOATING POINT
C            NUMBERS IS NOT A POWER OF THREE.
C        2.  THE QUANTITY  A  IN STATEMENT 10 IS REPRESENTED TO
C            THE ACCURACY USED IN FLOATING POINT VARIABLES
C            THAT ARE STORED IN MEMORY.
C     THE STATEMENT NUMBER 10 AND THE GO TO 10 ARE INTENDED TO
C     FORCE OPTIMIZING COMPILERS TO GENERATE CODE SATISFYING
C     ASSUMPTION 2.
C     UNDER THESE ASSUMPTIONS, IT SHOULD BE TRUE THAT,
C            A  IS NOT EXACTLY EQUAL TO FOUR-THIRDS,
C            B  HAS A ZERO FOR ITS LAST BIT OR DIGIT,
C            C  IS NOT EXACTLY EQUAL TO ONE,
C            EPS  MEASURES THE SEPARATION OF 1.0 FROM
C                 THE NEXT LARGER FLOATING POINT NUMBER.
C     THE DEVELOPERS OF EISPACK WOULD APPRECIATE BEING INFORMED
C     ABOUT ANY SYSTEMS WHERE THESE ASSUMPTIONS DO NOT HOLD.
C
C     THIS VERSION DATED 4/6/83.
C
      A = 4.0D0/3.0D0
   10 B = A - 1.0D0
      C = B + B + B
      EPS = DABS(C-1.0D0)
      IF (EPS .EQ. 0.0D0) GO TO 10
      EPSLON = EPS*DABS(X)
      RETURN
      END
      SUBROUTINE HQR(NM,N,LOW,IGH,H,WR,WI,IERR)
C
      INTEGER I,J,K,L,M,N,EN,LL,MM,NA,NM,IGH,ITN,ITS,LOW,MP2,ENM2,IERR
      DOUBLE PRECISION H(NM,N),WR(N),WI(N)
      DOUBLE PRECISION P,Q,R,S,T,W,X,Y,ZZ,NORM,TST1,TST2
      LOGICAL NOTLAS
*
*     COMMON BLOCK TO RETURN OPERATION COUNT AND ITERATION COUNT
*     ITCNT IS INITIALIZED TO 0, OPS IS ONLY INCREMENTED
*     OPST IS USED TO ACCUMULATE SMALL CONTRIBUTIONS TO OPS
*     TO AVOID ROUNDOFF ERROR
*     .. COMMON BLOCKS ..
      COMMON /LATIME/ OPS, ITCNT
*     ..
*     .. SCALARS IN COMMON ..
      DOUBLE PRECISION OPS, ITCNT, OPST
*     ..
C
C     THIS SUBROUTINE IS A TRANSLATION OF THE ALGOL PROCEDURE HQR,
C     NUM. MATH. 14, 219-231(1970) BY MARTIN, PETERS, AND WILKINSON.
C     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 359-371(1971).
C
C     THIS SUBROUTINE FINDS THE EIGENVALUES OF A REAL
C     UPPER HESSENBERG MATRIX BY THE QR METHOD.
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
C        H CONTAINS THE UPPER HESSENBERG MATRIX.  INFORMATION ABOUT
C          THE TRANSFORMATIONS USED IN THE REDUCTION TO HESSENBERG
C          FORM BY  ELMHES  OR  ORTHES, IF PERFORMED, IS STORED
C          IN THE REMAINING TRIANGLE UNDER THE HESSENBERG MATRIX.
C
C     ON OUTPUT
C
C        H HAS BEEN DESTROYED.  THEREFORE, IT MUST BE SAVED
C          BEFORE CALLING  HQR  IF SUBSEQUENT CALCULATION AND
C          BACK TRANSFORMATION OF EIGENVECTORS IS TO BE PERFORMED.
C
C        WR AND WI CONTAIN THE REAL AND IMAGINARY PARTS,
C          RESPECTIVELY, OF THE EIGENVALUES.  THE EIGENVALUES
C          ARE UNORDERED EXCEPT THAT COMPLEX CONJUGATE PAIRS
C          OF VALUES APPEAR CONSECUTIVELY WITH THE EIGENVALUE
C          HAVING THE POSITIVE IMAGINARY PART FIRST.  IF AN
C          ERROR EXIT IS MADE, THE EIGENVALUES SHOULD BE CORRECT
C          FOR INDICES IERR+1,...,N.
C
C        IERR IS SET TO
C          ZERO       FOR NORMAL RETURN,
C          J          IF THE LIMIT OF 30*N ITERATIONS IS EXHAUSTED
C                     WHILE THE J-TH EIGENVALUE IS BEING SOUGHT.
C
C     QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO BURTON S. GARBOW,
C     MATHEMATICS AND COMPUTER SCIENCE DIV, ARGONNE NATIONAL LABORATORY
C
C     THIS VERSION DATED AUGUST 1983.
C     MODIFIED ON 11/1/89; ADJUSTING INDICES OF LOOPS
C       200, 210, 230, AND 240 TO INCREASE PERFORMANCE. JACK DONGARRA
C
C     ------------------------------------------------------------------
C
*
      EXTERNAL DLAMCH
      DOUBLE PRECISION DLAMCH, UNFL,OVFL,ULP,SMLNUM,SMALL
      IF (N.LE.0) RETURN
*
*
*     INITIALIZE
      ITCNT = 0
      OPST = 0
      IERR = 0
      K = 1
C     .......... STORE ROOTS ISOLATED BY BALANC
C                AND COMPUTE MATRIX NORM ..........
      DO 50 I = 1, N
         K = I
         IF (I .GE. LOW .AND. I .LE. IGH) GO TO 50
         WR(I) = H(I,I)
         WI(I) = 0.0D0
   50 CONTINUE
*
*        INCREMENT OPCOUNT FOR COMPUTING MATRIX NORM
         OPS = OPS + (IGH-LOW+1)*(IGH-LOW+2)/2
*
*     COMPUTE THE 1-NORM OF MATRIX H
*
      NORM = 0.0D0
      DO 5 J = LOW, IGH
         S = 0.0D0
         DO 4 I = LOW, MIN(IGH,J+1)
              S = S + DABS(H(I,J))
  4      CONTINUE
         NORM = MAX(NORM, S)
  5   CONTINUE
*
      UNFL = DLAMCH( 'SAFE MINIMUM' )
      OVFL = DLAMCH( 'OVERFLOW' )
      ULP = DLAMCH( 'EPSILON' )*DLAMCH( 'BASE' )
      SMLNUM = MAX( UNFL*( N / ULP ), N / ( ULP*OVFL ) )
      SMALL = MAX( SMLNUM, ULP*NORM )
C
      EN = IGH
      T = 0.0D0
      ITN = 30*N
C     .......... SEARCH FOR NEXT EIGENVALUES ..........
   60 IF (EN .LT. LOW) GO TO 1001
      ITS = 0
      NA = EN - 1
      ENM2 = NA - 1
C     .......... LOOK FOR SINGLE SMALL SUB-DIAGONAL ELEMENT
C                FOR L=EN STEP -1 UNTIL LOW DO -- ..........
*     REPLACE SPLITTING CRITERION WITH NEW ONE AS IN LAPACK
*
   70 DO 80 LL = LOW, EN
         L = EN + LOW - LL
         IF (L .EQ. LOW) GO TO 100
         S = DABS(H(L-1,L-1)) + DABS(H(L,L))
         IF (S .EQ. 0.0D0) S = NORM
         IF (DABS(H(L,L-1)) .LE. MAX(ULP*S,SMALL))  GO TO 100
   80 CONTINUE
C     .......... FORM SHIFT ..........
  100 CONTINUE
*
*        INCREMENT OP COUNT FOR CONVERGENCE TEST
         OPS = OPS + 2*(EN-L+1)
      X = H(EN,EN)
      IF (L .EQ. EN) GO TO 270
      Y = H(NA,NA)
      W = H(EN,NA) * H(NA,EN)
      IF (L .EQ. NA) GO TO 280
      IF (ITN .EQ. 0) GO TO 1000
      IF (ITS .NE. 10 .AND. ITS .NE. 20) GO TO 130
C     .......... FORM EXCEPTIONAL SHIFT ..........
*
*        INCREMENT OP COUNT FOR FORMING EXCEPTIONAL SHIFT
         OPS = OPS + (EN-LOW+6)
      T = T + X
C
      DO 120 I = LOW, EN
  120 H(I,I) = H(I,I) - X
C
      S = DABS(H(EN,NA)) + DABS(H(NA,ENM2))
      X = 0.75D0 * S
      Y = X
      W = -0.4375D0 * S * S
  130 ITS = ITS + 1
      ITN = ITN - 1
*
*       UPDATE ITERATION NUMBER
        ITCNT = 30*N - ITN
C     .......... LOOK FOR TWO CONSECUTIVE SMALL
C                SUB-DIAGONAL ELEMENTS.
C                FOR M=EN-2 STEP -1 UNTIL L DO -- ..........
*     REPLACE SPLITTING CRITERION WITH NEW ONE AS IN LAPACK
      DO 140 MM = L, ENM2
         M = ENM2 + L - MM
         ZZ = H(M,M)
         R = X - ZZ
         S = Y - ZZ
         P = (R * S - W) / H(M+1,M) + H(M,M+1)
         Q = H(M+1,M+1) - ZZ - R - S
         R = H(M+2,M+1)
         S = DABS(P) + DABS(Q) + DABS(R)
         P = P / S
         Q = Q / S
         R = R / S
         IF (M .EQ. L) GO TO 150
         TST1 = DABS(P)*(DABS(H(M-1,M-1)) + DABS(ZZ) + DABS(H(M+1,M+1)))
         TST2 = DABS(H(M,M-1))*(DABS(Q) + DABS(R))
         IF ( TST2 .LE. MAX(ULP*TST1,SMALL) ) GO TO 150
  140 CONTINUE
C
  150 CONTINUE
*
*        INCREMENT OPCOUNT FOR LOOP 140
         OPST = OPST + 20*(ENM2-M+1)
      MP2 = M + 2
C
      DO 160 I = MP2, EN
         H(I,I-2) = 0.0D0
         IF (I .EQ. MP2) GO TO 160
         H(I,I-3) = 0.0D0
  160 CONTINUE
C     .......... DOUBLE QR STEP INVOLVING ROWS L TO EN AND
C                COLUMNS M TO EN ..........
*
*        INCREMENT OPCOUNT FOR LOOP 260
         OPST = OPST + 18*(NA-M+1)
      DO 260 K = M, NA
         NOTLAS = K .NE. NA
         IF (K .EQ. M) GO TO 170
         P = H(K,K-1)
         Q = H(K+1,K-1)
         R = 0.0D0
         IF (NOTLAS) R = H(K+2,K-1)
         X = DABS(P) + DABS(Q) + DABS(R)
         IF (X .EQ. 0.0D0) GO TO 260
         P = P / X
         Q = Q / X
         R = R / X
  170    S = DSIGN(DSQRT(P*P+Q*Q+R*R),P)
         IF (K .EQ. M) GO TO 180
         H(K,K-1) = -S * X
         GO TO 190
  180    IF (L .NE. M) H(K,K-1) = -H(K,K-1)
  190    P = P + S
         X = P / S
         Y = Q / S
         ZZ = R / S
         Q = Q / P
         R = R / P
         IF (NOTLAS) GO TO 225
C     .......... ROW MODIFICATION ..........
*
*        INCREMENT OPCOUNT
         OPS = OPS + 6*(EN-K+1)
         DO 200 J = K, EN
            P = H(K,J) + Q * H(K+1,J)
            H(K,J) = H(K,J) - P * X
            H(K+1,J) = H(K+1,J) - P * Y
  200    CONTINUE
C
         J = MIN0(EN,K+3)
C     .......... COLUMN MODIFICATION ..........
*
*        INCREMENT OPCOUNT
         OPS = OPS + 6*(J-L+1)
         DO 210 I = L, J
            P = X * H(I,K) + Y * H(I,K+1)
            H(I,K) = H(I,K) - P
            H(I,K+1) = H(I,K+1) - P * Q
  210    CONTINUE
         GO TO 255
  225    CONTINUE
C     .......... ROW MODIFICATION ..........
*
*        INCREMENT OPCOUNT
         OPS = OPS + 10*(EN-K+1)
         DO 230 J = K, EN
            P = H(K,J) + Q * H(K+1,J) + R * H(K+2,J)
            H(K,J) = H(K,J) - P * X
            H(K+1,J) = H(K+1,J) - P * Y
            H(K+2,J) = H(K+2,J) - P * ZZ
  230    CONTINUE
C
         J = MIN0(EN,K+3)
C     .......... COLUMN MODIFICATION ..........
*
*        INCREMENT OPCOUNT
         OPS = OPS + 10*(J-L+1)
         DO 240 I = L, J
            P = X * H(I,K) + Y * H(I,K+1) + ZZ * H(I,K+2)
            H(I,K) = H(I,K) - P
            H(I,K+1) = H(I,K+1) - P * Q
            H(I,K+2) = H(I,K+2) - P * R
  240    CONTINUE
  255    CONTINUE
C
  260 CONTINUE
C
      GO TO 70
C     .......... ONE ROOT FOUND ..........
  270 WR(EN) = X + T
      WI(EN) = 0.0D0
      EN = NA
      GO TO 60
C     .......... TWO ROOTS FOUND ..........
  280 P = (Y - X) / 2.0D0
      Q = P * P + W
      ZZ = DSQRT(DABS(Q))
      X = X + T
*
*        INCREMENT OP COUNT FOR FINDING TWO ROOTS.
         OPST = OPST + 8
      IF (Q .LT. 0.0D0) GO TO 320
C     .......... REAL PAIR ..........
      ZZ = P + DSIGN(ZZ,P)
      WR(NA) = X + ZZ
      WR(EN) = WR(NA)
      IF (ZZ .NE. 0.0D0) WR(EN) = X - W / ZZ
      WI(NA) = 0.0D0
      WI(EN) = 0.0D0
      GO TO 330
C     .......... COMPLEX PAIR ..........
  320 WR(NA) = X + P
      WR(EN) = X + P
      WI(NA) = ZZ
      WI(EN) = -ZZ
  330 EN = ENM2
      GO TO 60
C     .......... SET ERROR -- ALL EIGENVALUES HAVE NOT
C                CONVERGED AFTER 30*N ITERATIONS ..........
 1000 IERR = EN
 1001 CONTINUE
*
*     COMPUTE FINAL OP COUNT
      OPS = OPS + OPST
      RETURN
      END
      SUBROUTINE HQR2(NM,N,LOW,IGH,H,WR,WI,Z,IERR)
C
      INTEGER I,J,K,L,M,N,EN,II,JJ,LL,MM,NA,NM,NN,
     X        IGH,ITN,ITS,LOW,MP2,ENM2,IERR
      DOUBLE PRECISION H(NM,N),WR(N),WI(N),Z(NM,N)
      DOUBLE PRECISION P,Q,R,S,T,W,X,Y,RA,SA,VI,VR,ZZ,NORM,TST1,TST2
      LOGICAL NOTLAS
*
*     COMMON BLOCK TO RETURN OPERATION COUNT AND ITERATION COUNT
*     ITCNT IS INITIALIZED TO 0, OPS IS ONLY INCREMENTED
*     OPST IS USED TO ACCUMULATE SMALL CONTRIBUTIONS TO OPS
*     TO AVOID ROUNDOFF ERROR
*     .. COMMON BLOCKS ..
      COMMON /LATIME/ OPS, ITCNT
*     ..
*     .. SCALARS IN COMMON ..
      DOUBLE PRECISION OPS, ITCNT, OPST
*     ..
C
C     THIS SUBROUTINE IS A TRANSLATION OF THE ALGOL PROCEDURE HQR2,
C     NUM. MATH. 16, 181-204(1970) BY PETERS AND WILKINSON.
C     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 372-395(1971).
C
C     THIS SUBROUTINE FINDS THE EIGENVALUES AND EIGENVECTORS
C     OF A REAL UPPER HESSENBERG MATRIX BY THE QR METHOD.  THE
C     EIGENVECTORS OF A REAL GENERAL MATRIX CAN ALSO BE FOUND
C     IF  ELMHES  AND  ELTRAN  OR  ORTHES  AND  ORTRAN  HAVE
C     BEEN USED TO REDUCE THIS GENERAL MATRIX TO HESSENBERG FORM
C     AND TO ACCUMULATE THE SIMILARITY TRANSFORMATIONS.
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
C        H CONTAINS THE UPPER HESSENBERG MATRIX.
C
C        Z CONTAINS THE TRANSFORMATION MATRIX PRODUCED BY  ELTRAN
C          AFTER THE REDUCTION BY  ELMHES, OR BY  ORTRAN  AFTER THE
C          REDUCTION BY  ORTHES, IF PERFORMED.  IF THE EIGENVECTORS
C          OF THE HESSENBERG MATRIX ARE DESIRED, Z MUST CONTAIN THE
C          IDENTITY MATRIX.
C
C     ON OUTPUT
C
C        H HAS BEEN DESTROYED.
C
C        WR AND WI CONTAIN THE REAL AND IMAGINARY PARTS,
C          RESPECTIVELY, OF THE EIGENVALUES.  THE EIGENVALUES
C          ARE UNORDERED EXCEPT THAT COMPLEX CONJUGATE PAIRS
C          OF VALUES APPEAR CONSECUTIVELY WITH THE EIGENVALUE
C          HAVING THE POSITIVE IMAGINARY PART FIRST.  IF AN
C          ERROR EXIT IS MADE, THE EIGENVALUES SHOULD BE CORRECT
C          FOR INDICES IERR+1,...,N.
C
C        Z CONTAINS THE REAL AND IMAGINARY PARTS OF THE EIGENVECTORS.
C          IF THE I-TH EIGENVALUE IS REAL, THE I-TH COLUMN OF Z
C          CONTAINS ITS EIGENVECTOR.  IF THE I-TH EIGENVALUE IS COMPLEX
C          WITH POSITIVE IMAGINARY PART, THE I-TH AND (I+1)-TH
C          COLUMNS OF Z CONTAIN THE REAL AND IMAGINARY PARTS OF ITS
C          EIGENVECTOR.  THE EIGENVECTORS ARE UNNORMALIZED.  IF AN
C          ERROR EXIT IS MADE, NONE OF THE EIGENVECTORS HAS BEEN FOUND.
C
C        IERR IS SET TO
C          ZERO       FOR NORMAL RETURN,
C          J          IF THE LIMIT OF 30*N ITERATIONS IS EXHAUSTED
C                     WHILE THE J-TH EIGENVALUE IS BEING SOUGHT.
C
C     CALLS CDIV FOR COMPLEX DIVISION.
C
C     QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO BURTON S. GARBOW,
C     MATHEMATICS AND COMPUTER SCIENCE DIV, ARGONNE NATIONAL LABORATORY
C
C     THIS VERSION DATED AUGUST 1983.
C
C     ------------------------------------------------------------------
*
      EXTERNAL DLAMCH
      DOUBLE PRECISION DLAMCH, UNFL,OVFL,ULP,SMLNUM,SMALL
      IF (N.LE.0) RETURN
*
*     INITIALIZE
*
      ITCNT = 0
      OPST = 0
C
      IERR = 0
      K = 1
C     .......... STORE ROOTS ISOLATED BY BALANC
C                AND COMPUTE MATRIX NORM ..........
      DO 50 I = 1, N
         IF (I .GE. LOW .AND. I .LE. IGH) GO TO 50
         WR(I) = H(I,I)
         WI(I) = 0.0D0
   50 CONTINUE

*        INCREMENT OPCOUNT FOR COMPUTING MATRIX NORM
         OPS = OPS + (IGH-LOW+1)*(IGH-LOW+2)/2
*
*     COMPUTE THE 1-NORM OF MATRIX H
*
      NORM = 0.0D0
      DO 5 J = LOW, IGH
         S = 0.0D0
         DO 4 I = LOW, MIN(IGH,J+1)
              S = S + DABS(H(I,J))
  4      CONTINUE
         NORM = MAX(NORM, S)
  5   CONTINUE
C
      UNFL = DLAMCH( 'SAFE MINIMUM' )
      OVFL = DLAMCH( 'OVERFLOW' )
      ULP = DLAMCH( 'EPSILON' )*DLAMCH( 'BASE' )
      SMLNUM = MAX( UNFL*( N / ULP ), N / ( ULP*OVFL ) )
      SMALL = MAX( SMLNUM, MIN( ( NORM*SMLNUM )*NORM, ULP*NORM ) )
C
      EN = IGH
      T = 0.0D0
      ITN = 30*N
C     .......... SEARCH FOR NEXT EIGENVALUES ..........
   60 IF (EN .LT. LOW) GO TO 340
      ITS = 0
      NA = EN - 1
      ENM2 = NA - 1
C     .......... LOOK FOR SINGLE SMALL SUB-DIAGONAL ELEMENT
C                FOR L=EN STEP -1 UNTIL LOW DO -- ..........
*     REPLACE SPLITTING CRITERION WITH NEW ONE AS IN LAPACK
*
   70 DO 80 LL = LOW, EN
         L = EN + LOW - LL
         IF (L .EQ. LOW) GO TO 100
         S = DABS(H(L-1,L-1)) + DABS(H(L,L))
         IF (S .EQ. 0.0D0) S = NORM
         IF ( ABS(H(L,L-1)) .LE. MAX(ULP*S,SMALL) )  GO TO 100
   80 CONTINUE
C     .......... FORM SHIFT ..........
  100 CONTINUE
*
*        INCREMENT OP COUNT FOR CONVERGENCE TEST
         OPS = OPS + 2*(EN-L+1)
      X = H(EN,EN)
      IF (L .EQ. EN) GO TO 270
      Y = H(NA,NA)
      W = H(EN,NA) * H(NA,EN)
      IF (L .EQ. NA) GO TO 280
      IF (ITN .EQ. 0) GO TO 1000
      IF (ITS .NE. 10 .AND. ITS .NE. 20) GO TO 130
C     .......... FORM EXCEPTIONAL SHIFT ..........
*
*        INCREMENT OP COUNT
         OPS = OPS + (EN-LOW+6)
      T = T + X
C
      DO 120 I = LOW, EN
  120 H(I,I) = H(I,I) - X
C
      S = DABS(H(EN,NA)) + DABS(H(NA,ENM2))
      X = 0.75D0 * S
      Y = X
      W = -0.4375D0 * S * S
  130 ITS = ITS + 1
      ITN = ITN - 1
*
*       UPDATE ITERATION NUMBER
        ITCNT = 30*N - ITN
C     .......... LOOK FOR TWO CONSECUTIVE SMALL
C                SUB-DIAGONAL ELEMENTS.
C                FOR M=EN-2 STEP -1 UNTIL L DO -- ..........
      DO 140 MM = L, ENM2
         M = ENM2 + L - MM
         ZZ = H(M,M)
         R = X - ZZ
         S = Y - ZZ
         P = (R * S - W) / H(M+1,M) + H(M,M+1)
         Q = H(M+1,M+1) - ZZ - R - S
         R = H(M+2,M+1)
         S = DABS(P) + DABS(Q) + DABS(R)
         P = P / S
         Q = Q / S
         R = R / S
         IF (M .EQ. L) GO TO 150
         TST1 = DABS(P)*(DABS(H(M-1,M-1)) + DABS(ZZ) + DABS(H(M+1,M+1)))
         TST2 = DABS(H(M,M-1))*(DABS(Q) + DABS(R))
         IF ( TST2 .LE. MAX(ULP*TST1,SMALL) ) GO TO 150
  140 CONTINUE
C
  150 CONTINUE
*
*        INCREMENT OPCOUNT FOR LOOP 140
         OPST = OPST + 20*(ENM2-M+1)
      MP2 = M + 2
C
      DO 160 I = MP2, EN
         H(I,I-2) = 0.0D0
         IF (I .EQ. MP2) GO TO 160
         H(I,I-3) = 0.0D0
  160 CONTINUE
C     .......... DOUBLE QR STEP INVOLVING ROWS L TO EN AND
C                COLUMNS M TO EN ..........
*
*        INCREMENT OPCOUNT FOR LOOP 260
         OPST = OPST + 18*(NA-M+1)
      DO 260 K = M, NA
         NOTLAS = K .NE. NA
         IF (K .EQ. M) GO TO 170
         P = H(K,K-1)
         Q = H(K+1,K-1)
         R = 0.0D0
         IF (NOTLAS) R = H(K+2,K-1)
         X = DABS(P) + DABS(Q) + DABS(R)
         IF (X .EQ. 0.0D0) GO TO 260
         P = P / X
         Q = Q / X
         R = R / X
  170    S = DSIGN(DSQRT(P*P+Q*Q+R*R),P)
         IF (K .EQ. M) GO TO 180
         H(K,K-1) = -S * X
         GO TO 190
  180    IF (L .NE. M) H(K,K-1) = -H(K,K-1)
  190    P = P + S
         X = P / S
         Y = Q / S
         ZZ = R / S
         Q = Q / P
         R = R / P
         IF (NOTLAS) GO TO 225
C     .......... ROW MODIFICATION ..........
*
*        INCREMENT OP COUNT FOR LOOP 200
         OPS = OPS + 6*(N-K+1)
         DO 200 J = K, N
            P = H(K,J) + Q * H(K+1,J)
            H(K,J) = H(K,J) - P * X
            H(K+1,J) = H(K+1,J) - P * Y
  200    CONTINUE
C
         J = MIN0(EN,K+3)
C     .......... COLUMN MODIFICATION ..........
*
*        INCREMENT OPCOUNT FOR LOOP 210
         OPS = OPS + 6*J
         DO 210 I = 1, J
            P = X * H(I,K) + Y * H(I,K+1)
            H(I,K) = H(I,K) - P
            H(I,K+1) = H(I,K+1) - P * Q
  210    CONTINUE
C     .......... ACCUMULATE TRANSFORMATIONS ..........
*
*        INCREMENT OPCOUNT FOR LOOP 220
         OPS = OPS + 6*(IGH-LOW + 1)
         DO 220 I = LOW, IGH
            P = X * Z(I,K) + Y * Z(I,K+1)
            Z(I,K) = Z(I,K) - P
            Z(I,K+1) = Z(I,K+1) - P * Q
  220    CONTINUE
         GO TO 255
  225    CONTINUE
C     .......... ROW MODIFICATION ..........
*
*        INCREMENT OPCOUNT FOR LOOP 230
         OPS = OPS + 10*(N-K+1)
         DO 230 J = K, N
            P = H(K,J) + Q * H(K+1,J) + R * H(K+2,J)
            H(K,J) = H(K,J) - P * X
            H(K+1,J) = H(K+1,J) - P * Y
            H(K+2,J) = H(K+2,J) - P * ZZ
  230    CONTINUE
C
         J = MIN0(EN,K+3)
C     .......... COLUMN MODIFICATION ..........
*
*        INCREMENT OPCOUNT FOR LOOP 240
         OPS = OPS + 10*J
         DO 240 I = 1, J
            P = X * H(I,K) + Y * H(I,K+1) + ZZ * H(I,K+2)
            H(I,K) = H(I,K) - P
            H(I,K+1) = H(I,K+1) - P * Q
            H(I,K+2) = H(I,K+2) - P * R
  240    CONTINUE
C     .......... ACCUMULATE TRANSFORMATIONS ..........
*
*        INCREMENT OPCOUNT FOR LOOP 250
         OPS = OPS + 10*(IGH-LOW+1)
         DO 250 I = LOW, IGH
            P = X * Z(I,K) + Y * Z(I,K+1) + ZZ * Z(I,K+2)
            Z(I,K) = Z(I,K) - P
            Z(I,K+1) = Z(I,K+1) - P * Q
            Z(I,K+2) = Z(I,K+2) - P * R
  250    CONTINUE
  255    CONTINUE
C
  260 CONTINUE
C
      GO TO 70
C     .......... ONE ROOT FOUND ..........
  270 H(EN,EN) = X + T
      WR(EN) = H(EN,EN)
      WI(EN) = 0.0D0
      EN = NA
      GO TO 60
C     .......... TWO ROOTS FOUND ..........
  280 P = (Y - X) / 2.0D0
      Q = P * P + W
      ZZ = DSQRT(DABS(Q))
      H(EN,EN) = X + T
      X = H(EN,EN)
      H(NA,NA) = Y + T
      IF (Q .LT. 0.0D0) GO TO 320
C     .......... REAL PAIR ..........
      ZZ = P + DSIGN(ZZ,P)
      WR(NA) = X + ZZ
      WR(EN) = WR(NA)
      IF (ZZ .NE. 0.0D0) WR(EN) = X - W / ZZ
      WI(NA) = 0.0D0
      WI(EN) = 0.0D0
      X = H(EN,NA)
      S = DABS(X) + DABS(ZZ)
      P = X / S
      Q = ZZ / S
      R = DSQRT(P*P+Q*Q)
      P = P / R
      Q = Q / R
*
*        INCREMENT OP COUNT FOR FINDING TWO ROOTS.
         OPST = OPST + 18
*
*        INCREMENT OP COUNT FOR MODIFICATION AND ACCUMULATION
*        IN LOOP 290, 300, 310
         OPS = OPS + 6*(N-NA+1) + 6*EN + 6*(IGH-LOW+1)
C     .......... ROW MODIFICATION ..........
      DO 290 J = NA, N
         ZZ = H(NA,J)
         H(NA,J) = Q * ZZ + P * H(EN,J)
         H(EN,J) = Q * H(EN,J) - P * ZZ
  290 CONTINUE
C     .......... COLUMN MODIFICATION ..........
      DO 300 I = 1, EN
         ZZ = H(I,NA)
         H(I,NA) = Q * ZZ + P * H(I,EN)
         H(I,EN) = Q * H(I,EN) - P * ZZ
  300 CONTINUE
C     .......... ACCUMULATE TRANSFORMATIONS ..........
      DO 310 I = LOW, IGH
         ZZ = Z(I,NA)
         Z(I,NA) = Q * ZZ + P * Z(I,EN)
         Z(I,EN) = Q * Z(I,EN) - P * ZZ
  310 CONTINUE
C
      GO TO 330
C     .......... COMPLEX PAIR ..........
  320 WR(NA) = X + P
      WR(EN) = X + P
      WI(NA) = ZZ
      WI(EN) = -ZZ
*
*        INCREMENT OP COUNT FOR FINDING COMPLEX PAIR.
         OPST = OPST + 9
  330 EN = ENM2
      GO TO 60
C     .......... ALL ROOTS FOUND.  BACKSUBSTITUTE TO FIND
C                VECTORS OF UPPER TRIANGULAR FORM ..........
  340 IF (NORM .EQ. 0.0D0) GO TO 1001
C     .......... FOR EN=N STEP -1 UNTIL 1 DO -- ..........
      DO 800 NN = 1, N
         EN = N + 1 - NN
         P = WR(EN)
         Q = WI(EN)
         NA = EN - 1
         IF (Q) 710, 600, 800
C     .......... REAL VECTOR ..........
  600    M = EN
         H(EN,EN) = 1.0D0
         IF (NA .EQ. 0) GO TO 800
C     .......... FOR I=EN-1 STEP -1 UNTIL 1 DO -- ..........
         DO 700 II = 1, NA
            I = EN - II
            W = H(I,I) - P
            R = 0.0D0
C
*
*        INCREMENT OP COUNT FOR LOOP 610
         OPST = OPST + 2*(EN - M+1)
            DO 610 J = M, EN
  610       R = R + H(I,J) * H(J,EN)
C
            IF (WI(I) .GE. 0.0D0) GO TO 630
            ZZ = W
            S = R
            GO TO 700
  630       M = I
            IF (WI(I) .NE. 0.0D0) GO TO 640
            T = W
            IF (T .NE. 0.0D0) GO TO 635
               TST1 = NORM
               T = TST1
  632          T = 0.01D0 * T
               TST2 = NORM + T
               IF (TST2 .GT. TST1) GO TO 632
  635       H(I,EN) = -R / T
            GO TO 680
C     .......... SOLVE REAL EQUATIONS ..........
  640       X = H(I,I+1)
            Y = H(I+1,I)
            Q = (WR(I) - P) * (WR(I) - P) + WI(I) * WI(I)
            T = (X * S - ZZ * R) / Q
*
*        INCREMENT OP COUNT FOR SOLVING REAL EQUATION.
         OPST = OPST + 13
            H(I,EN) = T
            IF (DABS(X) .LE. DABS(ZZ)) GO TO 650
            H(I+1,EN) = (-R - W * T) / X
            GO TO 680
  650       H(I+1,EN) = (-S - Y * T) / ZZ
C
C     .......... OVERFLOW CONTROL ..........
  680       T = DABS(H(I,EN))
            IF (T .EQ. 0.0D0) GO TO 700
            TST1 = T
            TST2 = TST1 + 1.0D0/TST1
            IF (TST2 .GT. TST1) GO TO 700
*
*        INCREMENT OP COUNT.
         OPST = OPST + (EN-I+1)
            DO 690 J = I, EN
               H(J,EN) = H(J,EN)/T
  690       CONTINUE
C
  700    CONTINUE
C     .......... END REAL VECTOR ..........
         GO TO 800
C     .......... COMPLEX VECTOR ..........
  710    M = NA
C     .......... LAST VECTOR COMPONENT CHOSEN IMAGINARY SO THAT
C                EIGENVECTOR MATRIX IS TRIANGULAR ..........
         IF (DABS(H(EN,NA)) .LE. DABS(H(NA,EN))) GO TO 720
         H(NA,NA) = Q / H(EN,NA)
         H(NA,EN) = -(H(EN,EN) - P) / H(EN,NA)
*
*        INCREMENT OP COUNT.
         OPST = OPST + 3
         GO TO 730
  720    CALL CDIV(0.0D0,-H(NA,EN),H(NA,NA)-P,Q,H(NA,NA),H(NA,EN))
*
*        INCREMENT OP COUNT IF (ABS(H(EN,NA)) .LE. ABS(H(NA,EN)))
         OPST = OPST + 16
  730    H(EN,NA) = 0.0D0
         H(EN,EN) = 1.0D0
         ENM2 = NA - 1
         IF (ENM2 .EQ. 0) GO TO 800
C     .......... FOR I=EN-2 STEP -1 UNTIL 1 DO -- ..........
         DO 795 II = 1, ENM2
            I = NA - II
            W = H(I,I) - P
            RA = 0.0D0
            SA = 0.0D0
C
*
*        INCREMENT OP COUNT FOR LOOP 760
         OPST = OPST + 4*(EN-M+1)
            DO 760 J = M, EN
               RA = RA + H(I,J) * H(J,NA)
               SA = SA + H(I,J) * H(J,EN)
  760       CONTINUE
C
            IF (WI(I) .GE. 0.0D0) GO TO 770
            ZZ = W
            R = RA
            S = SA
            GO TO 795
  770       M = I
            IF (WI(I) .NE. 0.0D0) GO TO 780
            CALL CDIV(-RA,-SA,W,Q,H(I,NA),H(I,EN))
*
*        INCREMENT OP COUNT FOR CDIV
         OPST = OPST + 16
            GO TO 790
C     .......... SOLVE COMPLEX EQUATIONS ..........
  780       X = H(I,I+1)
            Y = H(I+1,I)
            VR = (WR(I) - P) * (WR(I) - P) + WI(I) * WI(I) - Q * Q
            VI = (WR(I) - P) * 2.0D0 * Q
*
*        INCREMENT OPCOUNT (AVERAGE) FOR SOLVING COMPLEX EQUATIONS
         OPST = OPST + 42
            IF (VR .NE. 0.0D0 .OR. VI .NE. 0.0D0) GO TO 784
               TST1 = NORM * (DABS(W) + DABS(Q) + DABS(X)
     X                      + DABS(Y) + DABS(ZZ))
               VR = TST1
  783          VR = 0.01D0 * VR
               TST2 = TST1 + VR
               IF (TST2 .GT. TST1) GO TO 783
  784       CALL CDIV(X*R-ZZ*RA+Q*SA,X*S-ZZ*SA-Q*RA,VR,VI,
     X                H(I,NA),H(I,EN))
            IF (DABS(X) .LE. DABS(ZZ) + DABS(Q)) GO TO 785
            H(I+1,NA) = (-RA - W * H(I,NA) + Q * H(I,EN)) / X
            H(I+1,EN) = (-SA - W * H(I,EN) - Q * H(I,NA)) / X
            GO TO 790
  785       CALL CDIV(-R-Y*H(I,NA),-S-Y*H(I,EN),ZZ,Q,
     X                H(I+1,NA),H(I+1,EN))
C
C     .......... OVERFLOW CONTROL ..........
  790       T = DMAX1(DABS(H(I,NA)), DABS(H(I,EN)))
            IF (T .EQ. 0.0D0) GO TO 795
            TST1 = T
            TST2 = TST1 + 1.0D0/TST1
            IF (TST2 .GT. TST1) GO TO 795
*
*        INCREMENT OP COUNT.
         OPST = OPST + 2*(EN-I+1)
            DO 792 J = I, EN
               H(J,NA) = H(J,NA)/T
               H(J,EN) = H(J,EN)/T
  792       CONTINUE
C
  795    CONTINUE
C     .......... END COMPLEX VECTOR ..........
  800 CONTINUE
C     .......... END BACK SUBSTITUTION.
C                VECTORS OF ISOLATED ROOTS ..........
      DO 840 I = 1, N
         IF (I .GE. LOW .AND. I .LE. IGH) GO TO 840
C
         DO 820 J = I, N
  820    Z(I,J) = H(I,J)
C
  840 CONTINUE
C     .......... MULTIPLY BY TRANSFORMATION MATRIX TO GIVE
C                VECTORS OF ORIGINAL FULL MATRIX.
C                FOR J=N STEP -1 UNTIL LOW DO -- ..........
      DO 880 JJ = LOW, N
         J = N + LOW - JJ
         M = MIN0(J,IGH)
C
*
*        INCREMENT OP COUNT.
         OPS = OPS + 2*(IGH-LOW+1)*(M-LOW+1)
         DO 880 I = LOW, IGH
            ZZ = 0.0D0
C
            DO 860 K = LOW, M
  860       ZZ = ZZ + Z(I,K) * H(K,J)
C
            Z(I,J) = ZZ
  880 CONTINUE
C
      GO TO 1001
C     .......... SET ERROR -- ALL EIGENVALUES HAVE NOT
C                CONVERGED AFTER 30*N ITERATIONS ..........
 1000 IERR = EN
 1001 CONTINUE
*
*     COMPUTE FINAL OP COUNT
      OPS = OPS + OPST
      RETURN
      END
      SUBROUTINE IMTQL1(N,D,E,IERR)
*
*     EISPACK ROUTINE
*     MODIFIED FOR COMPARISON WITH LAPACK ROUTINES.
*
*     CONVERGENCE TEST WAS MODIFIED TO BE THE SAME AS IN DSTEQR.
*
C
      INTEGER I,J,L,M,N,II,MML,IERR
      DOUBLE PRECISION D(N),E(N)
      DOUBLE PRECISION B,C,F,G,P,R,S,TST1,TST2,PYTHAG
      DOUBLE PRECISION EPS, TST
      DOUBLE PRECISION DLAMCH
      external pythag, dlamch
*
*     COMMON BLOCK TO RETURN OPERATION COUNT AND ITERATION COUNT
*     ITCNT IS INITIALIZED TO 0, OPS IS ONLY INCREMENTED
*     OPST IS USED TO ACCUMULATE CONTRIBUTIONS TO OPS FROM
*     FUNCTION PYTHAG.  IT IS PASSED TO AND FROM PYTHAG
*     THROUGH COMMON BLOCK PYTHOP.
*     .. COMMON BLOCKS ..
      COMMON             / LATIME / OPS, ITCNT
      COMMON             / PYTHOP / OPST
*
*     .. SCALARS IN COMMON ..
      DOUBLE PRECISION   ITCNT, OPS, OPST
*     ..
C
C     THIS SUBROUTINE IS A TRANSLATION OF THE ALGOL PROCEDURE IMTQL1,
C     NUM. MATH. 12, 377-383(1968) BY MARTIN AND WILKINSON,
C     AS MODIFIED IN NUM. MATH. 15, 450(1970) BY DUBRULLE.
C     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 241-248(1971).
C
C     THIS SUBROUTINE FINDS THE EIGENVALUES OF A SYMMETRIC
C     TRIDIAGONAL MATRIX BY THE IMPLICIT QL METHOD.
C
C     ON INPUT
C
C        N IS THE ORDER OF THE MATRIX.
C
C        D CONTAINS THE DIAGONAL ELEMENTS OF THE INPUT MATRIX.
C
C        E CONTAINS THE SUBDIAGONAL ELEMENTS OF THE INPUT MATRIX
C          IN ITS LAST N-1 POSITIONS.  E(1) IS ARBITRARY.
C
C      ON OUTPUT
C
C        D CONTAINS THE EIGENVALUES IN ASCENDING ORDER.  IF AN
C          ERROR EXIT IS MADE, THE EIGENVALUES ARE CORRECT AND
C          ORDERED FOR INDICES 1,2,...IERR-1, BUT MAY NOT BE
C          THE SMALLEST EIGENVALUES.
C
C        E HAS BEEN DESTROYED.
C
C        IERR IS SET TO
C          ZERO       FOR NORMAL RETURN,
C          J          IF THE J-TH EIGENVALUE HAS NOT BEEN
C                     DETERMINED AFTER 40 ITERATIONS.
C
C     CALLS PYTHAG FOR  SQRT(A*A + B*B) .
C
C     QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO BURTON S. GARBOW,
C     MATHEMATICS AND COMPUTER SCIENCE DIV, ARGONNE NATIONAL LABORATORY
C
C     THIS VERSION DATED AUGUST 1983.
C
C     ------------------------------------------------------------------
C
      IERR = 0
      IF (N .EQ. 1) GO TO 1001
*
*        INITIALIZE ITERATION COUNT AND OPST
            ITCNT = 0
            OPST = 0
*
*     DETERMINE THE UNIT ROUNDOFF FOR THIS ENVIRONMENT.
*
      EPS = DLAMCH( 'EPSILON' )
C
      DO 100 I = 2, N
  100 E(I-1) = E(I)
C
      E(N) = 0.0D0
C
      DO 290 L = 1, N
         J = 0
C     .......... LOOK FOR SMALL SUB-DIAGONAL ELEMENT ..........
  105    DO 110 M = L, N
            IF (M .EQ. N) GO TO 120
            TST = ABS( E(M) )
            IF( TST .LE. EPS * ( ABS(D(M)) + ABS(D(M+1)) ) ) GO TO 120
*            TST1 = ABS(D(M)) + ABS(D(M+1))
*            TST2 = TST1 + ABS(E(M))
*            IF (TST2 .EQ. TST1) GO TO 120
  110    CONTINUE
C
  120    P = D(L)
*
*        INCREMENT OPCOUNT FOR FINDING SMALL SUBDIAGONAL ELEMENT.
            OPS = OPS + 2*( MIN(M,N-1)-L+1 )
         IF (M .EQ. L) GO TO 215
         IF (J .EQ. 40) GO TO 1000
         J = J + 1
C     .......... FORM SHIFT ..........
         G = (D(L+1) - P) / (2.0D0 * E(L))
         R = PYTHAG(G,1.0D0)
         G = D(M) - P + E(L) / (G + DSIGN(R,G))
*
*        INCREMENT OPCOUNT FOR FORMING SHIFT.
            OPS = OPS + 7
         S = 1.0D0
         C = 1.0D0
         P = 0.0D0
         MML = M - L
C     .......... FOR I=M-1 STEP -1 UNTIL L DO -- ..........
         DO 200 II = 1, MML
            I = M - II
            F = S * E(I)
            B = C * E(I)
            R = PYTHAG(F,G)
            E(I+1) = R
            IF (R .EQ. 0.0D0) GO TO 210
            S = F / R
            C = G / R
            G = D(I+1) - P
            R = (D(I) - G) * S + 2.0D0 * C * B
            P = S * R
            D(I+1) = G + P
            G = C * R - B
  200    CONTINUE
C
         D(L) = D(L) - P
         E(L) = G
         E(M) = 0.0D0
*
*        INCREMENT OPCOUNT FOR INNER LOOP.
            OPS = OPS + MML*14 + 1
*
*        INCREMENT ITERATION COUNTER
            ITCNT = ITCNT + 1
         GO TO 105
C     .......... RECOVER FROM UNDERFLOW ..........
  210    D(I+1) = D(I+1) - P
         E(M) = 0.0D0
*
*        INCREMENT OPCOUNT FOR INNER LOOP, WHEN UNDERFLOW OCCURS.
            OPS = OPS + 2+(II-1)*14 + 1
         GO TO 105
C     .......... ORDER EIGENVALUES ..........
  215    IF (L .EQ. 1) GO TO 250
C     .......... FOR I=L STEP -1 UNTIL 2 DO -- ..........
         DO 230 II = 2, L
            I = L + 2 - II
            IF (P .GE. D(I-1)) GO TO 270
            D(I) = D(I-1)
  230    CONTINUE
C
  250    I = 1
  270    D(I) = P
  290 CONTINUE
C
      GO TO 1001
C     .......... SET ERROR -- NO CONVERGENCE TO AN
C                EIGENVALUE AFTER 40 ITERATIONS ..........
 1000 IERR = L
 1001 CONTINUE
*
*     COMPUTE FINAL OP COUNT
      OPS = OPS + OPST
      RETURN
      END
      SUBROUTINE IMTQL2(NM,N,D,E,Z,IERR)
*
*     EISPACK ROUTINE.  MODIFIED FOR COMPARISON WITH LAPACK.
*
*     CONVERGENCE TEST WAS MODIFIED TO BE THE SAME AS IN DSTEQR.
*
C
      INTEGER I,J,K,L,M,N,II,NM,MML,IERR
      DOUBLE PRECISION D(N),E(N),Z(NM,N)
      DOUBLE PRECISION B,C,F,G,P,R,S,TST1,TST2,PYTHAG
      DOUBLE PRECISION EPS, TST
      DOUBLE PRECISION DLAMCH
      external pythag, dlamch
*
*     COMMON BLOCK TO RETURN OPERATION COUNT AND ITERATION COUNT
*     ITCNT IS INITIALIZED TO 0, OPS IS ONLY INCREMENTED
*     OPST IS USED TO ACCUMULATE CONTRIBUTIONS TO OPS FROM
*     FUNCTION PYTHAG.  IT IS PASSED TO AND FROM PYTHAG
*     THROUGH COMMON BLOCK PYTHOP.
*     .. COMMON BLOCKS ..
      COMMON             / LATIME / OPS, ITCNT
      COMMON             / PYTHOP / OPST
*     ..
*     .. SCALARS IN COMMON ..
      DOUBLE PRECISION   ITCNT, OPS, OPST
*     ..
C
C     THIS SUBROUTINE IS A TRANSLATION OF THE ALGOL PROCEDURE IMTQL2,
C     NUM. MATH. 12, 377-383(1968) BY MARTIN AND WILKINSON,
C     AS MODIFIED IN NUM. MATH. 15, 450(1970) BY DUBRULLE.
C     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 241-248(1971).
C
C     THIS SUBROUTINE FINDS THE EIGENVALUES AND EIGENVECTORS
C     OF A SYMMETRIC TRIDIAGONAL MATRIX BY THE IMPLICIT QL METHOD.
C     THE EIGENVECTORS OF A FULL SYMMETRIC MATRIX CAN ALSO
C     BE FOUND IF  TRED2  HAS BEEN USED TO REDUCE THIS
C     FULL MATRIX TO TRIDIAGONAL FORM.
C
C     ON INPUT
C
C        NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL
C          ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM
C          DIMENSION STATEMENT.
C
C        N IS THE ORDER OF THE MATRIX.
C
C        D CONTAINS THE DIAGONAL ELEMENTS OF THE INPUT MATRIX.
C
C        E CONTAINS THE SUBDIAGONAL ELEMENTS OF THE INPUT MATRIX
C          IN ITS LAST N-1 POSITIONS.  E(1) IS ARBITRARY.
C
C        Z CONTAINS THE TRANSFORMATION MATRIX PRODUCED IN THE
C          REDUCTION BY  TRED2, IF PERFORMED.  IF THE EIGENVECTORS
C          OF THE TRIDIAGONAL MATRIX ARE DESIRED, Z MUST CONTAIN
C          THE IDENTITY MATRIX.
C
C      ON OUTPUT
C
C        D CONTAINS THE EIGENVALUES IN ASCENDING ORDER.  IF AN
C          ERROR EXIT IS MADE, THE EIGENVALUES ARE CORRECT BUT
C          UNORDERED FOR INDICES 1,2,...,IERR-1.
C
C        E HAS BEEN DESTROYED.
C
C        Z CONTAINS ORTHONORMAL EIGENVECTORS OF THE SYMMETRIC
C          TRIDIAGONAL (OR FULL) MATRIX.  IF AN ERROR EXIT IS MADE,
C          Z CONTAINS THE EIGENVECTORS ASSOCIATED WITH THE STORED
C          EIGENVALUES.
C
C        IERR IS SET TO
C          ZERO       FOR NORMAL RETURN,
C          J          IF THE J-TH EIGENVALUE HAS NOT BEEN
C                     DETERMINED AFTER 40 ITERATIONS.
C
C     CALLS PYTHAG FOR  SQRT(A*A + B*B) .
C
C     QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO BURTON S. GARBOW,
C     MATHEMATICS AND COMPUTER SCIENCE DIV, ARGONNE NATIONAL LABORATORY
C
C     THIS VERSION DATED AUGUST 1983.
C
C     ------------------------------------------------------------------
C
      IERR = 0
      IF (N .EQ. 1) GO TO 1001
*
*        INITIALIZE ITERATION COUNT AND OPST
            ITCNT = 0
            OPST = 0
*
*     DETERMINE UNIT ROUNDOFF FOR THIS MACHINE.
      EPS = DLAMCH( 'EPSILON' )
C
      DO 100 I = 2, N
  100 E(I-1) = E(I)
C
      E(N) = 0.0D0
C
      DO 240 L = 1, N
         J = 0
C     .......... LOOK FOR SMALL SUB-DIAGONAL ELEMENT ..........
  105    DO 110 M = L, N
            IF (M .EQ. N) GO TO 120
*            TST1 = ABS(D(M)) + ABS(D(M+1))
*            TST2 = TST1 + ABS(E(M))
*            IF (TST2 .EQ. TST1) GO TO 120
            TST = ABS( E(M) )
            IF( TST .LE. EPS * ( ABS(D(M)) + ABS(D(M+1)) ) ) GO TO 120
  110    CONTINUE
C
  120    P = D(L)
*
*        INCREMENT OPCOUNT FOR FINDING SMALL SUBDIAGONAL ELEMENT.
            OPS = OPS + 2*( MIN(M,N)-L+1 )
         IF (M .EQ. L) GO TO 240
         IF (J .EQ. 40) GO TO 1000
         J = J + 1
C     .......... FORM SHIFT ..........
         G = (D(L+1) - P) / (2.0D0 * E(L))
         R = PYTHAG(G,1.0D0)
         G = D(M) - P + E(L) / (G + DSIGN(R,G))
*
*        INCREMENT OPCOUNT FOR FORMING SHIFT.
            OPS = OPS + 7
         S = 1.0D0
         C = 1.0D0
         P = 0.0D0
         MML = M - L
C     .......... FOR I=M-1 STEP -1 UNTIL L DO -- ..........
         DO 200 II = 1, MML
            I = M - II
            F = S * E(I)
            B = C * E(I)
            R = PYTHAG(F,G)
            E(I+1) = R
            IF (R .EQ. 0.0D0) GO TO 210
            S = F / R
            C = G / R
            G = D(I+1) - P
            R = (D(I) - G) * S + 2.0D0 * C * B
            P = S * R
            D(I+1) = G + P
            G = C * R - B
C     .......... FORM VECTOR ..........
            DO 180 K = 1, N
               F = Z(K,I+1)
               Z(K,I+1) = S * Z(K,I) + C * F
               Z(K,I) = C * Z(K,I) - S * F
  180       CONTINUE
C
  200    CONTINUE
C
         D(L) = D(L) - P
         E(L) = G
         E(M) = 0.0D0
*
*        INCREMENT OPCOUNT FOR INNER LOOP.
            OPS = OPS + MML*( 14+6*N ) + 1
*
*        INCREMENT ITERATION COUNTER
            ITCNT = ITCNT + 1
         GO TO 105
C     .......... RECOVER FROM UNDERFLOW ..........
  210    D(I+1) = D(I+1) - P
         E(M) = 0.0D0
*
*        INCREMENT OPCOUNT FOR INNER LOOP, WHEN UNDERFLOW OCCURS.
            OPS = OPS + 2+(II-1)*(14+6*N) + 1
         GO TO 105
  240 CONTINUE
C     .......... ORDER EIGENVALUES AND EIGENVECTORS ..........
      DO 300 II = 2, N
         I = II - 1
         K = I
         P = D(I)
C
         DO 260 J = II, N
            IF (D(J) .GE. P) GO TO 260
            K = J
            P = D(J)
  260    CONTINUE
C
         IF (K .EQ. I) GO TO 300
         D(K) = D(I)
         D(I) = P
C
         DO 280 J = 1, N
            P = Z(J,I)
            Z(J,I) = Z(J,K)
            Z(J,K) = P
  280    CONTINUE
C
  300 CONTINUE
C
      GO TO 1001
C     .......... SET ERROR -- NO CONVERGENCE TO AN
C                EIGENVALUE AFTER 40 ITERATIONS ..........
 1000 IERR = L
 1001 CONTINUE
*
*     COMPUTE FINAL OP COUNT
      OPS = OPS + OPST
      RETURN
      END
      SUBROUTINE INVIT(NM,N,A,WR,WI,SELECT,MM,M,Z,IERR,RM1,RV1,RV2)
C
      INTEGER I,J,K,L,M,N,S,II,IP,MM,MP,NM,NS,N1,UK,IP1,ITS,KM1,IERR
      DOUBLE PRECISION A(NM,N),WR(N),WI(N),Z(NM,MM),RM1(N,N),
     X       RV1(N),RV2(N)
      DOUBLE PRECISION T,W,X,Y,EPS3,NORM,NORMV,GROWTO,ILAMBD,
     X       PYTHAG,RLAMBD,UKROOT
      LOGICAL SELECT(N)
      external pythag, dlamch
*
*     COMMON BLOCK TO RETURN OPERATION COUNT AND ITERATION COUNT
*     ITCNT IS INITIALIZED TO 0, OPS IS ONLY INCREMENTED
*     OPST IS USED TO ACCUMULATE SMALL CONTRIBUTIONS TO OPS
*     TO AVOID ROUNDOFF ERROR
*     .. COMMON BLOCKS ..
      COMMON /LATIME/ OPS, ITCNT
*     ..
*     .. SCALARS IN COMMON ..
      DOUBLE PRECISION OPS, ITCNT, OPST
*     ..
C
C     THIS SUBROUTINE IS A TRANSLATION OF THE ALGOL PROCEDURE INVIT
C     BY PETERS AND WILKINSON.
C     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 418-439(1971).
C
C     THIS SUBROUTINE FINDS THOSE EIGENVECTORS OF A REAL UPPER
C     HESSENBERG MATRIX CORRESPONDING TO SPECIFIED EIGENVALUES,
C     USING INVERSE ITERATION.
C
C     ON INPUT
C
C        NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL
C          ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM
C          DIMENSION STATEMENT.
C
C        N IS THE ORDER OF THE MATRIX.
C
C        A CONTAINS THE HESSENBERG MATRIX.
C
C        WR AND WI CONTAIN THE REAL AND IMAGINARY PARTS, RESPECTIVELY,
C          OF THE EIGENVALUES OF THE MATRIX.  THE EIGENVALUES MUST BE
C          STORED IN A MANNER IDENTICAL TO THAT OF SUBROUTINE  HQR,
C          WHICH RECOGNIZES POSSIBLE SPLITTING OF THE MATRIX.
C
C        SELECT SPECIFIES THE EIGENVECTORS TO BE FOUND. THE
C          EIGENVECTOR CORRESPONDING TO THE J-TH EIGENVALUE IS
C          SPECIFIED BY SETTING SELECT(J) TO .TRUE..
C
C        MM SHOULD BE SET TO AN UPPER BOUND FOR THE NUMBER OF
C          COLUMNS REQUIRED TO STORE THE EIGENVECTORS TO BE FOUND.
C          NOTE THAT TWO COLUMNS ARE REQUIRED TO STORE THE
C          EIGENVECTOR CORRESPONDING TO A COMPLEX EIGENVALUE.
C
C     ON OUTPUT
C
C        A AND WI ARE UNALTERED.
C
C        WR MAY HAVE BEEN ALTERED SINCE CLOSE EIGENVALUES ARE PERTURBED
C          SLIGHTLY IN SEARCHING FOR INDEPENDENT EIGENVECTORS.
C
C        SELECT MAY HAVE BEEN ALTERED.  IF THE ELEMENTS CORRESPONDING
C          TO A PAIR OF CONJUGATE COMPLEX EIGENVALUES WERE EACH
C          INITIALLY SET TO .TRUE., THE PROGRAM RESETS THE SECOND OF
C          THE TWO ELEMENTS TO .FALSE..
C
C        M IS THE NUMBER OF COLUMNS ACTUALLY USED TO STORE
C          THE EIGENVECTORS.
C
C        Z CONTAINS THE REAL AND IMAGINARY PARTS OF THE EIGENVECTORS.
C          IF THE NEXT SELECTED EIGENVALUE IS REAL, THE NEXT COLUMN
C          OF Z CONTAINS ITS EIGENVECTOR.  IF THE EIGENVALUE IS
C          COMPLEX, THE NEXT TWO COLUMNS OF Z CONTAIN THE REAL AND
C          IMAGINARY PARTS OF ITS EIGENVECTOR.  THE EIGENVECTORS ARE
C          NORMALIZED SO THAT THE COMPONENT OF LARGEST MAGNITUDE IS 1.
C          ANY VECTOR WHICH FAILS THE ACCEPTANCE TEST IS SET TO ZERO.
C
C        IERR IS SET TO
C          ZERO       FOR NORMAL RETURN,
C          -(2*N+1)   IF MORE THAN MM COLUMNS OF Z ARE NECESSARY
C                     TO STORE THE EIGENVECTORS CORRESPONDING TO
C                     THE SPECIFIED EIGENVALUES.
C          -K         IF THE ITERATION CORRESPONDING TO THE K-TH
C                     VALUE FAILS,
C          -(N+K)     IF BOTH ERROR SITUATIONS OCCUR.
C
C        RM1, RV1, AND RV2 ARE TEMPORARY STORAGE ARRAYS.  NOTE THAT RM1
C          IS SQUARE OF DIMENSION N BY N AND, AUGMENTED BY TWO COLUMNS
C          OF Z, IS THE TRANSPOSE OF THE CORRESPONDING ALGOL B ARRAY.
C
C     THE ALGOL PROCEDURE GUESSVEC APPEARS IN INVIT IN LINE.
C
C     CALLS CDIV FOR COMPLEX DIVISION.
C     CALLS PYTHAG FOR  SQRT(A*A + B*B) .
C
C     QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO BURTON S. GARBOW,
C     MATHEMATICS AND COMPUTER SCIENCE DIV, ARGONNE NATIONAL LABORATORY
C
C     THIS VERSION DATED AUGUST 1983.
C
C     ------------------------------------------------------------------
*
*     GET ULP FROM DLAMCH FOR NEW SMALL PERTURBATION AS IN LAPACK
      EXTERNAL DLAMCH
      DOUBLE PRECISION DLAMCH, ULP
      IF (N.LE.0) RETURN
      ULP = DLAMCH( 'EPSILON' )
C
*
*     INITIALIZE
      OPST = 0
      IERR = 0
      UK = 0
      S = 1
C     .......... IP = 0, REAL EIGENVALUE
C                     1, FIRST OF CONJUGATE COMPLEX PAIR
C                    -1, SECOND OF CONJUGATE COMPLEX PAIR ..........
      IP = 0
      N1 = N - 1
C
      DO 980 K = 1, N
         IF (WI(K) .EQ. 0.0D0 .OR. IP .LT. 0) GO TO 100
         IP = 1
         IF (SELECT(K) .AND. SELECT(K+1)) SELECT(K+1) = .FALSE.
  100    IF (.NOT. SELECT(K)) GO TO 960
         IF (WI(K) .NE. 0.0D0) S = S + 1
         IF (S .GT. MM) GO TO 1000
         IF (UK .GE. K) GO TO 200
C     .......... CHECK FOR POSSIBLE SPLITTING ..........
         DO 120 UK = K, N
            IF (UK .EQ. N) GO TO 140
            IF (A(UK+1,UK) .EQ. 0.0D0) GO TO 140
  120    CONTINUE
C     .......... COMPUTE INFINITY NORM OF LEADING UK BY UK
C                (HESSENBERG) MATRIX ..........
  140    NORM = 0.0D0
         MP = 1
C
*
*        INCREMENT OPCOUNT FOR COMPUTING MATRIX NORM
         OPS = OPS + UK*(UK-1)/2
         DO 180 I = 1, UK
            X = 0.0D0
C
            DO 160 J = MP, UK
  160       X = X + DABS(A(I,J))
C
            IF (X .GT. NORM) NORM = X
            MP = I
  180    CONTINUE
C     .......... EPS3 REPLACES ZERO PIVOT IN DECOMPOSITION
C                AND CLOSE ROOTS ARE MODIFIED BY EPS3 ..........
         IF (NORM .EQ. 0.0D0) NORM = 1.0D0
*        EPS3 = EPSLON(NORM)
*
*        INCREMENT OPCOUNT
         OPST = OPST + 3
         EPS3 = NORM*ULP
C     .......... GROWTO IS THE CRITERION FOR THE GROWTH ..........
         UKROOT = UK
         UKROOT = DSQRT(UKROOT)
         GROWTO = 0.1D0 / UKROOT
  200    RLAMBD = WR(K)
         ILAMBD = WI(K)
         IF (K .EQ. 1) GO TO 280
         KM1 = K - 1
         GO TO 240
C     .......... PERTURB EIGENVALUE IF IT IS CLOSE
C                TO ANY PREVIOUS EIGENVALUE ..........
  220    RLAMBD = RLAMBD + EPS3
C     .......... FOR I=K-1 STEP -1 UNTIL 1 DO -- ..........
  240    DO 260 II = 1, KM1
            I = K - II
            IF (SELECT(I) .AND. DABS(WR(I)-RLAMBD) .LT. EPS3 .AND.
     X         DABS(WI(I)-ILAMBD) .LT. EPS3) GO TO 220
  260    CONTINUE
*
*        INCREMENT OPCOUNT FOR LOOP 260 (ASSUME THAT ALL EIGENVALUES
*        ARE DIFFERENT)
         OPST = OPST + 2*(K-1)
C
         WR(K) = RLAMBD
C     .......... PERTURB CONJUGATE EIGENVALUE TO MATCH ..........
         IP1 = K + IP
         WR(IP1) = RLAMBD
C     .......... FORM UPPER HESSENBERG A-RLAMBD*I (TRANSPOSED)
C                AND INITIAL REAL VECTOR ..........
  280    MP = 1
C
*
*        INCREMENT OP COUNT FOR LOOP 320
         OPS = OPS + UK
         DO 320 I = 1, UK
C
            DO 300 J = MP, UK
  300       RM1(J,I) = A(I,J)
C
            RM1(I,I) = RM1(I,I) - RLAMBD
            MP = I
            RV1(I) = EPS3
  320    CONTINUE
C
         ITS = 0
         IF (ILAMBD .NE. 0.0D0) GO TO 520
C     .......... REAL EIGENVALUE.
C                TRIANGULAR DECOMPOSITION WITH INTERCHANGES,
C                REPLACING ZERO PIVOTS BY EPS3 ..........
         IF (UK .EQ. 1) GO TO 420
C
*
*        INCREMENT OPCOUNT LU DECOMPOSITION
         OPS = OPS + (UK-1)*(UK+2)
         DO 400 I = 2, UK
            MP = I - 1
            IF (DABS(RM1(MP,I)) .LE. DABS(RM1(MP,MP))) GO TO 360
C
            DO 340 J = MP, UK
               Y = RM1(J,I)
               RM1(J,I) = RM1(J,MP)
               RM1(J,MP) = Y
  340       CONTINUE
C
  360       IF (RM1(MP,MP) .EQ. 0.0D0) RM1(MP,MP) = EPS3
            X = RM1(MP,I) / RM1(MP,MP)
            IF (X .EQ. 0.0D0) GO TO 400
C
            DO 380 J = I, UK
  380       RM1(J,I) = RM1(J,I) - X * RM1(J,MP)
C
  400    CONTINUE
C
  420    IF (RM1(UK,UK) .EQ. 0.0D0) RM1(UK,UK) = EPS3
C     .......... BACK SUBSTITUTION FOR REAL VECTOR
C                FOR I=UK STEP -1 UNTIL 1 DO -- ..........
  440    DO 500 II = 1, UK
            I = UK + 1 - II
            Y = RV1(I)
            IF (I .EQ. UK) GO TO 480
            IP1 = I + 1
C
            DO 460 J = IP1, UK
  460       Y = Y - RM1(J,I) * RV1(J)
C
  480       RV1(I) = Y / RM1(I,I)
  500    CONTINUE
*
*        INCREMENT OP COUNT FOR BACK SUBSTITUTION LOOP 500
         OPS = OPS + UK*(UK+1)
C
         GO TO 740
C     .......... COMPLEX EIGENVALUE.
C                TRIANGULAR DECOMPOSITION WITH INTERCHANGES,
C                REPLACING ZERO PIVOTS BY EPS3.  STORE IMAGINARY
C                PARTS IN UPPER TRIANGLE STARTING AT (1,3) ..........
  520    NS = N - S
         Z(1,S-1) = -ILAMBD
         Z(1,S) = 0.0D0
         IF (N .EQ. 2) GO TO 550
         RM1(1,3) = -ILAMBD
         Z(1,S-1) = 0.0D0
         IF (N .EQ. 3) GO TO 550
C
         DO 540 I = 4, N
  540    RM1(1,I) = 0.0D0
C
  550    DO 640 I = 2, UK
            MP = I - 1
            W = RM1(MP,I)
            IF (I .LT. N) T = RM1(MP,I+1)
            IF (I .EQ. N) T = Z(MP,S-1)
            X = RM1(MP,MP) * RM1(MP,MP) + T * T
            IF (W * W .LE. X) GO TO 580
            X = RM1(MP,MP) / W
            Y = T / W
            RM1(MP,MP) = W
            IF (I .LT. N) RM1(MP,I+1) = 0.0D0
            IF (I .EQ. N) Z(MP,S-1) = 0.0D0
C
*
*        INCREMENT OPCOUNT FOR LOOP 560
         OPS = OPS + 4*(UK-I+1)
            DO 560 J = I, UK
               W = RM1(J,I)
               RM1(J,I) = RM1(J,MP) - X * W
               RM1(J,MP) = W
               IF (J .LT. N1) GO TO 555
               L = J - NS
               Z(I,L) = Z(MP,L) - Y * W
               Z(MP,L) = 0.0D0
               GO TO 560
  555          RM1(I,J+2) = RM1(MP,J+2) - Y * W
               RM1(MP,J+2) = 0.0D0
  560       CONTINUE
C
            RM1(I,I) = RM1(I,I) - Y * ILAMBD
            IF (I .LT. N1) GO TO 570
            L = I - NS
            Z(MP,L) = -ILAMBD
            Z(I,L) = Z(I,L) + X * ILAMBD
            GO TO 640
  570       RM1(MP,I+2) = -ILAMBD
            RM1(I,I+2) = RM1(I,I+2) + X * ILAMBD
            GO TO 640
  580       IF (X .NE. 0.0D0) GO TO 600
            RM1(MP,MP) = EPS3
            IF (I .LT. N) RM1(MP,I+1) = 0.0D0
            IF (I .EQ. N) Z(MP,S-1) = 0.0D0
            T = 0.0D0
            X = EPS3 * EPS3
  600       W = W / X
            X = RM1(MP,MP) * W
            Y = -T * W
C
*
*        INCREMENT OPCOUNT FOR LOOP 620
         OPS = OPS + 6*(UK-I+1)
            DO 620 J = I, UK
               IF (J .LT. N1) GO TO 610
               L = J - NS
               T = Z(MP,L)
               Z(I,L) = -X * T - Y * RM1(J,MP)
               GO TO 615
  610          T = RM1(MP,J+2)
               RM1(I,J+2) = -X * T - Y * RM1(J,MP)
  615          RM1(J,I) = RM1(J,I) - X * RM1(J,MP) + Y * T
  620       CONTINUE
C
            IF (I .LT. N1) GO TO 630
            L = I - NS
            Z(I,L) = Z(I,L) - ILAMBD
            GO TO 640
  630       RM1(I,I+2) = RM1(I,I+2) - ILAMBD
  640    CONTINUE
*
*        INCREMENT OP COUNT (AVERAGE) FOR COMPUTING
*        THE SCALARS IN LOOP 640
         OPS = OPS + 10*(UK -1)
C
         IF (UK .LT. N1) GO TO 650
         L = UK - NS
         T = Z(UK,L)
         GO TO 655
  650    T = RM1(UK,UK+2)
  655    IF (RM1(UK,UK) .EQ. 0.0D0 .AND. T .EQ. 0.0D0) RM1(UK,UK) = EPS3
C     .......... BACK SUBSTITUTION FOR COMPLEX VECTOR
C                FOR I=UK STEP -1 UNTIL 1 DO -- ..........
  660    DO 720 II = 1, UK
            I = UK + 1 - II
            X = RV1(I)
            Y = 0.0D0
            IF (I .EQ. UK) GO TO 700
            IP1 = I + 1
C
            DO 680 J = IP1, UK
               IF (J .LT. N1) GO TO 670
               L = J - NS
               T = Z(I,L)
               GO TO 675
  670          T = RM1(I,J+2)
  675          X = X - RM1(J,I) * RV1(J) + T * RV2(J)
               Y = Y - RM1(J,I) * RV2(J) - T * RV1(J)
  680       CONTINUE
C
  700       IF (I .LT. N1) GO TO 710
            L = I - NS
            T = Z(I,L)
            GO TO 715
  710       T = RM1(I,I+2)
  715       CALL CDIV(X,Y,RM1(I,I),T,RV1(I),RV2(I))
  720    CONTINUE
*
*        INCREMENT OP COUNT FOR LOOP 720.
         OPS = OPS + 4*UK*(UK+3)
C     .......... ACCEPTANCE TEST FOR REAL OR COMPLEX
C                EIGENVECTOR AND NORMALIZATION ..........
  740    ITS = ITS + 1
         NORM = 0.0D0
         NORMV = 0.0D0
C
         DO 780 I = 1, UK
            IF (ILAMBD .EQ. 0.0D0) X = DABS(RV1(I))
            IF (ILAMBD .NE. 0.0D0) X = PYTHAG(RV1(I),RV2(I))
            IF (NORMV .GE. X) GO TO 760
            NORMV = X
            J = I
  760       NORM = NORM + X
  780    CONTINUE
*
*        INCREMENT OP COUNT ACCEPTANCE TEST
         IF (ILAMBD .EQ. 0.0D0) OPS = OPS + UK
         IF (ILAMBD .NE. 0.0D0) OPS = OPS + 16*UK
C
         IF (NORM .LT. GROWTO) GO TO 840
C     .......... ACCEPT VECTOR ..........
         X = RV1(J)
         IF (ILAMBD .EQ. 0.0D0) X = 1.0D0 / X
         IF (ILAMBD .NE. 0.0D0) Y = RV2(J)
C
*
*        INCREMENT OPCOUNT FOR LOOP 820
         IF (ILAMBD .EQ. 0.0D0) OPS = OPS + UK
         IF (ILAMBD .NE. 0.0D0) OPS = OPS + 16*UK
         DO 820 I = 1, UK
            IF (ILAMBD .NE. 0.0D0) GO TO 800
            Z(I,S) = RV1(I) * X
            GO TO 820
  800       CALL CDIV(RV1(I),RV2(I),X,Y,Z(I,S-1),Z(I,S))
  820    CONTINUE
C
         IF (UK .EQ. N) GO TO 940
         J = UK + 1
         GO TO 900
C     .......... IN-LINE PROCEDURE FOR CHOOSING
C                A NEW STARTING VECTOR ..........
  840    IF (ITS .GE. UK) GO TO 880
         X = UKROOT
         Y = EPS3 / (X + 1.0D0)
         RV1(1) = EPS3
C
         DO 860 I = 2, UK
  860    RV1(I) = Y
C
         J = UK - ITS + 1
         RV1(J) = RV1(J) - EPS3 * X
         IF (ILAMBD .EQ. 0.0D0) GO TO 440
         GO TO 660
C     .......... SET ERROR -- UNACCEPTED EIGENVECTOR ..........
  880    J = 1
         IERR = -K
C     .......... SET REMAINING VECTOR COMPONENTS TO ZERO ..........
  900    DO 920 I = J, N
            Z(I,S) = 0.0D0
            IF (ILAMBD .NE. 0.0D0) Z(I,S-1) = 0.0D0
  920    CONTINUE
C
  940    S = S + 1
  960    IF (IP .EQ. (-1)) IP = 0
         IF (IP .EQ. 1) IP = -1
  980 CONTINUE
C
      GO TO 1001
C     .......... SET ERROR -- UNDERESTIMATE OF EIGENVECTOR
C                SPACE REQUIRED ..........
 1000 IF (IERR .NE. 0) IERR = IERR - N
      IF (IERR .EQ. 0) IERR = -(2 * N + 1)
 1001 M = S - 1 - IABS(IP)
*
*     COMPUTE FINAL OP COUNT
      OPS = OPS + OPST
      RETURN
      END
      SUBROUTINE ORTHES(NM,N,LOW,IGH,A,ORT)
C
      INTEGER I,J,M,N,II,JJ,LA,MP,NM,IGH,KP1,LOW
      DOUBLE PRECISION A(NM,N),ORT(IGH)
      DOUBLE PRECISION F,G,H,SCALE
*
*     COMMON BLOCK TO RETURN OPERATION COUNT AND ITERATION COUNT
*     ITCNT IS INITIALIZED TO 0, OPS IS ONLY INCREMENTED
*     OPST IS USED TO ACCUMULATE SMALL CONTRIBUTIONS TO OPS
*     TO AVOID ROUNDOFF ERROR
*     .. COMMON BLOCKS ..
      COMMON /LATIME/ OPS, ITCNT
*     ..
*     .. SCALARS IN COMMON ..
      DOUBLE PRECISION OPS, ITCNT, OPST
*     ..
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
      IF (N.LE.0) RETURN
      LA = IGH - 1
      KP1 = LOW + 1
      IF (LA .LT. KP1) GO TO 200
C
*
*     INCREMENT OP COUNR FOR COMPUTING G,H,ORT(M),.. IN LOOP 180
      OPS = OPS + 6*(LA - KP1 + 1)
      DO 180 M = KP1, LA
         H = 0.0D0
         ORT(M) = 0.0D0
         SCALE = 0.0D0
C     .......... SCALE COLUMN (ALGOL TOL THEN NOT NEEDED) ..........
*
*     INCREMENT OP COUNT FOR LOOP 90
      OPS = OPS + (IGH-M +1)
         DO 90 I = M, IGH
   90    SCALE = SCALE + DABS(A(I,M-1))
C
         IF (SCALE .EQ. 0.0D0) GO TO 180
         MP = M + IGH
C     .......... FOR I=IGH STEP -1 UNTIL M DO -- ..........
*
*     INCREMENT OP COUNT FOR LOOP 100
      OPS = OPS + 3*(IGH-M+1)
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
*
*     INCREMENT OP COUNT FOR LOOP 130 AND 160
      OPS = OPS + (N-M+1+IGH)*(4*(IGH-M+1) + 1)
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
      DOUBLE PRECISION FUNCTION PYTHAG(A,B)
      DOUBLE PRECISION A,B
C
C     FINDS SQRT(A**2+B**2) WITHOUT OVERFLOW OR DESTRUCTIVE UNDERFLOW
C
*
*     COMMON BLOCK TO RETURN OPERATION COUNT
*     OPST IS ONLY INCREMENTED HERE
*     .. COMMON BLOCKS ..
      COMMON             / PYTHOP / OPST
*     ..
*     .. SCALARS IN COMMON
      DOUBLE PRECISION   OPST
*     ..
      DOUBLE PRECISION P,R,S,T,U
      P = DMAX1(DABS(A),DABS(B))
      IF (P .EQ. 0.0D0) GO TO 20
      R = (DMIN1(DABS(A),DABS(B))/P)**2
*
*     INCREMENT OPST
      OPST = OPST + 2
   10 CONTINUE
         T = 4.0D0 + R
         IF (T .EQ. 4.0D0) GO TO 20
         S = R/T
         U = 1.0D0 + 2.0D0*S
         P = U*P
         R = (S/U)**2 * R
*
*        INCREMENT OPST
            OPST = OPST + 8
      GO TO 10
   20 PYTHAG = P
      RETURN
      END
      SUBROUTINE TQLRAT(N,D,E2,IERR)
*
*     EISPACK ROUTINE.
*     MODIFIED FOR COMPARISON WITH LAPACK ROUTINES.
*
*     CONVERGENCE TEST WAS MODIFIED TO BE THE SAME AS IN DSTEQR.
*
C
      INTEGER I,J,L,M,N,II,L1,MML,IERR
      DOUBLE PRECISION D(N),E2(N)
      DOUBLE PRECISION B,C,F,G,H,P,R,S,T,EPSLON,PYTHAG
      DOUBLE PRECISION EPS, TST
      DOUBLE PRECISION DLAMCH
      external pythag, dlamch, epslon
*
*     COMMON BLOCK TO RETURN OPERATION COUNT AND ITERATION COUNT
*     ITCNT IS INITIALIZED TO 0, OPS IS ONLY INCREMENTED
*     OPST IS USED TO ACCUMULATE CONTRIBUTIONS TO OPS FROM
*     FUNCTION PYTHAG.  IT IS PASSED TO AND FROM PYTHAG
*     THROUGH COMMON BLOCK PYTHOP.
*     .. COMMON BLOCKS ..
      COMMON             / LATIME / OPS, ITCNT
      COMMON             / PYTHOP / OPST
*     ..
*     .. SCALARS IN COMMON ..
      DOUBLE PRECISION   ITCNT, OPS, OPST
*     ..
C
C     THIS SUBROUTINE IS A TRANSLATION OF THE ALGOL PROCEDURE TQLRAT,
C     ALGORITHM 464, COMM. ACM 16, 689(1973) BY REINSCH.
C
C     THIS SUBROUTINE FINDS THE EIGENVALUES OF A SYMMETRIC
C     TRIDIAGONAL MATRIX BY THE RATIONAL QL METHOD.
C
C     ON INPUT
C
C        N IS THE ORDER OF THE MATRIX.
C
C        D CONTAINS THE DIAGONAL ELEMENTS OF THE INPUT MATRIX.
C
C        E2 CONTAINS THE SQUARES OF THE SUBDIAGONAL ELEMENTS OF THE
C          INPUT MATRIX IN ITS LAST N-1 POSITIONS.  E2(1) IS ARBITRARY.
C
C      ON OUTPUT
C
C        D CONTAINS THE EIGENVALUES IN ASCENDING ORDER.  IF AN
C          ERROR EXIT IS MADE, THE EIGENVALUES ARE CORRECT AND
C          ORDERED FOR INDICES 1,2,...IERR-1, BUT MAY NOT BE
C          THE SMALLEST EIGENVALUES.
C
C        E2 HAS BEEN DESTROYED.
C
C        IERR IS SET TO
C          ZERO       FOR NORMAL RETURN,
C          J          IF THE J-TH EIGENVALUE HAS NOT BEEN
C                     DETERMINED AFTER 30 ITERATIONS.
C
C     CALLS PYTHAG FOR  SQRT(A*A + B*B) .
C
C     QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO BURTON S. GARBOW,
C     MATHEMATICS AND COMPUTER SCIENCE DIV, ARGONNE NATIONAL LABORATORY
C
C     THIS VERSION DATED AUGUST 1983.
C
C     ------------------------------------------------------------------
C
      IERR = 0
      IF (N .EQ. 1) GO TO 1001
*
*        INITIALIZE ITERATION COUNT AND OPST
            ITCNT = 0
            OPST = 0
*
*     DETERMINE THE UNIT ROUNDOFF FOR THIS ENVIRONMENT.
*
      EPS = DLAMCH( 'EPSILON' )
C
      DO 100 I = 2, N
  100 E2(I-1) = E2(I)
C
      F = 0.0D0
      T = 0.0D0
      E2(N) = 0.0D0
C
      DO 290 L = 1, N
         J = 0
         H = DABS(D(L)) + DSQRT(E2(L))
         IF (T .GT. H) GO TO 105
         T = H
         B = EPSLON(T)
         C = B * B
*
*     INCREMENT OPCOUNT FOR THIS SECTION.
*     (FUNCTION EPSLON IS COUNTED AS 6 FLOPS.  THIS IS THE MINIMUM
*     NUMBER REQUIRED, BUT COUNTING THEM EXACTLY WOULD AFFECT
*     THE TIMING.)
         OPS = OPS + 9
C     .......... LOOK FOR SMALL SQUARED SUB-DIAGONAL ELEMENT ..........
  105    DO 110 M = L, N
            IF( M .EQ. N ) GO TO 120
            TST = SQRT( ABS( E2(M) ) )
            IF( TST .LE. EPS * ( ABS(D(M)) + ABS(D(M+1)) ) ) GO TO 120
*            IF (E2(M) .LE. C) GO TO 120
C     .......... E2(N) IS ALWAYS ZERO, SO THERE IS NO EXIT
C                THROUGH THE BOTTOM OF THE LOOP ..........
  110    CONTINUE
C
  120    CONTINUE
*
*        INCREMENT OPCOUNT FOR FINDING SMALL SUBDIAGONAL ELEMENT.
            OPS = OPS + 3*( MIN(M,N-1)-L+1 )
         IF (M .EQ. L) GO TO 210
  130    IF (J .EQ. 30) GO TO 1000
         J = J + 1
C     .......... FORM SHIFT ..........
         L1 = L + 1
         S = DSQRT(E2(L))
         G = D(L)
         P = (D(L1) - G) / (2.0D0 * S)
         R = PYTHAG(P,1.0D0)
         D(L) = S / (P + DSIGN(R,P))
         H = G - D(L)
C
         DO 140 I = L1, N
  140    D(I) = D(I) - H
C
         F = F + H
*
*        INCREMENT OPCOUNT FOR FORMING SHIFT AND SUBTRACTING.
            OPS = OPS + 8 + (I-L1+1)
C     .......... RATIONAL QL TRANSFORMATION ..........
         G = D(M)
         IF (G .EQ. 0.0D0) G = B
         H = G
         S = 0.0D0
         MML = M - L
C     .......... FOR I=M-1 STEP -1 UNTIL L DO -- ..........
         DO 200 II = 1, MML
            I = M - II
            P = G * H
            R = P + E2(I)
            E2(I+1) = S * R
            S = E2(I) / R
            D(I+1) = H + S * (H + D(I))
            G = D(I) - E2(I) / G
            IF (G .EQ. 0.0D0) G = B
            H = G * P / R
  200    CONTINUE
C
         E2(L) = S * G
         D(L) = H
*
*        INCREMENT OPCOUNT FOR INNER LOOP.
            OPS = OPS + MML*11 + 1
*
*        INCREMENT ITERATION COUNTER
            ITCNT = ITCNT + 1
C     .......... GUARD AGAINST UNDERFLOW IN CONVERGENCE TEST ..........
         IF (H .EQ. 0.0D0) GO TO 210
         IF (DABS(E2(L)) .LE. DABS(C/H)) GO TO 210
         E2(L) = H * E2(L)
         IF (E2(L) .NE. 0.0D0) GO TO 130
  210    P = D(L) + F
C     .......... ORDER EIGENVALUES ..........
         IF (L .EQ. 1) GO TO 250
C     .......... FOR I=L STEP -1 UNTIL 2 DO -- ..........
         DO 230 II = 2, L
            I = L + 2 - II
            IF (P .GE. D(I-1)) GO TO 270
            D(I) = D(I-1)
  230    CONTINUE
C
  250    I = 1
  270    D(I) = P
  290 CONTINUE
C
      GO TO 1001
C     .......... SET ERROR -- NO CONVERGENCE TO AN
C                EIGENVALUE AFTER 30 ITERATIONS ..........
 1000 IERR = L
 1001 CONTINUE
*
*     COMPUTE FINAL OP COUNT
      OPS = OPS + OPST
      RETURN
      END
      SUBROUTINE TRED1(NM,N,A,D,E,E2)
C
      INTEGER I,J,K,L,N,II,NM,JP1
      DOUBLE PRECISION A(NM,N),D(N),E(N),E2(N)
      DOUBLE PRECISION F,G,H,SCALE
*
*     COMMON BLOCK TO RETURN OPERATION COUNT AND ITERATION COUNT.
*     ITCNT IS INITIALIZED TO 0, OPS IS ONLY INCREMENTED.
*     .. COMMON BLOCKS ..
      COMMON             / LATIME / OPS, ITCNT
*     ..
*     .. SCALARS IN COMMON ..
      DOUBLE PRECISION   ITCNT, OPS
*     ..
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
*
      OPS = OPS + MAX( 0.0D0, (4.0D0/3.0D0)*DBLE(N)**3 +
     $                              12.0D0*DBLE(N)**2 +
     $                      (11.0D0/3.0D0)*N - 22 )
*
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
      SUBROUTINE BISECT(N,EPS1,D,E,E2,LB,UB,MM,M,W,IND,IERR,RV4,RV5)
*
*     EISPACK ROUTINE.
*     MODIFIED FOR COMPARISON WITH LAPACK ROUTINES.
*
*     CONVERGENCE TEST WAS MODIFIED TO BE THE SAME AS IN DSTEBZ.
*
C
      INTEGER I,J,K,L,M,N,P,Q,R,S,II,MM,M1,M2,TAG,IERR,ISTURM
      DOUBLE PRECISION D(N),E(N),E2(N),W(MM),RV4(N),RV5(N)
      DOUBLE PRECISION U,V,LB,T1,T2,UB,XU,X0,X1,EPS1,TST1,TST2,EPSLON
      INTEGER IND(MM)
      external epslon
*
*     COMMON BLOCK TO RETURN OPERATION COUNT AND ITERATION COUNT
*     ITCNT IS INITIALIZED TO 0, OPS IS ONLY INCREMENTED
*     .. COMMON BLOCKS ..
      COMMON             / LATIME / OPS, ITCNT
*     ..
*     .. SCALARS IN COMMON ..
      DOUBLE PRECISION   ITCNT, OPS
*     ..
C
C     THIS SUBROUTINE IS A TRANSLATION OF THE BISECTION TECHNIQUE
C     IN THE ALGOL PROCEDURE TRISTURM BY PETERS AND WILKINSON.
C     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 418-439(1971).
C
C     THIS SUBROUTINE FINDS THOSE EIGENVALUES OF A TRIDIAGONAL
C     SYMMETRIC MATRIX WHICH LIE IN A SPECIFIED INTERVAL,
C     USING BISECTION.
C
C     ON INPUT
C
C        N IS THE ORDER OF THE MATRIX.
C
C        EPS1 IS AN ABSOLUTE ERROR TOLERANCE FOR THE COMPUTED
C          EIGENVALUES.  IF THE INPUT EPS1 IS NON-POSITIVE,
C          IT IS RESET FOR EACH SUBMATRIX TO A DEFAULT VALUE,
C          NAMELY, MINUS THE PRODUCT OF THE RELATIVE MACHINE
C          PRECISION AND THE 1-NORM OF THE SUBMATRIX.
C
C        D CONTAINS THE DIAGONAL ELEMENTS OF THE INPUT MATRIX.
C
C        E CONTAINS THE SUBDIAGONAL ELEMENTS OF THE INPUT MATRIX
C          IN ITS LAST N-1 POSITIONS.  E(1) IS ARBITRARY.
C
C        E2 CONTAINS THE SQUARES OF THE CORRESPONDING ELEMENTS OF E.
C          E2(1) IS ARBITRARY.
C
C        LB AND UB DEFINE THE INTERVAL TO BE SEARCHED FOR EIGENVALUES.
C          IF LB IS NOT LESS THAN UB, NO EIGENVALUES WILL BE FOUND.
C
C        MM SHOULD BE SET TO AN UPPER BOUND FOR THE NUMBER OF
C          EIGENVALUES IN THE INTERVAL.  WARNING. IF MORE THAN
C          MM EIGENVALUES ARE DETERMINED TO LIE IN THE INTERVAL,
C          AN ERROR RETURN IS MADE WITH NO EIGENVALUES FOUND.
C
C     ON OUTPUT
C
C        EPS1 IS UNALTERED UNLESS IT HAS BEEN RESET TO ITS
C          (LAST) DEFAULT VALUE.
C
C        D AND E ARE UNALTERED.
C
C        ELEMENTS OF E2, CORRESPONDING TO ELEMENTS OF E REGARDED
C          AS NEGLIGIBLE, HAVE BEEN REPLACED BY ZERO CAUSING THE
C          MATRIX TO SPLIT INTO A DIRECT SUM OF SUBMATRICES.
C          E2(1) IS ALSO SET TO ZERO.
C
C        M IS THE NUMBER OF EIGENVALUES DETERMINED TO LIE IN (LB,UB).
C
C        W CONTAINS THE M EIGENVALUES IN ASCENDING ORDER.
C
C        IND CONTAINS IN ITS FIRST M POSITIONS THE SUBMATRIX INDICES
C          ASSOCIATED WITH THE CORRESPONDING EIGENVALUES IN W --
C          1 FOR EIGENVALUES BELONGING TO THE FIRST SUBMATRIX FROM
C          THE TOP, 2 FOR THOSE BELONGING TO THE SECOND SUBMATRIX, ETC..
C
C        IERR IS SET TO
C          ZERO       FOR NORMAL RETURN,
C          3*N+1      IF M EXCEEDS MM.
C
C        RV4 AND RV5 ARE TEMPORARY STORAGE ARRAYS.
C
C     THE ALGOL PROCEDURE STURMCNT CONTAINED IN TRISTURM
C     APPEARS IN BISECT IN-LINE.
C
C     NOTE THAT SUBROUTINE TQL1 OR IMTQL1 IS GENERALLY FASTER THAN
C     BISECT, IF MORE THAN N/4 EIGENVALUES ARE TO BE FOUND.
C
C     QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO BURTON S. GARBOW,
C     MATHEMATICS AND COMPUTER SCIENCE DIV, ARGONNE NATIONAL LABORATORY
C
C     THIS VERSION DATED AUGUST 1983.
C
C     ------------------------------------------------------------------
C
      DOUBLE PRECISION ONE
      PARAMETER        ( ONE = 1.0D0 )
      DOUBLE PRECISION RELFAC
      PARAMETER        ( RELFAC = 2.0D0 )
      DOUBLE PRECISION ATOLI, RTOLI, SAFEMN, TMP1, TMP2, TNORM, ULP
      DOUBLE PRECISION DLAMCH, PIVMIN
      EXTERNAL DLAMCH
*        INITIALIZE ITERATION COUNT.
            ITCNT = 0
      SAFEMN = DLAMCH( 'S' )
      ULP = DLAMCH( 'E' )*DLAMCH( 'B' )
      RTOLI = ULP*RELFAC
      IERR = 0
      TAG = 0
      T1 = LB
      T2 = UB
C     .......... LOOK FOR SMALL SUB-DIAGONAL ENTRIES ..........
      DO 40 I = 1, N
         IF (I .EQ. 1) GO TO 20
CCC         TST1 = DABS(D(I)) + DABS(D(I-1))
CCC         TST2 = TST1 + DABS(E(I))
CCC         IF (TST2 .GT. TST1) GO TO 40
         TMP1 = E( I )**2
         IF( ABS( D(I)*D(I-1) )*ULP**2+SAFEMN.LE.TMP1 )
     $      GO TO 40
   20    E2(I) = 0.0D0
   40 CONTINUE
*           INCREMENT OPCOUNT FOR DETERMINING IF MATRIX SPLITS.
               OPS = OPS + 5*( N-1 )
C
C                COMPUTE QUANTITIES NEEDED FOR CONVERGENCE TEST.
      TMP1 = D( 1 ) - ABS( E( 2 ) )
      TMP2 = D( 1 ) + ABS( E( 2 ) )
      PIVMIN = ONE
      DO 41 I = 2, N - 1
         TMP1 = MIN( TMP1, D( I )-ABS( E( I ) )-ABS( E( I+1 ) ) )
         TMP2 = MAX( TMP2, D( I )+ABS( E( I ) )+ABS( E( I+1 ) ) )
         PIVMIN = MAX( PIVMIN, E( I )**2 )
   41 CONTINUE
      TMP1 = MIN( TMP1, D( N )-ABS( E( N ) ) )
      TMP2 = MAX( TMP2, D( N )+ABS( E( N ) ) )
      PIVMIN = MAX( PIVMIN, E( N )**2 )
      PIVMIN = PIVMIN*SAFEMN
      TNORM = MAX( ABS(TMP1), ABS(TMP2) )
      ATOLI = ULP*TNORM
*        INCREMENT OPCOUNT FOR COMPUTING THESE QUANTITIES.
            OPS = OPS + 4*( N-1 )
C
C     .......... DETERMINE THE NUMBER OF EIGENVALUES
C                IN THE INTERVAL ..........
      P = 1
      Q = N
      X1 = UB
      ISTURM = 1
      GO TO 320
   60 M = S
      X1 = LB
      ISTURM = 2
      GO TO 320
   80 M = M - S
      IF (M .GT. MM) GO TO 980
      Q = 0
      R = 0
C     .......... ESTABLISH AND PROCESS NEXT SUBMATRIX, REFINING
C                INTERVAL BY THE GERSCHGORIN BOUNDS ..........
  100 IF (R .EQ. M) GO TO 1001
      TAG = TAG + 1
      P = Q + 1
      XU = D(P)
      X0 = D(P)
      U = 0.0D0
C
      DO 120 Q = P, N
         X1 = U
         U = 0.0D0
         V = 0.0D0
         IF (Q .EQ. N) GO TO 110
         U = DABS(E(Q+1))
         V = E2(Q+1)
  110    XU = DMIN1(D(Q)-(X1+U),XU)
         X0 = DMAX1(D(Q)+(X1+U),X0)
         IF (V .EQ. 0.0D0) GO TO 140
  120 CONTINUE
*        INCREMENT OPCOUNT FOR REFINING INTERVAL.
            OPS = OPS + ( N-P+1 )*2
C
  140 X1 = EPSLON(DMAX1(DABS(XU),DABS(X0)))
      IF (EPS1 .LE. 0.0D0) EPS1 = -X1
      IF (P .NE. Q) GO TO 180
C     .......... CHECK FOR ISOLATED ROOT WITHIN INTERVAL ..........
      IF (T1 .GT. D(P) .OR. D(P) .GE. T2) GO TO 940
      M1 = P
      M2 = P
      RV5(P) = D(P)
      GO TO 900
  180 X1 = X1 * (Q - P + 1)
      LB = DMAX1(T1,XU-X1)
      UB = DMIN1(T2,X0+X1)
      X1 = LB
      ISTURM = 3
      GO TO 320
  200 M1 = S + 1
      X1 = UB
      ISTURM = 4
      GO TO 320
  220 M2 = S
      IF (M1 .GT. M2) GO TO 940
C     .......... FIND ROOTS BY BISECTION ..........
      X0 = UB
      ISTURM = 5
C
      DO 240 I = M1, M2
         RV5(I) = UB
         RV4(I) = LB
  240 CONTINUE
C     .......... LOOP FOR K-TH EIGENVALUE
C                FOR K=M2 STEP -1 UNTIL M1 DO --
C                (-DO- NOT USED TO LEGALIZE -COMPUTED GO TO-) ..........
      K = M2
  250    XU = LB
C     .......... FOR I=K STEP -1 UNTIL M1 DO -- ..........
         DO 260 II = M1, K
            I = M1 + K - II
            IF (XU .GE. RV4(I)) GO TO 260
            XU = RV4(I)
            GO TO 280
  260    CONTINUE
C
  280    IF (X0 .GT. RV5(K)) X0 = RV5(K)
C     .......... NEXT BISECTION STEP ..........
  300    X1 = (XU + X0) * 0.5D0
CCC         IF ((X0 - XU) .LE. DABS(EPS1)) GO TO 420
CCC         TST1 = 2.0D0 * (DABS(XU) + DABS(X0))
CCC         TST2 = TST1 + (X0 - XU)
CCC         IF (TST2 .EQ. TST1) GO TO 420
         TMP1 = ABS( X0 - XU )
         TMP2 = MAX( ABS( X0 ), ABS( XU ) )
         IF( TMP1.LT.MAX( ATOLI, PIVMIN, RTOLI*TMP2 ) )
     $      GO TO 420
C     .......... IN-LINE PROCEDURE FOR STURM SEQUENCE ..........
  320    S = P - 1
         U = 1.0D0
C
         DO 340 I = P, Q
            IF (U .NE. 0.0D0) GO TO 325
            V = DABS(E(I)) / EPSLON(1.0D0)
            IF (E2(I) .EQ. 0.0D0) V = 0.0D0
            GO TO 330
  325       V = E2(I) / U
  330       U = D(I) - X1 - V
            IF (U .LT. 0.0D0) S = S + 1
  340    CONTINUE
*           INCREMENT OPCOUNT FOR STURM SEQUENCE.
               OPS = OPS + ( Q-P+1 )*3
*           INCREMENT ITERATION COUNTER.
               ITCNT = ITCNT + 1
C
         GO TO (60,80,200,220,360), ISTURM
C     .......... REFINE INTERVALS ..........
  360    IF (S .GE. K) GO TO 400
         XU = X1
         IF (S .GE. M1) GO TO 380
         RV4(M1) = X1
         GO TO 300
  380    RV4(S+1) = X1
         IF (RV5(S) .GT. X1) RV5(S) = X1
         GO TO 300
  400    X0 = X1
         GO TO 300
C     .......... K-TH EIGENVALUE FOUND ..........
  420    RV5(K) = X1
      K = K - 1
      IF (K .GE. M1) GO TO 250
C     .......... ORDER EIGENVALUES TAGGED WITH THEIR
C                SUBMATRIX ASSOCIATIONS ..........
  900 S = R
      R = R + M2 - M1 + 1
      J = 1
      K = M1
C
      DO 920 L = 1, R
         IF (J .GT. S) GO TO 910
         IF (K .GT. M2) GO TO 940
         IF (RV5(K) .GE. W(L)) GO TO 915
C
         DO 905 II = J, S
            I = L + S - II
            W(I+1) = W(I)
            IND(I+1) = IND(I)
  905    CONTINUE
C
  910    W(L) = RV5(K)
         IND(L) = TAG
         K = K + 1
         GO TO 920
  915    J = J + 1
  920 CONTINUE
C
  940 IF (Q .LT. N) GO TO 100
      GO TO 1001
C     .......... SET ERROR -- UNDERESTIMATE OF NUMBER OF
C                EIGENVALUES IN INTERVAL ..........
  980 IERR = 3 * N + 1
 1001 LB = T1
      UB = T2
      RETURN
      END
      SUBROUTINE TINVIT(NM,N,D,E,E2,M,W,IND,Z,
     X                  IERR,RV1,RV2,RV3,RV4,RV6)
*
*     EISPACK ROUTINE.
*
*     CONVERGENCE TEST WAS NOT MODIFIED, SINCE IT SHOULD GIVE
*     APPROXIMATELY THE SAME LEVEL OF ACCURACY AS LAPACK ROUTINE,
*     ALTHOUGH THE EIGENVECTORS MAY NOT BE AS CLOSE TO ORTHOGONAL.
*
C
      INTEGER I,J,M,N,P,Q,R,S,II,IP,JJ,NM,ITS,TAG,IERR,GROUP
      DOUBLE PRECISION D(N),E(N),E2(N),W(M),Z(NM,M),
     X       RV1(N),RV2(N),RV3(N),RV4(N),RV6(N)
      DOUBLE PRECISION U,V,UK,XU,X0,X1,EPS2,EPS3,EPS4,NORM,ORDER,EPSLON,
     X       PYTHAG
      INTEGER IND(M)
      external pythag, dlamch, epslon
*
*     COMMON BLOCK TO RETURN OPERATION COUNT AND ITERATION COUNT
*     ITCNT IS INITIALIZED TO 0, OPS IS ONLY INCREMENTED
*     .. COMMON BLOCKS ..
      COMMON             / LATIME / OPS, ITCNT
      COMMON             / PYTHOP / OPST
*     ..
*     .. SCALARS IN COMMON ..
      DOUBLE PRECISION   ITCNT, OPS, OPST
*     ..
C
C     THIS SUBROUTINE IS A TRANSLATION OF THE INVERSE ITERATION TECH-
C     NIQUE IN THE ALGOL PROCEDURE TRISTURM BY PETERS AND WILKINSON.
C     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 418-439(1971).
C
C     THIS SUBROUTINE FINDS THOSE EIGENVECTORS OF A TRIDIAGONAL
C     SYMMETRIC MATRIX CORRESPONDING TO SPECIFIED EIGENVALUES,
C     USING INVERSE ITERATION.
C
C     ON INPUT
C
C        NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL
C          ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM
C          DIMENSION STATEMENT.
C
C        N IS THE ORDER OF THE MATRIX.
C
C        D CONTAINS THE DIAGONAL ELEMENTS OF THE INPUT MATRIX.
C
C        E CONTAINS THE SUBDIAGONAL ELEMENTS OF THE INPUT MATRIX
C          IN ITS LAST N-1 POSITIONS.  E(1) IS ARBITRARY.
C
C        E2 CONTAINS THE SQUARES OF THE CORRESPONDING ELEMENTS OF E,
C          WITH ZEROS CORRESPONDING TO NEGLIGIBLE ELEMENTS OF E.
C          E(I) IS CONSIDERED NEGLIGIBLE IF IT IS NOT LARGER THAN
C          THE PRODUCT OF THE RELATIVE MACHINE PRECISION AND THE SUM
C          OF THE MAGNITUDES OF D(I) AND D(I-1).  E2(1) MUST CONTAIN
C          0.0D0 IF THE EIGENVALUES ARE IN ASCENDING ORDER, OR 2.0D0
C          IF THE EIGENVALUES ARE IN DESCENDING ORDER.  IF  BISECT,
C          TRIDIB, OR  IMTQLV  HAS BEEN USED TO FIND THE EIGENVALUES,
C          THEIR OUTPUT E2 ARRAY IS EXACTLY WHAT IS EXPECTED HERE.
C
C        M IS THE NUMBER OF SPECIFIED EIGENVALUES.
C
C        W CONTAINS THE M EIGENVALUES IN ASCENDING OR DESCENDING ORDER.
C
C        IND CONTAINS IN ITS FIRST M POSITIONS THE SUBMATRIX INDICES
C          ASSOCIATED WITH THE CORRESPONDING EIGENVALUES IN W --
C          1 FOR EIGENVALUES BELONGING TO THE FIRST SUBMATRIX FROM
C          THE TOP, 2 FOR THOSE BELONGING TO THE SECOND SUBMATRIX, ETC.
C
C     ON OUTPUT
C
C        ALL INPUT ARRAYS ARE UNALTERED.
C
C        Z CONTAINS THE ASSOCIATED SET OF ORTHONORMAL EIGENVECTORS.
C          ANY VECTOR WHICH FAILS TO CONVERGE IS SET TO ZERO.
C
C        IERR IS SET TO
C          ZERO       FOR NORMAL RETURN,
C          -R         IF THE EIGENVECTOR CORRESPONDING TO THE R-TH
C                     EIGENVALUE FAILS TO CONVERGE IN 5 ITERATIONS.
C
C        RV1, RV2, RV3, RV4, AND RV6 ARE TEMPORARY STORAGE ARRAYS.
C
C     CALLS PYTHAG FOR  DSQRT(A*A + B*B) .
C
C     QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO BURTON S. GARBOW,
C     MATHEMATICS AND COMPUTER SCIENCE DIV, ARGONNE NATIONAL LABORATORY
C
C     THIS VERSION DATED AUGUST 1983.
C
C     ------------------------------------------------------------------
C
*        INITIALIZE ITERATION COUNT.
            ITCNT = 0
      IERR = 0
      IF (M .EQ. 0) GO TO 1001
      TAG = 0
      ORDER = 1.0D0 - E2(1)
      Q = 0
C     .......... ESTABLISH AND PROCESS NEXT SUBMATRIX ..........
  100 P = Q + 1
C
      DO 120 Q = P, N
         IF (Q .EQ. N) GO TO 140
         IF (E2(Q+1) .EQ. 0.0D0) GO TO 140
  120 CONTINUE
C     .......... FIND VECTORS BY INVERSE ITERATION ..........
  140 TAG = TAG + 1
      S = 0
C
      DO 920 R = 1, M
         IF (IND(R) .NE. TAG) GO TO 920
         ITS = 1
         X1 = W(R)
         IF (S .NE. 0) GO TO 510
C     .......... CHECK FOR ISOLATED ROOT ..........
         XU = 1.0D0
         IF (P .NE. Q) GO TO 490
         RV6(P) = 1.0D0
         GO TO 870
  490    NORM = DABS(D(P))
         IP = P + 1
C
         DO 500 I = IP, Q
  500    NORM = DMAX1(NORM, DABS(D(I))+DABS(E(I)))
C     .......... EPS2 IS THE CRITERION FOR GROUPING,
C                EPS3 REPLACES ZERO PIVOTS AND EQUAL
C                ROOTS ARE MODIFIED BY EPS3,
C                EPS4 IS TAKEN VERY SMALL TO AVOID OVERFLOW ..........
         EPS2 = 1.0D-3 * NORM
         EPS3 = EPSLON(NORM)
         UK = Q - P + 1
         EPS4 = UK * EPS3
         UK = EPS4 / DSQRT(UK)
*           INCREMENT OPCOUNT FOR COMPUTING CRITERIA.
               OPS = OPS + ( Q-IP+4 )
         S = P
  505    GROUP = 0
         GO TO 520
C     .......... LOOK FOR CLOSE OR COINCIDENT ROOTS ..........
  510    IF (DABS(X1-X0) .GE. EPS2) GO TO 505
         GROUP = GROUP + 1
         IF (ORDER * (X1 - X0) .LE. 0.0D0) X1 = X0 + ORDER * EPS3
C     .......... ELIMINATION WITH INTERCHANGES AND
C                INITIALIZATION OF VECTOR ..........
  520    V = 0.0D0
C
         DO 580 I = P, Q
            RV6(I) = UK
            IF (I .EQ. P) GO TO 560
            IF (DABS(E(I)) .LT. DABS(U)) GO TO 540
C     .......... WARNING -- A DIVIDE CHECK MAY OCCUR HERE IF
C                E2 ARRAY HAS NOT BEEN SPECIFIED CORRECTLY ..........
            XU = U / E(I)
            RV4(I) = XU
            RV1(I-1) = E(I)
            RV2(I-1) = D(I) - X1
            RV3(I-1) = 0.0D0
            IF (I .NE. Q) RV3(I-1) = E(I+1)
            U = V - XU * RV2(I-1)
            V = -XU * RV3(I-1)
            GO TO 580
  540       XU = E(I) / U
            RV4(I) = XU
            RV1(I-1) = U
            RV2(I-1) = V
            RV3(I-1) = 0.0D0
  560       U = D(I) - X1 - XU * V
            IF (I .NE. Q) V = E(I+1)
  580    CONTINUE
*           INCREMENT OPCOUNT FOR ELIMINATION.
               OPS = OPS + ( Q-P+1 )*5
C
         IF (U .EQ. 0.0D0) U = EPS3
         RV1(Q) = U
         RV2(Q) = 0.0D0
         RV3(Q) = 0.0D0
C     .......... BACK SUBSTITUTION
C                FOR I=Q STEP -1 UNTIL P DO -- ..........
  600    DO 620 II = P, Q
            I = P + Q - II
            RV6(I) = (RV6(I) - U * RV2(I) - V * RV3(I)) / RV1(I)
            V = U
            U = RV6(I)
  620    CONTINUE
*           INCREMENT OPCOUNT FOR BACK SUBSTITUTION.
               OPS = OPS + ( Q-P+1 )*5
C     .......... ORTHOGONALIZE WITH RESPECT TO PREVIOUS
C                MEMBERS OF GROUP ..........
         IF (GROUP .EQ. 0) GO TO 700
         J = R
C
         DO 680 JJ = 1, GROUP
  630       J = J - 1
            IF (IND(J) .NE. TAG) GO TO 630
            XU = 0.0D0
C
            DO 640 I = P, Q
  640       XU = XU + RV6(I) * Z(I,J)
C
            DO 660 I = P, Q
  660       RV6(I) = RV6(I) - XU * Z(I,J)
C
*              INCREMENT OPCOUNT FOR ORTHOGONALIZING.
                  OPS = OPS + ( Q-P+1 )*4
  680    CONTINUE
C
  700    NORM = 0.0D0
C
         DO 720 I = P, Q
  720    NORM = NORM + DABS(RV6(I))
*           INCREMENT OPCOUNT FOR COMPUTING NORM.
               OPS = OPS + ( Q-P+1 )
C
         IF (NORM .GE. 1.0D0) GO TO 840
C     .......... FORWARD SUBSTITUTION ..........
         IF (ITS .EQ. 5) GO TO 830
         IF (NORM .NE. 0.0D0) GO TO 740
         RV6(S) = EPS4
         S = S + 1
         IF (S .GT. Q) S = P
         GO TO 780
  740    XU = EPS4 / NORM
C
         DO 760 I = P, Q
  760    RV6(I) = RV6(I) * XU
C     .......... ELIMINATION OPERATIONS ON NEXT VECTOR
C                ITERATE ..........
  780    DO 820 I = IP, Q
            U = RV6(I)
C     .......... IF RV1(I-1) .EQ. E(I), A ROW INTERCHANGE
C                WAS PERFORMED EARLIER IN THE
C                TRIANGULARIZATION PROCESS ..........
            IF (RV1(I-1) .NE. E(I)) GO TO 800
            U = RV6(I-1)
            RV6(I-1) = RV6(I)
  800       RV6(I) = U - RV4(I) * RV6(I-1)
  820    CONTINUE
*           INCREMENT OPCOUNT FOR FORWARD SUBSTITUTION.
               OPS = OPS + ( Q-P+1 ) + ( Q-IP+1 )*2
C
         ITS = ITS + 1
*           INCREMENT ITERATION COUNTER.
               ITCNT = ITCNT + 1
         GO TO 600
C     .......... SET ERROR -- NON-CONVERGED EIGENVECTOR ..........
  830    IERR = -R
         XU = 0.0D0
         GO TO 870
C     .......... NORMALIZE SO THAT SUM OF SQUARES IS
C                1 AND EXPAND TO FULL ORDER ..........
  840    U = 0.0D0
C
         DO 860 I = P, Q
  860    U = PYTHAG(U,RV6(I))
C
         XU = 1.0D0 / U
C
  870    DO 880 I = 1, N
  880    Z(I,R) = 0.0D0
C
         DO 900 I = P, Q
  900    Z(I,R) = RV6(I) * XU
*           INCREMENT OPCOUNT FOR NORMALIZING.
               OPS = OPS + ( Q-P+1 )
C
         X0 = X1
  920 CONTINUE
C
      IF (Q .LT. N) GO TO 100
*        INCREMENT OPCOUNT FOR USE OF FUNCTION PYTHAG.
            OPS = OPS + OPST
 1001 RETURN
      END
      SUBROUTINE TRIDIB(N,EPS1,D,E,E2,LB,UB,M11,M,W,IND,IERR,RV4,RV5)
*
*     EISPACK ROUTINE.
*     MODIFIED FOR COMPARISON WITH LAPACK ROUTINES.
*
*     CONVERGENCE TEST WAS MODIFIED TO BE THE SAME AS IN DSTEBZ.
*
C
      INTEGER I,J,K,L,M,N,P,Q,R,S,II,M1,M2,M11,M22,TAG,IERR,ISTURM
      DOUBLE PRECISION D(N),E(N),E2(N),W(M),RV4(N),RV5(N)
      DOUBLE PRECISION U,V,LB,T1,T2,UB,XU,X0,X1,EPS1,TST1,TST2,EPSLON
      INTEGER IND(M)
      external epslon
*
*     COMMON BLOCK TO RETURN OPERATION COUNT AND ITERATION COUNT
*     ITCNT IS INITIALIZED TO 0, OPS IS ONLY INCREMENTED
*     .. COMMON BLOCKS ..
      COMMON             / LATIME / OPS, ITCNT
*     ..
*     .. SCALARS IN COMMON ..
      DOUBLE PRECISION   ITCNT, OPS
*     ..
C
C     THIS SUBROUTINE IS A TRANSLATION OF THE ALGOL PROCEDURE BISECT,
C     NUM. MATH. 9, 386-393(1967) BY BARTH, MARTIN, AND WILKINSON.
C     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 249-256(1971).
C
C     THIS SUBROUTINE FINDS THOSE EIGENVALUES OF A TRIDIAGONAL
C     SYMMETRIC MATRIX BETWEEN SPECIFIED BOUNDARY INDICES,
C     USING BISECTION.
C
C     ON INPUT
C
C        N IS THE ORDER OF THE MATRIX.
C
C        EPS1 IS AN ABSOLUTE ERROR TOLERANCE FOR THE COMPUTED
C          EIGENVALUES.  IF THE INPUT EPS1 IS NON-POSITIVE,
C          IT IS RESET FOR EACH SUBMATRIX TO A DEFAULT VALUE,
C          NAMELY, MINUS THE PRODUCT OF THE RELATIVE MACHINE
C          PRECISION AND THE 1-NORM OF THE SUBMATRIX.
C
C        D CONTAINS THE DIAGONAL ELEMENTS OF THE INPUT MATRIX.
C
C        E CONTAINS THE SUBDIAGONAL ELEMENTS OF THE INPUT MATRIX
C          IN ITS LAST N-1 POSITIONS.  E(1) IS ARBITRARY.
C
C        E2 CONTAINS THE SQUARES OF THE CORRESPONDING ELEMENTS OF E.
C          E2(1) IS ARBITRARY.
C
C        M11 SPECIFIES THE LOWER BOUNDARY INDEX FOR THE DESIRED
C          EIGENVALUES.
C
C        M SPECIFIES THE NUMBER OF EIGENVALUES DESIRED.  THE UPPER
C          BOUNDARY INDEX M22 IS THEN OBTAINED AS M22=M11+M-1.
C
C     ON OUTPUT
C
C        EPS1 IS UNALTERED UNLESS IT HAS BEEN RESET TO ITS
C          (LAST) DEFAULT VALUE.
C
C        D AND E ARE UNALTERED.
C
C        ELEMENTS OF E2, CORRESPONDING TO ELEMENTS OF E REGARDED
C          AS NEGLIGIBLE, HAVE BEEN REPLACED BY ZERO CAUSING THE
C          MATRIX TO SPLIT INTO A DIRECT SUM OF SUBMATRICES.
C          E2(1) IS ALSO SET TO ZERO.
C
C        LB AND UB DEFINE AN INTERVAL CONTAINING EXACTLY THE DESIRED
C          EIGENVALUES.
C
C        W CONTAINS, IN ITS FIRST M POSITIONS, THE EIGENVALUES
C          BETWEEN INDICES M11 AND M22 IN ASCENDING ORDER.
C
C        IND CONTAINS IN ITS FIRST M POSITIONS THE SUBMATRIX INDICES
C          ASSOCIATED WITH THE CORRESPONDING EIGENVALUES IN W --
C          1 FOR EIGENVALUES BELONGING TO THE FIRST SUBMATRIX FROM
C          THE TOP, 2 FOR THOSE BELONGING TO THE SECOND SUBMATRIX, ETC..
C
C        IERR IS SET TO
C          ZERO       FOR NORMAL RETURN,
C          3*N+1      IF MULTIPLE EIGENVALUES AT INDEX M11 MAKE
C                     UNIQUE SELECTION IMPOSSIBLE,
C          3*N+2      IF MULTIPLE EIGENVALUES AT INDEX M22 MAKE
C                     UNIQUE SELECTION IMPOSSIBLE.
C
C        RV4 AND RV5 ARE TEMPORARY STORAGE ARRAYS.
C
C     NOTE THAT SUBROUTINE TQL1, IMTQL1, OR TQLRAT IS GENERALLY FASTER
C     THAN TRIDIB, IF MORE THAN N/4 EIGENVALUES ARE TO BE FOUND.
C
C     QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO BURTON S. GARBOW,
C     MATHEMATICS AND COMPUTER SCIENCE DIV, ARGONNE NATIONAL LABORATORY
C
C     THIS VERSION DATED AUGUST 1983.
C
C     ------------------------------------------------------------------
C
      DOUBLE PRECISION ONE
      PARAMETER        ( ONE = 1.0D0 )
      DOUBLE PRECISION RELFAC
      PARAMETER        ( RELFAC = 2.0D0 )
      DOUBLE PRECISION ATOLI, RTOLI, SAFEMN, TMP1, TMP2, TNORM, ULP
      DOUBLE PRECISION DLAMCH, PIVMIN
      EXTERNAL DLAMCH
*        INITIALIZE ITERATION COUNT.
            ITCNT = 0
      SAFEMN = DLAMCH( 'S' )
      ULP = DLAMCH( 'E' )*DLAMCH( 'B' )
      RTOLI = ULP*RELFAC
      IERR = 0
      TAG = 0
      XU = D(1)
      X0 = D(1)
      U = 0.0D0
C     .......... LOOK FOR SMALL SUB-DIAGONAL ENTRIES AND DETERMINE AN
C                INTERVAL CONTAINING ALL THE EIGENVALUES ..........
      PIVMIN = ONE
      DO 40 I = 1, N
         X1 = U
         U = 0.0D0
         IF (I .NE. N) U = DABS(E(I+1))
         XU = DMIN1(D(I)-(X1+U),XU)
         X0 = DMAX1(D(I)+(X1+U),X0)
         IF (I .EQ. 1) GO TO 20
CCC         TST1 = DABS(D(I)) + DABS(D(I-1))
CCC         TST2 = TST1 + DABS(E(I))
CCC         IF (TST2 .GT. TST1) GO TO 40
         TMP1 = E( I )**2
         IF( ABS( D(I)*D(I-1) )*ULP**2+SAFEMN.LE.TMP1 ) THEN
            PIVMIN = MAX( PIVMIN, TMP1 )
            GO TO 40
         END IF
   20    E2(I) = 0.0D0
   40 CONTINUE
      PIVMIN = PIVMIN*SAFEMN
      TNORM = MAX( ABS( XU ), ABS( X0 ) )
      ATOLI = ULP*TNORM
*        INCREMENT OPCOUNT FOR DETERMINING IF MATRIX SPLITS.
            OPS = OPS + 9*( N-1 )
C
      X1 = N
      X1 = X1 * EPSLON(DMAX1(DABS(XU),DABS(X0)))
      XU = XU - X1
      T1 = XU
      X0 = X0 + X1
      T2 = X0
C     .......... DETERMINE AN INTERVAL CONTAINING EXACTLY
C                THE DESIRED EIGENVALUES ..........
      P = 1
      Q = N
      M1 = M11 - 1
      IF (M1 .EQ. 0) GO TO 75
      ISTURM = 1
   50 V = X1
      X1 = XU + (X0 - XU) * 0.5D0
      IF (X1 .EQ. V) GO TO 980
      GO TO 320
   60 IF (S - M1) 65, 73, 70
   65 XU = X1
      GO TO 50
   70 X0 = X1
      GO TO 50
   73 XU = X1
      T1 = X1
   75 M22 = M1 + M
      IF (M22 .EQ. N) GO TO 90
      X0 = T2
      ISTURM = 2
      GO TO 50
   80 IF (S - M22) 65, 85, 70
   85 T2 = X1
   90 Q = 0
      R = 0
C     .......... ESTABLISH AND PROCESS NEXT SUBMATRIX, REFINING
C                INTERVAL BY THE GERSCHGORIN BOUNDS ..........
  100 IF (R .EQ. M) GO TO 1001
      TAG = TAG + 1
      P = Q + 1
      XU = D(P)
      X0 = D(P)
      U = 0.0D0
C
      DO 120 Q = P, N
         X1 = U
         U = 0.0D0
         V = 0.0D0
         IF (Q .EQ. N) GO TO 110
         U = DABS(E(Q+1))
         V = E2(Q+1)
  110    XU = DMIN1(D(Q)-(X1+U),XU)
         X0 = DMAX1(D(Q)+(X1+U),X0)
         IF (V .EQ. 0.0D0) GO TO 140
  120 CONTINUE
*        INCREMENT OPCOUNT FOR REFINING INTERVAL.
            OPS = OPS + ( N-P+1 )*2
C
  140 X1 = EPSLON(DMAX1(DABS(XU),DABS(X0)))
      IF (EPS1 .LE. 0.0D0) EPS1 = -X1
      IF (P .NE. Q) GO TO 180
C     .......... CHECK FOR ISOLATED ROOT WITHIN INTERVAL ..........
      IF (T1 .GT. D(P) .OR. D(P) .GE. T2) GO TO 940
      M1 = P
      M2 = P
      RV5(P) = D(P)
      GO TO 900
  180 X1 = X1 * (Q - P + 1)
      LB = DMAX1(T1,XU-X1)
      UB = DMIN1(T2,X0+X1)
      X1 = LB
      ISTURM = 3
      GO TO 320
  200 M1 = S + 1
      X1 = UB
      ISTURM = 4
      GO TO 320
  220 M2 = S
      IF (M1 .GT. M2) GO TO 940
C     .......... FIND ROOTS BY BISECTION ..........
      X0 = UB
      ISTURM = 5
C
      DO 240 I = M1, M2
         RV5(I) = UB
         RV4(I) = LB
  240 CONTINUE
C     .......... LOOP FOR K-TH EIGENVALUE
C                FOR K=M2 STEP -1 UNTIL M1 DO --
C                (-DO- NOT USED TO LEGALIZE -COMPUTED GO TO-) ..........
      K = M2
  250    XU = LB
C     .......... FOR I=K STEP -1 UNTIL M1 DO -- ..........
         DO 260 II = M1, K
            I = M1 + K - II
            IF (XU .GE. RV4(I)) GO TO 260
            XU = RV4(I)
            GO TO 280
  260    CONTINUE
C
  280    IF (X0 .GT. RV5(K)) X0 = RV5(K)
C     .......... NEXT BISECTION STEP ..........
  300    X1 = (XU + X0) * 0.5D0
CCC         IF ((X0 - XU) .LE. DABS(EPS1)) GO TO 420
CCC         TST1 = 2.0D0 * (DABS(XU) + DABS(X0))
CCC         TST2 = TST1 + (X0 - XU)
CCC         IF (TST2 .EQ. TST1) GO TO 420
         TMP1 = ABS( X0 - XU )
         TMP2 = MAX( ABS( X0 ), ABS( XU ) )
         IF( TMP1.LT.MAX( ATOLI, PIVMIN, RTOLI*TMP2 ) )
     $      GO TO 420
C     .......... IN-LINE PROCEDURE FOR STURM SEQUENCE ..........
  320    S = P - 1
         U = 1.0D0
C
         DO 340 I = P, Q
            IF (U .NE. 0.0D0) GO TO 325
            V = DABS(E(I)) / EPSLON(1.0D0)
            IF (E2(I) .EQ. 0.0D0) V = 0.0D0
            GO TO 330
  325       V = E2(I) / U
  330       U = D(I) - X1 - V
            IF (U .LT. 0.0D0) S = S + 1
  340    CONTINUE
*           INCREMENT OPCOUNT FOR STURM SEQUENCE.
               OPS = OPS + ( Q-P+1 )*3
*           INCREMENT ITERATION COUNTER.
               ITCNT = ITCNT + 1
C
         GO TO (60,80,200,220,360), ISTURM
C     .......... REFINE INTERVALS ..........
  360    IF (S .GE. K) GO TO 400
         XU = X1
         IF (S .GE. M1) GO TO 380
         RV4(M1) = X1
         GO TO 300
  380    RV4(S+1) = X1
         IF (RV5(S) .GT. X1) RV5(S) = X1
         GO TO 300
  400    X0 = X1
         GO TO 300
C     .......... K-TH EIGENVALUE FOUND ..........
  420    RV5(K) = X1
      K = K - 1
      IF (K .GE. M1) GO TO 250
C     .......... ORDER EIGENVALUES TAGGED WITH THEIR
C                SUBMATRIX ASSOCIATIONS ..........
  900 S = R
      R = R + M2 - M1 + 1
      J = 1
      K = M1
C
      DO 920 L = 1, R
         IF (J .GT. S) GO TO 910
         IF (K .GT. M2) GO TO 940
         IF (RV5(K) .GE. W(L)) GO TO 915
C
         DO 905 II = J, S
            I = L + S - II
            W(I+1) = W(I)
            IND(I+1) = IND(I)
  905    CONTINUE
C
  910    W(L) = RV5(K)
         IND(L) = TAG
         K = K + 1
         GO TO 920
  915    J = J + 1
  920 CONTINUE
C
  940 IF (Q .LT. N) GO TO 100
      GO TO 1001
C     .......... SET ERROR -- INTERVAL CANNOT BE FOUND CONTAINING
C                EXACTLY THE DESIRED EIGENVALUES ..........
  980 IERR = 3 * N + ISTURM
 1001 LB = T1
      UB = T2
      RETURN
      END
      SUBROUTINE DSVDC(X,LDX,N,P,S,E,U,LDU,V,LDV,WORK,JOB,INFO)
      INTEGER LDX,N,P,LDU,LDV,JOB,INFO
      DOUBLE PRECISION X(LDX,*),S(*),E(*),U(LDU,*),V(LDV,*),WORK(*)
*
*     COMMON BLOCK TO RETURN OPERATION COUNT AND ITERATION COUNT
*     ITCNT IS INITIALIZED TO 0, IOPS IS ONLY INCREMENTED
*     IOPST IS USED TO ACCUMULATE SMALL CONTRIBUTIONS TO IOPS
*     TO AVOID ROUNDOFF ERROR
*     .. COMMON BLOCKS ..
      COMMON /LATIME/ IOPS, ITCNT
*     ..
*     .. SCALARS IN COMMON ..
      DOUBLE PRECISION IOPS, ITCNT, IOPST
*     ..
C
C
C     DSVDC IS A SUBROUTINE TO REDUCE A DOUBLE PRECISION NXP MATRIX X
C     BY ORTHOGONAL TRANSFORMATIONS U AND V TO DIAGONAL FORM.  THE
C     DIAGONAL ELEMENTS S(I) ARE THE SINGULAR VALUES OF X.  THE
C     COLUMNS OF U ARE THE CORRESPONDING LEFT SINGULAR VECTORS,
C     AND THE COLUMNS OF V THE RIGHT SINGULAR VECTORS.
C
C     ON ENTRY
C
C         X         DOUBLE PRECISION(LDX,P), WHERE LDX.GE.N.
C                   X CONTAINS THE MATRIX WHOSE SINGULAR VALUE
C                   DECOMPOSITION IS TO BE COMPUTED.  X IS
C                   DESTROYED BY DSVDC.
C
C         LDX       INTEGER.
C                   LDX IS THE LEADING DIMENSION OF THE ARRAY X.
C
C         N         INTEGER.
C                   N IS THE NUMBER OF ROWS OF THE MATRIX X.
C
C         P         INTEGER.
C                   P IS THE NUMBER OF COLUMNS OF THE MATRIX X.
C
C         LDU       INTEGER.
C                   LDU IS THE LEADING DIMENSION OF THE ARRAY U.
C                   (SEE BELOW).
C
C         LDV       INTEGER.
C                   LDV IS THE LEADING DIMENSION OF THE ARRAY V.
C                   (SEE BELOW).
C
C         WORK      DOUBLE PRECISION(N).
C                   WORK IS A SCRATCH ARRAY.
C
C         JOB       INTEGER.
C                   JOB CONTROLS THE COMPUTATION OF THE SINGULAR
C                   VECTORS.  IT HAS THE DECIMAL EXPANSION AB
C                   WITH THE FOLLOWING MEANING
C
C                        A.EQ.0    DO NOT COMPUTE THE LEFT SINGULAR
C                                  VECTORS.
C                        A.EQ.1    RETURN THE N LEFT SINGULAR VECTORS
C                                  IN U.
C                        A.GE.2    RETURN THE FIRST MIN(N,P) SINGULAR
C                                  VECTORS IN U.
C                        B.EQ.0    DO NOT COMPUTE THE RIGHT SINGULAR
C                                  VECTORS.
C                        B.EQ.1    RETURN THE RIGHT SINGULAR VECTORS
C                                  IN V.
C
C     ON RETURN
C
C         S         DOUBLE PRECISION(MM), WHERE MM=MIN(N+1,P).
C                   THE FIRST MIN(N,P) ENTRIES OF S CONTAIN THE
C                   SINGULAR VALUES OF X ARRANGED IN DESCENDING
C                   ORDER OF MAGNITUDE.
C
C         E         DOUBLE PRECISION(P),
C                   E ORDINARILY CONTAINS ZEROS.  HOWEVER SEE THE
C                   DISCUSSION OF INFO FOR EXCEPTIONS.
C
C         U         DOUBLE PRECISION(LDU,K), WHERE LDU.GE.N.  IF
C                                   JOBA.EQ.1 THEN K.EQ.N, IF JOBA.GE.2
C                                   THEN K.EQ.MIN(N,P).
C                   U CONTAINS THE MATRIX OF LEFT SINGULAR VECTORS.
C                   U IS NOT REFERENCED IF JOBA.EQ.0.  IF N.LE.P
C                   OR IF JOBA.EQ.2, THEN U MAY BE IDENTIFIED WITH X
C                   IN THE SUBROUTINE CALL.
C
C         V         DOUBLE PRECISION(LDV,P), WHERE LDV.GE.P.
C                   V CONTAINS THE MATRIX OF RIGHT SINGULAR VECTORS.
C                   V IS NOT REFERENCED IF JOB.EQ.0.  IF P.LE.N,
C                   THEN V MAY BE IDENTIFIED WITH X IN THE
C                   SUBROUTINE CALL.
C
C         INFO      INTEGER.
C                   THE SINGULAR VALUES (AND THEIR CORRESPONDING
C                   SINGULAR VECTORS) S(INFO+1),S(INFO+2),...,S(M)
C                   ARE CORRECT (HERE M=MIN(N,P)).  THUS IF
C                   INFO.EQ.0, ALL THE SINGULAR VALUES AND THEIR
C                   VECTORS ARE CORRECT.  IN ANY EVENT, THE MATRIX
C                   B = TRANS(U)*X*V IS THE BIDIAGONAL MATRIX
C                   WITH THE ELEMENTS OF S ON ITS DIAGONAL AND THE
C                   ELEMENTS OF E ON ITS SUPER-DIAGONAL (TRANS(U)
C                   IS THE TRANSPOSE OF U).  THUS THE SINGULAR
C                   VALUES OF X AND B ARE THE SAME.
C
C     LINPACK. THIS VERSION DATED 08/14/78 .
C              CORRECTION MADE TO SHIFT 2/84.
C     G.W. STEWART, UNIVERSITY OF MARYLAND, ARGONNE NATIONAL LAB.
C
C     DSVDC USES THE FOLLOWING FUNCTIONS AND SUBPROGRAMS.
C
C     EXTERNAL DROT
C     BLAS DAXPY,DDOT,DSCAL,DSWAP,DNRM2,DROTG
C     FORTRAN DABS,DMAX1,MAX0,MIN0,MOD,DSQRT
C
C     INTERNAL VARIABLES
C
      INTEGER I,ITER,J,JOBU,K,KASE,KK,L,LL,LLS,LM1,LP1,LS,LU,M,MAXIT,
     *        MM,MM1,MP1,NCT,NCTP1,NCU,NRT,NRTP1
      DOUBLE PRECISION DDOT,T
      DOUBLE PRECISION B,C,CS,EL,EMM1,F,G,DNRM2,SCALE,SHIFT,SL,SM,SN,
     *                 SMM1,T1,TEST
*     DOUBLE PRECISION ZTEST,R
      LOGICAL WANTU,WANTV
*
*     GET EPS FROM DLAMCH FOR NEW STOPPING CRITERION
      EXTERNAL DLAMCH, dnrm2, ddot
      DOUBLE PRECISION DLAMCH, EPS
      IF (N.LE.0 .OR. P.LE.0) RETURN
      EPS = DLAMCH( 'EPSILON' )
*
C
C
C     SET THE MAXIMUM NUMBER OF ITERATIONS.
C
      MAXIT = 50
C
C     DETERMINE WHAT IS TO BE COMPUTED.
C
      WANTU = .FALSE.
      WANTV = .FALSE.
      JOBU = MOD(JOB,100)/10
      NCU = N
      IF (JOBU .GT. 1) NCU = MIN0(N,P)
      IF (JOBU .NE. 0) WANTU = .TRUE.
      IF (MOD(JOB,10) .NE. 0) WANTV = .TRUE.
C
C     REDUCE X TO BIDIAGONAL FORM, STORING THE DIAGONAL ELEMENTS
C     IN S AND THE SUPER-DIAGONAL ELEMENTS IN E.
C
*
*     INITIALIZE OP COUNT
      IOPST = 0
      INFO = 0
      NCT = MIN0(N-1,P)
      NRT = MAX0(0,MIN0(P-2,N))
      LU = MAX0(NCT,NRT)
      IF (LU .LT. 1) GO TO 170
      DO 160 L = 1, LU
         LP1 = L + 1
         IF (L .GT. NCT) GO TO 20
C
C           COMPUTE THE TRANSFORMATION FOR THE L-TH COLUMN AND
C           PLACE THE L-TH DIAGONAL IN S(L).
C
*
*           INCREMENT OP COUNT
            IOPS = IOPS + (2*(N-L+1)+1)
            S(L) = DNRM2(N-L+1,X(L,L),1)
            IF (S(L) .EQ. 0.0D0) GO TO 10
               IF (X(L,L) .NE. 0.0D0) S(L) = DSIGN(S(L),X(L,L))
*
*              INCREMENT OP COUNT
               IOPS = IOPS + (N-L+3)
               CALL DSCAL(N-L+1,1.0D0/S(L),X(L,L),1)
               X(L,L) = 1.0D0 + X(L,L)
   10       CONTINUE
            S(L) = -S(L)
   20    CONTINUE
         IF (P .LT. LP1) GO TO 50
         DO 40 J = LP1, P
            IF (L .GT. NCT) GO TO 30
            IF (S(L) .EQ. 0.0D0) GO TO 30
C
C              APPLY THE TRANSFORMATION.
C
*
*              INCREMENT OP COUNT
               IOPS = IOPS + (4*(N-L)+5)
               T = -DDOT(N-L+1,X(L,L),1,X(L,J),1)/X(L,L)
               CALL DAXPY(N-L+1,T,X(L,L),1,X(L,J),1)
   30       CONTINUE
C
C           PLACE THE L-TH ROW OF X INTO  E FOR THE
C           SUBSEQUENT CALCULATION OF THE ROW TRANSFORMATION.
C
            E(J) = X(L,J)
   40    CONTINUE
   50    CONTINUE
         IF (.NOT.WANTU .OR. L .GT. NCT) GO TO 70
C
C           PLACE THE TRANSFORMATION IN U FOR SUBSEQUENT BACK
C           MULTIPLICATION.
C
            DO 60 I = L, N
               U(I,L) = X(I,L)
   60       CONTINUE
   70    CONTINUE
         IF (L .GT. NRT) GO TO 150
C
C           COMPUTE THE L-TH ROW TRANSFORMATION AND PLACE THE
C           L-TH SUPER-DIAGONAL IN E(L).
C
*
*           INCREMENT OP COUNT
            IOPS = IOPS + (2*(P-L)+1)
            E(L) = DNRM2(P-L,E(LP1),1)
            IF (E(L) .EQ. 0.0D0) GO TO 80
               IF (E(LP1) .NE. 0.0D0) E(L) = DSIGN(E(L),E(LP1))
*
*              INCREMENT OP COUNT
               IOPS = IOPS + (P-L+2)
               CALL DSCAL(P-L,1.0D0/E(L),E(LP1),1)
               E(LP1) = 1.0D0 + E(LP1)
   80       CONTINUE
            E(L) = -E(L)
            IF (LP1 .GT. N .OR. E(L) .EQ. 0.0D0) GO TO 120
C
C              APPLY THE TRANSFORMATION.
C
               DO 90 I = LP1, N
                  WORK(I) = 0.0D0
   90          CONTINUE
*
*              INCREMENT OP COUNT
               IOPS = IOPS + DBLE(4*(N-L)+1)*(P-L)
               DO 100 J = LP1, P
                  CALL DAXPY(N-L,E(J),X(LP1,J),1,WORK(LP1),1)
  100          CONTINUE
               DO 110 J = LP1, P
                  CALL DAXPY(N-L,-E(J)/E(LP1),WORK(LP1),1,X(LP1,J),1)
  110          CONTINUE
  120       CONTINUE
            IF (.NOT.WANTV) GO TO 140
C
C              PLACE THE TRANSFORMATION IN V FOR SUBSEQUENT
C              BACK MULTIPLICATION.
C
               DO 130 I = LP1, P
                  V(I,L) = E(I)
  130          CONTINUE
  140       CONTINUE
  150    CONTINUE
  160 CONTINUE
  170 CONTINUE
C
C     SET UP THE FINAL BIDIAGONAL MATRIX OR ORDER M.
C
      M = MIN0(P,N+1)
      NCTP1 = NCT + 1
      NRTP1 = NRT + 1
      IF (NCT .LT. P) S(NCTP1) = X(NCTP1,NCTP1)
      IF (N .LT. M) S(M) = 0.0D0
      IF (NRTP1 .LT. M) E(NRTP1) = X(NRTP1,M)
      E(M) = 0.0D0
C
C     IF REQUIRED, GENERATE U.
C
      IF (.NOT.WANTU) GO TO 300
         IF (NCU .LT. NCTP1) GO TO 200
         DO 190 J = NCTP1, NCU
            DO 180 I = 1, N
               U(I,J) = 0.0D0
  180       CONTINUE
            U(J,J) = 1.0D0
  190    CONTINUE
  200    CONTINUE
         IF (NCT .LT. 1) GO TO 290
         DO 280 LL = 1, NCT
            L = NCT - LL + 1
            IF (S(L) .EQ. 0.0D0) GO TO 250
               LP1 = L + 1
               IF (NCU .LT. LP1) GO TO 220
*
*              INCREMENT OP COUNT
               IOPS = IOPS + (DBLE(4*(N-L)+5)*(NCU-L)+(N-L+2))
               DO 210 J = LP1, NCU
                  T = -DDOT(N-L+1,U(L,L),1,U(L,J),1)/U(L,L)
                  CALL DAXPY(N-L+1,T,U(L,L),1,U(L,J),1)
  210          CONTINUE
  220          CONTINUE
               CALL DSCAL(N-L+1,-1.0D0,U(L,L),1)
               U(L,L) = 1.0D0 + U(L,L)
               LM1 = L - 1
               IF (LM1 .LT. 1) GO TO 240
               DO 230 I = 1, LM1
                  U(I,L) = 0.0D0
  230          CONTINUE
  240          CONTINUE
            GO TO 270
  250       CONTINUE
               DO 260 I = 1, N
                  U(I,L) = 0.0D0
  260          CONTINUE
               U(L,L) = 1.0D0
  270       CONTINUE
  280    CONTINUE
  290    CONTINUE
  300 CONTINUE
C
C     IF IT IS REQUIRED, GENERATE V.
C
      IF (.NOT.WANTV) GO TO 350
         DO 340 LL = 1, P
            L = P - LL + 1
            LP1 = L + 1
            IF (L .GT. NRT) GO TO 320
            IF (E(L) .EQ. 0.0D0) GO TO 320
*
*              INCREMENT OP COUNT
               IOPS = IOPS + DBLE(4*(P-L)+1)*(P-L)
               DO 310 J = LP1, P
                  T = -DDOT(P-L,V(LP1,L),1,V(LP1,J),1)/V(LP1,L)
                  CALL DAXPY(P-L,T,V(LP1,L),1,V(LP1,J),1)
  310          CONTINUE
  320       CONTINUE
            DO 330 I = 1, P
               V(I,L) = 0.0D0
  330       CONTINUE
            V(L,L) = 1.0D0
  340    CONTINUE
  350 CONTINUE
C
C     MAIN ITERATION LOOP FOR THE SINGULAR VALUES.
C
      MM = M
*
*     INITIALIZE ITERATION COUNTER
      ITCNT = 0
      ITER = 0
  360 CONTINUE
C
C        QUIT IF ALL THE SINGULAR VALUES HAVE BEEN FOUND.
C
C     ...EXIT
         IF (M .EQ. 0) GO TO 620
C
C        IF TOO MANY ITERATIONS HAVE BEEN PERFORMED, SET
C        FLAG AND RETURN.
C
*
*        UPDATE ITERATION COUNTER
         ITCNT = ITER
         IF (ITER .LT. MAXIT) GO TO 370
            INFO = M
C     ......EXIT
            GO TO 620
  370    CONTINUE
C
C        THIS SECTION OF THE PROGRAM INSPECTS FOR
C        NEGLIGIBLE ELEMENTS IN THE S AND E ARRAYS.  ON
C        COMPLETION THE VARIABLES KASE AND L ARE SET AS FOLLOWS.
C
C           KASE = 1     IF S(M) AND E(L-1) ARE NEGLIGIBLE AND L.LT.M
C           KASE = 2     IF S(L) IS NEGLIGIBLE AND L.LT.M
C           KASE = 3     IF E(L-1) IS NEGLIGIBLE, L.LT.M, AND
C                        S(L), ..., S(M) ARE NOT NEGLIGIBLE (QR STEP).
C           KASE = 4     IF E(M-1) IS NEGLIGIBLE (CONVERGENCE).
C
         DO 390 LL = 1, M
            L = M - LL
C        ...EXIT
            IF (L .EQ. 0) GO TO 400
*
*           INCREMENT OP COUNT
            IOPST = IOPST + 2
            TEST = DABS(S(L)) + DABS(S(L+1))
*
*           REPLACE STOPPING CRITERION WITH NEW ONE AS IN LAPACK
*
*           ZTEST = TEST + DABS(E(L))
*           IF (ZTEST .NE. TEST) GO TO 380
            IF (DABS(E(L)) .GT. EPS * TEST) GOTO 380
*
               E(L) = 0.0D0
C        ......EXIT
               GO TO 400
  380       CONTINUE
  390    CONTINUE
  400    CONTINUE
         IF (L .NE. M - 1) GO TO 410
            KASE = 4
         GO TO 480
  410    CONTINUE
            LP1 = L + 1
            MP1 = M + 1
            DO 430 LLS = LP1, MP1
               LS = M - LLS + LP1
C           ...EXIT
               IF (LS .EQ. L) GO TO 440
               TEST = 0.0D0
*
*              INCREMENT OP COUNT
               IOPST = IOPST + 3
               IF (LS .NE. M) TEST = TEST + DABS(E(LS))
               IF (LS .NE. L + 1) TEST = TEST + DABS(E(LS-1))
*
*              REPLACE STOPPING CRITERION WITH NEW ONE AS IN LAPACK
*
*              ZTEST = TEST + DABS(S(LS))
*              IF (ZTEST .NE. TEST) GO TO 420
               IF (DABS(S(LS)) .GT. EPS * TEST) GOTO 420
*
                  S(LS) = 0.0D0
C           ......EXIT
                  GO TO 440
  420          CONTINUE
  430       CONTINUE
  440       CONTINUE
            IF (LS .NE. L) GO TO 450
               KASE = 3
            GO TO 470
  450       CONTINUE
            IF (LS .NE. M) GO TO 460
               KASE = 1
            GO TO 470
  460       CONTINUE
               KASE = 2
               L = LS
  470       CONTINUE
  480    CONTINUE
         L = L + 1
C
C        PERFORM THE TASK INDICATED BY KASE.
C
         GO TO (490,520,540,570), KASE
C
C        DEFLATE NEGLIGIBLE S(M).
C
  490    CONTINUE
            MM1 = M - 1
            F = E(M-1)
            E(M-1) = 0.0D0
*
*           INCREMENT OP COUNT
            IOPS = IOPS + ((MM1-L+1)*13 - 2)
            IF (WANTV) IOPS = IOPS + DBLE(MM1-L+1)*6*P
            DO 510 KK = L, MM1
               K = MM1 - KK + L
               T1 = S(K)
               CALL DROTG(T1,F,CS,SN)
               S(K) = T1
               IF (K .EQ. L) GO TO 500
                  F = -SN*E(K-1)
                  E(K-1) = CS*E(K-1)
  500          CONTINUE
               IF (WANTV) CALL DROT(P,V(1,K),1,V(1,M),1,CS,SN)
  510       CONTINUE
         GO TO 610
C
C        SPLIT AT NEGLIGIBLE S(L).
C
  520    CONTINUE
            F = E(L-1)
            E(L-1) = 0.0D0
*
*           INCREMENT OP COUNT
            IOPS = IOPS + (M-L+1)*13
            IF (WANTU) IOPS = IOPS + DBLE(M-L+1)*6*N
            DO 530 K = L, M
               T1 = S(K)
               CALL DROTG(T1,F,CS,SN)
               S(K) = T1
               F = -SN*E(K)
               E(K) = CS*E(K)
               IF (WANTU) CALL DROT(N,U(1,K),1,U(1,L-1),1,CS,SN)
  530       CONTINUE
         GO TO 610
C
C        PERFORM ONE QR STEP.
C
  540    CONTINUE
C
C           CALCULATE THE SHIFT.
C
*
*           INCREMENT OP COUNT
            IOPST = IOPST + 23
            SCALE = DMAX1(DABS(S(M)),DABS(S(M-1)),DABS(E(M-1)),
     *                    DABS(S(L)),DABS(E(L)))
            SM = S(M)/SCALE
            SMM1 = S(M-1)/SCALE
            EMM1 = E(M-1)/SCALE
            SL = S(L)/SCALE
            EL = E(L)/SCALE
            B = ((SMM1 + SM)*(SMM1 - SM) + EMM1**2)/2.0D0
            C = (SM*EMM1)**2
            SHIFT = 0.0D0
            IF (B .EQ. 0.0D0 .AND. C .EQ. 0.0D0) GO TO 550
               SHIFT = DSQRT(B**2+C)
               IF (B .LT. 0.0D0) SHIFT = -SHIFT
               SHIFT = C/(B + SHIFT)
  550       CONTINUE
            F = (SL + SM)*(SL - SM) + SHIFT
            G = SL*EL
C
C           CHASE ZEROS.
C
            MM1 = M - 1
*
*           INCREMENT OP COUNT
            IOPS = IOPS + (MM1-L+1)*38
            IF (WANTV) IOPS = IOPS+DBLE(MM1-L+1)*6*P
            IF (WANTU) IOPS = IOPS+DBLE(MAX((MIN(MM1,N-1)-L+1),0))*6*N
            DO 560 K = L, MM1
               CALL DROTG(F,G,CS,SN)
               IF (K .NE. L) E(K-1) = F
               F = CS*S(K) + SN*E(K)
               E(K) = CS*E(K) - SN*S(K)
               G = SN*S(K+1)
               S(K+1) = CS*S(K+1)
               IF (WANTV) CALL DROT(P,V(1,K),1,V(1,K+1),1,CS,SN)
               CALL DROTG(F,G,CS,SN)
               S(K) = F
               F = CS*E(K) + SN*S(K+1)
               S(K+1) = -SN*E(K) + CS*S(K+1)
               G = SN*E(K+1)
               E(K+1) = CS*E(K+1)
               IF (WANTU .AND. K .LT. N)
     *            CALL DROT(N,U(1,K),1,U(1,K+1),1,CS,SN)
  560       CONTINUE
            E(M-1) = F
            ITER = ITER + 1
         GO TO 610
C
C        CONVERGENCE.
C
  570    CONTINUE
C
C           MAKE THE SINGULAR VALUE  POSITIVE.
C
            IF (S(L) .GE. 0.0D0) GO TO 580
               S(L) = -S(L)
*
*              INCREMENT OP COUNT
               IF (WANTV) IOPS = IOPS + P
               IF (WANTV) CALL DSCAL(P,-1.0D0,V(1,L),1)
  580       CONTINUE
C
C           ORDER THE SINGULAR VALUE.
C
  590       IF (L .EQ. MM) GO TO 600
C           ...EXIT
               IF (S(L) .GE. S(L+1)) GO TO 600
               T = S(L)
               S(L) = S(L+1)
               S(L+1) = T
               IF (WANTV .AND. L .LT. P)
     *            CALL DSWAP(P,V(1,L),1,V(1,L+1),1)
               IF (WANTU .AND. L .LT. N)
     *            CALL DSWAP(N,U(1,L),1,U(1,L+1),1)
               L = L + 1
            GO TO 590
  600       CONTINUE
            ITER = 0
            M = M - 1
  610    CONTINUE
      GO TO 360
  620 CONTINUE
*
*     COMPUTE FINAL OPCOUNT
      IOPS = IOPS + IOPST
      RETURN
      END
      SUBROUTINE QZHES(NM,N,A,B,MATZ,Z)
C
      INTEGER I,J,K,L,N,LB,L1,NM,NK1,NM1,NM2
      DOUBLE PRECISION A(NM,N),B(NM,N),Z(NM,N)
      DOUBLE PRECISION R,S,T,U1,U2,V1,V2,RHO
      LOGICAL MATZ
*
*     ---------------------- BEGIN TIMING CODE -------------------------
*     COMMON BLOCK TO RETURN OPERATION COUNT AND ITERATION COUNT
*     ITCNT IS INITIALIZED TO 0, OPS IS ONLY INCREMENTED
*     OPST IS USED TO ACCUMULATE SMALL CONTRIBUTIONS TO OPS
*     TO AVOID ROUNDOFF ERROR
*     .. COMMON BLOCKS ..
      COMMON             / LATIME / OPS, ITCNT
*     ..
*     .. SCALARS IN COMMON ..
      DOUBLE PRECISION   ITCNT, OPS
*     ..
*     ----------------------- END TIMING CODE --------------------------
*
C
C     THIS SUBROUTINE IS THE FIRST STEP OF THE QZ ALGORITHM
C     FOR SOLVING GENERALIZED MATRIX EIGENVALUE PROBLEMS,
C     SIAM J. NUMER. ANAL. 10, 241-256(1973) BY MOLER AND STEWART.
C
C     THIS SUBROUTINE ACCEPTS A PAIR OF REAL GENERAL MATRICES AND
C     REDUCES ONE OF THEM TO UPPER HESSENBERG FORM AND THE OTHER
C     TO UPPER TRIANGULAR FORM USING ORTHOGONAL TRANSFORMATIONS.
C     IT IS USUALLY FOLLOWED BY  QZIT,  QZVAL  AND, POSSIBLY,  QZVEC.
C
C     ON INPUT
C
C        NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL
C          ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM
C          DIMENSION STATEMENT.
C
C        N IS THE ORDER OF THE MATRICES.
C
C        A CONTAINS A REAL GENERAL MATRIX.
C
C        B CONTAINS A REAL GENERAL MATRIX.
C
C        MATZ SHOULD BE SET TO .TRUE. IF THE RIGHT HAND TRANSFORMATIONS
C          ARE TO BE ACCUMULATED FOR LATER USE IN COMPUTING
C          EIGENVECTORS, AND TO .FALSE. OTHERWISE.
C
C     ON OUTPUT
C
C        A HAS BEEN REDUCED TO UPPER HESSENBERG FORM.  THE ELEMENTS
C          BELOW THE FIRST SUBDIAGONAL HAVE BEEN SET TO ZERO.
C
C        B HAS BEEN REDUCED TO UPPER TRIANGULAR FORM.  THE ELEMENTS
C          BELOW THE MAIN DIAGONAL HAVE BEEN SET TO ZERO.
C
C        Z CONTAINS THE PRODUCT OF THE RIGHT HAND TRANSFORMATIONS IF
C          MATZ HAS BEEN SET TO .TRUE.  OTHERWISE, Z IS NOT REFERENCED.
C
C     QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO BURTON S. GARBOW,
C     MATHEMATICS AND COMPUTER SCIENCE DIV, ARGONNE NATIONAL LABORATORY
C
C     THIS VERSION DATED AUGUST 1983.
C
C     ------------------------------------------------------------------
C
C     .......... INITIALIZE Z ..........
      IF (.NOT. MATZ) GO TO 10
C
      DO 3 J = 1, N
C
         DO 2 I = 1, N
            Z(I,J) = 0.0D0
    2    CONTINUE
C
         Z(J,J) = 1.0D0
    3 CONTINUE
C     .......... REDUCE B TO UPPER TRIANGULAR FORM ..........
   10 IF (N .LE. 1) GO TO 170
      NM1 = N - 1
C
      DO 100 L = 1, NM1
         L1 = L + 1
         S = 0.0D0
C
         DO 20 I = L1, N
            S = S + DABS(B(I,L))
   20    CONTINUE
C
         IF (S .EQ. 0.0D0) GO TO 100
         S = S + DABS(B(L,L))
         R = 0.0D0
C
         DO 25 I = L, N
            B(I,L) = B(I,L) / S
            R = R + B(I,L)**2
   25    CONTINUE
C
         R = DSIGN(DSQRT(R),B(L,L))
         B(L,L) = B(L,L) + R
         RHO = R * B(L,L)
C
         DO 50 J = L1, N
            T = 0.0D0
C
            DO 30 I = L, N
               T = T + B(I,L) * B(I,J)
   30       CONTINUE
C
            T = -T / RHO
C
            DO 40 I = L, N
               B(I,J) = B(I,J) + T * B(I,L)
   40       CONTINUE
C
   50    CONTINUE
C
         DO 80 J = 1, N
            T = 0.0D0
C
            DO 60 I = L, N
               T = T + B(I,L) * A(I,J)
   60       CONTINUE
C
            T = -T / RHO
C
            DO 70 I = L, N
               A(I,J) = A(I,J) + T * B(I,L)
   70       CONTINUE
C
   80    CONTINUE
C
         B(L,L) = -S * R
C
         DO 90 I = L1, N
            B(I,L) = 0.0D0
   90    CONTINUE
C
  100 CONTINUE
*
*     ---------------------- BEGIN TIMING CODE -------------------------
      OPS = OPS + DBLE( 8*N**2 + 17*N + 24 )*DBLE( N-1 ) / 3.0D0
*     ----------------------- END TIMING CODE --------------------------
*
C     .......... REDUCE A TO UPPER HESSENBERG FORM, WHILE
C                KEEPING B TRIANGULAR ..........
      IF (N .EQ. 2) GO TO 170
      NM2 = N - 2
C
      DO 160 K = 1, NM2
         NK1 = NM1 - K
C     .......... FOR L=N-1 STEP -1 UNTIL K+1 DO -- ..........
         DO 150 LB = 1, NK1
            L = N - LB
            L1 = L + 1
C     .......... ZERO A(L+1,K) ..........
            S = DABS(A(L,K)) + DABS(A(L1,K))
            IF (S .EQ. 0.0D0) GO TO 150
            U1 = A(L,K) / S
            U2 = A(L1,K) / S
            R = DSIGN(DSQRT(U1*U1+U2*U2),U1)
            V1 =  -(U1 + R) / R
            V2 = -U2 / R
            U2 = V2 / V1
C
            DO 110 J = K, N
               T = A(L,J) + U2 * A(L1,J)
               A(L,J) = A(L,J) + T * V1
               A(L1,J) = A(L1,J) + T * V2
  110       CONTINUE
C
            A(L1,K) = 0.0D0
C
            DO 120 J = L, N
               T = B(L,J) + U2 * B(L1,J)
               B(L,J) = B(L,J) + T * V1
               B(L1,J) = B(L1,J) + T * V2
  120       CONTINUE
C     .......... ZERO B(L+1,L) ..........
            S = DABS(B(L1,L1)) + DABS(B(L1,L))
            IF (S .EQ. 0.0D0) GO TO 150
            U1 = B(L1,L1) / S
            U2 = B(L1,L) / S
            R = DSIGN(DSQRT(U1*U1+U2*U2),U1)
            V1 =  -(U1 + R) / R
            V2 = -U2 / R
            U2 = V2 / V1
C
            DO 130 I = 1, L1
               T = B(I,L1) + U2 * B(I,L)
               B(I,L1) = B(I,L1) + T * V1
               B(I,L) = B(I,L) + T * V2
  130       CONTINUE
C
            B(L1,L) = 0.0D0
C
            DO 140 I = 1, N
               T = A(I,L1) + U2 * A(I,L)
               A(I,L1) = A(I,L1) + T * V1
               A(I,L) = A(I,L) + T * V2
  140       CONTINUE
C
            IF (.NOT. MATZ) GO TO 150
C
            DO 145 I = 1, N
               T = Z(I,L1) + U2 * Z(I,L)
               Z(I,L1) = Z(I,L1) + T * V1
               Z(I,L) = Z(I,L) + T * V2
  145       CONTINUE
C
  150    CONTINUE
C
  160 CONTINUE
C
*
*     ---------------------- BEGIN TIMING CODE -------------------------
      IF( MATZ ) THEN
         OPS = OPS + DBLE( 11*N + 20 )*DBLE( N-1 )*DBLE( N-2 )
      ELSE
         OPS = OPS + DBLE( 8*N + 20 )*DBLE( N-1 )*DBLE( N-2 )
      END IF
*     ----------------------- END TIMING CODE --------------------------
*
  170 RETURN
      END
      SUBROUTINE QZIT(NM,N,A,B,EPS1,MATZ,Z,IERR)
C
      INTEGER I,J,K,L,N,EN,K1,K2,LD,LL,L1,NA,NM,ISH,ITN,ITS,KM1,LM1,
     X        ENM2,IERR,LOR1,ENORN
      DOUBLE PRECISION A(NM,N),B(NM,N),Z(NM,N)
      DOUBLE PRECISION R,S,T,A1,A2,A3,EP,SH,U1,U2,U3,V1,V2,V3,ANI,A11,
     X       A12,A21,A22,A33,A34,A43,A44,BNI,B11,B12,B22,B33,B34,
     X       B44,EPSA,EPSB,EPS1,ANORM,BNORM,EPSLON
      LOGICAL MATZ,NOTLAS
      external epslon
*
*     ---------------------- BEGIN TIMING CODE -------------------------
*     COMMON BLOCK TO RETURN OPERATION COUNT AND ITERATION COUNT
*     ITCNT IS INITIALIZED TO 0, OPS IS ONLY INCREMENTED
*     OPST IS USED TO ACCUMULATE SMALL CONTRIBUTIONS TO OPS
*     TO AVOID ROUNDOFF ERROR
*     .. COMMON BLOCKS ..
      COMMON             / LATIME / OPS, ITCNT
*     ..
*     .. SCALARS IN COMMON ..
      DOUBLE PRECISION   ITCNT, OPS
*     ..
      DOUBLE PRECISION   OPST
*     ----------------------- END TIMING CODE --------------------------
*
C
C     THIS SUBROUTINE IS THE SECOND STEP OF THE QZ ALGORITHM
C     FOR SOLVING GENERALIZED MATRIX EIGENVALUE PROBLEMS,
C     SIAM J. NUMER. ANAL. 10, 241-256(1973) BY MOLER AND STEWART,
C     AS MODIFIED IN TECHNICAL NOTE NASA TN D-7305(1973) BY WARD.
C
C     THIS SUBROUTINE ACCEPTS A PAIR OF REAL MATRICES, ONE OF THEM
C     IN UPPER HESSENBERG FORM AND THE OTHER IN UPPER TRIANGULAR FORM.
C     IT REDUCES THE HESSENBERG MATRIX TO QUASI-TRIANGULAR FORM USING
C     ORTHOGONAL TRANSFORMATIONS WHILE MAINTAINING THE TRIANGULAR FORM
C     OF THE OTHER MATRIX.  IT IS USUALLY PRECEDED BY  QZHES  AND
C     FOLLOWED BY  QZVAL  AND, POSSIBLY,  QZVEC.
C
C     ON INPUT
C
C        NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL
C          ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM
C          DIMENSION STATEMENT.
C
C        N IS THE ORDER OF THE MATRICES.
C
C        A CONTAINS A REAL UPPER HESSENBERG MATRIX.
C
C        B CONTAINS A REAL UPPER TRIANGULAR MATRIX.
C
C        EPS1 IS A TOLERANCE USED TO DETERMINE NEGLIGIBLE ELEMENTS.
C          EPS1 = 0.0 (OR NEGATIVE) MAY BE INPUT, IN WHICH CASE AN
C          ELEMENT WILL BE NEGLECTED ONLY IF IT IS LESS THAN ROUNDOFF
C          ERROR TIMES THE NORM OF ITS MATRIX.  IF THE INPUT EPS1 IS
C          POSITIVE, THEN AN ELEMENT WILL BE CONSIDERED NEGLIGIBLE
C          IF IT IS LESS THAN EPS1 TIMES THE NORM OF ITS MATRIX.  A
C          POSITIVE VALUE OF EPS1 MAY RESULT IN FASTER EXECUTION,
C          BUT LESS ACCURATE RESULTS.
C
C        MATZ SHOULD BE SET TO .TRUE. IF THE RIGHT HAND TRANSFORMATIONS
C          ARE TO BE ACCUMULATED FOR LATER USE IN COMPUTING
C          EIGENVECTORS, AND TO .FALSE. OTHERWISE.
C
C        Z CONTAINS, IF MATZ HAS BEEN SET TO .TRUE., THE
C          TRANSFORMATION MATRIX PRODUCED IN THE REDUCTION
C          BY  QZHES, IF PERFORMED, OR ELSE THE IDENTITY MATRIX.
C          IF MATZ HAS BEEN SET TO .FALSE., Z IS NOT REFERENCED.
C
C     ON OUTPUT
C
C        A HAS BEEN REDUCED TO QUASI-TRIANGULAR FORM.  THE ELEMENTS
C          BELOW THE FIRST SUBDIAGONAL ARE STILL ZERO AND NO TWO
C          CONSECUTIVE SUBDIAGONAL ELEMENTS ARE NONZERO.
C
C        B IS STILL IN UPPER TRIANGULAR FORM, ALTHOUGH ITS ELEMENTS
C          HAVE BEEN ALTERED.  THE LOCATION B(N,1) IS USED TO STORE
C          EPS1 TIMES THE NORM OF B FOR LATER USE BY  QZVAL  AND  QZVEC.
C
C        Z CONTAINS THE PRODUCT OF THE RIGHT HAND TRANSFORMATIONS
C          (FOR BOTH STEPS) IF MATZ HAS BEEN SET TO .TRUE..
C
C        IERR IS SET TO
C          ZERO       FOR NORMAL RETURN,
C          J          IF THE LIMIT OF 30*N ITERATIONS IS EXHAUSTED
C                     WHILE THE J-TH EIGENVALUE IS BEING SOUGHT.
C
C     QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO BURTON S. GARBOW,
C     MATHEMATICS AND COMPUTER SCIENCE DIV, ARGONNE NATIONAL LABORATORY
C
C     THIS VERSION DATED AUGUST 1983.
C
C     ------------------------------------------------------------------
C
      IERR = 0
C     .......... COMPUTE EPSA,EPSB ..........
      ANORM = 0.0D0
      BNORM = 0.0D0
C
      DO 30 I = 1, N
         ANI = 0.0D0
         IF (I .NE. 1) ANI = DABS(A(I,I-1))
         BNI = 0.0D0
C
         DO 20 J = I, N
            ANI = ANI + DABS(A(I,J))
            BNI = BNI + DABS(B(I,J))
   20    CONTINUE
C
         IF (ANI .GT. ANORM) ANORM = ANI
         IF (BNI .GT. BNORM) BNORM = BNI
   30 CONTINUE
*
*     ---------------------- BEGIN TIMING CODE -------------------------
      OPS = OPS + DBLE( N*( N+1 ) )
      OPST = 0.0D0
      ITCNT = 0
*     ----------------------- END TIMING CODE --------------------------
*
C
      IF (ANORM .EQ. 0.0D0) ANORM = 1.0D0
      IF (BNORM .EQ. 0.0D0) BNORM = 1.0D0
      EP = EPS1
      IF (EP .GT. 0.0D0) GO TO 50
C     .......... USE ROUNDOFF LEVEL IF EPS1 IS ZERO ..........
      EP = EPSLON(1.0D0)
   50 EPSA = EP * ANORM
      EPSB = EP * BNORM
C     .......... REDUCE A TO QUASI-TRIANGULAR FORM, WHILE
C                KEEPING B TRIANGULAR ..........
      LOR1 = 1
      ENORN = N
      EN = N
      ITN = 30*N
C     .......... BEGIN QZ STEP ..........
   60 IF (EN .LE. 2) GO TO 1001
      IF (.NOT. MATZ) ENORN = EN
      ITS = 0
      NA = EN - 1
      ENM2 = NA - 1
   70 ISH = 2
*
*     ---------------------- BEGIN TIMING CODE -------------------------
      OPS = OPS + OPST
      OPST = 0.0D0
      ITCNT = ITCNT + 1
*     ----------------------- END TIMING CODE --------------------------
*
C     .......... CHECK FOR CONVERGENCE OR REDUCIBILITY.
C                FOR L=EN STEP -1 UNTIL 1 DO -- ..........
      DO 80 LL = 1, EN
         LM1 = EN - LL
         L = LM1 + 1
         IF (L .EQ. 1) GO TO 95
         IF (DABS(A(L,LM1)) .LE. EPSA) GO TO 90
   80 CONTINUE
C
   90 A(L,LM1) = 0.0D0
      IF (L .LT. NA) GO TO 95
C     .......... 1-BY-1 OR 2-BY-2 BLOCK ISOLATED ..........
      EN = LM1
      GO TO 60
C     .......... CHECK FOR SMALL TOP OF B ..........
   95 LD = L
  100 L1 = L + 1
      B11 = B(L,L)
      IF (DABS(B11) .GT. EPSB) GO TO 120
      B(L,L) = 0.0D0
      S = DABS(A(L,L)) + DABS(A(L1,L))
      U1 = A(L,L) / S
      U2 = A(L1,L) / S
      R = DSIGN(DSQRT(U1*U1+U2*U2),U1)
      V1 = -(U1 + R) / R
      V2 = -U2 / R
      U2 = V2 / V1
C
      DO 110 J = L, ENORN
         T = A(L,J) + U2 * A(L1,J)
         A(L,J) = A(L,J) + T * V1
         A(L1,J) = A(L1,J) + T * V2
         T = B(L,J) + U2 * B(L1,J)
         B(L,J) = B(L,J) + T * V1
         B(L1,J) = B(L1,J) + T * V2
  110 CONTINUE
C
*     ---------------------- BEGIN TIMING CODE -------------------------
      OPST = OPST + DBLE( 12*( ENORN+1-L ) + 11 )
*     ----------------------- END TIMING CODE --------------------------
      IF (L .NE. 1) A(L,LM1) = -A(L,LM1)
      LM1 = L
      L = L1
      GO TO 90
  120 A11 = A(L,L) / B11
      A21 = A(L1,L) / B11
      IF (ISH .EQ. 1) GO TO 140
C     .......... ITERATION STRATEGY ..........
      IF (ITN .EQ. 0) GO TO 1000
      IF (ITS .EQ. 10) GO TO 155
C     .......... DETERMINE TYPE OF SHIFT ..........
      B22 = B(L1,L1)
      IF (DABS(B22) .LT. EPSB) B22 = EPSB
      B33 = B(NA,NA)
      IF (DABS(B33) .LT. EPSB) B33 = EPSB
      B44 = B(EN,EN)
      IF (DABS(B44) .LT. EPSB) B44 = EPSB
      A33 = A(NA,NA) / B33
      A34 = A(NA,EN) / B44
      A43 = A(EN,NA) / B33
      A44 = A(EN,EN) / B44
      B34 = B(NA,EN) / B44
      T = 0.5D0 * (A43 * B34 - A33 - A44)
      R = T * T + A34 * A43 - A33 * A44
*     ---------------------- BEGIN TIMING CODE -------------------------
      OPST = OPST + DBLE( 16 )
*     ----------------------- END TIMING CODE --------------------------
      IF (R .LT. 0.0D0) GO TO 150
C     .......... DETERMINE SINGLE SHIFT ZEROTH COLUMN OF A ..........
      ISH = 1
      R = DSQRT(R)
      SH = -T + R
      S = -T - R
      IF (DABS(S-A44) .LT. DABS(SH-A44)) SH = S
C     .......... LOOK FOR TWO CONSECUTIVE SMALL
C                SUB-DIAGONAL ELEMENTS OF A.
C                FOR L=EN-2 STEP -1 UNTIL LD DO -- ..........
      DO 130 LL = LD, ENM2
         L = ENM2 + LD - LL
         IF (L .EQ. LD) GO TO 140
         LM1 = L - 1
         L1 = L + 1
         T = A(L,L)
         IF (DABS(B(L,L)) .GT. EPSB) T = T - SH * B(L,L)
*        --------------------- BEGIN TIMING CODE -----------------------
         IF (DABS(A(L,LM1)) .LE. DABS(T/A(L1,L)) * EPSA) THEN
            OPST = OPST + DBLE( 5 + 4*( LL+1-LD ) )
            GO TO 100
         END IF
*        ---------------------- END TIMING CODE ------------------------
  130 CONTINUE
*     ---------------------- BEGIN TIMING CODE -------------------------
      OPST = OPST + DBLE( 5 + 4*( ENM2+1-LD ) )
*     ----------------------- END TIMING CODE --------------------------
C
  140 A1 = A11 - SH
      A2 = A21
      IF (L .NE. LD) A(L,LM1) = -A(L,LM1)
      GO TO 160
C     .......... DETERMINE DOUBLE SHIFT ZEROTH COLUMN OF A ..........
  150 A12 = A(L,L1) / B22
      A22 = A(L1,L1) / B22
      B12 = B(L,L1) / B22
      A1 = ((A33 - A11) * (A44 - A11) - A34 * A43 + A43 * B34 * A11)
     X     / A21 + A12 - A11 * B12
      A2 = (A22 - A11) - A21 * B12 - (A33 - A11) - (A44 - A11)
     X     + A43 * B34
      A3 = A(L1+1,L1) / B22
*     ---------------------- BEGIN TIMING CODE -------------------------
      OPST = OPST + DBLE( 25 )
*     ----------------------- END TIMING CODE --------------------------
      GO TO 160
C     .......... AD HOC SHIFT ..........
  155 A1 = 0.0D0
      A2 = 1.0D0
      A3 = 1.1605D0
  160 ITS = ITS + 1
      ITN = ITN - 1
      IF (.NOT. MATZ) LOR1 = LD
C     .......... MAIN LOOP ..........
      DO 260 K = L, NA
         NOTLAS = K .NE. NA .AND. ISH .EQ. 2
         K1 = K + 1
         K2 = K + 2
         KM1 = MAX0(K-1,L)
         LL = MIN0(EN,K1+ISH)
         IF (NOTLAS) GO TO 190
C     .......... ZERO A(K+1,K-1) ..........
         IF (K .EQ. L) GO TO 170
         A1 = A(K,KM1)
         A2 = A(K1,KM1)
  170    S = DABS(A1) + DABS(A2)
         IF (S .EQ. 0.0D0) GO TO 70
         U1 = A1 / S
         U2 = A2 / S
         R = DSIGN(DSQRT(U1*U1+U2*U2),U1)
         V1 = -(U1 + R) / R
         V2 = -U2 / R
         U2 = V2 / V1
C
         DO 180 J = KM1, ENORN
            T = A(K,J) + U2 * A(K1,J)
            A(K,J) = A(K,J) + T * V1
            A(K1,J) = A(K1,J) + T * V2
            T = B(K,J) + U2 * B(K1,J)
            B(K,J) = B(K,J) + T * V1
            B(K1,J) = B(K1,J) + T * V2
  180    CONTINUE
C
*        --------------------- BEGIN TIMING CODE -----------------------
         OPST = OPST + DBLE( 11 + 12*( ENORN+1-KM1 ) )
*        ---------------------- END TIMING CODE ------------------------
         IF (K .NE. L) A(K1,KM1) = 0.0D0
         GO TO 240
C     .......... ZERO A(K+1,K-1) AND A(K+2,K-1) ..........
  190    IF (K .EQ. L) GO TO 200
         A1 = A(K,KM1)
         A2 = A(K1,KM1)
         A3 = A(K2,KM1)
  200    S = DABS(A1) + DABS(A2) + DABS(A3)
         IF (S .EQ. 0.0D0) GO TO 260
         U1 = A1 / S
         U2 = A2 / S
         U3 = A3 / S
         R = DSIGN(DSQRT(U1*U1+U2*U2+U3*U3),U1)
         V1 = -(U1 + R) / R
         V2 = -U2 / R
         V3 = -U3 / R
         U2 = V2 / V1
         U3 = V3 / V1
C
         DO 210 J = KM1, ENORN
            T = A(K,J) + U2 * A(K1,J) + U3 * A(K2,J)
            A(K,J) = A(K,J) + T * V1
            A(K1,J) = A(K1,J) + T * V2
            A(K2,J) = A(K2,J) + T * V3
            T = B(K,J) + U2 * B(K1,J) + U3 * B(K2,J)
            B(K,J) = B(K,J) + T * V1
            B(K1,J) = B(K1,J) + T * V2
            B(K2,J) = B(K2,J) + T * V3
  210    CONTINUE
*        --------------------- BEGIN TIMING CODE -----------------------
         OPST = OPST + DBLE( 17 + 20*( ENORN+1-KM1 ) )
*        ---------------------- END TIMING CODE ------------------------
C
         IF (K .EQ. L) GO TO 220
         A(K1,KM1) = 0.0D0
         A(K2,KM1) = 0.0D0
C     .......... ZERO B(K+2,K+1) AND B(K+2,K) ..........
  220    S = DABS(B(K2,K2)) + DABS(B(K2,K1)) + DABS(B(K2,K))
         IF (S .EQ. 0.0D0) GO TO 240
         U1 = B(K2,K2) / S
         U2 = B(K2,K1) / S
         U3 = B(K2,K) / S
         R = DSIGN(DSQRT(U1*U1+U2*U2+U3*U3),U1)
         V1 = -(U1 + R) / R
         V2 = -U2 / R
         V3 = -U3 / R
         U2 = V2 / V1
         U3 = V3 / V1
C
         DO 230 I = LOR1, LL
            T = A(I,K2) + U2 * A(I,K1) + U3 * A(I,K)
            A(I,K2) = A(I,K2) + T * V1
            A(I,K1) = A(I,K1) + T * V2
            A(I,K) = A(I,K) + T * V3
            T = B(I,K2) + U2 * B(I,K1) + U3 * B(I,K)
            B(I,K2) = B(I,K2) + T * V1
            B(I,K1) = B(I,K1) + T * V2
            B(I,K) = B(I,K) + T * V3
  230    CONTINUE
*        --------------------- BEGIN TIMING CODE -----------------------
         OPST = OPST + DBLE( 17 + 20*( LL+1-LOR1 ) )
*        ---------------------- END TIMING CODE ------------------------
C
         B(K2,K) = 0.0D0
         B(K2,K1) = 0.0D0
         IF (.NOT. MATZ) GO TO 240
C
         DO 235 I = 1, N
            T = Z(I,K2) + U2 * Z(I,K1) + U3 * Z(I,K)
            Z(I,K2) = Z(I,K2) + T * V1
            Z(I,K1) = Z(I,K1) + T * V2
            Z(I,K) = Z(I,K) + T * V3
  235    CONTINUE
*        --------------------- BEGIN TIMING CODE -----------------------
         OPST = OPST + DBLE( 10*N )
*        ---------------------- END TIMING CODE ------------------------
C     .......... ZERO B(K+1,K) ..........
  240    S = DABS(B(K1,K1)) + DABS(B(K1,K))
         IF (S .EQ. 0.0D0) GO TO 260
         U1 = B(K1,K1) / S
         U2 = B(K1,K) / S
         R = DSIGN(DSQRT(U1*U1+U2*U2),U1)
         V1 = -(U1 + R) / R
         V2 = -U2 / R
         U2 = V2 / V1
C
         DO 250 I = LOR1, LL
            T = A(I,K1) + U2 * A(I,K)
            A(I,K1) = A(I,K1) + T * V1
            A(I,K) = A(I,K) + T * V2
            T = B(I,K1) + U2 * B(I,K)
            B(I,K1) = B(I,K1) + T * V1
            B(I,K) = B(I,K) + T * V2
  250    CONTINUE
*        --------------------- BEGIN TIMING CODE -----------------------
         OPST = OPST + DBLE( 11 + 12*( LL+1-LOR1 ) )
*        ---------------------- END TIMING CODE ------------------------
C
         B(K1,K) = 0.0D0
         IF (.NOT. MATZ) GO TO 260
C
         DO 255 I = 1, N
            T = Z(I,K1) + U2 * Z(I,K)
            Z(I,K1) = Z(I,K1) + T * V1
            Z(I,K) = Z(I,K) + T * V2
  255    CONTINUE
*        --------------------- BEGIN TIMING CODE -----------------------
         OPST = OPST + DBLE( 6*N )
*        ---------------------- END TIMING CODE ------------------------
C
  260 CONTINUE
C     .......... END QZ STEP ..........
      GO TO 70
C     .......... SET ERROR -- ALL EIGENVALUES HAVE NOT
C                CONVERGED AFTER 30*N ITERATIONS ..........
 1000 IERR = EN
C     .......... SAVE EPSB FOR USE BY QZVAL AND QZVEC ..........
 1001 IF (N .GT. 1) B(N,1) = EPSB
*
*     ---------------------- BEGIN TIMING CODE -------------------------
      OPS = OPS + OPST
      OPST = 0.0D0
*     ----------------------- END TIMING CODE --------------------------
*
      RETURN
      END
      SUBROUTINE QZVAL(NM,N,A,B,ALFR,ALFI,BETA,MATZ,Z)
C
      INTEGER I,J,N,EN,NA,NM,NN,ISW
      DOUBLE PRECISION A(NM,N),B(NM,N),ALFR(N),ALFI(N),BETA(N),Z(NM,N)
      DOUBLE PRECISION C,D,E,R,S,T,AN,A1,A2,BN,CQ,CZ,DI,DR,EI,TI,TR,U1,
     X       U2,V1,V2,A1I,A11,A12,A2I,A21,A22,B11,B12,B22,SQI,SQR,
     X       SSI,SSR,SZI,SZR,A11I,A11R,A12I,A12R,A22I,A22R,EPSB
      LOGICAL MATZ
*
*     ---------------------- BEGIN TIMING CODE -------------------------
*     COMMON BLOCK TO RETURN OPERATION COUNT AND ITERATION COUNT
*     ITCNT IS INITIALIZED TO 0, OPS IS ONLY INCREMENTED
*     OPST IS USED TO ACCUMULATE SMALL CONTRIBUTIONS TO OPS
*     TO AVOID ROUNDOFF ERROR
*     .. COMMON BLOCKS ..
      COMMON             / LATIME / OPS, ITCNT
*     ..
*     .. SCALARS IN COMMON ..
      DOUBLE PRECISION   ITCNT, OPS
*     ..
      DOUBLE PRECISION   OPST, OPST2
*     ----------------------- END TIMING CODE --------------------------
*
C
C     THIS SUBROUTINE IS THE THIRD STEP OF THE QZ ALGORITHM
C     FOR SOLVING GENERALIZED MATRIX EIGENVALUE PROBLEMS,
C     SIAM J. NUMER. ANAL. 10, 241-256(1973) BY MOLER AND STEWART.
C
C     THIS SUBROUTINE ACCEPTS A PAIR OF REAL MATRICES, ONE OF THEM
C     IN QUASI-TRIANGULAR FORM AND THE OTHER IN UPPER TRIANGULAR FORM.
C     IT REDUCES THE QUASI-TRIANGULAR MATRIX FURTHER, SO THAT ANY
C     REMAINING 2-BY-2 BLOCKS CORRESPOND TO PAIRS OF COMPLEX
C     EIGENVALUES, AND RETURNS QUANTITIES WHOSE RATIOS GIVE THE
C     GENERALIZED EIGENVALUES.  IT IS USUALLY PRECEDED BY  QZHES
C     AND  QZIT  AND MAY BE FOLLOWED BY  QZVEC.
C
C     ON INPUT
C
C        NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL
C          ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM
C          DIMENSION STATEMENT.
C
C        N IS THE ORDER OF THE MATRICES.
C
C        A CONTAINS A REAL UPPER QUASI-TRIANGULAR MATRIX.
C
C        B CONTAINS A REAL UPPER TRIANGULAR MATRIX.  IN ADDITION,
C          LOCATION B(N,1) CONTAINS THE TOLERANCE QUANTITY (EPSB)
C          COMPUTED AND SAVED IN  QZIT.
C
C        MATZ SHOULD BE SET TO .TRUE. IF THE RIGHT HAND TRANSFORMATIONS
C          ARE TO BE ACCUMULATED FOR LATER USE IN COMPUTING
C          EIGENVECTORS, AND TO .FALSE. OTHERWISE.
C
C        Z CONTAINS, IF MATZ HAS BEEN SET TO .TRUE., THE
C          TRANSFORMATION MATRIX PRODUCED IN THE REDUCTIONS BY QZHES
C          AND QZIT, IF PERFORMED, OR ELSE THE IDENTITY MATRIX.
C          IF MATZ HAS BEEN SET TO .FALSE., Z IS NOT REFERENCED.
C
C     ON OUTPUT
C
C        A HAS BEEN REDUCED FURTHER TO A QUASI-TRIANGULAR MATRIX
C          IN WHICH ALL NONZERO SUBDIAGONAL ELEMENTS CORRESPOND TO
C          PAIRS OF COMPLEX EIGENVALUES.
C
C        B IS STILL IN UPPER TRIANGULAR FORM, ALTHOUGH ITS ELEMENTS
C          HAVE BEEN ALTERED.  B(N,1) IS UNALTERED.
C
C        ALFR AND ALFI CONTAIN THE REAL AND IMAGINARY PARTS OF THE
C          DIAGONAL ELEMENTS OF THE TRIANGULAR MATRIX THAT WOULD BE
C          OBTAINED IF A WERE REDUCED COMPLETELY TO TRIANGULAR FORM
C          BY UNITARY TRANSFORMATIONS.  NON-ZERO VALUES OF ALFI OCCUR
C          IN PAIRS, THE FIRST MEMBER POSITIVE AND THE SECOND NEGATIVE.
C
C        BETA CONTAINS THE DIAGONAL ELEMENTS OF THE CORRESPONDING B,
C          NORMALIZED TO BE REAL AND NON-NEGATIVE.  THE GENERALIZED
C          EIGENVALUES ARE THEN THE RATIOS ((ALFR+I*ALFI)/BETA).
C
C        Z CONTAINS THE PRODUCT OF THE RIGHT HAND TRANSFORMATIONS
C          (FOR ALL THREE STEPS) IF MATZ HAS BEEN SET TO .TRUE.
C
C     QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO BURTON S. GARBOW,
C     MATHEMATICS AND COMPUTER SCIENCE DIV, ARGONNE NATIONAL LABORATORY
C
C     THIS VERSION DATED AUGUST 1983.
C
C     ------------------------------------------------------------------
C
      EPSB = B(N,1)
      ISW = 1
C     .......... FIND EIGENVALUES OF QUASI-TRIANGULAR MATRICES.
C                FOR EN=N STEP -1 UNTIL 1 DO -- ..........
*
*     ---------------------- BEGIN TIMING CODE -------------------------
      OPST = 0.0D0
      OPST2 = 0.0D0
*     ----------------------- END TIMING CODE --------------------------
*
      DO 510 NN = 1, N
*
*        --------------------- BEGIN TIMING CODE -----------------------
         OPST = OPST + OPST2
         OPST2 = 0.0D0
*        ---------------------- END TIMING CODE ------------------------
*
         EN = N + 1 - NN
         NA = EN - 1
         IF (ISW .EQ. 2) GO TO 505
         IF (EN .EQ. 1) GO TO 410
         IF (A(EN,NA) .NE. 0.0D0) GO TO 420
C     .......... 1-BY-1 BLOCK, ONE REAL ROOT ..........
  410    ALFR(EN) = A(EN,EN)
         IF (B(EN,EN) .LT. 0.0D0) ALFR(EN) = -ALFR(EN)
         BETA(EN) = DABS(B(EN,EN))
         ALFI(EN) = 0.0D0
         GO TO 510
C     .......... 2-BY-2 BLOCK ..........
  420    IF (DABS(B(NA,NA)) .LE. EPSB) GO TO 455
         IF (DABS(B(EN,EN)) .GT. EPSB) GO TO 430
         A1 = A(EN,EN)
         A2 = A(EN,NA)
         BN = 0.0D0
         GO TO 435
  430    AN = DABS(A(NA,NA)) + DABS(A(NA,EN)) + DABS(A(EN,NA))
     X      + DABS(A(EN,EN))
         BN = DABS(B(NA,NA)) + DABS(B(NA,EN)) + DABS(B(EN,EN))
         A11 = A(NA,NA) / AN
         A12 = A(NA,EN) / AN
         A21 = A(EN,NA) / AN
         A22 = A(EN,EN) / AN
         B11 = B(NA,NA) / BN
         B12 = B(NA,EN) / BN
         B22 = B(EN,EN) / BN
         E = A11 / B11
         EI = A22 / B22
         S = A21 / (B11 * B22)
         T = (A22 - E * B22) / B22
         IF (DABS(E) .LE. DABS(EI)) GO TO 431
         E = EI
         T = (A11 - E * B11) / B11
  431    C = 0.5D0 * (T - S * B12)
         D = C * C + S * (A12 - E * B12)
*        --------------------- BEGIN TIMING CODE -----------------------
         OPST2 = OPST2 + DBLE( 28 )
*        ---------------------- END TIMING CODE ------------------------
         IF (D .LT. 0.0D0) GO TO 480
C     .......... TWO REAL ROOTS.
C                ZERO BOTH A(EN,NA) AND B(EN,NA) ..........
         E = E + (C + DSIGN(DSQRT(D),C))
         A11 = A11 - E * B11
         A12 = A12 - E * B12
         A22 = A22 - E * B22
*        --------------------- BEGIN TIMING CODE -----------------------
         OPST2 = OPST2 + DBLE( 11 )
*        ---------------------- END TIMING CODE ------------------------
         IF (DABS(A11) + DABS(A12) .LT.
     X       DABS(A21) + DABS(A22)) GO TO 432
         A1 = A12
         A2 = A11
         GO TO 435
  432    A1 = A22
         A2 = A21
C     .......... CHOOSE AND APPLY REAL Z ..........
  435    S = DABS(A1) + DABS(A2)
         U1 = A1 / S
         U2 = A2 / S
         R = DSIGN(DSQRT(U1*U1+U2*U2),U1)
         V1 = -(U1 + R) / R
         V2 = -U2 / R
         U2 = V2 / V1
C
         DO 440 I = 1, EN
            T = A(I,EN) + U2 * A(I,NA)
            A(I,EN) = A(I,EN) + T * V1
            A(I,NA) = A(I,NA) + T * V2
            T = B(I,EN) + U2 * B(I,NA)
            B(I,EN) = B(I,EN) + T * V1
            B(I,NA) = B(I,NA) + T * V2
  440    CONTINUE
*        --------------------- BEGIN TIMING CODE -----------------------
         OPST2 = OPST2 + DBLE( 11 + 12*EN )
*        ---------------------- END TIMING CODE ------------------------
C
         IF (.NOT. MATZ) GO TO 450
C
         DO 445 I = 1, N
            T = Z(I,EN) + U2 * Z(I,NA)
            Z(I,EN) = Z(I,EN) + T * V1
            Z(I,NA) = Z(I,NA) + T * V2
  445    CONTINUE
*        --------------------- BEGIN TIMING CODE -----------------------
         OPST2 = OPST2 + DBLE( 6*N )
*        ---------------------- END TIMING CODE ------------------------
C
  450    IF (BN .EQ. 0.0D0) GO TO 475
         IF (AN .LT. DABS(E) * BN) GO TO 455
         A1 = B(NA,NA)
         A2 = B(EN,NA)
         GO TO 460
  455    A1 = A(NA,NA)
         A2 = A(EN,NA)
C     .......... CHOOSE AND APPLY REAL Q ..........
  460    S = DABS(A1) + DABS(A2)
         IF (S .EQ. 0.0D0) GO TO 475
         U1 = A1 / S
         U2 = A2 / S
         R = DSIGN(DSQRT(U1*U1+U2*U2),U1)
         V1 = -(U1 + R) / R
         V2 = -U2 / R
         U2 = V2 / V1
C
         DO 470 J = NA, N
            T = A(NA,J) + U2 * A(EN,J)
            A(NA,J) = A(NA,J) + T * V1
            A(EN,J) = A(EN,J) + T * V2
            T = B(NA,J) + U2 * B(EN,J)
            B(NA,J) = B(NA,J) + T * V1
            B(EN,J) = B(EN,J) + T * V2
  470    CONTINUE
*        --------------------- BEGIN TIMING CODE -----------------------
         OPST2 = OPST2 + DBLE( 11 + 12*( N+1-NA ) )
*        ---------------------- END TIMING CODE ------------------------
C
  475    A(EN,NA) = 0.0D0
         B(EN,NA) = 0.0D0
         ALFR(NA) = A(NA,NA)
         ALFR(EN) = A(EN,EN)
         IF (B(NA,NA) .LT. 0.0D0) ALFR(NA) = -ALFR(NA)
         IF (B(EN,EN) .LT. 0.0D0) ALFR(EN) = -ALFR(EN)
         BETA(NA) = DABS(B(NA,NA))
         BETA(EN) = DABS(B(EN,EN))
         ALFI(EN) = 0.0D0
         ALFI(NA) = 0.0D0
         GO TO 505
C     .......... TWO COMPLEX ROOTS ..........
  480    E = E + C
         EI = DSQRT(-D)
         A11R = A11 - E * B11
         A11I = EI * B11
         A12R = A12 - E * B12
         A12I = EI * B12
         A22R = A22 - E * B22
         A22I = EI * B22
         IF (DABS(A11R) + DABS(A11I) + DABS(A12R) + DABS(A12I) .LT.
     X       DABS(A21) + DABS(A22R) + DABS(A22I)) GO TO 482
         A1 = A12R
         A1I = A12I
         A2 = -A11R
         A2I = -A11I
         GO TO 485
  482    A1 = A22R
         A1I = A22I
         A2 = -A21
         A2I = 0.0D0
C     .......... CHOOSE COMPLEX Z ..........
  485    CZ = DSQRT(A1*A1+A1I*A1I)
         IF (CZ .EQ. 0.0D0) GO TO 487
         SZR = (A1 * A2 + A1I * A2I) / CZ
         SZI = (A1 * A2I - A1I * A2) / CZ
         R = DSQRT(CZ*CZ+SZR*SZR+SZI*SZI)
         CZ = CZ / R
         SZR = SZR / R
         SZI = SZI / R
         GO TO 490
  487    SZR = 1.0D0
         SZI = 0.0D0
  490    IF (AN .LT. (DABS(E) + EI) * BN) GO TO 492
         A1 = CZ * B11 + SZR * B12
         A1I = SZI * B12
         A2 = SZR * B22
         A2I = SZI * B22
         GO TO 495
  492    A1 = CZ * A11 + SZR * A12
         A1I = SZI * A12
         A2 = CZ * A21 + SZR * A22
         A2I = SZI * A22
C     .......... CHOOSE COMPLEX Q ..........
  495    CQ = DSQRT(A1*A1+A1I*A1I)
         IF (CQ .EQ. 0.0D0) GO TO 497
         SQR = (A1 * A2 + A1I * A2I) / CQ
         SQI = (A1 * A2I - A1I * A2) / CQ
         R = DSQRT(CQ*CQ+SQR*SQR+SQI*SQI)
         CQ = CQ / R
         SQR = SQR / R
         SQI = SQI / R
         GO TO 500
  497    SQR = 1.0D0
         SQI = 0.0D0
C     .......... COMPUTE DIAGONAL ELEMENTS THAT WOULD RESULT
C                IF TRANSFORMATIONS WERE APPLIED ..........
  500    SSR = SQR * SZR + SQI * SZI
         SSI = SQR * SZI - SQI * SZR
         I = 1
         TR = CQ * CZ * A11 + CQ * SZR * A12 + SQR * CZ * A21
     X      + SSR * A22
         TI = CQ * SZI * A12 - SQI * CZ * A21 + SSI * A22
         DR = CQ * CZ * B11 + CQ * SZR * B12 + SSR * B22
         DI = CQ * SZI * B12 + SSI * B22
         GO TO 503
  502    I = 2
         TR = SSR * A11 - SQR * CZ * A12 - CQ * SZR * A21
     X      + CQ * CZ * A22
         TI = -SSI * A11 - SQI * CZ * A12 + CQ * SZI * A21
         DR = SSR * B11 - SQR * CZ * B12 + CQ * CZ * B22
         DI = -SSI * B11 - SQI * CZ * B12
  503    T = TI * DR - TR * DI
         J = NA
         IF (T .LT. 0.0D0) J = EN
         R = DSQRT(DR*DR+DI*DI)
         BETA(J) = BN * R
         ALFR(J) = AN * (TR * DR + TI * DI) / R
         ALFI(J) = AN * T / R
         IF (I .EQ. 1) GO TO 502
*        --------------------- BEGIN TIMING CODE -----------------------
         OPST2 = OPST2 + DBLE( 151 )
*        ---------------------- END TIMING CODE ------------------------
  505    ISW = 3 - ISW
  510 CONTINUE
*
*     ---------------------- BEGIN TIMING CODE -------------------------
      OPS = OPS + ( OPST + OPST2 )
*     ----------------------- END TIMING CODE --------------------------
*
      B(N,1) = EPSB
C
      RETURN
      END
      SUBROUTINE QZVEC(NM,N,A,B,ALFR,ALFI,BETA,Z)
C
      INTEGER I,J,K,M,N,EN,II,JJ,NA,NM,NN,ISW,ENM2
      DOUBLE PRECISION A(NM,N),B(NM,N),ALFR(N),ALFI(N),BETA(N),Z(NM,N)
      DOUBLE PRECISION D,Q,R,S,T,W,X,Y,DI,DR,RA,RR,SA,TI,TR,T1,T2,W1,X1,
     X       ZZ,Z1,ALFM,ALMI,ALMR,BETM,EPSB
*
*     ---------------------- BEGIN TIMING CODE -------------------------
*     COMMON BLOCK TO RETURN OPERATION COUNT AND ITERATION COUNT
*     ITCNT IS INITIALIZED TO 0, OPS IS ONLY INCREMENTED
*     OPST IS USED TO ACCUMULATE SMALL CONTRIBUTIONS TO OPS
*     TO AVOID ROUNDOFF ERROR
*     .. COMMON BLOCKS ..
      COMMON             / LATIME / OPS, ITCNT
*     ..
*     .. SCALARS IN COMMON ..
      DOUBLE PRECISION   ITCNT, OPS
*     ..
      INTEGER            IN2BY2
*     ----------------------- END TIMING CODE --------------------------
*
C
C     THIS SUBROUTINE IS THE OPTIONAL FOURTH STEP OF THE QZ ALGORITHM
C     FOR SOLVING GENERALIZED MATRIX EIGENVALUE PROBLEMS,
C     SIAM J. NUMER. ANAL. 10, 241-256(1973) BY MOLER AND STEWART.
C
C     THIS SUBROUTINE ACCEPTS A PAIR OF REAL MATRICES, ONE OF THEM IN
C     QUASI-TRIANGULAR FORM (IN WHICH EACH 2-BY-2 BLOCK CORRESPONDS TO
C     A PAIR OF COMPLEX EIGENVALUES) AND THE OTHER IN UPPER TRIANGULAR
C     FORM.  IT COMPUTES THE EIGENVECTORS OF THE TRIANGULAR PROBLEM AND
C     TRANSFORMS THE RESULTS BACK TO THE ORIGINAL COORDINATE SYSTEM.
C     IT IS USUALLY PRECEDED BY  QZHES,  QZIT, AND  QZVAL.
C
C     ON INPUT
C
C        NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL
C          ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM
C          DIMENSION STATEMENT.
C
C        N IS THE ORDER OF THE MATRICES.
C
C        A CONTAINS A REAL UPPER QUASI-TRIANGULAR MATRIX.
C
C        B CONTAINS A REAL UPPER TRIANGULAR MATRIX.  IN ADDITION,
C          LOCATION B(N,1) CONTAINS THE TOLERANCE QUANTITY (EPSB)
C          COMPUTED AND SAVED IN  QZIT.
C
C        ALFR, ALFI, AND BETA  ARE VECTORS WITH COMPONENTS WHOSE
C          RATIOS ((ALFR+I*ALFI)/BETA) ARE THE GENERALIZED
C          EIGENVALUES.  THEY ARE USUALLY OBTAINED FROM  QZVAL.
C
C        Z CONTAINS THE TRANSFORMATION MATRIX PRODUCED IN THE
C          REDUCTIONS BY  QZHES,  QZIT, AND  QZVAL, IF PERFORMED.
C          IF THE EIGENVECTORS OF THE TRIANGULAR PROBLEM ARE
C          DESIRED, Z MUST CONTAIN THE IDENTITY MATRIX.
C
C     ON OUTPUT
C
C        A IS UNALTERED.  ITS SUBDIAGONAL ELEMENTS PROVIDE INFORMATION
C           ABOUT THE STORAGE OF THE COMPLEX EIGENVECTORS.
C
C        B HAS BEEN DESTROYED.
C
C        ALFR, ALFI, AND BETA ARE UNALTERED.
C
C        Z CONTAINS THE REAL AND IMAGINARY PARTS OF THE EIGENVECTORS.
C          IF ALFI(I) .EQ. 0.0, THE I-TH EIGENVALUE IS REAL AND
C            THE I-TH COLUMN OF Z CONTAINS ITS EIGENVECTOR.
C          IF ALFI(I) .NE. 0.0, THE I-TH EIGENVALUE IS COMPLEX.
C            IF ALFI(I) .GT. 0.0, THE EIGENVALUE IS THE FIRST OF
C              A COMPLEX PAIR AND THE I-TH AND (I+1)-TH COLUMNS
C              OF Z CONTAIN ITS EIGENVECTOR.
C            IF ALFI(I) .LT. 0.0, THE EIGENVALUE IS THE SECOND OF
C              A COMPLEX PAIR AND THE (I-1)-TH AND I-TH COLUMNS
C              OF Z CONTAIN THE CONJUGATE OF ITS EIGENVECTOR.
C          EACH EIGENVECTOR IS NORMALIZED SO THAT THE MODULUS
C          OF ITS LARGEST COMPONENT IS 1.0 .
C
C     QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO BURTON S. GARBOW,
C     MATHEMATICS AND COMPUTER SCIENCE DIV, ARGONNE NATIONAL LABORATORY
C
C     THIS VERSION DATED AUGUST 1983.
C
C     ------------------------------------------------------------------
C
      EPSB = B(N,1)
      ISW = 1
C     .......... FOR EN=N STEP -1 UNTIL 1 DO -- ..........
      DO 800 NN = 1, N
*        --------------------- BEGIN TIMING CODE -----------------------
         IN2BY2 = 0
*        ---------------------- END TIMING CODE ------------------------
         EN = N + 1 - NN
         NA = EN - 1
         IF (ISW .EQ. 2) GO TO 795
         IF (ALFI(EN) .NE. 0.0D0) GO TO 710
C     .......... REAL VECTOR ..........
         M = EN
         B(EN,EN) = 1.0D0
         IF (NA .EQ. 0) GO TO 800
         ALFM = ALFR(M)
         BETM = BETA(M)
C     .......... FOR I=EN-1 STEP -1 UNTIL 1 DO -- ..........
         DO 700 II = 1, NA
            I = EN - II
            W = BETM * A(I,I) - ALFM * B(I,I)
            R = 0.0D0
C
            DO 610 J = M, EN
  610       R = R + (BETM * A(I,J) - ALFM * B(I,J)) * B(J,EN)
C
            IF (I .EQ. 1 .OR. ISW .EQ. 2) GO TO 630
            IF (BETM * A(I,I-1) .EQ. 0.0D0) GO TO 630
            ZZ = W
            S = R
            GO TO 690
  630       M = I
            IF (ISW .EQ. 2) GO TO 640
C     .......... REAL 1-BY-1 BLOCK ..........
            T = W
            IF (W .EQ. 0.0D0) T = EPSB
            B(I,EN) = -R / T
            GO TO 700
C     .......... REAL 2-BY-2 BLOCK ..........
  640       X = BETM * A(I,I+1) - ALFM * B(I,I+1)
            Y = BETM * A(I+1,I)
            Q = W * ZZ - X * Y
            T = (X * S - ZZ * R) / Q
            B(I,EN) = T
*           ------------------- BEGIN TIMING CODE ----------------------
            IN2BY2 = IN2BY2 + 1
*           -------------------- END TIMING CODE -----------------------
            IF (DABS(X) .LE. DABS(ZZ)) GO TO 650
            B(I+1,EN) = (-R - W * T) / X
            GO TO 690
  650       B(I+1,EN) = (-S - Y * T) / ZZ
  690       ISW = 3 - ISW
  700    CONTINUE
C     .......... END REAL VECTOR ..........
*        --------------------- BEGIN TIMING CODE -----------------------
         OPS = OPS + ( 5.0D0/2.0D0 )*DBLE( ( EN+2 )*( EN-1 ) + IN2BY2 )
*        ---------------------- END TIMING CODE ------------------------
         GO TO 800
C     .......... COMPLEX VECTOR ..........
  710    M = NA
         ALMR = ALFR(M)
         ALMI = ALFI(M)
         BETM = BETA(M)
C     .......... LAST VECTOR COMPONENT CHOSEN IMAGINARY SO THAT
C                EIGENVECTOR MATRIX IS TRIANGULAR ..........
         Y = BETM * A(EN,NA)
         B(NA,NA) = -ALMI * B(EN,EN) / Y
         B(NA,EN) = (ALMR * B(EN,EN) - BETM * A(EN,EN)) / Y
         B(EN,NA) = 0.0D0
         B(EN,EN) = 1.0D0
         ENM2 = NA - 1
         IF (ENM2 .EQ. 0) GO TO 795
C     .......... FOR I=EN-2 STEP -1 UNTIL 1 DO -- ..........
         DO 790 II = 1, ENM2
            I = NA - II
            W = BETM * A(I,I) - ALMR * B(I,I)
            W1 = -ALMI * B(I,I)
            RA = 0.0D0
            SA = 0.0D0
C
            DO 760 J = M, EN
               X = BETM * A(I,J) - ALMR * B(I,J)
               X1 = -ALMI * B(I,J)
               RA = RA + X * B(J,NA) - X1 * B(J,EN)
               SA = SA + X * B(J,EN) + X1 * B(J,NA)
  760       CONTINUE
C
            IF (I .EQ. 1 .OR. ISW .EQ. 2) GO TO 770
            IF (BETM * A(I,I-1) .EQ. 0.0D0) GO TO 770
            ZZ = W
            Z1 = W1
            R = RA
            S = SA
            ISW = 2
            GO TO 790
  770       M = I
            IF (ISW .EQ. 2) GO TO 780
C     .......... COMPLEX 1-BY-1 BLOCK ..........
            TR = -RA
            TI = -SA
  773       DR = W
            DI = W1
C     .......... COMPLEX DIVIDE (T1,T2) = (TR,TI) / (DR,DI) ..........
  775       IF (DABS(DI) .GT. DABS(DR)) GO TO 777
            RR = DI / DR
            D = DR + DI * RR
            T1 = (TR + TI * RR) / D
            T2 = (TI - TR * RR) / D
            GO TO (787,782), ISW
  777       RR = DR / DI
            D = DR * RR + DI
            T1 = (TR * RR + TI) / D
            T2 = (TI * RR - TR) / D
            GO TO (787,782), ISW
C     .......... COMPLEX 2-BY-2 BLOCK ..........
  780       X = BETM * A(I,I+1) - ALMR * B(I,I+1)
            X1 = -ALMI * B(I,I+1)
            Y = BETM * A(I+1,I)
            TR = Y * RA - W * R + W1 * S
            TI = Y * SA - W * S - W1 * R
            DR = W * ZZ - W1 * Z1 - X * Y
            DI = W * Z1 + W1 * ZZ - X1 * Y
*           ------------------- BEGIN TIMING CODE ----------------------
            IN2BY2 = IN2BY2 + 1
*           -------------------- END TIMING CODE -----------------------
            IF (DR .EQ. 0.0D0 .AND. DI .EQ. 0.0D0) DR = EPSB
            GO TO 775
  782       B(I+1,NA) = T1
            B(I+1,EN) = T2
            ISW = 1
            IF (DABS(Y) .GT. DABS(W) + DABS(W1)) GO TO 785
            TR = -RA - X * B(I+1,NA) + X1 * B(I+1,EN)
            TI = -SA - X * B(I+1,EN) - X1 * B(I+1,NA)
            GO TO 773
  785       T1 = (-R - ZZ * B(I+1,NA) + Z1 * B(I+1,EN)) / Y
            T2 = (-S - ZZ * B(I+1,EN) - Z1 * B(I+1,NA)) / Y
  787       B(I,NA) = T1
            B(I,EN) = T2
  790    CONTINUE
*        --------------------- BEGIN TIMING CODE -----------------------
         OPS = OPS + DBLE( ( 6*EN-7 )*( EN-2 ) + 31*IN2BY2 )
*        ---------------------- END TIMING CODE ------------------------
C     .......... END COMPLEX VECTOR ..........
  795    ISW = 3 - ISW
  800 CONTINUE
C     .......... END BACK SUBSTITUTION.
C                TRANSFORM TO ORIGINAL COORDINATE SYSTEM.
C                FOR J=N STEP -1 UNTIL 1 DO -- ..........
      DO 880 JJ = 1, N
         J = N + 1 - JJ
C
         DO 880 I = 1, N
            ZZ = 0.0D0
C
            DO 860 K = 1, J
  860       ZZ = ZZ + Z(I,K) * B(K,J)
C
            Z(I,J) = ZZ
  880 CONTINUE
*     ----------------------- BEGIN TIMING CODE ------------------------
      OPS = OPS + DBLE( N**2 )*DBLE( N+1 )
*     ------------------------ END TIMING CODE -------------------------
C     .......... NORMALIZE SO THAT MODULUS OF LARGEST
C                COMPONENT OF EACH VECTOR IS 1.
C                (ISW IS 1 INITIALLY FROM BEFORE) ..........
*     ------------------------ BEGIN TIMING CODE -----------------------
      IN2BY2 = 0
*     ------------------------- END TIMING CODE ------------------------
      DO 950 J = 1, N
         D = 0.0D0
         IF (ISW .EQ. 2) GO TO 920
         IF (ALFI(J) .NE. 0.0D0) GO TO 945
C
         DO 890 I = 1, N
            IF (DABS(Z(I,J)) .GT. D) D = DABS(Z(I,J))
  890    CONTINUE
C
         DO 900 I = 1, N
  900    Z(I,J) = Z(I,J) / D
C
         GO TO 950
C
  920    DO 930 I = 1, N
            R = DABS(Z(I,J-1)) + DABS(Z(I,J))
            IF (R .NE. 0.0D0) R = R * DSQRT((Z(I,J-1)/R)**2
     X                                     +(Z(I,J)/R)**2)
            IF (R .GT. D) D = R
  930    CONTINUE
C
         DO 940 I = 1, N
            Z(I,J-1) = Z(I,J-1) / D
            Z(I,J) = Z(I,J) / D
  940    CONTINUE
*        ---------------------- BEGIN TIMING CODE ----------------------
         IN2BY2 = IN2BY2 + 1
*        ----------------------- END TIMING CODE -----------------------
C
  945    ISW = 3 - ISW
  950 CONTINUE
*     ------------------------ BEGIN TIMING CODE -----------------------
      OPS = OPS + DBLE( N*( N + 5*IN2BY2 ) )
*     ------------------------- END TIMING CODE ------------------------
C
      RETURN
      END
      SUBROUTINE DLAQZH( ILQ, ILZ, N, ILO, IHI, A, LDA, B, LDB, Q, LDQ,
     $                   Z, LDZ, WORK, INFO )
*
*  -- LAPACK timing routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     September 30, 1994
*
*     .. Scalar Arguments ..
      LOGICAL            ILQ, ILZ
      INTEGER            IHI, ILO, INFO, LDA, LDB, LDQ, LDZ, N
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), B( LDB, * ), Q( LDQ, * ),
     $                   WORK( N ), Z( LDZ, * )
*     ..
*
*  Purpose
*  =======
*
*  This calls the LAPACK routines to perform the function of
*  QZHES.  It is similar in function to DGGHRD, except that
*  B is not assumed to be upper-triangular.
*
*  It reduces a pair of matrices (A,B) to a Hessenberg-triangular
*  pair (H,T).  More specifically, it computes orthogonal matrices
*  Q and Z, an (upper) Hessenberg matrix H, and an upper triangular
*  matrix T such that:
*
*    A = Q H Z'    and   B = Q T Z'
*
*
*  Arguments
*  =========
*
*  ILQ     (input) LOGICAL
*          = .FALSE. do not compute Q.
*          = .TRUE.  compute Q.
*
*  ILZ     (input) LOGICAL
*          = .FALSE. do not compute Z.
*          = .TRUE.  compute Z.
*
*  N       (input) INTEGER
*          The number of rows and columns in the matrices A, B, Q, and
*          Z.  N must be at least 0.
*
*  ILO     (input) INTEGER
*          Columns 1 through ILO-1 of A and B are assumed to be in
*          upper triangular form already, and will not be modified.
*          ILO must be at least 1.
*
*  IHI     (input) INTEGER
*          Rows IHI+1 through N of A and B are assumed to be in upper
*          triangular form already, and will not be touched.  IHI may
*          not be greater than N.
*
*  A       (input/output) DOUBLE PRECISION array, dimension (LDA, N)
*          On entry, the first of the pair of N x N general matrices to
*          be reduced.
*          On exit, the upper triangle and the first subdiagonal of A
*          are overwritten with the Hessenberg matrix H, and the rest
*          is set to zero.
*
*  LDA     (input) INTEGER
*          The leading dimension of A as declared in the calling
*          program. LDA must be at least max ( 1, N ) .
*
*  B       (input/output) DOUBLE PRECISION array, dimension (LDB, N)
*          On entry, the second of the pair of N x N general matrices to
*          be reduced.
*          On exit, the transformed matrix T = Q' B Z, which is upper
*          triangular.
*
*  LDB     (input) INTEGER
*          The leading dimension of B as declared in the calling
*          program. LDB must be at least max ( 1, N ) .
*
*  Q       (output) DOUBLE PRECISION array, dimension (LDQ,N)
*          If ILQ = .TRUE., Q will contain the orthogonal matrix Q.
*          (See "Purpose", above.)
*          Will not be referenced if ILQ = .FALSE.
*
*  LDQ     (input) INTEGER
*          The leading dimension of the matrix Q. LDQ must be at
*          least 1 and at least N.
*
*  Z       (output) DOUBLE PRECISION array, dimension (LDZ,N)
*          If ILZ = .TRUE., Z will contain the orthogonal matrix Z.
*          (See "Purpose", above.)
*          May be referenced even if ILZ = .FALSE.
*
*  LDZ     (input) INTEGER
*          The leading dimension of the matrix Z. LDZ must be at
*          least 1 and at least N.
*
*  WORK    (workspace) DOUBLE PRECISION array, dimension (N)
*          Workspace.
*
*  INFO    (output) INTEGER
*          = 0:  successful exit.
*          < 0:  if INFO = -i, the i-th argument had an illegal value.
*          > 0:  errors that usually indicate LAPACK problems:
*                = 2: error return from DGEQRF;
*                = 3: error return from DORMQR;
*                = 4: error return from DORGQR;
*                = 5: error return from DGGHRD.
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE
      PARAMETER          ( ZERO = 0.0D0, ONE = 1.0D0 )
*     ..
*     .. Local Scalars ..
      CHARACTER          COMPQ, COMPZ
      INTEGER            ICOLS, IINFO, IROWS
*     ..
*     .. External Subroutines ..
      EXTERNAL           DGEQRF, DGGHRD, DLACPY, DLASET, DORGQR, DORMQR
*     ..
*     .. Executable Statements ..
*
*     Quick return if possible
*
      IF( N.EQ.0 )
     $   RETURN
*
*     Reduce B to triangular form, and initialize Q and/or Z
*
      IROWS = IHI + 1 - ILO
      ICOLS = N + 1 - ILO
      CALL DGEQRF( IROWS, ICOLS, B( ILO, ILO ), LDB, WORK, Z, N*LDZ,
     $             IINFO )
      IF( IINFO.NE.0 ) THEN
         INFO = 2
         GO TO 10
      END IF
*
      CALL DORMQR( 'L', 'T', IROWS, ICOLS, IROWS, B( ILO, ILO ), LDB,
     $             WORK, A( ILO, ILO ), LDA, Z, N*LDZ, IINFO )
      IF( IINFO.NE.0 ) THEN
         INFO = 3
         GO TO 10
      END IF
*
      IF( ILQ ) THEN
         CALL DLASET( 'Full', N, N, ZERO, ONE, Q, LDQ )
         CALL DLACPY( 'L', IROWS-1, IROWS-1, B( ILO+1, ILO ), LDB,
     $                Q( ILO+1, ILO ), LDQ )
         CALL DORGQR( IROWS, IROWS, IROWS, Q( ILO, ILO ), LDQ, WORK, Z,
     $                N*LDZ, IINFO )
         IF( IINFO.NE.0 ) THEN
            INFO = 4
            GO TO 10
         END IF
      END IF
*
*     Reduce to generalized Hessenberg form
*
      IF( ILQ ) THEN
         COMPQ = 'V'
      ELSE
         COMPQ = 'N'
      END IF
*
      IF( ILZ ) THEN
         COMPZ = 'I'
      ELSE
         COMPZ = 'N'
      END IF
*
      CALL DGGHRD( COMPQ, COMPZ, N, ILO, IHI, A, LDA, B, LDB, Q, LDQ, Z,
     $             LDZ, IINFO )
      IF( IINFO.NE.0 ) THEN
         INFO = 5
         GO TO 10
      END IF
*
*     End
*
   10 CONTINUE
*
      RETURN
*
*     End of DLAQZH
*
      END
      SUBROUTINE DLATM4( ITYPE, N, NZ1, NZ2, ISIGN, AMAGN, RCOND,
     $                   TRIANG, IDIST, ISEED, A, LDA )
*
*  -- LAPACK auxiliary test routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     September 30, 1994
*
*     .. Scalar Arguments ..
      INTEGER            IDIST, ISIGN, ITYPE, LDA, N, NZ1, NZ2
      DOUBLE PRECISION   AMAGN, RCOND, TRIANG
*     ..
*     .. Array Arguments ..
      INTEGER            ISEED( 4 )
      DOUBLE PRECISION   A( LDA, * )
*     ..
*
*  Purpose
*  =======
*
*  DLATM4 generates basic square matrices, which may later be
*  multiplied by others in order to produce test matrices.  It is
*  intended mainly to be used to test the generalized eigenvalue
*  routines.
*
*  It first generates the diagonal and (possibly) subdiagonal,
*  according to the value of ITYPE, NZ1, NZ2, ISIGN, AMAGN, and RCOND.
*  It then fills in the upper triangle with random numbers, if TRIANG is
*  non-zero.
*
*  Arguments
*  =========
*
*  ITYPE   (input) INTEGER
*          The "type" of matrix on the diagonal and sub-diagonal.
*          If ITYPE < 0, then type abs(ITYPE) is generated and then
*             swapped end for end (A(I,J) := A'(N-J,N-I).)  See also
*             the description of AMAGN and ISIGN.
*
*          Special types:
*          = 0:  the zero matrix.
*          = 1:  the identity.
*          = 2:  a transposed Jordan block.
*          = 3:  If N is odd, then a k+1 x k+1 transposed Jordan block
*                followed by a k x k identity block, where k=(N-1)/2.
*                If N is even, then k=(N-2)/2, and a zero diagonal entry
*                is tacked onto the end.
*
*          Diagonal types.  The diagonal consists of NZ1 zeros, then
*             k=N-NZ1-NZ2 nonzeros.  The subdiagonal is zero.  ITYPE
*             specifies the nonzero diagonal entries as follows:
*          = 4:  1, ..., k
*          = 5:  1, RCOND, ..., RCOND
*          = 6:  1, ..., 1, RCOND
*          = 7:  1, a, a^2, ..., a^(k-1)=RCOND
*          = 8:  1, 1-d, 1-2*d, ..., 1-(k-1)*d=RCOND
*          = 9:  random numbers chosen from (RCOND,1)
*          = 10: random numbers with distribution IDIST (see DLARND.)
*
*  N       (input) INTEGER
*          The order of the matrix.
*
*  NZ1     (input) INTEGER
*          If abs(ITYPE) > 3, then the first NZ1 diagonal entries will
*          be zero.
*
*  NZ2     (input) INTEGER
*          If abs(ITYPE) > 3, then the last NZ2 diagonal entries will
*          be zero.
*
*  ISIGN   (input) INTEGER
*          = 0: The sign of the diagonal and subdiagonal entries will
*               be left unchanged.
*          = 1: The diagonal and subdiagonal entries will have their
*               sign changed at random.
*          = 2: If ITYPE is 2 or 3, then the same as ISIGN=1.
*               Otherwise, with probability 0.5, odd-even pairs of
*               diagonal entries A(2*j-1,2*j-1), A(2*j,2*j) will be
*               converted to a 2x2 block by pre- and post-multiplying
*               by distinct random orthogonal rotations.  The remaining
*               diagonal entries will have their sign changed at random.
*
*  AMAGN   (input) DOUBLE PRECISION
*          The diagonal and subdiagonal entries will be multiplied by
*          AMAGN.
*
*  RCOND   (input) DOUBLE PRECISION
*          If abs(ITYPE) > 4, then the smallest diagonal entry will be
*          entry will be RCOND.  RCOND must be between 0 and 1.
*
*  TRIANG  (input) DOUBLE PRECISION
*          The entries above the diagonal will be random numbers with
*          magnitude bounded by TRIANG (i.e., random numbers multiplied
*          by TRIANG.)
*
*  IDIST   (input) INTEGER
*          Specifies the type of distribution to be used to generate a
*          random matrix.
*          = 1:  UNIFORM( 0, 1 )
*          = 2:  UNIFORM( -1, 1 )
*          = 3:  NORMAL ( 0, 1 )
*
*  ISEED   (input/output) INTEGER array, dimension (4)
*          On entry ISEED specifies the seed of the random number
*          generator.  The values of ISEED are changed on exit, and can
*          be used in the next call to DLATM4 to continue the same
*          random number sequence.
*          Note: ISEED(4) should be odd, for the random number generator
*          used at present.
*
*  A       (output) DOUBLE PRECISION array, dimension (LDA, N)
*          Array to be computed.
*
*  LDA     (input) INTEGER
*          Leading dimension of A.  Must be at least 1 and at least N.
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE, TWO
      PARAMETER          ( ZERO = 0.0D0, ONE = 1.0D0, TWO = 2.0D0 )
      DOUBLE PRECISION   HALF
      PARAMETER          ( HALF = ONE / TWO )
*     ..
*     .. Local Scalars ..
      INTEGER            I, IOFF, ISDB, ISDE, JC, JD, JR, K, KBEG, KEND,
     $                   KLEN
      DOUBLE PRECISION   ALPHA, CL, CR, SAFMIN, SL, SR, SV1, SV2, TEMP
*     ..
*     .. External Functions ..
      DOUBLE PRECISION   DLAMCH, DLARAN, DLARND
      EXTERNAL           DLAMCH, DLARAN, DLARND
*     ..
*     .. External Subroutines ..
      EXTERNAL           DLASET
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, DBLE, EXP, LOG, MAX, MIN, MOD, SQRT
*     ..
*     .. Executable Statements ..
*
      IF( N.LE.0 )
     $   RETURN
      CALL DLASET( 'Full', N, N, ZERO, ZERO, A, LDA )
*
*     Insure a correct ISEED
*
      IF( MOD( ISEED( 4 ), 2 ).NE.1 )
     $   ISEED( 4 ) = ISEED( 4 ) + 1
*
*     Compute diagonal and subdiagonal according to ITYPE, NZ1, NZ2,
*     and RCOND
*
      IF( ITYPE.NE.0 ) THEN
         IF( ABS( ITYPE ).GE.4 ) THEN
            KBEG = MAX( 1, MIN( N, NZ1+1 ) )
            KEND = MAX( KBEG, MIN( N, N-NZ2 ) )
            KLEN = KEND + 1 - KBEG
         ELSE
            KBEG = 1
            KEND = N
            KLEN = N
         END IF
         ISDB = 1
         ISDE = 0
         GO TO ( 10, 30, 50, 80, 100, 120, 140, 160,
     $           180, 200 )ABS( ITYPE )
*
*        |ITYPE| = 1: Identity
*
   10    CONTINUE
         DO 20 JD = 1, N
            A( JD, JD ) = ONE
   20    CONTINUE
         GO TO 220
*
*        |ITYPE| = 2: Transposed Jordan block
*
   30    CONTINUE
         DO 40 JD = 1, N - 1
            A( JD+1, JD ) = ONE
   40    CONTINUE
         ISDB = 1
         ISDE = N - 1
         GO TO 220
*
*        |ITYPE| = 3: Transposed Jordan block, followed by the identity.
*
   50    CONTINUE
         K = ( N-1 ) / 2
         DO 60 JD = 1, K
            A( JD+1, JD ) = ONE
   60    CONTINUE
         ISDB = 1
         ISDE = K
         DO 70 JD = K + 2, 2*K + 1
            A( JD, JD ) = ONE
   70    CONTINUE
         GO TO 220
*
*        |ITYPE| = 4: 1,...,k
*
   80    CONTINUE
         DO 90 JD = KBEG, KEND
            A( JD, JD ) = DBLE( JD-NZ1 )
   90    CONTINUE
         GO TO 220
*
*        |ITYPE| = 5: One large D value:
*
  100    CONTINUE
         DO 110 JD = KBEG + 1, KEND
            A( JD, JD ) = RCOND
  110    CONTINUE
         A( KBEG, KBEG ) = ONE
         GO TO 220
*
*        |ITYPE| = 6: One small D value:
*
  120    CONTINUE
         DO 130 JD = KBEG, KEND - 1
            A( JD, JD ) = ONE
  130    CONTINUE
         A( KEND, KEND ) = RCOND
         GO TO 220
*
*        |ITYPE| = 7: Exponentially distributed D values:
*
  140    CONTINUE
         A( KBEG, KBEG ) = ONE
         IF( KLEN.GT.1 ) THEN
            ALPHA = RCOND**( ONE / DBLE( KLEN-1 ) )
            DO 150 I = 2, KLEN
               A( NZ1+I, NZ1+I ) = ALPHA**( I-1 )
  150       CONTINUE
         END IF
         GO TO 220
*
*        |ITYPE| = 8: Arithmetically distributed D values:
*
  160    CONTINUE
         A( KBEG, KBEG ) = ONE
         IF( KLEN.GT.1 ) THEN
            ALPHA = ( ONE-RCOND ) / DBLE( KLEN-1 )
            DO 170 I = 2, KLEN
               A( NZ1+I, NZ1+I ) = DBLE( KLEN-I )*ALPHA + RCOND
  170       CONTINUE
         END IF
         GO TO 220
*
*        |ITYPE| = 9: Randomly distributed D values on ( RCOND, 1):
*
  180    CONTINUE
         ALPHA = LOG( RCOND )
         DO 190 JD = KBEG, KEND
            A( JD, JD ) = EXP( ALPHA*DLARAN( ISEED ) )
  190    CONTINUE
         GO TO 220
*
*        |ITYPE| = 10: Randomly distributed D values from DIST
*
  200    CONTINUE
         DO 210 JD = KBEG, KEND
            A( JD, JD ) = DLARND( IDIST, ISEED )
  210    CONTINUE
*
  220    CONTINUE
*
*        Scale by AMAGN
*
         DO 230 JD = KBEG, KEND
            A( JD, JD ) = AMAGN*DBLE( A( JD, JD ) )
  230    CONTINUE
         DO 240 JD = ISDB, ISDE
            A( JD+1, JD ) = AMAGN*DBLE( A( JD+1, JD ) )
  240    CONTINUE
*
*        If ISIGN = 1 or 2, assign random signs to diagonal and
*        subdiagonal
*
         IF( ISIGN.GT.0 ) THEN
            DO 250 JD = KBEG, KEND
               IF( DBLE( A( JD, JD ) ).NE.ZERO ) THEN
                  IF( DLARAN( ISEED ).GT.HALF )
     $               A( JD, JD ) = -A( JD, JD )
               END IF
  250       CONTINUE
            DO 260 JD = ISDB, ISDE
               IF( DBLE( A( JD+1, JD ) ).NE.ZERO ) THEN
                  IF( DLARAN( ISEED ).GT.HALF )
     $               A( JD+1, JD ) = -A( JD+1, JD )
               END IF
  260       CONTINUE
         END IF
*
*        Reverse if ITYPE < 0
*
         IF( ITYPE.LT.0 ) THEN
            DO 270 JD = KBEG, ( KBEG+KEND-1 ) / 2
               TEMP = A( JD, JD )
               A( JD, JD ) = A( KBEG+KEND-JD, KBEG+KEND-JD )
               A( KBEG+KEND-JD, KBEG+KEND-JD ) = TEMP
  270       CONTINUE
            DO 280 JD = 1, ( N-1 ) / 2
               TEMP = A( JD+1, JD )
               A( JD+1, JD ) = A( N+1-JD, N-JD )
               A( N+1-JD, N-JD ) = TEMP
  280       CONTINUE
         END IF
*
*        If ISIGN = 2, and no subdiagonals already, then apply
*        random rotations to make 2x2 blocks.
*
         IF( ISIGN.EQ.2 .AND. ITYPE.NE.2 .AND. ITYPE.NE.3 ) THEN
            SAFMIN = DLAMCH( 'S' )
            DO 290 JD = KBEG, KEND - 1, 2
               IF( DLARAN( ISEED ).GT.HALF ) THEN
*
*                 Rotation on left.
*
                  CL = TWO*DLARAN( ISEED ) - ONE
                  SL = TWO*DLARAN( ISEED ) - ONE
                  TEMP = ONE / MAX( SAFMIN, SQRT( CL**2+SL**2 ) )
                  CL = CL*TEMP
                  SL = SL*TEMP
*
*                 Rotation on right.
*
                  CR = TWO*DLARAN( ISEED ) - ONE
                  SR = TWO*DLARAN( ISEED ) - ONE
                  TEMP = ONE / MAX( SAFMIN, SQRT( CR**2+SR**2 ) )
                  CR = CR*TEMP
                  SR = SR*TEMP
*
*                 Apply
*
                  SV1 = A( JD, JD )
                  SV2 = A( JD+1, JD+1 )
                  A( JD, JD ) = CL*CR*SV1 + SL*SR*SV2
                  A( JD+1, JD ) = -SL*CR*SV1 + CL*SR*SV2
                  A( JD, JD+1 ) = -CL*SR*SV1 + SL*CR*SV2
                  A( JD+1, JD+1 ) = SL*SR*SV1 + CL*CR*SV2
               END IF
  290       CONTINUE
         END IF
*
      END IF
*
*     Fill in upper triangle (except for 2x2 blocks)
*
      IF( TRIANG.NE.ZERO ) THEN
         IF( ISIGN.NE.2 .OR. ITYPE.EQ.2 .OR. ITYPE.EQ.3 ) THEN
            IOFF = 1
         ELSE
            IOFF = 2
            DO 300 JR = 1, N - 1
               IF( A( JR+1, JR ).EQ.ZERO )
     $            A( JR, JR+1 ) = TRIANG*DLARND( IDIST, ISEED )
  300       CONTINUE
         END IF
*
         DO 320 JC = 2, N
            DO 310 JR = 1, JC - IOFF
               A( JR, JC ) = TRIANG*DLARND( IDIST, ISEED )
  310       CONTINUE
  320    CONTINUE
      END IF
*
      RETURN
*
*     End of DLATM4
*
      END
      DOUBLE PRECISION FUNCTION DMFLOP( OPS, TIME, INFO )
*
*  -- LAPACK timing routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     February 29, 1992
*
*     .. Scalar Arguments ..
      INTEGER            INFO
      DOUBLE PRECISION   OPS, TIME
*     ..
*
*  Purpose
*  =======
*
*     DMFLOP computes the megaflop rate given the number of operations
*     and time in seconds.  This is basically just a divide operation,
*     but care is taken not to divide by zero.
*
*  Arguments
*  =========
*
*  OPS    - DOUBLE PRECISION
*           On entry, OPS is the number of floating point operations
*           performed by the timed routine.
*
*  TIME   - DOUBLE PRECISION
*           On entry, TIME is the total time in seconds used by the
*           timed routine.
*
*  INFO   - INTEGER
*           On entry, INFO specifies the return code from the timed
*           routine.  If INFO is not 0, then DMFLOP returns a negative
*           value, indicating an error.
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
      DOUBLE PRECISION FUNCTION DOPLA( SUBNAM, M, N, KL, KU, NB )
*
*  -- LAPACK timing routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     February 29, 1992
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
*     --------------------------------------------------------
*     SY:  SYmmetric indefinite matrices
*     SP:  Symmetric indefinite Packed matrices
*     HE:  HErmitian indefinite matrices (complex only)
*     HP:  Hermitian indefinite Packed matrices (complex only)
*     --------------------------------------------------------
*
      ELSE IF( LSAMEN( 2, C2, 'SY' ) .OR. LSAMEN( 2, C2, 'SP' ) .OR.
     $         LSAMEN( 3, SUBNAM, 'ZHE' ) .OR.
     $         LSAMEN( 3, SUBNAM, 'ZHE' ) .OR.
     $         LSAMEN( 3, SUBNAM, 'ZHP' ) .OR.
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
      DOUBLE PRECISION FUNCTION DOPLA2( SUBNAM, OPTS, M, N, K, L, NB )
*
*  -- LAPACK timing routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     February 29, 1992
*
*     .. Scalar Arguments ..
      CHARACTER*6        SUBNAM
      CHARACTER*( * )    OPTS
      INTEGER            K, L, M, N, NB
*     ..
*
*  Purpose
*  =======
*
*  DOPLA2 computes an approximation of the number of floating point
*  operations used by the subroutine SUBNAM with character options
*  OPTS and parameters M, N, K, L, and NB.
*
*  This version counts operations for the LAPACK subroutines that
*  call other LAPACK routines.
*
*  Arguments
*  =========
*
*  SUBNAM  (input) CHARACTER*6
*          The name of the subroutine.
*
*  OPTS    (input) CHRACTER*(*)
*          A string of character options to subroutine SUBNAM.
*
*  M       (input) INTEGER
*          The number of rows of the coefficient matrix.
*
*  N       (input) INTEGER
*          The number of columns of the coefficient matrix.
*
*  K       (input) INTEGER
*          A third problem dimension, if needed.
*
*  L       (input) INTEGER
*          A fourth problem dimension, if needed.
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
*  xORMBR:  VECT // SIDE // TRANS, M, N, K   =>  OPTS, M, N, K
*
*  means that the character string VECT // SIDE // TRANS is passed to
*  the argument OPTS, and the integer parameters M, N, and K are passed
*  to the arguments M, N, and K,
*
*  =====================================================================
*
*     .. Local Scalars ..
      LOGICAL            CORZ, SORD
      CHARACTER          C1, SIDE, UPLO, VECT
      CHARACTER*2        C2
      CHARACTER*3        C3
      CHARACTER*6        SUB2
      INTEGER            IHI, ILO, ISIDE, MI, NI, NQ
*     ..
*     .. External Functions ..
      LOGICAL            LSAME, LSAMEN
      DOUBLE PRECISION   DOPLA
      EXTERNAL           LSAME, LSAMEN, DOPLA
*     ..
*     .. Executable Statements ..
*
*     ---------------------------------------------------------
*     Initialize DOPLA2 to 0 and do a quick return if possible.
*     ---------------------------------------------------------
*
      DOPLA2 = 0
      C1 = SUBNAM( 1: 1 )
      C2 = SUBNAM( 2: 3 )
      C3 = SUBNAM( 4: 6 )
      SORD = LSAME( C1, 'S' ) .OR. LSAME( C1, 'D' )
      CORZ = LSAME( C1, 'C' ) .OR. LSAME( C1, 'Z' )
      IF( M.LE.0 .OR. .NOT.( SORD .OR. CORZ ) )
     $   RETURN
*
*     -------------------
*     Orthogonal matrices
*     -------------------
*
      IF( ( SORD .AND. LSAMEN( 2, C2, 'OR' ) ) .OR.
     $    ( CORZ .AND. LSAMEN( 2, C2, 'UN' ) ) ) THEN
*
         IF( LSAMEN( 3, C3, 'GBR' ) ) THEN
*
*           -GBR:  VECT, M, N, K  =>  OPTS, M, N, K
*
            VECT = OPTS( 1: 1 )
            IF( LSAME( VECT, 'Q' ) ) THEN
               SUB2 = SUBNAM( 1: 3 ) // 'GQR'
               IF( M.GE.K ) THEN
                  DOPLA2 = DOPLA( SUB2, M, N, K, 0, NB )
               ELSE
                  DOPLA2 = DOPLA( SUB2, M-1, M-1, M-1, 0, NB )
               END IF
            ELSE
               SUB2 = SUBNAM( 1: 3 ) // 'GLQ'
               IF( K.LT.N ) THEN
                  DOPLA2 = DOPLA( SUB2, M, N, K, 0, NB )
               ELSE
                  DOPLA2 = DOPLA( SUB2, N-1, N-1, N-1, 0, NB )
               END IF
            END IF
*
         ELSE IF( LSAMEN( 3, C3, 'MBR' ) ) THEN
*
*           -MBR:  VECT // SIDE // TRANS, M, N, K  =>  OPTS, M, N, K
*
            VECT = OPTS( 1: 1 )
            SIDE = OPTS( 2: 2 )
            IF( LSAME( SIDE, 'L' ) ) THEN
               NQ = M
               ISIDE = 0
            ELSE
               NQ = N
               ISIDE = 1
            END IF
            IF( LSAME( VECT, 'Q' ) ) THEN
               SUB2 = SUBNAM( 1: 3 ) // 'MQR'
               IF( NQ.GE.K ) THEN
                  DOPLA2 = DOPLA( SUB2, M, N, K, ISIDE, NB )
               ELSE IF( ISIDE.EQ.0 ) THEN
                  DOPLA2 = DOPLA( SUB2, M-1, N, NQ-1, ISIDE, NB )
               ELSE
                  DOPLA2 = DOPLA( SUB2, M, N-1, NQ-1, ISIDE, NB )
               END IF
            ELSE
               SUB2 = SUBNAM( 1: 3 ) // 'MLQ'
               IF( NQ.GT.K ) THEN
                  DOPLA2 = DOPLA( SUB2, M, N, K, ISIDE, NB )
               ELSE IF( ISIDE.EQ.0 ) THEN
                  DOPLA2 = DOPLA( SUB2, M-1, N, NQ-1, ISIDE, NB )
               ELSE
                  DOPLA2 = DOPLA( SUB2, M, N-1, NQ-1, ISIDE, NB )
               END IF
            END IF
*
         ELSE IF( LSAMEN( 3, C3, 'GHR' ) ) THEN
*
*           -GHR:  N, ILO, IHI  =>  M, N, K
*
            ILO = N
            IHI = K
            SUB2 = SUBNAM( 1: 3 ) // 'GQR'
            DOPLA2 = DOPLA( SUB2, IHI-ILO, IHI-ILO, IHI-ILO, 0, NB )
*
         ELSE IF( LSAMEN( 3, C3, 'MHR' ) ) THEN
*
*           -MHR:  SIDE // TRANS, M, N, ILO, IHI  =>  OPTS, M, N, K, L
*
            SIDE = OPTS( 1: 1 )
            ILO = K
            IHI = L
            IF( LSAME( SIDE, 'L' ) ) THEN
               MI = IHI - ILO
               NI = N
               ISIDE = -1
            ELSE
               MI = M
               NI = IHI - ILO
               ISIDE = 1
            END IF
            SUB2 = SUBNAM( 1: 3 ) // 'MQR'
            DOPLA2 = DOPLA( SUB2, MI, NI, IHI-ILO, ISIDE, NB )
*
         ELSE IF( LSAMEN( 3, C3, 'GTR' ) ) THEN
*
*           -GTR:  UPLO, N  =>  OPTS, M
*
            UPLO = OPTS( 1: 1 )
            IF( LSAME( UPLO, 'U' ) ) THEN
               SUB2 = SUBNAM( 1: 3 ) // 'GQL'
               DOPLA2 = DOPLA( SUB2, M-1, M-1, M-1, 0, NB )
            ELSE
               SUB2 = SUBNAM( 1: 3 ) // 'GQR'
               DOPLA2 = DOPLA( SUB2, M-1, M-1, M-1, 0, NB )
            END IF
*
         ELSE IF( LSAMEN( 3, C3, 'MTR' ) ) THEN
*
*           -MTR:  SIDE // UPLO // TRANS, M, N  =>  OPTS, M, N
*
            SIDE = OPTS( 1: 1 )
            UPLO = OPTS( 2: 2 )
            IF( LSAME( SIDE, 'L' ) ) THEN
               MI = M - 1
               NI = N
               NQ = M
               ISIDE = -1
            ELSE
               MI = M
               NI = N - 1
               NQ = N
               ISIDE = 1
            END IF
*
            IF( LSAME( UPLO, 'U' ) ) THEN
               SUB2 = SUBNAM( 1: 3 ) // 'MQL'
               DOPLA2 = DOPLA( SUB2, MI, NI, NQ-1, ISIDE, NB )
            ELSE
               SUB2 = SUBNAM( 1: 3 ) // 'MQR'
               DOPLA2 = DOPLA( SUB2, MI, NI, NQ-1, ISIDE, NB )
            END IF
*
         END IF
      END IF
*
      RETURN
*
*     End of DOPLA2
*
      END
      SUBROUTINE DPRTBE( SUBNAM, NTYPES, DOTYPE, NSIZES, NN, INPARM,
     $                   PNAMES, NPARMS, NP1, NP2, NP3, NP4, OPS, LDO1,
     $                   LDO2, TIMES, LDT1, LDT2, RWORK, LLWORK, NOUT )
*
*  -- LAPACK timing routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     February 29, 1992
*
*     .. Scalar Arguments ..
      CHARACTER*( * )    SUBNAM
      INTEGER            INPARM, LDO1, LDO2, LDT1, LDT2, NOUT, NPARMS,
     $                   NSIZES, NTYPES
*     ..
*     .. Array Arguments ..
      LOGICAL            DOTYPE( NTYPES ), LLWORK( NPARMS )
      CHARACTER*( * )    PNAMES( * )
      INTEGER            NN( NSIZES ), NP1( * ), NP2( * ), NP3( * ),
     $                   NP4( * )
      DOUBLE PRECISION   OPS( LDO1, LDO2, * ), RWORK( * ),
     $                   TIMES( LDT1, LDT2, * )
*     ..
*
*  Purpose
*  =======
*
*     DPRTBE prints out timing information for the eigenvalue routines.
*     The table has NTYPES block rows and NSIZES columns, with NPARMS
*     individual rows in each block row.  There are INPARM quantities
*     which depend on rows (currently, INPARM <= 4).
*
*  Arguments (none are modified)
*  =========
*
*  SUBNAM - CHARACTER*(*)
*           The label for the output.
*
*  NTYPES - INTEGER
*           The number of values of DOTYPE, and also the
*           number of sets of rows of the table.
*
*  DOTYPE - LOGICAL array of dimension( NTYPES )
*           If DOTYPE(j) is .TRUE., then block row j (which includes
*           data from RESLTS( i, j, k ), for all i and k) will be
*           printed.  If DOTYPE(j) is .FALSE., then block row j will
*           not be printed.
*
*  NSIZES - INTEGER
*           The number of values of NN, and also the
*           number of columns of the table.
*
*  NN   -   INTEGER array of dimension( NSIZES )
*           The values of N used to label each column.
*
*  INPARM - INTEGER
*           The number of different parameters which are functions of
*           the row number.  At the moment, INPARM <= 4.
*
*  PNAMES - CHARACTER*(*) array of dimension( INPARM )
*           The label for the columns.
*
*  NPARMS - INTEGER
*           The number of values for each "parameter", i.e., the
*           number of rows for each value of DOTYPE.
*
*  NP1    - INTEGER array of dimension( NPARMS )
*           The first quantity which depends on row number.
*
*  NP2    - INTEGER array of dimension( NPARMS )
*           The second quantity which depends on row number.
*
*  NP3    - INTEGER array of dimension( NPARMS )
*           The third quantity which depends on row number.
*
*  NP4    - INTEGER array of dimension( NPARMS )
*           The fourth quantity which depends on row number.
*
*  OPS    - DOUBLE PRECISION array of dimension( LDT1, LDT2, NSIZES )
*           The operation counts.  The first index indicates the row,
*           the second index indicates the block row, and the last
*           indicates the column.
*
*  LDO1   - INTEGER
*           The first dimension of OPS.  It must be at least
*           min( 1, NPARMS ).
*
*  LDO2   - INTEGER
*           The second dimension of OPS.  It must be at least
*           min( 1, NTYPES ).
*
*  TIMES  - DOUBLE PRECISION array of dimension( LDT1, LDT2, NSIZES )
*           The times (in seconds).  The first index indicates the row,
*           the second index indicates the block row, and the last
*           indicates the column.
*
*  LDT1   - INTEGER
*           The first dimension of RESLTS.  It must be at least
*           min( 1, NPARMS ).
*
*  LDT2   - INTEGER
*           The second dimension of RESLTS.  It must be at least
*           min( 1, NTYPES ).
*
*  RWORK  - DOUBLE PRECISION array of dimension( NSIZES*NTYPES*NPARMS )
*           Real workspace.
*           Modified.
*
*  LLWORK - LOGICAL array of dimension( NPARMS )
*           Logical workspace.  It is used to turn on or off specific
*           lines in the output.  If LLWORK(i) is .TRUE., then row i
*           (which includes data from OPS(i,j,k) or TIMES(i,j,k) for
*           all j and k) will be printed.  If LLWORK(i) is
*           .FALSE., then row i will not be printed.
*           Modified.
*
*  NOUT   - INTEGER
*           The output unit number on which the table
*           is to be printed.  If NOUT <= 0, no output is printed.
*
*  =====================================================================
*
*     .. Local Scalars ..
      LOGICAL            LTEMP
      INTEGER            I, IINFO, ILINE, ILINES, IPAR, J, JP, JS, JT
*     ..
*     .. External Functions ..
      DOUBLE PRECISION   DMFLOP
      EXTERNAL           DMFLOP
*     ..
*     .. External Subroutines ..
      EXTERNAL           DPRTBS
*     ..
*     .. Executable Statements ..
*
*
*     First line
*
      WRITE( NOUT, FMT = 9999 )SUBNAM
*
*     Set up which lines are to be printed.
*
      LLWORK( 1 ) = .TRUE.
      ILINES = 1
      DO 20 IPAR = 2, NPARMS
         LLWORK( IPAR ) = .TRUE.
         DO 10 J = 1, IPAR - 1
            LTEMP = .FALSE.
            IF( INPARM.GE.1 .AND. NP1( J ).NE.NP1( IPAR ) )
     $         LTEMP = .TRUE.
            IF( INPARM.GE.2 .AND. NP2( J ).NE.NP2( IPAR ) )
     $         LTEMP = .TRUE.
            IF( INPARM.GE.3 .AND. NP3( J ).NE.NP3( IPAR ) )
     $         LTEMP = .TRUE.
            IF( INPARM.GE.4 .AND. NP4( J ).NE.NP4( IPAR ) )
     $         LTEMP = .TRUE.
            IF( .NOT.LTEMP )
     $         LLWORK( IPAR ) = .FALSE.
   10    CONTINUE
         IF( LLWORK( IPAR ) )
     $      ILINES = ILINES + 1
   20 CONTINUE
      IF( ILINES.EQ.1 ) THEN
         IF( INPARM.EQ.1 ) THEN
            WRITE( NOUT, FMT = 9995 )PNAMES( 1 ), NP1( 1 )
         ELSE IF( INPARM.EQ.2 ) THEN
            WRITE( NOUT, FMT = 9995 )PNAMES( 1 ), NP1( 1 ),
     $         PNAMES( 2 ), NP2( 1 )
         ELSE IF( INPARM.EQ.3 ) THEN
            WRITE( NOUT, FMT = 9995 )PNAMES( 1 ), NP1( 1 ),
     $         PNAMES( 2 ), NP2( 1 ), PNAMES( 3 ), NP3( 1 )
         ELSE IF( INPARM.EQ.4 ) THEN
            WRITE( NOUT, FMT = 9995 )PNAMES( 1 ), NP1( 1 ),
     $         PNAMES( 2 ), NP2( 1 ), PNAMES( 3 ), NP3( 1 ),
     $         PNAMES( 4 ), NP4( 1 )
         END IF
      ELSE
         ILINE = 0
         DO 30 J = 1, NPARMS
            IF( LLWORK( J ) ) THEN
               ILINE = ILINE + 1
               IF( INPARM.EQ.1 ) THEN
                  WRITE( NOUT, FMT = 9994 )ILINE, PNAMES( 1 ), NP1( J )
               ELSE IF( INPARM.EQ.2 ) THEN
                  WRITE( NOUT, FMT = 9994 )ILINE, PNAMES( 1 ),
     $               NP1( J ), PNAMES( 2 ), NP2( J )
               ELSE IF( INPARM.EQ.3 ) THEN
                  WRITE( NOUT, FMT = 9994 )ILINE, PNAMES( 1 ),
     $               NP1( J ), PNAMES( 2 ), NP2( J ), PNAMES( 3 ),
     $               NP3( J )
               ELSE IF( INPARM.EQ.4 ) THEN
                  WRITE( NOUT, FMT = 9994 )ILINE, PNAMES( 1 ),
     $               NP1( J ), PNAMES( 2 ), NP2( J ), PNAMES( 3 ),
     $               NP3( J ), PNAMES( 4 ), NP4( J )
               END IF
            END IF
   30    CONTINUE
      END IF
*
*     Execution Times
*
      WRITE( NOUT, FMT = 9996 )
      CALL DPRTBS( 'Type', 'N ', NTYPES, DOTYPE, NSIZES, NN, NPARMS,
     $             LLWORK, TIMES, LDT1, LDT2, NOUT )
*
*     Operation Counts
*
      WRITE( NOUT, FMT = 9997 )
      CALL DPRTBS( 'Type', 'N ', NTYPES, DOTYPE, NSIZES, NN, NPARMS,
     $             LLWORK, OPS, LDO1, LDO2, NOUT )
*
*     Megaflop Rates
*
      IINFO = 0
      DO 60 JS = 1, NSIZES
         DO 50 JT = 1, NTYPES
            IF( DOTYPE( JT ) ) THEN
               DO 40 JP = 1, NPARMS
                  I = JP + NPARMS*( JT-1+NTYPES*( JS-1 ) )
                  RWORK( I ) = DMFLOP( OPS( JP, JT, JS ),
     $                         TIMES( JP, JT, JS ), IINFO )
   40          CONTINUE
            END IF
   50    CONTINUE
   60 CONTINUE
*
      WRITE( NOUT, FMT = 9998 )
      CALL DPRTBS( 'Type', 'N ', NTYPES, DOTYPE, NSIZES, NN, NPARMS,
     $             LLWORK, RWORK, NPARMS, NTYPES, NOUT )
*
 9999 FORMAT( / / / ' ****** Results for ', A, ' ******' )
 9998 FORMAT( / ' *** Speed in megaflops ***' )
 9997 FORMAT( / ' *** Number of floating-point operations ***' )
 9996 FORMAT( / ' *** Time in seconds ***' )
 9995 FORMAT( 5X, : 'with ', A, '=', I5, 3( : ', ', A, '=', I5 ) )
 9994 FORMAT( 5X, : 'line ', I2, ' with ', A, '=', I5,
     $      3( : ', ', A, '=', I5 ) )
      RETURN
*
*     End of DPRTBE
*
      END
      SUBROUTINE DPRTBG( SUBNAM, NTYPES, DOTYPE, NSIZES, NN, INPARM,
     $                   PNAMES, NPARMS, NP1, NP2, NP3, NP4, NP5, NP6,
     $                   OPS, LDO1, LDO2, TIMES, LDT1, LDT2, RWORK,
     $                   LLWORK, NOUT )
*
*  -- LAPACK timing routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     February 29, 1992
*
*     .. Scalar Arguments ..
      CHARACTER*( * )    SUBNAM
      INTEGER            INPARM, LDO1, LDO2, LDT1, LDT2, NOUT, NPARMS,
     $                   NSIZES, NTYPES
*     ..
*     .. Array Arguments ..
      LOGICAL            DOTYPE( NTYPES ), LLWORK( NPARMS )
      CHARACTER*( * )    PNAMES( * )
      INTEGER            NN( NSIZES ), NP1( * ), NP2( * ), NP3( * ),
     $                   NP4( * ), NP5( * ), NP6( * )
      DOUBLE PRECISION   OPS( LDO1, LDO2, * ), RWORK( * ),
     $                   TIMES( LDT1, LDT2, * )
*     ..
*
*  Purpose
*  =======
*
*     DPRTBG prints out timing information for the eigenvalue routines.
*     The table has NTYPES block rows and NSIZES columns, with NPARMS
*     individual rows in each block row.  There are INPARM quantities
*     which depend on rows (currently, INPARM <= 4).
*
*  Arguments (none are modified)
*  =========
*
*  SUBNAM - CHARACTER*(*)
*           The label for the output.
*
*  NTYPES - INTEGER
*           The number of values of DOTYPE, and also the
*           number of sets of rows of the table.
*
*  DOTYPE - LOGICAL array of dimension( NTYPES )
*           If DOTYPE(j) is .TRUE., then block row j (which includes
*           data from RESLTS( i, j, k ), for all i and k) will be
*           printed.  If DOTYPE(j) is .FALSE., then block row j will
*           not be printed.
*
*  NSIZES - INTEGER
*           The number of values of NN, and also the
*           number of columns of the table.
*
*  NN   -   INTEGER array of dimension( NSIZES )
*           The values of N used to label each column.
*
*  INPARM - INTEGER
*           The number of different parameters which are functions of
*           the row number.  At the moment, INPARM <= 4.
*
*  PNAMES - CHARACTER*(*) array of dimension( INPARM )
*           The label for the columns.
*
*  NPARMS - INTEGER
*           The number of values for each "parameter", i.e., the
*           number of rows for each value of DOTYPE.
*
*  NP1    - INTEGER array of dimension( NPARMS )
*           The first quantity which depends on row number.
*
*  NP2    - INTEGER array of dimension( NPARMS )
*           The second quantity which depends on row number.
*
*  NP3    - INTEGER array of dimension( NPARMS )
*           The third quantity which depends on row number.
*
*  NP4    - INTEGER array of dimension( NPARMS )
*           The fourth quantity which depends on row number.
*
*  NP5    - INTEGER array of dimension( NPARMS )
*           The fifth quantity which depends on row number.
*
*  NP6    - INTEGER array of dimension( NPARMS )
*           The sixth quantity which depends on row number.
*
*  OPS    - DOUBLE PRECISION array of dimension( LDT1, LDT2, NSIZES )
*           The operation counts.  The first index indicates the row,
*           the second index indicates the block row, and the last
*           indicates the column.
*
*  LDO1   - INTEGER
*           The first dimension of OPS.  It must be at least
*           min( 1, NPARMS ).
*
*  LDO2   - INTEGER
*           The second dimension of OPS.  It must be at least
*           min( 1, NTYPES ).
*
*  TIMES  - DOUBLE PRECISION array of dimension( LDT1, LDT2, NSIZES )
*           The times (in seconds).  The first index indicates the row,
*           the second index indicates the block row, and the last
*           indicates the column.
*
*  LDT1   - INTEGER
*           The first dimension of RESLTS.  It must be at least
*           min( 1, NPARMS ).
*
*  LDT2   - INTEGER
*           The second dimension of RESLTS.  It must be at least
*           min( 1, NTYPES ).
*
*  RWORK  - DOUBLE PRECISION array of dimension( NSIZES*NTYPES*NPARMS )
*           Real workspace.
*           Modified.
*
*  LLWORK - LOGICAL array of dimension( NPARMS )
*           Logical workspace.  It is used to turn on or off specific
*           lines in the output.  If LLWORK(i) is .TRUE., then row i
*           (which includes data from OPS(i,j,k) or TIMES(i,j,k) for
*           all j and k) will be printed.  If LLWORK(i) is
*           .FALSE., then row i will not be printed.
*           Modified.
*
*  NOUT   - INTEGER
*           The output unit number on which the table
*           is to be printed.  If NOUT <= 0, no output is printed.
*
*  =====================================================================
*
*     .. Local Scalars ..
      LOGICAL            LTEMP
      CHARACTER*40       FRMATA, FRMATI
      INTEGER            I, IINFO, ILINE, ILINES, IPADA, IPADI, IPAR, J,
     $                   JP, JS, JT
*     ..
*     .. External Functions ..
      DOUBLE PRECISION   DMFLOP
      EXTERNAL           DMFLOP
*     ..
*     .. External Subroutines ..
      EXTERNAL           DPRTBS
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          LEN, MAX, MIN
*     ..
*     .. Executable Statements ..
*
*
*     First line
*
      WRITE( NOUT, FMT = 9999 )SUBNAM
*
*     Set up which lines are to be printed.
*
      LLWORK( 1 ) = .TRUE.
      ILINES = 1
      DO 20 IPAR = 2, NPARMS
         LLWORK( IPAR ) = .TRUE.
         DO 10 J = 1, IPAR - 1
            LTEMP = .FALSE.
            IF( INPARM.GE.1 .AND. NP1( J ).NE.NP1( IPAR ) )
     $         LTEMP = .TRUE.
            IF( INPARM.GE.2 .AND. NP2( J ).NE.NP2( IPAR ) )
     $         LTEMP = .TRUE.
            IF( INPARM.GE.3 .AND. NP3( J ).NE.NP3( IPAR ) )
     $         LTEMP = .TRUE.
            IF( INPARM.GE.4 .AND. NP4( J ).NE.NP4( IPAR ) )
     $         LTEMP = .TRUE.
            IF( INPARM.GE.5 .AND. NP5( J ).NE.NP5( IPAR ) )
     $         LTEMP = .TRUE.
            IF( INPARM.GE.6 .AND. NP6( J ).NE.NP6( IPAR ) )
     $         LTEMP = .TRUE.
            IF( .NOT.LTEMP )
     $         LLWORK( IPAR ) = .FALSE.
   10    CONTINUE
         IF( LLWORK( IPAR ) )
     $      ILINES = ILINES + 1
   20 CONTINUE
      IF( ILINES.EQ.1 ) THEN
         IF( INPARM.EQ.1 ) THEN
            WRITE( NOUT, FMT = 9995 )PNAMES( 1 ), NP1( 1 )
         ELSE IF( INPARM.EQ.2 ) THEN
            WRITE( NOUT, FMT = 9995 )PNAMES( 1 ), NP1( 1 ),
     $         PNAMES( 2 ), NP2( 1 )
         ELSE IF( INPARM.EQ.3 ) THEN
            WRITE( NOUT, FMT = 9995 )PNAMES( 1 ), NP1( 1 ),
     $         PNAMES( 2 ), NP2( 1 ), PNAMES( 3 ), NP3( 1 )
         ELSE IF( INPARM.EQ.4 ) THEN
            WRITE( NOUT, FMT = 9995 )PNAMES( 1 ), NP1( 1 ),
     $         PNAMES( 2 ), NP2( 1 ), PNAMES( 3 ), NP3( 1 ),
     $         PNAMES( 4 ), NP4( 1 )
         ELSE IF( INPARM.EQ.5 ) THEN
            WRITE( NOUT, FMT = 9995 )PNAMES( 1 ), NP1( 1 ),
     $         PNAMES( 2 ), NP2( 1 ), PNAMES( 3 ), NP3( 1 ),
     $         PNAMES( 4 ), NP4( 1 ), PNAMES( 5 ), NP5( 1 )
         ELSE IF( INPARM.EQ.6 ) THEN
            WRITE( NOUT, FMT = 9995 )PNAMES( 1 ), NP1( 1 ),
     $         PNAMES( 2 ), NP2( 1 ), PNAMES( 3 ), NP3( 1 ),
     $         PNAMES( 4 ), NP4( 1 ), PNAMES( 5 ), NP5( 1 ),
     $         PNAMES( 6 ), NP6( 1 )
         END IF
      ELSE
         ILINE = 0
*
*        Compute output format statement.
*
         IPADI = MAX( LEN( PNAMES( 1 ) )-3, 1 )
         WRITE( FRMATI, FMT = 9993 )IPADI
         IPADA = 5 + IPADI - LEN( PNAMES( 1 ) )
         WRITE( FRMATA, FMT = 9994 )IPADA
         WRITE( NOUT, FMT = FRMATA )( PNAMES( J ), J = 1,
     $      MIN( 6, INPARM ) )
         DO 30 J = 1, NPARMS
            IF( LLWORK( J ) ) THEN
               ILINE = ILINE + 1
               IF( INPARM.EQ.1 ) THEN
                  WRITE( NOUT, FMT = FRMATI )ILINE, NP1( J )
               ELSE IF( INPARM.EQ.2 ) THEN
                  WRITE( NOUT, FMT = FRMATI )ILINE, NP1( J ), NP2( J )
               ELSE IF( INPARM.EQ.3 ) THEN
                  WRITE( NOUT, FMT = FRMATI )ILINE, NP1( J ), NP2( J ),
     $               NP3( J )
               ELSE IF( INPARM.EQ.4 ) THEN
                  WRITE( NOUT, FMT = FRMATI )ILINE, NP1( J ), NP2( J ),
     $               NP3( J ), NP4( J )
               ELSE IF( INPARM.EQ.5 ) THEN
                  WRITE( NOUT, FMT = FRMATI )ILINE, NP1( J ), NP2( J ),
     $               NP3( J ), NP4( J ), NP5( J )
               ELSE IF( INPARM.EQ.6 ) THEN
                  WRITE( NOUT, FMT = FRMATI )ILINE, NP1( J ), NP2( J ),
     $               NP3( J ), NP4( J ), NP5( J ), NP6( J )
               END IF
            END IF
   30    CONTINUE
      END IF
*
*     Execution Times
*
      WRITE( NOUT, FMT = 9996 )
      CALL DPRTBS( 'Type', 'N ', NTYPES, DOTYPE, NSIZES, NN, NPARMS,
     $             LLWORK, TIMES, LDT1, LDT2, NOUT )
*
*     Operation Counts
*
      WRITE( NOUT, FMT = 9997 )
      CALL DPRTBS( 'Type', 'N ', NTYPES, DOTYPE, NSIZES, NN, NPARMS,
     $             LLWORK, OPS, LDO1, LDO2, NOUT )
*
*     Megaflop Rates
*
      IINFO = 0
      DO 60 JS = 1, NSIZES
         DO 50 JT = 1, NTYPES
            IF( DOTYPE( JT ) ) THEN
               DO 40 JP = 1, NPARMS
                  I = JP + NPARMS*( JT-1+NTYPES*( JS-1 ) )
                  RWORK( I ) = DMFLOP( OPS( JP, JT, JS ),
     $                         TIMES( JP, JT, JS ), IINFO )
   40          CONTINUE
            END IF
   50    CONTINUE
   60 CONTINUE
*
      WRITE( NOUT, FMT = 9998 )
      CALL DPRTBS( 'Type', 'N ', NTYPES, DOTYPE, NSIZES, NN, NPARMS,
     $             LLWORK, RWORK, NPARMS, NTYPES, NOUT )
*
 9999 FORMAT( / / / ' ****** Results for ', A, ' ******' )
 9998 FORMAT( / ' *** Speed in megaflops ***' )
 9997 FORMAT( / ' *** Number of floating-point operations ***' )
 9996 FORMAT( / ' *** Time in seconds ***' )
 9995 FORMAT( 5X, : 'with ', 4( A, '=', I5, : ', ' ), / 10X,
     $      2( A, '=', I5, : ', ' ) )
*
*     Format statements for generating format statements.
*     9981 generates a string 21+2+11=34 characters long.
*     9980 generates a string 16+2+12=30 characters long.
*
 9994 FORMAT( '( 5X, : ''line '' , 6( ', I2, 'X, A, : ) )' )
 9993 FORMAT( '( 5X, : I5 , 6( ', I2, 'X, I5, : ) )' )
      RETURN
*
*     End of DPRTBG
*
      END
      SUBROUTINE DPRTBR( LAB1, LAB2, NTYPES, DOTYPE, NSIZES, MM, NN,
     $                   NPARMS, DOLINE, RESLTS, LDR1, LDR2, NOUT )
*
*  -- LAPACK timing routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     February 29, 1992
*
*     .. Scalar Arguments ..
      CHARACTER*( * )    LAB1, LAB2
      INTEGER            LDR1, LDR2, NOUT, NPARMS, NSIZES, NTYPES
*     ..
*     .. Array Arguments ..
      LOGICAL            DOLINE( NPARMS ), DOTYPE( NTYPES )
      INTEGER            MM( NSIZES ), NN( NSIZES )
      DOUBLE PRECISION   RESLTS( LDR1, LDR2, * )
*     ..
*
*  Purpose
*  =======
*
*     DPRTBR prints a table of timing data for the timing programs.
*     The table has NTYPES block rows and NSIZES columns, with NPARMS
*     individual rows in each block row.
*
*  Arguments (none are modified)
*  =========
*
*  LAB1   - CHARACTER*(*)
*           The label for the rows.
*
*  LAB2   - CHARACTER*(*)
*           The label for the columns.
*
*  NTYPES - INTEGER
*           The number of values of DOTYPE, and also the
*           number of sets of rows of the table.
*
*  DOTYPE - LOGICAL array of dimension( NTYPES )
*           If DOTYPE(j) is .TRUE., then block row j (which includes
*           data from RESLTS( i, j, k ), for all i and k) will be
*           printed.  If DOTYPE(j) is .FALSE., then block row j will
*           not be printed.
*
*  NSIZES - INTEGER
*           The number of values of NN, and also the
*           number of columns of the table.
*
*  MM   -   INTEGER array of dimension( NSIZES )
*           The values of M used to label each column.
*
*  NN   -   INTEGER array of dimension( NSIZES )
*           The values of N used to label each column.
*
*  NPARMS - INTEGER
*           The number of values of LDA, hence the
*           number of rows for each value of DOTYPE.
*
*  DOLINE - LOGICAL array of dimension( NPARMS )
*           If DOLINE(i) is .TRUE., then row i (which includes data
*           from RESLTS( i, j, k ) for all j and k) will be printed.
*           If DOLINE(i) is .FALSE., then row i will not be printed.
*
*  RESLTS - DOUBLE PRECISION array of dimension( LDR1, LDR2, NSIZES )
*           The timing results.  The first index indicates the row,
*           the second index indicates the block row, and the last
*           indicates the column.
*
*  LDR1   - INTEGER
*           The first dimension of RESLTS.  It must be at least
*           min( 1, NPARMS ).
*
*  LDR2   - INTEGER
*           The second dimension of RESLTS.  It must be at least
*           min( 1, NTYPES ).
*
*  NOUT   - INTEGER
*           The output unit number on which the table
*           is to be printed.  If NOUT <= 0, no output is printed.
*
*  =====================================================================
*
*     .. Local Scalars ..
      INTEGER            I, ILINE, J, K
*     ..
*     .. Executable Statements ..
*
      IF( NOUT.LE.0 )
     $   RETURN
      IF( NPARMS.LE.0 )
     $   RETURN
      WRITE( NOUT, FMT = 9999 )LAB2,
     $   ( MM( I ), NN( I ), I = 1, NSIZES )
      WRITE( NOUT, FMT = 9998 )LAB1
*
      DO 20 J = 1, NTYPES
         ILINE = 0
         IF( DOTYPE( J ) ) THEN
            DO 10 I = 1, NPARMS
               IF( DOLINE( I ) ) THEN
                  ILINE = ILINE + 1
                  IF( ILINE.LE.1 ) THEN
                     WRITE( NOUT, FMT = 9997 )J,
     $                  ( RESLTS( I, J, K ), K = 1, NSIZES )
                  ELSE
                     WRITE( NOUT, FMT = 9996 )( RESLTS( I, J, K ),
     $                  K = 1, NSIZES )
                  END IF
               END IF
   10       CONTINUE
            IF( ILINE.GT.1 .AND. J.LT.NTYPES )
     $         WRITE( NOUT, FMT = * )
         END IF
   20 CONTINUE
      RETURN
*
 9999 FORMAT( 7X, A4, ( 12( '(', I4, ',', I4, ')', : ) ) )
 9998 FORMAT( 3X, A4 )
 9997 FORMAT( 3X, I4, 4X, 1P, ( 12( 3X, G8.2 ) ) )
 9996 FORMAT( 11X, 1P, ( 12( 3X, G8.2 ) ) )
*
*     End of DPRTBR
*
      END
      SUBROUTINE DPRTBS( LAB1, LAB2, NTYPES, DOTYPE, NSIZES, NN, NPARMS,
     $                   DOLINE, RESLTS, LDR1, LDR2, NOUT )
*
*  -- LAPACK timing routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     February 29, 1992
*
*     .. Scalar Arguments ..
      CHARACTER*( * )    LAB1, LAB2
      INTEGER            LDR1, LDR2, NOUT, NPARMS, NSIZES, NTYPES
*     ..
*     .. Array Arguments ..
      LOGICAL            DOLINE( NPARMS ), DOTYPE( NTYPES )
      INTEGER            NN( NSIZES )
      DOUBLE PRECISION   RESLTS( LDR1, LDR2, * )
*     ..
*
*  Purpose
*  =======
*
*     DPRTBS prints a table of timing data for the timing programs.
*     The table has NTYPES block rows and NSIZES columns, with NPARMS
*     individual rows in each block row.
*
*  Arguments (none are modified)
*  =========
*
*  LAB1   - CHARACTER*(*)
*           The label for the rows.
*
*  LAB2   - CHARACTER*(*)
*           The label for the columns.
*
*  NTYPES - INTEGER
*           The number of values of DOTYPE, and also the
*           number of sets of rows of the table.
*
*  DOTYPE - LOGICAL array of dimension( NTYPES )
*           If DOTYPE(j) is .TRUE., then block row j (which includes
*           data from RESLTS( i, j, k ), for all i and k) will be
*           printed.  If DOTYPE(j) is .FALSE., then block row j will
*           not be printed.
*
*  NSIZES - INTEGER
*           The number of values of NN, and also the
*           number of columns of the table.
*
*  NN   -   INTEGER array of dimension( NSIZES )
*           The values of N used to label each column.
*
*  NPARMS - INTEGER
*           The number of values of LDA, hence the
*           number of rows for each value of DOTYPE.
*
*  DOLINE - LOGICAL array of dimension( NPARMS )
*           If DOLINE(i) is .TRUE., then row i (which includes data
*           from RESLTS( i, j, k ) for all j and k) will be printed.
*           If DOLINE(i) is .FALSE., then row i will not be printed.
*
*  RESLTS - DOUBLE PRECISION array of dimension( LDR1, LDR2, NSIZES )
*           The timing results.  The first index indicates the row,
*           the second index indicates the block row, and the last
*           indicates the column.
*
*  LDR1   - INTEGER
*           The first dimension of RESLTS.  It must be at least
*           min( 1, NPARMS ).
*
*  LDR2   - INTEGER
*           The second dimension of RESLTS.  It must be at least
*           min( 1, NTYPES ).
*
*  NOUT   - INTEGER
*           The output unit number on which the table
*           is to be printed.  If NOUT <= 0, no output is printed.
*
*  =====================================================================
*
*     .. Local Scalars ..
      INTEGER            I, ILINE, J, K
*     ..
*     .. Executable Statements ..
*
      IF( NOUT.LE.0 )
     $   RETURN
      IF( NPARMS.LE.0 )
     $   RETURN
      WRITE( NOUT, FMT = 9999 )LAB2, ( NN( I ), I = 1, NSIZES )
      WRITE( NOUT, FMT = 9998 )LAB1
*
      DO 20 J = 1, NTYPES
         ILINE = 0
         IF( DOTYPE( J ) ) THEN
            DO 10 I = 1, NPARMS
               IF( DOLINE( I ) ) THEN
                  ILINE = ILINE + 1
                  IF( ILINE.LE.1 ) THEN
                     WRITE( NOUT, FMT = 9997 )J,
     $                  ( RESLTS( I, J, K ), K = 1, NSIZES )
                  ELSE
                     WRITE( NOUT, FMT = 9996 )( RESLTS( I, J, K ),
     $                  K = 1, NSIZES )
                  END IF
               END IF
   10       CONTINUE
            IF( ILINE.GT.1 .AND. J.LT.NTYPES )
     $         WRITE( NOUT, FMT = * )
         END IF
   20 CONTINUE
      RETURN
*
 9999 FORMAT( 6X, A4, I6, 11I9 )
 9998 FORMAT( 3X, A4 )
 9997 FORMAT( 3X, I4, 4X, 1P, 12( 1X, G8.2 ) )
 9996 FORMAT( 11X, 1P, 12( 1X, G8.2 ) )
*
*     End of DPRTBS
*
      END
      SUBROUTINE DPRTBV( SUBNAM, NTYPES, DOTYPE, NSIZES, MM, NN, INPARM,
     $                   PNAMES, NPARMS, NP1, NP2, OPS, LDO1, LDO2,
     $                   TIMES, LDT1, LDT2, RWORK, LLWORK, NOUT )
*
*  -- LAPACK timing routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     February 29, 1992
*
*     .. Scalar Arguments ..
      CHARACTER*( * )    SUBNAM
      INTEGER            INPARM, LDO1, LDO2, LDT1, LDT2, NOUT, NPARMS,
     $                   NSIZES, NTYPES
*     ..
*     .. Array Arguments ..
      LOGICAL            DOTYPE( NTYPES ), LLWORK( NPARMS )
      CHARACTER*( * )    PNAMES( * )
      INTEGER            MM( NSIZES ), NN( NSIZES ), NP1( * ), NP2( * )
      DOUBLE PRECISION   OPS( LDO1, LDO2, * ), RWORK( * ),
     $                   TIMES( LDT1, LDT2, * )
*     ..
*
*  Purpose
*  =======
*
*     DPRTBV prints out timing information for the eigenvalue routines.
*     The table has NTYPES block rows and NSIZES columns, with NPARMS
*     individual rows in each block row.  There are INPARM quantities
*     which depend on rows (currently, INPARM <= 4).
*
*  Arguments (none are modified)
*  =========
*
*  SUBNAM - CHARACTER*(*)
*           The label for the output.
*
*  NTYPES - INTEGER
*           The number of values of DOTYPE, and also the
*           number of sets of rows of the table.
*
*  DOTYPE - LOGICAL array of dimension( NTYPES )
*           If DOTYPE(j) is .TRUE., then block row j (which includes
*           data from RESLTS( i, j, k ), for all i and k) will be
*           printed.  If DOTYPE(j) is .FALSE., then block row j will
*           not be printed.
*
*  NSIZES - INTEGER
*           The number of values of NN, and also the
*           number of columns of the table.
*
*  MM   -   INTEGER array of dimension( NSIZES )
*           The values of M used to label each column.
*
*  NN   -   INTEGER array of dimension( NSIZES )
*           The values of N used to label each column.
*
*  INPARM - INTEGER
*           The number of different parameters which are functions of
*           the row number.  At the moment, INPARM <= 4.
*
*  PNAMES - CHARACTER*(*) array of dimension( INPARM )
*           The label for the columns.
*
*  NPARMS - INTEGER
*           The number of values for each "parameter", i.e., the
*           number of rows for each value of DOTYPE.
*
*  NP1    - INTEGER array of dimension( NPARMS )
*           The first quantity which depends on row number.
*
*  NP2    - INTEGER array of dimension( NPARMS )
*           The second quantity which depends on row number.
*
*  OPS    - DOUBLE PRECISION array of dimension( LDT1, LDT2, NSIZES )
*           The operation counts.  The first index indicates the row,
*           the second index indicates the block row, and the last
*           indicates the column.
*
*  LDO1   - INTEGER
*           The first dimension of OPS.  It must be at least
*           min( 1, NPARMS ).
*
*  LDO2   - INTEGER
*           The second dimension of OPS.  It must be at least
*           min( 1, NTYPES ).
*
*  TIMES  - DOUBLE PRECISION array of dimension( LDT1, LDT2, NSIZES )
*           The times (in seconds).  The first index indicates the row,
*           the second index indicates the block row, and the last
*           indicates the column.
*
*  LDT1   - INTEGER
*           The first dimension of RESLTS.  It must be at least
*           min( 1, NPARMS ).
*
*  LDT2   - INTEGER
*           The second dimension of RESLTS.  It must be at least
*           min( 1, NTYPES ).
*
*  RWORK  - DOUBLE PRECISION array of dimension( NSIZES*NTYPES*NPARMS )
*           Real workspace.
*           Modified.
*
*  LLWORK - LOGICAL array of dimension( NPARMS )
*           Logical workspace.  It is used to turn on or off specific
*           lines in the output.  If LLWORK(i) is .TRUE., then row i
*           (which includes data from OPS(i,j,k) or TIMES(i,j,k) for
*           all j and k) will be printed.  If LLWORK(i) is
*           .FALSE., then row i will not be printed.
*           Modified.
*
*  NOUT   - INTEGER
*           The output unit number on which the table
*           is to be printed.  If NOUT <= 0, no output is printed.
*
*  =====================================================================
*
*     .. Local Scalars ..
      LOGICAL            LTEMP
      INTEGER            I, IINFO, ILINE, ILINES, IPAR, J, JP, JS, JT
*     ..
*     .. External Functions ..
      DOUBLE PRECISION   DMFLOP
      EXTERNAL           DMFLOP
*     ..
*     .. External Subroutines ..
      EXTERNAL           DPRTBR
*     ..
*     .. Executable Statements ..
*
*
*     First line
*
      WRITE( NOUT, FMT = 9999 )SUBNAM
*
*     Set up which lines are to be printed.
*
      LLWORK( 1 ) = .TRUE.
      ILINES = 1
      DO 20 IPAR = 2, NPARMS
         LLWORK( IPAR ) = .TRUE.
         DO 10 J = 1, IPAR - 1
            LTEMP = .FALSE.
            IF( INPARM.GE.1 .AND. NP1( J ).NE.NP1( IPAR ) )
     $         LTEMP = .TRUE.
            IF( INPARM.GE.2 .AND. NP2( J ).NE.NP2( IPAR ) )
     $         LTEMP = .TRUE.
            IF( .NOT.LTEMP )
     $         LLWORK( IPAR ) = .FALSE.
   10    CONTINUE
         IF( LLWORK( IPAR ) )
     $      ILINES = ILINES + 1
   20 CONTINUE
      IF( ILINES.EQ.1 ) THEN
         IF( INPARM.EQ.1 ) THEN
            WRITE( NOUT, FMT = 9995 )PNAMES( 1 ), NP1( 1 )
         ELSE IF( INPARM.EQ.2 ) THEN
            WRITE( NOUT, FMT = 9995 )PNAMES( 1 ), NP1( 1 ),
     $         PNAMES( 2 ), NP2( 1 )
         END IF
      ELSE
         ILINE = 0
         DO 30 J = 1, NPARMS
            IF( LLWORK( J ) ) THEN
               ILINE = ILINE + 1
               IF( INPARM.EQ.1 ) THEN
                  WRITE( NOUT, FMT = 9994 )ILINE, PNAMES( 1 ), NP1( J )
               ELSE IF( INPARM.EQ.2 ) THEN
                  WRITE( NOUT, FMT = 9994 )ILINE, PNAMES( 1 ),
     $               NP1( J ), PNAMES( 2 ), NP2( J )
               END IF
            END IF
   30    CONTINUE
      END IF
*
*     Execution Times
*
      WRITE( NOUT, FMT = 9996 )
      CALL DPRTBR( 'Type', 'M,N ', NTYPES, DOTYPE, NSIZES, MM, NN,
     $             NPARMS, LLWORK, TIMES, LDT1, LDT2, NOUT )
*
*     Operation Counts
*
      WRITE( NOUT, FMT = 9997 )
      CALL DPRTBR( 'Type', 'M,N ', NTYPES, DOTYPE, NSIZES, MM, NN,
     $             NPARMS, LLWORK, OPS, LDO1, LDO2, NOUT )
*
*     Megaflop Rates
*
      IINFO = 0
      DO 60 JS = 1, NSIZES
         DO 50 JT = 1, NTYPES
            IF( DOTYPE( JT ) ) THEN
               DO 40 JP = 1, NPARMS
                  I = JP + NPARMS*( JT-1+NTYPES*( JS-1 ) )
                  RWORK( I ) = DMFLOP( OPS( JP, JT, JS ),
     $                         TIMES( JP, JT, JS ), IINFO )
   40          CONTINUE
            END IF
   50    CONTINUE
   60 CONTINUE
*
      WRITE( NOUT, FMT = 9998 )
      CALL DPRTBR( 'Type', 'M,N ', NTYPES, DOTYPE, NSIZES, MM, NN,
     $             NPARMS, LLWORK, RWORK, NPARMS, NTYPES, NOUT )
*
 9999 FORMAT( / / / ' ****** Results for ', A, ' ******' )
 9998 FORMAT( / ' *** Speed in megaflops ***' )
 9997 FORMAT( / ' *** Number of floating-point operations ***' )
 9996 FORMAT( / ' *** Time in seconds ***' )
 9995 FORMAT( 5X, : 'with ', A, '=', I5, 3( : ', ', A, '=', I5 ) )
 9994 FORMAT( 5X, : 'line ', I2, ' with ', A, '=', I5,
     $      3( : ', ', A, '=', I5 ) )
      RETURN
*
*     End of DPRTBV
*
      END
      SUBROUTINE DTIM21( LINE, NSIZES, NN, NTYPES, DOTYPE, NPARMS, NNB,
     $                   NSHFTS, MAXBS, LDAS, TIMMIN, NOUT, ISEED, A, H,
     $                   Z, W, WORK, LWORK, LLWORK, IWORK, TIMES, LDT1,
     $                   LDT2, LDT3, OPCNTS, LDO1, LDO2, LDO3, INFO )
*
*  -- LAPACK timing routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     September 30, 1994
*
*     .. Scalar Arguments ..
      CHARACTER*80       LINE
      INTEGER            INFO, LDO1, LDO2, LDO3, LDT1, LDT2, LDT3,
     $                   LWORK, NOUT, NPARMS, NSIZES, NTYPES
      DOUBLE PRECISION   TIMMIN
*     ..
*     .. Array Arguments ..
      LOGICAL            DOTYPE( * ), LLWORK( * )
      INTEGER            ISEED( * ), IWORK( * ), LDAS( * ), MAXBS( * ),
     $                   NN( * ), NNB( * ), NSHFTS( * )
      DOUBLE PRECISION   A( * ), H( * ), OPCNTS( LDO1, LDO2, LDO3, * ),
     $                   TIMES( LDT1, LDT2, LDT3, * ), W( * ),
     $                   WORK( * ), Z( * )
*     ..
*
*  Purpose
*  =======
*
*     DTIM21 times the LAPACK routines for the DOUBLE PRECISION
*     non-symmetric eigenvalue problem.
*
*     For each N value in NN(1:NSIZES) and .TRUE. value in
*     DOTYPE(1:NTYPES), a matrix will be generated and used to test the
*     selected routines.  Thus, NSIZES*(number of .TRUE. values in
*     DOTYPE) matrices will be generated.
*
*  Arguments
*  =========
*
*  LINE    (input) CHARACTER*80
*          On entry, LINE contains the input line which requested
*          this routine.  This line may contain a subroutine name,
*          such as DGEHRD, indicating that only routine SGEHRD will
*          be timed, or it may contain a generic name, such as DHS.
*          In this case, the rest of the line is scanned for the
*          first 12 non-blank characters, corresponding to the twelve
*          combinations of subroutine and options:
*          LAPACK:
*          1: DGEHRD
*          2: DHSEQR(JOB='E')
*          3: DHSEQR(JOB='S')
*          4: DHSEQR(JOB='I')
*          5: DTREVC(JOB='L')
*          6: DTREVC(JOB='R')
*          7: DHSEIN(JOB='L')
*          8: DHSEIN(JOB='R')
*          EISPACK:
*           9: ORTHES (compare with DGEHRD)
*          10: HQR    (compare w/ DHSEQR -- JOB='E')
*          11: HQR2   (compare w/ DHSEQR(JOB='I') plus DTREVC(JOB='R'))
*          12: INVIT  (compare with DHSEIN)
*          If a character is 'T' or 't', the corresponding routine in
*          this path is timed.  If the entire line is blank, all the
*          routines in the path are timed.
*
*  NSIZES  (input) INTEGER
*          The number of values of N contained in the vector NN.
*
*  NN      (input) INTEGER array, dimension( NSIZES )
*          The values of the matrix size N to be tested.  For each
*          N value in the array NN, and each .TRUE. value in DOTYPE,
*          a matrix A will be generated and used to test the routines.
*
*  NTYPES  (input) INTEGER
*          The number of types in DOTYPE.  Only the first MAXTYP
*          elements will be examined.  Exception: if NSIZES=1 and
*          NTYPES=MAXTYP+1, and DOTYPE=MAXTYP*f,t, then the input
*          value of A will be used.
*
*  DOTYPE  (input) LOGICAL
*          If DOTYPE(j) is .TRUE., then a matrix of type j will be
*          generated.  The matrix A has the form X**(-1) T X, where
*          X is orthogonal (for j=1--4) or has condition sqrt(ULP)
*          (for j=5--8), and T has random O(1) entries in the upper
*          triangle and:
*          (j=1,5) evenly spaced entries 1, ..., ULP with random signs
*          (j=2,6) geometrically spaced entries 1, ..., ULP with random
*                  signs
*          (j=3,7) "clustered" entries 1, ULP,..., ULP with random
*                  signs
*          (j=4,8) real or complex conjugate paired eigenvalues
*                  randomly chosen from ( ULP, 1 )
*          on the diagonal.
*
*  NPARMS  (input) INTEGER
*          The number of values in each of the arrays NNB, NSHFTS,
*          MAXBS, and LDAS.  For each matrix A generated according to
*          NN and DOTYPE, tests will be run with (NB,NSHIFT,MAXB,LDA)=
*          (NNB(1), NSHFTS(1), MAXBS(1), LDAS(1)),...,
*          (NNB(NPARMS), NSHFTS(NPARMS), MAXBS(NPARMS), LDAS(NPARMS))
*
*  NNB     (input) INTEGER array, dimension( NPARMS )
*          The values of the blocksize ("NB") to be tested.
*
*  NSHFTS  (input) INTEGER array, dimension( NPARMS )
*          The values of the number of shifts ("NSHIFT") to be tested.
*
*  MAXBS   (input) INTEGER array, dimension( NPARMS )
*          The values of "MAXB", the size of largest submatrix to be
*          processed by DLAHQR (EISPACK method), to be tested.
*
*  LDAS    (input) INTEGER array, dimension( NPARMS )
*          The values of LDA, the leading dimension of all matrices,
*          to be tested.
*
*  TIMMIN  (input) DOUBLE PRECISION
*          The minimum time a subroutine will be timed.
*
*  NOUT    (input) INTEGER
*          If NOUT > 0 then NOUT specifies the unit number
*          on which the output will be printed.  If NOUT <= 0, no
*          output is printed.
*
*  ISEED   (input/output) INTEGER array, dimension( 4 )
*          The random seed used by the random number generator, used
*          by the test matrix generator.  It is used and updated on
*          each call to DTIM21
*
*  A       (workspace) DOUBLE PRECISION array,
*                      dimension( max(NN)*max(LDAS) )
*          (a) During the testing of DGEHRD, the original matrix to
*              be tested.
*          (b) Later, the Schur form of the original matrix.
*
*  H       (workspace) DOUBLE PRECISION array,
*                      dimension( max(NN)*max(LDAS) )
*          The Hessenberg form of the original matrix.
*
*  Z       (workspace) DOUBLE PRECISION array,
*                      dimension( max(NN)*max(LDAS) )
*          Various output arrays: from DGEHRD and DHSEQR, the
*          orthogonal reduction matrices; from DTREVC and DHSEIN,
*          the eigenvector matrices.
*
*  W       (workspace) DOUBLE PRECISION array,
*                      dimension( 2*max(LDAS) )
*          Treated as an LDA x 2 matrix whose 1st column holds WR, the
*          real parts of the eigenvalues, and whose 2nd column holds
*          WI, the imaginary parts of the eigenvalues of A.
*
*  WORK    (workspace) DOUBLE PRECISION array, dimension( LWORK )
*
*  LWORK   (input) INTEGER
*          Number of elements in WORK.  It must be at least
*          (a)  max(NN)*( 3*max(NNB) + 2 )
*          (b)  max(NN)*( max(NNB+NSHFTS) + 1 )
*          (c)  max(NSHFTS)*( max(NSHFTS) + max(NN) )
*          (d)  max(MAXBS)*( max(MAXBS) + max(NN) )
*          (e)  ( max(NN) + 2 )**2  +  max(NN)
*          (f)  NSIZES*NTYPES*NPARMS
*
*  LLWORK  (workspace) LOGICAL array, dimension( max( max(NN), NPARMS ))
*
*  IWORK   (workspace) INTEGER array, dimension( 2*max(NN) )
*          Workspace needed for parameters IFAILL and IFAILR in call
*          to DHSEIN.
*
*  TIMES   (output) DOUBLE PRECISION array,
*                   dimension (LDT1,LDT2,LDT3,NSUBS)
*          TIMES(i,j,k,l) will be set to the run time (in seconds) for
*          subroutine l, with N=NN(k), matrix type j, and LDA=LDAS(i),
*          MAXB=MAXBS(i), NBLOCK=NNB(i), and NSHIFT=NSHFTS(i).
*
*  LDT1    (input) INTEGER
*          The first dimension of TIMES.  LDT1 >= min( 1, NPARMS ).
*
*  LDT2    (input) INTEGER
*          The second dimension of TIMES.  LDT2 >= min( 1, NTYPES ).
*
*  LDT3    (input) INTEGER
*          The third dimension of TIMES.  LDT3 >= min( 1, NSIZES ).
*
*  OPCNTS  (output) DOUBLE PRECISION array,
*                   dimension (LDO1,LDO2,LDO3,NSUBS)
*          OPCNTS(i,j,k,l) will be set to the number of floating-point
*          operations executed by subroutine l, with N=NN(k), matrix
*          type j, and LDA=LDAS(i), MAXB=MAXBS(i), NBLOCK=NNB(i), and
*          NSHIFT=NSHFTS(i).
*
*  LDO1    (input) INTEGER
*          The first dimension of OPCNTS.  LDO1 >= min( 1, NPARMS ).
*
*  LDO2    (input) INTEGER
*          The second dimension of OPCNTS.  LDO2 >= min( 1, NTYPES ).
*
*  LDO3    (input) INTEGER
*          The third dimension of OPCNTS.  LDO3 >= min( 1, NSIZES ).
*
*  INFO    (output) INTEGER
*          Error flag.  It will be set to zero if no error occurred.
*
*  =====================================================================
*
*     .. Parameters ..
      INTEGER            MAXTYP, NSUBS
      PARAMETER          ( MAXTYP = 8, NSUBS = 12 )
      DOUBLE PRECISION   ZERO, ONE
      PARAMETER          ( ZERO = 0.0D0, ONE = 1.0D0 )
*     ..
*     .. Local Scalars ..
      LOGICAL            RUNHQR, RUNHRD, RUNORT, RUNQRE, RUNQRS
      INTEGER            IC, ICONDS, IINFO, IMODE, IN, IPAR, ISUB,
     $                   ITEMP, ITYPE, J, J1, J2, J3, J4, JC, JR, LASTL,
     $                   LASTNL, LDA, LDAMIN, LDH, LDT, LDW, MAXB,
     $                   MBMAX, MTYPES, N, NB, NBMAX, NMAX, NSBMAX,
     $                   NSHIFT, NSMAX
      DOUBLE PRECISION   CONDS, RTULP, RTULPI, S1, S2, TIME, ULP,
     $                   ULPINV, UNTIME
*     ..
*     .. Local Arrays ..
      LOGICAL            TIMSUB( NSUBS )
      CHARACTER          ADUMMA( 1 )
      CHARACTER*4        PNAMES( 4 )
      CHARACTER*9        SUBNAM( NSUBS )
      INTEGER            INPARM( NSUBS ), IOLDSD( 4 ), KCONDS( MAXTYP ),
     $                   KMODE( MAXTYP )
*     ..
*     .. External Functions ..
      DOUBLE PRECISION   DLAMCH, DOPLA, DSECND
      EXTERNAL           DLAMCH, DOPLA, DSECND
*     ..
*     .. External Subroutines ..
      EXTERNAL           ATIMIN, DGEHRD, DHSEIN, DHSEQR, DLACPY, DLASET,
     $                   DLATME, DPRTBE, DTREVC, HQR, HQR2, INVIT,
     $                   ORTHES, XLAENV
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, DBLE, MAX, MIN, SQRT
*     ..
*     .. Scalars in Common ..
      DOUBLE PRECISION   ITCNT, OPS
*     ..
*     .. Common blocks ..
      COMMON             / LATIME / OPS, ITCNT
*     ..
*     .. Data statements ..
      DATA               SUBNAM / 'DGEHRD', 'DHSEQR(E)', 'DHSEQR(S)',
     $                   'DHSEQR(V)', 'DTREVC(L)', 'DTREVC(R)',
     $                   'DHSEIN(L)', 'DHSEIN(R)', 'ORTHES', 'HQR',
     $                   'HQR2', 'INVIT' /
      DATA               INPARM / 2, 4, 4, 4, 1, 1, 1, 1, 1, 1, 1, 1 /
      DATA               PNAMES / 'LDA', 'NB', 'NS', 'MAXB' /
      DATA               KMODE / 4, 3, 1, 5, 4, 3, 1, 5 /
      DATA               KCONDS / 4*1, 4*2 /
*     ..
*     .. Executable Statements ..
*
*     Quick Return
*
      INFO = 0
      IF( NSIZES.LE.0 .OR. NTYPES.LE.0 .OR. NPARMS.LE.0 )
     $   RETURN
*
*     Extract the timing request from the input line.
*
      CALL ATIMIN( 'DHS', LINE, NSUBS, SUBNAM, TIMSUB, NOUT, INFO )
      IF( INFO.NE.0 )
     $   RETURN
*
*     Compute Maximum Values
*
      NMAX = 0
      DO 10 J1 = 1, NSIZES
         NMAX = MAX( NMAX, NN( J1 ) )
   10 CONTINUE
*
      LDAMIN = 2*MAX( 1, NMAX )
      NBMAX = 0
      NSMAX = 0
      MBMAX = 0
      NSBMAX = 0
      DO 20 J1 = 1, NPARMS
         LDAMIN = MIN( LDAMIN, LDAS( J1 ) )
         NBMAX = MAX( NBMAX, NNB( J1 ) )
         NSMAX = MAX( NSMAX, NSHFTS( J1 ) )
         MBMAX = MAX( MBMAX, MAXBS( J1 ) )
         NSBMAX = MAX( NSBMAX, NNB( J1 )+NSHFTS( J1 ) )
   20 CONTINUE
*
*     Check that N <= LDA for the input values.
*
      IF( NMAX.GT.LDAMIN ) THEN
         INFO = -10
         WRITE( NOUT, FMT = 9999 )LINE( 1: 6 )
 9999    FORMAT( 1X, A, ' timing run not attempted -- N > LDA', / )
         RETURN
      END IF
*
*     Check LWORK
*
      IF( LWORK.LT.MAX( NMAX*MAX( 3*NBMAX+2, NSBMAX+1 ),
     $    NSMAX*( NSMAX+NMAX ), MBMAX*( MBMAX+NMAX ),
     $    ( NMAX+1 )*( NMAX+4 ), NSIZES*NTYPES*NPARMS ) ) THEN
         INFO = -19
         WRITE( NOUT, FMT = 9998 )LINE( 1: 6 )
 9998    FORMAT( 1X, A, ' timing run not attempted -- LWORK too small.',
     $         / )
         RETURN
      END IF
*
*     Check to see whether DGEHRD or DHSEQR must be run.
*
*     RUNQRE -- if DHSEQR must be run to get eigenvalues.
*     RUNQRS -- if DHSEQR must be run to get Schur form.
*     RUNHRD -- if DGEHRD must be run.
*
      RUNQRS = .FALSE.
      RUNQRE = .FALSE.
      RUNHRD = .FALSE.
      IF( TIMSUB( 5 ) .OR. TIMSUB( 6 ) )
     $   RUNQRS = .TRUE.
      IF( ( TIMSUB( 7 ) .OR. TIMSUB( 8 ) ) )
     $   RUNQRE = .TRUE.
      IF( TIMSUB( 2 ) .OR. TIMSUB( 3 ) .OR. TIMSUB( 4 ) .OR. RUNQRS .OR.
     $    RUNQRE )RUNHRD = .TRUE.
      IF( TIMSUB( 3 ) .OR. TIMSUB( 4 ) .OR. RUNQRS )
     $   RUNQRE = .FALSE.
      IF( TIMSUB( 4 ) )
     $   RUNQRS = .FALSE.
*
*     Check to see whether ORTHES or HQR must be run.
*
*     RUNHQR -- if HQR must be run to get eigenvalues.
*     RUNORT -- if ORTHES must be run.
*
      RUNHQR = .FALSE.
      RUNORT = .FALSE.
      IF( TIMSUB( 12 ) )
     $   RUNHQR = .TRUE.
      IF( TIMSUB( 10 ) .OR. TIMSUB( 11 ) .OR. RUNHQR )
     $   RUNORT = .TRUE.
      IF( TIMSUB( 10 ) .OR. TIMSUB( 11 ) )
     $   RUNHQR = .FALSE.
      IF( TIMSUB( 9 ) )
     $   RUNORT = .FALSE.
*
*     Various Constants
*
      ULP = DLAMCH( 'Epsilon' )*DLAMCH( 'Base' )
      ULPINV = ONE / ULP
      RTULP = SQRT( ULP )
      RTULPI = ONE / RTULP
*
*     Zero out OPCNTS, TIMES
*
      DO 60 J4 = 1, NSUBS
         DO 50 J3 = 1, NSIZES
            DO 40 J2 = 1, NTYPES
               DO 30 J1 = 1, NPARMS
                  OPCNTS( J1, J2, J3, J4 ) = ZERO
                  TIMES( J1, J2, J3, J4 ) = ZERO
   30          CONTINUE
   40       CONTINUE
   50    CONTINUE
   60 CONTINUE
*
*     Do for each value of N:
*
      DO 620 IN = 1, NSIZES
*
         N = NN( IN )
*
*        Do for each .TRUE. value in DOTYPE:
*
         MTYPES = MIN( MAXTYP, NTYPES )
         IF( NTYPES.EQ.MAXTYP+1 .AND. NSIZES.EQ.1 )
     $      MTYPES = NTYPES
         DO 610 ITYPE = 1, MTYPES
            IF( .NOT.DOTYPE( ITYPE ) )
     $         GO TO 610
*
*           Save random number seed for error messages
*
            DO 70 J = 1, 4
               IOLDSD( J ) = ISEED( J )
   70       CONTINUE
*
*-----------------------------------------------------------------------
*
*           Time the LAPACK Routines
*
*           Generate A
*
            IF( ITYPE.LE.MAXTYP ) THEN
               IMODE = KMODE( ITYPE )
               ICONDS = KCONDS( ITYPE )
               IF( ICONDS.EQ.1 ) THEN
                  CONDS = ONE
               ELSE
                  CONDS = RTULPI
               END IF
               ADUMMA( 1 ) = ' '
               CALL DLATME( N, 'S', ISEED, WORK, IMODE, ULPINV, ONE,
     $                      ADUMMA, 'T', 'T', 'T', WORK( N+1 ), 4,
     $                      CONDS, N, N, ONE, A, N, WORK( 2*N+1 ),
     $                      IINFO )
            END IF
*
*           Time DGEHRD for each pair NNB(j), LDAS(j)
*
            IF( TIMSUB( 1 ) ) THEN
               DO 110 IPAR = 1, NPARMS
                  LDA = LDAS( IPAR )
                  NB = MIN( N, NNB( IPAR ) )
*
*                 If this combination of (NB,LDA) has occurred before,
*                 just use that value.
*
                  LASTNL = 0
                  DO 80 J = 1, IPAR - 1
                     IF( LDA.EQ.LDAS( J ) .AND. NB.EQ.
     $                   MIN( N, NNB( J ) ) )LASTNL = J
   80             CONTINUE
*
                  IF( LASTNL.EQ.0 ) THEN
                     CALL XLAENV( 1, NB )
                     CALL XLAENV( 2, 2 )
                     CALL XLAENV( 3, NB )
*
*                    Time DGEHRD
*
                     IC = 0
                     OPS = ZERO
                     S1 = DSECND( )
   90                CONTINUE
                     CALL DLACPY( 'Full', N, N, A, N, H, LDA )
*
                     CALL DGEHRD( N, 1, N, H, LDA, WORK, WORK( N+1 ),
     $                            LWORK-N, IINFO )
*
                     IF( IINFO.NE.0 ) THEN
                        WRITE( NOUT, FMT = 9997 )SUBNAM( 1 ), IINFO, N,
     $                     ITYPE, IPAR, IOLDSD
                        INFO = ABS( IINFO )
                        GO TO 610
                     END IF
*
                     S2 = DSECND( )
                     TIME = S2 - S1
                     IC = IC + 1
                     IF( TIME.LT.TIMMIN )
     $                  GO TO 90
*
*                    Subtract the time used in DLACPY.
*
                     S1 = DSECND( )
                     DO 100 J = 1, IC
                        CALL DLACPY( 'Full', N, N, A, N, Z, LDA )
  100                CONTINUE
                     S2 = DSECND( )
                     UNTIME = S2 - S1
*
                     TIMES( IPAR, ITYPE, IN, 1 ) = MAX( TIME-UNTIME,
     $                  ZERO ) / DBLE( IC )
                     OPCNTS( IPAR, ITYPE, IN, 1 ) = DOPLA( 'DGEHRD', N,
     $                  1, N, 0, NB )
                  ELSE
                     OPCNTS( IPAR, ITYPE, IN, 1 ) = OPCNTS( LASTNL,
     $                  ITYPE, IN, 1 )
                     TIMES( IPAR, ITYPE, IN, 1 ) = TIMES( LASTNL, ITYPE,
     $                  IN, 1 )
                  END IF
  110          CONTINUE
               LDH = LDA
            ELSE
               IF( RUNHRD ) THEN
                  CALL DLACPY( 'Full', N, N, A, N, H, N )
*
                  CALL DGEHRD( N, 1, N, H, N, WORK, WORK( N+1 ),
     $                         LWORK-N, IINFO )
*
                  IF( IINFO.NE.0 ) THEN
                     WRITE( NOUT, FMT = 9997 )SUBNAM( 1 ), IINFO, N,
     $                  ITYPE, 0, IOLDSD
                     INFO = ABS( IINFO )
                     GO TO 610
                  END IF
                  LDH = N
               END IF
            END IF
*
*           Time DHSEQR with JOB='E' for each 4-tuple
*           NNB(j), NSHFTS(j), MAXBS(j), LDAS(j)
*
            IF( TIMSUB( 2 ) ) THEN
               DO 140 IPAR = 1, NPARMS
                  LDA = LDAS( IPAR )
                  NB = 1
                  NSHIFT = NSHFTS( IPAR )
                  MAXB = MAXBS( IPAR )
                  CALL XLAENV( 4, NSHIFT )
                  CALL XLAENV( 8, MAXB )
*
*                 Time DHSEQR with JOB='E'
*
                  IC = 0
                  OPS = ZERO
                  S1 = DSECND( )
  120             CONTINUE
                  CALL DLACPY( 'Full', N, N, H, LDH, A, LDA )
*
                  CALL DHSEQR( 'E', 'N', N, 1, N, A, LDA, W, W( LDA+1 ),
     $                         Z, LDA, WORK, LWORK, IINFO )
*
                  IF( IINFO.NE.0 ) THEN
                     WRITE( NOUT, FMT = 9997 )SUBNAM( 2 ), IINFO, N,
     $                  ITYPE, IPAR, IOLDSD
                     INFO = ABS( IINFO )
                     GO TO 610
                  END IF
*
                  S2 = DSECND( )
                  TIME = S2 - S1
                  IC = IC + 1
                  IF( TIME.LT.TIMMIN )
     $               GO TO 120
*
*                 Subtract the time used in DLACPY.
*
                  S1 = DSECND( )
                  DO 130 J = 1, IC
                     CALL DLACPY( 'Full', N, N, H, LDH, Z, LDA )
  130             CONTINUE
                  S2 = DSECND( )
                  UNTIME = S2 - S1
*
                  TIMES( IPAR, ITYPE, IN, 2 ) = MAX( TIME-UNTIME,
     $               ZERO ) / DBLE( IC )
                  OPCNTS( IPAR, ITYPE, IN, 2 ) = OPS / DBLE( IC )
  140          CONTINUE
               LDT = 0
               LDW = LDA
            ELSE
               IF( RUNQRE ) THEN
                  CALL DLACPY( 'Full', N, N, H, LDH, A, N )
*
                  CALL DHSEQR( 'E', 'N', N, 1, N, A, N, W, W( N+1 ), Z,
     $                         N, WORK, LWORK, IINFO )
*
                  IF( IINFO.NE.0 ) THEN
                     WRITE( NOUT, FMT = 9997 )SUBNAM( 2 ), IINFO, N,
     $                  ITYPE, 0, IOLDSD
                     INFO = ABS( IINFO )
                     GO TO 610
                  END IF
                  LDT = 0
                  LDW = N
               END IF
            END IF
*
*           Time DHSEQR with JOB='S' for each 4-tuple
*           NNB(j), NSHFTS(j), MAXBS(j), LDAS(j)
*
            IF( TIMSUB( 3 ) ) THEN
               DO 170 IPAR = 1, NPARMS
                  LDA = LDAS( IPAR )
                  NSHIFT = NSHFTS( IPAR )
                  MAXB = MAXBS( IPAR )
                  NB = 1
                  CALL XLAENV( 4, NSHIFT )
                  CALL XLAENV( 8, MAXB )
*
*                 Time DHSEQR with JOB='S'
*
                  IC = 0
                  OPS = ZERO
                  S1 = DSECND( )
  150             CONTINUE
                  CALL DLACPY( 'Full', N, N, H, LDH, A, LDA )
*
                  CALL DHSEQR( 'S', 'N', N, 1, N, A, LDA, W, W( LDA+1 ),
     $                         Z, LDA, WORK, LWORK, IINFO )
*
                  IF( IINFO.NE.0 ) THEN
                     WRITE( NOUT, FMT = 9997 )SUBNAM( 3 ), IINFO, N,
     $                  ITYPE, IPAR, IOLDSD
                     INFO = ABS( IINFO )
                     GO TO 610
                  END IF
*
                  S2 = DSECND( )
                  TIME = S2 - S1
                  IC = IC + 1
                  IF( TIME.LT.TIMMIN )
     $               GO TO 150
*
*                 Subtract the time used in DLACPY.
*
                  S1 = DSECND( )
                  DO 160 J = 1, IC
                     CALL DLACPY( 'Full', N, N, H, LDH, Z, LDA )
  160             CONTINUE
                  S2 = DSECND( )
                  UNTIME = S2 - S1
*
                  TIMES( IPAR, ITYPE, IN, 3 ) = MAX( TIME-UNTIME,
     $               ZERO ) / DBLE( IC )
                  OPCNTS( IPAR, ITYPE, IN, 3 ) = OPS / DBLE( IC )
  170          CONTINUE
               LDT = LDA
               LDW = LDA
            ELSE
               IF( RUNQRS ) THEN
                  CALL DLACPY( 'Full', N, N, H, LDH, A, N )
*
                  CALL DHSEQR( 'S', 'N', N, 1, N, A, N, W, W( N+1 ), Z,
     $                         N, WORK, LWORK, IINFO )
*
                  IF( IINFO.NE.0 ) THEN
                     WRITE( NOUT, FMT = 9997 )SUBNAM( 3 ), IINFO, N,
     $                  ITYPE, 0, IOLDSD
                     INFO = ABS( IINFO )
                     GO TO 610
                  END IF
                  LDT = N
                  LDW = N
               END IF
            END IF
*
*           Time DHSEQR with JOB='I' for each 4-tuple
*           NNB(j), NSHFTS(j), MAXBS(j), LDAS(j)
*
            IF( TIMSUB( 4 ) ) THEN
               DO 200 IPAR = 1, NPARMS
                  LDA = LDAS( IPAR )
                  NSHIFT = NSHFTS( IPAR )
                  MAXB = MAXBS( IPAR )
                  NB = 1
                  CALL XLAENV( 4, NSHIFT )
                  CALL XLAENV( 8, MAXB )
*
*                 Time DHSEQR with JOB='I'
*
                  IC = 0
                  OPS = ZERO
                  S1 = DSECND( )
  180             CONTINUE
                  CALL DLACPY( 'Full', N, N, H, LDH, A, LDA )
*
                  CALL DHSEQR( 'S', 'I', N, 1, N, A, LDA, W, W( LDA+1 ),
     $                         Z, LDA, WORK, LWORK, IINFO )
*
                  IF( IINFO.NE.0 ) THEN
                     WRITE( NOUT, FMT = 9997 )SUBNAM( 4 ), IINFO, N,
     $                  ITYPE, IPAR, IOLDSD
                     INFO = ABS( IINFO )
                     GO TO 610
                  END IF
*
                  S2 = DSECND( )
                  TIME = S2 - S1
                  IC = IC + 1
                  IF( TIME.LT.TIMMIN )
     $               GO TO 180
*
*                 Subtract the time used in DLACPY.
*
                  S1 = DSECND( )
                  DO 190 J = 1, IC
                     CALL DLACPY( 'Full', N, N, H, LDH, Z, LDA )
  190             CONTINUE
                  S2 = DSECND( )
                  UNTIME = S2 - S1
*
                  TIMES( IPAR, ITYPE, IN, 4 ) = MAX( TIME-UNTIME,
     $               ZERO ) / DBLE( IC )
                  OPCNTS( IPAR, ITYPE, IN, 4 ) = OPS / DBLE( IC )
  200          CONTINUE
               LDT = LDA
               LDW = LDA
            END IF
*
*           Time DTREVC and DHSEIN with various values of LDA
*
*           Select All Eigenvectors
*
            DO 210 J = 1, N
               LLWORK( J ) = .TRUE.
  210       CONTINUE
*
            DO 370 IPAR = 1, NPARMS
               LDA = LDAS( IPAR )
*
*              If this value of LDA has come up before, just use
*              the value previously computed.
*
               LASTL = 0
               DO 220 J = 1, IPAR - 1
                  IF( LDA.EQ.LDAS( J ) )
     $               LASTL = J
  220          CONTINUE
*
*              Time DTREVC
*
               IF( ( TIMSUB( 5 ) .OR. TIMSUB( 6 ) ) .AND. LASTL.EQ.0 )
     $              THEN
*
*                 Copy T (which is in A) if necessary to get right LDA.
*
                  IF( LDA.GT.LDT ) THEN
                     DO 240 JC = N, 1, -1
                        DO 230 JR = N, 1, -1
                           A( JR+( JC-1 )*LDA ) = A( JR+( JC-1 )*LDT )
  230                   CONTINUE
  240                CONTINUE
                  ELSE IF( LDA.LT.LDT ) THEN
                     DO 260 JC = 1, N
                        DO 250 JR = 1, N
                           A( JR+( JC-1 )*LDA ) = A( JR+( JC-1 )*LDT )
  250                   CONTINUE
  260                CONTINUE
                  END IF
                  LDT = LDA
*
*                 Time DTREVC for Left Eigenvectors
*
                  IF( TIMSUB( 5 ) ) THEN
                     IC = 0
                     OPS = ZERO
                     S1 = DSECND( )
  270                CONTINUE
*
                     CALL DTREVC( 'L', 'A', LLWORK, N, A, LDA, Z, LDA,
     $                            Z, LDA, N, ITEMP, WORK, IINFO )
*
                     IF( IINFO.NE.0 ) THEN
                        WRITE( NOUT, FMT = 9997 )SUBNAM( 5 ), IINFO, N,
     $                     ITYPE, IPAR, IOLDSD
                        INFO = ABS( IINFO )
                        GO TO 610
                     END IF
                     S2 = DSECND( )
                     TIME = S2 - S1
                     IC = IC + 1
                     IF( TIME.LT.TIMMIN )
     $                  GO TO 270
*
                     TIMES( IPAR, ITYPE, IN, 5 ) = TIME / DBLE( IC )
                     OPCNTS( IPAR, ITYPE, IN, 5 ) = OPS / DBLE( IC )
                  END IF
*
*                 Time DTREVC for Right Eigenvectors
*
                  IF( TIMSUB( 6 ) ) THEN
                     IC = 0
                     OPS = ZERO
                     S1 = DSECND( )
  280                CONTINUE
                     CALL DTREVC( 'R', 'A', LLWORK, N, A, LDA, Z, LDA,
     $                            Z, LDA, N, ITEMP, WORK, IINFO )
*
                     IF( IINFO.NE.0 ) THEN
                        WRITE( NOUT, FMT = 9997 )SUBNAM( 6 ), IINFO, N,
     $                     ITYPE, IPAR, IOLDSD
                        INFO = ABS( IINFO )
                        GO TO 610
                     END IF
                     S2 = DSECND( )
                     TIME = S2 - S1
                     IC = IC + 1
                     IF( TIME.LT.TIMMIN )
     $                  GO TO 280
*
                     TIMES( IPAR, ITYPE, IN, 6 ) = TIME / DBLE( IC )
                     OPCNTS( IPAR, ITYPE, IN, 6 ) = OPS / DBLE( IC )
                  END IF
               ELSE
                  IF( TIMSUB( 5 ) ) THEN
                     OPCNTS( IPAR, ITYPE, IN, 5 ) = OPCNTS( LASTL,
     $                  ITYPE, IN, 5 )
                     TIMES( IPAR, ITYPE, IN, 5 ) = TIMES( LASTL, ITYPE,
     $                  IN, 5 )
                  END IF
                  IF( TIMSUB( 6 ) ) THEN
                     OPCNTS( IPAR, ITYPE, IN, 6 ) = OPCNTS( LASTL,
     $                  ITYPE, IN, 6 )
                     TIMES( IPAR, ITYPE, IN, 6 ) = TIMES( LASTL, ITYPE,
     $                  IN, 6 )
                  END IF
               END IF
*
*              Time DHSEIN
*
               IF( ( TIMSUB( 7 ) .OR. TIMSUB( 8 ) ) .AND. LASTL.EQ.0 )
     $              THEN
*
*                 Copy H if necessary to get right LDA.
*
                  IF( LDA.GT.LDH ) THEN
                     DO 300 JC = N, 1, -1
                        DO 290 JR = N, 1, -1
                           H( JR+( JC-1 )*LDA ) = H( JR+( JC-1 )*LDH )
  290                   CONTINUE
                        W( JC+LDA ) = W( JC+LDH )
  300                CONTINUE
                  ELSE IF( LDA.LT.LDH ) THEN
                     DO 320 JC = 1, N
                        DO 310 JR = 1, N
                           H( JR+( JC-1 )*LDA ) = H( JR+( JC-1 )*LDH )
  310                   CONTINUE
                        W( JC+LDA ) = W( JC+LDH )
  320                CONTINUE
                  END IF
                  LDH = LDA
*
*                 Copy W if necessary to get right LDA.
*
                  IF( LDA.GT.LDW ) THEN
                     DO 330 J = N, 1, -1
                        W( J+LDA ) = W( J+LDW )
  330                CONTINUE
                  ELSE IF( LDA.LT.LDW ) THEN
                     DO 340 J = 1, N
                        W( J+LDA ) = W( J+LDW )
  340                CONTINUE
                  END IF
                  LDW = LDA
*
*                 Time DHSEIN for Left Eigenvectors
*
                  IF( TIMSUB( 7 ) ) THEN
                     IC = 0
                     OPS = ZERO
                     S1 = DSECND( )
  350                CONTINUE
*
                     CALL DHSEIN( 'L', 'Q', 'N', LLWORK, N, H, LDA, W,
     $                            W( LDA+1 ), Z, LDA, Z, LDA, N, ITEMP,
     $                            WORK, IWORK, IWORK( N+1 ), IINFO )
*
                     IF( IINFO.NE.0 ) THEN
                        WRITE( NOUT, FMT = 9997 )SUBNAM( 7 ), IINFO, N,
     $                     ITYPE, IPAR, IOLDSD
                        INFO = ABS( IINFO )
                        GO TO 610
                     END IF
                     S2 = DSECND( )
                     TIME = S2 - S1
                     IC = IC + 1
                     IF( TIME.LT.TIMMIN )
     $                  GO TO 350
*
                     TIMES( IPAR, ITYPE, IN, 7 ) = TIME / DBLE( IC )
                     OPCNTS( IPAR, ITYPE, IN, 7 ) = OPS / DBLE( IC )
                  END IF
*
*                 Time DHSEIN for Right Eigenvectors
*
                  IF( TIMSUB( 8 ) ) THEN
                     IC = 0
                     OPS = ZERO
                     S1 = DSECND( )
  360                CONTINUE
*
                     CALL DHSEIN( 'R', 'Q', 'N', LLWORK, N, H, LDA, W,
     $                            W( LDA+1 ), Z, LDA, Z, LDA, N, ITEMP,
     $                            WORK, IWORK, IWORK( N+1 ), IINFO )
*
                     IF( IINFO.NE.0 ) THEN
                        WRITE( NOUT, FMT = 9997 )SUBNAM( 8 ), IINFO, N,
     $                     ITYPE, IPAR, IOLDSD
                        INFO = ABS( IINFO )
                        GO TO 610
                     END IF
                     S2 = DSECND( )
                     TIME = S2 - S1
                     IC = IC + 1
                     IF( TIME.LT.TIMMIN )
     $                  GO TO 360
*
                     TIMES( IPAR, ITYPE, IN, 8 ) = TIME / DBLE( IC )
                     OPCNTS( IPAR, ITYPE, IN, 8 ) = OPS / DBLE( IC )
                  END IF
               ELSE
                  IF( TIMSUB( 7 ) ) THEN
                     OPCNTS( IPAR, ITYPE, IN, 7 ) = OPCNTS( LASTL,
     $                  ITYPE, IN, 7 )
                     TIMES( IPAR, ITYPE, IN, 7 ) = TIMES( LASTL, ITYPE,
     $                  IN, 7 )
                  END IF
                  IF( TIMSUB( 8 ) ) THEN
                     OPCNTS( IPAR, ITYPE, IN, 8 ) = OPCNTS( LASTL,
     $                  ITYPE, IN, 8 )
                     TIMES( IPAR, ITYPE, IN, 8 ) = TIMES( LASTL, ITYPE,
     $                  IN, 8 )
                  END IF
               END IF
  370       CONTINUE
*
*-----------------------------------------------------------------------
*
*           Time the EISPACK Routines
*
*           Restore random number seed
*
            DO 380 J = 1, 4
               ISEED( J ) = IOLDSD( J )
  380       CONTINUE
*
*           Re-generate A
*
            IF( ITYPE.LE.MAXTYP ) THEN
               IMODE = KMODE( ITYPE )
               IF( ICONDS.EQ.1 ) THEN
                  CONDS = ONE
               ELSE
                  CONDS = RTULPI
               END IF
               CALL DLATME( N, 'S', ISEED, WORK, IMODE, ULPINV, ONE,
     $                      ADUMMA, 'T', 'T', 'T', WORK( N+1 ), 4,
     $                      CONDS, N, N, ONE, A, N, WORK( 2*N+1 ),
     $                      IINFO )
            END IF
*
*           Time ORTHES for each LDAS(j)
*
            IF( TIMSUB( 9 ) ) THEN
               DO 420 IPAR = 1, NPARMS
                  LDA = LDAS( IPAR )
*
*                 If this value of LDA has come up before, just use
*                 the value previously computed.
*
                  LASTL = 0
                  DO 390 J = 1, IPAR - 1
                     IF( LDA.EQ.LDAS( J ) )
     $                  LASTL = J
  390             CONTINUE
*
                  IF( LASTL.EQ.0 ) THEN
*
*                    Time ORTHES
*
                     IC = 0
                     OPS = ZERO
                     S1 = DSECND( )
*
  400                CONTINUE
                     CALL DLACPY( 'Full', N, N, A, N, H, LDA )
*
                     CALL ORTHES( LDA, N, 1, N, H, WORK )
*
                     S2 = DSECND( )
                     TIME = S2 - S1
                     IC = IC + 1
                     IF( TIME.LT.TIMMIN )
     $                  GO TO 400
*
*                    Subtract the time used in DLACPY.
*
                     S1 = DSECND( )
                     DO 410 J = 1, IC
                        CALL DLACPY( 'Full', N, N, A, N, Z, LDA )
  410                CONTINUE
                     S2 = DSECND( )
                     UNTIME = S2 - S1
*
*                     OPS1 = ( 20*N**3 - 3*N**2 - 23*N ) / 6 - 17
*
                     TIMES( IPAR, ITYPE, IN, 9 ) = MAX( TIME-UNTIME,
     $                  ZERO ) / DBLE( IC )
                     OPCNTS( IPAR, ITYPE, IN, 9 ) = OPS / DBLE( IC )
                  ELSE
                     OPCNTS( IPAR, ITYPE, IN, 9 ) = OPCNTS( LASTL,
     $                  ITYPE, IN, 9 )
                     TIMES( IPAR, ITYPE, IN, 9 ) = TIMES( LASTL, ITYPE,
     $                  IN, 9 )
                  END IF
                  LDH = LDA
  420          CONTINUE
            ELSE
               IF( RUNORT ) THEN
                  CALL DLACPY( 'Full', N, N, A, N, H, N )
*
                  CALL ORTHES( N, N, 1, N, H, WORK )
*
                  LDH = N
               END IF
            END IF
*
*           Time HQR for each LDAS(j)
*
            IF( TIMSUB( 10 ) ) THEN
               DO 460 IPAR = 1, NPARMS
                  LDA = LDAS( IPAR )
*
*                 If this value of LDA has come up before, just use
*                 the value previously computed.
*
                  LASTL = 0
                  DO 430 J = 1, IPAR - 1
                     IF( LDA.EQ.LDAS( J ) )
     $                  LASTL = J
  430             CONTINUE
*
                  IF( LASTL.EQ.0 ) THEN
*
*                    Time HQR
*
                     IC = 0
                     OPS = ZERO
                     S1 = DSECND( )
  440                CONTINUE
                     CALL DLACPY( 'Full', N, N, H, LDH, A, LDA )
*
                     CALL HQR( LDA, N, 1, N, A, W, W( LDA+1 ), IINFO )
*
                     IF( IINFO.NE.0 ) THEN
                        WRITE( NOUT, FMT = 9997 )SUBNAM( 10 ), IINFO,
     $                     N, ITYPE, IPAR, IOLDSD
                        INFO = ABS( IINFO )
                        GO TO 610
                     END IF
                     S2 = DSECND( )
                     TIME = S2 - S1
                     IC = IC + 1
                     IF( TIME.LT.TIMMIN )
     $                  GO TO 440
*
*                    Subtract the time used in DLACPY.
*
                     S1 = DSECND( )
                     DO 450 J = 1, IC
                        CALL DLACPY( 'Full', N, N, H, LDH, Z, LDA )
  450                CONTINUE
                     S2 = DSECND( )
                     UNTIME = S2 - S1
*
                     TIMES( IPAR, ITYPE, IN, 10 ) = MAX( TIME-UNTIME,
     $                  ZERO ) / DBLE( IC )
                     OPCNTS( IPAR, ITYPE, IN, 10 ) = OPS / DBLE( IC )
                  ELSE
                     OPCNTS( IPAR, ITYPE, IN, 10 ) = OPCNTS( LASTL,
     $                  ITYPE, IN, 10 )
                     TIMES( IPAR, ITYPE, IN, 10 ) = TIMES( LASTL, ITYPE,
     $                  IN, 10 )
                  END IF
                  LDW = LDA
  460          CONTINUE
            ELSE
               IF( RUNHQR ) THEN
                  CALL DLACPY( 'Full', N, N, A, N, H, N )
*
                  CALL HQR( N, N, 1, N, A, W, W( N+1 ), IINFO )
*
                  LDW = N
               END IF
            END IF
*
*           Time HQR2 for each LDAS(j)
*
            IF( TIMSUB( 11 ) ) THEN
               DO 500 IPAR = 1, NPARMS
                  LDA = LDAS( IPAR )
*
*                 If this value of LDA has come up before, just use
*                 the value previously computed.
*
                  LASTL = 0
                  DO 470 J = 1, IPAR - 1
                     IF( LDA.EQ.LDAS( J ) )
     $                  LASTL = J
  470             CONTINUE
*
                  IF( LASTL.EQ.0 ) THEN
*
*                    Time HQR2
*
                     IC = 0
                     OPS = ZERO
                     S1 = DSECND( )
  480                CONTINUE
                     CALL DLACPY( 'Full', N, N, H, LDH, A, LDA )
                     CALL DLASET( 'Full', N, N, ZERO, ONE, Z, LDA )
*
                     CALL HQR2( LDA, N, 1, N, A, W, W( LDA+1 ), Z,
     $                          IINFO )
*
                     IF( IINFO.NE.0 ) THEN
                        WRITE( NOUT, FMT = 9997 )SUBNAM( 11 ), IINFO,
     $                     N, ITYPE, IPAR, IOLDSD
                        INFO = ABS( IINFO )
                        GO TO 610
                     END IF
                     S2 = DSECND( )
                     TIME = S2 - S1
                     IC = IC + 1
                     IF( TIME.LT.TIMMIN )
     $                  GO TO 480
*
*                    Subtract the time used in DLACPY.
*
                     S1 = DSECND( )
                     DO 490 J = 1, IC
                        CALL DLACPY( 'Full', N, N, H, LDH, Z, LDA )
  490                CONTINUE
                     S2 = DSECND( )
                     UNTIME = S2 - S1
*
                     TIMES( IPAR, ITYPE, IN, 11 ) = MAX( TIME-UNTIME,
     $                  ZERO ) / DBLE( IC )
                     OPCNTS( IPAR, ITYPE, IN, 11 ) = OPS / DBLE( IC )
                  ELSE
                     OPCNTS( IPAR, ITYPE, IN, 11 ) = OPCNTS( LASTL,
     $                  ITYPE, IN, 11 )
                     TIMES( IPAR, ITYPE, IN, 11 ) = TIMES( LASTL, ITYPE,
     $                  IN, 11 )
                  END IF
                  LDW = LDA
  500          CONTINUE
            END IF
*
*           Time INVIT for each LDAS(j)
*
*           Select All Eigenvectors
*
            DO 510 J = 1, N
               LLWORK( J ) = .TRUE.
  510       CONTINUE
*
            IF( TIMSUB( 12 ) ) THEN
               DO 600 IPAR = 1, NPARMS
                  LDA = LDAS( IPAR )
*
*                 If this value of LDA has come up before, just use
*                 the value previously computed.
*
                  LASTL = 0
                  DO 520 J = 1, IPAR - 1
                     IF( LDA.EQ.LDAS( J ) )
     $                  LASTL = J
  520             CONTINUE
*
                  IF( LASTL.EQ.0 ) THEN
*
*                    Copy H if necessary to get right LDA.
*
                     IF( LDA.GT.LDH ) THEN
                        DO 540 JC = N, 1, -1
                           DO 530 JR = N, 1, -1
                              H( JR+( JC-1 )*LDA ) = H( JR+( JC-1 )*
     $                           LDH )
  530                      CONTINUE
  540                   CONTINUE
                     ELSE IF( LDA.LT.LDH ) THEN
                        DO 560 JC = 1, N
                           DO 550 JR = 1, N
                              H( JR+( JC-1 )*LDA ) = H( JR+( JC-1 )*
     $                           LDH )
  550                      CONTINUE
  560                   CONTINUE
                     END IF
                     LDH = LDA
*
*                    Copy W if necessary to get right LDA.
*
                     IF( LDA.GT.LDW ) THEN
                        DO 570 J = N, 1, -1
                           W( J+LDA ) = W( J+LDW )
  570                   CONTINUE
                     ELSE IF( LDA.LT.LDW ) THEN
                        DO 580 J = 1, N
                           W( J+LDA ) = W( J+LDW )
  580                   CONTINUE
                     END IF
                     LDW = LDA
*
*                    Time INVIT for right eigenvectors.
*
                     IC = 0
                     OPS = ZERO
                     S1 = DSECND( )
  590                CONTINUE
*
                     CALL INVIT( LDA, N, H, W, W( LDA+1 ), LLWORK, N,
     $                           ITEMP, Z, IINFO, WORK( 2*N+1 ), WORK,
     $                           WORK( N+1 ) )
*
                     IF( IINFO.NE.0 ) THEN
                        WRITE( NOUT, FMT = 9997 )SUBNAM( 12 ), IINFO,
     $                     N, ITYPE, IPAR, IOLDSD
                        INFO = ABS( IINFO )
                        GO TO 610
                     END IF
                     S2 = DSECND( )
                     TIME = S2 - S1
                     IC = IC + 1
                     IF( TIME.LT.TIMMIN )
     $                  GO TO 590
*
*                    TIME = TIME / DOUBLE PRECISION( IC )
*                    OPS1 = OPS / DOUBLE PRECISION( IC )
*                    OPCNTS( IPAR, ITYPE, IN, 12 ) = OPS1
*                    TIMES( IPAR, ITYPE, IN, 12 ) = DMFLOP( OPS1, TIME,
*     $                  IINFO )
*
                     TIMES( IPAR, ITYPE, IN, 12 ) = TIME / DBLE( IC )
                     OPCNTS( IPAR, ITYPE, IN, 12 ) = OPS / DBLE( IC )
                  ELSE
                     OPCNTS( IPAR, ITYPE, IN, 12 ) = OPCNTS( LASTL,
     $                  ITYPE, IN, 12 )
                     TIMES( IPAR, ITYPE, IN, 12 ) = TIMES( LASTL, ITYPE,
     $                  IN, 12 )
                  END IF
  600          CONTINUE
            END IF
*
  610    CONTINUE
  620 CONTINUE
*
*-----------------------------------------------------------------------
*
*     Print a table of results for each timed routine.
*
      ISUB = 1
      IF( TIMSUB( ISUB ) ) THEN
         CALL DPRTBE( SUBNAM( ISUB ), MTYPES, DOTYPE, NSIZES, NN,
     $                INPARM( ISUB ), PNAMES, NPARMS, LDAS, NNB, NSHFTS,
     $                MAXBS, OPCNTS( 1, 1, 1, ISUB ), LDO1, LDO2,
     $                TIMES( 1, 1, 1, ISUB ), LDT1, LDT2, WORK, LLWORK,
     $                NOUT )
      END IF
*
      DO 630 IN = 1, NPARMS
         NNB( IN ) = 1
  630 CONTINUE
*
      DO 640 ISUB = 2, NSUBS
         IF( TIMSUB( ISUB ) ) THEN
            CALL DPRTBE( SUBNAM( ISUB ), MTYPES, DOTYPE, NSIZES, NN,
     $                   INPARM( ISUB ), PNAMES, NPARMS, LDAS, NNB,
     $                   NSHFTS, MAXBS, OPCNTS( 1, 1, 1, ISUB ), LDO1,
     $                   LDO2, TIMES( 1, 1, 1, ISUB ), LDT1, LDT2, WORK,
     $                   LLWORK, NOUT )
         END IF
  640 CONTINUE
*
      RETURN
*
*     End of DTIM21
*
 9997 FORMAT( ' DTIM21: ', A, ' returned INFO=', I6, '.', / 9X, 'N=',
     $      I6, ', ITYPE=', I6, ', IPAR=', I6, ', ISEED=(',
     $      3( I5, ',' ), I5, ')' )
*
      END
      SUBROUTINE DTIM22( LINE, NSIZES, NN, NTYPES, DOTYPE, NPARMS, NNB,
     $                   LDAS, TIMMIN, NOUT, ISEED, A, D, E, E2, Z, Z1,
     $                   WORK, LWORK, LLWORK, IWORK, TIMES, LDT1, LDT2,
     $                   LDT3, OPCNTS, LDO1, LDO2, LDO3, INFO )
*
*  -- LAPACK timing routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     February 20, 2000
*
*     .. Scalar Arguments ..
      CHARACTER*80       LINE
      INTEGER            INFO, LDO1, LDO2, LDO3, LDT1, LDT2, LDT3,
     $                   LWORK, NOUT, NPARMS, NSIZES, NTYPES
      DOUBLE PRECISION   TIMMIN
*     ..
*     .. Array Arguments ..
      LOGICAL            DOTYPE( * ), LLWORK( * )
      INTEGER            ISEED( * ), IWORK( * ), LDAS( * ), NN( * ),
     $                   NNB( * )
      DOUBLE PRECISION   A( * ), D( * ), E( * ), E2( * ),
     $                   OPCNTS( LDO1, LDO2, LDO3, * ),
     $                   TIMES( LDT1, LDT2, LDT3, * ), WORK( * ),
     $                   Z( * ), Z1( * )
*     ..
*
*  Purpose
*  =======
*
*     DTIM22 times the LAPACK routines for the real symmetric
*     eigenvalue problem.
*
*     For each N value in NN(1:NSIZES) and .TRUE. value in
*     DOTYPE(1:NTYPES), a matrix will be generated and used to test the
*     selected routines.  Thus, NSIZES*(number of .TRUE. values in
*     DOTYPE) matrices will be generated.
*
*  Arguments
*  =========
*
*  LINE    (input) CHARACTER*80
*          On entry, LINE contains the input line which requested
*          this routine.  This line may contain a subroutine name,
*          such as DSYTRD, indicating that only routine SSYTRD will
*          be timed, or it may contain a generic name, such as DST.
*          In this case, the rest of the line is scanned for the
*          first 23 non-blank characters, corresponding to the eight
*          combinations of subroutine and options:
*          LAPACK:
*          1: DSYTRD
*          2: DORGTR
*          3: DORMTR
*          4: DSTEQR(VECT='N')
*          5: DSTEQR(VECT='V')
*          6: DSTERF
*          7: DPTEQR(VECT='N')
*          8: DPTEQR(VECT='V')
*          9: DSTEBZ(RANGE='I')
*          10: DSTEBZ(RANGE='V')
*          11: DSTEIN
*          12: DSTEDC(COMPQ='N')
*          13: DSTEDC(COMPQ='I')
*          14: DSTEDC(COMPQ='V')
*          15: DSTEGR(COMPQ='N')
*          16: DSTEGR(COMPQ='V')
*          EISPACK:
*          17: TRED1  (compare with DSYTRD)
*          18: IMTQL1 (compare w/ DSTEQR -- VECT='N')
*          19: IMTQL2 (compare w/ DSTEQR -- VECT='V')
*          20: TQLRAT (compare with DSTERF)
*          21: TRIDIB (compare with DSTEBZ -- RANGE='I')
*          22: BISECT (compare with DSTEBZ -- RANGE='V')
*          23: TINVIT (compare with DSTEIN)
*          If a character is 'T' or 't', the corresponding routine in
*          this path is timed.  If the entire line is blank, all the
*          routines in the path are timed.
*
*  NSIZES  (input) INTEGER
*          The number of values of N contained in the vector NN.
*
*  NN      (input) INTEGER array, dimension( NSIZES )
*          The values of the matrix size N to be tested.  For each
*          N value in the array NN, and each .TRUE. value in DOTYPE,
*          a matrix A will be generated and used to test the routines.
*
*  NTYPES  (input) INTEGER
*          The number of types in DOTYPE.  Only the first MAXTYP
*          elements will be examined.  Exception: if NSIZES=1 and
*          NTYPES=MAXTYP+1, and DOTYPE=MAXTYP*f,t, then the input
*          value of A will be used.
*
*  DOTYPE  (input) LOGICAL
*          If DOTYPE(j) is .TRUE., then a matrix of type j will be
*          generated.  The matrix A has the form X**(-1) D X, where
*          X is orthogonal and D is diagonal with:
*          (j=1)  evenly spaced entries 1, ..., ULP with random signs.
*          (j=2)  geometrically spaced entries 1, ..., ULP with random
*                 signs.
*          (j=3)  "clustered" entries 1, ULP,..., ULP with random
*                 signs.
*          (j=4)  entries randomly chosen from ( ULP, 1 ).
*
*  NPARMS  (input) INTEGER
*          The number of values in each of the arrays NNB and LDAS.
*          For each matrix A generated according to NN and DOTYPE,
*          tests will be run with (NB,LDA)=
*          (NNB(1),LDAS(1)),...,(NNB(NPARMS), LDAS(NPARMS))
*
*  NNB     (input) INTEGER array, dimension( NPARMS )
*          The values of the blocksize ("NB") to be tested.
*
*  LDAS    (input) INTEGER array, dimension( NPARMS )
*          The values of LDA, the leading dimension of all matrices,
*          to be tested.
*
*  TIMMIN  (input) DOUBLE PRECISION
*          The minimum time a subroutine will be timed.
*
*  NOUT    (input) INTEGER
*          If NOUT > 0 then NOUT specifies the unit number
*          on which the output will be printed.  If NOUT <= 0, no
*          output is printed.
*
*  ISEED   (input/output) INTEGER array, dimension( 4 )
*          The random seed used by the random number generator, used
*          by the test matrix generator.  It is used and updated on
*          each call to DTIM22
*
*  A       (workspace) DOUBLE PRECISION array,
*                      dimension( max(NN)*max(LDAS) )
*          The original matrix to be tested.
*
*  D       (workspace) DOUBLE PRECISION array,
*                      dimension( max(NN) )
*          The diagonal of the tridiagonal generated by DSYTRD/TRED1.
*
*  E       (workspace) DOUBLE PRECISION array,
*                      dimension( max(NN) )
*          The off-diagonal of the tridiagonal generated by
*          DSYTRD/TRED1.
*
*  E2      (workspace) DOUBLE PRECISION array,
*                      dimension( max(NN) )
*          The square of the off-diagonal of the tridiagonal generated
*          by TRED1.  (Used by TQLRAT.)
*
*  Z       (workspace) DOUBLE PRECISION array,
*                      dimension( max(NN)*max(LDAS) )
*          Various output arrays.
*
*  WORK    (workspace) DOUBLE PRECISION array, dimension( LWORK )
*
*  LWORK   (input) INTEGER
*          Number of elements in WORK.  It must be at least
*          (a)  max( (NNB + 2 )*LDAS )
*          (b)  max( 5*LDAS )
*          (c)  NSIZES*NTYPES*NPARMS
*          (d)  2*LDAS + 1 + 3*maxNN + 2*maxNN*log2(maxNN) + 3*maxNN**2
*               where maxNN = maximum matrix dimension in NN
*                     log2(x) = smallest integer power of 2 .ge. x
*
*  LLWORK  (workspace) LOGICAL array of dimension( NPARMS ),
*
*  IWORK   (workspace) INTEGER array of dimension
*          6 + 6*maxNN + 5*maxNN*log2(maxNN)
*
*  TIMES   (output) DOUBLE PRECISION array,
*                   dimension (LDT1,LDT2,LDT3,NSUBS)
*          TIMES(i,j,k,l) will be set to the run time (in seconds) for
*          subroutine l, with N=NN(k), matrix type j, and LDA=LDAS(i),
*          NBLOCK=NNB(i).
*
*  LDT1    (input) INTEGER
*          The first dimension of TIMES.  LDT1 >= min( 1, NPARMS ).
*
*  LDT2    (input) INTEGER
*          The second dimension of TIMES.  LDT2 >= min( 1, NTYPES ).
*
*  LDT3    (input) INTEGER
*          The third dimension of TIMES.  LDT3 >= min( 1, NSIZES ).
*
*  OPCNTS  (output) DOUBLE PRECISION array,
*                   dimension (LDO1,LDO2,LDO3,NSUBS)
*          OPCNTS(i,j,k,l) will be set to the number of floating-point
*          operations executed by subroutine l, with N=NN(k), matrix
*          type j, and LDA=LDAS(i), NBLOCK=NNB(i).
*
*  LDO1    (input) INTEGER
*          The first dimension of OPCNTS.  LDO1 >= min( 1, NPARMS ).
*
*  LDO2    (input) INTEGER
*          The second dimension of OPCNTS.  LDO2 >= min( 1, NTYPES ).
*
*  LDO3    (input) INTEGER
*          The third dimension of OPCNTS.  LDO3 >= min( 1, NSIZES ).
*
*  INFO    (output) INTEGER
*          Error flag.  It will be set to zero if no error occurred.
*
*  =====================================================================
*
*     .. Parameters ..
      INTEGER            MAXTYP, NSUBS
      PARAMETER          ( MAXTYP = 4, NSUBS = 23 )
      DOUBLE PRECISION   ZERO, ONE, TWO
      PARAMETER          ( ZERO = 0.0D0, ONE = 1.0D0, TWO = 2.0D0 )
*     ..
*     .. Local Scalars ..
      LOGICAL            RUNTR1, RUNTRD
      CHARACTER          UPLO
      INTEGER            I, IC, IINFO, IL, ILWORK, IMODE, IN, INFSOK,
     $                   IPAR, ISUB, ITYPE, IU, J, J1, J2, J3, J4,
     $                   LASTL, LDA, LGN, LIWEDC, LIWEVR, LWEDC, LWEVR,
     $                   M, M11, MM, MMM, MTYPES, N, NANSOK, NB, NSPLIT
      DOUBLE PRECISION   ABSTOL, EPS1, RLB, RUB, S1, S2, TIME, ULP,
     $                   ULPINV, UNTIME, VL, VU
*     ..
*     .. Local Arrays ..
      LOGICAL            TIMSUB( NSUBS )
      CHARACTER*4        PNAMES( 4 )
      CHARACTER*9        SUBNAM( NSUBS )
      INTEGER            IDUMMA( 1 ), INPARM( NSUBS ), IOLDSD( 4 ),
     $                   KMODE( MAXTYP )
*     ..
*     .. External Functions ..
      INTEGER            ILAENV
      DOUBLE PRECISION   DLAMCH, DOPLA, DSECND, DOPLA2
      EXTERNAL           DLAMCH, DOPLA, DSECND, DOPLA2, ILAENV
*     ..
*     .. External Subroutines ..
      EXTERNAL           ATIMIN, BISECT, DCOPY, DLACPY, DLASET, DLATMS,
     $                   DORGTR, DORMTR, DPRTBE, DPTEQR, DSTEBZ, DSTEDC,
     $                   DSTEGR, DSTEIN, DSTEQR, DSTERF, DSYTRD, IMTQL1,
     $                   IMTQL2, TINVIT, TQLRAT, TRED1, TRIDIB, XLAENV
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, DBLE, INT, LOG, MAX, MIN
*     ..
*     .. Common blocks ..
      COMMON             / LATIME / OPS, ITCNT
*     ..
*     .. Scalars in Common ..
      DOUBLE PRECISION   ITCNT, OPS
*     ..
*     .. Data statements ..
      DATA               SUBNAM / 'DSYTRD', 'DORGTR', 'DORMTR',
     $                   'DSTEQR(N)', 'DSTEQR(V)', 'DSTERF',
     $                   'DPTEQR(N)', 'DPTEQR(V)', 'DSTEBZ(I)',
     $                   'DSTEBZ(V)', 'DSTEIN', 'DSTEDC(N)',
     $                   'DSTEDC(I)', 'DSTEDC(V)', 'DSTEGR(N)',
     $                   'DSTEGR(V)', 'TRED1', 'IMTQL1', 'IMTQL2',
     $                   'TQLRAT', 'TRIDIB', 'BISECT', 'TINVIT' /
      DATA               INPARM / 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
     $                   1, 1, 1, 1, 1, 1, 1, 1, 1, 1 /
      DATA               PNAMES / 'LDA', 'NB', 'bad1', 'bad2' /
      DATA               KMODE / 4, 3, 1, 5 /
*     ..
*     .. Executable Statements ..
*
*
*     Extract the timing request from the input line.
*
      CALL ATIMIN( 'DST', LINE, NSUBS, SUBNAM, TIMSUB, NOUT, INFO )
*
*     Disable timing of DSTEGR if we're non-IEEE-754 compliant.
*
      NANSOK = ILAENV( 10, 'DSTEGR', ' ', 0, 0, 0, 0 )
      INFSOK = ILAENV( 11, 'DSTEGR', ' ', 0, 0, 0, 0 )
      IF( NANSOK.NE.1 .OR. INFSOK.NE.1 )  THEN
         TIMSUB(15) = .FALSE.
         TIMSUB(16) = .FALSE.
      END IF
*
      IF( INFO.NE.0 )
     $   RETURN
*
*     Check that N <= LDA for the input values.
*
      DO 20 J2 = 1, NSIZES
         DO 10 J1 = 1, NPARMS
            IF( NN( J2 ).GT.LDAS( J1 ) ) THEN
               INFO = -8
               WRITE( NOUT, FMT = 9999 )LINE( 1: 6 )
 9999          FORMAT( 1X, A, ' timing run not attempted -- N > LDA',
     $               / )
               RETURN
            END IF
   10    CONTINUE
   20 CONTINUE
*
*     Check LWORK
*
      ILWORK = NSIZES*NPARMS*NTYPES
      DO 30 J1 = 1, NPARMS
         ILWORK = MAX( ILWORK, 5*LDAS( J1 ),
     $            ( NNB( J1 )+2 )*LDAS( J1 ) )
   30 CONTINUE
      IF( ILWORK.GT.LWORK ) THEN
         INFO = -18
         WRITE( NOUT, FMT = 9998 )LINE( 1: 6 )
 9998    FORMAT( 1X, A, ' timing run not attempted -- LWORK too small.',
     $         / )
         RETURN
      END IF
*
*     Check to see whether DSYTRD must be run.
*
*     RUNTRD -- if DSYTRD must be run.
*
      RUNTRD = .FALSE.
      IF( TIMSUB( 4 ) .OR. TIMSUB( 5 ) .OR. TIMSUB( 6 ) .OR.
     $    TIMSUB( 7 ) .OR. TIMSUB( 8 ) .OR. TIMSUB( 9 ) .OR.
     $    TIMSUB( 10 ) .OR. TIMSUB( 11 ) .OR. TIMSUB( 12 ) .OR.
     $    TIMSUB( 13 ) .OR. TIMSUB( 14 ) .OR. TIMSUB( 15 ) .OR.
     $    TIMSUB( 16 ) )RUNTRD = .TRUE.
*
*     Check to see whether TRED1 must be run.
*
*     RUNTR1 -- if TRED1 must be run.
*
      RUNTR1 = .FALSE.
      IF( TIMSUB( 17 ) .OR. TIMSUB( 18 ) .OR. TIMSUB( 19 ) .OR.
     $    TIMSUB( 20 ) .OR. TIMSUB( 21 ) .OR. TIMSUB( 22 ) .OR.
     $    TIMSUB( 23 ) )RUNTR1 = .TRUE.
*
*     Various Constants
*
      ULP = DLAMCH( 'Epsilon' )*DLAMCH( 'Base' )
      ULPINV = ONE / ULP
      CALL XLAENV( 9, 25 )
*
*     Zero out OPCNTS, TIMES
*
      DO 70 J4 = 1, NSUBS
         DO 60 J3 = 1, NSIZES
            DO 50 J2 = 1, NTYPES
               DO 40 J1 = 1, NPARMS
                  OPCNTS( J1, J2, J3, J4 ) = ZERO
                  TIMES( J1, J2, J3, J4 ) = ZERO
   40          CONTINUE
   50       CONTINUE
   60    CONTINUE
   70 CONTINUE
*
*     Do for each value of N:
*
      DO 940 IN = 1, NSIZES
*
         N = NN( IN )
         IF( N.GT.0 ) THEN
            LGN = INT( LOG( DBLE( N ) ) / LOG( TWO ) )
            IF( 2**LGN.LT.N )
     $         LGN = LGN + 1
            IF( 2**LGN.LT.N )
     $         LGN = LGN + 1
            LWEDC = 1 + 4*N + 2*N*LGN + 3*N**2
            LIWEDC = 6 + 6*N + 5*N*LGN
            LWEVR = 18*N
            LIWEVR = 10*N
         ELSE
            LWEDC = 8
            LIWEDC = 12
            LWEVR = 1
            LIWEVR = 1
         END IF
*
*        Do for each .TRUE. value in DOTYPE:
*
         MTYPES = MIN( MAXTYP, NTYPES )
         IF( NTYPES.EQ.MAXTYP+1 .AND. NSIZES.EQ.1 )
     $      MTYPES = NTYPES
         DO 930 ITYPE = 1, MTYPES
            IF( .NOT.DOTYPE( ITYPE ) )
     $         GO TO 930
*
*           Save random number seed for error messages
*
            DO 80 J = 1, 4
               IOLDSD( J ) = ISEED( J )
   80       CONTINUE
*
*-----------------------------------------------------------------------
*
*           Time the LAPACK Routines
*
*           Generate A
*
            UPLO = 'L'
            IF( ITYPE.LE.MAXTYP ) THEN
               IMODE = KMODE( ITYPE )
               CALL DLATMS( N, N, 'S', ISEED, 'S', WORK, IMODE, ULPINV,
     $                      ONE, N, N, UPLO, A, N, WORK( N+1 ), IINFO )
            END IF
*
*           Time DSYTRD for each pair NNB(j), LDAS(j)
*
            IF( TIMSUB( 1 ) ) THEN
               DO 110 IPAR = 1, NPARMS
                  LDA = LDAS( IPAR )
                  NB = MIN( N, NNB( IPAR ) )
                  CALL XLAENV( 1, NB )
                  CALL XLAENV( 2, 2 )
                  CALL XLAENV( 3, NB )
*
*                 Time DSYTRD
*
                  IC = 0
                  OPS = ZERO
                  S1 = DSECND( )
   90             CONTINUE
                  CALL DLACPY( UPLO, N, N, A, N, Z, LDA )
                  CALL DSYTRD( UPLO, N, Z, LDA, D, E, WORK, WORK( N+1 ),
     $                         LWORK-N, IINFO )
                  IF( IINFO.NE.0 ) THEN
                     WRITE( NOUT, FMT = 9997 )SUBNAM( 1 ), IINFO, N,
     $                  ITYPE, IPAR, IOLDSD
                     INFO = ABS( IINFO )
                     GO TO 590
                  END IF
*
                  S2 = DSECND( )
                  TIME = S2 - S1
                  IC = IC + 1
                  IF( TIME.LT.TIMMIN )
     $               GO TO 90
*
*                 Subtract the time used in DLACPY.
*
                  S1 = DSECND( )
                  DO 100 J = 1, IC
                     CALL DLACPY( UPLO, N, N, A, N, Z, LDA )
  100             CONTINUE
                  S2 = DSECND( )
                  UNTIME = S2 - S1
*
                  TIMES( IPAR, ITYPE, IN, 1 ) = MAX( TIME-UNTIME,
     $               ZERO ) / DBLE( IC )
                  OPCNTS( IPAR, ITYPE, IN, 1 ) = DOPLA( 'DSYTRD', N, 0,
     $               0, 0, NB )
  110          CONTINUE
            ELSE
               IF( RUNTRD ) THEN
                  CALL DLACPY( UPLO, N, N, A, N, Z, N )
                  CALL DSYTRD( UPLO, N, Z, N, D, E, WORK, WORK( N+1 ),
     $                         LWORK-N, IINFO )
                  IF( IINFO.NE.0 ) THEN
                     WRITE( NOUT, FMT = 9997 )SUBNAM( 1 ), IINFO, N,
     $                  ITYPE, 0, IOLDSD
                     INFO = ABS( IINFO )
                     GO TO 590
                  END IF
               END IF
            END IF
*
*           Time DORGTR for each pair NNB(j), LDAS(j)
*
            IF( TIMSUB( 2 ) ) THEN
               DO 140 IPAR = 1, NPARMS
                  LDA = LDAS( IPAR )
                  NB = MIN( N, NNB( IPAR ) )
                  CALL XLAENV( 1, NB )
                  CALL XLAENV( 2, 2 )
                  CALL XLAENV( 3, NB )
*
*                 Time DORGTR
*
                  CALL DLACPY( UPLO, N, N, A, N, Z, LDA )
                  CALL DSYTRD( UPLO, N, Z, LDA, D, E, WORK, WORK( N+1 ),
     $                         LWORK-N, IINFO )
                  IC = 0
                  OPS = ZERO
                  S1 = DSECND( )
  120             CONTINUE
                  CALL DLACPY( 'F', N, N, Z, LDA, Z1, LDA )
                  CALL DORGTR( UPLO, N, Z1, LDA, WORK, WORK( N+1 ),
     $                         LWORK-N, IINFO )
                  IF( IINFO.NE.0 ) THEN
                     WRITE( NOUT, FMT = 9997 )SUBNAM( 2 ), IINFO, N,
     $                  ITYPE, IPAR, IOLDSD
                     INFO = ABS( IINFO )
                     GO TO 590
                  END IF
*
                  S2 = DSECND( )
                  TIME = S2 - S1
                  IC = IC + 1
                  IF( TIME.LT.TIMMIN )
     $               GO TO 120
*
*                 Subtract the time used in DLACPY
*
                  S1 = DSECND( )
                  DO 130 J = 1, IC
                     CALL DLACPY( 'F', N, N, Z, LDA, Z1, LDA )
  130             CONTINUE
                  S2 = DSECND( )
                  UNTIME = S2 - S1
*
                  TIMES( IPAR, ITYPE, IN, 2 ) = MAX( TIME-UNTIME,
     $               ZERO ) / DBLE( IC )
                  OPCNTS( IPAR, ITYPE, IN, 2 ) = DOPLA2( 'DORGTR', UPLO,
     $               N, N, N, 0, NB )
  140          CONTINUE
            END IF
*
*           Time DORMTR for each pair NNB(j), LDAS(j)
*
            IF( TIMSUB( 3 ) ) THEN
               DO 170 IPAR = 1, NPARMS
                  LDA = LDAS( IPAR )
                  NB = MIN( N, NNB( IPAR ) )
                  CALL XLAENV( 1, NB )
                  CALL XLAENV( 2, 2 )
                  CALL XLAENV( 3, NB )
*
*                 Time DORMTR
*
                  CALL DLACPY( UPLO, N, N, A, N, Z, LDA )
                  CALL DSYTRD( UPLO, N, Z, LDA, D, E, WORK, WORK( N+1 ),
     $                         LWORK-N, IINFO )
                  IC = 0
                  OPS = ZERO
                  S1 = DSECND( )
  150             CONTINUE
                  CALL DCOPY( N, D, 1, WORK( LDA+1 ), 1 )
                  CALL DCOPY( N-1, E, 1, WORK( 2*LDA+1 ), 1 )
                  CALL DSTEDC( 'N', N, WORK( LDA+1 ), WORK( 2*LDA+1 ),
     $                         Z1, LDA, WORK( 3*LDA+1 ), LWEDC, IWORK,
     $                         LIWEDC, IINFO )
                  CALL DORMTR( 'L', UPLO, 'N', N, N, Z, LDA, WORK, Z1,
     $                         LDA, WORK( N+1 ), LWORK-N, IINFO )
                  IF( IINFO.NE.0 ) THEN
                     WRITE( NOUT, FMT = 9997 )SUBNAM( 3 ), IINFO, N,
     $                  ITYPE, IPAR, IOLDSD
                     INFO = ABS( IINFO )
                     GO TO 590
                  END IF
*
                  S2 = DSECND( )
                  TIME = S2 - S1
                  IC = IC + 1
                  IF( TIME.LT.TIMMIN )
     $               GO TO 150
*
*                 Subtract the time used in DCOPY and DSTEDC
*
                  S1 = DSECND( )
                  DO 160 J = 1, IC
                     CALL DCOPY( N, D, 1, WORK( LDA+1 ), 1 )
                     CALL DCOPY( N-1, E, 1, WORK( 2*LDA+1 ), 1 )
                     CALL DSTEDC( 'N', N, WORK( LDA+1 ),
     $                            WORK( 2*LDA+1 ), Z1, LDA,
     $                            WORK( 3*LDA+1 ), LWEDC, IWORK, LIWEDC,
     $                            IINFO )
  160             CONTINUE
                  S2 = DSECND( )
                  UNTIME = S2 - S1
*
                  TIMES( IPAR, ITYPE, IN, 3 ) = MAX( TIME-UNTIME,
     $               ZERO ) / DBLE( IC )
                  OPCNTS( IPAR, ITYPE, IN, 3 ) = DOPLA2( 'DORMTR', UPLO,
     $               N, N, N, 0, NB )
  170          CONTINUE
            END IF
*
*           Time DSTEQR, SSTERF, DPTEQR, SSTEBZ, SSTEIN, SSTEDC, SSTERV
*           for each distinct LDA=LDAS(j)
*
            IF( TIMSUB( 4 ) .OR. TIMSUB( 5 ) .OR. TIMSUB( 6 ) .OR.
     $          TIMSUB( 7 ) .OR. TIMSUB( 8 ) .OR. TIMSUB( 9 ) .OR.
     $          TIMSUB( 10 ) .OR. TIMSUB( 11 ) .OR. TIMSUB( 12 ) .OR.
     $          TIMSUB( 13 ) .OR. TIMSUB( 14 ) .OR. TIMSUB( 15 ) .OR.
     $          TIMSUB( 16 ) ) THEN
               DO 580 IPAR = 1, NPARMS
                  LDA = LDAS( IPAR )
*
*                 If this value of LDA has come up before, just use
*                 the value previously computed.
*
                  LASTL = 0
                  DO 180 J = 1, IPAR - 1
                     IF( LDA.EQ.LDAS( J ) )
     $                  LASTL = J
  180             CONTINUE
                  IF( LASTL.EQ.0 ) THEN
*
*                    Time DSTEQR with VECT='N'
*
                     IF( TIMSUB( 4 ) ) THEN
                        IC = 0
                        OPS = ZERO
                        S1 = DSECND( )
  190                   CONTINUE
                        CALL DCOPY( N, D, 1, WORK, 1 )
                        CALL DCOPY( N-1, E, 1, WORK( LDA+1 ), 1 )
                        CALL DSTEQR( 'N', N, WORK, WORK( LDA+1 ), Z,
     $                               LDA, WORK( 2*LDA+1 ), IINFO )
                        IF( IINFO.NE.0 ) THEN
                           WRITE( NOUT, FMT = 9997 )SUBNAM( 4 ), IINFO,
     $                        N, ITYPE, IPAR, IOLDSD
                           INFO = ABS( IINFO )
                           GO TO 210
                        END IF
                        S2 = DSECND( )
                        TIME = S2 - S1
                        IC = IC + 1
                        IF( TIME.LT.TIMMIN )
     $                     GO TO 190
*
*                       Subtract the time used in DCOPY.
*
                        S1 = DSECND( )
                        DO 200 J = 1, IC
                           CALL DCOPY( N, D, 1, WORK, 1 )
                           CALL DCOPY( N-1, E, 1, WORK( LDA+1 ), 1 )
  200                   CONTINUE
                        S2 = DSECND( )
                        UNTIME = S2 - S1
*
                        TIMES( IPAR, ITYPE, IN, 4 ) = MAX( TIME-UNTIME,
     $                     ZERO ) / DBLE( IC )
                        OPCNTS( IPAR, ITYPE, IN, 4 ) = OPS / DBLE( IC )
                     END IF
*
*                    Time DSTEQR with VECT='V'
*
  210                CONTINUE
                     IF( TIMSUB( 5 ) ) THEN
                        IC = 0
                        OPS = ZERO
                        S1 = DSECND( )
  220                   CONTINUE
                        CALL DCOPY( N, D, 1, WORK, 1 )
                        CALL DCOPY( N-1, E, 1, WORK( LDA+1 ), 1 )
                        CALL DLASET( 'Full', LDA, N, ONE, TWO, Z, LDA )
                        CALL DSTEQR( 'V', N, WORK, WORK( LDA+1 ), Z,
     $                               LDA, WORK( 2*LDA+1 ), IINFO )
                        IF( IINFO.NE.0 ) THEN
                           WRITE( NOUT, FMT = 9997 )SUBNAM( 5 ), IINFO,
     $                        N, ITYPE, IPAR, IOLDSD
                           INFO = ABS( IINFO )
                           GO TO 240
                        END IF
                        S2 = DSECND( )
                        TIME = S2 - S1
                        IC = IC + 1
                        IF( TIME.LT.TIMMIN )
     $                     GO TO 220
*
*                       Subtract the time used in DCOPY.
*
                        S1 = DSECND( )
                        DO 230 J = 1, IC
                           CALL DLASET( 'Full', LDA, N, ONE, TWO, Z,
     $                                  LDA )
                           CALL DCOPY( N, D, 1, WORK, 1 )
                           CALL DCOPY( N-1, E, 1, WORK( LDA+1 ), 1 )
  230                   CONTINUE
                        S2 = DSECND( )
                        UNTIME = S2 - S1
*
                        TIMES( IPAR, ITYPE, IN, 5 ) = MAX( TIME-UNTIME,
     $                     ZERO ) / DBLE( IC )
                        OPCNTS( IPAR, ITYPE, IN, 5 ) = OPS / DBLE( IC )
                     END IF
*
*                    Time DSTERF
*
  240                CONTINUE
                     IF( TIMSUB( 6 ) ) THEN
                        IC = 0
                        OPS = ZERO
                        S1 = DSECND( )
  250                   CONTINUE
                        CALL DCOPY( N, D, 1, WORK, 1 )
                        CALL DCOPY( N-1, E, 1, WORK( LDA+1 ), 1 )
                        CALL DSTERF( N, WORK, WORK( LDA+1 ), IINFO )
                        IF( IINFO.NE.0 ) THEN
                           WRITE( NOUT, FMT = 9997 )SUBNAM( 6 ), IINFO,
     $                        N, ITYPE, IPAR, IOLDSD
                           INFO = ABS( IINFO )
                           GO TO 270
                        END IF
                        S2 = DSECND( )
                        TIME = S2 - S1
                        IC = IC + 1
                        IF( TIME.LT.TIMMIN )
     $                     GO TO 250
*
*                       Subtract the time used in DCOPY.
*
                        S1 = DSECND( )
                        DO 260 J = 1, IC
                           CALL DCOPY( N, D, 1, WORK, 1 )
                           CALL DCOPY( N-1, E, 1, WORK( LDA+1 ), 1 )
  260                   CONTINUE
                        S2 = DSECND( )
                        UNTIME = S2 - S1
*
                        TIMES( IPAR, ITYPE, IN, 6 ) = MAX( TIME-UNTIME,
     $                     ZERO ) / DBLE( IC )
                        OPCNTS( IPAR, ITYPE, IN, 6 ) = OPS / DBLE( IC )
                     END IF
*
*                    Time DPTEQR with VECT='N'
*
  270                CONTINUE
                     IF( TIMSUB( 7 ) ) THEN
*
*                       Modify the tridiagonal matrix to make it
*                       positive definite.
                        E2( 1 ) = ABS( D( 1 ) ) + ABS( E( 1 ) )
                        DO 280 I = 2, N - 1
                           E2( I ) = ABS( D( I ) ) + ABS( E( I ) ) +
     $                               ABS( E( I-1 ) )
  280                   CONTINUE
                        E2( N ) = ABS( D( N ) ) + ABS( E( N-1 ) )
                        IC = 0
                        OPS = ZERO
                        S1 = DSECND( )
  290                   CONTINUE
                        CALL DCOPY( N, E2, 1, WORK, 1 )
                        CALL DCOPY( N-1, E, 1, WORK( LDA+1 ), 1 )
                        CALL DPTEQR( 'N', N, WORK, WORK( LDA+1 ), Z,
     $                               LDA, WORK( 2*LDA+1 ), IINFO )
                        IF( IINFO.NE.0 ) THEN
                           WRITE( NOUT, FMT = 9997 )SUBNAM( 7 ), IINFO,
     $                        N, ITYPE, IPAR, IOLDSD
                           INFO = ABS( IINFO )
                           GO TO 310
                        END IF
                        S2 = DSECND( )
                        TIME = S2 - S1
                        IC = IC + 1
                        IF( TIME.LT.TIMMIN )
     $                     GO TO 290
*
*                       Subtract the time used in DCOPY.
*
                        S1 = DSECND( )
                        DO 300 J = 1, IC
                           CALL DCOPY( N, D, 1, WORK, 1 )
                           CALL DCOPY( N-1, E, 1, WORK( LDA+1 ), 1 )
  300                   CONTINUE
                        S2 = DSECND( )
                        UNTIME = S2 - S1
*
                        TIMES( IPAR, ITYPE, IN, 7 ) = MAX( TIME-UNTIME,
     $                     ZERO ) / DBLE( IC )
                        OPCNTS( IPAR, ITYPE, IN, 7 ) = OPS / DBLE( IC )
                     END IF
*
*                    Time DPTEQR with VECT='V'
*
  310                CONTINUE
                     IF( TIMSUB( 8 ) ) THEN
*
*                       Modify the tridiagonal matrix to make it
*                       positive definite.
                        E2( 1 ) = ABS( D( 1 ) ) + ABS( E( 1 ) )
                        DO 320 I = 2, N - 1
                           E2( I ) = ABS( D( I ) ) + ABS( E( I ) ) +
     $                               ABS( E( I-1 ) )
  320                   CONTINUE
                        E2( N ) = ABS( D( N ) ) + ABS( E( N-1 ) )
                        IC = 0
                        OPS = ZERO
                        S1 = DSECND( )
  330                   CONTINUE
                        CALL DCOPY( N, E2, 1, WORK, 1 )
                        CALL DCOPY( N-1, E, 1, WORK( LDA+1 ), 1 )
                        CALL DPTEQR( 'V', N, WORK, WORK( LDA+1 ), Z,
     $                               LDA, WORK( 2*LDA+1 ), IINFO )
                        IF( IINFO.NE.0 ) THEN
                           WRITE( NOUT, FMT = 9997 )SUBNAM( 8 ), IINFO,
     $                        N, ITYPE, IPAR, IOLDSD
                           INFO = ABS( IINFO )
                           GO TO 350
                        END IF
                        S2 = DSECND( )
                        TIME = S2 - S1
                        IC = IC + 1
                        IF( TIME.LT.TIMMIN )
     $                     GO TO 330
*
*                       Subtract the time used in DCOPY.
*
                        S1 = DSECND( )
                        DO 340 J = 1, IC
                           CALL DCOPY( N, D, 1, WORK, 1 )
                           CALL DCOPY( N-1, E, 1, WORK( LDA+1 ), 1 )
  340                   CONTINUE
                        S2 = DSECND( )
                        UNTIME = S2 - S1
*
                        TIMES( IPAR, ITYPE, IN, 8 ) = MAX( TIME-UNTIME,
     $                     ZERO ) / DBLE( IC )
                        OPCNTS( IPAR, ITYPE, IN, 8 ) = OPS / DBLE( IC )
                     END IF
*
*                    Time DSTEBZ(I)
*
  350                CONTINUE
                     IF( TIMSUB( 9 ) ) THEN
                        IL = 1
                        IU = N
                        ABSTOL = ZERO
                        IC = 0
                        OPS = ZERO
                        S1 = DSECND( )
  360                   CONTINUE
                        CALL DSTEBZ( 'I', 'B', N, VL, VU, IL, IU,
     $                               ABSTOL, D, E, MM, NSPLIT, WORK,
     $                               IWORK, IWORK( LDA+1 ),
     $                               WORK( 2*LDA+1 ), IWORK( 2*LDA+1 ),
     $                               IINFO )
                        IF( IINFO.NE.0 ) THEN
                           WRITE( NOUT, FMT = 9997 )SUBNAM( 9 ), IINFO,
     $                        N, ITYPE, IPAR, IOLDSD
                           INFO = ABS( IINFO )
                           GO TO 370
                        END IF
                        S2 = DSECND( )
                        TIME = S2 - S1
                        IC = IC + 1
                        IF( TIME.LT.TIMMIN )
     $                     GO TO 360
                        UNTIME = ZERO
*
                        TIMES( IPAR, ITYPE, IN, 9 ) = MAX( TIME-UNTIME,
     $                     ZERO ) / DBLE( IC )
                        OPCNTS( IPAR, ITYPE, IN, 9 ) = OPS / DBLE( IC )
                     END IF
*
*                    Time DSTEBZ(V)
*
  370                CONTINUE
                     IF( TIMSUB( 10 ) ) THEN
                        IF( N.EQ.1 ) THEN
                           VL = D( 1 ) - ABS( D( 1 ) )
                           VU = D( 1 ) + ABS( D( 1 ) )
                        ELSE
                           VL = D( 1 ) - ABS( E( 1 ) )
                           VU = D( 1 ) + ABS( E( 1 ) )
                           DO 380 I = 2, N - 1
                              VL = MIN( VL, D( I )-ABS( E( I ) )-
     $                             ABS( E( I-1 ) ) )
                              VU = MAX( VU, D( I )+ABS( E( I ) )+
     $                             ABS( E( I-1 ) ) )
  380                      CONTINUE
                           VL = MIN( VL, D( N )-ABS( E( N-1 ) ) )
                           VU = MAX( VU, D( N )+ABS( E( N-1 ) ) )
                        END IF
                        ABSTOL = ZERO
                        IC = 0
                        OPS = ZERO
                        S1 = DSECND( )
  390                   CONTINUE
                        CALL DSTEBZ( 'V', 'B', N, VL, VU, IL, IU,
     $                               ABSTOL, D, E, MM, NSPLIT, WORK,
     $                               IWORK, IWORK( LDA+1 ),
     $                               WORK( 2*LDA+1 ), IWORK( 2*LDA+1 ),
     $                               IINFO )
                        IF( IINFO.NE.0 ) THEN
                           WRITE( NOUT, FMT = 9997 )SUBNAM( 10 ),
     $                        IINFO, N, ITYPE, IPAR, IOLDSD
                           INFO = ABS( IINFO )
                           GO TO 400
                        END IF
                        S2 = DSECND( )
                        TIME = S2 - S1
                        IC = IC + 1
                        IF( TIME.LT.TIMMIN )
     $                     GO TO 390
                        UNTIME = ZERO
*
                        TIMES( IPAR, ITYPE, IN, 10 ) = MAX( TIME-UNTIME,
     $                     ZERO ) / DBLE( IC )
                        OPCNTS( IPAR, ITYPE, IN, 10 ) = OPS / DBLE( IC )
                     END IF
*
*                    Time DSTEIN
*
  400                CONTINUE
                     IF( TIMSUB( 11 ) ) THEN
                        IC = 0
                        OPS = ZERO
                        S1 = DSECND( )
  410                   CONTINUE
                        CALL DSTEIN( N, D, E, MM, WORK, IWORK,
     $                               IWORK( LDA+1 ), Z, LDA,
     $                               WORK( LDA+1 ), IWORK( 2*LDA+1 ),
     $                               IWORK( 3*LDA+1 ), IINFO )
                        IF( IINFO.NE.0 ) THEN
                           WRITE( NOUT, FMT = 9997 )SUBNAM( 11 ),
     $                        IINFO, N, ITYPE, IPAR, IOLDSD
                           INFO = ABS( IINFO )
                           GO TO 420
                        END IF
                        S2 = DSECND( )
                        TIME = S2 - S1
                        IC = IC + 1
                        IF( TIME.LT.TIMMIN )
     $                     GO TO 410
                        UNTIME = ZERO
*
                        TIMES( IPAR, ITYPE, IN, 11 ) = MAX( TIME-UNTIME,
     $                     ZERO ) / DBLE( IC )
                        OPCNTS( IPAR, ITYPE, IN, 11 ) = OPS / DBLE( IC )
                     END IF
*
*                    Time DSTEDC with COMPQ='N'
*
  420                CONTINUE
                     IF( TIMSUB( 12 ) ) THEN
                        IC = 0
                        OPS = ZERO
                        S1 = DSECND( )
  430                   CONTINUE
                        CALL DCOPY( N, D, 1, WORK, 1 )
                        CALL DCOPY( N-1, E, 1, WORK( LDA+1 ), 1 )
                        CALL DSTEDC( 'N', N, WORK, WORK( LDA+1 ), Z,
     $                               LDA, WORK( 2*LDA+1 ), LWEDC, IWORK,
     $                               LIWEDC, IINFO )
                        IF( IINFO.NE.0 ) THEN
                           WRITE( NOUT, FMT = 9997 )SUBNAM( 12 ),
     $                        IINFO, N, ITYPE, IPAR, IOLDSD
                           INFO = ABS( IINFO )
                           GO TO 450
                        END IF
                        S2 = DSECND( )
                        TIME = S2 - S1
                        IC = IC + 1
                        IF( TIME.LT.TIMMIN )
     $                     GO TO 430
*
*                       Subtract the time used in DCOPY.
*
                        S1 = DSECND( )
                        DO 440 J = 1, IC
                           CALL DCOPY( N, D, 1, WORK, 1 )
                           CALL DCOPY( N-1, E, 1, WORK( LDA+1 ), 1 )
  440                   CONTINUE
                        S2 = DSECND( )
                        UNTIME = S2 - S1
*
                        TIMES( IPAR, ITYPE, IN, 12 ) = MAX( TIME-UNTIME,
     $                     ZERO ) / DBLE( IC )
                        OPCNTS( IPAR, ITYPE, IN, 12 ) = OPS / DBLE( IC )
                     END IF
*
*                    Time DSTEDC with COMPQ='I'
*
  450                CONTINUE
                     IF( TIMSUB( 13 ) ) THEN
                        IC = 0
                        OPS = ZERO
                        S1 = DSECND( )
  460                   CONTINUE
                        CALL DCOPY( N, D, 1, WORK, 1 )
                        CALL DCOPY( N-1, E, 1, WORK( LDA+1 ), 1 )
                        CALL DLASET( 'Full', LDA, N, ONE, TWO, Z, LDA )
                        CALL DSTEDC( 'I', N, WORK, WORK( LDA+1 ), Z,
     $                               LDA, WORK( 2*LDA+1 ), LWEDC, IWORK,
     $                               LIWEDC, IINFO )
                        IF( IINFO.NE.0 ) THEN
                           WRITE( NOUT, FMT = 9997 )SUBNAM( 13 ),
     $                        IINFO, N, ITYPE, IPAR, IOLDSD
                           INFO = ABS( IINFO )
                           GO TO 480
                        END IF
                        S2 = DSECND( )
                        TIME = S2 - S1
                        IC = IC + 1
                        IF( TIME.LT.TIMMIN )
     $                     GO TO 460
*
*                       Subtract the time used in DCOPY.
*
                        S1 = DSECND( )
                        DO 470 J = 1, IC
                           CALL DLASET( 'Full', LDA, N, ONE, TWO, Z,
     $                                  LDA )
                           CALL DCOPY( N, D, 1, WORK, 1 )
                           CALL DCOPY( N-1, E, 1, WORK( LDA+1 ), 1 )
  470                   CONTINUE
                        S2 = DSECND( )
                        UNTIME = S2 - S1
*
                        TIMES( IPAR, ITYPE, IN, 13 ) = MAX( TIME-UNTIME,
     $                     ZERO ) / DBLE( IC )
                        OPCNTS( IPAR, ITYPE, IN, 13 ) = OPS / DBLE( IC )
                     END IF
  480                CONTINUE
*
*                    Time DSTEDC with COMPQ='V'
*
                     IF( TIMSUB( 14 ) ) THEN
                        IC = 0
                        OPS = ZERO
                        S1 = DSECND( )
  490                   CONTINUE
                        CALL DCOPY( N, D, 1, WORK, 1 )
                        CALL DCOPY( N-1, E, 1, WORK( LDA+1 ), 1 )
                        CALL DLASET( 'Full', LDA, N, ONE, TWO, Z, LDA )
                        CALL DSTEDC( 'V', N, WORK, WORK( LDA+1 ), Z,
     $                               LDA, WORK( 2*LDA+1 ), LWEDC, IWORK,
     $                               LIWEDC, IINFO )
                        IF( IINFO.NE.0 ) THEN
                           WRITE( NOUT, FMT = 9997 )SUBNAM( 14 ),
     $                        IINFO, N, ITYPE, IPAR, IOLDSD
                           INFO = ABS( IINFO )
                           GO TO 510
                        END IF
                        S2 = DSECND( )
                        TIME = S2 - S1
                        IC = IC + 1
                        IF( TIME.LT.TIMMIN )
     $                     GO TO 490
*
*                       Subtract the time used in DCOPY.
*
                        S1 = DSECND( )
                        DO 500 J = 1, IC
                           CALL DLASET( 'Full', LDA, N, ONE, TWO, Z,
     $                                  LDA )
                           CALL DCOPY( N, D, 1, WORK, 1 )
                           CALL DCOPY( N-1, E, 1, WORK( LDA+1 ), 1 )
  500                   CONTINUE
                        S2 = DSECND( )
                        UNTIME = S2 - S1
*
                        TIMES( IPAR, ITYPE, IN, 14 ) = MAX( TIME-UNTIME,
     $                     ZERO ) / DBLE( IC )
                        OPCNTS( IPAR, ITYPE, IN, 14 ) = OPS / DBLE( IC )
                     END IF
  510                CONTINUE
*
*                    Time DSTEGR with COMPQ='N'
*
                     IF( TIMSUB( 15 ) ) THEN
                        ABSTOL = ZERO
                        VL = ZERO
                        VU = ZERO
                        IL = 1
                        IU = N
                        IC = 0
                        OPS = ZERO
                        S1 = DSECND( )
  520                   CONTINUE
                        CALL DCOPY( N, D, 1, WORK, 1 )
                        CALL DCOPY( N-1, E, 1, WORK( LDA+1 ), 1 )
                        CALL DSTEGR( 'N', 'A', N, WORK, WORK( LDA+1 ),
     $                               VL, VU, IL, IU, ABSTOL, M,
     $                               WORK( 2*LDA+1 ), Z, LDA, IWORK,
     $                               WORK( 3*LDA+1 ), LWEVR,
     $                               IWORK( 2*LDA+1 ), LIWEVR, INFO )
                        IF( IINFO.NE.0 ) THEN
                           WRITE( NOUT, FMT = 9997 )SUBNAM( 15 ),
     $                        IINFO, N, ITYPE, IPAR, IOLDSD
                           INFO = ABS( IINFO )
                           GO TO 540
                        END IF
                        S2 = DSECND( )
                        TIME = S2 - S1
                        IC = IC + 1
                        IF( TIME.LT.TIMMIN )
     $                     GO TO 520
*
*                       Subtract the time used in DCOPY.
*
                        S1 = DSECND( )
                        DO 530 J = 1, IC
                           CALL DLASET( 'Full', LDA, N, ONE, TWO, Z,
     $                                  LDA )
                           CALL DCOPY( N, D, 1, WORK, 1 )
                           CALL DCOPY( N-1, E, 1, WORK( LDA+1 ), 1 )
  530                   CONTINUE
                        S2 = DSECND( )
                        UNTIME = S2 - S1
*
                        TIMES( IPAR, ITYPE, IN, 15 ) = MAX( TIME-UNTIME,
     $                     ZERO ) / DBLE( IC )
                        OPCNTS( IPAR, ITYPE, IN, 15 ) = OPS / DBLE( IC )
                     END IF
  540                CONTINUE
*
*                    Time DSTEGR with COMPQ='V'
*
                     IF( TIMSUB( 16 ) ) THEN
                        ABSTOL = ZERO
                        VL = ZERO
                        VU = ZERO
                        IL = 1
                        IU = N
                        IC = 0
                        OPS = ZERO
                        S1 = DSECND( )
  550                   CONTINUE
                        CALL DCOPY( N, D, 1, WORK, 1 )
                        CALL DCOPY( N-1, E, 1, WORK( LDA+1 ), 1 )
                        CALL DSTEGR( 'V', 'A', N, WORK, WORK( LDA+1 ),
     $                               VL, VU, IL, IU, ABSTOL, M,
     $                               WORK( 2*LDA+1 ), Z, LDA, IWORK,
     $                               WORK( 3*LDA+1 ), LWEVR,
     $                               IWORK( 2*LDA+1 ), LIWEVR, INFO )
                        IF( IINFO.NE.0 ) THEN
                           WRITE( NOUT, FMT = 9997 )SUBNAM( 16 ),
     $                        IINFO, N, ITYPE, IPAR, IOLDSD
                           INFO = ABS( IINFO )
                           GO TO 570
                        END IF
                        S2 = DSECND( )
                        TIME = S2 - S1
                        IC = IC + 1
                        IF( TIME.LT.TIMMIN )
     $                     GO TO 550
*
*                       Subtract the time used in DCOPY.
*
                        S1 = DSECND( )
                        DO 560 J = 1, IC
                           CALL DLASET( 'Full', LDA, N, ONE, TWO, Z,
     $                                  LDA )
                           CALL DCOPY( N, D, 1, WORK, 1 )
                           CALL DCOPY( N-1, E, 1, WORK( LDA+1 ), 1 )
  560                   CONTINUE
                        S2 = DSECND( )
                        UNTIME = S2 - S1
*
                        TIMES( IPAR, ITYPE, IN, 16 ) = MAX( TIME-UNTIME,
     $                     ZERO ) / DBLE( IC )
                        OPCNTS( IPAR, ITYPE, IN, 16 ) = OPS / DBLE( IC )
                     END IF
  570                CONTINUE
*
                  ELSE
                     IF( TIMSUB( 4 ) ) THEN
                        OPCNTS( IPAR, ITYPE, IN, 4 ) = OPCNTS( LASTL,
     $                     ITYPE, IN, 4 )
                        TIMES( IPAR, ITYPE, IN, 4 ) = TIMES( LASTL,
     $                     ITYPE, IN, 4 )
                     END IF
                     IF( TIMSUB( 5 ) ) THEN
                        OPCNTS( IPAR, ITYPE, IN, 5 ) = OPCNTS( LASTL,
     $                     ITYPE, IN, 5 )
                        TIMES( IPAR, ITYPE, IN, 5 ) = TIMES( LASTL,
     $                     ITYPE, IN, 5 )
                     END IF
                     IF( TIMSUB( 6 ) ) THEN
                        OPCNTS( IPAR, ITYPE, IN, 6 ) = OPCNTS( LASTL,
     $                     ITYPE, IN, 6 )
                        TIMES( IPAR, ITYPE, IN, 6 ) = TIMES( LASTL,
     $                     ITYPE, IN, 6 )
                     END IF
                     IF( TIMSUB( 7 ) ) THEN
                        OPCNTS( IPAR, ITYPE, IN, 7 ) = OPCNTS( LASTL,
     $                     ITYPE, IN, 7 )
                        TIMES( IPAR, ITYPE, IN, 7 ) = TIMES( LASTL,
     $                     ITYPE, IN, 7 )
                     END IF
                     IF( TIMSUB( 8 ) ) THEN
                        OPCNTS( IPAR, ITYPE, IN, 8 ) = OPCNTS( LASTL,
     $                     ITYPE, IN, 8 )
                        TIMES( IPAR, ITYPE, IN, 8 ) = TIMES( LASTL,
     $                     ITYPE, IN, 8 )
                     END IF
                     IF( TIMSUB( 9 ) ) THEN
                        OPCNTS( IPAR, ITYPE, IN, 9 ) = OPCNTS( LASTL,
     $                     ITYPE, IN, 9 )
                        TIMES( IPAR, ITYPE, IN, 9 ) = TIMES( LASTL,
     $                     ITYPE, IN, 9 )
                     END IF
                     IF( TIMSUB( 10 ) ) THEN
                        OPCNTS( IPAR, ITYPE, IN, 10 ) = OPCNTS( LASTL,
     $                     ITYPE, IN, 10 )
                        TIMES( IPAR, ITYPE, IN, 10 ) = TIMES( LASTL,
     $                     ITYPE, IN, 10 )
                     END IF
                     IF( TIMSUB( 11 ) ) THEN
                        OPCNTS( IPAR, ITYPE, IN, 11 ) = OPCNTS( LASTL,
     $                     ITYPE, IN, 11 )
                        TIMES( IPAR, ITYPE, IN, 11 ) = TIMES( LASTL,
     $                     ITYPE, IN, 11 )
                     END IF
                     IF( TIMSUB( 12 ) ) THEN
                        OPCNTS( IPAR, ITYPE, IN, 12 ) = OPCNTS( LASTL,
     $                     ITYPE, IN, 12 )
                        TIMES( IPAR, ITYPE, IN, 12 ) = TIMES( LASTL,
     $                     ITYPE, IN, 12 )
                     END IF
                     IF( TIMSUB( 13 ) ) THEN
                        OPCNTS( IPAR, ITYPE, IN, 13 ) = OPCNTS( LASTL,
     $                     ITYPE, IN, 13 )
                        TIMES( IPAR, ITYPE, IN, 13 ) = TIMES( LASTL,
     $                     ITYPE, IN, 13 )
                     END IF
                     IF( TIMSUB( 14 ) ) THEN
                        OPCNTS( IPAR, ITYPE, IN, 14 ) = OPCNTS( LASTL,
     $                     ITYPE, IN, 14 )
                        TIMES( IPAR, ITYPE, IN, 14 ) = TIMES( LASTL,
     $                     ITYPE, IN, 14 )
                     END IF
                     IF( TIMSUB( 15 ) ) THEN
                        OPCNTS( IPAR, ITYPE, IN, 15 ) = OPCNTS( LASTL,
     $                     ITYPE, IN, 15 )
                        TIMES( IPAR, ITYPE, IN, 15 ) = TIMES( LASTL,
     $                     ITYPE, IN, 15 )
                     END IF
                     IF( TIMSUB( 16 ) ) THEN
                        OPCNTS( IPAR, ITYPE, IN, 16 ) = OPCNTS( LASTL,
     $                     ITYPE, IN, 16 )
                        TIMES( IPAR, ITYPE, IN, 16 ) = TIMES( LASTL,
     $                     ITYPE, IN, 16 )
                     END IF
                  END IF
  580          CONTINUE
            END IF
  590       CONTINUE
*
*-----------------------------------------------------------------------
*
*           Time the EISPACK Routines
*
*           Skip routines if N <= 0 (EISPACK requirement)
*
            IF( N.LE.0 )
     $         GO TO 930
*
*           Time TRED1 for each LDAS(j)
*
            IF( TIMSUB( 17 ) ) THEN
               DO 630 IPAR = 1, NPARMS
                  LDA = LDAS( IPAR )
*
*                 If this value of LDA has come up before, just use
*                 the value previously computed.
*
                  LASTL = 0
                  DO 600 J = 1, IPAR - 1
                     IF( LDA.EQ.LDAS( J ) )
     $                  LASTL = J
  600             CONTINUE
*
                  IF( LASTL.EQ.0 ) THEN
*
*                    Time TRED1
*
                     IC = 0
                     OPS = ZERO
                     S1 = DSECND( )
  610                CONTINUE
                     CALL DLACPY( 'L', N, N, A, N, Z, LDA )
                     CALL TRED1( LDA, N, Z, D, E, E2 )
                     S2 = DSECND( )
                     TIME = S2 - S1
                     IC = IC + 1
                     IF( TIME.LT.TIMMIN )
     $                  GO TO 610
*
*                    Subtract the time used in DLACPY.
*
                     S1 = DSECND( )
                     DO 620 J = 1, IC
                        CALL DLACPY( 'L', N, N, A, N, Z, LDA )
  620                CONTINUE
                     S2 = DSECND( )
                     UNTIME = S2 - S1
*
                     TIMES( IPAR, ITYPE, IN, 17 ) = MAX( TIME-UNTIME,
     $                  ZERO ) / DBLE( IC )
                     OPCNTS( IPAR, ITYPE, IN, 17 ) = OPS / DBLE( IC )
                  ELSE
                     OPCNTS( IPAR, ITYPE, IN, 17 ) = OPCNTS( LASTL,
     $                  ITYPE, IN, 17 )
                     TIMES( IPAR, ITYPE, IN, 17 ) = TIMES( LASTL, ITYPE,
     $                  IN, 17 )
                  END IF
  630          CONTINUE
            ELSE
               IF( RUNTR1 ) THEN
                  CALL DLACPY( 'L', N, N, A, N, Z, LDA )
                  CALL TRED1( LDA, N, Z, D, E, E2 )
               END IF
            END IF
*
*           Time IMTQL1 for each LDAS(j)
*
            IF( TIMSUB( 18 ) ) THEN
               DO 670 IPAR = 1, NPARMS
                  LDA = LDAS( IPAR )
*
*                 If this value of LDA has come up before, just use
*                 the value previously computed.
*
                  LASTL = 0
                  DO 640 J = 1, IPAR - 1
                     IF( LDA.EQ.LDAS( J ) )
     $                  LASTL = J
  640             CONTINUE
*
                  IF( LASTL.EQ.0 ) THEN
*
*                    Time IMTQL1
*
                     IC = 0
                     OPS = ZERO
                     S1 = DSECND( )
  650                CONTINUE
                     CALL DCOPY( N, D, 1, WORK, 1 )
                     CALL DCOPY( N-1, E, 1, WORK( LDA+1 ), 1 )
                     CALL IMTQL1( N, WORK, WORK( LDA+1 ), IINFO )
                     IF( IINFO.NE.0 ) THEN
                        WRITE( NOUT, FMT = 9997 )SUBNAM( 18 ), IINFO,
     $                     N, ITYPE, IPAR, IOLDSD
                        INFO = ABS( IINFO )
                        GO TO 680
                     END IF
                     S2 = DSECND( )
                     TIME = S2 - S1
                     IC = IC + 1
                     IF( TIME.LT.TIMMIN )
     $                  GO TO 650
*
*                    Subtract the time used in DCOPY.
*
                     S1 = DSECND( )
                     DO 660 J = 1, IC
                        CALL DCOPY( N, D, 1, WORK, 1 )
                        CALL DCOPY( N-1, E, 1, WORK( LDA+1 ), 1 )
  660                CONTINUE
                     S2 = DSECND( )
                     UNTIME = S2 - S1
*
                     TIMES( IPAR, ITYPE, IN, 18 ) = MAX( TIME-UNTIME,
     $                  ZERO ) / DBLE( IC )
                     OPCNTS( IPAR, ITYPE, IN, 18 ) = OPS / DBLE( IC )
                  ELSE
                     OPCNTS( IPAR, ITYPE, IN, 18 ) = OPCNTS( LASTL,
     $                  ITYPE, IN, 18 )
                     TIMES( IPAR, ITYPE, IN, 18 ) = TIMES( LASTL, ITYPE,
     $                  IN, 18 )
                  END IF
  670          CONTINUE
            END IF
*
*           Time IMTQL2 for each LDAS(j)
*
  680       CONTINUE
            IF( TIMSUB( 19 ) ) THEN
               DO 720 IPAR = 1, NPARMS
                  LDA = LDAS( IPAR )
*
*                 If this value of LDA has come up before, just use
*                 the value previously computed.
*
                  LASTL = 0
                  DO 690 J = 1, IPAR - 1
                     IF( LDA.EQ.LDAS( J ) )
     $                  LASTL = J
  690             CONTINUE
*
                  IF( LASTL.EQ.0 ) THEN
*
*                    Time IMTQL2
*
                     IC = 0
                     OPS = ZERO
                     S1 = DSECND( )
  700                CONTINUE
                     CALL DCOPY( N, D, 1, WORK, 1 )
                     CALL DCOPY( N-1, E, 1, WORK( LDA+1 ), 1 )
                     CALL DLASET( 'Full', N, N, ONE, TWO, Z, LDA )
                     CALL IMTQL2( LDA, N, WORK, WORK( LDA+1 ), Z,
     $                            IINFO )
                     IF( IINFO.NE.0 ) THEN
                        WRITE( NOUT, FMT = 9997 )SUBNAM( 19 ), IINFO,
     $                     N, ITYPE, IPAR, IOLDSD
                        INFO = ABS( IINFO )
                        GO TO 730
                     END IF
                     S2 = DSECND( )
                     TIME = S2 - S1
                     IC = IC + 1
                     IF( TIME.LT.TIMMIN )
     $                  GO TO 700
*
*                    Subtract the time used in DCOPY.
*
                     S1 = DSECND( )
                     DO 710 J = 1, IC
                        CALL DCOPY( N, D, 1, WORK, 1 )
                        CALL DCOPY( N-1, E, 1, WORK( LDA+1 ), 1 )
                        CALL DLASET( 'Full', N, N, ONE, TWO, Z, LDA )
  710                CONTINUE
                     S2 = DSECND( )
                     UNTIME = S2 - S1
*
                     TIMES( IPAR, ITYPE, IN, 19 ) = MAX( TIME-UNTIME,
     $                  ZERO ) / DBLE( IC )
                     OPCNTS( IPAR, ITYPE, IN, 19 ) = OPS / DBLE( IC )
                  ELSE
                     OPCNTS( IPAR, ITYPE, IN, 19 ) = OPCNTS( LASTL,
     $                  ITYPE, IN, 19 )
                     TIMES( IPAR, ITYPE, IN, 19 ) = TIMES( LASTL, ITYPE,
     $                  IN, 19 )
                  END IF
  720          CONTINUE
            END IF
*
*           Time TQLRAT for each LDAS(j)
*
  730       CONTINUE
            IF( TIMSUB( 20 ) ) THEN
               DO 770 IPAR = 1, NPARMS
                  LDA = LDAS( IPAR )
*
*                 If this value of LDA has come up before, just use
*                 the value previously computed.
*
                  LASTL = 0
                  DO 740 J = 1, IPAR - 1
                     IF( LDA.EQ.LDAS( J ) )
     $                  LASTL = J
  740             CONTINUE
*
                  IF( LASTL.EQ.0 ) THEN
*
*                    Time TQLRAT
*
                     IC = 0
                     OPS = ZERO
                     S1 = DSECND( )
  750                CONTINUE
                     CALL DCOPY( N, D, 1, WORK, 1 )
                     CALL DCOPY( N-1, E2, 1, WORK( LDA+1 ), 1 )
                     CALL TQLRAT( N, WORK, WORK( LDA+1 ), IINFO )
                     IF( IINFO.NE.0 ) THEN
                        WRITE( NOUT, FMT = 9997 )SUBNAM( 20 ), IINFO,
     $                     N, ITYPE, IPAR, IOLDSD
                        INFO = ABS( IINFO )
                        GO TO 780
                     END IF
                     S2 = DSECND( )
                     TIME = S2 - S1
                     IC = IC + 1
                     IF( TIME.LT.TIMMIN )
     $                  GO TO 750
*
*                    Subtract the time used in DCOPY.
*
                     S1 = DSECND( )
                     DO 760 J = 1, IC
                        CALL DCOPY( N, D, 1, WORK, 1 )
                        CALL DCOPY( N-1, E2, 1, WORK( LDA+1 ), 1 )
  760                CONTINUE
                     S2 = DSECND( )
                     UNTIME = S2 - S1
*
                     TIMES( IPAR, ITYPE, IN, 20 ) = MAX( TIME-UNTIME,
     $                  ZERO ) / DBLE( IC )
                     OPCNTS( IPAR, ITYPE, IN, 20 ) = OPS / DBLE( IC )
                  ELSE
                     OPCNTS( IPAR, ITYPE, IN, 20 ) = OPCNTS( LASTL,
     $                  ITYPE, IN, 20 )
                     TIMES( IPAR, ITYPE, IN, 20 ) = TIMES( LASTL, ITYPE,
     $                  IN, 20 )
                  END IF
  770          CONTINUE
            END IF
*
*           Time TRIDIB for each LDAS(j)
*
  780       CONTINUE
            IF( TIMSUB( 21 ) ) THEN
               DO 820 IPAR = 1, NPARMS
                  LDA = LDAS( IPAR )
*
*                 If this value of LDA has come up before, just use
*                 the value previously computed.
*
                  LASTL = 0
                  DO 790 J = 1, IPAR - 1
                     IF( LDA.EQ.LDAS( J ) )
     $                  LASTL = J
  790             CONTINUE
*
                  IF( LASTL.EQ.0 ) THEN
*
*                    Time TRIDIB
*
                     IC = 0
                     OPS = ZERO
                     EPS1 = ZERO
                     RLB = ZERO
                     RUB = ZERO
                     M11 = 1
                     MM = N
                     S1 = DSECND( )
  800                CONTINUE
                     CALL DCOPY( N, D, 1, WORK, 1 )
                     CALL DCOPY( N-1, E, 1, WORK( LDA+1 ), 1 )
                     CALL DCOPY( N-1, E2, 1, WORK( 2*LDA+1 ), 1 )
                     CALL TRIDIB( N, EPS1, WORK( 1 ), WORK( LDA+1 ),
     $                            WORK( 2*LDA+1 ), RLB, RUB, M11, MM,
     $                            WORK( 3*LDA+1 ), IWORK, IINFO,
     $                            WORK( 4*LDA+1 ), WORK( 5*LDA+1 ) )
                     IF( IINFO.NE.0 ) THEN
                        WRITE( NOUT, FMT = 9997 )SUBNAM( 21 ), IINFO,
     $                     N, ITYPE, IPAR, IOLDSD
                        INFO = ABS( IINFO )
                        GO TO 830
                     END IF
                     S2 = DSECND( )
                     TIME = S2 - S1
                     IC = IC + 1
                     IF( TIME.LT.TIMMIN )
     $                  GO TO 800
*
*                    Subtract the time used in DCOPY.
*
                     S1 = DSECND( )
                     DO 810 J = 1, IC
                        CALL DCOPY( N, D, 1, WORK, 1 )
                        CALL DCOPY( N-1, E, 1, WORK( LDA+1 ), 1 )
                        CALL DCOPY( N-1, E2, 1, WORK( 2*LDA+1 ), 1 )
  810                CONTINUE
                     S2 = DSECND( )
                     UNTIME = S2 - S1
*
                     TIMES( IPAR, ITYPE, IN, 21 ) = MAX( TIME-UNTIME,
     $                  ZERO ) / DBLE( IC )
                     OPCNTS( IPAR, ITYPE, IN, 21 ) = OPS / DBLE( IC )
                  ELSE
                     OPCNTS( IPAR, ITYPE, IN, 21 ) = OPCNTS( LASTL,
     $                  ITYPE, IN, 21 )
                     TIMES( IPAR, ITYPE, IN, 21 ) = TIMES( LASTL, ITYPE,
     $                  IN, 21 )
                  END IF
  820          CONTINUE
            END IF
*
*           Time BISECT for each LDAS(j)
*
  830       CONTINUE
            IF( TIMSUB( 22 ) ) THEN
               DO 880 IPAR = 1, NPARMS
                  LDA = LDAS( IPAR )
*
*                 If this value of LDA has come up before, just use
*                 the value previously computed.
*
                  LASTL = 0
                  DO 840 J = 1, IPAR - 1
                     IF( LDA.EQ.LDAS( J ) )
     $                  LASTL = J
  840             CONTINUE
*
                  IF( LASTL.EQ.0 ) THEN
*
*                    Time BISECT
*
                     VL = D( 1 ) - ABS( E( 2 ) )
                     VU = D( 1 ) + ABS( E( 2 ) )
                     DO 850 I = 2, N - 1
                        VL = MIN( VL, D( I )-ABS( E( I+1 ) )-
     $                       ABS( E( I ) ) )
                        VU = MAX( VU, D( I )+ABS( E( I+1 ) )+
     $                       ABS( E( I ) ) )
  850                CONTINUE
                     VL = MIN( VL, D( N )-ABS( E( N ) ) )
                     VU = MAX( VU, D( N )+ABS( E( N ) ) )
                     IC = 0
                     OPS = ZERO
                     EPS1 = ZERO
                     MM = N
                     MMM = 0
                     S1 = DSECND( )
  860                CONTINUE
                     CALL DCOPY( N, D, 1, WORK, 1 )
                     CALL DCOPY( N, E, 1, WORK( LDA+1 ), 1 )
                     CALL DCOPY( N, E2, 1, WORK( 2*LDA+1 ), 1 )
                     CALL BISECT( N, EPS1, WORK( 1 ), WORK( LDA+1 ),
     $                            WORK( 2*LDA+1 ), VL, VU, MM, MMM,
     $                            WORK( 3*LDA+1 ), IWORK, IINFO,
     $                            WORK( 4*LDA+1 ), WORK( 5*LDA+1 ) )
                     IF( IINFO.NE.0 ) THEN
                        WRITE( NOUT, FMT = 9997 )SUBNAM( 22 ), IINFO,
     $                     N, ITYPE, IPAR, IOLDSD
                        INFO = ABS( IINFO )
                        GO TO 890
                     END IF
                     S2 = DSECND( )
                     TIME = S2 - S1
                     IC = IC + 1
                     IF( TIME.LT.TIMMIN )
     $                  GO TO 860
*
*                    Subtract the time used in DCOPY.
*
                     S1 = DSECND( )
                     DO 870 J = 1, IC
                        CALL DCOPY( N, D, 1, WORK, 1 )
                        CALL DCOPY( N, E, 1, WORK( LDA+1 ), 1 )
                        CALL DCOPY( N, E2, 1, WORK( 2*LDA+1 ), 1 )
  870                CONTINUE
                     S2 = DSECND( )
                     UNTIME = S2 - S1
*
                     TIMES( IPAR, ITYPE, IN, 22 ) = MAX( TIME-UNTIME,
     $                  ZERO ) / DBLE( IC )
                     OPCNTS( IPAR, ITYPE, IN, 22 ) = OPS / DBLE( IC )
                  ELSE
                     OPCNTS( IPAR, ITYPE, IN, 22 ) = OPCNTS( LASTL,
     $                  ITYPE, IN, 22 )
                     TIMES( IPAR, ITYPE, IN, 22 ) = TIMES( LASTL, ITYPE,
     $                  IN, 22 )
                  END IF
  880          CONTINUE
            END IF
*
*           Time TINVIT for each LDAS(j)
*
  890       CONTINUE
            IF( TIMSUB( 23 ) ) THEN
               CALL DCOPY( N, WORK( 3*LDA+1 ), 1, WORK( 1 ), 1 )
               DO 920 IPAR = 1, NPARMS
                  LDA = LDAS( IPAR )
*
*                 If this value of LDA has come up before, just use
*                 the value previously computed.
*
                  LASTL = 0
                  DO 900 J = 1, IPAR - 1
                     IF( LDA.EQ.LDAS( J ) )
     $                  LASTL = J
  900             CONTINUE
*
                  IF( LASTL.EQ.0 ) THEN
*
*                    Time TINVIT
*
                     IC = 0
                     OPS = ZERO
                     S1 = DSECND( )
  910                CONTINUE
                     CALL TINVIT( LDA, N, D, E, E2, MMM, WORK, IWORK, Z,
     $                            IINFO, WORK( LDA+1 ), WORK( 2*LDA+1 ),
     $                            WORK( 3*LDA+1 ), WORK( 4*LDA+1 ),
     $                            WORK( 5*LDA+1 ) )
                     IF( IINFO.NE.0 ) THEN
                        WRITE( NOUT, FMT = 9997 )SUBNAM( 23 ), IINFO,
     $                     N, ITYPE, IPAR, IOLDSD
                        INFO = ABS( IINFO )
                        GO TO 930
                     END IF
                     S2 = DSECND( )
                     TIME = S2 - S1
                     IC = IC + 1
                     IF( TIME.LT.TIMMIN )
     $                  GO TO 910
                     UNTIME = ZERO
*
                     TIMES( IPAR, ITYPE, IN, 23 ) = MAX( TIME-UNTIME,
     $                  ZERO ) / DBLE( IC )
                     OPCNTS( IPAR, ITYPE, IN, 23 ) = OPS / DBLE( IC )
                  ELSE
                     OPCNTS( IPAR, ITYPE, IN, 23 ) = OPCNTS( LASTL,
     $                  ITYPE, IN, 23 )
                     TIMES( IPAR, ITYPE, IN, 23 ) = TIMES( LASTL, ITYPE,
     $                  IN, 23 )
                  END IF
  920          CONTINUE
            END IF
*
  930    CONTINUE
  940 CONTINUE
*
*-----------------------------------------------------------------------
*
*     Print a table of results for each timed routine.
*
      DO 950 ISUB = 1, NSUBS
         IF( TIMSUB( ISUB ) ) THEN
            CALL DPRTBE( SUBNAM( ISUB ), MTYPES, DOTYPE, NSIZES, NN,
     $                   INPARM( ISUB ), PNAMES, NPARMS, LDAS, NNB,
     $                   IDUMMA, IDUMMA, OPCNTS( 1, 1, 1, ISUB ), LDO1,
     $                   LDO2, TIMES( 1, 1, 1, ISUB ), LDT1, LDT2, WORK,
     $                   LLWORK, NOUT )
         END IF
  950 CONTINUE
*
 9997 FORMAT( ' DTIM22: ', A, ' returned INFO=', I6, '.', / 9X, 'N=',
     $      I6, ', ITYPE=', I6, ', IPAR=', I6, ', ISEED=(',
     $      3( I5, ',' ), I5, ')' )
*
      RETURN
*
*     End of DTIM22
*
      END
      SUBROUTINE DTIM26( LINE, NSIZES, NN, MM, NTYPES, DOTYPE, NPARMS,
     $                   NNB, LDAS, TIMMIN, NOUT, ISEED, A, H, U, VT, D,
     $                   E, TAUP, TAUQ, WORK, LWORK, IWORK, LLWORK,
     $                   TIMES, LDT1, LDT2, LDT3, OPCNTS, LDO1, LDO2,
     $                   LDO3, INFO )
*
*  -- LAPACK timing routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     June 30, 1999
*
*     .. Scalar Arguments ..
      CHARACTER*80       LINE
      INTEGER            INFO, LDO1, LDO2, LDO3, LDT1, LDT2, LDT3,
     $                   LWORK, NOUT, NPARMS, NSIZES, NTYPES
      DOUBLE PRECISION   TIMMIN
*     ..
*     .. Array Arguments ..
      LOGICAL            DOTYPE( * ), LLWORK( * )
      INTEGER            ISEED( * ), IWORK( * ), LDAS( * ), MM( * ),
     $                   NN( * ), NNB( * )
      DOUBLE PRECISION   A( * ), D( * ), E( * ), H( * ),
     $                   OPCNTS( LDO1, LDO2, LDO3, * ), TAUP( * ),
     $                   TAUQ( * ), TIMES( LDT1, LDT2, LDT3, * ),
     $                   U( * ), VT( * ), WORK( * )
*     ..
*
*  Purpose
*  =======
*
*     DTIM26 times the LAPACK routines for the DOUBLE PRECISION
*     singular value decomposition.
*
*     For each N value in NN(1:NSIZES), M value in MM(1:NSIZES),
*     and .TRUE. value in DOTYPE(1:NTYPES), a matrix will be generated
*     and used to test the selected routines.  Thus, NSIZES*(number of
*     .TRUE. values in DOTYPE) matrices will be generated.
*
*  Arguments
*  =========
*
*  LINE    (input) CHARACTER*80
*          On entry, LINE contains the input line which requested
*          this routine.  This line may contain a subroutine name,
*          such as DGEBRD, indicating that only routine SGEBRD will
*          be timed, or it may contain a generic name, such as DBD.
*          In this case, the rest of the line is scanned for the
*          first 11 non-blank characters, corresponding to the eleven
*          combinations of subroutine and options:
*          LAPACK:
*           1: DGEBRD
*              (labeled DGEBRD in the output)
*           2: DBDSQR (singular values only)
*              (labeled DBDSQR in the output)
*           3: DBDSQR (singular values and left singular vectors;
*                      assume original matrix M by N)
*              (labeled DBDSQR(L) in the output)
*           4: DBDSQR (singular values and right singular vectors;
*                      assume original matrix M by N)
*              (labeled DBDSQR(R) in the output)
*           5: DBDSQR (singular values and left and right singular
*                      vectors; assume original matrix M by N)
*              (labeled DBDSQR(B) in the output)
*           6: DBDSQR (singular value and multiply square MIN(M,N)
*                      matrix by transpose of left singular vectors)
*              (labeled DBDSQR(V) in the output)
*           7: DGEBRD+DBDSQR (singular values only)
*              (labeled LAPSVD in the output)
*           8: DGEBRD+DORGBR+DBDSQR(L) (singular values and min(M,N)
*                                       left singular vectors)
*              (labeled LAPSVD(l) in the output)
*           9: DGEBRD+DORGBR+DBDSQR(L) (singular values and M left
*                                       singular vectors)
*              (labeled LAPSVD(L) in the output)
*          10: DGEBRD+DORGBR+DBDSQR(R) (singular values and N right
*                                       singular vectors)
*              (labeled LAPSVD(R) in the output)
*          11: DGEBRD+DORGBR+DBDSQR(B) (singular values and min(M,N)
*                                       left singular vectors and N
*                                       right singular vectors)
*              (labeled LAPSVD(B) in the output)
*          12: DBDSDC (singular values and left and right singular
*                      vectors; assume original matrix min(M,N) by
*                      min(M,N))
*              (labeled DBDSDC(B) in the output)
*          13: DGESDD (singular values and min(M,N) left singular
*                      vectors and N right singular vectors if M>=N,
*                      singular values and M left singular vectors
*                      and min(M,N) right singular vectors otherwise.)
*              (labeled DGESDD(B) in the output)
*          LINPACK:
*          14: DSVDC (singular values only) (comparable to 7 above)
*              (labeled LINSVD in the output)
*          15: DSVDC (singular values and min(M,N) left singular
*                     vectors) (comparable to 8 above)
*              (labeled LINSVD(l) in the output)
*          16: DSVDC (singular values and M left singular vectors)
*                     (comparable to 9 above)
*              (labeled LINSVD(L) in the output)
*          17: DSVDC (singular values and N right singular vectors)
*                     (comparable to 10 above)
*              (labeled LINSVD(R) in the output)
*          18: DSVDC (singular values and min(M,N) left singular
*                     vectors and N right singular vectors)
*                     (comparable to 11 above)
*              (labeled LINSVD(B) in the output)
*
*          If a character is 'T' or 't', the corresponding routine in
*          this path is timed.  If the entire line is blank, all the
*          routines in the path are timed.
*
*  NSIZES  (input) INTEGER
*          The number of values of N contained in the vector NN.
*
*  NN      (input) INTEGER array, dimension( NSIZES )
*          The numbers of columns of the matrices to be tested.  For
*          each N value in the array NN, and each .TRUE. value in
*          DOTYPE, a matrix A will be generated and used to test the
*          routines.
*
*  MM      (input) INTEGER array, dimension( NSIZES )
*          The numbers of rows of the matrices to be tested.  For
*          each M value in the array MM, and each .TRUE. value in
*          DOTYPE, a matrix A will be generated and used to test the
*          routines.
*
*  NTYPES  (input) INTEGER
*          The number of types in DOTYPE.  Only the first MAXTYP
*          elements will be examined.  Exception: if NSIZES=1 and
*          NTYPES=MAXTYP+1, and DOTYPE=MAXTYP*f,t, then the input
*          value of A will be used.
*
*  DOTYPE  (input) LOGICAL
*          If DOTYPE(j) is .TRUE., then a matrix of type j will be
*          generated as follows:
*           j=1: A = U*D*V where U and V are random orthogonal
*                matrices and D has evenly spaced entries 1,...,ULP
*                with random signs on the diagonal
*           j=2: A = U*D*V where U and V are random orthogonal
*                matrices and D has geometrically spaced entries
*                1,...,ULP with random signs on the diagonal
*           j=3: A = U*D*V where U and V are random orthogonal
*                matrices and D has "clustered" entries
*                 1,ULP,...,ULP with random signs on the diagonal
*           j=4: A contains uniform random numbers from [-1,1]
*           j=5: A is a special nearly bidiagonal matrix, where the
*                upper bidiagonal entries are exp(-2*r*log(ULP))
*                and the nonbidiagonal entries are r*ULP, where r
*                is a uniform random number from [0,1]
*
*  NPARMS  (input) INTEGER
*          The number of values in each of the arrays NNB and LDAS.
*          For each matrix A generated according to NN, MM and DOTYPE,
*          tests will be run with (NB,,LDA)= (NNB(1), LDAS(1)),...,
*          (NNB(NPARMS), LDAS(NPARMS)).
*
*  NNB     (input) INTEGER array, dimension( NPARMS )
*          The values of the blocksize ("NB") to be tested.
*
*  LDAS    (input) INTEGER array, dimension( NPARMS )
*          The values of LDA, the leading dimension of all matrices,
*          to be tested.
*
*  TIMMIN  (input) DOUBLE PRECISION
*          The minimum time a subroutine will be timed.
*
*  NOUT    (input) INTEGER
*          If NOUT > 0 then NOUT specifies the unit number
*          on which the output will be printed.  If NOUT <= 0, no
*          output is printed.
*
*  ISEED   (input/output) INTEGER array, dimension( 4 )
*          The random seed used by the random number generator, used
*          by the test matrix generator.  It is used and updated on
*          each call to DTIM26.
*
*  A       (workspace) DOUBLE PRECISION array,
*                      dimension( max(NN)*max(LDAS))
*          During the testing of DGEBRD, the original dense matrix.
*
*  H       (workspace) DOUBLE PRECISION array,
*                      dimension( max(NN)*max(LDAS))
*          The Householder vectors used to reduce A to bidiagonal
*          form (as returned by DGEBD2.)
*
*  U       (workspace) DOUBLE PRECISION array,
*                      dimension( max(NN,MM)*max(LDAS) )
*          The left singular vectors of the original matrix.
*
*  VT      (workspace) DOUBLE PRECISION array,
*                      dimension( max(NN,MM)*max(LDAS) )
*          The right singular vectors of the original matrix.
*
*  D       (workspace) DOUBLE PRECISION array, dimension( max(NN,MM) )
*          Diagonal entries of bidiagonal matrix to which A
*          is reduced.
*
*  E       (workspace) DOUBLE PRECISION array, dimension( max(NN,MM) )
*          Offdiagonal entries of bidiagonal matrix to which A
*          is reduced.
*
*  TAUP    (workspace) DOUBLE PRECISION array, dimension( max(NN,MM) )
*          The coefficients for the Householder transformations
*          applied on the right to reduce A to bidiagonal form.
*
*  TAUQ    (workspace) DOUBLE PRECISION array, dimension( max(NN,MM) )
*          The coefficients for the Householder transformations
*          applied on the left to reduce A to bidiagonal form.
*
*  WORK    (workspace) DOUBLE PRECISION array, dimension( LWORK )
*
*  LWORK   (input) INTEGER
*          Number of elements in WORK. Must be at least
*          MAX(6*MIN(M,N),3*MAX(M,N),NSIZES*NPARMS*NTYPES)
*
*  IWORK   (workspace) INTEGER array, dimension at least 8*min(M,N).
*
*  LLWORK  (workspace) LOGICAL array, dimension( NPARMS ),
*
*  TIMES   (output) DOUBLE PRECISION array,
*                   dimension (LDT1,LDT2,LDT3,NSUBS)
*          TIMES(i,j,k,l) will be set to the run time (in seconds) for
*          subroutine/path l, with N=NN(k), M=MM(k), matrix type j,
*          LDA=LDAS(i), and NBLOCK=NNB(i).
*
*  LDT1    (input) INTEGER
*          The first dimension of TIMES.  LDT1 >= min( 1, NPARMS ).
*
*  LDT2    (input) INTEGER
*          The second dimension of TIMES.  LDT2 >= min( 1, NTYPES ).
*
*  LDT3    (input) INTEGER
*          The third dimension of TIMES.  LDT3 >= min( 1, NSIZES ).
*
*  OPCNTS  (output) DOUBLE PRECISION array,
*                   dimension (LDO1,LDO2,LDO3,NSUBS)
*          OPCNTS(i,j,k,l) will be set to the number of floating-point
*          operations executed by subroutine/path l, with N=NN(k),
*          M=MM(k), matrix type j, LDA=LDAS(i), and NBLOCK=NNB(i).
*
*  LDO1    (input) INTEGER
*          The first dimension of OPCNTS.  LDO1 >= min( 1, NPARMS ).
*
*  LDO2    (input) INTEGER
*          The second dimension of OPCNTS.  LDO2 >= min( 1, NTYPES ).
*
*  LDO3    (input) INTEGER
*          The third dimension of OPCNTS.  LDO3 >= min( 1, NSIZES ).
*
*  INFO    (output) INTEGER
*          Error flag.  It will be set to zero if no error occurred.
*
*  =====================================================================
*
*     .. Parameters ..
      INTEGER            MAXTYP, NSUBS
      PARAMETER          ( MAXTYP = 5, NSUBS = 18 )
      DOUBLE PRECISION   ZERO, ONE, TWO
      PARAMETER          ( ZERO = 0.0D0, ONE = 1.0D0, TWO = 2.0D0 )
*     ..
*     .. Local Scalars ..
      LOGICAL            RUNBRD, TRNBRD
      CHARACTER          UPLO
      INTEGER            IC, IINFO, IMODE, IN, IPAR, ISUB, ITYPE, J, J1,
     $                   J2, J3, J4, KU, KVT, LASTNL, LDA, LDH, M,
     $                   MINMN, MTYPES, N, NB
      DOUBLE PRECISION   CONDS, ESUM, S1, S2, TIME, ULP, ULPINV, UNTIME
*     ..
*     .. Local Arrays ..
      LOGICAL            TIMSUB( NSUBS )
      CHARACTER*4        PNAMES( 2 )
      CHARACTER*9        SUBNAM( NSUBS )
      INTEGER            INPARM( NSUBS ), IOLDSD( 4 ), JDUM( 1 ),
     $                   KMODE( 3 )
      DOUBLE PRECISION   DUM( 1 )
*     ..
*     .. External Functions ..
      DOUBLE PRECISION   DASUM, DLAMCH, DLARND, DOPLA, DSECND, DOPLA2
      EXTERNAL           DASUM, DLAMCH, DLARND, DOPLA, DSECND, DOPLA2
*     ..
*     .. External Subroutines ..
      EXTERNAL           ATIMIN, DBDSDC, DBDSQR, DCOPY, DGEBRD, DGESDD,
     $                   DLACPY, DLASET, DLATMR, DLATMS, DORGBR, DPRTBV,
     $                   DSVDC, XLAENV
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, DBLE, EXP, LOG, MAX, MIN
*     ..
*     .. Common blocks ..
      COMMON             / LATIME / OPS, ITCNT
*     ..
*     .. Scalars in Common ..
      DOUBLE PRECISION   ITCNT, OPS
*     ..
*     .. Data statements ..
      DATA               SUBNAM / 'DGEBRD', 'DBDSQR', 'DBDSQR(L)',
     $                   'DBDSQR(R)', 'DBDSQR(B)', 'DBDSQR(V)',
     $                   'LAPSVD', 'LAPSVD(l)', 'LAPSVD(L)',
     $                   'LAPSVD(R)', 'LAPSVD(B)', 'DBDSDC(B)',
     $                   'DGESDD(B)', 'LINSVD', 'LINSVD(l)',
     $                   'LINSVD(L)', 'LINSVD(R)', 'LINSVD(B)' /
      DATA               INPARM / 2, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 1, 2,
     $                   1, 1, 1, 1, 1 /
      DATA               PNAMES / 'LDA', 'NB' /
      DATA               KMODE / 4, 3, 1 /
*     ..
*     .. Executable Statements ..
*
*
*     Extract the timing request from the input line.
*
      CALL ATIMIN( 'DBD', LINE, NSUBS, SUBNAM, TIMSUB, NOUT, INFO )
      IF( INFO.NE.0 )
     $   RETURN
*
*     Check LWORK and
*     Check that N <= LDA and M <= LDA for the input values.
*
      DO 20 J2 = 1, NSIZES
         IF( LWORK.LT.MAX( 6*MIN( MM( J2 ), NN( J2 ) ), 3*MAX( MM( J2 ),
     $       NN( J2 ) ), NSIZES*NPARMS*NTYPES ) ) THEN
            INFO = -22
            WRITE( NOUT, FMT = 9999 )LINE( 1: 6 )
            RETURN
         END IF
         DO 10 J1 = 1, NPARMS
            IF( MAX( NN( J2 ), MM( J2 ) ).GT.LDAS( J1 ) ) THEN
               INFO = -9
               WRITE( NOUT, FMT = 9999 )LINE( 1: 6 )
 9999          FORMAT( 1X, A, ' timing run not attempted', / )
               RETURN
            END IF
   10    CONTINUE
   20 CONTINUE
*
*     Check to see whether DGEBRD must be run.
*
*     RUNBRD -- if DGEBRD must be run without timing.
*     TRNBRD -- if DGEBRD must be run with timing.
*
      RUNBRD = .FALSE.
      TRNBRD = .FALSE.
      IF( TIMSUB( 2 ) .OR. TIMSUB( 3 ) .OR. TIMSUB( 4 ) .OR.
     $    TIMSUB( 5 ) .OR. TIMSUB( 6 ) )RUNBRD = .TRUE.
      IF( TIMSUB( 1 ) )
     $   RUNBRD = .FALSE.
      IF( TIMSUB( 7 ) .OR. TIMSUB( 8 ) .OR. TIMSUB( 9 ) .OR.
     $    TIMSUB( 10 ) .OR. TIMSUB( 11 ) )TRNBRD = .TRUE.
*
*     Various Constants
*
      ULP = DLAMCH( 'Epsilon' )*DLAMCH( 'Base' )
      ULPINV = ONE / ULP
      CALL XLAENV( 9, 25 )
*
*     Zero out OPCNTS, TIMES
*
      DO 60 J4 = 1, NSUBS
         DO 50 J3 = 1, NSIZES
            DO 40 J2 = 1, NTYPES
               DO 30 J1 = 1, NPARMS
                  OPCNTS( J1, J2, J3, J4 ) = ZERO
                  TIMES( J1, J2, J3, J4 ) = ZERO
   30          CONTINUE
   40       CONTINUE
   50    CONTINUE
   60 CONTINUE
*
*     Do for each value of N:
*
      DO 750 IN = 1, NSIZES
*
         N = NN( IN )
         M = MM( IN )
         MINMN = MIN( M, N )
         IF( M.GE.N ) THEN
            UPLO = 'U'
            KU = MINMN
            KVT = MAX( MINMN-1, 0 )
         ELSE
            UPLO = 'L'
            KU = MAX( MINMN-1, 0 )
            KVT = MINMN
         END IF
*
*        Do for each .TRUE. value in DOTYPE:
*
         MTYPES = MIN( MAXTYP, NTYPES )
         IF( NTYPES.EQ.MAXTYP+1 .AND. NSIZES.EQ.1 )
     $      MTYPES = NTYPES
         DO 740 ITYPE = 1, MTYPES
            IF( .NOT.DOTYPE( ITYPE ) )
     $         GO TO 740
*
*           Save random number seed for error messages
*
            DO 70 J = 1, 4
               IOLDSD( J ) = ISEED( J )
   70       CONTINUE
*
*-----------------------------------------------------------------------
*
*           Time the LAPACK Routines
*
*           Generate A
*
            IF( ITYPE.LE.MAXTYP ) THEN
               IF( ITYPE.GE.1 .AND. ITYPE.LE.3 ) THEN
                  IMODE = KMODE( ITYPE )
                  CALL DLATMS( M, N, 'U', ISEED, 'N', D, IMODE, ULPINV,
     $                         ONE, M, N, 'N', A, M, WORK, INFO )
               ELSE IF( ITYPE.GE.4 .AND. ITYPE.LE.5 ) THEN
                  IF( ITYPE.EQ.4 )
     $               CONDS = -ONE
                  IF( ITYPE.EQ.5 )
     $               CONDS = ULP
                  CALL DLATMR( M, N, 'S', ISEED, 'N', D, 6, ZERO, ONE,
     $                         'T', 'N', D, 0, ONE, D, 0, ONE, 'N',
     $                         JDUM, M, N, ZERO, CONDS, 'N', A, M, JDUM,
     $                         INFO )
                  IF( ITYPE.EQ.5 ) THEN
                     CONDS = -TWO*LOG( ULP )
                     DO 80 J = 1, ( MINMN-1 )*M + MINMN, M + 1
                        A( J ) = EXP( CONDS*DLARND( 1, ISEED ) )
   80                CONTINUE
                     IF( M.GE.N ) THEN
                        DO 90 J = M + 1, ( MINMN-1 )*M + MINMN - 1,
     $                          M + 1
                           A( J ) = EXP( CONDS*DLARND( 1, ISEED ) )
   90                   CONTINUE
                     ELSE
                        DO 100 J = 2, ( MINMN-2 )*M + MINMN, M + 1
                           A( J ) = EXP( CONDS*DLARND( 1, ISEED ) )
  100                   CONTINUE
                     END IF
                  END IF
               END IF
            END IF
*
*           Time DGEBRD for each pair NNB(j), LDAS(j)
*
            IF( TIMSUB( 1 ) .OR. TRNBRD ) THEN
               DO 130 IPAR = 1, NPARMS
                  LDA = LDAS( IPAR )
                  NB = MIN( N, NNB( IPAR ) )
                  CALL XLAENV( 1, NB )
                  CALL XLAENV( 2, 2 )
                  CALL XLAENV( 3, NB )
*
*                 Time DGEBRD
*
                  IC = 0
                  OPS = ZERO
                  S1 = DSECND( )
  110             CONTINUE
                  CALL DLACPY( 'Full', M, N, A, M, H, LDA )
                  CALL DGEBRD( M, N, H, LDA, D, E, TAUQ, TAUP, WORK,
     $                         LWORK, IINFO )
                  IF( IINFO.NE.0 ) THEN
                     WRITE( NOUT, FMT = 9998 )SUBNAM( 1 ), IINFO, M, N,
     $                  ITYPE, IPAR, IOLDSD
                     INFO = ABS( IINFO )
                     GO TO 740
                  END IF
*
                  S2 = DSECND( )
                  TIME = S2 - S1
                  IC = IC + 1
                  IF( TIME.LT.TIMMIN )
     $               GO TO 110
*
*                 Subtract the time used in DLACPY.
*
                  S1 = DSECND( )
                  DO 120 J = 1, IC
                     CALL DLACPY( 'Full', M, N, A, M, U, LDA )
  120             CONTINUE
                  S2 = DSECND( )
                  UNTIME = S2 - S1
*
                  TIMES( IPAR, ITYPE, IN, 1 ) = MAX( TIME-UNTIME,
     $               ZERO ) / DBLE( IC )
                  OPCNTS( IPAR, ITYPE, IN, 1 ) = DOPLA( 'DGEBRD', M, N,
     $               0, 0, NB )
  130          CONTINUE
               LDH = LDA
            ELSE
               IF( RUNBRD ) THEN
                  CALL DLACPY( 'Full', M, N, A, M, H, M )
                  CALL DGEBRD( M, N, H, M, D, E, TAUQ, TAUP, WORK,
     $                         LWORK, IINFO )
                  IF( IINFO.NE.0 ) THEN
                     WRITE( NOUT, FMT = 9998 )SUBNAM( 1 ), IINFO, M, N,
     $                  ITYPE, 0, IOLDSD
                     INFO = ABS( IINFO )
                     GO TO 740
                  END IF
                  LDH = M
               END IF
            END IF
*
*           Time DBDSQR (singular values only) for each pair
*           NNB(j), LDAS(j)
*
            IF( TIMSUB( 2 ) .OR. TIMSUB( 7 ) ) THEN
               DO 170 IPAR = 1, NPARMS
                  LDA = LDAS( IPAR )
                  NB = MIN( N, NNB( IPAR ) )
*
*                 If this value of LDA has been used before, just
*                 use that value
*
                  LASTNL = 0
                  DO 140 J = 1, IPAR - 1
                     IF( LDA.EQ.LDAS( J ) )
     $                  LASTNL = J
  140             CONTINUE
*
                  IF( LASTNL.EQ.0 ) THEN
*
*                    Time DBDSQR (singular values only)
*
                     IC = 0
                     OPS = ZERO
                     S1 = DSECND( )
  150                CONTINUE
                     CALL DCOPY( MINMN, D, 1, WORK, 1 )
                     CALL DCOPY( MINMN-1, E, 1, WORK( MINMN+1 ), 1 )
                     CALL DBDSQR( UPLO, MINMN, 0, 0, 0, WORK,
     $                            WORK( MINMN+1 ), VT, LDA, U, LDA, U,
     $                            LDA, WORK( 2*MINMN+1 ), IINFO )
                     IF( IINFO.NE.0 ) THEN
                        WRITE( NOUT, FMT = 9998 )SUBNAM( 2 ), IINFO, M,
     $                     N, ITYPE, IPAR, IOLDSD
                        INFO = ABS( IINFO )
                        GO TO 740
                     END IF
                     S2 = DSECND( )
                     TIME = S2 - S1
                     IC = IC + 1
                     IF( TIME.LT.TIMMIN )
     $                  GO TO 150
*
*                    Subtract the time used in DLACPY.
*
                     S1 = DSECND( )
                     DO 160 J = 1, IC
                        CALL DCOPY( MINMN, D, 1, WORK, 1 )
                        CALL DCOPY( MINMN-1, E, 1, WORK( MINMN+1 ), 1 )
  160                CONTINUE
                     S2 = DSECND( )
                     UNTIME = S2 - S1
*
                     TIMES( IPAR, ITYPE, IN, 2 ) = MAX( TIME-UNTIME,
     $                  ZERO ) / DBLE( IC )
                     OPCNTS( IPAR, ITYPE, IN, 2 ) = OPS / DBLE( IC )
*
                  ELSE
*
                     TIMES( IPAR, ITYPE, IN, 2 ) = TIMES( LASTNL, ITYPE,
     $                  IN, 2 )
                     OPCNTS( IPAR, ITYPE, IN, 2 ) = OPCNTS( LASTNL,
     $                  ITYPE, IN, 2 )
                  END IF
  170          CONTINUE
            END IF
*
*           Time DBDSQR (singular values and left singular vectors,
*           assume original matrix square) for each pair NNB(j), LDAS(j)
*
            IF( TIMSUB( 3 ) .OR. TIMSUB( 8 ) .OR. TIMSUB( 9 ) ) THEN
               DO 210 IPAR = 1, NPARMS
                  LDA = LDAS( IPAR )
                  NB = MIN( N, NNB( IPAR ) )
*
*                 If this value of LDA has been used before, just
*                 use that value
*
                  LASTNL = 0
                  DO 180 J = 1, IPAR - 1
                     IF( LDA.EQ.LDAS( J ) )
     $                  LASTNL = J
  180             CONTINUE
*
                  IF( LASTNL.EQ.0 ) THEN
*
*                    Time DBDSQR (singular values and left singular
*                    vectors, assume original matrix square)
*
                     IC = 0
                     OPS = ZERO
                     S1 = DSECND( )
  190                CONTINUE
                     CALL DLASET( 'Full', M, MINMN, ONE, TWO, U, LDA )
                     CALL DCOPY( MINMN, D, 1, WORK, 1 )
                     CALL DCOPY( MINMN-1, E, 1, WORK( MINMN+1 ), 1 )
                     CALL DBDSQR( UPLO, MINMN, 0, M, 0, WORK,
     $                            WORK( MINMN+1 ), VT, LDA, U, LDA, U,
     $                            LDA, WORK( 2*MINMN+1 ), IINFO )
                     IF( IINFO.NE.0 ) THEN
                        WRITE( NOUT, FMT = 9998 )SUBNAM( 3 ), IINFO, M,
     $                     N, ITYPE, IPAR, IOLDSD
                        INFO = ABS( IINFO )
                        GO TO 740
                     END IF
                     S2 = DSECND( )
                     TIME = S2 - S1
                     IC = IC + 1
                     IF( TIME.LT.TIMMIN )
     $                  GO TO 190
*
*                    Subtract the time used in DLACPY.
*
                     S1 = DSECND( )
                     DO 200 J = 1, IC
                        CALL DLASET( 'Full', M, MINMN, ONE, TWO, U,
     $                               LDA )
                        CALL DCOPY( MINMN, D, 1, WORK, 1 )
                        CALL DCOPY( MINMN-1, E, 1, WORK( MINMN+1 ), 1 )
  200                CONTINUE
                     S2 = DSECND( )
                     UNTIME = S2 - S1
*
                     TIMES( IPAR, ITYPE, IN, 3 ) = MAX( TIME-UNTIME,
     $                  ZERO ) / DBLE( IC )
                     OPCNTS( IPAR, ITYPE, IN, 3 ) = OPS / DBLE( IC )
*
                  ELSE
*
                     TIMES( IPAR, ITYPE, IN, 3 ) = TIMES( LASTNL, ITYPE,
     $                  IN, 3 )
                     OPCNTS( IPAR, ITYPE, IN, 3 ) = OPCNTS( LASTNL,
     $                  ITYPE, IN, 3 )
                  END IF
  210          CONTINUE
            END IF
*
*           Time DBDSQR (singular values and right singular vectors,
*           assume original matrix square) for each pair NNB(j), LDAS(j)
*
            IF( TIMSUB( 4 ) .OR. TIMSUB( 10 ) ) THEN
               DO 250 IPAR = 1, NPARMS
                  LDA = LDAS( IPAR )
                  NB = MIN( N, NNB( IPAR ) )
*
*                 If this value of LDA has been used before, just
*                 use that value
*
                  LASTNL = 0
                  DO 220 J = 1, IPAR - 1
                     IF( LDA.EQ.LDAS( J ) )
     $                  LASTNL = J
  220             CONTINUE
*
                  IF( LASTNL.EQ.0 ) THEN
*
*                    Time DBDSQR (singular values and right singular
*                    vectors, assume original matrix square)
*
                     IC = 0
                     OPS = ZERO
                     S1 = DSECND( )
  230                CONTINUE
                     CALL DLASET( 'Full', MINMN, N, ONE, TWO, VT, LDA )
                     CALL DCOPY( MINMN, D, 1, WORK, 1 )
                     CALL DCOPY( MINMN-1, E, 1, WORK( MINMN+1 ), 1 )
                     CALL DBDSQR( UPLO, MINMN, N, 0, 0, WORK,
     $                            WORK( MINMN+1 ), VT, LDA, U, LDA, U,
     $                            LDA, WORK( 2*MINMN+1 ), IINFO )
                     IF( IINFO.NE.0 ) THEN
                        WRITE( NOUT, FMT = 9998 )SUBNAM( 4 ), IINFO, M,
     $                     N, ITYPE, IPAR, IOLDSD
                        INFO = ABS( IINFO )
                        GO TO 740
                     END IF
                     S2 = DSECND( )
                     TIME = S2 - S1
                     IC = IC + 1
                     IF( TIME.LT.TIMMIN )
     $                  GO TO 230
*
*                    Subtract the time used in DLACPY.
*
                     S1 = DSECND( )
                     DO 240 J = 1, IC
                        CALL DLASET( 'Full', MINMN, N, ONE, TWO, VT,
     $                               LDA )
                        CALL DCOPY( MINMN, D, 1, WORK, 1 )
                        CALL DCOPY( MINMN-1, E, 1, WORK( MINMN+1 ), 1 )
  240                CONTINUE
                     S2 = DSECND( )
                     UNTIME = S2 - S1
*
                     TIMES( IPAR, ITYPE, IN, 4 ) = MAX( TIME-UNTIME,
     $                  ZERO ) / DBLE( IC )
                     OPCNTS( IPAR, ITYPE, IN, 4 ) = OPS / DBLE( IC )
*
                  ELSE
*
                     TIMES( IPAR, ITYPE, IN, 4 ) = TIMES( LASTNL, ITYPE,
     $                  IN, 4 )
                     OPCNTS( IPAR, ITYPE, IN, 4 ) = OPCNTS( LASTNL,
     $                  ITYPE, IN, 4 )
                  END IF
  250          CONTINUE
            END IF
*
*           Time DBDSQR (singular values and left and right singular
*           vectors,assume original matrix square) for each pair
*           NNB(j), LDAS(j)
*
            IF( TIMSUB( 5 ) .OR. TIMSUB( 11 ) ) THEN
               DO 290 IPAR = 1, NPARMS
                  LDA = LDAS( IPAR )
                  NB = MIN( N, NNB( IPAR ) )
*
*                 If this value of LDA has been used before, just
*                 use that value
*
                  LASTNL = 0
                  DO 260 J = 1, IPAR - 1
                     IF( LDA.EQ.LDAS( J ) )
     $                  LASTNL = J
  260             CONTINUE
*
                  IF( LASTNL.EQ.0 ) THEN
*
*                    Time DBDSQR (singular values and left and right
*                    singular vectors, assume original matrix square)
*
                     IC = 0
                     OPS = ZERO
                     S1 = DSECND( )
  270                CONTINUE
                     CALL DLASET( 'Full', MINMN, N, ONE, TWO, VT, LDA )
                     CALL DLASET( 'Full', M, MINMN, ONE, TWO, U, LDA )
                     CALL DCOPY( MINMN, D, 1, WORK, 1 )
                     CALL DCOPY( MINMN-1, E, 1, WORK( MINMN+1 ), 1 )
                     CALL DBDSQR( UPLO, MINMN, N, M, 0, WORK,
     $                            WORK( MINMN+1 ), VT, LDA, U, LDA, U,
     $                            LDA, WORK( 2*MINMN+1 ), IINFO )
                     IF( IINFO.NE.0 ) THEN
                        WRITE( NOUT, FMT = 9998 )SUBNAM( 5 ), IINFO, M,
     $                     N, ITYPE, IPAR, IOLDSD
                        INFO = ABS( IINFO )
                        GO TO 740
                     END IF
                     S2 = DSECND( )
                     TIME = S2 - S1
                     IC = IC + 1
                     IF( TIME.LT.TIMMIN )
     $                  GO TO 270
*
*                    Subtract the time used in DLACPY.
*
                     S1 = DSECND( )
                     DO 280 J = 1, IC
                        CALL DLASET( 'Full', MINMN, N, ONE, TWO, VT,
     $                               LDA )
                        CALL DLASET( 'Full', M, MINMN, ONE, TWO, U,
     $                               LDA )
                        CALL DCOPY( MINMN, D, 1, WORK, 1 )
                        CALL DCOPY( MINMN-1, E, 1, WORK( MINMN+1 ), 1 )
  280                CONTINUE
                     S2 = DSECND( )
                     UNTIME = S2 - S1
*
                     TIMES( IPAR, ITYPE, IN, 5 ) = MAX( TIME-UNTIME,
     $                  ZERO ) / DBLE( IC )
                     OPCNTS( IPAR, ITYPE, IN, 5 ) = OPS / DBLE( IC )
*
                  ELSE
*
                     TIMES( IPAR, ITYPE, IN, 5 ) = TIMES( LASTNL, ITYPE,
     $                  IN, 5 )
                     OPCNTS( IPAR, ITYPE, IN, 5 ) = OPCNTS( LASTNL,
     $                  ITYPE, IN, 5 )
                  END IF
  290          CONTINUE
            END IF
*
*           Time DBDSQR (singular values and multiply square matrix
*           by transpose of left singular vectors) for each pair
*           NNB(j), LDAS(j)
*
            IF( TIMSUB( 6 ) ) THEN
               DO 330 IPAR = 1, NPARMS
                  LDA = LDAS( IPAR )
                  NB = MIN( N, NNB( IPAR ) )
*
*                 If this value of LDA has been used before, just
*                 use that value
*
                  LASTNL = 0
                  DO 300 J = 1, IPAR - 1
                     IF( LDA.EQ.LDAS( J ) )
     $                  LASTNL = J
  300             CONTINUE
*
                  IF( LASTNL.EQ.0 ) THEN
*
*                    Time DBDSQR (singular values and multiply square
*                    matrix by transpose of left singular vectors)
*
                     IC = 0
                     OPS = ZERO
                     S1 = DSECND( )
  310                CONTINUE
                     CALL DLASET( 'Full', MINMN, MINMN, ONE, TWO, U,
     $                            LDA )
                     CALL DCOPY( MINMN, D, 1, WORK, 1 )
                     CALL DCOPY( MINMN-1, E, 1, WORK( MINMN+1 ), 1 )
                     CALL DBDSQR( UPLO, MINMN, 0, 0, MINMN, WORK,
     $                            WORK( MINMN+1 ), VT, LDA, U, LDA, U,
     $                            LDA, WORK( 2*MINMN+1 ), IINFO )
                     IF( IINFO.NE.0 ) THEN
                        WRITE( NOUT, FMT = 9998 )SUBNAM( 6 ), IINFO, M,
     $                     N, ITYPE, IPAR, IOLDSD
                        INFO = ABS( IINFO )
                        GO TO 740
                     END IF
                     S2 = DSECND( )
                     TIME = S2 - S1
                     IC = IC + 1
                     IF( TIME.LT.TIMMIN )
     $                  GO TO 310
*
*                    Subtract the time used in DLACPY.
*
                     S1 = DSECND( )
                     DO 320 J = 1, IC
                        CALL DLASET( 'Full', MINMN, MINMN, ONE, TWO, U,
     $                               LDA )
                        CALL DCOPY( MINMN, D, 1, WORK, 1 )
                        CALL DCOPY( MINMN-1, E, 1, WORK( MINMN+1 ), 1 )
  320                CONTINUE
                     S2 = DSECND( )
                     UNTIME = S2 - S1
*
                     TIMES( IPAR, ITYPE, IN, 6 ) = MAX( TIME-UNTIME,
     $                  ZERO ) / DBLE( IC )
                     OPCNTS( IPAR, ITYPE, IN, 6 ) = OPS / DBLE( IC )
*
                  ELSE
*
                     TIMES( IPAR, ITYPE, IN, 6 ) = TIMES( LASTNL, ITYPE,
     $                  IN, 6 )
                     OPCNTS( IPAR, ITYPE, IN, 6 ) = OPCNTS( LASTNL,
     $                  ITYPE, IN, 6 )
                  END IF
  330          CONTINUE
            END IF
*
*           Time DGEBRD+DBDSQR (singular values only) for each pair
*           NNB(j), LDAS(j)
*           Use previously computed timings for DGEBRD & DBDSQR
*
            IF( TIMSUB( 7 ) ) THEN
               DO 340 IPAR = 1, NPARMS
                  TIMES( IPAR, ITYPE, IN, 7 ) = TIMES( IPAR, ITYPE, IN,
     $               1 ) + TIMES( IPAR, ITYPE, IN, 2 )
                  OPCNTS( IPAR, ITYPE, IN, 7 ) = OPCNTS( IPAR, ITYPE,
     $               IN, 1 ) + OPCNTS( IPAR, ITYPE, IN, 2 )
  340          CONTINUE
            END IF
*
*           Time DGEBRD+DORGBR+DBDSQR (singular values and min(M,N)
*           left singular vectors) for each pair NNB(j), LDAS(j)
*
*           Use previously computed timings for DGEBRD & DBDSQR
*
            IF( TIMSUB( 8 ) ) THEN
               DO 370 IPAR = 1, NPARMS
                  LDA = LDAS( IPAR )
                  NB = MIN( N, NNB( IPAR ) )
                  CALL XLAENV( 1, NB )
                  CALL XLAENV( 2, 2 )
                  CALL XLAENV( 3, NB )
*
*                 Time DGEBRD+DORGBR+DBDSQR (singular values and
*                 min(M,N) left singular vectors)
*
                  IC = 0
                  OPS = ZERO
                  S1 = DSECND( )
  350             CONTINUE
                  CALL DLACPY( 'L', M, MINMN, H, LDH, U, LDA )
                  CALL DORGBR( 'Q', M, MINMN, KU, U, LDA, TAUQ, WORK,
     $                         LWORK, IINFO )
                  IF( IINFO.NE.0 ) THEN
                     WRITE( NOUT, FMT = 9998 )SUBNAM( 8 ), IINFO, M, N,
     $                  ITYPE, IPAR, IOLDSD
                     INFO = ABS( IINFO )
                     GO TO 740
                  END IF
                  S2 = DSECND( )
                  TIME = S2 - S1
                  IC = IC + 1
                  IF( TIME.LT.TIMMIN )
     $               GO TO 350
*
*                 Subtract the time used in DLACPY.
*
                  S1 = DSECND( )
                  DO 360 J = 1, IC
                     CALL DLACPY( 'L', M, MINMN, H, LDH, U, LDA )
  360             CONTINUE
                  S2 = DSECND( )
                  UNTIME = S2 - S1
*
                  TIMES( IPAR, ITYPE, IN, 8 ) = MAX( TIME-UNTIME,
     $               ZERO ) / DBLE( IC ) + TIMES( IPAR, ITYPE, IN, 1 ) +
     $               TIMES( IPAR, ITYPE, IN, 3 )
                  OPCNTS( IPAR, ITYPE, IN, 8 ) = DOPLA2( 'DORGBR', 'Q',
     $               M, MINMN, KU, 0, NB ) + OPCNTS( IPAR, ITYPE, IN,
     $               1 ) + OPCNTS( IPAR, ITYPE, IN, 3 )
  370          CONTINUE
            END IF
*
*           Time DGEBRD+DORGBR+DBDSQR (singular values and M
*           left singular vectors) for each pair NNB(j), LDAS(j)
*
*           Use previously computed timings for DGEBRD & DBDSQR
*
            IF( TIMSUB( 9 ) ) THEN
               DO 400 IPAR = 1, NPARMS
                  LDA = LDAS( IPAR )
                  NB = MIN( N, NNB( IPAR ) )
                  CALL XLAENV( 1, NB )
                  CALL XLAENV( 2, 2 )
                  CALL XLAENV( 3, NB )
*
*                 Time DGEBRD+DORGBR+DBDSQR (singular values and
*                 M left singular vectors)
*
                  IC = 0
                  OPS = ZERO
                  S1 = DSECND( )
  380             CONTINUE
                  CALL DLACPY( 'L', M, MINMN, H, LDH, U, LDA )
                  CALL DORGBR( 'Q', M, M, KU, U, LDA, TAUQ, WORK, LWORK,
     $                         IINFO )
                  IF( IINFO.NE.0 ) THEN
                     WRITE( NOUT, FMT = 9998 )SUBNAM( 9 ), IINFO, M, N,
     $                  ITYPE, IPAR, IOLDSD
                     INFO = ABS( IINFO )
                     GO TO 740
                  END IF
                  S2 = DSECND( )
                  TIME = S2 - S1
                  IC = IC + 1
                  IF( TIME.LT.TIMMIN )
     $               GO TO 380
*
*                 Subtract the time used in DLACPY.
*
                  S1 = DSECND( )
                  DO 390 J = 1, IC
                     CALL DLACPY( 'L', M, MINMN, H, LDH, U, LDA )
  390             CONTINUE
                  S2 = DSECND( )
                  UNTIME = S2 - S1
*
                  TIMES( IPAR, ITYPE, IN, 9 ) = MAX( TIME-UNTIME,
     $               ZERO ) / DBLE( IC ) + TIMES( IPAR, ITYPE, IN, 1 ) +
     $               TIMES( IPAR, ITYPE, IN, 3 )
                  OPCNTS( IPAR, ITYPE, IN, 9 ) = DOPLA2( 'DORGBR', 'Q',
     $               M, M, KU, 0, NB ) + OPCNTS( IPAR, ITYPE, IN, 1 ) +
     $               OPCNTS( IPAR, ITYPE, IN, 3 )
  400          CONTINUE
            END IF
*
*           Time DGEBRD+DORGBR+DBDSQR (singular values and N
*           right singular vectors) for each pair NNB(j), LDAS(j)
*
*           Use previously computed timings for DGEBRD & DBDSQR
*
            IF( TIMSUB( 10 ) ) THEN
               DO 430 IPAR = 1, NPARMS
                  LDA = LDAS( IPAR )
                  NB = MIN( N, NNB( IPAR ) )
                  CALL XLAENV( 1, NB )
                  CALL XLAENV( 2, 2 )
                  CALL XLAENV( 3, NB )
*
*                 Time DGEBRD+DORGBR+DBDSQR (singular values and
*                 N right singular vectors)
*
                  IC = 0
                  OPS = ZERO
                  S1 = DSECND( )
  410             CONTINUE
                  CALL DLACPY( 'U', MINMN, N, H, LDH, VT, LDA )
                  CALL DORGBR( 'P', N, N, KVT, VT, LDA, TAUP, WORK,
     $                         LWORK, IINFO )
                  IF( IINFO.NE.0 ) THEN
                     WRITE( NOUT, FMT = 9998 )SUBNAM( 10 ), IINFO, M,
     $                  N, ITYPE, IPAR, IOLDSD
                     INFO = ABS( IINFO )
                     GO TO 740
                  END IF
                  S2 = DSECND( )
                  TIME = S2 - S1
                  IC = IC + 1
                  IF( TIME.LT.TIMMIN )
     $               GO TO 410
*
*                 Subtract the time used in DLACPY.
*
                  S1 = DSECND( )
                  DO 420 J = 1, IC
                     CALL DLACPY( 'U', MINMN, N, H, LDH, VT, LDA )
  420             CONTINUE
                  S2 = DSECND( )
                  UNTIME = S2 - S1
*
                  TIMES( IPAR, ITYPE, IN, 10 ) = MAX( TIME-UNTIME,
     $               ZERO ) / DBLE( IC ) + TIMES( IPAR, ITYPE, IN, 1 ) +
     $               TIMES( IPAR, ITYPE, IN, 4 )
                  OPCNTS( IPAR, ITYPE, IN, 10 ) = DOPLA2( 'DORGBR', 'P',
     $               N, N, KVT, 0, NB ) + OPCNTS( IPAR, ITYPE, IN, 1 ) +
     $               OPCNTS( IPAR, ITYPE, IN, 4 )
  430          CONTINUE
            END IF
*
*           Time DGEBRD+DORGBR+DBDSQR (singular values and min(M,N) left
*           singular vectors and N right singular vectors) for each pair
*           NNB(j), LDAS(j)
*
*           Use previously computed timings for DGEBRD & DBDSQR
*
            IF( TIMSUB( 11 ) ) THEN
               DO 460 IPAR = 1, NPARMS
                  LDA = LDAS( IPAR )
                  NB = MIN( N, NNB( IPAR ) )
                  CALL XLAENV( 1, NB )
                  CALL XLAENV( 2, 2 )
                  CALL XLAENV( 3, NB )
*
*                 Time DGEBRD+DORGBR+DBDSQR (singular values and
*                 min(M,N) left singular vectors and N right singular
*                 vectors)
*
                  IC = 0
                  OPS = ZERO
                  S1 = DSECND( )
  440             CONTINUE
                  CALL DLACPY( 'L', M, MINMN, H, LDH, U, LDA )
                  CALL DORGBR( 'Q', M, MINMN, KU, U, LDA, TAUQ, WORK,
     $                         LWORK, IINFO )
                  IF( IINFO.NE.0 ) THEN
                     WRITE( NOUT, FMT = 9998 )SUBNAM( 11 ), IINFO, M,
     $                  N, ITYPE, IPAR, IOLDSD
                     INFO = ABS( IINFO )
                     GO TO 740
                  END IF
                  CALL DLACPY( 'U', MINMN, N, H, LDH, VT, LDA )
                  CALL DORGBR( 'P', N, N, KVT, VT, LDA, TAUP, WORK,
     $                         LWORK, IINFO )
                  IF( IINFO.NE.0 ) THEN
                     WRITE( NOUT, FMT = 9998 )SUBNAM( 11 ), IINFO, M,
     $                  N, ITYPE, IPAR, IOLDSD
                     INFO = ABS( IINFO )
                     GO TO 740
                  END IF
                  S2 = DSECND( )
                  TIME = S2 - S1
                  IC = IC + 1
                  IF( TIME.LT.TIMMIN )
     $               GO TO 440
*
*                 Subtract the time used in DLACPY.
*
                  S1 = DSECND( )
                  DO 450 J = 1, IC
                     CALL DLACPY( 'L', MINMN, MINMN, H, LDH, VT, LDA )
  450             CONTINUE
                  S2 = DSECND( )
                  UNTIME = S2 - S1
*
                  TIMES( IPAR, ITYPE, IN, 11 ) = MAX( TIME-UNTIME,
     $               ZERO ) / DBLE( IC ) + TIMES( IPAR, ITYPE, IN, 1 ) +
     $               TIMES( IPAR, ITYPE, IN, 5 )
                  OPCNTS( IPAR, ITYPE, IN, 11 ) = DOPLA2( 'DORGBR', 'Q',
     $               M, MINMN, KU, 0, NB ) + DOPLA2( 'DORGBR', 'P', N,
     $               N, KVT, 0, NB ) + OPCNTS( IPAR, ITYPE, IN, 1 ) +
     $               OPCNTS( IPAR, ITYPE, IN, 5 )
  460          CONTINUE
            END IF
*
*           Time DBDSDC (singular values and left and right singular
*           vectors,assume original matrix square) for each pair
*           NNB(j), LDAS(j)
*
            IF( TIMSUB( 12 ) ) THEN
               ESUM = DASUM( MINMN-1, E, 1 )
               IF( ESUM.EQ.ZERO ) THEN
                  CALL DLACPY( 'Full', M, N, A, M, H, M )
                  CALL DGEBRD( M, N, H, M, D, E, TAUQ, TAUP, WORK,
     $                         LWORK, IINFO )
               END IF
               DO 500 IPAR = 1, NPARMS
                  LDA = LDAS( IPAR )
                  NB = MIN( N, NNB( IPAR ) )
*
*                 If this value of LDA has been used before, just
*                 use that value
*
                  LASTNL = 0
                  DO 470 J = 1, IPAR - 1
                     IF( LDA.EQ.LDAS( J ) )
     $                  LASTNL = J
  470             CONTINUE
*
                  IF( LASTNL.EQ.0 ) THEN
*
*                    Time DBDSDC (singular values and left and right
*                    singular vectors, assume original matrix square).
*
                     IC = 0
                     OPS = ZERO
                     S1 = DSECND( )
  480                CONTINUE
                     CALL DCOPY( MINMN, D, 1, WORK, 1 )
                     CALL DCOPY( MINMN-1, E, 1, WORK( MINMN+1 ), 1 )
                     CALL DBDSDC( UPLO, 'I', MINMN, WORK,
     $                            WORK( MINMN+1 ), U, LDA, VT, LDA, DUM,
     $                            JDUM, WORK( 2*MINMN+1 ), IWORK,
     $                            IINFO )
                     IF( IINFO.NE.0 ) THEN
                        WRITE( NOUT, FMT = 9998 )SUBNAM( 12 ), IINFO,
     $                     M, N, ITYPE, IPAR, IOLDSD
                        INFO = ABS( IINFO )
                        GO TO 740
                     END IF
                     S2 = DSECND( )
                     TIME = S2 - S1
                     IC = IC + 1
                     IF( TIME.LT.TIMMIN )
     $                  GO TO 480
*
*                    Subtract the time used in DCOPY.
*
                     S1 = DSECND( )
                     DO 490 J = 1, IC
                        CALL DCOPY( MINMN, D, 1, WORK, 1 )
                        CALL DCOPY( MINMN-1, E, 1, WORK( MINMN+1 ), 1 )
  490                CONTINUE
                     S2 = DSECND( )
                     UNTIME = S2 - S1
*
                     TIMES( IPAR, ITYPE, IN, 12 ) = MAX( TIME-UNTIME,
     $                  ZERO ) / DBLE( IC )
                     OPCNTS( IPAR, ITYPE, IN, 12 ) = OPS / DBLE( IC )
*
                  ELSE
*
                     TIMES( IPAR, ITYPE, IN, 12 ) = TIMES( LASTNL,
     $                  ITYPE, IN, 12 )
                     OPCNTS( IPAR, ITYPE, IN, 12 ) = OPCNTS( LASTNL,
     $                  ITYPE, IN, 12 )
                  END IF
  500          CONTINUE
            END IF
*
*           Time DGESDD( singular values and min(M,N) left singular
*           vectors and N right singular vectors when M>=N,
*           singular values and M left singular vectors and min(M,N)
*           right singular vectors otherwise) for each pair
*           NNB(j), LDAS(j)
*
            IF( TIMSUB( 13 ) ) THEN
               DO 530 IPAR = 1, NPARMS
                  LDA = LDAS( IPAR )
                  NB = MIN( N, NNB( IPAR ) )
                  CALL XLAENV( 1, NB )
                  CALL XLAENV( 2, 2 )
                  CALL XLAENV( 3, NB )
*
*                 Time DGESDD(singular values and min(M,N) left singular
*                 vectors and N right singular vectors when M>=N;
*                 singular values and M left singular vectors and
*                 min(M,N) right singular vectors)
*
                  IC = 0
                  OPS = ZERO
                  S1 = DSECND( )
  510             CONTINUE
                  CALL DLACPY( 'Full', M, N, A, M, H, LDA )
                  CALL DGESDD( 'S', M, N, H, LDA, WORK, U, LDA, VT, LDA,
     $                         WORK( MINMN+1 ), LWORK-MINMN, IWORK,
     $                         IINFO )
                  S2 = DSECND( )
                  IF( IINFO.NE.0 ) THEN
                     WRITE( NOUT, FMT = 9998 )SUBNAM( 13 ), IINFO, M,
     $                  N, ITYPE, IPAR, IOLDSD
                     INFO = ABS( IINFO )
                     GO TO 740
                  END IF
                  TIME = S2 - S1
                  IC = IC + 1
                  IF( TIME.LT.TIMMIN )
     $               GO TO 510
*
*                 Subtract the time used in DLACPY.
*
                  S1 = DSECND( )
                  DO 520 J = 1, IC
                     CALL DLACPY( 'Full', M, N, A, M, H, LDA )
  520             CONTINUE
                  S2 = DSECND( )
                  UNTIME = S2 - S1
*
                  TIMES( IPAR, ITYPE, IN, 13 ) = MAX( TIME-UNTIME,
     $               ZERO ) / DBLE( IC )
                  OPCNTS( IPAR, ITYPE, IN, 13 ) = OPS / DBLE( IC )
  530          CONTINUE
            END IF
*
*           Time DSVDC (singular values only) for each pair
*           NNB(j), LDAS(j)
*
            IF( TIMSUB( 14 ) ) THEN
               DO 570 IPAR = 1, NPARMS
                  LDA = LDAS( IPAR )
*
*                 If this value of LDA has been used before, just
*                 use that value
*
                  LASTNL = 0
                  DO 540 J = 1, IPAR - 1
                     IF( LDA.EQ.LDAS( J ) )
     $                  LASTNL = J
  540             CONTINUE
*
                  IF( LASTNL.EQ.0 ) THEN
*
*                    Time DSVDC (singular values only)
*
                     IC = 0
                     OPS = ZERO
                     S1 = DSECND( )
  550                CONTINUE
                     CALL DLACPY( 'Full', M, N, A, M, H, LDA )
                     CALL DSVDC( H, LDA, M, N, D, E, U, LDA, VT, LDA,
     $                           WORK, 0, IINFO )
                     IF( IINFO.NE.0 ) THEN
                        WRITE( NOUT, FMT = 9998 )SUBNAM( 14 ), IINFO,
     $                     M, N, ITYPE, IPAR, IOLDSD
                        INFO = ABS( IINFO )
                        GO TO 740
                     END IF
                     S2 = DSECND( )
                     TIME = S2 - S1
                     IC = IC + 1
                     IF( TIME.LT.TIMMIN )
     $                  GO TO 550
*
*                    Subtract the time used in DLACPY.
*
                     S1 = DSECND( )
                     DO 560 J = 1, IC
                        CALL DLACPY( 'Full', M, N, A, M, H, LDA )
  560                CONTINUE
                     S2 = DSECND( )
                     UNTIME = S2 - S1
*
                     TIMES( IPAR, ITYPE, IN, 14 ) = MAX( TIME-UNTIME,
     $                  ZERO ) / DBLE( IC )
                     OPCNTS( IPAR, ITYPE, IN, 14 ) = OPS / DBLE( IC )
*
                  ELSE
*
                     TIMES( IPAR, ITYPE, IN, 14 ) = TIMES( LASTNL,
     $                  ITYPE, IN, 14 )
                     OPCNTS( IPAR, ITYPE, IN, 14 ) = OPCNTS( LASTNL,
     $                  ITYPE, IN, 14 )
                  END IF
  570          CONTINUE
            END IF
*
*           Time DSVDC (singular values and min(M,N) left singular
*           vectors) for each pair NNB(j), LDAS(j)
*
            IF( TIMSUB( 15 ) ) THEN
               DO 610 IPAR = 1, NPARMS
                  LDA = LDAS( IPAR )
*
*                 If this value of LDA has been used before, just
*                 use that value
*
                  LASTNL = 0
                  DO 580 J = 1, IPAR - 1
                     IF( LDA.EQ.LDAS( J ) )
     $                  LASTNL = J
  580             CONTINUE
*
                  IF( LASTNL.EQ.0 ) THEN
*
*                    Time DSVDC (singular values and min(M,N) left
*                    singular vectors)
*
                     IC = 0
                     OPS = ZERO
                     S1 = DSECND( )
  590                CONTINUE
                     CALL DLACPY( 'Full', M, N, A, M, H, LDA )
                     CALL DSVDC( H, LDA, M, N, D, E, U, LDA, VT, LDA,
     $                           WORK, 20, IINFO )
                     IF( IINFO.NE.0 ) THEN
                        WRITE( NOUT, FMT = 9998 )SUBNAM( 15 ), IINFO,
     $                     M, N, ITYPE, IPAR, IOLDSD
                        INFO = ABS( IINFO )
                        GO TO 740
                     END IF
                     S2 = DSECND( )
                     TIME = S2 - S1
                     IC = IC + 1
                     IF( TIME.LT.TIMMIN )
     $                  GO TO 590
*
*                    Subtract the time used in DLACPY.
*
                     S1 = DSECND( )
                     DO 600 J = 1, IC
                        CALL DLACPY( 'Full', M, N, A, M, H, LDA )
  600                CONTINUE
                     S2 = DSECND( )
                     UNTIME = S2 - S1
*
                     TIMES( IPAR, ITYPE, IN, 15 ) = MAX( TIME-UNTIME,
     $                  ZERO ) / DBLE( IC )
                     OPCNTS( IPAR, ITYPE, IN, 15 ) = OPS / DBLE( IC )
*
                  ELSE
*
                     TIMES( IPAR, ITYPE, IN, 15 ) = TIMES( LASTNL,
     $                  ITYPE, IN, 15 )
                     OPCNTS( IPAR, ITYPE, IN, 15 ) = OPCNTS( LASTNL,
     $                  ITYPE, IN, 15 )
                  END IF
  610          CONTINUE
            END IF
*
*           Time DSVDC (singular values and M left singular
*           vectors) for each pair NNB(j), LDAS(j)
*
            IF( TIMSUB( 16 ) ) THEN
               DO 650 IPAR = 1, NPARMS
                  LDA = LDAS( IPAR )
*
*                 If this value of LDA has been used before, just
*                 use that value
*
                  LASTNL = 0
                  DO 620 J = 1, IPAR - 1
                     IF( LDA.EQ.LDAS( J ) )
     $                  LASTNL = J
  620             CONTINUE
*
                  IF( LASTNL.EQ.0 ) THEN
*
*                    Time DSVDC (singular values and M left singular
*                    vectors)
*
                     IC = 0
                     OPS = ZERO
                     S1 = DSECND( )
  630                CONTINUE
                     CALL DLACPY( 'Full', M, N, A, M, H, LDA )
                     CALL DSVDC( H, LDA, M, N, D, E, U, LDA, VT, LDA,
     $                           WORK, 10, IINFO )
                     IF( IINFO.NE.0 ) THEN
                        WRITE( NOUT, FMT = 9998 )SUBNAM( 16 ), IINFO,
     $                     M, N, ITYPE, IPAR, IOLDSD
                        INFO = ABS( IINFO )
                        GO TO 740
                     END IF
                     S2 = DSECND( )
                     TIME = S2 - S1
                     IC = IC + 1
                     IF( TIME.LT.TIMMIN )
     $                  GO TO 630
*
*                    Subtract the time used in DLACPY.
*
                     S1 = DSECND( )
                     DO 640 J = 1, IC
                        CALL DLACPY( 'Full', M, N, A, M, H, LDA )
  640                CONTINUE
                     S2 = DSECND( )
                     UNTIME = S2 - S1
*
                     TIMES( IPAR, ITYPE, IN, 16 ) = MAX( TIME-UNTIME,
     $                  ZERO ) / DBLE( IC )
                     OPCNTS( IPAR, ITYPE, IN, 16 ) = OPS / DBLE( IC )
*
                  ELSE
*
                     TIMES( IPAR, ITYPE, IN, 16 ) = TIMES( LASTNL,
     $                  ITYPE, IN, 16 )
                     OPCNTS( IPAR, ITYPE, IN, 16 ) = OPCNTS( LASTNL,
     $                  ITYPE, IN, 16 )
                  END IF
  650          CONTINUE
            END IF
*
*           Time DSVDC (singular values and N right singular
*           vectors) for each pair NNB(j), LDAS(j)
*
            IF( TIMSUB( 17 ) ) THEN
               DO 690 IPAR = 1, NPARMS
                  LDA = LDAS( IPAR )
*
*                 If this value of LDA has been used before, just
*                 use that value
*
                  LASTNL = 0
                  DO 660 J = 1, IPAR - 1
                     IF( LDA.EQ.LDAS( J ) )
     $                  LASTNL = J
  660             CONTINUE
*
                  IF( LASTNL.EQ.0 ) THEN
*
*                    Time DSVDC (singular values and N right singular
*                    vectors)
*
                     IC = 0
                     OPS = ZERO
                     S1 = DSECND( )
  670                CONTINUE
                     CALL DLACPY( 'Full', M, N, A, M, H, LDA )
                     CALL DSVDC( H, LDA, M, N, D, E, U, LDA, VT, LDA,
     $                           WORK, 1, IINFO )
                     IF( IINFO.NE.0 ) THEN
                        WRITE( NOUT, FMT = 9998 )SUBNAM( 17 ), IINFO,
     $                     M, N, ITYPE, IPAR, IOLDSD
                        INFO = ABS( IINFO )
                        GO TO 740
                     END IF
                     S2 = DSECND( )
                     TIME = S2 - S1
                     IC = IC + 1
                     IF( TIME.LT.TIMMIN )
     $                  GO TO 670
*
*                    Subtract the time used in DLACPY.
*
                     S1 = DSECND( )
                     DO 680 J = 1, IC
                        CALL DLACPY( 'Full', M, N, A, M, H, LDA )
  680                CONTINUE
                     S2 = DSECND( )
                     UNTIME = S2 - S1
*
                     TIMES( IPAR, ITYPE, IN, 17 ) = MAX( TIME-UNTIME,
     $                  ZERO ) / DBLE( IC )
                     OPCNTS( IPAR, ITYPE, IN, 17 ) = OPS / DBLE( IC )
*
                  ELSE
*
                     TIMES( IPAR, ITYPE, IN, 17 ) = TIMES( LASTNL,
     $                  ITYPE, IN, 17 )
                     OPCNTS( IPAR, ITYPE, IN, 17 ) = OPCNTS( LASTNL,
     $                  ITYPE, IN, 17 )
                  END IF
  690          CONTINUE
            END IF
*
*           Time DSVDC (singular values and min(M,N) left singular
*           vectors and N right singular vectors) for each pair
*           NNB(j), LDAS(j)
*
            IF( TIMSUB( 18 ) ) THEN
               DO 730 IPAR = 1, NPARMS
                  LDA = LDAS( IPAR )
*
*                 If this value of LDA has been used before, just
*                 use that value
*
                  LASTNL = 0
                  DO 700 J = 1, IPAR - 1
                     IF( LDA.EQ.LDAS( J ) )
     $                  LASTNL = J
  700             CONTINUE
*
                  IF( LASTNL.EQ.0 ) THEN
*
*                    Time DSVDC (singular values and min(M,N) left
*                    singular vectors and N right singular vectors)
*
                     IC = 0
                     OPS = ZERO
                     S1 = DSECND( )
  710                CONTINUE
                     CALL DLACPY( 'Full', M, N, A, M, H, LDA )
                     CALL DSVDC( H, LDA, M, N, D, E, U, LDA, VT, LDA,
     $                           WORK, 21, IINFO )
                     IF( IINFO.NE.0 ) THEN
                        WRITE( NOUT, FMT = 9998 )SUBNAM( 18 ), IINFO,
     $                     M, N, ITYPE, IPAR, IOLDSD
                        INFO = ABS( IINFO )
                        GO TO 740
                     END IF
                     S2 = DSECND( )
                     TIME = S2 - S1
                     IC = IC + 1
                     IF( TIME.LT.TIMMIN )
     $                  GO TO 710
*
*                    Subtract the time used in DLACPY.
*
                     S1 = DSECND( )
                     DO 720 J = 1, IC
                        CALL DLACPY( 'Full', M, N, A, M, H, LDA )
  720                CONTINUE
                     S2 = DSECND( )
                     UNTIME = S2 - S1
*
                     TIMES( IPAR, ITYPE, IN, 18 ) = MAX( TIME-UNTIME,
     $                  ZERO ) / DBLE( IC )
                     OPCNTS( IPAR, ITYPE, IN, 18 ) = OPS / DBLE( IC )
*
                  ELSE
*
                     TIMES( IPAR, ITYPE, IN, 18 ) = TIMES( LASTNL,
     $                  ITYPE, IN, 18 )
                     OPCNTS( IPAR, ITYPE, IN, 18 ) = OPCNTS( LASTNL,
     $                  ITYPE, IN, 18 )
                  END IF
  730          CONTINUE
            END IF
*
  740    CONTINUE
  750 CONTINUE
*
*-----------------------------------------------------------------------
*
*     Print a table of results for each timed routine.
*
      DO 760 ISUB = 1, NSUBS
         IF( TIMSUB( ISUB ) ) THEN
            CALL DPRTBV( SUBNAM( ISUB ), NTYPES, DOTYPE, NSIZES, MM, NN,
     $                   INPARM( ISUB ), PNAMES, NPARMS, LDAS, NNB,
     $                   OPCNTS( 1, 1, 1, ISUB ), LDO1, LDO2,
     $                   TIMES( 1, 1, 1, ISUB ), LDT1, LDT2, WORK,
     $                   LLWORK, NOUT )
         END IF
  760 CONTINUE
*
      RETURN
*
*     End of DTIM26
*
 9998 FORMAT( ' DTIM26: ', A, ' returned INFO=', I6, '.', / 9X, 'M=',
     $      I6, ', N=', I6, ', ITYPE=', I6, ', IPAR=', I6, ',         ',
     $      '        ISEED=(', 4( I5, ',' ), I5, ')' )
*
      END
      SUBROUTINE DTIM51( LINE, NSIZES, NN, NTYPES, DOTYPE, NPARMS, NNB,
     $                   NSHFTS, NEISPS, MINNBS, MINBKS, LDAS, TIMMIN,
     $                   NOUT, ISEED, A, B, H, T, Q, Z, W, WORK, LWORK,
     $                   LLWORK, TIMES, LDT1, LDT2, LDT3, OPCNTS, LDO1,
     $                   LDO2, LDO3, INFO )
*
*  -- LAPACK timing routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     September 30, 1994
*
*     .. Scalar Arguments ..
      CHARACTER*80       LINE
      INTEGER            INFO, LDO1, LDO2, LDO3, LDT1, LDT2, LDT3,
     $                   LWORK, NOUT, NPARMS, NSIZES, NTYPES
      DOUBLE PRECISION   TIMMIN
*     ..
*     .. Array Arguments ..
      LOGICAL            DOTYPE( * ), LLWORK( * )
      INTEGER            ISEED( * ), LDAS( * ), MINBKS( * ),
     $                   MINNBS( * ), NEISPS( * ), NN( * ), NNB( * ),
     $                   NSHFTS( * )
      DOUBLE PRECISION   A( * ), B( * ), H( * ),
     $                   OPCNTS( LDO1, LDO2, LDO3, * ), Q( * ), T( * ),
     $                   TIMES( LDT1, LDT2, LDT3, * ), W( * ),
     $                   WORK( * ), Z( * )
*     ..
*
*  Purpose
*  =======
*
*  DTIM51 times the LAPACK routines for the real non-symmetric
*  generalized eigenvalue problem   A x = w B x.
*
*  For each N value in NN(1:NSIZES) and .TRUE. value in
*  DOTYPE(1:NTYPES), a pair of matrices will be generated and used to
*  test the selected routines.  Thus, NSIZES*(number of .TRUE. values
*  in DOTYPE) matrices will be generated.
*
*  Arguments
*  =========
*
*  LINE    (input) CHARACTER*80
*          The input line which requested this routine.  This line may
*          contain a subroutine name, such as DGGHRD, indicating that
*          only routine DGGHRD will be timed, or it may contain a
*          generic name, such as DHG.  In this case, the rest of the
*          line is scanned for the first 18 non-blank characters,
*          corresponding to the eighteen combinations of subroutine and
*          options:
*          LAPACK:                                     Table Heading:
*           1: DGGHRD(no Q, no Z) (+DGEQRF, etc.)      'SGGHRD(N)'
*           2: DGGHRD(Q only)     (+DGEQRF, etc.)      'SGGHRD(Q)'
*           3: DGGHRD(Z only)     (+DGEQRF, etc.)      'SGGHRD(Z)'
*           4: DGGHRD(Q and Z)    (+DGEQRF, etc.)      'SGGHRD(Q,Z)'
*           5: DHGEQZ(Eigenvalues only)                'SHGEQZ(E)'
*           6: DHGEQZ(Schur form only)                 'SHGEQZ(S)'
*           7: DHGEQZ(Schur form and Q)                'SHGEQZ(Q)'
*           8: DHGEQZ(Schur form and Z)                'SHGEQZ(Z)'
*           9: DHGEQZ(Schur form, Q and Z)             'SHGEQZ(Q,Z)'
*          10: DTGEVC(SIDE='L', HOWMNY='A')            'STGEVC(L,A)'
*          11: DTGEVC(SIDE='L', HOWMNY='B')            'STGEVC(L,B)'
*          12: DTGEVC(SIDE='R', HOWMNY='A')            'STGEVC(R,A)'
*          13: DTGEVC(SIDE='R', HOWMNY='B')            'STGEVC(R,B)'
*          EISPACK:                       Compare w/:  Table Heading:
*          14: QZHES w/ matz=.false.            1      'QZHES(F)'
*          15: QZHES w/ matz=.true.             3      'QZHES(T)'
*          16: QZIT and QZVAL w/ matz=.false.   5      'QZIT(F)'
*          17: QZIT and QZVAL w/ matz=.true.    8      'QZIT(T)'
*          18: QZVEC                           13      'QZVEC'
*          If a character is 'T' or 't', the corresponding routine in
*          this path is timed.  If the entire line is blank, all the
*          routines in the path are timed.
*
*          Note that since QZHES does more than DGGHRD, the
*          "DGGHRD" timing also includes the time for the calls
*          to DGEQRF, DORMQR, and (if Q is computed) DORGQR
*          which are necessary to get the same functionality
*          as QZHES.
*
*  NSIZES  (input) INTEGER
*          The number of values of N contained in the vector NN.
*
*  NN      (input) INTEGER array, dimension (NSIZES)
*          The values of the matrix size N to be tested.  For each
*          N value in the array NN, and each .TRUE. value in DOTYPE,
*          a matrix A will be generated and used to test the routines.
*
*  NTYPES  (input) INTEGER
*          The number of types in DOTYPE.  Only the first MAXTYP
*          elements will be examined.  Exception: if NSIZES=1 and
*          NTYPES=MAXTYP+1, and DOTYPE=MAXTYP*f,t, then the input
*          value of A will be used.
*
*  DOTYPE  (input) LOGICAL
*          If DOTYPE(j) is .TRUE., then a pair of matrices (A,B) of
*          type j will be generated.  A and B have the form  U T1 V
*          and  U T2 V , resp., where U and V are orthogonal, T1 is
*          block upper triangular (with 1x1 and 2x2 diagonal blocks),
*          and T2 is upper triangular.  T2 has random O(1) entries in
*          the strict upper triangle and ( 0, 1, 0, 1, 1, ..., 1, 0 )
*          on the diagonal, while T1 has random O(1) entries in the
*          strict (block) upper triangle, its block diagonal will have
*          the singular values:
*          (j=1)   0, 0, 1, 1, ULP,..., ULP, 0.
*          (j=2)   0, 0, 1, 1, 1-d, 1-2*d, ..., 1-(N-5)*d=ULP, 0.
*
*                                  2        N-5
*          (j=3)   0, 0, 1, 1, a, a , ..., a   =ULP, 0.
*          (j=4)   0, 0, 1, r1, r2, ..., r(N-4), 0, where r1, etc.
*                  are random numbers in (ULP,1).
*
*  NPARMS  (input) INTEGER
*          The number of values in each of the arrays NNB, NSHFTS,
*          NEISPS, and LDAS.  For each matrix A generated according to
*          NN and DOTYPE, tests will be run with (NB,NSHIFT,NEISP,LDA)=
*          (NNB(1), NSHFTS(1), NEISPS(1), LDAS(1)),...,
*          (NNB(NPARMS), NSHFTS(NPARMS), NEISPS(NPARMS), LDAS(NPARMS))
*
*  NNB     (input) INTEGER array, dimension (NPARMS)
*          The values of the blocksize ("NB") to be tested.  They must
*          be at least 1.  Currently, this is only used by DGEQRF,
*          etc., in the timing of DGGHRD.
*
*  NSHFTS  (input) INTEGER array, dimension (NPARMS)
*          The values of the number of shifts ("NSHIFT") to be tested.
*          (Currently not used.)
*
*  NEISPS  (input) INTEGER array, dimension (NPARMS)
*          The values of "NEISP", the size of largest submatrix to be
*          processed by DLAEQZ (EISPACK method), to be tested.
*          (Currently not used.)
*
*  MINNBS  (input) INTEGER array, dimension (NPARMS)
*          The values of "MINNB", the minimum size of a product of
*          transformations which may be applied as a blocked
*          transformation, to be tested.  (Currently not used.)
*
*  MINBKS  (input) INTEGER array, dimension (NPARMS)
*          The values of "MINBK", the minimum number of rows/columns
*          to be updated with a blocked transformation, to be tested.
*          (Currently not used.)
*
*  LDAS    (input) INTEGER array, dimension (NPARMS)
*          The values of LDA, the leading dimension of all matrices,
*          to be tested.
*
*  TIMMIN  (input) DOUBLE PRECISION
*          The minimum time a subroutine will be timed.
*
*  NOUT    (input) INTEGER
*          If NOUT > 0 then NOUT specifies the unit number
*          on which the output will be printed.  If NOUT <= 0, no
*          output is printed.
*
*  ISEED   (input/output) INTEGER array, dimension (4)
*          The random seed used by the random number generator, used
*          by the test matrix generator.  It is used and updated on
*          each call to DTIM51
*
*  A       (workspace) DOUBLE PRECISION array, dimension
*                      (max(NN)*max(LDAS))
*          (a) During the testing of DGGHRD, "A", the original
*              left-hand-side matrix to be tested.
*          (b) Later, "S", the Schur form of the original "A" matrix.
*
*  B       (workspace) DOUBLE PRECISION array, dimension
*                      (max(NN)*max(LDAS))
*          (a) During the testing of DGGHRD, "B", the original
*              right-hand-side matrix to be tested.
*          (b) Later, "P", the Schur form of the original "B" matrix.
*
*  H       (workspace) DOUBLE PRECISION array, dimension
*                      (max(NN)*max(LDAS))
*          (a) During the testing of DGGHRD and DHGEQZ, "H", the
*              Hessenberg form of the original "A" matrix.
*          (b) During the testing of DTGEVC, "L", the matrix of left
*              eigenvectors.
*
*  T       (workspace) DOUBLE PRECISION array, dimension
*                      (max(NN)*max(LDAS))
*          (a) During the testing of DGGHRD and DHGEQZ, "T", the
*              triangular form of the original "B" matrix.
*          (b) During the testing of DTGEVC, "R", the matrix of right
*              eigenvectors.
*
*  Q       (workspace) DOUBLE PRECISION array, dimension
*                      (max(NN)*max(LDAS))
*          The orthogonal matrix on the left generated by DGGHRD.  If
*          DHGEQZ computes only Q or Z, then that matrix is stored here.
*          If both Q and Z are computed, the Q matrix goes here.
*
*  Z       (workspace) DOUBLE PRECISION array, dimension
*                      (max(NN)*max(LDAS))
*          The orthogonal matrix on the right generated by DGGHRD.
*          If DHGEQZ computes both Q and Z, the Z matrix is stored here.
*          Also used as scratch space for timing the DLACPY calls.
*
*  W       (workspace) DOUBLE PRECISION array, dimension (3*max(LDAS))
*          Treated as an LDA x 3 matrix whose 1st and 2nd columns hold
*          ALPHAR and ALPHAI, the real and imaginary parts of the
*          diagonal entries of "S" that would result from reducing "S"
*          and "P" simultaneously to triangular form), and whose 3rd
*          column holds BETA, the diagonal entries of "P" that would so
*          result.
*
*  WORK    (workspace) DOUBLE PRECISION array, dimension (LWORK)
*
*  LWORK   (input) INTEGER
*          Number of elements in WORK.  It must be at least
*          (a)  6*max(NN)
*          (b)  NSIZES*NTYPES*NPARMS
*
*  LLWORK  (workspace) LOGICAL array, dimension (max( max(NN), NPARMS ))
*
*  TIMES   (output) DOUBLE PRECISION array, dimension
*                   (LDT1,LDT2,LDT3,NSUBS)
*          TIMES(i,j,k,l) will be set to the run time (in seconds) for
*          subroutine l, with N=NN(k), matrix type j, and LDA=LDAS(i),
*          NEISP=NEISPS(i), NBLOCK=NNB(i), NSHIFT=NSHFTS(i),
*          MINNB=MINNBS(i), and MINBLK=MINBKS(i).
*
*  LDT1    (input) INTEGER
*          The first dimension of TIMES.  LDT1 >= min( 1, NPARMS ).
*
*  LDT2    (input) INTEGER
*          The second dimension of TIMES.  LDT2 >= min( 1, NTYPES ).
*
*  LDT3    (input) INTEGER
*          The third dimension of TIMES.  LDT3 >= min( 1, NSIZES ).
*
*  OPCNTS  (output) DOUBLE PRECISION array, dimension
*                   (LDO1,LDO2,LDO3,NSUBS)
*          OPCNTS(i,j,k,l) will be set to the number of floating-point
*          operations executed by subroutine l, with N=NN(k), matrix
*          type j, and LDA=LDAS(i), NEISP=NEISPS(i), NBLOCK=NNB(i),
*          NSHIFT=NSHFTS(i), MINNB=MINNBS(i), and MINBLK=MINBKS(i).
*
*  LDO1    (input) INTEGER
*          The first dimension of OPCNTS.  LDO1 >= min( 1, NPARMS ).
*
*  LDO2    (input) INTEGER
*          The second dimension of OPCNTS.  LDO2 >= min( 1, NTYPES ).
*
*  LDO3    (input) INTEGER
*          The third dimension of OPCNTS.  LDO3 >= min( 1, NSIZES ).
*
*  INFO    (output) INTEGER
*          Error flag.  It will be set to zero if no error occurred.
*
*  =====================================================================
*
*     .. Parameters ..
      INTEGER            MAXTYP, NSUBS
      PARAMETER          ( MAXTYP = 4, NSUBS = 18 )
      DOUBLE PRECISION   ZERO, ONE
      PARAMETER          ( ZERO = 0.0D0, ONE = 1.0D0 )
*     ..
*     .. Local Scalars ..
      LOGICAL            RUNEQ, RUNES, RUNHES, RUNHRD, RUNQZ
      INTEGER            IC, IINFO, IN, IPAR, ISUB, ITEMP, ITYPE, J, J1,
     $                   J2, J3, J4, JC, JR, LASTL, LDA, LDAMIN, LDH,
     $                   LDQ, LDS, LDW, MINBLK, MINNB, MTYPES, N, N1,
     $                   NB, NBSMAX, NEISP, NMAX, NSHIFT
      DOUBLE PRECISION   S1, S2, TIME, ULP, UNTIME
*     ..
*     .. Local Arrays ..
      LOGICAL            TIMSUB( NSUBS )
      CHARACTER*6        PNAMES( 6 )
      CHARACTER*11       SUBNAM( NSUBS )
      INTEGER            INPARM( NSUBS ), IOLDSD( 4 ), KATYPE( MAXTYP )
*     ..
*     .. External Functions ..
      DOUBLE PRECISION   DLAMCH, DLARND, DOPLA, DSECND
      EXTERNAL           DLAMCH, DLARND, DOPLA, DSECND
*     ..
*     .. External Subroutines ..
      EXTERNAL           ATIMIN, DHGEQZ, DLACPY, DLAQZH, DLARFG, DLASET,
     $                   DLATM4, DORM2R, DPRTBG, DTGEVC, QZHES, QZIT,
     $                   QZVAL, QZVEC, XLAENV
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, DBLE, MAX, MIN, SIGN
*     ..
*     .. Common blocks ..
      COMMON             / LATIME / OPS, ITCNT
*     ..
*     .. Scalars in Common ..
      DOUBLE PRECISION   ITCNT, OPS
*     ..
*     .. Data statements ..
      DATA               SUBNAM / 'DGGHRD(N)', 'DGGHRD(Q)', 'DGGHRD(Z)',
     $                   'DGGHRD(Q,Z)', 'DHGEQZ(E)', 'DHGEQZ(S)',
     $                   'DHGEQZ(Q)', 'DHGEQZ(Z)', 'DHGEQZ(Q,Z)',
     $                   'DTGEVC(L,A)', 'DTGEVC(L,B)', 'DTGEVC(R,A)',
     $                   'DTGEVC(R,B)', 'QZHES(F)', 'QZHES(T)',
     $                   'QZIT(F)', 'QZIT(T)', 'QZVEC' /
      DATA               INPARM / 4*2, 5*1, 4*1, 5*1 /
      DATA               PNAMES / '   LDA', '    NB', '    NS',
     $                   ' NEISP', ' MINNB', 'MINBLK' /
      DATA               KATYPE / 5, 8, 7, 9 /
*     ..
*     .. Executable Statements ..
*
*     Quick Return
*
      INFO = 0
      IF( NSIZES.LE.0 .OR. NTYPES.LE.0 .OR. NPARMS.LE.0 )
     $   RETURN
*
*     Extract the timing request from the input line.
*
      CALL ATIMIN( 'DHG', LINE, NSUBS, SUBNAM, TIMSUB, NOUT, INFO )
      IF( INFO.NE.0 )
     $   RETURN
*
*     Compute Maximum Values
*
      NMAX = 0
      DO 10 J1 = 1, NSIZES
         NMAX = MAX( NMAX, NN( J1 ) )
   10 CONTINUE
*
      LDAMIN = 2*MAX( 1, NMAX )
      NBSMAX = 0
      DO 20 J1 = 1, NPARMS
         LDAMIN = MIN( LDAMIN, LDAS( J1 ) )
         NBSMAX = MAX( NBSMAX, NNB( J1 )+NSHFTS( J1 ) )
   20 CONTINUE
*
*     Check that N <= LDA for the input values.
*
      IF( NMAX.GT.LDAMIN ) THEN
         INFO = -12
         WRITE( NOUT, FMT = 9999 )LINE( 1: 6 )
 9999    FORMAT( 1X, A, ' timing run not attempted -- N > LDA', / )
         RETURN
      END IF
*
*     Check LWORK
*
      IF( LWORK.LT.MAX( ( NBSMAX+1 )*( 2*NBSMAX+NMAX+1 ), 6*NMAX,
     $    NSIZES*NTYPES*NPARMS ) ) THEN
         INFO = -24
         WRITE( NOUT, FMT = 9998 )LINE( 1: 6 )
 9998    FORMAT( 1X, A, ' timing run not attempted -- LWORK too small.',
     $         / )
         RETURN
      END IF
*
*     Check to see whether DGGHRD or DHGEQZ must be run.
*        RUNHRD -- if DGGHRD must be run.
*        RUNES  -- if DHGEQZ must be run to get Schur form.
*        RUNEQ  -- if DHGEQZ must be run to get Schur form and Q.
*
      RUNHRD = .FALSE.
      RUNES = .FALSE.
      RUNEQ = .FALSE.
*
      IF( TIMSUB( 10 ) .OR. TIMSUB( 12 ) )
     $   RUNES = .TRUE.
      IF( TIMSUB( 11 ) .OR. TIMSUB( 13 ) )
     $   RUNEQ = .TRUE.
      IF( TIMSUB( 5 ) .OR. TIMSUB( 6 ) .OR. TIMSUB( 7 ) .OR.
     $    TIMSUB( 8 ) .OR. TIMSUB( 9 ) .OR. RUNES .OR. RUNEQ )
     $    RUNHRD = .TRUE.
*
      IF( TIMSUB( 6 ) .OR. TIMSUB( 7 ) .OR. TIMSUB( 8 ) .OR.
     $    TIMSUB( 9 ) .OR. RUNEQ )RUNES = .FALSE.
      IF( TIMSUB( 7 ) .OR. TIMSUB( 8 ) .OR. TIMSUB( 9 ) )
     $   RUNEQ = .FALSE.
      IF( TIMSUB( 1 ) .OR. TIMSUB( 2 ) .OR. TIMSUB( 3 ) .OR.
     $    TIMSUB( 4 ) )RUNHRD = .FALSE.
*
*     Check to see whether QZHES or QZIT must be run.
*
*     RUNHES -- if QZHES must be run.
*     RUNQZ  -- if QZIT and QZVAL must be run (w/ MATZ=.TRUE.).
*
      RUNHES = .FALSE.
      RUNQZ = .FALSE.
*
      IF( TIMSUB( 18 ) )
     $   RUNQZ = .TRUE.
      IF( TIMSUB( 16 ) .OR. TIMSUB( 17 ) .OR. RUNQZ )
     $   RUNHES = .TRUE.
      IF( TIMSUB( 17 ) )
     $   RUNQZ = .FALSE.
      IF( TIMSUB( 14 ) .OR. TIMSUB( 15 ) )
     $   RUNHES = .FALSE.
*
*     Various Constants
*
      ULP = DLAMCH( 'Epsilon' )*DLAMCH( 'Base' )
*
*     Zero out OPCNTS, TIMES
*
      DO 60 J4 = 1, NSUBS
         DO 50 J3 = 1, NSIZES
            DO 40 J2 = 1, NTYPES
               DO 30 J1 = 1, NPARMS
                  OPCNTS( J1, J2, J3, J4 ) = ZERO
                  TIMES( J1, J2, J3, J4 ) = ZERO
   30          CONTINUE
   40       CONTINUE
   50    CONTINUE
   60 CONTINUE
*
*     Do for each value of N:
*
      DO 930 IN = 1, NSIZES
*
         N = NN( IN )
         N1 = MAX( 1, N )
*
*        Do for each .TRUE. value in DOTYPE:
*
         MTYPES = MIN( MAXTYP, NTYPES )
         IF( NTYPES.EQ.MAXTYP+1 .AND. NSIZES.EQ.1 )
     $      MTYPES = NTYPES
         DO 920 ITYPE = 1, MTYPES
            IF( .NOT.DOTYPE( ITYPE ) )
     $         GO TO 920
*
*           Save random number seed for error messages
*
            DO 70 J = 1, 4
               IOLDSD( J ) = ISEED( J )
   70       CONTINUE
*
*           Time the LAPACK Routines
*
*           Generate A and B
*
            IF( ITYPE.LE.MAXTYP ) THEN
*
*              Generate A (w/o rotation)
*
               CALL DLATM4( KATYPE( ITYPE ), N, 3, 1, 2, ONE, ULP, ONE,
     $                      2, ISEED, A, N1 )
               IF( 3.LE.N )
     $            A( 3+2*N1 ) = ONE
*
*              Generate B (w/o rotation)
*
               CALL DLATM4( 8, N, 3, 1, 0, ONE, ONE, ONE, 2, ISEED, B,
     $                      N1 )
               IF( 2.LE.N )
     $            B( 2+N1 ) = ONE
*
               IF( N.GT.0 ) THEN
*
*                 Include rotations
*
*                 Generate U, V as Householder transformations times
*                 a diagonal matrix.
*
                  DO 90 JC = 1, N - 1
                     IC = ( JC-1 )*N1
                     DO 80 JR = JC, N
                        Q( JR+IC ) = DLARND( 3, ISEED )
                        Z( JR+IC ) = DLARND( 3, ISEED )
   80                CONTINUE
                     CALL DLARFG( N+1-JC, Q( JC+IC ), Q( JC+1+IC ), 1,
     $                            WORK( JC ) )
                     WORK( 2*N+JC ) = SIGN( ONE, Q( JC+IC ) )
                     Q( JC+IC ) = ONE
                     CALL DLARFG( N+1-JC, Z( JC+IC ), Z( JC+1+IC ), 1,
     $                            WORK( N+JC ) )
                     WORK( 3*N+JC ) = SIGN( ONE, Z( JC+IC ) )
                     Z( JC+IC ) = ONE
   90             CONTINUE
                  IC = ( N-1 )*N1
                  Q( N+IC ) = ONE
                  WORK( N ) = ZERO
                  WORK( 3*N ) = SIGN( ONE, DLARND( 2, ISEED ) )
                  Z( N+IC ) = ONE
                  WORK( 2*N ) = ZERO
                  WORK( 4*N ) = SIGN( ONE, DLARND( 2, ISEED ) )
*
*                 Apply the diagonal matrices
*
                  DO 110 JC = 1, N
                     DO 100 JR = 1, N
                        A( JR+IC ) = WORK( 2*N+JR )*WORK( 3*N+JC )*
     $                               A( JR+IC )
                        B( JR+IC ) = WORK( 2*N+JR )*WORK( 3*N+JC )*
     $                               B( JR+IC )
  100                CONTINUE
  110             CONTINUE
                  CALL DORM2R( 'L', 'N', N, N, N-1, Q, N1, WORK, A, N1,
     $                         WORK( 2*N+1 ), IINFO )
                  IF( IINFO.NE.0 )
     $               GO TO 120
                  CALL DORM2R( 'R', 'T', N, N, N-1, Z, N1, WORK( N+1 ),
     $                         A, N1, WORK( 2*N+1 ), IINFO )
                  IF( IINFO.NE.0 )
     $               GO TO 120
                  CALL DORM2R( 'L', 'N', N, N, N-1, Q, N1, WORK, B, N1,
     $                         WORK( 2*N+1 ), IINFO )
                  IF( IINFO.NE.0 )
     $               GO TO 120
                  CALL DORM2R( 'R', 'T', N, N, N-1, Z, N1, WORK( N+1 ),
     $                         B, N1, WORK( 2*N+1 ), IINFO )
                  IF( IINFO.NE.0 )
     $               GO TO 120
               END IF
  120          CONTINUE
            END IF
*
* . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
*
*           Time DGGHRD
*
*           Time DGEQRF+DGGHRD('N','N',...) for each pair
*           (LDAS(j),NNB(j))
*
            IF( TIMSUB( 1 ) ) THEN
               DO 160 IPAR = 1, NPARMS
                  LDA = LDAS( IPAR )
                  NB = NNB( IPAR )
                  IF( LDA.LT.N1 ) THEN
                     TIMES( IPAR, ITYPE, IN, 1 ) = ZERO
                     OPCNTS( IPAR, ITYPE, IN, 1 ) = ZERO
                     GO TO 160
                  END IF
*
*                 If this value of (NB,LDA) has occurred before,
*                 just use that value.
*
                  LASTL = 0
                  DO 130 J = 1, IPAR - 1
                     IF( LDA.EQ.LDAS( J ) .AND. NB.EQ.NNB( J ) )
     $                  LASTL = J
  130             CONTINUE
*
                  IF( LASTL.EQ.0 ) THEN
*
*                    Time DGGHRD, computing neither Q nor Z
*                    (Actually, time DGEQRF + DORMQR + DGGHRD.)
*
                     CALL XLAENV( 1, NB )
                     IC = 0
                     OPS = ZERO
                     S1 = DSECND( )
  140                CONTINUE
                     CALL DLACPY( 'Full', N, N, A, N1, H, LDA )
                     CALL DLACPY( 'Full', N, N, B, N1, T, LDA )
                     CALL DLAQZH( .FALSE., .FALSE., N, 1, N, H, LDA, T,
     $                            LDA, Q, LDA, Z, LDA, WORK, IINFO )
                     IF( IINFO.NE.0 ) THEN
                        WRITE( NOUT, FMT = 9997 )SUBNAM( 1 ), IINFO, N,
     $                     ITYPE, IPAR, IOLDSD
                        INFO = ABS( IINFO )
                        GO TO 920
                     END IF
*
                     S2 = DSECND( )
                     TIME = S2 - S1
                     IC = IC + 1
                     IF( TIME.LT.TIMMIN )
     $                  GO TO 140
*
*                    Subtract the time used in DLACPY.
*
                     S1 = DSECND( )
                     DO 150 J = 1, IC
                        CALL DLACPY( 'Full', N, N, A, N1, Z, LDA )
                        CALL DLACPY( 'Full', N, N, B, N1, Z, LDA )
  150                CONTINUE
                     S2 = DSECND( )
                     UNTIME = S2 - S1
*
                     TIMES( IPAR, ITYPE, IN, 1 ) = MAX( TIME-UNTIME,
     $                  ZERO ) / DBLE( IC )
                     OPCNTS( IPAR, ITYPE, IN, 1 ) = OPS / DBLE( IC ) +
     $                  DOPLA( 'DGEQRF', N, N, 0, 0, NB ) +
     $                  DOPLA( 'DORMQR', N, N, 0, 0, NB )
                     LDH = LDA
                  ELSE
                     OPCNTS( IPAR, ITYPE, IN, 1 ) = OPCNTS( LASTL,
     $                  ITYPE, IN, 1 )
                     TIMES( IPAR, ITYPE, IN, 1 ) = TIMES( LASTL, ITYPE,
     $                  IN, 1 )
                  END IF
  160          CONTINUE
            ELSE IF( RUNHRD ) THEN
               CALL DLACPY( 'Full', N, N, A, N1, H, N1 )
               CALL DLACPY( 'Full', N, N, B, N1, T, N1 )
               CALL DLAQZH( .FALSE., .FALSE., N, 1, N, H, N1, T, N1, Q,
     $                      N1, Z, N1, WORK, IINFO )
               IF( IINFO.NE.0 ) THEN
                  WRITE( NOUT, FMT = 9997 )SUBNAM( 1 ), IINFO, N,
     $               ITYPE, 0, IOLDSD
                  INFO = ABS( IINFO )
                  GO TO 920
               END IF
               LDH = N
            END IF
*
*           Time DGGHRD('I','N',...) for each pair (LDAS(j),NNB(j))
*
            IF( TIMSUB( 2 ) ) THEN
               DO 200 IPAR = 1, NPARMS
                  LDA = LDAS( IPAR )
                  NB = NNB( IPAR )
                  IF( LDA.LT.N1 ) THEN
                     TIMES( IPAR, ITYPE, IN, 2 ) = ZERO
                     OPCNTS( IPAR, ITYPE, IN, 2 ) = ZERO
                     GO TO 200
                  END IF
*
*                 If this value of (NB,LDA) has occurred before,
*                 just use that value.
*
                  LASTL = 0
                  DO 170 J = 1, IPAR - 1
                     IF( LDA.EQ.LDAS( J ) .AND. NB.EQ.NNB( J ) )
     $                  LASTL = J
  170             CONTINUE
*
                  IF( LASTL.EQ.0 ) THEN
*
*                    Time DGGHRD, computing Q but not Z
*                    (Actually, DGEQRF + DORMQR + DORGQR + DGGHRD.)
*
                     CALL XLAENV( 1, NB )
                     IC = 0
                     OPS = ZERO
                     S1 = DSECND( )
  180                CONTINUE
                     CALL DLACPY( 'Full', N, N, A, N1, H, LDA )
                     CALL DLACPY( 'Full', N, N, B, N1, T, LDA )
                     CALL DLAQZH( .TRUE., .FALSE., N, 1, N, H, LDA, T,
     $                            LDA, Q, LDA, Z, LDA, WORK, IINFO )
                     IF( IINFO.NE.0 ) THEN
                        WRITE( NOUT, FMT = 9997 )SUBNAM( 2 ), IINFO, N,
     $                     ITYPE, IPAR, IOLDSD
                        INFO = ABS( IINFO )
                        GO TO 920
                     END IF
*
                     S2 = DSECND( )
                     TIME = S2 - S1
                     IC = IC + 1
                     IF( TIME.LT.TIMMIN )
     $                  GO TO 180
*
*                    Subtract the time used in DLACPY.
*
                     S1 = DSECND( )
                     DO 190 J = 1, IC
                        CALL DLACPY( 'Full', N, N, A, N1, Z, LDA )
                        CALL DLACPY( 'Full', N, N, B, N1, Z, LDA )
  190                CONTINUE
                     S2 = DSECND( )
                     UNTIME = S2 - S1
*
                     TIMES( IPAR, ITYPE, IN, 2 ) = MAX( TIME-UNTIME,
     $                  ZERO ) / DBLE( IC )
                     OPCNTS( IPAR, ITYPE, IN, 2 ) = OPS / DBLE( IC ) +
     $                  DOPLA( 'DGEQRF', N, N, 0, 0, NB ) +
     $                  DOPLA( 'DORMQR', N, N, 0, 0, NB ) +
     $                  DOPLA( 'DORGQR', N, N, 0, 0, NB )
                     LDH = LDA
                  ELSE
                     OPCNTS( IPAR, ITYPE, IN, 2 ) = OPCNTS( LASTL,
     $                  ITYPE, IN, 2 )
                     TIMES( IPAR, ITYPE, IN, 2 ) = TIMES( LASTL, ITYPE,
     $                  IN, 2 )
                  END IF
  200          CONTINUE
            END IF
*
*           Time DGGHRD('N','I',...) for each pair (LDAS(j),NNB(j))
*
            IF( TIMSUB( 3 ) ) THEN
               DO 240 IPAR = 1, NPARMS
                  LDA = LDAS( IPAR )
                  NB = NNB( IPAR )
                  IF( LDA.LT.N1 ) THEN
                     TIMES( IPAR, ITYPE, IN, 3 ) = ZERO
                     OPCNTS( IPAR, ITYPE, IN, 3 ) = ZERO
                     GO TO 240
                  END IF
*
*                 If this value of (NB,LDA) has occurred before,
*                 just use that value.
*
                  LASTL = 0
                  DO 210 J = 1, IPAR - 1
                     IF( LDA.EQ.LDAS( J ) .AND. NB.EQ.NNB( J ) )
     $                  LASTL = J
  210             CONTINUE
*
                  IF( LASTL.EQ.0 ) THEN
*
*                    Time DGGHRD, computing Z but not Q
*                    (Actually, DGEQRF + DORMQR + DGGHRD.)
*
                     CALL XLAENV( 1, NB )
                     IC = 0
                     OPS = ZERO
                     S1 = DSECND( )
  220                CONTINUE
                     CALL DLACPY( 'Full', N, N, A, N1, H, LDA )
                     CALL DLACPY( 'Full', N, N, B, N1, T, LDA )
                     CALL DLAQZH( .FALSE., .TRUE., N, 1, N, H, LDA, T,
     $                            LDA, Q, LDA, Z, LDA, WORK, IINFO )
                     IF( IINFO.NE.0 ) THEN
                        WRITE( NOUT, FMT = 9997 )SUBNAM( 3 ), IINFO, N,
     $                     ITYPE, IPAR, IOLDSD
                        INFO = ABS( IINFO )
                        GO TO 920
                     END IF
*
                     S2 = DSECND( )
                     TIME = S2 - S1
                     IC = IC + 1
                     IF( TIME.LT.TIMMIN )
     $                  GO TO 220
*
*                    Subtract the time used in DLACPY.
*
                     S1 = DSECND( )
                     DO 230 J = 1, IC
                        CALL DLACPY( 'Full', N, N, A, N1, Z, LDA )
                        CALL DLACPY( 'Full', N, N, B, N1, Z, LDA )
  230                CONTINUE
                     S2 = DSECND( )
                     UNTIME = S2 - S1
*
                     TIMES( IPAR, ITYPE, IN, 3 ) = MAX( TIME-UNTIME,
     $                  ZERO ) / DBLE( IC )
                     OPCNTS( IPAR, ITYPE, IN, 3 ) = OPS / DBLE( IC ) +
     $                  DOPLA( 'DGEQRF', N, N, 0, 0, NB ) +
     $                  DOPLA( 'DORMQR', N, N, 0, 0, NB )
                     LDH = LDA
                  ELSE
                     OPCNTS( IPAR, ITYPE, IN, 3 ) = OPCNTS( LASTL,
     $                  ITYPE, IN, 3 )
                     TIMES( IPAR, ITYPE, IN, 3 ) = TIMES( LASTL, ITYPE,
     $                  IN, 3 )
                  END IF
  240          CONTINUE
            END IF
*
*           Time DGGHRD('I','I',...) for each pair (LDAS(j),NNB(j))
*
            IF( TIMSUB( 4 ) ) THEN
               DO 280 IPAR = 1, NPARMS
                  LDA = LDAS( IPAR )
                  NB = NNB( IPAR )
                  IF( LDA.LT.N1 ) THEN
                     TIMES( IPAR, ITYPE, IN, 4 ) = ZERO
                     OPCNTS( IPAR, ITYPE, IN, 4 ) = ZERO
                     GO TO 280
                  END IF
*
*                 If this value of (NB,LDA) has occurred before,
*                 just use that value.
*
                  LASTL = 0
                  DO 250 J = 1, IPAR - 1
                     IF( LDA.EQ.LDAS( J ) .AND. NB.EQ.NNB( J ) )
     $                  LASTL = J
  250             CONTINUE
*
                  IF( LASTL.EQ.0 ) THEN
*
*                    Time DGGHRD, computing Q and Z
*                    (Actually, DGEQRF + DORMQR + DORGQR + DGGHRD.)
*
                     CALL XLAENV( 1, NB )
                     IC = 0
                     OPS = ZERO
                     S1 = DSECND( )
  260                CONTINUE
                     CALL DLACPY( 'Full', N, N, A, N1, H, LDA )
                     CALL DLACPY( 'Full', N, N, B, N1, T, LDA )
                     CALL DLAQZH( .TRUE., .TRUE., N, 1, N, H, LDA, T,
     $                            LDA, Q, LDA, Z, LDA, WORK, IINFO )
                     IF( IINFO.NE.0 ) THEN
                        WRITE( NOUT, FMT = 9997 )SUBNAM( 4 ), IINFO, N,
     $                     ITYPE, IPAR, IOLDSD
                        INFO = ABS( IINFO )
                        GO TO 920
                     END IF
*
                     S2 = DSECND( )
                     TIME = S2 - S1
                     IC = IC + 1
                     IF( TIME.LT.TIMMIN )
     $                  GO TO 260
*
*                    Subtract the time used in DLACPY.
*
                     S1 = DSECND( )
                     DO 270 J = 1, IC
                        CALL DLACPY( 'Full', N, N, A, N1, Z, LDA )
                        CALL DLACPY( 'Full', N, N, B, N1, Z, LDA )
  270                CONTINUE
                     S2 = DSECND( )
                     UNTIME = S2 - S1
*
                     TIMES( IPAR, ITYPE, IN, 4 ) = MAX( TIME-UNTIME,
     $                  ZERO ) / DBLE( IC )
                     OPCNTS( IPAR, ITYPE, IN, 4 ) = OPS / DBLE( IC ) +
     $                  DOPLA( 'DGEQRF', N, N, 0, 0, NB ) +
     $                  DOPLA( 'DORMQR', N, N, 0, 0, NB ) +
     $                  DOPLA( 'DORGQR', N, N, 0, 0, NB )
                     LDH = LDA
                  ELSE
                     OPCNTS( IPAR, ITYPE, IN, 4 ) = OPCNTS( LASTL,
     $                  ITYPE, IN, 4 )
                     TIMES( IPAR, ITYPE, IN, 4 ) = TIMES( LASTL, ITYPE,
     $                  IN, 4 )
                  END IF
  280          CONTINUE
            END IF
*
* . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
*
*           Time DHGEQZ
*
*           Time DHGEQZ with JOB='E' for each value of LDAS(j)
*
            IF( TIMSUB( 5 ) ) THEN
               DO 320 IPAR = 1, NPARMS
                  LDA = LDAS( IPAR )
                  IF( LDA.LT.N1 ) THEN
                     TIMES( IPAR, ITYPE, IN, 5 ) = ZERO
                     OPCNTS( IPAR, ITYPE, IN, 5 ) = ZERO
                     GO TO 320
                  END IF
*
*                 If this value of LDA has occurred before,
*                 just use that value.
*
                  LASTL = 0
                  DO 290 J = 1, IPAR - 1
                     IF( LDA.EQ.LDAS( J ) )
     $                  LASTL = J
  290             CONTINUE
*
                  IF( LASTL.EQ.0 ) THEN
*
*                    Time DHGEQZ with JOB='E'
*
                     IC = 0
                     OPS = ZERO
                     S1 = DSECND( )
  300                CONTINUE
                     CALL DLACPY( 'Full', N, N, H, LDH, A, LDA )
                     CALL DLACPY( 'Full', N, N, T, LDH, B, LDA )
                     CALL DHGEQZ( 'E', 'N', 'N', N, 1, N, A, LDA, B,
     $                            LDA, W, W( LDA+1 ), W( 2*LDA+1 ), Q,
     $                            LDA, Z, LDA, WORK, LWORK, IINFO )
                     IF( IINFO.NE.0 ) THEN
                        WRITE( NOUT, FMT = 9997 )SUBNAM( 5 ), IINFO, N,
     $                     ITYPE, IPAR, IOLDSD
                        INFO = ABS( IINFO )
                        GO TO 920
                     END IF
                     S2 = DSECND( )
                     TIME = S2 - S1
                     IC = IC + 1
                     IF( TIME.LT.TIMMIN )
     $                  GO TO 300
*
*                    Subtract the time used in DLACPY.
*
                     S1 = DSECND( )
                     DO 310 J = 1, IC
                        CALL DLACPY( 'Full', N, N, H, LDH, Z, LDA )
                        CALL DLACPY( 'Full', N, N, T, LDH, Z, LDA )
  310                CONTINUE
                     S2 = DSECND( )
                     UNTIME = S2 - S1
*
                     TIMES( IPAR, ITYPE, IN, 5 ) = MAX( TIME-UNTIME,
     $                  ZERO ) / DBLE( IC )
                     OPCNTS( IPAR, ITYPE, IN, 5 ) = OPS / DBLE( IC )
                     LDS = 0
                     LDQ = 0
                  ELSE
                     OPCNTS( IPAR, ITYPE, IN, 5 ) = OPCNTS( LASTL,
     $                  ITYPE, IN, 5 )
                     TIMES( IPAR, ITYPE, IN, 5 ) = TIMES( LASTL, ITYPE,
     $                  IN, 5 )
                  END IF
  320          CONTINUE
            END IF
*
*           Time DHGEQZ with JOB='S', COMPQ=COMPZ='N' for each value
*           of LDAS(j)
*
            IF( TIMSUB( 6 ) ) THEN
               DO 360 IPAR = 1, NPARMS
                  LDA = LDAS( IPAR )
                  IF( LDA.LT.N1 ) THEN
                     TIMES( IPAR, ITYPE, IN, 6 ) = ZERO
                     OPCNTS( IPAR, ITYPE, IN, 6 ) = ZERO
                     GO TO 360
                  END IF
*
*                 If this value of LDA has occurred before,
*                 just use that value.
*
                  LASTL = 0
                  DO 330 J = 1, IPAR - 1
                     IF( LDA.EQ.LDAS( J ) )
     $                  LASTL = J
  330             CONTINUE
*
                  IF( LASTL.EQ.0 ) THEN
*
*                 Time DHGEQZ with JOB='S', COMPQ=COMPZ='N'
*
                     IC = 0
                     OPS = ZERO
                     S1 = DSECND( )
  340                CONTINUE
                     CALL DLACPY( 'Full', N, N, H, LDH, A, LDA )
                     CALL DLACPY( 'Full', N, N, T, LDH, B, LDA )
                     CALL DHGEQZ( 'S', 'N', 'N', N, 1, N, A, LDA, B,
     $                            LDA, W, W( LDA+1 ), W( 2*LDA+1 ), Q,
     $                            LDA, Z, LDA, WORK, LWORK, IINFO )
                     IF( IINFO.NE.0 ) THEN
                        WRITE( NOUT, FMT = 9997 )SUBNAM( 6 ), IINFO, N,
     $                     ITYPE, IPAR, IOLDSD
                        INFO = ABS( IINFO )
                        GO TO 920
                     END IF
                     S2 = DSECND( )
                     TIME = S2 - S1
                     IC = IC + 1
                     IF( TIME.LT.TIMMIN )
     $                  GO TO 340
*
*                 Subtract the time used in DLACPY.
*
                     S1 = DSECND( )
                     DO 350 J = 1, IC
                        CALL DLACPY( 'Full', N, N, H, LDH, Z, LDA )
                        CALL DLACPY( 'Full', N, N, T, LDH, Z, LDA )
  350                CONTINUE
                     S2 = DSECND( )
                     UNTIME = S2 - S1
*
                     TIMES( IPAR, ITYPE, IN, 6 ) = MAX( TIME-UNTIME,
     $                  ZERO ) / DBLE( IC )
                     OPCNTS( IPAR, ITYPE, IN, 6 ) = OPS / DBLE( IC )
                     LDS = LDA
                     LDQ = 0
                  ELSE
                     OPCNTS( IPAR, ITYPE, IN, 6 ) = OPCNTS( LASTL,
     $                  ITYPE, IN, 6 )
                     TIMES( IPAR, ITYPE, IN, 6 ) = TIMES( LASTL, ITYPE,
     $                  IN, 6 )
                  END IF
  360          CONTINUE
            ELSE IF( RUNES ) THEN
               CALL DLACPY( 'Full', N, N, H, LDH, A, N1 )
               CALL DLACPY( 'Full', N, N, T, LDH, B, N1 )
               CALL DHGEQZ( 'S', 'N', 'N', N, 1, N, A, N1, B, N1, W,
     $                      W( N1+1 ), W( 2*N1+1 ), Q, N1, Z, N1, WORK,
     $                      LWORK, IINFO )
               IF( IINFO.NE.0 ) THEN
                  WRITE( NOUT, FMT = 9997 )SUBNAM( 6 ), IINFO, N,
     $               ITYPE, 0, IOLDSD
                  INFO = ABS( IINFO )
                  GO TO 920
               END IF
               LDS = N1
               LDQ = 0
            END IF
*
*           Time DHGEQZ with JOB='S', COMPQ='I', COMPZ='N' for each
*           value of LDAS(j)
*
            IF( TIMSUB( 7 ) ) THEN
               DO 400 IPAR = 1, NPARMS
                  LDA = LDAS( IPAR )
                  IF( LDA.LT.N1 ) THEN
                     TIMES( IPAR, ITYPE, IN, 7 ) = ZERO
                     OPCNTS( IPAR, ITYPE, IN, 7 ) = ZERO
                     GO TO 400
                  END IF
*
*                 If this value of LDA has occurred before,
*                 just use that value.
*
                  LASTL = 0
                  DO 370 J = 1, IPAR - 1
                     IF( LDA.EQ.LDAS( J ) )
     $                  LASTL = J
  370             CONTINUE
*
                  IF( LASTL.EQ.0 ) THEN
*
*                 Time DHGEQZ with JOB='S', COMPQ='I', COMPZ='N'
*
                     IC = 0
                     OPS = ZERO
                     S1 = DSECND( )
  380                CONTINUE
                     CALL DLACPY( 'Full', N, N, H, LDH, A, LDA )
                     CALL DLACPY( 'Full', N, N, T, LDH, B, LDA )
                     CALL DHGEQZ( 'S', 'I', 'N', N, 1, N, A, LDA, B,
     $                            LDA, W, W( LDA+1 ), W( 2*LDA+1 ), Q,
     $                            LDA, Z, LDA, WORK, LWORK, IINFO )
                     IF( IINFO.NE.0 ) THEN
                        WRITE( NOUT, FMT = 9997 )SUBNAM( 7 ), IINFO, N,
     $                     ITYPE, IPAR, IOLDSD
                        INFO = ABS( IINFO )
                        GO TO 920
                     END IF
                     S2 = DSECND( )
                     TIME = S2 - S1
                     IC = IC + 1
                     IF( TIME.LT.TIMMIN )
     $                  GO TO 380
*
*                 Subtract the time used in DLACPY.
*
                     S1 = DSECND( )
                     DO 390 J = 1, IC
                        CALL DLACPY( 'Full', N, N, H, LDH, Z, LDA )
                        CALL DLACPY( 'Full', N, N, T, LDH, Z, LDA )
  390                CONTINUE
                     S2 = DSECND( )
                     UNTIME = S2 - S1
*
                     TIMES( IPAR, ITYPE, IN, 7 ) = MAX( TIME-UNTIME,
     $                  ZERO ) / DBLE( IC )
                     OPCNTS( IPAR, ITYPE, IN, 7 ) = OPS / DBLE( IC )
                     LDS = LDA
                     LDQ = LDA
                  ELSE
                     OPCNTS( IPAR, ITYPE, IN, 7 ) = OPCNTS( LASTL,
     $                  ITYPE, IN, 7 )
                     TIMES( IPAR, ITYPE, IN, 7 ) = TIMES( LASTL, ITYPE,
     $                  IN, 7 )
                  END IF
  400          CONTINUE
            ELSE IF( RUNEQ ) THEN
               CALL DLACPY( 'Full', N, N, H, LDH, A, N1 )
               CALL DLACPY( 'Full', N, N, T, LDH, B, N1 )
               CALL DHGEQZ( 'S', 'I', 'N', N, 1, N, A, N1, B, N1, W,
     $                      W( N1+1 ), W( 2*N1+1 ), Q, N1, Z, N1, WORK,
     $                      LWORK, IINFO )
               IF( IINFO.NE.0 ) THEN
                  WRITE( NOUT, FMT = 9997 )SUBNAM( 7 ), IINFO, N,
     $               ITYPE, 0, IOLDSD
                  INFO = ABS( IINFO )
                  GO TO 920
               END IF
               LDS = N1
               LDQ = N1
            END IF
*
*           Time DHGEQZ with JOB='S', COMPQ='N', COMPZ='I' for each
*           value of LDAS(j)
*
            IF( TIMSUB( 8 ) ) THEN
               DO 440 IPAR = 1, NPARMS
                  LDA = LDAS( IPAR )
                  IF( LDA.LT.N1 ) THEN
                     TIMES( IPAR, ITYPE, IN, 8 ) = ZERO
                     OPCNTS( IPAR, ITYPE, IN, 8 ) = ZERO
                     GO TO 440
                  END IF
*
*                 If this value of LDA has occurred before,
*                 just use that value.
*
                  LASTL = 0
                  DO 410 J = 1, IPAR - 1
                     IF( LDA.EQ.LDAS( J ) )
     $                  LASTL = J
  410             CONTINUE
*
                  IF( LASTL.EQ.0 ) THEN
*
                     NB = MIN( N, NNB( IPAR ) )
                     NSHIFT = NSHFTS( IPAR )
                     NEISP = NEISPS( IPAR )
                     MINNB = MINNBS( IPAR )
                     MINBLK = MINBKS( IPAR )
*
*                 Time DHGEQZ with JOB='S', COMPQ='N', COMPZ='I'
*                 (Note that the "Z" matrix is stored in the array Q)
*
                     IC = 0
                     OPS = ZERO
                     S1 = DSECND( )
  420                CONTINUE
                     CALL DLACPY( 'Full', N, N, H, LDH, A, LDA )
                     CALL DLACPY( 'Full', N, N, T, LDH, B, LDA )
                     CALL DHGEQZ( 'S', 'N', 'I', N, 1, N, A, LDA, B,
     $                            LDA, W, W( LDA+1 ), W( 2*LDA+1 ), Z,
     $                            LDA, Q, LDA, WORK, LWORK, IINFO )
                     IF( IINFO.NE.0 ) THEN
                        WRITE( NOUT, FMT = 9997 )SUBNAM( 8 ), IINFO, N,
     $                     ITYPE, IPAR, IOLDSD
                        INFO = ABS( IINFO )
                        GO TO 920
                     END IF
                     S2 = DSECND( )
                     TIME = S2 - S1
                     IC = IC + 1
                     IF( TIME.LT.TIMMIN )
     $                  GO TO 420
*
*                 Subtract the time used in DLACPY.
*
                     S1 = DSECND( )
                     DO 430 J = 1, IC
                        CALL DLACPY( 'Full', N, N, H, LDH, Z, LDA )
                        CALL DLACPY( 'Full', N, N, T, LDH, Z, LDA )
  430                CONTINUE
                     S2 = DSECND( )
                     UNTIME = S2 - S1
*
                     TIMES( IPAR, ITYPE, IN, 8 ) = MAX( TIME-UNTIME,
     $                  ZERO ) / DBLE( IC )
                     OPCNTS( IPAR, ITYPE, IN, 8 ) = OPS / DBLE( IC )
                     LDS = LDA
                     LDQ = LDA
                  ELSE
                     OPCNTS( IPAR, ITYPE, IN, 8 ) = OPCNTS( LASTL,
     $                  ITYPE, IN, 8 )
                     TIMES( IPAR, ITYPE, IN, 8 ) = TIMES( LASTL, ITYPE,
     $                  IN, 8 )
                  END IF
  440          CONTINUE
            END IF
*
*           Time DHGEQZ with JOB='S', COMPQ='I', COMPZ='I' for each
*           value of LDAS(j)
*
            IF( TIMSUB( 9 ) ) THEN
               DO 480 IPAR = 1, NPARMS
                  LDA = LDAS( IPAR )
                  IF( LDA.LT.N1 ) THEN
                     TIMES( IPAR, ITYPE, IN, 9 ) = ZERO
                     OPCNTS( IPAR, ITYPE, IN, 9 ) = ZERO
                     GO TO 480
                  END IF
*
*                 If this value of LDA has occurred before,
*                 just use that value.
*
                  LASTL = 0
                  DO 450 J = 1, IPAR - 1
                     IF( LDA.EQ.LDAS( J ) )
     $                  LASTL = J
  450             CONTINUE
*
                  IF( LASTL.EQ.0 ) THEN
*
*                 Time DHGEQZ with JOB='S', COMPQ='I', COMPZ='I'
*
                     IC = 0
                     OPS = ZERO
                     S1 = DSECND( )
  460                CONTINUE
                     CALL DLACPY( 'Full', N, N, H, LDH, A, LDA )
                     CALL DLACPY( 'Full', N, N, T, LDH, B, LDA )
                     CALL DHGEQZ( 'S', 'I', 'I', N, 1, N, A, LDA, B,
     $                            LDA, W, W( LDA+1 ), W( 2*LDA+1 ), Q,
     $                            LDA, Z, LDA, WORK, LWORK, IINFO )
                     IF( IINFO.NE.0 ) THEN
                        WRITE( NOUT, FMT = 9997 )SUBNAM( 9 ), IINFO, N,
     $                     ITYPE, IPAR, IOLDSD
                        INFO = ABS( IINFO )
                        GO TO 920
                     END IF
                     S2 = DSECND( )
                     TIME = S2 - S1
                     IC = IC + 1
                     IF( TIME.LT.TIMMIN )
     $                  GO TO 460
*
*                 Subtract the time used in DLACPY.
*
                     S1 = DSECND( )
                     DO 470 J = 1, IC
                        CALL DLACPY( 'Full', N, N, H, LDH, Z, LDA )
                        CALL DLACPY( 'Full', N, N, T, LDH, Z, LDA )
  470                CONTINUE
                     S2 = DSECND( )
                     UNTIME = S2 - S1
*
                     TIMES( IPAR, ITYPE, IN, 9 ) = MAX( TIME-UNTIME,
     $                  ZERO ) / DBLE( IC )
                     OPCNTS( IPAR, ITYPE, IN, 9 ) = OPS / DBLE( IC )
                     LDS = LDA
                     LDQ = LDA
                  ELSE
                     OPCNTS( IPAR, ITYPE, IN, 9 ) = OPCNTS( LASTL,
     $                  ITYPE, IN, 9 )
                     TIMES( IPAR, ITYPE, IN, 9 ) = TIMES( LASTL, ITYPE,
     $                  IN, 9 )
                  END IF
  480          CONTINUE
            END IF
*
* . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
*
*           Time DTGEVC
*
            IF( TIMSUB( 10 ) .OR. TIMSUB( 11 ) .OR. TIMSUB( 12 ) .OR.
     $          TIMSUB( 13 ) ) THEN
               DO 610 IPAR = 1, NPARMS
                  LDA = LDAS( IPAR )
                  IF( LDA.LT.N1 ) THEN
                     DO 490 J = 10, 13
                        IF( TIMSUB( J ) ) THEN
                           TIMES( IPAR, ITYPE, IN, J ) = ZERO
                           OPCNTS( IPAR, ITYPE, IN, J ) = ZERO
                        END IF
  490                CONTINUE
                     GO TO 610
                  END IF
*
*                 If this value of LDA has come up before, just use
*                 the value previously computed.
*
                  LASTL = 0
                  DO 500 J = 1, IPAR - 1
                     IF( LDA.EQ.LDAS( J ) )
     $                  LASTL = J
  500             CONTINUE
*
*                 Time DTGEVC if this is a new value of LDA
*
                  IF( LASTL.EQ.0 ) THEN
*
*                    Copy S (which is in A) and P (which is in B)
*                    if necessary to get right LDA.
*
                     IF( LDA.GT.LDS ) THEN
                        DO 520 JC = N, 1, -1
                           DO 510 JR = N, 1, -1
                              A( JR+( JC-1 )*LDA ) = A( JR+( JC-1 )*
     $                           LDS )
                              B( JR+( JC-1 )*LDA ) = B( JR+( JC-1 )*
     $                           LDS )
  510                      CONTINUE
  520                   CONTINUE
                     ELSE IF( LDA.LT.LDS ) THEN
                        DO 540 JC = 1, N
                           DO 530 JR = 1, N
                              A( JR+( JC-1 )*LDA ) = A( JR+( JC-1 )*
     $                           LDS )
                              B( JR+( JC-1 )*LDA ) = B( JR+( JC-1 )*
     $                           LDS )
  530                      CONTINUE
  540                   CONTINUE
                     END IF
                     LDS = LDA
*
*                    Time DTGEVC for Left Eigenvectors only,
*                    without back transforming
*
                     IF( TIMSUB( 10 ) ) THEN
                        IC = 0
                        OPS = ZERO
                        S1 = DSECND( )
  550                   CONTINUE
                        CALL DTGEVC( 'L', 'A', LLWORK, N, A, LDA, B,
     $                               LDA, H, LDA, T, LDA, N, ITEMP,
     $                               WORK, IINFO )
                        IF( IINFO.NE.0 ) THEN
                           WRITE( NOUT, FMT = 9997 )SUBNAM( 10 ),
     $                        IINFO, N, ITYPE, IPAR, IOLDSD
                           INFO = ABS( IINFO )
                           GO TO 920
                        END IF
                        S2 = DSECND( )
                        TIME = S2 - S1
                        IC = IC + 1
                        IF( TIME.LT.TIMMIN )
     $                     GO TO 550
*
                        TIMES( IPAR, ITYPE, IN, 10 ) = TIME / DBLE( IC )
                        OPCNTS( IPAR, ITYPE, IN, 10 ) = OPS / DBLE( IC )
                     END IF
*
*                    Time DTGEVC for Left Eigenvectors only,
*                    with back transforming
*
                     IF( TIMSUB( 11 ) ) THEN
                        IC = 0
                        OPS = ZERO
                        S1 = DSECND( )
  560                   CONTINUE
                        CALL DLACPY( 'Full', N, N, Q, LDQ, H, LDA )
                        CALL DTGEVC( 'L', 'B', LLWORK, N, A, LDA, B,
     $                               LDA, H, LDA, T, LDA, N, ITEMP,
     $                               WORK, IINFO )
                        IF( IINFO.NE.0 ) THEN
                           WRITE( NOUT, FMT = 9997 )SUBNAM( 11 ),
     $                        IINFO, N, ITYPE, IPAR, IOLDSD
                           INFO = ABS( IINFO )
                           GO TO 920
                        END IF
                        S2 = DSECND( )
                        TIME = S2 - S1
                        IC = IC + 1
                        IF( TIME.LT.TIMMIN )
     $                     GO TO 560
*
*                       Subtract the time used in DLACPY.
*
                        S1 = DSECND( )
                        DO 570 J = 1, IC
                           CALL DLACPY( 'Full', N, N, Q, LDQ, H, LDA )
  570                   CONTINUE
                        S2 = DSECND( )
                        UNTIME = S2 - S1
*
                        TIMES( IPAR, ITYPE, IN, 11 ) = MAX( TIME-UNTIME,
     $                     ZERO ) / DBLE( IC )
                        OPCNTS( IPAR, ITYPE, IN, 11 ) = OPS / DBLE( IC )
                     END IF
*
*                    Time DTGEVC for Right Eigenvectors only,
*                    without back transforming
*
                     IF( TIMSUB( 12 ) ) THEN
                        IC = 0
                        OPS = ZERO
                        S1 = DSECND( )
  580                   CONTINUE
                        CALL DTGEVC( 'R', 'A', LLWORK, N, A, LDA, B,
     $                               LDA, H, LDA, T, LDA, N, ITEMP,
     $                               WORK, IINFO )
                        IF( IINFO.NE.0 ) THEN
                           WRITE( NOUT, FMT = 9997 )SUBNAM( 12 ),
     $                        IINFO, N, ITYPE, IPAR, IOLDSD
                           INFO = ABS( IINFO )
                           GO TO 920
                        END IF
                        S2 = DSECND( )
                        TIME = S2 - S1
                        IC = IC + 1
                        IF( TIME.LT.TIMMIN )
     $                     GO TO 580
*
                        TIMES( IPAR, ITYPE, IN, 12 ) = TIME / DBLE( IC )
                        OPCNTS( IPAR, ITYPE, IN, 12 ) = OPS / DBLE( IC )
                     END IF
*
*                    Time DTGEVC for Right Eigenvectors only,
*                    with back transforming
*
                     IF( TIMSUB( 13 ) ) THEN
                        IC = 0
                        OPS = ZERO
                        S1 = DSECND( )
  590                   CONTINUE
                        CALL DLACPY( 'Full', N, N, Q, LDQ, T, LDA )
                        CALL DTGEVC( 'R', 'B', LLWORK, N, A, LDA, B,
     $                               LDA, H, LDA, T, LDA, N, ITEMP,
     $                               WORK, IINFO )
                        IF( IINFO.NE.0 ) THEN
                           WRITE( NOUT, FMT = 9997 )SUBNAM( 13 ),
     $                        IINFO, N, ITYPE, IPAR, IOLDSD
                           INFO = ABS( IINFO )
                           GO TO 920
                        END IF
                        S2 = DSECND( )
                        TIME = S2 - S1
                        IC = IC + 1
                        IF( TIME.LT.TIMMIN )
     $                     GO TO 590
*
*                       Subtract the time used in DLACPY.
*
                        S1 = DSECND( )
                        DO 600 J = 1, IC
                           CALL DLACPY( 'Full', N, N, Q, LDQ, T, LDA )
  600                   CONTINUE
                        S2 = DSECND( )
                        UNTIME = S2 - S1
*
                        TIMES( IPAR, ITYPE, IN, 13 ) = MAX( TIME-UNTIME,
     $                     ZERO ) / DBLE( IC )
                        OPCNTS( IPAR, ITYPE, IN, 13 ) = OPS / DBLE( IC )
                     END IF
*
                  ELSE
*
*                    If this LDA has previously appeared, use the
*                    previously computed value(s).
*
                     IF( TIMSUB( 10 ) ) THEN
                        OPCNTS( IPAR, ITYPE, IN, 10 ) = OPCNTS( LASTL,
     $                     ITYPE, IN, 10 )
                        TIMES( IPAR, ITYPE, IN, 10 ) = TIMES( LASTL,
     $                     ITYPE, IN, 10 )
                     END IF
                     IF( TIMSUB( 11 ) ) THEN
                        OPCNTS( IPAR, ITYPE, IN, 11 ) = OPCNTS( LASTL,
     $                     ITYPE, IN, 11 )
                        TIMES( IPAR, ITYPE, IN, 11 ) = TIMES( LASTL,
     $                     ITYPE, IN, 11 )
                     END IF
                     IF( TIMSUB( 12 ) ) THEN
                        OPCNTS( IPAR, ITYPE, IN, 12 ) = OPCNTS( LASTL,
     $                     ITYPE, IN, 12 )
                        TIMES( IPAR, ITYPE, IN, 12 ) = TIMES( LASTL,
     $                     ITYPE, IN, 12 )
                     END IF
                     IF( TIMSUB( 13 ) ) THEN
                        OPCNTS( IPAR, ITYPE, IN, 13 ) = OPCNTS( LASTL,
     $                     ITYPE, IN, 13 )
                        TIMES( IPAR, ITYPE, IN, 13 ) = TIMES( LASTL,
     $                     ITYPE, IN, 13 )
                     END IF
                  END IF
  610          CONTINUE
            END IF
*
*           Time the EISPACK Routines
*
*           Restore random number seed
*
            DO 620 J = 1, 4
               ISEED( J ) = IOLDSD( J )
  620       CONTINUE
*
*           Re-generate A
*
            IF( ITYPE.LE.MAXTYP ) THEN
*
*              Generate A (w/o rotation)
*
               CALL DLATM4( KATYPE( ITYPE ), N, 3, 1, 2, ONE, ULP, ONE,
     $                      2, ISEED, A, N1 )
               IF( 3.LE.N )
     $            A( 3+2*N1 ) = ONE
*
*              Generate B (w/o rotation)
*
               CALL DLATM4( 8, N, 3, 1, 0, ONE, ONE, ONE, 2, ISEED, B,
     $                      N1 )
               IF( 2.LE.N )
     $            B( 2+N1 ) = ONE
*
               IF( N.GT.0 ) THEN
*
*                 Include rotations
*
*                 Generate U, V as Householder transformations times
*                 a diagonal matrix.
*
                  DO 640 JC = 1, N - 1
                     IC = ( JC-1 )*N1
                     DO 630 JR = JC, N
                        Q( JR+IC ) = DLARND( 3, ISEED )
                        Z( JR+IC ) = DLARND( 3, ISEED )
  630                CONTINUE
                     CALL DLARFG( N+1-JC, Q( JC+IC ), Q( JC+1+IC ), 1,
     $                            WORK( JC ) )
                     WORK( 2*N+JC ) = SIGN( ONE, Q( JC+IC ) )
                     Q( JC+IC ) = ONE
                     CALL DLARFG( N+1-JC, Z( JC+IC ), Z( JC+1+IC ), 1,
     $                            WORK( N+JC ) )
                     WORK( 3*N+JC ) = SIGN( ONE, Z( JC+IC ) )
                     Z( JC+IC ) = ONE
  640             CONTINUE
                  IC = ( N-1 )*N1
                  Q( N+IC ) = ONE
                  WORK( N ) = ZERO
                  WORK( 3*N ) = SIGN( ONE, DLARND( 2, ISEED ) )
                  Z( N+IC ) = ONE
                  WORK( 2*N ) = ZERO
                  WORK( 4*N ) = SIGN( ONE, DLARND( 2, ISEED ) )
*
*                 Apply the diagonal matrices
*
                  DO 660 JC = 1, N
                     DO 650 JR = 1, N
                        A( JR+IC ) = WORK( 2*N+JR )*WORK( 3*N+JC )*
     $                               A( JR+IC )
                        B( JR+IC ) = WORK( 2*N+JR )*WORK( 3*N+JC )*
     $                               B( JR+IC )
  650                CONTINUE
  660             CONTINUE
                  CALL DORM2R( 'L', 'N', N, N, N-1, Q, N1, WORK, A, N1,
     $                         WORK( 2*N+1 ), IINFO )
                  IF( IINFO.NE.0 )
     $               GO TO 670
                  CALL DORM2R( 'R', 'T', N, N, N-1, Z, N1, WORK( N+1 ),
     $                         A, N1, WORK( 2*N+1 ), IINFO )
                  IF( IINFO.NE.0 )
     $               GO TO 670
                  CALL DORM2R( 'L', 'N', N, N, N-1, Q, N1, WORK, B, N1,
     $                         WORK( 2*N+1 ), IINFO )
                  IF( IINFO.NE.0 )
     $               GO TO 670
                  CALL DORM2R( 'R', 'T', N, N, N-1, Z, N1, WORK( N+1 ),
     $                         B, N1, WORK( 2*N+1 ), IINFO )
                  IF( IINFO.NE.0 )
     $               GO TO 670
               END IF
  670          CONTINUE
            END IF
*
*           Time QZHES w/ MATZ=.FALSE. for each LDAS(j)
*
            IF( TIMSUB( 14 ) ) THEN
               DO 710 IPAR = 1, NPARMS
                  LDA = LDAS( IPAR )
                  IF( LDA.LT.N1 ) THEN
                     TIMES( IPAR, ITYPE, IN, 14 ) = ZERO
                     OPCNTS( IPAR, ITYPE, IN, 14 ) = ZERO
                     GO TO 710
                  END IF
*
*                 If this value of LDA has come up before, just use
*                 the value previously computed.
*
                  LASTL = 0
                  DO 680 J = 1, IPAR - 1
                     IF( LDA.EQ.LDAS( J ) )
     $                  LASTL = J
  680             CONTINUE
*
                  IF( LASTL.EQ.0 ) THEN
*
*                    Time QZHES( ...,.FALSE.,..)
*
                     IC = 0
                     OPS = ZERO
                     S1 = DSECND( )
  690                CONTINUE
                     CALL DLACPY( 'Full', N, N, A, N1, H, LDA )
                     CALL DLACPY( 'Full', N, N, B, N1, T, LDA )
                     CALL QZHES( LDA, N, H, T, .FALSE., Q )
                     S2 = DSECND( )
                     TIME = S2 - S1
                     IC = IC + 1
                     IF( TIME.LT.TIMMIN )
     $                  GO TO 690
*
*                    Subtract the time used in DLACPY.
*
                     S1 = DSECND( )
                     DO 700 J = 1, IC
                        CALL DLACPY( 'Full', N, N, A, N1, Z, LDA )
                        CALL DLACPY( 'Full', N, N, B, N1, Z, LDA )
  700                CONTINUE
                     S2 = DSECND( )
                     UNTIME = S2 - S1
*
                     TIMES( IPAR, ITYPE, IN, 14 ) = MAX( TIME-UNTIME,
     $                  ZERO ) / DBLE( IC )
                     OPCNTS( IPAR, ITYPE, IN, 14 ) = OPS / DBLE( IC )
                  ELSE
                     OPCNTS( IPAR, ITYPE, IN, 14 ) = OPCNTS( LASTL,
     $                  ITYPE, IN, 14 )
                     TIMES( IPAR, ITYPE, IN, 14 ) = TIMES( LASTL, ITYPE,
     $                  IN, 14 )
                  END IF
                  LDH = LDA
  710          CONTINUE
            ELSE IF( RUNHES ) THEN
               CALL DLACPY( 'Full', N, N, A, N1, H, N1 )
               CALL DLACPY( 'Full', N, N, B, N1, T, N1 )
               CALL QZHES( N1, N, H, T, .FALSE., Q )
               LDH = N1
            END IF
*
*           Time QZHES w/ MATZ=.TRUE. for each LDAS(j)
*
            IF( TIMSUB( 15 ) ) THEN
               DO 750 IPAR = 1, NPARMS
                  LDA = LDAS( IPAR )
                  IF( LDA.LT.N1 ) THEN
                     TIMES( IPAR, ITYPE, IN, 15 ) = ZERO
                     OPCNTS( IPAR, ITYPE, IN, 15 ) = ZERO
                     GO TO 750
                  END IF
*
*                 If this value of LDA has come up before, just use
*                 the value previously computed.
*
                  LASTL = 0
                  DO 720 J = 1, IPAR - 1
                     IF( LDA.EQ.LDAS( J ) )
     $                  LASTL = J
  720             CONTINUE
*
                  IF( LASTL.EQ.0 ) THEN
*
*                    Time QZHES( ...,.TRUE.,..)
*
                     IC = 0
                     OPS = ZERO
                     S1 = DSECND( )
  730                CONTINUE
                     CALL DLACPY( 'Full', N, N, A, N1, H, LDA )
                     CALL DLACPY( 'Full', N, N, B, N1, T, LDA )
                     CALL QZHES( LDA, N, H, T, .TRUE., Q )
                     S2 = DSECND( )
                     TIME = S2 - S1
                     IC = IC + 1
                     IF( TIME.LT.TIMMIN )
     $                  GO TO 730
*
*                    Subtract the time used in DLACPY.
*
                     S1 = DSECND( )
                     DO 740 J = 1, IC
                        CALL DLACPY( 'Full', N, N, A, N1, Z, LDA )
                        CALL DLACPY( 'Full', N, N, B, N1, Z, LDA )
  740                CONTINUE
                     S2 = DSECND( )
                     UNTIME = S2 - S1
*
                     TIMES( IPAR, ITYPE, IN, 15 ) = MAX( TIME-UNTIME,
     $                  ZERO ) / DBLE( IC )
                     OPCNTS( IPAR, ITYPE, IN, 15 ) = OPS / DBLE( IC )
                  ELSE
                     OPCNTS( IPAR, ITYPE, IN, 15 ) = OPCNTS( LASTL,
     $                  ITYPE, IN, 15 )
                     TIMES( IPAR, ITYPE, IN, 15 ) = TIMES( LASTL, ITYPE,
     $                  IN, 15 )
                  END IF
                  LDH = LDA
  750          CONTINUE
            END IF
*
*           Time QZIT and QZVAL w/ MATZ=.FALSE. for each LDAS(j)
*
            IF( TIMSUB( 16 ) ) THEN
               DO 790 IPAR = 1, NPARMS
                  LDA = LDAS( IPAR )
                  IF( LDA.LT.N1 ) THEN
                     TIMES( IPAR, ITYPE, IN, 16 ) = ZERO
                     OPCNTS( IPAR, ITYPE, IN, 16 ) = ZERO
                     GO TO 790
                  END IF
*
*                 If this value of LDA has come up before, just use
*                 the value previously computed.
*
                  LASTL = 0
                  DO 760 J = 1, IPAR - 1
                     IF( LDA.EQ.LDAS( J ) )
     $                  LASTL = J
  760             CONTINUE
*
                  IF( LASTL.EQ.0 ) THEN
*
*                    Time QZIT and QZVAL with MATZ=.FALSE.
*
                     IC = 0
                     OPS = ZERO
                     S1 = DSECND( )
  770                CONTINUE
                     CALL DLACPY( 'Full', N, N, H, LDH, A, LDA )
                     CALL DLACPY( 'Full', N, N, T, LDH, B, LDA )
                     CALL QZIT( LDA, N, A, B, ZERO, .FALSE., Q, IINFO )
                     IF( IINFO.NE.0 ) THEN
                        WRITE( NOUT, FMT = 9997 )SUBNAM( 16 ), IINFO,
     $                     N, ITYPE, IPAR, IOLDSD
                        INFO = ABS( IINFO )
                        GO TO 920
                     END IF
*
                     CALL QZVAL( LDA, N, A, B, W, W( LDA+1 ),
     $                           W( 2*LDA+1 ), .FALSE., Q )
                     S2 = DSECND( )
                     TIME = S2 - S1
                     IC = IC + 1
                     IF( TIME.LT.TIMMIN )
     $                  GO TO 770
*
*                    Subtract the time used in DLACPY.
*
                     S1 = DSECND( )
                     DO 780 J = 1, IC
                        CALL DLACPY( 'Full', N, N, H, LDH, Z, LDA )
                        CALL DLACPY( 'Full', N, N, T, LDH, Z, LDA )
  780                CONTINUE
                     S2 = DSECND( )
                     UNTIME = S2 - S1
*
                     TIMES( IPAR, ITYPE, IN, 16 ) = MAX( TIME-UNTIME,
     $                  ZERO ) / DBLE( IC )
                     OPCNTS( IPAR, ITYPE, IN, 16 ) = OPS / DBLE( IC )
                  ELSE
                     OPCNTS( IPAR, ITYPE, IN, 16 ) = OPCNTS( LASTL,
     $                  ITYPE, IN, 16 )
                     TIMES( IPAR, ITYPE, IN, 16 ) = TIMES( LASTL, ITYPE,
     $                  IN, 16 )
                  END IF
                  LDS = 0
  790          CONTINUE
            END IF
*
*           Time QZIT and QZVAL w/ MATZ=.TRUE. for each LDAS(j)
*
            IF( TIMSUB( 17 ) ) THEN
               DO 830 IPAR = 1, NPARMS
                  LDA = LDAS( IPAR )
                  IF( LDA.LT.N1 ) THEN
                     TIMES( IPAR, ITYPE, IN, 17 ) = ZERO
                     OPCNTS( IPAR, ITYPE, IN, 17 ) = ZERO
                     GO TO 830
                  END IF
*
*                 If this value of LDA has come up before, just use
*                 the value previously computed.
*
                  LASTL = 0
                  DO 800 J = 1, IPAR - 1
                     IF( LDA.EQ.LDAS( J ) )
     $                  LASTL = J
  800             CONTINUE
*
                  IF( LASTL.EQ.0 ) THEN
*
*                    Time QZIT and QZVAL with MATZ=.TRUE.
*
                     IC = 0
                     OPS = ZERO
                     S1 = DSECND( )
  810                CONTINUE
                     CALL DLACPY( 'Full', N, N, H, LDH, A, LDA )
                     CALL DLACPY( 'Full', N, N, T, LDH, B, LDA )
                     CALL DLASET( 'Full', N, N, ZERO, ONE, Q, LDA )
                     CALL QZIT( LDA, N, A, B, ZERO, .TRUE., Q, IINFO )
                     IF( IINFO.NE.0 ) THEN
                        WRITE( NOUT, FMT = 9997 )SUBNAM( 17 ), IINFO,
     $                     N, ITYPE, IPAR, IOLDSD
                        INFO = ABS( IINFO )
                        GO TO 920
                     END IF
*
                     CALL QZVAL( LDA, N, A, B, W, W( LDA+1 ),
     $                           W( 2*LDA+1 ), .TRUE., Q )
                     S2 = DSECND( )
                     TIME = S2 - S1
                     IC = IC + 1
                     IF( TIME.LT.TIMMIN )
     $                  GO TO 810
*
*                    Subtract the time used in DLACPY.
*
                     S1 = DSECND( )
                     DO 820 J = 1, IC
                        CALL DLACPY( 'Full', N, N, H, LDH, Z, LDA )
                        CALL DLACPY( 'Full', N, N, T, LDH, Z, LDA )
                        CALL DLASET( 'Full', N, N, ZERO, ONE, Z, LDA )
  820                CONTINUE
                     S2 = DSECND( )
                     UNTIME = S2 - S1
*
                     TIMES( IPAR, ITYPE, IN, 17 ) = MAX( TIME-UNTIME,
     $                  ZERO ) / DBLE( IC )
                     OPCNTS( IPAR, ITYPE, IN, 17 ) = OPS / DBLE( IC )
                  ELSE
                     OPCNTS( IPAR, ITYPE, IN, 17 ) = OPCNTS( LASTL,
     $                  ITYPE, IN, 17 )
                     TIMES( IPAR, ITYPE, IN, 17 ) = TIMES( LASTL, ITYPE,
     $                  IN, 17 )
                  END IF
                  LDS = LDA
                  LDW = LDA
  830          CONTINUE
            ELSE IF( RUNQZ ) THEN
               CALL DLACPY( 'Full', N, N, H, LDH, A, N1 )
               CALL DLACPY( 'Full', N, N, T, LDH, B, N1 )
               CALL DLASET( 'Full', N, N, ZERO, ONE, Q, N1 )
               CALL QZIT( N1, N, A, B, ZERO, .TRUE., Q, IINFO )
               IF( IINFO.NE.0 ) THEN
                  WRITE( NOUT, FMT = 9997 )SUBNAM( 17 ), IINFO, N,
     $               ITYPE, IPAR, IOLDSD
                  INFO = ABS( IINFO )
                  GO TO 920
               END IF
*
               CALL QZVAL( N1, N, A, B, W, W( N1+1 ), W( 2*N1+1 ),
     $                     .TRUE., Q )
               LDS = N1
               LDW = N1
            END IF
*
*           Time QZVEC for each LDAS(j)
*
            IF( TIMSUB( 18 ) ) THEN
               DO 910 IPAR = 1, NPARMS
                  LDA = LDAS( IPAR )
                  IF( LDA.LT.N1 ) THEN
                     TIMES( IPAR, ITYPE, IN, 18 ) = ZERO
                     OPCNTS( IPAR, ITYPE, IN, 18 ) = ZERO
                     GO TO 910
                  END IF
*
*                 If this value of LDA has come up before, just use
*                 the value previously computed.
*
                  LASTL = 0
                  DO 840 J = 1, IPAR - 1
                     IF( LDA.EQ.LDAS( J ) )
     $                  LASTL = J
  840             CONTINUE
*
                  IF( LASTL.EQ.0 ) THEN
*
*                    Copy W if necessary to get right LDA.
*
                     IF( LDA.GT.LDW ) THEN
                        DO 860 JC = 3, 1, -1
                           DO 850 JR = N, 1, -1
                              W( JR+( JC-1 )*LDA ) = W( JR+( JC-1 )*
     $                           LDW )
  850                      CONTINUE
  860                   CONTINUE
                     ELSE IF( LDA.LT.LDW ) THEN
                        DO 880 JC = 1, 3
                           DO 870 JR = 1, N
                              W( JR+( JC-1 )*LDA ) = W( JR+( JC-1 )*
     $                           LDW )
  870                      CONTINUE
  880                   CONTINUE
                     END IF
                     LDW = LDA
*
*                    Time QZVEC
*
                     IC = 0
                     OPS = ZERO
                     S1 = DSECND( )
  890                CONTINUE
                     CALL DLACPY( 'Full', N, N, A, LDS, H, LDA )
                     CALL DLACPY( 'Full', N, N, B, LDS, T, LDA )
                     CALL DLACPY( 'Full', N, N, Q, LDS, Z, LDA )
                     CALL QZVEC( LDA, N, H, T, W, W( LDA+1 ),
     $                           W( 2*LDA+1 ), Z )
                     S2 = DSECND( )
                     TIME = S2 - S1
                     IC = IC + 1
                     IF( TIME.LT.TIMMIN )
     $                  GO TO 890
*
*                    Subtract the time used in DLACPY.
*
                     S1 = DSECND( )
                     DO 900 J = 1, IC
                        CALL DLACPY( 'Full', N, N, A, LDS, Z, LDA )
                        CALL DLACPY( 'Full', N, N, B, LDS, Z, LDA )
                        CALL DLACPY( 'Full', N, N, Q, LDS, Z, LDA )
  900                CONTINUE
                     S2 = DSECND( )
                     UNTIME = S2 - S1
*
                     TIMES( IPAR, ITYPE, IN, 18 ) = MAX( TIME-UNTIME,
     $                  ZERO ) / DBLE( IC )
                     OPCNTS( IPAR, ITYPE, IN, 18 ) = OPS / DBLE( IC )
                  ELSE
                     OPCNTS( IPAR, ITYPE, IN, 18 ) = OPCNTS( LASTL,
     $                  ITYPE, IN, 18 )
                     TIMES( IPAR, ITYPE, IN, 18 ) = TIMES( LASTL, ITYPE,
     $                  IN, 18 )
                  END IF
  910          CONTINUE
            END IF
*
  920    CONTINUE
  930 CONTINUE
*
*     Print a table of results for each timed routine.
*
      DO 940 ISUB = 1, NSUBS
         IF( TIMSUB( ISUB ) ) THEN
            CALL DPRTBG( SUBNAM( ISUB ), MTYPES, DOTYPE, NSIZES, NN,
     $                   INPARM( ISUB ), PNAMES, NPARMS, LDAS, NNB,
     $                   NSHFTS, NEISPS, MINNBS, MINBKS,
     $                   OPCNTS( 1, 1, 1, ISUB ), LDO1, LDO2,
     $                   TIMES( 1, 1, 1, ISUB ), LDT1, LDT2, WORK,
     $                   LLWORK, NOUT )
         END IF
  940 CONTINUE
*
      RETURN
*
*     End of DTIM51
*
 9997 FORMAT( ' DTIM51: ', A, ' returned INFO=', I6, '.', / 9X, 'N=',
     $      I6, ', ITYPE=', I6, ', IPAR=', I6, ', ISEED=(',
     $      3( I5, ',' ), I5, ')' )
*
      END
      PROGRAM DTIMEE
*
*  -- LAPACK timing routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     June 30, 1999
*
*  Purpose
*  =======
*
*  DTIMEE is the main timing program for the DOUBLE PRECISION matrix
*  eigenvalue routines in LAPACK.
*
*  There are four sets of routines that can be timed:
*
*  NEP (Nonsymmetric Eigenvalue Problem):
*      Includes DGEHRD, DHSEQR, DTREVC, and DHSEIN
*
*  SEP (Symmetric Eigenvalue Problem):
*      Includes DSYTRD, DORGTR, DORMTR, DSTEQR, DSTERF, DPTEQR, DSTEBZ,
*      DSTEIN, and DSTEDC
*
*  SVD (Singular Value Decomposition):
*      Includes DGEBRD, DBDSQR, DORGBR, DBDSDC and DGESDD
*
*  GEP (Generalized nonsymmetric Eigenvalue Problem):
*      Includes DGGHRD, DHGEQZ, and DTGEVC
*
*  Each test path has a different input file.  The first line of the
*  input file should contain the characters NEP, SEP, SVD, or GEP in
*  columns 1-3.  The number of remaining lines depends on what is found
*  on the first line.
*
*-----------------------------------------------------------------------
*
*  NEP input file:
*
*  line 2:  NN, INTEGER
*           Number of values of N.
*
*  line 3:  NVAL, INTEGER array, dimension (NN)
*           The values for the matrix dimension N.
*
*  line 4:  NPARM, INTEGER
*           Number of values of the parameters NB, NS, MAXB, and LDA.
*
*  line 5:  NBVAL, INTEGER array, dimension (NPARM)
*           The values for the blocksize NB.
*
*  line 6:  NSVAL, INTEGER array, dimension (NPARM)
*           The values for the number of shifts.
*
*  line 7:  MXBVAL, INTEGER array, dimension (NPARM)
*           The values for MAXB, used in determining whether multishift
*           will be used.
*
*  line 8:  LDAVAL, INTEGER array, dimension (NPARM)
*           The values for the leading dimension LDA.
*
*  line 9:  TIMMIN, DOUBLE PRECISION
*           The minimum time (in seconds) that a subroutine will be
*           timed.  If TIMMIN is zero, each routine should be timed only
*           once.
*
*  line 10: NTYPES, INTEGER
*           The number of matrix types to be used in the timing run.
*           If NTYPES >= MAXTYP, all the types are used.
*
*  If 0 < NTYPES < MAXTYP, then line 11 specifies NTYPES integer
*  values, which are the numbers of the matrix types to be used.
*
*  The remaining lines specify a path name and the specific routines to
*  be timed.  For the nonsymmetric eigenvalue problem, the path name is
*  'DHS'.  A line to request all the routines in this path has the form
*     DHS   T T T T T T T T T T T T
*  where the first 3 characters specify the path name, and up to MAXTYP
*  nonblank characters may appear in columns 4-80.  If the k-th such
*  character is 'T' or 't', the k-th routine will be timed.  If at least
*  one but fewer than 12 nonblank characters are specified, the
*  remaining routines will not be timed.  If columns 4-80 are blank, all
*  the routines will be timed, so the input line
*     DHS
*  is equivalent to the line above.
*
*-----------------------------------------------------------------------
*
*  SEP input file:
*
*  line 2:  NN, INTEGER
*           Number of values of N.
*
*  line 3:  NVAL, INTEGER array, dimension (NN)
*           The values for the matrix dimension N.
*
*  line 4:  NPARM, INTEGER
*           Number of values of the parameters NB and LDA.
*
*  line 5:  NBVAL, INTEGER array, dimension (NPARM)
*           The values for the blocksize NB.
*
*  line 6:  LDAVAL, INTEGER array, dimension (NPARM)
*           The values for the leading dimension LDA.
*
*  line 7:  TIMMIN, DOUBLE PRECISION
*           The minimum time (in seconds) that a subroutine will be
*           timed.  If TIMMIN is zero, each routine should be timed only
*           once.
*
*  line 8:  NTYPES, INTEGER
*           The number of matrix types to be used in the timing run.
*           If NTYPES >= MAXTYP, all the types are used.
*
*  If 0 < NTYPES < MAXTYP, then line 9 specifies NTYPES integer
*  values, which are the numbers of the matrix types to be used.
*
*  The remaining lines specify a path name and the specific routines to
*  be timed as for the NEP input file.  For the symmetric eigenvalue
*  problem, the path name is 'DST' and up to 8 routines may be timed.
*
*-----------------------------------------------------------------------
*
*  SVD input file:
*
*  line 2:  NN, INTEGER
*           Number of values of M and N.
*
*  line 3:  MVAL, INTEGER array, dimension (NN)
*           The values for the matrix dimension M.
*
*  line 4:  NVAL, INTEGER array, dimension (NN)
*           The values for the matrix dimension N.
*
*  line 5:  NPARM, INTEGER
*           Number of values of the parameters NB and LDA.
*
*  line 6:  NBVAL, INTEGER array, dimension (NPARM)
*           The values for the blocksize NB.
*
*  line 7:  LDAVAL, INTEGER array, dimension (NPARM)
*           The values for the leading dimension LDA.
*
*  line 8:  TIMMIN, DOUBLE PRECISION
*           The minimum time (in seconds) that a subroutine will be
*           timed.  If TIMMIN is zero, each routine should be timed only
*           once.
*
*  line 9:  NTYPES, INTEGER
*           The number of matrix types to be used in the timing run.
*           If NTYPES >= MAXTYP, all the types are used.
*
*  If 0 < NTYPES < MAXTYP, then line 10 specifies NTYPES integer
*  values, which are the numbers of the matrix types to be used.
*
*  The remaining lines specify a path name and the specific routines to
*  be timed as for the NEP input file.  For the singular value
*  decomposition the path name is 'DBD' and up to 16 routines may be
*  timed.
*
*-----------------------------------------------------------------------
*
*  GEP input file:
*
*  line 2:  NN, INTEGER
*           Number of values of N.
*
*  line 3:  NVAL, INTEGER array, dimension (NN)
*           The values for the matrix dimension N.
*
*  line 4:  NPARM, INTEGER
*           Number of values of the parameters NB, NS, MAXB, and LDA.
*
*  line 5:  NBVAL, INTEGER array, dimension (NPARM)
*           The values for the blocksize NB.
*
*  line 6:  NSVAL, INTEGER array, dimension (NPARM)
*           The values for the number of shifts.
*
*  line 7:  NEIVAL, INTEGER array, dimension (NPARM)
*           The values for NEISP, used in determining whether multishift
*           will be used.
*
*  line 8:  NBMVAL, INTEGER array, dimension (NPARM)
*           The values for MINNB, used in determining minimum blocksize.
*
*  line 9:  NBKVAL, INTEGER array, dimension (NPARM)
*           The values for MINBLK, also used in determining minimum
*           blocksize.
*
*  line 10: LDAVAL, INTEGER array, dimension (NPARM)
*           The values for the leading dimension LDA.
*
*  line 11: TIMMIN, DOUBLE PRECISION
*           The minimum time (in seconds) that a subroutine will be
*           timed.  If TIMMIN is zero, each routine should be timed only
*           once.
*
*  line 12: NTYPES, INTEGER
*           The number of matrix types to be used in the timing run.
*           If NTYPES >= MAXTYP, all the types are used.
*
*  If 0 < NTYPES < MAXTYP, then line 13 specifies NTYPES integer
*  values, which are the numbers of the matrix types to be used.
*
*  The remaining lines specify a path name and the specific routines to
*  be timed.  For the nonsymmetric eigenvalue problem, the path name is
*  'DHG'.  A line to request all the routines in this path has the form
*     DHG   T T T T T T T T T T T T T T T T T T
*  where the first 3 characters specify the path name, and up to MAXTYP
*  nonblank characters may appear in columns 4-80.  If the k-th such
*  character is 'T' or 't', the k-th routine will be timed.  If at least
*  one but fewer than 18 nonblank characters are specified, the
*  remaining routines will not be timed.  If columns 4-80 are blank, all
*  the routines will be timed, so the input line
*     DHG
*  is equivalent to the line above.
*
*=======================================================================
*
*  The workspace requirements in terms of square matrices for the
*  different test paths are as follows:
*
*  NEP:   3 N**2 + N*(3*NB+2)
*  SEP:   2 N**2 + N*(2*N) + N
*  SVD:   4 N**2 + MAX( 6*N, MAXIN*MAXPRM*MAXT )
*  GEP:   6 N**2 + 3*N
*
*  MAXN is currently set to 400,
*  LG2MXN = ceiling of log-base-2 of MAXN = 9, and LDAMAX = 420.
*  The real work space needed is LWORK = MAX( MAXN*(4*MAXN+2),
*       2*LDAMAX+1+3*MAXN+2*MAXN*LG2MXN+3*MAXN**2 ),  and the integer
*  workspace needed is  LIWRK2 = 6 + 6*MAXN + 5*MAXN*LG2MXN.
*  For SVD, we assume NRHS may be as big
*  as N.  The parameter NEED is set to 4 to allow for 4 NxN matrices
*  for SVD.
*
*     .. Parameters ..
      INTEGER            MAXN, LDAMAX, LG2MXN
      PARAMETER          ( MAXN = 400, LDAMAX = 420, LG2MXN = 9 )
      INTEGER            NEED
      PARAMETER          ( NEED = 6 )
      INTEGER            LIWRK2
      PARAMETER          ( LIWRK2 = 6+6*MAXN+5*MAXN*LG2MXN )
      INTEGER            LWORK
      PARAMETER          ( LWORK = 2*LDAMAX+1+3*MAXN+2*MAXN*LG2MXN+
     $                   4*MAXN**2 )
      INTEGER            MAXIN, MAXPRM, MAXT, MAXSUB
      PARAMETER          ( MAXIN = 12, MAXPRM = 10, MAXT = 10,
     $                   MAXSUB = 25 )
      INTEGER            NIN, NOUT
      PARAMETER          ( NIN = 5, NOUT = 6 )
*     ..
*     .. Local Scalars ..
      LOGICAL            FATAL, GEP, NEP, SEP, SVD
      CHARACTER*3        C3, PATH
      CHARACTER*6        VNAME
      CHARACTER*80       LINE
      INTEGER            I, INFO, MAXTYP, NN, NPARMS, NTYPES
      DOUBLE PRECISION   S1, S2, TIMMIN
*     ..
*     .. Local Arrays ..
      LOGICAL            DOTYPE( MAXT ), LOGWRK( MAXN )
      INTEGER            ISEED( 4 ), IWORK( MAXT ), IWORK2( LIWRK2 ),
     $                   LDAVAL( MAXPRM ), MVAL( MAXIN ),
     $                   MXBVAL( MAXPRM ), MXTYPE( 4 ),
     $                   NBKVAL( MAXPRM ), NBMVAL( MAXPRM ),
     $                   NBVAL( MAXPRM ), NSVAL( MAXPRM ), NVAL( MAXIN )
      DOUBLE PRECISION   A( LDAMAX*MAXN, NEED ), D( MAXN, 4 ),
     $                   OPCNTS( MAXPRM, MAXT, MAXIN, MAXSUB ),
     $                   RESULT( MAXPRM, MAXT, MAXIN, MAXSUB ),
     $                   WORK( LWORK )
*     ..
*     .. External Functions ..
      LOGICAL            LSAMEN
      DOUBLE PRECISION   DSECND
      EXTERNAL           LSAMEN, DSECND
*     ..
*     .. External Subroutines ..
      EXTERNAL           DTIM21, DTIM22, DTIM26, DTIM51
*     ..
*     .. Scalars in Common ..
      DOUBLE PRECISION   ITCNT, OPS
*     ..
*     .. Arrays in Common ..
      INTEGER            IPARMS( 100 )
*     ..
*     .. Common blocks ..
      COMMON             / LATIME / OPS, ITCNT
      COMMON             / CLAENV / IPARMS
*     ..
*     .. Save statement ..
      SAVE               / CLAENV /
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX
*     ..
*     .. Data statements ..
      DATA               ISEED / 0, 0, 0, 1 /
      DATA               MXTYPE / 8, 4, 5, 4 /
*     ..
*     .. Executable Statements ..
*
      S1 = DSECND( )
      FATAL = .FALSE.
      NEP = .FALSE.
      SEP = .FALSE.
      SVD = .FALSE.
      GEP = .FALSE.
*
*     Read the 3-character test path
*
      READ( NIN, FMT = '(A3)', END = 160 )PATH
      NEP = LSAMEN( 3, PATH, 'NEP' ) .OR. LSAMEN( 3, PATH, 'DHS' )
      SEP = LSAMEN( 3, PATH, 'SEP' ) .OR. LSAMEN( 3, PATH, 'DST' )
      SVD = LSAMEN( 3, PATH, 'SVD' ) .OR. LSAMEN( 3, PATH, 'DBD' )
      GEP = LSAMEN( 3, PATH, 'GEP' ) .OR. LSAMEN( 3, PATH, 'DHG' )
*
*     Report values of parameters as they are read.
*
      IF( NEP ) THEN
         WRITE( NOUT, FMT = 9993 )
      ELSE IF( SEP ) THEN
         WRITE( NOUT, FMT = 9992 )
      ELSE IF( SVD ) THEN
         WRITE( NOUT, FMT = 9991 )
      ELSE IF( GEP ) THEN
         WRITE( NOUT, FMT = 9990 )
      ELSE
         WRITE( NOUT, FMT = 9996 )PATH
         STOP
      END IF
      WRITE( NOUT, FMT = 9985 )
      WRITE( NOUT, FMT = 9989 )
*
*     Read the number of values of M and N.
*
      READ( NIN, FMT = * )NN
      IF( NN.LT.1 ) THEN
         WRITE( NOUT, FMT = 9995 )'NN  ', NN, 1
         NN = 0
         FATAL = .TRUE.
      ELSE IF( NN.GT.MAXIN ) THEN
         WRITE( NOUT, FMT = 9994 )'NN  ', NN, MAXIN
         NN = 0
         FATAL = .TRUE.
      END IF
*
*     Read the values of M
*
      READ( NIN, FMT = * )( MVAL( I ), I = 1, NN )
      IF( SVD ) THEN
         VNAME = '  M'
      ELSE
         VNAME = '  N'
      END IF
      DO 10 I = 1, NN
         IF( MVAL( I ).LT.0 ) THEN
            WRITE( NOUT, FMT = 9995 )VNAME, MVAL( I ), 0
            FATAL = .TRUE.
         ELSE IF( MVAL( I ).GT.MAXN ) THEN
            WRITE( NOUT, FMT = 9994 )VNAME, MVAL( I ), MAXN
            FATAL = .TRUE.
         END IF
   10 CONTINUE
*
*     Read the values of N
*
      IF( SVD ) THEN
         WRITE( NOUT, FMT = 9988 )'M   ', ( MVAL( I ), I = 1, NN )
         READ( NIN, FMT = * )( NVAL( I ), I = 1, NN )
         DO 20 I = 1, NN
            IF( NVAL( I ).LT.0 ) THEN
               WRITE( NOUT, FMT = 9995 )'N   ', NVAL( I ), 0
               FATAL = .TRUE.
            ELSE IF( NVAL( I ).GT.MAXN ) THEN
               WRITE( NOUT, FMT = 9994 )'N   ', NVAL( I ), MAXN
               FATAL = .TRUE.
            END IF
   20    CONTINUE
      ELSE
         DO 30 I = 1, NN
            NVAL( I ) = MVAL( I )
   30    CONTINUE
      END IF
      WRITE( NOUT, FMT = 9988 )'N   ', ( NVAL( I ), I = 1, NN )
*
*     Read the number of parameter values.
*
      READ( NIN, FMT = * )NPARMS
      IF( NPARMS.LT.1 ) THEN
         WRITE( NOUT, FMT = 9995 )'NPARMS', NPARMS, 1
         NPARMS = 0
         FATAL = .TRUE.
      ELSE IF( NPARMS.GT.MAXIN ) THEN
         WRITE( NOUT, FMT = 9994 )'NPARMS', NPARMS, MAXIN
         NPARMS = 0
         FATAL = .TRUE.
      END IF
*
*     Read the values of NB
*
      READ( NIN, FMT = * )( NBVAL( I ), I = 1, NPARMS )
      DO 40 I = 1, NPARMS
         IF( NBVAL( I ).LT.0 ) THEN
            WRITE( NOUT, FMT = 9995 )'NB  ', NBVAL( I ), 0
            FATAL = .TRUE.
         END IF
   40 CONTINUE
      WRITE( NOUT, FMT = 9988 )'NB  ', ( NBVAL( I ), I = 1, NPARMS )
*
      IF( NEP .OR. GEP ) THEN
*
*        Read the values of NSHIFT
*
         READ( NIN, FMT = * )( NSVAL( I ), I = 1, NPARMS )
         DO 50 I = 1, NPARMS
            IF( NSVAL( I ).LT.0 ) THEN
               WRITE( NOUT, FMT = 9995 )'NS  ', NSVAL( I ), 0
               FATAL = .TRUE.
            END IF
   50    CONTINUE
         WRITE( NOUT, FMT = 9988 )'NS  ', ( NSVAL( I ), I = 1, NPARMS )
*
*        Read the values of MAXB
*
         READ( NIN, FMT = * )( MXBVAL( I ), I = 1, NPARMS )
         DO 60 I = 1, NPARMS
            IF( MXBVAL( I ).LT.0 ) THEN
               WRITE( NOUT, FMT = 9995 )'MAXB', MXBVAL( I ), 0
               FATAL = .TRUE.
            END IF
   60    CONTINUE
         WRITE( NOUT, FMT = 9988 )'MAXB',
     $      ( MXBVAL( I ), I = 1, NPARMS )
      ELSE
         DO 70 I = 1, NPARMS
            NSVAL( I ) = 1
            MXBVAL( I ) = 1
   70    CONTINUE
      END IF
*
      IF( GEP ) THEN
*
*        Read the values of NBMIN
*
         READ( NIN, FMT = * )( NBMVAL( I ), I = 1, NPARMS )
         DO 80 I = 1, NPARMS
            IF( NBMVAL( I ).LT.0 ) THEN
               WRITE( NOUT, FMT = 9995 )'NBMIN', NBMVAL( I ), 0
               FATAL = .TRUE.
            END IF
   80    CONTINUE
         WRITE( NOUT, FMT = 9988 )'NBMIN',
     $      ( NBMVAL( I ), I = 1, NPARMS )
*
*        Read the values of MINBLK
*
         READ( NIN, FMT = * )( NBKVAL( I ), I = 1, NPARMS )
         DO 90 I = 1, NPARMS
            IF( NBKVAL( I ).LT.0 ) THEN
               WRITE( NOUT, FMT = 9995 )'MINBLK', NBKVAL( I ), 0
               FATAL = .TRUE.
            END IF
   90    CONTINUE
         WRITE( NOUT, FMT = 9988 )'MINBLK',
     $      ( NBKVAL( I ), I = 1, NPARMS )
      ELSE
         DO 100 I = 1, NPARMS
            NBMVAL( I ) = MAXN + 1
            NBKVAL( I ) = MAXN + 1
  100    CONTINUE
      END IF
*
*     Read the values of LDA
*
      READ( NIN, FMT = * )( LDAVAL( I ), I = 1, NPARMS )
      DO 110 I = 1, NPARMS
         IF( LDAVAL( I ).LT.0 ) THEN
            WRITE( NOUT, FMT = 9995 )'LDA ', LDAVAL( I ), 0
            FATAL = .TRUE.
         ELSE IF( LDAVAL( I ).GT.LDAMAX ) THEN
            WRITE( NOUT, FMT = 9994 )'LDA ', LDAVAL( I ), LDAMAX
            FATAL = .TRUE.
         END IF
  110 CONTINUE
      WRITE( NOUT, FMT = 9988 )'LDA ', ( LDAVAL( I ), I = 1, NPARMS )
*
*     Read the minimum time a subroutine will be timed.
*
      READ( NIN, FMT = * )TIMMIN
      WRITE( NOUT, FMT = 9987 )TIMMIN
*
*     Read the number of matrix types to use in timing.
*
      READ( NIN, FMT = * )NTYPES
      IF( NTYPES.LT.0 ) THEN
         WRITE( NOUT, FMT = 9995 )'NTYPES', NTYPES, 0
         FATAL = .TRUE.
         NTYPES = 0
      END IF
*
*     Read the matrix types.
*
      IF( NEP ) THEN
         MAXTYP = MXTYPE( 1 )
      ELSE IF( SEP ) THEN
         MAXTYP = MXTYPE( 2 )
      ELSE IF( SVD ) THEN
         MAXTYP = MXTYPE( 3 )
      ELSE
         MAXTYP = MXTYPE( 4 )
      END IF
      IF( NTYPES.LT.MAXTYP ) THEN
         READ( NIN, FMT = * )( IWORK( I ), I = 1, NTYPES )
         DO 120 I = 1, MAXTYP
            DOTYPE( I ) = .FALSE.
  120    CONTINUE
         DO 130 I = 1, NTYPES
            IF( IWORK( I ).LT.0 ) THEN
               WRITE( NOUT, FMT = 9995 )'TYPE', IWORK( I ), 0
               FATAL = .TRUE.
            ELSE IF( IWORK( I ).GT.MAXTYP ) THEN
               WRITE( NOUT, FMT = 9994 )'TYPE', IWORK( I ), MAXTYP
               FATAL = .TRUE.
            ELSE
               DOTYPE( IWORK( I ) ) = .TRUE.
            END IF
  130    CONTINUE
      ELSE
         NTYPES = MAXTYP
         DO 140 I = 1, MAXT
            DOTYPE( I ) = .TRUE.
  140    CONTINUE
      END IF
*
      IF( FATAL ) THEN
         WRITE( NOUT, FMT = 9999 )
 9999    FORMAT( / ' Execution not attempted due to input errors' )
         STOP
      END IF
*
*     Read the input lines indicating the test path and the routines
*     to be timed.  The first three characters indicate the test path.
*
  150 CONTINUE
      READ( NIN, FMT = '(A80)', END = 160 )LINE
      C3 = LINE( 1: 3 )
*
*     -------------------------------------
*     NEP:  Nonsymmetric Eigenvalue Problem
*     -------------------------------------
*
      IF( LSAMEN( 3, C3, 'DHS' ) .OR. LSAMEN( 3, C3, 'NEP' ) ) THEN
         CALL DTIM21( LINE, NN, NVAL, MAXTYP, DOTYPE, NPARMS, NBVAL,
     $                NSVAL, MXBVAL, LDAVAL, TIMMIN, NOUT, ISEED,
     $                A( 1, 1 ), A( 1, 2 ), A( 1, 3 ), D( 1, 1 ), WORK,
     $                LWORK, LOGWRK, IWORK2, RESULT, MAXPRM, MAXT,
     $                MAXIN, OPCNTS, MAXPRM, MAXT, MAXIN, INFO )
         IF( INFO.NE.0 )
     $      WRITE( NOUT, FMT = 9986 )'DTIM21', INFO
*
*     ----------------------------------
*     SEP:  Symmetric Eigenvalue Problem
*     ----------------------------------
*
      ELSE IF( LSAMEN( 3, C3, 'DST' ) .OR. LSAMEN( 3, C3, 'SEP' ) ) THEN
         CALL DTIM22( LINE, NN, NVAL, MAXTYP, DOTYPE, NPARMS, NBVAL,
     $                LDAVAL, TIMMIN, NOUT, ISEED, A( 1, 1 ), D( 1, 1 ),
     $                D( 1, 2 ), D( 1, 3 ), A( 1, 2 ), A( 1, 3 ), WORK,
     $                LWORK, LOGWRK, IWORK2, RESULT, MAXPRM, MAXT,
     $                MAXIN, OPCNTS, MAXPRM, MAXT, MAXIN, INFO )
         IF( INFO.NE.0 )
     $      WRITE( NOUT, FMT = 9986 )'DTIM22', INFO
*
*     ----------------------------------
*     SVD:  Singular Value Decomposition
*     ----------------------------------
*
      ELSE IF( LSAMEN( 3, C3, 'DBD' ) .OR. LSAMEN( 3, C3, 'SVD' ) ) THEN
         CALL DTIM26( LINE, NN, NVAL, MVAL, MAXTYP, DOTYPE, NPARMS,
     $                NBVAL, LDAVAL, TIMMIN, NOUT, ISEED, A( 1, 1 ),
     $                A( 1, 2 ), A( 1, 3 ), A( 1, 4 ), D( 1, 1 ),
     $                D( 1, 2 ), D( 1, 3 ), D( 1, 4 ), WORK, LWORK,
     $                IWORK2, LOGWRK, RESULT, MAXPRM, MAXT, MAXIN,
     $                OPCNTS, MAXPRM, MAXT, MAXIN, INFO )
         IF( INFO.NE.0 )
     $      WRITE( NOUT, FMT = 9986 )'DTIM26', INFO
*
*     -------------------------------------------------
*     GEP:  Generalized Nonsymmetric Eigenvalue Problem
*     -------------------------------------------------
*
      ELSE IF( LSAMEN( 3, C3, 'DHG' ) .OR. LSAMEN( 3, C3, 'GEP' ) ) THEN
         CALL DTIM51( LINE, NN, NVAL, MAXTYP, DOTYPE, NPARMS, NBVAL,
     $                NSVAL, MXBVAL, NBMVAL, NBKVAL, LDAVAL, TIMMIN,
     $                NOUT, ISEED, A( 1, 1 ), A( 1, 2 ), A( 1, 3 ),
     $                A( 1, 4 ), A( 1, 5 ), A( 1, 6 ), D( 1, 1 ), WORK,
     $                LWORK, LOGWRK, RESULT, MAXPRM, MAXT, MAXIN,
     $                OPCNTS, MAXPRM, MAXT, MAXIN, INFO )
         IF( INFO.NE.0 )
     $      WRITE( NOUT, FMT = 9986 )'DTIM51', INFO
      ELSE
         WRITE( NOUT, FMT = * )
         WRITE( NOUT, FMT = * )
         WRITE( NOUT, FMT = 9996 )C3
      END IF
      GO TO 150
  160 CONTINUE
      WRITE( NOUT, FMT = 9998 )
 9998 FORMAT( / / ' End of timing run' )
      S2 = DSECND( )
      WRITE( NOUT, FMT = 9997 )S2 - S1
*
 9997 FORMAT( ' Total time used = ', F12.2, ' seconds', / )
 9996 FORMAT( 1X, A3, ':  Unrecognized path name' )
 9995 FORMAT( ' *** Invalid input value: ', A6, '=', I6, '; must be >=',
     $      I6 )
 9994 FORMAT( ' *** Invalid input value: ', A6, '=', I6, '; must be <=',
     $      I6 )
 9993 FORMAT( ' Timing the Nonsymmetric Eigenvalue Problem routines',
     $      / '    DGEHRD, DHSEQR, DTREVC, and DHSEIN' )
 9992 FORMAT( ' Timing the Symmetric Eigenvalue Problem routines',
     $      / '    DSYTRD, DSTEQR, and DSTERF' )
 9991 FORMAT( ' Timing the Singular Value Decomposition routines',
     $      / '    DGEBRD, DBDSQR, DORGBR, DBDSDC and DGESDD' )
 9990 FORMAT( ' Timing the Generalized Eigenvalue Problem routines',
     $      / '    DGGHRD, DHGEQZ, and DTGEVC ' )
 9989 FORMAT( / ' The following parameter values will be used:' )
 9988 FORMAT( '    Values of ', A5, ':  ', 10I6, / 19X, 10I6 )
 9987 FORMAT( / ' Minimum time a subroutine will be timed = ', F8.2,
     $      ' seconds', / )
 9986 FORMAT( ' *** Error code from ', A6, ' = ', I4 )
 9985 FORMAT( / ' LAPACK VERSION 3.0, released June 30, 1999 ' )
*
*     End of DTIMEE
*
      END
      INTEGER          FUNCTION ILAENV( ISPEC, NAME, OPTS, N1, N2, N3,
     $                 N4 )
*
*  -- LAPACK auxiliary routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     June 30, 1999
*
*     .. Scalar Arguments ..
      CHARACTER*( * )    NAME, OPTS
      INTEGER            ISPEC, N1, N2, N3, N4
*     ..
*
*  Purpose
*  =======
*
*  ILAENV returns problem-dependent parameters for the local
*  environment.  See ISPEC for a description of the parameters.
*
*  In this version, the problem-dependent parameters are contained in
*  the integer array IPARMS in the common block CLAENV and the value
*  with index ISPEC is copied to ILAENV.  This version of ILAENV is
*  to be used in conjunction with XLAENV in TESTING and TIMING.
*
*  Arguments
*  =========
*
*  ISPEC   (input) INTEGER
*          Specifies the parameter to be returned as the value of
*          ILAENV.
*          = 1: the optimal blocksize; if this value is 1, an unblocked
*               algorithm will give the best performance.
*          = 2: the minimum block size for which the block routine
*               should be used; if the usable block size is less than
*               this value, an unblocked routine should be used.
*          = 3: the crossover point (in a block routine, for N less
*               than this value, an unblocked routine should be used)
*          = 4: the number of shifts, used in the nonsymmetric
*               eigenvalue routines
*          = 5: the minimum column dimension for blocking to be used;
*               rectangular blocks must have dimension at least k by m,
*               where k is given by ILAENV(2,...) and m by ILAENV(5,...)
*          = 6: the crossover point for the SVD (when reducing an m by n
*               matrix to bidiagonal form, if max(m,n)/min(m,n) exceeds
*               this value, a QR factorization is used first to reduce
*               the matrix to a triangular form.)
*          = 7: the number of processors
*          = 8: the crossover point for the multishift QR and QZ methods
*               for nonsymmetric eigenvalue problems.
*          = 9: maximum size of the subproblems at the bottom of the
*               computation tree in the divide-and-conquer algorithm
*          =10: ieee NaN arithmetic can be trusted not to trap
*          =11: infinity arithmetic can be trusted not to trap
*
*          Other specifications (up to 100) can be added later.
*
*  NAME    (input) CHARACTER*(*)
*          The name of the calling subroutine.
*
*  OPTS    (input) CHARACTER*(*)
*          The character options to the subroutine NAME, concatenated
*          into a single character string.  For example, UPLO = 'U',
*          TRANS = 'T', and DIAG = 'N' for a triangular routine would
*          be specified as OPTS = 'UTN'.
*
*  N1      (input) INTEGER
*  N2      (input) INTEGER
*  N3      (input) INTEGER
*  N4      (input) INTEGER
*          Problem dimensions for the subroutine NAME; these may not all
*          be required.
*
* (ILAENV) (output) INTEGER
*          >= 0: the value of the parameter specified by ISPEC
*          < 0:  if ILAENV = -k, the k-th argument had an illegal value.
*
*  Further Details
*  ===============
*
*  The following conventions have been used when calling ILAENV from the
*  LAPACK routines:
*  1)  OPTS is a concatenation of all of the character options to
*      subroutine NAME, in the same order that they appear in the
*      argument list for NAME, even if they are not used in determining
*      the value of the parameter specified by ISPEC.
*  2)  The problem dimensions N1, N2, N3, N4 are specified in the order
*      that they appear in the argument list for NAME.  N1 is used
*      first, N2 second, and so on, and unused problem dimensions are
*      passed a value of -1.
*  3)  The parameter value returned by ILAENV is checked for validity in
*      the calling subroutine.  For example, ILAENV is used to retrieve
*      the optimal blocksize for STRTRI as follows:
*
*      NB = ILAENV( 1, 'STRTRI', UPLO // DIAG, N, -1, -1, -1 )
*      IF( NB.LE.1 ) NB = MAX( 1, N )
*
*  =====================================================================
*
*     .. Intrinsic Functions ..
      INTRINSIC          INT, MIN, REAL
*     ..
*     .. External Functions ..
      INTEGER            IEEECK
      EXTERNAL           IEEECK
*     ..
*     .. Arrays in Common ..
      INTEGER            IPARMS( 100 )
*     ..
*     .. Common blocks ..
      COMMON             / CLAENV / IPARMS
*     ..
*     .. Save statement ..
      SAVE               / CLAENV /
*     ..
*     .. Executable Statements ..
*
      IF( ISPEC.GE.1 .AND. ISPEC.LE.5 ) THEN
*
*        Return a value from the common block.
*
         ILAENV = IPARMS( ISPEC )
*
      ELSE IF( ISPEC.EQ.6 ) THEN
*
*        Compute SVD crossover point.
*
         ILAENV = INT( REAL( MIN( N1, N2 ) )*1.6E0 )
*
      ELSE IF( ISPEC.GE.7 .AND. ISPEC.LE.9 ) THEN
*
*        Return a value from the common block.
*
         ILAENV = IPARMS( ISPEC )
*
      ELSE IF( ISPEC.EQ.10 ) THEN
*
*        IEEE NaN arithmetic can be trusted not to trap
*
         ILAENV = 1
         IF( ILAENV.EQ.1 ) THEN
            ILAENV = IEEECK( 0, 0.0, 1.0 )
         END IF
*
      ELSE IF( ISPEC.EQ.11 ) THEN
*
*        Infinity arithmetic can be trusted not to trap
*
         ILAENV = 1
         IF( ILAENV.EQ.1 ) THEN
            ILAENV = IEEECK( 1, 0.0, 1.0 )
         END IF
*
      ELSE
*
*        Invalid value for ISPEC
*
         ILAENV = -1
      END IF
*
      RETURN
*
*     End of ILAENV
*
      END
      SUBROUTINE XLAENV( ISPEC, NVALUE )
*
*  -- LAPACK auxiliary routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     June 30, 1999
*
*     .. Scalar Arguments ..
      INTEGER            ISPEC, NVALUE
*     ..
*
*  Purpose
*  =======
*
*  XLAENV sets certain machine- and problem-dependent quantities
*  which will later be retrieved by ILAENV.
*
*  Arguments
*  =========
*
*  ISPEC   (input) INTEGER
*          Specifies the parameter to be set in the COMMON array IPARMS.
*          = 1: the optimal blocksize; if this value is 1, an unblocked
*               algorithm will give the best performance.
*          = 2: the minimum block size for which the block routine
*               should be used; if the usable block size is less than
*               this value, an unblocked routine should be used.
*          = 3: the crossover point (in a block routine, for N less
*               than this value, an unblocked routine should be used)
*          = 4: the number of shifts, used in the nonsymmetric
*               eigenvalue routines
*          = 5: the minimum column dimension for blocking to be used;
*               rectangular blocks must have dimension at least k by m,
*               where k is given by ILAENV(2,...) and m by ILAENV(5,...)
*          = 6: the crossover point for the SVD (when reducing an m by n
*               matrix to bidiagonal form, if max(m,n)/min(m,n) exceeds
*               this value, a QR factorization is used first to reduce
*               the matrix to a triangular form)
*          = 7: the number of processors
*          = 8: another crossover point, for the multishift QR and QZ
*               methods for nonsymmetric eigenvalue problems.
*          = 9: maximum size of the subproblems at the bottom of the
*               computation tree in the divide-and-conquer algorithm
*               (used by xGELSD and xGESDD)
*          =10: ieee NaN arithmetic can be trusted not to trap
*          =11: infinity arithmetic can be trusted not to trap
*
*  NVALUE  (input) INTEGER
*          The value of the parameter specified by ISPEC.
*
*  =====================================================================
*
*     .. Arrays in Common ..
      INTEGER            IPARMS( 100 )
*     ..
*     .. Common blocks ..
      COMMON             / CLAENV / IPARMS
*     ..
*     .. Save statement ..
      SAVE               / CLAENV /
*     ..
*     .. Executable Statements ..
*
      IF( ISPEC.GE.1 .AND. ISPEC.LE.9 ) THEN
         IPARMS( ISPEC ) = NVALUE
      END IF
*
      RETURN
*
*     End of XLAENV
*
      END
