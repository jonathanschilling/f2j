      SUBROUTINE DBDSDC( UPLO, COMPQ, N, D, E, U, LDU, VT, LDVT, Q, IQ,
     $                   WORK, IWORK, INFO )
*
*  -- LAPACK routine (instrumented to count ops, version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     October 31, 1999
*
*     .. Scalar Arguments ..
      CHARACTER          COMPQ, UPLO
      INTEGER            INFO, LDU, LDVT, N
*     ..
*     .. Array Arguments ..
      INTEGER            IQ( * ), IWORK( * )
      DOUBLE PRECISION   D( * ), E( * ), Q( * ), U( LDU, * ),
     $                   VT( LDVT, * ), WORK( * )
*     ..
*     .. Common blocks ..
      COMMON             / LATIME / OPS, ITCNT
*     ..
*     .. Scalars in Common ..
      DOUBLE PRECISION   ITCNT, OPS
*     ..
*
*  Purpose
*  =======
*
*  DBDSDC computes the singular value decomposition (SVD) of a real
*  N-by-N (upper or lower) bidiagonal matrix B:  B = U * S * VT,
*  using a divide and conquer method, where S is a diagonal matrix
*  with non-negative diagonal elements (the singular values of B), and
*  U and VT are orthogonal matrices of left and right singular vectors,
*  respectively. DBDSDC can be used to compute all singular values,
*  and optionally, singular vectors or singular vectors in compact form.
*
*  This code makes very mild assumptions about floating point
*  arithmetic. It will work on machines with a guard digit in
*  add/subtract, or on those binary machines without guard digits
*  which subtract like the Cray X-MP, Cray Y-MP, Cray C-90, or Cray-2.
*  It could conceivably fail on hexadecimal or decimal machines
*  without guard digits, but we know of none.  See DLASD3 for details.
*
*  The code currently call DLASDQ if singular values only are desired.
*  However, it can be slightly modified to compute singular values
*  using the divide and conquer method.
*
*  Arguments
*  =========
*
*  UPLO    (input) CHARACTER*1
*          = 'U':  B is upper bidiagonal.
*          = 'L':  B is lower bidiagonal.
*
*  COMPQ   (input) CHARACTER*1
*          Specifies whether singular vectors are to be computed
*          as follows:
*          = 'N':  Compute singular values only;
*          = 'P':  Compute singular values and compute singular
*                  vectors in compact form;
*          = 'I':  Compute singular values and singular vectors.
*
*  N       (input) INTEGER
*          The order of the matrix B.  N >= 0.
*
*  D       (input/output) DOUBLE PRECISION array, dimension (N)
*          On entry, the n diagonal elements of the bidiagonal matrix B.
*          On exit, if INFO=0, the singular values of B.
*
*  E       (input/output) DOUBLE PRECISION array, dimension (N)
*          On entry, the elements of E contain the offdiagonal
*          elements of the bidiagonal matrix whose SVD is desired.
*          On exit, E has been destroyed.
*
*  U       (output) DOUBLE PRECISION array, dimension (LDU,N)
*          If  COMPQ = 'I', then:
*             On exit, if INFO = 0, U contains the left singular vectors
*             of the bidiagonal matrix.
*          For other values of COMPQ, U is not referenced.
*
*  LDU     (input) INTEGER
*          The leading dimension of the array U.  LDU >= 1.
*          If singular vectors are desired, then LDU >= max( 1, N ).
*
*  VT      (output) DOUBLE PRECISION array, dimension (LDVT,N)
*          If  COMPQ = 'I', then:
*             On exit, if INFO = 0, VT' contains the right singular
*             vectors of the bidiagonal matrix.
*          For other values of COMPQ, VT is not referenced.
*
*  LDVT    (input) INTEGER
*          The leading dimension of the array VT.  LDVT >= 1.
*          If singular vectors are desired, then LDVT >= max( 1, N ).
*
*  Q       (output) DOUBLE PRECISION array, dimension (LDQ)
*          If  COMPQ = 'P', then:
*             On exit, if INFO = 0, Q and IQ contain the left
*             and right singular vectors in a compact form,
*             requiring O(N log N) space instead of 2*N**2.
*             In particular, Q contains all the DOUBLE PRECISION data in
*             LDQ >= N*(11 + 2*SMLSIZ + 8*INT(LOG_2(N/(SMLSIZ+1))))
*             words of memory, where SMLSIZ is returned by ILAENV and
*             is equal to the maximum size of the subproblems at the
*             bottom of the computation tree (usually about 25).
*          For other values of COMPQ, Q is not referenced.
*
*  IQ      (output) INTEGER array, dimension (LDIQ)
*          If  COMPQ = 'P', then:
*             On exit, if INFO = 0, Q and IQ contain the left
*             and right singular vectors in a compact form,
*             requiring O(N log N) space instead of 2*N**2.
*             In particular, IQ contains all INTEGER data in
*             LDIQ >= N*(3 + 3*INT(LOG_2(N/(SMLSIZ+1))))
*             words of memory, where SMLSIZ is returned by ILAENV and
*             is equal to the maximum size of the subproblems at the
*             bottom of the computation tree (usually about 25).
*          For other values of COMPQ, IQ is not referenced.
*
*  WORK    (workspace) DOUBLE PRECISION array, dimension (LWORK)
*          If COMPQ = 'N' then LWORK >= (4 * N).
*          If COMPQ = 'P' then LWORK >= (6 * N).
*          If COMPQ = 'I' then LWORK >= (3 * N**2 + 4 * N).
*
*  IWORK   (workspace) INTEGER array, dimension (7*N)
*
*  INFO    (output) INTEGER
*          = 0:  successful exit.
*          < 0:  if INFO = -i, the i-th argument had an illegal value.
*          > 0:  The algorithm failed to compute an singular value.
*                The update process of divide and conquer failed.
*
*  Further Details
*  ===============
*
*  Based on contributions by
*     Ming Gu and Huan Ren, Computer Science Division, University of
*     California at Berkeley, USA
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE, TWO
      PARAMETER          ( ZERO = 0.0D+0, ONE = 1.0D+0, TWO = 2.0D+0 )
*     ..
*     .. Local Scalars ..
      INTEGER            DIFL, DIFR, GIVCOL, GIVNUM, GIVPTR, I, IC,
     $                   ICOMPQ, IERR, II, IS, IU, IUPLO, IVT, J, K, KK,
     $                   MLVL, NM1, NSIZE, PERM, POLES, QSTART, SMLSIZ,
     $                   SMLSZP, SQRE, START, WSTART, Z
      DOUBLE PRECISION   CS, EPS, ORGNRM, P, R, SN
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      INTEGER            ILAENV
      DOUBLE PRECISION   DLAMCH, DLANST
      EXTERNAL           LSAME, ILAENV, DLAMCH, DLANST
*     ..
*     .. External Subroutines ..
      EXTERNAL           DCOPY, DLARTG, DLASCL, DLASD0, DLASDA, DLASDQ,
     $                   DLASET, DLASR, DSWAP, XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, DBLE, INT, LOG, SIGN
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters.
*
      INFO = 0
*
      IUPLO = 0
      IF( LSAME( UPLO, 'U' ) )
     $   IUPLO = 1
      IF( LSAME( UPLO, 'L' ) )
     $   IUPLO = 2
      IF( LSAME( COMPQ, 'N' ) ) THEN
         ICOMPQ = 0
      ELSE IF( LSAME( COMPQ, 'P' ) ) THEN
         ICOMPQ = 1
      ELSE IF( LSAME( COMPQ, 'I' ) ) THEN
         ICOMPQ = 2
      ELSE
         ICOMPQ = -1
      END IF
      IF( IUPLO.EQ.0 ) THEN
         INFO = -1
      ELSE IF( ICOMPQ.LT.0 ) THEN
         INFO = -2
      ELSE IF( N.LT.0 ) THEN
         INFO = -3
      ELSE IF( ( LDU.LT.1 ) .OR. ( ( ICOMPQ.EQ.2 ) .AND. ( LDU.LT.
     $         N ) ) ) THEN
         INFO = -7
      ELSE IF( ( LDVT.LT.1 ) .OR. ( ( ICOMPQ.EQ.2 ) .AND. ( LDVT.LT.
     $         N ) ) ) THEN
         INFO = -9
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DBDSDC', -INFO )
         RETURN
      END IF
*
*     Quick return if possible
*
      IF( N.EQ.0 )
     $   RETURN
      SMLSIZ = ILAENV( 9, 'DBDSDC', ' ', 0, 0, 0, 0 )
      IF( N.EQ.1 ) THEN
         IF( ICOMPQ.EQ.1 ) THEN
            Q( 1 ) = SIGN( ONE, D( 1 ) )
            Q( 1+SMLSIZ*N ) = ONE
         ELSE IF( ICOMPQ.EQ.2 ) THEN
            U( 1, 1 ) = SIGN( ONE, D( 1 ) )
            VT( 1, 1 ) = ONE
         END IF
         D( 1 ) = ABS( D( 1 ) )
         RETURN
      END IF
      NM1 = N - 1
*
*     If matrix lower bidiagonal, rotate to be upper bidiagonal
*     by applying Givens rotations on the left
*
      WSTART = 1
      QSTART = 3
      IF( ICOMPQ.EQ.1 ) THEN
         CALL DCOPY( N, D, 1, Q( 1 ), 1 )
         CALL DCOPY( N-1, E, 1, Q( N+1 ), 1 )
      END IF
      IF( IUPLO.EQ.2 ) THEN
         QSTART = 5
         WSTART = 2*N - 1
         OPS = OPS + DBLE( 8*( N-1 ) )
         DO 10 I = 1, N - 1
            CALL DLARTG( D( I ), E( I ), CS, SN, R )
            D( I ) = R
            E( I ) = SN*D( I+1 )
            D( I+1 ) = CS*D( I+1 )
            IF( ICOMPQ.EQ.1 ) THEN
               Q( I+2*N ) = CS
               Q( I+3*N ) = SN
            ELSE IF( ICOMPQ.EQ.2 ) THEN
               WORK( I ) = CS
               WORK( NM1+I ) = -SN
            END IF
   10    CONTINUE
      END IF
*
*     If ICOMPQ = 0, use DLASDQ to compute the singular values.
*
      IF( ICOMPQ.EQ.0 ) THEN
         CALL DLASDQ( 'U', 0, N, 0, 0, 0, D, E, VT, LDVT, U, LDU, U,
     $                LDU, WORK( WSTART ), INFO )
         GO TO 40
      END IF
*
*     If N is smaller than the minimum divide size SMLSIZ, then solve
*     the problem with another solver.
*
      IF( N.LE.SMLSIZ ) THEN
         IF( ICOMPQ.EQ.2 ) THEN
            CALL DLASET( 'A', N, N, ZERO, ONE, U, LDU )
            CALL DLASET( 'A', N, N, ZERO, ONE, VT, LDVT )
            CALL DLASDQ( 'U', 0, N, N, N, 0, D, E, VT, LDVT, U, LDU, U,
     $                   LDU, WORK( WSTART ), INFO )
         ELSE IF( ICOMPQ.EQ.1 ) THEN
            IU = 1
            IVT = IU + N
            CALL DLASET( 'A', N, N, ZERO, ONE, Q( IU+( QSTART-1 )*N ),
     $                   N )
            CALL DLASET( 'A', N, N, ZERO, ONE, Q( IVT+( QSTART-1 )*N ),
     $                   N )
            CALL DLASDQ( 'U', 0, N, N, N, 0, D, E,
     $                   Q( IVT+( QSTART-1 )*N ), N,
     $                   Q( IU+( QSTART-1 )*N ), N,
     $                   Q( IU+( QSTART-1 )*N ), N, WORK( WSTART ),
     $                   INFO )
         END IF
         GO TO 40
      END IF
*
      IF( ICOMPQ.EQ.2 ) THEN
         CALL DLASET( 'A', N, N, ZERO, ONE, U, LDU )
         CALL DLASET( 'A', N, N, ZERO, ONE, VT, LDVT )
      END IF
*
*     Scale.
*
      ORGNRM = DLANST( 'M', N, D, E )
      IF( ORGNRM.EQ.ZERO )
     $   RETURN
      OPS = OPS + DBLE( N+NM1 )
      CALL DLASCL( 'G', 0, 0, ORGNRM, ONE, N, 1, D, N, IERR )
      CALL DLASCL( 'G', 0, 0, ORGNRM, ONE, NM1, 1, E, NM1, IERR )
*
      EPS = DLAMCH( 'Epsilon' )
*
      MLVL = INT( LOG( DBLE( N ) / DBLE( SMLSIZ+1 ) ) / LOG( TWO ) ) + 1
      SMLSZP = SMLSIZ + 1
*
      IF( ICOMPQ.EQ.1 ) THEN
         IU = 1
         IVT = 1 + SMLSIZ
         DIFL = IVT + SMLSZP
         DIFR = DIFL + MLVL
         Z = DIFR + MLVL*2
         IC = Z + MLVL
         IS = IC + 1
         POLES = IS + 1
         GIVNUM = POLES + 2*MLVL
*
         K = 1
         GIVPTR = 2
         PERM = 3
         GIVCOL = PERM + MLVL
      END IF
*
      DO 20 I = 1, N
         IF( ABS( D( I ) ).LT.EPS ) THEN
            D( I ) = SIGN( EPS, D( I ) )
         END IF
   20 CONTINUE
*
      START = 1
      SQRE = 0
*
      DO 30 I = 1, NM1
         IF( ( ABS( E( I ) ).LT.EPS ) .OR. ( I.EQ.NM1 ) ) THEN
*
*        Subproblem found. First determine its size and then
*        apply divide and conquer on it.
*
            IF( I.LT.NM1 ) THEN
*
*        A subproblem with E(I) small for I < NM1.
*
               NSIZE = I - START + 1
            ELSE IF( ABS( E( I ) ).GE.EPS ) THEN
*
*        A subproblem with E(NM1) not too small but I = NM1.
*
               NSIZE = N - START + 1
            ELSE
*
*        A subproblem with E(NM1) small. This implies an
*        1-by-1 subproblem at D(N). Solve this 1-by-1 problem
*        first.
*
               NSIZE = I - START + 1
               IF( ICOMPQ.EQ.2 ) THEN
                  U( N, N ) = SIGN( ONE, D( N ) )
                  VT( N, N ) = ONE
               ELSE IF( ICOMPQ.EQ.1 ) THEN
                  Q( N+( QSTART-1 )*N ) = SIGN( ONE, D( N ) )
                  Q( N+( SMLSIZ+QSTART-1 )*N ) = ONE
               END IF
               D( N ) = ABS( D( N ) )
            END IF
            IF( ICOMPQ.EQ.2 ) THEN
               CALL DLASD0( NSIZE, SQRE, D( START ), E( START ),
     $                      U( START, START ), LDU, VT( START, START ),
     $                      LDVT, SMLSIZ, IWORK, WORK( WSTART ), INFO )
            ELSE
               CALL DLASDA( ICOMPQ, SMLSIZ, NSIZE, SQRE, D( START ),
     $                      E( START ), Q( START+( IU+QSTART-2 )*N ), N,
     $                      Q( START+( IVT+QSTART-2 )*N ),
     $                      IQ( START+K*N ), Q( START+( DIFL+QSTART-2 )*
     $                      N ), Q( START+( DIFR+QSTART-2 )*N ),
     $                      Q( START+( Z+QSTART-2 )*N ),
     $                      Q( START+( POLES+QSTART-2 )*N ),
     $                      IQ( START+GIVPTR*N ), IQ( START+GIVCOL*N ),
     $                      N, IQ( START+PERM*N ),
     $                      Q( START+( GIVNUM+QSTART-2 )*N ),
     $                      Q( START+( IC+QSTART-2 )*N ),
     $                      Q( START+( IS+QSTART-2 )*N ),
     $                      WORK( WSTART ), IWORK, INFO )
               IF( INFO.NE.0 ) THEN
                  RETURN
               END IF
            END IF
            START = I + 1
         END IF
   30 CONTINUE
*
*     Unscale
*
      OPS = OPS + DBLE( N )
      CALL DLASCL( 'G', 0, 0, ONE, ORGNRM, N, 1, D, N, IERR )
   40 CONTINUE
*
*     Use Selection Sort to minimize swaps of singular vectors
*
      DO 60 II = 2, N
         I = II - 1
         KK = I
         P = D( I )
         DO 50 J = II, N
            IF( D( J ).GT.P ) THEN
               KK = J
               P = D( J )
            END IF
   50    CONTINUE
         IF( KK.NE.I ) THEN
            D( KK ) = D( I )
            D( I ) = P
            IF( ICOMPQ.EQ.1 ) THEN
               IQ( I ) = KK
            ELSE IF( ICOMPQ.EQ.2 ) THEN
               CALL DSWAP( N, U( 1, I ), 1, U( 1, KK ), 1 )
               CALL DSWAP( N, VT( I, 1 ), LDVT, VT( KK, 1 ), LDVT )
            END IF
         ELSE IF( ICOMPQ.EQ.1 ) THEN
            IQ( I ) = I
         END IF
   60 CONTINUE
*
*     If ICOMPQ = 1, use IQ(N,1) as the indicator for UPLO
*
      IF( ICOMPQ.EQ.1 ) THEN
         IF( IUPLO.EQ.1 ) THEN
            IQ( N ) = 1
         ELSE
            IQ( N ) = 0
         END IF
      END IF
*
*     If B is lower bidiagonal, update U by those Givens rotations
*     which rotated B to be upper bidiagonal
*
      IF( ( IUPLO.EQ.2 ) .AND. ( ICOMPQ.EQ.2 ) ) THEN
         OPS = OPS + DBLE( 6*( N-1 )*N )
         CALL DLASR( 'L', 'V', 'B', N, N, WORK( 1 ), WORK( N ), U, LDU )
      END IF
*
      RETURN
*
*     End of DBDSDC
*
      END
      SUBROUTINE DBDSQR( UPLO, N, NCVT, NRU, NCC, D, E, VT, LDVT, U,
     $                   LDU, C, LDC, WORK, INFO )
*
*  -- LAPACK routine (instrumented to count operations, version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     October 31, 1999
*
*     .. Scalar Arguments ..
      CHARACTER          UPLO
      INTEGER            INFO, LDC, LDU, LDVT, N, NCC, NCVT, NRU
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   C( LDC, * ), D( * ), E( * ), U( LDU, * ),
     $                   VT( LDVT, * ), WORK( * )
*     ..
*     Common block to return operation count and iteration count
*     ITCNT is initialized to 0, OPS is only incremented
*     .. Common blocks ..
      COMMON             / LATIME / OPS, ITCNT
*     ..
*     .. Scalars in Common ..
      DOUBLE PRECISION   ITCNT, OPS
*     ..
*
*  Purpose
*  =======
*
*  DBDSQR computes the singular value decomposition (SVD) of a real
*  N-by-N (upper or lower) bidiagonal matrix B:  B = Q * S * P' (P'
*  denotes the transpose of P), where S is a diagonal matrix with
*  non-negative diagonal elements (the singular values of B), and Q
*  and P are orthogonal matrices.
*
*  The routine computes S, and optionally computes U * Q, P' * VT,
*  or Q' * C, for given real input matrices U, VT, and C.
*
*  See "Computing  Small Singular Values of Bidiagonal Matrices With
*  Guaranteed High Relative Accuracy," by J. Demmel and W. Kahan,
*  LAPACK Working Note #3 (or SIAM J. Sci. Statist. Comput. vol. 11,
*  no. 5, pp. 873-912, Sept 1990) and
*  "Accurate singular values and differential qd algorithms," by
*  B. Parlett and V. Fernando, Technical Report CPAM-554, Mathematics
*  Department, University of California at Berkeley, July 1992
*  for a detailed description of the algorithm.
*
*  Arguments
*  =========
*
*  UPLO    (input) CHARACTER*1
*          = 'U':  B is upper bidiagonal;
*          = 'L':  B is lower bidiagonal.
*
*  N       (input) INTEGER
*          The order of the matrix B.  N >= 0.
*
*  NCVT    (input) INTEGER
*          The number of columns of the matrix VT. NCVT >= 0.
*
*  NRU     (input) INTEGER
*          The number of rows of the matrix U. NRU >= 0.
*
*  NCC     (input) INTEGER
*          The number of columns of the matrix C. NCC >= 0.
*
*  D       (input/output) DOUBLE PRECISION array, dimension (N)
*          On entry, the n diagonal elements of the bidiagonal matrix B.
*          On exit, if INFO=0, the singular values of B in decreasing
*          order.
*
*  E       (input/output) DOUBLE PRECISION array, dimension (N)
*          On entry, the elements of E contain the
*          offdiagonal elements of the bidiagonal matrix whose SVD
*          is desired. On normal exit (INFO = 0), E is destroyed.
*          If the algorithm does not converge (INFO > 0), D and E
*          will contain the diagonal and superdiagonal elements of a
*          bidiagonal matrix orthogonally equivalent to the one given
*          as input. E(N) is used for workspace.
*
*  VT      (input/output) DOUBLE PRECISION array, dimension (LDVT, NCVT)
*          On entry, an N-by-NCVT matrix VT.
*          On exit, VT is overwritten by P' * VT.
*          VT is not referenced if NCVT = 0.
*
*  LDVT    (input) INTEGER
*          The leading dimension of the array VT.
*          LDVT >= max(1,N) if NCVT > 0; LDVT >= 1 if NCVT = 0.
*
*  U       (input/output) DOUBLE PRECISION array, dimension (LDU, N)
*          On entry, an NRU-by-N matrix U.
*          On exit, U is overwritten by U * Q.
*          U is not referenced if NRU = 0.
*
*  LDU     (input) INTEGER
*          The leading dimension of the array U.  LDU >= max(1,NRU).
*
*  C       (input/output) DOUBLE PRECISION array, dimension (LDC, NCC)
*          On entry, an N-by-NCC matrix C.
*          On exit, C is overwritten by Q' * C.
*          C is not referenced if NCC = 0.
*
*  LDC     (input) INTEGER
*          The leading dimension of the array C.
*          LDC >= max(1,N) if NCC > 0; LDC >=1 if NCC = 0.
*
*  WORK    (workspace) DOUBLE PRECISION array, dimension (4*N)
*
*  INFO    (output) INTEGER
*          = 0:  successful exit
*          < 0:  If INFO = -i, the i-th argument had an illegal value
*          > 0:  the algorithm did not converge; D and E contain the
*                elements of a bidiagonal matrix which is orthogonally
*                similar to the input matrix B;  if INFO = i, i
*                elements of E have not converged to zero.
*
*  Internal Parameters
*  ===================
*
*  TOLMUL  DOUBLE PRECISION, default = max(10,min(100,EPS**(-1/8)))
*          TOLMUL controls the convergence criterion of the QR loop.
*          If it is positive, TOLMUL*EPS is the desired relative
*             precision in the computed singular values.
*          If it is negative, abs(TOLMUL*EPS*sigma_max) is the
*             desired absolute accuracy in the computed singular
*             values (corresponds to relative accuracy
*             abs(TOLMUL*EPS) in the largest singular value.
*          abs(TOLMUL) should be between 1 and 1/EPS, and preferably
*             between 10 (for fast convergence) and .1/EPS
*             (for there to be some accuracy in the results).
*          Default is to lose at either one eighth or 2 of the
*             available decimal digits in each computed singular value
*             (whichever is smaller).
*
*  MAXITR  INTEGER, default = 6
*          MAXITR controls the maximum number of passes of the
*          algorithm through its inner loop. The algorithms stops
*          (and so fails to converge) if the number of passes
*          through the inner loop exceeds MAXITR*N**2.
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO
      PARAMETER          ( ZERO = 0.0D0 )
      DOUBLE PRECISION   ONE
      PARAMETER          ( ONE = 1.0D0 )
      DOUBLE PRECISION   NEGONE
      PARAMETER          ( NEGONE = -1.0D0 )
      DOUBLE PRECISION   HNDRTH
      PARAMETER          ( HNDRTH = 0.01D0 )
      DOUBLE PRECISION   TEN
      PARAMETER          ( TEN = 10.0D0 )
      DOUBLE PRECISION   HNDRD
      PARAMETER          ( HNDRD = 100.0D0 )
      DOUBLE PRECISION   MEIGTH
      PARAMETER          ( MEIGTH = -0.125D0 )
      INTEGER            MAXITR
      PARAMETER          ( MAXITR = 6 )
*     ..
*     .. Local Scalars ..
      LOGICAL            LOWER, ROTATE
      INTEGER            I, IDIR, ISUB, ITER, J, LL, LLL, M, MAXIT, NM1,
     $                   NM12, NM13, OLDLL, OLDM
      DOUBLE PRECISION   ABSE, ABSS, COSL, COSR, CS, EPS, F, G, H, MU,
     $                   OLDCS, OLDSN, R, SHIFT, SIGMN, SIGMX, SINL,
     $                   SINR, SLL, SMAX, SMIN, SMINL, SMINLO, SMINOA,
     $                   SN, THRESH, TOL, TOLMUL, UNFL
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      DOUBLE PRECISION   DLAMCH
      EXTERNAL           LSAME, DLAMCH
*     ..
*     .. External Subroutines ..
      EXTERNAL           DLARTG, DLAS2, DLASQ1, DLASR, DLASV2, DROT,
     $                   DSCAL, DSWAP, XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, DBLE, MAX, MIN, SIGN, SQRT
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters.
*
      INFO = 0
      LOWER = LSAME( UPLO, 'L' )
      IF( .NOT.LSAME( UPLO, 'U' ) .AND. .NOT.LOWER ) THEN
         INFO = -1
      ELSE IF( N.LT.0 ) THEN
         INFO = -2
      ELSE IF( NCVT.LT.0 ) THEN
         INFO = -3
      ELSE IF( NRU.LT.0 ) THEN
         INFO = -4
      ELSE IF( NCC.LT.0 ) THEN
         INFO = -5
      ELSE IF( ( NCVT.EQ.0 .AND. LDVT.LT.1 ) .OR.
     $         ( NCVT.GT.0 .AND. LDVT.LT.MAX( 1, N ) ) ) THEN
         INFO = -9
      ELSE IF( LDU.LT.MAX( 1, NRU ) ) THEN
         INFO = -11
      ELSE IF( ( NCC.EQ.0 .AND. LDC.LT.1 ) .OR.
     $         ( NCC.GT.0 .AND. LDC.LT.MAX( 1, N ) ) ) THEN
         INFO = -13
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DBDSQR', -INFO )
         RETURN
      END IF
      IF( N.EQ.0 )
     $   RETURN
      IF( N.EQ.1 )
     $   GO TO 160
*
*     ROTATE is true if any singular vectors desired, false otherwise
*
      ROTATE = ( NCVT.GT.0 ) .OR. ( NRU.GT.0 ) .OR. ( NCC.GT.0 )
*
*     If no singular vectors desired, use qd algorithm
*
      IF( .NOT.ROTATE ) THEN
         CALL DLASQ1( N, D, E, WORK, INFO )
         RETURN
      END IF
*
      NM1 = N - 1
      NM12 = NM1 + NM1
      NM13 = NM12 + NM1
      IDIR = 0
*
*     Get machine constants
*
      EPS = DLAMCH( 'Epsilon' )
      UNFL = DLAMCH( 'Safe minimum' )
*
*     If matrix lower bidiagonal, rotate to be upper bidiagonal
*     by applying Givens rotations on the left
*
      IF( LOWER ) THEN
         OPS = OPS + DBLE( N-1 )*( 8+6*( NRU+NCC ) )
         DO 10 I = 1, N - 1
            CALL DLARTG( D( I ), E( I ), CS, SN, R )
            D( I ) = R
            E( I ) = SN*D( I+1 )
            D( I+1 ) = CS*D( I+1 )
            WORK( I ) = CS
            WORK( NM1+I ) = SN
   10    CONTINUE
*
*        Update singular vectors if desired
*
         IF( NRU.GT.0 )
     $      CALL DLASR( 'R', 'V', 'F', NRU, N, WORK( 1 ), WORK( N ), U,
     $                  LDU )
         IF( NCC.GT.0 )
     $      CALL DLASR( 'L', 'V', 'F', N, NCC, WORK( 1 ), WORK( N ), C,
     $                  LDC )
      END IF
*
*     Compute singular values to relative accuracy TOL
*     (By setting TOL to be negative, algorithm will compute
*     singular values to absolute accuracy ABS(TOL)*norm(input matrix))
*
      OPS = OPS + 4
      TOLMUL = MAX( TEN, MIN( HNDRD, EPS**MEIGTH ) )
      TOL = TOLMUL*EPS
*
*     Compute approximate maximum, minimum singular values
*
      SMAX = ZERO
      DO 20 I = 1, N
         SMAX = MAX( SMAX, ABS( D( I ) ) )
   20 CONTINUE
      DO 30 I = 1, N - 1
         SMAX = MAX( SMAX, ABS( E( I ) ) )
   30 CONTINUE
      SMINL = ZERO
      IF( TOL.GE.ZERO ) THEN
*
*        Relative accuracy desired
*
         SMINOA = ABS( D( 1 ) )
         IF( SMINOA.EQ.ZERO )
     $      GO TO 50
         MU = SMINOA
         OPS = OPS + 3*N - 1
         DO 40 I = 2, N
            MU = ABS( D( I ) )*( MU / ( MU+ABS( E( I-1 ) ) ) )
            SMINOA = MIN( SMINOA, MU )
            IF( SMINOA.EQ.ZERO )
     $         GO TO 50
   40    CONTINUE
   50    CONTINUE
         SMINOA = SMINOA / SQRT( DBLE( N ) )
         THRESH = MAX( TOL*SMINOA, MAXITR*N*N*UNFL )
      ELSE
*
*        Absolute accuracy desired
*
         THRESH = MAX( ABS( TOL )*SMAX, MAXITR*N*N*UNFL )
      END IF
*
*     Prepare for main iteration loop for the singular values
*     (MAXIT is the maximum number of passes through the inner
*     loop permitted before nonconvergence signalled.)
*
      MAXIT = MAXITR*N*N
      ITER = 0
      OLDLL = -1
      OLDM = -1
*
*     M points to last element of unconverged part of matrix
*
      M = N
*
*     Begin main iteration loop
*
   60 CONTINUE
*
*     Check for convergence or exceeding iteration count
*
      IF( M.LE.1 )
     $   GO TO 160
      IF( ITER.GT.MAXIT )
     $   GO TO 200
*
*     Find diagonal block of matrix to work on
*
      IF( TOL.LT.ZERO .AND. ABS( D( M ) ).LE.THRESH )
     $   D( M ) = ZERO
      SMAX = ABS( D( M ) )
      SMIN = SMAX
      DO 70 LLL = 1, M - 1
         LL = M - LLL
         ABSS = ABS( D( LL ) )
         ABSE = ABS( E( LL ) )
         IF( TOL.LT.ZERO .AND. ABSS.LE.THRESH )
     $      D( LL ) = ZERO
         IF( ABSE.LE.THRESH )
     $      GO TO 80
         SMIN = MIN( SMIN, ABSS )
         SMAX = MAX( SMAX, ABSS, ABSE )
   70 CONTINUE
      LL = 0
      GO TO 90
   80 CONTINUE
      E( LL ) = ZERO
*
*     Matrix splits since E(LL) = 0
*
      IF( LL.EQ.M-1 ) THEN
*
*        Convergence of bottom singular value, return to top of loop
*
         M = M - 1
         GO TO 60
      END IF
   90 CONTINUE
      LL = LL + 1
*
*     E(LL) through E(M-1) are nonzero, E(LL-1) is zero
*
      IF( LL.EQ.M-1 ) THEN
*
*        2 by 2 block, handle separately
*
         OPS = OPS + 37 + 6*( NCVT+NRU+NCC )
         CALL DLASV2( D( M-1 ), E( M-1 ), D( M ), SIGMN, SIGMX, SINR,
     $                COSR, SINL, COSL )
         D( M-1 ) = SIGMX
         E( M-1 ) = ZERO
         D( M ) = SIGMN
*
*        Compute singular vectors, if desired
*
         IF( NCVT.GT.0 )
     $      CALL DROT( NCVT, VT( M-1, 1 ), LDVT, VT( M, 1 ), LDVT, COSR,
     $                 SINR )
         IF( NRU.GT.0 )
     $      CALL DROT( NRU, U( 1, M-1 ), 1, U( 1, M ), 1, COSL, SINL )
         IF( NCC.GT.0 )
     $      CALL DROT( NCC, C( M-1, 1 ), LDC, C( M, 1 ), LDC, COSL,
     $                 SINL )
         M = M - 2
         GO TO 60
      END IF
*
*     If working on new submatrix, choose shift direction
*     (from larger end diagonal element towards smaller)
*
      IF( LL.GT.OLDM .OR. M.LT.OLDLL ) THEN
         IF( ABS( D( LL ) ).GE.ABS( D( M ) ) ) THEN
*
*           Chase bulge from top (big end) to bottom (small end)
*
            IDIR = 1
         ELSE
*
*           Chase bulge from bottom (big end) to top (small end)
*
            IDIR = 2
         END IF
      END IF
*
*     Apply convergence tests
*
      IF( IDIR.EQ.1 ) THEN
*
*        Run convergence test in forward direction
*        First apply standard test to bottom of matrix
*
         OPS = OPS + 1
         IF( ABS( E( M-1 ) ).LE.ABS( TOL )*ABS( D( M ) ) .OR.
     $       ( TOL.LT.ZERO .AND. ABS( E( M-1 ) ).LE.THRESH ) ) THEN
            E( M-1 ) = ZERO
            GO TO 60
         END IF
*
         IF( TOL.GE.ZERO ) THEN
*
*           If relative accuracy desired,
*           apply convergence criterion forward
*
            MU = ABS( D( LL ) )
            SMINL = MU
            DO 100 LLL = LL, M - 1
               IF( ABS( E( LLL ) ).LE.TOL*MU ) THEN
                  E( LLL ) = ZERO
                  GO TO 60
               END IF
               SMINLO = SMINL
               OPS = OPS + 4
               MU = ABS( D( LLL+1 ) )*( MU / ( MU+ABS( E( LLL ) ) ) )
               SMINL = MIN( SMINL, MU )
  100       CONTINUE
         END IF
*
      ELSE
*
*        Run convergence test in backward direction
*        First apply standard test to top of matrix
*
         OPS = OPS + 1
         IF( ABS( E( LL ) ).LE.ABS( TOL )*ABS( D( LL ) ) .OR.
     $       ( TOL.LT.ZERO .AND. ABS( E( LL ) ).LE.THRESH ) ) THEN
            E( LL ) = ZERO
            GO TO 60
         END IF
*
         IF( TOL.GE.ZERO ) THEN
*
*           If relative accuracy desired,
*           apply convergence criterion backward
*
            MU = ABS( D( M ) )
            SMINL = MU
            DO 110 LLL = M - 1, LL, -1
               IF( ABS( E( LLL ) ).LE.TOL*MU ) THEN
                  E( LLL ) = ZERO
                  GO TO 60
               END IF
               SMINLO = SMINL
               OPS = OPS + 4
               MU = ABS( D( LLL ) )*( MU / ( MU+ABS( E( LLL ) ) ) )
               SMINL = MIN( SMINL, MU )
  110       CONTINUE
         END IF
      END IF
      OLDLL = LL
      OLDM = M
*
*     Compute shift.  First, test if shifting would ruin relative
*     accuracy, and if so set the shift to zero.
*
      OPS = OPS + 4
      IF( TOL.GE.ZERO .AND. N*TOL*( SMINL / SMAX ).LE.
     $    MAX( EPS, HNDRTH*TOL ) ) THEN
*
*        Use a zero shift to avoid loss of relative accuracy
*
         SHIFT = ZERO
      ELSE
*
*        Compute the shift from 2-by-2 block at end of matrix
*
         OPS = OPS + 20
         IF( IDIR.EQ.1 ) THEN
            SLL = ABS( D( LL ) )
            CALL DLAS2( D( M-1 ), E( M-1 ), D( M ), SHIFT, R )
         ELSE
            SLL = ABS( D( M ) )
            CALL DLAS2( D( LL ), E( LL ), D( LL+1 ), SHIFT, R )
         END IF
*
*        Test if shift negligible, and if so set to zero
*
         IF( SLL.GT.ZERO ) THEN
            IF( ( SHIFT / SLL )**2.LT.EPS )
     $         SHIFT = ZERO
         END IF
      END IF
*
*     Increment iteration count
*
      ITER = ITER + M - LL
*
*     If SHIFT = 0, do simplified QR iteration
*
      IF( SHIFT.EQ.ZERO ) THEN
         OPS = OPS + 2 + DBLE( M-LL )*( 20+6*( NCVT+NRU+NCC ) )
         IF( IDIR.EQ.1 ) THEN
*
*           Chase bulge from top to bottom
*           Save cosines and sines for later singular vector updates
*
            CS = ONE
            OLDCS = ONE
            DO 120 I = LL, M - 1
               CALL DLARTG( D( I )*CS, E( I ), CS, SN, R )
               IF( I.GT.LL )
     $            E( I-1 ) = OLDSN*R
               CALL DLARTG( OLDCS*R, D( I+1 )*SN, OLDCS, OLDSN, D( I ) )
               WORK( I-LL+1 ) = CS
               WORK( I-LL+1+NM1 ) = SN
               WORK( I-LL+1+NM12 ) = OLDCS
               WORK( I-LL+1+NM13 ) = OLDSN
  120       CONTINUE
            H = D( M )*CS
            D( M ) = H*OLDCS
            E( M-1 ) = H*OLDSN
*
*           Update singular vectors
*
            IF( NCVT.GT.0 )
     $         CALL DLASR( 'L', 'V', 'F', M-LL+1, NCVT, WORK( 1 ),
     $                     WORK( N ), VT( LL, 1 ), LDVT )
            IF( NRU.GT.0 )
     $         CALL DLASR( 'R', 'V', 'F', NRU, M-LL+1, WORK( NM12+1 ),
     $                     WORK( NM13+1 ), U( 1, LL ), LDU )
            IF( NCC.GT.0 )
     $         CALL DLASR( 'L', 'V', 'F', M-LL+1, NCC, WORK( NM12+1 ),
     $                     WORK( NM13+1 ), C( LL, 1 ), LDC )
*
*           Test convergence
*
            IF( ABS( E( M-1 ) ).LE.THRESH )
     $         E( M-1 ) = ZERO
*
         ELSE
*
*           Chase bulge from bottom to top
*           Save cosines and sines for later singular vector updates
*
            CS = ONE
            OLDCS = ONE
            DO 130 I = M, LL + 1, -1
               CALL DLARTG( D( I )*CS, E( I-1 ), CS, SN, R )
               IF( I.LT.M )
     $            E( I ) = OLDSN*R
               CALL DLARTG( OLDCS*R, D( I-1 )*SN, OLDCS, OLDSN, D( I ) )
               WORK( I-LL ) = CS
               WORK( I-LL+NM1 ) = -SN
               WORK( I-LL+NM12 ) = OLDCS
               WORK( I-LL+NM13 ) = -OLDSN
  130       CONTINUE
            H = D( LL )*CS
            D( LL ) = H*OLDCS
            E( LL ) = H*OLDSN
*
*           Update singular vectors
*
            IF( NCVT.GT.0 )
     $         CALL DLASR( 'L', 'V', 'B', M-LL+1, NCVT, WORK( NM12+1 ),
     $                     WORK( NM13+1 ), VT( LL, 1 ), LDVT )
            IF( NRU.GT.0 )
     $         CALL DLASR( 'R', 'V', 'B', NRU, M-LL+1, WORK( 1 ),
     $                     WORK( N ), U( 1, LL ), LDU )
            IF( NCC.GT.0 )
     $         CALL DLASR( 'L', 'V', 'B', M-LL+1, NCC, WORK( 1 ),
     $                     WORK( N ), C( LL, 1 ), LDC )
*
*           Test convergence
*
            IF( ABS( E( LL ) ).LE.THRESH )
     $         E( LL ) = ZERO
         END IF
      ELSE
*
*        Use nonzero shift
*
         OPS = OPS + 2 + ( M-LL )*( 32+6*( NCVT+NRU+NCC ) )
         IF( IDIR.EQ.1 ) THEN
*
*           Chase bulge from top to bottom
*           Save cosines and sines for later singular vector updates
*
            F = ( ABS( D( LL ) )-SHIFT )*
     $          ( SIGN( ONE, D( LL ) )+SHIFT / D( LL ) )
            G = E( LL )
            DO 140 I = LL, M - 1
               CALL DLARTG( F, G, COSR, SINR, R )
               IF( I.GT.LL )
     $            E( I-1 ) = R
               F = COSR*D( I ) + SINR*E( I )
               E( I ) = COSR*E( I ) - SINR*D( I )
               G = SINR*D( I+1 )
               D( I+1 ) = COSR*D( I+1 )
               CALL DLARTG( F, G, COSL, SINL, R )
               D( I ) = R
               F = COSL*E( I ) + SINL*D( I+1 )
               D( I+1 ) = COSL*D( I+1 ) - SINL*E( I )
               IF( I.LT.M-1 ) THEN
                  G = SINL*E( I+1 )
                  E( I+1 ) = COSL*E( I+1 )
               END IF
               WORK( I-LL+1 ) = COSR
               WORK( I-LL+1+NM1 ) = SINR
               WORK( I-LL+1+NM12 ) = COSL
               WORK( I-LL+1+NM13 ) = SINL
  140       CONTINUE
            E( M-1 ) = F
*
*           Update singular vectors
*
            IF( NCVT.GT.0 )
     $         CALL DLASR( 'L', 'V', 'F', M-LL+1, NCVT, WORK( 1 ),
     $                     WORK( N ), VT( LL, 1 ), LDVT )
            IF( NRU.GT.0 )
     $         CALL DLASR( 'R', 'V', 'F', NRU, M-LL+1, WORK( NM12+1 ),
     $                     WORK( NM13+1 ), U( 1, LL ), LDU )
            IF( NCC.GT.0 )
     $         CALL DLASR( 'L', 'V', 'F', M-LL+1, NCC, WORK( NM12+1 ),
     $                     WORK( NM13+1 ), C( LL, 1 ), LDC )
*
*           Test convergence
*
            IF( ABS( E( M-1 ) ).LE.THRESH )
     $         E( M-1 ) = ZERO
*
         ELSE
*
*           Chase bulge from bottom to top
*           Save cosines and sines for later singular vector updates
*
            F = ( ABS( D( M ) )-SHIFT )*( SIGN( ONE, D( M ) )+SHIFT /
     $          D( M ) )
            G = E( M-1 )
            DO 150 I = M, LL + 1, -1
               CALL DLARTG( F, G, COSR, SINR, R )
               IF( I.LT.M )
     $            E( I ) = R
               F = COSR*D( I ) + SINR*E( I-1 )
               E( I-1 ) = COSR*E( I-1 ) - SINR*D( I )
               G = SINR*D( I-1 )
               D( I-1 ) = COSR*D( I-1 )
               CALL DLARTG( F, G, COSL, SINL, R )
               D( I ) = R
               F = COSL*E( I-1 ) + SINL*D( I-1 )
               D( I-1 ) = COSL*D( I-1 ) - SINL*E( I-1 )
               IF( I.GT.LL+1 ) THEN
                  G = SINL*E( I-2 )
                  E( I-2 ) = COSL*E( I-2 )
               END IF
               WORK( I-LL ) = COSR
               WORK( I-LL+NM1 ) = -SINR
               WORK( I-LL+NM12 ) = COSL
               WORK( I-LL+NM13 ) = -SINL
  150       CONTINUE
            E( LL ) = F
*
*           Test convergence
*
            IF( ABS( E( LL ) ).LE.THRESH )
     $         E( LL ) = ZERO
*
*           Update singular vectors if desired
*
            IF( NCVT.GT.0 )
     $         CALL DLASR( 'L', 'V', 'B', M-LL+1, NCVT, WORK( NM12+1 ),
     $                     WORK( NM13+1 ), VT( LL, 1 ), LDVT )
            IF( NRU.GT.0 )
     $         CALL DLASR( 'R', 'V', 'B', NRU, M-LL+1, WORK( 1 ),
     $                     WORK( N ), U( 1, LL ), LDU )
            IF( NCC.GT.0 )
     $         CALL DLASR( 'L', 'V', 'B', M-LL+1, NCC, WORK( 1 ),
     $                     WORK( N ), C( LL, 1 ), LDC )
         END IF
      END IF
*
*     QR iteration finished, go back and check convergence
*
      GO TO 60
*
*     All singular values converged, so make them positive
*
  160 CONTINUE
      DO 170 I = 1, N
         IF( D( I ).LT.ZERO ) THEN
            D( I ) = -D( I )
*
*           Change sign of singular vectors, if desired
*
            OPS = OPS + NCVT
            IF( NCVT.GT.0 )
     $         CALL DSCAL( NCVT, NEGONE, VT( I, 1 ), LDVT )
         END IF
  170 CONTINUE
*
*     Sort the singular values into decreasing order (insertion sort on
*     singular values, but only one transposition per singular vector)
*
      DO 190 I = 1, N - 1
*
*        Scan for smallest D(I)
*
         ISUB = 1
         SMIN = D( 1 )
         DO 180 J = 2, N + 1 - I
            IF( D( J ).LE.SMIN ) THEN
               ISUB = J
               SMIN = D( J )
            END IF
  180    CONTINUE
         IF( ISUB.NE.N+1-I ) THEN
*
*           Swap singular values and vectors
*
            D( ISUB ) = D( N+1-I )
            D( N+1-I ) = SMIN
            IF( NCVT.GT.0 )
     $         CALL DSWAP( NCVT, VT( ISUB, 1 ), LDVT, VT( N+1-I, 1 ),
     $                     LDVT )
            IF( NRU.GT.0 )
     $         CALL DSWAP( NRU, U( 1, ISUB ), 1, U( 1, N+1-I ), 1 )
            IF( NCC.GT.0 )
     $         CALL DSWAP( NCC, C( ISUB, 1 ), LDC, C( N+1-I, 1 ), LDC )
         END IF
  190 CONTINUE
      GO TO 220
*
*     Maximum number of iterations exceeded, failure to converge
*
  200 CONTINUE
      INFO = 0
      DO 210 I = 1, N - 1
         IF( E( I ).NE.ZERO )
     $      INFO = INFO + 1
  210 CONTINUE
  220 CONTINUE
      RETURN
*
*     End of DBDSQR
*
      END
      SUBROUTINE DGESDD( JOBZ, M, N, A, LDA, S, U, LDU, VT, LDVT, WORK,
     $                   LWORK, IWORK, INFO )
*
*  -- LAPACK driver routine (instrumented to count ops, version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     October 31, 1999
*
*     .. Scalar Arguments ..
      CHARACTER          JOBZ
      INTEGER            INFO, LDA, LDU, LDVT, LWORK, M, N
*     ..
*     .. Array Arguments ..
      INTEGER            IWORK( * )
      DOUBLE PRECISION   A( LDA, * ), S( * ), U( LDU, * ),
     $                   VT( LDVT, * ), WORK( * )
*     ..
*     .. Common block to return operation count ..
      COMMON             / LATIME / OPS, ITCNT
*     ..
*     .. Scalars in Common ..
      DOUBLE PRECISION   ITCNT, OPS
*     ..
*
*  Purpose
*  =======
*
*  DGESDD computes the singular value decomposition (SVD) of a real
*  M-by-N matrix A, optionally computing the left and right singular
*  vectors.  If singular vectors are desired, it uses a
*  divide-and-conquer algorithm.
*
*  The SVD is written
*
*       A = U * SIGMA * transpose(V)
*
*  where SIGMA is an M-by-N matrix which is zero except for its
*  min(m,n) diagonal elements, U is an M-by-M orthogonal matrix, and
*  V is an N-by-N orthogonal matrix.  The diagonal elements of SIGMA
*  are the singular values of A; they are real and non-negative, and
*  are returned in descending order.  The first min(m,n) columns of
*  U and V are the left and right singular vectors of A.
*
*  Note that the routine returns VT = V**T, not V.
*
*  The divide and conquer algorithm makes very mild assumptions about
*  floating point arithmetic. It will work on machines with a guard
*  digit in add/subtract, or on those binary machines without guard
*  digits which subtract like the Cray X-MP, Cray Y-MP, Cray C-90, or
*  Cray-2. It could conceivably fail on hexadecimal or decimal machines
*  without guard digits, but we know of none.
*
*  Arguments
*  =========
*
*  JOBZ    (input) CHARACTER*1
*          Specifies options for computing all or part of the matrix U:
*          = 'A':  all M columns of U and all N rows of V**T are
*                  returned in the arrays U and VT;
*          = 'S':  the first min(M,N) columns of U and the first
*                  min(M,N) rows of V**T are returned in the arrays U
*                  and VT;
*          = 'O':  If M >= N, the first N columns of U are overwritten
*                  on the array A and all rows of V**T are returned in
*                  the array VT;
*                  otherwise, all columns of U are returned in the
*                  array U and the first M rows of V**T are overwritten
*                  in the array VT;
*          = 'N':  no columns of U or rows of V**T are computed.
*
*  M       (input) INTEGER
*          The number of rows of the input matrix A.  M >= 0.
*
*  N       (input) INTEGER
*          The number of columns of the input matrix A.  N >= 0.
*
*  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
*          On entry, the M-by-N matrix A.
*          On exit,
*          if JOBZ = 'O',  A is overwritten with the first N columns
*                          of U (the left singular vectors, stored
*                          columnwise) if M >= N;
*                          A is overwritten with the first M rows
*                          of V**T (the right singular vectors, stored
*                          rowwise) otherwise.
*          if JOBZ .ne. 'O', the contents of A are destroyed.
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A.  LDA >= max(1,M).
*
*  S       (output) DOUBLE PRECISION array, dimension (min(M,N))
*          The singular values of A, sorted so that S(i) >= S(i+1).
*
*  U       (output) DOUBLE PRECISION array, dimension (LDU,UCOL)
*          UCOL = M if JOBZ = 'A' or JOBZ = 'O' and M < N;
*          UCOL = min(M,N) if JOBZ = 'S'.
*          If JOBZ = 'A' or JOBZ = 'O' and M < N, U contains the M-by-M
*          orthogonal matrix U;
*          if JOBZ = 'S', U contains the first min(M,N) columns of U
*          (the left singular vectors, stored columnwise);
*          if JOBZ = 'O' and M >= N, or JOBZ = 'N', U is not referenced.
*
*  LDU     (input) INTEGER
*          The leading dimension of the array U.  LDU >= 1; if
*          JOBZ = 'S' or 'A' or JOBZ = 'O' and M < N, LDU >= M.
*
*  VT      (output) DOUBLE PRECISION array, dimension (LDVT,N)
*          If JOBZ = 'A' or JOBZ = 'O' and M >= N, VT contains the
*          N-by-N orthogonal matrix V**T;
*          if JOBZ = 'S', VT contains the first min(M,N) rows of
*          V**T (the right singular vectors, stored rowwise);
*          if JOBZ = 'O' and M < N, or JOBZ = 'N', VT is not referenced.
*
*  LDVT    (input) INTEGER
*          The leading dimension of the array VT.  LDVT >= 1; if
*          JOBZ = 'A' or JOBZ = 'O' and M >= N, LDVT >= N;
*          if JOBZ = 'S', LDVT >= min(M,N).
*
*  WORK    (workspace/output) DOUBLE PRECISION array, dimension (LWORK)
*          On exit, if INFO = 0, WORK(1) returns the optimal LWORK;
*
*  LWORK   (input) INTEGER
*          The dimension of the array WORK. LWORK >= 1.
*          If JOBZ = 'N',
*            LWORK >= 3*min(M,N) + max(max(M,N),6*min(M,N)).
*          If JOBZ = 'O',
*            LWORK >= 3*min(M,N)*min(M,N) + 
*                     max(max(M,N),5*min(M,N)*min(M,N)+4*min(M,N)).
*          If JOBZ = 'S' or 'A'
*            LWORK >= 3*min(M,N)*min(M,N) +
*                     max(max(M,N),4*min(M,N)*min(M,N)+4*min(M,N)).
*          For good performance, LWORK should generally be larger.
*          If LWORK < 0 but other input arguments are legal, WORK(1)
*          returns the optimal LWORK.
*
*  IWORK   (workspace) INTEGER array, dimension (8*min(M,N))
*
*  INFO    (output) INTEGER
*          = 0:  successful exit.
*          < 0:  if INFO = -i, the i-th argument had an illegal value.
*          > 0:  DBDSDC did not converge, updating process failed.
*
*  Further Details
*  ===============
*
*  Based on contributions by
*     Ming Gu and Huan Ren, Computer Science Division, University of
*     California at Berkeley, USA
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE
      PARAMETER          ( ZERO = 0.0D0, ONE = 1.0D0 )
*     ..
*     .. Local Scalars ..
      LOGICAL            LQUERY, WNTQA, WNTQAS, WNTQN, WNTQO, WNTQS
      INTEGER            BDSPAC, BLK, CHUNK, I, IE, IERR, IL,
     $                   IR, ISCL, ITAU, ITAUP, ITAUQ, IU, IVT, LDWKVT,
     $                   LDWRKL, LDWRKR, LDWRKU, MAXWRK, MINMN, MINWRK,
     $                   MNTHR, NB, NWORK, WRKBL
      DOUBLE PRECISION   ANRM, BIGNUM, EPS, SMLNUM
*     ..
*     .. Local Arrays ..
      INTEGER            IDUM( 1 )
      DOUBLE PRECISION   DUM( 1 )
*     ..
*     .. External Subroutines ..
      EXTERNAL           DBDSDC, DGEBRD, DGELQF, DGEMM, DGEQRF, DLACPY,
     $                   DLASCL, DLASET, DORGBR, DORGLQ, DORGQR, DORMBR,
     $                   XERBLA
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      INTEGER            ILAENV
      DOUBLE PRECISION   DLAMCH, DLANGE, DOPBL3, DOPLA, DOPLA2
      EXTERNAL           DLAMCH, DLANGE, DOPBL3, DOPLA, DOPLA2, ILAENV, 
     $                   LSAME
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          DBLE, INT, MAX, MIN, SQRT
*     ..
*     .. Executable Statements ..
*
*     Test the input arguments
*
      INFO = 0
      MINMN = MIN( M, N )
      MNTHR = INT( MINMN*11.0D0 / 6.0D0 )
      WNTQA = LSAME( JOBZ, 'A' )
      WNTQS = LSAME( JOBZ, 'S' )
      WNTQAS = WNTQA .OR. WNTQS
      WNTQO = LSAME( JOBZ, 'O' )
      WNTQN = LSAME( JOBZ, 'N' )
      MINWRK = 1
      MAXWRK = 1
      LQUERY = ( LWORK.EQ.-1 )
*
      IF( .NOT.( WNTQA .OR. WNTQS .OR. WNTQO .OR. WNTQN ) ) THEN
         INFO = -1
      ELSE IF( M.LT.0 ) THEN
         INFO = -2
      ELSE IF( N.LT.0 ) THEN
         INFO = -3
      ELSE IF( LDA.LT.MAX( 1, M ) ) THEN
         INFO = -5
      ELSE IF( LDU.LT.1 .OR. ( WNTQAS .AND. LDU.LT.M ) .OR.
     $         ( WNTQO .AND. M.LT.N .AND. LDU.LT.M ) ) THEN
         INFO = -8
      ELSE IF( LDVT.LT.1 .OR. ( WNTQA .AND. LDVT.LT.N ) .OR.
     $         ( WNTQS .AND. LDVT.LT.MINMN ) .OR.
     $         ( WNTQO .AND. M.GE.N .AND. LDVT.LT.N ) ) THEN
         INFO = -10
      END IF
*
*     Compute workspace
*      (Note: Comments in the code beginning "Workspace:" describe the
*       minimal amount of workspace needed at that point in the code,
*       as well as the preferred amount for good performance.
*       NB refers to the optimal block size for the immediately
*       following subroutine, as returned by ILAENV.)
*
      IF( INFO.EQ.0 .AND. M.GT.0 .AND. N.GT.0 ) THEN
         IF( M.GE.N ) THEN
*
*           Compute space needed for DBDSDC
*
            IF( WNTQN ) THEN
               BDSPAC = 7*N
            ELSE
               BDSPAC = 3*N*N + 4*N
            END IF
            IF( M.GE.MNTHR ) THEN
               IF( WNTQN ) THEN
*
*                 Path 1 (M much larger than N, JOBZ='N')
*
                  WRKBL = N + N*ILAENV( 1, 'DGEQRF', ' ', M, N, -1,
     $                    -1 )
                  WRKBL = MAX( WRKBL, 3*N+2*N*
     $                    ILAENV( 1, 'DGEBRD', ' ', N, N, -1, -1 ) )
                  MAXWRK = MAX( WRKBL, BDSPAC+N )
                  MINWRK = BDSPAC + N
               ELSE IF( WNTQO ) THEN
*
*                 Path 2 (M much larger than N, JOBZ='O')
*
                  WRKBL = N + N*ILAENV( 1, 'DGEQRF', ' ', M, N, -1, -1 )
                  WRKBL = MAX( WRKBL, N+N*ILAENV( 1, 'DORGQR', ' ', M,
     $                    N, N, -1 ) )
                  WRKBL = MAX( WRKBL, 3*N+2*N*
     $                    ILAENV( 1, 'DGEBRD', ' ', N, N, -1, -1 ) )
                  WRKBL = MAX( WRKBL, 3*N+N*
     $                    ILAENV( 1, 'DORMBR', 'QLN', N, N, N, -1 ) )
                  WRKBL = MAX( WRKBL, 3*N+N*
     $                    ILAENV( 1, 'DORMBR', 'PRT', N, N, N, -1 ) )
                  WRKBL = MAX( WRKBL, BDSPAC+3*N )
                  MAXWRK = WRKBL + 2*N*N
                  MINWRK = BDSPAC + 2*N*N + 3*N
               ELSE IF( WNTQS ) THEN
*
*                 Path 3 (M much larger than N, JOBZ='S')
*
                  WRKBL = N + N*ILAENV( 1, 'DGEQRF', ' ', M, N, -1, -1 )
                  WRKBL = MAX( WRKBL, N+N*ILAENV( 1, 'DORGQR', ' ', M,
     $                    N, N, -1 ) )
                  WRKBL = MAX( WRKBL, 3*N+2*N*
     $                    ILAENV( 1, 'DGEBRD', ' ', N, N, -1, -1 ) )
                  WRKBL = MAX( WRKBL, 3*N+N*
     $                    ILAENV( 1, 'DORMBR', 'QLN', N, N, N, -1 ) )
                  WRKBL = MAX( WRKBL, 3*N+N*
     $                    ILAENV( 1, 'DORMBR', 'PRT', N, N, N, -1 ) )
                  WRKBL = MAX( WRKBL, BDSPAC+3*N )
                  MAXWRK = WRKBL + N*N
                  MINWRK = BDSPAC + N*N + 3*N
               ELSE IF( WNTQA ) THEN
*
*                 Path 4 (M much larger than N, JOBZ='A')
*
                  WRKBL = N + N*ILAENV( 1, 'DGEQRF', ' ', M, N, -1, -1 )
                  WRKBL = MAX( WRKBL, N+M*ILAENV( 1, 'DORGQR', ' ', M,
     $                    M, N, -1 ) )
                  WRKBL = MAX( WRKBL, 3*N+2*N*
     $                    ILAENV( 1, 'DGEBRD', ' ', N, N, -1, -1 ) )
                  WRKBL = MAX( WRKBL, 3*N+N*
     $                    ILAENV( 1, 'DORMBR', 'QLN', N, N, N, -1 ) )
                  WRKBL = MAX( WRKBL, 3*N+N*
     $                    ILAENV( 1, 'DORMBR', 'PRT', N, N, N, -1 ) )
                  WRKBL = MAX( WRKBL, BDSPAC+3*N )
                  MAXWRK = WRKBL + N*N
                  MINWRK = BDSPAC + N*N + 3*N
               END IF
            ELSE
*
*              Path 5 (M at least N, but not much larger)
*
               WRKBL = 3*N + ( M+N )*ILAENV( 1, 'DGEBRD', ' ', M, N, -1,
     $                 -1 )
               IF( WNTQN ) THEN
                  MAXWRK = MAX( WRKBL, BDSPAC+3*N )
                  MINWRK = 3*N + MAX( M, BDSPAC )
               ELSE IF( WNTQO ) THEN
                  WRKBL = MAX( WRKBL, 3*N+N*
     $                    ILAENV( 1, 'DORMBR', 'QLN', M, N, N, -1 ) )
                  WRKBL = MAX( WRKBL, 3*N+N*
     $                    ILAENV( 1, 'DORMBR', 'PRT', N, N, N, -1 ) )
                  WRKBL = MAX( WRKBL, BDSPAC+3*N )
                  MAXWRK = WRKBL + M*N
                  MINWRK = 3*N + MAX( M, N*N+BDSPAC )
               ELSE IF( WNTQS ) THEN
                  WRKBL = MAX( WRKBL, 3*N+N*
     $                    ILAENV( 1, 'DORMBR', 'QLN', M, N, N, -1 ) )
                  WRKBL = MAX( WRKBL, 3*N+N*
     $                    ILAENV( 1, 'DORMBR', 'PRT', N, N, N, -1 ) )
                  MAXWRK = MAX( WRKBL, BDSPAC+3*N )
                  MINWRK = 3*N + MAX( M, BDSPAC )
               ELSE IF( WNTQA ) THEN
                  WRKBL = MAX( WRKBL, 3*N+M*
     $                    ILAENV( 1, 'DORMBR', 'QLN', M, M, N, -1 ) )
                  WRKBL = MAX( WRKBL, 3*N+N*
     $                    ILAENV( 1, 'DORMBR', 'PRT', N, N, N, -1 ) )
                  MAXWRK = MAX( MAXWRK, BDSPAC+3*N )
                  MINWRK = 3*N + MAX( M, BDSPAC )
               END IF
            END IF
         ELSE
*
*           Compute space needed for DBDSDC
*
            IF( WNTQN ) THEN
               BDSPAC = 7*M
            ELSE
               BDSPAC = 3*M*M + 4*M
            END IF
            IF( N.GE.MNTHR ) THEN
               IF( WNTQN ) THEN
*
*                 Path 1t (N much larger than M, JOBZ='N')
*
                  WRKBL = M + M*ILAENV( 1, 'DGELQF', ' ', M, N, -1,
     $                    -1 )
                  WRKBL = MAX( WRKBL, 3*M+2*M*
     $                    ILAENV( 1, 'DGEBRD', ' ', M, M, -1, -1 ) )
                  MAXWRK = MAX( WRKBL, BDSPAC+M )
                  MINWRK = BDSPAC + M
               ELSE IF( WNTQO ) THEN
*
*                 Path 2t (N much larger than M, JOBZ='O')
*
                  WRKBL = M + M*ILAENV( 1, 'DGELQF', ' ', M, N, -1, -1 )
                  WRKBL = MAX( WRKBL, M+M*ILAENV( 1, 'DORGLQ', ' ', M,
     $                    N, M, -1 ) )
                  WRKBL = MAX( WRKBL, 3*M+2*M*
     $                    ILAENV( 1, 'DGEBRD', ' ', M, M, -1, -1 ) )
                  WRKBL = MAX( WRKBL, 3*M+M*
     $                    ILAENV( 1, 'DORMBR', 'QLN', M, M, M, -1 ) )
                  WRKBL = MAX( WRKBL, 3*M+M*
     $                    ILAENV( 1, 'DORMBR', 'PRT', M, M, M, -1 ) )
                  WRKBL = MAX( WRKBL, BDSPAC+3*M )
                  MAXWRK = WRKBL + 2*M*M
                  MINWRK = BDSPAC + 2*M*M + 3*M
               ELSE IF( WNTQS ) THEN
*
*                 Path 3t (N much larger than M, JOBZ='S')
*
                  WRKBL = M + M*ILAENV( 1, 'DGELQF', ' ', M, N, -1, -1 )
                  WRKBL = MAX( WRKBL, M+M*ILAENV( 1, 'DORGLQ', ' ', M,
     $                    N, M, -1 ) )
                  WRKBL = MAX( WRKBL, 3*M+2*M*
     $                    ILAENV( 1, 'DGEBRD', ' ', M, M, -1, -1 ) )
                  WRKBL = MAX( WRKBL, 3*M+M*
     $                    ILAENV( 1, 'DORMBR', 'QLN', M, M, M, -1 ) )
                  WRKBL = MAX( WRKBL, 3*M+M*
     $                    ILAENV( 1, 'DORMBR', 'PRT', M, M, M, -1 ) )
                  WRKBL = MAX( WRKBL, BDSPAC+3*M )
                  MAXWRK = WRKBL + M*M
                  MINWRK = BDSPAC + M*M + 3*M
               ELSE IF( WNTQA ) THEN
*
*                 Path 4t (N much larger than M, JOBZ='A')
*
                  WRKBL = M + M*ILAENV( 1, 'DGELQF', ' ', M, N, -1, -1 )
                  WRKBL = MAX( WRKBL, M+N*ILAENV( 1, 'DORGLQ', ' ', N,
     $                    N, M, -1 ) )
                  WRKBL = MAX( WRKBL, 3*M+2*M*
     $                    ILAENV( 1, 'DGEBRD', ' ', M, M, -1, -1 ) )
                  WRKBL = MAX( WRKBL, 3*M+M*
     $                    ILAENV( 1, 'DORMBR', 'QLN', M, M, M, -1 ) )
                  WRKBL = MAX( WRKBL, 3*M+M*
     $                    ILAENV( 1, 'DORMBR', 'PRT', M, M, M, -1 ) )
                  WRKBL = MAX( WRKBL, BDSPAC+3*M )
                  MAXWRK = WRKBL + M*M
                  MINWRK = BDSPAC + M*M + 3*M
               END IF
            ELSE
*
*              Path 5t (N greater than M, but not much larger)
*
               WRKBL = 3*M + ( M+N )*ILAENV( 1, 'DGEBRD', ' ', M, N, -1,
     $                 -1 )
               IF( WNTQN ) THEN
                  MAXWRK = MAX( WRKBL, BDSPAC+3*M )
                  MINWRK = 3*M + MAX( N, BDSPAC )
               ELSE IF( WNTQO ) THEN
                  WRKBL = MAX( WRKBL, 3*M+M*
     $                    ILAENV( 1, 'DORMBR', 'QLN', M, M, N, -1 ) )
                  WRKBL = MAX( WRKBL, 3*M+M*
     $                    ILAENV( 1, 'DORMBR', 'PRT', M, N, M, -1 ) )
                  WRKBL = MAX( WRKBL, BDSPAC+3*M )
                  MAXWRK = WRKBL + M*N
                  MINWRK = 3*M + MAX( N, M*M+BDSPAC )
               ELSE IF( WNTQS ) THEN
                  WRKBL = MAX( WRKBL, 3*M+M*
     $                    ILAENV( 1, 'DORMBR', 'QLN', M, M, N, -1 ) )
                  WRKBL = MAX( WRKBL, 3*M+M*
     $                    ILAENV( 1, 'DORMBR', 'PRT', M, N, M, -1 ) )
                  MAXWRK = MAX( WRKBL, BDSPAC+3*M )
                  MINWRK = 3*M + MAX( N, BDSPAC )
               ELSE IF( WNTQA ) THEN
                  WRKBL = MAX( WRKBL, 3*M+M*
     $                    ILAENV( 1, 'DORMBR', 'QLN', M, M, N, -1 ) )
                  WRKBL = MAX( WRKBL, 3*M+M*
     $                    ILAENV( 1, 'DORMBR', 'PRT', N, N, M, -1 ) )
                  MAXWRK = MAX( WRKBL, BDSPAC+3*M )
                  MINWRK = 3*M + MAX( N, BDSPAC )
               END IF
            END IF
         END IF
         WORK( 1 ) = MAXWRK
      END IF
*
      IF( LWORK.LT.MINWRK .AND. .NOT.LQUERY ) THEN
         INFO = -12
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DGESDD', -INFO )
         RETURN
      ELSE IF( LQUERY ) THEN
         RETURN
      END IF
*
*     Quick return if possible
*
      IF( M.EQ.0 .OR. N.EQ.0 ) THEN
         IF( LWORK.GE.1 )
     $      WORK( 1 ) = ONE
         RETURN
      END IF
*
*     Get machine constants
*
      EPS = DLAMCH( 'P' )
      SMLNUM = SQRT( DLAMCH( 'S' ) ) / EPS
      BIGNUM = ONE / SMLNUM
*
*     Scale A if max element outside range [SMLNUM,BIGNUM]
*
      ANRM = DLANGE( 'M', M, N, A, LDA, DUM )
      ISCL = 0
      IF( ANRM.GT.ZERO .AND. ANRM.LT.SMLNUM ) THEN
         ISCL = 1
         OPS = OPS + DBLE( M*N )
         CALL DLASCL( 'G', 0, 0, ANRM, SMLNUM, M, N, A, LDA, IERR )
      ELSE IF( ANRM.GT.BIGNUM ) THEN
         ISCL = 1
         OPS = OPS + DBLE( M*N )
         CALL DLASCL( 'G', 0, 0, ANRM, BIGNUM, M, N, A, LDA, IERR )
      END IF
*
      IF( M.GE.N ) THEN
*
*        A has at least as many rows as columns. If A has sufficiently
*        more rows than columns, first reduce using the QR
*        decomposition (if sufficient workspace available)
*
         IF( M.GE.MNTHR ) THEN
*
            IF( WNTQN ) THEN
*
*              Path 1 (M much larger than N, JOBZ='N')
*              No singular vectors to be computed
*
               ITAU = 1
               NWORK = ITAU + N
*
*              Compute A=Q*R
*              (Workspace: need 2*N, prefer N+N*NB)
*
               NB = ILAENV( 1, 'DGEQRF', ' ', M, N, -1, -1 )
               OPS = OPS + DOPLA( 'DGEQRF', M, N, 0, 0, NB )
               CALL DGEQRF( M, N, A, LDA, WORK( ITAU ), WORK( NWORK ),
     $                      LWORK-NWORK+1, IERR )
*
*              Zero out below R
*
               CALL DLASET( 'L', N-1, N-1, ZERO, ZERO, A( 2, 1 ), LDA )
               IE = 1
               ITAUQ = IE + N
               ITAUP = ITAUQ + N
               NWORK = ITAUP + N
*
*              Bidiagonalize R in A
*              (Workspace: need 4*N, prefer 3*N+2*N*NB)
*
               NB = ILAENV( 1, 'DGEBRD', ' ', N, N, -1, -1 )
               OPS = OPS + DOPLA( 'DGEBRD', N, N, 0, 0, NB )
               CALL DGEBRD( N, N, A, LDA, S, WORK( IE ), WORK( ITAUQ ),
     $                      WORK( ITAUP ), WORK( NWORK ), LWORK-NWORK+1,
     $                      IERR )
               NWORK = IE + N
*
*              Perform bidiagonal SVD, computing singular values only
*              (Workspace: need N+BDSPAC)
*
               CALL DBDSDC( 'U', 'N', N, S, WORK( IE ), DUM, 1, DUM, 1,
     $                      DUM, IDUM, WORK( NWORK ), IWORK, INFO )
*
            ELSE IF( WNTQO ) THEN
*
*              Path 2 (M much larger than N, JOBZ = 'O')
*              N left singular vectors to be overwritten on A and
*              N right singular vectors to be computed in VT
*
               IR = 1
*
*              WORK(IR) is LDWRKR by N
*
               IF( LWORK.GE.LDA*N+N*N+3*N+BDSPAC ) THEN
                  LDWRKR = LDA
               ELSE
                  LDWRKR = ( LWORK-N*N-3*N-BDSPAC ) / N
               END IF
               ITAU = IR + LDWRKR*N
               NWORK = ITAU + N
*
*              Compute A=Q*R
*              (Workspace: need N*N+2*N, prefer N*N+N+N*NB)
*
               NB = ILAENV( 1, 'DGEQRF', ' ', M, N, -1, -1 )
               OPS = OPS + DOPLA( 'DGEQRF', M, N, 0, 0, NB )
               CALL DGEQRF( M, N, A, LDA, WORK( ITAU ), WORK( NWORK ),
     $                      LWORK-NWORK+1, IERR )
*
*              Copy R to WORK(IR), zeroing out below it
*
               CALL DLACPY( 'U', N, N, A, LDA, WORK( IR ), LDWRKR )
               CALL DLASET( 'L', N-1, N-1, ZERO, ZERO, WORK( IR+1 ),
     $                      LDWRKR )
*
*              Generate Q in A
*              (Workspace: need N*N+2*N, prefer N*N+N+N*NB)
*
               NB = ILAENV( 1, 'DORGQR', ' ', M, N, N, -1 )
               OPS = OPS + DOPLA( 'DORGQR', M, N, N, 0, NB )
               CALL DORGQR( M, N, N, A, LDA, WORK( ITAU ),
     $                      WORK( NWORK ), LWORK-NWORK+1, IERR )
               IE = ITAU
               ITAUQ = IE + N
               ITAUP = ITAUQ + N
               NWORK = ITAUP + N
*
*              Bidiagonalize R in VT, copying result to WORK(IR)
*              (Workspace: need N*N+4*N, prefer N*N+3*N+2*N*NB)
*
               NB = ILAENV( 1, 'DGEBRD', ' ', N, N, -1, -1 )
               OPS = OPS + DOPLA( 'DGEBRD', N, N, 0, 0, NB )
               CALL DGEBRD( N, N, WORK( IR ), LDWRKR, S, WORK( IE ),
     $                      WORK( ITAUQ ), WORK( ITAUP ), WORK( NWORK ),
     $                      LWORK-NWORK+1, IERR )
*
*              WORK(IU) is N by N
*
               IU = NWORK
               NWORK = IU + N*N
*
*              Perform bidiagonal SVD, computing left singular vectors
*              of bidiagonal matrix in WORK(IU) and computing right
*              singular vectors of bidiagonal matrix in VT
*              (Workspace: need N+N*N+BDSPAC)
*
               CALL DBDSDC( 'U', 'I', N, S, WORK( IE ), WORK( IU ), N,
     $                      VT, LDVT, DUM, IDUM, WORK( NWORK ), IWORK,
     $                      INFO )
*
*              Overwrite WORK(IU) by left singular vectors of R
*              and VT by right singular vectors of R
*              (Workspace: need 2*N*N+3*N, prefer 2*N*N+2*N+N*NB)
*
               NB = ILAENV( 1, 'DORMBR', 'QLN', N, N, N, -1 )
               OPS = OPS + DOPLA2( 'DORMBR', 'QLN', N, N, N, 0, NB )
               CALL DORMBR( 'Q', 'L', 'N', N, N, N, WORK( IR ), LDWRKR,
     $                      WORK( ITAUQ ), WORK( IU ), N, WORK( NWORK ),
     $                      LWORK-NWORK+1, IERR )
               NB = ILAENV( 1, 'DORMBR', 'PRT', N, N, N, -1 )
               OPS = OPS + DOPLA2( 'DORMBR', 'PRT', N, N, N, 0, NB )
               CALL DORMBR( 'P', 'R', 'T', N, N, N, WORK( IR ), LDWRKR,
     $                      WORK( ITAUP ), VT, LDVT, WORK( NWORK ),
     $                      LWORK-NWORK+1, IERR )
*
*              Multiply Q in A by left singular vectors of R in
*              WORK(IU), storing result in WORK(IR) and copying to A
*              (Workspace: need 2*N*N, prefer N*N+M*N)
*
               DO 10 I = 1, M, LDWRKR
                  CHUNK = MIN( M-I+1, LDWRKR )
                  OPS = OPS + DOPBL3( 'DGEMM ', CHUNK, N, N )
                  CALL DGEMM( 'N', 'N', CHUNK, N, N, ONE, A( I, 1 ),
     $                        LDA, WORK( IU ), N, ZERO, WORK( IR ),
     $                        LDWRKR )
                  CALL DLACPY( 'F', CHUNK, N, WORK( IR ), LDWRKR,
     $                         A( I, 1 ), LDA )
   10          CONTINUE
*
            ELSE IF( WNTQS ) THEN
*
*              Path 3 (M much larger than N, JOBZ='S')
*              N left singular vectors to be computed in U and
*              N right singular vectors to be computed in VT
*
               IR = 1
*
*              WORK(IR) is N by N
*
               LDWRKR = N
               ITAU = IR + LDWRKR*N
               NWORK = ITAU + N
*
*              Compute A=Q*R
*              (Workspace: need N*N+2*N, prefer N*N+N+N*NB)
*
               NB = ILAENV( 1, 'DGEQRF', ' ', M, N, -1, -1 )
               OPS = OPS + DOPLA( 'DGEQRF', M, N, 0, 0, NB )
               CALL DGEQRF( M, N, A, LDA, WORK( ITAU ), WORK( NWORK ),
     $                      LWORK-NWORK+1, IERR )
*
*              Copy R to WORK(IR), zeroing out below it
*
               CALL DLACPY( 'U', N, N, A, LDA, WORK( IR ), LDWRKR )
               CALL DLASET( 'L', N-1, N-1, ZERO, ZERO, WORK( IR+1 ),
     $                      LDWRKR )
*
*              Generate Q in A
*              (Workspace: need N*N+2*N, prefer N*N+N+N*NB)
*
               NB = ILAENV( 1, 'DORGQR', ' ', M, N, N, -1 )
               OPS = OPS + DOPLA( 'DORGQR', M, N, N, 0, NB )
               CALL DORGQR( M, N, N, A, LDA, WORK( ITAU ),
     $                      WORK( NWORK ), LWORK-NWORK+1, IERR )
               IE = ITAU
               ITAUQ = IE + N
               ITAUP = ITAUQ + N
               NWORK = ITAUP + N
*
*              Bidiagonalize R in WORK(IR)
*              (Workspace: need N*N+4*N, prefer N*N+3*N+2*N*NB)
*
               NB = MAX( 1, ILAENV( 1, 'DGEBRD', ' ', N, N, -1, -1 ) )
               OPS = OPS + DOPLA( 'DGEBRD', N, N, 0, 0, NB )
               CALL DGEBRD( N, N, WORK( IR ), LDWRKR, S, WORK( IE ),
     $                      WORK( ITAUQ ), WORK( ITAUP ), WORK( NWORK ),
     $                      LWORK-NWORK+1, IERR )
*
*              Perform bidiagonal SVD, computing left singular vectors
*              of bidiagoal matrix in U and computing right singular
*              vectors of bidiagonal matrix in VT
*              (Workspace: need N+BDSPAC)
*
               CALL DBDSDC( 'U', 'I', N, S, WORK( IE ), U, LDU, VT,
     $                      LDVT, DUM, IDUM, WORK( NWORK ), IWORK,
     $                      INFO )
*
*              Overwrite U by left singular vectors of R and VT
*              by right singular vectors of R
*              (Workspace: need N*N+3*N, prefer N*N+2*N+N*NB)
*
               NB = ILAENV( 1, 'DORMBR', 'QLN', N, N, N, -1 )
               OPS = OPS + DOPLA2( 'DORMBR', 'QLN', N, N, N, 0, NB )
               CALL DORMBR( 'Q', 'L', 'N', N, N, N, WORK( IR ), LDWRKR,
     $                      WORK( ITAUQ ), U, LDU, WORK( NWORK ),
     $                      LWORK-NWORK+1, IERR )
*
               NB = ILAENV( 1, 'DORMBR', 'PRT', N, N, N, -1 )
               OPS = OPS + DOPLA2( 'DORMBR', 'PRT', N, N, N, 0, NB )
               CALL DORMBR( 'P', 'R', 'T', N, N, N, WORK( IR ), LDWRKR,
     $                      WORK( ITAUP ), VT, LDVT, WORK( NWORK ),
     $                      LWORK-NWORK+1, IERR )
*
*              Multiply Q in A by left singular vectors of R in
*              WORK(IR), storing result in U
*              (Workspace: need N*N)
*
               CALL DLACPY( 'F', N, N, U, LDU, WORK( IR ), LDWRKR )
               OPS = OPS + DOPBL3( 'DGEMM ', M, N, N )
               CALL DGEMM( 'N', 'N', M, N, N, ONE, A, LDA, WORK( IR ),
     $                     LDWRKR, ZERO, U, LDU )
*
            ELSE IF( WNTQA ) THEN
*
*              Path 4 (M much larger than N, JOBZ='A')
*              M left singular vectors to be computed in U and
*              N right singular vectors to be computed in VT
*
               IU = 1
*
*              WORK(IU) is N by N
*
               LDWRKU = N
               ITAU = IU + LDWRKU*N
               NWORK = ITAU + N
*
*              Compute A=Q*R, copying result to U
*              (Workspace: need N*N+2*N, prefer N*N+N+N*NB)
*
               NB = ILAENV( 1, 'DGEQRF', ' ', M, N, -1, -1 )
               OPS = OPS + DOPLA( 'DGEQRF', M, N, 0, 0, NB )
               CALL DGEQRF( M, N, A, LDA, WORK( ITAU ), WORK( NWORK ),
     $                      LWORK-NWORK+1, IERR )
               CALL DLACPY( 'L', M, N, A, LDA, U, LDU )
*
*              Generate Q in U
*              (Workspace: need N*N+2*N, prefer N*N+N+N*NB)
               NB = ILAENV( 1, 'DORGQR', ' ', M, M, N, -1 )
               OPS = OPS + DOPLA( 'DORGQR', M, M, N, 0, NB )
               CALL DORGQR( M, M, N, U, LDU, WORK( ITAU ),
     $                      WORK( NWORK ), LWORK-NWORK+1, IERR )
*
*              Produce R in A, zeroing out other entries
*
               CALL DLASET( 'L', N-1, N-1, ZERO, ZERO, A( 2, 1 ), LDA )
               IE = ITAU
               ITAUQ = IE + N
               ITAUP = ITAUQ + N
               NWORK = ITAUP + N
*
*              Bidiagonalize R in A
*              (Workspace: need N*N+4*N, prefer N*N+3*N+2*N*NB)
*
               NB = ILAENV( 1, 'DGEBRD', ' ', N, N, -1, -1 )
               OPS = OPS + DOPLA( 'DGEBRD', N, N, 0, 0, NB )
               CALL DGEBRD( N, N, A, LDA, S, WORK( IE ), WORK( ITAUQ ),
     $                      WORK( ITAUP ), WORK( NWORK ), LWORK-NWORK+1,
     $                      IERR )
*
*              Perform bidiagonal SVD, computing left singular vectors
*              of bidiagonal matrix in WORK(IU) and computing right
*              singular vectors of bidiagonal matrix in VT
*              (Workspace: need N+N*N+BDSPAC)
*
               CALL DBDSDC( 'U', 'I', N, S, WORK( IE ), WORK( IU ), N,
     $                      VT, LDVT, DUM, IDUM, WORK( NWORK ), IWORK,
     $                      INFO )
*
*              Overwrite WORK(IU) by left singular vectors of R and VT
*              by right singular vectors of R
*              (Workspace: need N*N+3*N, prefer N*N+2*N+N*NB)
*
               NB = ILAENV( 1, 'DORMBR', 'QLN', N, N, N, -1 )
               OPS = OPS + DOPLA2( 'DORMBR', 'QLN', N, N, N, 0, NB )
               CALL DORMBR( 'Q', 'L', 'N', N, N, N, A, LDA,
     $                      WORK( ITAUQ ), WORK( IU ), LDWRKU,
     $                      WORK( NWORK ), LWORK-NWORK+1, IERR )
               NB = ILAENV( 1, 'DORMBR', 'PRT', N, N, N, -1 )
               OPS = OPS + DOPLA2( 'DORMBR', 'PRT', N, N, N, 0, NB )
               CALL DORMBR( 'P', 'R', 'T', N, N, N, A, LDA,
     $                      WORK( ITAUP ), VT, LDVT, WORK( NWORK ),
     $                      LWORK-NWORK+1, IERR )
*
*              Multiply Q in U by left singular vectors of R in
*              WORK(IU), storing result in A
*              (Workspace: need N*N)
*
               OPS = OPS + DOPBL3( 'DGEMM ', M, N, N )
               CALL DGEMM( 'N', 'N', M, N, N, ONE, U, LDU, WORK( IU ),
     $                     LDWRKU, ZERO, A, LDA )
*
*              Copy left singular vectors of A from A to U
*
               CALL DLACPY( 'F', M, N, A, LDA, U, LDU )
*
            END IF
*
         ELSE
*
*           M .LT. MNTHR
*
*           Path 5 (M at least N, but not much larger)
*           Reduce to bidiagonal form without QR decomposition
*
            IE = 1
            ITAUQ = IE + N
            ITAUP = ITAUQ + N
            NWORK = ITAUP + N
*
*           Bidiagonalize A
*           (Workspace: need 3*N+M, prefer 3*N+(M+N)*NB)
*
            NB = MAX( 1, ILAENV( 1, 'DGEBRD', ' ', M, N, -1, -1 ) )
            OPS = OPS + DOPLA( 'DGEBRD', M, N, 0, 0, NB )
            CALL DGEBRD( M, N, A, LDA, S, WORK( IE ), WORK( ITAUQ ),
     $                   WORK( ITAUP ), WORK( NWORK ), LWORK-NWORK+1,
     $                   IERR )
            IF( WNTQN ) THEN
*
*              Perform bidiagonal SVD, only computing singular values
*              (Workspace: need N+BDSPAC)
*
               CALL DBDSDC( 'U', 'N', N, S, WORK( IE ), DUM, 1, DUM, 1,
     $                      DUM, IDUM, WORK( NWORK ), IWORK, INFO )
            ELSE IF( WNTQO ) THEN
               IU = NWORK
               IF( LWORK.GE.M*N+3*N+BDSPAC ) THEN
*
*                 WORK( IU ) is M by N
*
                  LDWRKU = M
                  NWORK = IU + LDWRKU*N
                  CALL DLASET( 'F', M, N, ZERO, ZERO, WORK( IU ),
     $                         LDWRKU )
               ELSE
*
*                 WORK( IU ) is N by N
*
                  LDWRKU = N
                  NWORK = IU + LDWRKU*N
*
*                 WORK(IR) is LDWRKR by N
*
                  IR = NWORK
                  LDWRKR = ( LWORK-N*N-3*N ) / N
               END IF
               NWORK = IU + LDWRKU*N
*
*              Perform bidiagonal SVD, computing left singular vectors
*              of bidiagonal matrix in WORK(IU) and computing right
*              singular vectors of bidiagonal matrix in VT
*              (Workspace: need N+N*N+BDSPAC)
*
               CALL DBDSDC( 'U', 'I', N, S, WORK( IE ), WORK( IU ),
     $                      LDWRKU, VT, LDVT, DUM, IDUM, WORK( NWORK ),
     $                      IWORK, INFO )
*
*              Overwrite VT by right singular vectors of A
*              (Workspace: need N*N+2*N, prefer N*N+N+N*NB)
*
               NB = ILAENV( 1, 'DORMBR', 'PRT', N, N, N, -1 )
               OPS = OPS + DOPLA2( 'DORMBR', 'PRT', N, N, N, 0, NB )
               CALL DORMBR( 'P', 'R', 'T', N, N, N, A, LDA,
     $                      WORK( ITAUP ), VT, LDVT, WORK( NWORK ),
     $                      LWORK-NWORK+1, IERR )
*
               IF( LWORK.GE.M*N+3*N+BDSPAC ) THEN
*
*                 Overwrite WORK(IU) by left singular vectors of A
*                 (Workspace: need N*N+2*N, prefer N*N+N+N*NB)
*
                  NB = ILAENV( 1, 'DORMBR', 'QLN', M, N, N, -1 )
                  OPS = OPS + DOPLA2( 'DORMBR', 'QLN', M, N, N, 0, NB )
                  CALL DORMBR( 'Q', 'L', 'N', M, N, N, A, LDA,
     $                         WORK( ITAUQ ), WORK( IU ), LDWRKU,
     $                         WORK( NWORK ), LWORK-NWORK+1, IERR )
*
*                 Copy left singular vectors of A from WORK(IU) to A
*
                  CALL DLACPY( 'F', M, N, WORK( IU ), LDWRKU, A, LDA )
               ELSE
*
*                 Generate Q in A
*                 (Workspace: need N*N+2*N, prefer N*N+N+N*NB)
*
                  NB = ILAENV( 1, 'DORGBR', 'Q', M, N, N, -1 )
                  OPS = OPS + DOPLA2( 'DORGBR', 'Q', M, N, N, 0, NB )
                  CALL DORGBR( 'Q', M, N, N, A, LDA, WORK( ITAUQ ),
     $                         WORK( NWORK ), LWORK-NWORK+1, IERR )
*
*                 Multiply Q in A by left singular vectors of
*                 bidiagonal matrix in WORK(IU), storing result in
*                 WORK(IR) and copying to A
*                 (Workspace: need 2*N*N, prefer N*N+M*N)
*
                  DO 20 I = 1, M, LDWRKR
                     CHUNK = MIN( M-I+1, LDWRKR )
                     OPS = OPS + DOPBL3( 'DGEMM ', CHUNK, N, N )
                     CALL DGEMM( 'N', 'N', CHUNK, N, N, ONE, A( I, 1 ),
     $                           LDA, WORK( IU ), LDWRKU, ZERO,
     $                           WORK( IR ), LDWRKR )
                     CALL DLACPY( 'F', CHUNK, N, WORK( IR ), LDWRKR,
     $                            A( I, 1 ), LDA )
   20             CONTINUE
               END IF
*
            ELSE IF( WNTQS ) THEN
*
*              Perform bidiagonal SVD, computing left singular vectors
*              of bidiagonal matrix in U and computing right singular
*              vectors of bidiagonal matrix in VT
*              (Workspace: need N+BDSPAC)
*
               CALL DLASET( 'F', M, N, ZERO, ZERO, U, LDU )
               CALL DBDSDC( 'U', 'I', N, S, WORK( IE ), U, LDU, VT,
     $                      LDVT, DUM, IDUM, WORK( NWORK ), IWORK,
     $                      INFO )
*
*              Overwrite U by left singular vectors of A and VT
*              by right singular vectors of A
*              (Workspace: need 3*N, prefer 2*N+N*NB)
*
               NB = ILAENV( 1, 'DORMBR', 'QLN', M, N, N, -1 )
               OPS = OPS + DOPLA2( 'DORMBR', 'QLN', M, N, N, 0, NB )
               CALL DORMBR( 'Q', 'L', 'N', M, N, N, A, LDA,
     $                      WORK( ITAUQ ), U, LDU, WORK( NWORK ),
     $                      LWORK-NWORK+1, IERR )
               NB = ILAENV( 1, 'DORMBR', 'PRT', N, N, N, -1 )
               OPS = OPS + DOPLA2( 'DORMBR', 'PRT', N, N, N, 0, NB )
               CALL DORMBR( 'P', 'R', 'T', N, N, N, A, LDA,
     $                      WORK( ITAUP ), VT, LDVT, WORK( NWORK ),
     $                      LWORK-NWORK+1, IERR )
            ELSE IF( WNTQA ) THEN
*
*              Perform bidiagonal SVD, computing left singular vectors
*              of bidiagonal matrix in U and computing right singular
*              vectors of bidiagonal matrix in VT
*              (Workspace: need N+BDSPAC)
*
               CALL DLASET( 'F', M, M, ZERO, ZERO, U, LDU )
               CALL DBDSDC( 'U', 'I', N, S, WORK( IE ), U, LDU, VT,
     $                      LDVT, DUM, IDUM, WORK( NWORK ), IWORK,
     $                      INFO )
*
*              Set the right corner of U to identity matrix
*
               CALL DLASET( 'F', M-N, M-N, ZERO, ONE, U( N+1, N+1 ),
     $                      LDU )
*
*              Overwrite U by left singular vectors of A and VT
*              by right singular vectors of A
*              (Workspace: need N*N+2*N+M, prefer N*N+2*N+M*NB)
*
               NB = ILAENV( 1, 'DORMBR', 'QLN', M, M, N, -1 )
               OPS = OPS + DOPLA2( 'DORMBR', 'QLN', M, M, N, 0, NB )
               CALL DORMBR( 'Q', 'L', 'N', M, M, N, A, LDA,
     $                      WORK( ITAUQ ), U, LDU, WORK( NWORK ),
     $                      LWORK-NWORK+1, IERR )
               NB = ILAENV( 1, 'DORMBR', 'PRT', N, N, M, -1 )
               OPS = OPS + DOPLA2( 'DORMBR', 'PRT', N, N, M, 0, NB )
               CALL DORMBR( 'P', 'R', 'T', N, N, M, A, LDA,
     $                      WORK( ITAUP ), VT, LDVT, WORK( NWORK ),
     $                      LWORK-NWORK+1, IERR )
            END IF
*
         END IF
*
      ELSE
*
*        A has more columns than rows. If A has sufficiently more
*        columns than rows, first reduce using the LQ decomposition (if
*        sufficient workspace available)
*
         IF( N.GE.MNTHR ) THEN
*
            IF( WNTQN ) THEN
*
*              Path 1t (N much larger than M, JOBZ='N')
*              No singular vectors to be computed
*
               ITAU = 1
               NWORK = ITAU + M
*
*              Compute A=L*Q
*              (Workspace: need 2*M, prefer M+M*NB)
*
               NB = ILAENV( 1, 'DGELQF', ' ', M, N, -1, -1 )
               OPS = OPS + DOPLA( 'DGELQF', M, N, 0, 0, NB )
               CALL DGELQF( M, N, A, LDA, WORK( ITAU ), WORK( NWORK ),
     $                      LWORK-NWORK+1, IERR )
*
*              Zero out above L
*
               CALL DLASET( 'U', M-1, M-1, ZERO, ZERO, A( 1, 2 ), LDA )
               IE = 1
               ITAUQ = IE + M
               ITAUP = ITAUQ + M
               NWORK = ITAUP + M
*
*              Bidiagonalize L in A
*              (Workspace: need 4*M, prefer 3*M+2*M*NB)
*
               NB = ILAENV( 1, 'DGEBRD', ' ', M, M, -1, -1 )
               OPS = OPS + DOPLA( 'DGEBRD', M, M, 0, 0, NB )
               CALL DGEBRD( M, M, A, LDA, S, WORK( IE ), WORK( ITAUQ ),
     $                      WORK( ITAUP ), WORK( NWORK ), LWORK-NWORK+1,
     $                      IERR )
               NWORK = IE + M
*
*              Perform bidiagonal SVD, computing singular values only
*              (Workspace: need M+BDSPAC)
*
               CALL DBDSDC( 'U', 'N', M, S, WORK( IE ), DUM, 1, DUM, 1,
     $                      DUM, IDUM, WORK( NWORK ), IWORK, INFO )
*
            ELSE IF( WNTQO ) THEN
*
*              Path 2t (N much larger than M, JOBZ='O')
*              M right singular vectors to be overwritten on A and
*              M left singular vectors to be computed in U
*
               IVT = 1
*
*              IVT is M by M
*
               IL = IVT + M*M
               IF( LWORK.GE.M*N+M*M+3*M+BDSPAC ) THEN
*
*                 WORK(IL) is M by N
*
                  LDWRKL = M
                  CHUNK = N
               ELSE
                  LDWRKL = M
                  CHUNK = ( LWORK-M*M ) / M
               END IF
               ITAU = IL + LDWRKL*M
               NWORK = ITAU + M
*
*              Compute A=L*Q
*              (Workspace: need M*M+2*M, prefer M*M+M+M*NB)
*
               NB = ILAENV( 1, 'DGELQF', ' ', M, N, -1, -1 )
               OPS = OPS + DOPLA( 'DGELQF', M, N, 0, 0, NB )
               CALL DGELQF( M, N, A, LDA, WORK( ITAU ), WORK( NWORK ),
     $                      LWORK-NWORK+1, IERR )
*
*              Copy L to WORK(IL), zeroing about above it
*
               CALL DLACPY( 'L', M, M, A, LDA, WORK( IL ), LDWRKL )
               CALL DLASET( 'U', M-1, M-1, ZERO, ZERO,
     $                      WORK( IL+LDWRKL ), LDWRKL )
*
*              Generate Q in A
*              (Workspace: need M*M+2*M, prefer M*M+M+M*NB)
*
               NB = ILAENV( 1, 'DORGLQ', ' ', M, N, M, -1 )
               OPS = OPS + DOPLA( 'DORGLQ', M, N, M, 0, NB )
               CALL DORGLQ( M, N, M, A, LDA, WORK( ITAU ),
     $                      WORK( NWORK ), LWORK-NWORK+1, IERR )
               IE = ITAU
               ITAUQ = IE + M
               ITAUP = ITAUQ + M
               NWORK = ITAUP + M
*
*              Bidiagonalize L in WORK(IL)
*              (Workspace: need M*M+4*M, prefer M*M+3*M+2*M*NB)
*
               NB = ILAENV( 1, 'DGEBRD', ' ', M, M, -1, -1 )
               OPS = OPS + DOPLA( 'DGEBRD', M, M, 0, 0, NB )
               CALL DGEBRD( M, M, WORK( IL ), LDWRKL, S, WORK( IE ),
     $                      WORK( ITAUQ ), WORK( ITAUP ), WORK( NWORK ),
     $                      LWORK-NWORK+1, IERR )
*
*              Perform bidiagonal SVD, computing left singular vectors
*              of bidiagonal matrix in U, and computing right singular
*              vectors of bidiagonal matrix in WORK(IVT)
*              (Workspace: need M+M*M+BDSPAC)
*
               CALL DBDSDC( 'U', 'I', M, S, WORK( IE ), U, LDU,
     $                      WORK( IVT ), M, DUM, IDUM, WORK( NWORK ),
     $                      IWORK, INFO )
*
*              Overwrite U by left singular vectors of L and WORK(IVT)
*              by right singular vectors of L
*              (Workspace: need 2*M*M+3*M, prefer 2*M*M+2*M+M*NB)
*
               NB = ILAENV( 1, 'DORMBR', 'QLN', M, M, M, -1 )
               OPS = OPS + DOPLA2( 'DORMBR', 'QLN', M, M, M, 0, NB )
               CALL DORMBR( 'Q', 'L', 'N', M, M, M, WORK( IL ), LDWRKL,
     $                      WORK( ITAUQ ), U, LDU, WORK( NWORK ),
     $                      LWORK-NWORK+1, IERR )
               NB = ILAENV( 1, 'DORMBR', 'PRT', M, M, M, -1 )
               OPS = OPS + DOPLA2( 'DORMBR', 'PRT', M, M, M, 0, NB )
               CALL DORMBR( 'P', 'R', 'T', M, M, M, WORK( IL ), LDWRKL,
     $                      WORK( ITAUP ), WORK( IVT ), M,
     $                      WORK( NWORK ), LWORK-NWORK+1, IERR )
*
*              Multiply right singular vectors of L in WORK(IVT) by Q
*              in A, storing result in WORK(IL) and copying to A
*              (Workspace: need 2*M*M, prefer M*M+M*N)
*
               DO 30 I = 1, N, CHUNK
                  BLK = MIN( N-I+1, CHUNK )
                  OPS = OPS + DOPBL3( 'DGEMM ', M, BLK, M )
                  CALL DGEMM( 'N', 'N', M, BLK, M, ONE, WORK( IVT ), M,
     $                        A( 1, I ), LDA, ZERO, WORK( IL ), LDWRKL )
                  CALL DLACPY( 'F', M, BLK, WORK( IL ), LDWRKL,
     $                         A( 1, I ), LDA )
   30          CONTINUE
*
            ELSE IF( WNTQS ) THEN
*
*              Path 3t (N much larger than M, JOBZ='S')
*              M right singular vectors to be computed in VT and
*              M left singular vectors to be computed in U
*
               IL = 1
*
*              WORK(IL) is M by M
*
               LDWRKL = M
               ITAU = IL + LDWRKL*M
               NWORK = ITAU + M
*
*              Compute A=L*Q
*              (Workspace: need M*M+2*M, prefer M*M+M+M*NB)
*
               NB = ILAENV( 1, 'DGELQF', ' ', M, N, -1, -1 )
               OPS = OPS + DOPLA( 'DGELQF', M, N, 0, 0, NB )
               CALL DGELQF( M, N, A, LDA, WORK( ITAU ), WORK( NWORK ),
     $                      LWORK-NWORK+1, IERR )
*
*              Copy L to WORK(IL), zeroing out above it
*
               CALL DLACPY( 'L', M, M, A, LDA, WORK( IL ), LDWRKL )
               CALL DLASET( 'U', M-1, M-1, ZERO, ZERO,
     $                      WORK( IL+LDWRKL ), LDWRKL )
*
*              Generate Q in A
*              (Workspace: need M*M+2*M, prefer M*M+M+M*NB)
*
               NB = ILAENV( 1, 'DORGLQ', ' ', M, N, M, -1 )
               OPS = OPS + DOPLA( 'DORGLQ', M, N, M, 0, NB )
               CALL DORGLQ( M, N, M, A, LDA, WORK( ITAU ),
     $                      WORK( NWORK ), LWORK-NWORK+1, IERR )
               IE = ITAU
               ITAUQ = IE + M
               ITAUP = ITAUQ + M
               NWORK = ITAUP + M
*
*              Bidiagonalize L in WORK(IU), copying result to U
*              (Workspace: need M*M+4*M, prefer M*M+3*M+2*M*NB)
*
               NB = MAX( 1, ILAENV( 1, 'DGEBRD', ' ', M, M, -1, -1 ) )
               OPS = OPS + DOPLA( 'DGEBRD', M, M, 0, 0, NB )
               CALL DGEBRD( M, M, WORK( IL ), LDWRKL, S, WORK( IE ),
     $                      WORK( ITAUQ ), WORK( ITAUP ), WORK( NWORK ),
     $                      LWORK-NWORK+1, IERR )
*
*              Perform bidiagonal SVD, computing left singular vectors
*              of bidiagonal matrix in U and computing right singular
*              vectors of bidiagonal matrix in VT
*              (Workspace: need M+BDSPAC)
*
               CALL DBDSDC( 'U', 'I', M, S, WORK( IE ), U, LDU, VT,
     $                      LDVT, DUM, IDUM, WORK( NWORK ), IWORK,
     $                      INFO )
*
*              Overwrite U by left singular vectors of L and VT
*              by right singular vectors of L
*              (Workspace: need M*M+3*M, prefer M*M+2*M+M*NB)
*
               NB = ILAENV( 1, 'DORMBR', 'QLN', M, M, M, -1 )
               OPS = OPS + DOPLA2( 'DORMBR', 'QLN', M, M, M, 0, NB )
               CALL DORMBR( 'Q', 'L', 'N', M, M, M, WORK( IL ), LDWRKL,
     $                      WORK( ITAUQ ), U, LDU, WORK( NWORK ),
     $                      LWORK-NWORK+1, IERR )
               NB = ILAENV( 1, 'DORMBR', 'PRT', M, M, M, -1 )
               OPS = OPS + DOPLA2( 'DORMBR', 'PRT', M, M, M, 0, NB )
               CALL DORMBR( 'P', 'R', 'T', M, M, M, WORK( IL ), LDWRKL,
     $                      WORK( ITAUP ), VT, LDVT, WORK( NWORK ),
     $                      LWORK-NWORK+1, IERR )
*
*              Multiply right singular vectors of L in WORK(IL) by
*              Q in A, storing result in VT
*              (Workspace: need M*M)
*
               CALL DLACPY( 'F', M, M, VT, LDVT, WORK( IL ), LDWRKL )
               OPS = OPS + DOPBL3( 'DGEMM ', M, N, M )
               CALL DGEMM( 'N', 'N', M, N, M, ONE, WORK( IL ), LDWRKL,
     $                     A, LDA, ZERO, VT, LDVT )
*
            ELSE IF( WNTQA ) THEN
*
*              Path 4t (N much larger than M, JOBZ='A')
*              N right singular vectors to be computed in VT and
*              M left singular vectors to be computed in U
*
               IVT = 1
*
*              WORK(IVT) is M by M
*
               LDWKVT = M
               ITAU = IVT + LDWKVT*M
               NWORK = ITAU + M
*
*              Compute A=L*Q, copying result to VT
*              (Workspace: need M*M+2*M, prefer M*M+M+M*NB)
*
               NB = ILAENV( 1, 'DGELQF', ' ', M, N, -1, -1 )
               OPS = OPS + DOPLA( 'DGELQF', M, N, 0, 0, NB )
               CALL DGELQF( M, N, A, LDA, WORK( ITAU ), WORK( NWORK ),
     $                      LWORK-NWORK+1, IERR )
               CALL DLACPY( 'U', M, N, A, LDA, VT, LDVT )
*
*              Generate Q in VT
*              (Workspace: need M*M+2*M, prefer M*M+M+M*NB)
*
               NB = ILAENV( 1, 'DORGLQ', ' ', N, N, M, -1 )
               OPS = OPS + DOPLA( 'DORGLQ', N, N, M, 0, NB )
               CALL DORGLQ( N, N, M, VT, LDVT, WORK( ITAU ),
     $                      WORK( NWORK ), LWORK-NWORK+1, IERR )
*
*              Produce L in A, zeroing out other entries
*
               CALL DLASET( 'U', M-1, M-1, ZERO, ZERO, A( 1, 2 ), LDA )
               IE = ITAU
               ITAUQ = IE + M
               ITAUP = ITAUQ + M
               NWORK = ITAUP + M
*
*              Bidiagonalize L in A
*              (Workspace: need M*M+4*M, prefer M*M+3*M+2*M*NB)
*
               NB = ILAENV( 1, 'DGEBRD', ' ', M, M, -1, -1 )
               OPS = OPS + DOPLA( 'DGEBRD', M, M, 0, 0, NB )
               CALL DGEBRD( M, M, A, LDA, S, WORK( IE ), WORK( ITAUQ ),
     $                      WORK( ITAUP ), WORK( NWORK ), LWORK-NWORK+1,
     $                      IERR )
*
*              Perform bidiagonal SVD, computing left singular vectors
*              of bidiagonal matrix in U and computing right singular
*              vectors of bidiagonal matrix in WORK(IVT)
*              (Workspace: need M+M*M+BDSPAC)
*
               CALL DBDSDC( 'U', 'I', M, S, WORK( IE ), U, LDU,
     $                      WORK( IVT ), LDWKVT, DUM, IDUM,
     $                      WORK( NWORK ), IWORK, INFO )
*
*              Overwrite U by left singular vectors of L and WORK(IVT)
*              by right singular vectors of L
*              (Workspace: need M*M+3*M, prefer M*M+2*M+M*NB)
*
               NB = ILAENV( 1, 'DORMBR', 'QLN', M, M, M, -1 )
               OPS = OPS + DOPLA2( 'DORMBR', 'QLN', M, M, M, 0, NB )
               CALL DORMBR( 'Q', 'L', 'N', M, M, M, A, LDA,
     $                      WORK( ITAUQ ), U, LDU, WORK( NWORK ),
     $                      LWORK-NWORK+1, IERR )
               NB = ILAENV( 1, 'DORMBR', 'PRT', M, M, M, -1 )
               OPS = OPS + DOPLA2( 'DORMBR', 'PRT', M, M, M, 0, NB )
               CALL DORMBR( 'P', 'R', 'T', M, M, M, A, LDA,
     $                      WORK( ITAUP ), WORK( IVT ), LDWKVT,
     $                      WORK( NWORK ), LWORK-NWORK+1, IERR )
*
*              Multiply right singular vectors of L in WORK(IVT) by
*              Q in VT, storing result in A
*              (Workspace: need M*M)
*
               OPS = OPS + DOPBL3( 'DGEMM ', M, N, M )
               CALL DGEMM( 'N', 'N', M, N, M, ONE, WORK( IVT ), LDWKVT,
     $                     VT, LDVT, ZERO, A, LDA )
*
*              Copy right singular vectors of A from A to VT
*
               CALL DLACPY( 'F', M, N, A, LDA, VT, LDVT )
*
            END IF
*
         ELSE
*
*           N .LT. MNTHR
*
*           Path 5t (N greater than M, but not much larger)
*           Reduce to bidiagonal form without LQ decomposition
*
            IE = 1
            ITAUQ = IE + M
            ITAUP = ITAUQ + M
            NWORK = ITAUP + M
*
*           Bidiagonalize A
*           (Workspace: need 3*M+N, prefer 3*M+(M+N)*NB)
*
            NB = MAX( 1, ILAENV( 1, 'DGEBRD', ' ', M, N, -1, -1 ) )
            OPS = OPS + DOPLA( 'DGEBRD', M, N, 0, 0, NB )
            CALL DGEBRD( M, N, A, LDA, S, WORK( IE ), WORK( ITAUQ ),
     $                   WORK( ITAUP ), WORK( NWORK ), LWORK-NWORK+1,
     $                   IERR )
            IF( WNTQN ) THEN
*
*              Perform bidiagonal SVD, only computing singular values
*              (Workspace: need M+BDSPAC)
*
               CALL DBDSDC( 'L', 'N', M, S, WORK( IE ), DUM, 1, DUM, 1,
     $                      DUM, IDUM, WORK( NWORK ), IWORK, INFO )
            ELSE IF( WNTQO ) THEN
               LDWKVT = M
               IVT = NWORK
               IF( LWORK.GE.M*N+3*M+BDSPAC ) THEN
*
*                 WORK( IVT ) is M by N
*
                  CALL DLASET( 'F', M, N, ZERO, ZERO, WORK( IVT ),
     $                         LDWKVT )
                  NWORK = IVT + LDWKVT*N
               ELSE
*
*                 WORK( IVT ) is M by M
*
                  NWORK = IVT + LDWKVT*M
                  IL = NWORK
*
*                 WORK(IL) is M by CHUNK
*
                  CHUNK = ( LWORK-M*M-3*M ) / M
               END IF
*
*              Perform bidiagonal SVD, computing left singular vectors
*              of bidiagonal matrix in U and computing right singular
*              vectors of bidiagonal matrix in WORK(IVT)
*              (Workspace: need M*M+BDSPAC)
*
               CALL DBDSDC( 'L', 'I', M, S, WORK( IE ), U, LDU,
     $                      WORK( IVT ), LDWKVT, DUM, IDUM,
     $                      WORK( NWORK ), IWORK, INFO )
*
*              Overwrite U by left singular vectors of A
*              (Workspace: need M*M+2*M, prefer M*M+M+M*NB)
*
               NB = ILAENV( 1, 'DORMBR', 'QLN', M, M, N, -1 )
               OPS = OPS + DOPLA2( 'DORMBR', 'QLN', M, M, N, 0, NB )
               CALL DORMBR( 'Q', 'L', 'N', M, M, N, A, LDA,
     $                      WORK( ITAUQ ), U, LDU, WORK( NWORK ),
     $                      LWORK-NWORK+1, IERR )
*
               IF( LWORK.GE.M*N+3*M+BDSPAC ) THEN
*
*                 Overwrite WORK(IVT) by left singular vectors of A
*                 (Workspace: need M*M+2*M, prefer M*M+M+M*NB)
*
                  NB = ILAENV( 1, 'DORMBR', 'PRT', M, N, M, -1 )
                  OPS = OPS + DOPLA2( 'DORMBR', 'PRT', M, N, M, 0, NB )
                  CALL DORMBR( 'P', 'R', 'T', M, N, M, A, LDA,
     $                         WORK( ITAUP ), WORK( IVT ), LDWKVT,
     $                         WORK( NWORK ), LWORK-NWORK+1, IERR )
*
*                 Copy right singular vectors of A from WORK(IVT) to A
*
                  CALL DLACPY( 'F', M, N, WORK( IVT ), LDWKVT, A, LDA )
               ELSE
*
*                 Generate P**T in A
*                 (Workspace: need M*M+2*M, prefer M*M+M+M*NB)
*
                  NB = ILAENV( 1, 'DORGBR', 'P', M, N, M, -1 )
                  OPS = OPS + DOPLA2( 'DORGBR', 'P', M, N, M, 0, NB )
                  CALL DORGBR( 'P', M, N, M, A, LDA, WORK( ITAUP ),
     $                         WORK( NWORK ), LWORK-NWORK+1, IERR )
*
*                 Multiply Q in A by right singular vectors of
*                 bidiagonal matrix in WORK(IVT), storing result in
*                 WORK(IL) and copying to A
*                 (Workspace: need 2*M*M, prefer M*M+M*N)
*
                  DO 40 I = 1, N, CHUNK
                     BLK = MIN( N-I+1, CHUNK )
                     OPS = OPS + DOPBL3( 'DGEMM ', M, BLK, M )
                     CALL DGEMM( 'N', 'N', M, BLK, M, ONE, WORK( IVT ),
     $                           LDWKVT, A( 1, I ), LDA, ZERO,
     $                           WORK( IL ), M )
                     CALL DLACPY( 'F', M, BLK, WORK( IL ), M, A( 1, I ),
     $                            LDA )
   40             CONTINUE
               END IF
            ELSE IF( WNTQS ) THEN
*
*              Perform bidiagonal SVD, computing left singular vectors
*              of bidiagonal matrix in U and computing right singular
*              vectors of bidiagonal matrix in VT
*              (Workspace: need M+BDSPAC)
*
               CALL DLASET( 'F', M, N, ZERO, ZERO, VT, LDVT )
               CALL DBDSDC( 'L', 'I', M, S, WORK( IE ), U, LDU, VT,
     $                      LDVT, DUM, IDUM, WORK( NWORK ), IWORK,
     $                      INFO )
*
*              Overwrite U by left singular vectors of A and VT
*              by right singular vectors of A
*              (Workspace: need 3*M, prefer 2*M+M*NB)
*
               NB = ILAENV( 1, 'DORMBR', 'QLN', M, M, N, -1 )
               OPS = OPS + DOPLA2( 'DORMBR', 'QLN', M, M, N, 0, NB )
               CALL DORMBR( 'Q', 'L', 'N', M, M, N, A, LDA,
     $                      WORK( ITAUQ ), U, LDU, WORK( NWORK ),
     $                      LWORK-NWORK+1, IERR )
               NB = ILAENV( 1, 'DORMBR', 'PRT', M, N, M, -1 )
               OPS = OPS + DOPLA2( 'DORMBR', 'PRT', M, N, M, 0, NB )
               CALL DORMBR( 'P', 'R', 'T', M, N, M, A, LDA,
     $                      WORK( ITAUP ), VT, LDVT, WORK( NWORK ),
     $                      LWORK-NWORK+1, IERR )
            ELSE IF( WNTQA ) THEN
*
*              Perform bidiagonal SVD, computing left singular vectors
*              of bidiagonal matrix in U and computing right singular
*              vectors of bidiagonal matrix in VT
*              (Workspace: need M+BDSPAC)
*
               CALL DLASET( 'F', N, N, ZERO, ZERO, VT, LDVT )
               CALL DBDSDC( 'L', 'I', M, S, WORK( IE ), U, LDU, VT,
     $                      LDVT, DUM, IDUM, WORK( NWORK ), IWORK,
     $                      INFO )
*
*              Set the right corner of VT to identity matrix
*
               CALL DLASET( 'F', N-M, N-M, ZERO, ONE, VT( M+1, M+1 ),
     $                      LDVT )
*
*              Overwrite U by left singular vectors of A and VT
*              by right singular vectors of A
*              (Workspace: need 2*M+N, prefer 2*M+N*NB)
*
               NB = ILAENV( 1, 'DORMBR', 'QLN', M, M, N, -1 )
               OPS = OPS + DOPLA2( 'DORMBR', 'QLN', M, M, N, 0, NB )
               CALL DORMBR( 'Q', 'L', 'N', M, M, N, A, LDA,
     $                      WORK( ITAUQ ), U, LDU, WORK( NWORK ),
     $                      LWORK-NWORK+1, IERR )
               NB = ILAENV( 1, 'DORMBR', 'PRT', N, N, M, -1 )
               OPS = OPS + DOPLA2( 'DORMBR', 'PRT', N, N, M, 0, NB )
               CALL DORMBR( 'P', 'R', 'T', N, N, M, A, LDA,
     $                      WORK( ITAUP ), VT, LDVT, WORK( NWORK ),
     $                      LWORK-NWORK+1, IERR )
            END IF
*
         END IF
*
      END IF
*
*     Undo scaling if necessary
*
      IF( ISCL.EQ.1 ) THEN
         IF( ANRM.GT.BIGNUM ) THEN
            OPS = OPS + DBLE( MINMN )
            CALL DLASCL( 'G', 0, 0, BIGNUM, ANRM, MINMN, 1, S, MINMN,
     $                   IERR )
         END IF
         IF( ANRM.LT.SMLNUM ) THEN
            OPS = OPS + DBLE( MINMN )
            CALL DLASCL( 'G', 0, 0, SMLNUM, ANRM, MINMN, 1, S, MINMN,
     $                   IERR )
         END IF
      END IF
*
*     Return optimal workspace in WORK(1)
*
      WORK( 1 ) = DBLE( MAXWRK )
*
      RETURN
*
*     End of DGESDD
*
      END
      SUBROUTINE DGGHRD( COMPQ, COMPZ, N, ILO, IHI, A, LDA, B, LDB, Q,
     $                   LDQ, Z, LDZ, INFO )
*
*  -- LAPACK routine (instrumented to count operations, version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     September 30, 1994
*
*     .. Scalar Arguments ..
      CHARACTER          COMPQ, COMPZ
      INTEGER            IHI, ILO, INFO, LDA, LDB, LDQ, LDZ, N
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), B( LDB, * ), Q( LDQ, * ),
     $                   Z( LDZ, * )
*     ..
*     ---------------------- Begin Timing Code -------------------------
*     Common block to return operation count and iteration count
*     ITCNT is initialized to 0, OPS is only incremented
*     OPST is used to accumulate small contributions to OPS
*     to avoid roundoff error
*     .. Common blocks ..
      COMMON             / LATIME / OPS, ITCNT
*     ..
*     .. Scalars in Common ..
      DOUBLE PRECISION   ITCNT, OPS
*     ..
*     ----------------------- End Timing Code --------------------------
*
*
*  Purpose
*  =======
*
*  DGGHRD reduces a pair of real matrices (A,B) to generalized upper
*  Hessenberg form using orthogonal transformations, where A is a
*  general matrix and B is upper triangular:  Q' * A * Z = H and
*  Q' * B * Z = T, where H is upper Hessenberg, T is upper triangular,
*  and Q and Z are orthogonal, and ' means transpose.
*
*  The orthogonal matrices Q and Z are determined as products of Givens
*  rotations.  They may either be formed explicitly, or they may be
*  postmultiplied into input matrices Q1 and Z1, so that
*
*       Q1 * A * Z1' = (Q1*Q) * H * (Z1*Z)'
*       Q1 * B * Z1' = (Q1*Q) * T * (Z1*Z)'
*
*  Arguments
*  =========
*
*  COMPQ   (input) CHARACTER*1
*          = 'N': do not compute Q;
*          = 'I': Q is initialized to the unit matrix, and the
*                 orthogonal matrix Q is returned;
*          = 'V': Q must contain an orthogonal matrix Q1 on entry,
*                 and the product Q1*Q is returned.
*
*  COMPZ   (input) CHARACTER*1
*          = 'N': do not compute Z;
*          = 'I': Z is initialized to the unit matrix, and the
*                 orthogonal matrix Z is returned;
*          = 'V': Z must contain an orthogonal matrix Z1 on entry,
*                 and the product Z1*Z is returned.
*
*  N       (input) INTEGER
*          The order of the matrices A and B.  N >= 0.
*
*  ILO     (input) INTEGER
*  IHI     (input) INTEGER
*          It is assumed that A is already upper triangular in rows and
*          columns 1:ILO-1 and IHI+1:N.  ILO and IHI are normally set
*          by a previous call to DGGBAL; otherwise they should be set
*          to 1 and N respectively.
*          1 <= ILO <= IHI <= N, if N > 0; ILO=1 and IHI=0, if N=0.
*
*  A       (input/output) DOUBLE PRECISION array, dimension (LDA, N)
*          On entry, the N-by-N general matrix to be reduced.
*          On exit, the upper triangle and the first subdiagonal of A
*          are overwritten with the upper Hessenberg matrix H, and the
*          rest is set to zero.
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A.  LDA >= max(1,N).
*
*  B       (input/output) DOUBLE PRECISION array, dimension (LDB, N)
*          On entry, the N-by-N upper triangular matrix B.
*          On exit, the upper triangular matrix T = Q' B Z.  The
*          elements below the diagonal are set to zero.
*
*  LDB     (input) INTEGER
*          The leading dimension of the array B.  LDB >= max(1,N).
*
*  Q       (input/output) DOUBLE PRECISION array, dimension (LDQ, N)
*          If COMPQ='N':  Q is not referenced.
*          If COMPQ='I':  on entry, Q need not be set, and on exit it
*                         contains the orthogonal matrix Q, where Q'
*                         is the product of the Givens transformations
*                         which are applied to A and B on the left.
*          If COMPQ='V':  on entry, Q must contain an orthogonal matrix
*                         Q1, and on exit this is overwritten by Q1*Q.
*
*  LDQ     (input) INTEGER
*          The leading dimension of the array Q.
*          LDQ >= N if COMPQ='V' or 'I'; LDQ >= 1 otherwise.
*
*  Z       (input/output) DOUBLE PRECISION array, dimension (LDZ, N)
*          If COMPZ='N':  Z is not referenced.
*          If COMPZ='I':  on entry, Z need not be set, and on exit it
*                         contains the orthogonal matrix Z, which is
*                         the product of the Givens transformations
*                         which are applied to A and B on the right.
*          If COMPZ='V':  on entry, Z must contain an orthogonal matrix
*                         Z1, and on exit this is overwritten by Z1*Z.
*
*  LDZ     (input) INTEGER
*          The leading dimension of the array Z.
*          LDZ >= N if COMPZ='V' or 'I'; LDZ >= 1 otherwise.
*
*  INFO    (output) INTEGER
*          = 0:  successful exit
*          < 0:  if INFO = -i, the i-th argument had an illegal value.
*
*  Further Details
*  ===============
*
*  This routine reduces A to Hessenberg and B to triangular form by
*  an unblocked reduction, as described in _Matrix_Computations_,
*  by Golub and Van Loan (Johns Hopkins Press.)
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ONE, ZERO
      PARAMETER          ( ONE = 1.0D+0, ZERO = 0.0D+0 )
*     ..
*     .. Local Scalars ..
      LOGICAL            ILQ, ILZ
      INTEGER            ICOMPQ, ICOMPZ, JCOL, JROW
      DOUBLE PRECISION   C, S, TEMP
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
*     ..
*     .. External Subroutines ..
      EXTERNAL           DLARTG, DLASET, DROT, XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          DBLE, MAX
*     ..
*     .. Executable Statements ..
*
*     Decode COMPQ
*
      IF( LSAME( COMPQ, 'N' ) ) THEN
         ILQ = .FALSE.
         ICOMPQ = 1
      ELSE IF( LSAME( COMPQ, 'V' ) ) THEN
         ILQ = .TRUE.
         ICOMPQ = 2
      ELSE IF( LSAME( COMPQ, 'I' ) ) THEN
         ILQ = .TRUE.
         ICOMPQ = 3
      ELSE
         ICOMPQ = 0
      END IF
*
*     Decode COMPZ
*
      IF( LSAME( COMPZ, 'N' ) ) THEN
         ILZ = .FALSE.
         ICOMPZ = 1
      ELSE IF( LSAME( COMPZ, 'V' ) ) THEN
         ILZ = .TRUE.
         ICOMPZ = 2
      ELSE IF( LSAME( COMPZ, 'I' ) ) THEN
         ILZ = .TRUE.
         ICOMPZ = 3
      ELSE
         ICOMPZ = 0
      END IF
*
*     Test the input parameters.
*
      INFO = 0
      IF( ICOMPQ.LE.0 ) THEN
         INFO = -1
      ELSE IF( ICOMPZ.LE.0 ) THEN
         INFO = -2
      ELSE IF( N.LT.0 ) THEN
         INFO = -3
      ELSE IF( ILO.LT.1 ) THEN
         INFO = -4
      ELSE IF( IHI.GT.N .OR. IHI.LT.ILO-1 ) THEN
         INFO = -5
      ELSE IF( LDA.LT.MAX( 1, N ) ) THEN
         INFO = -7
      ELSE IF( LDB.LT.MAX( 1, N ) ) THEN
         INFO = -9
      ELSE IF( ( ILQ .AND. LDQ.LT.N ) .OR. LDQ.LT.1 ) THEN
         INFO = -11
      ELSE IF( ( ILZ .AND. LDZ.LT.N ) .OR. LDZ.LT.1 ) THEN
         INFO = -13
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DGGHRD', -INFO )
         RETURN
      END IF
*
*     Initialize Q and Z if desired.
*
      IF( ICOMPQ.EQ.3 )
     $   CALL DLASET( 'Full', N, N, ZERO, ONE, Q, LDQ )
      IF( ICOMPZ.EQ.3 )
     $   CALL DLASET( 'Full', N, N, ZERO, ONE, Z, LDZ )
*
*     Quick return if possible
*
      IF( N.LE.1 )
     $   RETURN
*
*     Zero out lower triangle of B
*
      DO 20 JCOL = 1, N - 1
         DO 10 JROW = JCOL + 1, N
            B( JROW, JCOL ) = ZERO
   10    CONTINUE
   20 CONTINUE
*
*     Reduce A and B
*
      DO 40 JCOL = ILO, IHI - 2
*
         DO 30 JROW = IHI, JCOL + 2, -1
*
*           Step 1: rotate rows JROW-1, JROW to kill A(JROW,JCOL)
*
            TEMP = A( JROW-1, JCOL )
            CALL DLARTG( TEMP, A( JROW, JCOL ), C, S,
     $                   A( JROW-1, JCOL ) )
            A( JROW, JCOL ) = ZERO
            CALL DROT( N-JCOL, A( JROW-1, JCOL+1 ), LDA,
     $                 A( JROW, JCOL+1 ), LDA, C, S )
            CALL DROT( N+2-JROW, B( JROW-1, JROW-1 ), LDB,
     $                 B( JROW, JROW-1 ), LDB, C, S )
            IF( ILQ )
     $         CALL DROT( N, Q( 1, JROW-1 ), 1, Q( 1, JROW ), 1, C, S )
*
*           Step 2: rotate columns JROW, JROW-1 to kill B(JROW,JROW-1)
*
            TEMP = B( JROW, JROW )
            CALL DLARTG( TEMP, B( JROW, JROW-1 ), C, S,
     $                   B( JROW, JROW ) )
            B( JROW, JROW-1 ) = ZERO
            CALL DROT( IHI, A( 1, JROW ), 1, A( 1, JROW-1 ), 1, C, S )
            CALL DROT( JROW-1, B( 1, JROW ), 1, B( 1, JROW-1 ), 1, C,
     $                 S )
            IF( ILZ )
     $         CALL DROT( N, Z( 1, JROW ), 1, Z( 1, JROW-1 ), 1, C, S )
   30    CONTINUE
   40 CONTINUE
*
*     ---------------------- Begin Timing Code -------------------------
*     Operation count:                                          factor
*     * number of calls to DLARTG   TEMP                          *7
*     * total number of rows/cols
*       rotated in A and B          TEMP*[6n + 2(ihi-ilo) + 5]/6  *6
*     * rows rotated in Q           TEMP*n/2                      *6
*     * rows rotated in Z           TEMP*n/2                      *6
*
      TEMP = DBLE( IHI-ILO )*DBLE( IHI-ILO-1 )
      JROW = 6*N + 2*( IHI-ILO ) + 12
      IF( ILQ )
     $   JROW = JROW + 3*N
      IF( ILZ )
     $   JROW = JROW + 3*N
      OPS = OPS + DBLE( JROW )*TEMP
      ITCNT = ZERO
*
*     ----------------------- End Timing Code --------------------------
*
      RETURN
*
*     End of DGGHRD
*
      END
      SUBROUTINE DHGEQZ( JOB, COMPQ, COMPZ, N, ILO, IHI, A, LDA, B, LDB,
     $                   ALPHAR, ALPHAI, BETA, Q, LDQ, Z, LDZ, WORK,
     $                   LWORK, INFO )
*
*  -- LAPACK routine (instrumented to count operations, version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     June 30, 1999
*
*     .. Scalar Arguments ..
      CHARACTER          COMPQ, COMPZ, JOB
      INTEGER            IHI, ILO, INFO, LDA, LDB, LDQ, LDZ, LWORK, N
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), ALPHAI( * ), ALPHAR( * ),
     $                   B( LDB, * ), BETA( * ), Q( LDQ, * ), WORK( * ),
     $                   Z( LDZ, * )
*     ..
*     ---------------------- Begin Timing Code -------------------------
*     Common block to return operation count and iteration count
*     ITCNT is initialized to 0, OPS is only incremented
*     OPST is used to accumulate small contributions to OPS
*     to avoid roundoff error
*     .. Common blocks ..
      COMMON             / LATIME / OPS, ITCNT
*     ..
*     .. Scalars in Common ..
      DOUBLE PRECISION   ITCNT, OPS
*     ..
*     ----------------------- End Timing Code --------------------------
*
*  Purpose
*  =======
*
*  DHGEQZ implements a single-/double-shift version of the QZ method for
*  finding the generalized eigenvalues
*
*  w(j)=(ALPHAR(j) + i*ALPHAI(j))/BETAR(j)   of the equation
*
*       det( A - w(i) B ) = 0
*
*  In addition, the pair A,B may be reduced to generalized Schur form:
*  B is upper triangular, and A is block upper triangular, where the
*  diagonal blocks are either 1-by-1 or 2-by-2, the 2-by-2 blocks having
*  complex generalized eigenvalues (see the description of the argument
*  JOB.)
*
*  If JOB='S', then the pair (A,B) is simultaneously reduced to Schur
*  form by applying one orthogonal tranformation (usually called Q) on
*  the left and another (usually called Z) on the right.  The 2-by-2
*  upper-triangular diagonal blocks of B corresponding to 2-by-2 blocks
*  of A will be reduced to positive diagonal matrices.  (I.e.,
*  if A(j+1,j) is non-zero, then B(j+1,j)=B(j,j+1)=0 and B(j,j) and
*  B(j+1,j+1) will be positive.)
*
*  If JOB='E', then at each iteration, the same transformations
*  are computed, but they are only applied to those parts of A and B
*  which are needed to compute ALPHAR, ALPHAI, and BETAR.
*
*  If JOB='S' and COMPQ and COMPZ are 'V' or 'I', then the orthogonal
*  transformations used to reduce (A,B) are accumulated into the arrays
*  Q and Z s.t.:
*
*       Q(in) A(in) Z(in)* = Q(out) A(out) Z(out)*
*       Q(in) B(in) Z(in)* = Q(out) B(out) Z(out)*
*
*  Ref: C.B. Moler & G.W. Stewart, "An Algorithm for Generalized Matrix
*       Eigenvalue Problems", SIAM J. Numer. Anal., 10(1973),
*       pp. 241--256.
*
*  Arguments
*  =========
*
*  JOB     (input) CHARACTER*1
*          = 'E': compute only ALPHAR, ALPHAI, and BETA.  A and B will
*                 not necessarily be put into generalized Schur form.
*          = 'S': put A and B into generalized Schur form, as well
*                 as computing ALPHAR, ALPHAI, and BETA.
*
*  COMPQ   (input) CHARACTER*1
*          = 'N': do not modify Q.
*          = 'V': multiply the array Q on the right by the transpose of
*                 the orthogonal tranformation that is applied to the
*                 left side of A and B to reduce them to Schur form.
*          = 'I': like COMPQ='V', except that Q will be initialized to
*                 the identity first.
*
*  COMPZ   (input) CHARACTER*1
*          = 'N': do not modify Z.
*          = 'V': multiply the array Z on the right by the orthogonal
*                 tranformation that is applied to the right side of
*                 A and B to reduce them to Schur form.
*          = 'I': like COMPZ='V', except that Z will be initialized to
*                 the identity first.
*
*  N       (input) INTEGER
*          The order of the matrices A, B, Q, and Z.  N >= 0.
*
*  ILO     (input) INTEGER
*  IHI     (input) INTEGER
*          It is assumed that A is already upper triangular in rows and
*          columns 1:ILO-1 and IHI+1:N.
*          1 <= ILO <= IHI <= N, if N > 0; ILO=1 and IHI=0, if N=0.
*
*  A       (input/output) DOUBLE PRECISION array, dimension (LDA, N)
*          On entry, the N-by-N upper Hessenberg matrix A.  Elements
*          below the subdiagonal must be zero.
*          If JOB='S', then on exit A and B will have been
*             simultaneously reduced to generalized Schur form.
*          If JOB='E', then on exit A will have been destroyed.
*             The diagonal blocks will be correct, but the off-diagonal
*             portion will be meaningless.
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A.  LDA >= max( 1, N ).
*
*  B       (input/output) DOUBLE PRECISION array, dimension (LDB, N)
*          On entry, the N-by-N upper triangular matrix B.  Elements
*          below the diagonal must be zero.  2-by-2 blocks in B
*          corresponding to 2-by-2 blocks in A will be reduced to
*          positive diagonal form.  (I.e., if A(j+1,j) is non-zero,
*          then B(j+1,j)=B(j,j+1)=0 and B(j,j) and B(j+1,j+1) will be
*          positive.)
*          If JOB='S', then on exit A and B will have been
*             simultaneously reduced to Schur form.
*          If JOB='E', then on exit B will have been destroyed.
*             Elements corresponding to diagonal blocks of A will be
*             correct, but the off-diagonal portion will be meaningless.
*
*  LDB     (input) INTEGER
*          The leading dimension of the array B.  LDB >= max( 1, N ).
*
*  ALPHAR  (output) DOUBLE PRECISION array, dimension (N)
*          ALPHAR(1:N) will be set to real parts of the diagonal
*          elements of A that would result from reducing A and B to
*          Schur form and then further reducing them both to triangular
*          form using unitary transformations s.t. the diagonal of B
*          was non-negative real.  Thus, if A(j,j) is in a 1-by-1 block
*          (i.e., A(j+1,j)=A(j,j+1)=0), then ALPHAR(j)=A(j,j).
*          Note that the (real or complex) values
*          (ALPHAR(j) + i*ALPHAI(j))/BETA(j), j=1,...,N, are the
*          generalized eigenvalues of the matrix pencil A - wB.
*
*  ALPHAI  (output) DOUBLE PRECISION array, dimension (N)
*          ALPHAI(1:N) will be set to imaginary parts of the diagonal
*          elements of A that would result from reducing A and B to
*          Schur form and then further reducing them both to triangular
*          form using unitary transformations s.t. the diagonal of B
*          was non-negative real.  Thus, if A(j,j) is in a 1-by-1 block
*          (i.e., A(j+1,j)=A(j,j+1)=0), then ALPHAR(j)=0.
*          Note that the (real or complex) values
*          (ALPHAR(j) + i*ALPHAI(j))/BETA(j), j=1,...,N, are the
*          generalized eigenvalues of the matrix pencil A - wB.
*
*  BETA    (output) DOUBLE PRECISION array, dimension (N)
*          BETA(1:N) will be set to the (real) diagonal elements of B
*          that would result from reducing A and B to Schur form and
*          then further reducing them both to triangular form using
*          unitary transformations s.t. the diagonal of B was
*          non-negative real.  Thus, if A(j,j) is in a 1-by-1 block
*          (i.e., A(j+1,j)=A(j,j+1)=0), then BETA(j)=B(j,j).
*          Note that the (real or complex) values
*          (ALPHAR(j) + i*ALPHAI(j))/BETA(j), j=1,...,N, are the
*          generalized eigenvalues of the matrix pencil A - wB.
*          (Note that BETA(1:N) will always be non-negative, and no
*          BETAI is necessary.)
*
*  Q       (input/output) DOUBLE PRECISION array, dimension (LDQ, N)
*          If COMPQ='N', then Q will not be referenced.
*          If COMPQ='V' or 'I', then the transpose of the orthogonal
*             transformations which are applied to A and B on the left
*             will be applied to the array Q on the right.
*
*  LDQ     (input) INTEGER
*          The leading dimension of the array Q.  LDQ >= 1.
*          If COMPQ='V' or 'I', then LDQ >= N.
*
*  Z       (input/output) DOUBLE PRECISION array, dimension (LDZ, N)
*          If COMPZ='N', then Z will not be referenced.
*          If COMPZ='V' or 'I', then the orthogonal transformations
*             which are applied to A and B on the right will be applied
*             to the array Z on the right.
*
*  LDZ     (input) INTEGER
*          The leading dimension of the array Z.  LDZ >= 1.
*          If COMPZ='V' or 'I', then LDZ >= N.
*
*  WORK    (workspace/output) DOUBLE PRECISION array, dimension (LWORK)
*          On exit, if INFO >= 0, WORK(1) returns the optimal LWORK.
*
*  LWORK   (input) INTEGER
*          The dimension of the array WORK.  LWORK >= max(1,N).
*
*          If LWORK = -1, then a workspace query is assumed; the routine
*          only calculates the optimal size of the WORK array, returns
*          this value as the first entry of the WORK array, and no error
*          message related to LWORK is issued by XERBLA.
*
*  INFO    (output) INTEGER
*          = 0: successful exit
*          < 0: if INFO = -i, the i-th argument had an illegal value
*          = 1,...,N: the QZ iteration did not converge.  (A,B) is not
*                     in Schur form, but ALPHAR(i), ALPHAI(i), and
*                     BETA(i), i=INFO+1,...,N should be correct.
*          = N+1,...,2*N: the shift calculation failed.  (A,B) is not
*                     in Schur form, but ALPHAR(i), ALPHAI(i), and
*                     BETA(i), i=INFO-N+1,...,N should be correct.
*          > 2*N:     various "impossible" errors.
*
*  Further Details
*  ===============
*
*  Iteration counters:
*
*  JITER  -- counts iterations.
*  IITER  -- counts iterations run since ILAST was last
*            changed.  This is therefore reset only when a 1-by-1 or
*            2-by-2 block deflates off the bottom.
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   HALF, ZERO, ONE, SAFETY
      PARAMETER          ( HALF = 0.5D+0, ZERO = 0.0D+0, ONE = 1.0D+0,
     $                   SAFETY = 1.0D+2 )
*     ..
*     .. Local Scalars ..
      LOGICAL            ILAZR2, ILAZRO, ILPIVT, ILQ, ILSCHR, ILZ,
     $                   LQUERY
      INTEGER            ICOMPQ, ICOMPZ, IFIRST, IFRSTM, IITER, ILAST,
     $                   ILASTM, IN, ISCHUR, ISTART, J, JC, JCH, JITER,
     $                   JR, MAXIT, NQ, NZ
      DOUBLE PRECISION   A11, A12, A1I, A1R, A21, A22, A2I, A2R, AD11,
     $                   AD11L, AD12, AD12L, AD21, AD21L, AD22, AD22L,
     $                   AD32L, AN, ANORM, ASCALE, ATOL, B11, B1A, B1I,
     $                   B1R, B22, B2A, B2I, B2R, BN, BNORM, BSCALE,
     $                   BTOL, C, C11I, C11R, C12, C21, C22I, C22R, CL,
     $                   CQ, CR, CZ, ESHIFT, OPST, S, S1, S1INV, S2,
     $                   SAFMAX, SAFMIN, SCALE, SL, SQI, SQR, SR, SZI,
     $                   SZR, T, TAU, TEMP, TEMP2, TEMPI, TEMPR, U1,
     $                   U12, U12L, U2, ULP, VS, W11, W12, W21, W22,
     $                   WABS, WI, WR, WR2
*     ..
*     .. Local Arrays ..
      DOUBLE PRECISION   V( 3 )
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      DOUBLE PRECISION   DLAMCH, DLANHS, DLAPY2, DLAPY3
      EXTERNAL           LSAME, DLAMCH, DLANHS, DLAPY2, DLAPY3
*     ..
*     .. External Subroutines ..
      EXTERNAL           DLAG2, DLARFG, DLARTG, DLASET, DLASV2, DROT,
     $                   XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, DBLE, MAX, MIN, SQRT
*     ..
*     .. Executable Statements ..
*
*     Decode JOB, COMPQ, COMPZ
*
      IF( LSAME( JOB, 'E' ) ) THEN
         ILSCHR = .FALSE.
         ISCHUR = 1
      ELSE IF( LSAME( JOB, 'S' ) ) THEN
         ILSCHR = .TRUE.
         ISCHUR = 2
      ELSE
         ISCHUR = 0
      END IF
*
      IF( LSAME( COMPQ, 'N' ) ) THEN
         ILQ = .FALSE.
         ICOMPQ = 1
         NQ = 0
      ELSE IF( LSAME( COMPQ, 'V' ) ) THEN
         ILQ = .TRUE.
         ICOMPQ = 2
         NQ = N
      ELSE IF( LSAME( COMPQ, 'I' ) ) THEN
         ILQ = .TRUE.
         ICOMPQ = 3
         NQ = N
      ELSE
         ICOMPQ = 0
      END IF
*
      IF( LSAME( COMPZ, 'N' ) ) THEN
         ILZ = .FALSE.
         ICOMPZ = 1
         NZ = 0
      ELSE IF( LSAME( COMPZ, 'V' ) ) THEN
         ILZ = .TRUE.
         ICOMPZ = 2
         NZ = N
      ELSE IF( LSAME( COMPZ, 'I' ) ) THEN
         ILZ = .TRUE.
         ICOMPZ = 3
         NZ = N
      ELSE
         ICOMPZ = 0
      END IF
*
*     Check Argument Values
*
      INFO = 0
      WORK( 1 ) = MAX( 1, N )
      LQUERY = ( LWORK.EQ.-1 )
      IF( ISCHUR.EQ.0 ) THEN
         INFO = -1
      ELSE IF( ICOMPQ.EQ.0 ) THEN
         INFO = -2
      ELSE IF( ICOMPZ.EQ.0 ) THEN
         INFO = -3
      ELSE IF( N.LT.0 ) THEN
         INFO = -4
      ELSE IF( ILO.LT.1 ) THEN
         INFO = -5
      ELSE IF( IHI.GT.N .OR. IHI.LT.ILO-1 ) THEN
         INFO = -6
      ELSE IF( LDA.LT.N ) THEN
         INFO = -8
      ELSE IF( LDB.LT.N ) THEN
         INFO = -10
      ELSE IF( LDQ.LT.1 .OR. ( ILQ .AND. LDQ.LT.N ) ) THEN
         INFO = -15
      ELSE IF( LDZ.LT.1 .OR. ( ILZ .AND. LDZ.LT.N ) ) THEN
         INFO = -17
      ELSE IF( LWORK.LT.MAX( 1, N ) .AND. .NOT.LQUERY ) THEN
         INFO = -19
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DHGEQZ', -INFO )
         RETURN
      ELSE IF( LQUERY ) THEN
         RETURN
      END IF
*
*     Quick return if possible
*
      IF( N.LE.0 ) THEN
         WORK( 1 ) = DBLE( 1 )
*        --------------------- Begin Timing Code -----------------------
         ITCNT = ZERO
*        ---------------------- End Timing Code ------------------------
         RETURN
      END IF
*
*     Initialize Q and Z
*
      IF( ICOMPQ.EQ.3 )
     $   CALL DLASET( 'Full', N, N, ZERO, ONE, Q, LDQ )
      IF( ICOMPZ.EQ.3 )
     $   CALL DLASET( 'Full', N, N, ZERO, ONE, Z, LDZ )
*
*     Machine Constants
*
      IN = IHI + 1 - ILO
      SAFMIN = DLAMCH( 'S' )
      SAFMAX = ONE / SAFMIN
      ULP = DLAMCH( 'E' )*DLAMCH( 'B' )
      ANORM = DLANHS( 'F', IN, A( ILO, ILO ), LDA, WORK )
      BNORM = DLANHS( 'F', IN, B( ILO, ILO ), LDB, WORK )
      ATOL = MAX( SAFMIN, ULP*ANORM )
      BTOL = MAX( SAFMIN, ULP*BNORM )
      ASCALE = ONE / MAX( SAFMIN, ANORM )
      BSCALE = ONE / MAX( SAFMIN, BNORM )
*
*     Set Eigenvalues IHI+1:N
*
      DO 30 J = IHI + 1, N
         IF( B( J, J ).LT.ZERO ) THEN
            IF( ILSCHR ) THEN
               DO 10 JR = 1, J
                  A( JR, J ) = -A( JR, J )
                  B( JR, J ) = -B( JR, J )
   10          CONTINUE
            ELSE
               A( J, J ) = -A( J, J )
               B( J, J ) = -B( J, J )
            END IF
            IF( ILZ ) THEN
               DO 20 JR = 1, N
                  Z( JR, J ) = -Z( JR, J )
   20          CONTINUE
            END IF
         END IF
         ALPHAR( J ) = A( J, J )
         ALPHAI( J ) = ZERO
         BETA( J ) = B( J, J )
   30 CONTINUE
*
*     ---------------------- Begin Timing Code -------------------------
*     Count ops for norms, etc.
      OPST = ZERO
      OPS = OPS + DBLE( 2*N**2+6*N )
*     ----------------------- End Timing Code --------------------------
*
*
*     If IHI < ILO, skip QZ steps
*
      IF( IHI.LT.ILO )
     $   GO TO 380
*
*     MAIN QZ ITERATION LOOP
*
*     Initialize dynamic indices
*
*     Eigenvalues ILAST+1:N have been found.
*        Column operations modify rows IFRSTM:whatever.
*        Row operations modify columns whatever:ILASTM.
*
*     If only eigenvalues are being computed, then
*        IFRSTM is the row of the last splitting row above row ILAST;
*        this is always at least ILO.
*     IITER counts iterations since the last eigenvalue was found,
*        to tell when to use an extraordinary shift.
*     MAXIT is the maximum number of QZ sweeps allowed.
*
      ILAST = IHI
      IF( ILSCHR ) THEN
         IFRSTM = 1
         ILASTM = N
      ELSE
         IFRSTM = ILO
         ILASTM = IHI
      END IF
      IITER = 0
      ESHIFT = ZERO
      MAXIT = 30*( IHI-ILO+1 )
*
      DO 360 JITER = 1, MAXIT
*
*        Split the matrix if possible.
*
*        Two tests:
*           1: A(j,j-1)=0  or  j=ILO
*           2: B(j,j)=0
*
         IF( ILAST.EQ.ILO ) THEN
*
*           Special case: j=ILAST
*
            GO TO 80
         ELSE
            IF( ABS( A( ILAST, ILAST-1 ) ).LE.ATOL ) THEN
               A( ILAST, ILAST-1 ) = ZERO
               GO TO 80
            END IF
         END IF
*
         IF( ABS( B( ILAST, ILAST ) ).LE.BTOL ) THEN
            B( ILAST, ILAST ) = ZERO
            GO TO 70
         END IF
*
*        General case: j<ILAST
*
         DO 60 J = ILAST - 1, ILO, -1
*
*           Test 1: for A(j,j-1)=0 or j=ILO
*
            IF( J.EQ.ILO ) THEN
               ILAZRO = .TRUE.
            ELSE
               IF( ABS( A( J, J-1 ) ).LE.ATOL ) THEN
                  A( J, J-1 ) = ZERO
                  ILAZRO = .TRUE.
               ELSE
                  ILAZRO = .FALSE.
               END IF
            END IF
*
*           Test 2: for B(j,j)=0
*
            IF( ABS( B( J, J ) ).LT.BTOL ) THEN
               B( J, J ) = ZERO
*
*              Test 1a: Check for 2 consecutive small subdiagonals in A
*
               ILAZR2 = .FALSE.
               IF( .NOT.ILAZRO ) THEN
                  TEMP = ABS( A( J, J-1 ) )
                  TEMP2 = ABS( A( J, J ) )
                  TEMPR = MAX( TEMP, TEMP2 )
                  IF( TEMPR.LT.ONE .AND. TEMPR.NE.ZERO ) THEN
                     TEMP = TEMP / TEMPR
                     TEMP2 = TEMP2 / TEMPR
                  END IF
                  IF( TEMP*( ASCALE*ABS( A( J+1, J ) ) ).LE.TEMP2*
     $                ( ASCALE*ATOL ) )ILAZR2 = .TRUE.
               END IF
*
*              If both tests pass (1 & 2), i.e., the leading diagonal
*              element of B in the block is zero, split a 1x1 block off
*              at the top. (I.e., at the J-th row/column) The leading
*              diagonal element of the remainder can also be zero, so
*              this may have to be done repeatedly.
*
               IF( ILAZRO .OR. ILAZR2 ) THEN
                  DO 40 JCH = J, ILAST - 1
                     TEMP = A( JCH, JCH )
                     CALL DLARTG( TEMP, A( JCH+1, JCH ), C, S,
     $                            A( JCH, JCH ) )
                     A( JCH+1, JCH ) = ZERO
                     CALL DROT( ILASTM-JCH, A( JCH, JCH+1 ), LDA,
     $                          A( JCH+1, JCH+1 ), LDA, C, S )
                     CALL DROT( ILASTM-JCH, B( JCH, JCH+1 ), LDB,
     $                          B( JCH+1, JCH+1 ), LDB, C, S )
                     IF( ILQ )
     $                  CALL DROT( N, Q( 1, JCH ), 1, Q( 1, JCH+1 ), 1,
     $                             C, S )
                     IF( ILAZR2 )
     $                  A( JCH, JCH-1 ) = A( JCH, JCH-1 )*C
                     ILAZR2 = .FALSE.
*
*                    --------------- Begin Timing Code -----------------
                     OPST = OPST + DBLE( 7+12*( ILASTM-JCH )+6*NQ )
*                    ---------------- End Timing Code ------------------
*
                     IF( ABS( B( JCH+1, JCH+1 ) ).GE.BTOL ) THEN
                        IF( JCH+1.GE.ILAST ) THEN
                           GO TO 80
                        ELSE
                           IFIRST = JCH + 1
                           GO TO 110
                        END IF
                     END IF
                     B( JCH+1, JCH+1 ) = ZERO
   40             CONTINUE
                  GO TO 70
               ELSE
*
*                 Only test 2 passed -- chase the zero to B(ILAST,ILAST)
*                 Then process as in the case B(ILAST,ILAST)=0
*
                  DO 50 JCH = J, ILAST - 1
                     TEMP = B( JCH, JCH+1 )
                     CALL DLARTG( TEMP, B( JCH+1, JCH+1 ), C, S,
     $                            B( JCH, JCH+1 ) )
                     B( JCH+1, JCH+1 ) = ZERO
                     IF( JCH.LT.ILASTM-1 )
     $                  CALL DROT( ILASTM-JCH-1, B( JCH, JCH+2 ), LDB,
     $                             B( JCH+1, JCH+2 ), LDB, C, S )
                     CALL DROT( ILASTM-JCH+2, A( JCH, JCH-1 ), LDA,
     $                          A( JCH+1, JCH-1 ), LDA, C, S )
                     IF( ILQ )
     $                  CALL DROT( N, Q( 1, JCH ), 1, Q( 1, JCH+1 ), 1,
     $                             C, S )
                     TEMP = A( JCH+1, JCH )
                     CALL DLARTG( TEMP, A( JCH+1, JCH-1 ), C, S,
     $                            A( JCH+1, JCH ) )
                     A( JCH+1, JCH-1 ) = ZERO
                     CALL DROT( JCH+1-IFRSTM, A( IFRSTM, JCH ), 1,
     $                          A( IFRSTM, JCH-1 ), 1, C, S )
                     CALL DROT( JCH-IFRSTM, B( IFRSTM, JCH ), 1,
     $                          B( IFRSTM, JCH-1 ), 1, C, S )
                     IF( ILZ )
     $                  CALL DROT( N, Z( 1, JCH ), 1, Z( 1, JCH-1 ), 1,
     $                             C, S )
   50             CONTINUE
*
*                 ---------------- Begin Timing Code -------------------
                  OPST = OPST + DBLE( 26+12*( ILASTM-IFRSTM )+6*
     $                   ( NQ+NZ ) )*DBLE( ILAST-J )
*                 ----------------- End Timing Code --------------------
*
                  GO TO 70
               END IF
            ELSE IF( ILAZRO ) THEN
*
*              Only test 1 passed -- work on J:ILAST
*
               IFIRST = J
               GO TO 110
            END IF
*
*           Neither test passed -- try next J
*
   60    CONTINUE
*
*        (Drop-through is "impossible")
*
         INFO = N + 1
         GO TO 420
*
*        B(ILAST,ILAST)=0 -- clear A(ILAST,ILAST-1) to split off a
*        1x1 block.
*
   70    CONTINUE
         TEMP = A( ILAST, ILAST )
         CALL DLARTG( TEMP, A( ILAST, ILAST-1 ), C, S,
     $                A( ILAST, ILAST ) )
         A( ILAST, ILAST-1 ) = ZERO
         CALL DROT( ILAST-IFRSTM, A( IFRSTM, ILAST ), 1,
     $              A( IFRSTM, ILAST-1 ), 1, C, S )
         CALL DROT( ILAST-IFRSTM, B( IFRSTM, ILAST ), 1,
     $              B( IFRSTM, ILAST-1 ), 1, C, S )
         IF( ILZ )
     $      CALL DROT( N, Z( 1, ILAST ), 1, Z( 1, ILAST-1 ), 1, C, S )
*
*        --------------------- Begin Timing Code -----------------------
         OPST = OPST + DBLE( 7+12*( ILAST-IFRSTM )+6*NZ )
*        ---------------------- End Timing Code ------------------------
*
*
*        A(ILAST,ILAST-1)=0 -- Standardize B, set ALPHAR, ALPHAI,
*                              and BETA
*
   80    CONTINUE
         IF( B( ILAST, ILAST ).LT.ZERO ) THEN
            IF( ILSCHR ) THEN
               DO 90 J = IFRSTM, ILAST
                  A( J, ILAST ) = -A( J, ILAST )
                  B( J, ILAST ) = -B( J, ILAST )
   90          CONTINUE
            ELSE
               A( ILAST, ILAST ) = -A( ILAST, ILAST )
               B( ILAST, ILAST ) = -B( ILAST, ILAST )
            END IF
            IF( ILZ ) THEN
               DO 100 J = 1, N
                  Z( J, ILAST ) = -Z( J, ILAST )
  100          CONTINUE
            END IF
         END IF
         ALPHAR( ILAST ) = A( ILAST, ILAST )
         ALPHAI( ILAST ) = ZERO
         BETA( ILAST ) = B( ILAST, ILAST )
*
*        Go to next block -- exit if finished.
*
         ILAST = ILAST - 1
         IF( ILAST.LT.ILO )
     $      GO TO 380
*
*        Reset counters
*
         IITER = 0
         ESHIFT = ZERO
         IF( .NOT.ILSCHR ) THEN
            ILASTM = ILAST
            IF( IFRSTM.GT.ILAST )
     $         IFRSTM = ILO
         END IF
         GO TO 350
*
*        QZ step
*
*        This iteration only involves rows/columns IFIRST:ILAST. We
*        assume IFIRST < ILAST, and that the diagonal of B is non-zero.
*
  110    CONTINUE
         IITER = IITER + 1
         IF( .NOT.ILSCHR ) THEN
            IFRSTM = IFIRST
         END IF
*
*        Compute single shifts.
*
*        At this point, IFIRST < ILAST, and the diagonal elements of
*        B(IFIRST:ILAST,IFIRST,ILAST) are larger than BTOL (in
*        magnitude)
*
         IF( ( IITER / 10 )*10.EQ.IITER ) THEN
*
*           Exceptional shift.  Chosen for no particularly good reason.
*           (Single shift only.)
*
            IF( ( DBLE( MAXIT )*SAFMIN )*ABS( A( ILAST-1, ILAST ) ).LT.
     $          ABS( B( ILAST-1, ILAST-1 ) ) ) THEN
               ESHIFT = ESHIFT + A( ILAST-1, ILAST ) /
     $                  B( ILAST-1, ILAST-1 )
            ELSE
               ESHIFT = ESHIFT + ONE / ( SAFMIN*DBLE( MAXIT ) )
            END IF
            S1 = ONE
            WR = ESHIFT
*
*           ------------------- Begin Timing Code ----------------------
            OPST = OPST + DBLE( 4 )
*           -------------------- End Timing Code -----------------------
*
         ELSE
*
*           Shifts based on the generalized eigenvalues of the
*           bottom-right 2x2 block of A and B. The first eigenvalue
*           returned by DLAG2 is the Wilkinson shift (AEP p.512),
*
            CALL DLAG2( A( ILAST-1, ILAST-1 ), LDA,
     $                  B( ILAST-1, ILAST-1 ), LDB, SAFMIN*SAFETY, S1,
     $                  S2, WR, WR2, WI )
*
            TEMP = MAX( S1, SAFMIN*MAX( ONE, ABS( WR ), ABS( WI ) ) )
*
*           ------------------- Begin Timing Code ----------------------
            OPST = OPST + DBLE( 57 )
*           -------------------- End Timing Code -----------------------
*
            IF( WI.NE.ZERO )
     $         GO TO 200
         END IF
*
*        Fiddle with shift to avoid overflow
*
         TEMP = MIN( ASCALE, ONE )*( HALF*SAFMAX )
         IF( S1.GT.TEMP ) THEN
            SCALE = TEMP / S1
         ELSE
            SCALE = ONE
         END IF
*
         TEMP = MIN( BSCALE, ONE )*( HALF*SAFMAX )
         IF( ABS( WR ).GT.TEMP )
     $      SCALE = MIN( SCALE, TEMP / ABS( WR ) )
         S1 = SCALE*S1
         WR = SCALE*WR
*
*        Now check for two consecutive small subdiagonals.
*
         DO 120 J = ILAST - 1, IFIRST + 1, -1
            ISTART = J
            TEMP = ABS( S1*A( J, J-1 ) )
            TEMP2 = ABS( S1*A( J, J )-WR*B( J, J ) )
            TEMPR = MAX( TEMP, TEMP2 )
            IF( TEMPR.LT.ONE .AND. TEMPR.NE.ZERO ) THEN
               TEMP = TEMP / TEMPR
               TEMP2 = TEMP2 / TEMPR
            END IF
            IF( ABS( ( ASCALE*A( J+1, J ) )*TEMP ).LE.( ASCALE*ATOL )*
     $          TEMP2 )GO TO 130
  120    CONTINUE
*
         ISTART = IFIRST
  130    CONTINUE
*
*        Do an implicit single-shift QZ sweep.
*
*        Initial Q
*
         TEMP = S1*A( ISTART, ISTART ) - WR*B( ISTART, ISTART )
         TEMP2 = S1*A( ISTART+1, ISTART )
         CALL DLARTG( TEMP, TEMP2, C, S, TEMPR )
*
*        Sweep
*
         DO 190 J = ISTART, ILAST - 1
            IF( J.GT.ISTART ) THEN
               TEMP = A( J, J-1 )
               CALL DLARTG( TEMP, A( J+1, J-1 ), C, S, A( J, J-1 ) )
               A( J+1, J-1 ) = ZERO
            END IF
*
            DO 140 JC = J, ILASTM
               TEMP = C*A( J, JC ) + S*A( J+1, JC )
               A( J+1, JC ) = -S*A( J, JC ) + C*A( J+1, JC )
               A( J, JC ) = TEMP
               TEMP2 = C*B( J, JC ) + S*B( J+1, JC )
               B( J+1, JC ) = -S*B( J, JC ) + C*B( J+1, JC )
               B( J, JC ) = TEMP2
  140       CONTINUE
            IF( ILQ ) THEN
               DO 150 JR = 1, N
                  TEMP = C*Q( JR, J ) + S*Q( JR, J+1 )
                  Q( JR, J+1 ) = -S*Q( JR, J ) + C*Q( JR, J+1 )
                  Q( JR, J ) = TEMP
  150          CONTINUE
            END IF
*
            TEMP = B( J+1, J+1 )
            CALL DLARTG( TEMP, B( J+1, J ), C, S, B( J+1, J+1 ) )
            B( J+1, J ) = ZERO
*
            DO 160 JR = IFRSTM, MIN( J+2, ILAST )
               TEMP = C*A( JR, J+1 ) + S*A( JR, J )
               A( JR, J ) = -S*A( JR, J+1 ) + C*A( JR, J )
               A( JR, J+1 ) = TEMP
  160       CONTINUE
            DO 170 JR = IFRSTM, J
               TEMP = C*B( JR, J+1 ) + S*B( JR, J )
               B( JR, J ) = -S*B( JR, J+1 ) + C*B( JR, J )
               B( JR, J+1 ) = TEMP
  170       CONTINUE
            IF( ILZ ) THEN
               DO 180 JR = 1, N
                  TEMP = C*Z( JR, J+1 ) + S*Z( JR, J )
                  Z( JR, J ) = -S*Z( JR, J+1 ) + C*Z( JR, J )
                  Z( JR, J+1 ) = TEMP
  180          CONTINUE
            END IF
  190    CONTINUE
*
*        --------------------- Begin Timing Code -----------------------
         OPST = OPST + DBLE( 6+( ILAST-ISTART )*
     $          ( 8+14+36+12*( ILASTM-IFRSTM )+6*( NQ+NZ ) ) )
*        ---------------------- End Timing Code ------------------------
*
         GO TO 350
*
*        Use Francis double-shift
*
*        Note: the Francis double-shift should work with real shifts,
*              but only if the block is at least 3x3.
*              This code may break if this point is reached with
*              a 2x2 block with real eigenvalues.
*
  200    CONTINUE
         IF( IFIRST+1.EQ.ILAST ) THEN
*
*           Special case -- 2x2 block with complex eigenvectors
*
*           Step 1: Standardize, that is, rotate so that
*
*                       ( B11  0  )
*                   B = (         )  with B11 non-negative.
*                       (  0  B22 )
*
            CALL DLASV2( B( ILAST-1, ILAST-1 ), B( ILAST-1, ILAST ),
     $                   B( ILAST, ILAST ), B22, B11, SR, CR, SL, CL )
*
            IF( B11.LT.ZERO ) THEN
               CR = -CR
               SR = -SR
               B11 = -B11
               B22 = -B22
            END IF
*
            CALL DROT( ILASTM+1-IFIRST, A( ILAST-1, ILAST-1 ), LDA,
     $                 A( ILAST, ILAST-1 ), LDA, CL, SL )
            CALL DROT( ILAST+1-IFRSTM, A( IFRSTM, ILAST-1 ), 1,
     $                 A( IFRSTM, ILAST ), 1, CR, SR )
*
            IF( ILAST.LT.ILASTM )
     $         CALL DROT( ILASTM-ILAST, B( ILAST-1, ILAST+1 ), LDB,
     $                    B( ILAST, ILAST+1 ), LDA, CL, SL )
            IF( IFRSTM.LT.ILAST-1 )
     $         CALL DROT( IFIRST-IFRSTM, B( IFRSTM, ILAST-1 ), 1,
     $                    B( IFRSTM, ILAST ), 1, CR, SR )
*
            IF( ILQ )
     $         CALL DROT( N, Q( 1, ILAST-1 ), 1, Q( 1, ILAST ), 1, CL,
     $                    SL )
            IF( ILZ )
     $         CALL DROT( N, Z( 1, ILAST-1 ), 1, Z( 1, ILAST ), 1, CR,
     $                    SR )
*
            B( ILAST-1, ILAST-1 ) = B11
            B( ILAST-1, ILAST ) = ZERO
            B( ILAST, ILAST-1 ) = ZERO
            B( ILAST, ILAST ) = B22
*
*           If B22 is negative, negate column ILAST
*
            IF( B22.LT.ZERO ) THEN
               DO 210 J = IFRSTM, ILAST
                  A( J, ILAST ) = -A( J, ILAST )
                  B( J, ILAST ) = -B( J, ILAST )
  210          CONTINUE
*
               IF( ILZ ) THEN
                  DO 220 J = 1, N
                     Z( J, ILAST ) = -Z( J, ILAST )
  220             CONTINUE
               END IF
            END IF
*
*           Step 2: Compute ALPHAR, ALPHAI, and BETA (see refs.)
*
*           Recompute shift
*
            CALL DLAG2( A( ILAST-1, ILAST-1 ), LDA,
     $                  B( ILAST-1, ILAST-1 ), LDB, SAFMIN*SAFETY, S1,
     $                  TEMP, WR, TEMP2, WI )
*
*           ------------------- Begin Timing Code ----------------------
            OPST = OPST + DBLE( 103+12*( ILASTM+ILAST-IFIRST-IFRSTM )+6*
     $             ( NQ+NZ ) )
*           -------------------- End Timing Code -----------------------
*
*           If standardization has perturbed the shift onto real line,
*           do another (real single-shift) QR step.
*
            IF( WI.EQ.ZERO )
     $         GO TO 350
            S1INV = ONE / S1
*
*           Do EISPACK (QZVAL) computation of alpha and beta
*
            A11 = A( ILAST-1, ILAST-1 )
            A21 = A( ILAST, ILAST-1 )
            A12 = A( ILAST-1, ILAST )
            A22 = A( ILAST, ILAST )
*
*           Compute complex Givens rotation on right
*           (Assume some element of C = (sA - wB) > unfl )
*                            __
*           (sA - wB) ( CZ   -SZ )
*                     ( SZ    CZ )
*
            C11R = S1*A11 - WR*B11
            C11I = -WI*B11
            C12 = S1*A12
            C21 = S1*A21
            C22R = S1*A22 - WR*B22
            C22I = -WI*B22
*
            IF( ABS( C11R )+ABS( C11I )+ABS( C12 ).GT.ABS( C21 )+
     $          ABS( C22R )+ABS( C22I ) ) THEN
               T = DLAPY3( C12, C11R, C11I )
               CZ = C12 / T
               SZR = -C11R / T
               SZI = -C11I / T
            ELSE
               CZ = DLAPY2( C22R, C22I )
               IF( CZ.LE.SAFMIN ) THEN
                  CZ = ZERO
                  SZR = ONE
                  SZI = ZERO
               ELSE
                  TEMPR = C22R / CZ
                  TEMPI = C22I / CZ
                  T = DLAPY2( CZ, C21 )
                  CZ = CZ / T
                  SZR = -C21*TEMPR / T
                  SZI = C21*TEMPI / T
               END IF
            END IF
*
*           Compute Givens rotation on left
*
*           (  CQ   SQ )
*           (  __      )  A or B
*           ( -SQ   CQ )
*
            AN = ABS( A11 ) + ABS( A12 ) + ABS( A21 ) + ABS( A22 )
            BN = ABS( B11 ) + ABS( B22 )
            WABS = ABS( WR ) + ABS( WI )
            IF( S1*AN.GT.WABS*BN ) THEN
               CQ = CZ*B11
               SQR = SZR*B22
               SQI = -SZI*B22
            ELSE
               A1R = CZ*A11 + SZR*A12
               A1I = SZI*A12
               A2R = CZ*A21 + SZR*A22
               A2I = SZI*A22
               CQ = DLAPY2( A1R, A1I )
               IF( CQ.LE.SAFMIN ) THEN
                  CQ = ZERO
                  SQR = ONE
                  SQI = ZERO
               ELSE
                  TEMPR = A1R / CQ
                  TEMPI = A1I / CQ
                  SQR = TEMPR*A2R + TEMPI*A2I
                  SQI = TEMPI*A2R - TEMPR*A2I
               END IF
            END IF
            T = DLAPY3( CQ, SQR, SQI )
            CQ = CQ / T
            SQR = SQR / T
            SQI = SQI / T
*
*           Compute diagonal elements of QBZ
*
            TEMPR = SQR*SZR - SQI*SZI
            TEMPI = SQR*SZI + SQI*SZR
            B1R = CQ*CZ*B11 + TEMPR*B22
            B1I = TEMPI*B22
            B1A = DLAPY2( B1R, B1I )
            B2R = CQ*CZ*B22 + TEMPR*B11
            B2I = -TEMPI*B11
            B2A = DLAPY2( B2R, B2I )
*
*           Normalize so beta > 0, and Im( alpha1 ) > 0
*
            BETA( ILAST-1 ) = B1A
            BETA( ILAST ) = B2A
            ALPHAR( ILAST-1 ) = ( WR*B1A )*S1INV
            ALPHAI( ILAST-1 ) = ( WI*B1A )*S1INV
            ALPHAR( ILAST ) = ( WR*B2A )*S1INV
            ALPHAI( ILAST ) = -( WI*B2A )*S1INV
*
*           ------------------- Begin Timing Code ----------------------
            OPST = OPST + DBLE( 93 )
*           -------------------- End Timing Code -----------------------
*
*           Step 3: Go to next block -- exit if finished.
*
            ILAST = IFIRST - 1
            IF( ILAST.LT.ILO )
     $         GO TO 380
*
*           Reset counters
*
            IITER = 0
            ESHIFT = ZERO
            IF( .NOT.ILSCHR ) THEN
               ILASTM = ILAST
               IF( IFRSTM.GT.ILAST )
     $            IFRSTM = ILO
            END IF
            GO TO 350
         ELSE
*
*           Usual case: 3x3 or larger block, using Francis implicit
*                       double-shift
*
*                                    2
*           Eigenvalue equation is  w  - c w + d = 0,
*
*                                         -1 2        -1
*           so compute 1st column of  (A B  )  - c A B   + d
*           using the formula in QZIT (from EISPACK)
*
*           We assume that the block is at least 3x3
*
            AD11 = ( ASCALE*A( ILAST-1, ILAST-1 ) ) /
     $             ( BSCALE*B( ILAST-1, ILAST-1 ) )
            AD21 = ( ASCALE*A( ILAST, ILAST-1 ) ) /
     $             ( BSCALE*B( ILAST-1, ILAST-1 ) )
            AD12 = ( ASCALE*A( ILAST-1, ILAST ) ) /
     $             ( BSCALE*B( ILAST, ILAST ) )
            AD22 = ( ASCALE*A( ILAST, ILAST ) ) /
     $             ( BSCALE*B( ILAST, ILAST ) )
            U12 = B( ILAST-1, ILAST ) / B( ILAST, ILAST )
            AD11L = ( ASCALE*A( IFIRST, IFIRST ) ) /
     $              ( BSCALE*B( IFIRST, IFIRST ) )
            AD21L = ( ASCALE*A( IFIRST+1, IFIRST ) ) /
     $              ( BSCALE*B( IFIRST, IFIRST ) )
            AD12L = ( ASCALE*A( IFIRST, IFIRST+1 ) ) /
     $              ( BSCALE*B( IFIRST+1, IFIRST+1 ) )
            AD22L = ( ASCALE*A( IFIRST+1, IFIRST+1 ) ) /
     $              ( BSCALE*B( IFIRST+1, IFIRST+1 ) )
            AD32L = ( ASCALE*A( IFIRST+2, IFIRST+1 ) ) /
     $              ( BSCALE*B( IFIRST+1, IFIRST+1 ) )
            U12L = B( IFIRST, IFIRST+1 ) / B( IFIRST+1, IFIRST+1 )
*
            V( 1 ) = ( AD11-AD11L )*( AD22-AD11L ) - AD12*AD21 +
     $               AD21*U12*AD11L + ( AD12L-AD11L*U12L )*AD21L
            V( 2 ) = ( ( AD22L-AD11L )-AD21L*U12L-( AD11-AD11L )-
     $               ( AD22-AD11L )+AD21*U12 )*AD21L
            V( 3 ) = AD32L*AD21L
*
            ISTART = IFIRST
*
            CALL DLARFG( 3, V( 1 ), V( 2 ), 1, TAU )
            V( 1 ) = ONE
*
*           Sweep
*
            DO 290 J = ISTART, ILAST - 2
*
*              All but last elements: use 3x3 Householder transforms.
*
*              Zero (j-1)st column of A
*
               IF( J.GT.ISTART ) THEN
                  V( 1 ) = A( J, J-1 )
                  V( 2 ) = A( J+1, J-1 )
                  V( 3 ) = A( J+2, J-1 )
*
                  CALL DLARFG( 3, A( J, J-1 ), V( 2 ), 1, TAU )
                  V( 1 ) = ONE
                  A( J+1, J-1 ) = ZERO
                  A( J+2, J-1 ) = ZERO
               END IF
*
               DO 230 JC = J, ILASTM
                  TEMP = TAU*( A( J, JC )+V( 2 )*A( J+1, JC )+V( 3 )*
     $                   A( J+2, JC ) )
                  A( J, JC ) = A( J, JC ) - TEMP
                  A( J+1, JC ) = A( J+1, JC ) - TEMP*V( 2 )
                  A( J+2, JC ) = A( J+2, JC ) - TEMP*V( 3 )
                  TEMP2 = TAU*( B( J, JC )+V( 2 )*B( J+1, JC )+V( 3 )*
     $                    B( J+2, JC ) )
                  B( J, JC ) = B( J, JC ) - TEMP2
                  B( J+1, JC ) = B( J+1, JC ) - TEMP2*V( 2 )
                  B( J+2, JC ) = B( J+2, JC ) - TEMP2*V( 3 )
  230          CONTINUE
               IF( ILQ ) THEN
                  DO 240 JR = 1, N
                     TEMP = TAU*( Q( JR, J )+V( 2 )*Q( JR, J+1 )+V( 3 )*
     $                      Q( JR, J+2 ) )
                     Q( JR, J ) = Q( JR, J ) - TEMP
                     Q( JR, J+1 ) = Q( JR, J+1 ) - TEMP*V( 2 )
                     Q( JR, J+2 ) = Q( JR, J+2 ) - TEMP*V( 3 )
  240             CONTINUE
               END IF
*
*              Zero j-th column of B (see DLAGBC for details)
*
*              Swap rows to pivot
*
               ILPIVT = .FALSE.
               TEMP = MAX( ABS( B( J+1, J+1 ) ), ABS( B( J+1, J+2 ) ) )
               TEMP2 = MAX( ABS( B( J+2, J+1 ) ), ABS( B( J+2, J+2 ) ) )
               IF( MAX( TEMP, TEMP2 ).LT.SAFMIN ) THEN
                  SCALE = ZERO
                  U1 = ONE
                  U2 = ZERO
                  GO TO 250
               ELSE IF( TEMP.GE.TEMP2 ) THEN
                  W11 = B( J+1, J+1 )
                  W21 = B( J+2, J+1 )
                  W12 = B( J+1, J+2 )
                  W22 = B( J+2, J+2 )
                  U1 = B( J+1, J )
                  U2 = B( J+2, J )
               ELSE
                  W21 = B( J+1, J+1 )
                  W11 = B( J+2, J+1 )
                  W22 = B( J+1, J+2 )
                  W12 = B( J+2, J+2 )
                  U2 = B( J+1, J )
                  U1 = B( J+2, J )
               END IF
*
*              Swap columns if nec.
*
               IF( ABS( W12 ).GT.ABS( W11 ) ) THEN
                  ILPIVT = .TRUE.
                  TEMP = W12
                  TEMP2 = W22
                  W12 = W11
                  W22 = W21
                  W11 = TEMP
                  W21 = TEMP2
               END IF
*
*              LU-factor
*
               TEMP = W21 / W11
               U2 = U2 - TEMP*U1
               W22 = W22 - TEMP*W12
               W21 = ZERO
*
*              Compute SCALE
*
               SCALE = ONE
               IF( ABS( W22 ).LT.SAFMIN ) THEN
                  SCALE = ZERO
                  U2 = ONE
                  U1 = -W12 / W11
                  GO TO 250
               END IF
               IF( ABS( W22 ).LT.ABS( U2 ) )
     $            SCALE = ABS( W22 / U2 )
               IF( ABS( W11 ).LT.ABS( U1 ) )
     $            SCALE = MIN( SCALE, ABS( W11 / U1 ) )
*
*              Solve
*
               U2 = ( SCALE*U2 ) / W22
               U1 = ( SCALE*U1-W12*U2 ) / W11
*
  250          CONTINUE
               IF( ILPIVT ) THEN
                  TEMP = U2
                  U2 = U1
                  U1 = TEMP
               END IF
*
*              Compute Householder Vector
*
               T = SQRT( SCALE**2+U1**2+U2**2 )
               TAU = ONE + SCALE / T
               VS = -ONE / ( SCALE+T )
               V( 1 ) = ONE
               V( 2 ) = VS*U1
               V( 3 ) = VS*U2
*
*              Apply transformations from the right.
*
               DO 260 JR = IFRSTM, MIN( J+3, ILAST )
                  TEMP = TAU*( A( JR, J )+V( 2 )*A( JR, J+1 )+V( 3 )*
     $                   A( JR, J+2 ) )
                  A( JR, J ) = A( JR, J ) - TEMP
                  A( JR, J+1 ) = A( JR, J+1 ) - TEMP*V( 2 )
                  A( JR, J+2 ) = A( JR, J+2 ) - TEMP*V( 3 )
  260          CONTINUE
               DO 270 JR = IFRSTM, J + 2
                  TEMP = TAU*( B( JR, J )+V( 2 )*B( JR, J+1 )+V( 3 )*
     $                   B( JR, J+2 ) )
                  B( JR, J ) = B( JR, J ) - TEMP
                  B( JR, J+1 ) = B( JR, J+1 ) - TEMP*V( 2 )
                  B( JR, J+2 ) = B( JR, J+2 ) - TEMP*V( 3 )
  270          CONTINUE
               IF( ILZ ) THEN
                  DO 280 JR = 1, N
                     TEMP = TAU*( Z( JR, J )+V( 2 )*Z( JR, J+1 )+V( 3 )*
     $                      Z( JR, J+2 ) )
                     Z( JR, J ) = Z( JR, J ) - TEMP
                     Z( JR, J+1 ) = Z( JR, J+1 ) - TEMP*V( 2 )
                     Z( JR, J+2 ) = Z( JR, J+2 ) - TEMP*V( 3 )
  280             CONTINUE
               END IF
               B( J+1, J ) = ZERO
               B( J+2, J ) = ZERO
  290       CONTINUE
*
*           Last elements: Use Givens rotations
*
*           Rotations from the left
*
            J = ILAST - 1
            TEMP = A( J, J-1 )
            CALL DLARTG( TEMP, A( J+1, J-1 ), C, S, A( J, J-1 ) )
            A( J+1, J-1 ) = ZERO
*
            DO 300 JC = J, ILASTM
               TEMP = C*A( J, JC ) + S*A( J+1, JC )
               A( J+1, JC ) = -S*A( J, JC ) + C*A( J+1, JC )
               A( J, JC ) = TEMP
               TEMP2 = C*B( J, JC ) + S*B( J+1, JC )
               B( J+1, JC ) = -S*B( J, JC ) + C*B( J+1, JC )
               B( J, JC ) = TEMP2
  300       CONTINUE
            IF( ILQ ) THEN
               DO 310 JR = 1, N
                  TEMP = C*Q( JR, J ) + S*Q( JR, J+1 )
                  Q( JR, J+1 ) = -S*Q( JR, J ) + C*Q( JR, J+1 )
                  Q( JR, J ) = TEMP
  310          CONTINUE
            END IF
*
*           Rotations from the right.
*
            TEMP = B( J+1, J+1 )
            CALL DLARTG( TEMP, B( J+1, J ), C, S, B( J+1, J+1 ) )
            B( J+1, J ) = ZERO
*
            DO 320 JR = IFRSTM, ILAST
               TEMP = C*A( JR, J+1 ) + S*A( JR, J )
               A( JR, J ) = -S*A( JR, J+1 ) + C*A( JR, J )
               A( JR, J+1 ) = TEMP
  320       CONTINUE
            DO 330 JR = IFRSTM, ILAST - 1
               TEMP = C*B( JR, J+1 ) + S*B( JR, J )
               B( JR, J ) = -S*B( JR, J+1 ) + C*B( JR, J )
               B( JR, J+1 ) = TEMP
  330       CONTINUE
            IF( ILZ ) THEN
               DO 340 JR = 1, N
                  TEMP = C*Z( JR, J+1 ) + S*Z( JR, J )
                  Z( JR, J ) = -S*Z( JR, J+1 ) + C*Z( JR, J )
                  Z( JR, J+1 ) = TEMP
  340          CONTINUE
            END IF
*
*           ------------------- Begin Timing Code ----------------------
            OPST = OPST + ( DBLE( 14+30-10+52+12*( ILASTM-IFRSTM )+6*
     $             ( NQ+NZ ) )+DBLE( ILAST-1-ISTART )*
     $             DBLE( 14+24+90+20*( ILASTM-IFRSTM )+10*( NQ+NZ ) ) )
*           -------------------- End Timing Code -----------------------
*
*           End of Double-Shift code
*
         END IF
*
         GO TO 350
*
*        End of iteration loop
*
  350    CONTINUE
*        --------------------- Begin Timing Code -----------------------
         OPS = OPS + OPST
         OPST = ZERO
*        ---------------------- End Timing Code ------------------------
*
*
  360 CONTINUE
*
*     Drop-through = non-convergence
*
  370 CONTINUE
*     ---------------------- Begin Timing Code -------------------------
      OPS = OPS + OPST
      OPST = ZERO
*     ----------------------- End Timing Code --------------------------
*
      INFO = ILAST
      GO TO 420
*
*     Successful completion of all QZ steps
*
  380 CONTINUE
*
*     Set Eigenvalues 1:ILO-1
*
      DO 410 J = 1, ILO - 1
         IF( B( J, J ).LT.ZERO ) THEN
            IF( ILSCHR ) THEN
               DO 390 JR = 1, J
                  A( JR, J ) = -A( JR, J )
                  B( JR, J ) = -B( JR, J )
  390          CONTINUE
            ELSE
               A( J, J ) = -A( J, J )
               B( J, J ) = -B( J, J )
            END IF
            IF( ILZ ) THEN
               DO 400 JR = 1, N
                  Z( JR, J ) = -Z( JR, J )
  400          CONTINUE
            END IF
         END IF
         ALPHAR( J ) = A( J, J )
         ALPHAI( J ) = ZERO
         BETA( J ) = B( J, J )
  410 CONTINUE
*
*     Normal Termination
*
      INFO = 0
*
*     Exit (other than argument error) -- return optimal workspace size
*
  420 CONTINUE
*
*     ---------------------- Begin Timing Code -------------------------
      OPS = OPS + OPST
      OPST = ZERO
      ITCNT = JITER
*     ----------------------- End Timing Code --------------------------
*
      WORK( 1 ) = DBLE( N )
      RETURN
*
*     End of DHGEQZ
*
      END
      SUBROUTINE DHSEIN( SIDE, EIGSRC, INITV, SELECT, N, H, LDH, WR, WI,
     $                   VL, LDVL, VR, LDVR, MM, M, WORK, IFAILL,
     $                   IFAILR, INFO )
*
*  -- LAPACK routine (instrumented to count operations, version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     September 30, 1994
*
*     .. Scalar Arguments ..
      CHARACTER          EIGSRC, INITV, SIDE
      INTEGER            INFO, LDH, LDVL, LDVR, M, MM, N
*     ..
*     .. Array Arguments ..
      LOGICAL            SELECT( * )
      INTEGER            IFAILL( * ), IFAILR( * )
      DOUBLE PRECISION   H( LDH, * ), VL( LDVL, * ), VR( LDVR, * ),
     $                   WI( * ), WORK( * ), WR( * )
*     ..
*     Common block to return operation count.
*     .. Common blocks ..
      COMMON             / LATIME / OPS, ITCNT
*     ..
*     .. Scalars in Common ..
      DOUBLE PRECISION   ITCNT, OPS
*     ..
*
*  Purpose
*  =======
*
*  DHSEIN uses inverse iteration to find specified right and/or left
*  eigenvectors of a real upper Hessenberg matrix H.
*
*  The right eigenvector x and the left eigenvector y of the matrix H
*  corresponding to an eigenvalue w are defined by:
*
*               H * x = w * x,     y**h * H = w * y**h
*
*  where y**h denotes the conjugate transpose of the vector y.
*
*  Arguments
*  =========
*
*  SIDE    (input) CHARACTER*1
*          = 'R': compute right eigenvectors only;
*          = 'L': compute left eigenvectors only;
*          = 'B': compute both right and left eigenvectors.
*
*  EIGSRC  (input) CHARACTER*1
*          Specifies the source of eigenvalues supplied in (WR,WI):
*          = 'Q': the eigenvalues were found using DHSEQR; thus, if
*                 H has zero subdiagonal elements, and so is
*                 block-triangular, then the j-th eigenvalue can be
*                 assumed to be an eigenvalue of the block containing
*                 the j-th row/column.  This property allows DHSEIN to
*                 perform inverse iteration on just one diagonal block.
*          = 'N': no assumptions are made on the correspondence
*                 between eigenvalues and diagonal blocks.  In this
*                 case, DHSEIN must always perform inverse iteration
*                 using the whole matrix H.
*
*  INITV   (input) CHARACTER*1
*          = 'N': no initial vectors are supplied;
*          = 'U': user-supplied initial vectors are stored in the arrays
*                 VL and/or VR.
*
*  SELECT  (input/output) LOGICAL array, dimension(N)
*          Specifies the eigenvectors to be computed. To select the
*          real eigenvector corresponding to a real eigenvalue WR(j),
*          SELECT(j) must be set to .TRUE.. To select the complex
*          eigenvector corresponding to a complex eigenvalue
*          (WR(j),WI(j)), with complex conjugate (WR(j+1),WI(j+1)),
*          either SELECT(j) or SELECT(j+1) or both must be set to
*          .TRUE.; then on exit SELECT(j) is .TRUE. and SELECT(j+1) is
*          .FALSE..
*
*  N       (input) INTEGER
*          The order of the matrix H.  N >= 0.
*
*  H       (input) DOUBLE PRECISION array, dimension (LDH,N)
*          The upper Hessenberg matrix H.
*
*  LDH     (input) INTEGER
*          The leading dimension of the array H.  LDH >= max(1,N).
*
*  WR      (input/output) DOUBLE PRECISION array, dimension (N)
*  WI      (input) DOUBLE PRECISION array, dimension (N)
*          On entry, the real and imaginary parts of the eigenvalues of
*          H; a complex conjugate pair of eigenvalues must be stored in
*          consecutive elements of WR and WI.
*          On exit, WR may have been altered since close eigenvalues
*          are perturbed slightly in searching for independent
*          eigenvectors.
*
*  VL      (input/output) DOUBLE PRECISION array, dimension (LDVL,MM)
*          On entry, if INITV = 'U' and SIDE = 'L' or 'B', VL must
*          contain starting vectors for the inverse iteration for the
*          left eigenvectors; the starting vector for each eigenvector
*          must be in the same column(s) in which the eigenvector will
*          be stored.
*          On exit, if SIDE = 'L' or 'B', the left eigenvectors
*          specified by SELECT will be stored consecutively in the
*          columns of VL, in the same order as their eigenvalues. A
*          complex eigenvector corresponding to a complex eigenvalue is
*          stored in two consecutive columns, the first holding the real
*          part and the second the imaginary part.
*          If SIDE = 'R', VL is not referenced.
*
*  LDVL    (input) INTEGER
*          The leading dimension of the array VL.
*          LDVL >= max(1,N) if SIDE = 'L' or 'B'; LDVL >= 1 otherwise.
*
*  VR      (input/output) DOUBLE PRECISION array, dimension (LDVR,MM)
*          On entry, if INITV = 'U' and SIDE = 'R' or 'B', VR must
*          contain starting vectors for the inverse iteration for the
*          right eigenvectors; the starting vector for each eigenvector
*          must be in the same column(s) in which the eigenvector will
*          be stored.
*          On exit, if SIDE = 'R' or 'B', the right eigenvectors
*          specified by SELECT will be stored consecutively in the
*          columns of VR, in the same order as their eigenvalues. A
*          complex eigenvector corresponding to a complex eigenvalue is
*          stored in two consecutive columns, the first holding the real
*          part and the second the imaginary part.
*          If SIDE = 'L', VR is not referenced.
*
*  LDVR    (input) INTEGER
*          The leading dimension of the array VR.
*          LDVR >= max(1,N) if SIDE = 'R' or 'B'; LDVR >= 1 otherwise.
*
*  MM      (input) INTEGER
*          The number of columns in the arrays VL and/or VR. MM >= M.
*
*  M       (output) INTEGER
*          The number of columns in the arrays VL and/or VR required to
*          store the eigenvectors; each selected real eigenvector
*          occupies one column and each selected complex eigenvector
*          occupies two columns.
*
*  WORK    (workspace) DOUBLE PRECISION array, dimension ((N+2)*N)
*
*  IFAILL  (output) INTEGER array, dimension (MM)
*          If SIDE = 'L' or 'B', IFAILL(i) = j > 0 if the left
*          eigenvector in the i-th column of VL (corresponding to the
*          eigenvalue w(j)) failed to converge; IFAILL(i) = 0 if the
*          eigenvector converged satisfactorily. If the i-th and (i+1)th
*          columns of VL hold a complex eigenvector, then IFAILL(i) and
*          IFAILL(i+1) are set to the same value.
*          If SIDE = 'R', IFAILL is not referenced.
*
*  IFAILR  (output) INTEGER array, dimension (MM)
*          If SIDE = 'R' or 'B', IFAILR(i) = j > 0 if the right
*          eigenvector in the i-th column of VR (corresponding to the
*          eigenvalue w(j)) failed to converge; IFAILR(i) = 0 if the
*          eigenvector converged satisfactorily. If the i-th and (i+1)th
*          columns of VR hold a complex eigenvector, then IFAILR(i) and
*          IFAILR(i+1) are set to the same value.
*          If SIDE = 'L', IFAILR is not referenced.
*
*  INFO    (output) INTEGER
*          = 0:  successful exit
*          < 0:  if INFO = -i, the i-th argument had an illegal value
*          > 0:  if INFO = i, i is the number of eigenvectors which
*                failed to converge; see IFAILL and IFAILR for further
*                details.
*
*  Further Details
*  ===============
*
*  Each eigenvector is normalized so that the element of largest
*  magnitude has magnitude 1; here the magnitude of a complex number
*  (x,y) is taken to be |x|+|y|.
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE
      PARAMETER          ( ZERO = 0.0D+0, ONE = 1.0D+0 )
*     ..
*     .. Local Scalars ..
      LOGICAL            BOTHV, FROMQR, LEFTV, NOINIT, PAIR, RIGHTV
      INTEGER            I, IINFO, K, KL, KLN, KR, KSI, KSR, LDWORK
      DOUBLE PRECISION   BIGNUM, EPS3, HNORM, OPST, SMLNUM, ULP, UNFL,
     $                   WKI, WKR
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      DOUBLE PRECISION   DLAMCH, DLANHS
      EXTERNAL           LSAME, DLAMCH, DLANHS
*     ..
*     .. External Subroutines ..
      EXTERNAL           DLAEIN, XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, MAX
*     ..
*     .. Executable Statements ..
*
*     Decode and test the input parameters.
*
      BOTHV = LSAME( SIDE, 'B' )
      RIGHTV = LSAME( SIDE, 'R' ) .OR. BOTHV
      LEFTV = LSAME( SIDE, 'L' ) .OR. BOTHV
*
      FROMQR = LSAME( EIGSRC, 'Q' )
*
      NOINIT = LSAME( INITV, 'N' )
*
*     Set M to the number of columns required to store the selected
*     eigenvectors, and standardize the array SELECT.
*
      M = 0
      PAIR = .FALSE.
      DO 10 K = 1, N
         IF( PAIR ) THEN
            PAIR = .FALSE.
            SELECT( K ) = .FALSE.
         ELSE
            IF( WI( K ).EQ.ZERO ) THEN
               IF( SELECT( K ) )
     $            M = M + 1
            ELSE
               PAIR = .TRUE.
               IF( SELECT( K ) .OR. SELECT( K+1 ) ) THEN
                  SELECT( K ) = .TRUE.
                  M = M + 2
               END IF
            END IF
         END IF
   10 CONTINUE
*
      INFO = 0
      IF( .NOT.RIGHTV .AND. .NOT.LEFTV ) THEN
         INFO = -1
      ELSE IF( .NOT.FROMQR .AND. .NOT.LSAME( EIGSRC, 'N' ) ) THEN
         INFO = -2
      ELSE IF( .NOT.NOINIT .AND. .NOT.LSAME( INITV, 'U' ) ) THEN
         INFO = -3
      ELSE IF( N.LT.0 ) THEN
         INFO = -5
      ELSE IF( LDH.LT.MAX( 1, N ) ) THEN
         INFO = -7
      ELSE IF( LDVL.LT.1 .OR. ( LEFTV .AND. LDVL.LT.N ) ) THEN
         INFO = -11
      ELSE IF( LDVR.LT.1 .OR. ( RIGHTV .AND. LDVR.LT.N ) ) THEN
         INFO = -13
      ELSE IF( MM.LT.M ) THEN
         INFO = -14
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DHSEIN', -INFO )
         RETURN
      END IF
***
*     Initialize
      OPST = 0
***
*
*     Quick return if possible.
*
      IF( N.EQ.0 )
     $   RETURN
*
*     Set machine-dependent constants.
*
      UNFL = DLAMCH( 'Safe minimum' )
      ULP = DLAMCH( 'Precision' )
      SMLNUM = UNFL*( N / ULP )
      BIGNUM = ( ONE-ULP ) / SMLNUM
*
      LDWORK = N + 1
*
      KL = 1
      KLN = 0
      IF( FROMQR ) THEN
         KR = 0
      ELSE
         KR = N
      END IF
      KSR = 1
*
      DO 120 K = 1, N
         IF( SELECT( K ) ) THEN
*
*           Compute eigenvector(s) corresponding to W(K).
*
            IF( FROMQR ) THEN
*
*              If affiliation of eigenvalues is known, check whether
*              the matrix splits.
*
*              Determine KL and KR such that 1 <= KL <= K <= KR <= N
*              and H(KL,KL-1) and H(KR+1,KR) are zero (or KL = 1 or
*              KR = N).
*
*              Then inverse iteration can be performed with the
*              submatrix H(KL:N,KL:N) for a left eigenvector, and with
*              the submatrix H(1:KR,1:KR) for a right eigenvector.
*
               DO 20 I = K, KL + 1, -1
                  IF( H( I, I-1 ).EQ.ZERO )
     $               GO TO 30
   20          CONTINUE
   30          CONTINUE
               KL = I
               IF( K.GT.KR ) THEN
                  DO 40 I = K, N - 1
                     IF( H( I+1, I ).EQ.ZERO )
     $                  GO TO 50
   40             CONTINUE
   50             CONTINUE
                  KR = I
               END IF
            END IF
*
            IF( KL.NE.KLN ) THEN
               KLN = KL
*
*              Compute infinity-norm of submatrix H(KL:KR,KL:KR) if it
*              has not ben computed before.
*
               HNORM = DLANHS( 'I', KR-KL+1, H( KL, KL ), LDH, WORK )
***
*     Increment opcount for computing the norm of matrix
               OPS = OPS + N*( N+1 ) / 2
***
               IF( HNORM.GT.ZERO ) THEN
                  EPS3 = HNORM*ULP
               ELSE
                  EPS3 = SMLNUM
               END IF
            END IF
*
*           Perturb eigenvalue if it is close to any previous
*           selected eigenvalues affiliated to the submatrix
*           H(KL:KR,KL:KR). Close roots are modified by EPS3.
*
            WKR = WR( K )
            WKI = WI( K )
   60       CONTINUE
            DO 70 I = K - 1, KL, -1
               IF( SELECT( I ) .AND. ABS( WR( I )-WKR )+
     $             ABS( WI( I )-WKI ).LT.EPS3 ) THEN
                  WKR = WKR + EPS3
                  GO TO 60
               END IF
   70       CONTINUE
            WR( K ) = WKR
***
*        Increment opcount for loop 70
            OPST = OPST + 2*( K-KL )
**
*
            PAIR = WKI.NE.ZERO
            IF( PAIR ) THEN
               KSI = KSR + 1
            ELSE
               KSI = KSR
            END IF
            IF( LEFTV ) THEN
*
*              Compute left eigenvector.
*
               CALL DLAEIN( .FALSE., NOINIT, N-KL+1, H( KL, KL ), LDH,
     $                      WKR, WKI, VL( KL, KSR ), VL( KL, KSI ),
     $                      WORK, LDWORK, WORK( N*N+N+1 ), EPS3, SMLNUM,
     $                      BIGNUM, IINFO )
               IF( IINFO.GT.0 ) THEN
                  IF( PAIR ) THEN
                     INFO = INFO + 2
                  ELSE
                     INFO = INFO + 1
                  END IF
                  IFAILL( KSR ) = K
                  IFAILL( KSI ) = K
               ELSE
                  IFAILL( KSR ) = 0
                  IFAILL( KSI ) = 0
               END IF
               DO 80 I = 1, KL - 1
                  VL( I, KSR ) = ZERO
   80          CONTINUE
               IF( PAIR ) THEN
                  DO 90 I = 1, KL - 1
                     VL( I, KSI ) = ZERO
   90             CONTINUE
               END IF
            END IF
            IF( RIGHTV ) THEN
*
*              Compute right eigenvector.
*
               CALL DLAEIN( .TRUE., NOINIT, KR, H, LDH, WKR, WKI,
     $                      VR( 1, KSR ), VR( 1, KSI ), WORK, LDWORK,
     $                      WORK( N*N+N+1 ), EPS3, SMLNUM, BIGNUM,
     $                      IINFO )
               IF( IINFO.GT.0 ) THEN
                  IF( PAIR ) THEN
                     INFO = INFO + 2
                  ELSE
                     INFO = INFO + 1
                  END IF
                  IFAILR( KSR ) = K
                  IFAILR( KSI ) = K
               ELSE
                  IFAILR( KSR ) = 0
                  IFAILR( KSI ) = 0
               END IF
               DO 100 I = KR + 1, N
                  VR( I, KSR ) = ZERO
  100          CONTINUE
               IF( PAIR ) THEN
                  DO 110 I = KR + 1, N
                     VR( I, KSI ) = ZERO
  110             CONTINUE
               END IF
            END IF
*
            IF( PAIR ) THEN
               KSR = KSR + 2
            ELSE
               KSR = KSR + 1
            END IF
         END IF
  120 CONTINUE
*
***
*     Compute final op count
      OPS = OPS + OPST
***
      RETURN
*
*     End of DHSEIN
*
      END
      SUBROUTINE DHSEQR( JOB, COMPZ, N, ILO, IHI, H, LDH, WR, WI, Z,
     $                   LDZ, WORK, LWORK, INFO )
*
*  -- LAPACK routine (instrumented to count operations, version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     June 30, 1999
*
*     .. Scalar Arguments ..
      CHARACTER          COMPZ, JOB
      INTEGER            IHI, ILO, INFO, LDH, LDZ, LWORK, N
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   H( LDH, * ), WI( * ), WORK( * ), WR( * ),
     $                   Z( LDZ, * )
*     ..
*     Common block to return operation count.
*     .. Common blocks ..
      COMMON             / LATIME / OPS, ITCNT
*     ..
*     .. Scalars in Common ..
      DOUBLE PRECISION   ITCNT, OPS
*     ..
*
*  Purpose
*  =======
*
*  DHSEQR computes the eigenvalues of a real upper Hessenberg matrix H
*  and, optionally, the matrices T and Z from the Schur decomposition
*  H = Z T Z**T, where T is an upper quasi-triangular matrix (the Schur
*  form), and Z is the orthogonal matrix of Schur vectors.
*
*  Optionally Z may be postmultiplied into an input orthogonal matrix Q,
*  so that this routine can give the Schur factorization of a matrix A
*  which has been reduced to the Hessenberg form H by the orthogonal
*  matrix Q:  A = Q*H*Q**T = (QZ)*T*(QZ)**T.
*
*  Arguments
*  =========
*
*  JOB     (input) CHARACTER*1
*          = 'E':  compute eigenvalues only;
*          = 'S':  compute eigenvalues and the Schur form T.
*
*  COMPZ   (input) CHARACTER*1
*          = 'N':  no Schur vectors are computed;
*          = 'I':  Z is initialized to the unit matrix and the matrix Z
*                  of Schur vectors of H is returned;
*          = 'V':  Z must contain an orthogonal matrix Q on entry, and
*                  the product Q*Z is returned.
*
*  N       (input) INTEGER
*          The order of the matrix H.  N >= 0.
*
*  ILO     (input) INTEGER
*  IHI     (input) INTEGER
*          It is assumed that H is already upper triangular in rows
*          and columns 1:ILO-1 and IHI+1:N. ILO and IHI are normally
*          set by a previous call to DGEBAL, and then passed to SGEHRD
*          when the matrix output by DGEBAL is reduced to Hessenberg
*          form. Otherwise ILO and IHI should be set to 1 and N
*          respectively.
*          1 <= ILO <= IHI <= N, if N > 0; ILO=1 and IHI=0, if N=0.
*
*  H       (input/output) DOUBLE PRECISION array, dimension (LDH,N)
*          On entry, the upper Hessenberg matrix H.
*          On exit, if JOB = 'S', H contains the upper quasi-triangular
*          matrix T from the Schur decomposition (the Schur form);
*          2-by-2 diagonal blocks (corresponding to complex conjugate
*          pairs of eigenvalues) are returned in standard form, with
*          H(i,i) = H(i+1,i+1) and H(i+1,i)*H(i,i+1) < 0. If JOB = 'E',
*          the contents of H are unspecified on exit.
*
*  LDH     (input) INTEGER
*          The leading dimension of the array H. LDH >= max(1,N).
*
*  WR      (output) DOUBLE PRECISION array, dimension (N)
*  WI      (output) DOUBLE PRECISION array, dimension (N)
*          The real and imaginary parts, respectively, of the computed
*          eigenvalues. If two eigenvalues are computed as a complex
*          conjugate pair, they are stored in consecutive elements of
*          WR and WI, say the i-th and (i+1)th, with WI(i) > 0 and
*          WI(i+1) < 0. If JOB = 'S', the eigenvalues are stored in the
*          same order as on the diagonal of the Schur form returned in
*          H, with WR(i) = H(i,i) and, if H(i:i+1,i:i+1) is a 2-by-2
*          diagonal block, WI(i) = sqrt(H(i+1,i)*H(i,i+1)) and
*          WI(i+1) = -WI(i).
*
*  Z       (input/output) DOUBLE PRECISION array, dimension (LDZ,N)
*          If COMPZ = 'N': Z is not referenced.
*          If COMPZ = 'I': on entry, Z need not be set, and on exit, Z
*          contains the orthogonal matrix Z of the Schur vectors of H.
*          If COMPZ = 'V': on entry Z must contain an N-by-N matrix Q,
*          which is assumed to be equal to the unit matrix except for
*          the submatrix Z(ILO:IHI,ILO:IHI); on exit Z contains Q*Z.
*          Normally Q is the orthogonal matrix generated by DORGHR after
*          the call to DGEHRD which formed the Hessenberg matrix H.
*
*  LDZ     (input) INTEGER
*          The leading dimension of the array Z.
*          LDZ >= max(1,N) if COMPZ = 'I' or 'V'; LDZ >= 1 otherwise.
*
*  WORK    (workspace/output) DOUBLE PRECISION array, dimension (LWORK)
*          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
*
*  LWORK   (input) INTEGER
*          The dimension of the array WORK.  LWORK >= max(1,N).
*
*          If LWORK = -1, then a workspace query is assumed; the routine
*          only calculates the optimal size of the WORK array, returns
*          this value as the first entry of the WORK array, and no error
*          message related to LWORK is issued by XERBLA.
*
*  INFO    (output) INTEGER
*          = 0:  successful exit
*          < 0:  if INFO = -i, the i-th argument had an illegal value
*          > 0:  if INFO = i, DHSEQR failed to compute all of the
*                eigenvalues in a total of 30*(IHI-ILO+1) iterations;
*                elements 1:ilo-1 and i+1:n of WR and WI contain those
*                eigenvalues which have been successfully computed.
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE, TWO
      PARAMETER          ( ZERO = 0.0D+0, ONE = 1.0D+0, TWO = 2.0D+0 )
      DOUBLE PRECISION   CONST
      PARAMETER          ( CONST = 1.5D+0 )
      INTEGER            NSMAX, LDS
      PARAMETER          ( NSMAX = 15, LDS = NSMAX )
*     ..
*     .. Local Scalars ..
      LOGICAL            INITZ, LQUERY, WANTT, WANTZ
      INTEGER            I, I1, I2, IERR, II, ITEMP, ITN, ITS, J, K, L,
     $                   MAXB, NH, NR, NS, NV
      DOUBLE PRECISION   ABSW, OPST, OVFL, SMLNUM, TAU, TEMP, TST1, ULP,
     $                   UNFL
*     ..
*     .. Local Arrays ..
      DOUBLE PRECISION   S( LDS, NSMAX ), V( NSMAX+1 ), VV( NSMAX+1 )
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      INTEGER            IDAMAX, ILAENV
      DOUBLE PRECISION   DLAMCH, DLANHS, DLAPY2
      EXTERNAL           LSAME, IDAMAX, ILAENV, DLAMCH, DLANHS, DLAPY2
*     ..
*     .. External Subroutines ..
      EXTERNAL           DCOPY, DGEMV, DLABAD, DLACPY, DLAHQR, DLARFG,
     $                   DLARFX, DLASET, DSCAL, XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, MAX, MIN
*     ..
*     .. Executable Statements ..
*
*     Decode and test the input parameters
*
      WANTT = LSAME( JOB, 'S' )
      INITZ = LSAME( COMPZ, 'I' )
      WANTZ = INITZ .OR. LSAME( COMPZ, 'V' )
*
      INFO = 0
      WORK( 1 ) = MAX( 1, N )
      LQUERY = ( LWORK.EQ.-1 )
      IF( .NOT.LSAME( JOB, 'E' ) .AND. .NOT.WANTT ) THEN
         INFO = -1
      ELSE IF( .NOT.LSAME( COMPZ, 'N' ) .AND. .NOT.WANTZ ) THEN
         INFO = -2
      ELSE IF( N.LT.0 ) THEN
         INFO = -3
      ELSE IF( ILO.LT.1 .OR. ILO.GT.MAX( 1, N ) ) THEN
         INFO = -4
      ELSE IF( IHI.LT.MIN( ILO, N ) .OR. IHI.GT.N ) THEN
         INFO = -5
      ELSE IF( LDH.LT.MAX( 1, N ) ) THEN
         INFO = -7
      ELSE IF( LDZ.LT.1 .OR. WANTZ .AND. LDZ.LT.MAX( 1, N ) ) THEN
         INFO = -11
      ELSE IF( LWORK.LT.MAX( 1, N ) .AND. .NOT.LQUERY ) THEN
         INFO = -13
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DHSEQR', -INFO )
         RETURN
      ELSE IF( LQUERY ) THEN
         RETURN
      END IF
***
*     Initialize
      OPST = 0
***
*
*     Initialize Z, if necessary
*
      IF( INITZ )
     $   CALL DLASET( 'Full', N, N, ZERO, ONE, Z, LDZ )
*
*     Store the eigenvalues isolated by DGEBAL.
*
      DO 10 I = 1, ILO - 1
         WR( I ) = H( I, I )
         WI( I ) = ZERO
   10 CONTINUE
      DO 20 I = IHI + 1, N
         WR( I ) = H( I, I )
         WI( I ) = ZERO
   20 CONTINUE
*
*     Quick return if possible.
*
      IF( N.EQ.0 )
     $   RETURN
      IF( ILO.EQ.IHI ) THEN
         WR( ILO ) = H( ILO, ILO )
         WI( ILO ) = ZERO
         RETURN
      END IF
*
*     Set rows and columns ILO to IHI to zero below the first
*     subdiagonal.
*
      DO 40 J = ILO, IHI - 2
         DO 30 I = J + 2, N
            H( I, J ) = ZERO
   30    CONTINUE
   40 CONTINUE
      NH = IHI - ILO + 1
*
*     Determine the order of the multi-shift QR algorithm to be used.
*
      NS = ILAENV( 4, 'DHSEQR', JOB // COMPZ, N, ILO, IHI, -1 )
      MAXB = ILAENV( 8, 'DHSEQR', JOB // COMPZ, N, ILO, IHI, -1 )
      IF( NS.LE.2 .OR. NS.GT.NH .OR. MAXB.GE.NH ) THEN
*
*        Use the standard double-shift algorithm
*
         CALL DLAHQR( WANTT, WANTZ, N, ILO, IHI, H, LDH, WR, WI, ILO,
     $                IHI, Z, LDZ, INFO )
         RETURN
      END IF
      MAXB = MAX( 3, MAXB )
      NS = MIN( NS, MAXB, NSMAX )
*
*     Now 2 < NS <= MAXB < NH.
*
*     Set machine-dependent constants for the stopping criterion.
*     If norm(H) <= sqrt(OVFL), overflow should not occur.
*
      UNFL = DLAMCH( 'Safe minimum' )
      OVFL = ONE / UNFL
      CALL DLABAD( UNFL, OVFL )
      ULP = DLAMCH( 'Precision' )
      SMLNUM = UNFL*( NH / ULP )
*
*     I1 and I2 are the indices of the first row and last column of H
*     to which transformations must be applied. If eigenvalues only are
*     being computed, I1 and I2 are set inside the main loop.
*
      IF( WANTT ) THEN
         I1 = 1
         I2 = N
      END IF
*
*     ITN is the total number of multiple-shift QR iterations allowed.
*
      ITN = 30*NH
*
*     The main loop begins here. I is the loop index and decreases from
*     IHI to ILO in steps of at most MAXB. Each iteration of the loop
*     works with the active submatrix in rows and columns L to I.
*     Eigenvalues I+1 to IHI have already converged. Either L = ILO or
*     H(L,L-1) is negligible so that the matrix splits.
*
      I = IHI
   50 CONTINUE
      L = ILO
      IF( I.LT.ILO )
     $   GO TO 170
*
*     Perform multiple-shift QR iterations on rows and columns ILO to I
*     until a submatrix of order at most MAXB splits off at the bottom
*     because a subdiagonal element has become negligible.
*
      DO 150 ITS = 0, ITN
*
*        Look for a single small subdiagonal element.
*
         DO 60 K = I, L + 1, -1
            TST1 = ABS( H( K-1, K-1 ) ) + ABS( H( K, K ) )
            IF( TST1.EQ.ZERO ) THEN
               TST1 = DLANHS( '1', I-L+1, H( L, L ), LDH, WORK )
***
*              Increment op count
               OPS = OPS + ( I-L+1 )*( I-L+2 ) / 2
***
            END IF
            IF( ABS( H( K, K-1 ) ).LE.MAX( ULP*TST1, SMLNUM ) )
     $         GO TO 70
   60    CONTINUE
   70    CONTINUE
         L = K
***
*        Increment op count
         OPST = OPST + 3*( I-L+1 )
***
         IF( L.GT.ILO ) THEN
*
*           H(L,L-1) is negligible.
*
            H( L, L-1 ) = ZERO
         END IF
*
*        Exit from loop if a submatrix of order <= MAXB has split off.
*
         IF( L.GE.I-MAXB+1 )
     $      GO TO 160
*
*        Now the active submatrix is in rows and columns L to I. If
*        eigenvalues only are being computed, only the active submatrix
*        need be transformed.
*
         IF( .NOT.WANTT ) THEN
            I1 = L
            I2 = I
         END IF
*
         IF( ITS.EQ.20 .OR. ITS.EQ.30 ) THEN
*
*           Exceptional shifts.
*
            DO 80 II = I - NS + 1, I
               WR( II ) = CONST*( ABS( H( II, II-1 ) )+
     $                    ABS( H( II, II ) ) )
               WI( II ) = ZERO
   80       CONTINUE
***
*           Increment op count
            OPST = OPST + 2*NS
***
         ELSE
*
*           Use eigenvalues of trailing submatrix of order NS as shifts.
*
            CALL DLACPY( 'Full', NS, NS, H( I-NS+1, I-NS+1 ), LDH, S,
     $                   LDS )
            CALL DLAHQR( .FALSE., .FALSE., NS, 1, NS, S, LDS,
     $                   WR( I-NS+1 ), WI( I-NS+1 ), 1, NS, Z, LDZ,
     $                   IERR )
            IF( IERR.GT.0 ) THEN
*
*              If DLAHQR failed to compute all NS eigenvalues, use the
*              unconverged diagonal elements as the remaining shifts.
*
               DO 90 II = 1, IERR
                  WR( I-NS+II ) = S( II, II )
                  WI( I-NS+II ) = ZERO
   90          CONTINUE
            END IF
         END IF
*
*        Form the first column of (G-w(1)) (G-w(2)) . . . (G-w(ns))
*        where G is the Hessenberg submatrix H(L:I,L:I) and w is
*        the vector of shifts (stored in WR and WI). The result is
*        stored in the local array V.
*
         V( 1 ) = ONE
         DO 100 II = 2, NS + 1
            V( II ) = ZERO
  100    CONTINUE
         NV = 1
         DO 120 J = I - NS + 1, I
            IF( WI( J ).GE.ZERO ) THEN
               IF( WI( J ).EQ.ZERO ) THEN
*
*                 real shift
*
                  CALL DCOPY( NV+1, V, 1, VV, 1 )
                  CALL DGEMV( 'No transpose', NV+1, NV, ONE, H( L, L ),
     $                        LDH, VV, 1, -WR( J ), V, 1 )
                  NV = NV + 1
***
*                 Increment op count
                  OPST = OPST + 2*NV*( NV+1 ) + NV + 1
***
               ELSE IF( WI( J ).GT.ZERO ) THEN
*
*                 complex conjugate pair of shifts
*
                  CALL DCOPY( NV+1, V, 1, VV, 1 )
                  CALL DGEMV( 'No transpose', NV+1, NV, ONE, H( L, L ),
     $                        LDH, V, 1, -TWO*WR( J ), VV, 1 )
                  ITEMP = IDAMAX( NV+1, VV, 1 )
                  TEMP = ONE / MAX( ABS( VV( ITEMP ) ), SMLNUM )
                  CALL DSCAL( NV+1, TEMP, VV, 1 )
                  ABSW = DLAPY2( WR( J ), WI( J ) )
                  TEMP = ( TEMP*ABSW )*ABSW
                  CALL DGEMV( 'No transpose', NV+2, NV+1, ONE,
     $                        H( L, L ), LDH, VV, 1, TEMP, V, 1 )
                  NV = NV + 2
***
*                 Increment op count
                  OPST = OPST + 4*( NV+1 )**2 + 4*NV + 9
***
               END IF
*
*              Scale V(1:NV) so that max(abs(V(i))) = 1. If V is zero,
*              reset it to the unit vector.
*
               ITEMP = IDAMAX( NV, V, 1 )
***
*              Increment op count
               OPST = OPST + NV
***
               TEMP = ABS( V( ITEMP ) )
               IF( TEMP.EQ.ZERO ) THEN
                  V( 1 ) = ONE
                  DO 110 II = 2, NV
                     V( II ) = ZERO
  110             CONTINUE
               ELSE
                  TEMP = MAX( TEMP, SMLNUM )
                  CALL DSCAL( NV, ONE / TEMP, V, 1 )
***
*                 Increment op count
                  OPST = OPST + NV
***
               END IF
            END IF
  120    CONTINUE
*
*        Multiple-shift QR step
*
         DO 140 K = L, I - 1
*
*           The first iteration of this loop determines a reflection G
*           from the vector V and applies it from left and right to H,
*           thus creating a nonzero bulge below the subdiagonal.
*
*           Each subsequent iteration determines a reflection G to
*           restore the Hessenberg form in the (K-1)th column, and thus
*           chases the bulge one step toward the bottom of the active
*           submatrix. NR is the order of G.
*
            NR = MIN( NS+1, I-K+1 )
            IF( K.GT.L )
     $         CALL DCOPY( NR, H( K, K-1 ), 1, V, 1 )
            CALL DLARFG( NR, V( 1 ), V( 2 ), 1, TAU )
***
*           Increment op count
            OPST = OPST + 3*NR + 9
***
            IF( K.GT.L ) THEN
               H( K, K-1 ) = V( 1 )
               DO 130 II = K + 1, I
                  H( II, K-1 ) = ZERO
  130          CONTINUE
            END IF
            V( 1 ) = ONE
*
*           Apply G from the left to transform the rows of the matrix in
*           columns K to I2.
*
            CALL DLARFX( 'Left', NR, I2-K+1, V, TAU, H( K, K ), LDH,
     $                   WORK )
*
*           Apply G from the right to transform the columns of the
*           matrix in rows I1 to min(K+NR,I).
*
            CALL DLARFX( 'Right', MIN( K+NR, I )-I1+1, NR, V, TAU,
     $                   H( I1, K ), LDH, WORK )
***
*           Increment op count
            OPS = OPS + ( 4*NR-2 )*( I2-I1+2+MIN( NR, I-K ) )
***
*
            IF( WANTZ ) THEN
*
*              Accumulate transformations in the matrix Z
*
               CALL DLARFX( 'Right', NH, NR, V, TAU, Z( ILO, K ), LDZ,
     $                      WORK )
***
*              Increment op count
               OPS = OPS + ( 4*NR-2 )*NH
***
            END IF
  140    CONTINUE
*
  150 CONTINUE
*
*     Failure to converge in remaining number of iterations
*
      INFO = I
      RETURN
*
  160 CONTINUE
*
*     A submatrix of order <= MAXB in rows and columns L to I has split
*     off. Use the double-shift QR algorithm to handle it.
*
      CALL DLAHQR( WANTT, WANTZ, N, L, I, H, LDH, WR, WI, ILO, IHI, Z,
     $             LDZ, INFO )
      IF( INFO.GT.0 )
     $   RETURN
*
*     Decrement number of remaining iterations, and return to start of
*     the main loop with a new value of I.
*
      ITN = ITN - ITS
      I = L - 1
      GO TO 50
*
  170 CONTINUE
***
*     Compute final op count
      OPS = OPS + OPST
***
      WORK( 1 ) = MAX( 1, N )
      RETURN
*
*     End of DHSEQR
*
      END
      SUBROUTINE DLAEBZ( IJOB, NITMAX, N, MMAX, MINP, NBMIN, ABSTOL,
     $                   RELTOL, PIVMIN, D, E, E2, NVAL, AB, C, MOUT,
     $                   NAB, WORK, IWORK, INFO )
*
*  -- LAPACK auxiliary routine (instrum. to count ops. version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     June 30, 1999
*
*     .. Scalar Arguments ..
      INTEGER            IJOB, INFO, MINP, MMAX, MOUT, N, NBMIN, NITMAX
      DOUBLE PRECISION   ABSTOL, PIVMIN, RELTOL
*     ..
*     .. Array Arguments ..
      INTEGER            IWORK( * ), NAB( MMAX, * ), NVAL( * )
      DOUBLE PRECISION   AB( MMAX, * ), C( * ), D( * ), E( * ), E2( * ),
     $                   WORK( * )
*     ..
*     Common block to return operation count and iteration count
*     ITCNT and OPS are only incremented (not initialized)
*     .. Common blocks ..
      COMMON             / LATIME / OPS, ITCNT
*     ..
*     .. Scalars in Common ..
      DOUBLE PRECISION   ITCNT, OPS
*     ..
*-----------------------------------------------------------------------
*
*  Purpose
*  =======
*
*  DLAEBZ contains the iteration loops which compute and use the
*  function N(w), which is the count of eigenvalues of a symmetric
*  tridiagonal matrix T less than or equal to its argument  w.  It
*  performs a choice of two types of loops:
*
*  IJOB=1, followed by
*  IJOB=2: It takes as input a list of intervals and returns a list of
*          sufficiently small intervals whose union contains the same
*          eigenvalues as the union of the original intervals.
*          The input intervals are (AB(j,1),AB(j,2)], j=1,...,MINP.
*          The output interval (AB(j,1),AB(j,2)] will contain
*          eigenvalues NAB(j,1)+1,...,NAB(j,2), where 1 <= j <= MOUT.
*
*  IJOB=3: It performs a binary search in each input interval
*          (AB(j,1),AB(j,2)] for a point  w(j)  such that
*          N(w(j))=NVAL(j), and uses  C(j)  as the starting point of
*          the search.  If such a w(j) is found, then on output
*          AB(j,1)=AB(j,2)=w.  If no such w(j) is found, then on output
*          (AB(j,1),AB(j,2)] will be a small interval containing the
*          point where N(w) jumps through NVAL(j), unless that point
*          lies outside the initial interval.
*
*  Note that the intervals are in all cases half-open intervals,
*  i.e., of the form  (a,b] , which includes  b  but not  a .
*
*  To avoid underflow, the matrix should be scaled so that its largest
*  element is no greater than  overflow**(1/2) * underflow**(1/4)
*  in absolute value.  To assure the most accurate computation
*  of small eigenvalues, the matrix should be scaled to be
*  not much smaller than that, either.
*
*  See W. Kahan "Accurate Eigenvalues of a Symmetric Tridiagonal
*  Matrix", Report CS41, Computer Science Dept., Stanford
*  University, July 21, 1966
*
*  Note: the arguments are, in general, *not* checked for unreasonable
*  values.
*
*  Arguments
*  =========
*
*  IJOB    (input) INTEGER
*          Specifies what is to be done:
*          = 1:  Compute NAB for the initial intervals.
*          = 2:  Perform bisection iteration to find eigenvalues of T.
*          = 3:  Perform bisection iteration to invert N(w), i.e.,
*                to find a point which has a specified number of
*                eigenvalues of T to its left.
*          Other values will cause DLAEBZ to return with INFO=-1.
*
*  NITMAX  (input) INTEGER
*          The maximum number of "levels" of bisection to be
*          performed, i.e., an interval of width W will not be made
*          smaller than 2^(-NITMAX) * W.  If not all intervals
*          have converged after NITMAX iterations, then INFO is set
*          to the number of non-converged intervals.
*
*  N       (input) INTEGER
*          The dimension n of the tridiagonal matrix T.  It must be at
*          least 1.
*
*  MMAX    (input) INTEGER
*          The maximum number of intervals.  If more than MMAX intervals
*          are generated, then DLAEBZ will quit with INFO=MMAX+1.
*
*  MINP    (input) INTEGER
*          The initial number of intervals.  It may not be greater than
*          MMAX.
*
*  NBMIN   (input) INTEGER
*          The smallest number of intervals that should be processed
*          using a vector loop.  If zero, then only the scalar loop
*          will be used.
*
*  ABSTOL  (input) DOUBLE PRECISION
*          The minimum (absolute) width of an interval.  When an
*          interval is narrower than ABSTOL, or than RELTOL times the
*          larger (in magnitude) endpoint, then it is considered to be
*          sufficiently small, i.e., converged.  This must be at least
*          zero.
*
*  RELTOL  (input) DOUBLE PRECISION
*          The minimum relative width of an interval.  When an interval
*          is narrower than ABSTOL, or than RELTOL times the larger (in
*          magnitude) endpoint, then it is considered to be
*          sufficiently small, i.e., converged.  Note: this should
*          always be at least radix*machine epsilon.
*
*  PIVMIN  (input) DOUBLE PRECISION
*          The minimum absolute value of a "pivot" in the Sturm
*          sequence loop.  This *must* be at least  max |e(j)**2| *
*          safe_min  and at least safe_min, where safe_min is at least
*          the smallest number that can divide one without overflow.
*
*  D       (input) DOUBLE PRECISION array, dimension (N)
*          The diagonal elements of the tridiagonal matrix T.
*
*  E       (input) DOUBLE PRECISION array, dimension (N)
*          The offdiagonal elements of the tridiagonal matrix T in
*          positions 1 through N-1.  E(N) is arbitrary.
*
*  E2      (input) DOUBLE PRECISION array, dimension (N)
*          The squares of the offdiagonal elements of the tridiagonal
*          matrix T.  E2(N) is ignored.
*
*  NVAL    (input/output) INTEGER array, dimension (MINP)
*          If IJOB=1 or 2, not referenced.
*          If IJOB=3, the desired values of N(w).  The elements of NVAL
*          will be reordered to correspond with the intervals in AB.
*          Thus, NVAL(j) on output will not, in general be the same as
*          NVAL(j) on input, but it will correspond with the interval
*          (AB(j,1),AB(j,2)] on output.
*
*  AB      (input/output) DOUBLE PRECISION array, dimension (MMAX,2)
*          The endpoints of the intervals.  AB(j,1) is  a(j), the left
*          endpoint of the j-th interval, and AB(j,2) is b(j), the
*          right endpoint of the j-th interval.  The input intervals
*          will, in general, be modified, split, and reordered by the
*          calculation.
*
*  C       (input/output) DOUBLE PRECISION array, dimension (MMAX)
*          If IJOB=1, ignored.
*          If IJOB=2, workspace.
*          If IJOB=3, then on input C(j) should be initialized to the
*          first search point in the binary search.
*
*  MOUT    (output) INTEGER
*          If IJOB=1, the number of eigenvalues in the intervals.
*          If IJOB=2 or 3, the number of intervals output.
*          If IJOB=3, MOUT will equal MINP.
*
*  NAB     (input/output) INTEGER array, dimension (MMAX,2)
*          If IJOB=1, then on output NAB(i,j) will be set to N(AB(i,j)).
*          If IJOB=2, then on input, NAB(i,j) should be set.  It must
*             satisfy the condition:
*             N(AB(i,1)) <= NAB(i,1) <= NAB(i,2) <= N(AB(i,2)),
*             which means that in interval i only eigenvalues
*             NAB(i,1)+1,...,NAB(i,2) will be considered.  Usually,
*             NAB(i,j)=N(AB(i,j)), from a previous call to DLAEBZ with
*             IJOB=1.
*             On output, NAB(i,j) will contain
*             max(na(k),min(nb(k),N(AB(i,j)))), where k is the index of
*             the input interval that the output interval
*             (AB(j,1),AB(j,2)] came from, and na(k) and nb(k) are the
*             the input values of NAB(k,1) and NAB(k,2).
*          If IJOB=3, then on output, NAB(i,j) contains N(AB(i,j)),
*             unless N(w) > NVAL(i) for all search points  w , in which
*             case NAB(i,1) will not be modified, i.e., the output
*             value will be the same as the input value (modulo
*             reorderings -- see NVAL and AB), or unless N(w) < NVAL(i)
*             for all search points  w , in which case NAB(i,2) will
*             not be modified.  Normally, NAB should be set to some
*             distinctive value(s) before DLAEBZ is called.
*
*  WORK    (workspace) DOUBLE PRECISION array, dimension (MMAX)
*          Workspace.
*
*  IWORK   (workspace) INTEGER array, dimension (MMAX)
*          Workspace.
*
*  INFO    (output) INTEGER
*          = 0:       All intervals converged.
*          = 1--MMAX: The last INFO intervals did not converge.
*          = MMAX+1:  More than MMAX intervals were generated.
*
*  Further Details
*  ===============
*
*      This routine is intended to be called only by other LAPACK
*  routines, thus the interface is less user-friendly.  It is intended
*  for two purposes:
*
*  (a) finding eigenvalues.  In this case, DLAEBZ should have one or
*      more initial intervals set up in AB, and DLAEBZ should be called
*      with IJOB=1.  This sets up NAB, and also counts the eigenvalues.
*      Intervals with no eigenvalues would usually be thrown out at
*      this point.  Also, if not all the eigenvalues in an interval i
*      are desired, NAB(i,1) can be increased or NAB(i,2) decreased.
*      For example, set NAB(i,1)=NAB(i,2)-1 to get the largest
*      eigenvalue.  DLAEBZ is then called with IJOB=2 and MMAX
*      no smaller than the value of MOUT returned by the call with
*      IJOB=1.  After this (IJOB=2) call, eigenvalues NAB(i,1)+1
*      through NAB(i,2) are approximately AB(i,1) (or AB(i,2)) to the
*      tolerance specified by ABSTOL and RELTOL.
*
*  (b) finding an interval (a',b'] containing eigenvalues w(f),...,w(l).
*      In this case, start with a Gershgorin interval  (a,b).  Set up
*      AB to contain 2 search intervals, both initially (a,b).  One
*      NVAL element should contain  f-1  and the other should contain  l
*      , while C should contain a and b, resp.  NAB(i,1) should be -1
*      and NAB(i,2) should be N+1, to flag an error if the desired
*      interval does not lie in (a,b).  DLAEBZ is then called with
*      IJOB=3.  On exit, if w(f-1) < w(f), then one of the intervals --
*      j -- will have AB(j,1)=AB(j,2) and NAB(j,1)=NAB(j,2)=f-1, while
*      if, to the specified tolerance, w(f-k)=...=w(f+r), k > 0 and r
*      >= 0, then the interval will have  N(AB(j,1))=NAB(j,1)=f-k and
*      N(AB(j,2))=NAB(j,2)=f+r.  The cases w(l) < w(l+1) and
*      w(l-r)=...=w(l+k) are handled similarly.
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO, TWO, HALF
      PARAMETER          ( ZERO = 0.0D0, TWO = 2.0D0,
     $                   HALF = 1.0D0 / TWO )
*     ..
*     .. Local Scalars ..
      INTEGER            ITMP1, ITMP2, J, JI, JIT, JP, KF, KFNEW, KL,
     $                   KLNEW
      DOUBLE PRECISION   TMP1, TMP2
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, MAX, MIN
*     ..
*     .. Executable Statements ..
*
*     Check for Errors
*
      INFO = 0
      IF( IJOB.LT.1 .OR. IJOB.GT.3 ) THEN
         INFO = -1
         RETURN
      END IF
*
*     Initialize NAB
*
      IF( IJOB.EQ.1 ) THEN
*
*        Compute the number of eigenvalues in the initial intervals.
*
         MOUT = 0
*DIR$ NOVECTOR
         DO 30 JI = 1, MINP
            DO 20 JP = 1, 2
               TMP1 = D( 1 ) - AB( JI, JP )
               IF( ABS( TMP1 ).LT.PIVMIN )
     $            TMP1 = -PIVMIN
               NAB( JI, JP ) = 0
               IF( TMP1.LE.ZERO )
     $            NAB( JI, JP ) = 1
*
               DO 10 J = 2, N
                  TMP1 = D( J ) - E2( J-1 ) / TMP1 - AB( JI, JP )
                  IF( ABS( TMP1 ).LT.PIVMIN )
     $               TMP1 = -PIVMIN
                  IF( TMP1.LE.ZERO )
     $               NAB( JI, JP ) = NAB( JI, JP ) + 1
   10          CONTINUE
   20       CONTINUE
            MOUT = MOUT + NAB( JI, 2 ) - NAB( JI, 1 )
   30    CONTINUE
*
*        Increment opcount for determining the number of eigenvalues
*        in the initial intervals.
*
         OPS = OPS + MINP*2*( N-1 )*3
         RETURN
      END IF
*
*     Initialize for loop
*
*     KF and KL have the following meaning:
*        Intervals 1,...,KF-1 have converged.
*        Intervals KF,...,KL  still need to be refined.
*
      KF = 1
      KL = MINP
*
*     If IJOB=2, initialize C.
*     If IJOB=3, use the user-supplied starting point.
*
      IF( IJOB.EQ.2 ) THEN
         DO 40 JI = 1, MINP
            C( JI ) = HALF*( AB( JI, 1 )+AB( JI, 2 ) )
   40    CONTINUE
*
*        Increment opcount for initializing C.
*
         OPS = OPS + MINP*2
      END IF
*
*     Iteration loop
*
      DO 130 JIT = 1, NITMAX
*
*        Loop over intervals
*
         IF( KL-KF+1.GE.NBMIN .AND. NBMIN.GT.0 ) THEN
*
*           Begin of Parallel Version of the loop
*
            DO 60 JI = KF, KL
*
*              Compute N(c), the number of eigenvalues less than c
*
               WORK( JI ) = D( 1 ) - C( JI )
               IWORK( JI ) = 0
               IF( WORK( JI ).LE.PIVMIN ) THEN
                  IWORK( JI ) = 1
                  WORK( JI ) = MIN( WORK( JI ), -PIVMIN )
               END IF
*
               DO 50 J = 2, N
                  WORK( JI ) = D( J ) - E2( J-1 ) / WORK( JI ) - C( JI )
                  IF( WORK( JI ).LE.PIVMIN ) THEN
                     IWORK( JI ) = IWORK( JI ) + 1
                     WORK( JI ) = MIN( WORK( JI ), -PIVMIN )
                  END IF
   50          CONTINUE
   60       CONTINUE
*
*           Increment iteration counter.
*
            ITCNT = ITCNT + KL - KF + 1
*
*           Increment opcount for evaluating Sturm sequences on
*           each interval.
*
            OPS = OPS + ( KL-KF+1 )*( N-1 )*3
*
            IF( IJOB.LE.2 ) THEN
*
*              IJOB=2: Choose all intervals containing eigenvalues.
*
               KLNEW = KL
               DO 70 JI = KF, KL
*
*                 Insure that N(w) is monotone
*
                  IWORK( JI ) = MIN( NAB( JI, 2 ),
     $                          MAX( NAB( JI, 1 ), IWORK( JI ) ) )
*
*                 Update the Queue -- add intervals if both halves
*                 contain eigenvalues.
*
                  IF( IWORK( JI ).EQ.NAB( JI, 2 ) ) THEN
*
*                    No eigenvalue in the upper interval:
*                    just use the lower interval.
*
                     AB( JI, 2 ) = C( JI )
*
                  ELSE IF( IWORK( JI ).EQ.NAB( JI, 1 ) ) THEN
*
*                    No eigenvalue in the lower interval:
*                    just use the upper interval.
*
                     AB( JI, 1 ) = C( JI )
                  ELSE
                     KLNEW = KLNEW + 1
                     IF( KLNEW.LE.MMAX ) THEN
*
*                       Eigenvalue in both intervals -- add upper to
*                       queue.
*
                        AB( KLNEW, 2 ) = AB( JI, 2 )
                        NAB( KLNEW, 2 ) = NAB( JI, 2 )
                        AB( KLNEW, 1 ) = C( JI )
                        NAB( KLNEW, 1 ) = IWORK( JI )
                        AB( JI, 2 ) = C( JI )
                        NAB( JI, 2 ) = IWORK( JI )
                     ELSE
                        INFO = MMAX + 1
                     END IF
                  END IF
   70          CONTINUE
               IF( INFO.NE.0 )
     $            RETURN
               KL = KLNEW
            ELSE
*
*              IJOB=3: Binary search.  Keep only the interval containing
*                      w   s.t. N(w) = NVAL
*
               DO 80 JI = KF, KL
                  IF( IWORK( JI ).LE.NVAL( JI ) ) THEN
                     AB( JI, 1 ) = C( JI )
                     NAB( JI, 1 ) = IWORK( JI )
                  END IF
                  IF( IWORK( JI ).GE.NVAL( JI ) ) THEN
                     AB( JI, 2 ) = C( JI )
                     NAB( JI, 2 ) = IWORK( JI )
                  END IF
   80          CONTINUE
            END IF
*
         ELSE
*
*           End of Parallel Version of the loop
*
*           Begin of Serial Version of the loop
*
            KLNEW = KL
            DO 100 JI = KF, KL
*
*              Compute N(w), the number of eigenvalues less than w
*
               TMP1 = C( JI )
               TMP2 = D( 1 ) - TMP1
               ITMP1 = 0
               IF( TMP2.LE.PIVMIN ) THEN
                  ITMP1 = 1
                  TMP2 = MIN( TMP2, -PIVMIN )
               END IF
*
*              A series of compiler directives to defeat vectorization
*              for the next loop
*
*$PL$ CMCHAR=' '
CDIR$          NEXTSCALAR
C$DIR          SCALAR
CDIR$          NEXT SCALAR
CVD$L          NOVECTOR
CDEC$          NOVECTOR
CVD$           NOVECTOR
*VDIR          NOVECTOR
*VOCL          LOOP,SCALAR
CIBM           PREFER SCALAR
*$PL$ CMCHAR='*'
*
               DO 90 J = 2, N
                  TMP2 = D( J ) - E2( J-1 ) / TMP2 - TMP1
                  IF( TMP2.LE.PIVMIN ) THEN
                     ITMP1 = ITMP1 + 1
                     TMP2 = MIN( TMP2, -PIVMIN )
                  END IF
   90          CONTINUE
*
               IF( IJOB.LE.2 ) THEN
*
*                 IJOB=2: Choose all intervals containing eigenvalues.
*
*                 Insure that N(w) is monotone
*
                  ITMP1 = MIN( NAB( JI, 2 ),
     $                    MAX( NAB( JI, 1 ), ITMP1 ) )
*
*                 Update the Queue -- add intervals if both halves
*                 contain eigenvalues.
*
                  IF( ITMP1.EQ.NAB( JI, 2 ) ) THEN
*
*                    No eigenvalue in the upper interval:
*                    just use the lower interval.
*
                     AB( JI, 2 ) = TMP1
*
                  ELSE IF( ITMP1.EQ.NAB( JI, 1 ) ) THEN
*
*                    No eigenvalue in the lower interval:
*                    just use the upper interval.
*
                     AB( JI, 1 ) = TMP1
                  ELSE IF( KLNEW.LT.MMAX ) THEN
*
*                    Eigenvalue in both intervals -- add upper to queue.
*
                     KLNEW = KLNEW + 1
                     AB( KLNEW, 2 ) = AB( JI, 2 )
                     NAB( KLNEW, 2 ) = NAB( JI, 2 )
                     AB( KLNEW, 1 ) = TMP1
                     NAB( KLNEW, 1 ) = ITMP1
                     AB( JI, 2 ) = TMP1
                     NAB( JI, 2 ) = ITMP1
                  ELSE
                     INFO = MMAX + 1
                     RETURN
                  END IF
               ELSE
*
*                 IJOB=3: Binary search.  Keep only the interval
*                         containing  w  s.t. N(w) = NVAL
*
                  IF( ITMP1.LE.NVAL( JI ) ) THEN
                     AB( JI, 1 ) = TMP1
                     NAB( JI, 1 ) = ITMP1
                  END IF
                  IF( ITMP1.GE.NVAL( JI ) ) THEN
                     AB( JI, 2 ) = TMP1
                     NAB( JI, 2 ) = ITMP1
                  END IF
               END IF
  100       CONTINUE
*
*           Increment iteration counter.
*
            ITCNT = ITCNT + KL - KF + 1
*
*           Increment opcount for evaluating Sturm sequences on
*           each interval.
*
            OPS = OPS + ( KL-KF+1 )*( N-1 )*3
            KL = KLNEW
*
*           End of Serial Version of the loop
*
         END IF
*
*        Check for convergence
*
         KFNEW = KF
         DO 110 JI = KF, KL
            TMP1 = ABS( AB( JI, 2 )-AB( JI, 1 ) )
            TMP2 = MAX( ABS( AB( JI, 2 ) ), ABS( AB( JI, 1 ) ) )
            IF( TMP1.LT.MAX( ABSTOL, PIVMIN, RELTOL*TMP2 ) .OR.
     $          NAB( JI, 1 ).GE.NAB( JI, 2 ) ) THEN
*
*              Converged -- Swap with position KFNEW,
*                           then increment KFNEW
*
               IF( JI.GT.KFNEW ) THEN
                  TMP1 = AB( JI, 1 )
                  TMP2 = AB( JI, 2 )
                  ITMP1 = NAB( JI, 1 )
                  ITMP2 = NAB( JI, 2 )
                  AB( JI, 1 ) = AB( KFNEW, 1 )
                  AB( JI, 2 ) = AB( KFNEW, 2 )
                  NAB( JI, 1 ) = NAB( KFNEW, 1 )
                  NAB( JI, 2 ) = NAB( KFNEW, 2 )
                  AB( KFNEW, 1 ) = TMP1
                  AB( KFNEW, 2 ) = TMP2
                  NAB( KFNEW, 1 ) = ITMP1
                  NAB( KFNEW, 2 ) = ITMP2
                  IF( IJOB.EQ.3 ) THEN
                     ITMP1 = NVAL( JI )
                     NVAL( JI ) = NVAL( KFNEW )
                     NVAL( KFNEW ) = ITMP1
                  END IF
               END IF
               KFNEW = KFNEW + 1
            END IF
  110    CONTINUE
         KF = KFNEW
*
*        Choose Midpoints
*
         DO 120 JI = KF, KL
            C( JI ) = HALF*( AB( JI, 1 )+AB( JI, 2 ) )
  120    CONTINUE
*
*        Increment opcount for convergence check and choosing midpoints.
*
         OPS = OPS + ( KL-KF+1 )*4
*
*        If no more intervals to refine, quit.
*
         IF( KF.GT.KL )
     $      GO TO 140
  130 CONTINUE
*
*     Converged
*
  140 CONTINUE
      INFO = MAX( KL+1-KF, 0 )
      MOUT = KL
*
      RETURN
*
*     End of DLAEBZ
*
      END
      SUBROUTINE DLAED0( ICOMPQ, QSIZ, N, D, E, Q, LDQ, QSTORE, LDQS,
     $                   WORK, IWORK, INFO )
*
*  -- LAPACK routine (instrumented to count operations, version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     June 30, 1999
*
*     .. Scalar Arguments ..
      INTEGER            ICOMPQ, INFO, LDQ, LDQS, N, QSIZ
*     ..
*     .. Array Arguments ..
      INTEGER            IWORK( * )
      DOUBLE PRECISION   D( * ), E( * ), Q( LDQ, * ), QSTORE( LDQS, * ),
     $                   WORK( * )
*     ..
*     Common block to return operation count and iteration count
*     ITCNT is unchanged, OPS is only incremented
*     .. Common blocks ..
      COMMON             / LATIME / OPS, ITCNT
*     ..
*     .. Scalars in Common ..
      DOUBLE PRECISION   ITCNT, OPS
*     ..
*
*  Purpose
*  =======
*
*  DLAED0 computes all eigenvalues and corresponding eigenvectors of a
*  symmetric tridiagonal matrix using the divide and conquer method.
*
*  Arguments
*  =========
*
*  ICOMPQ  (input) INTEGER
*          = 0:  Compute eigenvalues only.
*          = 1:  Compute eigenvectors of original dense symmetric matrix
*                also.  On entry, Q contains the orthogonal matrix used
*                to reduce the original matrix to tridiagonal form.
*          = 2:  Compute eigenvalues and eigenvectors of tridiagonal
*                matrix.
*
*  QSIZ   (input) INTEGER
*         The dimension of the orthogonal matrix used to reduce
*         the full matrix to tridiagonal form.  QSIZ >= N if ICOMPQ = 1.
*
*  N      (input) INTEGER
*         The dimension of the symmetric tridiagonal matrix.  N >= 0.
*
*  D      (input/output) DOUBLE PRECISION array, dimension (N)
*         On entry, the main diagonal of the tridiagonal matrix.
*         On exit, its eigenvalues.
*
*  E      (input) DOUBLE PRECISION array, dimension (N-1)
*         The off-diagonal elements of the tridiagonal matrix.
*         On exit, E has been destroyed.
*
*  Q      (input/output) DOUBLE PRECISION array, dimension (LDQ, N)
*         On entry, Q must contain an N-by-N orthogonal matrix.
*         If ICOMPQ = 0    Q is not referenced.
*         If ICOMPQ = 1    On entry, Q is a subset of the columns of the
*                          orthogonal matrix used to reduce the full
*                          matrix to tridiagonal form corresponding to
*                          the subset of the full matrix which is being
*                          decomposed at this time.
*         If ICOMPQ = 2    On entry, Q will be the identity matrix.
*                          On exit, Q contains the eigenvectors of the
*                          tridiagonal matrix.
*
*  LDQ    (input) INTEGER
*         The leading dimension of the array Q.  If eigenvectors are
*         desired, then  LDQ >= max(1,N).  In any case,  LDQ >= 1.
*
*  QSTORE (workspace) DOUBLE PRECISION array, dimension (LDQS, N)
*         Referenced only when ICOMPQ = 1.  Used to store parts of
*         the eigenvector matrix when the updating matrix multiplies
*         take place.
*
*  LDQS   (input) INTEGER
*         The leading dimension of the array QSTORE.  If ICOMPQ = 1,
*         then  LDQS >= max(1,N).  In any case,  LDQS >= 1.
*
*  WORK   (workspace) DOUBLE PRECISION array,
*         If ICOMPQ = 0 or 1, the dimension of WORK must be at least
*                     1 + 3*N + 2*N*lg N + 2*N**2
*                     ( lg( N ) = smallest integer k
*                                 such that 2^k >= N )
*         If ICOMPQ = 2, the dimension of WORK must be at least
*                     4*N + N**2.
*
*  IWORK  (workspace) INTEGER array,
*         If ICOMPQ = 0 or 1, the dimension of IWORK must be at least
*                        6 + 6*N + 5*N*lg N.
*                        ( lg( N ) = smallest integer k
*                                    such that 2^k >= N )
*         If ICOMPQ = 2, the dimension of IWORK must be at least
*                        3 + 5*N.
*
*  INFO   (output) INTEGER
*          = 0:  successful exit.
*          < 0:  if INFO = -i, the i-th argument had an illegal value.
*          > 0:  The algorithm failed to compute an eigenvalue while
*                working on the submatrix lying in rows and columns
*                INFO/(N+1) through mod(INFO,N+1).
*
*  Further Details
*  ===============
*
*  Based on contributions by
*     Jeff Rutter, Computer Science Division, University of California
*     at Berkeley, USA
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE, TWO
      PARAMETER          ( ZERO = 0.D0, ONE = 1.D0, TWO = 2.D0 )
*     ..
*     .. Local Scalars ..
      INTEGER            CURLVL, CURPRB, CURR, I, IGIVCL, IGIVNM,
     $                   IGIVPT, INDXQ, IPERM, IPRMPT, IQ, IQPTR, IWREM,
     $                   J, K, LGN, MATSIZ, MSD2, SMLSIZ, SMM1, SPM1,
     $                   SPM2, SUBMAT, SUBPBS, TLVLS
      DOUBLE PRECISION   TEMP
*     ..
*     .. External Functions ..
      INTEGER            ILAENV
      EXTERNAL           ILAENV
*     ..
*     .. External Subroutines ..
      EXTERNAL           DCOPY, DGEMM, DLACPY, DLAED1, DLAED7, DSTEQR,
     $                   XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, DBLE, INT, LOG, MAX
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters.
*
      INFO = 0
*
      IF( ICOMPQ.LT.0 .OR. ICOMPQ.GT.2 ) THEN
         INFO = -1
      ELSE IF( ( ICOMPQ.EQ.1 ) .AND. ( QSIZ.LT.MAX( 0, N ) ) ) THEN
         INFO = -2
      ELSE IF( N.LT.0 ) THEN
         INFO = -3
      ELSE IF( LDQ.LT.MAX( 1, N ) ) THEN
         INFO = -7
      ELSE IF( LDQS.LT.MAX( 1, N ) ) THEN
         INFO = -9
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DLAED0', -INFO )
         RETURN
      END IF
*
*     Quick return if possible
*
      IF( N.EQ.0 )
     $   RETURN
*
      SMLSIZ = ILAENV( 9, 'DLAED0', ' ', 0, 0, 0, 0 )
*
*     Determine the size and placement of the submatrices, and save in
*     the leading elements of IWORK.
*
      IWORK( 1 ) = N
      SUBPBS = 1
      TLVLS = 0
   10 CONTINUE
      IF( IWORK( SUBPBS ).GT.SMLSIZ ) THEN
         DO 20 J = SUBPBS, 1, -1
            IWORK( 2*J ) = ( IWORK( J )+1 ) / 2
            IWORK( 2*J-1 ) = IWORK( J ) / 2
   20    CONTINUE
         TLVLS = TLVLS + 1
         SUBPBS = 2*SUBPBS
         GO TO 10
      END IF
      DO 30 J = 2, SUBPBS
         IWORK( J ) = IWORK( J ) + IWORK( J-1 )
   30 CONTINUE
*
*     Divide the matrix into SUBPBS submatrices of size at most SMLSIZ+1
*     using rank-1 modifications (cuts).
*
      SPM1 = SUBPBS - 1
      OPS = OPS + 2*SPM1
      DO 40 I = 1, SPM1
         SUBMAT = IWORK( I ) + 1
         SMM1 = SUBMAT - 1
         D( SMM1 ) = D( SMM1 ) - ABS( E( SMM1 ) )
         D( SUBMAT ) = D( SUBMAT ) - ABS( E( SMM1 ) )
   40 CONTINUE
*
      INDXQ = 4*N + 3
      IF( ICOMPQ.NE.2 ) THEN
*
*        Set up workspaces for eigenvalues only/accumulate new vectors
*        routine
*
         OPS = OPS + 3
         TEMP = LOG( DBLE( N ) ) / LOG( TWO )
         LGN = INT( TEMP )
         IF( 2**LGN.LT.N )
     $      LGN = LGN + 1
         IF( 2**LGN.LT.N )
     $      LGN = LGN + 1
         IPRMPT = INDXQ + N + 1
         IPERM = IPRMPT + N*LGN
         IQPTR = IPERM + N*LGN
         IGIVPT = IQPTR + N + 2
         IGIVCL = IGIVPT + N*LGN
*
         IGIVNM = 1
         IQ = IGIVNM + 2*N*LGN
         IWREM = IQ + N**2 + 1
*
*        Initialize pointers
*
         DO 50 I = 0, SUBPBS
            IWORK( IPRMPT+I ) = 1
            IWORK( IGIVPT+I ) = 1
   50    CONTINUE
         IWORK( IQPTR ) = 1
      END IF
*
*     Solve each submatrix eigenproblem at the bottom of the divide and
*     conquer tree.
*
      CURR = 0
      DO 70 I = 0, SPM1
         IF( I.EQ.0 ) THEN
            SUBMAT = 1
            MATSIZ = IWORK( 1 )
         ELSE
            SUBMAT = IWORK( I ) + 1
            MATSIZ = IWORK( I+1 ) - IWORK( I )
         END IF
         IF( ICOMPQ.EQ.2 ) THEN
            CALL DSTEQR( 'I', MATSIZ, D( SUBMAT ), E( SUBMAT ),
     $                   Q( SUBMAT, SUBMAT ), LDQ, WORK, INFO )
            IF( INFO.NE.0 )
     $         GO TO 130
         ELSE
            CALL DSTEQR( 'I', MATSIZ, D( SUBMAT ), E( SUBMAT ),
     $                   WORK( IQ-1+IWORK( IQPTR+CURR ) ), MATSIZ, WORK,
     $                   INFO )
            IF( INFO.NE.0 )
     $         GO TO 130
            IF( ICOMPQ.EQ.1 ) THEN
               OPS = OPS + 2*DBLE( QSIZ )*MATSIZ*MATSIZ
               CALL DGEMM( 'N', 'N', QSIZ, MATSIZ, MATSIZ, ONE,
     $                     Q( 1, SUBMAT ), LDQ, WORK( IQ-1+IWORK( IQPTR+
     $                     CURR ) ), MATSIZ, ZERO, QSTORE( 1, SUBMAT ),
     $                     LDQS )
            END IF
            IWORK( IQPTR+CURR+1 ) = IWORK( IQPTR+CURR ) + MATSIZ**2
            CURR = CURR + 1
         END IF
         K = 1
         DO 60 J = SUBMAT, IWORK( I+1 )
            IWORK( INDXQ+J ) = K
            K = K + 1
   60    CONTINUE
   70 CONTINUE
*
*     Successively merge eigensystems of adjacent submatrices
*     into eigensystem for the corresponding larger matrix.
*
*     while ( SUBPBS > 1 )
*
      CURLVL = 1
   80 CONTINUE
      IF( SUBPBS.GT.1 ) THEN
         SPM2 = SUBPBS - 2
         DO 90 I = 0, SPM2, 2
            IF( I.EQ.0 ) THEN
               SUBMAT = 1
               MATSIZ = IWORK( 2 )
               MSD2 = IWORK( 1 )
               CURPRB = 0
            ELSE
               SUBMAT = IWORK( I ) + 1
               MATSIZ = IWORK( I+2 ) - IWORK( I )
               MSD2 = MATSIZ / 2
               CURPRB = CURPRB + 1
            END IF
*
*     Merge lower order eigensystems (of size MSD2 and MATSIZ - MSD2)
*     into an eigensystem of size MATSIZ.
*     DLAED1 is used only for the full eigensystem of a tridiagonal
*     matrix.
*     DLAED7 handles the cases in which eigenvalues only or eigenvalues
*     and eigenvectors of a full symmetric matrix (which was reduced to
*     tridiagonal form) are desired.
*
            IF( ICOMPQ.EQ.2 ) THEN
               CALL DLAED1( MATSIZ, D( SUBMAT ), Q( SUBMAT, SUBMAT ),
     $                      LDQ, IWORK( INDXQ+SUBMAT ),
     $                      E( SUBMAT+MSD2-1 ), MSD2, WORK,
     $                      IWORK( SUBPBS+1 ), INFO )
            ELSE
               CALL DLAED7( ICOMPQ, MATSIZ, QSIZ, TLVLS, CURLVL, CURPRB,
     $                      D( SUBMAT ), QSTORE( 1, SUBMAT ), LDQS,
     $                      IWORK( INDXQ+SUBMAT ), E( SUBMAT+MSD2-1 ),
     $                      MSD2, WORK( IQ ), IWORK( IQPTR ),
     $                      IWORK( IPRMPT ), IWORK( IPERM ),
     $                      IWORK( IGIVPT ), IWORK( IGIVCL ),
     $                      WORK( IGIVNM ), WORK( IWREM ),
     $                      IWORK( SUBPBS+1 ), INFO )
            END IF
            IF( INFO.NE.0 )
     $         GO TO 130
            IWORK( I / 2+1 ) = IWORK( I+2 )
   90    CONTINUE
         SUBPBS = SUBPBS / 2
         CURLVL = CURLVL + 1
         GO TO 80
      END IF
*
*     end while
*
*     Re-merge the eigenvalues/vectors which were deflated at the final
*     merge step.
*
      IF( ICOMPQ.EQ.1 ) THEN
         DO 100 I = 1, N
            J = IWORK( INDXQ+I )
            WORK( I ) = D( J )
            CALL DCOPY( QSIZ, QSTORE( 1, J ), 1, Q( 1, I ), 1 )
  100    CONTINUE
         CALL DCOPY( N, WORK, 1, D, 1 )
      ELSE IF( ICOMPQ.EQ.2 ) THEN
         DO 110 I = 1, N
            J = IWORK( INDXQ+I )
            WORK( I ) = D( J )
            CALL DCOPY( N, Q( 1, J ), 1, WORK( N*I+1 ), 1 )
  110    CONTINUE
         CALL DCOPY( N, WORK, 1, D, 1 )
         CALL DLACPY( 'A', N, N, WORK( N+1 ), N, Q, LDQ )
      ELSE
         DO 120 I = 1, N
            J = IWORK( INDXQ+I )
            WORK( I ) = D( J )
  120    CONTINUE
         CALL DCOPY( N, WORK, 1, D, 1 )
      END IF
      GO TO 140
*
  130 CONTINUE
      INFO = SUBMAT*( N+1 ) + SUBMAT + MATSIZ - 1
*
  140 CONTINUE
      RETURN
*
*     End of DLAED0
*
      END
      SUBROUTINE DLAED1( N, D, Q, LDQ, INDXQ, RHO, CUTPNT, WORK, IWORK,
     $                   INFO )
*
*  -- LAPACK routine (instrumented to count operations, version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     June 30, 1999
*
*     .. Scalar Arguments ..
      INTEGER            CUTPNT, INFO, LDQ, N
      DOUBLE PRECISION   RHO
*     ..
*     .. Array Arguments ..
      INTEGER            INDXQ( * ), IWORK( * )
      DOUBLE PRECISION   D( * ), Q( LDQ, * ), WORK( * )
*     ..
*     Common block to return operation count and iteration count
*     ITCNT is unchanged, OPS is only incremented
*     .. Common blocks ..
      COMMON             / LATIME / OPS, ITCNT
*     ..
*     .. Scalars in Common ..
      DOUBLE PRECISION   ITCNT, OPS
*     ..
*
*  Purpose
*  =======
*
*  DLAED1 computes the updated eigensystem of a diagonal
*  matrix after modification by a rank-one symmetric matrix.  This
*  routine is used only for the eigenproblem which requires all
*  eigenvalues and eigenvectors of a tridiagonal matrix.  DLAED7 handles
*  the case in which eigenvalues only or eigenvalues and eigenvectors
*  of a full symmetric matrix (which was reduced to tridiagonal form)
*  are desired.
*
*    T = Q(in) ( D(in) + RHO * Z*Z' ) Q'(in) = Q(out) * D(out) * Q'(out)
*
*     where Z = Q'u, u is a vector of length N with ones in the
*     CUTPNT and CUTPNT + 1 th elements and zeros elsewhere.
*
*     The eigenvectors of the original matrix are stored in Q, and the
*     eigenvalues are in D.  The algorithm consists of three stages:
*
*        The first stage consists of deflating the size of the problem
*        when there are multiple eigenvalues or if there is a zero in
*        the Z vector.  For each such occurence the dimension of the
*        secular equation problem is reduced by one.  This stage is
*        performed by the routine DLAED2.
*
*        The second stage consists of calculating the updated
*        eigenvalues. This is done by finding the roots of the secular
*        equation via the routine DLAED4 (as called by DLAED3).
*        This routine also calculates the eigenvectors of the current
*        problem.
*
*        The final stage consists of computing the updated eigenvectors
*        directly using the updated eigenvalues.  The eigenvectors for
*        the current problem are multiplied with the eigenvectors from
*        the overall problem.
*
*  Arguments
*  =========
*
*  N      (input) INTEGER
*         The dimension of the symmetric tridiagonal matrix.  N >= 0.
*
*  D      (input/output) DOUBLE PRECISION array, dimension (N)
*         On entry, the eigenvalues of the rank-1-perturbed matrix.
*         On exit, the eigenvalues of the repaired matrix.
*
*  Q      (input/output) DOUBLE PRECISION array, dimension (LDQ,N)
*         On entry, the eigenvectors of the rank-1-perturbed matrix.
*         On exit, the eigenvectors of the repaired tridiagonal matrix.
*
*  LDQ    (input) INTEGER
*         The leading dimension of the array Q.  LDQ >= max(1,N).
*
*  INDXQ  (input/output) INTEGER array, dimension (N)
*         On entry, the permutation which separately sorts the two
*         subproblems in D into ascending order.
*         On exit, the permutation which will reintegrate the
*         subproblems back into sorted order,
*         i.e. D( INDXQ( I = 1, N ) ) will be in ascending order.
*
*  RHO    (input) DOUBLE PRECISION
*         The subdiagonal entry used to create the rank-1 modification.
*
*  CUTPNT (input) INTEGER
*         The location of the last eigenvalue in the leading sub-matrix.
*         min(1,N) <= CUTPNT <= N/2.
*
*  WORK   (workspace) DOUBLE PRECISION array, dimension (4*N + N**2)
*
*  IWORK  (workspace) INTEGER array, dimension (4*N)
*
*  INFO   (output) INTEGER
*          = 0:  successful exit.
*          < 0:  if INFO = -i, the i-th argument had an illegal value.
*          > 0:  if INFO = 1, an eigenvalue did not converge
*
*  Further Details
*  ===============
*
*  Based on contributions by
*     Jeff Rutter, Computer Science Division, University of California
*     at Berkeley, USA
*  Modified by Francoise Tisseur, University of Tennessee.
*
*  =====================================================================
*
*     .. Local Scalars ..
      INTEGER            COLTYP, I, IDLMDA, INDX, INDXC, INDXP, IQ2, IS,
     $                   IW, IZ, K, N1, N2, ZPP1
*     ..
*     .. External Subroutines ..
      EXTERNAL           DCOPY, DLAED2, DLAED3, DLAMRG, XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX, MIN
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters.
*
      INFO = 0
*
      IF( N.LT.0 ) THEN
         INFO = -1
      ELSE IF( LDQ.LT.MAX( 1, N ) ) THEN
         INFO = -4
      ELSE IF( MIN( 1, N / 2 ).GT.CUTPNT .OR. ( N / 2 ).LT.CUTPNT ) THEN
         INFO = -7
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DLAED1', -INFO )
         RETURN
      END IF
*
*     Quick return if possible
*
      IF( N.EQ.0 )
     $   RETURN
*
*     The following values are integer pointers which indicate
*     the portion of the workspace
*     used by a particular array in DLAED2 and DLAED3.
*
      IZ = 1
      IDLMDA = IZ + N
      IW = IDLMDA + N
      IQ2 = IW + N
*
      INDX = 1
      INDXC = INDX + N
      COLTYP = INDXC + N
      INDXP = COLTYP + N
*
*
*     Form the z-vector which consists of the last row of Q_1 and the
*     first row of Q_2.
*
      CALL DCOPY( CUTPNT, Q( CUTPNT, 1 ), LDQ, WORK( IZ ), 1 )
      ZPP1 = CUTPNT + 1
      CALL DCOPY( N-CUTPNT, Q( ZPP1, ZPP1 ), LDQ, WORK( IZ+CUTPNT ), 1 )
*
*     Deflate eigenvalues.
*
      CALL DLAED2( K, N, CUTPNT, D, Q, LDQ, INDXQ, RHO, WORK( IZ ),
     $             WORK( IDLMDA ), WORK( IW ), WORK( IQ2 ),
     $             IWORK( INDX ), IWORK( INDXC ), IWORK( INDXP ),
     $             IWORK( COLTYP ), INFO )
*
      IF( INFO.NE.0 )
     $   GO TO 20
*
*     Solve Secular Equation.
*
      IF( K.NE.0 ) THEN
         IS = ( IWORK( COLTYP )+IWORK( COLTYP+1 ) )*CUTPNT +
     $        ( IWORK( COLTYP+1 )+IWORK( COLTYP+2 ) )*( N-CUTPNT ) + IQ2
         CALL DLAED3( K, N, CUTPNT, D, Q, LDQ, RHO, WORK( IDLMDA ),
     $                WORK( IQ2 ), IWORK( INDXC ), IWORK( COLTYP ),
     $                WORK( IW ), WORK( IS ), INFO )
         IF( INFO.NE.0 )
     $      GO TO 20
*
*     Prepare the INDXQ sorting permutation.
*
         N1 = K
         N2 = N - K
         CALL DLAMRG( N1, N2, D, 1, -1, INDXQ )
      ELSE
         DO 10 I = 1, N
            INDXQ( I ) = I
   10    CONTINUE
      END IF
*
   20 CONTINUE
      RETURN
*
*     End of DLAED1
*
      END
      SUBROUTINE DLAED2( K, N, N1, D, Q, LDQ, INDXQ, RHO, Z, DLAMDA, W,
     $                   Q2, INDX, INDXC, INDXP, COLTYP, INFO )
*
*  -- LAPACK routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     October 31, 1999
*
*     .. Scalar Arguments ..
      INTEGER            INFO, K, LDQ, N, N1
      DOUBLE PRECISION   RHO
*     ..
*     .. Array Arguments ..
      INTEGER            COLTYP( * ), INDX( * ), INDXC( * ), INDXP( * ),
     $                   INDXQ( * )
      DOUBLE PRECISION   D( * ), DLAMDA( * ), Q( LDQ, * ), Q2( * ),
     $                   W( * ), Z( * )
*     ..
*     Common block to return operation count and iteration count
*     ITCNT is unchanged, OPS is only incremented
*     .. Common blocks ..
      COMMON             / LATIME / OPS, ITCNT
*     ..
*     .. Scalars in Common ..
      DOUBLE PRECISION   ITCNT, OPS
*     ..
*
*  Purpose
*  =======
*
*  DLAED2 merges the two sets of eigenvalues together into a single
*  sorted set.  Then it tries to deflate the size of the problem.
*  There are two ways in which deflation can occur:  when two or more
*  eigenvalues are close together or if there is a tiny entry in the
*  Z vector.  For each such occurrence the order of the related secular
*  equation problem is reduced by one.
*
*  Arguments
*  =========
*
*  K      (output) INTEGER
*         The number of non-deflated eigenvalues, and the order of the
*         related secular equation. 0 <= K <=N.
*
*  N      (input) INTEGER
*         The dimension of the symmetric tridiagonal matrix.  N >= 0.
*
*  N1     (input) INTEGER
*         The location of the last eigenvalue in the leading sub-matrix.
*         min(1,N) <= N1 <= N/2.
*
*  D      (input/output) DOUBLE PRECISION array, dimension (N)
*         On entry, D contains the eigenvalues of the two submatrices to
*         be combined.
*         On exit, D contains the trailing (N-K) updated eigenvalues
*         (those which were deflated) sorted into increasing order.
*
*  Q      (input/output) DOUBLE PRECISION array, dimension (LDQ, N)
*         On entry, Q contains the eigenvectors of two submatrices in
*         the two square blocks with corners at (1,1), (N1,N1)
*         and (N1+1, N1+1), (N,N).
*         On exit, Q contains the trailing (N-K) updated eigenvectors
*         (those which were deflated) in its last N-K columns.
*
*  LDQ    (input) INTEGER
*         The leading dimension of the array Q.  LDQ >= max(1,N).
*
*  INDXQ  (input/output) INTEGER array, dimension (N)
*         The permutation which separately sorts the two sub-problems
*         in D into ascending order.  Note that elements in the second
*         half of this permutation must first have N1 added to their
*         values. Destroyed on exit.
*
*  RHO    (input/output) DOUBLE PRECISION
*         On entry, the off-diagonal element associated with the rank-1
*         cut which originally split the two submatrices which are now
*         being recombined.
*         On exit, RHO has been modified to the value required by
*         DLAED3.
*
*  Z      (input) DOUBLE PRECISION array, dimension (N)
*         On entry, Z contains the updating vector (the last
*         row of the first sub-eigenvector matrix and the first row of
*         the second sub-eigenvector matrix).
*         On exit, the contents of Z have been destroyed by the updating
*         process.
*
*  DLAMDA (output) DOUBLE PRECISION array, dimension (N)
*         A copy of the first K eigenvalues which will be used by
*         DLAED3 to form the secular equation.
*
*  W      (output) DOUBLE PRECISION array, dimension (N)
*         The first k values of the final deflation-altered z-vector
*         which will be passed to DLAED3.
*
*  Q2     (output) DOUBLE PRECISION array, dimension (N1**2+(N-N1)**2)
*         A copy of the first K eigenvectors which will be used by
*         DLAED3 in a matrix multiply (DGEMM) to solve for the new
*         eigenvectors.
*
*  INDX   (workspace) INTEGER array, dimension (N)
*         The permutation used to sort the contents of DLAMDA into
*         ascending order.
*
*  INDXC  (output) INTEGER array, dimension (N)
*         The permutation used to arrange the columns of the deflated
*         Q matrix into three groups:  the first group contains non-zero
*         elements only at and above N1, the second contains
*         non-zero elements only below N1, and the third is dense.
*
*  INDXP  (workspace) INTEGER array, dimension (N)
*         The permutation used to place deflated values of D at the end
*         of the array.  INDXP(1:K) points to the nondeflated D-values
*         and INDXP(K+1:N) points to the deflated eigenvalues.
*
*  COLTYP (workspace/output) INTEGER array, dimension (N)
*         During execution, a label which will indicate which of the
*         following types a column in the Q2 matrix is:
*         1 : non-zero in the upper half only;
*         2 : dense;
*         3 : non-zero in the lower half only;
*         4 : deflated.
*         On exit, COLTYP(i) is the number of columns of type i,
*         for i=1 to 4 only.
*
*  INFO   (output) INTEGER
*          = 0:  successful exit.
*          < 0:  if INFO = -i, the i-th argument had an illegal value.
*
*  Further Details
*  ===============
*
*  Based on contributions by
*     Jeff Rutter, Computer Science Division, University of California
*     at Berkeley, USA
*  Modified by Francoise Tisseur, University of Tennessee.
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   MONE, ZERO, ONE, TWO, EIGHT
      PARAMETER          ( MONE = -1.0D0, ZERO = 0.0D0, ONE = 1.0D0,
     $                   TWO = 2.0D0, EIGHT = 8.0D0 )
*     ..
*     .. Local Arrays ..
      INTEGER            CTOT( 4 ), PSM( 4 )
*     ..
*     .. Local Scalars ..
      INTEGER            CT, I, IMAX, IQ1, IQ2, J, JMAX, JS, K2, N1P1,
     $                   N2, NJ, PJ
      DOUBLE PRECISION   C, EPS, S, T, TAU, TOL
*     ..
*     .. External Functions ..
      INTEGER            IDAMAX
      DOUBLE PRECISION   DLAMCH, DLAPY2
      EXTERNAL           IDAMAX, DLAMCH, DLAPY2
*     ..
*     .. External Subroutines ..
      EXTERNAL           DCOPY, DLACPY, DLAMRG, DROT, DSCAL, XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, MAX, MIN, SQRT
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters.
*
      INFO = 0
*
      IF( N.LT.0 ) THEN
         INFO = -2
      ELSE IF( LDQ.LT.MAX( 1, N ) ) THEN
         INFO = -6
      ELSE IF( MIN( 1, ( N / 2 ) ).GT.N1 .OR. ( N / 2 ).LT.N1 ) THEN
         INFO = -3
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DLAED2', -INFO )
         RETURN
      END IF
*
*     Quick return if possible
*
      IF( N.EQ.0 )
     $   RETURN
*
      N2 = N - N1
      N1P1 = N1 + 1
*
      IF( RHO.LT.ZERO ) THEN
         OPS = OPS + N2
         CALL DSCAL( N2, MONE, Z( N1P1 ), 1 )
      END IF
*
*     Normalize z so that norm(z) = 1.  Since z is the concatenation of
*     two normalized vectors, norm2(z) = sqrt(2).
*
      OPS = OPS + N + 3
      T = ONE / SQRT( TWO )
      CALL DSCAL( N, T, Z, 1 )
*
*     RHO = ABS( norm(z)**2 * RHO )
*
      RHO = ABS( TWO*RHO )
*
*     Sort the eigenvalues into increasing order
*
      DO 10 I = N1P1, N
         INDXQ( I ) = INDXQ( I ) + N1
   10 CONTINUE
*
*     re-integrate the deflated parts from the last pass
*
      DO 20 I = 1, N
         DLAMDA( I ) = D( INDXQ( I ) )
   20 CONTINUE
      CALL DLAMRG( N1, N2, DLAMDA, 1, 1, INDXC )
      DO 30 I = 1, N
         INDX( I ) = INDXQ( INDXC( I ) )
   30 CONTINUE
*
*     Calculate the allowable deflation tolerance
*
      IMAX = IDAMAX( N, Z, 1 )
      JMAX = IDAMAX( N, D, 1 )
      EPS = DLAMCH( 'Epsilon' )
      OPS = OPS + 2
      TOL = EIGHT*EPS*MAX( ABS( D( JMAX ) ), ABS( Z( IMAX ) ) )
*
*     If the rank-1 modifier is small enough, no more needs to be done
*     except to reorganize Q so that its columns correspond with the
*     elements in D.
*
      OPS = OPS + 1
      IF( RHO*ABS( Z( IMAX ) ).LE.TOL ) THEN
         K = 0
         IQ2 = 1
         DO 40 J = 1, N
            I = INDX( J )
            CALL DCOPY( N, Q( 1, I ), 1, Q2( IQ2 ), 1 )
            DLAMDA( J ) = D( I )
            IQ2 = IQ2 + N
   40    CONTINUE
         CALL DLACPY( 'A', N, N, Q2, N, Q, LDQ )
         CALL DCOPY( N, DLAMDA, 1, D, 1 )
         GO TO 190
      END IF
*
*     If there are multiple eigenvalues then the problem deflates.  Here
*     the number of equal eigenvalues are found.  As each equal
*     eigenvalue is found, an elementary reflector is computed to rotate
*     the corresponding eigensubspace so that the corresponding
*     components of Z are zero in this new basis.
*
      DO 50 I = 1, N1
         COLTYP( I ) = 1
   50 CONTINUE
      DO 60 I = N1P1, N
         COLTYP( I ) = 3
   60 CONTINUE
*
*
      K = 0
      K2 = N + 1
      DO 70 J = 1, N
         NJ = INDX( J )
         OPS = OPS + 1
         IF( RHO*ABS( Z( NJ ) ).LE.TOL ) THEN
*
*           Deflate due to small z component.
*
            K2 = K2 - 1
            COLTYP( NJ ) = 4
            INDXP( K2 ) = NJ
            IF( J.EQ.N )
     $         GO TO 100
         ELSE
            PJ = NJ
            GO TO 80
         END IF
   70 CONTINUE
   80 CONTINUE
      J = J + 1
      NJ = INDX( J )
      IF( J.GT.N )
     $   GO TO 100
      OPS = OPS + 1
      IF( RHO*ABS( Z( NJ ) ).LE.TOL ) THEN
*
*        Deflate due to small z component.
*
         K2 = K2 - 1
         COLTYP( NJ ) = 4
         INDXP( K2 ) = NJ
      ELSE
*
*        Check if eigenvalues are close enough to allow deflation.
*
         S = Z( PJ )
         C = Z( NJ )
*
*        Find sqrt(a**2+b**2) without overflow or
*        destructive underflow.
*
         OPS = OPS + 10
         TAU = DLAPY2( C, S )
         T = D( NJ ) - D( PJ )
         C = C / TAU
         S = -S / TAU
         IF( ABS( T*C*S ).LE.TOL ) THEN
*
*           Deflation is possible.
*
            Z( NJ ) = TAU
            Z( PJ ) = ZERO
            IF( COLTYP( NJ ).NE.COLTYP( PJ ) )
     $         COLTYP( NJ ) = 2
            COLTYP( PJ ) = 4
            OPS = OPS + 6*N
            CALL DROT( N, Q( 1, PJ ), 1, Q( 1, NJ ), 1, C, S )
            OPS = OPS + 10
            T = D( PJ )*C**2 + D( NJ )*S**2
            D( NJ ) = D( PJ )*S**2 + D( NJ )*C**2
            D( PJ ) = T
            K2 = K2 - 1
            I = 1
   90       CONTINUE
            IF( K2+I.LE.N ) THEN
               IF( D( PJ ).LT.D( INDXP( K2+I ) ) ) THEN
                  INDXP( K2+I-1 ) = INDXP( K2+I )
                  INDXP( K2+I ) = PJ
                  I = I + 1
                  GO TO 90
               ELSE
                  INDXP( K2+I-1 ) = PJ
               END IF
            ELSE
               INDXP( K2+I-1 ) = PJ
            END IF
            PJ = NJ
         ELSE
            K = K + 1
            DLAMDA( K ) = D( PJ )
            W( K ) = Z( PJ )
            INDXP( K ) = PJ
            PJ = NJ
         END IF
      END IF
      GO TO 80
  100 CONTINUE
*
*     Record the last eigenvalue.
*
      K = K + 1
      DLAMDA( K ) = D( PJ )
      W( K ) = Z( PJ )
      INDXP( K ) = PJ
*
*     Count up the total number of the various types of columns, then
*     form a permutation which positions the four column types into
*     four uniform groups (although one or more of these groups may be
*     empty).
*
      DO 110 J = 1, 4
         CTOT( J ) = 0
  110 CONTINUE
      DO 120 J = 1, N
         CT = COLTYP( J )
         CTOT( CT ) = CTOT( CT ) + 1
  120 CONTINUE
*
*     PSM(*) = Position in SubMatrix (of types 1 through 4)
*
      PSM( 1 ) = 1
      PSM( 2 ) = 1 + CTOT( 1 )
      PSM( 3 ) = PSM( 2 ) + CTOT( 2 )
      PSM( 4 ) = PSM( 3 ) + CTOT( 3 )
      K = N - CTOT( 4 )
*
*     Fill out the INDXC array so that the permutation which it induces
*     will place all type-1 columns first, all type-2 columns next,
*     then all type-3's, and finally all type-4's.
*
      DO 130 J = 1, N
         JS = INDXP( J )
         CT = COLTYP( JS )
         INDX( PSM( CT ) ) = JS
         INDXC( PSM( CT ) ) = J
         PSM( CT ) = PSM( CT ) + 1
  130 CONTINUE
*
*     Sort the eigenvalues and corresponding eigenvectors into DLAMDA
*     and Q2 respectively.  The eigenvalues/vectors which were not
*     deflated go into the first K slots of DLAMDA and Q2 respectively,
*     while those which were deflated go into the last N - K slots.
*
      I = 1
      IQ1 = 1
      IQ2 = 1 + ( CTOT( 1 )+CTOT( 2 ) )*N1
      DO 140 J = 1, CTOT( 1 )
         JS = INDX( I )
         CALL DCOPY( N1, Q( 1, JS ), 1, Q2( IQ1 ), 1 )
         Z( I ) = D( JS )
         I = I + 1
         IQ1 = IQ1 + N1
  140 CONTINUE
*
      DO 150 J = 1, CTOT( 2 )
         JS = INDX( I )
         CALL DCOPY( N1, Q( 1, JS ), 1, Q2( IQ1 ), 1 )
         CALL DCOPY( N2, Q( N1+1, JS ), 1, Q2( IQ2 ), 1 )
         Z( I ) = D( JS )
         I = I + 1
         IQ1 = IQ1 + N1
         IQ2 = IQ2 + N2
  150 CONTINUE
*
      DO 160 J = 1, CTOT( 3 )
         JS = INDX( I )
         CALL DCOPY( N2, Q( N1+1, JS ), 1, Q2( IQ2 ), 1 )
         Z( I ) = D( JS )
         I = I + 1
         IQ2 = IQ2 + N2
  160 CONTINUE
*
      IQ1 = IQ2
      DO 170 J = 1, CTOT( 4 )
         JS = INDX( I )
         CALL DCOPY( N, Q( 1, JS ), 1, Q2( IQ2 ), 1 )
         IQ2 = IQ2 + N
         Z( I ) = D( JS )
         I = I + 1
  170 CONTINUE
*
*     The deflated eigenvalues and their corresponding vectors go back
*     into the last N - K slots of D and Q respectively.
*
      CALL DLACPY( 'A', N, CTOT( 4 ), Q2( IQ1 ), N, Q( 1, K+1 ), LDQ )
      CALL DCOPY( N-K, Z( K+1 ), 1, D( K+1 ), 1 )
*
*     Copy CTOT into COLTYP for referencing in DLAED3.
*
      DO 180 J = 1, 4
         COLTYP( J ) = CTOT( J )
  180 CONTINUE
*
  190 CONTINUE
      RETURN
*
*     End of DLAED2
*
      END
      SUBROUTINE DLAED3( K, N, N1, D, Q, LDQ, RHO, DLAMDA, Q2, INDX,
     $                   CTOT, W, S, INFO )
*
*  -- LAPACK routine (instrumented to count operations, version 3.0) --
*     Univ. of Tennessee, Oak Ridge National Lab, Argonne National Lab,
*     Courant Institute, NAG Ltd., and Rice University
*     June 30, 1999
*
*     .. Scalar Arguments ..
      INTEGER            INFO, K, LDQ, N, N1
      DOUBLE PRECISION   RHO
*     ..
*     .. Array Arguments ..
      INTEGER            CTOT( * ), INDX( * )
      DOUBLE PRECISION   D( * ), DLAMDA( * ), Q( LDQ, * ), Q2( * ),
     $                   S( * ), W( * )
*     ..
*     Common block to return operation count and iteration count
*     ITCNT is unchanged, OPS is only incremented
*     .. Common blocks ..
      COMMON             / LATIME / OPS, ITCNT
*     ..
*     .. Scalars in Common ..
      DOUBLE PRECISION   ITCNT, OPS
*     ..
*
*  Purpose
*  =======
*
*  DLAED3 finds the roots of the secular equation, as defined by the
*  values in D, W, and RHO, between 1 and K.  It makes the
*  appropriate calls to DLAED4 and then updates the eigenvectors by
*  multiplying the matrix of eigenvectors of the pair of eigensystems
*  being combined by the matrix of eigenvectors of the K-by-K system
*  which is solved here.
*
*  This code makes very mild assumptions about floating point
*  arithmetic. It will work on machines with a guard digit in
*  add/subtract, or on those binary machines without guard digits
*  which subtract like the Cray X-MP, Cray Y-MP, Cray C-90, or Cray-2.
*  It could conceivably fail on hexadecimal or decimal machines
*  without guard digits, but we know of none.
*
*  Arguments
*  =========
*
*  K       (input) INTEGER
*          The number of terms in the rational function to be solved by
*          DLAED4.  K >= 0.
*
*  N       (input) INTEGER
*          The number of rows and columns in the Q matrix.
*          N >= K (deflation may result in N>K).
*
*  N1      (input) INTEGER
*          The location of the last eigenvalue in the leading submatrix.
*          min(1,N) <= N1 <= N/2.
*
*  D       (output) DOUBLE PRECISION array, dimension (N)
*          D(I) contains the updated eigenvalues for
*          1 <= I <= K.
*
*  Q       (output) DOUBLE PRECISION array, dimension (LDQ,N)
*          Initially the first K columns are used as workspace.
*          On output the columns 1 to K contain
*          the updated eigenvectors.
*
*  LDQ     (input) INTEGER
*          The leading dimension of the array Q.  LDQ >= max(1,N).
*
*  RHO     (input) DOUBLE PRECISION
*          The value of the parameter in the rank one update equation.
*          RHO >= 0 required.
*
*  DLAMDA  (input/output) DOUBLE PRECISION array, dimension (K)
*          The first K elements of this array contain the old roots
*          of the deflated updating problem.  These are the poles
*          of the secular equation. May be changed on output by
*          having lowest order bit set to zero on Cray X-MP, Cray Y-MP,
*          Cray-2, or Cray C-90, as described above.
*
*  Q2      (input) DOUBLE PRECISION array, dimension (LDQ2, N)
*          The first K columns of this matrix contain the non-deflated
*          eigenvectors for the split problem.
*
*  INDX    (input) INTEGER array, dimension (N)
*          The permutation used to arrange the columns of the deflated
*          Q matrix into three groups (see DLAED2).
*          The rows of the eigenvectors found by DLAED4 must be likewise
*          permuted before the matrix multiply can take place.
*
*  CTOT    (input) INTEGER array, dimension (4)
*          A count of the total number of the various types of columns
*          in Q, as described in INDX.  The fourth column type is any
*          column which has been deflated.
*
*  W       (input/output) DOUBLE PRECISION array, dimension (K)
*          The first K elements of this array contain the components
*          of the deflation-adjusted updating vector. Destroyed on
*          output.
*
*  S       (workspace) DOUBLE PRECISION array, dimension (N1 + 1)*K
*          Will contain the eigenvectors of the repaired matrix which
*          will be multiplied by the previously accumulated eigenvectors
*          to update the system.
*
*  LDS     (input) INTEGER
*          The leading dimension of S.  LDS >= max(1,K).
*
*  INFO    (output) INTEGER
*          = 0:  successful exit.
*          < 0:  if INFO = -i, the i-th argument had an illegal value.
*          > 0:  if INFO = 1, an eigenvalue did not converge
*
*  Further Details
*  ===============
*
*  Based on contributions by
*     Jeff Rutter, Computer Science Division, University of California
*     at Berkeley, USA
*  Modified by Francoise Tisseur, University of Tennessee.
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ONE, ZERO
      PARAMETER          ( ONE = 1.0D0, ZERO = 0.0D0 )
*     ..
*     .. Local Scalars ..
      INTEGER            I, II, IQ2, J, N12, N2, N23
      DOUBLE PRECISION   TEMP
*     ..
*     .. External Functions ..
      DOUBLE PRECISION   DLAMC3, DNRM2
      EXTERNAL           DLAMC3, DNRM2
*     ..
*     .. External Subroutines ..
      EXTERNAL           DCOPY, DGEMM, DLACPY, DLAED4, DLASET, XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          DBLE, MAX, SIGN, SQRT
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters.
*
      INFO = 0
*
      IF( K.LT.0 ) THEN
         INFO = -1
      ELSE IF( N.LT.K ) THEN
         INFO = -2
      ELSE IF( LDQ.LT.MAX( 1, N ) ) THEN
         INFO = -6
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DLAED3', -INFO )
         RETURN
      END IF
*
*     Quick return if possible
*
      IF( K.EQ.0 )
     $   RETURN
*
*     Modify values DLAMDA(i) to make sure all DLAMDA(i)-DLAMDA(j) can
*     be computed with high relative accuracy (barring over/underflow).
*     This is a problem on machines without a guard digit in
*     add/subtract (Cray XMP, Cray YMP, Cray C 90 and Cray 2).
*     The following code replaces DLAMDA(I) by 2*DLAMDA(I)-DLAMDA(I),
*     which on any of these machines zeros out the bottommost
*     bit of DLAMDA(I) if it is 1; this makes the subsequent
*     subtractions DLAMDA(I)-DLAMDA(J) unproblematic when cancellation
*     occurs. On binary machines with a guard digit (almost all
*     machines) it does not change DLAMDA(I) at all. On hexadecimal
*     and decimal machines with a guard digit, it slightly
*     changes the bottommost bits of DLAMDA(I). It does not account
*     for hexadecimal or decimal machines without guard digits
*     (we know of none). We use a subroutine call to compute
*     2*DLAMBDA(I) to prevent optimizing compilers from eliminating
*     this code.
*
      OPS = OPS + 2*N
      DO 10 I = 1, K
         DLAMDA( I ) = DLAMC3( DLAMDA( I ), DLAMDA( I ) ) - DLAMDA( I )
   10 CONTINUE
*
      DO 20 J = 1, K
         CALL DLAED4( K, J, DLAMDA, W, Q( 1, J ), RHO, D( J ), INFO )
*
*        If the zero finder fails, the computation is terminated.
*
         IF( INFO.NE.0 )
     $      GO TO 120
   20 CONTINUE
*
      IF( K.EQ.1 )
     $   GO TO 110
      IF( K.EQ.2 ) THEN
         DO 30 J = 1, K
            W( 1 ) = Q( 1, J )
            W( 2 ) = Q( 2, J )
            II = INDX( 1 )
            Q( 1, J ) = W( II )
            II = INDX( 2 )
            Q( 2, J ) = W( II )
   30    CONTINUE
         GO TO 110
      END IF
*
*     Compute updated W.
*
      CALL DCOPY( K, W, 1, S, 1 )
*
*     Initialize W(I) = Q(I,I)
*
      CALL DCOPY( K, Q, LDQ+1, W, 1 )
      OPS = OPS + 3*K*( K-1 )
      DO 60 J = 1, K
         DO 40 I = 1, J - 1
            W( I ) = W( I )*( Q( I, J ) / ( DLAMDA( I )-DLAMDA( J ) ) )
   40    CONTINUE
         DO 50 I = J + 1, K
            W( I ) = W( I )*( Q( I, J ) / ( DLAMDA( I )-DLAMDA( J ) ) )
   50    CONTINUE
   60 CONTINUE
      OPS = OPS + K
      DO 70 I = 1, K
         W( I ) = SIGN( SQRT( -W( I ) ), S( I ) )
   70 CONTINUE
*
*     Compute eigenvectors of the modified rank-1 modification.
*
      OPS = OPS + 4*K*K
      DO 100 J = 1, K
         DO 80 I = 1, K
            S( I ) = W( I ) / Q( I, J )
   80    CONTINUE
         TEMP = DNRM2( K, S, 1 )
         DO 90 I = 1, K
            II = INDX( I )
            Q( I, J ) = S( II ) / TEMP
   90    CONTINUE
  100 CONTINUE
*
*     Compute the updated eigenvectors.
*
  110 CONTINUE
*
      N2 = N - N1
      N12 = CTOT( 1 ) + CTOT( 2 )
      N23 = CTOT( 2 ) + CTOT( 3 )
*
      CALL DLACPY( 'A', N23, K, Q( CTOT( 1 )+1, 1 ), LDQ, S, N23 )
      IQ2 = N1*N12 + 1
      IF( N23.NE.0 ) THEN
         OPS = OPS + 2*DBLE( N2 )*K*N23
         CALL DGEMM( 'N', 'N', N2, K, N23, ONE, Q2( IQ2 ), N2, S, N23,
     $               ZERO, Q( N1+1, 1 ), LDQ )
      ELSE
         CALL DLASET( 'A', N2, K, ZERO, ZERO, Q( N1+1, 1 ), LDQ )
      END IF
*
      CALL DLACPY( 'A', N12, K, Q, LDQ, S, N12 )
      IF( N12.NE.0 ) THEN
         OPS = OPS + 2*DBLE( N1 )*K*N12
         CALL DGEMM( 'N', 'N', N1, K, N12, ONE, Q2, N1, S, N12, ZERO, Q,
     $               LDQ )
      ELSE
         CALL DLASET( 'A', N1, K, ZERO, ZERO, Q( 1, 1 ), LDQ )
      END IF
*
*
  120 CONTINUE
      RETURN
*
*     End of DLAED3
*
      END
      SUBROUTINE DLAED4( N, I, D, Z, DELTA, RHO, DLAM, INFO )
*
*  -- LAPACK routine (instrumented to count operations, version 3.0) --
*     Univ. of Tennessee, Oak Ridge National Lab, Argonne National Lab,
*     Courant Institute, NAG Ltd., and Rice University
*     June 30, 1999
*
*     .. Scalar Arguments ..
      INTEGER            I, INFO, N
      DOUBLE PRECISION   DLAM, RHO
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   D( * ), DELTA( * ), Z( * )
*     ..
*     Common block to return operation count and iteration count
*     ITCNT is unchanged, OPS is only incremented
*     .. Common blocks ..
      COMMON             / LATIME / OPS, ITCNT
*     ..
*     .. Scalars in Common ..
      DOUBLE PRECISION   ITCNT, OPS
*     ..
*
*  Purpose
*  =======
*
*  This subroutine computes the I-th updated eigenvalue of a symmetric
*  rank-one modification to a diagonal matrix whose elements are
*  given in the array d, and that
*
*             D(i) < D(j)  for  i < j
*
*  and that RHO > 0.  This is arranged by the calling routine, and is
*  no loss in generality.  The rank-one modified system is thus
*
*             diag( D )  +  RHO *  Z * Z_transpose.
*
*  where we assume the Euclidean norm of Z is 1.
*
*  The method consists of approximating the rational functions in the
*  secular equation by simpler interpolating rational functions.
*
*  Arguments
*  =========
*
*  N      (input) INTEGER
*         The length of all arrays.
*
*  I      (input) INTEGER
*         The index of the eigenvalue to be computed.  1 <= I <= N.
*
*  D      (input) DOUBLE PRECISION array, dimension (N)
*         The original eigenvalues.  It is assumed that they are in
*         order, D(I) < D(J)  for I < J.
*
*  Z      (input) DOUBLE PRECISION array, dimension (N)
*         The components of the updating vector.
*
*  DELTA  (output) DOUBLE PRECISION array, dimension (N)
*         If N .ne. 1, DELTA contains (D(j) - lambda_I) in its  j-th
*         component.  If N = 1, then DELTA(1) = 1.  The vector DELTA
*         contains the information necessary to construct the
*         eigenvectors.
*
*  RHO    (input) DOUBLE PRECISION
*         The scalar in the symmetric updating formula.
*
*  DLAM   (output) DOUBLE PRECISION
*         The computed lambda_I, the I-th updated eigenvalue.
*
*  INFO   (output) INTEGER
*         = 0:  successful exit
*         > 0:  if INFO = 1, the updating process failed.
*
*  Internal Parameters
*  ===================
*
*  Logical variable ORGATI (origin-at-i?) is used for distinguishing
*  whether D(i) or D(i+1) is treated as the origin.
*
*            ORGATI = .true.    origin at i
*            ORGATI = .false.   origin at i+1
*
*   Logical variable SWTCH3 (switch-for-3-poles?) is for noting
*   if we are working with THREE poles!
*
*   MAXIT is the maximum number of iterations allowed for each
*   eigenvalue.
*
*  Further Details
*  ===============
*
*  Based on contributions by
*     Ren-Cang Li, Computer Science Division, University of California
*     at Berkeley, USA
*
*  =====================================================================
*
*     .. Parameters ..
      INTEGER            MAXIT
      PARAMETER          ( MAXIT = 20 )
      DOUBLE PRECISION   ZERO, ONE, TWO, THREE, FOUR, EIGHT, TEN
      PARAMETER          ( ZERO = 0.0D0, ONE = 1.0D0, TWO = 2.0D0,
     $                   THREE = 3.0D0, FOUR = 4.0D0, EIGHT = 8.0D0,
     $                   TEN = 10.0D0 )
*     ..
*     .. Local Scalars ..
      LOGICAL            ORGATI, SWTCH, SWTCH3
      INTEGER            II, IIM1, IIP1, IP1, ITER, J, NITER
      DOUBLE PRECISION   A, B, C, DEL, DPHI, DPSI, DW, EPS, ERRETM, ETA,
     $                   PHI, PREW, PSI, RHOINV, TAU, TEMP, TEMP1, W
*     ..
*     .. Local Arrays ..
      DOUBLE PRECISION   ZZ( 3 )
*     ..
*     .. External Functions ..
      DOUBLE PRECISION   DLAMCH
      EXTERNAL           DLAMCH
*     ..
*     .. External Subroutines ..
      EXTERNAL           DLAED5, DLAED6
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, SQRT
*     ..
*     .. Executable Statements ..
*
*     Since this routine is called in an inner loop, we do no argument
*     checking.
*
*     Quick return for N=1 and 2.
*
      INFO = 0
      IF( N.EQ.1 ) THEN
*
*         Presumably, I=1 upon entry
*
         OPS = OPS + 3
         DLAM = D( 1 ) + RHO*Z( 1 )*Z( 1 )
         DELTA( 1 ) = ONE
         RETURN
      END IF
      IF( N.EQ.2 ) THEN
         CALL DLAED5( I, D, Z, DELTA, RHO, DLAM )
         RETURN
      END IF
*
*     Compute machine epsilon
*
      EPS = DLAMCH( 'Epsilon' )
      OPS = OPS + 1
      RHOINV = ONE / RHO
*
*     The case I = N
*
      IF( I.EQ.N ) THEN
*
*        Initialize some basic variables
*
         II = N - 1
         NITER = 1
*
*        Calculate initial guess
*
         OPS = OPS + 5*N + 1
         TEMP = RHO / TWO
*
*        If ||Z||_2 is not one, then TEMP should be set to
*        RHO * ||Z||_2^2 / TWO
*
         DO 10 J = 1, N
            DELTA( J ) = ( D( J )-D( I ) ) - TEMP
   10    CONTINUE
*
         PSI = ZERO
         DO 20 J = 1, N - 2
            PSI = PSI + Z( J )*Z( J ) / DELTA( J )
   20    CONTINUE
*
         C = RHOINV + PSI
         W = C + Z( II )*Z( II ) / DELTA( II ) +
     $       Z( N )*Z( N ) / DELTA( N )
*
         IF( W.LE.ZERO ) THEN
            OPS = OPS + 7
            TEMP = Z( N-1 )*Z( N-1 ) / ( D( N )-D( N-1 )+RHO ) +
     $             Z( N )*Z( N ) / RHO
            IF( C.LE.TEMP ) THEN
               TAU = RHO
            ELSE
               OPS = OPS + 14
               DEL = D( N ) - D( N-1 )
               A = -C*DEL + Z( N-1 )*Z( N-1 ) + Z( N )*Z( N )
               B = Z( N )*Z( N )*DEL
               IF( A.LT.ZERO ) THEN
                  TAU = TWO*B / ( SQRT( A*A+FOUR*B*C )-A )
               ELSE
                  TAU = ( A+SQRT( A*A+FOUR*B*C ) ) / ( TWO*C )
               END IF
            END IF
*
*           It can be proved that
*               D(N)+RHO/2 <= LAMBDA(N) < D(N)+TAU <= D(N)+RHO
*
         ELSE
            OPS = OPS + 16
            DEL = D( N ) - D( N-1 )
            A = -C*DEL + Z( N-1 )*Z( N-1 ) + Z( N )*Z( N )
            B = Z( N )*Z( N )*DEL
            IF( A.LT.ZERO ) THEN
               TAU = TWO*B / ( SQRT( A*A+FOUR*B*C )-A )
            ELSE
               TAU = ( A+SQRT( A*A+FOUR*B*C ) ) / ( TWO*C )
            END IF
*
*           It can be proved that
*               D(N) < D(N)+TAU < LAMBDA(N) < D(N)+RHO/2
*
         END IF
*
         OPS = OPS + 2*N + 6*II + 14
         DO 30 J = 1, N
            DELTA( J ) = ( D( J )-D( I ) ) - TAU
   30    CONTINUE
*
*        Evaluate PSI and the derivative DPSI
*
         DPSI = ZERO
         PSI = ZERO
         ERRETM = ZERO
         DO 40 J = 1, II
            TEMP = Z( J ) / DELTA( J )
            PSI = PSI + Z( J )*TEMP
            DPSI = DPSI + TEMP*TEMP
            ERRETM = ERRETM + PSI
   40    CONTINUE
         ERRETM = ABS( ERRETM )
*
*        Evaluate PHI and the derivative DPHI
*
         TEMP = Z( N ) / DELTA( N )
         PHI = Z( N )*TEMP
         DPHI = TEMP*TEMP
         ERRETM = EIGHT*( -PHI-PSI ) + ERRETM - PHI + RHOINV +
     $            ABS( TAU )*( DPSI+DPHI )
*
         W = RHOINV + PHI + PSI
*
*        Test for convergence
*
         IF( ABS( W ).LE.EPS*ERRETM ) THEN
            OPS = OPS + 1
            DLAM = D( I ) + TAU
            GO TO 250
         END IF
*
*        Calculate the new step
*
         OPS = OPS + 12
         NITER = NITER + 1
         C = W - DELTA( N-1 )*DPSI - DELTA( N )*DPHI
         A = ( DELTA( N-1 )+DELTA( N ) )*W -
     $       DELTA( N-1 )*DELTA( N )*( DPSI+DPHI )
         B = DELTA( N-1 )*DELTA( N )*W
         IF( C.LT.ZERO )
     $      C = ABS( C )
         IF( C.EQ.ZERO ) THEN
*           ETA = B/A
            OPS = OPS + 1
            ETA = RHO - TAU
         ELSE IF( A.GE.ZERO ) THEN
            OPS = OPS + 8
            ETA = ( A+SQRT( ABS( A*A-FOUR*B*C ) ) ) / ( TWO*C )
         ELSE
            OPS = OPS + 8
            ETA = TWO*B / ( A-SQRT( ABS( A*A-FOUR*B*C ) ) )
         END IF
*
*        Note, eta should be positive if w is negative, and
*        eta should be negative otherwise. However,
*        if for some reason caused by roundoff, eta*w > 0,
*        we simply use one Newton step instead. This way
*        will guarantee eta*w < 0.
*
         OPS = OPS + N + 6*II + 16
         IF( W*ETA.GT.ZERO ) THEN
            OPS = OPS + 2
            ETA = -W / ( DPSI+DPHI )
         END IF
         TEMP = TAU + ETA
         IF( TEMP.GT.RHO ) THEN
            OPS = OPS + 1
            ETA = RHO - TAU
         END IF
         DO 50 J = 1, N
            DELTA( J ) = DELTA( J ) - ETA
   50    CONTINUE
*
         TAU = TAU + ETA
*
*        Evaluate PSI and the derivative DPSI
*
         DPSI = ZERO
         PSI = ZERO
         ERRETM = ZERO
         DO 60 J = 1, II
            TEMP = Z( J ) / DELTA( J )
            PSI = PSI + Z( J )*TEMP
            DPSI = DPSI + TEMP*TEMP
            ERRETM = ERRETM + PSI
   60    CONTINUE
         ERRETM = ABS( ERRETM )
*
*        Evaluate PHI and the derivative DPHI
*
         TEMP = Z( N ) / DELTA( N )
         PHI = Z( N )*TEMP
         DPHI = TEMP*TEMP
         ERRETM = EIGHT*( -PHI-PSI ) + ERRETM - PHI + RHOINV +
     $            ABS( TAU )*( DPSI+DPHI )
*
         W = RHOINV + PHI + PSI
*
*        Main loop to update the values of the array   DELTA
*
         ITER = NITER + 1
*
         DO 90 NITER = ITER, MAXIT
*
*           Test for convergence
*
            OPS = OPS + 1
            IF( ABS( W ).LE.EPS*ERRETM ) THEN
               OPS = OPS + 1
               DLAM = D( I ) + TAU
               GO TO 250
            END IF
*
*           Calculate the new step
*
            OPS = OPS + 36 + N + 6*II
            C = W - DELTA( N-1 )*DPSI - DELTA( N )*DPHI
            A = ( DELTA( N-1 )+DELTA( N ) )*W -
     $          DELTA( N-1 )*DELTA( N )*( DPSI+DPHI )
            B = DELTA( N-1 )*DELTA( N )*W
            IF( A.GE.ZERO ) THEN
               ETA = ( A+SQRT( ABS( A*A-FOUR*B*C ) ) ) / ( TWO*C )
            ELSE
               ETA = TWO*B / ( A-SQRT( ABS( A*A-FOUR*B*C ) ) )
            END IF
*
*           Note, eta should be positive if w is negative, and
*           eta should be negative otherwise. However,
*           if for some reason caused by roundoff, eta*w > 0,
*           we simply use one Newton step instead. This way
*           will guarantee eta*w < 0.
*
            IF( W*ETA.GT.ZERO )
     $         ETA = -W / ( DPSI+DPHI )
            TEMP = TAU + ETA
            IF( TEMP.LE.ZERO )
     $         ETA = ETA / TWO
            DO 70 J = 1, N
               DELTA( J ) = DELTA( J ) - ETA
   70       CONTINUE
*
            TAU = TAU + ETA
*
*           Evaluate PSI and the derivative DPSI
*
            DPSI = ZERO
            PSI = ZERO
            ERRETM = ZERO
            DO 80 J = 1, II
               TEMP = Z( J ) / DELTA( J )
               PSI = PSI + Z( J )*TEMP
               DPSI = DPSI + TEMP*TEMP
               ERRETM = ERRETM + PSI
   80       CONTINUE
            ERRETM = ABS( ERRETM )
*
*           Evaluate PHI and the derivative DPHI
*
            TEMP = Z( N ) / DELTA( N )
            PHI = Z( N )*TEMP
            DPHI = TEMP*TEMP
            ERRETM = EIGHT*( -PHI-PSI ) + ERRETM - PHI + RHOINV +
     $               ABS( TAU )*( DPSI+DPHI )
*
            W = RHOINV + PHI + PSI
   90    CONTINUE
*
*        Return with INFO = 1, NITER = MAXIT and not converged
*
         INFO = 1
         OPS = OPS + 1
         DLAM = D( I ) + TAU
         GO TO 250
*
*        End for the case I = N
*
      ELSE
*
*        The case for I < N
*
         NITER = 1
         IP1 = I + 1
*
*        Calculate initial guess
*
         TEMP = ( D( IP1 )-D( I ) ) / TWO
         DO 100 J = 1, N
            DELTA( J ) = ( D( J )-D( I ) ) - TEMP
  100    CONTINUE
*
         PSI = ZERO
         DO 110 J = 1, I - 1
            PSI = PSI + Z( J )*Z( J ) / DELTA( J )
  110    CONTINUE
*
         PHI = ZERO
         DO 120 J = N, I + 2, -1
            PHI = PHI + Z( J )*Z( J ) / DELTA( J )
  120    CONTINUE
         C = RHOINV + PSI + PHI
         W = C + Z( I )*Z( I ) / DELTA( I ) +
     $       Z( IP1 )*Z( IP1 ) / DELTA( IP1 )
*
         IF( W.GT.ZERO ) THEN
*
*           d(i)< the ith eigenvalue < (d(i)+d(i+1))/2
*
*           We choose d(i) as origin.
*
            ORGATI = .TRUE.
            DEL = D( IP1 ) - D( I )
            A = C*DEL + Z( I )*Z( I ) + Z( IP1 )*Z( IP1 )
            B = Z( I )*Z( I )*DEL
            IF( A.GT.ZERO ) THEN
               TAU = TWO*B / ( A+SQRT( ABS( A*A-FOUR*B*C ) ) )
            ELSE
               TAU = ( A-SQRT( ABS( A*A-FOUR*B*C ) ) ) / ( TWO*C )
            END IF
         ELSE
*
*           (d(i)+d(i+1))/2 <= the ith eigenvalue < d(i+1)
*
*           We choose d(i+1) as origin.
*
            ORGATI = .FALSE.
            DEL = D( IP1 ) - D( I )
            A = C*DEL - Z( I )*Z( I ) - Z( IP1 )*Z( IP1 )
            B = Z( IP1 )*Z( IP1 )*DEL
            IF( A.LT.ZERO ) THEN
               TAU = TWO*B / ( A-SQRT( ABS( A*A+FOUR*B*C ) ) )
            ELSE
               TAU = -( A+SQRT( ABS( A*A+FOUR*B*C ) ) ) / ( TWO*C )
            END IF
         END IF
*
         IF( ORGATI ) THEN
            DO 130 J = 1, N
               DELTA( J ) = ( D( J )-D( I ) ) - TAU
  130       CONTINUE
         ELSE
            DO 140 J = 1, N
               DELTA( J ) = ( D( J )-D( IP1 ) ) - TAU
  140       CONTINUE
         END IF
         IF( ORGATI ) THEN
            II = I
         ELSE
            II = I + 1
         END IF
         IIM1 = II - 1
         IIP1 = II + 1
         OPS = OPS + 13*N + 6*( IIM1-IIP1 ) + 45
*
*        Evaluate PSI and the derivative DPSI
*
         DPSI = ZERO
         PSI = ZERO
         ERRETM = ZERO
         DO 150 J = 1, IIM1
            TEMP = Z( J ) / DELTA( J )
            PSI = PSI + Z( J )*TEMP
            DPSI = DPSI + TEMP*TEMP
            ERRETM = ERRETM + PSI
  150    CONTINUE
         ERRETM = ABS( ERRETM )
*
*        Evaluate PHI and the derivative DPHI
*
         DPHI = ZERO
         PHI = ZERO
         DO 160 J = N, IIP1, -1
            TEMP = Z( J ) / DELTA( J )
            PHI = PHI + Z( J )*TEMP
            DPHI = DPHI + TEMP*TEMP
            ERRETM = ERRETM + PHI
  160    CONTINUE
*
         W = RHOINV + PHI + PSI
*
*        W is the value of the secular function with
*        its ii-th element removed.
*
         SWTCH3 = .FALSE.
         IF( ORGATI ) THEN
            IF( W.LT.ZERO )
     $         SWTCH3 = .TRUE.
         ELSE
            IF( W.GT.ZERO )
     $         SWTCH3 = .TRUE.
         END IF
         IF( II.EQ.1 .OR. II.EQ.N )
     $      SWTCH3 = .FALSE.
*
         TEMP = Z( II ) / DELTA( II )
         DW = DPSI + DPHI + TEMP*TEMP
         TEMP = Z( II )*TEMP
         W = W + TEMP
         ERRETM = EIGHT*( PHI-PSI ) + ERRETM + TWO*RHOINV +
     $            THREE*ABS( TEMP ) + ABS( TAU )*DW
*
*        Test for convergence
*
         IF( ABS( W ).LE.EPS*ERRETM ) THEN
            IF( ORGATI ) THEN
               DLAM = D( I ) + TAU
            ELSE
               DLAM = D( IP1 ) + TAU
            END IF
            GO TO 250
         END IF
*
*        Calculate the new step
*
         OPS = OPS + 14
         NITER = NITER + 1
         IF( .NOT.SWTCH3 ) THEN
            IF( ORGATI ) THEN
               C = W - DELTA( IP1 )*DW - ( D( I )-D( IP1 ) )*
     $             ( Z( I ) / DELTA( I ) )**2
            ELSE
               C = W - DELTA( I )*DW - ( D( IP1 )-D( I ) )*
     $             ( Z( IP1 ) / DELTA( IP1 ) )**2
            END IF
            A = ( DELTA( I )+DELTA( IP1 ) )*W -
     $          DELTA( I )*DELTA( IP1 )*DW
            B = DELTA( I )*DELTA( IP1 )*W
            IF( C.EQ.ZERO ) THEN
               IF( A.EQ.ZERO ) THEN
                  OPS = OPS + 5
                  IF( ORGATI ) THEN
                     A = Z( I )*Z( I ) + DELTA( IP1 )*DELTA( IP1 )*
     $                   ( DPSI+DPHI )
                  ELSE
                     A = Z( IP1 )*Z( IP1 ) + DELTA( I )*DELTA( I )*
     $                   ( DPSI+DPHI )
                  END IF
               END IF
               ETA = B / A
            ELSE IF( A.LE.ZERO ) THEN
               OPS = OPS + 8
               ETA = ( A-SQRT( ABS( A*A-FOUR*B*C ) ) ) / ( TWO*C )
            ELSE
               OPS = OPS + 8
               ETA = TWO*B / ( A+SQRT( ABS( A*A-FOUR*B*C ) ) )
            END IF
         ELSE
*
*           Interpolation using THREE most relevant poles
*
            OPS = OPS + 15
            TEMP = RHOINV + PSI + PHI
            IF( ORGATI ) THEN
               TEMP1 = Z( IIM1 ) / DELTA( IIM1 )
               TEMP1 = TEMP1*TEMP1
               C = TEMP - DELTA( IIP1 )*( DPSI+DPHI ) -
     $             ( D( IIM1 )-D( IIP1 ) )*TEMP1
               ZZ( 1 ) = Z( IIM1 )*Z( IIM1 )
               ZZ( 3 ) = DELTA( IIP1 )*DELTA( IIP1 )*
     $                   ( ( DPSI-TEMP1 )+DPHI )
            ELSE
               TEMP1 = Z( IIP1 ) / DELTA( IIP1 )
               TEMP1 = TEMP1*TEMP1
               C = TEMP - DELTA( IIM1 )*( DPSI+DPHI ) -
     $             ( D( IIP1 )-D( IIM1 ) )*TEMP1
               ZZ( 1 ) = DELTA( IIM1 )*DELTA( IIM1 )*
     $                   ( DPSI+( DPHI-TEMP1 ) )
               ZZ( 3 ) = Z( IIP1 )*Z( IIP1 )
            END IF
            ZZ( 2 ) = Z( II )*Z( II )
            CALL DLAED6( NITER, ORGATI, C, DELTA( IIM1 ), ZZ, W, ETA,
     $                   INFO )
            IF( INFO.NE.0 )
     $         GO TO 250
         END IF
*
*        Note, eta should be positive if w is negative, and
*        eta should be negative otherwise. However,
*        if for some reason caused by roundoff, eta*w > 0,
*        we simply use one Newton step instead. This way
*        will guarantee eta*w < 0.
*
         OPS = OPS + 18 + 7*N + 6*( IIM1-IIP1 )
         IF( W*ETA.GE.ZERO ) THEN
            OPS = OPS + 1
            ETA = -W / DW
         END IF
         TEMP = TAU + ETA
         DEL = ( D( IP1 )-D( I ) ) / TWO
         IF( ORGATI ) THEN
            IF( TEMP.GE.DEL ) THEN
               OPS = OPS + 1
               ETA = DEL - TAU
            END IF
            IF( TEMP.LE.ZERO ) THEN
               OPS = OPS + 1
               ETA = ETA / TWO
            END IF
         ELSE
            IF( TEMP.LE.-DEL ) THEN
               OPS = OPS + 1
               ETA = -DEL - TAU
            END IF
            IF( TEMP.GE.ZERO ) THEN
               OPS = OPS + 1
               ETA = ETA / TWO
            END IF
         END IF
*
         PREW = W
*
  170    CONTINUE
         DO 180 J = 1, N
            DELTA( J ) = DELTA( J ) - ETA
  180    CONTINUE
*
*        Evaluate PSI and the derivative DPSI
*
         DPSI = ZERO
         PSI = ZERO
         ERRETM = ZERO
         DO 190 J = 1, IIM1
            TEMP = Z( J ) / DELTA( J )
            PSI = PSI + Z( J )*TEMP
            DPSI = DPSI + TEMP*TEMP
            ERRETM = ERRETM + PSI
  190    CONTINUE
         ERRETM = ABS( ERRETM )
*
*        Evaluate PHI and the derivative DPHI
*
         DPHI = ZERO
         PHI = ZERO
         DO 200 J = N, IIP1, -1
            TEMP = Z( J ) / DELTA( J )
            PHI = PHI + Z( J )*TEMP
            DPHI = DPHI + TEMP*TEMP
            ERRETM = ERRETM + PHI
  200    CONTINUE
*
         TEMP = Z( II ) / DELTA( II )
         DW = DPSI + DPHI + TEMP*TEMP
         TEMP = Z( II )*TEMP
         W = RHOINV + PHI + PSI + TEMP
         ERRETM = EIGHT*( PHI-PSI ) + ERRETM + TWO*RHOINV +
     $            THREE*ABS( TEMP ) + ABS( TAU+ETA )*DW
*
         SWTCH = .FALSE.
         IF( ORGATI ) THEN
            IF( -W.GT.ABS( PREW ) / TEN )
     $         SWTCH = .TRUE.
         ELSE
            IF( W.GT.ABS( PREW ) / TEN )
     $         SWTCH = .TRUE.
         END IF
*
         TAU = TAU + ETA
*
*        Main loop to update the values of the array   DELTA
*
         ITER = NITER + 1
*
         DO 240 NITER = ITER, MAXIT
*
*           Test for convergence
*
            OPS = OPS + 1
            IF( ABS( W ).LE.EPS*ERRETM ) THEN
               OPS = OPS + 1
               IF( ORGATI ) THEN
                  DLAM = D( I ) + TAU
               ELSE
                  DLAM = D( IP1 ) + TAU
               END IF
               GO TO 250
            END IF
*
*           Calculate the new step
*
            IF( .NOT.SWTCH3 ) THEN
               OPS = OPS + 14
               IF( .NOT.SWTCH ) THEN
                  IF( ORGATI ) THEN
                     C = W - DELTA( IP1 )*DW -
     $                   ( D( I )-D( IP1 ) )*( Z( I ) / DELTA( I ) )**2
                  ELSE
                     C = W - DELTA( I )*DW - ( D( IP1 )-D( I ) )*
     $                   ( Z( IP1 ) / DELTA( IP1 ) )**2
                  END IF
               ELSE
                  TEMP = Z( II ) / DELTA( II )
                  IF( ORGATI ) THEN
                     DPSI = DPSI + TEMP*TEMP
                  ELSE
                     DPHI = DPHI + TEMP*TEMP
                  END IF
                  C = W - DELTA( I )*DPSI - DELTA( IP1 )*DPHI
               END IF
               A = ( DELTA( I )+DELTA( IP1 ) )*W -
     $             DELTA( I )*DELTA( IP1 )*DW
               B = DELTA( I )*DELTA( IP1 )*W
               IF( C.EQ.ZERO ) THEN
                  IF( A.EQ.ZERO ) THEN
                     OPS = OPS + 5
                     IF( .NOT.SWTCH ) THEN
                        IF( ORGATI ) THEN
                           A = Z( I )*Z( I ) + DELTA( IP1 )*
     $                         DELTA( IP1 )*( DPSI+DPHI )
                        ELSE
                           A = Z( IP1 )*Z( IP1 ) +
     $                         DELTA( I )*DELTA( I )*( DPSI+DPHI )
                        END IF
                     ELSE
                        A = DELTA( I )*DELTA( I )*DPSI +
     $                      DELTA( IP1 )*DELTA( IP1 )*DPHI
                     END IF
                  END IF
                  OPS = OPS + 1
                  ETA = B / A
               ELSE IF( A.LE.ZERO ) THEN
                  OPS = OPS + 8
                  ETA = ( A-SQRT( ABS( A*A-FOUR*B*C ) ) ) / ( TWO*C )
               ELSE
                  OPS = OPS + 8
                  ETA = TWO*B / ( A+SQRT( ABS( A*A-FOUR*B*C ) ) )
               END IF
            ELSE
*
*              Interpolation using THREE most relevant poles
*
               OPS = OPS + 2
               TEMP = RHOINV + PSI + PHI
               IF( SWTCH ) THEN
                  OPS = OPS + 10
                  C = TEMP - DELTA( IIM1 )*DPSI - DELTA( IIP1 )*DPHI
                  ZZ( 1 ) = DELTA( IIM1 )*DELTA( IIM1 )*DPSI
                  ZZ( 3 ) = DELTA( IIP1 )*DELTA( IIP1 )*DPHI
               ELSE
                  OPS = OPS + 14
                  IF( ORGATI ) THEN
                     TEMP1 = Z( IIM1 ) / DELTA( IIM1 )
                     TEMP1 = TEMP1*TEMP1
                     C = TEMP - DELTA( IIP1 )*( DPSI+DPHI ) -
     $                   ( D( IIM1 )-D( IIP1 ) )*TEMP1
                     ZZ( 1 ) = Z( IIM1 )*Z( IIM1 )
                     ZZ( 3 ) = DELTA( IIP1 )*DELTA( IIP1 )*
     $                         ( ( DPSI-TEMP1 )+DPHI )
                  ELSE
                     TEMP1 = Z( IIP1 ) / DELTA( IIP1 )
                     TEMP1 = TEMP1*TEMP1
                     C = TEMP - DELTA( IIM1 )*( DPSI+DPHI ) -
     $                   ( D( IIP1 )-D( IIM1 ) )*TEMP1
                     ZZ( 1 ) = DELTA( IIM1 )*DELTA( IIM1 )*
     $                         ( DPSI+( DPHI-TEMP1 ) )
                     ZZ( 3 ) = Z( IIP1 )*Z( IIP1 )
                  END IF
               END IF
               CALL DLAED6( NITER, ORGATI, C, DELTA( IIM1 ), ZZ, W, ETA,
     $                      INFO )
               IF( INFO.NE.0 )
     $            GO TO 250
            END IF
*
*           Note, eta should be positive if w is negative, and
*           eta should be negative otherwise. However,
*           if for some reason caused by roundoff, eta*w > 0,
*           we simply use one Newton step instead. This way
*           will guarantee eta*w < 0.
*
            OPS = OPS + 7*N + 6*( IIM1-IIP1 ) + 18
            IF( W*ETA.GE.ZERO ) THEN
               OPS = OPS + 1
               ETA = -W / DW
            END IF
            TEMP = TAU + ETA
            DEL = ( D( IP1 )-D( I ) ) / TWO
            IF( ORGATI ) THEN
               IF( TEMP.GE.DEL ) THEN
                  ETA = DEL - TAU
                  OPS = OPS + 1
               END IF
               IF( TEMP.LE.ZERO ) THEN
                  ETA = ETA / TWO
                  OPS = OPS + 1
               END IF
            ELSE
               IF( TEMP.LE.-DEL ) THEN
                  ETA = -DEL - TAU
                  OPS = OPS + 1
               END IF
               IF( TEMP.GE.ZERO ) THEN
                  ETA = ETA / TWO
                  OPS = OPS + 1
               END IF
            END IF
*
            DO 210 J = 1, N
               DELTA( J ) = DELTA( J ) - ETA
  210       CONTINUE
*
            TAU = TAU + ETA
            PREW = W
*
*           Evaluate PSI and the derivative DPSI
*
            DPSI = ZERO
            PSI = ZERO
            ERRETM = ZERO
            DO 220 J = 1, IIM1
               TEMP = Z( J ) / DELTA( J )
               PSI = PSI + Z( J )*TEMP
               DPSI = DPSI + TEMP*TEMP
               ERRETM = ERRETM + PSI
  220       CONTINUE
            ERRETM = ABS( ERRETM )
*
*           Evaluate PHI and the derivative DPHI
*
            DPHI = ZERO
            PHI = ZERO
            DO 230 J = N, IIP1, -1
               TEMP = Z( J ) / DELTA( J )
               PHI = PHI + Z( J )*TEMP
               DPHI = DPHI + TEMP*TEMP
               ERRETM = ERRETM + PHI
  230       CONTINUE
*
            TEMP = Z( II ) / DELTA( II )
            DW = DPSI + DPHI + TEMP*TEMP
            TEMP = Z( II )*TEMP
            W = RHOINV + PHI + PSI + TEMP
            ERRETM = EIGHT*( PHI-PSI ) + ERRETM + TWO*RHOINV +
     $               THREE*ABS( TEMP ) + ABS( TAU )*DW
            IF( W*PREW.GT.ZERO .AND. ABS( W ).GT.ABS( PREW ) / TEN )
     $         SWTCH = .NOT.SWTCH
*
  240    CONTINUE
*
*        Return with INFO = 1, NITER = MAXIT and not converged
*
         INFO = 1
         OPS = OPS + 1
         IF( ORGATI ) THEN
            DLAM = D( I ) + TAU
         ELSE
            DLAM = D( IP1 ) + TAU
         END IF
*
      END IF
*
  250 CONTINUE
      RETURN
*
*     End of DLAED4
*
      END
      SUBROUTINE DLAED5( I, D, Z, DELTA, RHO, DLAM )
*
*  -- LAPACK routine (instrumented to count operations, version 3.0) --
*     Univ. of Tennessee, Oak Ridge National Lab, Argonne National Lab,
*     Courant Institute, NAG Ltd., and Rice University
*     September 30, 1994
*
*     .. Scalar Arguments ..
      INTEGER            I
      DOUBLE PRECISION   DLAM, RHO
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   D( 2 ), DELTA( 2 ), Z( 2 )
*     ..
*     Common block to return operation count and iteration count
*     ITCNT is unchanged, OPS is only incremented
*     .. Common blocks ..
      COMMON             / LATIME / OPS, ITCNT
*     ..
*     .. Scalars in Common ..
      DOUBLE PRECISION   ITCNT, OPS
*     ..
*
*  Purpose
*  =======
*
*  This subroutine computes the I-th eigenvalue of a symmetric rank-one
*  modification of a 2-by-2 diagonal matrix
*
*             diag( D )  +  RHO *  Z * transpose(Z) .
*
*  The diagonal elements in the array D are assumed to satisfy
*
*             D(i) < D(j)  for  i < j .
*
*  We also assume RHO > 0 and that the Euclidean norm of the vector
*  Z is one.
*
*  Arguments
*  =========
*
*  I      (input) INTEGER
*         The index of the eigenvalue to be computed.  I = 1 or I = 2.
*
*  D      (input) DOUBLE PRECISION array, dimension (2)
*         The original eigenvalues.  We assume D(1) < D(2).
*
*  Z      (input) DOUBLE PRECISION array, dimension (2)
*         The components of the updating vector.
*
*  DELTA  (output) DOUBLE PRECISION array, dimension (2)
*         The vector DELTA contains the information necessary
*         to construct the eigenvectors.
*
*  RHO    (input) DOUBLE PRECISION
*         The scalar in the symmetric updating formula.
*
*  DLAM   (output) DOUBLE PRECISION
*         The computed lambda_I, the I-th updated eigenvalue.
*
*  Further Details
*  ===============
*
*  Based on contributions by
*     Ren-Cang Li, Computer Science Division, University of California
*     at Berkeley, USA
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE, TWO, FOUR
      PARAMETER          ( ZERO = 0.0D0, ONE = 1.0D0, TWO = 2.0D0,
     $                   FOUR = 4.0D0 )
*     ..
*     .. Local Scalars ..
      DOUBLE PRECISION   B, C, DEL, TAU, TEMP, W
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, SQRT
*     ..
*     .. Executable Statements ..
*
      DEL = D( 2 ) - D( 1 )
      IF( I.EQ.1 ) THEN
         W = ONE + TWO*RHO*( Z( 2 )*Z( 2 )-Z( 1 )*Z( 1 ) ) / DEL
         IF( W.GT.ZERO ) THEN
            OPS = OPS + 33
            B = DEL + RHO*( Z( 1 )*Z( 1 )+Z( 2 )*Z( 2 ) )
            C = RHO*Z( 1 )*Z( 1 )*DEL
*
*           B > ZERO, always
*
            TAU = TWO*C / ( B+SQRT( ABS( B*B-FOUR*C ) ) )
            DLAM = D( 1 ) + TAU
            DELTA( 1 ) = -Z( 1 ) / TAU
            DELTA( 2 ) = Z( 2 ) / ( DEL-TAU )
         ELSE
            OPS = OPS + 31
            B = -DEL + RHO*( Z( 1 )*Z( 1 )+Z( 2 )*Z( 2 ) )
            C = RHO*Z( 2 )*Z( 2 )*DEL
            IF( B.GT.ZERO ) THEN
               TAU = -TWO*C / ( B+SQRT( B*B+FOUR*C ) )
            ELSE
               TAU = ( B-SQRT( B*B+FOUR*C ) ) / TWO
            END IF
            DLAM = D( 2 ) + TAU
            DELTA( 1 ) = -Z( 1 ) / ( DEL+TAU )
            DELTA( 2 ) = -Z( 2 ) / TAU
         END IF
         TEMP = SQRT( DELTA( 1 )*DELTA( 1 )+DELTA( 2 )*DELTA( 2 ) )
         DELTA( 1 ) = DELTA( 1 ) / TEMP
         DELTA( 2 ) = DELTA( 2 ) / TEMP
      ELSE
*
*     Now I=2
*
         OPS = OPS + 24
         B = -DEL + RHO*( Z( 1 )*Z( 1 )+Z( 2 )*Z( 2 ) )
         C = RHO*Z( 2 )*Z( 2 )*DEL
         IF( B.GT.ZERO ) THEN
            TAU = ( B+SQRT( B*B+FOUR*C ) ) / TWO
         ELSE
            TAU = TWO*C / ( -B+SQRT( B*B+FOUR*C ) )
         END IF
         DLAM = D( 2 ) + TAU
         DELTA( 1 ) = -Z( 1 ) / ( DEL+TAU )
         DELTA( 2 ) = -Z( 2 ) / TAU
         TEMP = SQRT( DELTA( 1 )*DELTA( 1 )+DELTA( 2 )*DELTA( 2 ) )
         DELTA( 1 ) = DELTA( 1 ) / TEMP
         DELTA( 2 ) = DELTA( 2 ) / TEMP
      END IF
      RETURN
*
*     End OF DLAED5
*
      END
      SUBROUTINE DLAED6( KNITER, ORGATI, RHO, D, Z, FINIT, TAU, INFO )
*
*  -- LAPACK routine (instrumented to count operations, version 3.0) --
*     Univ. of Tennessee, Oak Ridge National Lab, Argonne National Lab,
*     Courant Institute, NAG Ltd., and Rice University
*     June 30, 1999
*
*     .. Scalar Arguments ..
      LOGICAL            ORGATI
      INTEGER            INFO, KNITER
      DOUBLE PRECISION   FINIT, RHO, TAU
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   D( 3 ), Z( 3 )
*     ..
*     Common block to return operation count and iteration count
*     ITCNT is unchanged, OPS is only incremented
*     .. Common blocks ..
      COMMON             / LATIME / OPS, ITCNT
*     ..
*     .. Scalars in Common ..
      DOUBLE PRECISION   ITCNT, OPS
*     ..
*
*  Purpose
*  =======
*
*  DLAED6 computes the positive or negative root (closest to the origin)
*  of
*                   z(1)        z(2)        z(3)
*  f(x) =   rho + --------- + ---------- + ---------
*                  d(1)-x      d(2)-x      d(3)-x
*
*  It is assumed that
*
*        if ORGATI = .true. the root is between d(2) and d(3);
*        otherwise it is between d(1) and d(2)
*
*  This routine will be called by DLAED4 when necessary. In most cases,
*  the root sought is the smallest in magnitude, though it might not be
*  in some extremely rare situations.
*
*  Arguments
*  =========
*
*  KNITER       (input) INTEGER
*               Refer to DLAED4 for its significance.
*
*  ORGATI       (input) LOGICAL
*               If ORGATI is true, the needed root is between d(2) and
*               d(3); otherwise it is between d(1) and d(2).  See
*               DLAED4 for further details.
*
*  RHO          (input) DOUBLE PRECISION
*               Refer to the equation f(x) above.
*
*  D            (input) DOUBLE PRECISION array, dimension (3)
*               D satisfies d(1) < d(2) < d(3).
*
*  Z            (input) DOUBLE PRECISION array, dimension (3)
*               Each of the elements in z must be positive.
*
*  FINIT        (input) DOUBLE PRECISION
*               The value of f at 0. It is more accurate than the one
*               evaluated inside this routine (if someone wants to do
*               so).
*
*  TAU          (output) DOUBLE PRECISION
*               The root of the equation f(x).
*
*  INFO         (output) INTEGER
*               = 0: successful exit
*               > 0: if INFO = 1, failure to converge
*
*  Further Details
*  ===============
*
*  Based on contributions by
*     Ren-Cang Li, Computer Science Division, University of California
*     at Berkeley, USA
*
*  =====================================================================
*
*     .. Parameters ..
      INTEGER            MAXIT
      PARAMETER          ( MAXIT = 20 )
      DOUBLE PRECISION   ZERO, ONE, TWO, THREE, FOUR, EIGHT
      PARAMETER          ( ZERO = 0.0D0, ONE = 1.0D0, TWO = 2.0D0,
     $                   THREE = 3.0D0, FOUR = 4.0D0, EIGHT = 8.0D0 )
*     ..
*     .. External Functions ..
      DOUBLE PRECISION   DLAMCH
      EXTERNAL           DLAMCH
*     ..
*     .. Local Arrays ..
      DOUBLE PRECISION   DSCALE( 3 ), ZSCALE( 3 )
*     ..
*     .. Local Scalars ..
      LOGICAL            FIRST, SCALE
      INTEGER            I, ITER, NITER
      DOUBLE PRECISION   A, B, BASE, C, DDF, DF, EPS, ERRETM, ETA, F,
     $                   FC, SCLFAC, SCLINV, SMALL1, SMALL2, SMINV1,
     $                   SMINV2, TEMP, TEMP1, TEMP2, TEMP3, TEMP4
*     ..
*     .. Save statement ..
      SAVE               FIRST, SMALL1, SMINV1, SMALL2, SMINV2, EPS
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, INT, LOG, MAX, MIN, SQRT
*     ..
*     .. Data statements ..
      DATA               FIRST / .TRUE. /
*     ..
*     .. Executable Statements ..
*
      INFO = 0
*
      NITER = 1
      TAU = ZERO
      IF( KNITER.EQ.2 ) THEN
         IF( ORGATI ) THEN
            TEMP = ( D( 3 )-D( 2 ) ) / TWO
            C = RHO + Z( 1 ) / ( ( D( 1 )-D( 2 ) )-TEMP )
            A = C*( D( 2 )+D( 3 ) ) + Z( 2 ) + Z( 3 )
            B = C*D( 2 )*D( 3 ) + Z( 2 )*D( 3 ) + Z( 3 )*D( 2 )
         ELSE
            TEMP = ( D( 1 )-D( 2 ) ) / TWO
            C = RHO + Z( 3 ) / ( ( D( 3 )-D( 2 ) )-TEMP )
            A = C*( D( 1 )+D( 2 ) ) + Z( 1 ) + Z( 2 )
            B = C*D( 1 )*D( 2 ) + Z( 1 )*D( 2 ) + Z( 2 )*D( 1 )
         END IF
         TEMP = MAX( ABS( A ), ABS( B ), ABS( C ) )
         A = A / TEMP
         B = B / TEMP
         C = C / TEMP
         OPS = OPS + 19
         IF( C.EQ.ZERO ) THEN
            TAU = B / A
            OPS = OPS + 1
         ELSE IF( A.LE.ZERO ) THEN
            OPS = OPS + 8
            TAU = ( A-SQRT( ABS( A*A-FOUR*B*C ) ) ) / ( TWO*C )
         ELSE
            OPS = OPS + 8
            TAU = TWO*B / ( A+SQRT( ABS( A*A-FOUR*B*C ) ) )
         END IF
         OPS = OPS + 9
         TEMP = RHO + Z( 1 ) / ( D( 1 )-TAU ) +
     $          Z( 2 ) / ( D( 2 )-TAU ) + Z( 3 ) / ( D( 3 )-TAU )
         IF( ABS( FINIT ).LE.ABS( TEMP ) )
     $      TAU = ZERO
      END IF
*
*     On first call to routine, get machine parameters for
*     possible scaling to avoid overflow
*
      IF( FIRST ) THEN
         EPS = DLAMCH( 'Epsilon' )
         BASE = DLAMCH( 'Base' )
         SMALL1 = BASE**( INT( LOG( DLAMCH( 'SafMin' ) ) / LOG( BASE ) /
     $            THREE ) )
         SMINV1 = ONE / SMALL1
         SMALL2 = SMALL1*SMALL1
         SMINV2 = SMINV1*SMINV1
         FIRST = .FALSE.
      END IF
*
*     Determine if scaling of inputs necessary to avoid overflow
*     when computing 1/TEMP**3
*
      OPS = OPS + 2
      IF( ORGATI ) THEN
         TEMP = MIN( ABS( D( 2 )-TAU ), ABS( D( 3 )-TAU ) )
      ELSE
         TEMP = MIN( ABS( D( 1 )-TAU ), ABS( D( 2 )-TAU ) )
      END IF
      SCALE = .FALSE.
      IF( TEMP.LE.SMALL1 ) THEN
         SCALE = .TRUE.
         IF( TEMP.LE.SMALL2 ) THEN
*
*        Scale up by power of radix nearest 1/SAFMIN**(2/3)
*
            SCLFAC = SMINV2
            SCLINV = SMALL2
         ELSE
*
*        Scale up by power of radix nearest 1/SAFMIN**(1/3)
*
            SCLFAC = SMINV1
            SCLINV = SMALL1
         END IF
*
*        Scaling up safe because D, Z, TAU scaled elsewhere to be O(1)
*
         OPS = OPS + 7
         DO 10 I = 1, 3
            DSCALE( I ) = D( I )*SCLFAC
            ZSCALE( I ) = Z( I )*SCLFAC
   10    CONTINUE
         TAU = TAU*SCLFAC
      ELSE
*
*        Copy D and Z to DSCALE and ZSCALE
*
         DO 20 I = 1, 3
            DSCALE( I ) = D( I )
            ZSCALE( I ) = Z( I )
   20    CONTINUE
      END IF
*
      FC = ZERO
      DF = ZERO
      DDF = ZERO
      OPS = OPS + 11
      DO 30 I = 1, 3
         TEMP = ONE / ( DSCALE( I )-TAU )
         TEMP1 = ZSCALE( I )*TEMP
         TEMP2 = TEMP1*TEMP
         TEMP3 = TEMP2*TEMP
         FC = FC + TEMP1 / DSCALE( I )
         DF = DF + TEMP2
         DDF = DDF + TEMP3
   30 CONTINUE
      F = FINIT + TAU*FC
*
      IF( ABS( F ).LE.ZERO )
     $   GO TO 60
*
*        Iteration begins
*
*     It is not hard to see that
*
*           1) Iterations will go up monotonically
*              if FINIT < 0;
*
*           2) Iterations will go down monotonically
*              if FINIT > 0.
*
      ITER = NITER + 1
*
      DO 50 NITER = ITER, MAXIT
*
         OPS = OPS + 18
         IF( ORGATI ) THEN
            TEMP1 = DSCALE( 2 ) - TAU
            TEMP2 = DSCALE( 3 ) - TAU
         ELSE
            TEMP1 = DSCALE( 1 ) - TAU
            TEMP2 = DSCALE( 2 ) - TAU
         END IF
         A = ( TEMP1+TEMP2 )*F - TEMP1*TEMP2*DF
         B = TEMP1*TEMP2*F
         C = F - ( TEMP1+TEMP2 )*DF + TEMP1*TEMP2*DDF
         TEMP = MAX( ABS( A ), ABS( B ), ABS( C ) )
         A = A / TEMP
         B = B / TEMP
         C = C / TEMP
         IF( C.EQ.ZERO ) THEN
            OPS = OPS + 1
            ETA = B / A
         ELSE IF( A.LE.ZERO ) THEN
            OPS = OPS + 8
            ETA = ( A-SQRT( ABS( A*A-FOUR*B*C ) ) ) / ( TWO*C )
         ELSE
            OPS = OPS + 8
            ETA = TWO*B / ( A+SQRT( ABS( A*A-FOUR*B*C ) ) )
         END IF
         IF( F*ETA.GE.ZERO ) THEN
            OPS = OPS + 1
            ETA = -F / DF
         END IF
*
         OPS = OPS + 1
         TEMP = ETA + TAU
         IF( ORGATI ) THEN
            IF( ETA.GT.ZERO .AND. TEMP.GE.DSCALE( 3 ) ) THEN
               OPS = OPS + 2
               ETA = ( DSCALE( 3 )-TAU ) / TWO
            END IF
            IF( ETA.LT.ZERO .AND. TEMP.LE.DSCALE( 2 ) ) THEN
               OPS = OPS + 2
               ETA = ( DSCALE( 2 )-TAU ) / TWO
            END IF
         ELSE
            IF( ETA.GT.ZERO .AND. TEMP.GE.DSCALE( 2 ) ) THEN
               OPS = OPS + 2
               ETA = ( DSCALE( 2 )-TAU ) / TWO
            END IF
            IF( ETA.LT.ZERO .AND. TEMP.LE.DSCALE( 1 ) ) THEN
               OPS = OPS + 2
               ETA = ( DSCALE( 1 )-TAU ) / TWO
            END IF
         END IF
         OPS = OPS + 1
         TAU = TAU + ETA
*
         FC = ZERO
         ERRETM = ZERO
         DF = ZERO
         DDF = ZERO
         OPS = OPS + 37
         DO 40 I = 1, 3
            TEMP = ONE / ( DSCALE( I )-TAU )
            TEMP1 = ZSCALE( I )*TEMP
            TEMP2 = TEMP1*TEMP
            TEMP3 = TEMP2*TEMP
            TEMP4 = TEMP1 / DSCALE( I )
            FC = FC + TEMP4
            ERRETM = ERRETM + ABS( TEMP4 )
            DF = DF + TEMP2
            DDF = DDF + TEMP3
   40    CONTINUE
         F = FINIT + TAU*FC
         ERRETM = EIGHT*( ABS( FINIT )+ABS( TAU )*ERRETM ) +
     $            ABS( TAU )*DF
         IF( ABS( F ).LE.EPS*ERRETM )
     $      GO TO 60
   50 CONTINUE
      INFO = 1
   60 CONTINUE
*
*     Undo scaling
*
      IF( SCALE ) THEN
         OPS = OPS + 1
         TAU = TAU*SCLINV
      END IF
      RETURN
*
*     End of DLAED6
*
      END
      SUBROUTINE DLAED7( ICOMPQ, N, QSIZ, TLVLS, CURLVL, CURPBM, D, Q,
     $                   LDQ, INDXQ, RHO, CUTPNT, QSTORE, QPTR, PRMPTR,
     $                   PERM, GIVPTR, GIVCOL, GIVNUM, WORK, IWORK,
     $                   INFO )
*
*  -- LAPACK routine (instrumented to count operations, version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     September 30, 1994
*
*     .. Scalar Arguments ..
      INTEGER            CURLVL, CURPBM, CUTPNT, ICOMPQ, INFO, LDQ, N,
     $                   QSIZ, TLVLS
      DOUBLE PRECISION   RHO
*     ..
*     .. Array Arguments ..
      INTEGER            GIVCOL( 2, * ), GIVPTR( * ), INDXQ( * ),
     $                   IWORK( * ), PERM( * ), PRMPTR( * ), QPTR( * )
      DOUBLE PRECISION   D( * ), GIVNUM( 2, * ), Q( LDQ, * ),
     $                   QSTORE( * ), WORK( * )
*     ..
*     Common block to return operation count and iteration count
*     ITCNT is unchanged, OPS is only incremented
*     .. Common blocks ..
      COMMON             / LATIME / OPS, ITCNT
*     ..
*     .. Scalars in Common ..
      DOUBLE PRECISION   ITCNT, OPS
*     ..
*
*  Purpose
*  =======
*
*  DLAED7 computes the updated eigensystem of a diagonal
*  matrix after modification by a rank-one symmetric matrix. This
*  routine is used only for the eigenproblem which requires all
*  eigenvalues and optionally eigenvectors of a dense symmetric matrix
*  that has been reduced to tridiagonal form.  DLAED1 handles
*  the case in which all eigenvalues and eigenvectors of a symmetric
*  tridiagonal matrix are desired.
*
*    T = Q(in) ( D(in) + RHO * Z*Z' ) Q'(in) = Q(out) * D(out) * Q'(out)
*
*     where Z = Q'u, u is a vector of length N with ones in the
*     CUTPNT and CUTPNT + 1 th elements and zeros elsewhere.
*
*     The eigenvectors of the original matrix are stored in Q, and the
*     eigenvalues are in D.  The algorithm consists of three stages:
*
*        The first stage consists of deflating the size of the problem
*        when there are multiple eigenvalues or if there is a zero in
*        the Z vector.  For each such occurence the dimension of the
*        secular equation problem is reduced by one.  This stage is
*        performed by the routine DLAED8.
*
*        The second stage consists of calculating the updated
*        eigenvalues. This is done by finding the roots of the secular
*        equation via the routine DLAED4 (as called by DLAED9).
*        This routine also calculates the eigenvectors of the current
*        problem.
*
*        The final stage consists of computing the updated eigenvectors
*        directly using the updated eigenvalues.  The eigenvectors for
*        the current problem are multiplied with the eigenvectors from
*        the overall problem.
*
*  Arguments
*  =========
*
*  ICOMPQ  (input) INTEGER
*          = 0:  Compute eigenvalues only.
*          = 1:  Compute eigenvectors of original dense symmetric matrix
*                also.  On entry, Q contains the orthogonal matrix used
*                to reduce the original matrix to tridiagonal form.
*
*  N      (input) INTEGER
*         The dimension of the symmetric tridiagonal matrix.  N >= 0.
*
*  QSIZ   (input) INTEGER
*         The dimension of the orthogonal matrix used to reduce
*         the full matrix to tridiagonal form.  QSIZ >= N if ICOMPQ = 1.
*
*  TLVLS  (input) INTEGER
*         The total number of merging levels in the overall divide and
*         conquer tree.
*
*  CURLVL (input) INTEGER
*         The current level in the overall merge routine,
*         0 <= CURLVL <= TLVLS.
*
*  CURPBM (input) INTEGER
*         The current problem in the current level in the overall
*         merge routine (counting from upper left to lower right).
*
*  D      (input/output) DOUBLE PRECISION array, dimension (N)
*         On entry, the eigenvalues of the rank-1-perturbed matrix.
*         On exit, the eigenvalues of the repaired matrix.
*
*  Q      (input/output) DOUBLE PRECISION array, dimension (LDQ, N)
*         On entry, the eigenvectors of the rank-1-perturbed matrix.
*         On exit, the eigenvectors of the repaired tridiagonal matrix.
*
*  LDQ    (input) INTEGER
*         The leading dimension of the array Q.  LDQ >= max(1,N).
*
*  INDXQ  (output) INTEGER array, dimension (N)
*         The permutation which will reintegrate the subproblem just
*         solved back into sorted order, i.e., D( INDXQ( I = 1, N ) )
*         will be in ascending order.
*
*  RHO    (input) DOUBLE PRECISION
*         The subdiagonal element used to create the rank-1
*         modification.
*
*  CUTPNT (input) INTEGER
*         Contains the location of the last eigenvalue in the leading
*         sub-matrix.  min(1,N) <= CUTPNT <= N.
*
*  QSTORE (input/output) DOUBLE PRECISION array, dimension (N**2+1)
*         Stores eigenvectors of submatrices encountered during
*         divide and conquer, packed together. QPTR points to
*         beginning of the submatrices.
*
*  QPTR   (input/output) INTEGER array, dimension (N+2)
*         List of indices pointing to beginning of submatrices stored
*         in QSTORE. The submatrices are numbered starting at the
*         bottom left of the divide and conquer tree, from left to
*         right and bottom to top.
*
*  PRMPTR (input) INTEGER array, dimension (N lg N)
*         Contains a list of pointers which indicate where in PERM a
*         level's permutation is stored.  PRMPTR(i+1) - PRMPTR(i)
*         indicates the size of the permutation and also the size of
*         the full, non-deflated problem.
*
*  PERM   (input) INTEGER array, dimension (N lg N)
*         Contains the permutations (from deflation and sorting) to be
*         applied to each eigenblock.
*
*  GIVPTR (input) INTEGER array, dimension (N lg N)
*         Contains a list of pointers which indicate where in GIVCOL a
*         level's Givens rotations are stored.  GIVPTR(i+1) - GIVPTR(i)
*         indicates the number of Givens rotations.
*
*  GIVCOL (input) INTEGER array, dimension (2, N lg N)
*         Each pair of numbers indicates a pair of columns to take place
*         in a Givens rotation.
*
*  GIVNUM (input) DOUBLE PRECISION array, dimension (2, N lg N)
*         Each number indicates the S value to be used in the
*         corresponding Givens rotation.
*
*  WORK   (workspace) DOUBLE PRECISION array, dimension (3*N+QSIZ*N)
*
*  IWORK  (workspace) INTEGER array, dimension (4*N)
*
*  INFO   (output) INTEGER
*          = 0:  successful exit.
*          < 0:  if INFO = -i, the i-th argument had an illegal value.
*          > 0:  if INFO = 1, an eigenvalue did not converge
*
*  Further Details
*  ===============
*
*  Based on contributions by
*     Jeff Rutter, Computer Science Division, University of California
*     at Berkeley, USA
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ONE, ZERO
      PARAMETER          ( ONE = 1.0D0, ZERO = 0.0D0 )
*     ..
*     .. Local Scalars ..
      INTEGER            COLTYP, CURR, I, IDLMDA, INDX, INDXC, INDXP,
     $                   IQ2, IS, IW, IZ, K, LDQ2, N1, N2, PTR
*     ..
*     .. External Subroutines ..
      EXTERNAL           DGEMM, DLAED8, DLAED9, DLAEDA, DLAMRG, XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          DBLE, MAX, MIN
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters.
*
      INFO = 0
*
      IF( ICOMPQ.LT.0 .OR. ICOMPQ.GT.1 ) THEN
         INFO = -1
      ELSE IF( N.LT.0 ) THEN
         INFO = -2
      ELSE IF( ICOMPQ.EQ.1 .AND. QSIZ.LT.N ) THEN
         INFO = -4
      ELSE IF( LDQ.LT.MAX( 1, N ) ) THEN
         INFO = -9
      ELSE IF( MIN( 1, N ).GT.CUTPNT .OR. N.LT.CUTPNT ) THEN
         INFO = -12
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DLAED7', -INFO )
         RETURN
      END IF
*
*     Quick return if possible
*
      IF( N.EQ.0 )
     $   RETURN
*
*     The following values are for bookkeeping purposes only.  They are
*     integer pointers which indicate the portion of the workspace
*     used by a particular array in DLAED8 and DLAED9.
*
      IF( ICOMPQ.EQ.1 ) THEN
         LDQ2 = QSIZ
      ELSE
         LDQ2 = N
      END IF
*
      IZ = 1
      IDLMDA = IZ + N
      IW = IDLMDA + N
      IQ2 = IW + N
      IS = IQ2 + N*LDQ2
*
      INDX = 1
      INDXC = INDX + N
      COLTYP = INDXC + N
      INDXP = COLTYP + N
*
*     Form the z-vector which consists of the last row of Q_1 and the
*     first row of Q_2.
*
      PTR = 1 + 2**TLVLS
      DO 10 I = 1, CURLVL - 1
         PTR = PTR + 2**( TLVLS-I )
   10 CONTINUE
      CURR = PTR + CURPBM
      CALL DLAEDA( N, TLVLS, CURLVL, CURPBM, PRMPTR, PERM, GIVPTR,
     $             GIVCOL, GIVNUM, QSTORE, QPTR, WORK( IZ ),
     $             WORK( IZ+N ), INFO )
*
*     When solving the final problem, we no longer need the stored data,
*     so we will overwrite the data from this level onto the previously
*     used storage space.
*
      IF( CURLVL.EQ.TLVLS ) THEN
         QPTR( CURR ) = 1
         PRMPTR( CURR ) = 1
         GIVPTR( CURR ) = 1
      END IF
*
*     Sort and Deflate eigenvalues.
*
      CALL DLAED8( ICOMPQ, K, N, QSIZ, D, Q, LDQ, INDXQ, RHO, CUTPNT,
     $             WORK( IZ ), WORK( IDLMDA ), WORK( IQ2 ), LDQ2,
     $             WORK( IW ), PERM( PRMPTR( CURR ) ), GIVPTR( CURR+1 ),
     $             GIVCOL( 1, GIVPTR( CURR ) ),
     $             GIVNUM( 1, GIVPTR( CURR ) ), IWORK( INDXP ),
     $             IWORK( INDX ), INFO )
      PRMPTR( CURR+1 ) = PRMPTR( CURR ) + N
      GIVPTR( CURR+1 ) = GIVPTR( CURR+1 ) + GIVPTR( CURR )
*
*     Solve Secular Equation.
*
      IF( K.NE.0 ) THEN
         CALL DLAED9( K, 1, K, N, D, WORK( IS ), K, RHO, WORK( IDLMDA ),
     $                WORK( IW ), QSTORE( QPTR( CURR ) ), K, INFO )
         IF( INFO.NE.0 )
     $      GO TO 30
         IF( ICOMPQ.EQ.1 ) THEN
            OPS = OPS + 2*DBLE( QSIZ )*K*K
            CALL DGEMM( 'N', 'N', QSIZ, K, K, ONE, WORK( IQ2 ), LDQ2,
     $                  QSTORE( QPTR( CURR ) ), K, ZERO, Q, LDQ )
         END IF
         QPTR( CURR+1 ) = QPTR( CURR ) + K**2
*
*     Prepare the INDXQ sorting permutation.
*
         N1 = K
         N2 = N - K
         CALL DLAMRG( N1, N2, D, 1, -1, INDXQ )
      ELSE
         QPTR( CURR+1 ) = QPTR( CURR )
         DO 20 I = 1, N
            INDXQ( I ) = I
   20    CONTINUE
      END IF
*
   30 CONTINUE
      RETURN
*
*     End of DLAED7
*
      END
      SUBROUTINE DLAED8( ICOMPQ, K, N, QSIZ, D, Q, LDQ, INDXQ, RHO,
     $                   CUTPNT, Z, DLAMDA, Q2, LDQ2, W, PERM, GIVPTR,
     $                   GIVCOL, GIVNUM, INDXP, INDX, INFO )
*
*  -- LAPACK routine (instrumented to count operations, version 3.0) --
*     Univ. of Tennessee, Oak Ridge National Lab, Argonne National Lab,
*     Courant Institute, NAG Ltd., and Rice University
*     September 30, 1994
*
*     .. Scalar Arguments ..
      INTEGER            CUTPNT, GIVPTR, ICOMPQ, INFO, K, LDQ, LDQ2, N,
     $                   QSIZ
      DOUBLE PRECISION   RHO
*     ..
*     .. Array Arguments ..
      INTEGER            GIVCOL( 2, * ), INDX( * ), INDXP( * ),
     $                   INDXQ( * ), PERM( * )
      DOUBLE PRECISION   D( * ), DLAMDA( * ), GIVNUM( 2, * ),
     $                   Q( LDQ, * ), Q2( LDQ2, * ), W( * ), Z( * )
*     ..
*     Common block to return operation count and iteration count
*     ITCNT is unchanged, OPS is only incremented
*     .. Common blocks ..
      COMMON             / LATIME / OPS, ITCNT
*     ..
*     .. Scalars in Common ..
      DOUBLE PRECISION   ITCNT, OPS
*     ..
*
*  Purpose
*  =======
*
*  DLAED8 merges the two sets of eigenvalues together into a single
*  sorted set.  Then it tries to deflate the size of the problem.
*  There are two ways in which deflation can occur:  when two or more
*  eigenvalues are close together or if there is a tiny element in the
*  Z vector.  For each such occurrence the order of the related secular
*  equation problem is reduced by one.
*
*  Arguments
*  =========
*
*  ICOMPQ  (input) INTEGER
*          = 0:  Compute eigenvalues only.
*          = 1:  Compute eigenvectors of original dense symmetric matrix
*                also.  On entry, Q contains the orthogonal matrix used
*                to reduce the original matrix to tridiagonal form.
*
*  K      (output) INTEGER
*         The number of non-deflated eigenvalues, and the order of the
*         related secular equation.
*
*  N      (input) INTEGER
*         The dimension of the symmetric tridiagonal matrix.  N >= 0.
*
*  QSIZ   (input) INTEGER
*         The dimension of the orthogonal matrix used to reduce
*         the full matrix to tridiagonal form.  QSIZ >= N if ICOMPQ = 1.
*
*  D      (input/output) DOUBLE PRECISION array, dimension (N)
*         On entry, the eigenvalues of the two submatrices to be
*         combined.  On exit, the trailing (N-K) updated eigenvalues
*         (those which were deflated) sorted into increasing order.
*
*  Q      (input/output) DOUBLE PRECISION array, dimension (LDQ,N)
*         If ICOMPQ = 0, Q is not referenced.  Otherwise,
*         on entry, Q contains the eigenvectors of the partially solved
*         system which has been previously updated in matrix
*         multiplies with other partially solved eigensystems.
*         On exit, Q contains the trailing (N-K) updated eigenvectors
*         (those which were deflated) in its last N-K columns.
*
*  LDQ    (input) INTEGER
*         The leading dimension of the array Q.  LDQ >= max(1,N).
*
*  INDXQ  (input) INTEGER array, dimension (N)
*         The permutation which separately sorts the two sub-problems
*         in D into ascending order.  Note that elements in the second
*         half of this permutation must first have CUTPNT added to
*         their values in order to be accurate.
*
*  RHO    (input/output) DOUBLE PRECISION
*         On entry, the off-diagonal element associated with the rank-1
*         cut which originally split the two submatrices which are now
*         being recombined.
*         On exit, RHO has been modified to the value required by
*         DLAED3.
*
*  CUTPNT (input) INTEGER
*         The location of the last eigenvalue in the leading
*         sub-matrix.  min(1,N) <= CUTPNT <= N.
*
*  Z      (input) DOUBLE PRECISION array, dimension (N)
*         On entry, Z contains the updating vector (the last row of
*         the first sub-eigenvector matrix and the first row of the
*         second sub-eigenvector matrix).
*         On exit, the contents of Z are destroyed by the updating
*         process.
*
*  DLAMDA (output) DOUBLE PRECISION array, dimension (N)
*         A copy of the first K eigenvalues which will be used by
*         DLAED3 to form the secular equation.
*
*  Q2     (output) DOUBLE PRECISION array, dimension (LDQ2,N)
*         If ICOMPQ = 0, Q2 is not referenced.  Otherwise,
*         a copy of the first K eigenvectors which will be used by
*         DLAED7 in a matrix multiply (DGEMM) to update the new
*         eigenvectors.
*
*  LDQ2   (input) INTEGER
*         The leading dimension of the array Q2.  LDQ2 >= max(1,N).
*
*  W      (output) DOUBLE PRECISION array, dimension (N)
*         The first k values of the final deflation-altered z-vector and
*         will be passed to DLAED3.
*
*  PERM   (output) INTEGER array, dimension (N)
*         The permutations (from deflation and sorting) to be applied
*         to each eigenblock.
*
*  GIVPTR (output) INTEGER
*         The number of Givens rotations which took place in this
*         subproblem.
*
*  GIVCOL (output) INTEGER array, dimension (2, N)
*         Each pair of numbers indicates a pair of columns to take place
*         in a Givens rotation.
*
*  GIVNUM (output) DOUBLE PRECISION array, dimension (2, N)
*         Each number indicates the S value to be used in the
*         corresponding Givens rotation.
*
*  INDXP  (workspace) INTEGER array, dimension (N)
*         The permutation used to place deflated values of D at the end
*         of the array.  INDXP(1:K) points to the nondeflated D-values
*         and INDXP(K+1:N) points to the deflated eigenvalues.
*
*  INDX   (workspace) INTEGER array, dimension (N)
*         The permutation used to sort the contents of D into ascending
*         order.
*
*  INFO   (output) INTEGER
*          = 0:  successful exit.
*          < 0:  if INFO = -i, the i-th argument had an illegal value.
*
*  Further Details
*  ===============
*
*  Based on contributions by
*     Jeff Rutter, Computer Science Division, University of California
*     at Berkeley, USA
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   MONE, ZERO, ONE, TWO, EIGHT
      PARAMETER          ( MONE = -1.0D0, ZERO = 0.0D0, ONE = 1.0D0,
     $                   TWO = 2.0D0, EIGHT = 8.0D0 )
*     ..
*     .. Local Scalars ..
*
      INTEGER            I, IMAX, J, JLAM, JMAX, JP, K2, N1, N1P1, N2
      DOUBLE PRECISION   C, EPS, S, T, TAU, TOL
*     ..
*     .. External Functions ..
      INTEGER            IDAMAX
      DOUBLE PRECISION   DLAMCH, DLAPY2
      EXTERNAL           IDAMAX, DLAMCH, DLAPY2
*     ..
*     .. External Subroutines ..
      EXTERNAL           DCOPY, DLACPY, DLAMRG, DROT, DSCAL, XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, MAX, MIN, SQRT
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters.
*
      INFO = 0
*
      IF( ICOMPQ.LT.0 .OR. ICOMPQ.GT.1 ) THEN
         INFO = -1
      ELSE IF( N.LT.0 ) THEN
         INFO = -3
      ELSE IF( ICOMPQ.EQ.1 .AND. QSIZ.LT.N ) THEN
         INFO = -4
      ELSE IF( LDQ.LT.MAX( 1, N ) ) THEN
         INFO = -7
      ELSE IF( CUTPNT.LT.MIN( 1, N ) .OR. CUTPNT.GT.N ) THEN
         INFO = -10
      ELSE IF( LDQ2.LT.MAX( 1, N ) ) THEN
         INFO = -14
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DLAED8', -INFO )
         RETURN
      END IF
*
*     Quick return if possible
*
      IF( N.EQ.0 )
     $   RETURN
*
      N1 = CUTPNT
      N2 = N - N1
      N1P1 = N1 + 1
*
      IF( RHO.LT.ZERO ) THEN
         OPS = OPS + N2
         CALL DSCAL( N2, MONE, Z( N1P1 ), 1 )
      END IF
*
*     Normalize z so that norm(z) = 1
*
      OPS = OPS + N + 6
      T = ONE / SQRT( TWO )
      DO 10 J = 1, N
         INDX( J ) = J
   10 CONTINUE
      CALL DSCAL( N, T, Z, 1 )
      RHO = ABS( TWO*RHO )
*
*     Sort the eigenvalues into increasing order
*
      DO 20 I = CUTPNT + 1, N
         INDXQ( I ) = INDXQ( I ) + CUTPNT
   20 CONTINUE
      DO 30 I = 1, N
         DLAMDA( I ) = D( INDXQ( I ) )
         W( I ) = Z( INDXQ( I ) )
   30 CONTINUE
      I = 1
      J = CUTPNT + 1
      CALL DLAMRG( N1, N2, DLAMDA, 1, 1, INDX )
      DO 40 I = 1, N
         D( I ) = DLAMDA( INDX( I ) )
         Z( I ) = W( INDX( I ) )
   40 CONTINUE
*
*     Calculate the allowable deflation tolerence
*
      IMAX = IDAMAX( N, Z, 1 )
      JMAX = IDAMAX( N, D, 1 )
      EPS = DLAMCH( 'Epsilon' )
      TOL = EIGHT*EPS*ABS( D( JMAX ) )
*
*     If the rank-1 modifier is small enough, no more needs to be done
*     except to reorganize Q so that its columns correspond with the
*     elements in D.
*
      IF( RHO*ABS( Z( IMAX ) ).LE.TOL ) THEN
         K = 0
         IF( ICOMPQ.EQ.0 ) THEN
            DO 50 J = 1, N
               PERM( J ) = INDXQ( INDX( J ) )
   50       CONTINUE
         ELSE
            DO 60 J = 1, N
               PERM( J ) = INDXQ( INDX( J ) )
               CALL DCOPY( QSIZ, Q( 1, PERM( J ) ), 1, Q2( 1, J ), 1 )
   60       CONTINUE
            CALL DLACPY( 'A', QSIZ, N, Q2( 1, 1 ), LDQ2, Q( 1, 1 ),
     $                   LDQ )
         END IF
         RETURN
      END IF
*
*     If there are multiple eigenvalues then the problem deflates.  Here
*     the number of equal eigenvalues are found.  As each equal
*     eigenvalue is found, an elementary reflector is computed to rotate
*     the corresponding eigensubspace so that the corresponding
*     components of Z are zero in this new basis.
*
      K = 0
      GIVPTR = 0
      K2 = N + 1
      DO 70 J = 1, N
         OPS = OPS + 1
         IF( RHO*ABS( Z( J ) ).LE.TOL ) THEN
*
*           Deflate due to small z component.
*
            K2 = K2 - 1
            INDXP( K2 ) = J
            IF( J.EQ.N )
     $         GO TO 110
         ELSE
            JLAM = J
            GO TO 80
         END IF
   70 CONTINUE
   80 CONTINUE
      J = J + 1
      IF( J.GT.N )
     $   GO TO 100
      OPS = OPS + 1
      IF( RHO*ABS( Z( J ) ).LE.TOL ) THEN
*
*        Deflate due to small z component.
*
         K2 = K2 - 1
         INDXP( K2 ) = J
      ELSE
*
*        Check if eigenvalues are close enough to allow deflation.
*
         S = Z( JLAM )
         C = Z( J )
*
*        Find sqrt(a**2+b**2) without overflow or
*        destructive underflow.
*
         OPS = OPS + 10
         TAU = DLAPY2( C, S )
         T = D( J ) - D( JLAM )
         C = C / TAU
         S = -S / TAU
         IF( ABS( T*C*S ).LE.TOL ) THEN
*
*           Deflation is possible.
*
            Z( J ) = TAU
            Z( JLAM ) = ZERO
*
*           Record the appropriate Givens rotation
*
            GIVPTR = GIVPTR + 1
            GIVCOL( 1, GIVPTR ) = INDXQ( INDX( JLAM ) )
            GIVCOL( 2, GIVPTR ) = INDXQ( INDX( J ) )
            GIVNUM( 1, GIVPTR ) = C
            GIVNUM( 2, GIVPTR ) = S
            IF( ICOMPQ.EQ.1 ) THEN
               OPS = OPS + 6*QSIZ
               CALL DROT( QSIZ, Q( 1, INDXQ( INDX( JLAM ) ) ), 1,
     $                    Q( 1, INDXQ( INDX( J ) ) ), 1, C, S )
            END IF
            OPS = OPS + 10
            T = D( JLAM )*C*C + D( J )*S*S
            D( J ) = D( JLAM )*S*S + D( J )*C*C
            D( JLAM ) = T
            K2 = K2 - 1
            I = 1
   90       CONTINUE
            IF( K2+I.LE.N ) THEN
               IF( D( JLAM ).LT.D( INDXP( K2+I ) ) ) THEN
                  INDXP( K2+I-1 ) = INDXP( K2+I )
                  INDXP( K2+I ) = JLAM
                  I = I + 1
                  GO TO 90
               ELSE
                  INDXP( K2+I-1 ) = JLAM
               END IF
            ELSE
               INDXP( K2+I-1 ) = JLAM
            END IF
            JLAM = J
         ELSE
            K = K + 1
            W( K ) = Z( JLAM )
            DLAMDA( K ) = D( JLAM )
            INDXP( K ) = JLAM
            JLAM = J
         END IF
      END IF
      GO TO 80
  100 CONTINUE
*
*     Record the last eigenvalue.
*
      K = K + 1
      W( K ) = Z( JLAM )
      DLAMDA( K ) = D( JLAM )
      INDXP( K ) = JLAM
*
  110 CONTINUE
*
*     Sort the eigenvalues and corresponding eigenvectors into DLAMDA
*     and Q2 respectively.  The eigenvalues/vectors which were not
*     deflated go into the first K slots of DLAMDA and Q2 respectively,
*     while those which were deflated go into the last N - K slots.
*
      IF( ICOMPQ.EQ.0 ) THEN
         DO 120 J = 1, N
            JP = INDXP( J )
            DLAMDA( J ) = D( JP )
            PERM( J ) = INDXQ( INDX( JP ) )
  120    CONTINUE
      ELSE
         DO 130 J = 1, N
            JP = INDXP( J )
            DLAMDA( J ) = D( JP )
            PERM( J ) = INDXQ( INDX( JP ) )
            CALL DCOPY( QSIZ, Q( 1, PERM( J ) ), 1, Q2( 1, J ), 1 )
  130    CONTINUE
      END IF
*
*     The deflated eigenvalues and their corresponding vectors go back
*     into the last N - K slots of D and Q respectively.
*
      IF( K.LT.N ) THEN
         IF( ICOMPQ.EQ.0 ) THEN
            CALL DCOPY( N-K, DLAMDA( K+1 ), 1, D( K+1 ), 1 )
         ELSE
            CALL DCOPY( N-K, DLAMDA( K+1 ), 1, D( K+1 ), 1 )
            CALL DLACPY( 'A', QSIZ, N-K, Q2( 1, K+1 ), LDQ2,
     $                   Q( 1, K+1 ), LDQ )
         END IF
      END IF
*
      RETURN
*
*     End of DLAED8
*
      END
      SUBROUTINE DLAED9( K, KSTART, KSTOP, N, D, Q, LDQ, RHO, DLAMDA, W,
     $                   S, LDS, INFO )
*
*  -- LAPACK routine (instrumented to count operations, version 3.0) --
*     Univ. of Tennessee, Oak Ridge National Lab, Argonne National Lab,
*     Courant Institute, NAG Ltd., and Rice University
*     September 30, 1994
*
*     .. Scalar Arguments ..
      INTEGER            INFO, K, KSTART, KSTOP, LDQ, LDS, N
      DOUBLE PRECISION   RHO
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   D( * ), DLAMDA( * ), Q( LDQ, * ), S( LDS, * ),
     $                   W( * )
*     ..
*     Common block to return operation count and iteration count
*     ITCNT is unchanged, OPS is only incremented
*     .. Common blocks ..
      COMMON             / LATIME / OPS, ITCNT
*     ..
*     .. Scalars in Common ..
      DOUBLE PRECISION   ITCNT, OPS
*     ..
*
*  Purpose
*  =======
*
*  DLAED9 finds the roots of the secular equation, as defined by the
*  values in D, Z, and RHO, between KSTART and KSTOP.  It makes the
*  appropriate calls to DLAED4 and then stores the new matrix of
*  eigenvectors for use in calculating the next level of Z vectors.
*
*  Arguments
*  =========
*
*  K       (input) INTEGER
*          The number of terms in the rational function to be solved by
*          DLAED4.  K >= 0.
*
*  KSTART  (input) INTEGER
*  KSTOP   (input) INTEGER
*          The updated eigenvalues Lambda(I), KSTART <= I <= KSTOP
*          are to be computed.  1 <= KSTART <= KSTOP <= K.
*
*  N       (input) INTEGER
*          The number of rows and columns in the Q matrix.
*          N >= K (delation may result in N > K).
*
*  D       (output) DOUBLE PRECISION array, dimension (N)
*          D(I) contains the updated eigenvalues
*          for KSTART <= I <= KSTOP.
*
*  Q       (workspace) DOUBLE PRECISION array, dimension (LDQ,N)
*
*  LDQ     (input) INTEGER
*          The leading dimension of the array Q.  LDQ >= max( 1, N ).
*
*  RHO     (input) DOUBLE PRECISION
*          The value of the parameter in the rank one update equation.
*          RHO >= 0 required.
*
*  DLAMDA  (input) DOUBLE PRECISION array, dimension (K)
*          The first K elements of this array contain the old roots
*          of the deflated updating problem.  These are the poles
*          of the secular equation.
*
*  W       (input) DOUBLE PRECISION array, dimension (K)
*          The first K elements of this array contain the components
*          of the deflation-adjusted updating vector.
*
*  S       (output) DOUBLE PRECISION array, dimension (LDS, K)
*          Will contain the eigenvectors of the repaired matrix which
*          will be stored for subsequent Z vector calculation and
*          multiplied by the previously accumulated eigenvectors
*          to update the system.
*
*  LDS     (input) INTEGER
*          The leading dimension of S.  LDS >= max( 1, K ).
*
*  INFO    (output) INTEGER
*          = 0:  successful exit.
*          < 0:  if INFO = -i, the i-th argument had an illegal value.
*          > 0:  if INFO = 1, an eigenvalue did not converge
*
*  Further Details
*  ===============
*
*  Based on contributions by
*     Jeff Rutter, Computer Science Division, University of California
*     at Berkeley, USA
*
*  =====================================================================
*
*     .. Local Scalars ..
      INTEGER            I, J
      DOUBLE PRECISION   TEMP
*     ..
*     .. External Functions ..
      DOUBLE PRECISION   DLAMC3, DNRM2
      EXTERNAL           DLAMC3, DNRM2
*     ..
*     .. External Subroutines ..
      EXTERNAL           DCOPY, DLAED4, XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX, SIGN, SQRT
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters.
*
      INFO = 0
*
      IF( K.LT.0 ) THEN
         INFO = -1
      ELSE IF( KSTART.LT.1 .OR. KSTART.GT.MAX( 1, K ) ) THEN
         INFO = -2
      ELSE IF( MAX( 1, KSTOP ).LT.KSTART .OR. KSTOP.GT.MAX( 1, K ) )
     $          THEN
         INFO = -3
      ELSE IF( N.LT.K ) THEN
         INFO = -4
      ELSE IF( LDQ.LT.MAX( 1, K ) ) THEN
         INFO = -7
      ELSE IF( LDS.LT.MAX( 1, K ) ) THEN
         INFO = -12
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DLAED9', -INFO )
         RETURN
      END IF
*
*     Quick return if possible
*
      IF( K.EQ.0 )
     $   RETURN
*
*     Modify values DLAMDA(i) to make sure all DLAMDA(i)-DLAMDA(j) can
*     be computed with high relative accuracy (barring over/underflow).
*     This is a problem on machines without a guard digit in
*     add/subtract (Cray XMP, Cray YMP, Cray C 90 and Cray 2).
*     The following code replaces DLAMDA(I) by 2*DLAMDA(I)-DLAMDA(I),
*     which on any of these machines zeros out the bottommost
*     bit of DLAMDA(I) if it is 1; this makes the subsequent
*     subtractions DLAMDA(I)-DLAMDA(J) unproblematic when cancellation
*     occurs. On binary machines with a guard digit (almost all
*     machines) it does not change DLAMDA(I) at all. On hexadecimal
*     and decimal machines with a guard digit, it slightly
*     changes the bottommost bits of DLAMDA(I). It does not account
*     for hexadecimal or decimal machines without guard digits
*     (we know of none). We use a subroutine call to compute
*     2*DLAMBDA(I) to prevent optimizing compilers from eliminating
*     this code.
*
      OPS = OPS + 2*N
      DO 10 I = 1, N
         DLAMDA( I ) = DLAMC3( DLAMDA( I ), DLAMDA( I ) ) - DLAMDA( I )
   10 CONTINUE
*
      DO 20 J = KSTART, KSTOP
         CALL DLAED4( K, J, DLAMDA, W, Q( 1, J ), RHO, D( J ), INFO )
*
*        If the zero finder fails, the computation is terminated.
*
         IF( INFO.NE.0 )
     $      GO TO 120
   20 CONTINUE
*
      IF( K.EQ.1 .OR. K.EQ.2 ) THEN
         DO 40 I = 1, K
            DO 30 J = 1, K
               S( J, I ) = Q( J, I )
   30       CONTINUE
   40    CONTINUE
         GO TO 120
      END IF
*
*     Compute updated W.
*
      CALL DCOPY( K, W, 1, S, 1 )
*
*     Initialize W(I) = Q(I,I)
*
      CALL DCOPY( K, Q, LDQ+1, W, 1 )
      OPS = OPS + 3*K*( K-1 ) + K
      DO 70 J = 1, K
         DO 50 I = 1, J - 1
            W( I ) = W( I )*( Q( I, J ) / ( DLAMDA( I )-DLAMDA( J ) ) )
   50    CONTINUE
         DO 60 I = J + 1, K
            W( I ) = W( I )*( Q( I, J ) / ( DLAMDA( I )-DLAMDA( J ) ) )
   60    CONTINUE
   70 CONTINUE
      DO 80 I = 1, K
         W( I ) = SIGN( SQRT( -W( I ) ), S( I, 1 ) )
   80 CONTINUE
*
*     Compute eigenvectors of the modified rank-1 modification.
*
      OPS = OPS + 4*K*K
      DO 110 J = 1, K
         DO 90 I = 1, K
            Q( I, J ) = W( I ) / Q( I, J )
   90    CONTINUE
         TEMP = DNRM2( K, Q( 1, J ), 1 )
         DO 100 I = 1, K
            S( I, J ) = Q( I, J ) / TEMP
  100    CONTINUE
  110 CONTINUE
*
  120 CONTINUE
      RETURN
*
*     End of DLAED9
*
      END
      SUBROUTINE DLAEDA( N, TLVLS, CURLVL, CURPBM, PRMPTR, PERM, GIVPTR,
     $                   GIVCOL, GIVNUM, Q, QPTR, Z, ZTEMP, INFO )
*
*  -- LAPACK routine (instrumented to count operations, version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     September 30, 1994
*
*     .. Scalar Arguments ..
      INTEGER            CURLVL, CURPBM, INFO, N, TLVLS
*     ..
*     .. Array Arguments ..
      INTEGER            GIVCOL( 2, * ), GIVPTR( * ), PERM( * ),
     $                   PRMPTR( * ), QPTR( * )
      DOUBLE PRECISION   GIVNUM( 2, * ), Q( * ), Z( * ), ZTEMP( * )
*     ..
*     Common block to return operation count and iteration count
*     ITCNT is unchanged, OPS is only incremented
*     .. Common blocks ..
      COMMON             / LATIME / OPS, ITCNT
*     ..
*     .. Scalars in Common ..
      DOUBLE PRECISION   ITCNT, OPS
*     ..
*
*  Purpose
*  =======
*
*  DLAEDA computes the Z vector corresponding to the merge step in the
*  CURLVLth step of the merge process with TLVLS steps for the CURPBMth
*  problem.
*
*  Arguments
*  =========
*
*  N      (input) INTEGER
*         The dimension of the symmetric tridiagonal matrix.  N >= 0.
*
*  TLVLS  (input) INTEGER
*         The total number of merging levels in the overall divide and
*         conquer tree.
*
*  CURLVL (input) INTEGER
*         The current level in the overall merge routine,
*         0 <= curlvl <= tlvls.
*
*  CURPBM (input) INTEGER
*         The current problem in the current level in the overall
*         merge routine (counting from upper left to lower right).
*
*  PRMPTR (input) INTEGER array, dimension (N lg N)
*         Contains a list of pointers which indicate where in PERM a
*         level's permutation is stored.  PRMPTR(i+1) - PRMPTR(i)
*         indicates the size of the permutation and incidentally the
*         size of the full, non-deflated problem.
*
*  PERM   (input) INTEGER array, dimension (N lg N)
*         Contains the permutations (from deflation and sorting) to be
*         applied to each eigenblock.
*
*  GIVPTR (input) INTEGER array, dimension (N lg N)
*         Contains a list of pointers which indicate where in GIVCOL a
*         level's Givens rotations are stored.  GIVPTR(i+1) - GIVPTR(i)
*         indicates the number of Givens rotations.
*
*  GIVCOL (input) INTEGER array, dimension (2, N lg N)
*         Each pair of numbers indicates a pair of columns to take place
*         in a Givens rotation.
*
*  GIVNUM (input) DOUBLE PRECISION array, dimension (2, N lg N)
*         Each number indicates the S value to be used in the
*         corresponding Givens rotation.
*
*  Q      (input) DOUBLE PRECISION array, dimension (N**2)
*         Contains the square eigenblocks from previous levels, the
*         starting positions for blocks are given by QPTR.
*
*  QPTR   (input) INTEGER array, dimension (N+2)
*         Contains a list of pointers which indicate where in Q an
*         eigenblock is stored.  SQRT( QPTR(i+1) - QPTR(i) ) indicates
*         the size of the block.
*
*  Z      (output) DOUBLE PRECISION array, dimension (N)
*         On output this vector contains the updating vector (the last
*         row of the first sub-eigenvector matrix and the first row of
*         the second sub-eigenvector matrix).
*
*  ZTEMP  (workspace) DOUBLE PRECISION array, dimension (N)
*
*  INFO   (output) INTEGER
*          = 0:  successful exit.
*          < 0:  if INFO = -i, the i-th argument had an illegal value.
*
*  Further Details
*  ===============
*
*  Based on contributions by
*     Jeff Rutter, Computer Science Division, University of California
*     at Berkeley, USA
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO, HALF, ONE
      PARAMETER          ( ZERO = 0.0D0, HALF = 0.5D0, ONE = 1.0D0 )
*     ..
*     .. Local Scalars ..
      INTEGER            BSIZ1, BSIZ2, CURR, I, K, MID, PSIZ1, PSIZ2,
     $                   PTR, ZPTR1
*     ..
*     .. External Subroutines ..
      EXTERNAL           DCOPY, DGEMV, DROT, XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          DBLE, INT, SQRT
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters.
*
      INFO = 0
*
      IF( N.LT.0 ) THEN
         INFO = -1
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DLAEDA', -INFO )
         RETURN
      END IF
*
*     Quick return if possible
*
      IF( N.EQ.0 )
     $   RETURN
*
*     Determine location of first number in second half.
*
      MID = N / 2 + 1
*
*     Gather last/first rows of appropriate eigenblocks into center of Z
*
      PTR = 1
*
*     Determine location of lowest level subproblem in the full storage
*     scheme
*
      CURR = PTR + CURPBM*2**CURLVL + 2**( CURLVL-1 ) - 1
*
*     Determine size of these matrices.  We add HALF to the value of
*     the SQRT in case the machine underestimates one of these square
*     roots.
*
      OPS = OPS + 8
      BSIZ1 = INT( HALF+SQRT( DBLE( QPTR( CURR+1 )-QPTR( CURR ) ) ) )
      BSIZ2 = INT( HALF+SQRT( DBLE( QPTR( CURR+2 )-QPTR( CURR+1 ) ) ) )
      DO 10 K = 1, MID - BSIZ1 - 1
         Z( K ) = ZERO
   10 CONTINUE
      CALL DCOPY( BSIZ1, Q( QPTR( CURR )+BSIZ1-1 ), BSIZ1,
     $            Z( MID-BSIZ1 ), 1 )
      CALL DCOPY( BSIZ2, Q( QPTR( CURR+1 ) ), BSIZ2, Z( MID ), 1 )
      DO 20 K = MID + BSIZ2, N
         Z( K ) = ZERO
   20 CONTINUE
*
*     Loop thru remaining levels 1 -> CURLVL applying the Givens
*     rotations and permutation and then multiplying the center matrices
*     against the current Z.
*
      PTR = 2**TLVLS + 1
      DO 70 K = 1, CURLVL - 1
         CURR = PTR + CURPBM*2**( CURLVL-K ) + 2**( CURLVL-K-1 ) - 1
         PSIZ1 = PRMPTR( CURR+1 ) - PRMPTR( CURR )
         PSIZ2 = PRMPTR( CURR+2 ) - PRMPTR( CURR+1 )
         ZPTR1 = MID - PSIZ1
*
*       Apply Givens at CURR and CURR+1
*
         OPS = OPS + 6*( GIVPTR( CURR+2 )-GIVPTR( CURR ) )
         DO 30 I = GIVPTR( CURR ), GIVPTR( CURR+1 ) - 1
            CALL DROT( 1, Z( ZPTR1+GIVCOL( 1, I )-1 ), 1,
     $                 Z( ZPTR1+GIVCOL( 2, I )-1 ), 1, GIVNUM( 1, I ),
     $                 GIVNUM( 2, I ) )
   30    CONTINUE
         DO 40 I = GIVPTR( CURR+1 ), GIVPTR( CURR+2 ) - 1
            CALL DROT( 1, Z( MID-1+GIVCOL( 1, I ) ), 1,
     $                 Z( MID-1+GIVCOL( 2, I ) ), 1, GIVNUM( 1, I ),
     $                 GIVNUM( 2, I ) )
   40    CONTINUE
         PSIZ1 = PRMPTR( CURR+1 ) - PRMPTR( CURR )
         PSIZ2 = PRMPTR( CURR+2 ) - PRMPTR( CURR+1 )
         DO 50 I = 0, PSIZ1 - 1
            ZTEMP( I+1 ) = Z( ZPTR1+PERM( PRMPTR( CURR )+I )-1 )
   50    CONTINUE
         DO 60 I = 0, PSIZ2 - 1
            ZTEMP( PSIZ1+I+1 ) = Z( MID+PERM( PRMPTR( CURR+1 )+I )-1 )
   60    CONTINUE
*
*        Multiply Blocks at CURR and CURR+1
*
*        Determine size of these matrices.  We add HALF to the value of
*        the SQRT in case the machine underestimates one of these
*        square roots.
*
         OPS = OPS + 8
         BSIZ1 = INT( HALF+SQRT( DBLE( QPTR( CURR+1 )-QPTR( CURR ) ) ) )
         BSIZ2 = INT( HALF+SQRT( DBLE( QPTR( CURR+2 )-QPTR( CURR+
     $           1 ) ) ) )
         IF( BSIZ1.GT.0 ) THEN
            OPS = OPS + 2*BSIZ1*BSIZ1
            CALL DGEMV( 'T', BSIZ1, BSIZ1, ONE, Q( QPTR( CURR ) ),
     $                  BSIZ1, ZTEMP( 1 ), 1, ZERO, Z( ZPTR1 ), 1 )
         END IF
         CALL DCOPY( PSIZ1-BSIZ1, ZTEMP( BSIZ1+1 ), 1, Z( ZPTR1+BSIZ1 ),
     $               1 )
         IF( BSIZ2.GT.0 ) THEN
            OPS = OPS + 2*BSIZ2*BSIZ2
            CALL DGEMV( 'T', BSIZ2, BSIZ2, ONE, Q( QPTR( CURR+1 ) ),
     $                  BSIZ2, ZTEMP( PSIZ1+1 ), 1, ZERO, Z( MID ), 1 )
         END IF
         CALL DCOPY( PSIZ2-BSIZ2, ZTEMP( PSIZ1+BSIZ2+1 ), 1,
     $               Z( MID+BSIZ2 ), 1 )
*
         PTR = PTR + 2**( TLVLS-K )
   70 CONTINUE
*
      RETURN
*
*     End of DLAEDA
*
      END
      SUBROUTINE DLAEIN( RIGHTV, NOINIT, N, H, LDH, WR, WI, VR, VI, B,
     $                   LDB, WORK, EPS3, SMLNUM, BIGNUM, INFO )
*
*  -- LAPACK auxiliary routine (instrumented to count operations) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     September 30, 1994
*
*     .. Scalar Arguments ..
      LOGICAL            NOINIT, RIGHTV
      INTEGER            INFO, LDB, LDH, N
      DOUBLE PRECISION   BIGNUM, EPS3, SMLNUM, WI, WR
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   B( LDB, * ), H( LDH, * ), VI( * ), VR( * ),
     $                   WORK( * )
*     ..
*     Common block to return operation count.
*     .. Common blocks ..
      COMMON             / LATIME / OPS, ITCNT
*     ..
*     .. Scalars in Common ..
      DOUBLE PRECISION   ITCNT, OPS
*     ..
*
*  Purpose
*  =======
*
*  DLAEIN uses inverse iteration to find a right or left eigenvector
*  corresponding to the eigenvalue (WR,WI) of a real upper Hessenberg
*  matrix H.
*
*  Arguments
*  =========
*
*  RIGHTV   (input) LOGICAL
*          = .TRUE. : compute right eigenvector;
*          = .FALSE.: compute left eigenvector.
*
*  NOINIT   (input) LOGICAL
*          = .TRUE. : no initial vector supplied in (VR,VI).
*          = .FALSE.: initial vector supplied in (VR,VI).
*
*  N       (input) INTEGER
*          The order of the matrix H.  N >= 0.
*
*  H       (input) DOUBLE PRECISION array, dimension (LDH,N)
*          The upper Hessenberg matrix H.
*
*  LDH     (input) INTEGER
*          The leading dimension of the array H.  LDH >= max(1,N).
*
*  WR      (input) DOUBLE PRECISION
*  WI      (input) DOUBLE PRECISION
*          The real and imaginary parts of the eigenvalue of H whose
*          corresponding right or left eigenvector is to be computed.
*
*  VR      (input/output) DOUBLE PRECISION array, dimension (N)
*  VI      (input/output) DOUBLE PRECISION array, dimension (N)
*          On entry, if NOINIT = .FALSE. and WI = 0.0, VR must contain
*          a real starting vector for inverse iteration using the real
*          eigenvalue WR; if NOINIT = .FALSE. and WI.ne.0.0, VR and VI
*          must contain the real and imaginary parts of a complex
*          starting vector for inverse iteration using the complex
*          eigenvalue (WR,WI); otherwise VR and VI need not be set.
*          On exit, if WI = 0.0 (real eigenvalue), VR contains the
*          computed real eigenvector; if WI.ne.0.0 (complex eigenvalue),
*          VR and VI contain the real and imaginary parts of the
*          computed complex eigenvector. The eigenvector is normalized
*          so that the component of largest magnitude has magnitude 1;
*          here the magnitude of a complex number (x,y) is taken to be
*          |x| + |y|.
*          VI is not referenced if WI = 0.0.
*
*  B       (workspace) DOUBLE PRECISION array, dimension (LDB,N)
*
*  LDB     (input) INTEGER
*          The leading dimension of the array B.  LDB >= N+1.
*
*  WORK   (workspace) DOUBLE PRECISION array, dimension (N)
*
*  EPS3    (input) DOUBLE PRECISION
*          A small machine-dependent value which is used to perturb
*          close eigenvalues, and to replace zero pivots.
*
*  SMLNUM  (input) DOUBLE PRECISION
*          A machine-dependent value close to the underflow threshold.
*
*  BIGNUM  (input) DOUBLE PRECISION
*          A machine-dependent value close to the overflow threshold.
*
*  INFO    (output) INTEGER
*          = 0:  successful exit
*          = 1:  inverse iteration did not converge; VR is set to the
*                last iterate, and so is VI if WI.ne.0.0.
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE, TENTH
      PARAMETER          ( ZERO = 0.0D+0, ONE = 1.0D+0, TENTH = 1.0D-1 )
*     ..
*     .. Local Scalars ..
      CHARACTER          NORMIN, TRANS
      INTEGER            I, I1, I2, I3, IERR, ITS, J
      DOUBLE PRECISION   ABSBII, ABSBJJ, EI, EJ, GROWTO, NORM, NRMSML,
     $                   OPST, REC, ROOTN, SCALE, TEMP, VCRIT, VMAX,
     $                   VNORM, W, W1, X, XI, XR, Y
*     ..
*     .. External Functions ..
      INTEGER            IDAMAX
      DOUBLE PRECISION   DASUM, DLAPY2, DNRM2
      EXTERNAL           IDAMAX, DASUM, DLAPY2, DNRM2
*     ..
*     .. External Subroutines ..
      EXTERNAL           DLADIV, DLATRS, DSCAL
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, DBLE, MAX, SQRT
*     ..
*     .. Executable Statements ..
*
      INFO = 0
***
*     Initialize
      OPST = 0
***
*
*     GROWTO is the threshold used in the acceptance test for an
*     eigenvector.
*
      ROOTN = SQRT( DBLE( N ) )
      GROWTO = TENTH / ROOTN
      NRMSML = MAX( ONE, EPS3*ROOTN )*SMLNUM
***
*        Increment op count for computing ROOTN, GROWTO and NRMSML
      OPST = OPST + 4
***
*
*     Form B = H - (WR,WI)*I (except that the subdiagonal elements and
*     the imaginary parts of the diagonal elements are not stored).
*
      DO 20 J = 1, N
         DO 10 I = 1, J - 1
            B( I, J ) = H( I, J )
   10    CONTINUE
         B( J, J ) = H( J, J ) - WR
   20 CONTINUE
***
      OPST = OPST + N
***
*
      IF( WI.EQ.ZERO ) THEN
*
*        Real eigenvalue.
*
         IF( NOINIT ) THEN
*
*           Set initial vector.
*
            DO 30 I = 1, N
               VR( I ) = EPS3
   30       CONTINUE
         ELSE
*
*           Scale supplied initial vector.
*
            VNORM = DNRM2( N, VR, 1 )
            CALL DSCAL( N, ( EPS3*ROOTN ) / MAX( VNORM, NRMSML ), VR,
     $                  1 )
***
            OPST = OPST + ( 3*N+2 )
***
         END IF
*
         IF( RIGHTV ) THEN
*
*           LU decomposition with partial pivoting of B, replacing zero
*           pivots by EPS3.
*
            DO 60 I = 1, N - 1
               EI = H( I+1, I )
               IF( ABS( B( I, I ) ).LT.ABS( EI ) ) THEN
*
*                 Interchange rows and eliminate.
*
                  X = B( I, I ) / EI
                  B( I, I ) = EI
                  DO 40 J = I + 1, N
                     TEMP = B( I+1, J )
                     B( I+1, J ) = B( I, J ) - X*TEMP
                     B( I, J ) = TEMP
   40             CONTINUE
               ELSE
*
*                 Eliminate without interchange.
*
                  IF( B( I, I ).EQ.ZERO )
     $               B( I, I ) = EPS3
                  X = EI / B( I, I )
                  IF( X.NE.ZERO ) THEN
                     DO 50 J = I + 1, N
                        B( I+1, J ) = B( I+1, J ) - X*B( I, J )
   50                CONTINUE
                  END IF
               END IF
   60       CONTINUE
            IF( B( N, N ).EQ.ZERO )
     $         B( N, N ) = EPS3
***
*           Increment op count for LU decomposition
            OPS = OPS + ( N-1 )*( N+1 )
***
*
            TRANS = 'N'
*
         ELSE
*
*           UL decomposition with partial pivoting of B, replacing zero
*           pivots by EPS3.
*
            DO 90 J = N, 2, -1
               EJ = H( J, J-1 )
               IF( ABS( B( J, J ) ).LT.ABS( EJ ) ) THEN
*
*                 Interchange columns and eliminate.
*
                  X = B( J, J ) / EJ
                  B( J, J ) = EJ
                  DO 70 I = 1, J - 1
                     TEMP = B( I, J-1 )
                     B( I, J-1 ) = B( I, J ) - X*TEMP
                     B( I, J ) = TEMP
   70             CONTINUE
               ELSE
*
*                 Eliminate without interchange.
*
                  IF( B( J, J ).EQ.ZERO )
     $               B( J, J ) = EPS3
                  X = EJ / B( J, J )
                  IF( X.NE.ZERO ) THEN
                     DO 80 I = 1, J - 1
                        B( I, J-1 ) = B( I, J-1 ) - X*B( I, J )
   80                CONTINUE
                  END IF
               END IF
   90       CONTINUE
            IF( B( 1, 1 ).EQ.ZERO )
     $         B( 1, 1 ) = EPS3
***
*           Increment op count for UL decomposition
            OPS = OPS + ( N-1 )*( N+1 )
***
*
            TRANS = 'T'
*
         END IF
*
         NORMIN = 'N'
         DO 110 ITS = 1, N
*
*           Solve U*x = scale*v for a right eigenvector
*             or U'*x = scale*v for a left eigenvector,
*           overwriting x on v.
*
            CALL DLATRS( 'Upper', TRANS, 'Nonunit', NORMIN, N, B, LDB,
     $                   VR, SCALE, WORK, IERR )
***
*           Increment opcount for triangular solver, assuming that
*           ops DLATRS = ops DTRSV, with no scaling in DLATRS.
            OPS = OPS + N*N
***
            NORMIN = 'Y'
*
*           Test for sufficient growth in the norm of v.
*
            VNORM = DASUM( N, VR, 1 )
***
            OPST = OPST + N
***
            IF( VNORM.GE.GROWTO*SCALE )
     $         GO TO 120
*
*           Choose new orthogonal starting vector and try again.
*
            TEMP = EPS3 / ( ROOTN+ONE )
            VR( 1 ) = EPS3
            DO 100 I = 2, N
               VR( I ) = TEMP
  100       CONTINUE
            VR( N-ITS+1 ) = VR( N-ITS+1 ) - EPS3*ROOTN
***
            OPST = OPST + 4
***
  110    CONTINUE
*
*        Failure to find eigenvector in N iterations.
*
         INFO = 1
*
  120    CONTINUE
*
*        Normalize eigenvector.
*
         I = IDAMAX( N, VR, 1 )
         CALL DSCAL( N, ONE / ABS( VR( I ) ), VR, 1 )
***
         OPST = OPST + ( 2*N+1 )
***
      ELSE
*
*        Complex eigenvalue.
*
         IF( NOINIT ) THEN
*
*           Set initial vector.
*
            DO 130 I = 1, N
               VR( I ) = EPS3
               VI( I ) = ZERO
  130       CONTINUE
         ELSE
*
*           Scale supplied initial vector.
*
            NORM = DLAPY2( DNRM2( N, VR, 1 ), DNRM2( N, VI, 1 ) )
            REC = ( EPS3*ROOTN ) / MAX( NORM, NRMSML )
            CALL DSCAL( N, REC, VR, 1 )
            CALL DSCAL( N, REC, VI, 1 )
***
            OPST = OPST + ( 6*N+5 )
***
         END IF
*
         IF( RIGHTV ) THEN
*
*           LU decomposition with partial pivoting of B, replacing zero
*           pivots by EPS3.
*
*           The imaginary part of the (i,j)-th element of U is stored in
*           B(j+1,i).
*
            B( 2, 1 ) = -WI
            DO 140 I = 2, N
               B( I+1, 1 ) = ZERO
  140       CONTINUE
*
            DO 170 I = 1, N - 1
               ABSBII = DLAPY2( B( I, I ), B( I+1, I ) )
               EI = H( I+1, I )
               IF( ABSBII.LT.ABS( EI ) ) THEN
*
*                 Interchange rows and eliminate.
*
                  XR = B( I, I ) / EI
                  XI = B( I+1, I ) / EI
                  B( I, I ) = EI
                  B( I+1, I ) = ZERO
                  DO 150 J = I + 1, N
                     TEMP = B( I+1, J )
                     B( I+1, J ) = B( I, J ) - XR*TEMP
                     B( J+1, I+1 ) = B( J+1, I ) - XI*TEMP
                     B( I, J ) = TEMP
                     B( J+1, I ) = ZERO
  150             CONTINUE
                  B( I+2, I ) = -WI
                  B( I+1, I+1 ) = B( I+1, I+1 ) - XI*WI
                  B( I+2, I+1 ) = B( I+2, I+1 ) + XR*WI
***
                  OPST = OPST + ( 4*( N-I )+6 )
***
               ELSE
*
*                 Eliminate without interchanging rows.
*
                  IF( ABSBII.EQ.ZERO ) THEN
                     B( I, I ) = EPS3
                     B( I+1, I ) = ZERO
                     ABSBII = EPS3
                  END IF
                  EI = ( EI / ABSBII ) / ABSBII
                  XR = B( I, I )*EI
                  XI = -B( I+1, I )*EI
                  DO 160 J = I + 1, N
                     B( I+1, J ) = B( I+1, J ) - XR*B( I, J ) +
     $                             XI*B( J+1, I )
                     B( J+1, I+1 ) = -XR*B( J+1, I ) - XI*B( I, J )
  160             CONTINUE
                  B( I+2, I+1 ) = B( I+2, I+1 ) - WI
***
                  OPST = OPST + ( 7*( N-I )+4 )
***
               END IF
*
*              Compute 1-norm of offdiagonal elements of i-th row.
*
               WORK( I ) = DASUM( N-I, B( I, I+1 ), LDB ) +
     $                     DASUM( N-I, B( I+2, I ), 1 )
***
               OPST = OPST + ( 2*( N-I )+4 )
***
  170       CONTINUE
            IF( B( N, N ).EQ.ZERO .AND. B( N+1, N ).EQ.ZERO )
     $         B( N, N ) = EPS3
            WORK( N ) = ZERO
*
            I1 = N
            I2 = 1
            I3 = -1
         ELSE
*
*           UL decomposition with partial pivoting of conjg(B),
*           replacing zero pivots by EPS3.
*
*           The imaginary part of the (i,j)-th element of U is stored in
*           B(j+1,i).
*
            B( N+1, N ) = WI
            DO 180 J = 1, N - 1
               B( N+1, J ) = ZERO
  180       CONTINUE
*
            DO 210 J = N, 2, -1
               EJ = H( J, J-1 )
               ABSBJJ = DLAPY2( B( J, J ), B( J+1, J ) )
               IF( ABSBJJ.LT.ABS( EJ ) ) THEN
*
*                 Interchange columns and eliminate
*
                  XR = B( J, J ) / EJ
                  XI = B( J+1, J ) / EJ
                  B( J, J ) = EJ
                  B( J+1, J ) = ZERO
                  DO 190 I = 1, J - 1
                     TEMP = B( I, J-1 )
                     B( I, J-1 ) = B( I, J ) - XR*TEMP
                     B( J, I ) = B( J+1, I ) - XI*TEMP
                     B( I, J ) = TEMP
                     B( J+1, I ) = ZERO
  190             CONTINUE
                  B( J+1, J-1 ) = WI
                  B( J-1, J-1 ) = B( J-1, J-1 ) + XI*WI
                  B( J, J-1 ) = B( J, J-1 ) - XR*WI
***
                  OPST = OPST + ( 4*( J-1 )+6 )
***
               ELSE
*
*                 Eliminate without interchange.
*
                  IF( ABSBJJ.EQ.ZERO ) THEN
                     B( J, J ) = EPS3
                     B( J+1, J ) = ZERO
                     ABSBJJ = EPS3
                  END IF
                  EJ = ( EJ / ABSBJJ ) / ABSBJJ
                  XR = B( J, J )*EJ
                  XI = -B( J+1, J )*EJ
                  DO 200 I = 1, J - 1
                     B( I, J-1 ) = B( I, J-1 ) - XR*B( I, J ) +
     $                             XI*B( J+1, I )
                     B( J, I ) = -XR*B( J+1, I ) - XI*B( I, J )
  200             CONTINUE
                  B( J, J-1 ) = B( J, J-1 ) + WI
***
                  OPST = OPST + ( 7*( J-1 )+4 )
***
               END IF
*
*              Compute 1-norm of offdiagonal elements of j-th column.
*
               WORK( J ) = DASUM( J-1, B( 1, J ), 1 ) +
     $                     DASUM( J-1, B( J+1, 1 ), LDB )
***
               OPST = OPST + ( 2*( J-1 )+4 )
***
  210       CONTINUE
            IF( B( 1, 1 ).EQ.ZERO .AND. B( 2, 1 ).EQ.ZERO )
     $         B( 1, 1 ) = EPS3
            WORK( 1 ) = ZERO
*
            I1 = 1
            I2 = N
            I3 = 1
         END IF
*
         DO 270 ITS = 1, N
            SCALE = ONE
            VMAX = ONE
            VCRIT = BIGNUM
*
*           Solve U*(xr,xi) = scale*(vr,vi) for a right eigenvector,
*             or U'*(xr,xi) = scale*(vr,vi) for a left eigenvector,
*           overwriting (xr,xi) on (vr,vi).
*
            DO 250 I = I1, I2, I3
*
               IF( WORK( I ).GT.VCRIT ) THEN
                  REC = ONE / VMAX
                  CALL DSCAL( N, REC, VR, 1 )
                  CALL DSCAL( N, REC, VI, 1 )
                  SCALE = SCALE*REC
                  VMAX = ONE
                  VCRIT = BIGNUM
               END IF
*
               XR = VR( I )
               XI = VI( I )
               IF( RIGHTV ) THEN
                  DO 220 J = I + 1, N
                     XR = XR - B( I, J )*VR( J ) + B( J+1, I )*VI( J )
                     XI = XI - B( I, J )*VI( J ) - B( J+1, I )*VR( J )
  220             CONTINUE
               ELSE
                  DO 230 J = 1, I - 1
                     XR = XR - B( J, I )*VR( J ) + B( I+1, J )*VI( J )
                     XI = XI - B( J, I )*VI( J ) - B( I+1, J )*VR( J )
  230             CONTINUE
               END IF
*
               W = ABS( B( I, I ) ) + ABS( B( I+1, I ) )
               IF( W.GT.SMLNUM ) THEN
                  IF( W.LT.ONE ) THEN
                     W1 = ABS( XR ) + ABS( XI )
                     IF( W1.GT.W*BIGNUM ) THEN
                        REC = ONE / W1
                        CALL DSCAL( N, REC, VR, 1 )
                        CALL DSCAL( N, REC, VI, 1 )
                        XR = VR( I )
                        XI = VI( I )
                        SCALE = SCALE*REC
                        VMAX = VMAX*REC
                     END IF
                  END IF
*
*                 Divide by diagonal element of B.
*
                  CALL DLADIV( XR, XI, B( I, I ), B( I+1, I ), VR( I ),
     $                         VI( I ) )
                  VMAX = MAX( ABS( VR( I ) )+ABS( VI( I ) ), VMAX )
                  VCRIT = BIGNUM / VMAX
***
                  OPST = OPST + 9
***
               ELSE
                  DO 240 J = 1, N
                     VR( J ) = ZERO
                     VI( J ) = ZERO
  240             CONTINUE
                  VR( I ) = ONE
                  VI( I ) = ONE
                  SCALE = ZERO
                  VMAX = ONE
                  VCRIT = BIGNUM
               END IF
  250       CONTINUE
***
*           Increment op count for loop 260, assuming no scaling
            OPS = OPS + 4*N*( N-1 )
***
*
*           Test for sufficient growth in the norm of (VR,VI).
*
            VNORM = DASUM( N, VR, 1 ) + DASUM( N, VI, 1 )
***
            OPST = OPST + 2*N
***
            IF( VNORM.GE.GROWTO*SCALE )
     $         GO TO 280
*
*           Choose a new orthogonal starting vector and try again.
*
            Y = EPS3 / ( ROOTN+ONE )
            VR( 1 ) = EPS3
            VI( 1 ) = ZERO
*
            DO 260 I = 2, N
               VR( I ) = Y
               VI( I ) = ZERO
  260       CONTINUE
            VR( N-ITS+1 ) = VR( N-ITS+1 ) - EPS3*ROOTN
***
            OPST = OPST + 4
***
  270    CONTINUE
*
*        Failure to find eigenvector in N iterations
*
         INFO = 1
*
  280    CONTINUE
*
*        Normalize eigenvector.
*
         VNORM = ZERO
         DO 290 I = 1, N
            VNORM = MAX( VNORM, ABS( VR( I ) )+ABS( VI( I ) ) )
  290    CONTINUE
         CALL DSCAL( N, ONE / VNORM, VR, 1 )
         CALL DSCAL( N, ONE / VNORM, VI, 1 )
***
         OPST = OPST + ( 4*N+1 )
***
*
      END IF
*
***
*     Compute final op count
      OPS = OPS + OPST
***
      RETURN
*
*     End of DLAEIN
*
      END
      SUBROUTINE DLAHQR( WANTT, WANTZ, N, ILO, IHI, H, LDH, WR, WI,
     $                   ILOZ, IHIZ, Z, LDZ, INFO )
*
*  -- LAPACK auxiliary routine (instrum. to count ops. version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     June 30, 1999
*
*     .. Scalar Arguments ..
      LOGICAL            WANTT, WANTZ
      INTEGER            IHI, IHIZ, ILO, ILOZ, INFO, LDH, LDZ, N
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   H( LDH, * ), WI( * ), WR( * ), Z( LDZ, * )
*     ..
*     Common block to return operation count.
*     .. Common blocks ..
      COMMON             / LATIME / OPS, ITCNT
*     ..
*     .. Scalars in Common ..
      DOUBLE PRECISION   ITCNT, OPS
*     ..
*
*  Purpose
*  =======
*
*  DLAHQR is an auxiliary routine called by DHSEQR to update the
*  eigenvalues and Schur decomposition already computed by DHSEQR, by
*  dealing with the Hessenberg submatrix in rows and columns ILO to IHI.
*
*  Arguments
*  =========
*
*  WANTT   (input) LOGICAL
*          = .TRUE. : the full Schur form T is required;
*          = .FALSE.: only eigenvalues are required.
*
*  WANTZ   (input) LOGICAL
*          = .TRUE. : the matrix of Schur vectors Z is required;
*          = .FALSE.: Schur vectors are not required.
*
*  N       (input) INTEGER
*          The order of the matrix H.  N >= 0.
*
*  ILO     (input) INTEGER
*  IHI     (input) INTEGER
*          It is assumed that H is already upper quasi-triangular in
*          rows and columns IHI+1:N, and that H(ILO,ILO-1) = 0 (unless
*          ILO = 1). DLAHQR works primarily with the Hessenberg
*          submatrix in rows and columns ILO to IHI, but applies
*          transformations to all of H if WANTT is .TRUE..
*          1 <= ILO <= max(1,IHI); IHI <= N.
*
*  H       (input/output) DOUBLE PRECISION array, dimension (LDH,N)
*          On entry, the upper Hessenberg matrix H.
*          On exit, if WANTT is .TRUE., H is upper quasi-triangular in
*          rows and columns ILO:IHI, with any 2-by-2 diagonal blocks in
*          standard form. If WANTT is .FALSE., the contents of H are
*          unspecified on exit.
*
*  LDH     (input) INTEGER
*          The leading dimension of the array H. LDH >= max(1,N).
*
*  WR      (output) DOUBLE PRECISION array, dimension (N)
*  WI      (output) DOUBLE PRECISION array, dimension (N)
*          The real and imaginary parts, respectively, of the computed
*          eigenvalues ILO to IHI are stored in the corresponding
*          elements of WR and WI. If two eigenvalues are computed as a
*          complex conjugate pair, they are stored in consecutive
*          elements of WR and WI, say the i-th and (i+1)th, with
*          WI(i) > 0 and WI(i+1) < 0. If WANTT is .TRUE., the
*          eigenvalues are stored in the same order as on the diagonal
*          of the Schur form returned in H, with WR(i) = H(i,i), and, if
*          H(i:i+1,i:i+1) is a 2-by-2 diagonal block,
*          WI(i) = sqrt(H(i+1,i)*H(i,i+1)) and WI(i+1) = -WI(i).
*
*  ILOZ    (input) INTEGER
*  IHIZ    (input) INTEGER
*          Specify the rows of Z to which transformations must be
*          applied if WANTZ is .TRUE..
*          1 <= ILOZ <= ILO; IHI <= IHIZ <= N.
*
*  Z       (input/output) DOUBLE PRECISION array, dimension (LDZ,N)
*          If WANTZ is .TRUE., on entry Z must contain the current
*          matrix Z of transformations accumulated by DHSEQR, and on
*          exit Z has been updated; transformations are applied only to
*          the submatrix Z(ILOZ:IHIZ,ILO:IHI).
*          If WANTZ is .FALSE., Z is not referenced.
*
*  LDZ     (input) INTEGER
*          The leading dimension of the array Z. LDZ >= max(1,N).
*
*  INFO    (output) INTEGER
*          = 0: successful exit
*          > 0: DLAHQR failed to compute all the eigenvalues ILO to IHI
*               in a total of 30*(IHI-ILO+1) iterations; if INFO = i,
*               elements i+1:ihi of WR and WI contain those eigenvalues
*               which have been successfully computed.
*
*  Further Details
*  ===============
*
*  2-96 Based on modifications by
*     David Day, Sandia National Laboratory, USA
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE, HALF
      PARAMETER          ( ZERO = 0.0D+0, ONE = 1.0D+0, HALF = 0.5D0 )
      DOUBLE PRECISION   DAT1, DAT2
      PARAMETER          ( DAT1 = 0.75D+0, DAT2 = -0.4375D+0 )
*     ..
*     .. Local Scalars ..
      INTEGER            I, I1, I2, ITN, ITS, J, K, L, M, NH, NR, NZ
      DOUBLE PRECISION   AVE, CS, DISC, H00, H10, H11, H12, H21, H22,
     $                   H33, H33S, H43H34, H44, H44S, OPST, OVFL, S,
     $                   SMLNUM, SN, SUM, T1, T2, T3, TST1, ULP, UNFL,
     $                   V1, V2, V3
*     ..
*     .. Local Arrays ..
      DOUBLE PRECISION   V( 3 ), WORK( 1 )
*     ..
*     .. External Functions ..
      DOUBLE PRECISION   DLAMCH, DLANHS
      EXTERNAL           DLAMCH, DLANHS
*     ..
*     .. External Subroutines ..
      EXTERNAL           DCOPY, DLABAD, DLANV2, DLARFG, DROT
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, MAX, MIN, SIGN, SQRT
*     ..
*     .. Executable Statements ..
*
      INFO = 0
***
*     Initialize
      OPST = 0
***
*
*     Quick return if possible
*
      IF( N.EQ.0 )
     $   RETURN
      IF( ILO.EQ.IHI ) THEN
         WR( ILO ) = H( ILO, ILO )
         WI( ILO ) = ZERO
         RETURN
      END IF
*
      NH = IHI - ILO + 1
      NZ = IHIZ - ILOZ + 1
*
*     Set machine-dependent constants for the stopping criterion.
*     If norm(H) <= sqrt(OVFL), overflow should not occur.
*
      UNFL = DLAMCH( 'Safe minimum' )
      OVFL = ONE / UNFL
      CALL DLABAD( UNFL, OVFL )
      ULP = DLAMCH( 'Precision' )
      SMLNUM = UNFL*( NH / ULP )
*
*     I1 and I2 are the indices of the first row and last column of H
*     to which transformations must be applied. If eigenvalues only are
*     being computed, I1 and I2 are set inside the main loop.
*
      IF( WANTT ) THEN
         I1 = 1
         I2 = N
      END IF
*
*     ITN is the total number of QR iterations allowed.
*
      ITN = 30*NH
*
*     The main loop begins here. I is the loop index and decreases from
*     IHI to ILO in steps of 1 or 2. Each iteration of the loop works
*     with the active submatrix in rows and columns L to I.
*     Eigenvalues I+1 to IHI have already converged. Either L = ILO or
*     H(L,L-1) is negligible so that the matrix splits.
*
      I = IHI
   10 CONTINUE
      L = ILO
      IF( I.LT.ILO )
     $   GO TO 150
*
*     Perform QR iterations on rows and columns ILO to I until a
*     submatrix of order 1 or 2 splits off at the bottom because a
*     subdiagonal element has become negligible.
*
      DO 130 ITS = 0, ITN
*
*        Look for a single small subdiagonal element.
*
         DO 20 K = I, L + 1, -1
            TST1 = ABS( H( K-1, K-1 ) ) + ABS( H( K, K ) )
            IF( TST1.EQ.ZERO ) THEN
               TST1 = DLANHS( '1', I-L+1, H( L, L ), LDH, WORK )
***
*              Increment op count
               OPS = OPS + ( I-L+1 )*( I-L+2 ) / 2
***
            END IF
            IF( ABS( H( K, K-1 ) ).LE.MAX( ULP*TST1, SMLNUM ) )
     $         GO TO 30
   20    CONTINUE
   30    CONTINUE
         L = K
***
*        Increment op count
         OPST = OPST + 3*( I-L+1 )
***
         IF( L.GT.ILO ) THEN
*
*           H(L,L-1) is negligible
*
            H( L, L-1 ) = ZERO
         END IF
*
*        Exit from loop if a submatrix of order 1 or 2 has split off.
*
         IF( L.GE.I-1 )
     $      GO TO 140
*
*        Now the active submatrix is in rows and columns L to I. If
*        eigenvalues only are being computed, only the active submatrix
*        need be transformed.
*
         IF( .NOT.WANTT ) THEN
            I1 = L
            I2 = I
         END IF
*
         IF( ITS.EQ.10 .OR. ITS.EQ.20 ) THEN
*
*           Exceptional shift.
*
            S = ABS( H( I, I-1 ) ) + ABS( H( I-1, I-2 ) )
            H44 = DAT1*S + H( I, I )
            H33 = H44
            H43H34 = DAT2*S*S
***
*           Increment op count
            OPST = OPST + 5
***
         ELSE
*
*           Prepare to use Francis' double shift
*           (i.e. 2nd degree generalized Rayleigh quotient)
*
            H44 = H( I, I )
            H33 = H( I-1, I-1 )
            H43H34 = H( I, I-1 )*H( I-1, I )
            S = H( I-1, I-2 )*H( I-1, I-2 )
            DISC = ( H33-H44 )*HALF
            DISC = DISC*DISC + H43H34
***
*           Increment op count
            OPST = OPST + 6
***
            IF( DISC.GT.ZERO ) THEN
*
*              Real roots: use Wilkinson's shift twice
*
               DISC = SQRT( DISC )
               AVE = HALF*( H33+H44 )
***
*              Increment op count
               OPST = OPST + 2
***
               IF( ABS( H33 )-ABS( H44 ).GT.ZERO ) THEN
                  H33 = H33*H44 - H43H34
                  H44 = H33 / ( SIGN( DISC, AVE )+AVE )
***
*                 Increment op count
                  OPST = OPST + 4
***
               ELSE
                  H44 = SIGN( DISC, AVE ) + AVE
***
*                 Increment op count
                  OPST = OPST + 1
***
               END IF
               H33 = H44
               H43H34 = ZERO
            END IF
         END IF
*
*        Look for two consecutive small subdiagonal elements.
*
         DO 40 M = I - 2, L, -1
*
*           Determine the effect of starting the double-shift QR
*           iteration at row M, and see if this would make H(M,M-1)
*           negligible.
*
            H11 = H( M, M )
            H22 = H( M+1, M+1 )
            H21 = H( M+1, M )
            H12 = H( M, M+1 )
            H44S = H44 - H11
            H33S = H33 - H11
            V1 = ( H33S*H44S-H43H34 ) / H21 + H12
            V2 = H22 - H11 - H33S - H44S
            V3 = H( M+2, M+1 )
            S = ABS( V1 ) + ABS( V2 ) + ABS( V3 )
            V1 = V1 / S
            V2 = V2 / S
            V3 = V3 / S
            V( 1 ) = V1
            V( 2 ) = V2
            V( 3 ) = V3
            IF( M.EQ.L )
     $         GO TO 50
            H00 = H( M-1, M-1 )
            H10 = H( M, M-1 )
            TST1 = ABS( V1 )*( ABS( H00 )+ABS( H11 )+ABS( H22 ) )
            IF( ABS( H10 )*( ABS( V2 )+ABS( V3 ) ).LE.ULP*TST1 )
     $         GO TO 50
   40    CONTINUE
   50    CONTINUE
***
*        Increment op count
         OPST = OPST + 20*( I-M-1 )
***
*
*        Double-shift QR step
*
         DO 120 K = M, I - 1
*
*           The first iteration of this loop determines a reflection G
*           from the vector V and applies it from left and right to H,
*           thus creating a nonzero bulge below the subdiagonal.
*
*           Each subsequent iteration determines a reflection G to
*           restore the Hessenberg form in the (K-1)th column, and thus
*           chases the bulge one step toward the bottom of the active
*           submatrix. NR is the order of G.
*
            NR = MIN( 3, I-K+1 )
            IF( K.GT.M )
     $         CALL DCOPY( NR, H( K, K-1 ), 1, V, 1 )
            CALL DLARFG( NR, V( 1 ), V( 2 ), 1, T1 )
***
*           Increment op count
            OPST = OPST + 3*NR + 9
***
            IF( K.GT.M ) THEN
               H( K, K-1 ) = V( 1 )
               H( K+1, K-1 ) = ZERO
               IF( K.LT.I-1 )
     $            H( K+2, K-1 ) = ZERO
            ELSE IF( M.GT.L ) THEN
               H( K, K-1 ) = -H( K, K-1 )
            END IF
            V2 = V( 2 )
            T2 = T1*V2
            IF( NR.EQ.3 ) THEN
               V3 = V( 3 )
               T3 = T1*V3
*
*              Apply G from the left to transform the rows of the matrix
*              in columns K to I2.
*
               DO 60 J = K, I2
                  SUM = H( K, J ) + V2*H( K+1, J ) + V3*H( K+2, J )
                  H( K, J ) = H( K, J ) - SUM*T1
                  H( K+1, J ) = H( K+1, J ) - SUM*T2
                  H( K+2, J ) = H( K+2, J ) - SUM*T3
   60          CONTINUE
*
*              Apply G from the right to transform the columns of the
*              matrix in rows I1 to min(K+3,I).
*
               DO 70 J = I1, MIN( K+3, I )
                  SUM = H( J, K ) + V2*H( J, K+1 ) + V3*H( J, K+2 )
                  H( J, K ) = H( J, K ) - SUM*T1
                  H( J, K+1 ) = H( J, K+1 ) - SUM*T2
                  H( J, K+2 ) = H( J, K+2 ) - SUM*T3
   70          CONTINUE
***
*              Increment op count
               OPS = OPS + 10*( I2-I1+2+MIN( 3, I-K ) )
***
*
               IF( WANTZ ) THEN
*
*                 Accumulate transformations in the matrix Z
*
                  DO 80 J = ILOZ, IHIZ
                     SUM = Z( J, K ) + V2*Z( J, K+1 ) + V3*Z( J, K+2 )
                     Z( J, K ) = Z( J, K ) - SUM*T1
                     Z( J, K+1 ) = Z( J, K+1 ) - SUM*T2
                     Z( J, K+2 ) = Z( J, K+2 ) - SUM*T3
   80             CONTINUE
***
*                 Increment op count
                  OPS = OPS + 10*NZ
***
               END IF
            ELSE IF( NR.EQ.2 ) THEN
*
*              Apply G from the left to transform the rows of the matrix
*              in columns K to I2.
*
               DO 90 J = K, I2
                  SUM = H( K, J ) + V2*H( K+1, J )
                  H( K, J ) = H( K, J ) - SUM*T1
                  H( K+1, J ) = H( K+1, J ) - SUM*T2
   90          CONTINUE
*
*              Apply G from the right to transform the columns of the
*              matrix in rows I1 to min(K+3,I).
*
               DO 100 J = I1, I
                  SUM = H( J, K ) + V2*H( J, K+1 )
                  H( J, K ) = H( J, K ) - SUM*T1
                  H( J, K+1 ) = H( J, K+1 ) - SUM*T2
  100          CONTINUE
***
*              Increment op count
               OPS = OPS + 6*( I2-I1+3 )
***
*
               IF( WANTZ ) THEN
*
*                 Accumulate transformations in the matrix Z
*
                  DO 110 J = ILOZ, IHIZ
                     SUM = Z( J, K ) + V2*Z( J, K+1 )
                     Z( J, K ) = Z( J, K ) - SUM*T1
                     Z( J, K+1 ) = Z( J, K+1 ) - SUM*T2
  110             CONTINUE
***
*                 Increment op count
                  OPS = OPS + 6*NZ
***
               END IF
            END IF
  120    CONTINUE
*
  130 CONTINUE
*
*     Failure to converge in remaining number of iterations
*
      INFO = I
      RETURN
*
  140 CONTINUE
*
      IF( L.EQ.I ) THEN
*
*        H(I,I-1) is negligible: one eigenvalue has converged.
*
         WR( I ) = H( I, I )
         WI( I ) = ZERO
      ELSE IF( L.EQ.I-1 ) THEN
*
*        H(I-1,I-2) is negligible: a pair of eigenvalues have converged.
*
*        Transform the 2-by-2 submatrix to standard Schur form,
*        and compute and store the eigenvalues.
*
         CALL DLANV2( H( I-1, I-1 ), H( I-1, I ), H( I, I-1 ),
     $                H( I, I ), WR( I-1 ), WI( I-1 ), WR( I ), WI( I ),
     $                CS, SN )
*
         IF( WANTT ) THEN
*
*           Apply the transformation to the rest of H.
*
            IF( I2.GT.I )
     $         CALL DROT( I2-I, H( I-1, I+1 ), LDH, H( I, I+1 ), LDH,
     $                    CS, SN )
            CALL DROT( I-I1-1, H( I1, I-1 ), 1, H( I1, I ), 1, CS, SN )
***
*           Increment op count
            OPS = OPS + 6*( I2-I1-1 )
***
         END IF
         IF( WANTZ ) THEN
*
*           Apply the transformation to Z.
*
            CALL DROT( NZ, Z( ILOZ, I-1 ), 1, Z( ILOZ, I ), 1, CS, SN )
***
*           Increment op count
            OPS = OPS + 6*NZ
***
         END IF
      END IF
*
*     Decrement number of remaining iterations, and return to start of
*     the main loop with new value of I.
*
      ITN = ITN - ITS
      I = L - 1
      GO TO 10
*
  150 CONTINUE
***
*     Compute final op count
      OPS = OPS + OPST
***
      RETURN
*
*     End of DLAHQR
*
      END
      SUBROUTINE DLAR1V( N, B1, BN, SIGMA, D, L, LD, LLD, GERSCH, Z,
     $                   ZTZ, MINGMA, R, ISUPPZ, WORK )
*
*  -- LAPACK auxiliary routine (instru to count ops, version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     June 30, 1999
*
*     .. Scalar Arguments ..
      INTEGER            B1, BN, N, R
      DOUBLE PRECISION   MINGMA, SIGMA, ZTZ
*     ..
*     .. Array Arguments ..
      INTEGER            ISUPPZ( * )
      DOUBLE PRECISION   D( * ), GERSCH( * ), L( * ), LD( * ), LLD( * ),
     $                   WORK( * ), Z( * )
*     ..
*     Common block to return operation count
*     .. Common blocks ..
      COMMON             / LATIME / OPS, ITCNT
*     ..
*     .. Scalars in Common ..
      DOUBLE PRECISION   ITCNT, OPS
*     ..
*
*  Purpose
*  =======
*
*  DLAR1V computes the (scaled) r-th column of the inverse of
*  the sumbmatrix in rows B1 through BN of the tridiagonal matrix
*  L D L^T - sigma I. The following steps accomplish this computation :
*  (a) Stationary qd transform,  L D L^T - sigma I = L(+) D(+) L(+)^T,
*  (b) Progressive qd transform, L D L^T - sigma I = U(-) D(-) U(-)^T,
*  (c) Computation of the diagonal elements of the inverse of
*      L D L^T - sigma I by combining the above transforms, and choosing
*      r as the index where the diagonal of the inverse is (one of the)
*      largest in magnitude.
*  (d) Computation of the (scaled) r-th column of the inverse using the
*      twisted factorization obtained by combining the top part of the
*      the stationary and the bottom part of the progressive transform.
*
*  Arguments
*  =========
*
*  N        (input) INTEGER
*           The order of the matrix L D L^T.
*
*  B1       (input) INTEGER
*           First index of the submatrix of L D L^T.
*
*  BN       (input) INTEGER
*           Last index of the submatrix of L D L^T.
*
*  SIGMA    (input) DOUBLE PRECISION
*           The shift. Initially, when R = 0, SIGMA should be a good
*           approximation to an eigenvalue of L D L^T.
*
*  L        (input) DOUBLE PRECISION array, dimension (N-1)
*           The (n-1) subdiagonal elements of the unit bidiagonal matrix
*           L, in elements 1 to N-1.
*
*  D        (input) DOUBLE PRECISION array, dimension (N)
*           The n diagonal elements of the diagonal matrix D.
*
*  LD       (input) DOUBLE PRECISION array, dimension (N-1)
*           The n-1 elements L(i)*D(i).
*
*  LLD      (input) DOUBLE PRECISION array, dimension (N-1)
*           The n-1 elements L(i)*L(i)*D(i).
*
*  GERSCH   (input) DOUBLE PRECISION array, dimension (2*N)
*           The n Gerschgorin intervals. These are used to restrict
*           the initial search for R, when R is input as 0.
*
*  Z        (output) DOUBLE PRECISION array, dimension (N)
*           The (scaled) r-th column of the inverse. Z(R) is returned
*           to be 1.
*
*  ZTZ      (output) DOUBLE PRECISION
*           The square of the norm of Z.
*
*  MINGMA   (output) DOUBLE PRECISION
*           The reciprocal of the largest (in magnitude) diagonal
*           element of the inverse of L D L^T - sigma I.
*
*  R        (input/output) INTEGER
*           Initially, R should be input to be 0 and is then output as
*           the index where the diagonal element of the inverse is
*           largest in magnitude. In later iterations, this same value
*           of R should be input.
*
*  ISUPPZ   (output) INTEGER array, dimension (2)
*           The support of the vector in Z, i.e., the vector Z is
*           nonzero only in elements ISUPPZ(1) through ISUPPZ( 2 ).
*
*  WORK     (workspace) DOUBLE PRECISION array, dimension (4*N)
*
*  Further Details
*  ===============
*
*  Based on contributions by
*     Inderjit Dhillon, IBM Almaden, USA
*     Osni Marques, LBNL/NERSC, USA
*
*  =====================================================================
*
*     .. Parameters ..
      INTEGER            BLKSIZ
      PARAMETER          ( BLKSIZ = 32 )
      DOUBLE PRECISION   ZERO, ONE
      PARAMETER          ( ZERO = 0.0D0, ONE = 1.0D0 )
*     ..
*     .. Local Scalars ..
      LOGICAL            SAWNAN
      INTEGER            FROM, I, INDP, INDS, INDUMN, J, R1, R2, TO
      DOUBLE PRECISION   DMINUS, DPLUS, EPS, S, TMP
*     ..
*     .. External Functions ..
      DOUBLE PRECISION   DLAMCH
      EXTERNAL           DLAMCH
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, DBLE, MAX, MIN
*     ..
*     .. Executable Statements ..
*
      EPS = DLAMCH( 'Precision' )
      IF( R.EQ.0 ) THEN
*
*        Eliminate the top and bottom indices from the possible values
*        of R where the desired eigenvector is largest in magnitude.
*
         R1 = B1
         DO 10 I = B1, BN
            IF( SIGMA.GE.GERSCH( 2*I-1 ) .OR. SIGMA.LE.GERSCH( 2*I ) )
     $           THEN
               R1 = I
               GO TO 20
            END IF
   10    CONTINUE
   20    CONTINUE
         R2 = BN
         DO 30 I = BN, B1, -1
            IF( SIGMA.GE.GERSCH( 2*I-1 ) .OR. SIGMA.LE.GERSCH( 2*I ) )
     $           THEN
               R2 = I
               GO TO 40
            END IF
   30    CONTINUE
   40    CONTINUE
      ELSE
         R1 = R
         R2 = R
      END IF
*
      INDUMN = N
      INDS = 2*N + 1
      INDP = 3*N + 1
      SAWNAN = .FALSE.
*
*     Compute the stationary transform (using the differential form)
*     untill the index R2
*
      IF( B1.EQ.1 ) THEN
         WORK( INDS ) = ZERO
      ELSE
         WORK( INDS ) = LLD( B1-1 )
      END IF
      OPS = OPS + DBLE( 1 )
      S = WORK( INDS ) - SIGMA
      DO 50 I = B1, R2 - 1
         OPS = OPS + DBLE( 5 )
         DPLUS = D( I ) + S
         WORK( I ) = LD( I ) / DPLUS
         WORK( INDS+I ) = S*WORK( I )*L( I )
         S = WORK( INDS+I ) - SIGMA
   50 CONTINUE
*
      IF( .NOT.( S.GT.ZERO .OR. S.LT.ONE ) ) THEN
*
*        Run a slower version of the above loop if a NaN is detected
*
         SAWNAN = .TRUE.
         J = B1 + 1
   60    CONTINUE
         IF( WORK( INDS+J ).GT.ZERO .OR. WORK( INDS+J ).LT.ONE ) THEN
            J = J + 1
            GO TO 60
         END IF
         WORK( INDS+J ) = LLD( J )
         S = WORK( INDS+J ) - SIGMA
         DO 70 I = J + 1, R2 - 1
            OPS = OPS + DBLE( 3 )
            DPLUS = D( I ) + S
            WORK( I ) = LD( I ) / DPLUS
            IF( WORK( I ).EQ.ZERO ) THEN
               WORK( INDS+I ) = LLD( I )
            ELSE
               OPS = OPS + DBLE( 2 )
               WORK( INDS+I ) = S*WORK( I )*L( I )
            END IF
            S = WORK( INDS+I ) - SIGMA
   70    CONTINUE
      END IF
      OPS = OPS + DBLE( 1 )
      WORK( INDP+BN-1 ) = D( BN ) - SIGMA
      DO 80 I = BN - 1, R1, -1
         OPS = OPS + DBLE( 5 )
         DMINUS = LLD( I ) + WORK( INDP+I )
         TMP = D( I ) / DMINUS
         WORK( INDUMN+I ) = L( I )*TMP
         WORK( INDP+I-1 ) = WORK( INDP+I )*TMP - SIGMA
   80 CONTINUE
      TMP = WORK( INDP+R1-1 )
      IF( .NOT.( TMP.GT.ZERO .OR. TMP.LT.ONE ) ) THEN
*
*        Run a slower version of the above loop if a NaN is detected
*
         SAWNAN = .TRUE.
         J = BN - 3
   90    CONTINUE
         IF( WORK( INDP+J ).GT.ZERO .OR. WORK( INDP+J ).LT.ONE ) THEN
            J = J - 1
            GO TO 90
         END IF
         OPS = OPS + DBLE( 1 )
         WORK( INDP+J ) = D( J+1 ) - SIGMA
         DO 100 I = J, R1, -1
            OPS = OPS + DBLE( 3 )
            DMINUS = LLD( I ) + WORK( INDP+I )
            TMP = D( I ) / DMINUS
            WORK( INDUMN+I ) = L( I )*TMP
            IF( TMP.EQ.ZERO ) THEN
               OPS = OPS + DBLE( 1 )
               WORK( INDP+I-1 ) = D( I ) - SIGMA
            ELSE
               OPS = OPS + DBLE( 2 )
               WORK( INDP+I-1 ) = WORK( INDP+I )*TMP - SIGMA
            END IF
  100    CONTINUE
      END IF
*
*     Find the index (from R1 to R2) of the largest (in magnitude)
*     diagonal element of the inverse
*
      MINGMA = WORK( INDS+R1-1 ) + WORK( INDP+R1-1 )
      IF( MINGMA.EQ.ZERO )
     $   MINGMA = EPS*WORK( INDS+R1-1 )
      R = R1
      DO 110 I = R1, R2 - 1
         OPS = OPS + DBLE( 1 )
         TMP = WORK( INDS+I ) + WORK( INDP+I )
         IF( TMP.EQ.ZERO ) THEN
            OPS = OPS + DBLE( 1 )
            TMP = EPS*WORK( INDS+I )
         END IF
         IF( ABS( TMP ).LT.ABS( MINGMA ) ) THEN
            MINGMA = TMP
            R = I + 1
         END IF
  110 CONTINUE
*
*     Compute the (scaled) r-th column of the inverse
*
      ISUPPZ( 1 ) = B1
      ISUPPZ( 2 ) = BN
      Z( R ) = ONE
      ZTZ = ONE
      IF( .NOT.SAWNAN ) THEN
         FROM = R - 1
         TO = MAX( R-BLKSIZ, B1 )
  120    CONTINUE
         IF( FROM.GE.B1 ) THEN
            DO 130 I = FROM, TO, -1
               OPS = OPS + DBLE( 3 )
               Z( I ) = -( WORK( I )*Z( I+1 ) )
               ZTZ = ZTZ + Z( I )*Z( I )
  130       CONTINUE
            IF( ABS( Z( TO ) ).LE.EPS .AND. ABS( Z( TO+1 ) ).LE.EPS )
     $           THEN
               ISUPPZ( 1 ) = TO + 2
            ELSE
               FROM = TO - 1
               TO = MAX( TO-BLKSIZ, B1 )
               GO TO 120
            END IF
         END IF
         FROM = R + 1
         TO = MIN( R+BLKSIZ, BN )
  140    CONTINUE
         IF( FROM.LE.BN ) THEN
            DO 150 I = FROM, TO
               OPS = OPS + DBLE( 3 )
               Z( I ) = -( WORK( INDUMN+I-1 )*Z( I-1 ) )
               ZTZ = ZTZ + Z( I )*Z( I )
  150       CONTINUE
            IF( ABS( Z( TO ) ).LE.EPS .AND. ABS( Z( TO-1 ) ).LE.EPS )
     $           THEN
               ISUPPZ( 2 ) = TO - 2
            ELSE
               FROM = TO + 1
               TO = MIN( TO+BLKSIZ, BN )
               GO TO 140
            END IF
         END IF
      ELSE
         DO 160 I = R - 1, B1, -1
            IF( Z( I+1 ).EQ.ZERO ) THEN
               OPS = OPS + DBLE( 2 )
               Z( I ) = -( LD( I+1 ) / LD( I ) )*Z( I+2 )
            ELSE IF( ABS( Z( I+1 ) ).LE.EPS .AND. ABS( Z( I+2 ) ).LE.
     $               EPS ) THEN
               ISUPPZ( 1 ) = I + 3
               GO TO 170
            ELSE
               OPS = OPS + DBLE( 1 )
               Z( I ) = -( WORK( I )*Z( I+1 ) )
            END IF
            OPS = OPS + DBLE( 2 )
            ZTZ = ZTZ + Z( I )*Z( I )
  160    CONTINUE
  170    CONTINUE
         DO 180 I = R, BN - 1
            IF( Z( I ).EQ.ZERO ) THEN
               OPS = OPS + DBLE( 2 )
               Z( I+1 ) = -( LD( I-1 ) / LD( I ) )*Z( I-1 )
            ELSE IF( ABS( Z( I ) ).LE.EPS .AND. ABS( Z( I-1 ) ).LE.EPS )
     $                THEN
               ISUPPZ( 2 ) = I - 2
               GO TO 190
            ELSE
               OPS = OPS + DBLE( 1 )
               Z( I+1 ) = -( WORK( INDUMN+I )*Z( I ) )
            END IF
            OPS = OPS + DBLE( 2 )
            ZTZ = ZTZ + Z( I+1 )*Z( I+1 )
  180    CONTINUE
  190    CONTINUE
      END IF
      DO 200 I = B1, ISUPPZ( 1 ) - 3
         Z( I ) = ZERO
  200 CONTINUE
      DO 210 I = ISUPPZ( 2 ) + 3, BN
         Z( I ) = ZERO
  210 CONTINUE
*
      RETURN
*
*     End of DLAR1V
*
      END
      SUBROUTINE DLARRB( N, D, L, LD, LLD, IFIRST, ILAST, SIGMA, RELTOL,
     $                   W, WGAP, WERR, WORK, IWORK, INFO )
*
*  -- LAPACK auxiliary routine (instru to count ops, version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     June 30, 1999
*
*     .. Scalar Arguments ..
      INTEGER            IFIRST, ILAST, INFO, N
      DOUBLE PRECISION   RELTOL, SIGMA
*     ..
*     .. Array Arguments ..
      INTEGER            IWORK( * )
      DOUBLE PRECISION   D( * ), L( * ), LD( * ), LLD( * ), W( * ),
     $                   WERR( * ), WGAP( * ), WORK( * )
*     ..
*     Common block to return operation count
*     .. Common blocks ..
      COMMON             / LATIME / OPS, ITCNT
*     ..
*     .. Scalars in Common ..
      DOUBLE PRECISION   ITCNT, OPS
*     ..
*
*  Purpose
*  =======
*
*  Given the relatively robust representation(RRR) L D L^T, DLARRB
*  does ``limited'' bisection to locate the eigenvalues of L D L^T,
*  W( IFIRST ) thru' W( ILAST ), to more accuracy. Intervals
*  [left, right] are maintained by storing their mid-points and
*  semi-widths in the arrays W and WERR respectively.
*
*  Arguments
*  =========
*
*  N       (input) INTEGER
*          The order of the matrix.
*
*  D       (input) DOUBLE PRECISION array, dimension (N)
*          The n diagonal elements of the diagonal matrix D.
*
*  L       (input) DOUBLE PRECISION array, dimension (N-1)
*          The n-1 subdiagonal elements of the unit bidiagonal matrix L.
*
*  LD      (input) DOUBLE PRECISION array, dimension (N-1)
*          The n-1 elements L(i)*D(i).
*
*  LLD     (input) DOUBLE PRECISION array, dimension (N-1)
*          The n-1 elements L(i)*L(i)*D(i).
*
*  IFIRST  (input) INTEGER
*          The index of the first eigenvalue in the cluster.
*
*  ILAST   (input) INTEGER
*          The index of the last eigenvalue in the cluster.
*
*  SIGMA   (input) DOUBLE PRECISION
*          The shift used to form L D L^T (see DLARRF).
*
*  RELTOL  (input) DOUBLE PRECISION
*          The relative tolerance.
*
*  W       (input/output) DOUBLE PRECISION array, dimension (N)
*          On input, W( IFIRST ) thru' W( ILAST ) are estimates of the
*          corresponding eigenvalues of L D L^T.
*          On output, these estimates are ``refined''.
*
*  WGAP    (input/output) DOUBLE PRECISION array, dimension (N)
*          The gaps between the eigenvalues of L D L^T. Very small
*          gaps are changed on output.
*
*  WERR    (input/output) DOUBLE PRECISION array, dimension (N)
*          On input, WERR( IFIRST ) thru' WERR( ILAST ) are the errors
*          in the estimates W( IFIRST ) thru' W( ILAST ).
*          On output, these are the ``refined'' errors.
*
*****Reminder to Inder --- WORK is never used in this subroutine *****
*  WORK    (input) DOUBLE PRECISION array, dimension (???)
*          Workspace.
*
*  IWORK   (input) INTEGER array, dimension (2*N)
*          Workspace.
*
*****Reminder to Inder --- INFO is never set in this subroutine ******
*  INFO    (output) INTEGER
*          Error flag.
*
*  Further Details
*  ===============
*
*  Based on contributions by
*     Inderjit Dhillon, IBM Almaden, USA
*     Osni Marques, LBNL/NERSC, USA
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO, TWO, HALF
      PARAMETER          ( ZERO = 0.0D0, TWO = 2.0D0, HALF = 0.5D0 )
*     ..
*     .. Local Scalars ..
      INTEGER            CNT, I, I1, I2, INITI1, INITI2, J, K, NCNVRG,
     $                   NEIG, NINT, NRIGHT, OLNINT
      DOUBLE PRECISION   DELTA, EPS, GAP, LEFT, MID, PERT, RIGHT, S,
     $                   THRESH, TMP, WIDTH
*     ..
*     .. External Functions ..
      DOUBLE PRECISION   DLAMCH
      EXTERNAL           DLAMCH
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, DBLE, MAX, MIN
*     ..
*     .. Executable Statements ..
*
      EPS = DLAMCH( 'Precision' )
      I1 = IFIRST
      I2 = IFIRST
      NEIG = ILAST - IFIRST + 1
      NCNVRG = 0
      THRESH = RELTOL
      DO 10 I = IFIRST, ILAST
         OPS = OPS + DBLE( 3 )
         IWORK( I ) = 0
         PERT = EPS*( ABS( SIGMA )+ABS( W( I ) ) )
         WERR( I ) = WERR( I ) + PERT
         IF( WGAP( I ).LT.PERT )
     $      WGAP( I ) = PERT
   10 CONTINUE
      DO 20 I = I1, ILAST
         IF( I.EQ.1 ) THEN
            GAP = WGAP( I )
         ELSE IF( I.EQ.N ) THEN
            GAP = WGAP( I-1 )
         ELSE
            GAP = MIN( WGAP( I-1 ), WGAP( I ) )
         END IF
         OPS = OPS + DBLE( 1 )
         IF( WERR( I ).LT.THRESH*GAP ) THEN
            NCNVRG = NCNVRG + 1
            IWORK( I ) = 1
            IF( I1.EQ.I )
     $         I1 = I1 + 1
         ELSE
            I2 = I
         END IF
   20 CONTINUE
*
*     Initialize the unconverged intervals.
*
      I = I1
      NINT = 0
      RIGHT = ZERO
   30 CONTINUE
      IF( I.LE.I2 ) THEN
         IF( IWORK( I ).EQ.0 ) THEN
            DELTA = EPS
            OPS = OPS + DBLE( 1 )
            LEFT = W( I ) - WERR( I )
*
*           Do while( CNT(LEFT).GT.I-1 )
*
   40       CONTINUE
            IF( I.GT.I1 .AND. LEFT.LE.RIGHT ) THEN
               LEFT = RIGHT
               CNT = I - 1
            ELSE
               S = -LEFT
               CNT = 0
               DO 50 J = 1, N - 1
                  OPS = OPS + DBLE( 5 )
                  TMP = D( J ) + S
                  S = S*( LD( J ) / TMP )*L( J ) - LEFT
                  IF( TMP.LT.ZERO )
     $               CNT = CNT + 1
   50          CONTINUE
               TMP = D( N ) + S
               IF( TMP.LT.ZERO )
     $            CNT = CNT + 1
               IF( CNT.GT.I-1 ) THEN
                  OPS = OPS + DBLE( 4 )
                  DELTA = TWO*DELTA
                  LEFT = LEFT - ( ABS( SIGMA )+ABS( LEFT ) )*DELTA
                  GO TO 40
               END IF
            END IF
            OPS = OPS + DBLE( 1 )
            DELTA = EPS
            RIGHT = W( I ) + WERR( I )
*
*           Do while( CNT(RIGHT).LT.I )
*
   60       CONTINUE
            S = -RIGHT
            CNT = 0
            OPS = OPS + DBLE( 5*( N-1 )+1 )
            DO 70 J = 1, N - 1
               TMP = D( J ) + S
               S = S*( LD( J ) / TMP )*L( J ) - RIGHT
               IF( TMP.LT.ZERO )
     $            CNT = CNT + 1
   70       CONTINUE
            TMP = D( N ) + S
            IF( TMP.LT.ZERO )
     $         CNT = CNT + 1
            IF( CNT.LT.I ) THEN
               OPS = OPS + DBLE( 4 )
               DELTA = TWO*DELTA
               RIGHT = RIGHT + ( ABS( SIGMA )+ABS( RIGHT ) )*DELTA
               GO TO 60
            END IF
            WERR( I ) = LEFT
            W( I ) = RIGHT
            IWORK( N+I ) = CNT
            NINT = NINT + 1
            I = CNT + 1
         ELSE
            I = I + 1
         END IF
         GO TO 30
      END IF
*
*     While( NCNVRG.LT.NEIG )
*
      INITI1 = I1
      INITI2 = I2
   80 CONTINUE
      IF( NCNVRG.LT.NEIG ) THEN
         OLNINT = NINT
         I = I1
         DO 100 K = 1, OLNINT
            NRIGHT = IWORK( N+I )
            IF( IWORK( I ).EQ.0 ) THEN
               OPS = OPS + DBLE( 2 )
               MID = HALF*( WERR( I )+W( I ) )
               S = -MID
               CNT = 0
               OPS = OPS + DBLE( 5*( N-1 )+1 )
               DO 90 J = 1, N - 1
                  TMP = D( J ) + S
                  S = S*( LD( J ) / TMP )*L( J ) - MID
                  IF( TMP.LT.ZERO )
     $               CNT = CNT + 1
   90          CONTINUE
               TMP = D( N ) + S
               IF( TMP.LT.ZERO )
     $            CNT = CNT + 1
               CNT = MAX( I-1, MIN( NRIGHT, CNT ) )
               IF( I.EQ.NRIGHT ) THEN
                  IF( I.EQ.IFIRST ) THEN
                     OPS = OPS + DBLE( 1 )
                     GAP = WERR( I+1 ) - W( I )
                  ELSE IF( I.EQ.ILAST ) THEN
                     OPS = OPS + DBLE( 1 )
                     GAP = WERR( I ) - W( I-1 )
                  ELSE
                     OPS = OPS + DBLE( 2 )
                     GAP = MIN( WERR( I+1 )-W( I ), WERR( I )-W( I-1 ) )
                  END IF
                  OPS = OPS + DBLE( 2 )
                  WIDTH = W( I ) - MID
                  IF( WIDTH.LT.THRESH*GAP ) THEN
                     NCNVRG = NCNVRG + 1
                     IWORK( I ) = 1
                     IF( I1.EQ.I ) THEN
                        I1 = I1 + 1
                        NINT = NINT - 1
                     END IF
                  END IF
               END IF
               IF( IWORK( I ).EQ.0 )
     $            I2 = K
               IF( CNT.EQ.I-1 ) THEN
                  WERR( I ) = MID
               ELSE IF( CNT.EQ.NRIGHT ) THEN
                  W( I ) = MID
               ELSE
                  IWORK( N+I ) = CNT
                  NINT = NINT + 1
                  WERR( CNT+1 ) = MID
                  W( CNT+1 ) = W( I )
                  W( I ) = MID
                  I = CNT + 1
                  IWORK( N+I ) = NRIGHT
               END IF
            END IF
            I = NRIGHT + 1
  100    CONTINUE
         NINT = NINT - OLNINT + I2
         GO TO 80
      END IF
      DO 110 I = INITI1, INITI2
         OPS = OPS + DBLE( 3 )
         W( I ) = HALF*( WERR( I )+W( I ) )
         WERR( I ) = W( I ) - WERR( I )
  110 CONTINUE
*
      RETURN
*
*     End of DLARRB
*
      END
      SUBROUTINE DLARRE( N, D, E, TOL, NSPLIT, ISPLIT, M, W, WOFF,
     $                   GERSCH, WORK, INFO )
*
*  -- LAPACK auxiliary routine (instru to count ops, version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     June 30, 1999
*
*     .. Scalar Arguments ..
      INTEGER            INFO, M, N, NSPLIT
      DOUBLE PRECISION   TOL
*     ..
*     .. Array Arguments ..
      INTEGER            ISPLIT( * )
      DOUBLE PRECISION   D( * ), E( * ), GERSCH( * ), W( * ), WOFF( * ),
     $                   WORK( * )
*     ..
*     Common block to return operation count
*     .. Common blocks ..
      COMMON             / LATIME / OPS, ITCNT
*     ..
*     .. Scalars in Common ..
      DOUBLE PRECISION   ITCNT, OPS
*     ..
*
*  Purpose
*  =======
*
*  Given the tridiagonal matrix T, DLARRE sets "small" off-diagonal
*  elements to zero, and for each unreduced block T_i, it finds
*  (i) the numbers sigma_i
*  (ii) the base T_i - sigma_i I = L_i D_i L_i^T representations and
*  (iii) eigenvalues of each L_i D_i L_i^T.
*  The representations and eigenvalues found are then used by
*  DSTEGR to compute the eigenvectors of a symmetric tridiagonal
*  matrix. Currently, the base representations are limited to being
*  positive or negative definite, and the eigenvalues of the definite
*  matrices are found by the dqds algorithm (subroutine DLASQ2). As
*  an added benefit, DLARRE also outputs the n Gerschgorin
*  intervals for each L_i D_i L_i^T.
*
*  Arguments
*  =========
*
*  N       (input) INTEGER
*          The order of the matrix.
*
*  D       (input/output) DOUBLE PRECISION array, dimension (N)
*          On entry, the n diagonal elements of the tridiagonal
*          matrix T.
*          On exit, the n diagonal elements of the diagonal
*          matrices D_i.
*
*  E       (input/output) DOUBLE PRECISION array, dimension (N)
*          On entry, the (n-1) subdiagonal elements of the tridiagonal
*          matrix T; E(N) need not be set.
*          On exit, the subdiagonal elements of the unit bidiagonal
*          matrices L_i.
*
*  TOL     (input) DOUBLE PRECISION
*          The threshold for splitting. If on input |E(i)| < TOL, then
*          the matrix T is split into smaller blocks.
*
*  NSPLIT  (input) INTEGER
*          The number of blocks T splits into. 1 <= NSPLIT <= N.
*
*  ISPLIT  (output) INTEGER array, dimension (2*N)
*          The splitting points, at which T breaks up into submatrices.
*          The first submatrix consists of rows/columns 1 to ISPLIT(1),
*          the second of rows/columns ISPLIT(1)+1 through ISPLIT(2),
*          etc., and the NSPLIT-th consists of rows/columns
*          ISPLIT(NSPLIT-1)+1 through ISPLIT(NSPLIT)=N.
*
*  M       (output) INTEGER
*          The total number of eigenvalues (of all the L_i D_i L_i^T)
*          found.
*
*  W       (output) DOUBLE PRECISION array, dimension (N)
*          The first M elements contain the eigenvalues. The
*          eigenvalues of each of the blocks, L_i D_i L_i^T, are
*          sorted in ascending order.
*
*  WOFF    (output) DOUBLE PRECISION array, dimension (N)
*          The NSPLIT base points sigma_i.
*
*  GERSCH  (output) DOUBLE PRECISION array, dimension (2*N)
*          The n Gerschgorin intervals.
*
*  WORK    (input) DOUBLE PRECISION array, dimension (4*N???)
*          Workspace.
*
*  INFO    (output) INTEGER
*          Output error code from DLASQ2
*
*  Further Details
*  ===============
*
*  Based on contributions by
*     Inderjit Dhillon, IBM Almaden, USA
*     Osni Marques, LBNL/NERSC, USA
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE, TWO, FOUR, FOURTH
      PARAMETER          ( ZERO = 0.0D0, ONE = 1.0D0, TWO = 2.0D0,
     $                   FOUR = 4.0D0, FOURTH = ONE / FOUR )
*     ..
*     .. Local Scalars ..
      INTEGER            CNT, I, IBEGIN, IEND, IN, J, JBLK, MAXCNT
      DOUBLE PRECISION   DELTA, EPS, GL, GU, NRM, OFFD, S, SGNDEF,
     $                   SIGMA, TAU, TMP1, WIDTH
*     ..
*     .. External Functions ..
      DOUBLE PRECISION   DLAMCH
      EXTERNAL           DLAMCH
*     ..
*     .. External Subroutines ..
      EXTERNAL           DCOPY, DLASQ2
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, DBLE, MAX, MIN
*     ..
*     .. Executable Statements ..
*
      INFO = 0
      EPS = DLAMCH( 'Precision' )
*
*     Compute Splitting Points
*
      NSPLIT = 1
      DO 10 I = 1, N - 1
         IF( ABS( E( I ) ).LE.TOL ) THEN
            ISPLIT( NSPLIT ) = I
            NSPLIT = NSPLIT + 1
         END IF
   10 CONTINUE
      ISPLIT( NSPLIT ) = N
*
      IBEGIN = 1
      DO 170 JBLK = 1, NSPLIT
         IEND = ISPLIT( JBLK )
         IF( IBEGIN.EQ.IEND ) THEN
            W( IBEGIN ) = D( IBEGIN )
            WOFF( JBLK ) = ZERO
            IBEGIN = IEND + 1
            GO TO 170
         END IF
         IN = IEND - IBEGIN + 1
*
*        Form the n Gerschgorin intervals
*
         OPS = OPS + DBLE( 4 )
         GL = D( IBEGIN ) - ABS( E( IBEGIN ) )
         GU = D( IBEGIN ) + ABS( E( IBEGIN ) )
         GERSCH( 2*IBEGIN-1 ) = GL
         GERSCH( 2*IBEGIN ) = GU
         GERSCH( 2*IEND-1 ) = D( IEND ) - ABS( E( IEND-1 ) )
         GERSCH( 2*IEND ) = D( IEND ) + ABS( E( IEND-1 ) )
         GL = MIN( GERSCH( 2*IEND-1 ), GL )
         GU = MAX( GERSCH( 2*IEND ), GU )
         DO 20 I = IBEGIN + 1, IEND - 1
            OPS = OPS + DBLE( 3 )
            OFFD = ABS( E( I-1 ) ) + ABS( E( I ) )
            GERSCH( 2*I-1 ) = D( I ) - OFFD
            GL = MIN( GERSCH( 2*I-1 ), GL )
            GERSCH( 2*I ) = D( I ) + OFFD
            GU = MAX( GERSCH( 2*I ), GU )
   20    CONTINUE
         NRM = MAX( ABS( GL ), ABS( GU ) )
*
*        Find the number SIGMA where the base representation
*        T - sigma I = L D L^T is to be formed.
*
         WIDTH = GU - GL
         DO 30 I = IBEGIN, IEND - 1
            OPS = OPS + DBLE( 1 )
            WORK( I ) = E( I )*E( I )
   30    CONTINUE
         OPS = OPS + DBLE( 6 )
         DO 50 J = 1, 2
            IF( J.EQ.1 ) THEN
               TAU = GL + FOURTH*WIDTH
            ELSE
               TAU = GU - FOURTH*WIDTH
            END IF
            TMP1 = D( IBEGIN ) - TAU
            IF( TMP1.LT.ZERO ) THEN
               CNT = 1
            ELSE
               CNT = 0
            END IF
            DO 40 I = IBEGIN + 1, IEND
               OPS = OPS + DBLE( 3 )
               TMP1 = D( I ) - TAU - WORK( I-1 ) / TMP1
               IF( TMP1.LT.ZERO )
     $            CNT = CNT + 1
   40       CONTINUE
            IF( CNT.EQ.0 ) THEN
               GL = TAU
            ELSE IF( CNT.EQ.IN ) THEN
               GU = TAU
            END IF
            IF( J.EQ.1 ) THEN
               MAXCNT = CNT
               SIGMA = GL
               SGNDEF = ONE
            ELSE
               IF( IN-CNT.GT.MAXCNT ) THEN
                  SIGMA = GU
                  SGNDEF = -ONE
               END IF
            END IF
   50    CONTINUE
*
*        Find the base L D L^T representation
*
         OPS = OPS + DBLE( 1 )
         WORK( 3*IN ) = ONE
         DELTA = EPS
         TAU = SGNDEF*NRM
   60    CONTINUE
         OPS = OPS + DBLE( 3+5*( IN-1 ) )
         SIGMA = SIGMA - DELTA*TAU
         WORK( 1 ) = D( IBEGIN ) - SIGMA
         J = IBEGIN
         DO 70 I = 1, IN - 1
            WORK( 2*IN+I ) = ONE / WORK( 2*I-1 )
            TMP1 = E( J )*WORK( 2*IN+I )
            WORK( 2*I+1 ) = ( D( J+1 )-SIGMA ) - TMP1*E( J )
            WORK( 2*I ) = TMP1
            J = J + 1
   70    CONTINUE
         OPS = OPS + DBLE( IN )
         DO 80 I = IN, 1, -1
            TMP1 = SGNDEF*WORK( 2*I-1 )
            IF( TMP1.LT.ZERO .OR. WORK( 2*IN+I ).EQ.ZERO .OR. .NOT.
     $          ( TMP1.GT.ZERO .OR. TMP1.LT.ONE ) ) THEN
               OPS = OPS + DBLE( 1 )
               DELTA = TWO*DELTA
               GO TO 60
            END IF
            J = J - 1
   80    CONTINUE
*
         OPS = OPS + DBLE( IN-1 )
         J = IBEGIN
         D( IBEGIN ) = WORK( 1 )
         WORK( 1 ) = ABS( WORK( 1 ) )
         DO 90 I = 1, IN - 1
            TMP1 = E( J )
            E( J ) = WORK( 2*I )
            WORK( 2*I ) = ABS( TMP1*WORK( 2*I ) )
            J = J + 1
            D( J ) = WORK( 2*I+1 )
            WORK( 2*I+1 ) = ABS( WORK( 2*I+1 ) )
   90    CONTINUE
*
         CALL DLASQ2( IN, WORK, INFO )
*
         OPS = OPS + DBLE( 2 )
         TAU = SGNDEF*WORK( IN )
         WORK( 3*IN ) = ONE
         DELTA = TWO*EPS
  100    CONTINUE
         OPS = OPS + DBLE( 2 )
         TAU = TAU*( ONE-DELTA )
*
         OPS = OPS + DBLE( 9*( IN-1 )+1 )
         S = -TAU
         J = IBEGIN
         DO 110 I = 1, IN - 1
            WORK( I ) = D( J ) + S
            WORK( 2*IN+I ) = ONE / WORK( I )
*           WORK( N+I ) = ( E( I ) * D( I ) ) / WORK( I )
            WORK( IN+I ) = ( E( J )*D( J ) )*WORK( 2*IN+I )
            S = S*WORK( IN+I )*E( J ) - TAU
            J = J + 1
  110    CONTINUE
         WORK( IN ) = D( IEND ) + S
*
*        Checking to see if all the diagonal elements of the new
*        L D L^T representation have the same sign
*
         OPS = OPS + DBLE( IN+1 )
         DO 120 I = IN, 1, -1
            TMP1 = SGNDEF*WORK( I )
            IF( TMP1.LT.ZERO .OR. WORK( 2*IN+I ).EQ.ZERO .OR. .NOT.
     $          ( TMP1.GT.ZERO .OR. TMP1.LT.ONE ) ) THEN
               OPS = OPS + DBLE( 1 )
               DELTA = TWO*DELTA
               GO TO 100
            END IF
  120    CONTINUE
*
         SIGMA = SIGMA + TAU
         CALL DCOPY( IN, WORK, 1, D( IBEGIN ), 1 )
         CALL DCOPY( IN-1, WORK( IN+1 ), 1, E( IBEGIN ), 1 )
         WOFF( JBLK ) = SIGMA
*
*        Update the n Gerschgorin intervals
*
         OPS = OPS + DBLE( 2 )
         DO 130 I = IBEGIN, IEND
            GERSCH( 2*I-1 ) = GERSCH( 2*I-1 ) - SIGMA
            GERSCH( 2*I ) = GERSCH( 2*I ) - SIGMA
  130    CONTINUE
*
*        Compute the eigenvalues of L D L^T.
*
         J = IBEGIN
         OPS = OPS + DBLE( 2*( IN-1 ) )
         DO 140 I = 1, IN - 1
            WORK( 2*I-1 ) = ABS( D( J ) )
            WORK( 2*I ) = E( J )*E( J )*WORK( 2*I-1 )
            J = J + 1
  140    CONTINUE
         WORK( 2*IN-1 ) = ABS( D( IEND ) )
*
         CALL DLASQ2( IN, WORK, INFO )
*
         J = IBEGIN
         IF( SGNDEF.GT.ZERO ) THEN
            DO 150 I = 1, IN
               W( J ) = WORK( IN-I+1 )
               J = J + 1
  150       CONTINUE
         ELSE
            DO 160 I = 1, IN
               W( J ) = -WORK( I )
               J = J + 1
  160       CONTINUE
         END IF
         IBEGIN = IEND + 1
  170 CONTINUE
      M = N
*
      RETURN
*
*     End of DLARRE
*
      END
      SUBROUTINE DLARRF( N, D, L, LD, LLD, IFIRST, ILAST, W, DPLUS,
     $                   LPLUS, WORK, IWORK, INFO )
*
*  -- LAPACK auxiliary routine (instru to count ops, version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     June 30, 1999
*
*     .. Scalar Arguments ..
      INTEGER            IFIRST, ILAST, INFO, N
*     ..
*     .. Array Arguments ..
      INTEGER            IWORK( * )
      DOUBLE PRECISION   D( * ), DPLUS( * ), L( * ), LD( * ), LLD( * ),
     $                   LPLUS( * ), W( * ), WORK( * )
*     ..
*     Common block to return operation count
*     .. Common blocks ..
      COMMON             / LATIME / OPS, ITCNT
*     ..
*     .. Scalars in Common ..
      DOUBLE PRECISION   ITCNT, OPS
*     ..
*
*  Purpose
*  =======
*
*  Given the initial representation L D L^T and its cluster of close
*  eigenvalues (in a relative measure), W( IFIRST ), W( IFIRST+1 ), ...
*  W( ILAST ), DLARRF finds a new relatively robust representation
*  L D L^T - SIGMA I = L(+) D(+) L(+)^T such that at least one of the
*  eigenvalues of L(+) D(+) L(+)^T is relatively isolated.
*
*  Arguments
*  =========
*
*  N       (input) INTEGER
*          The order of the matrix.
*
*  D       (input) DOUBLE PRECISION array, dimension (N)
*          The n diagonal elements of the diagonal matrix D.
*
*  L       (input) DOUBLE PRECISION array, dimension (N-1)
*          The (n-1) subdiagonal elements of the unit bidiagonal
*          matrix L.
*
*  LD      (input) DOUBLE PRECISION array, dimension (N-1)
*          The n-1 elements L(i)*D(i).
*
*  LLD     (input) DOUBLE PRECISION array, dimension (N-1)
*          The n-1 elements L(i)*L(i)*D(i).
*
*  IFIRST  (input) INTEGER
*          The index of the first eigenvalue in the cluster.
*
*  ILAST   (input) INTEGER
*          The index of the last eigenvalue in the cluster.
*
*  W       (input/output) DOUBLE PRECISION array, dimension (N)
*          On input, the eigenvalues of L D L^T in ascending order.
*          W( IFIRST ) through W( ILAST ) form the cluster of relatively
*          close eigenalues.
*          On output, W( IFIRST ) thru' W( ILAST ) are estimates of the
*          corresponding eigenvalues of L(+) D(+) L(+)^T.
*
*  SIGMA   (input) DOUBLE PRECISION
*          The shift used to form L(+) D(+) L(+)^T.
*
*  DPLUS   (output) DOUBLE PRECISION array, dimension (N)
*          The n diagonal elements of the diagonal matrix D(+).
*
*  LPLUS   (output) DOUBLE PRECISION array, dimension (N)
*          The first (n-1) elements of LPLUS contain the subdiagonal
*          elements of the unit bidiagonal matrix L(+). LPLUS( N ) is
*          set to SIGMA.
*
*  WORK    (input) DOUBLE PRECISION array, dimension (???)
*          Workspace.
*
*  Further Details
*  ===============
*
*  Based on contributions by
*     Inderjit Dhillon, IBM Almaden, USA
*     Osni Marques, LBNL/NERSC, USA
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO, TWO
      PARAMETER          ( ZERO = 0.0D0, TWO = 2.0D0 )
*     ..
*     .. Local Scalars ..
      INTEGER            I
      DOUBLE PRECISION   DELTA, EPS, S, SIGMA
*     ..
*     .. External Functions ..
      DOUBLE PRECISION   DLAMCH
      EXTERNAL           DLAMCH
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, DBLE
*     ..
*     .. Executable Statements ..
*
      INFO = 0
      EPS = DLAMCH( 'Precision' )
      IF( IFIRST.EQ.1 ) THEN
         SIGMA = W( IFIRST )
      ELSE IF( ILAST.EQ.N ) THEN
         SIGMA = W( ILAST )
      ELSE
         INFO = 1
         RETURN
      END IF
*
*     Compute the new relatively robust representation (RRR)
*
      OPS = OPS + DBLE( 3 )
      DELTA = TWO*EPS
   10 CONTINUE
      IF( IFIRST.EQ.1 ) THEN
         SIGMA = SIGMA - ABS( SIGMA )*DELTA
      ELSE
         SIGMA = SIGMA + ABS( SIGMA )*DELTA
      END IF
      S = -SIGMA
      OPS = OPS + DBLE( 5*( N-1 )+1 )
      DO 20 I = 1, N - 1
         DPLUS( I ) = D( I ) + S
         LPLUS( I ) = LD( I ) / DPLUS( I )
         S = S*LPLUS( I )*L( I ) - SIGMA
   20 CONTINUE
      DPLUS( N ) = D( N ) + S
      IF( IFIRST.EQ.1 ) THEN
         DO 30 I = 1, N
            IF( DPLUS( I ).LT.ZERO ) THEN
               OPS = OPS + DBLE( 1 )
               DELTA = TWO*DELTA
               GO TO 10
            END IF
   30    CONTINUE
      ELSE
         DO 40 I = 1, N
            IF( DPLUS( I ).GT.ZERO ) THEN
               OPS = OPS + DBLE( 1 )
               DELTA = TWO*DELTA
               GO TO 10
            END IF
   40    CONTINUE
      END IF
      DO 50 I = IFIRST, ILAST
         OPS = OPS + DBLE( 1 )
         W( I ) = W( I ) - SIGMA
   50 CONTINUE
      LPLUS( N ) = SIGMA
*
      RETURN
*
*     End of DLARRF
*
      END
      SUBROUTINE DLARRV( N, D, L, ISPLIT, M, W, IBLOCK, GERSCH, TOL, Z,
     $                   LDZ, ISUPPZ, WORK, IWORK, INFO )
*
*  -- LAPACK auxiliary routine (instru to count ops, version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     June 30, 1999
*
*     .. Scalar Arguments ..
      INTEGER            INFO, LDZ, M, N
      DOUBLE PRECISION   TOL
*     ..
*     .. Array Arguments ..
      INTEGER            IBLOCK( * ), ISPLIT( * ), ISUPPZ( * ),
     $                   IWORK( * )
      DOUBLE PRECISION   D( * ), GERSCH( * ), L( * ), W( * ), WORK( * ),
     $                   Z( LDZ, * )
*     ..
*     Common block to return operation count
*     .. Common blocks ..
      COMMON             / LATIME / OPS, ITCNT
*     ..
*     .. Scalars in Common ..
      DOUBLE PRECISION   ITCNT, OPS
*     ..
*
*  Purpose
*  =======
*
*  DLARRV computes the eigenvectors of the tridiagonal matrix
*  T = L D L^T given L, D and the eigenvalues of L D L^T.
*  The input eigenvalues should have high relative accuracy with
*  respect to the entries of L and D. The desired accuracy of the
*  output can be specified by the input parameter TOL.
*
*  Arguments
*  =========
*
*  N       (input) INTEGER
*          The order of the matrix.  N >= 0.
*
*  D       (input/output) DOUBLE PRECISION array, dimension (N)
*          On entry, the n diagonal elements of the diagonal matrix D.
*          On exit, D may be overwritten.
*
*  L       (input/output) DOUBLE PRECISION array, dimension (N-1)
*          On entry, the (n-1) subdiagonal elements of the unit
*          bidiagonal matrix L in elements 1 to N-1 of L. L(N) need
*          not be set. On exit, L is overwritten.
*
*  ISPLIT  (input) INTEGER array, dimension (N)
*          The splitting points, at which T breaks up into submatrices.
*          The first submatrix consists of rows/columns 1 to
*          ISPLIT( 1 ), the second of rows/columns ISPLIT( 1 )+1
*          through ISPLIT( 2 ), etc.
*
*  TOL     (input) DOUBLE PRECISION
*          The absolute error tolerance for the
*          eigenvalues/eigenvectors.
*          Errors in the input eigenvalues must be bounded by TOL.
*          The eigenvectors output have residual norms
*          bounded by TOL, and the dot products between different
*          eigenvectors are bounded by TOL. TOL must be at least
*          N*EPS*|T|, where EPS is the machine precision and |T| is
*          the 1-norm of the tridiagonal matrix.
*
*  M       (input) INTEGER
*          The total number of eigenvalues found.  0 <= M <= N.
*          If RANGE = 'A', M = N, and if RANGE = 'I', M = IU-IL+1.
*
*  W       (input) DOUBLE PRECISION array, dimension (N)
*          The first M elements of W contain the eigenvalues for
*          which eigenvectors are to be computed.  The eigenvalues
*          should be grouped by split-off block and ordered from
*          smallest to largest within the block ( The output array
*          W from DLARRE is expected here ).
*          Errors in W must be bounded by TOL (see above).
*
*  IBLOCK  (input) INTEGER array, dimension (N)
*          The submatrix indices associated with the corresponding
*          eigenvalues in W; IBLOCK(i)=1 if eigenvalue W(i) belongs to
*          the first submatrix from the top, =2 if W(i) belongs to
*          the second submatrix, etc.
*
*  Z       (output) DOUBLE PRECISION array, dimension (LDZ, max(1,M) )
*          If JOBZ = 'V', then if INFO = 0, the first M columns of Z
*          contain the orthonormal eigenvectors of the matrix T
*          corresponding to the selected eigenvalues, with the i-th
*          column of Z holding the eigenvector associated with W(i).
*          If JOBZ = 'N', then Z is not referenced.
*          Note: the user must ensure that at least max(1,M) columns are
*          supplied in the array Z; if RANGE = 'V', the exact value of M
*          is not known in advance and an upper bound must be used.
*
*  LDZ     (input) INTEGER
*          The leading dimension of the array Z.  LDZ >= 1, and if
*          JOBZ = 'V', LDZ >= max(1,N).
*
*  ISUPPZ  (output) INTEGER ARRAY, dimension ( 2*max(1,M) )
*          The support of the eigenvectors in Z, i.e., the indices
*          indicating the nonzero elements in Z. The i-th eigenvector
*          is nonzero only in elements ISUPPZ( 2*i-1 ) through
*          ISUPPZ( 2*i ).
*
*  WORK    (workspace) DOUBLE PRECISION array, dimension (13*N)
*
*  IWORK   (workspace) INTEGER array, dimension (6*N)
*
*  INFO    (output) INTEGER
*          = 0:  successful exit
*          < 0:  if INFO = -i, the i-th argument had an illegal value
*          > 0:  if INFO = 1, internal error in DLARRB
*                if INFO = 2, internal error in DSTEIN
*
*  Further Details
*  ===============
*
*  Based on contributions by
*     Inderjit Dhillon, IBM Almaden, USA
*     Osni Marques, LBNL/NERSC, USA
*     Ken Stanley, Computer Science Division, University of
*       California at Berkeley, USA
*
*  =====================================================================
*
*     .. Parameters ..
      INTEGER            MGSSIZ
      PARAMETER          ( MGSSIZ = 20 )
      DOUBLE PRECISION   ZERO, ONE, FOUR
      PARAMETER          ( ZERO = 0.0D0, ONE = 1.0D0, FOUR = 4.0D0 )
*     ..
*     .. Local Scalars ..
      LOGICAL            MGSCLS
      INTEGER            I, IBEGIN, IEND, IINDC1, IINDC2, IINDR, IINDWK,
     $                   IINFO, IM, IN, INDERR, INDGAP, INDLD, INDLLD,
     $                   INDWRK, ITER, ITMP1, ITMP2, J, JBLK, K, KTOT,
     $                   LSBDPT, MAXITR, NCLUS, NDEPTH, NDONE, NEWCLS,
     $                   NEWFRS, NEWFTT, NEWLST, NEWSIZ, NSPLIT, OLDCLS,
     $                   OLDFST, OLDIEN, OLDLST, OLDNCL, P, Q
      DOUBLE PRECISION   EPS, GAP, LAMBDA, MGSTOL, MINGMA, MINRGP,
     $                   NRMINV, RELGAP, RELTOL, RESID, RQCORR, SIGMA,
     $                   TMP1, ZTZ
*     ..
*     .. External Functions ..
      DOUBLE PRECISION   DDOT, DLAMCH, DNRM2
      EXTERNAL           DDOT, DLAMCH, DNRM2
*     ..
*     .. External Subroutines ..
      EXTERNAL           DAXPY, DCOPY, DLAR1V, DLARRB, DLARRF, DLASET,
     $                   DSCAL, DSTEIN
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, DBLE, MAX, MIN, SQRT
*     ..
*     .. Local Arrays ..
      INTEGER            TEMP( 1 )
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters.
*
      INDERR = N + 1
      INDLD = 2*N
      INDLLD = 3*N
      INDGAP = 4*N
      INDWRK = 5*N + 1
*
      IINDR = N
      IINDC1 = 2*N
      IINDC2 = 3*N
      IINDWK = 4*N + 1
*
      EPS = DLAMCH( 'Precision' )
*
      DO 10 I = 1, 2*N
         IWORK( I ) = 0
   10 CONTINUE
      OPS = OPS + DBLE( M+1 )
      DO 20 I = 1, M
         WORK( INDERR+I-1 ) = EPS*ABS( W( I ) )
   20 CONTINUE
      CALL DLASET( 'Full', N, N, ZERO, ZERO, Z, LDZ )
      MGSTOL = 5.0D0*EPS
*
      NSPLIT = IBLOCK( M )
      IBEGIN = 1
      DO 170 JBLK = 1, NSPLIT
         IEND = ISPLIT( JBLK )
*
*        Find the eigenvectors of the submatrix indexed IBEGIN
*        through IEND.
*
         IF( IBEGIN.EQ.IEND ) THEN
            Z( IBEGIN, IBEGIN ) = ONE
            ISUPPZ( 2*IBEGIN-1 ) = IBEGIN
            ISUPPZ( 2*IBEGIN ) = IBEGIN
            IBEGIN = IEND + 1
            GO TO 170
         END IF
         OLDIEN = IBEGIN - 1
         IN = IEND - OLDIEN
         OPS = OPS + DBLE( 1 )
         RELTOL = MIN( 1.0D-2, ONE / DBLE( IN ) )
         IM = IN
         CALL DCOPY( IM, W( IBEGIN ), 1, WORK, 1 )
         OPS = OPS + DBLE( IN-1 )
         DO 30 I = 1, IN - 1
            WORK( INDGAP+I ) = WORK( I+1 ) - WORK( I )
   30    CONTINUE
         WORK( INDGAP+IN ) = MAX( ABS( WORK( IN ) ), EPS )
         NDONE = 0
*
         NDEPTH = 0
         LSBDPT = 1
         NCLUS = 1
         IWORK( IINDC1+1 ) = 1
         IWORK( IINDC1+2 ) = IN
*
*        While( NDONE.LT.IM ) do
*
   40    CONTINUE
         IF( NDONE.LT.IM ) THEN
            OLDNCL = NCLUS
            NCLUS = 0
            LSBDPT = 1 - LSBDPT
            DO 150 I = 1, OLDNCL
               IF( LSBDPT.EQ.0 ) THEN
                  OLDCLS = IINDC1
                  NEWCLS = IINDC2
               ELSE
                  OLDCLS = IINDC2
                  NEWCLS = IINDC1
               END IF
*
*              If NDEPTH > 1, retrieve the relatively robust
*              representation (RRR) and perform limited bisection
*              (if necessary) to get approximate eigenvalues.
*
               J = OLDCLS + 2*I
               OLDFST = IWORK( J-1 )
               OLDLST = IWORK( J )
               IF( NDEPTH.GT.0 ) THEN
                  J = OLDIEN + OLDFST
                  CALL DCOPY( IN, Z( IBEGIN, J ), 1, D( IBEGIN ), 1 )
                  CALL DCOPY( IN, Z( IBEGIN, J+1 ), 1, L( IBEGIN ), 1 )
                  SIGMA = L( IEND )
               END IF
               K = IBEGIN
               OPS = OPS + DBLE( 2*( IN-1 ) )
               DO 50 J = 1, IN - 1
                  WORK( INDLD+J ) = D( K )*L( K )
                  WORK( INDLLD+J ) = WORK( INDLD+J )*L( K )
                  K = K + 1
   50          CONTINUE
               IF( NDEPTH.GT.0 ) THEN
                  CALL DLARRB( IN, D( IBEGIN ), L( IBEGIN ),
     $                         WORK( INDLD+1 ), WORK( INDLLD+1 ),
     $                         OLDFST, OLDLST, SIGMA, RELTOL, WORK,
     $                         WORK( INDGAP+1 ), WORK( INDERR ),
     $                         WORK( INDWRK ), IWORK( IINDWK ), IINFO )
                  IF( IINFO.NE.0 ) THEN
                     INFO = 1
                     RETURN
                  END IF
               END IF
*
*              Classify eigenvalues of the current representation (RRR)
*              as (i) isolated, (ii) loosely clustered or (iii) tightly
*              clustered
*
               NEWFRS = OLDFST
               DO 140 J = OLDFST, OLDLST
                  OPS = OPS + DBLE( 1 )
                  IF( J.EQ.OLDLST .OR. WORK( INDGAP+J ).GE.RELTOL*
     $                ABS( WORK( J ) ) ) THEN
                     NEWLST = J
                  ELSE
*
*                    continue (to the next loop)
*
                     OPS = OPS + DBLE( 1 )
                     RELGAP = WORK( INDGAP+J ) / ABS( WORK( J ) )
                     IF( J.EQ.NEWFRS ) THEN
                        MINRGP = RELGAP
                     ELSE
                        MINRGP = MIN( MINRGP, RELGAP )
                     END IF
                     GO TO 140
                  END IF
                  NEWSIZ = NEWLST - NEWFRS + 1
                  MAXITR = 10
                  NEWFTT = OLDIEN + NEWFRS
                  IF( NEWSIZ.GT.1 ) THEN
                     MGSCLS = NEWSIZ.LE.MGSSIZ .AND. MINRGP.GE.MGSTOL
                     IF( .NOT.MGSCLS ) THEN
                        CALL DLARRF( IN, D( IBEGIN ), L( IBEGIN ),
     $                               WORK( INDLD+1 ), WORK( INDLLD+1 ),
     $                               NEWFRS, NEWLST, WORK,
     $                               Z( IBEGIN, NEWFTT ),
     $                               Z( IBEGIN, NEWFTT+1 ),
     $                               WORK( INDWRK ), IWORK( IINDWK ),
     $                               INFO )
                        IF( INFO.EQ.0 ) THEN
                           NCLUS = NCLUS + 1
                           K = NEWCLS + 2*NCLUS
                           IWORK( K-1 ) = NEWFRS
                           IWORK( K ) = NEWLST
                        ELSE
                           INFO = 0
                           IF( MINRGP.GE.MGSTOL ) THEN
                              MGSCLS = .TRUE.
                           ELSE
*
*                             Call DSTEIN to process this tight cluster.
*                             This happens only if MINRGP <= MGSTOL
*                             and DLARRF returns INFO = 1. The latter
*                             means that a new RRR to "break" the
*                             cluster could not be found.
*
                              WORK( INDWRK ) = D( IBEGIN )
                              OPS = OPS + DBLE( IN-1 )
                              DO 60 K = 1, IN - 1
                                 WORK( INDWRK+K ) = D( IBEGIN+K ) +
     $                                              WORK( INDLLD+K )
   60                         CONTINUE
                              DO 70 K = 1, NEWSIZ
                                 IWORK( IINDWK+K-1 ) = 1
   70                         CONTINUE
                              DO 80 K = NEWFRS, NEWLST
                                 ISUPPZ( 2*( IBEGIN+K )-3 ) = 1
                                 ISUPPZ( 2*( IBEGIN+K )-2 ) = IN
   80                         CONTINUE
                              TEMP( 1 ) = IN
                              CALL DSTEIN( IN, WORK( INDWRK ),
     $                                     WORK( INDLD+1 ), NEWSIZ,
     $                                     WORK( NEWFRS ),
     $                                     IWORK( IINDWK ), TEMP( 1 ),
     $                                     Z( IBEGIN, NEWFTT ), LDZ,
     $                                     WORK( INDWRK+IN ),
     $                                     IWORK( IINDWK+IN ),
     $                                     IWORK( IINDWK+2*IN ), IINFO )
                              IF( IINFO.NE.0 ) THEN
                                 INFO = 2
                                 RETURN
                              END IF
                              NDONE = NDONE + NEWSIZ
                           END IF
                        END IF
                     END IF
                  ELSE
                     MGSCLS = .FALSE.
                  END IF
                  IF( NEWSIZ.EQ.1 .OR. MGSCLS ) THEN
                     KTOT = NEWFTT
                     DO 100 K = NEWFRS, NEWLST
                        ITER = 0
   90                   CONTINUE
                        LAMBDA = WORK( K )
                        CALL DLAR1V( IN, 1, IN, LAMBDA, D( IBEGIN ),
     $                               L( IBEGIN ), WORK( INDLD+1 ),
     $                               WORK( INDLLD+1 ),
     $                               GERSCH( 2*OLDIEN+1 ),
     $                               Z( IBEGIN, KTOT ), ZTZ, MINGMA,
     $                               IWORK( IINDR+KTOT ),
     $                               ISUPPZ( 2*KTOT-1 ),
     $                               WORK( INDWRK ) )
                        OPS = OPS + DBLE( 4 )
                        TMP1 = ONE / ZTZ
                        NRMINV = SQRT( TMP1 )
                        RESID = ABS( MINGMA )*NRMINV
                        RQCORR = MINGMA*TMP1
                        IF( K.EQ.IN ) THEN
                           GAP = WORK( INDGAP+K-1 )
                        ELSE IF( K.EQ.1 ) THEN
                           GAP = WORK( INDGAP+K )
                        ELSE
                           GAP = MIN( WORK( INDGAP+K-1 ),
     $                           WORK( INDGAP+K ) )
                        END IF
                        ITER = ITER + 1
                        OPS = OPS + DBLE( 3 )
                        IF( RESID.GT.TOL*GAP .AND. ABS( RQCORR ).GT.
     $                      FOUR*EPS*ABS( LAMBDA ) ) THEN
                           OPS = OPS + DBLE( 1 )
                           WORK( K ) = LAMBDA + RQCORR
                           IF( ITER.LT.MAXITR ) THEN
                              GO TO 90
                           END IF
                        END IF
                        IWORK( KTOT ) = 1
                        IF( NEWSIZ.EQ.1 )
     $                     NDONE = NDONE + 1
                        OPS = OPS + DBLE( IN )
                        CALL DSCAL( IN, NRMINV, Z( IBEGIN, KTOT ), 1 )
                        KTOT = KTOT + 1
  100                CONTINUE
                     IF( NEWSIZ.GT.1 ) THEN
                        ITMP1 = ISUPPZ( 2*NEWFTT-1 )
                        ITMP2 = ISUPPZ( 2*NEWFTT )
                        KTOT = OLDIEN + NEWLST
                        DO 120 P = NEWFTT + 1, KTOT
                           DO 110 Q = NEWFTT, P - 1
                              OPS = OPS + DBLE( 4*IN )
                              TMP1 = -DDOT( IN, Z( IBEGIN, P ), 1,
     $                               Z( IBEGIN, Q ), 1 )
                              CALL DAXPY( IN, TMP1, Z( IBEGIN, Q ), 1,
     $                                    Z( IBEGIN, P ), 1 )
  110                      CONTINUE
                           OPS = OPS + DBLE( 3*IN+1 )
                           TMP1 = ONE / DNRM2( IN, Z( IBEGIN, P ), 1 )
                           CALL DSCAL( IN, TMP1, Z( IBEGIN, P ), 1 )
                           ITMP1 = MIN( ITMP1, ISUPPZ( 2*P-1 ) )
                           ITMP2 = MAX( ITMP2, ISUPPZ( 2*P ) )
  120                   CONTINUE
                        DO 130 P = NEWFTT, KTOT
                           ISUPPZ( 2*P-1 ) = ITMP1
                           ISUPPZ( 2*P ) = ITMP2
  130                   CONTINUE
                        NDONE = NDONE + NEWSIZ
                     END IF
                  END IF
                  NEWFRS = J + 1
  140          CONTINUE
  150       CONTINUE
            NDEPTH = NDEPTH + 1
            GO TO 40
         END IF
         J = 2*IBEGIN
         DO 160 I = IBEGIN, IEND
            ISUPPZ( J-1 ) = ISUPPZ( J-1 ) + OLDIEN
            ISUPPZ( J ) = ISUPPZ( J ) + OLDIEN
            J = J + 2
  160    CONTINUE
         IBEGIN = IEND + 1
  170 CONTINUE
*
      RETURN
*
*     End of DLARRV
*
      END
      SUBROUTINE DLASD0( N, SQRE, D, E, U, LDU, VT, LDVT, SMLSIZ, IWORK,
     $                   WORK, INFO )
*
*  -- LAPACK auxiliary routine (instrumented to count ops, version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     June 30, 1999
*
*     .. Scalar Arguments ..
      INTEGER            INFO, LDU, LDVT, N, SMLSIZ, SQRE
*     ..
*     .. Array Arguments ..
      INTEGER            IWORK( * )
      DOUBLE PRECISION   D( * ), E( * ), U( LDU, * ), VT( LDVT, * ),
     $                   WORK( * )
*     ..
*     .. Common block to return operation count ..
      COMMON             / LATIME / OPS, ITCNT
*     ..
*     .. Scalars in Common ..
      DOUBLE PRECISION   ITCNT, OPS
*     ..
*
*  Purpose
*  =======
*
*  Using a divide and conquer approach, DLASD0 computes the singular
*  value decomposition (SVD) of a real upper bidiagonal N-by-M
*  matrix B with diagonal D and offdiagonal E, where M = N + SQRE.
*  The algorithm computes orthogonal matrices U and VT such that
*  B = U * S * VT. The singular values S are overwritten on D.
*
*  A related subroutine, DLASDA, computes only the singular values,
*  and optionally, the singular vectors in compact form.
*
*  Arguments
*  =========
*
*  N      (input) INTEGER
*         On entry, the row dimension of the upper bidiagonal matrix.
*         This is also the dimension of the main diagonal array D.
*
*  SQRE   (input) INTEGER
*         Specifies the column dimension of the bidiagonal matrix.
*         = 0: The bidiagonal matrix has column dimension M = N;
*         = 1: The bidiagonal matrix has column dimension M = N+1;
*
*  D      (input/output) DOUBLE PRECISION array, dimension (N)
*         On entry D contains the main diagonal of the bidiagonal
*         matrix.
*         On exit D, if INFO = 0, contains its singular values.
*
*  E      (input) DOUBLE PRECISION array, dimension (M-1)
*         Contains the subdiagonal entries of the bidiagonal matrix.
*         On exit, E has been destroyed.
*
*  U      (output) DOUBLE PRECISION array, dimension at least (LDQ, N)
*         On exit, U contains the left singular vectors.
*
*  LDU    (input) INTEGER
*         On entry, leading dimension of U.
*
*  VT     (output) DOUBLE PRECISION array, dimension at least (LDVT, M)
*         On exit, VT' contains the right singular vectors.
*
*  LDVT   (input) INTEGER
*         On entry, leading dimension of VT.
*
*  SMLSIZ (input) INTEGER
*         On entry, maximum size of the subproblems at the
*         bottom of the computation tree.
*
*  IWORK  INTEGER work array.
*         Dimension must be at least (8 * N)
*
*  WORK   DOUBLE PRECISION work array.
*         Dimension must be at least (3 * M**2 + 2 * M)
*
*  INFO   (output) INTEGER
*          = 0:  successful exit.
*          < 0:  if INFO = -i, the i-th argument had an illegal value.
*          > 0:  if INFO = 1, an singular value did not converge
*
*  Further Details
*  ===============
*
*  Based on contributions by
*     Ming Gu and Huan Ren, Computer Science Division, University of
*     California at Berkeley, USA
*
*  =====================================================================
*
*     .. Local Scalars ..
      INTEGER            I, I1, IC, IDXQ, IDXQC, IM1, INODE, ITEMP, IWK,
     $                   J, LF, LL, LVL, M, NCC, ND, NDB1, NDIML, NDIMR,
     $                   NL, NLF, NLP1, NLVL, NR, NRF, NRP1, SQREI
      DOUBLE PRECISION   ALPHA, BETA
*     ..
*     .. External Subroutines ..
      EXTERNAL           DLASD1, DLASDQ, DLASDT, XERBLA
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters.
*
      INFO = 0
*
      IF( N.LT.0 ) THEN
         INFO = -1
      ELSE IF( ( SQRE.LT.0 ) .OR. ( SQRE.GT.1 ) ) THEN
         INFO = -2
      END IF
*
      M = N + SQRE
*
      IF( LDU.LT.N ) THEN
         INFO = -6
      ELSE IF( LDVT.LT.M ) THEN
         INFO = -8
      ELSE IF( SMLSIZ.LT.3 ) THEN
         INFO = -9
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DLASD0', -INFO )
         RETURN
      END IF
*
*     If the input matrix is too small, call DLASDQ to find the SVD.
*
      IF( N.LE.SMLSIZ ) THEN
         CALL DLASDQ( 'U', SQRE, N, M, N, 0, D, E, VT, LDVT, U, LDU, U,
     $                LDU, WORK, INFO )
         RETURN
      END IF
*
*     Set up the computation tree.
*
      INODE = 1
      NDIML = INODE + N
      NDIMR = NDIML + N
      IDXQ = NDIMR + N
      IWK = IDXQ + N
      CALL DLASDT( N, NLVL, ND, IWORK( INODE ), IWORK( NDIML ),
     $             IWORK( NDIMR ), SMLSIZ )
*
*     For the nodes on bottom level of the tree, solve
*     their subproblems by DLASDQ.
*
      NDB1 = ( ND+1 ) / 2
      NCC = 0
      DO 30 I = NDB1, ND
*
*     IC : center row of each node
*     NL : number of rows of left  subproblem
*     NR : number of rows of right subproblem
*     NLF: starting row of the left   subproblem
*     NRF: starting row of the right  subproblem
*
         I1 = I - 1
         IC = IWORK( INODE+I1 )
         NL = IWORK( NDIML+I1 )
         NLP1 = NL + 1
         NR = IWORK( NDIMR+I1 )
         NRP1 = NR + 1
         NLF = IC - NL
         NRF = IC + 1
         SQREI = 1
         CALL DLASDQ( 'U', SQREI, NL, NLP1, NL, NCC, D( NLF ), E( NLF ),
     $                VT( NLF, NLF ), LDVT, U( NLF, NLF ), LDU,
     $                U( NLF, NLF ), LDU, WORK, INFO )
         IF( INFO.NE.0 ) THEN
            RETURN
         END IF
         ITEMP = IDXQ + NLF - 2
         DO 10 J = 1, NL
            IWORK( ITEMP+J ) = J
   10    CONTINUE
         IF( I.EQ.ND ) THEN
            SQREI = SQRE
         ELSE
            SQREI = 1
         END IF
         NRP1 = NR + SQREI
         CALL DLASDQ( 'U', SQREI, NR, NRP1, NR, NCC, D( NRF ), E( NRF ),
     $                VT( NRF, NRF ), LDVT, U( NRF, NRF ), LDU,
     $                U( NRF, NRF ), LDU, WORK, INFO )
         IF( INFO.NE.0 ) THEN
            RETURN
         END IF
         ITEMP = IDXQ + IC
         DO 20 J = 1, NR
            IWORK( ITEMP+J-1 ) = J
   20    CONTINUE
   30 CONTINUE
*
*     Now conquer each subproblem bottom-up.
*
      DO 50 LVL = NLVL, 1, -1
*
*        Find the first node LF and last node LL on the
*        current level LVL.
*
         IF( LVL.EQ.1 ) THEN
            LF = 1
            LL = 1
         ELSE
            LF = 2**( LVL-1 )
            LL = 2*LF - 1
         END IF
         DO 40 I = LF, LL
            IM1 = I - 1
            IC = IWORK( INODE+IM1 )
            NL = IWORK( NDIML+IM1 )
            NR = IWORK( NDIMR+IM1 )
            NLF = IC - NL
            IF( ( SQRE.EQ.0 ) .AND. ( I.EQ.LL ) ) THEN
               SQREI = SQRE
            ELSE
               SQREI = 1
            END IF
            IDXQC = IDXQ + NLF - 1
            ALPHA = D( IC )
            BETA = E( IC )
            CALL DLASD1( NL, NR, SQREI, D( NLF ), ALPHA, BETA,
     $                   U( NLF, NLF ), LDU, VT( NLF, NLF ), LDVT,
     $                   IWORK( IDXQC ), IWORK( IWK ), WORK, INFO )
            IF( INFO.NE.0 ) THEN
               RETURN
            END IF
   40    CONTINUE
   50 CONTINUE
*
      RETURN
*
*     End of DLASD0
*
      END
      SUBROUTINE DLASD1( NL, NR, SQRE, D, ALPHA, BETA, U, LDU, VT, LDVT,
     $                   IDXQ, IWORK, WORK, INFO )
*
*  -- LAPACK auxiliary routine (instrumented to count ops, version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     June 30, 1999
*
*     .. Scalar Arguments ..
      INTEGER            INFO, LDU, LDVT, NL, NR, SQRE
      DOUBLE PRECISION   ALPHA, BETA
*     ..
*     .. Array Arguments ..
      INTEGER            IDXQ( * ), IWORK( * )
      DOUBLE PRECISION   D( * ), U( LDU, * ), VT( LDVT, * ), WORK( * )
*     ..
*     .. Common block to return operation count ..
      COMMON             / LATIME / OPS, ITCNT
*     ..
*     .. Scalars in Common ..
      DOUBLE PRECISION   ITCNT, OPS
*     ..
*
*  Purpose
*  =======
*
*  DLASD1 computes the SVD of an upper bidiagonal N-by-M matrix B,
*  where N = NL + NR + 1 and M = N + SQRE. DLASD1 is called from DLASD0.
*
*  A related subroutine DLASD7 handles the case in which the singular
*  values (and the singular vectors in factored form) are desired.
*
*  DLASD1 computes the SVD as follows:
*
*                ( D1(in)  0    0     0 )
*    B = U(in) * (   Z1'   a   Z2'    b ) * VT(in)
*                (   0     0   D2(in) 0 )
*
*      = U(out) * ( D(out) 0) * VT(out)
*
*  where Z' = (Z1' a Z2' b) = u' VT', and u is a vector of dimension M
*  with ALPHA and BETA in the NL+1 and NL+2 th entries and zeros
*  elsewhere; and the entry b is empty if SQRE = 0.
*
*  The left singular vectors of the original matrix are stored in U, and
*  the transpose of the right singular vectors are stored in VT, and the
*  singular values are in D.  The algorithm consists of three stages:
*
*     The first stage consists of deflating the size of the problem
*     when there are multiple singular values or when there are zeros in
*     the Z vector.  For each such occurence the dimension of the
*     secular equation problem is reduced by one.  This stage is
*     performed by the routine DLASD2.
*
*     The second stage consists of calculating the updated
*     singular values. This is done by finding the square roots of the
*     roots of the secular equation via the routine DLASD4 (as called
*     by DLASD3). This routine also calculates the singular vectors of
*     the current problem.
*
*     The final stage consists of computing the updated singular vectors
*     directly using the updated singular values.  The singular vectors
*     for the current problem are multiplied with the singular vectors
*     from the overall problem.
*
*  Arguments
*  =========
*
*  NL     (input) INTEGER
*         The row dimension of the upper block.  NL >= 1.
*
*  NR     (input) INTEGER
*         The row dimension of the lower block.  NR >= 1.
*
*  SQRE   (input) INTEGER
*         = 0: the lower block is an NR-by-NR square matrix.
*         = 1: the lower block is an NR-by-(NR+1) rectangular matrix.
*
*         The bidiagonal matrix has row dimension N = NL + NR + 1,
*         and column dimension M = N + SQRE.
*
*  D      (input/output) DOUBLE PRECISION array,
*                        dimension (N = NL+NR+1).
*         On entry D(1:NL,1:NL) contains the singular values of the
*         upper block; and D(NL+2:N) contains the singular values of
*         the lower block. On exit D(1:N) contains the singular values
*         of the modified matrix.
*
*  ALPHA  (input) DOUBLE PRECISION
*         Contains the diagonal element associated with the added row.
*
*  BETA   (input) DOUBLE PRECISION
*         Contains the off-diagonal element associated with the added
*         row.
*
*  U      (input/output) DOUBLE PRECISION array, dimension(LDU,N)
*         On entry U(1:NL, 1:NL) contains the left singular vectors of
*         the upper block; U(NL+2:N, NL+2:N) contains the left singular
*         vectors of the lower block. On exit U contains the left
*         singular vectors of the bidiagonal matrix.
*
*  LDU    (input) INTEGER
*         The leading dimension of the array U.  LDU >= max( 1, N ).
*
*  VT     (input/output) DOUBLE PRECISION array, dimension(LDVT,M)
*         where M = N + SQRE.
*         On entry VT(1:NL+1, 1:NL+1)' contains the right singular
*         vectors of the upper block; VT(NL+2:M, NL+2:M)' contains
*         the right singular vectors of the lower block. On exit
*         VT' contains the right singular vectors of the
*         bidiagonal matrix.
*
*  LDVT   (input) INTEGER
*         The leading dimension of the array VT.  LDVT >= max( 1, M ).
*
*  IDXQ  (output) INTEGER array, dimension(N)
*         This contains the permutation which will reintegrate the
*         subproblem just solved back into sorted order, i.e.
*         D( IDXQ( I = 1, N ) ) will be in ascending order.
*
*  IWORK  (workspace) INTEGER array, dimension( 4 * N )
*
*  WORK   (workspace) DOUBLE PRECISION array, dimension( 3*M**2 + 2*M )
*
*  INFO   (output) INTEGER
*          = 0:  successful exit.
*          < 0:  if INFO = -i, the i-th argument had an illegal value.
*          > 0:  if INFO = 1, an singular value did not converge
*
*  Further Details
*  ===============
*
*  Based on contributions by
*     Ming Gu and Huan Ren, Computer Science Division, University of
*     California at Berkeley, USA
*
*  =====================================================================
*
*     .. Parameters ..
*
      DOUBLE PRECISION   ONE, ZERO
      PARAMETER          ( ONE = 1.0D0, ZERO = 0.0D0 )
*     ..
*     .. Local Scalars ..
      INTEGER            COLTYP, I, IDX, IDXC, IDXP, IQ, ISIGMA, IU2,
     $                   IVT2, IZ, K, LDQ, LDU2, LDVT2, M, N, N1, N2
      DOUBLE PRECISION   ORGNRM
*     ..
*     .. External Subroutines ..
      EXTERNAL           DLAMRG, DLASCL, DLASD2, DLASD3, XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          DBLE, ABS, MAX
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters.
*
      INFO = 0
*
      IF( NL.LT.1 ) THEN
         INFO = -1
      ELSE IF( NR.LT.1 ) THEN
         INFO = -2
      ELSE IF( ( SQRE.LT.0 ) .OR. ( SQRE.GT.1 ) ) THEN
         INFO = -3
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DLASD1', -INFO )
         RETURN
      END IF
*
      N = NL + NR + 1
      M = N + SQRE
*
*     The following values are for bookkeeping purposes only.  They are
*     integer pointers which indicate the portion of the workspace
*     used by a particular array in DLASD2 and DLASD3.
*
      LDU2 = N
      LDVT2 = M
*
      IZ = 1
      ISIGMA = IZ + M
      IU2 = ISIGMA + N
      IVT2 = IU2 + LDU2*N
      IQ = IVT2 + LDVT2*M
*
      IDX = 1
      IDXC = IDX + N
      COLTYP = IDXC + N
      IDXP = COLTYP + N
*
*     Scale.
*
      ORGNRM = MAX( ABS( ALPHA ), ABS( BETA ) )
      D( NL+1 ) = ZERO
      DO 10 I = 1, N
         IF( ABS( D( I ) ).GT.ORGNRM ) THEN
            ORGNRM = ABS( D( I ) )
         END IF
   10 CONTINUE
      OPS = OPS + DBLE( N + 2 )
      CALL DLASCL( 'G', 0, 0, ORGNRM, ONE, N, 1, D, N, INFO )
      ALPHA = ALPHA / ORGNRM
      BETA = BETA / ORGNRM
*
*     Deflate singular values.
*
      CALL DLASD2( NL, NR, SQRE, K, D, WORK( IZ ), ALPHA, BETA, U, LDU,
     $             VT, LDVT, WORK( ISIGMA ), WORK( IU2 ), LDU2,
     $             WORK( IVT2 ), LDVT2, IWORK( IDXP ), IWORK( IDX ),
     $             IWORK( IDXC ), IDXQ, IWORK( COLTYP ), INFO )
*
*     Solve Secular Equation and update singular vectors.
*
      LDQ = K
      CALL DLASD3( NL, NR, SQRE, K, D, WORK( IQ ), LDQ, WORK( ISIGMA ),
     $             U, LDU, WORK( IU2 ), LDU2, VT, LDVT, WORK( IVT2 ),
     $             LDVT2, IWORK( IDXC ), IWORK( COLTYP ), WORK( IZ ),
     $             INFO )
      IF( INFO.NE.0 ) THEN
         RETURN
      END IF
*
*     Unscale.
*
      OPS = OPS + DBLE( N )
      CALL DLASCL( 'G', 0, 0, ONE, ORGNRM, N, 1, D, N, INFO )
*
*     Prepare the IDXQ sorting permutation.
*
      N1 = K
      N2 = N - K
      CALL DLAMRG( N1, N2, D, 1, -1, IDXQ )
*
      RETURN
*
*     End of DLASD1
*
      END
      SUBROUTINE DLASD2( NL, NR, SQRE, K, D, Z, ALPHA, BETA, U, LDU, VT,
     $                   LDVT, DSIGMA, U2, LDU2, VT2, LDVT2, IDXP, IDX,
     $                   IDXC, IDXQ, COLTYP, INFO )
*
*  -- LAPACK auxiliary routine (instrumented to count ops, version 3.0) --
*     Univ. of Tennessee, Oak Ridge National Lab, Argonne National Lab,
*     Courant Institute, NAG Ltd., and Rice University
*     October 31, 1999
*
*     .. Scalar Arguments ..
      INTEGER            INFO, K, LDU, LDU2, LDVT, LDVT2, NL, NR, SQRE
      DOUBLE PRECISION   ALPHA, BETA
*     ..
*     .. Array Arguments ..
      INTEGER            COLTYP( * ), IDX( * ), IDXC( * ), IDXP( * ),
     $                   IDXQ( * )
      DOUBLE PRECISION   D( * ), DSIGMA( * ), U( LDU, * ),
     $                   U2( LDU2, * ), VT( LDVT, * ), VT2( LDVT2, * ),
     $                   Z( * )
*     ..
*     .. Common block to return operation count ..
      COMMON             / LATIME / OPS, ITCNT
*     ..
*     .. Scalars in Common ..
      DOUBLE PRECISION   ITCNT, OPS
*     ..
*
*  Purpose
*  =======
*
*  DLASD2 merges the two sets of singular values together into a single
*  sorted set.  Then it tries to deflate the size of the problem.
*  There are two ways in which deflation can occur:  when two or more
*  singular values are close together or if there is a tiny entry in the
*  Z vector.  For each such occurrence the order of the related secular
*  equation problem is reduced by one.
*
*  DLASD2 is called from DLASD1.
*
*  Arguments
*  =========
*
*  NL     (input) INTEGER
*         The row dimension of the upper block.  NL >= 1.
*
*  NR     (input) INTEGER
*         The row dimension of the lower block.  NR >= 1.
*
*  SQRE   (input) INTEGER
*         = 0: the lower block is an NR-by-NR square matrix.
*         = 1: the lower block is an NR-by-(NR+1) rectangular matrix.
*
*         The bidiagonal matrix has N = NL + NR + 1 rows and
*         M = N + SQRE >= N columns.
*
*  K      (output) INTEGER
*         Contains the dimension of the non-deflated matrix,
*         This is the order of the related secular equation. 1 <= K <=N.
*
*  D      (input/output) DOUBLE PRECISION array, dimension(N)
*         On entry D contains the singular values of the two submatrices
*         to be combined.  On exit D contains the trailing (N-K) updated
*         singular values (those which were deflated) sorted into
*         increasing order.
*
*  ALPHA  (input) DOUBLE PRECISION
*         Contains the diagonal element associated with the added row.
*
*  BETA   (input) DOUBLE PRECISION
*         Contains the off-diagonal element associated with the added
*         row.
*
*  U      (input/output) DOUBLE PRECISION array, dimension(LDU,N)
*         On entry U contains the left singular vectors of two
*         submatrices in the two square blocks with corners at (1,1),
*         (NL, NL), and (NL+2, NL+2), (N,N).
*         On exit U contains the trailing (N-K) updated left singular
*         vectors (those which were deflated) in its last N-K columns.
*
*  LDU    (input) INTEGER
*         The leading dimension of the array U.  LDU >= N.
*
*  Z      (output) DOUBLE PRECISION array, dimension(N)
*         On exit Z contains the updating row vector in the secular
*         equation.
*
*  DSIGMA (output) DOUBLE PRECISION array, dimension (N)
*         Contains a copy of the diagonal elements (K-1 singular values
*         and one zero) in the secular equation.
*
*  U2     (output) DOUBLE PRECISION array, dimension(LDU2,N)
*         Contains a copy of the first K-1 left singular vectors which
*         will be used by DLASD3 in a matrix multiply (DGEMM) to solve
*         for the new left singular vectors. U2 is arranged into four
*         blocks. The first block contains a column with 1 at NL+1 and
*         zero everywhere else; the second block contains non-zero
*         entries only at and above NL; the third contains non-zero
*         entries only below NL+1; and the fourth is dense.
*
*  LDU2   (input) INTEGER
*         The leading dimension of the array U2.  LDU2 >= N.
*
*  VT     (input/output) DOUBLE PRECISION array, dimension(LDVT,M)
*         On entry VT' contains the right singular vectors of two
*         submatrices in the two square blocks with corners at (1,1),
*         (NL+1, NL+1), and (NL+2, NL+2), (M,M).
*         On exit VT' contains the trailing (N-K) updated right singular
*         vectors (those which were deflated) in its last N-K columns.
*         In case SQRE =1, the last row of VT spans the right null
*         space.
*
*  LDVT   (input) INTEGER
*         The leading dimension of the array VT.  LDVT >= M.
*
*  VT2    (output) DOUBLE PRECISION array, dimension(LDVT2,N)
*         VT2' contains a copy of the first K right singular vectors
*         which will be used by DLASD3 in a matrix multiply (DGEMM) to
*         solve for the new right singular vectors. VT2 is arranged into
*         three blocks. The first block contains a row that corresponds
*         to the special 0 diagonal element in SIGMA; the second block
*         contains non-zeros only at and before NL +1; the third block
*         contains non-zeros only at and after  NL +2.
*
*  LDVT2  (input) INTEGER
*         The leading dimension of the array VT2.  LDVT2 >= M.
*
*  IDXP   (workspace) INTEGER array, dimension(N)
*         This will contain the permutation used to place deflated
*         values of D at the end of the array. On output IDXP(2:K)
*         points to the nondeflated D-values and IDXP(K+1:N)
*         points to the deflated singular values.
*
*  IDX    (workspace) INTEGER array, dimension(N)
*         This will contain the permutation used to sort the contents of
*         D into ascending order.
*
*  IDXC   (output) INTEGER array, dimension(N)
*         This will contain the permutation used to arrange the columns
*         of the deflated U matrix into three groups:  the first group
*         contains non-zero entries only at and above NL, the second
*         contains non-zero entries only below NL+2, and the third is
*         dense.
*
*  COLTYP (workspace/output) INTEGER array, dimension(N)
*         As workspace, this will contain a label which will indicate
*         which of the following types a column in the U2 matrix or a
*         row in the VT2 matrix is:
*         1 : non-zero in the upper half only
*         2 : non-zero in the lower half only
*         3 : dense
*         4 : deflated
*
*         On exit, it is an array of dimension 4, with COLTYP(I) being
*         the dimension of the I-th type columns.
*
*  IDXQ   (input) INTEGER array, dimension(N)
*         This contains the permutation which separately sorts the two
*         sub-problems in D into ascending order.  Note that entries in
*         the first hlaf of this permutation must first be moved one
*         position backward; and entries in the second half
*         must first have NL+1 added to their values.
*
*  INFO   (output) INTEGER
*          = 0:  successful exit.
*          < 0:  if INFO = -i, the i-th argument had an illegal value.
*
*  Further Details
*  ===============
*
*  Based on contributions by
*     Ming Gu and Huan Ren, Computer Science Division, University of
*     California at Berkeley, USA
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE, TWO, EIGHT
      PARAMETER          ( ZERO = 0.0D0, ONE = 1.0D0, TWO = 2.0D0,
     $                   EIGHT = 8.0D0 )
*     ..
*     .. Local Arrays ..
      INTEGER            CTOT( 4 ), PSM( 4 )
*     ..
*     .. Local Scalars ..
      INTEGER            CT, I, IDXI, IDXJ, IDXJP, J, JP, JPREV, K2, M,
     $                   N, NLP1, NLP2
      DOUBLE PRECISION   C, EPS, HLFTOL, S, TAU, TOL, Z1
*     ..
*     .. External Functions ..
      DOUBLE PRECISION   DLAMCH, DLAPY2
      EXTERNAL           DLAMCH, DLAPY2
*     ..
*     .. External Subroutines ..
      EXTERNAL           DCOPY, DLACPY, DLAMRG, DLASET, DROT, XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          DBLE, ABS, MAX
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters.
*
      INFO = 0
*
      IF( NL.LT.1 ) THEN
         INFO = -1
      ELSE IF( NR.LT.1 ) THEN
         INFO = -2
      ELSE IF( ( SQRE.NE.1 ) .AND. ( SQRE.NE.0 ) ) THEN
         INFO = -3
      END IF
*
      N = NL + NR + 1
      M = N + SQRE
*
      IF( LDU.LT.N ) THEN
         INFO = -10
      ELSE IF( LDVT.LT.M ) THEN
         INFO = -12
      ELSE IF( LDU2.LT.N ) THEN
         INFO = -15
      ELSE IF( LDVT2.LT.M ) THEN
         INFO = -17
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DLASD2', -INFO )
         RETURN
      END IF
*
      NLP1 = NL + 1
      NLP2 = NL + 2
*
*     Generate the first part of the vector Z; and move the singular
*     values in the first part of D one position backward.
*
      OPS = OPS + DBLE( 1 + NL )
      Z1 = ALPHA*VT( NLP1, NLP1 )
      Z( 1 ) = Z1
      DO 10 I = NL, 1, -1
         Z( I+1 ) = ALPHA*VT( I, NLP1 )
         D( I+1 ) = D( I )
         IDXQ( I+1 ) = IDXQ( I ) + 1
   10 CONTINUE
*
*     Generate the second part of the vector Z.
*
      OPS = OPS + DBLE( M-NLP2+1 )
      DO 20 I = NLP2, M
         Z( I ) = BETA*VT( I, NLP2 )
   20 CONTINUE
*
*     Initialize some reference arrays.
*
      DO 30 I = 2, NLP1
         COLTYP( I ) = 1
   30 CONTINUE
      DO 40 I = NLP2, N
         COLTYP( I ) = 2
   40 CONTINUE
*
*     Sort the singular values into increasing order
*
      DO 50 I = NLP2, N
         IDXQ( I ) = IDXQ( I ) + NLP1
   50 CONTINUE
*
*     DSIGMA, IDXC, IDXC, and the first column of U2
*     are used as storage space.
*
      DO 60 I = 2, N
         DSIGMA( I ) = D( IDXQ( I ) )
         U2( I, 1 ) = Z( IDXQ( I ) )
         IDXC( I ) = COLTYP( IDXQ( I ) )
   60 CONTINUE
*
      CALL DLAMRG( NL, NR, DSIGMA( 2 ), 1, 1, IDX( 2 ) )
*
      DO 70 I = 2, N
         IDXI = 1 + IDX( I )
         D( I ) = DSIGMA( IDXI )
         Z( I ) = U2( IDXI, 1 )
         COLTYP( I ) = IDXC( IDXI )
   70 CONTINUE
*
*     Calculate the allowable deflation tolerance
*
      OPS = OPS + DBLE( 2 )
      EPS = DLAMCH( 'Epsilon' )
      TOL = MAX( ABS( ALPHA ), ABS( BETA ) )
      TOL = EIGHT*EPS*MAX( ABS( D( N ) ), TOL )
*
*     There are 2 kinds of deflation -- first a value in the z-vector
*     is small, second two (or more) singular values are very close
*     together (their difference is small).
*
*     If the value in the z-vector is small, we simply permute the
*     array so that the corresponding singular value is moved to the
*     end.
*
*     If two values in the D-vector are close, we perform a two-sided
*     rotation designed to make one of the corresponding z-vector
*     entries zero, and then permute the array so that the deflated
*     singular value is moved to the end.
*
*     If there are multiple singular values then the problem deflates.
*     Here the number of equal singular values are found.  As each equal
*     singular value is found, an elementary reflector is computed to
*     rotate the corresponding singular subspace so that the
*     corresponding components of Z are zero in this new basis.
*
      K = 1
      K2 = N + 1
      DO 80 J = 2, N
         IF( ABS( Z( J ) ).LE.TOL ) THEN
*
*           Deflate due to small z component.
*
            K2 = K2 - 1
            IDXP( K2 ) = J
            COLTYP( J ) = 4
            IF( J.EQ.N )
     $         GO TO 120
         ELSE
            JPREV = J
            GO TO 90
         END IF
   80 CONTINUE
   90 CONTINUE
      J = JPREV
  100 CONTINUE
      J = J + 1
      IF( J.GT.N )
     $   GO TO 110
      IF( ABS( Z( J ) ).LE.TOL ) THEN
*
*        Deflate due to small z component.
*
         K2 = K2 - 1
         IDXP( K2 ) = J
         COLTYP( J ) = 4
      ELSE
*
*        Check if singular values are close enough to allow deflation.
*
         OPS = OPS + DBLE( 1 )
         IF( ABS( D( J )-D( JPREV ) ).LE.TOL ) THEN
*
*           Deflation is possible.
*
            S = Z( JPREV )
            C = Z( J )
*
*           Find sqrt(a**2+b**2) without overflow or
*           destructive underflow.
*
            OPS = OPS + DBLE( 7 )
            TAU = DLAPY2( C, S )
            C = C / TAU
            S = -S / TAU
            Z( J ) = TAU
            Z( JPREV ) = ZERO
*
*           Apply back the Givens rotation to the left and right
*           singular vector matrices.
*
            IDXJP = IDXQ( IDX( JPREV )+1 )
            IDXJ = IDXQ( IDX( J )+1 )
            IF( IDXJP.LE.NLP1 ) THEN
               IDXJP = IDXJP - 1
            END IF
            IF( IDXJ.LE.NLP1 ) THEN
               IDXJ = IDXJ - 1
            END IF
            OPS = OPS + DBLE( 12 )
            CALL DROT( N, U( 1, IDXJP ), 1, U( 1, IDXJ ), 1, C, S )
            CALL DROT( M, VT( IDXJP, 1 ), LDVT, VT( IDXJ, 1 ), LDVT, C,
     $                 S )
            IF( COLTYP( J ).NE.COLTYP( JPREV ) ) THEN
               COLTYP( J ) = 3
            END IF
            COLTYP( JPREV ) = 4
            K2 = K2 - 1
            IDXP( K2 ) = JPREV
            JPREV = J
         ELSE
            K = K + 1
            U2( K, 1 ) = Z( JPREV )
            DSIGMA( K ) = D( JPREV )
            IDXP( K ) = JPREV
            JPREV = J
         END IF
      END IF
      GO TO 100
  110 CONTINUE
*
*     Record the last singular value.
*
      K = K + 1
      U2( K, 1 ) = Z( JPREV )
      DSIGMA( K ) = D( JPREV )
      IDXP( K ) = JPREV
*
  120 CONTINUE
*
*     Count up the total number of the various types of columns, then
*     form a permutation which positions the four column types into
*     four groups of uniform structure (although one or more of these
*     groups may be empty).
*
      DO 130 J = 1, 4
         CTOT( J ) = 0
  130 CONTINUE
      DO 140 J = 2, N
         CT = COLTYP( J )
         CTOT( CT ) = CTOT( CT ) + 1
  140 CONTINUE
*
*     PSM(*) = Position in SubMatrix (of types 1 through 4)
*
      PSM( 1 ) = 2
      PSM( 2 ) = 2 + CTOT( 1 )
      PSM( 3 ) = PSM( 2 ) + CTOT( 2 )
      PSM( 4 ) = PSM( 3 ) + CTOT( 3 )
*
*     Fill out the IDXC array so that the permutation which it induces
*     will place all type-1 columns first, all type-2 columns next,
*     then all type-3's, and finally all type-4's, starting from the
*     second column. This applies similarly to the rows of VT.
*
      DO 150 J = 2, N
         JP = IDXP( J )
         CT = COLTYP( JP )
         IDXC( PSM( CT ) ) = J
         PSM( CT ) = PSM( CT ) + 1
  150 CONTINUE
*
*     Sort the singular values and corresponding singular vectors into
*     DSIGMA, U2, and VT2 respectively.  The singular values/vectors
*     which were not deflated go into the first K slots of DSIGMA, U2,
*     and VT2 respectively, while those which were deflated go into the
*     last N - K slots, except that the first column/row will be treated
*     separately.
*
      DO 160 J = 2, N
         JP = IDXP( J )
         DSIGMA( J ) = D( JP )
         IDXJ = IDXQ( IDX( IDXP( IDXC( J ) ) )+1 )
         IF( IDXJ.LE.NLP1 ) THEN
            IDXJ = IDXJ - 1
         END IF
         CALL DCOPY( N, U( 1, IDXJ ), 1, U2( 1, J ), 1 )
         CALL DCOPY( M, VT( IDXJ, 1 ), LDVT, VT2( J, 1 ), LDVT2 )
  160 CONTINUE
*
*     Determine DSIGMA(1), DSIGMA(2) and Z(1)
*
      OPS = OPS + DBLE( 1 )
      DSIGMA( 1 ) = ZERO
      HLFTOL = TOL / TWO
      IF( ABS( DSIGMA( 2 ) ).LE.HLFTOL )
     $   DSIGMA( 2 ) = HLFTOL
      IF( M.GT.N ) THEN
         OPS = OPS + DBLE( 5 )
         Z( 1 ) = DLAPY2( Z1, Z( M ) )
         IF( Z( 1 ).LE.TOL ) THEN
            C = ONE
            S = ZERO
            Z( 1 ) = TOL
         ELSE
            OPS = OPS + DBLE( 2 )
            C = Z1 / Z( 1 )
            S = Z( M ) / Z( 1 )
         END IF
      ELSE
         IF( ABS( Z1 ).LE.TOL ) THEN
            Z( 1 ) = TOL
         ELSE
            Z( 1 ) = Z1
         END IF
      END IF
*
*     Move the rest of the updating row to Z.
*
      CALL DCOPY( K-1, U2( 2, 1 ), 1, Z( 2 ), 1 )
*
*     Determine the first column of U2, the first row of VT2 and the
*     last row of VT.
*
      CALL DLASET( 'A', N, 1, ZERO, ZERO, U2, LDU2 )
      U2( NLP1, 1 ) = ONE
      IF( M.GT.N ) THEN
         OPS = OPS + DBLE( NLP1*2 )
         DO 170 I = 1, NLP1
            VT( M, I ) = -S*VT( NLP1, I )
            VT2( 1, I ) = C*VT( NLP1, I )
  170    CONTINUE
         OPS = OPS + DBLE( (M-NLP2+1)*2 )
         DO 180 I = NLP2, M
            VT2( 1, I ) = S*VT( M, I )
            VT( M, I ) = C*VT( M, I )
  180    CONTINUE
      ELSE
         CALL DCOPY( M, VT( NLP1, 1 ), LDVT, VT2( 1, 1 ), LDVT2 )
      END IF
      IF( M.GT.N ) THEN
         CALL DCOPY( M, VT( M, 1 ), LDVT, VT2( M, 1 ), LDVT2 )
      END IF
*
*     The deflated singular values and their corresponding vectors go
*     into the back of D, U, and V respectively.
*
      IF( N.GT.K ) THEN
         CALL DCOPY( N-K, DSIGMA( K+1 ), 1, D( K+1 ), 1 )
         CALL DLACPY( 'A', N, N-K, U2( 1, K+1 ), LDU2, U( 1, K+1 ),
     $                LDU )
         CALL DLACPY( 'A', N-K, M, VT2( K+1, 1 ), LDVT2, VT( K+1, 1 ),
     $                LDVT )
      END IF
*
*     Copy CTOT into COLTYP for referencing in DLASD3.
*
      DO 190 J = 1, 4
         COLTYP( J ) = CTOT( J )
  190 CONTINUE
*
      RETURN
*
*     End of DLASD2
*
      END
      SUBROUTINE DLASD3( NL, NR, SQRE, K, D, Q, LDQ, DSIGMA, U, LDU, U2,
     $                   LDU2, VT, LDVT, VT2, LDVT2, IDXC, CTOT, Z,
     $                   INFO )
*
*  -- LAPACK auxiliary routine (instrumented to count ops, version 3.0) --
*     Univ. of Tennessee, Oak Ridge National Lab, Argonne National Lab,
*     Courant Institute, NAG Ltd., and Rice University
*     October 31, 1999
*
*     .. Scalar Arguments ..
      INTEGER            INFO, K, LDQ, LDU, LDU2, LDVT, LDVT2, NL, NR,
     $                   SQRE
*     ..
*     .. Array Arguments ..
      INTEGER            CTOT( * ), IDXC( * )
      DOUBLE PRECISION   D( * ), DSIGMA( * ), Q( LDQ, * ), U( LDU, * ),
     $                   U2( LDU2, * ), VT( LDVT, * ), VT2( LDVT2, * ),
     $                   Z( * )
*     ..
*     .. Common block to return operation count ..
      COMMON             / LATIME / OPS, ITCNT
*     ..
*     .. Scalars in Common ..
      DOUBLE PRECISION   ITCNT, OPS
*     ..
*
*  Purpose
*  =======
*
*  DLASD3 finds all the square roots of the roots of the secular
*  equation, as defined by the values in D and Z.  It makes the
*  appropriate calls to DLASD4 and then updates the singular
*  vectors by matrix multiplication.
*
*  This code makes very mild assumptions about floating point
*  arithmetic. It will work on machines with a guard digit in
*  add/subtract, or on those binary machines without guard digits
*  which subtract like the Cray XMP, Cray YMP, Cray C 90, or Cray 2.
*  It could conceivably fail on hexadecimal or decimal machines
*  without guard digits, but we know of none.
*
*  DLASD3 is called from DLASD1.
*
*  Arguments
*  =========
*
*  NL     (input) INTEGER
*         The row dimension of the upper block.  NL >= 1.
*
*  NR     (input) INTEGER
*         The row dimension of the lower block.  NR >= 1.
*
*  SQRE   (input) INTEGER
*         = 0: the lower block is an NR-by-NR square matrix.
*         = 1: the lower block is an NR-by-(NR+1) rectangular matrix.
*
*         The bidiagonal matrix has N = NL + NR + 1 rows and
*         M = N + SQRE >= N columns.
*
*  K      (input) INTEGER
*         The size of the secular equation, 1 =< K = < N.
*
*  D      (output) DOUBLE PRECISION array, dimension(K)
*         On exit the square roots of the roots of the secular equation,
*         in ascending order.
*
*  Q      (workspace) DOUBLE PRECISION array,
*                     dimension at least (LDQ,K).
*
*  LDQ    (input) INTEGER
*         The leading dimension of the array Q.  LDQ >= K.
*
*  DSIGMA (input) DOUBLE PRECISION array, dimension(K)
*         The first K elements of this array contain the old roots
*         of the deflated updating problem.  These are the poles
*         of the secular equation.
*
*  U      (input) DOUBLE PRECISION array, dimension (LDU, N)
*         The last N - K columns of this matrix contain the deflated
*         left singular vectors.
*
*  LDU    (input) INTEGER
*         The leading dimension of the array U.  LDU >= N.
*
*  U2     (input) DOUBLE PRECISION array, dimension (LDU2, N)
*         The first K columns of this matrix contain the non-deflated
*         left singular vectors for the split problem.
*
*  LDU2   (input) INTEGER
*         The leading dimension of the array U2.  LDU2 >= N.
*
*  VT     (input) DOUBLE PRECISION array, dimension (LDVT, M)
*         The last M - K columns of VT' contain the deflated
*         right singular vectors.
*
*  LDVT   (input) INTEGER
*         The leading dimension of the array VT.  LDVT >= N.
*
*  VT2    (input) DOUBLE PRECISION array, dimension (LDVT2, N)
*         The first K columns of VT2' contain the non-deflated
*         right singular vectors for the split problem.
*
*  LDVT2  (input) INTEGER
*         The leading dimension of the array VT2.  LDVT2 >= N.
*
*  IDXC   (input) INTEGER array, dimension ( N )
*         The permutation used to arrange the columns of U (and rows of
*         VT) into three groups:  the first group contains non-zero
*         entries only at and above (or before) NL +1; the second
*         contains non-zero entries only at and below (or after) NL+2;
*         and the third is dense. The first column of U and the row of
*         VT are treated separately, however.
*
*         The rows of the singular vectors found by DLASD4
*         must be likewise permuted before the matrix multiplies can
*         take place.
*
*  CTOT   (input) INTEGER array, dimension ( 4 )
*         A count of the total number of the various types of columns
*         in U (or rows in VT), as described in IDXC. The fourth column
*         type is any column which has been deflated.
*
*  Z      (input) DOUBLE PRECISION array, dimension (K)
*         The first K elements of this array contain the components
*         of the deflation-adjusted updating row vector.
*
*  INFO   (output) INTEGER
*         = 0:  successful exit.
*         < 0:  if INFO = -i, the i-th argument had an illegal value.
*         > 0:  if INFO = 1, an singular value did not converge
*
*  Further Details
*  ===============
*
*  Based on contributions by
*     Ming Gu and Huan Ren, Computer Science Division, University of
*     California at Berkeley, USA
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ONE, ZERO, NEGONE
      PARAMETER          ( ONE = 1.0D0, ZERO = 0.0D0, NEGONE = -1.0D0 )
*     ..
*     .. Local Scalars ..
      INTEGER            CTEMP, I, J, JC, KTEMP, M, N, NLP1, NLP2, NRP1
      DOUBLE PRECISION   RHO, TEMP
*     ..
*     .. External Functions ..
      DOUBLE PRECISION   DLAMC3, DNRM2, DOPBL3
      EXTERNAL           DLAMC3, DNRM2, DOPBL3
*     ..
*     .. External Subroutines ..
      EXTERNAL           DCOPY, DGEMM, DLACPY, DLASCL, DLASD4, XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          DBLE, ABS, MAX, SIGN, SQRT
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters.
*
      INFO = 0
*
      IF( NL.LT.1 ) THEN
         INFO = -1
      ELSE IF( NR.LT.1 ) THEN
         INFO = -2
      ELSE IF( ( SQRE.NE.1 ) .AND. ( SQRE.NE.0 ) ) THEN
         INFO = -3
      END IF
*
      N = NL + NR + 1
      M = N + SQRE
      NLP1 = NL + 1
      NLP2 = NL + 2
*
      IF( ( K.LT.1 ) .OR. ( K.GT.N ) ) THEN
         INFO = -4
      ELSE IF( LDQ.LT.K ) THEN
         INFO = -7
      ELSE IF( LDU.LT.N ) THEN
         INFO = -10
      ELSE IF( LDU2.LT.N ) THEN
         INFO = -12
      ELSE IF( LDVT.LT.M ) THEN
         INFO = -14
      ELSE IF( LDVT2.LT.M ) THEN
         INFO = -16
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DLASD3', -INFO )
         RETURN
      END IF
*
*     Quick return if possible
*
      IF( K.EQ.1 ) THEN
         D( 1 ) = ABS( Z( 1 ) )
         CALL DCOPY( M, VT2( 1, 1 ), LDVT2, VT( 1, 1 ), LDVT )
         IF( Z( 1 ).GT.ZERO ) THEN
            CALL DCOPY( N, U2( 1, 1 ), 1, U( 1, 1 ), 1 )
         ELSE
            DO 10 I = 1, N
               U( I, 1 ) = -U2( I, 1 )
   10       CONTINUE
         END IF
         RETURN
      END IF
*
*     Modify values DSIGMA(i) to make sure all DSIGMA(i)-DSIGMA(j) can
*     be computed with high relative accuracy (barring over/underflow).
*     This is a problem on machines without a guard digit in
*     add/subtract (Cray XMP, Cray YMP, Cray C 90 and Cray 2).
*     The following code replaces DSIGMA(I) by 2*DSIGMA(I)-DSIGMA(I),
*     which on any of these machines zeros out the bottommost
*     bit of DSIGMA(I) if it is 1; this makes the subsequent
*     subtractions DSIGMA(I)-DSIGMA(J) unproblematic when cancellation
*     occurs. On binary machines with a guard digit (almost all
*     machines) it does not change DSIGMA(I) at all. On hexadecimal
*     and decimal machines with a guard digit, it slightly
*     changes the bottommost bits of DSIGMA(I). It does not account
*     for hexadecimal or decimal machines without guard digits
*     (we know of none). We use a subroutine call to compute
*     2*DLAMBDA(I) to prevent optimizing compilers from eliminating
*     this code.
*
      DO 20 I = 1, K
         DSIGMA( I ) = DLAMC3( DSIGMA( I ), DSIGMA( I ) ) - DSIGMA( I )
   20 CONTINUE
*
*     Keep a copy of Z.
*
      CALL DCOPY( K, Z, 1, Q, 1 )
*
*     Normalize Z.
*
      OPS = OPS + DBLE( K*3 + 1)
      RHO = DNRM2( K, Z, 1 )
      CALL DLASCL( 'G', 0, 0, RHO, ONE, K, 1, Z, K, INFO )
      RHO = RHO*RHO
*
*     Find the new singular values.
*
      DO 30 J = 1, K
         CALL DLASD4( K, J, DSIGMA, Z, U( 1, J ), RHO, D( J ),
     $                VT( 1, J ), INFO )
*
*        If the zero finder fails, the computation is terminated.
*
         IF( INFO.NE.0 ) THEN
            RETURN
         END IF
   30 CONTINUE
*
*     Compute updated Z.
*
      OPS = OPS + DBLE( K*2 )
      DO 60 I = 1, K
         Z( I ) = U( I, K )*VT( I, K )
         OPS = OPS + DBLE( (I-1)*6 )
         DO 40 J = 1, I - 1
            Z( I ) = Z( I )*( U( I, J )*VT( I, J ) /
     $               ( DSIGMA( I )-DSIGMA( J ) ) /
     $               ( DSIGMA( I )+DSIGMA( J ) ) )
   40    CONTINUE
         OPS = OPS + DBLE( (K-I)*6 )
         DO 50 J = I, K - 1
            Z( I ) = Z( I )*( U( I, J )*VT( I, J ) /
     $               ( DSIGMA( I )-DSIGMA( J+1 ) ) /
     $               ( DSIGMA( I )+DSIGMA( J+1 ) ) )
   50    CONTINUE
         Z( I ) = SIGN( SQRT( ABS( Z( I ) ) ), Q( I, 1 ) )
   60 CONTINUE
*
*     Compute left singular vectors of the modified diagonal matrix,
*     and store related information for the right singular vectors.
*
      OPS = OPS + DBLE( K*(3+K*2) + MAX(0,(K-1)*4) )
      DO 90 I = 1, K
         VT( 1, I ) = Z( 1 ) / U( 1, I ) / VT( 1, I )
         U( 1, I ) = NEGONE
         DO 70 J = 2, K
            VT( J, I ) = Z( J ) / U( J, I ) / VT( J, I )
            U( J, I ) = DSIGMA( J )*VT( J, I )
   70    CONTINUE
         TEMP = DNRM2( K, U( 1, I ), 1 )
         Q( 1, I ) = U( 1, I ) / TEMP
         DO 80 J = 2, K
            JC = IDXC( J )
            Q( J, I ) = U( JC, I ) / TEMP
   80    CONTINUE
   90 CONTINUE
*
*     Update the left singular vector matrix.
*
      IF( K.EQ.2 ) THEN
         OPS = OPS + DOPBL3( 'DGEMM ', N, K, K ) 
         CALL DGEMM( 'N', 'N', N, K, K, ONE, U2, LDU2, Q, LDQ, ZERO, U,
     $               LDU )
         GO TO 100
      END IF
      IF( CTOT( 1 ).GT.0 ) THEN
         OPS = OPS + DOPBL3( 'DGEMM ', NL, K, CTOT( 1 ) )
         CALL DGEMM( 'N', 'N', NL, K, CTOT( 1 ), ONE, U2( 1, 2 ), LDU2,
     $               Q( 2, 1 ), LDQ, ZERO, U( 1, 1 ), LDU )
         IF( CTOT( 3 ).GT.0 ) THEN
            KTEMP = 2 + CTOT( 1 ) + CTOT( 2 )
            OPS = OPS + DOPBL3( 'DGEMM ', NL, K, CTOT( 3 ) )
            CALL DGEMM( 'N', 'N', NL, K, CTOT( 3 ), ONE, U2( 1, KTEMP ),
     $                  LDU2, Q( KTEMP, 1 ), LDQ, ONE, U( 1, 1 ), LDU )
         END IF
      ELSE IF( CTOT( 3 ).GT.0 ) THEN
         KTEMP = 2 + CTOT( 1 ) + CTOT( 2 )
         OPS = OPS + DOPBL3( 'DGEMM ', NL, K, CTOT( 3 ) )
         CALL DGEMM( 'N', 'N', NL, K, CTOT( 3 ), ONE, U2( 1, KTEMP ),
     $               LDU2, Q( KTEMP, 1 ), LDQ, ZERO, U( 1, 1 ), LDU )
      ELSE
         CALL DLACPY( 'F', NL, K, U2, LDU2, U, LDU )
      END IF
      CALL DCOPY( K, Q( 1, 1 ), LDQ, U( NLP1, 1 ), LDU )
      KTEMP = 2 + CTOT( 1 )
      CTEMP = CTOT( 2 ) + CTOT( 3 )
      OPS = OPS + DOPBL3( 'DGEMM ', NR, K, CTEMP )
      CALL DGEMM( 'N', 'N', NR, K, CTEMP, ONE, U2( NLP2, KTEMP ), LDU2,
     $            Q( KTEMP, 1 ), LDQ, ZERO, U( NLP2, 1 ), LDU )
*
*     Generate the right singular vectors.
*
  100 CONTINUE
      OPS = OPS + DBLE( K*(K*2+1) + MAX(0,K-1) )
      DO 120 I = 1, K
         TEMP = DNRM2( K, VT( 1, I ), 1 )
         Q( I, 1 ) = VT( 1, I ) / TEMP
         DO 110 J = 2, K
            JC = IDXC( J )
            Q( I, J ) = VT( JC, I ) / TEMP
  110    CONTINUE
  120 CONTINUE
*
*     Update the right singular vector matrix.
*
      IF( K.EQ.2 ) THEN
         OPS = OPS + DOPBL3( 'DGEMM ', K, M, K ) 
         CALL DGEMM( 'N', 'N', K, M, K, ONE, Q, LDQ, VT2, LDVT2, ZERO,
     $               VT, LDVT )
         RETURN
      END IF
      KTEMP = 1 + CTOT( 1 )
      OPS = OPS + DOPBL3( 'DGEMM ', K, NLP1, KTEMP )
      CALL DGEMM( 'N', 'N', K, NLP1, KTEMP, ONE, Q( 1, 1 ), LDQ,
     $            VT2( 1, 1 ), LDVT2, ZERO, VT( 1, 1 ), LDVT )
      KTEMP = 2 + CTOT( 1 ) + CTOT( 2 )
      OPS = OPS + DOPBL3( 'DGEMM ', K, NLP1, CTOT( 3 ) )
      IF( KTEMP.LE.LDVT2 )
     $   CALL DGEMM( 'N', 'N', K, NLP1, CTOT( 3 ), ONE, Q( 1, KTEMP ),
     $               LDQ, VT2( KTEMP, 1 ), LDVT2, ONE, VT( 1, 1 ),
     $               LDVT )
*
      KTEMP = CTOT( 1 ) + 1
      NRP1 = NR + SQRE
      IF( KTEMP.GT.1 ) THEN
         DO 130 I = 1, K
            Q( I, KTEMP ) = Q( I, 1 )
  130    CONTINUE
         DO 140 I = NLP2, M
            VT2( KTEMP, I ) = VT2( 1, I )
  140    CONTINUE
      END IF
      CTEMP = 1 + CTOT( 2 ) + CTOT( 3 )
      OPS = OPS + DOPBL3( 'DGEMM ', K, NRP1, CTEMP ) 
      CALL DGEMM( 'N', 'N', K, NRP1, CTEMP, ONE, Q( 1, KTEMP ), LDQ,
     $            VT2( KTEMP, NLP2 ), LDVT2, ZERO, VT( 1, NLP2 ), LDVT )
*
      RETURN
*
*     End of DLASD3
*
      END
      SUBROUTINE DLASD4( N, I, D, Z, DELTA, RHO, SIGMA, WORK, INFO )
*
*  -- LAPACK auxiliary routine (instrumented to count ops, version 3.0) --
*     Univ. of Tennessee, Oak Ridge National Lab, Argonne National Lab,
*     Courant Institute, NAG Ltd., and Rice University
*     October 31, 1999
*
*     .. Scalar Arguments ..
      INTEGER            I, INFO, N
      DOUBLE PRECISION   RHO, SIGMA
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   D( * ), DELTA( * ), WORK( * ), Z( * )
*     ..
*     .. Common block to return operation count ..
      COMMON             / LATIME / OPS, ITCNT
*     ..
*     .. Scalars in Common ..
      DOUBLE PRECISION   ITCNT, OPS
*     ..
*
*  Purpose
*  =======
*
*  This subroutine computes the square root of the I-th updated
*  eigenvalue of a positive symmetric rank-one modification to
*  a positive diagonal matrix whose entries are given as the squares
*  of the corresponding entries in the array d, and that
*
*         0 <= D(i) < D(j)  for  i < j
*
*  and that RHO > 0. This is arranged by the calling routine, and is
*  no loss in generality.  The rank-one modified system is thus
*
*         diag( D ) * diag( D ) +  RHO *  Z * Z_transpose.
*
*  where we assume the Euclidean norm of Z is 1.
*
*  The method consists of approximating the rational functions in the
*  secular equation by simpler interpolating rational functions.
*
*  Arguments
*  =========
*
*  N      (input) INTEGER
*         The length of all arrays.
*
*  I      (input) INTEGER
*         The index of the eigenvalue to be computed.  1 <= I <= N.
*
*  D      (input) DOUBLE PRECISION array, dimension ( N )
*         The original eigenvalues.  It is assumed that they are in
*         order, 0 <= D(I) < D(J)  for I < J.
*
*  Z      (input) DOUBLE PRECISION array, dimension ( N )
*         The components of the updating vector.
*
*  DELTA  (output) DOUBLE PRECISION array, dimension ( N )
*         If N .ne. 1, DELTA contains (D(j) - sigma_I) in its  j-th
*         component.  If N = 1, then DELTA(1) = 1.  The vector DELTA
*         contains the information necessary to construct the
*         (singular) eigenvectors.
*
*  RHO    (input) DOUBLE PRECISION
*         The scalar in the symmetric updating formula.
*
*  SIGMA  (output) DOUBLE PRECISION
*         The computed lambda_I, the I-th updated eigenvalue.
*
*  WORK   (workspace) DOUBLE PRECISION array, dimension ( N )
*         If N .ne. 1, WORK contains (D(j) + sigma_I) in its  j-th
*         component.  If N = 1, then WORK( 1 ) = 1.
*
*  INFO   (output) INTEGER
*         = 0:  successful exit
*         > 0:  if INFO = 1, the updating process failed.
*
*  Internal Parameters
*  ===================
*
*  Logical variable ORGATI (origin-at-i?) is used for distinguishing
*  whether D(i) or D(i+1) is treated as the origin.
*
*            ORGATI = .true.    origin at i
*            ORGATI = .false.   origin at i+1
*
*  Logical variable SWTCH3 (switch-for-3-poles?) is for noting
*  if we are working with THREE poles!
*
*  MAXIT is the maximum number of iterations allowed for each
*  eigenvalue.
*
*  Further Details
*  ===============
*
*  Based on contributions by
*     Ren-Cang Li, Computer Science Division, University of California
*     at Berkeley, USA
*
*  =====================================================================
*
*     .. Parameters ..
      INTEGER            MAXIT
      PARAMETER          ( MAXIT = 20 )
      DOUBLE PRECISION   ZERO, ONE, TWO, THREE, FOUR, EIGHT, TEN
      PARAMETER          ( ZERO = 0.0D0, ONE = 1.0D0, TWO = 2.0D0,
     $                   THREE = 3.0D0, FOUR = 4.0D0, EIGHT = 8.0D0,
     $                   TEN = 10.0D0 )
*     ..
*     .. Local Scalars ..
      LOGICAL            ORGATI, SWTCH, SWTCH3
      INTEGER            II, IIM1, IIP1, IP1, ITER, J, NITER
      DOUBLE PRECISION   A, B, C, DELSQ, DELSQ2, DPHI, DPSI, DTIIM,
     $                   DTIIP, DTIPSQ, DTISQ, DTNSQ, DTNSQ1, DW, EPS,
     $                   ERRETM, ETA, PHI, PREW, PSI, RHOINV, SG2LB,
     $                   SG2UB, TAU, TEMP, TEMP1, TEMP2, W
*     ..
*     .. Local Arrays ..
      DOUBLE PRECISION   DD( 3 ), ZZ( 3 )
*     ..
*     .. External Subroutines ..
      EXTERNAL           DLAED6, DLASD5
*     ..
*     .. External Functions ..
      DOUBLE PRECISION   DLAMCH
      EXTERNAL           DLAMCH
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          DBLE, ABS, MAX, MIN, SQRT
*     ..
*     .. Executable Statements ..
*
*     Since this routine is called in an inner loop, we do no argument
*     checking.
*
*     Quick return for N=1 and 2.
*
      INFO = 0
      IF( N.EQ.1 ) THEN
*
*        Presumably, I=1 upon entry
*
         OPS = OPS + DBLE( 5 )
         SIGMA = SQRT( D( 1 )*D( 1 )+RHO*Z( 1 )*Z( 1 ) )
         DELTA( 1 ) = ONE
         WORK( 1 ) = ONE
         RETURN
      END IF
      IF( N.EQ.2 ) THEN
         CALL DLASD5( I, D, Z, DELTA, RHO, SIGMA, WORK )
         RETURN
      END IF
*
*     Compute machine epsilon
*
      EPS = DLAMCH( 'Epsilon' )
      OPS = OPS + DBLE( 1 )
      RHOINV = ONE / RHO
*
*     The case I = N
*
      IF( I.EQ.N ) THEN
*
*        Initialize some basic variables
*
         II = N - 1
         NITER = 1
*
*        Calculate initial guess
*
         OPS = OPS + DBLE( 1 )
         TEMP = RHO / TWO
*
*        If ||Z||_2 is not one, then TEMP should be set to
*        RHO * ||Z||_2^2 / TWO
*
         OPS = OPS + DBLE( 5 + 4*N )
         TEMP1 = TEMP / ( D( N )+SQRT( D( N )*D( N )+TEMP ) )
         DO 10 J = 1, N
            WORK( J ) = D( J ) + D( N ) + TEMP1
            DELTA( J ) = ( D( J )-D( N ) ) - TEMP1
   10    CONTINUE
*
         PSI = ZERO
         OPS = OPS + DBLE( 4*( N-2 ) )
         DO 20 J = 1, N - 2
            PSI = PSI + Z( J )*Z( J ) / ( DELTA( J )*WORK( J ) )
   20    CONTINUE
*
         OPS = OPS + DBLE( 9 )
         C = RHOINV + PSI
         W = C + Z( II )*Z( II ) / ( DELTA( II )*WORK( II ) ) +
     $       Z( N )*Z( N ) / ( DELTA( N )*WORK( N ) )
*
         IF( W.LE.ZERO ) THEN
            OPS = OPS + DBLE( 14 )
            TEMP1 = SQRT( D( N )*D( N )+RHO )
            TEMP = Z( N-1 )*Z( N-1 ) / ( ( D( N-1 )+TEMP1 )*
     $             ( D( N )-D( N-1 )+RHO / ( D( N )+TEMP1 ) ) ) +
     $             Z( N )*Z( N ) / RHO
*
*           The following TAU is to approximate
*           SIGMA_n^2 - D( N )*D( N )
*
            IF( C.LE.TEMP ) THEN
               TAU = RHO
            ELSE
               OPS = OPS + DBLE( 10 )
               DELSQ = ( D( N )-D( N-1 ) )*( D( N )+D( N-1 ) )
               A = -C*DELSQ + Z( N-1 )*Z( N-1 ) + Z( N )*Z( N )
               B = Z( N )*Z( N )*DELSQ
               IF( A.LT.ZERO ) THEN
                  OPS = OPS + DBLE( 8 )
                  TAU = TWO*B / ( SQRT( A*A+FOUR*B*C )-A )
               ELSE
                  OPS = OPS + DBLE( 8 )
                  TAU = ( A+SQRT( A*A+FOUR*B*C ) ) / ( TWO*C )
               END IF
            END IF
*
*           It can be proved that
*               D(N)^2+RHO/2 <= SIGMA_n^2 < D(N)^2+TAU <= D(N)^2+RHO
*
         ELSE
            OPS = OPS + DBLE( 10 )
            DELSQ = ( D( N )-D( N-1 ) )*( D( N )+D( N-1 ) )
            A = -C*DELSQ + Z( N-1 )*Z( N-1 ) + Z( N )*Z( N )
            B = Z( N )*Z( N )*DELSQ
*
*           The following TAU is to approximate
*           SIGMA_n^2 - D( N )*D( N )
*
            IF( A.LT.ZERO ) THEN
               OPS = OPS + DBLE( 8 )
               TAU = TWO*B / ( SQRT( A*A+FOUR*B*C )-A )
            ELSE
               OPS = OPS + DBLE( 8 )
               TAU = ( A+SQRT( A*A+FOUR*B*C ) ) / ( TWO*C )
            END IF
*
*           It can be proved that
*           D(N)^2 < D(N)^2+TAU < SIGMA(N)^2 < D(N)^2+RHO/2
*
         END IF
*
*        The following ETA is to approximate SIGMA_n - D( N )
*
         OPS = OPS + DBLE( 5 )
         ETA = TAU / ( D( N )+SQRT( D( N )*D( N )+TAU ) )
*
         OPS = OPS + DBLE( 1 + 4*N )
         SIGMA = D( N ) + ETA
         DO 30 J = 1, N
            DELTA( J ) = ( D( J )-D( I ) ) - ETA
            WORK( J ) = D( J ) + D( I ) + ETA
   30    CONTINUE
*
*        Evaluate PSI and the derivative DPSI
*
         DPSI = ZERO
         PSI = ZERO
         ERRETM = ZERO
         OPS = OPS + DBLE( II*7 )
         DO 40 J = 1, II
            TEMP = Z( J ) / ( DELTA( J )*WORK( J ) )
            PSI = PSI + Z( J )*TEMP
            DPSI = DPSI + TEMP*TEMP
            ERRETM = ERRETM + PSI
   40    CONTINUE
         ERRETM = ABS( ERRETM )
*
*        Evaluate PHI and the derivative DPHI
*
         OPS = OPS + DBLE( 14 )
         TEMP = Z( N ) / ( DELTA( N )*WORK( N ) )
         PHI = Z( N )*TEMP
         DPHI = TEMP*TEMP
         ERRETM = EIGHT*( -PHI-PSI ) + ERRETM - PHI + RHOINV +
     $            ABS( TAU )*( DPSI+DPHI )
*
         W = RHOINV + PHI + PSI
*
*        Test for convergence
*
         IF( ABS( W ).LE.EPS*ERRETM ) THEN
            GO TO 240
         END IF
*
*        Calculate the new step
*
         NITER = NITER + 1
         OPS = OPS + DBLE( 14 )
         DTNSQ1 = WORK( N-1 )*DELTA( N-1 )
         DTNSQ = WORK( N )*DELTA( N )
         C = W - DTNSQ1*DPSI - DTNSQ*DPHI
         A = ( DTNSQ+DTNSQ1 )*W - DTNSQ*DTNSQ1*( DPSI+DPHI )
         B = DTNSQ*DTNSQ1*W
         IF( C.LT.ZERO )
     $      C = ABS( C )
         IF( C.EQ.ZERO ) THEN
            OPS = OPS + DBLE( 2 )
            ETA = RHO - SIGMA*SIGMA
         ELSE IF( A.GE.ZERO ) THEN
            OPS = OPS + DBLE( 8 )
            ETA = ( A+SQRT( ABS( A*A-FOUR*B*C ) ) ) / ( TWO*C )
         ELSE
            OPS = OPS + DBLE( 8 )
            ETA = TWO*B / ( A-SQRT( ABS( A*A-FOUR*B*C ) ) )
         END IF
*
*        Note, eta should be positive if w is negative, and
*        eta should be negative otherwise. However,
*        if for some reason caused by roundoff, eta*w > 0,
*        we simply use one Newton step instead. This way
*        will guarantee eta*w < 0.
*
         OPS = OPS + DBLE( 1 )
         IF( W*ETA.GT.ZERO ) THEN
            OPS = OPS + DBLE( 2 )
            ETA = -W / ( DPSI+DPHI )
         END IF
         TEMP = ETA - DTNSQ
         IF( TEMP.GT.RHO ) THEN
            OPS = OPS + DBLE( 1 )
            ETA = RHO + DTNSQ
         END IF
*
         OPS = OPS + DBLE( 6 + 2*N + 1 )
         TAU = TAU + ETA
         ETA = ETA / ( SIGMA+SQRT( ETA+SIGMA*SIGMA ) )
         DO 50 J = 1, N
            DELTA( J ) = DELTA( J ) - ETA
            WORK( J ) = WORK( J ) + ETA
   50    CONTINUE
*
         SIGMA = SIGMA + ETA
*
*        Evaluate PSI and the derivative DPSI
*
         DPSI = ZERO
         PSI = ZERO
         ERRETM = ZERO
         OPS = OPS + DBLE( 7*II )
         DO 60 J = 1, II
            TEMP = Z( J ) / ( WORK( J )*DELTA( J ) )
            PSI = PSI + Z( J )*TEMP
            DPSI = DPSI + TEMP*TEMP
            ERRETM = ERRETM + PSI
   60    CONTINUE
         ERRETM = ABS( ERRETM )
*
*        Evaluate PHI and the derivative DPHI
*
         OPS = OPS + DBLE( 14 )
         TEMP = Z( N ) / ( WORK( N )*DELTA( N ) )
         PHI = Z( N )*TEMP
         DPHI = TEMP*TEMP
         ERRETM = EIGHT*( -PHI-PSI ) + ERRETM - PHI + RHOINV +
     $            ABS( TAU )*( DPSI+DPHI )
*
         W = RHOINV + PHI + PSI
*
*        Main loop to update the values of the array   DELTA
*
         ITER = NITER + 1
*
         DO 90 NITER = ITER, MAXIT
*
*           Test for convergence
*
            OPS = OPS + DBLE( 1 )
            IF( ABS( W ).LE.EPS*ERRETM ) THEN
               GO TO 240
            END IF
*
*           Calculate the new step
*
            OPS = OPS + DBLE( 22 )
            DTNSQ1 = WORK( N-1 )*DELTA( N-1 )
            DTNSQ = WORK( N )*DELTA( N )
            C = W - DTNSQ1*DPSI - DTNSQ*DPHI
            A = ( DTNSQ+DTNSQ1 )*W - DTNSQ1*DTNSQ*( DPSI+DPHI )
            B = DTNSQ1*DTNSQ*W
            IF( A.GE.ZERO ) THEN
               ETA = ( A+SQRT( ABS( A*A-FOUR*B*C ) ) ) / ( TWO*C )
            ELSE
               ETA = TWO*B / ( A-SQRT( ABS( A*A-FOUR*B*C ) ) )
            END IF
*
*           Note, eta should be positive if w is negative, and
*           eta should be negative otherwise. However,
*           if for some reason caused by roundoff, eta*w > 0,
*           we simply use one Newton step instead. This way
*           will guarantee eta*w < 0.
*
            OPS = OPS + DBLE( 2 )
            IF( W*ETA.GT.ZERO ) THEN
               OPS = OPS + DBLE( 2 )
               ETA = -W / ( DPSI+DPHI )
            END IF
            TEMP = ETA - DTNSQ
            IF( TEMP.LE.ZERO ) THEN
               OPS = OPS + DBLE( 1 )
               ETA = ETA / TWO
            END IF
*
            OPS = OPS + DBLE( 6 + 2*N + 1 )
            TAU = TAU + ETA
            ETA = ETA / ( SIGMA+SQRT( ETA+SIGMA*SIGMA ) )
            DO 70 J = 1, N
               DELTA( J ) = DELTA( J ) - ETA
               WORK( J ) = WORK( J ) + ETA
   70       CONTINUE
*
            SIGMA = SIGMA + ETA
*
*           Evaluate PSI and the derivative DPSI
*
            DPSI = ZERO
            PSI = ZERO
            ERRETM = ZERO
            OPS = OPS + DBLE( 7*II )
            DO 80 J = 1, II
               TEMP = Z( J ) / ( WORK( J )*DELTA( J ) )
               PSI = PSI + Z( J )*TEMP
               DPSI = DPSI + TEMP*TEMP
               ERRETM = ERRETM + PSI
   80       CONTINUE
            ERRETM = ABS( ERRETM )
*
*           Evaluate PHI and the derivative DPHI
*
            OPS = OPS + DBLE( 14 )
            TEMP = Z( N ) / ( WORK( N )*DELTA( N ) )
            PHI = Z( N )*TEMP
            DPHI = TEMP*TEMP
            ERRETM = EIGHT*( -PHI-PSI ) + ERRETM - PHI + RHOINV +
     $               ABS( TAU )*( DPSI+DPHI )
*
            W = RHOINV + PHI + PSI
   90    CONTINUE
*
*        Return with INFO = 1, NITER = MAXIT and not converged
*
         INFO = 1
         GO TO 240
*
*        End for the case I = N
*
      ELSE
*
*        The case for I < N
*
         NITER = 1
         IP1 = I + 1
*
*        Calculate initial guess
*
         OPS = OPS + DBLE( 9 + 4*N )
         DELSQ = ( D( IP1 )-D( I ) )*( D( IP1 )+D( I ) )
         DELSQ2 = DELSQ / TWO
         TEMP = DELSQ2 / ( D( I )+SQRT( D( I )*D( I )+DELSQ2 ) )
         DO 100 J = 1, N
            WORK( J ) = D( J ) + D( I ) + TEMP
            DELTA( J ) = ( D( J )-D( I ) ) - TEMP
  100    CONTINUE
*
         PSI = ZERO
         OPS = OPS + DBLE( 4*( I-1 ) )
         DO 110 J = 1, I - 1
            PSI = PSI + Z( J )*Z( J ) / ( WORK( J )*DELTA( J ) )
  110    CONTINUE
*
         PHI = ZERO
         OPS = OPS + DBLE( 4*( N-I-1 ) + 10 )
         DO 120 J = N, I + 2, -1
            PHI = PHI + Z( J )*Z( J ) / ( WORK( J )*DELTA( J ) )
  120    CONTINUE
         C = RHOINV + PSI + PHI
         W = C + Z( I )*Z( I ) / ( WORK( I )*DELTA( I ) ) +
     $       Z( IP1 )*Z( IP1 ) / ( WORK( IP1 )*DELTA( IP1 ) )
*
         IF( W.GT.ZERO ) THEN
*
*           d(i)^2 < the ith sigma^2 < (d(i)^2+d(i+1)^2)/2
*
*           We choose d(i) as origin.
*
            OPS = OPS + DBLE( 20 )
            ORGATI = .TRUE.
            SG2LB = ZERO
            SG2UB = DELSQ2
            A = C*DELSQ + Z( I )*Z( I ) + Z( IP1 )*Z( IP1 )
            B = Z( I )*Z( I )*DELSQ
            IF( A.GT.ZERO ) THEN
               TAU = TWO*B / ( A+SQRT( ABS( A*A-FOUR*B*C ) ) )
            ELSE
               TAU = ( A-SQRT( ABS( A*A-FOUR*B*C ) ) ) / ( TWO*C )
            END IF
*
*           TAU now is an estimation of SIGMA^2 - D( I )^2. The
*           following, however, is the corresponding estimation of
*           SIGMA - D( I ).
*
            ETA = TAU / ( D( I )+SQRT( D( I )*D( I )+TAU ) )
         ELSE
*
*           (d(i)^2+d(i+1)^2)/2 <= the ith sigma^2 < d(i+1)^2/2
*
*           We choose d(i+1) as origin.
*
            OPS = OPS + DBLE( 20 )
            ORGATI = .FALSE.
            SG2LB = -DELSQ2
            SG2UB = ZERO
            A = C*DELSQ - Z( I )*Z( I ) - Z( IP1 )*Z( IP1 )
            B = Z( IP1 )*Z( IP1 )*DELSQ
            IF( A.LT.ZERO ) THEN
               TAU = TWO*B / ( A-SQRT( ABS( A*A+FOUR*B*C ) ) )
            ELSE
               TAU = -( A+SQRT( ABS( A*A+FOUR*B*C ) ) ) / ( TWO*C )
            END IF
*
*           TAU now is an estimation of SIGMA^2 - D( IP1 )^2. The
*           following, however, is the corresponding estimation of
*           SIGMA - D( IP1 ).
*
            ETA = TAU / ( D( IP1 )+SQRT( ABS( D( IP1 )*D( IP1 )+
     $            TAU ) ) )
         END IF
*
         OPS = OPS + DBLE( 1 + 4*N )
         IF( ORGATI ) THEN
            II = I
            SIGMA = D( I ) + ETA
            DO 130 J = 1, N
               WORK( J ) = D( J ) + D( I ) + ETA
               DELTA( J ) = ( D( J )-D( I ) ) - ETA
  130       CONTINUE
         ELSE
            II = I + 1
            SIGMA = D( IP1 ) + ETA
            DO 140 J = 1, N
               WORK( J ) = D( J ) + D( IP1 ) + ETA
               DELTA( J ) = ( D( J )-D( IP1 ) ) - ETA
  140       CONTINUE
         END IF
         IIM1 = II - 1
         IIP1 = II + 1
*
*        Evaluate PSI and the derivative DPSI
*
         DPSI = ZERO
         PSI = ZERO
         ERRETM = ZERO
         OPS = OPS + DBLE( 7*IIM1 )
         DO 150 J = 1, IIM1
            TEMP = Z( J ) / ( WORK( J )*DELTA( J ) )
            PSI = PSI + Z( J )*TEMP
            DPSI = DPSI + TEMP*TEMP
            ERRETM = ERRETM + PSI
  150    CONTINUE
         ERRETM = ABS( ERRETM )
*
*        Evaluate PHI and the derivative DPHI
*
         DPHI = ZERO
         PHI = ZERO
         OPS = OPS + DBLE( 7*( N-IIP1+1 ) + 2 )
         DO 160 J = N, IIP1, -1
            TEMP = Z( J ) / ( WORK( J )*DELTA( J ) )
            PHI = PHI + Z( J )*TEMP
            DPHI = DPHI + TEMP*TEMP
            ERRETM = ERRETM + PHI
  160    CONTINUE
*
         W = RHOINV + PHI + PSI
*
*        W is the value of the secular function with
*        its ii-th element removed.
*
         SWTCH3 = .FALSE.
         IF( ORGATI ) THEN
            IF( W.LT.ZERO )
     $         SWTCH3 = .TRUE.
         ELSE
            IF( W.GT.ZERO )
     $         SWTCH3 = .TRUE.
         END IF
         IF( II.EQ.1 .OR. II.EQ.N )
     $      SWTCH3 = .FALSE.
*
         OPS = OPS + DBLE( 17 )
         TEMP = Z( II ) / ( WORK( II )*DELTA( II ) )
         DW = DPSI + DPHI + TEMP*TEMP
         TEMP = Z( II )*TEMP
         W = W + TEMP
         ERRETM = EIGHT*( PHI-PSI ) + ERRETM + TWO*RHOINV +
     $            THREE*ABS( TEMP ) + ABS( TAU )*DW
*
*        Test for convergence
*
         IF( ABS( W ).LE.EPS*ERRETM ) THEN
            GO TO 240
         END IF
*
         IF( W.LE.ZERO ) THEN
            SG2LB = MAX( SG2LB, TAU )
         ELSE
            SG2UB = MIN( SG2UB, TAU )
         END IF
*
*        Calculate the new step
*
         NITER = NITER + 1
         IF( .NOT.SWTCH3 ) THEN
            OPS = OPS + DBLE( 15 )
            DTIPSQ = WORK( IP1 )*DELTA( IP1 )
            DTISQ = WORK( I )*DELTA( I )
            IF( ORGATI ) THEN
               C = W - DTIPSQ*DW + DELSQ*( Z( I ) / DTISQ )**2
            ELSE
               C = W - DTISQ*DW - DELSQ*( Z( IP1 ) / DTIPSQ )**2
            END IF
            A = ( DTIPSQ+DTISQ )*W - DTIPSQ*DTISQ*DW
            B = DTIPSQ*DTISQ*W
            IF( C.EQ.ZERO ) THEN
               IF( A.EQ.ZERO ) THEN
                  OPS = OPS + DBLE( 5 )
                  IF( ORGATI ) THEN
                     A = Z( I )*Z( I ) + DTIPSQ*DTIPSQ*( DPSI+DPHI )
                  ELSE
                     A = Z( IP1 )*Z( IP1 ) + DTISQ*DTISQ*( DPSI+DPHI )
                  END IF
               END IF
               OPS = OPS + DBLE( 1 )
               ETA = B / A
            ELSE IF( A.LE.ZERO ) THEN
               OPS = OPS + DBLE( 8 )
               ETA = ( A-SQRT( ABS( A*A-FOUR*B*C ) ) ) / ( TWO*C )
            ELSE
               OPS = OPS + DBLE( 8 )
               ETA = TWO*B / ( A+SQRT( ABS( A*A-FOUR*B*C ) ) )
            END IF
         ELSE
*
*           Interpolation using THREE most relevant poles
*
            OPS = OPS + DBLE( 15 )
            DTIIM = WORK( IIM1 )*DELTA( IIM1 )
            DTIIP = WORK( IIP1 )*DELTA( IIP1 )
            TEMP = RHOINV + PSI + PHI
            IF( ORGATI ) THEN
               TEMP1 = Z( IIM1 ) / DTIIM
               TEMP1 = TEMP1*TEMP1
               C = ( TEMP - DTIIP*( DPSI+DPHI ) ) -
     $             ( D( IIM1 )-D( IIP1 ) )*( D( IIM1 )+D( IIP1 ) )*TEMP1
               ZZ( 1 ) = Z( IIM1 )*Z( IIM1 )
               IF( DPSI.LT.TEMP1 ) THEN
                  OPS = OPS + DBLE( 2 )
                  ZZ( 3 ) = DTIIP*DTIIP*DPHI
               ELSE
                  OPS = OPS + DBLE( 4 )
                  ZZ( 3 ) = DTIIP*DTIIP*( ( DPSI-TEMP1 )+DPHI )
               END IF
            ELSE
               TEMP1 = Z( IIP1 ) / DTIIP
               TEMP1 = TEMP1*TEMP1
               C = ( TEMP - DTIIM*( DPSI+DPHI ) ) -
     $             ( D( IIP1 )-D( IIM1 ) )*( D( IIM1 )+D( IIP1 ) )*TEMP1
               IF( DPHI.LT.TEMP1 ) THEN
                  OPS = OPS + DBLE( 2 )
                  ZZ( 1 ) = DTIIM*DTIIM*DPSI
               ELSE
                  OPS = OPS + DBLE( 4 )
                  ZZ( 1 ) = DTIIM*DTIIM*( DPSI+( DPHI-TEMP1 ) )
               END IF
               ZZ( 3 ) = Z( IIP1 )*Z( IIP1 )
            END IF
            OPS = OPS + DBLE( 2 )
            ZZ( 2 ) = Z( II )*Z( II )
            DD( 1 ) = DTIIM
            DD( 2 ) = DELTA( II )*WORK( II )
            DD( 3 ) = DTIIP
            CALL DLAED6( NITER, ORGATI, C, DD, ZZ, W, ETA, INFO )
            IF( INFO.NE.0 )
     $         GO TO 240
         END IF
*
*        Note, eta should be positive if w is negative, and
*        eta should be negative otherwise. However,
*        if for some reason caused by roundoff, eta*w > 0,
*        we simply use one Newton step instead. This way
*        will guarantee eta*w < 0.
*
         OPS = OPS + DBLE( 1 )
         IF( W*ETA.GE.ZERO ) THEN
            OPS = OPS + DBLE( 1 )
            ETA = -W / DW
         END IF
         OPS = OPS + DBLE( 8 )
         IF( ORGATI ) THEN
            TEMP1 = WORK( I )*DELTA( I )
            TEMP = ETA - TEMP1
         ELSE
            TEMP1 = WORK( IP1 )*DELTA( IP1 )
            TEMP = ETA - TEMP1
         END IF
         IF( TEMP.GT.SG2UB .OR. TEMP.LT.SG2LB ) THEN
            OPS = OPS + DBLE( 2 )
            IF( W.LT.ZERO ) THEN
               ETA = ( SG2UB-TAU ) / TWO
            ELSE
               ETA = ( SG2LB-TAU ) / TWO
            END IF
         END IF
*
         TAU = TAU + ETA
         ETA = ETA / ( SIGMA+SQRT( SIGMA*SIGMA+ETA ) )
*
         PREW = W
*
         OPS = OPS + DBLE( 1 + 2*N )
         SIGMA = SIGMA + ETA
         DO 170 J = 1, N
            WORK( J ) = WORK( J ) + ETA
            DELTA( J ) = DELTA( J ) - ETA
  170    CONTINUE
*
*        Evaluate PSI and the derivative DPSI
*
         DPSI = ZERO
         PSI = ZERO
         ERRETM = ZERO
         OPS = OPS + DBLE( 7*IIM1 )
         DO 180 J = 1, IIM1
            TEMP = Z( J ) / ( WORK( J )*DELTA( J ) )
            PSI = PSI + Z( J )*TEMP
            DPSI = DPSI + TEMP*TEMP
            ERRETM = ERRETM + PSI
  180    CONTINUE
         ERRETM = ABS( ERRETM )
*
*        Evaluate PHI and the derivative DPHI
*
         DPHI = ZERO
         PHI = ZERO
         OPS = OPS + DBLE( 7*(N-IIM1+1) )
         DO 190 J = N, IIP1, -1
            TEMP = Z( J ) / ( WORK( J )*DELTA( J ) )
            PHI = PHI + Z( J )*TEMP
            DPHI = DPHI + TEMP*TEMP
            ERRETM = ERRETM + PHI
  190    CONTINUE
*
         OPS = OPS + DBLE( 19 )
         TEMP = Z( II ) / ( WORK( II )*DELTA( II ) )
         DW = DPSI + DPHI + TEMP*TEMP
         TEMP = Z( II )*TEMP
         W = RHOINV + PHI + PSI + TEMP
         ERRETM = EIGHT*( PHI-PSI ) + ERRETM + TWO*RHOINV +
     $            THREE*ABS( TEMP ) + ABS( TAU )*DW
*
         IF( W.LE.ZERO ) THEN
            SG2LB = MAX( SG2LB, TAU )
         ELSE
            SG2UB = MIN( SG2UB, TAU )
         END IF
*
         SWTCH = .FALSE.
         IF( ORGATI ) THEN
            IF( -W.GT.ABS( PREW ) / TEN )
     $         SWTCH = .TRUE.
         ELSE
            IF( W.GT.ABS( PREW ) / TEN )
     $         SWTCH = .TRUE.
         END IF
*
*        Main loop to update the values of the array   DELTA and WORK
*
         ITER = NITER + 1
*
         DO 230 NITER = ITER, MAXIT
*
*           Test for convergence
*
            OPS = OPS + DBLE( 1 )
            IF( ABS( W ).LE.EPS*ERRETM ) THEN
               GO TO 240
            END IF
*
*           Calculate the new step
*
            IF( .NOT.SWTCH3 ) THEN
               OPS = OPS + DBLE( 2 )
               DTIPSQ = WORK( IP1 )*DELTA( IP1 )
               DTISQ = WORK( I )*DELTA( I )
               IF( .NOT.SWTCH ) THEN
                  OPS = OPS + DBLE( 6 )
                  IF( ORGATI ) THEN
                     C = W - DTIPSQ*DW + DELSQ*( Z( I ) / DTISQ )**2
                  ELSE
                     C = W - DTISQ*DW - DELSQ*( Z( IP1 ) / DTIPSQ )**2
                  END IF
               ELSE
                  OPS = OPS + DBLE( 8 )
                  TEMP = Z( II ) / ( WORK( II )*DELTA( II ) )
                  IF( ORGATI ) THEN
                     DPSI = DPSI + TEMP*TEMP
                  ELSE
                     DPHI = DPHI + TEMP*TEMP
                  END IF
                  C = W - DTISQ*DPSI - DTIPSQ*DPHI
               END IF
               OPS = OPS + DBLE( 7 )
               A = ( DTIPSQ+DTISQ )*W - DTIPSQ*DTISQ*DW
               B = DTIPSQ*DTISQ*W
               IF( C.EQ.ZERO ) THEN
                  IF( A.EQ.ZERO ) THEN
                     OPS = OPS + DBLE( 5 )
                     IF( .NOT.SWTCH ) THEN
                        IF( ORGATI ) THEN
                           A = Z( I )*Z( I ) + DTIPSQ*DTIPSQ*
     $                         ( DPSI+DPHI )
                        ELSE
                           A = Z( IP1 )*Z( IP1 ) +
     $                         DTISQ*DTISQ*( DPSI+DPHI )
                        END IF
                     ELSE
                        A = DTISQ*DTISQ*DPSI + DTIPSQ*DTIPSQ*DPHI
                     END IF
                  END IF
                  OPS = OPS + DBLE( 1 )
                  ETA = B / A
               ELSE IF( A.LE.ZERO ) THEN
                  OPS = OPS + DBLE( 8 )
                  ETA = ( A-SQRT( ABS( A*A-FOUR*B*C ) ) ) / ( TWO*C )
               ELSE
                  OPS = OPS + DBLE( 8 )
                  ETA = TWO*B / ( A+SQRT( ABS( A*A-FOUR*B*C ) ) )
               END IF
            ELSE
*
*              Interpolation using THREE most relevant poles
*
               OPS = OPS + DBLE( 4 )
               DTIIM = WORK( IIM1 )*DELTA( IIM1 )
               DTIIP = WORK( IIP1 )*DELTA( IIP1 )
               TEMP = RHOINV + PSI + PHI
               IF( SWTCH ) THEN
                  OPS = OPS + DBLE( 8 )
                  C = TEMP - DTIIM*DPSI - DTIIP*DPHI
                  ZZ( 1 ) = DTIIM*DTIIM*DPSI
                  ZZ( 3 ) = DTIIP*DTIIP*DPHI
               ELSE
                  IF( ORGATI ) THEN
                     OPS = OPS + DBLE( 11 )
                     TEMP1 = Z( IIM1 ) / DTIIM
                     TEMP1 = TEMP1*TEMP1
                     TEMP2 = ( D( IIM1 )-D( IIP1 ) )*
     $                       ( D( IIM1 )+D( IIP1 ) )*TEMP1
                     C = TEMP - DTIIP*( DPSI+DPHI ) - TEMP2
                     ZZ( 1 ) = Z( IIM1 )*Z( IIM1 )
                     IF( DPSI.LT.TEMP1 ) THEN
                        OPS = OPS + DBLE( 2 )
                        ZZ( 3 ) = DTIIP*DTIIP*DPHI
                     ELSE
                        OPS = OPS + DBLE( 4 )
                        ZZ( 3 ) = DTIIP*DTIIP*( ( DPSI-TEMP1 )+DPHI )
                     END IF
                  ELSE
                     OPS = OPS + DBLE( 10 )
                     TEMP1 = Z( IIP1 ) / DTIIP
                     TEMP1 = TEMP1*TEMP1
                     TEMP2 = ( D( IIP1 )-D( IIM1 ) )*
     $                       ( D( IIM1 )+D( IIP1 ) )*TEMP1
                     C = TEMP - DTIIM*( DPSI+DPHI ) - TEMP2
                     IF( DPHI.LT.TEMP1 ) THEN
                        OPS = OPS + DBLE( 2 )
                        ZZ( 1 ) = DTIIM*DTIIM*DPSI
                     ELSE
                        OPS = OPS + DBLE( 4 )
                        ZZ( 1 ) = DTIIM*DTIIM*( DPSI+( DPHI-TEMP1 ) )
                     END IF
                     OPS = OPS + DBLE( 1 )
                     ZZ( 3 ) = Z( IIP1 )*Z( IIP1 )
                  END IF
               END IF
               OPS = OPS + DBLE( 1 )
               DD( 1 ) = DTIIM
               DD( 2 ) = DELTA( II )*WORK( II )
               DD( 3 ) = DTIIP
               CALL DLAED6( NITER, ORGATI, C, DD, ZZ, W, ETA, INFO )
               IF( INFO.NE.0 )
     $            GO TO 240
            END IF
*
*           Note, eta should be positive if w is negative, and
*           eta should be negative otherwise. However,
*           if for some reason caused by roundoff, eta*w > 0,
*           we simply use one Newton step instead. This way
*           will guarantee eta*w < 0.
*
            OPS = OPS + DBLE( 1 )
            IF( W*ETA.GE.ZERO ) THEN
               OPS = OPS + DBLE( 1 )
               ETA = -W / DW
            END IF
            OPS = OPS + DBLE( 2 )
            IF( ORGATI ) THEN
               TEMP1 = WORK( I )*DELTA( I )
               TEMP = ETA - TEMP1
            ELSE
               TEMP1 = WORK( IP1 )*DELTA( IP1 )
               TEMP = ETA - TEMP1
            END IF
            IF( TEMP.GT.SG2UB .OR. TEMP.LT.SG2LB ) THEN
               OPS = OPS + DBLE( 2 )
               IF( W.LT.ZERO ) THEN
                  ETA = ( SG2UB-TAU ) / TWO
               ELSE
                  ETA = ( SG2LB-TAU ) / TWO
               END IF
            END IF
*
            OPS = OPS + DBLE( 6 )
            TAU = TAU + ETA
            ETA = ETA / ( SIGMA+SQRT( SIGMA*SIGMA+ETA ) )
*
            OPS = OPS + DBLE( 1 + 2*N )
            SIGMA = SIGMA + ETA
            DO 200 J = 1, N
               WORK( J ) = WORK( J ) + ETA
               DELTA( J ) = DELTA( J ) - ETA
  200       CONTINUE
*
            PREW = W
*
*           Evaluate PSI and the derivative DPSI
*
            DPSI = ZERO
            PSI = ZERO
            ERRETM = ZERO
            OPS = OPS + DBLE( 7*IIM1 )
            DO 210 J = 1, IIM1
               TEMP = Z( J ) / ( WORK( J )*DELTA( J ) )
               PSI = PSI + Z( J )*TEMP
               DPSI = DPSI + TEMP*TEMP
               ERRETM = ERRETM + PSI
  210       CONTINUE
            ERRETM = ABS( ERRETM )
*
*           Evaluate PHI and the derivative DPHI
*
            DPHI = ZERO
            PHI = ZERO
            OPS = OPS + DBLE( 7*( IIM1-N+1 ) )
            DO 220 J = N, IIP1, -1
               TEMP = Z( J ) / ( WORK( J )*DELTA( J ) )
               PHI = PHI + Z( J )*TEMP
               DPHI = DPHI + TEMP*TEMP
               ERRETM = ERRETM + PHI
  220       CONTINUE
*
            OPS = OPS + DBLE( 19 )
            TEMP = Z( II ) / ( WORK( II )*DELTA( II ) )
            DW = DPSI + DPHI + TEMP*TEMP
            TEMP = Z( II )*TEMP
            W = RHOINV + PHI + PSI + TEMP
            ERRETM = EIGHT*( PHI-PSI ) + ERRETM + TWO*RHOINV +
     $               THREE*ABS( TEMP ) + ABS( TAU )*DW
            IF( W*PREW.GT.ZERO .AND. ABS( W ).GT.ABS( PREW ) / TEN )
     $         SWTCH = .NOT.SWTCH
*
            IF( W.LE.ZERO ) THEN
               SG2LB = MAX( SG2LB, TAU )
            ELSE
               SG2UB = MIN( SG2UB, TAU )
            END IF
*
  230    CONTINUE
*
*        Return with INFO = 1, NITER = MAXIT and not converged
*
         INFO = 1
*
      END IF
*
  240 CONTINUE
      RETURN
*
*     End of DLASD4
*
      END
      SUBROUTINE DLASD5( I, D, Z, DELTA, RHO, DSIGMA, WORK )
*
*  -- LAPACK auxiliary routine (instrumented to count ops, version 3.0) --
*     Univ. of Tennessee, Oak Ridge National Lab, Argonne National Lab,
*     Courant Institute, NAG Ltd., and Rice University
*     June 30, 1999
*
*     .. Scalar Arguments ..
      INTEGER            I
      DOUBLE PRECISION   DSIGMA, RHO
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   D( 2 ), DELTA( 2 ), WORK( 2 ), Z( 2 )
*     ..
*     .. Common block to return operation count ..
      COMMON             / LATIME / OPS, ITCNT
*     ..
*     .. Scalars in Common ..
      DOUBLE PRECISION   ITCNT, OPS
*     ..
*
*  Purpose
*  =======
*
*  This subroutine computes the square root of the I-th eigenvalue
*  of a positive symmetric rank-one modification of a 2-by-2 diagonal
*  matrix
*
*             diag( D ) * diag( D ) +  RHO *  Z * transpose(Z) .
*
*  The diagonal entries in the array D are assumed to satisfy
*
*             0 <= D(i) < D(j)  for  i < j .
*
*  We also assume RHO > 0 and that the Euclidean norm of the vector
*  Z is one.
*
*  Arguments
*  =========
*
*  I      (input) INTEGER
*         The index of the eigenvalue to be computed.  I = 1 or I = 2.
*
*  D      (input) DOUBLE PRECISION array, dimension ( 2 )
*         The original eigenvalues.  We assume 0 <= D(1) < D(2).
*
*  Z      (input) DOUBLE PRECISION array, dimension ( 2 )
*         The components of the updating vector.
*
*  DELTA  (output) DOUBLE PRECISION array, dimension ( 2 )
*         Contains (D(j) - lambda_I) in its  j-th component.
*         The vector DELTA contains the information necessary
*         to construct the eigenvectors.
*
*  RHO    (input) DOUBLE PRECISION
*         The scalar in the symmetric updating formula.
*
*  DSIGMA (output) DOUBLE PRECISION
*         The computed lambda_I, the I-th updated eigenvalue.
*
*  WORK   (workspace) DOUBLE PRECISION array, dimension ( 2 )
*         WORK contains (D(j) + sigma_I) in its  j-th component.
*
*  Further Details
*  ===============
*
*  Based on contributions by
*     Ren-Cang Li, Computer Science Division, University of California
*     at Berkeley, USA
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE, TWO, THREE, FOUR
      PARAMETER          ( ZERO = 0.0D0, ONE = 1.0D0, TWO = 2.0D0,
     $                   THREE = 3.0D0, FOUR = 4.0D0 )
*     ..
*     .. Local Scalars ..
      DOUBLE PRECISION   B, C, DEL, DELSQ, TAU, W
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          DBLE, ABS, SQRT
*     ..
*     .. Executable Statements ..
*
      OPS = OPS + DBLE( 3 )
      DEL = D( 2 ) - D( 1 )
      DELSQ = DEL*( D( 2 )+D( 1 ) )
      IF( I.EQ.1 ) THEN
         OPS = OPS + DBLE( 13 )
         W = ONE + FOUR*RHO*( Z( 2 )*Z( 2 ) / ( D( 1 )+THREE*D( 2 ) )-
     $       Z( 1 )*Z( 1 ) / ( THREE*D( 1 )+D( 2 ) ) ) / DEL
         IF( W.GT.ZERO ) THEN
            OPS = OPS + DBLE( 8 )
            B = DELSQ + RHO*( Z( 1 )*Z( 1 )+Z( 2 )*Z( 2 ) )
            C = RHO*Z( 1 )*Z( 1 )*DELSQ
*
*           B > ZERO, always
*
*           The following TAU is DSIGMA * DSIGMA - D( 1 ) * D( 1 )
*
            OPS = OPS + DBLE( 7 )
            TAU = TWO*C / ( B+SQRT( ABS( B*B-FOUR*C ) ) )
*
*           The following TAU is DSIGMA - D( 1 )
*
            OPS = OPS + DBLE( 14 )
            TAU = TAU / ( D( 1 )+SQRT( D( 1 )*D( 1 )+TAU ) )
            DSIGMA = D( 1 ) + TAU
            DELTA( 1 ) = -TAU
            DELTA( 2 ) = DEL - TAU
            WORK( 1 ) = TWO*D( 1 ) + TAU
            WORK( 2 ) = ( D( 1 )+TAU ) + D( 2 )
*           DELTA( 1 ) = -Z( 1 ) / TAU
*           DELTA( 2 ) = Z( 2 ) / ( DEL-TAU )
         ELSE
            OPS = OPS + DBLE( 8 )
            B = -DELSQ + RHO*( Z( 1 )*Z( 1 )+Z( 2 )*Z( 2 ) )
            C = RHO*Z( 2 )*Z( 2 )*DELSQ
*
*           The following TAU is DSIGMA * DSIGMA - D( 2 ) * D( 2 )
*
            IF( B.GT.ZERO ) THEN
               OPS = OPS + DBLE( 7 )
               TAU = -TWO*C / ( B+SQRT( B*B+FOUR*C ) )
            ELSE
               OPS = OPS + DBLE( 6 )
               TAU = ( B-SQRT( B*B+FOUR*C ) ) / TWO
            END IF
*
*           The following TAU is DSIGMA - D( 2 )
*
            OPS = OPS + DBLE( 14 )
            TAU = TAU / ( D( 2 )+SQRT( ABS( D( 2 )*D( 2 )+TAU ) ) )
            DSIGMA = D( 2 ) + TAU
            DELTA( 1 ) = -( DEL+TAU )
            DELTA( 2 ) = -TAU
            WORK( 1 ) = D( 1 ) + TAU + D( 2 )
            WORK( 2 ) = TWO*D( 2 ) + TAU
*           DELTA( 1 ) = -Z( 1 ) / ( DEL+TAU )
*           DELTA( 2 ) = -Z( 2 ) / TAU
         END IF
         OPS = OPS + DBLE( 6 )
*        TEMP = SQRT( DELTA( 1 )*DELTA( 1 )+DELTA( 2 )*DELTA( 2 ) )
*        DELTA( 1 ) = DELTA( 1 ) / TEMP
*        DELTA( 2 ) = DELTA( 2 ) / TEMP
      ELSE
*
*        Now I=2
*
         OPS = OPS + DBLE( 8 )
         B = -DELSQ + RHO*( Z( 1 )*Z( 1 )+Z( 2 )*Z( 2 ) )
         C = RHO*Z( 2 )*Z( 2 )*DELSQ
*
*        The following TAU is DSIGMA * DSIGMA - D( 2 ) * D( 2 )
*
         IF( B.GT.ZERO ) THEN
            OPS = OPS + DBLE( 6 )
            TAU = ( B+SQRT( B*B+FOUR*C ) ) / TWO
         ELSE
            OPS = OPS + DBLE( 7 )
            TAU = TWO*C / ( -B+SQRT( B*B+FOUR*C ) )
         END IF
*
*        The following TAU is DSIGMA - D( 2 )
*
         OPS = OPS + DBLE( 20 )
         TAU = TAU / ( D( 2 )+SQRT( D( 2 )*D( 2 )+TAU ) )
         DSIGMA = D( 2 ) + TAU
         DELTA( 1 ) = -( DEL+TAU )
         DELTA( 2 ) = -TAU
         WORK( 1 ) = D( 1 ) + TAU + D( 2 )
         WORK( 2 ) = TWO*D( 2 ) + TAU
*        DELTA( 1 ) = -Z( 1 ) / ( DEL+TAU )
*        DELTA( 2 ) = -Z( 2 ) / TAU
*        TEMP = SQRT( DELTA( 1 )*DELTA( 1 )+DELTA( 2 )*DELTA( 2 ) )
*        DELTA( 1 ) = DELTA( 1 ) / TEMP
*        DELTA( 2 ) = DELTA( 2 ) / TEMP
      END IF
      RETURN
*
*     End of DLASD5
*
      END
      SUBROUTINE DLASD6( ICOMPQ, NL, NR, SQRE, D, VF, VL, ALPHA, BETA,
     $                   IDXQ, PERM, GIVPTR, GIVCOL, LDGCOL, GIVNUM,
     $                   LDGNUM, POLES, DIFL, DIFR, Z, K, C, S, WORK,
     $                   IWORK, INFO )
*
*  -- LAPACK auxiliary routine (instrumented to count ops, version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     June 30, 1999
*
*     .. Scalar Arguments ..
      INTEGER            GIVPTR, ICOMPQ, INFO, K, LDGCOL, LDGNUM, NL,
     $                   NR, SQRE
      DOUBLE PRECISION   ALPHA, BETA, C, S
*     ..
*     .. Array Arguments ..
      INTEGER            GIVCOL( LDGCOL, * ), IDXQ( * ), IWORK( * ),
     $                   PERM( * )
      DOUBLE PRECISION   D( * ), DIFL( * ), DIFR( * ),
     $                   GIVNUM( LDGNUM, * ), POLES( LDGNUM, * ),
     $                   VF( * ), VL( * ), WORK( * ), Z( * )
*     ..
*     .. Common block to return operation count ..
      COMMON             / LATIME / OPS, ITCNT
*     ..
*     .. Scalars in Common ..
      DOUBLE PRECISION   ITCNT, OPS
*     ..
*
*  Purpose
*  =======
*
*  DLASD6 computes the SVD of an updated upper bidiagonal matrix B
*  obtained by merging two smaller ones by appending a row. This
*  routine is used only for the problem which requires all singular
*  values and optionally singular vector matrices in factored form.
*  B is an N-by-M matrix with N = NL + NR + 1 and M = N + SQRE.
*  A related subroutine, DLASD1, handles the case in which all singular
*  values and singular vectors of the bidiagonal matrix are desired.
*
*  DLASD6 computes the SVD as follows:
*
*                ( D1(in)  0    0     0 )
*    B = U(in) * (   Z1'   a   Z2'    b ) * VT(in)
*                (   0     0   D2(in) 0 )
*
*      = U(out) * ( D(out) 0) * VT(out)
*
*  where Z' = (Z1' a Z2' b) = u' VT', and u is a vector of dimension M
*  with ALPHA and BETA in the NL+1 and NL+2 th entries and zeros
*  elsewhere; and the entry b is empty if SQRE = 0.
*
*  The singular values of B can be computed using D1, D2, the first
*  components of all the right singular vectors of the lower block, and
*  the last components of all the right singular vectors of the upper
*  block. These components are stored and updated in VF and VL,
*  respectively, in DLASD6. Hence U and VT are not explicitly
*  referenced.
*
*  The singular values are stored in D. The algorithm consists of two
*  stages:
*
*        The first stage consists of deflating the size of the problem
*        when there are multiple singular values or if there is a zero
*        in the Z vector. For each such occurence the dimension of the
*        secular equation problem is reduced by one. This stage is
*        performed by the routine DLASD7.
*
*        The second stage consists of calculating the updated
*        singular values. This is done by finding the roots of the
*        secular equation via the routine DLASD4 (as called by DLASD8).
*        This routine also updates VF and VL and computes the distances
*        between the updated singular values and the old singular
*        values.
*
*  DLASD6 is called from DLASDA.
*
*  Arguments
*  =========
*
*  ICOMPQ (input) INTEGER
*         Specifies whether singular vectors are to be computed in
*         factored form:
*         = 0: Compute singular values only.
*         = 1: Compute singular vectors in factored form as well.
*
*  NL     (input) INTEGER
*         The row dimension of the upper block.  NL >= 1.
*
*  NR     (input) INTEGER
*         The row dimension of the lower block.  NR >= 1.
*
*  SQRE   (input) INTEGER
*         = 0: the lower block is an NR-by-NR square matrix.
*         = 1: the lower block is an NR-by-(NR+1) rectangular matrix.
*
*         The bidiagonal matrix has row dimension N = NL + NR + 1,
*         and column dimension M = N + SQRE.
*
*  D      (input/output) DOUBLE PRECISION array, dimension ( NL+NR+1 ).
*         On entry D(1:NL,1:NL) contains the singular values of the
*         upper block, and D(NL+2:N) contains the singular values
*         of the lower block. On exit D(1:N) contains the singular
*         values of the modified matrix.
*
*  VF     (input/output) DOUBLE PRECISION array, dimension ( M )
*         On entry, VF(1:NL+1) contains the first components of all
*         right singular vectors of the upper block; and VF(NL+2:M)
*         contains the first components of all right singular vectors
*         of the lower block. On exit, VF contains the first components
*         of all right singular vectors of the bidiagonal matrix.
*
*  VL     (input/output) DOUBLE PRECISION array, dimension ( M )
*         On entry, VL(1:NL+1) contains the  last components of all
*         right singular vectors of the upper block; and VL(NL+2:M)
*         contains the last components of all right singular vectors of
*         the lower block. On exit, VL contains the last components of
*         all right singular vectors of the bidiagonal matrix.
*
*  ALPHA  (input) DOUBLE PRECISION
*         Contains the diagonal element associated with the added row.
*
*  BETA   (input) DOUBLE PRECISION
*         Contains the off-diagonal element associated with the added
*         row.
*
*  IDXQ   (output) INTEGER array, dimension ( N )
*         This contains the permutation which will reintegrate the
*         subproblem just solved back into sorted order, i.e.
*         D( IDXQ( I = 1, N ) ) will be in ascending order.
*
*  PERM   (output) INTEGER array, dimension ( N )
*         The permutations (from deflation and sorting) to be applied
*         to each block. Not referenced if ICOMPQ = 0.
*
*  GIVPTR (output) INTEGER
*         The number of Givens rotations which took place in this
*         subproblem. Not referenced if ICOMPQ = 0.
*
*  GIVCOL (output) INTEGER array, dimension ( LDGCOL, 2 )
*         Each pair of numbers indicates a pair of columns to take place
*         in a Givens rotation. Not referenced if ICOMPQ = 0.
*
*  LDGCOL (input) INTEGER
*         leading dimension of GIVCOL, must be at least N.
*
*  GIVNUM (output) DOUBLE PRECISION array, dimension ( LDGNUM, 2 )
*         Each number indicates the C or S value to be used in the
*         corresponding Givens rotation. Not referenced if ICOMPQ = 0.
*
*  LDGNUM (input) INTEGER
*         The leading dimension of GIVNUM and POLES, must be at least N.
*
*  POLES  (output) DOUBLE PRECISION array, dimension ( LDGNUM, 2 )
*         On exit, POLES(1,*) is an array containing the new singular
*         values obtained from solving the secular equation, and
*         POLES(2,*) is an array containing the poles in the secular
*         equation. Not referenced if ICOMPQ = 0.
*
*  DIFL   (output) DOUBLE PRECISION array, dimension ( N )
*         On exit, DIFL(I) is the distance between I-th updated
*         (undeflated) singular value and the I-th (undeflated) old
*         singular value.
*
*  DIFR   (output) DOUBLE PRECISION array,
*                  dimension ( LDGNUM, 2 ) if ICOMPQ = 1 and
*                  dimension ( N ) if ICOMPQ = 0.
*         On exit, DIFR(I, 1) is the distance between I-th updated
*         (undeflated) singular value and the I+1-th (undeflated) old
*         singular value.
*
*         If ICOMPQ = 1, DIFR(1:K,2) is an array containing the
*         normalizing factors for the right singular vector matrix.
*
*         See DLASD8 for details on DIFL and DIFR.
*
*  Z      (output) DOUBLE PRECISION array, dimension ( M )
*         The first elements of this array contain the components
*         of the deflation-adjusted updating row vector.
*
*  K      (output) INTEGER
*         Contains the dimension of the non-deflated matrix,
*         This is the order of the related secular equation. 1 <= K <=N.
*
*  C      (output) DOUBLE PRECISION
*         C contains garbage if SQRE =0 and the C-value of a Givens
*         rotation related to the right null space if SQRE = 1.
*
*  S      (output) DOUBLE PRECISION
*         S contains garbage if SQRE =0 and the S-value of a Givens
*         rotation related to the right null space if SQRE = 1.
*
*  WORK   (workspace) DOUBLE PRECISION array, dimension ( 4 * M )
*
*  IWORK  (workspace) INTEGER array, dimension ( 3 * N )
*
*  INFO   (output) INTEGER
*          = 0:  successful exit.
*          < 0:  if INFO = -i, the i-th argument had an illegal value.
*          > 0:  if INFO = 1, an singular value did not converge
*
*  Further Details
*  ===============
*
*  Based on contributions by
*     Ming Gu and Huan Ren, Computer Science Division, University of
*     California at Berkeley, USA
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ONE, ZERO
      PARAMETER          ( ONE = 1.0D0, ZERO = 0.0D0 )
*     ..
*     .. Local Scalars ..
      INTEGER            I, IDX, IDXC, IDXP, ISIGMA, IVFW, IVLW, IW, M,
     $                   N, N1, N2
      DOUBLE PRECISION   ORGNRM
*     ..
*     .. External Subroutines ..
      EXTERNAL           DCOPY, DLAMRG, DLASCL, DLASD7, DLASD8, XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          DBLE, ABS, MAX
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters.
*
      INFO = 0
      N = NL + NR + 1
      M = N + SQRE
*
      IF( ( ICOMPQ.LT.0 ) .OR. ( ICOMPQ.GT.1 ) ) THEN
         INFO = -1
      ELSE IF( NL.LT.1 ) THEN
         INFO = -2
      ELSE IF( NR.LT.1 ) THEN
         INFO = -3
      ELSE IF( ( SQRE.LT.0 ) .OR. ( SQRE.GT.1 ) ) THEN
         INFO = -4
      ELSE IF( LDGCOL.LT.N ) THEN
         INFO = -14
      ELSE IF( LDGNUM.LT.N ) THEN
         INFO = -16
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DLASD6', -INFO )
         RETURN
      END IF
*
*     The following values are for bookkeeping purposes only.  They are
*     integer pointers which indicate the portion of the workspace
*     used by a particular array in DLASD7 and DLASD8.
*
      ISIGMA = 1
      IW = ISIGMA + N
      IVFW = IW + M
      IVLW = IVFW + M
*
      IDX = 1
      IDXC = IDX + N
      IDXP = IDXC + N
*
*     Scale.
*
      ORGNRM = MAX( ABS( ALPHA ), ABS( BETA ) )
      D( NL+1 ) = ZERO
      DO 10 I = 1, N
         IF( ABS( D( I ) ).GT.ORGNRM ) THEN
            ORGNRM = ABS( D( I ) )
         END IF
   10 CONTINUE
      OPS = OPS + DBLE( N + 2 )
      CALL DLASCL( 'G', 0, 0, ORGNRM, ONE, N, 1, D, N, INFO )
      ALPHA = ALPHA / ORGNRM
      BETA = BETA / ORGNRM
*
*     Sort and Deflate singular values.
*
      CALL DLASD7( ICOMPQ, NL, NR, SQRE, K, D, Z, WORK( IW ), VF,
     $             WORK( IVFW ), VL, WORK( IVLW ), ALPHA, BETA,
     $             WORK( ISIGMA ), IWORK( IDX ), IWORK( IDXP ), IDXQ,
     $             PERM, GIVPTR, GIVCOL, LDGCOL, GIVNUM, LDGNUM, C, S,
     $             INFO )
*
*     Solve Secular Equation, compute DIFL, DIFR, and update VF, VL.
*
      CALL DLASD8( ICOMPQ, K, D, Z, VF, VL, DIFL, DIFR, LDGNUM,
     $             WORK( ISIGMA ), WORK( IW ), INFO )
*
*     Save the poles if ICOMPQ = 1.
*
      IF( ICOMPQ.EQ.1 ) THEN
         CALL DCOPY( K, D, 1, POLES( 1, 1 ), 1 )
         CALL DCOPY( K, WORK( ISIGMA ), 1, POLES( 1, 2 ), 1 )
      END IF
*
*     Unscale.
*
      OPS = OPS + DBLE( N )
      CALL DLASCL( 'G', 0, 0, ONE, ORGNRM, N, 1, D, N, INFO )
*
*     Prepare the IDXQ sorting permutation.
*
      N1 = K
      N2 = N - K
      CALL DLAMRG( N1, N2, D, 1, -1, IDXQ )
*
      RETURN
*
*     End of DLASD6
*
      END
      SUBROUTINE DLASD7( ICOMPQ, NL, NR, SQRE, K, D, Z, ZW, VF, VFW, VL,
     $                   VLW, ALPHA, BETA, DSIGMA, IDX, IDXP, IDXQ,
     $                   PERM, GIVPTR, GIVCOL, LDGCOL, GIVNUM, LDGNUM,
     $                   C, S, INFO )
*
*  -- LAPACK auxiliary routine (instrumented to count ops, version 3.0) --
*     Univ. of Tennessee, Oak Ridge National Lab, Argonne National Lab,
*     Courant Institute, NAG Ltd., and Rice University
*     June 30, 1999
*
*     .. Scalar Arguments ..
      INTEGER            GIVPTR, ICOMPQ, INFO, K, LDGCOL, LDGNUM, NL,
     $                   NR, SQRE
      DOUBLE PRECISION   ALPHA, BETA, C, S
*     ..
*     .. Array Arguments ..
      INTEGER            GIVCOL( LDGCOL, * ), IDX( * ), IDXP( * ),
     $                   IDXQ( * ), PERM( * )
      DOUBLE PRECISION   D( * ), DSIGMA( * ), GIVNUM( LDGNUM, * ),
     $                   VF( * ), VFW( * ), VL( * ), VLW( * ), Z( * ),
     $                   ZW( * )
*     ..
*     .. Common block to return operation count ..
      COMMON             / LATIME / OPS, ITCNT
*     ..
*     .. Scalars in Common ..
      DOUBLE PRECISION   ITCNT, OPS
*     ..
*
*  Purpose
*  =======
*
*  DLASD7 merges the two sets of singular values together into a single
*  sorted set. Then it tries to deflate the size of the problem. There
*  are two ways in which deflation can occur:  when two or more singular
*  values are close together or if there is a tiny entry in the Z
*  vector. For each such occurrence the order of the related
*  secular equation problem is reduced by one.
*
*  DLASD7 is called from DLASD6.
*
*  Arguments
*  =========
*
*  ICOMPQ  (input) INTEGER
*          Specifies whether singular vectors are to be computed
*          in compact form, as follows:
*          = 0: Compute singular values only.
*          = 1: Compute singular vectors of upper
*               bidiagonal matrix in compact form.
*
*  NL     (input) INTEGER
*         The row dimension of the upper block. NL >= 1.
*
*  NR     (input) INTEGER
*         The row dimension of the lower block. NR >= 1.
*
*  SQRE   (input) INTEGER
*         = 0: the lower block is an NR-by-NR square matrix.
*         = 1: the lower block is an NR-by-(NR+1) rectangular matrix.
*
*         The bidiagonal matrix has
*         N = NL + NR + 1 rows and
*         M = N + SQRE >= N columns.
*
*  K      (output) INTEGER
*         Contains the dimension of the non-deflated matrix, this is
*         the order of the related secular equation. 1 <= K <=N.
*
*  D      (input/output) DOUBLE PRECISION array, dimension ( N )
*         On entry D contains the singular values of the two submatrices
*         to be combined. On exit D contains the trailing (N-K) updated
*         singular values (those which were deflated) sorted into
*         increasing order.
*
*  Z      (output) DOUBLE PRECISION array, dimension ( M )
*         On exit Z contains the updating row vector in the secular
*         equation.
*
*  ZW     (workspace) DOUBLE PRECISION array, dimension ( M )
*         Workspace for Z.
*
*  VF     (input/output) DOUBLE PRECISION array, dimension ( M )
*         On entry, VF(1:NL+1) contains the first components of all
*         right singular vectors of the upper block; and VF(NL+2:M)
*         contains the first components of all right singular vectors
*         of the lower block. On exit, VF contains the first components
*         of all right singular vectors of the bidiagonal matrix.
*
*  VFW    (workspace) DOUBLE PRECISION array, dimension ( M )
*         Workspace for VF.
*
*  VL     (input/output) DOUBLE PRECISION array, dimension ( M )
*         On entry, VL(1:NL+1) contains the  last components of all
*         right singular vectors of the upper block; and VL(NL+2:M)
*         contains the last components of all right singular vectors
*         of the lower block. On exit, VL contains the last components
*         of all right singular vectors of the bidiagonal matrix.
*
*  VLW    (workspace) DOUBLE PRECISION array, dimension ( M )
*         Workspace for VL.
*
*  ALPHA  (input) DOUBLE PRECISION
*         Contains the diagonal element associated with the added row.
*
*  BETA   (input) DOUBLE PRECISION
*         Contains the off-diagonal element associated with the added
*         row.
*
*  DSIGMA (output) DOUBLE PRECISION array, dimension ( N )
*         Contains a copy of the diagonal elements (K-1 singular values
*         and one zero) in the secular equation.
*
*  IDX    (workspace) INTEGER array, dimension ( N )
*         This will contain the permutation used to sort the contents of
*         D into ascending order.
*
*  IDXP   (workspace) INTEGER array, dimension ( N )
*         This will contain the permutation used to place deflated
*         values of D at the end of the array. On output IDXP(2:K)
*         points to the nondeflated D-values and IDXP(K+1:N)
*         points to the deflated singular values.
*
*  IDXQ   (input) INTEGER array, dimension ( N )
*         This contains the permutation which separately sorts the two
*         sub-problems in D into ascending order.  Note that entries in
*         the first half of this permutation must first be moved one
*         position backward; and entries in the second half
*         must first have NL+1 added to their values.
*
*  PERM   (output) INTEGER array, dimension ( N )
*         The permutations (from deflation and sorting) to be applied
*         to each singular block. Not referenced if ICOMPQ = 0.
*
*  GIVPTR (output) INTEGER
*         The number of Givens rotations which took place in this
*         subproblem. Not referenced if ICOMPQ = 0.
*
*  GIVCOL (output) INTEGER array, dimension ( LDGCOL, 2 )
*         Each pair of numbers indicates a pair of columns to take place
*         in a Givens rotation. Not referenced if ICOMPQ = 0.
*
*  LDGCOL (input) INTEGER
*         The leading dimension of GIVCOL, must be at least N.
*
*  GIVNUM (output) DOUBLE PRECISION array, dimension ( LDGNUM, 2 )
*         Each number indicates the C or S value to be used in the
*         corresponding Givens rotation. Not referenced if ICOMPQ = 0.
*
*  LDGNUM (input) INTEGER
*         The leading dimension of GIVNUM, must be at least N.
*
*  C      (output) DOUBLE PRECISION
*         C contains garbage if SQRE =0 and the C-value of a Givens
*         rotation related to the right null space if SQRE = 1.
*
*  S      (output) DOUBLE PRECISION
*         S contains garbage if SQRE =0 and the S-value of a Givens
*         rotation related to the right null space if SQRE = 1.
*
*  INFO   (output) INTEGER
*         = 0:  successful exit.
*         < 0:  if INFO = -i, the i-th argument had an illegal value.
*
*  Further Details
*  ===============
*
*  Based on contributions by
*     Ming Gu and Huan Ren, Computer Science Division, University of
*     California at Berkeley, USA
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE, TWO, EIGHT
      PARAMETER          ( ZERO = 0.0D0, ONE = 1.0D0, TWO = 2.0D0,
     $                   EIGHT = 8.0D0 )
*     ..
*     .. Local Scalars ..
*
      INTEGER            I, IDXI, IDXJ, IDXJP, J, JP, JPREV, K2, M, N,
     $                   NLP1, NLP2
      DOUBLE PRECISION   EPS, HLFTOL, TAU, TOL, Z1
*     ..
*     .. External Subroutines ..
      EXTERNAL           DCOPY, DLAMRG, DROT, XERBLA
*     ..
*     .. External Functions ..
      DOUBLE PRECISION   DLAMCH, DLAPY2
      EXTERNAL           DLAMCH, DLAPY2
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          DBLE, ABS, MAX
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters.
*
      INFO = 0
      N = NL + NR + 1
      M = N + SQRE
*
      IF( ( ICOMPQ.LT.0 ) .OR. ( ICOMPQ.GT.1 ) ) THEN
         INFO = -1
      ELSE IF( NL.LT.1 ) THEN
         INFO = -2
      ELSE IF( NR.LT.1 ) THEN
         INFO = -3
      ELSE IF( ( SQRE.LT.0 ) .OR. ( SQRE.GT.1 ) ) THEN
         INFO = -4
      ELSE IF( LDGCOL.LT.N ) THEN
         INFO = -22
      ELSE IF( LDGNUM.LT.N ) THEN
         INFO = -24
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DLASD7', -INFO )
         RETURN
      END IF
*
      NLP1 = NL + 1
      NLP2 = NL + 2
      IF( ICOMPQ.EQ.1 ) THEN
         GIVPTR = 0
      END IF
*
*     Generate the first part of the vector Z and move the singular
*     values in the first part of D one position backward.
*
      OPS = OPS + DBLE( 1 + NL )
      Z1 = ALPHA*VL( NLP1 )
      VL( NLP1 ) = ZERO
      TAU = VF( NLP1 )
      DO 10 I = NL, 1, -1
         Z( I+1 ) = ALPHA*VL( I )
         VL( I ) = ZERO
         VF( I+1 ) = VF( I )
         D( I+1 ) = D( I )
         IDXQ( I+1 ) = IDXQ( I ) + 1
   10 CONTINUE
      VF( 1 ) = TAU
*
*     Generate the second part of the vector Z.
*
      OPS = OPS + DBLE( ( M-NLP2+1 ) )
      DO 20 I = NLP2, M
         Z( I ) = BETA*VF( I )
         VF( I ) = ZERO
   20 CONTINUE
*
*     Sort the singular values into increasing order
*
      DO 30 I = NLP2, N
         IDXQ( I ) = IDXQ( I ) + NLP1
   30 CONTINUE
*
*     DSIGMA, IDXC, IDXC, and ZW are used as storage space.
*
      DO 40 I = 2, N
         DSIGMA( I ) = D( IDXQ( I ) )
         ZW( I ) = Z( IDXQ( I ) )
         VFW( I ) = VF( IDXQ( I ) )
         VLW( I ) = VL( IDXQ( I ) )
   40 CONTINUE
*
      CALL DLAMRG( NL, NR, DSIGMA( 2 ), 1, 1, IDX( 2 ) )
*
      DO 50 I = 2, N
         IDXI = 1 + IDX( I )
         D( I ) = DSIGMA( IDXI )
         Z( I ) = ZW( IDXI )
         VF( I ) = VFW( IDXI )
         VL( I ) = VLW( IDXI )
   50 CONTINUE
*
*     Calculate the allowable deflation tolerence
*
      OPS = OPS + DBLE( 3 )
      EPS = DLAMCH( 'Epsilon' )
      TOL = MAX( ABS( ALPHA ), ABS( BETA ) )
      TOL = EIGHT*EIGHT*EPS*MAX( ABS( D( N ) ), TOL )
*
*     There are 2 kinds of deflation -- first a value in the z-vector
*     is small, second two (or more) singular values are very close
*     together (their difference is small).
*
*     If the value in the z-vector is small, we simply permute the
*     array so that the corresponding singular value is moved to the
*     end.
*
*     If two values in the D-vector are close, we perform a two-sided
*     rotation designed to make one of the corresponding z-vector
*     entries zero, and then permute the array so that the deflated
*     singular value is moved to the end.
*
*     If there are multiple singular values then the problem deflates.
*     Here the number of equal singular values are found.  As each equal
*     singular value is found, an elementary reflector is computed to
*     rotate the corresponding singular subspace so that the
*     corresponding components of Z are zero in this new basis.
*
      K = 1
      K2 = N + 1
      DO 60 J = 2, N
         IF( ABS( Z( J ) ).LE.TOL ) THEN
*
*           Deflate due to small z component.
*
            K2 = K2 - 1
            IDXP( K2 ) = J
            IF( J.EQ.N )
     $         GO TO 100
         ELSE
            JPREV = J
            GO TO 70
         END IF
   60 CONTINUE
   70 CONTINUE
      J = JPREV
   80 CONTINUE
      J = J + 1
      IF( J.GT.N )
     $   GO TO 90
      IF( ABS( Z( J ) ).LE.TOL ) THEN
*
*        Deflate due to small z component.
*
         K2 = K2 - 1
         IDXP( K2 ) = J
      ELSE
*
*        Check if singular values are close enough to allow deflation.
*
         OPS = OPS + DBLE( 1 )
         IF( ABS( D( J )-D( JPREV ) ).LE.TOL ) THEN
*
*           Deflation is possible.
*
            S = Z( JPREV )
            C = Z( J )
*
*           Find sqrt(a**2+b**2) without overflow or
*           destructive underflow.
*
            OPS = OPS + DBLE( 7 )
            TAU = DLAPY2( C, S )
            Z( J ) = TAU
            Z( JPREV ) = ZERO
            C = C / TAU
            S = -S / TAU
*
*           Record the appropriate Givens rotation
*
            IF( ICOMPQ.EQ.1 ) THEN
               GIVPTR = GIVPTR + 1
               IDXJP = IDXQ( IDX( JPREV )+1 )
               IDXJ = IDXQ( IDX( J )+1 )
               IF( IDXJP.LE.NLP1 ) THEN
                  IDXJP = IDXJP - 1
               END IF
               IF( IDXJ.LE.NLP1 ) THEN
                  IDXJ = IDXJ - 1
               END IF
               GIVCOL( GIVPTR, 2 ) = IDXJP
               GIVCOL( GIVPTR, 1 ) = IDXJ
               GIVNUM( GIVPTR, 2 ) = C
               GIVNUM( GIVPTR, 1 ) = S
            END IF
            OPS = OPS + DBLE( 12 )
            CALL DROT( 1, VF( JPREV ), 1, VF( J ), 1, C, S )
            CALL DROT( 1, VL( JPREV ), 1, VL( J ), 1, C, S )
            K2 = K2 - 1
            IDXP( K2 ) = JPREV
            JPREV = J
         ELSE
            K = K + 1
            ZW( K ) = Z( JPREV )
            DSIGMA( K ) = D( JPREV )
            IDXP( K ) = JPREV
            JPREV = J
         END IF
      END IF
      GO TO 80
   90 CONTINUE
*
*     Record the last singular value.
*
      K = K + 1
      ZW( K ) = Z( JPREV )
      DSIGMA( K ) = D( JPREV )
      IDXP( K ) = JPREV
*
  100 CONTINUE
*
*     Sort the singular values into DSIGMA. The singular values which
*     were not deflated go into the first K slots of DSIGMA, except
*     that DSIGMA(1) is treated separately.
*
      DO 110 J = 2, N
         JP = IDXP( J )
         DSIGMA( J ) = D( JP )
         VFW( J ) = VF( JP )
         VLW( J ) = VL( JP )
  110 CONTINUE
      IF( ICOMPQ.EQ.1 ) THEN
         DO 120 J = 2, N
            JP = IDXP( J )
            PERM( J ) = IDXQ( IDX( JP )+1 )
            IF( PERM( J ).LE.NLP1 ) THEN
               PERM( J ) = PERM( J ) - 1
            END IF
  120    CONTINUE
      END IF
*
*     The deflated singular values go back into the last N - K slots of
*     D.
*
      CALL DCOPY( N-K, DSIGMA( K+1 ), 1, D( K+1 ), 1 )
*
*     Determine DSIGMA(1), DSIGMA(2), Z(1), VF(1), VL(1), VF(M), and
*     VL(M).
*
      OPS = OPS + DBLE( 1 )
      DSIGMA( 1 ) = ZERO
      HLFTOL = TOL / TWO
      IF( ABS( DSIGMA( 2 ) ).LE.HLFTOL )
     $   DSIGMA( 2 ) = HLFTOL
      IF( M.GT.N ) THEN
         OPS = OPS + DBLE( 5 )
         Z( 1 ) = DLAPY2( Z1, Z( M ) )
         IF( Z( 1 ).LE.TOL ) THEN
            C = ONE
            S = ZERO
            Z( 1 ) = TOL
         ELSE
            OPS = OPS + DBLE( 2 )
            C = Z1 / Z( 1 )
            S = -Z( M ) / Z( 1 )
         END IF
         OPS = OPS + DBLE( 12 )
         CALL DROT( 1, VF( M ), 1, VF( 1 ), 1, C, S )
         CALL DROT( 1, VL( M ), 1, VL( 1 ), 1, C, S )
      ELSE
         IF( ABS( Z1 ).LE.TOL ) THEN
            Z( 1 ) = TOL
         ELSE
            Z( 1 ) = Z1
         END IF
      END IF
*
*     Restore Z, VF, and VL.
*
      CALL DCOPY( K-1, ZW( 2 ), 1, Z( 2 ), 1 )
      CALL DCOPY( N-1, VFW( 2 ), 1, VF( 2 ), 1 )
      CALL DCOPY( N-1, VLW( 2 ), 1, VL( 2 ), 1 )
*
      RETURN
*
*     End of DLASD7
*
      END
      SUBROUTINE DLASD8( ICOMPQ, K, D, Z, VF, VL, DIFL, DIFR, LDDIFR,
     $                   DSIGMA, WORK, INFO )
*
*  -- LAPACK auxiliary routine (instrumented to count ops, version 3.0) --
*     Univ. of Tennessee, Oak Ridge National Lab, Argonne National Lab,
*     Courant Institute, NAG Ltd., and Rice University
*     June 30, 1999
*
*     .. Scalar Arguments ..
      INTEGER            ICOMPQ, INFO, K, LDDIFR
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   D( * ), DIFL( * ), DIFR( LDDIFR, * ),
     $                   DSIGMA( * ), VF( * ), VL( * ), WORK( * ),
     $                   Z( * )
*     ..
*     .. Common block to return operation count ..
      COMMON             / LATIME / OPS, ITCNT
*     ..
*     .. Scalars in Common ..
      DOUBLE PRECISION   ITCNT, OPS
*     ..
*
*  Purpose
*  =======
*
*  DLASD8 finds the square roots of the roots of the secular equation,
*  as defined by the values in DSIGMA and Z. It makes the appropriate
*  calls to DLASD4, and stores, for each  element in D, the distance
*  to its two nearest poles (elements in DSIGMA). It also updates
*  the arrays VF and VL, the first and last components of all the
*  right singular vectors of the original bidiagonal matrix.
*
*  DLASD8 is called from DLASD6.
*
*  Arguments
*  =========
*
*  ICOMPQ  (input) INTEGER
*          Specifies whether singular vectors are to be computed in
*          factored form in the calling routine:
*          = 0: Compute singular values only.
*          = 1: Compute singular vectors in factored form as well.
*
*  K       (input) INTEGER
*          The number of terms in the rational function to be solved
*          by DLASD4.  K >= 1.
*
*  D       (output) DOUBLE PRECISION array, dimension ( K )
*          On output, D contains the updated singular values.
*
*  Z       (input) DOUBLE PRECISION array, dimension ( K )
*          The first K elements of this array contain the components
*          of the deflation-adjusted updating row vector.
*
*  VF      (input/output) DOUBLE PRECISION array, dimension ( K )
*          On entry, VF contains  information passed through DBEDE8.
*          On exit, VF contains the first K components of the first
*          components of all right singular vectors of the bidiagonal
*          matrix.
*
*  VL      (input/output) DOUBLE PRECISION array, dimension ( K )
*          On entry, VL contains  information passed through DBEDE8.
*          On exit, VL contains the first K components of the last
*          components of all right singular vectors of the bidiagonal
*          matrix.
*
*  DIFL    (output) DOUBLE PRECISION array, dimension ( K )
*          On exit, DIFL(I) = D(I) - DSIGMA(I).
*
*  DIFR    (output) DOUBLE PRECISION array,
*                   dimension ( LDDIFR, 2 ) if ICOMPQ = 1 and
*                   dimension ( K ) if ICOMPQ = 0.
*          On exit, DIFR(I,1) = D(I) - DSIGMA(I+1), DIFR(K,1) is not
*          defined and will not be referenced.
*
*          If ICOMPQ = 1, DIFR(1:K,2) is an array containing the
*          normalizing factors for the right singular vector matrix.
*
*  LDDIFR  (input) INTEGER
*          The leading dimension of DIFR, must be at least K.
*
*  DSIGMA  (input) DOUBLE PRECISION array, dimension ( K )
*          The first K elements of this array contain the old roots
*          of the deflated updating problem.  These are the poles
*          of the secular equation.
*
*  WORK    (workspace) DOUBLE PRECISION array, dimension at least 3 * K
*
*  INFO    (output) INTEGER
*          = 0:  successful exit.
*          < 0:  if INFO = -i, the i-th argument had an illegal value.
*          > 0:  if INFO = 1, an singular value did not converge
*
*  Further Details
*  ===============
*
*  Based on contributions by
*     Ming Gu and Huan Ren, Computer Science Division, University of
*     California at Berkeley, USA
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ONE
      PARAMETER          ( ONE = 1.0D0 )
*     ..
*     .. Local Scalars ..
      INTEGER            I, IWK1, IWK2, IWK2I, IWK3, IWK3I, J
      DOUBLE PRECISION   DIFLJ, DIFRJ, DJ, DSIGJ, DSIGJP, RHO, TEMP
*     ..
*     .. External Subroutines ..
      EXTERNAL           DCOPY, DLASCL, DLASD4, DLASET, XERBLA
*     ..
*     .. External Functions ..
      DOUBLE PRECISION   DDOT, DLAMC3, DNRM2
      EXTERNAL           DDOT, DLAMC3, DNRM2
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          DBLE, ABS, SIGN, SQRT
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters.
*
      INFO = 0
*
      IF( ( ICOMPQ.LT.0 ) .OR. ( ICOMPQ.GT.1 ) ) THEN
         INFO = -1
      ELSE IF( K.LT.1 ) THEN
         INFO = -2
      ELSE IF( LDDIFR.LT.K ) THEN
         INFO = -9
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DLASD8', -INFO )
         RETURN
      END IF
*
*     Quick return if possible
*
      IF( K.EQ.1 ) THEN
         D( 1 ) = ABS( Z( 1 ) )
         DIFL( 1 ) = D( 1 )
         IF( ICOMPQ.EQ.1 ) THEN
            DIFL( 2 ) = ONE
            DIFR( 1, 2 ) = ONE
         END IF
         RETURN
      END IF
*
*     Modify values DSIGMA(i) to make sure all DSIGMA(i)-DSIGMA(j) can
*     be computed with high relative accuracy (barring over/underflow).
*     This is a problem on machines without a guard digit in
*     add/subtract (Cray XMP, Cray YMP, Cray C 90 and Cray 2).
*     The following code replaces DSIGMA(I) by 2*DSIGMA(I)-DSIGMA(I),
*     which on any of these machines zeros out the bottommost
*     bit of DSIGMA(I) if it is 1; this makes the subsequent
*     subtractions DSIGMA(I)-DSIGMA(J) unproblematic when cancellation
*     occurs. On binary machines with a guard digit (almost all
*     machines) it does not change DSIGMA(I) at all. On hexadecimal
*     and decimal machines with a guard digit, it slightly
*     changes the bottommost bits of DSIGMA(I). It does not account
*     for hexadecimal or decimal machines without guard digits
*     (we know of none). We use a subroutine call to compute
*     2*DLAMBDA(I) to prevent optimizing compilers from eliminating
*     this code.
*
      OPS = OPS + DBLE( 2*K )
      DO 10 I = 1, K
         DSIGMA( I ) = DLAMC3( DSIGMA( I ), DSIGMA( I ) ) - DSIGMA( I )
   10 CONTINUE
*
*     Book keeping.
*
      IWK1 = 1
      IWK2 = IWK1 + K
      IWK3 = IWK2 + K
      IWK2I = IWK2 - 1
      IWK3I = IWK3 - 1
*
*     Normalize Z.
*
      OPS = OPS + DBLE( 3*K + 1 )
      RHO = DNRM2( K, Z, 1 )
      CALL DLASCL( 'G', 0, 0, RHO, ONE, K, 1, Z, K, INFO )
      RHO = RHO*RHO
*
*     Initialize WORK(IWK3).
*
      CALL DLASET( 'A', K, 1, ONE, ONE, WORK( IWK3 ), K )
*
*     Compute the updated singular values, the arrays DIFL, DIFR,
*     and the updated Z.
*
      DO 40 J = 1, K
         CALL DLASD4( K, J, DSIGMA, Z, WORK( IWK1 ), RHO, D( J ),
     $                WORK( IWK2 ), INFO )
*
*        If the root finder fails, the computation is terminated.
*
         IF( INFO.NE.0 ) THEN
            RETURN
         END IF
         OPS = OPS + DBLE( 2 )
         WORK( IWK3I+J ) = WORK( IWK3I+J )*WORK( J )*WORK( IWK2I+J )
         DIFL( J ) = -WORK( J )
         DIFR( J, 1 ) = -WORK( J+1 )
         OPS = OPS + DBLE( 6*( J - 1 ) )
         DO 20 I = 1, J - 1
            WORK( IWK3I+I ) = WORK( IWK3I+I )*WORK( I )*
     $                        WORK( IWK2I+I ) / ( DSIGMA( I )-
     $                        DSIGMA( J ) ) / ( DSIGMA( I )+
     $                        DSIGMA( J ) )
   20    CONTINUE
         OPS = OPS + DBLE( 6*( K-J ) )
         DO 30 I = J + 1, K
            WORK( IWK3I+I ) = WORK( IWK3I+I )*WORK( I )*
     $                        WORK( IWK2I+I ) / ( DSIGMA( I )-
     $                        DSIGMA( J ) ) / ( DSIGMA( I )+
     $                        DSIGMA( J ) )
   30    CONTINUE
   40 CONTINUE
*
*     Compute updated Z.
*
      OPS = OPS + DBLE( K )
      DO 50 I = 1, K
         Z( I ) = SIGN( SQRT( ABS( WORK( IWK3I+I ) ) ), Z( I ) )
   50 CONTINUE
*
*     Update VF and VL.
*
      DO 80 J = 1, K
         DIFLJ = DIFL( J )
         DJ = D( J )
         DSIGJ = -DSIGMA( J )
         IF( J.LT.K ) THEN
            DIFRJ = -DIFR( J, 1 )
            DSIGJP = -DSIGMA( J+1 )
         END IF
         OPS = OPS + DBLE( 3 )
         WORK( J ) = -Z( J ) / DIFLJ / ( DSIGMA( J )+DJ )
         OPS = OPS + DBLE( 5*( J-1 ) )
         DO 60 I = 1, J - 1
            WORK( I ) = Z( I ) / ( DLAMC3( DSIGMA( I ), DSIGJ )-DIFLJ )
     $                   / ( DSIGMA( I )+DJ )
   60    CONTINUE
         OPS = OPS + DBLE( 5*( K-J ) )
         DO 70 I = J + 1, K
            WORK( I ) = Z( I ) / ( DLAMC3( DSIGMA( I ), DSIGJP )+DIFRJ )
     $                   / ( DSIGMA( I )+DJ )
   70    CONTINUE
         OPS = OPS + DBLE( 6*K )
         TEMP = DNRM2( K, WORK, 1 )
         WORK( IWK2I+J ) = DDOT( K, WORK, 1, VF, 1 ) / TEMP
         WORK( IWK3I+J ) = DDOT( K, WORK, 1, VL, 1 ) / TEMP
         IF( ICOMPQ.EQ.1 ) THEN
            DIFR( J, 2 ) = TEMP
         END IF
   80 CONTINUE
*
      CALL DCOPY( K, WORK( IWK2 ), 1, VF, 1 )
      CALL DCOPY( K, WORK( IWK3 ), 1, VL, 1 )
*
      RETURN
*
*     End of DLASD8
*
      END
      SUBROUTINE DLASDA( ICOMPQ, SMLSIZ, N, SQRE, D, E, U, LDU, VT, K,
     $                   DIFL, DIFR, Z, POLES, GIVPTR, GIVCOL, LDGCOL,
     $                   PERM, GIVNUM, C, S, WORK, IWORK, INFO )
*
*  -- LAPACK auxiliary routine (instrumented to count ops, version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     June 30, 1999
*
*     .. Scalar Arguments ..
      INTEGER            ICOMPQ, INFO, LDGCOL, LDU, N, SMLSIZ, SQRE
*     ..
*     .. Array Arguments ..
      INTEGER            GIVCOL( LDGCOL, * ), GIVPTR( * ), IWORK( * ),
     $                   K( * ), PERM( LDGCOL, * )
      DOUBLE PRECISION   C( * ), D( * ), DIFL( LDU, * ), DIFR( LDU, * ),
     $                   E( * ), GIVNUM( LDU, * ), POLES( LDU, * ),
     $                   S( * ), U( LDU, * ), VT( LDU, * ), WORK( * ),
     $                   Z( LDU, * )
*     ..
*     .. Common block to return operation count ..
      COMMON             / LATIME / OPS, ITCNT
*     ..
*     .. Scalars in Common ..
      DOUBLE PRECISION   ITCNT, OPS
*     ..
*
*  Purpose
*  =======
*
*  Using a divide and conquer approach, DLASDA computes the singular
*  value decomposition (SVD) of a real upper bidiagonal N-by-M matrix
*  B with diagonal D and offdiagonal E, where M = N + SQRE. The
*  algorithm computes the singular values in the SVD B = U * S * VT.
*  The orthogonal matrices U and VT are optionally computed in
*  compact form.
*
*  A related subroutine, DLASD0, computes the singular values and
*  the singular vectors in explicit form.
*
*  Arguments
*  =========
*
*  ICOMPQ (input) INTEGER
*         Specifies whether singular vectors are to be computed
*         in compact form, as follows
*         = 0: Compute singular values only.
*         = 1: Compute singular vectors of upper bidiagonal
*              matrix in compact form.
*
*  SMLSIZ (input) INTEGER
*         The maximum size of the subproblems at the bottom of the
*         computation tree.
*
*  N      (input) INTEGER
*         The row dimension of the upper bidiagonal matrix. This is
*         also the dimension of the main diagonal array D.
*
*  SQRE   (input) INTEGER
*         Specifies the column dimension of the bidiagonal matrix.
*         = 0: The bidiagonal matrix has column dimension M = N;
*         = 1: The bidiagonal matrix has column dimension M = N + 1.
*
*  D      (input/output) DOUBLE PRECISION array, dimension ( N )
*         On entry D contains the main diagonal of the bidiagonal
*         matrix. On exit D, if INFO = 0, contains its singular values.
*
*  E      (input) DOUBLE PRECISION array, dimension ( M-1 )
*         Contains the subdiagonal entries of the bidiagonal matrix.
*         On exit, E has been destroyed.
*
*  U      (output) DOUBLE PRECISION array,
*         dimension ( LDU, SMLSIZ ) if ICOMPQ = 1, and not referenced
*         if ICOMPQ = 0. If ICOMPQ = 1, on exit, U contains the left
*         singular vector matrices of all subproblems at the bottom
*         level.
*
*  LDU    (input) INTEGER, LDU = > N.
*         The leading dimension of arrays U, VT, DIFL, DIFR, POLES,
*         GIVNUM, and Z.
*
*  VT     (output) DOUBLE PRECISION array,
*         dimension ( LDU, SMLSIZ+1 ) if ICOMPQ = 1, and not referenced
*         if ICOMPQ = 0. If ICOMPQ = 1, on exit, VT' contains the right
*         singular vector matrices of all subproblems at the bottom
*         level.
*
*  K      (output) INTEGER array,
*         dimension ( N ) if ICOMPQ = 1 and dimension 1 if ICOMPQ = 0.
*         If ICOMPQ = 1, on exit, K(I) is the dimension of the I-th
*         secular equation on the computation tree.
*
*  DIFL   (output) DOUBLE PRECISION array, dimension ( LDU, NLVL ),
*         where NLVL = floor(log_2 (N/SMLSIZ))).
*
*  DIFR   (output) DOUBLE PRECISION array,
*                  dimension ( LDU, 2 * NLVL ) if ICOMPQ = 1 and
*                  dimension ( N ) if ICOMPQ = 0.
*         If ICOMPQ = 1, on exit, DIFL(1:N, I) and DIFR(1:N, 2 * I - 1)
*         record distances between singular values on the I-th
*         level and singular values on the (I -1)-th level, and
*         DIFR(1:N, 2 * I ) contains the normalizing factors for
*         the right singular vector matrix. See DLASD8 for details.
*
*  Z      (output) DOUBLE PRECISION array,
*                  dimension ( LDU, NLVL ) if ICOMPQ = 1 and
*                  dimension ( N ) if ICOMPQ = 0.
*         The first K elements of Z(1, I) contain the components of
*         the deflation-adjusted updating row vector for subproblems
*         on the I-th level.
*
*  POLES  (output) DOUBLE PRECISION array,
*         dimension ( LDU, 2 * NLVL ) if ICOMPQ = 1, and not referenced
*         if ICOMPQ = 0. If ICOMPQ = 1, on exit, POLES(1, 2*I - 1) and
*         POLES(1, 2*I) contain  the new and old singular values
*         involved in the secular equations on the I-th level.
*
*  GIVPTR (output) INTEGER array,
*         dimension ( N ) if ICOMPQ = 1, and not referenced if
*         ICOMPQ = 0. If ICOMPQ = 1, on exit, GIVPTR( I ) records
*         the number of Givens rotations performed on the I-th
*         problem on the computation tree.
*
*  GIVCOL (output) INTEGER array,
*         dimension ( LDGCOL, 2 * NLVL ) if ICOMPQ = 1, and not
*         referenced if ICOMPQ = 0. If ICOMPQ = 1, on exit, for each I,
*         GIVCOL(1, 2 *I - 1) and GIVCOL(1, 2 *I) record the locations
*         of Givens rotations performed on the I-th level on the
*         computation tree.
*
*  LDGCOL (input) INTEGER, LDGCOL = > N.
*         The leading dimension of arrays GIVCOL and PERM.
*
*  PERM   (output) INTEGER array,
*         dimension ( LDGCOL, NLVL ) if ICOMPQ = 1, and not referenced
*         if ICOMPQ = 0. If ICOMPQ = 1, on exit, PERM(1, I) records
*         permutations done on the I-th level of the computation tree.
*
*  GIVNUM (output) DOUBLE PRECISION array,
*         dimension ( LDU,  2 * NLVL ) if ICOMPQ = 1, and not
*         referenced if ICOMPQ = 0. If ICOMPQ = 1, on exit, for each I,
*         GIVNUM(1, 2 *I - 1) and GIVNUM(1, 2 *I) record the C- and S-
*         values of Givens rotations performed on the I-th level on
*         the computation tree.
*
*  C      (output) DOUBLE PRECISION array,
*         dimension ( N ) if ICOMPQ = 1, and dimension 1 if ICOMPQ = 0.
*         If ICOMPQ = 1 and the I-th subproblem is not square, on exit,
*         C( I ) contains the C-value of a Givens rotation related to
*         the right null space of the I-th subproblem.
*
*  S      (output) DOUBLE PRECISION array, dimension ( N ) if
*         ICOMPQ = 1, and dimension 1 if ICOMPQ = 0. If ICOMPQ = 1
*         and the I-th subproblem is not square, on exit, S( I )
*         contains the S-value of a Givens rotation related to
*         the right null space of the I-th subproblem.
*
*  WORK   (workspace) DOUBLE PRECISION array
*         If ICOMPQ = 0 its dimension must be at least
*         (2 * N + max(4 * N, (SMLSIZ + 4)*(SMLSIZ + 1))).
*         and if ICOMPQ = 1, dimension must be at least (6 * N).
*
*  IWORK  (workspace) INTEGER array.
*         Dimension must be at least (7 * N).
*
*  INFO   (output) INTEGER
*          = 0:  successful exit.
*          < 0:  if INFO = -i, the i-th argument had an illegal value.
*          > 0:  if INFO = 1, an singular value did not converge
*
*  Further Details
*  ===============
*
*  Based on contributions by
*     Ming Gu and Huan Ren, Computer Science Division, University of
*     California at Berkeley, USA
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE
      PARAMETER          ( ZERO = 0.0D0, ONE = 1.0D0 )
*     ..
*     .. Local Scalars ..
      INTEGER            I, I1, IC, IDXQ, IDXQI, IM1, INODE, ITEMP, IWK,
     $                   J, LF, LL, LVL, LVL2, M, NCC, ND, NDB1, NDIML,
     $                   NDIMR, NL, NLF, NLP1, NLVL, NR, NRF, NRP1, NRU,
     $                   NWORK1, NWORK2, SMLSZP, SQREI, VF, VFI, VL, VLI
      DOUBLE PRECISION   ALPHA, BETA
*     ..
*     .. External Subroutines ..
      EXTERNAL           DCOPY, DLASD6, DLASDQ, DLASDT, DLASET, XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          DBLE 
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters.
*
      INFO = 0
*
      IF( ( ICOMPQ.LT.0 ) .OR. ( ICOMPQ.GT.1 ) ) THEN
         INFO = -1
      ELSE IF( SMLSIZ.LT.3 ) THEN
         INFO = -2
      ELSE IF( N.LT.0 ) THEN
         INFO = -3
      ELSE IF( ( SQRE.LT.0 ) .OR. ( SQRE.GT.1 ) ) THEN
         INFO = -4
      ELSE IF( LDU.LT.( N+SQRE ) ) THEN
         INFO = -8
      ELSE IF( LDGCOL.LT.N ) THEN
         INFO = -17
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DLASDA', -INFO )
         RETURN
      END IF
*
      M = N + SQRE
*
*     If the input matrix is too small, call DLASDQ to find the SVD.
*
      IF( N.LE.SMLSIZ ) THEN
         IF( ICOMPQ.EQ.0 ) THEN
            CALL DLASDQ( 'U', SQRE, N, 0, 0, 0, D, E, VT, LDU, U, LDU,
     $                   U, LDU, WORK, INFO )
         ELSE
            CALL DLASDQ( 'U', SQRE, N, M, N, 0, D, E, VT, LDU, U, LDU,
     $                   U, LDU, WORK, INFO )
         END IF
         RETURN
      END IF
*
*     Book-keeping and  set up the computation tree.
*
      INODE = 1
      NDIML = INODE + N
      NDIMR = NDIML + N
      IDXQ = NDIMR + N
      IWK = IDXQ + N
*
      NCC = 0
      NRU = 0
*
      SMLSZP = SMLSIZ + 1
      VF = 1
      VL = VF + M
      NWORK1 = VL + M
      NWORK2 = NWORK1 + SMLSZP*SMLSZP
*
      CALL DLASDT( N, NLVL, ND, IWORK( INODE ), IWORK( NDIML ),
     $             IWORK( NDIMR ), SMLSIZ )
*
*     for the nodes on bottom level of the tree, solve
*     their subproblems by DLASDQ.
*
      OPS = OPS + DBLE( 1 )
      NDB1 = ( ND+1 ) / 2
      DO 30 I = NDB1, ND
*
*        IC : center row of each node
*        NL : number of rows of left  subproblem
*        NR : number of rows of right subproblem
*        NLF: starting row of the left   subproblem
*        NRF: starting row of the right  subproblem
*
         I1 = I - 1
         IC = IWORK( INODE+I1 )
         NL = IWORK( NDIML+I1 )
         NLP1 = NL + 1
         NR = IWORK( NDIMR+I1 )
         NLF = IC - NL
         NRF = IC + 1
         IDXQI = IDXQ + NLF - 2
         VFI = VF + NLF - 1
         VLI = VL + NLF - 1
         SQREI = 1
         IF( ICOMPQ.EQ.0 ) THEN
            CALL DLASET( 'A', NLP1, NLP1, ZERO, ONE, WORK( NWORK1 ),
     $                   SMLSZP )
            CALL DLASDQ( 'U', SQREI, NL, NLP1, NRU, NCC, D( NLF ),
     $                   E( NLF ), WORK( NWORK1 ), SMLSZP,
     $                   WORK( NWORK2 ), NL, WORK( NWORK2 ), NL,
     $                   WORK( NWORK2 ), INFO )
            ITEMP = NWORK1 + NL*SMLSZP
            CALL DCOPY( NLP1, WORK( NWORK1 ), 1, WORK( VFI ), 1 )
            CALL DCOPY( NLP1, WORK( ITEMP ), 1, WORK( VLI ), 1 )
         ELSE
            CALL DLASET( 'A', NL, NL, ZERO, ONE, U( NLF, 1 ), LDU )
            CALL DLASET( 'A', NLP1, NLP1, ZERO, ONE, VT( NLF, 1 ), LDU )
            CALL DLASDQ( 'U', SQREI, NL, NLP1, NL, NCC, D( NLF ),
     $                   E( NLF ), VT( NLF, 1 ), LDU, U( NLF, 1 ), LDU,
     $                   U( NLF, 1 ), LDU, WORK( NWORK1 ), INFO )
            CALL DCOPY( NLP1, VT( NLF, 1 ), 1, WORK( VFI ), 1 )
            CALL DCOPY( NLP1, VT( NLF, NLP1 ), 1, WORK( VLI ), 1 )
         END IF
         IF( INFO.NE.0 ) THEN
            RETURN
         END IF
         DO 10 J = 1, NL
            IWORK( IDXQI+J ) = J
   10    CONTINUE
         IF( ( I.EQ.ND ) .AND. ( SQRE.EQ.0 ) ) THEN
            SQREI = 0
         ELSE
            SQREI = 1
         END IF
         IDXQI = IDXQI + NLP1
         VFI = VFI + NLP1
         VLI = VLI + NLP1
         NRP1 = NR + SQREI
         IF( ICOMPQ.EQ.0 ) THEN
            CALL DLASET( 'A', NRP1, NRP1, ZERO, ONE, WORK( NWORK1 ),
     $                   SMLSZP )
            CALL DLASDQ( 'U', SQREI, NR, NRP1, NRU, NCC, D( NRF ),
     $                   E( NRF ), WORK( NWORK1 ), SMLSZP,
     $                   WORK( NWORK2 ), NR, WORK( NWORK2 ), NR,
     $                   WORK( NWORK2 ), INFO )
            ITEMP = NWORK1 + ( NRP1-1 )*SMLSZP
            CALL DCOPY( NRP1, WORK( NWORK1 ), 1, WORK( VFI ), 1 )
            CALL DCOPY( NRP1, WORK( ITEMP ), 1, WORK( VLI ), 1 )
         ELSE
            CALL DLASET( 'A', NR, NR, ZERO, ONE, U( NRF, 1 ), LDU )
            CALL DLASET( 'A', NRP1, NRP1, ZERO, ONE, VT( NRF, 1 ), LDU )
            CALL DLASDQ( 'U', SQREI, NR, NRP1, NR, NCC, D( NRF ),
     $                   E( NRF ), VT( NRF, 1 ), LDU, U( NRF, 1 ), LDU,
     $                   U( NRF, 1 ), LDU, WORK( NWORK1 ), INFO )
            CALL DCOPY( NRP1, VT( NRF, 1 ), 1, WORK( VFI ), 1 )
            CALL DCOPY( NRP1, VT( NRF, NRP1 ), 1, WORK( VLI ), 1 )
         END IF
         IF( INFO.NE.0 ) THEN
            RETURN
         END IF
         DO 20 J = 1, NR
            IWORK( IDXQI+J ) = J
   20    CONTINUE
   30 CONTINUE
*
*     Now conquer each subproblem bottom-up.
*
      J = 2**NLVL
      DO 50 LVL = NLVL, 1, -1
         LVL2 = LVL*2 - 1
*
*        Find the first node LF and last node LL on
*        the current level LVL.
*
         IF( LVL.EQ.1 ) THEN
            LF = 1
            LL = 1
         ELSE
            LF = 2**( LVL-1 )
            LL = 2*LF - 1
         END IF
         DO 40 I = LF, LL
            IM1 = I - 1
            IC = IWORK( INODE+IM1 )
            NL = IWORK( NDIML+IM1 )
            NR = IWORK( NDIMR+IM1 )
            NLF = IC - NL
            NRF = IC + 1
            IF( I.EQ.LL ) THEN
               SQREI = SQRE
            ELSE
               SQREI = 1
            END IF
            VFI = VF + NLF - 1
            VLI = VL + NLF - 1
            IDXQI = IDXQ + NLF - 1
            ALPHA = D( IC )
            BETA = E( IC )
            IF( ICOMPQ.EQ.0 ) THEN
               CALL DLASD6( ICOMPQ, NL, NR, SQREI, D( NLF ),
     $                      WORK( VFI ), WORK( VLI ), ALPHA, BETA,
     $                      IWORK( IDXQI ), PERM, GIVPTR( 1 ), GIVCOL,
     $                      LDGCOL, GIVNUM, LDU, POLES, DIFL, DIFR, Z,
     $                      K( 1 ), C( 1 ), S( 1 ), WORK( NWORK1 ),
     $                      IWORK( IWK ), INFO )
            ELSE
               J = J - 1
               CALL DLASD6( ICOMPQ, NL, NR, SQREI, D( NLF ),
     $                      WORK( VFI ), WORK( VLI ), ALPHA, BETA,
     $                      IWORK( IDXQI ), PERM( NLF, LVL ),
     $                      GIVPTR( J ), GIVCOL( NLF, LVL2 ), LDGCOL,
     $                      GIVNUM( NLF, LVL2 ), LDU,
     $                      POLES( NLF, LVL2 ), DIFL( NLF, LVL ),
     $                      DIFR( NLF, LVL2 ), Z( NLF, LVL ), K( J ),
     $                      C( J ), S( J ), WORK( NWORK1 ),
     $                      IWORK( IWK ), INFO )
            END IF
            IF( INFO.NE.0 ) THEN
               RETURN
            END IF
   40    CONTINUE
   50 CONTINUE
*
      RETURN
*
*     End of DLASDA
*
      END
      SUBROUTINE DLASDQ( UPLO, SQRE, N, NCVT, NRU, NCC, D, E, VT, LDVT,
     $                   U, LDU, C, LDC, WORK, INFO )
*
*  -- LAPACK auxiliary routine (instrumented to count ops, version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     October 31, 1999
*
*     .. Scalar Arguments ..
      CHARACTER          UPLO
      INTEGER            INFO, LDC, LDU, LDVT, N, NCC, NCVT, NRU, SQRE
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   C( LDC, * ), D( * ), E( * ), U( LDU, * ),
     $                   VT( LDVT, * ), WORK( * )
*     ..
*     .. Common block to return operation count ..
      COMMON             / LATIME / OPS, ITCNT
*     ..
*     .. Scalars in Common ..
      DOUBLE PRECISION   ITCNT, OPS
*     ..
*
*  Purpose
*  =======
*
*  DLASDQ computes the singular value decomposition (SVD) of a real
*  (upper or lower) bidiagonal matrix with diagonal D and offdiagonal
*  E, accumulating the transformations if desired. Letting B denote
*  the input bidiagonal matrix, the algorithm computes orthogonal
*  matrices Q and P such that B = Q * S * P' (P' denotes the transpose
*  of P). The singular values S are overwritten on D.
*
*  The input matrix U  is changed to U  * Q  if desired.
*  The input matrix VT is changed to P' * VT if desired.
*  The input matrix C  is changed to Q' * C  if desired.
*
*  See "Computing  Small Singular Values of Bidiagonal Matrices With
*  Guaranteed High Relative Accuracy," by J. Demmel and W. Kahan,
*  LAPACK Working Note #3, for a detailed description of the algorithm.
*
*  Arguments
*  =========
*
*  UPLO  (input) CHARACTER*1
*        On entry, UPLO specifies whether the input bidiagonal matrix
*        is upper or lower bidiagonal, and wether it is square are
*        not.
*           UPLO = 'U' or 'u'   B is upper bidiagonal.
*           UPLO = 'L' or 'l'   B is lower bidiagonal.
*
*  SQRE  (input) INTEGER
*        = 0: then the input matrix is N-by-N.
*        = 1: then the input matrix is N-by-(N+1) if UPLU = 'U' and
*             (N+1)-by-N if UPLU = 'L'.
*
*        The bidiagonal matrix has
*        N = NL + NR + 1 rows and
*        M = N + SQRE >= N columns.
*
*  N     (input) INTEGER
*        On entry, N specifies the number of rows and columns
*        in the matrix. N must be at least 0.
*
*  NCVT  (input) INTEGER
*        On entry, NCVT specifies the number of columns of
*        the matrix VT. NCVT must be at least 0.
*
*  NRU   (input) INTEGER
*        On entry, NRU specifies the number of rows of
*        the matrix U. NRU must be at least 0.
*
*  NCC   (input) INTEGER
*        On entry, NCC specifies the number of columns of
*        the matrix C. NCC must be at least 0.
*
*  D     (input/output) DOUBLE PRECISION array, dimension (N)
*        On entry, D contains the diagonal entries of the
*        bidiagonal matrix whose SVD is desired. On normal exit,
*        D contains the singular values in ascending order.
*
*  E     (input/output) DOUBLE PRECISION array.
*        dimension is (N-1) if SQRE = 0 and N if SQRE = 1.
*        On entry, the entries of E contain the offdiagonal entries
*        of the bidiagonal matrix whose SVD is desired. On normal
*        exit, E will contain 0. If the algorithm does not converge,
*        D and E will contain the diagonal and superdiagonal entries
*        of a bidiagonal matrix orthogonally equivalent to the one
*        given as input.
*
*  VT    (input/output) DOUBLE PRECISION array, dimension (LDVT, NCVT)
*        On entry, contains a matrix which on exit has been
*        premultiplied by P', dimension N-by-NCVT if SQRE = 0
*        and (N+1)-by-NCVT if SQRE = 1 (not referenced if NCVT=0).
*
*  LDVT  (input) INTEGER
*        On entry, LDVT specifies the leading dimension of VT as
*        declared in the calling (sub) program. LDVT must be at
*        least 1. If NCVT is nonzero LDVT must also be at least N.
*
*  U     (input/output) DOUBLE PRECISION array, dimension (LDU, N)
*        On entry, contains a  matrix which on exit has been
*        postmultiplied by Q, dimension NRU-by-N if SQRE = 0
*        and NRU-by-(N+1) if SQRE = 1 (not referenced if NRU=0).
*
*  LDU   (input) INTEGER
*        On entry, LDU  specifies the leading dimension of U as
*        declared in the calling (sub) program. LDU must be at
*        least max( 1, NRU ) .
*
*  C     (input/output) DOUBLE PRECISION array, dimension (LDC, NCC)
*        On entry, contains an N-by-NCC matrix which on exit
*        has been premultiplied by Q'  dimension N-by-NCC if SQRE = 0
*        and (N+1)-by-NCC if SQRE = 1 (not referenced if NCC=0).
*
*  LDC   (input) INTEGER
*        On entry, LDC  specifies the leading dimension of C as
*        declared in the calling (sub) program. LDC must be at
*        least 1. If NCC is nonzero, LDC must also be at least N.
*
*  WORK  (workspace) DOUBLE PRECISION array, dimension (MAX( 1, 4*N ))
*        Workspace. Only referenced if one of NCVT, NRU, or NCC is
*        nonzero, and if N is at least 2.
*
*  INFO  (output) INTEGER
*        On exit, a value of 0 indicates a successful exit.
*        If INFO < 0, argument number -INFO is illegal.
*        If INFO > 0, the algorithm did not converge, and INFO
*        specifies how many superdiagonals did not converge.
*
*  Further Details
*  ===============
*
*  Based on contributions by
*     Ming Gu and Huan Ren, Computer Science Division, University of
*     California at Berkeley, USA
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO
      PARAMETER          ( ZERO = 0.0D0 )
*     ..
*     .. Local Scalars ..
      LOGICAL            ROTATE
      INTEGER            I, ISUB, IUPLO, J, NP1, SQRE1
      DOUBLE PRECISION   CS, R, SMIN, SN
*     ..
*     .. External Subroutines ..
      EXTERNAL           DBDSQR, DLARTG, DLASR, DSWAP, XERBLA
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          DBLE, MAX
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters.
*
      INFO = 0
      IUPLO = 0
      IF( LSAME( UPLO, 'U' ) )
     $   IUPLO = 1
      IF( LSAME( UPLO, 'L' ) )
     $   IUPLO = 2
      IF( IUPLO.EQ.0 ) THEN
         INFO = -1
      ELSE IF( ( SQRE.LT.0 ) .OR. ( SQRE.GT.1 ) ) THEN
         INFO = -2
      ELSE IF( N.LT.0 ) THEN
         INFO = -3
      ELSE IF( NCVT.LT.0 ) THEN
         INFO = -4
      ELSE IF( NRU.LT.0 ) THEN
         INFO = -5
      ELSE IF( NCC.LT.0 ) THEN
         INFO = -6
      ELSE IF( ( NCVT.EQ.0 .AND. LDVT.LT.1 ) .OR.
     $         ( NCVT.GT.0 .AND. LDVT.LT.MAX( 1, N ) ) ) THEN
         INFO = -10
      ELSE IF( LDU.LT.MAX( 1, NRU ) ) THEN
         INFO = -12
      ELSE IF( ( NCC.EQ.0 .AND. LDC.LT.1 ) .OR.
     $         ( NCC.GT.0 .AND. LDC.LT.MAX( 1, N ) ) ) THEN
         INFO = -14
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DBDSQR', -INFO )
         RETURN
      END IF
      IF( N.EQ.0 )
     $   RETURN
*
*     ROTATE is true if any singular vectors desired, false otherwise
*
      ROTATE = ( NCVT.GT.0 ) .OR. ( NRU.GT.0 ) .OR. ( NCC.GT.0 )
      NP1 = N + 1
      SQRE1 = SQRE
*
*     If matrix non-square upper bidiagonal, rotate to be lower
*     bidiagonal.  The rotations are on the right.
*
      IF( ( IUPLO.EQ.1 ) .AND. ( SQRE1.EQ.1 ) ) THEN
         OPS = OPS + DBLE( 8*( N-1 ) )
         DO 10 I = 1, N - 1
            CALL DLARTG( D( I ), E( I ), CS, SN, R )
            D( I ) = R
            E( I ) = SN*D( I+1 )
            D( I+1 ) = CS*D( I+1 )
            IF( ROTATE ) THEN
               WORK( I ) = CS
               WORK( N+I ) = SN
            END IF
   10    CONTINUE
         OPS = OPS + DBLE( 6 )
         CALL DLARTG( D( N ), E( N ), CS, SN, R )
         D( N ) = R
         E( N ) = ZERO
         IF( ROTATE ) THEN
            WORK( N ) = CS
            WORK( N+N ) = SN
         END IF
         IUPLO = 2
         SQRE1 = 0
*
*        Update singular vectors if desired.
*
         IF( NCVT.GT.0 ) THEN
            OPS = OPS + DBLE( 6*( NP1-1 )*NCVT )
            CALL DLASR( 'L', 'V', 'F', NP1, NCVT, WORK( 1 ),
     $                  WORK( NP1 ), VT, LDVT )
         END IF
      END IF
*
*     If matrix lower bidiagonal, rotate to be upper bidiagonal
*     by applying Givens rotations on the left.
*
      IF( IUPLO.EQ.2 ) THEN
         OPS = OPS + DBLE( 8*( N-1 ) )
         DO 20 I = 1, N - 1
            CALL DLARTG( D( I ), E( I ), CS, SN, R )
            D( I ) = R
            E( I ) = SN*D( I+1 )
            D( I+1 ) = CS*D( I+1 )
            IF( ROTATE ) THEN
               WORK( I ) = CS
               WORK( N+I ) = SN
            END IF
   20    CONTINUE
*
*        If matrix (N+1)-by-N lower bidiagonal, one additional
*        rotation is needed.
*
         IF( SQRE1.EQ.1 ) THEN
            OPS = OPS + DBLE( 6 )
            CALL DLARTG( D( N ), E( N ), CS, SN, R )
            D( N ) = R
            IF( ROTATE ) THEN
               WORK( N ) = CS
               WORK( N+N ) = SN
            END IF
         END IF
*
*        Update singular vectors if desired.
*
         IF( NRU.GT.0 ) THEN
            IF( SQRE1.EQ.0 ) THEN
               OPS = OPS + DBLE( 6*( N-1 )*NRU )
               CALL DLASR( 'R', 'V', 'F', NRU, N, WORK( 1 ),
     $                     WORK( NP1 ), U, LDU )
            ELSE
               OPS = OPS + DBLE( 6*N*NRU )
               CALL DLASR( 'R', 'V', 'F', NRU, NP1, WORK( 1 ),
     $                     WORK( NP1 ), U, LDU )
            END IF
         END IF
         IF( NCC.GT.0 ) THEN
            IF( SQRE1.EQ.0 ) THEN
               OPS = OPS + DBLE( 6*( N-1 )*NCC )
               CALL DLASR( 'L', 'V', 'F', N, NCC, WORK( 1 ),
     $                     WORK( NP1 ), C, LDC )
            ELSE
               OPS = OPS + DBLE( 6*N*NCC )
               CALL DLASR( 'L', 'V', 'F', NP1, NCC, WORK( 1 ),
     $                     WORK( NP1 ), C, LDC )
            END IF
         END IF
      END IF
*
*     Call DBDSQR to compute the SVD of the reduced real
*     N-by-N upper bidiagonal matrix.
*
      CALL DBDSQR( 'U', N, NCVT, NRU, NCC, D, E, VT, LDVT, U, LDU, C,
     $             LDC, WORK, INFO )
*
*     Sort the singular values into ascending order (insertion sort on
*     singular values, but only one transposition per singular vector)
*
      DO 40 I = 1, N
*
*        Scan for smallest D(I).
*
         ISUB = I
         SMIN = D( I )
         DO 30 J = I + 1, N
            IF( D( J ).LT.SMIN ) THEN
               ISUB = J
               SMIN = D( J )
            END IF
   30    CONTINUE
         IF( ISUB.NE.I ) THEN
*
*           Swap singular values and vectors.
*
            D( ISUB ) = D( I )
            D( I ) = SMIN
            IF( NCVT.GT.0 )
     $         CALL DSWAP( NCVT, VT( ISUB, 1 ), LDVT, VT( I, 1 ), LDVT )
            IF( NRU.GT.0 )
     $         CALL DSWAP( NRU, U( 1, ISUB ), 1, U( 1, I ), 1 )
            IF( NCC.GT.0 )
     $         CALL DSWAP( NCC, C( ISUB, 1 ), LDC, C( I, 1 ), LDC )
         END IF
   40 CONTINUE
*
      RETURN
*
*     End of DLASDQ
*
      END
      SUBROUTINE DLASDT( N, LVL, ND, INODE, NDIML, NDIMR, MSUB )
*
*  -- LAPACK auxiliary routine (instrum to count ops, version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     June 30, 1999
*
*     .. Scalar Arguments ..
      INTEGER            LVL, MSUB, N, ND
*     ..
*     .. Array Arguments ..
      INTEGER            INODE( * ), NDIML( * ), NDIMR( * )
*     ..
*     Common block to return operation count
*     .. Common blocks ..
      COMMON             / LATIME / OPS, ITCNT
*     ..
*     .. Scalars in Common ..
      DOUBLE PRECISION   ITCNT, OPS
*     ..
*
*  Purpose
*  =======
*
*  DLASDT creates a tree of subproblems for bidiagonal divide and
*  conquer.
*
*  Arguments
*  =========
*
*   N      (input) INTEGER
*          On entry, the number of diagonal elements of the
*          bidiagonal matrix.
*
*   LVL    (output) INTEGER
*          On exit, the number of levels on the computation tree.
*
*   ND     (output) INTEGER
*          On exit, the number of nodes on the tree.
*
*   INODE  (output) INTEGER array, dimension ( N )
*          On exit, centers of subproblems.
*
*   NDIML  (output) INTEGER array, dimension ( N )
*          On exit, row dimensions of left children.
*
*   NDIMR  (output) INTEGER array, dimension ( N )
*          On exit, row dimensions of right children.
*
*   MSUB   (input) INTEGER.
*          On entry, the maximum row dimension each subproblem at the
*          bottom of the tree can be of.
*
*  Further Details
*  ===============
*
*  Based on contributions by
*     Ming Gu and Huan Ren, Computer Science Division, University of
*     California at Berkeley, USA
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   TWO
      PARAMETER          ( TWO = 2.0D0 )
*     ..
*     .. Local Scalars ..
      INTEGER            I, IL, IR, LLST, MAXN, NCRNT, NLVL
      DOUBLE PRECISION   TEMP
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          DBLE, INT, LOG, MAX
*     ..
*     .. Executable Statements ..
*
*     Find the number of levels on the tree.
*
      OPS = OPS + DBLE( 2 )
      MAXN = MAX( 1, N )
      TEMP = LOG( DBLE( MAXN ) / DBLE( MSUB+1 ) ) / LOG( TWO )
      LVL = INT( TEMP ) + 1
*
      I = N / 2
      INODE( 1 ) = I + 1
      NDIML( 1 ) = I
      NDIMR( 1 ) = N - I - 1
      IL = 0
      IR = 1
      LLST = 1
      DO 20 NLVL = 1, LVL - 1
*
*        Constructing the tree at (NLVL+1)-st level. The number of
*        nodes created on this level is LLST * 2.
*
         DO 10 I = 0, LLST - 1
            IL = IL + 2
            IR = IR + 2
            NCRNT = LLST + I
            NDIML( IL ) = NDIML( NCRNT ) / 2
            NDIMR( IL ) = NDIML( NCRNT ) - NDIML( IL ) - 1
            INODE( IL ) = INODE( NCRNT ) - NDIMR( IL ) - 1
            NDIML( IR ) = NDIMR( NCRNT ) / 2
            NDIMR( IR ) = NDIMR( NCRNT ) - NDIML( IR ) - 1
            INODE( IR ) = INODE( NCRNT ) + NDIML( IR ) + 1
   10    CONTINUE
         LLST = LLST*2
   20 CONTINUE
      ND = LLST*2 - 1
*
      RETURN
*
*     End of DLASDT
*
      END
      SUBROUTINE DLASQ1( N, D, E, WORK, INFO )
*
*  -- LAPACK routine (instrumented to count ops, version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     October 31, 1999 
*
*     .. Scalar Arguments ..
      INTEGER            INFO, N
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   D( * ), E( * ), WORK( * )
*     ..
*     .. Common block to return operation count ..
      COMMON             / LATIME / OPS, ITCNT
*     ..
*     .. Scalars in Common ..
      DOUBLE PRECISION   ITCNT, OPS
*     ..
*
*  Purpose
*  =======
*
*  DLASQ1 computes the singular values of a real N-by-N bidiagonal
*  matrix with diagonal D and off-diagonal E. The singular values
*  are computed to high relative accuracy, in the absence of
*  denormalization, underflow and overflow. The algorithm was first
*  presented in
*
*  "Accurate singular values and differential qd algorithms" by K. V.
*  Fernando and B. N. Parlett, Numer. Math., Vol-67, No. 2, pp. 191-230,
*  1994,
*
*  and the present implementation is described in "An implementation of
*  the dqds Algorithm (Positive Case)", LAPACK Working Note.
*
*  Arguments
*  =========
*
*  N     (input) INTEGER
*        The number of rows and columns in the matrix. N >= 0.
*
*  D     (input/output) DOUBLE PRECISION array, dimension (N)
*        On entry, D contains the diagonal elements of the
*        bidiagonal matrix whose SVD is desired. On normal exit,
*        D contains the singular values in decreasing order.
*
*  E     (input/output) DOUBLE PRECISION array, dimension (N)
*        On entry, elements E(1:N-1) contain the off-diagonal elements
*        of the bidiagonal matrix whose SVD is desired.
*        On exit, E is overwritten.
*
*  WORK  (workspace) DOUBLE PRECISION array, dimension (4*N)
*
*  INFO  (output) INTEGER
*        = 0: successful exit
*        < 0: if INFO = -i, the i-th argument had an illegal value
*        > 0: the algorithm failed
*             = 1, a split was marked by a positive value in E
*             = 2, current block of Z not diagonalized after 30*N
*                  iterations (in inner while loop)
*             = 3, termination criterion of outer while loop not met 
*                  (program created more than N unreduced blocks)
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO
      PARAMETER          ( ZERO = 0.0D0 )
*     ..
*     .. Local Scalars ..
      INTEGER            I, IINFO
      DOUBLE PRECISION   EPS, SCALE, SAFMIN, SIGMN, SIGMX
*     ..
*     .. External Subroutines ..
      EXTERNAL           DLAS2, DLASQ2, DLASRT, XERBLA
*     ..
*     .. External Functions ..
      DOUBLE PRECISION   DLAMCH
      EXTERNAL           DLAMCH
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, DBLE, MAX, SQRT
*     ..
*     .. Executable Statements ..
*
      INFO = 0
      IF( N.LT.0 ) THEN
         INFO = -2
         CALL XERBLA( 'DLASQ1', -INFO )
         RETURN
      ELSE IF( N.EQ.0 ) THEN
         RETURN
      ELSE IF( N.EQ.1 ) THEN
         D( 1 ) = ABS( D( 1 ) )
         RETURN
      ELSE IF( N.EQ.2 ) THEN
         CALL DLAS2( D( 1 ), E( 1 ), D( 2 ), SIGMN, SIGMX )
         D( 1 ) = SIGMX
         D( 2 ) = SIGMN
         RETURN
      END IF
*
*     Estimate the largest singular value.
*
      SIGMX = ZERO
      DO 10 I = 1, N - 1
         D( I ) = ABS( D( I ) )
         SIGMX = MAX( SIGMX, ABS( E( I ) ) )
   10 CONTINUE
      D( N ) = ABS( D( N ) )
*
*     Early return if SIGMX is zero (matrix is already diagonal).
*
      IF( SIGMX.EQ.ZERO ) THEN
         CALL DLASRT( 'D', N, D, IINFO )
         RETURN
      END IF
*
      DO 20 I = 1, N
         SIGMX = MAX( SIGMX, D( I ) )
   20 CONTINUE
*
*     Copy D and E into WORK (in the Z format) and scale (squaring the
*     input data makes scaling by a power of the radix pointless).
*
      OPS = OPS + DBLE( 1 + 2*N )
      EPS = DLAMCH( 'Precision' )
      SAFMIN = DLAMCH( 'Safe minimum' )
      SCALE = SQRT( EPS / SAFMIN )
      CALL DCOPY( N, D, 1, WORK( 1 ), 2 )
      CALL DCOPY( N-1, E, 1, WORK( 2 ), 2 )
      CALL DLASCL( 'G', 0, 0, SIGMX, SCALE, 2*N-1, 1, WORK, 2*N-1,
     $             IINFO )
*         
*     Compute the q's and e's.
*
      OPS = OPS + DBLE( 2*N-1 )
      DO 30 I = 1, 2*N - 1
         WORK( I ) = WORK( I )**2
   30 CONTINUE
      WORK( 2*N ) = ZERO
*
      CALL DLASQ2( N, WORK, INFO )
*
      IF( INFO.EQ.0 ) THEN
         OPS = OPS + DBLE( 2*N )
         DO 40 I = 1, N
            D( I ) = SQRT( WORK( I ) )
   40    CONTINUE
         CALL DLASCL( 'G', 0, 0, SCALE, SIGMX, N, 1, D, N, IINFO )
      END IF
*
      RETURN
*
*     End of DLASQ1
*
      END
      SUBROUTINE DLASQ2( N, Z, INFO )
*
*  -- LAPACK routine (instrumented to count ops, version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     October 31, 1999 
*
*     .. Scalar Arguments ..
      INTEGER            INFO, N
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   Z( * )
*     ..
*     .. Common block to return operation count ..
      COMMON             / LATIME / OPS, ITCNT
*     ..
*     .. Scalars in Common ..
      DOUBLE PRECISION   ITCNT, OPS
*     ..
*
*  Purpose
*  =======
*
*  DLASQ2 computes all the eigenvalues of the symmetric positive 
*  definite tridiagonal matrix associated with the qd array Z to high
*  relative accuracy are computed to high relative accuracy, in the
*  absence of denormalization, underflow and overflow.
*
*  To see the relation of Z to the tridiagonal matrix, let L be a
*  unit lower bidiagonal matrix with subdiagonals Z(2,4,6,,..) and
*  let U be an upper bidiagonal matrix with 1's above and diagonal
*  Z(1,3,5,,..). The tridiagonal is L*U or, if you prefer, the
*  symmetric tridiagonal to which it is similar.
*
*  Note : DLASQ2 defines a logical variable, IEEE, which is true
*  on machines which follow ieee-754 floating-point standard in their
*  handling of infinities and NaNs, and false otherwise. This variable
*  is passed to DLASQ3.
*
*  Arguments
*  =========
*
*  N     (input) INTEGER
*        The number of rows and columns in the matrix. N >= 0.
*
*  Z     (workspace) DOUBLE PRECISION array, dimension ( 4*N )
*        On entry Z holds the qd array. On exit, entries 1 to N hold
*        the eigenvalues in decreasing order, Z( 2*N+1 ) holds the
*        trace, and Z( 2*N+2 ) holds the sum of the eigenvalues. If
*        N > 2, then Z( 2*N+3 ) holds the iteration count, Z( 2*N+4 )
*        holds NDIVS/NIN^2, and Z( 2*N+5 ) holds the percentage of
*        shifts that failed.
*
*  INFO  (output) INTEGER
*        = 0: successful exit
*        < 0: if the i-th argument is a scalar and had an illegal
*             value, then INFO = -i, if the i-th argument is an
*             array and the j-entry had an illegal value, then
*             INFO = -(i*100+j)
*        > 0: the algorithm failed
*              = 1, a split was marked by a positive value in E
*              = 2, current block of Z not diagonalized after 30*N
*                   iterations (in inner while loop)
*              = 3, termination criterion of outer while loop not met 
*                   (program created more than N unreduced blocks)
*
*  Further Details
*  ===============
*  Local Variables: I0:N0 defines a current unreduced segment of Z.
*  The shifts are accumulated in SIGMA. Iteration count is in ITER.
*  Ping-pong is controlled by PP (alternates between 0 and 1).
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   CBIAS
      PARAMETER          ( CBIAS = 1.50D0 )
      DOUBLE PRECISION   ZERO, HALF, ONE, TWO, FOUR, HUNDRD
      PARAMETER          ( ZERO = 0.0D0, HALF = 0.5D0, ONE = 1.0D0,
     $                     TWO = 2.0D0, FOUR = 4.0D0, HUNDRD = 100.0D0 )
*     ..
*     .. Local Scalars ..
      LOGICAL            IEEE
      INTEGER            I0, I4, IINFO, IPN4, ITER, IWHILA, IWHILB, K, 
     $                   N0, NBIG, NDIV, NFAIL, PP, SPLT
      DOUBLE PRECISION   D, DESIG, DMIN, E, EMAX, EMIN, EPS, OLDEMN, 
     $                   QMAX, QMIN, S, SAFMIN, SIGMA, T, TEMP, TOL, 
     $                   TOL2, TRACE, ZMAX
*     ..
*     .. External Subroutines ..
      EXTERNAL           DLASQ3, DLASRT, XERBLA
*     ..
*     .. External Functions ..
      INTEGER            ILAENV
      DOUBLE PRECISION   DLAMCH
      EXTERNAL           DLAMCH, ILAENV
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          DBLE, MAX, MIN, SQRT
*     ..
*     .. Executable Statements ..
*      
*     Test the input arguments.
*     (in case DLASQ2 is not called by DLASQ1)
*
      OPS = OPS + DBLE( 2 )
      INFO = 0
      EPS = DLAMCH( 'Precision' )
      SAFMIN = DLAMCH( 'Safe minimum' )
      TOL = EPS*HUNDRD
      TOL2 = TOL**2
*
      IF( N.LT.0 ) THEN
         INFO = -1
         CALL XERBLA( 'DLASQ2', 1 )
         RETURN
      ELSE IF( N.EQ.0 ) THEN
         RETURN
      ELSE IF( N.EQ.1 ) THEN
*
*        1-by-1 case.
*
         IF( Z( 1 ).LT.ZERO ) THEN
            INFO = -201
            CALL XERBLA( 'DLASQ2', 2 )
         END IF
         RETURN
      ELSE IF( N.EQ.2 ) THEN
*
*        2-by-2 case.
*
         IF( Z( 2 ).LT.ZERO .OR. Z( 3 ).LT.ZERO ) THEN
            INFO = -2
            CALL XERBLA( 'DLASQ2', 2 )
            RETURN
         ELSE IF( Z( 3 ).GT.Z( 1 ) ) THEN
            D = Z( 3 )
            Z( 3 ) = Z( 1 )
            Z( 1 ) = D
         END IF
         OPS = OPS + DBLE( 4 )
         Z( 5 ) = Z( 1 ) + Z( 2 ) + Z( 3 )
         IF( Z( 2 ).GT.Z( 3 )*TOL2 ) THEN
            OPS = OPS + DBLE( 16 )
            T = HALF*( ( Z( 1 )-Z( 3 ) )+Z( 2 ) ) 
            S = Z( 3 )*( Z( 2 ) / T )
            IF( S.LE.T ) THEN
               S = Z( 3 )*( Z( 2 ) / ( T*( ONE+SQRT( ONE+S / T ) ) ) )
            ELSE
               S = Z( 3 )*( Z( 2 ) / ( T+SQRT( T )*SQRT( T+S ) ) )
            END IF
            T = Z( 1 ) + ( S+Z( 2 ) )
            Z( 3 ) = Z( 3 )*( Z( 1 ) / T )
            Z( 1 ) = T
         END IF
         Z( 2 ) = Z( 3 )
         Z( 6 ) = Z( 2 ) + Z( 1 )
         RETURN
      END IF
*
*     Check for negative data and compute sums of q's and e's.
*
      Z( 2*N ) = ZERO
      EMIN = Z( 2 )
      QMAX = ZERO
      ZMAX = ZERO
      D = ZERO
      E = ZERO
*
      OPS = OPS + DBLE( 2*N )
      DO 10 K = 1, 2*( N-1 ), 2
         IF( Z( K ).LT.ZERO ) THEN
            INFO = -( 200+K )
            CALL XERBLA( 'DLASQ2', 2 )
            RETURN
         ELSE IF( Z( K+1 ).LT.ZERO ) THEN
            INFO = -( 200+K+1 )
            CALL XERBLA( 'DLASQ2', 2 )
            RETURN
         END IF
         D = D + Z( K )
         E = E + Z( K+1 )
         QMAX = MAX( QMAX, Z( K ) )
         EMIN = MIN( EMIN, Z( K+1 ) )
         ZMAX = MAX( QMAX, ZMAX, Z( K+1 ) )
   10 CONTINUE
      IF( Z( 2*N-1 ).LT.ZERO ) THEN
         INFO = -( 200+2*N-1 )
         CALL XERBLA( 'DLASQ2', 2 )
         RETURN
      END IF
      D = D + Z( 2*N-1 )
      QMAX = MAX( QMAX, Z( 2*N-1 ) )
      ZMAX = MAX( QMAX, ZMAX )
*
*     Check for diagonality.
*
      IF( E.EQ.ZERO ) THEN
         DO 20 K = 2, N
            Z( K ) = Z( 2*K-1 )
   20    CONTINUE
         CALL DLASRT( 'D', N, Z, IINFO )
         Z( 2*N-1 ) = D
         RETURN
      END IF
*
      TRACE = D + E
*
*     Check for zero data.
*
      IF( TRACE.EQ.ZERO ) THEN
         Z( 2*N-1 ) = ZERO
         RETURN
      END IF
*         
*     Check whether the machine is IEEE conformable.
*         
      IEEE = ILAENV( 10, 'DLASQ2', 'N', 1, 2, 3, 4 ).EQ.1 .AND.
     $       ILAENV( 11, 'DLASQ2', 'N', 1, 2, 3, 4 ).EQ.1      
*         
*     Rearrange data for locality: Z=(q1,qq1,e1,ee1,q2,qq2,e2,ee2,...).
*
      DO 30 K = 2*N, 2, -2
         Z( 2*K ) = ZERO 
         Z( 2*K-1 ) = Z( K ) 
         Z( 2*K-2 ) = ZERO 
         Z( 2*K-3 ) = Z( K-1 ) 
   30 CONTINUE
*
      I0 = 1
      N0 = N
*
*     Reverse the qd-array, if warranted.
*
      OPS = OPS + DBLE( 1 )
      IF( CBIAS*Z( 4*I0-3 ).LT.Z( 4*N0-3 ) ) THEN
         IPN4 = 4*( I0+N0 )
         DO 40 I4 = 4*I0, 2*( I0+N0-1 ), 4
            TEMP = Z( I4-3 )
            Z( I4-3 ) = Z( IPN4-I4-3 )
            Z( IPN4-I4-3 ) = TEMP
            TEMP = Z( I4-1 )
            Z( I4-1 ) = Z( IPN4-I4-5 )
            Z( IPN4-I4-5 ) = TEMP
   40    CONTINUE
      END IF
*
*     Initial split checking via dqd and Li's test.
*
      PP = 0
*
      DO 80 K = 1, 2
*
         OPS = OPS + DBLE( N0-I0 )
         D = Z( 4*N0+PP-3 )
         DO 50 I4 = 4*( N0-1 ) + PP, 4*I0 + PP, -4
            IF( Z( I4-1 ).LE.TOL2*D ) THEN
               Z( I4-1 ) = -ZERO
               D = Z( I4-3 )
            ELSE
               OPS = OPS + DBLE( 3 )
               D = Z( I4-3 )*( D / ( D+Z( I4-1 ) ) )
            END IF
   50    CONTINUE
*
*        dqd maps Z to ZZ plus Li's test.
*
         OPS = OPS + DBLE( N0-I0 )
         EMIN = Z( 4*I0+PP+1 )
         D = Z( 4*I0+PP-3 )
         DO 60 I4 = 4*I0 + PP, 4*( N0-1 ) + PP, 4
            Z( I4-2*PP-2 ) = D + Z( I4-1 )
            IF( Z( I4-1 ).LE.TOL2*D ) THEN
               Z( I4-1 ) = -ZERO
               Z( I4-2*PP-2 ) = D
               Z( I4-2*PP ) = ZERO
               D = Z( I4+1 )
            ELSE IF( SAFMIN*Z( I4+1 ).LT.Z( I4-2*PP-2 ) .AND.
     $               SAFMIN*Z( I4-2*PP-2 ).LT.Z( I4+1 ) ) THEN
               OPS = OPS + DBLE( 5 )
               TEMP = Z( I4+1 ) / Z( I4-2*PP-2 )
               Z( I4-2*PP ) = Z( I4-1 )*TEMP
               D = D*TEMP
            ELSE
               OPS = OPS + DBLE( 5 )
               Z( I4-2*PP ) = Z( I4+1 )*( Z( I4-1 ) / Z( I4-2*PP-2 ) )
               D = Z( I4+1 )*( D / Z( I4-2*PP-2 ) )
            END IF
            EMIN = MIN( EMIN, Z( I4-2*PP ) )
   60    CONTINUE 
         Z( 4*N0-PP-2 ) = D
*
*        Now find qmax.
*
         QMAX = Z( 4*I0-PP-2 )
         DO 70 I4 = 4*I0 - PP + 2, 4*N0 - PP - 2, 4
            QMAX = MAX( QMAX, Z( I4 ) )
   70    CONTINUE
*
*        Prepare for the next iteration on K.
*
         PP = 1 - PP
   80 CONTINUE
*
      ITER = 2
      NFAIL = 0
      NDIV = 2*( N0-I0 )
*
      DO 140 IWHILA = 1, N + 1
         IF( N0.LT.1 ) 
     $      GO TO 150
*
*        While array unfinished do 
*
*        E(N0) holds the value of SIGMA when submatrix in I0:N0
*        splits from the rest of the array, but is negated.
*      
         DESIG = ZERO
         IF( N0.EQ.N ) THEN
            SIGMA = ZERO
         ELSE
            SIGMA = -Z( 4*N0-1 )
         END IF
         IF( SIGMA.LT.ZERO ) THEN
            INFO = 1
            RETURN
         END IF
*
*        Find last unreduced submatrix's top index I0, find QMAX and
*        EMIN. Find Gershgorin-type bound if Q's much greater than E's.
*
         EMAX = ZERO 
         IF( N0.GT.I0 ) THEN
            EMIN = ABS( Z( 4*N0-5 ) )
         ELSE
            EMIN = ZERO
         END IF
         QMIN = Z( 4*N0-3 )
         QMAX = QMIN
         DO 90 I4 = 4*N0, 8, -4
            IF( Z( I4-5 ).LE.ZERO )
     $         GO TO 100
            OPS = OPS + DBLE( 2 )
            IF( QMIN.GE.FOUR*EMAX ) THEN
               QMIN = MIN( QMIN, Z( I4-3 ) )
               EMAX = MAX( EMAX, Z( I4-5 ) )
            END IF
            QMAX = MAX( QMAX, Z( I4-7 )+Z( I4-5 ) )
            EMIN = MIN( EMIN, Z( I4-5 ) )
   90    CONTINUE
         I4 = 4 
*
  100    CONTINUE
         I0 = I4 / 4
*
*        Store EMIN for passing to DLASQ3.
*
         Z( 4*N0-1 ) = EMIN
*
*        Put -(initial shift) into DMIN.
*
         OPS = OPS + DBLE( 5 )
         DMIN = -MAX( ZERO, QMIN-TWO*SQRT( QMIN )*SQRT( EMAX ) )
*
*        Now I0:N0 is unreduced. PP = 0 for ping, PP = 1 for pong.
*
         PP = 0 
*
         NBIG = 30*( N0-I0+1 )
         DO 120 IWHILB = 1, NBIG
            IF( I0.GT.N0 ) 
     $         GO TO 130
*
*           While submatrix unfinished take a good dqds step.
*
            CALL DLASQ3( I0, N0, Z, PP, DMIN, SIGMA, DESIG, QMAX, NFAIL,
     $                   ITER, NDIV, IEEE )
*
	    PP = 1 - PP
*
*           When EMIN is very small check for splits.
*
            IF( PP.EQ.0 .AND. N0-I0.GE.3 ) THEN
               OPS = OPS + DBLE( 2 )
               IF( Z( 4*N0 ).LE.TOL2*QMAX .OR.
     $             Z( 4*N0-1 ).LE.TOL2*SIGMA ) THEN
                  SPLT = I0 - 1
                  QMAX = Z( 4*I0-3 )
                  EMIN = Z( 4*I0-1 )
                  OLDEMN = Z( 4*I0 )
                  DO 110 I4 = 4*I0, 4*( N0-3 ), 4
                     OPS = OPS + DBLE( 1 )
                     IF( Z( I4 ).LE.TOL2*Z( I4-3 ) .OR.
     $                   Z( I4-1 ).LE.TOL2*SIGMA ) THEN
                        Z( I4-1 ) = -SIGMA
                        SPLT = I4 / 4
                        QMAX = ZERO
                        EMIN = Z( I4+3 )
                        OLDEMN = Z( I4+4 )
                     ELSE
                        QMAX = MAX( QMAX, Z( I4+1 ) )
                        EMIN = MIN( EMIN, Z( I4-1 ) )
                        OLDEMN = MIN( OLDEMN, Z( I4 ) )
                     END IF
  110             CONTINUE
                  Z( 4*N0-1 ) = EMIN
                  Z( 4*N0 ) = OLDEMN
                  I0 = SPLT + 1
               END IF
            END IF
*
  120    CONTINUE
*
         INFO = 2
         RETURN
*
*        end IWHILB
*
  130    CONTINUE
*
  140 CONTINUE
*
      INFO = 3
      RETURN
*
*     end IWHILA   
*
  150 CONTINUE
*      
*     Move q's to the front.
*      
      DO 160 K = 2, N
         Z( K ) = Z( 4*K-3 )
  160 CONTINUE
*      
*     Sort and compute sum of eigenvalues.
*
      CALL DLASRT( 'D', N, Z, IINFO )
*
      E = ZERO
      DO 170 K = N, 1, -1
         E = E + Z( K )
  170 CONTINUE
*
*     Store trace, sum(eigenvalues) and information on performance.
*
      Z( 2*N+1 ) = TRACE 
      Z( 2*N+2 ) = E
      Z( 2*N+3 ) = DBLE( ITER )
      Z( 2*N+4 ) = DBLE( NDIV ) / DBLE( N**2 )
      Z( 2*N+5 ) = HUNDRD*NFAIL / DBLE( ITER )
      RETURN
*
*     End of DLASQ2
*
      END
      SUBROUTINE DLASQ3( I0, N0, Z, PP, DMIN, SIGMA, DESIG, QMAX, NFAIL,
     $                   ITER, NDIV, IEEE )
*
*  -- LAPACK auxiliary routine (instrumented to count ops, version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     May 17, 2000
*
*     .. Scalar Arguments ..
      LOGICAL            IEEE
      INTEGER            I0, ITER, N0, NDIV, NFAIL, PP
      DOUBLE PRECISION   DESIG, DMIN, QMAX, SIGMA
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   Z( * )
*     ..
*     .. Common block to return operation count ..
      COMMON             / LATIME / OPS, ITCNT
*     ..
*     .. Scalars in Common ..
      DOUBLE PRECISION   ITCNT, OPS
*     ..
*
*  Purpose
*  =======
*
*  DLASQ3 checks for deflation, computes a shift (TAU) and calls dqds.
*  In case of failure it changes shifts, and tries again until output
*  is positive.
*
*  Arguments
*  =========
*
*  I0     (input) INTEGER
*         First index.
*
*  N0     (input) INTEGER
*         Last index.
*
*  Z      (input) DOUBLE PRECISION array, dimension ( 4*N )
*         Z holds the qd array.
*
*  PP     (input) INTEGER
*         PP=0 for ping, PP=1 for pong.
*
*  DMIN   (output) DOUBLE PRECISION
*         Minimum value of d.
*
*  SIGMA  (output) DOUBLE PRECISION
*         Sum of shifts used in current segment.
*
*  DESIG  (input/output) DOUBLE PRECISION
*         Lower order part of SIGMA
*
*  QMAX   (input) DOUBLE PRECISION
*         Maximum value of q.
*
*  NFAIL  (output) INTEGER
*         Number of times shift was too big.
*
*  ITER   (output) INTEGER
*         Number of iterations.
*
*  NDIV   (output) INTEGER
*         Number of divisions.
*
*  TTYPE  (output) INTEGER
*         Shift type.
*
*  IEEE   (input) LOGICAL
*         Flag for IEEE or non IEEE arithmetic (passed to DLASQ5).
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   CBIAS
      PARAMETER          ( CBIAS = 1.50D0 )
      DOUBLE PRECISION   ZERO, QURTR, HALF, ONE, TWO, HUNDRD
      PARAMETER          ( ZERO = 0.0D0, QURTR = 0.250D0, HALF = 0.5D0,
     $                     ONE = 1.0D0, TWO = 2.0D0, HUNDRD = 100.0D0 )
*     ..
*     .. Local Scalars ..
      INTEGER            IPN4, J4, N0IN, NN, TTYPE
      DOUBLE PRECISION   DMIN1, DMIN2, DN, DN1, DN2, EPS, S, SAFMIN, T,
     $                   TAU, TEMP, TOL, TOL2
*     ..
*     .. External Subroutines ..
      EXTERNAL           DLASQ4, DLASQ5, DLASQ6
*     ..
*     .. External Function ..
      DOUBLE PRECISION   DLAMCH
      EXTERNAL           DLAMCH
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, DBLE, MIN, SQRT
*     ..
*     .. Save statement ..
      SAVE               TTYPE
      SAVE               DMIN1, DMIN2, DN, DN1, DN2, TAU
*     ..
*     .. Data statement ..
      DATA               TTYPE / 0 /
      DATA               DMIN1 / ZERO /, DMIN2 / ZERO /, DN / ZERO /,
     $                   DN1 / ZERO /, DN2 / ZERO /, TAU / ZERO /
*     ..
*     .. Executable Statements ..
*
      OPS = OPS + DBLE( 2 )
      N0IN = N0
      EPS = DLAMCH( 'Precision' )
      SAFMIN = DLAMCH( 'Safe minimum' )
      TOL = EPS*HUNDRD
      TOL2 = TOL**2
*
*     Check for deflation.
*
   10 CONTINUE
*
      IF( N0.LT.I0 )
     $   RETURN
      IF( N0.EQ.I0 )
     $   GO TO 20
      NN = 4*N0 + PP
      IF( N0.EQ.( I0+1 ) )
     $   GO TO 40
*
*     Check whether E(N0-1) is negligible, 1 eigenvalue.
*
      OPS = OPS + DBLE( 3 )
      IF( Z( NN-5 ).GT.TOL2*( SIGMA+Z( NN-3 ) ) .AND.
     $    Z( NN-2*PP-4 ).GT.TOL2*Z( NN-7 ) )
     $   GO TO 30
*
   20 CONTINUE
*
      OPS = OPS + DBLE( 1 )
      Z( 4*N0-3 ) = Z( 4*N0+PP-3 ) + SIGMA
      N0 = N0 - 1
      GO TO 10
*
*     Check  whether E(N0-2) is negligible, 2 eigenvalues.
*
   30 CONTINUE
*
      OPS = OPS + DBLE( 2 )
      IF( Z( NN-9 ).GT.TOL2*SIGMA .AND.
     $    Z( NN-2*PP-8 ).GT.TOL2*Z( NN-11 ) )
     $   GO TO 50
*
   40 CONTINUE
*
      IF( Z( NN-3 ).GT.Z( NN-7 ) ) THEN
         S = Z( NN-3 )
         Z( NN-3 ) = Z( NN-7 )
         Z( NN-7 ) = S
      END IF
      OPS = OPS + DBLE( 3 )
      IF( Z( NN-5 ).GT.Z( NN-3 )*TOL2 ) THEN
         OPS = OPS + DBLE( 5 )
         T = HALF*( ( Z( NN-7 )-Z( NN-3 ) )+Z( NN-5 ) )
         S = Z( NN-3 )*( Z( NN-5 ) / T )
         IF( S.LE.T ) THEN
            OPS = OPS + DBLE( 7 )
            S = Z( NN-3 )*( Z( NN-5 ) /
     $          ( T*( ONE+SQRT( ONE+S / T ) ) ) )
         ELSE
            OPS = OPS + DBLE( 6 )
            S = Z( NN-3 )*( Z( NN-5 ) / ( T+SQRT( T )*SQRT( T+S ) ) )
         END IF
         OPS = OPS + DBLE( 4 )
         T = Z( NN-7 ) + ( S+Z( NN-5 ) )
         Z( NN-3 ) = Z( NN-3 )*( Z( NN-7 ) / T )
         Z( NN-7 ) = T
      END IF
      Z( 4*N0-7 ) = Z( NN-7 ) + SIGMA
      Z( 4*N0-3 ) = Z( NN-3 ) + SIGMA
      N0 = N0 - 2
      GO TO 10
*
   50 CONTINUE
*
*     Reverse the qd-array, if warranted.
*
      IF( DMIN.LE.ZERO .OR. N0.LT.N0IN ) THEN
         OPS = OPS + DBLE( 1 )
         IF( CBIAS*Z( 4*I0+PP-3 ).LT.Z( 4*N0+PP-3 ) ) THEN
            IPN4 = 4*( I0+N0 )
            DO 60 J4 = 4*I0, 2*( I0+N0-1 ), 4
               TEMP = Z( J4-3 )
               Z( J4-3 ) = Z( IPN4-J4-3 )
               Z( IPN4-J4-3 ) = TEMP
               TEMP = Z( J4-2 )
               Z( J4-2 ) = Z( IPN4-J4-2 )
               Z( IPN4-J4-2 ) = TEMP
               TEMP = Z( J4-1 )
               Z( J4-1 ) = Z( IPN4-J4-5 )
               Z( IPN4-J4-5 ) = TEMP
               TEMP = Z( J4 )
               Z( J4 ) = Z( IPN4-J4-4 )
               Z( IPN4-J4-4 ) = TEMP
   60       CONTINUE
            IF( N0-I0.LE.4 ) THEN
               Z( 4*N0+PP-1 ) = Z( 4*I0+PP-1 )
               Z( 4*N0-PP ) = Z( 4*I0-PP )
            END IF
            DMIN2 = MIN( DMIN2, Z( 4*N0+PP-1 ) )
            Z( 4*N0+PP-1 ) = MIN( Z( 4*N0+PP-1 ), Z( 4*I0+PP-1 ),
     $                            Z( 4*I0+PP+3 ) )
            Z( 4*N0-PP ) = MIN( Z( 4*N0-PP ), Z( 4*I0-PP ),
     $                          Z( 4*I0-PP+4 ) )
            QMAX = MAX( QMAX, Z( 4*I0+PP-3 ), Z( 4*I0+PP+1 ) )
            DMIN = -ZERO
         END IF
      END IF
*
   70 CONTINUE
*
      IF( DMIN.LT.ZERO .OR. SAFMIN*QMAX.LT.MIN( Z( 4*N0+PP-1 ),
     $    Z( 4*N0+PP-9 ), DMIN2+Z( 4*N0-PP ) ) ) THEN
*
*        Choose a shift.
*
         CALL DLASQ4( I0, N0, Z, PP, N0IN, DMIN, DMIN1, DMIN2, DN, DN1,
     $                DN2, TAU, TTYPE )
*
*        Call dqds until DMIN > 0.
*
   80    CONTINUE
*
         CALL DLASQ5( I0, N0, Z, PP, TAU, DMIN, DMIN1, DMIN2, DN,
     $                DN1, DN2, IEEE )
*
         NDIV = NDIV + ( N0-I0+2 )
         ITER = ITER + 1
*
*        Check status.
*
         IF( DMIN.GE.ZERO .AND. DMIN1.GT.ZERO ) THEN
*
*           Success.
*
            GO TO 100
*
         ELSE IF( DMIN.LT.ZERO .AND. DMIN1.GT.ZERO .AND.
     $            Z( 4*( N0-1 )-PP ).LT.TOL*( SIGMA+DN1 ) .AND.
     $            ABS( DN ).LT.TOL*SIGMA ) THEN
*
*           Convergence hidden by negative DN.
*
            OPS = OPS + DBLE( 2 )
            Z( 4*( N0-1 )-PP+2 ) = ZERO
            DMIN = ZERO
            GO TO 100
         ELSE IF( DMIN.LT.ZERO ) THEN
*
*           TAU too big. Select new TAU and try again.
*
            NFAIL = NFAIL + 1
            IF( TTYPE.LT.-22 ) THEN
*
*              Failed twice. Play it safe.
*
               TAU = ZERO
            ELSE IF( DMIN1.GT.ZERO ) THEN
*
*              Late failure. Gives excellent shift.
*
               OPS = OPS + DBLE( 4 )
               TAU = ( TAU+DMIN )*( ONE-TWO*EPS )
               TTYPE = TTYPE - 11
            ELSE
*
*              Early failure. Divide by 4.
*
               OPS = OPS + DBLE( 1 )
               TAU = QURTR*TAU
               TTYPE = TTYPE - 12
            END IF
            GO TO 80
         ELSE IF( DMIN.NE.DMIN ) THEN
*
*           NaN.
*
            TAU = ZERO
            GO TO 80
         ELSE
*
*           Possible underflow. Play it safe.
*
            GO TO 90
         END IF
      END IF
*
*     Risk of underflow.
*
   90 CONTINUE
      CALL DLASQ6( I0, N0, Z, PP, DMIN, DMIN1, DMIN2, DN, DN1, DN2 )
      NDIV = NDIV + ( N0-I0+2 )
      ITER = ITER + 1
      TAU = ZERO
*
  100 CONTINUE
      OPS = OPS + DBLE( 4 )
      IF( TAU.LT.SIGMA ) THEN
         DESIG = DESIG + TAU
         T = SIGMA + DESIG
         DESIG = DESIG - ( T-SIGMA )
      ELSE
         T = SIGMA + TAU
         DESIG = SIGMA - ( T-TAU ) + DESIG
      END IF
      SIGMA = T
*
      RETURN
*
*     End of DLASQ3
*
      END
      SUBROUTINE DLASQ4( I0, N0, Z, PP, N0IN, DMIN, DMIN1, DMIN2, DN,
     $                   DN1, DN2, TAU, TTYPE )
*
*  -- LAPACK auxiliary routine (instrumented to count ops, version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     May 17, 2000
*
*     .. Scalar Arguments ..
      INTEGER            I0, N0, N0IN, PP, TTYPE
      DOUBLE PRECISION   DMIN, DMIN1, DMIN2, DN, DN1, DN2, TAU
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   Z( * )
*     ..
*     .. Common block to return operation count ..
      COMMON             / LATIME / OPS, ITCNT
*     ..
*     .. Scalars in Common ..
      DOUBLE PRECISION   ITCNT, OPS
*     ..
*
*  Purpose
*  =======
*
*  DLASQ4 computes an approximation TAU to the smallest eigenvalue
*  using values of d from the previous transform.
*
*  I0    (input) INTEGER
*        First index.
*
*  N0    (input) INTEGER
*        Last index.
*
*  Z     (input) DOUBLE PRECISION array, dimension ( 4*N )
*        Z holds the qd array.
*
*  PP    (input) INTEGER
*        PP=0 for ping, PP=1 for pong.
*
*  NOIN  (input) INTEGER
*        The value of N0 at start of EIGTEST.
*
*  DMIN  (input) DOUBLE PRECISION
*        Minimum value of d.
*
*  DMIN1 (input) DOUBLE PRECISION
*        Minimum value of d, excluding D( N0 ).
*
*  DMIN2 (input) DOUBLE PRECISION
*        Minimum value of d, excluding D( N0 ) and D( N0-1 ).
*
*  DN    (input) DOUBLE PRECISION
*        d(N)
*
*  DN1   (input) DOUBLE PRECISION
*        d(N-1)
*
*  DN2   (input) DOUBLE PRECISION
*        d(N-2)
*
*  TAU   (output) DOUBLE PRECISION
*        This is the shift.
*
*  TTYPE (output) INTEGER
*        Shift type.
*
*  Further Details
*  ===============
*  CNST1 = 9/16
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   CNST1, CNST2, CNST3
      PARAMETER          ( CNST1 = 0.5630D0, CNST2 = 1.010D0,
     $                   CNST3 = 1.050D0 )
      DOUBLE PRECISION   QURTR, THIRD, HALF, ZERO, ONE, TWO, HUNDRD
      PARAMETER          ( QURTR = 0.250D0, THIRD = 0.3330D0,
     $                   HALF = 0.50D0, ZERO = 0.0D0, ONE = 1.0D0,
     $                   TWO = 2.0D0, HUNDRD = 100.0D0 )
*     ..
*     .. Local Scalars ..
      INTEGER            I4, NN, NP
      DOUBLE PRECISION   A2, B1, B2, G, GAM, GAP1, GAP2, S
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          DBLE, MAX, MIN, SQRT
*     ..
*     .. Save statement ..
      SAVE               G
*     ..
*     .. Data statement ..
      DATA               G / ZERO /
*     ..
*     .. Executable Statements ..
*
*     A negative DMIN forces the shift to take that absolute value
*     TTYPE records the type of shift.
*
      IF( DMIN.LE.ZERO ) THEN
         TAU = -DMIN
         TTYPE = -1
         RETURN
      END IF
*
      NN = 4*N0 + PP
      IF( N0IN.EQ.N0 ) THEN
*
*        No eigenvalues deflated.
*
         IF( DMIN.EQ.DN .OR. DMIN.EQ.DN1 ) THEN
*
            OPS = OPS + DBLE( 7 )
            B1 = SQRT( Z( NN-3 ) )*SQRT( Z( NN-5 ) )
            B2 = SQRT( Z( NN-7 ) )*SQRT( Z( NN-9 ) )
            A2 = Z( NN-7 ) + Z( NN-5 )
*
*           Cases 2 and 3.
*
            IF( DMIN.EQ.DN .AND. DMIN1.EQ.DN1 ) THEN
               OPS = OPS + DBLE( 3 )
               GAP2 = DMIN2 - A2 - DMIN2*QURTR
               IF( GAP2.GT.ZERO .AND. GAP2.GT.B2 ) THEN
                  OPS = OPS + DBLE( 4 )
                  GAP1 = A2 - DN - ( B2 / GAP2 )*B2
               ELSE
                  OPS = OPS + DBLE( 3 )
                  GAP1 = A2 - DN - ( B1+B2 )
               END IF
               IF( GAP1.GT.ZERO .AND. GAP1.GT.B1 ) THEN
                  OPS = OPS + DBLE( 4 )
                  S = MAX( DN-( B1 / GAP1 )*B1, HALF*DMIN )
                  TTYPE = -2
               ELSE
                  OPS = OPS + DBLE( 2 )
                  S = ZERO
                  IF( DN.GT.B1 )
     $               S = DN - B1
                  IF( A2.GT.( B1+B2 ) )
     $               S = MIN( S, A2-( B1+B2 ) )
                  S = MAX( S, THIRD*DMIN )
                  TTYPE = -3
               END IF
            ELSE
*
*              Case 4.
*
               TTYPE = -4
               OPS = OPS + DBLE( 1 )
               S = QURTR*DMIN
               IF( DMIN.EQ.DN ) THEN
                  OPS = OPS + DBLE( 1 )
                  GAM = DN
                  A2 = ZERO
                  IF( Z( NN-5 ) .GT. Z( NN-7 ) )
     $               RETURN
                  B2 = Z( NN-5 ) / Z( NN-7 )
                  NP = NN - 9
               ELSE
                  OPS = OPS + DBLE( 2 )
                  NP = NN - 2*PP
                  B2 = Z( NP-2 )
                  GAM = DN1
                  IF( Z( NP-4 ) .GT. Z( NP-2 ) )
     $               RETURN
                  A2 = Z( NP-4 ) / Z( NP-2 )
                  IF( Z( NN-9 ) .GT. Z( NN-11 ) )
     $               RETURN
                  B2 = Z( NN-9 ) / Z( NN-11 )
                  NP = NN - 13
               END IF
*
*              Approximate contribution to norm squared from I < NN-1.
*
               A2 = A2 + B2
               DO 10 I4 = NP, 4*I0 - 1 + PP, -4
                  OPS = OPS + DBLE( 5 )
                  IF( B2.EQ.ZERO )
     $               GO TO 20
                  B1 = B2
                  IF( Z( I4 ) .GT. Z( I4-2 ) )
     $               RETURN
                  B2 = B2*( Z( I4 ) / Z( I4-2 ) )
                  A2 = A2 + B2
                  IF( HUNDRD*MAX( B2, B1 ).LT.A2 .OR. CNST1.LT.A2 )
     $               GO TO 20
   10          CONTINUE
   20          CONTINUE
               OPS = OPS + DBLE( 1 )
               A2 = CNST3*A2
*
*              Rayleigh quotient residual bound.
*
               OPS = OPS + DBLE( 5 )
               IF( A2.LT.CNST1 )
     $            S = GAM*( ONE-SQRT( A2 ) ) / ( ONE+A2 )
            END IF
         ELSE IF( DMIN.EQ.DN2 ) THEN
*
*           Case 5.
*
            TTYPE = -5
            OPS = OPS + DBLE( 1 )
            S = QURTR*DMIN
*
*           Compute contribution to norm squared from I > NN-2.
*
            OPS = OPS + DBLE( 4 )
            NP = NN - 2*PP
            B1 = Z( NP-2 )
            B2 = Z( NP-6 )
            GAM = DN2
            IF( Z( NP-8 ).GT.B2 .OR. Z( NP-4 ).GT.B1 )
     $         RETURN
            A2 = ( Z( NP-8 ) / B2 )*( ONE+Z( NP-4 ) / B1 )
*
*           Approximate contribution to norm squared from I < NN-2.
*
            IF( N0-I0.GT.2 ) THEN
               OPS = OPS + DBLE( 3 )
               B2 = Z( NN-13 ) / Z( NN-15 )
               A2 = A2 + B2
               DO 30 I4 = NN - 17, 4*I0 - 1 + PP, -4
                  OPS = OPS + DBLE( 5 )
                  IF( B2.EQ.ZERO )
     $               GO TO 40
                  B1 = B2
                  IF( Z( I4 ) .GT. Z( I4-2 ) )
     $               RETURN
                  B2 = B2*( Z( I4 ) / Z( I4-2 ) )
                  A2 = A2 + B2
                  IF( HUNDRD*MAX( B2, B1 ).LT.A2 .OR. CNST1.LT.A2 )
     $               GO TO 40
   30          CONTINUE
   40          CONTINUE
               A2 = CNST3*A2
            END IF
*
            OPS = OPS + DBLE( 5 )
            IF( A2.LT.CNST1 )
     $         S = GAM*( ONE-SQRT( A2 ) ) / ( ONE+A2 )
         ELSE
*
*           Case 6, no information to guide us.
*
            IF( TTYPE.EQ.-6 ) THEN
               OPS = OPS + DBLE( 3 )
               G = G + THIRD*( ONE-G )
            ELSE IF( TTYPE.EQ.-18 ) THEN
               OPS = OPS + DBLE( 1 )
               G = QURTR*THIRD
            ELSE
               G = QURTR
            END IF
            OPS = OPS + DBLE( 1 )
            S = G*DMIN
            TTYPE = -6
         END IF
*
      ELSE IF( N0IN.EQ.( N0+1 ) ) THEN
*
*        One eigenvalue just deflated. Use DMIN1, DN1 for DMIN and DN.
*
         IF( DMIN1.EQ.DN1 .AND. DMIN2.EQ.DN2 ) THEN
*
*           Cases 7 and 8.
*
            TTYPE = -7
            OPS = OPS + DBLE( 2 )
            S = THIRD*DMIN1
            IF( Z( NN-5 ).GT.Z( NN-7 ) )
     $         RETURN
            B1 = Z( NN-5 ) / Z( NN-7 )
            B2 = B1
            IF( B2.EQ.ZERO )
     $         GO TO 60
            DO 50 I4 = 4*N0 - 9 + PP, 4*I0 - 1 + PP, -4
               OPS = OPS + DBLE( 4 )
               A2 = B1
               IF( Z( I4 ).GT.Z( I4-2 ) )
     $            RETURN
               B1 = B1*( Z( I4 ) / Z( I4-2 ) )
               B2 = B2 + B1
               IF( HUNDRD*MAX( B1, A2 ).LT.B2 )
     $            GO TO 60
   50       CONTINUE
   60       CONTINUE
            OPS = OPS + DBLE( 8 )
            B2 = SQRT( CNST3*B2 )
            A2 = DMIN1 / ( ONE+B2**2 )
            GAP2 = HALF*DMIN2 - A2
            IF( GAP2.GT.ZERO .AND. GAP2.GT.B2*A2 ) THEN
               OPS = OPS + DBLE( 7 )
               S = MAX( S, A2*( ONE-CNST2*A2*( B2 / GAP2 )*B2 ) )
            ELSE
               OPS = OPS + DBLE( 4 )
               S = MAX( S, A2*( ONE-CNST2*B2 ) )
               TTYPE = -8
            END IF
         ELSE
*
*           Case 9.
*
            OPS = OPS + DBLE( 2 )
            S = QURTR*DMIN1
            IF( DMIN1.EQ.DN1 )
     $         S = HALF*DMIN1
            TTYPE = -9
         END IF
*
      ELSE IF( N0IN.EQ.( N0+2 ) ) THEN
*
*        Two eigenvalues deflated. Use DMIN2, DN2 for DMIN and DN.
*
*        Cases 10 and 11.
*
         OPS = OPS + DBLE( 1 )
         IF( DMIN2.EQ.DN2 .AND. TWO*Z( NN-5 ).LT.Z( NN-7 ) ) THEN
            TTYPE = -10
            OPS = OPS + DBLE( 1 )
            S = THIRD*DMIN2
            IF( Z( NN-5 ).GT.Z( NN-7 ) )
     $         RETURN
            B1 = Z( NN-5 ) / Z( NN-7 )
            B2 = B1
            IF( B2.EQ.ZERO )
     $         GO TO 80
            DO 70 I4 = 4*N0 - 9 + PP, 4*I0 - 1 + PP, -4
               OPS = OPS + DBLE( 4 )
               IF( Z( I4 ).GT.Z( I4-2 ) )
     $            RETURN
               B1 = B1*( Z( I4 ) / Z( I4-2 ) )
               B2 = B2 + B1
               IF( HUNDRD*B1.LT.B2 )
     $            GO TO 80
   70       CONTINUE
   80       CONTINUE
            OPS = OPS + DBLE( 12 )
            B2 = SQRT( CNST3*B2 )
            A2 = DMIN2 / ( ONE+B2**2 )
            GAP2 = Z( NN-7 ) + Z( NN-9 ) -
     $             SQRT( Z( NN-11 ) )*SQRT( Z( NN-9 ) ) - A2
            IF( GAP2.GT.ZERO .AND. GAP2.GT.B2*A2 ) THEN
               OPS = OPS + DBLE( 7 )
               S = MAX( S, A2*( ONE-CNST2*A2*( B2 / GAP2 )*B2 ) )
            ELSE
               OPS = OPS + DBLE( 4 )
               S = MAX( S, A2*( ONE-CNST2*B2 ) )
            END IF
         ELSE
            OPS = OPS + DBLE( 1 )
            S = QURTR*DMIN2
            TTYPE = -11
         END IF
      ELSE IF( N0IN.GT.( N0+2 ) ) THEN
*
*        Case 12, more than two eigenvalues deflated. No information.
*
         S = ZERO
         TTYPE = -12
      END IF
*
      TAU = S
      RETURN
*
*     End of DLASQ4
*
      END
      SUBROUTINE DLASQ5( I0, N0, Z, PP, TAU, DMIN, DMIN1, DMIN2, DN,
     $                   DNM1, DNM2, IEEE )
*
*  -- LAPACK auxiliary routine (instrumented to count ops, version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     May 17, 2000
*
*     .. Scalar Arguments ..
      LOGICAL            IEEE
      INTEGER            I0, N0, PP
      DOUBLE PRECISION   DMIN, DMIN1, DMIN2, DN, DNM1, DNM2, TAU
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   Z( * )
*     ..
*     .. Common block to return operation count ..
      COMMON             / LATIME / OPS, ITCNT
*     ..
*     .. Scalars in Common ..
      DOUBLE PRECISION   ITCNT, OPS
*     ..
*
*  Purpose
*  =======
*
*  DLASQ5 computes one dqds transform in ping-pong form, one
*  version for IEEE machines another for non IEEE machines.
*
*  Arguments
*  =========
*
*  I0    (input) INTEGER
*        First index.
*
*  N0    (input) INTEGER
*        Last index.
*
*  Z     (input) DOUBLE PRECISION array, dimension ( 4*N )
*        Z holds the qd array. EMIN is stored in Z(4*N0) to avoid
*        an extra argument.
*
*  PP    (input) INTEGER
*        PP=0 for ping, PP=1 for pong.
*
*  TAU   (input) DOUBLE PRECISION
*        This is the shift.
*
*  DMIN  (output) DOUBLE PRECISION
*        Minimum value of d.
*
*  DMIN1 (output) DOUBLE PRECISION
*        Minimum value of d, excluding D( N0 ).
*
*  DMIN2 (output) DOUBLE PRECISION
*        Minimum value of d, excluding D( N0 ) and D( N0-1 ).
*
*  DN    (output) DOUBLE PRECISION
*        d(N0), the last value of d.
*
*  DNM1  (output) DOUBLE PRECISION
*        d(N0-1).
*
*  DNM2  (output) DOUBLE PRECISION
*        d(N0-2).
*
*  IEEE  (input) LOGICAL
*        Flag for IEEE or non IEEE arithmetic.
*
*  =====================================================================
*
*     .. Parameter ..
      DOUBLE PRECISION   ZERO
      PARAMETER          ( ZERO = 0.0D0 )
*     ..
*     .. Local Scalars ..
      INTEGER            J4, J4P2
      DOUBLE PRECISION   D, EMIN, TEMP
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          DBLE, MIN
*     ..
*     .. Executable Statements ..
*
      IF( ( N0-I0-1 ).LE.0 )
     $   RETURN
*
      OPS = OPS + DBLE( 1 )
      J4 = 4*I0 + PP - 3
      EMIN = Z( J4+4 )
      D = Z( J4 ) - TAU
      DMIN = D
      DMIN1 = -Z( J4 )
*
      IF( IEEE ) THEN
*
*        Code for IEEE arithmetic.
*
         IF( PP.EQ.0 ) THEN
            DO 10 J4 = 4*I0, 4*( N0-3 ), 4
               OPS = OPS + DBLE( 5 )
               Z( J4-2 ) = D + Z( J4-1 )
               TEMP = Z( J4+1 ) / Z( J4-2 )
               D = D*TEMP - TAU
               DMIN = MIN( DMIN, D )
               Z( J4 ) = Z( J4-1 )*TEMP
               EMIN = MIN( Z( J4 ), EMIN )
   10       CONTINUE
         ELSE
            DO 20 J4 = 4*I0, 4*( N0-3 ), 4
               OPS = OPS + DBLE( 5 )
               Z( J4-3 ) = D + Z( J4 )
               TEMP = Z( J4+2 ) / Z( J4-3 )
               D = D*TEMP - TAU
               DMIN = MIN( DMIN, D )
               Z( J4-1 ) = Z( J4 )*TEMP
               EMIN = MIN( Z( J4-1 ), EMIN )
   20       CONTINUE
         END IF
*
*        Unroll last two steps.
*
         OPS = OPS + DBLE( 6 )
         DNM2 = D
         DMIN2 = DMIN
         J4 = 4*( N0-2 ) - PP
         J4P2 = J4 + 2*PP - 1
         Z( J4-2 ) = DNM2 + Z( J4P2 )
         Z( J4 ) = Z( J4P2+2 )*( Z( J4P2 ) / Z( J4-2 ) )
         DNM1 = Z( J4P2+2 )*( DNM2 / Z( J4-2 ) ) - TAU
         DMIN = MIN( DMIN, DNM1 )
*
         OPS = OPS + DBLE( 6 )
         DMIN1 = DMIN
         J4 = J4 + 4
         J4P2 = J4 + 2*PP - 1
         Z( J4-2 ) = DNM1 + Z( J4P2 )
         Z( J4 ) = Z( J4P2+2 )*( Z( J4P2 ) / Z( J4-2 ) )
         DN = Z( J4P2+2 )*( DNM1 / Z( J4-2 ) ) - TAU
         DMIN = MIN( DMIN, DN )
*
      ELSE
*
*        Code for non IEEE arithmetic.
*
         IF( PP.EQ.0 ) THEN
            DO 30 J4 = 4*I0, 4*( N0-3 ), 4
               Z( J4-2 ) = D + Z( J4-1 )
               IF( D.LT.ZERO ) THEN
                  RETURN
               ELSE
                  OPS = OPS + DBLE( 5 )
                  Z( J4 ) = Z( J4+1 )*( Z( J4-1 ) / Z( J4-2 ) )
                  D = Z( J4+1 )*( D / Z( J4-2 ) ) - TAU
               END IF
               DMIN = MIN( DMIN, D )
               EMIN = MIN( EMIN, Z( J4 ) )
   30       CONTINUE
         ELSE
            DO 40 J4 = 4*I0, 4*( N0-3 ), 4
               Z( J4-3 ) = D + Z( J4 )
               IF( D.LT.ZERO ) THEN
                  RETURN
               ELSE
                  OPS = OPS + DBLE( 5 )
                  Z( J4-1 ) = Z( J4+2 )*( Z( J4 ) / Z( J4-3 ) )
                  D = Z( J4+2 )*( D / Z( J4-3 ) ) - TAU
               END IF
               DMIN = MIN( DMIN, D )
               EMIN = MIN( EMIN, Z( J4-1 ) )
   40       CONTINUE
         END IF
*
*        Unroll last two steps.
*
         OPS = OPS + DBLE( 1 )
         DNM2 = D
         DMIN2 = DMIN
         J4 = 4*( N0-2 ) - PP
         J4P2 = J4 + 2*PP - 1
         Z( J4-2 ) = DNM2 + Z( J4P2 )
         IF( DNM2.LT.ZERO ) THEN
            RETURN
         ELSE
            OPS = OPS + DBLE( 5 )
            Z( J4 ) = Z( J4P2+2 )*( Z( J4P2 ) / Z( J4-2 ) )
            DNM1 = Z( J4P2+2 )*( DNM2 / Z( J4-2 ) ) - TAU
         END IF
         DMIN = MIN( DMIN, DNM1 )
*
         OPS = OPS + DBLE( 1 )
         DMIN1 = DMIN
         J4 = J4 + 4
         J4P2 = J4 + 2*PP - 1
         Z( J4-2 ) = DNM1 + Z( J4P2 )
         IF( DNM1.LT.ZERO ) THEN
            RETURN
         ELSE
            OPS = OPS + DBLE( 5 )
            Z( J4 ) = Z( J4P2+2 )*( Z( J4P2 ) / Z( J4-2 ) )
            DN = Z( J4P2+2 )*( DNM1 / Z( J4-2 ) ) - TAU
         END IF
         DMIN = MIN( DMIN, DN )
*
      END IF
*
      Z( J4+2 ) = DN
      Z( 4*N0-PP ) = EMIN
      RETURN
*
*     End of DLASQ5
*
      END
      SUBROUTINE DLASQ6( I0, N0, Z, PP, DMIN, DMIN1, DMIN2, DN,
     $                   DNM1, DNM2 )
*
*  -- LAPACK auxiliary routine (instrumented to count ops, version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     October 31, 1999
*
*     .. Scalar Arguments ..
      INTEGER            I0, N0, PP
      DOUBLE PRECISION   DMIN, DMIN1, DMIN2, DN, DNM1, DNM2
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   Z( * )
*     ..
*     .. Common block to return operation count ..
      COMMON             / LATIME / OPS, ITCNT
*     ..
*     .. Scalars in Common ..
      DOUBLE PRECISION   ITCNT, OPS
*     ..
*
*  Purpose
*  =======
*
*  DLASQ6 computes one dqd (shift equal to zero) transform in
*  ping-pong form, with protection against underflow and overflow.
*
*  Arguments
*  =========
*
*  I0    (input) INTEGER
*        First index.
*
*  N0    (input) INTEGER
*        Last index.
*
*  Z     (input) DOUBLE PRECISION array, dimension ( 4*N )
*        Z holds the qd array. EMIN is stored in Z(4*N0) to avoid
*        an extra argument.
*
*  PP    (input) INTEGER
*        PP=0 for ping, PP=1 for pong.
*
*  DMIN  (output) DOUBLE PRECISION
*        Minimum value of d.
*
*  DMIN1 (output) DOUBLE PRECISION
*        Minimum value of d, excluding D( N0 ).
*
*  DMIN2 (output) DOUBLE PRECISION
*        Minimum value of d, excluding D( N0 ) and D( N0-1 ).
*
*  DN    (output) DOUBLE PRECISION
*        d(N0), the last value of d.
*
*  DNM1  (output) DOUBLE PRECISION
*        d(N0-1).
*
*  DNM2  (output) DOUBLE PRECISION
*        d(N0-2).
*
*  =====================================================================
*
*     .. Parameter ..
      DOUBLE PRECISION   ZERO
      PARAMETER          ( ZERO = 0.0D0 )
*     ..
*     .. Local Scalars ..
      INTEGER            J4, J4P2
      DOUBLE PRECISION   D, EMIN, SAFMIN, TEMP
*     ..
*     .. External Function ..
      DOUBLE PRECISION   DLAMCH
      EXTERNAL           DLAMCH
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          DBLE, MIN
*     ..
*     .. Executable Statements ..
*
      IF( ( N0-I0-1 ).LE.0 )
     $   RETURN
*
      SAFMIN = DLAMCH( 'Safe minimum' )
      J4 = 4*I0 + PP - 3
      EMIN = Z( J4+4 ) 
      D = Z( J4 )
      DMIN = D
*
      IF( PP.EQ.0 ) THEN
         DO 10 J4 = 4*I0, 4*( N0-3 ), 4
            Z( J4-2 ) = D + Z( J4-1 ) 
            IF( Z( J4-2 ).EQ.ZERO ) THEN
               Z( J4 ) = ZERO
               D = Z( J4+1 )
               DMIN = D
               EMIN = ZERO
            ELSE IF( SAFMIN*Z( J4+1 ).LT.Z( J4-2 ) .AND.
     $               SAFMIN*Z( J4-2 ).LT.Z( J4+1 ) ) THEN
               OPS = OPS + DBLE( 2 )
               TEMP = Z( J4+1 ) / Z( J4-2 )
               Z( J4 ) = Z( J4-1 )*TEMP
               D = D*TEMP
            ELSE 
               OPS = OPS + DBLE( 4 )
               Z( J4 ) = Z( J4+1 )*( Z( J4-1 ) / Z( J4-2 ) )
               D = Z( J4+1 )*( D / Z( J4-2 ) )
            END IF
            DMIN = MIN( DMIN, D )
            EMIN = MIN( EMIN, Z( J4 ) )
   10    CONTINUE
      ELSE
         DO 20 J4 = 4*I0, 4*( N0-3 ), 4
            Z( J4-3 ) = D + Z( J4 ) 
            IF( Z( J4-3 ).EQ.ZERO ) THEN
               Z( J4-1 ) = ZERO
               D = Z( J4+2 )
               DMIN = D
               EMIN = ZERO
            ELSE IF( SAFMIN*Z( J4+2 ).LT.Z( J4-3 ) .AND.
     $               SAFMIN*Z( J4-3 ).LT.Z( J4+2 ) ) THEN
               OPS = OPS + DBLE( 2 )
               TEMP = Z( J4+2 ) / Z( J4-3 )
               Z( J4-1 ) = Z( J4 )*TEMP
               D = D*TEMP
            ELSE 
               OPS = OPS + DBLE( 4 )
               Z( J4-1 ) = Z( J4+2 )*( Z( J4 ) / Z( J4-3 ) )
               D = Z( J4+2 )*( D / Z( J4-3 ) )
            END IF
            DMIN = MIN( DMIN, D )
            EMIN = MIN( EMIN, Z( J4-1 ) )
   20    CONTINUE
      END IF
*
*     Unroll last two steps. 
*
      OPS = OPS + DBLE( 1 )
      DNM2 = D
      DMIN2 = DMIN
      J4 = 4*( N0-2 ) - PP
      J4P2 = J4 + 2*PP - 1
      Z( J4-2 ) = DNM2 + Z( J4P2 )
      IF( Z( J4-2 ).EQ.ZERO ) THEN
         Z( J4 ) = ZERO
         DNM1 = Z( J4P2+2 )
         DMIN = DNM1
         EMIN = ZERO
      ELSE IF( SAFMIN*Z( J4P2+2 ).LT.Z( J4-2 ) .AND.
     $         SAFMIN*Z( J4-2 ).LT.Z( J4P2+2 ) ) THEN
         OPS = OPS + DBLE( 3 )
         TEMP = Z( J4P2+2 ) / Z( J4-2 )
         Z( J4 ) = Z( J4P2 )*TEMP
         DNM1 = DNM2*TEMP
      ELSE
         OPS = OPS + DBLE( 4 )
         Z( J4 ) = Z( J4P2+2 )*( Z( J4P2 ) / Z( J4-2 ) )
         DNM1 = Z( J4P2+2 )*( DNM2 / Z( J4-2 ) )
      END IF
      DMIN = MIN( DMIN, DNM1 )
*
      OPS = OPS + DBLE( 1 )
      DMIN1 = DMIN
      J4 = J4 + 4
      J4P2 = J4 + 2*PP - 1
      Z( J4-2 ) = DNM1 + Z( J4P2 )
      IF( Z( J4-2 ).EQ.ZERO ) THEN
         Z( J4 ) = ZERO
         DN = Z( J4P2+2 )
         DMIN = DN
         EMIN = ZERO
      ELSE IF( SAFMIN*Z( J4P2+2 ).LT.Z( J4-2 ) .AND.
     $         SAFMIN*Z( J4-2 ).LT.Z( J4P2+2 ) ) THEN
         OPS = OPS + DBLE( 3 )
         TEMP = Z( J4P2+2 ) / Z( J4-2 )
         Z( J4 ) = Z( J4P2 )*TEMP
         DN = DNM1*TEMP
      ELSE
         OPS = OPS + DBLE( 4 )
         Z( J4 ) = Z( J4P2+2 )*( Z( J4P2 ) / Z( J4-2 ) )
         DN = Z( J4P2+2 )*( DNM1 / Z( J4-2 ) )
      END IF
      DMIN = MIN( DMIN, DN )
*
      Z( J4+2 ) = DN
      Z( 4*N0-PP ) = EMIN
      RETURN
*
*     End of DLASQ6
*
      END
      SUBROUTINE DPTEQR( COMPZ, N, D, E, Z, LDZ, WORK, INFO )
*
*  -- LAPACK routine (instrumented to count operations, version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     October 31, 1999
*
*     .. Scalar Arguments ..
      CHARACTER          COMPZ
      INTEGER            INFO, LDZ, N
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   D( * ), E( * ), WORK( * ), Z( LDZ, * )
*     ..
*     Common block to return operation count and iteration count
*     ITCNT is initialized to 0, OPS is only incremented
*     .. Common blocks ..
      COMMON             / LATIME / OPS, ITCNT
*     ..
*     .. Scalars in Common ..
      DOUBLE PRECISION   ITCNT, OPS
*     ..
*
*  Purpose
*  =======
*
*  DPTEQR computes all eigenvalues and, optionally, eigenvectors of a
*  symmetric positive definite tridiagonal matrix by first factoring the
*  matrix using DPTTRF, and then calling DBDSQR to compute the singular
*  values of the bidiagonal factor.
*
*  This routine computes the eigenvalues of the positive definite
*  tridiagonal matrix to high relative accuracy.  This means that if the
*  eigenvalues range over many orders of magnitude in size, then the
*  small eigenvalues and corresponding eigenvectors will be computed
*  more accurately than, for example, with the standard QR method.
*
*  The eigenvectors of a full or band symmetric positive definite matrix
*  can also be found if DSYTRD, DSPTRD, or DSBTRD has been used to
*  reduce this matrix to tridiagonal form. (The reduction to tridiagonal
*  form, however, may preclude the possibility of obtaining high
*  relative accuracy in the small eigenvalues of the original matrix, if
*  these eigenvalues range over many orders of magnitude.)
*
*  Arguments
*  =========
*
*  COMPZ   (input) CHARACTER*1
*          = 'N':  Compute eigenvalues only.
*          = 'V':  Compute eigenvectors of original symmetric
*                  matrix also.  Array Z contains the orthogonal
*                  matrix used to reduce the original matrix to
*                  tridiagonal form.
*          = 'I':  Compute eigenvectors of tridiagonal matrix also.
*
*  N       (input) INTEGER
*          The order of the matrix.  N >= 0.
*
*  D       (input/output) DOUBLE PRECISION array, dimension (N)
*          On entry, the n diagonal elements of the tridiagonal
*          matrix.
*          On normal exit, D contains the eigenvalues, in descending
*          order.
*
*  E       (input/output) DOUBLE PRECISION array, dimension (N-1)
*          On entry, the (n-1) subdiagonal elements of the tridiagonal
*          matrix.
*          On exit, E has been destroyed.
*
*  Z       (input/output) DOUBLE PRECISION array, dimension (LDZ, N)
*          On entry, if COMPZ = 'V', the orthogonal matrix used in the
*          reduction to tridiagonal form.
*          On exit, if COMPZ = 'V', the orthonormal eigenvectors of the
*          original symmetric matrix;
*          if COMPZ = 'I', the orthonormal eigenvectors of the
*          tridiagonal matrix.
*          If INFO > 0 on exit, Z contains the eigenvectors associated
*          with only the stored eigenvalues.
*          If  COMPZ = 'N', then Z is not referenced.
*
*  LDZ     (input) INTEGER
*          The leading dimension of the array Z.  LDZ >= 1, and if
*          COMPZ = 'V' or 'I', LDZ >= max(1,N).
*
*  WORK    (workspace) DOUBLE PRECISION array, dimension (4*N)
*
*  INFO    (output) INTEGER
*          = 0:  successful exit.
*          < 0:  if INFO = -i, the i-th argument had an illegal value.
*          > 0:  if INFO = i, and i is:
*                <= N  the Cholesky factorization of the matrix could
*                      not be performed because the i-th principal minor
*                      was not positive definite.
*                > N   the SVD algorithm failed to converge;
*                      if INFO = N+i, i off-diagonal elements of the
*                      bidiagonal factor did not converge to zero.
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE
      PARAMETER          ( ZERO = 0.0D0, ONE = 1.0D0 )
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
*     ..
*     .. External Subroutines ..
      EXTERNAL           DBDSQR, DLASET, DPTTRF, XERBLA
*     ..
*     .. Local Arrays ..
      DOUBLE PRECISION   C( 1, 1 ), VT( 1, 1 )
*     ..
*     .. Local Scalars ..
      INTEGER            I, ICOMPZ, NRU
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX, SQRT
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters.
*
      INFO = 0
*
      IF( LSAME( COMPZ, 'N' ) ) THEN
         ICOMPZ = 0
      ELSE IF( LSAME( COMPZ, 'V' ) ) THEN
         ICOMPZ = 1
      ELSE IF( LSAME( COMPZ, 'I' ) ) THEN
         ICOMPZ = 2
      ELSE
         ICOMPZ = -1
      END IF
      IF( ICOMPZ.LT.0 ) THEN
         INFO = -1
      ELSE IF( N.LT.0 ) THEN
         INFO = -2
      ELSE IF( ( LDZ.LT.1 ) .OR. ( ICOMPZ.GT.0 .AND. LDZ.LT.MAX( 1,
     $         N ) ) ) THEN
         INFO = -6
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DPTEQR', -INFO )
         RETURN
      END IF
*
*     Quick return if possible
*
      IF( N.EQ.0 )
     $   RETURN
*
      IF( N.EQ.1 ) THEN
         IF( ICOMPZ.GT.0 )
     $      Z( 1, 1 ) = ONE
         RETURN
      END IF
      IF( ICOMPZ.EQ.2 )
     $   CALL DLASET( 'Full', N, N, ZERO, ONE, Z, LDZ )
*
*     Call DPTTRF to factor the matrix.
*
      OPS = OPS + 5*N - 4
      CALL DPTTRF( N, D, E, INFO )
      IF( INFO.NE.0 )
     $   RETURN
      DO 10 I = 1, N
         D( I ) = SQRT( D( I ) )
   10 CONTINUE
      DO 20 I = 1, N - 1
         E( I ) = E( I )*D( I )
   20 CONTINUE
*
*     Call DBDSQR to compute the singular values/vectors of the
*     bidiagonal factor.
*
      IF( ICOMPZ.GT.0 ) THEN
         NRU = N
      ELSE
         NRU = 0
      END IF
      CALL DBDSQR( 'Lower', N, 0, NRU, 0, D, E, VT, 1, Z, LDZ, C, 1,
     $             WORK, INFO )
*
*     Square the singular values.
*
      IF( INFO.EQ.0 ) THEN
         OPS = OPS + N
         DO 30 I = 1, N
            D( I ) = D( I )*D( I )
   30    CONTINUE
      ELSE
         INFO = N + INFO
      END IF
*
      RETURN
*
*     End of DPTEQR
*
      END
      SUBROUTINE DSTEBZ( RANGE, ORDER, N, VL, VU, IL, IU, ABSTOL, D, E,
     $                   M, NSPLIT, W, IBLOCK, ISPLIT, WORK, IWORK,
     $                   INFO )
*
*  -- LAPACK routine (instrumented to count operations, version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     June 30, 1999
*
*     .. Scalar Arguments ..
      CHARACTER          ORDER, RANGE
      INTEGER            IL, INFO, IU, M, N, NSPLIT
      DOUBLE PRECISION   ABSTOL, VL, VU
*     ..
*     .. Array Arguments ..
      INTEGER            IBLOCK( * ), ISPLIT( * ), IWORK( * )
      DOUBLE PRECISION   D( * ), E( * ), W( * ), WORK( * )
*     ..
*     Common block to return operation count and iteration count
*     ITCNT is initialized to 0, OPS is only incremented
*     .. Common blocks ..
      COMMON             / LATIME / OPS, ITCNT
*     ..
*     .. Scalars in Common ..
      DOUBLE PRECISION   ITCNT, OPS
*     ..
*
*  Purpose
*  =======
*
*  DSTEBZ computes the eigenvalues of a symmetric tridiagonal
*  matrix T.  The user may ask for all eigenvalues, all eigenvalues
*  in the half-open interval (VL, VU], or the IL-th through IU-th
*  eigenvalues.
*
*  To avoid overflow, the matrix must be scaled so that its
*  largest element is no greater than overflow**(1/2) *
*  underflow**(1/4) in absolute value, and for greatest
*  accuracy, it should not be much smaller than that.
*
*  See W. Kahan "Accurate Eigenvalues of a Symmetric Tridiagonal
*  Matrix", Report CS41, Computer Science Dept., Stanford
*  University, July 21, 1966.
*
*  Arguments
*  =========
*
*  RANGE   (input) CHARACTER
*          = 'A': ("All")   all eigenvalues will be found.
*          = 'V': ("Value") all eigenvalues in the half-open interval
*                           (VL, VU] will be found.
*          = 'I': ("Index") the IL-th through IU-th eigenvalues (of the
*                           entire matrix) will be found.
*
*  ORDER   (input) CHARACTER
*          = 'B': ("By Block") the eigenvalues will be grouped by
*                              split-off block (see IBLOCK, ISPLIT) and
*                              ordered from smallest to largest within
*                              the block.
*          = 'E': ("Entire matrix")
*                              the eigenvalues for the entire matrix
*                              will be ordered from smallest to
*                              largest.
*
*  N       (input) INTEGER
*          The order of the tridiagonal matrix T.  N >= 0.
*
*  VL      (input) DOUBLE PRECISION
*  VU      (input) DOUBLE PRECISION
*          If RANGE='V', the lower and upper bounds of the interval to
*          be searched for eigenvalues.  Eigenvalues less than or equal
*          to VL, or greater than VU, will not be returned.  VL < VU.
*          Not referenced if RANGE = 'A' or 'I'.
*
*  IL      (input) INTEGER
*  IU      (input) INTEGER
*          If RANGE='I', the indices (in ascending order) of the
*          smallest and largest eigenvalues to be returned.
*          1 <= IL <= IU <= N, if N > 0; IL = 1 and IU = 0 if N = 0.
*          Not referenced if RANGE = 'A' or 'V'.
*
*  ABSTOL  (input) DOUBLE PRECISION
*          The absolute tolerance for the eigenvalues.  An eigenvalue
*          (or cluster) is considered to be located if it has been
*          determined to lie in an interval whose width is ABSTOL or
*          less.  If ABSTOL is less than or equal to zero, then ULP*|T|
*          will be used, where |T| means the 1-norm of T.
*
*          Eigenvalues will be computed most accurately when ABSTOL is
*          set to twice the underflow threshold 2*DLAMCH('S'), not zero.
*
*  D       (input) DOUBLE PRECISION array, dimension (N)
*          The n diagonal elements of the tridiagonal matrix T.
*
*  E       (input) DOUBLE PRECISION array, dimension (N-1)
*          The (n-1) off-diagonal elements of the tridiagonal matrix T.
*
*  M       (output) INTEGER
*          The actual number of eigenvalues found. 0 <= M <= N.
*          (See also the description of INFO=2,3.)
*
*  NSPLIT  (output) INTEGER
*          The number of diagonal blocks in the matrix T.
*          1 <= NSPLIT <= N.
*
*  W       (output) DOUBLE PRECISION array, dimension (N)
*          On exit, the first M elements of W will contain the
*          eigenvalues.  (DSTEBZ may use the remaining N-M elements as
*          workspace.)
*
*  IBLOCK  (output) INTEGER array, dimension (N)
*          At each row/column j where E(j) is zero or small, the
*          matrix T is considered to split into a block diagonal
*          matrix.  On exit, if INFO = 0, IBLOCK(i) specifies to which
*          block (from 1 to the number of blocks) the eigenvalue W(i)
*          belongs.  (DSTEBZ may use the remaining N-M elements as
*          workspace.)
*
*  ISPLIT  (output) INTEGER array, dimension (N)
*          The splitting points, at which T breaks up into submatrices.
*          The first submatrix consists of rows/columns 1 to ISPLIT(1),
*          the second of rows/columns ISPLIT(1)+1 through ISPLIT(2),
*          etc., and the NSPLIT-th consists of rows/columns
*          ISPLIT(NSPLIT-1)+1 through ISPLIT(NSPLIT)=N.
*          (Only the first NSPLIT elements will actually be used, but
*          since the user cannot know a priori what value NSPLIT will
*          have, N words must be reserved for ISPLIT.)
*
*  WORK    (workspace) DOUBLE PRECISION array, dimension (4*N)
*
*  IWORK   (workspace) INTEGER array, dimension (3*N)
*
*  INFO    (output) INTEGER
*          = 0:  successful exit
*          < 0:  if INFO = -i, the i-th argument had an illegal value
*          > 0:  some or all of the eigenvalues failed to converge or
*                were not computed:
*                =1 or 3: Bisection failed to converge for some
*                        eigenvalues; these eigenvalues are flagged by a
*                        negative block number.  The effect is that the
*                        eigenvalues may not be as accurate as the
*                        absolute and relative tolerances.  This is
*                        generally caused by unexpectedly inaccurate
*                        arithmetic.
*                =2 or 3: RANGE='I' only: Not all of the eigenvalues
*                        IL:IU were found.
*                        Effect: M < IU+1-IL
*                        Cause:  non-monotonic arithmetic, causing the
*                                Sturm sequence to be non-monotonic.
*                        Cure:   recalculate, using RANGE='A', and pick
*                                out eigenvalues IL:IU.  In some cases,
*                                increasing the PARAMETER "FUDGE" may
*                                make things work.
*                = 4:    RANGE='I', and the Gershgorin interval
*                        initially used was too small.  No eigenvalues
*                        were computed.
*                        Probable cause: your machine has sloppy
*                                        floating-point arithmetic.
*                        Cure: Increase the PARAMETER "FUDGE",
*                              recompile, and try again.
*
*  Internal Parameters
*  ===================
*
*  RELFAC  DOUBLE PRECISION, default = 2.0e0
*          The relative tolerance.  An interval (a,b] lies within
*          "relative tolerance" if  b-a < RELFAC*ulp*max(|a|,|b|),
*          where "ulp" is the machine precision (distance from 1 to
*          the next larger floating point number.)
*
*  FUDGE   DOUBLE PRECISION, default = 2
*          A "fudge factor" to widen the Gershgorin intervals.  Ideally,
*          a value of 1 should work, but on machines with sloppy
*          arithmetic, this needs to be larger.  The default for
*          publicly released versions should be large enough to handle
*          the worst machine around.  Note that this has no effect
*          on accuracy of the solution.
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE, TWO, HALF
      PARAMETER          ( ZERO = 0.0D0, ONE = 1.0D0, TWO = 2.0D0,
     $                   HALF = 1.0D0 / TWO )
      DOUBLE PRECISION   FUDGE, RELFAC
      PARAMETER          ( FUDGE = 2.0D0, RELFAC = 2.0D0 )
*     ..
*     .. Local Scalars ..
      LOGICAL            NCNVRG, TOOFEW
      INTEGER            IB, IBEGIN, IDISCL, IDISCU, IE, IEND, IINFO,
     $                   IM, IN, IOFF, IORDER, IOUT, IRANGE, ITMAX,
     $                   ITMP1, IW, IWOFF, J, JB, JDISC, JE, NB, NWL,
     $                   NWU
      DOUBLE PRECISION   ATOLI, BNORM, GL, GU, PIVMIN, RTOLI, SAFEMN,
     $                   TMP1, TMP2, TNORM, ULP, WKILL, WL, WLU, WU, WUL
*     ..
*     .. Local Arrays ..
      INTEGER            IDUMMA( 1 )
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      INTEGER            ILAENV
      DOUBLE PRECISION   DLAMCH
      EXTERNAL           LSAME, ILAENV, DLAMCH
*     ..
*     .. External Subroutines ..
      EXTERNAL           DLAEBZ, XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, INT, LOG, MAX, MIN, SQRT
*     ..
*     .. Executable Statements ..
*
      INFO = 0
*
*     Decode RANGE
*
      IF( LSAME( RANGE, 'A' ) ) THEN
         IRANGE = 1
      ELSE IF( LSAME( RANGE, 'V' ) ) THEN
         IRANGE = 2
      ELSE IF( LSAME( RANGE, 'I' ) ) THEN
         IRANGE = 3
      ELSE
         IRANGE = 0
      END IF
*
*     Decode ORDER
*
      IF( LSAME( ORDER, 'B' ) ) THEN
         IORDER = 2
      ELSE IF( LSAME( ORDER, 'E' ) ) THEN
         IORDER = 1
      ELSE
         IORDER = 0
      END IF
*
*     Check for Errors
*
      IF( IRANGE.LE.0 ) THEN
         INFO = -1
      ELSE IF( IORDER.LE.0 ) THEN
         INFO = -2
      ELSE IF( N.LT.0 ) THEN
         INFO = -3
      ELSE IF( IRANGE.EQ.2 ) THEN
         IF( VL.GE.VU )
     $      INFO = -5
      ELSE IF( IRANGE.EQ.3 .AND. ( IL.LT.1 .OR. IL.GT.MAX( 1, N ) ) )
     $          THEN
         INFO = -6
      ELSE IF( IRANGE.EQ.3 .AND. ( IU.LT.MIN( N, IL ) .OR. IU.GT.N ) )
     $          THEN
         INFO = -7
      END IF
*
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DSTEBZ', -INFO )
         RETURN
      END IF
*
*     Initialize error flags
*
      INFO = 0
      NCNVRG = .FALSE.
      TOOFEW = .FALSE.
*
*     Quick return if possible
*
      M = 0
      IF( N.EQ.0 )
     $   RETURN
*
*     Simplifications:
*
      IF( IRANGE.EQ.3 .AND. IL.EQ.1 .AND. IU.EQ.N )
     $   IRANGE = 1
*
*     Get machine constants
*     NB is the minimum vector length for vector bisection, or 0
*     if only scalar is to be done.
*
      SAFEMN = DLAMCH( 'S' )
      ULP = DLAMCH( 'P' )
      OPS = OPS + 1
      RTOLI = ULP*RELFAC
      NB = ILAENV( 1, 'DSTEBZ', ' ', N, -1, -1, -1 )
      IF( NB.LE.1 )
     $   NB = 0
*
*     Special Case when N=1
*
      IF( N.EQ.1 ) THEN
         NSPLIT = 1
         ISPLIT( 1 ) = 1
         IF( IRANGE.EQ.2 .AND. ( VL.GE.D( 1 ) .OR. VU.LT.D( 1 ) ) ) THEN
            M = 0
         ELSE
            W( 1 ) = D( 1 )
            IBLOCK( 1 ) = 1
            M = 1
         END IF
         RETURN
      END IF
*
*     Compute Splitting Points
*
      NSPLIT = 1
      WORK( N ) = ZERO
      PIVMIN = ONE
*
      OPS = OPS + ( N-1 )*5 + 1
*DIR$ NOVECTOR
      DO 10 J = 2, N
         TMP1 = E( J-1 )**2
         IF( ABS( D( J )*D( J-1 ) )*ULP**2+SAFEMN.GT.TMP1 ) THEN
            ISPLIT( NSPLIT ) = J - 1
            NSPLIT = NSPLIT + 1
            WORK( J-1 ) = ZERO
         ELSE
            WORK( J-1 ) = TMP1
            PIVMIN = MAX( PIVMIN, TMP1 )
         END IF
   10 CONTINUE
      ISPLIT( NSPLIT ) = N
      PIVMIN = PIVMIN*SAFEMN
*
*     Compute Interval and ATOLI
*
      IF( IRANGE.EQ.3 ) THEN
*
*        RANGE='I': Compute the interval containing eigenvalues
*                   IL through IU.
*
*        Compute Gershgorin interval for entire (split) matrix
*        and use it as the initial interval
*
         GU = D( 1 )
         GL = D( 1 )
         TMP1 = ZERO
*
         OPS = OPS + 5*( N-1 ) + 23
         DO 20 J = 1, N - 1
            TMP2 = SQRT( WORK( J ) )
            GU = MAX( GU, D( J )+TMP1+TMP2 )
            GL = MIN( GL, D( J )-TMP1-TMP2 )
            TMP1 = TMP2
   20    CONTINUE
*
         GU = MAX( GU, D( N )+TMP1 )
         GL = MIN( GL, D( N )-TMP1 )
         TNORM = MAX( ABS( GL ), ABS( GU ) )
         GL = GL - FUDGE*TNORM*ULP*N - FUDGE*TWO*PIVMIN
         GU = GU + FUDGE*TNORM*ULP*N + FUDGE*PIVMIN
*
*        Compute Iteration parameters
*
         ITMAX = INT( ( LOG( TNORM+PIVMIN )-LOG( PIVMIN ) ) /
     $           LOG( TWO ) ) + 2
         IF( ABSTOL.LE.ZERO ) THEN
            ATOLI = ULP*TNORM
         ELSE
            ATOLI = ABSTOL
         END IF
*
         WORK( N+1 ) = GL
         WORK( N+2 ) = GL
         WORK( N+3 ) = GU
         WORK( N+4 ) = GU
         WORK( N+5 ) = GL
         WORK( N+6 ) = GU
         IWORK( 1 ) = -1
         IWORK( 2 ) = -1
         IWORK( 3 ) = N + 1
         IWORK( 4 ) = N + 1
         IWORK( 5 ) = IL - 1
         IWORK( 6 ) = IU
*
         CALL DLAEBZ( 3, ITMAX, N, 2, 2, NB, ATOLI, RTOLI, PIVMIN, D, E,
     $                WORK, IWORK( 5 ), WORK( N+1 ), WORK( N+5 ), IOUT,
     $                IWORK, W, IBLOCK, IINFO )
*
         IF( IWORK( 6 ).EQ.IU ) THEN
            WL = WORK( N+1 )
            WLU = WORK( N+3 )
            NWL = IWORK( 1 )
            WU = WORK( N+4 )
            WUL = WORK( N+2 )
            NWU = IWORK( 4 )
         ELSE
            WL = WORK( N+2 )
            WLU = WORK( N+4 )
            NWL = IWORK( 2 )
            WU = WORK( N+3 )
            WUL = WORK( N+1 )
            NWU = IWORK( 3 )
         END IF
*
         IF( NWL.LT.0 .OR. NWL.GE.N .OR. NWU.LT.1 .OR. NWU.GT.N ) THEN
            INFO = 4
            RETURN
         END IF
      ELSE
*
*        RANGE='A' or 'V' -- Set ATOLI
*
         OPS = OPS + 3 + 2*( N-2 )
         TNORM = MAX( ABS( D( 1 ) )+ABS( E( 1 ) ),
     $           ABS( D( N ) )+ABS( E( N-1 ) ) )
*
         DO 30 J = 2, N - 1
            TNORM = MAX( TNORM, ABS( D( J ) )+ABS( E( J-1 ) )+
     $              ABS( E( J ) ) )
   30    CONTINUE
*
         IF( ABSTOL.LE.ZERO ) THEN
            ATOLI = ULP*TNORM
         ELSE
            ATOLI = ABSTOL
         END IF
*
         IF( IRANGE.EQ.2 ) THEN
            WL = VL
            WU = VU
         ELSE
            WL = ZERO
            WU = ZERO
         END IF
      END IF
*
*     Find Eigenvalues -- Loop Over Blocks and recompute NWL and NWU.
*     NWL accumulates the number of eigenvalues .le. WL,
*     NWU accumulates the number of eigenvalues .le. WU
*
      M = 0
      IEND = 0
      INFO = 0
      NWL = 0
      NWU = 0
*
      DO 70 JB = 1, NSPLIT
         IOFF = IEND
         IBEGIN = IOFF + 1
         IEND = ISPLIT( JB )
         IN = IEND - IOFF
*
         IF( IN.EQ.1 ) THEN
*
*           Special Case -- IN=1
*
            OPS = OPS + 4
            IF( IRANGE.EQ.1 .OR. WL.GE.D( IBEGIN )-PIVMIN )
     $         NWL = NWL + 1
            IF( IRANGE.EQ.1 .OR. WU.GE.D( IBEGIN )-PIVMIN )
     $         NWU = NWU + 1
            IF( IRANGE.EQ.1 .OR. ( WL.LT.D( IBEGIN )-PIVMIN .AND. WU.GE.
     $          D( IBEGIN )-PIVMIN ) ) THEN
               M = M + 1
               W( M ) = D( IBEGIN )
               IBLOCK( M ) = JB
            END IF
         ELSE
*
*           General Case -- IN > 1
*
*           Compute Gershgorin Interval
*           and use it as the initial interval
*
            GU = D( IBEGIN )
            GL = D( IBEGIN )
            TMP1 = ZERO
*
            OPS = OPS + 4*( IEND-IBEGIN ) + 13
            DO 40 J = IBEGIN, IEND - 1
               TMP2 = ABS( E( J ) )
               GU = MAX( GU, D( J )+TMP1+TMP2 )
               GL = MIN( GL, D( J )-TMP1-TMP2 )
               TMP1 = TMP2
   40       CONTINUE
*
            GU = MAX( GU, D( IEND )+TMP1 )
            GL = MIN( GL, D( IEND )-TMP1 )
            BNORM = MAX( ABS( GL ), ABS( GU ) )
            GL = GL - FUDGE*BNORM*ULP*IN - FUDGE*PIVMIN
            GU = GU + FUDGE*BNORM*ULP*IN + FUDGE*PIVMIN
*
*           Compute ATOLI for the current submatrix
*
            IF( ABSTOL.LE.ZERO ) THEN
               ATOLI = ULP*MAX( ABS( GL ), ABS( GU ) )
            ELSE
               ATOLI = ABSTOL
            END IF
*
            IF( IRANGE.GT.1 ) THEN
               IF( GU.LT.WL ) THEN
                  NWL = NWL + IN
                  NWU = NWU + IN
                  GO TO 70
               END IF
               GL = MAX( GL, WL )
               GU = MIN( GU, WU )
               IF( GL.GE.GU )
     $            GO TO 70
            END IF
*
*           Set Up Initial Interval
*
            WORK( N+1 ) = GL
            WORK( N+IN+1 ) = GU
            CALL DLAEBZ( 1, 0, IN, IN, 1, NB, ATOLI, RTOLI, PIVMIN,
     $                   D( IBEGIN ), E( IBEGIN ), WORK( IBEGIN ),
     $                   IDUMMA, WORK( N+1 ), WORK( N+2*IN+1 ), IM,
     $                   IWORK, W( M+1 ), IBLOCK( M+1 ), IINFO )
*
            NWL = NWL + IWORK( 1 )
            NWU = NWU + IWORK( IN+1 )
            IWOFF = M - IWORK( 1 )
*
*           Compute Eigenvalues
*
            OPS = OPS + 8
            ITMAX = INT( ( LOG( GU-GL+PIVMIN )-LOG( PIVMIN ) ) /
     $              LOG( TWO ) ) + 2
            CALL DLAEBZ( 2, ITMAX, IN, IN, 1, NB, ATOLI, RTOLI, PIVMIN,
     $                   D( IBEGIN ), E( IBEGIN ), WORK( IBEGIN ),
     $                   IDUMMA, WORK( N+1 ), WORK( N+2*IN+1 ), IOUT,
     $                   IWORK, W( M+1 ), IBLOCK( M+1 ), IINFO )
*
*           Copy Eigenvalues Into W and IBLOCK
*           Use -JB for block number for unconverged eigenvalues.
*
            OPS = OPS + 2*IOUT
            DO 60 J = 1, IOUT
               TMP1 = HALF*( WORK( J+N )+WORK( J+IN+N ) )
*
*              Flag non-convergence.
*
               IF( J.GT.IOUT-IINFO ) THEN
                  NCNVRG = .TRUE.
                  IB = -JB
               ELSE
                  IB = JB
               END IF
               DO 50 JE = IWORK( J ) + 1 + IWOFF,
     $                 IWORK( J+IN ) + IWOFF
                  W( JE ) = TMP1
                  IBLOCK( JE ) = IB
   50          CONTINUE
   60       CONTINUE
*
            M = M + IM
         END IF
   70 CONTINUE
*
*     If RANGE='I', then (WL,WU) contains eigenvalues NWL+1,...,NWU
*     If NWL+1 < IL or NWU > IU, discard extra eigenvalues.
*
      IF( IRANGE.EQ.3 ) THEN
         IM = 0
         IDISCL = IL - 1 - NWL
         IDISCU = NWU - IU
*
         IF( IDISCL.GT.0 .OR. IDISCU.GT.0 ) THEN
            DO 80 JE = 1, M
               IF( W( JE ).LE.WLU .AND. IDISCL.GT.0 ) THEN
                  IDISCL = IDISCL - 1
               ELSE IF( W( JE ).GE.WUL .AND. IDISCU.GT.0 ) THEN
                  IDISCU = IDISCU - 1
               ELSE
                  IM = IM + 1
                  W( IM ) = W( JE )
                  IBLOCK( IM ) = IBLOCK( JE )
               END IF
   80       CONTINUE
            M = IM
         END IF
         IF( IDISCL.GT.0 .OR. IDISCU.GT.0 ) THEN
*
*           Code to deal with effects of bad arithmetic:
*           Some low eigenvalues to be discarded are not in (WL,WLU],
*           or high eigenvalues to be discarded are not in (WUL,WU]
*           so just kill off the smallest IDISCL/largest IDISCU
*           eigenvalues, by simply finding the smallest/largest
*           eigenvalue(s).
*
*           (If N(w) is monotone non-decreasing, this should never
*               happen.)
*
            IF( IDISCL.GT.0 ) THEN
               WKILL = WU
               DO 100 JDISC = 1, IDISCL
                  IW = 0
                  DO 90 JE = 1, M
                     IF( IBLOCK( JE ).NE.0 .AND.
     $                   ( W( JE ).LT.WKILL .OR. IW.EQ.0 ) ) THEN
                        IW = JE
                        WKILL = W( JE )
                     END IF
   90             CONTINUE
                  IBLOCK( IW ) = 0
  100          CONTINUE
            END IF
            IF( IDISCU.GT.0 ) THEN
*
               WKILL = WL
               DO 120 JDISC = 1, IDISCU
                  IW = 0
                  DO 110 JE = 1, M
                     IF( IBLOCK( JE ).NE.0 .AND.
     $                   ( W( JE ).GT.WKILL .OR. IW.EQ.0 ) ) THEN
                        IW = JE
                        WKILL = W( JE )
                     END IF
  110             CONTINUE
                  IBLOCK( IW ) = 0
  120          CONTINUE
            END IF
            IM = 0
            DO 130 JE = 1, M
               IF( IBLOCK( JE ).NE.0 ) THEN
                  IM = IM + 1
                  W( IM ) = W( JE )
                  IBLOCK( IM ) = IBLOCK( JE )
               END IF
  130       CONTINUE
            M = IM
         END IF
         IF( IDISCL.LT.0 .OR. IDISCU.LT.0 ) THEN
            TOOFEW = .TRUE.
         END IF
      END IF
*
*     If ORDER='B', do nothing -- the eigenvalues are already sorted
*        by block.
*     If ORDER='E', sort the eigenvalues from smallest to largest
*
      IF( IORDER.EQ.1 .AND. NSPLIT.GT.1 ) THEN
         DO 150 JE = 1, M - 1
            IE = 0
            TMP1 = W( JE )
            DO 140 J = JE + 1, M
               IF( W( J ).LT.TMP1 ) THEN
                  IE = J
                  TMP1 = W( J )
               END IF
  140       CONTINUE
*
            IF( IE.NE.0 ) THEN
               ITMP1 = IBLOCK( IE )
               W( IE ) = W( JE )
               IBLOCK( IE ) = IBLOCK( JE )
               W( JE ) = TMP1
               IBLOCK( JE ) = ITMP1
            END IF
  150    CONTINUE
      END IF
*
      INFO = 0
      IF( NCNVRG )
     $   INFO = INFO + 1
      IF( TOOFEW )
     $   INFO = INFO + 2
      RETURN
*
*     End of DSTEBZ
*
      END
      SUBROUTINE DSTEDC( COMPZ, N, D, E, Z, LDZ, WORK, LWORK, IWORK,
     $                   LIWORK, INFO )
*
*  -- LAPACK driver routine (instrum. to count ops, version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     June 30, 1999
*
*     .. Scalar Arguments ..
      CHARACTER          COMPZ
      INTEGER            INFO, LDZ, LIWORK, LWORK, N
*     ..
*     .. Array Arguments ..
      INTEGER            IWORK( * )
      DOUBLE PRECISION   D( * ), E( * ), WORK( * ), Z( LDZ, * )
*     ..
*     Common block to return operation count and iteration count
*     ITCNT is initialized to 0, OPS is only incremented
*     .. Common blocks ..
      COMMON             / LATIME / OPS, ITCNT
*     ..
*     .. Scalars in Common ..
      DOUBLE PRECISION   ITCNT, OPS
*     ..
*
*  Purpose
*  =======
*
*  DSTEDC computes all eigenvalues and, optionally, eigenvectors of a
*  symmetric tridiagonal matrix using the divide and conquer method.
*  The eigenvectors of a full or band real symmetric matrix can also be
*  found if DSYTRD or DSPTRD or DSBTRD has been used to reduce this
*  matrix to tridiagonal form.
*
*  This code makes very mild assumptions about floating point
*  arithmetic. It will work on machines with a guard digit in
*  add/subtract, or on those binary machines without guard digits
*  which subtract like the Cray X-MP, Cray Y-MP, Cray C-90, or Cray-2.
*  It could conceivably fail on hexadecimal or decimal machines
*  without guard digits, but we know of none.  See DLAED3 for details.
*
*  Arguments
*  =========
*
*  COMPZ   (input) CHARACTER*1
*          = 'N':  Compute eigenvalues only.
*          = 'I':  Compute eigenvectors of tridiagonal matrix also.
*          = 'V':  Compute eigenvectors of original dense symmetric
*                  matrix also.  On entry, Z contains the orthogonal
*                  matrix used to reduce the original matrix to
*                  tridiagonal form.
*
*  N       (input) INTEGER
*          The dimension of the symmetric tridiagonal matrix.  N >= 0.
*
*  D       (input/output) DOUBLE PRECISION array, dimension (N)
*          On entry, the diagonal elements of the tridiagonal matrix.
*          On exit, if INFO = 0, the eigenvalues in ascending order.
*
*  E       (input/output) DOUBLE PRECISION array, dimension (N-1)
*          On entry, the subdiagonal elements of the tridiagonal matrix.
*          On exit, E has been destroyed.
*
*  Z       (input/output) DOUBLE PRECISION array, dimension (LDZ,N)
*          On entry, if COMPZ = 'V', then Z contains the orthogonal
*          matrix used in the reduction to tridiagonal form.
*          On exit, if INFO = 0, then if COMPZ = 'V', Z contains the
*          orthonormal eigenvectors of the original symmetric matrix,
*          and if COMPZ = 'I', Z contains the orthonormal eigenvectors
*          of the symmetric tridiagonal matrix.
*          If  COMPZ = 'N', then Z is not referenced.
*
*  LDZ     (input) INTEGER
*          The leading dimension of the array Z.  LDZ >= 1.
*          If eigenvectors are desired, then LDZ >= max(1,N).
*
*  WORK    (workspace/output) DOUBLE PRECISION array,
*                                         dimension (LWORK)
*          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
*
*  LWORK   (input) INTEGER
*          The dimension of the array WORK.
*          If COMPZ = 'N' or N <= 1 then LWORK must be at least 1.
*          If COMPZ = 'V' and N > 1 then LWORK must be at least
*                         ( 1 + 3*N + 2*N*lg N + 3*N**2 ),
*                         where lg( N ) = smallest integer k such
*                         that 2**k >= N.
*          If COMPZ = 'I' and N > 1 then LWORK must be at least
*                         ( 1 + 4*N + N**2 ).
*
*          If LWORK = -1, then a workspace query is assumed; the routine
*          only calculates the optimal size of the WORK array, returns
*          this value as the first entry of the WORK array, and no error
*          message related to LWORK is issued by XERBLA.
*
*  IWORK   (workspace/output) INTEGER array, dimension (LIWORK)
*          On exit, if INFO = 0, IWORK(1) returns the optimal LIWORK.
*
*  LIWORK  (input) INTEGER
*          The dimension of the array IWORK.
*          If COMPZ = 'N' or N <= 1 then LIWORK must be at least 1.
*          If COMPZ = 'V' and N > 1 then LIWORK must be at least
*                         ( 6 + 6*N + 5*N*lg N ).
*          If COMPZ = 'I' and N > 1 then LIWORK must be at least
*                         ( 3 + 5*N ).
*
*          If LIWORK = -1, then a workspace query is assumed; the
*          routine only calculates the optimal size of the IWORK array,
*          returns this value as the first entry of the IWORK array, and
*          no error message related to LIWORK is issued by XERBLA.
*
*  INFO    (output) INTEGER
*          = 0:  successful exit.
*          < 0:  if INFO = -i, the i-th argument had an illegal value.
*          > 0:  The algorithm failed to compute an eigenvalue while
*                working on the submatrix lying in rows and columns
*                INFO/(N+1) through mod(INFO,N+1).
*
*  Further Details
*  ===============
*
*  Based on contributions by
*     Jeff Rutter, Computer Science Division, University of California
*     at Berkeley, USA
*  Modified by Francoise Tisseur, University of Tennessee.
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE, TWO
      PARAMETER          ( ZERO = 0.0D0, ONE = 1.0D0, TWO = 2.0D0 )
*     ..
*     .. Local Scalars ..
      LOGICAL            LQUERY
      INTEGER            DTRTRW, END, I, ICOMPZ, II, J, K, LGN, LIWMIN,
     $                   LWMIN, M, SMLSIZ, START, STOREZ
      DOUBLE PRECISION   EPS, ORGNRM, P, TINY
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      INTEGER            ILAENV
      DOUBLE PRECISION   DLAMCH, DLANST
      EXTERNAL           LSAME, ILAENV, DLAMCH, DLANST
*     ..
*     .. External Subroutines ..
      EXTERNAL           DGEMM, DLACPY, DLAED0, DLASCL, DLASET, DLASRT,
     $                   DSTEQR, DSTERF, DSWAP, XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, DBLE, INT, LOG, MAX, MOD, SQRT
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters.
*
      INFO = 0
      LQUERY = ( LWORK.EQ.-1 .OR. LIWORK.EQ.-1 )
*
      IF( LSAME( COMPZ, 'N' ) ) THEN
         ICOMPZ = 0
      ELSE IF( LSAME( COMPZ, 'V' ) ) THEN
         ICOMPZ = 1
      ELSE IF( LSAME( COMPZ, 'I' ) ) THEN
         ICOMPZ = 2
      ELSE
         ICOMPZ = -1
      END IF
      IF( N.LE.1 .OR. ICOMPZ.LE.0 ) THEN
         LIWMIN = 1
         LWMIN = 1
      ELSE
         LGN = INT( LOG( DBLE( N ) ) / LOG( TWO ) )
         IF( 2**LGN.LT.N )
     $      LGN = LGN + 1
         IF( 2**LGN.LT.N )
     $      LGN = LGN + 1
         IF( ICOMPZ.EQ.1 ) THEN
            LWMIN = 1 + 3*N + 2*N*LGN + 3*N**2
            LIWMIN = 6 + 6*N + 5*N*LGN
         ELSE IF( ICOMPZ.EQ.2 ) THEN
            LWMIN = 1 + 4*N + N**2
            LIWMIN = 3 + 5*N
         END IF
      END IF
      IF( ICOMPZ.LT.0 ) THEN
         INFO = -1
      ELSE IF( N.LT.0 ) THEN
         INFO = -2
      ELSE IF( ( LDZ.LT.1 ) .OR. ( ICOMPZ.GT.0 .AND. LDZ.LT.MAX( 1,
     $         N ) ) ) THEN
         INFO = -6
      ELSE IF( LWORK.LT.LWMIN .AND. .NOT.LQUERY ) THEN
         INFO = -8
      ELSE IF( LIWORK.LT.LIWMIN .AND. .NOT.LQUERY ) THEN
         INFO = -10
      END IF
*
      IF( INFO.EQ.0 ) THEN
         WORK( 1 ) = LWMIN
         IWORK( 1 ) = LIWMIN
      END IF
*
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DSTEDC', -INFO )
         RETURN
      ELSE IF( LQUERY ) THEN
         RETURN
      END IF
*
*     Quick return if possible
*
      ITCNT = 0
      IF( N.EQ.0 )
     $   RETURN
      IF( N.EQ.1 ) THEN
         IF( ICOMPZ.NE.0 )
     $      Z( 1, 1 ) = ONE
         RETURN
      END IF
*
      SMLSIZ = ILAENV( 9, 'DSTEDC', ' ', 0, 0, 0, 0 )
*
*     If the following conditional clause is removed, then the routine
*     will use the Divide and Conquer routine to compute only the
*     eigenvalues, which requires (3N + 3N**2) real workspace and
*     (2 + 5N + 2N lg(N)) integer workspace.
*     Since on many architectures DSTERF is much faster than any other
*     algorithm for finding eigenvalues only, it is used here
*     as the default.
*
*     If COMPZ = 'N', use DSTERF to compute the eigenvalues.
*
      IF( ICOMPZ.EQ.0 ) THEN
         CALL DSTERF( N, D, E, INFO )
         RETURN
      END IF
*
*     If N is smaller than the minimum divide size (SMLSIZ+1), then
*     solve the problem with another solver.
*
      IF( N.LE.SMLSIZ ) THEN
         IF( ICOMPZ.EQ.0 ) THEN
            CALL DSTERF( N, D, E, INFO )
            RETURN
         ELSE IF( ICOMPZ.EQ.2 ) THEN
            CALL DSTEQR( 'I', N, D, E, Z, LDZ, WORK, INFO )
            RETURN
         ELSE
            CALL DSTEQR( 'V', N, D, E, Z, LDZ, WORK, INFO )
            RETURN
         END IF
      END IF
*
*     If COMPZ = 'V', the Z matrix must be stored elsewhere for later
*     use.
*
      IF( ICOMPZ.EQ.1 ) THEN
         STOREZ = 1 + N*N
      ELSE
         STOREZ = 1
      END IF
*
      IF( ICOMPZ.EQ.2 ) THEN
         CALL DLASET( 'Full', N, N, ZERO, ONE, Z, LDZ )
      END IF
*
*     Scale.
*
      ORGNRM = DLANST( 'M', N, D, E )
      IF( ORGNRM.EQ.ZERO )
     $   RETURN
*
      EPS = DLAMCH( 'Epsilon' )
*
      START = 1
*
*     while ( START <= N )
*
   10 CONTINUE
      IF( START.LE.N ) THEN
*
*     Let END be the position of the next subdiagonal entry such that
*     E( END ) <= TINY or END = N if no such subdiagonal exists.  The
*     matrix identified by the elements between START and END
*     constitutes an independent sub-problem.
*
         END = START
   20    CONTINUE
         IF( END.LT.N ) THEN
            OPS = OPS + 4
            TINY = EPS*SQRT( ABS( D( END ) ) )*SQRT( ABS( D( END+1 ) ) )
            IF( ABS( E( END ) ).GT.TINY ) THEN
               END = END + 1
               GO TO 20
            END IF
         END IF
*
*        (Sub) Problem determined.  Compute its size and solve it.
*
         M = END - START + 1
         IF( M.EQ.1 ) THEN
            START = END + 1
            GO TO 10
         END IF
         IF( M.GT.SMLSIZ ) THEN
            INFO = SMLSIZ
*
*           Scale.
*
            ORGNRM = DLANST( 'M', M, D( START ), E( START ) )
            OPS = OPS + 2*M - 1
            CALL DLASCL( 'G', 0, 0, ORGNRM, ONE, M, 1, D( START ), M,
     $                   INFO )
            CALL DLASCL( 'G', 0, 0, ORGNRM, ONE, M-1, 1, E( START ),
     $                   M-1, INFO )
*
            IF( ICOMPZ.EQ.1 ) THEN
               DTRTRW = 1
            ELSE
               DTRTRW = START
            END IF
            CALL DLAED0( ICOMPZ, N, M, D( START ), E( START ),
     $                   Z( DTRTRW, START ), LDZ, WORK( 1 ), N,
     $                   WORK( STOREZ ), IWORK, INFO )
            IF( INFO.NE.0 ) THEN
               INFO = ( INFO / ( M+1 )+START-1 )*( N+1 ) +
     $                MOD( INFO, ( M+1 ) ) + START - 1
               RETURN
            END IF
*
*           Scale back.
*
            OPS = OPS + M
            CALL DLASCL( 'G', 0, 0, ONE, ORGNRM, M, 1, D( START ), M,
     $                   INFO )
*
         ELSE
            IF( ICOMPZ.EQ.1 ) THEN
*
*     Since QR won't update a Z matrix which is larger than the
*     length of D, we must solve the sub-problem in a workspace and
*     then multiply back into Z.
*
               CALL DSTEQR( 'I', M, D( START ), E( START ), WORK, M,
     $                      WORK( M*M+1 ), INFO )
               CALL DLACPY( 'A', N, M, Z( 1, START ), LDZ,
     $                      WORK( STOREZ ), N )
               OPS = OPS + 2*DBLE( N )*M*M
               CALL DGEMM( 'N', 'N', N, M, M, ONE, WORK( STOREZ ), LDZ,
     $                     WORK, M, ZERO, Z( 1, START ), LDZ )
            ELSE IF( ICOMPZ.EQ.2 ) THEN
               CALL DSTEQR( 'I', M, D( START ), E( START ),
     $                      Z( START, START ), LDZ, WORK, INFO )
            ELSE
               CALL DSTERF( M, D( START ), E( START ), INFO )
            END IF
            IF( INFO.NE.0 ) THEN
               INFO = START*( N+1 ) + END
               RETURN
            END IF
         END IF
*
         START = END + 1
         GO TO 10
      END IF
*
*     endwhile
*
*     If the problem split any number of times, then the eigenvalues
*     will not be properly ordered.  Here we permute the eigenvalues
*     (and the associated eigenvectors) into ascending order.
*
      IF( M.NE.N ) THEN
         IF( ICOMPZ.EQ.0 ) THEN
*
*        Use Quick Sort
*
            CALL DLASRT( 'I', N, D, INFO )
*
         ELSE
*
*        Use Selection Sort to minimize swaps of eigenvectors
*
            DO 40 II = 2, N
               I = II - 1
               K = I
               P = D( I )
               DO 30 J = II, N
                  IF( D( J ).LT.P ) THEN
                     K = J
                     P = D( J )
                  END IF
   30          CONTINUE
               IF( K.NE.I ) THEN
                  D( K ) = D( I )
                  D( I ) = P
                  CALL DSWAP( N, Z( 1, I ), 1, Z( 1, K ), 1 )
               END IF
   40       CONTINUE
         END IF
      END IF
*
      WORK( 1 ) = LWMIN
      IWORK( 1 ) = LIWMIN
*
      RETURN
*
*     End of DSTEDC
*
      END
      SUBROUTINE DSTEGR( JOBZ, RANGE, N, D, E, VL, VU, IL, IU, ABSTOL,
     $                   M, W, Z, LDZ, ISUPPZ, WORK, LWORK, IWORK,
     $                   LIWORK, INFO )
*
*  -- LAPACK computational routine (instru to count ops, version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     June 30, 1999
*
*     .. Scalar Arguments ..
      CHARACTER          JOBZ, RANGE
      INTEGER            IL, INFO, IU, LDZ, LIWORK, LWORK, M, N
      DOUBLE PRECISION   ABSTOL, VL, VU
*     ..
*     .. Array Arguments ..
      INTEGER            ISUPPZ( * ), IWORK( * )
      DOUBLE PRECISION   D( * ), E( * ), W( * ), WORK( * ), Z( LDZ, * )
*     ..
*     Common block to return operation count
*     .. Common blocks ..
      COMMON             / LATIME / OPS, ITCNT
*     ..
*     .. Scalars in Common ..
      DOUBLE PRECISION   ITCNT, OPS
*     ..
*
*  Purpose
*  =======
*
*  DSTEGR computes eigenvalues by the dqds algorithm, while
*  orthogonal eigenvectors are computed from various "good" L D L^T
*  representations (also known as Relatively Robust Representations).
*  Gram-Schmidt orthogonalization is avoided as far as possible. More
*  specifically, the various steps of the algorithm are as follows.
*  For the i-th unreduced block of T,
*     (a) Compute T - sigma_i = L_i D_i L_i^T, such that L_i D_i L_i^T
*         is a relatively robust representation,
*     (b) Compute the eigenvalues, lambda_j, of L_i D_i L_i^T to high
*         relative accuracy by the dqds algorithm,
*     (c) If there is a cluster of close eigenvalues, "choose" sigma_i
*         close to the cluster, and go to step (a),
*     (d) Given the approximate eigenvalue lambda_j of L_i D_i L_i^T,
*         compute the corresponding eigenvector by forming a
*         rank-revealing twisted factorization.
*  The desired accuracy of the output can be specified by the input
*  parameter ABSTOL.
*
*  For more details, see "A new O(n^2) algorithm for the symmetric
*  tridiagonal eigenvalue/eigenvector problem", by Inderjit Dhillon,
*  Computer Science Division Technical Report No. UCB/CSD-97-971,
*  UC Berkeley, May 1997.
*
*  Note 1 : Currently DSTEGR is only set up to find ALL the n
*  eigenvalues and eigenvectors of T in O(n^2) time
*  Note 2 : Currently the routine DSTEIN is called when an appropriate
*  sigma_i cannot be chosen in step (c) above. DSTEIN invokes modified
*  Gram-Schmidt when eigenvalues are close.
*  Note 3 : DSTEGR works only on machines which follow ieee-754
*  floating-point standard in their handling of infinities and NaNs.
*  Normal execution of DSTEGR may create NaNs and infinities and hence
*  may abort due to a floating point exception in environments which
*  do not conform to the ieee standard.
*
*  Arguments
*  =========
*
*  JOBZ    (input) CHARACTER*1
*          = 'N':  Compute eigenvalues only;
*          = 'V':  Compute eigenvalues and eigenvectors.
*
*  RANGE   (input) CHARACTER*1
*          = 'A': all eigenvalues will be found.
*          = 'V': all eigenvalues in the half-open interval (VL,VU]
*                 will be found.
*          = 'I': the IL-th through IU-th eigenvalues will be found.
********** Only RANGE = 'A' is currently supported *********************
*
*  N       (input) INTEGER
*          The order of the matrix.  N >= 0.
*
*  D       (input/output) DOUBLE PRECISION array, dimension (N)
*          On entry, the n diagonal elements of the tridiagonal matrix
*          T. On exit, D is overwritten.
*
*  E       (input/output) DOUBLE PRECISION array, dimension (N)
*          On entry, the (n-1) subdiagonal elements of the tridiagonal
*          matrix T in elements 1 to N-1 of E; E(N) need not be set.
*          On exit, E is overwritten.
*
*  VL      (input) DOUBLE PRECISION
*  VU      (input) DOUBLE PRECISION
*          If RANGE='V', the lower and upper bounds of the interval to
*          be searched for eigenvalues. VL < VU.
*          Not referenced if RANGE = 'A' or 'I'.
*
*  IL      (input) INTEGER
*  IU      (input) INTEGER
*          If RANGE='I', the indices (in ascending order) of the
*          smallest and largest eigenvalues to be returned.
*          1 <= IL <= IU <= N, if N > 0; IL = 1 and IU = 0 if N = 0.
*          Not referenced if RANGE = 'A' or 'V'.
*
*  ABSTOL  (input) DOUBLE PRECISION
*          The absolute error tolerance for the
*          eigenvalues/eigenvectors. IF JOBZ = 'V', the eigenvalues and
*          eigenvectors output have residual norms bounded by ABSTOL,
*          and the dot products between different eigenvectors are
*          bounded by ABSTOL. If ABSTOL is less than N*EPS*|T|, then
*          N*EPS*|T| will be used in its place, where EPS is the
*          machine precision and |T| is the 1-norm of the tridiagonal
*          matrix. The eigenvalues are computed to an accuracy of
*          EPS*|T| irrespective of ABSTOL. If high relative accuracy
*          is important, set ABSTOL to DLAMCH( 'Safe minimum' ).
*          See Barlow and Demmel "Computing Accurate Eigensystems of
*          Scaled Diagonally Dominant Matrices", LAPACK Working Note #7
*          for a discussion of which matrices define their eigenvalues
*          to high relative accuracy.
*
*  M       (output) INTEGER
*          The total number of eigenvalues found.  0 <= M <= N.
*          If RANGE = 'A', M = N, and if RANGE = 'I', M = IU-IL+1.
*
*  W       (output) DOUBLE PRECISION array, dimension (N)
*          The first M elements contain the selected eigenvalues in
*          ascending order.
*
*  Z       (output) DOUBLE PRECISION array, dimension (LDZ, max(1,M) )
*          If JOBZ = 'V', then if INFO = 0, the first M columns of Z
*          contain the orthonormal eigenvectors of the matrix T
*          corresponding to the selected eigenvalues, with the i-th
*          column of Z holding the eigenvector associated with W(i).
*          If JOBZ = 'N', then Z is not referenced.
*          Note: the user must ensure that at least max(1,M) columns are
*          supplied in the array Z; if RANGE = 'V', the exact value of M
*          is not known in advance and an upper bound must be used.
*
*  LDZ     (input) INTEGER
*          The leading dimension of the array Z.  LDZ >= 1, and if
*          JOBZ = 'V', LDZ >= max(1,N).
*
*  ISUPPZ  (output) INTEGER ARRAY, dimension ( 2*max(1,M) )
*          The support of the eigenvectors in Z, i.e., the indices
*          indicating the nonzero elements in Z. The i-th eigenvector
*          is nonzero only in elements ISUPPZ( 2*i-1 ) through
*          ISUPPZ( 2*i ).
*
*  WORK    (workspace/output) DOUBLE PRECISION array, dimension (LWORK)
*          On exit, if INFO = 0, WORK(1) returns the optimal
*          (and minimal) LWORK.
*
*  LWORK   (input) INTEGER
*          The dimension of the array WORK.  LWORK >= max(1,18*N)
*
*          If LWORK = -1, then a workspace query is assumed; the routine
*          only calculates the optimal size of the WORK array, returns
*          this value as the first entry of the WORK array, and no error
*          message related to LWORK is issued by XERBLA.
*
*  IWORK   (workspace/output) INTEGER array, dimension (LIWORK)
*          On exit, if INFO = 0, IWORK(1) returns the optimal LIWORK.
*
*  LIWORK  (input) INTEGER
*          The dimension of the array IWORK.  LIWORK >= max(1,10*N)
*
*          If LIWORK = -1, then a workspace query is assumed; the
*          routine only calculates the optimal size of the IWORK array,
*          returns this value as the first entry of the IWORK array, and
*          no error message related to LIWORK is issued by XERBLA.
*
*  INFO    (output) INTEGER
*          = 0:  successful exit
*          < 0:  if INFO = -i, the i-th argument had an illegal value
*          > 0:  if INFO = 1, internal error in DLARRE,
*                if INFO = 2, internal error in DLARRV.
*
*  Further Details
*  ===============
*
*  Based on contributions by
*     Inderjit Dhillon, IBM Almaden, USA
*     Osni Marques, LBNL/NERSC, USA
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE
      PARAMETER          ( ZERO = 0.0D0, ONE = 1.0D0 )
*     ..
*     .. Local Scalars ..
      LOGICAL            ALLEIG, INDEIG, LQUERY, VALEIG, WANTZ
      INTEGER            I, IBEGIN, IEND, IINDBL, IINDWK, IINFO, IINSPL,
     $                   INDGRS, INDWOF, INDWRK, ITMP, J, JJ, LIWMIN,
     $                   LWMIN, NSPLIT
      DOUBLE PRECISION   BIGNUM, EPS, RMAX, RMIN, SAFMIN, SCALE, SMLNUM,
     $                   THRESH, TMP, TNRM, TOL
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      DOUBLE PRECISION   DLAMCH, DLANST
      EXTERNAL           LSAME, DLAMCH, DLANST
*     ..
*     .. External Subroutines ..
      EXTERNAL           DLARRE, DLARRV, DLASET, DSCAL, DSWAP, XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          DBLE, MAX, MIN, SQRT
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters.
*
      WANTZ = LSAME( JOBZ, 'V' )
      ALLEIG = LSAME( RANGE, 'A' )
      VALEIG = LSAME( RANGE, 'V' )
      INDEIG = LSAME( RANGE, 'I' )
*
      LQUERY = ( ( LWORK.EQ.-1 ) .OR. ( LIWORK.EQ.-1 ) )
      LWMIN = 18*N
      LIWMIN = 10*N
*
      INFO = 0
      IF( .NOT.( WANTZ .OR. LSAME( JOBZ, 'N' ) ) ) THEN
         INFO = -1
      ELSE IF( .NOT.( ALLEIG .OR. VALEIG .OR. INDEIG ) ) THEN
         INFO = -2
*
*     The following two lines need to be removed once the
*     RANGE = 'V' and RANGE = 'I' options are provided.
*
      ELSE IF( VALEIG .OR. INDEIG ) THEN
         INFO = -2
      ELSE IF( N.LT.0 ) THEN
         INFO = -3
      ELSE IF( VALEIG .AND. N.GT.0 .AND. VU.LE.VL ) THEN
         INFO = -7
      ELSE IF( INDEIG .AND. IL.LT.1 ) THEN
         INFO = -8
*     The following change should be made in DSTEVX also, otherwise
*     IL can be specified as N+1 and IU as N.
*     ELSE IF( INDEIG .AND. ( IU.LT.MIN( N, IL ) .OR. IU.GT.N ) ) THEN
      ELSE IF( INDEIG .AND. ( IU.LT.IL .OR. IU.GT.N ) ) THEN
         INFO = -9
      ELSE IF( LDZ.LT.1 .OR. ( WANTZ .AND. LDZ.LT.N ) ) THEN
         INFO = -14
      ELSE IF( LWORK.LT.LWMIN .AND. .NOT.LQUERY ) THEN
         INFO = -17
      ELSE IF( LIWORK.LT.LIWMIN .AND. .NOT.LQUERY ) THEN
         INFO = -19
      END IF
      IF( INFO.EQ.0 ) THEN
         WORK( 1 ) = LWMIN
         IWORK( 1 ) = LIWMIN
      END IF
*
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DSTEGR', -INFO )
         RETURN
      ELSE IF( LQUERY ) THEN
         RETURN
      END IF
*
*     Quick return if possible
*
      M = 0
      IF( N.EQ.0 )
     $   RETURN
*
      IF( N.EQ.1 ) THEN
         IF( ALLEIG .OR. INDEIG ) THEN
            M = 1
            W( 1 ) = D( 1 )
         ELSE
            IF( VL.LT.D( 1 ) .AND. VU.GE.D( 1 ) ) THEN
               M = 1
               W( 1 ) = D( 1 )
            END IF
         END IF
         IF( WANTZ )
     $      Z( 1, 1 ) = ONE
         RETURN
      END IF
*
*     Get machine constants.
*
      OPS = OPS + DBLE( 7 )
      SAFMIN = DLAMCH( 'Safe minimum' )
      EPS = DLAMCH( 'Precision' )
      SMLNUM = SAFMIN / EPS
      BIGNUM = ONE / SMLNUM
      RMIN = SQRT( SMLNUM )
      RMAX = MIN( SQRT( BIGNUM ), ONE / SQRT( SQRT( SAFMIN ) ) )
*
*     Scale matrix to allowable range, if necessary.
*
      SCALE = ONE
      TNRM = DLANST( 'M', N, D, E )
      IF( TNRM.GT.ZERO .AND. TNRM.LT.RMIN ) THEN
         OPS = OPS + DBLE( 1 )
         SCALE = RMIN / TNRM
      ELSE IF( TNRM.GT.RMAX ) THEN
         OPS = OPS + DBLE( 1 )
         SCALE = RMAX / TNRM
      END IF
      IF( SCALE.NE.ONE ) THEN
         OPS = OPS + DBLE( 2*N )
         CALL DSCAL( N, SCALE, D, 1 )
         CALL DSCAL( N-1, SCALE, E, 1 )
         TNRM = TNRM*SCALE
      END IF
      INDGRS = 1
      INDWOF = 2*N + 1
      INDWRK = 3*N + 1
*
      IINSPL = 1
      IINDBL = N + 1
      IINDWK = 2*N + 1
*
      CALL DLASET( 'Full', N, N, ZERO, ZERO, Z, LDZ )
*
*     Compute the desired eigenvalues of the tridiagonal after splitting
*     into smaller subblocks if the corresponding of-diagonal elements
*     are small
*
      OPS = OPS + DBLE( 1 )
      THRESH = EPS*TNRM
      CALL DLARRE( N, D, E, THRESH, NSPLIT, IWORK( IINSPL ), M, W,
     $             WORK( INDWOF ), WORK( INDGRS ), WORK( INDWRK ),
     $             IINFO )
      IF( IINFO.NE.0 ) THEN
         INFO = 1
         RETURN
      END IF
*
      IF( WANTZ ) THEN
*
*        Compute the desired eigenvectors corresponding to the computed
*        eigenvalues
*
         OPS = OPS + DBLE( 1 )
         TOL = MAX( ABSTOL, DBLE( N )*THRESH )
         IBEGIN = 1
         DO 20 I = 1, NSPLIT
            IEND = IWORK( IINSPL+I-1 )
            DO 10 J = IBEGIN, IEND
               IWORK( IINDBL+J-1 ) = I
   10       CONTINUE
            IBEGIN = IEND + 1
   20    CONTINUE
*
         CALL DLARRV( N, D, E, IWORK( IINSPL ), M, W, IWORK( IINDBL ),
     $                WORK( INDGRS ), TOL, Z, LDZ, ISUPPZ,
     $                WORK( INDWRK ), IWORK( IINDWK ), IINFO )
         IF( IINFO.NE.0 ) THEN
            INFO = 2
            RETURN
         END IF
*
      END IF
*
      IBEGIN = 1
      DO 40 I = 1, NSPLIT
         IEND = IWORK( IINSPL+I-1 )
         DO 30 J = IBEGIN, IEND
            OPS = OPS + DBLE( 1 )
            W( J ) = W( J ) + WORK( INDWOF+I-1 )
   30    CONTINUE
         IBEGIN = IEND + 1
   40 CONTINUE
*
*     If matrix was scaled, then rescale eigenvalues appropriately.
*
      IF( SCALE.NE.ONE ) THEN
         CALL DSCAL( M, ONE / SCALE, W, 1 )
      END IF
*
*     If eigenvalues are not in order, then sort them, along with
*     eigenvectors.
*
      IF( NSPLIT.GT.1 ) THEN
         DO 60 J = 1, M - 1
            I = 0
            TMP = W( J )
            DO 50 JJ = J + 1, M
               IF( W( JJ ).LT.TMP ) THEN
                  I = JJ
                  TMP = W( JJ )
               END IF
   50       CONTINUE
            IF( I.NE.0 ) THEN
               W( I ) = W( J )
               W( J ) = TMP
               IF( WANTZ ) THEN
                  CALL DSWAP( N, Z( 1, I ), 1, Z( 1, J ), 1 )
                  ITMP = ISUPPZ( 2*I-1 )
                  ISUPPZ( 2*I-1 ) = ISUPPZ( 2*J-1 )
                  ISUPPZ( 2*J-1 ) = ITMP
                  ITMP = ISUPPZ( 2*I )
                  ISUPPZ( 2*I ) = ISUPPZ( 2*J )
                  ISUPPZ( 2*J ) = ITMP
               END IF
            END IF
   60    CONTINUE
      END IF
*
      WORK( 1 ) = LWMIN
      IWORK( 1 ) = LIWMIN
      RETURN
*
*     End of DSTEGR
*
      END
      SUBROUTINE DSTEIN( N, D, E, M, W, IBLOCK, ISPLIT, Z, LDZ, WORK,
     $                   IWORK, IFAIL, INFO )
*
*  -- LAPACK routine (instrumented to count operations, version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     September 30, 1994
*
*     .. Scalar Arguments ..
      INTEGER            INFO, LDZ, M, N
*     ..
*     .. Array Arguments ..
      INTEGER            IBLOCK( * ), IFAIL( * ), ISPLIT( * ),
     $                   IWORK( * )
      DOUBLE PRECISION   D( * ), E( * ), W( * ), WORK( * ), Z( LDZ, * )
*     ..
*     Common block to return operation count and iteration count
*     ITCNT is initialized to 0, OPS is only incremented
*     .. Common blocks ..
      COMMON             / LATIME / OPS, ITCNT
*     ..
*     .. Scalars in Common ..
      DOUBLE PRECISION   ITCNT, OPS
*     ..
*
*  Purpose
*  =======
*
*  DSTEIN computes the eigenvectors of a real symmetric tridiagonal
*  matrix T corresponding to specified eigenvalues, using inverse
*  iteration.
*
*  The maximum number of iterations allowed for each eigenvector is
*  specified by an internal parameter MAXITS (currently set to 5).
*
*  Arguments
*  =========
*
*  N       (input) INTEGER
*          The order of the matrix.  N >= 0.
*
*  D       (input) DOUBLE PRECISION array, dimension (N)
*          The n diagonal elements of the tridiagonal matrix T.
*
*  E       (input) DOUBLE PRECISION array, dimension (N)
*          The (n-1) subdiagonal elements of the tridiagonal matrix
*          T, in elements 1 to N-1.  E(N) need not be set.
*
*  M       (input) INTEGER
*          The number of eigenvectors to be found.  0 <= M <= N.
*
*  W       (input) DOUBLE PRECISION array, dimension (N)
*          The first M elements of W contain the eigenvalues for
*          which eigenvectors are to be computed.  The eigenvalues
*          should be grouped by split-off block and ordered from
*          smallest to largest within the block.  ( The output array
*          W from DSTEBZ with ORDER = 'B' is expected here. )
*
*  IBLOCK  (input) INTEGER array, dimension (N)
*          The submatrix indices associated with the corresponding
*          eigenvalues in W; IBLOCK(i)=1 if eigenvalue W(i) belongs to
*          the first submatrix from the top, =2 if W(i) belongs to
*          the second submatrix, etc.  ( The output array IBLOCK
*          from DSTEBZ is expected here. )
*
*  ISPLIT  (input) INTEGER array, dimension (N)
*          The splitting points, at which T breaks up into submatrices.
*          The first submatrix consists of rows/columns 1 to
*          ISPLIT( 1 ), the second of rows/columns ISPLIT( 1 )+1
*          through ISPLIT( 2 ), etc.
*          ( The output array ISPLIT from DSTEBZ is expected here. )
*
*  Z       (output) DOUBLE PRECISION array, dimension (LDZ, M)
*          The computed eigenvectors.  The eigenvector associated
*          with the eigenvalue W(i) is stored in the i-th column of
*          Z.  Any vector which fails to converge is set to its current
*          iterate after MAXITS iterations.
*
*  LDZ     (input) INTEGER
*          The leading dimension of the array Z.  LDZ >= max(1,N).
*
*  WORK    (workspace) DOUBLE PRECISION array, dimension (5*N)
*
*  IWORK   (workspace) INTEGER array, dimension (N)
*
*  IFAIL   (output) INTEGER array, dimension (M)
*          On normal exit, all elements of IFAIL are zero.
*          If one or more eigenvectors fail to converge after
*          MAXITS iterations, then their indices are stored in
*          array IFAIL.
*
*  INFO    (output) INTEGER
*          = 0: successful exit.
*          < 0: if INFO = -i, the i-th argument had an illegal value
*          > 0: if INFO = i, then i eigenvectors failed to converge
*               in MAXITS iterations.  Their indices are stored in
*               array IFAIL.
*
*  Internal Parameters
*  ===================
*
*  MAXITS  INTEGER, default = 5
*          The maximum number of iterations performed.
*
*  EXTRA   INTEGER, default = 2
*          The number of iterations performed after norm growth
*          criterion is satisfied, should be at least 1.
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE, TEN, ODM3, ODM1
      PARAMETER          ( ZERO = 0.0D+0, ONE = 1.0D+0, TEN = 1.0D+1,
     $                   ODM3 = 1.0D-3, ODM1 = 1.0D-1 )
      INTEGER            MAXITS, EXTRA
      PARAMETER          ( MAXITS = 5, EXTRA = 2 )
*     ..
*     .. Local Scalars ..
      INTEGER            B1, BLKSIZ, BN, GPIND, I, IINFO, INDRV1,
     $                   INDRV2, INDRV3, INDRV4, INDRV5, ITS, J, J1,
     $                   JBLK, JMAX, NBLK, NRMCHK
      DOUBLE PRECISION   DTPCRT, EPS, EPS1, NRM, ONENRM, ORTOL, PERTOL,
     $                   SCL, SEP, TOL, XJ, XJM, ZTR
*     ..
*     .. Local Arrays ..
      INTEGER            ISEED( 4 )
*     ..
*     .. External Functions ..
      INTEGER            IDAMAX
      DOUBLE PRECISION   DASUM, DDOT, DLAMCH, DNRM2
      EXTERNAL           IDAMAX, DASUM, DDOT, DLAMCH, DNRM2
*     ..
*     .. External Subroutines ..
      EXTERNAL           DAXPY, DCOPY, DLAGTF, DLAGTS, DLARNV, DSCAL,
     $                   XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, MAX, SQRT
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters.
*
      INFO = 0
      DO 10 I = 1, M
         IFAIL( I ) = 0
   10 CONTINUE
*
      IF( N.LT.0 ) THEN
         INFO = -1
      ELSE IF( M.LT.0 .OR. M.GT.N ) THEN
         INFO = -4
      ELSE IF( LDZ.LT.MAX( 1, N ) ) THEN
         INFO = -9
      ELSE
         DO 20 J = 2, M
            IF( IBLOCK( J ).LT.IBLOCK( J-1 ) ) THEN
               INFO = -6
               GO TO 30
            END IF
            IF( IBLOCK( J ).EQ.IBLOCK( J-1 ) .AND. W( J ).LT.W( J-1 ) )
     $           THEN
               INFO = -5
               GO TO 30
            END IF
   20    CONTINUE
   30    CONTINUE
      END IF
*
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DSTEIN', -INFO )
         RETURN
      END IF
*
*     Initialize iteration count.
*
      ITCNT = 0
*
*     Quick return if possible
*
      IF( N.EQ.0 .OR. M.EQ.0 ) THEN
         RETURN
      ELSE IF( N.EQ.1 ) THEN
         Z( 1, 1 ) = ONE
         RETURN
      END IF
*
*     Get machine constants.
*
      EPS = DLAMCH( 'Precision' )
*
*     Initialize seed for random number generator DLARNV.
*
      DO 40 I = 1, 4
         ISEED( I ) = 1
   40 CONTINUE
*
*     Initialize pointers.
*
      INDRV1 = 0
      INDRV2 = INDRV1 + N
      INDRV3 = INDRV2 + N
      INDRV4 = INDRV3 + N
      INDRV5 = INDRV4 + N
*
*     Compute eigenvectors of matrix blocks.
*
      J1 = 1
      DO 160 NBLK = 1, IBLOCK( M )
*
*        Find starting and ending indices of block nblk.
*
         IF( NBLK.EQ.1 ) THEN
            B1 = 1
         ELSE
            B1 = ISPLIT( NBLK-1 ) + 1
         END IF
         BN = ISPLIT( NBLK )
         BLKSIZ = BN - B1 + 1
         IF( BLKSIZ.EQ.1 )
     $      GO TO 60
         GPIND = B1
*
*        Compute reorthogonalization criterion and stopping criterion.
*
         ONENRM = ABS( D( B1 ) ) + ABS( E( B1 ) )
         ONENRM = MAX( ONENRM, ABS( D( BN ) )+ABS( E( BN-1 ) ) )
         DO 50 I = B1 + 1, BN - 1
            ONENRM = MAX( ONENRM, ABS( D( I ) )+ABS( E( I-1 ) )+
     $               ABS( E( I ) ) )
   50    CONTINUE
         ORTOL = ODM3*ONENRM
*
         DTPCRT = SQRT( ODM1 / BLKSIZ )
*
*        Increment opcount for computing criteria.
*
         OPS = OPS + ( BN-B1 )*2 + 3
*
*        Loop through eigenvalues of block nblk.
*
   60    CONTINUE
         JBLK = 0
         DO 150 J = J1, M
            IF( IBLOCK( J ).NE.NBLK ) THEN
               J1 = J
               GO TO 160
            END IF
            JBLK = JBLK + 1
            XJ = W( J )
*
*           Skip all the work if the block size is one.
*
            IF( BLKSIZ.EQ.1 ) THEN
               WORK( INDRV1+1 ) = ONE
               GO TO 120
            END IF
*
*           If eigenvalues j and j-1 are too close, add a relatively
*           small perturbation.
*
            IF( JBLK.GT.1 ) THEN
               EPS1 = ABS( EPS*XJ )
               PERTOL = TEN*EPS1
               SEP = XJ - XJM
               IF( SEP.LT.PERTOL )
     $            XJ = XJM + PERTOL
            END IF
*
            ITS = 0
            NRMCHK = 0
*
*           Get random starting vector.
*
            CALL DLARNV( 2, ISEED, BLKSIZ, WORK( INDRV1+1 ) )
*
*           Increment opcount for getting random starting vector.
*           ( DLARND(2,.) requires 9 flops. )
*
            OPS = OPS + BLKSIZ*9
*
*           Copy the matrix T so it won't be destroyed in factorization.
*
            CALL DCOPY( BLKSIZ, D( B1 ), 1, WORK( INDRV4+1 ), 1 )
            CALL DCOPY( BLKSIZ-1, E( B1 ), 1, WORK( INDRV2+2 ), 1 )
            CALL DCOPY( BLKSIZ-1, E( B1 ), 1, WORK( INDRV3+1 ), 1 )
*
*           Compute LU factors with partial pivoting  ( PT = LU )
*
            TOL = ZERO
            CALL DLAGTF( BLKSIZ, WORK( INDRV4+1 ), XJ, WORK( INDRV2+2 ),
     $                   WORK( INDRV3+1 ), TOL, WORK( INDRV5+1 ), IWORK,
     $                   IINFO )
*
*           Increment opcount for computing LU factors.
*           ( DLAGTF(BLKSIZ,...) requires about 8*BLKSIZ flops. )
*
            OPS = OPS + 8*BLKSIZ
*
*           Update iteration count.
*
   70       CONTINUE
            ITS = ITS + 1
            IF( ITS.GT.MAXITS )
     $         GO TO 100
*
*           Normalize and scale the righthand side vector Pb.
*
            SCL = BLKSIZ*ONENRM*MAX( EPS,
     $            ABS( WORK( INDRV4+BLKSIZ ) ) ) /
     $            DASUM( BLKSIZ, WORK( INDRV1+1 ), 1 )
            CALL DSCAL( BLKSIZ, SCL, WORK( INDRV1+1 ), 1 )
*
*           Solve the system LU = Pb.
*
            CALL DLAGTS( -1, BLKSIZ, WORK( INDRV4+1 ), WORK( INDRV2+2 ),
     $                   WORK( INDRV3+1 ), WORK( INDRV5+1 ), IWORK,
     $                   WORK( INDRV1+1 ), TOL, IINFO )
*
*           Increment opcount for scaling and solving linear system.
*           ( DLAGTS(-1,BLKSIZ,...) requires about 8*BLKSIZ flops. )
*
            OPS = OPS + 3 + 10*BLKSIZ
*
*           Reorthogonalize by modified Gram-Schmidt if eigenvalues are
*           close enough.
*
            IF( JBLK.EQ.1 )
     $         GO TO 90
            IF( ABS( XJ-XJM ).GT.ORTOL )
     $         GPIND = J
            IF( GPIND.NE.J ) THEN
               DO 80 I = GPIND, J - 1
                  ZTR = -DDOT( BLKSIZ, WORK( INDRV1+1 ), 1, Z( B1, I ),
     $                  1 )
                  CALL DAXPY( BLKSIZ, ZTR, Z( B1, I ), 1,
     $                        WORK( INDRV1+1 ), 1 )
   80          CONTINUE
*
*              Increment opcount for reorthogonalizing.
*
               OPS = OPS + ( J-GPIND )*BLKSIZ*4
*
            END IF
*
*           Check the infinity norm of the iterate.
*
   90       CONTINUE
            JMAX = IDAMAX( BLKSIZ, WORK( INDRV1+1 ), 1 )
            NRM = ABS( WORK( INDRV1+JMAX ) )
*
*           Continue for additional iterations after norm reaches
*           stopping criterion.
*
            IF( NRM.LT.DTPCRT )
     $         GO TO 70
            NRMCHK = NRMCHK + 1
            IF( NRMCHK.LT.EXTRA+1 )
     $         GO TO 70
*
            GO TO 110
*
*           If stopping criterion was not satisfied, update info and
*           store eigenvector number in array ifail.
*
  100       CONTINUE
            INFO = INFO + 1
            IFAIL( INFO ) = J
*
*           Accept iterate as jth eigenvector.
*
  110       CONTINUE
            SCL = ONE / DNRM2( BLKSIZ, WORK( INDRV1+1 ), 1 )
            JMAX = IDAMAX( BLKSIZ, WORK( INDRV1+1 ), 1 )
            IF( WORK( INDRV1+JMAX ).LT.ZERO )
     $         SCL = -SCL
            CALL DSCAL( BLKSIZ, SCL, WORK( INDRV1+1 ), 1 )
*
*           Increment opcount for scaling.
*
            OPS = OPS + 3*BLKSIZ
*
  120       CONTINUE
            DO 130 I = 1, N
               Z( I, J ) = ZERO
  130       CONTINUE
            DO 140 I = 1, BLKSIZ
               Z( B1+I-1, J ) = WORK( INDRV1+I )
  140       CONTINUE
*
*           Save the shift to check eigenvalue spacing at next
*           iteration.
*
            XJM = XJ
*
  150    CONTINUE
  160 CONTINUE
*
      RETURN
*
*     End of DSTEIN
*
      END
      SUBROUTINE DSTEQR( COMPZ, N, D, E, Z, LDZ, WORK, INFO )
*
*  -- LAPACK routine (instrumented to count operations, version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     September 30, 1994
*
*     .. Scalar Arguments ..
      CHARACTER          COMPZ
      INTEGER            INFO, LDZ, N
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   D( * ), E( * ), WORK( * ), Z( LDZ, * )
*     ..
*     Common block to return operation count and iteration count
*     ITCNT is initialized to 0, OPS is only incremented
*     .. Common blocks ..
      COMMON             / LATIME / OPS, ITCNT
*     ..
*     .. Scalars in Common ..
      DOUBLE PRECISION   ITCNT, OPS
*     ..
*
*  Purpose
*  =======
*
*  DSTEQR computes all eigenvalues and, optionally, eigenvectors of a
*  symmetric tridiagonal matrix using the implicit QL or QR method.
*  The eigenvectors of a full or band symmetric matrix can also be found
*  if DSYTRD or DSPTRD or DSBTRD has been used to reduce this matrix to
*  tridiagonal form.
*
*  Arguments
*  =========
*
*  COMPZ   (input) CHARACTER*1
*          = 'N':  Compute eigenvalues only.
*          = 'V':  Compute eigenvalues and eigenvectors of the original
*                  symmetric matrix.  On entry, Z must contain the
*                  orthogonal matrix used to reduce the original matrix
*                  to tridiagonal form.
*          = 'I':  Compute eigenvalues and eigenvectors of the
*                  tridiagonal matrix.  Z is initialized to the identity
*                  matrix.
*
*  N       (input) INTEGER
*          The order of the matrix.  N >= 0.
*
*  D       (input/output) DOUBLE PRECISION array, dimension (N)
*          On entry, the diagonal elements of the tridiagonal matrix.
*          On exit, if INFO = 0, the eigenvalues in ascending order.
*
*  E       (input/output) DOUBLE PRECISION array, dimension (N-1)
*          On entry, the (n-1) subdiagonal elements of the tridiagonal
*          matrix.
*          On exit, E has been destroyed.
*
*  Z       (input/output) DOUBLE PRECISION array, dimension (LDZ, N)
*          On entry, if  COMPZ = 'V', then Z contains the orthogonal
*          matrix used in the reduction to tridiagonal form.
*          On exit, if INFO = 0, then if  COMPZ = 'V', Z contains the
*          orthonormal eigenvectors of the original symmetric matrix,
*          and if COMPZ = 'I', Z contains the orthonormal eigenvectors
*          of the symmetric tridiagonal matrix.
*          If COMPZ = 'N', then Z is not referenced.
*
*  LDZ     (input) INTEGER
*          The leading dimension of the array Z.  LDZ >= 1, and if
*          eigenvectors are desired, then  LDZ >= max(1,N).
*
*  WORK    (workspace) DOUBLE PRECISION array, dimension (max(1,2*N-2))
*          If COMPZ = 'N', then WORK is not referenced.
*
*  INFO    (output) INTEGER
*          = 0:  successful exit
*          < 0:  if INFO = -i, the i-th argument had an illegal value
*          > 0:  the algorithm has failed to find all the eigenvalues in
*                a total of 30*N iterations; if INFO = i, then i
*                elements of E have not converged to zero; on exit, D
*                and E contain the elements of a symmetric tridiagonal
*                matrix which is orthogonally similar to the original
*                matrix.
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE, TWO, THREE
      PARAMETER          ( ZERO = 0.0D0, ONE = 1.0D0, TWO = 2.0D0,
     $                   THREE = 3.0D0 )
      INTEGER            MAXIT
      PARAMETER          ( MAXIT = 30 )
*     ..
*     .. Local Scalars ..
      INTEGER            I, ICOMPZ, II, ISCALE, J, JTOT, K, L, L1, LEND,
     $                   LENDM1, LENDP1, LENDSV, LM1, LSV, M, MM, MM1,
     $                   NM1, NMAXIT
      DOUBLE PRECISION   ANORM, B, C, EPS, EPS2, F, G, P, R, RT1, RT2,
     $                   S, SAFMAX, SAFMIN, SSFMAX, SSFMIN, TST
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      DOUBLE PRECISION   DLAMCH, DLANST, DLAPY2
      EXTERNAL           LSAME, DLAMCH, DLANST, DLAPY2
*     ..
*     .. External Subroutines ..
      EXTERNAL           DLAE2, DLAEV2, DLARTG, DLASCL, DLASET, DLASR,
     $                   DLASRT, DSWAP, XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, MAX, SIGN, SQRT
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters.
*
      INFO = 0
*
      IF( LSAME( COMPZ, 'N' ) ) THEN
         ICOMPZ = 0
      ELSE IF( LSAME( COMPZ, 'V' ) ) THEN
         ICOMPZ = 1
      ELSE IF( LSAME( COMPZ, 'I' ) ) THEN
         ICOMPZ = 2
      ELSE
         ICOMPZ = -1
      END IF
      IF( ICOMPZ.LT.0 ) THEN
         INFO = -1
      ELSE IF( N.LT.0 ) THEN
         INFO = -2
      ELSE IF( ( LDZ.LT.1 ) .OR. ( ICOMPZ.GT.0 .AND. LDZ.LT.MAX( 1,
     $         N ) ) ) THEN
         INFO = -6
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DSTEQR', -INFO )
         RETURN
      END IF
*
*     Quick return if possible
*
      ITCNT = 0
      IF( N.EQ.0 )
     $   RETURN
*
      IF( N.EQ.1 ) THEN
         IF( ICOMPZ.EQ.2 )
     $      Z( 1, 1 ) = ONE
         RETURN
      END IF
*
*     Determine the unit roundoff and over/underflow thresholds.
*
      OPS = OPS + 6
      EPS = DLAMCH( 'E' )
      EPS2 = EPS**2
      SAFMIN = DLAMCH( 'S' )
      SAFMAX = ONE / SAFMIN
      SSFMAX = SQRT( SAFMAX ) / THREE
      SSFMIN = SQRT( SAFMIN ) / EPS2
*
*     Compute the eigenvalues and eigenvectors of the tridiagonal
*     matrix.
*
      IF( ICOMPZ.EQ.2 )
     $   CALL DLASET( 'Full', N, N, ZERO, ONE, Z, LDZ )
*
      NMAXIT = N*MAXIT
      JTOT = 0
*
*     Determine where the matrix splits and choose QL or QR iteration
*     for each block, according to whether top or bottom diagonal
*     element is smaller.
*
      L1 = 1
      NM1 = N - 1
*
   10 CONTINUE
      IF( L1.GT.N )
     $   GO TO 160
      IF( L1.GT.1 )
     $   E( L1-1 ) = ZERO
      IF( L1.LE.NM1 ) THEN
         DO 20 M = L1, NM1
            TST = ABS( E( M ) )
            IF( TST.EQ.ZERO )
     $         GO TO 30
            OPS = OPS + 4
            IF( TST.LE.( SQRT( ABS( D( M ) ) )*SQRT( ABS( D( M+
     $          1 ) ) ) )*EPS ) THEN
               E( M ) = ZERO
               GO TO 30
            END IF
   20    CONTINUE
      END IF
      M = N
*
   30 CONTINUE
      L = L1
      LSV = L
      LEND = M
      LENDSV = LEND
      L1 = M + 1
      IF( LEND.EQ.L )
     $   GO TO 10
*
*     Scale submatrix in rows and columns L to LEND
*
      OPS = OPS + 2*( LEND-L+1 )
      ANORM = DLANST( 'I', LEND-L+1, D( L ), E( L ) )
      ISCALE = 0
      IF( ANORM.EQ.ZERO )
     $   GO TO 10
      IF( ANORM.GT.SSFMAX ) THEN
         ISCALE = 1
         OPS = OPS + 2*( LEND-L ) + 1
         CALL DLASCL( 'G', 0, 0, ANORM, SSFMAX, LEND-L+1, 1, D( L ), N,
     $                INFO )
         CALL DLASCL( 'G', 0, 0, ANORM, SSFMAX, LEND-L, 1, E( L ), N,
     $                INFO )
      ELSE IF( ANORM.LT.SSFMIN ) THEN
         ISCALE = 2
         OPS = OPS + 2*( LEND-L ) + 1
         CALL DLASCL( 'G', 0, 0, ANORM, SSFMIN, LEND-L+1, 1, D( L ), N,
     $                INFO )
         CALL DLASCL( 'G', 0, 0, ANORM, SSFMIN, LEND-L, 1, E( L ), N,
     $                INFO )
      END IF
*
*     Choose between QL and QR iteration
*
      IF( ABS( D( LEND ) ).LT.ABS( D( L ) ) ) THEN
         LEND = LSV
         L = LENDSV
      END IF
*
      IF( LEND.GT.L ) THEN
*
*        QL Iteration
*
*        Look for small subdiagonal element.
*
   40    CONTINUE
         IF( L.NE.LEND ) THEN
            LENDM1 = LEND - 1
            DO 50 M = L, LENDM1
               TST = ABS( E( M ) )**2
               OPS = OPS + 4
               IF( TST.LE.( EPS2*ABS( D( M ) ) )*ABS( D( M+1 ) )+
     $             SAFMIN )GO TO 60
   50       CONTINUE
         END IF
*
         M = LEND
*
   60    CONTINUE
         IF( M.LT.LEND )
     $      E( M ) = ZERO
         P = D( L )
         IF( M.EQ.L )
     $      GO TO 80
*
*        If remaining matrix is 2-by-2, use DLAE2 or SLAEV2
*        to compute its eigensystem.
*
         IF( M.EQ.L+1 ) THEN
            IF( ICOMPZ.GT.0 ) THEN
               OPS = OPS + 22
               CALL DLAEV2( D( L ), E( L ), D( L+1 ), RT1, RT2, C, S )
               WORK( L ) = C
               WORK( N-1+L ) = S
               OPS = OPS + 6*N
               CALL DLASR( 'R', 'V', 'B', N, 2, WORK( L ),
     $                     WORK( N-1+L ), Z( 1, L ), LDZ )
            ELSE
               OPS = OPS + 15
               CALL DLAE2( D( L ), E( L ), D( L+1 ), RT1, RT2 )
            END IF
            D( L ) = RT1
            D( L+1 ) = RT2
            E( L ) = ZERO
            L = L + 2
            IF( L.LE.LEND )
     $         GO TO 40
            GO TO 140
         END IF
*
         IF( JTOT.EQ.NMAXIT )
     $      GO TO 140
         JTOT = JTOT + 1
*
*        Form shift.
*
         OPS = OPS + 12
         G = ( D( L+1 )-P ) / ( TWO*E( L ) )
         R = DLAPY2( G, ONE )
         G = D( M ) - P + ( E( L ) / ( G+SIGN( R, G ) ) )
*
         S = ONE
         C = ONE
         P = ZERO
*
*        Inner loop
*
         MM1 = M - 1
         OPS = OPS + 18*( M-L )
         DO 70 I = MM1, L, -1
            F = S*E( I )
            B = C*E( I )
            CALL DLARTG( G, F, C, S, R )
            IF( I.NE.M-1 )
     $         E( I+1 ) = R
            G = D( I+1 ) - P
            R = ( D( I )-G )*S + TWO*C*B
            P = S*R
            D( I+1 ) = G + P
            G = C*R - B
*
*           If eigenvectors are desired, then save rotations.
*
            IF( ICOMPZ.GT.0 ) THEN
               WORK( I ) = C
               WORK( N-1+I ) = -S
            END IF
*
   70    CONTINUE
*
*        If eigenvectors are desired, then apply saved rotations.
*
         IF( ICOMPZ.GT.0 ) THEN
            MM = M - L + 1
            OPS = OPS + 6*N*( MM-1 )
            CALL DLASR( 'R', 'V', 'B', N, MM, WORK( L ), WORK( N-1+L ),
     $                  Z( 1, L ), LDZ )
         END IF
*
         OPS = OPS + 1
         D( L ) = D( L ) - P
         E( L ) = G
         GO TO 40
*
*        Eigenvalue found.
*
   80    CONTINUE
         D( L ) = P
*
         L = L + 1
         IF( L.LE.LEND )
     $      GO TO 40
         GO TO 140
*
      ELSE
*
*        QR Iteration
*
*        Look for small superdiagonal element.
*
   90    CONTINUE
         IF( L.NE.LEND ) THEN
            LENDP1 = LEND + 1
            DO 100 M = L, LENDP1, -1
               OPS = OPS + 4
               TST = ABS( E( M-1 ) )**2
               IF( TST.LE.( EPS2*ABS( D( M ) ) )*ABS( D( M-1 ) )+
     $             SAFMIN )GO TO 110
  100       CONTINUE
         END IF
*
         M = LEND
*
  110    CONTINUE
         IF( M.GT.LEND )
     $      E( M-1 ) = ZERO
         P = D( L )
         IF( M.EQ.L )
     $      GO TO 130
*
*        If remaining matrix is 2-by-2, use DLAE2 or SLAEV2
*        to compute its eigensystem.
*
         IF( M.EQ.L-1 ) THEN
            IF( ICOMPZ.GT.0 ) THEN
               OPS = OPS + 22
               CALL DLAEV2( D( L-1 ), E( L-1 ), D( L ), RT1, RT2, C, S )
               WORK( M ) = C
               WORK( N-1+M ) = S
               OPS = OPS + 6*N
               CALL DLASR( 'R', 'V', 'F', N, 2, WORK( M ),
     $                     WORK( N-1+M ), Z( 1, L-1 ), LDZ )
            ELSE
               OPS = OPS + 15
               CALL DLAE2( D( L-1 ), E( L-1 ), D( L ), RT1, RT2 )
            END IF
            D( L-1 ) = RT1
            D( L ) = RT2
            E( L-1 ) = ZERO
            L = L - 2
            IF( L.GE.LEND )
     $         GO TO 90
            GO TO 140
         END IF
*
         IF( JTOT.EQ.NMAXIT )
     $      GO TO 140
         JTOT = JTOT + 1
*
*        Form shift.
*
         OPS = OPS + 12
         G = ( D( L-1 )-P ) / ( TWO*E( L-1 ) )
         R = DLAPY2( G, ONE )
         G = D( M ) - P + ( E( L-1 ) / ( G+SIGN( R, G ) ) )
*
         S = ONE
         C = ONE
         P = ZERO
*
*        Inner loop
*
         LM1 = L - 1
         OPS = OPS + 18*( L-M )
         DO 120 I = M, LM1
            F = S*E( I )
            B = C*E( I )
            CALL DLARTG( G, F, C, S, R )
            IF( I.NE.M )
     $         E( I-1 ) = R
            G = D( I ) - P
            R = ( D( I+1 )-G )*S + TWO*C*B
            P = S*R
            D( I ) = G + P
            G = C*R - B
*
*           If eigenvectors are desired, then save rotations.
*
            IF( ICOMPZ.GT.0 ) THEN
               WORK( I ) = C
               WORK( N-1+I ) = S
            END IF
*
  120    CONTINUE
*
*        If eigenvectors are desired, then apply saved rotations.
*
         IF( ICOMPZ.GT.0 ) THEN
            MM = L - M + 1
            OPS = OPS + 6*N*( MM-1 )
            CALL DLASR( 'R', 'V', 'F', N, MM, WORK( M ), WORK( N-1+M ),
     $                  Z( 1, M ), LDZ )
         END IF
*
         OPS = OPS + 1
         D( L ) = D( L ) - P
         E( LM1 ) = G
         GO TO 90
*
*        Eigenvalue found.
*
  130    CONTINUE
         D( L ) = P
*
         L = L - 1
         IF( L.GE.LEND )
     $      GO TO 90
         GO TO 140
*
      END IF
*
*     Undo scaling if necessary
*
  140 CONTINUE
      IF( ISCALE.EQ.1 ) THEN
         OPS = OPS + 2*( LENDSV-LSV ) + 1
         CALL DLASCL( 'G', 0, 0, SSFMAX, ANORM, LENDSV-LSV+1, 1,
     $                D( LSV ), N, INFO )
         CALL DLASCL( 'G', 0, 0, SSFMAX, ANORM, LENDSV-LSV, 1, E( LSV ),
     $                N, INFO )
      ELSE IF( ISCALE.EQ.2 ) THEN
         OPS = OPS + 2*( LENDSV-LSV ) + 1
         CALL DLASCL( 'G', 0, 0, SSFMIN, ANORM, LENDSV-LSV+1, 1,
     $                D( LSV ), N, INFO )
         CALL DLASCL( 'G', 0, 0, SSFMIN, ANORM, LENDSV-LSV, 1, E( LSV ),
     $                N, INFO )
      END IF
*
*     Check for no convergence to an eigenvalue after a total
*     of N*MAXIT iterations.
*
      IF( JTOT.LT.NMAXIT )
     $   GO TO 10
      DO 150 I = 1, N - 1
         IF( E( I ).NE.ZERO )
     $      INFO = INFO + 1
  150 CONTINUE
      GO TO 190
*
*     Order eigenvalues and eigenvectors.
*
  160 CONTINUE
      IF( ICOMPZ.EQ.0 ) THEN
*
*        Use Quick Sort
*
         CALL DLASRT( 'I', N, D, INFO )
*
      ELSE
*
*        Use Selection Sort to minimize swaps of eigenvectors
*
         DO 180 II = 2, N
            I = II - 1
            K = I
            P = D( I )
            DO 170 J = II, N
               IF( D( J ).LT.P ) THEN
                  K = J
                  P = D( J )
               END IF
  170       CONTINUE
            IF( K.NE.I ) THEN
               D( K ) = D( I )
               D( I ) = P
               CALL DSWAP( N, Z( 1, I ), 1, Z( 1, K ), 1 )
            END IF
  180    CONTINUE
      END IF
*
  190 CONTINUE
      RETURN
*
*     End of DSTEQR
*
      END
      SUBROUTINE DSTERF( N, D, E, INFO )
*
*  -- LAPACK routine (instrumented to count operations, version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     June 30, 1999
*
*     .. Scalar Arguments ..
      INTEGER            INFO, N
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   D( * ), E( * )
*     ..
*     Common block to return operation count and iteration count
*     ITCNT is initialized to 0, OPS is only incremented
*     .. Common blocks ..
      COMMON             / LATIME / OPS, ITCNT
*     ..
*     .. Scalars in Common ..
      DOUBLE PRECISION   ITCNT, OPS
*     ..
*
*  Purpose
*  =======
*
*  DSTERF computes all eigenvalues of a symmetric tridiagonal matrix
*  using the Pal-Walker-Kahan variant of the QL or QR algorithm.
*
*  Arguments
*  =========
*
*  N       (input) INTEGER
*          The order of the matrix.  N >= 0.
*
*  D       (input/output) DOUBLE PRECISION array, dimension (N)
*          On entry, the n diagonal elements of the tridiagonal matrix.
*          On exit, if INFO = 0, the eigenvalues in ascending order.
*
*  E       (input/output) DOUBLE PRECISION array, dimension (N-1)
*          On entry, the (n-1) subdiagonal elements of the tridiagonal
*          matrix.
*          On exit, E has been destroyed.
*
*  INFO    (output) INTEGER
*          = 0:  successful exit
*          < 0:  if INFO = -i, the i-th argument had an illegal value
*          > 0:  the algorithm failed to find all of the eigenvalues in
*                a total of 30*N iterations; if INFO = i, then i
*                elements of E have not converged to zero.
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE, TWO, THREE
      PARAMETER          ( ZERO = 0.0D0, ONE = 1.0D0, TWO = 2.0D0,
     $                   THREE = 3.0D0 )
      INTEGER            MAXIT
      PARAMETER          ( MAXIT = 30 )
*     ..
*     .. Local Scalars ..
      INTEGER            I, ISCALE, JTOT, L, L1, LEND, LENDSV, LSV, M,
     $                   NMAXIT
      DOUBLE PRECISION   ALPHA, ANORM, BB, C, EPS, EPS2, GAMMA, OLDC,
     $                   OLDGAM, P, R, RT1, RT2, RTE, S, SAFMAX, SAFMIN,
     $                   SIGMA, SSFMAX, SSFMIN
*     ..
*     .. External Functions ..
      DOUBLE PRECISION   DLAMCH, DLANST, DLAPY2
      EXTERNAL           DLAMCH, DLANST, DLAPY2
*     ..
*     .. External Subroutines ..
      EXTERNAL           DLAE2, DLASCL, DLASRT, XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, SIGN, SQRT
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters.
*
      INFO = 0
*
*     Quick return if possible
*
      ITCNT = 0
      IF( N.LT.0 ) THEN
         INFO = -1
         CALL XERBLA( 'DSTERF', -INFO )
         RETURN
      END IF
      IF( N.LE.1 )
     $   RETURN
*
*     Determine the unit roundoff for this environment.
*
      OPS = OPS + 6
      EPS = DLAMCH( 'E' )
      EPS2 = EPS**2
      SAFMIN = DLAMCH( 'S' )
      SAFMAX = ONE / SAFMIN
      SSFMAX = SQRT( SAFMAX ) / THREE
      SSFMIN = SQRT( SAFMIN ) / EPS2
*
*     Compute the eigenvalues of the tridiagonal matrix.
*
      NMAXIT = N*MAXIT
      SIGMA = ZERO
      JTOT = 0
*
*     Determine where the matrix splits and choose QL or QR iteration
*     for each block, according to whether top or bottom diagonal
*     element is smaller.
*
      L1 = 1
*
   10 CONTINUE
      IF( L1.GT.N )
     $   GO TO 170
      IF( L1.GT.1 )
     $   E( L1-1 ) = ZERO
      DO 20 M = L1, N - 1
         OPS = OPS + 4
         IF( ABS( E( M ) ).LE.( SQRT( ABS( D( M ) ) )*SQRT( ABS( D( M+
     $       1 ) ) ) )*EPS ) THEN
            E( M ) = ZERO
            GO TO 30
         END IF
   20 CONTINUE
      M = N
*
   30 CONTINUE
      L = L1
      LSV = L
      LEND = M
      LENDSV = LEND
      L1 = M + 1
      IF( LEND.EQ.L )
     $   GO TO 10
*
*     Scale submatrix in rows and columns L to LEND
*
      OPS = OPS + 2*( LEND-L+1 )
      ANORM = DLANST( 'I', LEND-L+1, D( L ), E( L ) )
      ISCALE = 0
      IF( ANORM.GT.SSFMAX ) THEN
         ISCALE = 1
         OPS = OPS + 2*( LEND-L ) + 1
         CALL DLASCL( 'G', 0, 0, ANORM, SSFMAX, LEND-L+1, 1, D( L ), N,
     $                INFO )
         CALL DLASCL( 'G', 0, 0, ANORM, SSFMAX, LEND-L, 1, E( L ), N,
     $                INFO )
      ELSE IF( ANORM.LT.SSFMIN ) THEN
         ISCALE = 2
         OPS = OPS + 2*( LEND-L ) + 1
         CALL DLASCL( 'G', 0, 0, ANORM, SSFMIN, LEND-L+1, 1, D( L ), N,
     $                INFO )
         CALL DLASCL( 'G', 0, 0, ANORM, SSFMIN, LEND-L, 1, E( L ), N,
     $                INFO )
      END IF
*
      OPS = OPS + 2*( LEND-L )
      DO 40 I = L, LEND - 1
         E( I ) = E( I )**2
   40 CONTINUE
*
*     Choose between QL and QR iteration
*
      IF( ABS( D( LEND ) ).LT.ABS( D( L ) ) ) THEN
         LEND = LSV
         L = LENDSV
      END IF
*
      IF( LEND.GE.L ) THEN
*
*        QL Iteration
*
*        Look for small subdiagonal element.
*
   50    CONTINUE
         IF( L.NE.LEND ) THEN
            DO 60 M = L, LEND - 1
               OPS = OPS + 3
               IF( ABS( E( M ) ).LE.EPS2*ABS( D( M )*D( M+1 ) ) )
     $            GO TO 70
   60       CONTINUE
         END IF
         M = LEND
*
   70    CONTINUE
         IF( M.LT.LEND )
     $      E( M ) = ZERO
         P = D( L )
         IF( M.EQ.L )
     $      GO TO 90
*
*        If remaining matrix is 2 by 2, use DLAE2 to compute its
*        eigenvalues.
*
         IF( M.EQ.L+1 ) THEN
            OPS = OPS + 16
            RTE = SQRT( E( L ) )
            CALL DLAE2( D( L ), RTE, D( L+1 ), RT1, RT2 )
            D( L ) = RT1
            D( L+1 ) = RT2
            E( L ) = ZERO
            L = L + 2
            IF( L.LE.LEND )
     $         GO TO 50
            GO TO 150
         END IF
*
         IF( JTOT.EQ.NMAXIT )
     $      GO TO 150
         JTOT = JTOT + 1
*
*        Form shift.
*
         OPS = OPS + 14
         RTE = SQRT( E( L ) )
         SIGMA = ( D( L+1 )-P ) / ( TWO*RTE )
         R = DLAPY2( SIGMA, ONE )
         SIGMA = P - ( RTE / ( SIGMA+SIGN( R, SIGMA ) ) )
*
         C = ONE
         S = ZERO
         GAMMA = D( M ) - SIGMA
         P = GAMMA*GAMMA
*
*        Inner loop
*
         OPS = OPS + 12*( M-L )
         DO 80 I = M - 1, L, -1
            BB = E( I )
            R = P + BB
            IF( I.NE.M-1 )
     $         E( I+1 ) = S*R
            OLDC = C
            C = P / R
            S = BB / R
            OLDGAM = GAMMA
            ALPHA = D( I )
            GAMMA = C*( ALPHA-SIGMA ) - S*OLDGAM
            D( I+1 ) = OLDGAM + ( ALPHA-GAMMA )
            IF( C.NE.ZERO ) THEN
               P = ( GAMMA*GAMMA ) / C
            ELSE
               P = OLDC*BB
            END IF
   80    CONTINUE
*
         OPS = OPS + 2
         E( L ) = S*P
         D( L ) = SIGMA + GAMMA
         GO TO 50
*
*        Eigenvalue found.
*
   90    CONTINUE
         D( L ) = P
*
         L = L + 1
         IF( L.LE.LEND )
     $      GO TO 50
         GO TO 150
*
      ELSE
*
*        QR Iteration
*
*        Look for small superdiagonal element.
*
  100    CONTINUE
         DO 110 M = L, LEND + 1, -1
            OPS = OPS + 3
            IF( ABS( E( M-1 ) ).LE.EPS2*ABS( D( M )*D( M-1 ) ) )
     $         GO TO 120
  110    CONTINUE
         M = LEND
*
  120    CONTINUE
         IF( M.GT.LEND )
     $      E( M-1 ) = ZERO
         P = D( L )
         IF( M.EQ.L )
     $      GO TO 140
*
*        If remaining matrix is 2 by 2, use DLAE2 to compute its
*        eigenvalues.
*
         IF( M.EQ.L-1 ) THEN
            OPS = OPS + 16
            RTE = SQRT( E( L-1 ) )
            CALL DLAE2( D( L ), RTE, D( L-1 ), RT1, RT2 )
            D( L ) = RT1
            D( L-1 ) = RT2
            E( L-1 ) = ZERO
            L = L - 2
            IF( L.GE.LEND )
     $         GO TO 100
            GO TO 150
         END IF
*
         IF( JTOT.EQ.NMAXIT )
     $      GO TO 150
         JTOT = JTOT + 1
*
*        Form shift.
*
         OPS = OPS + 14
         RTE = SQRT( E( L-1 ) )
         SIGMA = ( D( L-1 )-P ) / ( TWO*RTE )
         R = DLAPY2( SIGMA, ONE )
         SIGMA = P - ( RTE / ( SIGMA+SIGN( R, SIGMA ) ) )
*
         C = ONE
         S = ZERO
         GAMMA = D( M ) - SIGMA
         P = GAMMA*GAMMA
*
*        Inner loop
*
         OPS = OPS + 12*( L-M )
         DO 130 I = M, L - 1
            BB = E( I )
            R = P + BB
            IF( I.NE.M )
     $         E( I-1 ) = S*R
            OLDC = C
            C = P / R
            S = BB / R
            OLDGAM = GAMMA
            ALPHA = D( I+1 )
            GAMMA = C*( ALPHA-SIGMA ) - S*OLDGAM
            D( I ) = OLDGAM + ( ALPHA-GAMMA )
            IF( C.NE.ZERO ) THEN
               P = ( GAMMA*GAMMA ) / C
            ELSE
               P = OLDC*BB
            END IF
  130    CONTINUE
*
         OPS = OPS + 2
         E( L-1 ) = S*P
         D( L ) = SIGMA + GAMMA
         GO TO 100
*
*        Eigenvalue found.
*
  140    CONTINUE
         D( L ) = P
*
         L = L - 1
         IF( L.GE.LEND )
     $      GO TO 100
         GO TO 150
*
      END IF
*
*     Undo scaling if necessary
*
  150 CONTINUE
      IF( ISCALE.EQ.1 ) THEN
         OPS = OPS + LENDSV - LSV + 1
         CALL DLASCL( 'G', 0, 0, SSFMAX, ANORM, LENDSV-LSV+1, 1,
     $                D( LSV ), N, INFO )
      END IF
      IF( ISCALE.EQ.2 ) THEN
         OPS = OPS + LENDSV - LSV + 1
         CALL DLASCL( 'G', 0, 0, SSFMIN, ANORM, LENDSV-LSV+1, 1,
     $                D( LSV ), N, INFO )
      END IF
*
*     Check for no convergence to an eigenvalue after a total
*     of N*MAXIT iterations.
*
      IF( JTOT.LT.NMAXIT )
     $   GO TO 10
      DO 160 I = 1, N - 1
         IF( E( I ).NE.ZERO )
     $      INFO = INFO + 1
  160 CONTINUE
      GO TO 180
*
*     Sort eigenvalues in increasing order.
*
  170 CONTINUE
      CALL DLASRT( 'I', N, D, INFO )
*
  180 CONTINUE
      RETURN
*
*     End of DSTERF
*
      END
      SUBROUTINE DTGEVC( SIDE, HOWMNY, SELECT, N, A, LDA, B, LDB, VL,
     $                   LDVL, VR, LDVR, MM, M, WORK, INFO )
*
*  -- LAPACK routine (instrumented to count operations, version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     June 30, 1999
*
*     .. Scalar Arguments ..
      CHARACTER          HOWMNY, SIDE
      INTEGER            INFO, LDA, LDB, LDVL, LDVR, M, MM, N
*     ..
*     .. Array Arguments ..
      LOGICAL            SELECT( * )
      DOUBLE PRECISION   A( LDA, * ), B( LDB, * ), VL( LDVL, * ),
     $                   VR( LDVR, * ), WORK( * )
*     ..
*
*     ---------------------- Begin Timing Code -------------------------
*     Common block to return operation count and iteration count
*     ITCNT is initialized to 0, OPS is only incremented
*     OPST is used to accumulate small contributions to OPS
*     to avoid roundoff error
*     .. Common blocks ..
      COMMON             / LATIME / OPS, ITCNT
*     ..
*     .. Scalars in Common ..
      DOUBLE PRECISION   ITCNT, OPS
*     ..
*     ----------------------- End Timing Code --------------------------
*
*
*  Purpose
*  =======
*
*  DTGEVC computes some or all of the right and/or left generalized
*  eigenvectors of a pair of real upper triangular matrices (A,B).
*
*  The right generalized eigenvector x and the left generalized
*  eigenvector y of (A,B) corresponding to a generalized eigenvalue
*  w are defined by:
*
*          (A - wB) * x = 0  and  y**H * (A - wB) = 0
*
*  where y**H denotes the conjugate tranpose of y.
*
*  If an eigenvalue w is determined by zero diagonal elements of both A
*  and B, a unit vector is returned as the corresponding eigenvector.
*
*  If all eigenvectors are requested, the routine may either return
*  the matrices X and/or Y of right or left eigenvectors of (A,B), or
*  the products Z*X and/or Q*Y, where Z and Q are input orthogonal
*  matrices.  If (A,B) was obtained from the generalized real-Schur
*  factorization of an original pair of matrices
*     (A0,B0) = (Q*A*Z**H,Q*B*Z**H),
*  then Z*X and Q*Y are the matrices of right or left eigenvectors of
*  A.
*
*  A must be block upper triangular, with 1-by-1 and 2-by-2 diagonal
*  blocks.  Corresponding to each 2-by-2 diagonal block is a complex
*  conjugate pair of eigenvalues and eigenvectors; only one
*  eigenvector of the pair is computed, namely the one corresponding
*  to the eigenvalue with positive imaginary part.
*
*  Arguments
*  =========
*
*  SIDE    (input) CHARACTER*1
*          = 'R': compute right eigenvectors only;
*          = 'L': compute left eigenvectors only;
*          = 'B': compute both right and left eigenvectors.
*
*  HOWMNY  (input) CHARACTER*1
*          = 'A': compute all right and/or left eigenvectors;
*          = 'B': compute all right and/or left eigenvectors, and
*                 backtransform them using the input matrices supplied
*                 in VR and/or VL;
*          = 'S': compute selected right and/or left eigenvectors,
*                 specified by the logical array SELECT.
*
*  SELECT  (input) LOGICAL array, dimension (N)
*          If HOWMNY='S', SELECT specifies the eigenvectors to be
*          computed.
*          If HOWMNY='A' or 'B', SELECT is not referenced.
*          To select the real eigenvector corresponding to the real
*          eigenvalue w(j), SELECT(j) must be set to .TRUE.  To select
*          the complex eigenvector corresponding to a complex conjugate
*          pair w(j) and w(j+1), either SELECT(j) or SELECT(j+1) must
*          be set to .TRUE..
*
*  N       (input) INTEGER
*          The order of the matrices A and B.  N >= 0.
*
*  A       (input) DOUBLE PRECISION array, dimension (LDA,N)
*          The upper quasi-triangular matrix A.
*
*  LDA     (input) INTEGER
*          The leading dimension of array A.  LDA >= max(1,N).
*
*  B       (input) DOUBLE PRECISION array, dimension (LDB,N)
*          The upper triangular matrix B.  If A has a 2-by-2 diagonal
*          block, then the corresponding 2-by-2 block of B must be
*          diagonal with positive elements.
*
*  LDB     (input) INTEGER
*          The leading dimension of array B.  LDB >= max(1,N).
*
*  VL      (input/output) DOUBLE PRECISION array, dimension (LDVL,MM)
*          On entry, if SIDE = 'L' or 'B' and HOWMNY = 'B', VL must
*          contain an N-by-N matrix Q (usually the orthogonal matrix Q
*          of left Schur vectors returned by DHGEQZ).
*          On exit, if SIDE = 'L' or 'B', VL contains:
*          if HOWMNY = 'A', the matrix Y of left eigenvectors of (A,B);
*          if HOWMNY = 'B', the matrix Q*Y;
*          if HOWMNY = 'S', the left eigenvectors of (A,B) specified by
*                      SELECT, stored consecutively in the columns of
*                      VL, in the same order as their eigenvalues.
*          If SIDE = 'R', VL is not referenced.
*
*          A complex eigenvector corresponding to a complex eigenvalue
*          is stored in two consecutive columns, the first holding the
*          real part, and the second the imaginary part.
*
*  LDVL    (input) INTEGER
*          The leading dimension of array VL.
*          LDVL >= max(1,N) if SIDE = 'L' or 'B'; LDVL >= 1 otherwise.
*
*  VR      (input/output) DOUBLE PRECISION array, dimension (LDVR,MM)
*          On entry, if SIDE = 'R' or 'B' and HOWMNY = 'B', VR must
*          contain an N-by-N matrix Q (usually the orthogonal matrix Z
*          of right Schur vectors returned by DHGEQZ).
*          On exit, if SIDE = 'R' or 'B', VR contains:
*          if HOWMNY = 'A', the matrix X of right eigenvectors of (A,B);
*          if HOWMNY = 'B', the matrix Z*X;
*          if HOWMNY = 'S', the right eigenvectors of (A,B) specified by
*                      SELECT, stored consecutively in the columns of
*                      VR, in the same order as their eigenvalues.
*          If SIDE = 'L', VR is not referenced.
*
*          A complex eigenvector corresponding to a complex eigenvalue
*          is stored in two consecutive columns, the first holding the
*          real part and the second the imaginary part.
*
*  LDVR    (input) INTEGER
*          The leading dimension of the array VR.
*          LDVR >= max(1,N) if SIDE = 'R' or 'B'; LDVR >= 1 otherwise.
*
*  MM      (input) INTEGER
*          The number of columns in the arrays VL and/or VR. MM >= M.
*
*  M       (output) INTEGER
*          The number of columns in the arrays VL and/or VR actually
*          used to store the eigenvectors.  If HOWMNY = 'A' or 'B', M
*          is set to N.  Each selected real eigenvector occupies one
*          column and each selected complex eigenvector occupies two
*          columns.
*
*  WORK    (workspace) DOUBLE PRECISION array, dimension (6*N)
*
*  INFO    (output) INTEGER
*          = 0:  successful exit.
*          < 0:  if INFO = -i, the i-th argument had an illegal value.
*          > 0:  the 2-by-2 block (INFO:INFO+1) does not have a complex
*                eigenvalue.
*
*  Further Details
*  ===============
*
*  Allocation of workspace:
*  ---------- -- ---------
*
*     WORK( j ) = 1-norm of j-th column of A, above the diagonal
*     WORK( N+j ) = 1-norm of j-th column of B, above the diagonal
*     WORK( 2*N+1:3*N ) = real part of eigenvector
*     WORK( 3*N+1:4*N ) = imaginary part of eigenvector
*     WORK( 4*N+1:5*N ) = real part of back-transformed eigenvector
*     WORK( 5*N+1:6*N ) = imaginary part of back-transformed eigenvector
*
*  Rowwise vs. columnwise solution methods:
*  ------- --  ---------- -------- -------
*
*  Finding a generalized eigenvector consists basically of solving the
*  singular triangular system
*
*   (A - w B) x = 0     (for right) or:   (A - w B)**H y = 0  (for left)
*
*  Consider finding the i-th right eigenvector (assume all eigenvalues
*  are real). The equation to be solved is:
*       n                   i
*  0 = sum  C(j,k) v(k)  = sum  C(j,k) v(k)     for j = i,. . .,1
*      k=j                 k=j
*
*  where  C = (A - w B)  (The components v(i+1:n) are 0.)
*
*  The "rowwise" method is:
*
*  (1)  v(i) := 1
*  for j = i-1,. . .,1:
*                          i
*      (2) compute  s = - sum C(j,k) v(k)   and
*                        k=j+1
*
*      (3) v(j) := s / C(j,j)
*
*  Step 2 is sometimes called the "dot product" step, since it is an
*  inner product between the j-th row and the portion of the eigenvector
*  that has been computed so far.
*
*  The "columnwise" method consists basically in doing the sums
*  for all the rows in parallel.  As each v(j) is computed, the
*  contribution of v(j) times the j-th column of C is added to the
*  partial sums.  Since FORTRAN arrays are stored columnwise, this has
*  the advantage that at each step, the elements of C that are accessed
*  are adjacent to one another, whereas with the rowwise method, the
*  elements accessed at a step are spaced LDA (and LDB) words apart.
*
*  When finding left eigenvectors, the matrix in question is the
*  transpose of the one in storage, so the rowwise method then
*  actually accesses columns of A and B at each step, and so is the
*  preferred method.
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE, SAFETY
      PARAMETER          ( ZERO = 0.0D+0, ONE = 1.0D+0,
     $                   SAFETY = 1.0D+2 )
*     ..
*     .. Local Scalars ..
      LOGICAL            COMPL, COMPR, IL2BY2, ILABAD, ILALL, ILBACK,
     $                   ILBBAD, ILCOMP, ILCPLX, LSA, LSB
      INTEGER            I, IBEG, IEIG, IEND, IHWMNY, IINFO, IM, IN2BY2,
     $                   ISIDE, J, JA, JC, JE, JR, JW, NA, NW
      DOUBLE PRECISION   ACOEF, ACOEFA, ANORM, ASCALE, BCOEFA, BCOEFI,
     $                   BCOEFR, BIG, BIGNUM, BNORM, BSCALE, CIM2A,
     $                   CIM2B, CIMAGA, CIMAGB, CRE2A, CRE2B, CREALA,
     $                   CREALB, DMIN, OPSSCA, OPST, SAFMIN, SALFAR,
     $                   SBETA, SCALE, SMALL, TEMP, TEMP2, TEMP2I,
     $                   TEMP2R, ULP, XMAX, XSCALE
*     ..
*     .. Local Arrays ..
      DOUBLE PRECISION   BDIAG( 2 ), SUM( 2, 2 ), SUMA( 2, 2 ),
     $                   SUMB( 2, 2 )
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      DOUBLE PRECISION   DLAMCH
      EXTERNAL           LSAME, DLAMCH
*     ..
*     .. External Subroutines ..
      EXTERNAL           DGEMV, DLABAD, DLACPY, DLAG2, DLALN2, XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, DBLE, MAX, MIN
*     ..
*     .. Executable Statements ..
*
*     Decode and Test the input parameters
*
      IF( LSAME( HOWMNY, 'A' ) ) THEN
         IHWMNY = 1
         ILALL = .TRUE.
         ILBACK = .FALSE.
      ELSE IF( LSAME( HOWMNY, 'S' ) ) THEN
         IHWMNY = 2
         ILALL = .FALSE.
         ILBACK = .FALSE.
      ELSE IF( LSAME( HOWMNY, 'B' ) .OR. LSAME( HOWMNY, 'T' ) ) THEN
         IHWMNY = 3
         ILALL = .TRUE.
         ILBACK = .TRUE.
      ELSE
         IHWMNY = -1
         ILALL = .TRUE.
      END IF
*
      IF( LSAME( SIDE, 'R' ) ) THEN
         ISIDE = 1
         COMPL = .FALSE.
         COMPR = .TRUE.
      ELSE IF( LSAME( SIDE, 'L' ) ) THEN
         ISIDE = 2
         COMPL = .TRUE.
         COMPR = .FALSE.
      ELSE IF( LSAME( SIDE, 'B' ) ) THEN
         ISIDE = 3
         COMPL = .TRUE.
         COMPR = .TRUE.
      ELSE
         ISIDE = -1
      END IF
*
      INFO = 0
      IF( ISIDE.LT.0 ) THEN
         INFO = -1
      ELSE IF( IHWMNY.LT.0 ) THEN
         INFO = -2
      ELSE IF( N.LT.0 ) THEN
         INFO = -4
      ELSE IF( LDA.LT.MAX( 1, N ) ) THEN
         INFO = -6
      ELSE IF( LDB.LT.MAX( 1, N ) ) THEN
         INFO = -8
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DTGEVC', -INFO )
         RETURN
      END IF
*
*     Count the number of eigenvectors to be computed
*
      IF( .NOT.ILALL ) THEN
         IM = 0
         ILCPLX = .FALSE.
         DO 10 J = 1, N
            IF( ILCPLX ) THEN
               ILCPLX = .FALSE.
               GO TO 10
            END IF
            IF( J.LT.N ) THEN
               IF( A( J+1, J ).NE.ZERO )
     $            ILCPLX = .TRUE.
            END IF
            IF( ILCPLX ) THEN
               IF( SELECT( J ) .OR. SELECT( J+1 ) )
     $            IM = IM + 2
            ELSE
               IF( SELECT( J ) )
     $            IM = IM + 1
            END IF
   10    CONTINUE
      ELSE
         IM = N
      END IF
*
*     Check 2-by-2 diagonal blocks of A, B
*
      ILABAD = .FALSE.
      ILBBAD = .FALSE.
      DO 20 J = 1, N - 1
         IF( A( J+1, J ).NE.ZERO ) THEN
            IF( B( J, J ).EQ.ZERO .OR. B( J+1, J+1 ).EQ.ZERO .OR.
     $          B( J, J+1 ).NE.ZERO )ILBBAD = .TRUE.
            IF( J.LT.N-1 ) THEN
               IF( A( J+2, J+1 ).NE.ZERO )
     $            ILABAD = .TRUE.
            END IF
         END IF
   20 CONTINUE
*
      IF( ILABAD ) THEN
         INFO = -5
      ELSE IF( ILBBAD ) THEN
         INFO = -7
      ELSE IF( COMPL .AND. LDVL.LT.N .OR. LDVL.LT.1 ) THEN
         INFO = -10
      ELSE IF( COMPR .AND. LDVR.LT.N .OR. LDVR.LT.1 ) THEN
         INFO = -12
      ELSE IF( MM.LT.IM ) THEN
         INFO = -13
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DTGEVC', -INFO )
         RETURN
      END IF
*
*     Quick return if possible
*
      M = IM
      IF( N.EQ.0 )
     $   RETURN
*
*     Machine Constants
*
      SAFMIN = DLAMCH( 'Safe minimum' )
      BIG = ONE / SAFMIN
      CALL DLABAD( SAFMIN, BIG )
      ULP = DLAMCH( 'Epsilon' )*DLAMCH( 'Base' )
      SMALL = SAFMIN*N / ULP
      BIG = ONE / SMALL
      BIGNUM = ONE / ( SAFMIN*N )
*
*     Compute the 1-norm of each column of the strictly upper triangular
*     part (i.e., excluding all elements belonging to the diagonal
*     blocks) of A and B to check for possible overflow in the
*     triangular solver.
*
      ANORM = ABS( A( 1, 1 ) )
      IF( N.GT.1 )
     $   ANORM = ANORM + ABS( A( 2, 1 ) )
      BNORM = ABS( B( 1, 1 ) )
      WORK( 1 ) = ZERO
      WORK( N+1 ) = ZERO
*
      DO 50 J = 2, N
         TEMP = ZERO
         TEMP2 = ZERO
         IF( A( J, J-1 ).EQ.ZERO ) THEN
            IEND = J - 1
         ELSE
            IEND = J - 2
         END IF
         DO 30 I = 1, IEND
            TEMP = TEMP + ABS( A( I, J ) )
            TEMP2 = TEMP2 + ABS( B( I, J ) )
   30    CONTINUE
         WORK( J ) = TEMP
         WORK( N+J ) = TEMP2
         DO 40 I = IEND + 1, MIN( J+1, N )
            TEMP = TEMP + ABS( A( I, J ) )
            TEMP2 = TEMP2 + ABS( B( I, J ) )
   40    CONTINUE
         ANORM = MAX( ANORM, TEMP )
         BNORM = MAX( BNORM, TEMP2 )
   50 CONTINUE
*
      ASCALE = ONE / MAX( ANORM, SAFMIN )
      BSCALE = ONE / MAX( BNORM, SAFMIN )
*
*     ---------------------- Begin Timing Code -------------------------
      OPS = OPS + DBLE( N**2+3*N+6 )
*     ----------------------- End Timing Code --------------------------
*
*     Left eigenvectors
*
      IF( COMPL ) THEN
         IEIG = 0
*
*        Main loop over eigenvalues
*
         ILCPLX = .FALSE.
         DO 220 JE = 1, N
*
*           Skip this iteration if (a) HOWMNY='S' and SELECT=.FALSE., or
*           (b) this would be the second of a complex pair.
*           Check for complex eigenvalue, so as to be sure of which
*           entry(-ies) of SELECT to look at.
*
            IF( ILCPLX ) THEN
               ILCPLX = .FALSE.
               GO TO 220
            END IF
            NW = 1
            IF( JE.LT.N ) THEN
               IF( A( JE+1, JE ).NE.ZERO ) THEN
                  ILCPLX = .TRUE.
                  NW = 2
               END IF
            END IF
            IF( ILALL ) THEN
               ILCOMP = .TRUE.
            ELSE IF( ILCPLX ) THEN
               ILCOMP = SELECT( JE ) .OR. SELECT( JE+1 )
            ELSE
               ILCOMP = SELECT( JE )
            END IF
            IF( .NOT.ILCOMP )
     $         GO TO 220
*
*           Decide if (a) singular pencil, (b) real eigenvalue, or
*           (c) complex eigenvalue.
*
            IF( .NOT.ILCPLX ) THEN
               IF( ABS( A( JE, JE ) ).LE.SAFMIN .AND.
     $             ABS( B( JE, JE ) ).LE.SAFMIN ) THEN
*
*                 Singular matrix pencil -- returns unit eigenvector
*
                  IEIG = IEIG + 1
                  DO 60 JR = 1, N
                     VL( JR, IEIG ) = ZERO
   60             CONTINUE
                  VL( IEIG, IEIG ) = ONE
                  GO TO 220
               END IF
            END IF
*
*           Clear vector
*
            DO 70 JR = 1, NW*N
               WORK( 2*N+JR ) = ZERO
   70       CONTINUE
*                                                 T
*           Compute coefficients in  ( a A - b B )  y = 0
*              a  is  ACOEF
*              b  is  BCOEFR + i*BCOEFI
*
            IF( .NOT.ILCPLX ) THEN
*
*              Real eigenvalue
*
               TEMP = ONE / MAX( ABS( A( JE, JE ) )*ASCALE,
     $                ABS( B( JE, JE ) )*BSCALE, SAFMIN )
               SALFAR = ( TEMP*A( JE, JE ) )*ASCALE
               SBETA = ( TEMP*B( JE, JE ) )*BSCALE
               ACOEF = SBETA*ASCALE
               BCOEFR = SALFAR*BSCALE
               BCOEFI = ZERO
*
*              Scale to avoid underflow
*
               SCALE = ONE
               LSA = ABS( SBETA ).GE.SAFMIN .AND. ABS( ACOEF ).LT.SMALL
               LSB = ABS( SALFAR ).GE.SAFMIN .AND. ABS( BCOEFR ).LT.
     $               SMALL
               IF( LSA )
     $            SCALE = ( SMALL / ABS( SBETA ) )*MIN( ANORM, BIG )
               IF( LSB )
     $            SCALE = MAX( SCALE, ( SMALL / ABS( SALFAR ) )*
     $                    MIN( BNORM, BIG ) )
               IF( LSA .OR. LSB ) THEN
                  SCALE = MIN( SCALE, ONE /
     $                    ( SAFMIN*MAX( ONE, ABS( ACOEF ),
     $                    ABS( BCOEFR ) ) ) )
                  IF( LSA ) THEN
                     ACOEF = ASCALE*( SCALE*SBETA )
                  ELSE
                     ACOEF = SCALE*ACOEF
                  END IF
                  IF( LSB ) THEN
                     BCOEFR = BSCALE*( SCALE*SALFAR )
                  ELSE
                     BCOEFR = SCALE*BCOEFR
                  END IF
               END IF
               ACOEFA = ABS( ACOEF )
               BCOEFA = ABS( BCOEFR )
*
*              First component is 1
*
               WORK( 2*N+JE ) = ONE
               XMAX = ONE
            ELSE
*
*              Complex eigenvalue
*
               CALL DLAG2( A( JE, JE ), LDA, B( JE, JE ), LDB,
     $                     SAFMIN*SAFETY, ACOEF, TEMP, BCOEFR, TEMP2,
     $                     BCOEFI )
               BCOEFI = -BCOEFI
               IF( BCOEFI.EQ.ZERO ) THEN
                  INFO = JE
                  RETURN
               END IF
*
*              Scale to avoid over/underflow
*
               ACOEFA = ABS( ACOEF )
               BCOEFA = ABS( BCOEFR ) + ABS( BCOEFI )
               SCALE = ONE
               IF( ACOEFA*ULP.LT.SAFMIN .AND. ACOEFA.GE.SAFMIN )
     $            SCALE = ( SAFMIN / ULP ) / ACOEFA
               IF( BCOEFA*ULP.LT.SAFMIN .AND. BCOEFA.GE.SAFMIN )
     $            SCALE = MAX( SCALE, ( SAFMIN / ULP ) / BCOEFA )
               IF( SAFMIN*ACOEFA.GT.ASCALE )
     $            SCALE = ASCALE / ( SAFMIN*ACOEFA )
               IF( SAFMIN*BCOEFA.GT.BSCALE )
     $            SCALE = MIN( SCALE, BSCALE / ( SAFMIN*BCOEFA ) )
               IF( SCALE.NE.ONE ) THEN
                  ACOEF = SCALE*ACOEF
                  ACOEFA = ABS( ACOEF )
                  BCOEFR = SCALE*BCOEFR
                  BCOEFI = SCALE*BCOEFI
                  BCOEFA = ABS( BCOEFR ) + ABS( BCOEFI )
               END IF
*
*              Compute first two components of eigenvector
*
               TEMP = ACOEF*A( JE+1, JE )
               TEMP2R = ACOEF*A( JE, JE ) - BCOEFR*B( JE, JE )
               TEMP2I = -BCOEFI*B( JE, JE )
               IF( ABS( TEMP ).GT.ABS( TEMP2R )+ABS( TEMP2I ) ) THEN
                  WORK( 2*N+JE ) = ONE
                  WORK( 3*N+JE ) = ZERO
                  WORK( 2*N+JE+1 ) = -TEMP2R / TEMP
                  WORK( 3*N+JE+1 ) = -TEMP2I / TEMP
               ELSE
                  WORK( 2*N+JE+1 ) = ONE
                  WORK( 3*N+JE+1 ) = ZERO
                  TEMP = ACOEF*A( JE, JE+1 )
                  WORK( 2*N+JE ) = ( BCOEFR*B( JE+1, JE+1 )-ACOEF*
     $                             A( JE+1, JE+1 ) ) / TEMP
                  WORK( 3*N+JE ) = BCOEFI*B( JE+1, JE+1 ) / TEMP
               END IF
               XMAX = MAX( ABS( WORK( 2*N+JE ) )+ABS( WORK( 3*N+JE ) ),
     $                ABS( WORK( 2*N+JE+1 ) )+ABS( WORK( 3*N+JE+1 ) ) )
            END IF
*
            DMIN = MAX( ULP*ACOEFA*ANORM, ULP*BCOEFA*BNORM, SAFMIN )
*
*                                           T
*           Triangular solve of  (a A - b B)  y = 0
*
*                                   T
*           (rowwise in  (a A - b B) , or columnwise in (a A - b B) )
*
            IL2BY2 = .FALSE.
*           ------------------- Begin Timing Code ----------------------
            OPST = ZERO
            IN2BY2 = 0
*           -------------------- End Timing Code -----------------------
*
            DO 160 J = JE + NW, N
*              ------------------- Begin Timing Code -------------------
               OPSSCA = DBLE( NW*( J-JE )+1 )
*              -------------------- End Timing Code --------------------
               IF( IL2BY2 ) THEN
                  IL2BY2 = .FALSE.
                  GO TO 160
               END IF
*
               NA = 1
               BDIAG( 1 ) = B( J, J )
               IF( J.LT.N ) THEN
                  IF( A( J+1, J ).NE.ZERO ) THEN
                     IL2BY2 = .TRUE.
                     BDIAG( 2 ) = B( J+1, J+1 )
                     NA = 2
*                    ---------------- Begin Timing Code ----------------
                     IN2BY2 = IN2BY2 + 1
*                    ----------------- End Timing Code -----------------
                  END IF
               END IF
*
*              Check whether scaling is necessary for dot products
*
               XSCALE = ONE / MAX( ONE, XMAX )
               TEMP = MAX( WORK( J ), WORK( N+J ),
     $                ACOEFA*WORK( J )+BCOEFA*WORK( N+J ) )
               IF( IL2BY2 )
     $            TEMP = MAX( TEMP, WORK( J+1 ), WORK( N+J+1 ),
     $                   ACOEFA*WORK( J+1 )+BCOEFA*WORK( N+J+1 ) )
               IF( TEMP.GT.BIGNUM*XSCALE ) THEN
                  DO 90 JW = 0, NW - 1
                     DO 80 JR = JE, J - 1
                        WORK( ( JW+2 )*N+JR ) = XSCALE*
     $                     WORK( ( JW+2 )*N+JR )
   80                CONTINUE
   90             CONTINUE
                  XMAX = XMAX*XSCALE
*                 ------------------ Begin Timing Code -----------------
                  OPST = OPST + OPSSCA
*                 ------------------- End Timing Code ------------------
               END IF
*
*              Compute dot products
*
*                    j-1
*              SUM = sum  conjg( a*A(k,j) - b*B(k,j) )*x(k)
*                    k=je
*
*              To reduce the op count, this is done as
*
*              _        j-1                  _        j-1
*              a*conjg( sum  A(k,j)*x(k) ) - b*conjg( sum  B(k,j)*x(k) )
*                       k=je                          k=je
*
*              which may cause underflow problems if A or B are close
*              to underflow.  (E.g., less than SMALL.)
*
*
*              A series of compiler directives to defeat vectorization
*              for the next loop
*
*$PL$ CMCHAR=' '
CDIR$          NEXTSCALAR
C$DIR          SCALAR
CDIR$          NEXT SCALAR
CVD$L          NOVECTOR
CDEC$          NOVECTOR
CVD$           NOVECTOR
*VDIR          NOVECTOR
*VOCL          LOOP,SCALAR
CIBM           PREFER SCALAR
*$PL$ CMCHAR='*'
*
               DO 120 JW = 1, NW
*
*$PL$ CMCHAR=' '
CDIR$             NEXTSCALAR
C$DIR             SCALAR
CDIR$             NEXT SCALAR
CVD$L             NOVECTOR
CDEC$             NOVECTOR
CVD$              NOVECTOR
*VDIR             NOVECTOR
*VOCL             LOOP,SCALAR
CIBM              PREFER SCALAR
*$PL$ CMCHAR='*'
*
                  DO 110 JA = 1, NA
                     SUMA( JA, JW ) = ZERO
                     SUMB( JA, JW ) = ZERO
*
                     DO 100 JR = JE, J - 1
                        SUMA( JA, JW ) = SUMA( JA, JW ) +
     $                                   A( JR, J+JA-1 )*
     $                                   WORK( ( JW+1 )*N+JR )
                        SUMB( JA, JW ) = SUMB( JA, JW ) +
     $                                   B( JR, J+JA-1 )*
     $                                   WORK( ( JW+1 )*N+JR )
  100                CONTINUE
  110             CONTINUE
  120          CONTINUE
*
*$PL$ CMCHAR=' '
CDIR$          NEXTSCALAR
C$DIR          SCALAR
CDIR$          NEXT SCALAR
CVD$L          NOVECTOR
CDEC$          NOVECTOR
CVD$           NOVECTOR
*VDIR          NOVECTOR
*VOCL          LOOP,SCALAR
CIBM           PREFER SCALAR
*$PL$ CMCHAR='*'
*
               DO 130 JA = 1, NA
                  IF( ILCPLX ) THEN
                     SUM( JA, 1 ) = -ACOEF*SUMA( JA, 1 ) +
     $                              BCOEFR*SUMB( JA, 1 ) -
     $                              BCOEFI*SUMB( JA, 2 )
                     SUM( JA, 2 ) = -ACOEF*SUMA( JA, 2 ) +
     $                              BCOEFR*SUMB( JA, 2 ) +
     $                              BCOEFI*SUMB( JA, 1 )
                  ELSE
                     SUM( JA, 1 ) = -ACOEF*SUMA( JA, 1 ) +
     $                              BCOEFR*SUMB( JA, 1 )
                  END IF
  130          CONTINUE
*
*                                  T
*              Solve  ( a A - b B )  y = SUM(,)
*              with scaling and perturbation of the denominator
*
               CALL DLALN2( .TRUE., NA, NW, DMIN, ACOEF, A( J, J ), LDA,
     $                      BDIAG( 1 ), BDIAG( 2 ), SUM, 2, BCOEFR,
     $                      BCOEFI, WORK( 2*N+J ), N, SCALE, TEMP,
     $                      IINFO )
               IF( SCALE.LT.ONE ) THEN
                  DO 150 JW = 0, NW - 1
                     DO 140 JR = JE, J - 1
                        WORK( ( JW+2 )*N+JR ) = SCALE*
     $                     WORK( ( JW+2 )*N+JR )
  140                CONTINUE
  150             CONTINUE
                  XMAX = SCALE*XMAX
*                 ------------------ Begin Timing Code -----------------
                  OPST = OPST + OPSSCA
*                 ------------------- End Timing Code ------------------
               END IF
               XMAX = MAX( XMAX, TEMP )
  160       CONTINUE
*
*           Copy eigenvector to VL, back transforming if
*           HOWMNY='B'.
*
            IEIG = IEIG + 1
            IF( ILBACK ) THEN
               DO 170 JW = 0, NW - 1
                  CALL DGEMV( 'N', N, N+1-JE, ONE, VL( 1, JE ), LDVL,
     $                        WORK( ( JW+2 )*N+JE ), 1, ZERO,
     $                        WORK( ( JW+4 )*N+1 ), 1 )
  170          CONTINUE
               CALL DLACPY( ' ', N, NW, WORK( 4*N+1 ), N, VL( 1, JE ),
     $                      LDVL )
               IBEG = 1
            ELSE
               CALL DLACPY( ' ', N, NW, WORK( 2*N+1 ), N, VL( 1, IEIG ),
     $                      LDVL )
               IBEG = JE
            END IF
*
*           Scale eigenvector
*
            XMAX = ZERO
            IF( ILCPLX ) THEN
               DO 180 J = IBEG, N
                  XMAX = MAX( XMAX, ABS( VL( J, IEIG ) )+
     $                   ABS( VL( J, IEIG+1 ) ) )
  180          CONTINUE
            ELSE
               DO 190 J = IBEG, N
                  XMAX = MAX( XMAX, ABS( VL( J, IEIG ) ) )
  190          CONTINUE
            END IF
*
            IF( XMAX.GT.SAFMIN ) THEN
               XSCALE = ONE / XMAX
*
               DO 210 JW = 0, NW - 1
                  DO 200 JR = IBEG, N
                     VL( JR, IEIG+JW ) = XSCALE*VL( JR, IEIG+JW )
  200             CONTINUE
  210          CONTINUE
            END IF
            IEIG = IEIG + NW - 1
*
*           ------------------- Begin Timing Code ----------------------
*           Opcounts for each eigenvector
*
*                                Real                Complex
*           Initialization       8--16               71--87
*
*           Dot Prod (per iter)  4*NA*(J-JE) + 2     8*NA*(J-JE) + 2
*                                + 6*NA + scaling    + 13*NA + scaling
*           Solve (per iter)     NA*(5 + 7*(NA-1))   NA*(17 + 17*(NA-1))
*                                + scaling           + scaling
*
*           Back xform           2*N*(N+1-JE) - N    4*N*(N+1-JE) - 2*N
*           Scaling (w/back x.)  N                   3*N
*           Scaling (w/o back)   N - (JE-1)          3*N - 3*(JE-1)
*
            IF( .NOT.ILCPLX ) THEN
               OPST = OPST + DBLE( 2*( N-JE )*( N+1-JE )+13*( N-JE )+8*
     $                IN2BY2+12 )
               IF( ILBACK ) THEN
                  OPST = OPST + DBLE( 2*N*( N+1-JE ) )
               ELSE
                  OPST = OPST + DBLE( N+1-JE )
               END IF
            ELSE
               OPST = OPST + DBLE( 32*( N-1-JE )+4*( N-JE )*( N+1-JE )+
     $                24*IN2BY2+71 )
               IF( ILBACK ) THEN
                  OPST = OPST + DBLE( 4*N*( N+1-JE )+N )
               ELSE
                  OPST = OPST + DBLE( 3*( N+1-JE ) )
               END IF
            END IF
            OPS = OPS + OPST
*
*           -------------------- End Timing Code -----------------------
*
  220    CONTINUE
      END IF
*
*     Right eigenvectors
*
      IF( COMPR ) THEN
         IEIG = IM + 1
*
*        Main loop over eigenvalues
*
         ILCPLX = .FALSE.
         DO 500 JE = N, 1, -1
*
*           Skip this iteration if (a) HOWMNY='S' and SELECT=.FALSE., or
*           (b) this would be the second of a complex pair.
*           Check for complex eigenvalue, so as to be sure of which
*           entry(-ies) of SELECT to look at -- if complex, SELECT(JE)
*           or SELECT(JE-1).
*           If this is a complex pair, the 2-by-2 diagonal block
*           corresponding to the eigenvalue is in rows/columns JE-1:JE
*
            IF( ILCPLX ) THEN
               ILCPLX = .FALSE.
               GO TO 500
            END IF
            NW = 1
            IF( JE.GT.1 ) THEN
               IF( A( JE, JE-1 ).NE.ZERO ) THEN
                  ILCPLX = .TRUE.
                  NW = 2
               END IF
            END IF
            IF( ILALL ) THEN
               ILCOMP = .TRUE.
            ELSE IF( ILCPLX ) THEN
               ILCOMP = SELECT( JE ) .OR. SELECT( JE-1 )
            ELSE
               ILCOMP = SELECT( JE )
            END IF
            IF( .NOT.ILCOMP )
     $         GO TO 500
*
*           Decide if (a) singular pencil, (b) real eigenvalue, or
*           (c) complex eigenvalue.
*
            IF( .NOT.ILCPLX ) THEN
               IF( ABS( A( JE, JE ) ).LE.SAFMIN .AND.
     $             ABS( B( JE, JE ) ).LE.SAFMIN ) THEN
*
*                 Singular matrix pencil -- returns unit eigenvector
*
                  IEIG = IEIG - 1
                  DO 230 JR = 1, N
                     VR( JR, IEIG ) = ZERO
  230             CONTINUE
                  VR( IEIG, IEIG ) = ONE
                  GO TO 500
               END IF
            END IF
*
*           Clear vector
*
            DO 250 JW = 0, NW - 1
               DO 240 JR = 1, N
                  WORK( ( JW+2 )*N+JR ) = ZERO
  240          CONTINUE
  250       CONTINUE
*
*           Compute coefficients in  ( a A - b B ) x = 0
*              a  is  ACOEF
*              b  is  BCOEFR + i*BCOEFI
*
            IF( .NOT.ILCPLX ) THEN
*
*              Real eigenvalue
*
               TEMP = ONE / MAX( ABS( A( JE, JE ) )*ASCALE,
     $                ABS( B( JE, JE ) )*BSCALE, SAFMIN )
               SALFAR = ( TEMP*A( JE, JE ) )*ASCALE
               SBETA = ( TEMP*B( JE, JE ) )*BSCALE
               ACOEF = SBETA*ASCALE
               BCOEFR = SALFAR*BSCALE
               BCOEFI = ZERO
*
*              Scale to avoid underflow
*
               SCALE = ONE
               LSA = ABS( SBETA ).GE.SAFMIN .AND. ABS( ACOEF ).LT.SMALL
               LSB = ABS( SALFAR ).GE.SAFMIN .AND. ABS( BCOEFR ).LT.
     $               SMALL
               IF( LSA )
     $            SCALE = ( SMALL / ABS( SBETA ) )*MIN( ANORM, BIG )
               IF( LSB )
     $            SCALE = MAX( SCALE, ( SMALL / ABS( SALFAR ) )*
     $                    MIN( BNORM, BIG ) )
               IF( LSA .OR. LSB ) THEN
                  SCALE = MIN( SCALE, ONE /
     $                    ( SAFMIN*MAX( ONE, ABS( ACOEF ),
     $                    ABS( BCOEFR ) ) ) )
                  IF( LSA ) THEN
                     ACOEF = ASCALE*( SCALE*SBETA )
                  ELSE
                     ACOEF = SCALE*ACOEF
                  END IF
                  IF( LSB ) THEN
                     BCOEFR = BSCALE*( SCALE*SALFAR )
                  ELSE
                     BCOEFR = SCALE*BCOEFR
                  END IF
               END IF
               ACOEFA = ABS( ACOEF )
               BCOEFA = ABS( BCOEFR )
*
*              First component is 1
*
               WORK( 2*N+JE ) = ONE
               XMAX = ONE
*
*              Compute contribution from column JE of A and B to sum
*              (See "Further Details", above.)
*
               DO 260 JR = 1, JE - 1
                  WORK( 2*N+JR ) = BCOEFR*B( JR, JE ) -
     $                             ACOEF*A( JR, JE )
  260          CONTINUE
            ELSE
*
*              Complex eigenvalue
*
               CALL DLAG2( A( JE-1, JE-1 ), LDA, B( JE-1, JE-1 ), LDB,
     $                     SAFMIN*SAFETY, ACOEF, TEMP, BCOEFR, TEMP2,
     $                     BCOEFI )
               IF( BCOEFI.EQ.ZERO ) THEN
                  INFO = JE - 1
                  RETURN
               END IF
*
*              Scale to avoid over/underflow
*
               ACOEFA = ABS( ACOEF )
               BCOEFA = ABS( BCOEFR ) + ABS( BCOEFI )
               SCALE = ONE
               IF( ACOEFA*ULP.LT.SAFMIN .AND. ACOEFA.GE.SAFMIN )
     $            SCALE = ( SAFMIN / ULP ) / ACOEFA
               IF( BCOEFA*ULP.LT.SAFMIN .AND. BCOEFA.GE.SAFMIN )
     $            SCALE = MAX( SCALE, ( SAFMIN / ULP ) / BCOEFA )
               IF( SAFMIN*ACOEFA.GT.ASCALE )
     $            SCALE = ASCALE / ( SAFMIN*ACOEFA )
               IF( SAFMIN*BCOEFA.GT.BSCALE )
     $            SCALE = MIN( SCALE, BSCALE / ( SAFMIN*BCOEFA ) )
               IF( SCALE.NE.ONE ) THEN
                  ACOEF = SCALE*ACOEF
                  ACOEFA = ABS( ACOEF )
                  BCOEFR = SCALE*BCOEFR
                  BCOEFI = SCALE*BCOEFI
                  BCOEFA = ABS( BCOEFR ) + ABS( BCOEFI )
               END IF
*
*              Compute first two components of eigenvector
*              and contribution to sums
*
               TEMP = ACOEF*A( JE, JE-1 )
               TEMP2R = ACOEF*A( JE, JE ) - BCOEFR*B( JE, JE )
               TEMP2I = -BCOEFI*B( JE, JE )
               IF( ABS( TEMP ).GE.ABS( TEMP2R )+ABS( TEMP2I ) ) THEN
                  WORK( 2*N+JE ) = ONE
                  WORK( 3*N+JE ) = ZERO
                  WORK( 2*N+JE-1 ) = -TEMP2R / TEMP
                  WORK( 3*N+JE-1 ) = -TEMP2I / TEMP
               ELSE
                  WORK( 2*N+JE-1 ) = ONE
                  WORK( 3*N+JE-1 ) = ZERO
                  TEMP = ACOEF*A( JE-1, JE )
                  WORK( 2*N+JE ) = ( BCOEFR*B( JE-1, JE-1 )-ACOEF*
     $                             A( JE-1, JE-1 ) ) / TEMP
                  WORK( 3*N+JE ) = BCOEFI*B( JE-1, JE-1 ) / TEMP
               END IF
*
               XMAX = MAX( ABS( WORK( 2*N+JE ) )+ABS( WORK( 3*N+JE ) ),
     $                ABS( WORK( 2*N+JE-1 ) )+ABS( WORK( 3*N+JE-1 ) ) )
*
*              Compute contribution from columns JE and JE-1
*              of A and B to the sums.
*
               CREALA = ACOEF*WORK( 2*N+JE-1 )
               CIMAGA = ACOEF*WORK( 3*N+JE-1 )
               CREALB = BCOEFR*WORK( 2*N+JE-1 ) -
     $                  BCOEFI*WORK( 3*N+JE-1 )
               CIMAGB = BCOEFI*WORK( 2*N+JE-1 ) +
     $                  BCOEFR*WORK( 3*N+JE-1 )
               CRE2A = ACOEF*WORK( 2*N+JE )
               CIM2A = ACOEF*WORK( 3*N+JE )
               CRE2B = BCOEFR*WORK( 2*N+JE ) - BCOEFI*WORK( 3*N+JE )
               CIM2B = BCOEFI*WORK( 2*N+JE ) + BCOEFR*WORK( 3*N+JE )
               DO 270 JR = 1, JE - 2
                  WORK( 2*N+JR ) = -CREALA*A( JR, JE-1 ) +
     $                             CREALB*B( JR, JE-1 ) -
     $                             CRE2A*A( JR, JE ) + CRE2B*B( JR, JE )
                  WORK( 3*N+JR ) = -CIMAGA*A( JR, JE-1 ) +
     $                             CIMAGB*B( JR, JE-1 ) -
     $                             CIM2A*A( JR, JE ) + CIM2B*B( JR, JE )
  270          CONTINUE
            END IF
*
            DMIN = MAX( ULP*ACOEFA*ANORM, ULP*BCOEFA*BNORM, SAFMIN )
*
*           Columnwise triangular solve of  (a A - b B)  x = 0
*
            IL2BY2 = .FALSE.
*           ------------------- Begin Timing Code ----------------------
            OPST = ZERO
            IN2BY2 = 0
*           -------------------- End Timing Code -----------------------
            DO 370 J = JE - NW, 1, -1
*              ------------------- Begin Timing Code -------------------
               OPSSCA = DBLE( NW*JE+1 )
*              -------------------- End Timing Code --------------------
*
*              If a 2-by-2 block, is in position j-1:j, wait until
*              next iteration to process it (when it will be j:j+1)
*
               IF( .NOT.IL2BY2 .AND. J.GT.1 ) THEN
                  IF( A( J, J-1 ).NE.ZERO ) THEN
                     IL2BY2 = .TRUE.
*                    -------------- Begin Timing Code -----------------
                     IN2BY2 = IN2BY2 + 1
*                    --------------- End Timing Code -------------------
                     GO TO 370
                  END IF
               END IF
               BDIAG( 1 ) = B( J, J )
               IF( IL2BY2 ) THEN
                  NA = 2
                  BDIAG( 2 ) = B( J+1, J+1 )
               ELSE
                  NA = 1
               END IF
*
*              Compute x(j) (and x(j+1), if 2-by-2 block)
*
               CALL DLALN2( .FALSE., NA, NW, DMIN, ACOEF, A( J, J ),
     $                      LDA, BDIAG( 1 ), BDIAG( 2 ), WORK( 2*N+J ),
     $                      N, BCOEFR, BCOEFI, SUM, 2, SCALE, TEMP,
     $                      IINFO )
               IF( SCALE.LT.ONE ) THEN
*
                  DO 290 JW = 0, NW - 1
                     DO 280 JR = 1, JE
                        WORK( ( JW+2 )*N+JR ) = SCALE*
     $                     WORK( ( JW+2 )*N+JR )
  280                CONTINUE
  290             CONTINUE
               END IF
               XMAX = MAX( SCALE*XMAX, TEMP )
*              ------------------ Begin Timing Code -----------------
               OPST = OPST + OPSSCA
*              ------------------- End Timing Code ------------------
*
               DO 310 JW = 1, NW
                  DO 300 JA = 1, NA
                     WORK( ( JW+1 )*N+J+JA-1 ) = SUM( JA, JW )
  300             CONTINUE
  310          CONTINUE
*
*              w = w + x(j)*(a A(*,j) - b B(*,j) ) with scaling
*
               IF( J.GT.1 ) THEN
*
*                 Check whether scaling is necessary for sum.
*
                  XSCALE = ONE / MAX( ONE, XMAX )
                  TEMP = ACOEFA*WORK( J ) + BCOEFA*WORK( N+J )
                  IF( IL2BY2 )
     $               TEMP = MAX( TEMP, ACOEFA*WORK( J+1 )+BCOEFA*
     $                      WORK( N+J+1 ) )
                  TEMP = MAX( TEMP, ACOEFA, BCOEFA )
                  IF( TEMP.GT.BIGNUM*XSCALE ) THEN
*
                     DO 330 JW = 0, NW - 1
                        DO 320 JR = 1, JE
                           WORK( ( JW+2 )*N+JR ) = XSCALE*
     $                        WORK( ( JW+2 )*N+JR )
  320                   CONTINUE
  330                CONTINUE
                     XMAX = XMAX*XSCALE
*                    ----------------- Begin Timing Code ---------------
                     OPST = OPST + OPSSCA
*                    ------------------ End Timing Code ----------------
                  END IF
*
*                 Compute the contributions of the off-diagonals of
*                 column j (and j+1, if 2-by-2 block) of A and B to the
*                 sums.
*
*
                  DO 360 JA = 1, NA
                     IF( ILCPLX ) THEN
                        CREALA = ACOEF*WORK( 2*N+J+JA-1 )
                        CIMAGA = ACOEF*WORK( 3*N+J+JA-1 )
                        CREALB = BCOEFR*WORK( 2*N+J+JA-1 ) -
     $                           BCOEFI*WORK( 3*N+J+JA-1 )
                        CIMAGB = BCOEFI*WORK( 2*N+J+JA-1 ) +
     $                           BCOEFR*WORK( 3*N+J+JA-1 )
                        DO 340 JR = 1, J - 1
                           WORK( 2*N+JR ) = WORK( 2*N+JR ) -
     $                                      CREALA*A( JR, J+JA-1 ) +
     $                                      CREALB*B( JR, J+JA-1 )
                           WORK( 3*N+JR ) = WORK( 3*N+JR ) -
     $                                      CIMAGA*A( JR, J+JA-1 ) +
     $                                      CIMAGB*B( JR, J+JA-1 )
  340                   CONTINUE
                     ELSE
                        CREALA = ACOEF*WORK( 2*N+J+JA-1 )
                        CREALB = BCOEFR*WORK( 2*N+J+JA-1 )
                        DO 350 JR = 1, J - 1
                           WORK( 2*N+JR ) = WORK( 2*N+JR ) -
     $                                      CREALA*A( JR, J+JA-1 ) +
     $                                      CREALB*B( JR, J+JA-1 )
  350                   CONTINUE
                     END IF
  360             CONTINUE
               END IF
*
               IL2BY2 = .FALSE.
  370       CONTINUE
*
*           Copy eigenvector to VR, back transforming if
*           HOWMNY='B'.
*
            IEIG = IEIG - NW
            IF( ILBACK ) THEN
*
               DO 410 JW = 0, NW - 1
                  DO 380 JR = 1, N
                     WORK( ( JW+4 )*N+JR ) = WORK( ( JW+2 )*N+1 )*
     $                                       VR( JR, 1 )
  380             CONTINUE
*
*                 A series of compiler directives to defeat
*                 vectorization for the next loop
*
*
                  DO 400 JC = 2, JE
                     DO 390 JR = 1, N
                        WORK( ( JW+4 )*N+JR ) = WORK( ( JW+4 )*N+JR ) +
     $                     WORK( ( JW+2 )*N+JC )*VR( JR, JC )
  390                CONTINUE
  400             CONTINUE
  410          CONTINUE
*
               DO 430 JW = 0, NW - 1
                  DO 420 JR = 1, N
                     VR( JR, IEIG+JW ) = WORK( ( JW+4 )*N+JR )
  420             CONTINUE
  430          CONTINUE
*
               IEND = N
            ELSE
               DO 450 JW = 0, NW - 1
                  DO 440 JR = 1, N
                     VR( JR, IEIG+JW ) = WORK( ( JW+2 )*N+JR )
  440             CONTINUE
  450          CONTINUE
*
               IEND = JE
            END IF
*
*           Scale eigenvector
*
            XMAX = ZERO
            IF( ILCPLX ) THEN
               DO 460 J = 1, IEND
                  XMAX = MAX( XMAX, ABS( VR( J, IEIG ) )+
     $                   ABS( VR( J, IEIG+1 ) ) )
  460          CONTINUE
            ELSE
               DO 470 J = 1, IEND
                  XMAX = MAX( XMAX, ABS( VR( J, IEIG ) ) )
  470          CONTINUE
            END IF
*
            IF( XMAX.GT.SAFMIN ) THEN
               XSCALE = ONE / XMAX
               DO 490 JW = 0, NW - 1
                  DO 480 JR = 1, IEND
                     VR( JR, IEIG+JW ) = XSCALE*VR( JR, IEIG+JW )
  480             CONTINUE
  490          CONTINUE
            END IF
*
*           ------------------- Begin Timing Code ----------------------
*           Opcounts for each eigenvector
*
*                                Real                Complex
*           Initialization       8--16 + 3*(JE-1)    71--87+16+14*(JE-2)
*
*           Solve (per iter)     NA*(5 + 7*(NA-1))   NA*(17 + 17*(NA-1))
*                                + scaling           + scaling
*           column add (per iter)
*                                2 + 5*NA            2 + 11*NA
*                                + 4*NA*(J-1)        + 8*NA*(J-1)
*                                + scaling           + scaling
*           iteration:           J=JE-1,...,1        J=JE-2,...,1
*
*           Back xform           2*N*JE - N          4*N*JE - 2*N
*           Scaling (w/back x.)  N                   3*N
*           Scaling (w/o back)   JE                  3*JE
*
            IF( .NOT.ILCPLX ) THEN
               OPST = OPST + DBLE( ( 2*JE+11 )*( JE-1 )+12+8*IN2BY2 )
               IF( ILBACK ) THEN
                  OPST = OPST + DBLE( 2*N*JE )
               ELSE
                  OPST = OPST + DBLE( JE )
               END IF
            ELSE
               OPST = OPST + DBLE( ( 4*JE+32 )*( JE-2 )+95+24*IN2BY2 )
               IF( ILBACK ) THEN
                  OPST = OPST + DBLE( 4*N*JE+N )
               ELSE
                  OPST = OPST + DBLE( 3*JE )
               END IF
            END IF
            OPS = OPS + OPST
*
*           -------------------- End Timing Code -----------------------
*
  500    CONTINUE
      END IF
*
      RETURN
*
*     End of DTGEVC
*
      END
      SUBROUTINE DTREVC( SIDE, HOWMNY, SELECT, N, T, LDT, VL, LDVL, VR,
     $                   LDVR, MM, M, WORK, INFO )
*
*  -- LAPACK routine (instrumented to count operations, version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     June 30, 1999
*
*     .. Scalar Arguments ..
      CHARACTER          HOWMNY, SIDE
      INTEGER            INFO, LDT, LDVL, LDVR, M, MM, N
*     ..
*     .. Array Arguments ..
      LOGICAL            SELECT( * )
      DOUBLE PRECISION   T( LDT, * ), VL( LDVL, * ), VR( LDVR, * ),
     $                   WORK( * )
*     ..
*     Common block to return operation count.
*     OPS is only incremented, OPST is used to accumulate small
*     contributions to OPS to avoid roundoff error
*     .. Common blocks ..
      COMMON             / LATIME / OPS, ITCNT
*     ..
*     .. Scalars in Common ..
      DOUBLE PRECISION   ITCNT, OPS
*     ..
*
*  Purpose
*  =======
*
*  DTREVC computes some or all of the right and/or left eigenvectors of
*  a real upper quasi-triangular matrix T.
*
*  The right eigenvector x and the left eigenvector y of T corresponding
*  to an eigenvalue w are defined by:
*
*               T*x = w*x,     y'*T = w*y'
*
*  where y' denotes the conjugate transpose of the vector y.
*
*  If all eigenvectors are requested, the routine may either return the
*  matrices X and/or Y of right or left eigenvectors of T, or the
*  products Q*X and/or Q*Y, where Q is an input orthogonal
*  matrix. If T was obtained from the real-Schur factorization of an
*  original matrix A = Q*T*Q', then Q*X and Q*Y are the matrices of
*  right or left eigenvectors of A.
*
*  T must be in Schur canonical form (as returned by DHSEQR), that is,
*  block upper triangular with 1-by-1 and 2-by-2 diagonal blocks; each
*  2-by-2 diagonal block has its diagonal elements equal and its
*  off-diagonal elements of opposite sign.  Corresponding to each 2-by-2
*  diagonal block is a complex conjugate pair of eigenvalues and
*  eigenvectors; only one eigenvector of the pair is computed, namely
*  the one corresponding to the eigenvalue with positive imaginary part.
*
*  Arguments
*  =========
*
*  SIDE    (input) CHARACTER*1
*          = 'R':  compute right eigenvectors only;
*          = 'L':  compute left eigenvectors only;
*          = 'B':  compute both right and left eigenvectors.
*
*  HOWMNY  (input) CHARACTER*1
*          = 'A':  compute all right and/or left eigenvectors;
*          = 'B':  compute all right and/or left eigenvectors,
*                  and backtransform them using the input matrices
*                  supplied in VR and/or VL;
*          = 'S':  compute selected right and/or left eigenvectors,
*                  specified by the logical array SELECT.
*
*  SELECT  (input/output) LOGICAL array, dimension (N)
*          If HOWMNY = 'S', SELECT specifies the eigenvectors to be
*          computed.
*          If HOWMNY = 'A' or 'B', SELECT is not referenced.
*          To select the real eigenvector corresponding to a real
*          eigenvalue w(j), SELECT(j) must be set to .TRUE..  To select
*          the complex eigenvector corresponding to a complex conjugate
*          pair w(j) and w(j+1), either SELECT(j) or SELECT(j+1) must be
*          set to .TRUE.; then on exit SELECT(j) is .TRUE. and
*          SELECT(j+1) is .FALSE..
*
*  N       (input) INTEGER
*          The order of the matrix T. N >= 0.
*
*  T       (input) DOUBLE PRECISION array, dimension (LDT,N)
*          The upper quasi-triangular matrix T in Schur canonical form.
*
*  LDT     (input) INTEGER
*          The leading dimension of the array T. LDT >= max(1,N).
*
*  VL      (input/output) DOUBLE PRECISION array, dimension (LDVL,MM)
*          On entry, if SIDE = 'L' or 'B' and HOWMNY = 'B', VL must
*          contain an N-by-N matrix Q (usually the orthogonal matrix Q
*          of Schur vectors returned by DHSEQR).
*          On exit, if SIDE = 'L' or 'B', VL contains:
*          if HOWMNY = 'A', the matrix Y of left eigenvectors of T;
*                           VL has the same quasi-lower triangular form
*                           as T'. If T(i,i) is a real eigenvalue, then
*                           the i-th column VL(i) of VL  is its
*                           corresponding eigenvector. If T(i:i+1,i:i+1)
*                           is a 2-by-2 block whose eigenvalues are
*                           complex-conjugate eigenvalues of T, then
*                           VL(i)+sqrt(-1)*VL(i+1) is the complex
*                           eigenvector corresponding to the eigenvalue
*                           with positive real part.
*          if HOWMNY = 'B', the matrix Q*Y;
*          if HOWMNY = 'S', the left eigenvectors of T specified by
*                           SELECT, stored consecutively in the columns
*                           of VL, in the same order as their
*                           eigenvalues.
*          A complex eigenvector corresponding to a complex eigenvalue
*          is stored in two consecutive columns, the first holding the
*          real part, and the second the imaginary part.
*          If SIDE = 'R', VL is not referenced.
*
*  LDVL    (input) INTEGER
*          The leading dimension of the array VL.  LDVL >= max(1,N) if
*          SIDE = 'L' or 'B'; LDVL >= 1 otherwise.
*
*  VR      (input/output) DOUBLE PRECISION array, dimension (LDVR,MM)
*          On entry, if SIDE = 'R' or 'B' and HOWMNY = 'B', VR must
*          contain an N-by-N matrix Q (usually the orthogonal matrix Q
*          of Schur vectors returned by DHSEQR).
*          On exit, if SIDE = 'R' or 'B', VR contains:
*          if HOWMNY = 'A', the matrix X of right eigenvectors of T;
*                           VR has the same quasi-upper triangular form
*                           as T. If T(i,i) is a real eigenvalue, then
*                           the i-th column VR(i) of VR  is its
*                           corresponding eigenvector. If T(i:i+1,i:i+1)
*                           is a 2-by-2 block whose eigenvalues are
*                           complex-conjugate eigenvalues of T, then
*                           VR(i)+sqrt(-1)*VR(i+1) is the complex
*                           eigenvector corresponding to the eigenvalue
*                           with positive real part.
*          if HOWMNY = 'B', the matrix Q*X;
*          if HOWMNY = 'S', the right eigenvectors of T specified by
*                           SELECT, stored consecutively in the columns
*                           of VR, in the same order as their
*                           eigenvalues.
*          A complex eigenvector corresponding to a complex eigenvalue
*          is stored in two consecutive columns, the first holding the
*          real part and the second the imaginary part.
*          If SIDE = 'L', VR is not referenced.
*
*  LDVR    (input) INTEGER
*          The leading dimension of the array VR.  LDVR >= max(1,N) if
*          SIDE = 'R' or 'B'; LDVR >= 1 otherwise.
*
*  MM      (input) INTEGER
*          The number of columns in the arrays VL and/or VR. MM >= M.
*
*  M       (output) INTEGER
*          The number of columns in the arrays VL and/or VR actually
*          used to store the eigenvectors.
*          If HOWMNY = 'A' or 'B', M is set to N.
*          Each selected real eigenvector occupies one column and each
*          selected complex eigenvector occupies two columns.
*
*  WORK    (workspace) DOUBLE PRECISION array, dimension (3*N)
*
*  INFO    (output) INTEGER
*          = 0:  successful exit
*          < 0:  if INFO = -i, the i-th argument had an illegal value
*
*  Further Details
*  ===============
*
*  The algorithm used in this program is basically backward (forward)
*  substitution, with scaling to make the the code robust against
*  possible overflow.
*
*  Each eigenvector is normalized so that the element of largest
*  magnitude has magnitude 1; here the magnitude of a complex number
*  (x,y) is taken to be |x| + |y|.
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE
      PARAMETER          ( ZERO = 0.0D+0, ONE = 1.0D+0 )
*     ..
*     .. Local Scalars ..
      LOGICAL            ALLV, BOTHV, LEFTV, OVER, PAIR, RIGHTV, SOMEV
      INTEGER            I, IERR, II, IP, IS, J, J1, J2, JNXT, K, KI, N2
      DOUBLE PRECISION   BETA, BIGNUM, EMAX, OPST, OVFL, REC, REMAX,
     $                   SCALE, SMIN, SMLNUM, ULP, UNFL, VCRIT, VMAX,
     $                   WI, WR, XNORM
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      INTEGER            IDAMAX
      DOUBLE PRECISION   DDOT, DLAMCH
      EXTERNAL           LSAME, IDAMAX, DDOT, DLAMCH
*     ..
*     .. External Subroutines ..
      EXTERNAL           DAXPY, DCOPY, DGEMV, DLABAD, DLALN2, DSCAL,
     $                   XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, MAX, SQRT
*     ..
*     .. Local Arrays ..
      DOUBLE PRECISION   X( 2, 2 )
*     ..
*     .. Executable Statements ..
*
*     Decode and test the input parameters
*
      BOTHV = LSAME( SIDE, 'B' )
      RIGHTV = LSAME( SIDE, 'R' ) .OR. BOTHV
      LEFTV = LSAME( SIDE, 'L' ) .OR. BOTHV
*
      ALLV = LSAME( HOWMNY, 'A' )
      OVER = LSAME( HOWMNY, 'B' )
      SOMEV = LSAME( HOWMNY, 'S' )
*
      INFO = 0
      IF( .NOT.RIGHTV .AND. .NOT.LEFTV ) THEN
         INFO = -1
      ELSE IF( .NOT.ALLV .AND. .NOT.OVER .AND. .NOT.SOMEV ) THEN
         INFO = -2
      ELSE IF( N.LT.0 ) THEN
         INFO = -4
      ELSE IF( LDT.LT.MAX( 1, N ) ) THEN
         INFO = -6
      ELSE IF( LDVL.LT.1 .OR. ( LEFTV .AND. LDVL.LT.N ) ) THEN
         INFO = -8
      ELSE IF( LDVR.LT.1 .OR. ( RIGHTV .AND. LDVR.LT.N ) ) THEN
         INFO = -10
      ELSE
*
*        Set M to the number of columns required to store the selected
*        eigenvectors, standardize the array SELECT if necessary, and
*        test MM.
*
         IF( SOMEV ) THEN
            M = 0
            PAIR = .FALSE.
            DO 10 J = 1, N
               IF( PAIR ) THEN
                  PAIR = .FALSE.
                  SELECT( J ) = .FALSE.
               ELSE
                  IF( J.LT.N ) THEN
                     IF( T( J+1, J ).EQ.ZERO ) THEN
                        IF( SELECT( J ) )
     $                     M = M + 1
                     ELSE
                        PAIR = .TRUE.
                        IF( SELECT( J ) .OR. SELECT( J+1 ) ) THEN
                           SELECT( J ) = .TRUE.
                           M = M + 2
                        END IF
                     END IF
                  ELSE
                     IF( SELECT( N ) )
     $                  M = M + 1
                  END IF
               END IF
   10       CONTINUE
         ELSE
            M = N
         END IF
*
         IF( MM.LT.M ) THEN
            INFO = -11
         END IF
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DTREVC', -INFO )
         RETURN
      END IF
*
*     Quick return if possible.
*
      IF( N.EQ.0 )
     $   RETURN
***
*     Initialize
      OPST = 0
***
*
*     Set the constants to control overflow.
*
      UNFL = DLAMCH( 'Safe minimum' )
      OVFL = ONE / UNFL
      CALL DLABAD( UNFL, OVFL )
      ULP = DLAMCH( 'Precision' )
      SMLNUM = UNFL*( N / ULP )
      BIGNUM = ( ONE-ULP ) / SMLNUM
*
*     Compute 1-norm of each column of strictly upper triangular
*     part of T to control overflow in triangular solver.
*
      WORK( 1 ) = ZERO
      DO 30 J = 2, N
         WORK( J ) = ZERO
         DO 20 I = 1, J - 1
            WORK( J ) = WORK( J ) + ABS( T( I, J ) )
   20    CONTINUE
   30 CONTINUE
***
      OPS = OPS + N*( N-1 ) / 2
***
*
*     Index IP is used to specify the real or complex eigenvalue:
*       IP = 0, real eigenvalue,
*            1, first of conjugate complex pair: (wr,wi)
*           -1, second of conjugate complex pair: (wr,wi)
*
      N2 = 2*N
*
      IF( RIGHTV ) THEN
*
*        Compute right eigenvectors.
*
         IP = 0
         IS = M
         DO 140 KI = N, 1, -1
*
            IF( IP.EQ.1 )
     $         GO TO 130
            IF( KI.EQ.1 )
     $         GO TO 40
            IF( T( KI, KI-1 ).EQ.ZERO )
     $         GO TO 40
            IP = -1
*
   40       CONTINUE
            IF( SOMEV ) THEN
               IF( IP.EQ.0 ) THEN
                  IF( .NOT.SELECT( KI ) )
     $               GO TO 130
               ELSE
                  IF( .NOT.SELECT( KI-1 ) )
     $               GO TO 130
               END IF
            END IF
*
*           Compute the KI-th eigenvalue (WR,WI).
*
            WR = T( KI, KI )
            WI = ZERO
            IF( IP.NE.0 )
     $         WI = SQRT( ABS( T( KI, KI-1 ) ) )*
     $              SQRT( ABS( T( KI-1, KI ) ) )
            SMIN = MAX( ULP*( ABS( WR )+ABS( WI ) ), SMLNUM )
*
            IF( IP.EQ.0 ) THEN
*
*              Real right eigenvector
*
               WORK( KI+N ) = ONE
*
*              Form right-hand side
*
               DO 50 K = 1, KI - 1
                  WORK( K+N ) = -T( K, KI )
   50          CONTINUE
*
*              Solve the upper quasi-triangular system:
*                 (T(1:KI-1,1:KI-1) - WR)*X = SCALE*WORK.
*
               JNXT = KI - 1
               DO 60 J = KI - 1, 1, -1
                  IF( J.GT.JNXT )
     $               GO TO 60
                  J1 = J
                  J2 = J
                  JNXT = J - 1
                  IF( J.GT.1 ) THEN
                     IF( T( J, J-1 ).NE.ZERO ) THEN
                        J1 = J - 1
                        JNXT = J - 2
                     END IF
                  END IF
*
                  IF( J1.EQ.J2 ) THEN
*
*                    1-by-1 diagonal block
*
                     CALL DLALN2( .FALSE., 1, 1, SMIN, ONE, T( J, J ),
     $                            LDT, ONE, ONE, WORK( J+N ), N, WR,
     $                            ZERO, X, 2, SCALE, XNORM, IERR )
*
*                    Scale X(1,1) to avoid overflow when updating
*                    the right-hand side.
*
                     IF( XNORM.GT.ONE ) THEN
                        IF( WORK( J ).GT.BIGNUM / XNORM ) THEN
                           X( 1, 1 ) = X( 1, 1 ) / XNORM
                           SCALE = SCALE / XNORM
                        END IF
                     END IF
*
*                    Scale if necessary
*
                     IF( SCALE.NE.ONE )
     $                  CALL DSCAL( KI, SCALE, WORK( 1+N ), 1 )
                     WORK( J+N ) = X( 1, 1 )
*
*                    Update right-hand side
*
                     CALL DAXPY( J-1, -X( 1, 1 ), T( 1, J ), 1,
     $                           WORK( 1+N ), 1 )
***
*                    Increment op count, ignoring the possible scaling
                     OPST = OPST + ( 2*( J-1 )+6 )
***
*
                  ELSE
*
*                    2-by-2 diagonal block
*
                     CALL DLALN2( .FALSE., 2, 1, SMIN, ONE,
     $                            T( J-1, J-1 ), LDT, ONE, ONE,
     $                            WORK( J-1+N ), N, WR, ZERO, X, 2,
     $                            SCALE, XNORM, IERR )
*
*                    Scale X(1,1) and X(2,1) to avoid overflow when
*                    updating the right-hand side.
*
                     IF( XNORM.GT.ONE ) THEN
                        BETA = MAX( WORK( J-1 ), WORK( J ) )
                        IF( BETA.GT.BIGNUM / XNORM ) THEN
                           X( 1, 1 ) = X( 1, 1 ) / XNORM
                           X( 2, 1 ) = X( 2, 1 ) / XNORM
                           SCALE = SCALE / XNORM
                        END IF
                     END IF
*
*                    Scale if necessary
*
                     IF( SCALE.NE.ONE )
     $                  CALL DSCAL( KI, SCALE, WORK( 1+N ), 1 )
                     WORK( J-1+N ) = X( 1, 1 )
                     WORK( J+N ) = X( 2, 1 )
*
*                    Update right-hand side
*
                     CALL DAXPY( J-2, -X( 1, 1 ), T( 1, J-1 ), 1,
     $                           WORK( 1+N ), 1 )
                     CALL DAXPY( J-2, -X( 2, 1 ), T( 1, J ), 1,
     $                           WORK( 1+N ), 1 )
***
*                    Increment op count, ignoring the possible scaling
                     OPST = OPST + ( 4*( J-2 )+24 )
***
                  END IF
   60          CONTINUE
*
*              Copy the vector x or Q*x to VR and normalize.
*
               IF( .NOT.OVER ) THEN
                  CALL DCOPY( KI, WORK( 1+N ), 1, VR( 1, IS ), 1 )
*
                  II = IDAMAX( KI, VR( 1, IS ), 1 )
                  REMAX = ONE / ABS( VR( II, IS ) )
                  CALL DSCAL( KI, REMAX, VR( 1, IS ), 1 )
***
                  OPST = OPST + ( 2*KI+1 )
***
*
                  DO 70 K = KI + 1, N
                     VR( K, IS ) = ZERO
   70             CONTINUE
               ELSE
                  IF( KI.GT.1 )
     $               CALL DGEMV( 'N', N, KI-1, ONE, VR, LDVR,
     $                           WORK( 1+N ), 1, WORK( KI+N ),
     $                           VR( 1, KI ), 1 )
*
                  II = IDAMAX( N, VR( 1, KI ), 1 )
                  REMAX = ONE / ABS( VR( II, KI ) )
                  CALL DSCAL( N, REMAX, VR( 1, KI ), 1 )
***
                  OPS = OPS + ( 2*N*KI+1 )
***
               END IF
*
            ELSE
*
*              Complex right eigenvector.
*
*              Initial solve
*                [ (T(KI-1,KI-1) T(KI-1,KI) ) - (WR + I* WI)]*X = 0.
*                [ (T(KI,KI-1)   T(KI,KI)   )               ]
*
               IF( ABS( T( KI-1, KI ) ).GE.ABS( T( KI, KI-1 ) ) ) THEN
                  WORK( KI-1+N ) = ONE
                  WORK( KI+N2 ) = WI / T( KI-1, KI )
               ELSE
                  WORK( KI-1+N ) = -WI / T( KI, KI-1 )
                  WORK( KI+N2 ) = ONE
               END IF
               WORK( KI+N ) = ZERO
               WORK( KI-1+N2 ) = ZERO
*
*              Form right-hand side
*
               DO 80 K = 1, KI - 2
                  WORK( K+N ) = -WORK( KI-1+N )*T( K, KI-1 )
                  WORK( K+N2 ) = -WORK( KI+N2 )*T( K, KI )
   80          CONTINUE
***
               OPST = OPST + 2*( KI-2 )
***
*
*              Solve upper quasi-triangular system:
*              (T(1:KI-2,1:KI-2) - (WR+i*WI))*X = SCALE*(WORK+i*WORK2)
*
               JNXT = KI - 2
               DO 90 J = KI - 2, 1, -1
                  IF( J.GT.JNXT )
     $               GO TO 90
                  J1 = J
                  J2 = J
                  JNXT = J - 1
                  IF( J.GT.1 ) THEN
                     IF( T( J, J-1 ).NE.ZERO ) THEN
                        J1 = J - 1
                        JNXT = J - 2
                     END IF
                  END IF
*
                  IF( J1.EQ.J2 ) THEN
*
*                    1-by-1 diagonal block
*
                     CALL DLALN2( .FALSE., 1, 2, SMIN, ONE, T( J, J ),
     $                            LDT, ONE, ONE, WORK( J+N ), N, WR, WI,
     $                            X, 2, SCALE, XNORM, IERR )
*
*                    Scale X(1,1) and X(1,2) to avoid overflow when
*                    updating the right-hand side.
*
                     IF( XNORM.GT.ONE ) THEN
                        IF( WORK( J ).GT.BIGNUM / XNORM ) THEN
                           X( 1, 1 ) = X( 1, 1 ) / XNORM
                           X( 1, 2 ) = X( 1, 2 ) / XNORM
                           SCALE = SCALE / XNORM
                        END IF
                     END IF
*
*                    Scale if necessary
*
                     IF( SCALE.NE.ONE ) THEN
                        CALL DSCAL( KI, SCALE, WORK( 1+N ), 1 )
                        CALL DSCAL( KI, SCALE, WORK( 1+N2 ), 1 )
                     END IF
                     WORK( J+N ) = X( 1, 1 )
                     WORK( J+N2 ) = X( 1, 2 )
*
*                    Update the right-hand side
*
                     CALL DAXPY( J-1, -X( 1, 1 ), T( 1, J ), 1,
     $                           WORK( 1+N ), 1 )
                     CALL DAXPY( J-1, -X( 1, 2 ), T( 1, J ), 1,
     $                           WORK( 1+N2 ), 1 )
***
*                    Increment op count, ignoring the possible scaling
                     OPST = OPST + ( 4*( J-1 )+24 )
***
*
                  ELSE
*
*                    2-by-2 diagonal block
*
                     CALL DLALN2( .FALSE., 2, 2, SMIN, ONE,
     $                            T( J-1, J-1 ), LDT, ONE, ONE,
     $                            WORK( J-1+N ), N, WR, WI, X, 2, SCALE,
     $                            XNORM, IERR )
*
*                    Scale X to avoid overflow when updating
*                    the right-hand side.
*
                     IF( XNORM.GT.ONE ) THEN
                        BETA = MAX( WORK( J-1 ), WORK( J ) )
                        IF( BETA.GT.BIGNUM / XNORM ) THEN
                           REC = ONE / XNORM
                           X( 1, 1 ) = X( 1, 1 )*REC
                           X( 1, 2 ) = X( 1, 2 )*REC
                           X( 2, 1 ) = X( 2, 1 )*REC
                           X( 2, 2 ) = X( 2, 2 )*REC
                           SCALE = SCALE*REC
                        END IF
                     END IF
*
*                    Scale if necessary
*
                     IF( SCALE.NE.ONE ) THEN
                        CALL DSCAL( KI, SCALE, WORK( 1+N ), 1 )
                        CALL DSCAL( KI, SCALE, WORK( 1+N2 ), 1 )
                     END IF
                     WORK( J-1+N ) = X( 1, 1 )
                     WORK( J+N ) = X( 2, 1 )
                     WORK( J-1+N2 ) = X( 1, 2 )
                     WORK( J+N2 ) = X( 2, 2 )
*
*                    Update the right-hand side
*
                     CALL DAXPY( J-2, -X( 1, 1 ), T( 1, J-1 ), 1,
     $                           WORK( 1+N ), 1 )
                     CALL DAXPY( J-2, -X( 2, 1 ), T( 1, J ), 1,
     $                           WORK( 1+N ), 1 )
                     CALL DAXPY( J-2, -X( 1, 2 ), T( 1, J-1 ), 1,
     $                           WORK( 1+N2 ), 1 )
                     CALL DAXPY( J-2, -X( 2, 2 ), T( 1, J ), 1,
     $                           WORK( 1+N2 ), 1 )
***
*                    Increment op count, ignoring the possible scaling
                     OPST = OPST + ( 8*( J-2 )+64 )
***
                  END IF
   90          CONTINUE
*
*              Copy the vector x or Q*x to VR and normalize.
*
               IF( .NOT.OVER ) THEN
                  CALL DCOPY( KI, WORK( 1+N ), 1, VR( 1, IS-1 ), 1 )
                  CALL DCOPY( KI, WORK( 1+N2 ), 1, VR( 1, IS ), 1 )
*
                  EMAX = ZERO
                  DO 100 K = 1, KI
                     EMAX = MAX( EMAX, ABS( VR( K, IS-1 ) )+
     $                      ABS( VR( K, IS ) ) )
  100             CONTINUE
*
                  REMAX = ONE / EMAX
                  CALL DSCAL( KI, REMAX, VR( 1, IS-1 ), 1 )
                  CALL DSCAL( KI, REMAX, VR( 1, IS ), 1 )
***
                  OPST = OPST + ( 4*KI+1 )
***
*
                  DO 110 K = KI + 1, N
                     VR( K, IS-1 ) = ZERO
                     VR( K, IS ) = ZERO
  110             CONTINUE
*
               ELSE
*
                  IF( KI.GT.2 ) THEN
                     CALL DGEMV( 'N', N, KI-2, ONE, VR, LDVR,
     $                           WORK( 1+N ), 1, WORK( KI-1+N ),
     $                           VR( 1, KI-1 ), 1 )
                     CALL DGEMV( 'N', N, KI-2, ONE, VR, LDVR,
     $                           WORK( 1+N2 ), 1, WORK( KI+N2 ),
     $                           VR( 1, KI ), 1 )
                  ELSE
                     CALL DSCAL( N, WORK( KI-1+N ), VR( 1, KI-1 ), 1 )
                     CALL DSCAL( N, WORK( KI+N2 ), VR( 1, KI ), 1 )
                  END IF
*
                  EMAX = ZERO
                  DO 120 K = 1, N
                     EMAX = MAX( EMAX, ABS( VR( K, KI-1 ) )+
     $                      ABS( VR( K, KI ) ) )
  120             CONTINUE
                  REMAX = ONE / EMAX
                  CALL DSCAL( N, REMAX, VR( 1, KI-1 ), 1 )
                  CALL DSCAL( N, REMAX, VR( 1, KI ), 1 )
***
                  OPS = OPS + ( 4*N*( KI-2 )+6*N+1 )
***
               END IF
            END IF
*
            IS = IS - 1
            IF( IP.NE.0 )
     $         IS = IS - 1
  130       CONTINUE
            IF( IP.EQ.1 )
     $         IP = 0
            IF( IP.EQ.-1 )
     $         IP = 1
  140    CONTINUE
      END IF
*
      IF( LEFTV ) THEN
*
*        Compute left eigenvectors.
*
         IP = 0
         IS = 1
         DO 260 KI = 1, N
*
            IF( IP.EQ.-1 )
     $         GO TO 250
            IF( KI.EQ.N )
     $         GO TO 150
            IF( T( KI+1, KI ).EQ.ZERO )
     $         GO TO 150
            IP = 1
*
  150       CONTINUE
            IF( SOMEV ) THEN
               IF( .NOT.SELECT( KI ) )
     $            GO TO 250
            END IF
*
*           Compute the KI-th eigenvalue (WR,WI).
*
            WR = T( KI, KI )
            WI = ZERO
            IF( IP.NE.0 )
     $         WI = SQRT( ABS( T( KI, KI+1 ) ) )*
     $              SQRT( ABS( T( KI+1, KI ) ) )
            SMIN = MAX( ULP*( ABS( WR )+ABS( WI ) ), SMLNUM )
*
            IF( IP.EQ.0 ) THEN
*
*              Real left eigenvector.
*
               WORK( KI+N ) = ONE
*
*              Form right-hand side
*
               DO 160 K = KI + 1, N
                  WORK( K+N ) = -T( KI, K )
  160          CONTINUE
*
*              Solve the quasi-triangular system:
*                 (T(KI+1:N,KI+1:N) - WR)'*X = SCALE*WORK
*
               VMAX = ONE
               VCRIT = BIGNUM
*
               JNXT = KI + 1
               DO 170 J = KI + 1, N
                  IF( J.LT.JNXT )
     $               GO TO 170
                  J1 = J
                  J2 = J
                  JNXT = J + 1
                  IF( J.LT.N ) THEN
                     IF( T( J+1, J ).NE.ZERO ) THEN
                        J2 = J + 1
                        JNXT = J + 2
                     END IF
                  END IF
*
                  IF( J1.EQ.J2 ) THEN
*
*                    1-by-1 diagonal block
*
*                    Scale if necessary to avoid overflow when forming
*                    the right-hand side.
*
                     IF( WORK( J ).GT.VCRIT ) THEN
                        REC = ONE / VMAX
                        CALL DSCAL( N-KI+1, REC, WORK( KI+N ), 1 )
                        VMAX = ONE
                        VCRIT = BIGNUM
                     END IF
*
                     WORK( J+N ) = WORK( J+N ) -
     $                             DDOT( J-KI-1, T( KI+1, J ), 1,
     $                             WORK( KI+1+N ), 1 )
*
*                    Solve (T(J,J)-WR)'*X = WORK
*
                     CALL DLALN2( .FALSE., 1, 1, SMIN, ONE, T( J, J ),
     $                            LDT, ONE, ONE, WORK( J+N ), N, WR,
     $                            ZERO, X, 2, SCALE, XNORM, IERR )
*
*                    Scale if necessary
*
                     IF( SCALE.NE.ONE )
     $                  CALL DSCAL( N-KI+1, SCALE, WORK( KI+N ), 1 )
                     WORK( J+N ) = X( 1, 1 )
                     VMAX = MAX( ABS( WORK( J+N ) ), VMAX )
                     VCRIT = BIGNUM / VMAX
***
*                    Increment op count, ignoring the possible scaling
                     OPST = OPST + ( 2*( J-KI-1 )+6 )
***
*
                  ELSE
*
*                    2-by-2 diagonal block
*
*                    Scale if necessary to avoid overflow when forming
*                    the right-hand side.
*
                     BETA = MAX( WORK( J ), WORK( J+1 ) )
                     IF( BETA.GT.VCRIT ) THEN
                        REC = ONE / VMAX
                        CALL DSCAL( N-KI+1, REC, WORK( KI+N ), 1 )
                        VMAX = ONE
                        VCRIT = BIGNUM
                     END IF
*
                     WORK( J+N ) = WORK( J+N ) -
     $                             DDOT( J-KI-1, T( KI+1, J ), 1,
     $                             WORK( KI+1+N ), 1 )
*
                     WORK( J+1+N ) = WORK( J+1+N ) -
     $                               DDOT( J-KI-1, T( KI+1, J+1 ), 1,
     $                               WORK( KI+1+N ), 1 )
*
*                    Solve
*                      [T(J,J)-WR   T(J,J+1)     ]'* X = SCALE*( WORK1 )
*                      [T(J+1,J)    T(J+1,J+1)-WR]             ( WORK2 )
*
                     CALL DLALN2( .TRUE., 2, 1, SMIN, ONE, T( J, J ),
     $                            LDT, ONE, ONE, WORK( J+N ), N, WR,
     $                            ZERO, X, 2, SCALE, XNORM, IERR )
*
*                    Scale if necessary
*
                     IF( SCALE.NE.ONE )
     $                  CALL DSCAL( N-KI+1, SCALE, WORK( KI+N ), 1 )
                     WORK( J+N ) = X( 1, 1 )
                     WORK( J+1+N ) = X( 2, 1 )
*
                     VMAX = MAX( ABS( WORK( J+N ) ),
     $                      ABS( WORK( J+1+N ) ), VMAX )
                     VCRIT = BIGNUM / VMAX
***
*                    Increment op count, ignoring the possible scaling
                     OPST = OPST + ( 4*( J-KI-1 )+24 )
***
*
                  END IF
  170          CONTINUE
*
*              Copy the vector x or Q*x to VL and normalize.
*
               IF( .NOT.OVER ) THEN
                  CALL DCOPY( N-KI+1, WORK( KI+N ), 1, VL( KI, IS ), 1 )
*
                  II = IDAMAX( N-KI+1, VL( KI, IS ), 1 ) + KI - 1
                  REMAX = ONE / ABS( VL( II, IS ) )
                  CALL DSCAL( N-KI+1, REMAX, VL( KI, IS ), 1 )
***
                  OPST = OPST + ( 2*( N-KI+1 )+1 )
***
*
                  DO 180 K = 1, KI - 1
                     VL( K, IS ) = ZERO
  180             CONTINUE
*
               ELSE
*
                  IF( KI.LT.N )
     $               CALL DGEMV( 'N', N, N-KI, ONE, VL( 1, KI+1 ), LDVL,
     $                           WORK( KI+1+N ), 1, WORK( KI+N ),
     $                           VL( 1, KI ), 1 )
*
                  II = IDAMAX( N, VL( 1, KI ), 1 )
                  REMAX = ONE / ABS( VL( II, KI ) )
                  CALL DSCAL( N, REMAX, VL( 1, KI ), 1 )
***
                  OPS = OPS + ( 2*N*( N-KI+1 )+1 )
***
*
               END IF
*
            ELSE
*
*              Complex left eigenvector.
*
*               Initial solve:
*                 ((T(KI,KI)    T(KI,KI+1) )' - (WR - I* WI))*X = 0.
*                 ((T(KI+1,KI) T(KI+1,KI+1))                )
*
               IF( ABS( T( KI, KI+1 ) ).GE.ABS( T( KI+1, KI ) ) ) THEN
                  WORK( KI+N ) = WI / T( KI, KI+1 )
                  WORK( KI+1+N2 ) = ONE
               ELSE
                  WORK( KI+N ) = ONE
                  WORK( KI+1+N2 ) = -WI / T( KI+1, KI )
               END IF
               WORK( KI+1+N ) = ZERO
               WORK( KI+N2 ) = ZERO
*
*              Form right-hand side
*
               DO 190 K = KI + 2, N
                  WORK( K+N ) = -WORK( KI+N )*T( KI, K )
                  WORK( K+N2 ) = -WORK( KI+1+N2 )*T( KI+1, K )
  190          CONTINUE
***
               OPST = OPST + 2*( N-KI-1 )
***
*
*              Solve complex quasi-triangular system:
*              ( T(KI+2,N:KI+2,N) - (WR-i*WI) )*X = WORK1+i*WORK2
*
               VMAX = ONE
               VCRIT = BIGNUM
*
               JNXT = KI + 2
               DO 200 J = KI + 2, N
                  IF( J.LT.JNXT )
     $               GO TO 200
                  J1 = J
                  J2 = J
                  JNXT = J + 1
                  IF( J.LT.N ) THEN
                     IF( T( J+1, J ).NE.ZERO ) THEN
                        J2 = J + 1
                        JNXT = J + 2
                     END IF
                  END IF
*
                  IF( J1.EQ.J2 ) THEN
*
*                    1-by-1 diagonal block
*
*                    Scale if necessary to avoid overflow when
*                    forming the right-hand side elements.
*
                     IF( WORK( J ).GT.VCRIT ) THEN
                        REC = ONE / VMAX
                        CALL DSCAL( N-KI+1, REC, WORK( KI+N ), 1 )
                        CALL DSCAL( N-KI+1, REC, WORK( KI+N2 ), 1 )
                        VMAX = ONE
                        VCRIT = BIGNUM
                     END IF
*
                     WORK( J+N ) = WORK( J+N ) -
     $                             DDOT( J-KI-2, T( KI+2, J ), 1,
     $                             WORK( KI+2+N ), 1 )
                     WORK( J+N2 ) = WORK( J+N2 ) -
     $                              DDOT( J-KI-2, T( KI+2, J ), 1,
     $                              WORK( KI+2+N2 ), 1 )
*
*                    Solve (T(J,J)-(WR-i*WI))*(X11+i*X12)= WK+I*WK2
*
                     CALL DLALN2( .FALSE., 1, 2, SMIN, ONE, T( J, J ),
     $                            LDT, ONE, ONE, WORK( J+N ), N, WR,
     $                            -WI, X, 2, SCALE, XNORM, IERR )
*
*                    Scale if necessary
*
                     IF( SCALE.NE.ONE ) THEN
                        CALL DSCAL( N-KI+1, SCALE, WORK( KI+N ), 1 )
                        CALL DSCAL( N-KI+1, SCALE, WORK( KI+N2 ), 1 )
                     END IF
                     WORK( J+N ) = X( 1, 1 )
                     WORK( J+N2 ) = X( 1, 2 )
                     VMAX = MAX( ABS( WORK( J+N ) ),
     $                      ABS( WORK( J+N2 ) ), VMAX )
                     VCRIT = BIGNUM / VMAX
***
*                    Increment op count, ignoring the possible scaling
                     OPST = OPST + ( 4*( J-KI-2 )+24 )
***
*
                  ELSE
*
*                    2-by-2 diagonal block
*
*                    Scale if necessary to avoid overflow when forming
*                    the right-hand side elements.
*
                     BETA = MAX( WORK( J ), WORK( J+1 ) )
                     IF( BETA.GT.VCRIT ) THEN
                        REC = ONE / VMAX
                        CALL DSCAL( N-KI+1, REC, WORK( KI+N ), 1 )
                        CALL DSCAL( N-KI+1, REC, WORK( KI+N2 ), 1 )
                        VMAX = ONE
                        VCRIT = BIGNUM
                     END IF
*
                     WORK( J+N ) = WORK( J+N ) -
     $                             DDOT( J-KI-2, T( KI+2, J ), 1,
     $                             WORK( KI+2+N ), 1 )
*
                     WORK( J+N2 ) = WORK( J+N2 ) -
     $                              DDOT( J-KI-2, T( KI+2, J ), 1,
     $                              WORK( KI+2+N2 ), 1 )
*
                     WORK( J+1+N ) = WORK( J+1+N ) -
     $                               DDOT( J-KI-2, T( KI+2, J+1 ), 1,
     $                               WORK( KI+2+N ), 1 )
*
                     WORK( J+1+N2 ) = WORK( J+1+N2 ) -
     $                                DDOT( J-KI-2, T( KI+2, J+1 ), 1,
     $                                WORK( KI+2+N2 ), 1 )
*
*                    Solve 2-by-2 complex linear equation
*                      ([T(j,j)   T(j,j+1)  ]'-(wr-i*wi)*I)*X = SCALE*B
*                      ([T(j+1,j) T(j+1,j+1)]             )
*
                     CALL DLALN2( .TRUE., 2, 2, SMIN, ONE, T( J, J ),
     $                            LDT, ONE, ONE, WORK( J+N ), N, WR,
     $                            -WI, X, 2, SCALE, XNORM, IERR )
*
*                    Scale if necessary
*
                     IF( SCALE.NE.ONE ) THEN
                        CALL DSCAL( N-KI+1, SCALE, WORK( KI+N ), 1 )
                        CALL DSCAL( N-KI+1, SCALE, WORK( KI+N2 ), 1 )
                     END IF
                     WORK( J+N ) = X( 1, 1 )
                     WORK( J+N2 ) = X( 1, 2 )
                     WORK( J+1+N ) = X( 2, 1 )
                     WORK( J+1+N2 ) = X( 2, 2 )
                     VMAX = MAX( ABS( X( 1, 1 ) ), ABS( X( 1, 2 ) ),
     $                      ABS( X( 2, 1 ) ), ABS( X( 2, 2 ) ), VMAX )
                     VCRIT = BIGNUM / VMAX
***
*                    Increment op count, ignoring the possible scaling
                     OPST = OPST + ( 8*( J-KI-2 )+64 )
***
*
                  END IF
  200          CONTINUE
*
*              Copy the vector x or Q*x to VL and normalize.
*
  210          CONTINUE
               IF( .NOT.OVER ) THEN
                  CALL DCOPY( N-KI+1, WORK( KI+N ), 1, VL( KI, IS ), 1 )
                  CALL DCOPY( N-KI+1, WORK( KI+N2 ), 1, VL( KI, IS+1 ),
     $                        1 )
*
                  EMAX = ZERO
                  DO 220 K = KI, N
                     EMAX = MAX( EMAX, ABS( VL( K, IS ) )+
     $                      ABS( VL( K, IS+1 ) ) )
  220             CONTINUE
                  REMAX = ONE / EMAX
                  CALL DSCAL( N-KI+1, REMAX, VL( KI, IS ), 1 )
                  CALL DSCAL( N-KI+1, REMAX, VL( KI, IS+1 ), 1 )
***
                  OPST = OPST + ( 4*( N-KI+1 )+1 )
***
*
                  DO 230 K = 1, KI - 1
                     VL( K, IS ) = ZERO
                     VL( K, IS+1 ) = ZERO
  230             CONTINUE
               ELSE
                  IF( KI.LT.N-1 ) THEN
                     CALL DGEMV( 'N', N, N-KI-1, ONE, VL( 1, KI+2 ),
     $                           LDVL, WORK( KI+2+N ), 1, WORK( KI+N ),
     $                           VL( 1, KI ), 1 )
                     CALL DGEMV( 'N', N, N-KI-1, ONE, VL( 1, KI+2 ),
     $                           LDVL, WORK( KI+2+N2 ), 1,
     $                           WORK( KI+1+N2 ), VL( 1, KI+1 ), 1 )
                  ELSE
                     CALL DSCAL( N, WORK( KI+N ), VL( 1, KI ), 1 )
                     CALL DSCAL( N, WORK( KI+1+N2 ), VL( 1, KI+1 ), 1 )
                  END IF
*
                  EMAX = ZERO
                  DO 240 K = 1, N
                     EMAX = MAX( EMAX, ABS( VL( K, KI ) )+
     $                      ABS( VL( K, KI+1 ) ) )
  240             CONTINUE
                  REMAX = ONE / EMAX
                  CALL DSCAL( N, REMAX, VL( 1, KI ), 1 )
                  CALL DSCAL( N, REMAX, VL( 1, KI+1 ), 1 )
***
                  OPS = OPS + ( 4*N*( N-KI-1 )+6*N+1 )
***
*
               END IF
*
            END IF
*
            IS = IS + 1
            IF( IP.NE.0 )
     $         IS = IS + 1
  250       CONTINUE
            IF( IP.EQ.-1 )
     $         IP = 0
            IF( IP.EQ.1 )
     $         IP = -1
*
  260    CONTINUE
*
      END IF
***
*     Compute final op count
      OPS = OPS + OPST
***
*
      RETURN
*
*     End of DTREVC
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
