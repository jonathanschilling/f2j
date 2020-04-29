      SUBROUTINE DLAGGE( M, N, KL, KU, D, A, LDA, ISEED, WORK, INFO )
*
*  -- LAPACK auxiliary test routine (version 3.0)
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     February 29, 1992
*
*     .. Scalar Arguments ..
      INTEGER            INFO, KL, KU, LDA, M, N
*     ..
*     .. Array Arguments ..
      INTEGER            ISEED( 4 )
      DOUBLE PRECISION   A( LDA, * ), D( * ), WORK( * )
*     ..
*
*  Purpose
*  =======
*
*  DLAGGE generates a real general m by n matrix A, by pre- and post-
*  multiplying a real diagonal matrix D with random orthogonal matrices:
*  A = U*D*V. The lower and upper bandwidths may then be reduced to
*  kl and ku by additional orthogonal transformations.
*
*  Arguments
*  =========
*
*  M       (input) INTEGER
*          The number of rows of the matrix A.  M >= 0.
*
*  N       (input) INTEGER
*          The number of columns of the matrix A.  N >= 0.
*
*  KL      (input) INTEGER
*          The number of nonzero subdiagonals within the band of A.
*          0 <= KL <= M-1.
*
*  KU      (input) INTEGER
*          The number of nonzero superdiagonals within the band of A.
*          0 <= KU <= N-1.
*
*  D       (input) DOUBLE PRECISION array, dimension (min(M,N))
*          The diagonal elements of the diagonal matrix D.
*
*  A       (output) DOUBLE PRECISION array, dimension (LDA,N)
*          The generated m by n matrix A.
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A.  LDA >= M.
*
*  ISEED   (input/output) INTEGER array, dimension (4)
*          On entry, the seed of the random number generator; the array
*          elements must be between 0 and 4095, and ISEED(4) must be
*          odd.
*          On exit, the seed is updated.
*
*  WORK    (workspace) DOUBLE PRECISION array, dimension (M+N)
*
*  INFO    (output) INTEGER
*          = 0: successful exit
*          < 0: if INFO = -i, the i-th argument had an illegal value
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE
      PARAMETER          ( ZERO = 0.0D+0, ONE = 1.0D+0 )
*     ..
*     .. Local Scalars ..
      INTEGER            I, J
      DOUBLE PRECISION   TAU, WA, WB, WN
*     ..
*     .. External Subroutines ..
      EXTERNAL           DGEMV, DGER, DLARNV, DSCAL, XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX, MIN, SIGN
*     ..
*     .. External Functions ..
      DOUBLE PRECISION   DNRM2
      EXTERNAL           DNRM2
*     ..
*     .. Executable Statements ..
*
*     Test the input arguments
*
      INFO = 0
      IF( M.LT.0 ) THEN
         INFO = -1
      ELSE IF( N.LT.0 ) THEN
         INFO = -2
      ELSE IF( KL.LT.0 .OR. KL.GT.M-1 ) THEN
         INFO = -3
      ELSE IF( KU.LT.0 .OR. KU.GT.N-1 ) THEN
         INFO = -4
      ELSE IF( LDA.LT.MAX( 1, M ) ) THEN
         INFO = -7
      END IF
      IF( INFO.LT.0 ) THEN
         CALL XERBLA( 'DLAGGE', -INFO )
         RETURN
      END IF
*
*     initialize A to diagonal matrix
*
      DO 20 J = 1, N
         DO 10 I = 1, M
            A( I, J ) = ZERO
   10    CONTINUE
   20 CONTINUE
      DO 30 I = 1, MIN( M, N )
         A( I, I ) = D( I )
   30 CONTINUE
*
*     pre- and post-multiply A by random orthogonal matrices
*
      DO 40 I = MIN( M, N ), 1, -1
         IF( I.LT.M ) THEN
*
*           generate random reflection
*
            CALL DLARNV( 3, ISEED, M-I+1, WORK )
            WN = DNRM2( M-I+1, WORK, 1 )
            WA = SIGN( WN, WORK( 1 ) )
            IF( WN.EQ.ZERO ) THEN
               TAU = ZERO
            ELSE
               WB = WORK( 1 ) + WA
               CALL DSCAL( M-I, ONE / WB, WORK( 2 ), 1 )
               WORK( 1 ) = ONE
               TAU = WB / WA
            END IF
*
*           multiply A(i:m,i:n) by random reflection from the left
*
            CALL DGEMV( 'Transpose', M-I+1, N-I+1, ONE, A( I, I ), LDA,
     $                  WORK, 1, ZERO, WORK( M+1 ), 1 )
            CALL DGER( M-I+1, N-I+1, -TAU, WORK, 1, WORK( M+1 ), 1,
     $                 A( I, I ), LDA )
         END IF
         IF( I.LT.N ) THEN
*
*           generate random reflection
*
            CALL DLARNV( 3, ISEED, N-I+1, WORK )
            WN = DNRM2( N-I+1, WORK, 1 )
            WA = SIGN( WN, WORK( 1 ) )
            IF( WN.EQ.ZERO ) THEN
               TAU = ZERO
            ELSE
               WB = WORK( 1 ) + WA
               CALL DSCAL( N-I, ONE / WB, WORK( 2 ), 1 )
               WORK( 1 ) = ONE
               TAU = WB / WA
            END IF
*
*           multiply A(i:m,i:n) by random reflection from the right
*
            CALL DGEMV( 'No transpose', M-I+1, N-I+1, ONE, A( I, I ),
     $                  LDA, WORK, 1, ZERO, WORK( N+1 ), 1 )
            CALL DGER( M-I+1, N-I+1, -TAU, WORK( N+1 ), 1, WORK, 1,
     $                 A( I, I ), LDA )
         END IF
   40 CONTINUE
*
*     Reduce number of subdiagonals to KL and number of superdiagonals
*     to KU
*
      DO 70 I = 1, MAX( M-1-KL, N-1-KU )
         IF( KL.LE.KU ) THEN
*
*           annihilate subdiagonal elements first (necessary if KL = 0)
*
            IF( I.LE.MIN( M-1-KL, N ) ) THEN
*
*              generate reflection to annihilate A(kl+i+1:m,i)
*
               WN = DNRM2( M-KL-I+1, A( KL+I, I ), 1 )
               WA = SIGN( WN, A( KL+I, I ) )
               IF( WN.EQ.ZERO ) THEN
                  TAU = ZERO
               ELSE
                  WB = A( KL+I, I ) + WA
                  CALL DSCAL( M-KL-I, ONE / WB, A( KL+I+1, I ), 1 )
                  A( KL+I, I ) = ONE
                  TAU = WB / WA
               END IF
*
*              apply reflection to A(kl+i:m,i+1:n) from the left
*
               CALL DGEMV( 'Transpose', M-KL-I+1, N-I, ONE,
     $                     A( KL+I, I+1 ), LDA, A( KL+I, I ), 1, ZERO,
     $                     WORK, 1 )
               CALL DGER( M-KL-I+1, N-I, -TAU, A( KL+I, I ), 1, WORK, 1,
     $                    A( KL+I, I+1 ), LDA )
               A( KL+I, I ) = -WA
            END IF
*
            IF( I.LE.MIN( N-1-KU, M ) ) THEN
*
*              generate reflection to annihilate A(i,ku+i+1:n)
*
               WN = DNRM2( N-KU-I+1, A( I, KU+I ), LDA )
               WA = SIGN( WN, A( I, KU+I ) )
               IF( WN.EQ.ZERO ) THEN
                  TAU = ZERO
               ELSE
                  WB = A( I, KU+I ) + WA
                  CALL DSCAL( N-KU-I, ONE / WB, A( I, KU+I+1 ), LDA )
                  A( I, KU+I ) = ONE
                  TAU = WB / WA
               END IF
*
*              apply reflection to A(i+1:m,ku+i:n) from the right
*
               CALL DGEMV( 'No transpose', M-I, N-KU-I+1, ONE,
     $                     A( I+1, KU+I ), LDA, A( I, KU+I ), LDA, ZERO,
     $                     WORK, 1 )
               CALL DGER( M-I, N-KU-I+1, -TAU, WORK, 1, A( I, KU+I ),
     $                    LDA, A( I+1, KU+I ), LDA )
               A( I, KU+I ) = -WA
            END IF
         ELSE
*
*           annihilate superdiagonal elements first (necessary if
*           KU = 0)
*
            IF( I.LE.MIN( N-1-KU, M ) ) THEN
*
*              generate reflection to annihilate A(i,ku+i+1:n)
*
               WN = DNRM2( N-KU-I+1, A( I, KU+I ), LDA )
               WA = SIGN( WN, A( I, KU+I ) )
               IF( WN.EQ.ZERO ) THEN
                  TAU = ZERO
               ELSE
                  WB = A( I, KU+I ) + WA
                  CALL DSCAL( N-KU-I, ONE / WB, A( I, KU+I+1 ), LDA )
                  A( I, KU+I ) = ONE
                  TAU = WB / WA
               END IF
*
*              apply reflection to A(i+1:m,ku+i:n) from the right
*
               CALL DGEMV( 'No transpose', M-I, N-KU-I+1, ONE,
     $                     A( I+1, KU+I ), LDA, A( I, KU+I ), LDA, ZERO,
     $                     WORK, 1 )
               CALL DGER( M-I, N-KU-I+1, -TAU, WORK, 1, A( I, KU+I ),
     $                    LDA, A( I+1, KU+I ), LDA )
               A( I, KU+I ) = -WA
            END IF
*
            IF( I.LE.MIN( M-1-KL, N ) ) THEN
*
*              generate reflection to annihilate A(kl+i+1:m,i)
*
               WN = DNRM2( M-KL-I+1, A( KL+I, I ), 1 )
               WA = SIGN( WN, A( KL+I, I ) )
               IF( WN.EQ.ZERO ) THEN
                  TAU = ZERO
               ELSE
                  WB = A( KL+I, I ) + WA
                  CALL DSCAL( M-KL-I, ONE / WB, A( KL+I+1, I ), 1 )
                  A( KL+I, I ) = ONE
                  TAU = WB / WA
               END IF
*
*              apply reflection to A(kl+i:m,i+1:n) from the left
*
               CALL DGEMV( 'Transpose', M-KL-I+1, N-I, ONE,
     $                     A( KL+I, I+1 ), LDA, A( KL+I, I ), 1, ZERO,
     $                     WORK, 1 )
               CALL DGER( M-KL-I+1, N-I, -TAU, A( KL+I, I ), 1, WORK, 1,
     $                    A( KL+I, I+1 ), LDA )
               A( KL+I, I ) = -WA
            END IF
         END IF
*
         DO 50 J = KL + I + 1, M
            A( J, I ) = ZERO
   50    CONTINUE
*
         DO 60 J = KU + I + 1, N
            A( I, J ) = ZERO
   60    CONTINUE
   70 CONTINUE
      RETURN
*
*     End of DLAGGE
*
      END
      SUBROUTINE DLAGSY( N, K, D, A, LDA, ISEED, WORK, INFO )
*
*  -- LAPACK auxiliary test routine (version 3.0)
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     February 29, 1992
*
*     .. Scalar Arguments ..
      INTEGER            INFO, K, LDA, N
*     ..
*     .. Array Arguments ..
      INTEGER            ISEED( 4 )
      DOUBLE PRECISION   A( LDA, * ), D( * ), WORK( * )
*     ..
*
*  Purpose
*  =======
*
*  DLAGSY generates a real symmetric matrix A, by pre- and post-
*  multiplying a real diagonal matrix D with a random orthogonal matrix:
*  A = U*D*U'. The semi-bandwidth may then be reduced to k by additional
*  orthogonal transformations.
*
*  Arguments
*  =========
*
*  N       (input) INTEGER
*          The order of the matrix A.  N >= 0.
*
*  K       (input) INTEGER
*          The number of nonzero subdiagonals within the band of A.
*          0 <= K <= N-1.
*
*  D       (input) DOUBLE PRECISION array, dimension (N)
*          The diagonal elements of the diagonal matrix D.
*
*  A       (output) DOUBLE PRECISION array, dimension (LDA,N)
*          The generated n by n symmetric matrix A (the full matrix is
*          stored).
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A.  LDA >= N.
*
*  ISEED   (input/output) INTEGER array, dimension (4)
*          On entry, the seed of the random number generator; the array
*          elements must be between 0 and 4095, and ISEED(4) must be
*          odd.
*          On exit, the seed is updated.
*
*  WORK    (workspace) DOUBLE PRECISION array, dimension (2*N)
*
*  INFO    (output) INTEGER
*          = 0: successful exit
*          < 0: if INFO = -i, the i-th argument had an illegal value
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE, HALF
      PARAMETER          ( ZERO = 0.0D+0, ONE = 1.0D+0, HALF = 0.5D+0 )
*     ..
*     .. Local Scalars ..
      INTEGER            I, J
      DOUBLE PRECISION   ALPHA, TAU, WA, WB, WN
*     ..
*     .. External Subroutines ..
      EXTERNAL           DAXPY, DGEMV, DGER, DLARNV, DSCAL, DSYMV,
     $                   DSYR2, XERBLA
*     ..
*     .. External Functions ..
      DOUBLE PRECISION   DDOT, DNRM2
      EXTERNAL           DDOT, DNRM2
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX, SIGN
*     ..
*     .. Executable Statements ..
*
*     Test the input arguments
*
      INFO = 0
      IF( N.LT.0 ) THEN
         INFO = -1
      ELSE IF( K.LT.0 .OR. K.GT.N-1 ) THEN
         INFO = -2
      ELSE IF( LDA.LT.MAX( 1, N ) ) THEN
         INFO = -5
      END IF
      IF( INFO.LT.0 ) THEN
         CALL XERBLA( 'DLAGSY', -INFO )
         RETURN
      END IF
*
*     initialize lower triangle of A to diagonal matrix
*
      DO 20 J = 1, N
         DO 10 I = J + 1, N
            A( I, J ) = ZERO
   10    CONTINUE
   20 CONTINUE
      DO 30 I = 1, N
         A( I, I ) = D( I )
   30 CONTINUE
*
*     Generate lower triangle of symmetric matrix
*
      DO 40 I = N - 1, 1, -1
*
*        generate random reflection
*
         CALL DLARNV( 3, ISEED, N-I+1, WORK )
         WN = DNRM2( N-I+1, WORK, 1 )
         WA = SIGN( WN, WORK( 1 ) )
         IF( WN.EQ.ZERO ) THEN
            TAU = ZERO
         ELSE
            WB = WORK( 1 ) + WA
            CALL DSCAL( N-I, ONE / WB, WORK( 2 ), 1 )
            WORK( 1 ) = ONE
            TAU = WB / WA
         END IF
*
*        apply random reflection to A(i:n,i:n) from the left
*        and the right
*
*        compute  y := tau * A * u
*
         CALL DSYMV( 'Lower', N-I+1, TAU, A( I, I ), LDA, WORK, 1, ZERO,
     $               WORK( N+1 ), 1 )
*
*        compute  v := y - 1/2 * tau * ( y, u ) * u
*
         ALPHA = -HALF*TAU*DDOT( N-I+1, WORK( N+1 ), 1, WORK, 1 )
         CALL DAXPY( N-I+1, ALPHA, WORK, 1, WORK( N+1 ), 1 )
*
*        apply the transformation as a rank-2 update to A(i:n,i:n)
*
         CALL DSYR2( 'Lower', N-I+1, -ONE, WORK, 1, WORK( N+1 ), 1,
     $               A( I, I ), LDA )
   40 CONTINUE
*
*     Reduce number of subdiagonals to K
*
      DO 60 I = 1, N - 1 - K
*
*        generate reflection to annihilate A(k+i+1:n,i)
*
         WN = DNRM2( N-K-I+1, A( K+I, I ), 1 )
         WA = SIGN( WN, A( K+I, I ) )
         IF( WN.EQ.ZERO ) THEN
            TAU = ZERO
         ELSE
            WB = A( K+I, I ) + WA
            CALL DSCAL( N-K-I, ONE / WB, A( K+I+1, I ), 1 )
            A( K+I, I ) = ONE
            TAU = WB / WA
         END IF
*
*        apply reflection to A(k+i:n,i+1:k+i-1) from the left
*
         CALL DGEMV( 'Transpose', N-K-I+1, K-1, ONE, A( K+I, I+1 ), LDA,
     $               A( K+I, I ), 1, ZERO, WORK, 1 )
         CALL DGER( N-K-I+1, K-1, -TAU, A( K+I, I ), 1, WORK, 1,
     $              A( K+I, I+1 ), LDA )
*
*        apply reflection to A(k+i:n,k+i:n) from the left and the right
*
*        compute  y := tau * A * u
*
         CALL DSYMV( 'Lower', N-K-I+1, TAU, A( K+I, K+I ), LDA,
     $               A( K+I, I ), 1, ZERO, WORK, 1 )
*
*        compute  v := y - 1/2 * tau * ( y, u ) * u
*
         ALPHA = -HALF*TAU*DDOT( N-K-I+1, WORK, 1, A( K+I, I ), 1 )
         CALL DAXPY( N-K-I+1, ALPHA, A( K+I, I ), 1, WORK, 1 )
*
*        apply symmetric rank-2 update to A(k+i:n,k+i:n)
*
         CALL DSYR2( 'Lower', N-K-I+1, -ONE, A( K+I, I ), 1, WORK, 1,
     $               A( K+I, K+I ), LDA )
*
         A( K+I, I ) = -WA
         DO 50 J = K + I + 1, N
            A( J, I ) = ZERO
   50    CONTINUE
   60 CONTINUE
*
*     Store full symmetric matrix
*
      DO 80 J = 1, N
         DO 70 I = J + 1, N
            A( J, I ) = A( I, J )
   70    CONTINUE
   80 CONTINUE
      RETURN
*
*     End of DLAGSY
*
      END
      SUBROUTINE DLARGE( N, A, LDA, ISEED, WORK, INFO )
*
*  -- LAPACK auxiliary test routine (version 3.0)
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     February 29, 1992
*
*     .. Scalar Arguments ..
      INTEGER            INFO, LDA, N
*     ..
*     .. Array Arguments ..
      INTEGER            ISEED( 4 )
      DOUBLE PRECISION   A( LDA, * ), WORK( * )
*     ..
*
*  Purpose
*  =======
*
*  DLARGE pre- and post-multiplies a real general n by n matrix A
*  with a random orthogonal matrix: A = U*D*U'.
*
*  Arguments
*  =========
*
*  N       (input) INTEGER
*          The order of the matrix A.  N >= 0.
*
*  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
*          On entry, the original n by n matrix A.
*          On exit, A is overwritten by U*A*U' for some random
*          orthogonal matrix U.
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A.  LDA >= N.
*
*  ISEED   (input/output) INTEGER array, dimension (4)
*          On entry, the seed of the random number generator; the array
*          elements must be between 0 and 4095, and ISEED(4) must be
*          odd.
*          On exit, the seed is updated.
*
*  WORK    (workspace) DOUBLE PRECISION array, dimension (2*N)
*
*  INFO    (output) INTEGER
*          = 0: successful exit
*          < 0: if INFO = -i, the i-th argument had an illegal value
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE
      PARAMETER          ( ZERO = 0.0D+0, ONE = 1.0D+0 )
*     ..
*     .. Local Scalars ..
      INTEGER            I
      DOUBLE PRECISION   TAU, WA, WB, WN
*     ..
*     .. External Subroutines ..
      EXTERNAL           DGEMV, DGER, DLARNV, DSCAL, XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX, SIGN
*     ..
*     .. External Functions ..
      DOUBLE PRECISION   DNRM2
      EXTERNAL           DNRM2
*     ..
*     .. Executable Statements ..
*
*     Test the input arguments
*
      INFO = 0
      IF( N.LT.0 ) THEN
         INFO = -1
      ELSE IF( LDA.LT.MAX( 1, N ) ) THEN
         INFO = -3
      END IF
      IF( INFO.LT.0 ) THEN
         CALL XERBLA( 'DLARGE', -INFO )
         RETURN
      END IF
*
*     pre- and post-multiply A by random orthogonal matrix
*
      DO 10 I = N, 1, -1
*
*        generate random reflection
*
         CALL DLARNV( 3, ISEED, N-I+1, WORK )
         WN = DNRM2( N-I+1, WORK, 1 )
         WA = SIGN( WN, WORK( 1 ) )
         IF( WN.EQ.ZERO ) THEN
            TAU = ZERO
         ELSE
            WB = WORK( 1 ) + WA
            CALL DSCAL( N-I, ONE / WB, WORK( 2 ), 1 )
            WORK( 1 ) = ONE
            TAU = WB / WA
         END IF
*
*        multiply A(i:n,1:n) by random reflection from the left
*
         CALL DGEMV( 'Transpose', N-I+1, N, ONE, A( I, 1 ), LDA, WORK,
     $               1, ZERO, WORK( N+1 ), 1 )
         CALL DGER( N-I+1, N, -TAU, WORK, 1, WORK( N+1 ), 1, A( I, 1 ),
     $              LDA )
*
*        multiply A(1:n,i:n) by random reflection from the right
*
         CALL DGEMV( 'No transpose', N, N-I+1, ONE, A( 1, I ), LDA,
     $               WORK, 1, ZERO, WORK( N+1 ), 1 )
         CALL DGER( N, N-I+1, -TAU, WORK( N+1 ), 1, WORK, 1, A( 1, I ),
     $              LDA )
   10 CONTINUE
      RETURN
*
*     End of DLARGE
*
      END
      SUBROUTINE DLAROR( SIDE, INIT, M, N, A, LDA, ISEED, X, INFO )
*
*  -- LAPACK auxiliary test routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     September 30, 1994
*
*     .. Scalar Arguments ..
      CHARACTER          INIT, SIDE
      INTEGER            INFO, LDA, M, N
*     ..
*     .. Array Arguments ..
      INTEGER            ISEED( 4 )
      DOUBLE PRECISION   A( LDA, * ), X( * )
*     ..
*
*  Purpose
*  =======
*
*  DLAROR pre- or post-multiplies an M by N matrix A by a random
*  orthogonal matrix U, overwriting A.  A may optionally be initialized
*  to the identity matrix before multiplying by U.  U is generated using
*  the method of G.W. Stewart (SIAM J. Numer. Anal. 17, 1980, 403-409).
*
*  Arguments
*  =========
*
*  SIDE    (input) CHARACTER*1
*          Specifies whether A is multiplied on the left or right by U.
*          = 'L':         Multiply A on the left (premultiply) by U
*          = 'R':         Multiply A on the right (postmultiply) by U'
*          = 'C' or 'T':  Multiply A on the left by U and the right
*                          by U' (Here, U' means U-transpose.)
*
*  INIT    (input) CHARACTER*1
*          Specifies whether or not A should be initialized to the
*          identity matrix.
*          = 'I':  Initialize A to (a section of) the identity matrix
*                   before applying U.
*          = 'N':  No initialization.  Apply U to the input matrix A.
*
*          INIT = 'I' may be used to generate square or rectangular
*          orthogonal matrices:
*
*          For M = N and SIDE = 'L' or 'R', the rows will be orthogonal
*          to each other, as will the columns.
*
*          If M < N, SIDE = 'R' produces a dense matrix whose rows are
*          orthogonal and whose columns are not, while SIDE = 'L'
*          produces a matrix whose rows are orthogonal, and whose first
*          M columns are orthogonal, and whose remaining columns are
*          zero.
*
*          If M > N, SIDE = 'L' produces a dense matrix whose columns
*          are orthogonal and whose rows are not, while SIDE = 'R'
*          produces a matrix whose columns are orthogonal, and whose
*          first M rows are orthogonal, and whose remaining rows are
*          zero.
*
*  M       (input) INTEGER
*          The number of rows of A.
*
*  N       (input) INTEGER
*          The number of columns of A.
*
*  A       (input/output) DOUBLE PRECISION array, dimension (LDA, N)
*          On entry, the array A.
*          On exit, overwritten by U A ( if SIDE = 'L' ),
*           or by A U ( if SIDE = 'R' ),
*           or by U A U' ( if SIDE = 'C' or 'T').
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A.  LDA >= max(1,M).
*
*  ISEED   (input/output) INTEGER array, dimension (4)
*          On entry ISEED specifies the seed of the random number
*          generator. The array elements should be between 0 and 4095;
*          if not they will be reduced mod 4096.  Also, ISEED(4) must
*          be odd.  The random number generator uses a linear
*          congruential sequence limited to small integers, and so
*          should produce machine independent random numbers. The
*          values of ISEED are changed on exit, and can be used in the
*          next call to DLAROR to continue the same random number
*          sequence.
*
*  X       (workspace) DOUBLE PRECISION array, dimension (3*MAX( M, N ))
*          Workspace of length
*              2*M + N if SIDE = 'L',
*              2*N + M if SIDE = 'R',
*              3*N     if SIDE = 'C' or 'T'.
*
*  INFO    (output) INTEGER
*          An error flag.  It is set to:
*          = 0:  normal return
*          < 0:  if INFO = -k, the k-th argument had an illegal value
*          = 1:  if the random numbers generated by DLARND are bad.
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE, TOOSML
      PARAMETER          ( ZERO = 0.0D+0, ONE = 1.0D+0,
     $                   TOOSML = 1.0D-20 )
*     ..
*     .. Local Scalars ..
      INTEGER            IROW, ITYPE, IXFRM, J, JCOL, KBEG, NXFRM
      DOUBLE PRECISION   FACTOR, XNORM, XNORMS
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      DOUBLE PRECISION   DLARND, DNRM2
      EXTERNAL           LSAME, DLARND, DNRM2
*     ..
*     .. External Subroutines ..
      EXTERNAL           DGEMV, DGER, DLASET, DSCAL, XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, SIGN
*     ..
*     .. Executable Statements ..
*
      IF( N.EQ.0 .OR. M.EQ.0 )
     $   RETURN
*
      ITYPE = 0
      IF( LSAME( SIDE, 'L' ) ) THEN
         ITYPE = 1
      ELSE IF( LSAME( SIDE, 'R' ) ) THEN
         ITYPE = 2
      ELSE IF( LSAME( SIDE, 'C' ) .OR. LSAME( SIDE, 'T' ) ) THEN
         ITYPE = 3
      END IF
*
*     Check for argument errors.
*
      INFO = 0
      IF( ITYPE.EQ.0 ) THEN
         INFO = -1
      ELSE IF( M.LT.0 ) THEN
         INFO = -3
      ELSE IF( N.LT.0 .OR. ( ITYPE.EQ.3 .AND. N.NE.M ) ) THEN
         INFO = -4
      ELSE IF( LDA.LT.M ) THEN
         INFO = -6
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DLAROR', -INFO )
         RETURN
      END IF
*
      IF( ITYPE.EQ.1 ) THEN
         NXFRM = M
      ELSE
         NXFRM = N
      END IF
*
*     Initialize A to the identity matrix if desired
*
      IF( LSAME( INIT, 'I' ) )
     $   CALL DLASET( 'Full', M, N, ZERO, ONE, A, LDA )
*
*     If no rotation possible, multiply by random +/-1
*
*     Compute rotation by computing Householder transformations
*     H(2), H(3), ..., H(nhouse)
*
      DO 10 J = 1, NXFRM
         X( J ) = ZERO
   10 CONTINUE
*
      DO 30 IXFRM = 2, NXFRM
         KBEG = NXFRM - IXFRM + 1
*
*        Generate independent normal( 0, 1 ) random numbers
*
         DO 20 J = KBEG, NXFRM
            X( J ) = DLARND( 3, ISEED )
   20    CONTINUE
*
*        Generate a Householder transformation from the random vector X
*
         XNORM = DNRM2( IXFRM, X( KBEG ), 1 )
         XNORMS = SIGN( XNORM, X( KBEG ) )
         X( KBEG+NXFRM ) = SIGN( ONE, -X( KBEG ) )
         FACTOR = XNORMS*( XNORMS+X( KBEG ) )
         IF( ABS( FACTOR ).LT.TOOSML ) THEN
            INFO = 1
            CALL XERBLA( 'DLAROR', INFO )
            RETURN
         ELSE
            FACTOR = ONE / FACTOR
         END IF
         X( KBEG ) = X( KBEG ) + XNORMS
*
*        Apply Householder transformation to A
*
         IF( ITYPE.EQ.1 .OR. ITYPE.EQ.3 ) THEN
*
*           Apply H(k) from the left.
*
            CALL DGEMV( 'T', IXFRM, N, ONE, A( KBEG, 1 ), LDA,
     $                  X( KBEG ), 1, ZERO, X( 2*NXFRM+1 ), 1 )
            CALL DGER( IXFRM, N, -FACTOR, X( KBEG ), 1, X( 2*NXFRM+1 ),
     $                 1, A( KBEG, 1 ), LDA )
*
         END IF
*
         IF( ITYPE.EQ.2 .OR. ITYPE.EQ.3 ) THEN
*
*           Apply H(k) from the right.
*
            CALL DGEMV( 'N', M, IXFRM, ONE, A( 1, KBEG ), LDA,
     $                  X( KBEG ), 1, ZERO, X( 2*NXFRM+1 ), 1 )
            CALL DGER( M, IXFRM, -FACTOR, X( 2*NXFRM+1 ), 1, X( KBEG ),
     $                 1, A( 1, KBEG ), LDA )
*
         END IF
   30 CONTINUE
*
      X( 2*NXFRM ) = SIGN( ONE, DLARND( 3, ISEED ) )
*
*     Scale the matrix A by D.
*
      IF( ITYPE.EQ.1 .OR. ITYPE.EQ.3 ) THEN
         DO 40 IROW = 1, M
            CALL DSCAL( N, X( NXFRM+IROW ), A( IROW, 1 ), LDA )
   40    CONTINUE
      END IF
*
      IF( ITYPE.EQ.2 .OR. ITYPE.EQ.3 ) THEN
         DO 50 JCOL = 1, N
            CALL DSCAL( M, X( NXFRM+JCOL ), A( 1, JCOL ), 1 )
   50    CONTINUE
      END IF
      RETURN
*
*     End of DLAROR
*
      END
      SUBROUTINE DLAROT( LROWS, LLEFT, LRIGHT, NL, C, S, A, LDA, XLEFT,
     $                   XRIGHT )
*
*  -- LAPACK auxiliary test routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     February 29, 1992
*
*     .. Scalar Arguments ..
      LOGICAL            LLEFT, LRIGHT, LROWS
      INTEGER            LDA, NL
      DOUBLE PRECISION   C, S, XLEFT, XRIGHT
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   A( * )
*     ..
*
*  Purpose
*  =======
*
*     DLAROT applies a (Givens) rotation to two adjacent rows or
*     columns, where one element of the first and/or last column/row
*     may be a separate variable.  This is specifically indended
*     for use on matrices stored in some format other than GE, so
*     that elements of the matrix may be used or modified for which
*     no array element is provided.
*
*     One example is a symmetric matrix in SB format (bandwidth=4), for
*     which UPLO='L':  Two adjacent rows will have the format:
*
*     row j:     *  *  *  *  *  .  .  .  .
*     row j+1:      *  *  *  *  *  .  .  .  .
*
*     '*' indicates elements for which storage is provided,
*     '.' indicates elements for which no storage is provided, but
*     are not necessarily zero; their values are determined by
*     symmetry.  ' ' indicates elements which are necessarily zero,
*      and have no storage provided.
*
*     Those columns which have two '*'s can be handled by DROT.
*     Those columns which have no '*'s can be ignored, since as long
*     as the Givens rotations are carefully applied to preserve
*     symmetry, their values are determined.
*     Those columns which have one '*' have to be handled separately,
*     by using separate variables "p" and "q":
*
*     row j:     *  *  *  *  *  p  .  .  .
*     row j+1:   q  *  *  *  *  *  .  .  .  .
*
*     The element p would have to be set correctly, then that column
*     is rotated, setting p to its new value.  The next call to
*     DLAROT would rotate columns j and j+1, using p, and restore
*     symmetry.  The element q would start out being zero, and be
*     made non-zero by the rotation.  Later, rotations would presumably
*     be chosen to zero q out.
*
*     Typical Calling Sequences: rotating the i-th and (i+1)-st rows.
*     ------- ------- ---------
*
*       General dense matrix:
*
*               CALL DLAROT(.TRUE.,.FALSE.,.FALSE., N, C,S,
*                       A(i,1),LDA, DUMMY, DUMMY)
*
*       General banded matrix in GB format:
*
*               j = MAX(1, i-KL )
*               NL = MIN( N, i+KU+1 ) + 1-j
*               CALL DLAROT( .TRUE., i-KL.GE.1, i+KU.LT.N, NL, C,S,
*                       A(KU+i+1-j,j),LDA-1, XLEFT, XRIGHT )
*
*               [ note that i+1-j is just MIN(i,KL+1) ]
*
*       Symmetric banded matrix in SY format, bandwidth K,
*       lower triangle only:
*
*               j = MAX(1, i-K )
*               NL = MIN( K+1, i ) + 1
*               CALL DLAROT( .TRUE., i-K.GE.1, .TRUE., NL, C,S,
*                       A(i,j), LDA, XLEFT, XRIGHT )
*
*       Same, but upper triangle only:
*
*               NL = MIN( K+1, N-i ) + 1
*               CALL DLAROT( .TRUE., .TRUE., i+K.LT.N, NL, C,S,
*                       A(i,i), LDA, XLEFT, XRIGHT )
*
*       Symmetric banded matrix in SB format, bandwidth K,
*       lower triangle only:
*
*               [ same as for SY, except:]
*                   . . . .
*                       A(i+1-j,j), LDA-1, XLEFT, XRIGHT )
*
*               [ note that i+1-j is just MIN(i,K+1) ]
*
*       Same, but upper triangle only:
*                    . . .
*                       A(K+1,i), LDA-1, XLEFT, XRIGHT )
*
*       Rotating columns is just the transpose of rotating rows, except
*       for GB and SB: (rotating columns i and i+1)
*
*       GB:
*               j = MAX(1, i-KU )
*               NL = MIN( N, i+KL+1 ) + 1-j
*               CALL DLAROT( .TRUE., i-KU.GE.1, i+KL.LT.N, NL, C,S,
*                       A(KU+j+1-i,i),LDA-1, XTOP, XBOTTM )
*
*               [note that KU+j+1-i is just MAX(1,KU+2-i)]
*
*       SB: (upper triangle)
*
*                    . . . . . .
*                       A(K+j+1-i,i),LDA-1, XTOP, XBOTTM )
*
*       SB: (lower triangle)
*
*                    . . . . . .
*                       A(1,i),LDA-1, XTOP, XBOTTM )
*
*  Arguments
*  =========
*
*  LROWS  - LOGICAL
*           If .TRUE., then DLAROT will rotate two rows.  If .FALSE.,
*           then it will rotate two columns.
*           Not modified.
*
*  LLEFT  - LOGICAL
*           If .TRUE., then XLEFT will be used instead of the
*           corresponding element of A for the first element in the
*           second row (if LROWS=.FALSE.) or column (if LROWS=.TRUE.)
*           If .FALSE., then the corresponding element of A will be
*           used.
*           Not modified.
*
*  LRIGHT - LOGICAL
*           If .TRUE., then XRIGHT will be used instead of the
*           corresponding element of A for the last element in the
*           first row (if LROWS=.FALSE.) or column (if LROWS=.TRUE.) If
*           .FALSE., then the corresponding element of A will be used.
*           Not modified.
*
*  NL     - INTEGER
*           The length of the rows (if LROWS=.TRUE.) or columns (if
*           LROWS=.FALSE.) to be rotated.  If XLEFT and/or XRIGHT are
*           used, the columns/rows they are in should be included in
*           NL, e.g., if LLEFT = LRIGHT = .TRUE., then NL must be at
*           least 2.  The number of rows/columns to be rotated
*           exclusive of those involving XLEFT and/or XRIGHT may
*           not be negative, i.e., NL minus how many of LLEFT and
*           LRIGHT are .TRUE. must be at least zero; if not, XERBLA
*           will be called.
*           Not modified.
*
*  C, S   - DOUBLE PRECISION
*           Specify the Givens rotation to be applied.  If LROWS is
*           true, then the matrix ( c  s )
*                                 (-s  c )  is applied from the left;
*           if false, then the transpose thereof is applied from the
*           right.  For a Givens rotation, C**2 + S**2 should be 1,
*           but this is not checked.
*           Not modified.
*
*  A      - DOUBLE PRECISION array.
*           The array containing the rows/columns to be rotated.  The
*           first element of A should be the upper left element to
*           be rotated.
*           Read and modified.
*
*  LDA    - INTEGER
*           The "effective" leading dimension of A.  If A contains
*           a matrix stored in GE or SY format, then this is just
*           the leading dimension of A as dimensioned in the calling
*           routine.  If A contains a matrix stored in band (GB or SB)
*           format, then this should be *one less* than the leading
*           dimension used in the calling routine.  Thus, if
*           A were dimensioned A(LDA,*) in DLAROT, then A(1,j) would
*           be the j-th element in the first of the two rows
*           to be rotated, and A(2,j) would be the j-th in the second,
*           regardless of how the array may be stored in the calling
*           routine.  [A cannot, however, actually be dimensioned thus,
*           since for band format, the row number may exceed LDA, which
*           is not legal FORTRAN.]
*           If LROWS=.TRUE., then LDA must be at least 1, otherwise
*           it must be at least NL minus the number of .TRUE. values
*           in XLEFT and XRIGHT.
*           Not modified.
*
*  XLEFT  - DOUBLE PRECISION
*           If LLEFT is .TRUE., then XLEFT will be used and modified
*           instead of A(2,1) (if LROWS=.TRUE.) or A(1,2)
*           (if LROWS=.FALSE.).
*           Read and modified.
*
*  XRIGHT - DOUBLE PRECISION
*           If LRIGHT is .TRUE., then XRIGHT will be used and modified
*           instead of A(1,NL) (if LROWS=.TRUE.) or A(NL,1)
*           (if LROWS=.FALSE.).
*           Read and modified.
*
*  =====================================================================
*
*     .. Local Scalars ..
      INTEGER            IINC, INEXT, IX, IY, IYT, NT
*     ..
*     .. Local Arrays ..
      DOUBLE PRECISION   XT( 2 ), YT( 2 )
*     ..
*     .. External Subroutines ..
      EXTERNAL           DROT, XERBLA
*     ..
*     .. Executable Statements ..
*
*     Set up indices, arrays for ends
*
      IF( LROWS ) THEN
         IINC = LDA
         INEXT = 1
      ELSE
         IINC = 1
         INEXT = LDA
      END IF
*
      IF( LLEFT ) THEN
         NT = 1
         IX = 1 + IINC
         IY = 2 + LDA
         XT( 1 ) = A( 1 )
         YT( 1 ) = XLEFT
      ELSE
         NT = 0
         IX = 1
         IY = 1 + INEXT
      END IF
*
      IF( LRIGHT ) THEN
         IYT = 1 + INEXT + ( NL-1 )*IINC
         NT = NT + 1
         XT( NT ) = XRIGHT
         YT( NT ) = A( IYT )
      END IF
*
*     Check for errors
*
      IF( NL.LT.NT ) THEN
         CALL XERBLA( 'DLAROT', 4 )
         RETURN
      END IF
      IF( LDA.LE.0 .OR. ( .NOT.LROWS .AND. LDA.LT.NL-NT ) ) THEN
         CALL XERBLA( 'DLAROT', 8 )
         RETURN
      END IF
*
*     Rotate
*
      CALL DROT( NL-NT, A( IX ), IINC, A( IY ), IINC, C, S )
      CALL DROT( NT, XT, 1, YT, 1, C, S )
*
*     Stuff values back into XLEFT, XRIGHT, etc.
*
      IF( LLEFT ) THEN
         A( 1 ) = XT( 1 )
         XLEFT = YT( 1 )
      END IF
*
      IF( LRIGHT ) THEN
         XRIGHT = XT( NT )
         A( IYT ) = YT( NT )
      END IF
*
      RETURN
*
*     End of DLAROT
*
      END
      SUBROUTINE DLATM1( MODE, COND, IRSIGN, IDIST, ISEED, D, N, INFO )
*
*  -- LAPACK auxiliary test routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     September 30, 1994
*
*     .. Scalar Arguments ..
      INTEGER            IDIST, INFO, IRSIGN, MODE, N
      DOUBLE PRECISION   COND
*     ..
*     .. Array Arguments ..
      INTEGER            ISEED( 4 )
      DOUBLE PRECISION   D( * )
*     ..
*
*  Purpose
*  =======
*
*     DLATM1 computes the entries of D(1..N) as specified by
*     MODE, COND and IRSIGN. IDIST and ISEED determine the generation
*     of random numbers. DLATM1 is called by SLATMR to generate
*     random test matrices for LAPACK programs.
*
*  Arguments
*  =========
*
*  MODE   - INTEGER
*           On entry describes how D is to be computed:
*           MODE = 0 means do not change D.
*           MODE = 1 sets D(1)=1 and D(2:N)=1.0/COND
*           MODE = 2 sets D(1:N-1)=1 and D(N)=1.0/COND
*           MODE = 3 sets D(I)=COND**(-(I-1)/(N-1))
*           MODE = 4 sets D(i)=1 - (i-1)/(N-1)*(1 - 1/COND)
*           MODE = 5 sets D to random numbers in the range
*                    ( 1/COND , 1 ) such that their logarithms
*                    are uniformly distributed.
*           MODE = 6 set D to random numbers from same distribution
*                    as the rest of the matrix.
*           MODE < 0 has the same meaning as ABS(MODE), except that
*              the order of the elements of D is reversed.
*           Thus if MODE is positive, D has entries ranging from
*              1 to 1/COND, if negative, from 1/COND to 1,
*           Not modified.
*
*  COND   - DOUBLE PRECISION
*           On entry, used as described under MODE above.
*           If used, it must be >= 1. Not modified.
*
*  IRSIGN - INTEGER
*           On entry, if MODE neither -6, 0 nor 6, determines sign of
*           entries of D
*           0 => leave entries of D unchanged
*           1 => multiply each entry of D by 1 or -1 with probability .5
*
*  IDIST  - CHARACTER*1
*           On entry, IDIST specifies the type of distribution to be
*           used to generate a random matrix .
*           1 => UNIFORM( 0, 1 )
*           2 => UNIFORM( -1, 1 )
*           3 => NORMAL( 0, 1 )
*           Not modified.
*
*  ISEED  - INTEGER array, dimension ( 4 )
*           On entry ISEED specifies the seed of the random number
*           generator. The random number generator uses a
*           linear congruential sequence limited to small
*           integers, and so should produce machine independent
*           random numbers. The values of ISEED are changed on
*           exit, and can be used in the next call to DLATM1
*           to continue the same random number sequence.
*           Changed on exit.
*
*  D      - DOUBLE PRECISION array, dimension ( MIN( M , N ) )
*           Array to be computed according to MODE, COND and IRSIGN.
*           May be changed on exit if MODE is nonzero.
*
*  N      - INTEGER
*           Number of entries of D. Not modified.
*
*  INFO   - INTEGER
*            0  => normal termination
*           -1  => if MODE not in range -6 to 6
*           -2  => if MODE neither -6, 0 nor 6, and
*                  IRSIGN neither 0 nor 1
*           -3  => if MODE neither -6, 0 nor 6 and COND less than 1
*           -4  => if MODE equals 6 or -6 and IDIST not in range 1 to 3
*           -7  => if N negative
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ONE
      PARAMETER          ( ONE = 1.0D0 )
      DOUBLE PRECISION   HALF
      PARAMETER          ( HALF = 0.5D0 )
*     ..
*     .. Local Scalars ..
      INTEGER            I
      DOUBLE PRECISION   ALPHA, TEMP
*     ..
*     .. External Functions ..
      DOUBLE PRECISION   DLARAN
      EXTERNAL           DLARAN
*     ..
*     .. External Subroutines ..
      EXTERNAL           DLARNV, XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, DBLE, EXP, LOG
*     ..
*     .. Executable Statements ..
*
*     Decode and Test the input parameters. Initialize flags & seed.
*
      INFO = 0
*
*     Quick return if possible
*
      IF( N.EQ.0 )
     $   RETURN
*
*     Set INFO if an error
*
      IF( MODE.LT.-6 .OR. MODE.GT.6 ) THEN
         INFO = -1
      ELSE IF( ( MODE.NE.-6 .AND. MODE.NE.0 .AND. MODE.NE.6 ) .AND.
     $         ( IRSIGN.NE.0 .AND. IRSIGN.NE.1 ) ) THEN
         INFO = -2
      ELSE IF( ( MODE.NE.-6 .AND. MODE.NE.0 .AND. MODE.NE.6 ) .AND.
     $         COND.LT.ONE ) THEN
         INFO = -3
      ELSE IF( ( MODE.EQ.6 .OR. MODE.EQ.-6 ) .AND.
     $         ( IDIST.LT.1 .OR. IDIST.GT.3 ) ) THEN
         INFO = -4
      ELSE IF( N.LT.0 ) THEN
         INFO = -7
      END IF
*
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DLATM1', -INFO )
         RETURN
      END IF
*
*     Compute D according to COND and MODE
*
      IF( MODE.NE.0 ) THEN
         GO TO ( 10, 30, 50, 70, 90, 110 )ABS( MODE )
*
*        One large D value:
*
   10    CONTINUE
         DO 20 I = 1, N
            D( I ) = ONE / COND
   20    CONTINUE
         D( 1 ) = ONE
         GO TO 120
*
*        One small D value:
*
   30    CONTINUE
         DO 40 I = 1, N
            D( I ) = ONE
   40    CONTINUE
         D( N ) = ONE / COND
         GO TO 120
*
*        Exponentially distributed D values:
*
   50    CONTINUE
         D( 1 ) = ONE
         IF( N.GT.1 ) THEN
            ALPHA = COND**( -ONE / DBLE( N-1 ) )
            DO 60 I = 2, N
               D( I ) = ALPHA**( I-1 )
   60       CONTINUE
         END IF
         GO TO 120
*
*        Arithmetically distributed D values:
*
   70    CONTINUE
         D( 1 ) = ONE
         IF( N.GT.1 ) THEN
            TEMP = ONE / COND
            ALPHA = ( ONE-TEMP ) / DBLE( N-1 )
            DO 80 I = 2, N
               D( I ) = DBLE( N-I )*ALPHA + TEMP
   80       CONTINUE
         END IF
         GO TO 120
*
*        Randomly distributed D values on ( 1/COND , 1):
*
   90    CONTINUE
         ALPHA = LOG( ONE / COND )
         DO 100 I = 1, N
            D( I ) = EXP( ALPHA*DLARAN( ISEED ) )
  100    CONTINUE
         GO TO 120
*
*        Randomly distributed D values from IDIST
*
  110    CONTINUE
         CALL DLARNV( IDIST, ISEED, N, D )
*
  120    CONTINUE
*
*        If MODE neither -6 nor 0 nor 6, and IRSIGN = 1, assign
*        random signs to D
*
         IF( ( MODE.NE.-6 .AND. MODE.NE.0 .AND. MODE.NE.6 ) .AND.
     $       IRSIGN.EQ.1 ) THEN
            DO 130 I = 1, N
               TEMP = DLARAN( ISEED )
               IF( TEMP.GT.HALF )
     $            D( I ) = -D( I )
  130       CONTINUE
         END IF
*
*        Reverse if MODE < 0
*
         IF( MODE.LT.0 ) THEN
            DO 140 I = 1, N / 2
               TEMP = D( I )
               D( I ) = D( N+1-I )
               D( N+1-I ) = TEMP
  140       CONTINUE
         END IF
*
      END IF
*
      RETURN
*
*     End of DLATM1
*
      END
      SUBROUTINE DLATME( N, DIST, ISEED, D, MODE, COND, DMAX, EI, RSIGN,
     $                   UPPER, SIM, DS, MODES, CONDS, KL, KU, ANORM, A,
     $                   LDA, WORK, INFO )
*
*  -- LAPACK test routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     September 30, 1994
*
*     .. Scalar Arguments ..
      CHARACTER          DIST, RSIGN, SIM, UPPER
      INTEGER            INFO, KL, KU, LDA, MODE, MODES, N
      DOUBLE PRECISION   ANORM, COND, CONDS, DMAX
*     ..
*     .. Array Arguments ..
      CHARACTER          EI( * )
      INTEGER            ISEED( 4 )
      DOUBLE PRECISION   A( LDA, * ), D( * ), DS( * ), WORK( * )
*     ..
*
*  Purpose
*  =======
*
*     DLATME generates random non-symmetric square matrices with
*     specified eigenvalues for testing LAPACK programs.
*
*     DLATME operates by applying the following sequence of
*     operations:
*
*     1. Set the diagonal to D, where D may be input or
*          computed according to MODE, COND, DMAX, and RSIGN
*          as described below.
*
*     2. If complex conjugate pairs are desired (MODE=0 and EI(1)='R',
*          or MODE=5), certain pairs of adjacent elements of D are
*          interpreted as the real and complex parts of a complex
*          conjugate pair; A thus becomes block diagonal, with 1x1
*          and 2x2 blocks.
*
*     3. If UPPER='T', the upper triangle of A is set to random values
*          out of distribution DIST.
*
*     4. If SIM='T', A is multiplied on the left by a random matrix
*          X, whose singular values are specified by DS, MODES, and
*          CONDS, and on the right by X inverse.
*
*     5. If KL < N-1, the lower bandwidth is reduced to KL using
*          Householder transformations.  If KU < N-1, the upper
*          bandwidth is reduced to KU.
*
*     6. If ANORM is not negative, the matrix is scaled to have
*          maximum-element-norm ANORM.
*
*     (Note: since the matrix cannot be reduced beyond Hessenberg form,
*      no packing options are available.)
*
*  Arguments
*  =========
*
*  N      - INTEGER
*           The number of columns (or rows) of A. Not modified.
*
*  DIST   - CHARACTER*1
*           On entry, DIST specifies the type of distribution to be used
*           to generate the random eigen-/singular values, and for the
*           upper triangle (see UPPER).
*           'U' => UNIFORM( 0, 1 )  ( 'U' for uniform )
*           'S' => UNIFORM( -1, 1 ) ( 'S' for symmetric )
*           'N' => NORMAL( 0, 1 )   ( 'N' for normal )
*           Not modified.
*
*  ISEED  - INTEGER array, dimension ( 4 )
*           On entry ISEED specifies the seed of the random number
*           generator. They should lie between 0 and 4095 inclusive,
*           and ISEED(4) should be odd. The random number generator
*           uses a linear congruential sequence limited to small
*           integers, and so should produce machine independent
*           random numbers. The values of ISEED are changed on
*           exit, and can be used in the next call to DLATME
*           to continue the same random number sequence.
*           Changed on exit.
*
*  D      - DOUBLE PRECISION array, dimension ( N )
*           This array is used to specify the eigenvalues of A.  If
*           MODE=0, then D is assumed to contain the eigenvalues (but
*           see the description of EI), otherwise they will be
*           computed according to MODE, COND, DMAX, and RSIGN and
*           placed in D.
*           Modified if MODE is nonzero.
*
*  MODE   - INTEGER
*           On entry this describes how the eigenvalues are to
*           be specified:
*           MODE = 0 means use D (with EI) as input
*           MODE = 1 sets D(1)=1 and D(2:N)=1.0/COND
*           MODE = 2 sets D(1:N-1)=1 and D(N)=1.0/COND
*           MODE = 3 sets D(I)=COND**(-(I-1)/(N-1))
*           MODE = 4 sets D(i)=1 - (i-1)/(N-1)*(1 - 1/COND)
*           MODE = 5 sets D to random numbers in the range
*                    ( 1/COND , 1 ) such that their logarithms
*                    are uniformly distributed.  Each odd-even pair
*                    of elements will be either used as two real
*                    eigenvalues or as the real and imaginary part
*                    of a complex conjugate pair of eigenvalues;
*                    the choice of which is done is random, with
*                    50-50 probability, for each pair.
*           MODE = 6 set D to random numbers from same distribution
*                    as the rest of the matrix.
*           MODE < 0 has the same meaning as ABS(MODE), except that
*              the order of the elements of D is reversed.
*           Thus if MODE is between 1 and 4, D has entries ranging
*              from 1 to 1/COND, if between -1 and -4, D has entries
*              ranging from 1/COND to 1,
*           Not modified.
*
*  COND   - DOUBLE PRECISION
*           On entry, this is used as described under MODE above.
*           If used, it must be >= 1. Not modified.
*
*  DMAX   - DOUBLE PRECISION
*           If MODE is neither -6, 0 nor 6, the contents of D, as
*           computed according to MODE and COND, will be scaled by
*           DMAX / max(abs(D(i))).  Note that DMAX need not be
*           positive: if DMAX is negative (or zero), D will be
*           scaled by a negative number (or zero).
*           Not modified.
*
*  EI     - CHARACTER*1 array, dimension ( N )
*           If MODE is 0, and EI(1) is not ' ' (space character),
*           this array specifies which elements of D (on input) are
*           real eigenvalues and which are the real and imaginary parts
*           of a complex conjugate pair of eigenvalues.  The elements
*           of EI may then only have the values 'R' and 'I'.  If
*           EI(j)='R' and EI(j+1)='I', then the j-th eigenvalue is
*           CMPLX( D(j) , D(j+1) ), and the (j+1)-th is the complex
*           conjugate thereof.  If EI(j)=EI(j+1)='R', then the j-th
*           eigenvalue is D(j) (i.e., real).  EI(1) may not be 'I',
*           nor may two adjacent elements of EI both have the value 'I'.
*           If MODE is not 0, then EI is ignored.  If MODE is 0 and
*           EI(1)=' ', then the eigenvalues will all be real.
*           Not modified.
*
*  RSIGN  - CHARACTER*1
*           If MODE is not 0, 6, or -6, and RSIGN='T', then the
*           elements of D, as computed according to MODE and COND, will
*           be multiplied by a random sign (+1 or -1).  If RSIGN='F',
*           they will not be.  RSIGN may only have the values 'T' or
*           'F'.
*           Not modified.
*
*  UPPER  - CHARACTER*1
*           If UPPER='T', then the elements of A above the diagonal
*           (and above the 2x2 diagonal blocks, if A has complex
*           eigenvalues) will be set to random numbers out of DIST.
*           If UPPER='F', they will not.  UPPER may only have the
*           values 'T' or 'F'.
*           Not modified.
*
*  SIM    - CHARACTER*1
*           If SIM='T', then A will be operated on by a "similarity
*           transform", i.e., multiplied on the left by a matrix X and
*           on the right by X inverse.  X = U S V, where U and V are
*           random unitary matrices and S is a (diagonal) matrix of
*           singular values specified by DS, MODES, and CONDS.  If
*           SIM='F', then A will not be transformed.
*           Not modified.
*
*  DS     - DOUBLE PRECISION array, dimension ( N )
*           This array is used to specify the singular values of X,
*           in the same way that D specifies the eigenvalues of A.
*           If MODE=0, the DS contains the singular values, which
*           may not be zero.
*           Modified if MODE is nonzero.
*
*  MODES  - INTEGER
*  CONDS  - DOUBLE PRECISION
*           Same as MODE and COND, but for specifying the diagonal
*           of S.  MODES=-6 and +6 are not allowed (since they would
*           result in randomly ill-conditioned eigenvalues.)
*
*  KL     - INTEGER
*           This specifies the lower bandwidth of the  matrix.  KL=1
*           specifies upper Hessenberg form.  If KL is at least N-1,
*           then A will have full lower bandwidth.  KL must be at
*           least 1.
*           Not modified.
*
*  KU     - INTEGER
*           This specifies the upper bandwidth of the  matrix.  KU=1
*           specifies lower Hessenberg form.  If KU is at least N-1,
*           then A will have full upper bandwidth; if KU and KL
*           are both at least N-1, then A will be dense.  Only one of
*           KU and KL may be less than N-1.  KU must be at least 1.
*           Not modified.
*
*  ANORM  - DOUBLE PRECISION
*           If ANORM is not negative, then A will be scaled by a non-
*           negative real number to make the maximum-element-norm of A
*           to be ANORM.
*           Not modified.
*
*  A      - DOUBLE PRECISION array, dimension ( LDA, N )
*           On exit A is the desired test matrix.
*           Modified.
*
*  LDA    - INTEGER
*           LDA specifies the first dimension of A as declared in the
*           calling program.  LDA must be at least N.
*           Not modified.
*
*  WORK   - DOUBLE PRECISION array, dimension ( 3*N )
*           Workspace.
*           Modified.
*
*  INFO   - INTEGER
*           Error code.  On exit, INFO will be set to one of the
*           following values:
*             0 => normal return
*            -1 => N negative
*            -2 => DIST illegal string
*            -5 => MODE not in range -6 to 6
*            -6 => COND less than 1.0, and MODE neither -6, 0 nor 6
*            -8 => EI(1) is not ' ' or 'R', EI(j) is not 'R' or 'I', or
*                  two adjacent elements of EI are 'I'.
*            -9 => RSIGN is not 'T' or 'F'
*           -10 => UPPER is not 'T' or 'F'
*           -11 => SIM   is not 'T' or 'F'
*           -12 => MODES=0 and DS has a zero singular value.
*           -13 => MODES is not in the range -5 to 5.
*           -14 => MODES is nonzero and CONDS is less than 1.
*           -15 => KL is less than 1.
*           -16 => KU is less than 1, or KL and KU are both less than
*                  N-1.
*           -19 => LDA is less than N.
*            1  => Error return from DLATM1 (computing D)
*            2  => Cannot scale to DMAX (max. eigenvalue is 0)
*            3  => Error return from DLATM1 (computing DS)
*            4  => Error return from DLARGE
*            5  => Zero singular value from DLATM1.
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO
      PARAMETER          ( ZERO = 0.0D0 )
      DOUBLE PRECISION   ONE
      PARAMETER          ( ONE = 1.0D0 )
      DOUBLE PRECISION   HALF
      PARAMETER          ( HALF = 1.0D0 / 2.0D0 )
*     ..
*     .. Local Scalars ..
      LOGICAL            BADEI, BADS, USEEI
      INTEGER            I, IC, ICOLS, IDIST, IINFO, IR, IROWS, IRSIGN,
     $                   ISIM, IUPPER, J, JC, JCR, JR
      DOUBLE PRECISION   ALPHA, TAU, TEMP, XNORMS
*     ..
*     .. Local Arrays ..
      DOUBLE PRECISION   TEMPA( 1 )
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      DOUBLE PRECISION   DLANGE, DLARAN
      EXTERNAL           LSAME, DLANGE, DLARAN
*     ..
*     .. External Subroutines ..
      EXTERNAL           DCOPY, DGEMV, DGER, DLARFG, DLARGE, DLARNV,
     $                   DLASET, DLATM1, DSCAL, XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, MAX, MOD
*     ..
*     .. Executable Statements ..
*
*     1)      Decode and Test the input parameters.
*             Initialize flags & seed.
*
      INFO = 0
*
*     Quick return if possible
*
      IF( N.EQ.0 )
     $   RETURN
*
*     Decode DIST
*
      IF( LSAME( DIST, 'U' ) ) THEN
         IDIST = 1
      ELSE IF( LSAME( DIST, 'S' ) ) THEN
         IDIST = 2
      ELSE IF( LSAME( DIST, 'N' ) ) THEN
         IDIST = 3
      ELSE
         IDIST = -1
      END IF
*
*     Check EI
*
      USEEI = .TRUE.
      BADEI = .FALSE.
      IF( LSAME( EI( 1 ), ' ' ) .OR. MODE.NE.0 ) THEN
         USEEI = .FALSE.
      ELSE
         IF( LSAME( EI( 1 ), 'R' ) ) THEN
            DO 10 J = 2, N
               IF( LSAME( EI( J ), 'I' ) ) THEN
                  IF( LSAME( EI( J-1 ), 'I' ) )
     $               BADEI = .TRUE.
               ELSE
                  IF( .NOT.LSAME( EI( J ), 'R' ) )
     $               BADEI = .TRUE.
               END IF
   10       CONTINUE
         ELSE
            BADEI = .TRUE.
         END IF
      END IF
*
*     Decode RSIGN
*
      IF( LSAME( RSIGN, 'T' ) ) THEN
         IRSIGN = 1
      ELSE IF( LSAME( RSIGN, 'F' ) ) THEN
         IRSIGN = 0
      ELSE
         IRSIGN = -1
      END IF
*
*     Decode UPPER
*
      IF( LSAME( UPPER, 'T' ) ) THEN
         IUPPER = 1
      ELSE IF( LSAME( UPPER, 'F' ) ) THEN
         IUPPER = 0
      ELSE
         IUPPER = -1
      END IF
*
*     Decode SIM
*
      IF( LSAME( SIM, 'T' ) ) THEN
         ISIM = 1
      ELSE IF( LSAME( SIM, 'F' ) ) THEN
         ISIM = 0
      ELSE
         ISIM = -1
      END IF
*
*     Check DS, if MODES=0 and ISIM=1
*
      BADS = .FALSE.
      IF( MODES.EQ.0 .AND. ISIM.EQ.1 ) THEN
         DO 20 J = 1, N
            IF( DS( J ).EQ.ZERO )
     $         BADS = .TRUE.
   20    CONTINUE
      END IF
*
*     Set INFO if an error
*
      IF( N.LT.0 ) THEN
         INFO = -1
      ELSE IF( IDIST.EQ.-1 ) THEN
         INFO = -2
      ELSE IF( ABS( MODE ).GT.6 ) THEN
         INFO = -5
      ELSE IF( ( MODE.NE.0 .AND. ABS( MODE ).NE.6 ) .AND. COND.LT.ONE )
     $          THEN
         INFO = -6
      ELSE IF( BADEI ) THEN
         INFO = -8
      ELSE IF( IRSIGN.EQ.-1 ) THEN
         INFO = -9
      ELSE IF( IUPPER.EQ.-1 ) THEN
         INFO = -10
      ELSE IF( ISIM.EQ.-1 ) THEN
         INFO = -11
      ELSE IF( BADS ) THEN
         INFO = -12
      ELSE IF( ISIM.EQ.1 .AND. ABS( MODES ).GT.5 ) THEN
         INFO = -13
      ELSE IF( ISIM.EQ.1 .AND. MODES.NE.0 .AND. CONDS.LT.ONE ) THEN
         INFO = -14
      ELSE IF( KL.LT.1 ) THEN
         INFO = -15
      ELSE IF( KU.LT.1 .OR. ( KU.LT.N-1 .AND. KL.LT.N-1 ) ) THEN
         INFO = -16
      ELSE IF( LDA.LT.MAX( 1, N ) ) THEN
         INFO = -19
      END IF
*
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DLATME', -INFO )
         RETURN
      END IF
*
*     Initialize random number generator
*
      DO 30 I = 1, 4
         ISEED( I ) = MOD( ABS( ISEED( I ) ), 4096 )
   30 CONTINUE
*
      IF( MOD( ISEED( 4 ), 2 ).NE.1 )
     $   ISEED( 4 ) = ISEED( 4 ) + 1
*
*     2)      Set up diagonal of A
*
*             Compute D according to COND and MODE
*
      CALL DLATM1( MODE, COND, IRSIGN, IDIST, ISEED, D, N, IINFO )
      IF( IINFO.NE.0 ) THEN
         INFO = 1
         RETURN
      END IF
      IF( MODE.NE.0 .AND. ABS( MODE ).NE.6 ) THEN
*
*        Scale by DMAX
*
         TEMP = ABS( D( 1 ) )
         DO 40 I = 2, N
            TEMP = MAX( TEMP, ABS( D( I ) ) )
   40    CONTINUE
*
         IF( TEMP.GT.ZERO ) THEN
            ALPHA = DMAX / TEMP
         ELSE IF( DMAX.NE.ZERO ) THEN
            INFO = 2
            RETURN
         ELSE
            ALPHA = ZERO
         END IF
*
         CALL DSCAL( N, ALPHA, D, 1 )
*
      END IF
*
      CALL DLASET( 'Full', N, N, ZERO, ZERO, A, LDA )
      CALL DCOPY( N, D, 1, A, LDA+1 )
*
*     Set up complex conjugate pairs
*
      IF( MODE.EQ.0 ) THEN
         IF( USEEI ) THEN
            DO 50 J = 2, N
               IF( LSAME( EI( J ), 'I' ) ) THEN
                  A( J-1, J ) = A( J, J )
                  A( J, J-1 ) = -A( J, J )
                  A( J, J ) = A( J-1, J-1 )
               END IF
   50       CONTINUE
         END IF
*
      ELSE IF( ABS( MODE ).EQ.5 ) THEN
*
         DO 60 J = 2, N, 2
            IF( DLARAN( ISEED ).GT.HALF ) THEN
               A( J-1, J ) = A( J, J )
               A( J, J-1 ) = -A( J, J )
               A( J, J ) = A( J-1, J-1 )
            END IF
   60    CONTINUE
      END IF
*
*     3)      If UPPER='T', set upper triangle of A to random numbers.
*             (but don't modify the corners of 2x2 blocks.)
*
      IF( IUPPER.NE.0 ) THEN
         DO 70 JC = 2, N
            IF( A( JC-1, JC ).NE.ZERO ) THEN
               JR = JC - 2
            ELSE
               JR = JC - 1
            END IF
            CALL DLARNV( IDIST, ISEED, JR, A( 1, JC ) )
   70    CONTINUE
      END IF
*
*     4)      If SIM='T', apply similarity transformation.
*
*                                -1
*             Transform is  X A X  , where X = U S V, thus
*
*             it is  U S V A V' (1/S) U'
*
      IF( ISIM.NE.0 ) THEN
*
*        Compute S (singular values of the eigenvector matrix)
*        according to CONDS and MODES
*
         CALL DLATM1( MODES, CONDS, 0, 0, ISEED, DS, N, IINFO )
         IF( IINFO.NE.0 ) THEN
            INFO = 3
            RETURN
         END IF
*
*        Multiply by V and V'
*
         CALL DLARGE( N, A, LDA, ISEED, WORK, IINFO )
         IF( IINFO.NE.0 ) THEN
            INFO = 4
            RETURN
         END IF
*
*        Multiply by S and (1/S)
*
         DO 80 J = 1, N
            CALL DSCAL( N, DS( J ), A( J, 1 ), LDA )
            IF( DS( J ).NE.ZERO ) THEN
               CALL DSCAL( N, ONE / DS( J ), A( 1, J ), 1 )
            ELSE
               INFO = 5
               RETURN
            END IF
   80    CONTINUE
*
*        Multiply by U and U'
*
         CALL DLARGE( N, A, LDA, ISEED, WORK, IINFO )
         IF( IINFO.NE.0 ) THEN
            INFO = 4
            RETURN
         END IF
      END IF
*
*     5)      Reduce the bandwidth.
*
      IF( KL.LT.N-1 ) THEN
*
*        Reduce bandwidth -- kill column
*
         DO 90 JCR = KL + 1, N - 1
            IC = JCR - KL
            IROWS = N + 1 - JCR
            ICOLS = N + KL - JCR
*
            CALL DCOPY( IROWS, A( JCR, IC ), 1, WORK, 1 )
            XNORMS = WORK( 1 )
            CALL DLARFG( IROWS, XNORMS, WORK( 2 ), 1, TAU )
            WORK( 1 ) = ONE
*
            CALL DGEMV( 'T', IROWS, ICOLS, ONE, A( JCR, IC+1 ), LDA,
     $                  WORK, 1, ZERO, WORK( IROWS+1 ), 1 )
            CALL DGER( IROWS, ICOLS, -TAU, WORK, 1, WORK( IROWS+1 ), 1,
     $                 A( JCR, IC+1 ), LDA )
*
            CALL DGEMV( 'N', N, IROWS, ONE, A( 1, JCR ), LDA, WORK, 1,
     $                  ZERO, WORK( IROWS+1 ), 1 )
            CALL DGER( N, IROWS, -TAU, WORK( IROWS+1 ), 1, WORK, 1,
     $                 A( 1, JCR ), LDA )
*
            A( JCR, IC ) = XNORMS
            CALL DLASET( 'Full', IROWS-1, 1, ZERO, ZERO, A( JCR+1, IC ),
     $                   LDA )
   90    CONTINUE
      ELSE IF( KU.LT.N-1 ) THEN
*
*        Reduce upper bandwidth -- kill a row at a time.
*
         DO 100 JCR = KU + 1, N - 1
            IR = JCR - KU
            IROWS = N + KU - JCR
            ICOLS = N + 1 - JCR
*
            CALL DCOPY( ICOLS, A( IR, JCR ), LDA, WORK, 1 )
            XNORMS = WORK( 1 )
            CALL DLARFG( ICOLS, XNORMS, WORK( 2 ), 1, TAU )
            WORK( 1 ) = ONE
*
            CALL DGEMV( 'N', IROWS, ICOLS, ONE, A( IR+1, JCR ), LDA,
     $                  WORK, 1, ZERO, WORK( ICOLS+1 ), 1 )
            CALL DGER( IROWS, ICOLS, -TAU, WORK( ICOLS+1 ), 1, WORK, 1,
     $                 A( IR+1, JCR ), LDA )
*
            CALL DGEMV( 'C', ICOLS, N, ONE, A( JCR, 1 ), LDA, WORK, 1,
     $                  ZERO, WORK( ICOLS+1 ), 1 )
            CALL DGER( ICOLS, N, -TAU, WORK, 1, WORK( ICOLS+1 ), 1,
     $                 A( JCR, 1 ), LDA )
*
            A( IR, JCR ) = XNORMS
            CALL DLASET( 'Full', 1, ICOLS-1, ZERO, ZERO, A( IR, JCR+1 ),
     $                   LDA )
  100    CONTINUE
      END IF
*
*     Scale the matrix to have norm ANORM
*
      IF( ANORM.GE.ZERO ) THEN
         TEMP = DLANGE( 'M', N, N, A, LDA, TEMPA )
         IF( TEMP.GT.ZERO ) THEN
            ALPHA = ANORM / TEMP
            DO 110 J = 1, N
               CALL DSCAL( N, ALPHA, A( 1, J ), 1 )
  110       CONTINUE
         END IF
      END IF
*
      RETURN
*
*     End of DLATME
*
      END
      SUBROUTINE DLATMR( M, N, DIST, ISEED, SYM, D, MODE, COND, DMAX,
     $                   RSIGN, GRADE, DL, MODEL, CONDL, DR, MODER,
     $                   CONDR, PIVTNG, IPIVOT, KL, KU, SPARSE, ANORM,
     $                   PACK, A, LDA, IWORK, INFO )
*
*  -- LAPACK test routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     February 29, 1992
*
*     .. Scalar Arguments ..
      CHARACTER          DIST, GRADE, PACK, PIVTNG, RSIGN, SYM
      INTEGER            INFO, KL, KU, LDA, M, MODE, MODEL, MODER, N
      DOUBLE PRECISION   ANORM, COND, CONDL, CONDR, DMAX, SPARSE
*     ..
*     .. Array Arguments ..
      INTEGER            IPIVOT( * ), ISEED( 4 ), IWORK( * )
      DOUBLE PRECISION   A( LDA, * ), D( * ), DL( * ), DR( * )
*     ..
*
*  Purpose
*  =======
*
*     DLATMR generates random matrices of various types for testing
*     LAPACK programs.
*
*     DLATMR operates by applying the following sequence of
*     operations:
*
*       Generate a matrix A with random entries of distribution DIST
*          which is symmetric if SYM='S', and nonsymmetric
*          if SYM='N'.
*
*       Set the diagonal to D, where D may be input or
*          computed according to MODE, COND, DMAX and RSIGN
*          as described below.
*
*       Grade the matrix, if desired, from the left and/or right
*          as specified by GRADE. The inputs DL, MODEL, CONDL, DR,
*          MODER and CONDR also determine the grading as described
*          below.
*
*       Permute, if desired, the rows and/or columns as specified by
*          PIVTNG and IPIVOT.
*
*       Set random entries to zero, if desired, to get a random sparse
*          matrix as specified by SPARSE.
*
*       Make A a band matrix, if desired, by zeroing out the matrix
*          outside a band of lower bandwidth KL and upper bandwidth KU.
*
*       Scale A, if desired, to have maximum entry ANORM.
*
*       Pack the matrix if desired. Options specified by PACK are:
*          no packing
*          zero out upper half (if symmetric)
*          zero out lower half (if symmetric)
*          store the upper half columnwise (if symmetric or
*              square upper triangular)
*          store the lower half columnwise (if symmetric or
*              square lower triangular)
*              same as upper half rowwise if symmetric
*          store the lower triangle in banded format (if symmetric)
*          store the upper triangle in banded format (if symmetric)
*          store the entire matrix in banded format
*
*     Note: If two calls to DLATMR differ only in the PACK parameter,
*           they will generate mathematically equivalent matrices.
*
*           If two calls to DLATMR both have full bandwidth (KL = M-1
*           and KU = N-1), and differ only in the PIVTNG and PACK
*           parameters, then the matrices generated will differ only
*           in the order of the rows and/or columns, and otherwise
*           contain the same data. This consistency cannot be and
*           is not maintained with less than full bandwidth.
*
*  Arguments
*  =========
*
*  M      - INTEGER
*           Number of rows of A. Not modified.
*
*  N      - INTEGER
*           Number of columns of A. Not modified.
*
*  DIST   - CHARACTER*1
*           On entry, DIST specifies the type of distribution to be used
*           to generate a random matrix .
*           'U' => UNIFORM( 0, 1 )  ( 'U' for uniform )
*           'S' => UNIFORM( -1, 1 ) ( 'S' for symmetric )
*           'N' => NORMAL( 0, 1 )   ( 'N' for normal )
*           Not modified.
*
*  ISEED  - INTEGER array, dimension (4)
*           On entry ISEED specifies the seed of the random number
*           generator. They should lie between 0 and 4095 inclusive,
*           and ISEED(4) should be odd. The random number generator
*           uses a linear congruential sequence limited to small
*           integers, and so should produce machine independent
*           random numbers. The values of ISEED are changed on
*           exit, and can be used in the next call to DLATMR
*           to continue the same random number sequence.
*           Changed on exit.
*
*  SYM    - CHARACTER*1
*           If SYM='S' or 'H', generated matrix is symmetric.
*           If SYM='N', generated matrix is nonsymmetric.
*           Not modified.
*
*  D      - DOUBLE PRECISION array, dimension (min(M,N))
*           On entry this array specifies the diagonal entries
*           of the diagonal of A.  D may either be specified
*           on entry, or set according to MODE and COND as described
*           below. May be changed on exit if MODE is nonzero.
*
*  MODE   - INTEGER
*           On entry describes how D is to be used:
*           MODE = 0 means use D as input
*           MODE = 1 sets D(1)=1 and D(2:N)=1.0/COND
*           MODE = 2 sets D(1:N-1)=1 and D(N)=1.0/COND
*           MODE = 3 sets D(I)=COND**(-(I-1)/(N-1))
*           MODE = 4 sets D(i)=1 - (i-1)/(N-1)*(1 - 1/COND)
*           MODE = 5 sets D to random numbers in the range
*                    ( 1/COND , 1 ) such that their logarithms
*                    are uniformly distributed.
*           MODE = 6 set D to random numbers from same distribution
*                    as the rest of the matrix.
*           MODE < 0 has the same meaning as ABS(MODE), except that
*              the order of the elements of D is reversed.
*           Thus if MODE is positive, D has entries ranging from
*              1 to 1/COND, if negative, from 1/COND to 1,
*           Not modified.
*
*  COND   - DOUBLE PRECISION
*           On entry, used as described under MODE above.
*           If used, it must be >= 1. Not modified.
*
*  DMAX   - DOUBLE PRECISION
*           If MODE neither -6, 0 nor 6, the diagonal is scaled by
*           DMAX / max(abs(D(i))), so that maximum absolute entry
*           of diagonal is abs(DMAX). If DMAX is negative (or zero),
*           diagonal will be scaled by a negative number (or zero).
*
*  RSIGN  - CHARACTER*1
*           If MODE neither -6, 0 nor 6, specifies sign of diagonal
*           as follows:
*           'T' => diagonal entries are multiplied by 1 or -1
*                  with probability .5
*           'F' => diagonal unchanged
*           Not modified.
*
*  GRADE  - CHARACTER*1
*           Specifies grading of matrix as follows:
*           'N'  => no grading
*           'L'  => matrix premultiplied by diag( DL )
*                   (only if matrix nonsymmetric)
*           'R'  => matrix postmultiplied by diag( DR )
*                   (only if matrix nonsymmetric)
*           'B'  => matrix premultiplied by diag( DL ) and
*                         postmultiplied by diag( DR )
*                   (only if matrix nonsymmetric)
*           'S' or 'H'  => matrix premultiplied by diag( DL ) and
*                          postmultiplied by diag( DL )
*                          ('S' for symmetric, or 'H' for Hermitian)
*           'E'  => matrix premultiplied by diag( DL ) and
*                         postmultiplied by inv( diag( DL ) )
*                         ( 'E' for eigenvalue invariance)
*                   (only if matrix nonsymmetric)
*                   Note: if GRADE='E', then M must equal N.
*           Not modified.
*
*  DL     - DOUBLE PRECISION array, dimension (M)
*           If MODEL=0, then on entry this array specifies the diagonal
*           entries of a diagonal matrix used as described under GRADE
*           above. If MODEL is not zero, then DL will be set according
*           to MODEL and CONDL, analogous to the way D is set according
*           to MODE and COND (except there is no DMAX parameter for DL).
*           If GRADE='E', then DL cannot have zero entries.
*           Not referenced if GRADE = 'N' or 'R'. Changed on exit.
*
*  MODEL  - INTEGER
*           This specifies how the diagonal array DL is to be computed,
*           just as MODE specifies how D is to be computed.
*           Not modified.
*
*  CONDL  - DOUBLE PRECISION
*           When MODEL is not zero, this specifies the condition number
*           of the computed DL.  Not modified.
*
*  DR     - DOUBLE PRECISION array, dimension (N)
*           If MODER=0, then on entry this array specifies the diagonal
*           entries of a diagonal matrix used as described under GRADE
*           above. If MODER is not zero, then DR will be set according
*           to MODER and CONDR, analogous to the way D is set according
*           to MODE and COND (except there is no DMAX parameter for DR).
*           Not referenced if GRADE = 'N', 'L', 'H', 'S' or 'E'.
*           Changed on exit.
*
*  MODER  - INTEGER
*           This specifies how the diagonal array DR is to be computed,
*           just as MODE specifies how D is to be computed.
*           Not modified.
*
*  CONDR  - DOUBLE PRECISION
*           When MODER is not zero, this specifies the condition number
*           of the computed DR.  Not modified.
*
*  PIVTNG - CHARACTER*1
*           On entry specifies pivoting permutations as follows:
*           'N' or ' ' => none.
*           'L' => left or row pivoting (matrix must be nonsymmetric).
*           'R' => right or column pivoting (matrix must be
*                  nonsymmetric).
*           'B' or 'F' => both or full pivoting, i.e., on both sides.
*                         In this case, M must equal N
*
*           If two calls to DLATMR both have full bandwidth (KL = M-1
*           and KU = N-1), and differ only in the PIVTNG and PACK
*           parameters, then the matrices generated will differ only
*           in the order of the rows and/or columns, and otherwise
*           contain the same data. This consistency cannot be
*           maintained with less than full bandwidth.
*
*  IPIVOT - INTEGER array, dimension (N or M)
*           This array specifies the permutation used.  After the
*           basic matrix is generated, the rows, columns, or both
*           are permuted.   If, say, row pivoting is selected, DLATMR
*           starts with the *last* row and interchanges the M-th and
*           IPIVOT(M)-th rows, then moves to the next-to-last row,
*           interchanging the (M-1)-th and the IPIVOT(M-1)-th rows,
*           and so on.  In terms of "2-cycles", the permutation is
*           (1 IPIVOT(1)) (2 IPIVOT(2)) ... (M IPIVOT(M))
*           where the rightmost cycle is applied first.  This is the
*           *inverse* of the effect of pivoting in LINPACK.  The idea
*           is that factoring (with pivoting) an identity matrix
*           which has been inverse-pivoted in this way should
*           result in a pivot vector identical to IPIVOT.
*           Not referenced if PIVTNG = 'N'. Not modified.
*
*  SPARSE - DOUBLE PRECISION
*           On entry specifies the sparsity of the matrix if a sparse
*           matrix is to be generated. SPARSE should lie between
*           0 and 1. To generate a sparse matrix, for each matrix entry
*           a uniform ( 0, 1 ) random number x is generated and
*           compared to SPARSE; if x is larger the matrix entry
*           is unchanged and if x is smaller the entry is set
*           to zero. Thus on the average a fraction SPARSE of the
*           entries will be set to zero.
*           Not modified.
*
*  KL     - INTEGER
*           On entry specifies the lower bandwidth of the  matrix. For
*           example, KL=0 implies upper triangular, KL=1 implies upper
*           Hessenberg, and KL at least M-1 implies the matrix is not
*           banded. Must equal KU if matrix is symmetric.
*           Not modified.
*
*  KU     - INTEGER
*           On entry specifies the upper bandwidth of the  matrix. For
*           example, KU=0 implies lower triangular, KU=1 implies lower
*           Hessenberg, and KU at least N-1 implies the matrix is not
*           banded. Must equal KL if matrix is symmetric.
*           Not modified.
*
*  ANORM  - DOUBLE PRECISION
*           On entry specifies maximum entry of output matrix
*           (output matrix will by multiplied by a constant so that
*           its largest absolute entry equal ANORM)
*           if ANORM is nonnegative. If ANORM is negative no scaling
*           is done. Not modified.
*
*  PACK   - CHARACTER*1
*           On entry specifies packing of matrix as follows:
*           'N' => no packing
*           'U' => zero out all subdiagonal entries (if symmetric)
*           'L' => zero out all superdiagonal entries (if symmetric)
*           'C' => store the upper triangle columnwise
*                  (only if matrix symmetric or square upper triangular)
*           'R' => store the lower triangle columnwise
*                  (only if matrix symmetric or square lower triangular)
*                  (same as upper half rowwise if symmetric)
*           'B' => store the lower triangle in band storage scheme
*                  (only if matrix symmetric)
*           'Q' => store the upper triangle in band storage scheme
*                  (only if matrix symmetric)
*           'Z' => store the entire matrix in band storage scheme
*                      (pivoting can be provided for by using this
*                      option to store A in the trailing rows of
*                      the allocated storage)
*
*           Using these options, the various LAPACK packed and banded
*           storage schemes can be obtained:
*           GB               - use 'Z'
*           PB, SB or TB     - use 'B' or 'Q'
*           PP, SP or TP     - use 'C' or 'R'
*
*           If two calls to DLATMR differ only in the PACK parameter,
*           they will generate mathematically equivalent matrices.
*           Not modified.
*
*  A      - DOUBLE PRECISION array, dimension (LDA,N)
*           On exit A is the desired test matrix. Only those
*           entries of A which are significant on output
*           will be referenced (even if A is in packed or band
*           storage format). The 'unoccupied corners' of A in
*           band format will be zeroed out.
*
*  LDA    - INTEGER
*           on entry LDA specifies the first dimension of A as
*           declared in the calling program.
*           If PACK='N', 'U' or 'L', LDA must be at least max ( 1, M ).
*           If PACK='C' or 'R', LDA must be at least 1.
*           If PACK='B', or 'Q', LDA must be MIN ( KU+1, N )
*           If PACK='Z', LDA must be at least KUU+KLL+1, where
*           KUU = MIN ( KU, N-1 ) and KLL = MIN ( KL, N-1 )
*           Not modified.
*
*  IWORK  - INTEGER array, dimension ( N or M)
*           Workspace. Not referenced if PIVTNG = 'N'. Changed on exit.
*
*  INFO   - INTEGER
*           Error parameter on exit:
*             0 => normal return
*            -1 => M negative or unequal to N and SYM='S' or 'H'
*            -2 => N negative
*            -3 => DIST illegal string
*            -5 => SYM illegal string
*            -7 => MODE not in range -6 to 6
*            -8 => COND less than 1.0, and MODE neither -6, 0 nor 6
*           -10 => MODE neither -6, 0 nor 6 and RSIGN illegal string
*           -11 => GRADE illegal string, or GRADE='E' and
*                  M not equal to N, or GRADE='L', 'R', 'B' or 'E' and
*                  SYM = 'S' or 'H'
*           -12 => GRADE = 'E' and DL contains zero
*           -13 => MODEL not in range -6 to 6 and GRADE= 'L', 'B', 'H',
*                  'S' or 'E'
*           -14 => CONDL less than 1.0, GRADE='L', 'B', 'H', 'S' or 'E',
*                  and MODEL neither -6, 0 nor 6
*           -16 => MODER not in range -6 to 6 and GRADE= 'R' or 'B'
*           -17 => CONDR less than 1.0, GRADE='R' or 'B', and
*                  MODER neither -6, 0 nor 6
*           -18 => PIVTNG illegal string, or PIVTNG='B' or 'F' and
*                  M not equal to N, or PIVTNG='L' or 'R' and SYM='S'
*                  or 'H'
*           -19 => IPIVOT contains out of range number and
*                  PIVTNG not equal to 'N'
*           -20 => KL negative
*           -21 => KU negative, or SYM='S' or 'H' and KU not equal to KL
*           -22 => SPARSE not in range 0. to 1.
*           -24 => PACK illegal string, or PACK='U', 'L', 'B' or 'Q'
*                  and SYM='N', or PACK='C' and SYM='N' and either KL
*                  not equal to 0 or N not equal to M, or PACK='R' and
*                  SYM='N', and either KU not equal to 0 or N not equal
*                  to M
*           -26 => LDA too small
*             1 => Error return from DLATM1 (computing D)
*             2 => Cannot scale diagonal to DMAX (max. entry is 0)
*             3 => Error return from DLATM1 (computing DL)
*             4 => Error return from DLATM1 (computing DR)
*             5 => ANORM is positive, but matrix constructed prior to
*                  attempting to scale it to have norm ANORM, is zero
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO
      PARAMETER          ( ZERO = 0.0D0 )
      DOUBLE PRECISION   ONE
      PARAMETER          ( ONE = 1.0D0 )
*     ..
*     .. Local Scalars ..
      LOGICAL            BADPVT, DZERO, FULBND
      INTEGER            I, IDIST, IGRADE, IISUB, IPACK, IPVTNG, IRSIGN,
     $                   ISUB, ISYM, J, JJSUB, JSUB, K, KLL, KUU, MNMIN,
     $                   MNSUB, MXSUB, NPVTS
      DOUBLE PRECISION   ALPHA, ONORM, TEMP
*     ..
*     .. Local Arrays ..
      DOUBLE PRECISION   TEMPA( 1 )
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      DOUBLE PRECISION   DLANGB, DLANGE, DLANSB, DLANSP, DLANSY, DLATM2,
     $                   DLATM3
      EXTERNAL           LSAME, DLANGB, DLANGE, DLANSB, DLANSP, DLANSY,
     $                   DLATM2, DLATM3
*     ..
*     .. External Subroutines ..
      EXTERNAL           DLATM1, DSCAL, XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, MAX, MIN, MOD
*     ..
*     .. Executable Statements ..
*
*     1)      Decode and Test the input parameters.
*             Initialize flags & seed.
*
      INFO = 0
*
*     Quick return if possible
*
      IF( M.EQ.0 .OR. N.EQ.0 )
     $   RETURN
*
*     Decode DIST
*
      IF( LSAME( DIST, 'U' ) ) THEN
         IDIST = 1
      ELSE IF( LSAME( DIST, 'S' ) ) THEN
         IDIST = 2
      ELSE IF( LSAME( DIST, 'N' ) ) THEN
         IDIST = 3
      ELSE
         IDIST = -1
      END IF
*
*     Decode SYM
*
      IF( LSAME( SYM, 'S' ) ) THEN
         ISYM = 0
      ELSE IF( LSAME( SYM, 'N' ) ) THEN
         ISYM = 1
      ELSE IF( LSAME( SYM, 'H' ) ) THEN
         ISYM = 0
      ELSE
         ISYM = -1
      END IF
*
*     Decode RSIGN
*
      IF( LSAME( RSIGN, 'F' ) ) THEN
         IRSIGN = 0
      ELSE IF( LSAME( RSIGN, 'T' ) ) THEN
         IRSIGN = 1
      ELSE
         IRSIGN = -1
      END IF
*
*     Decode PIVTNG
*
      IF( LSAME( PIVTNG, 'N' ) ) THEN
         IPVTNG = 0
      ELSE IF( LSAME( PIVTNG, ' ' ) ) THEN
         IPVTNG = 0
      ELSE IF( LSAME( PIVTNG, 'L' ) ) THEN
         IPVTNG = 1
         NPVTS = M
      ELSE IF( LSAME( PIVTNG, 'R' ) ) THEN
         IPVTNG = 2
         NPVTS = N
      ELSE IF( LSAME( PIVTNG, 'B' ) ) THEN
         IPVTNG = 3
         NPVTS = MIN( N, M )
      ELSE IF( LSAME( PIVTNG, 'F' ) ) THEN
         IPVTNG = 3
         NPVTS = MIN( N, M )
      ELSE
         IPVTNG = -1
      END IF
*
*     Decode GRADE
*
      IF( LSAME( GRADE, 'N' ) ) THEN
         IGRADE = 0
      ELSE IF( LSAME( GRADE, 'L' ) ) THEN
         IGRADE = 1
      ELSE IF( LSAME( GRADE, 'R' ) ) THEN
         IGRADE = 2
      ELSE IF( LSAME( GRADE, 'B' ) ) THEN
         IGRADE = 3
      ELSE IF( LSAME( GRADE, 'E' ) ) THEN
         IGRADE = 4
      ELSE IF( LSAME( GRADE, 'H' ) .OR. LSAME( GRADE, 'S' ) ) THEN
         IGRADE = 5
      ELSE
         IGRADE = -1
      END IF
*
*     Decode PACK
*
      IF( LSAME( PACK, 'N' ) ) THEN
         IPACK = 0
      ELSE IF( LSAME( PACK, 'U' ) ) THEN
         IPACK = 1
      ELSE IF( LSAME( PACK, 'L' ) ) THEN
         IPACK = 2
      ELSE IF( LSAME( PACK, 'C' ) ) THEN
         IPACK = 3
      ELSE IF( LSAME( PACK, 'R' ) ) THEN
         IPACK = 4
      ELSE IF( LSAME( PACK, 'B' ) ) THEN
         IPACK = 5
      ELSE IF( LSAME( PACK, 'Q' ) ) THEN
         IPACK = 6
      ELSE IF( LSAME( PACK, 'Z' ) ) THEN
         IPACK = 7
      ELSE
         IPACK = -1
      END IF
*
*     Set certain internal parameters
*
      MNMIN = MIN( M, N )
      KLL = MIN( KL, M-1 )
      KUU = MIN( KU, N-1 )
*
*     If inv(DL) is used, check to see if DL has a zero entry.
*
      DZERO = .FALSE.
      IF( IGRADE.EQ.4 .AND. MODEL.EQ.0 ) THEN
         DO 10 I = 1, M
            IF( DL( I ).EQ.ZERO )
     $         DZERO = .TRUE.
   10    CONTINUE
      END IF
*
*     Check values in IPIVOT
*
      BADPVT = .FALSE.
      IF( IPVTNG.GT.0 ) THEN
         DO 20 J = 1, NPVTS
            IF( IPIVOT( J ).LE.0 .OR. IPIVOT( J ).GT.NPVTS )
     $         BADPVT = .TRUE.
   20    CONTINUE
      END IF
*
*     Set INFO if an error
*
      IF( M.LT.0 ) THEN
         INFO = -1
      ELSE IF( M.NE.N .AND. ISYM.EQ.0 ) THEN
         INFO = -1
      ELSE IF( N.LT.0 ) THEN
         INFO = -2
      ELSE IF( IDIST.EQ.-1 ) THEN
         INFO = -3
      ELSE IF( ISYM.EQ.-1 ) THEN
         INFO = -5
      ELSE IF( MODE.LT.-6 .OR. MODE.GT.6 ) THEN
         INFO = -7
      ELSE IF( ( MODE.NE.-6 .AND. MODE.NE.0 .AND. MODE.NE.6 ) .AND.
     $         COND.LT.ONE ) THEN
         INFO = -8
      ELSE IF( ( MODE.NE.-6 .AND. MODE.NE.0 .AND. MODE.NE.6 ) .AND.
     $         IRSIGN.EQ.-1 ) THEN
         INFO = -10
      ELSE IF( IGRADE.EQ.-1 .OR. ( IGRADE.EQ.4 .AND. M.NE.N ) .OR.
     $         ( ( IGRADE.GE.1 .AND. IGRADE.LE.4 ) .AND. ISYM.EQ.0 ) )
     $          THEN
         INFO = -11
      ELSE IF( IGRADE.EQ.4 .AND. DZERO ) THEN
         INFO = -12
      ELSE IF( ( IGRADE.EQ.1 .OR. IGRADE.EQ.3 .OR. IGRADE.EQ.4 .OR.
     $         IGRADE.EQ.5 ) .AND. ( MODEL.LT.-6 .OR. MODEL.GT.6 ) )
     $          THEN
         INFO = -13
      ELSE IF( ( IGRADE.EQ.1 .OR. IGRADE.EQ.3 .OR. IGRADE.EQ.4 .OR.
     $         IGRADE.EQ.5 ) .AND. ( MODEL.NE.-6 .AND. MODEL.NE.0 .AND.
     $         MODEL.NE.6 ) .AND. CONDL.LT.ONE ) THEN
         INFO = -14
      ELSE IF( ( IGRADE.EQ.2 .OR. IGRADE.EQ.3 ) .AND.
     $         ( MODER.LT.-6 .OR. MODER.GT.6 ) ) THEN
         INFO = -16
      ELSE IF( ( IGRADE.EQ.2 .OR. IGRADE.EQ.3 ) .AND.
     $         ( MODER.NE.-6 .AND. MODER.NE.0 .AND. MODER.NE.6 ) .AND.
     $         CONDR.LT.ONE ) THEN
         INFO = -17
      ELSE IF( IPVTNG.EQ.-1 .OR. ( IPVTNG.EQ.3 .AND. M.NE.N ) .OR.
     $         ( ( IPVTNG.EQ.1 .OR. IPVTNG.EQ.2 ) .AND. ISYM.EQ.0 ) )
     $          THEN
         INFO = -18
      ELSE IF( IPVTNG.NE.0 .AND. BADPVT ) THEN
         INFO = -19
      ELSE IF( KL.LT.0 ) THEN
         INFO = -20
      ELSE IF( KU.LT.0 .OR. ( ISYM.EQ.0 .AND. KL.NE.KU ) ) THEN
         INFO = -21
      ELSE IF( SPARSE.LT.ZERO .OR. SPARSE.GT.ONE ) THEN
         INFO = -22
      ELSE IF( IPACK.EQ.-1 .OR. ( ( IPACK.EQ.1 .OR. IPACK.EQ.2 .OR.
     $         IPACK.EQ.5 .OR. IPACK.EQ.6 ) .AND. ISYM.EQ.1 ) .OR.
     $         ( IPACK.EQ.3 .AND. ISYM.EQ.1 .AND. ( KL.NE.0 .OR. M.NE.
     $         N ) ) .OR. ( IPACK.EQ.4 .AND. ISYM.EQ.1 .AND. ( KU.NE.
     $         0 .OR. M.NE.N ) ) ) THEN
         INFO = -24
      ELSE IF( ( ( IPACK.EQ.0 .OR. IPACK.EQ.1 .OR. IPACK.EQ.2 ) .AND.
     $         LDA.LT.MAX( 1, M ) ) .OR. ( ( IPACK.EQ.3 .OR. IPACK.EQ.
     $         4 ) .AND. LDA.LT.1 ) .OR. ( ( IPACK.EQ.5 .OR. IPACK.EQ.
     $         6 ) .AND. LDA.LT.KUU+1 ) .OR.
     $         ( IPACK.EQ.7 .AND. LDA.LT.KLL+KUU+1 ) ) THEN
         INFO = -26
      END IF
*
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DLATMR', -INFO )
         RETURN
      END IF
*
*     Decide if we can pivot consistently
*
      FULBND = .FALSE.
      IF( KUU.EQ.N-1 .AND. KLL.EQ.M-1 )
     $   FULBND = .TRUE.
*
*     Initialize random number generator
*
      DO 30 I = 1, 4
         ISEED( I ) = MOD( ABS( ISEED( I ) ), 4096 )
   30 CONTINUE
*
      ISEED( 4 ) = 2*( ISEED( 4 ) / 2 ) + 1
*
*     2)      Set up D, DL, and DR, if indicated.
*
*             Compute D according to COND and MODE
*
      CALL DLATM1( MODE, COND, IRSIGN, IDIST, ISEED, D, MNMIN, INFO )
      IF( INFO.NE.0 ) THEN
         INFO = 1
         RETURN
      END IF
      IF( MODE.NE.0 .AND. MODE.NE.-6 .AND. MODE.NE.6 ) THEN
*
*        Scale by DMAX
*
         TEMP = ABS( D( 1 ) )
         DO 40 I = 2, MNMIN
            TEMP = MAX( TEMP, ABS( D( I ) ) )
   40    CONTINUE
         IF( TEMP.EQ.ZERO .AND. DMAX.NE.ZERO ) THEN
            INFO = 2
            RETURN
         END IF
         IF( TEMP.NE.ZERO ) THEN
            ALPHA = DMAX / TEMP
         ELSE
            ALPHA = ONE
         END IF
         DO 50 I = 1, MNMIN
            D( I ) = ALPHA*D( I )
   50    CONTINUE
*
      END IF
*
*     Compute DL if grading set
*
      IF( IGRADE.EQ.1 .OR. IGRADE.EQ.3 .OR. IGRADE.EQ.4 .OR. IGRADE.EQ.
     $    5 ) THEN
         CALL DLATM1( MODEL, CONDL, 0, IDIST, ISEED, DL, M, INFO )
         IF( INFO.NE.0 ) THEN
            INFO = 3
            RETURN
         END IF
      END IF
*
*     Compute DR if grading set
*
      IF( IGRADE.EQ.2 .OR. IGRADE.EQ.3 ) THEN
         CALL DLATM1( MODER, CONDR, 0, IDIST, ISEED, DR, N, INFO )
         IF( INFO.NE.0 ) THEN
            INFO = 4
            RETURN
         END IF
      END IF
*
*     3)     Generate IWORK if pivoting
*
      IF( IPVTNG.GT.0 ) THEN
         DO 60 I = 1, NPVTS
            IWORK( I ) = I
   60    CONTINUE
         IF( FULBND ) THEN
            DO 70 I = 1, NPVTS
               K = IPIVOT( I )
               J = IWORK( I )
               IWORK( I ) = IWORK( K )
               IWORK( K ) = J
   70       CONTINUE
         ELSE
            DO 80 I = NPVTS, 1, -1
               K = IPIVOT( I )
               J = IWORK( I )
               IWORK( I ) = IWORK( K )
               IWORK( K ) = J
   80       CONTINUE
         END IF
      END IF
*
*     4)      Generate matrices for each kind of PACKing
*             Always sweep matrix columnwise (if symmetric, upper
*             half only) so that matrix generated does not depend
*             on PACK
*
      IF( FULBND ) THEN
*
*        Use DLATM3 so matrices generated with differing PIVOTing only
*        differ only in the order of their rows and/or columns.
*
         IF( IPACK.EQ.0 ) THEN
            IF( ISYM.EQ.0 ) THEN
               DO 100 J = 1, N
                  DO 90 I = 1, J
                     TEMP = DLATM3( M, N, I, J, ISUB, JSUB, KL, KU,
     $                      IDIST, ISEED, D, IGRADE, DL, DR, IPVTNG,
     $                      IWORK, SPARSE )
                     A( ISUB, JSUB ) = TEMP
                     A( JSUB, ISUB ) = TEMP
   90             CONTINUE
  100          CONTINUE
            ELSE IF( ISYM.EQ.1 ) THEN
               DO 120 J = 1, N
                  DO 110 I = 1, M
                     TEMP = DLATM3( M, N, I, J, ISUB, JSUB, KL, KU,
     $                      IDIST, ISEED, D, IGRADE, DL, DR, IPVTNG,
     $                      IWORK, SPARSE )
                     A( ISUB, JSUB ) = TEMP
  110             CONTINUE
  120          CONTINUE
            END IF
*
         ELSE IF( IPACK.EQ.1 ) THEN
*
            DO 140 J = 1, N
               DO 130 I = 1, J
                  TEMP = DLATM3( M, N, I, J, ISUB, JSUB, KL, KU, IDIST,
     $                   ISEED, D, IGRADE, DL, DR, IPVTNG, IWORK,
     $                   SPARSE )
                  MNSUB = MIN( ISUB, JSUB )
                  MXSUB = MAX( ISUB, JSUB )
                  A( MNSUB, MXSUB ) = TEMP
                  IF( MNSUB.NE.MXSUB )
     $               A( MXSUB, MNSUB ) = ZERO
  130          CONTINUE
  140       CONTINUE
*
         ELSE IF( IPACK.EQ.2 ) THEN
*
            DO 160 J = 1, N
               DO 150 I = 1, J
                  TEMP = DLATM3( M, N, I, J, ISUB, JSUB, KL, KU, IDIST,
     $                   ISEED, D, IGRADE, DL, DR, IPVTNG, IWORK,
     $                   SPARSE )
                  MNSUB = MIN( ISUB, JSUB )
                  MXSUB = MAX( ISUB, JSUB )
                  A( MXSUB, MNSUB ) = TEMP
                  IF( MNSUB.NE.MXSUB )
     $               A( MNSUB, MXSUB ) = ZERO
  150          CONTINUE
  160       CONTINUE
*
         ELSE IF( IPACK.EQ.3 ) THEN
*
            DO 180 J = 1, N
               DO 170 I = 1, J
                  TEMP = DLATM3( M, N, I, J, ISUB, JSUB, KL, KU, IDIST,
     $                   ISEED, D, IGRADE, DL, DR, IPVTNG, IWORK,
     $                   SPARSE )
*
*                 Compute K = location of (ISUB,JSUB) entry in packed
*                 array
*
                  MNSUB = MIN( ISUB, JSUB )
                  MXSUB = MAX( ISUB, JSUB )
                  K = MXSUB*( MXSUB-1 ) / 2 + MNSUB
*
*                 Convert K to (IISUB,JJSUB) location
*
                  JJSUB = ( K-1 ) / LDA + 1
                  IISUB = K - LDA*( JJSUB-1 )
*
                  A( IISUB, JJSUB ) = TEMP
  170          CONTINUE
  180       CONTINUE
*
         ELSE IF( IPACK.EQ.4 ) THEN
*
            DO 200 J = 1, N
               DO 190 I = 1, J
                  TEMP = DLATM3( M, N, I, J, ISUB, JSUB, KL, KU, IDIST,
     $                   ISEED, D, IGRADE, DL, DR, IPVTNG, IWORK,
     $                   SPARSE )
*
*                 Compute K = location of (I,J) entry in packed array
*
                  MNSUB = MIN( ISUB, JSUB )
                  MXSUB = MAX( ISUB, JSUB )
                  IF( MNSUB.EQ.1 ) THEN
                     K = MXSUB
                  ELSE
                     K = N*( N+1 ) / 2 - ( N-MNSUB+1 )*( N-MNSUB+2 ) /
     $                   2 + MXSUB - MNSUB + 1
                  END IF
*
*                 Convert K to (IISUB,JJSUB) location
*
                  JJSUB = ( K-1 ) / LDA + 1
                  IISUB = K - LDA*( JJSUB-1 )
*
                  A( IISUB, JJSUB ) = TEMP
  190          CONTINUE
  200       CONTINUE
*
         ELSE IF( IPACK.EQ.5 ) THEN
*
            DO 220 J = 1, N
               DO 210 I = J - KUU, J
                  IF( I.LT.1 ) THEN
                     A( J-I+1, I+N ) = ZERO
                  ELSE
                     TEMP = DLATM3( M, N, I, J, ISUB, JSUB, KL, KU,
     $                      IDIST, ISEED, D, IGRADE, DL, DR, IPVTNG,
     $                      IWORK, SPARSE )
                     MNSUB = MIN( ISUB, JSUB )
                     MXSUB = MAX( ISUB, JSUB )
                     A( MXSUB-MNSUB+1, MNSUB ) = TEMP
                  END IF
  210          CONTINUE
  220       CONTINUE
*
         ELSE IF( IPACK.EQ.6 ) THEN
*
            DO 240 J = 1, N
               DO 230 I = J - KUU, J
                  TEMP = DLATM3( M, N, I, J, ISUB, JSUB, KL, KU, IDIST,
     $                   ISEED, D, IGRADE, DL, DR, IPVTNG, IWORK,
     $                   SPARSE )
                  MNSUB = MIN( ISUB, JSUB )
                  MXSUB = MAX( ISUB, JSUB )
                  A( MNSUB-MXSUB+KUU+1, MXSUB ) = TEMP
  230          CONTINUE
  240       CONTINUE
*
         ELSE IF( IPACK.EQ.7 ) THEN
*
            IF( ISYM.EQ.0 ) THEN
               DO 260 J = 1, N
                  DO 250 I = J - KUU, J
                     TEMP = DLATM3( M, N, I, J, ISUB, JSUB, KL, KU,
     $                      IDIST, ISEED, D, IGRADE, DL, DR, IPVTNG,
     $                      IWORK, SPARSE )
                     MNSUB = MIN( ISUB, JSUB )
                     MXSUB = MAX( ISUB, JSUB )
                     A( MNSUB-MXSUB+KUU+1, MXSUB ) = TEMP
                     IF( I.LT.1 )
     $                  A( J-I+1+KUU, I+N ) = ZERO
                     IF( I.GE.1 .AND. MNSUB.NE.MXSUB )
     $                  A( MXSUB-MNSUB+1+KUU, MNSUB ) = TEMP
  250             CONTINUE
  260          CONTINUE
            ELSE IF( ISYM.EQ.1 ) THEN
               DO 280 J = 1, N
                  DO 270 I = J - KUU, J + KLL
                     TEMP = DLATM3( M, N, I, J, ISUB, JSUB, KL, KU,
     $                      IDIST, ISEED, D, IGRADE, DL, DR, IPVTNG,
     $                      IWORK, SPARSE )
                     A( ISUB-JSUB+KUU+1, JSUB ) = TEMP
  270             CONTINUE
  280          CONTINUE
            END IF
*
         END IF
*
      ELSE
*
*        Use DLATM2
*
         IF( IPACK.EQ.0 ) THEN
            IF( ISYM.EQ.0 ) THEN
               DO 300 J = 1, N
                  DO 290 I = 1, J
                     A( I, J ) = DLATM2( M, N, I, J, KL, KU, IDIST,
     $                           ISEED, D, IGRADE, DL, DR, IPVTNG,
     $                           IWORK, SPARSE )
                     A( J, I ) = A( I, J )
  290             CONTINUE
  300          CONTINUE
            ELSE IF( ISYM.EQ.1 ) THEN
               DO 320 J = 1, N
                  DO 310 I = 1, M
                     A( I, J ) = DLATM2( M, N, I, J, KL, KU, IDIST,
     $                           ISEED, D, IGRADE, DL, DR, IPVTNG,
     $                           IWORK, SPARSE )
  310             CONTINUE
  320          CONTINUE
            END IF
*
         ELSE IF( IPACK.EQ.1 ) THEN
*
            DO 340 J = 1, N
               DO 330 I = 1, J
                  A( I, J ) = DLATM2( M, N, I, J, KL, KU, IDIST, ISEED,
     $                        D, IGRADE, DL, DR, IPVTNG, IWORK, SPARSE )
                  IF( I.NE.J )
     $               A( J, I ) = ZERO
  330          CONTINUE
  340       CONTINUE
*
         ELSE IF( IPACK.EQ.2 ) THEN
*
            DO 360 J = 1, N
               DO 350 I = 1, J
                  A( J, I ) = DLATM2( M, N, I, J, KL, KU, IDIST, ISEED,
     $                        D, IGRADE, DL, DR, IPVTNG, IWORK, SPARSE )
                  IF( I.NE.J )
     $               A( I, J ) = ZERO
  350          CONTINUE
  360       CONTINUE
*
         ELSE IF( IPACK.EQ.3 ) THEN
*
            ISUB = 0
            JSUB = 1
            DO 380 J = 1, N
               DO 370 I = 1, J
                  ISUB = ISUB + 1
                  IF( ISUB.GT.LDA ) THEN
                     ISUB = 1
                     JSUB = JSUB + 1
                  END IF
                  A( ISUB, JSUB ) = DLATM2( M, N, I, J, KL, KU, IDIST,
     $                              ISEED, D, IGRADE, DL, DR, IPVTNG,
     $                              IWORK, SPARSE )
  370          CONTINUE
  380       CONTINUE
*
         ELSE IF( IPACK.EQ.4 ) THEN
*
            IF( ISYM.EQ.0 ) THEN
               DO 400 J = 1, N
                  DO 390 I = 1, J
*
*                    Compute K = location of (I,J) entry in packed array
*
                     IF( I.EQ.1 ) THEN
                        K = J
                     ELSE
                        K = N*( N+1 ) / 2 - ( N-I+1 )*( N-I+2 ) / 2 +
     $                      J - I + 1
                     END IF
*
*                    Convert K to (ISUB,JSUB) location
*
                     JSUB = ( K-1 ) / LDA + 1
                     ISUB = K - LDA*( JSUB-1 )
*
                     A( ISUB, JSUB ) = DLATM2( M, N, I, J, KL, KU,
     $                                 IDIST, ISEED, D, IGRADE, DL, DR,
     $                                 IPVTNG, IWORK, SPARSE )
  390             CONTINUE
  400          CONTINUE
            ELSE
               ISUB = 0
               JSUB = 1
               DO 420 J = 1, N
                  DO 410 I = J, M
                     ISUB = ISUB + 1
                     IF( ISUB.GT.LDA ) THEN
                        ISUB = 1
                        JSUB = JSUB + 1
                     END IF
                     A( ISUB, JSUB ) = DLATM2( M, N, I, J, KL, KU,
     $                                 IDIST, ISEED, D, IGRADE, DL, DR,
     $                                 IPVTNG, IWORK, SPARSE )
  410             CONTINUE
  420          CONTINUE
            END IF
*
         ELSE IF( IPACK.EQ.5 ) THEN
*
            DO 440 J = 1, N
               DO 430 I = J - KUU, J
                  IF( I.LT.1 ) THEN
                     A( J-I+1, I+N ) = ZERO
                  ELSE
                     A( J-I+1, I ) = DLATM2( M, N, I, J, KL, KU, IDIST,
     $                               ISEED, D, IGRADE, DL, DR, IPVTNG,
     $                               IWORK, SPARSE )
                  END IF
  430          CONTINUE
  440       CONTINUE
*
         ELSE IF( IPACK.EQ.6 ) THEN
*
            DO 460 J = 1, N
               DO 450 I = J - KUU, J
                  A( I-J+KUU+1, J ) = DLATM2( M, N, I, J, KL, KU, IDIST,
     $                                ISEED, D, IGRADE, DL, DR, IPVTNG,
     $                                IWORK, SPARSE )
  450          CONTINUE
  460       CONTINUE
*
         ELSE IF( IPACK.EQ.7 ) THEN
*
            IF( ISYM.EQ.0 ) THEN
               DO 480 J = 1, N
                  DO 470 I = J - KUU, J
                     A( I-J+KUU+1, J ) = DLATM2( M, N, I, J, KL, KU,
     $                                   IDIST, ISEED, D, IGRADE, DL,
     $                                   DR, IPVTNG, IWORK, SPARSE )
                     IF( I.LT.1 )
     $                  A( J-I+1+KUU, I+N ) = ZERO
                     IF( I.GE.1 .AND. I.NE.J )
     $                  A( J-I+1+KUU, I ) = A( I-J+KUU+1, J )
  470             CONTINUE
  480          CONTINUE
            ELSE IF( ISYM.EQ.1 ) THEN
               DO 500 J = 1, N
                  DO 490 I = J - KUU, J + KLL
                     A( I-J+KUU+1, J ) = DLATM2( M, N, I, J, KL, KU,
     $                                   IDIST, ISEED, D, IGRADE, DL,
     $                                   DR, IPVTNG, IWORK, SPARSE )
  490             CONTINUE
  500          CONTINUE
            END IF
*
         END IF
*
      END IF
*
*     5)      Scaling the norm
*
      IF( IPACK.EQ.0 ) THEN
         ONORM = DLANGE( 'M', M, N, A, LDA, TEMPA )
      ELSE IF( IPACK.EQ.1 ) THEN
         ONORM = DLANSY( 'M', 'U', N, A, LDA, TEMPA )
      ELSE IF( IPACK.EQ.2 ) THEN
         ONORM = DLANSY( 'M', 'L', N, A, LDA, TEMPA )
      ELSE IF( IPACK.EQ.3 ) THEN
         ONORM = DLANSP( 'M', 'U', N, A, TEMPA )
      ELSE IF( IPACK.EQ.4 ) THEN
         ONORM = DLANSP( 'M', 'L', N, A, TEMPA )
      ELSE IF( IPACK.EQ.5 ) THEN
         ONORM = DLANSB( 'M', 'L', N, KLL, A, LDA, TEMPA )
      ELSE IF( IPACK.EQ.6 ) THEN
         ONORM = DLANSB( 'M', 'U', N, KUU, A, LDA, TEMPA )
      ELSE IF( IPACK.EQ.7 ) THEN
         ONORM = DLANGB( 'M', N, KLL, KUU, A, LDA, TEMPA )
      END IF
*
      IF( ANORM.GE.ZERO ) THEN
*
         IF( ANORM.GT.ZERO .AND. ONORM.EQ.ZERO ) THEN
*
*           Desired scaling impossible
*
            INFO = 5
            RETURN
*
         ELSE IF( ( ANORM.GT.ONE .AND. ONORM.LT.ONE ) .OR.
     $            ( ANORM.LT.ONE .AND. ONORM.GT.ONE ) ) THEN
*
*           Scale carefully to avoid over / underflow
*
            IF( IPACK.LE.2 ) THEN
               DO 510 J = 1, N
                  CALL DSCAL( M, ONE / ONORM, A( 1, J ), 1 )
                  CALL DSCAL( M, ANORM, A( 1, J ), 1 )
  510          CONTINUE
*
            ELSE IF( IPACK.EQ.3 .OR. IPACK.EQ.4 ) THEN
*
               CALL DSCAL( N*( N+1 ) / 2, ONE / ONORM, A, 1 )
               CALL DSCAL( N*( N+1 ) / 2, ANORM, A, 1 )
*
            ELSE IF( IPACK.GE.5 ) THEN
*
               DO 520 J = 1, N
                  CALL DSCAL( KLL+KUU+1, ONE / ONORM, A( 1, J ), 1 )
                  CALL DSCAL( KLL+KUU+1, ANORM, A( 1, J ), 1 )
  520          CONTINUE
*
            END IF
*
         ELSE
*
*           Scale straightforwardly
*
            IF( IPACK.LE.2 ) THEN
               DO 530 J = 1, N
                  CALL DSCAL( M, ANORM / ONORM, A( 1, J ), 1 )
  530          CONTINUE
*
            ELSE IF( IPACK.EQ.3 .OR. IPACK.EQ.4 ) THEN
*
               CALL DSCAL( N*( N+1 ) / 2, ANORM / ONORM, A, 1 )
*
            ELSE IF( IPACK.GE.5 ) THEN
*
               DO 540 J = 1, N
                  CALL DSCAL( KLL+KUU+1, ANORM / ONORM, A( 1, J ), 1 )
  540          CONTINUE
            END IF
*
         END IF
*
      END IF
*
*     End of DLATMR
*
      END
      SUBROUTINE DLATMS( M, N, DIST, ISEED, SYM, D, MODE, COND, DMAX,
     $                   KL, KU, PACK, A, LDA, WORK, INFO )
*
*  -- LAPACK test routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     September 30, 1994
*
*     .. Scalar Arguments ..
      CHARACTER          DIST, PACK, SYM
      INTEGER            INFO, KL, KU, LDA, M, MODE, N
      DOUBLE PRECISION   COND, DMAX
*     ..
*     .. Array Arguments ..
      INTEGER            ISEED( 4 )
      DOUBLE PRECISION   A( LDA, * ), D( * ), WORK( * )
*     ..
*
*  Purpose
*  =======
*
*     DLATMS generates random matrices with specified singular values
*     (or symmetric/hermitian with specified eigenvalues)
*     for testing LAPACK programs.
*
*     DLATMS operates by applying the following sequence of
*     operations:
*
*       Set the diagonal to D, where D may be input or
*          computed according to MODE, COND, DMAX, and SYM
*          as described below.
*
*       Generate a matrix with the appropriate band structure, by one
*          of two methods:
*
*       Method A:
*           Generate a dense M x N matrix by multiplying D on the left
*               and the right by random unitary matrices, then:
*
*           Reduce the bandwidth according to KL and KU, using
*           Householder transformations.
*
*       Method B:
*           Convert the bandwidth-0 (i.e., diagonal) matrix to a
*               bandwidth-1 matrix using Givens rotations, "chasing"
*               out-of-band elements back, much as in QR; then
*               convert the bandwidth-1 to a bandwidth-2 matrix, etc.
*               Note that for reasonably small bandwidths (relative to
*               M and N) this requires less storage, as a dense matrix
*               is not generated.  Also, for symmetric matrices, only
*               one triangle is generated.
*
*       Method A is chosen if the bandwidth is a large fraction of the
*           order of the matrix, and LDA is at least M (so a dense
*           matrix can be stored.)  Method B is chosen if the bandwidth
*           is small (< 1/2 N for symmetric, < .3 N+M for
*           non-symmetric), or LDA is less than M and not less than the
*           bandwidth.
*
*       Pack the matrix if desired. Options specified by PACK are:
*          no packing
*          zero out upper half (if symmetric)
*          zero out lower half (if symmetric)
*          store the upper half columnwise (if symmetric or upper
*                triangular)
*          store the lower half columnwise (if symmetric or lower
*                triangular)
*          store the lower triangle in banded format (if symmetric
*                or lower triangular)
*          store the upper triangle in banded format (if symmetric
*                or upper triangular)
*          store the entire matrix in banded format
*       If Method B is chosen, and band format is specified, then the
*          matrix will be generated in the band format, so no repacking
*          will be necessary.
*
*  Arguments
*  =========
*
*  M      - INTEGER
*           The number of rows of A. Not modified.
*
*  N      - INTEGER
*           The number of columns of A. Not modified.
*
*  DIST   - CHARACTER*1
*           On entry, DIST specifies the type of distribution to be used
*           to generate the random eigen-/singular values.
*           'U' => UNIFORM( 0, 1 )  ( 'U' for uniform )
*           'S' => UNIFORM( -1, 1 ) ( 'S' for symmetric )
*           'N' => NORMAL( 0, 1 )   ( 'N' for normal )
*           Not modified.
*
*  ISEED  - INTEGER array, dimension ( 4 )
*           On entry ISEED specifies the seed of the random number
*           generator. They should lie between 0 and 4095 inclusive,
*           and ISEED(4) should be odd. The random number generator
*           uses a linear congruential sequence limited to small
*           integers, and so should produce machine independent
*           random numbers. The values of ISEED are changed on
*           exit, and can be used in the next call to DLATMS
*           to continue the same random number sequence.
*           Changed on exit.
*
*  SYM    - CHARACTER*1
*           If SYM='S' or 'H', the generated matrix is symmetric, with
*             eigenvalues specified by D, COND, MODE, and DMAX; they
*             may be positive, negative, or zero.
*           If SYM='P', the generated matrix is symmetric, with
*             eigenvalues (= singular values) specified by D, COND,
*             MODE, and DMAX; they will not be negative.
*           If SYM='N', the generated matrix is nonsymmetric, with
*             singular values specified by D, COND, MODE, and DMAX;
*             they will not be negative.
*           Not modified.
*
*  D      - DOUBLE PRECISION array, dimension ( MIN( M , N ) )
*           This array is used to specify the singular values or
*           eigenvalues of A (see SYM, above.)  If MODE=0, then D is
*           assumed to contain the singular/eigenvalues, otherwise
*           they will be computed according to MODE, COND, and DMAX,
*           and placed in D.
*           Modified if MODE is nonzero.
*
*  MODE   - INTEGER
*           On entry this describes how the singular/eigenvalues are to
*           be specified:
*           MODE = 0 means use D as input
*           MODE = 1 sets D(1)=1 and D(2:N)=1.0/COND
*           MODE = 2 sets D(1:N-1)=1 and D(N)=1.0/COND
*           MODE = 3 sets D(I)=COND**(-(I-1)/(N-1))
*           MODE = 4 sets D(i)=1 - (i-1)/(N-1)*(1 - 1/COND)
*           MODE = 5 sets D to random numbers in the range
*                    ( 1/COND , 1 ) such that their logarithms
*                    are uniformly distributed.
*           MODE = 6 set D to random numbers from same distribution
*                    as the rest of the matrix.
*           MODE < 0 has the same meaning as ABS(MODE), except that
*              the order of the elements of D is reversed.
*           Thus if MODE is positive, D has entries ranging from
*              1 to 1/COND, if negative, from 1/COND to 1,
*           If SYM='S' or 'H', and MODE is neither 0, 6, nor -6, then
*              the elements of D will also be multiplied by a random
*              sign (i.e., +1 or -1.)
*           Not modified.
*
*  COND   - DOUBLE PRECISION
*           On entry, this is used as described under MODE above.
*           If used, it must be >= 1. Not modified.
*
*  DMAX   - DOUBLE PRECISION
*           If MODE is neither -6, 0 nor 6, the contents of D, as
*           computed according to MODE and COND, will be scaled by
*           DMAX / max(abs(D(i))); thus, the maximum absolute eigen- or
*           singular value (which is to say the norm) will be abs(DMAX).
*           Note that DMAX need not be positive: if DMAX is negative
*           (or zero), D will be scaled by a negative number (or zero).
*           Not modified.
*
*  KL     - INTEGER
*           This specifies the lower bandwidth of the  matrix. For
*           example, KL=0 implies upper triangular, KL=1 implies upper
*           Hessenberg, and KL being at least M-1 means that the matrix
*           has full lower bandwidth.  KL must equal KU if the matrix
*           is symmetric.
*           Not modified.
*
*  KU     - INTEGER
*           This specifies the upper bandwidth of the  matrix. For
*           example, KU=0 implies lower triangular, KU=1 implies lower
*           Hessenberg, and KU being at least N-1 means that the matrix
*           has full upper bandwidth.  KL must equal KU if the matrix
*           is symmetric.
*           Not modified.
*
*  PACK   - CHARACTER*1
*           This specifies packing of matrix as follows:
*           'N' => no packing
*           'U' => zero out all subdiagonal entries (if symmetric)
*           'L' => zero out all superdiagonal entries (if symmetric)
*           'C' => store the upper triangle columnwise
*                  (only if the matrix is symmetric or upper triangular)
*           'R' => store the lower triangle columnwise
*                  (only if the matrix is symmetric or lower triangular)
*           'B' => store the lower triangle in band storage scheme
*                  (only if matrix symmetric or lower triangular)
*           'Q' => store the upper triangle in band storage scheme
*                  (only if matrix symmetric or upper triangular)
*           'Z' => store the entire matrix in band storage scheme
*                      (pivoting can be provided for by using this
*                      option to store A in the trailing rows of
*                      the allocated storage)
*
*           Using these options, the various LAPACK packed and banded
*           storage schemes can be obtained:
*           GB               - use 'Z'
*           PB, SB or TB     - use 'B' or 'Q'
*           PP, SP or TP     - use 'C' or 'R'
*
*           If two calls to DLATMS differ only in the PACK parameter,
*           they will generate mathematically equivalent matrices.
*           Not modified.
*
*  A      - DOUBLE PRECISION array, dimension ( LDA, N )
*           On exit A is the desired test matrix.  A is first generated
*           in full (unpacked) form, and then packed, if so specified
*           by PACK.  Thus, the first M elements of the first N
*           columns will always be modified.  If PACK specifies a
*           packed or banded storage scheme, all LDA elements of the
*           first N columns will be modified; the elements of the
*           array which do not correspond to elements of the generated
*           matrix are set to zero.
*           Modified.
*
*  LDA    - INTEGER
*           LDA specifies the first dimension of A as declared in the
*           calling program.  If PACK='N', 'U', 'L', 'C', or 'R', then
*           LDA must be at least M.  If PACK='B' or 'Q', then LDA must
*           be at least MIN( KL, M-1) (which is equal to MIN(KU,N-1)).
*           If PACK='Z', LDA must be large enough to hold the packed
*           array: MIN( KU, N-1) + MIN( KL, M-1) + 1.
*           Not modified.
*
*  WORK   - DOUBLE PRECISION array, dimension ( 3*MAX( N , M ) )
*           Workspace.
*           Modified.
*
*  INFO   - INTEGER
*           Error code.  On exit, INFO will be set to one of the
*           following values:
*             0 => normal return
*            -1 => M negative or unequal to N and SYM='S', 'H', or 'P'
*            -2 => N negative
*            -3 => DIST illegal string
*            -5 => SYM illegal string
*            -7 => MODE not in range -6 to 6
*            -8 => COND less than 1.0, and MODE neither -6, 0 nor 6
*           -10 => KL negative
*           -11 => KU negative, or SYM='S' or 'H' and KU not equal to KL
*           -12 => PACK illegal string, or PACK='U' or 'L', and SYM='N';
*                  or PACK='C' or 'Q' and SYM='N' and KL is not zero;
*                  or PACK='R' or 'B' and SYM='N' and KU is not zero;
*                  or PACK='U', 'L', 'C', 'R', 'B', or 'Q', and M is not
*                  N.
*           -14 => LDA is less than M, or PACK='Z' and LDA is less than
*                  MIN(KU,N-1) + MIN(KL,M-1) + 1.
*            1  => Error return from DLATM1
*            2  => Cannot scale to DMAX (max. sing. value is 0)
*            3  => Error return from DLAGGE or SLAGSY
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO
      PARAMETER          ( ZERO = 0.0D0 )
      DOUBLE PRECISION   ONE
      PARAMETER          ( ONE = 1.0D0 )
      DOUBLE PRECISION   TWOPI
      PARAMETER          ( TWOPI = 6.2831853071795864769252867663D+0 )
*     ..
*     .. Local Scalars ..
      LOGICAL            GIVENS, ILEXTR, ILTEMP, TOPDWN
      INTEGER            I, IC, ICOL, IDIST, IENDCH, IINFO, IL, ILDA,
     $                   IOFFG, IOFFST, IPACK, IPACKG, IR, IR1, IR2,
     $                   IROW, IRSIGN, ISKEW, ISYM, ISYMPK, J, JC, JCH,
     $                   JKL, JKU, JR, K, LLB, MINLDA, MNMIN, MR, NC,
     $                   UUB
      DOUBLE PRECISION   ALPHA, ANGLE, C, DUMMY, EXTRA, S, TEMP
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      DOUBLE PRECISION   DLARND
      EXTERNAL           LSAME, DLARND
*     ..
*     .. External Subroutines ..
      EXTERNAL           DCOPY, DLAGGE, DLAGSY, DLAROT, DLARTG, DLASET,
     $                   DLATM1, DSCAL, XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, COS, DBLE, MAX, MIN, MOD, SIN
*     ..
*     .. Executable Statements ..
*
*     1)      Decode and Test the input parameters.
*             Initialize flags & seed.
*
      INFO = 0
*
*     Quick return if possible
*
      IF( M.EQ.0 .OR. N.EQ.0 )
     $   RETURN
*
*     Decode DIST
*
      IF( LSAME( DIST, 'U' ) ) THEN
         IDIST = 1
      ELSE IF( LSAME( DIST, 'S' ) ) THEN
         IDIST = 2
      ELSE IF( LSAME( DIST, 'N' ) ) THEN
         IDIST = 3
      ELSE
         IDIST = -1
      END IF
*
*     Decode SYM
*
      IF( LSAME( SYM, 'N' ) ) THEN
         ISYM = 1
         IRSIGN = 0
      ELSE IF( LSAME( SYM, 'P' ) ) THEN
         ISYM = 2
         IRSIGN = 0
      ELSE IF( LSAME( SYM, 'S' ) ) THEN
         ISYM = 2
         IRSIGN = 1
      ELSE IF( LSAME( SYM, 'H' ) ) THEN
         ISYM = 2
         IRSIGN = 1
      ELSE
         ISYM = -1
      END IF
*
*     Decode PACK
*
      ISYMPK = 0
      IF( LSAME( PACK, 'N' ) ) THEN
         IPACK = 0
      ELSE IF( LSAME( PACK, 'U' ) ) THEN
         IPACK = 1
         ISYMPK = 1
      ELSE IF( LSAME( PACK, 'L' ) ) THEN
         IPACK = 2
         ISYMPK = 1
      ELSE IF( LSAME( PACK, 'C' ) ) THEN
         IPACK = 3
         ISYMPK = 2
      ELSE IF( LSAME( PACK, 'R' ) ) THEN
         IPACK = 4
         ISYMPK = 3
      ELSE IF( LSAME( PACK, 'B' ) ) THEN
         IPACK = 5
         ISYMPK = 3
      ELSE IF( LSAME( PACK, 'Q' ) ) THEN
         IPACK = 6
         ISYMPK = 2
      ELSE IF( LSAME( PACK, 'Z' ) ) THEN
         IPACK = 7
      ELSE
         IPACK = -1
      END IF
*
*     Set certain internal parameters
*
      MNMIN = MIN( M, N )
      LLB = MIN( KL, M-1 )
      UUB = MIN( KU, N-1 )
      MR = MIN( M, N+LLB )
      NC = MIN( N, M+UUB )
*
      IF( IPACK.EQ.5 .OR. IPACK.EQ.6 ) THEN
         MINLDA = UUB + 1
      ELSE IF( IPACK.EQ.7 ) THEN
         MINLDA = LLB + UUB + 1
      ELSE
         MINLDA = M
      END IF
*
*     Use Givens rotation method if bandwidth small enough,
*     or if LDA is too small to store the matrix unpacked.
*
      GIVENS = .FALSE.
      IF( ISYM.EQ.1 ) THEN
         IF( DBLE( LLB+UUB ).LT.0.3D0*DBLE( MAX( 1, MR+NC ) ) )
     $      GIVENS = .TRUE.
      ELSE
         IF( 2*LLB.LT.M )
     $      GIVENS = .TRUE.
      END IF
      IF( LDA.LT.M .AND. LDA.GE.MINLDA )
     $   GIVENS = .TRUE.
*
*     Set INFO if an error
*
      IF( M.LT.0 ) THEN
         INFO = -1
      ELSE IF( M.NE.N .AND. ISYM.NE.1 ) THEN
         INFO = -1
      ELSE IF( N.LT.0 ) THEN
         INFO = -2
      ELSE IF( IDIST.EQ.-1 ) THEN
         INFO = -3
      ELSE IF( ISYM.EQ.-1 ) THEN
         INFO = -5
      ELSE IF( ABS( MODE ).GT.6 ) THEN
         INFO = -7
      ELSE IF( ( MODE.NE.0 .AND. ABS( MODE ).NE.6 ) .AND. COND.LT.ONE )
     $          THEN
         INFO = -8
      ELSE IF( KL.LT.0 ) THEN
         INFO = -10
      ELSE IF( KU.LT.0 .OR. ( ISYM.NE.1 .AND. KL.NE.KU ) ) THEN
         INFO = -11
      ELSE IF( IPACK.EQ.-1 .OR. ( ISYMPK.EQ.1 .AND. ISYM.EQ.1 ) .OR.
     $         ( ISYMPK.EQ.2 .AND. ISYM.EQ.1 .AND. KL.GT.0 ) .OR.
     $         ( ISYMPK.EQ.3 .AND. ISYM.EQ.1 .AND. KU.GT.0 ) .OR.
     $         ( ISYMPK.NE.0 .AND. M.NE.N ) ) THEN
         INFO = -12
      ELSE IF( LDA.LT.MAX( 1, MINLDA ) ) THEN
         INFO = -14
      END IF
*
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DLATMS', -INFO )
         RETURN
      END IF
*
*     Initialize random number generator
*
      DO 10 I = 1, 4
         ISEED( I ) = MOD( ABS( ISEED( I ) ), 4096 )
   10 CONTINUE
*
      IF( MOD( ISEED( 4 ), 2 ).NE.1 )
     $   ISEED( 4 ) = ISEED( 4 ) + 1
*
*     2)      Set up D  if indicated.
*
*             Compute D according to COND and MODE
*
      CALL DLATM1( MODE, COND, IRSIGN, IDIST, ISEED, D, MNMIN, IINFO )
      IF( IINFO.NE.0 ) THEN
         INFO = 1
         RETURN
      END IF
*
*     Choose Top-Down if D is (apparently) increasing,
*     Bottom-Up if D is (apparently) decreasing.
*
      IF( ABS( D( 1 ) ).LE.ABS( D( MNMIN ) ) ) THEN
         TOPDWN = .TRUE.
      ELSE
         TOPDWN = .FALSE.
      END IF
*
      IF( MODE.NE.0 .AND. ABS( MODE ).NE.6 ) THEN
*
*        Scale by DMAX
*
         TEMP = ABS( D( 1 ) )
         DO 20 I = 2, MNMIN
            TEMP = MAX( TEMP, ABS( D( I ) ) )
   20    CONTINUE
*
         IF( TEMP.GT.ZERO ) THEN
            ALPHA = DMAX / TEMP
         ELSE
            INFO = 2
            RETURN
         END IF
*
         CALL DSCAL( MNMIN, ALPHA, D, 1 )
*
      END IF
*
*     3)      Generate Banded Matrix using Givens rotations.
*             Also the special case of UUB=LLB=0
*
*               Compute Addressing constants to cover all
*               storage formats.  Whether GE, SY, GB, or SB,
*               upper or lower triangle or both,
*               the (i,j)-th element is in
*               A( i - ISKEW*j + IOFFST, j )
*
      IF( IPACK.GT.4 ) THEN
         ILDA = LDA - 1
         ISKEW = 1
         IF( IPACK.GT.5 ) THEN
            IOFFST = UUB + 1
         ELSE
            IOFFST = 1
         END IF
      ELSE
         ILDA = LDA
         ISKEW = 0
         IOFFST = 0
      END IF
*
*     IPACKG is the format that the matrix is generated in. If this is
*     different from IPACK, then the matrix must be repacked at the
*     end.  It also signals how to compute the norm, for scaling.
*
      IPACKG = 0
      CALL DLASET( 'Full', LDA, N, ZERO, ZERO, A, LDA )
*
*     Diagonal Matrix -- We are done, unless it
*     is to be stored SP/PP/TP (PACK='R' or 'C')
*
      IF( LLB.EQ.0 .AND. UUB.EQ.0 ) THEN
         CALL DCOPY( MNMIN, D, 1, A( 1-ISKEW+IOFFST, 1 ), ILDA+1 )
         IF( IPACK.LE.2 .OR. IPACK.GE.5 )
     $      IPACKG = IPACK
*
      ELSE IF( GIVENS ) THEN
*
*        Check whether to use Givens rotations,
*        Householder transformations, or nothing.
*
         IF( ISYM.EQ.1 ) THEN
*
*           Non-symmetric -- A = U D V
*
            IF( IPACK.GT.4 ) THEN
               IPACKG = IPACK
            ELSE
               IPACKG = 0
            END IF
*
            CALL DCOPY( MNMIN, D, 1, A( 1-ISKEW+IOFFST, 1 ), ILDA+1 )
*
            IF( TOPDWN ) THEN
               JKL = 0
               DO 50 JKU = 1, UUB
*
*                 Transform from bandwidth JKL, JKU-1 to JKL, JKU
*
*                 Last row actually rotated is M
*                 Last column actually rotated is MIN( M+JKU, N )
*
                  DO 40 JR = 1, MIN( M+JKU, N ) + JKL - 1
                     EXTRA = ZERO
                     ANGLE = TWOPI*DLARND( 1, ISEED )
                     C = COS( ANGLE )
                     S = SIN( ANGLE )
                     ICOL = MAX( 1, JR-JKL )
                     IF( JR.LT.M ) THEN
                        IL = MIN( N, JR+JKU ) + 1 - ICOL
                        CALL DLAROT( .TRUE., JR.GT.JKL, .FALSE., IL, C,
     $                               S, A( JR-ISKEW*ICOL+IOFFST, ICOL ),
     $                               ILDA, EXTRA, DUMMY )
                     END IF
*
*                    Chase "EXTRA" back up
*
                     IR = JR
                     IC = ICOL
                     DO 30 JCH = JR - JKL, 1, -JKL - JKU
                        IF( IR.LT.M ) THEN
                           CALL DLARTG( A( IR+1-ISKEW*( IC+1 )+IOFFST,
     $                                  IC+1 ), EXTRA, C, S, DUMMY )
                        END IF
                        IROW = MAX( 1, JCH-JKU )
                        IL = IR + 2 - IROW
                        TEMP = ZERO
                        ILTEMP = JCH.GT.JKU
                        CALL DLAROT( .FALSE., ILTEMP, .TRUE., IL, C, -S,
     $                               A( IROW-ISKEW*IC+IOFFST, IC ),
     $                               ILDA, TEMP, EXTRA )
                        IF( ILTEMP ) THEN
                           CALL DLARTG( A( IROW+1-ISKEW*( IC+1 )+IOFFST,
     $                                  IC+1 ), TEMP, C, S, DUMMY )
                           ICOL = MAX( 1, JCH-JKU-JKL )
                           IL = IC + 2 - ICOL
                           EXTRA = ZERO
                           CALL DLAROT( .TRUE., JCH.GT.JKU+JKL, .TRUE.,
     $                                  IL, C, -S, A( IROW-ISKEW*ICOL+
     $                                  IOFFST, ICOL ), ILDA, EXTRA,
     $                                  TEMP )
                           IC = ICOL
                           IR = IROW
                        END IF
   30                CONTINUE
   40             CONTINUE
   50          CONTINUE
*
               JKU = UUB
               DO 80 JKL = 1, LLB
*
*                 Transform from bandwidth JKL-1, JKU to JKL, JKU
*
                  DO 70 JC = 1, MIN( N+JKL, M ) + JKU - 1
                     EXTRA = ZERO
                     ANGLE = TWOPI*DLARND( 1, ISEED )
                     C = COS( ANGLE )
                     S = SIN( ANGLE )
                     IROW = MAX( 1, JC-JKU )
                     IF( JC.LT.N ) THEN
                        IL = MIN( M, JC+JKL ) + 1 - IROW
                        CALL DLAROT( .FALSE., JC.GT.JKU, .FALSE., IL, C,
     $                               S, A( IROW-ISKEW*JC+IOFFST, JC ),
     $                               ILDA, EXTRA, DUMMY )
                     END IF
*
*                    Chase "EXTRA" back up
*
                     IC = JC
                     IR = IROW
                     DO 60 JCH = JC - JKU, 1, -JKL - JKU
                        IF( IC.LT.N ) THEN
                           CALL DLARTG( A( IR+1-ISKEW*( IC+1 )+IOFFST,
     $                                  IC+1 ), EXTRA, C, S, DUMMY )
                        END IF
                        ICOL = MAX( 1, JCH-JKL )
                        IL = IC + 2 - ICOL
                        TEMP = ZERO
                        ILTEMP = JCH.GT.JKL
                        CALL DLAROT( .TRUE., ILTEMP, .TRUE., IL, C, -S,
     $                               A( IR-ISKEW*ICOL+IOFFST, ICOL ),
     $                               ILDA, TEMP, EXTRA )
                        IF( ILTEMP ) THEN
                           CALL DLARTG( A( IR+1-ISKEW*( ICOL+1 )+IOFFST,
     $                                  ICOL+1 ), TEMP, C, S, DUMMY )
                           IROW = MAX( 1, JCH-JKL-JKU )
                           IL = IR + 2 - IROW
                           EXTRA = ZERO
                           CALL DLAROT( .FALSE., JCH.GT.JKL+JKU, .TRUE.,
     $                                  IL, C, -S, A( IROW-ISKEW*ICOL+
     $                                  IOFFST, ICOL ), ILDA, EXTRA,
     $                                  TEMP )
                           IC = ICOL
                           IR = IROW
                        END IF
   60                CONTINUE
   70             CONTINUE
   80          CONTINUE
*
            ELSE
*
*              Bottom-Up -- Start at the bottom right.
*
               JKL = 0
               DO 110 JKU = 1, UUB
*
*                 Transform from bandwidth JKL, JKU-1 to JKL, JKU
*
*                 First row actually rotated is M
*                 First column actually rotated is MIN( M+JKU, N )
*
                  IENDCH = MIN( M, N+JKL ) - 1
                  DO 100 JC = MIN( M+JKU, N ) - 1, 1 - JKL, -1
                     EXTRA = ZERO
                     ANGLE = TWOPI*DLARND( 1, ISEED )
                     C = COS( ANGLE )
                     S = SIN( ANGLE )
                     IROW = MAX( 1, JC-JKU+1 )
                     IF( JC.GT.0 ) THEN
                        IL = MIN( M, JC+JKL+1 ) + 1 - IROW
                        CALL DLAROT( .FALSE., .FALSE., JC+JKL.LT.M, IL,
     $                               C, S, A( IROW-ISKEW*JC+IOFFST,
     $                               JC ), ILDA, DUMMY, EXTRA )
                     END IF
*
*                    Chase "EXTRA" back down
*
                     IC = JC
                     DO 90 JCH = JC + JKL, IENDCH, JKL + JKU
                        ILEXTR = IC.GT.0
                        IF( ILEXTR ) THEN
                           CALL DLARTG( A( JCH-ISKEW*IC+IOFFST, IC ),
     $                                  EXTRA, C, S, DUMMY )
                        END IF
                        IC = MAX( 1, IC )
                        ICOL = MIN( N-1, JCH+JKU )
                        ILTEMP = JCH + JKU.LT.N
                        TEMP = ZERO
                        CALL DLAROT( .TRUE., ILEXTR, ILTEMP, ICOL+2-IC,
     $                               C, S, A( JCH-ISKEW*IC+IOFFST, IC ),
     $                               ILDA, EXTRA, TEMP )
                        IF( ILTEMP ) THEN
                           CALL DLARTG( A( JCH-ISKEW*ICOL+IOFFST,
     $                                  ICOL ), TEMP, C, S, DUMMY )
                           IL = MIN( IENDCH, JCH+JKL+JKU ) + 2 - JCH
                           EXTRA = ZERO
                           CALL DLAROT( .FALSE., .TRUE.,
     $                                  JCH+JKL+JKU.LE.IENDCH, IL, C, S,
     $                                  A( JCH-ISKEW*ICOL+IOFFST,
     $                                  ICOL ), ILDA, TEMP, EXTRA )
                           IC = ICOL
                        END IF
   90                CONTINUE
  100             CONTINUE
  110          CONTINUE
*
               JKU = UUB
               DO 140 JKL = 1, LLB
*
*                 Transform from bandwidth JKL-1, JKU to JKL, JKU
*
*                 First row actually rotated is MIN( N+JKL, M )
*                 First column actually rotated is N
*
                  IENDCH = MIN( N, M+JKU ) - 1
                  DO 130 JR = MIN( N+JKL, M ) - 1, 1 - JKU, -1
                     EXTRA = ZERO
                     ANGLE = TWOPI*DLARND( 1, ISEED )
                     C = COS( ANGLE )
                     S = SIN( ANGLE )
                     ICOL = MAX( 1, JR-JKL+1 )
                     IF( JR.GT.0 ) THEN
                        IL = MIN( N, JR+JKU+1 ) + 1 - ICOL
                        CALL DLAROT( .TRUE., .FALSE., JR+JKU.LT.N, IL,
     $                               C, S, A( JR-ISKEW*ICOL+IOFFST,
     $                               ICOL ), ILDA, DUMMY, EXTRA )
                     END IF
*
*                    Chase "EXTRA" back down
*
                     IR = JR
                     DO 120 JCH = JR + JKU, IENDCH, JKL + JKU
                        ILEXTR = IR.GT.0
                        IF( ILEXTR ) THEN
                           CALL DLARTG( A( IR-ISKEW*JCH+IOFFST, JCH ),
     $                                  EXTRA, C, S, DUMMY )
                        END IF
                        IR = MAX( 1, IR )
                        IROW = MIN( M-1, JCH+JKL )
                        ILTEMP = JCH + JKL.LT.M
                        TEMP = ZERO
                        CALL DLAROT( .FALSE., ILEXTR, ILTEMP, IROW+2-IR,
     $                               C, S, A( IR-ISKEW*JCH+IOFFST,
     $                               JCH ), ILDA, EXTRA, TEMP )
                        IF( ILTEMP ) THEN
                           CALL DLARTG( A( IROW-ISKEW*JCH+IOFFST, JCH ),
     $                                  TEMP, C, S, DUMMY )
                           IL = MIN( IENDCH, JCH+JKL+JKU ) + 2 - JCH
                           EXTRA = ZERO
                           CALL DLAROT( .TRUE., .TRUE.,
     $                                  JCH+JKL+JKU.LE.IENDCH, IL, C, S,
     $                                  A( IROW-ISKEW*JCH+IOFFST, JCH ),
     $                                  ILDA, TEMP, EXTRA )
                           IR = IROW
                        END IF
  120                CONTINUE
  130             CONTINUE
  140          CONTINUE
            END IF
*
         ELSE
*
*           Symmetric -- A = U D U'
*
            IPACKG = IPACK
            IOFFG = IOFFST
*
            IF( TOPDWN ) THEN
*
*              Top-Down -- Generate Upper triangle only
*
               IF( IPACK.GE.5 ) THEN
                  IPACKG = 6
                  IOFFG = UUB + 1
               ELSE
                  IPACKG = 1
               END IF
               CALL DCOPY( MNMIN, D, 1, A( 1-ISKEW+IOFFG, 1 ), ILDA+1 )
*
               DO 170 K = 1, UUB
                  DO 160 JC = 1, N - 1
                     IROW = MAX( 1, JC-K )
                     IL = MIN( JC+1, K+2 )
                     EXTRA = ZERO
                     TEMP = A( JC-ISKEW*( JC+1 )+IOFFG, JC+1 )
                     ANGLE = TWOPI*DLARND( 1, ISEED )
                     C = COS( ANGLE )
                     S = SIN( ANGLE )
                     CALL DLAROT( .FALSE., JC.GT.K, .TRUE., IL, C, S,
     $                            A( IROW-ISKEW*JC+IOFFG, JC ), ILDA,
     $                            EXTRA, TEMP )
                     CALL DLAROT( .TRUE., .TRUE., .FALSE.,
     $                            MIN( K, N-JC )+1, C, S,
     $                            A( ( 1-ISKEW )*JC+IOFFG, JC ), ILDA,
     $                            TEMP, DUMMY )
*
*                    Chase EXTRA back up the matrix
*
                     ICOL = JC
                     DO 150 JCH = JC - K, 1, -K
                        CALL DLARTG( A( JCH+1-ISKEW*( ICOL+1 )+IOFFG,
     $                               ICOL+1 ), EXTRA, C, S, DUMMY )
                        TEMP = A( JCH-ISKEW*( JCH+1 )+IOFFG, JCH+1 )
                        CALL DLAROT( .TRUE., .TRUE., .TRUE., K+2, C, -S,
     $                               A( ( 1-ISKEW )*JCH+IOFFG, JCH ),
     $                               ILDA, TEMP, EXTRA )
                        IROW = MAX( 1, JCH-K )
                        IL = MIN( JCH+1, K+2 )
                        EXTRA = ZERO
                        CALL DLAROT( .FALSE., JCH.GT.K, .TRUE., IL, C,
     $                               -S, A( IROW-ISKEW*JCH+IOFFG, JCH ),
     $                               ILDA, EXTRA, TEMP )
                        ICOL = JCH
  150                CONTINUE
  160             CONTINUE
  170          CONTINUE
*
*              If we need lower triangle, copy from upper. Note that
*              the order of copying is chosen to work for 'q' -> 'b'
*
               IF( IPACK.NE.IPACKG .AND. IPACK.NE.3 ) THEN
                  DO 190 JC = 1, N
                     IROW = IOFFST - ISKEW*JC
                     DO 180 JR = JC, MIN( N, JC+UUB )
                        A( JR+IROW, JC ) = A( JC-ISKEW*JR+IOFFG, JR )
  180                CONTINUE
  190             CONTINUE
                  IF( IPACK.EQ.5 ) THEN
                     DO 210 JC = N - UUB + 1, N
                        DO 200 JR = N + 2 - JC, UUB + 1
                           A( JR, JC ) = ZERO
  200                   CONTINUE
  210                CONTINUE
                  END IF
                  IF( IPACKG.EQ.6 ) THEN
                     IPACKG = IPACK
                  ELSE
                     IPACKG = 0
                  END IF
               END IF
            ELSE
*
*              Bottom-Up -- Generate Lower triangle only
*
               IF( IPACK.GE.5 ) THEN
                  IPACKG = 5
                  IF( IPACK.EQ.6 )
     $               IOFFG = 1
               ELSE
                  IPACKG = 2
               END IF
               CALL DCOPY( MNMIN, D, 1, A( 1-ISKEW+IOFFG, 1 ), ILDA+1 )
*
               DO 240 K = 1, UUB
                  DO 230 JC = N - 1, 1, -1
                     IL = MIN( N+1-JC, K+2 )
                     EXTRA = ZERO
                     TEMP = A( 1+( 1-ISKEW )*JC+IOFFG, JC )
                     ANGLE = TWOPI*DLARND( 1, ISEED )
                     C = COS( ANGLE )
                     S = -SIN( ANGLE )
                     CALL DLAROT( .FALSE., .TRUE., N-JC.GT.K, IL, C, S,
     $                            A( ( 1-ISKEW )*JC+IOFFG, JC ), ILDA,
     $                            TEMP, EXTRA )
                     ICOL = MAX( 1, JC-K+1 )
                     CALL DLAROT( .TRUE., .FALSE., .TRUE., JC+2-ICOL, C,
     $                            S, A( JC-ISKEW*ICOL+IOFFG, ICOL ),
     $                            ILDA, DUMMY, TEMP )
*
*                    Chase EXTRA back down the matrix
*
                     ICOL = JC
                     DO 220 JCH = JC + K, N - 1, K
                        CALL DLARTG( A( JCH-ISKEW*ICOL+IOFFG, ICOL ),
     $                               EXTRA, C, S, DUMMY )
                        TEMP = A( 1+( 1-ISKEW )*JCH+IOFFG, JCH )
                        CALL DLAROT( .TRUE., .TRUE., .TRUE., K+2, C, S,
     $                               A( JCH-ISKEW*ICOL+IOFFG, ICOL ),
     $                               ILDA, EXTRA, TEMP )
                        IL = MIN( N+1-JCH, K+2 )
                        EXTRA = ZERO
                        CALL DLAROT( .FALSE., .TRUE., N-JCH.GT.K, IL, C,
     $                               S, A( ( 1-ISKEW )*JCH+IOFFG, JCH ),
     $                               ILDA, TEMP, EXTRA )
                        ICOL = JCH
  220                CONTINUE
  230             CONTINUE
  240          CONTINUE
*
*              If we need upper triangle, copy from lower. Note that
*              the order of copying is chosen to work for 'b' -> 'q'
*
               IF( IPACK.NE.IPACKG .AND. IPACK.NE.4 ) THEN
                  DO 260 JC = N, 1, -1
                     IROW = IOFFST - ISKEW*JC
                     DO 250 JR = JC, MAX( 1, JC-UUB ), -1
                        A( JR+IROW, JC ) = A( JC-ISKEW*JR+IOFFG, JR )
  250                CONTINUE
  260             CONTINUE
                  IF( IPACK.EQ.6 ) THEN
                     DO 280 JC = 1, UUB
                        DO 270 JR = 1, UUB + 1 - JC
                           A( JR, JC ) = ZERO
  270                   CONTINUE
  280                CONTINUE
                  END IF
                  IF( IPACKG.EQ.5 ) THEN
                     IPACKG = IPACK
                  ELSE
                     IPACKG = 0
                  END IF
               END IF
            END IF
         END IF
*
      ELSE
*
*        4)      Generate Banded Matrix by first
*                Rotating by random Unitary matrices,
*                then reducing the bandwidth using Householder
*                transformations.
*
*                Note: we should get here only if LDA .ge. N
*
         IF( ISYM.EQ.1 ) THEN
*
*           Non-symmetric -- A = U D V
*
            CALL DLAGGE( MR, NC, LLB, UUB, D, A, LDA, ISEED, WORK,
     $                   IINFO )
         ELSE
*
*           Symmetric -- A = U D U'
*
            CALL DLAGSY( M, LLB, D, A, LDA, ISEED, WORK, IINFO )
*
         END IF
         IF( IINFO.NE.0 ) THEN
            INFO = 3
            RETURN
         END IF
      END IF
*
*     5)      Pack the matrix
*
      IF( IPACK.NE.IPACKG ) THEN
         IF( IPACK.EQ.1 ) THEN
*
*           'U' -- Upper triangular, not packed
*
            DO 300 J = 1, M
               DO 290 I = J + 1, M
                  A( I, J ) = ZERO
  290          CONTINUE
  300       CONTINUE
*
         ELSE IF( IPACK.EQ.2 ) THEN
*
*           'L' -- Lower triangular, not packed
*
            DO 320 J = 2, M
               DO 310 I = 1, J - 1
                  A( I, J ) = ZERO
  310          CONTINUE
  320       CONTINUE
*
         ELSE IF( IPACK.EQ.3 ) THEN
*
*           'C' -- Upper triangle packed Columnwise.
*
            ICOL = 1
            IROW = 0
            DO 340 J = 1, M
               DO 330 I = 1, J
                  IROW = IROW + 1
                  IF( IROW.GT.LDA ) THEN
                     IROW = 1
                     ICOL = ICOL + 1
                  END IF
                  A( IROW, ICOL ) = A( I, J )
  330          CONTINUE
  340       CONTINUE
*
         ELSE IF( IPACK.EQ.4 ) THEN
*
*           'R' -- Lower triangle packed Columnwise.
*
            ICOL = 1
            IROW = 0
            DO 360 J = 1, M
               DO 350 I = J, M
                  IROW = IROW + 1
                  IF( IROW.GT.LDA ) THEN
                     IROW = 1
                     ICOL = ICOL + 1
                  END IF
                  A( IROW, ICOL ) = A( I, J )
  350          CONTINUE
  360       CONTINUE
*
         ELSE IF( IPACK.GE.5 ) THEN
*
*           'B' -- The lower triangle is packed as a band matrix.
*           'Q' -- The upper triangle is packed as a band matrix.
*           'Z' -- The whole matrix is packed as a band matrix.
*
            IF( IPACK.EQ.5 )
     $         UUB = 0
            IF( IPACK.EQ.6 )
     $         LLB = 0
*
            DO 380 J = 1, UUB
               DO 370 I = MIN( J+LLB, M ), 1, -1
                  A( I-J+UUB+1, J ) = A( I, J )
  370          CONTINUE
  380       CONTINUE
*
            DO 400 J = UUB + 2, N
               DO 390 I = J - UUB, MIN( J+LLB, M )
                  A( I-J+UUB+1, J ) = A( I, J )
  390          CONTINUE
  400       CONTINUE
         END IF
*
*        If packed, zero out extraneous elements.
*
*        Symmetric/Triangular Packed --
*        zero out everything after A(IROW,ICOL)
*
         IF( IPACK.EQ.3 .OR. IPACK.EQ.4 ) THEN
            DO 420 JC = ICOL, M
               DO 410 JR = IROW + 1, LDA
                  A( JR, JC ) = ZERO
  410          CONTINUE
               IROW = 0
  420       CONTINUE
*
         ELSE IF( IPACK.GE.5 ) THEN
*
*           Packed Band --
*              1st row is now in A( UUB+2-j, j), zero above it
*              m-th row is now in A( M+UUB-j,j), zero below it
*              last non-zero diagonal is now in A( UUB+LLB+1,j ),
*                 zero below it, too.
*
            IR1 = UUB + LLB + 2
            IR2 = UUB + M + 2
            DO 450 JC = 1, N
               DO 430 JR = 1, UUB + 1 - JC
                  A( JR, JC ) = ZERO
  430          CONTINUE
               DO 440 JR = MAX( 1, MIN( IR1, IR2-JC ) ), LDA
                  A( JR, JC ) = ZERO
  440          CONTINUE
  450       CONTINUE
         END IF
      END IF
*
      RETURN
*
*     End of DLATMS
*
      END
      DOUBLE PRECISION FUNCTION DLARND( IDIST, ISEED )
*
*  -- LAPACK auxiliary routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     September 30, 1994
*
*     .. Scalar Arguments ..
      INTEGER            IDIST
*     ..
*     .. Array Arguments ..
      INTEGER            ISEED( 4 )
*     ..
*
*  Purpose
*  =======
*
*  DLARND returns a random real number from a uniform or normal
*  distribution.
*
*  Arguments
*  =========
*
*  IDIST   (input) INTEGER
*          Specifies the distribution of the random numbers:
*          = 1:  uniform (0,1)
*          = 2:  uniform (-1,1)
*          = 3:  normal (0,1)
*
*  ISEED   (input/output) INTEGER array, dimension (4)
*          On entry, the seed of the random number generator; the array
*          elements must be between 0 and 4095, and ISEED(4) must be
*          odd.
*          On exit, the seed is updated.
*
*  Further Details
*  ===============
*
*  This routine calls the auxiliary routine DLARAN to generate a random
*  real number from a uniform (0,1) distribution. The Box-Muller method
*  is used to transform numbers from a uniform to a normal distribution.
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ONE, TWO
      PARAMETER          ( ONE = 1.0D+0, TWO = 2.0D+0 )
      DOUBLE PRECISION   TWOPI
      PARAMETER          ( TWOPI = 6.2831853071795864769252867663D+0 )
*     ..
*     .. Local Scalars ..
      DOUBLE PRECISION   T1, T2
*     ..
*     .. External Functions ..
      DOUBLE PRECISION   DLARAN
      EXTERNAL           DLARAN
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          COS, LOG, SQRT
*     ..
*     .. Executable Statements ..
*
*     Generate a real random number from a uniform (0,1) distribution
*
      T1 = DLARAN( ISEED )
*
      IF( IDIST.EQ.1 ) THEN
*
*        uniform (0,1)
*
         DLARND = T1
      ELSE IF( IDIST.EQ.2 ) THEN
*
*        uniform (-1,1)
*
         DLARND = TWO*T1 - ONE
      ELSE IF( IDIST.EQ.3 ) THEN
*
*        normal (0,1)
*
         T2 = DLARAN( ISEED )
         DLARND = SQRT( -TWO*LOG( T1 ) )*COS( TWOPI*T2 )
      END IF
      RETURN
*
*     End of DLARND
*
      END
      DOUBLE PRECISION FUNCTION DLARAN( ISEED )
*
*  -- LAPACK auxiliary routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     February 29, 1992
*
*     .. Array Arguments ..
      INTEGER            ISEED( 4 )
*     ..
*
*  Purpose
*  =======
*
*  DLARAN returns a random real number from a uniform (0,1)
*  distribution.
*
*  Arguments
*  =========
*
*  ISEED   (input/output) INTEGER array, dimension (4)
*          On entry, the seed of the random number generator; the array
*          elements must be between 0 and 4095, and ISEED(4) must be
*          odd.
*          On exit, the seed is updated.
*
*  Further Details
*  ===============
*
*  This routine uses a multiplicative congruential method with modulus
*  2**48 and multiplier 33952834046453 (see G.S.Fishman,
*  'Multiplicative congruential random number generators with modulus
*  2**b: an exhaustive analysis for b = 32 and a partial analysis for
*  b = 48', Math. Comp. 189, pp 331-344, 1990).
*
*  48-bit integers are stored in 4 integer array elements with 12 bits
*  per element. Hence the routine is portable across machines with
*  integers of 32 bits or more.
*
*  =====================================================================
*
*     .. Parameters ..
      INTEGER            M1, M2, M3, M4
      PARAMETER          ( M1 = 494, M2 = 322, M3 = 2508, M4 = 2549 )
      DOUBLE PRECISION   ONE
      PARAMETER          ( ONE = 1.0D+0 )
      INTEGER            IPW2
      DOUBLE PRECISION   R
      PARAMETER          ( IPW2 = 4096, R = ONE / IPW2 )
*     ..
*     .. Local Scalars ..
      INTEGER            IT1, IT2, IT3, IT4
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          DBLE, MOD
*     ..
*     .. Executable Statements ..
*
*     multiply the seed by the multiplier modulo 2**48
*
      IT4 = ISEED( 4 )*M4
      IT3 = IT4 / IPW2
      IT4 = IT4 - IPW2*IT3
      IT3 = IT3 + ISEED( 3 )*M4 + ISEED( 4 )*M3
      IT2 = IT3 / IPW2
      IT3 = IT3 - IPW2*IT2
      IT2 = IT2 + ISEED( 2 )*M4 + ISEED( 3 )*M3 + ISEED( 4 )*M2
      IT1 = IT2 / IPW2
      IT2 = IT2 - IPW2*IT1
      IT1 = IT1 + ISEED( 1 )*M4 + ISEED( 2 )*M3 + ISEED( 3 )*M2 +
     $      ISEED( 4 )*M1
      IT1 = MOD( IT1, IPW2 )
*
*     return updated seed
*
      ISEED( 1 ) = IT1
      ISEED( 2 ) = IT2
      ISEED( 3 ) = IT3
      ISEED( 4 ) = IT4
*
*     convert 48-bit integer to a real number in the interval (0,1)
*
      DLARAN = R*( DBLE( IT1 )+R*( DBLE( IT2 )+R*( DBLE( IT3 )+R*
     $         ( DBLE( IT4 ) ) ) ) )
      RETURN
*
*     End of DLARAN
*
      END
      DOUBLE PRECISION FUNCTION DLATM3( M, N, I, J, ISUB, JSUB, KL, KU,
     $                 IDIST, ISEED, D, IGRADE, DL, DR, IPVTNG, IWORK,
     $                 SPARSE )
*
*  -- LAPACK auxiliary test routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     February 29, 1992
*
*     .. Scalar Arguments ..
*
      INTEGER            I, IDIST, IGRADE, IPVTNG, ISUB, J, JSUB, KL,
     $                   KU, M, N
      DOUBLE PRECISION   SPARSE
*     ..
*
*     .. Array Arguments ..
*
      INTEGER            ISEED( 4 ), IWORK( * )
      DOUBLE PRECISION   D( * ), DL( * ), DR( * )
*     ..
*
*  Purpose
*  =======
*
*     DLATM3 returns the (ISUB,JSUB) entry of a random matrix of
*     dimension (M, N) described by the other paramters. (ISUB,JSUB)
*     is the final position of the (I,J) entry after pivoting
*     according to IPVTNG and IWORK. DLATM3 is called by the
*     DLATMR routine in order to build random test matrices. No error
*     checking on parameters is done, because this routine is called in
*     a tight loop by DLATMR which has already checked the parameters.
*
*     Use of DLATM3 differs from SLATM2 in the order in which the random
*     number generator is called to fill in random matrix entries.
*     With DLATM2, the generator is called to fill in the pivoted matrix
*     columnwise. With DLATM3, the generator is called to fill in the
*     matrix columnwise, after which it is pivoted. Thus, DLATM3 can
*     be used to construct random matrices which differ only in their
*     order of rows and/or columns. DLATM2 is used to construct band
*     matrices while avoiding calling the random number generator for
*     entries outside the band (and therefore generating random numbers
*     in different orders for different pivot orders).
*
*     The matrix whose (ISUB,JSUB) entry is returned is constructed as
*     follows (this routine only computes one entry):
*
*       If ISUB is outside (1..M) or JSUB is outside (1..N), return zero
*          (this is convenient for generating matrices in band format).
*
*       Generate a matrix A with random entries of distribution IDIST.
*
*       Set the diagonal to D.
*
*       Grade the matrix, if desired, from the left (by DL) and/or
*          from the right (by DR or DL) as specified by IGRADE.
*
*       Permute, if desired, the rows and/or columns as specified by
*          IPVTNG and IWORK.
*
*       Band the matrix to have lower bandwidth KL and upper
*          bandwidth KU.
*
*       Set random entries to zero as specified by SPARSE.
*
*  Arguments
*  =========
*
*  M      - INTEGER
*           Number of rows of matrix. Not modified.
*
*  N      - INTEGER
*           Number of columns of matrix. Not modified.
*
*  I      - INTEGER
*           Row of unpivoted entry to be returned. Not modified.
*
*  J      - INTEGER
*           Column of unpivoted entry to be returned. Not modified.
*
*  ISUB   - INTEGER
*           Row of pivoted entry to be returned. Changed on exit.
*
*  JSUB   - INTEGER
*           Column of pivoted entry to be returned. Changed on exit.
*
*  KL     - INTEGER
*           Lower bandwidth. Not modified.
*
*  KU     - INTEGER
*           Upper bandwidth. Not modified.
*
*  IDIST  - INTEGER
*           On entry, IDIST specifies the type of distribution to be
*           used to generate a random matrix .
*           1 => UNIFORM( 0, 1 )
*           2 => UNIFORM( -1, 1 )
*           3 => NORMAL( 0, 1 )
*           Not modified.
*
*  ISEED  - INTEGER array of dimension ( 4 )
*           Seed for random number generator.
*           Changed on exit.
*
*  D      - DOUBLE PRECISION array of dimension ( MIN( I , J ) )
*           Diagonal entries of matrix. Not modified.
*
*  IGRADE - INTEGER
*           Specifies grading of matrix as follows:
*           0  => no grading
*           1  => matrix premultiplied by diag( DL )
*           2  => matrix postmultiplied by diag( DR )
*           3  => matrix premultiplied by diag( DL ) and
*                         postmultiplied by diag( DR )
*           4  => matrix premultiplied by diag( DL ) and
*                         postmultiplied by inv( diag( DL ) )
*           5  => matrix premultiplied by diag( DL ) and
*                         postmultiplied by diag( DL )
*           Not modified.
*
*  DL     - DOUBLE PRECISION array ( I or J, as appropriate )
*           Left scale factors for grading matrix.  Not modified.
*
*  DR     - DOUBLE PRECISION array ( I or J, as appropriate )
*           Right scale factors for grading matrix.  Not modified.
*
*  IPVTNG - INTEGER
*           On entry specifies pivoting permutations as follows:
*           0 => none.
*           1 => row pivoting.
*           2 => column pivoting.
*           3 => full pivoting, i.e., on both sides.
*           Not modified.
*
*  IWORK  - INTEGER array ( I or J, as appropriate )
*           This array specifies the permutation used. The
*           row (or column) originally in position K is in
*           position IWORK( K ) after pivoting.
*           This differs from IWORK for DLATM2. Not modified.
*
*  SPARSE - DOUBLE PRECISION between 0. and 1.
*           On entry specifies the sparsity of the matrix
*           if sparse matix is to be generated.
*           SPARSE should lie between 0 and 1.
*           A uniform ( 0, 1 ) random number x is generated and
*           compared to SPARSE; if x is larger the matrix entry
*           is unchanged and if x is smaller the entry is set
*           to zero. Thus on the average a fraction SPARSE of the
*           entries will be set to zero.
*           Not modified.
*
*  =====================================================================
*
*     .. Parameters ..
*
      DOUBLE PRECISION   ZERO
      PARAMETER          ( ZERO = 0.0D0 )
*     ..
*
*     .. Local Scalars ..
*
      DOUBLE PRECISION   TEMP
*     ..
*
*     .. External Functions ..
*
      DOUBLE PRECISION   DLARAN, DLARND
      EXTERNAL           DLARAN, DLARND
*     ..
*
*-----------------------------------------------------------------------
*
*     .. Executable Statements ..
*
*
*     Check for I and J in range
*
      IF( I.LT.1 .OR. I.GT.M .OR. J.LT.1 .OR. J.GT.N ) THEN
         ISUB = I
         JSUB = J
         DLATM3 = ZERO
         RETURN
      END IF
*
*     Compute subscripts depending on IPVTNG
*
      IF( IPVTNG.EQ.0 ) THEN
         ISUB = I
         JSUB = J
      ELSE IF( IPVTNG.EQ.1 ) THEN
         ISUB = IWORK( I )
         JSUB = J
      ELSE IF( IPVTNG.EQ.2 ) THEN
         ISUB = I
         JSUB = IWORK( J )
      ELSE IF( IPVTNG.EQ.3 ) THEN
         ISUB = IWORK( I )
         JSUB = IWORK( J )
      END IF
*
*     Check for banding
*
      IF( JSUB.GT.ISUB+KU .OR. JSUB.LT.ISUB-KL ) THEN
         DLATM3 = ZERO
         RETURN
      END IF
*
*     Check for sparsity
*
      IF( SPARSE.GT.ZERO ) THEN
         IF( DLARAN( ISEED ).LT.SPARSE ) THEN
            DLATM3 = ZERO
            RETURN
         END IF
      END IF
*
*     Compute entry and grade it according to IGRADE
*
      IF( I.EQ.J ) THEN
         TEMP = D( I )
      ELSE
         TEMP = DLARND( IDIST, ISEED )
      END IF
      IF( IGRADE.EQ.1 ) THEN
         TEMP = TEMP*DL( I )
      ELSE IF( IGRADE.EQ.2 ) THEN
         TEMP = TEMP*DR( J )
      ELSE IF( IGRADE.EQ.3 ) THEN
         TEMP = TEMP*DL( I )*DR( J )
      ELSE IF( IGRADE.EQ.4 .AND. I.NE.J ) THEN
         TEMP = TEMP*DL( I ) / DL( J )
      ELSE IF( IGRADE.EQ.5 ) THEN
         TEMP = TEMP*DL( I )*DL( J )
      END IF
      DLATM3 = TEMP
      RETURN
*
*     End of DLATM3
*
      END
      DOUBLE PRECISION FUNCTION DLATM2( M, N, I, J, KL, KU, IDIST,
     $                 ISEED, D, IGRADE, DL, DR, IPVTNG, IWORK, SPARSE )
*
*  -- LAPACK auxiliary test routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     February 29, 1992
*
*     .. Scalar Arguments ..
*
      INTEGER            I, IDIST, IGRADE, IPVTNG, J, KL, KU, M, N
      DOUBLE PRECISION   SPARSE
*     ..
*
*     .. Array Arguments ..
*
      INTEGER            ISEED( 4 ), IWORK( * )
      DOUBLE PRECISION   D( * ), DL( * ), DR( * )
*     ..
*
*  Purpose
*  =======
*
*     DLATM2 returns the (I,J) entry of a random matrix of dimension
*     (M, N) described by the other paramters. It is called by the
*     DLATMR routine in order to build random test matrices. No error
*     checking on parameters is done, because this routine is called in
*     a tight loop by DLATMR which has already checked the parameters.
*
*     Use of DLATM2 differs from SLATM3 in the order in which the random
*     number generator is called to fill in random matrix entries.
*     With DLATM2, the generator is called to fill in the pivoted matrix
*     columnwise. With DLATM3, the generator is called to fill in the
*     matrix columnwise, after which it is pivoted. Thus, DLATM3 can
*     be used to construct random matrices which differ only in their
*     order of rows and/or columns. DLATM2 is used to construct band
*     matrices while avoiding calling the random number generator for
*     entries outside the band (and therefore generating random numbers
*
*     The matrix whose (I,J) entry is returned is constructed as
*     follows (this routine only computes one entry):
*
*       If I is outside (1..M) or J is outside (1..N), return zero
*          (this is convenient for generating matrices in band format).
*
*       Generate a matrix A with random entries of distribution IDIST.
*
*       Set the diagonal to D.
*
*       Grade the matrix, if desired, from the left (by DL) and/or
*          from the right (by DR or DL) as specified by IGRADE.
*
*       Permute, if desired, the rows and/or columns as specified by
*          IPVTNG and IWORK.
*
*       Band the matrix to have lower bandwidth KL and upper
*          bandwidth KU.
*
*       Set random entries to zero as specified by SPARSE.
*
*  Arguments
*  =========
*
*  M      - INTEGER
*           Number of rows of matrix. Not modified.
*
*  N      - INTEGER
*           Number of columns of matrix. Not modified.
*
*  I      - INTEGER
*           Row of entry to be returned. Not modified.
*
*  J      - INTEGER
*           Column of entry to be returned. Not modified.
*
*  KL     - INTEGER
*           Lower bandwidth. Not modified.
*
*  KU     - INTEGER
*           Upper bandwidth. Not modified.
*
*  IDIST  - INTEGER
*           On entry, IDIST specifies the type of distribution to be
*           used to generate a random matrix .
*           1 => UNIFORM( 0, 1 )
*           2 => UNIFORM( -1, 1 )
*           3 => NORMAL( 0, 1 )
*           Not modified.
*
*  ISEED  - INTEGER array of dimension ( 4 )
*           Seed for random number generator.
*           Changed on exit.
*
*  D      - DOUBLE PRECISION array of dimension ( MIN( I , J ) )
*           Diagonal entries of matrix. Not modified.
*
*  IGRADE - INTEGER
*           Specifies grading of matrix as follows:
*           0  => no grading
*           1  => matrix premultiplied by diag( DL )
*           2  => matrix postmultiplied by diag( DR )
*           3  => matrix premultiplied by diag( DL ) and
*                         postmultiplied by diag( DR )
*           4  => matrix premultiplied by diag( DL ) and
*                         postmultiplied by inv( diag( DL ) )
*           5  => matrix premultiplied by diag( DL ) and
*                         postmultiplied by diag( DL )
*           Not modified.
*
*  DL     - DOUBLE PRECISION array ( I or J, as appropriate )
*           Left scale factors for grading matrix.  Not modified.
*
*  DR     - DOUBLE PRECISION array ( I or J, as appropriate )
*           Right scale factors for grading matrix.  Not modified.
*
*  IPVTNG - INTEGER
*           On entry specifies pivoting permutations as follows:
*           0 => none.
*           1 => row pivoting.
*           2 => column pivoting.
*           3 => full pivoting, i.e., on both sides.
*           Not modified.
*
*  IWORK  - INTEGER array ( I or J, as appropriate )
*           This array specifies the permutation used. The
*           row (or column) in position K was originally in
*           position IWORK( K ).
*           This differs from IWORK for DLATM3. Not modified.
*
*  SPARSE - DOUBLE PRECISION    between 0. and 1.
*           On entry specifies the sparsity of the matrix
*           if sparse matix is to be generated.
*           SPARSE should lie between 0 and 1.
*           A uniform ( 0, 1 ) random number x is generated and
*           compared to SPARSE; if x is larger the matrix entry
*           is unchanged and if x is smaller the entry is set
*           to zero. Thus on the average a fraction SPARSE of the
*           entries will be set to zero.
*           Not modified.
*
*  =====================================================================
*
*     .. Parameters ..
*
      DOUBLE PRECISION   ZERO
      PARAMETER          ( ZERO = 0.0D0 )
*     ..
*
*     .. Local Scalars ..
*
      INTEGER            ISUB, JSUB
      DOUBLE PRECISION   TEMP
*     ..
*
*     .. External Functions ..
*
      DOUBLE PRECISION   DLARAN, DLARND
      EXTERNAL           DLARAN, DLARND
*     ..
*
*-----------------------------------------------------------------------
*
*     .. Executable Statements ..
*
*
*     Check for I and J in range
*
      IF( I.LT.1 .OR. I.GT.M .OR. J.LT.1 .OR. J.GT.N ) THEN
         DLATM2 = ZERO
         RETURN
      END IF
*
*     Check for banding
*
      IF( J.GT.I+KU .OR. J.LT.I-KL ) THEN
         DLATM2 = ZERO
         RETURN
      END IF
*
*     Check for sparsity
*
      IF( SPARSE.GT.ZERO ) THEN
         IF( DLARAN( ISEED ).LT.SPARSE ) THEN
            DLATM2 = ZERO
            RETURN
         END IF
      END IF
*
*     Compute subscripts depending on IPVTNG
*
      IF( IPVTNG.EQ.0 ) THEN
         ISUB = I
         JSUB = J
      ELSE IF( IPVTNG.EQ.1 ) THEN
         ISUB = IWORK( I )
         JSUB = J
      ELSE IF( IPVTNG.EQ.2 ) THEN
         ISUB = I
         JSUB = IWORK( J )
      ELSE IF( IPVTNG.EQ.3 ) THEN
         ISUB = IWORK( I )
         JSUB = IWORK( J )
      END IF
*
*     Compute entry and grade it according to IGRADE
*
      IF( ISUB.EQ.JSUB ) THEN
         TEMP = D( ISUB )
      ELSE
         TEMP = DLARND( IDIST, ISEED )
      END IF
      IF( IGRADE.EQ.1 ) THEN
         TEMP = TEMP*DL( ISUB )
      ELSE IF( IGRADE.EQ.2 ) THEN
         TEMP = TEMP*DR( JSUB )
      ELSE IF( IGRADE.EQ.3 ) THEN
         TEMP = TEMP*DL( ISUB )*DR( JSUB )
      ELSE IF( IGRADE.EQ.4 .AND. ISUB.NE.JSUB ) THEN
         TEMP = TEMP*DL( ISUB ) / DL( JSUB )
      ELSE IF( IGRADE.EQ.5 ) THEN
         TEMP = TEMP*DL( ISUB )*DL( JSUB )
      END IF
      DLATM2 = TEMP
      RETURN
*
*     End of DLATM2
*
      END
      SUBROUTINE DLAKF2( M, N, A, LDA, B, D, E, Z, LDZ )
*
*  -- LAPACK test routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     June 30, 1999
*
*     .. Scalar Arguments ..
      INTEGER            LDA, LDZ, M, N
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), B( LDA, * ), D( LDA, * ),
     $                   E( LDA, * ), Z( LDZ, * )
*     ..
*
*  Purpose
*  =======
*
*  Form the 2*M*N by 2*M*N matrix
*
*         Z = [ kron(In, A)  -kron(B', Im) ]
*             [ kron(In, D)  -kron(E', Im) ],
*
*  where In is the identity matrix of size n and X' is the transpose
*  of X. kron(X, Y) is the Kronecker product between the matrices X
*  and Y.
*
*  Arguments
*  =========
*
*  M       (input) INTEGER
*          Size of matrix, must be >= 1.
*
*  N       (input) INTEGER
*          Size of matrix, must be >= 1.
*
*  A       (input) DOUBLE PRECISION, dimension ( LDA, M )
*          The matrix A in the output matrix Z.
*
*  LDA     (input) INTEGER
*          The leading dimension of A, B, D, and E. ( LDA >= M+N )
*
*  B       (input) DOUBLE PRECISION, dimension ( LDA, N )
*  D       (input) DOUBLE PRECISION, dimension ( LDA, M )
*  E       (input) DOUBLE PRECISION, dimension ( LDA, N )
*          The matrices used in forming the output matrix Z.
*
*  Z       (output) DOUBLE PRECISION, dimension ( LDZ, 2*M*N )
*          The resultant Kronecker M*N*2 by M*N*2 matrix (see above.)
*
*  LDZ     (input) INTEGER
*          The leading dimension of Z. ( LDZ >= 2*M*N )
*
*  ====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO
      PARAMETER          ( ZERO = 0.0D+0 )
*     ..
*     .. Local Scalars ..
      INTEGER            I, IK, J, JK, L, MN, MN2
*     ..
*     .. External Subroutines ..
      EXTERNAL           DLASET
*     ..
*     .. Executable Statements ..
*
*     Initialize Z
*
      MN = M*N
      MN2 = 2*MN
      CALL DLASET( 'Full', MN2, MN2, ZERO, ZERO, Z, LDZ )
*
      IK = 1
      DO 50 L = 1, N
*
*        form kron(In, A)
*
         DO 20 I = 1, M
            DO 10 J = 1, M
               Z( IK+I-1, IK+J-1 ) = A( I, J )
   10       CONTINUE
   20    CONTINUE
*
*        form kron(In, D)
*
         DO 40 I = 1, M
            DO 30 J = 1, M
               Z( IK+MN+I-1, IK+J-1 ) = D( I, J )
   30       CONTINUE
   40    CONTINUE
*
         IK = IK + M
   50 CONTINUE
*
      IK = 1
      DO 90 L = 1, N
         JK = MN + 1
*
         DO 80 J = 1, N
*
*           form -kron(B', Im)
*
            DO 60 I = 1, M
               Z( IK+I-1, JK+I-1 ) = -B( J, L )
   60       CONTINUE
*
*           form -kron(E', Im)
*
            DO 70 I = 1, M
               Z( IK+MN+I-1, JK+I-1 ) = -E( J, L )
   70       CONTINUE
*
            JK = JK + M
   80    CONTINUE
*
         IK = IK + M
   90 CONTINUE
*
      RETURN
*
*     End of DLAKF2
*
      END
      SUBROUTINE DLATM5( PRTYPE, M, N, A, LDA, B, LDB, C, LDC, D, LDD,
     $                   E, LDE, F, LDF, R, LDR, L, LDL, ALPHA, QBLCKA,
     $                   QBLCKB )
*
*  -- LAPACK test routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     June 30, 1999
*
*     .. Scalar Arguments ..
      INTEGER            LDA, LDB, LDC, LDD, LDE, LDF, LDL, LDR, M, N,
     $                   PRTYPE, QBLCKA, QBLCKB
      DOUBLE PRECISION   ALPHA
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), B( LDB, * ), C( LDC, * ),
     $                   D( LDD, * ), E( LDE, * ), F( LDF, * ),
     $                   L( LDL, * ), R( LDR, * )
*     ..
*
*  Purpose
*  =======
*
*  DLATM5 generates matrices involved in the Generalized Sylvester
*  equation:
*
*      A * R - L * B = C
*      D * R - L * E = F
*
*  They also satisfy (the diagonalization condition)
*
*   [ I -L ] ( [ A  -C ], [ D -F ] ) [ I  R ] = ( [ A    ], [ D    ] )
*   [    I ] ( [     B ]  [    E ] ) [    I ]   ( [    B ]  [    E ] )
*
*
*  Arguments
*  =========
*
*  PRTYPE  (input) INTEGER
*          "Points" to a certian type of the matrices to generate
*          (see futher details).
*
*  M       (input) INTEGER
*          Specifies the order of A and D and the number of rows in
*          C, F,  R and L.
*
*  N       (input) INTEGER
*          Specifies the order of B and E and the number of columns in
*          C, F, R and L.
*
*  A       (output) DOUBLE PRECISION array, dimension (LDA, M).
*          On exit A M-by-M is initialized according to PRTYPE.
*
*  LDA     (input) INTEGER
*          The leading dimension of A.
*
*  B       (output) DOUBLE PRECISION array, dimension (LDB, N).
*          On exit B N-by-N is initialized according to PRTYPE.
*
*  LDB     (input) INTEGER
*          The leading dimension of B.
*
*  C       (output) DOUBLE PRECISION array, dimension (LDC, N).
*          On exit C M-by-N is initialized according to PRTYPE.
*
*  LDC     (input) INTEGER
*          The leading dimension of C.
*
*  D       (output) DOUBLE PRECISION array, dimension (LDD, M).
*          On exit D M-by-M is initialized according to PRTYPE.
*
*  LDD     (input) INTEGER
*          The leading dimension of D.
*
*  E       (output) DOUBLE PRECISION array, dimension (LDE, N).
*          On exit E N-by-N is initialized according to PRTYPE.
*
*  LDE     (input) INTEGER
*          The leading dimension of E.
*
*  F       (output) DOUBLE PRECISION array, dimension (LDF, N).
*          On exit F M-by-N is initialized according to PRTYPE.
*
*  LDF     (input) INTEGER
*          The leading dimension of F.
*
*  R       (output) DOUBLE PRECISION array, dimension (LDR, N).
*          On exit R M-by-N is initialized according to PRTYPE.
*
*  LDR     (input) INTEGER
*          The leading dimension of R.
*
*  L       (output) DOUBLE PRECISION array, dimension (LDL, N).
*          On exit L M-by-N is initialized according to PRTYPE.
*
*  LDL     (input) INTEGER
*          The leading dimension of L.
*
*  ALPHA   (input) DOUBLE PRECISION
*          Parameter used in generating PRTYPE = 1 and 5 matrices.
*
*  QBLCKA  (input) INTEGER
*          When PRTYPE = 3, specifies the distance between 2-by-2
*          blocks on the diagonal in A. Otherwise, QBLCKA is not
*          referenced. QBLCKA > 1.
*
*  QBLCKB  (input) INTEGER
*          When PRTYPE = 3, specifies the distance between 2-by-2
*          blocks on the diagonal in B. Otherwise, QBLCKB is not
*          referenced. QBLCKB > 1.
*
*
*  Further Details
*  ===============
*
*  PRTYPE = 1: A and B are Jordan blocks, D and E are identity matrices
*
*             A : if (i == j) then A(i, j) = 1.0
*                 if (j == i + 1) then A(i, j) = -1.0
*                 else A(i, j) = 0.0,            i, j = 1...M
*
*             B : if (i == j) then B(i, j) = 1.0 - ALPHA
*                 if (j == i + 1) then B(i, j) = 1.0
*                 else B(i, j) = 0.0,            i, j = 1...N
*
*             D : if (i == j) then D(i, j) = 1.0
*                 else D(i, j) = 0.0,            i, j = 1...M
*
*             E : if (i == j) then E(i, j) = 1.0
*                 else E(i, j) = 0.0,            i, j = 1...N
*
*             L =  R are chosen from [-10...10],
*                  which specifies the right hand sides (C, F).
*
*  PRTYPE = 2 or 3: Triangular and/or quasi- triangular.
*
*             A : if (i <= j) then A(i, j) = [-1...1]
*                 else A(i, j) = 0.0,             i, j = 1...M
*
*                 if (PRTYPE = 3) then
*                    A(k + 1, k + 1) = A(k, k)
*                    A(k + 1, k) = [-1...1]
*                    sign(A(k, k + 1) = -(sin(A(k + 1, k))
*                        k = 1, M - 1, QBLCKA
*
*             B : if (i <= j) then B(i, j) = [-1...1]
*                 else B(i, j) = 0.0,            i, j = 1...N
*
*                 if (PRTYPE = 3) then
*                    B(k + 1, k + 1) = B(k, k)
*                    B(k + 1, k) = [-1...1]
*                    sign(B(k, k + 1) = -(sign(B(k + 1, k))
*                        k = 1, N - 1, QBLCKB
*
*             D : if (i <= j) then D(i, j) = [-1...1].
*                 else D(i, j) = 0.0,            i, j = 1...M
*
*
*             E : if (i <= j) then D(i, j) = [-1...1]
*                 else E(i, j) = 0.0,            i, j = 1...N
*
*                 L, R are chosen from [-10...10],
*                 which specifies the right hand sides (C, F).
*
*  PRTYPE = 4 Full
*             A(i, j) = [-10...10]
*             D(i, j) = [-1...1]    i,j = 1...M
*             B(i, j) = [-10...10]
*             E(i, j) = [-1...1]    i,j = 1...N
*             R(i, j) = [-10...10]
*             L(i, j) = [-1...1]    i = 1..M ,j = 1...N
*
*             L, R specifies the right hand sides (C, F).
*
*  PRTYPE = 5 special case common and/or close eigs.
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ONE, ZERO, TWENTY, HALF, TWO
      PARAMETER          ( ONE = 1.0D+0, ZERO = 0.0D+0, TWENTY = 2.0D+1,
     $                   HALF = 0.5D+0, TWO = 2.0D+0 )
*     ..
*     .. Local Scalars ..
      INTEGER            I, J, K
      DOUBLE PRECISION   IMEPS, REEPS
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          DBLE, MOD, SIN
*     ..
*     .. External Subroutines ..
      EXTERNAL           DGEMM
*     ..
*     .. Executable Statements ..
*
      IF( PRTYPE.EQ.1 ) THEN
         DO 20 I = 1, M
            DO 10 J = 1, M
               IF( I.EQ.J ) THEN
                  A( I, J ) = ONE
                  D( I, J ) = ONE
               ELSE IF( I.EQ.J-1 ) THEN
                  A( I, J ) = -ONE
                  D( I, J ) = ZERO
               ELSE
                  A( I, J ) = ZERO
                  D( I, J ) = ZERO
               END IF
   10       CONTINUE
   20    CONTINUE
*
         DO 40 I = 1, N
            DO 30 J = 1, N
               IF( I.EQ.J ) THEN
                  B( I, J ) = ONE - ALPHA
                  E( I, J ) = ONE
               ELSE IF( I.EQ.J-1 ) THEN
                  B( I, J ) = ONE
                  E( I, J ) = ZERO
               ELSE
                  B( I, J ) = ZERO
                  E( I, J ) = ZERO
               END IF
   30       CONTINUE
   40    CONTINUE
*
         DO 60 I = 1, M
            DO 50 J = 1, N
               R( I, J ) = ( HALF-SIN( DBLE( I / J ) ) )*TWENTY
               L( I, J ) = R( I, J )
   50       CONTINUE
   60    CONTINUE
*
      ELSE IF( PRTYPE.EQ.2 .OR. PRTYPE.EQ.3 ) THEN
         DO 80 I = 1, M
            DO 70 J = 1, M
               IF( I.LE.J ) THEN
                  A( I, J ) = ( HALF-SIN( DBLE( I ) ) )*TWO
                  D( I, J ) = ( HALF-SIN( DBLE( I*J ) ) )*TWO
               ELSE
                  A( I, J ) = ZERO
                  D( I, J ) = ZERO
               END IF
   70       CONTINUE
   80    CONTINUE
*
         DO 100 I = 1, N
            DO 90 J = 1, N
               IF( I.LE.J ) THEN
                  B( I, J ) = ( HALF-SIN( DBLE( I+J ) ) )*TWO
                  E( I, J ) = ( HALF-SIN( DBLE( J ) ) )*TWO
               ELSE
                  B( I, J ) = ZERO
                  E( I, J ) = ZERO
               END IF
   90       CONTINUE
  100    CONTINUE
*
         DO 120 I = 1, M
            DO 110 J = 1, N
               R( I, J ) = ( HALF-SIN( DBLE( I*J ) ) )*TWENTY
               L( I, J ) = ( HALF-SIN( DBLE( I+J ) ) )*TWENTY
  110       CONTINUE
  120    CONTINUE
*
         IF( PRTYPE.EQ.3 ) THEN
            IF( QBLCKA.LE.1 )
     $         QBLCKA = 2
            DO 130 K = 1, M - 1, QBLCKA
               A( K+1, K+1 ) = A( K, K )
               A( K+1, K ) = -SIN( A( K, K+1 ) )
  130       CONTINUE
*
            IF( QBLCKB.LE.1 )
     $         QBLCKB = 2
            DO 140 K = 1, N - 1, QBLCKB
               B( K+1, K+1 ) = B( K, K )
               B( K+1, K ) = -SIN( B( K, K+1 ) )
  140       CONTINUE
         END IF
*
      ELSE IF( PRTYPE.EQ.4 ) THEN
         DO 160 I = 1, M
            DO 150 J = 1, M
               A( I, J ) = ( HALF-SIN( DBLE( I*J ) ) )*TWENTY
               D( I, J ) = ( HALF-SIN( DBLE( I+J ) ) )*TWO
  150       CONTINUE
  160    CONTINUE
*
         DO 180 I = 1, N
            DO 170 J = 1, N
               B( I, J ) = ( HALF-SIN( DBLE( I+J ) ) )*TWENTY
               E( I, J ) = ( HALF-SIN( DBLE( I*J ) ) )*TWO
  170       CONTINUE
  180    CONTINUE
*
         DO 200 I = 1, M
            DO 190 J = 1, N
               R( I, J ) = ( HALF-SIN( DBLE( J / I ) ) )*TWENTY
               L( I, J ) = ( HALF-SIN( DBLE( I*J ) ) )*TWO
  190       CONTINUE
  200    CONTINUE
*
      ELSE IF( PRTYPE.GE.5 ) THEN
         REEPS = HALF*TWO*TWENTY / ALPHA
         IMEPS = ( HALF-TWO ) / ALPHA
         DO 220 I = 1, M
            DO 210 J = 1, N
               R( I, J ) = ( HALF-SIN( DBLE( I*J ) ) )*ALPHA / TWENTY
               L( I, J ) = ( HALF-SIN( DBLE( I+J ) ) )*ALPHA / TWENTY
  210       CONTINUE
  220    CONTINUE
*
         DO 230 I = 1, M
            D( I, I ) = ONE
  230    CONTINUE
*
         DO 240 I = 1, M
            IF( I.LE.4 ) THEN
               A( I, I ) = ONE
               IF( I.GT.2 )
     $            A( I, I ) = ONE + REEPS
               IF( MOD( I, 2 ).NE.0 .AND. I.LT.M ) THEN
                  A( I, I+1 ) = IMEPS
               ELSE IF( I.GT.1 ) THEN
                  A( I, I-1 ) = -IMEPS
               END IF
            ELSE IF( I.LE.8 ) THEN
               IF( I.LE.6 ) THEN
                  A( I, I ) = REEPS
               ELSE
                  A( I, I ) = -REEPS
               END IF
               IF( MOD( I, 2 ).NE.0 .AND. I.LT.M ) THEN
                  A( I, I+1 ) = ONE
               ELSE IF( I.GT.1 ) THEN
                  A( I, I-1 ) = -ONE
               END IF
            ELSE
               A( I, I ) = ONE
               IF( MOD( I, 2 ).NE.0 .AND. I.LT.M ) THEN
                  A( I, I+1 ) = IMEPS*2
               ELSE IF( I.GT.1 ) THEN
                  A( I, I-1 ) = -IMEPS*2
               END IF
            END IF
  240    CONTINUE
*
         DO 250 I = 1, N
            E( I, I ) = ONE
            IF( I.LE.4 ) THEN
               B( I, I ) = -ONE
               IF( I.GT.2 )
     $            B( I, I ) = ONE - REEPS
               IF( MOD( I, 2 ).NE.0 .AND. I.LT.N ) THEN
                  B( I, I+1 ) = IMEPS
               ELSE IF( I.GT.1 ) THEN
                  B( I, I-1 ) = -IMEPS
               END IF
            ELSE IF( I.LE.8 ) THEN
               IF( I.LE.6 ) THEN
                  B( I, I ) = REEPS
               ELSE
                  B( I, I ) = -REEPS
               END IF
               IF( MOD( I, 2 ).NE.0 .AND. I.LT.N ) THEN
                  B( I, I+1 ) = ONE + IMEPS
               ELSE IF( I.GT.1 ) THEN
                  B( I, I-1 ) = -ONE - IMEPS
               END IF
            ELSE
               B( I, I ) = ONE - REEPS
               IF( MOD( I, 2 ).NE.0 .AND. I.LT.N ) THEN
                  B( I, I+1 ) = IMEPS*2
               ELSE IF( I.GT.1 ) THEN
                  B( I, I-1 ) = -IMEPS*2
               END IF
            END IF
  250    CONTINUE
      END IF
*
*     Compute rhs (C, F)
*
      CALL DGEMM( 'N', 'N', M, N, M, ONE, A, LDA, R, LDR, ZERO, C, LDC )
      CALL DGEMM( 'N', 'N', M, N, N, -ONE, L, LDL, B, LDB, ONE, C, LDC )
      CALL DGEMM( 'N', 'N', M, N, M, ONE, D, LDD, R, LDR, ZERO, F, LDF )
      CALL DGEMM( 'N', 'N', M, N, N, -ONE, L, LDL, E, LDE, ONE, F, LDF )
*
*     End of DLATM5
*
      END
      SUBROUTINE DLATM6( TYPE, N, A, LDA, B, X, LDX, Y, LDY, ALPHA,
     $                   BETA, WX, WY, S, DIF )
*
*  -- LAPACK test routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     October 31, 1999
*
*     .. Scalar Arguments ..
      INTEGER            LDA, LDX, LDY, N, TYPE
      DOUBLE PRECISION   ALPHA, BETA, WX, WY
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), B( LDA, * ), DIF( * ), S( * ),
     $                   X( LDX, * ), Y( LDY, * )
*     ..
*
*  Purpose
*  =======
*
*  DLATM6 generates test matrices for the generalized eigenvalue
*  problem, their corresponding right and left eigenvector matrices,
*  and also reciprocal condition numbers for all eigenvalues and
*  the reciprocal condition numbers of eigenvectors corresponding to
*  the 1th and 5th eigenvalues.
*
*  Test Matrices
*  =============
*
*  Two kinds of test matrix pairs
*
*        (A, B) = inverse(YH) * (Da, Db) * inverse(X)
*
*  are used in the tests:
*
*  Type 1:
*     Da = 1+a   0    0    0    0    Db = 1   0   0   0   0
*           0   2+a   0    0    0         0   1   0   0   0
*           0    0   3+a   0    0         0   0   1   0   0
*           0    0    0   4+a   0         0   0   0   1   0
*           0    0    0    0   5+a ,      0   0   0   0   1 , and
*
*  Type 2:
*     Da =  1   -1    0    0    0    Db = 1   0   0   0   0
*           1    1    0    0    0         0   1   0   0   0
*           0    0    1    0    0         0   0   1   0   0
*           0    0    0   1+a  1+b        0   0   0   1   0
*           0    0    0  -1-b  1+a ,      0   0   0   0   1 .
*
*  In both cases the same inverse(YH) and inverse(X) are used to compute
*  (A, B), giving the exact eigenvectors to (A,B) as (YH, X):
*
*  YH:  =  1    0   -y    y   -y    X =  1   0  -x  -x   x
*          0    1   -y    y   -y         0   1   x  -x  -x
*          0    0    1    0    0         0   0   1   0   0
*          0    0    0    1    0         0   0   0   1   0
*          0    0    0    0    1,        0   0   0   0   1 ,
*
* where a, b, x and y will have all values independently of each other.
*
*  Arguments
*  =========
*
*  TYPE    (input) INTEGER
*          Specifies the problem type (see futher details).
*
*  N       (input) INTEGER
*          Size of the matrices A and B.
*
*  A       (output) DOUBLE PRECISION array, dimension (LDA, N).
*          On exit A N-by-N is initialized according to TYPE.
*
*  LDA     (input) INTEGER
*          The leading dimension of A and of B.
*
*  B       (output) DOUBLE PRECISION array, dimension (LDA, N).
*          On exit B N-by-N is initialized according to TYPE.
*
*  X       (output) DOUBLE PRECISION array, dimension (LDX, N).
*          On exit X is the N-by-N matrix of right eigenvectors.
*
*  LDX     (input) INTEGER
*          The leading dimension of X.
*
*  Y       (output) DOUBLE PRECISION array, dimension (LDY, N).
*          On exit Y is the N-by-N matrix of left eigenvectors.
*
*  LDY     (input) INTEGER
*          The leading dimension of Y.
*
*  ALPHA   (input) DOUBLE PRECISION
*  BETA    (input) DOUBLE PRECISION
*          Weighting constants for matrix A.
*
*  WX      (input) DOUBLE PRECISION
*          Constant for right eigenvector matrix.
*
*  WY      (input) DOUBLE PRECISION
*          Constant for left eigenvector matrix.
*
*  S       (output) DOUBLE PRECISION array, dimension (N)
*          S(i) is the reciprocal condition number for eigenvalue i.
*
*  DIF     (output) DOUBLE PRECISION array, dimension (N)
*          DIF(i) is the reciprocal condition number for eigenvector i.
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE, TWO, THREE
      PARAMETER          ( ZERO = 0.0D+0, ONE = 1.0D+0, TWO = 2.0D+0,
     $                   THREE = 3.0D+0 )
*     ..
*     .. Local Scalars ..
      INTEGER            I, INFO, J
*     ..
*     .. Local Arrays ..
      DOUBLE PRECISION   WORK( 100 ), Z( 12, 12 )
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          DBLE, SQRT
*     ..
*     .. External Subroutines ..
      EXTERNAL           DGESVD, DLACPY, DLAKF2
*     ..
*     .. Executable Statements ..
*
*     Generate test problem ...
*     (Da, Db) ...
*
      DO 20 I = 1, N
         DO 10 J = 1, N
*
            IF( I.EQ.J ) THEN
               A( I, I ) = DBLE( I ) + ALPHA
               B( I, I ) = ONE
            ELSE
               A( I, J ) = ZERO
               B( I, J ) = ZERO
            END IF
*
   10    CONTINUE
   20 CONTINUE
*
*     Form X and Y
*
      CALL DLACPY( 'F', N, N, B, LDA, Y, LDY )
      Y( 3, 1 ) = -WY
      Y( 4, 1 ) = WY
      Y( 5, 1 ) = -WY
      Y( 3, 2 ) = -WY
      Y( 4, 2 ) = WY
      Y( 5, 2 ) = -WY
*
      CALL DLACPY( 'F', N, N, B, LDA, X, LDX )
      X( 1, 3 ) = -WX
      X( 1, 4 ) = -WX
      X( 1, 5 ) = WX
      X( 2, 3 ) = WX
      X( 2, 4 ) = -WX
      X( 2, 5 ) = -WX
*
*     Form (A, B)
*
      B( 1, 3 ) = WX + WY
      B( 2, 3 ) = -WX + WY
      B( 1, 4 ) = WX - WY
      B( 2, 4 ) = WX - WY
      B( 1, 5 ) = -WX + WY
      B( 2, 5 ) = WX + WY
      IF( TYPE.EQ.1 ) THEN
         A( 1, 3 ) = WX*A( 1, 1 ) + WY*A( 3, 3 )
         A( 2, 3 ) = -WX*A( 2, 2 ) + WY*A( 3, 3 )
         A( 1, 4 ) = WX*A( 1, 1 ) - WY*A( 4, 4 )
         A( 2, 4 ) = WX*A( 2, 2 ) - WY*A( 4, 4 )
         A( 1, 5 ) = -WX*A( 1, 1 ) + WY*A( 5, 5 )
         A( 2, 5 ) = WX*A( 2, 2 ) + WY*A( 5, 5 )
      ELSE IF( TYPE.EQ.2 ) THEN
         A( 1, 3 ) = TWO*WX + WY
         A( 2, 3 ) = WY
         A( 1, 4 ) = -WY*( TWO+ALPHA+BETA )
         A( 2, 4 ) = TWO*WX - WY*( TWO+ALPHA+BETA )
         A( 1, 5 ) = -TWO*WX + WY*( ALPHA-BETA )
         A( 2, 5 ) = WY*( ALPHA-BETA )
         A( 1, 1 ) = ONE
         A( 1, 2 ) = -ONE
         A( 2, 1 ) = ONE
         A( 2, 2 ) = A( 1, 1 )
         A( 3, 3 ) = ONE
         A( 4, 4 ) = ONE + ALPHA
         A( 4, 5 ) = ONE + BETA
         A( 5, 4 ) = -A( 4, 5 )
         A( 5, 5 ) = A( 4, 4 )
      END IF
*
*     Compute condition numbers
*
      IF( TYPE.EQ.1 ) THEN
*
         S( 1 ) = ONE / SQRT( ( ONE+THREE*WY*WY ) /
     $            ( ONE+A( 1, 1 )*A( 1, 1 ) ) )
         S( 2 ) = ONE / SQRT( ( ONE+THREE*WY*WY ) /
     $            ( ONE+A( 2, 2 )*A( 2, 2 ) ) )
         S( 3 ) = ONE / SQRT( ( ONE+TWO*WX*WX ) /
     $            ( ONE+A( 3, 3 )*A( 3, 3 ) ) )
         S( 4 ) = ONE / SQRT( ( ONE+TWO*WX*WX ) /
     $            ( ONE+A( 4, 4 )*A( 4, 4 ) ) )
         S( 5 ) = ONE / SQRT( ( ONE+TWO*WX*WX ) /
     $            ( ONE+A( 5, 5 )*A( 5, 5 ) ) )
*
         CALL DLAKF2( 1, 4, A, LDA, A( 2, 2 ), B, B( 2, 2 ), Z, 12 )
         CALL DGESVD( 'N', 'N', 8, 8, Z, 12, WORK, WORK( 9 ), 1,
     $                WORK( 10 ), 1, WORK( 11 ), 40, INFO )
         DIF( 1 ) = WORK( 8 )
*
         CALL DLAKF2( 4, 1, A, LDA, A( 5, 5 ), B, B( 5, 5 ), Z, 12 )
         CALL DGESVD( 'N', 'N', 8, 8, Z, 12, WORK, WORK( 9 ), 1,
     $                WORK( 10 ), 1, WORK( 11 ), 40, INFO )
         DIF( 5 ) = WORK( 8 )
*
      ELSE IF( TYPE.EQ.2 ) THEN
*
         S( 1 ) = ONE / SQRT( ONE / THREE+WY*WY )
         S( 2 ) = S( 1 )
         S( 3 ) = ONE / SQRT( ONE / TWO+WX*WX )
         S( 4 ) = ONE / SQRT( ( ONE+TWO*WX*WX ) /
     $            ( ONE+( ONE+ALPHA )*( ONE+ALPHA )+( ONE+BETA )*( ONE+
     $            BETA ) ) )
         S( 5 ) = S( 4 )
*
         CALL DLAKF2( 2, 3, A, LDA, A( 3, 3 ), B, B( 3, 3 ), Z, 12 )
         CALL DGESVD( 'N', 'N', 12, 12, Z, 12, WORK, WORK( 13 ), 1,
     $                WORK( 14 ), 1, WORK( 15 ), 60, INFO )
         DIF( 1 ) = WORK( 12 )
*
         CALL DLAKF2( 3, 2, A, LDA, A( 4, 4 ), B, B( 4, 4 ), Z, 12 )
         CALL DGESVD( 'N', 'N', 12, 12, Z, 12, WORK, WORK( 13 ), 1,
     $                WORK( 14 ), 1, WORK( 15 ), 60, INFO )
         DIF( 5 ) = WORK( 12 )
*
      END IF
*
      RETURN
*
*     End of DLATM6
*
      END
