      SUBROUTINE DGELS( TRANS, M, N, NRHS, A, LDA, B, LDB, WORK, LWORK,
     $                  INFO )
*
*  -- LAPACK driver routine (instrumented to count ops, version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     June 30, 1999
*
*     .. Scalar Arguments ..
      CHARACTER          TRANS
      INTEGER            INFO, LDA, LDB, LWORK, M, N, NRHS
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), B( LDB, * ), WORK( * )
*     ..
*     Common block to return operation count.
*     .. Common blocks ..
      COMMON             / LSTIME / OPCNT, TIMNG
*     ..
*     .. Arrays in Common ..
      DOUBLE PRECISION   OPCNT( 6 ), TIMNG( 6 )
*     ..
*
*  Purpose
*  =======
*
*  DGELS solves overdetermined or underdetermined real linear systems
*  involving an M-by-N matrix A, or its transpose, using a QR or LQ
*  factorization of A.  It is assumed that A has full rank.
*
*  The following options are provided:
*
*  1. If TRANS = 'N' and m >= n:  find the least squares solution of
*     an overdetermined system, i.e., solve the least squares problem
*                  minimize || B - A*X ||.
*
*  2. If TRANS = 'N' and m < n:  find the minimum norm solution of
*     an underdetermined system A * X = B.
*
*  3. If TRANS = 'T' and m >= n:  find the minimum norm solution of
*     an undetermined system A**T * X = B.
*
*  4. If TRANS = 'T' and m < n:  find the least squares solution of
*     an overdetermined system, i.e., solve the least squares problem
*                  minimize || B - A**T * X ||.
*
*  Several right hand side vectors b and solution vectors x can be
*  handled in a single call; they are stored as the columns of the
*  M-by-NRHS right hand side matrix B and the N-by-NRHS solution
*  matrix X.
*
*  Arguments
*  =========
*
*  TRANS   (input) CHARACTER
*          = 'N': the linear system involves A;
*          = 'T': the linear system involves A**T.
*
*  M       (input) INTEGER
*          The number of rows of the matrix A.  M >= 0.
*
*  N       (input) INTEGER
*          The number of columns of the matrix A.  N >= 0.
*
*  NRHS    (input) INTEGER
*          The number of right hand sides, i.e., the number of
*          columns of the matrices B and X. NRHS >=0.
*
*  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
*          On entry, the M-by-N matrix A.
*          On exit,
*            if M >= N, A is overwritten by details of its QR
*                       factorization as returned by DGEQRF;
*            if M <  N, A is overwritten by details of its LQ
*                       factorization as returned by DGELQF.
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A.  LDA >= max(1,M).
*
*  B       (input/output) DOUBLE PRECISION array, dimension (LDB,NRHS)
*          On entry, the matrix B of right hand side vectors, stored
*          columnwise; B is M-by-NRHS if TRANS = 'N', or N-by-NRHS
*          if TRANS = 'T'.
*          On exit, B is overwritten by the solution vectors, stored
*          columnwise:
*          if TRANS = 'N' and m >= n, rows 1 to n of B contain the least
*          squares solution vectors; the residual sum of squares for the
*          solution in each column is given by the sum of squares of
*          elements N+1 to M in that column;
*          if TRANS = 'N' and m < n, rows 1 to N of B contain the
*          minimum norm solution vectors;
*          if TRANS = 'T' and m >= n, rows 1 to M of B contain the
*          minimum norm solution vectors;
*          if TRANS = 'T' and m < n, rows 1 to M of B contain the
*          least squares solution vectors; the residual sum of squares
*          for the solution in each column is given by the sum of
*          squares of elements M+1 to N in that column.
*
*  LDB     (input) INTEGER
*          The leading dimension of the array B. LDB >= MAX(1,M,N).
*
*  WORK    (workspace/output) DOUBLE PRECISION array, dimension (LWORK)
*          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
*
*  LWORK   (input) INTEGER
*          The dimension of the array WORK.
*          LWORK >= max( 1, MN + max( MN, NRHS ) ).
*          For optimal performance,
*          LWORK >= max( 1, MN + max( MN, NRHS )*NB ).
*          where MN = min(M,N) and NB is the optimum block size.
*
*          If LWORK = -1, then a workspace query is assumed; the routine
*          only calculates the optimal size of the WORK array, returns
*          this value as the first entry of the WORK array, and no error
*          message related to LWORK is issued by XERBLA.
*
*  INFO    (output) INTEGER
*          = 0:  successful exit
*          < 0:  if INFO = -i, the i-th argument had an illegal value
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE
      PARAMETER          ( ZERO = 0.0D0, ONE = 1.0D0 )
*     ..
*     .. Local Scalars ..
      LOGICAL            LQUERY, TPSD
      INTEGER            BROW, GELQF, GELS, GEQRF, I, IASCL, IBSCL, J,
     $                   MN, NB, ORMLQ, ORMQR, SCLLEN, TRSM, WSIZE
      DOUBLE PRECISION   ANRM, BIGNUM, BNRM, SMLNUM, T1, T2
*     ..
*     .. Local Arrays ..
      DOUBLE PRECISION   RWORK( 1 )
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      INTEGER            ILAENV
      DOUBLE PRECISION   DLAMCH, DLANGE, DOPBL3, DOPLA, DSECND
      EXTERNAL           LSAME, ILAENV, DLAMCH, DLANGE, DOPBL3, DOPLA,
     $                   DSECND
*     ..
*     .. External Subroutines ..
      EXTERNAL           DGELQF, DGEQRF, DLABAD, DLASCL, DLASET, DORMLQ,
     $                   DORMQR, DTRSM, XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          DBLE, MAX, MIN
*     ..
*     .. Data statements ..
      DATA               GELQF / 2 / , GELS / 1 / , GEQRF / 2 / ,
     $                   ORMLQ / 3 / , ORMQR / 3 / , TRSM / 4 /
*     ..
*     .. Executable Statements ..
*
*     Test the input arguments.
*
      INFO = 0
      MN = MIN( M, N )
      LQUERY = ( LWORK.EQ.-1 )
      IF( .NOT.( LSAME( TRANS, 'N' ) .OR. LSAME( TRANS, 'T' ) ) ) THEN
         INFO = -1
      ELSE IF( M.LT.0 ) THEN
         INFO = -2
      ELSE IF( N.LT.0 ) THEN
         INFO = -3
      ELSE IF( NRHS.LT.0 ) THEN
         INFO = -4
      ELSE IF( LDA.LT.MAX( 1, M ) ) THEN
         INFO = -6
      ELSE IF( LDB.LT.MAX( 1, M, N ) ) THEN
         INFO = -8
      ELSE IF( LWORK.LT.MAX( 1, MN+MAX( MN, NRHS ) ) .AND. .NOT.LQUERY )
     $          THEN
         INFO = -10
      END IF
*
*     Figure out optimal block size
*
      IF( INFO.EQ.0 .OR. INFO.EQ.-10 ) THEN
*
         TPSD = .TRUE.
         IF( LSAME( TRANS, 'N' ) )
     $      TPSD = .FALSE.
*
         IF( M.GE.N ) THEN
            NB = ILAENV( 1, 'DGEQRF', ' ', M, N, -1, -1 )
            IF( TPSD ) THEN
               NB = MAX( NB, ILAENV( 1, 'DORMQR', 'LN', M, NRHS, N,
     $              -1 ) )
            ELSE
               NB = MAX( NB, ILAENV( 1, 'DORMQR', 'LT', M, NRHS, N,
     $              -1 ) )
            END IF
         ELSE
            NB = ILAENV( 1, 'DGELQF', ' ', M, N, -1, -1 )
            IF( TPSD ) THEN
               NB = MAX( NB, ILAENV( 1, 'DORMLQ', 'LT', N, NRHS, M,
     $              -1 ) )
            ELSE
               NB = MAX( NB, ILAENV( 1, 'DORMLQ', 'LN', N, NRHS, M,
     $              -1 ) )
            END IF
         END IF
*
         WSIZE = MAX( 1, MN+MAX( MN, NRHS )*NB )
         WORK( 1 ) = DBLE( WSIZE )
*
      END IF
*
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DGELS ', -INFO )
         RETURN
      ELSE IF( LQUERY ) THEN
         RETURN
      END IF
*
*     Quick return if possible
*
      IF( MIN( M, N, NRHS ).EQ.0 ) THEN
         CALL DLASET( 'Full', MAX( M, N ), NRHS, ZERO, ZERO, B, LDB )
         RETURN
      END IF
*
*     Get machine parameters
*
      OPCNT( GELS ) = OPCNT( GELS ) + DBLE( 2 )
      SMLNUM = DLAMCH( 'S' ) / DLAMCH( 'P' )
      BIGNUM = ONE / SMLNUM
      CALL DLABAD( SMLNUM, BIGNUM )
*
*     Scale A, B if max element outside range [SMLNUM,BIGNUM]
*
      ANRM = DLANGE( 'M', M, N, A, LDA, RWORK )
      IASCL = 0
      IF( ANRM.GT.ZERO .AND. ANRM.LT.SMLNUM ) THEN
*
*        Scale matrix norm up to SMLNUM
*
         OPCNT( GELS ) = OPCNT( GELS ) + DBLE( M*N )
         CALL DLASCL( 'G', 0, 0, ANRM, SMLNUM, M, N, A, LDA, INFO )
         IASCL = 1
      ELSE IF( ANRM.GT.BIGNUM ) THEN
*
*        Scale matrix norm down to BIGNUM
*
         OPCNT( GELS ) = OPCNT( GELS ) + DBLE( M*N )
         CALL DLASCL( 'G', 0, 0, ANRM, BIGNUM, M, N, A, LDA, INFO )
         IASCL = 2
      ELSE IF( ANRM.EQ.ZERO ) THEN
*
*        Matrix all zero. Return zero solution.
*
         CALL DLASET( 'F', MAX( M, N ), NRHS, ZERO, ZERO, B, LDB )
         GO TO 50
      END IF
*
      BROW = M
      IF( TPSD )
     $   BROW = N
      BNRM = DLANGE( 'M', BROW, NRHS, B, LDB, RWORK )
      IBSCL = 0
      IF( BNRM.GT.ZERO .AND. BNRM.LT.SMLNUM ) THEN
*
*        Scale matrix norm up to SMLNUM
*
         OPCNT( GELS ) = OPCNT( GELS ) + DBLE( BROW*NRHS )
         CALL DLASCL( 'G', 0, 0, BNRM, SMLNUM, BROW, NRHS, B, LDB,
     $                INFO )
         IBSCL = 1
      ELSE IF( BNRM.GT.BIGNUM ) THEN
*
*        Scale matrix norm down to BIGNUM
*
         OPCNT( GELS ) = OPCNT( GELS ) + DBLE( BROW*NRHS )
         CALL DLASCL( 'G', 0, 0, BNRM, BIGNUM, BROW, NRHS, B, LDB,
     $                INFO )
         IBSCL = 2
      END IF
*
      IF( M.GE.N ) THEN
*
*        compute QR factorization of A
*
         NB = ILAENV( 1, 'DGEQRF', ' ', M, N, -1, -1 )
         OPCNT( GEQRF ) = OPCNT( GEQRF ) +
     $                    DOPLA( 'DGEQRF', M, N, 0, 0, NB )
         T1 = DSECND( )
         CALL DGEQRF( M, N, A, LDA, WORK( 1 ), WORK( MN+1 ), LWORK-MN,
     $                INFO )
         T2 = DSECND( )
         TIMNG( GEQRF ) = TIMNG( GEQRF ) + ( T2-T1 )
*
*        workspace at least N, optimally N*NB
*
         IF( .NOT.TPSD ) THEN
*
*           Least-Squares Problem min || A * X - B ||
*
*           B(1:M,1:NRHS) := Q' * B(1:M,1:NRHS)
*
            NB = ILAENV( 1, 'DORMQR', 'LT', M, NRHS, N, -1 )
            OPCNT( ORMQR ) = OPCNT( ORMQR ) +
     $                       DOPLA( 'DORMQR', M, NRHS, N, 0, NB )
            T1 = DSECND( )
            CALL DORMQR( 'Left', 'Transpose', M, NRHS, N, A, LDA,
     $                   WORK( 1 ), B, LDB, WORK( MN+1 ), LWORK-MN,
     $                   INFO )
            T2 = DSECND( )
            TIMNG( ORMQR ) = TIMNG( ORMQR ) + ( T2-T1 )
*
*           workspace at least NRHS, optimally NRHS*NB
*
*           B(1:N,1:NRHS) := inv(R) * B(1:N,1:NRHS)
*
            OPCNT( TRSM ) = OPCNT( TRSM ) +
     $                      DOPBL3( 'DTRSM ', N, NRHS, 0 )
            T1 = DSECND( )
            CALL DTRSM( 'Left', 'Upper', 'No transpose', 'Non-unit', N,
     $                  NRHS, ONE, A, LDA, B, LDB )
            T2 = DSECND( )
            TIMNG( TRSM ) = TIMNG( TRSM ) + ( T2-T1 )
*
            SCLLEN = N
*
         ELSE
*
*           Overdetermined system of equations A' * X = B
*
*           B(1:N,1:NRHS) := inv(R') * B(1:N,1:NRHS)
*
            OPCNT( TRSM ) = OPCNT( TRSM ) +
     $                      DOPBL3( 'DTRSM ', N, NRHS, 0 )
            T1 = DSECND( )
            CALL DTRSM( 'Left', 'Upper', 'Transpose', 'Non-unit', N,
     $                  NRHS, ONE, A, LDA, B, LDB )
            T2 = DSECND( )
            TIMNG( TRSM ) = TIMNG( TRSM ) + ( T2-T1 )
*
*           B(N+1:M,1:NRHS) = ZERO
*
            DO 20 J = 1, NRHS
               DO 10 I = N + 1, M
                  B( I, J ) = ZERO
   10          CONTINUE
   20       CONTINUE
*
*           B(1:M,1:NRHS) := Q(1:N,:) * B(1:N,1:NRHS)
*
            NB = ILAENV( 1, 'DORMQR', 'LN', M, NRHS, N, -1 )
            OPCNT( ORMQR ) = OPCNT( ORMQR ) +
     $                       DOPLA( 'DORMQR', M, NRHS, N, 0, NB )
            T1 = DSECND( )
            CALL DORMQR( 'Left', 'No transpose', M, NRHS, N, A, LDA,
     $                   WORK( 1 ), B, LDB, WORK( MN+1 ), LWORK-MN,
     $                   INFO )
            T2 = DSECND( )
            TIMNG( ORMQR ) = TIMNG( ORMQR ) + ( T2-T1 )
*
*           workspace at least NRHS, optimally NRHS*NB
*
            SCLLEN = M
*
         END IF
*
      ELSE
*
*        Compute LQ factorization of A
*
         NB = ILAENV( 1, 'DGELQF', ' ', M, N, -1, -1 )
         OPCNT( GELQF ) = OPCNT( GELQF ) +
     $                    DOPLA( 'DGELQF', M, N, 0, 0, NB )
         T1 = DSECND( )
         CALL DGELQF( M, N, A, LDA, WORK( 1 ), WORK( MN+1 ), LWORK-MN,
     $                INFO )
         T2 = DSECND( )
         TIMNG( GELQF ) = TIMNG( GELQF ) + ( T2-T1 )
*
*        workspace at least M, optimally M*NB.
*
         IF( .NOT.TPSD ) THEN
*
*           underdetermined system of equations A * X = B
*
*           B(1:M,1:NRHS) := inv(L) * B(1:M,1:NRHS)
*
            OPCNT( TRSM ) = OPCNT( TRSM ) +
     $                      DOPBL3( 'DTRSM ', M, NRHS, 0 )
            T1 = DSECND( )
            CALL DTRSM( 'Left', 'Lower', 'No transpose', 'Non-unit', M,
     $                  NRHS, ONE, A, LDA, B, LDB )
            T2 = DSECND( )
            TIMNG( TRSM ) = TIMNG( TRSM ) + ( T2-T1 )
*
*           B(M+1:N,1:NRHS) = 0
*
            DO 40 J = 1, NRHS
               DO 30 I = M + 1, N
                  B( I, J ) = ZERO
   30          CONTINUE
   40       CONTINUE
*
*           B(1:N,1:NRHS) := Q(1:N,:)' * B(1:M,1:NRHS)
*
            NB = ILAENV( 1, 'DORMLQ', 'LT', N, NRHS, M, -1 )
            OPCNT( ORMLQ ) = OPCNT( ORMLQ ) +
     $                       DOPLA( 'DORMLQ', N, NRHS, M, 0, NB )
            T1 = DSECND( )
            CALL DORMLQ( 'Left', 'Transpose', N, NRHS, M, A, LDA,
     $                   WORK( 1 ), B, LDB, WORK( MN+1 ), LWORK-MN,
     $                   INFO )
            T2 = DSECND( )
            TIMNG( ORMLQ ) = TIMNG( ORMLQ ) + ( T2-T1 )
*
*           workspace at least NRHS, optimally NRHS*NB
*
            SCLLEN = N
*
         ELSE
*
*           overdetermined system min || A' * X - B ||
*
*           B(1:N,1:NRHS) := Q * B(1:N,1:NRHS)
*
            NB = ILAENV( 1, 'DORMLQ', 'LN', N, NRHS, M, -1 )
            OPCNT( ORMLQ ) = OPCNT( ORMLQ ) +
     $                       DOPLA( 'DORMLQ', N, NRHS, M, 0, NB )
            T1 = DSECND( )
            CALL DORMLQ( 'Left', 'No transpose', N, NRHS, M, A, LDA,
     $                   WORK( 1 ), B, LDB, WORK( MN+1 ), LWORK-MN,
     $                   INFO )
            T2 = DSECND( )
            TIMNG( ORMLQ ) = TIMNG( ORMLQ ) + ( T2-T1 )
*
*           workspace at least NRHS, optimally NRHS*NB
*
*           B(1:M,1:NRHS) := inv(L') * B(1:M,1:NRHS)
*
            OPCNT( TRSM ) = OPCNT( TRSM ) +
     $                      DOPBL3( 'DTRSM ', M, NRHS, 0 )
            T1 = DSECND( )
            CALL DTRSM( 'Left', 'Lower', 'Transpose', 'Non-unit', M,
     $                  NRHS, ONE, A, LDA, B, LDB )
            T2 = DSECND( )
            TIMNG( TRSM ) = TIMNG( TRSM ) + ( T2-T1 )
*
            SCLLEN = M
*
         END IF
*
      END IF
*
*     Undo scaling
*
      IF( IASCL.EQ.1 ) THEN
         OPCNT( GELS ) = OPCNT( GELS ) + DBLE( SCLLEN*NRHS )
         CALL DLASCL( 'G', 0, 0, ANRM, SMLNUM, SCLLEN, NRHS, B, LDB,
     $                INFO )
      ELSE IF( IASCL.EQ.2 ) THEN
         OPCNT( GELS ) = OPCNT( GELS ) + DBLE( SCLLEN*NRHS )
         CALL DLASCL( 'G', 0, 0, ANRM, BIGNUM, SCLLEN, NRHS, B, LDB,
     $                INFO )
      END IF
      IF( IBSCL.EQ.1 ) THEN
         OPCNT( GELS ) = OPCNT( GELS ) + DBLE( SCLLEN*NRHS )
         CALL DLASCL( 'G', 0, 0, SMLNUM, BNRM, SCLLEN, NRHS, B, LDB,
     $                INFO )
      ELSE IF( IBSCL.EQ.2 ) THEN
         OPCNT( GELS ) = OPCNT( GELS ) + DBLE( SCLLEN*NRHS )
         CALL DLASCL( 'G', 0, 0, BIGNUM, BNRM, SCLLEN, NRHS, B, LDB,
     $                INFO )
      END IF
*
   50 CONTINUE
      WORK( 1 ) = DBLE( WSIZE )
*
      RETURN
*
*     End of DGELS
*
      END
      SUBROUTINE DGELSD( M, N, NRHS, A, LDA, B, LDB, S, RCOND, RANK,
     $                   WORK, LWORK, IWORK, INFO )
*
*  -- LAPACK driver routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     October 31, 1999
*
*     .. Scalar Arguments ..
      INTEGER            INFO, LDA, LDB, LWORK, M, N, NRHS, RANK
      DOUBLE PRECISION   RCOND
*     ..
*     .. Array Arguments ..
      INTEGER            IWORK( * )
      DOUBLE PRECISION   A( LDA, * ), B( LDB, * ), S( * ), WORK( * )
*     ..
*
*  Purpose
*  =======
*
*  DGELSD computes the minimum-norm solution to a real linear least
*  squares problem:
*      minimize 2-norm(| b - A*x |)
*  using the singular value decomposition (SVD) of A. A is an M-by-N
*  matrix which may be rank-deficient.
*
*  Several right hand side vectors b and solution vectors x can be
*  handled in a single call; they are stored as the columns of the
*  M-by-NRHS right hand side matrix B and the N-by-NRHS solution
*  matrix X.
*
*  The problem is solved in three steps:
*  (1) Reduce the coefficient matrix A to bidiagonal form with
*      Householder transformations, reducing the original problem
*      into a "bidiagonal least squares problem" (BLS)
*  (2) Solve the BLS using a divide and conquer approach.
*  (3) Apply back all the Householder tranformations to solve
*      the original least squares problem.
*
*  The effective rank of A is determined by treating as zero those
*  singular values which are less than RCOND times the largest singular
*  value.
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
*  M       (input) INTEGER
*          The number of rows of A. M >= 0.
*
*  N       (input) INTEGER
*          The number of columns of A. N >= 0.
*
*  NRHS    (input) INTEGER
*          The number of right hand sides, i.e., the number of columns
*          of the matrices B and X. NRHS >= 0.
*
*  A       (input) DOUBLE PRECISION array, dimension (LDA,N)
*          On entry, the M-by-N matrix A.
*          On exit, A has been destroyed.
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A.  LDA >= max(1,M).
*
*  B       (input/output) DOUBLE PRECISION array, dimension (LDB,NRHS)
*          On entry, the M-by-NRHS right hand side matrix B.
*          On exit, B is overwritten by the N-by-NRHS solution
*          matrix X.  If m >= n and RANK = n, the residual
*          sum-of-squares for the solution in the i-th column is given
*          by the sum of squares of elements n+1:m in that column.
*
*  LDB     (input) INTEGER
*          The leading dimension of the array B. LDB >= max(1,max(M,N)).
*
*  S       (output) DOUBLE PRECISION array, dimension (min(M,N))
*          The singular values of A in decreasing order.
*          The condition number of A in the 2-norm = S(1)/S(min(m,n)).
*
*  RCOND   (input) DOUBLE PRECISION
*          RCOND is used to determine the effective rank of A.
*          Singular values S(i) <= RCOND*S(1) are treated as zero.
*          If RCOND < 0, machine precision is used instead.
*
*  RANK    (output) INTEGER
*          The effective rank of A, i.e., the number of singular values
*          which are greater than RCOND*S(1).
*
*  WORK    (workspace/output) DOUBLE PRECISION array, dimension (LWORK)
*          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
*
*  LWORK   (input) INTEGER
*          The dimension of the array WORK. LWORK must be at least 1.
*          The exact minimum amount of workspace needed depends on M,
*          N and NRHS. As long as LWORK is at least
*              12*N + 2*N*SMLSIZ + 8*N*NLVL + N*NRHS + (SMLSIZ+1)**2,
*          if M is greater than or equal to N or
*              12*M + 2*M*SMLSIZ + 8*M*NLVL + M*NRHS + (SMLSIZ+1)**2,
*          if M is less than N, the code will execute correctly.
*          SMLSIZ is returned by ILAENV and is equal to the maximum
*          size of the subproblems at the bottom of the computation
*          tree (usually about 25), and
*             NLVL = MAX( 0, INT( LOG_2( MIN( M,N )/(SMLSIZ+1) ) ) + 1 )
*          For good performance, LWORK should generally be larger.
*
*          If LWORK = -1, then a workspace query is assumed; the routine
*          only calculates the optimal size of the WORK array, returns
*          this value as the first entry of the WORK array, and no error
*          message related to LWORK is issued by XERBLA.
*
*  IWORK   (workspace) INTEGER array, dimension (LIWORK)
*          LIWORK >= 3 * MINMN * NLVL + 11 * MINMN,
*          where MINMN = MIN( M,N ).
*
*  INFO    (output) INTEGER
*          = 0:  successful exit
*          < 0:  if INFO = -i, the i-th argument had an illegal value.
*          > 0:  the algorithm for computing the SVD failed to converge;
*                if INFO = i, i off-diagonal elements of an intermediate
*                bidiagonal form did not converge to zero.
*
*  Further Details
*  ===============
*
*  Based on contributions by
*     Ming Gu and Ren-Cang Li, Computer Science Division, University of
*       California at Berkeley, USA
*     Osni Marques, LBNL/NERSC, USA
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE, TWO
      PARAMETER          ( ZERO = 0.0D0, ONE = 1.0D0, TWO = 2.0D0 )
*     ..
*     .. Local Scalars ..
      LOGICAL            LQUERY
      INTEGER            IASCL, IBSCL, IE, IL, ITAU, ITAUP, ITAUQ,
     $                   LDWORK, MAXMN, MAXWRK, MINMN, MINWRK, MM,
     $                   MNTHR, NLVL, NWORK, SMLSIZ, WLALSD
      DOUBLE PRECISION   ANRM, BIGNUM, BNRM, EPS, SFMIN, SMLNUM
*     ..
*     .. External Subroutines ..
      EXTERNAL           DGEBRD, DGELQF, DGEQRF, DLABAD, DLACPY, DLALSD,
     $                   DLASCL, DLASET, DORMBR, DORMLQ, DORMQR, XERBLA
*     ..
*     .. External Functions ..
      INTEGER            ILAENV
      DOUBLE PRECISION   DLAMCH, DLANGE
      EXTERNAL           ILAENV, DLAMCH, DLANGE
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          DBLE, INT, LOG, MAX, MIN
*     ..
*     .. Executable Statements ..
*
*     Test the input arguments.
*
      INFO = 0
      MINMN = MIN( M, N )
      MAXMN = MAX( M, N )
      MNTHR = ILAENV( 6, 'DGELSD', ' ', M, N, NRHS, -1 )
      LQUERY = ( LWORK.EQ.-1 )
      IF( M.LT.0 ) THEN
         INFO = -1
      ELSE IF( N.LT.0 ) THEN
         INFO = -2
      ELSE IF( NRHS.LT.0 ) THEN
         INFO = -3
      ELSE IF( LDA.LT.MAX( 1, M ) ) THEN
         INFO = -5
      ELSE IF( LDB.LT.MAX( 1, MAXMN ) ) THEN
         INFO = -7
      END IF
*
      SMLSIZ = ILAENV( 9, 'DGELSD', ' ', 0, 0, 0, 0 )
*
*     Compute workspace.
*     (Note: Comments in the code beginning "Workspace:" describe the
*     minimal amount of workspace needed at that point in the code,
*     as well as the preferred amount for good performance.
*     NB refers to the optimal block size for the immediately
*     following subroutine, as returned by ILAENV.)
*
      MINWRK = 1
      MINMN = MAX( 1, MINMN )
      NLVL = MAX( INT( LOG( DBLE( MINMN ) / DBLE( SMLSIZ+1 ) ) /
     $       LOG( TWO ) )+ 1, 0 )
*
      IF( INFO.EQ.0 ) THEN
         MAXWRK = 0
         MM = M
         IF( M.GE.N .AND. M.GE.MNTHR ) THEN
*
*           Path 1a - overdetermined, with many more rows than columns.
*
            MM = N
            MAXWRK = MAX( MAXWRK, N+N*ILAENV( 1, 'DGEQRF', ' ', M, N,
     $               -1, -1 ) )
            MAXWRK = MAX( MAXWRK, N+NRHS*
     $               ILAENV( 1, 'DORMQR', 'LT', M, NRHS, N, -1 ) )
         END IF
         IF( M.GE.N ) THEN
*
*           Path 1 - overdetermined or exactly determined.
*
            MAXWRK = MAX( MAXWRK, 3*N+( MM+N )*
     $               ILAENV( 1, 'DGEBRD', ' ', MM, N, -1, -1 ) )
            MAXWRK = MAX( MAXWRK, 3*N+NRHS*
     $               ILAENV( 1, 'DORMBR', 'QLT', MM, NRHS, N, -1 ) )
            MAXWRK = MAX( MAXWRK, 3*N+( N-1 )*
     $               ILAENV( 1, 'DORMBR', 'PLN', N, NRHS, N, -1 ) )
            WLALSD = 9*N+2*N*SMLSIZ+8*N*NLVL+N*NRHS+(SMLSIZ+1)**2
            MAXWRK = MAX( MAXWRK, 3*N+WLALSD )
            MINWRK = MAX( 3*N+MM, 3*N+NRHS, 3*N+WLALSD )
         END IF
         IF( N.GT.M ) THEN
            WLALSD = 9*M+2*M*SMLSIZ+8*M*NLVL+M*NRHS+(SMLSIZ+1)**2
            IF( N.GE.MNTHR ) THEN
*
*              Path 2a - underdetermined, with many more columns
*              than rows.
*
               MAXWRK = M + M*ILAENV( 1, 'DGELQF', ' ', M, N, -1, -1 )
               MAXWRK = MAX( MAXWRK, M*M+4*M+2*M*
     $                  ILAENV( 1, 'DGEBRD', ' ', M, M, -1, -1 ) )
               MAXWRK = MAX( MAXWRK, M*M+4*M+NRHS*
     $                  ILAENV( 1, 'DORMBR', 'QLT', M, NRHS, M, -1 ) )
               MAXWRK = MAX( MAXWRK, M*M+4*M+( M-1 )*
     $                  ILAENV( 1, 'DORMBR', 'PLN', M, NRHS, M, -1 ) )
               IF( NRHS.GT.1 ) THEN
                  MAXWRK = MAX( MAXWRK, M*M+M+M*NRHS )
               ELSE
                  MAXWRK = MAX( MAXWRK, M*M+2*M )
               END IF
               MAXWRK = MAX( MAXWRK, M+NRHS*
     $                  ILAENV( 1, 'DORMLQ', 'LT', N, NRHS, M, -1 ) )
               MAXWRK = MAX( MAXWRK, M*M+4*M+WLALSD )
            ELSE
*
*              Path 2 - remaining underdetermined cases.
*
               MAXWRK = 3*M + ( N+M )*ILAENV( 1, 'DGEBRD', ' ', M, N,
     $                  -1, -1 )
               MAXWRK = MAX( MAXWRK, 3*M+NRHS*
     $                  ILAENV( 1, 'DORMBR', 'QLT', M, NRHS, N, -1 ) )
               MAXWRK = MAX( MAXWRK, 3*M+M*
     $                  ILAENV( 1, 'DORMBR', 'PLN', N, NRHS, M, -1 ) )
               MAXWRK = MAX( MAXWRK, 3*M+WLALSD )
            END IF
            MINWRK = MAX( 3*M+NRHS, 3*M+M, 3*M+WLALSD )
         END IF
         MINWRK = MIN( MINWRK, MAXWRK )
         WORK( 1 ) = MAXWRK
         IF( LWORK.LT.MINWRK .AND. .NOT.LQUERY ) THEN
            INFO = -12
         END IF
      END IF
*
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DGELSD', -INFO )
         RETURN
      ELSE IF( LQUERY ) THEN
         GO TO 10
      END IF
*
*     Quick return if possible.
*
      IF( M.EQ.0 .OR. N.EQ.0 ) THEN
         RANK = 0
         RETURN
      END IF
*
*     Get machine parameters.
*
      EPS = DLAMCH( 'P' )
      SFMIN = DLAMCH( 'S' )
      SMLNUM = SFMIN / EPS
      BIGNUM = ONE / SMLNUM
      CALL DLABAD( SMLNUM, BIGNUM )
*
*     Scale A if max entry outside range [SMLNUM,BIGNUM].
*
      ANRM = DLANGE( 'M', M, N, A, LDA, WORK )
      IASCL = 0
      IF( ANRM.GT.ZERO .AND. ANRM.LT.SMLNUM ) THEN
*
*        Scale matrix norm up to SMLNUM.
*
         CALL DLASCL( 'G', 0, 0, ANRM, SMLNUM, M, N, A, LDA, INFO )
         IASCL = 1
      ELSE IF( ANRM.GT.BIGNUM ) THEN
*
*        Scale matrix norm down to BIGNUM.
*
         CALL DLASCL( 'G', 0, 0, ANRM, BIGNUM, M, N, A, LDA, INFO )
         IASCL = 2
      ELSE IF( ANRM.EQ.ZERO ) THEN
*
*        Matrix all zero. Return zero solution.
*
         CALL DLASET( 'F', MAX( M, N ), NRHS, ZERO, ZERO, B, LDB )
         CALL DLASET( 'F', MINMN, 1, ZERO, ZERO, S, 1 )
         RANK = 0
         GO TO 10
      END IF
*
*     Scale B if max entry outside range [SMLNUM,BIGNUM].
*
      BNRM = DLANGE( 'M', M, NRHS, B, LDB, WORK )
      IBSCL = 0
      IF( BNRM.GT.ZERO .AND. BNRM.LT.SMLNUM ) THEN
*
*        Scale matrix norm up to SMLNUM.
*
         CALL DLASCL( 'G', 0, 0, BNRM, SMLNUM, M, NRHS, B, LDB, INFO )
         IBSCL = 1
      ELSE IF( BNRM.GT.BIGNUM ) THEN
*
*        Scale matrix norm down to BIGNUM.
*
         CALL DLASCL( 'G', 0, 0, BNRM, BIGNUM, M, NRHS, B, LDB, INFO )
         IBSCL = 2
      END IF
*
*     If M < N make sure certain entries of B are zero.
*
      IF( M.LT.N )
     $   CALL DLASET( 'F', N-M, NRHS, ZERO, ZERO, B( M+1, 1 ), LDB )
*
*     Overdetermined case.
*
      IF( M.GE.N ) THEN
*
*        Path 1 - overdetermined or exactly determined.
*
         MM = M
         IF( M.GE.MNTHR ) THEN
*
*           Path 1a - overdetermined, with many more rows than columns.
*
            MM = N
            ITAU = 1
            NWORK = ITAU + N
*
*           Compute A=Q*R.
*           (Workspace: need 2*N, prefer N+N*NB)
*
            CALL DGEQRF( M, N, A, LDA, WORK( ITAU ), WORK( NWORK ),
     $                   LWORK-NWORK+1, INFO )
*
*           Multiply B by transpose(Q).
*           (Workspace: need N+NRHS, prefer N+NRHS*NB)
*
            CALL DORMQR( 'L', 'T', M, NRHS, N, A, LDA, WORK( ITAU ), B,
     $                   LDB, WORK( NWORK ), LWORK-NWORK+1, INFO )
*
*           Zero out below R.
*
            IF( N.GT.1 ) THEN
               CALL DLASET( 'L', N-1, N-1, ZERO, ZERO, A( 2, 1 ), LDA )
            END IF
         END IF
*
         IE = 1
         ITAUQ = IE + N
         ITAUP = ITAUQ + N
         NWORK = ITAUP + N
*
*        Bidiagonalize R in A.
*        (Workspace: need 3*N+MM, prefer 3*N+(MM+N)*NB)
*
         CALL DGEBRD( MM, N, A, LDA, S, WORK( IE ), WORK( ITAUQ ),
     $                WORK( ITAUP ), WORK( NWORK ), LWORK-NWORK+1,
     $                INFO )
*
*        Multiply B by transpose of left bidiagonalizing vectors of R.
*        (Workspace: need 3*N+NRHS, prefer 3*N+NRHS*NB)
*
         CALL DORMBR( 'Q', 'L', 'T', MM, NRHS, N, A, LDA, WORK( ITAUQ ),
     $                B, LDB, WORK( NWORK ), LWORK-NWORK+1, INFO )
*
*        Solve the bidiagonal least squares problem.
*
         CALL DLALSD( 'U', SMLSIZ, N, NRHS, S, WORK( IE ), B, LDB,
     $                RCOND, RANK, WORK( NWORK ), IWORK, INFO )
         IF( INFO.NE.0 ) THEN
            GO TO 10
         END IF
*
*        Multiply B by right bidiagonalizing vectors of R.
*
         CALL DORMBR( 'P', 'L', 'N', N, NRHS, N, A, LDA, WORK( ITAUP ),
     $                B, LDB, WORK( NWORK ), LWORK-NWORK+1, INFO )
*
      ELSE IF( N.GE.MNTHR .AND. LWORK.GE.4*M+M*M+
     $         MAX( M, 2*M-4, NRHS, N-3*M ) ) THEN
*
*        Path 2a - underdetermined, with many more columns than rows
*        and sufficient workspace for an efficient algorithm.
*
         LDWORK = M
         IF( LWORK.GE.MAX( 4*M+M*LDA+MAX( M, 2*M-4, NRHS, N-3*M ),
     $       M*LDA+M+M*NRHS ) )LDWORK = LDA
         ITAU = 1
         NWORK = M + 1
*
*        Compute A=L*Q.
*        (Workspace: need 2*M, prefer M+M*NB)
*
         CALL DGELQF( M, N, A, LDA, WORK( ITAU ), WORK( NWORK ),
     $                LWORK-NWORK+1, INFO )
         IL = NWORK
*
*        Copy L to WORK(IL), zeroing out above its diagonal.
*
         CALL DLACPY( 'L', M, M, A, LDA, WORK( IL ), LDWORK )
         CALL DLASET( 'U', M-1, M-1, ZERO, ZERO, WORK( IL+LDWORK ),
     $                LDWORK )
         IE = IL + LDWORK*M
         ITAUQ = IE + M
         ITAUP = ITAUQ + M
         NWORK = ITAUP + M
*
*        Bidiagonalize L in WORK(IL).
*        (Workspace: need M*M+5*M, prefer M*M+4*M+2*M*NB)
*
         CALL DGEBRD( M, M, WORK( IL ), LDWORK, S, WORK( IE ),
     $                WORK( ITAUQ ), WORK( ITAUP ), WORK( NWORK ),
     $                LWORK-NWORK+1, INFO )
*
*        Multiply B by transpose of left bidiagonalizing vectors of L.
*        (Workspace: need M*M+4*M+NRHS, prefer M*M+4*M+NRHS*NB)
*
         CALL DORMBR( 'Q', 'L', 'T', M, NRHS, M, WORK( IL ), LDWORK,
     $                WORK( ITAUQ ), B, LDB, WORK( NWORK ),
     $                LWORK-NWORK+1, INFO )
*
*        Solve the bidiagonal least squares problem.
*
         CALL DLALSD( 'U', SMLSIZ, M, NRHS, S, WORK( IE ), B, LDB,
     $                RCOND, RANK, WORK( NWORK ), IWORK, INFO )
         IF( INFO.NE.0 ) THEN
            GO TO 10
         END IF
*
*        Multiply B by right bidiagonalizing vectors of L.
*
         CALL DORMBR( 'P', 'L', 'N', M, NRHS, M, WORK( IL ), LDWORK,
     $                WORK( ITAUP ), B, LDB, WORK( NWORK ),
     $                LWORK-NWORK+1, INFO )
*
*        Zero out below first M rows of B.
*
         CALL DLASET( 'F', N-M, NRHS, ZERO, ZERO, B( M+1, 1 ), LDB )
         NWORK = ITAU + M
*
*        Multiply transpose(Q) by B.
*        (Workspace: need M+NRHS, prefer M+NRHS*NB)
*
         CALL DORMLQ( 'L', 'T', N, NRHS, M, A, LDA, WORK( ITAU ), B,
     $                LDB, WORK( NWORK ), LWORK-NWORK+1, INFO )
*
      ELSE
*
*        Path 2 - remaining underdetermined cases.
*
         IE = 1
         ITAUQ = IE + M
         ITAUP = ITAUQ + M
         NWORK = ITAUP + M
*
*        Bidiagonalize A.
*        (Workspace: need 3*M+N, prefer 3*M+(M+N)*NB)
*
         CALL DGEBRD( M, N, A, LDA, S, WORK( IE ), WORK( ITAUQ ),
     $                WORK( ITAUP ), WORK( NWORK ), LWORK-NWORK+1,
     $                INFO )
*
*        Multiply B by transpose of left bidiagonalizing vectors.
*        (Workspace: need 3*M+NRHS, prefer 3*M+NRHS*NB)
*
         CALL DORMBR( 'Q', 'L', 'T', M, NRHS, N, A, LDA, WORK( ITAUQ ),
     $                B, LDB, WORK( NWORK ), LWORK-NWORK+1, INFO )
*
*        Solve the bidiagonal least squares problem.
*
         CALL DLALSD( 'L', SMLSIZ, M, NRHS, S, WORK( IE ), B, LDB,
     $                RCOND, RANK, WORK( NWORK ), IWORK, INFO )
         IF( INFO.NE.0 ) THEN
            GO TO 10
         END IF
*
*        Multiply B by right bidiagonalizing vectors of A.
*
         CALL DORMBR( 'P', 'L', 'N', N, NRHS, M, A, LDA, WORK( ITAUP ),
     $                B, LDB, WORK( NWORK ), LWORK-NWORK+1, INFO )
*
      END IF
*
*     Undo scaling.
*
      IF( IASCL.EQ.1 ) THEN
         CALL DLASCL( 'G', 0, 0, ANRM, SMLNUM, N, NRHS, B, LDB, INFO )
         CALL DLASCL( 'G', 0, 0, SMLNUM, ANRM, MINMN, 1, S, MINMN,
     $                INFO )
      ELSE IF( IASCL.EQ.2 ) THEN
         CALL DLASCL( 'G', 0, 0, ANRM, BIGNUM, N, NRHS, B, LDB, INFO )
         CALL DLASCL( 'G', 0, 0, BIGNUM, ANRM, MINMN, 1, S, MINMN,
     $                INFO )
      END IF
      IF( IBSCL.EQ.1 ) THEN
         CALL DLASCL( 'G', 0, 0, SMLNUM, BNRM, N, NRHS, B, LDB, INFO )
      ELSE IF( IBSCL.EQ.2 ) THEN
         CALL DLASCL( 'G', 0, 0, BIGNUM, BNRM, N, NRHS, B, LDB, INFO )
      END IF
*
   10 CONTINUE
      WORK( 1 ) = MAXWRK
      RETURN
*
*     End of DGELSD
*
      END
      SUBROUTINE DGELSS( M, N, NRHS, A, LDA, B, LDB, S, RCOND, RANK,
     $                   WORK, LWORK, INFO )
*
*  -- LAPACK driver routine (instrumented to count ops, version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     October 31, 1999
*
*     .. Scalar Arguments ..
      INTEGER            INFO, LDA, LDB, LWORK, M, N, NRHS, RANK
      DOUBLE PRECISION   RCOND
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), B( LDB, * ), S( * ), WORK( * )
*     ..
*     Common blocks to return operation counts and timings
*     .. Common blocks ..
      COMMON             / LATIME / OPS, ITCNT
      COMMON             / LSTIME / OPCNT, TIMNG
*     ..
*     .. Scalars in Common ..
      DOUBLE PRECISION   ITCNT, OPS
*     ..
*     .. Arrays in Common ..
      DOUBLE PRECISION   OPCNT( 6 ), TIMNG( 6 )
*     ..
*
*  Purpose
*  =======
*
*  DGELSS computes the minimum norm solution to a real linear least
*  squares problem:
*
*  Minimize 2-norm(| b - A*x |).
*
*  using the singular value decomposition (SVD) of A. A is an M-by-N
*  matrix which may be rank-deficient.
*
*  Several right hand side vectors b and solution vectors x can be
*  handled in a single call; they are stored as the columns of the
*  M-by-NRHS right hand side matrix B and the N-by-NRHS solution matrix
*  X.
*
*  The effective rank of A is determined by treating as zero those
*  singular values which are less than RCOND times the largest singular
*  value.
*
*  Arguments
*  =========
*
*  M       (input) INTEGER
*          The number of rows of the matrix A. M >= 0.
*
*  N       (input) INTEGER
*          The number of columns of the matrix A. N >= 0.
*
*  NRHS    (input) INTEGER
*          The number of right hand sides, i.e., the number of columns
*          of the matrices B and X. NRHS >= 0.
*
*  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
*          On entry, the M-by-N matrix A.
*          On exit, the first min(m,n) rows of A are overwritten with
*          its right singular vectors, stored rowwise.
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A.  LDA >= max(1,M).
*
*  B       (input/output) DOUBLE PRECISION array, dimension (LDB,NRHS)
*          On entry, the M-by-NRHS right hand side matrix B.
*          On exit, B is overwritten by the N-by-NRHS solution
*          matrix X.  If m >= n and RANK = n, the residual
*          sum-of-squares for the solution in the i-th column is given
*          by the sum of squares of elements n+1:m in that column.
*
*  LDB     (input) INTEGER
*          The leading dimension of the array B. LDB >= max(1,max(M,N)).
*
*  S       (output) DOUBLE PRECISION array, dimension (min(M,N))
*          The singular values of A in decreasing order.
*          The condition number of A in the 2-norm = S(1)/S(min(m,n)).
*
*  RCOND   (input) DOUBLE PRECISION
*          RCOND is used to determine the effective rank of A.
*          Singular values S(i) <= RCOND*S(1) are treated as zero.
*          If RCOND < 0, machine precision is used instead.
*
*  RANK    (output) INTEGER
*          The effective rank of A, i.e., the number of singular values
*          which are greater than RCOND*S(1).
*
*  WORK    (workspace/output) DOUBLE PRECISION array, dimension (LWORK)
*          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
*
*  LWORK   (input) INTEGER
*          The dimension of the array WORK. LWORK >= 1, and also:
*          LWORK >= 3*min(M,N) + max( 2*min(M,N), max(M,N), NRHS )
*          For good performance, LWORK should generally be larger.
*
*          If LWORK = -1, then a workspace query is assumed; the routine
*          only calculates the optimal size of the WORK array, returns
*          this value as the first entry of the WORK array, and no error
*          message related to LWORK is issued by XERBLA.
*
*  INFO    (output) INTEGER
*          = 0:  successful exit
*          < 0:  if INFO = -i, the i-th argument had an illegal value.
*          > 0:  the algorithm for computing the SVD failed to converge;
*                if INFO = i, i off-diagonal elements of an intermediate
*                bidiagonal form did not converge to zero.
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE
      PARAMETER          ( ZERO = 0.0D0, ONE = 1.0D0 )
*     ..
*     .. Local Scalars ..
      LOGICAL            LQUERY
      INTEGER            BDSPAC, BDSQR, BL, CHUNK, GEBRD, GELQF, GELSS,
     $                   GEMM, GEMV, GEQRF, I, IASCL, IBSCL, IE, IL,
     $                   ITAU, ITAUP, ITAUQ, IWORK, LDWORK, MAXMN,
     $                   MAXWRK, MINMN, MINWRK, MM, MNTHR, NB, ORGBR,
     $                   ORMBR, ORMLQ, ORMQR
      DOUBLE PRECISION   ANRM, BIGNUM, BNRM, EPS, SFMIN, SMLNUM, T1, T2,
     $                   THR
*     ..
*     .. Local Arrays ..
      DOUBLE PRECISION   VDUM( 1 )
*     ..
*     .. External Subroutines ..
      EXTERNAL           DBDSQR, DCOPY, DGEBRD, DGELQF, DGEMM, DGEMV,
     $                   DGEQRF, DLABAD, DLACPY, DLASCL, DLASET, DORGBR,
     $                   DORMBR, DORMLQ, DORMQR, DRSCL, XERBLA
*     ..
*     .. External Functions ..
      INTEGER            ILAENV
      DOUBLE PRECISION   DLAMCH, DLANGE, DOPBL2, DOPBL3, DOPLA, DSECND,
     $                   DOPLA2
      EXTERNAL           ILAENV, DLAMCH, DLANGE, DOPBL2, DOPBL3, DOPLA,
     $                   DSECND, DOPLA2
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          DBLE, MAX, MIN
*     ..
*     .. Data statements ..
      DATA               BDSQR / 5 / , GEBRD / 3 / , GELQF / 2 / ,
     $                   GELSS / 1 / , GEMM / 6 / , GEMV / 6 / ,
     $                   GEQRF / 2 / , ORGBR / 4 / , ORMBR / 4 / ,
     $                   ORMLQ / 6 / , ORMQR / 2 /
*     ..
*     .. Executable Statements ..
*
*     Test the input arguments
*
      INFO = 0
      MINMN = MIN( M, N )
      MAXMN = MAX( M, N )
      MNTHR = ILAENV( 6, 'DGELSS', ' ', M, N, NRHS, -1 )
      LQUERY = ( LWORK.EQ.-1 )
      IF( M.LT.0 ) THEN
         INFO = -1
      ELSE IF( N.LT.0 ) THEN
         INFO = -2
      ELSE IF( NRHS.LT.0 ) THEN
         INFO = -3
      ELSE IF( LDA.LT.MAX( 1, M ) ) THEN
         INFO = -5
      ELSE IF( LDB.LT.MAX( 1, MAXMN ) ) THEN
         INFO = -7
      END IF
*
*     Compute workspace
*      (Note: Comments in the code beginning "Workspace:" describe the
*       minimal amount of workspace needed at that point in the code,
*       as well as the preferred amount for good performance.
*       NB refers to the optimal block size for the immediately
*       following subroutine, as returned by ILAENV.)
*
      MINWRK = 1
      IF( INFO.EQ.0 .AND. ( LWORK.GE.1 .OR. LQUERY ) ) THEN
         MAXWRK = 0
         MM = M
         IF( M.GE.N .AND. M.GE.MNTHR ) THEN
*
*           Path 1a - overdetermined, with many more rows than columns
*
            MM = N
            MAXWRK = MAX( MAXWRK, N+N*ILAENV( 1, 'DGEQRF', ' ', M, N,
     $               -1, -1 ) )
            MAXWRK = MAX( MAXWRK, N+NRHS*
     $               ILAENV( 1, 'DORMQR', 'LT', M, NRHS, N, -1 ) )
         END IF
         IF( M.GE.N ) THEN
*
*           Path 1 - overdetermined or exactly determined
*
*           Compute workspace needed for DBDSQR
*
            BDSPAC = MAX( 1, 5*N )
            MAXWRK = MAX( MAXWRK, 3*N+( MM+N )*
     $               ILAENV( 1, 'DGEBRD', ' ', MM, N, -1, -1 ) )
            MAXWRK = MAX( MAXWRK, 3*N+NRHS*
     $               ILAENV( 1, 'DORMBR', 'QLT', MM, NRHS, N, -1 ) )
            MAXWRK = MAX( MAXWRK, 3*N+( N-1 )*
     $               ILAENV( 1, 'DORGBR', 'P', N, N, N, -1 ) )
            MAXWRK = MAX( MAXWRK, BDSPAC )
            MAXWRK = MAX( MAXWRK, N*NRHS )
            MINWRK = MAX( 3*N+MM, 3*N+NRHS, BDSPAC )
            MAXWRK = MAX( MINWRK, MAXWRK )
         END IF
         IF( N.GT.M ) THEN
*
*           Compute workspace needed for DBDSQR
*
            BDSPAC = MAX( 1, 5*M )
            MINWRK = MAX( 3*M+NRHS, 3*M+N, BDSPAC )
            IF( N.GE.MNTHR ) THEN
*
*              Path 2a - underdetermined, with many more columns
*              than rows
*
               MAXWRK = M + M*ILAENV( 1, 'DGELQF', ' ', M, N, -1, -1 )
               MAXWRK = MAX( MAXWRK, M*M+4*M+2*M*
     $                  ILAENV( 1, 'DGEBRD', ' ', M, M, -1, -1 ) )
               MAXWRK = MAX( MAXWRK, M*M+4*M+NRHS*
     $                  ILAENV( 1, 'DORMBR', 'QLT', M, NRHS, M, -1 ) )
               MAXWRK = MAX( MAXWRK, M*M+4*M+( M-1 )*
     $                  ILAENV( 1, 'DORGBR', 'P', M, M, M, -1 ) )
               MAXWRK = MAX( MAXWRK, M*M+M+BDSPAC )
               IF( NRHS.GT.1 ) THEN
                  MAXWRK = MAX( MAXWRK, M*M+M+M*NRHS )
               ELSE
                  MAXWRK = MAX( MAXWRK, M*M+2*M )
               END IF
               MAXWRK = MAX( MAXWRK, M+NRHS*
     $                  ILAENV( 1, 'DORMLQ', 'LT', N, NRHS, M, -1 ) )
            ELSE
*
*              Path 2 - underdetermined
*
               MAXWRK = 3*M + ( N+M )*ILAENV( 1, 'DGEBRD', ' ', M, N,
     $                  -1, -1 )
               MAXWRK = MAX( MAXWRK, 3*M+NRHS*
     $                  ILAENV( 1, 'DORMBR', 'QLT', M, NRHS, M, -1 ) )
               MAXWRK = MAX( MAXWRK, 3*M+M*
     $                  ILAENV( 1, 'DORGBR', 'P', M, N, M, -1 ) )
               MAXWRK = MAX( MAXWRK, BDSPAC )
               MAXWRK = MAX( MAXWRK, N*NRHS )
            END IF
         END IF
         MAXWRK = MAX( MINWRK, MAXWRK )
         WORK( 1 ) = MAXWRK
      END IF
*
      MINWRK = MAX( MINWRK, 1 )
      IF( LWORK.LT.MINWRK .AND. .NOT.LQUERY )
     $   INFO = -12
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DGELSS', -INFO )
         RETURN
      ELSE IF( LQUERY ) THEN
         RETURN
      END IF
*
*     Quick return if possible
*
      IF( M.EQ.0 .OR. N.EQ.0 ) THEN
         RANK = 0
         RETURN
      END IF
*
*     Get machine parameters
*
      EPS = DLAMCH( 'P' )
      SFMIN = DLAMCH( 'S' )
      OPCNT( GELSS ) = OPCNT( GELSS ) + DBLE( 2 )
      SMLNUM = SFMIN / EPS
      BIGNUM = ONE / SMLNUM
      CALL DLABAD( SMLNUM, BIGNUM )
*
*     Scale A if max element outside range [SMLNUM,BIGNUM]
*
      ANRM = DLANGE( 'M', M, N, A, LDA, WORK )
      IASCL = 0
      IF( ANRM.GT.ZERO .AND. ANRM.LT.SMLNUM ) THEN
*
*        Scale matrix norm up to SMLNUM
*
         OPCNT( GELSS ) = OPCNT( GELSS ) + DBLE( M*N )
         CALL DLASCL( 'G', 0, 0, ANRM, SMLNUM, M, N, A, LDA, INFO )
         IASCL = 1
      ELSE IF( ANRM.GT.BIGNUM ) THEN
*
*        Scale matrix norm down to BIGNUM
*
         OPCNT( GELSS ) = OPCNT( GELSS ) + DBLE( M*N )
         CALL DLASCL( 'G', 0, 0, ANRM, BIGNUM, M, N, A, LDA, INFO )
         IASCL = 2
      ELSE IF( ANRM.EQ.ZERO ) THEN
*
*        Matrix all zero. Return zero solution.
*
         CALL DLASET( 'F', MAX( M, N ), NRHS, ZERO, ZERO, B, LDB )
         CALL DLASET( 'F', MINMN, 1, ZERO, ZERO, S, 1 )
         RANK = 0
         GO TO 70
      END IF
*
*     Scale B if max element outside range [SMLNUM,BIGNUM]
*
      BNRM = DLANGE( 'M', M, NRHS, B, LDB, WORK )
      IBSCL = 0
      IF( BNRM.GT.ZERO .AND. BNRM.LT.SMLNUM ) THEN
*
*        Scale matrix norm up to SMLNUM
*
         OPCNT( GELSS ) = OPCNT( GELSS ) + DBLE( M*NRHS )
         CALL DLASCL( 'G', 0, 0, BNRM, SMLNUM, M, NRHS, B, LDB, INFO )
         IBSCL = 1
      ELSE IF( BNRM.GT.BIGNUM ) THEN
*
*        Scale matrix norm down to BIGNUM
*
         OPCNT( GELSS ) = OPCNT( GELSS ) + DBLE( M*NRHS )
         CALL DLASCL( 'G', 0, 0, BNRM, BIGNUM, M, NRHS, B, LDB, INFO )
         IBSCL = 2
      END IF
*
*     Overdetermined case
*
      IF( M.GE.N ) THEN
*
*        Path 1 - overdetermined or exactly determined
*
         MM = M
         IF( M.GE.MNTHR ) THEN
*
*           Path 1a - overdetermined, with many more rows than columns
*
            MM = N
            ITAU = 1
            IWORK = ITAU + N
*
*           Compute A=Q*R
*           (Workspace: need 2*N, prefer N+N*NB)
*
            NB = ILAENV( 1, 'DGEQRF', ' ', M, N, -1, -1 )
            OPCNT( GEQRF ) = OPCNT( GEQRF ) +
     $                       DOPLA( 'DGEQRF', M, N, 0, 0, NB )
            T1 = DSECND( )
            CALL DGEQRF( M, N, A, LDA, WORK( ITAU ), WORK( IWORK ),
     $                   LWORK-IWORK+1, INFO )
            T2 = DSECND( )
            TIMNG( GEQRF ) = TIMNG( GEQRF ) + ( T2-T1 )
*
*           Multiply B by transpose(Q)
*           (Workspace: need N+NRHS, prefer N+NRHS*NB)
*
            NB = ILAENV( 1, 'DORMQR', 'LT', M, NRHS, N, -1 )
            OPCNT( ORMQR ) = OPCNT( ORMQR ) +
     $                       DOPLA( 'DORMQR', M, NRHS, N, 0, NB )
            T1 = DSECND( )
            CALL DORMQR( 'L', 'T', M, NRHS, N, A, LDA, WORK( ITAU ), B,
     $                   LDB, WORK( IWORK ), LWORK-IWORK+1, INFO )
            T2 = DSECND( )
            TIMNG( ORMQR ) = TIMNG( ORMQR ) + ( T2-T1 )
*
*           Zero out below R
*
            IF( N.GT.1 )
     $         CALL DLASET( 'L', N-1, N-1, ZERO, ZERO, A( 2, 1 ), LDA )
         END IF
*
         IE = 1
         ITAUQ = IE + N
         ITAUP = ITAUQ + N
         IWORK = ITAUP + N
*
*        Bidiagonalize R in A
*        (Workspace: need 3*N+MM, prefer 3*N+(MM+N)*NB)
*
         NB = ILAENV( 1, 'DGEBRD', ' ', MM, N, -1, -1 )
         OPCNT( GEBRD ) = OPCNT( GEBRD ) +
     $                    DOPLA( 'DGEBRD', MM, N, 0, 0, NB )
         T1 = DSECND( )
         CALL DGEBRD( MM, N, A, LDA, S, WORK( IE ), WORK( ITAUQ ),
     $                WORK( ITAUP ), WORK( IWORK ), LWORK-IWORK+1,
     $                INFO )
         T2 = DSECND( )
         TIMNG( GEBRD ) = TIMNG( GEBRD ) + ( T2-T1 )
*
*        Multiply B by transpose of left bidiagonalizing vectors of R
*        (Workspace: need 3*N+NRHS, prefer 3*N+NRHS*NB)
*
         NB = ILAENV( 1, 'DORMBR', 'QLT', MM, NRHS, N, -1 )
         OPCNT( ORMBR ) = OPCNT( ORMBR ) +
     $                    DOPLA2( 'DORMBR', 'QLT', MM, NRHS, N, 0, NB )
         T1 = DSECND( )
         CALL DORMBR( 'Q', 'L', 'T', MM, NRHS, N, A, LDA, WORK( ITAUQ ),
     $                B, LDB, WORK( IWORK ), LWORK-IWORK+1, INFO )
         T2 = DSECND( )
         TIMNG( ORMBR ) = TIMNG( ORMBR ) + ( T2-T1 )
*
*        Generate right bidiagonalizing vectors of R in A
*        (Workspace: need 4*N-1, prefer 3*N+(N-1)*NB)
*
         NB = ILAENV( 1, 'DORGBR', 'P', N, N, N, -1 )
         OPCNT( ORGBR ) = OPCNT( ORGBR ) +
     $                    DOPLA2( 'DORGBR', 'P', N, N, N, 0, NB )
         T1 = DSECND( )
         CALL DORGBR( 'P', N, N, N, A, LDA, WORK( ITAUP ),
     $                WORK( IWORK ), LWORK-IWORK+1, INFO )
         T2 = DSECND( )
         TIMNG( ORGBR ) = TIMNG( ORGBR ) + ( T2-T1 )
         IWORK = IE + N
*
*        Perform bidiagonal QR iteration
*          multiply B by transpose of left singular vectors
*          compute right singular vectors in A
*        (Workspace: need BDSPAC)
*
         OPS = 0
         T1 = DSECND( )
         CALL DBDSQR( 'U', N, N, 0, NRHS, S, WORK( IE ), A, LDA, VDUM,
     $                1, B, LDB, WORK( IWORK ), INFO )
         T2 = DSECND( )
         TIMNG( BDSQR ) = TIMNG( BDSQR ) + ( T2-T1 )
         OPCNT( BDSQR ) = OPCNT( BDSQR ) + OPS
         IF( INFO.NE.0 )
     $      GO TO 70
*
*        Multiply B by reciprocals of singular values
*
         OPCNT( GELSS ) = OPCNT( GELSS ) + DBLE( 1 )
         THR = MAX( RCOND*S( 1 ), SFMIN )
         IF( RCOND.LT.ZERO ) THEN
            OPCNT( GELSS ) = OPCNT( GELSS ) + DBLE( 1 )
            THR = MAX( EPS*S( 1 ), SFMIN )
         END IF
         RANK = 0
         DO 10 I = 1, N
            IF( S( I ).GT.THR ) THEN
               OPCNT( GELSS ) = OPCNT( GELSS ) + DBLE( NRHS+3 )
               CALL DRSCL( NRHS, S( I ), B( I, 1 ), LDB )
               RANK = RANK + 1
            ELSE
               CALL DLASET( 'F', 1, NRHS, ZERO, ZERO, B( I, 1 ), LDB )
            END IF
   10    CONTINUE
*
*        Multiply B by right singular vectors
*        (Workspace: need N, prefer N*NRHS)
*
         IF( LWORK.GE.LDB*NRHS .AND. NRHS.GT.1 ) THEN
            OPCNT( GEMM ) = OPCNT( GEMM ) +
     $                      DOPBL3( 'DGEMM ', N, NRHS, N )
            T1 = DSECND( )
            CALL DGEMM( 'T', 'N', N, NRHS, N, ONE, A, LDA, B, LDB, ZERO,
     $                  WORK, LDB )
            T2 = DSECND( )
            TIMNG( GEMM ) = TIMNG( GEMM ) + ( T2-T1 )
            CALL DLACPY( 'G', N, NRHS, WORK, LDB, B, LDB )
         ELSE IF( NRHS.GT.1 ) THEN
            CHUNK = LWORK / N
            DO 20 I = 1, NRHS, CHUNK
               BL = MIN( NRHS-I+1, CHUNK )
               OPCNT( GEMM ) = OPCNT( GEMM ) +
     $                         DOPBL3( 'DGEMM ', N, BL, N )
               T1 = DSECND( )
               CALL DGEMM( 'T', 'N', N, BL, N, ONE, A, LDA, B( 1, I ),
     $                     LDB, ZERO, WORK, N )
               T2 = DSECND( )
               TIMNG( GEMM ) = TIMNG( GEMM ) + ( T2-T1 )
               CALL DLACPY( 'G', N, BL, WORK, N, B( 1, I ), LDB )
   20       CONTINUE
         ELSE
            OPCNT( GEMV ) = OPCNT( GEMV ) +
     $                      DOPBL2( 'DGEMV ', N, N, 0, 0 )
            T1 = DSECND( )
            CALL DGEMV( 'T', N, N, ONE, A, LDA, B, 1, ZERO, WORK, 1 )
            T2 = DSECND( )
            TIMNG( GEMV ) = TIMNG( GEMV ) + ( T2-T1 )
            CALL DCOPY( N, WORK, 1, B, 1 )
         END IF
*
      ELSE IF( N.GE.MNTHR .AND. LWORK.GE.4*M+M*M+
     $         MAX( M, 2*M-4, NRHS, N-3*M ) ) THEN
*
*        Path 2a - underdetermined, with many more columns than rows
*        and sufficient workspace for an efficient algorithm
*
         LDWORK = M
         IF( LWORK.GE.MAX( 4*M+M*LDA+MAX( M, 2*M-4, NRHS, N-3*M ),
     $       M*LDA+M+M*NRHS ) )LDWORK = LDA
         ITAU = 1
         IWORK = M + 1
*
*        Compute A=L*Q
*        (Workspace: need 2*M, prefer M+M*NB)
*
         NB = ILAENV( 1, 'DGELQF', ' ', M, N, -1, -1 )
         OPCNT( GELQF ) = OPCNT( GELQF ) +
     $                    DOPLA( 'DGELQF', M, N, 0, 0, NB )
         T1 = DSECND( )
         CALL DGELQF( M, N, A, LDA, WORK( ITAU ), WORK( IWORK ),
     $                LWORK-IWORK+1, INFO )
         T2 = DSECND( )
         TIMNG( GELQF ) = TIMNG( GELQF ) + ( T2-T1 )
         IL = IWORK
*
*        Copy L to WORK(IL), zeroing out above it
*
         CALL DLACPY( 'L', M, M, A, LDA, WORK( IL ), LDWORK )
         CALL DLASET( 'U', M-1, M-1, ZERO, ZERO, WORK( IL+LDWORK ),
     $                LDWORK )
         IE = IL + LDWORK*M
         ITAUQ = IE + M
         ITAUP = ITAUQ + M
         IWORK = ITAUP + M
*
*        Bidiagonalize L in WORK(IL)
*        (Workspace: need M*M+5*M, prefer M*M+4*M+2*M*NB)
*
         NB = ILAENV( 1, 'DGEBRD', ' ', M, M, -1, -1 )
         OPCNT( GEBRD ) = OPCNT( GEBRD ) +
     $                    DOPLA( 'DGEBRD', M, M, 0, 0, NB )
         T1 = DSECND( )
         CALL DGEBRD( M, M, WORK( IL ), LDWORK, S, WORK( IE ),
     $                WORK( ITAUQ ), WORK( ITAUP ), WORK( IWORK ),
     $                LWORK-IWORK+1, INFO )
         T2 = DSECND( )
         TIMNG( GEBRD ) = TIMNG( GEBRD ) + ( T2-T1 )
*
*        Multiply B by transpose of left bidiagonalizing vectors of L
*        (Workspace: need M*M+4*M+NRHS, prefer M*M+4*M+NRHS*NB)
*
         NB = ILAENV( 1, 'DORMBR', 'QLT', M, NRHS, M, -1 )
         OPCNT( ORMBR ) = OPCNT( ORMBR ) +
     $                    DOPLA2( 'DORMBR', 'QLT', M, NRHS, M, 0, NB )
         T1 = DSECND( )
         CALL DORMBR( 'Q', 'L', 'T', M, NRHS, M, WORK( IL ), LDWORK,
     $                WORK( ITAUQ ), B, LDB, WORK( IWORK ),
     $                LWORK-IWORK+1, INFO )
         T2 = DSECND( )
         TIMNG( ORMBR ) = TIMNG( ORMBR ) + ( T2-T1 )
*
*        Generate right bidiagonalizing vectors of R in WORK(IL)
*        (Workspace: need M*M+5*M-1, prefer M*M+4*M+(M-1)*NB)
*
         NB = ILAENV( 1, 'DORGBR', 'P', M, M, M, -1 )
         OPCNT( ORGBR ) = OPCNT( ORGBR ) +
     $                    DOPLA2( 'DORGBR', 'P', M, M, M, 0, NB )
         T1 = DSECND( )
         CALL DORGBR( 'P', M, M, M, WORK( IL ), LDWORK, WORK( ITAUP ),
     $                WORK( IWORK ), LWORK-IWORK+1, INFO )
         T2 = DSECND( )
         TIMNG( ORGBR ) = TIMNG( ORGBR ) + ( T2-T1 )
         IWORK = IE + M
*
*        Perform bidiagonal QR iteration,
*           computing right singular vectors of L in WORK(IL) and
*           multiplying B by transpose of left singular vectors
*        (Workspace: need M*M+M+BDSPAC)
*
         OPS = 0
         T1 = DSECND( )
         CALL DBDSQR( 'U', M, M, 0, NRHS, S, WORK( IE ), WORK( IL ),
     $                LDWORK, A, LDA, B, LDB, WORK( IWORK ), INFO )
         T2 = DSECND( )
         TIMNG( BDSQR ) = TIMNG( BDSQR ) + ( T2-T1 )
         OPCNT( BDSQR ) = OPCNT( BDSQR ) + OPS
         IF( INFO.NE.0 )
     $      GO TO 70
*
*        Multiply B by reciprocals of singular values
*
         OPCNT( GELSS ) = OPCNT( GELSS ) + DBLE( 1 )
         THR = MAX( RCOND*S( 1 ), SFMIN )
         IF( RCOND.LT.ZERO ) THEN
            OPCNT( GELSS ) = OPCNT( GELSS ) + DBLE( 1 )
            THR = MAX( EPS*S( 1 ), SFMIN )
         END IF
         RANK = 0
         DO 30 I = 1, M
            IF( S( I ).GT.THR ) THEN
               OPCNT( GELSS ) = OPCNT( GELSS ) + DBLE( NRHS+3 )
               CALL DRSCL( NRHS, S( I ), B( I, 1 ), LDB )
               RANK = RANK + 1
            ELSE
               CALL DLASET( 'F', 1, NRHS, ZERO, ZERO, B( I, 1 ), LDB )
            END IF
   30    CONTINUE
         IWORK = IE
*
*        Multiply B by right singular vectors of L in WORK(IL)
*        (Workspace: need M*M+2*M, prefer M*M+M+M*NRHS)
*
         IF( LWORK.GE.LDB*NRHS+IWORK-1 .AND. NRHS.GT.1 ) THEN
            OPCNT( GEMM ) = OPCNT( GEMM ) +
     $                      DOPBL3( 'DGEMM ', M, NRHS, M )
            T1 = DSECND( )
            CALL DGEMM( 'T', 'N', M, NRHS, M, ONE, WORK( IL ), LDWORK,
     $                  B, LDB, ZERO, WORK( IWORK ), LDB )
            T2 = DSECND( )
            TIMNG( GEMM ) = TIMNG( GEMM ) + ( T2-T1 )
            CALL DLACPY( 'G', M, NRHS, WORK( IWORK ), LDB, B, LDB )
         ELSE IF( NRHS.GT.1 ) THEN
            CHUNK = ( LWORK-IWORK+1 ) / M
            DO 40 I = 1, NRHS, CHUNK
               BL = MIN( NRHS-I+1, CHUNK )
               OPCNT( GEMM ) = OPCNT( GEMM ) +
     $                         DOPBL3( 'DGEMM ', M, BL, M )
               T1 = DSECND( )
               CALL DGEMM( 'T', 'N', M, BL, M, ONE, WORK( IL ), LDWORK,
     $                     B( 1, I ), LDB, ZERO, WORK( IWORK ), N )
               T2 = DSECND( )
               TIMNG( GEMM ) = TIMNG( GEMM ) + ( T2-T1 )
               CALL DLACPY( 'G', M, BL, WORK( IWORK ), N, B( 1, I ),
     $                      LDB )
   40       CONTINUE
         ELSE
            OPCNT( GEMV ) = OPCNT( GEMV ) +
     $                      DOPBL2( 'DGEMV ', M, M, 0, 0 )
            T1 = DSECND( )
            CALL DGEMV( 'T', M, M, ONE, WORK( IL ), LDWORK, B( 1, 1 ),
     $                  1, ZERO, WORK( IWORK ), 1 )
            T2 = DSECND( )
            TIMNG( GEMV ) = TIMNG( GEMV ) + ( T2-T1 )
            CALL DCOPY( M, WORK( IWORK ), 1, B( 1, 1 ), 1 )
         END IF
*
*        Zero out below first M rows of B
*
         CALL DLASET( 'F', N-M, NRHS, ZERO, ZERO, B( M+1, 1 ), LDB )
         IWORK = ITAU + M
*
*        Multiply transpose(Q) by B
*        (Workspace: need M+NRHS, prefer M+NRHS*NB)
*
         NB = ILAENV( 1, 'DORMLQ', 'LT', N, NRHS, M, -1 )
         OPCNT( ORMLQ ) = OPCNT( ORMLQ ) +
     $                    DOPLA( 'DORMLQ', N, NRHS, M, 0, NB )
         T1 = DSECND( )
         CALL DORMLQ( 'L', 'T', N, NRHS, M, A, LDA, WORK( ITAU ), B,
     $                LDB, WORK( IWORK ), LWORK-IWORK+1, INFO )
         T2 = DSECND( )
         TIMNG( ORMLQ ) = TIMNG( ORMLQ ) + ( T2-T1 )
*
      ELSE
*
*        Path 2 - remaining underdetermined cases
*
         IE = 1
         ITAUQ = IE + M
         ITAUP = ITAUQ + M
         IWORK = ITAUP + M
*
*        Bidiagonalize A
*        (Workspace: need 3*M+N, prefer 3*M+(M+N)*NB)
*
         NB = ILAENV( 1, 'DGEBRD', ' ', M, N, -1, -1 )
         OPCNT( GEBRD ) = OPCNT( GEBRD ) +
     $                    DOPLA( 'DGEBRD', M, N, 0, 0, NB )
         T1 = DSECND( )
         CALL DGEBRD( M, N, A, LDA, S, WORK( IE ), WORK( ITAUQ ),
     $                WORK( ITAUP ), WORK( IWORK ), LWORK-IWORK+1,
     $                INFO )
         T2 = DSECND( )
         TIMNG( GEBRD ) = TIMNG( GEBRD ) + ( T2-T1 )
*
*        Multiply B by transpose of left bidiagonalizing vectors
*        (Workspace: need 3*M+NRHS, prefer 3*M+NRHS*NB)
*
         NB = ILAENV( 1, 'DORMBR', 'QLT', M, NRHS, N, -1 )
         OPCNT( ORMBR ) = OPCNT( ORMBR ) +
     $                    DOPLA2( 'DORMBR', 'QLT', M, NRHS, N, 0, NB )
         T1 = DSECND( )
         CALL DORMBR( 'Q', 'L', 'T', M, NRHS, N, A, LDA, WORK( ITAUQ ),
     $                B, LDB, WORK( IWORK ), LWORK-IWORK+1, INFO )
         T2 = DSECND( )
         TIMNG( ORMBR ) = TIMNG( ORMBR ) + ( T2-T1 )
*
*        Generate right bidiagonalizing vectors in A
*        (Workspace: need 4*M, prefer 3*M+M*NB)
*
         NB = ILAENV( 1, 'DORGBR', 'P', M, N, M, -1 )
         OPCNT( ORGBR ) = OPCNT( ORGBR ) +
     $                    DOPLA2( 'DORGBR', 'P', M, N, M, 0, NB )
         T1 = DSECND( )
         CALL DORGBR( 'P', M, N, M, A, LDA, WORK( ITAUP ),
     $                WORK( IWORK ), LWORK-IWORK+1, INFO )
         T2 = DSECND( )
         TIMNG( ORGBR ) = TIMNG( ORGBR ) + ( T2-T1 )
         IWORK = IE + M
*
*        Perform bidiagonal QR iteration,
*           computing right singular vectors of A in A and
*           multiplying B by transpose of left singular vectors
*        (Workspace: need BDSPAC)
*
         OPS = 0
         T1 = DSECND( )
         CALL DBDSQR( 'L', M, N, 0, NRHS, S, WORK( IE ), A, LDA, VDUM,
     $                1, B, LDB, WORK( IWORK ), INFO )
         T2 = DSECND( )
         TIMNG( BDSQR ) = TIMNG( BDSQR ) + ( T2-T1 )
         OPCNT( BDSQR ) = OPCNT( BDSQR ) + OPS
         IF( INFO.NE.0 )
     $      GO TO 70
*
*        Multiply B by reciprocals of singular values
*
         OPCNT( GELSS ) = OPCNT( GELSS ) + DBLE( 1 )
         THR = MAX( RCOND*S( 1 ), SFMIN )
         IF( RCOND.LT.ZERO ) THEN
            OPCNT( GELSS ) = OPCNT( GELSS ) + DBLE( 1 )
            THR = MAX( EPS*S( 1 ), SFMIN )
         END IF
         RANK = 0
         DO 50 I = 1, M
            IF( S( I ).GT.THR ) THEN
               OPCNT( GELSS ) = OPCNT( GELSS ) + DBLE( NRHS+3 )
               CALL DRSCL( NRHS, S( I ), B( I, 1 ), LDB )
               RANK = RANK + 1
            ELSE
               CALL DLASET( 'F', 1, NRHS, ZERO, ZERO, B( I, 1 ), LDB )
            END IF
   50    CONTINUE
*
*        Multiply B by right singular vectors of A
*        (Workspace: need N, prefer N*NRHS)
*
         IF( LWORK.GE.LDB*NRHS .AND. NRHS.GT.1 ) THEN
            OPCNT( GEMM ) = OPCNT( GEMM ) +
     $                      DOPBL3( 'DGEMM ', N, NRHS, M )
            T1 = DSECND( )
            CALL DGEMM( 'T', 'N', N, NRHS, M, ONE, A, LDA, B, LDB, ZERO,
     $                  WORK, LDB )
            T2 = DSECND( )
            TIMNG( GEMM ) = TIMNG( GEMM ) + ( T2-T1 )
            CALL DLACPY( 'F', N, NRHS, WORK, LDB, B, LDB )
         ELSE IF( NRHS.GT.1 ) THEN
            CHUNK = LWORK / N
            DO 60 I = 1, NRHS, CHUNK
               BL = MIN( NRHS-I+1, CHUNK )
               OPCNT( GEMM ) = OPCNT( GEMM ) +
     $                         DOPBL3( 'DGEMM ', N, BL, M )
               T1 = DSECND( )
               CALL DGEMM( 'T', 'N', N, BL, M, ONE, A, LDA, B( 1, I ),
     $                     LDB, ZERO, WORK, N )
               T2 = DSECND( )
               TIMNG( GEMM ) = TIMNG( GEMM ) + ( T2-T1 )
               CALL DLACPY( 'F', N, BL, WORK, N, B( 1, I ), LDB )
   60       CONTINUE
         ELSE
            OPCNT( GEMV ) = OPCNT( GEMV ) +
     $                      DOPBL2( 'DGEMV ', M, N, 0, 0 )
            T1 = DSECND( )
            CALL DGEMV( 'T', M, N, ONE, A, LDA, B, 1, ZERO, WORK, 1 )
            T2 = DSECND( )
            TIMNG( GEMV ) = TIMNG( GEMV ) + ( T2-T1 )
            CALL DCOPY( N, WORK, 1, B, 1 )
         END IF
      END IF
*
*     Undo scaling
*
      IF( IASCL.EQ.1 ) THEN
         OPCNT( GELSS ) = OPCNT( GELSS ) + DBLE( N*NRHS+MINMN )
         CALL DLASCL( 'G', 0, 0, ANRM, SMLNUM, N, NRHS, B, LDB, INFO )
         CALL DLASCL( 'G', 0, 0, SMLNUM, ANRM, MINMN, 1, S, MINMN,
     $                INFO )
      ELSE IF( IASCL.EQ.2 ) THEN
         OPCNT( GELSS ) = OPCNT( GELSS ) + DBLE( N*NRHS+MINMN )
         CALL DLASCL( 'G', 0, 0, ANRM, BIGNUM, N, NRHS, B, LDB, INFO )
         CALL DLASCL( 'G', 0, 0, BIGNUM, ANRM, MINMN, 1, S, MINMN,
     $                INFO )
      END IF
      IF( IBSCL.EQ.1 ) THEN
         OPCNT( GELSS ) = OPCNT( GELSS ) + DBLE( N*NRHS )
         CALL DLASCL( 'G', 0, 0, SMLNUM, BNRM, N, NRHS, B, LDB, INFO )
      ELSE IF( IBSCL.EQ.2 ) THEN
         OPCNT( GELSS ) = OPCNT( GELSS ) + DBLE( N*NRHS )
         CALL DLASCL( 'G', 0, 0, BIGNUM, BNRM, N, NRHS, B, LDB, INFO )
      END IF
*
   70 CONTINUE
      WORK( 1 ) = MAXWRK
      RETURN
*
*     End of DGELSS
*
      END
      SUBROUTINE DGELSX( M, N, NRHS, A, LDA, B, LDB, JPVT, RCOND, RANK,
     $                   WORK, INFO )
*
*  -- LAPACK driver routine (instrumented to count ops, version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     September 30, 1994
*
*     .. Scalar Arguments ..
      INTEGER            INFO, LDA, LDB, M, N, NRHS, RANK
      DOUBLE PRECISION   RCOND
*     ..
*     .. Array Arguments ..
      INTEGER            JPVT( * )
      DOUBLE PRECISION   A( LDA, * ), B( LDB, * ), WORK( * )
*     ..
*     Common blocks to return operation counts and timings
*     .. Common blocks ..
      COMMON             / LATIME / OPS, ITCNT
      COMMON             / LSTIME / OPCNT, TIMNG
*     ..
*     .. Scalars in Common ..
      DOUBLE PRECISION   ITCNT, OPS
*     ..
*     .. Arrays in Common ..
      DOUBLE PRECISION   OPCNT( 6 ), TIMNG( 6 )
*     ..
*
*  Purpose
*  =======
*
*  DGELSX computes the minimum-norm solution to a real linear least
*  squares problem:
*      minimize || A * X - B ||
*  using a complete orthogonal factorization of A.  A is an M-by-N
*  matrix which may be rank-deficient.
*
*  Several right hand side vectors b and solution vectors x can be
*  handled in a single call; they are stored as the columns of the
*  M-by-NRHS right hand side matrix B and the N-by-NRHS solution
*  matrix X.
*
*  The routine first computes a QR factorization with column pivoting:
*      A * P = Q * [ R11 R12 ]
*                  [  0  R22 ]
*  with R11 defined as the largest leading submatrix whose estimated
*  condition number is less than 1/RCOND.  The order of R11, RANK,
*  is the effective rank of A.
*
*  Then, R22 is considered to be negligible, and R12 is annihilated
*  by orthogonal transformations from the right, arriving at the
*  complete orthogonal factorization:
*     A * P = Q * [ T11 0 ] * Z
*                 [  0  0 ]
*  The minimum-norm solution is then
*     X = P * Z' [ inv(T11)*Q1'*B ]
*                [        0       ]
*  where Q1 consists of the first RANK columns of Q.
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
*  NRHS    (input) INTEGER
*          The number of right hand sides, i.e., the number of
*          columns of matrices B and X. NRHS >= 0.
*
*  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
*          On entry, the M-by-N matrix A.
*          On exit, A has been overwritten by details of its
*          complete orthogonal factorization.
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A.  LDA >= max(1,M).
*
*  B       (input/output) DOUBLE PRECISION array, dimension (LDB,NRHS)
*          On entry, the M-by-NRHS right hand side matrix B.
*          On exit, the N-by-NRHS solution matrix X.
*          If m >= n and RANK = n, the residual sum-of-squares for
*          the solution in the i-th column is given by the sum of
*          squares of elements N+1:M in that column.
*
*  LDB     (input) INTEGER
*          The leading dimension of the array B. LDB >= max(1,M,N).
*
*  JPVT    (input/output) INTEGER array, dimension (N)
*          On entry, if JPVT(i) .ne. 0, the i-th column of A is an
*          initial column, otherwise it is a free column.  Before
*          the QR factorization of A, all initial columns are
*          permuted to the leading positions; only the remaining
*          free columns are moved as a result of column pivoting
*          during the factorization.
*          On exit, if JPVT(i) = k, then the i-th column of A*P
*          was the k-th column of A.
*
*  RCOND   (input) DOUBLE PRECISION
*          RCOND is used to determine the effective rank of A, which
*          is defined as the order of the largest leading triangular
*          submatrix R11 in the QR factorization with pivoting of A,
*          whose estimated condition number < 1/RCOND.
*
*  RANK    (output) INTEGER
*          The effective rank of A, i.e., the order of the submatrix
*          R11.  This is the same as the order of the submatrix T11
*          in the complete orthogonal factorization of A.
*
*  WORK    (workspace) DOUBLE PRECISION array, dimension
*                      (max( min(M,N)+3*N, 2*min(M,N)+NRHS )),
*
*  INFO    (output) INTEGER
*          = 0:  successful exit
*          < 0:  if INFO = -i, the i-th argument had an illegal value
*
*  =====================================================================
*
*     .. Parameters ..
      INTEGER            IMAX, IMIN
      PARAMETER          ( IMAX = 1, IMIN = 2 )
      DOUBLE PRECISION   ZERO, ONE, DONE, NTDONE
      PARAMETER          ( ZERO = 0.0D0, ONE = 1.0D0, DONE = ZERO,
     $                   NTDONE = ONE )
*     ..
*     .. Local Scalars ..
      INTEGER            GELSX, GEQPF, I, IASCL, IBSCL, ISMAX, ISMIN, J,
     $                   K, LATZM, MN, ORM2R, TRSM, TZRQF
      DOUBLE PRECISION   ANRM, BIGNUM, BNRM, C1, C2, S1, S2, SMAX,
     $                   SMAXPR, SMIN, SMINPR, SMLNUM, T1, T2, TIM1,
     $                   TIM2
*     ..
*     .. External Functions ..
      DOUBLE PRECISION   DLAMCH, DLANGE, DOPBL3, DOPLA, DSECND
      EXTERNAL           DLAMCH, DLANGE, DOPBL3, DOPLA, DSECND
*     ..
*     .. External Subroutines ..
      EXTERNAL           DGEQPF, DLABAD, DLAIC1, DLASCL, DLASET, DLATZM,
     $                   DORM2R, DTRSM, DTZRQF, XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, DBLE, MAX, MIN
*     ..
*     .. Data statements ..
      DATA               GELSX / 1 / , GEQPF / 2 / , LATZM / 6 / ,
     $                   ORM2R / 4 / , TRSM / 5 / , TZRQF / 3 /
*     ..
*     .. Executable Statements ..
*
      MN = MIN( M, N )
      ISMIN = MN + 1
      ISMAX = 2*MN + 1
*
*     Test the input arguments.
*
      INFO = 0
      IF( M.LT.0 ) THEN
         INFO = -1
      ELSE IF( N.LT.0 ) THEN
         INFO = -2
      ELSE IF( NRHS.LT.0 ) THEN
         INFO = -3
      ELSE IF( LDA.LT.MAX( 1, M ) ) THEN
         INFO = -5
      ELSE IF( LDB.LT.MAX( 1, M, N ) ) THEN
         INFO = -7
      END IF
*
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DGELSX', -INFO )
         RETURN
      END IF
*
*     Quick return if possible
*
      IF( MIN( M, N, NRHS ).EQ.0 ) THEN
         RANK = 0
         RETURN
      END IF
*
*     Get machine parameters
*
      OPCNT( GELSX ) = OPCNT( GELSX ) + DBLE( 2 )
      SMLNUM = DLAMCH( 'S' ) / DLAMCH( 'P' )
      BIGNUM = ONE / SMLNUM
      CALL DLABAD( SMLNUM, BIGNUM )
*
*     Scale A, B if max elements outside range [SMLNUM,BIGNUM]
*
      ANRM = DLANGE( 'M', M, N, A, LDA, WORK )
      IASCL = 0
      IF( ANRM.GT.ZERO .AND. ANRM.LT.SMLNUM ) THEN
*
*        Scale matrix norm up to SMLNUM
*
         OPCNT( GELSX ) = OPCNT( GELSX ) + DBLE( M*N )
         CALL DLASCL( 'G', 0, 0, ANRM, SMLNUM, M, N, A, LDA, INFO )
         IASCL = 1
      ELSE IF( ANRM.GT.BIGNUM ) THEN
*
*        Scale matrix norm down to BIGNUM
*
         OPCNT( GELSX ) = OPCNT( GELSX ) + DBLE( M*N )
         CALL DLASCL( 'G', 0, 0, ANRM, BIGNUM, M, N, A, LDA, INFO )
         IASCL = 2
      ELSE IF( ANRM.EQ.ZERO ) THEN
*
*        Matrix all zero. Return zero solution.
*
         CALL DLASET( 'F', MAX( M, N ), NRHS, ZERO, ZERO, B, LDB )
         RANK = 0
         GO TO 100
      END IF
*
      BNRM = DLANGE( 'M', M, NRHS, B, LDB, WORK )
      IBSCL = 0
      IF( BNRM.GT.ZERO .AND. BNRM.LT.SMLNUM ) THEN
*
*        Scale matrix norm up to SMLNUM
*
         OPCNT( GELSX ) = OPCNT( GELSX ) + DBLE( M*NRHS )
         CALL DLASCL( 'G', 0, 0, BNRM, SMLNUM, M, NRHS, B, LDB, INFO )
         IBSCL = 1
      ELSE IF( BNRM.GT.BIGNUM ) THEN
*
*        Scale matrix norm down to BIGNUM
*
         OPCNT( GELSX ) = OPCNT( GELSX ) + DBLE( M*NRHS )
         CALL DLASCL( 'G', 0, 0, BNRM, BIGNUM, M, NRHS, B, LDB, INFO )
         IBSCL = 2
      END IF
*
*     Compute QR factorization with column pivoting of A:
*        A * P = Q * R
*
      OPCNT( GEQPF ) = OPCNT( GEQPF ) + DOPLA( 'DGEQPF', M, N, 0, 0, 0 )
      TIM1 = DSECND( )
      CALL DGEQPF( M, N, A, LDA, JPVT, WORK( 1 ), WORK( MN+1 ), INFO )
      TIM2 = DSECND( )
      TIMNG( GEQPF ) = TIMNG( GEQPF ) + ( TIM2-TIM1 )
*
*     workspace 3*N. Details of Householder rotations stored
*     in WORK(1:MN).
*
*     Determine RANK using incremental condition estimation
*
      WORK( ISMIN ) = ONE
      WORK( ISMAX ) = ONE
      SMAX = ABS( A( 1, 1 ) )
      SMIN = SMAX
      IF( ABS( A( 1, 1 ) ).EQ.ZERO ) THEN
         RANK = 0
         CALL DLASET( 'F', MAX( M, N ), NRHS, ZERO, ZERO, B, LDB )
         GO TO 100
      ELSE
         RANK = 1
      END IF
*
   10 CONTINUE
      IF( RANK.LT.MN ) THEN
         I = RANK + 1
         OPS = 0
         CALL DLAIC1( IMIN, RANK, WORK( ISMIN ), SMIN, A( 1, I ),
     $                A( I, I ), SMINPR, S1, C1 )
         CALL DLAIC1( IMAX, RANK, WORK( ISMAX ), SMAX, A( 1, I ),
     $                A( I, I ), SMAXPR, S2, C2 )
         OPCNT( GELSX ) = OPCNT( GELSX ) + OPS + DBLE( 1 )
*
         IF( SMAXPR*RCOND.LE.SMINPR ) THEN
            OPCNT( GELSX ) = OPCNT( GELSX ) + DBLE( RANK*2 )
            DO 20 I = 1, RANK
               WORK( ISMIN+I-1 ) = S1*WORK( ISMIN+I-1 )
               WORK( ISMAX+I-1 ) = S2*WORK( ISMAX+I-1 )
   20       CONTINUE
            WORK( ISMIN+RANK ) = C1
            WORK( ISMAX+RANK ) = C2
            SMIN = SMINPR
            SMAX = SMAXPR
            RANK = RANK + 1
            GO TO 10
         END IF
      END IF
*
*     Logically partition R = [ R11 R12 ]
*                             [  0  R22 ]
*     where R11 = R(1:RANK,1:RANK)
*
*     [R11,R12] = [ T11, 0 ] * Y
*
      IF( RANK.LT.N ) THEN
         OPCNT( TZRQF ) = OPCNT( TZRQF ) +
     $                    DOPLA( 'DTZRQF', RANK, N, 0, 0, 0 )
         TIM1 = DSECND( )
         CALL DTZRQF( RANK, N, A, LDA, WORK( MN+1 ), INFO )
         TIM2 = DSECND( )
         TIMNG( TZRQF ) = TIMNG( TZRQF ) + ( TIM2-TIM1 )
      END IF
*
*     Details of Householder rotations stored in WORK(MN+1:2*MN)
*
*     B(1:M,1:NRHS) := Q' * B(1:M,1:NRHS)
*
      OPCNT( ORM2R ) = OPCNT( ORM2R ) +
     $                 DOPLA( 'DORMQR', M, NRHS, MN, 0, 0 )
      TIM1 = DSECND( )
      CALL DORM2R( 'Left', 'Transpose', M, NRHS, MN, A, LDA, WORK( 1 ),
     $             B, LDB, WORK( 2*MN+1 ), INFO )
      TIM2 = DSECND( )
      TIMNG( ORM2R ) = TIMNG( ORM2R ) + ( TIM2-TIM1 )
*
*     workspace NRHS
*
*     B(1:RANK,1:NRHS) := inv(T11) * B(1:RANK,1:NRHS)
*
      OPCNT( TRSM ) = OPCNT( TRSM ) + DOPBL3( 'DTRSM ', RANK, NRHS, 0 )
      TIM1 = DSECND( )
      CALL DTRSM( 'Left', 'Upper', 'No transpose', 'Non-unit', RANK,
     $            NRHS, ONE, A, LDA, B, LDB )
      TIM2 = DSECND( )
      TIMNG( TRSM ) = TIMNG( TRSM ) + ( TIM2-TIM1 )
*
      DO 40 I = RANK + 1, N
         DO 30 J = 1, NRHS
            B( I, J ) = ZERO
   30    CONTINUE
   40 CONTINUE
*
*     B(1:N,1:NRHS) := Y' * B(1:N,1:NRHS)
*
      IF( RANK.LT.N ) THEN
         OPCNT( LATZM ) = OPCNT( LATZM ) +
     $                    DBLE( 2*( ( N-RANK )*NRHS+NRHS+( N-RANK )*
     $                    NRHS )*RANK )
         TIM1 = DSECND( )
         DO 50 I = 1, RANK
            CALL DLATZM( 'Left', N-RANK+1, NRHS, A( I, RANK+1 ), LDA,
     $                   WORK( MN+I ), B( I, 1 ), B( RANK+1, 1 ), LDB,
     $                   WORK( 2*MN+1 ) )
   50    CONTINUE
         TIM2 = DSECND( )
         TIMNG( LATZM ) = TIMNG( LATZM ) + ( TIM2-TIM1 )
      END IF
*
*     workspace NRHS
*
*     B(1:N,1:NRHS) := P * B(1:N,1:NRHS)
*
      DO 90 J = 1, NRHS
         DO 60 I = 1, N
            WORK( 2*MN+I ) = NTDONE
   60    CONTINUE
         DO 80 I = 1, N
            IF( WORK( 2*MN+I ).EQ.NTDONE ) THEN
               IF( JPVT( I ).NE.I ) THEN
                  K = I
                  T1 = B( K, J )
                  T2 = B( JPVT( K ), J )
   70             CONTINUE
                  B( JPVT( K ), J ) = T1
                  WORK( 2*MN+K ) = DONE
                  T1 = T2
                  K = JPVT( K )
                  T2 = B( JPVT( K ), J )
                  IF( JPVT( K ).NE.I )
     $               GO TO 70
                  B( I, J ) = T1
                  WORK( 2*MN+K ) = DONE
               END IF
            END IF
   80    CONTINUE
   90 CONTINUE
*
*     Undo scaling
*
      IF( IASCL.EQ.1 ) THEN
         OPCNT( GELSX ) = OPCNT( GELSX ) + DBLE( N*NRHS+RANK*RANK )
         CALL DLASCL( 'G', 0, 0, ANRM, SMLNUM, N, NRHS, B, LDB, INFO )
         CALL DLASCL( 'U', 0, 0, SMLNUM, ANRM, RANK, RANK, A, LDA,
     $                INFO )
      ELSE IF( IASCL.EQ.2 ) THEN
         OPCNT( GELSX ) = OPCNT( GELSX ) + DBLE( N*NRHS+RANK*RANK )
         CALL DLASCL( 'G', 0, 0, ANRM, BIGNUM, N, NRHS, B, LDB, INFO )
         CALL DLASCL( 'U', 0, 0, BIGNUM, ANRM, RANK, RANK, A, LDA,
     $                INFO )
      END IF
      IF( IBSCL.EQ.1 ) THEN
         OPCNT( GELSX ) = OPCNT( GELSX ) + DBLE( N*NRHS )
         CALL DLASCL( 'G', 0, 0, SMLNUM, BNRM, N, NRHS, B, LDB, INFO )
      ELSE IF( IBSCL.EQ.2 ) THEN
         OPCNT( GELSX ) = OPCNT( GELSX ) + DBLE( N*NRHS )
         CALL DLASCL( 'G', 0, 0, BIGNUM, BNRM, N, NRHS, B, LDB, INFO )
      END IF
*
  100 CONTINUE
*
      RETURN
*
*     End of DGELSX
*
      END
      SUBROUTINE DGELSY( M, N, NRHS, A, LDA, B, LDB, JPVT, RCOND, RANK,
     $                   WORK, LWORK, INFO )
*
*  -- LAPACK driver routine (instrumented to count ops, version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     June 30, 1999
*
*     .. Scalar Arguments ..
      INTEGER            INFO, LDA, LDB, LWORK, M, N, NRHS, RANK
      DOUBLE PRECISION   RCOND
*     ..
*     .. Array Arguments ..
      INTEGER            JPVT( * )
      DOUBLE PRECISION   A( LDA, * ), B( LDB, * ), WORK( * )
*     ..
*     Common block to return operation counts and timings
*     .. Common blocks ..
      COMMON             / LATIME / OPS, ITCNT
      COMMON             / LSTIME / OPCNT, TIMNG
*     ..
*     .. Scalars in Common ..
      DOUBLE PRECISION   ITCNT, OPS
*     ..
*     .. Arrays in Common ..
      DOUBLE PRECISION   OPCNT( 6 ), TIMNG( 6 )
*     ..
*
*  Purpose
*  =======
*
*  DGELSY computes the minimum-norm solution to a real linear least
*  squares problem:
*      min || A * X - B ||
*  using a complete orthogonal factorization of A.  A is an M-by-N
*  matrix which may be rank-deficient.
*
*  Several right hand side vectors b and solution vectors x can be
*  handled in a single call; they are stored as the columns of the
*  M-by-NRHS right hand side matrix B and the N-by-NRHS solution
*  matrix X.
*
*  The routine first computes a QR factorization with column pivoting:
*      A * P = Q * [ R11 R12 ]
*                  [  0  R22 ]
*  with R11 defined as the largest leading submatrix whose estimated
*  condition number is less than 1/RCOND.  The order of R11, RANK,
*  is the effective rank of A.
*
*  Then, R22 is considered to be negligible, and R12 is annihilated
*  by orthogonal transformations from the right, arriving at the
*  complete orthogonal factorization:
*     A * P = Q * [ T11 0 ] * Z
*                 [  0  0 ]
*  The minimum-norm solution is then
*     X = P * Z' [ inv(T11)*Q1'*B ]
*                [        0       ]
*  where Q1 consists of the first RANK columns of Q.
*
*  This routine is basically identical to the original xGELSX except
*  three differences:
*    o The call to the subroutine xGEQPF has been substituted by the
*      the call to the subroutine xGEQP3. This subroutine is a Blas-3
*      version of the QR factorization with column pivoting.
*    o Matrix B (the right hand side) is updated with Blas-3.
*    o The permutation of matrix B (the right hand side) is faster and
*      more simple.
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
*  NRHS    (input) INTEGER
*          The number of right hand sides, i.e., the number of
*          columns of matrices B and X. NRHS >= 0.
*
*  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
*          On entry, the M-by-N matrix A.
*          On exit, A has been overwritten by details of its
*          complete orthogonal factorization.
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A.  LDA >= max(1,M).
*
*  B       (input/output) DOUBLE PRECISION array, dimension (LDB,NRHS)
*          On entry, the M-by-NRHS right hand side matrix B.
*          On exit, the N-by-NRHS solution matrix X.
*
*  LDB     (input) INTEGER
*          The leading dimension of the array B. LDB >= max(1,M,N).
*
*  JPVT    (input/output) INTEGER array, dimension (N)
*          On entry, if JPVT(i) .ne. 0, the i-th column of A is permuted
*          to the front of AP, otherwise column i is a free column.
*          On exit, if JPVT(i) = k, then the i-th column of AP
*          was the k-th column of A.
*
*  RCOND   (input) DOUBLE PRECISION
*          RCOND is used to determine the effective rank of A, which
*          is defined as the order of the largest leading triangular
*          submatrix R11 in the QR factorization with pivoting of A,
*          whose estimated condition number < 1/RCOND.
*
*  RANK    (output) INTEGER
*          The effective rank of A, i.e., the order of the submatrix
*          R11.  This is the same as the order of the submatrix T11
*          in the complete orthogonal factorization of A.
*
*  WORK    (workspace/output) DOUBLE PRECISION array, dimension (LWORK)
*          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
*
*  LWORK   (input) INTEGER
*          The dimension of the array WORK.
*          The unblocked strategy requires that:
*             LWORK >= MAX( MN+3*N+1, 2*MN+NRHS ),
*          where MN = min( M, N ).
*          The block algorithm requires that:
*             LWORK >= MAX( MN+2*N+NB*(N+1), 2*MN+NB*NRHS ),
*          where NB is an upper bound on the blocksize returned
*          by ILAENV for the routines DGEQP3, DTZRZF, STZRQF, DORMQR,
*          and DORMRZ.
*
*          If LWORK = -1, then a workspace query is assumed; the routine
*          only calculates the optimal size of the WORK array, returns
*          this value as the first entry of the WORK array, and no error
*          message related to LWORK is issued by XERBLA.
*
*  INFO    (output) INTEGER
*          = 0: successful exit
*          < 0: If INFO = -i, the i-th argument had an illegal value.
*
*  Further Details
*  ===============
*
*  Based on contributions by
*    A. Petitet, Computer Science Dept., Univ. of Tenn., Knoxville, USA
*    E. Quintana-Orti, Depto. de Informatica, Universidad Jaime I, Spain
*    G. Quintana-Orti, Depto. de Informatica, Universidad Jaime I, Spain
*
*  =====================================================================
*
*     .. Parameters ..
      INTEGER            IMAX, IMIN
      PARAMETER          ( IMAX = 1, IMIN = 2 )
      DOUBLE PRECISION   ZERO, ONE
      PARAMETER          ( ZERO = 0.0D+0, ONE = 1.0D+0 )
*     ..
*     .. Local Scalars ..
      LOGICAL            LQUERY
      INTEGER            GELSY, GEQP3, I, IASCL, IBSCL, ISMAX, ISMIN, J,
     $                   LWKOPT, MN, NB, NB1, NB2, NB3, NB4, ORMQR,
     $                   ORMRZ, TRSM, TZRZF
      DOUBLE PRECISION   ANRM, BIGNUM, BNRM, C1, C2, S1, S2, SMAX,
     $                   SMAXPR, SMIN, SMINPR, SMLNUM, T1, T2, WSIZE
*     ..
*     .. External Functions ..
      INTEGER            ILAENV
      DOUBLE PRECISION   DLAMCH, DLANGE, DOPBL3, DOPLA, DSECND
      EXTERNAL           ILAENV, DLAMCH, DLANGE, DOPBL3, DOPLA, DSECND
*     ..
*     .. External Subroutines ..
      EXTERNAL           DCOPY, DGEQP3, DLABAD, DLAIC1, DLASCL, DLASET,
     $                   DORMQR, DORMRZ, DTRSM, DTZRZF, XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, DBLE, MAX, MIN
*     ..
*     .. Data statements ..
      DATA               GELSY / 1 / , GEQP3 / 2 / , ORMQR / 4 / ,
     $                   ORMRZ / 6 / , TRSM / 5 / , TZRZF / 3 /
*     ..
*     .. Executable Statements ..
*
      MN = MIN( M, N )
      ISMIN = MN + 1
      ISMAX = 2*MN + 1
*
*     Test the input arguments.
*
      INFO = 0
      NB1 = ILAENV( 1, 'DGEQRF', ' ', M, N, -1, -1 )
      NB2 = ILAENV( 1, 'DGERQF', ' ', M, N, -1, -1 )
      NB3 = ILAENV( 1, 'DORMQR', ' ', M, N, NRHS, -1 )
      NB4 = ILAENV( 1, 'DORMRQ', ' ', M, N, NRHS, -1 )
      NB = MAX( NB1, NB2, NB3, NB4 )
      LWKOPT = MAX( 1, MN+2*N+NB*( N+1 ), 2*MN+NB*NRHS )
      WORK( 1 ) = DBLE( LWKOPT )
      LQUERY = ( LWORK.EQ.-1 )
      IF( M.LT.0 ) THEN
         INFO = -1
      ELSE IF( N.LT.0 ) THEN
         INFO = -2
      ELSE IF( NRHS.LT.0 ) THEN
         INFO = -3
      ELSE IF( LDA.LT.MAX( 1, M ) ) THEN
         INFO = -5
      ELSE IF( LDB.LT.MAX( 1, M, N ) ) THEN
         INFO = -7
      ELSE IF( LWORK.LT.MAX( 1, MN+3*N+1, 2*MN+NRHS ) .AND. .NOT.
     $         LQUERY ) THEN
         INFO = -12
      END IF
*
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DGELSY', -INFO )
         RETURN
      ELSE IF( LQUERY ) THEN
         RETURN
      END IF
*
*     Quick return if possible
*
      IF( MIN( M, N, NRHS ).EQ.0 ) THEN
         RANK = 0
         RETURN
      END IF
*
*     Get machine parameters
*
      OPCNT( GELSY ) = OPCNT( GELSY ) + DBLE( 2 )
      SMLNUM = DLAMCH( 'S' ) / DLAMCH( 'P' )
      BIGNUM = ONE / SMLNUM
      CALL DLABAD( SMLNUM, BIGNUM )
*
*     Scale A, B if max entries outside range [SMLNUM,BIGNUM]
*
      ANRM = DLANGE( 'M', M, N, A, LDA, WORK )
      IASCL = 0
      IF( ANRM.GT.ZERO .AND. ANRM.LT.SMLNUM ) THEN
*
*        Scale matrix norm up to SMLNUM
*
         OPCNT( GELSY ) = OPCNT( GELSY ) + DBLE( M*N )
         CALL DLASCL( 'G', 0, 0, ANRM, SMLNUM, M, N, A, LDA, INFO )
         IASCL = 1
      ELSE IF( ANRM.GT.BIGNUM ) THEN
*
*        Scale matrix norm down to BIGNUM
*
         OPCNT( GELSY ) = OPCNT( GELSY ) + DBLE( M*N )
         CALL DLASCL( 'G', 0, 0, ANRM, BIGNUM, M, N, A, LDA, INFO )
         IASCL = 2
      ELSE IF( ANRM.EQ.ZERO ) THEN
*
*        Matrix all zero. Return zero solution.
*
         CALL DLASET( 'F', MAX( M, N ), NRHS, ZERO, ZERO, B, LDB )
         RANK = 0
         GO TO 70
      END IF
*
      BNRM = DLANGE( 'M', M, NRHS, B, LDB, WORK )
      IBSCL = 0
      IF( BNRM.GT.ZERO .AND. BNRM.LT.SMLNUM ) THEN
*
*        Scale matrix norm up to SMLNUM
*
         OPCNT( GELSY ) = OPCNT( GELSY ) + DBLE( M*NRHS )
         CALL DLASCL( 'G', 0, 0, BNRM, SMLNUM, M, NRHS, B, LDB, INFO )
         IBSCL = 1
      ELSE IF( BNRM.GT.BIGNUM ) THEN
*
*        Scale matrix norm down to BIGNUM
*
         OPCNT( GELSY ) = OPCNT( GELSY ) + DBLE( M*NRHS )
         CALL DLASCL( 'G', 0, 0, BNRM, BIGNUM, M, NRHS, B, LDB, INFO )
         IBSCL = 2
      END IF
*
*     Compute QR factorization with column pivoting of A:
*        A * P = Q * R
*
      OPCNT( GEQP3 ) = OPCNT( GEQP3 ) + DOPLA( 'DGEQPF', M, N, 0, 0, 0 )
      T1 = DSECND( )
      CALL DGEQP3( M, N, A, LDA, JPVT, WORK( 1 ), WORK( MN+1 ),
     $             LWORK-MN, INFO )
      T2 = DSECND( )
      TIMNG( GEQP3 ) = TIMNG( GEQP3 ) + ( T2-T1 )
      WSIZE = MN + WORK( MN+1 )
*
*     workspace: MN+2*N+NB*(N+1).
*     Details of Householder rotations stored in WORK(1:MN).
*
*     Determine RANK using incremental condition estimation
*
      WORK( ISMIN ) = ONE
      WORK( ISMAX ) = ONE
      SMAX = ABS( A( 1, 1 ) )
      SMIN = SMAX
      IF( ABS( A( 1, 1 ) ).EQ.ZERO ) THEN
         RANK = 0
         CALL DLASET( 'F', MAX( M, N ), NRHS, ZERO, ZERO, B, LDB )
         GO TO 70
      ELSE
         RANK = 1
      END IF
*
   10 CONTINUE
      IF( RANK.LT.MN ) THEN
         I = RANK + 1
         OPS = 0
         CALL DLAIC1( IMIN, RANK, WORK( ISMIN ), SMIN, A( 1, I ),
     $                A( I, I ), SMINPR, S1, C1 )
         CALL DLAIC1( IMAX, RANK, WORK( ISMAX ), SMAX, A( 1, I ),
     $                A( I, I ), SMAXPR, S2, C2 )
         OPCNT( GELSY ) = OPCNT( GELSY ) + OPS + DBLE( 1 )
*
         IF( SMAXPR*RCOND.LE.SMINPR ) THEN
            OPCNT( GELSY ) = OPCNT( GELSY ) + DBLE( RANK*2 )
            DO 20 I = 1, RANK
               WORK( ISMIN+I-1 ) = S1*WORK( ISMIN+I-1 )
               WORK( ISMAX+I-1 ) = S2*WORK( ISMAX+I-1 )
   20       CONTINUE
            WORK( ISMIN+RANK ) = C1
            WORK( ISMAX+RANK ) = C2
            SMIN = SMINPR
            SMAX = SMAXPR
            RANK = RANK + 1
            GO TO 10
         END IF
      END IF
*
*     workspace: 3*MN.
*
*     Logically partition R = [ R11 R12 ]
*                             [  0  R22 ]
*     where R11 = R(1:RANK,1:RANK)
*
*     [R11,R12] = [ T11, 0 ] * Y
*
      IF( RANK.LT.N ) THEN
         OPCNT( TZRZF ) = OPCNT( TZRZF ) +
     $                    DOPLA( 'DTZRQF', RANK, N, 0, 0, 0 )
         T1 = DSECND( )
         CALL DTZRZF( RANK, N, A, LDA, WORK( MN+1 ), WORK( 2*MN+1 ),
     $                LWORK-2*MN, INFO )
         T2 = DSECND( )
         TIMNG( TZRZF ) = TIMNG( TZRZF ) + ( T2-T1 )
      END IF
*
*     workspace: 2*MN.
*     Details of Householder rotations stored in WORK(MN+1:2*MN)
*
*     B(1:M,1:NRHS) := Q' * B(1:M,1:NRHS)
*
      OPCNT( ORMQR ) = OPCNT( ORMQR ) +
     $                 DOPLA( 'DORMQR', M, NRHS, MN, 0, 0 )
      T1 = DSECND( )
      CALL DORMQR( 'Left', 'Transpose', M, NRHS, MN, A, LDA, WORK( 1 ),
     $             B, LDB, WORK( 2*MN+1 ), LWORK-2*MN, INFO )
      T2 = DSECND( )
      TIMNG( ORMQR ) = TIMNG( ORMQR ) + ( T2-T1 )
      WSIZE = MAX( WSIZE, 2*MN+WORK( 2*MN+1 ) )
*
*     workspace: 2*MN+NB*NRHS.
*
*     B(1:RANK,1:NRHS) := inv(T11) * B(1:RANK,1:NRHS)
*
      OPCNT( TRSM ) = OPCNT( TRSM ) + DOPBL3( 'DTRSM ', RANK, NRHS, 0 )
      T1 = DSECND( )
      CALL DTRSM( 'Left', 'Upper', 'No transpose', 'Non-unit', RANK,
     $            NRHS, ONE, A, LDA, B, LDB )
      T2 = DSECND( )
      TIMNG( TRSM ) = TIMNG( TRSM ) + ( T2-T1 )
*
      DO 40 J = 1, NRHS
         DO 30 I = RANK + 1, N
            B( I, J ) = ZERO
   30    CONTINUE
   40 CONTINUE
*
*     B(1:N,1:NRHS) := Y' * B(1:N,1:NRHS)
*
      IF( RANK.LT.N ) THEN
         NB = ILAENV( 1, 'DORMRQ', 'LT', N, NRHS, RANK, -1 )
         OPCNT( ORMRZ ) = OPCNT( ORMRZ ) +
     $                    DOPLA( 'DORMRQ', N, NRHS, RANK, 0, NB )
         T1 = DSECND( )
         CALL DORMRZ( 'Left', 'Transpose', N, NRHS, RANK, N-RANK, A,
     $                LDA, WORK( MN+1 ), B, LDB, WORK( 2*MN+1 ),
     $                LWORK-2*MN, INFO )
         T2 = DSECND( )
         TIMNG( ORMRZ ) = TIMNG( ORMRZ ) + ( T2-T1 )
      END IF
*
*     workspace: 2*MN+NRHS.
*
*     B(1:N,1:NRHS) := P * B(1:N,1:NRHS)
*
      DO 60 J = 1, NRHS
         DO 50 I = 1, N
            WORK( JPVT( I ) ) = B( I, J )
   50    CONTINUE
         CALL DCOPY( N, WORK( 1 ), 1, B( 1, J ), 1 )
   60 CONTINUE
*
*     workspace: N.
*
*     Undo scaling
*
      IF( IASCL.EQ.1 ) THEN
         OPCNT( GELSY ) = OPCNT( GELSY ) + DBLE( N*NRHS+RANK*RANK )
         CALL DLASCL( 'G', 0, 0, ANRM, SMLNUM, N, NRHS, B, LDB, INFO )
         CALL DLASCL( 'U', 0, 0, SMLNUM, ANRM, RANK, RANK, A, LDA,
     $                INFO )
      ELSE IF( IASCL.EQ.2 ) THEN
         OPCNT( GELSY ) = OPCNT( GELSY ) + DBLE( N*NRHS+RANK*RANK )
         CALL DLASCL( 'G', 0, 0, ANRM, BIGNUM, N, NRHS, B, LDB, INFO )
         CALL DLASCL( 'U', 0, 0, BIGNUM, ANRM, RANK, RANK, A, LDA,
     $                INFO )
      END IF
      IF( IBSCL.EQ.1 ) THEN
         OPCNT( GELSY ) = OPCNT( GELSY ) + DBLE( N*NRHS )
         CALL DLASCL( 'G', 0, 0, SMLNUM, BNRM, N, NRHS, B, LDB, INFO )
      ELSE IF( IBSCL.EQ.2 ) THEN
         OPCNT( GELSY ) = OPCNT( GELSY ) + DBLE( N*NRHS )
         CALL DLASCL( 'G', 0, 0, BIGNUM, BNRM, N, NRHS, B, LDB, INFO )
      END IF
*
   70 CONTINUE
      WORK( 1 ) = DBLE( LWKOPT )
*
      RETURN
*
*     End of DGELSY
*
      END
      SUBROUTINE DLAIC1( JOB, J, X, SEST, W, GAMMA, SESTPR, S, C )
*
*  -- LAPACK auxiliary routine (instrumented to count ops, version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     June 30, 1999
*
*     .. Scalar Arguments ..
      INTEGER            J, JOB
      DOUBLE PRECISION   C, GAMMA, S, SEST, SESTPR
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   W( J ), X( J )
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
*  DLAIC1 applies one step of incremental condition estimation in
*  its simplest version:
*
*  Let x, twonorm(x) = 1, be an approximate singular vector of an j-by-j
*  lower triangular matrix L, such that
*           twonorm(L*x) = sest
*  Then DLAIC1 computes sestpr, s, c such that
*  the vector
*                  [ s*x ]
*           xhat = [  c  ]
*  is an approximate singular vector of
*                  [ L     0  ]
*           Lhat = [ w' gamma ]
*  in the sense that
*           twonorm(Lhat*xhat) = sestpr.
*
*  Depending on JOB, an estimate for the largest or smallest singular
*  value is computed.
*
*  Note that [s c]' and sestpr**2 is an eigenpair of the system
*
*      diag(sest*sest, 0) + [alpha  gamma] * [ alpha ]
*                                            [ gamma ]
*
*  where  alpha =  x'*w.
*
*  Arguments
*  =========
*
*  JOB     (input) INTEGER
*          = 1: an estimate for the largest singular value is computed.
*          = 2: an estimate for the smallest singular value is computed.
*
*  J       (input) INTEGER
*          Length of X and W
*
*  X       (input) DOUBLE PRECISION array, dimension (J)
*          The j-vector x.
*
*  SEST    (input) DOUBLE PRECISION
*          Estimated singular value of j by j matrix L
*
*  W       (input) DOUBLE PRECISION array, dimension (J)
*          The j-vector w.
*
*  GAMMA   (input) DOUBLE PRECISION
*          The diagonal element gamma.
*
*  SESTPR  (output) DOUBLE PRECISION
*          Estimated singular value of (j+1) by (j+1) matrix Lhat.
*
*  S       (output) DOUBLE PRECISION
*          Sine needed in forming xhat.
*
*  C       (output) DOUBLE PRECISION
*          Cosine needed in forming xhat.
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE, TWO
      PARAMETER          ( ZERO = 0.0D0, ONE = 1.0D0, TWO = 2.0D0 )
      DOUBLE PRECISION   HALF, FOUR
      PARAMETER          ( HALF = 0.5D0, FOUR = 4.0D0 )
*     ..
*     .. Local Scalars ..
      DOUBLE PRECISION   ABSALP, ABSEST, ABSGAM, ALPHA, B, COSINE, EPS,
     $                   NORMA, S1, S2, SINE, T, TEST, TMP, ZETA1, ZETA2
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, MAX, SIGN, SQRT
*     ..
*     .. External Functions ..
      DOUBLE PRECISION   DDOT, DLAMCH
      EXTERNAL           DDOT, DLAMCH
*     ..
*     .. Executable Statements ..
*
      EPS = DLAMCH( 'Epsilon' )
      ALPHA = DDOT( J, X, 1, W, 1 )
*
      ABSALP = ABS( ALPHA )
      ABSGAM = ABS( GAMMA )
      ABSEST = ABS( SEST )
*
      IF( JOB.EQ.1 ) THEN
*
*        Estimating largest singular value
*
*        special cases
*
         IF( SEST.EQ.ZERO ) THEN
            S1 = MAX( ABSGAM, ABSALP )
            IF( S1.EQ.ZERO ) THEN
               S = ZERO
               C = ONE
               SESTPR = ZERO
            ELSE
               OPS = OPS + 9
               S = ALPHA / S1
               C = GAMMA / S1
               TMP = SQRT( S*S+C*C )
               S = S / TMP
               C = C / TMP
               SESTPR = S1*TMP
            END IF
            RETURN
         ELSE IF( ABSGAM.LE.EPS*ABSEST ) THEN
            OPS = OPS + 7
            S = ONE
            C = ZERO
            TMP = MAX( ABSEST, ABSALP )
            S1 = ABSEST / TMP
            S2 = ABSALP / TMP
            SESTPR = TMP*SQRT( S1*S1+S2*S2 )
            RETURN
         ELSE IF( ABSALP.LE.EPS*ABSEST ) THEN
            S1 = ABSGAM
            S2 = ABSEST
            IF( S1.LE.S2 ) THEN
               S = ONE
               C = ZERO
               SESTPR = S2
            ELSE
               S = ZERO
               C = ONE
               SESTPR = S1
            END IF
            RETURN
         ELSE IF( ABSEST.LE.EPS*ABSALP .OR. ABSEST.LE.EPS*ABSGAM ) THEN
            S1 = ABSGAM
            S2 = ABSALP
            IF( S1.LE.S2 ) THEN
               OPS = OPS + 8
               TMP = S1 / S2
               S = SQRT( ONE+TMP*TMP )
               SESTPR = S2*S
               C = ( GAMMA / S2 ) / S
               S = SIGN( ONE, ALPHA ) / S
            ELSE
               OPS = OPS + 8
               TMP = S2 / S1
               C = SQRT( ONE+TMP*TMP )
               SESTPR = S1*C
               S = ( ALPHA / S1 ) / C
               C = SIGN( ONE, GAMMA ) / C
            END IF
            RETURN
         ELSE
*
*           normal case
*
            OPS = OPS + 8
            ZETA1 = ALPHA / ABSEST
            ZETA2 = GAMMA / ABSEST
*
            B = ( ONE-ZETA1*ZETA1-ZETA2*ZETA2 )*HALF
            C = ZETA1*ZETA1
            IF( B.GT.ZERO ) THEN
               OPS = OPS + 5
               T = C / ( B+SQRT( B*B+C ) )
            ELSE
               OPS = OPS + 4
               T = SQRT( B*B+C ) - B
            END IF
*
            OPS = OPS + 12
            SINE = -ZETA1 / T
            COSINE = -ZETA2 / ( ONE+T )
            TMP = SQRT( SINE*SINE+COSINE*COSINE )
            S = SINE / TMP
            C = COSINE / TMP
            SESTPR = SQRT( T+ONE )*ABSEST
            RETURN
         END IF
*
      ELSE IF( JOB.EQ.2 ) THEN
*
*        Estimating smallest singular value
*
*        special cases
*
         IF( SEST.EQ.ZERO ) THEN
            SESTPR = ZERO
            IF( MAX( ABSGAM, ABSALP ).EQ.ZERO ) THEN
               SINE = ONE
               COSINE = ZERO
            ELSE
               SINE = -GAMMA
               COSINE = ALPHA
            END IF
            OPS = OPS + 7
            S1 = MAX( ABS( SINE ), ABS( COSINE ) )
            S = SINE / S1
            C = COSINE / S1
            TMP = SQRT( S*S+C*C )
            S = S / TMP
            C = C / TMP
            RETURN
         ELSE IF( ABSGAM.LE.EPS*ABSEST ) THEN
            S = ZERO
            C = ONE
            SESTPR = ABSGAM
            RETURN
         ELSE IF( ABSALP.LE.EPS*ABSEST ) THEN
            S1 = ABSGAM
            S2 = ABSEST
            IF( S1.LE.S2 ) THEN
               S = ZERO
               C = ONE
               SESTPR = S1
            ELSE
               S = ONE
               C = ZERO
               SESTPR = S2
            END IF
            RETURN
         ELSE IF( ABSEST.LE.EPS*ABSALP .OR. ABSEST.LE.EPS*ABSGAM ) THEN
            S1 = ABSGAM
            S2 = ABSALP
            IF( S1.LE.S2 ) THEN
               OPS = OPS + 9
               TMP = S1 / S2
               C = SQRT( ONE+TMP*TMP )
               SESTPR = ABSEST*( TMP / C )
               S = -( GAMMA / S2 ) / C
               C = SIGN( ONE, ALPHA ) / C
            ELSE
               OPS = OPS + 8
               TMP = S2 / S1
               S = SQRT( ONE+TMP*TMP )
               SESTPR = ABSEST / S
               C = ( ALPHA / S1 ) / S
               S = -SIGN( ONE, GAMMA ) / S
            END IF
            RETURN
         ELSE
*
*           normal case
*
            OPS = OPS + 14
            ZETA1 = ALPHA / ABSEST
            ZETA2 = GAMMA / ABSEST
*
            NORMA = MAX( ONE+ZETA1*ZETA1+ABS( ZETA1*ZETA2 ),
     $              ABS( ZETA1*ZETA2 )+ZETA2*ZETA2 )
*
*           See if root is closer to zero or to ONE
*
            TEST = ONE + TWO*( ZETA1-ZETA2 )*( ZETA1+ZETA2 )
            IF( TEST.GE.ZERO ) THEN
*
*              root is close to zero, compute directly
*
               OPS = OPS + 20
               B = ( ZETA1*ZETA1+ZETA2*ZETA2+ONE )*HALF
               C = ZETA2*ZETA2
               T = C / ( B+SQRT( ABS( B*B-C ) ) )
               SINE = ZETA1 / ( ONE-T )
               COSINE = -ZETA2 / T
               SESTPR = SQRT( T+FOUR*EPS*EPS*NORMA )*ABSEST
            ELSE
*
*              root is closer to ONE, shift by that amount
*
               OPS = OPS + 6
               B = ( ZETA2*ZETA2+ZETA1*ZETA1-ONE )*HALF
               C = ZETA1*ZETA1
               IF( B.GE.ZERO ) THEN
                  OPS = OPS + 5
                  T = -C / ( B+SQRT( B*B+C ) )
               ELSE
                  OPS = OPS + 4
                  T = B - SQRT( B*B+C )
               END IF
                  OPS = OPS + 10
               SINE = -ZETA1 / T
               COSINE = -ZETA2 / ( ONE+T )
               SESTPR = SQRT( ONE+T+FOUR*EPS*EPS*NORMA )*ABSEST
            END IF
            OPS = OPS + 6
            TMP = SQRT( SINE*SINE+COSINE*COSINE )
            S = SINE / TMP
            C = COSINE / TMP
            RETURN
*
         END IF
      END IF
      RETURN
*
*     End of DLAIC1
*
      END
      SUBROUTINE DLALS0( ICOMPQ, NL, NR, SQRE, NRHS, B, LDB, BX, LDBX,
     $                   PERM, GIVPTR, GIVCOL, LDGCOL, GIVNUM, LDGNUM,
     $                   POLES, DIFL, DIFR, Z, K, C, S, WORK, INFO )
*
*  -- LAPACK routine (instrumented to count ops, version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     December 22, 1999
*
*     .. Scalar Arguments ..
      INTEGER            GIVPTR, ICOMPQ, INFO, K, LDB, LDBX, LDGCOL,
     $                   LDGNUM, NL, NR, NRHS, SQRE
      DOUBLE PRECISION   C, S
*     ..
*     .. Array Arguments ..
      INTEGER            GIVCOL( LDGCOL, * ), PERM( * )
      DOUBLE PRECISION   B( LDB, * ), BX( LDBX, * ), DIFL( * ),
     $                   DIFR( LDGNUM, * ), GIVNUM( LDGNUM, * ),
     $                   POLES( LDGNUM, * ), WORK( * ), Z( * )
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
*  DLALS0 applies back the multiplying factors of either the left or the
*  right singular vector matrix of a diagonal matrix appended by a row
*  to the right hand side matrix B in solving the least squares problem
*  using the divide-and-conquer SVD approach.
*
*  For the left singular vector matrix, three types of orthogonal
*  matrices are involved:
*
*  (1L) Givens rotations: the number of such rotations is GIVPTR; the
*       pairs of columns/rows they were applied to are stored in GIVCOL;
*       and the C- and S-values of these rotations are stored in GIVNUM.
*
*  (2L) Permutation. The (NL+1)-st row of B is to be moved to the first
*       row, and for J=2:N, PERM(J)-th row of B is to be moved to the
*       J-th row.
*
*  (3L) The left singular vector matrix of the remaining matrix.
*
*  For the right singular vector matrix, four types of orthogonal
*  matrices are involved:
*
*  (1R) The right singular vector matrix of the remaining matrix.
*
*  (2R) If SQRE = 1, one extra Givens rotation to generate the right
*       null space.
*
*  (3R) The inverse transformation of (2L).
*
*  (4R) The inverse transformation of (1L).
*
*  Arguments
*  =========
*
*  ICOMPQ (input) INTEGER
*         Specifies whether singular vectors are to be computed in
*         factored form:
*         = 0: Left singular vector matrix.
*         = 1: Right singular vector matrix.
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
*         The bidiagonal matrix has row dimension N = NL + NR + 1,
*         and column dimension M = N + SQRE.
*
*  NRHS   (input) INTEGER
*         The number of columns of B and BX. NRHS must be at least 1.
*
*  B      (input/output) DOUBLE PRECISION array, dimension ( LDB, NRHS )
*         On input, B contains the right hand sides of the least
*         squares problem in rows 1 through M. On output, B contains
*         the solution X in rows 1 through N.
*
*  LDB    (input) INTEGER
*         The leading dimension of B. LDB must be at least
*         max(1,MAX( M, N ) ).
*
*  BX     (workspace) DOUBLE PRECISION array, dimension ( LDBX, NRHS )
*
*  LDBX   (input) INTEGER
*         The leading dimension of BX.
*
*  PERM   (input) INTEGER array, dimension ( N )
*         The permutations (from deflation and sorting) applied
*         to the two blocks.
*
*  GIVPTR (input) INTEGER
*         The number of Givens rotations which took place in this
*         subproblem.
*
*  GIVCOL (input) INTEGER array, dimension ( LDGCOL, 2 )
*         Each pair of numbers indicates a pair of rows/columns
*         involved in a Givens rotation.
*
*  LDGCOL (input) INTEGER
*         The leading dimension of GIVCOL, must be at least N.
*
*  GIVNUM (input) DOUBLE PRECISION array, dimension ( LDGNUM, 2 )
*         Each number indicates the C or S value used in the
*         corresponding Givens rotation.
*
*  LDGNUM (input) INTEGER
*         The leading dimension of arrays DIFR, POLES and
*         GIVNUM, must be at least K.
*
*  POLES  (input) DOUBLE PRECISION array, dimension ( LDGNUM, 2 )
*         On entry, POLES(1:K, 1) contains the new singular
*         values obtained from solving the secular equation, and
*         POLES(1:K, 2) is an array containing the poles in the secular
*         equation.
*
*  DIFL   (input) DOUBLE PRECISION array, dimension ( K ).
*         On entry, DIFL(I) is the distance between I-th updated
*         (undeflated) singular value and the I-th (undeflated) old
*         singular value.
*
*  DIFR   (input) DOUBLE PRECISION array, dimension ( LDGNUM, 2 ).
*         On entry, DIFR(I, 1) contains the distances between I-th
*         updated (undeflated) singular value and the I+1-th
*         (undeflated) old singular value. And DIFR(I, 2) is the
*         normalizing factor for the I-th right singular vector.
*
*  Z      (input) DOUBLE PRECISION array, dimension ( K )
*         Contain the components of the deflation-adjusted updating row
*         vector.
*
*  K      (input) INTEGER
*         Contains the dimension of the non-deflated matrix,
*         This is the order of the related secular equation. 1 <= K <=N.
*
*  C      (input) DOUBLE PRECISION
*         C contains garbage if SQRE =0 and the C-value of a Givens
*         rotation related to the right null space if SQRE = 1.
*
*  S      (input) DOUBLE PRECISION
*         S contains garbage if SQRE =0 and the S-value of a Givens
*         rotation related to the right null space if SQRE = 1.
*
*  WORK   (workspace) DOUBLE PRECISION array, dimension ( K )
*
*  INFO   (output) INTEGER
*          = 0:  successful exit.
*          < 0:  if INFO = -i, the i-th argument had an illegal value.
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ONE, ZERO, NEGONE
      PARAMETER          ( ONE = 1.0D0, ZERO = 0.0D0, NEGONE = -1.0D0 )
*     ..
*     .. Local Scalars ..
      INTEGER            I, J, M, N, NLP1
      DOUBLE PRECISION   DIFLJ, DIFRJ, DJ, DSIGJ, DSIGJP, TEMP
*     ..
*     .. External Subroutines ..
      EXTERNAL           DCOPY, DGEMV, DLACPY, DLASCL, DROT, DSCAL,
     $                   XERBLA
*     ..
*     .. External Functions ..
      DOUBLE PRECISION   DLAMC3, DNRM2, DOPBL2
      EXTERNAL           DLAMC3, DNRM2, DOPBL2
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
      ELSE IF( NL.LT.1 ) THEN
         INFO = -2
      ELSE IF( NR.LT.1 ) THEN
         INFO = -3
      ELSE IF( ( SQRE.LT.0 ) .OR. ( SQRE.GT.1 ) ) THEN
         INFO = -4
      END IF
*
      N = NL + NR + 1
*
      IF( NRHS.LT.1 ) THEN
         INFO = -5
      ELSE IF( LDB.LT.N ) THEN
         INFO = -7
      ELSE IF( LDBX.LT.N ) THEN
         INFO = -9
      ELSE IF( GIVPTR.LT.0 ) THEN
         INFO = -11
      ELSE IF( LDGCOL.LT.N ) THEN
         INFO = -13
      ELSE IF( LDGNUM.LT.N ) THEN
         INFO = -15
      ELSE IF( K.LT.1 ) THEN
         INFO = -20
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DLALS0', -INFO )
         RETURN
      END IF
*
      M = N + SQRE
      NLP1 = NL + 1
*
      IF( ICOMPQ.EQ.0 ) THEN
*
*        Apply back orthogonal transformations from the left.
*
*        Step (1L): apply back the Givens rotations performed.
*
         OPS = OPS + DBLE( 6*NRHS*GIVPTR )
         DO 10 I = 1, GIVPTR
            CALL DROT( NRHS, B( GIVCOL( I, 2 ), 1 ), LDB,
     $                 B( GIVCOL( I, 1 ), 1 ), LDB, GIVNUM( I, 2 ),
     $                 GIVNUM( I, 1 ) )
   10    CONTINUE
*
*        Step (2L): permute rows of B.
*
         CALL DCOPY( NRHS, B( NLP1, 1 ), LDB, BX( 1, 1 ), LDBX )
         DO 20 I = 2, N
            CALL DCOPY( NRHS, B( PERM( I ), 1 ), LDB, BX( I, 1 ), LDBX )
   20    CONTINUE
*
*        Step (3L): apply the inverse of the left singular vector
*        matrix to BX.
*
         IF( K.EQ.1 ) THEN
            CALL DCOPY( NRHS, BX, LDBX, B, LDB )
            IF( Z( 1 ).LT.ZERO ) THEN
               OPS = OPS + DBLE( NRHS )
               CALL DSCAL( NRHS, NEGONE, B, LDB )
            END IF
         ELSE
            DO 50 J = 1, K
               DIFLJ = DIFL( J )
               DJ = POLES( J, 1 )
               DSIGJ = -POLES( J, 2 )
               IF( J.LT.K ) THEN
                  DIFRJ = -DIFR( J, 1 )
                  DSIGJP = -POLES( J+1, 2 )
               END IF
               IF( ( Z( J ).EQ.ZERO ) .OR. ( POLES( J, 2 ).EQ.ZERO ) )
     $              THEN
                  WORK( J ) = ZERO
               ELSE
                  OPS = OPS + DBLE( 4 )
                  WORK( J ) = -POLES( J, 2 )*Z( J ) / DIFLJ /
     $                        ( POLES( J, 2 )+DJ )
               END IF
               DO 30 I = 1, J - 1
                  IF( ( Z( I ).EQ.ZERO ) .OR.
     $                ( POLES( I, 2 ).EQ.ZERO ) ) THEN
                     WORK( I ) = ZERO
                  ELSE
                     OPS = OPS + DBLE( 6 )
                     WORK( I ) = POLES( I, 2 )*Z( I ) /
     $                           ( DLAMC3( POLES( I, 2 ), DSIGJ )-
     $                           DIFLJ ) / ( POLES( I, 2 )+DJ )
                  END IF
   30          CONTINUE
               DO 40 I = J + 1, K
                  IF( ( Z( I ).EQ.ZERO ) .OR.
     $                ( POLES( I, 2 ).EQ.ZERO ) ) THEN
                     WORK( I ) = ZERO
                  ELSE
                     OPS = OPS + DBLE( 6 )
                     WORK( I ) = POLES( I, 2 )*Z( I ) /
     $                           ( DLAMC3( POLES( I, 2 ), DSIGJP )+
     $                           DIFRJ ) / ( POLES( I, 2 )+DJ )
                  END IF
   40          CONTINUE
               WORK( 1 ) = NEGONE
               OPS = OPS + 2*K + NRHS +
     $               DOPBL2( 'DGEMV ', K, NRHS, 0, 0 )
               TEMP = DNRM2( K, WORK, 1 )
               CALL DGEMV( 'T', K, NRHS, ONE, BX, LDBX, WORK, 1, ZERO,
     $                     B( J, 1 ), LDB )
               CALL DLASCL( 'G', 0, 0, TEMP, ONE, 1, NRHS, B( J, 1 ),
     $                      LDB, INFO )
   50       CONTINUE
         END IF
*
*        Move the deflated rows of BX to B also.
*
         IF( K.LT.MAX( M, N ) )
     $      CALL DLACPY( 'A', N-K, NRHS, BX( K+1, 1 ), LDBX,
     $                   B( K+1, 1 ), LDB )
      ELSE
*
*        Apply back the right orthogonal transformations.
*
*        Step (1R): apply back the new right singular vector matrix
*        to B.
*
         IF( K.EQ.1 ) THEN
            CALL DCOPY( NRHS, B, LDB, BX, LDBX )
         ELSE
            DO 80 J = 1, K
               DSIGJ = POLES( J, 2 )
               IF( Z( J ).EQ.ZERO ) THEN
                  WORK( J ) = ZERO
               ELSE
                  OPS = OPS + DBLE( 4 )
                  WORK( J ) = -Z( J ) / DIFL( J ) /
     $                        ( DSIGJ+POLES( J, 1 ) ) / DIFR( J, 2 )
               END IF
               DO 60 I = 1, J - 1
                  IF( Z( J ).EQ.ZERO ) THEN
                     WORK( I ) = ZERO
                  ELSE
                     OPS = OPS + DBLE( 6 )
                     WORK( I ) = Z( J ) / ( DLAMC3( DSIGJ, -POLES( I+1,
     $                           2 ) )-DIFR( I, 1 ) ) /
     $                           ( DSIGJ+POLES( I, 1 ) ) / DIFR( I, 2 )
                  END IF
   60          CONTINUE
               DO 70 I = J + 1, K
                  IF( Z( J ).EQ.ZERO ) THEN
                     WORK( I ) = ZERO
                  ELSE
                     OPS = OPS + DBLE( 6 )
                     WORK( I ) = Z( J ) / ( DLAMC3( DSIGJ, -POLES( I,
     $                           2 ) )-DIFL( I ) ) /
     $                           ( DSIGJ+POLES( I, 1 ) ) / DIFR( I, 2 )
                  END IF
   70          CONTINUE
               OPS = OPS + DOPBL2( 'DGEMV ', K, NRHS, 0, 0 ) 
               CALL DGEMV( 'T', K, NRHS, ONE, B, LDB, WORK, 1, ZERO,
     $                     BX( J, 1 ), LDBX )
   80       CONTINUE
         END IF
*
*        Step (2R): if SQRE = 1, apply back the rotation that is
*        related to the right null space of the subproblem.
*
         IF( SQRE.EQ.1 ) THEN
            OPS = OPS + DBLE( 6*NRHS )
            CALL DCOPY( NRHS, B( M, 1 ), LDB, BX( M, 1 ), LDBX )
            CALL DROT( NRHS, BX( 1, 1 ), LDBX, BX( M, 1 ), LDBX, C, S )
         END IF
         IF( K.LT.MAX( M, N ) )
     $      CALL DLACPY( 'A', N-K, NRHS, B( K+1, 1 ), LDB,
     $                   BX( K+1, 1 ), LDBX )
*
*        Step (3R): permute rows of B.
*
         CALL DCOPY( NRHS, BX( 1, 1 ), LDBX, B( NLP1, 1 ), LDB )
         IF( SQRE.EQ.1 ) THEN
            CALL DCOPY( NRHS, BX( M, 1 ), LDBX, B( M, 1 ), LDB )
         END IF
         DO 90 I = 2, N
            CALL DCOPY( NRHS, BX( I, 1 ), LDBX, B( PERM( I ), 1 ), LDB )
   90    CONTINUE
*
*        Step (4R): apply back the Givens rotations performed.
*
         OPS = OPS + DBLE( 6*NRHS*GIVPTR )
         DO 100 I = GIVPTR, 1, -1
            CALL DROT( NRHS, B( GIVCOL( I, 2 ), 1 ), LDB,
     $                 B( GIVCOL( I, 1 ), 1 ), LDB, GIVNUM( I, 2 ),
     $                 -GIVNUM( I, 1 ) )
  100    CONTINUE
      END IF
*
      RETURN
*
*     End of DLALS0
*
      END
      SUBROUTINE DLALSA( ICOMPQ, SMLSIZ, N, NRHS, B, LDB, BX, LDBX, U,
     $                   LDU, VT, K, DIFL, DIFR, Z, POLES, GIVPTR,
     $                   GIVCOL, LDGCOL, PERM, GIVNUM, C, S, WORK,
     $                   IWORK, INFO )
*
*  -- LAPACK routine (instrumented to count ops, version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     June 30, 1999
*
*     .. Scalar Arguments ..
      INTEGER            ICOMPQ, INFO, LDB, LDBX, LDGCOL, LDU, N, NRHS,
     $                   SMLSIZ
*     ..
*     .. Array Arguments ..
      INTEGER            GIVCOL( LDGCOL, * ), GIVPTR( * ), IWORK( * ),
     $                   K( * ), PERM( LDGCOL, * )
      DOUBLE PRECISION   B( LDB, * ), BX( LDBX, * ), C( * ),
     $                   DIFL( LDU, * ), DIFR( LDU, * ),
     $                   GIVNUM( LDU, * ), POLES( LDU, * ), S( * ),
     $                   U( LDU, * ), VT( LDU, * ), WORK( * ),
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
*  DLALSA is an itermediate step in solving the least squares problem
*  by computing the SVD of the coefficient matrix in compact form (The
*  singular vectors are computed as products of simple orthorgonal
*  matrices.).
*
*  If ICOMPQ = 0, DLALSA applies the inverse of the left singular vector
*  matrix of an upper bidiagonal matrix to the right hand side; and if
*  ICOMPQ = 1, DLALSA applies the right singular vector matrix to the
*  right hand side. The singular vector matrices were generated in
*  compact form by DLALSA.
*
*  Arguments
*  =========
*
*
*  ICOMPQ (input) INTEGER
*         Specifies whether the left or the right singular vector
*         matrix is involved.
*         = 0: Left singular vector matrix
*         = 1: Right singular vector matrix
*
*  SMLSIZ (input) INTEGER
*         The maximum size of the subproblems at the bottom of the
*         computation tree.
*
*  N      (input) INTEGER
*         The row and column dimensions of the upper bidiagonal matrix.
*
*  NRHS   (input) INTEGER
*         The number of columns of B and BX. NRHS must be at least 1.
*
*  B      (input) DOUBLE PRECISION array, dimension ( LDB, NRHS )
*         On input, B contains the right hand sides of the least
*         squares problem in rows 1 through M. On output, B contains
*         the solution X in rows 1 through N.
*
*  LDB    (input) INTEGER
*         The leading dimension of B in the calling subprogram.
*         LDB must be at least max(1,MAX( M, N ) ).
*
*  BX     (output) DOUBLE PRECISION array, dimension ( LDBX, NRHS )
*         On exit, the result of applying the left or right singular
*         vector matrix to B.
*
*  LDBX   (input) INTEGER
*         The leading dimension of BX.
*
*  U      (input) DOUBLE PRECISION array, dimension ( LDU, SMLSIZ ).
*         On entry, U contains the left singular vector matrices of all
*         subproblems at the bottom level.
*
*  LDU    (input) INTEGER, LDU = > N.
*         The leading dimension of arrays U, VT, DIFL, DIFR,
*         POLES, GIVNUM, and Z.
*
*  VT     (input) DOUBLE PRECISION array, dimension ( LDU, SMLSIZ+1 ).
*         On entry, VT' contains the right singular vector matrices of
*         all subproblems at the bottom level.
*
*  K      (input) INTEGER array, dimension ( N ).
*
*  DIFL   (input) DOUBLE PRECISION array, dimension ( LDU, NLVL ).
*         where NLVL = INT(log_2 (N/(SMLSIZ+1))) + 1.
*
*  DIFR   (input) DOUBLE PRECISION array, dimension ( LDU, 2 * NLVL ).
*         On entry, DIFL(*, I) and DIFR(*, 2 * I -1) record
*         distances between singular values on the I-th level and
*         singular values on the (I -1)-th level, and DIFR(*, 2 * I)
*         record the normalizing factors of the right singular vectors
*         matrices of subproblems on I-th level.
*
*  Z      (input) DOUBLE PRECISION array, dimension ( LDU, NLVL ).
*         On entry, Z(1, I) contains the components of the deflation-
*         adjusted updating row vector for subproblems on the I-th
*         level.
*
*  POLES  (input) DOUBLE PRECISION array, dimension ( LDU, 2 * NLVL ).
*         On entry, POLES(*, 2 * I -1: 2 * I) contains the new and old
*         singular values involved in the secular equations on the I-th
*         level.
*
*  GIVPTR (input) INTEGER array, dimension ( N ).
*         On entry, GIVPTR( I ) records the number of Givens
*         rotations performed on the I-th problem on the computation
*         tree.
*
*  GIVCOL (input) INTEGER array, dimension ( LDGCOL, 2 * NLVL ).
*         On entry, for each I, GIVCOL(*, 2 * I - 1: 2 * I) records the
*         locations of Givens rotations performed on the I-th level on
*         the computation tree.
*
*  LDGCOL (input) INTEGER, LDGCOL = > N.
*         The leading dimension of arrays GIVCOL and PERM.
*
*  PERM   (input) INTEGER array, dimension ( LDGCOL, NLVL ).
*         On entry, PERM(*, I) records permutations done on the I-th
*         level of the computation tree.
*
*  GIVNUM (input) DOUBLE PRECISION array, dimension ( LDU, 2 * NLVL ).
*         On entry, GIVNUM(*, 2 *I -1 : 2 * I) records the C- and S-
*         values of Givens rotations performed on the I-th level on the
*         computation tree.
*
*  C      (input) DOUBLE PRECISION array, dimension ( N ).
*         On entry, if the I-th subproblem is not square,
*         C( I ) contains the C-value of a Givens rotation related to
*         the right null space of the I-th subproblem.
*
*  S      (input) DOUBLE PRECISION array, dimension ( N ).
*         On entry, if the I-th subproblem is not square,
*         S( I ) contains the S-value of a Givens rotation related to
*         the right null space of the I-th subproblem.
*
*  WORK   (workspace) DOUBLE PRECISION array.
*         The dimension must be at least N.
*
*  IWORK  (workspace) INTEGER array.
*         The dimension must be at least 3 * N
*
*  INFO   (output) INTEGER
*          = 0:  successful exit.
*          < 0:  if INFO = -i, the i-th argument had an illegal value.
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE
      PARAMETER          ( ZERO = 0.0D0, ONE = 1.0D0 )
*     ..
*     .. Local Scalars ..
      INTEGER            I, I1, IC, IM1, INODE, J, LF, LL, LVL, LVL2,
     $                   ND, NDB1, NDIML, NDIMR, NL, NLF, NLP1, NLVL,
     $                   NR, NRF, NRP1, SQRE
*     ..
*     .. External Subroutines ..
      EXTERNAL           DCOPY, DGEMM, DLALS0, DLASDT, XERBLA
*     ..
*     .. External Functions ..
      DOUBLE PRECISION   DOPBL3
      EXTERNAL           DOPBL3
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
      ELSE IF( N.LT.SMLSIZ ) THEN
         INFO = -3
      ELSE IF( NRHS.LT.1 ) THEN
         INFO = -4
      ELSE IF( LDB.LT.N ) THEN
         INFO = -6
      ELSE IF( LDBX.LT.N ) THEN
         INFO = -8
      ELSE IF( LDU.LT.N ) THEN
         INFO = -10
      ELSE IF( LDGCOL.LT.N ) THEN
         INFO = -19
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DLALSA', -INFO )
         RETURN
      END IF
*
*     Book-keeping and  setting up the computation tree.
*
      INODE = 1
      NDIML = INODE + N
      NDIMR = NDIML + N
*
      CALL DLASDT( N, NLVL, ND, IWORK( INODE ), IWORK( NDIML ),
     $             IWORK( NDIMR ), SMLSIZ )
*
*     The following code applies back the left singular vector factors.
*     For applying back the right singular vector factors, go to 50.
*
      IF( ICOMPQ.EQ.1 ) THEN
         GO TO 50
      END IF
*
*     The nodes on the bottom level of the tree were solved by DLASDQ.
*     The corresponding left and right singular vector matrices are in
*     explicit form. First apply back the left singular vector matrices.
*
      NDB1 = ( ND+1 ) / 2
      DO 10 I = NDB1, ND
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
         NR = IWORK( NDIMR+I1 )
         NLF = IC - NL
         NRF = IC + 1
         OPS = OPS + DOPBL3( 'DGEMM ', NL, NRHS, NL ) 
         OPS = OPS + DOPBL3( 'DGEMM ', NR, NRHS, NR ) 
         CALL DGEMM( 'T', 'N', NL, NRHS, NL, ONE, U( NLF, 1 ), LDU,
     $               B( NLF, 1 ), LDB, ZERO, BX( NLF, 1 ), LDBX )
         CALL DGEMM( 'T', 'N', NR, NRHS, NR, ONE, U( NRF, 1 ), LDU,
     $               B( NRF, 1 ), LDB, ZERO, BX( NRF, 1 ), LDBX )
   10 CONTINUE
*
*     Next copy the rows of B that correspond to unchanged rows
*     in the bidiagonal matrix to BX.
*
      DO 20 I = 1, ND
         IC = IWORK( INODE+I-1 )
         CALL DCOPY( NRHS, B( IC, 1 ), LDB, BX( IC, 1 ), LDBX )
   20 CONTINUE
*
*     Finally go through the left singular vector matrices of all
*     the other subproblems bottom-up on the tree.
*
      J = 2**NLVL
      SQRE = 0
*
      DO 40 LVL = NLVL, 1, -1
         LVL2 = 2*LVL - 1
*
*        find the first node LF and last node LL on
*        the current level LVL
*
         IF( LVL.EQ.1 ) THEN
            LF = 1
            LL = 1
         ELSE
            LF = 2**( LVL-1 )
            LL = 2*LF - 1
         END IF
         DO 30 I = LF, LL
            IM1 = I - 1
            IC = IWORK( INODE+IM1 )
            NL = IWORK( NDIML+IM1 )
            NR = IWORK( NDIMR+IM1 )
            NLF = IC - NL
            NRF = IC + 1
            J = J - 1
            CALL DLALS0( ICOMPQ, NL, NR, SQRE, NRHS, BX( NLF, 1 ), LDBX,
     $                   B( NLF, 1 ), LDB, PERM( NLF, LVL ),
     $                   GIVPTR( J ), GIVCOL( NLF, LVL2 ), LDGCOL,
     $                   GIVNUM( NLF, LVL2 ), LDU, POLES( NLF, LVL2 ),
     $                   DIFL( NLF, LVL ), DIFR( NLF, LVL2 ),
     $                   Z( NLF, LVL ), K( J ), C( J ), S( J ), WORK,
     $                   INFO )
   30    CONTINUE
   40 CONTINUE
      GO TO 90
*
*     ICOMPQ = 1: applying back the right singular vector factors.
*
   50 CONTINUE
*
*     First now go through the right singular vector matrices of all
*     the tree nodes top-down.
*
      J = 0
      DO 70 LVL = 1, NLVL
         LVL2 = 2*LVL - 1
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
         DO 60 I = LL, LF, -1
            IM1 = I - 1
            IC = IWORK( INODE+IM1 )
            NL = IWORK( NDIML+IM1 )
            NR = IWORK( NDIMR+IM1 )
            NLF = IC - NL
            NRF = IC + 1
            IF( I.EQ.LL ) THEN
               SQRE = 0
            ELSE
               SQRE = 1
            END IF
            J = J + 1
            CALL DLALS0( ICOMPQ, NL, NR, SQRE, NRHS, B( NLF, 1 ), LDB,
     $                   BX( NLF, 1 ), LDBX, PERM( NLF, LVL ),
     $                   GIVPTR( J ), GIVCOL( NLF, LVL2 ), LDGCOL,
     $                   GIVNUM( NLF, LVL2 ), LDU, POLES( NLF, LVL2 ),
     $                   DIFL( NLF, LVL ), DIFR( NLF, LVL2 ),
     $                   Z( NLF, LVL ), K( J ), C( J ), S( J ), WORK,
     $                   INFO )
   60    CONTINUE
   70 CONTINUE
*
*     The nodes on the bottom level of the tree were solved by DLASDQ.
*     The corresponding right singular vector matrices are in explicit
*     form. Apply them back.
*
      NDB1 = ( ND+1 ) / 2
      DO 80 I = NDB1, ND
         I1 = I - 1
         IC = IWORK( INODE+I1 )
         NL = IWORK( NDIML+I1 )
         NR = IWORK( NDIMR+I1 )
         NLP1 = NL + 1
         IF( I.EQ.ND ) THEN
            NRP1 = NR
         ELSE
            NRP1 = NR + 1
         END IF
         NLF = IC - NL
         NRF = IC + 1
         OPS = OPS + DOPBL3( 'DGEMM ', NLP1, NRHS, NLP1 ) 
         OPS = OPS + DOPBL3( 'DGEMM ', NRP1, NRHS, NRP1 ) 
         CALL DGEMM( 'T', 'N', NLP1, NRHS, NLP1, ONE, VT( NLF, 1 ), LDU,
     $               B( NLF, 1 ), LDB, ZERO, BX( NLF, 1 ), LDBX )
         CALL DGEMM( 'T', 'N', NRP1, NRHS, NRP1, ONE, VT( NRF, 1 ), LDU,
     $               B( NRF, 1 ), LDB, ZERO, BX( NRF, 1 ), LDBX )
   80 CONTINUE
*
   90 CONTINUE
*
      RETURN
*
*     End of DLALSA
*
      END
      SUBROUTINE DLALSD( UPLO, SMLSIZ, N, NRHS, D, E, B, LDB, RCOND,
     $                   RANK, WORK, IWORK, INFO )
*
*  -- LAPACK routine (instrumented to count ops, version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     October 31, 1999
*
*     .. Scalar Arguments ..
      CHARACTER          UPLO
      INTEGER            INFO, LDB, N, NRHS, RANK, SMLSIZ
      DOUBLE PRECISION   RCOND
*     ..
*     .. Array Arguments ..
      INTEGER            IWORK( * )
      DOUBLE PRECISION   B( LDB, * ), D( * ), E( * ), WORK( * )
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
*  DLALSD uses the singular value decomposition of A to solve the least
*  squares problem of finding X to minimize the Euclidean norm of each
*  column of A*X-B, where A is N-by-N upper bidiagonal, and X and B
*  are N-by-NRHS. The solution X overwrites B.
*
*  The singular values of A smaller than RCOND times the largest
*  singular value are treated as zero in solving the least squares
*  problem; in this case a minimum norm solution is returned.
*  The actual singular values are returned in D in ascending order.
*
*  This code makes very mild assumptions about floating point
*  arithmetic. It will work on machines with a guard digit in
*  add/subtract, or on those binary machines without guard digits
*  which subtract like the Cray XMP, Cray YMP, Cray C 90, or Cray 2.
*  It could conceivably fail on hexadecimal or decimal machines
*  without guard digits, but we know of none.
*
*  Arguments
*  =========
*
*  UPLO   (input) CHARACTER*1
*         = 'U': D and E define an upper bidiagonal matrix.
*         = 'L': D and E define a  lower bidiagonal matrix.
*
*  SMLSIZ (input) INTEGER
*         The maximum size of the subproblems at the bottom of the
*         computation tree.
*
*  N      (input) INTEGER
*         The dimension of the  bidiagonal matrix.  N >= 0.
*
*  NRHS   (input) INTEGER
*         The number of columns of B. NRHS must be at least 1.
*
*  D      (input/output) DOUBLE PRECISION array, dimension (N)
*         On entry D contains the main diagonal of the bidiagonal
*         matrix. On exit, if INFO = 0, D contains its singular values.
*
*  E      (input) DOUBLE PRECISION array, dimension (N-1)
*         Contains the super-diagonal entries of the bidiagonal matrix.
*         On exit, E has been destroyed.
*
*  B      (input/output) DOUBLE PRECISION array, dimension (LDB,NRHS)
*         On input, B contains the right hand sides of the least
*         squares problem. On output, B contains the solution X.
*
*  LDB    (input) INTEGER
*         The leading dimension of B in the calling subprogram.
*         LDB must be at least max(1,N).
*
*  RCOND  (input) DOUBLE PRECISION
*         The singular values of A less than or equal to RCOND times
*         the largest singular value are treated as zero in solving
*         the least squares problem. If RCOND is negative,
*         machine precision is used instead.
*         For example, if diag(S)*X=B were the least squares problem,
*         where diag(S) is a diagonal matrix of singular values, the
*         solution would be X(i) = B(i) / S(i) if S(i) is greater than
*         RCOND*max(S), and X(i) = 0 if S(i) is less than or equal to
*         RCOND*max(S).
*
*  RANK   (output) INTEGER
*         The number of singular values of A greater than RCOND times
*         the largest singular value.
*
*  WORK   (workspace) DOUBLE PRECISION array, dimension at least
*         (9*N + 2*N*SMLSIZ + 8*N*NLVL + N*NRHS + (SMLSIZ+1)**2),
*         where NLVL = max(0, INT(log_2 (N/(SMLSIZ+1))) + 1).
*
*  IWORK  (workspace) INTEGER array, dimension at least
*         (3*N*NLVL + 11*N)
*
*  INFO   (output) INTEGER
*         = 0:  successful exit.
*         < 0:  if INFO = -i, the i-th argument had an illegal value.
*         > 0:  The algorithm failed to compute an singular value while
*               working on the submatrix lying in rows and columns
*               INFO/(N+1) through MOD(INFO,N+1).
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE, TWO
      PARAMETER          ( ZERO = 0.0D0, ONE = 1.0D0, TWO = 2.0D0 )
*     ..
*     .. Local Scalars ..
      INTEGER            BX, BXST, C, DIFL, DIFR, GIVCOL, GIVNUM,
     $                   GIVPTR, I, ICMPQ1, ICMPQ2, IWK, J, K, NLVL,
     $                   NM1, NSIZE, NSUB, NWORK, PERM, POLES, S, SIZEI,
     $                   SMLSZP, SQRE, ST, ST1, U, VT, Z
      DOUBLE PRECISION   CS, EPS, ORGNRM, R, SN, TOL
*     ..
*     .. External Subroutines ..
      EXTERNAL           DCOPY, DGEMM, DLACPY, DLALSA, DLARTG, DLASCL,
     $                   DLASDA, DLASDQ, DLASET, DLASRT, DROT, XERBLA
*     ..
*     .. External Functions ..
      INTEGER            IDAMAX
      DOUBLE PRECISION   DLAMCH, DLANST, DOPBL3
      EXTERNAL           IDAMAX, DLAMCH, DLANST, DOPBL3
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          DBLE, ABS, INT, LOG, SIGN
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters.
*
      INFO = 0
*
      IF( N.LT.0 ) THEN
         INFO = -3
      ELSE IF( NRHS.LT.1 ) THEN
         INFO = -4
      ELSE IF( ( LDB.LT.1 ) .OR. ( LDB.LT.N ) ) THEN
         INFO = -8
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DLALSD', -INFO )
         RETURN
      END IF
*
      EPS = DLAMCH( 'Epsilon' )
*
*     Set up the tolerance.
*
      IF( ( RCOND.LE.ZERO ) .OR. ( RCOND.GE.ONE ) ) THEN
         RCOND = EPS
      END IF
*
      RANK = 0
*
*     Quick return if possible.
*
      IF( N.EQ.0 ) THEN
         RETURN
      ELSE IF( N.EQ.1 ) THEN
         IF( D( 1 ).EQ.ZERO ) THEN
            CALL DLASET( 'A', 1, NRHS, ZERO, ZERO, B, LDB )
         ELSE
            RANK = 1
            OPS = OPS + DBLE( 2*NRHS )
            CALL DLASCL( 'G', 0, 0, D( 1 ), ONE, 1, NRHS, B, LDB, INFO )
            D( 1 ) = ABS( D( 1 ) )
         END IF
         RETURN
      END IF
*
*     Rotate the matrix if it is lower bidiagonal.
*
      IF( UPLO.EQ.'L' ) THEN
         OPS = OPS + DBLE( 6*( N-1 ) )
         DO 10 I = 1, N - 1
            CALL DLARTG( D( I ), E( I ), CS, SN, R )
            D( I ) = R
            E( I ) = SN*D( I+1 )
            D( I+1 ) = CS*D( I+1 )
            IF( NRHS.EQ.1 ) THEN
               OPS = OPS + DBLE( 6 )
               CALL DROT( 1, B( I, 1 ), 1, B( I+1, 1 ), 1, CS, SN )
            ELSE
               WORK( I*2-1 ) = CS
               WORK( I*2 ) = SN
            END IF
   10    CONTINUE
         IF( NRHS.GT.1 ) THEN
            OPS = OPS + DBLE( 6*( N-1 )*NRHS )
            DO 30 I = 1, NRHS
               DO 20 J = 1, N - 1
                  CS = WORK( J*2-1 )
                  SN = WORK( J*2 )
                  CALL DROT( 1, B( J, I ), 1, B( J+1, I ), 1, CS, SN )
   20          CONTINUE
   30       CONTINUE
         END IF
      END IF
*
*     Scale.
*
      NM1 = N - 1
      ORGNRM = DLANST( 'M', N, D, E )
      IF( ORGNRM.EQ.ZERO ) THEN
         CALL DLASET( 'A', N, NRHS, ZERO, ZERO, B, LDB )
         RETURN
      END IF
*
      OPS = OPS + DBLE( N + NM1 )
      CALL DLASCL( 'G', 0, 0, ORGNRM, ONE, N, 1, D, N, INFO )
      CALL DLASCL( 'G', 0, 0, ORGNRM, ONE, NM1, 1, E, NM1, INFO )
*
*     If N is smaller than the minimum divide size SMLSIZ, then solve
*     the problem with another solver.
*
      IF( N.LE.SMLSIZ ) THEN
         NWORK = 1 + N*N
         CALL DLASET( 'A', N, N, ZERO, ONE, WORK, N )
         CALL DLASDQ( 'U', 0, N, N, 0, NRHS, D, E, WORK, N, WORK, N, B,
     $                LDB, WORK( NWORK ), INFO )
         IF( INFO.NE.0 ) THEN
            RETURN
         END IF
         OPS = OPS + DBLE( 1 )
         TOL = RCOND*ABS( D( IDAMAX( N, D, 1 ) ) )
         DO 40 I = 1, N
            IF( D( I ).LE.TOL ) THEN
               CALL DLASET( 'A', 1, NRHS, ZERO, ZERO, B( I, 1 ), LDB )
            ELSE
               OPS = OPS + DBLE( NRHS )
               CALL DLASCL( 'G', 0, 0, D( I ), ONE, 1, NRHS, B( I, 1 ),
     $                      LDB, INFO )
               RANK = RANK + 1
            END IF
   40    CONTINUE
         OPS = OPS + DOPBL3( 'DGEMM ', N, NRHS, N )
         CALL DGEMM( 'T', 'N', N, NRHS, N, ONE, WORK, N, B, LDB, ZERO,
     $               WORK( NWORK ), N )
         CALL DLACPY( 'A', N, NRHS, WORK( NWORK ), N, B, LDB )
*
*        Unscale.
*
         OPS = OPS + DBLE( N + N*NRHS )
         CALL DLASCL( 'G', 0, 0, ONE, ORGNRM, N, 1, D, N, INFO )
         CALL DLASRT( 'D', N, D, INFO )
         CALL DLASCL( 'G', 0, 0, ORGNRM, ONE, N, NRHS, B, LDB, INFO )
*
         RETURN
      END IF
*
*     Book-keeping and setting up some constants.
*
      NLVL = INT( LOG( DBLE( N ) / DBLE( SMLSIZ+1 ) ) / LOG( TWO ) ) + 1
*
      SMLSZP = SMLSIZ + 1
*
      U = 1
      VT = 1 + SMLSIZ*N
      DIFL = VT + SMLSZP*N
      DIFR = DIFL + NLVL*N
      Z = DIFR + NLVL*N*2
      C = Z + NLVL*N
      S = C + N
      POLES = S + N
      GIVNUM = POLES + 2*NLVL*N
      BX = GIVNUM + 2*NLVL*N
      NWORK = BX + N*NRHS
*
      SIZEI = 1 + N
      K = SIZEI + N
      GIVPTR = K + N
      PERM = GIVPTR + N
      GIVCOL = PERM + NLVL*N
      IWK = GIVCOL + NLVL*N*2
*
      ST = 1
      SQRE = 0
      ICMPQ1 = 1
      ICMPQ2 = 0
      NSUB = 0
*
      DO 50 I = 1, N
         IF( ABS( D( I ) ).LT.EPS ) THEN
            D( I ) = SIGN( EPS, D( I ) )
         END IF
   50 CONTINUE
*
      DO 60 I = 1, NM1
         IF( ( ABS( E( I ) ).LT.EPS ) .OR. ( I.EQ.NM1 ) ) THEN
            NSUB = NSUB + 1
            IWORK( NSUB ) = ST
*
*           Subproblem found. First determine its size and then
*           apply divide and conquer on it.
*
            IF( I.LT.NM1 ) THEN
*
*              A subproblem with E(I) small for I < NM1.
*
               NSIZE = I - ST + 1
               IWORK( SIZEI+NSUB-1 ) = NSIZE
            ELSE IF( ABS( E( I ) ).GE.EPS ) THEN
*
*              A subproblem with E(NM1) not too small but I = NM1.
*
               NSIZE = N - ST + 1
               IWORK( SIZEI+NSUB-1 ) = NSIZE
            ELSE
*
*              A subproblem with E(NM1) small. This implies an
*              1-by-1 subproblem at D(N), which is not solved
*              explicitly.
*
               NSIZE = I - ST + 1
               IWORK( SIZEI+NSUB-1 ) = NSIZE
               NSUB = NSUB + 1
               IWORK( NSUB ) = N
               IWORK( SIZEI+NSUB-1 ) = 1
               CALL DCOPY( NRHS, B( N, 1 ), LDB, WORK( BX+NM1 ), N )
            END IF
            ST1 = ST - 1
            IF( NSIZE.EQ.1 ) THEN
*
*              This is a 1-by-1 subproblem and is not solved
*              explicitly.
*
               CALL DCOPY( NRHS, B( ST, 1 ), LDB, WORK( BX+ST1 ), N )
            ELSE IF( NSIZE.LE.SMLSIZ ) THEN
*
*              This is a small subproblem and is solved by DLASDQ.
*
               CALL DLASET( 'A', NSIZE, NSIZE, ZERO, ONE,
     $                      WORK( VT+ST1 ), N )
               CALL DLASDQ( 'U', 0, NSIZE, NSIZE, 0, NRHS, D( ST ),
     $                      E( ST ), WORK( VT+ST1 ), N, WORK( NWORK ),
     $                      N, B( ST, 1 ), LDB, WORK( NWORK ), INFO )
               IF( INFO.NE.0 ) THEN
                  RETURN
               END IF
               CALL DLACPY( 'A', NSIZE, NRHS, B( ST, 1 ), LDB,
     $                      WORK( BX+ST1 ), N )
            ELSE
*
*              A large problem. Solve it using divide and conquer.
*
               CALL DLASDA( ICMPQ1, SMLSIZ, NSIZE, SQRE, D( ST ),
     $                      E( ST ), WORK( U+ST1 ), N, WORK( VT+ST1 ),
     $                      IWORK( K+ST1 ), WORK( DIFL+ST1 ),
     $                      WORK( DIFR+ST1 ), WORK( Z+ST1 ),
     $                      WORK( POLES+ST1 ), IWORK( GIVPTR+ST1 ),
     $                      IWORK( GIVCOL+ST1 ), N, IWORK( PERM+ST1 ),
     $                      WORK( GIVNUM+ST1 ), WORK( C+ST1 ),
     $                      WORK( S+ST1 ), WORK( NWORK ), IWORK( IWK ),
     $                      INFO )
               IF( INFO.NE.0 ) THEN
                  RETURN
               END IF
               BXST = BX + ST1
               CALL DLALSA( ICMPQ2, SMLSIZ, NSIZE, NRHS, B( ST, 1 ),
     $                      LDB, WORK( BXST ), N, WORK( U+ST1 ), N,
     $                      WORK( VT+ST1 ), IWORK( K+ST1 ),
     $                      WORK( DIFL+ST1 ), WORK( DIFR+ST1 ),
     $                      WORK( Z+ST1 ), WORK( POLES+ST1 ),
     $                      IWORK( GIVPTR+ST1 ), IWORK( GIVCOL+ST1 ), N,
     $                      IWORK( PERM+ST1 ), WORK( GIVNUM+ST1 ),
     $                      WORK( C+ST1 ), WORK( S+ST1 ), WORK( NWORK ),
     $                      IWORK( IWK ), INFO )
               IF( INFO.NE.0 ) THEN
                  RETURN
               END IF
            END IF
            ST = I + 1
         END IF
   60 CONTINUE
*
*     Apply the singular values and treat the tiny ones as zero.
*
      TOL = RCOND*ABS( D( IDAMAX( N, D, 1 ) ) )
*
      DO 70 I = 1, N
*
*        Some of the elements in D can be negative because 1-by-1
*        subproblems were not solved explicitly.
*
         IF( ABS( D( I ) ).LE.TOL ) THEN
            CALL DLASET( 'A', 1, NRHS, ZERO, ZERO, WORK( BX+I-1 ), N )
         ELSE
            RANK = RANK + 1
            OPS = OPS + DBLE( NRHS )
            CALL DLASCL( 'G', 0, 0, D( I ), ONE, 1, NRHS,
     $                   WORK( BX+I-1 ), N, INFO )
         END IF
         D( I ) = ABS( D( I ) )
   70 CONTINUE
*
*     Now apply back the right singular vectors.
*
      ICMPQ2 = 1
      DO 80 I = 1, NSUB
         ST = IWORK( I )
         ST1 = ST - 1
         NSIZE = IWORK( SIZEI+I-1 )
         BXST = BX + ST1
         IF( NSIZE.EQ.1 ) THEN
            CALL DCOPY( NRHS, WORK( BXST ), N, B( ST, 1 ), LDB )
         ELSE IF( NSIZE.LE.SMLSIZ ) THEN
            OPS = OPS + DOPBL3( 'DGEMM ', NSIZE, NRHS, NSIZE ) 
            CALL DGEMM( 'T', 'N', NSIZE, NRHS, NSIZE, ONE,
     $                  WORK( VT+ST1 ), N, WORK( BXST ), N, ZERO,
     $                  B( ST, 1 ), LDB )
         ELSE
            CALL DLALSA( ICMPQ2, SMLSIZ, NSIZE, NRHS, WORK( BXST ), N,
     $                   B( ST, 1 ), LDB, WORK( U+ST1 ), N,
     $                   WORK( VT+ST1 ), IWORK( K+ST1 ),
     $                   WORK( DIFL+ST1 ), WORK( DIFR+ST1 ),
     $                   WORK( Z+ST1 ), WORK( POLES+ST1 ),
     $                   IWORK( GIVPTR+ST1 ), IWORK( GIVCOL+ST1 ), N,
     $                   IWORK( PERM+ST1 ), WORK( GIVNUM+ST1 ),
     $                   WORK( C+ST1 ), WORK( S+ST1 ), WORK( NWORK ),
     $                   IWORK( IWK ), INFO )
            IF( INFO.NE.0 ) THEN
               RETURN
            END IF
         END IF
   80 CONTINUE
*
*     Unscale and sort the singular values.
*
      OPS = OPS + DBLE( N + N*NRHS )
      CALL DLASCL( 'G', 0, 0, ONE, ORGNRM, N, 1, D, N, INFO )
      CALL DLASRT( 'D', N, D, INFO )
      CALL DLASCL( 'G', 0, 0, ORGNRM, ONE, N, NRHS, B, LDB, INFO )
*
      RETURN
*
*     End of DLALSD
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
