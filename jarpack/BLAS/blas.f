      double precision function dasum(n,dx,incx)
c
c     takes the sum of the absolute values.
c     uses unrolled loops for increment equal to one.
c     jack dongarra, linpack, 3/11/78.
c     modified to correct problem with negative increment, 8/21/90.
c
      double precision dx(1),dtemp
      integer i,incx,ix,m,mp1,n
c
      dasum = 0.0d0
      dtemp = 0.0d0
      if(n.le.0)return
      if(incx.eq.1)go to 20
c
c        code for increment not equal to 1
c
      ix = 1
      if(incx.lt.0)ix = (-n+1)*incx + 1
      do 10 i = 1,n
        dtemp = dtemp + dabs(dx(ix))
        ix = ix + incx
   10 continue
      dasum = dtemp
      return
c
c        code for increment equal to 1
c
c
c        clean-up loop
c
   20 m = mod(n,6)
      if( m .eq. 0 ) go to 40
      do 30 i = 1,m
        dtemp = dtemp + dabs(dx(i))
   30 continue
      if( n .lt. 6 ) go to 60
   40 mp1 = m + 1
      do 50 i = mp1,n,6
        dtemp = dtemp + dabs(dx(i)) + dabs(dx(i + 1)) + dabs(dx(i + 2))
     *  + dabs(dx(i + 3)) + dabs(dx(i + 4)) + dabs(dx(i + 5))
   50 continue
   60 dasum = dtemp
      return
      end
      subroutine daxpy(n,da,dx,incx,dy,incy)
c
c     constant times a vector plus a vector.
c     uses unrolled loops for increments equal to one.
c     jack dongarra, linpack, 3/11/78.
c
      double precision dx(1),dy(1),da
      integer i,incx,incy,ix,iy,m,mp1,n
c
      if(n.le.0)return
      if (da .eq. 0.0d0) return
      if(incx.eq.1.and.incy.eq.1)go to 20
c
c        code for unequal increments or equal increments
c          not equal to 1
c
      ix = 1
      iy = 1
      if(incx.lt.0)ix = (-n+1)*incx + 1
      if(incy.lt.0)iy = (-n+1)*incy + 1
      do 10 i = 1,n
        dy(iy) = dy(iy) + da*dx(ix)
        ix = ix + incx
        iy = iy + incy
   10 continue
      return
c
c        code for both increments equal to 1
c
c
c        clean-up loop
c
   20 m = mod(n,4)
      if( m .eq. 0 ) go to 40
      do 30 i = 1,m
        dy(i) = dy(i) + da*dx(i)
   30 continue
      if( n .lt. 4 ) return
   40 mp1 = m + 1
      do 50 i = mp1,n,4
        dy(i) = dy(i) + da*dx(i)
        dy(i + 1) = dy(i + 1) + da*dx(i + 1)
        dy(i + 2) = dy(i + 2) + da*dx(i + 2)
        dy(i + 3) = dy(i + 3) + da*dx(i + 3)
   50 continue
      return
      end
      subroutine  dcopy(n,dx,incx,dy,incy)
c
c     copies a vector, x, to a vector, y.
c     uses unrolled loops for increments equal to one.
c     jack dongarra, linpack, 3/11/78.
c
      double precision dx(1),dy(1)
      integer i,incx,incy,ix,iy,m,mp1,n
c
      if(n.le.0)return
      if(incx.eq.1.and.incy.eq.1)go to 20
c
c        code for unequal increments or equal increments
c          not equal to 1
c
      ix = 1
      iy = 1
      if(incx.lt.0)ix = (-n+1)*incx + 1
      if(incy.lt.0)iy = (-n+1)*incy + 1
      do 10 i = 1,n
        dy(iy) = dx(ix)
        ix = ix + incx
        iy = iy + incy
   10 continue
      return
c
c        code for both increments equal to 1
c
c
c        clean-up loop
c
   20 m = mod(n,7)
      if( m .eq. 0 ) go to 40
      do 30 i = 1,m
        dy(i) = dx(i)
   30 continue
      if( n .lt. 7 ) return
   40 mp1 = m + 1
      do 50 i = mp1,n,7
        dy(i) = dx(i)
        dy(i + 1) = dx(i + 1)
        dy(i + 2) = dx(i + 2)
        dy(i + 3) = dx(i + 3)
        dy(i + 4) = dx(i + 4)
        dy(i + 5) = dx(i + 5)
        dy(i + 6) = dx(i + 6)
   50 continue
      return
      end
      double precision function ddot(n,dx,incx,dy,incy)
c
c     forms the dot product of two vectors.
c     uses unrolled loops for increments equal to one.
c     jack dongarra, linpack, 3/11/78.
c
      double precision dx(1),dy(1),dtemp
      integer i,incx,incy,ix,iy,m,mp1,n
c
      ddot = 0.0d0
      dtemp = 0.0d0
      if(n.le.0)return
      if(incx.eq.1.and.incy.eq.1)go to 20
c
c        code for unequal increments or equal increments
c          not equal to 1
c
      ix = 1
      iy = 1
      if(incx.lt.0)ix = (-n+1)*incx + 1
      if(incy.lt.0)iy = (-n+1)*incy + 1
      do 10 i = 1,n
        dtemp = dtemp + dx(ix)*dy(iy)
        ix = ix + incx
        iy = iy + incy
   10 continue
      ddot = dtemp
      return
c
c        code for both increments equal to 1
c
c
c        clean-up loop
c
   20 m = mod(n,5)
      if( m .eq. 0 ) go to 40
      do 30 i = 1,m
        dtemp = dtemp + dx(i)*dy(i)
   30 continue
      if( n .lt. 5 ) go to 60
   40 mp1 = m + 1
      do 50 i = mp1,n,5
        dtemp = dtemp + dx(i)*dy(i) + dx(i + 1)*dy(i + 1) +
     *   dx(i + 2)*dy(i + 2) + dx(i + 3)*dy(i + 3) + dx(i + 4)*dy(i + 4)
   50 continue
   60 ddot = dtemp
      return
      end
      SUBROUTINE DGBMV ( TRANS, M, N, KL, KU, ALPHA, A, LDA, X, INCX,
     $                   BETA, Y, INCY )
*     .. Scalar Arguments ..
      DOUBLE PRECISION   ALPHA, BETA
      INTEGER            INCX, INCY, KL, KU, LDA, M, N
      CHARACTER*1        TRANS
*     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), X( * ), Y( * )
*     ..
*
*  Purpose
*  =======
*
*  DGBMV  performs one of the matrix-vector operations
*
*     y := alpha*A*x + beta*y,   or   y := alpha*A'*x + beta*y,
*
*  where alpha and beta are scalars, x and y are vectors and A is an
*  m by n band matrix, with kl sub-diagonals and ku super-diagonals.
*
*  Parameters
*  ==========
*
*  TRANS  - CHARACTER*1.
*           On entry, TRANS specifies the operation to be performed as
*           follows:
*
*              TRANS = 'N' or 'n'   y := alpha*A*x + beta*y.
*
*              TRANS = 'T' or 't'   y := alpha*A'*x + beta*y.
*
*              TRANS = 'C' or 'c'   y := alpha*A'*x + beta*y.
*
*           Unchanged on exit.
*
*  M      - INTEGER.
*           On entry, M specifies the number of rows of the matrix A.
*           M must be at least zero.
*           Unchanged on exit.
*
*  N      - INTEGER.
*           On entry, N specifies the number of columns of the matrix A.
*           N must be at least zero.
*           Unchanged on exit.
*
*  KL     - INTEGER.
*           On entry, KL specifies the number of sub-diagonals of the
*           matrix A. KL must satisfy  0 .le. KL.
*           Unchanged on exit.
*
*  KU     - INTEGER.
*           On entry, KU specifies the number of super-diagonals of the
*           matrix A. KU must satisfy  0 .le. KU.
*           Unchanged on exit.
*
*  ALPHA  - DOUBLE PRECISION.
*           On entry, ALPHA specifies the scalar alpha.
*           Unchanged on exit.
*
*  A      - DOUBLE PRECISION array of DIMENSION ( LDA, n ).
*           Before entry, the leading ( kl + ku + 1 ) by n part of the
*           array A must contain the matrix of coefficients, supplied
*           column by column, with the leading diagonal of the matrix in
*           row ( ku + 1 ) of the array, the first super-diagonal
*           starting at position 2 in row ku, the first sub-diagonal
*           starting at position 1 in row ( ku + 2 ), and so on.
*           Elements in the array A that do not correspond to elements
*           in the band matrix (such as the top left ku by ku triangle)
*           are not referenced.
*           The following program segment will transfer a band matrix
*           from conventional full matrix storage to band storage:
*
*                 DO 20, J = 1, N
*                    K = KU + 1 - J
*                    DO 10, I = MAX( 1, J - KU ), MIN( M, J + KL )
*                       A( K + I, J ) = matrix( I, J )
*              10    CONTINUE
*              20 CONTINUE
*
*           Unchanged on exit.
*
*  LDA    - INTEGER.
*           On entry, LDA specifies the first dimension of A as declared
*           in the calling (sub) program. LDA must be at least
*           ( kl + ku + 1 ).
*           Unchanged on exit.
*
*  X      - DOUBLE PRECISION array of DIMENSION at least
*           ( 1 + ( n - 1 )*abs( INCX ) ) when TRANS = 'N' or 'n'
*           and at least
*           ( 1 + ( m - 1 )*abs( INCX ) ) otherwise.
*           Before entry, the incremented array X must contain the
*           vector x.
*           Unchanged on exit.
*
*  INCX   - INTEGER.
*           On entry, INCX specifies the increment for the elements of
*           X. INCX must not be zero.
*           Unchanged on exit.
*
*  BETA   - DOUBLE PRECISION.
*           On entry, BETA specifies the scalar beta. When BETA is
*           supplied as zero then Y need not be set on input.
*           Unchanged on exit.
*
*  Y      - DOUBLE PRECISION array of DIMENSION at least
*           ( 1 + ( m - 1 )*abs( INCY ) ) when TRANS = 'N' or 'n'
*           and at least
*           ( 1 + ( n - 1 )*abs( INCY ) ) otherwise.
*           Before entry, the incremented array Y must contain the
*           vector y. On exit, Y is overwritten by the updated vector y.
*
*  INCY   - INTEGER.
*           On entry, INCY specifies the increment for the elements of
*           Y. INCY must not be zero.
*           Unchanged on exit.
*
*
*  Level 2 Blas routine.
*
*  -- Written on 22-October-1986.
*     Jack Dongarra, Argonne National Lab.
*     Jeremy Du Croz, Nag Central Office.
*     Sven Hammarling, Nag Central Office.
*     Richard Hanson, Sandia National Labs.
*
*     .. Parameters ..
      DOUBLE PRECISION   ONE         , ZERO
      PARAMETER        ( ONE = 1.0D+0, ZERO = 0.0D+0 )
*     .. Local Scalars ..
      DOUBLE PRECISION   TEMP
      INTEGER            I, INFO, IX, IY, J, JX, JY, K, KUP1, KX, KY,
     $                   LENX, LENY
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
*     .. External Subroutines ..
      EXTERNAL           XERBLA
*     .. Intrinsic Functions ..
      INTRINSIC          MAX, MIN
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters.
*
      INFO = 0
      IF     ( .NOT.LSAME( TRANS, 'N' ).AND.
     $         .NOT.LSAME( TRANS, 'T' ).AND.
     $         .NOT.LSAME( TRANS, 'C' )      )THEN
         INFO = 1
      ELSE IF( M.LT.0 )THEN
         INFO = 2
      ELSE IF( N.LT.0 )THEN
         INFO = 3
      ELSE IF( KL.LT.0 )THEN
         INFO = 4
      ELSE IF( KU.LT.0 )THEN
         INFO = 5
      ELSE IF( LDA.LT.( KL + KU + 1 ) )THEN
         INFO = 8
      ELSE IF( INCX.EQ.0 )THEN
         INFO = 10
      ELSE IF( INCY.EQ.0 )THEN
         INFO = 13
      END IF
      IF( INFO.NE.0 )THEN
         CALL XERBLA( 'DGBMV ', INFO )
         RETURN
      END IF
*
*     Quick return if possible.
*
      IF( ( M.EQ.0 ).OR.( N.EQ.0 ).OR.
     $    ( ( ALPHA.EQ.ZERO ).AND.( BETA.EQ.ONE ) ) )
     $   RETURN
*
*     Set  LENX  and  LENY, the lengths of the vectors x and y, and set
*     up the start points in  X  and  Y.
*
      IF( LSAME( TRANS, 'N' ) )THEN
         LENX = N
         LENY = M
      ELSE
         LENX = M
         LENY = N
      END IF
      IF( INCX.GT.0 )THEN
         KX = 1
      ELSE
         KX = 1 - ( LENX - 1 )*INCX
      END IF
      IF( INCY.GT.0 )THEN
         KY = 1
      ELSE
         KY = 1 - ( LENY - 1 )*INCY
      END IF
*
*     Start the operations. In this version the elements of A are
*     accessed sequentially with one pass through the band part of A.
*
*     First form  y := beta*y.
*
      IF( BETA.NE.ONE )THEN
         IF( INCY.EQ.1 )THEN
            IF( BETA.EQ.ZERO )THEN
               DO 10, I = 1, LENY
                  Y( I ) = ZERO
   10          CONTINUE
            ELSE
               DO 20, I = 1, LENY
                  Y( I ) = BETA*Y( I )
   20          CONTINUE
            END IF
         ELSE
            IY = KY
            IF( BETA.EQ.ZERO )THEN
               DO 30, I = 1, LENY
                  Y( IY ) = ZERO
                  IY      = IY   + INCY
   30          CONTINUE
            ELSE
               DO 40, I = 1, LENY
                  Y( IY ) = BETA*Y( IY )
                  IY      = IY           + INCY
   40          CONTINUE
            END IF
         END IF
      END IF
      IF( ALPHA.EQ.ZERO )
     $   RETURN
      KUP1 = KU + 1
      IF( LSAME( TRANS, 'N' ) )THEN
*
*        Form  y := alpha*A*x + y.
*
         JX = KX
         IF( INCY.EQ.1 )THEN
            DO 60, J = 1, N
               IF( X( JX ).NE.ZERO )THEN
                  TEMP = ALPHA*X( JX )
                  K    = KUP1 - J
                  DO 50, I = MAX( 1, J - KU ), MIN( M, J + KL )
                     Y( I ) = Y( I ) + TEMP*A( K + I, J )
   50             CONTINUE
               END IF
               JX = JX + INCX
   60       CONTINUE
         ELSE
            DO 80, J = 1, N
               IF( X( JX ).NE.ZERO )THEN
                  TEMP = ALPHA*X( JX )
                  IY   = KY
                  K    = KUP1 - J
                  DO 70, I = MAX( 1, J - KU ), MIN( M, J + KL )
                     Y( IY ) = Y( IY ) + TEMP*A( K + I, J )
                     IY      = IY      + INCY
   70             CONTINUE
               END IF
               JX = JX + INCX
               IF( J.GT.KU )
     $            KY = KY + INCY
   80       CONTINUE
         END IF
      ELSE
*
*        Form  y := alpha*A'*x + y.
*
         JY = KY
         IF( INCX.EQ.1 )THEN
            DO 100, J = 1, N
               TEMP = ZERO
               K    = KUP1 - J
               DO 90, I = MAX( 1, J - KU ), MIN( M, J + KL )
                  TEMP = TEMP + A( K + I, J )*X( I )
   90          CONTINUE
               Y( JY ) = Y( JY ) + ALPHA*TEMP
               JY      = JY      + INCY
  100       CONTINUE
         ELSE
            DO 120, J = 1, N
               TEMP = ZERO
               IX   = KX
               K    = KUP1 - J
               DO 110, I = MAX( 1, J - KU ), MIN( M, J + KL )
                  TEMP = TEMP + A( K + I, J )*X( IX )
                  IX   = IX   + INCX
  110          CONTINUE
               Y( JY ) = Y( JY ) + ALPHA*TEMP
               JY      = JY      + INCY
               IF( J.GT.KU )
     $            KX = KX + INCX
  120       CONTINUE
         END IF
      END IF
*
      RETURN
*
*     End of DGBMV .
*
      END
      SUBROUTINE DGEMM ( TRANSA, TRANSB, M, N, K, ALPHA, A, LDA, B, LDB,
     $                   BETA, C, LDC )
*     .. Scalar Arguments ..
      CHARACTER*1        TRANSA, TRANSB
      INTEGER            M, N, K, LDA, LDB, LDC
      DOUBLE PRECISION   ALPHA, BETA
*     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), B( LDB, * ), C( LDC, * )
*     ..
*
*  Purpose
*  =======
*
*  DGEMM  performs one of the matrix-matrix operations
*
*     C := alpha*op( A )*op( B ) + beta*C,
*
*  where  op( X ) is one of
*
*     op( X ) = X   or   op( X ) = X',
*
*  alpha and beta are scalars, and A, B and C are matrices, with op( A )
*  an m by k matrix,  op( B )  a  k by n matrix and  C an m by n matrix.
*
*  Parameters
*  ==========
*
*  TRANSA - CHARACTER*1.
*           On entry, TRANSA specifies the form of op( A ) to be used in
*           the matrix multiplication as follows:
*
*              TRANSA = 'N' or 'n',  op( A ) = A.
*
*              TRANSA = 'T' or 't',  op( A ) = A'.
*
*              TRANSA = 'C' or 'c',  op( A ) = A'.
*
*           Unchanged on exit.
*
*  TRANSB - CHARACTER*1.
*           On entry, TRANSB specifies the form of op( B ) to be used in
*           the matrix multiplication as follows:
*
*              TRANSB = 'N' or 'n',  op( B ) = B.
*
*              TRANSB = 'T' or 't',  op( B ) = B'.
*
*              TRANSB = 'C' or 'c',  op( B ) = B'.
*
*           Unchanged on exit.
*
*  M      - INTEGER.
*           On entry,  M  specifies  the number  of rows  of the  matrix
*           op( A )  and of the  matrix  C.  M  must  be at least  zero.
*           Unchanged on exit.
*
*  N      - INTEGER.
*           On entry,  N  specifies the number  of columns of the matrix
*           op( B ) and the number of columns of the matrix C. N must be
*           at least zero.
*           Unchanged on exit.
*
*  K      - INTEGER.
*           On entry,  K  specifies  the number of columns of the matrix
*           op( A ) and the number of rows of the matrix op( B ). K must
*           be at least  zero.
*           Unchanged on exit.
*
*  ALPHA  - DOUBLE PRECISION.
*           On entry, ALPHA specifies the scalar alpha.
*           Unchanged on exit.
*
*  A      - DOUBLE PRECISION array of DIMENSION ( LDA, ka ), where ka is
*           k  when  TRANSA = 'N' or 'n',  and is  m  otherwise.
*           Before entry with  TRANSA = 'N' or 'n',  the leading  m by k
*           part of the array  A  must contain the matrix  A,  otherwise
*           the leading  k by m  part of the array  A  must contain  the
*           matrix A.
*           Unchanged on exit.
*
*  LDA    - INTEGER.
*           On entry, LDA specifies the first dimension of A as declared
*           in the calling (sub) program. When  TRANSA = 'N' or 'n' then
*           LDA must be at least  max( 1, m ), otherwise  LDA must be at
*           least  max( 1, k ).
*           Unchanged on exit.
*
*  B      - DOUBLE PRECISION array of DIMENSION ( LDB, kb ), where kb is
*           n  when  TRANSB = 'N' or 'n',  and is  k  otherwise.
*           Before entry with  TRANSB = 'N' or 'n',  the leading  k by n
*           part of the array  B  must contain the matrix  B,  otherwise
*           the leading  n by k  part of the array  B  must contain  the
*           matrix B.
*           Unchanged on exit.
*
*  LDB    - INTEGER.
*           On entry, LDB specifies the first dimension of B as declared
*           in the calling (sub) program. When  TRANSB = 'N' or 'n' then
*           LDB must be at least  max( 1, k ), otherwise  LDB must be at
*           least  max( 1, n ).
*           Unchanged on exit.
*
*  BETA   - DOUBLE PRECISION.
*           On entry,  BETA  specifies the scalar  beta.  When  BETA  is
*           supplied as zero then C need not be set on input.
*           Unchanged on exit.
*
*  C      - DOUBLE PRECISION array of DIMENSION ( LDC, n ).
*           Before entry, the leading  m by n  part of the array  C must
*           contain the matrix  C,  except when  beta  is zero, in which
*           case C need not be set on entry.
*           On exit, the array  C  is overwritten by the  m by n  matrix
*           ( alpha*op( A )*op( B ) + beta*C ).
*
*  LDC    - INTEGER.
*           On entry, LDC specifies the first dimension of C as declared
*           in  the  calling  (sub)  program.   LDC  must  be  at  least
*           max( 1, m ).
*           Unchanged on exit.
*
*
*  Level 3 Blas routine.
*
*  -- Written on 8-February-1989.
*     Jack Dongarra, Argonne National Laboratory.
*     Iain Duff, AERE Harwell.
*     Jeremy Du Croz, Numerical Algorithms Group Ltd.
*     Sven Hammarling, Numerical Algorithms Group Ltd.
*
*
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
*     .. External Subroutines ..
      EXTERNAL           XERBLA
*     .. Intrinsic Functions ..
      INTRINSIC          MAX
*     .. Local Scalars ..
      LOGICAL            NOTA, NOTB
      INTEGER            I, INFO, J, L, NCOLA, NROWA, NROWB
      DOUBLE PRECISION   TEMP
*     .. Parameters ..
      DOUBLE PRECISION   ONE         , ZERO
      PARAMETER        ( ONE = 1.0D+0, ZERO = 0.0D+0 )
*     ..
*     .. Executable Statements ..
*
*     Set  NOTA  and  NOTB  as  true if  A  and  B  respectively are not
*     transposed and set  NROWA, NCOLA and  NROWB  as the number of rows
*     and  columns of  A  and the  number of  rows  of  B  respectively.
*
      NOTA  = LSAME( TRANSA, 'N' )
      NOTB  = LSAME( TRANSB, 'N' )
      IF( NOTA )THEN
         NROWA = M
         NCOLA = K
      ELSE
         NROWA = K
         NCOLA = M
      END IF
      IF( NOTB )THEN
         NROWB = K
      ELSE
         NROWB = N
      END IF
*
*     Test the input parameters.
*
      INFO = 0
      IF(      ( .NOT.NOTA                 ).AND.
     $         ( .NOT.LSAME( TRANSA, 'C' ) ).AND.
     $         ( .NOT.LSAME( TRANSA, 'T' ) )      )THEN
         INFO = 1
      ELSE IF( ( .NOT.NOTB                 ).AND.
     $         ( .NOT.LSAME( TRANSB, 'C' ) ).AND.
     $         ( .NOT.LSAME( TRANSB, 'T' ) )      )THEN
         INFO = 2
      ELSE IF( M  .LT.0               )THEN
         INFO = 3
      ELSE IF( N  .LT.0               )THEN
         INFO = 4
      ELSE IF( K  .LT.0               )THEN
         INFO = 5
      ELSE IF( LDA.LT.MAX( 1, NROWA ) )THEN
         INFO = 8
      ELSE IF( LDB.LT.MAX( 1, NROWB ) )THEN
         INFO = 10
      ELSE IF( LDC.LT.MAX( 1, M     ) )THEN
         INFO = 13
      END IF
      IF( INFO.NE.0 )THEN
         CALL XERBLA( 'DGEMM ', INFO )
         RETURN
      END IF
*
*     Quick return if possible.
*
      IF( ( M.EQ.0 ).OR.( N.EQ.0 ).OR.
     $    ( ( ( ALPHA.EQ.ZERO ).OR.( K.EQ.0 ) ).AND.( BETA.EQ.ONE ) ) )
     $   RETURN
*
*     And if  alpha.eq.zero.
*
      IF( ALPHA.EQ.ZERO )THEN
         IF( BETA.EQ.ZERO )THEN
            DO 20, J = 1, N
               DO 10, I = 1, M
                  C( I, J ) = ZERO
   10          CONTINUE
   20       CONTINUE
         ELSE
            DO 40, J = 1, N
               DO 30, I = 1, M
                  C( I, J ) = BETA*C( I, J )
   30          CONTINUE
   40       CONTINUE
         END IF
         RETURN
      END IF
*
*     Start the operations.
*
      IF( NOTB )THEN
         IF( NOTA )THEN
*
*           Form  C := alpha*A*B + beta*C.
*
            DO 90, J = 1, N
               IF( BETA.EQ.ZERO )THEN
                  DO 50, I = 1, M
                     C( I, J ) = ZERO
   50             CONTINUE
               ELSE IF( BETA.NE.ONE )THEN
                  DO 60, I = 1, M
                     C( I, J ) = BETA*C( I, J )
   60             CONTINUE
               END IF
               DO 80, L = 1, K
                  IF( B( L, J ).NE.ZERO )THEN
                     TEMP = ALPHA*B( L, J )
                     DO 70, I = 1, M
                        C( I, J ) = C( I, J ) + TEMP*A( I, L )
   70                CONTINUE
                  END IF
   80          CONTINUE
   90       CONTINUE
         ELSE
*
*           Form  C := alpha*A'*B + beta*C
*
            DO 120, J = 1, N
               DO 110, I = 1, M
                  TEMP = ZERO
                  DO 100, L = 1, K
                     TEMP = TEMP + A( L, I )*B( L, J )
  100             CONTINUE
                  IF( BETA.EQ.ZERO )THEN
                     C( I, J ) = ALPHA*TEMP
                  ELSE
                     C( I, J ) = ALPHA*TEMP + BETA*C( I, J )
                  END IF
  110          CONTINUE
  120       CONTINUE
         END IF
      ELSE
         IF( NOTA )THEN
*
*           Form  C := alpha*A*B' + beta*C
*
            DO 170, J = 1, N
               IF( BETA.EQ.ZERO )THEN
                  DO 130, I = 1, M
                     C( I, J ) = ZERO
  130             CONTINUE
               ELSE IF( BETA.NE.ONE )THEN
                  DO 140, I = 1, M
                     C( I, J ) = BETA*C( I, J )
  140             CONTINUE
               END IF
               DO 160, L = 1, K
                  IF( B( J, L ).NE.ZERO )THEN
                     TEMP = ALPHA*B( J, L )
                     DO 150, I = 1, M
                        C( I, J ) = C( I, J ) + TEMP*A( I, L )
  150                CONTINUE
                  END IF
  160          CONTINUE
  170       CONTINUE
         ELSE
*
*           Form  C := alpha*A'*B' + beta*C
*
            DO 200, J = 1, N
               DO 190, I = 1, M
                  TEMP = ZERO
                  DO 180, L = 1, K
                     TEMP = TEMP + A( L, I )*B( J, L )
  180             CONTINUE
                  IF( BETA.EQ.ZERO )THEN
                     C( I, J ) = ALPHA*TEMP
                  ELSE
                     C( I, J ) = ALPHA*TEMP + BETA*C( I, J )
                  END IF
  190          CONTINUE
  200       CONTINUE
         END IF
      END IF
*
      RETURN
*
*     End of DGEMM .
*
      END
      SUBROUTINE DGEMV ( TRANS, M, N, ALPHA, A, LDA, X, INCX,
     $                   BETA, Y, INCY )
*     .. Scalar Arguments ..
      DOUBLE PRECISION   ALPHA, BETA
      INTEGER            INCX, INCY, LDA, M, N
      CHARACTER*1        TRANS
*     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), X( * ), Y( * )
*     ..
*
*  Purpose
*  =======
*
*  DGEMV  performs one of the matrix-vector operations
*
*     y := alpha*A*x + beta*y,   or   y := alpha*A'*x + beta*y,
*
*  where alpha and beta are scalars, x and y are vectors and A is an
*  m by n matrix.
*
*  Parameters
*  ==========
*
*  TRANS  - CHARACTER*1.
*           On entry, TRANS specifies the operation to be performed as
*           follows:
*
*              TRANS = 'N' or 'n'   y := alpha*A*x + beta*y.
*
*              TRANS = 'T' or 't'   y := alpha*A'*x + beta*y.
*
*              TRANS = 'C' or 'c'   y := alpha*A'*x + beta*y.
*
*           Unchanged on exit.
*
*  M      - INTEGER.
*           On entry, M specifies the number of rows of the matrix A.
*           M must be at least zero.
*           Unchanged on exit.
*
*  N      - INTEGER.
*           On entry, N specifies the number of columns of the matrix A.
*           N must be at least zero.
*           Unchanged on exit.
*
*  ALPHA  - DOUBLE PRECISION.
*           On entry, ALPHA specifies the scalar alpha.
*           Unchanged on exit.
*
*  A      - DOUBLE PRECISION array of DIMENSION ( LDA, n ).
*           Before entry, the leading m by n part of the array A must
*           contain the matrix of coefficients.
*           Unchanged on exit.
*
*  LDA    - INTEGER.
*           On entry, LDA specifies the first dimension of A as declared
*           in the calling (sub) program. LDA must be at least
*           max( 1, m ).
*           Unchanged on exit.
*
*  X      - DOUBLE PRECISION array of DIMENSION at least
*           ( 1 + ( n - 1 )*abs( INCX ) ) when TRANS = 'N' or 'n'
*           and at least
*           ( 1 + ( m - 1 )*abs( INCX ) ) otherwise.
*           Before entry, the incremented array X must contain the
*           vector x.
*           Unchanged on exit.
*
*  INCX   - INTEGER.
*           On entry, INCX specifies the increment for the elements of
*           X. INCX must not be zero.
*           Unchanged on exit.
*
*  BETA   - DOUBLE PRECISION.
*           On entry, BETA specifies the scalar beta. When BETA is
*           supplied as zero then Y need not be set on input.
*           Unchanged on exit.
*
*  Y      - DOUBLE PRECISION array of DIMENSION at least
*           ( 1 + ( m - 1 )*abs( INCY ) ) when TRANS = 'N' or 'n'
*           and at least
*           ( 1 + ( n - 1 )*abs( INCY ) ) otherwise.
*           Before entry with BETA non-zero, the incremented array Y
*           must contain the vector y. On exit, Y is overwritten by the
*           updated vector y.
*
*  INCY   - INTEGER.
*           On entry, INCY specifies the increment for the elements of
*           Y. INCY must not be zero.
*           Unchanged on exit.
*
*
*  Level 2 Blas routine.
*
*  -- Written on 22-October-1986.
*     Jack Dongarra, Argonne National Lab.
*     Jeremy Du Croz, Nag Central Office.
*     Sven Hammarling, Nag Central Office.
*     Richard Hanson, Sandia National Labs.
*
*
*     .. Parameters ..
      DOUBLE PRECISION   ONE         , ZERO
      PARAMETER        ( ONE = 1.0D+0, ZERO = 0.0D+0 )
*     .. Local Scalars ..
      DOUBLE PRECISION   TEMP
      INTEGER            I, INFO, IX, IY, J, JX, JY, KX, KY, LENX, LENY
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
*     .. External Subroutines ..
      EXTERNAL           XERBLA
*     .. Intrinsic Functions ..
      INTRINSIC          MAX
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters.
*
      INFO = 0
      IF     ( .NOT.LSAME( TRANS, 'N' ).AND.
     $         .NOT.LSAME( TRANS, 'T' ).AND.
     $         .NOT.LSAME( TRANS, 'C' )      )THEN
         INFO = 1
      ELSE IF( M.LT.0 )THEN
         INFO = 2
      ELSE IF( N.LT.0 )THEN
         INFO = 3
      ELSE IF( LDA.LT.MAX( 1, M ) )THEN
         INFO = 6
      ELSE IF( INCX.EQ.0 )THEN
         INFO = 8
      ELSE IF( INCY.EQ.0 )THEN
         INFO = 11
      END IF
      IF( INFO.NE.0 )THEN
         CALL XERBLA( 'DGEMV ', INFO )
         RETURN
      END IF
*
*     Quick return if possible.
*
      IF( ( M.EQ.0 ).OR.( N.EQ.0 ).OR.
     $    ( ( ALPHA.EQ.ZERO ).AND.( BETA.EQ.ONE ) ) )
     $   RETURN
*
*     Set  LENX  and  LENY, the lengths of the vectors x and y, and set
*     up the start points in  X  and  Y.
*
      IF( LSAME( TRANS, 'N' ) )THEN
         LENX = N
         LENY = M
      ELSE
         LENX = M
         LENY = N
      END IF
      IF( INCX.GT.0 )THEN
         KX = 1
      ELSE
         KX = 1 - ( LENX - 1 )*INCX
      END IF
      IF( INCY.GT.0 )THEN
         KY = 1
      ELSE
         KY = 1 - ( LENY - 1 )*INCY
      END IF
*
*     Start the operations. In this version the elements of A are
*     accessed sequentially with one pass through A.
*
*     First form  y := beta*y.
*
      IF( BETA.NE.ONE )THEN
         IF( INCY.EQ.1 )THEN
            IF( BETA.EQ.ZERO )THEN
               DO 10, I = 1, LENY
                  Y( I ) = ZERO
   10          CONTINUE
            ELSE
               DO 20, I = 1, LENY
                  Y( I ) = BETA*Y( I )
   20          CONTINUE
            END IF
         ELSE
            IY = KY
            IF( BETA.EQ.ZERO )THEN
               DO 30, I = 1, LENY
                  Y( IY ) = ZERO
                  IY      = IY   + INCY
   30          CONTINUE
            ELSE
               DO 40, I = 1, LENY
                  Y( IY ) = BETA*Y( IY )
                  IY      = IY           + INCY
   40          CONTINUE
            END IF
         END IF
      END IF
      IF( ALPHA.EQ.ZERO )
     $   RETURN
      IF( LSAME( TRANS, 'N' ) )THEN
*
*        Form  y := alpha*A*x + y.
*
         JX = KX
         IF( INCY.EQ.1 )THEN
            DO 60, J = 1, N
               IF( X( JX ).NE.ZERO )THEN
                  TEMP = ALPHA*X( JX )
                  DO 50, I = 1, M
                     Y( I ) = Y( I ) + TEMP*A( I, J )
   50             CONTINUE
               END IF
               JX = JX + INCX
   60       CONTINUE
         ELSE
            DO 80, J = 1, N
               IF( X( JX ).NE.ZERO )THEN
                  TEMP = ALPHA*X( JX )
                  IY   = KY
                  DO 70, I = 1, M
                     Y( IY ) = Y( IY ) + TEMP*A( I, J )
                     IY      = IY      + INCY
   70             CONTINUE
               END IF
               JX = JX + INCX
   80       CONTINUE
         END IF
      ELSE
*
*        Form  y := alpha*A'*x + y.
*
         JY = KY
         IF( INCX.EQ.1 )THEN
            DO 100, J = 1, N
               TEMP = ZERO
               DO 90, I = 1, M
                  TEMP = TEMP + A( I, J )*X( I )
   90          CONTINUE
               Y( JY ) = Y( JY ) + ALPHA*TEMP
               JY      = JY      + INCY
  100       CONTINUE
         ELSE
            DO 120, J = 1, N
               TEMP = ZERO
               IX   = KX
               DO 110, I = 1, M
                  TEMP = TEMP + A( I, J )*X( IX )
                  IX   = IX   + INCX
  110          CONTINUE
               Y( JY ) = Y( JY ) + ALPHA*TEMP
               JY      = JY      + INCY
  120       CONTINUE
         END IF
      END IF
*
      RETURN
*
*     End of DGEMV .
*
      END
      SUBROUTINE DGER  ( M, N, ALPHA, X, INCX, Y, INCY, A, LDA )
*     .. Scalar Arguments ..
      DOUBLE PRECISION   ALPHA
      INTEGER            INCX, INCY, LDA, M, N
*     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), X( * ), Y( * )
*     ..
*
*  Purpose
*  =======
*
*  DGER   performs the rank 1 operation
*
*     A := alpha*x*y' + A,
*
*  where alpha is a scalar, x is an m element vector, y is an n element
*  vector and A is an m by n matrix.
*
*  Parameters
*  ==========
*
*  M      - INTEGER.
*           On entry, M specifies the number of rows of the matrix A.
*           M must be at least zero.
*           Unchanged on exit.
*
*  N      - INTEGER.
*           On entry, N specifies the number of columns of the matrix A.
*           N must be at least zero.
*           Unchanged on exit.
*
*  ALPHA  - DOUBLE PRECISION.
*           On entry, ALPHA specifies the scalar alpha.
*           Unchanged on exit.
*
*  X      - DOUBLE PRECISION array of dimension at least
*           ( 1 + ( m - 1 )*abs( INCX ) ).
*           Before entry, the incremented array X must contain the m
*           element vector x.
*           Unchanged on exit.
*
*  INCX   - INTEGER.
*           On entry, INCX specifies the increment for the elements of
*           X. INCX must not be zero.
*           Unchanged on exit.
*
*  Y      - DOUBLE PRECISION array of dimension at least
*           ( 1 + ( n - 1 )*abs( INCY ) ).
*           Before entry, the incremented array Y must contain the n
*           element vector y.
*           Unchanged on exit.
*
*  INCY   - INTEGER.
*           On entry, INCY specifies the increment for the elements of
*           Y. INCY must not be zero.
*           Unchanged on exit.
*
*  A      - DOUBLE PRECISION array of DIMENSION ( LDA, n ).
*           Before entry, the leading m by n part of the array A must
*           contain the matrix of coefficients. On exit, A is
*           overwritten by the updated matrix.
*
*  LDA    - INTEGER.
*           On entry, LDA specifies the first dimension of A as declared
*           in the calling (sub) program. LDA must be at least
*           max( 1, m ).
*           Unchanged on exit.
*
*
*  Level 2 Blas routine.
*
*  -- Written on 22-October-1986.
*     Jack Dongarra, Argonne National Lab.
*     Jeremy Du Croz, Nag Central Office.
*     Sven Hammarling, Nag Central Office.
*     Richard Hanson, Sandia National Labs.
*
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO
      PARAMETER        ( ZERO = 0.0D+0 )
*     .. Local Scalars ..
      DOUBLE PRECISION   TEMP
      INTEGER            I, INFO, IX, J, JY, KX
*     .. External Subroutines ..
      EXTERNAL           XERBLA
*     .. Intrinsic Functions ..
      INTRINSIC          MAX
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters.
*
      INFO = 0
      IF     ( M.LT.0 )THEN
         INFO = 1
      ELSE IF( N.LT.0 )THEN
         INFO = 2
      ELSE IF( INCX.EQ.0 )THEN
         INFO = 5
      ELSE IF( INCY.EQ.0 )THEN
         INFO = 7
      ELSE IF( LDA.LT.MAX( 1, M ) )THEN
         INFO = 9
      END IF
      IF( INFO.NE.0 )THEN
         CALL XERBLA( 'DGER  ', INFO )
         RETURN
      END IF
*
*     Quick return if possible.
*
      IF( ( M.EQ.0 ).OR.( N.EQ.0 ).OR.( ALPHA.EQ.ZERO ) )
     $   RETURN
*
*     Start the operations. In this version the elements of A are
*     accessed sequentially with one pass through A.
*
      IF( INCY.GT.0 )THEN
         JY = 1
      ELSE
         JY = 1 - ( N - 1 )*INCY
      END IF
      IF( INCX.EQ.1 )THEN
         DO 20, J = 1, N
            IF( Y( JY ).NE.ZERO )THEN
               TEMP = ALPHA*Y( JY )
               DO 10, I = 1, M
                  A( I, J ) = A( I, J ) + X( I )*TEMP
   10          CONTINUE
            END IF
            JY = JY + INCY
   20    CONTINUE
      ELSE
         IF( INCX.GT.0 )THEN
            KX = 1
         ELSE
            KX = 1 - ( M - 1 )*INCX
         END IF
         DO 40, J = 1, N
            IF( Y( JY ).NE.ZERO )THEN
               TEMP = ALPHA*Y( JY )
               IX   = KX
               DO 30, I = 1, M
                  A( I, J ) = A( I, J ) + X( IX )*TEMP
                  IX        = IX        + INCX
   30          CONTINUE
            END IF
            JY = JY + INCY
   40    CONTINUE
      END IF
*
      RETURN
*
*     End of DGER  .
*
      END
      double precision function dnrm2 ( n, dx, incx)
      integer i, incx, ix, j, n, next
      double precision   dx(1), cutlo, cuthi, hitest, sum, xmax,zero,one
      data   zero, one /0.0d0, 1.0d0/
c
c     euclidean norm of the n-vector stored in dx() with storage
c     increment incx .
c     if    n .le. 0 return with result = 0.
c     if n .ge. 1 then incx must be .ge. 1
c
c           c.l.lawson, 1978 jan 08
c     modified to correct problem with negative increment, 8/21/90.
c
c     four phase method     using two built-in constants that are
c     hopefully applicable to all machines.
c         cutlo = maximum of  dsqrt(u/eps)  over all known machines.
c         cuthi = minimum of  dsqrt(v)      over all known machines.
c     where
c         eps = smallest no. such that eps + 1. .gt. 1.
c         u   = smallest positive no.   (underflow limit)
c         v   = largest  no.            (overflow  limit)
c
c     brief outline of algorithm..
c
c     phase 1    scans zero components.
c     move to phase 2 when a component is nonzero and .le. cutlo
c     move to phase 3 when a component is .gt. cutlo
c     move to phase 4 when a component is .ge. cuthi/m
c     where m = n for x() real and m = 2*n for complex.
c
c     values for cutlo and cuthi..
c     from the environmental parameters listed in the imsl converter
c     document the limiting values are as follows..
c     cutlo, s.p.   u/eps = 2**(-102) for  honeywell.  close seconds are
c                   univac and dec at 2**(-103)
c                   thus cutlo = 2**(-51) = 4.44089e-16
c     cuthi, s.p.   v = 2**127 for univac, honeywell, and dec.
c                   thus cuthi = 2**(63.5) = 1.30438e19
c     cutlo, d.p.   u/eps = 2**(-67) for honeywell and dec.
c                   thus cutlo = 2**(-33.5) = 8.23181d-11
c     cuthi, d.p.   same as s.p.  cuthi = 1.30438d19
c     data cutlo, cuthi / 8.232d-11,  1.304d19 /
c     data cutlo, cuthi / 4.441e-16,  1.304e19 /
c     data cutlo, cuthi / 8.232d-11,  1.304d19 /
c.... from Ed Anderson
      data cutlo /   0.1415686533102923D-145 /
      data cuthi /   0.1340780792994260D+155 /
c
      if(n .gt. 0) go to 10
         dnrm2  = zero
         go to 300
c
   10 assign 30 to next
      sum = zero
      i = 1
      if( incx .lt. 0 )i = (-n+1)*incx + 1
      ix = 1
c                                                 begin main loop
   20    go to next,(30, 50, 70, 110)
   30 if( dabs(dx(i)) .gt. cutlo) go to 85
      assign 50 to next
      xmax = zero
c
c                        phase 1.  sum is zero
c
   50 if( dx(i) .eq. zero) go to 200
      if( dabs(dx(i)) .gt. cutlo) go to 85
c
c                                prepare for phase 2.
      assign 70 to next
      go to 105
c
c                                prepare for phase 4.
c
  100 continue
      assign 110 to next
      sum = (sum / dx(i)) / dx(i)
  105 xmax = dabs(dx(i))
      go to 115
c
c                   phase 2.  sum is small.
c                             scale to avoid destructive underflow.
c
   70 if( dabs(dx(i)) .gt. cutlo ) go to 75
c
c                     common code for phases 2 and 4.
c                     in phase 4 sum is large.  scale to avoid overflow.
c
  110 if( dabs(dx(i)) .le. xmax ) go to 115
         sum = one + sum * (xmax / dx(i))**2
         xmax = dabs(dx(i))
         go to 200
c
  115 sum = sum + (dx(i)/xmax)**2
      go to 200
c
c
c                  prepare for phase 3.
c
   75 sum = (sum * xmax) * xmax
c
c
c     for real or d.p. set hitest = cuthi/n
c     for complex      set hitest = cuthi/(2*n)
c
   85 hitest = cuthi/float( n )
c
c                   phase 3.  sum is mid-range.  no scaling.
c
      do 95 j = ix,n
      if(dabs(dx(i)) .ge. hitest) go to 100
         sum = sum + dx(i)**2
         i = i + incx
   95 continue
      dnrm2 = dsqrt( sum )
      go to 300
c
  200 continue
      ix = ix + 1
      i = i + incx
      if( ix .le. n ) go to 20
c
c              end of main loop.
c
c              compute square root and adjust for scaling.
c
      dnrm2 = xmax * dsqrt(sum)
  300 continue
      return
      end
      subroutine  drot (n,dx,incx,dy,incy,c,s)
c
c     applies a plane rotation.
c     jack dongarra, linpack, 3/11/78.
c
      double precision dx(1),dy(1),dtemp,c,s
      integer i,incx,incy,ix,iy,n
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
        dtemp = c*dx(ix) + s*dy(iy)
        dy(iy) = c*dy(iy) - s*dx(ix)
        dx(ix) = dtemp
        ix = ix + incx
        iy = iy + incy
   10 continue
      return
c
c       code for both increments equal to 1
c
   20 do 30 i = 1,n
        dtemp = c*dx(i) + s*dy(i)
        dy(i) = c*dy(i) - s*dx(i)
        dx(i) = dtemp
   30 continue
      return
      end
      subroutine drotg(da,db,c,s)
c
c     construct givens plane rotation.
c     jack dongarra, linpack, 3/11/78.
c
      double precision da,db,c,s,roe,scale,r,z
c
      roe = db
      if( dabs(da) .gt. dabs(db) ) roe = da
      scale = dabs(da) + dabs(db)
      if( scale .ne. 0.0d0 ) go to 10
         c = 1.0d0
         s = 0.0d0
         r = 0.0d0
         z = 0.0d0
         go to 20
   10 r = scale*dsqrt((da/scale)**2 + (db/scale)**2)
      r = dsign(1.0d0,roe)*r
      c = da/r
      s = db/r
      z = 1.0d0
      if( dabs(da) .gt. dabs(db) ) z = s
      if( dabs(db) .ge. dabs(da) .and. c .ne. 0.0d0 ) z = 1.0d0/c
   20 da = r
      db = z
      return
      end
      subroutine  dscal(n,da,dx,incx)
c
c     scales a vector by a constant.
c     uses unrolled loops for increment equal to one.
c     jack dongarra, linpack, 3/11/78.
c     modified to correct problem with negative increment, 8/21/90.
c
      double precision da,dx(1)
      integer i,incx,ix,m,mp1,n
c
      if(n.le.0)return
      if(incx.eq.1)go to 20
c
c        code for increment not equal to 1
c
      ix = 1
      if(incx.lt.0)ix = (-n+1)*incx + 1
      do 10 i = 1,n
        dx(ix) = da*dx(ix)
        ix = ix + incx
   10 continue
      return
c
c        code for increment equal to 1
c
c
c        clean-up loop
c
   20 m = mod(n,5)
      if( m .eq. 0 ) go to 40
      do 30 i = 1,m
        dx(i) = da*dx(i)
   30 continue
      if( n .lt. 5 ) return
   40 mp1 = m + 1
      do 50 i = mp1,n,5
        dx(i) = da*dx(i)
        dx(i + 1) = da*dx(i + 1)
        dx(i + 2) = da*dx(i + 2)
        dx(i + 3) = da*dx(i + 3)
        dx(i + 4) = da*dx(i + 4)
   50 continue
      return
      end
      subroutine  dswap (n,dx,incx,dy,incy)
c
c     interchanges two vectors.
c     uses unrolled loops for increments equal one.
c     jack dongarra, linpack, 3/11/78.
c
      double precision dx(1),dy(1),dtemp
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
        dtemp = dx(ix)
        dx(ix) = dy(iy)
        dy(iy) = dtemp
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
        dtemp = dx(i)
        dx(i) = dy(i)
        dy(i) = dtemp
   30 continue
      if( n .lt. 3 ) return
   40 mp1 = m + 1
      do 50 i = mp1,n,3
        dtemp = dx(i)
        dx(i) = dy(i)
        dy(i) = dtemp
        dtemp = dx(i + 1)
        dx(i + 1) = dy(i + 1)
        dy(i + 1) = dtemp
        dtemp = dx(i + 2)
        dx(i + 2) = dy(i + 2)
        dy(i + 2) = dtemp
   50 continue
      return
      end
      SUBROUTINE DSYMV ( UPLO, N, ALPHA, A, LDA, X, INCX,
     $                   BETA, Y, INCY )
*     .. Scalar Arguments ..
      DOUBLE PRECISION   ALPHA, BETA
      INTEGER            INCX, INCY, LDA, N
      CHARACTER*1        UPLO
*     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), X( * ), Y( * )
*     ..
*
*  Purpose
*  =======
*
*  DSYMV  performs the matrix-vector  operation
*
*     y := alpha*A*x + beta*y,
*
*  where alpha and beta are scalars, x and y are n element vectors and
*  A is an n by n symmetric matrix.
*
*  Parameters
*  ==========
*
*  UPLO   - CHARACTER*1.
*           On entry, UPLO specifies whether the upper or lower
*           triangular part of the array A is to be referenced as
*           follows:
*
*              UPLO = 'U' or 'u'   Only the upper triangular part of A
*                                  is to be referenced.
*
*              UPLO = 'L' or 'l'   Only the lower triangular part of A
*                                  is to be referenced.
*
*           Unchanged on exit.
*
*  N      - INTEGER.
*           On entry, N specifies the order of the matrix A.
*           N must be at least zero.
*           Unchanged on exit.
*
*  ALPHA  - DOUBLE PRECISION.
*           On entry, ALPHA specifies the scalar alpha.
*           Unchanged on exit.
*
*  A      - DOUBLE PRECISION array of DIMENSION ( LDA, n ).
*           Before entry with  UPLO = 'U' or 'u', the leading n by n
*           upper triangular part of the array A must contain the upper
*           triangular part of the symmetric matrix and the strictly
*           lower triangular part of A is not referenced.
*           Before entry with UPLO = 'L' or 'l', the leading n by n
*           lower triangular part of the array A must contain the lower
*           triangular part of the symmetric matrix and the strictly
*           upper triangular part of A is not referenced.
*           Unchanged on exit.
*
*  LDA    - INTEGER.
*           On entry, LDA specifies the first dimension of A as declared
*           in the calling (sub) program. LDA must be at least
*           max( 1, n ).
*           Unchanged on exit.
*
*  X      - DOUBLE PRECISION array of dimension at least
*           ( 1 + ( n - 1 )*abs( INCX ) ).
*           Before entry, the incremented array X must contain the n
*           element vector x.
*           Unchanged on exit.
*
*  INCX   - INTEGER.
*           On entry, INCX specifies the increment for the elements of
*           X. INCX must not be zero.
*           Unchanged on exit.
*
*  BETA   - DOUBLE PRECISION.
*           On entry, BETA specifies the scalar beta. When BETA is
*           supplied as zero then Y need not be set on input.
*           Unchanged on exit.
*
*  Y      - DOUBLE PRECISION array of dimension at least
*           ( 1 + ( n - 1 )*abs( INCY ) ).
*           Before entry, the incremented array Y must contain the n
*           element vector y. On exit, Y is overwritten by the updated
*           vector y.
*
*  INCY   - INTEGER.
*           On entry, INCY specifies the increment for the elements of
*           Y. INCY must not be zero.
*           Unchanged on exit.
*
*
*  Level 2 Blas routine.
*
*  -- Written on 22-October-1986.
*     Jack Dongarra, Argonne National Lab.
*     Jeremy Du Croz, Nag Central Office.
*     Sven Hammarling, Nag Central Office.
*     Richard Hanson, Sandia National Labs.
*
*
*     .. Parameters ..
      DOUBLE PRECISION   ONE         , ZERO
      PARAMETER        ( ONE = 1.0D+0, ZERO = 0.0D+0 )
*     .. Local Scalars ..
      DOUBLE PRECISION   TEMP1, TEMP2
      INTEGER            I, INFO, IX, IY, J, JX, JY, KX, KY
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
*     .. External Subroutines ..
      EXTERNAL           XERBLA
*     .. Intrinsic Functions ..
      INTRINSIC          MAX
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters.
*
      INFO = 0
      IF     ( .NOT.LSAME( UPLO, 'U' ).AND.
     $         .NOT.LSAME( UPLO, 'L' )      )THEN
         INFO = 1
      ELSE IF( N.LT.0 )THEN
         INFO = 2
      ELSE IF( LDA.LT.MAX( 1, N ) )THEN
         INFO = 5
      ELSE IF( INCX.EQ.0 )THEN
         INFO = 7
      ELSE IF( INCY.EQ.0 )THEN
         INFO = 10
      END IF
      IF( INFO.NE.0 )THEN
         CALL XERBLA( 'DSYMV ', INFO )
         RETURN
      END IF
*
*     Quick return if possible.
*
      IF( ( N.EQ.0 ).OR.( ( ALPHA.EQ.ZERO ).AND.( BETA.EQ.ONE ) ) )
     $   RETURN
*
*     Set up the start points in  X  and  Y.
*
      IF( INCX.GT.0 )THEN
         KX = 1
      ELSE
         KX = 1 - ( N - 1 )*INCX
      END IF
      IF( INCY.GT.0 )THEN
         KY = 1
      ELSE
         KY = 1 - ( N - 1 )*INCY
      END IF
*
*     Start the operations. In this version the elements of A are
*     accessed sequentially with one pass through the triangular part
*     of A.
*
*     First form  y := beta*y.
*
      IF( BETA.NE.ONE )THEN
         IF( INCY.EQ.1 )THEN
            IF( BETA.EQ.ZERO )THEN
               DO 10, I = 1, N
                  Y( I ) = ZERO
   10          CONTINUE
            ELSE
               DO 20, I = 1, N
                  Y( I ) = BETA*Y( I )
   20          CONTINUE
            END IF
         ELSE
            IY = KY
            IF( BETA.EQ.ZERO )THEN
               DO 30, I = 1, N
                  Y( IY ) = ZERO
                  IY      = IY   + INCY
   30          CONTINUE
            ELSE
               DO 40, I = 1, N
                  Y( IY ) = BETA*Y( IY )
                  IY      = IY           + INCY
   40          CONTINUE
            END IF
         END IF
      END IF
      IF( ALPHA.EQ.ZERO )
     $   RETURN
      IF( LSAME( UPLO, 'U' ) )THEN
*
*        Form  y  when A is stored in upper triangle.
*
         IF( ( INCX.EQ.1 ).AND.( INCY.EQ.1 ) )THEN
            DO 60, J = 1, N
               TEMP1 = ALPHA*X( J )
               TEMP2 = ZERO
               DO 50, I = 1, J - 1
                  Y( I ) = Y( I ) + TEMP1*A( I, J )
                  TEMP2  = TEMP2  + A( I, J )*X( I )
   50          CONTINUE
               Y( J ) = Y( J ) + TEMP1*A( J, J ) + ALPHA*TEMP2
   60       CONTINUE
         ELSE
            JX = KX
            JY = KY
            DO 80, J = 1, N
               TEMP1 = ALPHA*X( JX )
               TEMP2 = ZERO
               IX    = KX
               IY    = KY
               DO 70, I = 1, J - 1
                  Y( IY ) = Y( IY ) + TEMP1*A( I, J )
                  TEMP2   = TEMP2   + A( I, J )*X( IX )
                  IX      = IX      + INCX
                  IY      = IY      + INCY
   70          CONTINUE
               Y( JY ) = Y( JY ) + TEMP1*A( J, J ) + ALPHA*TEMP2
               JX      = JX      + INCX
               JY      = JY      + INCY
   80       CONTINUE
         END IF
      ELSE
*
*        Form  y  when A is stored in lower triangle.
*
         IF( ( INCX.EQ.1 ).AND.( INCY.EQ.1 ) )THEN
            DO 100, J = 1, N
               TEMP1  = ALPHA*X( J )
               TEMP2  = ZERO
               Y( J ) = Y( J )       + TEMP1*A( J, J )
               DO 90, I = J + 1, N
                  Y( I ) = Y( I ) + TEMP1*A( I, J )
                  TEMP2  = TEMP2  + A( I, J )*X( I )
   90          CONTINUE
               Y( J ) = Y( J ) + ALPHA*TEMP2
  100       CONTINUE
         ELSE
            JX = KX
            JY = KY
            DO 120, J = 1, N
               TEMP1   = ALPHA*X( JX )
               TEMP2   = ZERO
               Y( JY ) = Y( JY )       + TEMP1*A( J, J )
               IX      = JX
               IY      = JY
               DO 110, I = J + 1, N
                  IX      = IX      + INCX
                  IY      = IY      + INCY
                  Y( IY ) = Y( IY ) + TEMP1*A( I, J )
                  TEMP2   = TEMP2   + A( I, J )*X( IX )
  110          CONTINUE
               Y( JY ) = Y( JY ) + ALPHA*TEMP2
               JX      = JX      + INCX
               JY      = JY      + INCY
  120       CONTINUE
         END IF
      END IF
*
      RETURN
*
*     End of DSYMV .
*
      END
      SUBROUTINE DSYR2 ( UPLO, N, ALPHA, X, INCX, Y, INCY, A, LDA )
*     .. Scalar Arguments ..
      DOUBLE PRECISION   ALPHA
      INTEGER            INCX, INCY, LDA, N
      CHARACTER*1        UPLO
*     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), X( * ), Y( * )
*     ..
*
*  Purpose
*  =======
*
*  DSYR2  performs the symmetric rank 2 operation
*
*     A := alpha*x*y' + alpha*y*x' + A,
*
*  where alpha is a scalar, x and y are n element vectors and A is an n
*  by n symmetric matrix.
*
*  Parameters
*  ==========
*
*  UPLO   - CHARACTER*1.
*           On entry, UPLO specifies whether the upper or lower
*           triangular part of the array A is to be referenced as
*           follows:
*
*              UPLO = 'U' or 'u'   Only the upper triangular part of A
*                                  is to be referenced.
*
*              UPLO = 'L' or 'l'   Only the lower triangular part of A
*                                  is to be referenced.
*
*           Unchanged on exit.
*
*  N      - INTEGER.
*           On entry, N specifies the order of the matrix A.
*           N must be at least zero.
*           Unchanged on exit.
*
*  ALPHA  - DOUBLE PRECISION.
*           On entry, ALPHA specifies the scalar alpha.
*           Unchanged on exit.
*
*  X      - DOUBLE PRECISION array of dimension at least
*           ( 1 + ( n - 1 )*abs( INCX ) ).
*           Before entry, the incremented array X must contain the n
*           element vector x.
*           Unchanged on exit.
*
*  INCX   - INTEGER.
*           On entry, INCX specifies the increment for the elements of
*           X. INCX must not be zero.
*           Unchanged on exit.
*
*  Y      - DOUBLE PRECISION array of dimension at least
*           ( 1 + ( n - 1 )*abs( INCY ) ).
*           Before entry, the incremented array Y must contain the n
*           element vector y.
*           Unchanged on exit.
*
*  INCY   - INTEGER.
*           On entry, INCY specifies the increment for the elements of
*           Y. INCY must not be zero.
*           Unchanged on exit.
*
*  A      - DOUBLE PRECISION array of DIMENSION ( LDA, n ).
*           Before entry with  UPLO = 'U' or 'u', the leading n by n
*           upper triangular part of the array A must contain the upper
*           triangular part of the symmetric matrix and the strictly
*           lower triangular part of A is not referenced. On exit, the
*           upper triangular part of the array A is overwritten by the
*           upper triangular part of the updated matrix.
*           Before entry with UPLO = 'L' or 'l', the leading n by n
*           lower triangular part of the array A must contain the lower
*           triangular part of the symmetric matrix and the strictly
*           upper triangular part of A is not referenced. On exit, the
*           lower triangular part of the array A is overwritten by the
*           lower triangular part of the updated matrix.
*
*  LDA    - INTEGER.
*           On entry, LDA specifies the first dimension of A as declared
*           in the calling (sub) program. LDA must be at least
*           max( 1, n ).
*           Unchanged on exit.
*
*
*  Level 2 Blas routine.
*
*  -- Written on 22-October-1986.
*     Jack Dongarra, Argonne National Lab.
*     Jeremy Du Croz, Nag Central Office.
*     Sven Hammarling, Nag Central Office.
*     Richard Hanson, Sandia National Labs.
*
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO
      PARAMETER        ( ZERO = 0.0D+0 )
*     .. Local Scalars ..
      DOUBLE PRECISION   TEMP1, TEMP2
      INTEGER            I, INFO, IX, IY, J, JX, JY, KX, KY
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
*     .. External Subroutines ..
      EXTERNAL           XERBLA
*     .. Intrinsic Functions ..
      INTRINSIC          MAX
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters.
*
      INFO = 0
      IF     ( .NOT.LSAME( UPLO, 'U' ).AND.
     $         .NOT.LSAME( UPLO, 'L' )      )THEN
         INFO = 1
      ELSE IF( N.LT.0 )THEN
         INFO = 2
      ELSE IF( INCX.EQ.0 )THEN
         INFO = 5
      ELSE IF( INCY.EQ.0 )THEN
         INFO = 7
      ELSE IF( LDA.LT.MAX( 1, N ) )THEN
         INFO = 9
      END IF
      IF( INFO.NE.0 )THEN
         CALL XERBLA( 'DSYR2 ', INFO )
         RETURN
      END IF
*
*     Quick return if possible.
*
      IF( ( N.EQ.0 ).OR.( ALPHA.EQ.ZERO ) )
     $   RETURN
*
*     Set up the start points in X and Y if the increments are not both
*     unity.
*
      IF( ( INCX.NE.1 ).OR.( INCY.NE.1 ) )THEN
         IF( INCX.GT.0 )THEN
            KX = 1
         ELSE
            KX = 1 - ( N - 1 )*INCX
         END IF
         IF( INCY.GT.0 )THEN
            KY = 1
         ELSE
            KY = 1 - ( N - 1 )*INCY
         END IF
         JX = KX
         JY = KY
      END IF
*
*     Start the operations. In this version the elements of A are
*     accessed sequentially with one pass through the triangular part
*     of A.
*
      IF( LSAME( UPLO, 'U' ) )THEN
*
*        Form  A  when A is stored in the upper triangle.
*
         IF( ( INCX.EQ.1 ).AND.( INCY.EQ.1 ) )THEN
            DO 20, J = 1, N
               IF( ( X( J ).NE.ZERO ).OR.( Y( J ).NE.ZERO ) )THEN
                  TEMP1 = ALPHA*Y( J )
                  TEMP2 = ALPHA*X( J )
                  DO 10, I = 1, J
                     A( I, J ) = A( I, J ) + X( I )*TEMP1 + Y( I )*TEMP2
   10             CONTINUE
               END IF
   20       CONTINUE
         ELSE
            DO 40, J = 1, N
               IF( ( X( JX ).NE.ZERO ).OR.( Y( JY ).NE.ZERO ) )THEN
                  TEMP1 = ALPHA*Y( JY )
                  TEMP2 = ALPHA*X( JX )
                  IX    = KX
                  IY    = KY
                  DO 30, I = 1, J
                     A( I, J ) = A( I, J ) + X( IX )*TEMP1
     $                                     + Y( IY )*TEMP2
                     IX        = IX        + INCX
                     IY        = IY        + INCY
   30             CONTINUE
               END IF
               JX = JX + INCX
               JY = JY + INCY
   40       CONTINUE
         END IF
      ELSE
*
*        Form  A  when A is stored in the lower triangle.
*
         IF( ( INCX.EQ.1 ).AND.( INCY.EQ.1 ) )THEN
            DO 60, J = 1, N
               IF( ( X( J ).NE.ZERO ).OR.( Y( J ).NE.ZERO ) )THEN
                  TEMP1 = ALPHA*Y( J )
                  TEMP2 = ALPHA*X( J )
                  DO 50, I = J, N
                     A( I, J ) = A( I, J ) + X( I )*TEMP1 + Y( I )*TEMP2
   50             CONTINUE
               END IF
   60       CONTINUE
         ELSE
            DO 80, J = 1, N
               IF( ( X( JX ).NE.ZERO ).OR.( Y( JY ).NE.ZERO ) )THEN
                  TEMP1 = ALPHA*Y( JY )
                  TEMP2 = ALPHA*X( JX )
                  IX    = JX
                  IY    = JY
                  DO 70, I = J, N
                     A( I, J ) = A( I, J ) + X( IX )*TEMP1
     $                                     + Y( IY )*TEMP2
                     IX        = IX        + INCX
                     IY        = IY        + INCY
   70             CONTINUE
               END IF
               JX = JX + INCX
               JY = JY + INCY
   80       CONTINUE
         END IF
      END IF
*
      RETURN
*
*     End of DSYR2 .
*
      END
      SUBROUTINE DTBSV ( UPLO, TRANS, DIAG, N, K, A, LDA, X, INCX )
*     .. Scalar Arguments ..
      INTEGER            INCX, K, LDA, N
      CHARACTER*1        DIAG, TRANS, UPLO
*     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), X( * )
*     ..
*
*  Purpose
*  =======
*
*  DTBSV  solves one of the systems of equations
*
*     A*x = b,   or   A'*x = b,
*
*  where b and x are n element vectors and A is an n by n unit, or
*  non-unit, upper or lower triangular band matrix, with ( k + 1 )
*  diagonals.
*
*  No test for singularity or near-singularity is included in this
*  routine. Such tests must be performed before calling this routine.
*
*  Parameters
*  ==========
*
*  UPLO   - CHARACTER*1.
*           On entry, UPLO specifies whether the matrix is an upper or
*           lower triangular matrix as follows:
*
*              UPLO = 'U' or 'u'   A is an upper triangular matrix.
*
*              UPLO = 'L' or 'l'   A is a lower triangular matrix.
*
*           Unchanged on exit.
*
*  TRANS  - CHARACTER*1.
*           On entry, TRANS specifies the equations to be solved as
*           follows:
*
*              TRANS = 'N' or 'n'   A*x = b.
*
*              TRANS = 'T' or 't'   A'*x = b.
*
*              TRANS = 'C' or 'c'   A'*x = b.
*
*           Unchanged on exit.
*
*  DIAG   - CHARACTER*1.
*           On entry, DIAG specifies whether or not A is unit
*           triangular as follows:
*
*              DIAG = 'U' or 'u'   A is assumed to be unit triangular.
*
*              DIAG = 'N' or 'n'   A is not assumed to be unit
*                                  triangular.
*
*           Unchanged on exit.
*
*  N      - INTEGER.
*           On entry, N specifies the order of the matrix A.
*           N must be at least zero.
*           Unchanged on exit.
*
*  K      - INTEGER.
*           On entry with UPLO = 'U' or 'u', K specifies the number of
*           super-diagonals of the matrix A.
*           On entry with UPLO = 'L' or 'l', K specifies the number of
*           sub-diagonals of the matrix A.
*           K must satisfy  0 .le. K.
*           Unchanged on exit.
*
*  A      - DOUBLE PRECISION array of DIMENSION ( LDA, n ).
*           Before entry with UPLO = 'U' or 'u', the leading ( k + 1 )
*           by n part of the array A must contain the upper triangular
*           band part of the matrix of coefficients, supplied column by
*           column, with the leading diagonal of the matrix in row
*           ( k + 1 ) of the array, the first super-diagonal starting at
*           position 2 in row k, and so on. The top left k by k triangle
*           of the array A is not referenced.
*           The following program segment will transfer an upper
*           triangular band matrix from conventional full matrix storage
*           to band storage:
*
*                 DO 20, J = 1, N
*                    M = K + 1 - J
*                    DO 10, I = MAX( 1, J - K ), J
*                       A( M + I, J ) = matrix( I, J )
*              10    CONTINUE
*              20 CONTINUE
*
*           Before entry with UPLO = 'L' or 'l', the leading ( k + 1 )
*           by n part of the array A must contain the lower triangular
*           band part of the matrix of coefficients, supplied column by
*           column, with the leading diagonal of the matrix in row 1 of
*           the array, the first sub-diagonal starting at position 1 in
*           row 2, and so on. The bottom right k by k triangle of the
*           array A is not referenced.
*           The following program segment will transfer a lower
*           triangular band matrix from conventional full matrix storage
*           to band storage:
*
*                 DO 20, J = 1, N
*                    M = 1 - J
*                    DO 10, I = J, MIN( N, J + K )
*                       A( M + I, J ) = matrix( I, J )
*              10    CONTINUE
*              20 CONTINUE
*
*           Note that when DIAG = 'U' or 'u' the elements of the array A
*           corresponding to the diagonal elements of the matrix are not
*           referenced, but are assumed to be unity.
*           Unchanged on exit.
*
*  LDA    - INTEGER.
*           On entry, LDA specifies the first dimension of A as declared
*           in the calling (sub) program. LDA must be at least
*           ( k + 1 ).
*           Unchanged on exit.
*
*  X      - DOUBLE PRECISION array of dimension at least
*           ( 1 + ( n - 1 )*abs( INCX ) ).
*           Before entry, the incremented array X must contain the n
*           element right-hand side vector b. On exit, X is overwritten
*           with the solution vector x.
*
*  INCX   - INTEGER.
*           On entry, INCX specifies the increment for the elements of
*           X. INCX must not be zero.
*           Unchanged on exit.
*
*
*  Level 2 Blas routine.
*
*  -- Written on 22-October-1986.
*     Jack Dongarra, Argonne National Lab.
*     Jeremy Du Croz, Nag Central Office.
*     Sven Hammarling, Nag Central Office.
*     Richard Hanson, Sandia National Labs.
*
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO
      PARAMETER        ( ZERO = 0.0D+0 )
*     .. Local Scalars ..
      DOUBLE PRECISION   TEMP
      INTEGER            I, INFO, IX, J, JX, KPLUS1, KX, L
      LOGICAL            NOUNIT
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
*     .. External Subroutines ..
      EXTERNAL           XERBLA
*     .. Intrinsic Functions ..
      INTRINSIC          MAX, MIN
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters.
*
      INFO = 0
      IF     ( .NOT.LSAME( UPLO , 'U' ).AND.
     $         .NOT.LSAME( UPLO , 'L' )      )THEN
         INFO = 1
      ELSE IF( .NOT.LSAME( TRANS, 'N' ).AND.
     $         .NOT.LSAME( TRANS, 'T' ).AND.
     $         .NOT.LSAME( TRANS, 'C' )      )THEN
         INFO = 2
      ELSE IF( .NOT.LSAME( DIAG , 'U' ).AND.
     $         .NOT.LSAME( DIAG , 'N' )      )THEN
         INFO = 3
      ELSE IF( N.LT.0 )THEN
         INFO = 4
      ELSE IF( K.LT.0 )THEN
         INFO = 5
      ELSE IF( LDA.LT.( K + 1 ) )THEN
         INFO = 7
      ELSE IF( INCX.EQ.0 )THEN
         INFO = 9
      END IF
      IF( INFO.NE.0 )THEN
         CALL XERBLA( 'DTBSV ', INFO )
         RETURN
      END IF
*
*     Quick return if possible.
*
      IF( N.EQ.0 )
     $   RETURN
*
      NOUNIT = LSAME( DIAG, 'N' )
*
*     Set up the start point in X if the increment is not unity. This
*     will be  ( N - 1 )*INCX  too small for descending loops.
*
      IF( INCX.LE.0 )THEN
         KX = 1 - ( N - 1 )*INCX
      ELSE IF( INCX.NE.1 )THEN
         KX = 1
      END IF
*
*     Start the operations. In this version the elements of A are
*     accessed by sequentially with one pass through A.
*
      IF( LSAME( TRANS, 'N' ) )THEN
*
*        Form  x := inv( A )*x.
*
         IF( LSAME( UPLO, 'U' ) )THEN
            KPLUS1 = K + 1
            IF( INCX.EQ.1 )THEN
               DO 20, J = N, 1, -1
                  IF( X( J ).NE.ZERO )THEN
                     L = KPLUS1 - J
                     IF( NOUNIT )
     $                  X( J ) = X( J )/A( KPLUS1, J )
                     TEMP = X( J )
                     DO 10, I = J - 1, MAX( 1, J - K ), -1
                        X( I ) = X( I ) - TEMP*A( L + I, J )
   10                CONTINUE
                  END IF
   20          CONTINUE
            ELSE
               KX = KX + ( N - 1 )*INCX
               JX = KX
               DO 40, J = N, 1, -1
                  KX = KX - INCX
                  IF( X( JX ).NE.ZERO )THEN
                     IX = KX
                     L  = KPLUS1 - J
                     IF( NOUNIT )
     $                  X( JX ) = X( JX )/A( KPLUS1, J )
                     TEMP = X( JX )
                     DO 30, I = J - 1, MAX( 1, J - K ), -1
                        X( IX ) = X( IX ) - TEMP*A( L + I, J )
                        IX      = IX      - INCX
   30                CONTINUE
                  END IF
                  JX = JX - INCX
   40          CONTINUE
            END IF
         ELSE
            IF( INCX.EQ.1 )THEN
               DO 60, J = 1, N
                  IF( X( J ).NE.ZERO )THEN
                     L = 1 - J
                     IF( NOUNIT )
     $                  X( J ) = X( J )/A( 1, J )
                     TEMP = X( J )
                     DO 50, I = J + 1, MIN( N, J + K )
                        X( I ) = X( I ) - TEMP*A( L + I, J )
   50                CONTINUE
                  END IF
   60          CONTINUE
            ELSE
               JX = KX
               DO 80, J = 1, N
                  KX = KX + INCX
                  IF( X( JX ).NE.ZERO )THEN
                     IX = KX
                     L  = 1  - J
                     IF( NOUNIT )
     $                  X( JX ) = X( JX )/A( 1, J )
                     TEMP = X( JX )
                     DO 70, I = J + 1, MIN( N, J + K )
                        X( IX ) = X( IX ) - TEMP*A( L + I, J )
                        IX      = IX      + INCX
   70                CONTINUE
                  END IF
                  JX = JX + INCX
   80          CONTINUE
            END IF
         END IF
      ELSE
*
*        Form  x := inv( A')*x.
*
         IF( LSAME( UPLO, 'U' ) )THEN
            KPLUS1 = K + 1
            IF( INCX.EQ.1 )THEN
               DO 100, J = 1, N
                  TEMP = X( J )
                  L    = KPLUS1 - J
                  DO 90, I = MAX( 1, J - K ), J - 1
                     TEMP = TEMP - A( L + I, J )*X( I )
   90             CONTINUE
                  IF( NOUNIT )
     $               TEMP = TEMP/A( KPLUS1, J )
                  X( J ) = TEMP
  100          CONTINUE
            ELSE
               JX = KX
               DO 120, J = 1, N
                  TEMP = X( JX )
                  IX   = KX
                  L    = KPLUS1  - J
                  DO 110, I = MAX( 1, J - K ), J - 1
                     TEMP = TEMP - A( L + I, J )*X( IX )
                     IX   = IX   + INCX
  110             CONTINUE
                  IF( NOUNIT )
     $               TEMP = TEMP/A( KPLUS1, J )
                  X( JX ) = TEMP
                  JX      = JX   + INCX
                  IF( J.GT.K )
     $               KX = KX + INCX
  120          CONTINUE
            END IF
         ELSE
            IF( INCX.EQ.1 )THEN
               DO 140, J = N, 1, -1
                  TEMP = X( J )
                  L    = 1      - J
                  DO 130, I = MIN( N, J + K ), J + 1, -1
                     TEMP = TEMP - A( L + I, J )*X( I )
  130             CONTINUE
                  IF( NOUNIT )
     $               TEMP = TEMP/A( 1, J )
                  X( J ) = TEMP
  140          CONTINUE
            ELSE
               KX = KX + ( N - 1 )*INCX
               JX = KX
               DO 160, J = N, 1, -1
                  TEMP = X( JX )
                  IX   = KX
                  L    = 1       - J
                  DO 150, I = MIN( N, J + K ), J + 1, -1
                     TEMP = TEMP - A( L + I, J )*X( IX )
                     IX   = IX   - INCX
  150             CONTINUE
                  IF( NOUNIT )
     $               TEMP = TEMP/A( 1, J )
                  X( JX ) = TEMP
                  JX      = JX   - INCX
                  IF( ( N - J ).GE.K )
     $               KX = KX - INCX
  160          CONTINUE
            END IF
         END IF
      END IF
*
      RETURN
*
*     End of DTBSV .
*
      END
      SUBROUTINE DTRMM ( SIDE, UPLO, TRANSA, DIAG, M, N, ALPHA, A, LDA,
     $                   B, LDB )
*     .. Scalar Arguments ..
      CHARACTER*1        SIDE, UPLO, TRANSA, DIAG
      INTEGER            M, N, LDA, LDB
      DOUBLE PRECISION   ALPHA
*     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), B( LDB, * )
*     ..
*
*  Purpose
*  =======
*
*  DTRMM  performs one of the matrix-matrix operations
*
*     B := alpha*op( A )*B,   or   B := alpha*B*op( A ),
*
*  where  alpha  is a scalar,  B  is an m by n matrix,  A  is a unit, or
*  non-unit,  upper or lower triangular matrix  and  op( A )  is one  of
*
*     op( A ) = A   or   op( A ) = A'.
*
*  Parameters
*  ==========
*
*  SIDE   - CHARACTER*1.
*           On entry,  SIDE specifies whether  op( A ) multiplies B from
*           the left or right as follows:
*
*              SIDE = 'L' or 'l'   B := alpha*op( A )*B.
*
*              SIDE = 'R' or 'r'   B := alpha*B*op( A ).
*
*           Unchanged on exit.
*
*  UPLO   - CHARACTER*1.
*           On entry, UPLO specifies whether the matrix A is an upper or
*           lower triangular matrix as follows:
*
*              UPLO = 'U' or 'u'   A is an upper triangular matrix.
*
*              UPLO = 'L' or 'l'   A is a lower triangular matrix.
*
*           Unchanged on exit.
*
*  TRANSA - CHARACTER*1.
*           On entry, TRANSA specifies the form of op( A ) to be used in
*           the matrix multiplication as follows:
*
*              TRANSA = 'N' or 'n'   op( A ) = A.
*
*              TRANSA = 'T' or 't'   op( A ) = A'.
*
*              TRANSA = 'C' or 'c'   op( A ) = A'.
*
*           Unchanged on exit.
*
*  DIAG   - CHARACTER*1.
*           On entry, DIAG specifies whether or not A is unit triangular
*           as follows:
*
*              DIAG = 'U' or 'u'   A is assumed to be unit triangular.
*
*              DIAG = 'N' or 'n'   A is not assumed to be unit
*                                  triangular.
*
*           Unchanged on exit.
*
*  M      - INTEGER.
*           On entry, M specifies the number of rows of B. M must be at
*           least zero.
*           Unchanged on exit.
*
*  N      - INTEGER.
*           On entry, N specifies the number of columns of B.  N must be
*           at least zero.
*           Unchanged on exit.
*
*  ALPHA  - DOUBLE PRECISION.
*           On entry,  ALPHA specifies the scalar  alpha. When  alpha is
*           zero then  A is not referenced and  B need not be set before
*           entry.
*           Unchanged on exit.
*
*  A      - DOUBLE PRECISION array of DIMENSION ( LDA, k ), where k is m
*           when  SIDE = 'L' or 'l'  and is  n  when  SIDE = 'R' or 'r'.
*           Before entry  with  UPLO = 'U' or 'u',  the  leading  k by k
*           upper triangular part of the array  A must contain the upper
*           triangular matrix  and the strictly lower triangular part of
*           A is not referenced.
*           Before entry  with  UPLO = 'L' or 'l',  the  leading  k by k
*           lower triangular part of the array  A must contain the lower
*           triangular matrix  and the strictly upper triangular part of
*           A is not referenced.
*           Note that when  DIAG = 'U' or 'u',  the diagonal elements of
*           A  are not referenced either,  but are assumed to be  unity.
*           Unchanged on exit.
*
*  LDA    - INTEGER.
*           On entry, LDA specifies the first dimension of A as declared
*           in the calling (sub) program.  When  SIDE = 'L' or 'l'  then
*           LDA  must be at least  max( 1, m ),  when  SIDE = 'R' or 'r'
*           then LDA must be at least max( 1, n ).
*           Unchanged on exit.
*
*  B      - DOUBLE PRECISION array of DIMENSION ( LDB, n ).
*           Before entry,  the leading  m by n part of the array  B must
*           contain the matrix  B,  and  on exit  is overwritten  by the
*           transformed matrix.
*
*  LDB    - INTEGER.
*           On entry, LDB specifies the first dimension of B as declared
*           in  the  calling  (sub)  program.   LDB  must  be  at  least
*           max( 1, m ).
*           Unchanged on exit.
*
*
*  Level 3 Blas routine.
*
*  -- Written on 8-February-1989.
*     Jack Dongarra, Argonne National Laboratory.
*     Iain Duff, AERE Harwell.
*     Jeremy Du Croz, Numerical Algorithms Group Ltd.
*     Sven Hammarling, Numerical Algorithms Group Ltd.
*
*
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
*     .. External Subroutines ..
      EXTERNAL           XERBLA
*     .. Intrinsic Functions ..
      INTRINSIC          MAX
*     .. Local Scalars ..
      LOGICAL            LSIDE, NOUNIT, UPPER
      INTEGER            I, INFO, J, K, NROWA
      DOUBLE PRECISION   TEMP
*     .. Parameters ..
      DOUBLE PRECISION   ONE         , ZERO
      PARAMETER        ( ONE = 1.0D+0, ZERO = 0.0D+0 )
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters.
*
      LSIDE  = LSAME( SIDE  , 'L' )
      IF( LSIDE )THEN
         NROWA = M
      ELSE
         NROWA = N
      END IF
      NOUNIT = LSAME( DIAG  , 'N' )
      UPPER  = LSAME( UPLO  , 'U' )
*
      INFO   = 0
      IF(      ( .NOT.LSIDE                ).AND.
     $         ( .NOT.LSAME( SIDE  , 'R' ) )      )THEN
         INFO = 1
      ELSE IF( ( .NOT.UPPER                ).AND.
     $         ( .NOT.LSAME( UPLO  , 'L' ) )      )THEN
         INFO = 2
      ELSE IF( ( .NOT.LSAME( TRANSA, 'N' ) ).AND.
     $         ( .NOT.LSAME( TRANSA, 'T' ) ).AND.
     $         ( .NOT.LSAME( TRANSA, 'C' ) )      )THEN
         INFO = 3
      ELSE IF( ( .NOT.LSAME( DIAG  , 'U' ) ).AND.
     $         ( .NOT.LSAME( DIAG  , 'N' ) )      )THEN
         INFO = 4
      ELSE IF( M  .LT.0               )THEN
         INFO = 5
      ELSE IF( N  .LT.0               )THEN
         INFO = 6
      ELSE IF( LDA.LT.MAX( 1, NROWA ) )THEN
         INFO = 9
      ELSE IF( LDB.LT.MAX( 1, M     ) )THEN
         INFO = 11
      END IF
      IF( INFO.NE.0 )THEN
         CALL XERBLA( 'DTRMM ', INFO )
         RETURN
      END IF
*
*     Quick return if possible.
*
      IF( N.EQ.0 )
     $   RETURN
*
*     And when  alpha.eq.zero.
*
      IF( ALPHA.EQ.ZERO )THEN
         DO 20, J = 1, N
            DO 10, I = 1, M
               B( I, J ) = ZERO
   10       CONTINUE
   20    CONTINUE
         RETURN
      END IF
*
*     Start the operations.
*
      IF( LSIDE )THEN
         IF( LSAME( TRANSA, 'N' ) )THEN
*
*           Form  B := alpha*A*B.
*
            IF( UPPER )THEN
               DO 50, J = 1, N
                  DO 40, K = 1, M
                     IF( B( K, J ).NE.ZERO )THEN
                        TEMP = ALPHA*B( K, J )
                        DO 30, I = 1, K - 1
                           B( I, J ) = B( I, J ) + TEMP*A( I, K )
   30                   CONTINUE
                        IF( NOUNIT )
     $                     TEMP = TEMP*A( K, K )
                        B( K, J ) = TEMP
                     END IF
   40             CONTINUE
   50          CONTINUE
            ELSE
               DO 80, J = 1, N
                  DO 70 K = M, 1, -1
                     IF( B( K, J ).NE.ZERO )THEN
                        TEMP      = ALPHA*B( K, J )
                        B( K, J ) = TEMP
                        IF( NOUNIT )
     $                     B( K, J ) = B( K, J )*A( K, K )
                        DO 60, I = K + 1, M
                           B( I, J ) = B( I, J ) + TEMP*A( I, K )
   60                   CONTINUE
                     END IF
   70             CONTINUE
   80          CONTINUE
            END IF
         ELSE
*
*           Form  B := alpha*B*A'.
*
            IF( UPPER )THEN
               DO 110, J = 1, N
                  DO 100, I = M, 1, -1
                     TEMP = B( I, J )
                     IF( NOUNIT )
     $                  TEMP = TEMP*A( I, I )
                     DO 90, K = 1, I - 1
                        TEMP = TEMP + A( K, I )*B( K, J )
   90                CONTINUE
                     B( I, J ) = ALPHA*TEMP
  100             CONTINUE
  110          CONTINUE
            ELSE
               DO 140, J = 1, N
                  DO 130, I = 1, M
                     TEMP = B( I, J )
                     IF( NOUNIT )
     $                  TEMP = TEMP*A( I, I )
                     DO 120, K = I + 1, M
                        TEMP = TEMP + A( K, I )*B( K, J )
  120                CONTINUE
                     B( I, J ) = ALPHA*TEMP
  130             CONTINUE
  140          CONTINUE
            END IF
         END IF
      ELSE
         IF( LSAME( TRANSA, 'N' ) )THEN
*
*           Form  B := alpha*B*A.
*
            IF( UPPER )THEN
               DO 180, J = N, 1, -1
                  TEMP = ALPHA
                  IF( NOUNIT )
     $               TEMP = TEMP*A( J, J )
                  DO 150, I = 1, M
                     B( I, J ) = TEMP*B( I, J )
  150             CONTINUE
                  DO 170, K = 1, J - 1
                     IF( A( K, J ).NE.ZERO )THEN
                        TEMP = ALPHA*A( K, J )
                        DO 160, I = 1, M
                           B( I, J ) = B( I, J ) + TEMP*B( I, K )
  160                   CONTINUE
                     END IF
  170             CONTINUE
  180          CONTINUE
            ELSE
               DO 220, J = 1, N
                  TEMP = ALPHA
                  IF( NOUNIT )
     $               TEMP = TEMP*A( J, J )
                  DO 190, I = 1, M
                     B( I, J ) = TEMP*B( I, J )
  190             CONTINUE
                  DO 210, K = J + 1, N
                     IF( A( K, J ).NE.ZERO )THEN
                        TEMP = ALPHA*A( K, J )
                        DO 200, I = 1, M
                           B( I, J ) = B( I, J ) + TEMP*B( I, K )
  200                   CONTINUE
                     END IF
  210             CONTINUE
  220          CONTINUE
            END IF
         ELSE
*
*           Form  B := alpha*B*A'.
*
            IF( UPPER )THEN
               DO 260, K = 1, N
                  DO 240, J = 1, K - 1
                     IF( A( J, K ).NE.ZERO )THEN
                        TEMP = ALPHA*A( J, K )
                        DO 230, I = 1, M
                           B( I, J ) = B( I, J ) + TEMP*B( I, K )
  230                   CONTINUE
                     END IF
  240             CONTINUE
                  TEMP = ALPHA
                  IF( NOUNIT )
     $               TEMP = TEMP*A( K, K )
                  IF( TEMP.NE.ONE )THEN
                     DO 250, I = 1, M
                        B( I, K ) = TEMP*B( I, K )
  250                CONTINUE
                  END IF
  260          CONTINUE
            ELSE
               DO 300, K = N, 1, -1
                  DO 280, J = K + 1, N
                     IF( A( J, K ).NE.ZERO )THEN
                        TEMP = ALPHA*A( J, K )
                        DO 270, I = 1, M
                           B( I, J ) = B( I, J ) + TEMP*B( I, K )
  270                   CONTINUE
                     END IF
  280             CONTINUE
                  TEMP = ALPHA
                  IF( NOUNIT )
     $               TEMP = TEMP*A( K, K )
                  IF( TEMP.NE.ONE )THEN
                     DO 290, I = 1, M
                        B( I, K ) = TEMP*B( I, K )
  290                CONTINUE
                  END IF
  300          CONTINUE
            END IF
         END IF
      END IF
*
      RETURN
*
*     End of DTRMM .
*
      END
      SUBROUTINE DTRSM ( SIDE, UPLO, TRANSA, DIAG, M, N, ALPHA, A, LDA,
     $                   B, LDB )
*     .. Scalar Arguments ..
      CHARACTER*1        SIDE, UPLO, TRANSA, DIAG
      INTEGER            M, N, LDA, LDB
      DOUBLE PRECISION   ALPHA
*     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), B( LDB, * )
*     ..
*
*  Purpose
*  =======
*
*  DTRSM  solves one of the matrix equations
*
*     op( A )*X = alpha*B,   or   X*op( A ) = alpha*B,
*
*  where alpha is a scalar, X and B are m by n matrices, A is a unit, or
*  non-unit,  upper or lower triangular matrix  and  op( A )  is one  of
*
*     op( A ) = A   or   op( A ) = A'.
*
*  The matrix X is overwritten on B.
*
*  Parameters
*  ==========
*
*  SIDE   - CHARACTER*1.
*           On entry, SIDE specifies whether op( A ) appears on the left
*           or right of X as follows:
*
*              SIDE = 'L' or 'l'   op( A )*X = alpha*B.
*
*              SIDE = 'R' or 'r'   X*op( A ) = alpha*B.
*
*           Unchanged on exit.
*
*  UPLO   - CHARACTER*1.
*           On entry, UPLO specifies whether the matrix A is an upper or
*           lower triangular matrix as follows:
*
*              UPLO = 'U' or 'u'   A is an upper triangular matrix.
*
*              UPLO = 'L' or 'l'   A is a lower triangular matrix.
*
*           Unchanged on exit.
*
*  TRANSA - CHARACTER*1.
*           On entry, TRANSA specifies the form of op( A ) to be used in
*           the matrix multiplication as follows:
*
*              TRANSA = 'N' or 'n'   op( A ) = A.
*
*              TRANSA = 'T' or 't'   op( A ) = A'.
*
*              TRANSA = 'C' or 'c'   op( A ) = A'.
*
*           Unchanged on exit.
*
*  DIAG   - CHARACTER*1.
*           On entry, DIAG specifies whether or not A is unit triangular
*           as follows:
*
*              DIAG = 'U' or 'u'   A is assumed to be unit triangular.
*
*              DIAG = 'N' or 'n'   A is not assumed to be unit
*                                  triangular.
*
*           Unchanged on exit.
*
*  M      - INTEGER.
*           On entry, M specifies the number of rows of B. M must be at
*           least zero.
*           Unchanged on exit.
*
*  N      - INTEGER.
*           On entry, N specifies the number of columns of B.  N must be
*           at least zero.
*           Unchanged on exit.
*
*  ALPHA  - DOUBLE PRECISION.
*           On entry,  ALPHA specifies the scalar  alpha. When  alpha is
*           zero then  A is not referenced and  B need not be set before
*           entry.
*           Unchanged on exit.
*
*  A      - DOUBLE PRECISION array of DIMENSION ( LDA, k ), where k is m
*           when  SIDE = 'L' or 'l'  and is  n  when  SIDE = 'R' or 'r'.
*           Before entry  with  UPLO = 'U' or 'u',  the  leading  k by k
*           upper triangular part of the array  A must contain the upper
*           triangular matrix  and the strictly lower triangular part of
*           A is not referenced.
*           Before entry  with  UPLO = 'L' or 'l',  the  leading  k by k
*           lower triangular part of the array  A must contain the lower
*           triangular matrix  and the strictly upper triangular part of
*           A is not referenced.
*           Note that when  DIAG = 'U' or 'u',  the diagonal elements of
*           A  are not referenced either,  but are assumed to be  unity.
*           Unchanged on exit.
*
*  LDA    - INTEGER.
*           On entry, LDA specifies the first dimension of A as declared
*           in the calling (sub) program.  When  SIDE = 'L' or 'l'  then
*           LDA  must be at least  max( 1, m ),  when  SIDE = 'R' or 'r'
*           then LDA must be at least max( 1, n ).
*           Unchanged on exit.
*
*  B      - DOUBLE PRECISION array of DIMENSION ( LDB, n ).
*           Before entry,  the leading  m by n part of the array  B must
*           contain  the  right-hand  side  matrix  B,  and  on exit  is
*           overwritten by the solution matrix  X.
*
*  LDB    - INTEGER.
*           On entry, LDB specifies the first dimension of B as declared
*           in  the  calling  (sub)  program.   LDB  must  be  at  least
*           max( 1, m ).
*           Unchanged on exit.
*
*
*  Level 3 Blas routine.
*
*
*  -- Written on 8-February-1989.
*     Jack Dongarra, Argonne National Laboratory.
*     Iain Duff, AERE Harwell.
*     Jeremy Du Croz, Numerical Algorithms Group Ltd.
*     Sven Hammarling, Numerical Algorithms Group Ltd.
*
*
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
*     .. External Subroutines ..
      EXTERNAL           XERBLA
*     .. Intrinsic Functions ..
      INTRINSIC          MAX
*     .. Local Scalars ..
      LOGICAL            LSIDE, NOUNIT, UPPER
      INTEGER            I, INFO, J, K, NROWA
      DOUBLE PRECISION   TEMP
*     .. Parameters ..
      DOUBLE PRECISION   ONE         , ZERO
      PARAMETER        ( ONE = 1.0D+0, ZERO = 0.0D+0 )
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters.
*
      LSIDE  = LSAME( SIDE  , 'L' )
      IF( LSIDE )THEN
         NROWA = M
      ELSE
         NROWA = N
      END IF
      NOUNIT = LSAME( DIAG  , 'N' )
      UPPER  = LSAME( UPLO  , 'U' )
*
      INFO   = 0
      IF(      ( .NOT.LSIDE                ).AND.
     $         ( .NOT.LSAME( SIDE  , 'R' ) )      )THEN
         INFO = 1
      ELSE IF( ( .NOT.UPPER                ).AND.
     $         ( .NOT.LSAME( UPLO  , 'L' ) )      )THEN
         INFO = 2
      ELSE IF( ( .NOT.LSAME( TRANSA, 'N' ) ).AND.
     $         ( .NOT.LSAME( TRANSA, 'T' ) ).AND.
     $         ( .NOT.LSAME( TRANSA, 'C' ) )      )THEN
         INFO = 3
      ELSE IF( ( .NOT.LSAME( DIAG  , 'U' ) ).AND.
     $         ( .NOT.LSAME( DIAG  , 'N' ) )      )THEN
         INFO = 4
      ELSE IF( M  .LT.0               )THEN
         INFO = 5
      ELSE IF( N  .LT.0               )THEN
         INFO = 6
      ELSE IF( LDA.LT.MAX( 1, NROWA ) )THEN
         INFO = 9
      ELSE IF( LDB.LT.MAX( 1, M     ) )THEN
         INFO = 11
      END IF
      IF( INFO.NE.0 )THEN
         CALL XERBLA( 'DTRSM ', INFO )
         RETURN
      END IF
*
*     Quick return if possible.
*
      IF( N.EQ.0 )
     $   RETURN
*
*     And when  alpha.eq.zero.
*
      IF( ALPHA.EQ.ZERO )THEN
         DO 20, J = 1, N
            DO 10, I = 1, M
               B( I, J ) = ZERO
   10       CONTINUE
   20    CONTINUE
         RETURN
      END IF
*
*     Start the operations.
*
      IF( LSIDE )THEN
         IF( LSAME( TRANSA, 'N' ) )THEN
*
*           Form  B := alpha*inv( A )*B.
*
            IF( UPPER )THEN
               DO 60, J = 1, N
                  IF( ALPHA.NE.ONE )THEN
                     DO 30, I = 1, M
                        B( I, J ) = ALPHA*B( I, J )
   30                CONTINUE
                  END IF
                  DO 50, K = M, 1, -1
                     IF( B( K, J ).NE.ZERO )THEN
                        IF( NOUNIT )
     $                     B( K, J ) = B( K, J )/A( K, K )
                        DO 40, I = 1, K - 1
                           B( I, J ) = B( I, J ) - B( K, J )*A( I, K )
   40                   CONTINUE
                     END IF
   50             CONTINUE
   60          CONTINUE
            ELSE
               DO 100, J = 1, N
                  IF( ALPHA.NE.ONE )THEN
                     DO 70, I = 1, M
                        B( I, J ) = ALPHA*B( I, J )
   70                CONTINUE
                  END IF
                  DO 90 K = 1, M
                     IF( B( K, J ).NE.ZERO )THEN
                        IF( NOUNIT )
     $                     B( K, J ) = B( K, J )/A( K, K )
                        DO 80, I = K + 1, M
                           B( I, J ) = B( I, J ) - B( K, J )*A( I, K )
   80                   CONTINUE
                     END IF
   90             CONTINUE
  100          CONTINUE
            END IF
         ELSE
*
*           Form  B := alpha*inv( A' )*B.
*
            IF( UPPER )THEN
               DO 130, J = 1, N
                  DO 120, I = 1, M
                     TEMP = ALPHA*B( I, J )
                     DO 110, K = 1, I - 1
                        TEMP = TEMP - A( K, I )*B( K, J )
  110                CONTINUE
                     IF( NOUNIT )
     $                  TEMP = TEMP/A( I, I )
                     B( I, J ) = TEMP
  120             CONTINUE
  130          CONTINUE
            ELSE
               DO 160, J = 1, N
                  DO 150, I = M, 1, -1
                     TEMP = ALPHA*B( I, J )
                     DO 140, K = I + 1, M
                        TEMP = TEMP - A( K, I )*B( K, J )
  140                CONTINUE
                     IF( NOUNIT )
     $                  TEMP = TEMP/A( I, I )
                     B( I, J ) = TEMP
  150             CONTINUE
  160          CONTINUE
            END IF
         END IF
      ELSE
         IF( LSAME( TRANSA, 'N' ) )THEN
*
*           Form  B := alpha*B*inv( A ).
*
            IF( UPPER )THEN
               DO 210, J = 1, N
                  IF( ALPHA.NE.ONE )THEN
                     DO 170, I = 1, M
                        B( I, J ) = ALPHA*B( I, J )
  170                CONTINUE
                  END IF
                  DO 190, K = 1, J - 1
                     IF( A( K, J ).NE.ZERO )THEN
                        DO 180, I = 1, M
                           B( I, J ) = B( I, J ) - A( K, J )*B( I, K )
  180                   CONTINUE
                     END IF
  190             CONTINUE
                  IF( NOUNIT )THEN
                     TEMP = ONE/A( J, J )
                     DO 200, I = 1, M
                        B( I, J ) = TEMP*B( I, J )
  200                CONTINUE
                  END IF
  210          CONTINUE
            ELSE
               DO 260, J = N, 1, -1
                  IF( ALPHA.NE.ONE )THEN
                     DO 220, I = 1, M
                        B( I, J ) = ALPHA*B( I, J )
  220                CONTINUE
                  END IF
                  DO 240, K = J + 1, N
                     IF( A( K, J ).NE.ZERO )THEN
                        DO 230, I = 1, M
                           B( I, J ) = B( I, J ) - A( K, J )*B( I, K )
  230                   CONTINUE
                     END IF
  240             CONTINUE
                  IF( NOUNIT )THEN
                     TEMP = ONE/A( J, J )
                     DO 250, I = 1, M
                       B( I, J ) = TEMP*B( I, J )
  250                CONTINUE
                  END IF
  260          CONTINUE
            END IF
         ELSE
*
*           Form  B := alpha*B*inv( A' ).
*
            IF( UPPER )THEN
               DO 310, K = N, 1, -1
                  IF( NOUNIT )THEN
                     TEMP = ONE/A( K, K )
                     DO 270, I = 1, M
                        B( I, K ) = TEMP*B( I, K )
  270                CONTINUE
                  END IF
                  DO 290, J = 1, K - 1
                     IF( A( J, K ).NE.ZERO )THEN
                        TEMP = A( J, K )
                        DO 280, I = 1, M
                           B( I, J ) = B( I, J ) - TEMP*B( I, K )
  280                   CONTINUE
                     END IF
  290             CONTINUE
                  IF( ALPHA.NE.ONE )THEN
                     DO 300, I = 1, M
                        B( I, K ) = ALPHA*B( I, K )
  300                CONTINUE
                  END IF
  310          CONTINUE
            ELSE
               DO 360, K = 1, N
                  IF( NOUNIT )THEN
                     TEMP = ONE/A( K, K )
                     DO 320, I = 1, M
                        B( I, K ) = TEMP*B( I, K )
  320                CONTINUE
                  END IF
                  DO 340, J = K + 1, N
                     IF( A( J, K ).NE.ZERO )THEN
                        TEMP = A( J, K )
                        DO 330, I = 1, M
                           B( I, J ) = B( I, J ) - TEMP*B( I, K )
  330                   CONTINUE
                     END IF
  340             CONTINUE
                  IF( ALPHA.NE.ONE )THEN
                     DO 350, I = 1, M
                        B( I, K ) = ALPHA*B( I, K )
  350                CONTINUE
                  END IF
  360          CONTINUE
            END IF
         END IF
      END IF
*
      RETURN
*
*     End of DTRSM .
*
      END
      integer function idamax(n,dx,incx)
c
c     finds the index of element having max. absolute value.
c     jack dongarra, linpack, 3/11/78.
c     modified to correct problem with negative increment, 8/21/90.
c
      double precision dx(1),dmax
      integer i,incx,ix,n
c
      idamax = 0
      if( n .lt. 1 ) return
      idamax = 1
      if(n.eq.1)return
      if(incx.eq.1)go to 20
c
c        code for increment not equal to 1
c
      ix = 1
      if(incx.lt.0)ix = (-n+1)*incx + 1
      dmax = dabs(dx(ix))
      ix = ix + incx
      do 10 i = 2,n
         if(dabs(dx(ix)).le.dmax) go to 5
         idamax = i
         dmax = dabs(dx(ix))
    5    ix = ix + incx
   10 continue
      return
c
c        code for increment equal to 1
c
   20 dmax = dabs(dx(1))
      do 30 i = 2,n
         if(dabs(dx(i)).le.dmax) go to 30
         idamax = i
         dmax = dabs(dx(i))
   30 continue
      return
      end
      integer function isamax(n,sx,incx)
c
c     finds the index of element having max. absolute value.
c     jack dongarra, linpack, 3/11/78.
c     modified to correct problem with negative increment, 8/21/90.
c
      real sx(1),smax
      integer i,incx,ix,n
c
      isamax = 0
      if( n .lt. 1 ) return
      isamax = 1
      if(n.eq.1)return
      if(incx.eq.1)go to 20
c
c        code for increment not equal to 1
c
      ix = 1
      if(incx.lt.0)ix = (-n+1)*incx + 1
      smax = abs(sx(ix))
      ix = ix + incx
      do 10 i = 2,n
         if(abs(sx(ix)).le.smax) go to 5
         isamax = i
         smax = abs(sx(ix))
    5    ix = ix + incx
   10 continue
      return
c
c        code for increment equal to 1
c
   20 smax = abs(sx(1))
      do 30 i = 2,n
         if(abs(sx(i)).le.smax) go to 30
         isamax = i
         smax = abs(sx(i))
   30 continue
      return
      end
      real function sasum(n,sx,incx)
c
c     takes the sum of the absolute values.
c     uses unrolled loops for increment equal to one.
c     jack dongarra, linpack, 3/11/78.
c     modified to correct problem with negative increment, 8/21/90.
c
      real sx(1),stemp
      integer i,incx,ix,m,mp1,n
c
      sasum = 0.0e0
      stemp = 0.0e0
      if(n.le.0)return
      if(incx.eq.1)go to 20
c
c        code for increment not equal to 1
c
      ix = 1
      if(incx.lt.0)ix = (-n+1)*incx + 1
      do 10 i = 1,n
        stemp = stemp + abs(sx(ix))
        ix = ix + incx
   10 continue
      sasum = stemp
      return
c
c        code for increment equal to 1
c
c
c        clean-up loop
c
   20 m = mod(n,6)
      if( m .eq. 0 ) go to 40
      do 30 i = 1,m
        stemp = stemp + abs(sx(i))
   30 continue
      if( n .lt. 6 ) go to 60
   40 mp1 = m + 1
      do 50 i = mp1,n,6
        stemp = stemp + abs(sx(i)) + abs(sx(i + 1)) + abs(sx(i + 2))
     *  + abs(sx(i + 3)) + abs(sx(i + 4)) + abs(sx(i + 5))
   50 continue
   60 sasum = stemp
      return
      end
      subroutine saxpy(n,sa,sx,incx,sy,incy)
c
c     constant times a vector plus a vector.
c     uses unrolled loop for increments equal to one.
c     jack dongarra, linpack, 3/11/78.
c
      real sx(1),sy(1),sa
      integer i,incx,incy,ix,iy,m,mp1,n
c
      if(n.le.0)return
      if (sa .eq. 0.0) return
      if(incx.eq.1.and.incy.eq.1)go to 20
c
c        code for unequal increments or equal increments
c          not equal to 1
c
      ix = 1
      iy = 1
      if(incx.lt.0)ix = (-n+1)*incx + 1
      if(incy.lt.0)iy = (-n+1)*incy + 1
      do 10 i = 1,n
        sy(iy) = sy(iy) + sa*sx(ix)
        ix = ix + incx
        iy = iy + incy
   10 continue
      return
c
c        code for both increments equal to 1
c
c
c        clean-up loop
c
   20 m = mod(n,4)
      if( m .eq. 0 ) go to 40
      do 30 i = 1,m
        sy(i) = sy(i) + sa*sx(i)
   30 continue
      if( n .lt. 4 ) return
   40 mp1 = m + 1
      do 50 i = mp1,n,4
        sy(i) = sy(i) + sa*sx(i)
        sy(i + 1) = sy(i + 1) + sa*sx(i + 1)
        sy(i + 2) = sy(i + 2) + sa*sx(i + 2)
        sy(i + 3) = sy(i + 3) + sa*sx(i + 3)
   50 continue
      return
      end
      subroutine scopy(n,sx,incx,sy,incy)
c
c     copies a vector, x, to a vector, y.
c     uses unrolled loops for increments equal to 1.
c     jack dongarra, linpack, 3/11/78.
c
      real sx(1),sy(1)
      integer i,incx,incy,ix,iy,m,mp1,n
c
      if(n.le.0)return
      if(incx.eq.1.and.incy.eq.1)go to 20
c
c        code for unequal increments or equal increments
c          not equal to 1
c
      ix = 1
      iy = 1
      if(incx.lt.0)ix = (-n+1)*incx + 1
      if(incy.lt.0)iy = (-n+1)*incy + 1
      do 10 i = 1,n
        sy(iy) = sx(ix)
        ix = ix + incx
        iy = iy + incy
   10 continue
      return
c
c        code for both increments equal to 1
c
c
c        clean-up loop
c
   20 m = mod(n,7)
      if( m .eq. 0 ) go to 40
      do 30 i = 1,m
        sy(i) = sx(i)
   30 continue
      if( n .lt. 7 ) return
   40 mp1 = m + 1
      do 50 i = mp1,n,7
        sy(i) = sx(i)
        sy(i + 1) = sx(i + 1)
        sy(i + 2) = sx(i + 2)
        sy(i + 3) = sx(i + 3)
        sy(i + 4) = sx(i + 4)
        sy(i + 5) = sx(i + 5)
        sy(i + 6) = sx(i + 6)
   50 continue
      return
      end
      real function sdot(n,sx,incx,sy,incy)
c
c     forms the dot product of two vectors.
c     uses unrolled loops for increments equal to one.
c     jack dongarra, linpack, 3/11/78.
c
      real sx(1),sy(1),stemp
      integer i,incx,incy,ix,iy,m,mp1,n
c
      stemp = 0.0e0
      sdot = 0.0e0
      if(n.le.0)return
      if(incx.eq.1.and.incy.eq.1)go to 20
c
c        code for unequal increments or equal increments
c          not equal to 1
c
      ix = 1
      iy = 1
      if(incx.lt.0)ix = (-n+1)*incx + 1
      if(incy.lt.0)iy = (-n+1)*incy + 1
      do 10 i = 1,n
        stemp = stemp + sx(ix)*sy(iy)
        ix = ix + incx
        iy = iy + incy
   10 continue
      sdot = stemp
      return
c
c        code for both increments equal to 1
c
c
c        clean-up loop
c
   20 m = mod(n,5)
      if( m .eq. 0 ) go to 40
      do 30 i = 1,m
        stemp = stemp + sx(i)*sy(i)
   30 continue
      if( n .lt. 5 ) go to 60
   40 mp1 = m + 1
      do 50 i = mp1,n,5
        stemp = stemp + sx(i)*sy(i) + sx(i + 1)*sy(i + 1) +
     *   sx(i + 2)*sy(i + 2) + sx(i + 3)*sy(i + 3) + sx(i + 4)*sy(i + 4)
   50 continue
   60 sdot = stemp
      return
      end
      SUBROUTINE SGBMV ( TRANS, M, N, KL, KU, ALPHA, A, LDA, X, INCX,
     $                   BETA, Y, INCY )
*     .. Scalar Arguments ..
      REAL               ALPHA, BETA
      INTEGER            INCX, INCY, KL, KU, LDA, M, N
      CHARACTER*1        TRANS
*     .. Array Arguments ..
      REAL               A( LDA, * ), X( * ), Y( * )
*     ..
*
*  Purpose
*  =======
*
*  SGBMV  performs one of the matrix-vector operations
*
*     y := alpha*A*x + beta*y,   or   y := alpha*A'*x + beta*y,
*
*  where alpha and beta are scalars, x and y are vectors and A is an
*  m by n band matrix, with kl sub-diagonals and ku super-diagonals.
*
*  Parameters
*  ==========
*
*  TRANS  - CHARACTER*1.
*           On entry, TRANS specifies the operation to be performed as
*           follows:
*
*              TRANS = 'N' or 'n'   y := alpha*A*x + beta*y.
*
*              TRANS = 'T' or 't'   y := alpha*A'*x + beta*y.
*
*              TRANS = 'C' or 'c'   y := alpha*A'*x + beta*y.
*
*           Unchanged on exit.
*
*  M      - INTEGER.
*           On entry, M specifies the number of rows of the matrix A.
*           M must be at least zero.
*           Unchanged on exit.
*
*  N      - INTEGER.
*           On entry, N specifies the number of columns of the matrix A.
*           N must be at least zero.
*           Unchanged on exit.
*
*  KL     - INTEGER.
*           On entry, KL specifies the number of sub-diagonals of the
*           matrix A. KL must satisfy  0 .le. KL.
*           Unchanged on exit.
*
*  KU     - INTEGER.
*           On entry, KU specifies the number of super-diagonals of the
*           matrix A. KU must satisfy  0 .le. KU.
*           Unchanged on exit.
*
*  ALPHA  - REAL            .
*           On entry, ALPHA specifies the scalar alpha.
*           Unchanged on exit.
*
*  A      - REAL             array of DIMENSION ( LDA, n ).
*           Before entry, the leading ( kl + ku + 1 ) by n part of the
*           array A must contain the matrix of coefficients, supplied
*           column by column, with the leading diagonal of the matrix in
*           row ( ku + 1 ) of the array, the first super-diagonal
*           starting at position 2 in row ku, the first sub-diagonal
*           starting at position 1 in row ( ku + 2 ), and so on.
*           Elements in the array A that do not correspond to elements
*           in the band matrix (such as the top left ku by ku triangle)
*           are not referenced.
*           The following program segment will transfer a band matrix
*           from conventional full matrix storage to band storage:
*
*                 DO 20, J = 1, N
*                    K = KU + 1 - J
*                    DO 10, I = MAX( 1, J - KU ), MIN( M, J + KL )
*                       A( K + I, J ) = matrix( I, J )
*              10    CONTINUE
*              20 CONTINUE
*
*           Unchanged on exit.
*
*  LDA    - INTEGER.
*           On entry, LDA specifies the first dimension of A as declared
*           in the calling (sub) program. LDA must be at least
*           ( kl + ku + 1 ).
*           Unchanged on exit.
*
*  X      - REAL             array of DIMENSION at least
*           ( 1 + ( n - 1 )*abs( INCX ) ) when TRANS = 'N' or 'n'
*           and at least
*           ( 1 + ( m - 1 )*abs( INCX ) ) otherwise.
*           Before entry, the incremented array X must contain the
*           vector x.
*           Unchanged on exit.
*
*  INCX   - INTEGER.
*           On entry, INCX specifies the increment for the elements of
*           X. INCX must not be zero.
*           Unchanged on exit.
*
*  BETA   - REAL            .
*           On entry, BETA specifies the scalar beta. When BETA is
*           supplied as zero then Y need not be set on input.
*           Unchanged on exit.
*
*  Y      - REAL             array of DIMENSION at least
*           ( 1 + ( m - 1 )*abs( INCY ) ) when TRANS = 'N' or 'n'
*           and at least
*           ( 1 + ( n - 1 )*abs( INCY ) ) otherwise.
*           Before entry, the incremented array Y must contain the
*           vector y. On exit, Y is overwritten by the updated vector y.
*
*  INCY   - INTEGER.
*           On entry, INCY specifies the increment for the elements of
*           Y. INCY must not be zero.
*           Unchanged on exit.
*
*
*  Level 2 Blas routine.
*
*  -- Written on 22-October-1986.
*     Jack Dongarra, Argonne National Lab.
*     Jeremy Du Croz, Nag Central Office.
*     Sven Hammarling, Nag Central Office.
*     Richard Hanson, Sandia National Labs.
*
*     .. Parameters ..
      REAL               ONE         , ZERO
      PARAMETER        ( ONE = 1.0E+0, ZERO = 0.0E+0 )
*     .. Local Scalars ..
      REAL               TEMP
      INTEGER            I, INFO, IX, IY, J, JX, JY, K, KUP1, KX, KY,
     $                   LENX, LENY
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
*     .. External Subroutines ..
      EXTERNAL           XERBLA
*     .. Intrinsic Functions ..
      INTRINSIC          MAX, MIN
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters.
*
      INFO = 0
      IF     ( .NOT.LSAME( TRANS, 'N' ).AND.
     $         .NOT.LSAME( TRANS, 'T' ).AND.
     $         .NOT.LSAME( TRANS, 'C' )      )THEN
         INFO = 1
      ELSE IF( M.LT.0 )THEN
         INFO = 2
      ELSE IF( N.LT.0 )THEN
         INFO = 3
      ELSE IF( KL.LT.0 )THEN
         INFO = 4
      ELSE IF( KU.LT.0 )THEN
         INFO = 5
      ELSE IF( LDA.LT.( KL + KU + 1 ) )THEN
         INFO = 8
      ELSE IF( INCX.EQ.0 )THEN
         INFO = 10
      ELSE IF( INCY.EQ.0 )THEN
         INFO = 13
      END IF
      IF( INFO.NE.0 )THEN
         CALL XERBLA( 'SGBMV ', INFO )
         RETURN
      END IF
*
*     Quick return if possible.
*
      IF( ( M.EQ.0 ).OR.( N.EQ.0 ).OR.
     $    ( ( ALPHA.EQ.ZERO ).AND.( BETA.EQ.ONE ) ) )
     $   RETURN
*
*     Set  LENX  and  LENY, the lengths of the vectors x and y, and set
*     up the start points in  X  and  Y.
*
      IF( LSAME( TRANS, 'N' ) )THEN
         LENX = N
         LENY = M
      ELSE
         LENX = M
         LENY = N
      END IF
      IF( INCX.GT.0 )THEN
         KX = 1
      ELSE
         KX = 1 - ( LENX - 1 )*INCX
      END IF
      IF( INCY.GT.0 )THEN
         KY = 1
      ELSE
         KY = 1 - ( LENY - 1 )*INCY
      END IF
*
*     Start the operations. In this version the elements of A are
*     accessed sequentially with one pass through the band part of A.
*
*     First form  y := beta*y.
*
      IF( BETA.NE.ONE )THEN
         IF( INCY.EQ.1 )THEN
            IF( BETA.EQ.ZERO )THEN
               DO 10, I = 1, LENY
                  Y( I ) = ZERO
   10          CONTINUE
            ELSE
               DO 20, I = 1, LENY
                  Y( I ) = BETA*Y( I )
   20          CONTINUE
            END IF
         ELSE
            IY = KY
            IF( BETA.EQ.ZERO )THEN
               DO 30, I = 1, LENY
                  Y( IY ) = ZERO
                  IY      = IY   + INCY
   30          CONTINUE
            ELSE
               DO 40, I = 1, LENY
                  Y( IY ) = BETA*Y( IY )
                  IY      = IY           + INCY
   40          CONTINUE
            END IF
         END IF
      END IF
      IF( ALPHA.EQ.ZERO )
     $   RETURN
      KUP1 = KU + 1
      IF( LSAME( TRANS, 'N' ) )THEN
*
*        Form  y := alpha*A*x + y.
*
         JX = KX
         IF( INCY.EQ.1 )THEN
            DO 60, J = 1, N
               IF( X( JX ).NE.ZERO )THEN
                  TEMP = ALPHA*X( JX )
                  K    = KUP1 - J
                  DO 50, I = MAX( 1, J - KU ), MIN( M, J + KL )
                     Y( I ) = Y( I ) + TEMP*A( K + I, J )
   50             CONTINUE
               END IF
               JX = JX + INCX
   60       CONTINUE
         ELSE
            DO 80, J = 1, N
               IF( X( JX ).NE.ZERO )THEN
                  TEMP = ALPHA*X( JX )
                  IY   = KY
                  K    = KUP1 - J
                  DO 70, I = MAX( 1, J - KU ), MIN( M, J + KL )
                     Y( IY ) = Y( IY ) + TEMP*A( K + I, J )
                     IY      = IY      + INCY
   70             CONTINUE
               END IF
               JX = JX + INCX
               IF( J.GT.KU )
     $            KY = KY + INCY
   80       CONTINUE
         END IF
      ELSE
*
*        Form  y := alpha*A'*x + y.
*
         JY = KY
         IF( INCX.EQ.1 )THEN
            DO 100, J = 1, N
               TEMP = ZERO
               K    = KUP1 - J
               DO 90, I = MAX( 1, J - KU ), MIN( M, J + KL )
                  TEMP = TEMP + A( K + I, J )*X( I )
   90          CONTINUE
               Y( JY ) = Y( JY ) + ALPHA*TEMP
               JY      = JY      + INCY
  100       CONTINUE
         ELSE
            DO 120, J = 1, N
               TEMP = ZERO
               IX   = KX
               K    = KUP1 - J
               DO 110, I = MAX( 1, J - KU ), MIN( M, J + KL )
                  TEMP = TEMP + A( K + I, J )*X( IX )
                  IX   = IX   + INCX
  110          CONTINUE
               Y( JY ) = Y( JY ) + ALPHA*TEMP
               JY      = JY      + INCY
               IF( J.GT.KU )
     $            KX = KX + INCX
  120       CONTINUE
         END IF
      END IF
*
      RETURN
*
*     End of SGBMV .
*
      END
      SUBROUTINE SGEMM ( TRANSA, TRANSB, M, N, K, ALPHA, A, LDA, B, LDB,
     $                   BETA, C, LDC )
*     .. Scalar Arguments ..
      CHARACTER*1        TRANSA, TRANSB
      INTEGER            M, N, K, LDA, LDB, LDC
      REAL               ALPHA, BETA
*     .. Array Arguments ..
      REAL               A( LDA, * ), B( LDB, * ), C( LDC, * )
*     ..
*
*  Purpose
*  =======
*
*  SGEMM  performs one of the matrix-matrix operations
*
*     C := alpha*op( A )*op( B ) + beta*C,
*
*  where  op( X ) is one of
*
*     op( X ) = X   or   op( X ) = X',
*
*  alpha and beta are scalars, and A, B and C are matrices, with op( A )
*  an m by k matrix,  op( B )  a  k by n matrix and  C an m by n matrix.
*
*  Parameters
*  ==========
*
*  TRANSA - CHARACTER*1.
*           On entry, TRANSA specifies the form of op( A ) to be used in
*           the matrix multiplication as follows:
*
*              TRANSA = 'N' or 'n',  op( A ) = A.
*
*              TRANSA = 'T' or 't',  op( A ) = A'.
*
*              TRANSA = 'C' or 'c',  op( A ) = A'.
*
*           Unchanged on exit.
*
*  TRANSB - CHARACTER*1.
*           On entry, TRANSB specifies the form of op( B ) to be used in
*           the matrix multiplication as follows:
*
*              TRANSB = 'N' or 'n',  op( B ) = B.
*
*              TRANSB = 'T' or 't',  op( B ) = B'.
*
*              TRANSB = 'C' or 'c',  op( B ) = B'.
*
*           Unchanged on exit.
*
*  M      - INTEGER.
*           On entry,  M  specifies  the number  of rows  of the  matrix
*           op( A )  and of the  matrix  C.  M  must  be at least  zero.
*           Unchanged on exit.
*
*  N      - INTEGER.
*           On entry,  N  specifies the number  of columns of the matrix
*           op( B ) and the number of columns of the matrix C. N must be
*           at least zero.
*           Unchanged on exit.
*
*  K      - INTEGER.
*           On entry,  K  specifies  the number of columns of the matrix
*           op( A ) and the number of rows of the matrix op( B ). K must
*           be at least  zero.
*           Unchanged on exit.
*
*  ALPHA  - REAL            .
*           On entry, ALPHA specifies the scalar alpha.
*           Unchanged on exit.
*
*  A      - REAL             array of DIMENSION ( LDA, ka ), where ka is
*           k  when  TRANSA = 'N' or 'n',  and is  m  otherwise.
*           Before entry with  TRANSA = 'N' or 'n',  the leading  m by k
*           part of the array  A  must contain the matrix  A,  otherwise
*           the leading  k by m  part of the array  A  must contain  the
*           matrix A.
*           Unchanged on exit.
*
*  LDA    - INTEGER.
*           On entry, LDA specifies the first dimension of A as declared
*           in the calling (sub) program. When  TRANSA = 'N' or 'n' then
*           LDA must be at least  max( 1, m ), otherwise  LDA must be at
*           least  max( 1, k ).
*           Unchanged on exit.
*
*  B      - REAL             array of DIMENSION ( LDB, kb ), where kb is
*           n  when  TRANSB = 'N' or 'n',  and is  k  otherwise.
*           Before entry with  TRANSB = 'N' or 'n',  the leading  k by n
*           part of the array  B  must contain the matrix  B,  otherwise
*           the leading  n by k  part of the array  B  must contain  the
*           matrix B.
*           Unchanged on exit.
*
*  LDB    - INTEGER.
*           On entry, LDB specifies the first dimension of B as declared
*           in the calling (sub) program. When  TRANSB = 'N' or 'n' then
*           LDB must be at least  max( 1, k ), otherwise  LDB must be at
*           least  max( 1, n ).
*           Unchanged on exit.
*
*  BETA   - REAL            .
*           On entry,  BETA  specifies the scalar  beta.  When  BETA  is
*           supplied as zero then C need not be set on input.
*           Unchanged on exit.
*
*  C      - REAL             array of DIMENSION ( LDC, n ).
*           Before entry, the leading  m by n  part of the array  C must
*           contain the matrix  C,  except when  beta  is zero, in which
*           case C need not be set on entry.
*           On exit, the array  C  is overwritten by the  m by n  matrix
*           ( alpha*op( A )*op( B ) + beta*C ).
*
*  LDC    - INTEGER.
*           On entry, LDC specifies the first dimension of C as declared
*           in  the  calling  (sub)  program.   LDC  must  be  at  least
*           max( 1, m ).
*           Unchanged on exit.
*
*
*  Level 3 Blas routine.
*
*  -- Written on 8-February-1989.
*     Jack Dongarra, Argonne National Laboratory.
*     Iain Duff, AERE Harwell.
*     Jeremy Du Croz, Numerical Algorithms Group Ltd.
*     Sven Hammarling, Numerical Algorithms Group Ltd.
*
*
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
*     .. External Subroutines ..
      EXTERNAL           XERBLA
*     .. Intrinsic Functions ..
      INTRINSIC          MAX
*     .. Local Scalars ..
      LOGICAL            NOTA, NOTB
      INTEGER            I, INFO, J, L, NCOLA, NROWA, NROWB
      REAL               TEMP
*     .. Parameters ..
      REAL               ONE         , ZERO
      PARAMETER        ( ONE = 1.0E+0, ZERO = 0.0E+0 )
*     ..
*     .. Executable Statements ..
*
*     Set  NOTA  and  NOTB  as  true if  A  and  B  respectively are not
*     transposed and set  NROWA, NCOLA and  NROWB  as the number of rows
*     and  columns of  A  and the  number of  rows  of  B  respectively.
*
      NOTA  = LSAME( TRANSA, 'N' )
      NOTB  = LSAME( TRANSB, 'N' )
      IF( NOTA )THEN
         NROWA = M
         NCOLA = K
      ELSE
         NROWA = K
         NCOLA = M
      END IF
      IF( NOTB )THEN
         NROWB = K
      ELSE
         NROWB = N
      END IF
*
*     Test the input parameters.
*
      INFO = 0
      IF(      ( .NOT.NOTA                 ).AND.
     $         ( .NOT.LSAME( TRANSA, 'C' ) ).AND.
     $         ( .NOT.LSAME( TRANSA, 'T' ) )      )THEN
         INFO = 1
      ELSE IF( ( .NOT.NOTB                 ).AND.
     $         ( .NOT.LSAME( TRANSB, 'C' ) ).AND.
     $         ( .NOT.LSAME( TRANSB, 'T' ) )      )THEN
         INFO = 2
      ELSE IF( M  .LT.0               )THEN
         INFO = 3
      ELSE IF( N  .LT.0               )THEN
         INFO = 4
      ELSE IF( K  .LT.0               )THEN
         INFO = 5
      ELSE IF( LDA.LT.MAX( 1, NROWA ) )THEN
         INFO = 8
      ELSE IF( LDB.LT.MAX( 1, NROWB ) )THEN
         INFO = 10
      ELSE IF( LDC.LT.MAX( 1, M     ) )THEN
         INFO = 13
      END IF
      IF( INFO.NE.0 )THEN
         CALL XERBLA( 'SGEMM ', INFO )
         RETURN
      END IF
*
*     Quick return if possible.
*
      IF( ( M.EQ.0 ).OR.( N.EQ.0 ).OR.
     $    ( ( ( ALPHA.EQ.ZERO ).OR.( K.EQ.0 ) ).AND.( BETA.EQ.ONE ) ) )
     $   RETURN
*
*     And if  alpha.eq.zero.
*
      IF( ALPHA.EQ.ZERO )THEN
         IF( BETA.EQ.ZERO )THEN
            DO 20, J = 1, N
               DO 10, I = 1, M
                  C( I, J ) = ZERO
   10          CONTINUE
   20       CONTINUE
         ELSE
            DO 40, J = 1, N
               DO 30, I = 1, M
                  C( I, J ) = BETA*C( I, J )
   30          CONTINUE
   40       CONTINUE
         END IF
         RETURN
      END IF
*
*     Start the operations.
*
      IF( NOTB )THEN
         IF( NOTA )THEN
*
*           Form  C := alpha*A*B + beta*C.
*
            DO 90, J = 1, N
               IF( BETA.EQ.ZERO )THEN
                  DO 50, I = 1, M
                     C( I, J ) = ZERO
   50             CONTINUE
               ELSE IF( BETA.NE.ONE )THEN
                  DO 60, I = 1, M
                     C( I, J ) = BETA*C( I, J )
   60             CONTINUE
               END IF
               DO 80, L = 1, K
                  IF( B( L, J ).NE.ZERO )THEN
                     TEMP = ALPHA*B( L, J )
                     DO 70, I = 1, M
                        C( I, J ) = C( I, J ) + TEMP*A( I, L )
   70                CONTINUE
                  END IF
   80          CONTINUE
   90       CONTINUE
         ELSE
*
*           Form  C := alpha*A'*B + beta*C
*
            DO 120, J = 1, N
               DO 110, I = 1, M
                  TEMP = ZERO
                  DO 100, L = 1, K
                     TEMP = TEMP + A( L, I )*B( L, J )
  100             CONTINUE
                  IF( BETA.EQ.ZERO )THEN
                     C( I, J ) = ALPHA*TEMP
                  ELSE
                     C( I, J ) = ALPHA*TEMP + BETA*C( I, J )
                  END IF
  110          CONTINUE
  120       CONTINUE
         END IF
      ELSE
         IF( NOTA )THEN
*
*           Form  C := alpha*A*B' + beta*C
*
            DO 170, J = 1, N
               IF( BETA.EQ.ZERO )THEN
                  DO 130, I = 1, M
                     C( I, J ) = ZERO
  130             CONTINUE
               ELSE IF( BETA.NE.ONE )THEN
                  DO 140, I = 1, M
                     C( I, J ) = BETA*C( I, J )
  140             CONTINUE
               END IF
               DO 160, L = 1, K
                  IF( B( J, L ).NE.ZERO )THEN
                     TEMP = ALPHA*B( J, L )
                     DO 150, I = 1, M
                        C( I, J ) = C( I, J ) + TEMP*A( I, L )
  150                CONTINUE
                  END IF
  160          CONTINUE
  170       CONTINUE
         ELSE
*
*           Form  C := alpha*A'*B' + beta*C
*
            DO 200, J = 1, N
               DO 190, I = 1, M
                  TEMP = ZERO
                  DO 180, L = 1, K
                     TEMP = TEMP + A( L, I )*B( J, L )
  180             CONTINUE
                  IF( BETA.EQ.ZERO )THEN
                     C( I, J ) = ALPHA*TEMP
                  ELSE
                     C( I, J ) = ALPHA*TEMP + BETA*C( I, J )
                  END IF
  190          CONTINUE
  200       CONTINUE
         END IF
      END IF
*
      RETURN
*
*     End of SGEMM .
*
      END
      SUBROUTINE SGEMV ( TRANS, M, N, ALPHA, A, LDA, X, INCX,
     $                   BETA, Y, INCY )
*     .. Scalar Arguments ..
      REAL               ALPHA, BETA
      INTEGER            INCX, INCY, LDA, M, N
      CHARACTER*1        TRANS
*     .. Array Arguments ..
      REAL               A( LDA, * ), X( * ), Y( * )
*     ..
*
*  Purpose
*  =======
*
*  SGEMV  performs one of the matrix-vector operations
*
*     y := alpha*A*x + beta*y,   or   y := alpha*A'*x + beta*y,
*
*  where alpha and beta are scalars, x and y are vectors and A is an
*  m by n matrix.
*
*  Parameters
*  ==========
*
*  TRANS  - CHARACTER*1.
*           On entry, TRANS specifies the operation to be performed as
*           follows:
*
*              TRANS = 'N' or 'n'   y := alpha*A*x + beta*y.
*
*              TRANS = 'T' or 't'   y := alpha*A'*x + beta*y.
*
*              TRANS = 'C' or 'c'   y := alpha*A'*x + beta*y.
*
*           Unchanged on exit.
*
*  M      - INTEGER.
*           On entry, M specifies the number of rows of the matrix A.
*           M must be at least zero.
*           Unchanged on exit.
*
*  N      - INTEGER.
*           On entry, N specifies the number of columns of the matrix A.
*           N must be at least zero.
*           Unchanged on exit.
*
*  ALPHA  - REAL            .
*           On entry, ALPHA specifies the scalar alpha.
*           Unchanged on exit.
*
*  A      - REAL             array of DIMENSION ( LDA, n ).
*           Before entry, the leading m by n part of the array A must
*           contain the matrix of coefficients.
*           Unchanged on exit.
*
*  LDA    - INTEGER.
*           On entry, LDA specifies the first dimension of A as declared
*           in the calling (sub) program. LDA must be at least
*           max( 1, m ).
*           Unchanged on exit.
*
*  X      - REAL             array of DIMENSION at least
*           ( 1 + ( n - 1 )*abs( INCX ) ) when TRANS = 'N' or 'n'
*           and at least
*           ( 1 + ( m - 1 )*abs( INCX ) ) otherwise.
*           Before entry, the incremented array X must contain the
*           vector x.
*           Unchanged on exit.
*
*  INCX   - INTEGER.
*           On entry, INCX specifies the increment for the elements of
*           X. INCX must not be zero.
*           Unchanged on exit.
*
*  BETA   - REAL            .
*           On entry, BETA specifies the scalar beta. When BETA is
*           supplied as zero then Y need not be set on input.
*           Unchanged on exit.
*
*  Y      - REAL             array of DIMENSION at least
*           ( 1 + ( m - 1 )*abs( INCY ) ) when TRANS = 'N' or 'n'
*           and at least
*           ( 1 + ( n - 1 )*abs( INCY ) ) otherwise.
*           Before entry with BETA non-zero, the incremented array Y
*           must contain the vector y. On exit, Y is overwritten by the
*           updated vector y.
*
*  INCY   - INTEGER.
*           On entry, INCY specifies the increment for the elements of
*           Y. INCY must not be zero.
*           Unchanged on exit.
*
*
*  Level 2 Blas routine.
*
*  -- Written on 22-October-1986.
*     Jack Dongarra, Argonne National Lab.
*     Jeremy Du Croz, Nag Central Office.
*     Sven Hammarling, Nag Central Office.
*     Richard Hanson, Sandia National Labs.
*
*
*     .. Parameters ..
      REAL               ONE         , ZERO
      PARAMETER        ( ONE = 1.0E+0, ZERO = 0.0E+0 )
*     .. Local Scalars ..
      REAL               TEMP
      INTEGER            I, INFO, IX, IY, J, JX, JY, KX, KY, LENX, LENY
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
*     .. External Subroutines ..
      EXTERNAL           XERBLA
*     .. Intrinsic Functions ..
      INTRINSIC          MAX
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters.
*
      INFO = 0
      IF     ( .NOT.LSAME( TRANS, 'N' ).AND.
     $         .NOT.LSAME( TRANS, 'T' ).AND.
     $         .NOT.LSAME( TRANS, 'C' )      )THEN
         INFO = 1
      ELSE IF( M.LT.0 )THEN
         INFO = 2
      ELSE IF( N.LT.0 )THEN
         INFO = 3
      ELSE IF( LDA.LT.MAX( 1, M ) )THEN
         INFO = 6
      ELSE IF( INCX.EQ.0 )THEN
         INFO = 8
      ELSE IF( INCY.EQ.0 )THEN
         INFO = 11
      END IF
      IF( INFO.NE.0 )THEN
         CALL XERBLA( 'SGEMV ', INFO )
         RETURN
      END IF
*
*     Quick return if possible.
*
      IF( ( M.EQ.0 ).OR.( N.EQ.0 ).OR.
     $    ( ( ALPHA.EQ.ZERO ).AND.( BETA.EQ.ONE ) ) )
     $   RETURN
*
*     Set  LENX  and  LENY, the lengths of the vectors x and y, and set
*     up the start points in  X  and  Y.
*
      IF( LSAME( TRANS, 'N' ) )THEN
         LENX = N
         LENY = M
      ELSE
         LENX = M
         LENY = N
      END IF
      IF( INCX.GT.0 )THEN
         KX = 1
      ELSE
         KX = 1 - ( LENX - 1 )*INCX
      END IF
      IF( INCY.GT.0 )THEN
         KY = 1
      ELSE
         KY = 1 - ( LENY - 1 )*INCY
      END IF
*
*     Start the operations. In this version the elements of A are
*     accessed sequentially with one pass through A.
*
*     First form  y := beta*y.
*
      IF( BETA.NE.ONE )THEN
         IF( INCY.EQ.1 )THEN
            IF( BETA.EQ.ZERO )THEN
               DO 10, I = 1, LENY
                  Y( I ) = ZERO
   10          CONTINUE
            ELSE
               DO 20, I = 1, LENY
                  Y( I ) = BETA*Y( I )
   20          CONTINUE
            END IF
         ELSE
            IY = KY
            IF( BETA.EQ.ZERO )THEN
               DO 30, I = 1, LENY
                  Y( IY ) = ZERO
                  IY      = IY   + INCY
   30          CONTINUE
            ELSE
               DO 40, I = 1, LENY
                  Y( IY ) = BETA*Y( IY )
                  IY      = IY           + INCY
   40          CONTINUE
            END IF
         END IF
      END IF
      IF( ALPHA.EQ.ZERO )
     $   RETURN
      IF( LSAME( TRANS, 'N' ) )THEN
*
*        Form  y := alpha*A*x + y.
*
         JX = KX
         IF( INCY.EQ.1 )THEN
            DO 60, J = 1, N
               IF( X( JX ).NE.ZERO )THEN
                  TEMP = ALPHA*X( JX )
                  DO 50, I = 1, M
                     Y( I ) = Y( I ) + TEMP*A( I, J )
   50             CONTINUE
               END IF
               JX = JX + INCX
   60       CONTINUE
         ELSE
            DO 80, J = 1, N
               IF( X( JX ).NE.ZERO )THEN
                  TEMP = ALPHA*X( JX )
                  IY   = KY
                  DO 70, I = 1, M
                     Y( IY ) = Y( IY ) + TEMP*A( I, J )
                     IY      = IY      + INCY
   70             CONTINUE
               END IF
               JX = JX + INCX
   80       CONTINUE
         END IF
      ELSE
*
*        Form  y := alpha*A'*x + y.
*
         JY = KY
         IF( INCX.EQ.1 )THEN
            DO 100, J = 1, N
               TEMP = ZERO
               DO 90, I = 1, M
                  TEMP = TEMP + A( I, J )*X( I )
   90          CONTINUE
               Y( JY ) = Y( JY ) + ALPHA*TEMP
               JY      = JY      + INCY
  100       CONTINUE
         ELSE
            DO 120, J = 1, N
               TEMP = ZERO
               IX   = KX
               DO 110, I = 1, M
                  TEMP = TEMP + A( I, J )*X( IX )
                  IX   = IX   + INCX
  110          CONTINUE
               Y( JY ) = Y( JY ) + ALPHA*TEMP
               JY      = JY      + INCY
  120       CONTINUE
         END IF
      END IF
*
      RETURN
*
*     End of SGEMV .
*
      END
      SUBROUTINE SGER  ( M, N, ALPHA, X, INCX, Y, INCY, A, LDA )
*     .. Scalar Arguments ..
      REAL               ALPHA
      INTEGER            INCX, INCY, LDA, M, N
*     .. Array Arguments ..
      REAL               A( LDA, * ), X( * ), Y( * )
*     ..
*
*  Purpose
*  =======
*
*  SGER   performs the rank 1 operation
*
*     A := alpha*x*y' + A,
*
*  where alpha is a scalar, x is an m element vector, y is an n element
*  vector and A is an m by n matrix.
*
*  Parameters
*  ==========
*
*  M      - INTEGER.
*           On entry, M specifies the number of rows of the matrix A.
*           M must be at least zero.
*           Unchanged on exit.
*
*  N      - INTEGER.
*           On entry, N specifies the number of columns of the matrix A.
*           N must be at least zero.
*           Unchanged on exit.
*
*  ALPHA  - REAL            .
*           On entry, ALPHA specifies the scalar alpha.
*           Unchanged on exit.
*
*  X      - REAL             array of dimension at least
*           ( 1 + ( m - 1 )*abs( INCX ) ).
*           Before entry, the incremented array X must contain the m
*           element vector x.
*           Unchanged on exit.
*
*  INCX   - INTEGER.
*           On entry, INCX specifies the increment for the elements of
*           X. INCX must not be zero.
*           Unchanged on exit.
*
*  Y      - REAL             array of dimension at least
*           ( 1 + ( n - 1 )*abs( INCY ) ).
*           Before entry, the incremented array Y must contain the n
*           element vector y.
*           Unchanged on exit.
*
*  INCY   - INTEGER.
*           On entry, INCY specifies the increment for the elements of
*           Y. INCY must not be zero.
*           Unchanged on exit.
*
*  A      - REAL             array of DIMENSION ( LDA, n ).
*           Before entry, the leading m by n part of the array A must
*           contain the matrix of coefficients. On exit, A is
*           overwritten by the updated matrix.
*
*  LDA    - INTEGER.
*           On entry, LDA specifies the first dimension of A as declared
*           in the calling (sub) program. LDA must be at least
*           max( 1, m ).
*           Unchanged on exit.
*
*
*  Level 2 Blas routine.
*
*  -- Written on 22-October-1986.
*     Jack Dongarra, Argonne National Lab.
*     Jeremy Du Croz, Nag Central Office.
*     Sven Hammarling, Nag Central Office.
*     Richard Hanson, Sandia National Labs.
*
*
*     .. Parameters ..
      REAL               ZERO
      PARAMETER        ( ZERO = 0.0E+0 )
*     .. Local Scalars ..
      REAL               TEMP
      INTEGER            I, INFO, IX, J, JY, KX
*     .. External Subroutines ..
      EXTERNAL           XERBLA
*     .. Intrinsic Functions ..
      INTRINSIC          MAX
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters.
*
      INFO = 0
      IF     ( M.LT.0 )THEN
         INFO = 1
      ELSE IF( N.LT.0 )THEN
         INFO = 2
      ELSE IF( INCX.EQ.0 )THEN
         INFO = 5
      ELSE IF( INCY.EQ.0 )THEN
         INFO = 7
      ELSE IF( LDA.LT.MAX( 1, M ) )THEN
         INFO = 9
      END IF
      IF( INFO.NE.0 )THEN
         CALL XERBLA( 'SGER  ', INFO )
         RETURN
      END IF
*
*     Quick return if possible.
*
      IF( ( M.EQ.0 ).OR.( N.EQ.0 ).OR.( ALPHA.EQ.ZERO ) )
     $   RETURN
*
*     Start the operations. In this version the elements of A are
*     accessed sequentially with one pass through A.
*
      IF( INCY.GT.0 )THEN
         JY = 1
      ELSE
         JY = 1 - ( N - 1 )*INCY
      END IF
      IF( INCX.EQ.1 )THEN
         DO 20, J = 1, N
            IF( Y( JY ).NE.ZERO )THEN
               TEMP = ALPHA*Y( JY )
               DO 10, I = 1, M
                  A( I, J ) = A( I, J ) + X( I )*TEMP
   10          CONTINUE
            END IF
            JY = JY + INCY
   20    CONTINUE
      ELSE
         IF( INCX.GT.0 )THEN
            KX = 1
         ELSE
            KX = 1 - ( M - 1 )*INCX
         END IF
         DO 40, J = 1, N
            IF( Y( JY ).NE.ZERO )THEN
               TEMP = ALPHA*Y( JY )
               IX   = KX
               DO 30, I = 1, M
                  A( I, J ) = A( I, J ) + X( IX )*TEMP
                  IX        = IX        + INCX
   30          CONTINUE
            END IF
            JY = JY + INCY
   40    CONTINUE
      END IF
*
      RETURN
*
*     End of SGER  .
*
      END
      real function snrm2 ( n, sx, incx)
      integer i, incx, ix, j, n, next
      real   sx(1),  cutlo, cuthi, hitest, sum, xmax, zero, one
      data   zero, one /0.0e0, 1.0e0/
c
c     euclidean norm of the n-vector stored in sx() with storage
c     increment incx .
c     if    n .le. 0 return with result = 0.
c     if n .ge. 1 then incx must be .ge. 1
c
c           c.l.lawson, 1978 jan 08
c     modified to correct problem with negative increment, 8/21/90.
c
c     four phase method     using two built-in constants that are
c     hopefully applicable to all machines.
c         cutlo = maximum of  sqrt(u/eps)  over all known machines.
c         cuthi = minimum of  sqrt(v)      over all known machines.
c     where
c         eps = smallest no. such that eps + 1. .gt. 1.
c         u   = smallest positive no.   (underflow limit)
c         v   = largest  no.            (overflow  limit)
c
c     brief outline of algorithm..
c
c     phase 1    scans zero components.
c     move to phase 2 when a component is nonzero and .le. cutlo
c     move to phase 3 when a component is .gt. cutlo
c     move to phase 4 when a component is .ge. cuthi/m
c     where m = n for x() real and m = 2*n for complex.
c
c     values for cutlo and cuthi..
c     from the environmental parameters listed in the imsl converter
c     document the limiting values are as follows..
c     cutlo, s.p.   u/eps = 2**(-102) for  honeywell.  close seconds are
c                   univac and dec at 2**(-103)
c                   thus cutlo = 2**(-51) = 4.44089e-16
c     cuthi, s.p.   v = 2**127 for univac, honeywell, and dec.
c                   thus cuthi = 2**(63.5) = 1.30438e19
c     cutlo, d.p.   u/eps = 2**(-67) for honeywell and dec.
c                   thus cutlo = 2**(-33.5) = 8.23181d-11
c     cuthi, d.p.   same as s.p.  cuthi = 1.30438d19
c     data cutlo, cuthi / 8.232d-11,  1.304d19 /
c     data cutlo, cuthi / 4.441e-16,  1.304e19 /
c     data cutlo, cuthi / 4.441e-16,  1.304e19 /
c...  from Ed Anderson (for Cray)
c     data cutlo / 0300315520236314774737b /
c     data cuthi / 0500004000000000000000b /
c...  from Ed Anderson (for Sun4)
      data cutlo /   0.44408921E-15 /
      data cuthi /   0.18446743E+20 /
c
      if(n .gt. 0) go to 10
         snrm2  = zero
         go to 300
c
   10 assign 30 to next
      sum = zero
      i = 1
      if(incx.lt.0)i = (-n+1)*incx + 1
      ix = 1
c                                                 begin main loop
   20    go to next,(30, 50, 70, 110)
   30 if( abs(sx(i)) .gt. cutlo) go to 85
      assign 50 to next
      xmax = zero
c
c                        phase 1.  sum is zero
c
   50 if( sx(i) .eq. zero) go to 200
      if( abs(sx(i)) .gt. cutlo) go to 85
c
c                                prepare for phase 2.
      assign 70 to next
      go to 105
c
c                                prepare for phase 4.
c
  100 continue
      assign 110 to next
      sum = (sum / sx(i)) / sx(i)
  105 xmax = abs(sx(i))
      go to 115
c
c                   phase 2.  sum is small.
c                             scale to avoid destructive underflow.
c
   70 if( abs(sx(i)) .gt. cutlo ) go to 75
c
c                     common code for phases 2 and 4.
c                     in phase 4 sum is large.  scale to avoid overflow.
c
  110 if( abs(sx(i)) .le. xmax ) go to 115
         sum = one + sum * (xmax / sx(i))**2
         xmax = abs(sx(i))
         go to 200
c
  115 sum = sum + (sx(i)/xmax)**2
      go to 200
c
c
c                  prepare for phase 3.
c
   75 sum = (sum * xmax) * xmax
c
c
c     for real or d.p. set hitest = cuthi/n
c     for complex      set hitest = cuthi/(2*n)
c
   85 hitest = cuthi/float( n )
c
c                   phase 3.  sum is mid-range.  no scaling.
c
      do 95 j = ix, n
         if(abs(sx(i)) .ge. hitest) go to 100
         sum = sum + sx(i)**2
         i = i + incx
   95 continue
      snrm2 = sqrt( sum )
      go to 300
c
  200 continue
      ix = ix + 1
      i = i + incx
      if( ix .le. n ) go to 20
c
c              end of main loop.
c
c              compute square root and adjust for scaling.
c
      snrm2 = xmax * sqrt(sum)
  300 continue
      return
      end
      subroutine srot (n,sx,incx,sy,incy,c,s)
c
c     applies a plane rotation.
c     jack dongarra, linpack, 3/11/78.
c
      real sx(1),sy(1),stemp,c,s
      integer i,incx,incy,ix,iy,n
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
        stemp = c*sx(ix) + s*sy(iy)
        sy(iy) = c*sy(iy) - s*sx(ix)
        sx(ix) = stemp
        ix = ix + incx
        iy = iy + incy
   10 continue
      return
c
c       code for both increments equal to 1
c
   20 do 30 i = 1,n
        stemp = c*sx(i) + s*sy(i)
        sy(i) = c*sy(i) - s*sx(i)
        sx(i) = stemp
   30 continue
      return
      end
      subroutine srotg(sa,sb,c,s)
c
c     construct givens plane rotation.
c     jack dongarra, linpack, 3/11/78.
c
      real sa,sb,c,s,roe,scale,r,z
c
      roe = sb
      if( abs(sa) .gt. abs(sb) ) roe = sa
      scale = abs(sa) + abs(sb)
      if( scale .ne. 0.0 ) go to 10
         c = 1.0
         s = 0.0
         r = 0.0
         z = 0.0
         go to 20
   10 r = scale*sqrt((sa/scale)**2 + (sb/scale)**2)
      r = sign(1.0,roe)*r
      c = sa/r
      s = sb/r
      z = 1.0
      if( abs(sa) .gt. abs(sb) ) z = s
      if( abs(sb) .ge. abs(sa) .and. c .ne. 0.0 ) z = 1.0/c
   20 sa = r
      sb = z
      return
      end
      subroutine sscal(n,sa,sx,incx)
c
c     scales a vector by a constant.
c     uses unrolled loops for increment equal to 1.
c     jack dongarra, linpack, 3/11/78.
c     modified to correct problem with negative increment, 8/21/90.
c
      real sa,sx(1)
      integer i,incx,ix,m,mp1,n
c
      if(n.le.0)return
      if(incx.eq.1)go to 20
c
c        code for increment not equal to 1
c
      ix = 1 
      if(incx.lt.0)ix = (-n+1)*incx + 1 
      do 10 i = 1,n 
        sx(ix) = sa*sx(ix)
        ix = ix + incx 
   10 continue
      return
c
c        code for increment equal to 1
c
c
c        clean-up loop
c
   20 m = mod(n,5)
      if( m .eq. 0 ) go to 40
      do 30 i = 1,m
        sx(i) = sa*sx(i)
   30 continue
      if( n .lt. 5 ) return
   40 mp1 = m + 1
      do 50 i = mp1,n,5
        sx(i) = sa*sx(i)
        sx(i + 1) = sa*sx(i + 1)
        sx(i + 2) = sa*sx(i + 2)
        sx(i + 3) = sa*sx(i + 3)
        sx(i + 4) = sa*sx(i + 4)
   50 continue
      return
      end
      subroutine sswap (n,sx,incx,sy,incy)
c
c     interchanges two vectors.
c     uses unrolled loops for increments equal to 1.
c     jack dongarra, linpack, 3/11/78.
c
      real sx(1),sy(1),stemp
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
      SUBROUTINE SSYMV ( UPLO, N, ALPHA, A, LDA, X, INCX,
     $                   BETA, Y, INCY )
*     .. Scalar Arguments ..
      REAL               ALPHA, BETA
      INTEGER            INCX, INCY, LDA, N
      CHARACTER*1        UPLO
*     .. Array Arguments ..
      REAL               A( LDA, * ), X( * ), Y( * )
*     ..
*
*  Purpose
*  =======
*
*  SSYMV  performs the matrix-vector  operation
*
*     y := alpha*A*x + beta*y,
*
*  where alpha and beta are scalars, x and y are n element vectors and
*  A is an n by n symmetric matrix.
*
*  Parameters
*  ==========
*
*  UPLO   - CHARACTER*1.
*           On entry, UPLO specifies whether the upper or lower
*           triangular part of the array A is to be referenced as
*           follows:
*
*              UPLO = 'U' or 'u'   Only the upper triangular part of A
*                                  is to be referenced.
*
*              UPLO = 'L' or 'l'   Only the lower triangular part of A
*                                  is to be referenced.
*
*           Unchanged on exit.
*
*  N      - INTEGER.
*           On entry, N specifies the order of the matrix A.
*           N must be at least zero.
*           Unchanged on exit.
*
*  ALPHA  - REAL            .
*           On entry, ALPHA specifies the scalar alpha.
*           Unchanged on exit.
*
*  A      - REAL             array of DIMENSION ( LDA, n ).
*           Before entry with  UPLO = 'U' or 'u', the leading n by n
*           upper triangular part of the array A must contain the upper
*           triangular part of the symmetric matrix and the strictly
*           lower triangular part of A is not referenced.
*           Before entry with UPLO = 'L' or 'l', the leading n by n
*           lower triangular part of the array A must contain the lower
*           triangular part of the symmetric matrix and the strictly
*           upper triangular part of A is not referenced.
*           Unchanged on exit.
*
*  LDA    - INTEGER.
*           On entry, LDA specifies the first dimension of A as declared
*           in the calling (sub) program. LDA must be at least
*           max( 1, n ).
*           Unchanged on exit.
*
*  X      - REAL             array of dimension at least
*           ( 1 + ( n - 1 )*abs( INCX ) ).
*           Before entry, the incremented array X must contain the n
*           element vector x.
*           Unchanged on exit.
*
*  INCX   - INTEGER.
*           On entry, INCX specifies the increment for the elements of
*           X. INCX must not be zero.
*           Unchanged on exit.
*
*  BETA   - REAL            .
*           On entry, BETA specifies the scalar beta. When BETA is
*           supplied as zero then Y need not be set on input.
*           Unchanged on exit.
*
*  Y      - REAL             array of dimension at least
*           ( 1 + ( n - 1 )*abs( INCY ) ).
*           Before entry, the incremented array Y must contain the n
*           element vector y. On exit, Y is overwritten by the updated
*           vector y.
*
*  INCY   - INTEGER.
*           On entry, INCY specifies the increment for the elements of
*           Y. INCY must not be zero.
*           Unchanged on exit.
*
*
*  Level 2 Blas routine.
*
*  -- Written on 22-October-1986.
*     Jack Dongarra, Argonne National Lab.
*     Jeremy Du Croz, Nag Central Office.
*     Sven Hammarling, Nag Central Office.
*     Richard Hanson, Sandia National Labs.
*
*
*     .. Parameters ..
      REAL               ONE         , ZERO
      PARAMETER        ( ONE = 1.0E+0, ZERO = 0.0E+0 )
*     .. Local Scalars ..
      REAL               TEMP1, TEMP2
      INTEGER            I, INFO, IX, IY, J, JX, JY, KX, KY
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
*     .. External Subroutines ..
      EXTERNAL           XERBLA
*     .. Intrinsic Functions ..
      INTRINSIC          MAX
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters.
*
      INFO = 0
      IF     ( .NOT.LSAME( UPLO, 'U' ).AND.
     $         .NOT.LSAME( UPLO, 'L' )      )THEN
         INFO = 1
      ELSE IF( N.LT.0 )THEN
         INFO = 2
      ELSE IF( LDA.LT.MAX( 1, N ) )THEN
         INFO = 5
      ELSE IF( INCX.EQ.0 )THEN
         INFO = 7
      ELSE IF( INCY.EQ.0 )THEN
         INFO = 10
      END IF
      IF( INFO.NE.0 )THEN
         CALL XERBLA( 'SSYMV ', INFO )
         RETURN
      END IF
*
*     Quick return if possible.
*
      IF( ( N.EQ.0 ).OR.( ( ALPHA.EQ.ZERO ).AND.( BETA.EQ.ONE ) ) )
     $   RETURN
*
*     Set up the start points in  X  and  Y.
*
      IF( INCX.GT.0 )THEN
         KX = 1
      ELSE
         KX = 1 - ( N - 1 )*INCX
      END IF
      IF( INCY.GT.0 )THEN
         KY = 1
      ELSE
         KY = 1 - ( N - 1 )*INCY
      END IF
*
*     Start the operations. In this version the elements of A are
*     accessed sequentially with one pass through the triangular part
*     of A.
*
*     First form  y := beta*y.
*
      IF( BETA.NE.ONE )THEN
         IF( INCY.EQ.1 )THEN
            IF( BETA.EQ.ZERO )THEN
               DO 10, I = 1, N
                  Y( I ) = ZERO
   10          CONTINUE
            ELSE
               DO 20, I = 1, N
                  Y( I ) = BETA*Y( I )
   20          CONTINUE
            END IF
         ELSE
            IY = KY
            IF( BETA.EQ.ZERO )THEN
               DO 30, I = 1, N
                  Y( IY ) = ZERO
                  IY      = IY   + INCY
   30          CONTINUE
            ELSE
               DO 40, I = 1, N
                  Y( IY ) = BETA*Y( IY )
                  IY      = IY           + INCY
   40          CONTINUE
            END IF
         END IF
      END IF
      IF( ALPHA.EQ.ZERO )
     $   RETURN
      IF( LSAME( UPLO, 'U' ) )THEN
*
*        Form  y  when A is stored in upper triangle.
*
         IF( ( INCX.EQ.1 ).AND.( INCY.EQ.1 ) )THEN
            DO 60, J = 1, N
               TEMP1 = ALPHA*X( J )
               TEMP2 = ZERO
               DO 50, I = 1, J - 1
                  Y( I ) = Y( I ) + TEMP1*A( I, J )
                  TEMP2  = TEMP2  + A( I, J )*X( I )
   50          CONTINUE
               Y( J ) = Y( J ) + TEMP1*A( J, J ) + ALPHA*TEMP2
   60       CONTINUE
         ELSE
            JX = KX
            JY = KY
            DO 80, J = 1, N
               TEMP1 = ALPHA*X( JX )
               TEMP2 = ZERO
               IX    = KX
               IY    = KY
               DO 70, I = 1, J - 1
                  Y( IY ) = Y( IY ) + TEMP1*A( I, J )
                  TEMP2   = TEMP2   + A( I, J )*X( IX )
                  IX      = IX      + INCX
                  IY      = IY      + INCY
   70          CONTINUE
               Y( JY ) = Y( JY ) + TEMP1*A( J, J ) + ALPHA*TEMP2
               JX      = JX      + INCX
               JY      = JY      + INCY
   80       CONTINUE
         END IF
      ELSE
*
*        Form  y  when A is stored in lower triangle.
*
         IF( ( INCX.EQ.1 ).AND.( INCY.EQ.1 ) )THEN
            DO 100, J = 1, N
               TEMP1  = ALPHA*X( J )
               TEMP2  = ZERO
               Y( J ) = Y( J )       + TEMP1*A( J, J )
               DO 90, I = J + 1, N
                  Y( I ) = Y( I ) + TEMP1*A( I, J )
                  TEMP2  = TEMP2  + A( I, J )*X( I )
   90          CONTINUE
               Y( J ) = Y( J ) + ALPHA*TEMP2
  100       CONTINUE
         ELSE
            JX = KX
            JY = KY
            DO 120, J = 1, N
               TEMP1   = ALPHA*X( JX )
               TEMP2   = ZERO
               Y( JY ) = Y( JY )       + TEMP1*A( J, J )
               IX      = JX
               IY      = JY
               DO 110, I = J + 1, N
                  IX      = IX      + INCX
                  IY      = IY      + INCY
                  Y( IY ) = Y( IY ) + TEMP1*A( I, J )
                  TEMP2   = TEMP2   + A( I, J )*X( IX )
  110          CONTINUE
               Y( JY ) = Y( JY ) + ALPHA*TEMP2
               JX      = JX      + INCX
               JY      = JY      + INCY
  120       CONTINUE
         END IF
      END IF
*
      RETURN
*
*     End of SSYMV .
*
      END
      SUBROUTINE SSYR2 ( UPLO, N, ALPHA, X, INCX, Y, INCY, A, LDA )
*     .. Scalar Arguments ..
      REAL               ALPHA
      INTEGER            INCX, INCY, LDA, N
      CHARACTER*1        UPLO
*     .. Array Arguments ..
      REAL               A( LDA, * ), X( * ), Y( * )
*     ..
*
*  Purpose
*  =======
*
*  SSYR2  performs the symmetric rank 2 operation
*
*     A := alpha*x*y' + alpha*y*x' + A,
*
*  where alpha is a scalar, x and y are n element vectors and A is an n
*  by n symmetric matrix.
*
*  Parameters
*  ==========
*
*  UPLO   - CHARACTER*1.
*           On entry, UPLO specifies whether the upper or lower
*           triangular part of the array A is to be referenced as
*           follows:
*
*              UPLO = 'U' or 'u'   Only the upper triangular part of A
*                                  is to be referenced.
*
*              UPLO = 'L' or 'l'   Only the lower triangular part of A
*                                  is to be referenced.
*
*           Unchanged on exit.
*
*  N      - INTEGER.
*           On entry, N specifies the order of the matrix A.
*           N must be at least zero.
*           Unchanged on exit.
*
*  ALPHA  - REAL            .
*           On entry, ALPHA specifies the scalar alpha.
*           Unchanged on exit.
*
*  X      - REAL             array of dimension at least
*           ( 1 + ( n - 1 )*abs( INCX ) ).
*           Before entry, the incremented array X must contain the n
*           element vector x.
*           Unchanged on exit.
*
*  INCX   - INTEGER.
*           On entry, INCX specifies the increment for the elements of
*           X. INCX must not be zero.
*           Unchanged on exit.
*
*  Y      - REAL             array of dimension at least
*           ( 1 + ( n - 1 )*abs( INCY ) ).
*           Before entry, the incremented array Y must contain the n
*           element vector y.
*           Unchanged on exit.
*
*  INCY   - INTEGER.
*           On entry, INCY specifies the increment for the elements of
*           Y. INCY must not be zero.
*           Unchanged on exit.
*
*  A      - REAL             array of DIMENSION ( LDA, n ).
*           Before entry with  UPLO = 'U' or 'u', the leading n by n
*           upper triangular part of the array A must contain the upper
*           triangular part of the symmetric matrix and the strictly
*           lower triangular part of A is not referenced. On exit, the
*           upper triangular part of the array A is overwritten by the
*           upper triangular part of the updated matrix.
*           Before entry with UPLO = 'L' or 'l', the leading n by n
*           lower triangular part of the array A must contain the lower
*           triangular part of the symmetric matrix and the strictly
*           upper triangular part of A is not referenced. On exit, the
*           lower triangular part of the array A is overwritten by the
*           lower triangular part of the updated matrix.
*
*  LDA    - INTEGER.
*           On entry, LDA specifies the first dimension of A as declared
*           in the calling (sub) program. LDA must be at least
*           max( 1, n ).
*           Unchanged on exit.
*
*
*  Level 2 Blas routine.
*
*  -- Written on 22-October-1986.
*     Jack Dongarra, Argonne National Lab.
*     Jeremy Du Croz, Nag Central Office.
*     Sven Hammarling, Nag Central Office.
*     Richard Hanson, Sandia National Labs.
*
*
*     .. Parameters ..
      REAL               ZERO
      PARAMETER        ( ZERO = 0.0E+0 )
*     .. Local Scalars ..
      REAL               TEMP1, TEMP2
      INTEGER            I, INFO, IX, IY, J, JX, JY, KX, KY
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
*     .. External Subroutines ..
      EXTERNAL           XERBLA
*     .. Intrinsic Functions ..
      INTRINSIC          MAX
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters.
*
      INFO = 0
      IF     ( .NOT.LSAME( UPLO, 'U' ).AND.
     $         .NOT.LSAME( UPLO, 'L' )      )THEN
         INFO = 1
      ELSE IF( N.LT.0 )THEN
         INFO = 2
      ELSE IF( INCX.EQ.0 )THEN
         INFO = 5
      ELSE IF( INCY.EQ.0 )THEN
         INFO = 7
      ELSE IF( LDA.LT.MAX( 1, N ) )THEN
         INFO = 9
      END IF
      IF( INFO.NE.0 )THEN
         CALL XERBLA( 'SSYR2 ', INFO )
         RETURN
      END IF
*
*     Quick return if possible.
*
      IF( ( N.EQ.0 ).OR.( ALPHA.EQ.ZERO ) )
     $   RETURN
*
*     Set up the start points in X and Y if the increments are not both
*     unity.
*
      IF( ( INCX.NE.1 ).OR.( INCY.NE.1 ) )THEN
         IF( INCX.GT.0 )THEN
            KX = 1
         ELSE
            KX = 1 - ( N - 1 )*INCX
         END IF
         IF( INCY.GT.0 )THEN
            KY = 1
         ELSE
            KY = 1 - ( N - 1 )*INCY
         END IF
         JX = KX
         JY = KY
      END IF
*
*     Start the operations. In this version the elements of A are
*     accessed sequentially with one pass through the triangular part
*     of A.
*
      IF( LSAME( UPLO, 'U' ) )THEN
*
*        Form  A  when A is stored in the upper triangle.
*
         IF( ( INCX.EQ.1 ).AND.( INCY.EQ.1 ) )THEN
            DO 20, J = 1, N
               IF( ( X( J ).NE.ZERO ).OR.( Y( J ).NE.ZERO ) )THEN
                  TEMP1 = ALPHA*Y( J )
                  TEMP2 = ALPHA*X( J )
                  DO 10, I = 1, J
                     A( I, J ) = A( I, J ) + X( I )*TEMP1 + Y( I )*TEMP2
   10             CONTINUE
               END IF
   20       CONTINUE
         ELSE
            DO 40, J = 1, N
               IF( ( X( JX ).NE.ZERO ).OR.( Y( JY ).NE.ZERO ) )THEN
                  TEMP1 = ALPHA*Y( JY )
                  TEMP2 = ALPHA*X( JX )
                  IX    = KX
                  IY    = KY
                  DO 30, I = 1, J
                     A( I, J ) = A( I, J ) + X( IX )*TEMP1
     $                                     + Y( IY )*TEMP2
                     IX        = IX        + INCX
                     IY        = IY        + INCY
   30             CONTINUE
               END IF
               JX = JX + INCX
               JY = JY + INCY
   40       CONTINUE
         END IF
      ELSE
*
*        Form  A  when A is stored in the lower triangle.
*
         IF( ( INCX.EQ.1 ).AND.( INCY.EQ.1 ) )THEN
            DO 60, J = 1, N
               IF( ( X( J ).NE.ZERO ).OR.( Y( J ).NE.ZERO ) )THEN
                  TEMP1 = ALPHA*Y( J )
                  TEMP2 = ALPHA*X( J )
                  DO 50, I = J, N
                     A( I, J ) = A( I, J ) + X( I )*TEMP1 + Y( I )*TEMP2
   50             CONTINUE
               END IF
   60       CONTINUE
         ELSE
            DO 80, J = 1, N
               IF( ( X( JX ).NE.ZERO ).OR.( Y( JY ).NE.ZERO ) )THEN
                  TEMP1 = ALPHA*Y( JY )
                  TEMP2 = ALPHA*X( JX )
                  IX    = JX
                  IY    = JY
                  DO 70, I = J, N
                     A( I, J ) = A( I, J ) + X( IX )*TEMP1
     $                                     + Y( IY )*TEMP2
                     IX        = IX        + INCX
                     IY        = IY        + INCY
   70             CONTINUE
               END IF
               JX = JX + INCX
               JY = JY + INCY
   80       CONTINUE
         END IF
      END IF
*
      RETURN
*
*     End of SSYR2 .
*
      END
      SUBROUTINE STBSV ( UPLO, TRANS, DIAG, N, K, A, LDA, X, INCX )
*     .. Scalar Arguments ..
      INTEGER            INCX, K, LDA, N
      CHARACTER*1        DIAG, TRANS, UPLO
*     .. Array Arguments ..
      REAL               A( LDA, * ), X( * )
*     ..
*
*  Purpose
*  =======
*
*  STBSV  solves one of the systems of equations
*
*     A*x = b,   or   A'*x = b,
*
*  where b and x are n element vectors and A is an n by n unit, or
*  non-unit, upper or lower triangular band matrix, with ( k + 1 )
*  diagonals.
*
*  No test for singularity or near-singularity is included in this
*  routine. Such tests must be performed before calling this routine.
*
*  Parameters
*  ==========
*
*  UPLO   - CHARACTER*1.
*           On entry, UPLO specifies whether the matrix is an upper or
*           lower triangular matrix as follows:
*
*              UPLO = 'U' or 'u'   A is an upper triangular matrix.
*
*              UPLO = 'L' or 'l'   A is a lower triangular matrix.
*
*           Unchanged on exit.
*
*  TRANS  - CHARACTER*1.
*           On entry, TRANS specifies the equations to be solved as
*           follows:
*
*              TRANS = 'N' or 'n'   A*x = b.
*
*              TRANS = 'T' or 't'   A'*x = b.
*
*              TRANS = 'C' or 'c'   A'*x = b.
*
*           Unchanged on exit.
*
*  DIAG   - CHARACTER*1.
*           On entry, DIAG specifies whether or not A is unit
*           triangular as follows:
*
*              DIAG = 'U' or 'u'   A is assumed to be unit triangular.
*
*              DIAG = 'N' or 'n'   A is not assumed to be unit
*                                  triangular.
*
*           Unchanged on exit.
*
*  N      - INTEGER.
*           On entry, N specifies the order of the matrix A.
*           N must be at least zero.
*           Unchanged on exit.
*
*  K      - INTEGER.
*           On entry with UPLO = 'U' or 'u', K specifies the number of
*           super-diagonals of the matrix A.
*           On entry with UPLO = 'L' or 'l', K specifies the number of
*           sub-diagonals of the matrix A.
*           K must satisfy  0 .le. K.
*           Unchanged on exit.
*
*  A      - REAL             array of DIMENSION ( LDA, n ).
*           Before entry with UPLO = 'U' or 'u', the leading ( k + 1 )
*           by n part of the array A must contain the upper triangular
*           band part of the matrix of coefficients, supplied column by
*           column, with the leading diagonal of the matrix in row
*           ( k + 1 ) of the array, the first super-diagonal starting at
*           position 2 in row k, and so on. The top left k by k triangle
*           of the array A is not referenced.
*           The following program segment will transfer an upper
*           triangular band matrix from conventional full matrix storage
*           to band storage:
*
*                 DO 20, J = 1, N
*                    M = K + 1 - J
*                    DO 10, I = MAX( 1, J - K ), J
*                       A( M + I, J ) = matrix( I, J )
*              10    CONTINUE
*              20 CONTINUE
*
*           Before entry with UPLO = 'L' or 'l', the leading ( k + 1 )
*           by n part of the array A must contain the lower triangular
*           band part of the matrix of coefficients, supplied column by
*           column, with the leading diagonal of the matrix in row 1 of
*           the array, the first sub-diagonal starting at position 1 in
*           row 2, and so on. The bottom right k by k triangle of the
*           array A is not referenced.
*           The following program segment will transfer a lower
*           triangular band matrix from conventional full matrix storage
*           to band storage:
*
*                 DO 20, J = 1, N
*                    M = 1 - J
*                    DO 10, I = J, MIN( N, J + K )
*                       A( M + I, J ) = matrix( I, J )
*              10    CONTINUE
*              20 CONTINUE
*
*           Note that when DIAG = 'U' or 'u' the elements of the array A
*           corresponding to the diagonal elements of the matrix are not
*           referenced, but are assumed to be unity.
*           Unchanged on exit.
*
*  LDA    - INTEGER.
*           On entry, LDA specifies the first dimension of A as declared
*           in the calling (sub) program. LDA must be at least
*           ( k + 1 ).
*           Unchanged on exit.
*
*  X      - REAL             array of dimension at least
*           ( 1 + ( n - 1 )*abs( INCX ) ).
*           Before entry, the incremented array X must contain the n
*           element right-hand side vector b. On exit, X is overwritten
*           with the solution vector x.
*
*  INCX   - INTEGER.
*           On entry, INCX specifies the increment for the elements of
*           X. INCX must not be zero.
*           Unchanged on exit.
*
*
*  Level 2 Blas routine.
*
*  -- Written on 22-October-1986.
*     Jack Dongarra, Argonne National Lab.
*     Jeremy Du Croz, Nag Central Office.
*     Sven Hammarling, Nag Central Office.
*     Richard Hanson, Sandia National Labs.
*
*
*     .. Parameters ..
      REAL               ZERO
      PARAMETER        ( ZERO = 0.0E+0 )
*     .. Local Scalars ..
      REAL               TEMP
      INTEGER            I, INFO, IX, J, JX, KPLUS1, KX, L
      LOGICAL            NOUNIT
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
*     .. External Subroutines ..
      EXTERNAL           XERBLA
*     .. Intrinsic Functions ..
      INTRINSIC          MAX, MIN
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters.
*
      INFO = 0
      IF     ( .NOT.LSAME( UPLO , 'U' ).AND.
     $         .NOT.LSAME( UPLO , 'L' )      )THEN
         INFO = 1
      ELSE IF( .NOT.LSAME( TRANS, 'N' ).AND.
     $         .NOT.LSAME( TRANS, 'T' ).AND.
     $         .NOT.LSAME( TRANS, 'C' )      )THEN
         INFO = 2
      ELSE IF( .NOT.LSAME( DIAG , 'U' ).AND.
     $         .NOT.LSAME( DIAG , 'N' )      )THEN
         INFO = 3
      ELSE IF( N.LT.0 )THEN
         INFO = 4
      ELSE IF( K.LT.0 )THEN
         INFO = 5
      ELSE IF( LDA.LT.( K + 1 ) )THEN
         INFO = 7
      ELSE IF( INCX.EQ.0 )THEN
         INFO = 9
      END IF
      IF( INFO.NE.0 )THEN
         CALL XERBLA( 'STBSV ', INFO )
         RETURN
      END IF
*
*     Quick return if possible.
*
      IF( N.EQ.0 )
     $   RETURN
*
      NOUNIT = LSAME( DIAG, 'N' )
*
*     Set up the start point in X if the increment is not unity. This
*     will be  ( N - 1 )*INCX  too small for descending loops.
*
      IF( INCX.LE.0 )THEN
         KX = 1 - ( N - 1 )*INCX
      ELSE IF( INCX.NE.1 )THEN
         KX = 1
      END IF
*
*     Start the operations. In this version the elements of A are
*     accessed by sequentially with one pass through A.
*
      IF( LSAME( TRANS, 'N' ) )THEN
*
*        Form  x := inv( A )*x.
*
         IF( LSAME( UPLO, 'U' ) )THEN
            KPLUS1 = K + 1
            IF( INCX.EQ.1 )THEN
               DO 20, J = N, 1, -1
                  IF( X( J ).NE.ZERO )THEN
                     L = KPLUS1 - J
                     IF( NOUNIT )
     $                  X( J ) = X( J )/A( KPLUS1, J )
                     TEMP = X( J )
                     DO 10, I = J - 1, MAX( 1, J - K ), -1
                        X( I ) = X( I ) - TEMP*A( L + I, J )
   10                CONTINUE
                  END IF
   20          CONTINUE
            ELSE
               KX = KX + ( N - 1 )*INCX
               JX = KX
               DO 40, J = N, 1, -1
                  KX = KX - INCX
                  IF( X( JX ).NE.ZERO )THEN
                     IX = KX
                     L  = KPLUS1 - J
                     IF( NOUNIT )
     $                  X( JX ) = X( JX )/A( KPLUS1, J )
                     TEMP = X( JX )
                     DO 30, I = J - 1, MAX( 1, J - K ), -1
                        X( IX ) = X( IX ) - TEMP*A( L + I, J )
                        IX      = IX      - INCX
   30                CONTINUE
                  END IF
                  JX = JX - INCX
   40          CONTINUE
            END IF
         ELSE
            IF( INCX.EQ.1 )THEN
               DO 60, J = 1, N
                  IF( X( J ).NE.ZERO )THEN
                     L = 1 - J
                     IF( NOUNIT )
     $                  X( J ) = X( J )/A( 1, J )
                     TEMP = X( J )
                     DO 50, I = J + 1, MIN( N, J + K )
                        X( I ) = X( I ) - TEMP*A( L + I, J )
   50                CONTINUE
                  END IF
   60          CONTINUE
            ELSE
               JX = KX
               DO 80, J = 1, N
                  KX = KX + INCX
                  IF( X( JX ).NE.ZERO )THEN
                     IX = KX
                     L  = 1  - J
                     IF( NOUNIT )
     $                  X( JX ) = X( JX )/A( 1, J )
                     TEMP = X( JX )
                     DO 70, I = J + 1, MIN( N, J + K )
                        X( IX ) = X( IX ) - TEMP*A( L + I, J )
                        IX      = IX      + INCX
   70                CONTINUE
                  END IF
                  JX = JX + INCX
   80          CONTINUE
            END IF
         END IF
      ELSE
*
*        Form  x := inv( A')*x.
*
         IF( LSAME( UPLO, 'U' ) )THEN
            KPLUS1 = K + 1
            IF( INCX.EQ.1 )THEN
               DO 100, J = 1, N
                  TEMP = X( J )
                  L    = KPLUS1 - J
                  DO 90, I = MAX( 1, J - K ), J - 1
                     TEMP = TEMP - A( L + I, J )*X( I )
   90             CONTINUE
                  IF( NOUNIT )
     $               TEMP = TEMP/A( KPLUS1, J )
                  X( J ) = TEMP
  100          CONTINUE
            ELSE
               JX = KX
               DO 120, J = 1, N
                  TEMP = X( JX )
                  IX   = KX
                  L    = KPLUS1  - J
                  DO 110, I = MAX( 1, J - K ), J - 1
                     TEMP = TEMP - A( L + I, J )*X( IX )
                     IX   = IX   + INCX
  110             CONTINUE
                  IF( NOUNIT )
     $               TEMP = TEMP/A( KPLUS1, J )
                  X( JX ) = TEMP
                  JX      = JX   + INCX
                  IF( J.GT.K )
     $               KX = KX + INCX
  120          CONTINUE
            END IF
         ELSE
            IF( INCX.EQ.1 )THEN
               DO 140, J = N, 1, -1
                  TEMP = X( J )
                  L    = 1      - J
                  DO 130, I = MIN( N, J + K ), J + 1, -1
                     TEMP = TEMP - A( L + I, J )*X( I )
  130             CONTINUE
                  IF( NOUNIT )
     $               TEMP = TEMP/A( 1, J )
                  X( J ) = TEMP
  140          CONTINUE
            ELSE
               KX = KX + ( N - 1 )*INCX
               JX = KX
               DO 160, J = N, 1, -1
                  TEMP = X( JX )
                  IX   = KX
                  L    = 1       - J
                  DO 150, I = MIN( N, J + K ), J + 1, -1
                     TEMP = TEMP - A( L + I, J )*X( IX )
                     IX   = IX   - INCX
  150             CONTINUE
                  IF( NOUNIT )
     $               TEMP = TEMP/A( 1, J )
                  X( JX ) = TEMP
                  JX      = JX   - INCX
                  IF( ( N - J ).GE.K )
     $               KX = KX - INCX
  160          CONTINUE
            END IF
         END IF
      END IF
*
      RETURN
*
*     End of STBSV .
*
      END
      SUBROUTINE STRMM ( SIDE, UPLO, TRANSA, DIAG, M, N, ALPHA, A, LDA,
     $                   B, LDB )
*     .. Scalar Arguments ..
      CHARACTER*1        SIDE, UPLO, TRANSA, DIAG
      INTEGER            M, N, LDA, LDB
      REAL               ALPHA
*     .. Array Arguments ..
      REAL               A( LDA, * ), B( LDB, * )
*     ..
*
*  Purpose
*  =======
*
*  STRMM  performs one of the matrix-matrix operations
*
*     B := alpha*op( A )*B,   or   B := alpha*B*op( A ),
*
*  where  alpha  is a scalar,  B  is an m by n matrix,  A  is a unit, or
*  non-unit,  upper or lower triangular matrix  and  op( A )  is one  of
*
*     op( A ) = A   or   op( A ) = A'.
*
*  Parameters
*  ==========
*
*  SIDE   - CHARACTER*1.
*           On entry,  SIDE specifies whether  op( A ) multiplies B from
*           the left or right as follows:
*
*              SIDE = 'L' or 'l'   B := alpha*op( A )*B.
*
*              SIDE = 'R' or 'r'   B := alpha*B*op( A ).
*
*           Unchanged on exit.
*
*  UPLO   - CHARACTER*1.
*           On entry, UPLO specifies whether the matrix A is an upper or
*           lower triangular matrix as follows:
*
*              UPLO = 'U' or 'u'   A is an upper triangular matrix.
*
*              UPLO = 'L' or 'l'   A is a lower triangular matrix.
*
*           Unchanged on exit.
*
*  TRANSA - CHARACTER*1.
*           On entry, TRANSA specifies the form of op( A ) to be used in
*           the matrix multiplication as follows:
*
*              TRANSA = 'N' or 'n'   op( A ) = A.
*
*              TRANSA = 'T' or 't'   op( A ) = A'.
*
*              TRANSA = 'C' or 'c'   op( A ) = A'.
*
*           Unchanged on exit.
*
*  DIAG   - CHARACTER*1.
*           On entry, DIAG specifies whether or not A is unit triangular
*           as follows:
*
*              DIAG = 'U' or 'u'   A is assumed to be unit triangular.
*
*              DIAG = 'N' or 'n'   A is not assumed to be unit
*                                  triangular.
*
*           Unchanged on exit.
*
*  M      - INTEGER.
*           On entry, M specifies the number of rows of B. M must be at
*           least zero.
*           Unchanged on exit.
*
*  N      - INTEGER.
*           On entry, N specifies the number of columns of B.  N must be
*           at least zero.
*           Unchanged on exit.
*
*  ALPHA  - REAL            .
*           On entry,  ALPHA specifies the scalar  alpha. When  alpha is
*           zero then  A is not referenced and  B need not be set before
*           entry.
*           Unchanged on exit.
*
*  A      - REAL             array of DIMENSION ( LDA, k ), where k is m
*           when  SIDE = 'L' or 'l'  and is  n  when  SIDE = 'R' or 'r'.
*           Before entry  with  UPLO = 'U' or 'u',  the  leading  k by k
*           upper triangular part of the array  A must contain the upper
*           triangular matrix  and the strictly lower triangular part of
*           A is not referenced.
*           Before entry  with  UPLO = 'L' or 'l',  the  leading  k by k
*           lower triangular part of the array  A must contain the lower
*           triangular matrix  and the strictly upper triangular part of
*           A is not referenced.
*           Note that when  DIAG = 'U' or 'u',  the diagonal elements of
*           A  are not referenced either,  but are assumed to be  unity.
*           Unchanged on exit.
*
*  LDA    - INTEGER.
*           On entry, LDA specifies the first dimension of A as declared
*           in the calling (sub) program.  When  SIDE = 'L' or 'l'  then
*           LDA  must be at least  max( 1, m ),  when  SIDE = 'R' or 'r'
*           then LDA must be at least max( 1, n ).
*           Unchanged on exit.
*
*  B      - REAL             array of DIMENSION ( LDB, n ).
*           Before entry,  the leading  m by n part of the array  B must
*           contain the matrix  B,  and  on exit  is overwritten  by the
*           transformed matrix.
*
*  LDB    - INTEGER.
*           On entry, LDB specifies the first dimension of B as declared
*           in  the  calling  (sub)  program.   LDB  must  be  at  least
*           max( 1, m ).
*           Unchanged on exit.
*
*
*  Level 3 Blas routine.
*
*  -- Written on 8-February-1989.
*     Jack Dongarra, Argonne National Laboratory.
*     Iain Duff, AERE Harwell.
*     Jeremy Du Croz, Numerical Algorithms Group Ltd.
*     Sven Hammarling, Numerical Algorithms Group Ltd.
*
*
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
*     .. External Subroutines ..
      EXTERNAL           XERBLA
*     .. Intrinsic Functions ..
      INTRINSIC          MAX
*     .. Local Scalars ..
      LOGICAL            LSIDE, NOUNIT, UPPER
      INTEGER            I, INFO, J, K, NROWA
      REAL               TEMP
*     .. Parameters ..
      REAL               ONE         , ZERO
      PARAMETER        ( ONE = 1.0E+0, ZERO = 0.0E+0 )
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters.
*
      LSIDE  = LSAME( SIDE  , 'L' )
      IF( LSIDE )THEN
         NROWA = M
      ELSE
         NROWA = N
      END IF
      NOUNIT = LSAME( DIAG  , 'N' )
      UPPER  = LSAME( UPLO  , 'U' )
*
      INFO   = 0
      IF(      ( .NOT.LSIDE                ).AND.
     $         ( .NOT.LSAME( SIDE  , 'R' ) )      )THEN
         INFO = 1
      ELSE IF( ( .NOT.UPPER                ).AND.
     $         ( .NOT.LSAME( UPLO  , 'L' ) )      )THEN
         INFO = 2
      ELSE IF( ( .NOT.LSAME( TRANSA, 'N' ) ).AND.
     $         ( .NOT.LSAME( TRANSA, 'T' ) ).AND.
     $         ( .NOT.LSAME( TRANSA, 'C' ) )      )THEN
         INFO = 3
      ELSE IF( ( .NOT.LSAME( DIAG  , 'U' ) ).AND.
     $         ( .NOT.LSAME( DIAG  , 'N' ) )      )THEN
         INFO = 4
      ELSE IF( M  .LT.0               )THEN
         INFO = 5
      ELSE IF( N  .LT.0               )THEN
         INFO = 6
      ELSE IF( LDA.LT.MAX( 1, NROWA ) )THEN
         INFO = 9
      ELSE IF( LDB.LT.MAX( 1, M     ) )THEN
         INFO = 11
      END IF
      IF( INFO.NE.0 )THEN
         CALL XERBLA( 'STRMM ', INFO )
         RETURN
      END IF
*
*     Quick return if possible.
*
      IF( N.EQ.0 )
     $   RETURN
*
*     And when  alpha.eq.zero.
*
      IF( ALPHA.EQ.ZERO )THEN
         DO 20, J = 1, N
            DO 10, I = 1, M
               B( I, J ) = ZERO
   10       CONTINUE
   20    CONTINUE
         RETURN
      END IF
*
*     Start the operations.
*
      IF( LSIDE )THEN
         IF( LSAME( TRANSA, 'N' ) )THEN
*
*           Form  B := alpha*A*B.
*
            IF( UPPER )THEN
               DO 50, J = 1, N
                  DO 40, K = 1, M
                     IF( B( K, J ).NE.ZERO )THEN
                        TEMP = ALPHA*B( K, J )
                        DO 30, I = 1, K - 1
                           B( I, J ) = B( I, J ) + TEMP*A( I, K )
   30                   CONTINUE
                        IF( NOUNIT )
     $                     TEMP = TEMP*A( K, K )
                        B( K, J ) = TEMP
                     END IF
   40             CONTINUE
   50          CONTINUE
            ELSE
               DO 80, J = 1, N
                  DO 70 K = M, 1, -1
                     IF( B( K, J ).NE.ZERO )THEN
                        TEMP      = ALPHA*B( K, J )
                        B( K, J ) = TEMP
                        IF( NOUNIT )
     $                     B( K, J ) = B( K, J )*A( K, K )
                        DO 60, I = K + 1, M
                           B( I, J ) = B( I, J ) + TEMP*A( I, K )
   60                   CONTINUE
                     END IF
   70             CONTINUE
   80          CONTINUE
            END IF
         ELSE
*
*           Form  B := alpha*B*A'.
*
            IF( UPPER )THEN
               DO 110, J = 1, N
                  DO 100, I = M, 1, -1
                     TEMP = B( I, J )
                     IF( NOUNIT )
     $                  TEMP = TEMP*A( I, I )
                     DO 90, K = 1, I - 1
                        TEMP = TEMP + A( K, I )*B( K, J )
   90                CONTINUE
                     B( I, J ) = ALPHA*TEMP
  100             CONTINUE
  110          CONTINUE
            ELSE
               DO 140, J = 1, N
                  DO 130, I = 1, M
                     TEMP = B( I, J )
                     IF( NOUNIT )
     $                  TEMP = TEMP*A( I, I )
                     DO 120, K = I + 1, M
                        TEMP = TEMP + A( K, I )*B( K, J )
  120                CONTINUE
                     B( I, J ) = ALPHA*TEMP
  130             CONTINUE
  140          CONTINUE
            END IF
         END IF
      ELSE
         IF( LSAME( TRANSA, 'N' ) )THEN
*
*           Form  B := alpha*B*A.
*
            IF( UPPER )THEN
               DO 180, J = N, 1, -1
                  TEMP = ALPHA
                  IF( NOUNIT )
     $               TEMP = TEMP*A( J, J )
                  DO 150, I = 1, M
                     B( I, J ) = TEMP*B( I, J )
  150             CONTINUE
                  DO 170, K = 1, J - 1
                     IF( A( K, J ).NE.ZERO )THEN
                        TEMP = ALPHA*A( K, J )
                        DO 160, I = 1, M
                           B( I, J ) = B( I, J ) + TEMP*B( I, K )
  160                   CONTINUE
                     END IF
  170             CONTINUE
  180          CONTINUE
            ELSE
               DO 220, J = 1, N
                  TEMP = ALPHA
                  IF( NOUNIT )
     $               TEMP = TEMP*A( J, J )
                  DO 190, I = 1, M
                     B( I, J ) = TEMP*B( I, J )
  190             CONTINUE
                  DO 210, K = J + 1, N
                     IF( A( K, J ).NE.ZERO )THEN
                        TEMP = ALPHA*A( K, J )
                        DO 200, I = 1, M
                           B( I, J ) = B( I, J ) + TEMP*B( I, K )
  200                   CONTINUE
                     END IF
  210             CONTINUE
  220          CONTINUE
            END IF
         ELSE
*
*           Form  B := alpha*B*A'.
*
            IF( UPPER )THEN
               DO 260, K = 1, N
                  DO 240, J = 1, K - 1
                     IF( A( J, K ).NE.ZERO )THEN
                        TEMP = ALPHA*A( J, K )
                        DO 230, I = 1, M
                           B( I, J ) = B( I, J ) + TEMP*B( I, K )
  230                   CONTINUE
                     END IF
  240             CONTINUE
                  TEMP = ALPHA
                  IF( NOUNIT )
     $               TEMP = TEMP*A( K, K )
                  IF( TEMP.NE.ONE )THEN
                     DO 250, I = 1, M
                        B( I, K ) = TEMP*B( I, K )
  250                CONTINUE
                  END IF
  260          CONTINUE
            ELSE
               DO 300, K = N, 1, -1
                  DO 280, J = K + 1, N
                     IF( A( J, K ).NE.ZERO )THEN
                        TEMP = ALPHA*A( J, K )
                        DO 270, I = 1, M
                           B( I, J ) = B( I, J ) + TEMP*B( I, K )
  270                   CONTINUE
                     END IF
  280             CONTINUE
                  TEMP = ALPHA
                  IF( NOUNIT )
     $               TEMP = TEMP*A( K, K )
                  IF( TEMP.NE.ONE )THEN
                     DO 290, I = 1, M
                        B( I, K ) = TEMP*B( I, K )
  290                CONTINUE
                  END IF
  300          CONTINUE
            END IF
         END IF
      END IF
*
      RETURN
*
*     End of STRMM .
*
      END
      SUBROUTINE STRSM ( SIDE, UPLO, TRANSA, DIAG, M, N, ALPHA, A, LDA,
     $                   B, LDB )
*     .. Scalar Arguments ..
      CHARACTER*1        SIDE, UPLO, TRANSA, DIAG
      INTEGER            M, N, LDA, LDB
      REAL               ALPHA
*     .. Array Arguments ..
      REAL               A( LDA, * ), B( LDB, * )
*     ..
*
*  Purpose
*  =======
*
*  STRSM  solves one of the matrix equations
*
*     op( A )*X = alpha*B,   or   X*op( A ) = alpha*B,
*
*  where alpha is a scalar, X and B are m by n matrices, A is a unit, or
*  non-unit,  upper or lower triangular matrix  and  op( A )  is one  of
*
*     op( A ) = A   or   op( A ) = A'.
*
*  The matrix X is overwritten on B.
*
*  Parameters
*  ==========
*
*  SIDE   - CHARACTER*1.
*           On entry, SIDE specifies whether op( A ) appears on the left
*           or right of X as follows:
*
*              SIDE = 'L' or 'l'   op( A )*X = alpha*B.
*
*              SIDE = 'R' or 'r'   X*op( A ) = alpha*B.
*
*           Unchanged on exit.
*
*  UPLO   - CHARACTER*1.
*           On entry, UPLO specifies whether the matrix A is an upper or
*           lower triangular matrix as follows:
*
*              UPLO = 'U' or 'u'   A is an upper triangular matrix.
*
*              UPLO = 'L' or 'l'   A is a lower triangular matrix.
*
*           Unchanged on exit.
*
*  TRANSA - CHARACTER*1.
*           On entry, TRANSA specifies the form of op( A ) to be used in
*           the matrix multiplication as follows:
*
*              TRANSA = 'N' or 'n'   op( A ) = A.
*
*              TRANSA = 'T' or 't'   op( A ) = A'.
*
*              TRANSA = 'C' or 'c'   op( A ) = A'.
*
*           Unchanged on exit.
*
*  DIAG   - CHARACTER*1.
*           On entry, DIAG specifies whether or not A is unit triangular
*           as follows:
*
*              DIAG = 'U' or 'u'   A is assumed to be unit triangular.
*
*              DIAG = 'N' or 'n'   A is not assumed to be unit
*                                  triangular.
*
*           Unchanged on exit.
*
*  M      - INTEGER.
*           On entry, M specifies the number of rows of B. M must be at
*           least zero.
*           Unchanged on exit.
*
*  N      - INTEGER.
*           On entry, N specifies the number of columns of B.  N must be
*           at least zero.
*           Unchanged on exit.
*
*  ALPHA  - REAL            .
*           On entry,  ALPHA specifies the scalar  alpha. When  alpha is
*           zero then  A is not referenced and  B need not be set before
*           entry.
*           Unchanged on exit.
*
*  A      - REAL             array of DIMENSION ( LDA, k ), where k is m
*           when  SIDE = 'L' or 'l'  and is  n  when  SIDE = 'R' or 'r'.
*           Before entry  with  UPLO = 'U' or 'u',  the  leading  k by k
*           upper triangular part of the array  A must contain the upper
*           triangular matrix  and the strictly lower triangular part of
*           A is not referenced.
*           Before entry  with  UPLO = 'L' or 'l',  the  leading  k by k
*           lower triangular part of the array  A must contain the lower
*           triangular matrix  and the strictly upper triangular part of
*           A is not referenced.
*           Note that when  DIAG = 'U' or 'u',  the diagonal elements of
*           A  are not referenced either,  but are assumed to be  unity.
*           Unchanged on exit.
*
*  LDA    - INTEGER.
*           On entry, LDA specifies the first dimension of A as declared
*           in the calling (sub) program.  When  SIDE = 'L' or 'l'  then
*           LDA  must be at least  max( 1, m ),  when  SIDE = 'R' or 'r'
*           then LDA must be at least max( 1, n ).
*           Unchanged on exit.
*
*  B      - REAL             array of DIMENSION ( LDB, n ).
*           Before entry,  the leading  m by n part of the array  B must
*           contain  the  right-hand  side  matrix  B,  and  on exit  is
*           overwritten by the solution matrix  X.
*
*  LDB    - INTEGER.
*           On entry, LDB specifies the first dimension of B as declared
*           in  the  calling  (sub)  program.   LDB  must  be  at  least
*           max( 1, m ).
*           Unchanged on exit.
*
*
*  Level 3 Blas routine.
*
*
*  -- Written on 8-February-1989.
*     Jack Dongarra, Argonne National Laboratory.
*     Iain Duff, AERE Harwell.
*     Jeremy Du Croz, Numerical Algorithms Group Ltd.
*     Sven Hammarling, Numerical Algorithms Group Ltd.
*
*
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
*     .. External Subroutines ..
      EXTERNAL           XERBLA
*     .. Intrinsic Functions ..
      INTRINSIC          MAX
*     .. Local Scalars ..
      LOGICAL            LSIDE, NOUNIT, UPPER
      INTEGER            I, INFO, J, K, NROWA
      REAL               TEMP
*     .. Parameters ..
      REAL               ONE         , ZERO
      PARAMETER        ( ONE = 1.0E+0, ZERO = 0.0E+0 )
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters.
*
      LSIDE  = LSAME( SIDE  , 'L' )
      IF( LSIDE )THEN
         NROWA = M
      ELSE
         NROWA = N
      END IF
      NOUNIT = LSAME( DIAG  , 'N' )
      UPPER  = LSAME( UPLO  , 'U' )
*
      INFO   = 0
      IF(      ( .NOT.LSIDE                ).AND.
     $         ( .NOT.LSAME( SIDE  , 'R' ) )      )THEN
         INFO = 1
      ELSE IF( ( .NOT.UPPER                ).AND.
     $         ( .NOT.LSAME( UPLO  , 'L' ) )      )THEN
         INFO = 2
      ELSE IF( ( .NOT.LSAME( TRANSA, 'N' ) ).AND.
     $         ( .NOT.LSAME( TRANSA, 'T' ) ).AND.
     $         ( .NOT.LSAME( TRANSA, 'C' ) )      )THEN
         INFO = 3
      ELSE IF( ( .NOT.LSAME( DIAG  , 'U' ) ).AND.
     $         ( .NOT.LSAME( DIAG  , 'N' ) )      )THEN
         INFO = 4
      ELSE IF( M  .LT.0               )THEN
         INFO = 5
      ELSE IF( N  .LT.0               )THEN
         INFO = 6
      ELSE IF( LDA.LT.MAX( 1, NROWA ) )THEN
         INFO = 9
      ELSE IF( LDB.LT.MAX( 1, M     ) )THEN
         INFO = 11
      END IF
      IF( INFO.NE.0 )THEN
         CALL XERBLA( 'STRSM ', INFO )
         RETURN
      END IF
*
*     Quick return if possible.
*
      IF( N.EQ.0 )
     $   RETURN
*
*     And when  alpha.eq.zero.
*
      IF( ALPHA.EQ.ZERO )THEN
         DO 20, J = 1, N
            DO 10, I = 1, M
               B( I, J ) = ZERO
   10       CONTINUE
   20    CONTINUE
         RETURN
      END IF
*
*     Start the operations.
*
      IF( LSIDE )THEN
         IF( LSAME( TRANSA, 'N' ) )THEN
*
*           Form  B := alpha*inv( A )*B.
*
            IF( UPPER )THEN
               DO 60, J = 1, N
                  IF( ALPHA.NE.ONE )THEN
                     DO 30, I = 1, M
                        B( I, J ) = ALPHA*B( I, J )
   30                CONTINUE
                  END IF
                  DO 50, K = M, 1, -1
                     IF( B( K, J ).NE.ZERO )THEN
                        IF( NOUNIT )
     $                     B( K, J ) = B( K, J )/A( K, K )
                        DO 40, I = 1, K - 1
                           B( I, J ) = B( I, J ) - B( K, J )*A( I, K )
   40                   CONTINUE
                     END IF
   50             CONTINUE
   60          CONTINUE
            ELSE
               DO 100, J = 1, N
                  IF( ALPHA.NE.ONE )THEN
                     DO 70, I = 1, M
                        B( I, J ) = ALPHA*B( I, J )
   70                CONTINUE
                  END IF
                  DO 90 K = 1, M
                     IF( B( K, J ).NE.ZERO )THEN
                        IF( NOUNIT )
     $                     B( K, J ) = B( K, J )/A( K, K )
                        DO 80, I = K + 1, M
                           B( I, J ) = B( I, J ) - B( K, J )*A( I, K )
   80                   CONTINUE
                     END IF
   90             CONTINUE
  100          CONTINUE
            END IF
         ELSE
*
*           Form  B := alpha*inv( A' )*B.
*
            IF( UPPER )THEN
               DO 130, J = 1, N
                  DO 120, I = 1, M
                     TEMP = ALPHA*B( I, J )
                     DO 110, K = 1, I - 1
                        TEMP = TEMP - A( K, I )*B( K, J )
  110                CONTINUE
                     IF( NOUNIT )
     $                  TEMP = TEMP/A( I, I )
                     B( I, J ) = TEMP
  120             CONTINUE
  130          CONTINUE
            ELSE
               DO 160, J = 1, N
                  DO 150, I = M, 1, -1
                     TEMP = ALPHA*B( I, J )
                     DO 140, K = I + 1, M
                        TEMP = TEMP - A( K, I )*B( K, J )
  140                CONTINUE
                     IF( NOUNIT )
     $                  TEMP = TEMP/A( I, I )
                     B( I, J ) = TEMP
  150             CONTINUE
  160          CONTINUE
            END IF
         END IF
      ELSE
         IF( LSAME( TRANSA, 'N' ) )THEN
*
*           Form  B := alpha*B*inv( A ).
*
            IF( UPPER )THEN
               DO 210, J = 1, N
                  IF( ALPHA.NE.ONE )THEN
                     DO 170, I = 1, M
                        B( I, J ) = ALPHA*B( I, J )
  170                CONTINUE
                  END IF
                  DO 190, K = 1, J - 1
                     IF( A( K, J ).NE.ZERO )THEN
                        DO 180, I = 1, M
                           B( I, J ) = B( I, J ) - A( K, J )*B( I, K )
  180                   CONTINUE
                     END IF
  190             CONTINUE
                  IF( NOUNIT )THEN
                     TEMP = ONE/A( J, J )
                     DO 200, I = 1, M
                        B( I, J ) = TEMP*B( I, J )
  200                CONTINUE
                  END IF
  210          CONTINUE
            ELSE
               DO 260, J = N, 1, -1
                  IF( ALPHA.NE.ONE )THEN
                     DO 220, I = 1, M
                        B( I, J ) = ALPHA*B( I, J )
  220                CONTINUE
                  END IF
                  DO 240, K = J + 1, N
                     IF( A( K, J ).NE.ZERO )THEN
                        DO 230, I = 1, M
                           B( I, J ) = B( I, J ) - A( K, J )*B( I, K )
  230                   CONTINUE
                     END IF
  240             CONTINUE
                  IF( NOUNIT )THEN
                     TEMP = ONE/A( J, J )
                     DO 250, I = 1, M
                       B( I, J ) = TEMP*B( I, J )
  250                CONTINUE
                  END IF
  260          CONTINUE
            END IF
         ELSE
*
*           Form  B := alpha*B*inv( A' ).
*
            IF( UPPER )THEN
               DO 310, K = N, 1, -1
                  IF( NOUNIT )THEN
                     TEMP = ONE/A( K, K )
                     DO 270, I = 1, M
                        B( I, K ) = TEMP*B( I, K )
  270                CONTINUE
                  END IF
                  DO 290, J = 1, K - 1
                     IF( A( J, K ).NE.ZERO )THEN
                        TEMP = A( J, K )
                        DO 280, I = 1, M
                           B( I, J ) = B( I, J ) - TEMP*B( I, K )
  280                   CONTINUE
                     END IF
  290             CONTINUE
                  IF( ALPHA.NE.ONE )THEN
                     DO 300, I = 1, M
                        B( I, K ) = ALPHA*B( I, K )
  300                CONTINUE
                  END IF
  310          CONTINUE
            ELSE
               DO 360, K = 1, N
                  IF( NOUNIT )THEN
                     TEMP = ONE/A( K, K )
                     DO 320, I = 1, M
                        B( I, K ) = TEMP*B( I, K )
  320                CONTINUE
                  END IF
                  DO 340, J = K + 1, N
                     IF( A( J, K ).NE.ZERO )THEN
                        TEMP = A( J, K )
                        DO 330, I = 1, M
                           B( I, J ) = B( I, J ) - TEMP*B( I, K )
  330                   CONTINUE
                     END IF
  340             CONTINUE
                  IF( ALPHA.NE.ONE )THEN
                     DO 350, I = 1, M
                        B( I, K ) = ALPHA*B( I, K )
  350                CONTINUE
                  END IF
  360          CONTINUE
            END IF
         END IF
      END IF
*
      RETURN
*
*     End of STRSM .
*
      END
      LOGICAL          FUNCTION LSAME( CA, CB )
*
*  -- LAPACK auxiliary routine (version 2.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     September 30, 1994 
*     
*     .. Scalar Arguments ..
      CHARACTER          CA, CB
*     ..
*     
*  Purpose
*  =======
*  
*  LSAME returns .TRUE. if CA is the same letter as CB regardless of
*  case. 
*  
*  Arguments
*  =========
*  
*  CA      (input) CHARACTER*1
*  CB      (input) CHARACTER*1
*          CA and CB specify the single characters to be compared.
*
* =====================================================================
* 
*     .. Intrinsic Functions ..
      INTRINSIC          ICHAR
*     ..
*     .. Local Scalars ..
      INTEGER            INTA, INTB, ZCODE
*     ..
*     .. Executable Statements ..
*     
*     Test if the characters are equal
*     
      LSAME = CA.EQ.CB
      IF( LSAME )
     $   RETURN 
*
*     Now test for equivalence if both characters are alphabetic.
*
      ZCODE = ICHAR( 'Z' )
*
*     Use 'Z' rather than 'A' so that ASCII can be detected on Prime
*     machines, on which ICHAR returns a value with bit 8 set.
*     ICHAR('A') on Prime machines returns 193 which is the same as
*     ICHAR('A') on an EBCDIC machine.
*
      INTA = ICHAR( CA )
      INTB = ICHAR( CB )
*
      IF( ZCODE.EQ.90 .OR. ZCODE.EQ.122 ) THEN
*
*        ASCII is assumed - ZCODE is the ASCII code of either lower or
*        upper case 'Z'.
*
         IF( INTA.GE.97 .AND. INTA.LE.122 ) INTA = INTA - 32
         IF( INTB.GE.97 .AND. INTB.LE.122 ) INTB = INTB - 32
*
      ELSE IF( ZCODE.EQ.233 .OR. ZCODE.EQ.169 ) THEN
*
*        EBCDIC is assumed - ZCODE is the EBCDIC code of either lower or
*        upper case 'Z'.
*
         IF( INTA.GE.129 .AND. INTA.LE.137 .OR.
     $       INTA.GE.145 .AND. INTA.LE.153 .OR.
     $       INTA.GE.162 .AND. INTA.LE.169 ) INTA = INTA + 64
         IF( INTB.GE.129 .AND. INTB.LE.137 .OR.
     $       INTB.GE.145 .AND. INTB.LE.153 .OR.
     $       INTB.GE.162 .AND. INTB.LE.169 ) INTB = INTB + 64
*
      ELSE IF( ZCODE.EQ.218 .OR. ZCODE.EQ.250 ) THEN
*
*        ASCII is assumed, on Prime machines - ZCODE is the ASCII code
*        plus 128 of either lower or upper case 'Z'.
*
         IF( INTA.GE.225 .AND. INTA.LE.250 ) INTA = INTA - 32
         IF( INTB.GE.225 .AND. INTB.LE.250 ) INTB = INTB - 32
      END IF
      LSAME = INTA.EQ.INTB
*
*     RETURN
*
*     End of LSAME
*
      END
