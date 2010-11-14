      PROGRAM DSTEVREX
*     DSTEVR Example Program Text
*     NAG Copyright 2005.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          NMAX, MMAX
      PARAMETER        (NMAX=10,MMAX=5)
      INTEGER          LDZ, LIWORK, LWORK
      PARAMETER        (LDZ=NMAX,LIWORK=10*NMAX,LWORK=20*NMAX)
      DOUBLE PRECISION ZERO
      PARAMETER        (ZERO=0.0D+0)
*     .. Local Scalars ..
      DOUBLE PRECISION ABSTOL, VL, VU
      INTEGER          I, IFAIL, IL, INFO, IU, J, LIWOPT, LWOPT, M, N
*     .. Local Arrays ..
      DOUBLE PRECISION D(NMAX), E(NMAX-1), W(NMAX), WORK(LWORK),
     +                 Z(LDZ,MMAX)
      INTEGER          ISUPPZ(2*NMAX), IWORK(LIWORK)
*     .. External Subroutines ..
      EXTERNAL         DSTEVR
*     .. Executable Statements ..
      WRITE (NOUT,*) 'DSTEVR Example Program Results'
      WRITE (NOUT,*)
*     Skip heading in data file and read N and the lower and upper
*     indices of the eigenvalues to be found
      READ (NIN,*)
      READ (NIN,*) N, IL, IU
      IF (N.LE.NMAX .AND. (IU-IL+1).LE.MMAX) THEN
*
*        Read the diagonal and off-diagonal elements of the matrix A
*        from data file
*
         READ (NIN,*) (D(I),I=1,N)
         READ (NIN,*) (E(I),I=1,N-1)
*
*        Set the absolute error tolerance for eigenvalues.  With ABSTOL
*        set to zero, the default value is used instead
*
         ABSTOL = ZERO
*
*        Solve the symmetric tridiagonal eigenvalue problem
*
         CALL DSTEVR('Vectors','Indices',N,D,E,VL,VU,IL,IU,ABSTOL,M,W,Z,
     +               LDZ,ISUPPZ,WORK,LWORK,IWORK,LIWORK,INFO)
         LWOPT = WORK(1)
         LIWOPT = IWORK(1)
*
         IF (INFO.EQ.0) THEN
*
*           Print solution
*
            WRITE (NOUT,*) 'Selected eigenvalues'
            WRITE (NOUT,99999) (W(J),J=1,M)
*
            IFAIL = 0
*            CALL X04CAF('General',' ',N,M,Z,LDZ,'Selected eigenvectors',
*     +                  IFAIL)
         ELSE
            WRITE (NOUT,99998) 'Failure in DSTEVR. INFO =', INFO
         END IF
*
*        Print workspace information
*
         IF (LWORK.LT.LWOPT) THEN
            WRITE (NOUT,*)
            WRITE (NOUT,99997) 'Real workspace required = ', LWOPT,
     +        'Real workspace provided = ', LWORK
         END IF
         IF (LIWORK.LT.LIWOPT) THEN
            WRITE (NOUT,*)
            WRITE (NOUT,99997) 'Integer workspace required = ', LIWOPT,
     +        'Integer workspace provided = ', LIWORK
         END IF
      ELSE
         WRITE (NOUT,*) 'NMAX and/or MMAX too small'
      END IF
      STOP
*
99999 FORMAT (3X,(8F8.4))
99998 FORMAT (1X,A,I5)
99997 FORMAT (1X,A,I5,/1X,A,I5)
      END
