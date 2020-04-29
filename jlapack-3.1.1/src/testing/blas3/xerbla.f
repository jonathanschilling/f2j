      SUBROUTINE XERBLA( SRNAME, INFO )
*
*  f2j NOTE:  this is compiled separately from dblat2.f because
*  it needs to be in package org.netlib.err while the rest of
*  dblat2.f routines should be in org.netlib.blas.testing.
*
*  This is a special version of XERBLA to be used only as part of
*  the test program for testing error exits from the Level 3 BLAS
*  routines.
*
*  XERBLA  is an error handler for the Level 3 BLAS routines.
*
*  It is called by the Level 3 BLAS routines if an input parameter is
*  invalid.
*
*  Auxiliary routine for test program for Level 3 Blas.
*
*  -- Written on 8-February-1989.
*     Jack Dongarra, Argonne National Laboratory.
*     Iain Duff, AERE Harwell.
*     Jeremy Du Croz, Numerical Algorithms Group Ltd.
*     Sven Hammarling, Numerical Algorithms Group Ltd.
*
*     .. Scalar Arguments ..
      INTEGER            INFO
      CHARACTER*6        SRNAME
*     .. Scalars in Common ..
      INTEGER            INFOT, NOUT
      LOGICAL            LERR, OK
      CHARACTER*6        SRNAMT
*     .. Common blocks ..
      COMMON             /INFOC/INFOT, NOUT, OK, LERR
      COMMON             /SRNAMC/SRNAMT
*     .. Executable Statements ..
      LERR = .TRUE.
      IF( INFO.NE.INFOT )THEN
         IF( INFOT.NE.0 )THEN
            WRITE( NOUT, FMT = 9999 )INFO, INFOT
         ELSE
            WRITE( NOUT, FMT = 9997 )INFO
         END IF
         OK = .FALSE.
      END IF
      IF( SRNAME.NE.SRNAMT )THEN
         WRITE( NOUT, FMT = 9998 )SRNAME, SRNAMT
         OK = .FALSE.
      END IF
      RETURN
*
 9999 FORMAT( ' ******* XERBLA WAS CALLED WITH INFO = ', I6, ' INSTEAD',
     $      ' OF ', I2, ' *******' )
 9998 FORMAT( ' ******* XERBLA WAS CALLED WITH SRNAME = ', A6, ' INSTE',
     $      'AD OF ', A6, ' *******' )
 9997 FORMAT( ' ******* XERBLA WAS CALLED WITH INFO = ', I6,
     $      ' *******' )
*
*     End of XERBLA
*
      END

