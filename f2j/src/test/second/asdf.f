      program asdf
*
      real s1, s2, et
      REAL               SECOND
      EXTERNAL           SECOND
*
      s1 = second()
      read(*,*) et
      s2 = second()
      et = s2 - s1
      write(*,*) 's1 = ', s1, ', s2 = ', s2, ', et = ', et
      stop
      end
      REAL             FUNCTION SECOND( )
*
*  -- LAPACK auxiliary routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     September 30, 1994
*
*  Purpose
*  =======
*
*  SECOND returns the user time for a process in seconds.
*  This version gets the time from the system function ETIME.
*
* =====================================================================
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
      SECOND = TARRAY( 1 )
      RETURN
*
*     End of SECOND
*
      END
