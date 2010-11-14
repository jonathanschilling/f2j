      program strdec
      integer x
      x = 12
      call bar(x)
      stop
      end
*
      SUBROUTINE bar(i)
      integer i
*
      INTEGER            NTRAN
      PARAMETER          ( NTRAN = 3 )
*
      CHARACTER          TRANSS( NTRAN )
*
      DATA TRANSS / 'N', 'T', 'C' /
*
      write(*,*) transs(1), transs(2), transs(3)
      return
      end
