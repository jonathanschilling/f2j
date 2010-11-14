      program test11
      integer nalf
      character*10 alf(20)
      logical lcrap(20)
*
      nalf = 5
      READ( *, FMT = 999 )( ALF( I ), I = 1, NALF )
      write( *, *)( ALF( I ), I = 1, NALF )
      READ( *, FMT = 998 )( lcrap( I ), I = 1, NALF )
      write( *, *)( lcrap( I ), I = 1, NALF )
      stop
 998  format (5L2)
 999  format (5A6)
      end
