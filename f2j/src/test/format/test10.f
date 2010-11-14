      program test10
      integer nalf, alf(20)
*
      nalf = 5
      READ( *, FMT = 999 )( ALF( I ), I = 1, NALF )
      write( *, *)( ALF( I ), I = 1, NALF )
      stop
 999  format (5I4)
      end
