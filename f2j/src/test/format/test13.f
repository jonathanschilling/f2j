      program test13
      integer nalf, alf(5)
*
      nalf = 5
      READ( *, FMT = 999 ) alf
      write( *, *)( ALF( I ), I = 1, NALF )
      stop
 999  format (5I4)
      end
