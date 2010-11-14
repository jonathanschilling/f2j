      program pread
      real a,b,c
      character*32 foo
*
      READ( *, FMT = 998 ) foo
      write( *, *) 'foo = ', foo
      READ( *, FMT = 999 ) a,b,c
      write( *, *) a,b,c
      stop
 998  format (A16)
 999  format (0P 3F6.2)
      end
