* tests read()
      program test8
      CHARACTER*6        SNAMET
      CHARACTER*32       SUMMRY
      logical ltestt
      integer x, nout
*
      READ( *, FMT = * )SUMMRY
      READ( *, FMT = * )NOUT
      write(*, *) '[', summry, ']'
      READ( *, FMT = 9988)SNAMET, LTESTT
      write(*, *) 'without end spec ', snamet, ltestt
*
   30 READ( *, FMT = 9988, END = 60 )SNAMET, LTESTT
      write(*, *) snamet, ltestt
      go to 30
*
   60 write(*,*) 'done'
      stop
 9988 FORMAT( A6, L2 )
      end
