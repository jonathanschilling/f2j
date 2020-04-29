      program test12
      character*72 aline
*
      READ( *, FMT = '(A72)', END = 140 )ALINE
      write(*,*) '[', aline, ']'
 140  write(*,*) 'done'
      end
