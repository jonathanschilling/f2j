      program small
      integer logfil
      external ivout
      logfil = 1
      call ivout ('_naup2: no. "converged" Ritz values at this iter.')
      write(*,*) '_naup2: no. "converged" Ritz values at this iter.'
      end
C
      SUBROUTINE IVOUT (IFMT)
      CHARACTER  IFMT*(*)
      write(*,*) 'hi ', ifmt
C
      RETURN
      END
