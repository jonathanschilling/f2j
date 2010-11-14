      program foo2
*
      double precision s1, s2, et
      double precision               dsecnd
      EXTERNAL           dsecnd
*
      s1 = dsecnd()
      read(*,*) et
      s2 = dsecnd()
      et = s2 - s1
      write(*,*) 's1 = ', s1, ', s2 = ', s2, ', et = ', et
      stop
      end
