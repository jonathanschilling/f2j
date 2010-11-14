      program foo1
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
