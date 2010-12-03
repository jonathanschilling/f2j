      program diff
      COMMON /foo/a,b,c,d(20)
      a = 12
      call FS051
      write(*,*) a
      call FS051
      write(*,*) a
      end
      SUBROUTINE FS051
      COMMON /foo/x
      x = x + 1
      RETURN
      END
