      program sqrttest
      intrinsic dsqrt
      double precision dsqrt, x, y
*
      y = 4.0d0
      x = dsqrt(y)
*
      write(*,*) x
*
      stop
      end
