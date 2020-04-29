      program implicit_function_cast 
      integer unit
      integer a
      common /infoc/unit

      unit = 3
      call c(a, b)  

      write(*, *)unit
      end
      subroutine c(a, b)
      integer nout
      common /infoc/nout

      i = d(nout)

      nout = 4
      end
      function d(ii)
      integer ii

      ii = 3
      d = ii
      return 
      end
