      program commontest
      integer i

      call test()
      end
      subroutine test()
      integer i
      common /commontest/i

      i = 2
      end
