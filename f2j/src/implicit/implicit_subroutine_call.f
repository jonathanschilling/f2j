      program implicit_sub
      integer ii

      i = 1
      x = 2 

      call help(i, x)

      end

      subroutine help(i, x)

      write(*, *)i, x

      end 
