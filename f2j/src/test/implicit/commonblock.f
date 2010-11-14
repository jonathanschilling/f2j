      program commonblock
      external check, booze
      common /coeff/ alpha, beta

      alpha = 1

      write(*, *)alpha, beta 

      call check()
      call booze()

      write(*, *)alpha, beta 

      end 

      subroutine check()
      common /coeff/ alpha, cta 

      alpha = 100
      cta = 100.0

      end

      subroutine booze()
      common /coeff/ alpha, cta

      alpha = -1 
      cta = 101.0

      end
