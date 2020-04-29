      program rain
      
      t = 1
      sum = 0.0
      do 10 m = 1, 12
         sum = sum + rem(m, t)
  10  continue
      write (*,*) 'sum ', sum

      x = in()

      write(*, *)x

      stop
      end

      function rem(m,t)

      rem = 0.1*t * (m**2 + 14*m + 46)
      if (rem .LT. 0) rem = 0.0

      return
      end

      function in()

      in = 2.1

      return    
      end
