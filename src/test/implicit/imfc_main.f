      program rain

      t = 1
      sum = 0.0
      do 10 m = 1, 12
         sum = sum + r(m, t)
  10  continue
      write (*,*) 'sum ', sum

      x = in()

      write(*, *)x

      stop
      end
