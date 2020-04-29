      program rain
 
      t = 1
      sum = 0.0
      do 10 m = 1, 12
         sum = sum + r(m, t)
  10  continue
      write (*,*) 'sum ', sum

      stop
      end
  
      double precision function r(m,t)

      r = 0.1*t * (m**2 + 14*m + 46)
      if (r .LT. 0) r = 0.0

      return
      end
