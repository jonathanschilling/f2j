      program dim

      dimension p(10)
      dimension i(10)

      p(1) = 1.1
      p(2) = 1.2

      i(1) = 1.1

      p1 = dmax1(p(1), p(2))

      write(*,  *)'p = ', p(1)
      write(*,  *)'i = ', i(1)
      end
