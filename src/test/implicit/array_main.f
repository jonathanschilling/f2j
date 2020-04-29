      program array_common
      parameter (nmax=5)
      real A(nmax)
      common /matrix/ A

      do 100 i = 1, 5
         A(i) = 0
         write(*, *)A(i)
  100 continue

      call fill_array()

      do 101 i = 1, 5
         write(*, *)A(i)
  101 continue

      end
