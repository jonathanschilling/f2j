      program c_sub
      integer i

      call fill_array()

      end     

      subroutine fill_array()
      parameter (nmax=5)
      real A(nmax)
      common /matrix/ A

      do 102 i = 1, 5
         A(i) = i**2
  102 continue

      end
