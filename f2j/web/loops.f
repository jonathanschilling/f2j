c  Test subroutine for building a Fortran to 
c  Jasmin (java assembler interface).
c
      subroutine loops(x,z)
      integer x, z
      integer y, i, j
      do 20 j = 1, 100
        do 10 i = 1, 10
          y = z - 1
   10 continue
   20 continue
      do 30 j = 1, 20
      y = y + 1
      goto 40
   30 continue
   40 z = y/8
      return
      end
