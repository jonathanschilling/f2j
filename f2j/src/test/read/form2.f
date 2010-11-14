      program form2
      integer integer1, integer2
c
      read (*,150) INTEGER1, INTEGER2
      write(*,150) INTEGER1, INTEGER2
      stop
  150 format (I3, 12x, I3)
      end
