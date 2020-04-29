      program ltest2
      integer x
      double precision y, elseif
      external elseif
      if(x.eq.1) then
        x = 12
  5   y=else if(33)
        x = 99
 10   else
        x = 13
      end if
      end
      double precision function elseif(x)
      integer x
      elseif = 6.33
      return
      end
