       program crap
       double precision x
*
       x = 100.0
*
  10   write(*,999) x
  15   write(*,998) x
       x = x / 2.0
       if(x .eq. 0.0) then
          go to 20
       endif
       go to 10
  20   write(*,*) 'done'
 998   format (F16.6)
 999   format (D16.6)
       end
