      program foo
      integer x, y
*
      read(*,*) y
      if(y .eq. 20) then
        assign 20 to x
      else if(y .eq. 30) then
        assign 30 to x
      else if(y .eq. 40) then
        assign 40 to x
      else if(y .eq. 10) then
        assign 10 to x
      endif
*      go to x (10, 20, 30, 40)
      go to x
*
      write(*,*) 'foo'
*
      go to 50
*
   10 write(*,*) '10'
      go to 50
   20 write(*,*) '20'
      go to 50
   30 write(*,*) '30'
      go to 50
   40 write(*,*) '40'
      go to 50
*
   50 stop
      end
