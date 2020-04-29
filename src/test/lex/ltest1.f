      program ltest1
      integer x
      x = 3
*     go to 12
      write(*,*) 'hi'
  3   if(x.eq.1) then
  4     x = 12
  5   else if (x.eq.1+2) then
  6     x = 99
 10   else
 11     x = 13
        write(*,*) 'bad'
 12   end if
      write(*,*) 'cool, x = ', x
      end
