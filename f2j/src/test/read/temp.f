      program form5
      real a,b,c
      READ(*,140,end=30) a, b, c
      goto 30
 25   write(*,*) 'there was an error reading'
      stop
 30   write(*,*) 'done'
      stop
 140  format(f7.2,2x,f10.5,3x,e12.7)
      end
