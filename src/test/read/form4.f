      program form4
      real a,b,c
c
  10  read(*,130,end=30) a,b,c
      write(*,*) a,b,c
      go to 10 
 30   write(*,*) 'done'
      stop
 130  format(3(f7.2,1x))
      end
