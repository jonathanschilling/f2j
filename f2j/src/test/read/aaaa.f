      program aaaa
      integer x,y
      read(*,*,err=100,end=200,iostat=k) x,y
      write(*,*) x,y,k
      stop
  100 write(*,*)'done 100 ', k
      stop
  200 write(*,*)'done 200 ', k
      end
