      program unfread
      integer x
      integer i,j,k
      x = 12
      i = 0
      j = 0
      k = 0
      write(*,*) 'hit enter to begin'
      read(*,*)
      open(x,file='foo.dat',status='old')
      read(x,*) i,j,k
      close(x, status='keep')
      write(*,*) i,j,k
c
      stop
      end
