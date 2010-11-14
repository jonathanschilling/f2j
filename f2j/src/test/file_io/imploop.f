      program imploop
      integer i, x(5)
*
      write(*,*) 'enter 5 integers:'
      read(*,*) (x(i), i=1,5)
      write(*,*) (x(i), i=1,5)
      write(*,fmt='(5I10)') (x(i), i=1,5)
*
      write(*,*) 'now doing same thing from file'
      open(9,file='foo2.dat',status='old')
      read(9,*) (x(i), i=1,5)
      close(9, status='keep')
      write(*,*) (x(i), i=1,5)
      write(*,fmt='(5I10)') (x(i), i=1,5)
*
      end
