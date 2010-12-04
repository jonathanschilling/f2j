      program rewind
      integer u,x,k
      u = 12
      x = 13
      open(x,file='foo.dat',status='old')
      read(x,*) i,j,k
      write(*,*) i,j,k
      rewind x
      read(x,*) i,j,k
      write(*,*) i,j,k
      close(x, status='keep')
c just some checks on syntax
      rewind u
      rewind (UNIT=u, ERR=200, iostat = k)
      rewind (UNIT=u, ERR=200)
      rewind (UNIT=u)
      rewind 13
      stop
 200  write(*,*) 'hit error, k = ', k
      end
