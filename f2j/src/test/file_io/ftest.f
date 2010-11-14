      program ftest
      integer x,y,z
      integer i,j,k
      x = 12
      y = 24
      i = 123
      j = 456
      k = 789
      open(x,file='aaaaaaaaaaaaaa',status='new')
      write(x,*) 'howdy to file a'
c      endfile(x)
c      write(x,*) 'after endfile'
      close(x)
c
      open(x,file='bbbbbbbbbbbbbb',status='new')
      write(x,*) 'howdy to file b'
      close(x, status='keep')
c
      open(x,file='cccccccccccccc',status='new')
      write(x,*) 'howdy to file c'
      close(x, status='delete')
c
      open(x,file='dddddddddddddd',status='new')
      write(x,fmt='(3I10)') i,j,k
      close(x, status='keep')
c
      write(6,*) 'explicit write to stdout'
      write(0,*) 'explicit write to stderr'
c      read(*,*) y, z
c      read(*,fmt='(i20 i20)') y,z
c      read(*,fmt='(I20,I20)') y,z
c      write(*,*) y, z
c
      i = 0
      j = 0
      k = 0
      open(x,file='dddddddddddddd',status='old')
      read(x,fmt='(3I10)') i,j,k
      close(x, status='keep')
      write(*,*) i,j,k
c
      stop
      end
