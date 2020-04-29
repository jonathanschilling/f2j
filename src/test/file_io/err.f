      program err
      integer u,v,k
*
      u = 32
      v = 12
      open(u-v, iostat=k, FILE='blah.txt',err=30)
      write(u-v,*) 'howdy, k = ', k
      goto 40
*
 30   write(*,*) 'error'
 40   continue
      end
