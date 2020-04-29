      program bbbb
      integer x
      x= 666
      write(*,*,err=30,iostat=k) x
      write(*,*,err=30) x
      write(*,*,iostat=k) x
      write(*,*) x
      write(*,*) k
      stop
 30   write(*,*) 'done 30'
      end
