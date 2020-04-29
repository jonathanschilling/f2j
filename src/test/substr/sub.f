      program sub
      character*10 x
*
      x = 'zyxwvutsrq'
      write(*,*) x
      x(2:3) = 'AB'
*
      write(*,*) x
      x(2:2) = '#'
      write(*,*) x
*
      stop
      end
