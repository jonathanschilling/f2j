      program test
      character*101 foo
      character x(15)
      character y(15)
      data x,y/10*'a',20*'b'/
*
      foo = 'blahblahblahblahblahblahblahblahblahblahblahblahblahblahbla
     $hblahblahblahblahblahblahblahblahblahblahx'
*
      write(*,*) foo
      write(*,*) x
      write(*,*) y
*
      stop 'aslkdfjalksdf'
      end
