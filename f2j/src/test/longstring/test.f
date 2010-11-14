      program test
      character*101 foo
      character x(30)
      data x/30*'a'/
*
      foo = 'blahblahblahblahblahblahblahblahblahblahblahblahblahblahbla
     $hblahblahblahblahblahblahblahblahblahblahx'
*
      write(*,*) foo
      write(*,*) x
*
      stop 'aslkdfjalksdf'
      end
