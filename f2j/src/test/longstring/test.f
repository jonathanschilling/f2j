      program test
      integer a,b,p
      parameter (p=20)
      character*101 foo
      character x(15)
      character y(15)
      data a,x,y,b/1234,10*'a',p*'b',666/
*
      foo = 'blahblahblahblahblahblahblahblahblahblahblahblahblahblahbla
     $hblahblahblahblahblahblahblahblahblahblahx'
*
      write(*,*) foo
      write(*,*) x
      write(*,*) y
      write(*,*) a,b
*
      stop 'aslkdfjalksdf'
      end
