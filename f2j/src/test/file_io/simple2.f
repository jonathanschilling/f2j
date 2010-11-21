      program simple2
      integer a,b,c,d
*
      a=10
      b=11
      c=12
      d=13
*
      open(a, FILE='blah1.txt',err=100,iostat=k)
      open(b, FILE='blah2.txt',err=100)
      open(c, FILE='blah3.txt',iostat=k)
      open(d, FILE='blah4.txt')
      write(a,*) 'howdy to a'
      write(b,*) 'howdy to b'
      write(c,*) 'howdy to c'
      write(d,*) 'howdy to d'
      stop
*
  100 write(*,*) 'ending at 100 k = ', k
      end
