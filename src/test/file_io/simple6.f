      program simple6
      character*1 a
      character*2 b
      character*3 c
      character*4 d
      character*5 e
      character*6 f
      character*7 g
      integer x
c
      x = 34
      a = 'a'
      b = 'bb'
      c = 'ccc'
      d = 'dddd'
      e = 'eeeee'
      f = 'ffffff'
      g = 'ggggggg'
c
      write(*,*) x,a
      write(*,*) x,b
      write(*,*) x,c
      write(*,*) x,d
      write(*,*) x,e
      write(*,*) x,f
      write(*,*) x,g
c
      write(*,*) a,x
      write(*,*) b,x
      write(*,*) c,x
      write(*,*) d,x
      write(*,*) e,x
      write(*,*) f,x
      write(*,*) g,x
      end
