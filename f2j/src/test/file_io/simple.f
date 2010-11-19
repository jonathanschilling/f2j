      program simple
      integer u,v
*
      u = 32
      v = 12
      open(u-v, FILE='blah.txt')
      write(u-v,*) 'howdy'
*
      end
