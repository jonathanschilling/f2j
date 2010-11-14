      PROGRAM HELLO3
      integer x
      WRITE (*,100) 12, 13, 14
      WRITE (*,200) 666
      WRITE (*,300)
      WRITE (*,400)
      WRITE (*,500)
      WRITE (*,600)
      WRITE (*,601)
      WRITE (*,602)
      WRITE (*,603)
      write (*,1000) 1.1, 2.2, 3.3, 4.4, 5.5, 6.6
      write (*,*) 'blah!!!'
      STOP
  100 format (3i8)
  200 FORMAT (11HHELLO WORLD,4hblah,3i8)
  300 FORMAT (11hHELLO WORLD)
  400 FORMAT (1hH)
  500 FORMAT (2hxy)
  600 FORMAT (3hxyz)
  601 FORMAT (1  hH)
  602 FORMAT (2  hxy)
  603 FORMAT (3    hxyz)
 1000 FORMAT(/20H THE ERROR NORMS ARE//3E23.13//
     *15H THE ERRORS ARE//(3E23.14))
      END
