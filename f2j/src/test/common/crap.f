      program crap
      integer x,y
*     integer ARGl, ARG2(10), ARG3, ARG4
*     COMMON /ARGMNT/ ARGl, ARG2, ARG3, ARG4
      integer one, two(10), three, four
      COMMON /ARGMNT/ one, two, three, four
*
      write(*,*) 'hello in main'
*
*     ARGl = 4
*     ARG2 = 3
*     ARG3 = 2
*     ARG4 = 1
      one = 4
      two(1) = 3
      three = 2
      four = 1
*
      x = 999
      y = 666
*      call mysub(x,y)
*     call mysub(x,ARG3)
      call mysub(x,three)
      write(*,*) 'after call, three = ', three
      stop
      end
*
      subroutine mysub(i,j)
      integer i,j
*
      write(*,*) 'in mysub, i = ', i, ', j = ', j
      j = j + 1
*
      return
      end
