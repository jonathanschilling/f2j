      program continue
      integer x
      x=12
      continue

  20  continue
      do 30 i=1,x
        write(*,*) i
  30  continue
      i = 1
  40  continue
      if(i .gt. x) goto 50
        write(*,*) i
      i = i + 1
      goto 40
  50  continue
      end
