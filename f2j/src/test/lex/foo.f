      program foo
      integer x
      external crap
c
      if(.true.) then
        x = 5
      else
        x = 7
      endif
      call crap(x)
      end
c
      subroutine crap(x)
      integer x
c
      do 130 i=1,x
      do 120 j=1,x
        write(*,*) 'crap'
  120    continue

  130 continue
c
      write(*,*) 'x = ',x
      end
