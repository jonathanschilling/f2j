      program small
      character blah(10)
      integer i
      do 10 i=1,10
       blah(i) = 'a'
 10   continue
      blah(2) = 'b'
      write(*,*) blah
      stop
      end
