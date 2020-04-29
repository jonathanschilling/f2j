      program test
      CHARACTER          ICOL( 3 )
      integer foo
      DATA ICOL( 1 ), ICOL( 2 ), ICOL( 3 ), foo / 'C', 'o',
     $ 'l' , 1234/
*
      write(*,*) icol
      write(*,*) 'hello world, foo = ', foo
*
      end
