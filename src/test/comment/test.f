C first routine
C with some prolog
      subroutine first(x)
C blah
      integer x
      write(*,*) x
      return
      end
      subroutine foo(x)
C  here is a
C comment for subroutine foo
C immediately following the 
C subroutine declaration
C
      integer x
      write(*,*) x
      return
      end
C  here is a
C comment for subroutine bar
C immediately preceding the 
C subroutine declaration
C
C extra lines just to
C make it longer than the
C block for foo
C
      subroutine bar(x)
      integer x
      write(*,*) x
C here is another
C comment block in bar()
      return
      end
      subroutine blah(y)
C and now here are some
C non-prolog comments in
C the third subroutine.
C
      integer y
C short comment
      write(*,*) y
C another short
C comment
      return
      end
C short prolog comment
C extra line for test
      subroutine asdf(z)
      integer z
      write(*,*) z
      return
      end
