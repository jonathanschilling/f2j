      program trim
      external r4vec_print_some
      integer n
      real r(10)
*
      n = 10
      write(*,*) 'non-empty'
      call r4vec_print_some ( n, r, 10, '  The original data:' )
      write(*,*) 'blanks'
      call r4vec_print_some ( n, r, 10, '    ' )
      write(*,*) 'empty'
      call r4vec_print_some ( n, r, 10, '' )
*
      end
*
*
      subroutine r4vec_print_some ( n, a, max_print, title )
      implicit none
      external crap
      integer crap
      integer n

      real a(n)
      integer i
      integer max_print
C      character ( len = * ) title
      character*(*) title
      i=crap(title)
C      if ( 0 .lt. len_trim ( title ) ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) title
        write ( *, '(a)' ) ' '
C      end if
*
      return
      end

      integer function crap(s)
      character*(*) s
*
      write(*,*) 'len is ', len(s)
*
      crap = 5
      return
      end
