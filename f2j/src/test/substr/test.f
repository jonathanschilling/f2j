      program test
      CHARACTER          JOB
*
      job = 'x'
      call foo(job)
      write(*,*) 'after foo, job = ', job
      call bar(job)
      write(*,*) 'after bar, job = ', job
      job = 'x'
      write(*,*) 'before asdf, job reset to ', job
      call asdf(job)
      write(*,*) 'after asdf, job = ', job
      job = 'x'
      write(*,*) 'before crap, job reset to ', job
      call crap(job)
      write(*,*) 'after crap, job = ', job
*
      stop
      end
      subroutine foo(C)
      character c
*
      write(*,*) 'fff ', c(1:1)
*
      return
      end
*
      subroutine bar(C)
      character c
      character d
*
      d = c
      c(1:1) = 'y'
      d(1:1) = 'o'
      write(*,*) 'fff ', c(1:1)
      write(*,*) 'eee ', d(1:1)
*
      return
      end
*
      subroutine asdf(C)
      character c
      character d
*
      d = c
      c(:1) = 'y'
      d(:1) = 'o'
      write(*,*) 'fff ', c(:1)
      write(*,*) 'eee ', d(:1)
*
      return
      end
*
      subroutine crap(C)
      character c
      character d
*
      d = c
      c(1:) = 'y'
      d(1:) = 'o'
      write(*,*) 'fff ', c(1:)
      write(*,*) 'eee ', d(1:)
*
      return
      end
