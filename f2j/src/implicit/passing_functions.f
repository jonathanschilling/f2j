      program passing_functions
      integer i
      integer func
      external func

      call c(func)
      end
      subroutine c(func)
      integer i
      integer func
      external func

      i = func() 
      end
      integer function func()
      integer func

      func = 3

      return 
      end
