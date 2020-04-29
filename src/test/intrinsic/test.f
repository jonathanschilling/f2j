      program test
      integer x
      character *10 name, opts
      name = 'foo'
      opts = 'opts'
      x = iparmq(1, name, opts, 2, 3, 4, 5)
      stop
      end
      INTEGER FUNCTION IPARMQ( ISPEC, NAME, OPTS, N, ILO, IHI, LWORK )
      INTEGER            IHI, ILO, ISPEC, LWORK, N
      CHARACTER          NAME*( * ), OPTS*( * )
*     CHARACTER          NAME( * ), OPTS( * )
*
      write(*,*) 'hello world, name = ', name, ', opts = ', opts
      iparmq = 10
      return
      end
