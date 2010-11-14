      program unftest
      integer i,j,k
      real a,b,c
      double precision x,y,z
      character*5 s,t
*
      i = 11
      j = 22
      k = 33
      a = 4.4
      b = 5.5
      c = 6.6
      x = 12.3d0
      y = 45.6d0
      z = 78.9d0
      s = 'abcde'
      t = 'vwxyz'
*
      write(*,*) s,i,a,x,s,j,b,y,t,k,z,t
      write(*,*) s
      write(*,*) k
      write(*,*) i,j,k
      write(*,*) i,b,j,k
      write(*,*) i,y,j,k
*
      end
