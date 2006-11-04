      program intrinsic
      real a,b,c
      integer i,j,k
      double precision x,y,z
      character d, e
      character*8 str
      external achk, dchk
      logical achk, dchk
      real tarray(2)
      external etime
      real etime
      external second
      double precision second
      integer fflag
      fflag = 0
c   ifunc_INT
      write(*,*) 'testing INT'
      i = 12
      j = int(i)
      if(i .ne. j) then
        write(*,*) 'INT failed case 1'
        fflag = fflag + 1
      endif
      a = 0.1
      j = int(a)
      if(j .ne. 0) then
        write(*,*) 'INT failed case 2'
        fflag = fflag + 1
      endif
      a = 3.7
      j = int(a)
      if(j .ne. 3) then
        write(*,*) 'INT failed case 3'
        fflag = fflag + 1
      endif
      a = -3.7
      j = int(a)
      if(j .ne. -3) then
        write(*,*) 'INT failed case 4'
        fflag = fflag + 1
      endif
      x = 0.1
      j = int(x)
      if(j .ne. 0) then
        write(*,*) 'INT failed case 5'
        fflag = fflag + 1
      endif
      x = 3.7
      j = int(x)
      if(j .ne. 3) then
        write(*,*) 'INT failed case 6'
        fflag = fflag + 1
      endif
      x = -3.7
      j = int(x)
      if(j .ne. -3) then
        write(*,*) 'INT failed case 7'
        fflag = fflag + 1
      endif
c   ifunc_IFIX
      write(*,*) 'testing IFIX'
      a = 0.1
      j = ifix(a)
      if(j .ne. 0) then
        write(*,*) 'IFIX failed case 1'
        fflag = fflag + 1
      endif
      a = 3.7
      j = ifix(a)
      if(j .ne. 3) then
        write(*,*) 'IFIX failed case 2'
        fflag = fflag + 1
      endif
      a = -3.7
      j = ifix(a)
      if(j .ne. -3) then
        write(*,*) 'IFIX failed case 3'
        fflag = fflag + 1
      endif
c   ifunc_IDINT
      write(*,*) 'testing IDINT'
      x = 0.1
      j = idint(x)
      if(j .ne. 0) then
        write(*,*) 'IDINT failed case 1'
        fflag = fflag + 1
      endif
      x = 3.7
      j = idint(x)
      if(j .ne. 3) then
        write(*,*) 'IDINT failed case 2'
        fflag = fflag + 1
      endif
      x = -3.7
      j = idint(x)
      if(j .ne. -3) then
        write(*,*) 'IDINT failed case 3'
        fflag = fflag + 1
      endif
c   ifunc_REAL
      write(*,*) 'testing REAL'
      i = 32
      a = real(i)
      if(a .ne. 32.0) then
        write(*,*) 'REAL failed case 1'
        fflag = fflag + 1
      endif
      b = 33.3
      a = real(b)
      if(a .ne. b) then
        write(*,*) 'REAL failed case 2'
        fflag = fflag + 1
      endif
      x = 33.3
      a = real(x)
      if(a .ne. x) then
        write(*,*) 'REAL failed case 3'
        fflag = fflag + 1
      endif
c   ifunc_FLOAT
      write(*,*) 'testing FLOAT'
      i = 32
      a = float(i)
      if(a .ne. 32.0) then
        write(*,*) 'FLOAT failed case 1'
        fflag = fflag + 1
      endif
c   ifunc_SNGL
      write(*,*) 'testing SNGL'
      x = 33.3
      a = sngl(x)
      if(a .ne. x) then
        write(*,*) 'SNGL failed case 1'
        fflag = fflag + 1
      endif
c   ifunc_DBLE
      write(*,*) 'testing DBLE'
      i = 32
      x = dble(i)
      if(x .ne. 32.0) then
        write(*,*) 'DBLE failed case 1'
        fflag = fflag + 1
      endif
      b = 33.3
      x = dble(b)
      if(x .ne. b) then
        write(*,*) 'DBLE failed case 2'
        fflag = fflag + 1
      endif
      y = 33.3
      x = dble(y)
      if(x .ne. y) then
        write(*,*) 'DBLE failed case 3'
        fflag = fflag + 1
      endif
c   ifunc_CMPLX
c     not supported
c   ifunc_ICHAR
      write(*,*) 'testing ICHAR'
      d = 'x'
      i = ichar(d)
      if(i .ne. 120) then
        write(*,*) 'ICHAR failed case 1'
        fflag = fflag + 1
      endif
c   ifunc_CHAR
      write(*,*) 'testing CHAR'
      i = 121
      d = char(i)
      if(d .ne. 'y') then
        write(*,*) 'CHAR failed case 1'
        fflag = fflag + 1
      endif
c   ifunc_AINT
      write(*,*) 'testing AINT'
      a = 75.3
      b = aint(a)
      if(b .ne. 75.0) then
        write(*,*) 'AINT failed case 1'
        fflag = fflag + 1
      endif
c   ifunc_DINT
      write(*,*) 'testing DINT'
      x = 75.3
      y = dint(x)
      if(y .ne. 75.0) then
        write(*,*) 'DINT failed case 1'
        fflag = fflag + 1
      endif
c   ifunc_ANINT
      write(*,*) 'testing ANINT'
      a = 11.2
      b = anint(a)
      if(b .ne. 11.0) then
        write(*,*) 'ANINT failed case 1'
        fflag = fflag + 1
      endif
      a = 11.5
      b = anint(a)
      if(b .ne. 12.0) then
        write(*,*) 'ANINT failed case 2'
        fflag = fflag + 1
      endif
      a = 11.8
      b = anint(a)
      if(b .ne. 12.0) then
        write(*,*) 'ANINT failed case 3'
        fflag = fflag + 1
      endif
c   ifunc_DNINT
      write(*,*) 'testing DNINT'
      x = 11.2
      y = dnint(x)
      if(y .ne. 11.0) then
        write(*,*) 'DNINT failed case 1'
        fflag = fflag + 1
      endif
      x = 11.5
      y = dnint(x)
      if(y .ne. 12.0) then
        write(*,*) 'DNINT failed case 2'
        fflag = fflag + 1
      endif
      x = 11.8
      y = dnint(x)
      if(y .ne. 12.0) then
        write(*,*) 'DNINT failed case 3'
        fflag = fflag + 1
      endif
c   ifunc_NINT
      write(*,*) 'testing NINT'
      a = 11.2
      i = nint(a)
      if(i .ne. 11) then
        write(*,*) 'NINT failed case 1'
        fflag = fflag + 1
      endif
      a = 11.5
      i = nint(a)
      if(i .ne. 12) then
        write(*,*) 'NINT failed case 2'
        fflag = fflag + 1
      endif
      a = 11.8
      i = nint(a)
      if(i .ne. 12) then
        write(*,*) 'NINT failed case 3'
        fflag = fflag + 1
      endif
c   ifunc_IDNINT
      write(*,*) 'testing IDNINT'
      x = 11.2
      i = idnint(x)
      if(i .ne. 11) then
        write(*,*) 'IDNINT failed case 1'
        fflag = fflag + 1
      endif
      x = 11.5
      i = idnint(x)
      if(i .ne. 12) then
        write(*,*) 'IDNINT failed case 2'
        fflag = fflag + 1
      endif
      x = 11.8
      i = idnint(x)
      if(i .ne. 12) then
        write(*,*) 'IDNINT failed case 3'
        fflag = fflag + 1
      endif
c   ifunc_ABS
      write(*,*) 'testing ABS'
      i = 5
      j = abs(i)
      if(j .ne. 5) then
        write(*,*) 'ABS failed case 1'
        fflag = fflag + 1
      endif
      i = -5
      j = abs(i)
      if(j .ne. 5) then
        write(*,*) 'ABS failed case 2'
        fflag = fflag + 1
      endif
      a = 5.5
      b = abs(a)
      if(b .ne. 5.5) then
        write(*,*) 'ABS failed case 3'
        fflag = fflag + 1
      endif
      a = -5.5
      b = abs(a)
      if(b .ne. 5.5) then
        write(*,*) 'ABS failed case 4'
        fflag = fflag + 1
      endif
      x = 5.5
      y = abs(x)
      if(y .ne. 5.5) then
        write(*,*) 'ABS failed case 5'
        fflag = fflag + 1
      endif
      x = -5.5
      y = abs(x)
      if(y .ne. 5.5) then
        write(*,*) 'ABS failed case 6'
        fflag = fflag + 1
      endif
c   ifunc_IABS
      write(*,*) 'testing IABS'
      i = 5
      j = iabs(i)
      if(j .ne. 5) then
        write(*,*) 'IABS failed case 1'
        fflag = fflag + 1
      endif
      i = -5
      j = iabs(i)
      if(j .ne. 5) then
        write(*,*) 'IABS failed case 2'
        fflag = fflag + 1
      endif
c   ifunc_DABS
      write(*,*) 'testing DABS'
      x = 5.5
      y = dabs(x)
      if(y .ne. 5.5) then
        write(*,*) 'DABS failed case 1'
        fflag = fflag + 1
      endif
      x = -5.5
      y = dabs(x)
      if(y .ne. 5.5) then
        write(*,*) 'DABS failed case 2'
        fflag = fflag + 1
      endif
c   ifunc_CABS
c     not implemented
c   ifunc_MOD
      write(*,*) 'testing MOD'
      i = 10
      j = 2
      k = mod(i,j)
      if(k .ne. 0) then
        write(*,*) 'MOD failed case 1'
        fflag = fflag + 1
      endif
      i = 10
      j = 3
      k = mod(i,j)
      if(k .ne. 1) then
        write(*,*) 'MOD failed case 2'
        fflag = fflag + 1
      endif
      a = 23.5
      b = 9.2
      c = mod(a,b)
      if(achk(c, 5.1)) then
        write(*,*) 'MOD failed case 3'
        fflag = fflag + 1
      endif
      x = 23.5
      y = 9.2
      z = mod(x,y)
      if(dchk(z, 5.1D0)) then
        write(*,*) 'MOD failed case 4'
        fflag = fflag + 1
      endif
c   ifunc_AMOD
      write(*,*) 'testing AMOD'
      a = 23.5
      b = 9.2
      c = amod(a,b)
      if(achk(c, 5.1)) then
        write(*,*) 'AMOD failed case 1'
        fflag = fflag + 1
      endif
c   ifunc_DMOD
      write(*,*) 'testing DMOD'
      x = 23.5
      y = 9.2
      z = dmod(x,y)
      if(dchk(z, 5.1D0)) then
        write(*,*) 'DMOD failed case 1'
        fflag = fflag + 1
      endif
c   ifunc_SIGN
      write(*,*) 'testing SIGN'
      i = 10
      j = 5
      k = sign(i,j)
      if(k .ne. 10) then
        write(*,*) 'SIGN failed case 1'
        fflag = fflag + 1
      endif
      i = 10
      j = -5
      k = sign(i,j)
      if(k .ne. -10) then
        write(*,*) 'SIGN failed case 2'
        fflag = fflag + 1
      endif
      i = 10
      j = 0
      k = sign(i,j)
      if(k .ne. 10) then
        write(*,*) 'SIGN failed case 3'
        fflag = fflag + 1
      endif
      a = 10.2
      b = 5.3
      c = sign(a,b)
      if(c .ne. 10.2) then
        write(*,*) 'SIGN failed case 4'
        fflag = fflag + 1
      endif
      a = 10.2
      b = -5.3
      c = sign(a,b)
      if(c .ne. -10.2) then
        write(*,*) 'SIGN failed case 5'
        fflag = fflag + 1
      endif
      a = 10.2
      b = 0.0
      c = sign(a,b)
      if(c .ne. 10.2) then
        write(*,*) 'SIGN failed case 6'
        fflag = fflag + 1
      endif
      x = 10.2
      y = 5.3
      z = sign(x,y)
      if(z .ne. 10.2) then
        write(*,*) 'SIGN failed case 7'
        fflag = fflag + 1
      endif
      x = 10.2
      y = -5.3
      z = sign(x,y)
      if(z .ne. -10.2) then
        write(*,*) 'SIGN failed case 8'
        fflag = fflag + 1
      endif
      x = 10.2
      y = 0.0
      z = sign(x,y)
      if(z .ne. 10.2) then
        write(*,*) 'SIGN failed case 9'
        fflag = fflag + 1
      endif
c   ifunc_ISIGN
      write(*,*) 'testing ISIGN'
      i = 10
      j = 5
      k = isign(i,j)
      if(k .ne. 10) then
        write(*,*) 'ISIGN failed case 1'
        fflag = fflag + 1
      endif
      i = 10
      j = -5
      k = isign(i,j)
      if(k .ne. -10) then
        write(*,*) 'ISIGN failed case 2'
        fflag = fflag + 1
      endif
      i = 10
      j = 0
      k = isign(i,j)
      if(k .ne. 10) then
        write(*,*) 'ISIGN failed case 3'
        fflag = fflag + 1
      endif
c   ifunc_DSIGN
      write(*,*) 'testing DSIGN'
      x = 10.2
      y = 5.3
      z = dsign(x,y)
      if(z .ne. 10.2) then
        write(*,*) 'DSIGN failed case 1'
        fflag = fflag + 1
      endif
      x = 10.2
      y = -5.3
      z = dsign(x,y)
      if(z .ne. -10.2) then
        write(*,*) 'DSIGN failed case 2'
        fflag = fflag + 1
      endif
      x = 10.2
      y = 0.0
      z = dsign(x,y)
      if(z .ne. 10.2) then
        write(*,*) 'DSIGN failed case 3'
        fflag = fflag + 1
      endif
c   ifunc_DIM
      write(*,*) 'testing DIM'
      j = 20
      k = 300
      i = dim(k,j)
      if(i .ne. 280) then
        write(*,*) 'DIM failed case 1'
        fflag = fflag + 1
      endif
      i = dim(j,k)
      if(i .ne. 0) then
        write(*,*) 'DIM failed case 2'
        fflag = fflag + 1
      endif
      b = 20.3
      c = 300.6
      a = dim(c,b)
      if(achk(a, 280.3)) then
        write(*,*) 'DIM failed case 3'
        fflag = fflag + 1
      endif
      a = dim(b,c)
      if(achk(a, 0.0)) then
        write(*,*) 'DIM failed case 4'
        fflag = fflag + 1
      endif
      y = 20.3
      z = 300.6
      x = dim(z,y)
      if(dchk(x, 280.3D0)) then
        write(*,*) 'DIM failed case 5'
        fflag = fflag + 1
      endif
      x = dim(y,z)
      if(dchk(x, 0.0D0)) then
        write(*,*) 'DIM failed case 6'
        fflag = fflag + 1
      endif
c   ifunc_IDIM
      write(*,*) 'testing IDIM'
      j = 20
      k = 300
      i = idim(k,j)
      if(i .ne. 280) then
        write(*,*) 'IDIM failed case 1'
        fflag = fflag + 1
      endif
      i = idim(j,k)
      if(i .ne. 0) then
        write(*,*) 'IDIM failed case 2'
        fflag = fflag + 1
      endif
c   ifunc_DDIM
      write(*,*) 'testing DDIM'
      y = 20.3
      z = 300.6
      x = ddim(z,y)
      if(dchk(x, 280.3D0)) then
        write(*,*) 'DDIM failed case 1'
        fflag = fflag + 1
      endif
      x = ddim(y,z)
      if(dchk(x, 0.0D0)) then
        write(*,*) 'DDIM failed case 2'
        fflag = fflag + 1
      endif
c   ifunc_DPROD
      write(*,*) 'testing DPROD'
      a = 3.6
      b = 93.3
      x = dprod(a,b)
      if(dchk(x, 335.88D0)) then
        write(*,*) 'DPROD failed case 1'
        fflag = fflag + 1
      endif
c   ifunc_MAX
      write(*,*) 'testing MAX'
      i = max(10, 20)
      if(i .ne. 20) then
        write(*,*) 'MAX failed case 1'
        fflag = fflag + 1
      endif
      i = max(20, 10)
      if(i .ne. 20) then
        write(*,*) 'MAX failed case 2'
        fflag = fflag + 1
      endif
      i = max(10, 20, 5)
      if(i .ne. 20) then
        write(*,*) 'MAX failed case 3'
        fflag = fflag + 1
      endif
      i = max(10, 20, 5, 11)
      if(i .ne. 20) then
        write(*,*) 'MAX failed case 4'
        fflag = fflag + 1
      endif
      i = max(10, 20, 5, 11, 77)
      if(i .ne. 77) then
        write(*,*) 'MAX failed case 5'
        fflag = fflag + 1
      endif
      i = max(99, 10, 20, 5, 11, 77)
      if(i .ne. 99) then
        write(*,*) 'MAX failed case 6'
        fflag = fflag + 1
      endif
      a = max(10.1, 20.2)
      if(achk(a , 20.2)) then
        write(*,*) 'MAX failed case 7'
        fflag = fflag + 1
      endif
      a = max(20.2, 10.1)
      if(achk(a , 20.2)) then
        write(*,*) 'MAX failed case 8'
        fflag = fflag + 1
      endif
      a = max(10.1, 20.2, 5.5)
      if(achk(a , 20.2)) then
        write(*,*) 'MAX failed case 9'
        fflag = fflag + 1
      endif
      a = max(10.1, 20.2, 5.5, 11.1)
      if(achk(a , 20.2)) then
        write(*,*) 'MAX failed case 10'
        fflag = fflag + 1
      endif
      a = max(10.1, 20.2, 5.5, 11.1, 77.7)
      if(achk(a , 77.7)) then
        write(*,*) 'MAX failed case 11'
        fflag = fflag + 1
      endif
      a = max(99.9, 10.1, 20.2, 5.5, 11.1, 77.7)
      if(achk(a , 99.9)) then
        write(*,*) 'MAX failed case 12'
        fflag = fflag + 1
      endif
      x = max(10.1d0, 20.2d0)
      if(dchk(x , 20.2d0)) then
        write(*,*) 'MAX failed case 13'
        fflag = fflag + 1
      endif
      x = max(20.2d0, 10.1d0)
      if(dchk(x , 20.2d0)) then
        write(*,*) 'MAX failed case 14'
        fflag = fflag + 1
      endif
      x = max(10.1d0, 20.2d0, 5.5d0)
      if(dchk(x , 20.2d0)) then
        write(*,*) 'MAX failed case 15'
        fflag = fflag + 1
      endif
      x = max(10.1d0, 20.2d0, 5.5d0, 11.1d0)
      if(dchk(x , 20.2d0)) then
        write(*,*) 'MAX failed case 16'
        fflag = fflag + 1
      endif
      x = max(10.1d0, 20.2d0, 5.5d0, 11.1d0, 77.7d0)
      if(dchk(x , 77.7d0)) then
        write(*,*) 'MAX failed case 17'
        fflag = fflag + 1
      endif
      x = max(99.9d0, 10.1d0, 20.2d0, 5.5d0, 11.1d0, 77.7d0)
      if(dchk(x , 99.9d0)) then
        write(*,*) 'MAX failed case 18'
        fflag = fflag + 1
      endif
c   ifunc_MAX0
      write(*,*) 'testing MAX0'
      i = max0(10, 20)
      if(i .ne. 20) then
        write(*,*) 'MAX0 failed case 1'
        fflag = fflag + 1
      endif
      i = max0(20, 10)
      if(i .ne. 20) then
        write(*,*) 'MAX0 failed case 2'
        fflag = fflag + 1
      endif
      i = max0(10, 20, 5)
      if(i .ne. 20) then
        write(*,*) 'MAX0 failed case 3'
        fflag = fflag + 1
      endif
      i = max0(10, 20, 5, 11)
      if(i .ne. 20) then 
        write(*,*) 'MAX0 failed case 4'
        fflag = fflag + 1
      endif
      i = max0(10, 20, 5, 11, 77)
      if(i .ne. 77) then
        write(*,*) 'MAX0 failed case 5'
        fflag = fflag + 1
      endif
      i = max0(99, 10, 20, 5, 11, 77)
      if(i .ne. 99) then
        write(*,*) 'MAX0 failed case 6'
        fflag = fflag + 1
      endif
c   ifunc_AMAX1
      write(*,*) 'testing AMAX1'
      a = amax1(10.1, 20.2)
      if(achk(a , 20.2)) then
        write(*,*) 'AMAX1 failed case 1'
        fflag = fflag + 1
      endif
      a = amax1(20.2, 10.1)
      if(achk(a , 20.2)) then
        write(*,*) 'AMAX1 failed case 2'
        fflag = fflag + 1
      endif
      a = amax1(10.1, 20.2, 5.5)
      if(achk(a , 20.2)) then
        write(*,*) 'AMAX1 failed case 3'
        fflag = fflag + 1
      endif
      a = amax1(10.1, 20.2, 5.5, 11.1)
      if(achk(a , 20.2)) then
        write(*,*) 'AMAX1 failed case 4'
        fflag = fflag + 1
      endif
      a = amax1(10.1, 20.2, 5.5, 11.1, 77.7)
      if(achk(a , 77.7)) then
        write(*,*) 'AMAX1 failed case 5'
        fflag = fflag + 1
      endif
      a = amax1(99.9, 10.1, 20.2, 5.5, 11.1, 77.7)
      if(achk(a , 99.9)) then
        write(*,*) 'AMAX1 failed case 6'
        fflag = fflag + 1
      endif
c   ifunc_DMAX1
      write(*,*) 'testing DMAX1'
      x = dmax1(10.1d0, 20.2d0)
      if(dchk(x , 20.2d0)) then
        write(*,*) 'DMAX1 failed case 1'
        fflag = fflag + 1
      endif
      x = dmax1(20.2d0, 10.1d0)
      if(dchk(x , 20.2d0)) then
        write(*,*) 'DMAX1 failed case 2'
        fflag = fflag + 1
      endif
      x = dmax1(10.1d0, 20.2d0, 5.5d0)
      if(dchk(x , 20.2d0)) then
        write(*,*) 'DMAX1 failed case 3'
        fflag = fflag + 1
      endif
      x = dmax1(10.1d0, 20.2d0, 5.5d0, 11.1d0)
      if(dchk(x , 20.2d0)) then
        write(*,*) 'DMAX1 failed case 4'
        fflag = fflag + 1
      endif
      x = dmax1(10.1d0, 20.2d0, 5.5d0, 11.1d0, 77.7d0)
      if(dchk(x , 77.7d0)) then
        write(*,*) 'DMAX1 failed case 5'
        fflag = fflag + 1
      endif
      x = dmax1(99.9d0, 10.1d0, 20.2d0, 5.5d0, 11.1d0, 77.7d0)
      if(dchk(x , 99.9d0)) then
        write(*,*) 'DMAX1 failed case 6'
        fflag = fflag + 1
      endif
c   ifunc_AMAX0
      write(*,*) 'testing AMAX0'
      a = amax0(10, 20)
      if(achk(a, 20.0)) then
        write(*,*) 'AMAX0 failed case 1'
        fflag = fflag + 1
      endif
      a = amax0(20, 10)
      if(achk(a, 20.0)) then
        write(*,*) 'AMAX0 failed case 2'
        fflag = fflag + 1
      endif
      a = amax0(10, 20, 5)
      if(achk(a, 20.0)) then
        write(*,*) 'AMAX0 failed case 3'
        fflag = fflag + 1
      endif
      a = amax0(10, 20, 5, 11)
      if(achk(a, 20.0)) then  
        write(*,*) 'AMAX0 failed case 4'
        fflag = fflag + 1
      endif
      a = amax0(10, 20, 5, 11, 77)
      if(achk(a, 77.0)) then
        write(*,*) 'AMAX0 failed case 5'
        fflag = fflag + 1
      endif
      a = amax0(99, 10, 20, 5, 11, 77)
      if(achk(a, 99.0)) then
        write(*,*) 'AMAX0 failed case 6'
        fflag = fflag + 1
      endif
c   ifunc_MAX1
      write(*,*) 'testing MAX1'
      i = max1(10.1, 20.2)
      if(i .ne. 20) then
        write(*,*) 'MAX1 failed case 1'
        fflag = fflag + 1
      endif
      i = max1(20.2, 10.1)
      if(i .ne. 20) then
        write(*,*) 'MAX1 failed case 2'
        fflag = fflag + 1
      endif
      i = max1(10.1, 20.2, 5.5)
      if(i .ne. 20) then
        write(*,*) 'MAX1 failed case 3'
        fflag = fflag + 1
      endif
      i = max1(10.1, 20.2, 5.5, 11.1)
      if(i .ne. 20) then
        write(*,*) 'MAX1 failed case 4'
        fflag = fflag + 1
      endif
      i = max1(10.1, 20.2, 5.5, 11.1, 77.7)
      if(i .ne. 77) then
        write(*,*) 'MAX1 failed case 5'
        fflag = fflag + 1
      endif
      i = max1(99.9, 10.1, 20.2, 5.5, 11.1, 77.7)
      if(i .ne. 99) then
        write(*,*) 'MAX1 failed case 6'
        fflag = fflag + 1
      endif
c   ifunc_MIN
      write(*,*) 'testing MIN'
      i = min(10, 20)
      if(i .ne. 10) then
        write(*,*) 'MIN failed case 1'
        fflag = fflag + 1
      endif
      i = min(20, 10)
      if(i .ne. 10) then
        write(*,*) 'MIN failed case 2'
        fflag = fflag + 1
      endif
      i = min(10, 20, 5)
      if(i .ne. 5) then
        write(*,*) 'MIN failed case 3'
        fflag = fflag + 1
      endif
      i = min(10, 20, 5, 11)
      if(i .ne. 5) then
        write(*,*) 'MIN failed case 4'
        fflag = fflag + 1
      endif
      i = min(10, 20, 5, 11, 3)
      if(i .ne. 3) then
        write(*,*) 'MIN failed case 5'
        fflag = fflag + 1
      endif
      i = min(3, 10, 20, 5, 11, 99)
      if(i .ne. 3) then
        write(*,*) 'MIN failed case 6'
        fflag = fflag + 1
      endif
      a = min(110.1, 20.2)
      if(achk(a , 20.2)) then
        write(*,*) 'MIN failed case 7'
        fflag = fflag + 1
      endif
      a = min(20.2, 110.1)
      if(achk(a , 20.2)) then
        write(*,*) 'MIN failed case 8'
        fflag = fflag + 1
      endif
      a = min(10.1, 2.2, 5.5)
      if(achk(a , 2.2)) then
        write(*,*) 'MIN failed case 9'
        fflag = fflag + 1
      endif
      a = min(10.1, 2.2, 5.5, 11.1)
      if(achk(a , 2.2)) then
        write(*,*) 'MIN failed case 10'
        fflag = fflag + 1
      endif
      a = min(10.1, 20.2, 5.5, 11.1, 3.7)
      if(achk(a , 3.7)) then
        write(*,*) 'MIN failed case 11'
        fflag = fflag + 1
      endif
      a = min(2.9, 10.1, 20.2, 5.5, 11.1, 77.7)
      if(achk(a , 2.9)) then
        write(*,*) 'MIN failed case 12'
        fflag = fflag + 1
      endif
      x = min(110.1d0, 20.2d0)
      if(dchk(x , 20.2d0)) then
        write(*,*) 'MIN failed case 13'
        fflag = fflag + 1
      endif
      x = min(20.2d0, 110.1d0)
      if(dchk(x , 20.2d0)) then
        write(*,*) 'MIN failed case 14'
        fflag = fflag + 1
      endif
      x = min(10.1d0, 2.2d0, 5.5d0)
      if(dchk(x , 2.2d0)) then
        write(*,*) 'MIN failed case 15'
        fflag = fflag + 1
      endif
      x = min(10.1d0, 2.2d0, 5.5d0, 11.1d0)
      if(dchk(x , 2.2d0)) then
        write(*,*) 'MIN failed case 16'
        fflag = fflag + 1
      endif
      x = min(10.1d0, 20.2d0, 5.5d0, 11.1d0, 3.7d0)
      if(dchk(x , 3.7d0)) then
        write(*,*) 'MIN failed case 17'
        fflag = fflag + 1
      endif
      x = min(2.9d0, 10.1d0, 20.2d0, 5.5d0, 11.1d0, 77.7d0)
      if(dchk(x , 2.9d0)) then
        write(*,*) 'MIN failed case 18'
        fflag = fflag + 1
      endif
c   ifunc_MIN0
      write(*,*) 'testing MIN0'
      i = min0(10, 20)
      if(i .ne. 10) then
        write(*,*) 'MIN0 failed case 1'
        fflag = fflag + 1
      endif
      i = min0(20, 10)
      if(i .ne. 10) then
        write(*,*) 'MIN0 failed case 2'
        fflag = fflag + 1
      endif
      i = min0(10, 20, 5)
      if(i .ne. 5) then 
        write(*,*) 'MIN0 failed case 3'
        fflag = fflag + 1
      endif
      i = min0(10, 20, 5, 11)
      if(i .ne. 5) then 
        write(*,*) 'MIN0 failed case 4'
        fflag = fflag + 1
      endif
      i = min0(10, 20, 5, 11, 3)
      if(i .ne. 3) then 
        write(*,*) 'MIN0 failed case 5'
        fflag = fflag + 1
      endif
      i = min0(3, 10, 20, 5, 11, 99)
      if(i .ne. 3) then
        write(*,*) 'MIN0 failed case 6'
        fflag = fflag + 1
      endif
c   ifunc_AMIN1
      write(*,*) 'testing AMIN1'
      a = amin1(110.1, 20.2)
      if(achk(a , 20.2)) then
        write(*,*) 'AMIN1 failed case 1'
        fflag = fflag + 1
      endif
      a = amin1(20.2, 110.1)
      if(achk(a , 20.2)) then
        write(*,*) 'AMIN1 failed case 2'
        fflag = fflag + 1
      endif
      a = amin1(10.1, 2.2, 5.5)
      if(achk(a , 2.2)) then
        write(*,*) 'AMIN1 failed case 3'
        fflag = fflag + 1
      endif
      a = amin1(10.1, 2.2, 5.5, 11.1)
      if(achk(a , 2.2)) then
        write(*,*) 'AMIN1 failed case 4'
        fflag = fflag + 1
      endif
      a = amin1(10.1, 20.2, 5.5, 11.1, 3.7)
      if(achk(a , 3.7)) then
        write(*,*) 'AMIN1 failed case 5'
        fflag = fflag + 1
      endif
      a = amin1(2.9, 10.1, 20.2, 5.5, 11.1, 77.7)
      if(achk(a , 2.9)) then
        write(*,*) 'AMIN1 failed case 6'
        fflag = fflag + 1
      endif
c   ifunc_DMIN1
      write(*,*) 'testing DMIN1'
      x = dmin1(110.1d0, 20.2d0)
      if(dchk(x , 20.2d0)) then
        write(*,*) 'DMIN1 failed case 1'
        fflag = fflag + 1
      endif
      x = dmin1(20.2d0, 110.1d0)
      if(dchk(x , 20.2d0)) then
        write(*,*) 'DMIN1 failed case 2'
        fflag = fflag + 1
      endif
      x = dmin1(10.1d0, 2.2d0, 5.5d0)
      if(dchk(x , 2.2d0)) then
        write(*,*) 'DMIN1 failed case 3'
        fflag = fflag + 1
      endif
      x = dmin1(10.1d0, 2.2d0, 5.5d0, 11.1d0)
      if(dchk(x , 2.2d0)) then
        write(*,*) 'DMIN1 failed case 4'
        fflag = fflag + 1
      endif
      x = dmin1(10.1d0, 20.2d0, 5.5d0, 11.1d0, 3.7d0)
      if(dchk(x , 3.7d0)) then
        write(*,*) 'DMIN1 failed case 5'
        fflag = fflag + 1
      endif
      x = dmin1(2.9d0, 10.1d0, 20.2d0, 5.5d0, 11.1d0, 77.7d0)
      if(dchk(x , 2.9d0)) then
        write(*,*) 'DMIN1 failed case 6'
        fflag = fflag + 1
      endif
c   ifunc_AMIN0
      write(*,*) 'testing AMIN0'
      a = amin0(10, 20)
      if(achk(a, 10.0)) then
        write(*,*) 'AMIN0 failed case 1'
        fflag = fflag + 1
      endif
      a = amin0(20, 10)
      if(achk(a, 10.0)) then
        write(*,*) 'AMIN0 failed case 2'
        fflag = fflag + 1
      endif
      a = amin0(10, 20, 5)
      if(achk(a, 5.0)) then
        write(*,*) 'AMIN0 failed case 3'
        fflag = fflag + 1
      endif
      a = amin0(10, 20, 5, 11)
      if(achk(a, 5.0)) then
        write(*,*) 'AMIN0 failed case 4'
        fflag = fflag + 1
      endif
      a = amin0(10, 20, 5, 11, 3)
      if(achk(a, 3.0)) then
        write(*,*) 'AMIN0 failed case 5'
        fflag = fflag + 1
      endif
      a = amin0(3, 10, 20, 5, 11, 99)
      if(achk(a, 3.0)) then
        write(*,*) 'AMIN0 failed case 6'
        fflag = fflag + 1
      endif
c   ifunc_MIN1
      write(*,*) 'testing MIN1'
      i = min1(110.1, 20.2)
      if(i .ne. 20) then
        write(*,*) 'MIN1 failed case 1'
        fflag = fflag + 1
      endif
      i = min1(20.2, 110.1)
      if(i .ne. 20) then
        write(*,*) 'MIN1 failed case 2'
        fflag = fflag + 1
      endif
      i = min1(10.1, 2.2, 5.5)
      if(i .ne. 2) then
        write(*,*) 'MIN1 failed case 3'
        fflag = fflag + 1
      endif
      i = min1(10.1, 2.2, 5.5, 11.1)
      if(i .ne. 2) then
        write(*,*) 'MIN1 failed case 4'
        fflag = fflag + 1
      endif
      i = min1(10.1, 20.2, 5.5, 11.1, 3.7)
      if(i .ne. 3) then
        write(*,*) 'MIN1 failed case 5'
        fflag = fflag + 1
      endif
      i = min1(2.9, 10.1, 20.2, 5.5, 11.1, 77.7)
      if(i .ne. 2) then
        write(*,*) 'MIN1 failed case 6'
        fflag = fflag + 1
      endif
c   ifunc_LEN
c     this intrinsic doesn't work in all cases
c     see comment in codegen.c
      write(*,*) 'testing LEN'
      str = 'foo'
      i = len(str)
      if(i .ne. 8) then
        write(*,*) 'LEN failed case 1'
        fflag = fflag + 1
      endif
c   ifunc_INDEX
c     not implemented
c   ifunc_AIMAG
c     not implemented
c   ifunc_CONJG
c     not implemented
c   ifunc_SQRT
      write(*,*) 'testing SQRT'
      b = 12.25
      a = sqrt(b)
      if(achk(a, 3.5)) then
        write(*,*) 'SQRT failed case 1'
        fflag = fflag + 1
      endif
      y = 12.25
      x = sqrt(y)
      if(dchk(x, 3.5d0)) then
        write(*,*) 'SQRT failed case 2'
        fflag = fflag + 1
      endif
c   ifunc_DSQRT
      write(*,*) 'testing DSQRT'
      y = 12.25
      x = dsqrt(y)
      if(dchk(x, 3.5d0)) then
        write(*,*) 'DSQRT failed case 1'
        fflag = fflag + 1
      endif
c   ifunc_CSQRT
c     not implemented
c   ifunc_EXP
      write(*,*) 'testing EXP'
      b = 0.0
      a = exp(b)
      if(achk(a, 1.0)) then
        write(*,*) 'EXP failed case 1'
        fflag = fflag + 1
      endif
      y = 0.0
      x = exp(y)
      if(dchk(x, 1.0d0)) then
        write(*,*) 'EXP failed case 2'
        fflag = fflag + 1
      endif
c   ifunc_DEXP
      write(*,*) 'testing DEXP'
      y = 0.0
      x = dexp(y)
      if(dchk(x, 1.0d0)) then
        write(*,*) 'DEXP failed case 2'
        fflag = fflag + 1
      endif
c   ifunc_CEXP
c     not implemented
c   ifunc_LOG
      write(*,*) 'testing LOG'
      b = 1.0
      a = log(b)
      if(achk(a, 0.0)) then
        write(*,*) 'LOG failed case 1'
        fflag = fflag + 1
      endif
      y = 1.0
      x = log(y)
      if(dchk(x, 0.0d0)) then
        write(*,*) 'LOG failed case 2'
        fflag = fflag + 1
      endif
c   ifunc_ALOG
      write(*,*) 'testing ALOG'
      b = 1.0
      a = alog(b)
      if(achk(a, 0.0)) then
        write(*,*) 'ALOG failed case 1'
        fflag = fflag + 1
      endif
c   ifunc_DLOG
      write(*,*) 'testing DLOG'
      y = 1.0
      x = dlog(y)
      if(dchk(x, 0.0d0)) then
        write(*,*) 'DLOG failed case 2'
        fflag = fflag + 1
      endif
c   ifunc_CLOG
c     not implemented
c   ifunc_LOG10
      write(*,*) 'testing LOG10'
      b = 100.0
      a = log10(b)
      if(achk(a, 2.0)) then
        write(*,*) 'LOG10 failed case 1'
        fflag = fflag + 1
      endif
      y = 100.0
      x = log10(y)
      if(dchk(x, 2.0d0)) then
        write(*,*) 'LOG10 failed case 2'
        fflag = fflag + 1
      endif
c   ifunc_ALOG10
      write(*,*) 'testing ALOG10'
      b = 100.0
      a = alog10(b)
      if(achk(a, 2.0)) then
        write(*,*) 'ALOG10 failed case 1'
        fflag = fflag + 1
      endif
c   ifunc_DLOG10
      write(*,*) 'testing DLOG10'
      y = 100.0
      x = dlog10(y)
      if(dchk(x, 2.0d0)) then
        write(*,*) 'DLOG10 failed case 2'
        fflag = fflag + 1
      endif
c   ifunc_SIN
      write(*,*) 'testing SIN'
      b = 3.14159 / 2.0
      a = sin(b)
      if(achk(a, 1.0)) then
        write(*,*) 'SIN failed case 1'
        fflag = fflag + 1
      endif
      y = 3.14159d0 / 2.0d0
      x = sin(y)
      if(dchk(x, 1.0d0)) then
        write(*,*) 'SIN failed case 2'
        fflag = fflag + 1
      endif
c   ifunc_DSIN
      write(*,*) 'testing DSIN'
      y = 3.14159d0 / 2.0d0
      x = dsin(y)
      if(dchk(x, 1.0d0)) then
        write(*,*) 'DSIN failed case 2'
        fflag = fflag + 1
      endif
c   ifunc_CSIN
c     not implemented
c   ifunc_COS
      write(*,*) 'testing COS'
      b = 0.0
      a = cos(b)
      if(achk(a, 1.0)) then
        write(*,*) 'COS failed case 1'
        fflag = fflag + 1
      endif
      y = 0.0d0
      x = cos(y)
      if(dchk(x, 1.0d0)) then
        write(*,*) 'COS failed case 2'
        fflag = fflag + 1
      endif
c   ifunc_DCOS
      write(*,*) 'testing DCOS'
      y = 0.0d0
      x = dcos(y)
      if(dchk(x, 1.0d0)) then
        write(*,*) 'DCOS failed case 2'
        fflag = fflag + 1
      endif
c   ifunc_CCOS
c     not implemented
c   ifunc_TAN
      write(*,*) 'testing TAN'
      b = 3.14159 / 4.0
      a = tan(b)
      if(achk(a, 1.0)) then
        write(*,*) 'TAN failed case 1'
        fflag = fflag + 1
      endif
      y = 3.14159d0 / 4.0d0
      x = tan(y)
      if(dchk(x, 1.0d0)) then
        write(*,*) 'TAN failed case 2'
        fflag = fflag + 1
      endif
c   ifunc_DTAN
      write(*,*) 'testing DTAN'
      y = 3.14159d0 / 4.0d0
      x = dtan(y)
      if(dchk(x, 1.0d0)) then
        write(*,*) 'DTAN failed case 2'
        fflag = fflag + 1
      endif
c   ifunc_ASIN
      write(*,*) 'testing ASIN'
      a = asin(1.0)
      b = 3.14159 / 2.0
      if(achk(a, b)) then
        write(*,*) 'ASIN failed case 1'
        fflag = fflag + 1
      endif
      x = asin(1.0)
      y = 3.14159d0 / 2.0d0
      if(dchk(x, y)) then
        write(*,*) 'ASIN failed case 2'
        fflag = fflag + 1
      endif
c   ifunc_DASIN
      write(*,*) 'testing DASIN'
      x = dasin(1.0d0)
      y = 3.14159d0 / 2.0d0
      if(dchk(x, y)) then
        write(*,*) 'DASIN failed case 1'
        fflag = fflag + 1
      endif
c   ifunc_ACOS
      write(*,*) 'testing ACOS'
      b = 1.0
      a = acos(b)
      if(achk(a, 0.0)) then
        write(*,*) 'ACOS failed case 1'
        fflag = fflag + 1
      endif
      y = 1.0d0
      x = acos(y)
      if(dchk(x, 0.0d0)) then
        write(*,*) 'ACOS failed case 2'
        fflag = fflag + 1
      endif
c   ifunc_DACOS
      write(*,*) 'testing DACOS'
      y = 1.0d0
      x = dacos(y)
      if(dchk(x, 0.0d0)) then
        write(*,*) 'DACOS failed case 2'
        fflag = fflag + 1
      endif
c   ifunc_ATAN
      write(*,*) 'testing ATAN'
      a = atan(1.0)
      b = 3.14159 / 4.0
      if(achk(a, b)) then
        write(*,*) 'ATAN failed case 1'
        fflag = fflag + 1
      endif
      x = atan(1.0d0)
      y = 3.14159d0 / 4.0d0
      if(dchk(x, y)) then
        write(*,*) 'ATAN failed case 2'
        fflag = fflag + 1
      endif
c   ifunc_DATAN
      write(*,*) 'testing DATAN'
      x = datan(1.0d0)
      y = 3.14159d0 / 4.0d0
      if(dchk(x, y)) then
        write(*,*) 'DATAN failed case 2'
        fflag = fflag + 1
      endif
c   ifunc_ATAN2
      write(*,*) 'testing ATAN2'
      c = 5.0
      a = atan2(5.0, 5.0)
      b = 3.14159 / 4.0
      if(achk(a, b)) then
        write(*,*) 'ATAN2 failed case 1'
        fflag = fflag + 1
      endif
      x = atan2(5.0d0, 5.0d0)
      y = 3.14159d0 / 4.0d0
      if(dchk(x, y)) then
        write(*,*) 'ATAN2 failed case 2'
        fflag = fflag + 1
      endif
c   ifunc_DATAN2
      write(*,*) 'testing DATAN2'
      x = datan2(5.0d0, 5.0d0)
      y = 3.14159d0 / 4.0d0
      if(dchk(x, y)) then
        write(*,*) 'DATAN2 failed case 2'
        fflag = fflag + 1
      endif
c   ifunc_SINH
      write(*,*) 'testing SINH'
      b = 3.0
      a = sinh(b)
      if(achk(a, 10.0)) then
        write(*,*) 'SINH failed case 1'
        fflag = fflag + 1
      endif
      y = 3.0
      x = sinh(y)
      if(dchk(x, 10.0d0)) then
        write(*,*) 'SINH failed case 2'
        fflag = fflag + 1
      endif
c   ifunc_DSINH
      write(*,*) 'testing DSINH'
      y = 3.0d0
      x = dsinh(y)
      if(dchk(x, 10.0d0)) then
        write(*,*) 'DSINH failed case 1'
        fflag = fflag + 1
      endif
c   ifunc_COSH
      write(*,*) 'testing COSH'
      b = 0.0
      a = cosh(b)
      if(achk(a, 1.0)) then
        write(*,*) 'COSH failed case 1'
        fflag = fflag + 1
      endif
      y = 0.0d0
      x = cosh(y)
      if(dchk(x, 1.0d0)) then
        write(*,*) 'COSH failed case 2'
        fflag = fflag + 1
      endif
c   ifunc_DCOSH
      write(*,*) 'testing DCOSH'
      y = 0.0d0
      x = dcosh(y)
      if(dchk(x, 1.0d0)) then
        write(*,*) 'DCOSH failed case 1'
        fflag = fflag + 1
      endif
c   ifunc_TANH
      write(*,*) 'testing TANH'
      b = 10.0
      a = tanh(b)
      if(achk(a, 1.0)) then
        write(*,*) 'TANH failed case 1'
        fflag = fflag + 1
      endif
      y = 10.0d0
      x = tanh(y)
      if(dchk(x, 1.0d0)) then
        write(*,*) 'TANH failed case 2'
        fflag = fflag + 1
      endif
c   ifunc_DTANH
      write(*,*) 'testing DTANH'
      y = 10.0d0
      x = dtanh(y)
      if(dchk(x, 1.0d0)) then
        write(*,*) 'DTANH failed case 1'
        fflag = fflag + 1
      endif
c   ifunc_LGE
      write(*,*) 'testing LGE'
      if(lge('foo', 'foobar')) then
        write(*,*) 'LGE failed case 1'
        fflag = fflag + 1
      endif
      if(.not.lge('foobar', 'foo')) then
        write(*,*) 'LGE failed case 2'
        fflag = fflag + 1
      endif
      if(lge('x', 'y')) then
        write(*,*) 'LGE failed case 3'
        fflag = fflag + 1
      endif
      if(.not.lge('y', 'x')) then
        write(*,*) 'LGE failed case 4'
        fflag = fflag + 1
      endif
      if(.not.lge('x', 'x')) then
        write(*,*) 'LGE failed case 5'
        fflag = fflag + 1
      endif
c   ifunc_LGT
      write(*,*) 'testing LGT'
      if(lgt('foo', 'foobar')) then
        write(*,*) 'LGT failed case 1'
        fflag = fflag + 1
      endif
      if(.not.lgt('foobar', 'foo')) then
        write(*,*) 'LGT failed case 2'
        fflag = fflag + 1
      endif
      if(lgt('x', 'y')) then
        write(*,*) 'LGT failed case 3'
        fflag = fflag + 1
      endif
      if(.not.lgt('y', 'x')) then
        write(*,*) 'LGT failed case 4'
        fflag = fflag + 1
      endif
      if(lgt('x', 'x')) then
        write(*,*) 'LGT failed case 5'
        fflag = fflag + 1
      endif
c   ifunc_LLE
      write(*,*) 'testing LLE'
      if(.not.lle('foo', 'foobar')) then
        write(*,*) 'LLE failed case 1'
        fflag = fflag + 1
      endif
      if(lle('foobar', 'foo')) then
        write(*,*) 'LLE failed case 2'
        fflag = fflag + 1
      endif
      if(.not.lle('x', 'y')) then
        write(*,*) 'LLE failed case 3'
        fflag = fflag + 1
      endif
      if(lle('y', 'x')) then
        write(*,*) 'LLE failed case 4'
        fflag = fflag + 1
      endif
      if(.not.lle('x', 'x')) then
        write(*,*) 'LLE failed case 5'
        fflag = fflag + 1
      endif
c   ifunc_LLT
      write(*,*) 'testing LLT'
      if(.not.llt('foo', 'foobar')) then
        write(*,*) 'LLT failed case 1'
        fflag = fflag + 1
      endif
      if(llt('foobar', 'foo')) then
        write(*,*) 'LLT failed case 2'
        fflag = fflag + 1
      endif
      if(.not.llt('x', 'y')) then
        write(*,*) 'LLT failed case 3'
        fflag = fflag + 1
      endif
      if(llt('y', 'x')) then
        write(*,*) 'LLT failed case 4'
        fflag = fflag + 1
      endif
      if(llt('x', 'x')) then
        write(*,*) 'LLT failed case 5'
        fflag = fflag + 1
      endif
c   ifunc_ETIME
      write(*,*) 'testing ETIME'
      do 10 i = 1, 5000000
       j = j + 1
  10  continue
      tarray(1) = 0.0
      b = etime(tarray)
      if(b .le. 0.0) then
        write(*,*) 'Warning: ETIME might have failed'
      endif
c   ifunc_SECOND
      write(*,*) 'testing SECOND'
      y = second()
      do 15 i = 1, 5000000
       j = j + 1
  15  continue
      x = second()
      z = x-y
      if(z .le. 0.0d0) then
        write(*,*) 'Warning: SECOND might have failed'
      endif
      if(fflag .gt. 0) then
        write(*,*) 'FAILURE COUNT: ', fflag
      else
        write(*,*) 'SUCCESS'
      endif
      stop
      end
      logical function achk(a, b)
      real a, b
c
      if(abs(a-b) .gt. 0.05) then
        achk = .true.
      else
        achk = .false.
      endif 
c
      return
      end
c
      logical function dchk(a, b)
      double precision a, b
c
      if(abs(a-b) .gt. 0.05) then
        dchk = .true.
      else
        dchk = .false.
      endif
c
      return
      end
