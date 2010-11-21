      program name
      integer unit, err, rec, iostat
      integer error
*
      error = 1234
      unit=6
      write(*,*) error
      write(unit=unit,fmt=*) 'hi', unit
*
      end
