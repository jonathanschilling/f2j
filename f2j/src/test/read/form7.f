       program form7
       integer x,y
*
       write(*,*) 'enter two integers for each read, fmt=(2I4)'
*       read(*,'(2I4)') x,y
       read(*,'(2I4)') x,y
       write(*,*) x,y
*       read(*,'(2I4)',end=100) x,y
       read(*,'(2I4)',end=100) x,y
       write(*,*) x,y
*       read(*,'(2I4)',err=110) x,y
       read(*,'(2I4)',err=110) x,y
       write(*,*) x,y
*       read(*,'(2I4)',err=120,end=130) x,y
       read(*,'(2I4)',err=120,end=130) x,y
       write(*,*) x,y
*       read(*,'(2I4)',end=200,iostat=k) x,y
       read(*,'(2I4)',end=200,iostat=k) x,y
       write(*,*) x,y
*       read(*,'(2I4)',err=210,iostat=k) x,y
       read(*,'(2I4)',err=210,iostat=k) x,y
       write(*,*) x,y
*       read(*,'(2I4)',err=220,end=230,iostat=k) x,y
       read(*,'(2I4)',err=220,end=230,iostat=k) x,y
       write(*,*) x,y
*
       stop
 100   write(*,*) 'ending at 100'
       stop
 110   write(*,*) 'ending at 110'
       stop
 120   write(*,*) 'ending at 120'
       stop
 130   write(*,*) 'ending at 130'
       stop
 200   write(*,*) 'ending at 200'
       stop
 210   write(*,*) 'ending at 210'
       stop
 220   write(*,*) 'ending at 220'
       stop
 230   write(*,*) 'ending at 230'
       stop
       end
