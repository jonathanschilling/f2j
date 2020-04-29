      program simple4
      RCON22 = .9
      RCON23 = -.9
      write(*,100) RCON22, RCON23
      write(*,200) RCON22, RCON23
      write(*,300) RCON22, RCON23
      write(*,400) RCON22, RCON23
 100  format(2F2.1)
 200  format(2F3.1)
 300  format(2F4.1)
 400  format(2F5.1)
      end
