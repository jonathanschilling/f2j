      program test101
      IPROG = 101                                                       01420101
      IFILE = 02                                                        01430101
      ILUN = 7                                                          01440101
      ITOTR = 31                                                        01450101
      IRLGN = 110                                                       01460101
      IEOF = 0000                                                       01470101
      irnum = 31
      RCON21 = 9.                                                       01480101
      RCON22 = .9                                                       01490101
      RCON31 = 21.                                                      01500101
      RCON32 = 2.1                                                      01510101
      RCON33 = .21                                                      01520101
      RCON41 = 512.                                                     01530101
      RCON42 = 51.2                                                     01540101
      RCON43 = 5.12                                                     01550101
      RCON44 = .512                                                     01560101
      RCON51 = 9995.                                                    01570101
      RCON52 = 999.6                                                    01580101
      RCON53 = 99.97                                                    01590101
      RCON54 = 9.998                                                    01600101
      RCON55 = .9999                                                    01610101
      RCON61 = 32764.                                                   01620101
      RCON62 = 3276.5                                                   01630101
      RCON63 = 327.66                                                   01640101
      RCON64 = 32.767                                                   01650101
      RCON65 = 3.2768                                                   01660101
      RCON66 = .32769                                                   01670101
      write(*,100) rc0n22
 100  format(F2.1)
      WRITE(*,77751)IPROG,IFILE,ILUN,IRNUM,ITOTR,IRLGN,IEOF,RCON21,RCO  01700101
     1N22,RCON31,RCON32,RCON33,RCON41,RCON42,RCON43,RCON44,RCON51,RCON5201710101
     2,RCON53,RCON54,RCON55,RCON61,RCON62,RCON63,RCON64,RCON65,RCON66   01720101
77751 FORMAT (I3,2I2,3I3,I4,F2.0,F2.1,F3.0,F3.1,F3.2,F4.0,F4.1,F4.2,F4.300510101
     1,F5.0,F5.1,F5.2,F5.3,F5.4,F6.0,F6.1,F6.2,F6.3,F6.4,F6.5 )         00520101
      end
