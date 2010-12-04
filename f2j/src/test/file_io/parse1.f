      program parse1
      DIMENSION IADN21(31,20), IADN31(31,10,2)                          00450107
      DIMENSION ITEST(27)                                               00460107
      DIMENSION IDUMP(136)                                              00470107
      CHARACTER*1 NINE,IDUMP                                            00480107
      DATA NINE/'9'/                                                    00490107
      i06=6
      IPROG = 107                                                       01450107
      IFILE = 08                                                        01460107
      ILUN = I06                                                        01470107
      ITOTR = 137                                                       01480107
      IRLGN = 80                                                        01490107
      IEOF = 0000                                                       01500107
      irnum=30
      DO 1146 IRNUM = 1, 31                                             01590107
      DO 1145 J = 1, 10                                                 01600107
      DO 1144 K = 1, 2                                                  01610107
      IADN31(IRNUM,J,K) = IRNUM + J + K + 298                           01620107
 1144 CONTINUE                                                          01630107
 1145 CONTINUE                                                          01640107
 1146 CONTINUE                                                          01650107
 1148 WRITE ( I06, 77752 ) IPROG, IFILE, ILUN, IRNUM, ITOTR, IRLGN, IEOF01740107
     1,((IADN31(IRNUM,J,K), K = 1, 2), J = 1, 10)                       01750107
77752 FORMAT ( I3,2(1I2), 3(1I3), I4, 3(1I3) )                          00600107
      end 
