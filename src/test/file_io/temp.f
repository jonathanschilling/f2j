      program temp
      DIMENSION IADN31(31,10,2)
      DO 1146 IRNUM = 1, 31                                             01590107
      DO 1145 J = 1, 10                                                 01600107
      DO 1144 K = 1, 2                                                  01610107
      IADN31(IRNUM,J,K) = IRNUM + J + K + 298                           01620107
 1144 CONTINUE                                                          01630107
 1145 CONTINUE                                                          01640107
 1146 CONTINUE                                                          01650107
 1148 WRITE(*,*)((IADN31(IRNUM,J,K), K = 1, 2), J = 1, 10)
      end 
