      program test118
      iczero = 0
      IF (ICZERO) 31180, 1180, 31180                                    03260012
 1180 CONTINUE                                                          03270012
      IVON01=0                                                          03280012
      DO 1182 M=1,2,1                                                   03290012
      DO 1182 N=1,10,1                                                  03300012
 1182 IVON01 = IVON01 + 1                                               03310012
      write(*,*) ivon01
      GO TO 41180                                                       03320012
31180 IVDELE = IVDELE + 1                                               03330012
      WRITE (*,80003) IVTNUM                                          03340012
      IF (ICZERO) 41180, 1191, 41180                                    03350012
41180 IF (IVON01 - 20 )  21180, 11180, 21180                            03360012
11180 IVPASS = IVPASS + 1                                               03370012
      WRITE (*,80001) IVTNUM                                          03380012
      GO TO 1191                                                        03390012
21180 IVFAIL = IVFAIL + 1                                               03400012
      IVCOMP=IVON01                                                     03410012
      IVCORR=20                                                         03420012
      WRITE (*,80004) IVTNUM, IVCOMP ,IVCORR                          03430012
 1191 CONTINUE                                                          03440012
      IVTNUM = 119                                                      03450012
80001 FORMAT (1H ,4X,I5,7X,4HPASS)                                      05920012
80002 FORMAT (1H ,4X,I5,7X,4HFAIL)                                      05930012
80003 FORMAT (1H ,4X,I5,7X,7HDELETED)                                   05940012
80004 FORMAT (1H ,4X,I5,7X,4HFAIL,10X,I6,9X,I6)                         05950012
80005 FORMAT (1H ,4X,I5,7X,4HFAIL,4X,E12.5,3X,E12.5)                    05960012
      end
