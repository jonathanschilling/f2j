      program label
      IVON01 = 1
      DO 1252 I1=1,4,1                                                  05390012
      DO 1252 I2=1,5,1                                                  05400012
      DO 1252 I3=2,8,1                                                  05410012
      IVON01=IVON01+1                                                   05420012
      IF ( I3 - 9 ) 1252, 1252, 1253                                    05430012
 1252 CONTINUE                                                          05440012
 1253 CONTINUE                                                          05440012
      end
