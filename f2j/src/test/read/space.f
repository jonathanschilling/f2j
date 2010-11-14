      program space
      integer num1, num2, num3
      NUM1 = 123
      NUM2 = 456
      NUM3 =9
c
      write(6,15) NUM1, NUM2, NUM3
 15   format (1x,I3,1x,I3,1x,I3)  
c          
c          output:  123 456   9
c                  ^   ^   ^^^
      write(6,18) NUM1, NUM2, NUM3
 18   format (1x,3(I2,1x))  
c
      end
