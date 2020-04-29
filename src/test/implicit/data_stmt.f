      program data_stmt 
       integer c
       integer a(5, 5)
       integer b(2, 2) 
       integer i(5, 5)
       character cc  

       data h/1/ 
       data x/1.1/
       data i(1,1)/1/
       data c/1.1/
       data z/1/
       data j/1.1/
       data a(1,1)/1.1/
       data b/4 * 1.1/

       write(*, *)'z = ', z 
       write(*, *)'j = ', j 
      end
