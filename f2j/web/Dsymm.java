import org.netlib.util.Dummy;
import org.netlib.util.Xerbla;

/*
*  Produced by f2java.  f2java is part of the Fortran-
*  -to-Java project at the University of Tennessee Netlib
*  numerical software repository.
*
*  Original authorship for the BLAS and LAPACK numerical
*  routines may be found in the Fortran source, available at
*  www.netlib.org.
*
*  Fortran input file: bigfile.f
*
*  The f2j compiler code was written by
*  David M. Doolin (doolin@cs.utk.edu) and
*  Keith  Seymour (seymour@cs.utk.edu)
*/
public class Dsymm 
{

  // $AUTO: Class fields.

  // Type declarations.
  static boolean upper = false;
  static int i = 0;
  static int info = 0;
  static int j = 0;
  static int k = 0;
  static int nrowa = 0;
  static double temp1 = 0.0;
  static double temp2 = 0.0;
  static double one = 1.0e+0;
  static double zero = 0.0e+0;


  // $AUTO: Class methods.

  public static void dsymm(java.lang.String side, java.lang.String uplo, int m, int n, double alpha, double [] a, int _a_offset, int lda, double [] b, int _b_offset, int ldb, double beta, double [] c, int _c_offset, int Ldc) 
  {
    // Executable code.
    if ((side.charAt(0) == "L".charAt(0))) 
    { 
      nrowa = m; 
    // Close if()
    } 
    else 
    { 
      nrowa = n; 
    //  Close else.
    } 
    upper = (uplo.charAt(0) == "U".charAt(0)); 
    info = 0; 
    if ((!(side.charAt(0) == "L".charAt(0))) && (!(side.charAt(0) == "R".charAt(0)))) 
    { 
      info = 1; 
    // Close if()
    } 
    else if ((!upper) && (!(uplo.charAt(0) == "L".charAt(0)))) 
    { 
      info = 2; 
    // Close else if()
    } 
    else if (m < 0) 
    { 
      info = 3; 
    // Close else if()
    } 
    else if (n < 0) 
    { 
      info = 4; 
    // Close else if()
    } 
    else if (lda < Math.max(1, nrowa)) 
    { 
      info = 7; 
    // Close else if()
    } 
    else if (ldb < Math.max(1, m)) 
    { 
      info = 9; 
    // Close else if()
    } 
    else if (Ldc < Math.max(1, m)) 
    { 
      info = 12; 
    // Close else if()
    } 
    if (info != 0) 
    { 
      Xerbla.xerbla("DSYMM ", info); 
      Dummy.go_to("Dsymm", 999999); 
    // Close if()
    } 
    if ((m == 0) || (n == 0) || ((alpha == zero) && (beta == one))) 
    { 
      Dummy.go_to("Dsymm", 999999); 
    } 
    if (alpha == zero) 
    { 
      if (beta == zero) 
      { 
	
	{ 
	  forloop20 : 
	  for (j = 1; j <= n; j++) 
	  { 
	    
	    { 
	      forloop10 : 
	      for (i = 1; i <= m; i++) 
	      { 
		c[(i) - 1 + (j - 1) * Ldc + _c_offset] = zero; 
		Dummy.label("Dsymm", 10); 
	      //  Close for() loop.
	      } 
	    } 
	    Dummy.label("Dsymm", 20); 
	  //  Close for() loop.
	  } 
	} 
      // Close if()
      } 
      else 
      { 
	
	{ 
	  forloop40 : 
	  for (j = 1; j <= n; j++) 
	  { 
	    
	    { 
	      forloop30 : 
	      for (i = 1; i <= m; i++) 
	      { 
		c[(i) - 1 + (j - 1) * Ldc + _c_offset] = beta * c[(i) - 1 + (j - 1) * Ldc + _c_offset]; 
		Dummy.label("Dsymm", 30); 
	      //  Close for() loop.
	      } 
	    } 
	    Dummy.label("Dsymm", 40); 
	  //  Close for() loop.
	  } 
	} 
      //  Close else.
      } 
      Dummy.go_to("Dsymm", 999999); 
    // Close if()
    } 
    if ((side.charAt(0) == "L".charAt(0))) 
    { 
      if (upper) 
      { 
	
	{ 
	  forloop70 : 
	  for (j = 1; j <= n; j++) 
	  { 
	    
	    { 
	      forloop60 : 
	      for (i = 1; i <= m; i++) 
	      { 
		temp1 = alpha * b[(i) - 1 + (j - 1) * ldb + _b_offset]; 
		temp2 = zero; 
		{ 
		  forloop50 : 
		  for (k = 1; k <= i - 1; k++) 
		  { 
		    c[(k) - 1 + (j - 1) * Ldc + _c_offset] = c[(k) - 1 + (j - 1) * Ldc + _c_offset] + temp1 * a[(k) - 1 + (i - 1) * lda + _a_offset]; 
		    temp2 = temp2 + b[(k) - 1 + (j - 1) * ldb + _b_offset] * a[(k) - 1 + (i - 1) * lda + _a_offset]; 
		    Dummy.label("Dsymm", 50); 
		  //  Close for() loop.
		  } 
		} 
		if (beta == zero) 
		{ 
		  c[(i) - 1 + (j - 1) * Ldc + _c_offset] = temp1 * a[(i) - 1 + (i - 1) * lda + _a_offset] + alpha * temp2; 
		// Close if()
		} 
		else 
		{ 
		  c[(i) - 1 + (j - 1) * Ldc + _c_offset] = beta * c[(i) - 1 + (j - 1) * Ldc + _c_offset] + temp1 * a[(i) - 1 + (i - 1) * lda + _a_offset] + alpha * temp2; 
		//  Close else.
		} 
		Dummy.label("Dsymm", 60); 
	      //  Close for() loop.
	      } 
	    } 
	    Dummy.label("Dsymm", 70); 
	  //  Close for() loop.
	  } 
	} 
      // Close if()
      } 
      else 
      { 
	
	{ 
	  forloop100 : 
	  for (j = 1; j <= n; j++) 
	  { 
	    
	    { 
	      int _i_inc = -1; 
	      forloop90 : 
	      for (i = m; (_i_inc < 0)? i >= 1 : i <= 1; i += _i_inc) 
	      { 
		temp1 = alpha * b[(i) - 1 + (j - 1) * ldb + _b_offset]; 
		temp2 = zero; 
		{ 
		  forloop80 : 
		  for (k = i + 1; k <= m; k++) 
		  { 
		    c[(k) - 1 + (j - 1) * Ldc + _c_offset] = c[(k) - 1 + (j - 1) * Ldc + _c_offset] + temp1 * a[(k) - 1 + (i - 1) * lda + _a_offset]; 
		    temp2 = temp2 + b[(k) - 1 + (j - 1) * ldb + _b_offset] * a[(k) - 1 + (i - 1) * lda + _a_offset]; 
		    Dummy.label("Dsymm", 80); 
		  //  Close for() loop.
		  } 
		} 
		if (beta == zero) 
		{ 
		  c[(i) - 1 + (j - 1) * Ldc + _c_offset] = temp1 * a[(i) - 1 + (i - 1) * lda + _a_offset] + alpha * temp2; 
		// Close if()
		} 
		else 
		{ 
		  c[(i) - 1 + (j - 1) * Ldc + _c_offset] = beta * c[(i) - 1 + (j - 1) * Ldc + _c_offset] + temp1 * a[(i) - 1 + (i - 1) * lda + _a_offset] + alpha * temp2; 
		//  Close else.
		} 
		Dummy.label("Dsymm", 90); 
	      //  Close for() loop.
	      } 
	    } 
	    Dummy.label("Dsymm", 100); 
	  //  Close for() loop.
	  } 
	} 
      //  Close else.
      } 
    // Close if()
    } 
    else 
    { 
      
      { 
	forloop170 : 
	for (j = 1; j <= n; j++) 
	{ 
	  temp1 = alpha * a[(j) - 1 + (j - 1) * lda + _a_offset]; 
	  if (beta == zero) 
	  { 
	    
	    { 
	      forloop110 : 
	      for (i = 1; i <= m; i++) 
	      { 
		c[(i) - 1 + (j - 1) * Ldc + _c_offset] = temp1 * b[(i) - 1 + (j - 1) * ldb + _b_offset]; 
		Dummy.label("Dsymm", 110); 
	      //  Close for() loop.
	      } 
	    } 
	  // Close if()
	  } 
	  else 
	  { 
	    
	    { 
	      forloop120 : 
	      for (i = 1; i <= m; i++) 
	      { 
		c[(i) - 1 + (j - 1) * Ldc + _c_offset] = beta * c[(i) - 1 + (j - 1) * Ldc + _c_offset] + temp1 * b[(i) - 1 + (j - 1) * ldb + _b_offset]; 
		Dummy.label("Dsymm", 120); 
	      //  Close for() loop.
	      } 
	    } 
	  //  Close else.
	  } 
	  
	  { 
	    forloop140 : 
	    for (k = 1; k <= j - 1; k++) 
	    { 
	      if (upper) 
	      { 
		temp1 = alpha * a[(k) - 1 + (j - 1) * lda + _a_offset]; 
	      // Close if()
	      } 
	      else 
	      { 
		temp1 = alpha * a[(j) - 1 + (k - 1) * lda + _a_offset]; 
	      //  Close else.
	      } 
	      
	      { 
		forloop130 : 
		for (i = 1; i <= m; i++) 
		{ 
		  c[(i) - 1 + (j - 1) * Ldc + _c_offset] = c[(i) - 1 + (j - 1) * Ldc + _c_offset] + temp1 * b[(i) - 1 + (k - 1) * ldb + _b_offset]; 
		  Dummy.label("Dsymm", 130); 
		//  Close for() loop.
		} 
	      } 
	      Dummy.label("Dsymm", 140); 
	    //  Close for() loop.
	    } 
	  } 
	  
	  { 
	    forloop160 : 
	    for (k = j + 1; k <= n; k++) 
	    { 
	      if (upper) 
	      { 
		temp1 = alpha * a[(j) - 1 + (k - 1) * lda + _a_offset]; 
	      // Close if()
	      } 
	      else 
	      { 
		temp1 = alpha * a[(k) - 1 + (j - 1) * lda + _a_offset]; 
	      //  Close else.
	      } 
	      
	      { 
		forloop150 : 
		for (i = 1; i <= m; i++) 
		{ 
		  c[(i) - 1 + (j - 1) * Ldc + _c_offset] = c[(i) - 1 + (j - 1) * Ldc + _c_offset] + temp1 * b[(i) - 1 + (k - 1) * ldb + _b_offset]; 
		  Dummy.label("Dsymm", 150); 
		//  Close for() loop.
		} 
	      } 
	      Dummy.label("Dsymm", 160); 
	    //  Close for() loop.
	    } 
	  } 
	  Dummy.label("Dsymm", 170); 
	//  Close for() loop.
	} 
      } 
    //  Close else.
    } 
    Dummy.go_to("Dsymm", 999999); 
    Dummy.label("Dsymm", 999999); 
    return ; 
  }

// End class.
}


