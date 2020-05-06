package de.labathome;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import org.junit.jupiter.api.Test;

public class TestBlasComments {
	
	@Test
	public static void testBlasCommentToJavadoc() {
		
		final String expectedJavadoc = "/**\n" + 
				"	 * DDOT forms the dot product of two vectors. Uses unrolled loops for increments\n" + 
				"	 * equal to one.\n" + 
				"	 * \n" + 
				"	 * <pre>\n" + 
				"	 *   jack dongarra, linpack, 3/11/78.\n" + 
				"	 *   modified 12/3/93, array(1) declarations changed to array(*)\n" + 
				"	 * </pre>\n" + 
				"	 *\n" + 
				"	 * @author Univ. of Tennessee\n" + 
				"	 * @author Univ. of California Berkeley\n" + 
				"	 * @author Univ. of Colorado Denver\n" + 
				"	 * @author NAG Ltd.\n" + 
				"	 * \n" + 
				"	 * @version 3.9.0, 11/01/17\n" + 
				"	 * \n" + 
				"	 * @double.blas.level1\n" + 
				"	 * \n" + 
				"	 * @param n    number of elements in input vector(s)\n" + 
				"	 * @param dx   dimension ( 1 + ( N - 1 )*abs( INCX ) )\n" + 
				"	 * @param x0   starting index in dx\n" + 
				"	 * @param incx storage spacing between elements of DX\n" + 
				"	 * @param dy   dimension ( 1 + ( N - 1 )*abs( INCY ) )\n" + 
				"	 * @param y0   starting index in dy\n" + 
				"	 * @param incy storage spacing between elements of DY\n" + 
				"	 * \n" + 
				"	 * @return DDOT forms the dot product of two vectors. Uses unrolled loops for\n" + 
				"	 *         increments equal to one.\n" + 
				"	 */";
		
		final String[] expectedJavadocLines = expectedJavadoc.split("\n");
		
		final String blasCommentDdot = "*> \\brief \\b DDOT\n" + 
				"*\n" + 
				"*  =========== DOCUMENTATION ===========\n" + 
				"*\n" + 
				"* Online html documentation available at\n" + 
				"*            http://www.netlib.org/lapack/explore-html/\n" + 
				"*\n" + 
				"*  Definition:\n" + 
				"*  ===========\n" + 
				"*\n" + 
				"*       DOUBLE PRECISION FUNCTION DDOT(N,DX,INCX,DY,INCY)\n" + 
				"*\n" + 
				"*       .. Scalar Arguments ..\n" + 
				"*       INTEGER INCX,INCY,N\n" + 
				"*       ..\n" + 
				"*       .. Array Arguments ..\n" + 
				"*       DOUBLE PRECISION DX(*),DY(*)\n" + 
				"*       ..\n" + 
				"*\n" + 
				"*\n" + 
				"*> \\par Purpose:\n" + 
				"*  =============\n" + 
				"*>\n" + 
				"*> \\verbatim\n" + 
				"*>\n" + 
				"*>    DDOT forms the dot product of two vectors.\n" + 
				"*>    uses unrolled loops for increments equal to one.\n" + 
				"*> \\endverbatim\n" + 
				"*\n" + 
				"*  Arguments:\n" + 
				"*  ==========\n" + 
				"*\n" + 
				"*> \\param[in] N\n" + 
				"*> \\verbatim\n" + 
				"*>          N is INTEGER\n" + 
				"*>         number of elements in input vector(s)\n" + 
				"*> \\endverbatim\n" + 
				"*>\n" + 
				"*> \\param[in] DX\n" + 
				"*> \\verbatim\n" + 
				"*>          DX is DOUBLE PRECISION array, dimension ( 1 + ( N - 1 )*abs( INCX ) )\n" + 
				"*> \\endverbatim\n" + 
				"*>\n" + 
				"*> \\param[in] INCX\n" + 
				"*> \\verbatim\n" + 
				"*>          INCX is INTEGER\n" + 
				"*>         storage spacing between elements of DX\n" + 
				"*> \\endverbatim\n" + 
				"*>\n" + 
				"*> \\param[in] DY\n" + 
				"*> \\verbatim\n" + 
				"*>          DY is DOUBLE PRECISION array, dimension ( 1 + ( N - 1 )*abs( INCY ) )\n" + 
				"*> \\endverbatim\n" + 
				"*>\n" + 
				"*> \\param[in] INCY\n" + 
				"*> \\verbatim\n" + 
				"*>          INCY is INTEGER\n" + 
				"*>         storage spacing between elements of DY\n" + 
				"*> \\endverbatim\n" + 
				"*\n" + 
				"*  Authors:\n" + 
				"*  ========\n" + 
				"*\n" + 
				"*> \\author Univ. of Tennessee\n" + 
				"*> \\author Univ. of California Berkeley\n" + 
				"*> \\author Univ. of Colorado Denver\n" + 
				"*> \\author NAG Ltd.\n" + 
				"*\n" + 
				"*> \\date November 2017\n" + 
				"*\n" + 
				"*> \\ingroup double_blas_level1\n" + 
				"*\n" + 
				"*> \\par Further Details:\n" + 
				"*  =====================\n" + 
				"*>\n" + 
				"*> \\verbatim\n" + 
				"*>\n" + 
				"*>     jack dongarra, linpack, 3/11/78.\n" + 
				"*>     modified 12/3/93, array(1) declarations changed to array(*)\n" + 
				"*> \\endverbatim\n" + 
				"*>\n" + 
				"*  =====================================================================";
		
		final String blasJavadoc = BlasComments.toJavadoc(blasCommentDdot);
		final String[] blasJavadocLines = blasJavadoc.split("\n");
		
		assertNotNull(blasJavadoc);
		assertEquals(expectedJavadocLines.length, blasJavadocLines.length);
		
		for (int i=0; i<expectedJavadocLines.length; ++i) {
			assertEquals(expectedJavadocLines[i], blasJavadocLines[i]);
		}
	}
}
