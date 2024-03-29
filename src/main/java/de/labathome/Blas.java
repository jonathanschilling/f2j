package de.labathome;

/**
 * <p>
 * Blas class.
 * </p>
 *
 * @author jonathan
 * @version $Id: $Id
 */
public class Blas {

	/**
	 * DDOT forms the dot product of two vectors.
	 * Uses unrolled loops for increments equal to one.
	 * 
	 * <pre>
	 * jack dongarra, linpack, 3/11/78.
	 * modified 12/3/93, array(1) declarations changed to array(*)
	 * </pre>
	 *
	 * @param n    number of elements in input vector(s)
	 * @param dx   dimension ( 1 + ( N - 1 )*abs( INCX ) )
	 * @param x0   starting index in dx
	 * @param incx storage spacing between elements of DX
	 * @param dy   dimension ( 1 + ( N - 1 )*abs( INCY ) )
	 * @param y0   starting index in dy
	 * @param incy storage spacing between elements of DY
	 * 
	 * @return DDOT forms the dot product of two vectors.
	 *         uses unrolled loops for increments equal to one.
	 * 
	 * @version 3.9.0, November 2017
	 *
	 * @author Univ. of Tennessee
	 * @author Univ. of California Berkeley
	 * @author Univ. of Colorado Denver
	 * @author NAG Ltd.
	 * 
	 * @see <a href="http://www.netlib.org/lapack/explore-html/">http://www.netlib.org/lapack/explore-html/</a>
	 * 
	 * @double.blas.level1
	 */
	public static double ddot(int n, final double[] dx, int x0, int incx, final double[] dy, int y0, int incy) {
		return 0.0;
	}

}
