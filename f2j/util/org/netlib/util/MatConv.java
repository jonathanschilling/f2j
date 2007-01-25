package org.netlib.util;

/**
 * This file is part of the Fortran-to-Java (f2j) system,
 * developed at the University of Tennessee.
 * <p>
 * This class contains methods for converting between the linearized
 * arrays used by f2j-generated code and the more natural Java-style
 * two-dimensional arrays.
 * <p>
 * @author Keith Seymour (seymour@cs.utk.edu)
 *
 */

public class MatConv
{
  
  /**
   * Convert a double precision two-dimensional array to
   * a linearized one-dimensional array.
   *
   * @param m the matrix to be converted
   *
   * @return the linearized array
   */
  public static double[] doubleTwoDtoOneD (double[][]m)
  {
    /* We make the assumption here that the matrices are
     * square (or rectangular), to get the value of
     * the second index.  
     */

    int ld = m.length;
    double[] apimatrix = new double[ld * m[0].length];

    for (int i = 0; i < ld; i++)
      for (int j = 0; j < m[0].length; j++)
        apimatrix[i + j * ld] = m[i][j];

    return apimatrix;
  }

  /**
   * Convert a double precision linearized one-dimensional array
   * to a two-dimensional array.
   *
   * @param vec the linearized array to be converted
   * @param ld leading dimension of the array
   *
   * @return the two-dimensional array
   */
  public static double[][] doubleOneDtoTwoD(double [] vec, int ld)
  {
    int i,j;
    double [][] mat = new double [ld][vec.length / ld];

   
    for (i = 0; i < ld; i++)
      for (j = 0; j < mat[0].length; j++)
        mat[i][j] = vec[i + j * ld];

    return mat;
  }

  /**
   * Convert a single precision two-dimensional array to
   * a linearized one-dimensional array.
   *
   * @param m the matrix to be converted
   *
   * @return the linearized array
   */
  public static float[] floatTwoDtoOneD (float[][]m)
  {
    /* We make the assumption here that the matrices are
     * square (or rectangular), to get the value of
     * the second index.
     */

    int ld = m.length;
    float[] apimatrix = new float[ld * m[0].length];

    for (int i = 0; i < ld; i++)
      for (int j = 0; j < m[0].length; j++)
        apimatrix[i + j * ld] = m[i][j];

    return apimatrix;
  }

  /**
   * Convert a single precision linearized one-dimensional array
   * to a two-dimensional array.
   *
   * @param vec the linearized array to be converted
   * @param ld leading dimension of the array
   *
   * @return the two-dimensional array
   */
  public static float[][] floatOneDtoTwoD(float [] vec, int ld)
  {
    int i,j;
    float [][] mat = new float [ld][vec.length / ld];
   
    for (i = 0; i < ld; i++)
      for (j = 0; j < mat[0].length; j++)
        mat[i][j] = vec[i + j * ld];

    return mat;
  }

  /**
   * Convert an integer two-dimensional array to
   * a linearized one-dimensional array.
   *
   * @param m the matrix to be converted
   *
   * @return the linearized array
   */
  public static int[] intTwoDtoOneD (int[][]m)
  {
    /* We make the assumption here that the matrices are
     * square (or rectangular), to get the value of
     * the second index.  
     */

    int ld = m.length;
    int[] apimatrix = new int[ld * m[0].length];

    for (int i = 0; i < ld; i++)
      for (int j = 0; j < m[0].length; j++)
        apimatrix[i + j * ld] = m[i][j];

    return apimatrix;
  }

  /**
   * Convert an integer linearized one-dimensional array
   * to a two-dimensional array.
   *
   * @param vec the linearized array to be converted
   * @param ld leading dimension of the array
   *
   * @return the two-dimensional array
   */
  public static int[][] intOneDtoTwoD(int [] vec, int ld)
  {
    int i,j;
    int [][] mat = new int [ld][vec.length / ld];

   
    for (i = 0; i < ld; i++)
      for (j = 0; j < mat[0].length; j++)
        mat[i][j] = vec[i + j * ld];

    return mat;
  }

  /**
   * Copies a linearized array into an already allocated two-dimensional
   * matrix.  This is typically called from the simplified wrappers
   * after the raw routine has been called and the results need to be
   * copied back into the Java-style two-dimensional matrix.
   *
   * @param mat destination matrix
   * @param vec source array
   */
  public static void copyOneDintoTwoD(double [][]mat, double[]vec)
  {
    int i,j;
    int ld = mat.length;

    for (i = 0; i < ld; i++)
      for (j = 0; j < mat[0].length; j++)
        mat[i][j] = vec[i + j * ld];
  }

  /**
   * Copies a linearized array into an already allocated two-dimensional
   * matrix.  This is typically called from the simplified wrappers
   * after the raw routine has been called and the results need to be
   * copied back into the Java-style two-dimensional matrix.
   *
   * @param mat destination matrix
   * @param vec source array
   */
  public static void copyOneDintoTwoD(float [][]mat, float[]vec)
  {
    int i,j;
    int ld = mat.length;

    for (i = 0; i < ld; i++)
      for (j = 0; j < mat[0].length; j++)
        mat[i][j] = vec[i + j * ld];
  }

  /**
   * Copies a linearized array into an already allocated two-dimensional
   * matrix.  This is typically called from the simplified wrappers
   * after the raw routine has been called and the results need to be
   * copied back into the Java-style two-dimensional matrix.
   *
   * @param mat destination matrix
   * @param vec source array
   */
  public static void copyOneDintoTwoD(int [][]mat, int[]vec)
  {
    int i,j;
    int ld = mat.length;

    for (i = 0; i < ld; i++)
      for (j = 0; j < mat[0].length; j++)
        mat[i][j] = vec[i + j * ld];
  }
}
