package org.netlib.util;

public class MatConv
{
  
  /*  Method to convert 2D matrices into 1D arrays for
   *  passing to blas and lapack subroutines.  
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
  }				//  Close TwoDtoOneD().

  public static double[][] doubleOneDtoTwoD(double [] vec, int ld)
  {
    int i,j;
    double [][] mat = new double [ld][vec.length / ld];

   
    for (i = 0; i < ld; i++)
      for (j = 0; j < mat[0].length; j++)
        mat[i][j] = vec[i + j * ld];

    return mat;
  }

  /*  Method to convert 2D matrices into 1D arrays for
   *  passing to blas and lapack subroutines.
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
  }                             //  Close TwoDtoOneD().

  public static float[][] floatOneDtoTwoD(float [] vec, int ld)
  {
    int i,j;
    float [][] mat = new float [ld][vec.length / ld];

   
    for (i = 0; i < ld; i++)
      for (j = 0; j < mat[0].length; j++)
        mat[i][j] = vec[i + j * ld];

    return mat;
  }

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
  }				//  Close TwoDtoOneD().

  public static int[][] intOneDtoTwoD(int [] vec, int ld)
  {
    int i,j;
    int [][] mat = new int [ld][vec.length / ld];

   
    for (i = 0; i < ld; i++)
      for (j = 0; j < mat[0].length; j++)
        mat[i][j] = vec[i + j * ld];

    return mat;
  }

  public static void copyOneDintoTwoD(double [][]mat, double[]vec)
  {
    int i,j;
    int ld = mat.length;

    for (i = 0; i < ld; i++)
      for (j = 0; j < mat[0].length; j++)
        mat[i][j] = vec[i + j * ld];
  }

  public static void copyOneDintoTwoD(float [][]mat, float[]vec)
  {
    int i,j;
    int ld = mat.length;

    for (i = 0; i < ld; i++)
      for (j = 0; j < mat[0].length; j++)
        mat[i][j] = vec[i + j * ld];
  }

  public static void copyOneDintoTwoD(int [][]mat, int[]vec)
  {
    int i,j;
    int ld = mat.length;

    for (i = 0; i < ld; i++)
      for (j = 0; j < mat[0].length; j++)
        mat[i][j] = vec[i + j * ld];
  }
}
