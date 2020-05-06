package de.labathome;

/**
 * This is a helper class to transform comment (block)s from the BLAS format into valid Javadoc.
 *
 * @author Jonathan Schilling (jonathan.schilling@mail.de)
 * @version $Id: $Id
 */
public class BlasComments {
	
	public static final String DEFINITION = "Definition:";
	public static final String PURPOSE = "Purpose:";
	public static final String ARGUMENTS = "Arguments:";
	public static final String AUTHORS = "Authors:";
	public static final String FURTHER_DETAILS = "Further Details:";
	
	/** link to online help for BLAS and LAPACK hosted by Netlib */
	public static final String ONLINE_NETLIB_HELP_URL = "http://www.netlib.org/lapack/explore-html/";
		
	/**
	 * Transform a comment from BLAS into Javadoc
	 *
	 * @param blasComment comment from BLAS Fortran source
	 * @return {@code blasComment} transformed into Javadoc
	 */
	public static final String toJavadoc(final String blasComment) {
		String[] lines = blasComment.split("\n");
		
		// trim away whitespace at end and comment char at beginning
		for (int i=0; i<lines.length; ++i) {
			lines[i] = lines[i].trim().substring(1);
		}
		
		// open comment
		String javadoc = "/**\n";
		
		// first pass: find paragraphs
		int lineDefinition=-1, linePurpose=-1, lineArguments=-1, lineAuthors=-1, lineFurtherDetails=-1;
		for (int i=0; i<lines.length; ++i) {
			if (lines[i].contains(DEFINITION)) {
				lineDefinition = i;
			} else if (lines[i].contains(PURPOSE)) {
				linePurpose = i;
			} else if (lines[i].contains(ARGUMENTS)) {
				lineArguments = i;
			} else if (lines[i].contains(AUTHORS)) {
				lineAuthors = i;
			} else if (lines[i].contains(FURTHER_DETAILS)) {
				lineFurtherDetails = i;
			}
		}
		if (lineDefinition < 0) { System.out.println("Could not find '"+DEFINITION+"'"); }
		if (linePurpose < 0) { System.out.println("Could not find '"+PURPOSE+"'"); }
		if (lineArguments < 0) { System.out.println("Could not find '"+ARGUMENTS+"'"); }
		if (lineAuthors < 0) { System.out.println("Could not find '"+AUTHORS+"'"); }
		if (lineFurtherDetails < 0) { System.out.println("Could not find '"+FURTHER_DETAILS+"'"); }
		
		// handle purpose; skip '====...' line after Purpose: statement and keep away one empty line before start of Arguments: paragraph
		for (int i=linePurpose+2; i<lineArguments-1; ++i) {
			String purposeLine = lines[i];
			
			// remove '>' from start of line, if present
			if (purposeLine.startsWith(">")) {
				purposeLine = purposeLine.substring(1);
			}
			
			// remove whitespaces at start and end
			purposeLine = purposeLine.trim();
			
			// skip empty lines
			if (purposeLine.length() == 0) {
				continue;
			}
			
			// for purpose statement, ignore \verbatim statement
			if (purposeLine.equals("\\verbatim") || purposeLine.equals("\\endverbatim")) {
				continue;
			}
			
			// finally, append purpose description to javadoc
			javadoc += " * "+purposeLine+"\n";
		}
		
		// empty line as separator in javadoc
		javadoc += " * \n";
		
		// handle further details
		for (int i=lineFurtherDetails+2; i<lines.length-1; ++i) {
			String furtherDetailsLine = lines[i];
			
			// remove '>' from start of line, if present
			if (furtherDetailsLine.startsWith(">")) {
				furtherDetailsLine = furtherDetailsLine.substring(1);
			}
			
			// remove whitespaces at start and end
			furtherDetailsLine = furtherDetailsLine.trim();
			
			// skip empty lines
			if (furtherDetailsLine.length() == 0) {
				continue;
			}
						
			// transform \verbatim ... \endverbatim into <pre> ... </pre> and indent properly
			String furtherDetail = "";
			if (furtherDetailsLine.equals("\\verbatim")) {
				furtherDetail = " <pre>";
			} else if (furtherDetailsLine.equals("\\endverbatim")) {
				furtherDetail = " </pre>";
			} else {
				furtherDetail += furtherDetailsLine;
			}
			
			// finally, append further detail to javadoc
			javadoc += " *"+furtherDetail+"\n";
		}
		
		// handle authors
		
		
		
		
		// close comment
		javadoc += " */";
		
		return javadoc;
	}
	
	public static void main(String[] args) {
		
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
		
		final String blasJavadoc = toJavadoc(blasCommentDdot);
		final String[] blasJavadocLines = blasJavadoc.split("\n");
		
		System.out.println("\nfinal javadoc:");
		for (String line: blasJavadocLines) {
			System.out.println(line);
		}
	}
}
