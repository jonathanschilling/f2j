package de.labathome;

import java.util.Arrays;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

/**
 * This is a helper class to transform comment( block)s from the BLAS format into valid Javadoc.
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
	
	public static final String AUTHOR = "> \\author";
	public static final String DATE = "> \\date";
	public static final String INGROUP = "> \\ingroup";
	public static final String PARAM = "> \\param";
	
	/** version of BLAS/LAPACK at the time of writing this code */
	public static final String DEFAULT_VERSION = "3.9.0";
	
	public static final String LOGICAL = "LOGICAL";
	public static final String CHARACTER = "CHARACTER";
	public static final String INTEGER = "INTEGER";
	public static final String REAL = "REAL";
	public static final String DOUBLE_PRECISION = "DOUBLE PRECISION";
	public static final String[] TYPES = new String[] {
			LOGICAL,
			CHARACTER,
			INTEGER,
			REAL,
			DOUBLE_PRECISION
	};
	
	/**
	 * Transform a comment from BLAS into Javadoc for version given by {@code DEFAULT_VERSION}.
	 * @param blasComment blasComment comment from BLAS Fortran source
	 * @return {@code blasComment} transformed into Javadoc
	 */
	public static final String toJavadoc(final String blasComment) {
		return toJavadoc(blasComment, null, DEFAULT_VERSION);
	}
	
	/**
	 * Transform a comment from BLAS into Javadoc for version given by {@code DEFAULT_VERSION}.
	 * @param blasComment blasComment comment from BLAS Fortran source
	 * @param additionalParametersNotInFortranDocs additional javadoc parameter descriptions to include after each parameter found in the key set of the given Map
	 * @return {@code blasComment} transformed into Javadoc
	 */
	public static final String toJavadoc(final String blasComment, Map<String, List<String>> additionalParametersNotInFortranDocs) {
		return toJavadoc(blasComment, additionalParametersNotInFortranDocs, DEFAULT_VERSION);
	}
	
	/**
	 * Transform a comment from BLAS into Javadoc.
	 * @param blasComment comment from BLAS Fortran source
	 * @param version version number of BLAS or LAPACK to put into the {@code @version} tag; e.g. "3.9.0"
	 * @return {@code blasComment} transformed into Javadoc
	 */
	public static final String toJavadoc(final String blasComment, Map<String, List<String>> additionalParametersNotInFortranDocs, String version) {
		String[] lines = blasComment.split("\n");
		
		// trim away whitespace at end and comment char at beginning
		// also trim whitespaces between char at beginning and actual comment 
		for (int i=0; i<lines.length; ++i) {
			lines[i] = lines[i].trim().substring(1).trim();
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
						
			// transform \verbatim ... \endverbatim into <pre> ... </pre>
			furtherDetailsLine = furtherDetailsLine.replace("\\verbatim", "<pre>");
			furtherDetailsLine = furtherDetailsLine.replace("\\endverbatim", "</pre>");
			
			// finally, append further detail to javadoc
			javadoc += " * "+furtherDetailsLine+"\n";
		}
		
		// empty line as separator in javadoc
		javadoc += " * \n";
		
		// handle parameters
		for (int i=0; i<lines.length; ++i) {
			if (lines[i].startsWith(PARAM)) {
				
				// get parameter name
				String param = lines[i].substring(PARAM.length());
				
				// remove indication of direction
				if (param.startsWith("[in]")) {
					param = param.substring(4);
				} else if (param.startsWith("[out]")) {
					param = param.substring(5);
				} else if (param.startsWith("[in,out]")) {
					param = param.substring(8);
				} 
				
				// remove any now-exposed whitespaces, which were exposed just now
				param = param.trim();
				
				// target storage for description of current parameter
				String paramDesc = "";
				
				// read further lines until next '\param' is found or Authors section begins
				int actualLines = 0;
				for (int j=i+1; j<lineAuthors; ++j) {
					String paramDescLine = lines[j].trim();
					
					// stop collecting when next parameter is found
					if (paramDescLine.startsWith(PARAM)) {
						break;
					}
					
					// skip '>'	at beginning of lines
					if (paramDescLine.startsWith(">")) {
						paramDescLine = paramDescLine.substring(1);
					}
					
					// trim any now-exposed whitespaces
					paramDescLine = paramDescLine.trim();
					
					// skip now-empty lines and verbatim statements
					if (paramDescLine.equals("") || 
							paramDescLine.equals("\\verbatim") ||
							paramDescLine.equals("\\endverbatim")) {
						
						continue;
					}
					
					// skip comment line (parts) which only describes the data type, e.g. "N is INTEGER"
					if (paramDescLine.startsWith(param+" is ")) {
						
						// get part of parameter description after "N is "
						String followingPart = paramDescLine.substring(param.length()+4);
						
						// check if next (set of )word(s) is in the list of Fortran data types used in BLAS/LAPACK
						int idxInTypes = -1;
						for (int k=0; k<TYPES.length; ++k) {
							if (followingPart.length() >= TYPES[k].length() && TYPES[k].equals(followingPart.substring(0, TYPES[k].length()))) {
								idxInTypes = k;
								break;
							}
						}
						
						if (idxInTypes > 0) {
							
							// trimmed description leftovers after type
							String furtherDescription = followingPart.substring(TYPES[idxInTypes].length()).trim();
							
							// skip lines which only read "N is INTEGER", since the types are documented in Java already
							if (furtherDescription.equals("")) {
								continue;
							}
							
							// if parameter is array, skip the 'array, ' part and directly list anything that describes the array further 
							if (furtherDescription.startsWith("array, ")) {
								paramDescLine = furtherDescription.substring(7);
							}
						}
					}
					
					// count the current line as actually appearing in the comment if we reach this point
					actualLines++;
					
					// fix indentation for description lines after the first one
					if (actualLines>1) {
						// indent past " * @param "
						paramDesc += " *        ";
						
						// indent also past parameter name
						for (int k=0; k<param.length(); ++k) {
							paramDesc += " ";
						}
						
						// one extra space for separation after parameter name
						paramDesc += " ";
					}
					
					// attach to indented parameter description
					paramDesc += paramDescLine+"\n";
				}
				
				// ensure that at least the newline character is present in the description
				if (!paramDesc.endsWith("\n")) {
					paramDesc += "\n";
				}
				
				// finally, attach parameter description to javadoc
				// use lower case parameter names as common in Java
				javadoc += " * @param " + param.toLowerCase()+" "+paramDesc;
				
				// include additional parameter descriptions if available
				if (additionalParametersNotInFortranDocs != null && additionalParametersNotInFortranDocs.containsKey(param.toLowerCase())) {
					
					// get list of additional parameters for current parameter
					List<String> additionalDescriptions = additionalParametersNotInFortranDocs.get(param.toLowerCase());
					
					// attach their javadoc to final documentaion
					for (String additionalDescription: additionalDescriptions) {
						javadoc += additionalDescription+"\n";
					}
				}
			}
		}

		// empty line as separator in javadoc
		javadoc += " * \n";
		
		// return value description
		javadoc += " * @return result\n";
		
		
		// empty line as separator in javadoc
		javadoc += " * \n";
		
		// handle date from Fortran comments and combine with given version number
		for (int i=0; i<lines.length; ++i) {
			if (lines[i].startsWith(DATE)) {
				
				// get substring after '\date ' --> date of last update (?)
				int dateStartIdx = DATE.length() + 1;
				String date = lines[i].substring(dateStartIdx);
				
				// add javadoc tag for author
				javadoc += " * @version "+version + ", "+date+"\n";
			}
		}
				
		// empty line as separator in javadoc
		javadoc += " * \n";		
		
		// handle authors: simply collect all authors found in the documentation
		for (int i=0; i<lines.length; ++i) {
			if (lines[i].startsWith(AUTHOR)) {
				
				// get substring after '\author ' --> contributing author
				int authorStartIdx = AUTHOR.length() + 1;
				String author = lines[i].substring(authorStartIdx);
				
				// add javadoc tag for author
				javadoc += " * @author "+author+"\n";
			}
		}
		
		// empty line as separator in javadoc
		javadoc += " * \n";
		
		// see also online documentation
		javadoc += " * @see <a href=\""+ONLINE_NETLIB_HELP_URL+"\">"+ONLINE_NETLIB_HELP_URL+"</a>\n";
		
		// empty line as separator in javadoc
		javadoc += " * \n";
		
		// handle BLAS Level X grouping (where X \in {1,2,3})
		for (int i=0; i<lines.length; ++i) {
			if (lines[i].startsWith(INGROUP)) {
				
				// get substring after '\ingroup ' --> name of group
				int inGroupStartIdx = INGROUP.length() + 1;
				String group = lines[i].substring(inGroupStartIdx);
								
				// transform group id into tag name
				// all tags occurring here have to be specified in the pom.xml !
				String tag = group.replace("_", ".");
				
				// add transformed group id as tag to javadoc
				javadoc += " * @"+tag+"\n";
			}
		}
		
		// close comment
		javadoc += " */";
		
		return javadoc;
	}
	
	/**
	 * demonstrate how to transform ddot documentation into javadoc
	 * @param args
	 */
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
		
		List<String> additionalParamsForDx = new LinkedList<>();
		additionalParamsForDx.add(" * @param x0 starting index in dx");
		
		List<String> additionalParamsForDy = new LinkedList<>();
		additionalParamsForDy.add(" * @param y0 starting index in dy");
				
		Map<String, List<String>> additionalParametersNotInFortranDocs = new HashMap<>();
		additionalParametersNotInFortranDocs.put("dx", additionalParamsForDx);
		additionalParametersNotInFortranDocs.put("dy", additionalParamsForDy);
		
		final String blasJavadoc = toJavadoc(blasCommentDdot, additionalParametersNotInFortranDocs);
		final String[] blasJavadocLines = blasJavadoc.split("\n");
		
		System.out.println("\nfinal javadoc:");
		for (int i=0; i<blasJavadocLines.length; ++i) {
			System.out.println("["+i+"]: '"+blasJavadocLines[i]+"'");
		}
	}
}
