/* Formatter.java 
 *
 * This is a modified version of Jocelyn Paine's Formatter package:
 *   http://www.j-paine.org/Formatter
 *
 * Modifications are flagged with "kgs" in the comments.
 */

package org.j_paine.formatter;

import java.io.DataInputStream;
import java.io.IOException;
import java.io.PrintStream;
import java.io.StringReader;
import java.util.Hashtable;
import java.util.Vector;


/* This class holds a Format, and has methods for reading and
   writing data against it.
*/
public class Formatter
{
  private Format format = null;
  private FormatMap format_map = null;

  public Formatter( String format ) throws InvalidFormatException
  {
    this( new Format(format) );
  }

  public Formatter( Format format )
  {
    this.format = format;
  }


  public void setFormatMap( FormatMap format_map )
  {
    this.format_map = format_map;
  }


  public void write( Vector v, PrintStream out )
              throws OutputFormatException
  {
    FormatX dummy_el = new FormatX();
    FormatOutputList vp = new VectorAndPointer( v );

    /* Loop back around and reuse the format spec if
     * there are still elements in the vector.  Keep
     * going until all elements in the vector have
     * been printed.  --kgs
     */
    while(true) {
      try {
        this.format.write( vp, out );
        vp.checkCurrentElementForWrite(dummy_el);
        out.println();
      }catch(EndOfVectorOnWriteException e) {
        break;
      }
    }
  }

  public void write( int i, PrintStream out )
              throws OutputFormatException
  {
    write( new Integer(i), out );
  }

  public void write( long l, PrintStream out )
              throws OutputFormatException
  {
    write( new Long(l), out );
  }

  public void write( float f, PrintStream out )
              throws OutputFormatException
  {
    write( new Float(f), out );
  }

  public void write( double d, PrintStream out )
              throws OutputFormatException
  {
    write( new Double(d), out );
  }

  public void write( Object o, PrintStream out )
              throws OutputFormatException
  {
    Vector v = new Vector();
    v.addElement( o );
    write( v, out );
  }


  public void read( Vector v, DataInputStream in )
              throws InputFormatException
  {
    FormatInputList vp = new VectorAndPointer( v );
    InputStreamAndBuffer inb = new InputStreamAndBuffer(in);
    this.format.read( vp, inb, this.format_map );
  }

  public void read( Vector v, Hashtable ht, DataInputStream in )
              throws InputFormatException
  {
    FormatInputList vp = new StringsHashtableAndPointer( v, ht );
    InputStreamAndBuffer inb = new InputStreamAndBuffer(in);
    this.format.read( vp, inb, this.format_map );
  }

  public void read( String[] s, Hashtable ht, DataInputStream in )
              throws InputFormatException
  {
    Vector v = new Vector();
    for ( int i = 0; i<s.length; i++ )
      v.addElement( s[i] );
    read( v, ht, in );
  }

  public Object read( DataInputStream in )
                throws InputFormatException
  {
    Vector v = new Vector();
    read( v, in );
    return v.elementAt(0);
  }


  public boolean eof( DataInputStream in )
                 throws IOException
  {
    return ( in.available() <= 0 );
  }


  public String toString()
  {
    return "[Formatter " + this.format.toString() + "]";
  }
}


/* Below, we define various classes for holding complete formats,
   format elements, and so on. The class FormatUniv is a superclass
   of them all. This makes it a convenient "universal type" to
   use to hold any piece of, or a complete, format.
*/
abstract class FormatUniv
{
  abstract void write( FormatOutputList vp, PrintStream out )
                throws OutputFormatException;

  abstract void read( FormatInputList vp,
                      InputStreamAndBuffer in,
                      FormatMap format_map
                    )
                throws InputFormatException;
}


/* This class represents a complete format, i.e. a sequence of
   elements such as F12.5 and so on. Some of the elements may
   themselves be formats.
   We implement it as a vector of elements.
*/
class Format extends FormatUniv
{
  private Vector elements = new Vector();

  public Format( String s ) throws InvalidFormatException
  {
    FormatParser fp =
      Parsers.theParsers().format_parser;
    fp.ReInit( new StringReader(s) );
    try {
      Format f = fp.Format();
      this.elements = f.elements;
    }
    catch ( ParseException e ) {
      throw new InvalidFormatException( e.getMessage() );
    }
    catch ( TokenMgrError e ) {
      throw new InvalidFormatException( e.getMessage() );
    }
  }

  // We call this one from inside the parser, which needs a Format
  // with its vector initialised.
  Format()
  {
  }


  public void addElement( FormatUniv fu )
  {
    this.elements.addElement( fu );
  }


  public void write( FormatOutputList vp, PrintStream out )
              throws OutputFormatException
  {
    for ( int i=0; i<this.elements.size(); i++ ) {
      FormatUniv fu = (FormatUniv)this.elements.elementAt(i);
      fu.write( vp, out );
    }
  }


  public void read( FormatInputList vp,
                    InputStreamAndBuffer in,
                    FormatMap format_map
                  )
              throws InputFormatException
  {
    for ( int i=0; i<this.elements.size(); i++ ) {
      FormatUniv fu = (FormatUniv)this.elements.elementAt(i);
      fu.read( vp, in, format_map );
    }
  }


  public String toString()
  {
    String s = "";
    for ( int i=0; i<this.elements.size(); i++ ) {
      if ( i!=0 )
        s = s + ", ";
      s = s + this.elements.elementAt(i).toString();
    }
    return s;
  }
}


/* This class represents a repeated item, e.g. 3F12.5 or 3X.
   The integer r gives the repetition factor.
   The item may be either a format element, or an entire format.
   To cater for either, we hold it in a FormatUniv object (this is
   why we introduced the class FormatUniv).
*/
class FormatRepeatedItem extends FormatUniv
{
  private int r=1;
  private FormatUniv format_univ = null;


  public FormatRepeatedItem( FormatUniv format_univ )
  {
    this( 1, format_univ );
  }

  public FormatRepeatedItem( int r, FormatUniv format_univ )
  {
    this.r = r;
    this.format_univ = format_univ;
  }


  public void write( FormatOutputList vp, PrintStream out )
              throws OutputFormatException
  {
    for ( int i=1; i<=this.r; i++ )
      this.format_univ.write( vp, out );
  }


  public void read( FormatInputList vp,
                    InputStreamAndBuffer in,
                    FormatMap format_map
                  )
              throws InputFormatException
  {
    for ( int i=1; i<=this.r; i++ )
      this.format_univ.read( vp, in, format_map );
  }


  public String toString()
  {
    if (r==1)
      return this.format_univ.toString();
    else
      return this.r+"("+this.format_univ.toString()+")";
  }
}


/* This class represents a single format element such as
   F12.5, I2, or X.
*/
abstract class FormatElement extends FormatUniv
{
  /* This method will be defined differently by each subclass.
  */
  public abstract void write( FormatOutputList vp, PrintStream out )
                       throws OutputFormatException;
}


/* This class represents a format element that reads or writes
   data. So F12.5 or I3, but not X.
   We assume that all format elements are fixed width.
*/
abstract class FormatIOElement extends FormatElement
{
  private int width;

  void setWidth( int width )
  {
    this.width = width;
  }

  int getWidth()
  {
    return this.width;
  }


  public void write( FormatOutputList vp, PrintStream out )
              throws OutputFormatException
  {
    vp.checkCurrentElementForWrite( this );
    Object o = vp.getCurrentElementAndAdvance();
    out.print( convertToString(o,vp.getPtr()-1) );
  }


  /* This method is called by write, above. It will be
     defined differently for each subclass of FormatIOElement.
     The idea is that getting the next element to write from
     the output list, and printing its string representation,
     are the same for all FormatIOElements. However, the
     conversion to string is different for each one.
  */
  abstract String convertToString( Object o, int vecptr )
                  throws OutputFormatException;


  public void read( FormatInputList vp,
                    InputStreamAndBuffer in,
                    FormatMap format_map
                  )
              throws InputFormatException
  {
    /* Get next width characters. */
    String s = in.getSlice( this.width, vp.getPtr(), this );

    /* Try translating if there's a format map. */
    if ( format_map != null ) {
      String repl = format_map.getMapping( s );
      if ( repl != null )
        s = repl;
    }

    /* Parse the string to check it's a valid number, and put into
       the vector if so.
       Also, advance the stream input pointer.
    */
    Object o = convertFromString( s, vp, in );
    vp.checkCurrentElementForRead( this, in );
    vp.putElementAndAdvance( o, this, in );
    in.advance( this.width );
  }


  /* This method is called by read, above. It will be
     defined differently for each subclass of FormatIOElement.
     The idea is that getting the next element to read from
     the input stream, and putting it into the input list,
     are the same for all FormatIOElements. However, the
     conversion from string is different for each one.
     vp and in are used only in generating error messages.
  */
  abstract Object convertFromString( String s,
                                     FormatInputList vp,
                                     InputStreamAndBuffer in
                                   )
                  throws InputFormatException;
}

/* This class represents a P format element, but the scaling
 * is not implemented yet.
 */
class FormatP extends FormatElement
{
  FormatRepeatedItem ritem = null;

  public FormatRepeatedItem getRepeatedItem() {
    return ritem;
  }

  public FormatP(int r, FormatUniv format_univ) {
    if(format_univ != null)
      ritem = new FormatRepeatedItem(r, format_univ);
  }

  public void write( FormatOutputList vp, PrintStream out )
  {
    /* the P element itself produces no output.  it's a scale factor
     * for other elements, but that isn't being handled yet.
     */
  } 


  public void read( FormatInputList vp,
                    InputStreamAndBuffer in, 
                    FormatMap format_map
                  )
  { 
    /* the P element doesn't consume input.  --kgs */
  }


  public String toString()
  { 
    return "P";
  } 
}


/* This class represents an X format element.
*/
class FormatX extends FormatElement
{
  public void write( FormatOutputList vp, PrintStream out )
  {
    out.print( " " );
  }


  public void read( FormatInputList vp,
                    InputStreamAndBuffer in,
                    FormatMap format_map
                  )
  {
    in.advance( 1 );
  }


  public String toString()
  {
    return "X";
  }
}


/* This class represents an Aw format element.
*/
class FormatA extends FormatIOElement
{
  public FormatA( int w )
  {
    setWidth( w );
  }


  String convertToString( Object o, int vecptr )
         throws IllegalObjectOnWriteException,
                StringTooWideOnWriteException
  {
    String s;

    if ( o instanceof String ) {
      /* pad or truncate strings as necessary.  --kgs */
      s = (String)o;
      if ( (getWidth() != -1) && (s.length() > getWidth()) )
        return s.substring(0, getWidth());
      else {
        if(getWidth() > s.length()) {
          char [] pad = new char[getWidth() - s.length()];

          for(int i=0;i<pad.length;i++)
            pad[i] = ' ';

          return new String(pad) + s;
        }
        else
          return s;
      }
    }
    else {
      char [] blah = new char[getWidth()];

      /* if this is a non-string argument with an A edit descriptor,
       * just print some nonsense.  --kgs
       */
      for(int i=0;i<blah.length;i++)
        blah[i] = '#';

      return new String(blah);
    }
  }


  /* vp and in are used only in generating error messages.
  */
  Object convertFromString( String s,
                            FormatInputList vp,
                            InputStreamAndBuffer in
                          )
         throws InvalidNumberOnReadException
  {
    int len;

    len = getWidth() - s.length();

    /* if the spec width is wider than the string, 
     * return a padded string.  --kgs
     */
    if(len > 0) {
      char [] pad = new char[len];
      for(int i=0;i<len;i++)
        pad[i] = ' ';
      String padstr = new String(pad);

      return s.concat(padstr);
    }

    /* We just return the slice read, as a string. */
    return s;
  }


  public String toString()
  {
    return "A"+getWidth();
  }
}


/* This class represents an Iw format element.
*/
class FormatI extends FormatIOElement
{
  public FormatI( int w )
  {
    setWidth( w );
  }


  String convertToString( Object o, int vecptr )
         throws IllegalObjectOnWriteException,
                NumberTooWideOnWriteException
  {
    String s;

    /* Convert the number to a string. */
    if ( o instanceof Integer || o instanceof Long ) {
      String fmtstr = "%" + Integer.toString(getWidth()) + "d";
      s = new PrintfFormat(fmtstr).sprintf(o);

      /* Throw an exception if the string won't fit. */
      if ( s.length() > getWidth() ) {
        // instead of throwing an exception, pad the field with asterisks to
        // match the behavior of g77/gfortran.   --kgs

        s = new PrintfFormat("%" + Integer.toString(getWidth()) + "s").sprintf(" ").replace(' ', '*');
        //throw new NumberTooWideOnWriteException( (Number)o, vecptr, this.toString());
      }

      return s;
    }
    else if(o instanceof String) {
      /* String passed to I edit descriptor.  try converting the
       * first character to an integer.  --kgs
       */
      return convertToString(new Integer((int) (((String)o).charAt(0))), vecptr);
    }
    else
      throw new IllegalObjectOnWriteException( o,
                                               vecptr,
                                               this.toString()
                                             );
  }


  /* vp and in are used only in generating error messages.
  */
  Object convertFromString( String str,
                            FormatInputList vp,
                            InputStreamAndBuffer in
                          )
         throws InvalidNumberOnReadException
  {
    String s = str.trim();

    /* Parse the string to check it's a valid number,
       and convert if so.
    */
    NumberParser np =
      Parsers.theParsers().number_parser;
    np.ReInit( new StringReader(s) );
    try {
      int start = np.Integer();
      Long l = new Long( s.substring(start) );
      return l;
    }
    catch ( ParseException e ) {
      throw new InvalidNumberOnReadException( s,
                                              vp.getPtr(),
                                              this.toString(),
                                              in.getLineErrorReport(),
                                              e.getMessage()
                                            );
    }
    catch ( TokenMgrError e ) {
      throw new InvalidNumberOnReadException( s,
                                              vp.getPtr(),
                                              this.toString(),
                                              in.getLineErrorReport(),
                                              e.getMessage()
                                            );
    }
  }


  public String toString()
  {
    return "I"+getWidth();
  }
}

/*
 * Handles logical (boolean) edit descriptors.
 */

class FormatL extends FormatIOElement
{
  public FormatL( int w )
  {
    setWidth( w );
  }

  String convertToString( Object o, int vecptr )
         throws IllegalObjectOnWriteException,
                NumberTooWideOnWriteException
  {
    String s;

    /* Convert the number to a string. */
    if ( o instanceof Boolean ) {
      char [] b = new char[getWidth()];
      int i;

      for(i=0;i<b.length-1;i++)
        b[i] = ' ';
      
      b[i] = (((Boolean)o).booleanValue() == true) ? 'T' : 'F';

      s = new String(b);

      /* Throw an exception if the string won't fit. */
      if ( s.length() > getWidth() ) {
        // instead of throwing an exception, pad the field with asterisks to
        // match the behavior of g77/gfortran.   --kgs

        s = new PrintfFormat("%" + Integer.toString(getWidth()) + "s").sprintf(" ").replace(' ', '*');
        //throw new NumberTooWideOnWriteException( (Number)o, vecptr, this.toString());
      }

      return s;
    }
    else
      throw new IllegalObjectOnWriteException( o,
                                               vecptr,
                                               this.toString()
                                             );
  }


  /* vp and in are used only in generating error messages.
  */
  Object convertFromString( String str,
                            FormatInputList vp,
                            InputStreamAndBuffer in
                          )
         throws InvalidNumberOnReadException
  {
    String s = str.trim();

    /* Parse the string to check it's a valid number,
       and convert if so.
    */
    NumberParser np =
      Parsers.theParsers().number_parser;
    np.ReInit( new StringReader(s) );
    try {
      int start = np.Boolean();
      char brep = s.substring(start).charAt(0);
      Boolean b;

      if(brep == '.')
        brep = s.substring(start+1).charAt(0);
      if(brep == 't' || brep == 'T')
        b = new Boolean(true);
      else if(brep == 'f' || brep == 'F')
        b = new Boolean(false);
      else
        throw new ParseException("bad logical value");
      return b;
    }
    catch ( ParseException e ) {
      throw new InvalidNumberOnReadException( s,
                                              vp.getPtr(),
                                              this.toString(),
                                              in.getLineErrorReport(),
                                              e.getMessage()
                                            );
    }
    catch ( TokenMgrError e ) {
      throw new InvalidNumberOnReadException( s,
                                              vp.getPtr(),
                                              this.toString(),
                                              in.getLineErrorReport(),
                                              e.getMessage()
                                            );
    }
  }

  public String toString()
  {
    return "L"+getWidth();
  }
}

/* This class represents an Fw.d format element.
   Numbers should be output with d decimal places.
*/
class FormatF extends FormatIOElement
{
  private int d;


  public FormatF( int w, int d )
  {
    setWidth( w );
    this.d = d;
  }


  String convertToString( Object o, int vecptr )
         throws IllegalObjectOnWriteException,
                NumberTooWideOnWriteException
  {
    String s;

    /* Convert the number to a string. */
    if ( o instanceof Integer || o instanceof Long ||
         o instanceof Float || o instanceof Double ) {
      String fmtstr;

      /* hack to match gfortran.  if the width after the decimal point is zero,
       * then append "." since the PrintfFormat doesn't do it.   --kgs
       */
      if(this.d == 0) {
        fmtstr = "%" + Integer.toString(getWidth()-1) + "." + 
           Integer.toString(this.d) + "f";
        s = new PrintfFormat(fmtstr).sprintf(o) + ".";
      }
      else {
        fmtstr = "%" + Integer.toString(getWidth()) + "." + 
           Integer.toString(this.d) + "f";
        s = new PrintfFormat(fmtstr).sprintf(o);
      }

      /* Throw an exception if the string won't fit. */
      if ( s.length() > getWidth() ) {

        /* another hack to match gfortran.  if the number is just one char too long, but
         * it begins with "0." or "-0.", then just drop the zero to fit in the given
         * width. --kgs
         */
        if(s.startsWith("0.") && (s.length() == getWidth()+1))
          return s.substring(1);
        else if(s.startsWith("-0.") && (s.length() == getWidth()+1))
          return "-" + s.substring(2);

        // instead of throwing an exception, pad the field with asterisks to
        // match the behavior of g77/gfortran.   --kgs

        s = new PrintfFormat("%" + Integer.toString(getWidth()) + "s").sprintf(" ").replace(' ', '*');
        //throw new NumberTooWideOnWriteException( (Number)o, vecptr, this.toString());
      }

      return s;
    }
    else
      throw new IllegalObjectOnWriteException( o,
                                               vecptr,
                                               this.toString()
                                             );
  }


  /* vp and in are used only in generating error messages.
  */
  Object convertFromString( String str,
                            FormatInputList vp,
                            InputStreamAndBuffer in
                          )
         throws InvalidNumberOnReadException
  {
    String s = str.trim();

    /* Parse the string to check it's a valid number,
       and convert if so.
    */
    NumberParser np =
      Parsers.theParsers().number_parser;
    np.ReInit( new StringReader(s) );
    try {
      int start = np.Float();
      Double d = new Double( s.substring(start) );
      return d;
    }
    catch ( ParseException e ) {
      throw new InvalidNumberOnReadException( s,
                                              vp.getPtr(),
                                              this.toString(),
                                              in.getLineErrorReport(),
                                              e.getMessage()
                                            );
    }
    catch ( TokenMgrError e ) {
      throw new InvalidNumberOnReadException( s,
                                              vp.getPtr(),
                                              this.toString(),
                                              in.getLineErrorReport(),
                                              e.getMessage()
                                            );
    }
  }


  public String toString()
  {
    return "F"+getWidth()+"."+this.d;
  }
}


/* This class represents an Ew.d format element.
   Numbers should be output as
     s0.dd...ddEsdd
   where s is a sign.
*/
class FormatE extends FormatIOElement
{ int d;


  public FormatE( int w, int d )
  {
    setWidth( w );
    this.d = d;
  }


  String convertToString( Object o, int vecptr )
         throws IllegalObjectOnWriteException,
                NumberTooWideOnWriteException
  {
    String s;

    /* Convert the number to a string. */
    if ( o instanceof Integer || o instanceof Long ||
         o instanceof Float || o instanceof Double ) {
      String fmtstr = "%" + Integer.toString(getWidth()) + "." + 
         Integer.toString(this.d) + "E";
      s = new PrintfFormat(fmtstr).sprintf(o);

      /* what follows is an ugly hack to make f2j's output match gfortran's.
       * if we get a result formatted with all zeroes in the exponent,
       * convert it to ..E+01 form.  gfortran never seems to emit numbers
       * with a zero exponent.  surely there's a better way to handle this.
       * --kgs
       */
      int e_idx = s.indexOf('E');
      int d_idx = s.indexOf('.');
      String pre_zero = (s.length() > getWidth()) ? "" : "0";
      String news = s.substring(0,d_idx-1) + pre_zero + "." + 
         s.charAt(d_idx-1) + s.substring(d_idx+1, e_idx-1);

      if(s.endsWith("+00") || s.endsWith("-00"))
        s = news + "E+01";

      if(s.endsWith("+000") || s.endsWith("-000"))
        s = news + "E+001";

      /* Throw an exception if the string won't fit. */
      if ( s.length() > getWidth() ) {
        // instead of throwing an exception, pad the field with asterisks to
        // match the behavior of g77/gfortran.   --kgs

        s = new PrintfFormat("%" + Integer.toString(getWidth()) + "s").sprintf(" ").replace(' ', '*');
        //throw new NumberTooWideOnWriteException( (Number)o, vecptr, this.toString());
      }

      return s;
    }
    else
      throw new IllegalObjectOnWriteException( o,
                                               vecptr,
                                               this.toString()
                                             );
  }


  /* vp and in are used only in generating error messages.
  */
  Object convertFromString( String str,
                            FormatInputList vp,
                            InputStreamAndBuffer in
                          )
         throws InvalidNumberOnReadException
  {
    String s = str.trim();

    /* Parse the string to check it's a valid number,
       and convert if so.
    */
    NumberParser np =
      Parsers.theParsers().number_parser;
    np.ReInit( new StringReader(s) );
    try {
      int start = np.Float();
      Double d = new Double( s.substring(start) );
      return d;
    }
    catch ( ParseException e ) {
      throw new InvalidNumberOnReadException( s,
                                              vp.getPtr(),
                                              this.toString(),
                                              in.getLineErrorReport(),
                                              e.getMessage()
                                            );
    }
    catch ( TokenMgrError e ) {
      throw new InvalidNumberOnReadException( s,
                                              vp.getPtr(),
                                              this.toString(),
                                              in.getLineErrorReport(),
                                              e.getMessage()
                                            );
    }
  }


  public String toString()
  {
    return "E"+getWidth()+"."+this.d;
  }
}


/* This class represents an / item.
*/
class FormatSlash extends FormatElement
{
  public void write( FormatOutputList vp, PrintStream out )
  {
    out.println();
  }


  public void read( FormatInputList vp,
                    InputStreamAndBuffer in,
                    FormatMap format_map
                  )
              throws InputFormatException
  {
    in.readLine( vp.getPtr(), this );
  }


  public String toString()
  {
    return "/";
  }
}


/* This class represents an embedded literal, e.g. 'Title'.
   toString() does not yet handle embedded quotes.
*/
class FormatString extends FormatElement
{
  private String s;


  public FormatString( String s )
  {
    this.s = s.replaceAll("''", "'");
  }


  public void write( FormatOutputList vp, PrintStream out )
  {
    out.print(this.s);
  }


  public void read( FormatInputList vp,
                    InputStreamAndBuffer in,
                    FormatMap format_map
                  )
              throws InputFormatException
  {
    String s = in.getSlice( this.s.length(), vp.getPtr(), this );
    if ( !( this.s.equals(s) ) )
      throw new UnmatchedStringOnReadException( s,
                                                vp.getPtr(),
                                                this.toString(),
                                                in.getLineErrorReport()
                                              );
    in.advance( this.s.length() );
  }


  public String toString()
  {
    return "'" + this.s + "'";
  }
}


/* This class represents a mapping from input data. We use it to specify,
   for example, that on input, an "X" should be replaced by a "0" before
   being interpreted by the formatted input routines.
   The user must provide an instance of this class, with getMapping
   defined. getMapping should return either null, if the input string
   is to be left as it is, or a replacement string.
*/
abstract class FormatMap
{
  public abstract String getMapping( String in );
}


interface FormatOutputList
{
  boolean hasCurrentElement();

  void checkCurrentElementForWrite( FormatElement format_element )
       throws EndOfVectorOnWriteException;

  Object getCurrentElement();

  Object getCurrentElementAndAdvance();

  /* Returns the current pointer.
     Used only in generating error messages.
  */
  int getPtr();
}


interface FormatInputList
{
  /* format_element and in are only for generating error messages.
  */
  void checkCurrentElementForRead( FormatElement format_element,
                                   InputStreamAndBuffer in
                                 )
       throws InputFormatException;
  // If the list is a VectorAndPointer, it won't throw an exception.
  // If it is a StringsHashtableAndPointer, it will throw a
  // EndOfKeyVectorOnReadException.

  /* Puts o into the input list and advances its pointer.
     Must be defined for each subclass.
     format_element and in are only for generating error messages.
  */
  void putElementAndAdvance( Object o,
                             FormatElement format_element,
                             InputStreamAndBuffer in
                           )
       throws InputFormatException;

  /* Returns the current pointer.
     Used only in generating error messages.
  */
  int getPtr();
}


/* This class represents a Vector and a current-element pointer.
   We use it when outputting or inputting a Vector against a format:
   the pointer keeps track of the current element being output, and
   can be incremented by the format write and read methods.
*/
class VectorAndPointer implements FormatInputList, FormatOutputList
{
  private Vector v = null;
  private int vecptr = 0;
  // On output, vecptr points at the next element to be used.
  // On input, it points at the next free slot to be filled.


  public VectorAndPointer( Vector v )
  {
    this.v = v;
  }


  public VectorAndPointer()
  {
    this.v = new Vector();
  }


  public boolean hasCurrentElement()
  {
    return ( this.vecptr < this.v.size() );
  }


  public void checkCurrentElementForWrite( FormatElement format_element )
         throws EndOfVectorOnWriteException
  {
    if ( !hasCurrentElement() )
      throw new EndOfVectorOnWriteException( this.vecptr,
                                             format_element.toString()
                                           );
  }


  /* Checks that the current element in the input list is OK and
     throws an exception if not. For this implementation of
     FormatInputList, there are no error conditions - we
     introduced the method for the StringHashtableAndPointer class,
     and need it here for compatibility.
     format_element and in are only for generating error messages.
  */
  public void checkCurrentElementForRead( FormatElement format_element,
                                          InputStreamAndBuffer in
                                        )
  {
  }


  public Object getCurrentElement()
  {
    return this.v.elementAt( this.vecptr );
  }

  public Object getCurrentElementAndAdvance()
  {
    this.vecptr = this.vecptr+1;
    return this.v.elementAt( this.vecptr-1 );
  }


  /* Puts o into the input list and advances its pointer.
     format_element and in are only for generating error messages,
     and not used in this implementation, since no error conditions
     can arise.
  */
  public void putElementAndAdvance( Object o,
                                    FormatElement format_element,
                                    InputStreamAndBuffer in
                                  )
  {
    this.v.addElement(o);
    this.vecptr = this.vecptr + 1;
  }


  public void advance()
  {
    this.vecptr = this.vecptr + 1;
  }


  /* Returns the current pointer.
     Used only in generating error messages.
  */
  public int getPtr()
  {
    return this.vecptr;
  }
}


/* This class represents a Vector of Strings and a current-element pointer.
   We use it when inputting data against a format.
*/
class StringsHashtableAndPointer implements FormatInputList
{
  private VectorAndPointer vp;
  private Hashtable ht;


  public StringsHashtableAndPointer( Vector strings, Hashtable ht )
  {
    this.vp = new VectorAndPointer( strings );
    this.ht = ht;
  }


  /* Checks that there is a current element in the key vector, and
     throws an exception if not.
     format_element and in are only for generating error messages.
  */
  public void checkCurrentElementForRead( FormatElement format_element,
                                          InputStreamAndBuffer in
                                        )
              throws EndOfKeyVectorOnReadException
  {
    if ( !(this.vp.hasCurrentElement() ) )
      throw new EndOfKeyVectorOnReadException( this.vp.getPtr(),
                                               format_element.toString(),
                                               in.getLineErrorReport()
                                             );
  }


  /* Puts o into the input list and advances its pointer.
     In this implementation, that means getting the current key,
     putting o into an appropriate hashtable slot, and advancing
     the pointer in the vector of keys.
     format_element and in are only for generating error messages.
  */
  public void putElementAndAdvance( Object o,
                                    FormatElement format_element,
                                    InputStreamAndBuffer in
                                  )
              throws KeyNotStringOnReadException
  {
    Object current_key = this.vp.getCurrentElement();
    if ( current_key instanceof String ) {
      this.ht.put( (String)current_key, o );
      this.vp.advance();
    }
    else
      throw new KeyNotStringOnReadException( current_key,
                                             this.vp.getPtr(),
                                             format_element.toString(),
                                             in.getLineErrorReport()
                                           );
  }


  /* Returns the current pointer.
     Used only in generating error messages.
  */
  public int getPtr()
  {
    return this.vp.getPtr();
  }
}


/* This class holds an input stream and a line buffer.
*/
class InputStreamAndBuffer
{
  private DataInputStream in;
  // The stream we read from.

  private String line;
  // The line just read.

  private int ptr;
  // Initialised to 0 after reading a line. Index of the next
  // character to use in line.

  private int line_number;
  // Initially 0. Is incremented each time a line is read, so
  // the first line read is number 1.

  private boolean nothing_read;
  // Initially true. Is set false after reading a line. We
  // use this so that the first call of getSlice
  // knows to read a line.


  public InputStreamAndBuffer( DataInputStream in )
  {
    this.in = in;
    this.ptr = 0;
    this.line = "";
    this.line_number = 0;
    this.nothing_read = true;
  }

  /* Really crappy readline implementation to quiet deprecation warnings
   * about using DataInputStream.readLine(). --kgs
   */

  public String readLine_hack() throws java.io.IOException
  {
    StringBuffer sb = new StringBuffer();
    int c = 0, skip = 0;

    /* if the first format spec is nX, then the ptr will be greater than zero,
     * but nothing_read will be true. in that case, skip the first n chars.
     * --kgs
     */
    if(this.nothing_read && (this.ptr > 0))
      skip = this.ptr;

    while(c >= 0) {
      c = in.read();

      if(c < 0)
        return null;

      if((char)c == '\n')
        break;

      if(skip > 0)
        skip--;
      else
        sb.append((char) c);
    }

    return sb.toString();
  }

  /* Reads the next line into the line buffer.
     vecptr and format are used only in generating error messages.
  */
  public void readLine( int vecptr, FormatElement format )
              throws EndOfFileWhenStartingReadException,
                     LineMissingOnReadException,
                     IOExceptionOnReadException
  {
    try {
      String line = readLine_hack();

      if ( line == null ) {
        if ( this.nothing_read )
          throw new EndOfFileWhenStartingReadException( vecptr,
                                                        format.toString(),
                                                        this.line,
                                                        this.line_number
                                                      );
        else
          throw new LineMissingOnReadException( vecptr,
                                                format.toString(),
                                                this.line,
                                                this.line_number
                                              );
      }
      else {
        this.ptr = 0;
        this.nothing_read = false;
        this.line_number = this.line_number + 1;
        this.line = line;
        // Don't do the assignment until we've checked for a null
        // line, because then we can then use this.line as the
        // previous value for error messages.
      }
    }
    catch ( IOException e ) {
      throw new IOExceptionOnReadException( this.line, this.line_number,
                                            e.getMessage()
                                          );
    }
  }


  /* Returns a string consisting of the next width characters,
     and throws an exception if the line is not long enough.
     The 'vecptr' and 'format' parameters are used only in generating error
     messages.
  */
  public String getSlice( int width, int vecptr, FormatElement format )
                throws DataMissingOnReadException,
                       LineMissingOnReadException,
                       EndOfFileWhenStartingReadException,
                       IOExceptionOnReadException
  {
    if ( this.nothing_read )
      readLine( vecptr, format );

    if ( this.ptr+width > this.line.length() ) {
      /* if there aren't 'width' characters left, just return the
       * remainder of the line.  --kgs
       */
      return this.line.substring( this.ptr );
    }
    else {
      return this.line.substring( this.ptr, this.ptr+width );
    }
  }


  /* Advances the pointer by width.
  */
  public void advance( int width )
  {
    this.ptr = this.ptr + width;
  }


  /* Generates an error report showing the line, character pointer
     ptr and line number.
  */
  public String getLineErrorReport()
  {
    StringBuffer s = new StringBuffer();

    /* Report the line number. */
    s.append( "  Line number = " + this.line_number + ":\n" );

    /* Show the line. */
    s.append( this.line + "\n" );

    /* Show an arrow under ptr. */
    for ( int i=0; i<this.ptr; i++ )
      s.append( " " );
    s.append( "^" );

    return s.toString();
  }
}


/* This exception is a generic one, a superclass of all those
   thrown to report an error while doing formatted output.
*/
abstract class OutputFormatException extends Exception
{
  public OutputFormatException( String s )
  {
    super( s );
  }

  public OutputFormatException()
  {
    super();
  }
}


/* This exception is thrown if formatted output runs off the
   end of the vector being output before it has completed the
   format.
*/
class EndOfVectorOnWriteException extends OutputFormatException
{
  public EndOfVectorOnWriteException( int vecptr,
                                      String format
                                    )
  {
    this( "End of vector while writing formatted data:\n" +
          "  Index  = " + vecptr + "\n" +
          "  Format = " + format + " ."
        );
  }

  public EndOfVectorOnWriteException( String s )
  {
    super( s );
  }

  public EndOfVectorOnWriteException( )
  {
    super( );
  }
}


/* This exception is thrown if formatted output detects an object
   that's the wrong type for a format element, e.g. a real
   when outputting against an Iw element.
*/
class IllegalObjectOnWriteException extends OutputFormatException
{
  public IllegalObjectOnWriteException( Object o,
                                        int vecptr,
                                        String format
                                      )
  {
    this( "Illegal object while writing formatted data:\n" +
          "  Object = \"" + o + "\"\n" +
          "  Index  = " + vecptr + "\n" +
          "  Format = " + format + " ."
        );
  }

  public IllegalObjectOnWriteException( String s )
  {
    super( s );
  }

  public IllegalObjectOnWriteException( )
  {
    super( );
  }
}


/* This exception is thrown if formatted output detects a string
   that won't fit in its format, e.g. trying to output abcde
   against an A4 element.
*/
class StringTooWideOnWriteException extends OutputFormatException
{
  public StringTooWideOnWriteException( String s,
                                        int vecptr,
                                        String format
                                      )
  {
    this( "String too wide while writing formatted data:\n" +
          "  String = \"" + s + "\"\n" +
          "  Index  = " + vecptr + "\n" +
          "  Format = " + format + " ."
        );
  }

  public StringTooWideOnWriteException( String s )
  {
    super( s );
  }

  public StringTooWideOnWriteException( )
  {
    super( );
  }
}


/* This exception is thrown if formatted output detects a number
   that won't fit in its format, e.g. trying to output 1234
   against an I3 element.
*/
class NumberTooWideOnWriteException extends OutputFormatException
{
  public NumberTooWideOnWriteException( Number n,
                                        int vecptr,
                                        String format
                                      )
  {
    this( "Number too wide while writing formatted data:\n" +
          "  Number = \"" + n + "\"\n" +
          "  Index  = " + vecptr + "\n" +
          "  Format = " + format + " ."
        );
  }

  public NumberTooWideOnWriteException( String s )
  {
    super( s );
  }

  public NumberTooWideOnWriteException( )
  {
    super( );
  }
}


/* This exception is a generic one, a superclass of all those
   thrown to report an error while doing formatted input.
*/
abstract class InputFormatException extends Exception
{
  public InputFormatException( String s )
  {
    super( s );
  }

  public InputFormatException()
  {
    super();
  }


}


class LineMissingOnReadException extends InputFormatException
{
  public LineMissingOnReadException( int vecptr,
                                     String format,
                                     String line,
                                     int line_number
                                   )
  {
    this( "End of file while reading formatted data:\n" +
          "  Index  = " + vecptr + "\n" +
          "  Format = " + format + "\n" +
          "Last line was number " + line_number + ":\n" +
          line
        );
  }

  public LineMissingOnReadException( String s )
  {
    super( s );
  }

  public LineMissingOnReadException( )
  {
    super( );
  }
}


class DataMissingOnReadException extends InputFormatException
{
  public DataMissingOnReadException( int vecptr,
                                     String format,
                                     String line_error_report
                                   )
  {
    this("Warning: EOL reading formatted data: idx=" +
          vecptr + " fmt=" + format);
  }

  public DataMissingOnReadException( String s )
  {
    super( s );
  }

  public DataMissingOnReadException( )
  {
    super( );
  }
}


class InvalidNumberOnReadException extends InputFormatException
{
  public InvalidNumberOnReadException( String number,
                                       int vecptr,
                                       String format,
                                       String line_error_report,
                                       String parser_message
                                     )
  {
    this( "Invalid number while reading formatted data:\n" +
          "  Number = \"" + number + "\"\n" +
          "  Index  = " + vecptr + "\n" +
          "  Format = " + format + "\n" +
          line_error_report + "\n" +
          parser_message
        );
  }

  public InvalidNumberOnReadException( String s )
  {
    super( s );
  }

  public InvalidNumberOnReadException( )
  {
    super( );
  }
}


class UnmatchedStringOnReadException extends InputFormatException
{
  public UnmatchedStringOnReadException( String string,
                                         int vecptr,
                                         String format,
                                         String line_error_report
                                       )
  {
    this( "Unmatched string while reading formatted data:\n" +
          "  String = \"" + string + "\"\n" +
          "  Index  = " + vecptr + "\n" +
          "  Format = " + format + "\n" +
          line_error_report + "\n"
        );
  }

  public UnmatchedStringOnReadException( String s )
  {
    super( s );
  }

  public UnmatchedStringOnReadException( )
  {
    super( );
  }
}


class EndOfKeyVectorOnReadException extends InputFormatException
{
  public EndOfKeyVectorOnReadException( int vecptr,
                                        String format,
                                        String line_error_report
                                      )
  {
    this( "End of key vector while reading formatted data:\n" +
          "  Index  = " + vecptr + "\n" +
          "  Format = " + format + "\n" +
          line_error_report + "\n"
        );
  }

  public EndOfKeyVectorOnReadException( String s )
  {
    super( s );
  }

  public EndOfKeyVectorOnReadException( )
  {
    super( );
  }
}


class KeyNotStringOnReadException extends InputFormatException
{
  public KeyNotStringOnReadException( Object key,
                                      int vecptr,
                                      String format,
                                      String line_error_report
                                    )
  {
    this( "Key not string while reading formatted data:\n" +
          "  Key    = \"" + vecptr + "\"\n" +
          "  Index  = " + vecptr + "\n" +
          "  Format = " + format + "\n" +
          line_error_report + "\n"
        );
  }

  public KeyNotStringOnReadException( String s )
  {
    super( s );
  }

  public KeyNotStringOnReadException( )
  {
    super( );
  }
}


class IOExceptionOnReadException extends InputFormatException
{
  public IOExceptionOnReadException( String line,
                                     int line_number,
                                     String IOMessage
                                   )
  {
    this( "IOException while reading formatted data:\n" +
          "Last line was number " + line_number + ":\n" +
          line + "\n" +
          IOMessage
        );
  }

  public IOExceptionOnReadException( String s )
  {
    super( s );
  }

  public IOExceptionOnReadException( )
  {
    super( );
  }
}


/* This exception is thrown when a syntax error is detected while
   parsing a format string.
*/
class InvalidFormatException extends Exception
{
  public InvalidFormatException( String parser_message )
  {
    super( parser_message );
  }

  public InvalidFormatException( )
  {
    super( );
  }
}


/* This class is used to hold the parsers for formats and numbers.
   We generate them static (see JavaCC documentation) because it
   makes them more efficient. However, that then means that we need
   somewhere to put an instance of each. That's what we use the result
   of Parsers.theParsers() for.
*/
class Parsers
{
  static boolean already_created = false;
  static Parsers parsers = null;

  FormatParser format_parser = null;
  NumberParser number_parser = null;


  static Parsers theParsers()
  {
    if ( !(already_created) ) {
      parsers = new Parsers();
      already_created = true;
    }
    return parsers;
  }


  private Parsers()
  {
    this.format_parser = new FormatParser( new StringReader("") );
    this.number_parser = new NumberParser( new StringReader("") );
  }
}
