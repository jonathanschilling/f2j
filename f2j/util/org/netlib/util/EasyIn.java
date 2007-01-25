package org.netlib.util;

import java.io.*;

/**
 * Simple input from the keyboard for all primitive types. ver 1.0
 * <p>
 * Copyright (c) Peter van der Linden,  May 5 1997.
 *     corrected error message 11/21/97
 * <p>
 * The creator of this software hereby gives you permission to:
 * <ol>
 *  <li> copy the work without changing it
 *  <li> modify the work providing you send me a copy which I can
 *     use in any way I want, including incorporating into this work.
 *  <li> distribute copies of the work to the public by sale, lease, 
 *     rental, or lending
 *  <li> perform the work
 *  <li> display the work
 *  <li> fold the work into a funny hat and wear it on your head.
 * </ol>
 * <p>
 * This is not thread safe, not high performance, and doesn't tell EOF.
 * It's intended for low-volume easy keyboard input.
 * An example of use is:
 * <p>
 * <code>
 *     EasyIn easy = new EasyIn(); <br>
 *     int i = easy.readInt();   // reads an int from System.in <br>
 *     float f = easy.readFloat();   // reads a float from System.in <br>
 * </code>
 * <p>
 * 2/25/98 - modified by Keith Seymour to be useful with the f2j
 *           translator.
 * <p>
 * @author Peter van der Linden
 */

public class EasyIn {
    static InputStreamReader is = new InputStreamReader( System.in );
    static BufferedReader br = new BufferedReader( is );
    static String line = null;
    static int idx, len;
    static String blank_string = "                                                                                           ";

    /**
     * Reset the tokenizer.
     *
     * @throws IOException if an input or output exception occurred.
     */
    private void initTokenizer() throws IOException {
      do {
        line = br.readLine();
  
        if(line == null)
          throw new IOException("EOF");

        idx = 0;
        len = line.length();
      } while(!hasTokens(line));
    }

    /**
     * Checks if the string contains any tokens.
     *
     * @param str string to check
     *
     * @return true if there are tokens, false otherwise.
     */
    private boolean hasTokens(String str)
    {
      int i, str_len;
   
      str_len = str.length();

      for(i=0;i < str_len;i++)
        if(! isDelim(str.charAt(i)))
          return true;

      return false;
    }

    /**
     * Checks if this character is a delimiter.
     *
     * @param c character to check
     *
     * @return true if this character is a delimiter, false otherwise.
     */
    private boolean isDelim(char c)
    {
      return ( (c == ' ') || (c == '\t') || (c == '\r') || (c == '\n'));
    }

    /**
     * Checks if there are more tokens.
     *
     * @return true if there are more tokens, false otherwise.
     */
    private boolean moreTokens()
    {
      return ( idx < len );
    }
      
    /**
     * Gets the next token.
     *
     * @throws IOException if an input or output exception occurred.
     *
     * @return the token
     */
    private String getToken() throws IOException {
       int begin,end;

       if( (line == null) || !moreTokens() )
         initTokenizer();

       while( (idx < len) && isDelim(line.charAt(idx)) )
         idx++;

       if(idx == len) {
         initTokenizer();
         while( (idx < len) && isDelim(line.charAt(idx)) )
           idx++;
       }

       begin = idx;

       while( (idx < len) && !isDelim(line.charAt(idx)) )
         idx++;

       end = idx;

       return line.substring(begin,end);
    }

    /**
     * Reads the specified number of characters and returns a new String
     * containing them.
     *
     * @param num_chars the number of characters to read
     *
     * @throws IOException if an input or output exception occurred.
     *
     * @return the String containing the characters read.
     */
    public String readchars(int num_chars) throws IOException {
      int cp_idx;

      if( (line == null) || !moreTokens() )
        initTokenizer();

      cp_idx = idx;

      if(cp_idx + num_chars < len)
      {
        idx += num_chars;
        return( line.substring(cp_idx,cp_idx+num_chars) );
      }
      else
      {
        idx = len;
        return(line.substring(cp_idx,len) + blank_string.substring(0,num_chars-(len-cp_idx)));
      }
    }

    /**
     * Reads the specified number of characters and returns a new String
     * containing them.  Unlike readchars(), does not throw IOException.
     *
     * @param num_chars the number of characters to read
     *
     * @return the String containing the characters read.
     */
    public String readChars(int num_chars) {
      try{ 
        return readchars(num_chars);
      }catch (IOException e) {
        System.err.println("IO Exception in EasyIn.readChars");
        return null;
      }
    }

    /** 
     * Skips any tokens remaining on this line.
     */
    public void skipRemaining() {
      line = null;  //may not be needed
      idx = len;
    }

    /**
     * Gets a boolean value from the next token.
     *
     * @return the boolean value
     *
     * @throws IOException if an input or output exception occurred.
     */
    public boolean readboolean() throws IOException {
          char ch = getToken().charAt(0);
          if((ch == 't') || (ch == 'T'))
            return true;
          else 
            return false;
    }

    /**
     * Gets a boolean value from the next token.
     * Same as readboolean() except it does not throw IOException.
     *
     * @return the boolean value
     */
    public boolean readBoolean() {
       try {
          char ch = getToken().charAt(0);
          if((ch == 't') || (ch == 'T'))
            return true;
          else 
            return false;
       } catch (IOException ioe) {
          System.err.println("IO Exception in EasyIn.readBoolean");
          return false;
       }
    }

    /**
     * Gets a byte value from the next token.
     *
     * @return the byte value
     *
     * @throws IOException if an input or output exception occurred.
     */
    public byte readbyte() throws IOException {
      return Byte.parseByte(getToken());
    }

    /**
     * Gets a byte value from the next token.
     * Same as readbyte() except it does not throw IOException.
     *
     * @return the byte value
     */
    public byte readByte() {
       try {
         return Byte.parseByte(getToken());
       } catch (IOException ioe) {
          System.err.println("IO Exception in EasyIn.readByte");
          return 0;
       }
    }

    /**
     * Gets a short value from the next token.
     *
     * @return the short value
     *
     * @throws IOException if an input or output exception occurred.
     */
    public short readshort() throws IOException {
      return Short.parseShort(getToken());
    }

    /**
     * Gets a short value from the next token.
     * Same as readshort() except it does not throw IOException.
     *
     * @return the short value
     */
    public short readShort() {
       try {
         return Short.parseShort(getToken());
       } catch (IOException ioe) {
          System.err.println("IO Exception in EasyIn.readShort");
          return 0;
       }
    }

    /**
     * Gets an integer value from the next token.
     *
     * @return the integer value
     *
     * @throws IOException if an input or output exception occurred.
     */
    public int readint() throws IOException {
      return Integer.parseInt(getToken());
    }

    /**
     * Gets an integer value from the next token.
     * Same as readint() except it does not throw IOException.
     *
     * @return the integer value
     */
    public int readInt() {
       try {
         return Integer.parseInt(getToken());
       } catch (IOException ioe) {
          System.err.println("IO Exception in EasyIn.readInt");
          return 0;
       }
    }

    /**
     * Gets a long integer value from the next token.
     *
     * @return the long integer value
     *
     * @throws IOException if an input or output exception occurred.
     */
    public long readlong() throws IOException {
      return Long.parseLong(getToken());
    }

    /**
     * Gets a long integer value from the next token.
     * Same as readlong() except it does not throw IOException.
     *
     * @return the long integer value
     */
    public long readLong() {
       try {
         return Long.parseLong(getToken());
       } catch (IOException ioe) {
          System.err.println("IO Exception in EasyIn.readLong");
          return 0L;
       }
    }

    /**
     * Gets a float value from the next token.
     *
     * @return the float value
     *
     * @throws IOException if an input or output exception occurred.
     */
    public float readfloat() throws IOException {
      return new Float(getToken()).floatValue();
    }

    /**
     * Gets a float value from the next token.
     * Same as readfloat() except it does not throw IOException.
     *
     * @return the float value
     */
    public float readFloat() {
       try {
         return new Float(getToken()).floatValue();
       } catch (IOException ioe) {
          System.err.println("IO Exception in EasyIn.readFloat");
          return 0.0F;
       }
    }

    /**
     * Gets a double value from the next token.
     *
     * @return the double value
     *
     * @throws IOException if an input or output exception occurred.
     */
    public double readdouble() throws IOException {
      return new Double(getToken()).doubleValue();
    }

    /**
     * Gets a double value from the next token.
     * Same as readdouble() except it does not throw IOException.
     *
     * @return the double value
     */
    public double readDouble() {
       try {
         return new Double(getToken()).doubleValue();
       } catch (IOException ioe) {
          System.err.println("IO Exception in EasyIn.readDouble");
          return 0.0;
       }
    }

    /**
     * Gets a character value from the next token.
     *
     * @return the character value
     *
     * @throws IOException if an input or output exception occurred.
     */
    public char readchar() throws IOException {
      return getToken().charAt(0);
    }

    /**
     * Gets a character value from the next token.
     * Same as readchar() except it does not throw IOException.
     *
     * @return the character value
     */
    public char readChar() {
       try {
          return getToken().charAt(0);
       } catch (IOException ioe) {
          System.err.println("IO Exception in EasyIn.readChar");
          return 0;
       }
    }

    /**
     * Gets a string value from the next token.
     *
     * @return the string value
     *
     * @throws IOException if an input or output exception occurred.
     */
    public String readstring() throws IOException {
      return br.readLine(); 
    }

    /**
     * Gets a string value from the next token.
     * Same as readstring() except it does not throw IOException.
     *
     * @return the string value
     */
    public String readString() {
       try {
         return br.readLine(); 
       } catch (IOException ioe) {
          System.err.println("IO Exception in EasyIn.readString");
          return "";
       }
    }

   /**
    * This method is just here to test the class
    */

   public static void main (String args[]){
       EasyIn easy = new EasyIn();

       System.out.print("enter char: "); System.out.flush();
       System.out.println("You entered: " + easy.readChar() );

       System.out.print("enter String: "); System.out.flush();
       System.out.println("You entered: " + easy.readString() );

       System.out.print("enter boolean: "); System.out.flush();
       System.out.println("You entered: " + easy.readBoolean() );

       System.out.print("enter byte: "); System.out.flush();
       System.out.println("You entered: " + easy.readByte() );

       System.out.print("enter short: "); System.out.flush();
       System.out.println("You entered: " + easy.readShort() );

       System.out.print("enter int: "); System.out.flush();
       System.out.println("You entered: " + easy.readInt() );

       System.out.print("enter long: "); System.out.flush();
       System.out.println("You entered: " + easy.readLong() );

       System.out.print("enter float: "); System.out.flush();
       System.out.println("You entered: " + easy.readFloat() );

       System.out.print("enter double: "); System.out.flush();
       System.out.println("You entered: " + easy.readDouble() );
   }
}
