// Simple input from the keyboard for all primitive types. ver 1.0
// Copyright (c) Peter van der Linden,  May 5 1997.
//     corrected error message 11/21/97
//
// The creator of this software hereby gives you permission to:
//  1. copy the work without changing it
//  2. modify the work providing you send me a copy which I can
//     use in any way I want, including incorporating into this work.
//  3. distribute copies of the work to the public by sale, lease, 
//     rental, or lending
//  4. perform the work
//  5. display the work
//  6. fold the work into a funny hat and wear it on your head.
//
// This is not thread safe, not high performance, and doesn't tell EOF.
// It's intended for low-volume easy keyboard input.
// An example of use is:
//     EasyIn easy = new EasyIn();
//     int i = easy.readInt();   // reads an int from System.in
//     float f = easy.readFloat();   // reads a float from System.in
//
// 2/25/98 - modified by Keith Seymour to be useful with the f2java
//           translator.

package org.netlib.util;
import java.io.*;

public class EasyIn {
    static InputStreamReader is = new InputStreamReader( System.in );
    static BufferedReader br = new BufferedReader( is );
    static String line = null;
    static int idx, len;
    static String blank_string = "                                                                                           ";

    private void initTokenizer() throws IOException {
      do {
        line = br.readLine();
  
        if(line == null)
          throw new IOException("EOF");

        idx = 0;
        len = line.length();
      } while(!hasTokens(line));
    }

    private boolean hasTokens(String str)
    {
      int i, str_len;
   
      str_len = str.length();

      for(i=0;i < str_len;i++)
        if(! isDelim(str.charAt(i)))
          return true;

      return false;
    }

    private boolean isDelim(char c)
    {
      // return ( (c == ' ') || (c == '\t') || (c == '\r') );
      return ( (c == ' ') || (c == '\t') || (c == '\r') || (c == '\n'));
    }

    private boolean moreTokens()
    {
      return ( idx < len );
    }
      
    private String getToken() throws IOException {
       int begin,end;

       if( (line == null) || !moreTokens() )
         initTokenizer();

       while( (idx < len) && isDelim(line.charAt(idx)) )
         idx++;

       begin = idx;

       while( (idx < len) && !isDelim(line.charAt(idx)) )
         idx++;

       end = idx;

       return line.substring(begin,end);
    }

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

    public String readChars(int num_chars) {
      try{ 
        return readchars(num_chars);
      }catch (IOException e) {
        System.err.println("IO Exception in EasyIn.readChars");
        return null;
      }
    }

    public void skipRemaining() {
      line = null;  //may not be needed
      idx = len;
    }

    public boolean readboolean() throws IOException {
          char ch = getToken().charAt(0);
          if((ch == 't') || (ch == 'T'))
            return true;
          else 
            return false;
    }

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

    public byte readbyte() throws IOException {
      return Byte.parseByte(getToken());
    }

    public byte readByte() {
       try {
         return Byte.parseByte(getToken());
       } catch (IOException ioe) {
          System.err.println("IO Exception in EasyIn.readByte");
          return 0;
       }
    }

    public short readshort() throws IOException {
      return Short.parseShort(getToken());
    }

    public short readShort() {
       try {
         return Short.parseShort(getToken());
       } catch (IOException ioe) {
          System.err.println("IO Exception in EasyIn.readShort");
          return 0;
       }
    }

    public int readint() throws IOException {
      return Integer.parseInt(getToken());
    }

    public int readInt() {
       try {
         return Integer.parseInt(getToken());
       } catch (IOException ioe) {
          System.err.println("IO Exception in EasyIn.readInt");
          return 0;
       }
    }

    public long readlong() throws IOException {
      return Long.parseLong(getToken());
    }

    public long readLong() {
       try {
         return Long.parseLong(getToken());
       } catch (IOException ioe) {
          System.err.println("IO Exception in EasyIn.readLong");
          return 0L;
       }
    }

    public float readfloat() throws IOException {
      return new Float(getToken()).floatValue();
    }

    public float readFloat() {
       try {
         return new Float(getToken()).floatValue();
       } catch (IOException ioe) {
          System.err.println("IO Exception in EasyIn.readFloat");
          return 0.0F;
       }
    }

    public double readdouble() throws IOException {
      return new Double(getToken()).doubleValue();
    }

    public double readDouble() {
       try {
         return new Double(getToken()).doubleValue();
       } catch (IOException ioe) {
          System.err.println("IO Exception in EasyIn.readDouble");
          return 0.0;
       }
    }

    public char readchar() throws IOException {
      return getToken().charAt(0);
    }

    public char readChar() {
       try {
          return getToken().charAt(0);
       } catch (IOException ioe) {
          System.err.println("IO Exception in EasyIn.readChar");
          return 0;
       }
    }

    public String readstring() throws IOException {
      return br.readLine(); 
    }

    public String readString() {
       try {
         return br.readLine(); 
       } catch (IOException ioe) {
          System.err.println("IO Exception in EasyIn.readString");
          return "";
       }
    }

// This method is just here to test the class
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
