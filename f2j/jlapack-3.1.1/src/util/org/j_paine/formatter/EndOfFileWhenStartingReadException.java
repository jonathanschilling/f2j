package org.j_paine.formatter;

public class EndOfFileWhenStartingReadException extends InputFormatException
{
  public EndOfFileWhenStartingReadException( int vecptr,
                                             String format,
                                             String line,
                                             int line_number
                                           )
  {
    this( "End of file when starting read of formatted data:\n" +
          "  Index  = " + vecptr + "\n" +
          "  Format = " + format + "\n" +
          "Last line was number " + line_number + ":\n" +
          line
        );
  }

  public EndOfFileWhenStartingReadException( String s )
  {
    super( s );
  }

  public EndOfFileWhenStartingReadException( )
  {
    super( );
  }
}
