/* Generated By:JavaCC: Do not edit this line. FormatParserTokenManager.java */
package org.j_paine.formatter;

public class FormatParserTokenManager implements FormatParserConstants
{
  public  java.io.PrintStream debugStream = System.out;
  public  void setDebugStream(java.io.PrintStream ds) { debugStream = ds; }
private final int jjStopStringLiteralDfa_0(int pos, long active0)
{
   switch (pos)
   {
      default :
         return -1;
   }
}
private final int jjStartNfa_0(int pos, long active0)
{
   return jjMoveNfa_0(jjStopStringLiteralDfa_0(pos, active0), pos + 1);
}
private final int jjStopAtPos(int pos, int kind)
{
   jjmatchedKind = kind;
   jjmatchedPos = pos;
   return pos + 1;
}
private final int jjStartNfaWithStates_0(int pos, int kind, int state)
{
   jjmatchedKind = kind;
   jjmatchedPos = pos;
   try { curChar = input_stream.readChar(); }
   catch(java.io.IOException e) { return pos + 1; }
   return jjMoveNfa_0(state, pos + 1);
}
private final int jjMoveStringLiteralDfa0_0()
{
   switch(curChar)
   {
      case 40:
         return jjStopAtPos(0, 16);
      case 41:
         return jjStopAtPos(0, 17);
      case 44:
         return jjStopAtPos(0, 18);
      case 45:
         return jjStopAtPos(0, 13);
      case 46:
         return jjStopAtPos(0, 14);
      case 47:
         return jjStopAtPos(0, 15);
      default :
         return jjMoveNfa_0(2, 0);
   }
}
private final void jjCheckNAdd(int state)
{
   if (jjrounds[state] != jjround)
   {
      jjstateSet[jjnewStateCnt++] = state;
      jjrounds[state] = jjround;
   }
}
private final void jjAddStates(int start, int end)
{
   do {
      jjstateSet[jjnewStateCnt++] = jjnextStates[start];
   } while (start++ != end);
}
private final void jjCheckNAddTwoStates(int state1, int state2)
{
   jjCheckNAdd(state1);
   jjCheckNAdd(state2);
}
private final void jjCheckNAddStates(int start, int end)
{
   do {
      jjCheckNAdd(jjnextStates[start]);
   } while (start++ != end);
}
private final void jjCheckNAddStates(int start)
{
   jjCheckNAdd(jjnextStates[start]);
   jjCheckNAdd(jjnextStates[start + 1]);
}
static final long[] jjbitVec0 = {
   0x0L, 0x0L, 0xffffffffffffffffL, 0xffffffffffffffffL
};
private final int jjMoveNfa_0(int startState, int curPos)
{
   int[] nextStates;
   int startsAt = 0;
   jjnewStateCnt = 16;
   int i = 1;
   jjstateSet[0] = startState;
   int j, kind = 0x7fffffff;
   for (;;)
   {
      if (++jjround == 0x7fffffff)
         ReInitRounds();
      if (curChar < 64)
      {
         long l = 1L << curChar;
         MatchLoop: do
         {
            switch(jjstateSet[--i])
            {
               case 2:
                  if ((0x3ff000000000000L & l) != 0L)
                  {
                     if (kind > 2)
                        kind = 2;
                     jjCheckNAdd(1);
                  }
                  else if (curChar == 39)
                     jjCheckNAddStates(0, 2);
                  else if (curChar == 32)
                  {
                     if (kind > 1)
                        kind = 1;
                     jjCheckNAdd(0);
                  }
                  break;
               case 0:
                  if (curChar != 32)
                     break;
                  if (kind > 1)
                     kind = 1;
                  jjCheckNAdd(0);
                  break;
               case 1:
                  if ((0x3ff000000000000L & l) == 0L)
                     break;
                  if (kind > 2)
                     kind = 2;
                  jjCheckNAdd(1);
                  break;
               case 3:
                  if ((0xffffff7fffffffffL & l) != 0L)
                     jjCheckNAddStates(0, 2);
                  break;
               case 4:
                  if (curChar == 39)
                     jjCheckNAddStates(0, 2);
                  break;
               case 5:
                  if (curChar == 39)
                     jjstateSet[jjnewStateCnt++] = 4;
                  break;
               case 6:
                  if (curChar == 39 && kind > 3)
                     kind = 3;
                  break;
               default : break;
            }
         } while(i != startsAt);
      }
      else if (curChar < 128)
      {
         long l = 1L << (curChar & 077);
         MatchLoop: do
         {
            switch(jjstateSet[--i])
            {
               case 2:
                  if ((0x100000001000L & l) != 0L)
                  {
                     if (kind > 12)
                        kind = 12;
                  }
                  else if ((0x8000000080L & l) != 0L)
                  {
                     if (kind > 11)
                        kind = 11;
                  }
                  else if ((0x2000000020L & l) != 0L)
                  {
                     if (kind > 10)
                        kind = 10;
                  }
                  else if ((0x1000000010L & l) != 0L)
                  {
                     if (kind > 9)
                        kind = 9;
                  }
                  else if ((0x4000000040L & l) != 0L)
                  {
                     if (kind > 8)
                        kind = 8;
                  }
                  else if ((0x20000000200L & l) != 0L)
                  {
                     if (kind > 7)
                        kind = 7;
                  }
                  else if ((0x100000001000000L & l) != 0L)
                  {
                     if (kind > 6)
                        kind = 6;
                  }
                  else if ((0x1000000010000L & l) != 0L)
                  {
                     if (kind > 5)
                        kind = 5;
                  }
                  else if ((0x200000002L & l) != 0L)
                  {
                     if (kind > 4)
                        kind = 4;
                  }
                  break;
               case 3:
                  jjAddStates(0, 2);
                  break;
               case 7:
                  if ((0x200000002L & l) != 0L && kind > 4)
                     kind = 4;
                  break;
               case 8:
                  if ((0x1000000010000L & l) != 0L && kind > 5)
                     kind = 5;
                  break;
               case 9:
                  if ((0x100000001000000L & l) != 0L && kind > 6)
                     kind = 6;
                  break;
               case 10:
                  if ((0x20000000200L & l) != 0L && kind > 7)
                     kind = 7;
                  break;
               case 11:
                  if ((0x4000000040L & l) != 0L && kind > 8)
                     kind = 8;
                  break;
               case 12:
                  if ((0x1000000010L & l) != 0L && kind > 9)
                     kind = 9;
                  break;
               case 13:
                  if ((0x2000000020L & l) != 0L && kind > 10)
                     kind = 10;
                  break;
               case 14:
                  if ((0x8000000080L & l) != 0L && kind > 11)
                     kind = 11;
                  break;
               case 15:
                  if ((0x100000001000L & l) != 0L && kind > 12)
                     kind = 12;
                  break;
               default : break;
            }
         } while(i != startsAt);
      }
      else
      {
         int i2 = (curChar & 0xff) >> 6;
         long l2 = 1L << (curChar & 077);
         MatchLoop: do
         {
            switch(jjstateSet[--i])
            {
               case 3:
                  if ((jjbitVec0[i2] & l2) != 0L)
                     jjAddStates(0, 2);
                  break;
               default : break;
            }
         } while(i != startsAt);
      }
      if (kind != 0x7fffffff)
      {
         jjmatchedKind = kind;
         jjmatchedPos = curPos;
         kind = 0x7fffffff;
      }
      ++curPos;
      if ((i = jjnewStateCnt) == (startsAt = 16 - (jjnewStateCnt = startsAt)))
         return curPos;
      try { curChar = input_stream.readChar(); }
      catch(java.io.IOException e) { return curPos; }
   }
}
static final int[] jjnextStates = {
   3, 5, 6, 
};
public static final String[] jjstrLiteralImages = {
"", null, null, null, null, null, null, null, null, null, null, null, null, 
"\55", "\56", "\57", "\50", "\51", "\54", };
public static final String[] lexStateNames = {
   "DEFAULT", 
};
static final long[] jjtoToken = {
   0x7fffdL, 
};
static final long[] jjtoSkip = {
   0x2L, 
};
protected SimpleCharStream input_stream;
private final int[] jjrounds = new int[16];
private final int[] jjstateSet = new int[32];
protected char curChar;
public FormatParserTokenManager(SimpleCharStream stream)
{
   if (SimpleCharStream.staticFlag)
      throw new Error("ERROR: Cannot use a static CharStream class with a non-static lexical analyzer.");
   input_stream = stream;
}
public FormatParserTokenManager(SimpleCharStream stream, int lexState)
{
   this(stream);
   SwitchTo(lexState);
}
public void ReInit(SimpleCharStream stream)
{
   jjmatchedPos = jjnewStateCnt = 0;
   curLexState = defaultLexState;
   input_stream = stream;
   ReInitRounds();
}
private final void ReInitRounds()
{
   int i;
   jjround = 0x80000001;
   for (i = 16; i-- > 0;)
      jjrounds[i] = 0x80000000;
}
public void ReInit(SimpleCharStream stream, int lexState)
{
   ReInit(stream);
   SwitchTo(lexState);
}
public void SwitchTo(int lexState)
{
   if (lexState >= 1 || lexState < 0)
      throw new TokenMgrError("Error: Ignoring invalid lexical state : " + lexState + ". State unchanged.", TokenMgrError.INVALID_LEXICAL_STATE);
   else
      curLexState = lexState;
}

protected Token jjFillToken()
{
   Token t = Token.newToken(jjmatchedKind);
   t.kind = jjmatchedKind;
   String im = jjstrLiteralImages[jjmatchedKind];
   t.image = (im == null) ? input_stream.GetImage() : im;
   t.beginLine = input_stream.getBeginLine();
   t.beginColumn = input_stream.getBeginColumn();
   t.endLine = input_stream.getEndLine();
   t.endColumn = input_stream.getEndColumn();
   return t;
}

int curLexState = 0;
int defaultLexState = 0;
int jjnewStateCnt;
int jjround;
int jjmatchedPos;
int jjmatchedKind;

public Token getNextToken() 
{
  int kind;
  Token specialToken = null;
  Token matchedToken;
  int curPos = 0;

  EOFLoop :
  for (;;)
  {   
   try   
   {     
      curChar = input_stream.BeginToken();
   }     
   catch(java.io.IOException e)
   {        
      jjmatchedKind = 0;
      matchedToken = jjFillToken();
      return matchedToken;
   }

   jjmatchedKind = 0x7fffffff;
   jjmatchedPos = 0;
   curPos = jjMoveStringLiteralDfa0_0();
   if (jjmatchedKind != 0x7fffffff)
   {
      if (jjmatchedPos + 1 < curPos)
         input_stream.backup(curPos - jjmatchedPos - 1);
      if ((jjtoToken[jjmatchedKind >> 6] & (1L << (jjmatchedKind & 077))) != 0L)
      {
         matchedToken = jjFillToken();
         return matchedToken;
      }
      else
      {
         continue EOFLoop;
      }
   }
   int error_line = input_stream.getEndLine();
   int error_column = input_stream.getEndColumn();
   String error_after = null;
   boolean EOFSeen = false;
   try { input_stream.readChar(); input_stream.backup(1); }
   catch (java.io.IOException e1) {
      EOFSeen = true;
      error_after = curPos <= 1 ? "" : input_stream.GetImage();
      if (curChar == '\n' || curChar == '\r') {
         error_line++;
         error_column = 0;
      }
      else
         error_column++;
   }
   if (!EOFSeen) {
      input_stream.backup(1);
      error_after = curPos <= 1 ? "" : input_stream.GetImage();
   }
   throw new TokenMgrError(EOFSeen, curLexState, error_line, error_column, error_after, curChar, TokenMgrError.LEXICAL_ERROR);
  }
}

}
