package de.labathome;

import com.khubla.antlr.Fortran77ParserBaseListener;
import com.khubla.antlr.Fortran77Parser.CommentStatementContext;
import com.khubla.antlr.Fortran77Parser.FunctionSubprogramContext;

public class FortranListener extends Fortran77ParserBaseListener {
	
	@Override
	public void exitCommentStatement(CommentStatementContext ctx) {
		super.exitCommentStatement(ctx);
		System.out.println("exit commentStatement: '"+ctx.getText()+"'");
		
		
		
		
		
		
	}
	
	
}
