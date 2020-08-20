package de.labathome;

import com.khubla.antlr.Fortran77ParserBaseListener;
import com.khubla.antlr.Fortran77Parser.CommentStatementContext;
import com.khubla.antlr.Fortran77Parser.FunctionStatementContext;
import com.khubla.antlr.Fortran77Parser.FunctionSubprogramContext;
import com.khubla.antlr.Fortran77Parser.IdentifierContext;
import com.khubla.antlr.Fortran77Parser.IfStatementContext;
import com.khubla.antlr.Fortran77Parser.LogicalExpressionContext;

/**
 * Handle the identified parsing results and build Java code from them.
 * 
 * @author Jonathan Schilling (jonathan.schilling@mail.de)
 */
public class FortranListener extends Fortran77ParserBaseListener {
	
	/** {@inheritDoc} */
	@Override
	public void exitCommentStatement(CommentStatementContext ctx) {
		super.exitCommentStatement(ctx);
		
		String comment = ctx.getText();
		
		// assume that '*> \brief' indicates the start of the documentation prologue
		if (comment.startsWith("*> \\brief")) {
			
			String javadocComment = BlasComments.toJavadoc(comment);
			
			System.out.println(javadocComment);
			
		} else {
			System.out.println(BlasComments.toJavaComment(comment));
		}
		
		
	}
	
	@Override
	public void exitLogicalExpression(LogicalExpressionContext ctx) {
		super.exitLogicalExpression(ctx);
		
		// only if inside if logical expression...
		System.out.println(ctx.getText() + ") {");
	}
	
	
	@Override
	public void enterIfStatement(IfStatementContext ctx) {
		super.enterIfStatement(ctx);
		System.out.print("if (");
	}
	
	@Override
	public void exitIfStatement(IfStatementContext ctx) {
		super.exitIfStatement(ctx);

		System.out.println("} // if ...");
	}
	
	
	
	@Override
	public void exitFunctionStatement(FunctionStatementContext ctx) {
		super.exitFunctionStatement(ctx);
		
		
		System.out.println("exit function statement; name is '"+ctx.NAME().getText()+"'");
		
		int retTypeChildCount = ctx.type().typename().getChildCount();
		for (int i=0; i<retTypeChildCount; ++i) {
			System.out.printf(" return type [%d]: '%s'\n", i, ctx.type().typename().getChild(i).getText());
		}
		
		for (IdentifierContext id: ctx.namelist().identifier()) {
			System.out.println("  parameter: "+id.NAME().getText());
		}
		
	}
	
	
}
