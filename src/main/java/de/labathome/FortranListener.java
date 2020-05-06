package de.labathome;

import com.khubla.antlr.Fortran77ParserBaseListener;
import com.khubla.antlr.Fortran77Parser.FunctionSubprogramContext;

public class FortranListener extends Fortran77ParserBaseListener {
	
	
	@Override
	public void enterFunctionSubprogram(FunctionSubprogramContext ctx) {
		super.enterFunctionSubprogram(ctx);
		System.out.println("start of function subprogram: '"+ctx.getText()+"'");
	}
	
	@Override
	public void exitFunctionSubprogram(FunctionSubprogramContext ctx) {
		super.exitFunctionSubprogram(ctx);
		System.out.println("end of function subprogram: '"+ctx.getText()+"'");
	}
}
