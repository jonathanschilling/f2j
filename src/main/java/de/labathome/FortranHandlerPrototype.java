package de.labathome;

import java.io.IOException;
import java.io.InputStream;

import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.Lexer;
import org.antlr.v4.runtime.Parser;
import org.antlr.v4.runtime.TokenStream;

import com.khubla.antlr.Fortran77Lexer;
import com.khubla.antlr.Fortran77Parser;

public class FortranHandlerPrototype {

	public static void main(String[] args) {

		parseDdot();
	}

	public static void parseDdot() {

		try (InputStream ddotFile = FortranHandlerPrototype.class.getResourceAsStream("/ddot.f")) {

			/** make Lexer */
			Lexer lexer = new Fortran77Lexer(CharStreams.fromStream(ddotFile));

			/** get a TokenStream on the Lexer */
			TokenStream tokenStream = new CommonTokenStream(lexer);

			/** make a Parser on the token stream */
			Fortran77Parser parser = new Fortran77Parser(tokenStream);

			parser.addParseListener(new FortranListener());

			parser.program();
			
			
		} catch (IOException e) {
			e.printStackTrace();
		}

	}
}
