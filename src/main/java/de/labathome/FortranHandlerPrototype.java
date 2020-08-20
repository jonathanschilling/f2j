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

/**
 * <p>FortranHandlerPrototype class.</p>
 *
 * @author jonathan
 * @version $Id: $Id
 */
public class FortranHandlerPrototype {

	/**
	 * <p>main.</p>
	 *
	 * @param args an array of {@link java.lang.String} objects.
	 */
	public static void main(String[] args) {

		parseDdot();
	}

	/**
	 * <p>parseDdot.</p>
	 */
	public static void parseDdot() {

		try (InputStream ddotFile = FortranHandlerPrototype.class.getResourceAsStream("/ddot.f")) {

			/** make Lexer */
			Lexer lexer = new Fortran77Lexer(CharStreams.fromStream(ddotFile));

			/** get a TokenStream on the Lexer */
			TokenStream tokenStream = new CommonTokenStream(lexer);

			/** make a Parser on the token stream */
			Fortran77Parser parser = new Fortran77Parser(tokenStream);

			/** add a consumer for the Parser results */
			parser.addParseListener(new FortranListener());

			/** start traversing the AST */
			parser.program();
			
		} catch (IOException e) {
			e.printStackTrace();
		}

	}
}
