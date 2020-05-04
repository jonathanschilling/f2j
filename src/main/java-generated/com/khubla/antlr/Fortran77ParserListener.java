// Generated from com/khubla/antlr/Fortran77Parser.g4 by ANTLR 4.7.2
package com.khubla.antlr;
import org.antlr.v4.runtime.tree.ParseTreeListener;

/**
 * This interface defines a complete listener for a parse tree produced by
 * {@link Fortran77Parser}.
 */
public interface Fortran77ParserListener extends ParseTreeListener {
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#program}.
	 * @param ctx the parse tree
	 */
	void enterProgram(Fortran77Parser.ProgramContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#program}.
	 * @param ctx the parse tree
	 */
	void exitProgram(Fortran77Parser.ProgramContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#executableUnit}.
	 * @param ctx the parse tree
	 */
	void enterExecutableUnit(Fortran77Parser.ExecutableUnitContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#executableUnit}.
	 * @param ctx the parse tree
	 */
	void exitExecutableUnit(Fortran77Parser.ExecutableUnitContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#mainProgram}.
	 * @param ctx the parse tree
	 */
	void enterMainProgram(Fortran77Parser.MainProgramContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#mainProgram}.
	 * @param ctx the parse tree
	 */
	void exitMainProgram(Fortran77Parser.MainProgramContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#functionSubprogram}.
	 * @param ctx the parse tree
	 */
	void enterFunctionSubprogram(Fortran77Parser.FunctionSubprogramContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#functionSubprogram}.
	 * @param ctx the parse tree
	 */
	void exitFunctionSubprogram(Fortran77Parser.FunctionSubprogramContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#subroutineSubprogram}.
	 * @param ctx the parse tree
	 */
	void enterSubroutineSubprogram(Fortran77Parser.SubroutineSubprogramContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#subroutineSubprogram}.
	 * @param ctx the parse tree
	 */
	void exitSubroutineSubprogram(Fortran77Parser.SubroutineSubprogramContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#blockdataSubprogram}.
	 * @param ctx the parse tree
	 */
	void enterBlockdataSubprogram(Fortran77Parser.BlockdataSubprogramContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#blockdataSubprogram}.
	 * @param ctx the parse tree
	 */
	void exitBlockdataSubprogram(Fortran77Parser.BlockdataSubprogramContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#otherSpecificationStatement}.
	 * @param ctx the parse tree
	 */
	void enterOtherSpecificationStatement(Fortran77Parser.OtherSpecificationStatementContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#otherSpecificationStatement}.
	 * @param ctx the parse tree
	 */
	void exitOtherSpecificationStatement(Fortran77Parser.OtherSpecificationStatementContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#executableStatement}.
	 * @param ctx the parse tree
	 */
	void enterExecutableStatement(Fortran77Parser.ExecutableStatementContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#executableStatement}.
	 * @param ctx the parse tree
	 */
	void exitExecutableStatement(Fortran77Parser.ExecutableStatementContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#programStatement}.
	 * @param ctx the parse tree
	 */
	void enterProgramStatement(Fortran77Parser.ProgramStatementContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#programStatement}.
	 * @param ctx the parse tree
	 */
	void exitProgramStatement(Fortran77Parser.ProgramStatementContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#entryStatement}.
	 * @param ctx the parse tree
	 */
	void enterEntryStatement(Fortran77Parser.EntryStatementContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#entryStatement}.
	 * @param ctx the parse tree
	 */
	void exitEntryStatement(Fortran77Parser.EntryStatementContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#functionStatement}.
	 * @param ctx the parse tree
	 */
	void enterFunctionStatement(Fortran77Parser.FunctionStatementContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#functionStatement}.
	 * @param ctx the parse tree
	 */
	void exitFunctionStatement(Fortran77Parser.FunctionStatementContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#blockdataStatement}.
	 * @param ctx the parse tree
	 */
	void enterBlockdataStatement(Fortran77Parser.BlockdataStatementContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#blockdataStatement}.
	 * @param ctx the parse tree
	 */
	void exitBlockdataStatement(Fortran77Parser.BlockdataStatementContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#subroutineStatement}.
	 * @param ctx the parse tree
	 */
	void enterSubroutineStatement(Fortran77Parser.SubroutineStatementContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#subroutineStatement}.
	 * @param ctx the parse tree
	 */
	void exitSubroutineStatement(Fortran77Parser.SubroutineStatementContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#namelist}.
	 * @param ctx the parse tree
	 */
	void enterNamelist(Fortran77Parser.NamelistContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#namelist}.
	 * @param ctx the parse tree
	 */
	void exitNamelist(Fortran77Parser.NamelistContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#statement}.
	 * @param ctx the parse tree
	 */
	void enterStatement(Fortran77Parser.StatementContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#statement}.
	 * @param ctx the parse tree
	 */
	void exitStatement(Fortran77Parser.StatementContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#subprogramBody}.
	 * @param ctx the parse tree
	 */
	void enterSubprogramBody(Fortran77Parser.SubprogramBodyContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#subprogramBody}.
	 * @param ctx the parse tree
	 */
	void exitSubprogramBody(Fortran77Parser.SubprogramBodyContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#wholeStatement}.
	 * @param ctx the parse tree
	 */
	void enterWholeStatement(Fortran77Parser.WholeStatementContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#wholeStatement}.
	 * @param ctx the parse tree
	 */
	void exitWholeStatement(Fortran77Parser.WholeStatementContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#endStatement}.
	 * @param ctx the parse tree
	 */
	void enterEndStatement(Fortran77Parser.EndStatementContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#endStatement}.
	 * @param ctx the parse tree
	 */
	void exitEndStatement(Fortran77Parser.EndStatementContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#dimensionStatement}.
	 * @param ctx the parse tree
	 */
	void enterDimensionStatement(Fortran77Parser.DimensionStatementContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#dimensionStatement}.
	 * @param ctx the parse tree
	 */
	void exitDimensionStatement(Fortran77Parser.DimensionStatementContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#arrayDeclarator}.
	 * @param ctx the parse tree
	 */
	void enterArrayDeclarator(Fortran77Parser.ArrayDeclaratorContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#arrayDeclarator}.
	 * @param ctx the parse tree
	 */
	void exitArrayDeclarator(Fortran77Parser.ArrayDeclaratorContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#arrayDeclarators}.
	 * @param ctx the parse tree
	 */
	void enterArrayDeclarators(Fortran77Parser.ArrayDeclaratorsContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#arrayDeclarators}.
	 * @param ctx the parse tree
	 */
	void exitArrayDeclarators(Fortran77Parser.ArrayDeclaratorsContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#arrayDeclaratorExtents}.
	 * @param ctx the parse tree
	 */
	void enterArrayDeclaratorExtents(Fortran77Parser.ArrayDeclaratorExtentsContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#arrayDeclaratorExtents}.
	 * @param ctx the parse tree
	 */
	void exitArrayDeclaratorExtents(Fortran77Parser.ArrayDeclaratorExtentsContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#arrayDeclaratorExtent}.
	 * @param ctx the parse tree
	 */
	void enterArrayDeclaratorExtent(Fortran77Parser.ArrayDeclaratorExtentContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#arrayDeclaratorExtent}.
	 * @param ctx the parse tree
	 */
	void exitArrayDeclaratorExtent(Fortran77Parser.ArrayDeclaratorExtentContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#equivalenceStatement}.
	 * @param ctx the parse tree
	 */
	void enterEquivalenceStatement(Fortran77Parser.EquivalenceStatementContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#equivalenceStatement}.
	 * @param ctx the parse tree
	 */
	void exitEquivalenceStatement(Fortran77Parser.EquivalenceStatementContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#equivEntityGroup}.
	 * @param ctx the parse tree
	 */
	void enterEquivEntityGroup(Fortran77Parser.EquivEntityGroupContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#equivEntityGroup}.
	 * @param ctx the parse tree
	 */
	void exitEquivEntityGroup(Fortran77Parser.EquivEntityGroupContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#equivEntity}.
	 * @param ctx the parse tree
	 */
	void enterEquivEntity(Fortran77Parser.EquivEntityContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#equivEntity}.
	 * @param ctx the parse tree
	 */
	void exitEquivEntity(Fortran77Parser.EquivEntityContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#commonStatement}.
	 * @param ctx the parse tree
	 */
	void enterCommonStatement(Fortran77Parser.CommonStatementContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#commonStatement}.
	 * @param ctx the parse tree
	 */
	void exitCommonStatement(Fortran77Parser.CommonStatementContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#commonName}.
	 * @param ctx the parse tree
	 */
	void enterCommonName(Fortran77Parser.CommonNameContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#commonName}.
	 * @param ctx the parse tree
	 */
	void exitCommonName(Fortran77Parser.CommonNameContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#commonItem}.
	 * @param ctx the parse tree
	 */
	void enterCommonItem(Fortran77Parser.CommonItemContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#commonItem}.
	 * @param ctx the parse tree
	 */
	void exitCommonItem(Fortran77Parser.CommonItemContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#commonItems}.
	 * @param ctx the parse tree
	 */
	void enterCommonItems(Fortran77Parser.CommonItemsContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#commonItems}.
	 * @param ctx the parse tree
	 */
	void exitCommonItems(Fortran77Parser.CommonItemsContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#commonBlock}.
	 * @param ctx the parse tree
	 */
	void enterCommonBlock(Fortran77Parser.CommonBlockContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#commonBlock}.
	 * @param ctx the parse tree
	 */
	void exitCommonBlock(Fortran77Parser.CommonBlockContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#commentStatement}.
	 * @param ctx the parse tree
	 */
	void enterCommentStatement(Fortran77Parser.CommentStatementContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#commentStatement}.
	 * @param ctx the parse tree
	 */
	void exitCommentStatement(Fortran77Parser.CommentStatementContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#typeStatement}.
	 * @param ctx the parse tree
	 */
	void enterTypeStatement(Fortran77Parser.TypeStatementContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#typeStatement}.
	 * @param ctx the parse tree
	 */
	void exitTypeStatement(Fortran77Parser.TypeStatementContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#typeStatementNameList}.
	 * @param ctx the parse tree
	 */
	void enterTypeStatementNameList(Fortran77Parser.TypeStatementNameListContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#typeStatementNameList}.
	 * @param ctx the parse tree
	 */
	void exitTypeStatementNameList(Fortran77Parser.TypeStatementNameListContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#typeStatementName}.
	 * @param ctx the parse tree
	 */
	void enterTypeStatementName(Fortran77Parser.TypeStatementNameContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#typeStatementName}.
	 * @param ctx the parse tree
	 */
	void exitTypeStatementName(Fortran77Parser.TypeStatementNameContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#typeStatementNameCharList}.
	 * @param ctx the parse tree
	 */
	void enterTypeStatementNameCharList(Fortran77Parser.TypeStatementNameCharListContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#typeStatementNameCharList}.
	 * @param ctx the parse tree
	 */
	void exitTypeStatementNameCharList(Fortran77Parser.TypeStatementNameCharListContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#typeStatementNameChar}.
	 * @param ctx the parse tree
	 */
	void enterTypeStatementNameChar(Fortran77Parser.TypeStatementNameCharContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#typeStatementNameChar}.
	 * @param ctx the parse tree
	 */
	void exitTypeStatementNameChar(Fortran77Parser.TypeStatementNameCharContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#typeStatementLenSpec}.
	 * @param ctx the parse tree
	 */
	void enterTypeStatementLenSpec(Fortran77Parser.TypeStatementLenSpecContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#typeStatementLenSpec}.
	 * @param ctx the parse tree
	 */
	void exitTypeStatementLenSpec(Fortran77Parser.TypeStatementLenSpecContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#typename}.
	 * @param ctx the parse tree
	 */
	void enterTypename(Fortran77Parser.TypenameContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#typename}.
	 * @param ctx the parse tree
	 */
	void exitTypename(Fortran77Parser.TypenameContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#type}.
	 * @param ctx the parse tree
	 */
	void enterType(Fortran77Parser.TypeContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#type}.
	 * @param ctx the parse tree
	 */
	void exitType(Fortran77Parser.TypeContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#typenameLen}.
	 * @param ctx the parse tree
	 */
	void enterTypenameLen(Fortran77Parser.TypenameLenContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#typenameLen}.
	 * @param ctx the parse tree
	 */
	void exitTypenameLen(Fortran77Parser.TypenameLenContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#pointerStatement}.
	 * @param ctx the parse tree
	 */
	void enterPointerStatement(Fortran77Parser.PointerStatementContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#pointerStatement}.
	 * @param ctx the parse tree
	 */
	void exitPointerStatement(Fortran77Parser.PointerStatementContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#pointerDecl}.
	 * @param ctx the parse tree
	 */
	void enterPointerDecl(Fortran77Parser.PointerDeclContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#pointerDecl}.
	 * @param ctx the parse tree
	 */
	void exitPointerDecl(Fortran77Parser.PointerDeclContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#implicitStatement}.
	 * @param ctx the parse tree
	 */
	void enterImplicitStatement(Fortran77Parser.ImplicitStatementContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#implicitStatement}.
	 * @param ctx the parse tree
	 */
	void exitImplicitStatement(Fortran77Parser.ImplicitStatementContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#implicitSpec}.
	 * @param ctx the parse tree
	 */
	void enterImplicitSpec(Fortran77Parser.ImplicitSpecContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#implicitSpec}.
	 * @param ctx the parse tree
	 */
	void exitImplicitSpec(Fortran77Parser.ImplicitSpecContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#implicitSpecs}.
	 * @param ctx the parse tree
	 */
	void enterImplicitSpecs(Fortran77Parser.ImplicitSpecsContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#implicitSpecs}.
	 * @param ctx the parse tree
	 */
	void exitImplicitSpecs(Fortran77Parser.ImplicitSpecsContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#implicitNone}.
	 * @param ctx the parse tree
	 */
	void enterImplicitNone(Fortran77Parser.ImplicitNoneContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#implicitNone}.
	 * @param ctx the parse tree
	 */
	void exitImplicitNone(Fortran77Parser.ImplicitNoneContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#implicitLetter}.
	 * @param ctx the parse tree
	 */
	void enterImplicitLetter(Fortran77Parser.ImplicitLetterContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#implicitLetter}.
	 * @param ctx the parse tree
	 */
	void exitImplicitLetter(Fortran77Parser.ImplicitLetterContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#implicitRange}.
	 * @param ctx the parse tree
	 */
	void enterImplicitRange(Fortran77Parser.ImplicitRangeContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#implicitRange}.
	 * @param ctx the parse tree
	 */
	void exitImplicitRange(Fortran77Parser.ImplicitRangeContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#implicitLetters}.
	 * @param ctx the parse tree
	 */
	void enterImplicitLetters(Fortran77Parser.ImplicitLettersContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#implicitLetters}.
	 * @param ctx the parse tree
	 */
	void exitImplicitLetters(Fortran77Parser.ImplicitLettersContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#lenSpecification}.
	 * @param ctx the parse tree
	 */
	void enterLenSpecification(Fortran77Parser.LenSpecificationContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#lenSpecification}.
	 * @param ctx the parse tree
	 */
	void exitLenSpecification(Fortran77Parser.LenSpecificationContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#characterWithLen}.
	 * @param ctx the parse tree
	 */
	void enterCharacterWithLen(Fortran77Parser.CharacterWithLenContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#characterWithLen}.
	 * @param ctx the parse tree
	 */
	void exitCharacterWithLen(Fortran77Parser.CharacterWithLenContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#cwlLen}.
	 * @param ctx the parse tree
	 */
	void enterCwlLen(Fortran77Parser.CwlLenContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#cwlLen}.
	 * @param ctx the parse tree
	 */
	void exitCwlLen(Fortran77Parser.CwlLenContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#parameterStatement}.
	 * @param ctx the parse tree
	 */
	void enterParameterStatement(Fortran77Parser.ParameterStatementContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#parameterStatement}.
	 * @param ctx the parse tree
	 */
	void exitParameterStatement(Fortran77Parser.ParameterStatementContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#paramlist}.
	 * @param ctx the parse tree
	 */
	void enterParamlist(Fortran77Parser.ParamlistContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#paramlist}.
	 * @param ctx the parse tree
	 */
	void exitParamlist(Fortran77Parser.ParamlistContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#paramassign}.
	 * @param ctx the parse tree
	 */
	void enterParamassign(Fortran77Parser.ParamassignContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#paramassign}.
	 * @param ctx the parse tree
	 */
	void exitParamassign(Fortran77Parser.ParamassignContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#externalStatement}.
	 * @param ctx the parse tree
	 */
	void enterExternalStatement(Fortran77Parser.ExternalStatementContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#externalStatement}.
	 * @param ctx the parse tree
	 */
	void exitExternalStatement(Fortran77Parser.ExternalStatementContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#intrinsicStatement}.
	 * @param ctx the parse tree
	 */
	void enterIntrinsicStatement(Fortran77Parser.IntrinsicStatementContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#intrinsicStatement}.
	 * @param ctx the parse tree
	 */
	void exitIntrinsicStatement(Fortran77Parser.IntrinsicStatementContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#saveStatement}.
	 * @param ctx the parse tree
	 */
	void enterSaveStatement(Fortran77Parser.SaveStatementContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#saveStatement}.
	 * @param ctx the parse tree
	 */
	void exitSaveStatement(Fortran77Parser.SaveStatementContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#saveEntity}.
	 * @param ctx the parse tree
	 */
	void enterSaveEntity(Fortran77Parser.SaveEntityContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#saveEntity}.
	 * @param ctx the parse tree
	 */
	void exitSaveEntity(Fortran77Parser.SaveEntityContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#dataStatement}.
	 * @param ctx the parse tree
	 */
	void enterDataStatement(Fortran77Parser.DataStatementContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#dataStatement}.
	 * @param ctx the parse tree
	 */
	void exitDataStatement(Fortran77Parser.DataStatementContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#dataStatementItem}.
	 * @param ctx the parse tree
	 */
	void enterDataStatementItem(Fortran77Parser.DataStatementItemContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#dataStatementItem}.
	 * @param ctx the parse tree
	 */
	void exitDataStatementItem(Fortran77Parser.DataStatementItemContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#dataStatementMultiple}.
	 * @param ctx the parse tree
	 */
	void enterDataStatementMultiple(Fortran77Parser.DataStatementMultipleContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#dataStatementMultiple}.
	 * @param ctx the parse tree
	 */
	void exitDataStatementMultiple(Fortran77Parser.DataStatementMultipleContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#dataStatementEntity}.
	 * @param ctx the parse tree
	 */
	void enterDataStatementEntity(Fortran77Parser.DataStatementEntityContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#dataStatementEntity}.
	 * @param ctx the parse tree
	 */
	void exitDataStatementEntity(Fortran77Parser.DataStatementEntityContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#dse1}.
	 * @param ctx the parse tree
	 */
	void enterDse1(Fortran77Parser.Dse1Context ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#dse1}.
	 * @param ctx the parse tree
	 */
	void exitDse1(Fortran77Parser.Dse1Context ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#dse2}.
	 * @param ctx the parse tree
	 */
	void enterDse2(Fortran77Parser.Dse2Context ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#dse2}.
	 * @param ctx the parse tree
	 */
	void exitDse2(Fortran77Parser.Dse2Context ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#dataImpliedDo}.
	 * @param ctx the parse tree
	 */
	void enterDataImpliedDo(Fortran77Parser.DataImpliedDoContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#dataImpliedDo}.
	 * @param ctx the parse tree
	 */
	void exitDataImpliedDo(Fortran77Parser.DataImpliedDoContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#dataImpliedDoRange}.
	 * @param ctx the parse tree
	 */
	void enterDataImpliedDoRange(Fortran77Parser.DataImpliedDoRangeContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#dataImpliedDoRange}.
	 * @param ctx the parse tree
	 */
	void exitDataImpliedDoRange(Fortran77Parser.DataImpliedDoRangeContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#dataImpliedDoList}.
	 * @param ctx the parse tree
	 */
	void enterDataImpliedDoList(Fortran77Parser.DataImpliedDoListContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#dataImpliedDoList}.
	 * @param ctx the parse tree
	 */
	void exitDataImpliedDoList(Fortran77Parser.DataImpliedDoListContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#dataImpliedDoListWhat}.
	 * @param ctx the parse tree
	 */
	void enterDataImpliedDoListWhat(Fortran77Parser.DataImpliedDoListWhatContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#dataImpliedDoListWhat}.
	 * @param ctx the parse tree
	 */
	void exitDataImpliedDoListWhat(Fortran77Parser.DataImpliedDoListWhatContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#gotoStatement}.
	 * @param ctx the parse tree
	 */
	void enterGotoStatement(Fortran77Parser.GotoStatementContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#gotoStatement}.
	 * @param ctx the parse tree
	 */
	void exitGotoStatement(Fortran77Parser.GotoStatementContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#unconditionalGoto}.
	 * @param ctx the parse tree
	 */
	void enterUnconditionalGoto(Fortran77Parser.UnconditionalGotoContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#unconditionalGoto}.
	 * @param ctx the parse tree
	 */
	void exitUnconditionalGoto(Fortran77Parser.UnconditionalGotoContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#computedGoto}.
	 * @param ctx the parse tree
	 */
	void enterComputedGoto(Fortran77Parser.ComputedGotoContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#computedGoto}.
	 * @param ctx the parse tree
	 */
	void exitComputedGoto(Fortran77Parser.ComputedGotoContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#lblRef}.
	 * @param ctx the parse tree
	 */
	void enterLblRef(Fortran77Parser.LblRefContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#lblRef}.
	 * @param ctx the parse tree
	 */
	void exitLblRef(Fortran77Parser.LblRefContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#labelList}.
	 * @param ctx the parse tree
	 */
	void enterLabelList(Fortran77Parser.LabelListContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#labelList}.
	 * @param ctx the parse tree
	 */
	void exitLabelList(Fortran77Parser.LabelListContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#assignedGoto}.
	 * @param ctx the parse tree
	 */
	void enterAssignedGoto(Fortran77Parser.AssignedGotoContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#assignedGoto}.
	 * @param ctx the parse tree
	 */
	void exitAssignedGoto(Fortran77Parser.AssignedGotoContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#ifStatement}.
	 * @param ctx the parse tree
	 */
	void enterIfStatement(Fortran77Parser.IfStatementContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#ifStatement}.
	 * @param ctx the parse tree
	 */
	void exitIfStatement(Fortran77Parser.IfStatementContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#arithmeticIfStatement}.
	 * @param ctx the parse tree
	 */
	void enterArithmeticIfStatement(Fortran77Parser.ArithmeticIfStatementContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#arithmeticIfStatement}.
	 * @param ctx the parse tree
	 */
	void exitArithmeticIfStatement(Fortran77Parser.ArithmeticIfStatementContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#logicalIfStatement}.
	 * @param ctx the parse tree
	 */
	void enterLogicalIfStatement(Fortran77Parser.LogicalIfStatementContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#logicalIfStatement}.
	 * @param ctx the parse tree
	 */
	void exitLogicalIfStatement(Fortran77Parser.LogicalIfStatementContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#blockIfStatement}.
	 * @param ctx the parse tree
	 */
	void enterBlockIfStatement(Fortran77Parser.BlockIfStatementContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#blockIfStatement}.
	 * @param ctx the parse tree
	 */
	void exitBlockIfStatement(Fortran77Parser.BlockIfStatementContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#firstIfBlock}.
	 * @param ctx the parse tree
	 */
	void enterFirstIfBlock(Fortran77Parser.FirstIfBlockContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#firstIfBlock}.
	 * @param ctx the parse tree
	 */
	void exitFirstIfBlock(Fortran77Parser.FirstIfBlockContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#elseIfStatement}.
	 * @param ctx the parse tree
	 */
	void enterElseIfStatement(Fortran77Parser.ElseIfStatementContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#elseIfStatement}.
	 * @param ctx the parse tree
	 */
	void exitElseIfStatement(Fortran77Parser.ElseIfStatementContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#elseStatement}.
	 * @param ctx the parse tree
	 */
	void enterElseStatement(Fortran77Parser.ElseStatementContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#elseStatement}.
	 * @param ctx the parse tree
	 */
	void exitElseStatement(Fortran77Parser.ElseStatementContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#endIfStatement}.
	 * @param ctx the parse tree
	 */
	void enterEndIfStatement(Fortran77Parser.EndIfStatementContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#endIfStatement}.
	 * @param ctx the parse tree
	 */
	void exitEndIfStatement(Fortran77Parser.EndIfStatementContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#doStatement}.
	 * @param ctx the parse tree
	 */
	void enterDoStatement(Fortran77Parser.DoStatementContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#doStatement}.
	 * @param ctx the parse tree
	 */
	void exitDoStatement(Fortran77Parser.DoStatementContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#doVarArgs}.
	 * @param ctx the parse tree
	 */
	void enterDoVarArgs(Fortran77Parser.DoVarArgsContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#doVarArgs}.
	 * @param ctx the parse tree
	 */
	void exitDoVarArgs(Fortran77Parser.DoVarArgsContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#doWithLabel}.
	 * @param ctx the parse tree
	 */
	void enterDoWithLabel(Fortran77Parser.DoWithLabelContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#doWithLabel}.
	 * @param ctx the parse tree
	 */
	void exitDoWithLabel(Fortran77Parser.DoWithLabelContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#doBody}.
	 * @param ctx the parse tree
	 */
	void enterDoBody(Fortran77Parser.DoBodyContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#doBody}.
	 * @param ctx the parse tree
	 */
	void exitDoBody(Fortran77Parser.DoBodyContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#doWithEndDo}.
	 * @param ctx the parse tree
	 */
	void enterDoWithEndDo(Fortran77Parser.DoWithEndDoContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#doWithEndDo}.
	 * @param ctx the parse tree
	 */
	void exitDoWithEndDo(Fortran77Parser.DoWithEndDoContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#enddoStatement}.
	 * @param ctx the parse tree
	 */
	void enterEnddoStatement(Fortran77Parser.EnddoStatementContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#enddoStatement}.
	 * @param ctx the parse tree
	 */
	void exitEnddoStatement(Fortran77Parser.EnddoStatementContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#continueStatement}.
	 * @param ctx the parse tree
	 */
	void enterContinueStatement(Fortran77Parser.ContinueStatementContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#continueStatement}.
	 * @param ctx the parse tree
	 */
	void exitContinueStatement(Fortran77Parser.ContinueStatementContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#stopStatement}.
	 * @param ctx the parse tree
	 */
	void enterStopStatement(Fortran77Parser.StopStatementContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#stopStatement}.
	 * @param ctx the parse tree
	 */
	void exitStopStatement(Fortran77Parser.StopStatementContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#pauseStatement}.
	 * @param ctx the parse tree
	 */
	void enterPauseStatement(Fortran77Parser.PauseStatementContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#pauseStatement}.
	 * @param ctx the parse tree
	 */
	void exitPauseStatement(Fortran77Parser.PauseStatementContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#writeStatement}.
	 * @param ctx the parse tree
	 */
	void enterWriteStatement(Fortran77Parser.WriteStatementContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#writeStatement}.
	 * @param ctx the parse tree
	 */
	void exitWriteStatement(Fortran77Parser.WriteStatementContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#readStatement}.
	 * @param ctx the parse tree
	 */
	void enterReadStatement(Fortran77Parser.ReadStatementContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#readStatement}.
	 * @param ctx the parse tree
	 */
	void exitReadStatement(Fortran77Parser.ReadStatementContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#printStatement}.
	 * @param ctx the parse tree
	 */
	void enterPrintStatement(Fortran77Parser.PrintStatementContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#printStatement}.
	 * @param ctx the parse tree
	 */
	void exitPrintStatement(Fortran77Parser.PrintStatementContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#assignmentStatement}.
	 * @param ctx the parse tree
	 */
	void enterAssignmentStatement(Fortran77Parser.AssignmentStatementContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#assignmentStatement}.
	 * @param ctx the parse tree
	 */
	void exitAssignmentStatement(Fortran77Parser.AssignmentStatementContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#controlInfoList}.
	 * @param ctx the parse tree
	 */
	void enterControlInfoList(Fortran77Parser.ControlInfoListContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#controlInfoList}.
	 * @param ctx the parse tree
	 */
	void exitControlInfoList(Fortran77Parser.ControlInfoListContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#controlErrSpec}.
	 * @param ctx the parse tree
	 */
	void enterControlErrSpec(Fortran77Parser.ControlErrSpecContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#controlErrSpec}.
	 * @param ctx the parse tree
	 */
	void exitControlErrSpec(Fortran77Parser.ControlErrSpecContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#controlInfoListItem}.
	 * @param ctx the parse tree
	 */
	void enterControlInfoListItem(Fortran77Parser.ControlInfoListItemContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#controlInfoListItem}.
	 * @param ctx the parse tree
	 */
	void exitControlInfoListItem(Fortran77Parser.ControlInfoListItemContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#ioList}.
	 * @param ctx the parse tree
	 */
	void enterIoList(Fortran77Parser.IoListContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#ioList}.
	 * @param ctx the parse tree
	 */
	void exitIoList(Fortran77Parser.IoListContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#ioListItem}.
	 * @param ctx the parse tree
	 */
	void enterIoListItem(Fortran77Parser.IoListItemContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#ioListItem}.
	 * @param ctx the parse tree
	 */
	void exitIoListItem(Fortran77Parser.IoListItemContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#ioImpliedDoList}.
	 * @param ctx the parse tree
	 */
	void enterIoImpliedDoList(Fortran77Parser.IoImpliedDoListContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#ioImpliedDoList}.
	 * @param ctx the parse tree
	 */
	void exitIoImpliedDoList(Fortran77Parser.IoImpliedDoListContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#openStatement}.
	 * @param ctx the parse tree
	 */
	void enterOpenStatement(Fortran77Parser.OpenStatementContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#openStatement}.
	 * @param ctx the parse tree
	 */
	void exitOpenStatement(Fortran77Parser.OpenStatementContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#openControl}.
	 * @param ctx the parse tree
	 */
	void enterOpenControl(Fortran77Parser.OpenControlContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#openControl}.
	 * @param ctx the parse tree
	 */
	void exitOpenControl(Fortran77Parser.OpenControlContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#controlFmt}.
	 * @param ctx the parse tree
	 */
	void enterControlFmt(Fortran77Parser.ControlFmtContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#controlFmt}.
	 * @param ctx the parse tree
	 */
	void exitControlFmt(Fortran77Parser.ControlFmtContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#controlUnit}.
	 * @param ctx the parse tree
	 */
	void enterControlUnit(Fortran77Parser.ControlUnitContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#controlUnit}.
	 * @param ctx the parse tree
	 */
	void exitControlUnit(Fortran77Parser.ControlUnitContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#controlRec}.
	 * @param ctx the parse tree
	 */
	void enterControlRec(Fortran77Parser.ControlRecContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#controlRec}.
	 * @param ctx the parse tree
	 */
	void exitControlRec(Fortran77Parser.ControlRecContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#controlEnd}.
	 * @param ctx the parse tree
	 */
	void enterControlEnd(Fortran77Parser.ControlEndContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#controlEnd}.
	 * @param ctx the parse tree
	 */
	void exitControlEnd(Fortran77Parser.ControlEndContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#controlErr}.
	 * @param ctx the parse tree
	 */
	void enterControlErr(Fortran77Parser.ControlErrContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#controlErr}.
	 * @param ctx the parse tree
	 */
	void exitControlErr(Fortran77Parser.ControlErrContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#controlIostat}.
	 * @param ctx the parse tree
	 */
	void enterControlIostat(Fortran77Parser.ControlIostatContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#controlIostat}.
	 * @param ctx the parse tree
	 */
	void exitControlIostat(Fortran77Parser.ControlIostatContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#controlFile}.
	 * @param ctx the parse tree
	 */
	void enterControlFile(Fortran77Parser.ControlFileContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#controlFile}.
	 * @param ctx the parse tree
	 */
	void exitControlFile(Fortran77Parser.ControlFileContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#controlStatus}.
	 * @param ctx the parse tree
	 */
	void enterControlStatus(Fortran77Parser.ControlStatusContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#controlStatus}.
	 * @param ctx the parse tree
	 */
	void exitControlStatus(Fortran77Parser.ControlStatusContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#controlAccess}.
	 * @param ctx the parse tree
	 */
	void enterControlAccess(Fortran77Parser.ControlAccessContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#controlAccess}.
	 * @param ctx the parse tree
	 */
	void exitControlAccess(Fortran77Parser.ControlAccessContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#controlPosition}.
	 * @param ctx the parse tree
	 */
	void enterControlPosition(Fortran77Parser.ControlPositionContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#controlPosition}.
	 * @param ctx the parse tree
	 */
	void exitControlPosition(Fortran77Parser.ControlPositionContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#controlForm}.
	 * @param ctx the parse tree
	 */
	void enterControlForm(Fortran77Parser.ControlFormContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#controlForm}.
	 * @param ctx the parse tree
	 */
	void exitControlForm(Fortran77Parser.ControlFormContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#controlRecl}.
	 * @param ctx the parse tree
	 */
	void enterControlRecl(Fortran77Parser.ControlReclContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#controlRecl}.
	 * @param ctx the parse tree
	 */
	void exitControlRecl(Fortran77Parser.ControlReclContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#controlBlank}.
	 * @param ctx the parse tree
	 */
	void enterControlBlank(Fortran77Parser.ControlBlankContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#controlBlank}.
	 * @param ctx the parse tree
	 */
	void exitControlBlank(Fortran77Parser.ControlBlankContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#controlExist}.
	 * @param ctx the parse tree
	 */
	void enterControlExist(Fortran77Parser.ControlExistContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#controlExist}.
	 * @param ctx the parse tree
	 */
	void exitControlExist(Fortran77Parser.ControlExistContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#controlOpened}.
	 * @param ctx the parse tree
	 */
	void enterControlOpened(Fortran77Parser.ControlOpenedContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#controlOpened}.
	 * @param ctx the parse tree
	 */
	void exitControlOpened(Fortran77Parser.ControlOpenedContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#controlNumber}.
	 * @param ctx the parse tree
	 */
	void enterControlNumber(Fortran77Parser.ControlNumberContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#controlNumber}.
	 * @param ctx the parse tree
	 */
	void exitControlNumber(Fortran77Parser.ControlNumberContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#controlNamed}.
	 * @param ctx the parse tree
	 */
	void enterControlNamed(Fortran77Parser.ControlNamedContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#controlNamed}.
	 * @param ctx the parse tree
	 */
	void exitControlNamed(Fortran77Parser.ControlNamedContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#controlName}.
	 * @param ctx the parse tree
	 */
	void enterControlName(Fortran77Parser.ControlNameContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#controlName}.
	 * @param ctx the parse tree
	 */
	void exitControlName(Fortran77Parser.ControlNameContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#controlSequential}.
	 * @param ctx the parse tree
	 */
	void enterControlSequential(Fortran77Parser.ControlSequentialContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#controlSequential}.
	 * @param ctx the parse tree
	 */
	void exitControlSequential(Fortran77Parser.ControlSequentialContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#controlDirect}.
	 * @param ctx the parse tree
	 */
	void enterControlDirect(Fortran77Parser.ControlDirectContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#controlDirect}.
	 * @param ctx the parse tree
	 */
	void exitControlDirect(Fortran77Parser.ControlDirectContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#controlFormatted}.
	 * @param ctx the parse tree
	 */
	void enterControlFormatted(Fortran77Parser.ControlFormattedContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#controlFormatted}.
	 * @param ctx the parse tree
	 */
	void exitControlFormatted(Fortran77Parser.ControlFormattedContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#controlUnformatted}.
	 * @param ctx the parse tree
	 */
	void enterControlUnformatted(Fortran77Parser.ControlUnformattedContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#controlUnformatted}.
	 * @param ctx the parse tree
	 */
	void exitControlUnformatted(Fortran77Parser.ControlUnformattedContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#controlNextrec}.
	 * @param ctx the parse tree
	 */
	void enterControlNextrec(Fortran77Parser.ControlNextrecContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#controlNextrec}.
	 * @param ctx the parse tree
	 */
	void exitControlNextrec(Fortran77Parser.ControlNextrecContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#closeStatement}.
	 * @param ctx the parse tree
	 */
	void enterCloseStatement(Fortran77Parser.CloseStatementContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#closeStatement}.
	 * @param ctx the parse tree
	 */
	void exitCloseStatement(Fortran77Parser.CloseStatementContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#closeControl}.
	 * @param ctx the parse tree
	 */
	void enterCloseControl(Fortran77Parser.CloseControlContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#closeControl}.
	 * @param ctx the parse tree
	 */
	void exitCloseControl(Fortran77Parser.CloseControlContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#inquireStatement}.
	 * @param ctx the parse tree
	 */
	void enterInquireStatement(Fortran77Parser.InquireStatementContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#inquireStatement}.
	 * @param ctx the parse tree
	 */
	void exitInquireStatement(Fortran77Parser.InquireStatementContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#inquireControl}.
	 * @param ctx the parse tree
	 */
	void enterInquireControl(Fortran77Parser.InquireControlContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#inquireControl}.
	 * @param ctx the parse tree
	 */
	void exitInquireControl(Fortran77Parser.InquireControlContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#backspaceStatement}.
	 * @param ctx the parse tree
	 */
	void enterBackspaceStatement(Fortran77Parser.BackspaceStatementContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#backspaceStatement}.
	 * @param ctx the parse tree
	 */
	void exitBackspaceStatement(Fortran77Parser.BackspaceStatementContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#endfileStatement}.
	 * @param ctx the parse tree
	 */
	void enterEndfileStatement(Fortran77Parser.EndfileStatementContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#endfileStatement}.
	 * @param ctx the parse tree
	 */
	void exitEndfileStatement(Fortran77Parser.EndfileStatementContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#rewindStatement}.
	 * @param ctx the parse tree
	 */
	void enterRewindStatement(Fortran77Parser.RewindStatementContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#rewindStatement}.
	 * @param ctx the parse tree
	 */
	void exitRewindStatement(Fortran77Parser.RewindStatementContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#berFinish}.
	 * @param ctx the parse tree
	 */
	void enterBerFinish(Fortran77Parser.BerFinishContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#berFinish}.
	 * @param ctx the parse tree
	 */
	void exitBerFinish(Fortran77Parser.BerFinishContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#berFinishItem}.
	 * @param ctx the parse tree
	 */
	void enterBerFinishItem(Fortran77Parser.BerFinishItemContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#berFinishItem}.
	 * @param ctx the parse tree
	 */
	void exitBerFinishItem(Fortran77Parser.BerFinishItemContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#unitIdentifier}.
	 * @param ctx the parse tree
	 */
	void enterUnitIdentifier(Fortran77Parser.UnitIdentifierContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#unitIdentifier}.
	 * @param ctx the parse tree
	 */
	void exitUnitIdentifier(Fortran77Parser.UnitIdentifierContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#formatIdentifier}.
	 * @param ctx the parse tree
	 */
	void enterFormatIdentifier(Fortran77Parser.FormatIdentifierContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#formatIdentifier}.
	 * @param ctx the parse tree
	 */
	void exitFormatIdentifier(Fortran77Parser.FormatIdentifierContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#formatStatement}.
	 * @param ctx the parse tree
	 */
	void enterFormatStatement(Fortran77Parser.FormatStatementContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#formatStatement}.
	 * @param ctx the parse tree
	 */
	void exitFormatStatement(Fortran77Parser.FormatStatementContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#fmtSpec}.
	 * @param ctx the parse tree
	 */
	void enterFmtSpec(Fortran77Parser.FmtSpecContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#fmtSpec}.
	 * @param ctx the parse tree
	 */
	void exitFmtSpec(Fortran77Parser.FmtSpecContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#formatsep}.
	 * @param ctx the parse tree
	 */
	void enterFormatsep(Fortran77Parser.FormatsepContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#formatsep}.
	 * @param ctx the parse tree
	 */
	void exitFormatsep(Fortran77Parser.FormatsepContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#formatedit}.
	 * @param ctx the parse tree
	 */
	void enterFormatedit(Fortran77Parser.FormateditContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#formatedit}.
	 * @param ctx the parse tree
	 */
	void exitFormatedit(Fortran77Parser.FormateditContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#editElement}.
	 * @param ctx the parse tree
	 */
	void enterEditElement(Fortran77Parser.EditElementContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#editElement}.
	 * @param ctx the parse tree
	 */
	void exitEditElement(Fortran77Parser.EditElementContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#statementFunctionStatement}.
	 * @param ctx the parse tree
	 */
	void enterStatementFunctionStatement(Fortran77Parser.StatementFunctionStatementContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#statementFunctionStatement}.
	 * @param ctx the parse tree
	 */
	void exitStatementFunctionStatement(Fortran77Parser.StatementFunctionStatementContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#sfArgs}.
	 * @param ctx the parse tree
	 */
	void enterSfArgs(Fortran77Parser.SfArgsContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#sfArgs}.
	 * @param ctx the parse tree
	 */
	void exitSfArgs(Fortran77Parser.SfArgsContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#callStatement}.
	 * @param ctx the parse tree
	 */
	void enterCallStatement(Fortran77Parser.CallStatementContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#callStatement}.
	 * @param ctx the parse tree
	 */
	void exitCallStatement(Fortran77Parser.CallStatementContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#subroutineCall}.
	 * @param ctx the parse tree
	 */
	void enterSubroutineCall(Fortran77Parser.SubroutineCallContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#subroutineCall}.
	 * @param ctx the parse tree
	 */
	void exitSubroutineCall(Fortran77Parser.SubroutineCallContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#callArgumentList}.
	 * @param ctx the parse tree
	 */
	void enterCallArgumentList(Fortran77Parser.CallArgumentListContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#callArgumentList}.
	 * @param ctx the parse tree
	 */
	void exitCallArgumentList(Fortran77Parser.CallArgumentListContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#callArgument}.
	 * @param ctx the parse tree
	 */
	void enterCallArgument(Fortran77Parser.CallArgumentContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#callArgument}.
	 * @param ctx the parse tree
	 */
	void exitCallArgument(Fortran77Parser.CallArgumentContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#returnStatement}.
	 * @param ctx the parse tree
	 */
	void enterReturnStatement(Fortran77Parser.ReturnStatementContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#returnStatement}.
	 * @param ctx the parse tree
	 */
	void exitReturnStatement(Fortran77Parser.ReturnStatementContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#expression}.
	 * @param ctx the parse tree
	 */
	void enterExpression(Fortran77Parser.ExpressionContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#expression}.
	 * @param ctx the parse tree
	 */
	void exitExpression(Fortran77Parser.ExpressionContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#ncExpr}.
	 * @param ctx the parse tree
	 */
	void enterNcExpr(Fortran77Parser.NcExprContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#ncExpr}.
	 * @param ctx the parse tree
	 */
	void exitNcExpr(Fortran77Parser.NcExprContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#lexpr0}.
	 * @param ctx the parse tree
	 */
	void enterLexpr0(Fortran77Parser.Lexpr0Context ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#lexpr0}.
	 * @param ctx the parse tree
	 */
	void exitLexpr0(Fortran77Parser.Lexpr0Context ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#lexpr1}.
	 * @param ctx the parse tree
	 */
	void enterLexpr1(Fortran77Parser.Lexpr1Context ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#lexpr1}.
	 * @param ctx the parse tree
	 */
	void exitLexpr1(Fortran77Parser.Lexpr1Context ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#lexpr2}.
	 * @param ctx the parse tree
	 */
	void enterLexpr2(Fortran77Parser.Lexpr2Context ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#lexpr2}.
	 * @param ctx the parse tree
	 */
	void exitLexpr2(Fortran77Parser.Lexpr2Context ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#lexpr3}.
	 * @param ctx the parse tree
	 */
	void enterLexpr3(Fortran77Parser.Lexpr3Context ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#lexpr3}.
	 * @param ctx the parse tree
	 */
	void exitLexpr3(Fortran77Parser.Lexpr3Context ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#lexpr4}.
	 * @param ctx the parse tree
	 */
	void enterLexpr4(Fortran77Parser.Lexpr4Context ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#lexpr4}.
	 * @param ctx the parse tree
	 */
	void exitLexpr4(Fortran77Parser.Lexpr4Context ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#aexpr0}.
	 * @param ctx the parse tree
	 */
	void enterAexpr0(Fortran77Parser.Aexpr0Context ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#aexpr0}.
	 * @param ctx the parse tree
	 */
	void exitAexpr0(Fortran77Parser.Aexpr0Context ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#aexpr1}.
	 * @param ctx the parse tree
	 */
	void enterAexpr1(Fortran77Parser.Aexpr1Context ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#aexpr1}.
	 * @param ctx the parse tree
	 */
	void exitAexpr1(Fortran77Parser.Aexpr1Context ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#aexpr2}.
	 * @param ctx the parse tree
	 */
	void enterAexpr2(Fortran77Parser.Aexpr2Context ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#aexpr2}.
	 * @param ctx the parse tree
	 */
	void exitAexpr2(Fortran77Parser.Aexpr2Context ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#aexpr3}.
	 * @param ctx the parse tree
	 */
	void enterAexpr3(Fortran77Parser.Aexpr3Context ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#aexpr3}.
	 * @param ctx the parse tree
	 */
	void exitAexpr3(Fortran77Parser.Aexpr3Context ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#aexpr4}.
	 * @param ctx the parse tree
	 */
	void enterAexpr4(Fortran77Parser.Aexpr4Context ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#aexpr4}.
	 * @param ctx the parse tree
	 */
	void exitAexpr4(Fortran77Parser.Aexpr4Context ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#iexpr}.
	 * @param ctx the parse tree
	 */
	void enterIexpr(Fortran77Parser.IexprContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#iexpr}.
	 * @param ctx the parse tree
	 */
	void exitIexpr(Fortran77Parser.IexprContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#iexprCode}.
	 * @param ctx the parse tree
	 */
	void enterIexprCode(Fortran77Parser.IexprCodeContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#iexprCode}.
	 * @param ctx the parse tree
	 */
	void exitIexprCode(Fortran77Parser.IexprCodeContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#iexpr1}.
	 * @param ctx the parse tree
	 */
	void enterIexpr1(Fortran77Parser.Iexpr1Context ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#iexpr1}.
	 * @param ctx the parse tree
	 */
	void exitIexpr1(Fortran77Parser.Iexpr1Context ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#iexpr2}.
	 * @param ctx the parse tree
	 */
	void enterIexpr2(Fortran77Parser.Iexpr2Context ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#iexpr2}.
	 * @param ctx the parse tree
	 */
	void exitIexpr2(Fortran77Parser.Iexpr2Context ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#iexpr3}.
	 * @param ctx the parse tree
	 */
	void enterIexpr3(Fortran77Parser.Iexpr3Context ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#iexpr3}.
	 * @param ctx the parse tree
	 */
	void exitIexpr3(Fortran77Parser.Iexpr3Context ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#iexpr4}.
	 * @param ctx the parse tree
	 */
	void enterIexpr4(Fortran77Parser.Iexpr4Context ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#iexpr4}.
	 * @param ctx the parse tree
	 */
	void exitIexpr4(Fortran77Parser.Iexpr4Context ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#constantExpr}.
	 * @param ctx the parse tree
	 */
	void enterConstantExpr(Fortran77Parser.ConstantExprContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#constantExpr}.
	 * @param ctx the parse tree
	 */
	void exitConstantExpr(Fortran77Parser.ConstantExprContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#arithmeticExpression}.
	 * @param ctx the parse tree
	 */
	void enterArithmeticExpression(Fortran77Parser.ArithmeticExpressionContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#arithmeticExpression}.
	 * @param ctx the parse tree
	 */
	void exitArithmeticExpression(Fortran77Parser.ArithmeticExpressionContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#integerExpr}.
	 * @param ctx the parse tree
	 */
	void enterIntegerExpr(Fortran77Parser.IntegerExprContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#integerExpr}.
	 * @param ctx the parse tree
	 */
	void exitIntegerExpr(Fortran77Parser.IntegerExprContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#intRealDpExpr}.
	 * @param ctx the parse tree
	 */
	void enterIntRealDpExpr(Fortran77Parser.IntRealDpExprContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#intRealDpExpr}.
	 * @param ctx the parse tree
	 */
	void exitIntRealDpExpr(Fortran77Parser.IntRealDpExprContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#arithmeticConstExpr}.
	 * @param ctx the parse tree
	 */
	void enterArithmeticConstExpr(Fortran77Parser.ArithmeticConstExprContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#arithmeticConstExpr}.
	 * @param ctx the parse tree
	 */
	void exitArithmeticConstExpr(Fortran77Parser.ArithmeticConstExprContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#intConstantExpr}.
	 * @param ctx the parse tree
	 */
	void enterIntConstantExpr(Fortran77Parser.IntConstantExprContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#intConstantExpr}.
	 * @param ctx the parse tree
	 */
	void exitIntConstantExpr(Fortran77Parser.IntConstantExprContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#characterExpression}.
	 * @param ctx the parse tree
	 */
	void enterCharacterExpression(Fortran77Parser.CharacterExpressionContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#characterExpression}.
	 * @param ctx the parse tree
	 */
	void exitCharacterExpression(Fortran77Parser.CharacterExpressionContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#concatOp}.
	 * @param ctx the parse tree
	 */
	void enterConcatOp(Fortran77Parser.ConcatOpContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#concatOp}.
	 * @param ctx the parse tree
	 */
	void exitConcatOp(Fortran77Parser.ConcatOpContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#logicalExpression}.
	 * @param ctx the parse tree
	 */
	void enterLogicalExpression(Fortran77Parser.LogicalExpressionContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#logicalExpression}.
	 * @param ctx the parse tree
	 */
	void exitLogicalExpression(Fortran77Parser.LogicalExpressionContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#logicalConstExpr}.
	 * @param ctx the parse tree
	 */
	void enterLogicalConstExpr(Fortran77Parser.LogicalConstExprContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#logicalConstExpr}.
	 * @param ctx the parse tree
	 */
	void exitLogicalConstExpr(Fortran77Parser.LogicalConstExprContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#arrayElementName}.
	 * @param ctx the parse tree
	 */
	void enterArrayElementName(Fortran77Parser.ArrayElementNameContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#arrayElementName}.
	 * @param ctx the parse tree
	 */
	void exitArrayElementName(Fortran77Parser.ArrayElementNameContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#subscripts}.
	 * @param ctx the parse tree
	 */
	void enterSubscripts(Fortran77Parser.SubscriptsContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#subscripts}.
	 * @param ctx the parse tree
	 */
	void exitSubscripts(Fortran77Parser.SubscriptsContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#varRef}.
	 * @param ctx the parse tree
	 */
	void enterVarRef(Fortran77Parser.VarRefContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#varRef}.
	 * @param ctx the parse tree
	 */
	void exitVarRef(Fortran77Parser.VarRefContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#varRefCode}.
	 * @param ctx the parse tree
	 */
	void enterVarRefCode(Fortran77Parser.VarRefCodeContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#varRefCode}.
	 * @param ctx the parse tree
	 */
	void exitVarRefCode(Fortran77Parser.VarRefCodeContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#substringApp}.
	 * @param ctx the parse tree
	 */
	void enterSubstringApp(Fortran77Parser.SubstringAppContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#substringApp}.
	 * @param ctx the parse tree
	 */
	void exitSubstringApp(Fortran77Parser.SubstringAppContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#variableName}.
	 * @param ctx the parse tree
	 */
	void enterVariableName(Fortran77Parser.VariableNameContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#variableName}.
	 * @param ctx the parse tree
	 */
	void exitVariableName(Fortran77Parser.VariableNameContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#arrayName}.
	 * @param ctx the parse tree
	 */
	void enterArrayName(Fortran77Parser.ArrayNameContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#arrayName}.
	 * @param ctx the parse tree
	 */
	void exitArrayName(Fortran77Parser.ArrayNameContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#subroutineName}.
	 * @param ctx the parse tree
	 */
	void enterSubroutineName(Fortran77Parser.SubroutineNameContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#subroutineName}.
	 * @param ctx the parse tree
	 */
	void exitSubroutineName(Fortran77Parser.SubroutineNameContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#functionName}.
	 * @param ctx the parse tree
	 */
	void enterFunctionName(Fortran77Parser.FunctionNameContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#functionName}.
	 * @param ctx the parse tree
	 */
	void exitFunctionName(Fortran77Parser.FunctionNameContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#constant}.
	 * @param ctx the parse tree
	 */
	void enterConstant(Fortran77Parser.ConstantContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#constant}.
	 * @param ctx the parse tree
	 */
	void exitConstant(Fortran77Parser.ConstantContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#unsignedArithmeticConstant}.
	 * @param ctx the parse tree
	 */
	void enterUnsignedArithmeticConstant(Fortran77Parser.UnsignedArithmeticConstantContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#unsignedArithmeticConstant}.
	 * @param ctx the parse tree
	 */
	void exitUnsignedArithmeticConstant(Fortran77Parser.UnsignedArithmeticConstantContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#complexConstant}.
	 * @param ctx the parse tree
	 */
	void enterComplexConstant(Fortran77Parser.ComplexConstantContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#complexConstant}.
	 * @param ctx the parse tree
	 */
	void exitComplexConstant(Fortran77Parser.ComplexConstantContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#logicalConstant}.
	 * @param ctx the parse tree
	 */
	void enterLogicalConstant(Fortran77Parser.LogicalConstantContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#logicalConstant}.
	 * @param ctx the parse tree
	 */
	void exitLogicalConstant(Fortran77Parser.LogicalConstantContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#identifier}.
	 * @param ctx the parse tree
	 */
	void enterIdentifier(Fortran77Parser.IdentifierContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#identifier}.
	 * @param ctx the parse tree
	 */
	void exitIdentifier(Fortran77Parser.IdentifierContext ctx);
	/**
	 * Enter a parse tree produced by {@link Fortran77Parser#to}.
	 * @param ctx the parse tree
	 */
	void enterTo(Fortran77Parser.ToContext ctx);
	/**
	 * Exit a parse tree produced by {@link Fortran77Parser#to}.
	 * @param ctx the parse tree
	 */
	void exitTo(Fortran77Parser.ToContext ctx);
}