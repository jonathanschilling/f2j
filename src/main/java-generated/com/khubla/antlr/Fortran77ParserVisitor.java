// Generated from com/khubla/antlr/Fortran77Parser.g4 by ANTLR 4.7.2
package com.khubla.antlr;
import org.antlr.v4.runtime.tree.ParseTreeVisitor;

/**
 * This interface defines a complete generic visitor for a parse tree produced
 * by {@link Fortran77Parser}.
 *
 * @param <T> The return type of the visit operation. Use {@link Void} for
 * operations with no return type.
 */
public interface Fortran77ParserVisitor<T> extends ParseTreeVisitor<T> {
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#program}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitProgram(Fortran77Parser.ProgramContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#executableUnit}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitExecutableUnit(Fortran77Parser.ExecutableUnitContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#mainProgram}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitMainProgram(Fortran77Parser.MainProgramContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#functionSubprogram}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitFunctionSubprogram(Fortran77Parser.FunctionSubprogramContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#subroutineSubprogram}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitSubroutineSubprogram(Fortran77Parser.SubroutineSubprogramContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#blockdataSubprogram}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitBlockdataSubprogram(Fortran77Parser.BlockdataSubprogramContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#otherSpecificationStatement}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitOtherSpecificationStatement(Fortran77Parser.OtherSpecificationStatementContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#executableStatement}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitExecutableStatement(Fortran77Parser.ExecutableStatementContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#programStatement}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitProgramStatement(Fortran77Parser.ProgramStatementContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#entryStatement}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitEntryStatement(Fortran77Parser.EntryStatementContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#functionStatement}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitFunctionStatement(Fortran77Parser.FunctionStatementContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#blockdataStatement}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitBlockdataStatement(Fortran77Parser.BlockdataStatementContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#subroutineStatement}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitSubroutineStatement(Fortran77Parser.SubroutineStatementContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#namelist}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitNamelist(Fortran77Parser.NamelistContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#statement}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitStatement(Fortran77Parser.StatementContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#subprogramBody}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitSubprogramBody(Fortran77Parser.SubprogramBodyContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#wholeStatement}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitWholeStatement(Fortran77Parser.WholeStatementContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#endStatement}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitEndStatement(Fortran77Parser.EndStatementContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#dimensionStatement}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitDimensionStatement(Fortran77Parser.DimensionStatementContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#arrayDeclarator}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitArrayDeclarator(Fortran77Parser.ArrayDeclaratorContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#arrayDeclarators}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitArrayDeclarators(Fortran77Parser.ArrayDeclaratorsContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#arrayDeclaratorExtents}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitArrayDeclaratorExtents(Fortran77Parser.ArrayDeclaratorExtentsContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#arrayDeclaratorExtent}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitArrayDeclaratorExtent(Fortran77Parser.ArrayDeclaratorExtentContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#equivalenceStatement}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitEquivalenceStatement(Fortran77Parser.EquivalenceStatementContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#equivEntityGroup}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitEquivEntityGroup(Fortran77Parser.EquivEntityGroupContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#equivEntity}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitEquivEntity(Fortran77Parser.EquivEntityContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#commonStatement}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitCommonStatement(Fortran77Parser.CommonStatementContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#commonName}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitCommonName(Fortran77Parser.CommonNameContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#commonItem}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitCommonItem(Fortran77Parser.CommonItemContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#commonItems}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitCommonItems(Fortran77Parser.CommonItemsContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#commonBlock}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitCommonBlock(Fortran77Parser.CommonBlockContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#commentStatement}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitCommentStatement(Fortran77Parser.CommentStatementContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#typeStatement}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitTypeStatement(Fortran77Parser.TypeStatementContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#typeStatementNameList}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitTypeStatementNameList(Fortran77Parser.TypeStatementNameListContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#typeStatementName}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitTypeStatementName(Fortran77Parser.TypeStatementNameContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#typeStatementNameCharList}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitTypeStatementNameCharList(Fortran77Parser.TypeStatementNameCharListContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#typeStatementNameChar}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitTypeStatementNameChar(Fortran77Parser.TypeStatementNameCharContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#typeStatementLenSpec}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitTypeStatementLenSpec(Fortran77Parser.TypeStatementLenSpecContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#typename}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitTypename(Fortran77Parser.TypenameContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#type}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitType(Fortran77Parser.TypeContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#typenameLen}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitTypenameLen(Fortran77Parser.TypenameLenContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#pointerStatement}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitPointerStatement(Fortran77Parser.PointerStatementContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#pointerDecl}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitPointerDecl(Fortran77Parser.PointerDeclContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#implicitStatement}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitImplicitStatement(Fortran77Parser.ImplicitStatementContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#implicitSpec}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitImplicitSpec(Fortran77Parser.ImplicitSpecContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#implicitSpecs}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitImplicitSpecs(Fortran77Parser.ImplicitSpecsContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#implicitNone}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitImplicitNone(Fortran77Parser.ImplicitNoneContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#implicitLetter}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitImplicitLetter(Fortran77Parser.ImplicitLetterContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#implicitRange}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitImplicitRange(Fortran77Parser.ImplicitRangeContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#implicitLetters}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitImplicitLetters(Fortran77Parser.ImplicitLettersContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#lenSpecification}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitLenSpecification(Fortran77Parser.LenSpecificationContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#characterWithLen}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitCharacterWithLen(Fortran77Parser.CharacterWithLenContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#cwlLen}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitCwlLen(Fortran77Parser.CwlLenContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#parameterStatement}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitParameterStatement(Fortran77Parser.ParameterStatementContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#paramlist}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitParamlist(Fortran77Parser.ParamlistContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#paramassign}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitParamassign(Fortran77Parser.ParamassignContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#externalStatement}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitExternalStatement(Fortran77Parser.ExternalStatementContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#intrinsicStatement}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitIntrinsicStatement(Fortran77Parser.IntrinsicStatementContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#saveStatement}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitSaveStatement(Fortran77Parser.SaveStatementContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#saveEntity}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitSaveEntity(Fortran77Parser.SaveEntityContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#dataStatement}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitDataStatement(Fortran77Parser.DataStatementContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#dataStatementItem}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitDataStatementItem(Fortran77Parser.DataStatementItemContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#dataStatementMultiple}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitDataStatementMultiple(Fortran77Parser.DataStatementMultipleContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#dataStatementEntity}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitDataStatementEntity(Fortran77Parser.DataStatementEntityContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#dse1}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitDse1(Fortran77Parser.Dse1Context ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#dse2}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitDse2(Fortran77Parser.Dse2Context ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#dataImpliedDo}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitDataImpliedDo(Fortran77Parser.DataImpliedDoContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#dataImpliedDoRange}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitDataImpliedDoRange(Fortran77Parser.DataImpliedDoRangeContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#dataImpliedDoList}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitDataImpliedDoList(Fortran77Parser.DataImpliedDoListContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#dataImpliedDoListWhat}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitDataImpliedDoListWhat(Fortran77Parser.DataImpliedDoListWhatContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#gotoStatement}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitGotoStatement(Fortran77Parser.GotoStatementContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#unconditionalGoto}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitUnconditionalGoto(Fortran77Parser.UnconditionalGotoContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#computedGoto}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitComputedGoto(Fortran77Parser.ComputedGotoContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#lblRef}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitLblRef(Fortran77Parser.LblRefContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#labelList}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitLabelList(Fortran77Parser.LabelListContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#assignedGoto}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitAssignedGoto(Fortran77Parser.AssignedGotoContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#ifStatement}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitIfStatement(Fortran77Parser.IfStatementContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#arithmeticIfStatement}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitArithmeticIfStatement(Fortran77Parser.ArithmeticIfStatementContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#logicalIfStatement}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitLogicalIfStatement(Fortran77Parser.LogicalIfStatementContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#blockIfStatement}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitBlockIfStatement(Fortran77Parser.BlockIfStatementContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#firstIfBlock}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitFirstIfBlock(Fortran77Parser.FirstIfBlockContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#elseIfStatement}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitElseIfStatement(Fortran77Parser.ElseIfStatementContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#elseStatement}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitElseStatement(Fortran77Parser.ElseStatementContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#endIfStatement}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitEndIfStatement(Fortran77Parser.EndIfStatementContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#doStatement}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitDoStatement(Fortran77Parser.DoStatementContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#doVarArgs}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitDoVarArgs(Fortran77Parser.DoVarArgsContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#doWithLabel}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitDoWithLabel(Fortran77Parser.DoWithLabelContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#doBody}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitDoBody(Fortran77Parser.DoBodyContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#doWithEndDo}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitDoWithEndDo(Fortran77Parser.DoWithEndDoContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#enddoStatement}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitEnddoStatement(Fortran77Parser.EnddoStatementContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#continueStatement}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitContinueStatement(Fortran77Parser.ContinueStatementContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#stopStatement}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitStopStatement(Fortran77Parser.StopStatementContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#pauseStatement}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitPauseStatement(Fortran77Parser.PauseStatementContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#writeStatement}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitWriteStatement(Fortran77Parser.WriteStatementContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#readStatement}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitReadStatement(Fortran77Parser.ReadStatementContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#printStatement}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitPrintStatement(Fortran77Parser.PrintStatementContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#assignmentStatement}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitAssignmentStatement(Fortran77Parser.AssignmentStatementContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#controlInfoList}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitControlInfoList(Fortran77Parser.ControlInfoListContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#controlErrSpec}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitControlErrSpec(Fortran77Parser.ControlErrSpecContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#controlInfoListItem}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitControlInfoListItem(Fortran77Parser.ControlInfoListItemContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#ioList}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitIoList(Fortran77Parser.IoListContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#ioListItem}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitIoListItem(Fortran77Parser.IoListItemContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#ioImpliedDoList}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitIoImpliedDoList(Fortran77Parser.IoImpliedDoListContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#openStatement}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitOpenStatement(Fortran77Parser.OpenStatementContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#openControl}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitOpenControl(Fortran77Parser.OpenControlContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#controlFmt}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitControlFmt(Fortran77Parser.ControlFmtContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#controlUnit}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitControlUnit(Fortran77Parser.ControlUnitContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#controlRec}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitControlRec(Fortran77Parser.ControlRecContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#controlEnd}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitControlEnd(Fortran77Parser.ControlEndContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#controlErr}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitControlErr(Fortran77Parser.ControlErrContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#controlIostat}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitControlIostat(Fortran77Parser.ControlIostatContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#controlFile}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitControlFile(Fortran77Parser.ControlFileContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#controlStatus}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitControlStatus(Fortran77Parser.ControlStatusContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#controlAccess}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitControlAccess(Fortran77Parser.ControlAccessContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#controlPosition}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitControlPosition(Fortran77Parser.ControlPositionContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#controlForm}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitControlForm(Fortran77Parser.ControlFormContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#controlRecl}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitControlRecl(Fortran77Parser.ControlReclContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#controlBlank}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitControlBlank(Fortran77Parser.ControlBlankContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#controlExist}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitControlExist(Fortran77Parser.ControlExistContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#controlOpened}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitControlOpened(Fortran77Parser.ControlOpenedContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#controlNumber}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitControlNumber(Fortran77Parser.ControlNumberContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#controlNamed}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitControlNamed(Fortran77Parser.ControlNamedContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#controlName}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitControlName(Fortran77Parser.ControlNameContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#controlSequential}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitControlSequential(Fortran77Parser.ControlSequentialContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#controlDirect}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitControlDirect(Fortran77Parser.ControlDirectContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#controlFormatted}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitControlFormatted(Fortran77Parser.ControlFormattedContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#controlUnformatted}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitControlUnformatted(Fortran77Parser.ControlUnformattedContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#controlNextrec}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitControlNextrec(Fortran77Parser.ControlNextrecContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#closeStatement}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitCloseStatement(Fortran77Parser.CloseStatementContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#closeControl}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitCloseControl(Fortran77Parser.CloseControlContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#inquireStatement}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitInquireStatement(Fortran77Parser.InquireStatementContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#inquireControl}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitInquireControl(Fortran77Parser.InquireControlContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#backspaceStatement}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitBackspaceStatement(Fortran77Parser.BackspaceStatementContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#endfileStatement}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitEndfileStatement(Fortran77Parser.EndfileStatementContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#rewindStatement}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitRewindStatement(Fortran77Parser.RewindStatementContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#berFinish}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitBerFinish(Fortran77Parser.BerFinishContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#berFinishItem}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitBerFinishItem(Fortran77Parser.BerFinishItemContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#unitIdentifier}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitUnitIdentifier(Fortran77Parser.UnitIdentifierContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#formatIdentifier}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitFormatIdentifier(Fortran77Parser.FormatIdentifierContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#formatStatement}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitFormatStatement(Fortran77Parser.FormatStatementContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#fmtSpec}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitFmtSpec(Fortran77Parser.FmtSpecContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#formatsep}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitFormatsep(Fortran77Parser.FormatsepContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#formatedit}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitFormatedit(Fortran77Parser.FormateditContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#editElement}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitEditElement(Fortran77Parser.EditElementContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#statementFunctionStatement}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitStatementFunctionStatement(Fortran77Parser.StatementFunctionStatementContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#sfArgs}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitSfArgs(Fortran77Parser.SfArgsContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#callStatement}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitCallStatement(Fortran77Parser.CallStatementContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#subroutineCall}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitSubroutineCall(Fortran77Parser.SubroutineCallContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#callArgumentList}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitCallArgumentList(Fortran77Parser.CallArgumentListContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#callArgument}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitCallArgument(Fortran77Parser.CallArgumentContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#returnStatement}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitReturnStatement(Fortran77Parser.ReturnStatementContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#expression}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitExpression(Fortran77Parser.ExpressionContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#ncExpr}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitNcExpr(Fortran77Parser.NcExprContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#lexpr0}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitLexpr0(Fortran77Parser.Lexpr0Context ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#lexpr1}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitLexpr1(Fortran77Parser.Lexpr1Context ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#lexpr2}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitLexpr2(Fortran77Parser.Lexpr2Context ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#lexpr3}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitLexpr3(Fortran77Parser.Lexpr3Context ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#lexpr4}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitLexpr4(Fortran77Parser.Lexpr4Context ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#aexpr0}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitAexpr0(Fortran77Parser.Aexpr0Context ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#aexpr1}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitAexpr1(Fortran77Parser.Aexpr1Context ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#aexpr2}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitAexpr2(Fortran77Parser.Aexpr2Context ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#aexpr3}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitAexpr3(Fortran77Parser.Aexpr3Context ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#aexpr4}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitAexpr4(Fortran77Parser.Aexpr4Context ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#iexpr}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitIexpr(Fortran77Parser.IexprContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#iexprCode}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitIexprCode(Fortran77Parser.IexprCodeContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#iexpr1}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitIexpr1(Fortran77Parser.Iexpr1Context ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#iexpr2}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitIexpr2(Fortran77Parser.Iexpr2Context ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#iexpr3}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitIexpr3(Fortran77Parser.Iexpr3Context ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#iexpr4}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitIexpr4(Fortran77Parser.Iexpr4Context ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#constantExpr}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitConstantExpr(Fortran77Parser.ConstantExprContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#arithmeticExpression}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitArithmeticExpression(Fortran77Parser.ArithmeticExpressionContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#integerExpr}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitIntegerExpr(Fortran77Parser.IntegerExprContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#intRealDpExpr}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitIntRealDpExpr(Fortran77Parser.IntRealDpExprContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#arithmeticConstExpr}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitArithmeticConstExpr(Fortran77Parser.ArithmeticConstExprContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#intConstantExpr}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitIntConstantExpr(Fortran77Parser.IntConstantExprContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#characterExpression}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitCharacterExpression(Fortran77Parser.CharacterExpressionContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#concatOp}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitConcatOp(Fortran77Parser.ConcatOpContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#logicalExpression}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitLogicalExpression(Fortran77Parser.LogicalExpressionContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#logicalConstExpr}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitLogicalConstExpr(Fortran77Parser.LogicalConstExprContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#arrayElementName}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitArrayElementName(Fortran77Parser.ArrayElementNameContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#subscripts}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitSubscripts(Fortran77Parser.SubscriptsContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#varRef}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitVarRef(Fortran77Parser.VarRefContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#varRefCode}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitVarRefCode(Fortran77Parser.VarRefCodeContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#substringApp}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitSubstringApp(Fortran77Parser.SubstringAppContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#variableName}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitVariableName(Fortran77Parser.VariableNameContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#arrayName}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitArrayName(Fortran77Parser.ArrayNameContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#subroutineName}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitSubroutineName(Fortran77Parser.SubroutineNameContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#functionName}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitFunctionName(Fortran77Parser.FunctionNameContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#constant}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitConstant(Fortran77Parser.ConstantContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#unsignedArithmeticConstant}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitUnsignedArithmeticConstant(Fortran77Parser.UnsignedArithmeticConstantContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#complexConstant}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitComplexConstant(Fortran77Parser.ComplexConstantContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#logicalConstant}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitLogicalConstant(Fortran77Parser.LogicalConstantContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#identifier}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitIdentifier(Fortran77Parser.IdentifierContext ctx);
	/**
	 * Visit a parse tree produced by {@link Fortran77Parser#to}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitTo(Fortran77Parser.ToContext ctx);
}