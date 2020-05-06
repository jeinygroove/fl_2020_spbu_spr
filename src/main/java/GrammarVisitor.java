// Generated from Grammar.g4 by ANTLR 4.7.1
import org.antlr.v4.runtime.tree.ParseTreeVisitor;

/**
 * This interface defines a complete generic visitor for a parse tree produced
 * by {@link GrammarParser}.
 *
 * @param <T> The return type of the visit operation. Use {@link Void} for
 * operations with no return type.
 */
public interface GrammarVisitor<T> extends ParseTreeVisitor<T> {
	/**
	 * Visit a parse tree produced by {@link GrammarParser#cfg_rules}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitCfg_rules(GrammarParser.Cfg_rulesContext ctx);
	/**
	 * Visit a parse tree produced by {@link GrammarParser#cfg_rule}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitCfg_rule(GrammarParser.Cfg_ruleContext ctx);
	/**
	 * Visit a parse tree produced by {@link GrammarParser#rhs}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitRhs(GrammarParser.RhsContext ctx);
	/**
	 * Visit a parse tree produced by {@link GrammarParser#str}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitStr(GrammarParser.StrContext ctx);
	/**
	 * Visit a parse tree produced by {@link GrammarParser#start_nonterminal}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitStart_nonterminal(GrammarParser.Start_nonterminalContext ctx);
	/**
	 * Visit a parse tree produced by {@link GrammarParser#nonterminal}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitNonterminal(GrammarParser.NonterminalContext ctx);
	/**
	 * Visit a parse tree produced by {@link GrammarParser#terminal}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitTerminal(GrammarParser.TerminalContext ctx);
	/**
	 * Visit a parse tree produced by {@link GrammarParser#delimeter}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitDelimeter(GrammarParser.DelimeterContext ctx);
	/**
	 * Visit a parse tree produced by {@link GrammarParser#space}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitSpace(GrammarParser.SpaceContext ctx);
	/**
	 * Visit a parse tree produced by {@link GrammarParser#end}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitEnd(GrammarParser.EndContext ctx);
	/**
	 * Visit a parse tree produced by {@link GrammarParser#newline}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitNewline(GrammarParser.NewlineContext ctx);
	/**
	 * Visit a parse tree produced by {@link GrammarParser#eof}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitEof(GrammarParser.EofContext ctx);
}