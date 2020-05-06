// Generated from Grammar.g4 by ANTLR 4.7.1
import org.antlr.v4.runtime.tree.ParseTreeListener;

/**
 * This interface defines a complete listener for a parse tree produced by
 * {@link GrammarParser}.
 */
public interface GrammarListener extends ParseTreeListener {
	/**
	 * Enter a parse tree produced by {@link GrammarParser#cfg_rules}.
	 * @param ctx the parse tree
	 */
	void enterCfg_rules(GrammarParser.Cfg_rulesContext ctx);
	/**
	 * Exit a parse tree produced by {@link GrammarParser#cfg_rules}.
	 * @param ctx the parse tree
	 */
	void exitCfg_rules(GrammarParser.Cfg_rulesContext ctx);
	/**
	 * Enter a parse tree produced by {@link GrammarParser#cfg_rule}.
	 * @param ctx the parse tree
	 */
	void enterCfg_rule(GrammarParser.Cfg_ruleContext ctx);
	/**
	 * Exit a parse tree produced by {@link GrammarParser#cfg_rule}.
	 * @param ctx the parse tree
	 */
	void exitCfg_rule(GrammarParser.Cfg_ruleContext ctx);
	/**
	 * Enter a parse tree produced by {@link GrammarParser#rhs}.
	 * @param ctx the parse tree
	 */
	void enterRhs(GrammarParser.RhsContext ctx);
	/**
	 * Exit a parse tree produced by {@link GrammarParser#rhs}.
	 * @param ctx the parse tree
	 */
	void exitRhs(GrammarParser.RhsContext ctx);
	/**
	 * Enter a parse tree produced by {@link GrammarParser#str}.
	 * @param ctx the parse tree
	 */
	void enterStr(GrammarParser.StrContext ctx);
	/**
	 * Exit a parse tree produced by {@link GrammarParser#str}.
	 * @param ctx the parse tree
	 */
	void exitStr(GrammarParser.StrContext ctx);
	/**
	 * Enter a parse tree produced by {@link GrammarParser#start_nonterminal}.
	 * @param ctx the parse tree
	 */
	void enterStart_nonterminal(GrammarParser.Start_nonterminalContext ctx);
	/**
	 * Exit a parse tree produced by {@link GrammarParser#start_nonterminal}.
	 * @param ctx the parse tree
	 */
	void exitStart_nonterminal(GrammarParser.Start_nonterminalContext ctx);
	/**
	 * Enter a parse tree produced by {@link GrammarParser#nonterminal}.
	 * @param ctx the parse tree
	 */
	void enterNonterminal(GrammarParser.NonterminalContext ctx);
	/**
	 * Exit a parse tree produced by {@link GrammarParser#nonterminal}.
	 * @param ctx the parse tree
	 */
	void exitNonterminal(GrammarParser.NonterminalContext ctx);
	/**
	 * Enter a parse tree produced by {@link GrammarParser#terminal}.
	 * @param ctx the parse tree
	 */
	void enterTerminal(GrammarParser.TerminalContext ctx);
	/**
	 * Exit a parse tree produced by {@link GrammarParser#terminal}.
	 * @param ctx the parse tree
	 */
	void exitTerminal(GrammarParser.TerminalContext ctx);
	/**
	 * Enter a parse tree produced by {@link GrammarParser#delimeter}.
	 * @param ctx the parse tree
	 */
	void enterDelimeter(GrammarParser.DelimeterContext ctx);
	/**
	 * Exit a parse tree produced by {@link GrammarParser#delimeter}.
	 * @param ctx the parse tree
	 */
	void exitDelimeter(GrammarParser.DelimeterContext ctx);
	/**
	 * Enter a parse tree produced by {@link GrammarParser#space}.
	 * @param ctx the parse tree
	 */
	void enterSpace(GrammarParser.SpaceContext ctx);
	/**
	 * Exit a parse tree produced by {@link GrammarParser#space}.
	 * @param ctx the parse tree
	 */
	void exitSpace(GrammarParser.SpaceContext ctx);
	/**
	 * Enter a parse tree produced by {@link GrammarParser#end}.
	 * @param ctx the parse tree
	 */
	void enterEnd(GrammarParser.EndContext ctx);
	/**
	 * Exit a parse tree produced by {@link GrammarParser#end}.
	 * @param ctx the parse tree
	 */
	void exitEnd(GrammarParser.EndContext ctx);
	/**
	 * Enter a parse tree produced by {@link GrammarParser#newline}.
	 * @param ctx the parse tree
	 */
	void enterNewline(GrammarParser.NewlineContext ctx);
	/**
	 * Exit a parse tree produced by {@link GrammarParser#newline}.
	 * @param ctx the parse tree
	 */
	void exitNewline(GrammarParser.NewlineContext ctx);
	/**
	 * Enter a parse tree produced by {@link GrammarParser#eof}.
	 * @param ctx the parse tree
	 */
	void enterEof(GrammarParser.EofContext ctx);
	/**
	 * Exit a parse tree produced by {@link GrammarParser#eof}.
	 * @param ctx the parse tree
	 */
	void exitEof(GrammarParser.EofContext ctx);
}