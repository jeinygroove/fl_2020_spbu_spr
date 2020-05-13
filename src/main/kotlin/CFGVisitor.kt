class CFGVisitor: GrammarBaseVisitor<AST>() {
    override fun visitCfg_rules(ctx: GrammarParser.Cfg_rulesContext?): AST {
        if (ctx == null) throw ParserException("visitCfg_rules: context is empty")
        return if (ctx.childCount == 1) {
            val l = visitCfg_rule(ctx.getChild(0) as GrammarParser.Cfg_ruleContext) as CFG_Rule
            CFG_Rules(null, l)
        } else {
            val l = visitCfg_rules(ctx.getChild(0) as GrammarParser.Cfg_rulesContext) as CFG_Rules
            val r = visitCfg_rule(ctx.getChild(1) as GrammarParser.Cfg_ruleContext) as CFG_Rule
            CFG_Rules(l, r)
        }
    }

    override fun visitCfg_rule(ctx: GrammarParser.Cfg_ruleContext?): AST {
        if (ctx == null) throw ParserException("visitCfg_rule: context is empty")
        val start =
            ctx.getChild(0) as GrammarParser.Start_nonterminalContext
        val l = visitStart_nonterminal(start) as Nonterminal
        val rhs = visitRhs(ctx.getChild(2) as GrammarParser.RhsContext)
        return CFG_Rule(l, rhs)
    }

    override fun visitStr(ctx: GrammarParser.StrContext?): AST {
        if (ctx == null) throw ParserException("visitStr: context is empty")
        val r = mutableListOf<AST>()
        for (child in ctx.children) {
            val token = when (child) {
                is GrammarParser.NonterminalContext -> visitNonterminal(child)
                is GrammarParser.TerminalContext -> visitTerminal(child)
                is GrammarParser.EndContext -> null
                else -> throw ParserException("visitCfg_rule: unknown type in CFG_Rule parsing\n${child.text}")
            }
            token?.let { r.add(it) }
        }
        return Str(r)
    }

    override fun visitRhs(ctx: GrammarParser.RhsContext?): Rhs {
        if (ctx == null) throw ParserException("visitRhs: context is empty")
        return if (ctx.childCount == 1) {
            val r = visitStr(ctx.getChild(0) as GrammarParser.StrContext) as Str
            Rhs(null, null, r)
        } else {
            val rhs = visitRhs(ctx.getChild(0) as GrammarParser.RhsContext)
            val del = visitDelimeter(ctx.getChild(1) as GrammarParser.DelimeterContext) as Delimeter
            val r = visitStr(ctx.getChild(2) as GrammarParser.StrContext) as Str
            Rhs(rhs, del, r)
        }
    }

    override fun visitNonterminal(ctx: GrammarParser.NonterminalContext?): AST {
        return Nonterminal(ctx?.text ?: "")
    }

    override fun visitTerminal(ctx: GrammarParser.TerminalContext?): AST {
        if (ctx?.text == "<eps>")
            return Epsilon()
        return Terminal(ctx?.text ?: "")
    }

    override fun visitDelimeter(ctx: GrammarParser.DelimeterContext?): AST {
        return Delimeter("|")
    }
}