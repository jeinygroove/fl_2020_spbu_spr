import junit.framework.TestCase.assertEquals
import org.antlr.v4.runtime.CharStreams
import org.antlr.v4.runtime.CommonTokenStream
import org.junit.Test
import java.io.File

internal class CNFConverterTest {
    private fun init(input: String) {
        val filename = "tmp.txt"
        val file = File(filename)
        file.createNewFile()
        file.writeText(input)
    }

    private fun clear() {
        val filename = "tmp.txt"
        val file = File(filename)
        file.deleteOnExit()
    }

    private fun parse(filename: String = "tmp.txt"): AST {
        val charStream = CharStreams.fromFileName(filename)

        val lexer = GrammarLexer(charStream)
        lexer.removeErrorListeners();
        lexer.addErrorListener(GrammarErrorListener.INSTANCE)

        val commonTokenStream = CommonTokenStream(lexer)

        val parser = GrammarParser(commonTokenStream)
        parser.removeErrorListeners();
        parser.addErrorListener(GrammarErrorListener.INSTANCE)

        val parseTree = parser.cfg_rules();
        val visitor = CFGVisitor()
        return visitor.visit(parseTree)
    }

    @Test
    fun testRemoveLongRules() {
        init(
            "<S> -> /a<U>/b<U>\n" +
                    "<U> -> /c | /d"
        )
        val ast = parse()
        val converter = CNFconverter(Nonterminal("<S>"), ast as CFG_Rules)
        val result = converter.reduceLongRules()
        assertEquals(
            setOf(
                SimpleRule(Nonterminal("<S>"), Str(listOf(Nonterminal("<S0>"), Nonterminal("<U>")))),
                SimpleRule(Nonterminal("<S0>"), Str(listOf(Nonterminal("<S1>"), Terminal("/b")))),
                SimpleRule(Nonterminal("<S1>"), Str(listOf(Terminal("/a"), Nonterminal("<U>")))),
                SimpleRule(Nonterminal("<U>"), Str(listOf(Terminal("/c")))),
                SimpleRule(Nonterminal("<U>"), Str(listOf(Terminal("/d"))))
            ), result
        )
    }

    @Test
    fun testReduceEpsilonRules() {
        init("<S> -> /a<N>\n <N> -> <eps>")
        val ast = parse()
        val converter = CNFconverter(Nonterminal("<S>"), ast as CFG_Rules)
        val result = converter.reduceEpsilonRules()
        assertEquals(
            setOf<SimpleRule>(
                SimpleRule(Nonterminal("<S>"), Str(listOf(Terminal("/a"), Nonterminal("<N>")))),
                SimpleRule(Nonterminal("<S>"), Str(listOf(Terminal("/a"))))
            ), result
        )
    }

    @Test
    fun testReduceUnitRules() {
        init("<S> -> /a<N>\n <N> -> <F>\n <F> -> /x\n <Z> -> <Z>")
        val ast = parse()
        val converter = CNFconverter(Nonterminal("<S>"), ast as CFG_Rules)
        val result = converter.reduceUnitRules()
        assertEquals(
            setOf<SimpleRule>(
                SimpleRule(Nonterminal("<S>"), Str(listOf(Terminal("/a"), Nonterminal("<N>")))),
                SimpleRule(Nonterminal("<F>"), Str(listOf(Terminal("/x")))),
                SimpleRule(Nonterminal("<N>"), Str(listOf(Terminal("/x"))))
            ), result
        )
    }

    @Test
    fun testReduceNonGeneratingRules() {
        init("<S> -> /a<N> | /a<F>\n <F> -> /x")
        val ast = parse()
        val converter = CNFconverter(Nonterminal("<S>"), ast as CFG_Rules)
        val result = converter.reduceNonGeneratingRules()
        assertEquals(
            setOf(
                SimpleRule(Nonterminal("<S>"), Str(listOf(Terminal("/a"), Nonterminal("<F>")))),
                SimpleRule(Nonterminal("<F>"), Str(listOf(Terminal("/x"))))
            ), result
        )
    }

    @Test
    fun testReduceUnreachable() {
        init("<S> -> /a<N> \n <N> -> /x\n <F> -> /x")
        val ast = parse()
        val converter = CNFconverter(Nonterminal("<S>"), ast as CFG_Rules)
        val result = converter.reduceUnreachable()
        assertEquals(
            setOf(
                SimpleRule(Nonterminal("<S>"), Str(listOf(Terminal("/a"), Nonterminal("<N>")))),
                SimpleRule(Nonterminal("<N>"), Str(listOf(Terminal("/x"))))
            ), result
        )
    }

    @Test
    fun testReduceLongTerminalRules() {
        init("<S> -> /a/b") //long terminal rules suppose to have length 2
        val ast = parse()
        val converter = CNFconverter(Nonterminal("<S>"), ast as CFG_Rules)
        val result = converter.reduceLongTerminalRules()
        assertEquals(
            setOf(
                SimpleRule(Nonterminal("<S>"), Str(listOf(Nonterminal("<S0>"), Nonterminal("<S1>")))),
                SimpleRule(Nonterminal("<S0>"), Str(listOf(Terminal("/a")))),
                SimpleRule(Nonterminal("<S1>"), Str(listOf(Terminal("/b"))))
            ), result
        )
    }

    @Test
    fun ToChomskyNormalForm1() {
        init(
            "<S> -> <eps> | /a<U>/b<U>\n" +
                    "<U> -> <S> | /b/a"
        )
        val ast = parse()
        val converter = CNFconverter(Nonterminal("<S>"), ast as CFG_Rules)
        converter.reduceLongTerminalRules()
        val result = converter.toRules()
        assertEquals(Nonterminal("<S3>"), converter.getStart())
        assertEquals(
            setOf<Rule>(
                Rule(
                    Nonterminal("<S3>"), setOf(
                        Str(listOf(Epsilon())), Str(listOf(Nonterminal("<S1>"), Nonterminal("<U>"))), Str(
                            listOf(Nonterminal("<S2>"), Nonterminal("<S0>"))
                        )
                    )
                ),
                Rule(
                    Nonterminal("<S2>"),
                    setOf(Str(listOf(Terminal("/a"))), Str(listOf(Nonterminal("<S4>"), Nonterminal("<U>"))))
                ),
                Rule(
                    Nonterminal("<U>"),
                    setOf(
                        Str(listOf(Nonterminal("<S1>"), Nonterminal("<U>"))),
                        Str(listOf(Nonterminal("<S0>"), Nonterminal("<S4>"))),
                        Str(listOf(Nonterminal("<S2>"), Nonterminal("<S0>")))
                    )
                ),
                Rule(Nonterminal("<S1>"), setOf(Str(listOf(Nonterminal("<S2>"), Nonterminal("<S0>"))))),
                Rule(Nonterminal("<S0>"), setOf(Str(listOf(Terminal("/b"))))),
                Rule(Nonterminal("<S4>"), setOf(Str(listOf(Terminal("/a")))))
            ), result
        )

    }

    @Test
    fun ToChomskyNormalForm2() {
        init(
            "<S> -> /a<X>/b<X> | /a<Z>\n" +
                    "<X> -> /a<Y> | /b<Y> | <eps>\n" +
                    "<Y> -> <X> | /c/c\n" +
                    "<Z> -> <Z><X>"
        )
        val ast = parse()
        val converter = CNFconverter(Nonterminal("<S>"), ast as CFG_Rules)
        converter.reduceLongTerminalRules()
        val result = converter.toRules()
        assertEquals(Nonterminal("<S>"), converter.getStart())
        assertEquals(
            setOf<Rule>(
                Rule(
                    Nonterminal("<X>"),
                    setOf(
                        Str(listOf(Terminal("/b"))),
                        Str(listOf(Terminal("/a"))),
                        Str(listOf(Nonterminal("<S3>"), Nonterminal("<Y>"))),
                        Str(listOf(Nonterminal("<S4>"), Nonterminal("<Y>")))
                    )
                ),
                Rule(
                    Nonterminal("<S>"),
                    setOf(
                        Str(listOf(Nonterminal("<S0>"), Nonterminal("<X>"))),
                        Str(listOf(Nonterminal("<S1>"), Nonterminal("<S3>")))
                    )
                ),
                Rule(
                    Nonterminal("<S1>"),
                    setOf(Str(listOf(Terminal("/a"))), Str(listOf(Nonterminal("<S4>"), Nonterminal("<X>"))))
                ),
                Rule(
                    Nonterminal("<Y>"), setOf(
                        Str(listOf(Terminal("/b"))),
                        Str(listOf(Terminal("/a"))),
                        Str(listOf(Nonterminal("<S2>"), Nonterminal("<S2>"))),
                        Str(listOf(Nonterminal("<S3>"), Nonterminal("<Y>"))),
                        Str(
                            listOf(Nonterminal("<S4>"), Nonterminal("<Y>"))
                        )
                    )
                ),
                Rule(Nonterminal("<S0>"), setOf(Str(listOf(Nonterminal("<S1>"), Nonterminal("<S3>"))))),
                Rule(Nonterminal("<S2>"), setOf(Str(listOf(Terminal("/c"))))),
                Rule(Nonterminal("<S3>"), setOf(Str(listOf(Terminal("/b"))))),
                Rule(Nonterminal("<S4>"), setOf(Str(listOf(Terminal("/a")))))
            ), result
        )
    }

    @Test
    fun ToChomskyNormalForm3() {
        init("<S> -> /a/b | /a<S>/b | <S><S>")
        val ast = parse()
        val converter = CNFconverter(Nonterminal("<S>"), ast as CFG_Rules)
        converter.toChomskyNormalForm()
        val result = converter.toRules()
        assertEquals(converter.getStart(), Nonterminal("<S0>"))
        assertEquals(
            setOf<Rule>(
                Rule(
                    Nonterminal("<S>"),
                    setOf(
                        Str(listOf(Nonterminal("<S>"), Nonterminal("<S>"))),
                        Str(listOf(Nonterminal("<S3>"), Nonterminal("<S2>"))),
                        Str(listOf(Nonterminal("<S1>"), Nonterminal("<S2>")))
                    )
                ),
                Rule(
                    Nonterminal("<S0>"),
                    setOf(
                        Str(listOf(Nonterminal("<S>"), Nonterminal("<S>"))),
                        Str(listOf(Nonterminal("<S3>"), Nonterminal("<S2>"))),
                        Str(listOf(Nonterminal("<S1>"), Nonterminal("<S2>")))
                    )
                ),
                Rule(Nonterminal("<S1>"), setOf(Str(listOf(Nonterminal("<S3>"), Nonterminal("<S>"))))),
                Rule(Nonterminal("<S2>"), setOf(Str(listOf(Terminal("/b"))))),
                Rule(Nonterminal("<S3>"), setOf(Str(listOf(Terminal("/a")))))
            ), result
        )

    }


}