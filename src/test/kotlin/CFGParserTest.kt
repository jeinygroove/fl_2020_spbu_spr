import junit.framework.TestCase.assertEquals
import org.antlr.v4.runtime.CharStreams
import org.antlr.v4.runtime.CommonTokenStream
import org.junit.Test
import java.io.File

internal class CFGParserTest {
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
    fun testTerminals() {
        init("<S> -> /u/n/o")
        val ast = parse()
        assertEquals(
            CFG_Rules(null,
                CFG_Rule(
                    Nonterminal("<S>"), Rhs(
                        null, null, Str(
                            listOf(
                                Terminal("/u"),
                                Terminal("/n"),
                                Terminal("/o")
                            )
                        )
                    )
                )
            ), ast
        )
        clear()
    }

    @Test
    fun testNonterminals() {
        init("<S> -> <U><N><O>")
        val ast = parse()
        assertEquals(
            CFG_Rules( null,
                CFG_Rule(
                    Nonterminal("<S>"), Rhs(
                        null, null, Str(
                            listOf(
                                Nonterminal("<U>"),
                                Nonterminal("<N>"),
                                Nonterminal("<O>")
                            )
                        )
                    )
                )
            ), ast
        )
        clear()
    }

    @Test
    fun testTandN() {
        init("<S> -> <U>/n<O>")
        val ast = parse()
        assertEquals(
            CFG_Rules( null,
                CFG_Rule(
                    Nonterminal("<S>"), Rhs(
                        null, null, Str(
                            listOf(
                                Nonterminal("<U>"),
                                Terminal("/n"),
                                Nonterminal("<O>")
                            )
                        )
                    )
                )
            ), ast
        )
        clear()
    }

    @Test
    fun testDelimeters() {
        init("<S> -> <U> | /d | <C> ")
        val ast = parse()
        assertEquals(
            CFG_Rules(null,
                CFG_Rule(
                    Nonterminal("<S>"), Rhs(
                        Rhs(
                            Rhs(null, null, Str(listOf(Nonterminal("<U>")))), Delimeter(), Str(
                                listOf(Terminal("/d"))
                            )
                        ), Delimeter(), Str(
                            listOf(
                                Nonterminal("<C>")
                            )
                        )
                    )
                )
            ), ast
        )
        clear()
    }

    @Test
    fun testEpsilon() {
        init("<S> -> <eps>")
        val ast = parse()
        assertEquals(
            CFG_Rules(null,
                CFG_Rule(
                    Nonterminal("<S>"), Rhs(
                        null, null, Str(
                            listOf(
                                Epsilon()
                            )
                        )
                    )
                )
            ), ast
        )
        clear()
    }

    @Test
    fun test1() {
        init(
            """
            <S> -> /a<S>/b
            <S> -> <N>|<eps>
            
            <N> -> <S><S>
        """.trimIndent()
        )
        val ast = parse()
        val l = CFG_Rules(
            CFG_Rules(null,
                CFG_Rule(
                    Nonterminal("<S>"),
                    Rhs(
                        null, null, Str(
                            listOf(
                                Terminal("/a"),
                                Nonterminal("<S>"),
                                Terminal("/b")
                            )
                        )
                    )
                )
            ),
            CFG_Rule(
                Nonterminal("<S>"),
                Rhs(
                    Rhs(
                        null, null, Str(
                            listOf(
                                Nonterminal("<N>")
                            )
                        )
                    ), Delimeter(), Str(listOf(Epsilon()))
                )
            )
        )
        val r = CFG_Rule(
            Nonterminal("<N>"),
            Rhs(
                null, null, Str(
                    listOf(
                        Nonterminal("<S>"),
                        Nonterminal("<S>")
                    )
                )
            )
        )
        assertEquals(CFG_Rules(l, r), ast)
        clear()
    }

    @Test
    fun unit_test() {
        init(
            """
            <S> -> /a<S>/b
            <S> -> <N>|<eps>
            
            <N> -> <S><S>
        """.trimIndent()
        )
        val ast = parse()
        val l = CFG_Rules(
            CFG_Rules( null,
                CFG_Rule(
                    Nonterminal("<S>"),
                    Rhs(
                        null, null, Str(
                            listOf(
                                Terminal("/a"),
                                Nonterminal("<S>"),
                                Terminal("/b")
                            )
                        )
                    )
                )
            ),
            CFG_Rule(
                Nonterminal("<S>"),
                Rhs(
                    Rhs(
                        null, null, Str(
                            listOf(
                                Nonterminal("<N>")
                            )
                        )
                    ), Delimeter(), Str(listOf(Epsilon()))
                )
            )
        )
        val r = CFG_Rule(
            Nonterminal("<N>"),
            Rhs(
                null, null, Str(
                    listOf(
                        Nonterminal("<S>"),
                        Nonterminal("<S>")
                    )
                )
            )
        )
        assertEquals(CFG_Rules(l, r), ast)
        clear()
    }
}