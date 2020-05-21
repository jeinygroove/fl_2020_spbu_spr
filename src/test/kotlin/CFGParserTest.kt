import org.junit.jupiter.api.Test
import kotlin.test.assertEquals

internal class CFGParserTest {
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