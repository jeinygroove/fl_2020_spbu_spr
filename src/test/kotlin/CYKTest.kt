import org.junit.jupiter.api.Test
import kotlin.test.assertEquals
import kotlin.test.assertFailsWith

class CYKTest {
    @Test
    fun testBrackets() {
        init(grammarTestSamples.correctBracketSequenceGrammar())
        val ast = parse()
        val converter = CNFconverter(Nonterminal("<S>"), ast as CFG_Rules)
        converter.toChomskyNormalForm()
        val result = converter.toChomskyNormalForm()
        val start = converter.getStart()
        assert(CYKQuery(start, result, "") != null)
        assert(CYKQuery(start, result, "/a/b") != null)
        assert(CYKQuery(start, result,"/a/a/b/b") != null)
        assert(CYKQuery(start, result,"/a/a/b/a/b/b/a/b") != null)

        assert(CYKQuery(start, result,"/a/a/a/a/a") == null)
        assert(CYKQuery(start, result, "/a/a/b/a/b/b/a/b/a") == null)
        assertFailsWith(TerminalsNotFromAlphabet::class) {
            CYKQuery(start, result, "/a/b/c") == null
        }
        assert(CYKQuery(start, result, "/b/a/b/a") == null)
    }

    @Test
    fun testBracketsAmbiguous() {
        init(grammarTestSamples.correctBracketSequenceAmbiguousGrammar())
        val ast = parse()
        val converter = CNFconverter(Nonterminal("<S>"), ast as CFG_Rules)
        converter.toChomskyNormalForm()
        val result = converter.toChomskyNormalForm()
        val start = converter.getStart()
        assert(CYKQuery(start, result, "") != null)
        assert(CYKQuery(start, result, "/a/b") != null)
        assert(CYKQuery(start, result,"/a/a/b/b") != null)
        assert(CYKQuery(start, result,"/a/a/b/a/b/b/a/b") != null)

        assert(CYKQuery(start, result,"/a/a/a/a/a") == null)
        assert(CYKQuery(start, result, "/a/a/b/a/b/b/a/b/a") == null)
        assertFailsWith(TerminalsNotFromAlphabet::class) {
            CYKQuery(start, result, "/a/b/c")
        }
        assert(CYKQuery(start, result, "/b/a/b/a") == null)
    }

    @Test
    fun testDistinctABs() {
        init(grammarTestSamples.distinctNumberABGrammar())
        val ast = parse()
        val converter = CNFconverter(Nonterminal("<S>"), ast as CFG_Rules)
        converter.toChomskyNormalForm()
        val result = converter.toChomskyNormalForm()
        val start = converter.getStart()
        assert(CYKQuery(start, result, "/a") != null)
        assert(CYKQuery(start, result, "/b") != null)
        assert(CYKQuery(start, result,"/a/a/a/b/b") != null)
        assert(CYKQuery(start, result,"/b/a/b/a/a/a/a/a/b/a/b/a/b/a/a/a/a/a") != null)

        assert(CYKQuery(start, result,"") == null)
        assert(CYKQuery(start, result, "/a/b") == null)
        assert(CYKQuery(start, result,"/b/a") == null)
        assert(CYKQuery(start, result,"/a/b/b/a") == null)
        assert(CYKQuery(start, result, "/b/a/b/a/b/a/a/a/b/b") == null)
    }

    @Test
    fun testInherentlyAG() {
        init(grammarTestSamples.inherentlyAmbiguousGrammar())
        val ast = parse()
        val converter = CNFconverter(Nonterminal("<S>"), ast as CFG_Rules)
        converter.toChomskyNormalForm()
        val result = converter.toChomskyNormalForm()
        val start = converter.getStart()
        assert(CYKQuery(start, result, "/a/b/c/d") != null)
        assert(CYKQuery(start, result, "/a/b/b/c/c/d") != null)
        assert(CYKQuery(start, result, "/a/a/b/c/d/d") != null)
        assert(CYKQuery(start, result, "/a/a/a/b/b/c/c/d/d/d") != null)
        assert(CYKQuery(start, result,"/a/a/b/b/c/d") != null)
        assert(CYKQuery(start, result,"/a/a/b/b/c/c/c/d/d/d") != null)

        assert(CYKQuery(start, result,"") == null)
        assert(CYKQuery(start, result, "/a/b") == null)
        assert(CYKQuery(start, result,"/a/d") == null)
        assert(CYKQuery(start, result,"/b/c") == null)
        assert(CYKQuery(start, result, "/a/a/a/b/b/c/d/d") == null)
    }

    @Test
    fun testWorstCase() {
        init(grammarTestSamples.CYKWorstCaseGrammar())
        val ast = parse()
        val converter = CNFconverter(Nonterminal("<S>"), ast as CFG_Rules)
        converter.toChomskyNormalForm()
        val result = converter.toChomskyNormalForm()
        val start = converter.getStart()
        assert(CYKQuery(start, result, "/a/b") != null)
        assert(CYKQuery(start, result, "/a/a/b/b") != null)
        assert(CYKQuery(start, result, "/a/a/a/b/b/b") != null)

        assert(CYKQuery(start, result,"") == null)
        assert(CYKQuery(start, result, "/a/b/b") == null)
        assert(CYKQuery(start, result,"/a/a/b") == null)
        assert(CYKQuery(start, result,"/a/b/a/b") == null)
        assert(CYKQuery(start, result, "/a/a/a/b/a/b/b/b") == null)
    }
}