import org.antlr.v4.runtime.CharStream
import org.antlr.v4.runtime.CharStreams
import org.antlr.v4.runtime.CommonTokenStream
import java.nio.file.NoSuchFileException

fun parseGrammar(charStream: CharStream): AST {
    val lexer = GrammarLexer(charStream)
    lexer.removeErrorListeners();
    lexer.addErrorListener(GrammarErrorListener.INSTANCE);

    val commonTokenStream = CommonTokenStream(lexer)

    val parser = GrammarParser(commonTokenStream)
    parser.removeErrorListeners();
    parser.addErrorListener(GrammarErrorListener.INSTANCE);

    val parseTree = parser.cfg_rules()
    val visitor = CFGVisitor()
    val ast = visitor.visit(parseTree)
    return ast
}

object CFGParser {
    @JvmStatic
    fun main(args: Array<String>) {
        while (true) {
            println("Print path to your file with grammar. For exit type: \'exit\'")
            val line = readLine() ?: "exit"
            if (line == "exit") {
                break
            }
            val charStream: CharStream?
            try {
                charStream = CharStreams.fromFileName(line)
            } catch (e: NoSuchFileException) {
                println("No such file!")
                continue
            }
            try {
                while(true) {
                    println("What do you want to do with it (1 - parse, 2 - convert to CNF, 3 - CYK)? (1/2/3)")
                    val cmd = readLine()
                    if (cmd == "1") {
                        val ast = parseGrammar(charStream)
                        println("Your grammar:")
                        ast.print()
                        println()
                        break
                    } else if (cmd == "2") {
                        val ast = parseGrammar(charStream)
                        val converter = CNFconverter(Nonterminal("<S>"), ast as CFG_Rules)
                        converter.toChomskyNormalFormWithSteps()
                        break
                    } else if (cmd == "3") {
                        println("Print word that you want to make from this grammar, make sure that terminals in this word have " +
                                "`/` symbol, e.g. /a/b and /ab are different words: ")
                        val query = readLine() ?: ""
                        val ast = parseGrammar(charStream)
                        val converter = CNFconverter(Nonterminal("<S>"), ast as CFG_Rules)
                        val rules = converter.toChomskyNormalForm()
                        val start = converter.getStart()
                        try {
                            val root = CYKQuery(start, rules, query)
                            if (root == null)
                                println("Word `${query}` is impossible to generate with this grammar")
                            else {
                                println("Grammar in CNF. Start: $start")
                                println(converter.toRules().forEach { it.print() })
                                println("Tree: ")
                                println(root.toString())
                            }
                        } catch (e: Exception) {
                            println(e.message)
                        }
                        break
                    } else {
                        println("Please, print 1 or 2.")
                    }
                }
            } catch (e: ParserException) {
                println("Parser error:" + e.message)
            }
        }
    }
}