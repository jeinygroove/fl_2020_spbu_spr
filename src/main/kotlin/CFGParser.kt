import org.antlr.v4.runtime.CharStream
import org.antlr.v4.runtime.CharStreams
import org.antlr.v4.runtime.CommonTokenStream
import java.nio.file.NoSuchFileException

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
                println("Your grammar:")
                ast.print()
                println()
            } catch (e: ParserException) {
                println("Parser error:" + e.message)
            }
        }
    }
}