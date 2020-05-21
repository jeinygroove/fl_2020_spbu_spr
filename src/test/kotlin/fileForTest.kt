import org.antlr.v4.runtime.CharStreams
import org.antlr.v4.runtime.CommonTokenStream
import java.io.File

fun init(input: String) {
    val filename = "tmp.txt"
    val file = File(filename)
    file.createNewFile()
    file.writeText(input)
}

fun clear() {
    val filename = "tmp.txt"
    val file = File(filename)
    file.deleteOnExit()
}

fun parse(filename: String = "tmp.txt"): AST {
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