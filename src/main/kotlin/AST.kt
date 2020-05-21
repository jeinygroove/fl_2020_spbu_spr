import org.antlr.v4.runtime.misc.ParseCancellationException

sealed class AST

data class CFG_Rules(val l: CFG_Rules?, val r: CFG_Rule): AST()

data class CFG_Rule(val l: Nonterminal, val r: Rhs): AST()

data class Rhs(val l: Rhs?, val del: Delimeter?, val r: Str): AST()

data class Str(val list: List<AST>): AST()

data class Nonterminal(val name: String): AST()

data class Terminal(val symbol: String): AST()

data class Delimeter(val del: String = "|"): AST()

data class Epsilon(val symbol: String = "\u03b5"): AST()

data class EpsilonOrTerminal(val symbol: String): AST()

// Print methods for AST
fun AST.print() {
    when(this) {
        is CFG_Rules -> print()
        is CFG_Rule -> print()
        is Rhs -> print()
        is Str -> print()
        is Nonterminal -> print()
        is Terminal -> print()
        is Epsilon -> print()
        is Delimeter -> print()
        is EpsilonOrTerminal -> print()
    }
}

fun CFG_Rules.print() = print(this.str())

fun CFG_Rule.print() = print(this.str())

fun Rhs.print() = print(this.str())

fun Str.print() = print(this.str())

fun Nonterminal.print() = print(this.str())

fun Terminal.print() = print(this.str())

fun Epsilon.print() = print(this.str())

fun Delimeter.print() = print(this.str())

fun EpsilonOrTerminal.print() = print(this.str())

fun AST.str(): String =
    when(this) {
        is CFG_Rules -> str()
        is CFG_Rule -> str()
        is Rhs -> str()
        is Str -> str()
        is Nonterminal -> str()
        is Terminal -> str()
        is Epsilon -> str()
        is Delimeter -> str()
        is EpsilonOrTerminal -> str()
    }

fun CFG_Rules.str(): String {
    val ans = l?.str().orEmpty()
    return ans + "\n" + r.str()
}

fun CFG_Rule.str(): String {
    return l.str() + " -> " + r.str()
}

fun Rhs.str(): String {
    if (l == null)
        return r.str()
    return l.str() + " | " + r.str()
}

fun Str.str(): String {
    return list.foldRight("", {x, y -> x.str() + y})
}

fun Nonterminal.str(): String {
    return name
}

fun Terminal.str(): String {
    return symbol
}

fun Epsilon.str(): String {
    return "\u03b5"
}

fun Delimeter.str(): String {
    return "|"
}

fun EpsilonOrTerminal.str(): String {
    return symbol
}

class ParserException(message: String): ParseCancellationException(message)
class EmptyLanguageException(): Exception()
class NotInCNFException(message: String): Exception()
class TerminalsNotFromAlphabet(message: String): Exception()

fun AST.isEpsilon(): Boolean {
    return this is Epsilon
}

fun AST.isNonterminal(): Boolean {
    return this is Nonterminal
}