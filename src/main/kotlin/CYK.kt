open class CYKItem(val A: Nonterminal)

class CYKSimpleItem(A: Nonterminal, val t: EpsilonOrTerminal, val k: Int): CYKItem(A)
class CYKComplexItem(A: Nonterminal, val B: Nonterminal, val C: Nonterminal, val k: Int): CYKItem(A)

private fun ceilHas(ceil: HashSet<CYKItem>, n: Nonterminal): Boolean {
    return ceil.any { item -> item.A == n}
}

private fun ceilFind(ceil: HashSet<CYKItem>, n: Nonterminal): CYKItem? {
    return ceil.find { item -> item.A == n}
}

fun CYKQuery(start: Nonterminal, cnfGrammar: Set<SimpleRule>, word: String): TreeNode? {
    var root = TreeNode(start.str(), listOf())

    if (word.isEmpty()) {
        val rule = cnfGrammar.find { rule -> rule.first == start && rule.second == Str(listOf(Epsilon())) }
        return if (rule == null)
            null
        else {
            root = TreeNode(start.str(), listOf(TreeNode(Epsilon().str(), listOf())))
            root
        }
    }

    val query = word.split("/").drop(1)

    if (!isInCNF(start, cnfGrammar))
        throw NotInCNFException("Impossible to run CYK, because grammar isn't in CNF")

    val grammarTerminals = mutableSetOf<String>()
    cnfGrammar.forEach { rule ->
        rule.second.list.forEach { term ->
            if (term is Terminal)
                grammarTerminals.add(term.str().drop(1))
        }
    }

    if (query.any { term -> !(grammarTerminals.contains(term))})
        throw TerminalsNotFromAlphabet("Word that you want to generate from this grammar contains terminals not from it")

    val dp = Array<Array<HashSet<CYKItem>>>(query.size) { Array(query.size + 1) { hashSetOf<CYKItem>() } }

    val symRules = cnfGrammar.filter { it.second.list.all { term -> !term.isNonterminal() } && !it.second.list.contains(Epsilon()) }
    val concatRules = cnfGrammar.filter { it.second.list.any { term -> term.isNonterminal() } }

    symRules.forEach { rule ->
        val sym = rule.second.list[0]
        for (i in query.indices) {
            if ("/" + query[i] == sym.str())
                dp[i][i + 1].add(CYKSimpleItem(rule.first, EpsilonOrTerminal(sym.str()), i))
        }
    }

    for (len in 2..query.size) {
        for (left in 0..(query.size - len)) {
            val right = left + len
            concatRules.forEach { rule ->
                val n1 = rule.second.list[0]
                val n2 = rule.second.list[1]
                for (border in left until right) {
                    if (ceilHas(dp[left][border], n1 as Nonterminal) && ceilHas(dp[border][right], n2 as Nonterminal)) {
                        dp[left][right].add(CYKComplexItem(rule.first, n1, n2, border))
                        break
                    }
                }
            }
        }
    }

    if(!ceilHas(dp[0][query.size], start))
        return null

    if (query.size == 1) {
        symRules.find { rule -> rule.first == start && (rule.second.list[0].str() == "/$query") }
        root = TreeNode(root.name, listOf(TreeNode(query[0], listOf())))
        return root
    }

    return makeTree(root, dp, 0, query.size)

}

fun makeTree(node: TreeNode, dp: Array<Array<HashSet<CYKItem>>>, left: Int, right: Int): TreeNode? {
    val item = ceilFind(dp[left][right], Nonterminal(node.name))
    return if (item == null) {
        null
    } else {
        return if (item is CYKComplexItem) {
            TreeNode(node.name, listOf(makeTree(TreeNode(item.B.str(), listOf()), dp, left, item.k),
                makeTree(TreeNode(item.C.str(), listOf()), dp, item.k, right)))
        } else if (item is CYKSimpleItem) {
            TreeNode(node.name, listOf(TreeNode(item.t.str(), listOf())))
        } else {
            null
        }
    }
}