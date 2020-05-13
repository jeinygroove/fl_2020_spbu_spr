import java.util.*

typealias Rule = Pair<Nonterminal, Set<Str>>
typealias SimpleRule = Pair<Nonterminal, Str>

fun Rule.print() {
    println(this.first.str() + " -> " + this.second.toList().foldRight("", {rhs, str -> if (str != "") {rhs.str() + " | " + str} else {rhs.str()} }))
}

class CNFconverter(val start_nonterminal: Nonterminal, rules: CFG_Rules) {
    private var start = Nonterminal("S")
    private val cnf_rules = mutableSetOf<SimpleRule>()

    private fun rhsWalk(rhs: Rhs): Set<Str> {
        val result = mutableSetOf<Str>()
        var v = rhs
        result.add(v.r)
        while (v.l != null) {
            v = v.l!!
            result.add(v.r)
        }
        return result
    }

    private fun isReachable(rules: List<SimpleRule>, nonterminal: Nonterminal): Boolean {
        if (nonterminal == start)
            return true
        for (rule in rules) {
            if (rule.second.list.contains(nonterminal))
                return true
        }
        return false
    }

    fun toRules(): Set<Rule> {
        val rules = mutableSetOf<Rule>()
        val rulesForNonterminal = mutableMapOf<Nonterminal, MutableSet<Str>>()

        for (rule in cnf_rules) {
            if (!rulesForNonterminal.containsKey(rule.first))
                rulesForNonterminal[rule.first] = mutableSetOf(rule.second)
            else
                rulesForNonterminal[rule.first]!!.add(rule.second)
        }

        for ((k, v) in rulesForNonterminal) {
            rules.add(Rule(k, v))
        }

        return rules
    }

    // divide rules of kind <S> -> .. | .. | .. to separate rules
    private fun simplifyRules(rules: List<Rule>): Set<SimpleRule> {
        val result = mutableSetOf<SimpleRule>()
        for (rule in rules) {
            val simpleRules = mutableListOf<SimpleRule>()
            rule.second.forEach { rhs ->
                simpleRules.add(SimpleRule(rule.first, rhs))
            }
            result.addAll(simpleRules.distinct().toSet())
        }

        val new_res = mutableSetOf<SimpleRule>()

        //remove unreachable rules and epsilon in not unit rhs
        for (rule in result) {
            if (isReachable(result.toList(), rule.first)) {
                var rhs = rule.second
                if (rule.second.list.size != 1)
                    rhs = Str(rule.second.list.filter { it !is Epsilon })
                new_res.add(SimpleRule(rule.first, rhs))
            }
        }
        return new_res
    }

    init {
        start = start_nonterminal
        val tmp = mutableListOf<Rule>()
        var v = rules
        tmp.add(Pair(v.r.l, rhsWalk(v.r.r)))
        while (v.l != null) {
            v = v.l!!
            tmp.add(Pair(v.r.l, rhsWalk(v.r.r)))
        }

        cnf_rules.addAll(simplifyRules(tmp))

        // new start if needed
        val newRule: SimpleRule
        if (cnf_rules.any { rule -> rule.second.list.contains(start) }) {
            val accountant = NodeAccountant()
            accountant.consume(cnf_rules)
            val newStart = accountant.freshNonTerminal()
            newRule = SimpleRule(newStart, Str(listOf(start)))
            start = newStart
            cnf_rules.add(newRule)
        }
    }

    class NodeAccountant {
        val nonTerminals = mutableSetOf<Nonterminal>()
        private var index = 0

        fun consume(rule: SimpleRule) {
            nonTerminals.add(rule.first)
            nonTerminals.addAll(rule.second.list.filterIsInstance<Nonterminal>())
        }

        fun consume(rules: List<SimpleRule>) {
            rules.forEach { consume(it) }
        }

        fun consume(rules: Set<SimpleRule>) {
            rules.forEach { consume(it) }
        }

        fun freshNonTerminal(): Nonterminal {
            while (true) {
                val name = "<S$index>"
                if (!nonTerminals.contains(Nonterminal(name))) {
                    nonTerminals.add(Nonterminal(name))
                    index++
                    return Nonterminal(name)
                }
                index++
            }
        }
    }

    // removes long rules (rules, where rhs length > 2)
    fun reduceLongRules(): Set<SimpleRule> {
        val accountant = NodeAccountant()
        accountant.consume(cnf_rules)
        val newRules = mutableSetOf<SimpleRule>()
        cnf_rules.forEach { rule ->
            var lhs = rule.first
            val rhs = rule.second.list.toMutableList()
            if (rhs.size > 2) {
                var index = rhs.size - 1
                while (index > 1) {
                    val l = accountant.freshNonTerminal()
                    rhs.dropLast(1)
                    newRules.add(SimpleRule(lhs, Str(listOf(l, rhs.last()))))
                    lhs = l
                    rhs.removeAt(index)
                    index--
                    if (index == 1)
                        newRules.add(SimpleRule(lhs, Str(rhs)))
                }
            } else {
                newRules.add(rule)
            }
        }
        cnf_rules.clear()
        cnf_rules.addAll(newRules)
        assert(hasOnlySmallRules())
        return cnf_rules
    }

    fun hasOnlySmallRules(): Boolean = cnf_rules.all { it.second.list.size <= 2 }

    fun getEpsilonProducers(): Set<Nonterminal> {
        val epsilonProducers = mutableSetOf<Nonterminal>()

        cnf_rules.forEach { rule ->
            if (rule.second.list.size == 1 && rule.second.list[0] is Epsilon)
                epsilonProducers.add(rule.first)
        }

        var changed = true
        while (changed) {
            changed = false
            cnf_rules.forEach { rule ->
                if (!epsilonProducers.contains(rule.first) && rule.second.list.all { epsilonProducers.contains(it) }) {
                    epsilonProducers.add(rule.first)
                    changed = true
                }
            }
        }

        return epsilonProducers
    }

    fun reduceEpsilonRules(): Set<SimpleRule> {
        if (!hasOnlySmallRules())
            reduceLongRules()

        val epsilonProducers = getEpsilonProducers()
        val newRules = mutableListOf<SimpleRule>()

        cnf_rules.forEach { (lhs, rhs) ->
            if (!(rhs.list.size == 1 && rhs.list[0] is Epsilon)) { // skip all eps-rules
                if (rhs.list.size == 1) {
                    newRules.add(SimpleRule(lhs, rhs))
                } else {
                    assert(rhs.list.size == 2)
                    if (epsilonProducers.contains(rhs.list[0])) {
                        newRules.add(SimpleRule(lhs, Str(listOf(rhs.list[1]))))
                    }
                    if (epsilonProducers.contains(rhs.list[1])) {
                        newRules.add(SimpleRule(lhs, Str(listOf(rhs.list[0]))))
                    }
                    newRules.add(SimpleRule(lhs, rhs))
                }
            }
        }

        if (epsilonProducers.contains(start)) {
            val accountant = NodeAccountant()
            accountant.consume(cnf_rules)
            val newStart = accountant.freshNonTerminal()
            newRules.add(SimpleRule(newStart, Str(listOf(Epsilon()))))
            newRules.add(SimpleRule(newStart, Str(listOf(start))))
            start = newStart
        }

        // remove eps rules and rules of kind Z -> Z
        newRules.removeAll { rule -> (rule.first != start && epsilonProducers.contains(rule.first) &&
                (rule.second.list.size == 1 && rule.second.list[0] is Epsilon))
                || (rule.second.list.size == 1 && rule.second.list[0] is Nonterminal && rule.first == rule.second.list[0])}

        cnf_rules.clear()
        cnf_rules.addAll(newRules)
        assert(isEpsilonReduced())
        return cnf_rules
    }

    fun isEpsilonReduced(): Boolean {
        return hasOnlySmallRules() && cnf_rules.all { rule ->
            !(rule.second.list.size == 1 && rule.second.list[0] is Epsilon) || (rule.first == start)
        }
    }

    private fun SimpleRule.isUnit(): Boolean {
        return (this.second.list.size == 1 && this.second.list[0] is Nonterminal)
    }

    fun reduceUnitRules(): Set<SimpleRule> {
        if (!isEpsilonReduced())
            reduceEpsilonRules()

        val unitRules = mutableMapOf<Nonterminal, MutableList<SimpleRule>>()
        val nonUnitRules = mutableMapOf<Nonterminal, MutableList<SimpleRule>>()
        val newRules = mutableListOf<SimpleRule>()

        //find all unit rules
        cnf_rules.forEach { rule ->
            if (rule.isUnit()) {
                unitRules.getOrPut(rule.first, { mutableListOf() }).add(rule)
            } else {
                nonUnitRules.getOrPut(rule.first, { mutableListOf() }).add(rule)
            }
        }

        val accountant = NodeAccountant()
        accountant.consume(cnf_rules)
        accountant.nonTerminals.forEach { origin ->
            val queue = ArrayDeque<Nonterminal>()
            val visited = hashSetOf<Nonterminal>()
            queue.add(origin)
            visited.add(origin)
            while (queue.isNotEmpty()) {
                val v = queue.pop()
                nonUnitRules[v]?.forEach {
                    newRules.add(SimpleRule(origin, it.second))
                }
                unitRules[v]?.forEach {
                    if (!visited.contains(it.second.list[0])) {
                        queue.add(it.second.list[0] as Nonterminal)
                        visited.add(it.second.list[0] as Nonterminal)
                    }
                }
            }
        }

        //delete unit rules
        cnf_rules.removeAll { rule -> rule.isUnit() }
        cnf_rules.addAll(newRules)
        assert(isUnitReduced())
        return cnf_rules
    }

    fun isUnitReduced(): Boolean {
        return isEpsilonReduced() && cnf_rules.all { !it.isUnit() }
    }

    private fun SimpleRule.containsTerminals(): Boolean {
        return second.list.all { it is Terminal || it is Epsilon }
    }

    fun reduceNonGeneratingRules(): Set<SimpleRule> {
        if (!isUnitReduced())
            reduceUnitRules()

        val generatingNonterminals = mutableSetOf<Nonterminal>()

        cnf_rules.forEach { rule ->
            if (rule.containsTerminals())
                generatingNonterminals.add(rule.first)
        }

        var changed = true
        while (changed) {
            changed = false
            cnf_rules.forEach { rule ->
                if (!generatingNonterminals.contains(rule.first) && rule.second.list.all { it !is Nonterminal || generatingNonterminals.contains(it) }) {
                    changed = true
                    generatingNonterminals.add(rule.first)
                }
            }
        }

        cnf_rules.removeAll { rule ->
            !(generatingNonterminals.contains(rule.first) && (rule.second.list.all {
                it !is Nonterminal || generatingNonterminals.contains(
                    it
                )
            }))
        }

        if (cnf_rules.isEmpty() || !cnf_rules.any { it.first == start })
            throw EmptyLanguageException()
        return cnf_rules
    }

    fun reduceUnreachable(): Set<SimpleRule> {
        val reachableNonterminal = mutableSetOf(start)

        var changed = true
        while (changed) {
            changed = false
            cnf_rules.forEach { rule ->
                if (reachableNonterminal.contains(rule.first)) {
                    rule.second.list.forEach {
                        if (it is Nonterminal && !reachableNonterminal.contains(it)) {
                            reachableNonterminal.add(it); changed = true
                        }
                    }
                }
            }
        }

        cnf_rules.removeAll { rule -> !reachableNonterminal.contains(rule.first) }
        return cnf_rules

    }

    fun reduceLongTerminalRules(): Set<SimpleRule> {
        if (!isEpsilonReduced()) {
            reduceEpsilonRules()
            reduceNonGeneratingRules()
            reduceUnreachable()
        }

        val accountant = NodeAccountant()
        accountant.consume(cnf_rules)
        val newRules = mutableListOf<SimpleRule>()
        val newRulesWithTerminals = mutableMapOf<Terminal, SimpleRule>()
        //add new rules
        cnf_rules.forEach { rule ->
            val rhs = rule.second.list
            if (rhs.size == 2 && rhs.any { it !is Nonterminal }) {
                if (rhs.all { it !is Nonterminal }) {
                    val fresh1: Nonterminal
                    val fresh2: Nonterminal
                    if (newRulesWithTerminals.containsKey(rhs[0] as Terminal)) {
                        fresh1 = newRulesWithTerminals[rhs[0] as Terminal]!!.first
                    } else {
                        fresh1 = accountant.freshNonTerminal()
                        newRulesWithTerminals[rhs[0] as Terminal] = SimpleRule(fresh1, Str(listOf(rhs[0])))
                    }
                    if (newRulesWithTerminals.containsKey(rhs[1] as Terminal)) {
                        fresh2 = newRulesWithTerminals[rhs[1] as Terminal]!!.first
                    } else {
                        fresh2 = accountant.freshNonTerminal()
                        newRulesWithTerminals[rhs[1] as Terminal] = SimpleRule(fresh2, Str(listOf(rhs[1])))
                    }
                    newRules.add(SimpleRule(rule.first, Str(listOf(fresh1, fresh2))))
                } else if (rhs[0] !is Nonterminal) {
                    val fresh: Nonterminal
                    if (newRulesWithTerminals.containsKey(rhs[0] as Terminal)) {
                        fresh = newRulesWithTerminals[rhs[0] as Terminal]!!.first
                    } else {
                        fresh = accountant.freshNonTerminal()
                        newRulesWithTerminals[rhs[0] as Terminal] = SimpleRule(fresh, Str(listOf(rhs[0])))
                    }
                    newRules.add(SimpleRule(rule.first, Str(listOf(fresh, rhs[1]))))
                } else /* rhs[1] is terminal */ {
                    val fresh: Nonterminal
                    if (newRulesWithTerminals.containsKey(rhs[1] as Terminal)) {
                        fresh = newRulesWithTerminals[rhs[1] as Terminal]!!.first
                    } else {
                        fresh = accountant.freshNonTerminal()
                        newRulesWithTerminals[rhs[1] as Terminal] = SimpleRule(fresh, Str(listOf(rhs[1])))
                    }
                    newRules.add(SimpleRule(rule.first, Str(listOf(rhs[0], fresh))))
                }
            }
        }



        //remove long terminal rules
        cnf_rules.removeAll { rule ->
            rule.second.list.size == 2 && rule.second.list.any { it !is Nonterminal }
        }

        // remove duplicates (<A> -> /a & <AA> -> /a and there's no other rules for <A> or <AA>)


        cnf_rules.addAll(newRules)
        newRulesWithTerminals.forEach { (_, rule) -> cnf_rules.add(rule) }

        return cnf_rules
    }

    fun toChomskyNormalForm(): Set<SimpleRule> {
        reduceLongRules()
        reduceEpsilonRules()
        reduceUnitRules()
        reduceNonGeneratingRules()
        reduceUnreachable()
        reduceLongTerminalRules()
        return cnf_rules
    }

    fun toChomskyNormalFormWithSteps() {
        //0. init
        println("0. Initial grammar with the new start (if needed):")
        println("Start nonterminal: ${getStart().str()}")
        println()
        toRules().forEach { it.print() }

        println()

        //1. long rules
        println("1. Reduced long rules:")
        reduceLongRules()
        println("Start nonterminal: ${getStart().str()}")
        println()
        toRules().forEach { it.print() }

        println()

        //2. eps rules
        println("2. Reduced epsilon rules:")
        reduceEpsilonRules()
        println("Start nonterminal: ${getStart().str()}")
        println()
        toRules().forEach { it.print() }

        println()

        //3. unit rules
        println("3. Reduced unit rules:")
        reduceUnitRules()
        println("Start nonterminal: ${getStart().str()}")
        println()
        toRules().forEach { it.print() }

        println()

        //4. unnecessary rules
        println("4. Reduced nongenerating and unreachable rules")
        reduceNonGeneratingRules()
        reduceUnreachable()
        println("Start nonterminal: ${getStart().str()}")
        println()
        toRules().forEach { it.print() }

        println()

        //5. long terminal rules
        println("5. Reduced long terminal rules")
        reduceLongTerminalRules()
        println("Start nonterminal: ${getStart().str()}")
        println()
        toRules().forEach { it.print() }
    }

    fun compactRules(): Set<Rule> {
        return toRules()
    }

    fun getStart(): Nonterminal {
        return start
    }
}
