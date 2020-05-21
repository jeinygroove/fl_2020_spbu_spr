class TreeNode(val name: String, val children: List<TreeNode?>) {
    override fun toString(): String {
        val buffer = StringBuilder(50)
        print(buffer, "", "")
        return buffer.toString()
    }

    private fun print(
        buffer: StringBuilder,
        prefix: String,
        childrenPrefix: String
    ) {
        buffer.append(prefix)
        buffer.append(name)
        buffer.append('\n')
        val it = children.reversed().iterator()
        while (it.hasNext()) {
            val next = it.next()
            if (it.hasNext()) {
                next?.print(buffer, "$childrenPrefix├── ", "$childrenPrefix│   ") ?: error("Incorrect tree")
            } else {
                next?.print(buffer, "$childrenPrefix└── ", "$childrenPrefix    ") ?: error("Incorrect tree")
            }
        }
    }

}