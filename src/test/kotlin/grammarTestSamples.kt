object grammarTestSamples {
    fun correctBracketSequenceGrammar() = "<S> -> /a<S>/b<S> | <eps>"

    fun correctBracketSequenceAmbiguousGrammar() =
            "<S> -> <eps> | /a<S>/b | <S><S>"

    fun distinctNumberABGrammar() =
            "<S> -> <T> | <U>\n" +
            "<T> -> <V>/a<T> | <V>/a<V> | <T>/a<V>\n" +
            "<U> -> <V>/b<U> | <V>/b<V> | <U>/b<V>\n" +
            "<V> -> /a<V>/b<V> | /b<V>/a<V> | <eps>"

    // { a^n b^m c^m d^n | n,m > 0 } U { a^n b^n c^m d^m | n,m > 0 }
    fun inherentlyAmbiguousGrammar() =
            "<S> -> <F1> | <F2>\n" +
            "<F1> ->/a<I1>/d | /a<F1>/d\n" +
            "<I1> -> /b/c | /b<I1>/c\n" +
            "<F2> -> <L><R>\n" +
            "<L> -> /a/b | /a<L>/b\n" +
            "<R> -> /c/d | /c<R>/d"

    fun CYKWorstCaseGrammar() =
            "<S> -> <A><B> | <A><S1>\n" +
            "<S1> -> <S><B>\n" +
            "<A> -> /a\n" +
            "<B> -> /b"
}