package io.github.andjsrk.v8.parsing

private val keywords = Token.values().filter { it.isKeyword }
val keywordMap =
    keywords.associateBy { it.string!! }
