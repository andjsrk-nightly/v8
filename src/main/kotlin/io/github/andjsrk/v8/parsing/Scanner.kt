package io.github.andjsrk.v8.parsing

class Scanner(sourceText: String) {
    class Source(val text: String) {
        var pos = 0
        fun peek(): Char =
            when {
                pos <= text.length -> text[pos]
                else -> endOfInput
            }
        fun advance(): Char {
            val peeked = peek()
            pos++
            return peeked
        }
        fun advanceUntil(check: (Char) -> Boolean): Char {
            val subStringIndex = text.subSequence(pos, text.length).indexOfFirst(check)
            pos += subStringIndex
            return text[pos]
        }
        fun back() {
            pos--
        }
        companion object {
            const val endOfInput = (-1).toChar()
        }
    }
    data class Location(var start: Int = 0, var end: Int = 0) {
        val length get() = end - start
        val isValid get() = start in 0..end
        companion object {
            val INVALID = Location(-1, 0)
        }
    }
    data class TokenDesc(
        var token: Token = Token.UNINITIALIZED,
        var location: Location = Location(0, 0),
    ) {
        var literalChars = ""
        var rawLiteralChars = ""
        var afterLineTerminator = false
    }
    private val source = Source(sourceText)
    private val hasParserError = ResettableState(false)
    private fun activateParserErrorState() {
        if (hasParserError.value) return

        source.pos = source.text.length
        hasParserError.value = true
        nextChar = Source.endOfInput
        arrayOf(current, next, nextNext).forEach {
            if (it.token != Token.UNINITIALIZED) it.token = Token.ILLEGAL
        }
    }
    /**
     * @see `c0_`
     */
    private var nextChar: Char = '\u0000'
    private var current = TokenDesc()
    private val currentToken get() =
        current.token
    private var next = TokenDesc()
    private val nextToken get() =
        next.token
    private var nextNext = TokenDesc()
    private val nextNextToken get() =
        nextNext.token
    private fun addRawLiteralChar(char: Char) {
        next.rawLiteralChars += char
    }
    private fun addLiteralChar(char: Char) {
        next.literalChars += char
    }
    fun peek() = source.peek()
    fun advance(captureRaw: Boolean = false) {
        if (captureRaw) {
            addRawLiteralChar(nextChar)
        }
        nextChar = source.advance()
    }
    fun advanceUntil(check: (Char) -> Boolean) {
        nextChar = source.advanceUntil(check)
    }
    fun skipWhitespace(): Token {
        if (nextChar.isAsciiWhitespace.not()) return Token.ILLEGAL

        if (next.afterLineTerminator.not() && nextChar.isLineTerminator) {
            next.afterLineTerminator = true
        }

        var hint = ' '
        advanceUntil { char ->
            if (char == hint) return@advanceUntil false

            if (char.isAsciiWhitespace || char.isAsciiLineTerminator) {
                if (next.afterLineTerminator.not() && nextChar.isLineTerminator) {
                    next.afterLineTerminator = true
                }
                hint = nextChar
                return@advanceUntil false
            }
            true
        }

        return Token.WHITESPACE
    }
    fun skipSingleLineComment(): Token {

    }
    fun skipMultiLineComment(): Token {

    }
    fun scanString(): Token {

    }
    fun scanTemplateSpan(): Token {

    }
    fun scanNumber(): Token {

    }
    fun scanIdentifierOrKeyword(): Token {
        var canBeKeyword = true

        if (nextChar <= asciiMax) {

        }
    }
    fun scanPrivateName(): Token {

    }
    fun scanSingleToken(): Token {
        var token = Token.UNINITIALIZED

        do {
            next.location.start = source.pos

            if (nextChar <= asciiMax) {
                token = getOneCharToken(nextChar)

                when (token) {
                    Token.LEFT_PAREN, Token.RIGHT_PAREN,
                    Token.LEFT_BRACE, Token.RIGHT_BRACE,
                    Token.LEFT_BRACK, Token.RIGHT_BRACK,
                    Token.COLON, Token.SEMICOLON, Token.COMMA, Token.BIT_NOT, Token.ILLEGAL ->
                        return token.alsoAdvance()
                    Token.CONDITIONAL -> {
                        // ? ?. ?? ??=
                        advance()
                        when (nextChar) {
                            '.' -> Token.QUESTION_PERIOD.alsoAdvance()
                            '?' -> selectIf('=', Token.ASSIGN_NULLISH, Token.NULLISH)
                            else -> Token.CONDITIONAL
                        }
                    }
                    Token.STRING -> scanString()
                    Token.LT -> {
                        // < <= << <<=
                        advance()
                        when (nextChar) {
                            '=' -> Token.LT_EQ.alsoAdvance()
                            '<' -> selectIf('=', Token.ASSIGN_SHL, Token.SHL)
                            else -> Token.LT
                        }
                    }
                    Token.GT -> {
                        // > >= >> >>= >>> >>>=
                        advance()
                        when (nextChar) {
                            '=' -> Token.GT_EQ.alsoAdvance()
                            '>' -> {
                                advance()
                                when (nextChar) {
                                    '=' -> Token.ASSIGN_SAR.alsoAdvance()
                                    '>' -> selectIf('=', Token.ASSIGN_SHR, Token.SHR)
                                    else -> Token.SAR
                                }
                            }
                            else -> Token.GT
                        }
                    }
                    Token.ASSIGN -> {
                        // = == === =>
                        advance()
                        when (nextChar) {
                            '=' -> selectIf('=', Token.EQ_STRICT, Token.EQ)
                            '>' -> Token.ARROW.alsoAdvance()
                            else -> Token.ASSIGN
                        }
                    }
                    Token.NOT -> {
                        // ! != !==
                        advance()
                        when (nextChar) {
                            '=' -> selectIf('=', Token.NOT_EQ_STRICT, Token.NOT_EQ)
                            else -> Token.NOT
                        }
                    }
                    Token.ADD -> {
                        // + ++ +=
                        advance()
                        when (nextChar) {
                            '+' -> Token.INC.alsoAdvance()
                            '=' -> Token.ASSIGN_ADD.alsoAdvance()
                            else -> Token.ADD
                        }
                    }
                    Token.SUB -> {
                        // - -- -=
                        advance()
                        when (nextChar) {
                            '-' -> Token.DEC.alsoAdvance()
                            '=' -> Token.ASSIGN_SUB.alsoAdvance()
                            else -> Token.SUB
                        }
                    }
                    Token.MUL -> {
                        // * *= ** **=
                        advance()
                        when (nextChar) {
                            '*' -> selectIf('=', Token.ASSIGN_EXP, Token.EXP)
                            '=' -> Token.ASSIGN_MUL.alsoAdvance()
                            else -> Token.MUL
                        }
                    }
                    // % %=
                    Token.MOD -> selectIf('=', Token.ASSIGN_MOD, Token.MOD)
                    Token.DIV -> {
                        // / // /* /=
                        advance()
                        when (nextChar) {
                            '/' -> {
                                token = skipSingleLineComment()
                                continue
                            }
                            '*' -> {
                                token = skipMultiLineComment()
                                continue
                            }
                            '=' -> Token.ASSIGN_DIV.alsoAdvance()
                            else -> Token.DIV
                        }
                    }
                    Token.BIT_AND -> {
                        // & &= && &&=
                        advance()
                        when (nextChar) {
                            '&' -> selectIf('=', Token.ASSIGN_AND, Token.AND)
                            '=' -> Token.ASSIGN_BIT_AND.alsoAdvance()
                            else -> Token.BIT_AND
                        }
                    }
                    Token.BIT_OR -> {
                        // | |= || ||=
                        advance()
                        when (nextChar) {
                            '|' -> selectIf('=', Token.ASSIGN_OR, Token.OR)
                            '=' -> Token.ASSIGN_BIT_OR
                            else -> Token.BIT_OR
                        }
                    }
                    // ^ ^=
                    Token.BIT_XOR -> selectIf('=', Token.ASSIGN_BIT_XOR, Token.BIT_XOR)
                    Token.PERIOD -> {
                        // . ...
                        advance()
                        return if (nextChar == '.' && peek() == '.') {
                            advance()
                            advance()
                            Token.ELLIPSIS
                        } else Token.PERIOD
                    }
                    Token.TEMPLATE_SPAN -> {
                        advance()
                        return scanTemplateSpan()
                    }
                    Token.PRIVATE_NAME -> {
                        if (source.pos == 0 && peek() == '!') {
                            token = skipSingleLineComment()
                            continue
                        }
                        return scanPrivateName()
                    }
                    Token.WHITESPACE -> {
                        token = skipWhitespace()
                        continue
                    }
                    Token.NUMBER -> scanNumber()
                    Token.IDENTIFIER -> scanIdentifierOrKeyword()
                    else -> throw Error()
                }
            }

            if (nextChar.isIdentifier) return scanIdentifierOrKeyword()
            if (nextChar == Source.endOfInput) return if (hasParserError.value) Token.ILLEGAL else Token.EOS
            token = skipWhitespace()
        } while (token == Token.WHITESPACE)

        return token
    }
    fun scan(desc: TokenDesc) {
        desc.token = scanSingleToken()
        desc.location.end = source.pos
    }
    fun scan() {
        scan(next)
    }
    companion object {
        private const val asciiMax = Byte.MAX_VALUE.toInt().toChar() // Byte.toChar() is deprecated
    }
    private fun Token.alsoAdvance() =
        this.also {
            advance()
        }
    private fun selectIf(next: Char, then: Token, `else`: Token): Token {
        advance()
        return (
            if (nextChar == next) then.alsoAdvance()
            else `else`
        )
    }
}

private fun getOneCharToken(char: Char) =
    Token.values().find { it.string == "$char" } ?: when (char) {
        '#' -> Token.PRIVATE_NAME
        '"', '\'' -> Token.STRING
        '`' -> Token.TEMPLATE_SPAN
        '\\' -> Token.IDENTIFIER
        else -> when {
            char.isAsciiWhitespace -> Token.WHITESPACE
            char.isAsciiDigit -> Token.NUMBER
            char.isIdentifier -> Token.IDENTIFIER
            else -> Token.ILLEGAL
        }
    }

private val Char.isAsciiWhitespace get() =
    this in setOf(' ', '\t', '\u000B', '\u000C', '\n', '\r')
private val Char.isWhitespace get() =
    this in setOf(' ', '\t', '\u000B', '\u000C', '\u00a0')
private val Char.isAsciiDigit get() =
    this in '0'..'9'
private val Char.isAsciiLetter get() =
    this in 'a'..'z' || this in 'A'..'Z'
private val Char.isAsciiLineTerminator get() =
    this == '\n' || this == '\r'
private val Char.isLineTerminator get() =
    this.isAsciiLineTerminator || this == '\u2028' || this == '\u2029'
private val Char.isIdentifier get() =
    this.isAsciiLetter || this == '$'
