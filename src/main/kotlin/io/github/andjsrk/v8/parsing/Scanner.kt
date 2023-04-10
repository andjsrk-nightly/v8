package io.github.andjsrk.v8.parsing

import io.github.andjsrk.v8.common.ErrorMessageTemplate

private typealias Error = ErrorMessageTemplate

private const val invalidSequence = -1
private const val invalidChar = invalidSequence.toChar()
private const val endOfInput = invalidChar

private class Source(val text: String) {
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
}

private data class TokenDesc(
    var token: Token = Token.UNINITIALIZED,
    var location: Location = Location(0, 0),
) {
    var literalChars = ""
    var rawLiteralChars = ""
    var afterLineTerminator = false
}

private enum class NumberKind {
    BINARY,
    OCTAL,
    HEX,
    DECIMAL,
    DECIMAL_WITH_LEADING_ZERO;

    val isValidBigInt get() =
        this in BINARY..DECIMAL
    val isDecimal get() =
        this in DECIMAL..DECIMAL_WITH_LEADING_ZERO
}

private data class ScannerError(val template: ErrorMessageTemplate, val location: Location)

class Scanner(sourceText: String) {
    private val source = Source(sourceText)
    private val hasParserError = ResettableState(false)
    private fun activateParserErrorState() {
        if (hasParserError.value) return

        source.pos = source.text.length
        hasParserError.value = true
        nextChar = endOfInput
        arrayOf(current, next, nextNext).forEach {
            if (it.token != Token.UNINITIALIZED) it.token = Token.ILLEGAL
        }
    }
    private var error: ScannerError? = null
    private val hasError get() =
        error != null
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
    private fun addLiteralCharAdvance() {
        addLiteralChar(nextChar)
        advance()
    }
    private fun peek() = source.peek()
    fun advance(captureRaw: Boolean = false) {
        if (captureRaw) {
            addRawLiteralChar(nextChar)
        }
        nextChar = source.advance()
    }
    fun advanceUntil(check: (Char) -> Boolean) {
        nextChar = source.advanceUntil(check)
    }
    private fun reportScannerError(template: ErrorMessageTemplate, location: Location = Location.untilNext(source.pos)) {
        if (hasError) return

        error = ScannerError(template, location)
    }
    fun skipWhitespace(): Token {
        if (nextChar.isAsciiWhitespace.not()) return Token.ILLEGAL

        if (next.afterLineTerminator.not() && nextChar.isLineTerminator) {
            next.afterLineTerminator = true
        }

        var hint = ' '
        advanceUntil {
            when {
                it == hint -> false
                it.isAsciiWhitespace || it.isAsciiLineTerminator -> {
                    if (next.afterLineTerminator.not() && nextChar.isLineTerminator) {
                        next.afterLineTerminator = true
                    }
                    hint = nextChar
                    false
                }
                else -> true
            }
        }

        return Token.WHITESPACE
    }
    fun skipSingleLineComment(): Token {
        advanceUntil { it.isLineTerminator }
        return Token.WHITESPACE
    }
    fun skipMultiLineComment(): Token {
        if (next.afterLineTerminator.not()) {
            do {
                advanceUntil {
                    it.isLineTerminator
                }

                while (nextChar == '*') {
                    advance()
                    if (nextChar == '/') {
                        advance()
                        return Token.WHITESPACE
                    }
                }

                if (nextChar.isLineTerminator) {
                    next.afterLineTerminator = true
                    break
                }
            } while ((nextChar == endOfInput).not())
        }

        while ((nextChar == endOfInput).not()) {
            advanceUntil { it == '*' }

            while (nextChar == '*') {
                advance()
                if (nextChar == '/') {
                    advance()
                    return Token.WHITESPACE
                }
            }
        }

        return Token.ILLEGAL
    }
    private fun scanHexNumber(expectedLength: Int, unicode: Boolean, captureRaw: Boolean): Int {
        val begin = source.pos - "\\u".length
        var res = 0

        (0 until expectedLength).forEach {
            val d = nextChar.toHexIntOrNull() ?: return invalidSequence.also {
                reportScannerError(
                    if (unicode) Error.INVALID_UNICODE_ESCAPE_SEQUENCE
                    else Error.INVALID_HEX_ESCAPE_SEQUENCE,
                    Location.since(begin, expectedLength + 2)
                )
            }
            res = res * 16 + d
            advance(captureRaw)
        }

        return res
    }
    private fun scanUnlimitedLengthHexNumber(maxValue: Int, beginPos: Int, captureRaw: Boolean): Int {
        var res = 0
        var parsed: Int? = nextChar.toHexIntOrNull() ?: return invalidSequence

        while (parsed != null) {
            res = res * 16 + parsed
            if (res > maxValue) {
                reportScannerError(ErrorMessageTemplate.UNDEFINED_UNICODE_CODE_POINT, Location(beginPos, source.pos + 1))
                return invalidSequence
            }
            advance(captureRaw)
            parsed = nextChar.toHexIntOrNull()
        }

        return res
    }
    private fun scanUnicodeEscape(captureRaw: Boolean): Int {
        // accept both \uXXXX and \u{XXXXXX}
        // \ and u have already been read

        if (nextChar == '{') {
            val begin = source.pos - "\\u".length
            advance(captureRaw)
            val codePoint = scanUnlimitedLengthHexNumber(0x10FFFF/* temp */, begin, captureRaw)
            if (codePoint == invalidSequence || nextChar != '}') {
                reportScannerError(ErrorMessageTemplate.INVALID_UNICODE_ESCAPE_SEQUENCE)
                return invalidSequence
            }
            advance(captureRaw)
            return codePoint
        }
        return scanHexNumber(4, true, captureRaw)
    }
    private fun checkEscape(captureRaw: Boolean): Boolean {
        val firstChar = nextChar
        advance(captureRaw)

        var escaped = firstChar
        when (firstChar) {
            'b' -> {
                escaped = '\b'
            }
            'f' -> {
                escaped = '\u000C'
            }
            'n' -> {
                escaped = '\n'
            }
            'r' -> {
                escaped = '\r'
            }
            't' -> {
                escaped = '\t'
            }
            'u' -> {
                val codePoint = scanUnicodeEscape(captureRaw)
                if (codePoint == invalidSequence) return false
                escaped = codePoint.toChar()
            }
        }

        addLiteralChar(escaped)
        return true
    }
    fun scanString(): Token {
        val quote = nextChar

        while (true) {
            advanceUntil {
                if (it.isStringTerminator) return@advanceUntil true
                addLiteralChar(it)
                return@advanceUntil false
            }

            while (nextChar == '\\') {
                advance()
                if (nextChar == endOfInput || !checkEscape(false)) return Token.ILLEGAL
            }

            if (nextChar == quote) {
                advance()
                return Token.STRING
            }

            if (nextChar == endOfInput || nextChar.isAsciiLineTerminator) return Token.ILLEGAL

            addLiteralChar(nextChar)
        }
    }
    fun scanTemplateHead(): Token {
        // TemplateHead ::
        //       ` TemplateCharacters? ${

        var result = Token.TEMPLATE_HEAD
        val captureRaw = true
        while (true) {
            when (val firstChar = nextChar) {
                '`' -> {
                    advance()
                    result = Token.TEMPLATE_TAIL
                }
                '\\' -> {
                    advance()
                    if (captureRaw) addRawLiteralChar('\\')
                    if (nextChar.isLineTerminator) {
                        var lastChar = nextChar
                        advance()
                        if (lastChar == '\r') {
                            if (nextChar == '\n') advance()
                            lastChar = '\n'
                        }
                        if (captureRaw) addRawLiteralChar(lastChar)
                    } else {
                        var success = checkEscape(captureRaw)

                    }
                }
                endOfInput -> {}
                else -> {
                    if (firstChar == '$' && peek() == '{') {
                        advance()
                        advance()
                    } else {
                        advance()
                        val normalizedFirstChar =
                            if (firstChar == '\r') {
                                if (nextChar == '\n') advance()
                                '\n'
                            } else firstChar
                        if (captureRaw) addRawLiteralChar(normalizedFirstChar)
                        addLiteralChar(normalizedFirstChar)
                    }
                }
            }
        }
    }
    private fun checkDigitsWithNumericSeparators(predicate: (Char) -> Boolean, isFirstDigitCheck: Boolean): Boolean {
        if (isFirstDigitCheck && !predicate(nextChar)) return false

        var separatorSeen = false

        while (predicate(nextChar) || nextChar.isNumericSeparator) {
            if (nextChar.isNumericSeparator) {
                advance()
                if (nextChar.isNumericSeparator) {
                    reportScannerError(Error.CONTINUOUS_NUMERIC_SEPARATOR)
                    return false
                }
                separatorSeen = true
                continue
            }
            separatorSeen = false
            addLiteralCharAdvance()
        }

        if (separatorSeen) {
            reportScannerError(ErrorMessageTemplate.TRAILING_NUMERIC_SEPARATOR)
            return false
        }

        return true
    }
    private fun checkDecimalDigits(): Boolean =
        checkDigitsWithNumericSeparators(Char::isAsciiDigit, false)
    private fun checkSignedInteger(): Boolean {
        if (nextChar == '+' || nextChar == '-') addLiteralCharAdvance()
        // we must have at least one decimal digit after 'e'/'E'
        if (nextChar.isAsciiDigit.not()) return false
        return checkDecimalDigits()
    }
    fun scanNumber(): Token {
        var kind = NumberKind.DECIMAL
        var seenPeriod = false

        if (nextChar == '0') { // we must check 0x, 0o, 0b
            addLiteralCharAdvance()

            when (nextChar.lowercaseChar()) {
                'x' -> { // hex
                    addLiteralCharAdvance()
                    kind = NumberKind.HEX
                    if (!checkDigitsWithNumericSeparators(Char::isHexDigit, true)) return Token.ILLEGAL
                }
                'o' -> { // octal
                    addLiteralCharAdvance()
                    kind = NumberKind.OCTAL
                    if (!checkDigitsWithNumericSeparators(Char::isOctalDigit, true)) return Token.ILLEGAL
                }
                'b' -> { // binary
                    addLiteralCharAdvance()
                    kind = NumberKind.BINARY
                    if (!checkDigitsWithNumericSeparators(Char::isBinaryDigit, true)) return Token.ILLEGAL
                }
                '_' -> {
                    reportScannerError(ErrorMessageTemplate.ZERO_DIGIT_NUMERIC_SEPARATOR)
                    return Token.ILLEGAL
                }
            }
        }

        if (kind.isDecimal) {
            if (!checkDecimalDigits()) return Token.ILLEGAL

            if (nextChar == '.') {
                seenPeriod = true
                addLiteralCharAdvance()
                if (nextChar.isNumericSeparator) return Token.ILLEGAL
                if (!checkDecimalDigits()) return Token.ILLEGAL
            }
        }

        var isBigint = false
        if (nextChar == 'n' && !seenPeriod && kind.isValidBigInt) {
            // val length = source.pos - startPos - (if (kind != NumberKind.DECIMAL) 2 else 0)
            isBigint = true
            advance()
        } else if (nextChar.lowercaseChar() == 'e') {
            if (kind.isDecimal.not()) return Token.ILLEGAL
            addLiteralCharAdvance()
            if (!checkSignedInteger()) return Token.ILLEGAL
        }

        if (nextChar.isAsciiDigit || nextChar.isIdentifier) return Token.ILLEGAL

        return if (isBigint) Token.BIGINT else Token.NUMBER
    }
    fun scanIdentifierOrKeyword(): Token {
        advanceUntil {
            when {
                it > asciiMax || it.isIdentifier.not() -> true
                else -> {
                    addLiteralChar(it)
                    false
                }
            }
        }

        return keywordMap[next.literalChars] ?: Token.IDENTIFIER
    }
    fun scanPrivateName(): Token {
        TODO()
    }
    fun scanSingleToken(): Token {
        var token: Token

        do {
            next.location.start = source.pos

            if (nextChar <= asciiMax) {
                token = getOneCharToken(nextChar)

                when (token) {
                    Token.LEFT_PAREN, Token.RIGHT_PAREN,
                    Token.LEFT_BRACE, Token.RIGHT_BRACE,
                    Token.LEFT_BRACK, Token.RIGHT_BRACK,
                    Token.COLON, Token.SEMICOLON, Token.COMMA, Token.BIT_NOT, Token.ILLEGAL -> {
                        return token.alsoAdvance()
                    }

                    Token.CONDITIONAL -> {
                        // ? ?. ?? ??=
                        advance()
                        return when (nextChar) {
                            '.' -> Token.QUESTION_PERIOD.alsoAdvance()
                            '?' -> selectIf('=', Token.ASSIGN_NULLISH, Token.NULLISH)
                            else -> Token.CONDITIONAL
                        }
                    }

                    Token.STRING -> return scanString()
                    Token.LT -> {
                        // < <= << <<=
                        advance()
                        return when (nextChar) {
                            '=' -> Token.LT_EQ.alsoAdvance()
                            '<' -> selectIf('=', Token.ASSIGN_SHL, Token.SHL)
                            else -> Token.LT
                        }
                    }

                    Token.GT -> {
                        // > >= >> >>= >>> >>>=
                        advance()
                        return when (nextChar) {
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
                        return when (nextChar) {
                            '=' -> selectIf('=', Token.EQ_STRICT, Token.EQ)
                            '>' -> Token.ARROW.alsoAdvance()
                            else -> Token.ASSIGN
                        }
                    }

                    Token.NOT -> {
                        // ! != !==
                        advance()
                        return when (nextChar) {
                            '=' -> selectIf('=', Token.NOT_EQ_STRICT, Token.NOT_EQ)
                            else -> Token.NOT
                        }
                    }

                    Token.ADD -> {
                        // + ++ +=
                        advance()
                        return when (nextChar) {
                            '+' -> Token.INC.alsoAdvance()
                            '=' -> Token.ASSIGN_ADD.alsoAdvance()
                            else -> Token.ADD
                        }
                    }

                    Token.SUB -> {
                        // - -- -=
                        advance()
                        return when (nextChar) {
                            '-' -> Token.DEC.alsoAdvance()
                            '=' -> Token.ASSIGN_SUB.alsoAdvance()
                            else -> Token.SUB
                        }
                    }

                    Token.MUL -> {
                        // * *= ** **=
                        advance()
                        return when (nextChar) {
                            '*' -> selectIf('=', Token.ASSIGN_EXP, Token.EXP)
                            '=' -> Token.ASSIGN_MUL.alsoAdvance()
                            else -> Token.MUL
                        }
                    }
                    // % %=
                    Token.MOD -> return selectIf('=', Token.ASSIGN_MOD, Token.MOD)
                    Token.DIV -> {
                        // / // /* /=
                        advance()
                        return when (nextChar) {
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
                        return when (nextChar) {
                            '&' -> selectIf('=', Token.ASSIGN_AND, Token.AND)
                            '=' -> Token.ASSIGN_BIT_AND.alsoAdvance()
                            else -> Token.BIT_AND
                        }
                    }

                    Token.BIT_OR -> {
                        // | |= || ||=
                        advance()
                        return when (nextChar) {
                            '|' -> selectIf('=', Token.ASSIGN_OR, Token.OR)
                            '=' -> Token.ASSIGN_BIT_OR
                            else -> Token.BIT_OR
                        }
                    }
                    // ^ ^=
                    Token.BIT_XOR -> return selectIf('=', Token.ASSIGN_BIT_XOR, Token.BIT_XOR)
                    Token.PERIOD -> {
                        // . ...
                        advance()
                        return if (nextChar == '.' && peek() == '.') {
                            advance()
                            advance()
                            Token.ELLIPSIS
                        } else Token.PERIOD
                    }

                    Token.TEMPLATE_HEAD -> {
                        advance()
                        return scanTemplateHead()
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

                    Token.NUMBER -> return scanNumber()
                    Token.IDENTIFIER -> return scanIdentifierOrKeyword()
                    else -> throw Error("Unreachable")
                }
            }

            if (nextChar.isIdentifier) return scanIdentifierOrKeyword()
            if (nextChar == endOfInput) return if (hasParserError.value) Token.ILLEGAL else Token.EOS
            token = skipWhitespace()
        } while (token == Token.WHITESPACE)

        return token
    }
    private fun scan(desc: TokenDesc) {
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
    Token.values().find { it.string == char.toString() } ?: when (char) {
        '#' -> Token.PRIVATE_NAME
        '"', '\'' -> Token.STRING
        '`' -> Token.TEMPLATE_HEAD
        '\\' -> Token.IDENTIFIER
        else -> when {
            char.isAsciiWhitespace -> Token.WHITESPACE
            char.isAsciiDigit -> Token.NUMBER
            char.isIdentifier -> Token.IDENTIFIER
            else -> Token.ILLEGAL
        }
    }

private inline fun Char.toHexIntOrNull() =
    this.toString().toIntOrNull(16)
private inline val Char.isNumericSeparator get() =
    this == '_'

private val Char.isStringTerminator get() =
    this in setOf('\'', '"', '\\') || this.isAsciiLineTerminator
private inline val Char.isHexDigit get() =
    this.isAsciiDigit || this in 'a'..'f' || this in 'A'..'F'
private inline val Char.isOctalDigit get() =
    this in '0'..'7'
private inline val Char.isBinaryDigit get() =
    this == '0' || this == '1'
private val Char.isAsciiWhitespace get() =
    this.isAsciiLineTerminator || this in setOf(' ', '\t', '\u000B', '\u000C')
private inline val Char.isAsciiDigit get() =
    this in '0'..'9'
private inline val Char.isAsciiLetter get() =
    this in 'a'..'z' || this in 'A'..'Z'
private inline val Char.isAsciiLineTerminator get() =
    this == '\n' || this == '\r'
private inline val Char.isLineTerminator get() =
    this.isAsciiLineTerminator || this == '\u2028' || this == '\u2029'
private inline val Char.isIdentifier get() =
    this.isAsciiLetter || this == '$'
