package io.github.andjsrk.v8.parsing

enum class Token(
    val string: String?,
    val precedence: Int,
    val isKeyword: Boolean = false,
) {
    TEMPLATE_HEAD(null, 0),
    TEMPLATE_MIDDLE(null, 0),
    TEMPLATE_TAIL(null, 0),
    PERIOD(".", 0),
    LEFT_BRACK("[", 0),
    QUESTION_PERIOD("?.", 0),
    LEFT_PAREN("(", 0),
    RIGHT_PAREN(")", 0),
    RIGHT_BRACK("]", 0),
    LEFT_BRACE("{", 0),
    COLON(":", 0),
    ELLIPSIS("...", 0),
    CONDITIONAL("?", 3),
    SEMICOLON(";", 0),
    RIGHT_BRACE("}", 0),
    EOS("EOS", 0), // End Of Source
    ARROW("=>", 0),
    INIT("=init", 2), // AST-use only
    ASSIGN("=", 2),
    ASSIGN_NULLISH("??=", 2),
    ASSIGN_OR("||=", 2),
    ASSIGN_AND("&&=", 2),
    ASSIGN_BIT_OR("|=", 2),
    ASSIGN_BIT_XOR("^=", 2),
    ASSIGN_BIT_AND("&=", 2),
    ASSIGN_SHL("<<=", 2),
    ASSIGN_SAR(">>=", 2),
    ASSIGN_SHR(">>>=", 2),
    ASSIGN_MUL("*=", 2),
    ASSIGN_DIV("/=", 2),
    ASSIGN_MOD("%=", 2),
    ASSIGN_EXP("**=", 2),
    ASSIGN_ADD("+=", 2),
    ASSIGN_SUB("-=", 2),
    COMMA(",", 1),
    NULLISH("??", 3),
    OR("||", 4),
    AND("&&", 5),
    BIT_OR("|", 6),
    BIT_XOR("^", 7),
    BIT_AND("&", 8),
    SHL("<<", 11),
    SAR(">>", 11),
    SHR(">>>", 11),
    MUL("*", 13),
    DIV("/", 13),
    MOD("%", 13),
    EXP("**", 14),
    ADD("+", 12),
    SUB("-", 12),
    NOT("!", 0),
    BIT_NOT("~", 0),
    DELETE("delete", 0, isKeyword=true),
    TYPEOF("typeof", 0, isKeyword=true),
    VOID("void", 0, isKeyword=true),
    INC("++", 0),
    DEC("--", 0),
    EQ("==", 9),
    EQ_STRICT("===", 9),
    NOT_EQ("!=", 9),
    NOT_EQ_STRICT("!==", 9),
    LT("<", 10),
    GT(">", 10),
    LT_EQ("<=", 10),
    GT_EQ(">=", 10),
    INSTANCEOF("instanceof", 10, isKeyword=true),
    IN("in", 10, isKeyword=true),
    BREAK("break", 0, isKeyword=true),
    CASE("case", 0, isKeyword=true),
    CATCH("catch", 0, isKeyword=true),
    CONTINUE("continue", 0, isKeyword=true),
    DEBUGGER("debugger", 0, isKeyword=true),
    DEFAULT("default", 0, isKeyword=true),
    DO("do", 0, isKeyword=true),
    ELSE("else", 0, isKeyword=true),
    FINALLY("finally", 0, isKeyword=true),
    FOR("for", 0, isKeyword=true),
    FUNCTION("function", 0, isKeyword=true),
    IF("if", 0, isKeyword=true),
    NEW("new", 0, isKeyword=true),
    RETURN("return", 0, isKeyword=true),
    SWITCH("switch", 0, isKeyword=true),
    THROW("throw", 0, isKeyword=true),
    TRY("try", 0, isKeyword=true),
    VAR("var", 0, isKeyword=true),
    WHILE("while", 0, isKeyword=true),
    WITH("with", 0, isKeyword=true),
    THIS("this", 0, isKeyword=true),
    NULL_LITERAL("null", 0, isKeyword=true),
    TRUE_LITERAL("true", 0, isKeyword=true),
    FALSE_LITERAL("false", 0, isKeyword=true),
    NUMBER(null, 0),
    SMI(null, 0),
    BIGINT(null, 0),
    STRING(null, 0),
    SUPER("super", 0, isKeyword=true),
    IDENTIFIER(null, 0),
    GET("get", 0, isKeyword=true),
    SET("set", 0, isKeyword=true),
    ASYNC("async", 0, isKeyword=true),
    AWAIT("await", 0, isKeyword=true),
    YIELD("yield", 0, isKeyword=true),
    LET("let", 0, isKeyword=true),
    STATIC("static", 0, isKeyword=true),
    FUTURE_STRICT_RESERVED_WORD(null, 0),
    ESCAPED_STRICT_RESERVED_WORD(null, 0),
    ENUM("enum", 0, isKeyword=true), // ?
    CLASS("class", 0, isKeyword=true),
    CONST("const", 0, isKeyword=true),
    EXPORT("export", 0, isKeyword=true),
    EXTENDS("extends", 0, isKeyword=true),
    IMPORT("import", 0, isKeyword=true),
    PRIVATE_NAME(null, 0),
    ILLEGAL("ILLEGAL", 0),
    ESCAPED_KEYWORD(null, 0),
    WHITESPACE(null, 0),
    UNINITIALIZED(null, 0),
    REGEXP_LITERAL(null, 0);

    val isPropertyName get() =
        this.isKeyword || this.isAnyIdentifier || this == ESCAPED_KEYWORD
    fun isValidIdentifier(isGenerator: Boolean, disallowAwait: Boolean) =
        when {
            this in IDENTIFIER..ASYNC -> true
            this == AWAIT -> !disallowAwait
            this == YIELD -> !isGenerator
            else -> this.isStrictReservedWord
        }
    val isCallable get() =
        this in SUPER..ESCAPED_STRICT_RESERVED_WORD
    val isAutoSemicolon get() =
        this in SEMICOLON..EOS
    val isAnyIdentifier get() =
        this in IDENTIFIER..ESCAPED_STRICT_RESERVED_WORD
    val isStrictReservedWord get() =
        this in YIELD..ESCAPED_STRICT_RESERVED_WORD
    val isLiteral get() =
        this in NULL_LITERAL..STRING
    val isTemplate get() =
        this in TEMPLATE_HEAD..TEMPLATE_TAIL
    val isMember get() =
        this in TEMPLATE_HEAD..LEFT_BRACK
    val isProperty get() =
        this in PERIOD..LEFT_BRACK
    val isPropertyOfCall get() =
        this in TEMPLATE_HEAD..LEFT_PAREN
    val isArrowOrAssignmentOp get() =
        this in ARROW..ASSIGN_SUB
    val isAssignmentOp get() =
        this in INIT..ASSIGN_SUB
    val isLogicalAssignmentOp get() =
        this in ASSIGN_NULLISH..ASSIGN_AND
    val isBinaryOp get() =
        this in COMMA..SUB
    val isCompareOp get() =
        this in EQ..IN
    val isOrderedRelationalCompareOp get() =
        this in LT..GT_EQ
    val isEqualityOp get() =
        this in EQ..EQ_STRICT
    val isBitOp get() =
        this in BIT_OR..SHR || this == BIT_NOT
    val isUnaryOp get() =
        this in ADD..VOID
    val isCountOp get() =
        this in INC..DEC
    val isUnaryOrCountOp get() =
        this in ADD..DEC
    val isShiftOp get() =
        this in SHL..SHR
    companion object {
        fun binaryOpForAssignment(op: Token): Token {
            require(op in ASSIGN_NULLISH..ASSIGN_SUB)
            return Token.values()[op.ordinal - ASSIGN_NULLISH.ordinal + NULLISH.ordinal]
        }
    }
}
