package io.github.andjsrk.v8.common

enum class ErrorMessageTemplate(val content: String) {
    /* Error */
    CONFLICTING_PRIVATE_NAME("Operation is ambiguous because there are more than one private name '%' on the object"),
    CYCLIC_PROTO("Cyclic __proto__ value"),
    DEBUGGER("Debugger: %"),
    DEBUGGER_LOADING("Error loading debugger"),
    DEFAULT_OPTIONS_MISSING("Internal % error. Default options are missing."),
    DELETE_PRIVATE_FIELD("Private fields can not be deleted"),
    PLACEHOLDER_ONLY("%"),
    UNCAUGHT_EXCEPTION("Uncaught %"),
    UNSUPPORTED("Not supported"),
    WRONG_SERVICE_TYPE("Internal error, wrong service type: %"),
    WRONG_VALUE_TYPE("Internal error. Wrong value type."),
    ICU_ERROR("Internal error. Icu error."),
    /* TypeError */
    APPLY_NON_FUNCTION("Function.prototype.apply was called on %, which is % and not a  function"),
    ARGUMENTS_DISALLOWED_IN_INITIALIZER_AND_STATIC_BLOCK("'arguments' is not allowed in class field initializer or static  initialization block"),
    ARRAY_BUFFER_DETACH_KEY_DOESNT_MATCH("Provided key doesn't match [[ArrayBufferDetachKey]]"),
    ARRAY_BUFFER_TOO_SHORT("Derived ArrayBuffer constructor created a buffer which was too small"),
    ARRAY_BUFFER_SPECIES_THIS("ArrayBuffer subclass returned this from species constructor"),
    AWAIT_NOT_IN_ASYNC_CONTEXT("await is only valid in async functions and the top level bodies of  modules"),
    AWAIT_NOT_IN_DEBUG_EVALUATE("await can not be used when evaluating code  while paused in the debugger"),
    ATOMICS_MUTEX_NOT_OWNED_BY_CURRENT_THREAD("Atomics.Mutex is not owned by the current agent"),
    ATOMICS_OPERATION_NOT_ALLOWED("% cannot be called in this context"),
    BAD_ROUNDING_TYPE("RoundingType is not fractionDigits"),
    BAD_SORT_COMPARISON_FUNCTION("The comparison function must be either a function or undefined"),
    BIGINT_FROM_NUMBER("The number % cannot be converted to a BigInt because it is not an  integer"),
    BIGINT_FROM_OBJECT("Cannot convert % to a BigInt"),
    BIGINT_MIXED_TYPES("Cannot mix BigInt and other types, use explicit conversions"),
    BIGINT_SERIALIZE_J_S_O_N("Do not know how to serialize a BigInt"),
    BIGINT_SHR("BigInts have no unsigned right shift, use >> instead"),
    BIGINT_TO_NUMBER("Cannot convert a BigInt value to a number"),
    CALLED_NON_CALLABLE("% is not a function"),
    CALLED_ON_NON_OBJECT("% called on non-object"),
    CALLED_ON_NULL_OR_UNDEFINED("% called on null or undefined"),
    CALL_SHADOW_REALM_EVALUATE_THREW("ShadowRealm evaluate threw (%)"),
    CALL_SITE_EXPECTS_FUNCTION("CallSite expects wasm object as first or function as second argument,  got <%, %>"),
    CALL_SITE_METHOD("CallSite method % expects CallSite as receiver"),
    CALL_SITE_METHOD_UNSUPPORTED_IN_SHADOW_REALM("CallSite method % is unsupported inside ShadowRealms"),
    CALL_WRAPPED_FUNCTION_THREW("WrappedFunction threw (%)"),
    CANNOT_BE_SHARED("% cannot be shared"),
    CANNOT_CONVERT_TO_PRIMITIVE("Cannot convert object to primitive value"),
    CANNOT_PREVENT_EXT("Cannot prevent extensions"),
    CANNOT_FREEZE("Cannot freeze"),
    CANNOT_FREEZE_ARRAY_BUFFER_VIEW("Cannot freeze array buffer views with elements"),
    CANNOT_SEAL("Cannot seal"),
    CANNOT_WRAP("Cannot wrap target callable (%)"),
    CIRCULAR_STRUCTURE("Converting circular structure to JSON%"),
    CONSTRUCT_ABSTRACT_CLASS("Abstract class % not directly constructable"),
    CONST_ASSIGN("Assignment to constant variable."),
    CONSTRUCTOR_CLASS_FIELD("Classes may not have a field named 'constructor'"),
    CONSTRUCTOR_NON_CALLABLE("Class constructor % cannot be invoked without 'new'"),
    ANONYMOUS_CONSTRUCTOR_NON_CALLABLE("Class constructors cannot be invoked without 'new'"),
    CONSTRUCTOR_NOT_FUNCTION("Constructor % requires 'new'"),
    CONSTRUCTOR_NOT_RECEIVER("The .constructor property is not an object"),
    CURRENCY_CODE("Currency code is required with currency style."),
    CYCLIC_MODULE_DEPENDENCY("Detected cycle while resolving name '%' in '%'"),
    DATA_VIEW_NOT_ARRAY_BUFFER("First argument to DataView constructor must be an ArrayBuffer"),
    DATE_TYPE("this is not a Date object."),
    DEBUGGER_FRAME("Debugger: Invalid frame index."),
    DEBUGGER_TYPE("Debugger: Parameters have wrong types."),
    DECLARATION_MISSING_INITIALIZER("Missing initializer in % declaration"),
    DEFINE_DISALLOWED("Cannot define property %, object is not extensible"),
    DEFINE_DISALLOWED_FIXED_LAYOUT("Cannot define property %, object is fixed layout"),
    DETACHED_OPERATION("Cannot perform % on a detached ArrayBuffer"),
    DO_NOT_USE("Do not use %; %"),
    DUPLICATE_TEMPLATE_PROPERTY("Object template has duplicate property '%'"),
    EXTENDS_VALUE_NOT_CONSTRUCTOR("Class extends value % is not a constructor or null"),
    FIRST_ARGUMENT_NOT_REG_EXP("First argument to % must not be a regular expression"),
    FUNCTION_BIND("Bind must be called on a function"),
    GENERATOR_RUNNING("Generator is already running"),
    ILLEGAL_INVOCATION("Illegal invocation"),
    IMMUTABLE_PROTOTYPE_SET("Immutable prototype object '%' cannot have their prototype set"),
    IMPORT_ASSERTION_DUPLICATE_KEY("Import assertion has duplicate key '%'"),
    IMPORT_CALL_NOT_NEW_EXPRESSION("Cannot use new with import"),
    IMPORT_OUTSIDE_MODULE("Cannot use import statement outside a module"),
    IMPORT_META_OUTSIDE_MODULE("Cannot use 'import.meta' outside a module"),
    IMPORT_MISSING_SPECIFIER("import() requires a specifier"),
    IMPORT_SHADOW_REALM_REJECTED("Cannot import in ShadowRealm (%)"),
    INCOMPATIBLE_METHOD_RECEIVER("Method % called on incompatible receiver %"),
    INSTANCEOF_NONOBJECT_PROTO("Function has non-object prototype '%' in instanceof check"),
    INVALID_ARGUMENT("invalid_argument"),
    INVALID_ARGUMENT_FOR_TEMPORAL("Invalid argument for Temporal %"),
    INVALID_IN_OPERATOR_USE("Cannot use 'in' operator to search for '%' in %"),
    INVALID_RAW_JSON_VALUE("Invalid value for JSON.rawJSON"),
    INVALID_REG_EXP_EXEC_RESULT("RegExp exec method returned something other than an Object or null"),
    INVALID_UNIT("Invalid unit argument for %() '%'"),
    ITERABLE_YIELDED_NON_STRING("Iterable yielded % which is not a string"),
    ITERATOR_REDUCE_NO_INITIAL("Reduce of a done iterator with no initial value"),
    ITERATOR_RESULT_NOT_AN_OBJECT("Iterator result % is not an object"),
    SPREAD_ITERATOR_SYMBOL_NON_CALLABLE("Spread syntax requires ...iterable[Symbol.iterator] to be a function"),
    FIRST_ARGUMENT_ITERATOR_SYMBOL_NON_CALLABLE("% requires that the property of the first argument,  items[Symbol.iterator], when exists, be a function"),
    ITERATOR_VALUE_NOT_AN_OBJECT("Iterator value % is not an entry object"),
    LANGUAGE_I_D("Language ID should be string or object."),
    LOCALE_NOT_EMPTY("First argument to Intl.Locale constructor can't be empty or missing"),
    LOCALE_BAD_PARAMETERS("Incorrect locale information provided"),
    LIST_FORMAT_BAD_PARAMETERS("Incorrect ListFormat information provided"),
    MAPPER_FUNCTION_NON_CALLABLE("flatMap mapper function is not callable"),
    METHOD_INVOKED_ON_WRONG_TYPE("Method invoked on an object that is not %."),
    NO_ACCESS("no access"),
    NON_CALLABLE_IN_INSTANCE_OF_CHECK("Right-hand side of 'instanceof' is not callable"),
    NON_COERCIBLE("Cannot destructure '%' as it is %."),
    NON_COERCIBLE_WITH_PROPERTY("Cannot destructure property '%' of '%' as it is %."),
    NON_EXTENSIBLE_PROTO("% is not extensible"),
    NON_OBJECT_ASSERT_OPTION("The 'assert' option must be an object"),
    NON_OBJECT_IN_INSTANCE_OF_CHECK("Right-hand side of 'instanceof' is not an object"),
    NON_OBJECT_PRIVATE_NAME_ACCESS("Cannot access private name % from %"),
    NON_OBJECT_PROPERTY_LOAD("Cannot read properties of %"),
    NON_OBJECT_PROPERTY_LOAD_WITH_PROPERTY("Cannot read properties of % (reading '%')"),
    NON_OBJECT_PROPERTY_STORE("Cannot set properties of %"),
    NON_OBJECT_PROPERTY_STORE_WITH_PROPERTY("Cannot set properties of % (setting '%')"),
    NON_OBJECT_IMPORT_ARGUMENT("The second argument to import() must be an object"),
    NON_STRING_IMPORT_ASSERTION_VALUE("Import assertion value must be a string"),
    NO_SETTER_IN_CALLBACK("Cannot set property % of % which has only a getter"),
    NOT_AN_ITERATOR("% is not an iterator"),
    PROMISE_NEW_TARGET_UNDEFINED("Promise constructor cannot be invoked without 'new'"),
    NOT_CONSTRUCTOR("% is not a constructor"),
    NOT_DATE_OBJECT("this is not a Date object."),
    NOT_GENERIC("% requires that 'this' be a %"),
    NOT_CALLABLE("% is not a function"),
    NOT_CALLABLE_OR_ITERABLE("% is not a function or its return value is not iterable"),
    NOT_CALLABLE_OR_ASYNC_ITERABLE("% is not a function or its return value is not async iterable"),
    NOT_FINITE_NUMBER("Value need to be finite number for %()"),
    NOT_ITERABLE("% is not iterable"),
    NOT_ITERABLE_NO_SYMBOL_LOAD("% is not iterable (cannot read property %)"),
    NOT_ASYNC_ITERABLE("% is not async iterable"),
    NOT_PROPERTY_NAME("% is not a valid property name"),
    NOT_TYPED_ARRAY("this is not a typed array."),
    NOT_SUPER_CONSTRUCTOR("Super constructor % of % is not a constructor"),
    NOT_SUPER_CONSTRUCTOR_ANONYMOUS_CLASS("Super constructor % of anonymous class is not a constructor"),
    NOT_INTEGER_TYPED_ARRAY("% is not an integer typed array."),
    NOT_INT32_OR_BIGINT64_TYPED_ARRAY("% is not an int32 or BigInt64 typed array."),
    NOT_SHARED_TYPED_ARRAY("% is not a shared typed array."),
    OBJECT_GETTER_EXPECTING_FUNCTION("Object.prototype.__defineGetter__: Expecting function"),
    OBJECT_GETTER_CALLABLE("Getter must be a function: %"),
    OBJECT_NOT_EXTENSIBLE("Cannot add property %, object is not extensible"),
    OBJECT_SETTER_EXPECTING_FUNCTION("Object.prototype.__defineSetter__: Expecting function"),
    OBJECT_SETTER_CALLABLE("Setter must be a function: %"),
    ORDINARY_FUNCTION_CALLED_AS_CONSTRUCTOR("Function object that's not a constructor was created with new"),
    PROMISE_CYCLIC("Chaining cycle detected for promise %"),
    PROMISE_EXECUTOR_ALREADY_INVOKED("Promise executor has already been invoked with non-undefined arguments"),
    PROMISE_NON_CALLABLE("Promise resolve or reject function is not callable"),
    PROPERTY_DESC_OBJECT("Property description must be an object: %"),
    PROPERTY_NOT_FUNCTION("'%' returned for property '%' of object '%' is not a function"),
    PROTO_OBJECT_OR_NULL("Object prototype may only be an Object or null: %"),
    PROTOTYPE_PARENT_NOT_AN_OBJECT("Class extends value does not have valid prototype property %"),
    PROXY_CONSTRUCT_NON_OBJECT("'construct' on proxy: trap returned non-object ('%')"),
    PROXY_DEFINE_PROPERTY_NON_CONFIGURABLE("'defineProperty' on proxy: trap returned truish for defining  non-configurable property '%' which is either non-existent or  configurable in the proxy target"),
    PROXY_DEFINE_PROPERTY_NON_CONFIGURABLE_WRITABLE("'defineProperty' on proxy: trap returned truish for defining  non-configurable property '%' which cannot be non-writable, unless  there exists a corresponding non-configurable, non-writable own  property of the target object."),
    PROXY_DEFINE_PROPERTY_NON_EXTENSIBLE("'defineProperty' on proxy: trap returned truish for adding property '%'   to the non-extensible proxy target"),
    PROXY_DEFINE_PROPERTY_INCOMPATIBLE("'defineProperty' on proxy: trap returned truish for adding property '%'   that is incompatible with the existing property in the proxy target"),
    PROXY_DELETE_PROPERTY_NON_CONFIGURABLE("'deleteProperty' on proxy: trap returned truish for property '%' which  is non-configurable in the proxy target"),
    PROXY_DELETE_PROPERTY_NON_EXTENSIBLE("'deleteProperty' on proxy: trap returned truish for property '%' but  the proxy target is non-extensible"),
    PROXY_GET_NON_CONFIGURABLE_DATA("'get' on proxy: property '%' is a read-only and  non-configurable data property on the proxy target but the proxy  did not return its actual value (expected '%' but got '%')"),
    PROXY_GET_NON_CONFIGURABLE_ACCESSOR("'get' on proxy: property '%' is a non-configurable accessor  property on the proxy target and does not have a getter function, but  the trap did not return 'undefined' (got '%')"),
    PROXY_GET_OWN_PROPERTY_DESCRIPTOR_INCOMPATIBLE("'getOwnPropertyDescriptor' on proxy: trap returned descriptor for  property '%' that is incompatible with the existing property in the  proxy target"),
    PROXY_GET_OWN_PROPERTY_DESCRIPTOR_INVALID("'getOwnPropertyDescriptor' on proxy: trap returned neither object nor  undefined for property '%'"),
    PROXY_GET_OWN_PROPERTY_DESCRIPTOR_NON_CONFIGURABLE("'getOwnPropertyDescriptor' on proxy: trap reported non-configurability  for property '%' which is either non-existent or configurable in the  proxy target"),
    PROXY_GET_OWN_PROPERTY_DESCRIPTOR_NON_CONFIGURABLE_WRITABLE("'getOwnPropertyDescriptor' on proxy: trap reported non-configurable  and writable for property '%' which is non-configurable, non-writable  in the proxy target"),
    PROXY_GET_OWN_PROPERTY_DESCRIPTOR_NON_EXTENSIBLE("'getOwnPropertyDescriptor' on proxy: trap returned undefined for  property '%' which exists in the non-extensible proxy target"),
    PROXY_GET_OWN_PROPERTY_DESCRIPTOR_UNDEFINED("'getOwnPropertyDescriptor' on proxy: trap returned undefined for  property '%' which is non-configurable in the proxy target"),
    PROXY_GET_PROTOTYPE_OF_INVALID("'getPrototypeOf' on proxy: trap returned neither object nor null"),
    PROXY_GET_PROTOTYPE_OF_NON_EXTENSIBLE("'getPrototypeOf' on proxy: proxy target is non-extensible but the  trap did not return its actual prototype"),
    PROXY_HAS_NON_CONFIGURABLE("'has' on proxy: trap returned falsish for property '%' which exists in  the proxy target as non-configurable"),
    PROXY_HAS_NON_EXTENSIBLE("'has' on proxy: trap returned falsish for property '%' but the proxy  target is not extensible"),
    PROXY_IS_EXTENSIBLE_INCONSISTENT("'isExtensible' on proxy: trap result does not reflect extensibility of  proxy target (which is '%')"),
    PROXY_NON_OBJECT("Cannot create proxy with a non-object as target or handler"),
    PROXY_OWN_KEYS_MISSING("'ownKeys' on proxy: trap result did not include '%'"),
    PROXY_OWN_KEYS_NON_EXTENSIBLE("'ownKeys' on proxy: trap returned extra keys but proxy target is  non-extensible"),
    PROXY_OWN_KEYS_DUPLICATE_ENTRIES("'ownKeys' on proxy: trap returned duplicate entries"),
    PROXY_PREVENT_EXTENSIONS_EXTENSIBLE("'preventExtensions' on proxy: trap returned truish but the proxy target  is extensible"),
    PROXY_PRIVATE("Cannot pass private property name to proxy trap"),
    PROXY_REVOKED("Cannot perform '%' on a proxy that has been revoked"),
    PROXY_SET_FROZEN_DATA("'set' on proxy: trap returned truish for property '%' which exists in  the proxy target as a non-configurable and non-writable data property  with a different value"),
    PROXY_SET_FROZEN_ACCESSOR("'set' on proxy: trap returned truish for property '%' which exists in  the proxy target as a non-configurable and non-writable accessor  property without a setter"),
    PROXY_SET_PROTOTYPE_OF_NON_EXTENSIBLE("'setPrototypeOf' on proxy: trap returned truish for setting a new  prototype on the non-extensible proxy target"),
    PROXY_TRAP_RETURNED_FALSISH("'%' on proxy: trap returned falsish"),
    PROXY_TRAP_RETURNED_FALSISH_FOR("'%' on proxy: trap returned falsish for property '%'"),
    REDEFINE_DISALLOWED("Cannot redefine property: %"),
    REDEFINE_EXTERNAL_ARRAY("Cannot redefine a property of an object with external array elements"),
    REDUCE_NO_INITIAL("Reduce of empty array with no initial value"),
    REG_EXP_FLAGS("Cannot supply flags when constructing one RegExp from another"),
    REG_EXP_NON_OBJECT("% getter called on non-object %"),
    REG_EXP_NON_REG_EXP("% getter called on non-RegExp object"),
    REG_EXP_GLOBAL_INVOKED_ON_NON_GLOBAL("% called with a non-global RegExp argument"),
    RELATIVE_DATE_TIME_FORMATTER_BAD_PARAMETERS("Incorrect RelativeDateTimeFormatter provided"),
    RESOLVER_NOT_A_FUNCTION("Promise resolver % is not a function"),
    RETURN_METHOD_NOT_CALLABLE("The iterator's 'return' method is not callable"),
    SHARED_ARRAY_BUFFER_TOO_SHORT("Derived SharedArrayBuffer constructor created a buffer which was too  small"),
    SHARED_ARRAY_BUFFER_SPECIES_THIS("SharedArrayBuffer subclass returned this from species constructor"),
    STATIC_PROTOTYPE("Classes may not have a static property named 'prototype'"),
    STRICT_DELETE_PROPERTY("Cannot delete property '%' of %"),
    STRICT_POISON_PILL("'caller', 'callee', and 'arguments' properties may not be accessed on  strict mode functions or the arguments objects for calls to them"),
    STRICT_READ_ONLY_PROPERTY("Cannot assign to read only property '%' of % '%'"),
    STRICT_CANNOT_CREATE_PROPERTY("Cannot create property '%' on % '%'"),
    STRING_MATCH_ALL_NULL_OR_UNDEFINED_FLAGS("The .flags property of the argument to String.prototype.matchAll cannot  be null or undefined"),
    SYMBOL_ITERATOR_INVALID("Result of the Symbol.iterator method is not an object"),
    SYMBOL_ASYNC_ITERATOR_INVALID("Result of the Symbol.asyncIterator method is not an object"),
    SYMBOL_KEY_FOR("% is not a symbol"),
    SYMBOL_TO_NUMBER("Cannot convert a Symbol value to a number"),
    SYMBOL_TO_STRING("Cannot convert a Symbol value to a string"),
    THROW_METHOD_MISSING("The iterator does not provide a 'throw' method."),
    TOP_LEVEL_AWAIT_STALLED("Top-level await promise never resolved"),
    UNDEFINED_OR_NULL_TO_OBJECT("Cannot convert undefined or null to object"),
    VALUE_AND_ACCESSOR("Invalid property descriptor. Cannot both specify accessors and a value  or writable attribute, %"),
    VAR_REDECLARATION("Identifier '%' has already been declared"),
    VAR_NOT_ALLOWED_IN_EVAL_SCOPE("Identifier '%' cannot be declared with 'var' in current evaluation  scope, consider trying 'let' instead"),
    WRONG_ARGS("%: Arguments list has wrong type"),
    /* ReferenceError */
    NOT_DEFINED("% is not defined"),
    SUPER_ALREADY_CALLED("Super constructor may only be called once"),
    ACCESSED_UNINITIALIZED_VARIABLE("Cannot access '%' before initialization"),
    UNSUPPORTED_SUPER("Unsupported reference to 'super'"),
    ACCESSED_UNAVAILABLE_VARIABLE("Cannot access '%' from debugger"),
    /* RangeError */
    BIGINT_DIV_ZERO("Division by zero"),
    BIGINT_TOO_BIG("Maximum BigInt size exceeded"),
    CANT_SET_OPTION_X_WHEN_Y_IS_USED("Can't set option % when % is used"),
    DATE_RANGE("Provided date is not in valid range."),
    EXPECTED_LOCATION("Expected letters optionally connected with underscores or hyphens for  a location, got %"),
    INVALID_ARRAY_BUFFER_LENGTH("Invalid array buffer length"),
    INVALID_ARRAY_BUFFER_MAX_LENGTH("Invalid array buffer max length"),
    INVALID_ARRAY_BUFFER_RESIZE_LENGTH("%: Invalid length parameter"),
    ARRAY_BUFFER_ALLOCATION_FAILED("Array buffer allocation failed"),
    INVALID("Invalid % : %"),
    INVALID_ARRAY_LENGTH("Invalid array length"),
    INVALID_ATOMIC_ACCESS_INDEX("Invalid atomic access index"),
    INVALID_CALENDAR("Invalid calendar specified: %"),
    INVALID_CODE_POINT("Invalid code point %"),
    INVALID_COUNT_VALUE("Invalid count value: %"),
    INVALID_DATA_VIEW_ACCESSOR_OFFSET("Offset is outside the bounds of the DataView"),
    INVALID_DATA_VIEW_LENGTH("Invalid DataView length %"),
    INVALID_OFFSET("Start offset % is outside the bounds of the buffer"),
    INVALID_HINT("Invalid hint: %"),
    INVALID_INDEX("Invalid value: not (convertible to) a safe integer"),
    INVALID_LANGUAGE_TAG("Invalid language tag: %"),
    INVALID_WEAK_MAP_KEY("Invalid value used as weak map key"),
    INVALID_WEAK_SET_VALUE("Invalid value used in weak set"),
    INVALID_SHADOW_REALM_EVALUATE_SOURCE_TEXT("Invalid value used as source text"),
    INVALID_STRING_LENGTH("Invalid string length"),
    INVALID_TIME_VALUE("Invalid time value"),
    INVALID_TIME_VALUE_FOR_TEMPORAL("Invalid time value for Temporal %"),
    INVALID_TIME_ZONE("Invalid time zone specified: %"),
    INVALID_TYPED_ARRAY_ALIGNMENT("% of % should be a multiple of %"),
    INVALID_TYPED_ARRAY_INDEX("Invalid typed array index"),
    INVALID_TYPED_ARRAY_LENGTH("Invalid typed array length: %"),
    LET_IN_LEXICAL_BINDING("let is disallowed as a lexically bound name"),
    LOCALE_MATCHER("Illegal value for localeMatcher:%"),
    MAXIMUM_FRACTION_DIGITS_NOT_EQUAL_MINIMUM_FRACTION_DIGITS("maximumFractionDigits not equal to minimumFractionDigits"),
    NORMALIZATION_FORM("The normalization form should be one of %."),
    OUT_OF_MEMORY("%: Out of memory"),
    PARAMETER_OF_FUNCTION_OUT_OF_RANGE("Paramenter % of function %() is % and out of range"),
    ZERO_DIGIT_NUMERIC_SEPARATOR("Numeric separator can not be used after leading 0."),
    NUMBER_FORMAT_RANGE("% argument must be between 0 and 100"),
    TRAILING_NUMERIC_SEPARATOR("Numeric separators are not allowed at the end of numeric literals"),
    CONTINUOUS_NUMERIC_SEPARATOR("Only one underscore is allowed as numeric separator"),
    PROPERTY_VALUE_OUT_OF_RANGE("% value is out of range."),
    STACK_OVERFLOW("Maximum call stack size exceeded"),
    TO_PRECISION_FORMAT_RANGE("toPrecision() argument must be between 1 and 100"),
    TO_RADIX_FORMAT_RANGE("toString() radix argument must be between 2 and 36"),
    SHARED_ARRAY_SIZE_OUT_OF_RANGE("SharedArray length out of range (maximum of 2**14-2 allowed)"),
    STRUCT_FIELD_COUNT_OUT_OF_RANGE("Struct field count out of range (maximum of 999 allowed)"),
    TYPED_ARRAY_SET_OFFSET_OUT_OF_BOUNDS("offset is out of bounds"),
    TYPED_ARRAY_SET_SOURCE_TOO_LARGE("Source is too large"),
    TYPED_ARRAY_TOO_LARGE_TO_SORT("Custom comparefn not supported for huge TypedArrays"),
    VALUE_OUT_OF_RANGE("Value % out of range for % options property %"),
    COLLECTION_GROW_FAILED("% maximum size exceeded"),
    MUST_BE_POSITIVE("% must be positive"),
    /* SyntaxError */
    AMBIGUOUS_EXPORT("The requested module '%' contains conflicting star exports for name '%'"),
    BAD_GETTER_ARITY("Getter must not have any formal parameters."),
    BAD_SETTER_ARITY("Setter must have exactly one formal parameter."),
    BIGINT_INVALID_STRING("Invalid BigInt string"),
    CONSTRUCTOR_IS_ACCESSOR("Class constructor may not be an accessor"),
    CONSTRUCTOR_IS_GENERATOR("Class constructor may not be a generator"),
    CONSTRUCTOR_IS_ASYNC("Class constructor may not be an async method"),
    CONSTRUCTOR_IS_PRIVATE("Class constructor may not be a private method"),
    DERIVED_CONSTRUCTOR_RETURNED_NON_OBJECT("Derived constructors may only return object or undefined"),
    DUPLICATE_CONSTRUCTOR("A class may only have one constructor"),
    DUPLICATE_EXPORT("Duplicate export of '%'"),
    DUPLICATE_PROTO("Duplicate __proto__ fields are not allowed in object literals"),
    FOR_IN_OF_LOOP_INITIALIZER("% loop variable declaration may not have an initializer."),
    FOR_OF_LET("The left-hand side of a for-of loop may not start with 'let'."),
    FOR_OF_ASYNC("The left-hand side of a for-of loop may not be 'async'."),
    FOR_IN_OF_LOOP_MULTI_BINDINGS("Invalid left-hand side in % loop: Must have a single binding."),
    GENERATOR_IN_SINGLE_STATEMENT_CONTEXT("Generators can only be declared at the top level or inside a block."),
    ASYNC_FUNCTION_IN_SINGLE_STATEMENT_CONTEXT("Async functions can only be declared at the top level or inside a  block."),
    ILLEGAL_BREAK("Illegal break statement"),
    MODULE_EXPORT_NAME_WITHOUT_FROM_CLAUSE("String literal module export names must be followed by a 'from' clause"),
    NO_ITERATION_STATEMENT("Illegal continue statement: no surrounding iteration statement"),
    ILLEGAL_CONTINUE("Illegal continue statement: '%' does not denote an iteration statement"),
    ILLEGAL_LANGUAGE_MODE_DIRECTIVE("Illegal '%' directive in function with non-simple parameter list"),
    ILLEGAL_RETURN("Illegal return statement"),
    INTRINSIC_WITH_SPREAD("Intrinsic calls do not support spread arguments"),
    INVALID_REST_BINDING_PATTERN("`...` must be followed by an identifier in declaration contexts"),
    INVALID_PROPERTY_BINDING_PATTERN("Illegal property in declaration context"),
    INVALID_REST_ASSIGNMENT_PATTERN("`...` must be followed by an assignable reference in assignment  contexts"),
    INVALID_ESCAPED_RESERVED_WORD("Keyword must not contain escaped characters"),
    INVALID_ESCAPED_META_PROPERTY("'%' must not contain escaped characters"),
    INVALID_LHS_IN_ASSIGNMENT("Invalid left-hand side in assignment"),
    INVALID_COVER_INITIALIZED_NAME("Invalid shorthand property initializer"),
    INVALID_DESTRUCTURING_TARGET("Invalid destructuring assignment target"),
    INVALID_LHS_IN_FOR("Invalid left-hand side in for-loop"),
    INVALID_LHS_IN_POSTFIX_OP("Invalid left-hand side expression in postfix operation"),
    INVALID_LHS_IN_PREFIX_OP("Invalid left-hand side expression in prefix operation"),
    INVALID_MODULE_EXPORT_NAME("Invalid module export name: contains unpaired surrogate"),
    INVALID_REG_EXP_FLAGS("Invalid flags supplied to RegExp constructor '%'"),
    INVALID_OR_UNEXPECTED_TOKEN("Invalid or unexpected token"),
    INVALID_PRIVATE_BRAND_INSTANCE("Receiver must be an instance of class %"),
    INVALID_PRIVATE_BRAND_STATIC("Receiver must be class %"),
    INVALID_PRIVATE_BRAND_REINITIALIZATION("Cannot initialize private methods of class % twice on the same object"),
    INVALID_PRIVATE_FIELD_REINITIALIZATION("Cannot initialize % twice on the same object"),
    INVALID_PRIVATE_FIELD_RESOLUTION("Private field '%' must be declared in an enclosing class"),
    INVALID_PRIVATE_MEMBER_READ("Cannot read private member % from an object whose class did not declare  it"),
    INVALID_PRIVATE_MEMBER_WRITE("Cannot write private member % to an object whose class did not declare  it"),
    INVALID_PRIVATE_METHOD_WRITE("Private method '%' is not writable"),
    INVALID_PRIVATE_GETTER_ACCESS("'%' was defined without a getter"),
    INVALID_PRIVATE_SETTER_ACCESS("'%' was defined without a setter"),
    INVALID_UNUSED_PRIVATE_STATIC_METHOD_ACCESSED_BY_DEBUGGER("Unused static private method '%' cannot be accessed at debug time"),
    JSON_PARSE_UNEXPECTED_E_O_S("Unexpected end of JSON input"),
    JSON_PARSE_UNEXPECTED_TOKEN_NUMBER("Unexpected number in JSON at position %"),
    JSON_PARSE_UNEXPECTED_TOKEN_STRING("Unexpected string in JSON at position %"),
    JSON_PARSE_UNTERMINATED_STRING("Unterminated string in JSON at position %"),
    JSON_PARSE_EXPECTED_PROP_NAME_OR_R_BRACE("Expected property name or '}' in JSON at position %"),
    JSON_PARSE_EXPECTED_COMMA_OR_R_BRACK("Expected ',' or ']' after array element in JSON at position %"),
    JSON_PARSE_EXPECTED_COMMA_OR_R_BRACE("Expected ',' or '}' after property value in JSON at position  %"),
    JSON_PARSE_EXPECTED_DOUBLE_QUOTED_PROPERTY_NAME("Expected double-quoted property name in JSON at position %"),
    JSON_PARSE_EXPONENT_PART_MISSING_NUMBER("Exponent part is missing a number in JSON at position %"),
    JSON_PARSE_EXPECTED_COLON_AFTER_PROPERTY_NAME("Expected ':' after property name in JSON at position %"),
    JSON_PARSE_UNTERMINATED_FRACTIONAL_NUMBER("Unterminated fractional number in JSON at position %"),
    JSON_PARSE_UNEXPECTED_NON_WHITE_SPACE_CHARACTER("Unexpected non-whitespace character after JSON at position  %"),
    JSON_PARSE_BAD_ESCAPED_CHARACTER("Bad escaped character in JSON at position %"),
    JSON_PARSE_BAD_CONTROL_CHARACTER("Bad control character in string literal in JSON at position %"),
    JSON_PARSE_BAD_UNICODE_ESCAPE("Bad Unicode escape in JSON at position %"),
    JSON_PARSE_NO_NUMBER_AFTER_MINUS_SIGN("No number after minus sign in JSON at position %"),
    JSON_PARSE_SHORT_STRING("\"%\" is not valid JSON"),
    JSON_PARSE_UNEXPECTED_TOKEN_SHORT_STRING("Unexpected token '%', \"%\" is not valid JSON"),
    JSON_PARSE_UNEXPECTED_TOKEN_SURROUND_STRING_WITH_CONTEXT("Unexpected token '%', ...\"%\"... is not valid JSON"),
    JSON_PARSE_UNEXPECTED_TOKEN_END_STRING_WITH_CONTEXT("Unexpected token '%', ...\"%\" is not valid JSON"),
    JSON_PARSE_UNEXPECTED_TOKEN_START_STRING_WITH_CONTEXT("Unexpected token '%', \"%\"... is not valid JSON"),
    LABEL_REDECLARATION("Label '%' has already been declared"),
    LABELLED_FUNCTION_DECLARATION("Labelled function declaration not allowed as the body of a control flow  structure"),
    MALFORMED_ARROW_FUN_PARAM_LIST("Malformed arrow function parameter list"),
    MALFORMED_REG_EXP("Invalid regular expression: /%/%: %"),
    MALFORMED_REG_EXP_FLAGS("Invalid regular expression flags"),
    MODULE_EXPORT_UNDEFINED("Export '%' is not defined in module"),
    MISSING_FUNCTION_NAME("Function statements require a function name"),
    HTML_COMMENT_IN_MODULE("HTML comments are not allowed in modules"),
    MULTIPLE_DEFAULTS_IN_SWITCH("More than one default clause in switch statement"),
    NEWLINE_AFTER_THROW("Illegal newline after throw"),
    NO_CATCH_OR_FINALLY("Missing catch or finally after try"),
    PARAM_AFTER_REST("Rest parameter must be last formal parameter"),
    FLATTEN_PAST_SAFE_LENGTH("Flattening % elements on an array-like of length %  is disallowed, as the total surpasses 2**53-1"),
    PUSH_PAST_SAFE_LENGTH("Pushing % elements on an array-like of length %  is disallowed, as the total surpasses 2**53-1"),
    ELEMENT_AFTER_REST("Rest element must be last element"),
    BAD_SETTER_REST_PARAMETER("Setter function argument must not be a rest parameter"),
    PARAM_DUPE("Duplicate parameter name not allowed in this context"),
    ARG_STRING_TERMINATES_PARAMETERS_EARLY("Arg string terminates parameters early"),
    UNEXPECTED_END_OF_ARG_STRING("Unexpected end of arg string"),
    REST_DEFAULT_INITIALIZER("Rest parameter may not have a default initializer"),
    RUNTIME_WRONG_NUM_ARGS("Runtime function given wrong number of arguments"),
    SUPER_NOT_CALLED("Must call super constructor in derived class before accessing 'this' or returning from derived constructor"),
    SINGLE_FUNCTION_LITERAL("Single function literal required"),
    SLOPPY_FUNCTION("In non-strict mode code, functions can only be declared at top level, inside a block, or as the body of an if statement."),
    SPECIES_NOT_CONSTRUCTOR("object.constructor[Symbol.species] is not a constructor"),
    STRICT_DELETE("Delete of an unqualified identifier in strict mode."),
    STRICT_EVAL_ARGUMENTS("Unexpected eval or arguments in strict mode"),
    STRICT_FUNCTION("In strict mode code, functions can only be declared at top level or inside a block."),
    STRICT_OCTAL_LITERAL("Octal literals are not allowed in strict mode."),
    STRICT_DECIMAL_WITH_LEADING_ZERO("Decimals with leading zeros are not allowed in strict mode."),
    STRICT_OCTAL_ESCAPE("Octal escape sequences are not allowed in strict mode."),
    STRICT8_OR9_ESCAPE("\\8 and \\9 are not allowed in strict mode."),
    STRICT_WITH("Strict mode code may not include a with statement"),
    TEMPLATE_OCTAL_LITERAL("Octal escape sequences are not allowed in template strings."),
    TEMPLATE8_OR9_ESCAPE("\\8 and \\9 are not allowed in template strings."),
    THIS_FORMAL_PARAMETER("'this' is not a valid formal parameter name"),
    AWAIT_BINDING_IDENTIFIER("'await' is not a valid identifier name in an async function"),
    AWAIT_EXPRESSION_FORMAL_PARAMETER("Illegal await-expression in formal parameters of async function"),
    TOO_MANY_ARGUMENTS("Too many arguments in function call (only 65535 allowed)"),
    TOO_MANY_PARAMETERS("Too many parameters in function definition (only 65534 allowed)"),
    TOO_MANY_PROPERTIES("Too many properties to enumerate"),
    TOO_MANY_SPREADS("Literal containing too many nested spreads (up to 65534 allowed)"),
    TOO_MANY_VARIABLES("Too many variables declared (only 4194303 allowed)"),
    TOO_MANY_ELEMENTS_IN_PROMISE_COMBINATOR("Too many elements passed to Promise.%"),
    TYPED_ARRAY_TOO_SHORT("Derived TypedArray constructor created an array which was too small"),
    UNEXPECTED_E_O_S("Unexpected end of input"),
    UNEXPECTED_PRIVATE_FIELD("Unexpected private field"),
    UNEXPECTED_RESERVED("Unexpected reserved word"),
    UNEXPECTED_STRICT_RESERVED("Unexpected strict mode reserved word"),
    UNEXPECTED_SUPER("'super' keyword unexpected here"),
    UNEXPECTED_NEW_TARGET("new.target expression is not allowed here"),
    UNEXPECTED_TEMPLATE_STRING("Unexpected template string"),
    UNEXPECTED_TOKEN("Unexpected token '%'"),
    UNEXPECTED_TOKEN_UNARY_EXPONENTIATION("Unary operator used immediately before exponentiation expression. Parenthesis must be used to disambiguate operator precedence"),
    UNEXPECTED_TOKEN_IDENTIFIER("Unexpected identifier '%'"),
    UNEXPECTED_TOKEN_NUMBER("Unexpected number"),
    UNEXPECTED_TOKEN_STRING("Unexpected string"),
    UNEXPECTED_TOKEN_REG_EXP("Unexpected regular expression"),
    UNEXPECTED_LEXICAL_DECLARATION("Lexical declaration cannot appear in a single-statement context"),
    UNKNOWN_LABEL("Undefined label '%'"),
    UNRESOLVABLE_EXPORT("The requested module '%' does not provide an export named '%'"),
    UNTERMINATED_ARG_LIST("missing ) after argument list"),
    UNTERMINATED_REG_EXP("Invalid regular expression: missing /"),
    UNTERMINATED_TEMPLATE("Unterminated template literal"),
    UNTERMINATED_TEMPLATE_EXPR("Missing } in template expression"),
    FOUND_NON_CALLABLE_HAS_INSTANCE("Found non-callable @@hasInstance"),
    INVALID_HEX_ESCAPE_SEQUENCE("Invalid hexadecimal escape sequence"),
    INVALID_UNICODE_ESCAPE_SEQUENCE("Invalid Unicode escape sequence"),
    UNDEFINED_UNICODE_CODE_POINT("Undefined Unicode code-point"),
    YIELD_IN_PARAMETER("Yield expression not allowed in formal parameter"),
    /* EvalError */
    CODE_GEN_FROM_STRINGS("%"),
    NO_SIDE_EFFECT_DEBUG_EVALUATE("Possible side-effect in debug-evaluate"),
    /* URIError */
    U_R_I_MALFORMED("URI malformed"),
    /* DataCloneError messages */
    DATA_CLONE_ERROR("% could not be cloned."),
    DATA_CLONE_ERROR_OUT_OF_MEMORY("Data cannot be cloned, out of memory."),
    DATA_CLONE_ERROR_DETACHED_ARRAY_BUFFER("An ArrayBuffer is detached and could not be cloned."),
    DATA_CLONE_ERROR_NON_DETACHABLE_ARRAY_BUFFER("ArrayBuffer is not detachable and could not be cloned."),
    DATA_CLONE_ERROR_SHARED_ARRAY_BUFFER_TRANSFERRED("A SharedArrayBuffer could not be cloned. SharedArrayBuffer must not be transferred."),
    DATA_CLONE_DESERIALIZATION_ERROR("Unable to deserialize cloned data."),
    DATA_CLONE_DESERIALIZATION_VERSION_ERROR("Unable to deserialize cloned data due to invalid or unsupported version."),
    /* Builtins-Trace Errors */
    TRACE_EVENT_CATEGORY_ERROR("Trace event category must be a string."),
    TRACE_EVENT_NAME_ERROR("Trace event name must be a string."),
    TRACE_EVENT_NAME_LENGTH_ERROR("Trace event name must not be an empty string."),
    TRACE_EVENT_PHASE_ERROR("Trace event phase must be a number."),
    TRACE_EVENT_I_D_ERROR("Trace event id must be a number."),
    /* Weak refs */
    INVALID_WEAK_REFS_UNREGISTER_TOKEN("Invalid unregisterToken ('%')"),
    WEAK_REFS_CLEANUP_MUST_BE_CALLABLE("FinalizationRegistry: cleanup must be callable"),
    INVALID_WEAK_REFS_REGISTER_TARGET("FinalizationRegistry.prototype.register: invalid target"),
    WEAK_REFS_REGISTER_TARGET_AND_HOLDINGS_MUST_NOT_BE_SAME("FinalizationRegistry.prototype.register: target and holdings must not be same"),
    INVALID_WEAK_REFS_WEAK_REF_CONSTRUCTOR_TARGET("WeakRef: invalid target"),
    OPTIONAL_CHAINING_NO_NEW("Invalid optional chain from new expression"),
    OPTIONAL_CHAINING_NO_SUPER("Invalid optional chain from super property"),
    OPTIONAL_CHAINING_NO_TEMPLATE("Invalid tagged template on optional chain"),
    /* AggregateError */
    ALL_PROMISES_REJECTED("All promises were rejected"),
    CANNOT_DEEP_FREEZE_OBJECT("Cannot DeepFreeze object of type %"),
    CANNOT_DEEP_FREEZE_VALUE("Cannot DeepFreeze non-const value %")
}
