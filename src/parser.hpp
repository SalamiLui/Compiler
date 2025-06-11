#pragma once

#include "tokenization.hpp"
#include "arena.hpp"
#include <variant>


/**
 * @brief Represents a character literal in the source code.
 *
 * This structure holds a token representing a character literal, typically enclosed
 * in single quotes (e.g., `'a'`).
 */
struct NodeTermCharLit {
    Token char_lit;
};

/**
 * @brief Represents an integer literal in the source code.
 *
 * This structure holds a token representing an integer literal, which can be either
 * positive or negative (e.g., `42`, `-7`).
 */
struct NodeTermIntLit {
    Token int_lit;
};

/**
 * @brief Represents a variable in the source code.
 *
 * This structure holds a token representing a variable (identifier) in the source code,
 * such as a variable name or function name.
 */
struct NodeTermIdent {
    Token ident;
};

struct NodeExpr;

/**
 * @brief Represents a function identifier term in the source code.
 *
 * This structure holds a function identifier along with its arguments. The function
 * can be used as a term in expressions, and its return value is typically used in
 * computations.
 *
 * @example
 * let i = genNum(); // function genNum() is used as a term.
 * if (isEqual()) { ...} // function isEqual() is used as a term.
 */
struct NodeTermFunIdent {
    std::string name;
    std::vector<NodeExpr*> args;
};

/**
 * @brief Represents an array element access with an index.
 *
 * This structure represents an array access, where the array identifier is combined
 * with an index expression. The index is an expression that is evaluated to determine
 * the specific element of the array being accessed.
 *
 * @example
 * // Example usage: array[i] + array[i+1]
 * let i = array[0]; // Access the first element of the array.
 */
struct NodeTermArrayIndex {
    Token ident;
    NodeExpr* index{};
};

/**
 * @brief Represents a parenthesized expression in the source code.
 *
 * This structure holds an expression that is enclosed in parentheses. Parentheses are
 * used to prioritize an expression, ensuring that it is computed before other terms in
 * the expression. It can be used as a regular arithmetic operation to group terms.
 *
 * @example
 * // Example usage: (2 + 1) is computed first before multiplication.
 * let result = 1 * (2 + 1);
 */
struct NodeTermParen {
    NodeExpr* expr;
};

/**
 * @brief Represents an addition operation between two expressions.
 *
 * This structure represents an addition operation (`lhs + rhs`) where `lhs` and `rhs`
 * are the left-hand side and right-hand side operands, respectively.
 */
struct NodeBinExprAdd {
    NodeExpr* lhs;
    NodeExpr* rhs;
};

/**
 * @brief Represents a multiplication operation between two expressions.
 *
 * This structure represents a multiplication operation (`lhs * rhs`) where `lhs` and `rhs`
 * are the left-hand side and right-hand side operands, respectively.
 */
struct NodeBinExprMulti {
    NodeExpr* lhs;
    NodeExpr* rhs;
};

/**
 * @brief Represents a division operation between two expressions.
 *
 * This structure represents a division operation (`lhs / rhs`) where `lhs` and `rhs`
 * are the left-hand side and right-hand side operands, respectively.
 */
struct  NodeBinExprDiv {
    NodeExpr* lhs;
    NodeExpr* rhs;
};

/**
 * @brief Represents a subtraction operation between two expressions.
 *
 * This structure represents a subtraction operation (`lhs - rhs`) where `lhs` and `rhs`
 * are the left-hand side and right-hand side operands, respectively.
 */
struct  NodeBinExprSub {
    NodeExpr* lhs;
    NodeExpr* rhs;
};

/**
 * @brief Represents an equality comparison between two expressions.
 *
 * This structure represents an equality comparison (`lhs == rhs`) where `lhs` and `rhs`
 * are the left-hand side and right-hand side operands, respectively.
 */
struct NodeBoolExprEq {
    NodeExpr* lhs;
    NodeExpr* rhs;
};

/**
 * @brief Represents a less-than comparison between two expressions.
 *
 * This structure represents a less-than comparison (`lhs < rhs`) where `lhs` and `rhs`
 * are the left-hand side and right-hand side operands, respectively.
 */
struct NodeBoolExprLess {
    NodeExpr* lhs;
    NodeExpr* rhs;
};

/**
 * @brief Represents a greater-than comparison between two expressions.
 *
 * This structure represents a greater-than comparison (`lhs > rhs`) where `lhs` and `rhs`
 * are the left-hand side and right-hand side operands, respectively.
 */
struct NodeBoolExprGreat {
    NodeExpr* lhs;
    NodeExpr* rhs;
};

/**
 * @brief Represents a not-equal comparison between two expressions.
 *
 * This structure represents a not-equal comparison (`lhs != rhs`) where `lhs` and `rhs`
 * are the left-hand side and right-hand side operands, respectively.
 */
struct NodeBoolExprNeq {
    NodeExpr* lhs;
    NodeExpr* rhs;
};

/**
 * @brief Represents a term in an expression.
 *
 * This structure can hold various types of terms, such as an identifier, an integer literal,
 * a parenthesized expression, a character literal, a function identifier, or an array element
 * access. The term is typically a value or an expression that can be used in larger computations.
 *
 */
struct NodeTerm {
    std::variant<
        NodeTermIdent*,
        NodeTermIntLit*,
        NodeTermParen*,
        NodeTermCharLit*,
        NodeTermFunIdent*,
        NodeTermArrayIndex*
            > var;
};

/**
 * @brief Represents a binary arithmetic expression.
 *
 * This structure can hold various types of binary arithmetic expressions, such as addition,
 * multiplication, division, and subtraction. The specific type of arithmetic operation is
 * represented as a pointer to one of the following:
 * - `NodeBinExprAdd`: Represents an addition operation.
 * - `NodeBinExprMulti`: Represents a multiplication operation.
 * - `NodeBinExprDiv`: Represents a division operation.
 * - `NodeBinExprSub`: Represents a subtraction operation.
 */
struct NodeBinExpr {
    std::variant<NodeBinExprAdd*, NodeBinExprMulti*, NodeBinExprDiv*, NodeBinExprSub*> var;
};

/**
 * @brief Represents a binary comparison expression.
 *
 * This structure can hold various types of binary comparison expressions, such as equality,
 * inequality, greater than, and less than comparisons. The specific comparison operation is
 * represented as a pointer to one of the following:
 * - `NodeBoolExprEq`: Represents an equality comparison (`==`).
 * - `NodeBoolExprNeq`: Represents an inequality comparison (`!=`).
 * - `NodeBoolExprGreat`: Represents a "greater than" comparison (`>`).
 * - `NodeBoolExprLess`: Represents a "less than" comparison (`<`).
 * - `NodeExpr`: Represents a general expression in the comparison.
 */
struct NodeBoolExpr {
    std::variant<NodeBoolExprEq*, NodeBoolExprNeq*, NodeBoolExprGreat*, NodeBoolExprLess*, NodeExpr*> var;
};


/**
 * @brief Represents an expression that can be either a term or a binary expression.
 *
 * An expression can be a simple term such as an integer, character, or a more complex binary
 * expression involving arithmetic operations between terms. The term or binary expression is
 * represented as a variant of `NodeTerm*` or `NodeBinExpr*`, respectively.
 *
 * @example
 * For a simple arithmetic expression: `1 + 2 * 3`, the expression is parsed as follows:
 * - `1` is a term.
 * - `2 * 3` is a binary expression.
 *
 * @example
 * For a variable expression: `x / y`, the expression contains terms (`x` and `y`) with
 * a division operation.
 */
struct NodeExpr {
    std::variant<NodeTerm*, NodeBinExpr*> var;
    TokenType type;
};

/**
 * @brief Represents an exit statement with an expression to return as the exit code.
 *
 * This structure represents the `exit(expr)` statement where the program exits with
 * the value of `expr` as the exit code.
 */
struct NodeStmtExit {
    NodeExpr* expr;
};

/**
 * @brief Represents a statement to declare an identifier (a variable).
 *
 * This structure represents a `let` statement used to declare a variable. It can
 * be followed by an optional type declaration and an expression to initialize the
 * variable.
 *
 * Examples:
 * - let i = 12;
 * - let i = x;
 * - let i : int = add(1, 2);
 * - let c : char = 'j';
 */
struct NodeStmtLet { // NOLINT(*-pro-type-member-init)
    /**
     * @brief The identifier (variable) being declared.
     */
    Token ident;
    /**
    * @brief The expression used to initialize the variable.
    */
    NodeExpr* expr;
    /**
    * @brief The optional type of the variable (e.g., `int`, `char`).
    * If not provided, the type is inferred.
    */
    std::optional<TokenType> type;
};

/**
 * @brief Represents a statement to declare an array variable.
 *
 * This structure represents a `let` statement used to declare an array variable.
 * It can either have a specified size, or the size can be inferred from an expression.
 * It also includes the type of the array elements.
 *
 * Examples:
 * - let array[] : int = [1, 2, 3, 4]; // Array with inferred size.
 * - let array[4] : int; // Array with fixed size of 4.
 * - let array[ident] : int = [expr*]; // Array with size defined by an identifier expression.
 */
struct NodeStmtLetArray {
    /**
     * @brief The identifier (name) of the array variable.
     */
    Token ident;
    /**
     * @brief The optional size of the array. If not provided, the size is inferred from the initializer.
     */
    std::optional<NodeTermIntLit*> size;
    /**
     * @brief The type of the elements in the array (e.g., `int`, `char`).
     */
    TokenType type;
    /**
     * @brief The expressions used to initialize the array elements.
     */
    std::vector<NodeExpr*> exprs;
};

struct NodeStmt;

/**
 * @brief Represents a scope containing a list of statements inside curly braces `{}`.
 *
 * This structure encapsulates a scope in the program, which contains a series of statements.
 * A scope is typically represented by a block of code enclosed within `{ }`, such as in control structures or functions.
 *
 * After the execution of the scope, the stack of variables is cleaned up, ensuring that only the variables declared
 * within the current scope persist until the scope ends.
 *
 * Example:
 * - Initial stack size = 10
 * - Start of scope {
 *     - Statements (stms*)
 * - End of scope }
 * - Final stack size = 12
 * - Variables that will be popped from the stack = final stack size - initial stack size
 *    - 12 - 10 = 2 variables
 *
 * If the scope belongs to a function, variables declared in the outer scope will be temporarily forgotten, allowing
 * for variables with the same name to be declared inside the scope.
 *
 * Note: Inside a scope, you cannot declare a function.
 */
struct NodeScope {
    std::vector<NodeStmt> stmts;
};

struct NodeStmtIf;

/**
 * @brief Represents the 'else' part of a conditional statement.
 *
 * This structure is used in conjunction with an `if` statement. The `else` block executes
 * if the `if` condition is not satisfied. The `else` can contain either a single scope
 * of statements or a subsequent `else if` statement.
 *
 * Example:
 * - `else { [scope] }`
 * - `else if (condition) { [scope] }`
 */
struct NodeElse {
    /**
     * @brief The scope of statements to execute if the condition is false.
     *
     * This represents the body of the `else` block, which will be executed if
     * the `if` condition evaluates to false. The `scope` holds the list of statements
     * to be executed inside the `else` block.
     */
    NodeScope* scope = nullptr;
    /**
     * @brief A reference to the associated `if` statement.
     *
     * If the `else` follows another `if` statement, this pointer holds the reference
     * to the corresponding `NodeStmtIf` structure. If the `else` is a standalone
     * statement, this will be nullptr.
     */
    NodeStmtIf* _if = nullptr;

};

/**
 * @brief Represents an 'if' statement.
 *
 * This structure models an `if` statement, where a boolean expression is evaluated.
 * If the expression evaluates to true, a block of statements (scope) is executed.
 * Optionally, an `else` block or another `else if` can follow the `if` statement.
 *
 * Example:
 * - `if ([bool_expr]){ [scope] }`
 * - `if ([bool_expr]){ [scope] } else { [scope] }`
 * - `if ([bool_expr]){ [scope] } else if ([bool_expr]){ [scope] } ...`
 */
struct NodeStmtIf { // NOLINT(*-pro-type-member-init)
    /**
     * @brief The boolean expression that controls the flow of the `if` statement.
     *
     * This is the condition that will be evaluated. If the condition is true,
     * the scope inside the `if` block will be executed.
     */
    NodeBoolExpr* bool_expr;
    /**
     * @brief The scope of statements to execute if the condition is true.
     *
     * This represents the body of the `if` block, which contains the statements
     * that will be executed if the boolean expression evaluates to true.
     */
    NodeScope* scope;
    /**
     * @brief An optional `else` or `else if` block.
     *
     * If the condition in the `if` statement evaluates to false, the corresponding
     * `else` or `else if` block will be executed. This is a recursive structure,
     * allowing for multiple `else if` statements.
     */
    NodeElse* _else = nullptr;
};

/**
 * @brief Represents an assignment statement for a variable.
 *
 * This structure models an assignment statement where an identifier (variable)
 * is assigned a value or expression. The variable's value can be a literal or
 * a result of an expression.
 *
 * Example:
 * - `var = 12;`
 * - `var = x + y;`
 */
struct NodeStmtIdent { // NOLINT(*-pro-type-member-init)
    /**
     * @brief The identifier (variable) to which the value is assigned.
     *
     * This token represents the variable that will be assigned a value or
     * expression. It stores the name of the variable being assigned.
     */
    Token ident;
    /**
    * @brief The expression or value to be assigned to the identifier.
    *
    * This expression will be evaluated and assigned to the variable
    * identified by `ident`.
    */
    NodeExpr* expr;
    /**
     * @brief The type of the variable being assigned.
     *
     * This is an optional field that holds the type of the identifier if
     * explicitly declared. If the type is not provided, it won't change from
     * its original type
     */
    std::optional<TokenType> type;
};

/**
 * @brief Represents an assignment to an element in an array.
 *
 * This structure models a statement where a specific index in an array
 * is assigned a value or expression. The array element is accessed using
 * an index, and the resulting value is assigned to that element.
 *
 * Example:
 * - `array[0] = 1;`
 * - `array[i] = j;`
 * - `[ident][index] = [expr];`
 */
struct NodeStmtArrayIndex {
    /**
    * @brief The array identifier.
    *
    * This token represents the name of the array where an element is
    * being assigned a value. It stores the identifier of the array.
    */
    Token ident;
    /**
    * @brief The index in the array where the value is assigned.
    *
    * This expression represents the index of the array element to which
    * the value will be assigned. It can be a literal or an expression.
    */
    NodeExpr* index{};
    /**
     * @brief The expression or value to be assigned to the array element.
     *
     * This expression is evaluated and assigned to the element in the array
     * identified by `ident` at the specified `index`.
     */
    NodeExpr* expr{};
};

/**
 * @brief Represents a 'for' loop statement in the source code.
 *
 * This structure models the components of a 'for' loop statement, which
 * includes an initialization, a condition to check during each iteration,
 * an update expression after each iteration, and a block of statements to
 * execute within the loop.
 *
 * Example:
 * - `for (let i = 0; i < 10; i++) { ... }`
 * - `for (var j = 0; j < 5; j++) { ... }`
 */
struct NodeStmtFor {
    /**
    * @brief The scope of statements executed within the loop.
    *
    * This represents the block of statements that will be executed
    * during each iteration of the loop. The `scope` holds the statements
    * enclosed by the curly braces `{ }`.
    */
    NodeScope* scope;
    /**
     * @brief The condition that is checked before each iteration.
     *
     * This is the Boolean expression that must evaluate to `true` for the loop
     * to continue iterating. If it evaluates to `false`, the loop terminates.
     */
    NodeBoolExpr* condition;
    /**
     * @brief The initialization statement of the loop.
     *
     * This represents the initialization of the loop, where variables are
     * typically declared or initialized before the loop begins. It can be
     * a `NodeStmtLet` or `NodeStmtIdent`, depending on how the initialization
     * is written in the source code.
     */
    std::variant<NodeStmtLet* , NodeStmtIdent*> init;
    /**
    * @brief The update statement of the loop.
    *
    * This is the statement executed after each iteration of the loop. It
    * typically updates the loop's control variable(s), such as incrementing
    * or decrementing an index.
    */
    NodeStmtIdent* update;

};

/**
 * @brief Represents a 'while' loop statement in the source code.
 *
 * This structure models the components of a 'while' loop, which consists of
 * a condition that is checked before each iteration, and a block of statements
 * to execute while the condition remains true.
 *
 * Example:
 * - `while (i < 10) { ... }`
 */
struct NodeStmtWhile {
    /**
     * @brief The scope of statements executed while the condition is true.
     *
     * This represents the block of statements that will be executed repeatedly
     * as long as the condition evaluates to true. The `scope` holds the statements
     * enclosed by the curly braces `{ }`.
     */
    NodeScope* scope;
    /**
     * @brief The condition that is checked before each iteration.
     *
     * This is the Boolean expression that must evaluate to `true` for the loop
     * to continue iterating. If it evaluates to `false`, the loop terminates.
     */
    NodeBoolExpr* condition;
};

/**
 * @brief Represents a print statement in the source code.
 *
 * This structure models a statement that prints the result of an expression.
 *
 * Example:
 * - `print(x);`
 * - `print(2 + 3);`
 */
struct NodeStmtPrint {
    /**
     * @brief The expression to be printed.
     *
     * This is the expression whose value will be printed when the statement
     * is executed. It could be any valid expression, such as an integer,
     * char, or result of an arithmetic operation.
     */
    NodeExpr* expr;
};

/**
 * @brief Represents a function call where the return value is not used.
 *
 * This structure models a function call whose return value is ignored.
 * The function is treated as a statement, executed for its side effects (if any).
 *
 * Example:
 * - `doSomething();`
 * - `checkThis();`
 */
struct NodeStmtFunIdent {
    /**
     * @brief The name of the function being called.
     *
     * This represents the identifier of the function that is being invoked.
     * It is a string containing the function's name.
     */
    std::string name;
    /**
     * @brief The arguments passed to the function.
     *
     * This vector contains the expressions (arguments) passed to the function.
     * The number and type of arguments should match the function's definition
     * in the language being parsed.
     */
    std::vector<NodeExpr*> args;
};

/**
 * @brief Represents a function declaration.
 *
 * This structure models the declaration of a function, which includes its name,
 * parameter list (with identifiers and their types), return type, and the scope of
 * the function (the body of the function).
 *
 * Example:
 * - `fun Add(n1 : int, n2 : int) -> int {...}`
 *
 * The function declaration defines the signature of the function, including the
 * types of its parameters and its return type.
 */
struct NodeStmtFun {
    /**
    * @brief The name of the function.
    */
    std::string name;
    /**
     * @brief The scope of the function.
     *
     * This pointer to a `NodeScope` contains the statements that are executed
     * within the body of the function. The scope defines the function's behavior
     * and what is computed when the function is called.
     */
    NodeScope* scope;
    /**
    * @brief The list of parameter identifiers for the function.
    *
    * This vector contains the identifiers (names) of the parameters that the
    * function accepts. Each element represents a single parameter's name.
    */
    std::vector<Token> idents;
    /**
     * @brief The list of types for each parameter in the function.
     *
     * This vector contains the types associated with each parameter. The
     * types correspond to the identifiers in `idents` and define the type
     * of each parameter. If not especified the type is automatically int.
     */
    std::vector<std::optional<TokenType>> types;
    /**
     * @brief The return type of the function.
     *
     * This field defines the type of value that the function returns.
     * If the function has no return value (i.e., it is of type `void`).
     */
    TokenType returnType;
};

/**
 * @brief Represents a return statement in the code.
 *
 * This structure models a return statement, which can either return a value
 * or simply return without any value.
 *
 * Example 1: `return 42;`
 * Example 2: `return;`
 *
 * - If `expr` is not `nullptr`, it represents the expression being returned.
 * - If `expr` is `nullptr`, it indicates that the function returns without a value.
 *
 * If return is not used inside a function it exits and an exit code (expretion)
 * must be specified
 */
struct NodeStmtReturn {
    NodeExpr* expr = nullptr;
};

/**
 * @brief Represents a statement in the code.
 *
 * This structure is used to model different types of statements in the source code.
 * It uses a `std::variant` to store various types of statements, including control structures,
 * assignments, function calls, and others.
 *
 * This structure allows the parser to handle different types of statements using a common interface.
 */
struct NodeStmt {
    std::variant<
        NodeStmtExit*,
        NodeStmtLet*,
        NodeStmtIf*,
        NodeStmtIdent*,
        NodeStmtFor*,
        NodeStmtWhile*,
        NodeStmtPrint*,
        NodeStmtFun*,
        NodeStmtFunIdent*,
        NodeStmtReturn*,
        NodeStmtArrayIndex*,
        NodeStmtLetArray*
            >
    stmt;
};

/**
 * @brief Represents the entire program as a list of statements.
 *
 * This structure holds a sequence of statements that make up the final program.
 * It is essentially the root node in the abstract syntax tree (AST) of the program.
 *
 * The statements in the `stmts` vector are executed in the order in which they appear in the source code.
 * This includes variable declarations, function calls, control structures (like `if`, `for`, `while`),
 * and other expressions. The program will be processed statement by statement.
 *
 */
struct NodeProg {
    std::vector<NodeStmt> stmts;
};




class Parser {
public:
    inline explicit Parser(std::vector<Token>& tokens )
        :m_tokens(std::move(tokens)),
        m_allocator(1024 * 1024 * 4) // the arena allocator starting size
    {
    }

    /**
 * @brief Parses the entire program and returns the parsed abstract syntax tree (AST).
 *
 * This function parses the source code by repeatedly calling `parse_stmt()` to process individual
 * statements. It collects each successfully parsed statement into a `NodeProg` structure, which represents
 * the entire program as a sequence of statements. Once the entire source code is processed, it returns
 * the `NodeProg` that contains the parsed program.
 *
 * The process stops when all tokens have been consumed (i.e., when there are no more tokens to process).
 *
 * @return NodeProg The parsed program represented as an abstract syntax tree (AST) of statements.
 *
 */
    NodeProg parse_prog() {
        NodeProg prog;
        while (currentToken().has_value()) {
            prog.stmts.push_back(parse_stmt());
        }
        return prog;
    }


private:

    /**
 * @brief Parses a positive integer literal term in the source code.
 *
 * This function consumes the current token (which must be a positive integer literal) from the source code,
 * creates a `NodeTermIntLit` structure to represent the integer literal term, and returns the created structure.
 *
 * The token representing the integer literal is consumed from the input stream and assigned to the
 * `int_lit` field of the `NodeTermIntLit` structure.
 *
 * @return NodeTermIntLit* A pointer to the `NodeTermIntLit` representing the parsed positive integer literal term.
 */
    NodeTermIntLit* parse_pos_term_int_lit() {
        const auto term_int_lit = m_allocator.alloc<NodeTermIntLit>();
        term_int_lit->int_lit = consume();
        return term_int_lit;
    }

    /**
     * @brief Parses a term in the source code, which can be an integer, character, identifier, or other.
     *
     * This function analyzes the current token in the source code and determines its type, such as integer literals,
     * character literals, identifiers (including functions and array indices), or expressions enclosed in parentheses.
     * The function then returns a `NodeTerm` representing the parsed term.
     *
     * The term can be one of the following types:
     * - A negative integer (e.g., `-5`)
     * - A positive integer (e.g., `42`)
     * - A character literal (e.g., `'a'`)
     * - An identifier (which can be a simple variable, function call, or array index)
     * - A parenthesized expression (e.g., `(1 + 2)`)
     *
     * If the term is a valid one, it is parsed and returned as a `NodeTerm` structure. If the term is invalid, the program exits with an error message.
     *
     * @return NodeTerm* A pointer to the `NodeTerm` representing the parsed term.
     *
     */
    NodeTerm* parse_term() { // NOLINT(*-no-recursion)

        // if the term is a negative int
        if (currentToken().has_value() && currentToken().value().type == TokenType::sub &&
            currentToken(1).has_value() && currentToken(1).value().type == TokenType::int_lit) {
            consume(); // -
            auto term_in_lit = m_allocator.alloc<NodeTermIntLit>();
            term_in_lit->int_lit = consume();
            term_in_lit->int_lit.value->insert(0, "-");
            auto const term = m_allocator.alloc<NodeTerm>();
            term->var = term_in_lit;
            return term;
        }
        // if the term is a positive int
        if (currentToken().has_value() && currentToken().value().type == TokenType::int_lit) {
            auto const term = m_allocator.alloc<NodeTerm>();
            term->var = parse_pos_term_int_lit();
            return term;

        }
        // if the term is a char
        if (currentToken().has_value() && currentToken().value().type == char_lit) {
            auto term_char_lit = m_allocator.alloc<NodeTermCharLit>();
            term_char_lit->char_lit = consume();
            auto const term = m_allocator.alloc<NodeTerm>();
            term->var = term_char_lit;
            return term;

        }

        // if the term is an identifier
        if (currentToken().has_value() && currentToken().value().type == TokenType::ident) {

            // if is a function identifier, example = add(), check() ...
            if (currentToken(1).has_value() && currentToken(1).value().type == TokenType::open_paren) {
                auto const term_fun = parse_fun_ident<NodeTermFunIdent>();
                auto const term = m_allocator.alloc<NodeTerm>();
                term->var = term_fun;
                return term;
            }
            // if is an array, example = array[i] ...
            if (currentToken(1).has_value() && currentToken(1).value().type == TokenType::open_bracket) {
                auto const term_array = m_allocator.alloc<NodeTermArrayIndex>();
                term_array->ident = checkIdent();
                consume(); // '['
                term_array->index = parse_expr();
                checkCloseBracket();
                auto const term = m_allocator.alloc<NodeTerm>();
                term->var = term_array;
                return term;
            }

            // else if is a simple identifier, example = a, x, y, var ...
            auto term_ident = m_allocator.alloc<NodeTermIdent>();
            term_ident->ident = consume();
            auto const term = m_allocator.alloc<NodeTerm>();
            term->var = term_ident;
            return term;
        }
        // if the term is an open paren term
        if (currentToken().has_value() && currentToken().value().type == TokenType::open_paren) {
            consume();
            auto term_paren = m_allocator.alloc<NodeTermParen>();
            NodeExpr* expr = parse_expr();
            term_paren->expr = expr;
            checkCloseParen();
            auto const term = m_allocator.alloc<NodeTerm>();
            term->var = term_paren;
            return term;
        }
        std::cerr << "Invalid term, expected int_literal, char or identifier " << std::endl;
        exit(EXIT_FAILURE);
    }

    /**
     * @brief Parses an arithmetic expression with operator precedence.
     *
     * This function recursively parses an expression by handling arithmetic
     * operators according to their precedence. It ensures that expressions
     * are correctly grouped based on their precedence levels.
     *
     * @param min_prec Minimum precedence required to continue parsing.
     * @return NodeExpr* A pointer to the parsed expression tree.
     */
    NodeExpr* parse_expr(int const min_prec = 0) { // NOLINT(*-no-recursion)
        // in an expr always has to be first a term (1+2), (x-y) ...
        NodeTerm* term = parse_term();
        auto const lhs = m_allocator.alloc<NodeExpr>();
        lhs->var = term;
        while (true) {
            /**
             * @brief Checks if the next token is a valid arithmetic operator.
             *
             * If the token's precedence is -1, it is not an arithmetic operator,
             * and the loop will break.
             */
            if (!currentToken().has_value() ||
                    bin_prec(currentToken().value().type) < min_prec) {
                break;
            }
            Token op = consume();
            // then check the next token term / expr
            NodeExpr* rhs = parse_expr(bin_prec(op.type));
            auto bin_expr = m_allocator.alloc<NodeBinExpr>();
            // lhs2 is going to become the lhs of the bin expr in order to re-use lhs later
            auto const lhs2 = m_allocator.alloc<NodeExpr>();

            if (op.type == TokenType::plus) {
                auto bin_expr_add = m_allocator.alloc<NodeBinExprAdd>();
                lhs2->var = lhs->var;
                bin_expr_add->lhs = lhs2;
                bin_expr_add->rhs = rhs;
                bin_expr->var = bin_expr_add;
            }
            else if (op.type == TokenType::star) {
                auto bin_expr_multi = m_allocator.alloc<NodeBinExprMulti>();
                lhs2->var = lhs->var;
                bin_expr_multi->lhs = lhs2;
                bin_expr_multi->rhs = rhs;
                bin_expr->var = bin_expr_multi;
            }
            else if (op.type == TokenType::slash) {
                auto bin_expr_div = m_allocator.alloc<NodeBinExprDiv>();
                lhs2->var = lhs->var;
                bin_expr_div->lhs = lhs2;
                bin_expr_div->rhs = rhs;
                bin_expr->var = bin_expr_div;
            }
            else if (op.type == TokenType::sub){
                auto bin_expr_sub= m_allocator.alloc<NodeBinExprSub>();
                lhs2->var = lhs->var;
                bin_expr_sub->lhs = lhs2;
                bin_expr_sub->rhs = rhs;
                bin_expr->var = bin_expr_sub;
            }

            lhs->var = bin_expr;
            /** then lhs is used to store the entire bin expr.
             *
             *  if the min prec of the next bin expr is less than the current bin expr
             *  example: +(next bin expr) < *(current bin expr)
             *  then lhs will stay as the lhs expr and the next expr will be the rhs expr
             *  thus the bin expr with a greater value always be generated first
             *  example: 1*2+3
             *                              (+)
             *                              |
             *                     | ------|------|
             *                    (*)             |
             *              |-----|-----|        (3)
             *             |            |        rhs
             *            (1)          (2)
             *            lhs          rhs
             *                  lhs
             *
             *  so is computed first lhs = 1*2 and the result is added to the rhs = 3
             *
             *  otherwise if the mic prec of the next bin expr is greater or equal than the current
             *  example: *(next bin expr) > +(current bin expr)
             *  then the loop will continue and the new bin expr will become the expretion of
             *  the current current rhs expr
             *  example: 1+2*3
             *                          (+)
             *                          |
             *                   |-----|---------|
             *                   |               |
             *                  (1)             (*)
             *                  lhs        |-----|-----|
             *                             |           |
             *                            (2)         (3)
             *                            lhs        rhs
             *                                  rhs
             *
             *  so first is generated the term 1, then in order to add 1 to the rhs
             *  first is needed to compute 2*3
             *
             *  if for example then is added a bin_expr_sub to the previous example
             *  (1+2*3-4), the bin prec of - is less than * but greater than +
             *  so in the recursive chain when the min prec = * the loop will break,
             *  but when the mic prec = +, the loop will continue and the rhs will
             *  now become the bin_expr_sub and it's lhs will be the bin_expr_mul
             *  example 1+2*3-4:
             *                          (+)
             *                          |
             *                   |-----|------------|
             *                  |                  |
             *                 (1)                |
             *                 lhs              (-)
         *                               |-------|---------|
         *                              |                 |
         *                             (*)              (4)
         *                       |-----|-----|          rhs
         *                       |           |
         *                      (2)         (3)
         *                      lhs        rhs
         *                            lhs
         *                                      rhs
         *
         *
             */
        }
        return lhs;

    }

    /**
 * @brief Parses a boolean expression in the source code.
 *
 * This function parses a boolean expression which consists of an expression followed by a comparison operator
 * and another expression, such as:
 * - `1 > x`
 * - `x + y == 5`
 * - `x != y`
 *
 * It first parses an expression (`lhs`), then checks if the next token is a comparison operator.
 * Depending on the operator, it creates the appropriate boolean expression node (e.g., `NodeBoolExprEq`,
 * `NodeBoolExprNeq`, `NodeBoolExprGreat`, or `NodeBoolExprLess`).
 *
 * The function handles the following types of comparisons:
 * - `==` (equal)
 * - `!=` (not equal)
 * - `>` (greater than)
 * - `<` (less than)
 *
 * If no comparison operator is found, the function treats the expression as a simple boolean expression (e.g.,
 * `if (1)`).
 *
 * @return NodeBoolExpr* A pointer to the `NodeBoolExpr` representing the parsed boolean expression.
 *
 * @throws std::exit() if the boolean expression is invalid or if the expected comparison operator is missing.
 *
 */
    NodeBoolExpr* parse_bool_expr() {
        // a bool expr always needs an expr first, examples: 1 > x, x + y == 5, ...
        NodeExpr* lhs= parse_expr();
        auto const bool_expr = m_allocator.alloc<NodeBoolExpr>();

        if (!currentToken().has_value()) {
            std::cerr << "Invalid bool expression" << std::endl;
            exit(EXIT_FAILURE);
        }
        // in case the bool_expr is just a single expretion, example: if (1) {...}
        if(currentToken().value().type == TokenType::close_paren) {
            bool_expr->var = lhs;
            return bool_expr;
        }

        // if the comparition is ==
        if (currentToken().value().type == TokenType::eq &&
            currentToken(1).has_value() &&
            currentToken(1).value().type == TokenType::eq) {

            consume();
            consume();
            auto bool_expr_eq= genBoolExprCmp<NodeBoolExprEq>();
            bool_expr_eq->lhs = lhs;
            bool_expr->var = bool_expr_eq;
        }
        // if the comparition is !=
        else if (currentToken().value().type == TokenType::exc_mark &&
            currentToken(1).has_value() &&
            currentToken(1).value().type == TokenType::eq) {

            consume();
            consume();
            auto bool_expr_neq = genBoolExprCmp<NodeBoolExprNeq>();
            bool_expr_neq->lhs = lhs;
            bool_expr->var = bool_expr_neq;

        }
        // if the comparition is >
        else if (currentToken().value().type == TokenType::great) {
            consume();
            auto bool_expr_great = genBoolExprCmp<NodeBoolExprGreat>();
            bool_expr_great->lhs = lhs;
            bool_expr->var = bool_expr_great;
        }
        // if the comparition is <
        else if (currentToken().value().type == TokenType::less) {
            consume();
            auto bool_expr_less = genBoolExprCmp<NodeBoolExprLess>();
            bool_expr_less->lhs = lhs;
            bool_expr->var = bool_expr_less;
        }
        else {
            std::cerr << "Invalid bool exprssion, expected valid comparition" << std::endl;
            exit(EXIT_FAILURE);
        }
        return bool_expr;

    }

    /**
 * @brief Parses a scope within the program.
 *
 * A scope is a block of statements enclosed by curly braces `{ }`. This function parses the statements inside the scope
 * by repeatedly calling `parse_stmt()` until it encounters the closing curly brace `}`.
 *
 * The scope is parsed as a sequence of statements, and when the closing curly brace is encountered, the function terminates.
 *
 * @return NodeScope* A pointer to the `NodeScope` representing the parsed scope with its list of statements.
 *
 */
    NodeScope* parse_scope() { // NOLINT(*-no-recursion)
        auto const scope = m_allocator.alloc<NodeScope>();
        // while the scope is not finished keep parsing the stmts inside (it's finished when token '{' is found)
            while (currentToken().has_value() &&
                currentToken().value().type != TokenType::close_curly) {

                scope->stmts.push_back(parse_stmt());
            }
        return scope;
    }

    /**
 * @brief Parses an 'if' statement in the source code.
 *
 * This function parses an 'if' statement, which may include an optional 'else' block. The statement consists of a condition
 * (a boolean expression) followed by a block of code enclosed in curly braces `{ }`. Additionally, the 'if' statement may have
 * an 'else' block with a separate scope.
 *
 * The function performs the following steps:
 * 1. Checks for the opening parenthesis `(` and parses the boolean expression.
 * 2. Checks for the closing parenthesis `)`.
 * 3. Parses the scope of statements inside the curly braces `{ }`.
 * 4. Checks if there is an optional 'else' block and, if present, parses it.
 *
 * @return NodeStmtIf* A pointer to the `NodeStmtIf` structure representing the parsed 'if' statement, including the
 *         optional 'else' block and the associated scope.
 *
 */
    NodeStmtIf* parse_if() {  // NOLINT(*-no-recursion)
        auto const stmt_if = m_allocator.alloc<NodeStmtIf>();
        checkOpenParen();
        stmt_if->bool_expr = parse_bool_expr();
        checkCloseParen();
        checkOpenCurly();
        NodeScope* scope = parse_scope();
        checkCloseCurly();
        // check if the 'if' stmt has an else, otherwise stmt_if->_else stays as nullptr
        if (currentToken().has_value() && currentToken().value().type == TokenType::_else) {
            consume();
            stmt_if->_else = parse_else();
        }
        stmt_if->scope = scope;
        return stmt_if;
    }

    /**
 * @brief Parses an 'else' block in an 'if' statement.
 *
 * This function parses the 'else' block following an 'if' statement. The 'else' block can either be:
 * 1. An 'else if' statement, which contains another conditional 'if' statement.
 * 2. A regular 'else' block with a scope of statements enclosed in curly braces `{ }`.
 *
 * The function performs the following steps:
 * 1. Checks if the next token is an 'if' statement (`else if`) and parses it accordingly.
 * 2. If the next token is an open curly brace `{`, it parses the scope of statements inside the 'else' block.
 * 3. If neither of the above conditions are met, an error is reported and the program exits.
 *
 * @return NodeElse* A pointer to the `NodeElse` structure representing the parsed 'else' block.
 *         This structure may contain a nested `if` statement (in case of 'else if') or a scope of statements.
 *
 */
    NodeElse* parse_else() {   // NOLINT(*-no-recursion)
        auto const _else = m_allocator.alloc<NodeElse>();
        // if it's an else if
        if (currentToken().has_value() && currentToken().value().type == TokenType::_if) {
            consume();
            _else->_if = parse_if();
        }
        // if it's just and else
        else if (currentToken().has_value() && currentToken().value().type == TokenType::open_curly) {
            consume();
            _else->scope = parse_scope();
            checkCloseCurly();
        }
        else {
            std::cerr << "Invalid else statement, expected if stmt or scope" << std::endl;
            exit(EXIT_FAILURE);
        }
        return _else;
    }

    /**
 * @brief Parses a 'let' statement to declare a variable.
 *
 * This function parses a 'let' statement, which is used to declare and initialize a variable.
 * The declaration includes the variable identifier, its type, and an optional expression for initialization.
 *
 * The function performs the following steps:
 * 1. Checks and stores the variable identifier.
 * 2. Checks and stores the variable type.
 * 3. Ensures the '=' symbol is present for assignment.
 * 4. Parses and stores the expression that will be assigned to the variable.
 * 5. Ensures a semicolon ';' follows the statement to indicate the end of the statement.
 *
 * @return NodeStmtLet* A pointer to the `NodeStmtLet` structure representing the parsed 'let' statement.
 *         This structure contains the variable identifier, type, and the assigned expression.
 *
 */
    NodeStmtLet* parse_let() {
        auto const stmt_let = m_allocator.alloc<NodeStmtLet>();
        stmt_let->ident = checkIdent();
        stmt_let->type = checkType();
        checkEq();
        stmt_let->expr = parse_expr();
        checkSemi();
        return stmt_let;
    }

    /**
     * @brief Parses an identifier assignment statement.
     *
     * This function parses a statement where an existing variable (identifier) is assigned a new value.
     * The identifier is followed by an expression that will be assigned to it.
     *
     * The function performs the following steps:
     * 1. Consumes and stores the variable identifier.
     * 2. Checks and stores the variable type.
     * 3. Ensures the '=' symbol is present for assignment.
     * 4. Parses and stores the expression that will be assigned to the variable.
     *
     * @return NodeStmtIdent* A pointer to the `NodeStmtIdent` structure representing the parsed assignment statement.
     *         This structure contains the identifier, the type, and the assigned expression.
     *
     */
    NodeStmtIdent* parse_ident() {
        auto const stmt_ident = m_allocator.alloc<NodeStmtIdent>();
        stmt_ident->ident = consume();
        stmt_ident->type = checkType();
        checkEq();
        stmt_ident->expr = parse_expr();
        return stmt_ident;
    }

    /**
 * @brief Parses a function identifier, extracting its name and arguments.
 *
 * This template function parses a function call identifier, which may include arguments in parentheses.
 * The function extracts the function name and its arguments, if any, and returns a pointer to the parsed function identifier node.
 *
 * It supports parsing both function identifiers used within expressions (e.g., `add(1, 2)`) and function calls.
 *
 * The function works by:
 * 1. Consuming the function name.
 * 2. Checking if the function has parameters (i.e., tokens between the parentheses).
 * 3. If there are parameters, parsing them one by one, separated by commas.
 * 4. Ensuring the closing parenthesis `)` is present.
 *
 * @tparam T The type of the node to be returned, either `NodeStmtFunIdent` or `NodeTermFunIdent`, depending on context.
 *
 * @return T* A pointer to the parsed function identifier node, containing the function name and arguments.
 *
 */
    template<typename T>
    T* parse_fun_ident() { // NOLINT(*-no-recursion)
        auto const stmt_fun_ident = m_allocator.alloc<T>();
        auto ident = consume();

        stmt_fun_ident->name = ident.value.value();
        checkOpenParen();
        // if the token isn't a ')', means that there should be an expretion example: add([expr])
        // otherwise the fun has no parameters and there's no need to parse any expr
        if (currentToken().has_value() && currentToken().value().type != TokenType::close_paren) {
            stmt_fun_ident->args.push_back(parse_expr());
            // if there is a comma ',' then it must contain other expretion, example: add([expr],[expr])
            // the process repeat until there's no comma
            while (currentToken().has_value() && currentToken().value().type == TokenType::comma) {
                consume(); // ','
                stmt_fun_ident->args.push_back(parse_expr());
            }
        }
        checkCloseParen();
        return stmt_fun_ident;
    }

    /**
 * @brief Parses a statement in the program.
 *
 * This function is responsible for parsing various types of statements in the program, including expressions,
 * declarations, loops, conditionals, and function calls. It uses the current token and token types to determine
 * what type of statement to parse, and then proceeds with the appropriate parsing logic.
 *
 * The function determines which type of statement to parse based on the first token in the input stream and then
 * invokes the appropriate parsing function for that statement.
 *
 * @return NodeStmt A node containing the parsed statement.
 *
 * @throws std::exit If an invalid statement or token is encountered, the program will terminate.
 */
    NodeStmt parse_stmt() { // NOLINT(*-no-recursion)
        if (currentToken().value().type == TokenType::__exit)
        {
            consume();
            checkOpenParen();
            auto stmt_exit = m_allocator.alloc<NodeStmtExit>();
            stmt_exit->expr = parse_expr();
            checkCloseParen();
            checkSemi();
            return NodeStmt{.stmt = stmt_exit};
        }

        if (currentToken().value().type == TokenType::let)
        {
            consume();

            // if after the identifer there is a '[' then it's a let array
            if (currentToken(1).has_value() && currentToken(1).value().type == TokenType::open_bracket) {
                auto const stmt_let = m_allocator.alloc<NodeStmtLetArray>();
                stmt_let->ident = checkIdent();
                consume(); // '['
                if (currentToken().has_value() && currentToken().value().type != TokenType::close_bracket) {
                    if (currentToken().value().type != TokenType::int_lit) {
                        std::cerr << "Expected positive integer for array size declaration" << std::endl;
                        exit(EXIT_FAILURE);
                    }
                    stmt_let->size = parse_pos_term_int_lit();
                }
                checkCloseBracket();
                auto const type = checkType();
                if (!type.has_value()) {
                    std::cerr << "Expected type for array declaration" << std::endl;
                    exit(EXIT_FAILURE);
                }
                stmt_let->type = type.value();
                if (currentToken().has_value() && currentToken().value().type == TokenType::semi ) {
                    consume(); // ';'
                    if (stmt_let->size.has_value()) {
                        return NodeStmt{.stmt = stmt_let};
                    }
                    std::cerr << "Expected size for array declaration without initializing" << std::endl;
                    exit(EXIT_FAILURE);
                }
                checkEq();
                checkOpenBracket();
                stmt_let->exprs.push_back(parse_expr());
                while (currentToken().has_value() && currentToken().value().type == TokenType::comma) {
                    consume(); // ','
                    stmt_let->exprs.push_back(parse_expr());
                }
                checkCloseBracket();
                checkSemi();

                return NodeStmt {.stmt = stmt_let};
            }

            // if it'simple let
            return NodeStmt{.stmt = parse_let()};
        }
        if (currentToken().value().type == TokenType::_if)
        {
            consume();
            return NodeStmt{.stmt = parse_if()};
        }
        if (currentToken().value().type == TokenType::ident)
        {

            // if it's a function ident after the identifier there has to be an '(' token
            if (currentToken(1).has_value() && currentToken(1).value().type == TokenType::open_paren) {
                // consume in parse_fun_ident
                auto stmt_fun = parse_fun_ident<NodeStmtFunIdent>();
                checkSemi();
                return NodeStmt{.stmt = stmt_fun};
            }

            // if it's an array ident it has a '[' after the ident
            if (currentToken(1).has_value() && currentToken(1).value().type == TokenType::open_bracket) {
                auto const stmt_array = m_allocator.alloc<NodeStmtArrayIndex>();
                stmt_array->ident = checkIdent();
                consume(); // '['
                stmt_array->index = parse_expr();
                checkCloseBracket();
                checkEq();
                stmt_array->expr = parse_expr();
                checkSemi();
                return NodeStmt{.stmt = stmt_array};
            }

            // it's just a simple identifier (a variable)
            // consume is in parse_ident
            auto stmt = parse_ident();
            checkSemi();
            return NodeStmt{.stmt = stmt};
        }
        if (currentToken().value().type == TokenType::_for) {
            auto const stmt_for = m_allocator.alloc<NodeStmtFor>();
            consume();
            checkOpenParen();

            // if it's a for(let i ...)
            if (currentToken().has_value() && currentToken().value().type == TokenType::let) {
                consume();
                stmt_for->init = parse_let();
            }
            // else if it's a for(i = ...)
            else if (currentToken().has_value() && currentToken().value().type == TokenType::ident) {
                // consume() is in parse_ident
                stmt_for->init = parse_ident();
                checkSemi();
            }
            // otherwise the init is not valid
            else {
                std::cerr << "Invalid For statement, expected valid initialization" << std::endl;
                exit(EXIT_FAILURE);
            }
            stmt_for->condition = parse_bool_expr();
            checkSemi();
            // to make shure that the update always starts with an ident stmt
            if (!currentToken().has_value() || currentToken().value().type != TokenType::ident) {
                std::cerr << "Invalid For condition, expected valid update stmt" << std::endl;
                exit(EXIT_FAILURE);
            }
            stmt_for->update = parse_ident();
            checkCloseParen();
            checkOpenCurly();
            stmt_for->scope = parse_scope();
            checkCloseCurly();
            return NodeStmt{.stmt = stmt_for};

        }
        if (currentToken().value().type == TokenType::_while) {
            consume();
            auto const stmt_while = m_allocator.alloc<NodeStmtWhile>();
            checkOpenParen();
            stmt_while->condition = parse_bool_expr();
            checkCloseParen();
            checkOpenCurly();
            stmt_while->scope = parse_scope();
            checkCloseCurly();
            return NodeStmt{.stmt = stmt_while};
        }
        if (currentToken().value().type == TokenType::print) {
            consume();
            auto stmt_print = m_allocator.alloc<NodeStmtPrint>();
            checkOpenParen();
            stmt_print->expr = parse_expr();
            checkCloseParen();
            checkSemi();
            return NodeStmt{.stmt = stmt_print};

        }
        if (currentToken().value().type == TokenType::fun) {

            consume();
            auto const stmt_fun = m_allocator.alloc<NodeStmtFun>();
            Token const name = checkIdent();
            stmt_fun->name = name.value.value();
            checkOpenParen();
            // if the token after de '(' is a ')' the function doesn't contain parameters
            // otherwise we check the expr and the type  [expr] : [type]
            // after that is there is a coma we repeat the process
            if (currentToken().has_value() && currentToken().value().type != TokenType::close_paren) {
                stmt_fun->idents.push_back(checkIdent());
                stmt_fun->types.push_back(checkType());
                while(currentToken().has_value() && currentToken().value().type == TokenType::comma) {
                    consume(); // ','
                    stmt_fun->idents.push_back(checkIdent());
                    stmt_fun->types.push_back(checkType());
                }
            }

            checkCloseParen();

            stmt_fun->returnType = checkReturnType(stmt_fun->name);

            checkOpenCurly();
            stmt_fun->scope = parse_scope();
            checkCloseCurly();
            return NodeStmt{.stmt = stmt_fun};

        }


        if (currentToken().value().type == TokenType::_return) {
            consume();
            auto const stmt_return = m_allocator.alloc<NodeStmtReturn>();
            if (currentToken().has_value() && currentToken().value().type == TokenType::semi) {
                consume(); // ';'
                return NodeStmt{.stmt = stmt_return};
            }
            stmt_return->expr = parse_expr();
            checkSemi();
            return NodeStmt{.stmt = stmt_return};
        }



        std::cerr << "Invalid statement " << std::endl;
        exit(EXIT_FAILURE);

    }

    /**
     * @brief Checks if the current token's type matches one of the specified possible types.
     *
     * This function iterates through a list of possible token types and checks if the current token matches
     * any of them. If a match is found, the token is consumed, and the matching type is returned. If no match is
     * found, an error message is printed, and the program terminates.
     *
     * @param possibleTypes A vector of possible token types to check against. Default is `{typeInt, typeChar}`.
     * @param errorMessage The error message to display if no match is found. Default is `"Expected valid type "`.
     *
     * @return std::optional<TokenType> The type of the token if a match is found.
     *
     * @throws std::exit If no matching token type is found, causing the program to terminate.
     */
    std::optional<TokenType> checkPossibleTypes(const std::vector<TokenType> &possibleTypes = {typeInt, typeChar},
                        const std::string& errorMessage = "Expected valid type ") {
        for (auto const &type : possibleTypes) {
            if (currentToken().value().type == type) {
                consume();
                return type;
            }
        }
        std::cerr << errorMessage << std::endl;
        exit(EXIT_FAILURE);
    }

    /**
     * @brief Checks if a type is specified after a colon (':') token.
     *
     * This function checks if the current token is a colon (':'). If so, it expects a type to follow.
     * If the colon is found, it consumes the token and verifies that the following token matches one of
     * the specified possible types. If the type is valid, it returns the type. If no type is specified,
     * or if no valid type is found, the function returns `std::nullopt` to indicate that the type is not present
     * and the compiler must decide the type.
     *
     * @param possibleTypes A vector of possible token types to check after the colon. Default is `{typeInt, typeChar}`.
     *
     * @return std::optional<TokenType> The type of the token if specified, or an empty `std::optional` if no type is provided.
     *
     * @throws std::exit If a colon is found but no type follows it, or if the type after the colon is invalid.
     */
    std::optional<TokenType> checkType(const std::vector<TokenType> &possibleTypes = {typeInt, typeChar} ) {
        if (currentToken().has_value() && currentToken().value().type == TokenType::colon) {
            consume();
            if (!currentToken().has_value()) {
                std::cerr << "Expected type after ':' \n";
                exit(EXIT_FAILURE);
            }
            return checkPossibleTypes(possibleTypes, "Expected valid type after ':' ");
        }

        return {} ;


    }

    /**
     * @brief Checks if the current token matches the expected type.
     *
     * This function checks if the current token matches the expected type. If the token does not match,
     * an error message is printed and the program is terminated. If the token matches, it consumes the token
     * and returns its value, automatically moving to the next token.
     *
     * @param expectedType The expected token type to match with the current token.
     * @param errorMessage The error message to print if the current token does not match the expected type.
     *
     * @return Token The value of the current token if it matches the expected type.
     *
     * @throws std::exit If the current token does not match the expected type, the program is terminated with an error message.
     */
    Token checkToken(TokenType const expectedType, const std::string& errorMessage) {
        if (!currentToken().has_value() || currentToken().value().type != expectedType) {
            std::cerr << errorMessage << std::endl;
            exit(EXIT_FAILURE);
        }
        return consume();
    }

    /**
     * @brief Checks and retrieves the return type of a function.
     *
     * This function verifies that the token representing the return type is correctly formatted for a function.
     * It first checks for the '->' symbol indicating the return type, and then it checks the actual return type.
     * You can specify the function name to improve the error messages.
     *
     * @param fun_name The name of the function (optional), used to improve error messages. Default is an empty string.
     *
     * @return TokenType The return type of the function (e.g., `typeInt`, `typeVoid`, `typeChar`).
     *
     * @throws std::exit If the '->' symbol or the return type is not found or invalid, the program terminates with an error message.
     */
    TokenType checkReturnType(std::string const &fun_name = "") {
        checkToken(TokenType::sub, "Expected return type for function " + fun_name + " expected '->' [type]");
        checkToken(TokenType::great, "Expected return type for function " + fun_name + " expected '->' [type]");
        return checkPossibleTypes({typeInt, typeVoid, typeChar},
            "Expected valid return type after '->' in function " + fun_name ).value();
    }

    // all the other checks are self-explanatory :/


    Token checkOpenParen() {
        return checkToken(TokenType::open_paren, "Expected '('");
    }

    Token checkCloseParen() {
        return checkToken(TokenType::close_paren, "Expected ')'");
    }

    Token checkSemi() {
        return checkToken(TokenType::semi, "Expected ';'");
    }

    Token checkEq() {
        return checkToken(TokenType::eq, "Expected '='");
    }

    Token checkIdent() {
        return checkToken(TokenType::ident, "Expected a valid identifier");
    }
    Token checkOpenCurly() {
        return checkToken(TokenType::open_curly, "Expected '{'");
    }
    Token checkCloseCurly() {
        return checkToken(TokenType::close_curly, "Expected '}'");
    }
    Token checkOpenBracket() {
        return checkToken(TokenType::open_bracket, "Expected '['");
    }
    Token checkCloseBracket() {
        return checkToken(TokenType::close_bracket, "Expected ']'");
    }

    /**
  * @brief Retrieves the current token, possibly with an offset.
  *
  * This function checks if the index, optionally offset by a given value, is within the range
  * of the list of tokens (`m_tokens`). If the index is out of bounds, it returns no value
  * (an empty `std::optional`), otherwise it returns the token at the calculated position.
  *
  * @param offset The offset from the current token index. Default is 0.
  *
  * @return std::optional<Token> The token at the current index plus the offset, or an empty optional
  *         if the index is out of range.
  */
    [[nodiscard]] inline std::optional<Token> currentToken(size_t const offset = 0) const
    {
        if (m_index + offset < m_tokens.size()) {
            return m_tokens[m_index + offset];
        }
        return {};
    }

    /**
 * @brief Generates and parses a boolean expression comparison.
 *
 * This helper function allocates and generates a boolean expression comparison of type `BExpr`.
 * It parses the right-hand side (RHS) of the expression and assigns it to the `rhs` field of
 * the allocated `BExpr` object. This function is used to avoid code repetition when parsing
 * comparison expressions in boolean logic.
 *
 * @tparam BExpr The type of boolean expression comparison node to be created.
 *               This can be a specific derived class of a base boolean expression.
 *
 * @return BExpr* A pointer to the allocated boolean expression comparison node with the
 *                parsed right-hand side expression set.
 */
    template<typename BExpr>
    inline BExpr* genBoolExprCmp(){
        auto bool_expr_cmp = m_allocator.alloc<BExpr>();
        NodeExpr* rhs = parse_expr();
        bool_expr_cmp->rhs = rhs;
        return bool_expr_cmp;
    }

    /**
 * @brief Consumes the current token and increments the token index.
 *
 * This function returns the current token and advances the index to the next token in the list.
 * It is typically used to process and consume tokens one by one while parsing.
 *
 * @return Token The current token before the index is incremented.
 */
    inline Token consume() {
        return m_tokens[m_index++];
    }

    /**
 * @brief Index used to iterate through the list of tokens.
 *
 * This member variable tracks the current position in the list of tokens that the tokenizer has generated.
 * It is incremented each time a token is consumed, allowing sequential processing of the tokens.
 */
    size_t m_index{};

    /**
 * @brief The list of tokens generated by the tokenizer.
 *
 * This member variable holds the list of tokens that were produced by the tokenizer.
 * The parser consumes and processes these tokens in sequence to generate the parse tree.
 */
    const std::vector<Token> m_tokens;

    /**
 * @brief The allocator used for allocating the parse tree.
 *
 * This member variable handles memory allocation for the parse tree nodes. It uses an arena allocator,
 * which allows efficient memory management during parsing and tree construction.
 */
    ArenaAllocator m_allocator;
};



