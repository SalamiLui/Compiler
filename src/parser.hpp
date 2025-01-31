#pragma once

#include "tokenization.hpp"
#include "arena.hpp"
#include <variant>


// a char
struct NodeTermCharLit {
    Token char_lit;
};

// an int (can be possitive or negative)
struct NodeTermIntLit {
    Token int_lit;
};

// a variable
struct NodeTermIdent {
    Token ident;
};

struct NodeExpr;

/**
 * functions that can be used as a term (its return value is used)
 * examples:
 * let i = genNum();
 * if ( isEqual() ){...}
 */
struct NodeTermFunIdent {
    std::string name;
    std::vector<NodeExpr*> args;
};

// '( [expr] )', the term paren (parenthesis) is used like a regular arithmetic operation
// to compute first an expretion
// examle: 1*(2+1)
struct NodeTermParen {
    NodeExpr* expr;
};

// lhs + rhs
struct NodeBinExprAdd {
    NodeExpr* lhs;
    NodeExpr* rhs;
};

// lhs * rhs
struct NodeBinExprMulti {
    NodeExpr* lhs;
    NodeExpr* rhs;
};

// lhs / rhs
struct  NodeBinExprDiv {
    NodeExpr* lhs;
    NodeExpr* rhs;
};

// lhs - rhs
struct  NodeBinExprSub {
    NodeExpr* lhs;
    NodeExpr* rhs;
};
// ==
struct NodeBoolExprEq {
    NodeExpr* lhs;
    NodeExpr* rhs;
};
// <
struct NodeBoolExprLess {
    NodeExpr* lhs;
    NodeExpr* rhs;
};
// >
struct NodeBoolExprGreat {
    NodeExpr* lhs;
    NodeExpr* rhs;
};
// !=
struct NodeBoolExprNeq {
    NodeExpr* lhs;
    NodeExpr* rhs;
};

// an int, a char, an identifier ...
struct NodeTerm {
    std::variant<
        NodeTermIdent*,
        NodeTermIntLit*,
        NodeTermParen*,
        NodeTermCharLit*,
        NodeTermFunIdent*
            > var;
};

// Artimetics with terms / expretions
struct NodeBinExpr {
    std::variant<NodeBinExprAdd*, NodeBinExprMulti*, NodeBinExprDiv*, NodeBinExprSub*> var;
};

// Comparitions with terms / expretions
struct NodeBoolExpr {
    std::variant<NodeBoolExprEq*, NodeBoolExprNeq*, NodeBoolExprGreat*, NodeBoolExprLess*, NodeExpr*> var;
};


/**
* Can be just a term:
*   int, char, ...
* or a Bin Expr (an aritmetic operetation between terms)
* example:
*   1 + 2 * 3
*   x / y
*/
struct NodeExpr {
    std::variant<NodeTerm*, NodeBinExpr*> var;
    TokenType type;
};


struct NodeStmtExit {
    NodeExpr* expr;
};

/**
* To declare an ident ( a variable )
* example:
* let i = 12;
* let i = x;
* let i : int = add(1,2);
* let c : char = 'j';
*/
struct NodeStmtLet { // NOLINT(*-pro-type-member-init)
    Token ident;
    NodeExpr* expr;
    std::optional<TokenType> type;
};

struct NodeStmt;

/**
* The list of stmts inside an { },
*
* the stack is cleaned after its use
* example:
* initial stack size = 10
* start of scope {
* stms*
* end of scope }
* final stack size = 12
* vars that will be popped of the stack = final stack size - initial stack size
*                                       = 12 - 10
*                                       = 2
*
* if the scope belongs to a function then previous declared vars will be forgotten temporally
* to allow vars inside the scope with the same name of vars outside the scope
*
* inside a scope you can not declare any function
*/
struct NodeScope {
    std::vector<NodeStmt> stmts;
};

struct NodeStmtIf;

/**
* Just can be used after an if stmt thus it's not a stmt
*
* else { [scope] }
* or
* else if ...
*
*/
struct NodeElse {
    NodeScope* scope = nullptr;
    NodeStmtIf* _if = nullptr;

};

/** if ([bool_expr]){ [scope] }
 *  or
 *  if ([bool_expr]){ [scope] } else { [scope] }
 *  or
 *  if ([bool_expr]){ [scope] } else if ([bool_expr]){ [scope] } ...
 */

struct NodeStmtIf { // NOLINT(*-pro-type-member-init)
    NodeBoolExpr* bool_expr;
    NodeScope* scope;
    NodeElse* _else = nullptr;
};
/**
* When an identifer (a variable) is used as a stmt to assign a value to that ident
* example:
*   var = 12;
*   var = x + y;
*/
struct NodeStmtIdent { // NOLINT(*-pro-type-member-init)
    Token ident;
    NodeExpr* expr;
    std::optional<TokenType> type;
};

// for ( [init], [condition], [update] ){ [scope] }
struct NodeStmtFor {
    NodeScope* scope;
    NodeBoolExpr* condition;
    std::variant<NodeStmtLet* , NodeStmtIdent*> init;
    NodeStmtIdent* update;

};

// while ( [condition] ) { [scope] }
struct NodeStmtWhile {
    NodeScope* scope;
    NodeBoolExpr* condition;
};

// print( [expr] )
struct NodeStmtPrint {
    NodeExpr* expr;
};

/**
* a function which its return value is not used and thus is declared as a stmt
* example:
*   doSomething();
*   checkThis();
*/
struct NodeStmtFunIdent {
    std::string name;
    std::vector<NodeExpr*> args;
};

/**
* a function that is being declared
* example:
*   fun Add(n1 : int, n2 : int){...}
*
* fun [name]( [idents*] : [types*] ) -> [returnType] { [scope] }
*/
struct NodeStmtFun {
    std::string name;
    NodeScope* scope;
    std::vector<Token> idents;
    std::vector<std::optional<TokenType>> types;
    TokenType returnType;
};

/**
* return [expr];
* or
* return;
*/
struct NodeStmtReturn {
    NodeExpr* expr = nullptr;
};

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
        NodeStmtReturn*
            >
    stmt;
};

// The final program, which is a list of stmts
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
    * Each stmt is parsed, if the process is succesfull the stmt is appended to the program (prog)
    * and then the program (parsed tree) is returned
*/
    NodeProg parse_prog() {
        NodeProg prog;
        while (currentToken().has_value()) {
            prog.stmts.push_back(parse_stmt());
        }
        return prog;
    }


private:


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
            auto term_int_lit = m_allocator.alloc<NodeTermIntLit>();
            term_int_lit->int_lit = consume();
            auto const term = m_allocator.alloc<NodeTerm>();
            term->var = term_int_lit;
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


    NodeExpr* parse_expr(int const min_prec = 0) { // NOLINT(*-no-recursion)
        // in an expr always has to be first a term (1+2), (x-y) ...
        NodeTerm* term = parse_term();
        auto const lhs = m_allocator.alloc<NodeExpr>();
        lhs->var = term;
        while (true) {
            /** then check if the next token is a valid aritmetic symbol
             * if the min prec of the token == -1 then it's not an arytmetic symbol and
             * will always break the loop
             *
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

    NodeScope* parse_scope() { // NOLINT(*-no-recursion)
        auto const scope = m_allocator.alloc<NodeScope>();
        // while the scope is not finished keep parsing the stmts inside (it's finished when token '{' is found)
            while (currentToken().has_value() &&
                currentToken().value().type != TokenType::close_curly) {

                scope->stmts.push_back(parse_stmt());
            }
        return scope;
    }

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

    NodeStmtLet* parse_let() {
        auto const stmt_let = m_allocator.alloc<NodeStmtLet>();
        stmt_let->ident = checkIdent();
        stmt_let->type = checkType();
        checkEq();
        stmt_let->expr = parse_expr();
        checkSemi();
        return stmt_let;
    }

    NodeStmtIdent* parse_ident() {
        auto const stmt_ident = m_allocator.alloc<NodeStmtIdent>();
        stmt_ident->ident = consume();
        stmt_ident->type = checkType();
        checkEq();
        stmt_ident->expr = parse_expr();
        return stmt_ident;
    }

    /** only use with a NodeStmtFunIdent or NodeTermFunIdent
     */
    template<typename T>
    T* parse_fun_ident() {
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
            // else it's just a simple ident
            if (currentToken(1).has_value() && currentToken(1).value().type == TokenType::open_paren) {
                // consume in parse_fun_ident
                auto stmt_fun = parse_fun_ident<NodeStmtFunIdent>();
                checkSemi();
                return NodeStmt{.stmt = stmt_fun};
            }


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
* iters thrw the vector possibleTypes, if the type is not in the vector throws and expection
* with the errorMessage
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
* if there is a ':' token then the type is specified otherwise the type is not and the compiler must
* decide its type
* returns the type (in case it has one), otherwise returns no value
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
* checks if the current token is the expectedType, if it's not throws and expection with errorMessage
* otherwise returns de value of the currentToken and automatically iters to the next token
*/
    Token checkToken(TokenType const expectedType, const std::string& errorMessage) {
        if (!currentToken().has_value() || currentToken().value().type != expectedType) {
            std::cerr << errorMessage << std::endl;
            exit(EXIT_FAILURE);
        }
        return consume();
    }
/**
* checks the return type of a function, first checks the '->' symbol and then the type
* you can specify the function name to improve the errorMessages
* returns the return type
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

    /**
    * checks if the index is in the range of the list of tokens (m_tokens)'s size
    * in case not, returns no value, otherwise returns the token
*/
    [[nodiscard]] inline std::optional<Token> currentToken(size_t const offset = 0) const
    {
        if (m_index + offset < m_tokens.size()) {
            return m_tokens[m_index + offset];
        }
        return {};
    }

    // used to avoid repetition of code when parsing an expretion compartion
    // allocs the expr comparition BExpr and parse the rhs
    template<typename BExpr>
    inline BExpr* genBoolExprCmp(){
        auto bool_expr_cmp = m_allocator.alloc<BExpr>();
        NodeExpr* rhs = parse_expr();
        bool_expr_cmp->rhs = rhs;
        return bool_expr_cmp;
    }

    // returns the current token and increments the index
    inline Token consume() {
        return m_tokens[m_index++];
    }

    // index used to iter threw the list of tokens
    size_t m_index{};
    // the list of tokens the tokenizer generated
    const std::vector<Token> m_tokens;
    // the algorithm used to alloc the parse tree
    ArenaAllocator m_allocator;
};



