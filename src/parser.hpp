#pragma once

#include "tokenization.hpp"
#include "arena.hpp"
#include <variant>


struct NodeTermIntLit {
    Token int_lit;
};

struct NodeTermIdent {
    Token ident;
};

struct NodeExpr;

struct NodeTermParen {
    NodeExpr* expr;
};


struct NodeBinExprAdd {
    NodeExpr* lhs;
    NodeExpr* rhs;
};

struct NodeBinExprMulti {
    NodeExpr* lhs;
    NodeExpr* rhs;
};

struct  NodeBinExprDiv {
    NodeExpr* lhs;
    NodeExpr* rhs;
};

struct  NodeBinExprSub {
    NodeExpr* lhs;
    NodeExpr* rhs;
};

struct NodeBoolExprEq {
    NodeExpr* lhs;
    NodeExpr* rhs;
};
struct NodeBoolExprLess {
    NodeExpr* lhs;
    NodeExpr* rhs;
};
struct NodeBoolExprGreat {
    NodeExpr* lhs;
    NodeExpr* rhs;
};
struct NodeBoolExprNeq {
    NodeExpr* lhs;
    NodeExpr* rhs;
};
struct NodeTerm {
    std::variant<NodeTermIdent*, NodeTermIntLit*, NodeTermParen*> var;
};

struct NodeBinExpr {
    std::variant<NodeBinExprAdd*, NodeBinExprMulti*, NodeBinExprDiv*, NodeBinExprSub*> var;
};

struct NodeBoolExpr {
    std::variant<NodeBoolExprEq*, NodeBoolExprNeq*, NodeBoolExprGreat*, NodeBoolExprLess*, NodeExpr*> var;
};

struct NodeExpr {
    std::variant<NodeTerm*, NodeBinExpr*> var;
};


struct NodeStmtExit {
    NodeExpr* expr;
};

struct NodeStmtLet { // NOLINT(*-pro-type-member-init)
    Token ident;
    NodeExpr* expr;
};

struct NodeStmt;

struct NodeScope {
    std::vector<NodeStmt> stmts;
};

struct NodeStmtIf;

struct NodeElse {
    NodeScope* scope = nullptr;
    NodeStmtIf* _if = nullptr;

};

struct NodeStmtIf {
    NodeBoolExpr* bool_expr;
    NodeScope* scope;
    NodeElse* _else = nullptr;
};



struct NodeStmt {
    std::variant<NodeStmtExit*, NodeStmtLet*, NodeStmtIf*> stmt;
};

struct NodeProg {
    std::vector<NodeStmt> stmts;
};




class Parser {
public:
    inline explicit Parser(std::vector<Token>& tokens )
        :m_tokens(std::move(tokens)),
        m_allocator(1024 * 1024 * 4)
    {
    }

    NodeProg parse_prog() {
        NodeProg prog;
        while (currentToken().has_value()) {
            prog.stmts.push_back(parse_stmt());
        }
        return prog;
    }


private:

    NodeTerm* parse_term() { // NOLINT(*-no-recursion)
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
        if (currentToken().has_value() && currentToken().value().type == TokenType::int_lit) {
            auto term_in_lit = m_allocator.alloc<NodeTermIntLit>();
            term_in_lit->int_lit = consume();
            auto const term = m_allocator.alloc<NodeTerm>();
            term->var = term_in_lit;
            return term;

        }
        if (currentToken().has_value() && currentToken().value().type == TokenType::ident) {
            auto term_ident = m_allocator.alloc<NodeTermIdent>();
            term_ident->ident = consume();
            auto const term = m_allocator.alloc<NodeTerm>();
            term->var = term_ident;
            return term;
        }
        if (currentToken().has_value() && currentToken().value().type == TokenType::open_paren) {
            consume();
            auto term_paren = m_allocator.alloc<NodeTermParen>();
            NodeExpr* expr = parse_expr();
            term_paren->expr = expr;
            checkCloseParen();
            auto term = m_allocator.alloc<NodeTerm>();
            term->var = term_paren;
            return term;
        }
        std::cerr << "Invalid term, expected int_literal or identifier " << std::endl;
        exit(EXIT_FAILURE);
    }


    NodeExpr* parse_expr(int const min_prec = 0) { // NOLINT(*-no-recursion)
        NodeTerm* term = parse_term();
        auto lhs = m_allocator.alloc<NodeExpr>();
        lhs->var = term;
        while (true) {
            if (!currentToken().has_value() ||
                    bin_prec(currentToken().value().type) < min_prec) {
                break;
            }
            Token op = consume();
            NodeExpr* rhs = parse_expr(bin_prec(op.type));
            auto bin_expr = m_allocator.alloc<NodeBinExpr>();
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
        }
        return lhs;

    }

    NodeBoolExpr* parse_bool_expr() {
        NodeExpr* lhs= parse_expr();
        auto bool_expr = m_allocator.alloc<NodeBoolExpr>();

        if (!currentToken().has_value()) {
            std::cerr << "Invalid bool expression" << std::endl;
            exit(EXIT_FAILURE);
        }
        if(currentToken().value().type == TokenType::close_paren) {
            consume();
            bool_expr->var = lhs;
            return bool_expr;
        }

        if (currentToken().value().type == TokenType::eq &&
            currentToken(1).has_value() &&
            currentToken(1).value().type == TokenType::eq) {

            consume();
            consume();
            auto bool_expr_eq= genBoolExprCmp<NodeBoolExprEq>();
            bool_expr_eq->lhs = lhs;
            bool_expr->var = bool_expr_eq;
        }
        else if (currentToken().value().type == TokenType::exc_mark &&
            currentToken(1).has_value() &&
            currentToken(1).value().type == TokenType::eq) {

            consume();
            consume();
            auto bool_expr_neq = genBoolExprCmp<NodeBoolExprNeq>();
            bool_expr_neq->lhs = lhs;
            bool_expr->var = bool_expr_neq;

        }
        else if (currentToken().value().type == TokenType::great) {
            consume();
            auto bool_expr_great = genBoolExprCmp<NodeBoolExprGreat>();
            bool_expr_great->lhs = lhs;
            bool_expr->var = bool_expr_great;
        }
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
        checkCloseParen();
        return bool_expr;

    }

    NodeScope* parse_scope() { // NOLINT(*-no-recursion)
        auto scope = m_allocator.alloc<NodeScope>();
            while (currentToken().has_value() &&
                currentToken().value().type != TokenType::close_curly) {

                scope->stmts.push_back(parse_stmt());
            }
        return scope;
    }

    NodeStmtIf* parse_if() {  // NOLINT(*-no-recursion)
        auto stmt_if = m_allocator.alloc<NodeStmtIf>();
        checkOpenParen();
        stmt_if->bool_expr = parse_bool_expr();
        checkOpenCurly();
        NodeScope* scope = parse_scope();
        checkCloseCurly();
        if (currentToken().has_value() && currentToken().value().type == TokenType::_else) {
            consume();
            stmt_if->_else = parse_else();
        }
        stmt_if->scope = scope;
        return stmt_if;
    }

    NodeElse* parse_else() {   // NOLINT(*-no-recursion)
        auto _else = m_allocator.alloc<NodeElse>();
        if (currentToken().has_value() && currentToken().value().type == TokenType::_if) {
            consume();
            _else->_if = parse_if();
        }
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
            auto stmt_let = m_allocator.alloc<NodeStmtLet>();
            stmt_let->ident = checkIdent();
            checkEq();
            stmt_let->expr = parse_expr();
            checkSemi();
            return NodeStmt{.stmt = stmt_let};
        }
        if (currentToken().value().type == TokenType::_if)
        {
            consume();
            return NodeStmt{.stmt = parse_if()};
        }
        std::cerr << "Invalid statement " << std::endl;
        exit(EXIT_FAILURE);

    }

    Token checkToken(TokenType const expectedType, const std::string& errorMessage) {
        if (!currentToken().has_value() || currentToken().value().type != expectedType) {
            std::cerr << errorMessage << std::endl;
            exit(EXIT_FAILURE);
        }
        return consume();
    }

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

    [[nodiscard]] inline std::optional<Token> currentToken(size_t offset = 0) const
    {
        if (m_index + offset < m_tokens.size()) {
            return m_tokens[m_index + offset];
        }
        return {};
    }

    template<typename BExpr>
    inline BExpr* genBoolExprCmp(){
        auto bool_expr_cmp = m_allocator.alloc<BExpr>();
        NodeExpr* rhs = parse_expr();
        bool_expr_cmp->rhs = rhs;
        return bool_expr_cmp;
    }


    inline Token consume() {
        return m_tokens[m_index++];
    }

    size_t m_index{};
    const std::vector<Token> m_tokens;
    ArenaAllocator m_allocator;
};



